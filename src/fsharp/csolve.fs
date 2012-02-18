//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2011 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module internal Microsoft.FSharp.Compiler.ConstraintSolver

//-------------------------------------------------------------------------
// Incremental type inference constraint solving.  
//
// Primary constraints are:
//   - type equations        ty1 = ty2
//   - subtype inequations   ty1 :> ty2
//   - trait constraints     tyname : (static member op_Addition : 'a * 'b -> 'c)
//
// Plus some other constraints inherited from .NET generics.
// 
// The constraints are immediately processed into a normal form, in particular
//   - type equations on inference parameters:   'tp = ty
//   - type inequations on inference parameters: 'tp :> ty
//   - other constraints on inference paramaters
//
// The state of the inference engine is kept in imperative mutations to inference
// type variables.
//
// The use of the normal form allows the state of the inference engine to 
// be queried for type-directed name resolution, type-directed overload 
// resolution and when generating warning messages.
//
// The inference engine can be used in 'undo' mode to implement
// can-unify predicates used in method overload resolution and trait constraint
// satisfaction.
//
//------------------------------------------------------------------------- 

open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler 

open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Infos.AccessibilityLogic
open Microsoft.FSharp.Compiler.Infos.AttributeChecking
open Microsoft.FSharp.Compiler.Typrelns
open Microsoft.FSharp.Compiler.PrettyNaming

//-------------------------------------------------------------------------
// Generate type variables and record them in within the scope of the
// compilation environment, which currently corresponds to the scope
// of the constraint resolution carried out by type checking.
//------------------------------------------------------------------------- 
   
let compgen_id = mkSynId range0 unassignedTyparName

let NewCompGenTypar (kind,rigid,staticReq,dynamicReq,error) = 
    NewTypar(kind,rigid,Typar(compgen_id,staticReq,true),error,dynamicReq,[],false,false) 
    
let anon_id m = mkSynId m unassignedTyparName

let NewAnonTypar (kind,m,rigid,var,dyn) = 
    NewTypar (kind,rigid,Typar(anon_id m,var,true),false,dyn,[],false,false)
    
let NewNamedInferenceMeasureVar (_m,rigid,var,id) = 
    NewTypar(KindMeasure,rigid,Typar(id,var,false),false,NoDynamicReq,[],false,false) 

let NewInferenceMeasurePar () = NewCompGenTypar (KindMeasure,TyparFlexible,NoStaticReq,NoDynamicReq,false)

let NewErrorTypar () = NewCompGenTypar (KindType,TyparFlexible,NoStaticReq,NoDynamicReq,true)
let NewErrorMeasureVar () = NewCompGenTypar (KindMeasure,TyparFlexible,NoStaticReq,NoDynamicReq,true)
let NewInferenceType () = mkTyparTy (NewTypar (KindType,TyparFlexible,Typar(compgen_id,NoStaticReq,true),false,NoDynamicReq,[],false,false))
let NewErrorType () = mkTyparTy (NewErrorTypar ())
let NewErrorMeasure () = MeasureVar (NewErrorMeasureVar ())

let NewInferenceTypes l = l |> List.map (fun _ -> NewInferenceType ()) 

// QUERY: should 'rigid' ever really be 'true'? We set this when we know 
// we are going to have to generalize a typar, e.g. when implementing a 
// abstract generic method slot. But we later check the generalization 
// condition anyway, so we could get away with a non-rigid typar. This 
// would sort of be cleaner, though give errors later. 
let FreshenAndFixupTypars m rigid fctps tinst tpsorig = 
    let copy_tyvar (tp:Typar) =  NewCompGenTypar (tp.Kind,rigid,tp.StaticReq,(if rigid=TyparRigid then DynamicReq else NoDynamicReq),false)
    let tps = tpsorig |> List.map copy_tyvar 
    let renaming,tinst = FixupNewTypars m fctps tinst tpsorig tps
    tps,renaming,tinst

let FreshenTypeInst m tpsorig = FreshenAndFixupTypars m TyparFlexible [] [] tpsorig 
let new_minst m fctps tinst tpsorig = FreshenAndFixupTypars m TyparFlexible fctps tinst tpsorig 

let FreshenTypars m tpsorig = 
    match tpsorig with 
    | [] -> []
    | _ -> 
        let _,_,tptys = FreshenTypeInst m tpsorig
        tptys

let FreshenMethInfo m (minfo:MethInfo) =
    let _,_,tptys = new_minst m (FormalTyparsOfEnclosingTypeOfMethInfo m minfo) minfo.ActualTypeInst minfo.FormalMethodTypars
    tptys


//-------------------------------------------------------------------------
// Unification of types: solve/record equality constraints
// Subsumption of types: solve/record subtyping constraints
//------------------------------------------------------------------------- 

exception ConstraintSolverTupleDiffLengths of DisplayEnv * TType list * TType list * range  * range 
exception ConstraintSolverInfiniteTypes of DisplayEnv * TType * TType * range * range
exception ConstraintSolverTypesNotInEqualityRelation of DisplayEnv * TType * TType * range  * range 
exception ConstraintSolverTypesNotInSubsumptionRelation of DisplayEnv * TType * TType * range  * range 
exception ConstraintSolverMissingConstraint of DisplayEnv * Tast.Typar * Tast.TyparConstraint * range  * range 
exception ConstraintSolverError of string * range * range
exception ConstraintSolverRelatedInformation of string option * range * exn 

exception ErrorFromApplyingDefault of Env.TcGlobals * DisplayEnv * Tast.Typar * TType * exn * range
exception ErrorFromAddingTypeEquation of Env.TcGlobals * DisplayEnv * TType * TType * exn * range
exception ErrorsFromAddingSubsumptionConstraint of Env.TcGlobals * DisplayEnv * TType * TType * exn * range
exception ErrorFromAddingConstraint of  DisplayEnv * exn * range

exception UnresolvedOverloading of DisplayEnv * exn list * exn list * exn list * string * range
exception UnresolvedConversionOperator of DisplayEnv * TType * TType * range
exception PossibleOverload of DisplayEnv * string * range

let GetPossibleOverloads  amap m denv (calledMethGroup:CalledMeth<_> list) = 
    calledMethGroup |> List.map (fun cmeth -> PossibleOverload(denv,stringOfMethInfo amap m denv cmeth.Method,m))

type ConstraintSolverState = 
    { 
      g: Env.TcGlobals;
      amap: Import.ImportMap; 
      InfoReader : InfoReader;
      /// This table stores all unsolved, ungeneralized trait constraints, indexed by free type variable.
      /// That is, there will be one entry in this table for each free type variable in 
      /// each outstanding, unsolved, ungeneralized trait constraint. Constraints are removed from the table and resolved 
      /// each time a solution to an index variable is found. 
      mutable ExtraCxs:  HashMultiMap<Stamp, (TraitConstraintInfo * range)>;
    }
    static member New(g,amap,infoReader) = 
          { g=g; amap=amap; 
            ExtraCxs= HashMultiMap(10, HashIdentity.Structural)
            InfoReader=infoReader } ;


type ConstraintSolverEnv = 
    { 
      SolverState: ConstraintSolverState;
      m: range;
      EquivEnv: TypeEquivEnv;
      DisplayEnv : DisplayEnv
    }
    member csenv.InfoReader = csenv.SolverState.InfoReader
    member csenv.g = csenv.SolverState.g
    member csenv.amap = csenv.SolverState.amap
    
let MakeConstraintSolverEnv css m denv = 
    { SolverState=css;
      m=m;
      EquivEnv=TypeEquivEnv.Empty; 
      DisplayEnv = denv }


//-------------------------------------------------------------------------
// Occurs check
//------------------------------------------------------------------------- 

/// Check whether a type variable occurs in the r.h.s. of a type, e.g. to catch
/// infinite equations such as 
///    'a = list<'a>
let rec occursCheck g un ty = 
    match stripTyEqns g ty with 
    | TType_ucase(_,l)
    | TType_app (_,l) 
    | TType_tuple l -> List.exists (occursCheck g un) l
    | TType_fun (d,r) -> occursCheck g un d || occursCheck g un r
    | TType_var r   ->  typarEq un r 
    | TType_forall (_,tau) -> occursCheck g un tau
    | _ -> false 


//-------------------------------------------------------------------------
// Predicates on types
//------------------------------------------------------------------------- 

let rec isNativeIntegerTy  g ty =
    typeEquivAux EraseMeasures g g.nativeint_ty ty || 
    typeEquivAux EraseMeasures g g.unativeint_ty ty ||
    (isEnumTy g ty && isNativeIntegerTy g (underlyingTypeOfEnumTy g ty))

let isSignedIntegerTy  g ty =
    typeEquivAux EraseMeasures g g.sbyte_ty ty || 
    typeEquivAux EraseMeasures g g.int16_ty ty || 
    typeEquivAux EraseMeasures g g.int32_ty ty || 
    typeEquivAux EraseMeasures g g.nativeint_ty ty || 
    typeEquivAux EraseMeasures g g.int64_ty ty 

let isUnsignedIntegerTy  g ty =
    typeEquivAux EraseMeasures g g.byte_ty ty || 
    typeEquivAux EraseMeasures g g.uint16_ty ty || 
    typeEquivAux EraseMeasures g g.uint32_ty ty || 
    typeEquivAux EraseMeasures g g.unativeint_ty ty || 
    typeEquivAux EraseMeasures g g.uint64_ty ty 

let rec isIntegerOrIntegerEnumTy g ty =
    isSignedIntegerTy g ty || 
    isUnsignedIntegerTy g ty || 
    (isEnumTy g ty && isIntegerOrIntegerEnumTy g (underlyingTypeOfEnumTy g ty))
    
let rec isIntegerTy g ty =
    isSignedIntegerTy g ty || 
    isUnsignedIntegerTy g ty 
    
let isStringTy g ty = typeEquiv g g.string_ty ty 
let isCharTy g ty = typeEquiv g g.char_ty ty 

/// float or float32 or float<_> or float32<_> 
let isFpTy g ty =
    typeEquivAux EraseMeasures g g.float_ty ty || 
    typeEquivAux EraseMeasures g g.float32_ty ty 

/// decimal or decimal<_>
let isDecimalTy g ty = 
    typeEquivAux EraseMeasures g g.decimal_ty ty 

let IsNonDecimalNumericOrIntegralEnumType g ty = isIntegerOrIntegerEnumTy g ty || isFpTy g ty
let IsNumericOrIntegralEnumType g ty = IsNonDecimalNumericOrIntegralEnumType g ty || isDecimalTy g ty
let IsNonDecimalNumericType g ty = isIntegerTy g ty || isFpTy g ty
let IsNumericType g ty = IsNonDecimalNumericType g ty || isDecimalTy g ty

// Get measure of type, float<_> or float32<_> or decimal<_> but not float=float<1> or float32=float32<1> or decimal=decimal<1> 
let GetMeasureOfType g ty =
    if isAppTy g ty then
        let tcref,tinst = destAppTy g ty
        match tinst with
        | [tyarg] ->  
            match stripTyEqns g tyarg with  
            | TType_measure ms -> 
              if measureEquiv g ms MeasureOne then None else Some (tcref,ms)
            | _ -> None
        | _ -> None
    else None

type TraitConstraintSolution = 
    | TTraitUnsolved
    | TTraitBuiltIn
    | TTraitSolved of MethInfo * TypeInst

let BakedInTraitConstraintNames = 
    [ "op_Division" ; "op_Multiply"; "op_Addition" 
      "op_Subtraction"; "op_Modulus"; 
      "get_Zero"; "get_One";
      "DivideByInt";"get_Item"; "set_Item";
      "op_BitwiseAnd"; "op_BitwiseOr"; "op_ExclusiveOr"; "op_LeftShift";
      "op_RightShift"; "op_UnaryPlus"; "op_UnaryNegation"; "get_Sign"; "op_LogicalNot"
      "op_OnesComplement"; "Abs"; "Sqrt"; "Sin"; "Cos"; "Tan";
      "Sinh";  "Cosh"; "Tanh"; "Atan"; "Acos"; "Asin"; "Exp"; "Ceiling"; "Floor"; "Round"; "Log10"; "Log"; "Sqrt";
      "Truncate"; "op_Explicit";
      "Pow"; "Atan2" ]
    
//-------------------------------------------------------------------------
// Run the constraint solver with undo (used during method overload resolution)

type Trace = 
    | Trace of (unit -> unit) list ref
    static member New () =  Trace (ref [])
    member t.Undo () = let (Trace trace) = t in List.iter (fun a -> a ()) !trace

type OptionalTrace = 
    | NoTrace
    | WithTrace of Trace
    member x.HasTrace = match x with NoTrace -> false | WithTrace _ -> true


let CollectThenUndo f = 
    let trace = Trace.New()
    let res = f trace
    trace.Undo(); 
    res

let CheckThenUndo f = CollectThenUndo f |> CheckNoErrorsAndGetWarnings 

let FilterEachThenUndo f meths = 
    meths |> List.choose (fun calledMeth -> 
        match CheckThenUndo (fun trace -> f trace calledMeth) with 
        | None -> None 
        | Some warns -> Some (calledMeth,warns.Length))


//-------------------------------------------------------------------------
// Solve

exception NonRigidTypar of DisplayEnv * string option * range * TType * TType * range
exception LocallyAbortOperationThatLosesAbbrevs 
let localAbortD = ErrorD LocallyAbortOperationThatLosesAbbrevs

/// Return true if we would rather unify this variable v1 := v2 than vice versa
let PreferUnifyTypar (v1:Typar) (v2:Typar) =
    match v1.Rigidity,v2.Rigidity with 
    // Rigid > all
    | TyparRigid,_ -> false
    // Prefer to unify away WillBeRigid in favour of Rigid
    | TyparWillBeRigid,TyparRigid -> true
    | TyparWillBeRigid,TyparWillBeRigid -> true
    | TyparWillBeRigid,TyparWarnIfNotRigid -> false
    | TyparWillBeRigid,TyparAnon -> false
    | TyparWillBeRigid,TyparFlexible -> false
    // Prefer to unify away WarnIfNotRigid in favour of Rigid
    | TyparWarnIfNotRigid,TyparRigid -> true
    | TyparWarnIfNotRigid,TyparWillBeRigid -> true
    | TyparWarnIfNotRigid,TyparWarnIfNotRigid -> true
    | TyparWarnIfNotRigid,TyparAnon -> false
    | TyparWarnIfNotRigid,TyparFlexible -> false
    // Prefer to unify away anonymous variables in favour of Rigid, WarnIfNotRigid 
    | TyparAnon,TyparRigid -> true
    | TyparAnon,TyparWillBeRigid -> true
    | TyparAnon,TyparWarnIfNotRigid -> true
    | TyparAnon,TyparAnon -> true
    | TyparAnon,TyparFlexible -> false
    // Prefer to unify away Flexible in favour of Rigid, WarnIfNotRigid or Anon
    | TyparFlexible,TyparRigid -> true
    | TyparFlexible,TyparWillBeRigid -> true
    | TyparFlexible,TyparWarnIfNotRigid -> true
    | TyparFlexible,TyparAnon -> true
    | TyparFlexible,TyparFlexible -> 

      // Prefer to unify away compiler generated type vars
      match v1.IsCompilerGenerated, v2.IsCompilerGenerated with
      | true,false -> true
      | false,true -> false
      | _ -> 
         // Prefer to unify away non-error vars - gives better error recovery since we keep
         // error vars lying around, and can avoid giving errors about illegal polymorphism 
         // if they occur 
         match v1.IsFromError, v2.IsFromError with
         | true,false -> false
         | _ -> true



/// Ensure that vs is ordered so that an element with minimum sized exponent
/// is at the head of the list. Also, if possible, this element should have rigidity TyparFlexible 
let FindMinimumMeasureExponent vs =
    let rec findmin vs = 
        match vs with
        | [] -> vs
        | (v:Typar,e)::vs ->
           match findmin vs with
            | [] -> [(v,e)]
            | (v',e')::vs' ->
                if abs e < abs e' || (abs e = abs e' && PreferUnifyTypar v v')
                then (v, e) :: vs
                else (v',e') :: (v,e) :: vs' 
    findmin vs
  
let SubstMeasure (r:Typar) ms = 
    if r.Rigidity = TyparRigid then error(InternalError("SubstMeasure: rigid",r.Range)); 
    if r.Kind = KindType then error(InternalError("SubstMeasure: kind=type",r.Range));

    let tp = r.Data
    match tp.typar_solution with
    | None -> tp.typar_solution <- Some (TType_measure ms)
    | Some _ -> error(InternalError("already solved",r.Range));

let rec TransactStaticReq (csenv:ConstraintSolverEnv) trace (tpr:Typar) req = 
    let m = csenv.m
    if (tpr.Rigidity.ErrorIfUnified && tpr.StaticReq <> req) then 
        ErrorD(ConstraintSolverError(FSComp.SR.csTypeCannotBeResolvedAtCompileTime(tpr.Name),m,m)) 
    else
        let orig = tpr.StaticReq
        match trace with 
        | NoTrace -> () 
        | WithTrace (Trace actions) -> actions := (fun () -> tpr.SetStaticReq orig) :: !actions
        tpr.SetStaticReq req;
        CompleteD

and SolveTypStaticReqTypar (csenv:ConstraintSolverEnv) trace req (tpr:Typar) =
    let orig = tpr.StaticReq
    let req2 = JoinTyparStaticReq req orig
    if orig <> req2 then TransactStaticReq csenv trace tpr req2 else CompleteD

and SolveTypStaticReq (csenv:ConstraintSolverEnv) trace req ty =
    match req with 
    | NoStaticReq -> CompleteD
    | HeadTypeStaticReq -> 
        // requires that a type constructor be known at compile time 
        match stripTyparEqns ty with
        | TType_measure ms ->
          let vs = ListMeasureVarOccsWithNonZeroExponents ms
          IterateD (fun ((tpr:Typar),_) -> SolveTypStaticReqTypar csenv trace req tpr) vs

        | _ -> 
          if (isAnyParTy csenv.g ty) then 
            let tpr = destAnyParTy csenv.g ty
            SolveTypStaticReqTypar csenv trace req tpr
          else CompleteD
      
let rec TransactDynamicReq trace (tpr:Typar) req = 
    let orig = tpr.DynamicReq
    match trace with 
    | NoTrace -> () 
    | WithTrace (Trace actions) -> actions := (fun () -> tpr.SetDynamicReq orig) :: !actions
    tpr.SetDynamicReq req;
    CompleteD

and SolveTypDynamicReq (csenv:ConstraintSolverEnv) trace req ty =
    match req with 
    | NoDynamicReq -> CompleteD
    | DynamicReq -> 
        if (isAnyParTy csenv.g ty) then 
            let tpr = destAnyParTy csenv.g ty
            if tpr.DynamicReq <> DynamicReq then TransactDynamicReq trace tpr DynamicReq else CompleteD
        else CompleteD

let SubstMeasureWarnIfRigid (csenv:ConstraintSolverEnv) trace (v:Typar) ms =
    if v.Rigidity.WarnIfUnified && not (isAnyParTy csenv.g (TType_measure ms)) then         
      // NOTE: we grab the name eagerly to make sure the type variable prints as a type variable 
      let tpnmOpt = if v.IsCompilerGenerated then None else Some v.Name 
      SolveTypStaticReq csenv trace v.StaticReq (TType_measure ms) ++ (fun () -> 
      SubstMeasure v ms;
      WarnD(NonRigidTypar(csenv.DisplayEnv,tpnmOpt,v.Range,TType_measure (MeasureVar v), TType_measure ms,csenv.m)))
    else 
      // Propagate static requirements from 'tp' to 'ty' 
      SolveTypStaticReq csenv trace v.StaticReq (TType_measure ms) ++ (fun () -> 
      SubstMeasure v ms;
      if v.Rigidity = TyparAnon && measureEquiv csenv.g ms MeasureOne then 
        WarnD(Error(FSComp.SR.csCodeLessGeneric(),v.Range))
      else CompleteD)

/// The division operator in F# rounds towards zero. For our purposes,
/// we want to round towards negative infinity. 
let DivRoundDown x y = 
    let signx=if x<0 then -1 else 1
    let signy=if y<0 then -1 else 1
  
    if signx=signy then x / y
    else (x-y+signy) / y

/// Imperatively unify the unit-of-measure expression ms against 1.
/// This is a gcd-like algorithm that proceeds as follows:
/// 1. Express ms in the form 'u1^x1 * ... * 'un^xn * c1^y1 * ... * cm^ym
///    where 'u1,...,'un are non-rigid measure variables, c1,...,cm are measure identifiers or rigid measure variables,
///    x1,...,xn and y1,...,yn are non-zero exponents with |x1| <= |xi| for all i.
/// 2. (a) If m=n=0 then we're done (we're unifying 1 against 1)
///    (b) If m=0 but n<>0 then fail (we're unifying a variable-free expression against 1)
///    (c) If xi is divisible by |x1| for all i, and yj is divisible by |x1| for all j, then 
///        immediately solve the constraint with the substitution 
///          'u1 := 'u2^(-x2/x1) * ... * 'un^(-xn/x1) * c1^(-y1/x1) * ... * cm^(-ym/x1)
///    (d) Otherwise, if m=1, fail (example: unifying 'u^2 * kg^3)
///    (e) Otherwise, make the substitution
///          'u1 := 'u * 'u2^(-x2/x1) * ... * 'un^(-xn/x1) * c1^(-y1/x1) * ... * cm^(-ym/x1)
///        where 'u is a fresh measure variable, and iterate.

let rec UnifyMeasureWithOne (csenv:ConstraintSolverEnv) trace ms = 
    if verbose then dprintf "  UnifyMeasureWithOne...%s\n" ("ms = " ^ Layout.showL(typeL (TType_measure ms)));
    let (rigidVars,nonRigidVars) = (ListMeasureVarOccsWithNonZeroExponents ms) |> List.partition (fun (v,_) -> v.Rigidity = TyparRigid) 
    let expandedCons = ListMeasureConOccsWithNonZeroExponents csenv.g true ms
    let unexpandedCons = ListMeasureConOccsWithNonZeroExponents csenv.g false ms
    match FindMinimumMeasureExponent nonRigidVars, rigidVars, expandedCons, unexpandedCons with
    | [], [], [], _ -> CompleteD
    | [], _, _, _ -> localAbortD
    | (v,e)::vs, rigidVars, _, _ -> 
      // don't break up abbreviations if we can help it!
      if unexpandedCons |> List.forall (fun (_,e') -> e' % e = 0) && (vs@rigidVars) |> List.forall (fun (_,e') -> e' % e = 0) 
      then 
        let newms = ProdMeasures (List.map (fun (c,e') -> MeasurePower (MeasureCon c) (- (DivRoundDown e' e))) unexpandedCons 
                                @ List.map (fun (v,e') -> MeasurePower (MeasureVar v) (- (DivRoundDown e' e))) (vs @ rigidVars))
        SubstMeasureWarnIfRigid csenv trace v newms
      else
        let newms = ProdMeasures (List.map (fun (c,e') -> MeasurePower (MeasureCon c) (- (DivRoundDown e' e))) expandedCons 
                              @ List.map (fun (v,e') -> MeasurePower (MeasureVar v) (- (DivRoundDown e' e))) (vs @ rigidVars))
        if expandedCons |> List.forall (fun (_,e') -> e' % e = 0) && (vs@rigidVars) |> List.forall (fun (_,e') -> e' % e = 0) 
        then SubstMeasureWarnIfRigid csenv trace v newms
        elif isNil vs 
        then localAbortD
        else
          // New variable v' must inherit WarnIfNotRigid from v
          let v' = NewAnonTypar (KindMeasure,v.Range,v.Rigidity,v.StaticReq,v.DynamicReq)
          SubstMeasure v (MeasureProd(MeasureVar v', newms));
          UnifyMeasureWithOne csenv trace ms

/// Imperatively unify unit-of-measure expression ms1 against ms2
let UnifyMeasures (csenv:ConstraintSolverEnv) trace ms1 ms2 = 
    if verbose then dprintf "UnifyMeasures...%s\n" ("ms1 = "^Layout.showL(typeL (TType_measure ms1))^", ms2 = "^Layout.showL(typeL (TType_measure ms2)));
    UnifyMeasureWithOne csenv trace (MeasureProd(ms1,MeasureInv ms2))


/// Simplify a unit-of-measure expression ms that forms part of a type scheme. 
/// We make substitutions for vars, which are the (remaining) bound variables
///   in the scheme that we wish to simplify. 
let SimplifyMeasure g vars ms =
    if verbose then dprintf ("SimplifyMeasure ms = %s generalizable = %s\n") (Layout.showL (typeL (TType_measure ms))) (String.concat "," (List.map (fun tp -> Layout.showL (typeL (mkTyparTy tp))) vars));
    let rec simp vars = 
        match FindMinimumMeasureExponent (List.filter (fun (_,e) -> e<>0) (List.map (fun v -> (v, MeasureVarExponent v ms)) vars)) with
        | [] -> 
          (vars, None)

        | (v,e)::vs -> 
            if e < 0 then
                let v' = NewAnonTypar (KindMeasure,v.Range,TyparFlexible,v.StaticReq,v.DynamicReq)
                let vars' = v' :: ListSet.remove typarEq v vars 
                SubstMeasure v (MeasureInv (MeasureVar v'));
                simp vars'
            else 
                let newv = if v.IsCompilerGenerated then NewAnonTypar (KindMeasure,v.Range,TyparFlexible,v.StaticReq,v.DynamicReq)
                                                    else NewNamedInferenceMeasureVar (v.Range,TyparFlexible,v.StaticReq,v.Id)
                let remainingvars = ListSet.remove typarEq v vars
                let newms = (ProdMeasures (List.map (fun (c,e') -> MeasurePower (MeasureCon c) (- (DivRoundDown e' e))) (ListMeasureConOccsWithNonZeroExponents g false ms)
                                            @ List.map (fun (v',e') -> if typarEq v v' then MeasureVar newv else MeasurePower (MeasureVar v') (- (DivRoundDown e' e))) (ListMeasureVarOccsWithNonZeroExponents ms)));
                SubstMeasure v newms;
                match vs with 
                | [] -> (remainingvars, Some newv) 
                | _ -> simp (newv::remainingvars)
   
    simp vars

// Normalize a type ty that forms part of a unit-of-measure-polymorphic type scheme. 
//  Generalizable are the unit-of-measure variables that remain to be simplified. Generalized
// is a list of unit-of-measure variables that have already been generalized. 
let rec SimplifyMeasuresInType g resultFirst ((generalizable, generalized) as param) ty =
    if verbose then dprintf ("SimplifyMeasuresInType ty = %s generalizable = %s\n") (Layout.showL (typeL ty)) (String.concat "," (List.map (fun tp -> Layout.showL (typeL (mkTyparTy tp))) generalizable));
    match stripTyparEqns ty with 
    | TType_ucase(_,l)
    | TType_app (_,l) 
    | TType_tuple l -> SimplifyMeasuresInTypes g param l

    | TType_fun (d,r) -> if resultFirst then SimplifyMeasuresInTypes g param [r;d] else SimplifyMeasuresInTypes g param [d;r]        
    | TType_var _   -> param
    | TType_forall (_,tau) -> SimplifyMeasuresInType g resultFirst param tau
    | TType_measure unt -> 
        let (generalizable', newlygeneralized) = SimplifyMeasure g generalizable unt   
        if verbose then dprintf "newlygeneralized = %s\n" (match newlygeneralized with None -> "none" | Some tp -> Layout.showL (typeL (mkTyparTy tp)));
        match newlygeneralized with
        | None -> (generalizable', generalized)
        | Some v -> (generalizable', v::generalized)

and SimplifyMeasuresInTypes g param tys = 
    match tys with
    | [] ->  param
    | ty::tys -> 
        let param' = SimplifyMeasuresInType g false param ty 
        SimplifyMeasuresInTypes g param' tys

let SimplifyMeasuresInConstraint g param c =
    match c with
      | TTyparDefaultsToType (_,ty,_) | TTyparCoercesToType(ty,_) -> SimplifyMeasuresInType g false param ty
      | TTyparSimpleChoice (tys,_) -> SimplifyMeasuresInTypes g param tys
      | TTyparIsDelegate (ty1,ty2,_) -> SimplifyMeasuresInTypes g param [ty1;ty2]
      | _ -> param

let rec SimplifyMeasuresInConstraints g param cs = 
    match cs with
    | [] ->  param
    | c::cs ->
        let param' = SimplifyMeasuresInConstraint g param c
        SimplifyMeasuresInConstraints g param' cs


  
// We normalize unit-of-measure-polymorphic type schemes as described in Kennedy's thesis. There  
// are three reasons for doing this:
//   (1) to present concise and consistent type schemes to the programmer
//   (2) so that we can compute equivalence of type schemes in signature matching
//   (3) in order to produce a list of type parameters ordered as they appear in the (normalized) scheme.
//
// Representing the normal form as a matrix, with a row for each variable,
// and a column for each unit-of-measure expression in the "skeleton" of the type. Entries are integer exponents.
//  
// ( 0...0  a1  as1    b1  bs1    c1  cs1    ...)
// ( 0...0  0   0...0  b2  bs2    c2  cs2    ...)
// ( 0...0  0   0...0  0   0...0  c3  cs3    ...)
//...
// ( 0...0  0   0...0  0   0...0  0   0...0  ...)
//
// The normal form is unique; what's more, it can be used to force a variable ordering 
// because the first occurrence of a variable in a type is in a unit-of-measure expression with no 
// other "new" variables (a1, b2, c3, above). 
//
// The corner entries a1, b2, c3 are all positive. Entries lying above them (b1, c1, c2, etc) are
// non-negative and smaller than the corresponding corner entry. Entries as1, bs1, bs2, etc are arbitrary.
// This is known as a *reduced row echelon* matrix or Hermite matrix.   
let SimplifyMeasuresInTypeScheme g resultFirst (generalizable:Typar list) ty constraints =
    // Only bother if we're generalizing over at least one unit-of-measure variable 
    let uvars, vars = 
        generalizable |> List.partition (fun v -> v.Kind = KindMeasure && v.Rigidity <> TyparRigid) 
 
    match uvars with
    | [] -> generalizable
    | _::_ ->
    let (untouched, generalized) = SimplifyMeasuresInType g resultFirst (SimplifyMeasuresInConstraints g (uvars, []) constraints) ty
   
    vars @ List.rev generalized @ untouched

let freshMeasure () = MeasureVar (NewInferenceMeasurePar ())

let CheckWarnIfRigid (csenv:ConstraintSolverEnv) ty1 (r:Typar) ty =
    let g = csenv.g
    let denv = csenv.DisplayEnv
    if r.Rigidity.WarnIfUnified && 
       (not (isAnyParTy g ty) ||
        (let tp2 = destAnyParTy g ty 
         not tp2.IsCompilerGenerated &&
         (r.IsCompilerGenerated || 
          // exclude this warning for two identically named user-specified type parameters, e.g. from different mutually recursive functions or types
          r.DisplayName <> tp2.DisplayName )))  then 
        
        // NOTE: we grab the name eagerly to make sure the type variable prints as a type variable 
        let tpnmOpt = if r.IsCompilerGenerated then None else Some r.Name 
        WarnD(NonRigidTypar(denv,tpnmOpt,r.Range,ty1,ty,csenv.m  )) 
    else 
        CompleteD

/// Add the constraint "ty1 = ty" to the constraint problem, where ty1 is a type variable. 
/// Propagate all effects of adding this constraint, e.g. to solve other variables 
let rec SolveTyparEqualsTyp (csenv:ConstraintSolverEnv) ndeep m2 trace ty1 ty =
    if verbose then dprintf "--> SolveTyparEqualsTyp...%s\n" ("ty1 = "^Layout.showL(typeL ty1)^", ty = "^Layout.showL(typeL ty));
    let m = csenv.m
    let denv = csenv.DisplayEnv
    DepthCheck ndeep m  ++ (fun () -> 
    match ty1 with 
    | TType_var r | TType_measure (MeasureVar r) ->
      // The types may still be equivalent due to abbreviations, which we are trying not to eliminate 
      if typeEquiv csenv.g ty1 ty then CompleteD else

      // The famous 'occursCheck' check to catch things like 'a = list<'a> 
      if occursCheck csenv.g r ty then ErrorD (ConstraintSolverInfiniteTypes(denv,ty1,ty,m,m2)) else

      // Note: warn _and_ continue! 
      CheckWarnIfRigid (csenv:ConstraintSolverEnv) ty1 r ty ++ (fun () ->

      // Record the solution before we solve the constraints, since 
      // We may need to make use of the equation when solving the constraints. 
      // Record a entry in the undo trace if one is provided 
      let tpdata = r.Data
      match trace with 
      | NoTrace -> () 
      | WithTrace (Trace actions) -> actions := (fun () -> tpdata.typar_solution <- None) :: !actions
      tpdata.typar_solution <- Some ty;
      
  (*   dprintf "setting typar %d to type %s at %a\n" r.Stamp ((DebugPrint.showType ty)) outputRange m; *)

      (* Only solve constraints if this is not an error var *)
      if r.IsFromError then CompleteD else
      
      // Check to see if this type variable is relevant to any trait constraints. 
      // If so, re-solve the relevant constraints. 
      (if csenv.SolverState.ExtraCxs.ContainsKey r.Stamp then 
           RepeatWhileD ndeep (fun ndeep -> SolveRelevantMemberConstraintsForTypar csenv ndeep false trace r)
       else 
           CompleteD) ++ (fun _ ->
      
      // Re-solve the other constraints associated with this type variable 
      solveTypMeetsTyparConstraints csenv ndeep m2 trace ty (r.DynamicReq,r.StaticReq,r.Constraints)))

    | _ -> failwith "SolveTyparEqualsTyp")
    

/// Given a type 'ty' and a set of constraints on that type, solve those constraints. 
and solveTypMeetsTyparConstraints (csenv:ConstraintSolverEnv) ndeep m2 trace ty (dreq,sreq,cs) =
    let g = csenv.g
    // Propagate dynamic requirements from 'tp' to 'ty'
    SolveTypDynamicReq csenv trace dreq ty ++ (fun () -> 
    // Propagate static requirements from 'tp' to 'ty' 
    SolveTypStaticReq csenv trace sreq ty ++ (fun () -> 
    
    // Solve constraints on 'tp' w.r.t. 'ty' 
    cs |> IterateD (function
      | TTyparDefaultsToType (priority,dty,m) -> 
          if not (isTyparTy g ty) || typeEquiv g ty dty then CompleteD else
          AddConstraint csenv ndeep m2 trace (destTyparTy g ty)  (TTyparDefaultsToType(priority,dty,m))
          
      | TTyparSupportsNull m2               -> SolveTypSupportsNull               csenv ndeep m2 trace ty
      | TTyparIsEnum(underlying, m2)        -> SolveTypIsEnum                     csenv ndeep m2 trace ty underlying
      | TTyparSupportsComparison(m2)        -> SolveTypeSupportsComparison        csenv ndeep m2 trace ty
      | TTyparSupportsEquality(m2)          -> SolveTypSupportsEquality           csenv ndeep m2 trace ty
      | TTyparIsDelegate(aty,bty, m2)       -> SolveTypIsDelegate                 csenv ndeep m2 trace ty aty bty
      | TTyparIsNotNullableValueType m2     -> SolveTypIsNonNullableValueType     csenv ndeep m2 trace ty
      | TTyparIsUnmanaged m2                -> SolveTypIsUnmanaged                csenv ndeep m2 trace ty
      | TTyparIsReferenceType m2            -> SolveTypIsReferenceType            csenv ndeep m2 trace ty
      | TTyparRequiresDefaultConstructor m2 -> SolveTypRequiresDefaultConstructor csenv ndeep m2 trace ty
      | TTyparSimpleChoice(tys,m2)          -> SolveTypChoice                     csenv ndeep m2 trace ty tys
      | TTyparCoercesToType(ty2,m2)         -> SolveTypSubsumesTypKeepAbbrevs     csenv ndeep m2 trace ty2 ty
      | TTyparMayResolveMemberConstraint(traitInfo,m2) -> 
          SolveMemberConstraint csenv false ndeep m2 trace traitInfo ++ (fun _ -> CompleteD) 
    )))

        
/// Add the constraint "ty1 = ty2" to the constraint problem. 
/// Propagate all effects of adding this constraint, e.g. to solve type variables 
and SolveTypEqualsTyp (csenv:ConstraintSolverEnv) ndeep m2 (trace: OptionalTrace) ty1 ty2 = 
    if verbose then  dprintf "SolveTypEqualsTyp ndeep @ %a\n" outputRange csenv.m;
(*     dprintf "SolveTypEqualsTyp ty1=%s ty2=%s\n" (showL (typeL ty1)) (showL (typeL ty2)); *)
    let ndeep = ndeep + 1
    let aenv = csenv.EquivEnv
    let g = csenv.g
    if ty1 === ty2 then CompleteD else
    let canShortcut = not trace.HasTrace
    let sty1 = stripTyEqnsA csenv.g canShortcut ty1
    let sty2 = stripTyEqnsA csenv.g canShortcut ty2

    match sty1, sty2 with 
    // type vars inside forall-types may be alpha-equivalent 
    | TType_var tp1, TType_var tp2 when  typarEq tp1 tp2 || (aenv.EquivTypars.ContainsKey tp1  && typeEquiv g aenv.EquivTypars.[tp1] ty2) -> CompleteD

    | TType_var tp1, TType_var tp2 when PreferUnifyTypar tp1 tp2 -> SolveTyparEqualsTyp csenv ndeep m2 trace sty1 ty2
    | TType_var tp1, TType_var tp2 when PreferUnifyTypar tp2 tp1 -> SolveTyparEqualsTyp csenv ndeep m2 trace sty2 ty1

    | TType_var r, _ when (r.Rigidity <> TyparRigid) -> SolveTyparEqualsTyp csenv ndeep m2 trace sty1 ty2
    | _, TType_var r when (r.Rigidity <> TyparRigid) -> SolveTyparEqualsTyp csenv ndeep m2 trace sty2 ty1

    // Catch float<_>=float<1>, float32<_>=float32<1> and decimal<_>=decimal<1> 
    | (_, TType_app (tc2,[ms])) when (tc2.IsMeasureableReprTycon && typeEquiv csenv.g sty1 (reduceTyconRefMeasureable tc2 [ms]))
        -> SolveTypEqualsTyp csenv ndeep m2 trace ms (TType_measure MeasureOne)
    | (TType_app (tc2,[ms]), _) when (tc2.IsMeasureableReprTycon && typeEquiv csenv.g sty2 (reduceTyconRefMeasureable tc2 [ms]))
        -> SolveTypEqualsTyp csenv ndeep m2 trace ms (TType_measure MeasureOne)

    | TType_app (tc1,l1)  ,TType_app (tc2,l2) when tyconRefEq g tc1 tc2  -> SolveTypEqualsTypEqns csenv ndeep m2 trace l1 l2
    | TType_app (_,_)   ,TType_app (_,_)   ->  localAbortD
    | TType_tuple l1      ,TType_tuple l2      -> SolveTypEqualsTypEqns csenv ndeep m2 trace l1 l2
    | TType_fun (d1,r1)   ,TType_fun (d2,r2)   -> SolveFunTypEqn csenv ndeep m2 trace d1 d2 r1 r2
    | TType_measure ms1   ,TType_measure ms2   -> UnifyMeasures csenv trace ms1 ms2
    | TType_forall(tps1,rty1), TType_forall(tps2,rty2) -> 
        if tps1.Length <> tps2.Length then localAbortD else
        let aenv = aenv.BindEquivTypars  tps1 tps2 
        let csenv = {csenv with EquivEnv = aenv }
        if not (typarsAEquiv g aenv tps1 tps2) then localAbortD else
        SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty1 rty2 

    | TType_ucase (uc1,l1)  ,TType_ucase (uc2,l2) when g.ucref_eq uc1 uc2  -> SolveTypEqualsTypEqns csenv ndeep m2 trace l1 l2


    | _  -> localAbortD

and SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ty1 ty2 = 
   let denv = csenv.DisplayEnv
   
   // Back out of expansions of type abbreviations to give improved error messages. 
   // Note: any "normalization" of equations on type variables must respect the trace parameter
   TryD (fun () -> SolveTypEqualsTyp csenv ndeep m2 trace ty1 ty2)
        (function LocallyAbortOperationThatLosesAbbrevs -> ErrorD(ConstraintSolverTypesNotInEqualityRelation(denv,ty1,ty2,csenv.m,m2))
                | err -> ErrorD err)

and SolveTypEqualsTypEqns csenv ndeep m2 trace origl1 origl2 = 
   match origl1,origl2 with 
   | [],[] -> CompleteD 
   | _ -> 
       // We unwind Iterate2D by hand here for performance reasons.
       let rec loop l1 l2 = 
           match l1,l2 with 
           | [],[] -> CompleteD 
           | h1::t1, h2::t2 -> 
               SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace h1 h2 ++ (fun () -> loop t1 t2) 
           | _ -> 
               ErrorD(ConstraintSolverTupleDiffLengths(csenv.DisplayEnv,origl1,origl2,csenv.m,m2)) 
       loop origl1 origl2

and SolveFunTypEqn csenv ndeep m2 trace d1 d2 r1 r2 = 
    SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace d1 d2 ++ (fun () -> 
    SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace r1 r2)

and SolveTypSubsumesTyp (csenv:ConstraintSolverEnv) ndeep m2 (trace: OptionalTrace) ty1 ty2 = 
    // 'a :> obj ---> <solved> 
    let ndeep = ndeep + 1
    let g = csenv.g
    let amap = csenv.amap
    let aenv = csenv.EquivEnv
    let denv = csenv.DisplayEnv
    let m = csenv.m
    if isObjTy g ty1 then CompleteD else 
    let canShortcut = not trace.HasTrace
    let sty1 = stripTyEqnsA csenv.g canShortcut ty1
    let sty2 = stripTyEqnsA csenv.g canShortcut ty2
    match sty1, sty2 with 
    | TType_var tp1, _ when aenv.EquivTypars.ContainsKey tp1 -> 
        SolveTypSubsumesTyp csenv ndeep m2 trace aenv.EquivTypars.[tp1] ty2 
        
    | TType_var r1, TType_var r2 when typarEq r1 r2 -> CompleteD
    | _, TType_var r (* when not (rigid_of_typar r) *) -> SolveTyparSubtypeOfType csenv ndeep m2 trace r ty1
    | TType_var _ , _ (* | _, TType_var r *)  ->  SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ty1 ty2
    | TType_tuple l1    ,TType_tuple l2     -> SolveTypEqualsTypEqns csenv ndeep m2 trace l1 l2 (* nb. can unify since no variance *)
    | TType_fun (d1,r1)  ,TType_fun (d2,r2)   -> SolveFunTypEqn csenv ndeep m2 trace d1 d2 r1 r2 (* nb. can unify since no variance *)
    | TType_measure ms1, TType_measure ms2    -> UnifyMeasures csenv trace ms1 ms2

    // Enforce the identities float=float<1>, float32=float32<1> and decimal=decimal<1> 
    | (_, TType_app (tc2,[ms])) when (tc2.IsMeasureableReprTycon && typeEquiv csenv.g sty1 (reduceTyconRefMeasureable tc2 [ms]))
        -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ms (TType_measure MeasureOne)
    | (TType_app (tc2,[ms]), _) when (tc2.IsMeasureableReprTycon && typeEquiv csenv.g sty2 (reduceTyconRefMeasureable tc2 [ms]))
        -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ms (TType_measure MeasureOne)

    | TType_app (tc1,l1)  ,TType_app (tc2,l2) when tyconRefEq g tc1 tc2  -> 
        SolveTypEqualsTypEqns csenv ndeep m2 trace l1 l2

    | TType_ucase (uc1,l1)  ,TType_ucase (uc2,l2) when g.ucref_eq uc1 uc2  -> 
        SolveTypEqualsTypEqns csenv ndeep m2 trace l1 l2

    | _ ->  
        // By now we know the type is not a variable type 

        // C :> obj ---> <solved> 
        if isObjTy g ty1 then CompleteD else

        // 'a[] :> IList<'b>   ---> 'a = 'b  
        // 'a[] :> ICollection<'b>   ---> 'a = 'b  
        // 'a[] :> IEnumerable<'b>   ---> 'a = 'b  
        // Note we don't support co-variance on array types nor 
        // the special .NET conversions for these types 
        if 
            (isArray1DTy g ty2 &&  
             isAppTy g ty1 && 
             (let tcr1 = tcrefOfAppTy g ty1
              tyconRefEq g tcr1 g.tcref_System_Collections_Generic_IList || 
              tyconRefEq g tcr1 g.tcref_System_Collections_Generic_ICollection || 
              tyconRefEq g tcr1 g.tcref_System_Collections_Generic_IEnumerable)) then

          let _,tinst = destAppTy g ty1
          match tinst with 
          | [ty1arg] -> 
              let ty2arg = destArrayTy g ty2
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ty1arg  ty2arg
          | _ -> error(InternalError("destArrayTy",m));

        // D<inst> :> Head<_> --> C<inst'> :> Head<_> for the 
        // first interface or super-class C supported by D which 
        // may feasibly convert to Head. 

        else 
            match (FindUniqueFeasibleSupertype g amap m ty1 ty2) with 
            | None -> ErrorD(ConstraintSolverTypesNotInSubsumptionRelation(denv,ty1,ty2,m,m2))
            | Some t -> SolveTypSubsumesTyp csenv ndeep m2 trace ty1 t

and SolveTypSubsumesTypKeepAbbrevs csenv ndeep m2 trace ty1 ty2 = 
   let denv = csenv.DisplayEnv
   TryD (fun () -> SolveTypSubsumesTyp csenv ndeep m2 trace ty1 ty2)
        (function LocallyAbortOperationThatLosesAbbrevs -> ErrorD(ConstraintSolverTypesNotInSubsumptionRelation(denv,ty1,ty2,csenv.m,m2))
                | err -> ErrorD err)

//-------------------------------------------------------------------------
// Solve and record non-equality constraints
//------------------------------------------------------------------------- 

      
and SolveTyparSubtypeOfType (csenv:ConstraintSolverEnv) ndeep m2 trace tp ty1 = 
    let g = csenv.g
    let m = csenv.m
    if isObjTy g ty1 then CompleteD
    elif typeEquiv g ty1 (mkTyparTy tp) then CompleteD
    elif isSealedTy g ty1 then 
        SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace (mkTyparTy tp) ty1
    else 
        AddConstraint csenv ndeep m2 trace tp  (TTyparCoercesToType(ty1,m))

and DepthCheck ndeep m = 
  if ndeep > 300 then error(Error(FSComp.SR.csTypeInferenceMaxDepth(),m)) else CompleteD

// If this is a type that's parameterized on a unit-of-measure (expected to be numeric), unify its measure with 1
and SolveDimensionlessNumericType (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    match GetMeasureOfType csenv.g ty with
    | Some (tcref, _) ->
      SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ty (mkAppTy tcref [TType_measure MeasureOne])
    | None ->
      CompleteD

/// We do a bunch of fakery to pretend that primitive types have certain members. 
/// We pretend int and other types support a number of operators.  In the actual IL for mscorlib they 
/// don't, however the type-directed static optimization rules in the library code that makes use of this 
/// will deal with the problem. 
and SolveMemberConstraint (csenv:ConstraintSolverEnv) permitWeakResolution ndeep m2 trace (TTrait(tys,nm,memFlags,argtys,rty,sln)) :  OperationResult<bool> =
    // Do not re-solve if already solved
    if sln.Value.IsSome then ResultD true else
    let g = csenv.g
    let m = csenv.m
    let amap = csenv.amap
    let aenv = csenv.EquivEnv
    let denv = csenv.DisplayEnv
    let ndeep = ndeep + 1
    DepthCheck ndeep m  ++ (fun () -> 

    if verbose then dprintf "-----------------------------\nResolve trait for %s\n" nm;

    // Remove duplicates from the set of types in the support 
    let tys = ListSet.setify (typeAEquiv g aenv) tys
    // Rebuild the trait info after removing duplicates 
    let traitInfo = TTrait(tys,nm,memFlags,argtys,rty,sln)
    let rty = GetFSharpViewOfReturnType g rty    
    
    // Assert the object type if the constraint is for an instance member
    begin
        if memFlags.IsInstance then 
            match tys, argtys with 
            | [ty], (h :: _) -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace h ty
            | _ -> ErrorD (ConstraintSolverError(FSComp.SR.csExpectedArguments(), m,m2))
        else CompleteD
    end ++ (fun () -> 
    
    // Trait calls are only supported on pseudo type (variables) 
    tys |> IterateD (SolveTypStaticReq csenv trace HeadTypeStaticReq)) ++ (fun () -> 
    
    let argtys = if memFlags.IsInstance then List.tail argtys else argtys

    let minfos = GetRelevantMethodsForTrait csenv permitWeakResolution nm traitInfo

    if verbose then minfos |> List.iter (fun minfo -> dprintf "Possible overload: %s\n" (stringOfMethInfo amap m denv minfo));
        
    match minfos,tys,memFlags.IsInstance,nm,argtys with 
      | _,_,false,("op_Division" | "op_Multiply"),[argty1;argty2]
          when 
               // This simulates the existence of 
               //    float * float -> float
               //    float32 * float32 -> float32
               //    float<'u> * float<'v> -> float<'u 'v>
               //    float32<'u> * float32<'v> -> float32<'u 'v>
               //    decimal<'u> * decimal<'v> -> decimal<'u 'v>
               //    decimal<'u> * decimal -> decimal<'u>
               //    float32<'u> * float32<'v> -> float32<'u 'v>
               //    int * int -> int
               //    int64 * int64 -> int64
               //
               // The rule is triggered by these sorts of inputs when permitWeakResolution=false
               //    float * float 
               //    float * float32 // will give error 
               //    decimal<m> * decimal<m>
               //    decimal<m> * decimal  <-- Note this one triggers even though "decimal" has some possibly-relevant methods
               //    float * Matrix // the rule doesn't trigger for this one since Matrix has overloads we can use and we prefer those instead
               //    float * Matrix // the rule doesn't trigger for this one since Matrix has overloads we can use and we prefer those instead
               //
               // The rule is triggered by these sorts of inputs when permitWeakResolution=true
               //    float * 'a 
               //    'a * float 
               //    decimal<'u> * 'a <---
                  (let checkRuleAppliesInPreferenceToMethods argty1 argty2 = 
                     // Check that at least one of the argument types is numeric
                     (IsNumericOrIntegralEnumType g argty1) && 
                     // Check that the support of type variables is empty. That is,
                     // if we're canonicalizing, then having one of the types nominal is sufficient.
                     // If not, then both must be nominal (i.e. not a type variable).
                     (permitWeakResolution || not (isTyparTy g argty2)) &&
                     // This next condition checks that either 
                     //   - Neither type contributes any methods OR
                     //   - We have the special case "decimal<_> * decimal". In this case we have some 
                     //     possibly-relevant methods from "decimal" but we ignore them in this case.
                     (isNil minfos || (isSome (GetMeasureOfType g argty1) && isDecimalTy g argty2)) in

                   checkRuleAppliesInPreferenceToMethods argty1 argty2 || 
                   checkRuleAppliesInPreferenceToMethods argty2 argty1) ->
                   
          match GetMeasureOfType g argty1 with
          | Some (tcref,ms1) ->
            let ms2 = freshMeasure ()
            SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 (mkAppTy tcref [TType_measure ms2]) ++ (fun () ->
            SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty (mkAppTy tcref [TType_measure (MeasureProd(ms1,if nm = "op_Multiply" then ms2 else MeasureInv ms2))]) ++ (fun () ->
            ResultD TTraitBuiltIn))
          | _ ->
            match GetMeasureOfType g argty2 with
            | Some (tcref,ms2) ->
              let ms1 = freshMeasure ()
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty1 (mkAppTy tcref [TType_measure ms1]) ++ (fun () ->
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty (mkAppTy tcref [TType_measure (MeasureProd(ms1, if nm = "op_Multiply" then ms2 else MeasureInv ms2))]) ++ (fun () ->
              ResultD TTraitBuiltIn))
            | _ ->
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 argty1 ++ (fun () -> 
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
              ResultD TTraitBuiltIn))

      | _,_,false,("op_Addition" | "op_Subtraction" | "op_Modulus"),[argty1;argty2] 
          when // Ignore any explicit +/- overloads from any basic integral types
               (isNil (minfos |> List.filter (fun minfo -> not(isIntegerTy g minfo.EnclosingType ))) &&
                (   (IsNumericOrIntegralEnumType g argty1 || (nm = "op_Addition" && (isCharTy g argty1 || isStringTy g argty1))) && (permitWeakResolution || not (isTyparTy g argty2))
                 || (IsNumericOrIntegralEnumType g argty2 || (nm = "op_Addition" && (isCharTy g argty2 || isStringTy g argty2))) && (permitWeakResolution || not (isTyparTy g argty1)))) ->
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 argty1 ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
          ResultD TTraitBuiltIn))


      // We pretend for uniformity that the numeric types have a static property called Zero and One 
      // As with constants, only zero is polymorphic in its units
      | [],[ty],false,"get_Zero",[] 
          when IsNumericType g ty ->
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty ty ++ (fun () -> 
          ResultD TTraitBuiltIn)

      | [],[ty],false,"get_One",[] 
          when IsNumericType g ty || isCharTy g ty ->
          SolveDimensionlessNumericType csenv ndeep m2 trace ty ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty ty ++ (fun () -> 
          ResultD TTraitBuiltIn))

      | [],_,false,("DivideByInt"),[argty1;argty2] 
          when isFpTy g argty1 || isDecimalTy g argty1 ->
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 g.int_ty ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
          ResultD TTraitBuiltIn))

      // We pretend for uniformity that the 'string' and 'array' types have an indexer property called 'Item' 
      | [], [ty],true,("get_Item"),[argty1] 
          when isStringTy g ty ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty1 g.int_ty ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty g.char_ty ++ (fun () -> 
          ResultD TTraitBuiltIn))

      | [], [ty],true,("get_Item"),argtys
          when isArrayTy g ty ->

          (if rankOfArrayTy g ty <> argtys.Length then ErrorD(ConstraintSolverError(FSComp.SR.csIndexArgumentMismatch((rankOfArrayTy g ty), argtys.Length),m,m2)) else CompleteD) ++ (fun () ->
          (argtys |> IterateD (fun argty -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty g.int_ty)) ++ (fun () -> 
          let ety = destArrayTy g ty
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty ety ++ (fun () -> 
          ResultD TTraitBuiltIn)))

      | [], [ty],true,("set_Item"),argtys
          when isArrayTy g ty ->
          
          (if rankOfArrayTy g ty <> argtys.Length - 1 then ErrorD(ConstraintSolverError(FSComp.SR.csIndexArgumentMismatch((rankOfArrayTy g ty), (argtys.Length - 1)),m,m2)) else CompleteD) ++ (fun () ->
          let argtys,ety = List.frontAndBack argtys
          (argtys |> IterateD (fun argty -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty g.int_ty)) ++ (fun () -> 
          let etys = destArrayTy g ty
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ety etys ++ (fun () -> 
          ResultD TTraitBuiltIn)))

      | [],_,false,("op_BitwiseAnd" | "op_BitwiseOr" | "op_ExclusiveOr"),[argty1;argty2] 
          when    (isIntegerOrIntegerEnumTy g argty1 || (isFlagEnumTy g argty1)) && (permitWeakResolution || not (isTyparTy g argty2))
               || (isIntegerOrIntegerEnumTy g argty2 || (isFlagEnumTy g argty2)) && (permitWeakResolution || not (isTyparTy g argty1)) ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 argty1 ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
          SolveDimensionlessNumericType csenv ndeep m2 trace argty1 ++ (fun () -> 
          ResultD TTraitBuiltIn)));

      | [], _,false,("op_LeftShift" | "op_RightShift"),[argty1;argty2] 
          when    isIntegerOrIntegerEnumTy g argty1  ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 g.int_ty ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
          SolveDimensionlessNumericType csenv ndeep m2 trace argty1 ++ (fun () -> 
          ResultD TTraitBuiltIn)))

      | _,_,false,("op_UnaryPlus"),[argty] 
          when IsNumericOrIntegralEnumType g argty ->  

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty ++ (fun () -> 
          ResultD TTraitBuiltIn)

      | _,_,false,("op_UnaryNegation"),[argty] 
          when isSignedIntegerTy g argty || isFpTy g argty || isDecimalTy g argty ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty ++ (fun () -> 
          ResultD TTraitBuiltIn)

      | _,_,true,("get_Sign"),[] 
          when (let argty = tys.Head in isSignedIntegerTy g argty || isFpTy g argty || isDecimalTy g argty) ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty g.int32_ty ++ (fun () -> 
          ResultD TTraitBuiltIn)

      | _,_,false,("op_LogicalNot" | "op_OnesComplement"),[argty] 
          when isIntegerOrIntegerEnumTy g argty  ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty ++ (fun () -> 
          SolveDimensionlessNumericType csenv ndeep m2 trace argty ++ (fun () -> 
          ResultD TTraitBuiltIn))

      | _,_,false,("Abs"),[argty] 
          when isSignedIntegerTy g argty || isFpTy g argty || isDecimalTy g argty ->

          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty ++ (fun () -> 
          ResultD TTraitBuiltIn)

      | _,_,false,"Sqrt",[argty1] 
          when isFpTy g argty1 ->
          match GetMeasureOfType g argty1 with
            | Some (tcref, _) ->
              let ms1 = freshMeasure () 
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty1 (mkAppTy tcref [TType_measure (MeasureProd (ms1,ms1))]) ++ (fun () ->
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty (mkAppTy tcref [TType_measure ms1]) ++ (fun () ->
              ResultD TTraitBuiltIn))
            | None ->
              SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
              ResultD TTraitBuiltIn)

      | _,_,false,("Sin" | "Cos" | "Tan" | "Sinh" | "Cosh" | "Tanh" | "Atan" | "Acos" | "Asin" | "Exp" | "Ceiling" | "Floor" | "Round" | "Truncate" | "Log10" | "Log" | "Sqrt"),[argty] 
          when isFpTy g argty ->

          SolveDimensionlessNumericType csenv ndeep m2 trace argty ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty ++ (fun () -> 
          ResultD TTraitBuiltIn))

      | _,_,false,("op_Explicit"),[argty] 
          when (// The input type. 
                (IsNonDecimalNumericOrIntegralEnumType g argty || isStringTy g argty || isCharTy g argty) &&
                // The output type
                (IsNonDecimalNumericOrIntegralEnumType g rty || isCharTy g rty) && 
                // Exclusion: IntPtr and UIntPtr do not support .Parse() from string 
                not (isStringTy g argty && isNativeIntegerTy g rty) &&
                // Exclusion: No conversion from char to decimal
                not (isCharTy g argty && isDecimalTy g rty)) -> 

          ResultD TTraitBuiltIn


      | _,_,false,("op_Explicit"),[argty] 
          when (// The input type. 
                (IsNumericOrIntegralEnumType g argty || isStringTy g argty) &&
                // The output type
                (isDecimalTy g rty)) -> 

          ResultD TTraitBuiltIn

      | [],_,false,"Pow",[argty1; argty2] 
          when isFpTy g argty1 ->
          
          SolveDimensionlessNumericType csenv ndeep m2 trace argty1 ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 argty1 ++ (fun () -> 
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1 ++ (fun () -> 
          ResultD TTraitBuiltIn)))

      | _,_,false,("Atan2"),[argty1; argty2] 
          when isFpTy g argty1 ->
          SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace argty2 argty1 ++ (fun () ->
          match GetMeasureOfType g argty1 with
          | None -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty argty1
          | Some (tcref, _) -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty (mkAppTy tcref [TType_measure MeasureOne])) ++ (fun () ->
          ResultD TTraitBuiltIn)

      | _ -> 
          // OK, this is not solved by a built-in constraint.
          // Now look for real solutions

          match minfos,tys  with 
          | [],[ty] when not (isTyparTy g ty) ->
              if tys |> List.exists (isFunTy g) then 
                  ErrorD (ConstraintSolverError(FSComp.SR.csExpectTypeWithOperatorButGivenFunction(DecompileOpName nm),m,m2)) 
              elif nm = "op_Explicit" then
                  match argtys with 
                  | [argty] -> ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportConversion((NicePrint.prettyStringOfTy denv argty), (NicePrint.prettyStringOfTy denv rty)),m,m2))
                  | _ -> ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportOperator((NicePrint.prettyStringOfTy denv ty), DecompileOpName nm),m,m2))
              else
                  ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportOperator((NicePrint.prettyStringOfTy denv ty), DecompileOpName nm),m,m2))

          | _ -> 

              let dummyExpr = mkUnit g m
              let calledMethGroup = 
                  minfos 
                    // curried members may not be used to satisfy constraints
                    |> List.filter (fun minfo -> not minfo.IsCurried) 
                    |> List.map (fun minfo -> 
                      let callerArgs = argtys |> List.map (fun argty -> CallerArg(argty,m,false,dummyExpr))
                      let minst = FreshenMethInfo m minfo
                      let objtys = (ObjTypesOfMethInfo amap m minfo minst)
                      MakeCalledMeth(csenv.InfoReader,false, FreshenMethInfo, m,AccessibleFromEverywhere,minfo,minst,minst,None,objtys,[(callerArgs,[])],false, false))
              (* dprintf "    ---> calling ResolveOverloading, nm = %s, ty = '%s'\n" nm (Layout.showL (typeL ty)); *)

              let result,errors = 
                  CollectThenUndo (fun trace -> ResolveOverloading csenv (WithTrace(trace)) nm ndeep true (0,0) AccessibleFromEverywhere calledMethGroup false (Some rty))  

              if verbose then dprintf "    <--- called ResolveOverloading, ok? = %b\n" (isSome (CheckNoErrorsAndGetWarnings errors));
                  
              match result with 
              | Some (calledMeth:CalledMeth<_>) -> 
                  // OK, the constraint is solved. 
                  // Re-run without undo to commit the inference equations. Throw errors away 
                  let minfo = calledMeth.Method
                  if verbose then dprintf "    ---> constraint solved, calling ResolveOverloading a second time, without undo, minfo = %s\n" (stringOfMethInfo amap m denv minfo);
                  let _,errors = ResolveOverloading csenv trace nm ndeep true (0,0) AccessibleFromEverywhere calledMethGroup false (Some rty)

                  errors ++ (fun () -> 
                      let isInstance = minfo.IsInstance
                      if isInstance <> memFlags.IsInstance then 
                          if isInstance then
                              ErrorD(ConstraintSolverError(FSComp.SR.csMethodFoundButIsNotStatic((NicePrint.prettyStringOfTy denv minfo.EnclosingType), (DecompileOpName nm), nm),m,m2 ))
                          else
                              ErrorD(ConstraintSolverError(FSComp.SR.csMethodFoundButIsStatic((NicePrint.prettyStringOfTy denv minfo.EnclosingType), (DecompileOpName nm), nm),m,m2 ))
                      else 
                          CheckMethInfoAttributes g m None minfo ++ (fun () -> 
                          ResultD (TTraitSolved (minfo,calledMeth.CalledTyArgs))))
                          
              | None ->
                      
                  let support =  GetSupportOfMemberConstraint csenv traitInfo
                  let frees =  GetFreeTyparsOfMemberConstraint csenv traitInfo

                  // If there's nothing left to learn then raise the errors 
                  (if (permitWeakResolution && isNil support) || isNil frees then errors  
                  // Otherwise re-record the trait waiting for canonicalization 
                   else AddMemberConstraint csenv ndeep m2 trace traitInfo support frees) ++ (fun () -> ResultD TTraitUnsolved)
    ) 
    ++ 
    (fun res -> RecordMemberConstraintSolution m trace traitInfo res))


/// Record the solution to a member constraint in the mutable reference cell attached to 
/// each member constraint.
and RecordMemberConstraintSolution m trace traitInfo res = 
            
        match res with 
        | TTraitUnsolved -> 
            ResultD false

        | TTraitSolved (minfo,minst) -> 
            let sln = MemberConstraintSolutionOfMethInfo m minfo minst
            TransactMemberConstraintSolution traitInfo trace sln;
            ResultD true

        | TTraitBuiltIn -> 
            TransactMemberConstraintSolution traitInfo trace BuiltInSln;
            ResultD true

/// Convert a MethInfo into the data we save in the TAST
and MemberConstraintSolutionOfMethInfo m minfo minst = 
    match minfo with 
    | ILMeth(_,ILMethInfo(ILTypeInfo(tcref,tref,tinst,_),extOpt,mdef,_),_) ->
       let mref = IL.mkRefToILMethod (tref,mdef)
       ILMethSln(mkAppTy tcref tinst,extOpt,mref,minst)
    | FSMeth(_,typ,vref,_) ->  
       FSMethSln(typ, vref,minst)
    | MethInfo.DefaultStructCtor _ -> 
       error(InternalError("the default struct constructor was the unexpected solution to a trait constraint",m))

/// Write into the reference cell stored in the TAST and add to the undo trace if necessary
and TransactMemberConstraintSolution traitInfo trace sln  =
    let prev = traitInfo.Solution 
    traitInfo.Solution <- Some sln
    match trace with 
    | NoTrace -> () 
    | WithTrace (Trace actions) -> actions := (fun () -> traitInfo.Solution <- prev) :: !actions

/// Only consider overload resolution if canonicalizing or all the types are now nominal. 
/// That is, don't perform resolution if more nominal information may influence the set of available overloads 
and GetRelevantMethodsForTrait (csenv:ConstraintSolverEnv) permitWeakResolution nm (TTrait(tys,_,memFlags,argtys,rty,soln) as traitInfo) =
    let results = 
        if permitWeakResolution || isNil (GetSupportOfMemberConstraint csenv traitInfo) then
            let m = csenv.m
            let g = csenv.g
            let minfos = tys |> List.map (GetIntrinsicMethInfosOfType csenv.SolverState.InfoReader (Some nm,AccessibleFromSomeFSharpCode,AllowMultiIntfInst) IgnoreOverrides m) 
            /// Merge the sets so we don't get the same minfo from each side 
            /// We merge based on whether minfos use identical metadata or not. 

            let minfos = List.fold (ListSet.unionFavourLeft (MethInfosUseIdenticalDefinitions g)) (List.head minfos) (List.tail minfos)
            minfos
        else 
            []
    // The trait name "op_Explicit" also covers "op_Implicit", so look for that one too.
    if nm = "op_Explicit" then 
        results @ GetRelevantMethodsForTrait (csenv:ConstraintSolverEnv) permitWeakResolution "op_Implicit" (TTrait(tys,"op_Implicit",memFlags,argtys,rty,soln))
    else
        results


/// The nominal support of the member constraint 
and GetSupportOfMemberConstraint (csenv:ConstraintSolverEnv) (TTrait(tys,_,_,_,_,_)) =
    tys |> List.choose (fun ty -> if isTyparTy csenv.g ty then Some (destTyparTy csenv.g ty) else  None)
    
/// All the typars relevant to the member constraint *)
and GetFreeTyparsOfMemberConstraint (csenv:ConstraintSolverEnv) (TTrait(tys,_,_,argtys,rty,_)) =
    (freeInTypesLeftToRightSkippingConstraints csenv.g (tys@argtys@ Option.toList rty))

/// Re-solve the global constraints involving any of the given type variables. 
/// Trait constraints can't always be solved using the pessimistic rules. We only canonicalize 
/// them forcefully (permitWeakResolution=true) prior to generalization. 
and SolveRelevantMemberConstraints (csenv:ConstraintSolverEnv) ndeep permitWeakResolution trace tps =
    RepeatWhileD ndeep
        (fun ndeep -> 
            tps |> AtLeastOneD (fun tp -> 
                /// Normalize the typar 
                let ty = mkTyparTy tp
                if isTyparTy csenv.g ty then 
                    let tp = destTyparTy csenv.g ty
                    SolveRelevantMemberConstraintsForTypar csenv ndeep permitWeakResolution trace tp
                else
                    ResultD false)) 

and SolveRelevantMemberConstraintsForTypar (csenv:ConstraintSolverEnv) ndeep permitWeakResolution trace tp =
    let cxst = csenv.SolverState.ExtraCxs
    let tpn = tp.Stamp
    let cxs = cxst.FindAll tpn
    if isNil cxs then ResultD false else

    if verbose then dprintf "SolveRelevantMemberConstraintsForTypar #cxs = %d, m = %a\n" cxs.Length outputRange csenv.m;
    cxs |> List.iter (fun _ -> cxst.Remove tpn);

    assert (isNil (cxst.FindAll tpn));

    match trace with 
    | NoTrace -> () 
    | WithTrace (Trace actions) -> actions := (fun () -> cxs |> List.iter (fun cx -> cxst.Add(tpn,cx))) :: !actions

    cxs |> AtLeastOneD (fun (traitInfo,m2) -> 
        let csenv = { csenv with m = m2 }
        SolveMemberConstraint csenv permitWeakResolution (ndeep+1) m2 trace traitInfo)

and CanonicalizeRelevantMemberConstraints (csenv:ConstraintSolverEnv) ndeep trace tps =
    SolveRelevantMemberConstraints csenv ndeep true trace tps 

  
and AddMemberConstraint (csenv:ConstraintSolverEnv) ndeep m2 trace traitInfo support frees =
    let g = csenv.g
    let aenv = csenv.EquivEnv
    let cxst = csenv.SolverState.ExtraCxs

    // Write the constraint into the global table. That is,
    // associate the constraint with each type variable in the free variables of the constraint.
    // This will mean the constraint gets resolved whenever one of these free variables gets solved.
    frees |> List.iter (fun tp -> 
        let tpn = tp.Stamp

        let cxs = cxst.FindAll tpn
        if verbose then dprintf "AddMemberConstraint: tpn = %d, #cxs = %d, m = %a\n" tpn cxs.Length outputRange csenv.m;
        if verbose && cxs.Length > 10 then 
            cxs |> List.iter (fun (cx,_) -> dprintf "     --> cx = %s, fvs = %s\n" (Layout.showL (traitL cx)) (Layout.showL (typarsL (GetFreeTyparsOfMemberConstraint csenv cx))));

        // check the constraint is not already listed for this type variable
        if not (cxs |> List.exists (fun (traitInfo2,_) -> traitsAEquiv g aenv traitInfo traitInfo2)) then 
            match trace with 
            | NoTrace -> () 
            | WithTrace (Trace actions) -> actions := (fun () -> csenv.SolverState.ExtraCxs.Remove tpn) :: !actions
            csenv.SolverState.ExtraCxs.Add (tpn,(traitInfo,m2))
    );

    // Associate the constraint with each type variable in the support, so if the type variable
    // gets generalized then this constraint is attached at the binding site.
    support |> IterateD (fun tp -> AddConstraint csenv ndeep m2 trace tp (TTyparMayResolveMemberConstraint(traitInfo,m2)))

    
/// Record a constraint on an inference type variable. 
and AddConstraint (csenv:ConstraintSolverEnv) ndeep m2 trace tp newConstraint  =
    let g = csenv.g
    let aenv = csenv.EquivEnv
    let amap = csenv.amap
    let denv = csenv.DisplayEnv
    let m = csenv.m


    let consistent tpc1 tpc2 =
        match tpc1,tpc2 with           
        | (TTyparMayResolveMemberConstraint(TTrait(tys1,nm1,memFlags1,argtys1,rty1,_),_),
           TTyparMayResolveMemberConstraint(TTrait(tys2,nm2,memFlags2,argtys2,rty2,_),_))  
              when (memFlags1 = memFlags2 &&
                    nm1 = nm2 &&
                    argtys1.Length = argtys2.Length &&
                    List.lengthsEqAndForall2 (typeEquiv g) tys1 tys2) -> 

                  let rty1 = GetFSharpViewOfReturnType g rty1
                  let rty2 = GetFSharpViewOfReturnType g rty2
                  Iterate2D (SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace) argtys1 argtys2 ++ (fun () -> 
                      SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace rty1 rty2 ++ (fun () -> 
                         if verbose then dprintf "\n-------------\nmerged constraint for %s, tp = %s\n---------\n" nm1 (Layout.showL (typarDeclL tp));
                         CompleteD))
          
        | (TTyparCoercesToType(ty1,_), 
           TTyparCoercesToType(ty2,_)) -> 


              // Record at most one subtype constraint for each head type. 
              // That is, we forbid constraints by both I<string> and I<int>. 
              // This works because the types on the r.h.s. of subtype 
              // constraints are head-types and so any further inferences are equational. 
              let collect ty = 
                  let res = ref [] 
                  IterateEntireHierarchyOfType (fun x -> res := x :: !res) g amap m FirstIntfInst ty; 
                  List.rev !res
              let parents1 = collect ty1
              let parents2 = collect ty2
              parents1 |> IterateD (fun ty1Parent -> 
                 parents2 |> IterateD (fun ty2Parent ->  
                     if not (HaveSameHeadType g ty1Parent ty2Parent) then CompleteD else
                     SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ty1Parent ty2Parent))

        | (TTyparIsEnum (u1,_),
           TTyparIsEnum (u2,m2)) ->   
            SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace u1 u2
            
        | (TTyparIsDelegate (aty1,bty1,_),
           TTyparIsDelegate (aty2,bty2,m2)) ->   
            SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace aty1 aty2 ++ (fun () -> 
            SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace bty1 bty2)

        | TTyparSupportsComparison _,TTyparIsDelegate _  
        | TTyparIsDelegate _ , TTyparSupportsComparison _
        | TTyparIsNotNullableValueType _,TTyparIsReferenceType _     
        | TTyparIsReferenceType _,TTyparIsNotNullableValueType _   ->
            ErrorD (Error(FSComp.SR.csStructConstraintInconsistent(),m))


        | TTyparSupportsComparison _,TTyparSupportsComparison _  
        | TTyparSupportsEquality _,TTyparSupportsEquality _  
        | TTyparSupportsNull _,TTyparSupportsNull _  
        | TTyparIsNotNullableValueType _,TTyparIsNotNullableValueType _     
        | TTyparIsUnmanaged _,TTyparIsUnmanaged _
        | TTyparIsReferenceType _,TTyparIsReferenceType _ 
        | TTyparRequiresDefaultConstructor _,TTyparRequiresDefaultConstructor _ 
        | TTyparSimpleChoice (_,_),TTyparSimpleChoice (_,_) -> 
            CompleteD
            
        | _ -> CompleteD

    // See when one constraint implies implies another. 
    // 'a :> ty1  implies 'a :> 'ty2 if the head type name of ty2 (say T2) occursCheck anywhere in the heirarchy of ty1 
    // If it does occcur, e.g. at instantiation T2<inst2>, then the check above will have enforced that 
    // T2<inst2> = ty2 
    let implies tpc1 tpc2 = 
        match tpc1,tpc2 with           
        | TTyparMayResolveMemberConstraint(trait1,_),
          TTyparMayResolveMemberConstraint(trait2,_) -> 
            traitsAEquiv g aenv trait1 trait2

        | TTyparCoercesToType(ty1,_),TTyparCoercesToType(ty2,_) -> 
              ExistsSameHeadTypeInHierarchy g amap m ty1 ty2

        | TTyparIsEnum(u1,_),TTyparIsEnum(u2,_) -> typeEquiv g u1 u2

        | TTyparIsDelegate(aty1,bty1,_),TTyparIsDelegate(aty2,bty2,_) -> 
            typeEquiv g aty1 aty2 && typeEquiv g bty1 bty2 

        | TTyparSupportsComparison _,TTyparSupportsComparison _  
        | TTyparSupportsEquality _,TTyparSupportsEquality _  
        // comparison implies equality
        | TTyparSupportsComparison _,TTyparSupportsEquality _  
        | TTyparSupportsNull _,TTyparSupportsNull _  
        | TTyparIsNotNullableValueType _,TTyparIsNotNullableValueType _     
        | TTyparIsUnmanaged _, TTyparIsUnmanaged _
        | TTyparIsReferenceType _,TTyparIsReferenceType _ 
        | TTyparRequiresDefaultConstructor _,TTyparRequiresDefaultConstructor _ -> true
        | TTyparSimpleChoice (tys1,_),TTyparSimpleChoice (tys2,_) -> ListSet.isSubsetOf (typeEquiv g) tys1 tys2
        | TTyparDefaultsToType (priority1,dty1,_), TTyparDefaultsToType (priority2,dty2,_) -> 
             (priority1 = priority2) && typeEquiv g dty1 dty2
        | _ -> false

        
    
    // First ensure constraint conforms with existing constraints 
    // NOTE: QUADRATIC 
    let existingConstraints = tp.Constraints

    let allCxs = newConstraint :: List.rev existingConstraints
    begin 
        let rec enforceMutualConsistency i cxs = 
            match cxs with 
            | [] ->  CompleteD
            | cx :: rest -> IterateIdxD (fun j cx2 -> if i = j then CompleteD else consistent cx cx2) allCxs ++ (fun () -> enforceMutualConsistency (i+1) rest)

        enforceMutualConsistency 0 allCxs 
    end ++ (fun ()  ->
    
    let impliedByExistingConstraints = existingConstraints |> List.exists (fun tpc2 -> implies tpc2 newConstraint) 
    
    if impliedByExistingConstraints then 
        CompleteD
    elif tp.Rigidity = TyparRigid then 
        ErrorD (ConstraintSolverMissingConstraint(denv,tp,newConstraint,m,m2)) 
    else
       (// It is important that we give a warning if a constraint is missing from a 
        // will-be-made-rigid type variable. This is because the existence of these warnings
        // is relevant to the overload resolution rules (see 'candidateWarnCount' in the overload resolution
        // implementation). See also FSharp 1.0 bug 5461
        (if tp.Rigidity.WarnIfMissingConstraint then
            WarnD (ConstraintSolverMissingConstraint(denv,tp,newConstraint,m,m2)) 
         else
            CompleteD) ++ (fun () -> 

        let newConstraints = 
              // Eliminate any constraints where one constraint implies another 
              // Keep constraints in the left-to-right form according to the order they are asserted. 
              // NOTE: QUADRATIC 
              let rec eliminateRedundant cxs acc = 
                  match cxs with 
                  | [] ->  acc
                  | cx :: rest -> 
                      eliminateRedundant rest (if List.exists (fun cx2 -> implies cx2 cx) acc then acc else (cx::acc))
                  
              eliminateRedundant allCxs []
              

        // Write the constraint into the type variable 
        // Record a entry in the undo trace if one is provided 
        let d = tp.Data
        let orig = d.typar_constraints
        begin match trace with 
        | NoTrace -> () 
        | WithTrace (Trace actions) -> actions := (fun () -> d.typar_constraints <- orig) :: !actions
        end;
        d.typar_constraints <- newConstraints;

        CompleteD)))

and SolveTypSupportsNull (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    let g = csenv.g
    let m = csenv.m
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then 
        AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparSupportsNull(m))
    elif 
        TypeSatisfiesNullConstraint g ty then CompleteD
    else 
        ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotHaveNull(NicePrint.prettyStringOfTy denv ty),m,m2))

and SolveTypeSupportsComparison (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    let g = csenv.g
    let m = csenv.m
    let amap = csenv.amap
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then 
        AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparSupportsComparison(m))
    // Check it isn't ruled out by the user
    elif isAppTy g ty && HasAttrib g g.attrib_NoComparisonAttribute (tcrefOfAppTy g ty).Attribs then
        ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportComparison1(NicePrint.prettyStringOfTy denv ty),m,m2))
    else        
        match ty with 
        | SpecialComparableHeadType g tinst -> 
            tinst |> IterateD (SolveTypeSupportsComparison (csenv:ConstraintSolverEnv) ndeep m2 trace)
        | _ -> 
           // Check the basic requirement - IComparable or IStructuralComparable or assumed
           if ExistsSameHeadTypeInHierarchy g amap m2 ty g.mk_IComparable_ty  ||
              ExistsSameHeadTypeInHierarchy g amap m2 ty g.mk_IStructuralComparable_ty   then 

               // The type is comparable because it implements IComparable
                if isAppTy g ty then 
                    let tcref,tinst = destAppTy g ty 
                    // Check the (possibly inferred) structural dependencies
                    (tinst, tcref.TyparsNoRange) ||> Iterate2D (fun ty tp -> 
                        if tp.ComparisonConditionalOn then 
                            SolveTypeSupportsComparison (csenv:ConstraintSolverEnv) ndeep m2 trace ty 
                        else 
                            CompleteD) 
                else
                    CompleteD

           // Give a good error for structural types excluded from the comparison relation because of their fields
           elif (isAppTy g ty && 
                 let tcref = tcrefOfAppTy g ty 
                 Augment.TyconIsCandidateForAugmentationWithCompare g tcref.Deref && 
                 isNone tcref.GeneratedCompareToWithComparerValues) then
 
               ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportComparison3(NicePrint.prettyStringOfTy denv ty),m,m2))

           else 
               ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportComparison2(NicePrint.prettyStringOfTy denv ty),m,m2))

and SolveTypSupportsEquality (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    let g = csenv.g
    let m = csenv.m
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then 
        AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparSupportsEquality(m))
    elif isAppTy g ty && HasAttrib g g.attrib_NoEqualityAttribute (tcrefOfAppTy g ty).Attribs then
        ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportEquality1(NicePrint.prettyStringOfTy denv ty),m,m2))
    else 
        match ty with 
        | SpecialEquatableHeadType g tinst -> 
            tinst |> IterateD (SolveTypSupportsEquality (csenv:ConstraintSolverEnv) ndeep m2 trace)
        | SpecialNotEquatableHeadType g _ -> 
            ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportEquality2(NicePrint.prettyStringOfTy denv ty),m,m2))
        | _ -> 
           // The type is equatable because it has Object.Equals(...)
           if isAppTy g ty then 
               let tcref,tinst = destAppTy g ty 

               // Give a good error for structural types excluded from the equality relation because of their fields
               if (Augment.TyconIsCandidateForAugmentationWithEquals g tcref.Deref && 
                   isNone tcref.GeneratedHashAndEqualsWithComparerValues) then

                   ErrorD (ConstraintSolverError(FSComp.SR.csTypeDoesNotSupportEquality3(NicePrint.prettyStringOfTy denv ty),m,m2))

               else
                   // Check the (possibly inferred) structural dependencies
                   (tinst, tcref.TyparsNoRange) ||> Iterate2D (fun ty tp -> 
                       if tp.EqualityConditionalOn then 
                           SolveTypSupportsEquality (csenv:ConstraintSolverEnv) ndeep m2 trace ty 
                       else 
                           CompleteD) 
           else
               CompleteD
           
and SolveTypIsEnum (csenv:ConstraintSolverEnv) ndeep m2 trace ty underlying =
    trackErrors {
        let g = csenv.g
        let m = csenv.m
        let denv = csenv.DisplayEnv
        if isTyparTy g ty then 
            return! AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparIsEnum(underlying,m))
        elif isEnumTy g ty then 
            do! SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace underlying (underlyingTypeOfEnumTy g ty) 
            return! CompleteD
        else 
            return! ErrorD (ConstraintSolverError(FSComp.SR.csTypeIsNotEnumType(NicePrint.prettyStringOfTy denv ty),m,m2))
    }

and SolveTypIsDelegate (csenv:ConstraintSolverEnv) ndeep m2 trace ty aty bty =
    trackErrors {
        let g = csenv.g
        let m = csenv.m
        let denv = csenv.DisplayEnv
        if isTyparTy g ty then 
            return! AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparIsDelegate(aty,bty,m))
        elif isDelegateTy g ty then 
            match TryDestStandardDelegateTyp csenv.InfoReader m AccessibleFromSomewhere ty with 
            | Some (tupledArgTy,rty) ->
                do! SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace aty tupledArgTy 
                do! SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace bty rty 
            | None ->
                return! ErrorD (ConstraintSolverError(FSComp.SR.csTypeHasNonStandardDelegateType(NicePrint.prettyStringOfTy denv ty),m,m2))
        else 
            return! ErrorD (ConstraintSolverError(FSComp.SR.csTypeIsNotDelegateType(NicePrint.prettyStringOfTy denv ty),m,m2))
    }
    
and SolveTypIsNonNullableValueType (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    trackErrors {
        let g = csenv.g
        let m = csenv.m
        let denv = csenv.DisplayEnv
        if isTyparTy g ty then 
            return! AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparIsNotNullableValueType(m))
        else
            let underlyingTy = stripTyEqnsAndMeasureEqns g ty
            if isStructTy g underlyingTy then
                if tyconRefEq g g.system_Nullable_tcref (tcrefOfAppTy g underlyingTy) then
                    return! ErrorD (ConstraintSolverError(FSComp.SR.csTypeParameterCannotBeNullable(),m,m))
            else
                return! ErrorD (ConstraintSolverError(FSComp.SR.csGenericConstructRequiresStructType(NicePrint.prettyStringOfTy denv ty),m,m2))
    }            

and SolveTypIsUnmanaged (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    let g = csenv.g
    let m = csenv.m
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then 
        AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparIsUnmanaged(m))
    else
        let underlyingTy = stripTyEqnsAndMeasureEqns g ty
        if isUnmanagedTy g underlyingTy then
            CompleteD
        else
            ErrorD (ConstraintSolverError(FSComp.SR.csGenericConstructRequiresUnmanagedType(NicePrint.prettyStringOfTy denv ty),m,m2))


and SolveTypChoice (csenv:ConstraintSolverEnv) ndeep m2 trace ty tys =
    let g = csenv.g
    let m = csenv.m
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparSimpleChoice(tys,m)) else 
    match stripTyEqns g ty with
    | TType_app (tc2,[ms]) when tc2.IsMeasureableReprTycon ->
        SolveTypEqualsTypKeepAbbrevs csenv ndeep m2 trace ms (TType_measure MeasureOne) 
    | _ ->
        if List.exists (typeEquiv g ty) tys then CompleteD
        else ErrorD (ConstraintSolverError(FSComp.SR.csTypeNotCompatibleBecauseOfPrintf((NicePrint.prettyStringOfTy denv ty), (String.concat "," (List.map (NicePrint.prettyStringOfTy denv) tys))),m,m2))


and SolveTypIsReferenceType (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    let g = csenv.g
    let m = csenv.m
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then AddConstraint csenv ndeep m2 trace (destTyparTy g ty)  (TTyparIsReferenceType(m))
    elif isRefTy g ty then CompleteD
    else ErrorD (ConstraintSolverError(FSComp.SR.csGenericConstructRequiresReferenceSemantics(NicePrint.prettyStringOfTy denv ty),m,m))

and SolveTypRequiresDefaultConstructor (csenv:ConstraintSolverEnv) ndeep m2 trace ty =
    let g = csenv.g
    let amap = csenv.amap
    let m = csenv.m
    let denv = csenv.DisplayEnv
    if isTyparTy g ty then 
        AddConstraint csenv ndeep m2 trace (destTyparTy g ty) (TTyparRequiresDefaultConstructor(m))
    elif isStructTy g ty && TypeHasDefaultValue g ty then 
        CompleteD
    elif
        GetIntrinsicConstructorInfosOfType csenv.InfoReader m ty 
        |> List.filter (IsMethInfoAccessible amap m AccessibleFromEverywhere)   
        |> List.exists (fun x -> x.IsNullary)
           then 
       if (isAppTy g ty && HasAttrib g g.attrib_AbstractClassAttribute (tcrefOfAppTy g ty).Attribs) then 
         ErrorD (ConstraintSolverError(FSComp.SR.csGenericConstructRequiresNonAbstract(NicePrint.prettyStringOfTy denv ty),m,m2))
       else
         CompleteD
    elif isAppTy g ty && (tcrefOfAppTy g ty).PreEstablishedHasDefaultConstructor then 
        CompleteD
    else 
        ErrorD (ConstraintSolverError(FSComp.SR.csGenericConstructRequiresPublicDefaultConstructor(NicePrint.prettyStringOfTy denv ty),m,m2))
     

// Parameterized compatibility relation between member signatures.  The real work
// is done by "equateTypes" and "subsumeTypes" and "subsumeArg"
and CanMemberSigsMatchUpToCheck 
      (csenv:ConstraintSolverEnv) 
      permitOptArgs // are we allowed to supply optional and/or "param" arguments?
      alwaysCheckReturn // always check the return type?
      unifyTypes   // used to equate the formal method instantiation with the actual method instantiation for a generic method, and the return types
      subsumeTypes  // used to compare the "obj" type 
      (subsumeArg: CalledArg -> CallerArg<_> -> OperationResult<unit>)    // used to compare the arguments for compatibility
      reqdRetTyOpt 
      calledMeth : ImperativeOperationResult =

    let g    = csenv.g
    let amap = csenv.amap
    let m    = csenv.m
    
    let (CalledMeth(minfo,
                    minst,
                    uminst,
                    callerObjArgTys,
                    _,
                    methodRetTy,
                    assignedNamedProps,
                    _,
                    _,
                    _,
                    unnamedCalledOptArgs,
                    unnamedCalledOutArgs)) = calledMeth

    // First equate the method instantiation (if any) with the method type parameters 
    if minst.Length <> uminst.Length then ErrorD(Error(FSComp.SR.csTypeInstantiationLengthMismatch(),m)) else
    
    Iterate2D unifyTypes minst uminst ++ (fun () -> 

    if not (permitOptArgs || isNil(unnamedCalledOptArgs)) then ErrorD(Error(FSComp.SR.csOptionalArgumentNotPermittedHere(),m)) else
    

    let calledObjArgTys = ObjTypesOfMethInfo amap m minfo minst
    
    // Check all the argument types. 

    if calledObjArgTys.Length <> callerObjArgTys.Length then 
        if (calledObjArgTys.Length <> 0) then
            ErrorD(Error (FSComp.SR.csMemberIsNotStatic(minfo.LogicalName),m))
        else
            ErrorD(Error (FSComp.SR.csMemberIsNotInstance(minfo.LogicalName),m))

    else
        Iterate2D subsumeTypes calledObjArgTys callerObjArgTys ++ (fun () -> 
        (calledMeth.ArgSets |> IterateD (fun argSet -> 
            if argSet.UnnamedCalledArgs.Length <> argSet.UnnamedCallerArgs.Length then ErrorD(Error(FSComp.SR.csArgumentLengthMismatch(),m)) else
            Iterate2D subsumeArg argSet.UnnamedCalledArgs argSet.UnnamedCallerArgs)) ++ (fun () -> 
        (calledMeth.ParamArrayCalledArgOpt |> OptionD (fun calledArg ->
            if isArray1DTy g calledArg.Type then 
                let ety = destArrayTy g calledArg.Type
                calledMeth.ParamArrayCallerArgs |> OptionD (IterateD (fun callerArg -> subsumeArg (CalledArg((0,0),false,NotOptional,false,None,ety)) callerArg))
            else
                CompleteD)
        
        ) ++ (fun () -> 
        (calledMeth.ArgSets |> IterateD (fun argSet -> 
            argSet.AssignedNamedArgs |> IterateD (fun (AssignedCalledArg(_,called,caller)) -> subsumeArg called caller)))  ++ (fun () -> 
        (assignedNamedProps |> IterateD (fun (AssignedItemSetter(_,item,caller)) -> 
            let name, calledArgTy = 
                match item with
                | AssignedPropSetter(_,pminfo,pminst) -> 
                    let calledArgTy = List.head (List.head (ParamTypesOfMethInfo amap m pminfo pminst))
                    pminfo.LogicalName, calledArgTy

                | AssignedIlFieldSetter(finfo) ->
                    (* Get or set instance IL field *)
                    let calledArgTy = finfo.FieldType(amap,m)
                    finfo.FieldName, calledArgTy
                
                | AssignedRecdFieldSetter(rfinfo) ->
                    let calledArgTy = rfinfo.FieldType
                    rfinfo.Name, calledArgTy
            
            subsumeArg (CalledArg((-1,0),false, NotOptional,false,Some(name), calledArgTy)) caller) )) ++ (fun () -> 
        
        // - Always take the return type into account for
        //      -- op_Explicit, op_Implicit
        //      -- methods using tupling of unfilled out args
        // - Never take into account return type information for constructors 
        match reqdRetTyOpt with 
        | None -> CompleteD 
        | Some _  when minfo.IsConstructor -> CompleteD 
        | Some _  when not alwaysCheckReturn && isNil unnamedCalledOutArgs -> CompleteD 
        | Some reqdRetTy -> 
            let methodRetTy = 
                if isNil unnamedCalledOutArgs then 
                    methodRetTy 
                else 
                    let outArgTys = unnamedCalledOutArgs |> List.map (fun (CalledArg(_,_,_,_,_,argty)) -> destByrefTy g argty) 
                    if isUnitTy g methodRetTy then mkTupledTy g outArgTys
                    else mkTupledTy g (methodRetTy :: outArgTys)
            unifyTypes reqdRetTy methodRetTy )))))

//-------------------------------------------------------------------------
// Resolve IL overloading. 
// 
// This utilizes the type inference constraint solving engine in undo mode.
//------------------------------------------------------------------------- 


// F# supports two adhoc conversions at method callsites (note C# supports more, though ones 
// such as implicit conversions interact badly with type inference). 
// The first is the use of "(fun x y -> ...)" when  a delegate it expected. This is not part of 
// the ":>" coercion relationship or inference constraint problem as 
// such, but is a special rule applied only to method arguments. 
// 
// The function AdjustCalledArgType detects this case based on types and needs to know that the type being applied 
// is a function type. 
// 
// The other conversion supported is the two ways to pass a value where a byref is expxected. 
// The first (default) is to use a reference cell, and the interioer address is taken automatically 
// The second is an explicit use of the "address-of" operator "&e". Here we detect the second case, 
// and record the presence of the sytnax "&e" in the pre-inferred actual type for the method argument. 
// The function AdjustCalledArgType detects this and refuses to apply the default byref-to-ref transformation. 
//
// The function AdjustCalledArgType also adjusts for optional arguments. 
and AdjustCalledArgType (csenv:ConstraintSolverEnv) (CalledArg(_,_,optArgInfo,_,_,calledArgTy)) (CallerArg(callerArgTy,m,isOptCallerArg,_)) =
    (* If the called method argument is a byref type, then the caller may provide a byref or ref *)
    let g = csenv.g
    if isByrefTy g calledArgTy then
        if isByrefTy g callerArgTy then 
            calledArgTy
        else 
            mkRefCellTy g (destByrefTy g calledArgTy)  
    else 
        // If the called method argument is a delegate type, then the caller may provide a function 
        let calledArgTy = 
            if isDelegateTy g calledArgTy && isFunTy g callerArgTy then 
                let _,delArgTys,_,fty = GetSigOfFunctionForDelegate csenv.InfoReader calledArgTy m  AccessibleFromSomeFSharpCode
                let delArgTys = (if isNil delArgTys then [g.unit_ty] else delArgTys)
                if (fst (stripFunTy g callerArgTy)).Length = delArgTys.Length
                then fty 
                else calledArgTy 
            else calledArgTy

        // Adjust the called argument type to take into account whether the caller's argument is M(?arg=Some(3)) or M(arg=1) 
        // If the called method argument is optional with type Option<T>, then the caller may provide a T, unless their argument is propogating-optional (i.e. isOptCallerArg) 
        let calledArgTy = 
            match optArgInfo with 
            | NotOptional                    -> calledArgTy
            | CalleeSide when not isOptCallerArg && isOptionTy g calledArgTy  -> destOptionTy g calledArgTy
            | CalleeSide | CallerSide _ -> calledArgTy
        calledArgTy
        

and private DefinitelyEquiv (csenv:ConstraintSolverEnv) calledArg (CallerArg(callerArgTy,m,_,_) as callerArg) = 
    let calledArgTy = AdjustCalledArgType csenv calledArg callerArg
    if not (typeEquiv csenv.g calledArgTy callerArgTy) then ErrorD(Error(FSComp.SR.csArgumentTypesDoNotMatch(),m)) else
    CompleteD
  
// Assert a subtype constraint, and wrap an ErrorsFromAddingSubsumptionConstraint error around any failure 
// to allow us to report the outer types involved in the constraint 
and private SolveTypSubsumesTypWithReport (csenv:ConstraintSolverEnv) ndeep m trace ty1 ty2 = 
    TryD (fun () -> SolveTypSubsumesTypKeepAbbrevs csenv ndeep m trace ty1 ty2)
         (fun res -> ErrorD (ErrorsFromAddingSubsumptionConstraint(csenv.g,csenv.DisplayEnv,ty1,ty2,res,m)))

and private SolveTypEqualsTypWithReport (csenv:ConstraintSolverEnv) ndeep  m trace ty1 ty2 = 
    TryD (fun () -> SolveTypEqualsTypKeepAbbrevs csenv ndeep m trace ty1 ty2)
         (fun res -> ErrorD (ErrorFromAddingTypeEquation(csenv.g,csenv.DisplayEnv,ty1,ty2,res,m)))
  
and ArgsMustSubsumeOrConvert 
        (csenv:ConstraintSolverEnv)
        ndeep
        trace
        (CalledArg(_,isParamArrayArg,_,_,_,_) as calledArg) 
        (CallerArg(callerArgTy,m,_,_) as callerArg) = 
        
    let g = csenv.g
    let amap = csenv.amap
    let calledArgTy = AdjustCalledArgType csenv calledArg callerArg
    SolveTypSubsumesTypWithReport csenv ndeep m trace calledArgTy callerArgTy ++ (fun () -> 

    if isParamArrayArg &&
        isAppTy g calledArgTy &&
        (let _,tinstf = destAppTy g calledArgTy
         tinstf.Length  = 1 &&
         TypesFeasiblyEquiv ndeep g amap m (List.head tinstf) callerArgTy)
    then 
        ErrorD(Error(FSComp.SR.csMethodExpectsParams(),m))
    else
        CompleteD)

and MustUnify csenv ndeep trace ty1 ty2 = 
    SolveTypEqualsTypWithReport csenv ndeep csenv.m trace ty1 ty2

and MustUnifyInsideUndo csenv ndeep trace ty1 ty2 = 
    SolveTypEqualsTypWithReport csenv ndeep csenv.m (WithTrace trace) ty1 ty2

and ArgsMustSubsumeOrConvertInsideUndo (csenv:ConstraintSolverEnv) ndeep trace calledArg (CallerArg(callerArgTy,m,_,_) as callerArg) = 
    let calledArgTy = AdjustCalledArgType csenv calledArg callerArg
    SolveTypSubsumesTypWithReport csenv ndeep  m (WithTrace trace) calledArgTy callerArgTy 

and TypesMustSubsumeOrConvertInsideUndo (csenv:ConstraintSolverEnv) ndeep trace m calledArgTy callerArgTy = 
    SolveTypSubsumesTypWithReport csenv ndeep m trace calledArgTy callerArgTy 

and ArgsEquivInsideUndo (csenv:ConstraintSolverEnv) _trace calledArg (CallerArg(callerArgTy,m,_,_) as callerArg) = 
    let calledArgTy = AdjustCalledArgType csenv calledArg callerArg
    if not (typeEquiv csenv.g calledArgTy callerArgTy) then ErrorD(Error(FSComp.SR.csArgumentTypesDoNotMatch(),m)) else
    CompleteD

and ReportNoCandidatesError (csenv:ConstraintSolverEnv) (nUnnamedCallerArgs,nNamedCallerArgs) methodName ad (calledMethGroup:CalledMeth<_> list) =

    let amap = csenv.amap
    let m    = csenv.m
    let denv = csenv.DisplayEnv

    match (calledMethGroup |> List.partition (CalledMeth.GetMethod >> IsMethInfoAccessible amap m ad)),
          (calledMethGroup |> List.partition (fun cmeth -> cmeth.HasCorrectObjArgs(amap,m,ad))),
          (calledMethGroup |> List.partition (fun cmeth -> cmeth.HasCorrectArity)),
          (calledMethGroup |> List.partition (fun cmeth -> cmeth.HasCorrectGenericArity)),
          (calledMethGroup  |> List.partition (fun cmeth -> cmeth.AssignsAllNamedArgs)) with

    // No version accessible 
    | ([],others),_,_,_,_ ->  
        if nonNil others then
            ErrorD (Error (FSComp.SR.csMemberIsNotAccessible2(methodName, (showAccessDomain ad)), m))
        else
            ErrorD (Error (FSComp.SR.csMemberIsNotAccessible(methodName, (showAccessDomain ad)), m))
    | _,([],(cmeth::_)),_,_,_ ->  
    
        // Check all the argument types. 

        if (cmeth.CalledObjArgTys(amap,m).Length <> 0) then
            ErrorD(Error (FSComp.SR.csMethodIsNotAStaticMethod(methodName),m))
        else
            ErrorD(Error (FSComp.SR.csMethodIsNotAnInstanceMethod(methodName),m))

    // One method, incorrect name/arg assignment 
    | _,_,_,_,([],[cmeth]) -> 
        let msg = 
            List.foldBack 
               (fun (CallerNamedArg(id,_)) (n,acc) -> (n,FSComp.SR.csMemberHasNoArgumentOrReturnProperty(methodName, id.idText, acc)))
               cmeth.UnassignedNamedArgs
               (FSComp.SR.csRequiredSignatureIs(stringOfMethInfo amap m denv cmeth.Method))
        ErrorD (Error (msg,m))

    // One method, incorrect number of arguments provided by the user
    | _,_,([],[cmeth]),_,_ when not cmeth.HasCorrectArity ->  
        let minfo = cmeth.Method
        let nReqd = cmeth.TotalNumUnnamedCalledArgs
        let nReqdNamed = cmeth.TotalNumAssignedNamedArgs
        let nActual = cmeth.TotalNumUnnamedCallerArgs
        let nreqdTyArgs = cmeth.NumCalledTyArgs
        let nactualTyArgs = cmeth.NumCallerTyArgs
        if nActual <> nReqd then 
            if nReqdNamed > 0 || cmeth.NumAssignedProps > 0 then 
                if nReqd > nActual then 
                    let errid =
                        let suggestNamesForMissingArguments = 
                            if nReqd > nActual then 
                                let missingArgs = List.drop nReqd cmeth.AllUnnamedCalledArgs
                                match NamesOfCalledArgs missingArgs with 
                                | [] -> (false, "")
                                | names -> (true, String.concat ";" names)
                            else (false, "")

                        match suggestNamesForMissingArguments with
                        | false, _ -> if nActual = 0 then (1, "") else (2, "")
                        | true, str -> if nActual = 0 then (3, str) else (4, str)

                    match errid with
                    | 1, _ -> ErrorD (Error (FSComp.SR.csMemberSignatureMismatch(methodName, (nReqd-nActual), (stringOfMethInfo amap m denv minfo)), m))
                    | 2, _ -> ErrorD (Error (FSComp.SR.csMemberSignatureMismatch2(methodName, (nReqd-nActual), (stringOfMethInfo amap m denv minfo)), m))
                    | 3, str -> ErrorD (Error (FSComp.SR.csMemberSignatureMismatch3(methodName, (nReqd-nActual), (stringOfMethInfo amap m denv minfo), str), m))
                    | 4, str -> ErrorD (Error (FSComp.SR.csMemberSignatureMismatch4(methodName, (nReqd-nActual), (stringOfMethInfo amap m denv minfo), str), m))
                    | _ -> failwith "unreachable"
                else 
                    ErrorD (Error (FSComp.SR.csMemberSignatureMismatchArityNamed(methodName, (nReqd+nReqdNamed), nActual, nReqdNamed, (stringOfMethInfo amap m denv minfo)), m))
            else
                ErrorD (Error (FSComp.SR.csMemberSignatureMismatchArity(methodName, nReqd, nActual, (stringOfMethInfo amap m denv minfo)), m))
        else 
            ErrorD (Error (FSComp.SR.csMemberSignatureMismatchArityType(methodName, nreqdTyArgs, nactualTyArgs, (stringOfMethInfo amap m denv minfo)), m))

    // One or more accessible, all the same arity, none correct 
    | ((cmeth :: cmeths2),_),_,_,_,_ when not cmeth.HasCorrectArity && cmeths2 |> List.forall (fun cmeth2 -> cmeth.TotalNumUnnamedCalledArgs = cmeth2.TotalNumUnnamedCalledArgs) -> 
        ErrorD (Error (FSComp.SR.csMemberNotAccessible(methodName, (cmeth.ArgSets |> List.sumBy (fun x -> x.NumUnnamedCalledArgs)), methodName, cmeth.TotalNumUnnamedCalledArgs),m))

    // Many methods, all with incorrect number of generic arguments
    | _,_,_,([],(cmeth :: _)),_ -> 
        let msg = FSComp.SR.csIncorrectGenericInstantiation((showAccessDomain ad), methodName, cmeth.NumCallerTyArgs)
        ErrorD (Error (msg,m))
    // Many methods of different arities, all incorrect 
    | _,_,([],(cmeth :: _)),_,_ -> 
        let minfo = cmeth.Method
        ErrorD (Error (FSComp.SR.csMemberOverloadArityMismatch(methodName, cmeth.TotalNumUnnamedCallerArgs, (List.sum minfo.NumArgs)),m))
    | _ -> 
        let msg = 
            if nNamedCallerArgs = 0 then 
                FSComp.SR.csNoMemberTakesTheseArguments((showAccessDomain ad), methodName, nUnnamedCallerArgs)
            else 
                let s = calledMethGroup |> List.map (fun cmeth -> cmeth.UnassignedNamedArgs |> List.map (fun na -> na.Name)|> Set.ofList) |> Set.intersectMany
                if s.IsEmpty then 
                    FSComp.SR.csNoMemberTakesTheseArguments2((showAccessDomain ad), methodName, nUnnamedCallerArgs, nNamedCallerArgs)
                else 
                    let sample = s.MinimumElement
                    FSComp.SR.csNoMemberTakesTheseArguments3((showAccessDomain ad), methodName, nUnnamedCallerArgs, sample)
        ErrorD (Error (msg,m))


// Resolve the overloading of a method 
// This is used after analyzing the types of arguments 
and ResolveOverloading 
         (csenv:ConstraintSolverEnv) 
         trace           // The undo trace, if any
         methodName      // The name of the method being called, for error reporting
         ndeep           // Depth of inference
         _isConstraint    // We're doing overload resolution as part of constraint solving, where special rules apply for op_Explicit and op_Implicit constraints.
         callerArgCounts // How many named/unnamed args id the caller provide? 
         ad              // The access domain of the caller, e.g. a module, type etc. 
         calledMethGroup // The set of methods being called 
         permitOptArgs   // Can we supply optional arguments?
         reqdRetTyOpt    // The expected return type, if known 
     =
    let g = csenv.g
    let amap = csenv.amap
    let m    = csenv.m
    let denv = csenv.DisplayEnv
    let isOpConversion = (methodName = "op_Explicit" || methodName = "op_Implicit")
    // See what candidates we have based on name and arity 
    let candidates = calledMethGroup |> List.filter (fun cmeth -> cmeth.IsCandidate(g,amap,m,ad))
    let calledMethOpt, errors = 

        match calledMethGroup,candidates with 
        | _,[calledMeth] when not isOpConversion -> 
            Some calledMeth, CompleteD

        | [],_ when not isOpConversion -> 
            None, ErrorD (Error (FSComp.SR.csMethodNotFound(methodName),m))

        | _,[] when not isOpConversion -> 
            None, ReportNoCandidatesError csenv callerArgCounts methodName ad calledMethGroup
            
        | _,_ -> 

          // - Always take the return type into account for
          //      -- op_Explicit, op_Implicit
          //      -- candidate method sets that potentially use tupling of unfilled out args
          let alwaysCheckReturn = isOpConversion || candidates |> List.exists (fun cmeth -> cmeth.HasOutArgs)

          // Exact match rule.
          //
          // See what candidates we have based on current inferred type information 
          // and _exact_ matches of argument types. 
          match candidates |> FilterEachThenUndo (fun newTrace calledMeth -> 
                     CanMemberSigsMatchUpToCheck 
                         csenv 
                         permitOptArgs 
                         alwaysCheckReturn
                         (MustUnifyInsideUndo csenv ndeep newTrace) 
                         (TypesMustSubsumeOrConvertInsideUndo csenv ndeep (WithTrace newTrace) m)
                         (ArgsEquivInsideUndo csenv Trace.New) 
                         reqdRetTyOpt 
                         calledMeth) with
          | [(calledMeth,_)] -> 
              Some calledMeth, CompleteD

          | _ -> 
            // Now determine the applicable methods.
            // Subsumption on arguments is allowed.
            let applicable = candidates |> FilterEachThenUndo (fun newTrace candidate -> 
                               CanMemberSigsMatchUpToCheck 
                                   csenv 
                                   permitOptArgs
                                   alwaysCheckReturn
                                   (MustUnifyInsideUndo csenv ndeep newTrace) 
                                   (TypesMustSubsumeOrConvertInsideUndo csenv ndeep (WithTrace newTrace) m)
                                   (ArgsMustSubsumeOrConvertInsideUndo csenv ndeep newTrace) 
                                   reqdRetTyOpt 
                                   candidate) 

            let failOverloading (msg : string) errors = 
                // Try to extract information to give better error for ambiguous op_Explicit and op_Implicit 
                let convOpData = 
                    if isOpConversion then 
                        match calledMethGroup, reqdRetTyOpt with 
                        | h :: _, Some rty -> 
                            Some (h.Method.EnclosingType, rty)
                        | _ -> None 
                    else
                        None

                match convOpData with 
                | Some (fromTy, toTy) -> 
                    UnresolvedConversionOperator (denv, fromTy, toTy, m)
                | None -> 
                    // Otherwise collect a list of possible overloads
                    let overloads = GetPossibleOverloads amap m denv calledMethGroup
                    UnresolvedOverloading (denv, overloads, [], errors, msg, m)

            match applicable with 
            | [] ->
                // OK, we failed. Collect up the errors from overload resolution and the possible overloads
                let errors = 
                    (candidates |> List.choose (fun calledMeth -> 
                            match CollectThenUndo (fun newTrace -> 
                                         CanMemberSigsMatchUpToCheck 
                                             csenv 
                                             permitOptArgs
                                             alwaysCheckReturn
                                             (MustUnifyInsideUndo csenv ndeep newTrace) 
                                             (TypesMustSubsumeOrConvertInsideUndo csenv ndeep (WithTrace newTrace) m)
                                             (ArgsMustSubsumeOrConvertInsideUndo csenv ndeep newTrace) 
                                             reqdRetTyOpt 
                                             calledMeth) with 
                            | OkResult _ -> None
                            | ErrorResult(_,exn) -> Some exn))

                None,ErrorD (failOverloading (FSComp.SR.csNoOverloadsFound methodName) errors)

            | [(calledMeth,_)] -> 
                Some calledMeth, CompleteD

            | applicableMeths -> 
                
                /// Compare two things by the given predicate. 
                /// If the predicate returns true for x1 and false for x2, then x1 > x2
                /// If the predicate returns false for x1 and true for x2, then x1 < x2
                /// Otherwise x1 = x2
                
                // Note: Relies on 'compare' respecting true > false
                let compareCond (p : 'T -> 'T -> bool) x1 x2 = 
                    compare (p x1 x2) (p x2 x1)

                /// Compare types under the feasibly-subsumes ordering
                let compareTypes ty1 ty2 = 
                    (ty1,ty2) ||> compareCond (fun x1 x2 -> TypeFeasiblySubsumesType ndeep csenv.g csenv.amap m x2 CanCoerce x1) 
                    
                /// Compare arguments under the feasibly-subsumes ordering and the adhoc Func-is-better-than-other-delegates rule
                let compareArg (CalledArg(_,_,_,_,_,argType1)) 
                              (CalledArg(_,_,_,_,_,argType2)) =
                    let c = compareTypes argType1 argType2
                    if c <> 0 then c else

                    // Func<_> is always considered better than any other delegate type
                    let c = (argType1, argType2) ||> compareCond (fun ty1 ty2 -> 
                                (match tryDestAppTy csenv.g ty1 with 
                                 | Some tcref1 when 
                                    (tcref1.DisplayName = "Func" &&  
                                     (match tcref1.PublicPath with Some p -> p.EnclosingPath = [| "System" |] | _ -> false) && 
                                     isDelegateTy g ty1 &&
                                     isDelegateTy g ty2) -> true
                                 | _ -> false))
                                 
                    if c <> 0 then c else
                    0

                let better (candidate:CalledMeth<_>, candidateWarnCount) (other:CalledMeth<_>, otherWarnCount) = 
                    // Prefer methods that don't give "this code is less generic" warnings
                    // Note: Relies on 'compare' respecting true > false
                    let c = compare (candidateWarnCount = 0) (otherWarnCount = 0)
                    if c <> 0 then c else
                    
                    // Prefer methods that don't use param array arg
                    // Note: Relies on 'compare' respecting true > false
                    let c =  compare (not candidate.UsesParamArrayConversion) (not other.UsesParamArrayConversion) 
                    if c <> 0 then c else

                    // Prefer methods with more precise param array arg type
                    let c = 
                        if candidate.UsesParamArrayConversion && other.UsesParamArrayConversion then
                            compareTypes (candidate.ParamArrayElementType(g)) (other.ParamArrayElementType(g))
                        else
                            0
                    if c <> 0 then c else
                    
                    // Prefer methods that don't use out args
                    // Note: Relies on 'compare' respecting true > false
                    let c = compare (not candidate.HasOutArgs) (not other.HasOutArgs)
                    if c <> 0 then c else

                    // Prefer methods that don't use optional args
                    // Note: Relies on 'compare' respecting true > false
                    let c = compare (not candidate.HasOptArgs) (not other.HasOptArgs)
                    if c <> 0 then c else

                    // check regular args. The argument counts will only be different if one is using param args
                    let c = 
                        if (candidate.TotalNumUnnamedCalledArgs = other.TotalNumUnnamedCalledArgs) then 
                           
                           // For extension members, we also include the object argument type, if any in the comparison set
                           // THis matches C#, where all extension members are treated and resolved as "static" methods calls
                           let cs = 
                               (if candidate.Method.IsExtensionMember && other.Method.IsExtensionMember then 
                                   let objArgTys1 = candidate.CalledObjArgTys(amap,m) 
                                   let objArgTys2 = other.CalledObjArgTys(amap,m) 
                                   if objArgTys1.Length = objArgTys2.Length then 
                                       List.map2 compareTypes objArgTys1 objArgTys2
                                   else
                                       []
                                else 
                                    []) @
                               ((candidate.AllUnnamedCalledArgs, other.AllUnnamedCalledArgs) ||> List.map2 compareArg ) 
                           // "all args are at least as good, and one argument is actually better"
                           if cs |> List.forall (fun x -> x >= 0) && cs |> List.exists (fun x -> x > 0) then 
                               1
                           // "all args are at least as bad, and one argument is actually worse"
                           elif cs |> List.forall (fun x -> x <= 0) && cs |> List.exists (fun x -> x < 0) then 
                               -1
                           // "argument lists are incomparable"
                           else
                               0
                        else
                            0
                    if c <> 0 then c else

                    // prefer non-extension methods 
                    let c = compare (not candidate.Method.IsExtensionMember) (not other.Method.IsExtensionMember)
                    if c <> 0 then c else

                    // between extension methods, prefer most recently opened
                    let c = 
                        if candidate.Method.IsExtensionMember && other.Method.IsExtensionMember then 
                            compare candidate.Method.Priority other.Method.Priority 
                        else 
                            0
                    if c <> 0 then c else


                    // Prefer non-generic methods 
                    // Note: Relies on 'compare' respecting true > false
                    let c = compare candidate.CalledTyArgs.IsEmpty other.CalledTyArgs.IsEmpty
                    if c <> 0 then c else
                    
                    0
                    

                let bestMethods =
                    applicableMeths |> List.choose (fun candidate -> 
                        if applicableMeths |> List.forall (fun other -> 
                             candidate === other || 
                             let res = better candidate other
                             //eprintfn "\n-------\nCandidate: %s\nOther: %s\nResult: %d\n" (stringOfMethInfo amap m denv (fst candidate).Method) (stringOfMethInfo amap m denv (fst other).Method) res
                             res > 0) then 
                           Some(candidate)
                        else 
                           None) 
                match bestMethods with 
                | [(calledMeth,_)] -> Some(calledMeth), CompleteD
                | _bestMethods -> None, ErrorD (failOverloading (FSComp.SR.csMethodIsOverloaded methodName) [])

    // If we've got a candidate solution: make the final checks - no undo here! 
    // Allow subsumption on arguments. Include the return type.
    // Unify return types.
    match calledMethOpt with 
    | Some(calledMeth) -> 
        calledMethOpt,
        errors ++ (fun () -> CanMemberSigsMatchUpToCheck 
                                 csenv 
                                 permitOptArgs
                                 true
                                 (MustUnify csenv ndeep trace) 
                                 (TypesMustSubsumeOrConvertInsideUndo csenv ndeep trace m)
                                 (ArgsMustSubsumeOrConvert csenv ndeep trace) 
                                 reqdRetTyOpt 
                                 calledMeth)

    | None -> 
        None, errors        


/// This is used before analyzing the types of arguments in a single overload resolution
let UnifyUniqueOverloading 
         (csenv:ConstraintSolverEnv) 
         callerArgCounts 
         methodName 
         ad 
         (calledMethGroup:CalledMeth<Expr> list) 
         reqdRetTy    // The expected return type, if known 
   =
    let g = csenv.g
    let amap = csenv.amap
    let m    = csenv.m
    (* See what candidates we have based on name and arity *)
    let candidates = calledMethGroup |> List.filter (fun cmeth -> cmeth.IsCandidate(g,amap,m,ad)) 
    let ndeep = 0
    match calledMethGroup,candidates with 
    | _,[calledMeth] -> 
        (* Only one candidate found - we thus know the types we expect of arguments *)
        CanMemberSigsMatchUpToCheck 
            csenv 
            true // permitOptArgs
            true // always check return type
            (MustUnify csenv ndeep NoTrace) 
            (TypesMustSubsumeOrConvertInsideUndo csenv ndeep NoTrace m)
            (ArgsMustSubsumeOrConvert csenv ndeep NoTrace) 
            (Some reqdRetTy)
            calledMeth
        ++ (fun () -> ResultD true)
        
    | [],_ -> 
        ErrorD (Error (FSComp.SR.csMethodNotFound(methodName),m))
    | _,[] -> 
        ReportNoCandidatesError csenv callerArgCounts methodName ad calledMethGroup 
        ++ (fun () -> ResultD false)
    | _ -> 
        ResultD false

let EliminateConstraintsForGeneralizedTypars csenv trace (generalizedTypars: Typars) =
    // Remove the global constraints where this type variable appears in the support of the constraint 
    generalizedTypars |> List.iter (fun tp -> 
        let tpn = tp.Stamp
        let cxst = csenv.SolverState.ExtraCxs
        let cxs = cxst.FindAll tpn
        if isNil cxs then () else
        if verbose then dprintf "EliminateConstraintsForGeneralizedTypars: #cxs = %d, m = %a\n" cxs.Length outputRange csenv.m;
        cxs |> List.iter (fun cx -> 
            cxst.Remove tpn;
            match trace with 
            | NoTrace -> () 
            | WithTrace (Trace actions) -> actions := (fun () -> (csenv.SolverState.ExtraCxs.Add (tpn,cx))) :: !actions)
    )


//-------------------------------------------------------------------------
// Main entry points to constraint solver (some backdoors are used for 
// some constructs)
//
// No error recovery here : we do that on a per-expression basis.
//------------------------------------------------------------------------- 

let AddCxTypeEqualsType denv css m ty1 ty2 = 
    SolveTypEqualsTypWithReport (MakeConstraintSolverEnv css m denv) 0 m NoTrace ty1 ty2
    |> RaiseOperationResult

let UndoIfFailed f =
    let trace = Trace.New()
    let res = 
        try f trace |> CheckNoErrorsAndGetWarnings
        with e -> None
    match res with 
    | None -> 
        // Don't report warnings if we failed
        trace.Undo(); false
    | Some warns -> 
        // Report warnings if we succeeded
        ReportWarnings warns; true

let AddCxTypeEqualsTypeUndoIfFailed denv css m ty1 ty2 =
    UndoIfFailed (fun trace -> SolveTypEqualsTypKeepAbbrevs (MakeConstraintSolverEnv css m denv) 0 m (WithTrace(trace)) ty1 ty2)

let AddCxTypeMustSubsumeTypeUndoIfFailed denv css m ty1 ty2 = 
    UndoIfFailed (fun trace -> SolveTypSubsumesTypKeepAbbrevs (MakeConstraintSolverEnv css m denv) 0 m (WithTrace(trace)) ty1 ty2)



let AddCxTypeMustSubsumeType denv css m trace ty1 ty2 = 
    SolveTypSubsumesTypWithReport (MakeConstraintSolverEnv css m denv) 0 m trace ty1 ty2
    |> RaiseOperationResult

let AddCxMethodConstraint denv css m trace traitInfo  =
    TryD (fun () -> SolveMemberConstraint (MakeConstraintSolverEnv css m denv) false 0 m trace traitInfo ++ (fun _ -> CompleteD))
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeMustSupportNull denv css m trace ty =
    TryD (fun () -> SolveTypSupportsNull (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeMustSupportComparison denv css m trace ty =
    TryD (fun () -> SolveTypeSupportsComparison (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeMustSupportEquality denv css m trace ty =
    TryD (fun () -> SolveTypSupportsEquality (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeMustSupportDefaultCtor denv css m trace ty =
    TryD (fun () -> SolveTypRequiresDefaultConstructor (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeIsReferenceType denv css m trace ty =
    TryD (fun () -> SolveTypIsReferenceType (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeIsValueType denv css m trace ty =
    TryD (fun () -> SolveTypIsNonNullableValueType (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult
    
let AddCxTypeIsUnmanaged denv css m trace ty =
    TryD (fun () -> SolveTypIsUnmanaged (MakeConstraintSolverEnv css m denv) 0 m trace ty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeIsEnum denv css m trace ty underlying =
    TryD (fun () -> SolveTypIsEnum (MakeConstraintSolverEnv css m denv) 0 m trace ty underlying)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let AddCxTypeIsDelegate denv css m trace ty aty bty =
    TryD (fun () -> SolveTypIsDelegate (MakeConstraintSolverEnv css m denv) 0 m trace ty aty bty)
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult

let CodegenWitnessThatTypSupportsTraitConstraint g amap m (traitInfo:TraitConstraintInfo) = 
    let css = { g=g;amap=amap;
                ExtraCxs=HashMultiMap(10, HashIdentity.Structural)
                InfoReader=new InfoReader(g,amap) }
    let csenv = MakeConstraintSolverEnv css m (DisplayEnv.Empty g)
    SolveMemberConstraint csenv true 0 m NoTrace traitInfo ++ (fun _res -> 
          match traitInfo.Solution with 
          | None -> ResultD None
          | Some sln ->
              match sln with 
              | ILMethSln(typ,extOpt,mref,minst) ->
                   let tcref,tinst = destAppTy g typ
                   let scoref,enc,tdef = tcref.ILTyconInfo
                   let mdef = IL.resolveILMethodRef tdef mref
                   let tref = IL.mkRefForNestedILTypeDef scoref (enc,tdef)
                   let mtps = Import.ImportIlTypars (fun () -> amap) m scoref tinst mdef.GenericParams
                   ResultD (Some (ILMeth(g,ILMethInfo(ILTypeInfo(tcref,tref,tinst,tdef),extOpt,mdef,mtps),None),minst))
              | FSMethSln(typ, vref,minst) ->
                   ResultD (Some (FSMeth(g,typ,vref,None),minst))
              | BuiltInSln -> 
                   ResultD  None) 

       //| TTraitUnsolved -> ResultD None //ErrorD(InternalError("unsolved trait constraint in codegen",m))
       //| TTraitBuiltIn -> ResultD  None //ErrorD(InternalError("trait constraint was resolved to F# library intrinsic in codegen",m))


let ChooseTyparSolutionAndSolve css denv tp =
    let g = css.g
    let amap = css.amap
    let max,m = ChooseTyparSolutionAndRange g amap tp 
    let csenv = (MakeConstraintSolverEnv css m denv) 
    TryD (fun () -> SolveTyparEqualsTyp csenv 0 m NoTrace (mkTyparTy tp) max)
         (fun err -> ErrorD(ErrorFromApplyingDefault(g,denv,tp,max,err,m)))
    |> RaiseOperationResult



let CheckDeclaredTypars denv css m typars1 typars2 = 
    TryD (fun () -> 
            CollectThenUndo (fun trace -> 
               SolveTypEqualsTypEqns (MakeConstraintSolverEnv css m denv) 0 m (WithTrace(trace)) 
                   (List.map mkTyparTy typars1) 
                   (List.map mkTyparTy typars2)))
         (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m)))
    |> RaiseOperationResult
