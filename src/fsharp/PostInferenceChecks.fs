// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// Implements a set of checks on the TAST for a file that can only be performed after type inference
/// is complete.
module internal Microsoft.FSharp.Compiler.PostTypeCheckSemanticChecks

open System.Collections.Generic
open Internal.Utilities

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics

open Microsoft.FSharp.Compiler.AccessibilityLogic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.TcGlobals
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.InfoReader
open Microsoft.FSharp.Compiler.TypeRelations



//--------------------------------------------------------------------------
// TestHooks - for dumping range to support source transforms
//--------------------------------------------------------------------------

let testFlagMemberBody = ref false
let testHookMemberBody (membInfo: ValMemberInfo) (expr:Expr) =
    if !testFlagMemberBody then
        let m = expr.Range 
        printf "TestMemberBody,%A,%s,%d,%d,%d,%d\n"
          membInfo.MemberFlags.MemberKind
          m.FileName
          m.StartLine
          m.StartColumn
          m.EndLine
          m.EndColumn

//--------------------------------------------------------------------------
// NOTES: byref safety checks
//
// The .NET runtime has safety requirements on the use of byrefs.
//  These include:
//    A1: No generic type/method can be instantiated with byref types (meaning contains byref type).
//    A2: No object field may be byref typed.
//
//  In F# TAST level, byref types can be introduced/consumed at:
//    B1: lambda ... (v:byref<a>) ...         -- binding sites for values.
//    B2: &m                                  -- address of operator, where m is local mutable or reference cell.
//    B3: ms.M()                              -- method calls on mutable structs.
//    B4: *br                                 -- dereference byref
//    B5: br <- x                             -- assign byref
//    B6: expr@[byrefType]                    -- any type instantiation could introduce byref types.
//    B7: asm                                 -- TExpr_asm forms that create/consume byrefs.
//        a) I_ldfld <byref> expr
//        b) I_stfld <byref>
//
//  Closures imply objects.
//  Closures are either:
//    a) explicit lambda expressions.
//    b) functions partially applied below their known arity.
// 
//  Checks:
//    C1: check no instantiation can contain byref types.
//    C2: check type declarations to ensure no object field will have byref type.
//    C3: check no explicit lambda expressions capture any free byref typed expression.    
//    C4: check byref type expr occur only as:
//        C4.a) arg to functions occurring within their known arity.
//        C4.b) arg to IL method calls, e.g. arising from calls to instance methods on mutable structs.
//        C4.c) arg to property getter on mutable struct (record field projection)
//        C4.d) rhs of byref typed binding (aliasing).
//              Note [1] aliasing should not effect safety. The restrictions on RHS byref will also apply to alias.
//              Note [2] aliasing happens in the generated hash/compare code.
//    C5: when is a byref-typed-binding acceptable?    
//        a) if it will be a method local, ok.
//        b) if it will be a top-level value stored as a field, then no. [These should have arity info].
//
//  Check commentary:
//    The C4 checks ensure byref expressions are only passed directly as method arguments (or aliased).
//    The C3 check ensures byref expressions are never captured, e.g. passed as direct method arg under a capturing thunk.
//    The C2 checks no type can store byrefs (C4 ensures F# code would never actually store them).
//    The C1 checks no generic type could be instanced to store byrefs.

//--------------------------------------------------------------------------
// NOTES: reraise safety checks
//--------------------------------------------------------------------------
 
// "rethrow may only occur with-in the body of a catch handler".
//   -- Section 4.23. Part III. CLI Instruction Set. ECMA Draft 2002.
//   
//   1. reraise() calls are converted to TOp.Reraise in the type checker.
//   2. any remaining reraise val_refs will be first class uses. These are trapped.
//   3. The freevars track free TOp.Reraise (they are bound (cleared) at try-catch handlers).
//   4. An outermost expression is not contained in a try-catch handler.
//      These may not have unbound rethrows.      
//      Outermost expressions occur at:
//      * module bindings.
//      * attribute arguments.
//      * Any more? What about fields of a static class?            
//   5. A lambda body (from lambda-expression or method binding) will not occur under a try-catch handler.
//      These may not have unbound rethrows.
//   6. All other constructs are assumed to generate IL code sequences.
//      For correctness, this claim needs to be justified.
//      
//      Q:  Do any post check rewrite passes factor expressions out to other functions?      
//      A1. The optimiser may introduce auxillary functions, e.g. by splitting out match-branches.
//          This should not be done if the refactored body contains an unbound reraise.
//      A2. TLR? Are any expression factored out into functions?
//      
//   Informal justification:
//   If a reraise occurs, then it is minimally contained by either:
//     a) a try-catch - accepted.
//     b) a lambda expression - rejected.
//     c) none of the above - rejected as when checking outmost expressions.



//--------------------------------------------------------------------------
// check environment
//--------------------------------------------------------------------------

type env = 
    { boundTyparNames: string list 
      boundTypars: TyparMap<unit>
      argVals: ValMap<unit>
      /// "module remap info", i.e. hiding information down the signature chain, used to compute what's hidden by a signature
      sigToImplRemapInfo: (Remap * SignatureHidingInfo) list 
      /// Constructor limited - are we in the prelude of a constructor, prior to object initialization
      limited: bool
      /// Are we in a quotation?
      quote : bool 
      /// Are we under [<ReflectedDefinition>]?
      reflect : bool
      /// Are we in an extern declaration?
      external : bool } 

let BindTypar env (tp:Typar) = 
    { env with 
         boundTyparNames = tp.Name :: env.boundTyparNames
         boundTypars = env.boundTypars.Add (tp, ()) } 

let BindTypars g env (tps:Typar list) = 
    let tps = NormalizeDeclaredTyparsForEquiRecursiveInference g tps
    if isNil tps then env else
    // Here we mutate to provide better names for generalized type parameters 
    let nms = PrettyTypes.PrettyTyparNames (fun _ -> true) env.boundTyparNames tps
    (tps,nms) ||> List.iter2 (fun tp nm -> 
            if PrettyTypes.NeedsPrettyTyparName tp  then 
                tp.typar_id <- ident (nm,tp.Range))      
    List.fold BindTypar env tps 

/// Set the set of vals which are arguments in the active lambda. We are allowed to return 
/// byref arguments as byref returns.
let SetArgVals env (vs: Val list) = 
    { env with argVals = ValMap.OfList (List.map (fun v -> (v,())) vs) }

type cenv = 
    { boundVals: Dictionary<Stamp,int> // really a hash set
      mutable potentialUnboundUsesOfVals: StampMap<range> 
      g: TcGlobals 
      amap: Import.ImportMap 
      /// For reading metadata
      infoReader: InfoReader
      internalsVisibleToPaths : CompilationPath list
      denv: DisplayEnv 
      viewCcu : CcuThunk
      reportErrors: bool
      isLastCompiland : bool*bool
      // outputs
      mutable usesQuotations : bool
      mutable entryPointGiven:bool  }

let BindVal cenv env (v:Val) = 
    //printfn "binding %s..." v.DisplayName
    let alreadyDone = cenv.boundVals.ContainsKey v.Stamp
    cenv.boundVals.[v.Stamp] <- 1
    if not env.external &&
       not alreadyDone &&
       cenv.reportErrors && 
       not v.HasBeenReferenced && 
       not v.IsCompiledAsTopLevel && 
       not (v.DisplayName.StartsWith("_", System.StringComparison.Ordinal)) && 
       not v.IsCompilerGenerated then 

        match v.BaseOrThisInfo with 
        | ValBaseOrThisInfo.CtorThisVal ->
            warning (Error(FSComp.SR.chkUnusedThisVariable v.DisplayName, v.Range))
        | _ -> 
            warning (Error(FSComp.SR.chkUnusedValue v.DisplayName, v.Range))

let BindVals cenv env vs = List.iter (BindVal cenv env) vs

//--------------------------------------------------------------------------
// approx walk of type
//--------------------------------------------------------------------------

let rec CheckTypeDeep ((visitTyp,visitTyconRefOpt,visitAppTyOpt,visitTraitSolutionOpt, visitTyparOpt) as f) g env typ =
    // We iterate the _solved_ constraints as well, to pick up any record of trait constraint solutions
    // This means we walk _all_ the constraints _everywhere_ in a type, including
    // those attached to _solved_ type variables. This is used by PostTypeCheckSemanticChecks to detect uses of
    // values as solutions to trait constraints and determine if inference has caused the value to escape its scope.
    // The only record of these solutions is in the _solved_ constraints of types.
    // In an ideal world we would, instead, record the solutions to these constraints as "witness variables" in expressions,
    // rather than solely in types. 
    match typ with 
    | TType_var tp  when tp.Solution.IsSome  -> 
        tp.Constraints |> List.iter (fun cx -> 
            match cx with 
            | TyparConstraint.MayResolveMember((TTrait(_,_,_,_,_,soln)),_) -> 
                 match visitTraitSolutionOpt, !soln with 
                 | Some visitTraitSolution, Some sln -> visitTraitSolution sln
                 | _ -> ()
            | _ -> ())
    | _ -> ()
    
    let typ = stripTyparEqns typ 
    visitTyp typ

    match typ with
    | TType_forall (tps,body) -> 
        let env = BindTypars g env tps
        CheckTypeDeep f g env body           
        tps |> List.iter (fun tp -> tp.Constraints |> List.iter (CheckTypeConstraintDeep f g env))

    | TType_measure _          -> ()
    | TType_app (tcref,tinst) -> 
        match visitTyconRefOpt with 
        | Some visitTyconRef -> visitTyconRef tcref 
        | None -> ()
        CheckTypesDeep f g env tinst
        match visitAppTyOpt with 
        | Some visitAppTy -> visitAppTy (tcref, tinst)
        | None -> ()

    | TType_ucase (_,tinst) -> CheckTypesDeep f g env tinst
    | TType_tuple (_,typs) -> CheckTypesDeep f g env typs
    | TType_fun (s,t) -> CheckTypeDeep f g env s; CheckTypeDeep f g env t
    | TType_var tp -> 
          if not tp.IsSolved then 
              match visitTyparOpt with 
              | None -> ()
              | Some visitTypar -> 
                    visitTypar (env,tp)

and CheckTypesDeep f g env tys = List.iter (CheckTypeDeep f g env) tys

and CheckTypeConstraintDeep f g env x =
     match x with 
     | TyparConstraint.CoercesTo(ty,_) -> CheckTypeDeep f g env ty
     | TyparConstraint.MayResolveMember(traitInfo,_) -> CheckTraitInfoDeep f g env traitInfo
     | TyparConstraint.DefaultsTo(_,ty,_) -> CheckTypeDeep f g env ty
     | TyparConstraint.SimpleChoice(tys,_) -> CheckTypesDeep f g env tys
     | TyparConstraint.IsEnum(uty,_) -> CheckTypeDeep f g env uty
     | TyparConstraint.IsDelegate(aty,bty,_) -> CheckTypeDeep f g env aty; CheckTypeDeep f g env bty
     | TyparConstraint.SupportsComparison _ 
     | TyparConstraint.SupportsEquality _ 
     | TyparConstraint.SupportsNull _ 
     | TyparConstraint.IsNonNullableStruct _ 
     | TyparConstraint.IsUnmanaged _
     | TyparConstraint.IsReferenceType _ 
     | TyparConstraint.RequiresDefaultConstructor _ -> ()
and CheckTraitInfoDeep ((_,_,_,visitTraitSolutionOpt,_) as f) g env (TTrait(typs,_,_,argtys,rty,soln))  = 
    CheckTypesDeep f g env typs 
    CheckTypesDeep f g env argtys 
    Option.iter (CheckTypeDeep f g env) rty
    match visitTraitSolutionOpt, !soln with 
    | Some visitTraitSolution, Some sln -> visitTraitSolution sln
    | _ -> ()

//--------------------------------------------------------------------------
// check for byref types
//--------------------------------------------------------------------------

let CheckForByrefLikeType cenv env typ check = 
    CheckTypeDeep (ignore, Some (fun tcref -> if isByrefLikeTyconRef cenv.g tcref then check()),  None, None, None) cenv.g env typ


//--------------------------------------------------------------------------
// check captures under lambdas
//--------------------------------------------------------------------------
  
/// This is the definition of what can/can't be free in a lambda expression. This is checked at lambdas OR TBind(v,e) nodes OR TObjExprMethod nodes. 
/// For TBind(v,e) nodes we may know an 'arity' which gives as a larger set of legitimate syntactic arguments for a lambda. 
/// For TObjExprMethod(v,e) nodes we always know the legitimate syntactic arguments. 
let CheckEscapes cenv allowProtected m syntacticArgs body = (* m is a range suited to error reporting *)
    if cenv.reportErrors then 
        let cantBeFree v = 
           // First, if v is a syntactic argument, then it can be free since it was passed in. 
           // The following can not be free: 
           //   a) BaseVal can never escape. 
           //   b) Byref typed values can never escape. 
           // Note that: Local mutables can be free, as they will be boxed later.

           // These checks must correspond to the tests governing the error messages below. 
           let passedIn = ListSet.contains valEq v syntacticArgs 
           if passedIn then
               false
           else
               (v.BaseOrThisInfo = BaseVal) ||
               (isByrefLikeTy cenv.g v.Type)

        let frees = freeInExpr CollectLocals body
        let fvs   = frees.FreeLocals 
        if not allowProtected && frees.UsesMethodLocalConstructs  then
            errorR(Error(FSComp.SR.chkProtectedOrBaseCalled(), m))
        elif Zset.exists cantBeFree fvs then 
            let v =  List.find cantBeFree (Zset.elements fvs) 
            (* byref error before mutable error (byrefs are mutable...). *)
            if (isByrefLikeTy cenv.g v.Type) then
                // Inner functions are not guaranteed to compile to method with a predictable arity (number of arguments). 
                // As such, partial applications involving byref arguments could lead to closures containing byrefs. 
                // For safety, such functions are assumed to have no known arity, and so can not accept byrefs. 
                errorR(Error(FSComp.SR.chkByrefUsedInInvalidWay(v.DisplayName), m))
            elif v.BaseOrThisInfo = BaseVal then
                errorR(Error(FSComp.SR.chkBaseUsedInInvalidWay(), m))
            else
                (* Should be dead code, unless governing tests change *)
                errorR(InternalError(FSComp.SR.chkVariableUsedInInvalidWay(v.DisplayName), m))
        Some frees
    else
        None


//--------------------------------------------------------------------------
// check type access
//--------------------------------------------------------------------------

let AccessInternalsVisibleToAsInternal thisCompPath internalsVisibleToPaths access =
  // Each internalsVisibleToPath is a compPath for the internals of some assembly.
  // Replace those by the compPath for the internals of this assembly.
  // This makes those internals visible here, but still internal. Bug://3737
  (access,internalsVisibleToPaths) ||> List.fold (fun access internalsVisibleToPath -> 
      accessSubstPaths (thisCompPath,internalsVisibleToPath) access)
    

let CheckTypeForAccess (cenv:cenv) env objName valAcc m ty =
    if cenv.reportErrors then 

        let visitType ty =         
            // We deliberately only check the fully stripped type for accessibility, 
            // because references to private type abbreviations are permitted
            match tryDestAppTy cenv.g ty with 
            | None -> ()
            | Some tcref ->
                let thisCompPath = compPathOfCcu cenv.viewCcu
                let tyconAcc = tcref.Accessibility |> AccessInternalsVisibleToAsInternal thisCompPath cenv.internalsVisibleToPaths
                if isLessAccessible tyconAcc valAcc then
                    errorR(Error(FSComp.SR.chkTypeLessAccessibleThanType(tcref.DisplayName, (objName())), m))

        CheckTypeDeep (visitType, None, None, None, None) cenv.g env ty

let WarnOnWrongTypeForAccess (cenv:cenv) env objName valAcc m ty =
    if cenv.reportErrors then 

        let visitType ty =         
            // We deliberately only check the fully stripped type for accessibility, 
            // because references to private type abbreviations are permitted
            match tryDestAppTy cenv.g ty with 
            | None -> ()
            | Some tcref ->
                let thisCompPath = compPathOfCcu cenv.viewCcu
                let tyconAcc = tcref.Accessibility |> AccessInternalsVisibleToAsInternal thisCompPath cenv.internalsVisibleToPaths
                if isLessAccessible tyconAcc valAcc then
                    let errorText = FSComp.SR.chkTypeLessAccessibleThanType(tcref.DisplayName, (objName())) |> snd
                    let warningText = errorText + System.Environment.NewLine + FSComp.SR.tcTypeAbbreviationsCheckedAtCompileTime()
                    warning(AttributeChecking.ObsoleteWarning(warningText, m))

        CheckTypeDeep (visitType, None, None, None, None) cenv.g env ty 

//--------------------------------------------------------------------------
// check type instantiations
//--------------------------------------------------------------------------

/// Check types occurring in the TAST.
let CheckType permitByrefs (cenv:cenv) env m ty =
    if cenv.reportErrors then 
        let visitTypar (env,tp) = 
          if not (env.boundTypars.ContainsKey tp) then 
             if tp.IsCompilerGenerated then 
               errorR (Error(FSComp.SR.checkNotSufficientlyGenericBecauseOfScopeAnon(),m))
             else
               errorR (Error(FSComp.SR.checkNotSufficientlyGenericBecauseOfScope(tp.DisplayName),m))

        let visitTyconRef tcref = 
            if not permitByrefs && isByrefLikeTyconRef cenv.g tcref then 
                errorR(Error(FSComp.SR.chkErrorUseOfByref(), m))
            if tyconRefEq cenv.g cenv.g.system_Void_tcref tcref then 
                errorR(Error(FSComp.SR.chkSystemVoidOnlyInTypeof(), m))

        // check if T contains byref types in case of byref<T>
        let visitAppTy (tcref,tinst) = 
            if isByrefLikeTyconRef cenv.g tcref then
                let visitType ty0 =
                    match tryDestAppTy cenv.g ty0 with
                    | None -> ()
                    | Some tcref ->  
                        if isByrefLikeTyconRef cenv.g tcref then 
                            errorR(Error(FSComp.SR.chkNoByrefsOfByrefs(NicePrint.minimalStringOfType cenv.denv ty), m)) 
                CheckTypesDeep (visitType, None, None, None, None) cenv.g env tinst

        let visitTraitSolution info = 
            match info with 
            | FSMethSln(_,vref,_) -> 
               //printfn "considering %s..." vref.DisplayName
               if valRefInThisAssembly cenv.g.compilingFslib vref && not (cenv.boundVals.ContainsKey(vref.Stamp)) then 
                   //printfn "recording %s..." vref.DisplayName
                   cenv.potentialUnboundUsesOfVals <- cenv.potentialUnboundUsesOfVals.Add(vref.Stamp,m)
            | _ -> ()

        CheckTypeDeep (ignore, Some visitTyconRef, Some visitAppTy, Some visitTraitSolution, Some visitTypar) cenv.g env ty


/// Check types occurring in TAST (like CheckType) and additionally reject any byrefs.
/// The additional byref checks are to catch "byref instantiations" - one place were byref are not permitted.  
let CheckTypeNoByrefs (cenv:cenv) env m ty = CheckType false cenv env m ty
let CheckTypePermitByrefs (cenv:cenv) env m ty = CheckType true cenv env m ty

let CheckTypeInstNoByrefs cenv env m tyargs =
    tyargs |> List.iter (CheckTypeNoByrefs cenv env m)

let CheckTypeInstPermitByrefs cenv env m tyargs =
    tyargs |> List.iter (CheckType true cenv env m)

    
//--------------------------------------------------------------------------
// check exprs etc
//--------------------------------------------------------------------------
  
type ByrefContext = 
    /// Tuple of arguments where elements can be byrefs
    | TupleOfArgsPermitByrefs of int 
    /// Context allows for byref typed expr. Flag indicates if this is a byref-return position
    | PermitByref of isReturn: bool                 
    /// General (byref type expr not allowed) 
    | NoByrefs            

let noByrefs context = match context with PermitByref _ -> false | _ -> true

let mkArgsPermitByrefs isByrefReturnCall n = 
    if n=1 then PermitByref isByrefReturnCall 
    else TupleOfArgsPermitByrefs n

/// Work out what byref-values are allowed at input positions to named F# functions or members
let mkArgsForAppliedVal isByrefReturnCall (vref:ValRef) = 
    match vref.ValReprInfo with
    | Some topValInfo -> List.map (mkArgsPermitByrefs isByrefReturnCall) topValInfo.AritiesOfArgs
    | None -> []  

/// Work out what byref-values are allowed at input positions to functions
let rec mkArgsForAppliedExpr isByrefReturnCall x =
    match x with 
    // recognise val 
    | Expr.Val (vref,_,_)         -> mkArgsForAppliedVal isByrefReturnCall vref      
    // step through reclink  
    | Expr.Link eref              -> mkArgsForAppliedExpr isByrefReturnCall !eref     
    // step through instantiations 
    | Expr.App(f,_fty,_tyargs,[],_) -> mkArgsForAppliedExpr isByrefReturnCall f         
    // step through subsumption coercions 
    | Expr.Op(TOp.Coerce,_,[f],_) -> mkArgsForAppliedExpr isByrefReturnCall f         
    | _  -> []
    
/// Applied functions get wrapped in coerce nodes for subsumption coercions
let (|OptionalCoerce|) = function 
    | Expr.Op(TOp.Coerce _, _, [Expr.App(f, _, _, [], _)], _) -> f 
    | x -> x

/// Check an expression doesn't contain a 'reraise'
let CheckNoReraise cenv freesOpt (body:Expr) = 
    if cenv.reportErrors then
        // Avoid recomputing the free variables 
        let fvs = match freesOpt with None -> freeInExpr CollectLocals body | Some fvs -> fvs
        if fvs.UsesUnboundRethrow then
            errorR(Error(FSComp.SR.chkErrorContainsCallToRethrow(), body.Range))

/// Check if a function is a quotation splice operator
let isSpliceOperator g v = valRefEq g v g.splice_expr_vref || valRefEq g v g.splice_raw_expr_vref 

/// Check conditions associated with implementing multiple instantiations of a generic interface
let CheckMultipleInterfaceInstantiations cenv interfaces m = 
    let keyf ty = assert isAppTy cenv.g ty; (tcrefOfAppTy cenv.g ty).Stamp
    let table = interfaces |> MultiMap.initBy keyf
    let firstInterfaceWithMultipleGenericInstantiations = 
        interfaces |> List.tryPick (fun typ1 -> 
            table |> MultiMap.find (keyf typ1) |> List.tryPick (fun typ2 -> 
                   if // same nominal type
                       tyconRefEq cenv.g (tcrefOfAppTy cenv.g typ1) (tcrefOfAppTy cenv.g typ2) &&
                       // different instantiations
                       not (typeEquivAux EraseNone cenv.g typ1 typ2) 
                    then Some (typ1,typ2)
                    else None))
    match firstInterfaceWithMultipleGenericInstantiations with 
    | None -> ()
    | Some (typ1,typ2) -> 
         errorR(Error(FSComp.SR.chkMultipleGenericInterfaceInstantiations((NicePrint.minimalStringOfType cenv.denv typ1), (NicePrint.minimalStringOfType cenv.denv typ2)),m))

/// Check an expression, where the expression is in a position where byrefs can be generated
let rec CheckExprNoByrefs (cenv:cenv) (env:env) expr =
    CheckExpr cenv env expr NoByrefs

/// Check a value
and CheckVal (cenv:cenv) (env:env) v m context = 
    if cenv.reportErrors then 
        if isSpliceOperator cenv.g v && not env.quote then errorR(Error(FSComp.SR.chkSplicingOnlyInQuotations(), m))
        if isSpliceOperator cenv.g v then errorR(Error(FSComp.SR.chkNoFirstClassSplicing(), m))
        if valRefEq cenv.g v cenv.g.addrof_vref  then errorR(Error(FSComp.SR.chkNoFirstClassAddressOf(), m))
        if valRefEq cenv.g v cenv.g.reraise_vref then errorR(Error(FSComp.SR.chkNoFirstClassRethrow(), m))
        if noByrefs context && isByrefLikeTy cenv.g v.Type then 
            // byref typed val can only occur in permitting contexts 
            errorR(Error(FSComp.SR.chkNoByrefAtThisPoint(v.DisplayName), m))
    CheckTypePermitByrefs cenv env m v.Type
    
/// Check an expression, given information about the position of the expression
and CheckExpr (cenv:cenv) (env:env) expr (context:ByrefContext) =    
    let expr = stripExpr expr

    match expr with
    | Expr.Sequential (e1,e2,dir,_,_) -> 
        CheckExprNoByrefs cenv env e1
        match dir with
        | NormalSeq -> CheckExpr cenv env e2 context       // carry context into _;RHS (normal sequencing only)      
        | ThenDoSeq -> CheckExprNoByrefs cenv {env with limited=false} e2

    | Expr.Let (bind,body,_,_) ->  
        CheckBinding cenv env false bind  
        BindVal cenv env bind.Var
        CheckExpr cenv env body context

    | Expr.Const (_,m,ty) -> 
        CheckTypePermitByrefs cenv env m ty 
            
    | Expr.Val (v,vFlags,m) -> 
          if cenv.reportErrors then 
              if v.BaseOrThisInfo = BaseVal then 
                errorR(Error(FSComp.SR.chkLimitationsOfBaseKeyword(), m))
              if (match vFlags with NormalValUse -> true | _ -> false) && 
                 v.IsConstructor && 
                 (match v.ActualParent with Parent tcref -> isAbstractTycon tcref.Deref | _ -> false) then 
                errorR(Error(FSComp.SR.tcAbstractTypeCannotBeInstantiated(),m))

              if isByrefTy cenv.g v.Type &&
                 // A byref return location...
                 (match context with PermitByref isReturn -> isReturn | _ -> false) && 
                 // The value is a local....
                 v.ValReprInfo.IsNone && 
                 // The value is not an argument...
                 not (env.argVals.ContainsVal(v.Deref)) then 
                errorR(Error(FSComp.SR.chkNoByrefReturnOfLocal(v.DisplayName), m))
          
          CheckVal cenv env v m context
          
    | Expr.Quote(ast,savedConv,_isFromQueryExpression,m,ty) -> 
          CheckExprNoByrefs cenv {env with quote=true} ast
          if cenv.reportErrors then 
              cenv.usesQuotations <- true
              try 
                  let qscope = QuotationTranslator.QuotationGenerationScope.Create (cenv.g,cenv.amap,cenv.viewCcu, QuotationTranslator.IsReflectedDefinition.No) 
                  let qdata = QuotationTranslator.ConvExprPublic qscope QuotationTranslator.QuotationTranslationEnv.Empty ast  
                  let typeDefs,spliceTypes,spliceExprs = qscope.Close()
                  match savedConv.Value with 
                  | None -> savedConv:= Some (typeDefs, List.map fst spliceTypes, List.map fst spliceExprs, qdata)
                  | Some _ -> ()
              with QuotationTranslator.InvalidQuotedTerm e -> 
                  errorRecovery e m
                
          CheckTypeNoByrefs cenv env m ty

    | Expr.Obj (_,typ,basev,superInitCall,overrides,iimpls,m) -> 
          CheckExprNoByrefs cenv env superInitCall
          CheckMethods cenv env basev overrides 
          CheckInterfaceImpls cenv env basev iimpls
          CheckTypePermitByrefs cenv env m typ
          let interfaces = 
              [ if isInterfaceTy cenv.g typ then 
                    yield! AllSuperTypesOfType cenv.g cenv.amap m AllowMultiIntfInstantiations.Yes typ
                for (ty,_) in iimpls do
                    yield! AllSuperTypesOfType cenv.g cenv.amap m AllowMultiIntfInstantiations.Yes ty  ]
              |> List.filter (isInterfaceTy cenv.g)
          CheckMultipleInterfaceInstantiations cenv interfaces m

    // Allow base calls to F# methods
    | Expr.App((InnerExprPat(ExprValWithPossibleTypeInst(v,vFlags,_,_)  as f)),fty,tyargs,(Expr.Val(baseVal,_,_) :: rest),m) 
          when ((match vFlags with VSlotDirectCall -> true | _ -> false) && 
                baseVal.BaseOrThisInfo = BaseVal) ->
        // dprintfn "GOT BASE VAL USE"
        let memberInfo = Option.get v.MemberInfo
        if memberInfo.MemberFlags.IsDispatchSlot then
            errorR(Error(FSComp.SR.tcCannotCallAbstractBaseMember(v.DisplayName),m))
        else         
            CheckVal cenv env v m NoByrefs
            CheckVal cenv env baseVal m NoByrefs
            CheckTypePermitByrefs cenv env m fty
            CheckTypeInstPermitByrefs cenv env m tyargs
            CheckExprs cenv env rest (List.tail (mkArgsForAppliedExpr false f))

    // Allow base calls to IL methods
    | Expr.Op (TOp.ILCall (virt,_,_,_,_,_,_,mref,enclTypeArgs,methTypeArgs,tys),tyargs,(Expr.Val(baseVal,_,_)::rest),m) 
          when not virt && baseVal.BaseOrThisInfo = BaseVal ->
        
        // Disallow calls to abstract base methods on IL types. 
        match tryDestAppTy cenv.g baseVal.Type with
        | Some tcref when tcref.IsILTycon ->
            try
                // This is awkward - we have to explicitly re-resolve back to the IL metadata to determine if the method is abstract.
                // We believe this may be fragile in some situations, since we are using the Abstract IL code to compare
                // type equality, and it would be much better to remove any F# dependency on that implementation of IL type
                // equality. It would be better to make this check in tc.fs when we have the Abstract IL metadata for the method to hand.
                let mdef = resolveILMethodRef tcref.ILTyconRawMetadata mref
                if mdef.IsAbstract then
                    errorR(Error(FSComp.SR.tcCannotCallAbstractBaseMember(mdef.Name),m))
            with _ -> () // defensive coding
        | _ -> ()
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckTypeInstNoByrefs cenv env m enclTypeArgs
        CheckTypeInstNoByrefs cenv env m methTypeArgs
        CheckTypeInstNoByrefs cenv env m tys
        CheckVal cenv env baseVal m NoByrefs
        CheckExprsPermitByrefs cenv env rest

    | Expr.Op (c,tyargs,args,m) ->
          CheckExprOp cenv env (c,tyargs,args,m) context

    // Allow 'typeof<System.Void>' calls as a special case, the only accepted use of System.Void! 
    | TypeOfExpr cenv.g ty when isVoidTy cenv.g ty ->
          () // typeof<System.Void> allowed. Special case. No further checks.

    | TypeDefOfExpr cenv.g ty when isVoidTy cenv.g ty ->
          () // typedefof<System.Void> allowed. Special case. No further checks.

    // Allow '%expr' in quotations
    | Expr.App(Expr.Val(vref,_,_),_,tinst,[arg],m) when isSpliceOperator cenv.g vref && env.quote ->
          CheckTypeInstPermitByrefs cenv env m tinst
          CheckExprNoByrefs cenv env arg

    | Expr.App(f,fty,tyargs,argsl,m) ->
        if cenv.reportErrors then
            let g = cenv.g
            match f with
            | OptionalCoerce(Expr.Val(v, _, funcRange)) 
                when (valRefEq g v g.raise_vref || valRefEq g v g.failwith_vref || valRefEq g v g.null_arg_vref || valRefEq g v g.invalid_op_vref) ->
                match argsl with
                | [] | [_] -> ()
                | _ :: _ :: _ ->
                    warning(Error(FSComp.SR.checkRaiseFamilyFunctionArgumentCount(v.DisplayName, 1, List.length argsl), funcRange)) 
            | OptionalCoerce(Expr.Val(v, _, funcRange)) when valRefEq g v g.invalid_arg_vref ->
                match argsl with
                | [] | [_] | [_; _] -> ()
                | _ :: _ :: _ :: _ ->
                    warning(Error(FSComp.SR.checkRaiseFamilyFunctionArgumentCount(v.DisplayName, 2, List.length argsl), funcRange))
            | OptionalCoerce(Expr.Val(failwithfFunc, _, funcRange)) when valRefEq g failwithfFunc g.failwithf_vref  ->
                match argsl with
                | Expr.App (Expr.Val(newFormat, _, _), _, [_; typB; typC; _; _], [Expr.Const(Const.String formatString, formatRange, _)], _) :: xs when valRefEq g newFormat g.new_format_vref ->
                    match CheckFormatStrings.TryCountFormatStringArguments formatRange g formatString typB typC with
                    | Some n ->
                        let expected = n + 1
                        let actual = List.length xs + 1
                        if expected < actual then
                            warning(Error(FSComp.SR.checkRaiseFamilyFunctionArgumentCount(failwithfFunc.DisplayName, expected, actual), funcRange))
                    | None -> ()
                | _ ->
                    ()
            | _ ->
                ()

        CheckTypeInstNoByrefs cenv env m tyargs
        CheckTypePermitByrefs cenv env m fty
        CheckTypeInstPermitByrefs cenv env m tyargs
        CheckExprNoByrefs cenv env f
        let isByrefReturnCall = 
            // if return is a byref, and being used as a return, then all arguments must be usable as byref returns
            match context with 
            | PermitByref true when isByrefTy cenv.g (tyOfExpr cenv.g expr) -> true
            | _ -> false
        CheckExprs cenv env argsl (mkArgsForAppliedExpr isByrefReturnCall f)

    // REVIEW: fold the next two cases together 
    | Expr.Lambda(_,_ctorThisValOpt,_baseValOpt,argvs,_,m,rty) -> 
        let topValInfo = ValReprInfo ([],[argvs |> List.map (fun _ -> ValReprInfo.unnamedTopArg1)],ValReprInfo.unnamedRetVal) 
        let ty = mkMultiLambdaTy m argvs rty in 
        CheckLambdas false None cenv env false topValInfo false expr m ty

    | Expr.TyLambda(_,tps,_,m,rty)  -> 
        let topValInfo = ValReprInfo (ValReprInfo.InferTyparInfo tps,[],ValReprInfo.unnamedRetVal) 
        let ty = tryMkForallTy tps rty in 
        CheckLambdas false None cenv env false topValInfo false expr m ty

    | Expr.TyChoose(tps,e1,_)  -> 
        let env = BindTypars cenv.g env tps 
        CheckExprNoByrefs cenv env e1 

    | Expr.Match(_,_,dtree,targets,m,ty) -> 
        CheckTypePermitByrefs cenv env m ty // computed byrefs allowed at each branch
        CheckDecisionTree cenv env dtree
        CheckDecisionTreeTargets cenv env targets context

    | Expr.LetRec (binds,e,_,_) ->  
        BindVals cenv env (valsOfBinds binds)
        CheckBindings cenv env binds
        CheckExprNoByrefs cenv env e

    | Expr.StaticOptimization (constraints,e2,e3,m) -> 
        CheckExprNoByrefs cenv env e2
        CheckExprNoByrefs cenv env e3
        constraints |> List.iter (function
            | TTyconEqualsTycon(ty1,ty2) -> 
                CheckTypeNoByrefs cenv env m ty1
                CheckTypeNoByrefs cenv env m ty2
            | TTyconIsStruct(ty1) -> 
                CheckTypeNoByrefs cenv env m ty1)

    | Expr.Link _ -> 
        failwith "Unexpected reclink"

and CheckMethods cenv env baseValOpt l = 
    l |> List.iter (CheckMethod cenv env baseValOpt) 

and CheckMethod cenv env baseValOpt (TObjExprMethod(_,attribs,tps,vs,body,m)) = 
    let env = BindTypars cenv.g env tps 
    let vs = List.concat vs
    CheckAttribs cenv env attribs
    CheckNoReraise cenv None body
    CheckEscapes cenv true m (match baseValOpt with Some x -> x:: vs | None -> vs) body |> ignore
    CheckExprPermitByref cenv env body

and CheckInterfaceImpls cenv env baseValOpt l = 
    l |> List.iter (CheckInterfaceImpl cenv env baseValOpt)
    
and CheckInterfaceImpl cenv env baseValOpt (_ty,overrides) = 
    CheckMethods cenv env baseValOpt overrides 

and CheckExprOp cenv env (op,tyargs,args,m) context =
    let limitedCheck() = 
        if env.limited then errorR(Error(FSComp.SR.chkObjCtorsCantUseExceptionHandling(), m))
    List.iter (CheckTypePermitByrefs cenv env m) tyargs
    (* Special cases *)
    match op,tyargs,args with 
    // Handle these as special cases since mutables are allowed inside their bodies 
    | TOp.While _,_,[Expr.Lambda(_,_,_,[_],e1,_,_);Expr.Lambda(_,_,_,[_],e2,_,_)]  ->
        CheckTypeInstNoByrefs cenv env m tyargs 
        CheckExprsNoByrefs cenv env [e1;e2]

    | TOp.TryFinally _,[_],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],e2,_,_)] ->
        CheckTypeInstPermitByrefs cenv env m tyargs  // result of a try/finally can be a byref 
        limitedCheck()
        CheckExpr cenv env e1 context   // result of a try/finally can be a byref if in a position where the overall expression is can be a byref
        CheckExprNoByrefs cenv env e2

    | TOp.For(_),_,[Expr.Lambda(_,_,_,[_],e1,_,_);Expr.Lambda(_,_,_,[_],e2,_,_);Expr.Lambda(_,_,_,[_],e3,_,_)]  ->
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckExprsNoByrefs cenv env [e1;e2;e3]

    | TOp.TryCatch _,[_],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],_e2,_,_); Expr.Lambda(_,_,_,[_],e3,_,_)] ->
        CheckTypeInstPermitByrefs cenv env m tyargs  // result of a try/catch can be a byref 
        limitedCheck()
        CheckExpr cenv env e1 context // result of a try/catch can be a byref if in a position where the overall expression is can be a byref
        // [(* e2; -- don't check filter body - duplicates logic in 'catch' body *) e3]
        CheckExpr cenv env e3 context // result of a try/catch can be a byref if in a position where the overall expression is can be a byref
        
    | TOp.ILCall (_,_,_,_,_,_,_,_,enclTypeArgs,methTypeArgs,tys),_,_ ->
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckTypeInstNoByrefs cenv env m enclTypeArgs
        CheckTypeInstNoByrefs cenv env m methTypeArgs
        CheckTypeInstPermitByrefs cenv env m tys // permit byref returns

        // if return is a byref, and being used as a return, then all arguments must be usable as byref returns
        match context,tys with 
        | PermitByref true, [ty] when isByrefTy cenv.g ty -> CheckExprsPermitByrefReturns cenv env args  
        | _ -> CheckExprsPermitByrefs cenv env args  


    | TOp.Tuple tupInfo,_,_ when not (evalTupInfoIsStruct tupInfo) ->           
        match context with 
        | TupleOfArgsPermitByrefs nArity -> 
            if cenv.reportErrors then 
                if args.Length <> nArity then 
                    errorR(InternalError("Tuple arity does not correspond to planned function argument arity",m))
            // This tuple should not be generated. The known function arity 
            // means it just bundles arguments. 
            CheckExprsPermitByrefs cenv env args  
        | _ -> 
            CheckTypeInstNoByrefs cenv env m tyargs
            CheckExprsNoByrefs cenv env args 

    | TOp.LValueOp(LGetAddr,v),_,_ -> 
        if cenv.reportErrors  then 
            if noByrefs context && cenv.reportErrors then 
                errorR(Error(FSComp.SR.chkNoAddressOfAtThisPoint(v.DisplayName), m))
            elif (// The context is a byref return....
                  match context with PermitByref isReturn -> isReturn | _ -> false) && 
                  // The value is a local....
                  v.ValReprInfo.IsNone && 
                  // The value is not an argument...
                  not (env.argVals.ContainsVal(v.Deref)) then 
                errorR(Error(FSComp.SR.chkNoByrefReturnOfLocal(v.DisplayName), m))

        // Address-of operator generates byref, and context permits this. 
        CheckExprsNoByrefs cenv env args                   

    | TOp.TupleFieldGet _,_,[arg1] -> 
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckExprsPermitByrefs cenv env [arg1]             (* Compiled pattern matches on immutable value structs come through here. *)

    | TOp.ValFieldGet _rf,_,[arg1] -> 
        CheckTypeInstNoByrefs cenv env m tyargs
        //See mkRecdFieldGetViaExprAddr -- byref arg1 when #args =1 
        // Property getters on mutable structs come through here. 
        CheckExprsPermitByrefs cenv env [arg1]          

    | TOp.ValFieldSet _rf,_,[arg1;arg2] -> 
        CheckTypeInstNoByrefs cenv env m tyargs
        // See mkRecdFieldSetViaExprAddr -- byref arg1 when #args=2 
        // Field setters on mutable structs come through here
        CheckExprsPermitByrefs cenv env [arg1]         
        CheckExprsNoByrefs            cenv env [arg2]          

    | TOp.Coerce,[_ty1;_ty2],[x] ->
        // Subsumption coercions of functions may involve byrefs in other argument positions
        CheckTypeInstPermitByrefs cenv env m tyargs
        CheckExpr cenv env x context

    | TOp.Reraise,[_ty1],[] ->
        CheckTypeInstNoByrefs cenv env m tyargs        

    | TOp.ValFieldGetAddr rfref,tyargs,[] ->
        if noByrefs context && cenv.reportErrors then
            errorR(Error(FSComp.SR.chkNoAddressStaticFieldAtThisPoint(rfref.FieldName), m)) 
        CheckTypeInstNoByrefs cenv env m tyargs
        // NOTE: there are no arg exprs to check in this case 

    | TOp.ValFieldGetAddr rfref,tyargs,[rx] ->
        if noByrefs context && cenv.reportErrors then
            errorR(Error(FSComp.SR.chkNoAddressFieldAtThisPoint(rfref.FieldName), m))
        // This construct is used for &(rx.rfield) and &(rx->rfield). Relax to permit byref types for rx. [See Bug 1263]. 
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckExprPermitByref cenv env rx 

    | TOp.UnionCaseFieldGet _,_,[arg1] -> 
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckExprPermitByref cenv env arg1

    | TOp.UnionCaseTagGet _,_,[arg1] -> 
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckExprPermitByref cenv env arg1  // allow byref - it may be address-of-struct

    | TOp.UnionCaseFieldGetAddr (uref, _idx),tyargs,[rx] ->
        if noByrefs context && cenv.reportErrors then
          errorR(Error(FSComp.SR.chkNoAddressFieldAtThisPoint(uref.CaseName), m))
        CheckTypeInstNoByrefs cenv env m tyargs
        // allow rx to be byref here, for struct unions
        CheckExprPermitByref cenv env rx

    | TOp.ILAsm (instrs,tys),_,_  ->
        CheckTypeInstPermitByrefs cenv env m tys
        CheckTypeInstNoByrefs cenv env m tyargs
        match instrs,args with
        | [ I_stfld (_alignment,_vol,_fspec) ],[lhs;rhs] ->
            // permit byref for lhs lvalue 
            CheckExprPermitByref cenv env lhs
            CheckExprNoByrefs         cenv env rhs                
        | [ I_ldfld (_alignment,_vol,_fspec) ],[lhs] ->
            // permit byref for lhs lvalue 
            CheckExprPermitByref cenv env lhs
        | [ I_ldfld (_alignment,_vol,_fspec); AI_nop ],[lhs] ->
            // permit byref for lhs lvalue of readonly value 
            CheckExprPermitByref cenv env lhs
        | [ I_ldflda (fspec) | I_ldsflda (fspec) ],[lhs] ->
            if noByrefs context && cenv.reportErrors then
                errorR(Error(FSComp.SR.chkNoAddressFieldAtThisPoint(fspec.Name), m))
            // permit byref for lhs lvalue
            CheckExprPermitByref cenv env lhs
        | [ I_ldelema (_,isNativePtr,_,_) ],lhsArray::indices ->
            if not(isNativePtr) && noByrefs context && cenv.reportErrors then
                errorR(Error(FSComp.SR.chkNoAddressOfArrayElementAtThisPoint(), m))
            // permit byref for lhs lvalue 
            CheckExprPermitByref cenv env lhsArray
            CheckExprsNoByrefs cenv env indices
        | [ AI_conv _ ],_ ->
            // permit byref for args to conv 
            CheckExprsPermitByrefs cenv env args 
        | _ ->
            CheckExprsNoByrefs cenv env args  

    | TOp.TraitCall _,_,_ ->
        CheckTypeInstNoByrefs cenv env m tyargs
        // allow args to be byref here 
        CheckExprsPermitByrefs cenv env args 

    | (   TOp.Tuple _
        | TOp.UnionCase _
        | TOp.ExnConstr _
        | TOp.Array
        | TOp.Bytes _
        | TOp.UInt16s _
        | TOp.Recd _
        | TOp.ValFieldSet _
        | TOp.UnionCaseTagGet _
        | TOp.UnionCaseProof _
        | TOp.UnionCaseFieldGet _
        | TOp.UnionCaseFieldSet _
        | TOp.ExnFieldGet _
        | TOp.ExnFieldSet _
        | TOp.TupleFieldGet _
        | TOp.RefAddrGet 
        | _ (* catch all! *)
        ),_,_ ->    
        CheckTypeInstNoByrefs cenv env m tyargs
        CheckExprsNoByrefs cenv env args 

and CheckLambdas isTop (memInfo: ValMemberInfo option) cenv env inlined topValInfo alwaysCheckNoReraise e m ety =
    // The topValInfo here says we are _guaranteeing_ to compile a function value 
    // as a .NET method with precisely the corresponding argument counts. 
    match e with
    | Expr.TyChoose(tps,e1,m)  -> 
        let env = BindTypars cenv.g env tps
        CheckLambdas isTop memInfo cenv env inlined topValInfo alwaysCheckNoReraise e1 m ety      

    | Expr.Lambda (_,_,_,_,_,m,_)  
    | Expr.TyLambda(_,_,_,m,_) ->

        let tps,ctorThisValOpt,baseValOpt,vsl,body,bodyty = destTopLambda cenv.g cenv.amap topValInfo (e, ety) in
        let env = BindTypars cenv.g env tps 
        let thisAndBase = Option.toList ctorThisValOpt @ Option.toList baseValOpt
        let restArgs = List.concat vsl
        let syntacticArgs = thisAndBase @ restArgs
        let env = SetArgVals env restArgs

        match memInfo with 
        | None -> ()
        | Some mi -> 
            // ctorThis and baseVal values are always considered used
            for v in thisAndBase do v.SetHasBeenReferenced() 
            // instance method 'this' is always considered used
            match mi.MemberFlags.IsInstance, restArgs with
            | true, firstArg::_ -> firstArg.SetHasBeenReferenced()
            | _ -> ()
            // any byRef arguments are considered used, as they may be 'out's
            restArgs |> List.iter (fun arg -> if isByrefTy cenv.g arg.Type then arg.SetHasBeenReferenced())

        syntacticArgs |> List.iter (CheckValSpec cenv env)
        syntacticArgs |> List.iter (BindVal cenv env)

        // Trigger a test hook
        match memInfo with 
        | None -> ()
        | Some membInfo -> testHookMemberBody membInfo body
        
        // Check escapes in the body.  Allow access to protected things within members.
        let freesOpt = CheckEscapes cenv (Option.isSome memInfo) m syntacticArgs body

        //  no reraise under lambda expression
        CheckNoReraise cenv freesOpt body 

        // Check the body of the lambda
        if (not (isNil tps) || not (isNil vsl)) && isTop && not cenv.g.compilingFslib && isByrefTy cenv.g bodyty then
            // allow byref to occur as return position for byref-typed top level function or method 
            CheckExprPermitByrefReturn cenv env body
        else
            CheckExprNoByrefs cenv env body

        // Check byref return types
        if cenv.reportErrors then 
            if (not inlined && (isNil tps && isNil vsl)) || not isTop then
                CheckForByrefLikeType cenv env bodyty (fun () -> 
                        errorR(Error(FSComp.SR.chkFirstClassFuncNoByref(), m)))

            elif not cenv.g.compilingFslib && isByrefTy cenv.g bodyty then 
                // check no byrefs-in-the-byref
                CheckForByrefLikeType cenv env (destByrefTy cenv.g bodyty) (fun () -> 
                    errorR(Error(FSComp.SR.chkReturnTypeNoByref(), m)))

            for tp in tps do 
                if tp.Constraints |> List.sumBy (function TyparConstraint.CoercesTo(ty,_) when isClassTy cenv.g ty -> 1 | _ -> 0) > 1 then 
                    errorR(Error(FSComp.SR.chkTyparMultipleClassConstraints(), m))
                
    // This path is for expression bindings that are not actually lambdas
    | _ -> 
        // Permit byrefs for let x = ...
        CheckTypePermitByrefs cenv env m ety
        if not inlined && isByrefLikeTy cenv.g ety then
            // allow byref to occur as RHS of byref binding. 
            CheckExprPermitByref cenv env e
        else 
            CheckExprNoByrefs cenv env e
        if alwaysCheckNoReraise then 
            CheckNoReraise cenv None e

and CheckExprs cenv env exprs contexts =
    let contexts = Array.ofList contexts 
    let argArity i = if i < contexts.Length then contexts.[i] else NoByrefs 
    exprs |> List.iteri (fun i exp -> CheckExpr cenv env exp (argArity i)) 

and CheckExprsNoByrefs cenv env exprs = 
    exprs |> List.iter (CheckExprNoByrefs cenv env) 

and CheckExprsPermitByrefs cenv env exprs = 
    exprs |> List.iter (CheckExprPermitByref cenv env)

and CheckExprsPermitByrefReturns cenv env exprs = 
    exprs |> List.iter (CheckExprPermitByrefReturn cenv env)

and CheckExprPermitByref cenv env expr = 
    CheckExpr cenv env expr (PermitByref false)

and CheckExprPermitByrefReturn cenv env expr = 
    CheckExpr cenv env expr (PermitByref true)

and CheckDecisionTreeTargets cenv env targets context = 
    targets |> Array.iter (CheckDecisionTreeTarget cenv env context) 

and CheckDecisionTreeTarget cenv env context (TTarget(vs,e,_)) = 
    BindVals cenv env vs 
    vs |> List.iter (CheckValSpec cenv env)
    CheckExpr cenv env e context 

and CheckDecisionTree cenv env x =
    match x with 
    | TDSuccess (es,_) -> CheckExprsNoByrefs cenv env es
    | TDBind(bind,rest) -> CheckBinding cenv env false bind; CheckDecisionTree cenv env rest 
    | TDSwitch (e,cases,dflt,m) -> CheckDecisionTreeSwitch cenv env (e,cases,dflt,m)

and CheckDecisionTreeSwitch cenv env (e,cases,dflt,m) =
    CheckExprPermitByref cenv env e // can be byref for struct union switch
    cases |> List.iter (fun (TCase(discrim,e)) -> CheckDecisionTreeTest cenv env m discrim; CheckDecisionTree cenv env e) 
    dflt |> Option.iter (CheckDecisionTree cenv env) 

and CheckDecisionTreeTest cenv env m discrim =
    match discrim with
    | DecisionTreeTest.UnionCase (_,tinst) -> CheckTypeInstPermitByrefs cenv env m tinst
    | DecisionTreeTest.ArrayLength (_,typ) -> CheckTypePermitByrefs cenv env m typ
    | DecisionTreeTest.Const _ -> ()
    | DecisionTreeTest.IsNull -> ()
    | DecisionTreeTest.IsInst (srcTyp,dstTyp)    -> CheckTypePermitByrefs cenv env m srcTyp; CheckTypePermitByrefs cenv env m dstTyp
    | DecisionTreeTest.ActivePatternCase (exp,_,_,_,_)     -> CheckExprNoByrefs cenv env exp

and CheckAttrib cenv env (Attrib(_,_,args,props,_,_,_)) = 
    props |> List.iter (fun (AttribNamedArg(_,_,_,expr)) -> CheckAttribExpr cenv env expr)
    args |> List.iter (CheckAttribExpr cenv env)

and CheckAttribExpr cenv env (AttribExpr(expr,vexpr)) = 
    CheckExprNoByrefs cenv env expr
    CheckExprNoByrefs cenv env vexpr
    CheckNoReraise cenv None expr 
    CheckAttribArgExpr cenv env vexpr

and CheckAttribArgExpr cenv env expr = 
    match expr with 

    // Detect standard constants 
    | Expr.Const(c,m,_) -> 
        match c with 
        | Const.Bool _ 
        | Const.Int32 _ 
        | Const.SByte  _
        | Const.Int16  _
        | Const.Int32 _
        | Const.Int64 _  
        | Const.Byte  _
        | Const.UInt16  _
        | Const.UInt32  _
        | Const.UInt64  _
        | Const.Double _
        | Const.Single _
        | Const.Char _
        | Const.Zero _
        | Const.String _  -> ()
        | _ -> 
            if cenv.reportErrors then 
                errorR (Error (FSComp.SR.tastNotAConstantExpression(), m))
                
    | Expr.Op(TOp.Array,[_elemTy],args,_m) -> 
        List.iter (CheckAttribArgExpr cenv env) args
    | TypeOfExpr cenv.g _ -> 
        ()
    | TypeDefOfExpr cenv.g _ -> 
        ()
    | Expr.Op(TOp.Coerce,_,[arg],_) -> 
        CheckAttribArgExpr cenv env arg
    | EnumExpr cenv.g arg1 -> 
        CheckAttribArgExpr cenv env arg1
    | AttribBitwiseOrExpr cenv.g (arg1,arg2) ->
        CheckAttribArgExpr cenv env arg1
        CheckAttribArgExpr cenv env arg2
    | _ -> 
        if cenv.reportErrors then 
           errorR (Error (FSComp.SR.chkInvalidCustAttrVal(), expr.Range))
  
and CheckAttribs cenv env (attribs: Attribs) = 
    if isNil attribs then () else
    let tcrefs = [ for (Attrib(tcref,_,_,_,_,_,m)) in attribs -> (tcref,m) ]

    // Check for violations of allowMultiple = false
    let duplicates = 
        tcrefs
        |> Seq.groupBy (fun (tcref,_) -> tcref.Stamp) 
        |> Seq.map (fun (_,elems) -> List.last (List.ofSeq elems), Seq.length elems) 
        |> Seq.filter (fun (_,count) -> count > 1) 
        |> Seq.map fst 
        |> Seq.toList
        // Filter for allowMultiple = false
        |> List.filter (fun (tcref,m) -> TryFindAttributeUsageAttribute cenv.g m tcref <> Some(true))

    if cenv.reportErrors then 
       for (tcref,m) in duplicates do
          errorR(Error(FSComp.SR.chkAttrHasAllowMultiFalse(tcref.DisplayName), m))
    
    attribs |> List.iter (CheckAttrib cenv env) 

and CheckValInfo cenv env (ValReprInfo(_,args,ret)) =
    args |> List.iterSquared (CheckArgInfo cenv env)
    ret |> CheckArgInfo cenv env

and CheckArgInfo cenv env (argInfo : ArgReprInfo)  = 
    CheckAttribs cenv env argInfo.Attribs

and CheckValSpec cenv env (v:Val) =
    v.Attribs |> CheckAttribs cenv env
    v.ValReprInfo |> Option.iter (CheckValInfo cenv env)
    v.Type |> CheckTypePermitByrefs cenv env v.Range

and AdjustAccess isHidden (cpath: unit -> CompilationPath) access =
    if isHidden then 
        let (TAccess(l)) = access
        // FSharp 1.0 bug 1908: Values hidden by signatures are implicitly at least 'internal'
        let scoref = cpath().ILScopeRef
        TAccess(CompPath(scoref,[])::l)
    else 
        access

and CheckBinding cenv env alwaysCheckNoReraise (TBind(v,bindRhs,_) as bind) =
    let isTop = Option.isSome bind.Var.ValReprInfo
    //printfn "visiting %s..." v.DisplayName

    let env = { env with external = env.external || cenv.g.attrib_DllImportAttribute |> Option.exists (fun attr -> HasFSharpAttribute cenv.g attr v.Attribs) }

    // Check that active patterns don't have free type variables in their result
    match TryGetActivePatternInfo (mkLocalValRef v) with 
    | Some _apinfo when _apinfo.ActiveTags.Length > 1 -> 
        if doesActivePatternHaveFreeTypars cenv.g (mkLocalValRef v) then
           errorR(Error(FSComp.SR.activePatternChoiceHasFreeTypars(v.LogicalName),v.Range))
    | _ -> ()
    
    match cenv.potentialUnboundUsesOfVals.TryFind v.Stamp with
    | None -> () 
    | Some m ->
         let nm = v.DisplayName
         errorR(Error(FSComp.SR.chkMemberUsedInInvalidWay(nm, nm, stringOfRange m), v.Range))

    // Byrefs allowed for x in 'let x = ...'
    v.Type |> CheckTypePermitByrefs cenv env v.Range
    v.Attribs |> CheckAttribs cenv env
    v.ValReprInfo |> Option.iter (CheckValInfo cenv env)

    // Check accessibility
    if (v.IsMemberOrModuleBinding || v.IsMember) && not v.IsIncrClassGeneratedMember then 
        let access =  AdjustAccess (IsHiddenVal env.sigToImplRemapInfo v) (fun () -> v.TopValActualParent.CompilationPath) v.Accessibility
        CheckTypeForAccess cenv env (fun () -> NicePrint.stringOfQualifiedValOrMember cenv.denv v) access v.Range v.Type
    
    let env = if v.IsConstructor && not v.IsIncrClassConstructor then { env with limited=true } else env

    if cenv.reportErrors  then 

        // Check top-level let-bound values
        match bind.Var.ValReprInfo with
          | Some info when info.HasNoArgs -> 
              CheckForByrefLikeType cenv env v.Type (fun () -> errorR(Error(FSComp.SR.chkNoByrefAsTopValue(),v.Range)))
          | _ -> ()

        if Option.isSome v.PublicPath then 
            if 
              // Don't support implicit [<ReflectedDefinition>] on generated members, except the implicit members
              // for 'let' bound functions in classes.
              (not v.IsCompilerGenerated || v.IsIncrClassGeneratedMember) &&
              
              (// Check the attributes on any enclosing module
               env.reflect || 
               // Check the attributes on the value
               HasFSharpAttribute cenv.g cenv.g.attrib_ReflectedDefinitionAttribute v.Attribs ||
               // Also check the enclosing type for members - for historical reasons, in the TAST member values 
               // are stored in the entity that encloses the type, hence we will not have noticed the ReflectedDefinition
               // on the enclosing type at this point.
               HasFSharpAttribute cenv.g cenv.g.attrib_ReflectedDefinitionAttribute v.TopValActualParent.Attribs) then 

                if v.IsInstanceMember && v.MemberApparentParent.IsStructOrEnumTycon then
                    errorR(Error(FSComp.SR.chkNoReflectedDefinitionOnStructMember(),v.Range))
                cenv.usesQuotations <- true

                // If we've already recorded a definition then skip this 
                match v.ReflectedDefinition with 
                | None -> v.val_defn <- Some bindRhs
                | Some _ -> ()
                // Run the conversion process over the reflected definition to report any errors in the
                // front end rather than the back end. We currently re-run this during ilxgen.fs but there's
                // no real need for that except that it helps us to bundle all reflected definitions up into 
                // one blob for pickling to the binary format
                try
                    let ety = tyOfExpr cenv.g bindRhs
                    let tps,taue,_ = 
                      match bindRhs with 
                      | Expr.TyLambda (_,tps,b,_,_) -> tps,b,applyForallTy cenv.g ety (List.map mkTyparTy tps)
                      | _ -> [],bindRhs,ety
                    let env = QuotationTranslator.QuotationTranslationEnv.Empty.BindTypars tps
                    let qscope = QuotationTranslator.QuotationGenerationScope.Create (cenv.g,cenv.amap,cenv.viewCcu, QuotationTranslator.IsReflectedDefinition.Yes) 
                    QuotationTranslator.ConvExprPublic qscope env taue  |> ignore
                    let _,_,argExprs = qscope.Close()
                    if not (isNil argExprs) then 
                        errorR(Error(FSComp.SR.chkReflectedDefCantSplice(), v.Range))
                    QuotationTranslator.ConvMethodBase qscope env (v.CompiledName, v) |> ignore
                with 
                  | QuotationTranslator.InvalidQuotedTerm e -> 
                          errorR(e)
            
    match v.MemberInfo with 
    | Some memberInfo when not v.IsIncrClassGeneratedMember -> 
        match memberInfo.MemberFlags.MemberKind with 
        | (MemberKind.PropertySet | MemberKind.PropertyGet)  ->
            // These routines raise errors for ill-formed properties
            v |> ReturnTypeOfPropertyVal cenv.g |> ignore
            v |> ArgInfosOfPropertyVal cenv.g |> ignore

        | _ -> ()
        
    | _ -> ()
        
    let topValInfo  = match bind.Var.ValReprInfo with Some info -> info | _ -> ValReprInfo.emptyValData 

    CheckLambdas isTop v.MemberInfo cenv env v.MustInline topValInfo alwaysCheckNoReraise bindRhs v.Range v.Type

and CheckBindings cenv env xs = List.iter (CheckBinding cenv env false) xs

// Top binds introduce expression, check they are reraise free.
let CheckModuleBinding cenv env (TBind(v,e,_) as bind) =
    let isExplicitEntryPoint = HasFSharpAttribute cenv.g cenv.g.attrib_EntryPointAttribute v.Attribs
    if isExplicitEntryPoint then 
        cenv.entryPointGiven <- true
        let isLastCompiland = fst cenv.isLastCompiland
        if not isLastCompiland && cenv.reportErrors  then 
            errorR(Error(FSComp.SR.chkEntryPointUsage(), v.Range)) 

    // Analyze the r.h.s. for the "IsCompiledAsStaticPropertyWithoutField" condition
    if // Mutable values always have fields
       not v.IsMutable && 
       // Literals always have fields
       not (HasFSharpAttribute cenv.g cenv.g.attrib_LiteralAttribute v.Attribs) && 
       not (HasFSharpAttributeOpt cenv.g cenv.g.attrib_ThreadStaticAttribute v.Attribs) && 
       not (HasFSharpAttributeOpt cenv.g cenv.g.attrib_ContextStaticAttribute v.Attribs) && 
       // Having a field makes the binding a static initialization trigger
       IsSimpleSyntacticConstantExpr cenv.g e && 
       // Check the thing is actually compiled as a property
       IsCompiledAsStaticProperty cenv.g v 
     then 
        v.SetIsCompiledAsStaticPropertyWithoutField()

    // Check for value name clashes
    begin
        try 

          // Skip compiler generated values
          if v.IsCompilerGenerated then () else
          // Skip explicit implementations of interface methods
          if ValIsExplicitImpl cenv.g v then () else
          
          match v.ActualParent with 
          | ParentNone -> () // this case can happen after error recovery from earlier error
          | Parent _ -> 
            let tcref = v.TopValActualParent 
            let hasDefaultAugmentation = 
                tcref.IsUnionTycon &&
                match TryFindFSharpAttribute cenv.g cenv.g.attrib_DefaultAugmentationAttribute tcref.Attribs with
                | Some(Attrib(_,_,[ AttribBoolArg(b) ],_,_,_,_)) -> b
                | _ -> true (* not hiddenRepr *)

            let kind = (if v.IsMember then "member" else "value")
            let check skipValCheck nm = 
                if not skipValCheck && 
                   v.IsModuleBinding && 
                   tcref.ModuleOrNamespaceType.AllValsByLogicalName.ContainsKey(nm) && 
                   not (valEq tcref.ModuleOrNamespaceType.AllValsByLogicalName.[nm] v) then
                    
                    error(Duplicate(kind,v.DisplayName,v.Range))

#if CASES_IN_NESTED_CLASS
                if tcref.IsUnionTycon && nm = "Cases" then 
                    errorR(NameClash(nm,kind,v.DisplayName,v.Range, "generated type","Cases",tcref.Range))
#endif
                if tcref.IsUnionTycon then 
                    match nm with 
                    | "Tag" -> errorR(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoGeneratedProperty(),"Tag",tcref.Range))
                    | "Tags" -> errorR(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoGeneratedType(),"Tags",tcref.Range))
                    | _ ->
                        if hasDefaultAugmentation then 
                            match tcref.GetUnionCaseByName(nm) with 
                            | Some(uc) -> error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoUnionCase(),uc.DisplayName,uc.Range))
                            | None -> ()

                            let hasNoArgs = 
                                match v.ValReprInfo with 
                                | None -> false 
                                | Some arity -> List.sum arity.AritiesOfArgs - v.NumObjArgs <= 0 && arity.NumTypars = 0

                            //  In unions user cannot define properties that clash with generated ones 
                            if tcref.UnionCasesArray.Length = 1 && hasNoArgs then 
                               let ucase1 = tcref.UnionCasesArray.[0]
                               for f in ucase1.RecdFieldsArray do
                                   if f.Name = nm then error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoGeneratedProperty(),f.Name,ucase1.Range))

                // Default augmentation contains the nasty 'Case<UnionCase>' etc.
                let prefix = "New"
                if nm.StartsWith prefix then
                    match tcref.GetUnionCaseByName(nm.[prefix.Length ..]) with 
                    | Some(uc) -> error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.chkUnionCaseCompiledForm(),uc.DisplayName,uc.Range))
                    | None -> ()

                // Default augmentation contains the nasty 'Is<UnionCase>' etc.
                let prefix = "Is"
                if nm.StartsWith prefix && hasDefaultAugmentation then
                    match tcref.GetUnionCaseByName(nm.[prefix.Length ..]) with 
                    | Some(uc) -> error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.chkUnionCaseDefaultAugmentation(),uc.DisplayName,uc.Range))
                    | None -> ()

                match tcref.GetFieldByName(nm) with 
                | Some(rf) -> error(NameClash(nm,kind,v.DisplayName,v.Range,"field",rf.Name,rf.Range))
                | None -> ()

            check false v.CoreDisplayName
            check false v.DisplayName
            check false v.CompiledName

            // Check if an F# extension member clashes
            if v.IsExtensionMember then 
                tcref.ModuleOrNamespaceType.AllValsAndMembersByLogicalNameUncached.[v.LogicalName] |> List.iter (fun v2 -> 
                    if v2.IsExtensionMember && not (valEq v v2) && v.CompiledName = v2.CompiledName then
                        let minfo1 =  FSMeth(cenv.g, generalizedTyconRef tcref, mkLocalValRef v, Some 0UL)
                        let minfo2 =  FSMeth(cenv.g, generalizedTyconRef tcref, mkLocalValRef v2, Some 0UL)
                        if tyconRefEq cenv.g v.MemberApparentParent v2.MemberApparentParent && 
                           MethInfosEquivByNameAndSig EraseAll true cenv.g cenv.amap v.Range minfo1 minfo2 then 
                            errorR(Duplicate(kind,v.DisplayName,v.Range)))



            // Properties get 'get_X', only if there are no args
            // Properties get 'get_X'
            match v.ValReprInfo with 
            | Some arity when arity.NumCurriedArgs = 0 && arity.NumTypars = 0 -> check false ("get_"^v.DisplayName)
            | _ -> ()
            match v.ValReprInfo with 
            | Some arity when v.IsMutable && arity.NumCurriedArgs = 0 && arity.NumTypars = 0 -> check false ("set_"^v.DisplayName)
            | _ -> ()
            match TryChopPropertyName v.DisplayName with 
            | Some res -> check true res 
            | None -> ()
        with e -> errorRecovery e v.Range 
    end

    CheckBinding cenv env true bind

let CheckModuleBindings cenv env binds = List.iter (CheckModuleBinding cenv env) binds

//--------------------------------------------------------------------------
// check tycons
//--------------------------------------------------------------------------

let CheckRecdField isUnion cenv env (tycon:Tycon) (rfield:RecdField) = 
    let isHidden = 
        IsHiddenTycon env.sigToImplRemapInfo tycon || 
        IsHiddenTyconRepr env.sigToImplRemapInfo tycon || 
        (not isUnion && IsHiddenRecdField env.sigToImplRemapInfo ((mkLocalTyconRef tycon).MakeNestedRecdFieldRef rfield))
    let access = AdjustAccess isHidden (fun () -> tycon.CompilationPath) rfield.Accessibility
    CheckTypeForAccess cenv env (fun () -> rfield.Name) access rfield.Range rfield.FormalType
    CheckTypeNoByrefs cenv env rfield.Range rfield.FormalType
    CheckAttribs cenv env rfield.PropertyAttribs
    CheckAttribs cenv env rfield.FieldAttribs
    if cenv.reportErrors then 
        CheckForByrefLikeType cenv env rfield.FormalType (fun () -> errorR(Error(FSComp.SR.chkCantStoreByrefValue(), tycon.Range)))

let CheckEntityDefn cenv env (tycon:Entity) =
#if EXTENSIONTYPING
  if not tycon.IsProvidedGeneratedTycon then
#endif
    let env = { env with reflect = env.reflect || HasFSharpAttribute cenv.g cenv.g.attrib_ReflectedDefinitionAttribute tycon.Attribs }
    let m = tycon.Range 
    let env = BindTypars cenv.g env (tycon.Typars(m))
    CheckAttribs cenv env tycon.Attribs
    match tycon.TypeAbbrev with
    | Some abbrev -> WarnOnWrongTypeForAccess cenv env (fun () -> tycon.CompiledName) tycon.Accessibility tycon.Range abbrev
    | _ -> ()

    if cenv.reportErrors then
      if not tycon.IsTypeAbbrev then
        let typ = generalizedTyconRef (mkLocalTyconRef tycon)
        let allVirtualMethsInParent = 
            match GetSuperTypeOfType cenv.g cenv.amap m typ with 
            | Some super -> 
                GetIntrinsicMethInfosOfType cenv.infoReader (None,AccessibleFromSomewhere,AllowMultiIntfInstantiations.Yes)  IgnoreOverrides m super
                |> List.filter (fun minfo -> minfo.IsVirtual)
            | None -> []

        let namesOfMethodsThatMayDifferOnlyInReturnType = ["op_Explicit";"op_Implicit"] (* hardwired *)
        let methodUniquenessIncludesReturnType (minfo:MethInfo) = List.contains minfo.LogicalName namesOfMethodsThatMayDifferOnlyInReturnType
        let MethInfosEquivWrtUniqueness eraseFlag m minfo minfo2 =
            if methodUniquenessIncludesReturnType minfo
            then MethInfosEquivByNameAndSig        eraseFlag true cenv.g cenv.amap m minfo minfo2
            else MethInfosEquivByNameAndPartialSig eraseFlag true cenv.g cenv.amap m minfo minfo2 (* partial ignores return type *)

        let immediateMeths = 
            [ for v in tycon.AllGeneratedValues do yield FSMeth (cenv.g,typ,v,None)
              yield! GetImmediateIntrinsicMethInfosOfType (None,AccessibleFromSomewhere) cenv.g cenv.amap m typ ]

        let immediateProps = GetImmediateIntrinsicPropInfosOfType (None,AccessibleFromSomewhere) cenv.g cenv.amap m typ

        let getHash (hash:Dictionary<string,_>) nm = 
             if hash.ContainsKey(nm) then hash.[nm] else []
        
        // precompute methods grouped by MethInfo.LogicalName
        let hashOfImmediateMeths = 
                let h = new Dictionary<string, _>()
                for minfo in immediateMeths do
                    match h.TryGetValue minfo.LogicalName with
                    | true, methods -> 
                        h.[minfo.LogicalName] <- minfo::methods
                    | false, _ -> 
                        h.[minfo.LogicalName] <- [minfo]
                h
        let getOtherMethods (minfo : MethInfo) =                         
            [
                //we have added all methods to the dictionary on the previous step
                let methods = hashOfImmediateMeths.[minfo.LogicalName]
                for m in methods do
                    // use referential identity to filter out 'minfo' method
                    if not(System.Object.ReferenceEquals(m, minfo)) then 
                        yield m
            ]

        let hashOfImmediateProps = new Dictionary<string,_>()
        for minfo in immediateMeths do
            let nm = minfo.LogicalName
            let m = (match minfo.ArbitraryValRef with None -> m | Some vref -> vref.DefinitionRange)
            let others = getOtherMethods minfo
            // abstract/default pairs of duplicate methods are OK
            let IsAbstractDefaultPair (x:MethInfo) (y:MethInfo) = 
                x.IsDispatchSlot && y.IsDefiniteFSharpOverride
            let IsAbstractDefaultPair2 (minfo:MethInfo) (minfo2:MethInfo) = 
                IsAbstractDefaultPair minfo minfo2 || IsAbstractDefaultPair minfo2 minfo
            let checkForDup erasureFlag (minfo2: MethInfo) =
                not (IsAbstractDefaultPair2 minfo minfo2)
                && (minfo.IsInstance = minfo2.IsInstance)
                && MethInfosEquivWrtUniqueness erasureFlag m minfo minfo2

            if others |> List.exists (checkForDup EraseAll) then 
                if others |> List.exists (checkForDup EraseNone) then 
                    errorR(Error(FSComp.SR.chkDuplicateMethod(nm),m))
                else
                    errorR(Error(FSComp.SR.chkDuplicateMethodWithSuffix(nm),m))

            let numCurriedArgSets = minfo.NumArgs.Length

            if numCurriedArgSets > 1 && others |> List.exists (fun minfo2 -> not (IsAbstractDefaultPair2 minfo minfo2)) then 
                errorR(Error(FSComp.SR.chkDuplicateMethodCurried nm,m))

            if numCurriedArgSets > 1 && 
               (minfo.GetParamDatas(cenv.amap, m, minfo.FormalMethodInst) 
                |> List.existsSquared (fun (ParamData(isParamArrayArg, isOutArg, optArgInfo, callerInfoInfo, _, reflArgInfo, ty)) -> 
                    isParamArrayArg || isOutArg || reflArgInfo.AutoQuote || optArgInfo.IsOptional || callerInfoInfo <> NoCallerInfo || isByrefTy cenv.g ty)) then 
                errorR(Error(FSComp.SR.chkCurriedMethodsCantHaveOutParams(), m))

            if numCurriedArgSets = 1 then
                minfo.GetParamDatas(cenv.amap, m, minfo.FormalMethodInst) 
                |> List.iterSquared (fun (ParamData(_, _, optArgInfo, callerInfoInfo, _, _, ty)) ->
                    match (optArgInfo, callerInfoInfo) with
                    | _, NoCallerInfo -> ()
                    | NotOptional, _ -> errorR(Error(FSComp.SR.tcCallerInfoNotOptional(callerInfoInfo.ToString()),m))
                    | CallerSide(_), CallerLineNumber ->
                        if not (typeEquiv cenv.g cenv.g.int32_ty ty) then
                            errorR(Error(FSComp.SR.tcCallerInfoWrongType(callerInfoInfo.ToString(), "int", NicePrint.minimalStringOfType cenv.denv ty),m))
                    | CalleeSide, CallerLineNumber ->
                        if not ((isOptionTy cenv.g ty) && (typeEquiv cenv.g cenv.g.int32_ty (destOptionTy cenv.g ty))) then
                            errorR(Error(FSComp.SR.tcCallerInfoWrongType(callerInfoInfo.ToString(), "int", NicePrint.minimalStringOfType cenv.denv (destOptionTy cenv.g ty)),m))
                    | CallerSide(_), CallerFilePath ->
                        if not (typeEquiv cenv.g cenv.g.string_ty ty) then
                            errorR(Error(FSComp.SR.tcCallerInfoWrongType(callerInfoInfo.ToString(), "string", NicePrint.minimalStringOfType cenv.denv ty),m))
                    | CalleeSide, CallerFilePath ->
                        if not ((isOptionTy cenv.g ty) && (typeEquiv cenv.g cenv.g.string_ty (destOptionTy cenv.g ty))) then
                            errorR(Error(FSComp.SR.tcCallerInfoWrongType(callerInfoInfo.ToString(), "string", NicePrint.minimalStringOfType cenv.denv (destOptionTy cenv.g ty)),m))
                    | CallerSide(_), CallerMemberName ->
                        if not (typeEquiv cenv.g cenv.g.string_ty ty) then
                            errorR(Error(FSComp.SR.tcCallerInfoWrongType(callerInfoInfo.ToString(), "string", NicePrint.minimalStringOfType cenv.denv ty),m))
                    | CalleeSide, CallerMemberName ->
                        if not ((isOptionTy cenv.g ty) && (typeEquiv cenv.g cenv.g.string_ty (destOptionTy cenv.g ty))) then
                            errorR(Error(FSComp.SR.tcCallerInfoWrongType(callerInfoInfo.ToString(), "string", NicePrint.minimalStringOfType cenv.denv (destOptionTy cenv.g ty)),m)))
            
        for pinfo in immediateProps do
            let nm = pinfo.PropertyName
            let m = (match pinfo.ArbitraryValRef with None -> m | Some vref -> vref.DefinitionRange)
            if hashOfImmediateMeths.ContainsKey(nm) then 
                errorR(Error(FSComp.SR.chkPropertySameNameMethod(nm),m))
            let others = getHash hashOfImmediateProps nm

            if pinfo.HasGetter && pinfo.HasSetter then 

                if (pinfo.GetterMethod.IsVirtual <>  pinfo.SetterMethod.IsVirtual) then 
                    errorR(Error(FSComp.SR.chkGetterSetterDoNotMatchAbstract(nm),m))

            let checkForDup erasureFlag pinfo2 =                         
                  // abstract/default pairs of duplicate properties are OK
                 let IsAbstractDefaultPair (x:PropInfo) (y:PropInfo) = 
                     x.IsDispatchSlot && y.IsDefiniteFSharpOverride

                 not (IsAbstractDefaultPair pinfo pinfo2 || IsAbstractDefaultPair pinfo2 pinfo)
                 && PropInfosEquivByNameAndPartialSig erasureFlag cenv.g cenv.amap m pinfo pinfo2 (* partial ignores return type *)

            if others |> List.exists (checkForDup EraseAll) then
                if others |> List.exists (checkForDup EraseNone) then 
                    errorR(Error(FSComp.SR.chkDuplicateProperty(nm) ,m))
                else
                    errorR(Error(FSComp.SR.chkDuplicatePropertyWithSuffix(nm) ,m))
            // Check to see if one is an indexer and one is not

            if ( (pinfo.HasGetter && 
                  pinfo.HasSetter && 
                  let setterArgs = pinfo.DropGetter.GetParamTypes(cenv.amap,m)
                  let getterArgs = pinfo.DropSetter.GetParamTypes(cenv.amap,m)
                  setterArgs.Length <> getterArgs.Length)
                || 
                 (let nargs = pinfo.GetParamTypes(cenv.amap,m).Length
                  others |> List.exists (fun pinfo2 -> (isNil(pinfo2.GetParamTypes(cenv.amap,m))) <> (nargs = 0)))) then 
                  
                  errorR(Error(FSComp.SR.chkPropertySameNameIndexer(nm),m))

            // Check to see if the signatures of the both getter and the setter imply the same property type

            if pinfo.HasGetter && pinfo.HasSetter && not pinfo.IsIndexer then
                let ty1 = pinfo.DropSetter.GetPropertyType(cenv.amap,m)
                let ty2 = pinfo.DropGetter.GetPropertyType(cenv.amap,m)
                if not (typeEquivAux EraseNone cenv.amap.g ty1 ty2) then
                    errorR(Error(FSComp.SR.chkGetterAndSetterHaveSamePropertyType(pinfo.PropertyName, NicePrint.minimalStringOfType cenv.denv ty1, NicePrint.minimalStringOfType cenv.denv ty2),m))

            hashOfImmediateProps.[nm] <- pinfo::others
            
        if not (isInterfaceTy cenv.g typ) then
            let hashOfAllVirtualMethsInParent = new Dictionary<string,_>()
            for minfo in allVirtualMethsInParent do
                let nm = minfo.LogicalName
                let others = getHash hashOfAllVirtualMethsInParent nm
                hashOfAllVirtualMethsInParent.[nm] <- minfo::others
            for minfo in immediateMeths do
                if not minfo.IsDispatchSlot && not minfo.IsVirtual && minfo.IsInstance then
                    let nm = minfo.LogicalName
                    let m = (match minfo.ArbitraryValRef with None -> m | Some vref -> vref.DefinitionRange)
                    let parentMethsOfSameName = getHash hashOfAllVirtualMethsInParent nm 
                    let checkForDup erasureFlag (minfo2:MethInfo) = minfo2.IsDispatchSlot && MethInfosEquivByNameAndSig erasureFlag true cenv.g cenv.amap m minfo minfo2
                    match parentMethsOfSameName |> List.tryFind (checkForDup EraseAll) with
                    | None -> ()
                    | Some minfo ->
                        let mtext = NicePrint.stringOfMethInfo cenv.amap m cenv.denv minfo
                        if parentMethsOfSameName |> List.exists (checkForDup EraseNone) then 
                            warning(Error(FSComp.SR.tcNewMemberHidesAbstractMember(mtext),m))
                        else
                            warning(Error(FSComp.SR.tcNewMemberHidesAbstractMemberWithSuffix(mtext),m))
                        

                if minfo.IsDispatchSlot then
                    let nm = minfo.LogicalName
                    let m = (match minfo.ArbitraryValRef with None -> m | Some vref -> vref.DefinitionRange)
                    let parentMethsOfSameName = getHash hashOfAllVirtualMethsInParent nm 
                    let checkForDup erasureFlag minfo2 = MethInfosEquivByNameAndSig erasureFlag true cenv.g cenv.amap m minfo minfo2
                    //if minfo.NumArgs.Length > 1 then 
                    //    warning(Error(sprintf "Abstract methods taking curried arguments Duplicate method. The method '%s' has curried arguments but has the same name as another method in this type. Methods with curried arguments may not be overloaded" nm,(match minfo.ArbitraryValRef with None -> m | Some vref -> vref.DefinitionRange)))
                    if parentMethsOfSameName |> List.exists (checkForDup EraseAll) then
                        if parentMethsOfSameName |> List.exists (checkForDup EraseNone) then 
                            errorR(Error(FSComp.SR.chkDuplicateMethodInheritedType(nm),m))
                        else
                            errorR(Error(FSComp.SR.chkDuplicateMethodInheritedTypeWithSuffix(nm),m))
    // Considers TFSharpObjectRepr, TRecdRepr and TUnionRepr. 
    // [Review] are all cases covered: TILObjectRepr,TAsmRepr. [Yes - these are FSharp.Core.dll only]
    tycon.AllFieldsArray |> Array.iter (CheckRecdField false cenv env tycon)
    
    // Abstract slots can have byref arguments and returns
    abstractSlotValsOfTycons [tycon] |> List.iter (typeOfVal >> CheckTypePermitByrefs cenv env m) 

    // Interface slots can have byref arguments and returns
    tycon.ImmediateInterfaceTypesOfFSharpTycon |> List.iter (CheckTypePermitByrefs cenv env m)   

    superOfTycon cenv.g tycon |> CheckTypeNoByrefs cenv env m                             

    if tycon.IsUnionTycon then                             
        tycon.UnionCasesAsList |> List.iter (fun uc ->
            CheckAttribs cenv env uc.Attribs 
            uc.RecdFields |> List.iter (CheckRecdField true cenv env tycon))

    // Access checks
    let access =  AdjustAccess (IsHiddenTycon env.sigToImplRemapInfo tycon) (fun () -> tycon.CompilationPath) tycon.Accessibility
    let visitType ty = CheckTypeForAccess cenv env (fun () -> tycon.DisplayNameWithStaticParametersAndUnderscoreTypars) access tycon.Range ty    
    abstractSlotValsOfTycons [tycon] |> List.iter (typeOfVal >> visitType) 
    superOfTycon cenv.g tycon |> visitType

    // We do not have to check access of interface implementations. See FSharp 1.0 5042
    //implements_of_tycon cenv.g tycon |> List.iter visitType
    if tycon.IsFSharpDelegateTycon then 
        match tycon.TypeReprInfo with 
        | TFSharpObjectRepr r ->
            match r.fsobjmodel_kind with 
            | TTyconDelegate ss ->
                //ss.ClassTypars 
                //ss.MethodTypars 
                ss.FormalReturnType |> Option.iter visitType
                ss.FormalParams |> List.iterSquared (fun (TSlotParam(_,ty,_,_,_,_)) -> visitType ty)
            | _ -> ()
        | _ -> ()


    let interfaces = 
        AllSuperTypesOfType cenv.g cenv.amap tycon.Range AllowMultiIntfInstantiations.Yes (generalizedTyconRef (mkLocalTyconRef tycon)) 
            |> List.filter (isInterfaceTy cenv.g)
            
    if tycon.IsFSharpInterfaceTycon then 
        List.iter visitType interfaces // Check inherited interface is as accessible
 
    if cenv.reportErrors then 
        if not tycon.IsTypeAbbrev then 
            let typ = generalizedTyconRef (mkLocalTyconRef tycon)
            let immediateInterfaces = GetImmediateInterfacesOfType SkipUnrefInterfaces.Yes cenv.g cenv.amap m typ
            let interfaces = 
              [ for ty in immediateInterfaces do
                    yield! AllSuperTypesOfType cenv.g cenv.amap m AllowMultiIntfInstantiations.Yes ty  ]
            CheckMultipleInterfaceInstantiations cenv interfaces m
        
        // Check struct fields. We check these late because we have to have first checked that the structs are
        // free of cycles
        if tycon.IsStructOrEnumTycon then 
            for f in tycon.AllInstanceFieldsAsList do
                // Check if it's marked unsafe 
                let zeroInitUnsafe = TryFindFSharpBoolAttribute cenv.g cenv.g.attrib_DefaultValueAttribute f.FieldAttribs
                if zeroInitUnsafe = Some(true) then
                   let ty' = generalizedTyconRef (mkLocalTyconRef tycon)
                   if not (TypeHasDefaultValue cenv.g m ty') then 
                       errorR(Error(FSComp.SR.chkValueWithDefaultValueMustHaveDefaultValue(), m))

        // Check type abbreviations
        match tycon.TypeAbbrev with                          
         | None     -> ()
         | Some typ -> 
             CheckForByrefLikeType cenv env typ (fun () -> errorR(Error(FSComp.SR.chkNoByrefInTypeAbbrev(), tycon.Range)))

let CheckEntityDefns cenv env tycons = 
    tycons |> List.iter (CheckEntityDefn cenv env) 

//--------------------------------------------------------------------------
// check modules
//--------------------------------------------------------------------------

let rec CheckModuleExpr cenv env x = 
    match x with  
    | ModuleOrNamespaceExprWithSig(mty,def,_) -> 
       let (rpi,mhi) = ComputeRemappingFromImplementationToSignature cenv.g def mty
       let env = { env with sigToImplRemapInfo = (mkRepackageRemapping rpi,mhi) :: env.sigToImplRemapInfo }
       CheckDefnInModule cenv env def
    
and CheckDefnsInModule cenv env x = 
    x |> List.iter (CheckDefnInModule cenv env)

and CheckNothingAfterEntryPoint cenv m =
    if cenv.entryPointGiven && cenv.reportErrors then 
        errorR(Error(FSComp.SR.chkEntryPointUsage(), m)) 

and CheckDefnInModule cenv env x = 
    match x with 
    | TMDefRec(isRec,tycons,mspecs,m) -> 
        CheckNothingAfterEntryPoint cenv m
        if isRec then BindVals cenv env (allValsOfModDef x |> Seq.toList)
        CheckEntityDefns cenv env tycons
        List.iter (CheckModuleSpec cenv env) mspecs
    | TMDefLet(bind,m)  -> 
        CheckNothingAfterEntryPoint cenv m
        CheckModuleBinding cenv env bind 
        BindVal cenv env bind.Var
    | TMDefDo(e,m)  -> 
        CheckNothingAfterEntryPoint cenv m
        CheckNoReraise cenv None e
        CheckExprNoByrefs cenv env e
    | TMAbstract(def)  -> CheckModuleExpr cenv env def
    | TMDefs(defs) -> CheckDefnsInModule cenv env defs 

and CheckModuleSpec cenv env x =
    match x with 
    | ModuleOrNamespaceBinding.Binding bind ->
        BindVals cenv env (valsOfBinds [bind])
        CheckModuleBinding cenv env bind
    | ModuleOrNamespaceBinding.Module (mspec, rhs) ->
        CheckEntityDefn cenv env mspec
        let env = { env with reflect = env.reflect || HasFSharpAttribute cenv.g cenv.g.attrib_ReflectedDefinitionAttribute mspec.Attribs }
        CheckDefnInModule cenv env rhs 

let CheckTopImpl (g,amap,reportErrors,infoReader,internalsVisibleToPaths,viewCcu,denv ,mexpr,extraAttribs,(isLastCompiland:bool*bool)) =
    let cenv = 
        { g =g  
          reportErrors=reportErrors 
          boundVals= new Dictionary<_,_>(100, HashIdentity.Structural) 
          potentialUnboundUsesOfVals=Map.empty 
          usesQuotations=false 
          infoReader=infoReader 
          internalsVisibleToPaths=internalsVisibleToPaths
          amap=amap 
          denv=denv 
          viewCcu= viewCcu
          isLastCompiland=isLastCompiland 
          entryPointGiven=false}
    
    // Certain type equality checks go faster if these TyconRefs are pre-resolved.
    // This is because pre-resolving allows tycon equality to be determined by pointer equality on the entities.
    // See primEntityRefEq.
    cenv.g.system_Void_tcref.TryDeref                  |> ignore
    cenv.g.byref_tcr.TryDeref                          |> ignore

    let resolve = function Some(t : TyconRef) -> ignore(t.TryDeref) | _ -> ()
    resolve cenv.g.system_TypedReference_tcref
    resolve cenv.g.system_ArgIterator_tcref
    resolve cenv.g.system_RuntimeArgumentHandle_tcref

    let env = 
        { sigToImplRemapInfo=[]
          quote=false
          limited=false 
          boundTyparNames=[]
          argVals = ValMap.Empty
          boundTypars= TyparMap.Empty
          reflect=false
          external=false }

    CheckModuleExpr cenv env mexpr
    CheckAttribs cenv env extraAttribs
    if cenv.usesQuotations && QuotationTranslator.QuotationGenerationScope.ComputeQuotationFormat(cenv.g) = QuotationTranslator.QuotationSerializationFormat.FSharp_20_Plus then 
        viewCcu.UsesFSharp20PlusQuotations <- true
    cenv.entryPointGiven
