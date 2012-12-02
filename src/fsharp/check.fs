module internal Microsoft.FSharp.Compiler.PostTypecheckSemanticChecks

open System.Collections.Generic
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler

open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Typrelns
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.PrettyNaming



//--------------------------------------------------------------------------
// TestHooks - for dumping range to support source transforms
//--------------------------------------------------------------------------

let testFlagMemberBody = ref false
let testHookMemberBody membInfo (expr:Expr) =
    if !testFlagMemberBody then
        let m = expr.Range 
        printf "TestMemberBody,%A,%s,%d,%d,%d,%d\n"
          (membInfo.MemberFlags.MemberKind)          
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
//        C4.a) arg to functions occuring within their known arity.
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
    { boundTypars: Typar list; 
      /// "module remap info", i.e. hiding information down the signature chain, used to compute what's hidden by a signature
      sigToImplRemapInfo: (Remap * SignatureHidingInfo) list; 
      /// Constructor limited - are we in the prelude of a constructor, prior to object initialization
      limited: bool;
      /// Are we in a quotation?
      quote : bool; 
      /// Are we under [<ReflectedDefinition>]?
      reflect : bool } 

let BindTypar env tyv = { env with boundTypars= tyv::env.boundTypars } 

let BindTypars env (tps:Typar list) = 
    if isNil tps then env else
    // Here we mutate to provide better names for generalized type parameters 
    let nms = PrettyTypes.PrettyTyparNames (fun _ -> true) (env.boundTypars |> List.map (fun tp -> tp.Name) ) tps
    (tps,nms) ||> List.iter2 (fun tp nm -> 
            if PrettyTypes.NeedsPrettyTyparName tp  then 
                tp.Data.typar_id <- ident (nm,tp.Range));      
    List.fold BindTypar env tps 

type cenv = 
    { boundVals: Dictionary<Stamp,int>; // really a hash set
      mutable potentialUnboundUsesOfVals: StampMap<range>; 
      g: TcGlobals; 
      amap: Import.ImportMap; 
      /// For reading metadata
      infoReader: InfoReader;
      internalsVisibleToPaths : CompilationPath list;
      denv: DisplayEnv; 
      viewCcu : CcuThunk;
      reportErrors: bool;
      isLastCompiland : bool;
      // outputs
      mutable usesQuotations : bool
      mutable entryPointGiven:bool  }

let BindVal cenv (v:Val) = 
    //printfn "binding %s..." v.DisplayName
    cenv.boundVals.[v.Stamp] <- 1
    if cenv.reportErrors && 
       not v.HasBeenReferenced && 
       not v.IsCompiledAsTopLevel && 
       not (v.DisplayName.StartsWith("_", System.StringComparison.Ordinal)) && 
       not v.IsCompilerGenerated then 

        match v.BaseOrThisInfo with 
        | ValBaseOrThisInfo.CtorThisVal ->
            warning (Error(FSComp.SR.chkUnusedThisVariable v.DisplayName, v.Range))
        | _ -> 
            warning (Error(FSComp.SR.chkUnusedValue v.DisplayName, v.Range))

let BindVals cenv vs = List.iter (BindVal cenv) vs

//--------------------------------------------------------------------------
// approx walk of type
//--------------------------------------------------------------------------

let rec CheckTypeDeep ((visitTyp,visitTyconRef,visitByrefsOfByrefs,visitTraitSolution) as f) typ =
    // We iterate the _solved_ constraints as well, to pick up any record of trait constraint solutions
    // This means we walk _all_ the constraints _everywhere_ in a type, including
    // those attached to _solved_ type variables. This is used by PostTypecheckSemanticChecks to detect uses of
    // values as solutions to trait constraints and determine if inference has caused the value to escape its scope.
    // The only record of these solutions is in the _solved_ constraints of types.
    // In an ideal world we would, instead, record the solutions to these constraints as "witness variables" in expressions,
    // rather than solely in types. 
    match typ with 
    | TType_var tp  when tp.Solution.IsSome  -> 
        tp.Constraints |> List.iter (fun cx -> 
            match cx with 
            | TyparConstraint.MayResolveMember((TTrait(_,_,_,_,_,soln)),_) -> 
                 Option.iter visitTraitSolution !soln
            | _ -> ())
    | _ -> ()
    
    let typ = stripTyparEqns typ 
    visitTyp typ

    match typ with
    | TType_forall (tps,body) -> 
        CheckTypeDeep f body;           
        tps |> List.iter (fun tp -> tp.Constraints |> List.iter (CheckTypeConstraintDeep f))

    | TType_measure _          -> ()
    | TType_app (tcref,tinst) -> 
        visitTyconRef tcref
        CheckTypesDeep f tinst
        visitByrefsOfByrefs (tcref, tinst)

    | TType_ucase (_,tinst) -> CheckTypesDeep f tinst
    | TType_tuple typs        -> CheckTypesDeep f typs
    | TType_fun (s,t)         -> CheckTypeDeep f s; CheckTypeDeep f t
    | TType_var _tp            -> ()
and CheckTypesDeep f tys = List.iter (CheckTypeDeep f) tys
and CheckTypeConstraintDeep f x =
     match x with 
     | TyparConstraint.CoercesTo(ty,_) -> CheckTypeDeep f ty
     | TyparConstraint.MayResolveMember(traitInfo,_) -> CheckTraitInfoDeep f traitInfo
     | TyparConstraint.DefaultsTo(_,ty,_) -> CheckTypeDeep f ty
     | TyparConstraint.SimpleChoice(tys,_) -> CheckTypesDeep f tys
     | TyparConstraint.IsEnum(uty,_) -> CheckTypeDeep f uty
     | TyparConstraint.IsDelegate(aty,bty,_) -> CheckTypeDeep f aty; CheckTypeDeep f bty
     | TyparConstraint.SupportsComparison _ 
     | TyparConstraint.SupportsEquality _ 
     | TyparConstraint.SupportsNull _ 
     | TyparConstraint.IsNonNullableStruct _ 
     | TyparConstraint.IsUnmanaged _
     | TyparConstraint.IsReferenceType _ 
     | TyparConstraint.RequiresDefaultConstructor _ -> ()
and CheckTraitInfoDeep ((_,_,_,visitTraitSolution) as f) (TTrait(typs,_,_,argtys,rty,soln))  = 
    CheckTypesDeep f typs; 
    CheckTypesDeep f argtys; 
    Option.iter (CheckTypeDeep f) rty;
    Option.iter visitTraitSolution !soln

//--------------------------------------------------------------------------
// check for byref types
//--------------------------------------------------------------------------

let CheckForByrefLikeType cenv typ check = 
    CheckTypeDeep (ignore, (fun tcref -> if isByrefLikeTyconRef cenv.g tcref then check()), ignore, ignore) typ


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
           //   a) "Local" mutables, being mutables such that: 
           //         i)  the mutable has no arity (since arity implies top-level storage, top level mutables...) 
           //             Note: "this" arguments to instance members on mutable structs are mutable arguments. 
           //   b) BaseVal can never escape. 
           //   c) Byref typed values can never escape. 

           // These checks must correspond to the tests governing the error messages below. 
           let passedIn = ListSet.contains valEq v syntacticArgs 
           if passedIn then
               false
           else
               (v.IsMutable && v.ValReprInfo.IsNone) ||
               (v.BaseOrThisInfo = BaseVal  && not passedIn) ||
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
            elif v.IsMutable then 
                errorR(Error(FSComp.SR.chkMutableUsedInInvalidWay(v.DisplayName), m))
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
    

let CheckTypeForAccess (cenv:cenv) objName valAcc m ty =
    if cenv.reportErrors then 

        let visitType ty =         
            // We deliberately only check the fully stripped type for accessibility, because references to private type abbreviations are 
            // permitted
            match tryDestAppTy cenv.g ty with 
            | None -> ()
            | Some tcref ->
                let thisCompPath = compPathOfCcu cenv.viewCcu
                let tyconAcc = tcref.Accessibility |> AccessInternalsVisibleToAsInternal thisCompPath cenv.internalsVisibleToPaths
                if isLessAccessible tyconAcc valAcc then
                    errorR(Error(FSComp.SR.chkTypeLessAccessibleThanType(tcref.DisplayName, (objName())), m))

        CheckTypeDeep (visitType, ignore, ignore, ignore) ty 

//--------------------------------------------------------------------------
// check type instantiations
//--------------------------------------------------------------------------

/// Check types occuring in the TAST.
let CheckType permitByrefs (cenv:cenv) m ty =
    if cenv.reportErrors then 
        let visitTyconRef tcref = 
            if not permitByrefs && isByrefLikeTyconRef cenv.g tcref then 
                errorR(Error(FSComp.SR.chkErrorUseOfByref(), m))
            if tyconRefEq cenv.g cenv.g.system_Void_tcref tcref then 
                errorR(Error(FSComp.SR.chkSystemVoidOnlyInTypeof(), m))

        // check if T contains byref types in case of byref<T>
        let visitByrefsOfByrefs (tcref,tinst) = 
            if isByrefLikeTyconRef cenv.g tcref then
                let visitType ty0 =
                    match tryDestAppTy cenv.g ty0 with
                    | None -> ()
                    | Some tcref ->  
                        if isByrefLikeTyconRef cenv.g tcref then 
                            errorR(Error(FSComp.SR.chkNoByrefsOfByrefs(NicePrint.minimalStringOfType cenv.denv ty), m)) 
                CheckTypesDeep (visitType, ignore, ignore, ignore) tinst

        let visitTraitSolution info = 
            match info with 
            | FSMethSln(_,vref,_) -> 
               //printfn "considering %s..." vref.DisplayName
               if valRefInThisAssembly cenv.g.compilingFslib vref && not (cenv.boundVals.ContainsKey(vref.Stamp)) then 
                   //printfn "recording %s..." vref.DisplayName
                   cenv.potentialUnboundUsesOfVals <- cenv.potentialUnboundUsesOfVals.Add(vref.Stamp,m)
            | _ -> ()

        ty |> CheckTypeDeep (ignore, visitTyconRef, visitByrefsOfByrefs, visitTraitSolution);


/// Check types occuring in TAST (like CheckType) and additionally reject any byrefs.
/// The additional byref checks are to catch "byref instantiations" - one place were byref are not permitted.  
let CheckTypeNoByrefs (cenv:cenv) m ty = CheckType false cenv m ty
let CheckTypePermitByrefs (cenv:cenv) m ty = CheckType true cenv m ty

let CheckTypeInstNoByrefs (cenv:cenv) m tyargs =
    tyargs |> List.iter (CheckTypeNoByrefs cenv m)

let CheckTypeInstPermitByrefs (cenv:cenv) m tyargs =
    tyargs |> List.iter (CheckType true cenv m)

    
//--------------------------------------------------------------------------
// check exprs etc
//--------------------------------------------------------------------------
  
type ByrefCallContext = 
    /// Tuple of contexts allowing byref typed expr 
    | KnownArityTuple of int    
    /// Context allows for byref typed expr 
    | DirectArg                 
    /// General (byref type expr not allowed) 
    | GeneralContext            

let mkKnownArity n = if n=1 then DirectArg else KnownArityTuple n

let argAritiesOfVal (vref:ValRef) = 
    match vref.ValReprInfo with
    | Some topValInfo -> List.map mkKnownArity topValInfo.AritiesOfArgs
    | None -> []  

let rec argAritiesOfFunExpr x =
    match x with 
    | Expr.Val (vref,_,_)         -> argAritiesOfVal     vref      (* recognise val *)
    | Expr.Link eref              -> argAritiesOfFunExpr !eref     (* step through reclink  *)
    | Expr.App(f,_fty,_tyargs,[],_) -> argAritiesOfFunExpr f         (* step through instantiations *)
    | Expr.Op(TOp.Coerce,_,[f],_) -> argAritiesOfFunExpr f         (* step through subsumption coercions *)
    | _                            -> []
    

let CheckNoReraise cenv freesOpt (body:Expr) = 
    if cenv.reportErrors then
        // Avoid recomputing the free variables 
        let fvs = match freesOpt with None -> freeInExpr CollectLocals body | Some fvs -> fvs
        if fvs.UsesUnboundRethrow then
            errorR(Error(FSComp.SR.chkErrorContainsCallToRethrow(), body.Range))

let is_splice g v = valRefEq g v g.splice_expr_vref || valRefEq g v g.splice_raw_expr_vref 

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


let rec CheckExpr   (cenv:cenv) (env:env) expr = 
    CheckExprInContext cenv env expr GeneralContext

and CheckVal (cenv:cenv) (env:env) v m context = 
    if cenv.reportErrors then 
          if is_splice cenv.g v && not env.quote then errorR(Error(FSComp.SR.chkSplicingOnlyInQuotations(), m));
          if is_splice cenv.g v then errorR(Error(FSComp.SR.chkNoFirstClassSplicing(), m));
          if valRefEq cenv.g v cenv.g.addrof_vref  then errorR(Error(FSComp.SR.chkNoFirstClassAddressOf(), m));
          if valRefEq cenv.g v cenv.g.reraise_vref then errorR(Error(FSComp.SR.chkNoFirstClassRethrow(), m));
          if isByrefLikeTy cenv.g v.Type then 
              // byref typed val can only occur in permitting contexts 
              if context <> DirectArg then errorR(Error(FSComp.SR.chkNoByrefAtThisPoint(v.DisplayName), m))
    CheckTypePermitByrefs cenv m v.Type
    
and CheckExprInContext (cenv:cenv) (env:env) expr (context:ByrefCallContext) =    
    // dprintf "CheckExpr: %s\n" (showL(exprL expr));
    let expr = stripExpr expr

    match expr with
    | Expr.Sequential (e1,e2,dir,_,_) -> 
        CheckExpr cenv env e1;
        match dir with
        | NormalSeq -> CheckExprInContext cenv env e2 context       // carry context into _;RHS (normal sequencing only)      
        | ThenDoSeq -> CheckExpr cenv {env with limited=false} e2
    | Expr.Let (bind,body,_,_) ->  
        CheckBinding cenv env false bind ; 
        BindVal cenv bind.Var
        CheckExpr cenv env body
    | Expr.Const (_,m,ty) -> 
        CheckTypePermitByrefs cenv m ty 
            
    | Expr.Val (v,vFlags,m) -> 
          if cenv.reportErrors then 
              if v.BaseOrThisInfo = BaseVal then 
                errorR(Error(FSComp.SR.chkLimitationsOfBaseKeyword(), m))
              if (match vFlags with NormalValUse -> true | _ -> false) && 
                 v.IsConstructor && 
                 (match v.ActualParent with Parent tcref -> isAbstractTycon tcref.Deref | _ -> false) then 
                errorR(Error(FSComp.SR.tcAbstractTypeCannotBeInstantiated(),m));
          
          CheckVal cenv env v m context
          
    | Expr.Quote(ast,savedConv,_isFromQueryExpression,m,ty) -> 
          CheckExpr cenv {env with quote=true} ast;
          if cenv.reportErrors then 
              cenv.usesQuotations <- true
              try 
                  let conv = QuotationTranslator.ConvExprPublic 
                                (cenv.g, cenv.amap, cenv.viewCcu, QuotationTranslator.IsReflectedDefinition.No) 
                                QuotationTranslator.QuotationTranslationEnv.Empty ast  
                  match !savedConv with 
                  | None -> savedConv:= Some conv
                  | Some _ -> ()
              with QuotationTranslator.InvalidQuotedTerm e -> 
                  errorRecovery e m
                
          CheckTypeNoByrefs cenv m ty

    | Expr.Obj (_,typ,basev,superInitCall,overrides,iimpls,m) -> 
          CheckExpr cenv env superInitCall;
          CheckMethods cenv env basev overrides ;
          CheckInterfaceImpls cenv env basev iimpls;
          CheckTypePermitByrefs cenv m typ
          let interfaces = 
              [ yield! AllSuperTypesOfType cenv.g cenv.amap m AllowMultiIntfInstantiations.Yes typ
                for (ty,_) in iimpls do
                    yield! AllSuperTypesOfType cenv.g cenv.amap m AllowMultiIntfInstantiations.Yes ty ]
              |> List.filter (isInterfaceTy cenv.g)
          CheckMultipleInterfaceInstantiations cenv interfaces m

    // Allow base calls to F# methods
    | Expr.App((InnerExprPat(Expr.Val(v,vFlags,_) as f)),fty,tyargs,(Expr.Val(baseVal,_,_)::rest),m) 
          when ((match vFlags with VSlotDirectCall -> true | _ -> false) && 
                baseVal.BaseOrThisInfo = BaseVal) ->
        // dprintfn "GOT BASE VAL USE"
        let memberInfo = Option.get v.MemberInfo
        if memberInfo.MemberFlags.IsDispatchSlot then
            errorR(Error(FSComp.SR.tcCannotCallAbstractBaseMember(v.DisplayName),m));
        else         
            CheckVal cenv env v m GeneralContext
            CheckVal cenv env baseVal m GeneralContext
            CheckTypePermitByrefs cenv m fty;
            CheckTypeInstPermitByrefs cenv m tyargs;
            CheckExprsInContext cenv env rest (argAritiesOfFunExpr f)

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
                    errorR(Error(FSComp.SR.tcCannotCallAbstractBaseMember(mdef.Name),m));
            with _ -> () // defensive coding
        | _ -> ()
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckTypeInstNoByrefs cenv m enclTypeArgs;
        CheckTypeInstNoByrefs cenv m methTypeArgs;
        CheckTypeInstNoByrefs cenv m tys;
        CheckVal cenv env baseVal m GeneralContext
        CheckExprDirectArgs cenv env rest

    | Expr.Op (c,tyargs,args,m) ->
          CheckExprOp cenv env (c,tyargs,args,m) context

    // Allow 'typeof<System.Void>' calls as a special case, the only accepted use of System.Void! 
    | TypeOfExpr cenv.g ty when isVoidTy cenv.g ty ->
          () // typeof<System.Void> allowed. Special case. No further checks.

    | TypeDefOfExpr cenv.g ty when isVoidTy cenv.g ty ->
          () // typedefof<System.Void> allowed. Special case. No further checks.

    // Allow '%expr' in quotations
    | Expr.App(Expr.Val(vref,_,_),_,tinst,[arg],m) when is_splice cenv.g vref && env.quote ->
          CheckTypeInstPermitByrefs cenv m tinst;
          CheckExpr cenv env arg


    | Expr.App(f,fty,tyargs,argsl,m) ->
        // dprintfn "NO BASE VAL USE"
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckTypePermitByrefs cenv m fty;
        CheckTypeInstPermitByrefs cenv m tyargs;
        CheckExpr cenv env f;
        CheckExprsInContext cenv env argsl (argAritiesOfFunExpr f)

    (* REVIEW: fold the next two cases together *)
    | Expr.Lambda(_,_ctorThisValOpt,_baseValOpt,argvs,_,m,rty) -> 
        let topValInfo = ValReprInfo ([],[argvs |> List.map (fun _ -> ValReprInfo.unnamedTopArg1)],ValReprInfo.unnamedRetVal) 
        let ty = mkMultiLambdaTy m argvs rty in 
        CheckLambdas None cenv env false topValInfo false expr m ty

    | Expr.TyLambda(_,tps,_,m,rty)  -> 
        let topValInfo = ValReprInfo (ValReprInfo.InferTyparInfo tps,[],ValReprInfo.unnamedRetVal) 
        let ty = tryMkForallTy tps rty in 
        CheckLambdas None cenv env false topValInfo false expr m ty

    | Expr.TyChoose(_,e1,_)  -> 
        CheckExpr cenv env e1 

    | Expr.Match(_,_,dtree,targets,m,ty) -> 
        CheckTypeNoByrefs cenv m ty;
        CheckDecisionTree cenv env dtree;
        CheckDecisionTreeTargets cenv env targets;
    | Expr.LetRec (binds,e,_,_) ->  
        BindVals cenv (valsOfBinds binds)
        CheckBindings cenv env binds;
        CheckExpr cenv env e
    | Expr.StaticOptimization (constraints,e2,e3,m) -> 
        CheckExpr cenv env e2;
        CheckExpr cenv env e3;
        constraints |> List.iter (function
            | TTyconEqualsTycon(ty1,ty2) -> 
                CheckTypeNoByrefs cenv m ty1;
                CheckTypeNoByrefs cenv m ty2
            | TTyconIsStruct(ty1) -> 
                CheckTypeNoByrefs cenv m ty1)
    | Expr.Link _ -> 
        failwith "Unexpected reclink"

and CheckMethods cenv env baseValOpt l = 
    l |> List.iter (CheckMethod cenv env baseValOpt) 

and CheckMethod cenv env baseValOpt (TObjExprMethod(_,attribs,tps,vs,e,m)) = 
    let env = BindTypars env tps 
    let vs = List.concat vs
    CheckAttribs cenv env attribs;
    CheckNoReraise cenv None e;
    CheckEscapes cenv true m (match baseValOpt with Some x -> x:: vs | None -> vs) e |> ignore
    CheckExpr cenv env e

and CheckInterfaceImpls cenv env baseValOpt l = 
    l |> List.iter (CheckInterfaceImpl cenv env baseValOpt)
    
and CheckInterfaceImpl cenv env baseValOpt (_ty,overrides) = 
    CheckMethods cenv env baseValOpt overrides 


and CheckExprOp cenv env (op,tyargs,args,m) context =
    let limitedCheck() = 
        if env.limited then errorR(Error(FSComp.SR.chkObjCtorsCantUseExceptionHandling(), m));
    List.iter (CheckTypePermitByrefs cenv m) tyargs;
    (* Special cases *)
    match op,tyargs,args,context with 
    // Handle these as special cases since mutables are allowed inside their bodies 
    | TOp.While _,_,[Expr.Lambda(_,_,_,[_],e1,_,_);Expr.Lambda(_,_,_,[_],e2,_,_)],_  ->
        CheckTypeInstNoByrefs cenv m tyargs; 
        CheckExprs cenv env [e1;e2]

    | TOp.TryFinally _,[_],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],e2,_,_)],_ ->
        CheckTypeInstNoByrefs cenv m tyargs; 
        limitedCheck();
        CheckExprs cenv env [e1;e2]

    | TOp.For(_),_,[Expr.Lambda(_,_,_,[_],e1,_,_);Expr.Lambda(_,_,_,[_],e2,_,_);Expr.Lambda(_,_,_,[_],e3,_,_)],_  ->
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprs cenv env [e1;e2;e3]

    | TOp.TryCatch _,[_],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],_e2,_,_); Expr.Lambda(_,_,_,[_],e3,_,_)],_ ->
        CheckTypeInstNoByrefs cenv m tyargs;
        limitedCheck();
        CheckExprs cenv env [e1;(* e2; -- don't check filter body - duplicates logic in 'catch' body *) e3]

    | TOp.ILCall (_,_,_,_,_,_,_,_,enclTypeArgs,methTypeArgs,tys),_,_,_ ->
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckTypeInstNoByrefs cenv m enclTypeArgs;
        CheckTypeInstNoByrefs cenv m methTypeArgs;
        CheckTypeInstNoByrefs cenv m tys;
        CheckExprDirectArgs cenv env args  

    // Tuple expression in known tuple context
    | TOp.Tuple,_,_,KnownArityTuple nArity ->           
        if cenv.reportErrors then 
            if args.Length <> nArity then 
                errorR(InternalError("Tuple arity does not correspond to planned function argument arity",m));
        // This tuple should not be generated. The known function arity 
        // means it just bundles arguments. 
        CheckExprDirectArgs cenv env args 

    | TOp.LValueOp(LGetAddr,v),_,_,arity -> 
        if arity = DirectArg then       
          CheckExprs cenv env args                   (* Address-of operator generates byref, and context permits this. *)
        else            
          if cenv.reportErrors  then 
              errorR(Error(FSComp.SR.chkNoAddressOfAtThisPoint(v.DisplayName), m))
    | TOp.ValFieldGet _rf,_,[arg1],_arity -> 
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprDirectArgs cenv env [arg1]          (* See mkRecdFieldGetViaExprAddr -- byref arg1 when #args =1 *)
                                                        (* Property getters on mutable structs come through here. *)
    | TOp.ValFieldSet _rf,_,[arg1;arg2],_arity -> 
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprDirectArgs cenv env [arg1];         (* See mkRecdFieldSetViaExprAddr -- byref arg1 when #args=2 *)
        CheckExprs            cenv env [arg2]          (* Property setters on mutable structs come through here (TBC). *)
    | TOp.Coerce,[_ty1;_ty2],[x],_arity ->
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprInContext cenv env x context
    | TOp.Reraise,[_ty1],[],_arity ->
        CheckTypeInstNoByrefs cenv m tyargs        
    | TOp.ValFieldGetAddr rfref,tyargs,[],_ ->
        if context <> DirectArg && cenv.reportErrors  then
          errorR(Error(FSComp.SR.chkNoAddressStaticFieldAtThisPoint(rfref.FieldName), m)); 
        CheckTypeInstNoByrefs cenv m tyargs
        (* NOTE: there are no arg exprs to check in this case *)
    | TOp.ValFieldGetAddr rfref,tyargs,[rx],_ ->
        if context <> DirectArg && cenv.reportErrors then
          errorR(Error(FSComp.SR.chkNoAddressFieldAtThisPoint(rfref.FieldName), m));
        (* This construct is used for &(rx.rfield) and &(rx->rfield). Relax to permit byref types for rx. [See Bug 1263]. *)
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprInContext cenv env rx DirectArg (* allow rx to be byref here *)
    | TOp.ILAsm (instrs,tys),_,_,_  ->
        CheckTypeInstPermitByrefs cenv m tys;
        CheckTypeInstNoByrefs cenv m tyargs;
        begin
            match instrs,args with
            | [ I_stfld (_alignment,_vol,_fspec) ],[lhs;rhs] ->
                CheckExprInContext cenv env lhs DirectArg; (* permit byref for lhs lvalue *)
                CheckExpr         cenv env rhs                
            | [ I_ldfld (_alignment,_vol,_fspec) ],[lhs] ->
                CheckExprInContext cenv env lhs DirectArg  (* permit byref for lhs lvalue *)
            | [ I_ldfld (_alignment,_vol,_fspec); AI_nop ],[lhs] ->
                CheckExprInContext cenv env lhs DirectArg  (* permit byref for lhs lvalue of readonly value *)
            | [ I_ldflda (fspec) | I_ldsflda (fspec) ],[lhs] ->
                if context <> DirectArg && cenv.reportErrors then
                  errorR(Error(FSComp.SR.chkNoAddressFieldAtThisPoint(fspec.Name), m));
                CheckExprInContext cenv env lhs DirectArg  (* permit byref for lhs lvalue *)
            | [ I_ldelema (_,isNativePtr,_,_) ],lhsArray::indices ->
                if not(isNativePtr) && context <> DirectArg && cenv.reportErrors then
                  errorR(Error(FSComp.SR.chkNoAddressOfArrayElementAtThisPoint(), m));
                CheckExprInContext cenv env lhsArray DirectArg  (* permit byref for lhs lvalue *)
                CheckExprs cenv env indices
            | _instrs ->
                CheckExprs cenv env args  
        end

    | TOp.TraitCall _,_,_,_ ->
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprDirectArgs cenv env args (* allow args to be byref here *)

    | (   TOp.Tuple
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
        ),_,_,_ ->    
        CheckTypeInstNoByrefs cenv m tyargs;
        CheckExprs cenv env args 

and CheckLambdas memInfo cenv env inlined topValInfo alwaysCheckNoReraise e m ety =
    // The topValInfo here says we are _guaranteeing_ to compile a function value 
    // as a .NET method with precisely the corresponding argument counts. 
    match e with
    | Expr.TyChoose(tps,e1,m)  -> 
        let env = BindTypars env tps
        CheckLambdas memInfo cenv env inlined topValInfo alwaysCheckNoReraise e1 m ety      

    | Expr.Lambda (_,_,_,_,_,m,_)  
    | Expr.TyLambda(_,_,_,m,_) ->

        let tps,ctorThisValOpt,baseValOpt,vsl,body,bodyty = destTopLambda cenv.g cenv.amap topValInfo (e, ety) in
        let env = BindTypars env tps 
        let thisAndBase = Option.toList ctorThisValOpt @ Option.toList baseValOpt
        let restArgs = List.concat vsl
        let syntacticArgs = thisAndBase @ restArgs

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

        syntacticArgs |> List.iter (CheckValSpec cenv env);
        syntacticArgs |> List.iter (BindVal cenv);

        // Allow access to protected things within members
        match memInfo with 
        | None -> ()
        | Some membInfo -> 
            testHookMemberBody membInfo body;
        
        let freesOpt = CheckEscapes cenv (isSome(memInfo)) m syntacticArgs body;
        CheckNoReraise cenv freesOpt body; (* no reraise under lambda expression *)
        CheckExpr cenv env body;
        if cenv.reportErrors then 
            if not inlined then 
                CheckForByrefLikeType cenv bodyty (fun () -> 
                    if vsl.Length = 0 then
                        errorR(Error(FSComp.SR.chkFirstClassFuncNoByref(), m))
                    else
                        errorR(Error(FSComp.SR.chkReturnTypeNoByref(), m)))
            for tp in tps do 
                if tp.Constraints |> List.sumBy (function TyparConstraint.CoercesTo(ty,_) when isClassTy cenv.g ty -> 1 | _ -> 0) > 1 then 
                    errorR(Error(FSComp.SR.chkTyparMultipleClassConstraints(), m))
                
    | _ -> 
        CheckTypePermitByrefs cenv m ety;
        if not inlined && isByrefLikeTy cenv.g ety then
            CheckExprInContext cenv env e DirectArg           (* allow byref to occur as RHS of byref binding. *)
        else 
            CheckExpr cenv env e
        if alwaysCheckNoReraise then 
            CheckNoReraise cenv None e; (* no reraise *)

and CheckExprsInContext cenv env exprs arities =
    let arities = Array.ofList arities 
    let argArity i = if i < arities.Length then arities.[i] else GeneralContext 
    exprs |> List.iteri (fun i exp -> CheckExprInContext cenv env exp (argArity i)) 

and CheckExprs cenv env exprs = 
    exprs |> List.iter (CheckExpr cenv env) 

and CheckFlatExprs cenv env exprs = 
    exprs |> FlatList.iter (CheckExpr cenv env) 

and CheckExprDirectArgs cenv env exprs = 
    exprs |> List.iter (fun x -> CheckExprInContext cenv env x DirectArg) 

and CheckDecisionTreeTargets cenv env targets = 
    targets |> Array.iter (CheckDecisionTreeTarget cenv env) 

and CheckDecisionTreeTarget cenv env (TTarget(vs,e,_)) = 
    BindVals cenv vs 
    vs |> FlatList.iter (CheckValSpec cenv env)
    CheckExpr cenv env e

and CheckDecisionTree cenv env x =
    match x with 
    | TDSuccess (es,_) -> CheckFlatExprs cenv env es;
    | TDBind(bind,rest) -> CheckBinding cenv env false bind; CheckDecisionTree cenv env rest 
    | TDSwitch (e,cases,dflt,m) -> CheckDecisionTreeSwitch cenv env (e,cases,dflt,m)

and CheckDecisionTreeSwitch cenv env (e,cases,dflt,m) =
    CheckExpr cenv env e;
    List.iter (fun (TCase(discrim,e)) -> CheckDecisionTreeTest cenv env m discrim; CheckDecisionTree cenv env e) cases;
    Option.iter (CheckDecisionTree cenv env) dflt

and CheckDecisionTreeTest cenv env m discrim =
    match discrim with
    | Test.UnionCase (_,tinst) -> CheckTypeInstPermitByrefs cenv m tinst
    | Test.ArrayLength (_,typ)      -> CheckTypePermitByrefs cenv m typ
    | Test.Const _                   -> ()
    | Test.IsNull                    -> ()
    | Test.IsInst (srcTyp,dstTyp)    -> (CheckTypePermitByrefs cenv m srcTyp; CheckTypePermitByrefs cenv m dstTyp)
    | Test.ActivePatternCase (exp,_,_,_,_)     -> CheckExpr cenv env exp

and CheckAttrib cenv env (Attrib(_,_,args,props,_,_,_)) = 
    props |> List.iter (fun (AttribNamedArg(_,_,_,expr)) -> CheckAttribExpr cenv env expr);
    args |> List.iter (CheckAttribExpr cenv env)

and CheckAttribExpr cenv env (AttribExpr(expr,vexpr)) = 
    CheckExpr cenv env expr;
    CheckExpr cenv env vexpr;
    CheckNoReraise cenv None expr; 
    CheckAttribArgExpr cenv env vexpr

and CheckAttribArgExpr cenv env expr = 
    match expr with 

    (* Detect standard constants *)
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
                errorR (Error (FSComp.SR.tastConstantCannotBeCustomAttribute(), m))
                
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
        CheckAttribArgExpr cenv env arg1;
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
        |> List.filter (fun (tcref,m) -> 
                let allowMultiple = 
                    Infos.AttributeChecking.TryBindTyconRefAttribute cenv.g m cenv.g.attrib_AttributeUsageAttribute tcref 
                             (fun (_,named)             -> named |> List.tryPick (function ("AllowMultiple",_,_,ILAttribElem.Bool res) -> Some res | _ -> None))
                             (fun (Attrib(_,_,_,named,_,_,_)) -> named |> List.tryPick (function AttribNamedArg("AllowMultiple",_,_,AttribBoolArg(res) ) -> Some res | _ -> None))
                             (fun _ -> None)
                
                (allowMultiple <> Some(true)))
    if cenv.reportErrors then 
       for (tcref,m) in duplicates do
          errorR(Error(FSComp.SR.chkAttrHasAllowMultiFalse(tcref.DisplayName), m))
    
    attribs |> List.iter (CheckAttrib cenv env) 

and CheckValInfo cenv env (ValReprInfo(_,args,ret)) =
    args |> List.iterSquared (CheckArgInfo cenv env);
    ret |> CheckArgInfo cenv env;

and CheckArgInfo cenv env (argInfo : ArgReprInfo)  = 
    CheckAttribs cenv env argInfo.Attribs

and CheckValSpec cenv env (v:Val) =
    v.Attribs |> CheckAttribs cenv env;
    v.ValReprInfo |> Option.iter (CheckValInfo cenv env);
    v.Type |> CheckTypePermitByrefs cenv v.Range

and AdjustAccess isHidden (cpath: unit -> CompilationPath) access =
    if isHidden then 
        let (TAccess(l)) = access
        // FSharp 1.0 bug 1908: Values hidden by signatures are implicitly at least 'internal'
        let scoref = cpath().ILScopeRef
        TAccess(CompPath(scoref,[])::l)
    else 
        access

and CheckBinding cenv env alwaysCheckNoReraise (TBind(v,e,_) as bind) =
    //printfn "visiting %s..." v.DisplayName
    match TryGetActivePatternInfo (mkLocalValRef v) with 
    | Some _ -> 
        let vty  = v.TauType
        let vtps = v.Typars |> Zset.ofList typarOrder
        if not (isFunTy cenv.g v.TauType) then
            errorR(Error(FSComp.SR.activePatternIdentIsNotFunctionTyped(v.LogicalName),v.Range))
        let argtys,resty  = stripFunTy cenv.g vty
        let argtps,restps= (freeInTypes CollectTypars argtys).FreeTypars,(freeInType CollectTypars resty).FreeTypars        
        // Error if an active pattern is generic in type variables that only occur in the result Choice<_,...>.
        // Note: The test restricts to v.Typars since typars from the closure are considered fixed.
        if not (Zset.isEmpty (Zset.inter (Zset.diff restps argtps) vtps)) then
            errorR(Error(FSComp.SR.activePatternChoiceHasFreeTypars(v.LogicalName),v.Range))
    | _ -> ()
    
    match cenv.potentialUnboundUsesOfVals.TryFind v.Stamp with
    | None -> () 
    | Some m ->
         let nm = v.DisplayName
         errorR(Error(FSComp.SR.chkMemberUsedInInvalidWay(nm, nm, stringOfRange m), v.Range))

    v.Type |> CheckTypePermitByrefs cenv v.Range;
    v.Attribs |> CheckAttribs cenv env;
    v.ValReprInfo |> Option.iter (CheckValInfo cenv env);
    if (v.IsMemberOrModuleBinding || v.IsMember) && not v.IsIncrClassGeneratedMember then 
        let access =  AdjustAccess (IsHiddenVal env.sigToImplRemapInfo v) (fun () -> v.TopValActualParent.CompilationPath) v.Accessibility
        v.Type |> CheckTypeForAccess cenv (fun () -> NicePrint.stringOfQualifiedValOrMember cenv.denv v) access v.Range;
    
    let env = if v.IsConstructor && not v.IsIncrClassConstructor then { env with limited=true } else env

    if cenv.reportErrors  then 
        if isByrefLikeTy cenv.g v.Type && isSome bind.Var.ValReprInfo then    
            errorR(Error(FSComp.SR.chkNoByrefAsTopValue(), v.Range));

        // Check top-level let-bound values (arity=0 so not compiled not method) for byref types (not allowed) 
        match bind.Var.ValReprInfo with
          | Some info when info.HasNoArgs -> 
              CheckForByrefLikeType cenv v.Type (fun () -> errorR(Error(FSComp.SR.chkNoByrefAsTopValue(),v.Range)))
          | _ -> ()

        if isSome v.PublicPath then 
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
                | None -> v.Data.val_defn <- Some e
                | Some _ -> ()
                // Run the conversion process over the reflected definition to report any errors in the
                // front end rather than the back end. We currenly re-run this during ilxgen.fs but there's
                // no real need for that except that it helps us to bundle all reflected definitions up into 
                // one blob for pickling to the binary format
                try
                    let ety = tyOfExpr cenv.g e
                    let tps,taue,_ = 
                      match e with 
                      | Expr.TyLambda (_,tps,b,_,_) -> tps,b,applyForallTy cenv.g ety (List.map mkTyparTy tps)
                      | _ -> [],e,ety
                    let env = QuotationTranslator.QuotationTranslationEnv.Empty.BindTypars tps
                    let _,argExprs,_ = QuotationTranslator.ConvExprPublic 
                                            (cenv.g,cenv.amap,cenv.viewCcu, QuotationTranslator.IsReflectedDefinition.Yes) 
                                            env taue 
                    if nonNil(argExprs) then 
                        errorR(Error(FSComp.SR.chkReflectedDefCantSplice(), v.Range));
                    QuotationTranslator.ConvMethodBase (cenv.g,cenv.amap,cenv.viewCcu) env (v.CompiledName, v) |> ignore
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
        | _ -> 
            ()
        
    | _ -> ()
        
        
    let topValInfo  = match bind.Var.ValReprInfo with Some info -> info | _ -> ValReprInfo.emptyValData 
    let inlined     = v.MustInline 
      // certain inline functions are permitted to have byref return types
      // e.g. for the byref operator itself, &. 
    CheckLambdas v.MemberInfo cenv env inlined topValInfo alwaysCheckNoReraise e v.Range v.Type;

and CheckBindings cenv env xs = FlatList.iter (CheckBinding cenv env false) xs

// Top binds introduce expression, check they are reraise free.
let CheckTopBinding cenv env (TBind(v,e,_) as bind) =
    let isExplicitEntryPoint = HasFSharpAttribute cenv.g cenv.g.attrib_EntryPointAttribute v.Attribs
    if isExplicitEntryPoint then 
        cenv.entryPointGiven <- true;
        if not cenv.isLastCompiland && cenv.reportErrors  then 
            errorR(Error(FSComp.SR.chkEntryPointUsage(), v.Range)) 

    // Analyze the r.h.s. for the "IsCompiledAsStaticPropertyWithoutField" condition
    if // Mutable values always have fields
       not v.IsMutable && 
       // Literals always have fields
       not (HasFSharpAttribute cenv.g cenv.g.attrib_LiteralAttribute v.Attribs) && 
       not (HasFSharpAttribute cenv.g cenv.g.attrib_ThreadStaticAttribute v.Attribs) && 
       not (HasFSharpAttribute cenv.g cenv.g.attrib_ContextStaticAttribute v.Attribs) && 
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
                    
                    error(Duplicate(kind,v.DisplayName,v.Range));

#if CASES_IN_NESTED_CLASS
                if tcref.IsUnionTycon && nm = "Cases" then 
                    errorR(NameClash(nm,kind,v.DisplayName,v.Range, "generated type","Cases",tcref.Range));
#endif
                if tcref.IsUnionTycon then 
                    match nm with 
                    | "Tag" -> errorR(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoGeneratedProperty(),"Tag",tcref.Range));
                    | "Tags" -> errorR(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoGeneratedType(),"Tags",tcref.Range));
                    | _ ->
                        if hasDefaultAugmentation then 
                            match tcref.GetUnionCaseByName(nm) with 
                            | Some(uc) -> error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoUnionCase(),uc.DisplayName,uc.Range));
                            | None -> ()

                            let hasNoArgs = 
                                match v.ValReprInfo with 
                                | None -> false 
                                | Some arity -> List.sum arity.AritiesOfArgs - v.NumObjArgs <= 0 && arity.NumTypars = 0

                            // Unions with one alternative use "Item", "Item1", "Item2" etc. But only if these
                            // are properties without arguments.
                            if tcref.UnionCasesArray.Length = 1 && hasNoArgs then 
                               let ucase1 = tcref.UnionCasesArray.[0]
                               let nFields = ucase1.RecdFieldsArray.Length
                               for i in 0 .. nFields - 1 do
                                   let propName = if nFields <= 1 then "Item" else "Item"+string (i+1)
                                   if nm = propName then error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.typeInfoGeneratedProperty(),propName,ucase1.Range));

                // Default augmentation contains the nasty 'Case<UnionCase>' etc.
                let prefix = "New"
                if nm.StartsWith prefix then
                    match tcref.GetUnionCaseByName(nm.[prefix.Length ..]) with 
                    | Some(uc) -> error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.chkUnionCaseCompiledForm(),uc.DisplayName,uc.Range));
                    | None -> ()

                // Default augmentation contains the nasty 'Is<UnionCase>' etc.
                let prefix = "Is"
                if nm.StartsWith prefix && hasDefaultAugmentation then
                    match tcref.GetUnionCaseByName(nm.[prefix.Length ..]) with 
                    | Some(uc) -> error(NameClash(nm,kind,v.DisplayName,v.Range, FSComp.SR.chkUnionCaseDefaultAugmentation(),uc.DisplayName,uc.Range));
                    | None -> ()

                match tcref.GetFieldByName(nm) with 
                | Some(rf) -> error(NameClash(nm,kind,v.DisplayName,v.Range,"field",rf.Name,rf.Range));
                | None -> ()

            check false v.CoreDisplayName
            check false v.DisplayName
            check false v.CompiledName

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
        with e -> errorRecovery e v.Range; 
    end

    CheckBinding cenv env true bind

let CheckTopBindings cenv env binds = FlatList.iter (CheckTopBinding cenv env) binds

//--------------------------------------------------------------------------
// check tycons
//--------------------------------------------------------------------------

let CheckRecdField isUnion cenv env (tycon:Tycon) (rfield:RecdField) = 
    let isHidden = 
        IsHiddenTycon env.sigToImplRemapInfo tycon || 
        IsHiddenTyconRepr env.sigToImplRemapInfo tycon || 
        (not isUnion && IsHiddenRecdField env.sigToImplRemapInfo (mkNestedRecdFieldRef (mkLocalTyconRef tycon) rfield))
    let access =  AdjustAccess isHidden (fun () -> tycon.CompilationPath) rfield.Accessibility
    CheckTypeForAccess cenv (fun () -> rfield.Name) access rfield.Range rfield.FormalType;
    CheckTypePermitByrefs cenv rfield.Range rfield.FormalType;
    CheckAttribs cenv env rfield.PropertyAttribs;
    CheckAttribs cenv env rfield.FieldAttribs;
    if cenv.reportErrors then 
        CheckForByrefLikeType cenv rfield.FormalType (fun () -> errorR(Error(FSComp.SR.chkCantStoreByrefValue(), tycon.Range)))

let CheckEntityDefn cenv env (tycon:Entity) =
#if EXTENSIONTYPING
  if not tycon.IsProvidedGeneratedTycon then
#endif
    let env = { env with reflect = env.reflect || HasFSharpAttribute cenv.g cenv.g.attrib_ReflectedDefinitionAttribute tycon.Attribs }
    let m = tycon.Range 
    CheckAttribs cenv env tycon.Attribs;

    if cenv.reportErrors then begin
      if not tycon.IsTypeAbbrev then
        let typ = generalizedTyconRef (mkLocalTyconRef tycon)
        let allVirtualMethsInParent = 
            match GetSuperTypeOfType cenv.g cenv.amap m typ with 
            | Some super -> 
                GetIntrinsicMethInfosOfType cenv.infoReader (None,AccessibleFromSomewhere,AllowMultiIntfInstantiations.Yes)  IgnoreOverrides m super
                |> List.filter (fun minfo -> minfo.IsVirtual)
            | None -> []

        let namesOfMethodsThatMayDifferOnlyInReturnType = ["op_Explicit";"op_Implicit"] (* hardwired *)
        let methodUniquenessIncludesReturnType (minfo:MethInfo) = List.mem minfo.LogicalName namesOfMethodsThatMayDifferOnlyInReturnType
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
                    // use referencial identity to filter out 'minfo' method
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

            if minfo.NumArgs.Length > 1 && others |> List.exists (fun minfo2 -> not (IsAbstractDefaultPair2 minfo minfo2)) then 
                errorR(Error(FSComp.SR.chkDuplicateMethodCurried nm,m))

            if minfo.NumArgs.Length > 1 && 
               (minfo.GetParamDatas(cenv.amap, m, minfo.FormalMethodInst) 
                |> List.existsSquared (fun (ParamData(isParamArrayArg, isOutArg, optArgInfo, _, ty)) -> 
                    isParamArrayArg || isOutArg || optArgInfo.IsOptional || isByrefTy cenv.g ty)) then 
                errorR(Error(FSComp.SR.chkCurriedMethodsCantHaveOutParams(), m))

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
                  others |> List.exists (fun pinfo2 -> (pinfo2.GetParamTypes(cenv.amap,m).Length = 0) <> (nargs = 0)))) then 
                  
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

    end;
    
    // Considers TFsObjModelRepr, TRecdRepr and TFiniteUnionRepr. 
    // [Review] are all cases covered: TILObjModelRepr,TAsmRepr. [Yes - these are FSharp.Core.dll only]
    tycon.AllFieldsArray |> Array.iter (CheckRecdField false cenv env tycon);
    abstractSlotValsOfTycons [tycon] |> List.iter (typeOfVal >> CheckTypePermitByrefs cenv m); (* check vslots = abstract slots *)
    tycon.ImmediateInterfaceTypesOfFSharpTycon |> List.iter (CheckTypePermitByrefs cenv m);                   (* check implemented interface types *)
    superOfTycon cenv.g tycon |> CheckTypePermitByrefs cenv m;                                    (* check super type *)

    if tycon.IsUnionTycon then                             (* This covers finite unions. *)
        tycon.UnionCasesAsList |> List.iter (fun uc ->
            CheckAttribs cenv env uc.Attribs; 
            uc.RecdFields |> List.iter (CheckRecdField true cenv env tycon))

    let access =  AdjustAccess (IsHiddenTycon env.sigToImplRemapInfo tycon) (fun () -> tycon.CompilationPath) tycon.Accessibility
    let visitType ty = CheckTypeForAccess cenv (fun () -> tycon.DisplayNameWithStaticParametersAndUnderscoreTypars) access tycon.Range ty    
    abstractSlotValsOfTycons [tycon] |> List.iter (typeOfVal >> visitType); (* check vslots = abstract slots *)
    superOfTycon cenv.g tycon |> visitType
    // We do not have to check access of interface implementations. See FSharp 1.0 5042
    //implements_of_tycon cenv.g tycon |> List.iter visitType
    if tycon.IsFSharpDelegateTycon then 
        match tycon.TypeReprInfo with 
        | TFsObjModelRepr r ->
            match r.fsobjmodel_kind with 
            | TTyconDelegate ss ->
                //ss.ClassTypars 
                //ss.MethodTypars 
                ss.FormalReturnType |> Option.iter visitType;
                ss.FormalParams |> List.iterSquared (fun (TSlotParam(_,ty,_,_,_,_)) -> visitType ty)
            | _ -> ()
        | _ -> ()


    let interfaces = 
        AllSuperTypesOfType cenv.g cenv.amap tycon.Range AllowMultiIntfInstantiations.Yes (generalizedTyconRef (mkLocalTyconRef tycon)) 
            |> List.filter (isInterfaceTy cenv.g)
            
    if tycon.IsFSharpInterfaceTycon then List.iter visitType interfaces // Check inheritted interface is as accessible
 
    if cenv.reportErrors then 
        if not tycon.IsTypeAbbrev then 
            CheckMultipleInterfaceInstantiations cenv interfaces m
        
        // Check struct fields. We check these late because we have to have first checked that the structs are
        // free of cycles
        if tycon.IsStructOrEnumTycon then 
            tycon.AllInstanceFieldsAsList |> List.iter (fun f -> 
                // Check if it's marked unsafe 
                let zeroInitUnsafe = TryFindFSharpBoolAttribute cenv.g cenv.g.attrib_DefaultValueAttribute f.FieldAttribs
                if zeroInitUnsafe = Some(true) then
                   let ty' = generalizedTyconRef (mkLocalTyconRef tycon)
                   if not (TypeHasDefaultValue cenv.g ty') then 
                       errorR(Error(FSComp.SR.chkValueWithDefaultValueMustHaveDefaultValue(), m));
            )
        match tycon.TypeAbbrev with                          (* And type abbreviations *)
         | None     -> ()
         | Some typ -> 
             CheckForByrefLikeType cenv typ (fun () -> errorR(Error(FSComp.SR.chkNoByrefInTypeAbbrev(), tycon.Range)))

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
    | TMDefRec(tycons,binds,mspecs,m) -> 
        CheckNothingAfterEntryPoint cenv m
        BindVals cenv (valsOfBinds binds)
        CheckEntityDefns cenv env tycons; 
        CheckTopBindings cenv env binds;
        List.iter (CheckModuleSpec cenv env) mspecs
    | TMDefLet(bind,m)  -> 
        CheckNothingAfterEntryPoint cenv m
        CheckTopBinding cenv env bind 
        BindVal cenv bind.Var
    | TMDefDo(e,m)  -> 
        CheckNothingAfterEntryPoint cenv m
        CheckNoReraise cenv None e;
        CheckExpr cenv env e
    | TMAbstract(def)  -> CheckModuleExpr cenv env def
    | TMDefs(defs) -> CheckDefnsInModule cenv env defs 

and CheckModuleSpec cenv env (ModuleOrNamespaceBinding(mspec, rhs)) = 
    CheckEntityDefn cenv env mspec;
    let env = { env with reflect = env.reflect || HasFSharpAttribute cenv.g cenv.g.attrib_ReflectedDefinitionAttribute mspec.Attribs }
    CheckDefnInModule cenv env rhs 

let CheckTopImpl (g,amap,reportErrors,infoReader,internalsVisibleToPaths,viewCcu,denv ,mexpr,extraAttribs,isLastCompiland) =
    let cenv = 
        { g =g ; 
          reportErrors=reportErrors; 
          boundVals= new Dictionary<_,_>(100, HashIdentity.Structural); 
          potentialUnboundUsesOfVals=Map.empty; 
          usesQuotations=false; 
          infoReader=infoReader; 
          internalsVisibleToPaths=internalsVisibleToPaths;
          amap=amap; 
          denv=denv; 
          viewCcu= viewCcu;
          isLastCompiland=isLastCompiland; 
          entryPointGiven=false}
    
    // Certain type equality checks go faster if these TyconRefs are pre-resolved.
    // This is because pre-resolving allows tycon equality to be determined by pointer equality on the entities.
    // See primEntityRefEq.
    cenv.g.system_Void_tcref.TryDeref                  |> ignore
    cenv.g.byref_tcr.TryDeref                          |> ignore
    cenv.g.system_TypedReference_tcref.TryDeref        |> ignore
    cenv.g.system_ArgIterator_tcref.TryDeref           |> ignore 
    cenv.g.system_RuntimeArgumentHandle_tcref.TryDeref |> ignore

    let env = 
        { sigToImplRemapInfo=[]
          quote=false
          limited=false 
          boundTypars=[]
          reflect=false }

    CheckModuleExpr cenv env mexpr;
    CheckAttribs cenv env extraAttribs;
    if cenv.usesQuotations then 
        viewCcu.UsesQuotations <- true
    cenv.entryPointGiven
