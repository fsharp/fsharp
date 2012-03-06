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


/// Primary relations on types and signatures, with the exception of
/// constraint solving and method overload resolution.
module internal Microsoft.FSharp.Compiler.Typrelns

open Internal.Utilities
open System.Text

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.Infos.AccessibilityLogic


//-------------------------------------------------------------------------
// a :> b without coercion based on finalized (no type variable) types
//------------------------------------------------------------------------- 


// QUERY: This relation is approximate and not part of the language specification. 
//
//  Some appropriate uses: 
//     patcompile.fs: IsDiscrimSubsumedBy (approximate warning for redundancy of 'isinst' patterns)
//     tc.fs: TcRuntimeTypeTest (approximate warning for redundant runtime type tests)
//     tc.fs: TcExnDefnCore (error for bad exception abbreviation)
//     ilxgen.fs: GenCoerce (omit unecessary castclass or isinst instruction)
//
let rec TypeDefinitelySubsumesTypeNoCoercion ndeep g amap m ty1 ty2 = 
  if ndeep > 100 then error(InternalError("recursive class hierarchy (detected in TypeDefinitelySubsumesTypeNoCoercion), ty1 = "^(DebugPrint.showType ty1),m));
  if ty1 === ty2 then true 
  // QUERY : quadratic
  elif typeEquiv g ty1 ty2 then true
  else
    let ty1 = stripTyEqns g ty1
    let ty2 = stripTyEqns g ty2
    match ty1,ty2 with 
    | TType_app (tc1,l1)  ,TType_app (tc2,l2) when tyconRefEq g tc1 tc2  ->  
        List.lengthsEqAndForall2 (typeEquiv g) l1 l2
    | TType_ucase (tc1,l1)  ,TType_ucase (tc2,l2) when g.ucref_eq tc1 tc2  ->  
        List.lengthsEqAndForall2 (typeEquiv g) l1 l2
    | TType_tuple l1    ,TType_tuple l2     -> 
        List.lengthsEqAndForall2 (typeEquiv g) l1 l2 
    | TType_fun (d1,r1)  ,TType_fun (d2,r2)   -> 
        typeEquiv g d1 d2 && typeEquiv g r1 r2
    | TType_measure measure1, TType_measure measure2 ->
        measureEquiv g measure1 measure2
    | _ ->  
        (typeEquiv g ty1 g.obj_ty && isRefTy g ty2) || (* F# reference types are subtypes of type 'obj' *)
        (isAppTy g ty2 &&
         isRefTy g ty2 && 

         ((match SuperTypeOfType g amap m ty2 with 
           | None -> false
           | Some ty -> TypeDefinitelySubsumesTypeNoCoercion (ndeep+1) g amap m ty1 ty) ||

           (isInterfaceTy g ty1 &&
            ty2 |> InterfacesOfType g amap m 
                |> List.exists (TypeDefinitelySubsumesTypeNoCoercion (ndeep+1) g amap m ty1))))



type canCoerce = CanCoerce | NoCoerce

/// The feasible equivalence relation. Part of the language spec.
let rec TypesFeasiblyEquiv ndeep g amap m ty1 ty2 = 

    if ndeep > 100 then error(InternalError("recursive class hierarchy (detected in TypeFeasiblySubsumesType), ty1 = "^(DebugPrint.showType ty1),m));
    let ty1 = stripTyEqns g ty1
    let ty2 = stripTyEqns g ty2
    match ty1,ty2 with 
    // QUERY: should these be false for non-equal rigid typars? warn-if-not-rigid typars?
    | TType_var _ , _  
    | _, TType_var _ -> true
    | TType_app (tc1,l1)  ,TType_app (tc2,l2) when tyconRefEq g tc1 tc2  ->  
        List.lengthsEqAndForall2 (TypesFeasiblyEquiv ndeep g amap m) l1 l2
    | TType_tuple l1    ,TType_tuple l2     -> 
        List.lengthsEqAndForall2 (TypesFeasiblyEquiv ndeep g amap m) l1 l2 
    | TType_fun (d1,r1)  ,TType_fun (d2,r2)   -> 
        (TypesFeasiblyEquiv ndeep g amap m) d1 d2 && (TypesFeasiblyEquiv ndeep g amap m) r1 r2
    | TType_measure _, TType_measure _ ->
        true
    | _ -> 
        false

/// The feasible coercion relation. Part of the language spec.

let rec TypeFeasiblySubsumesType ndeep g amap m ty1 canCoerce ty2 = 
    if ndeep > 100 then error(InternalError("recursive class hierarchy (detected in TypeFeasiblySubsumesType), ty1 = "^(DebugPrint.showType ty1),m));
    let ty1 = stripTyEqns g ty1
    let ty2 = stripTyEqns g ty2
    match ty1,ty2 with 
    // QUERY: should these be false for non-equal rigid typars? warn-if-not-rigid typars?
    | TType_var _ , _  | _, TType_var _ -> true

    | TType_app (tc1,l1)  ,TType_app (tc2,l2) when tyconRefEq g tc1 tc2  ->  
        List.lengthsEqAndForall2 (TypesFeasiblyEquiv ndeep g amap m) l1 l2
    | TType_tuple l1    ,TType_tuple l2     -> 
        List.lengthsEqAndForall2 (TypesFeasiblyEquiv ndeep g amap m) l1 l2 
    | TType_fun (d1,r1)  ,TType_fun (d2,r2)   -> 
        (TypesFeasiblyEquiv ndeep g amap m) d1 d2 && (TypesFeasiblyEquiv ndeep g amap m) r1 r2
    | TType_measure _, TType_measure _ ->
        true
    | _ -> 
        // F# reference types are subtypes of type 'obj' 
        (isObjTy g ty1 && (canCoerce = CanCoerce || isRefTy g ty2)) 
        ||
        (isAppTy g ty2 &&
         (canCoerce = CanCoerce || isRefTy g ty2) && 
         begin match SuperTypeOfType g amap m ty2 with 
         | None -> false
         | Some ty -> TypeFeasiblySubsumesType (ndeep+1) g amap m ty1 NoCoerce ty
         end ||
         ty2 |> InterfacesOfType g amap m 
             |> List.exists (TypeFeasiblySubsumesType (ndeep+1) g amap m ty1 NoCoerce))
                   

/// Choose solutions for Expr.TyChoose type "hidden" variables introduced
/// by letrec nodes. Also used by the pattern match compiler to choose type
/// variables when compiling patterns at generalized bindings.
///     e.g. let ([],x) = ([],[])
/// Here x gets a generalized type "list<'T>".
let ChooseTyparSolutionAndRange g amap (tp:Typar) =
    let m = tp.Range
    if verbose then dprintf "ChooseTyparSolution, arbitrary: tp = %s\n" (Layout.showL (typarsL [tp]));
    let max,m = 
         let initial = 
             match tp.Kind with 
             | KindType -> g.obj_ty 
             | KindMeasure -> TType_measure MeasureOne
         // Loop through the constraints computing the lub
         ((initial,m), tp.Constraints) ||> List.fold (fun (maxSoFar,_) tpc -> 
             let join m x = 
                 if TypeFeasiblySubsumesType 0 g amap m x CanCoerce maxSoFar then maxSoFar
                 elif TypeFeasiblySubsumesType 0 g amap m maxSoFar CanCoerce x then x
                 else errorR(Error(FSComp.SR.typrelCannotResolveImplicitGenericInstantiation((DebugPrint.showType x), (DebugPrint.showType maxSoFar)),m)); maxSoFar
             // Don't continue if an error occurred and we set the value eagerly 
             if tp.IsSolved then maxSoFar,m else
             match tpc with 
             | TTyparCoercesToType(x,m) -> 
                 join m x,m
             | TTyparMayResolveMemberConstraint(TTrait(_,nm,_,_,_,_),m) -> 
                 errorR(Error(FSComp.SR.typrelCannotResolveAmbiguityInOverloadedOperator(DemangleOperatorName nm),m));
                 maxSoFar,m
             | TTyparSimpleChoice(_,m) -> 
                 errorR(Error(FSComp.SR.typrelCannotResolveAmbiguityInPrintf(),m));
                 maxSoFar,m
             | TTyparSupportsNull m -> 
                 maxSoFar,m
             | TTyparSupportsComparison m -> 
                 join m g.mk_IComparable_ty,m
             | TTyparSupportsEquality m -> 
                 maxSoFar,m
             | TTyparIsEnum(_,m) -> 
                 errorR(Error(FSComp.SR.typrelCannotResolveAmbiguityInEnum(),m));
                 maxSoFar,m
             | TTyparIsDelegate(_,_,m) -> 
                 errorR(Error(FSComp.SR.typrelCannotResolveAmbiguityInDelegate(),m));
                 maxSoFar,m
             | TTyparIsNotNullableValueType m -> 
                 join m g.int_ty,m
             | TTyparIsUnmanaged m ->
                 errorR(Error(FSComp.SR.typrelCannotResolveAmbiguityInUnmanaged(),m))
                 maxSoFar,m
             | TTyparRequiresDefaultConstructor m -> 
                 maxSoFar,m
             | TTyparIsReferenceType m -> 
                 maxSoFar,m
             | TTyparDefaultsToType(_priority,_ty,m) -> 
                 maxSoFar,m)
    max,m

let ChooseTyparSolution g amap tp = 
    let ty,_m = ChooseTyparSolutionAndRange g amap tp
    if tp.Rigidity = TyparAnon && typeEquiv g ty (TType_measure MeasureOne) then
        warning(Error(FSComp.SR.csCodeLessGeneric(),tp.Range));
    ty

// Solutions can, in theory, refer to each other
// For example
//   'a = Expr<'b>
//   'b = int
// In this case the solutions are 
//   'a = Expr<int>
//   'b = int
// We ground out the solutions by repeatedly instantiating
let IterativelySubstituteTyparSolutions g tps solutions = 
    let rec loop n curr = 
        let tpenv = mkTyparInst tps curr
        let curr' = curr |> instTypes tpenv 
        // We cut out at n > 40 just in case this loops. It shouldn't, since there should be no cycles in the
        // solution equations, and we've only ever seen one example where even n = 2 was required.
        // Perhaps it's possible in error recovery some strange situations could occur where cycles
        // arise, so it's better to be on the safe side.
        //
        // We don't give an error if we hit the limit since it's feasible that the solutions of unknowns
        // is not actually relevant to the rest of type checking or compilation.
        if n > 40 || List.forall2 (typeEquiv g) curr curr' then 
            curr 
        else 
            loop (n+1) curr'

    loop 0 solutions

let ChooseTyparSolutionsForFreeChoiceTypars g amap e = 
    match e with 
    | Expr.TyChoose(tps,e1,_m)  -> 
    
        /// Only make choices for variables that are actually used in the expression 
        let ftvs = (freeInExpr CollectTyparsNoCaching e1).FreeTyvars.FreeTypars
        let tps = tps |> List.filter (Zset.memberOf ftvs)
        
        let solutions =  tps |> List.map (ChooseTyparSolution g amap) |> IterativelySubstituteTyparSolutions g tps
        
        let tpenv = mkTyparInst tps solutions
        
        instExpr g tpenv e1

    | _ -> e
                 

/// Break apart lambdas. Needs ChooseTyparSolutionsForFreeChoiceTypars because it's used in
/// PostTypecheckSemanticChecks before we've eliminated these nodes.
let tryDestTopLambda g amap (ValReprInfo (tpNames,_,_) as tvd) (e,ty) =
    let rec stripLambdaUpto n (e,ty) = 
        match e with 
        | Expr.Lambda (_,None,None,v,b,_,retTy) when n > 0 -> 
            let (vs',b',retTy') = stripLambdaUpto (n-1) (b,retTy)
            (v :: vs', b', retTy') 
        | _ -> 
            ([],e,ty)

    let rec startStripLambdaUpto n (e,ty) = 
        match e with 
        | Expr.Lambda (_,ctorThisValOpt,baseValOpt,v,b,_,retTy) when n > 0 -> 
            let (vs',b',retTy') = stripLambdaUpto (n-1) (b,retTy)
            (ctorThisValOpt,baseValOpt, (v :: vs'), b', retTy') 
        | Expr.TyChoose (_tps,_b,_) -> 
            startStripLambdaUpto n (ChooseTyparSolutionsForFreeChoiceTypars g amap e, ty)
        | _ -> 
            (None,None,[],e,ty)

    let n = tvd.NumCurriedArgs
    let tps,taue,tauty = 
        match e with 
        | Expr.TyLambda (_,tps,b,_,retTy) when nonNil tpNames -> tps,b,retTy 
        | _ -> [],e,ty
    let ctorThisValOpt,baseValOpt,vsl,body,retTy = startStripLambdaUpto n (taue,tauty)
    if vsl.Length <> n then 
        None 
    else
        Some (tps,ctorThisValOpt,baseValOpt,vsl,body,retTy)

let destTopLambda g amap topValInfo (e,ty) = 
    match tryDestTopLambda g amap topValInfo (e,ty) with 
    | None -> error(Error(FSComp.SR.typrelInvalidValue(), e.Range));
    | Some res -> res
    
let IteratedAdjustArityOfLambdaBody g arities vsl body  =
      (arities, vsl, ([],body)) |||> List.foldBack2 (fun arities vs (allvs,body) -> 
          let vs,body = AdjustArityOfLambdaBody g arities vs body
          vs :: allvs, body)

/// Do AdjustArityOfLambdaBody for a series of  
/// iterated lambdas, producing one method.  
/// The required iterated function arity (List.length topValInfo) must be identical 
/// to the iterated function arity of the input lambda (List.length vsl) 
let IteratedAdjustArityOfLambda g amap topValInfo e =
    let tps,ctorThisValOpt,baseValOpt,vsl,body,bodyty = destTopLambda g amap topValInfo (e, tyOfExpr g e)
    let arities = topValInfo.AritiesOfArgs
    if arities.Length <> vsl.Length then 
        errorR(InternalError(sprintf "IteratedAdjustArityOfLambda, List.length arities = %d, List.length vsl = %d" (List.length arities) (List.length vsl), body.Range))
    let vsl,body = IteratedAdjustArityOfLambdaBody g arities vsl body
    tps,ctorThisValOpt,baseValOpt,vsl,body,bodyty


exception RequiredButNotSpecified of DisplayEnv * Tast.ModuleOrNamespaceRef * string * (StringBuilder -> unit) * range
exception ValueNotContained       of DisplayEnv * Tast.ModuleOrNamespaceRef * Val * Val * (string * string * string -> string)
exception ConstrNotContained      of DisplayEnv * UnionCase * UnionCase * (string * string -> string)
exception ExnconstrNotContained   of DisplayEnv * Tycon * Tycon * (string * string -> string)
exception FieldNotContained       of DisplayEnv * RecdField * RecdField * (string * string -> string)
exception InterfaceNotRevealed    of DisplayEnv * TType * range


/// Containment relation for module types
module SignatureConformance = begin

    // Use a type to capture the constant, common parameters 
    type Checker(g, amap, denv, remapInfo: SignatureRepackageInfo, checkingSig) = 

        // Build a remap that maps tcrefs in the signature to tcrefs in the implementation
        // Used when checking attributes.
        let sigToImplRemap = 
            let remap = Remap.Empty 
            let remap = (remapInfo.mrpiEntities,remap) ||> List.foldBack (fun (implTcref ,signTcref) acc -> addTyconRefRemap signTcref implTcref acc) 
            let remap = (remapInfo.mrpiVals    ,remap) ||> List.foldBack (fun (implValRef,signValRef) acc -> addValRemap signValRef.Deref implValRef.Deref acc) 
            remap
            
        // For all attributable elements (types, modules, exceptions, record fields, unions, parameters, generic type parameters)
        //
        // (a)	Start with lists AImpl and ASig containing the attributes in the implementation and signature, in declaration order
        // (b)	Each attribute in AImpl is checked against the available attributes in ASig. 
        //     a.	If an attribute is found in ASig which is an exact match (after evaluating attribute arguments), then the attribute in the implementation is ignored, the attribute is removed from ASig, and checking continues
        //     b.	If an attribute is found in ASig that has the same attribute type, then a warning is given and the attribute in the implementation is ignored 
        //     c.	Otherwise, the attribute in the implementation is kept
        // (c)	The attributes appearing in the compiled element are the compiled forms of the attributes from the signature and the kept attributes from the implementation
        let checkAttribs aenv (implAttribs:Attribs) (sigAttribs:Attribs) fixup =
            
            // Remap the signature attributes to make them look as if they were declared in 
            // the implementation. This allows us to compare them and propagate them to the implementation
            // if needed.
            let sigAttribs = sigAttribs |> List.map (remapAttrib g sigToImplRemap)

            // Helper to check for equality of evaluated attribute expressions
            let attribExprEq (AttribExpr(_,e1)) (AttribExpr(_,e2)) = EvaledAttribExprEquality g e1 e2

            // Helper to check for equality of evaluated named attribute arguments
            let attribNamedArgEq (AttribNamedArg(nm1,ty1,isProp1,e1)) (AttribNamedArg(nm2,ty2,isProp2,e2)) = 
                (nm1 = nm2) && 
                typeEquiv g ty1 ty2 && 
                (isProp1 = isProp2) && 
                attribExprEq e1 e2

            let attribsEq  attrib1 attrib2 = 
                let (Attrib(implTcref,_,implArgs,implNamedArgs,_,_implRange)) = attrib1
                let (Attrib(signTcref,_,signArgs,signNamedArgs,_,_signRange)) = attrib2
                tyconRefEq g signTcref implTcref &&
                (implArgs,signArgs) ||> List.lengthsEqAndForall2 attribExprEq &&
                (implNamedArgs, signNamedArgs) ||> List.lengthsEqAndForall2 attribNamedArgEq

            let attribsHaveSameTycon attrib1 attrib2 = 
                let (Attrib(implTcref,_,_,_,_,_)) = attrib1
                let (Attrib(signTcref,_,_,_,_,_)) = attrib2
                tyconRefEq g signTcref implTcref 

            // For each implementation attribute, only keep if it it is not mentioned in the signature.
            // Emit a warning if it is mentioned in the signature and the arguments to the attributes are 
            // not identical.
            let rec check keptImplAttribsRev implAttribs sigAttribs = 
                match implAttribs with 
                | [] -> keptImplAttribsRev |> List.rev
                | implAttrib :: remainingImplAttribs -> 

                    // Look for an attribute in the signature that matches precisely. If so, remove it 
                    let lookForMatchingAttrib =  sigAttribs |> List.tryRemove (attribsEq implAttrib)
                    match lookForMatchingAttrib with 
                    | Some (_, remainingSigAttribs) -> check keptImplAttribsRev remainingImplAttribs remainingSigAttribs    
                    | None ->

                    // Look for an attribute in the signature that has the same type. If so, give a warning
                    let existsSimilarAttrib = sigAttribs |> List.exists (attribsHaveSameTycon implAttrib)

                    if existsSimilarAttrib then 
                        let (Attrib(implTcref,_,_,_,_,implRange)) = implAttrib
                        warning(Error(FSComp.SR.tcAttribArgsDiffer(implTcref.DisplayName), implRange))
                        check keptImplAttribsRev remainingImplAttribs sigAttribs    
                    else
                        check (implAttrib :: keptImplAttribsRev) remainingImplAttribs sigAttribs    
                
            let keptImplAttribs = check [] implAttribs sigAttribs 

            fixup (sigAttribs @ keptImplAttribs)
            true

        let rec checkTypars m (aenv: TypeEquivEnv) (implTypars:Typars) (sigTypars:Typars) = 
            if implTypars.Length <> sigTypars.Length then 
                errorR (Error(FSComp.SR.typrelSigImplNotCompatibleParamCountsDiffer(),m)); 
                false
            else 
              let aenv = aenv.BindEquivTypars implTypars sigTypars 
              (implTypars,sigTypars) ||> List.forall2 (fun implTypar sigTypar -> 
                  let m = sigTypar.Range
                  if implTypar.StaticReq <> sigTypar.StaticReq then 
                      errorR (Error(FSComp.SR.typrelSigImplNotCompatibleCompileTimeRequirementsDiffer(), m));          
                  
                  // Adjust the actual type parameter name to look look like the signature
                  implTypar.SetIdent (mkSynId implTypar.Range sigTypar.Id.idText)     

                  // Mark it as "not compiler generated", now that we've got a good name for it
                  implTypar.SetCompilerGenerated false 

                  // Check the constraints in the implementation are present in the signature
                  implTypar.Constraints |> List.forall (fun implTyparCx -> 
                      match implTyparCx with 
                      // defaults can be dropped in the signature 
                      | TTyparDefaultsToType(_,_acty,_) -> true
                      | _ -> 
                          if not (List.exists  (typarConstraintsAEquiv g aenv implTyparCx) sigTypar.Constraints)
                          then (errorR(Error(FSComp.SR.typrelSigImplNotCompatibleConstraintsDiffer(sigTypar.Name, Layout.showL(NicePrint.constraintL denv (implTypar,implTyparCx))),m)); false)
                          else  true) &&

                  // Check the constraints in the signature are present in the implementation
                  sigTypar.Constraints |> List.forall (fun sigTyparCx -> 
                      match sigTyparCx with 
                      // defaults can be present in the signature and not in the implementation  because they are erased
                      | TTyparDefaultsToType(_,_acty,_) -> true
                      // 'comparison' and 'equality' constraints can be present in the signature and not in the implementation  because they are erased
                      | TTyparSupportsComparison _ -> true
                      | TTyparSupportsEquality _ -> true
                      | _ -> 
                          if not (List.exists  (fun implTyparCx -> typarConstraintsAEquiv g aenv implTyparCx sigTyparCx) implTypar.Constraints) then
                              (errorR(Error(FSComp.SR.typrelSigImplNotCompatibleConstraintsDifferRemove(sigTypar.Name, Layout.showL(NicePrint.constraintL denv (sigTypar,sigTyparCx))),m)); false)
                          else  
                              true) &&
                  (not checkingSig || checkAttribs aenv implTypar.Attribs sigTypar.Attribs (fun attribs -> implTypar.Data.typar_attribs <- attribs)))

        and checkTypeDef (aenv: TypeEquivEnv) (implTycon:Tycon) (sigTycon:Tycon) =
            let m = implTycon.Range
            let err f =  Error(f(implTycon.TypeOrMeasureKind.ToString()), m)
            if implTycon.LogicalName <> sigTycon.LogicalName then  (errorR (err (FSComp.SR.DefinitionsInSigAndImplNotCompatibleNamesDiffer)); false) else
            if implTycon.CompiledName <> sigTycon.CompiledName then  (errorR (err (FSComp.SR.DefinitionsInSigAndImplNotCompatibleNamesDiffer)); false) else
            checkExnInfo  (fun f -> ExnconstrNotContained(denv,implTycon,sigTycon,f)) aenv implTycon.ExceptionInfo sigTycon.ExceptionInfo &&
            let implTypars = implTycon.Typars m
            let sigTypars = sigTycon.Typars m
            if implTypars.Length <> sigTypars.Length then  
                errorR (err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleParameterCountsDiffer)); 
                false
            elif isLessAccessible implTycon.Accessibility sigTycon.Accessibility then 
                errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleAccessibilityDiffer)); 
                false
            else 
                let aenv = aenv.BindEquivTypars implTypars sigTypars 

                let aintfs = implTycon.InterfaceTypesOfFSharpTycon 
                let fintfs = sigTycon.InterfaceTypesOfFSharpTycon 
                let aintfsUser = implTycon.TypeContents.tcaug_interfaces |> List.filter (fun (_,compgen,_) -> not compgen) |> List.map p13 
                let flatten tys = 
                   tys 
                   |> List.collect (AllSuperTypesOfType g amap m AllowMultiIntfInst) 
                   |> ListSet.setify (typeEquiv g) 
                   |> List.filter (isInterfaceTy g)
                let aintfs     = flatten aintfs 
                let aintfsUser = flatten aintfsUser 
                let fintfs     = flatten fintfs 
              
                let unimpl = ListSet.subtract (fun fity aity -> typeAEquiv g aenv aity fity) fintfs aintfs
                (unimpl |> List.forall (fun ity -> errorR (err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleMissingInterface(x, NicePrint.prettyStringOfTy denv ity))); false)) &&
                let hidden = ListSet.subtract (typeAEquiv g aenv) aintfsUser fintfs
                hidden |> List.iter (fun ity -> (if implTycon.IsFSharpInterfaceTycon then error else warning) (InterfaceNotRevealed(denv,ity,implTycon.Range)));

                let aNull = IsUnionTypeWithNullAsTrueValue g implTycon
                let fNull = IsUnionTypeWithNullAsTrueValue g sigTycon
                if aNull && not fNull then 
                  errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplementationSaysNull))
                elif fNull && not aNull then 
                  errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleSignatureSaysNull));

                let aNull2 = TypeNullIsExtraValue g (generalizedTyconRef (mkLocalTyconRef implTycon))
                let fNull2 = TypeNullIsExtraValue g (generalizedTyconRef (mkLocalTyconRef implTycon))
                if aNull2 && not fNull2 then 
                    errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplementationSaysNull2))
                elif fNull2 && not aNull2 then 
                    errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleSignatureSaysNull2));

                let aSealed = isSealedTy g (generalizedTyconRef (mkLocalTyconRef implTycon))
                let fSealed = isSealedTy g (generalizedTyconRef (mkLocalTyconRef sigTycon))
                if  aSealed && not fSealed  then 
                    errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplementationSealed));
                if  not aSealed && fSealed  then 
                    errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplementationIsNotSealed));

                let aPartial = isAbstractTycon implTycon
                let fPartial = isAbstractTycon sigTycon
                if aPartial && not fPartial then 
                    errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplementationIsAbstract));

                if not aPartial && fPartial then 
                    errorR(err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleSignatureIsAbstract));

                if not (typeAEquiv g aenv (superOfTycon g implTycon) (superOfTycon g sigTycon)) then 
                    errorR (err(FSComp.SR.DefinitionsInSigAndImplNotCompatibleTypesHaveDifferentBaseTypes));

                checkTypars m aenv implTypars sigTypars &&
                checkTypeRepr err aenv implTycon.TypeReprInfo sigTycon.TypeReprInfo &&
                checkTypeAbbrev err aenv implTycon.TypeOrMeasureKind sigTycon.TypeOrMeasureKind implTycon.TypeAbbrev sigTycon.TypeAbbrev  &&
                checkAttribs aenv implTycon.Attribs sigTycon.Attribs (fun attribs -> implTycon.Data.entity_attribs <- attribs)
            
        and checkValInfo aenv err (implVal : Val) (sigVal : Val) = 
            let id = implVal.Id
            match implVal.ValReprInfo, sigVal.ValReprInfo with 
            | _,None -> true
            | None, Some _ -> err(FSComp.SR.ValueNotContainedMutabilityArityNotInferred)
            | Some (ValReprInfo (implTyparNames,implArgInfos,implRetInfo) as implValInfo), Some (ValReprInfo (sigTyparNames,sigArgInfos,sigRetInfo) as sigValInfo) ->
                let ntps = implTyparNames.Length
                let mtps = sigTyparNames.Length
                if ntps <> mtps then
                  err(fun(x, y, z) -> FSComp.SR.ValueNotContainedMutabilityGenericParametersDiffer(x, y, z, string mtps, string ntps))
                elif implValInfo.KindsOfTypars <> sigValInfo.KindsOfTypars then
                  err(FSComp.SR.ValueNotContainedMutabilityGenericParametersAreDifferentKinds)
                elif not (sigArgInfos.Length <= implArgInfos.Length && List.forall2 (fun x y -> List.length x <= List.length y) sigArgInfos (fst (List.chop sigArgInfos.Length implArgInfos))) then 
                  err(fun(x, y, z) -> FSComp.SR.ValueNotContainedMutabilityAritiesDiffer(x, y, z, id.idText, string sigArgInfos.Length, id.idText, id.idText))
                else 
                  let implArgInfos = implArgInfos |> List.take sigArgInfos.Length  
                  let implArgInfos = (implArgInfos, sigArgInfos) ||> List.map2 (fun l1 l2 -> l1 |> List.take l2.Length)
                  // Propagate some information signature to implementation. 

                  // Check the attributes on each argument, and update the ValReprInfo for
                  // the value to reflect the information in the the signature.
                  // This ensures that the compiled form of the value matches the signature rather than 
                  // the implementation. This also propagates argument names from signature to implementation
                  let res = 
                      (implArgInfos,sigArgInfos) ||> List.forall2 (List.forall2 (fun implArgInfo sigArgInfo -> 
                          checkAttribs aenv implArgInfo.Attribs sigArgInfo.Attribs (fun attribs -> 
                              implArgInfo.Name <- sigArgInfo.Name
                              implArgInfo.Attribs <- attribs))) && 

                      checkAttribs aenv implRetInfo.Attribs sigRetInfo.Attribs (fun attribs -> 
                          implRetInfo.Name <- sigRetInfo.Name
                          implRetInfo.Attribs <- attribs)
                  
                  implVal.SetValReprInfo (Some (ValReprInfo (sigTyparNames,implArgInfos,implRetInfo)))
                  res

        and checkVal implModRef (aenv:TypeEquivEnv) (implVal:Val) (sigVal:Val) =

            // Propagate defn location information from implementation to signature . 
            sigVal.SetDefnRange implVal.DefinitionRange

            if verbose then  dprintf "checking value %s, %d, %d\n" implVal.DisplayName implVal.Stamp sigVal.Stamp;
            let mk_err denv f = ValueNotContained(denv,implModRef,implVal,sigVal,f)
            let err denv f = errorR(mk_err denv f); false
            let m = implVal.Range
            if implVal.IsMutable <> sigVal.IsMutable then (err denv FSComp.SR.ValueNotContainedMutabilityAttributesDiffer)
            elif implVal.LogicalName <> sigVal.LogicalName then (err denv FSComp.SR.ValueNotContainedMutabilityNamesDiffer)
            elif implVal.CompiledName <> sigVal.CompiledName then (err denv FSComp.SR.ValueNotContainedMutabilityCompiledNamesDiffer)
            elif implVal.DisplayName <> sigVal.DisplayName then (err denv FSComp.SR.ValueNotContainedMutabilityDisplayNamesDiffer)
            elif isLessAccessible implVal.Accessibility sigVal.Accessibility then (err denv FSComp.SR.ValueNotContainedMutabilityAccessibilityMore)
            elif implVal.MustInline <> sigVal.MustInline then (err denv FSComp.SR.ValueNotContainedMutabilityInlineFlagsDiffer)
            elif implVal.LiteralValue <> sigVal.LiteralValue then (err denv FSComp.SR.ValueNotContainedMutabilityLiteralConstantValuesDiffer)
            elif implVal.IsTypeFunction <> sigVal.IsTypeFunction then (err denv FSComp.SR.ValueNotContainedMutabilityOneIsTypeFunction)
            else 
                let implTypars,atau = implVal.TypeScheme
                let sigTypars,ftau = sigVal.TypeScheme
                if implTypars.Length <> sigTypars.Length then (err {denv with showTyparBinding=true} FSComp.SR.ValueNotContainedMutabilityParameterCountsDiffer) else
                let aenv = aenv.BindEquivTypars implTypars sigTypars 
                checkTypars m aenv implTypars sigTypars &&
                if not (typeAEquiv g aenv atau ftau) then err denv (FSComp.SR.ValueNotContainedMutabilityTypesDiffer)
                elif not (checkValInfo aenv (err denv) implVal sigVal) then false
                elif not (implVal.IsExtensionMember = sigVal.IsExtensionMember) then err denv (FSComp.SR.ValueNotContainedMutabilityExtensionsDiffer)
                elif not (checkMemberDatasConform (err denv) (implVal.Attribs, implVal,implVal.MemberInfo) (sigVal.Attribs,sigVal,sigVal.MemberInfo)) then false
                else checkAttribs aenv implVal.Attribs sigVal.Attribs (fun attribs -> implVal.Data.val_attribs <- attribs)              


        and checkExnInfo err aenv implTypeRepr sigTypeRepr =
            match implTypeRepr,sigTypeRepr with 
            | TExnAsmRepr _, TExnFresh _ -> 
                (errorR (err FSComp.SR.ExceptionDefsNotCompatibleHiddenBySignature); false)
            | TExnAsmRepr tcr1, TExnAsmRepr tcr2  -> 
                if tcr1 <> tcr2 then  (errorR (err FSComp.SR.ExceptionDefsNotCompatibleDotNetRepresentationsDiffer); false) else true
            | TExnAbbrevRepr _, TExnFresh _ -> 
                (errorR (err FSComp.SR.ExceptionDefsNotCompatibleAbbreviationHiddenBySignature); false)
            | TExnAbbrevRepr ecr1, TExnAbbrevRepr ecr2 -> 
                if not (tcrefAEquiv g aenv ecr1 ecr2) then 
                  (errorR (err FSComp.SR.ExceptionDefsNotCompatibleSignaturesDiffer); false)
                else true
            | TExnFresh r1, TExnFresh  r2-> checkRecordFieldsForExn g denv err aenv r1 r2
            | TExnNone,TExnNone -> true
            | _ -> 
                (errorR (err FSComp.SR.ExceptionDefsNotCompatibleExceptionDeclarationsDiffer); false)

        and checkUnionCase aenv implUnionCase sigUnionCase =
            let err f = errorR(ConstrNotContained(denv,implUnionCase,sigUnionCase,f));false
            if implUnionCase.Id.idText <> sigUnionCase.Id.idText then  err FSComp.SR.ModuleContainsConstructorButNamesDiffer
            elif implUnionCase.RecdFields.Length <> sigUnionCase.RecdFields.Length then err FSComp.SR.ModuleContainsConstructorButDataFieldsDiffer
            elif not (List.forall2 (checkField aenv) implUnionCase.RecdFields sigUnionCase.RecdFields) then err FSComp.SR.ModuleContainsConstructorButTypesOfFieldsDiffer
            elif isLessAccessible implUnionCase.Accessibility sigUnionCase.Accessibility then err FSComp.SR.ModuleContainsConstructorButAccessibilityDiffers
            else checkAttribs aenv implUnionCase.Attribs sigUnionCase.Attribs (fun attribs -> implUnionCase.Attribs <- attribs)

        and checkField aenv implField sigField =
            let err f = errorR(FieldNotContained(denv,implField,sigField,f)); false
            if implField.rfield_id.idText <> sigField.rfield_id.idText then err FSComp.SR.FieldNotContainedNamesDiffer
            elif isLessAccessible implField.Accessibility sigField.Accessibility then err FSComp.SR.FieldNotContainedAccessibilitiesDiffer
            elif implField.IsStatic <> sigField.IsStatic then err FSComp.SR.FieldNotContainedStaticsDiffer
            elif implField.IsMutable <> sigField.IsMutable then err FSComp.SR.FieldNotContainedMutablesDiffer
            elif implField.LiteralValue <> sigField.LiteralValue then err FSComp.SR.FieldNotContainedLiteralsDiffer
            elif not (typeAEquiv g aenv implField.FormalType sigField.FormalType) then err FSComp.SR.FieldNotContainedTypesDiffer
            else 
                checkAttribs aenv implField.FieldAttribs sigField.FieldAttribs (fun attribs -> implField.rfield_fattribs <- attribs) &&
                checkAttribs aenv implField.PropertyAttribs sigField.PropertyAttribs (fun attribs -> implField.rfield_pattribs <- attribs)
            
        and checkMemberDatasConform err  (implAttrs,implVal,implMemberInfo) (sigAttrs, sigVal,sigMemberInfo)  =
            match implMemberInfo,sigMemberInfo with 
            | None,None -> true
            | Some implMembInfo, Some sigMembInfo -> 
                if not (implVal.CompiledName = sigVal.CompiledName) then 
                  err(FSComp.SR.ValueNotContainedMutabilityDotNetNamesDiffer)
                elif not (implMembInfo.MemberFlags.IsInstance = sigMembInfo.MemberFlags.IsInstance) then 
                  err(FSComp.SR.ValueNotContainedMutabilityStaticsDiffer)
                elif false then 
                  err(FSComp.SR.ValueNotContainedMutabilityVirtualsDiffer)
                elif not (implMembInfo.MemberFlags.IsDispatchSlot = sigMembInfo.MemberFlags.IsDispatchSlot) then 
                  err(FSComp.SR.ValueNotContainedMutabilityAbstractsDiffer)
               // The final check is an implication:
               //     classes have non-final CompareTo/Hash methods 
               //     abstract have non-final CompareTo/Hash methods 
               //     records  have final CompareTo/Hash methods 
               //     unions  have final CompareTo/Hash methods 
               // This is an example where it is OK for the signaure to say 'non-final' when the implementation says 'final' 
                elif not implMembInfo.MemberFlags.IsFinal && sigMembInfo.MemberFlags.IsFinal then 
                  err(FSComp.SR.ValueNotContainedMutabilityFinalsDiffer)
                elif not (implMembInfo.MemberFlags.IsOverrideOrExplicitImpl = sigMembInfo.MemberFlags.IsOverrideOrExplicitImpl) then 
                  err(FSComp.SR.ValueNotContainedMutabilityOverridesDiffer)
                elif not (implMembInfo.MemberFlags.MemberKind = sigMembInfo.MemberFlags.MemberKind) then 
                  err(FSComp.SR.ValueNotContainedMutabilityOneIsConstructor)
                else  
                   let finstance = ValSpecIsCompiledAsInstance g sigVal
                   let ainstance = ValSpecIsCompiledAsInstance g implVal
                   if  finstance && not ainstance then 
                      err(FSComp.SR.ValueNotContainedMutabilityStaticButInstance)
                   elif not finstance && ainstance then 
                      err(FSComp.SR.ValueNotContainedMutabilityInstanceButStatic)
                   else true

            | _ -> false

        // -------------------------------------------------------------------------------
        // WARNING!!!!
        // checkRecordFields and checkRecordFieldsForExn are the EXACT SAME FUNCTION.
        // The only difference is the signature for err - this is because err is a function
        // that reports errors, and checkRecordFields is called with a different
        // sig for err then checkRecordFieldsForExn.
        // -------------------------------------------------------------------------------

        and checkRecordFields g amap denv err aenv (implFields:TyconRecdFields) (sigFields:TyconRecdFields) =
            let implFields = implFields.TrueFieldsAsList
            let sigFields = sigFields.TrueFieldsAsList
            let m1 = implFields |> NameMap.ofKeyedList (fun rfld -> rfld.Name)
            let m2 = sigFields |> NameMap.ofKeyedList (fun rfld -> rfld.Name)
            NameMap.suball2 (fun s _ -> errorR(err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleFieldRequiredButNotSpecified(x, s))); false) (checkField aenv)  m1 m2 &&
            NameMap.suball2 (fun s _ -> errorR(err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleFieldWasPresent(x, s))); false) (fun x y -> checkField aenv y x)  m2 m1 &&
            // This check is required because constructors etc. are externally visible 
            // and thus compiled representations do pick up dependencies on the field order  
            (if List.forall2 (checkField aenv)  implFields sigFields
             then true
             else (errorR(err (FSComp.SR.DefinitionsInSigAndImplNotCompatibleFieldOrderDiffer)); false))

        and checkRecordFieldsForExn g denv err aenv (implFields:TyconRecdFields) (sigFields:TyconRecdFields) =
            let implFields = implFields.TrueFieldsAsList
            let sigFields = sigFields.TrueFieldsAsList
            let m1 = implFields |> NameMap.ofKeyedList (fun rfld -> rfld.Name)
            let m2 = sigFields |> NameMap.ofKeyedList (fun rfld -> rfld.Name)
            NameMap.suball2 (fun s _ -> errorR(err (fun (x, y) -> FSComp.SR.ExceptionDefsNotCompatibleFieldInSigButNotImpl(s, x, y))); false) (checkField aenv)  m1 m2 &&
            NameMap.suball2 (fun s _ -> errorR(err (fun (x, y) -> FSComp.SR.ExceptionDefsNotCompatibleFieldInImplButNotSig(s, x, y))); false) (fun x y -> checkField aenv y x)  m2 m1 &&
            // This check is required because constructors etc. are externally visible 
            // and thus compiled representations do pick up dependencies on the field order  
            (if List.forall2 (checkField aenv)  implFields sigFields
             then true
             else (errorR(err (FSComp.SR.ExceptionDefsNotCompatibleFieldOrderDiffers)); false))

        and checkVirtualSlots g denv err aenv implAbstractSlots sigAbstractSlots =
            let m1 = NameMap.ofKeyedList (fun (v:ValRef) -> v.DisplayName) implAbstractSlots
            let m2 = NameMap.ofKeyedList (fun (v:ValRef) -> v.DisplayName) sigAbstractSlots
            (m1,m2) ||> NameMap.suball2 (fun _s vref -> errorR(err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInImpl(x, Layout.showL(NicePrint.valL denv vref.Deref)))); false) (fun _x _y -> true)  &&
            (m2,m1) ||> NameMap.suball2 (fun _s vref -> errorR(err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleAbstractMemberMissingInSig(x, Layout.showL(NicePrint.valL denv vref.Deref)))); false) (fun _x _y -> true)  

        and checkClassFields isStruct g amap denv err aenv (implFields:TyconRecdFields) (sigFields:TyconRecdFields) =
            let implFields = implFields.TrueFieldsAsList
            let sigFields = sigFields.TrueFieldsAsList
            let m1 = implFields |> NameMap.ofKeyedList (fun rfld -> rfld.Name) 
            let m2 = sigFields |> NameMap.ofKeyedList (fun rfld -> rfld.Name) 
            NameMap.suball2 (fun s _ -> errorR(err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleFieldRequiredButNotSpecified(x, s))); false) (checkField aenv)  m1 m2 &&
            (if isStruct then 
                NameMap.suball2 (fun s _ -> warning(err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleFieldIsInImplButNotSig(x, s))); false) (fun x y -> checkField aenv y x)  m2 m1 
             else
                true)
            

        and checkTypeRepr err aenv implTypeRepr sigTypeRepr =
            let reportNiceError k s1 s2 = 
              let aset = NameSet.ofList s1
              let fset = NameSet.ofList s2
              match Zset.elements (Zset.diff aset fset) with 
              | [] -> 
                  match Zset.elements (Zset.diff fset aset) with             
                  | [] -> (errorR (err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleNumbersDiffer(x, k))); false)
                  | l -> (errorR (err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleSignatureDefinesButImplDoesNot(x, k, String.concat ";" l))); false)
              | l -> (errorR (err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplDefinesButSignatureDoesNot(x, k, String.concat ";" l))); false)

            match implTypeRepr,sigTypeRepr with 
            | Some (TRecdRepr _ | TFiniteUnionRepr _ | TILObjModelRepr _ ), None  -> true
            | Some (TFsObjModelRepr r), None  -> 
                match r.fsobjmodel_kind with 
                | TTyconStruct | TTyconEnum -> 
                   (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleImplDefinesStruct); false)
                | _ -> 
                   true
            | Some (TAsmRepr _), None -> 
                (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleDotNetTypeRepresentationIsHidden); false)
            | Some (TMeasureableRepr _), None -> 
                (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleTypeIsHidden); false)
            | Some (TFiniteUnionRepr r1), Some (TFiniteUnionRepr r2) -> 
                let ucases1 = r1.UnionCasesAsList
                let ucases2 = r2.UnionCasesAsList
                if ucases1.Length <> ucases2.Length then
                  let names l = List.map (fun c -> c.Id.idText) l
                  reportNiceError "union case" (names ucases1) (names ucases2) 
                else List.forall2 (checkUnionCase aenv) ucases1 ucases2
            | Some (TRecdRepr implFields), Some (TRecdRepr sigFields) -> 
                checkRecordFields g amap denv err aenv implFields sigFields
            | Some (TFsObjModelRepr r1), Some (TFsObjModelRepr r2) -> 
                if not (match r1.fsobjmodel_kind,r2.fsobjmodel_kind with 
                         | TTyconClass,TTyconClass -> true
                         | TTyconInterface,TTyconInterface -> true
                         | TTyconStruct,TTyconStruct -> true
                         | TTyconEnum, TTyconEnum -> true
                         | TTyconDelegate (TSlotSig(_,typ1,ctps1,mtps1,ps1, rty1)), 
                           TTyconDelegate (TSlotSig(_,typ2,ctps2,mtps2,ps2, rty2)) -> 
                             (typeAEquiv g aenv typ1 typ2) &&
                             (ctps1.Length = ctps2.Length) &&
                             (let aenv = aenv.BindEquivTypars ctps1 ctps2 
                              (typarsAEquiv g aenv ctps1 ctps2) &&
                              (mtps1.Length = mtps2.Length) &&
                              (let aenv = aenv.BindEquivTypars mtps1 mtps2 
                               (typarsAEquiv g aenv mtps1 mtps2) &&
                               ((ps1,ps2) ||> List.lengthsEqAndForall2 (List.lengthsEqAndForall2 (fun p1 p2 -> typeAEquiv g aenv p1.Type p2.Type))) &&
                               (returnTypesAEquiv g aenv rty1 rty2)))
                         | _,_ -> false) then 
                  (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleTypeIsDifferentKind); false)
                else 
                  let isStruct = (match r1.fsobjmodel_kind with TTyconStruct -> true | _ -> false)
                  checkClassFields isStruct g amap denv err aenv r1.fsobjmodel_rfields r2.fsobjmodel_rfields &&
                  checkVirtualSlots g denv err aenv r1.fsobjmodel_vslots r2.fsobjmodel_vslots
            | Some (TAsmRepr tcr1),  Some (TAsmRepr tcr2) -> 
                if tcr1 <> tcr2 then  (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleILDiffer); false) else true
            | Some (TMeasureableRepr ty1),  Some (TMeasureableRepr ty2) -> 
                if typeAEquiv g aenv ty1 ty2 then true else (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleRepresentationsDiffer); false)
            | Some _, Some _ -> (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleRepresentationsDiffer); false)
            | None, Some _ -> (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleRepresentationsDiffer); false)
            | None, None -> true

        and checkTypeAbbrev err aenv kind1 kind2 implTypeAbbrev sigTypeAbbrev =
            if kind1 <> kind2 then (errorR (err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleSignatureDeclaresDiffer(x, kind2.ToString(), kind1.ToString()))); false)
            else
              match implTypeAbbrev,sigTypeAbbrev with 
              | Some ty1, Some ty2 -> if not (typeAEquiv g aenv ty1 ty2) then (errorR (err (fun x -> FSComp.SR.DefinitionsInSigAndImplNotCompatibleAbbreviationsDiffer(x, Layout.showL (typeL ty1), Layout.showL (typeL ty2)))); false) else true
              | None,None -> true
              | Some _, None -> (errorR (err (FSComp.SR.DefinitionsInSigAndImplNotCompatibleAbbreviationHiddenBySig)); false)
              | None, Some _ -> (errorR (err FSComp.SR.DefinitionsInSigAndImplNotCompatibleSigHasAbbreviation); false)

        and checkModuleOrNamespaceContents m aenv (implModRef:ModuleOrNamespaceRef) (signModType:ModuleOrNamespaceType) = 
            let implModType = implModRef.ModuleOrNamespaceType
            (if implModType.ModuleOrNamespaceKind <> signModType.ModuleOrNamespaceKind then errorR(Error(FSComp.SR.typrelModuleNamespaceAttributesDifferInSigAndImpl(),m)));


            (implModType.TypesByMangledName , signModType.TypesByMangledName)
             ||> NameMap.suball2 
                (fun s _fx -> errorR(RequiredButNotSpecified(denv,implModRef,"type",(fun os -> Printf.bprintf os "%s" s),m)); false) 
                (checkTypeDef aenv)  &&


            (implModType.ModulesAndNamespacesByDemangledName, signModType.ModulesAndNamespacesByDemangledName ) 
              ||> NameMap.suball2 
                   (fun s fx -> errorR(RequiredButNotSpecified(denv,implModRef,(if fx.IsModule then "module" else "namespace"),(fun os -> Printf.bprintf os "%s" s),m)); false) 
                   (fun x1 x2 -> checkModuleOrNamespace aenv (mkLocalModRef x1) x2)  &&

            let sigValHadNoMatchingImplementation (fx:Val) (_closeActualVal: Val option) = 
                errorR(RequiredButNotSpecified(denv,implModRef,"value",(fun os -> 
                   (* In the case of missing members show the full required enclosing type and signature *)
                   if fx.IsMember then 
                       NicePrint.outputQualifiedValSpec denv os fx
                   else
                       Printf.bprintf os "%s" fx.DisplayName),m))
            (implModType.AllValsAndMembersByLogicalNameUncached, signModType.AllValsAndMembersByLogicalNameUncached)
              ||> NameMap.suball2 
                    (fun _s (fxs:Val list) -> sigValHadNoMatchingImplementation fxs.Head None; false)
                    (fun avs fvs -> 
                        match avs,fvs with 
                        | [],_ | _,[] -> failwith "unreachable"
                        | [av],[fv] -> 
                            checkVal implModRef aenv av fv
                        | _ -> 
                             // for each formal requirement, try to find a precisely matching actual requirement
                             let matchingPairs = 
                                 fvs |> List.choose (fun fv -> 
                                     match avs |> List.tryFind (fun av -> 
                                                         let res = valLinkageAEquiv g aenv av fv
                                                         //if res then printfn "%s" (bufs (fun buf -> Printf.bprintf buf "YES MATCH: fv '%a', av '%a'" (NicePrint.outputQualifiedValSpec denv) fv (NicePrint.outputQualifiedValSpec denv) av))
                                                         //else printfn "%s" (bufs (fun buf -> Printf.bprintf buf "NO MATCH: fv '%a', av '%a'" (NicePrint.outputQualifiedValSpec denv) fv (NicePrint.outputQualifiedValSpec denv) av))  
                                                         res) with 
                                      | None -> None
                                      | Some av -> Some(fv,av))
                             
                             // Check the ones with matching linkage
                             let allPairsOk = matchingPairs |> List.map (fun (fv,av) -> checkVal implModRef aenv av fv) |> List.forall id
                             let someNotOk = matchingPairs.Length < fvs.Length
                             // Report an error for those that don't. Try pairing up by enclosing-type/name
                             if someNotOk then 
                                 let noMatches,partialMatchingPairs = 
                                     fvs |> List.splitChoose (fun fv -> 
                                         match avs |> List.tryFind (fun av -> 
                                                             (av.LinkagePartialKey.MemberParentMangledName = fv.LinkagePartialKey.MemberParentMangledName) &&
                                                             (av.LinkagePartialKey.LogicalName = fv.LinkagePartialKey.LogicalName) &&
                                                             (av.LinkagePartialKey.TotalArgCount = fv.LinkagePartialKey.TotalArgCount)) with 
                                          | None -> Choice1Of2 fv
                                          | Some av -> Choice2Of2(fv,av))
                                 for (fv,av) in partialMatchingPairs do
                                     checkVal implModRef aenv av fv |> ignore
                                 for fv in noMatches do 
                                     sigValHadNoMatchingImplementation fv None
                             allPairsOk && not someNotOk)


        and checkModuleOrNamespace aenv implModRef sigModRef = 
            checkModuleOrNamespaceContents implModRef.Range aenv implModRef sigModRef.ModuleOrNamespaceType &&
            checkAttribs aenv implModRef.Attribs sigModRef.Attribs implModRef.Deref.SetAttribs

        member __.CheckSignature aenv (implModRef:ModuleOrNamespaceRef) (signModType:ModuleOrNamespaceType) = 
            checkModuleOrNamespaceContents implModRef.Range aenv implModRef signModType

        member __.CheckTypars m aenv (implTypars: Typars) (signTypars: Typars) = 
            checkTypars m aenv implTypars signTypars


    /// Check the names add up between a signature and its implementation. We check this first.
    let rec CheckNamesOfModuleOrNamespaceContents denv (implModRef:ModuleOrNamespaceRef) (signModType:ModuleOrNamespaceType) = 
        let m = implModRef.Range 
        let implModType = implModRef.ModuleOrNamespaceType
        NameMap.suball2 
            (fun s _fx -> errorR(RequiredButNotSpecified(denv,implModRef,"type",(fun os -> Printf.bprintf os "%s" s),m)); false) 
            (fun _ _ -> true)  
            implModType.TypesByMangledName 
            signModType.TypesByMangledName &&

        (implModType.ModulesAndNamespacesByDemangledName, signModType.ModulesAndNamespacesByDemangledName ) 
          ||> NameMap.suball2 
                (fun s fx -> errorR(RequiredButNotSpecified(denv,implModRef,(if fx.IsModule then "module" else "namespace"),(fun os -> Printf.bprintf os "%s" s),m)); false) 
                (fun x1 (x2:ModuleOrNamespace) -> CheckNamesOfModuleOrNamespace denv (mkLocalModRef x1) x2.ModuleOrNamespaceType)  &&

        (implModType.AllValsAndMembersByLogicalNameUncached , signModType.AllValsAndMembersByLogicalNameUncached) 
          ||> NameMap.suball2 
                (fun _s (fxs:Val list) -> 
                    let fx = fxs.Head
                    errorR(RequiredButNotSpecified(denv,implModRef,"value",(fun os -> 
                       // In the case of missing members show the full required enclosing type and signature 
                       if isSome fx.MemberInfo then 
                           NicePrint.outputQualifiedValSpec denv os fx
                       else
                           Printf.bprintf os "%s" fx.DisplayName),m)); false)
                (fun _ _ -> true) 


    and CheckNamesOfModuleOrNamespace denv (implModRef:ModuleOrNamespaceRef) signModType = 
        CheckNamesOfModuleOrNamespaceContents denv implModRef signModType

end

//-------------------------------------------------------------------------
// Completeness of classes
//------------------------------------------------------------------------- 

type OverrideCanImplement = 
    | CanImplementAnyInterfaceSlot
    | CanImplementAnyClassHierarchySlot
    | CanImplementAnySlot
    | CanImplementNoSlots
    
type OverrideInfo = 
    | Override of OverrideCanImplement * TyconRef * Ident * (Typars * TyparInst) * (*argTypes:*)TType list list * (*Type:*)TType option * (*isFakeEventProperty:*)bool
    member x.IsFakeEventProperty = let (Override(_,_,_,_,_,_,b)) = x in b
    member x.LogicalName = let (Override(_,_,id,_,_,_,_)) = x in id.idText
    member x.Range = let (Override(_,_,id,_,_,_,_)) = x in id.idRange
    member x.BoundingTyconRef = let (Override(_,ty,_,_,_,_,_)) = x in ty

// If the bool is true then the slot is optional, i.e. is an interface slot
// which does not _have_ to be implemented, because an inherited implementation 
// is available.
type RequiredSlot = RequiredSlot of MethInfo * (* isOptional: *) bool 

type SlotImplSet = SlotImplSet of RequiredSlot list * NameMultiMap<RequiredSlot> * OverrideInfo list * PropInfo list

exception TypeIsImplicitlyAbstract of range
exception OverrideDoesntOverride of DisplayEnv * OverrideInfo * MethInfo option * TcGlobals * Import.ImportMap * range



module DispatchSlotChecking =
    /// The overall information about a method implementation in a class or obeject expression 

    let PrintOverrideToBuffer denv os (Override(_,_,id,(mtps,memberToParentInst),argTys,retTy,_)) = 
       let denv = { denv with showTyparBinding = true }
       let retTy = (retTy  |> GetFSharpViewOfReturnType denv.g)
       let argInfos = 
           match argTys with 
           | [] -> [[(denv.g.unit_ty,ValReprInfo.unnamedTopArg1)]]
           | _ -> argTys |> List.mapSquared (fun ty -> (ty, ValReprInfo.unnamedTopArg1)) 
       Layout.bufferL os (NicePrint.memberSigL denv (memberToParentInst,id.idText,mtps, argInfos, retTy))

    let PrintMethInfoSigToBuffer g amap m denv os minfo =
        let denv = { denv with showTyparBinding = true }
        let argTys,retTy,fmtps,ttpinst = CompiledSigOfMeth g amap m minfo
        let retTy = (retTy  |> GetFSharpViewOfReturnType g)
        let argInfos = argTys |> List.mapSquared (fun ty -> (ty, ValReprInfo.unnamedTopArg1))
        let nm = minfo.LogicalName
        Layout.bufferL os (NicePrint.memberSigL denv (ttpinst,nm,fmtps, argInfos, retTy))

    let FormatOverride denv d = bufs (fun buf -> PrintOverrideToBuffer denv buf d)
    let FormatMethInfoSig g amap m denv d = bufs (fun buf -> PrintMethInfoSigToBuffer g amap m denv buf d)

    let GetInheritedMemberOverrideInfo g amap m parentType (minfo:MethInfo) = 
        let nm = minfo.LogicalName
        let argTys,retTy,fmtps,ttpinst = CompiledSigOfMeth g amap m minfo

        let isFakeEventProperty = minfo.IsFSharpEventProperty
        Override(parentType,tcrefOfAppTy g minfo.EnclosingType,mkSynId m nm, (fmtps,ttpinst),argTys,retTy,isFakeEventProperty)

    let GetTypeMemberOverrideInfo g reqdTy (overrideBy:ValRef) = 
        let _,argInfos,retTy,_ = GetTypeOfMemberInMemberForm g overrideBy
        let nm = overrideBy.LogicalName

        let argTys = argInfos |> List.mapSquared fst
        
        let memberMethodTypars,memberToParentInst,argTys,retTy = 
            match PartitionValRefTypars g overrideBy with
            | Some(_,_,memberMethodTypars,memberToParentInst,_tinst) -> 
                let argTys = argTys |> List.mapSquared (instType memberToParentInst) 
                let retTy = retTy |> Option.map (instType memberToParentInst) 
                memberMethodTypars, memberToParentInst,argTys, retTy
            | None -> 
                error(Error(FSComp.SR.typrelMethodIsOverconstrained(),overrideBy.Range))
        let implKind = 
            if ValRefIsExplicitImpl g overrideBy then 
                
                let belongsToReqdTy = 
                    match overrideBy.MemberInfo.Value.ImplementedSlotSigs with
                    | [] -> false
                    | ss :: _ -> isInterfaceTy g ss.ImplementedType && typeEquiv g reqdTy ss.ImplementedType
                if belongsToReqdTy then 
                    CanImplementAnyInterfaceSlot
                else
                    CanImplementNoSlots
            else if MemberRefIsDispatchSlot overrideBy then 
                CanImplementNoSlots
                // abstract slots can only implement interface slots
                //CanImplementAnyInterfaceSlot  <<----- Change to this to enable implicit interface implementation
            
            else 
                CanImplementAnyClassHierarchySlot
                //CanImplementAnySlot  <<----- Change to this to enable implicit interface implementation

        let isFakeEventProperty = overrideBy.IsFSharpEventProperty(g)
        Override(implKind,overrideBy.MemberApparentParent, mkSynId overrideBy.Range nm, (memberMethodTypars,memberToParentInst),argTys,retTy,isFakeEventProperty)

    let GetObjectExprOverrideInfo g amap (implty, id:Ident, memberFlags, ty, arityInfo, bindingAttribs, rhsExpr) = 
        // Dissect the type
        let tps, argInfos, retTy, _ = GetMemberTypeInMemberForm g memberFlags arityInfo ty id.idRange
        let argTys = argInfos |> List.mapSquared fst
        // Dissect the implementation
        let _, ctorThisValOpt, baseValOpt, vsl, rhsExpr,_ = destTopLambda g amap arityInfo (rhsExpr,ty)
        assert ctorThisValOpt.IsNone

        // Drop 'this'
        match vsl with 
        | [thisv]::vs -> 
            // Check for empty variable list from a () arg
            let vs = if vs.Length = 1 && argInfos.IsEmpty then [] else vs
            let implKind = 
                if isInterfaceTy g implty then 
                    CanImplementAnyInterfaceSlot 
                else 
                    CanImplementAnyClassHierarchySlot
                    //CanImplementAnySlot  <<----- Change to this to enable implicit interface implementation
            let isFakeEventProperty = CompileAsEvent g bindingAttribs
            let overrideByInfo = Override(implKind, tcrefOfAppTy g implty, id, (tps,[]), argTys, retTy, isFakeEventProperty)
            overrideByInfo, (baseValOpt, thisv, vs, bindingAttribs, rhsExpr)
        | _ -> 
            error(InternalError("Unexpected shape for object expression override",id.idRange))
          
    let IsNameMatch (dispatchSlot:MethInfo) (overrideBy: OverrideInfo) = 
        (overrideBy.LogicalName = dispatchSlot.LogicalName)
          
    let IsImplMatch g (dispatchSlot:MethInfo) (Override(implKind,_,_,_,_,_,_)) = 
        // If the override is listed as only relevant to one type, and we're matching it against an abstract slot of an interface type,
        // then check that interface type is the right type.
        (match implKind with 
         | CanImplementNoSlots -> false
         | CanImplementAnySlot -> true 
         | CanImplementAnyClassHierarchySlot -> not (isInterfaceTy g dispatchSlot.EnclosingType)
         //| CanImplementSpecificInterfaceSlot parentTy -> isInterfaceTy g dispatchSlot.EnclosingType && typeEquiv g parentTy dispatchSlot.EnclosingType 
         | CanImplementAnyInterfaceSlot -> isInterfaceTy g dispatchSlot.EnclosingType)

    let IsTyparKindMatch g amap m (dispatchSlot:MethInfo) (Override(_,_,_,(mtps,_),_,_,_)) = 
        let _vargtys,_,fvmtps,_ = CompiledSigOfMeth g amap m dispatchSlot 
        List.lengthsEqAndForall2 (fun (tp1:Typar) (tp2:Typar) -> tp1.Kind = tp2.Kind) mtps fvmtps
        
    let IsPartialMatch g amap m (dispatchSlot:MethInfo) (Override(_,_,_,(mtps,_),argTys,_retTy,_) as overrideBy) = 
        IsNameMatch dispatchSlot overrideBy &&
        let vargtys,_,fvmtps,_ = CompiledSigOfMeth g amap m dispatchSlot 
        mtps.Length = fvmtps.Length &&
        IsTyparKindMatch g amap m dispatchSlot overrideBy && 
        argTys.Length = vargtys.Length &&
        IsImplMatch g dispatchSlot overrideBy  
          
    let reverseTyparRenaming g tinst = 
        tinst |> List.map (fun (tp,ty) -> (destTyparTy g ty, mkTyparTy tp))

    let composeTyparInsts inst1 inst2 = 
        inst1 |> List.map (map2Of2 (instType inst2)) 
     
    let IsExactMatch g amap m dispatchSlot (Override(_,_,_,(mtps,mtpinst),argTys,retTy,_) as overrideBy) =
        IsPartialMatch g amap m dispatchSlot overrideBy &&
        let vargtys,vrty,fvmtps,ttpinst = CompiledSigOfMeth g amap m dispatchSlot

        // Compare the types. CompiledSigOfMeth, GetObjectExprOverrideInfo and GetTypeMemberOverrideInfo have already 
        // applied all relevant substitutions except the renamings from fvtmps <-> mtps 

        let aenv = TypeEquivEnv.FromEquivTypars fvmtps mtps 

        List.forall2 (List.lengthsEqAndForall2 (typeAEquiv g aenv)) vargtys argTys &&
        returnTypesAEquiv g aenv vrty retTy &&
        
        (* Comparing the method typars and their constraints is much trickier since the substitutions have not been applied 
           to the constraints of these babies. This is partly because constraints are directly attached to typars so it's 
           difficult to apply substitutions to them unless we separate them off at some point, which we don't as yet.        

           Given   C<ctps>
                   D<dtps>
                   dispatchSlot :   C<ctys[dtps]>.M<fvmtps[ctps]>(...)
                   overrideBy:  parent: D<dtys[dtps]>  value: !<ttps> <mtps[ttps]>(...) 
                   
               where X[dtps] indicates that X may involve free type variables dtps
               
               we have 
                   ttpinst maps  ctps --> ctys[dtps] 
                   mtpinst maps  ttps --> dtps
               
               compare fvtmps[ctps] and mtps[ttps] by 
                  fvtmps[ctps]  @ ttpinst     -- gives fvtmps[dtps]
                  fvtmps[dtps] @ rev(mtpinst) -- gives fvtmps[ttps]
                  
               Now fvtmps[ttps] and mtpinst[ttps] are comparable, i.e. have contraints w.r.t. the same set of type variables 
                   
          i.e.  Compose the substitutions ttpinst and rev(mtpinst) *)

              
        (* Compose the substitutions *)
        
        let ttpinst = 
            // check we can reverse - in some error recovery situations we can't 
            if mtpinst |> List.exists (snd >> isTyparTy g >> not) then ttpinst 
            else composeTyparInsts ttpinst (reverseTyparRenaming g mtpinst)

        // Compare under the composed substitutions 
        let aenv = TypeEquivEnv.FromTyparInst ttpinst 
        
        typarsAEquiv g aenv fvmtps mtps

    let OverrideImplementsDispatchSlot g amap m dispatchSlot availPriorOverride =
        IsExactMatch g amap m dispatchSlot availPriorOverride &&
        // The override has to actually be in some subtype of the dispatch slot
        ExistsHeadTypeInEntireHierarchy g amap m (generalizedTyconRef availPriorOverride.BoundingTyconRef) (tcrefOfAppTy g dispatchSlot.EnclosingType)

    let DispatchSlotIsAlreadyImplemented g amap m availPriorOverridesKeyed (dispatchSlot: MethInfo) =
        availPriorOverridesKeyed 
            |> NameMultiMap.find  dispatchSlot.LogicalName  
            |> List.exists (OverrideImplementsDispatchSlot g amap m dispatchSlot)


    /// 6a. check all interface and abstract methods are implemented 
          
    let CheckDispatchSlotsAreImplemented (denv,g,amap,m,
                                          isOverallTyAbstract,
                                          reqdTy,
                                          dispatchSlots:RequiredSlot list,
                                          availPriorOverrides:OverrideInfo list,
                                          overrides:OverrideInfo list) = 

        let isReqdTyInterface = isInterfaceTy g reqdTy 
        let showMissingMethodsAndRaiseErrors = (isReqdTyInterface || not isOverallTyAbstract)
        let res = ref true
        let fail exn = (res := false ; if showMissingMethodsAndRaiseErrors then errorR exn)
        
        // Index the availPriorOverrides and overrides by name
        let availPriorOverridesKeyed = availPriorOverrides |> NameMultiMap.initBy (fun ov -> ov.LogicalName)
        let overridesKeyed = overrides |> NameMultiMap.initBy (fun ov -> ov.LogicalName)
        
        dispatchSlots |> List.iter (fun (RequiredSlot(dispatchSlot,isOptional)) -> 
          
            match NameMultiMap.find dispatchSlot.LogicalName overridesKeyed 
                    |> List.filter (OverrideImplementsDispatchSlot g amap m dispatchSlot)  with
            | [_] -> 
                ()
            | [] -> 
                if not isOptional && 
                   // Check that no available prior override implements this dispatch slot
                   not (DispatchSlotIsAlreadyImplemented g amap m availPriorOverridesKeyed dispatchSlot) then 
                    // error reporting path 
                    let vargtys,_vrty,fvmtps,_ = CompiledSigOfMeth g amap m dispatchSlot
                    let noimpl() = if isReqdTyInterface then fail(Error(FSComp.SR.typrelNoImplementationGivenWithSuggestion(stringOfMethInfo amap m denv dispatchSlot), m))
                                   else fail(Error(FSComp.SR.typrelNoImplementationGiven(stringOfMethInfo amap m denv dispatchSlot), m))
                    match  overrides |> List.filter (IsPartialMatch g amap m dispatchSlot)  with 
                    | [] -> 
                        match overrides |> List.filter (fun overrideBy -> IsNameMatch dispatchSlot overrideBy && 
                                                                            IsImplMatch g dispatchSlot overrideBy)  with 
                        | [] -> 
                            noimpl()
                        | [ Override(_,_,_,(mtps,_),argTys,_,_) as overrideBy ] -> 
                            let error_msg = 
                                if argTys.Length <> vargtys.Length then FSComp.SR.typrelMemberDoesNotHaveCorrectNumberOfArguments(FormatOverride denv overrideBy, FormatMethInfoSig g amap m denv dispatchSlot)
                                elif mtps.Length <> fvmtps.Length then FSComp.SR.typrelMemberDoesNotHaveCorrectNumberOfTypeParameters(FormatOverride denv overrideBy, FormatMethInfoSig g amap m denv dispatchSlot)
                                elif not (IsTyparKindMatch g amap m dispatchSlot overrideBy) then  FSComp.SR.typrelMemberDoesNotHaveCorrectKindsOfGenericParameters(FormatOverride denv overrideBy, FormatMethInfoSig g amap m denv dispatchSlot)
                                else FSComp.SR.typrelMemberCannotImplement(FormatOverride denv overrideBy, stringOfMethInfo amap m denv dispatchSlot, FormatMethInfoSig g amap m denv dispatchSlot)
                            fail(Error(error_msg, overrideBy.Range))
                        | overrideBy :: _ -> 
                            errorR(Error(FSComp.SR.typrelOverloadNotFound(FormatMethInfoSig g amap m denv dispatchSlot, FormatMethInfoSig g amap m denv dispatchSlot),overrideBy.Range))

                    | [ overrideBy ] -> 

                        match dispatchSlots  |> List.filter (fun (RequiredSlot(dispatchSlot,_)) -> OverrideImplementsDispatchSlot g amap m dispatchSlot overrideBy) with 
                        | [] -> 
                            // Error will be reported below in CheckOverridesAreAllUsedOnce 
                            ()
                        | _ -> 
                            noimpl()
                        
                    | _ -> 
                        fail(Error(FSComp.SR.typrelOverrideWasAmbiguous(FormatMethInfoSig g amap m denv dispatchSlot),m))
            | _ -> fail(Error(FSComp.SR.typrelMoreThenOneOverride(FormatMethInfoSig g amap m denv dispatchSlot),m)));
        !res

    /// 6b. check all implementations implement some virtual method 
    let CheckOverridesAreAllUsedOnce denv g amap (reqdTy,
                                                  dispatchSlotsKeyed: NameMultiMap<RequiredSlot>,
                                                  availPriorOverrides: OverrideInfo list,
                                                  overrides
                                                  ) = 
        let availPriorOverridesKeyed = availPriorOverrides |> NameMultiMap.initBy (fun ov -> ov.LogicalName)
        for (Override _ as overrideBy) in overrides do 
          if not overrideBy.IsFakeEventProperty then
            let m = overrideBy.Range
            let relevantVirts = NameMultiMap.find overrideBy.LogicalName dispatchSlotsKeyed

            let relevantVirts = relevantVirts |> List.map (fun (RequiredSlot(dispatchSlot,_)) -> dispatchSlot)
            match relevantVirts  
                     |> List.filter (fun dispatchSlot -> 
                             OverrideImplementsDispatchSlot g amap m dispatchSlot overrideBy) with
            | [] -> 
                // This is all error reporting
                match relevantVirts 
                        |> List.filter (fun dispatchSlot -> IsPartialMatch g amap m dispatchSlot overrideBy) with 
                | [dispatchSlot] -> 
                    errorR(OverrideDoesntOverride(denv,overrideBy,Some dispatchSlot,g,amap,m))
                | _ -> 
                    match relevantVirts 
                            |> List.filter (fun dispatchSlot -> IsNameMatch dispatchSlot overrideBy) with 
                    | [dispatchSlot] -> 
                        errorR(OverrideDoesntOverride(denv, overrideBy, Some dispatchSlot, g, amap, m))
                    | _ -> 
                        errorR(OverrideDoesntOverride(denv,overrideBy,None,g,amap,m))


            | [dispatchSlot] -> 
                if dispatchSlot.IsFinal && not (typeEquiv g reqdTy dispatchSlot.EnclosingType) then 
                    errorR(Error(FSComp.SR.typrelMethodIsSealed(stringOfMethInfo amap m denv dispatchSlot),m))
            | dispatchSlots -> 
                match dispatchSlots |> List.filter (fun dispatchSlot -> 
                              isInterfaceTy g dispatchSlot.EnclosingType || 
                              not (DispatchSlotIsAlreadyImplemented g amap m availPriorOverridesKeyed dispatchSlot)) with
                | h1 :: h2 :: _ -> 
                    errorR(Error(FSComp.SR.typrelOverrideImplementsMoreThenOneSlot((FormatOverride denv overrideBy), (stringOfMethInfo amap m denv h1), (stringOfMethInfo amap m denv h2)),m))
                | _ -> 
                    ()


    //-------------------------------------------------------------------------

    /// Get the slots of a type that can or must be implemented. This depends
    /// partly on the full set of interface types that are being implemented
    /// simultaneously, e.g.
    ///    { new C with  interface I2 = ... interface I3 = ... }
    /// allReqdTys = {C;I2;I3}
    ///
    /// allReqdTys can include one class/record/union type. 
    let GetSlotImplSets (infoReader:InfoReader) denv isObjExpr allReqdTys = 

        let g = infoReader.g
        let amap = infoReader.amap
        
        let availImpliedInterfaces : TType list = 
            [ for (reqdTy,m) in allReqdTys do
                if not (isInterfaceTy g reqdTy) then 
                    let baseTyOpt = if isObjExpr then Some reqdTy else SuperTypeOfType g amap m reqdTy 
                    match baseTyOpt with 
                    | None -> ()
                    | Some baseTy -> yield! AllInterfacesOfType g amap m AllowMultiIntfInst baseTy  ]
                    
        // For each implemented type, get a list containing the transitive closure of
        // interface types implied by the type. This includes the implemented type itself if the implemented type
        // is an interface type.
        let intfSets = 
            allReqdTys |> List.mapi (fun i (reqdTy,m) -> 
                let interfaces = AllInterfacesOfType g amap m AllowMultiIntfInst reqdTy 
                let impliedTys = (if isInterfaceTy g reqdTy then interfaces else reqdTy :: interfaces)
                (i, reqdTy, impliedTys,m))

        // For each implemented type, reduce its list of implied interfaces by subtracting out those implied 
        // by another implemented interface type.
        let reqdTyInfos = 
            intfSets |> List.map (fun (i,reqdTy,impliedTys,m) -> 
                let reduced = 
                    (impliedTys,intfSets) ||> List.fold (fun acc (j,jty,impliedTys2,m) -> 
                         if i <> j && TypeFeasiblySubsumesType 0 g amap m jty CanCoerce reqdTy 
                         then ListSet.subtract (TypesFeasiblyEquiv 0 g amap m) acc impliedTys2
                         else acc ) 
                (i, reqdTy, m, reduced))

        // Check that, for each implemented type, at least one implemented type is implied. This is enough to capture
        // duplicates.
        for (_i, reqdTy, m, impliedTys) in reqdTyInfos do
            if isInterfaceTy g reqdTy && isNil impliedTys then 
                errorR(Error(FSComp.SR.typrelDuplicateInterface(),m));

        // Check that no interface type is implied twice
        //
        // Note complexity O(reqdTy*reqdTy)
        for (i, _reqdTy, reqdTyRange, impliedTys) in reqdTyInfos do
            for (j,_,_,impliedTys2) in reqdTyInfos do
                if i > j then  
                    let overlap = ListSet.intersect (TypesFeasiblyEquiv 0 g amap reqdTyRange) impliedTys impliedTys2
                    overlap |> List.iter (fun overlappingTy -> 
                        if nonNil(GetImmediateIntrinsicMethInfosOfType (None,AccessibleFromSomewhere) g amap reqdTyRange overlappingTy |> List.filter MethInfo.IsVirtual) then                                
                            errorR(Error(FSComp.SR.typrelNeedExplicitImplementation(NicePrint.prettyStringOfTy denv (List.head overlap)),reqdTyRange)));

        // Get the SlotImplSet for each implemented type
        // This contains the list of required members and the list of available members
        [ for (_,reqdTy,reqdTyRange,impliedTys) in reqdTyInfos do

            // Build a table of the implied interface types, for quicker lookup
            let isImpliedInterfaceTable = 
                impliedTys 
                |> List.filter (isInterfaceTy g) 
                |> List.map (fun ty -> tcrefOfAppTy g ty, ()) 
                |> TyconRefMap.OfList 
            
            // Is a member an abstract slot of one of the implied interface types?
            let isImpliedInterfaceType ty =
                isImpliedInterfaceTable.ContainsKey (tcrefOfAppTy g ty) &&
                impliedTys |> List.exists (TypesFeasiblyEquiv 0 g amap reqdTyRange ty)

            //let isSlotImpl (minfo:MethInfo) = 
            //    not minfo.IsAbstract && minfo.IsVirtual 

            // Compute the abstract slots that require implementations
            let dispatchSlots = 
                [ if isInterfaceTy g reqdTy then 
                      for impliedTy in impliedTys  do
                          // Check if the interface has an inherited implementation
                          // If so, you do not have to implement all the methods - each
                          // specific method is "optionally" implemented.
                          let isOptional = 
                              ListSet.contains (typeEquiv g) impliedTy availImpliedInterfaces
                          for reqdSlot in GetImmediateIntrinsicMethInfosOfType (None,AccessibleFromSomewhere) g amap reqdTyRange impliedTy do
                              yield RequiredSlot(reqdSlot, isOptional)
                  else
                      
                      // In the normal case, the requirements for a class are precisely all the abstract slots up the whole hierarchy.
                      // So here we get and yield all of those.
                      for minfo in reqdTy |> GetIntrinsicMethInfosOfType infoReader (None,AccessibleFromSomewhere,AllowMultiIntfInst) IgnoreOverrides reqdTyRange do
                         if minfo.IsDispatchSlot then
                             yield RequiredSlot(minfo,(*isOptional=*)false) ]
                
                
            // Compute the methods that are available to implement abstract slots from the base class
            //
            // This is used in CheckDispatchSlotsAreImplemented when we think a dispatch slot may not
            // have been implemented. 
            let availPriorOverrides : OverrideInfo list = 
                if isInterfaceTy g reqdTy then 
                    []
                else 
                    let reqdTy = 
                        let baseTyOpt = if isObjExpr then Some reqdTy else SuperTypeOfType g amap reqdTyRange reqdTy 
                        match baseTyOpt with 
                        | None -> reqdTy
                        | Some baseTy -> baseTy 
                    [ // Get any class hierarchy methods on this type 
                      //
                      // NOTE: What we have below is an over-approximation that will get too many methods 
                      // and not always correctly relate them to the slots they implement. For example, 
                      // we may get an override from a base class and believe it implements a fresh, new abstract
                      // slot in a subclass. 
                      for minfos in infoReader.GetRawIntrinsicMethodSetsOfType(None,AccessibleFromSomewhere,AllowMultiIntfInst,reqdTyRange,reqdTy) do
                        for minfo in minfos do
                          if not minfo.IsAbstract then 
                              yield GetInheritedMemberOverrideInfo g amap reqdTyRange CanImplementAnyClassHierarchySlot minfo   ]
                     
            // We also collect up the properties. This is used for abstract slot inference when overriding properties
            let isRelevantRequiredProperty (x:PropInfo) = 
                (x.IsVirtualProperty && not (isInterfaceTy g reqdTy)) ||
                isImpliedInterfaceType x.EnclosingType
                
            let reqdProperties = 
                GetIntrinsicPropInfosOfType infoReader (None,AccessibleFromSomewhere,AllowMultiIntfInst) IgnoreOverrides reqdTyRange reqdTy 
                |> List.filter isRelevantRequiredProperty
                
            let dispatchSlotsKeyed = dispatchSlots |> NameMultiMap.initBy (fun (RequiredSlot(v,_)) -> v.LogicalName) 
            yield SlotImplSet(dispatchSlots, dispatchSlotsKeyed, availPriorOverrides, reqdProperties) ]


    let CheckImplementationRelationAtEndOfInferenceScope (infoReader:InfoReader,denv,tycon:Tycon,isImplementation) =

        let g = infoReader.g
        let amap = infoReader.amap

        let tcaug = tycon.TypeContents
        let interfaces = tycon.InterfacesOfFSharpTycon
        
        let interfaces = interfaces |> List.map (fun (ity,_compgen,m) -> (ity,m))

        let overallTy = generalizedTyconRef (mkLocalTyconRef tycon)

        let allReqdTys = (overallTy,tycon.Range) :: interfaces 

        // Get all the members that are immediately part of this type
        // Include the auto-generated members
        let allImmediateMembers = tycon.MembersOfFSharpTyconSorted @ tycon.AllGeneratedValues

        // Get all the members we have to implement, organized by each type we explicitly implement
        let slotImplSets = GetSlotImplSets infoReader denv false allReqdTys

        let allImpls = List.zip allReqdTys slotImplSets


        // Find the methods relevant to implementing the abstract slots listed under the reqdType being checked.
        let allImmediateMembersThatMightImplementDispatchSlots = 
            allImmediateMembers 
            |> List.filter (fun overrideBy -> overrideBy.IsInstanceMember   &&  // exclude static
                                              MemberRefIsVirtual overrideBy &&  // exclude non virtual (e.g. keep override/default). [4469]
                                              not (MemberRefIsDispatchSlot overrideBy))

        let mustOverrideSomething reqdTy (overrideBy:ValRef) =
           let memberInfo = overrideBy.MemberInfo.Value
           not (overrideBy.IsFSharpEventProperty(g)) &&
           memberInfo.MemberFlags.IsOverrideOrExplicitImpl && 
    
           match memberInfo.ImplementedSlotSigs with 
           | [] -> 
                // Are we looking at the implementation of the class hierarchy? If so include all the override members
                not (isInterfaceTy g reqdTy)
           | ss -> 
                 ss |> List.forall (fun ss -> 
                     let ty = ss.ImplementedType
                     if isInterfaceTy g ty then 
                         // Is this a method impl listed under the reqdTy?
                         typeEquiv g ty reqdTy
                     else
                         not (isInterfaceTy g reqdTy) )
        

        // We check all the abstracts related to the class hierarchy and then check each interface implementation
        for ((reqdTy,m),slotImplSet) in allImpls do
            let (SlotImplSet(dispatchSlots, dispatchSlotsKeyed, availPriorOverrides,_)) = slotImplSet
            try 

                // Now extract the information about each overriding method relevant to this SlotImplSet
                let allImmediateMembersThatMightImplementDispatchSlots = 
                    allImmediateMembersThatMightImplementDispatchSlots
                    |> List.map (fun overrideBy -> overrideBy,GetTypeMemberOverrideInfo g reqdTy overrideBy)
                
                // Now check the implementation
                // We don't give missing method errors for abstract classes 
                
                if isImplementation && not (isInterfaceTy g overallTy) then 
                    let overrides = allImmediateMembersThatMightImplementDispatchSlots |> List.map snd 
                    let allCorrect = CheckDispatchSlotsAreImplemented (denv,g,amap,m,tcaug.tcaug_abstract,reqdTy,dispatchSlots,availPriorOverrides,overrides)
                    
                    // Tell the user to mark the thing abstract if it was missing implementations
                    if not allCorrect && not tcaug.tcaug_abstract && not (isInterfaceTy g reqdTy) then 
                        errorR(TypeIsImplicitlyAbstract(m));
                    
                    let overridesToCheck = 
                        allImmediateMembersThatMightImplementDispatchSlots 
                           |> List.filter (fst >> mustOverrideSomething reqdTy)
                           |> List.map snd

                    CheckOverridesAreAllUsedOnce denv g amap (reqdTy, dispatchSlotsKeyed, availPriorOverrides, overridesToCheck);

            with e -> errorRecovery e m; ()

        // Now record the full slotsigs of the abstract members implemented by each override.
        // This is used to generate IL MethodImpls in the code generator.
        allImmediateMembersThatMightImplementDispatchSlots |> List.iter (fun overrideBy -> 

            let isFakeEventProperty = overrideBy.IsFSharpEventProperty(g)
            if not isFakeEventProperty then 
                
                let overriden = 
                    [ for ((reqdTy,m),(SlotImplSet(_dispatchSlots,dispatchSlotsKeyed,_,_))) in allImpls do
                          let overrideByInfo = GetTypeMemberOverrideInfo g reqdTy overrideBy
                          let overridenForThisSlotImplSet = 
                              [ for (RequiredSlot(dispatchSlot,_)) in NameMultiMap.find overrideByInfo.LogicalName dispatchSlotsKeyed do 
                                    if OverrideImplementsDispatchSlot g amap m dispatchSlot overrideByInfo then 
                                        if tyconRefEq g overrideByInfo.BoundingTyconRef (tcrefOfAppTy g dispatchSlot.EnclosingType) then 
                                             match dispatchSlot.ArbitraryValRef with 
                                             | Some virtMember -> 
                                                  if virtMember.MemberInfo.Value.IsImplemented then errorR(Error(FSComp.SR.tcDefaultImplementationAlreadyExists(),overrideByInfo.Range));
                                                  virtMember.MemberInfo.Value.IsImplemented <- true
                                             | None -> () // not an F# slot

                                        // Get the slotsig of the overriden method 
                                        let slotsig = SlotSigOfMethodInfo amap m dispatchSlot

                                        // The slotsig from the overriden method is in terms of the type parameters on the parent type of the overriding method,
                                        // Modify map the slotsig so it is in terms of the type parameters for the overriding method 
                                        let slotsig = ReparentSlotSigToUseMethodTypars g amap m overrideBy slotsig
                     
                                        // Record the slotsig via mutation 
                                        yield slotsig ] 
                          //if mustOverrideSomething reqdTy overrideBy then 
                          //    assert nonNil overridenForThisSlotImplSet
                          yield! overridenForThisSlotImplSet ]
                
                overrideBy.MemberInfo.Value.ImplementedSlotSigs <- overriden);

//-------------------------------------------------------------------------
// Sets of methods involved in overload resolution and trait constraint
// satisfaction.
//------------------------------------------------------------------------- 

/// In the following, 'T gets instantiated to: 
///   1. the expression being supplied for an argument 
///   2. "unit", when simply checking for the existence of an overload that satisfies 
///      a signature, or when finding the corresponding witness. 
/// Note the parametricity helps ensure that overload resolution doesn't depend on the 
/// expression on the callside (though it is in some circumstances allowed 
/// to depend on some type information inferred syntactically from that 
/// expression, e.g. a lambda expression may be converted to a delegate as 
/// an adhoc conversion. 
///
/// The bool indicates if named using a '?' 
type CallerArg<'T> = 
    | CallerArg of TType * range * bool * 'T  
    member x.Type = (let (CallerArg(ty,_,_,_)) = x in ty)
    
/// CalledArg(pos,isParamArray,optArgInfo,isOutArg,nmOpt,argType)
type CalledArg = 
    | CalledArg of (int * int) * bool (* isParamArray *) * OptionalArgInfo * bool (* isOutArg *) * string option * TType 
    member x.Type = (let (CalledArg(_,_,_,_,_,ty)) = x in ty)
    member x.Position = (let (CalledArg(i,_,_,_,_,_)) = x in i)

type AssignedCalledArg<'T> = 
    | AssignedCalledArg of Ident option * CalledArg * CallerArg<'T>
    member x.CalledArg = (let (AssignedCalledArg(_,calledArg,_)) = x in calledArg)
    member x.Position = x.CalledArg.Position

type AssignedItemSetterTarget = 
    | AssignedPropSetter of PropInfo * MethInfo * TypeInst   (* the MethInfo is a non-indexer setter property *)
    | AssignedIlFieldSetter of ILFieldInfo 
    | AssignedRecdFieldSetter of RecdFieldInfo 

type AssignedItemSetter<'T> = AssignedItemSetter of Ident * AssignedItemSetterTarget * CallerArg<'T> 

type CallerNamedArg<'T> = 
    | CallerNamedArg of Ident * CallerArg<'T>  
    member x.Ident = (let (CallerNamedArg(id,_carg)) = x in id)
    member x.Name = x.Ident.idText

type CalledMethArgSet<'T> = 
    | CalledMethArgSet of 
        // The called arguments corresponding to "unnamed" arguments
        CalledArg list * 
        // Any unnamed caller arguments not otherwise assigned 
        CallerArg<'T> list * 
        // The called "ParamArray" argument, if any
        CalledArg option * 
        // Any unnamed caller arguments assigned to a "param array" argument
        CallerArg<'T> list * 
        // named args
        AssignedCalledArg<'T> list 
    member x.UnnamedCalledArgs      = match x with (CalledMethArgSet(unnamedCalledArgs,_,_,_,_)) -> unnamedCalledArgs 
    member x.UnnamedCallerArgs      = match x with (CalledMethArgSet(_,unnamedCallerArgs,_,_,_)) -> unnamedCallerArgs 
    member x.ParamArrayCalledArgOpt = match x with (CalledMethArgSet(_,_,paramArrayCalledArgOpt,_,_)) -> paramArrayCalledArgOpt 
    member x.ParamArrayCallerArgs   = match x with (CalledMethArgSet(_,_,_,paramArrayCallerArgs,_)) -> paramArrayCallerArgs 
    member x.AssignedNamedArgs           = match x with (CalledMethArgSet(_,_,_,_,namedArgs)) -> namedArgs
    member x.NumUnnamedCallerArgs = x.UnnamedCallerArgs.Length
    member x.NumAssignedNamedArgs = x.AssignedNamedArgs.Length
    member x.NumUnnamedCalledArgs = x.UnnamedCalledArgs.Length


// CLEANUP: make this a record or class
type CalledMeth<'T> = 
    | CalledMeth of 
        // the method we're attempting to call 
        MethInfo * 
        // the instantiation of the method we're attempting to call 
        TypeInst * 
        // the formal instantiation of the method we're attempting to call 
        TypeInst * 
        // The types of the actual object arguments, if any
        TType list * 

        // The argument analysis for each set of curried arguments
        CalledMethArgSet<'T> list *

        // return type
        TType * 
        // named property setters
        AssignedItemSetter<'T> list * 
        // the property related to the method we're attempting to call, if any  
        PropInfo option * 
        // unassigned args
        CallerNamedArg<'T> list * 
        // args assigned to specifiy values for attribute fields and properties (these are not necessarily "property sets")
        CallerNamedArg<'T> list  * 
        // unnamed called optional args: pass defaults for these
        CalledArg list *
        // unnamed called out args: return these as part of the return tuple
        CalledArg list

    member x.Method                 = match x with (CalledMeth(minfo,_,_,_,_,_,_,_,_,_,_,_)) -> minfo
    static member GetMethod (x:CalledMeth<'T>) = x.Method

    member x.CalledTyArgs           = match x with (CalledMeth(_,minst,_,_,_,_,_,_,_,_,_,_)) -> minst
    member x.CallerTyArgs           = match x with (CalledMeth(_,_,userTypeArgs,_,_,_,_,_,_,_,_,_)) -> userTypeArgs
    member x.CallerObjArgTys        = match x with (CalledMeth(_,_,_,callerObjArgTys,_,_,_,_,_,_,_,_)) -> callerObjArgTys
    member x.ArgSets                = match x with (CalledMeth(_,_,_,_,argSets,_,_,_,_,_,_,_)) -> argSets
    member x.NumArgSets             = x.ArgSets.Length

    member x.AssignedProps          = match x with (CalledMeth(_,_,_,_,_,_,namedProps,_,_,_,_,_)) -> namedProps
    member x.AssociatedPropertyInfo = match x with (CalledMeth(_,_,_,_,_,_,_,x,_,_,_,_)) -> x
    member x.UnassignedNamedArgs    = match x with (CalledMeth(_,_,_,_,_,_,_,_,unassignedNamedItems,_,_,_)) -> unassignedNamedItems
    member x.AttributeAssignedNamedArgs = match x with (CalledMeth(_,_,_,_,_,_,_,_,_,x,_,_)) -> x
    member x.HasOptArgs             = match x with (CalledMeth(_,_,_,_,_,_,_,_,_,_,unnamedCalledOptArgs,_)) -> nonNil unnamedCalledOptArgs
    member x.HasOutArgs             = match x with (CalledMeth(_,_,_,_,_,_,_,_,_,_,_,unnamedCalledOutArgs)) -> nonNil unnamedCalledOutArgs
    member x.UsesParamArrayConversion = 
        x.ArgSets |> List.exists (fun argSet -> argSet.ParamArrayCalledArgOpt.IsSome)
    member x.ParamArrayCalledArgOpt = 
        x.ArgSets |> List.tryPick (fun argSet -> argSet.ParamArrayCalledArgOpt)
    member x.ParamArrayCallerArgs = 
        x.ArgSets |> List.tryPick (fun argSet -> if isSome argSet.ParamArrayCalledArgOpt then Some argSet.ParamArrayCallerArgs else None )
    member x.ParamArrayElementType(g) = 
        assert (x.UsesParamArrayConversion)
        x.ParamArrayCalledArgOpt.Value.Type |> destArrayTy g 
    member x.NumAssignedProps = x.AssignedProps.Length
    member x.CalledObjArgTys(amap,m) = ObjTypesOfMethInfo amap m x.Method x.CalledTyArgs
    member x.NumCalledTyArgs = x.CalledTyArgs.Length
    member x.NumCallerTyArgs = x.CallerTyArgs.Length 

    member x.AssignsAllNamedArgs = isNil x.UnassignedNamedArgs

    member x.HasCorrectArity =
      (x.NumCalledTyArgs = x.NumCallerTyArgs)  &&
      x.ArgSets |> List.forall (fun argSet -> argSet.NumUnnamedCalledArgs = argSet.NumUnnamedCallerArgs) 

    member x.HasCorrectGenericArity =
      (x.NumCalledTyArgs = x.NumCallerTyArgs)  

    member x.IsAccessible(amap,m,ad) = 
        IsMethInfoAccessible amap m ad x.Method 

    member x.HasCorrectObjArgs(amap,m,ad) = 
        x.CalledObjArgTys(amap,m).Length = x.CallerObjArgTys.Length 

    member x.IsCandidate(g,amap,m,ad) =
        x.IsAccessible(amap,m,ad) &&
        x.HasCorrectArity && 
        x.HasCorrectObjArgs(amap,m,ad) &&
        x.AssignsAllNamedArgs
    member x.AllUnnamedCalledArgs = x.ArgSets |> List.collect (fun x -> x.UnnamedCalledArgs)
    member x.TotalNumUnnamedCalledArgs = x.ArgSets |> List.sumBy (fun x -> x.NumUnnamedCalledArgs)
    member x.TotalNumUnnamedCallerArgs = x.ArgSets |> List.sumBy (fun x -> x.NumUnnamedCallerArgs)
    member x.TotalNumAssignedNamedArgs = x.ArgSets |> List.sumBy (fun x -> x.NumAssignedNamedArgs)

let MakeCalledArgs amap m minfo minst =
    // Mark up the arguments with their position, so we can sort them back into order later 
    let paramDatas = ParamDatasOfMethInfo amap m minfo minst
    paramDatas |> List.mapiSquared (fun i j (ParamData(isParamArrayArg,isOutArg,optArgInfo,nmOpt,typeOfCalledArg))  -> 
        CalledArg((i,j),isParamArrayArg,optArgInfo,isOutArg,nmOpt,typeOfCalledArg))  

let MakeCalledMeth 
      (infoReader:InfoReader,
       checkingAttributeCall, 
       freshenMethInfo,// a function to help generate fresh type variables the property setters methods in generic classes 
       m, 
       ad,                // the access domain of the place where the call is taking place
       minfo,             // the method we're attempting to call 
       minst,             // the instantiation of the method we're attempting to call 
       uminst,            // the formal instantiation of the method we're attempting to call 
       pinfoOpt,          // the property related to the method we're attempting to call, if any  
       objArgs,           // the types of the actual object argument, if any 
       callerArgs: (CallerArg<_> list * CallerNamedArg<_> list) list,     // the data about any arguments supplied by the caller 
       allowParamArgs:bool,       // do we allow the use of a param args method in its "expanded" form?
       allowOutAndOptArgs: bool)  // do we allow the use of the transformation that converts out arguments as tuple returns?
    =
    let g = infoReader.g
    let amap = infoReader.amap
    let methodRetTy = FSharpReturnTyOfMeth amap m minfo minst

    if verbose then dprintf "--> methodRetTy = %s\n" (Layout.showL (typeL methodRetTy));
    if verbose then dprintf "--> minfo.Type = %s\n" (Layout.showL (typeL minfo.EnclosingType));

    let fullCalledArgs = MakeCalledArgs amap m minfo minst
    assert (callerArgs.Length = fullCalledArgs.Length)
 
    let argSetInfos = 
        (callerArgs, fullCalledArgs) ||> List.map2 (fun (unnamedCallerArgs,namedCallerArgs) fullCalledArgs -> 
            // Find the arguments not given by name 
            let unnamedCalledArgs = 
                fullCalledArgs |> List.filter (function 
                    | (CalledArg(_,_,_,_,Some nm,_)) -> 
                        namedCallerArgs |> List.forall (fun (CallerNamedArg(nm2,_e)) -> nm <> nm2.idText)   
                    | _ -> true)

            // See if any of them are 'out' arguments being returned as part of a return tuple 
            let unnamedCalledArgs, unnamedCalledOptArgs, unnamedCalledOutArgs = 
                let nUnnamedCallerArgs = unnamedCallerArgs.Length
                if allowOutAndOptArgs && nUnnamedCallerArgs < unnamedCalledArgs.Length then
                    let unnamedCalledArgsTrimmed,unnamedCalledOptOrOutArgs = List.chop nUnnamedCallerArgs unnamedCalledArgs
                    
                    // Check if all optional/out arguments are byref-out args
                    if unnamedCalledOptOrOutArgs |> List.forall (fun (CalledArg(_i,_,_,isOutArg,_,typeOfCalledArg)) -> isOutArg && isByrefTy g typeOfCalledArg) then 
                        let unnamedCalledOutArgs = unnamedCalledOptOrOutArgs |> List.map (fun (CalledArg(i,isParamArrayArg,optArgInfo,isOutArg,nmOpt,typeOfCalledArg)) -> (CalledArg(i,isParamArrayArg,optArgInfo,isOutArg,nmOpt,typeOfCalledArg)))
                        unnamedCalledArgsTrimmed,[],unnamedCalledOutArgs
                    // Check if all optional/out arguments are optional args
                    elif unnamedCalledOptOrOutArgs |> List.forall (fun (CalledArg(_i,_,optArgInfo,_isOutArg,_,_typeOfCalledArg)) -> optArgInfo.IsOptional) then 
                        let unnamedCalledOptArgs = unnamedCalledOptOrOutArgs
                        unnamedCalledArgsTrimmed,unnamedCalledOptArgs,[]
                    // Otherwise drop them on the floor
                    else
                        unnamedCalledArgs,[],[]
                else 
                    unnamedCalledArgs,[],[]

            let (unnamedCallerArgs,paramArrayCallerArgs),unnamedCalledArgs,paramArrayCalledArgOpt = 
                let minArgs = unnamedCalledArgs.Length - 1
                let supportsParamArgs = 
                    allowParamArgs && 
                    minArgs >= 0 && 
                    unnamedCalledArgs |> List.last |> (fun (CalledArg(_,isParamArray,_,_,_,ty)) -> isParamArray && isArray1DTy g ty)

                if supportsParamArgs  && unnamedCallerArgs.Length >= minArgs then
                    let a,b = List.frontAndBack unnamedCalledArgs
                    List.chop minArgs unnamedCallerArgs, a, Some(b)
                else
                    (unnamedCallerArgs, []),unnamedCalledArgs, None
            //dprintfn "Calling %s: paramArrayCallerArgs = %d, paramArrayCalledArgOpt = %d" minfo.LogicalName paramArrayCallerArgs.Length (Option.length paramArrayCalledArgOpt)

            let assignedNamedArgs = fullCalledArgs |> List.choose (function CalledArg(_,_,_,_,Some nm,_) as arg -> List.tryPick (fun (CallerNamedArg(nm2,arg2)) -> if nm = nm2.idText then Some (AssignedCalledArg(Some(nm2),arg,arg2)) else None)  namedCallerArgs | _ -> None)
            let unassignedNamedItem = namedCallerArgs |> List.filter (fun (CallerNamedArg(nm,_e)) -> List.forall (function CalledArg(_,_,_,_,Some nm2,_) -> nm.idText <> nm2 | _ -> true) fullCalledArgs)

            let attributeAssignedNamedItems,unassignedNamedItem = 
                if checkingAttributeCall then 
                    // the assignment of names to properties is substantially for attribute specifications 
                    // permits bindings of names to non-mutable fields and properties, so we do that using the old 
                    // reliable code for this later on. 
                    unassignedNamedItem,[]
                 else 
                    [],unassignedNamedItem

            let assignedNamedProps,unassignedNamedItem = 
                let returnedObjTy = if minfo.IsConstructor then minfo.EnclosingType else methodRetTy
                unassignedNamedItem |> List.splitChoose (fun (CallerNamedArg(id,e) as arg) -> 
                    let nm = id.idText
                    let pinfos = GetIntrinsicPropInfoSetsOfType infoReader (Some(nm),ad,FirstIntfInst) IgnoreOverrides id.idRange returnedObjTy
                    let pinfos = pinfos |> ExcludeHiddenOfPropInfos g amap m 
                    match pinfos with 
                    | [pinfo] when pinfo.HasSetter && not pinfo.IsIndexer -> 
                        let pminfo = pinfo.SetterMethod
                        let pminst = freshenMethInfo m pminfo
                        Choice1Of2(AssignedItemSetter(id,AssignedPropSetter(pinfo,pminfo, pminst), e))
                    | _ ->
                        match infoReader.GetILFieldInfosOfType(Some(nm),ad,m,returnedObjTy) with
                        | finfo :: _ -> 
                            Choice1Of2(AssignedItemSetter(id,AssignedIlFieldSetter(finfo), e))
                        | _ ->              
                          match infoReader.TryFindRecdFieldInfoOfType(nm,m,returnedObjTy) with
                          | Some rfinfo -> 
                              Choice1Of2(AssignedItemSetter(id,AssignedRecdFieldSetter(rfinfo), e))
                          | None -> 
                              Choice2Of2(arg))

            let names = namedCallerArgs |> List.map (function CallerNamedArg(nm,_) -> nm.idText) 

            if (List.noRepeats String.order names).Length <> namedCallerArgs.Length then
                errorR(Error(FSComp.SR.typrelNamedArgumentHasBeenAssignedMoreThenOnce(),m));
                
            if verbose then dprintf "#fullCalledArgs = %d, #unnamedCalledArgs = %d, #assignedNamedArgs = %d, #residueNamedArgs = %d, #attributeAssignedNamedItems = %d\n"
                                        fullCalledArgs.Length unnamedCalledArgs.Length assignedNamedArgs.Length unassignedNamedItem.Length attributeAssignedNamedItems.Length;
            let argSet = CalledMethArgSet(unnamedCalledArgs,unnamedCallerArgs,paramArrayCalledArgOpt,paramArrayCallerArgs,assignedNamedArgs)
            (argSet,assignedNamedProps,unassignedNamedItem,attributeAssignedNamedItems,unnamedCalledOptArgs,unnamedCalledOutArgs))

    let argSets                     = argSetInfos |> List.map     (fun (x,_,_,_,_,_) -> x)
    let assignedNamedProps          = argSetInfos |> List.collect (fun (_,x,_,_,_,_) -> x)
    let unassignedNamedItems        = argSetInfos |> List.collect (fun (_,_,x,_,_,_) -> x)
    let attributeAssignedNamedItems = argSetInfos |> List.collect (fun (_,_,_,x,_,_) -> x)
    let unnamedCalledOptArgs        = argSetInfos |> List.collect (fun (_,_,_,_,x,_) -> x)
    let unnamedCalledOutArgs        = argSetInfos |> List.collect (fun (_,_,_,_,_,x) -> x)
    CalledMeth(minfo,minst,uminst,objArgs,argSets,methodRetTy,assignedNamedProps,pinfoOpt,unassignedNamedItems,attributeAssignedNamedItems,unnamedCalledOptArgs,unnamedCalledOutArgs)
    
let NamesOfCalledArgs calledArgs = 
    calledArgs |> List.choose (fun (CalledArg(_,_,_,_,nmOpt,_)) -> nmOpt) 


let showAccessDomain ad =
    match ad with 
    | AccessibleFromEverywhere -> "public" 
    | AccessibleFrom(_,_) -> "accessible"
    | AccessibleFromSomeFSharpCode -> "public, protected or internal" 
    | AccessibleFromSomewhere -> ""




/// "Type Completion" inference and a few other checks at the end of the
/// inference scope
let FinalTypeDefinitionChecksAtEndOfInferenceScope (infoReader:InfoReader) isImplementation denv (tycon:Tycon) =

    let g = infoReader.g
    let amap = infoReader.amap

    let tcaug = tycon.TypeContents
    tcaug.tcaug_closed <- true
  
    // Note you only have to explicitly implement 'System.IComparable' to customize structural comparison AND equality on F# types 
    if isImplementation &&
       isNone tycon.GeneratedCompareToValues &&
       tycon.HasInterface g g.mk_IComparable_ty && 
       not (tycon.HasOverride g "Equals" [g.obj_ty]) && 
       not tycon.IsFSharpInterfaceTycon
     then
        (* Warn when we're doing this for class types *)
        if Augment.TyconIsCandidateForAugmentationWithEquals g tycon then
            warning(Error(FSComp.SR.typrelTypeImplementsIComparableShouldOverrideObjectEquals(tycon.DisplayName),tycon.Range))
        else
            warning(Error(FSComp.SR.typrelTypeImplementsIComparableDefaultObjectEqualsProvided(tycon.DisplayName),tycon.Range))

    Augment.CheckAugmentationAttribs isImplementation g amap tycon;
    // Check some conditions about generic comparison and hashing. We can only check this condition after we've done the augmentation 
    if isImplementation then

        let tcaug = tycon.TypeContents
        let m = tycon.Range
        let hasExplicitObjectGetHashCode = tycon.HasOverride g "GetHashCode" []
        let hasExplicitObjectEqualsOverride = tycon.HasOverride g "Equals" [g.obj_ty]

        if (isSome tycon.GeneratedHashAndEqualsWithComparerValues) && 
           (hasExplicitObjectGetHashCode || hasExplicitObjectEqualsOverride) then 
            errorR(Error(FSComp.SR.typrelExplicitImplementationOfGetHashCodeOrEquals(tycon.DisplayName),m)); 

        if not hasExplicitObjectEqualsOverride && hasExplicitObjectGetHashCode then 
            warning(Error(FSComp.SR.typrelExplicitImplementationOfGetHashCode(tycon.DisplayName),m)); 

        if hasExplicitObjectEqualsOverride && not hasExplicitObjectGetHashCode then 
            warning(Error(FSComp.SR.typrelExplicitImplementationOfEquals(tycon.DisplayName),m)); 


        // remember these values to ensure we don't generate these methods during codegen 
        tcaug.SetHasObjectGetHashCode hasExplicitObjectGetHashCode;

        if not tycon.IsHiddenReprTycon
           && not tycon.IsTypeAbbrev
           && not tycon.IsMeasureableReprTycon
           && not tycon.IsAsmReprTycon
           && not tycon.IsFSharpInterfaceTycon
           && not tycon.IsFSharpDelegateTycon then 

            DispatchSlotChecking.CheckImplementationRelationAtEndOfInferenceScope (infoReader,denv,tycon,isImplementation) 
    
/// "Single Feasible Type" inference
/// Look for the unique supertype of ty2 for which ty2 :> ty1 might feasibly hold
let FindUniqueFeasibleSupertype g amap m ty1 ty2 =  
    if not (isAppTy g ty2) then None else
    let supertypes = Option.toList (SuperTypeOfType g amap m ty2) @ (InterfacesOfType g amap m ty2)
    supertypes |> List.tryFind (TypeFeasiblySubsumesType 0 g amap m ty1 NoCoerce) 
    


/// Get the methods relevant to deterimining if a uniquely-identified-override exists based on the syntactic information 
/// at the member signature prior to type inference. This is used to pre-assign type information if it does 
let GetAbstractMethInfosForSynMethodDecl(infoReader:InfoReader,ad,memberName:Ident,bindm,typToSearchForAbstractMembers,valSynData) =
    let minfos = 
        match typToSearchForAbstractMembers with 
        | _,Some(SlotImplSet(_, dispatchSlotsKeyed,_,_)) -> 
            NameMultiMap.find  memberName.idText dispatchSlotsKeyed |> List.map (fun (RequiredSlot(dispatchSlot,_)) -> dispatchSlot)
        | ty, None -> 
            GetIntrinsicMethInfosOfType infoReader (Some(memberName.idText), ad, AllowMultiIntfInst) IgnoreOverrides bindm ty
    let dispatchSlots = minfos |> List.filter (fun minfo -> minfo.IsDispatchSlot)
    let topValSynArities = SynInfo.AritiesOfArgs valSynData
    let topValSynArities = if topValSynArities.Length > 0 then topValSynArities.Tail else topValSynArities
    let dispatchSlotsArityMatch = dispatchSlots |> List.filter (fun minfo -> minfo.NumArgs = topValSynArities) 
    dispatchSlots,dispatchSlotsArityMatch 

/// Get the proeprties relevant to deterimining if a uniquely-identified-override exists based on the syntactic information 
/// at the member signature prior to type inference. This is used to pre-assign type information if it does 
let GetAbstractPropInfosForSynPropertyDecl(infoReader:InfoReader,ad,memberName:Ident,bindm,typToSearchForAbstractMembers,_k,_valSynData) = 
    let pinfos = 
        match typToSearchForAbstractMembers with 
        | _,Some(SlotImplSet(_,_,_,reqdProps)) -> 
            reqdProps |> List.filter (fun pinfo -> pinfo.PropertyName = memberName.idText) 
        | ty, None -> 
            GetIntrinsicPropInfosOfType infoReader (Some(memberName.idText), ad, AllowMultiIntfInst) IgnoreOverrides bindm ty
        
    let dispatchSlots = pinfos |> List.filter (fun pinfo -> pinfo.IsVirtualProperty)
    dispatchSlots

