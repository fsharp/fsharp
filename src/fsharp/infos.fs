// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// tinfos, minfos, finfos, pinfos - summaries of information for references
/// to .NET and F# constructs.


module internal Microsoft.FSharp.Compiler.Infos

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.TcGlobals
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Core.Printf

#if EXTENSIONTYPING
open Microsoft.FSharp.Compiler.ExtensionTyping
open Microsoft.FSharp.Core.CompilerServices
#endif

#if FX_RESHAPED_REFLECTION
open Microsoft.FSharp.Core.ReflectionAdapters
#endif

//-------------------------------------------------------------------------
// From IL types to F# types
//------------------------------------------------------------------------- 

/// Import an IL type as an F# type. importInst gives the context for interpreting type variables.
let ImportILType scoref amap m importInst ilty = 
    ilty |> rescopeILType scoref |>  Import.ImportILType amap m importInst 

let CanImportILType scoref amap m ilty = 
    ilty |> rescopeILType scoref |>  Import.CanImportILType amap m 

//-------------------------------------------------------------------------
// Fold the hierarchy. 
//  REVIEW: this code generalizes the iteration used below for member lookup.
//------------------------------------------------------------------------- 

/// Indicates if an F# type is the type associated with an F# exception declaration
let isExnDeclTy g typ = 
    isAppTy g typ && (tcrefOfAppTy g typ).IsExceptionDecl
    
/// Get the base type of a type, taking into account type instantiations. Return None if the
/// type has no base type.
let GetSuperTypeOfType g amap m typ = 
#if EXTENSIONTYPING
    let typ = (if isAppTy g typ && (tcrefOfAppTy g typ).IsProvided then stripTyEqns g typ else stripTyEqnsAndMeasureEqns g typ)
#else
    let typ = stripTyEqnsAndMeasureEqns g typ 
#endif

    match metadataOfTy g typ with 
#if EXTENSIONTYPING
    | ProvidedTypeMetadata info -> 
        let st = info.ProvidedType
        let superOpt = st.PApplyOption((fun st -> match st.BaseType with null -> None | t -> Some t),m)
        match superOpt with 
        | None -> None
        | Some super -> Some(Import.ImportProvidedType amap m super)
#endif
    | ILTypeMetadata (TILObjectReprData(scoref,_,tdef)) -> 
        let _,tinst = destAppTy g typ
        match tdef.Extends with 
        | None -> None
        | Some ilty -> Some (ImportILType scoref amap m tinst ilty)

    | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 

        if isFSharpObjModelTy g typ  || isExnDeclTy g typ then 
            let tcref,_tinst = destAppTy g typ
            Some (instType (mkInstForAppTy g typ) (superOfTycon g tcref.Deref))
        elif isArrayTy g typ then
            Some g.system_Array_typ
        elif isRefTy g typ && not (isObjTy g typ) then 
            Some g.obj_ty
        elif isStructTupleTy g typ then 
            Some g.obj_ty
        elif isRecdTy g typ || isUnionTy g typ then
            Some g.obj_ty
        else 
            None

/// Make a type for System.Collections.Generic.IList<ty>
let mkSystemCollectionsGenericIListTy (g: TcGlobals) ty = TType_app(g.tcref_System_Collections_Generic_IList,[ty])

[<RequireQualifiedAccess>]
/// Indicates whether we can skip interface types that lie outside the reference set
type SkipUnrefInterfaces = Yes | No


/// Collect the set of immediate declared interface types for an F# type, but do not
/// traverse the type hierarchy to collect further interfaces.
let rec GetImmediateInterfacesOfType skipUnref g amap m typ = 
    let itys = 
        if isAppTy g typ then
            let tcref,tinst = destAppTy g typ
            if tcref.IsMeasureableReprTycon then             
                [ match tcref.TypeReprInfo with 
                  | TMeasureableRepr reprTy -> 
                       for ity in GetImmediateInterfacesOfType skipUnref g amap m reprTy do 
                          if isAppTy g ity then 
                              let itcref = tcrefOfAppTy g ity
                              if not (tyconRefEq g itcref g.system_GenericIComparable_tcref) && 
                                 not (tyconRefEq g itcref g.system_GenericIEquatable_tcref)  then 
                                   yield ity
                  | _ -> ()
                  yield mkAppTy g.system_GenericIComparable_tcref [typ] 
                  yield mkAppTy g.system_GenericIEquatable_tcref [typ]]
            else
                match metadataOfTy g typ with 
#if EXTENSIONTYPING
                | ProvidedTypeMetadata info -> 
                    [ for ity in info.ProvidedType.PApplyArray((fun st -> st.GetInterfaces()), "GetInterfaces", m) do
                          yield Import.ImportProvidedType amap m ity ]
#endif
                | ILTypeMetadata (TILObjectReprData(scoref,_,tdef)) -> 

                    // ImportILType may fail for an interface if the assembly load set is incomplete and the interface
                    // comes from another assembly. In this case we simply skip the interface:
                    // if we don't skip it, then compilation will just fail here, and if type checking
                    // succeeds with fewer non-dereferencable interfaces reported then it would have 
                    // succeeded with more reported. There are pathological corner cases where this 
                    // doesn't apply: e.g. for mscorlib interfaces like IComparable, but we can always 
                    // assume those are present. 
                    tdef.Implements |> List.choose (fun ity -> 
                         if skipUnref = SkipUnrefInterfaces.No || CanImportILType scoref amap m ity then 
                             Some (ImportILType scoref amap m tinst ity)
                         else None)

                | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 
                    tcref.ImmediateInterfaceTypesOfFSharpTycon |> List.map (instType (mkInstForAppTy g typ)) 
        else 
            []
        
    // .NET array types are considered to implement IList<T>
    let itys =
        if isArray1DTy g typ then 
            mkSystemCollectionsGenericIListTy g (destArrayTy g typ) :: itys
        else 
            itys
    itys
        
[<RequireQualifiedAccess>]
/// Indicates whether we should visit multiple instantiations of the same generic interface or not
type AllowMultiIntfInstantiations = Yes | No

/// Traverse the type hierarchy, e.g. f D (f C (f System.Object acc)). 
/// Visit base types and interfaces first.
let private FoldHierarchyOfTypeAux followInterfaces allowMultiIntfInst skipUnref visitor g amap m typ acc = 
    let rec loop ndeep typ ((visitedTycon,visited:TyconRefMultiMap<_>,acc) as state) =

        let seenThisTycon = isAppTy g typ && Set.contains (tcrefOfAppTy g typ).Stamp visitedTycon 

        // Do not visit the same type twice. Could only be doing this if we've seen this tycon
        if seenThisTycon && List.exists (typeEquiv g typ) (visited.Find (tcrefOfAppTy g typ)) then state else

        // Do not visit the same tycon twice, e.g. I<int> and I<string>, collect I<int> only, unless directed to allow this
        if seenThisTycon && allowMultiIntfInst = AllowMultiIntfInstantiations.No then state else

        let state = 
            if isAppTy g typ then 
                let tcref = tcrefOfAppTy g typ
                let visitedTycon = Set.add tcref.Stamp visitedTycon 
                visitedTycon, visited.Add (tcref,typ), acc
            else
                state

        if ndeep > 100 then (errorR(Error((FSComp.SR.recursiveClassHierarchy (showType typ)),m)); (visitedTycon,visited,acc)) else
        let visitedTycon,visited,acc = 
            if isInterfaceTy g typ then 
                List.foldBack 
                   (loop (ndeep+1)) 
                   (GetImmediateInterfacesOfType skipUnref g amap m typ) 
                      (loop ndeep g.obj_ty state)
            else
                match tryDestTyparTy g typ with
                | Some tp ->
                    let state = loop (ndeep+1) g.obj_ty state 
                    List.foldBack 
                        (fun x vacc -> 
                          match x with 
                          | TyparConstraint.MayResolveMember _
                          | TyparConstraint.DefaultsTo _
                          | TyparConstraint.SupportsComparison _
                          | TyparConstraint.SupportsEquality _
                          | TyparConstraint.IsEnum _
                          | TyparConstraint.IsDelegate _
                          | TyparConstraint.SupportsNull _
                          | TyparConstraint.IsNonNullableStruct _ 
                          | TyparConstraint.IsUnmanaged _ 
                          | TyparConstraint.IsReferenceType _ 
                          | TyparConstraint.SimpleChoice _ 
                          | TyparConstraint.RequiresDefaultConstructor _ -> vacc
                          | TyparConstraint.CoercesTo(cty,_) -> 
                                  loop (ndeep + 1)  cty vacc) 
                        tp.Constraints 
                        state
                | None -> 
                    let state = 
                        if followInterfaces then 
                            List.foldBack 
                              (loop (ndeep+1)) 
                              (GetImmediateInterfacesOfType skipUnref g amap m typ) 
                              state 
                        else 
                            state
                    let state = 
                        Option.foldBack 
                          (loop (ndeep+1)) 
                          (GetSuperTypeOfType g amap m typ) 
                          state
                    state
        let acc = visitor typ acc
        (visitedTycon,visited,acc)
    loop 0 typ (Set.empty,TyconRefMultiMap<_>.Empty,acc)  |> p33

/// Fold, do not follow interfaces (unless the type is itself an interface)
let FoldPrimaryHierarchyOfType f g amap m allowMultiIntfInst typ acc = 
    FoldHierarchyOfTypeAux false allowMultiIntfInst SkipUnrefInterfaces.No f g amap m typ acc 

/// Fold, following interfaces. Skipping interfaces that lie outside the referenced assembly set is allowed.
let FoldEntireHierarchyOfType f g amap m allowMultiIntfInst typ acc = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst SkipUnrefInterfaces.Yes f g amap m typ acc

/// Iterate, following interfaces. Skipping interfaces that lie outside the referenced assembly set is allowed.
let IterateEntireHierarchyOfType f g amap m allowMultiIntfInst typ = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst SkipUnrefInterfaces.Yes (fun ty () -> f ty) g amap m typ () 

/// Search for one element satisfying a predicate, following interfaces
let ExistsInEntireHierarchyOfType f g amap m allowMultiIntfInst typ = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst SkipUnrefInterfaces.Yes (fun ty acc -> acc || f ty ) g amap m typ false 

/// Search for one element where a function returns a 'Some' result, following interfaces
let SearchEntireHierarchyOfType f g amap m typ = 
    FoldHierarchyOfTypeAux true AllowMultiIntfInstantiations.Yes SkipUnrefInterfaces.Yes
        (fun ty acc -> 
            match acc with 
            | None -> if f ty then Some(ty) else None 
            | Some _ -> acc) 
        g amap m typ None

/// Get all super types of the type, including the type itself
let AllSuperTypesOfType g amap m allowMultiIntfInst ty = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst SkipUnrefInterfaces.No (ListSet.insert (typeEquiv g)) g amap m ty [] 

/// Get all interfaces of a type, including the type itself if it is an interface
let AllInterfacesOfType g amap m allowMultiIntfInst ty = 
    AllSuperTypesOfType g amap m allowMultiIntfInst ty |> List.filter (isInterfaceTy g)

/// Check if two types have the same nominal head type
let HaveSameHeadType g ty1 ty2 = 
    isAppTy g ty1 && isAppTy g ty2 &&
    tyconRefEq g (tcrefOfAppTy g ty1) (tcrefOfAppTy g ty2)

/// Check if a type has a particular head type
let HasHeadType g tcref ty2 = 
        isAppTy g ty2 &&
        tyconRefEq g tcref (tcrefOfAppTy g ty2)
        

/// Check if a type exists somewhere in the hierarchy which has the same head type as the given type (note, the given type need not have a head type at all)
let ExistsSameHeadTypeInHierarchy g amap m typeToSearchFrom typeToLookFor = 
    ExistsInEntireHierarchyOfType (HaveSameHeadType g typeToLookFor)  g amap m AllowMultiIntfInstantiations.Yes typeToSearchFrom
  
/// Check if a type exists somewhere in the hierarchy which has the given head type.
let ExistsHeadTypeInEntireHierarchy g amap m typeToSearchFrom tcrefToLookFor = 
    ExistsInEntireHierarchyOfType (HasHeadType g tcrefToLookFor) g amap m AllowMultiIntfInstantiations.Yes typeToSearchFrom
  

/// Read an Abstract IL type from metadata and convert to an F# type.
let ImportILTypeFromMetadata amap m scoref tinst minst ilty = 
    ImportILType scoref amap m (tinst@minst) ilty

        
/// Get the return type of an IL method, taking into account instantiations for type and method generic parameters, and
/// translating 'void' to 'None'.
let ImportReturnTypeFromMetaData amap m ty scoref tinst minst =
    match ty with 
    | ILType.Void -> None
    | retTy ->  Some (ImportILTypeFromMetadata amap m scoref tinst minst retTy)

/// Copy constraints.  If the constraint comes from a type parameter associated
/// with a type constructor then we are simply renaming type variables.  If it comes
/// from a generic method in a generic class (e.g. typ.M<_>) then we may be both substituting the
/// instantiation associated with 'typ' as well as copying the type parameters associated with 
/// M and instantiating their constraints
///
/// Note: this now looks identical to constraint instantiation.

let CopyTyparConstraints m tprefInst (tporig:Typar) =
    tporig.Constraints 
    |>  List.map (fun tpc -> 
           match tpc with 
           | TyparConstraint.CoercesTo(ty,_) -> 
               TyparConstraint.CoercesTo (instType tprefInst ty,m)
           | TyparConstraint.DefaultsTo(priority,ty,_) -> 
               TyparConstraint.DefaultsTo (priority,instType tprefInst ty,m)
           | TyparConstraint.SupportsNull _ -> 
               TyparConstraint.SupportsNull m
           | TyparConstraint.IsEnum (uty,_) -> 
               TyparConstraint.IsEnum (instType tprefInst uty,m)
           | TyparConstraint.SupportsComparison _ -> 
               TyparConstraint.SupportsComparison m
           | TyparConstraint.SupportsEquality _ -> 
               TyparConstraint.SupportsEquality m
           | TyparConstraint.IsDelegate(aty, bty,_) -> 
               TyparConstraint.IsDelegate (instType tprefInst aty,instType tprefInst bty,m)
           | TyparConstraint.IsNonNullableStruct _ -> 
               TyparConstraint.IsNonNullableStruct m
           | TyparConstraint.IsUnmanaged _ ->
               TyparConstraint.IsUnmanaged m
           | TyparConstraint.IsReferenceType _ -> 
               TyparConstraint.IsReferenceType m
           | TyparConstraint.SimpleChoice (tys,_) -> 
               TyparConstraint.SimpleChoice (List.map (instType tprefInst) tys,m)
           | TyparConstraint.RequiresDefaultConstructor _ -> 
               TyparConstraint.RequiresDefaultConstructor m
           | TyparConstraint.MayResolveMember(traitInfo,_) -> 
               TyparConstraint.MayResolveMember (instTrait tprefInst traitInfo,m))

/// The constraints for each typar copied from another typar can only be fixed up once 
/// we have generated all the new constraints, e.g. f<A :> List<B>, B :> List<A>> ... 
let FixupNewTypars m (formalEnclosingTypars:Typars) (tinst: TType list) (tpsorig: Typars) (tps: Typars) =
    // Checks.. These are defensive programming against early reported errors.
    let n0 = formalEnclosingTypars.Length
    let n1 = tinst.Length
    let n2 = tpsorig.Length
    let n3 = tps.Length
    if n0 <> n1 then error(Error((FSComp.SR.tcInvalidTypeArgumentCount(n0,n1)),m))
    if n2 <> n3 then error(Error((FSComp.SR.tcInvalidTypeArgumentCount(n2,n3)),m))

    // The real code.. 
    let renaming,tptys = mkTyparToTyparRenaming tpsorig tps
    let tprefInst = mkTyparInst formalEnclosingTypars tinst @ renaming
    (tpsorig,tps) ||> List.iter2 (fun tporig tp -> tp.FixupConstraints (CopyTyparConstraints  m tprefInst tporig)) 
    renaming,tptys


//-------------------------------------------------------------------------
// Predicates and properties on values and members
 

type ValRef with 
    /// Indicates if an F#-declared function or member value is a CLIEvent property compiled as a .NET event
    member x.IsFSharpEventProperty g = 
        x.IsMember && CompileAsEvent g x.Attribs && not x.IsExtensionMember

    /// Check if an F#-declared member value is a virtual method
    member vref.IsVirtualMember = 
        let flags = vref.MemberInfo.Value.MemberFlags
        flags.IsDispatchSlot || flags.IsOverrideOrExplicitImpl

    /// Check if an F#-declared member value is a dispatch slot
    member vref.IsDispatchSlotMember =  
        let membInfo = vref.MemberInfo.Value
        membInfo.MemberFlags.IsDispatchSlot 

    /// Check if an F#-declared member value is an 'override' or explicit member implementation
    member vref.IsDefiniteFSharpOverrideMember = 
        let membInfo = vref.MemberInfo.Value   
        let flags = membInfo.MemberFlags
        not flags.IsDispatchSlot && (flags.IsOverrideOrExplicitImpl || not (isNil membInfo.ImplementedSlotSigs))

    /// Check if an F#-declared member value is an  explicit interface member implementation
    member vref.IsFSharpExplicitInterfaceImplementation g = 
        match vref.MemberInfo with 
        | None -> false
        | Some membInfo ->
        not membInfo.MemberFlags.IsDispatchSlot && 
        (match membInfo.ImplementedSlotSigs with 
         | TSlotSig(_,oty,_,_,_,_) :: _ -> isInterfaceTy g oty
         | [] -> false)

    member vref.ImplementedSlotSignatures =
        match vref.MemberInfo with
        | None -> []
        | Some membInfo -> membInfo.ImplementedSlotSigs

//-------------------------------------------------------------------------
// Helper methods associated with using TAST metadata (F# members, values etc.) 
// as backing data for MethInfo, PropInfo etc.


#if EXTENSIONTYPING
/// Get the return type of a provided method, where 'void' is returned as 'None'
let GetCompiledReturnTyOfProvidedMethodInfo amap m (mi:Tainted<ProvidedMethodBase>) =
    let returnType = 
        if mi.PUntaint((fun mi -> mi.IsConstructor),m) then  
            mi.PApply((fun mi -> mi.DeclaringType),m)
        else mi.Coerce<ProvidedMethodInfo>(m).PApply((fun mi -> mi.ReturnType),m)
    let typ = Import.ImportProvidedType amap m returnType
    if isVoidTy amap.g typ then None else Some typ
#endif

/// The slotsig returned by methInfo.GetSlotSig is in terms of the type parameters on the parent type of the overriding method.
/// Reverse-map the slotsig so it is in terms of the type parameters for the overriding method 
let ReparentSlotSigToUseMethodTypars g m ovByMethValRef slotsig = 
    match PartitionValRefTypars g ovByMethValRef with
    | Some(_,enclosingTypars,_,_,_) -> 
        let parentToMemberInst,_ = mkTyparToTyparRenaming (ovByMethValRef.MemberApparentParent.Typars(m)) enclosingTypars
        let res = instSlotSig parentToMemberInst slotsig
        res
    | None -> 
        // Note: it appears PartitionValRefTypars should never return 'None' 
        slotsig


/// Construct the data representing a parameter in the signature of an abstract method slot
let MakeSlotParam (ty,argInfo:ArgReprInfo) = TSlotParam(Option.map textOfId argInfo.Name, ty, false,false,false,argInfo.Attribs) 

/// Construct the data representing the signature of an abstract method slot
let MakeSlotSig (nm,typ,ctps,mtps,paraml,retTy) = copySlotSig (TSlotSig(nm,typ,ctps,mtps,paraml,retTy))


/// Split the type of an F# member value into 
///    - the type parameters associated with method but matching those of the enclosing type
///    - the type parameters associated with a generic method
///    - the return type of the method
///    - the actual type arguments of the enclosing type.
let private AnalyzeTypeOfMemberVal isCSharpExt g (typ,vref:ValRef) = 
    let memberAllTypars,_,retTy,_ = GetTypeOfMemberInMemberForm g vref
    if isCSharpExt || vref.IsExtensionMember then 
        [],memberAllTypars,retTy,[]
    else
        let parentTyArgs = argsOfAppTy g typ
        let memberParentTypars,memberMethodTypars = List.chop parentTyArgs.Length memberAllTypars
        memberParentTypars,memberMethodTypars,retTy,parentTyArgs

/// Get the object type for a member value which is an extension method  (C#-style or F#-style)
let private GetObjTypeOfInstanceExtensionMethod g (vref:ValRef) = 
    let _,curriedArgInfos,_,_ = GetTopValTypeInCompiledForm g vref.ValReprInfo.Value vref.Type vref.Range
    curriedArgInfos.Head.Head |> fst

/// Get the object type for a member value which is a C#-style extension method 
let private GetArgInfosOfMember isCSharpExt g (vref:ValRef) = 
    if isCSharpExt then 
        let _,curriedArgInfos,_,_ = GetTopValTypeInCompiledForm g vref.ValReprInfo.Value vref.Type vref.Range
        [ curriedArgInfos.Head.Tail ]
    else
        ArgInfosOfMember  g vref

/// Combine the type instantiation and generic method instantiation
let private CombineMethInsts ttps mtps tinst minst = (mkTyparInst ttps tinst @ mkTyparInst mtps minst) 

/// Work out the instantiation relevant to interpret the backing metadata for a member.
///
/// The 'minst' is the instantiation of any generic method type parameters (this instantiation is
/// not included in the MethInfo objects, but carreid separately).
let private GetInstantiationForMemberVal g isCSharpExt (typ,vref,minst) = 
    let memberParentTypars,memberMethodTypars,_retTy,parentTyArgs = AnalyzeTypeOfMemberVal isCSharpExt g (typ,vref)
    CombineMethInsts memberParentTypars memberMethodTypars parentTyArgs minst

/// Work out the instantiation relevant to interpret the backing metadata for a property.
let private GetInstantiationForPropertyVal g (typ,vref) = 
    let memberParentTypars,memberMethodTypars,_retTy,parentTyArgs = AnalyzeTypeOfMemberVal false g (typ,vref)
    CombineMethInsts memberParentTypars memberMethodTypars parentTyArgs (generalizeTypars memberMethodTypars)

/// Describes the sequence order of the introduction of an extension method. Extension methods that are introduced
/// later through 'open' get priority in overload resolution.
type ExtensionMethodPriority = uint64

//-------------------------------------------------------------------------
// OptionalArgCallerSideValue, OptionalArgInfo

/// The caller-side value for the optional arg, if any 
type OptionalArgCallerSideValue = 
    | Constant of IL.ILFieldInit
    | DefaultValue
    | MissingValue
    | WrapperForIDispatch 
    | WrapperForIUnknown
    | PassByRef of TType * OptionalArgCallerSideValue
    
/// Represents information about a parameter indicating if it is optional.
type OptionalArgInfo = 
    /// The argument is not optional
    | NotOptional
    /// The argument is optional, and is an F# callee-side optional arg 
    | CalleeSide
    /// The argument is optional, and is a caller-side .NET optional or default arg.
    /// Note this is correctly termed caller side, even though the default value is optically specified on the callee:
    /// in fact the default value is read from the metadata and passed explicitly to the callee on the caller side.
    | CallerSide of OptionalArgCallerSideValue 
    member x.IsOptional = match x with CalleeSide | CallerSide  _ -> true | NotOptional -> false 

    /// Compute the OptionalArgInfo for an IL parameter
    ///
    /// This includes the Visual Basic rules for IDispatchConstant and IUnknownConstant and optional arguments.
    static member FromILParameter g amap m ilScope ilTypeInst (ilParam: ILParameter) = 
        if ilParam.IsOptional then 
            match ilParam.Default with 
            | None -> 
                // Do a type-directed analysis of the IL type to determine the default value to pass.
                // The same rules as Visual Basic are applied here.
                let rec analyze ty = 
                    if isByrefTy g ty then 
                        let ty = destByrefTy g ty
                        PassByRef (ty, analyze ty)
                    elif isObjTy g ty then
                        match ilParam.Marshal with
                        | Some(ILNativeType.IUnknown | ILNativeType.IDispatch | ILNativeType.Interface) -> Constant(ILFieldInit.Null)
                        | _ -> 
                            if   TryFindILAttributeOpt g.attrib_IUnknownConstantAttribute ilParam.CustomAttrs then WrapperForIUnknown
                            elif TryFindILAttributeOpt g.attrib_IDispatchConstantAttribute ilParam.CustomAttrs then WrapperForIDispatch
                            else MissingValue
                    else 
                        DefaultValue
                CallerSide (analyze (ImportILTypeFromMetadata amap m ilScope ilTypeInst [] ilParam.Type))
            | Some v -> 
                CallerSide (Constant v)
        else 
            NotOptional
    
    static member ValueOfDefaultParameterValueAttrib (Attrib (_,_,exprs,_,_,_,_)) =
        let (AttribExpr (_,defaultValueExpr)) = List.head exprs
        match defaultValueExpr with
        | Expr.Const (_,_,_) -> Some defaultValueExpr
        | _ -> None
    static member FieldInitForDefaultParameterValueAttrib attrib =
        match OptionalArgInfo.ValueOfDefaultParameterValueAttrib attrib with
        | Some (Expr.Const (ConstToILFieldInit fi,_,_)) -> Some fi
        | _ -> None

type CallerInfoInfo =
    | NoCallerInfo
    | CallerLineNumber
    | CallerMemberName
    | CallerFilePath

    override x.ToString() = sprintf "%+A" x

[<RequireQualifiedAccess>]
type ReflectedArgInfo = 
    | None 
    | Quote of bool 
    member x.AutoQuote = match x with None -> false | Quote _ -> true

//-------------------------------------------------------------------------
// ParamNameAndType, ParamData

[<NoComparison; NoEquality>]
/// Partial information about a parameter returned for use by the Language Service
type ParamNameAndType = 
    | ParamNameAndType of Ident option * TType

    static member FromArgInfo (ty,argInfo : ArgReprInfo) = ParamNameAndType(argInfo.Name, ty)
    static member FromMember isCSharpExtMem g vref = GetArgInfosOfMember isCSharpExtMem g vref |> List.mapSquared ParamNameAndType.FromArgInfo
    static member Instantiate inst p = let (ParamNameAndType(nm,ty)) = p in ParamNameAndType(nm, instType inst ty)
    static member InstantiateCurried inst paramTypes = paramTypes |> List.mapSquared (ParamNameAndType.Instantiate inst)

[<NoComparison; NoEquality>]
/// Full information about a parameter returned for use by the type checker and language service.
type ParamData = 
    /// ParamData(isParamArray, isOut, optArgInfo, callerInfoInfo, nameOpt, reflArgInfo, ttype)
    ParamData of bool * bool * OptionalArgInfo * CallerInfoInfo * Ident option * ReflectedArgInfo * TType


//-------------------------------------------------------------------------
// Helper methods associated with type providers

#if EXTENSIONTYPING

type ILFieldInit with 
    /// Compute the ILFieldInit for the given provided constant value for a provided enum type.
    static member FromProvidedObj m (v:obj) = 
        match v with
        | null -> ILFieldInit.Null
        | _ ->
            let objTy = v.GetType()
            let v = if objTy.IsEnum then objTy.GetField("value__").GetValue(v) else v
            match v with 
            | :? single as i -> ILFieldInit.Single i
            | :? double as i -> ILFieldInit.Double i
            | :? bool as i -> ILFieldInit.Bool i
            | :? char as i -> ILFieldInit.Char (uint16 i)
            | :? string as i -> ILFieldInit.String i
            | :? sbyte as i -> ILFieldInit.Int8 i
            | :? byte as i -> ILFieldInit.UInt8 i
            | :? int16 as i -> ILFieldInit.Int16 i
            | :? uint16 as i -> ILFieldInit.UInt16 i
            | :? int as i -> ILFieldInit.Int32 i
            | :? uint32 as i -> ILFieldInit.UInt32 i
            | :? int64 as i -> ILFieldInit.Int64 i
            | :? uint64 as i -> ILFieldInit.UInt64 i
            | _ -> error(Error(FSComp.SR.infosInvalidProvidedLiteralValue(try v.ToString() with _ -> "?"),m))


/// Compute the OptionalArgInfo for a provided parameter. 
///
/// This is the same logic as OptionalArgInfoOfILParameter except we do not apply the 
/// Visual Basic rules for IDispatchConstant and IUnknownConstant to optional 
/// provided parameters.
let OptionalArgInfoOfProvidedParameter (amap:Import.ImportMap) m (provParam : Tainted<ProvidedParameterInfo>) = 
    let g = amap.g
    if provParam.PUntaint((fun p -> p.IsOptional),m) then 
        match provParam.PUntaint((fun p ->  p.HasDefaultValue),m) with 
        | false -> 
            // Do a type-directed analysis of the IL type to determine the default value to pass.
            let rec analyze ty = 
                if isByrefTy g ty then 
                    let ty = destByrefTy g ty
                    PassByRef (ty, analyze ty)
                elif isObjTy g ty then MissingValue
                else  DefaultValue

            let pty = Import.ImportProvidedType amap m (provParam.PApply((fun p -> p.ParameterType),m))
            CallerSide (analyze pty)
        | _ -> 
            let v = provParam.PUntaint((fun p ->  p.RawDefaultValue),m)
            CallerSide (Constant (ILFieldInit.FromProvidedObj m v))
    else 
        NotOptional

/// Compute the ILFieldInit for the given provided constant value for a provided enum type.
let GetAndSanityCheckProviderMethod m (mi: Tainted<'T :> ProvidedMemberInfo>) (get : 'T -> ProvidedMethodInfo) err = 
    match mi.PApply((fun mi -> (get mi :> ProvidedMethodBase)),m) with 
    | Tainted.Null -> error(Error(err(mi.PUntaint((fun mi -> mi.Name),m),mi.PUntaint((fun mi -> mi.DeclaringType.Name),m)),m))   
    | meth -> meth

/// Try to get an arbitrary ProvidedMethodInfo associated with a property.
let ArbitraryMethodInfoOfPropertyInfo (pi:Tainted<ProvidedPropertyInfo>) m =
    if pi.PUntaint((fun pi -> pi.CanRead), m) then 
        GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetGetMethod()) FSComp.SR.etPropertyCanReadButHasNoGetter
    elif pi.PUntaint((fun pi -> pi.CanWrite), m) then 
        GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetSetMethod()) FSComp.SR.etPropertyCanWriteButHasNoSetter
    else 
        error(Error(FSComp.SR.etPropertyNeedsCanWriteOrCanRead(pi.PUntaint((fun mi -> mi.Name),m),pi.PUntaint((fun mi -> mi.DeclaringType.Name),m)),m))   

#endif


//-------------------------------------------------------------------------
// ILTypeInfo

/// Describes an F# use of an IL type, including the type instantiation associated with the type at a particular usage point.
///
/// This is really just 1:1 with the subset ot TType which result from building types using IL type definitions.
[<NoComparison; NoEquality>]
type ILTypeInfo = 
    /// ILTypeInfo (tyconRef, ilTypeRef, typeArgs, ilTypeDef).
    | ILTypeInfo of TyconRef * ILTypeRef * TypeInst * ILTypeDef

    member x.TyconRef    = let (ILTypeInfo(tcref,_,_,_)) = x in tcref
    member x.ILTypeRef   = let (ILTypeInfo(_,tref,_,_))  = x in tref
    member x.TypeInst    = let (ILTypeInfo(_,_,tinst,_)) = x in tinst
    member x.RawMetadata = let (ILTypeInfo(_,_,_,tdef))  = x in tdef
    member x.ToType   = TType_app(x.TyconRef,x.TypeInst)
    member x.ILScopeRef = x.ILTypeRef.Scope
    member x.Name     = x.ILTypeRef.Name
    member x.IsValueType = x.RawMetadata.IsStructOrEnum
    member x.Instantiate inst = 
        let (ILTypeInfo(tcref,tref,tinst,tdef)) = x 
        ILTypeInfo(tcref,tref,instTypes inst tinst,tdef)

    member x.FormalTypars m = x.TyconRef.Typars m

    static member FromType g ty = 
        if isILAppTy g ty then 
            let tcref,tinst = destAppTy g ty
            let (TILObjectReprData(scoref,enc,tdef)) = tcref.ILTyconInfo
            let tref = mkRefForNestedILTypeDef scoref (enc,tdef)
            ILTypeInfo(tcref,tref,tinst,tdef)
        else 
            failwith "ILTypeInfo.FromType"

//-------------------------------------------------------------------------
// ILMethInfo


/// Describes an F# use of an IL method.
[<NoComparison; NoEquality>]
type ILMethInfo =
    /// ILMethInfo(g, ilApparentType, ilDeclaringTyconRefOpt, ilMethodDef, ilGenericMethodTyArgs)
    ///	
    /// Describes an F# use of an IL method. 
    ///
    /// If ilDeclaringTyconRefOpt is 'Some' then this is an F# use of an C#-style extension method.
    /// If ilDeclaringTyconRefOpt is 'None' then ilApparentType is an IL type definition.
    | ILMethInfo of TcGlobals * TType * TyconRef option  * ILMethodDef * Typars  

    member x.TcGlobals = match x with ILMethInfo(g,_,_,_,_) -> g

    /// Get the apparent declaring type of the method as an F# type. 
    /// If this is an C#-style extension method then this is the type which the method 
    /// appears to extend. This may be a variable type.
    member x.ApparentEnclosingType = match x with ILMethInfo(_,ty,_,_,_) -> ty

    /// Get the declaring type associated with an extension member, if any.
    member x.DeclaringTyconRefOption = match x with ILMethInfo(_,_,tcrefOpt,_,_) -> tcrefOpt

    /// Get the Abstract IL metadata associated with the method.
    member x.RawMetadata = match x with ILMethInfo(_,_,_,md,_) -> md 

    /// Get the formal method type parameters associated with a method.
    member x.FormalMethodTypars = match x with ILMethInfo(_,_,_,_,fmtps) -> fmtps

    /// Get the IL name of the method
    member x.ILName       = x.RawMetadata.Name

    /// Indicates if the method is an extension method
    member x.IsILExtensionMethod = x.DeclaringTyconRefOption.IsSome

    /// Get the declaring type of the method. If this is an C#-style extension method then this is the IL type
    /// holding the static member that is the extension method.
    member x.DeclaringTyconRef   = 
        match x.DeclaringTyconRefOption with 
        | Some tcref -> tcref 
        | None -> tcrefOfAppTy x.TcGlobals x.ApparentEnclosingType

    /// Get the instantiation of the declaring type of the method. 
    /// If this is an C#-style extension method then this is empty because extension members
    /// are never in generic classes.
    member x.DeclaringTypeInst   = 
        if x.IsILExtensionMethod then [] else argsOfAppTy x.TcGlobals x.ApparentEnclosingType

    /// Get the Abstract IL scope information associated with interpreting the Abstract IL metadata that backs this method.
    member x.MetadataScope   = x.DeclaringTyconRef.CompiledRepresentationForNamedType.Scope
    
    /// Get the Abstract IL metadata corresponding to the parameters of the method. 
    /// If this is an C#-style extension method then drop the object argument.
    member x.ParamMetadata = 
        let ps = x.RawMetadata.Parameters
        if x.IsILExtensionMethod then List.tail ps else ps

    /// Get the number of parameters of the method
    member x.NumParams = x.ParamMetadata.Length
    
    /// Indicates if the method is a constructor
    member x.IsConstructor = x.RawMetadata.IsConstructor 

    /// Indicates if the method is a class initializer.
    member x.IsClassConstructor = x.RawMetadata.IsClassInitializer

    /// Indicates if the method has protected accessibility,
    member x.IsProtectedAccessibility = 
        let md = x.RawMetadata 
        not md.IsConstructor &&
        not md.IsClassInitializer &&
        (md.Access = ILMemberAccess.Family || md.Access = ILMemberAccess.FamilyOrAssembly)

    /// Indicates if the IL method is marked virtual.
    member x.IsVirtual = x.RawMetadata.IsVirtual

    /// Indicates if the IL method is marked final.
    member x.IsFinal = x.RawMetadata.IsFinal

    /// Indicates if the IL method is marked abstract.
    member x.IsAbstract = 
        match x.RawMetadata.mdKind with 
        | MethodKind.Virtual vinfo -> vinfo.IsAbstract 
        | _ -> false

    /// Does it appear to the user as a static method?
    member x.IsStatic = 
        not x.IsILExtensionMethod &&  // all C#-declared extension methods are instance
        x.RawMetadata.CallingConv.IsStatic

    /// Does it have the .NET IL 'newslot' flag set, and is also a virtual?
    member x.IsNewSlot = 
        match x.RawMetadata.mdKind with 
        | MethodKind.Virtual vinfo -> vinfo.IsNewSlot 
        | _ -> false
    
    /// Does it appear to the user as an instance method?
    member x.IsInstance = not x.IsConstructor &&  not x.IsStatic

    /// Get the argument types of the the IL method. If this is an C#-style extension method 
    /// then drop the object argument.
    member x.GetParamTypes(amap,m,minst) = 
        x.ParamMetadata |> List.map (fun p -> ImportILTypeFromMetadata amap m x.MetadataScope x.DeclaringTypeInst minst p.Type) 

    /// Get all the argument types of the IL method. Include the object argument even if this is 
    /// an C#-style extension method.
    member x.GetRawArgTypes(amap,m,minst) = 
        x.RawMetadata.Parameters |> List.map (fun p -> ImportILTypeFromMetadata amap m x.MetadataScope x.DeclaringTypeInst minst p.Type) 

    /// Get info about the arguments of the IL method. If this is an C#-style extension method then 
    /// drop the object argument.
    member x.GetParamNamesAndTypes(amap,m,minst) = 
        x.ParamMetadata |> List.map (fun p -> ParamNameAndType(Option.map (mkSynId m) p.Name, ImportILTypeFromMetadata amap m x.MetadataScope x.DeclaringTypeInst minst p.Type) )

    /// Get a reference to the method (dropping all generic instantiations), as an Abstract IL ILMethodRef.
    member x.ILMethodRef = 
        let mref = mkRefToILMethod (x.DeclaringTyconRef.CompiledRepresentationForNamedType,x.RawMetadata)
        rescopeILMethodRef x.MetadataScope mref 

    /// Indicates if the method is marked as a DllImport (a PInvoke). This is done by looking at the IL custom attributes on 
    /// the method.
    member x.IsDllImport (g: TcGlobals) = 
        match g.attrib_DllImportAttribute with
        | None -> false
        | Some (AttribInfo(tref,_)) ->x.RawMetadata.CustomAttrs |> TryDecodeILAttribute g tref |> Option.isSome

    /// Get the (zero or one) 'self'/'this'/'object' arguments associated with an IL method. 
    /// An instance extension method returns one object argument.
    member x.GetObjArgTypes(amap, m, minst) =
        // All C#-style extension methods are instance. We have to re-read the 'obj' type w.r.t. the
        // method instantiation.
        if x.IsILExtensionMethod then
            [ImportILTypeFromMetadata amap m x.MetadataScope x.DeclaringTypeInst minst x.RawMetadata.Parameters.Head.Type]
        else if x.IsInstance then 
            [ x.ApparentEnclosingType ]
        else
            []

    /// Get the compiled return type of the method, where 'void' is None.
    member x.GetCompiledReturnTy (amap, m, minst) =
        ImportReturnTypeFromMetaData amap m x.RawMetadata.Return.Type x.MetadataScope x.DeclaringTypeInst minst 

    /// Get the F# view of the return type of the method, where 'void' is 'unit'.
    member x.GetFSharpReturnTy (amap, m, minst) = 
        x.GetCompiledReturnTy(amap, m, minst)
        |> GetFSharpViewOfReturnType amap.g

//-------------------------------------------------------------------------
// MethInfo


#if DEBUG
[<System.Diagnostics.DebuggerDisplayAttribute("{DebuggerDisplayName}")>]
#endif
/// Describes an F# use of a method
[<NoComparison; NoEquality>]
type MethInfo = 
    /// FSMeth(tcGlobals, declaringType, valRef, extensionMethodPriority).
    ///
    /// Describes a use of a method declared in F# code and backed by F# metadata.
    | FSMeth of TcGlobals * TType * ValRef  * ExtensionMethodPriority option

    /// ILMeth(tcGlobals, ilMethInfo, extensionMethodPriority).
    ///
    /// Describes a use of a method backed by Abstract IL # metadata
    | ILMeth of TcGlobals * ILMethInfo * ExtensionMethodPriority option

    /// Describes a use of a pseudo-method corresponding to the default constructor for a .NET struct type
    | DefaultStructCtor of TcGlobals * TType

#if EXTENSIONTYPING
    /// Describes a use of a method backed by provided metadata
    | ProvidedMeth of Import.ImportMap * Tainted<ProvidedMethodBase> * ExtensionMethodPriority option  * range
#endif

    /// Get the enclosing type of the method info. 
    ///
    /// If this is an extension member, then this is the apparent parent, i.e. the type the method appears to extend.
    /// This may be a variable type.
    member x.EnclosingType = 
        match x with
        | ILMeth(_g,ilminfo,_) -> ilminfo.ApparentEnclosingType
        | FSMeth(_g,typ,_,_) -> typ
        | DefaultStructCtor(_g,typ) -> typ
#if EXTENSIONTYPING
        | ProvidedMeth(amap,mi,_,m) -> 
              Import.ImportProvidedType amap m (mi.PApply((fun mi -> mi.DeclaringType),m))
#endif

    /// Get the declaring type or module holding the method. If this is an C#-style extension method then this is the type
    /// holding the static member that is the extension method. If this is an F#-style extension method it is the logical module
    /// holding the value for the extension method.
    member x.DeclaringEntityRef   = 
        match x with 
        | ILMeth(_,ilminfo,_) when x.IsExtensionMember  -> ilminfo.DeclaringTyconRef
        | FSMeth(_,_,vref,_) when x.IsExtensionMember -> vref.TopValActualParent
        | _ -> tcrefOfAppTy x.TcGlobals x.EnclosingType 

    /// Get the information about provided static parameters, if any 
    member x.ProvidedStaticParameterInfo = 
        match x with
        | ILMeth _ -> None
        | FSMeth _  -> None
#if EXTENSIONTYPING
        | ProvidedMeth (_, mb, _, m) -> 
            let staticParams = mb.PApplyWithProvider((fun (mb,provider) -> mb.GetStaticParametersForMethod(provider)), range=m) 
            let staticParams = staticParams.PApplyArray(id, "GetStaticParametersForMethod", m)
            match staticParams with 
            | [| |] -> None
            | _ -> Some (mb,staticParams)
#endif
        | DefaultStructCtor _ -> None


    /// Get the extension method priority of the method, if it has one.
    member x.ExtensionMemberPriorityOption = 
        match x with
        | ILMeth(_,_,pri) -> pri
        | FSMeth(_,_,_,pri) -> pri
#if EXTENSIONTYPING
        | ProvidedMeth(_,_,pri,_) -> pri
#endif
        | DefaultStructCtor _ -> None

     /// Get the extension method priority of the method. If it is not an extension method
     /// then use the highest possible value since non-extension methods always take priority
     /// over extension members.
    member x.ExtensionMemberPriority = defaultArg x.ExtensionMemberPriorityOption System.UInt64.MaxValue 

#if DEBUG
     /// Get the method name in DebuggerDisplayForm
    member x.DebuggerDisplayName = 
        match x with 
        | ILMeth(_,y,_) -> "ILMeth: " + y.ILName
        | FSMeth(_,_,vref,_) -> "FSMeth: " + vref.LogicalName
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> "ProvidedMeth: " + mi.PUntaint((fun mi -> mi.Name),m)
#endif
        | DefaultStructCtor _ -> ".ctor"
#endif

     /// Get the method name in LogicalName form, i.e. the name as it would be stored in .NET metadata
    member x.LogicalName = 
        match x with 
        | ILMeth(_,y,_) -> y.ILName
        | FSMeth(_,_,vref,_) -> vref.LogicalName
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.Name),m)
#endif
        | DefaultStructCtor _ -> ".ctor"

     /// Get the method name in DisplayName form
    member x.DisplayName = 
        match x with 
        | FSMeth(_,_,vref,_) -> vref.DisplayName
        | _ -> x.LogicalName

     /// Indicates if this is a method defined in this assembly with an internal XML comment
    member x.HasDirectXmlComment =
        match x with
        | FSMeth(g,_,vref,_) -> valRefInThisAssembly g.compilingFslib vref
#if EXTENSIONTYPING
        | ProvidedMeth _ -> true
#endif
        | _ -> false

    override x.ToString() =  x.EnclosingType.ToString() + x.LogicalName

    /// Get the actual type instantiation of the declaring type associated with this use of the method.
    /// 
    /// For extension members this is empty (the instantiation of the declaring type). 
    member x.DeclaringTypeInst = 
        if x.IsExtensionMember then [] else argsOfAppTy x.TcGlobals x.EnclosingType

    /// Get the TcGlobals value that governs the method declaration
    member x.TcGlobals = 
        match x with 
        | ILMeth(g,_,_) -> g
        | FSMeth(g,_,_,_) -> g
        | DefaultStructCtor (g,_) -> g
#if EXTENSIONTYPING
        | ProvidedMeth(amap,_,_,_) -> amap.g
#endif

    /// Get the formal generic method parameters for the method as a list of type variables.
    ///
    /// For an extension method this includes all type parameters, even if it is extending a generic type.
    member x.FormalMethodTypars = 
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.FormalMethodTypars
        | FSMeth(g,typ,vref,_) ->  
            let _,memberMethodTypars,_,_ = AnalyzeTypeOfMemberVal x.IsCSharpStyleExtensionMember g (typ,vref)
            memberMethodTypars
        | DefaultStructCtor _ -> []
#if EXTENSIONTYPING
        | ProvidedMeth _ -> [] // There will already have been an error if there are generic parameters here.
#endif
           
     /// Get the formal generic method parameters for the method as a list of variable types.
    member x.FormalMethodInst = generalizeTypars x.FormalMethodTypars

     /// Get the XML documentation associated with the method
    member x.XmlDoc = 
        match x with 
        | ILMeth(_,_,_) -> XmlDoc.Empty
        | FSMeth(_,_,vref,_) -> vref.XmlDoc
        | DefaultStructCtor _ -> XmlDoc.Empty
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m)-> 
            XmlDoc (mi.PUntaint((fun mix -> (mix :> IProvidedCustomAttributeProvider).GetXmlDocAttributes(mi.TypeProvider.PUntaintNoFailure(id))),m))
#endif

    /// Try to get an arbitrary F# ValRef associated with the member. This is to determine if the member is virtual, amongst other things.
    member x.ArbitraryValRef = 
        match x with 
        | FSMeth(_g,_,vref,_) -> Some vref
        | _ -> None

    /// Get a list of argument-number counts, one count for each set of curried arguments.
    ///
    /// For an extension member, drop the 'this' argument.
    member x.NumArgs = 
        match x with 
        | ILMeth(_,ilminfo,_) -> [ilminfo.NumParams]
        | FSMeth(g,_,vref,_) -> GetArgInfosOfMember x.IsCSharpStyleExtensionMember g vref |> List.map List.length 
        | DefaultStructCtor _ -> [0]
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> [mi.PUntaint((fun mi -> mi.GetParameters().Length),m)] // Why is this a list? Answer: because the method might be curried
#endif

    member x.IsCurried = x.NumArgs.Length > 1

    /// Does the method appear to the user as an instance method?
    member x.IsInstance = 
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsInstance
        | FSMeth(_,_,vref,_) -> vref.IsInstanceMember || x.IsCSharpStyleExtensionMember
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> not mi.IsConstructor && not mi.IsStatic),m)
#endif


    /// Get the number of generic method parameters for a method.
    /// For an extension method this includes all type parameters, even if it is extending a generic type.
    member x.GenericArity =  x.FormalMethodTypars.Length

    member x.IsProtectedAccessiblity = 
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsProtectedAccessibility
        | FSMeth _ -> false
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsFamily), m)
#endif

    member x.IsVirtual =
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsVirtual
        | FSMeth(_,_,vref,_) -> vref.IsVirtualMember
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsVirtual), m)
#endif

    member x.IsConstructor = 
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsConstructor
        | FSMeth(_g,_,vref,_) -> (vref.MemberInfo.Value.MemberFlags.MemberKind = MemberKind.Constructor)
        | DefaultStructCtor _ -> true
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsConstructor), m)
#endif

    member x.IsClassConstructor =
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsClassConstructor
        | FSMeth(_,_,vref,_) -> 
             match vref.TryDeref with
             | VSome x -> x.IsClassConstructor
             | _ -> false
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsConstructor && mi.IsStatic), m) // Note: these are never public anyway
#endif

    member x.IsDispatchSlot = 
        match x with 
        | ILMeth(_g,ilmeth,_) -> ilmeth.IsVirtual
        | FSMeth(g,_,vref,_) as x -> 
            isInterfaceTy g x.EnclosingType  || 
            vref.MemberInfo.Value.MemberFlags.IsDispatchSlot
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth _ -> x.IsVirtual // Note: follow same implementation as ILMeth
#endif


    member x.IsFinal = 
        not x.IsVirtual || 
        match x with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsFinal
        | FSMeth(_g,_,_vref,_) -> false
        | DefaultStructCtor _ -> true
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsFinal), m)
#endif

    // This means 'is this particular MethInfo one that doesn't provide an implementation?'.
    // For F# methods, this is 'true' for the MethInfos corresponding to 'abstract' declarations, 
    // and false for the (potentially) matching 'default' implementation MethInfos that eventually
    // provide an implementation for the dispatch slot.
    //
    // For IL methods, this is 'true' for abstract methods, and 'false' for virtual methods
    member minfo.IsAbstract = 
        match minfo with 
        | ILMeth(_,ilmeth,_) -> ilmeth.IsAbstract
        | FSMeth(g,_,vref,_)  -> isInterfaceTy g minfo.EnclosingType  || vref.IsDispatchSlotMember
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsAbstract), m)
#endif

    member x.IsNewSlot = 
        isInterfaceTy x.TcGlobals x.EnclosingType  || 
        (x.IsVirtual && 
          (match x with 
           | ILMeth(_,x,_) -> x.IsNewSlot
           | FSMeth(_,_,vref,_) -> vref.IsDispatchSlotMember
#if EXTENSIONTYPING
           | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsHideBySig), m) // REVIEW: Check this is correct
#endif
           | DefaultStructCtor _ -> false))

    /// Check if this method is an explicit implementation of an interface member
    member x.IsFSharpExplicitInterfaceImplementation = 
        match x with 
        | ILMeth _ -> false
        | FSMeth(g,_,vref,_) -> vref.IsFSharpExplicitInterfaceImplementation g
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth _ -> false 
#endif

    /// Check if this method is marked 'override' and thus definitely overrides another method.
    member x.IsDefiniteFSharpOverride = 
        match x with 
        | ILMeth _ -> false
        | FSMeth(_,_,vref,_) -> vref.IsDefiniteFSharpOverrideMember
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth _ -> false 
#endif

    member x.ImplementedSlotSignatures =
        match x with 
        | FSMeth(_,_,vref,_) -> vref.ImplementedSlotSignatures
        | _ -> failwith "not supported"

    /// Indicates if this is an extension member. 
    member x.IsExtensionMember = x.IsCSharpStyleExtensionMember || x.IsFSharpStyleExtensionMember

    /// Indicates if this is an F# extension member. 
    member x.IsFSharpStyleExtensionMember = 
        match x with FSMeth (_,_,vref,_) -> vref.IsExtensionMember | _ -> false

    /// Indicates if this is an C#-style extension member. 
    member x.IsCSharpStyleExtensionMember = 
        x.ExtensionMemberPriorityOption.IsSome && 
        (match x with ILMeth _ -> true | FSMeth (_,_,vref,_) -> not vref.IsExtensionMember | _ -> false)

    /// Add the actual type instantiation of the apparent type of an F# extension method.
    //
    // When an explicit type instantiation is given for an F# extension members the type
    // arguments implied by the object type are not given in source code. This means we must
    // add them explicitly. For example
    //    type List<'T> with 
    //        member xs.Map<'U>(f : 'T -> 'U) = ....
    // is called as
    //    xs.Map<int>
    // but is compiled as a generic methods with two type arguments
    //     Map<'T,'U>(this: List<'T>, f : 'T -> 'U)
    member x.AdjustUserTypeInstForFSharpStyleIndexedExtensionMembers(tyargs) =  
        (if x.IsFSharpStyleExtensionMember then argsOfAppTy x.TcGlobals x.EnclosingType else []) @ tyargs 

    /// Indicates if this method is a generated method associated with an F# CLIEvent property compiled as a .NET event
    member x.IsFSharpEventPropertyMethod = 
        match x with 
        | FSMeth(g,_,vref,_)  -> vref.IsFSharpEventProperty(g)
#if EXTENSIONTYPING
        | ProvidedMeth _ -> false 
#endif
        | _ -> false

    /// Indicates if this method takes no arguments
    member x.IsNullary = (x.NumArgs = [0])

    /// Indicates if the enclosing type for the method is a value type. 
    ///
    /// For an extension method, this indicates if the method extends a struct type.
    member x.IsStruct = 
        isStructTy x.TcGlobals x.EnclosingType

    /// Build IL method infos.  
    static member CreateILMeth (amap:Import.ImportMap, m, typ:TType, md: ILMethodDef) =     
        let tinfo = ILTypeInfo.FromType amap.g typ
        let mtps = Import.ImportILGenericParameters (fun () -> amap) m tinfo.ILScopeRef tinfo.TypeInst md.GenericParams
        ILMeth (amap.g,ILMethInfo(amap.g,tinfo.ToType,None,md,mtps),None)

    /// Build IL method infos for a C#-style extension method
    static member CreateILExtensionMeth (amap, m, apparentTy:TType, declaringTyconRef:TyconRef, extMethPri, md: ILMethodDef) =     
        let scoref =  declaringTyconRef.CompiledRepresentationForNamedType.Scope
        let mtps = Import.ImportILGenericParameters (fun () -> amap) m scoref [] md.GenericParams
        ILMeth (amap.g,ILMethInfo(amap.g,apparentTy,Some declaringTyconRef,md,mtps),extMethPri)

    /// Tests whether two method infos have the same underlying definition.
    /// Used to merge operator overloads collected from left and right of an operator constraint.
    static member MethInfosUseIdenticalDefinitions x1 x2 = 
        match x1,x2 with 
        | ILMeth(_,x1,_), ILMeth(_,x2,_) -> (x1.RawMetadata ===  x2.RawMetadata)
        | FSMeth(g,_,vref1,_), FSMeth(_,_,vref2,_)  -> valRefEq g vref1 vref2 
        | DefaultStructCtor(g,ty1), DefaultStructCtor(_,ty2) -> tyconRefEq g (tcrefOfAppTy g ty1) (tcrefOfAppTy g ty2) 
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi1,_,_),ProvidedMeth(_,mi2,_,_)  -> ProvidedMethodBase.TaintedEquals (mi1, mi2)
#endif
        | _ -> false

    /// Calculates a hash code of method info. Note: this is a very imperfect implementation,
    /// but it works decently for comparing methods in the language service...
    member x.ComputeHashCode() = 
        match x with 
        | ILMeth(_,x1,_) -> hash x1.RawMetadata.Name
        | FSMeth(_,_,vref,_) -> hash vref.LogicalName
        | DefaultStructCtor(_,_ty) -> 34892 // "ty" doesn't support hashing. We could use "hash (tcrefOfAppTy g ty).CompiledName" or 
                                           // something but we don't have a "g" parameter here yet. But this hash need only be very approximate anyway
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,_) -> ProvidedMethodInfo.TaintedGetHashCode(mi)
#endif

    /// Apply a type instantiation to a method info, i.e. apply the instantiation to the enclosing type. 
    member x.Instantiate(amap, m, inst) = 
        match x with 
        | ILMeth(_g,ilminfo,pri) ->
            match ilminfo with 
            | ILMethInfo(_,typ,None,md,_) -> MethInfo.CreateILMeth(amap, m, instType inst typ, md) 
            | ILMethInfo(_,typ,Some declaringTyconRef,md,_) -> MethInfo.CreateILExtensionMeth(amap, m, instType inst typ, declaringTyconRef, pri, md) 
        | FSMeth(g,typ,vref,pri) -> FSMeth(g,instType inst typ,vref,pri)
        | DefaultStructCtor(g,typ) -> DefaultStructCtor(g,instType inst typ)
#if EXTENSIONTYPING
        | ProvidedMeth _ -> 
            match inst with 
            | [] -> x
            | _ -> assert false; failwith "Not supported" 
#endif

    /// Get the return type of a method info, where 'void' is returned as 'None'
    member x.GetCompiledReturnTy (amap, m, minst) = 
        match x with 
        | ILMeth(_g,ilminfo,_) -> 
            ilminfo.GetCompiledReturnTy(amap, m, minst)
        | FSMeth(g,typ,vref,_) -> 
            let inst = GetInstantiationForMemberVal g x.IsCSharpStyleExtensionMember (typ,vref,minst)
            let _,_,retTy,_ = AnalyzeTypeOfMemberVal x.IsCSharpStyleExtensionMember g (typ,vref)
            retTy |> Option.map (instType inst)
        | DefaultStructCtor _ -> None
#if EXTENSIONTYPING
        | ProvidedMeth(amap,mi,_,m) -> 
            GetCompiledReturnTyOfProvidedMethodInfo amap m mi
#endif

    /// Get the return type of a method info, where 'void' is returned as 'unit'
    member x.GetFSharpReturnTy(amap, m, minst) =
        x.GetCompiledReturnTy(amap, m, minst) |> GetFSharpViewOfReturnType amap.g
       
    /// Get the parameter types of a method info
    member x.GetParamTypes(amap, m, minst) = 
        match x with 
        | ILMeth(_g,ilminfo,_) -> 
            // A single group of tupled arguments
            [ ilminfo.GetParamTypes(amap,m,minst) ]
        | FSMeth(g,typ,vref,_) -> 
            let paramTypes = ParamNameAndType.FromMember x.IsCSharpStyleExtensionMember g vref
            let inst = GetInstantiationForMemberVal g x.IsCSharpStyleExtensionMember (typ,vref,minst)
            paramTypes |> List.mapSquared (fun (ParamNameAndType(_,ty)) -> instType inst ty) 
        | DefaultStructCtor _ -> []
#if EXTENSIONTYPING
        | ProvidedMeth(amap,mi,_,m) -> 
            // A single group of tupled arguments
            [ [ for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters",m) do
                    yield Import.ImportProvidedType amap m (p.PApply((fun p -> p.ParameterType),m)) ] ]
#endif

    /// Get the (zero or one) 'self'/'this'/'object' arguments associated with a method.
    /// An instance method returns one object argument.
    member x.GetObjArgTypes (amap, m, minst) = 
        match x with 
        | ILMeth(_,ilminfo,_) -> ilminfo.GetObjArgTypes(amap, m, minst)
        | FSMeth(g,typ,vref,_) -> 
            if x.IsInstance then 
                // The 'this' pointer of an extension member can depend on the minst
                if x.IsExtensionMember then 
                    let inst = GetInstantiationForMemberVal g x.IsCSharpStyleExtensionMember (typ,vref,minst)
                    let rawObjTy = GetObjTypeOfInstanceExtensionMethod g vref
                    [ rawObjTy |> instType inst ]
                else
                    [ typ ]
            else []
        | DefaultStructCtor _ -> []
#if EXTENSIONTYPING
        | ProvidedMeth(amap,mi,_,m) -> 
            if x.IsInstance then [ Import.ImportProvidedType amap m (mi.PApply((fun mi -> mi.DeclaringType),m)) ] // find the type of the 'this' argument
            else []
#endif

    /// Get the parameter attributes of a method info, which get combined with the parameter names and types
    member x.GetParamAttribs(amap, m) = 
        match x with 
        | ILMeth(g,ilMethInfo,_) -> 
            [ [ for p in ilMethInfo.ParamMetadata do
                 let isParamArrayArg = TryFindILAttribute g.attrib_ParamArrayAttribute p.CustomAttrs
                 let reflArgInfo = 
                     match TryDecodeILAttribute g g.attrib_ReflectedDefinitionAttribute.TypeRef p.CustomAttrs with 
                     | Some ([ILAttribElem.Bool b ],_) ->  ReflectedArgInfo.Quote b
                     | Some _ -> ReflectedArgInfo.Quote false
                     | _ -> ReflectedArgInfo.None
                 let isOutArg = (p.IsOut && not p.IsIn)
                 // Note: we get default argument values from VB and other .NET language metadata 
                 let optArgInfo =  OptionalArgInfo.FromILParameter g amap m ilMethInfo.MetadataScope ilMethInfo.DeclaringTypeInst p

                 let isCallerLineNumberArg = TryFindILAttribute g.attrib_CallerLineNumberAttribute p.CustomAttrs
                 let isCallerFilePathArg = TryFindILAttribute g.attrib_CallerFilePathAttribute p.CustomAttrs
                 let isCallerMemberNameArg = TryFindILAttribute g.attrib_CallerMemberNameAttribute p.CustomAttrs

                 let callerInfoInfo =
                    match isCallerLineNumberArg, isCallerFilePathArg, isCallerMemberNameArg with
                    | false, false, false -> NoCallerInfo
                    | true, false, false -> CallerLineNumber
                    | false, true, false -> CallerFilePath
                    | false, false, true -> CallerMemberName
                    | _, _, _ ->
                        // if multiple caller info attributes are specified, pick the "wrong" one here
                        // so that we get an error later
                        if p.Type.TypeRef.FullName = "System.Int32" then CallerFilePath
                        else CallerLineNumber

                 yield (isParamArrayArg, isOutArg, optArgInfo, callerInfoInfo, reflArgInfo) ] ]

        | FSMeth(g,_,vref,_) -> 
            GetArgInfosOfMember x.IsCSharpStyleExtensionMember g vref 
            |> List.mapSquared (fun (ty,argInfo) -> 
                let isParamArrayArg = HasFSharpAttribute g g.attrib_ParamArrayAttribute argInfo.Attribs
                let reflArgInfo = 
                    match TryFindFSharpBoolAttributeAssumeFalse  g g.attrib_ReflectedDefinitionAttribute argInfo.Attribs  with 
                    | Some b -> ReflectedArgInfo.Quote b
                    | None -> ReflectedArgInfo.None
                let isOutArg = HasFSharpAttribute g g.attrib_OutAttribute argInfo.Attribs && isByrefTy g ty
                let isCalleeSideOptArg = HasFSharpAttribute g g.attrib_OptionalArgumentAttribute argInfo.Attribs
                let isCallerSideOptArg = HasFSharpAttributeOpt g g.attrib_OptionalAttribute argInfo.Attribs
                let optArgInfo = 
                    if isCalleeSideOptArg then 
                        CalleeSide 
                    elif isCallerSideOptArg then
                        let defaultParameterValueAttribute = TryFindFSharpAttributeOpt g g.attrib_DefaultParameterValueAttribute argInfo.Attribs
                        match defaultParameterValueAttribute with
                        | None -> 
                            // Do a type-directed analysis of the type to determine the default value to pass.
                            // Similar rules as OptionalArgInfo.FromILParameter are applied here, except for the COM and byref-related stuff.
                            CallerSide (if isObjTy g ty then MissingValue else DefaultValue)
                        | Some attr -> 
                            let defaultValue = OptionalArgInfo.ValueOfDefaultParameterValueAttrib attr
                            match defaultValue with
                            | Some (Expr.Const (_, m, typ)) when not (typeEquiv g typ ty) -> 
                                // the type of the default value does not match the type of the argument.
                                // Emit a warning, and ignore the DefaultParameterValue argument altogether.
                                warning(Error(FSComp.SR.DefaultParameterValueNotAppropriateForArgument(), m))
                                NotOptional
                            | Some (Expr.Const((ConstToILFieldInit fi),_,_)) ->
                                // Good case - all is well.
                                CallerSide (Constant fi)
                            | _ -> 
                                // Default value is not appropriate, i.e. not a constant.
                                // Compiler already gives an error in that case, so just ignore here.
                                NotOptional 
                    else NotOptional

                let isCallerLineNumberArg = HasFSharpAttribute g g.attrib_CallerLineNumberAttribute argInfo.Attribs
                let isCallerFilePathArg = HasFSharpAttribute g g.attrib_CallerFilePathAttribute argInfo.Attribs
                let isCallerMemberNameArg = HasFSharpAttribute g g.attrib_CallerMemberNameAttribute argInfo.Attribs

                let callerInfoInfo =
                    match isCallerLineNumberArg, isCallerFilePathArg, isCallerMemberNameArg with
                    | false, false, false -> NoCallerInfo
                    | true, false, false -> CallerLineNumber
                    | false, true, false -> CallerFilePath
                    | false, false, true -> CallerMemberName
                    | false, true, true -> match TryFindFSharpAttribute g g.attrib_CallerMemberNameAttribute argInfo.Attribs with
                                           | Some(Attrib(_,_,_,_,_,_,callerMemberNameAttributeRange)) -> warning(Error(FSComp.SR.CallerMemberNameIsOverriden(argInfo.Name.Value.idText), callerMemberNameAttributeRange))
                                                                                                         CallerFilePath
                                           | _ -> failwith "Impossible"
                    | _, _, _ ->
                        // if multiple caller info attributes are specified, pick the "wrong" one here
                        // so that we get an error later
                        match tryDestOptionTy g ty with
                        | Some optTy when typeEquiv g g.int32_ty optTy -> CallerFilePath
                        | _ -> CallerLineNumber

                (isParamArrayArg, isOutArg, optArgInfo, callerInfoInfo, reflArgInfo))

        | DefaultStructCtor _ -> 
            [[]]

#if EXTENSIONTYPING
        | ProvidedMeth(amap,mi,_,_) -> 
            // A single group of tupled arguments
            [ [for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters", m) do
                let isParamArrayArg = p.PUntaint((fun px -> (px :> IProvidedCustomAttributeProvider).GetAttributeConstructorArgs(p.TypeProvider.PUntaintNoFailure(id), typeof<System.ParamArrayAttribute>.FullName).IsSome),m)
                let optArgInfo =  OptionalArgInfoOfProvidedParameter amap m p 
                let reflArgInfo = 
                    match p.PUntaint((fun px -> (px :> IProvidedCustomAttributeProvider).GetAttributeConstructorArgs(p.TypeProvider.PUntaintNoFailure(id), typeof<Microsoft.FSharp.Core.ReflectedDefinitionAttribute>.FullName)),m) with
                    | Some ([ Some (:? bool as b) ], _) -> ReflectedArgInfo.Quote b
                    | Some _ -> ReflectedArgInfo.Quote false
                    | None -> ReflectedArgInfo.None
                yield (isParamArrayArg, p.PUntaint((fun p -> p.IsOut), m), optArgInfo, NoCallerInfo, reflArgInfo)] ]
#endif



    /// Get the signature of an abstract method slot.
    //
    // This code has grown organically over time. We've managed to unify the ILMeth+ProvidedMeth paths.
    // The FSMeth, ILMeth+ProvidedMeth paths can probably be unified too.
    member x.GetSlotSig(amap, m) =
        match x with 
        | FSMeth(g,typ,vref,_) -> 
            match vref.RecursiveValInfo with 
            | ValInRecScope(false) -> error(Error((FSComp.SR.InvalidRecursiveReferenceToAbstractSlot()),m))
            | _ -> ()

            let allTyparsFromMethod,_,retTy,_ = GetTypeOfMemberInMemberForm g vref
            // A slot signature is w.r.t. the type variables of the type it is associated with.
            // So we have to rename from the member type variables to the type variables of the type.
            let formalEnclosingTypars = (tcrefOfAppTy g typ).Typars(m)
            let formalEnclosingTyparsFromMethod,formalMethTypars = List.chop formalEnclosingTypars.Length allTyparsFromMethod
            let methodToParentRenaming,_ = mkTyparToTyparRenaming formalEnclosingTyparsFromMethod formalEnclosingTypars
            let formalParams = 
                GetArgInfosOfMember x.IsCSharpStyleExtensionMember g vref 
                |> List.mapSquared (map1Of2 (instType methodToParentRenaming) >> MakeSlotParam )
            let formalRetTy = Option.map (instType methodToParentRenaming) retTy
            MakeSlotSig(x.LogicalName, x.EnclosingType, formalEnclosingTypars, formalMethTypars, formalParams, formalRetTy)
        | DefaultStructCtor _ -> error(InternalError("no slotsig for DefaultStructCtor",m))
        | _ -> 
            let g = x.TcGlobals
            // slotsigs must contain the formal types for the arguments and return type 
            // a _formal_ 'void' return type is represented as a 'unit' type. 
            // slotsigs are independent of instantiation: if an instantiation 
            // happens to make the return type 'unit' (i.e. it was originally a variable type 
            // then that does not correspond to a slotsig compiled as a 'void' return type. 
            // REVIEW: should we copy down attributes to slot params? 
            let tcref =  tcrefOfAppTy g x.EnclosingType
            let formalEnclosingTyparsOrig = tcref.Typars(m)
            let formalEnclosingTypars = copyTypars formalEnclosingTyparsOrig
            let _,formalEnclosingTyparTys = FixupNewTypars m [] [] formalEnclosingTyparsOrig formalEnclosingTypars
            let formalMethTypars = copyTypars x.FormalMethodTypars
            let _,formalMethTyparTys = FixupNewTypars m formalEnclosingTypars formalEnclosingTyparTys x.FormalMethodTypars formalMethTypars
            let formalRetTy, formalParams = 
                match x with
                | ILMeth(_,ilminfo,_) -> 
                    let ftinfo = ILTypeInfo.FromType g (TType_app(tcref,formalEnclosingTyparTys))
                    let formalRetTy = ImportReturnTypeFromMetaData amap m ilminfo.RawMetadata.Return.Type ftinfo.ILScopeRef ftinfo.TypeInst formalMethTyparTys
                    let formalParams = 
                        [ [ for p in ilminfo.RawMetadata.Parameters do 
                                let paramType = ImportILTypeFromMetadata amap m ftinfo.ILScopeRef ftinfo.TypeInst formalMethTyparTys p.Type
                                yield TSlotParam(p.Name, paramType, p.IsIn, p.IsOut, p.IsOptional, []) ] ]
                    formalRetTy, formalParams
#if EXTENSIONTYPING
                | ProvidedMeth (_,mi,_,_) -> 
                    // GENERIC TYPE PROVIDERS: for generics, formal types should be  generated here, not the actual types
                    // For non-generic type providers there is no difference
                    let formalRetTy = x.GetCompiledReturnTy(amap, m, formalMethTyparTys)
                    // GENERIC TYPE PROVIDERS: formal types should be  generated here, not the actual types
                    // For non-generic type providers there is no difference
                    let formalParams = 
                        [ [ for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters", m) do 
                                let paramName = p.PUntaint((fun p -> match p.Name with null -> None | s -> Some s),m)
                                let paramType = Import.ImportProvidedType amap m (p.PApply((fun p -> p.ParameterType),m))
                                let isIn, isOut,isOptional = p.PUntaint((fun p -> p.IsIn, p.IsOut, p.IsOptional),m)
                                yield TSlotParam(paramName, paramType, isIn, isOut, isOptional, []) ] ]
                    formalRetTy, formalParams
#endif
                | _ -> failwith "unreachable"
            MakeSlotSig(x.LogicalName, x.EnclosingType, formalEnclosingTypars, formalMethTypars,formalParams, formalRetTy)
    
    /// Get the ParamData objects for the parameters of a MethInfo
    member x.GetParamDatas(amap, m, minst) = 
        let paramNamesAndTypes = 
            match x with 
            | ILMeth(_g,ilminfo,_) -> 
                [ ilminfo.GetParamNamesAndTypes(amap,m,minst)  ]
            | FSMeth(g,typ,vref,_) -> 
                let items = ParamNameAndType.FromMember x.IsCSharpStyleExtensionMember g vref 
                let inst = GetInstantiationForMemberVal g x.IsCSharpStyleExtensionMember (typ,vref,minst)
                items |> ParamNameAndType.InstantiateCurried inst 
            | DefaultStructCtor _ -> 
                [[]]
#if EXTENSIONTYPING
            | ProvidedMeth(amap,mi,_,_) -> 
                // A single set of tupled parameters
                [ [for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters", m) do 
                        let pname = 
                            match p.PUntaint((fun p -> p.Name), m) with
                            | null -> None
                            | name -> Some (mkSynId m name)
                        let ptyp =
                            match p.PApply((fun p -> p.ParameterType), m) with
                            | Tainted.Null ->  amap.g.unit_ty
                            | parameterType -> Import.ImportProvidedType amap m parameterType
                        yield ParamNameAndType(pname,ptyp) ] ]

#endif

        let paramAttribs = x.GetParamAttribs(amap, m)
        (paramAttribs,paramNamesAndTypes) ||> List.map2 (List.map2 (fun (isParamArrayArg,isOutArg,optArgInfo,callerInfoInfo,reflArgInfo) (ParamNameAndType(nmOpt,pty)) -> 
             ParamData(isParamArrayArg,isOutArg,optArgInfo,callerInfoInfo,nmOpt,reflArgInfo,pty)))

    /// Get the ParamData objects for the parameters of a MethInfo
    member x.HasParamArrayArg(amap, m, minst) = 
        x.GetParamDatas(amap, m, minst) |> List.existsSquared (fun (ParamData(isParamArrayArg,_,_,_,_,_,_)) -> isParamArrayArg)


    /// Select all the type parameters of the declaring type of a method. 
    ///
    /// For extension methods, no type parameters are returned, because all the 
    /// type parameters are part of the apparent type, rather the 
    /// declaring type, even for extension methods extending generic types.
    member x.GetFormalTyparsOfDeclaringType m = 
        if x.IsExtensionMember then [] 
        else 
            match x with
            | ILMeth(_,ilminfo,_) -> ilminfo.DeclaringTyconRef.Typars m                    
            | FSMeth(g,typ,vref,_) -> 
                let memberParentTypars,_,_,_ = AnalyzeTypeOfMemberVal false g (typ,vref)
                memberParentTypars
            | DefaultStructCtor(g,typ) -> 
                (tcrefOfAppTy g typ).Typars(m)
#if EXTENSIONTYPING
            | ProvidedMeth (amap,_,_,_) -> 
                (tcrefOfAppTy amap.g x.EnclosingType).Typars(m)
#endif

//-------------------------------------------------------------------------
// ILFieldInfo


/// Represents a single use of a IL or provided field from one point in an F# program
[<NoComparison; NoEquality>]
type ILFieldInfo = 
     /// Represents a single use of a field backed by Abstract IL metadata
    | ILFieldInfo of ILTypeInfo * ILFieldDef // .NET IL fields 
#if EXTENSIONTYPING
     /// Represents a single use of a field backed by provided metadata
    | ProvidedField of Import.ImportMap * Tainted<ProvidedFieldInfo> * range
#endif

    /// Get the enclosing ("parent"/"declaring") type of the field. 
    member x.EnclosingType = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.ToType
#if EXTENSIONTYPING
        | ProvidedField(amap,fi,m) -> (Import.ImportProvidedType amap m (fi.PApply((fun fi -> fi.DeclaringType),m)))
#endif

     /// Get a reference to the declaring type of the field as an ILTypeRef
    member x.ILTypeRef = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.ILTypeRef
#if EXTENSIONTYPING
        | ProvidedField(amap,fi,m) -> (Import.ImportProvidedTypeAsILType amap m (fi.PApply((fun fi -> fi.DeclaringType),m))).TypeRef
#endif
    
     /// Get the scope used to interpret IL metadata
    member x.ScopeRef = x.ILTypeRef.Scope

     /// Get the type instantiation of the declaring type of the field 
    member x.TypeInst = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.TypeInst
#if EXTENSIONTYPING
        | ProvidedField _ -> [] /// GENERIC TYPE PROVIDERS
#endif

     /// Get the name of the field
    member x.FieldName = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.Name
#if EXTENSIONTYPING
        | ProvidedField(_,fi,m) -> fi.PUntaint((fun fi -> fi.Name),m)
#endif

     /// Indicates if the field is readonly (in the .NET/C# sense of readonly)
    member x.IsInitOnly = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsInitOnly
#if EXTENSIONTYPING
        | ProvidedField(_,fi,m) -> fi.PUntaint((fun fi -> fi.IsInitOnly),m)
#endif

    /// Indicates if the field is a member of a struct or enum type
    member x.IsValueType = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.IsValueType
#if EXTENSIONTYPING
        | ProvidedField(amap,_,_) -> isStructTy amap.g x.EnclosingType
#endif

     /// Indicates if the field is static
    member x.IsStatic = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsStatic
#if EXTENSIONTYPING
        | ProvidedField(_,fi,m) -> fi.PUntaint((fun fi -> fi.IsStatic),m)
#endif

     /// Indicates if the field has the 'specialname' property in the .NET IL
    member x.IsSpecialName = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsSpecialName
#if EXTENSIONTYPING
        | ProvidedField(_,fi,m) -> fi.PUntaint((fun fi -> fi.IsSpecialName),m)
#endif
    
     /// Indicates if the field is a literal field with an associated literal value
    member x.LiteralValue = 
        match x with 
        | ILFieldInfo(_,pd) -> if pd.IsLiteral then pd.LiteralValue else None
#if EXTENSIONTYPING
        | ProvidedField(_,fi,m) -> 
            if fi.PUntaint((fun fi -> fi.IsLiteral),m) then 
                Some (ILFieldInit.FromProvidedObj m (fi.PUntaint((fun fi -> fi.GetRawConstantValue()),m)))
            else
                None
#endif
                                        
     /// Get the type of the field as an IL type
    member x.ILFieldType = 
        match x with 
        | ILFieldInfo (_,fdef) -> fdef.Type
#if EXTENSIONTYPING
        | ProvidedField(amap,fi,m) -> Import.ImportProvidedTypeAsILType amap m (fi.PApply((fun fi -> fi.FieldType),m))
#endif

     /// Get the type of the field as an F# type
    member x.FieldType(amap,m) = 
        match x with 
        | ILFieldInfo (tinfo,fdef) -> ImportILTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] fdef.Type
#if EXTENSIONTYPING
        | ProvidedField(amap,fi,m) -> Import.ImportProvidedType amap m (fi.PApply((fun fi -> fi.FieldType),m))
#endif

    static member ILFieldInfosUseIdenticalDefinitions x1 x2 = 
        match x1,x2 with 
        | ILFieldInfo(_, x1), ILFieldInfo(_, x2) -> (x1 === x2)
#if EXTENSIONTYPING
        | ProvidedField(_,fi1,_), ProvidedField(_,fi2,_)-> ProvidedFieldInfo.TaintedEquals (fi1, fi2) 
        | _ -> false
#endif

     /// Get an (uninstantiated) reference to the field as an Abstract IL ILFieldRef
    member x.ILFieldRef = rescopeILFieldRef x.ScopeRef (mkILFieldRef(x.ILTypeRef,x.FieldName,x.ILFieldType))
    override x.ToString() =  x.FieldName


/// Describes an F# use of a field in an F#-declared record, class or struct type 
[<NoComparison; NoEquality>]
type RecdFieldInfo = 
    | RecdFieldInfo of TypeInst * Tast.RecdFieldRef 

    /// Get the generic instantiation of the declaring type of the field
    member x.TypeInst = let (RecdFieldInfo(tinst,_)) = x in tinst

    /// Get a reference to the F# metadata for the uninstantiated field
    member x.RecdFieldRef = let (RecdFieldInfo(_,rfref)) = x in rfref

    /// Get the F# metadata for the uninstantiated field
    member x.RecdField = x.RecdFieldRef.RecdField

    /// Indicate if the field is a static field in an F#-declared record, class or struct type 
    member x.IsStatic = x.RecdField.IsStatic

    /// Indicate if the field is a literal field in an F#-declared record, class or struct type 
    member x.LiteralValue = x.RecdField.LiteralValue

    /// Get a reference to the F# metadata for the F#-declared record, class or struct type 
    member x.TyconRef = x.RecdFieldRef.TyconRef

    /// Get the F# metadata for the F#-declared record, class or struct type 
    member x.Tycon = x.RecdFieldRef.Tycon

    /// Get the name of the field in an F#-declared record, class or struct type 
    member x.Name = x.RecdField.Name

    /// Get the (instantiated) type of the field in an F#-declared record, class or struct type 
    member x.FieldType = actualTyOfRecdFieldRef x.RecdFieldRef x.TypeInst

    /// Get the enclosing (declaring) type of the field in an F#-declared record, class or struct type 
    member x.EnclosingType = TType_app (x.RecdFieldRef.TyconRef,x.TypeInst)
    override x.ToString() = x.TyconRef.ToString() + "::" + x.Name
    

/// Describes an F# use of a union case
[<NoComparison; NoEquality>]
type UnionCaseInfo = 
    | UnionCaseInfo of TypeInst * Tast.UnionCaseRef 

    /// Get the generic instantiation of the declaring type of the union case
    member x.TypeInst = let (UnionCaseInfo(tinst,_)) = x in tinst

    /// Get a reference to the F# metadata for the uninstantiated union case
    member x.UnionCaseRef = let (UnionCaseInfo(_,ucref)) = x in ucref

    /// Get the F# metadata for the uninstantiated union case
    member x.UnionCase = x.UnionCaseRef.UnionCase

    /// Get a reference to the F# metadata for the declaring union type
    member x.TyconRef = x.UnionCaseRef.TyconRef

    /// Get the F# metadata for the declaring union type
    member x.Tycon = x.UnionCaseRef.Tycon

    /// Get the name of the union case
    member x.Name = x.UnionCase.DisplayName
    override x.ToString() = x.TyconRef.ToString() + "::" + x.Name


/// Describes an F# use of a property backed by Abstract IL metadata
[<NoComparison; NoEquality>]
type ILPropInfo = 
    | ILPropInfo of ILTypeInfo * ILPropertyDef 

    /// Get the declaring IL type of the IL property, including any generic instantiation
    member x.ILTypeInfo = match x with (ILPropInfo(tinfo,_)) -> tinfo

    /// Get the raw Abstract IL metadata for the IL property
    member x.RawMetadata = match x with (ILPropInfo(_,pd)) -> pd

    /// Get the name of the IL property
    member x.PropertyName = x.RawMetadata.Name

    /// Gets the ILMethInfo of the 'get' method for the IL property
    member x.GetterMethod(g) = 
        assert x.HasGetter
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.GetMethod.Value
        ILMethInfo(g,x.ILTypeInfo.ToType,None,mdef,[]) 

    /// Gets the ILMethInfo of the 'set' method for the IL property
    member x.SetterMethod(g) = 
        assert x.HasSetter
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.SetMethod.Value
        ILMethInfo(g,x.ILTypeInfo.ToType,None,mdef,[]) 
          
    /// Indicates if the IL property has a 'get' method
    member x.HasGetter = Option.isSome x.RawMetadata.GetMethod 

    /// Indicates if the IL property has a 'set' method
    member x.HasSetter = Option.isSome x.RawMetadata.SetMethod 

    /// Indicates if the IL property is static
    member x.IsStatic = (x.RawMetadata.CallingConv = ILThisConvention.Static) 

    /// Indicates if the IL property is virtual
    member x.IsVirtual(g) = 
        (x.HasGetter && x.GetterMethod(g).IsVirtual) ||
        (x.HasSetter && x.SetterMethod(g).IsVirtual) 

    /// Indicates if the IL property is logically a 'newslot', i.e. hides any previous slots of the same name.
    member x.IsNewSlot(g) = 
        (x.HasGetter && x.GetterMethod(g).IsNewSlot) ||
        (x.HasSetter && x.SetterMethod(g).IsNewSlot) 

    /// Get the names and types of the indexer arguments associated with the IL property.
    member x.GetParamNamesAndTypes(amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = x
        pdef.Args |> List.map (fun ty -> ParamNameAndType(None, ImportILTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] ty) )

    /// Get the types of the indexer arguments associated with the IL property.
    member x.GetParamTypes(amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = x
        pdef.Args |> List.map (fun ty -> ImportILTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] ty) 

    /// Get the return type of the IL property.
    member x.GetPropertyType (amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = x
        ImportILTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] pdef.Type

    override x.ToString() = x.ILTypeInfo.ToString() + "::" + x.PropertyName



/// Describes an F# use of a property 
[<NoComparison; NoEquality>]
type PropInfo = 
    /// An F# use of a property backed by F#-declared metadata
    | FSProp of TcGlobals * TType * ValRef option * ValRef option
    /// An F# use of a property backed by Abstract IL metadata
    | ILProp of TcGlobals * ILPropInfo
#if EXTENSIONTYPING
    /// An F# use of a property backed by provided metadata
    | ProvidedProp of Import.ImportMap * Tainted<ProvidedPropertyInfo> * range
#endif

    /// Try to get an arbitrary F# ValRef associated with the member. This is to determine if the member is virtual, amongst other things.
    member x.ArbitraryValRef = 
        match x with 
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> Some vref
        | FSProp(_,_,None,None) -> failwith "unreachable"
        | _ -> None 

    /// Indicates if this property has an associated XML comment authored in this assembly.
    member x.HasDirectXmlComment =
        match x with
        | FSProp(g,_,Some vref,_)
        | FSProp(g,_,_,Some vref) -> valRefInThisAssembly g.compilingFslib vref
#if EXTENSIONTYPING
        | ProvidedProp _ -> true
#endif
        | _ -> false

    /// Get the logical name of the property.
    member x.PropertyName = 
        match x with 
        | ILProp(_,x) -> x.PropertyName
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.PropertyName
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> pi.PUntaint((fun pi -> pi.Name),m)
#endif
        | FSProp _ -> failwith "unreachable"

    /// Indicates if this property has an associated getter method.
    member x.HasGetter = 
        match x with
        | ILProp(_,x) -> x.HasGetter
        | FSProp(_,_,x,_) -> Option.isSome x 
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> pi.PUntaint((fun pi -> pi.CanRead),m)
#endif

    /// Indicates if this property has an associated setter method.
    member x.HasSetter = 
        match x with
        | ILProp(_,x) -> x.HasSetter
        | FSProp(_,_,_,x) -> Option.isSome x 
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> pi.PUntaint((fun pi -> pi.CanWrite),m)
#endif

    /// Get the enclosing type of the proeprty. 
    ///
    /// If this is an extension member, then this is the apparent parent, i.e. the type the property appears to extend.
    member x.EnclosingType = 
        match x with 
        | ILProp(_,x) -> x.ILTypeInfo.ToType
        | FSProp(_,typ,_,_) -> typ
#if EXTENSIONTYPING
        | ProvidedProp(amap,pi,m) -> 
            Import.ImportProvidedType amap m (pi.PApply((fun pi -> pi.DeclaringType),m)) 
#endif

    /// Indicates if this is an extension member
    member x.IsExtensionMember = 
        match x.ArbitraryValRef with Some vref -> vref.IsExtensionMember | _ -> false

    /// True if the getter (or, if absent, the setter) is a virtual method
    // REVIEW: for IL properties this is getter OR setter. For F# properties it is getter ELSE setter
    member x.IsVirtualProperty = 
        match x with 
        | ILProp(g,x) -> x.IsVirtual(g)
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.IsVirtualMember
        | FSProp _-> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            let mi = ArbitraryMethodInfoOfPropertyInfo pi m
            mi.PUntaint((fun mi -> mi.IsVirtual), m)
#endif
    
    /// Indicates if the property is logically a 'newslot', i.e. hides any previous slots of the same name.
    member x.IsNewSlot = 
        match x with 
        | ILProp(g,x) -> x.IsNewSlot(g)
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.IsDispatchSlotMember
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            let mi = ArbitraryMethodInfoOfPropertyInfo pi m
            mi.PUntaint((fun mi -> mi.IsHideBySig), m)
#endif


    /// Indicates if the getter (or, if absent, the setter) for the property is a dispatch slot.
    // REVIEW: for IL properties this is getter OR setter. For F# properties it is getter ELSE setter
    member x.IsDispatchSlot = 
        match x with 
        | ILProp(g,x) -> x.IsVirtual(g)
        | FSProp(g,typ,Some vref,_) 
        | FSProp(g,typ,_, Some vref) ->
            isInterfaceTy g typ  || (vref.MemberInfo.Value.MemberFlags.IsDispatchSlot)
        | FSProp _ -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            let mi = ArbitraryMethodInfoOfPropertyInfo pi m
            mi.PUntaint((fun mi -> mi.IsVirtual), m)
#endif

    /// Indicates if this property is static.
    member x.IsStatic =
        match x with 
        | ILProp(_,x) -> x.IsStatic
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> not vref.IsInstanceMember
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            (ArbitraryMethodInfoOfPropertyInfo pi m).PUntaint((fun mi -> mi.IsStatic), m)
#endif

    /// Indicates if this property is marked 'override' and thus definitely overrides another property.
    member x.IsDefiniteFSharpOverride = 
        match x.ArbitraryValRef with 
        | Some vref -> vref.IsDefiniteFSharpOverrideMember
        | None -> false

    member x.ImplementedSlotSignatures =
        x.ArbitraryValRef.Value.ImplementedSlotSignatures  

    member x.IsFSharpExplicitInterfaceImplementation = 
        match x.ArbitraryValRef with 
        | Some vref -> vref.IsFSharpExplicitInterfaceImplementation x.TcGlobals
        | None -> false


    /// Indicates if this property is an indexer property, i.e. a property with arguments.
    member x.IsIndexer = 
        match x with 
        | ILProp(_,ILPropInfo(_,pdef)) -> pdef.Args.Length <> 0
        | FSProp(g,_,Some vref,_)  ->
            // A getter has signature  { OptionalObjectType } -> Unit -> PropertyType 
            // A getter indexer has signature  { OptionalObjectType } -> TupledIndexerArguments -> PropertyType 
            let arginfos = ArgInfosOfMember g vref
            arginfos.Length = 1 && arginfos.Head.Length >= 1
        | FSProp(g,_,_, Some vref) -> 
            // A setter has signature  { OptionalObjectType } -> PropertyType -> Void 
            // A setter indexer has signature  { OptionalObjectType } -> TupledIndexerArguments -> PropertyType -> Void 
            let arginfos = ArgInfosOfMember g vref
            arginfos.Length = 1 && arginfos.Head.Length >= 2
        | FSProp(_,_,None,None) -> 
            failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            pi.PUntaint((fun pi -> pi.GetIndexParameters().Length), m)>0
#endif

    /// Indicates if this is an F# property compiled as a CLI event, e.g. a [<CLIEvent>] property.
    member x.IsFSharpEventProperty = 
        match x with 
        | FSProp(g,_,Some vref,None)  -> vref.IsFSharpEventProperty(g)
#if EXTENSIONTYPING
        | ProvidedProp _ -> false
#endif
        | _ -> false

    /// Return a new property info where there is no associated setter, only an associated getter.
    ///
    /// Property infos can combine getters and setters, assuming they are consistent w.r.t. 'virtual', indexer argument types etc.
    /// When checking consistency we split these apart
    member x.DropSetter = 
        match x with 
        | FSProp(g,typ,Some vref,_)  -> FSProp(g,typ,Some vref,None)
        | _ -> x


    /// Return a new property info where there is no associated getter, only an associated setter.
    member x.DropGetter = 
        match x with 
        | FSProp(g,typ,_,Some vref)  -> FSProp(g,typ,None,Some vref)
        | _ -> x

    /// Get the intra-assembly XML documentation for the property.
    member x.XmlDoc = 
        match x with 
        | ILProp _ -> XmlDoc.Empty
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.XmlDoc
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            XmlDoc (pi.PUntaint((fun pix -> (pix :> IProvidedCustomAttributeProvider).GetXmlDocAttributes(pi.TypeProvider.PUntaintNoFailure(id))), m))
#endif

    /// Get the TcGlobals associated with the object
    member x.TcGlobals = 
        match x with 
        | ILProp(g,_) -> g 
        | FSProp(g,_,_,_) -> g 
#if EXTENSIONTYPING
        | ProvidedProp(amap,_,_) -> amap.g
#endif

    /// Indicates if the enclosing type for the property is a value type. 
    ///
    /// For an extension property, this indicates if the property extends a struct type.
    member x.IsValueType = isStructTy x.TcGlobals x.EnclosingType


    /// Get the result type of the property
    member x.GetPropertyType (amap,m) = 
        match x with
        | ILProp (_,ilpinfo) -> ilpinfo.GetPropertyType (amap,m)
        | FSProp (g,typ,Some vref,_) 
        | FSProp (g,typ,_,Some vref) -> 
            let inst = GetInstantiationForPropertyVal g (typ,vref)
            ReturnTypeOfPropertyVal g vref.Deref |> instType inst
            
        | FSProp _ -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,m) -> 
            Import.ImportProvidedType amap m (pi.PApply((fun pi -> pi.PropertyType),m))
#endif


    /// Get the names and types of the indexer parameters associated with the property
    member x.GetParamNamesAndTypes(amap,m) = 
        match x with 
        | ILProp (_,ilpinfo) -> ilpinfo.GetParamNamesAndTypes(amap,m)
        | FSProp (g,typ,Some vref,_) 
        | FSProp (g,typ,_,Some vref) -> 
            let inst = GetInstantiationForPropertyVal g (typ,vref)
            ArgInfosOfPropertyVal g vref.Deref |> List.map (ParamNameAndType.FromArgInfo >> ParamNameAndType.Instantiate inst)
        | FSProp _ -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp (_,pi,m) -> 
            [ for p in pi.PApplyArray((fun pi -> pi.GetIndexParameters()), "GetIndexParameters", m) do
                let paramName = p.PUntaint((fun p -> match p.Name with null -> None | s -> Some (mkSynId m s)), m)
                let paramType = Import.ImportProvidedType amap m (p.PApply((fun p -> p.ParameterType), m))
                yield ParamNameAndType(paramName, paramType) ]
#endif
     
    /// Get the details of the indexer parameters associated with the property
    member x.GetParamDatas(amap,m) = 
        x.GetParamNamesAndTypes(amap,m)
        |> List.map (fun (ParamNameAndType(nmOpt,pty)) -> ParamData(false, false, NotOptional, NoCallerInfo, nmOpt, ReflectedArgInfo.None, pty))

    /// Get the types of the indexer parameters associated with the property
    member x.GetParamTypes(amap,m) = 
      x.GetParamNamesAndTypes(amap,m) |> List.map (fun (ParamNameAndType(_,ty)) -> ty)

    /// Get a MethInfo for the 'getter' method associated with the property
    member x.GetterMethod = 
        match x with
        | ILProp(g,x) -> ILMeth(g,x.GetterMethod(g),None)
        | FSProp(g,typ,Some vref,_) -> FSMeth(g,typ,vref,None) 
#if EXTENSIONTYPING
        | ProvidedProp(amap,pi,m) -> 
            let meth = GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetGetMethod()) FSComp.SR.etPropertyCanReadButHasNoGetter
            ProvidedMeth(amap, meth, None, m)

#endif
        | FSProp _ -> failwith "no getter method"

    /// Get a MethInfo for the 'setter' method associated with the property
    member x.SetterMethod = 
        match x with
        | ILProp(g,x) -> ILMeth(g,x.SetterMethod(g),None)
        | FSProp(g,typ,_,Some vref) -> FSMeth(g,typ,vref,None)
#if EXTENSIONTYPING
        | ProvidedProp(amap,pi,m) -> 
            let meth = GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetSetMethod()) FSComp.SR.etPropertyCanWriteButHasNoSetter
            ProvidedMeth(amap, meth, None, m)
#endif
        | FSProp _ -> failwith "no setter method"

    /// Test whether two property infos have the same underlying definition.
    ///
    /// Uses the same techniques as 'MethInfosUseIdenticalDefinitions'.
    static member PropInfosUseIdenticalDefinitions x1 x2 = 
        let optVrefEq g = function 
          | Some(v1), Some(v2) -> valRefEq g v1 v2
          | None, None -> true
          | _ -> false    
        match x1,x2 with 
        | ILProp(_, x1), ILProp(_, x2) -> (x1.RawMetadata === x2.RawMetadata)
        | FSProp(g, _, vrefa1, vrefb1), FSProp(_, _, vrefa2, vrefb2) ->
            (optVrefEq g (vrefa1, vrefa2)) && (optVrefEq g (vrefb1, vrefb2))
#if EXTENSIONTYPING
        | ProvidedProp(_,pi1,_), ProvidedProp(_,pi2,_) -> ProvidedPropertyInfo.TaintedEquals (pi1, pi2) 
#endif
        | _ -> false

    /// Calculates a hash code of property info (similar as previous)
    member pi.ComputeHashCode() = 
        match pi with 
        | ILProp(_, x1) -> hash x1.RawMetadata.Name
        | FSProp(_,_,vrefOpt1, vrefOpt2) -> 
            // Hash on option<string>*option<string>
            let vth = (vrefOpt1 |> Option.map (fun vr -> vr.LogicalName), (vrefOpt2 |> Option.map (fun vr -> vr.LogicalName)))
            hash vth
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_) -> ProvidedPropertyInfo.TaintedGetHashCode(pi)
#endif

//-------------------------------------------------------------------------
// ILEventInfo


/// Describes an F# use of an event backed by Abstract IL metadata
[<NoComparison; NoEquality>]
type ILEventInfo = 
    | ILEventInfo of ILTypeInfo * ILEventDef

    /// Get the raw Abstract IL metadata for the event
    member x.RawMetadata = match x with (ILEventInfo(_,ed)) -> ed

    /// Get the declaring IL type of the event as an ILTypeInfo
    member x.ILTypeInfo = match x with (ILEventInfo(tinfo,_)) -> tinfo

    /// Get the ILMethInfo describing the 'add' method associated with the event
    member x.AddMethod(g) =
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.AddMethod
        ILMethInfo(g,x.ILTypeInfo.ToType,None,mdef,[]) 

    /// Get the ILMethInfo describing the 'remove' method associated with the event
    member x.RemoveMethod(g) =
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.RemoveMethod
        ILMethInfo(g,x.ILTypeInfo.ToType,None,mdef,[]) 

    /// Get the declaring type of the event as an ILTypeRef
    member x.TypeRef = x.ILTypeInfo.ILTypeRef

    /// Get the name of the event
    member x.Name = x.RawMetadata.Name

    /// Indicates if the property is static
    member x.IsStatic(g) = x.AddMethod(g).IsStatic
    override x.ToString() = x.ILTypeInfo.ToString() + "::" + x.Name

//-------------------------------------------------------------------------
// Helpers for EventInfo

/// An exception type used to raise an error using the old error system.
///
/// Error text: "A definition to be compiled as a .NET event does not have the expected form. Only property members can be compiled as .NET events."
exception BadEventTransformation of range

/// Properties compatible with type IDelegateEvent and attributed with CLIEvent are special: 
/// we generate metadata and add/remove methods 
/// to make them into a .NET event, and mangle the name of a property.  
/// We don't handle static, indexer or abstract properties correctly. 
/// Note the name mangling doesn't affect the name of the get/set methods for the property 
/// and so doesn't affect how we compile F# accesses to the property. 
let private tyConformsToIDelegateEvent g ty = 
   isIDelegateEventType g ty && isDelegateTy g (destIDelegateEventType g ty) 
   

/// Create an error object to raise should an event not have the shape expected by the .NET idiom described further below 
let nonStandardEventError nm m = 
    Error ((FSComp.SR.eventHasNonStandardType(nm,("add_"+nm),("remove_"+nm))),m)

/// Find the delegate type that an F# event property implements by looking through the type hierarchy of the type of the property
/// for the first instantiation of IDelegateEvent.
let FindDelegateTypeOfPropertyEvent g amap nm m ty =
    match SearchEntireHierarchyOfType (tyConformsToIDelegateEvent g) g amap m ty with
    | None -> error(nonStandardEventError nm m)
    | Some ty -> destIDelegateEventType g ty

        
//-------------------------------------------------------------------------
// EventInfo

/// Describes an F# use of an event
[<NoComparison; NoEquality>]
type EventInfo = 
    /// An F# use of an event backed by F#-declared metadata
    | FSEvent of TcGlobals * PropInfo * ValRef * ValRef
    /// An F# use of an event backed by .NET metadata
    | ILEvent of TcGlobals * ILEventInfo
#if EXTENSIONTYPING
    /// An F# use of an event backed by provided metadata
    | ProvidedEvent of Import.ImportMap * Tainted<ProvidedEventInfo> * range
#endif

    /// Get the enclosing type of the event. 
    ///
    /// If this is an extension member, then this is the apparent parent, i.e. the type the event appears to extend.
    member x.EnclosingType = 
        match x with 
        | ILEvent(_,e) -> e.ILTypeInfo.ToType 
        | FSEvent (_,p,_,_) -> p.EnclosingType
#if EXTENSIONTYPING
        | ProvidedEvent (amap,ei,m) -> Import.ImportProvidedType amap m (ei.PApply((fun ei -> ei.DeclaringType),m)) 
#endif

    /// Indicates if this event has an associated XML comment authored in this assembly.
    member x.HasDirectXmlComment =
        match x with
        | FSEvent (_,p,_,_) -> p.HasDirectXmlComment 
#if EXTENSIONTYPING
        | ProvidedEvent _ -> true
#endif
        | _ -> false

    /// Get the intra-assembly XML documentation for the property.
    member x.XmlDoc = 
        match x with 
        | ILEvent _ -> XmlDoc.Empty
        | FSEvent (_,p,_,_) -> p.XmlDoc
#if EXTENSIONTYPING
        | ProvidedEvent (_,ei,m) -> 
            XmlDoc (ei.PUntaint((fun eix -> (eix :> IProvidedCustomAttributeProvider).GetXmlDocAttributes(ei.TypeProvider.PUntaintNoFailure(id))), m))
#endif

    /// Get the logical name of the event.
    member x.EventName = 
        match x with 
        | ILEvent(_,e) -> e.Name 
        | FSEvent (_,p,_,_) -> p.PropertyName
#if EXTENSIONTYPING
        | ProvidedEvent (_,ei,m) -> ei.PUntaint((fun ei -> ei.Name), m)
#endif

    /// Indicates if this property is static.
    member x.IsStatic = 
        match x with 
        | ILEvent(g,e) -> e.IsStatic(g)
        | FSEvent (_,p,_,_) -> p.IsStatic
#if EXTENSIONTYPING
        | ProvidedEvent (_,ei,m) -> 
            let meth = GetAndSanityCheckProviderMethod m ei (fun ei -> ei.GetAddMethod()) FSComp.SR.etEventNoAdd
            meth.PUntaint((fun mi -> mi.IsStatic), m)
#endif

    /// Get the TcGlobals associated with the object
    member x.TcGlobals = 
        match x with 
        | ILEvent(g,_) -> g
        | FSEvent(g,_,_,_) -> g
#if EXTENSIONTYPING
        | ProvidedEvent (amap,_,_) -> amap.g
#endif

    /// Indicates if the enclosing type for the event is a value type. 
    ///
    /// For an extension event, this indicates if the event extends a struct type.
    member x.IsValueType = isStructTy x.TcGlobals x.EnclosingType

    /// Get the 'add' method associated with an event
    member x.GetAddMethod() = 
        match x with 
        | ILEvent(g,e) -> ILMeth(g,e.AddMethod(g),None)
        | FSEvent(g,p,addValRef,_) -> FSMeth(g,p.EnclosingType,addValRef,None)
#if EXTENSIONTYPING
        | ProvidedEvent (amap,ei,m) -> 
            let meth = GetAndSanityCheckProviderMethod m ei (fun ei -> ei.GetAddMethod()) FSComp.SR.etEventNoAdd
            ProvidedMeth(amap, meth, None, m)
#endif

    /// Get the 'remove' method associated with an event
    member x.GetRemoveMethod() = 
        match x with 
        | ILEvent(g,e) -> ILMeth(g,e.RemoveMethod(g),None)
        | FSEvent(g,p,_,removeValRef) -> FSMeth(g,p.EnclosingType,removeValRef,None)
#if EXTENSIONTYPING
        | ProvidedEvent (amap,ei,m) -> 
            let meth = GetAndSanityCheckProviderMethod m ei (fun ei -> ei.GetRemoveMethod()) FSComp.SR.etEventNoRemove
            ProvidedMeth(amap, meth, None, m)
#endif
    
    /// Try to get an arbitrary F# ValRef associated with the member. This is to determine if the member is virtual, amongst other things.
    member x.ArbitraryValRef = 
        match x with 
        | FSEvent(_,_,addValRef,_) -> Some addValRef
        | _ ->  None

    /// Get the delegate type associated with the event. 
    member x.GetDelegateType(amap,m) = 
        match x with 
        | ILEvent(_,ILEventInfo(tinfo,edef)) -> 
            // Get the delegate type associated with an IL event, taking into account the instantiation of the
            // declaring type.
            if Option.isNone edef.Type then error (nonStandardEventError x.EventName m)
            ImportILTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] edef.Type.Value

        | FSEvent(g,p,_,_) -> 
            FindDelegateTypeOfPropertyEvent g amap x.EventName m (p.GetPropertyType(amap,m))
#if EXTENSIONTYPING
        | ProvidedEvent (_,ei,_) -> 
            Import.ImportProvidedType amap m (ei.PApply((fun ei -> ei.EventHandlerType), m))
#endif


    /// Test whether two event infos have the same underlying definition.
    static member EventInfosUseIdenticalDefintions x1 x2 =
        match x1, x2 with
        | FSEvent(g, pi1, vrefa1, vrefb1), FSEvent(_, pi2, vrefa2, vrefb2) ->
            PropInfo.PropInfosUseIdenticalDefinitions pi1 pi2 && valRefEq g vrefa1 vrefa2 && valRefEq g vrefb1 vrefb2
        | ILEvent(_, x1), ILEvent(_, x2) -> (x1.RawMetadata === x2.RawMetadata)
#if EXTENSIONTYPING
        | ProvidedEvent (_,ei1,_), ProvidedEvent (_,ei2,_) -> ProvidedEventInfo.TaintedEquals (ei1, ei2)  
#endif
        | _ -> false
  
    /// Calculates a hash code of event info (similar as previous)
    member ei.ComputeHashCode() = 
        match ei with 
        | ILEvent(_, x1) -> hash x1.RawMetadata.Name
        | FSEvent(_, pi, vref1, vref2) -> hash ( pi.ComputeHashCode(), vref1.LogicalName, vref2.LogicalName)
#if EXTENSIONTYPING
        | ProvidedEvent (_,ei,_) -> ProvidedEventInfo.TaintedGetHashCode(ei)
#endif

//-------------------------------------------------------------------------
// Helpers associated with getting and comparing method signatures


/// Represents the information about the compiled form of a method signature. Used when analyzing implementation
/// relations between members and abstract slots.
type CompiledSig = CompiledSig  of TType list list * TType option * Typars * TyparInst 

/// Get the information about the compiled form of a method signature. Used when analyzing implementation
/// relations between members and abstract slots.
let CompiledSigOfMeth g amap m (minfo:MethInfo) = 
    let formalMethTypars = minfo.FormalMethodTypars
    let fminst = generalizeTypars formalMethTypars
    let vargtys = minfo.GetParamTypes(amap, m, fminst)
    let vrty = minfo.GetCompiledReturnTy(amap, m, fminst)

    // The formal method typars returned are completely formal - they don't take into account the instantiation 
    // of the enclosing type. For example, they may have constraints involving the _formal_ type parameters 
    // of the enclosing type. This instantiations can be used to interpret those type parameters 
    let fmtpinst = 
        let parentTyArgs = argsOfAppTy g minfo.EnclosingType
        let memberParentTypars  = minfo.GetFormalTyparsOfDeclaringType m 
        mkTyparInst memberParentTypars parentTyArgs
            
    CompiledSig(vargtys,vrty,formalMethTypars,fmtpinst)

/// Used to hide/filter members from super classes based on signature 
let MethInfosEquivByNameAndPartialSig erasureFlag ignoreFinal g amap m (minfo:MethInfo) (minfo2:MethInfo) = 
    (minfo.LogicalName = minfo2.LogicalName) &&
    (minfo.GenericArity = minfo2.GenericArity) &&
    (ignoreFinal || minfo.IsFinal = minfo2.IsFinal) &&
    let formalMethTypars = minfo.FormalMethodTypars
    let fminst = generalizeTypars formalMethTypars
    let formalMethTypars2 = minfo2.FormalMethodTypars
    let fminst2 = generalizeTypars formalMethTypars2
    let argtys = minfo.GetParamTypes(amap, m, fminst)
    let argtys2 = minfo2.GetParamTypes(amap, m, fminst2)
    (argtys,argtys2) ||> List.lengthsEqAndForall2 (List.lengthsEqAndForall2 (typeAEquivAux erasureFlag g (TypeEquivEnv.FromEquivTypars formalMethTypars formalMethTypars2)))

/// Used to hide/filter members from super classes based on signature 
let PropInfosEquivByNameAndPartialSig erasureFlag g amap m (pinfo:PropInfo) (pinfo2:PropInfo) = 
    pinfo.PropertyName = pinfo2.PropertyName &&
    let argtys = pinfo.GetParamTypes(amap,m)
    let argtys2 = pinfo2.GetParamTypes(amap,m)
    List.lengthsEqAndForall2 (typeEquivAux erasureFlag g) argtys argtys2 

/// Used to hide/filter members from super classes based on signature 
let MethInfosEquivByNameAndSig erasureFlag ignoreFinal g amap m minfo minfo2 = 
    MethInfosEquivByNameAndPartialSig erasureFlag ignoreFinal g amap m minfo minfo2 &&
    let (CompiledSig(_,retTy,formalMethTypars,_)) = CompiledSigOfMeth g amap m minfo
    let (CompiledSig(_,retTy2,formalMethTypars2,_)) = CompiledSigOfMeth g amap m minfo2
    match retTy,retTy2 with 
    | None,None -> true
    | Some retTy,Some retTy2 -> typeAEquivAux erasureFlag g (TypeEquivEnv.FromEquivTypars formalMethTypars formalMethTypars2) retTy retTy2 
    | _ -> false

/// Used to hide/filter members from super classes based on signature 
let PropInfosEquivByNameAndSig erasureFlag g amap m (pinfo:PropInfo) (pinfo2:PropInfo) = 
    PropInfosEquivByNameAndPartialSig erasureFlag g amap m pinfo pinfo2 &&
    let retTy = pinfo.GetPropertyType(amap,m)
    let retTy2 = pinfo2.GetPropertyType(amap,m) 
    typeEquivAux erasureFlag g retTy retTy2
