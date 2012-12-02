//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

/// tinfos, minfos, finfos, pinfos - summaries of information for references
/// to .NET and F# constructs.


module internal Microsoft.FSharp.Compiler.Infos

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
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Core.Printf
#if EXTENSIONTYPING
open Microsoft.FSharp.Compiler.ExtensionTyping
open Microsoft.FSharp.Core.CompilerServices
#endif

//-------------------------------------------------------------------------
// From IL types to F# types
//------------------------------------------------------------------------- 

/// importInst gives the context for interpreting type variables 
let ImportType scoref amap m importInst ilty = 
    ilty |> rescopeILType scoref |>  Import.ImportILType amap m importInst 

//-------------------------------------------------------------------------
// Fold the hierarchy. 
//  REVIEW: this code generalizes the iteration used below for member lookup.
//------------------------------------------------------------------------- 

let isExnDeclTy g typ = 
    isAppTy g typ && (tcrefOfAppTy g typ).IsExceptionDecl
    
let GetSuperTypeOfType g amap m typ = 
#if EXTENSIONTYPING
    let typ = (if isAppTy g typ && (tcrefOfAppTy g typ).IsProvided then stripTyEqns g typ else stripTyEqnsAndMeasureEqns g typ)
#else
    let typ = stripTyEqns g typ 
#endif

    match metadataOfTy g typ with 
#if EXTENSIONTYPING
    | ExtensionTypeMetadata info -> 
        let st = info.ProvidedType
        let superOpt = st.PApplyOption((fun st -> match st.BaseType with null -> None | t -> Some t),m)
        match superOpt with 
        | None -> None
        | Some super -> Some(Import.ImportProvidedType amap m super)
#endif
    | ILTypeMetadata (scoref,tdef) -> 
        let _,tinst = destAppTy g typ
        match tdef.Extends with 
        | None -> None
        | Some ilty -> Some (ImportType scoref amap m tinst ilty)

    | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 

        if isFSharpObjModelTy g typ  || isExnDeclTy g typ then 
            let tcref,_tinst = destAppTy g typ
            Some (instType (mkInstForAppTy g typ) (superOfTycon g tcref.Deref))
        elif isArrayTy g typ then
            Some g.system_Array_typ
        elif isRefTy g typ && not (isObjTy g typ) then 
            Some g.obj_ty
        elif isTupleStructTy g typ then 
            Some g.obj_ty
        else 
            None

let mkSystemCollectionsGenericIListTy g ty = TType_app(g.tcref_System_Collections_Generic_IList,[ty])


/// Collect the set of interface types supported by an F# type
let rec GetImmediateInterfacesOfType g amap m typ = 
    let itys = 
        if isAppTy g typ then
            let tcref,tinst = destAppTy g typ
            if tcref.IsMeasureableReprTycon then             
                [ match tcref.TypeReprInfo with 
                  | TMeasureableRepr reprTy -> 
                       for ity in GetImmediateInterfacesOfType g amap m reprTy do 
                          if isAppTy g ity then 
                              let itcref = tcrefOfAppTy g ity
                              if not (tyconRefEq g itcref g.system_GenericIComparable_tcref) && 
                                 not (tyconRefEq g itcref g.system_GenericIEquatable_tcref)  then 
                                   yield ity
                  | _ -> ()
                  yield mkAppTy g.system_GenericIComparable_tcref [typ]; 
                  yield mkAppTy g.system_GenericIEquatable_tcref [typ]]
            else
                match metadataOfTy g typ with 
#if EXTENSIONTYPING
                | ExtensionTypeMetadata info -> 
                    [ for ity in info.ProvidedType.PApplyArray((fun st -> st.GetInterfaces()), "GetInterfaces", m) do
                          yield Import.ImportProvidedType amap m ity ]
#endif
                | ILTypeMetadata (scoref,tdef) -> 
                    tdef.Implements |> ILList.toList |> List.map (ImportType scoref amap m tinst) 
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
let private FoldHierarchyOfTypeAux followInterfaces allowMultiIntfInst visitor g amap m typ acc = 
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
                   (GetImmediateInterfacesOfType g amap m typ) 
                      (loop ndeep g.obj_ty state)
            elif isTyparTy g typ then 
                let tp = destTyparTy g typ
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
            else 
                let state = 
                    if followInterfaces then 
                        List.foldBack 
                          (loop (ndeep+1)) 
                          (GetImmediateInterfacesOfType g amap m typ) 
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

/// Fold, do not follow interfaces
let FoldPrimaryHierarchyOfType f g amap m allowMultiIntfInst typ acc = FoldHierarchyOfTypeAux false allowMultiIntfInst f g amap m typ acc 

/// Fold, following interfaces
let FoldEntireHierarchyOfType f g amap m allowMultiIntfInst typ acc = FoldHierarchyOfTypeAux true allowMultiIntfInst f g amap m typ acc

/// Iterate, following interfaces
let IterateEntireHierarchyOfType f g amap m allowMultiIntfInst typ = FoldHierarchyOfTypeAux true allowMultiIntfInst (fun ty () -> f ty) g amap m typ () 

/// Search for one element satisfying a predicate, following interfaces
let ExistsInEntireHierarchyOfType f g amap m allowMultiIntfInst typ = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst (fun ty acc -> acc || f ty ) g amap m typ false 

/// Search for one element where a function returns a 'Some' result, following interfaces
let SearchEntireHierarchyOfType f g amap m typ = 
    FoldHierarchyOfTypeAux true AllowMultiIntfInstantiations.No
        (fun ty acc -> 
            match acc with 
            | None -> if f ty then Some(ty) else None 
            | Some _ -> acc) 
        g amap m typ None

/// Get all super types of the type, including the type itself
let AllSuperTypesOfType g amap m allowMultiIntfInst ty = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst (ListSet.insert (typeEquiv g)) g amap m ty [] 

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
    ExistsInEntireHierarchyOfType (HaveSameHeadType g typeToLookFor)  g amap m AllowMultiIntfInstantiations.No typeToSearchFrom
  
/// Check if a type exists somewhere in the hierarchy which has the given head type.
let ExistsHeadTypeInEntireHierarchy g amap m typeToSearchFrom tcrefToLookFor = 
    ExistsInEntireHierarchyOfType (HasHeadType g tcrefToLookFor) g amap m AllowMultiIntfInstantiations.No typeToSearchFrom
  

/// Read an Abstract IL type from metadata and convert to an F# type.
let ImportTypeFromMetadata amap m scoref tinst minst ilty = 
    ImportType scoref amap m (tinst@minst) ilty

        
/// Get the return type of an IL method, taking into account instantiations for type and method generic parameters, and
/// translating 'void' to 'None'.
let ImportReturnTypeFromMetaData amap m ty scoref tinst minst =
    match ty with 
    | ILType.Void -> None
    | retTy ->  Some (ImportTypeFromMetadata amap m scoref tinst minst retTy)

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
    if n0 <> n1 then error(Error((FSComp.SR.tcInvalidTypeArgumentCount(n0,n1)),m));
    if n2 <> n3 then error(Error((FSComp.SR.tcInvalidTypeArgumentCount(n2,n3)),m));

    // The real code.. 
    let renaming,tptys = mkTyparToTyparRenaming tpsorig tps
    let tprefInst = mkTyparInst formalEnclosingTypars tinst @ renaming
    (tpsorig,tps) ||> List.iter2 (fun tporig tp -> tp.FixupConstraints (CopyTyparConstraints  m tprefInst tporig)) ;
    renaming,tptys


//-------------------------------------------------------------------------
// Predicates and properties on values and members
 
/// Check if an F#-declared member value is a virtual method
let MemberRefIsVirtual (vref:ValRef) = 
    let flags = vref.MemberInfo.Value.MemberFlags
    flags.IsDispatchSlot || flags.IsOverrideOrExplicitImpl

/// Check if an F#-declared member value is an 'override' or explicit member implementation
// REVIEW: We should not need the notion of "DefiniteFSharpOverride" at all
// REVIEW: MemberRefIsVirtual (vref:ValRef) = IsDispatchSlot || IsOverrideOrExplicitImpl.
//         So not IsDispatchSlot implies IsOverrideOrExplicitImpl
//         Q: why is this not "flags.IsOverrideOrExplicitImpl"?
//         Q: could an override (with nonNil membInfo.ImplementedSlotSigs) ever have flags.IsOverrideOrExplicitImpl = false?
let private MemberRefIsDefiniteFSharpOverride (vref:ValRef) = 
    let membInfo = vref.MemberInfo.Value   
    let flags = membInfo.MemberFlags
    not flags.IsDispatchSlot && (flags.IsOverrideOrExplicitImpl || nonNil membInfo.ImplementedSlotSigs)

/// Check if an F#-declared member value is a dispatch slot
let MemberRefIsDispatchSlot (vref:ValRef) =  
    let membInfo = vref.MemberInfo.Value
    membInfo.MemberFlags.IsDispatchSlot 

type ValRef with 
    /// Indicates if an F#-declared function or member value is a CLIEvent property compiled as a .NET event
    member x.IsFSharpEventProperty g = 
        x.IsMember && CompileAsEvent g x.Attribs && not x.IsExtensionMember

//-------------------------------------------------------------------------
// ILTypeInfo

/// Describes an F# use of an IL type, including the type instantiation associated with the type at a particular usage point.
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
            let scoref,enc,tdef = tcref.ILTyconInfo
            let tref = mkRefForNestedILTypeDef scoref (enc,tdef)
            ILTypeInfo(tcref,tref,tinst,tdef)
        else 
            failwith "ILTypeInfo.FromType"

//-------------------------------------------------------------------------
// ParamNameAndType, ParamData

[<NoComparison; NoEquality>]
type ParamNameAndType = ParamNameAndType of string option * TType

let ParamNameAndTypeOfArgInfo (ty,argInfo : ArgReprInfo) = ParamNameAndType(Option.map textOfId argInfo.Name, ty)

let ParamNameAndTypesOfMember g vref = ArgInfosOfMember g vref |> List.mapSquared ParamNameAndTypeOfArgInfo

let InstParamNameAndType inst (ParamNameAndType(nm,ty)) = ParamNameAndType(nm, instType inst ty)

let InstParamNameAndTypes inst paramTypes = paramTypes |> List.mapSquared (InstParamNameAndType inst)

/// The caller-side value for the optional arg, is any 
type OptionalArgCallerSideValue = 
    | Constant of IL.ILFieldInit
    | DefaultValue
    | MissingValue
    | WrapperForIDispatch 
    | WrapperForIUnknown
    | PassByRef of TType * OptionalArgCallerSideValue
    
type OptionalArgInfo = 
    /// The argument is not optional
    | NotOptional
    /// The argument is optional, and is an F# callee-side optional arg 
    | CalleeSide
    /// The argument is optional, and is a caller-side .NET optional or default arg 
    | CallerSide of OptionalArgCallerSideValue 
    member x.IsOptional = match x with CalleeSide | CallerSide  _ -> true | NotOptional -> false 

[<NoComparison; NoEquality>]
type ParamData = 
    /// ParamData(isParamArray, isOut, optArgInfo, nameOpt, ttype)
    ParamData of bool * bool * OptionalArgInfo * string option * TType

// Compute the OptionalArgInfo for an IL parameter
let OptionalArgInfoOfILParameter g amap m ilScope ilTypeInst (ilParam: ILParameter) = 
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
                    if TryFindILAttribute g.attrib_IDispatchConstantAttribute ilParam.CustomAttrs then WrapperForIDispatch
                    elif TryFindILAttribute g.attrib_IUnknownConstantAttribute ilParam.CustomAttrs then WrapperForIUnknown
                    else MissingValue
                else 
                    DefaultValue
            CallerSide (analyze (ImportTypeFromMetadata amap m ilScope ilTypeInst [] ilParam.Type))
        | Some v -> 
            CallerSide (Constant v)
    else 
        NotOptional

let ConstantObjToILFieldInit m (v:obj) = 
    if v = null then ILFieldInit.Null 
    else
    let objTy = v.GetType()
    let v = 
        if objTy.IsEnum then
            let fi = objTy.GetField("value__")
            fi.GetValue(v)
        else v
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


#if EXTENSIONTYPING
// Compute the OptionalArgInfo for a provided parameter. Same logic as OptionalArgInfoOfILParameter
// except we do not aply the Visual Basic rules for IDispatchConstant and IUnknownConstant to optional 
// provided parameters.
let OptionalArgInfoOfProvidedParameter g amap m (provParam : Tainted<ProvidedParameterInfo>) = 
    if provParam.PUntaint((fun p -> p.IsOptional),m) then 
        match provParam.PUntaint((fun p ->  p.RawDefaultValue),m) with 
        | null -> 
            // Do a type-directed analysis of the IL type to determine the default value to pass.
            let rec analyze ty = 
                if isByrefTy g ty then 
                    let ty = destByrefTy g ty
                    PassByRef (ty, analyze ty)
                elif isObjTy g ty then MissingValue
                else  DefaultValue

            let pty = Import.ImportProvidedType amap m (provParam.PApply((fun p -> p.ParameterType),m))
            CallerSide (analyze pty)
        | v -> 
            CallerSide (Constant (ConstantObjToILFieldInit m v))
    else 
        NotOptional

//-------------------------------------------------------------------------
// Some helper functions

let GetAndSanityCheckProviderMethod m (mi: Tainted<'T :> ProvidedMemberInfo>) (get : 'T -> ProvidedMethodInfo) err = 
    match mi.PApply((fun mi -> (get mi :> ProvidedMethodBase)),m) with 
    | Tainted.Null -> error(Error(err(mi.PUntaint((fun mi -> mi.Name),m),mi.PUntaint((fun mi -> mi.DeclaringType.Name),m)),m))   
    | meth -> meth


let RepresentativeMethodInfoOfPropertyInfo (pi:Tainted<ProvidedPropertyInfo>) m =
    if pi.PUntaint((fun pi -> pi.CanRead), m) then 
        GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetGetMethod()) FSComp.SR.etPropertyCanReadButHasNoGetter
    elif pi.PUntaint((fun pi -> pi.CanWrite), m) then 
        GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetSetMethod()) FSComp.SR.etPropertyCanWriteButHasNoSetter
    else 
        error(Error(FSComp.SR.etPropertyNeedsCanWriteOrCanRead(pi.PUntaint((fun mi -> mi.Name),m),pi.PUntaint((fun mi -> mi.DeclaringType.Name),m)),m))   

#endif

/// Split the type of an F# member value into 
///    - the type parameters associated with method but matching those of the enclosing type
///    - the type parameters associated with a generic method
///    - the return type of the method
///    - the actual type arguments of the enclosing type.
let AnalyzeTypeOfMemberVal g (typ,vref) = 
    let mamberAllTypars,_,retTy,_ = GetTypeOfMemberInMemberForm g vref
    
    let parentTyArgs = argsOfAppTy g typ
    let memberParentTypars,memberMethodTypars = List.chop parentTyArgs.Length mamberAllTypars

    memberParentTypars,memberMethodTypars,retTy,parentTyArgs


//-------------------------------------------------------------------------
// ILMethInfo


/// Describes an F# use of an IL method.
[<NoComparison; NoEquality>]
type ILMethInfo =
    /// ILMethInfo(ilDeclaringTypeInfo, extensionMethodInfo, ilMethodDef, ilGenericMethodTyArgs)
    ///
    /// Describes an F# use of an IL method.
    | ILMethInfo of ILTypeInfo * ILTypeRef option * ILMethodDef * Typars  

    /// ILFSMethInfo(DeclaringType, FSharpObjKind, extensionMethodInfo, ilMethodDef)
    /// - DeclaringType refers apparent parent type
    /// - FSharpObjKind Indicates whether the type declaration is a class, interface, enum, delegate or struct   
    /// Describes an F# use of an IL extension method on F# object.
    | ILFSMethInfo of TyconRef * TyconObjModelKind * ILTypeRef option * ILMethodDef

    /// Get the declaring type of the method as an ILTypeInfo. If this is an extension method, this is apparent type, i.e. the 
    /// logical type being extended. If this is an extension method on a F# type, nothing returns.
    member x.ILTypeInfo = match x with ILMethInfo(tinfo,_,_,_) -> Some tinfo | ILFSMethInfo _ -> None 

    /// Get the Abstract IL metadata associated with the method.
    member x.RawMetadata = match x with ILMethInfo(_,_,md,_) -> md | ILFSMethInfo(_,_,_,md) -> md

    /// Get the actual IL parent of a C#-style extension member with IL backing metadata
    member x.ExtensionMethodInfo = match x with ILMethInfo(_,extInfo,_,_) -> extInfo | ILFSMethInfo(_,_,extInfo,_) -> extInfo

    /// Get a reference to the named declaring type associated with the method, as an ILTypeRef
    member x.ILTypeRef = match x with ILMethInfo(tinfo,_,_,_) -> tinfo.ILTypeRef | ILFSMethInfo _ -> failwith "cannot happen"

    /// Get the IL name of the method
    member x.ILName       = x.RawMetadata.Name

    /// Indicates if the method is an extension method
    member x.IsCSharpExtensionMethod = x.ExtensionMethodInfo.IsSome

    /// Get the declaring type of the method. If this is a C# extension method then this is the C# type
    /// holding the static member that is the extension method.
    member x.ActualILTypeRef   = 
        match x.ExtensionMethodInfo with 
        | None -> x.ILTypeRef
        | Some info -> info

    /// Get the instantiation of the declaring type of the method. If this is a C# extension method then this is empty.
    member x.ActualTypeInst = 
        match x.ExtensionMethodInfo with 
        | None -> match x.ILTypeInfo with Some(tinfo) -> tinfo.TypeInst | None -> []
        | Some _info -> []


    /// Get the Abstract IL scope information associated with interpreting the Abstract IL metadata that backs this method.
    member x.MetadataScope   = x.ActualILTypeRef.Scope
    
    /// Get the Abstract IL metadata corresponding to the parameters of the method. If this is a C# extension
    /// method then drop the object argument.
    member x.ParamMetadata = 
        let ps = x.RawMetadata.Parameters |> ILList.toList
        if x.IsCSharpExtensionMethod then List.tail ps else ps

    /// Get the number of parameters of the method
    member x.NumParams = x.ParamMetadata.Length
    
    /// Get the number of generic parameters of the method. This includes all type parameters even if this is a C# extension method extending a generic type.
    member x.GenericArity = x.RawMetadata.GenericParams.Length 
   
    /// Indicates if the method is a constructor
    member x.IsConstructor = x.RawMetadata.IsConstructor 

    /// Indicates if the method is a class initializer.
    member x.IsClassConstructor = x.RawMetadata.IsClassInitializer

    /// Indicates if the method has protected accessibility,
    member x.IsProtectedAccessibility = 
        let md = x.RawMetadata 
        not md.IsConstructor &&
        not md.IsClassInitializer &&
        (md.Access = ILMemberAccess.Family)

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
        not x.IsCSharpExtensionMethod &&  // all C# extension methods are instance
        x.RawMetadata.CallingConv.IsStatic

    /// Does it have the .NET IL 'newslot' flag set, and is also a virtual?
    member x.IsNewSlot = 
        match x.RawMetadata.mdKind with 
        | MethodKind.Virtual vinfo -> vinfo.IsNewSlot 
        | _ -> false
    
    /// Does it appear to the user as an instance method?
    member x.IsInstance = not x.IsConstructor &&  not x.IsStatic

    /// Get the argument types of the the IL method. If this is a C# extension method then drop the object argument.
    member x.GetParamTypes(amap,m,minst) = 
        x.ParamMetadata |> List.map (fun p -> ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type) 

    /// Get all the argument types of the IL method. Include the object argument even if this is a C# extension method.
    member x.GetRawArgTypes(amap,m,minst) = 
        // This includes, for example, C# extension methods.
        x.RawMetadata.Parameters |> ILList.toList |> List.map (fun p -> ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type) 

    /// Get info about the arguments of the IL method. If this is a C# extension method then drop the object argument.
    member x.GetParamNamesAndTypes(amap,m,minst) = 
        x.ParamMetadata |> List.map (fun p -> ParamNameAndType(p.Name, ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type) )

    /// Get the declaring type of the method as an F# type.
    member x.EnclosingType = match x with ILMethInfo(tinfo,_,_,_) -> tinfo.ToType | ILFSMethInfo(t,_,_,_) -> TType_app(t,[])

    /// Get a reference to the method (dropping all generic instantiations), as an Abstract IL ILMethodRef.
    member minfo.ILMethodRef = 
        let mref = mkRefToILMethod (minfo.ActualILTypeRef,minfo.RawMetadata)
        rescopeILMethodRef minfo.MetadataScope mref 

    /// Indicates if the method is marked as a DllImport (a PInvoke). This is done by looking at the IL custom attributes on 
    /// the method.
    member minfo.IsDllImport g = 
        let (AttribInfo(tref,_)) = g.attrib_DllImportAttribute
        minfo.RawMetadata.CustomAttrs |> TryDecodeILAttribute g tref (Some(tref.Scope))  |> isSome


    /// Get the (zero or one) 'self'/'this'/'object' arguments associated with an IL method. An instance extension method returns
    /// one object argument.
    member x.GetObjArgTypes(amap, m, minst) =
        // all C# extension methods are instance
        if x.IsCSharpExtensionMethod then 
            x.RawMetadata.Parameters |> ILList.toList |> List.head |> (fun p -> [ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type]) 
        elif x.IsInstance then 
            [x.EnclosingType]
        else
            []

    /// Get the compiled return type of an ILMethInfo, where 'void' is None.
    member minfo.GetCompiledReturnTy (amap, m, minst) =
        ImportReturnTypeFromMetaData amap m minfo.RawMetadata.Return.Type minfo.MetadataScope minfo.ActualTypeInst minst 

    /// Get the F# view of the return type of an ILMethInfo, where 'void' is 'unit'.
    member minfo.GetFSharpReturnTy (amap, m, minst) = 
        minfo.GetCompiledReturnTy(amap, m, minst)
        |> GetFSharpViewOfReturnType amap.g

//-------------------------------------------------------------------------
// ExtensionMethodPriority


/// Describes the sequence order of the introduction of an extension method. Extension methods that are introduced
/// later through 'open' get priority in overload resolution.
type ExtensionMethodPriority = uint64



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
    | ProvidedMeth of TcGlobals * Tainted<ProvidedMethodBase> * Import.ImportMap * range
#endif

    /// Get the enclosing ("parent"/"declaring") type of the method info. If this is an extension member,
    /// then this is the apparent parent.
    member x.EnclosingType = 
        match x with
        | ILMeth(_g,x,_) -> x.EnclosingType
        | FSMeth(_g,typ,_,_) -> typ
        | DefaultStructCtor(_g,typ) -> typ
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,amap,m) -> 
              Import.ImportProvidedType amap m (mi.PApply((fun mi -> mi.DeclaringType),m))
#endif

     /// Get the extension method priority of the method, if it has one.
    member x.Priority = 
        match x with
        | ILMeth(_,_,Some pri) -> pri
        | FSMeth(_,_,_,Some pri) -> pri
        | _ -> System.UInt64.MaxValue // all others take prioity over extension members

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
     /// If this is an extension member, then this is the actual instantiation of the apparent parent 
     /// of the member.
    member x.ActualTypeInst = 
        match x with 
        | ILMeth(_g,y,_) -> y.ActualTypeInst
        | FSMeth(g,_,_,_) | DefaultStructCtor(g,_) -> argsOfAppTy g x.EnclosingType
#if EXTENSIONTYPING
        | ProvidedMeth(g,_,_,_) -> argsOfAppTy g x.EnclosingType
#endif

     /// Get the TcGlobals value that governs the method declaration
    member x.TcGlobals = 
        match x with 
        | ILMeth(g,_,_) -> g
        | FSMeth(g,_,_,_) -> g
        | DefaultStructCtor (g,_) -> g
#if EXTENSIONTYPING
        | ProvidedMeth(g,_,_,_) -> g
#endif


     /// Get the formal generic method parameters for the method as a list of type variables.
    member x.FormalMethodTypars = 
        match x with 
        | ILMeth(_,tinfo,_) ->  match tinfo with ILMethInfo(_,_,_,mtps) -> mtps | _ -> []
        | FSMeth(g,typ,vref,_) ->  
           let allTypars,_ = destTopForallTy g vref.ValReprInfo.Value vref.Type
           let parentTyArgs = argsOfAppTy g typ
           let methodTypars = List.drop parentTyArgs.Length allTypars
           methodTypars
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

     /// Try to get an F# ValRef for the method
    member x.ArbitraryValRef = 
        match x with 
        | FSMeth(_g,_,vref,_) -> Some vref
        | _ -> None

    member x.NumArgs = 
        match x with 
        | ILMeth(_g,x,_) -> [x.NumParams]
        | FSMeth(g,_,vref,_) -> ArgInfosOfMember  g vref |> List.map List.length 
        | DefaultStructCtor _ -> [0]
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> [mi.PUntaint((fun mi -> mi.GetParameters().Length),m)] // Why is this a list? Answer: because the method might be curried
#endif

    member x.IsCurried = x.NumArgs.Length > 1

    /// Does the method appear to the user as an instance method?
    member x.IsInstance = 
        match x with 
        | ILMeth(_,x,_) -> x.IsInstance
        | FSMeth(_,_,vref,_) -> vref.IsInstanceMember
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> not mi.IsConstructor && not mi.IsStatic),m)
#endif


    member x.GenericArity = 
        match x with 
        | ILMeth(_g,x,_) -> x.GenericArity
        | FSMeth(g,typ,vref,_) -> 
            let _,memberMethodTypars,_,_ = AnalyzeTypeOfMemberVal g (typ,vref)
            memberMethodTypars.Length
        | DefaultStructCtor _ -> 0
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> 
            mi.PUntaint((fun mi -> if mi.IsGenericMethod then mi.GetGenericArguments().Length else 0),m)
#endif

    member x.IsProtectedAccessiblity = 
        match x with 
        | ILMeth(_g,x,_) -> x.IsProtectedAccessibility
        | FSMeth _ -> false
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsFamily), m)
#endif

    member x.IsVirtual =
        match x with 
        | ILMeth(_,x,_) -> x.IsVirtual
        | FSMeth(_,_,vref,_) -> MemberRefIsVirtual vref
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsVirtual), m)
#endif

    member x.IsConstructor = 
        match x with 
        | ILMeth(_g,x,_) -> x.IsConstructor
        | FSMeth(_g,_,vref,_) ->
            let flags = (Option.get vref.MemberInfo).MemberFlags
            (flags.MemberKind = MemberKind.Constructor)
        | DefaultStructCtor _ -> true
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsConstructor), m)
#endif

    member x.IsClassConstructor =
        match x with 
        | ILMeth(_g,x,_) -> x.IsClassConstructor
        | FSMeth _ -> false
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsConstructor && mi.IsStatic), m) // Note: these are never public anyway
#endif

    member meth.IsDispatchSlot = 
        match meth with 
        | ILMeth(_g,x,_) -> 
            x.IsVirtual
        | FSMeth(g,_,vref,_) as x -> 
            isInterfaceTy g x.EnclosingType  || 
            vref.MemberInfo.Value.MemberFlags.IsDispatchSlot
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth _ -> meth.IsVirtual // Note: follow same implementation as ILMeth
#endif


    member x.IsFinal = 
        not x.IsVirtual || 
        match x with 
        | ILMeth(_g,x,_) -> x.IsFinal
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
        | ILMeth(_g,x,_) -> x.IsAbstract
        | FSMeth(g,_,vref,_)  -> 
            isInterfaceTy g minfo.EnclosingType  || 
            MemberRefIsDispatchSlot vref
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsAbstract), m)
#endif

    member x.IsNewSlot = 
        isInterfaceTy x.TcGlobals x.EnclosingType  || 
        (x.IsVirtual && 
          (match x with 
           | ILMeth(_,x,_) -> x.IsNewSlot
           | FSMeth(_,_,vref,_) -> MemberRefIsDispatchSlot vref
#if EXTENSIONTYPING
           | ProvidedMeth(_,mi,_,m) -> mi.PUntaint((fun mi -> mi.IsHideBySig), m) // REVIEW: Check this is correct
#endif
           | DefaultStructCtor _ -> false))


    member x.IsDefiniteFSharpOverride = 
        match x with 
        | ILMeth(_,_,_) -> false
        | FSMeth(_,_,vref,_) -> MemberRefIsDefiniteFSharpOverride vref
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth _ -> false 
#endif

    member x.IsExtensionMember = 
        match x with 
        | ILMeth(_,x,_) -> x.ExtensionMethodInfo.IsSome
        | FSMeth(_,_,vref,_) -> vref.IsExtensionMember
        | DefaultStructCtor _ -> false
#if EXTENSIONTYPING
        | ProvidedMeth _ -> false // We don't support provided extension members.
#endif

    member x.IsFSharpEventProperty = 
        match x with 
        | FSMeth(g,_,vref,_)  -> vref.IsFSharpEventProperty(g)
#if EXTENSIONTYPING
        | ProvidedMeth _ -> false 
#endif
        | _ -> false

    member x.IsNullary = (x.NumArgs = [0])

    member x.IsStruct = 
        isStructTy x.TcGlobals x.EnclosingType



//-------------------------------------------------------------------------
// ILFieldInfo


/// Represents a single use of a IL or provided field from one point in an F# program
[<NoComparison; NoEquality>]
type ILFieldInfo = 
     /// Represents a single use of a field backed by Abstract IL metadata
    | ILFieldInfo of ILTypeInfo * ILFieldDef // .NET IL fields 
#if EXTENSIONTYPING
     /// Represents a single use of a field backed by provided metadata
    | ProvidedField of TcGlobals * Tainted<ProvidedFieldInfo> * Import.ImportMap * range
#endif

    /// Get the enclosing ("parent"/"declaring") type of the field. 
    member x.EnclosingType = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.ToType
#if EXTENSIONTYPING
        | ProvidedField(_,fi,amap,m) -> (Import.ImportProvidedType amap m (fi.PApply((fun fi -> fi.DeclaringType),m)))
#endif

     /// Get a reference to the declaring type of the field as an ILTypeRef
    member x.ILTypeRef = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.ILTypeRef
#if EXTENSIONTYPING
        | ProvidedField(_,fi,amap,m) -> (Import.ImportProvidedTypeAsILType amap m (fi.PApply((fun fi -> fi.DeclaringType),m))).TypeRef
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
        | ProvidedField(_,fi,_,m) -> fi.PUntaint((fun fi -> fi.Name),m)
#endif

     /// Indicates if the field is readonly (in the .NET/C# sense of readonly)
    member x.IsInitOnly = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsInitOnly
#if EXTENSIONTYPING
        | ProvidedField(_,fi,_,m) -> fi.PUntaint((fun fi -> fi.IsInitOnly),m)
#endif



     /// Indicates if the field is a member of a struct or enum type
    member x.IsValueType = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.IsValueType
#if EXTENSIONTYPING
        | ProvidedField(g,_,_,_) -> isStructTy g x.EnclosingType
#endif

     /// Indicates if the field is static
    member x.IsStatic = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsStatic
#if EXTENSIONTYPING
        | ProvidedField(_,fi,_,m) -> fi.PUntaint((fun fi -> fi.IsStatic),m)
#endif

     /// Indicates if the field has the 'specialname' property in the .NET IL
    member x.IsSpecialName = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsSpecialName
#if EXTENSIONTYPING
        | ProvidedField(_,fi,_,m) -> fi.PUntaint((fun fi -> fi.IsSpecialName),m)
#endif
    
     /// Indicates if the field is a literal field with an associated literal value
    member x.LiteralValue = 
        match x with 
        | ILFieldInfo(_,pd) -> if pd.IsLiteral then pd.LiteralValue else None
#if EXTENSIONTYPING
        | ProvidedField(_,fi,_,m) -> 
            if fi.PUntaint((fun fi -> fi.IsLiteral),m) then 
                Some (ConstantObjToILFieldInit m (fi.PUntaint((fun fi -> fi.GetRawConstantValue()),m)))
            else
                None
#endif
                                        
     /// Get the type of the field as an IL type
    member x.ILFieldType = 
        match x with 
        | ILFieldInfo (_,fdef) -> fdef.Type
#if EXTENSIONTYPING
        | ProvidedField(_,fi,amap,m) -> Import.ImportProvidedTypeAsILType amap m (fi.PApply((fun fi -> fi.FieldType),m))
#endif

     /// Get the type of the field as an F# type
    member x.FieldType(amap,m) = 
        match x with 
        | ILFieldInfo (tinfo,fdef) -> ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] fdef.Type
#if EXTENSIONTYPING
        | ProvidedField(_,fi,amap,m) -> Import.ImportProvidedType amap m (fi.PApply((fun fi -> fi.FieldType),m))
#endif

     /// Get an (uninstantiated) reference to the field as an Abstract IL ILFieldRef
    member x.ILFieldRef = rescopeILFieldRef x.ScopeRef (mkILFieldRef(x.ILTypeRef,x.FieldName,x.ILFieldType))
    override x.ToString() =  x.FieldName


//-------------------------------------------------------------------------
// RecdFieldInfo


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
    

//-------------------------------------------------------------------------
// UnionCaseInfo


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



//-------------------------------------------------------------------------
// ILPropInfo

/// Describes an F# use of a property backed by Abstract IL metadata
[<NoComparison; NoEquality>]
type ILPropInfo = 
    | ILPropInfo of ILTypeInfo * ILPropertyDef 

    /// Get the declaring IL type of the property, including any generic instantiation
    member x.ILTypeInfo = match x with (ILPropInfo(tinfo,_)) -> tinfo

    /// Get the raw Abstract IL metadata for the property
    member x.RawMetadata = match x with (ILPropInfo(_,pd)) -> pd

    /// Get the name of the property
    member x.PropertyName = x.RawMetadata.Name

    /// Gets the ILMethInfo of the 'get' method for the property
    member x.GetterMethod = 
        assert x.HasGetter
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata (Option.get x.RawMetadata.GetMethod)
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 

    /// Gets the ILMethInfo of the 'set' method for the property
    member x.SetterMethod = 
        assert x.HasSetter
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata (Option.get x.RawMetadata.SetMethod)
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 
          
    /// Indicates if the property has a 'get' method
    member x.HasGetter = isSome x.RawMetadata.GetMethod 

    /// Indicates if the property has a 'set' method
    member x.HasSetter = isSome x.RawMetadata.SetMethod 

    /// Indicates if the property is static
    member x.IsStatic = (x.RawMetadata.CallingConv = ILThisConvention.Static) 

    member x.IsVirtual = 
        (x.HasGetter && x.GetterMethod.IsVirtual) ||
        (x.HasSetter && x.SetterMethod.IsVirtual) 

    member x.IsNewSlot = 
        (x.HasGetter && x.GetterMethod.IsNewSlot) ||
        (x.HasSetter && x.SetterMethod.IsNewSlot) 

    member x.GetParamNamesAndTypes(amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = x
        pdef.Args |> ILList.toList |> List.map (fun ty -> ParamNameAndType(None, ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] ty) )

    member x.GetParamTypes(amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = x
        pdef.Args |> ILList.toList |> List.map (fun ty -> ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] ty) 

    member x.GetPropertyType (amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = x
        ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] pdef.Type

    override x.ToString() = x.ILTypeInfo.ToString() + "::" + x.PropertyName

//-------------------------------------------------------------------------
// PropInfo

/// Describes an F# use of a property 
[<NoComparison; NoEquality>]
type PropInfo = 
    /// An F# use of a property backed by F#-declared metadata
    | FSProp of TcGlobals * TType * ValRef option * ValRef option
    /// An F# use of a property backed by Abstract IL metadata
    | ILProp of TcGlobals * ILPropInfo
#if EXTENSIONTYPING
    /// An F# use of a property backed by provided metadata
    | ProvidedProp of TcGlobals * Tainted<ProvidedPropertyInfo> * Import.ImportMap * range
#endif

    member x.HasDirectXmlComment =
        match x with
        | FSProp(g,_,Some(vref),_)
        | FSProp(g,_,_,Some(vref)) -> valRefInThisAssembly g.compilingFslib vref
#if EXTENSIONTYPING
        | ProvidedProp _ -> true
#endif
        | _ -> false

    member x.PropertyName = 
        match x with 
        | ILProp(_,x) -> x.PropertyName
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.PropertyName
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> pi.PUntaint((fun pi -> pi.Name),m)
#endif
        | FSProp _ -> failwith "unreachable"

    member x.HasGetter = 
        match x with
        | ILProp(_,x) -> x.HasGetter
        | FSProp(_,_,x,_) -> isSome x 
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> pi.PUntaint((fun pi -> pi.CanRead),m)
#endif

    member x.HasSetter = 
        match x with
        | ILProp(_,x) -> x.HasSetter
        | FSProp(_,_,_,x) -> isSome x 
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> pi.PUntaint((fun pi -> pi.CanWrite),m)
#endif

    member x.EnclosingType = 
        match x with 
        | ILProp(_,x) -> x.ILTypeInfo.ToType
        | FSProp(_,typ,_,_) -> typ
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,amap,m) -> 
            Import.ImportProvidedType amap m (pi.PApply((fun pi -> pi.DeclaringType),m)) 
#endif


    /// True if the getter (or, if absent, the setter) is a virtual method
    // REVIEW: for IL properties this is getter OR setter. For F# properties it is getter ELSE setter
    member x.IsVirtualProperty = 
        match x with 
        | ILProp(_,x) -> x.IsVirtual
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> MemberRefIsVirtual vref
        | FSProp _-> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> 
            let mi = RepresentativeMethodInfoOfPropertyInfo pi m
            mi.PUntaint((fun mi -> mi.IsVirtual), m)
#endif

    
    // REVIEW: this doesn't accord precisely with the IsNewSlot definition for members
    member x.IsNewSlot = 
        match x with 
        | ILProp(_,x) -> x.IsNewSlot
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> MemberRefIsDispatchSlot vref
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> 
            let mi = RepresentativeMethodInfoOfPropertyInfo pi m
            mi.PUntaint((fun mi -> mi.IsHideBySig), m)
#endif


    /// True if the getter (or, if absent, the setter) for the property is a dispatch slot
    // REVIEW: for IL properties this is getter OR setter. For F# properties it is getter ELSE setter
    member x.IsDispatchSlot = 
        match x with 
        | ILProp(_,x) -> x.IsVirtual

        | FSProp(g,typ,Some vref,_) 
        | FSProp(g,typ,_, Some vref) ->
            isInterfaceTy g typ  || 
            (let membInfo = (Option.get vref.MemberInfo)
             membInfo.MemberFlags.IsDispatchSlot)
        | FSProp _ -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> 
            let mi = RepresentativeMethodInfoOfPropertyInfo pi m
            mi.PUntaint((fun mi -> mi.IsVirtual), m)
#endif

    member x.IsStatic =
        match x with 
        | ILProp(_,x) -> x.IsStatic
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> not vref.IsInstanceMember
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> 
            (RepresentativeMethodInfoOfPropertyInfo pi m).PUntaint((fun mi -> mi.IsStatic), m)
#endif

    member x.IsDefiniteFSharpOverride = 
        match x with 
        | ILProp _ -> false
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_,Some vref) -> MemberRefIsDefiniteFSharpOverride vref
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp _ -> false
#endif

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
        | ProvidedProp(_,pi,_,m) -> 
            pi.PUntaint((fun pi -> pi.GetIndexParameters().Length), m)>0
#endif

    member x.IsFSharpEventProperty = 
        match x with 
        | FSProp(g,_,Some vref,None)  -> vref.IsFSharpEventProperty(g)
#if EXTENSIONTYPING
        | ProvidedProp _ -> false
#endif
        | _ -> false

    // Property infos can combine getters and setters, assuming they are consistent w.r.t. 'virtual', indexer argument types etc.
    // When checking consistency we split these apart
    member x.DropSetter = 
        match x with 
        | FSProp(g,typ,Some vref,_)  -> FSProp(g,typ,Some vref,None)
        | _ -> x


    member x.DropGetter = 
        match x with 
        | FSProp(g,typ,_,Some vref)  -> FSProp(g,typ,None,Some vref)
        | _ -> x

    member x.XmlDoc = 
        match x with 
        | ILProp _ -> XmlDoc.Empty
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.XmlDoc
        | FSProp(_,_,None,None) -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> 
            XmlDoc (pi.PUntaint((fun pix -> (pix :> IProvidedCustomAttributeProvider).GetXmlDocAttributes(pi.TypeProvider.PUntaintNoFailure(id))), m))
#endif

    member x.TcGlobals = 
        match x with 
        | ILProp(g,_) -> g 
        | FSProp(g,_,_,_) -> g 
#if EXTENSIONTYPING
        | ProvidedProp(g,_,_,_) -> g
#endif

    member x.IsValueType = isStructTy x.TcGlobals x.EnclosingType

    member x.ArbitraryValRef = 
        match x with 
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> Some vref
        | FSProp(_,_,None,None) -> failwith "unreachable"
        | _ -> None 

    member x.GetParamNamesAndTypes(amap,m) = 
        match x with 
        | ILProp (_,ilpinfo) -> ilpinfo.GetParamNamesAndTypes(amap,m)
        | FSProp (g,typ,Some vref,_) 
        | FSProp (g,typ,_,Some vref) -> 
            let memberParentTypars,_mtps,_retTy,parentTyArgs = AnalyzeTypeOfMemberVal amap.g (typ,vref)
            let inst = mkTyparInst memberParentTypars parentTyArgs
            ArgInfosOfPropertyVal g vref.Deref |> List.map (ParamNameAndTypeOfArgInfo >> InstParamNameAndType inst)
        | FSProp _ -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp (_,pi,_,m) -> 
            [ for p in pi.PApplyArray((fun pi -> pi.GetIndexParameters()), "GetIndexParameters", m) do
                let paramName = p.PUntaint((fun p -> match p.Name with null -> None | s -> Some s), m)
                let paramType = Import.ImportProvidedType amap m (p.PApply((fun p -> p.ParameterType), m))
                yield ParamNameAndType(paramName, paramType) ]
#endif
     
    member x.GetPropertyType (amap,m) = 
        match x with
        | ILProp (_,ilpinfo) -> ilpinfo.GetPropertyType (amap,m)
        | FSProp (g,typ,Some vref,_) 
        | FSProp (g,typ,_,Some vref) -> 
            let memberParentTypars,_,_,parentTyArgs = AnalyzeTypeOfMemberVal amap.g (typ,vref)
            let inst = mkTyparInst memberParentTypars parentTyArgs
            ReturnTypeOfPropertyVal g vref.Deref
            |> instType inst
            
        | FSProp _ -> failwith "unreachable"
#if EXTENSIONTYPING
        | ProvidedProp(_,pi,_,m) -> 
            Import.ImportProvidedType amap m (pi.PApply((fun pi -> pi.PropertyType),m))
#endif


    member x.GetParamDatas(amap,m) = 
        x.GetParamNamesAndTypes(amap,m)
        |> List.map (fun (ParamNameAndType(nm,pty)) -> ParamData(false,false,NotOptional,nm, pty))

    member x.GetParamTypes(amap,m) = 
      x.GetParamNamesAndTypes(amap,m) |> List.map (fun (ParamNameAndType(_,ty)) -> ty)

    member x.GetterMethod = 
        match x with
        | ILProp(g,x) -> ILMeth(g,x.GetterMethod,None)
        | FSProp(g,typ,Some vref,_) -> FSMeth(g,typ,vref,None) 
#if EXTENSIONTYPING
        | ProvidedProp(g,pi,amap,m) -> 
            let meth = GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetGetMethod()) FSComp.SR.etPropertyCanReadButHasNoGetter
            ProvidedMeth(g, meth, amap, m)

#endif
        | FSProp _ -> failwith "no getter method"

    member x.SetterMethod = 
        match x with
        | ILProp(g,x) -> ILMeth(g,x.SetterMethod,None)
        | FSProp(g,typ,_,Some vref) -> FSMeth(g,typ,vref,None)
#if EXTENSIONTYPING
        | ProvidedProp(g,pi,amap,m) -> 
            let meth = GetAndSanityCheckProviderMethod m pi (fun pi -> pi.GetSetMethod()) FSComp.SR.etPropertyCanWriteButHasNoSetter
            ProvidedMeth(g, meth, amap, m)
#endif
        | FSProp _ -> failwith "no setter method"


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
    member x.AddMethod =
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.AddMethod
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 

    /// Get the ILMethInfo describing the 'remove' method associated with the event
    member x.RemoveMethod =
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.RemoveMethod
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 

    /// Get the declaring type of the event as an ILTypeRef
    member x.TypeRef = x.ILTypeInfo.ILTypeRef

    /// Get the name of the event
    member x.Name = x.RawMetadata.Name

    /// Indicates if the property is static
    member x.IsStatic = x.AddMethod.IsStatic
    override x.ToString() = x.ILTypeInfo.ToString() + "::" + x.Name

//-------------------------------------------------------------------------
// Helpers for EventInfo

exception BadEventTransformation of range

/// Properties compatible with type IDelegateEvent and atributed with CLIEvent are special: we generate metadata and add/remove methods 
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
    | ProvidedEvent of TcGlobals * Import.ImportMap * Tainted<ProvidedEventInfo> * range
#endif

    member x.EnclosingType = 
        match x with 
        | ILEvent(_,e) -> e.ILTypeInfo.ToType 
        | FSEvent (_,p,_,_) -> p.EnclosingType
#if EXTENSIONTYPING
        | ProvidedEvent (_,amap,ei,m) -> Import.ImportProvidedType amap m (ei.PApply((fun ei -> ei.DeclaringType),m)) 
#endif

    member x.HasDirectXmlComment =
        match x with
        | FSEvent (_,p,_,_) -> p.HasDirectXmlComment 
#if EXTENSIONTYPING
        | ProvidedEvent _ -> true
#endif
        | _ -> false

    member x.XmlDoc = 
        match x with 
        | ILEvent _ -> XmlDoc.Empty
        | FSEvent (_,p,_,_) -> p.XmlDoc
#if EXTENSIONTYPING
        | ProvidedEvent (_,_,ei,m) -> 
            XmlDoc (ei.PUntaint((fun eix -> (eix :> IProvidedCustomAttributeProvider).GetXmlDocAttributes(ei.TypeProvider.PUntaintNoFailure(id))), m))
#endif

    member x.EventName = 
        match x with 
        | ILEvent(_,e) -> e.Name 
        | FSEvent (_,p,_,_) -> p.PropertyName
#if EXTENSIONTYPING
        | ProvidedEvent (_,_,ei,m) -> ei.PUntaint((fun ei -> ei.Name), m)
#endif

    member x.IsStatic = 
        match x with 
        | ILEvent(_,e) -> e.IsStatic 
        | FSEvent (_,p,_,_) -> p.IsStatic
#if EXTENSIONTYPING
        | ProvidedEvent (_,_,ei,m) -> 
            let meth = GetAndSanityCheckProviderMethod m ei (fun ei -> ei.GetAddMethod()) FSComp.SR.etEventNoAdd
            meth.PUntaint((fun mi -> mi.IsStatic), m)
#endif

    member x.TcGlobals = 
        match x with 
        | ILEvent(g,_) -> g
        | FSEvent(g,_,_,_) -> g
#if EXTENSIONTYPING
        | ProvidedEvent (g,_,_,_) -> g
#endif

    member x.IsValueType = isStructTy x.TcGlobals x.EnclosingType

    member x.GetAddMethod() = 
        match x with 
        | ILEvent(g,e) -> ILMeth(g,e.AddMethod,None)
        | FSEvent(g,p,addValRef,_) -> FSMeth(g,p.EnclosingType,addValRef,None)
#if EXTENSIONTYPING
        | ProvidedEvent (g,amap,ei,m) -> 
            let meth = GetAndSanityCheckProviderMethod m ei (fun ei -> ei.GetAddMethod()) FSComp.SR.etEventNoAdd
            ProvidedMeth(g, meth, amap, m)
#endif

    member x.GetRemoveMethod() = 
        match x with 
        | ILEvent(g,e) -> ILMeth(g,e.RemoveMethod,None)
        | FSEvent(g,p,_,removeValRef) -> FSMeth(g,p.EnclosingType,removeValRef,None)
#if EXTENSIONTYPING
        | ProvidedEvent (g,amap,ei,m) -> 
            let meth = GetAndSanityCheckProviderMethod m ei (fun ei -> ei.GetRemoveMethod()) FSComp.SR.etEventNoRemove
            ProvidedMeth(g, meth, amap, m)
#endif
    

    member x.ArbitraryValRef = 
        match x with 
        | FSEvent(_,_,addValRef,_) -> Some addValRef
        | _ ->  None

    member x.GetDelegateType(amap,m) = 
        match x with 
        | ILEvent(_,ILEventInfo(tinfo,edef)) -> 
            // Get the delegate type associated with an IL event, taking into account the instantiation of the
            // declaring type.
            if isNone edef.Type then error (nonStandardEventError x.EventName m);
            ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] edef.Type.Value

        | FSEvent(g,p,_,_) -> 
            FindDelegateTypeOfPropertyEvent g amap x.EventName m (p.GetPropertyType(amap,m))
#if EXTENSIONTYPING
        | ProvidedEvent (_,_,ei,_) -> 
            Import.ImportProvidedType amap m (ei.PApply((fun ei -> ei.EventHandlerType), m))
#endif


type ILMethInfo with 
    /// Build IL method infos.  
    static member Create (amap, m, tinfo:ILTypeInfo, extInfo:ILTypeRef option, extMethPri, md: ILMethodDef) =     
        let tinst,scoref =  
            match extInfo with 
            | None -> 
                tinfo.TypeInst,tinfo.ILScopeRef
            | Some tref -> 
                // C# extension methods have no type typars
                [], tref.Scope
        let mtps = Import.ImportILGenericParameters (fun () -> amap) m scoref tinst md.GenericParams
        ILMeth (amap.g,ILMethInfo(tinfo,extInfo, md,mtps),extMethPri)



/// Tests whether two method infos have the same underlying definition.
/// Used to merge operator overloads collected from left and right of an operator constraint.
let MethInfosUseIdenticalDefinitions _g x1 x2 = 
    match x1,x2 with 
    | ILMeth(_,x1,_), ILMeth(_,x2,_) -> (x1.RawMetadata ===  x2.RawMetadata)
    | FSMeth(g,_,vref1,_), FSMeth(_,_,vref2,_)  -> valRefEq g vref1 vref2 
    | DefaultStructCtor(g,ty1), DefaultStructCtor(_,ty2) -> tyconRefEq g (tcrefOfAppTy g ty1) (tcrefOfAppTy g ty2) 
#if EXTENSIONTYPING
    | ProvidedMeth(_,mi1,_,_),ProvidedMeth(_,mi2,_,_)  -> ProvidedMethodBase.TaintedEquals (mi1, mi2)
#endif
    | _ -> false

/// Tests whether two property infos have the same underlying definition.
/// Uses the same techniques as pervious 'MethInfosUseIdenticalDefinitions'.
let PropInfosUseIdenticalDefinitions x1 x2 = 
    let optVrefEq g = function 
      | Some(v1), Some(v2) -> valRefEq g v1 v2
      | None, None -> true
      | _ -> false    
    match x1,x2 with 
    | ILProp(_, x1), ILProp(_, x2) -> (x1.RawMetadata === x2.RawMetadata)
    | FSProp(g, _, vrefa1, vrefb1), FSProp(_, _, vrefa2, vrefb2) ->
        (optVrefEq g (vrefa1, vrefa2)) && (optVrefEq g (vrefb1, vrefb2))
#if EXTENSIONTYPING
    | ProvidedProp(_,pi1,_,_), ProvidedProp(_,pi2,_,_) -> ProvidedPropertyInfo.TaintedEquals (pi1, pi2) 
#endif
    | _ -> false

/// Test whether two event infos have the same underlying definition.
let EventInfosUseIdenticalDefintions x1 x2 =
    match x1, x2 with
    | FSEvent(g, pi1, vrefa1, vrefb1), FSEvent(_, pi2, vrefa2, vrefb2) ->
        PropInfosUseIdenticalDefinitions pi1 pi2 && valRefEq g vrefa1 vrefa2 && valRefEq g vrefb1 vrefb2
    | ILEvent(_, x1), ILEvent(_, x2) -> (x1.RawMetadata === x2.RawMetadata)
#if EXTENSIONTYPING
    | ProvidedEvent (_,_,ei1,_), ProvidedEvent (_,_,ei2,_) -> ProvidedEventInfo.TaintedEquals (ei1, ei2)  
#endif
    | _ -> false
  
/// Calculates a hash code of method info. Note: this is a very imperfect implementation,
/// but it works decently for comparing methods in the language service...
let GetMethInfoHashCode mi = 
    match mi with 
    | ILMeth(_,x1,_) -> hash x1.RawMetadata.Name
    | FSMeth(_,_,vref,_) -> hash vref.LogicalName
    | DefaultStructCtor(_,_ty) -> 34892 // "ty" doesn't support hashing. We could use "hash (tcrefOfAppTy g ty).CompiledName" or 
                                       // something but we don't have a "g" parameter here yet. But this hash need only be very approximate anyway
#if EXTENSIONTYPING
    | ProvidedMeth(_,mi,_,_) -> ProvidedMethodInfo.TaintedGetHashCode(mi)
#endif

/// Calculates a hash code of property info (similar as previous)
let GetPropInfoHashCode mi = 
    match mi with 
    | ILProp(_, x1) -> hash x1.RawMetadata.Name
    | FSProp(_,_,vrefOpt1, vrefOpt2) -> 
        // Value to hash is option<string>*option<string>, which can be hashed efficiently
        let vth = (vrefOpt1 |> Option.map (fun vr -> vr.LogicalName), vrefOpt2 |> Option.map (fun vr -> vr.LogicalName))
        hash vth
#if EXTENSIONTYPING
    | ProvidedProp(_,pi,_,_) -> ProvidedPropertyInfo.TaintedGetHashCode(pi)
#endif

/// Calculates a hash code of event info (similar as previous)
let GetEventInfoHashCode mi = 
    match mi with 
    | ILEvent(_, x1) -> hash x1.RawMetadata.Name
    | FSEvent(_, pi, vref1, vref2) -> hash (GetPropInfoHashCode pi, vref1.LogicalName, vref2.LogicalName)
#if EXTENSIONTYPING
    | ProvidedEvent (_,_,ei,_) -> ProvidedEventInfo.TaintedGetHashCode(ei)
#endif

/// Apply a type instantiation to a method info, i.e. apply the instantiation to the enclosing type. 
let InstMethInfo amap m inst meth = 
    match meth with 
    | ILMeth(_g,x,pri) ->
        match x with 
        | ILMethInfo(tinfo,_,_,_) -> ILMethInfo.Create(amap, m, tinfo.Instantiate inst, x.ExtensionMethodInfo, pri, x.RawMetadata) 
        | _ -> failwith "not supported"
    | FSMeth(g,typ,vref,pri) -> FSMeth(g,instType inst typ,vref,pri)
    | DefaultStructCtor(g,typ) -> DefaultStructCtor(g,instType inst typ)
#if EXTENSIONTYPING
    | ProvidedMeth _ -> 
        match inst with 
        | [] -> meth
        | _ -> assert false; failwith "Not supported" 
#endif


/// Combine the type instantiation and generic method instantiation
let CombineMethInsts ttps mtps tinst minst = (mkTyparInst ttps tinst @ mkTyparInst mtps minst) 

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
let mkSlotParam (ty,argInfo:ArgReprInfo) = TSlotParam(Option.map textOfId argInfo.Name, ty, false,false,false,argInfo.Attribs) 

/// Construct the data representing the signature of an abstract method slot
let mkSlotSig (nm,typ,ctps,mtps,paraml,retTy) = copySlotSig (TSlotSig(nm,typ,ctps,mtps,paraml,retTy))


type MethInfo with 
    /// Get the return type of a method info, where 'void' is returned as 'None'
    member minfo.GetCompiledReturnTy (amap, m, minst) = 
        match minfo with 
        | ILMeth(_g,ilminfo,_) -> 
            ilminfo.GetCompiledReturnTy(amap, m, minst)
        | FSMeth(g,typ,vref,_) -> 
            let memberParentTypars,memberMethodTypars,retTy,parentTyArgs = AnalyzeTypeOfMemberVal g (typ,vref)
            retTy |> Option.map (instType (CombineMethInsts memberParentTypars memberMethodTypars parentTyArgs minst)) 
        | DefaultStructCtor _ -> None
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,amap,m) -> 
            GetCompiledReturnTyOfProvidedMethodInfo amap m mi
#endif

    /// Get the return type of a method info, where 'void' is returned as 'unit'
    member minfo.GetFSharpReturnTy(amap, m, minst) =
        minfo.GetCompiledReturnTy(amap, m, minst) |> GetFSharpViewOfReturnType amap.g
       
    /// Get the parameter types of a method info
    member minfo.GetParamTypes(amap, m, minst) = 
        match minfo with 
        | ILMeth(_g,ilminfo,_) -> 
            // A single group of tupled arguments
            [ ilminfo.GetParamTypes(amap,m,minst) ]
        | FSMeth(g,typ,vref,_) -> 
            let memberParentTypars,memberMethodTypars,_,parentTyArgs = AnalyzeTypeOfMemberVal g (typ,vref)
            let paramTypes = ParamNameAndTypesOfMember g vref
            let inst = CombineMethInsts memberParentTypars memberMethodTypars parentTyArgs minst
            paramTypes |> List.mapSquared (fun (ParamNameAndType(_,ty)) -> instType inst ty) 
        | DefaultStructCtor _ -> []
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,amap,m) -> 
            // A single group of tupled arguments
            [ [ for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters",m) do
                    yield Import.ImportProvidedType amap m (p.PApply((fun p -> p.ParameterType),m)) ] ]
#endif

    /// Get the (zero or one) 'self'/'this'/'object' arguments associated with a method. An instance extension method returns
    /// one object argument.
    member minfo.GetObjArgTypes (amap, m, minst) = 
        match minfo with 
        | ILMeth(_g,ilminfo,_) -> ilminfo.GetObjArgTypes(amap, m, minst)
        | FSMeth(_g,typ,vref,_) -> if vref.IsInstanceMember then [typ] else []
        | DefaultStructCtor _ -> []
#if EXTENSIONTYPING
        | ProvidedMeth(_,mi,amap,m) -> 
            if mi.PUntaint((fun mi -> mi.IsStatic || mi.IsConstructor),m) then 
                [] // no 'this' argument
            else 
                [ Import.ImportProvidedType amap m (mi.PApply((fun mi -> mi.DeclaringType),m)) ] // find the type of the 'this' argument
#endif

    /// Get the parameter attributes of a method info, which get combined with the parameter names and types
    member minfo.GetParamAttribs(amap, m) = 
        match minfo with 
        | ILMeth(g,ilMethInfo,_) -> 
            [ [ for p in ilMethInfo.ParamMetadata do
                 let isParamArrayArg = TryFindILAttribute g.attrib_ParamArrayAttribute p.CustomAttrs
                 let isOutArg = (p.IsOut && not p.IsIn)
                 // Note: we get default argument values from VB and other .NET language metadata 
                 let optArgInfo =  OptionalArgInfoOfILParameter g amap m ilMethInfo.MetadataScope ilMethInfo.ActualTypeInst p 
                 yield (isParamArrayArg, isOutArg, optArgInfo) ] ]

        | FSMeth(g,_,vref,_) -> 
            vref 
            |> ArgInfosOfMember g 
            |> List.mapSquared (fun (ty,argInfo) -> 
                let isParamArrayArg = HasFSharpAttribute g g.attrib_ParamArrayAttribute argInfo.Attribs
                let isOutArg = HasFSharpAttribute g g.attrib_OutAttribute argInfo.Attribs && isByrefTy g ty
                let isOptArg = HasFSharpAttribute g g.attrib_OptionalArgumentAttribute argInfo.Attribs
                // Note: can't specify caller-side default arguments in F#, by design (default is specified on the callee-side) 
                let optArgInfo = if isOptArg then CalleeSide else NotOptional
                (isParamArrayArg,isOutArg,optArgInfo))

        | DefaultStructCtor _ -> 
            [[]]

#if EXTENSIONTYPING
        | ProvidedMeth(g,mi,_,_) -> 
            // A single group of tupled arguments
            [ [for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters", m) do
                let isParamArrayArg = p.PUntaint((fun px -> (px :> IProvidedCustomAttributeProvider).GetAttributeConstructorArgs(p.TypeProvider.PUntaintNoFailure(id), typeof<System.ParamArrayAttribute>.FullName).IsSome),m)
                let optArgInfo =  OptionalArgInfoOfProvidedParameter g amap m p 
                yield (isParamArrayArg, p.PUntaint((fun p -> p.IsOut), m), optArgInfo)] ]
#endif



    /// Get the signature of an abstract method slot.
    //
    // This code has grown organically over time. We've managed to unify the ILMeth+ProvidedMeth paths.
    // The FSMeth, ILMeth+ProvidedMeth paths can probably be unified too.
    member minfo.GetSlotSig(amap, m) =
        match minfo with 
        | FSMeth(g,typ,vref,_) -> 
            match vref.RecursiveValInfo with 
            | ValInRecScope(false) -> error(Error((FSComp.SR.InvalidRecursiveReferenceToAbstractSlot()),m));
            | _ -> ()

            let allTyparsFromMethod,_,retTy,_ = GetTypeOfMemberInMemberForm g vref
            // A slot signature is w.r.t. the type variables of the type it is associated with.
            // So we have to rename from the member type variables to the type variables of the type.
            let formalEnclosingTypars = (tcrefOfAppTy g typ).Typars(m)
            let formalEnclosingTyparsFromMethod,formalMethTypars = List.chop formalEnclosingTypars.Length allTyparsFromMethod
            let methodToParentRenaming,_ = mkTyparToTyparRenaming formalEnclosingTyparsFromMethod formalEnclosingTypars
            let formalParams = 
                vref 
                |> ArgInfosOfMember g 
                |> List.mapSquared (map1Of2 (instType methodToParentRenaming) >> mkSlotParam )
            let formalRetTy = Option.map (instType methodToParentRenaming) retTy
            mkSlotSig(minfo.LogicalName, minfo.EnclosingType, formalEnclosingTypars, formalMethTypars, formalParams, formalRetTy)
        | DefaultStructCtor _ -> error(InternalError("no slotsig for DefaultStructCtor",m))
        | _ -> 
            let g = minfo.TcGlobals
            // slotsigs must contain the formal types for the arguments and return type 
            // a _formal_ 'void' return type is represented as a 'unit' type. 
            // slotsigs are independent of instantiation: if an instantiation 
            // happens to make the return type 'unit' (i.e. it was originally a variable type 
            // then that does not correspond to a slotsig compiled as a 'void' return type. 
            // REVIEW: should we copy down attributes to slot params? 
            let tcref =  tcrefOfAppTy g minfo.EnclosingType
            let formalEnclosingTyparsOrig = tcref.Typars(m)
            let formalEnclosingTypars = copyTypars formalEnclosingTyparsOrig
            let _,formalEnclosingTyparTys = FixupNewTypars m [] [] formalEnclosingTyparsOrig formalEnclosingTypars
            let formalMethTypars = copyTypars minfo.FormalMethodTypars
            let _,formalMethTyparTys = FixupNewTypars m formalEnclosingTypars formalEnclosingTyparTys minfo.FormalMethodTypars formalMethTypars
            let formalRetTy, formalParams = 
                match minfo with
                | ILMeth(_,ilminfo,_) -> 
                    let ftinfo = ILTypeInfo.FromType g (TType_app(tcref,formalEnclosingTyparTys))
                    let formalRetTy = ImportReturnTypeFromMetaData amap m ilminfo.RawMetadata.Return.Type ftinfo.ILScopeRef ftinfo.TypeInst formalMethTyparTys
                    let formalParams = 
                        [ [ for p in ilminfo.RawMetadata.Parameters do 
                                let paramType = ImportTypeFromMetadata amap m ftinfo.ILScopeRef ftinfo.TypeInst formalMethTyparTys p.Type
                                yield TSlotParam(p.Name, paramType, p.IsIn, p.IsOut, p.IsOptional, []) ] ]
                    formalRetTy, formalParams
#if EXTENSIONTYPING
                | ProvidedMeth (_,mi,_,_) -> 
                    // GENERIC TYPE PROVIDERS: for generics, formal types should be  generated here, not the actual types
                    // For non-generic type providers there is no difference
                    let formalRetTy = minfo.GetCompiledReturnTy(amap, m, formalMethTyparTys)
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
            mkSlotSig(minfo.LogicalName, minfo.EnclosingType, formalEnclosingTypars, formalMethTypars,formalParams, formalRetTy)
    
    /// Get the ParamData objects for the parameters of a MethInfo
    member minfo.GetParamDatas(amap, m, minst) = 
        let paramNamesAndTypes = 
            match minfo with 
            | ILMeth(_g,ilminfo,_) -> 
                [ ilminfo.GetParamNamesAndTypes(amap,m,minst)  ]
            | FSMeth(g,typ,vref,_) -> 
                let memberParentTypars,memberMethodTypars,_,parentTyArgs = AnalyzeTypeOfMemberVal g (typ,vref)
                let items = ParamNameAndTypesOfMember g vref
                let inst = CombineMethInsts memberParentTypars memberMethodTypars parentTyArgs minst
                items |> InstParamNameAndTypes inst 
            | DefaultStructCtor _ -> 
                [[]]
#if EXTENSIONTYPING
            | ProvidedMeth(_g,mi,amap,_) -> 
                // A single set of tupled parameters
                [ [for p in mi.PApplyArray((fun mi -> mi.GetParameters()), "GetParameters", m) do 
                        let pname = 
                            match p.PUntaint((fun p -> p.Name), m) with
                            | null -> None
                            | name -> Some name
                        let ptyp =
                            match p.PApply((fun p -> p.ParameterType), m) with
                            | Tainted.Null ->  amap.g.unit_ty
                            | parameterType -> Import.ImportProvidedType amap m parameterType
                        yield ParamNameAndType(pname,ptyp) ] ]

#endif

        let paramAttribs = minfo.GetParamAttribs(amap, m)
        (paramAttribs,paramNamesAndTypes) ||> List.map2 (List.map2 (fun (isParamArrayArg,isOutArg,optArgInfo) (ParamNameAndType(nmOpt,pty)) -> 
             ParamData(isParamArrayArg,isOutArg,optArgInfo,nmOpt,pty)))


    /// Select all the type parameters of the declaring type of a method. 
    ///
    /// For extension methods, no type parameters are returned,
    /// because all the type parameters are considered to be associated with the method, rather than the declaring type, even for extension
    /// methods extending generic types.
    member minfo.GetFormalTyparsOfEnclosingType m = 
        match minfo with
        | ILMeth(_,ilminfo,_) ->
            match ilminfo with
            | ILMethInfo(tinfo,_,_,_) when not ilminfo.IsCSharpExtensionMethod -> tinfo.FormalTypars m                    
            | _ -> [] // For extension methods all type variables are on the method
        | FSMeth(g,typ,vref,_) -> 
            let memberParentTypars,_,_,_ = AnalyzeTypeOfMemberVal g (typ,vref)
            memberParentTypars
        | DefaultStructCtor(g,typ) -> 
            (tcrefOfAppTy g typ).Typars(m)
#if EXTENSIONTYPING
        | ProvidedMeth (g,_,_,_) -> 
            (tcrefOfAppTy g minfo.EnclosingType).Typars(m)
#endif

//-------------------------------------------------------------------------
// Method signatures
//------------------------------------------------------------------------- 


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
    // of the enclosing type. This instaniations can be used to interpret those type parameters 
    let fmtpinst = 
        let parentTyArgs = argsOfAppTy g minfo.EnclosingType
        let memberParentTypars  = minfo.GetFormalTyparsOfEnclosingType m 
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


//-------------------------------------------------------------------------
// Basic accessibility logic
//------------------------------------------------------------------------- 

/// Represents the 'keys' a particular piece of code can use to access other constructs?.
[<NoEquality; NoComparison>]
type AccessorDomain = 
    /// AccessibleFrom(cpaths, tyconRefOpt)
    ///
    /// cpaths: indicates we have the keys to access any members private to the given paths 
    // tyconRefOpt:  indicates we have the keys to access any protected members of the super types of 'TyconRef' 
    | AccessibleFrom of CompilationPath list * TyconRef option        
    | AccessibleFromEverywhere

    /// An AccessorDomain which returns everything but .NET private/internal items
    /// This is used 
    ///    - when solving member trait constraints, which are solved independently of accessibility 
    ///    - for failure paths in error reporting, e.g. to produce an error that an F# item is not accessible
    ///    - an adhoc use in service.fs to look up a delegate signature
    | AccessibleFromSomeFSharpCode 

    /// Can access everything
    | AccessibleFromSomewhere 

    // Hashing and comparison is used for the memoization tables keyed by an accessor domain.
    // It is dependent on a TcGlobals because of the TyconRef in the data structure
    static member CustomGetHashCode(ad:AccessorDomain) = 
        match ad with 
        | AccessibleFrom _ -> 1
        | AccessibleFromEverywhere -> 2
        | AccessibleFromSomeFSharpCode  -> 3
        | AccessibleFromSomewhere  -> 4
    static member CustomEquals(g:TcGlobals, ad1:AccessorDomain, ad2:AccessorDomain) = 
        match ad1, ad2 with 
        | AccessibleFrom(cs1,tc1), AccessibleFrom(cs2,tc2) -> (cs1 = cs2) && (match tc1,tc2 with None,None -> true | Some tc1, Some tc2 -> tyconRefEq g tc1 tc2 | _ -> false)
        | AccessibleFromEverywhere, AccessibleFromEverywhere -> true
        | AccessibleFromSomeFSharpCode, AccessibleFromSomeFSharpCode  -> true
        | AccessibleFromSomewhere, AccessibleFromSomewhere  -> true
        | _ -> false

module AccessibilityLogic = 

    let IsAccessible ad taccess = 
        match ad with 
        | AccessibleFromEverywhere -> canAccessFromEverywhere taccess
        | AccessibleFromSomeFSharpCode -> canAccessFromSomewhere taccess
        | AccessibleFromSomewhere -> true
        | AccessibleFrom (cpaths,_tcrefViewedFromOption) -> 
            List.exists (canAccessFrom taccess) cpaths

    let private CheckILMemberAccess g amap m (tcrefOfViewedItem : TyconRef) ad access = 
        match ad with 
        | AccessibleFromEverywhere -> 
              access = ILMemberAccess.Public
        | AccessibleFromSomeFSharpCode -> 
             (access = ILMemberAccess.Public || 
              access = ILMemberAccess.Family  || 
              access = ILMemberAccess.FamilyOrAssembly) 
        | AccessibleFrom (cpaths,tcrefViewedFromOption) ->
             let accessibleByFamily =
                  ((access = ILMemberAccess.Family  || 
                    access = ILMemberAccess.FamilyOrAssembly) &&
                   match tcrefViewedFromOption with 
                   | None -> false
                   | Some tcrefViewedFrom ->
                      ExistsHeadTypeInEntireHierarchy  g amap m (generalizedTyconRef tcrefViewedFrom) tcrefOfViewedItem)     
             let accessibleByInternalsVisibleTo = 
                  (access = ILMemberAccess.Assembly && canAccessFromOneOf cpaths tcrefOfViewedItem.CompilationPath)
             (access = ILMemberAccess.Public) || accessibleByFamily || accessibleByInternalsVisibleTo
        | AccessibleFromSomewhere -> 
             true

    let private isILTypeDefAccessible ad (tdef: ILTypeDef) =        
        match ad with 
        | AccessibleFromSomewhere -> true
        | AccessibleFromEverywhere 
        | AccessibleFromSomeFSharpCode 
        | AccessibleFrom _ -> tdef.Access = ILTypeDefAccess.Public || tdef.Access = ILTypeDefAccess.Nested ILMemberAccess.Public

    // is tcref visible through the AccessibleFrom(cpaths,_)? note: InternalsVisibleTo extends those cpaths.
    let private isTyconAccessibleViaVisibleTo ad (tcrefOfViewedItem:TyconRef) =
        match ad with 
        | AccessibleFromEverywhere 
        | AccessibleFromSomewhere 
        | AccessibleFromSomeFSharpCode -> false
        | AccessibleFrom (cpaths,_tcrefViewedFromOption) ->
            canAccessFromOneOf cpaths tcrefOfViewedItem.CompilationPath

    let private isILTypeInfoAccessible ad (ILTypeInfo(tcrefOfViewedItem,_,_tinst,tdef)) =       
        isILTypeDefAccessible ad tdef || isTyconAccessibleViaVisibleTo ad tcrefOfViewedItem
                       
    let private isILMemberAccessible g amap m ad tinfo access = 
        isILTypeInfoAccessible ad tinfo && CheckILMemberAccess g amap m tinfo.TyconRef ad access

    let IsEntityAccessible ad (tcref:TyconRef) = 
        if tcref.IsILTycon then 
            (isTyconAccessibleViaVisibleTo ad tcref) ||  // either: visibleTo (e.g. InternalsVisibleTo)              
              (let _scoref,enc,tdef = tcref.ILTyconInfo   // or:     accessible, along with all enclosing types
               List.forall (isILTypeDefAccessible ad) enc && 
               isILTypeDefAccessible ad tdef)
        else  
             tcref.Accessibility |> IsAccessible ad

    let CheckTyconAccessible m ad tcref =
        let res = IsEntityAccessible ad tcref
        if not res then  
            errorR(Error(FSComp.SR.typeIsNotAccessible tcref.DisplayName,m))
        res

    let IsTyconReprAccessible ad tcref =
        IsEntityAccessible ad tcref &&
        IsAccessible ad tcref.TypeReprAccessibility
            
    let CheckTyconReprAccessible m ad tcref =
        CheckTyconAccessible m ad tcref &&
        (let res = IsAccessible ad tcref.TypeReprAccessibility
         if not res then 
            errorR (Error (FSComp.SR.unionCasesAreNotAccessible tcref.DisplayName,m));
         res)
            
    let rec IsTypeAccessible g ad ty = 
        not (isAppTy g ty) ||
        let tcref,tinst = destAppTy g ty
        IsEntityAccessible ad tcref && IsTypeInstAccessible g ad tinst

    and IsProvidedMemberAccessible g amap m ad ty access = 
        let isTyAccessible = IsTypeAccessible g ad ty
        if not isTyAccessible then false
        else
            not (isAppTy g ty) ||
            let tcrefOfViewedItem,_ = destAppTy g ty
            CheckILMemberAccess g amap m tcrefOfViewedItem ad access

    and IsTypeInstAccessible g ad tinst = 
        match tinst with 
        | [] -> true 
        | _ -> List.forall (IsTypeAccessible g ad) tinst

    let getILAccess isPublic isFamily isFamilyOrAssembly isFamilyAndAssembly =
        if isPublic then ILMemberAccess.Public
        elif isFamily then ILMemberAccess.Family
        elif isFamilyOrAssembly then ILMemberAccess.FamilyOrAssembly
        elif isFamilyAndAssembly then ILMemberAccess.FamilyAndAssembly
        else ILMemberAccess.Private

    let IsILFieldInfoAccessible g amap m ad x = 
        match x with 
        | ILFieldInfo (tinfo,fd) -> isILMemberAccessible g amap m ad tinfo fd.Access
#if EXTENSIONTYPING
        | ProvidedField (g, tpfi, amap, m) as pfi -> 
            let access = tpfi.PUntaint((fun fi -> getILAccess fi.IsPublic fi.IsFamily fi.IsFamilyOrAssembly fi.IsFamilyAndAssembly), m)
            IsProvidedMemberAccessible g amap m ad pfi.EnclosingType access
#endif

    let IsILEventInfoAccessible g amap m ad (ILEventInfo (tinfo,edef)) =
        let access = (resolveILMethodRef tinfo.RawMetadata edef.AddMethod).Access 
        isILMemberAccessible g amap m ad tinfo access

    let IsILMethInfoAccessible g amap m ad = function
        | ILMethInfo (tinfo,_,mdef,_) -> isILMemberAccessible g amap m ad tinfo mdef.Access 
        | ILFSMethInfo (tcref,_,_,mdef) -> CheckILMemberAccess g amap m tcref ad mdef.Access


    let IsILPropInfoAccessible g amap m ad (ILPropInfo(tinfo,pdef)) =
        let tdef = tinfo.RawMetadata
        let ilAccess = 
            match pdef.GetMethod with 
            | Some mref -> (resolveILMethodRef tdef mref).Access 
            | None -> 
                match pdef.SetMethod with 
                | None -> ILMemberAccess.Public
                | Some mref -> (resolveILMethodRef tdef mref).Access

        isILMemberAccessible g amap m ad tinfo ilAccess

    let IsValAccessible ad (vref:ValRef) = 
        vref.Accessibility |> IsAccessible ad

    let CheckValAccessible  m ad (vref:ValRef) = 
        if not (IsValAccessible ad vref) then 
            errorR (Error (FSComp.SR.valueIsNotAccessible vref.DisplayName,m))
        
    let IsUnionCaseAccessible ad (ucref:UnionCaseRef) =
        IsTyconReprAccessible ad ucref.TyconRef &&
        IsAccessible ad ucref.UnionCase.Accessibility

    let CheckUnionCaseAccessible m ad (ucref:UnionCaseRef) =
        CheckTyconReprAccessible m ad ucref.TyconRef &&
        (let res = IsAccessible ad ucref.UnionCase.Accessibility
         if not res then 
            errorR (Error (FSComp.SR.unionCaseIsNotAccessible ucref.CaseName,m))
         res)

    let IsRecdFieldAccessible ad (rfref:RecdFieldRef) =
        IsTyconReprAccessible ad rfref.TyconRef &&
        IsAccessible ad rfref.RecdField.Accessibility

    let CheckRecdFieldAccessible m ad (rfref:RecdFieldRef) =
        CheckTyconReprAccessible m ad rfref.TyconRef &&
        (let res = IsAccessible ad rfref.RecdField.Accessibility
         if not res then 
            errorR (Error (FSComp.SR.fieldIsNotAccessible rfref.FieldName,m))
         res)

    let CheckRecdFieldInfoAccessible m ad (rfinfo:RecdFieldInfo) = 
        CheckRecdFieldAccessible m ad rfinfo.RecdFieldRef |> ignore

    let CheckILFieldInfoAccessible g amap m ad finfo =
        if not (IsILFieldInfoAccessible g amap m ad finfo) then 
            errorR (Error (FSComp.SR.structOrClassFieldIsNotAccessible finfo.FieldName,m))

    let IsMethInfoAccessible amap m ad = function
        | ILMeth (g,x,_) -> IsILMethInfoAccessible g amap m ad x 
        | FSMeth (_,_,vref,_) -> IsValAccessible ad vref
        | DefaultStructCtor(g,typ) -> IsTypeAccessible g ad typ
#if EXTENSIONTYPING
        | ProvidedMeth(g,tpmb,_amap,m) as etmi -> 
            let access = tpmb.PUntaint((fun mi -> getILAccess mi.IsPublic mi.IsFamily mi.IsFamilyOrAssembly mi.IsFamilyAndAssembly), m)        
            IsProvidedMemberAccessible g amap m ad etmi.EnclosingType access
#endif


    let IsPropInfoAccessible g amap m ad = function 
        | ILProp (_,x) -> IsILPropInfoAccessible g amap m ad x
        | FSProp (_,_,Some vref,_) 
        | FSProp (_,_,_,Some vref) -> IsValAccessible ad vref
#if EXTENSIONTYPING
        | ProvidedProp (g, tppi, amap, m) as pp-> 
            let access = 
                let a = tppi.PUntaint((fun ppi -> 
                    let tryGetILAccessForProvidedMethodBase (mi : ProvidedMethodBase) = 
                        match mi with
                        | null -> None
                        | mi -> Some(getILAccess mi.IsPublic mi.IsFamily mi.IsFamilyOrAssembly mi.IsFamilyAndAssembly)
                    match tryGetILAccessForProvidedMethodBase(ppi.GetGetMethod()) with
                    | None -> tryGetILAccessForProvidedMethodBase(ppi.GetSetMethod())
                    | x -> x), m)
                defaultArg a ILMemberAccess.Public
            IsProvidedMemberAccessible g amap m ad pp.EnclosingType access
#endif
        | _ -> false

    let IsFieldInfoAccessible ad (rfref:RecdFieldInfo) =
        IsAccessible ad rfref.RecdField.Accessibility

open AccessibilityLogic



//-------------------------------------------------------------------------
// Check custom attributes
//------------------------------------------------------------------------- 

exception ObsoleteWarning of string * range
exception ObsoleteError of string * range

/// Check custom attributes. This is particularly messy because custom attributes come in in three different
/// formats.
module AttributeChecking = 

    // This is used for AttributeUsageAttribute, DefaultMemberAttribute and ConditionalAttribute (on attribute types)
    let TryBindTyconRefAttribute g m (AttribInfo (atref,_) as args) (tcref:TyconRef) f1 f2 f3 = 
        ignore m; ignore f3
        match metadataOfTycon tcref.Deref with 
#if EXTENSIONTYPING
        | ExtensionTypeMetadata info -> 
            let provAttribs = info.ProvidedType.PApply((fun a -> (a :> IProvidedCustomAttributeProvider)),m)
            match provAttribs.PUntaint((fun a -> a.GetAttributeConstructorArgs(provAttribs.TypeProvider.PUntaintNoFailure(id), atref.FullName)),m) with
            | Some args -> f3 args
            | None -> None
#endif
        | ILTypeMetadata (_,tdef) -> 
            match TryDecodeILAttribute g atref (Some(atref.Scope)) tdef.CustomAttrs with 
            | Some attr -> f1 attr
            | _ -> None
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 
            match TryFindFSharpAttribute g args tcref.Attribs with 
            | Some attr -> f2 attr
            | _ -> None

    let BindMethInfoAttributes m minfo f1 f2 f3 = 
        ignore m; ignore f3
        match minfo with 
        | ILMeth (_,x,_) -> f1 x.RawMetadata.CustomAttrs 
        | FSMeth (_,_,vref,_) -> f2 vref.Attribs
        | DefaultStructCtor _ -> f2 []
#if EXTENSIONTYPING
        | ProvidedMeth (_,mi,_,_) -> f3 (mi.PApply((fun st -> (st :> IProvidedCustomAttributeProvider)),m))
#endif

    let TryBindMethInfoAttribute g m (AttribInfo(atref,_) as attribSpec) minfo f1 f2 f3 = 
        ignore f3
        BindMethInfoAttributes m minfo 
            (fun ilAttribs -> TryDecodeILAttribute g atref (Some(atref.Scope)) ilAttribs |> Option.bind f1)
            (fun fsAttribs -> TryFindFSharpAttribute g attribSpec fsAttribs |> Option.bind f2)
#if EXTENSIONTYPING
            (fun provAttribs -> 
                match provAttribs.PUntaint((fun a -> a.GetAttributeConstructorArgs(provAttribs.TypeProvider.PUntaintNoFailure(id), atref.FullName)),m) with
                | Some args -> f3 args
                | None -> None)  
#else
            (fun _provAttribs -> None)
#endif

    /// This is just used for the 'ConditionalAttribute' attribute
    let TryFindMethInfoStringAttribute g m attribSpec minfo  =
        TryBindMethInfoAttribute g m attribSpec minfo 
                     (function ([ILAttribElem.String (Some msg) ],_) -> Some msg | _ -> None) 
                     (function (Attrib(_,_,[ AttribStringArg msg ],_,_,_,_)) -> Some msg | _ -> None)
                     (function [ Some ((:? string as msg) : obj) ] -> Some msg | _ -> None)

    /// This is used to detect the 'DefaultMemberAttribute' and 'ConditionalAttribute' attributes (on type definitions)
    let TryFindTyconRefStringAttribute g m attribSpec tcref  =
        TryBindTyconRefAttribute g m attribSpec tcref 
                 (function ([ILAttribElem.String (Some(msg)) ],_) -> Some msg | _ -> None)
                 (function (Attrib(_,_,[ AttribStringArg(msg) ],_,_,_,_))  -> Some msg | _ -> None)
                 (function [ Some ((:? string as msg) : obj) ] -> Some msg | _ -> None)


    /// Check IL attributes for 'ObsoleteAttribute'
    let private CheckILAttributes g cattrs m = 
        let (AttribInfo(tref,_)) = g.attrib_SystemObsolete
        match TryDecodeILAttribute g tref (Some(tref.Scope)) cattrs with 
        | Some ([ILAttribElem.String (Some msg) ],_) -> 
             WarnD(ObsoleteWarning(msg,m))
        | Some ([ILAttribElem.String (Some msg); ILAttribElem.Bool isError ],_) -> 
            if isError then 
                ErrorD (ObsoleteError(msg,m))
            else 
                WarnD (ObsoleteWarning(msg,m))
        | Some ([ILAttribElem.String None ],_) -> 
            WarnD(ObsoleteWarning("",m))
        | Some _ -> 
            WarnD(ObsoleteWarning("",m))
        | None -> 
            CompleteD

    /// Check F# attributes for 'ObsoleteAttribute', 'CompilerMessageAttribute' and 'ExperimentalAttribute'
    let CheckFSharpAttributes g attribs m = 
        if isNil attribs then CompleteD 
        else 
            (match TryFindFSharpAttribute g g.attrib_SystemObsolete attribs with
            | Some(Attrib(_,_,[ AttribStringArg s ],_,_,_,_)) ->
                WarnD(ObsoleteWarning(s,m))
            | Some(Attrib(_,_,[ AttribStringArg s; AttribBoolArg(isError) ],_,_,_,_)) -> 
                if isError then 
                    ErrorD (ObsoleteError(s,m))
                else 
                    WarnD (ObsoleteWarning(s,m))
            | Some _ -> 
                WarnD(ObsoleteWarning("", m))
            | None -> 
                CompleteD
            ) ++ (fun () -> 
            
            match TryFindFSharpAttribute g g.attrib_CompilerMessageAttribute attribs with
            | Some(Attrib(_,_,[ AttribStringArg s ; AttribInt32Arg n ],namedArgs,_,_,_)) -> 
                let msg = UserCompilerMessage(s,n,m)
                let isError = 
                    match namedArgs with 
                    | ExtractAttribNamedArg "IsError" (AttribBoolArg v) -> v 
                    | _ -> false 
                if isError then ErrorD msg else WarnD msg
                 
            | _ -> 
                CompleteD
            ) ++ (fun () -> 
            
            match TryFindFSharpAttribute g g.attrib_ExperimentalAttribute attribs with
            | Some(Attrib(_,_,[ AttribStringArg(s) ],_,_,_,_)) -> 
                WarnD(Experimental(s,m))
            | Some _ -> 
                WarnD(Experimental(FSComp.SR.experimentalConstruct (), m))
            | _ ->  
                CompleteD
            ) ++ (fun () -> 

            match TryFindFSharpAttribute g g.attrib_UnverifiableAttribute attribs with
            | Some _ -> 
                WarnD(PossibleUnverifiableCode(m))
            | _ ->  
                CompleteD
            )

#if EXTENSIONTYPING
    let private CheckProvidedAttributes g m (provAttribs: Tainted<IProvidedCustomAttributeProvider>)  = 
        let (AttribInfo(tref,_)) = g.attrib_SystemObsolete
        match provAttribs.PUntaint((fun a -> a.GetAttributeConstructorArgs(provAttribs.TypeProvider.PUntaintNoFailure(id), tref.FullName)),m) with
        | Some [ Some (:? string as msg) ] -> WarnD(ObsoleteWarning(msg,m))
        | Some [ Some (:? string as msg); Some (:?bool as isError) ]  ->
            if isError then 
                ErrorD (ObsoleteError(msg,m))
            else 
                WarnD (ObsoleteWarning(msg,m))
        | Some [ None ] -> 
            WarnD(ObsoleteWarning("",m))
        | Some _ -> 
            WarnD(ObsoleteWarning("",m))
        | None -> 
            CompleteD
#endif

    /// Check IL attributes for existence of 'ObsoleteAttribute', to suppress the item in intellisense
    let CheckILAttributesForUnseen g cattrs _m = 
        let (AttribInfo(tref,_)) = g.attrib_SystemObsolete
        isSome (TryDecodeILAttribute g tref (Some(tref.Scope)) cattrs)

    /// Check F# attributes for existence of 'ObsoleteAttribute', to suppress the item in intellisense
    /// Also check F# attributes for CompilerMessageAttribute, which has an IsHidden argument that allows
    /// items to be suppressed from intellisense.
    let CheckFSharpAttributesForUnseen g attribs _m = 
        nonNil attribs && 
        (let isObsolete = isSome (TryFindFSharpAttribute g g.attrib_SystemObsolete attribs) 
         let isHidden = 
             (match TryFindFSharpAttribute g g.attrib_CompilerMessageAttribute attribs with
              | Some(Attrib(_,_,[AttribStringArg _; AttribInt32Arg messageNumber],
                            ExtractAttribNamedArg "IsHidden" (AttribBoolArg v),_,_,_)) -> 
                  // Message number 62 is for "ML Compatibility". Items labelled with this are visible in intellisense
                  // when mlCompatibility is set.
                  v && not (messageNumber = 62 && g.mlCompatibility)
              | _ -> false)
         isObsolete || isHidden
        )
      
#if EXTENSIONTYPING
    /// Check provided attributes for existence of 'ObsoleteAttribute', to suppress the item in intellisense
    let CheckProvidedAttributesForUnseen (provAttribs: Tainted<IProvidedCustomAttributeProvider>) m = 
        provAttribs.PUntaint((fun a -> a.GetAttributeConstructorArgs(provAttribs.TypeProvider.PUntaintNoFailure(id), typeof<System.ObsoleteAttribute>.FullName).IsSome),m)
#endif

    let CheckPropInfoAttributes pinfo m = 
        match pinfo with
        | ILProp(g,ILPropInfo(_,pdef)) -> CheckILAttributes g pdef.CustomAttrs m
        | FSProp(g,_,Some vref,_) 
        | FSProp(g,_,_,Some vref) -> CheckFSharpAttributes g vref.Attribs m
        | FSProp _ -> failwith "CheckPropInfoAttributes: unreachable"
#if EXTENSIONTYPING
        | ProvidedProp (g,pi,_amap,m) ->  
            CheckProvidedAttributes g m (pi.PApply((fun st -> (st :> IProvidedCustomAttributeProvider)),m))
         
#endif

      
    let CheckILFieldAttributes g (finfo:ILFieldInfo) m = 
        match finfo with 
        | ILFieldInfo(_,pd) -> 
            CheckILAttributes g pd.CustomAttrs m |> CommitOperationResult
#if EXTENSIONTYPING
        | ProvidedField (g,fi,_amap,m) -> 
            CheckProvidedAttributes g m (fi.PApply((fun st -> (st :> IProvidedCustomAttributeProvider)),m)) |> CommitOperationResult
#endif

    let CheckMethInfoAttributes g m tyargsOpt minfo = 
        let search = BindMethInfoAttributes m minfo 
                          (fun ilAttribs -> Some(CheckILAttributes g ilAttribs m)) 
                          (fun fsAttribs -> 
                              let res = 
                                  CheckFSharpAttributes g fsAttribs m ++ (fun () -> 
                                      if isNone tyargsOpt && HasFSharpAttribute g g.attrib_RequiresExplicitTypeArgumentsAttribute fsAttribs then
                                         ErrorD(Error(FSComp.SR.tcFunctionRequiresExplicitTypeArguments(minfo.LogicalName),m));
                                      else
                                         CompleteD)
                              Some res) 
#if EXTENSIONTYPING
                          (fun provAttribs -> Some (CheckProvidedAttributes g m provAttribs)) 
#else
                          (fun _provAttribs -> None)
#endif 
        match search with
        | Some res -> res
        | None -> CompleteD // no attribute = no errors 

    let MethInfoIsUnseen g m typ minfo = 
        let isUnseenByObsoleteAttrib = 
            match BindMethInfoAttributes m minfo 
                    (fun ilAttribs -> Some(CheckILAttributesForUnseen g ilAttribs m)) 
                    (fun fsAttribs -> Some(CheckFSharpAttributesForUnseen g fsAttribs m))
#if EXTENSIONTYPING
                    (fun provAttribs -> Some(CheckProvidedAttributesForUnseen provAttribs m))
#else
                    (fun _provAttribs -> None)
#endif
                     with
            | Some res -> res
            | None -> false

        let isUnseenByHidingAttribute = 
#if EXTENSIONTYPING
            not (isObjTy g typ) &&
            isAppTy g typ &&
            isObjTy g minfo.EnclosingType &&
            let tcref = tcrefOfAppTy g typ 
            match tcref.TypeReprInfo with 
            | TProvidedTypeExtensionPoint info -> 
                info.ProvidedType.PUntaint((fun st -> (st :> IProvidedCustomAttributeProvider).GetHasTypeProviderEditorHideMethodsAttribute(info.ProvidedType.TypeProvider.PUntaintNoFailure(id))), m)
            | _ -> 
            // This attribute check is done by name to ensure compilation doesn't take a dependency 
            // on Microsoft.FSharp.Core.CompilerServices.TypeProviderEditorHideMethodsAttribute.
            //
            // We are only interested in filtering out the method on System.Object, so it is sufficient
            // just to look at the attributes on IL methods.
            if tcref.IsILTycon then 
                  tcref.ILTyconRawMetadata.CustomAttrs.AsList 
                  |> List.exists (fun attr -> attr.Method.EnclosingType.TypeSpec.Name = typeof<TypeProviderEditorHideMethodsAttribute>.FullName)
            else 
                false
#else
            typ |> ignore
            false
#endif
        isUnseenByObsoleteAttrib || isUnseenByHidingAttribute

    let PropInfoIsUnseen m pinfo = 
        match pinfo with
        | ILProp (g,ILPropInfo(_,pdef)) -> CheckILAttributesForUnseen g pdef.CustomAttrs m
        | FSProp (g,_,Some vref,_) 
        | FSProp (g,_,_,Some vref) -> CheckFSharpAttributesForUnseen g vref.Attribs m
        | FSProp _ -> failwith "CheckPropInfoAttributes: unreachable"
#if EXTENSIONTYPING
        | ProvidedProp (_g,pi,_amap,m) -> 
            CheckProvidedAttributesForUnseen (pi.PApply((fun st -> (st :> IProvidedCustomAttributeProvider)),m)) m
#endif
     
    let CheckEntityAttributes g (x:TyconRef) m = 
        if x.IsILTycon then 
            CheckILAttributes g x.ILTyconRawMetadata.CustomAttrs m
        else 
            CheckFSharpAttributes g x.Attribs m

    let CheckUnionCaseAttributes g (x:UnionCaseRef) m =
        CheckEntityAttributes g x.TyconRef m ++ (fun () ->
        CheckFSharpAttributes g x.Attribs m)

    let CheckRecdFieldAttributes g (x:RecdFieldRef) m =
        CheckEntityAttributes g x.TyconRef m ++ (fun () ->
        CheckFSharpAttributes g x.PropertyAttribs m)

    let CheckValAttributes g (x:ValRef) m =
        CheckFSharpAttributes g x.Attribs m

    let CheckRecdFieldInfoAttributes g (x:RecdFieldInfo) m =
        CheckRecdFieldAttributes g x.RecdFieldRef m


open AttributeChecking
    
//-------------------------------------------------------------------------
// Build calls 
//------------------------------------------------------------------------- 


/// Build an expression node that is a call to a .NET method. 
let BuildILMethInfoCall g amap m isProp (minfo:ILMethInfo) valUseFlags minst direct args = 
    let valu = 
        match minfo with 
        | ILMethInfo(tinfo,_,_,_) -> tinfo.IsValueType
        | ILFSMethInfo(_,kind,_,_) when kind.IsValueType -> true
        | ILFSMethInfo _  -> false                                    
    let ctor = minfo.IsConstructor
    if minfo.IsClassConstructor then 
        error (InternalError (minfo.ILName+": cannot call a class constructor",m));
    let useCallvirt = 
        not valu && not direct && minfo.IsVirtual
    let isProtected = minfo.IsProtectedAccessibility
    let ilMethRef = minfo.ILMethodRef
    let newobj = ctor && (match valUseFlags with NormalValUse -> true | _ -> false)
    let exprty = if ctor then minfo.EnclosingType else minfo.GetFSharpReturnTy(amap, m, minst)
    // The thing might be an extension method, in which case adjust the instantiations
    let actualTypeInst = minfo.ActualTypeInst
    let actualMethInst = minst
    let retTy = (if not ctor && (ilMethRef.ReturnType = ILType.Void) then [] else [exprty])
    let isDllImport = minfo.IsDllImport g
    Expr.Op(TOp.ILCall(useCallvirt,isProtected,valu,newobj,valUseFlags,isProp,isDllImport,ilMethRef,actualTypeInst,actualMethInst, retTy),[],args,m),
    exprty

/// Build a call to the System.Object constructor taking no arguments,
let BuildObjCtorCall g m =
    let ilMethRef = (mkILCtorMethSpecForTy(g.ilg.typ_Object,[])).MethodRef
    Expr.Op(TOp.ILCall(false,false,false,false,CtorValUsedAsSuperInit,false,true,ilMethRef,[],[],[g.obj_ty]),[],[],m)


/// Build a call to an F# method.
///
/// Consume the arguments in chunks and build applications.  This copes with various F# calling signatures
/// all of which ultimately become 'methods'.
///
/// QUERY: this looks overly complex considering that we are doing a fundamentally simple 
/// thing here. 
let BuildFSharpMethodApp g m (vref: ValRef) vexp vexprty (args: Exprs) =
    let arities =  (arityOfVal vref.Deref).AritiesOfArgs
    
    let args3,(leftover,retTy) = 
        ((args,vexprty), arities) ||> List.mapFold (fun (args,fty) arity -> 
            match arity,args with 
            | (0|1),[] when typeEquiv g (domainOfFunTy g fty) g.unit_ty -> mkUnit g m, (args, rangeOfFunTy g fty)
            | 0,(arg::argst)-> 
                warning(InternalError(sprintf "Unexpected zero arity, args = %s" (Layout.showL (Layout.sepListL (Layout.rightL ";") (List.map exprL args))),m));
                arg, (argst, rangeOfFunTy g fty)
            | 1,(arg :: argst) -> arg, (argst, rangeOfFunTy g fty)
            | 1,[] -> error(InternalError("expected additional arguments here",m))
            | _ -> 
                if args.Length < arity then error(InternalError("internal error in getting arguments, n = "+string arity+", #args = "+string args.Length,m));
                let tupargs,argst = List.chop arity args
                let tuptys = tupargs |> List.map (tyOfExpr g) 
                (mkTupled g m tupargs tuptys),
                (argst, rangeOfFunTy g fty) )
    if not leftover.IsEmpty then error(InternalError("Unexpected "+string(leftover.Length)+" remaining arguments in method application",m));
    mkApps g ((vexp,vexprty),[],args3,m), 
    retTy
    
/// Build a call to an F# method.
let BuildFSharpMethodCall g m (typ,vref:ValRef) valUseFlags minst args =
    let vexp = Expr.Val (vref,valUseFlags,m)
    let vexpty = vref.Type
    let tpsorig,tau =  vref.TypeScheme
    let vtinst = argsOfAppTy g typ @ minst
    if tpsorig.Length <> vtinst.Length then error(InternalError("BuildFSharpMethodCall: unexpected List.length mismatch",m));
    let expr = mkTyAppExpr m (vexp,vexpty) vtinst
    let exprty = instType (mkTyparInst tpsorig vtinst) tau
    BuildFSharpMethodApp g m vref expr exprty args
    

/// Make a call to a method info. Used by the optimizer and code generator to build 
/// calls to the type-directed solutions to member constraints.
let MakeMethInfoCall amap m minfo minst args =
    let valUseFlags = NormalValUse // correct unless if we allow wild trait constraints like "T has a ctor and can be used as a parent class" 
    match minfo with 
    | ILMeth(g,ilminfo,_) -> 
        let direct = not minfo.IsVirtual
        let isProp = false // not necessarily correct, but this is only used post-creflect where this flag is irrelevant 
        BuildILMethInfoCall g amap m isProp ilminfo valUseFlags minst  direct args |> fst
    | FSMeth(g,typ,vref,_) -> 
        BuildFSharpMethodCall g m (typ,vref) valUseFlags minst args |> fst
    | DefaultStructCtor(_,typ) -> 
       mkDefault (m,typ)
#if EXTENSIONTYPING
    | ProvidedMeth(_,mi,amap,m) -> 
        let isProp = false // not necessarily correct, but this is only used post-creflect where this flag is irrelevant 
        let ilMethodRef = Import.ImportProvidedMethodBaseAsILMethodRef amap m mi
        let isConstructor = mi.PUntaint((fun c -> c.IsConstructor), m)
        let valu = mi.PUntaint((fun c -> c.DeclaringType.IsValueType), m)
        let actualTypeInst = [] // GENERIC TYPE PROVIDERS: for generics, we would have something here
        let actualMethInst = [] // GENERIC TYPE PROVIDERS: for generics, we would have something here
        let ilReturnTys = Option.toList (minfo.GetCompiledReturnTy(amap, m, []))  // GENERIC TYPE PROVIDERS: for generics, we would have more here
        // REVIEW: Should we allow protected calls?
        Expr.Op(TOp.ILCall(false,false, valu, isConstructor,valUseFlags,isProp,false,ilMethodRef,actualTypeInst,actualMethInst, ilReturnTys),[],args,m)

#endif
//---------------------------------------------------------------------------
// Helpers when selecting members 
//---------------------------------------------------------------------------


/// Use the given function to select some of the member values from the members of an F# type
let SelectImmediateMemberVals g optFilter f (tcref:TyconRef) = 
    let _aug = tcref.TypeContents

    let chooser (vref:ValRef) = 
        match vref.MemberInfo with 
        // The 'when' condition is a workaround for the fact that values providing 
        // override and interface implementations are published in inferred module types 
        // These cannot be selected directly via the "." notation. 
        // However, it certainly is useful to be able to publish these values, as we can in theory 
        // optimize code to make direct calls to these methods. 
        | Some membInfo when not (ValRefIsExplicitImpl g vref) -> 
            f membInfo vref
        | _ ->  
            None

    match optFilter with 
    | None -> tcref.MembersOfFSharpTyconByName |> NameMultiMap.chooseRange chooser
    | Some nm -> tcref.MembersOfFSharpTyconByName |> NameMultiMap.find nm |> List.choose chooser

/// Check whether a name matches an optional filter
let checkFilter optFilter (nm:string) = match optFilter with None -> true | Some n2 -> nm = n2

/// Try to select an F# value when querying members, and if so return a MethInfo that wraps the F# value.
let TrySelectMemberVal g optFilter typ pri _membInfo (vref:ValRef) =
    if checkFilter optFilter vref.LogicalName then 
        Some(FSMeth(g,typ,vref,pri))
    else 
        None

/// Query the immediate methods of an F# type, not taking into account inherited methods. The optFilter
/// parameter is an optional name to restrict the set of properties returned.
let GetImmediateIntrinsicMethInfosOfType (optFilter,ad) g amap m typ =
    let minfos =

        match metadataOfTy g typ with 
#if EXTENSIONTYPING
        | ExtensionTypeMetadata info -> 
            let st = info.ProvidedType
            let meths = 
                match optFilter with
                | Some name ->  st.PApplyArray ((fun st -> st.GetMethods() |> Array.filter (fun mi -> mi.Name = name) ), "GetMethods", m)
                | None -> st.PApplyArray ((fun st -> st.GetMethods()), "GetMethods", m)
            [   for mi in meths -> ProvidedMeth(g,mi.Coerce(m),amap,m) ]
#endif
        | ILTypeMetadata (_,tdef) -> 
            let tinfo = ILTypeInfo.FromType g typ
            let mdefs = tdef.Methods
            let mdefs = (match optFilter with None -> mdefs.AsList | Some nm -> mdefs.FindByName nm)
            mdefs |> List.map (fun mdef -> ILMethInfo.Create(amap, m, tinfo, None, None, mdef)) 
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 
            if not (isAppTy g typ) then []
            else SelectImmediateMemberVals g optFilter (TrySelectMemberVal g optFilter typ None) (tcrefOfAppTy g typ)
    let minfos = minfos |> List.filter (IsMethInfoAccessible amap m ad)
    minfos

/// A helper type to help collect properties.
///
/// Join up getters and setters which are not associated in the F# data structure 
type PropertyCollector(g,amap,m,typ,optFilter,ad) = 

    let hashIdentity = 
        Microsoft.FSharp.Collections.HashIdentity.FromFunctions 
            (fun (pinfo:PropInfo) -> hash pinfo.PropertyName) 
            (fun pinfo1 pinfo2 -> 
                pinfo1.IsStatic = pinfo2.IsStatic &&
                PropInfosEquivByNameAndPartialSig EraseNone g amap m pinfo1 pinfo2 &&
                pinfo1.IsDefiniteFSharpOverride = pinfo2.IsDefiniteFSharpOverride )
    let props = new System.Collections.Generic.Dictionary<PropInfo,PropInfo>(hashIdentity)
    let add pinfo =
        if props.ContainsKey(pinfo) then 
            match props.[pinfo], pinfo with 
            | FSProp (_,typ,Some vref1,_), FSProp (_,_,_,Some vref2)
            | FSProp (_,typ,_,Some vref2), FSProp (_,_,Some vref1,_)  -> 
                let pinfo = FSProp (g,typ,Some vref1,Some vref2)
                props.[pinfo] <- pinfo 
            | _ -> 
                // This assert fires while editing bad code. We will give a warning later in check.fs
                //assert ("unexpected case"= "")
                ()
        else
            props.[pinfo] <- pinfo

    member x.Collect(membInfo,vref:ValRef) = 
        match membInfo.MemberFlags.MemberKind with 
        | MemberKind.PropertyGet ->
            let pinfo = FSProp(g,typ,Some vref,None) 
            if checkFilter optFilter vref.PropertyName && IsPropInfoAccessible g amap m ad pinfo then
                add pinfo
        | MemberKind.PropertySet ->
            let pinfo = FSProp(g,typ,None,Some vref)
            if checkFilter optFilter vref.PropertyName  && IsPropInfoAccessible g amap m ad pinfo then 
                add pinfo
        | _ -> 
            ()

    member x.Close() = [ for KeyValue(_,pinfo) in props -> pinfo ]

/// Query the immediate properties of an F# type, not taking into account inherited properties. The optFilter
/// parameter is an optional name to restrict the set of properties returned.
let GetImmediateIntrinsicPropInfosOfType (optFilter,ad) g amap m typ =
    let pinfos =

        match metadataOfTy g typ with 
#if EXTENSIONTYPING
        | ExtensionTypeMetadata info -> 
            let st = info.ProvidedType
            let matchingProps =
                match optFilter with
                |   Some name ->
                        match st.PApply((fun st -> st.GetProperty name), m) with
                        |   Tainted.Null -> [||]
                        |   pi -> [|pi|]
                |   None ->
                        st.PApplyArray((fun st -> st.GetProperties()), "GetProperties", m)
            matchingProps
            |> Seq.map(fun pi -> ProvidedProp(g,pi,amap,m)) 
            |> List.ofSeq
#endif
        | ILTypeMetadata (_,tdef) -> 
            let tinfo = ILTypeInfo.FromType g typ
            let pdefs = tdef.Properties
            let pdefs = match optFilter with None -> pdefs.AsList | Some nm -> pdefs.LookupByName nm
            pdefs |> List.map (fun pd -> ILProp(g,ILPropInfo(tinfo,pd))) 
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 

            if not (isAppTy g typ) then []
            else
                let propCollector = new PropertyCollector(g,amap,m,typ,optFilter,ad)
                SelectImmediateMemberVals g None
                           (fun membInfo vref -> propCollector.Collect(membInfo,vref); None)
                           (tcrefOfAppTy g typ) |> ignore
                propCollector.Close()

    let pinfos = pinfos |> List.filter (IsPropInfoAccessible g amap m ad)
    pinfos


//---------------------------------------------------------------------------
// 

/// Sets of methods up the hierarchy, ignoring duplicates by name and sig.
/// Used to collect sets of virtual methods, protected methods, protected
/// properties etc. 
type HierarchyItem = 
    | MethodItem of MethInfo list list
    | PropertyItem of PropInfo list list
    | RecdFieldItem of RecdFieldInfo
    | EventItem of EventInfo list
    | ILFieldItem of ILFieldInfo list

/// An InfoReader is an object to help us read and cache infos. 
/// We create one of these for each file we typecheck. 
///
/// REVIEW: We could consider sharing one InfoReader across an entire compilation 
/// run or have one global one for each (g,amap) pair.
type InfoReader(g:TcGlobals, amap:Import.ImportMap) =

    let getImmediateIntrinsicILFieldsOfType (optFilter,ad) m typ =
        let infos =
            match metadataOfTy g typ with 
#if EXTENSIONTYPING
            | ExtensionTypeMetadata info -> 
                let st = info.ProvidedType
                match optFilter with
                |   None ->
                        [ for fi in st.PApplyArray((fun st -> st.GetFields()), "GetFields" , m) -> ProvidedField(g,fi,amap,m) ]
                |   Some name ->
                        match st.PApply ((fun st -> st.GetField name), m) with
                        |   Tainted.Null -> []
                        |   fi -> [  ProvidedField(g,fi,amap,m) ]
#endif
            | ILTypeMetadata (_,tdef) -> 
                let tinfo = ILTypeInfo.FromType g typ
                let fdefs = tdef.Fields
                let fdefs = match optFilter with None -> fdefs.AsList | Some nm -> fdefs.LookupByName nm
                fdefs |> List.map (fun pd -> ILFieldInfo(tinfo,pd)) 
            | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 
                []
        let infos = infos |> List.filter (IsILFieldInfoAccessible g amap m  ad)
        infos           

    let getImmediateIntrinsicEventsOfType (optFilter,ad) m typ =
        let infos =
            match metadataOfTy g typ with 
#if EXTENSIONTYPING
            | ExtensionTypeMetadata info -> 
                let st = info.ProvidedType
                match optFilter with
                |   None ->
                        [   for ei in st.PApplyArray((fun st -> st.GetEvents()), "GetEvents" , m) -> ProvidedEvent(g,amap,ei,m) ]
                |   Some name ->
                        match st.PApply ((fun st -> st.GetEvent name), m) with
                        |   Tainted.Null -> []
                        |   ei -> [  ProvidedEvent(g,amap,ei,m) ]
#endif
            | ILTypeMetadata (_,tdef) -> 
                let tinfo = ILTypeInfo.FromType g typ
                let edefs = tdef.Events
                let edefs = match optFilter with None -> edefs.AsList | Some nm -> edefs.LookupByName nm
                [ for edef in edefs   do
                    let einfo = ILEventInfo(tinfo,edef)
                    if IsILEventInfoAccessible g amap m ad einfo then 
                        yield ILEvent(g,einfo) ]
            | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 
                []
        infos 


    let mkRecdFieldInfo g typ tcref fspec = 
        RecdFieldInfo(argsOfAppTy g typ,mkNestedRecdFieldRef tcref fspec)

    /// Get the F#-declared record fields or class 'val' fields of a type
    let getImmediateIntrinsicRecdOrClassFieldsOfType (optFilter,_ad) _m typ =
        match tryDestAppTy g typ with 
        | None -> []
        | Some tcref -> 
            // Note;secret fields are not allowed in lookups here, as we're only looking
            // up user-visible fields in name resolution.
            match optFilter with
            | Some nm ->
               match tcref.GetFieldByName nm with
               | Some rfield when not rfield.IsCompilerGenerated -> [mkRecdFieldInfo g typ tcref rfield]
               | _ -> []
            | None -> 
                [ for fdef in tcref.AllFieldsArray do
                    if not fdef.IsCompilerGenerated then
                        yield mkRecdFieldInfo g typ tcref fdef ]


    /// The primitive reader for the method info sets up a hierarchy
    let readRawIntrinsicMethodSetsUncached ((optFilter,ad,allowMultiIntfInst),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> GetImmediateIntrinsicMethInfosOfType (optFilter,ad) g amap m typ :: acc) g amap m allowMultiIntfInst typ []

    /// The primitive reader for the property info sets up a hierarchy
    let readRawIntrinsicPropertySetsUncached ((optFilter,ad,allowMultiIntfInst),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> GetImmediateIntrinsicPropInfosOfType (optFilter,ad) g amap m typ :: acc) g amap m allowMultiIntfInst typ []

    let readIlFieldInfosUncached ((optFilter,ad),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> getImmediateIntrinsicILFieldsOfType (optFilter,ad) m typ @ acc) g amap m AllowMultiIntfInstantiations.No typ []

    let readEventInfosUncached ((optFilter,ad),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> getImmediateIntrinsicEventsOfType (optFilter,ad) m typ @ acc) g amap m AllowMultiIntfInstantiations.No typ []

    let readRecdOrClassFieldInfoUncached ((optFilter,ad),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> getImmediateIntrinsicRecdOrClassFieldsOfType (optFilter,ad) m typ @ acc) g amap m AllowMultiIntfInstantiations.No typ []
    
    let readEntireTypeHierachyUncached (allowMultiIntfInst,m,typ) =
        FoldEntireHierarchyOfType (fun typ acc -> typ :: acc) g amap m allowMultiIntfInst typ  [] 

    let readPrimaryTypeHierachyUncached (allowMultiIntfInst,m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> typ :: acc) g amap m allowMultiIntfInst typ [] 

    /// The primitive reader for the named items up a hierarchy
    let readRawIntrinsicNamedItemsUncached ((nm,ad),m,typ) =
        if nm = ".ctor" then None else // '.ctor' lookups only ever happen via constructor syntax
        let optFilter = Some nm
        FoldPrimaryHierarchyOfType (fun typ acc -> 
             let minfos = GetImmediateIntrinsicMethInfosOfType (optFilter,ad) g amap m typ
             let pinfos = GetImmediateIntrinsicPropInfosOfType (optFilter,ad) g amap m typ 
             let finfos = getImmediateIntrinsicILFieldsOfType (optFilter,ad) m typ 
             let einfos = getImmediateIntrinsicEventsOfType (optFilter,ad) m typ 
             let rfinfos = getImmediateIntrinsicRecdOrClassFieldsOfType (optFilter,ad) m typ 
             match acc with 
             | Some(MethodItem(inheritedMethSets)) when nonNil minfos -> Some(MethodItem (minfos::inheritedMethSets))
             | _ when nonNil minfos -> Some(MethodItem ([minfos]))
             | Some(PropertyItem(inheritedPropSets)) when nonNil pinfos -> Some(PropertyItem(pinfos::inheritedPropSets))
             | _ when nonNil pinfos -> Some(PropertyItem([pinfos]))
             | _ when nonNil finfos -> Some(ILFieldItem(finfos))
             | _ when nonNil einfos -> Some(EventItem(einfos))
             | _ when nonNil rfinfos -> 
                match rfinfos with
                | [single] -> Some(RecdFieldItem(single))
                | _ -> failwith "Unexpected multiple fields with the same name" // Because an explicit name was supplied.
             | _ -> acc)
          g amap m 
          AllowMultiIntfInstantiations.No
          typ
          None

    let makeInfoCache g f (flagsEq : System.Collections.Generic.IEqualityComparer<_>) = 
        new MemoizationTable<_,_>
             (compute=f,
              // Only cache closed, monomorphic types (closed = all members for the type
              // have been processed). Generic type instantiations could be processed if we had 
              // a decent hash function for these.
              canMemoize=(fun (_flags,(_:range),typ) -> 
                                    match stripTyEqns g typ with 
                                    | TType_app(tcref,[]) -> tcref.TypeContents.tcaug_closed 
                                    | _ -> false),
              
              keyComparer=
                 { new System.Collections.Generic.IEqualityComparer<_> with 
                       member x.Equals((flags1,_,typ1),(flags2,_,typ2)) =
                                    // Ignoring the ranges - that's OK.
                                    flagsEq.Equals(flags1,flags2) && 
                                    match stripTyEqns g typ1, stripTyEqns g typ2 with 
                                    | TType_app(tcref1,[]),TType_app(tcref2,[]) -> tyconRefEq g tcref1 tcref2
                                    | _ -> false
                       member x.GetHashCode((flags,_,typ)) =
                                    // Ignoring the ranges - that's OK.
                                    flagsEq.GetHashCode flags + 
                                    (match stripTyEqns g typ with 
                                     | TType_app(tcref,[]) -> hash tcref.LogicalName
                                     | _ -> 0) })

    
    let hashFlags0 = 
        { new System.Collections.Generic.IEqualityComparer<_> with 
               member x.GetHashCode((filter: string option, ad: AccessorDomain, _allowMultiIntfInst1)) = hash filter + AccessorDomain.CustomGetHashCode ad
               member x.Equals((filter1, ad1, allowMultiIntfInst1), (filter2,ad2, allowMultiIntfInst2)) = 
                   (filter1 = filter2) && AccessorDomain.CustomEquals(g,ad1,ad2) && allowMultiIntfInst1 = allowMultiIntfInst2 }

    let hashFlags1 = 
        { new System.Collections.Generic.IEqualityComparer<_> with 
               member x.GetHashCode((filter: string option,ad: AccessorDomain)) = hash filter + AccessorDomain.CustomGetHashCode ad
               member x.Equals((filter1,ad1), (filter2,ad2)) = (filter1 = filter2) && AccessorDomain.CustomEquals(g,ad1,ad2) }

    let hashFlags2 = 
        { new System.Collections.Generic.IEqualityComparer<_> with 
               member x.GetHashCode((nm: string,ad: AccessorDomain)) = hash nm + AccessorDomain.CustomGetHashCode ad
               member x.Equals((nm1,ad1), (nm2,ad2)) = (nm1 = nm2) && AccessorDomain.CustomEquals(g,ad1,ad2) }
                         
    let methodInfoCache = makeInfoCache g readRawIntrinsicMethodSetsUncached hashFlags0
    let propertyInfoCache = makeInfoCache g readRawIntrinsicPropertySetsUncached hashFlags0
    let recdOrClassFieldInfoCache =  makeInfoCache g readRecdOrClassFieldInfoUncached hashFlags1
    let ilFieldInfoCache = makeInfoCache g readIlFieldInfosUncached hashFlags1
    let eventInfoCache = makeInfoCache g readEventInfosUncached hashFlags1
    let namedItemsCache = makeInfoCache g readRawIntrinsicNamedItemsUncached hashFlags2

    let entireTypeHierarchyCache = makeInfoCache g readEntireTypeHierachyUncached HashIdentity.Structural
    let primaryTypeHierarchyCache = makeInfoCache g readPrimaryTypeHierachyUncached HashIdentity.Structural
                                            
    member x.g = g
    member x.amap = amap
    
    /// Read the method infos for a type
    ///
    /// Cache the result for monomorphic types
    member x.GetRawIntrinsicMethodSetsOfType (optFilter,ad,allowMultiIntfInst,m,typ) =
        methodInfoCache.Apply(((optFilter,ad,allowMultiIntfInst),m,typ))

    member x.GetRawIntrinsicPropertySetsOfType (optFilter,ad,allowMultiIntfInst,m,typ) =
        propertyInfoCache.Apply(((optFilter,ad,allowMultiIntfInst),m,typ))

    member x.GetRawIntrinsicRecordOrClassFieldsOfType (optFilter,ad,m,typ) =
        recdOrClassFieldInfoCache.Apply(((optFilter,ad),m,typ))

    member x.GetILFieldInfosOfType (optFilter,ad,m,typ) =
        ilFieldInfoCache.Apply(((optFilter,ad),m,typ))

    member x.GetEventInfosOfType (optFilter,ad,m,typ) =
        eventInfoCache.Apply(((optFilter,ad),m,typ))

    member x.TryFindRecdOrClassFieldInfoOfType (nm:string,m,typ) =
        match recdOrClassFieldInfoCache.Apply((Some nm,AccessibleFromSomewhere),m,typ) with
        | [] -> None
        | [single] -> Some single
        | _ -> failwith "unexpected multiple fields with same name"

    member x.TryFindNamedItemOfType (nm,ad,m,typ) =
        namedItemsCache.Apply(((nm,ad),m,typ))

    member x.ReadEntireTypeHierachy (allowMultiIntfInst,m,typ) =
        entireTypeHierarchyCache.Apply((allowMultiIntfInst,m,typ))

    member x.ReadPrimaryTypeHierachy (allowMultiIntfInst,m,typ) =
        primaryTypeHierarchyCache.Apply((allowMultiIntfInst,m,typ))


//-------------------------------------------------------------------------
// Constructor infos
//------------------------------------------------------------------------- 


/// Get the constructors of an IL type
let private ConstructorInfosOfILType g amap m typ = 
    let tinfo = ILTypeInfo.FromType g typ 
    tinfo.RawMetadata.Methods.FindByName ".ctor" 
    |> List.filter (fun md -> match md.mdKind with MethodKind.Ctor -> true | _ -> false) 
    |> List.map (fun mdef -> ILMethInfo.Create (amap, m, tinfo, None, None, mdef)) 
    
/// Get the constructors of any F# type
let GetIntrinsicConstructorInfosOfType (infoReader:InfoReader) m ty = 
    let g = infoReader.g
    let amap = infoReader.amap 
    if isAppTy g ty then
        match metadataOfTy g ty with 
#if EXTENSIONTYPING
        | ExtensionTypeMetadata info -> 
            let st = info.ProvidedType
            [ for ci in st.PApplyArray((fun st -> st.GetConstructors()), "GetConstructors", m) do
                 yield ProvidedMeth(g,ci.Coerce(m),amap,m) ]
#endif
        | ILTypeMetadata _ -> 
            ConstructorInfosOfILType g amap m ty
        | FSharpOrArrayOrByrefOrTupleOrExnTypeMetadata -> 
            let tcref = tcrefOfAppTy g ty
            tcref.MembersOfFSharpTyconByName 
            |> NameMultiMap.find ".ctor"
            |> List.choose(fun vref -> 
                match vref.MemberInfo with 
                | Some membInfo when (membInfo.MemberFlags.MemberKind = MemberKind.Constructor) -> Some vref 
                | _ -> None) 
            |> List.map (fun x -> FSMeth(g,ty,x,None)) 
    else []
    

  
type FindMemberFlag = 
    /// Prefer items toward the top of the hierarchy, which we do if the items are virtual 
    /// but not when resolving base calls. 
    | IgnoreOverrides 
    /// Get overrides instead of abstract slots when measuring whether a class/interface implements all its required slots. 
    | PreferOverrides

/// The input list is sorted from most-derived to least-derived type, so any System.Object methods 
/// are at the end of the list. Return a filtered list where prior/subsequent members matching by name and 
/// that are in the same equivalence class have been removed. We keep a name-indexed table to 
/// be more efficient when we check to see if we've already seen a particular named method. 
type private IndexedList<'a>(itemLists: 'a list list, itemsByName: 'a NameMultiMap) = 
    member x.Items = itemLists
    member x.ItemsWithName(nm)  = NameMultiMap.find nm itemsByName
    member x.AddItems(items,nmf) = IndexedList<'a>(items::itemLists,List.foldBack (fun x acc -> NameMultiMap.add (nmf x) x acc) items itemsByName )
    

let private excludeItems keepTest nmf itemsToAdd (ilist:IndexedList<_>) = 
    // Have we already seen an item with the same name and that is in the same equivalence class?
    // If so, ignore this one. Note we can check against the original incoming 'ilist' because we are assuming that
    // none the elements of 'itemsToAdd' are equivalent. 
    itemsToAdd |> List.filter (fun item -> List.forall (fun item2 -> keepTest item item2) (ilist.ItemsWithName(nmf item)))

/// Add all the items to the IndexedList if better items are not already present. This is used to hide methods
/// in super classes and/or hide overrides of methods in subclasses.
///
/// Assume no items in 'items' are equivalent according to 'equiv'. This is valid because each step in a
/// .NET class hierarchy introduces a consistent set of methods, none of which hide each other within the 
/// given set. This is an important optimization because it means we don't have to List.filter for equivalence between the 
/// large overload sets introduced by methods like System.WriteLine.
/// Assume items can be given names by 'nmf', where two items with different names are
/// not equivalent.

let private emptyIndexedList() = IndexedList([],NameMultiMap.empty)

let private filterItemsInSubTypesBasedOnItemsInSuperTypes nmf keepTest itemLists = 
    let rec loop itemLists = 
        match itemLists with
        | [] -> emptyIndexedList()
        | items :: itemsInSuperTypes -> 
            let ilist = loop itemsInSuperTypes
            let itemsToAdd = excludeItems keepTest nmf items ilist
            ilist.AddItems(itemsToAdd,nmf)
    (loop itemLists).Items

let private filterItemsInSuperTypesBasedOnItemsInSubTypes nmf keepTest itemLists  = 
    let rec loop itemLists (indexedItemsInSubTypes:IndexedList<_>) = 
        match itemLists with
        | [] -> List.rev indexedItemsInSubTypes.Items
        | items :: itemsInSuperTypes -> 
            let itemsToAdd = items |> List.filter (fun item -> keepTest item (indexedItemsInSubTypes.ItemsWithName(nmf item)))            
            let ilist = indexedItemsInSubTypes.AddItems(itemsToAdd,nmf)
            loop itemsInSuperTypes ilist

    loop itemLists (emptyIndexedList())

let private excludeItemsInSuperTypesBasedOnEquivTestWithItemsInSubTypes nmf equivTest itemLists = 
    filterItemsInSuperTypesBasedOnItemsInSubTypes nmf (fun item1 items -> not (items |> List.exists (fun item2 -> equivTest item1 item2))) itemLists 

let private filterOverrides findFlag (isVirt:'a->bool,isNewSlot,isDefiniteOverride,isFinal,equivSigs,nmf:'a->string) items = 
    let equivVirts x y = isVirt x && isVirt y && equivSigs x y

    match findFlag with 
    | PreferOverrides -> 
        items
        // For each F#-declared override, get rid of any equivalent abstract member in the same type
        // This is because F# abstract members with default overrides give rise to two members with the
        // same logical signature in the same type, e.g.
        // type ClassType1() =
        //      abstract VirtualMethod1: string -> int
        //      default x.VirtualMethod1(s) = 3
        
        |> List.map (fun items -> 
            let definiteOverrides = items |> List.filter isDefiniteOverride 
            items |> List.filter (fun item -> (isDefiniteOverride item || not (List.exists (equivVirts item) definiteOverrides))))
       
        // only keep virtuals that are not signature-equivalent to virtuals in subtypes
        |> excludeItemsInSuperTypesBasedOnEquivTestWithItemsInSubTypes nmf equivVirts 
    | IgnoreOverrides ->  
        let equivNewSlots x y = isNewSlot x && isNewSlot y && equivSigs x y
        items
          // Remove any F#-declared overrides. THese may occur in the same type as the abstract member (unlike with .NET metadata)
          // Include any 'newslot' declared methods.
          |> List.map (List.filter (fun x -> not (isDefiniteOverride x))) 

          // Remove any virtuals that are signature-equivalent to virtuals in subtypes, except for newslots
          // That is, keep if it's 
          ///      (a) not virtual
          //       (b) is a new slot or 
          //       (c) not equivalent
          // We keep virtual finals around for error detection later on
          |> filterItemsInSubTypesBasedOnItemsInSuperTypes nmf (fun newItem priorItem  ->
                 (isVirt newItem && isFinal newItem) || not (isVirt newItem) || isNewSlot newItem || not (equivVirts newItem priorItem) )

          // Remove any abstract slots in supertypes that are (a) hidden by another newslot and (b) implemented
          // We leave unimplemented ones around to give errors, e.g. for
          // [<AbstractClass>]
          //   type PA() =
          //   abstract M : int -> unit
          // 
          //   [<AbstractClass>]
          //   type PB<'a>() =
          //       inherit PA()
          //       abstract M : 'a -> unit
          // 
          //   [<AbstractClass>]
          //   type PC() =
          //       inherit PB<int>()
          //       // Here, PA.M amd PB<int>.M have the same signature, so PA.M is unimplementable.
          //       // REVIEW: in future we may give a friendly error at this point
          // 
          //   type PD() = 
          //       inherit PC()
          //       override this.M(x:int) = ()

          |> filterItemsInSuperTypesBasedOnItemsInSubTypes nmf (fun item1 superTypeItems -> 
                  not (isNewSlot item1 && 
                       superTypeItems |> List.exists (equivNewSlots item1) &&
                       superTypeItems |> List.exists (fun item2 -> isDefiniteOverride item1 && equivVirts item1 item2))) 

    
let private FilterOverridesOfMethInfos findFlag g amap m minfos = 
    filterOverrides findFlag ((fun (minfo:MethInfo) -> minfo.IsVirtual),(fun minfo -> minfo.IsNewSlot),(fun minfo -> minfo.IsDefiniteFSharpOverride),(fun minfo -> minfo.IsFinal),MethInfosEquivByNameAndSig EraseNone true g amap m,(fun minfo -> minfo.LogicalName)) minfos

let private FilterOverridesOfPropInfos findFlag g amap m props = 
    filterOverrides findFlag ((fun (pinfo:PropInfo) -> pinfo.IsVirtualProperty),(fun pinfo -> pinfo.IsNewSlot),(fun pinfo -> pinfo.IsDefiniteFSharpOverride),(fun _ -> false),PropInfosEquivByNameAndSig EraseNone g amap m, (fun pinfo -> pinfo.PropertyName)) props

let ExcludeHiddenOfMethInfos g amap m (minfos:MethInfo list list) = 
    minfos
    |> excludeItemsInSuperTypesBasedOnEquivTestWithItemsInSubTypes 
        (fun minfo -> minfo.LogicalName)
        (fun m1 m2 -> 
             // only hide those truly from super classes 
             not (tyconRefEq g (tcrefOfAppTy g m1.EnclosingType) (tcrefOfAppTy g m2.EnclosingType)) &&
             MethInfosEquivByNameAndPartialSig EraseNone true g amap m m1 m2)
        
    |> List.concat

let ExcludeHiddenOfPropInfos g amap m pinfos = 
    pinfos 
    |> excludeItemsInSuperTypesBasedOnEquivTestWithItemsInSubTypes (fun (pinfo:PropInfo) -> pinfo.PropertyName) (PropInfosEquivByNameAndPartialSig EraseNone g amap m) 
    |> List.concat

let GetIntrinsicMethInfoSetsOfType (infoReader:InfoReader) (optFilter,ad,allowMultiIntfInst) findFlag m typ = 
    infoReader.GetRawIntrinsicMethodSetsOfType(optFilter,ad,allowMultiIntfInst,m,typ)
    |> FilterOverridesOfMethInfos findFlag infoReader.g infoReader.amap m
  
let GetIntrinsicPropInfoSetsOfType (infoReader:InfoReader) (optFilter,ad,allowMultiIntfInst) findFlag m typ = 
    infoReader.GetRawIntrinsicPropertySetsOfType(optFilter,ad,allowMultiIntfInst,m,typ) 
    |> FilterOverridesOfPropInfos findFlag infoReader.g infoReader.amap m

let GetRecordOrClassFieldsOfType (infoReader:InfoReader) (optFilter,ad) m typ = 
    infoReader.GetRawIntrinsicRecordOrClassFieldsOfType(optFilter,ad,m,typ) 

let GetIntrinsicMethInfosOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ = 
    GetIntrinsicMethInfoSetsOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ |> List.concat
  
let GetIntrinsicPropInfosOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ = 
    GetIntrinsicPropInfoSetsOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ  |> List.concat

/// Perform type-directed name resolution of a particular named member in an F# type
let TryFindIntrinsicNamedItemOfType (infoReader:InfoReader) (nm,ad) findFlag m typ = 
    match infoReader.TryFindNamedItemOfType(nm, ad, m, typ) with
    | Some item -> 
        match item with 
        | PropertyItem psets -> Some(PropertyItem (psets |> FilterOverridesOfPropInfos findFlag infoReader.g infoReader.amap m))
        | MethodItem msets -> Some(MethodItem (msets |> FilterOverridesOfMethInfos findFlag infoReader.g infoReader.amap m))
        | _ -> Some(item)
    | None -> None

/// Try to detect the existence of a method on a type.
/// Used for 
///     -- getting the GetEnumerator, get_Current, MoveNext methods for enumerable types 
///     -- getting the Dispose method when resolving the 'use' construct 
///     -- getting the various methods used to desugar the computation expression syntax 
let TryFindIntrinsicMethInfo infoReader m ad nm ty = 
    GetIntrinsicMethInfosOfType infoReader (Some nm,ad,AllowMultiIntfInstantiations.No) IgnoreOverrides m ty 

/// Try to find a particular named property on a type. Only used to ensure that local 'let' definitions and property names
/// are distinct, a somewhat adhoc check in tc.fs.
let TryFindPropInfo infoReader m ad nm ty = 
    GetIntrinsicPropInfosOfType infoReader (Some nm,ad,AllowMultiIntfInstantiations.No) IgnoreOverrides m ty 

//-------------------------------------------------------------------------
// Helpers related to delegates and events
//------------------------------------------------------------------------- 

/// The Invoke MethInfo, the function argument types, the function return type 
/// and the overall F# function type for the function type associated with a .NET delegate type
[<NoEquality;NoComparison>]
type SigOfFunctionForDelegate = SigOfFunctionForDelegate of MethInfo * TType list * TType * TType

/// Given a delegate type work out the minfo, argument types, return type 
/// and F# function type by looking at the Invoke signature of the delegate. 
let GetSigOfFunctionForDelegate (infoReader:InfoReader) delty m ad =
    let g = infoReader.g
    let amap = infoReader.amap
    let invokeMethInfo = 
        match GetIntrinsicMethInfosOfType infoReader (Some "Invoke",ad,AllowMultiIntfInstantiations.No) IgnoreOverrides m delty with 
        | [h] -> h
        | [] -> error(Error(FSComp.SR.noInvokeMethodsFound (),m))
        | h :: _ -> warning(InternalError(FSComp.SR.moreThanOneInvokeMethodFound (),m)); h
    
    let minst = []   // a delegate's Invoke method is never generic 
    let compiledViewOfDelArgTys = 
        match invokeMethInfo.GetParamTypes(amap, m, minst) with 
        | [args] -> args
        | _ -> error(Error(FSComp.SR.delegatesNotAllowedToHaveCurriedSignatures (),m))
    let fsharpViewOfDelArgTys = 
        match compiledViewOfDelArgTys with 
        | [] -> [g.unit_ty] 
        | _ -> compiledViewOfDelArgTys
    let delRetTy = invokeMethInfo.GetFSharpReturnTy(amap, m, minst)
    CheckMethInfoAttributes g m None invokeMethInfo |> CommitOperationResult;
    let fty = mkIteratedFunTy fsharpViewOfDelArgTys delRetTy
    SigOfFunctionForDelegate(invokeMethInfo,compiledViewOfDelArgTys,delRetTy,fty)

/// Try and interpret a delegate type as a "standard" .NET delegate type associated with an event, with a "sender" parameter.
let TryDestStandardDelegateTyp (infoReader:InfoReader) m ad delTy =
    let g = infoReader.g
    let (SigOfFunctionForDelegate(_,compiledViewOfDelArgTys,delRetTy,_)) = GetSigOfFunctionForDelegate infoReader delTy m ad
    match compiledViewOfDelArgTys with 
    | senderTy :: argTys when isObjTy g senderTy  -> Some(mkTupledTy g argTys,delRetTy)
    | _ -> None


/// Indicates if an event info is associated with a delegate type that is a "standard" .NET delegate type
/// with a sender parameter.
//
/// In the F# design, we take advantage of the following idiom to simplify away the bogus "object" parameter of the 
/// of the "Add" methods associated with events.  If you want to access it you
/// can use AddHandler instead.
   
/// The .NET Framework guidelines indicate that the delegate type used for
/// an event should take two parameters, an "object source" parameter
/// indicating the source of the event, and an "e" parameter that
/// encapsulates any additional information about the event. The type of
/// the "e" parameter should derive from the EventArgs class. For events
/// that do not use any additional information, the .NET Framework has
/// already defined an appropriate delegate type: EventHandler.
/// (from http://msdn.microsoft.com/library/default.asp?url=/library/en-us/csref/html/vcwlkEventsTutorial.asp) 
let IsStandardEventInfo (infoReader:InfoReader) m ad (einfo:EventInfo) =
    let dty = einfo.GetDelegateType(infoReader.amap,m)
    match TryDestStandardDelegateTyp infoReader m ad dty with
    | Some _ -> true
    | None -> false

/// Get the (perhaps tupled) argument type accepted by an event 
let ArgsTypOfEventInfo (infoReader:InfoReader) m ad (einfo:EventInfo)  =
    let amap = infoReader.amap
    let dty = einfo.GetDelegateType(amap,m)
    match TryDestStandardDelegateTyp infoReader m ad dty with
    | Some(argtys,_) -> argtys
    | None -> error(nonStandardEventError einfo.EventName m)

/// Get the type of the event when looked at as if it is a property 
/// Used when displaying the property in Intellisense 
let PropTypOfEventInfo (infoReader:InfoReader) m ad (einfo:EventInfo) =  
    let g = infoReader.g
    let amap = infoReader.amap
    let delTy = einfo.GetDelegateType(amap,m)
    let argsTy = ArgsTypOfEventInfo infoReader m ad einfo 
    mkIEventType g delTy argsTy


