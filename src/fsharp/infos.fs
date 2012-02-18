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
open Microsoft.FSharp.Compiler.AbstractIL.IL (* Abstract IL  *)
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Core.Printf

//-------------------------------------------------------------------------
// From IL types to F# types
//------------------------------------------------------------------------- 

/// importInst gives the context for interpreting type variables 
let ImportType scoref amap m importInst ilty = 
    ilty |> rescopeILType scoref |>  Import.ImportILType amap m importInst 

//-------------------------------------------------------------------------
// Fold the hierarchy. 
//------------------------------------------------------------------------- 

let isExnDeclTy g typ = 
    isAppTy g typ && (tcrefOfAppTy g typ).IsExceptionDecl
    
let SuperTypeOfType g amap m typ = 
    let typ = stripTyEqnsAndMeasureEqns g typ

    if isILAppTy g typ then 
        let tcref,tinst = destAppTy g typ
        let scoref,_,tdef = tcref.ILTyconInfo
        match tdef.Extends with 
        | None -> None
        | Some ilty -> Some (ImportType scoref amap m tinst ilty)
    elif isFSharpObjModelTy g typ  || isExnDeclTy g typ then 
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


let InterfacesOfType g amap m typ = 
    let itys = 
        if isAppTy g typ then
            let tcref,tinst = destAppTy g typ
            if tcref.IsMeasureableReprTycon then             
                [g.mk_IComparable_ty; 
                 g.mk_IConvertible_ty; 
                 g.mk_IFormattable_ty; 
                 mkAppTy g.system_GenericIComparable_tcref [typ]; 
                 mkAppTy g.system_GenericIEquatable_tcref [typ]]
            elif tcref.IsILTycon then 
                let scoref,_,tdef = tcref.ILTyconInfo
                tdef.Implements |> List.map (ImportType scoref amap m tinst) 
            else  
                tcref.InterfaceTypesOfFSharpTycon |> List.map (instType (mkInstForAppTy g typ)) 
        else 
            []
        
    // .NET array types are considered to implement IList<T>
    let itys =
        if isArray1DTy g typ then 
            mkSystemCollectionsGenericIListTy g (destArrayTy g typ) :: itys
        else 
            itys
    itys
        
type AllowMultipleInterfaceInstantiations = AllowMultiIntfInst | FirstIntfInst
// Traverse the type hierarchy, e.g. f D (f C (f System.Object acc)). 
// Visit base types and interfaces first
let FoldHierarchyOfTypeAux followInterfaces allowMultiIntfInst visitor g amap m typ acc = 
    let rec loop ndeep typ ((visitedTycon,visited:TyconRefMultiMap<_>,acc) as state) =

        let seenThisTycon = isAppTy g typ && Set.contains (tcrefOfAppTy g typ).Stamp visitedTycon 

        // Do not visit the same type twice. Could only be doing this if we've seen this tycon
        if seenThisTycon && List.exists (typeEquiv g typ) (visited.Find (tcrefOfAppTy g typ)) then state else

        // Do not visit the same tycon twice, e.g. I<int> and I<string>, collect I<int> only, unless directed to allow this
        if seenThisTycon && allowMultiIntfInst = FirstIntfInst then state else

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
                   (InterfacesOfType g amap m typ) 
                      (loop ndeep g.obj_ty state)
            elif isTyparTy g typ then 
                let tp = destTyparTy g typ
                let state = loop (ndeep+1) g.obj_ty state 
                List.foldBack 
                    (fun x vacc -> 
                      match x with 
                      | TTyparMayResolveMemberConstraint _
                      | TTyparDefaultsToType _
                      | TTyparSupportsComparison _
                      | TTyparSupportsEquality _
                      | TTyparIsEnum _
                      | TTyparIsDelegate _
                      | TTyparSupportsNull _
                      | TTyparIsNotNullableValueType _ 
                      | TTyparIsUnmanaged _ 
                      | TTyparIsReferenceType _ 
                      | TTyparSimpleChoice _ 
                      | TTyparRequiresDefaultConstructor _ -> vacc
                      | TTyparCoercesToType(cty,_) -> 
                              loop (ndeep + 1)  cty vacc) 
                    tp.Constraints 
                    state
            else 
                let state = 
                    if followInterfaces then 
                        List.foldBack 
                          (loop (ndeep+1)) 
                          (InterfacesOfType g amap m typ) 
                          state 
                    else 
                        state
                let state = 
                    Option.foldBack 
                      (loop (ndeep+1)) 
                      (SuperTypeOfType g amap m typ) 
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

let ExistsInEntireHierarchyOfType f g amap m allowMultiIntfInst typ = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst (fun ty acc -> acc || f ty ) g amap m typ false 

let SearchEntireHierarchyOfType f g amap m typ = 
    FoldHierarchyOfTypeAux true FirstIntfInst
        (fun ty acc -> 
            match acc with 
            | None -> if f ty then Some(ty) else None 
            | Some _ -> acc) 
        g amap m typ None

// All super types of the type, including the type itself
let AllSuperTypesOfType g amap m allowMultiIntfInst ty = 
    FoldHierarchyOfTypeAux true allowMultiIntfInst (ListSet.insert (typeEquiv g)) g amap m ty [] 

let AllInterfacesOfType g amap m allowMultiIntfInst ty = 
    AllSuperTypesOfType g amap m allowMultiIntfInst ty |> List.filter (isInterfaceTy g)

let HaveSameHeadType g ty1 ty2 = 
    isAppTy g ty1 && isAppTy g ty2 &&
    tyconRefEq g (tcrefOfAppTy g ty1) (tcrefOfAppTy g ty2)

let HasHeadType g tcref ty2 = 
        isAppTy g ty2 &&
        tyconRefEq g tcref (tcrefOfAppTy g ty2)
        

let ExistsSameHeadTypeInHierarchy g amap m typeToSearchFrom typeToLookFor = 
    ExistsInEntireHierarchyOfType (HaveSameHeadType g typeToLookFor)  g amap m FirstIntfInst typeToSearchFrom
  
let ExistsHeadTypeInEntireHierarchy g amap m typeToSearchFrom tcrefToLookFor = 
    ExistsInEntireHierarchyOfType (HasHeadType g tcrefToLookFor) g amap m FirstIntfInst typeToSearchFrom
  

let ImportTypeFromMetadata amap m scoref tinst minst ilty = 
    ImportType scoref amap m (tinst@minst) ilty

//-------------------------------------------------------------------------
// Predicates and properties on values and members
//------------------------------------------------------------------------- 
 
let MemberRefIsVirtual (vref:ValRef) = 
    let flags = vref.MemberInfo.Value.MemberFlags
    flags.IsDispatchSlot || flags.IsOverrideOrExplicitImpl

let MemberRefIsDefiniteFSharpOverride (vref:ValRef) = 
    let membInfo = vref.MemberInfo.Value   
    let flags = membInfo.MemberFlags
    not flags.IsDispatchSlot && (flags.IsOverrideOrExplicitImpl || nonNil membInfo.ImplementedSlotSigs)

let MemberRefIsDispatchSlot (vref:ValRef) =  
    let membInfo = vref.MemberInfo.Value
    membInfo.MemberFlags.IsDispatchSlot 

type ValRef with 
    member x.IsFSharpEventProperty(g) = 
        x.IsMember && CompileAsEvent g x.Attribs && not x.IsExtensionMember

//-------------------------------------------------------------------------
// Basic infos
//------------------------------------------------------------------------- 

type ILTypeInfo = 
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



type TypeInfo = 
    | ILType of ILTypeInfo
    | FSType of TType

type ILMethInfo =
    | ILMethInfo of ILTypeInfo * ILTypeRef option (* extension? *) * ILMethodDef * Typars (* typars are the uninstantiated generic method args *) 

    member x.ILTypeInfo = let (ILMethInfo(tinfo,_,_,_)) = x in tinfo
    member x.RawMetadata = let (ILMethInfo(_,_,md,_)) = x in md
    member x.ExtensionMethodInfo = let (ILMethInfo(_,extInfo,_,_)) = x in extInfo
    member x.ILTypeRef = x.ILTypeInfo.ILTypeRef
    member x.ILName       = x.RawMetadata.Name

    // methods to hide logic related to extension methods
    member x.IsCSharpExtensionMethod = x.ExtensionMethodInfo.IsSome

    member x.ActualILTypeRef   = 
        match x.ExtensionMethodInfo with 
        | None -> x.ILTypeRef
        | Some info -> info

    member x.ActualTypeInst = 
        match x.ExtensionMethodInfo with 
        | None -> x.ILTypeInfo.TypeInst
        | Some _info -> []

    member x.MetadataScope   = x.ActualILTypeRef.Scope
    
    member x.ParamMetadata = 
        let ps = x.RawMetadata.Parameters in 
        if x.IsCSharpExtensionMethod then List.tail ps else ps

    member x.NumParams = x.ParamMetadata.Length
    
    member x.GenericArity = x.RawMetadata.GenericParams.Length 
   
    member x.IsConstructor = x.RawMetadata.IsConstructor 
    member x.IsClassConstructor = x.RawMetadata.IsClassInitializer

    member x.IsProtectedAccessibility = 
        let md = x.RawMetadata 
        not md.IsConstructor &&
        not md.IsClassInitializer &&
        (md.Access = ILMemberAccess.Family) &&
        not md.CallingConv.IsStatic 

    member x.IsVirtual = x.RawMetadata.IsVirtual
    member x.IsFinal = x.RawMetadata.IsFinal

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

    member x.ArgTypes(amap,m,minst) = 
        x.ParamMetadata |> List.map (fun p -> ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type) 

    member x.ParamInfos(amap,m,minst) = 
        x.ParamMetadata |> List.map (fun p -> p.Name, ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type) 

    member x.EnclosingType = x.ILTypeInfo.ToType


type ExtensionMethodPriority = uint64

type MethInfo = 
    | FSMeth of TcGlobals * TType * ValRef  * ExtensionMethodPriority option
    | ILMeth of TcGlobals * ILMethInfo * ExtensionMethodPriority option
    | DefaultStructCtor of TcGlobals * TType
    /// Get the enclosing ("parent") type of the method info. 
    member x.EnclosingType = 
      match x with
      | ILMeth(_g,x,_) -> x.EnclosingType
      | FSMeth(_g,typ,_,_) -> typ
      | DefaultStructCtor(_g,typ) -> typ
    member x.Priority = 
      match x with
      | ILMeth(_,_,Some pri) -> pri
      | FSMeth(_,_,_,Some pri) -> pri
      | _ -> System.UInt64.MaxValue // all others take prioity over extension members
    member x.LogicalName = 
        match x with 
        | ILMeth(_,y,_) -> y.ILName
        | FSMeth(_,_,vref,_) -> vref.LogicalName
        | DefaultStructCtor _ -> ".ctor"
    member x.IsInThisAssembly compilingFsLib =
        match x with
        | FSMeth(_,_,vref,_) -> valRefInThisAssembly compilingFsLib vref
        | _ -> false
    override x.ToString() =  x.EnclosingType.ToString() + x.LogicalName

type ILFieldInfo = 
    | ILFieldInfo of ILTypeInfo * ILFieldDef (* .NET IL fields *)

    //member x.ILTypeInfo = (let (ILFieldInfo(tinfo,_)) = x in tinfo)
    //member x.RawMetadata = (let (ILFieldInfo(_,pd)) = x in pd)
    member x.ScopeRef = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.ILScopeRef
    member x.ILTypeRef = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.ILTypeRef
    
    member x.TypeInst = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.TypeInst
    member x.FieldName = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.Name
    member x.IsInitOnly = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsInitOnly
    member x.IsValueType = 
        match x with 
        | ILFieldInfo(tinfo,_) -> tinfo.IsValueType
    member x.IsStatic = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsStatic
    member x.IsSpecialName = 
        match x with 
        | ILFieldInfo(_,pd) -> pd.IsSpecialName
    
    member x.LiteralValue = 
        match x with 
        | ILFieldInfo(_,pd) -> if pd.IsLiteral then pd.LiteralValue else None
                                        
    member x.ILFieldType = 
        match x with 
        | ILFieldInfo (_,fdef) -> fdef.Type
    member x.FieldType(amap,m) = 
        match x with 
        | ILFieldInfo (tinfo,fdef) -> ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] fdef.Type

    member x.ILFieldRef = rescopeILFieldRef x.ScopeRef (mkILFieldRef(x.ILTypeRef,x.FieldName,x.ILFieldType))
    override x.ToString() =  x.FieldName

type RecdFieldInfo = 
    | RecdFieldInfo of TypeInst * Tast.RecdFieldRef (* F# fields *)
    member x.TypeInst = let (RecdFieldInfo(tinst,_)) = x in tinst
    member x.RecdFieldRef = let (RecdFieldInfo(_,rfref)) = x in rfref
    member x.RecdField = x.RecdFieldRef.RecdField
    member x.IsStatic = x.RecdField.IsStatic
    member x.LiteralValue = x.RecdField.LiteralValue
    member x.TyconRef = x.RecdFieldRef.TyconRef
    member x.Tycon = x.RecdFieldRef.Tycon
    member x.Name = x.RecdField.Name
    member x.FieldType = actualTyOfRecdFieldRef x.RecdFieldRef x.TypeInst
    member x.EnclosingType = TType_app (x.RecdFieldRef.TyconRef,x.TypeInst)
    override x.ToString() = x.TyconRef.ToString() + "::" + x.Name
    
type UnionCaseInfo = 
    | UnionCaseInfo of TypeInst * Tast.UnionCaseRef 
    member x.TypeInst = let (UnionCaseInfo(tinst,_)) = x in tinst
    member x.UnionCaseRef = let (UnionCaseInfo(_,ucref)) = x in ucref
    member x.UnionCase = x.UnionCaseRef.UnionCase
    member x.TyconRef = x.UnionCaseRef.TyconRef
    member x.Tycon = x.UnionCaseRef.Tycon
    member x.Name = x.UnionCase.DisplayName
    override x.ToString() = x.TyconRef.ToString() + "::" + x.Name


type ILPropInfo = 
    | ILPropInfo of ILTypeInfo * ILPropertyDef 

    member x.ILTypeInfo = match x with (ILPropInfo(tinfo,_)) -> tinfo
    member x.RawMetadata = match x with (ILPropInfo(_,pd)) -> pd
    member x.PropertyName = x.RawMetadata.Name

    member x.GetterMethod = 
        assert (x.HasGetter)
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata (the x.RawMetadata.GetMethod)
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 

    member x.SetterMethod = 
        assert (x.HasSetter)
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata (the x.RawMetadata.SetMethod)
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 
          
    member x.HasGetter = isSome x.RawMetadata.GetMethod 
    member x.HasSetter = isSome x.RawMetadata.SetMethod 
    member x.IsStatic = (x.RawMetadata.CallingConv = ILThisConvention.Static) 
    override x.ToString() = x.ILTypeInfo.ToString() + "::" + x.PropertyName


type PropInfo = 
    | FSProp of TcGlobals * TType * ValRef option * ValRef option
    | ILProp of TcGlobals * ILPropInfo
    member x.IsInThisAssembly compilingFsLib =
        match x with
        | FSProp(_,_,Some(vref),_)
        | FSProp(_,_,_,Some(vref)) -> valRefInThisAssembly compilingFsLib vref
        | _ -> false

type ILEventInfo = 
    | ILEventInfo of ILTypeInfo * ILEventDef
    member x.RawMetadata = match x with (ILEventInfo(_,ed)) -> ed
    member x.ILTypeInfo = match x with (ILEventInfo(tinfo,_)) -> tinfo
    member x.AddMethod =
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.AddMethod
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 

    member x.RemoveMethod =
        let mdef = resolveILMethodRef x.ILTypeInfo.RawMetadata x.RawMetadata.RemoveMethod
        ILMethInfo(x.ILTypeInfo,None,mdef,[]) 

    member x.TypeRef = x.ILTypeInfo.ILTypeRef
    member x.Name = x.RawMetadata.Name
    member x.IsStatic = x.AddMethod.IsStatic
    override x.ToString() = x.ILTypeInfo.ToString() + "::" + x.Name


type EventInfo = 
    | FSEvent of TcGlobals * PropInfo * ValRef * ValRef
    | ILEvent of TcGlobals * ILEventInfo


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
           | TTyparCoercesToType(ty,_) -> 
               TTyparCoercesToType (instType tprefInst ty,m)
           | TTyparDefaultsToType(priority,ty,_) -> 
               TTyparDefaultsToType (priority,instType tprefInst ty,m)
           | TTyparSupportsNull _ -> 
               TTyparSupportsNull m
           | TTyparIsEnum (uty,_) -> 
               TTyparIsEnum (instType tprefInst uty,m)
           | TTyparSupportsComparison _ -> 
               TTyparSupportsComparison m
           | TTyparSupportsEquality _ -> 
               TTyparSupportsEquality m
           | TTyparIsDelegate(aty, bty,_) -> 
               TTyparIsDelegate (instType tprefInst aty,instType tprefInst bty,m)
           | TTyparIsNotNullableValueType _ -> 
               TTyparIsNotNullableValueType m
           | TTyparIsUnmanaged _ ->
               TTyparIsUnmanaged m
           | TTyparIsReferenceType _ -> 
               TTyparIsReferenceType m
           | TTyparSimpleChoice (tys,_) -> 
               TTyparSimpleChoice (List.map (instType tprefInst) tys,m)
           | TTyparRequiresDefaultConstructor _ -> 
               TTyparRequiresDefaultConstructor m
           | TTyparMayResolveMemberConstraint(traitInfo,_) -> 
               TTyparMayResolveMemberConstraint (instTrait tprefInst traitInfo,m))

/// The constraints for each typar copied from another typar can only be fixed up once 
/// we have generated all the new constraints, e.g. f<A :> List<B>, B :> List<A>> ... 
let FixupNewTypars m ftctps tinst tpsorig tps =
    let renaming,tptys = mkTyparToTyparRenaming tpsorig tps
    let tprefInst = mkTyparInst ftctps tinst @ renaming
    (tpsorig,tps) ||> List.iter2 (fun tporig tp -> tp.FixupConstraints (CopyTyparConstraints  m tprefInst tporig)) ;
    renaming,tptys


//-------------------------------------------------------------------------
// tinfos
//------------------------------------------------------------------------- 

/// Build IL method infos.  
let mkILMethInfo amap m (tinfo:ILTypeInfo) (extInfo:ILTypeRef option) extMethPri (md: ILMethodDef) =     
    let tinst,scoref =  
        match extInfo with 
        | None -> 
            tinfo.TypeInst,tinfo.ILScopeRef
        | Some tref -> 
            // C# extension methods have no type typars
            [], tref.Scope
    let mtps = Import.ImportIlTypars (fun () -> amap) m scoref tinst md.GenericParams
    ILMeth (amap.g,ILMethInfo(tinfo,extInfo, md,mtps),extMethPri)

//-------------------------------------------------------------------------
// Additional operations on ILMethInfo, ILPropInfo
//------------------------------------------------------------------------- 


// Get the logical object parameters of a type
let ObjTypesOfILMethInfo amap m (x:ILMethInfo) minst =
    // all C# extension methods are instance
    if x.IsCSharpExtensionMethod then 
        x.RawMetadata.Parameters |> List.head |> (fun p -> [ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst minst p.Type]) 
    elif x.IsInstance then 
        [x.EnclosingType]
    else
        []
        
let ImportReturnTypeFromMetaData amap m ty scoref tinst minst =
    match ty with 
    | ILType.Void -> None
    | retTy ->  Some (ImportTypeFromMetadata amap m scoref tinst minst retTy)

let GetCompiledReturnTyOfILMethod amap m (x:ILMethInfo) minst =
    ImportReturnTypeFromMetaData amap m x.RawMetadata.Return.Type x.MetadataScope x.ActualTypeInst minst 

let GetFSharpReturnTyOfILMethod amap m minfo minst = 
    GetCompiledReturnTyOfILMethod amap m minfo minst 
    |> GetFSharpViewOfReturnType amap.g

type ILMethInfo with 
    member minfo.ILMethodRef = 
        let mref = mkRefToILMethod (minfo.ActualILTypeRef,minfo.RawMetadata)
        rescopeILMethodRef minfo.MetadataScope mref 


let isILMethInfoDllImport g (minfo:ILMethInfo) = 
    let (AttribInfo(tref,_)) = g.attrib_DllImportAttribute
    minfo.RawMetadata.CustomAttrs |> ILThingDecodeILAttrib g tref (Some(tref.Scope))  |> isSome

/// Build an expression node that is a call to a .NET method. *)
let BuildILMethInfoCall g amap m isProp (minfo:ILMethInfo) valUseFlags minst direct args = 
    let valu = minfo.ILTypeInfo.IsValueType
    let ctor = minfo.IsConstructor
    if minfo.IsClassConstructor then 
        error (InternalError (minfo.ILName^": cannot call a class constructor",m));
    let useCallvirt = 
        not valu && not direct && minfo.IsVirtual
    let isProtected = minfo.IsProtectedAccessibility
    let mref = minfo.ILMethodRef
    let newobj = ctor && (match valUseFlags with NormalValUse -> true | _ -> false)
    let exprty = if ctor then minfo.EnclosingType else GetFSharpReturnTyOfILMethod amap m minfo minst
    // The thing might be an extension method, in which case adjust the instantiations
    let actualTypeInst = minfo.ActualTypeInst
    let actualMethInst = minst
    let retTy = (if not ctor && (mref.ReturnType = ILType.Void) then [] else [exprty])
    let isDllImport = isILMethInfoDllImport g minfo
    Expr.Op(TOp.ILCall(useCallvirt,isProtected,valu,newobj,valUseFlags,isProp,isDllImport,mref,actualTypeInst,actualMethInst, retTy),[],args,m),
    exprty

let BuildObjCtorCall g m =
    let mref = (mkILCtorMethSpecForTy(g.ilg.typ_Object,[])).MethodRef
    Expr.Op(TOp.ILCall(false,false,false,false,CtorValUsedAsSuperInit,false,false,mref,[],[],[g.obj_ty]),[],[],m)

//-------------------------------------------------------------------------
// .NET Property Infos
//------------------------------------------------------------------------- 

let getILPropDefAccessibility tdef pd =   
    match pd.GetMethod with 
    | Some mref -> (resolveILMethodRef tdef mref).Access 
    | None -> 
        match pd.SetMethod with 
        | None -> ILMemberAccess.Public
        | Some mref -> (resolveILMethodRef tdef mref).Access

type ILPropInfo with 
    member pinfo.IsVirtual = 
        (pinfo.HasGetter && pinfo.GetterMethod.IsVirtual) ||
        (pinfo.HasSetter && pinfo.SetterMethod.IsVirtual) 

    member pinfo.IsNewSlot = 
        (pinfo.HasGetter && pinfo.GetterMethod.IsNewSlot) ||
        (pinfo.HasSetter && pinfo.SetterMethod.IsNewSlot) 

    member pinfo.ParamNamesAndTypes(amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = pinfo
        pdef.Args |> List.map (fun ty -> None, ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] ty) 

    member pinfo.PropertyType (amap,m) = 
        let (ILPropInfo (tinfo,pdef)) = pinfo
        ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] pdef.Type

//-------------------------------------------------------------------------
// .NET Event Infos
//------------------------------------------------------------------------- 


let DelegateTypeOfILEventInfo amap m (ILEventInfo(tinfo,edef)) =
    ImportTypeFromMetadata amap m tinfo.ILScopeRef tinfo.TypeInst [] (the edef.Type)


//-------------------------------------------------------------------------
// Testing equality & calculating hash codes of method/prop/event infos
//------------------------------------------------------------------------- 

/// Do two minfos have the same underlying definitions? 
/// Used to merge operator overloads collected from left and right of an operator constraint 
let MethInfosUseIdenticalDefinitions _g x1 x2 = 
    match x1,x2 with 
    | ILMeth(_,x1,_), ILMeth(_,x2,_) -> (x1.RawMetadata ===  x2.RawMetadata)
    | FSMeth(g,_,vref1,_), FSMeth(_,_,vref2,_)  -> valRefEq g vref1 vref2 
    | DefaultStructCtor(g,ty1), DefaultStructCtor(_,ty2) -> tyconRefEq g (tcrefOfAppTy g ty1) (tcrefOfAppTy g ty2) 
    | _ -> false

/// Tests whether two property infos have the same underlying defintion
/// (uses the same techniques as pervious 'MethInfosUseIdenticalDefinitions')
let PropInfosUseIdenticalDefinitions x1 x2 = 
    let optVrefEq g = function 
      | Some(v1), Some(v2) -> valRefEq g v1 v2
      | None, None -> true
      | _ -> false    
    match x1,x2 with 
    | ILProp(_, x1), ILProp(_, x2) -> (x1.RawMetadata === x2.RawMetadata)
    | FSProp(g, _, vrefa1, vrefb1), FSProp(_, _, vrefa2, vrefb2) ->
        (optVrefEq g (vrefa1, vrefa2)) && (optVrefEq g (vrefb1, vrefb2))
    | _ -> false

/// Test whether two event infos have the same underlying defintion (similar as above)
let EventInfosUseIdenticalDefintions x1 x2 =
    match x1, x2 with
    | FSEvent(g, pi1, vrefa1, vrefb1), FSEvent(_, pi2, vrefa2, vrefb2) ->
        PropInfosUseIdenticalDefinitions pi1 pi2 && valRefEq g vrefa1 vrefa2 && valRefEq g vrefb1 vrefb2
    | ILEvent(_, x1), ILEvent(_, x2) -> (x1.RawMetadata === x2.RawMetadata)
    | _ -> false
  
/// Calculates a hash code of method info. Note: this is a very imperfect implementation,
/// but it works decently for comparing methods in the language service...
let GetMethInfoHashCode mi = 
    match mi with 
    | ILMeth(_,x1,_) -> hash x1.RawMetadata.Name
    | FSMeth(_,_,vref,_) -> hash vref.LogicalName
    | DefaultStructCtor(_,_ty) -> 34892 // "ty" doesn't support hashing. We could use "hash (tcrefOfAppTy g ty).CompiledName" or 
                                       // something but we don't have a "g" parameter here yet. But this hash need only be very approximate anyway

/// Calculates a hash code of property info (similar as previous)
let GetPropInfoHashCode mi = 
    match mi with 
    | ILProp(_, x1) -> hash x1.RawMetadata.Name
    | FSProp(_,_,vrefOpt1, vrefOpt2) -> 
        // Value to hash is option<string>*option<string>, which can be hashed efficiently
        let vth = vrefOpt1 |> Option.map (fun vr -> vr.LogicalName), vrefOpt2 |> Option.map (fun vr -> vr.LogicalName)
        hash(vth)

/// Calculates a hash code of event info (similar as previous)
let GetEventInfoHashCode mi = 
    match mi with 
    | ILEvent(_, x1) -> hash x1.RawMetadata.Name
    | FSEvent(_, pi, vref1, vref2) -> hash (GetPropInfoHashCode pi, vref1.LogicalName, vref2.LogicalName)

//-------------------------------------------------------------------------
// minfo, pinfo
//------------------------------------------------------------------------- 

/// Apply a type instantiation to a method info, i.e. apply the instantiation to the enclosing type. 
let InstMethInfo amap m inst meth = 
  match meth with 
  | ILMeth(_g,x,pri) -> mkILMethInfo amap m (x.ILTypeInfo.Instantiate inst) x.ExtensionMethodInfo pri x.RawMetadata
  | FSMeth(g,typ,vref,pri) -> FSMeth(g,instType inst typ,vref,pri)
  | DefaultStructCtor(g,typ) -> DefaultStructCtor(g,instType inst typ)

let AnalyzeTypeOfMemberVal g (typ,vref) = 
    (* if vref.RecursiveValInfo then retTy else *)
    let tps,_,retTy,_ = GetTypeOfMemberInMemberForm g vref
    
    let parentTyargs = argsOfAppTy g typ
    let memberParentTypars,memberMethodTypars = List.chop parentTyargs.Length tps

    memberParentTypars,memberMethodTypars,retTy,parentTyargs


type MethInfo with 

    member x.ActualTypeInst = 
        match x with 
        | ILMeth(_g,y,_) -> y.ActualTypeInst
        | FSMeth(g,_,_,_) | DefaultStructCtor(g,_) -> argsOfAppTy g x.EnclosingType

    member x.TcGlobals = 
        match x with 
        | ILMeth(g,_,_) -> g
        | FSMeth(g,_,_,_) -> g
        | DefaultStructCtor (g,_) -> g


    member x.FormalMethodTypars = 
        match x with 
        | ILMeth(_g,ILMethInfo(_tinfo,_extInfo,_,mtps),_) ->  mtps
        | FSMeth(g,typ,vref,_) ->  
           let _,mtps,_,_ = AnalyzeTypeOfMemberVal g (typ,vref)
           mtps 
        | DefaultStructCtor _ -> []
           
    member x.FormalMethodInst = generalizeTypars x.FormalMethodTypars

    member x.XmlDoc = 
        match x with 
        | ILMeth(_,_x,_) -> XmlDoc.Empty
        | FSMeth(_,_,vref,_) -> vref.XmlDoc
        | DefaultStructCtor _ -> XmlDoc.Empty

    member x.ArbitraryValRef = 
        match x with 
        | ILMeth(_g,_x,_) -> None
        | FSMeth(_g,_,vref,_) -> Some(vref)
        | DefaultStructCtor _ -> None

/// Combine the type instantiation and generic method instantiation
let CombineMethInsts ttps mtps tinst minst = (mkTyparInst ttps tinst @ mkTyparInst mtps minst) 

let CompiledReturnTyOfMeth amap m minfo minst = 
    match minfo with 
    | ILMeth(_g,ilminfo,_) -> GetCompiledReturnTyOfILMethod amap m ilminfo minst
    | FSMeth(g,typ,vref,_) -> 
       let ttps,mtps,retTy,tinst = AnalyzeTypeOfMemberVal g (typ,vref)
       Option.map (instType (CombineMethInsts ttps mtps tinst minst)) retTy
    | DefaultStructCtor _ -> None

let FSharpReturnTyOfMeth amap m minfo minst =
    CompiledReturnTyOfMeth amap m minfo minst |> GetFSharpViewOfReturnType amap.g
       
let ParamOfArgInfo (ty,argInfo : ArgReprInfo) = (Option.map textOfId argInfo.Name, ty)

let ParamsOfMember g vref = ArgInfosOfMember g vref |> List.mapSquared ParamOfArgInfo

let InstParam inst param = 
    map2Of2 (instType inst) param

let InstParams inst paramTypes = 
    paramTypes |> List.mapSquared (InstParam inst)

let ParamTypesOfMethInfo amap m minfo minst = 
    match minfo with 
    | ILMeth(_g,ilminfo,_) -> 
        [ ilminfo.ArgTypes(amap,m,minst) ]
    | FSMeth(g,typ,vref,_) -> 
        let ttps,mtps,_,tinst = AnalyzeTypeOfMemberVal g (typ,vref)
        let paramTypes = ParamsOfMember g vref
        let inst = (CombineMethInsts ttps mtps tinst minst)
        paramTypes |> List.mapSquared (snd  >> instType inst) 
    | DefaultStructCtor _ -> []

let ObjTypesOfMethInfo amap m minfo minst = 
    match minfo with 
    | ILMeth(_g,ilminfo,_) -> ObjTypesOfILMethInfo amap m ilminfo minst
    | FSMeth(_g,typ,vref,_) -> if vref.IsInstanceMember then [typ] else []
    | DefaultStructCtor _ -> []

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

let ParamAttribsOfMethInfo amap m minfo = 
    match minfo with 
    | ILMeth(g,x,_) -> 
        x.ParamMetadata
        |> List.map (fun p -> 
             let isParamArrayArg = ILThingHasAttrib g.attrib_ParamArrayAttribute p.CustomAttrs
             let isOutArg = (p.IsOut && not p.IsIn)
             (* Note: we get default argument values frmo VB and other .NET language metadata *)
             let optArgInfo = 
                 if p.IsOptional then 
                     CallerSide (match p.Default with 
                                 | None -> 
                                        let rec analyze ty = 
                                            if isByrefTy g ty then 
                                                let ty = destByrefTy g ty
                                                PassByRef (ty, analyze ty)
                                            elif isObjTy g ty then
                                                if ILThingHasAttrib g.attrib_IDispatchConstantAttribute p.CustomAttrs then
                                                    WrapperForIDispatch
                                                elif ILThingHasAttrib g.attrib_IUnknownConstantAttribute p.CustomAttrs then
                                                    WrapperForIUnknown
                                                else 
                                                    MissingValue
                                            else 
                                                DefaultValue
                                        analyze (ImportTypeFromMetadata amap m x.MetadataScope x.ActualTypeInst [] p.Type)

                                 | Some v -> Constant v)
                 else NotOptional
             (isParamArrayArg, isOutArg, optArgInfo))
        |> List.singleton
    | FSMeth(g,_,vref,_) -> 
        vref 
        |> ArgInfosOfMember g 
        |> List.mapSquared (fun (ty,argInfo) -> 
            let isParamArrayArg = HasAttrib g g.attrib_ParamArrayAttribute argInfo.Attribs
            let isOutArg = HasAttrib g g.attrib_OutAttribute argInfo.Attribs && isByrefTy g ty
            let isOptArg = HasAttrib g g.attrib_OptionalArgumentAttribute argInfo.Attribs
            // Note: can't specify caller-side default arguments in F#, by design (default is specified on the callee-side) 
            let optArgInfo = if isOptArg then CalleeSide else NotOptional
            (isParamArrayArg,isOutArg,optArgInfo))
    | DefaultStructCtor _ -> 
        [[]]


let mkSlotParam (ty,argInfo:ArgReprInfo) = TSlotParam(Option.map textOfId argInfo.Name, ty, false,false,false,argInfo.Attribs) 

let mkSlotSig (nm,typ,ctps,mtps,paraml,retTy) = copySlotSig (TSlotSig(nm,typ,ctps,mtps,paraml,retTy))


// This code has grown organically over time. We've managed to unify the ILMeth+EstMeth paths.
// The FSMeth, ILMeth+EstMeth paths can probably be unified too.
let SlotSigOfMethodInfo amap m minfo =
    match minfo with 
    | FSMeth(g,typ,vref,_) -> 
        match vref.RecursiveValInfo with 
        | ValInRecScope(false) -> error(Error((FSComp.SR.InvalidRecursiveReferenceToAbstractSlot()),m));
        | _ -> ()

        let tps,_,retTy,_ = GetTypeOfMemberInMemberForm g vref
        let ctps = (tcrefOfAppTy g typ).Typars(m)
        let ctpsorig,fmtps = List.chop ctps.Length tps
        let crenaming,_ = mkTyparToTyparRenaming ctpsorig ctps
        let fparams = 
            vref 
            |> ArgInfosOfMember g 
            |> List.mapSquared (map1Of2 (instType crenaming) >> mkSlotParam )
        let frty = Option.map (instType crenaming) retTy
        mkSlotSig(minfo.LogicalName,minfo.EnclosingType,ctps,fmtps,fparams, frty)
    | DefaultStructCtor _ -> error(InternalError("no slotsig for DefaultStructCtor",m))
    | _ -> 
        let g = minfo.TcGlobals
        // slotsigs must contain the formal types for the arguments and return type 
        // a _formal_ 'void' return type is represented as a 'unit' type. 
        // slotsigs are independent of instantiation: if an instantiation 
        // happens to make the return type 'unit' (i.e. it was originally a variable type 
        // then that does not correspond to a slotsig compiled as a 'void' return type. 
        let tcref =  tcrefOfAppTy g minfo.EnclosingType
        let filtctps = tcref.Typars(m)
        let ftctps = copyTypars filtctps
        let _,ftctptys = FixupNewTypars m [] [] filtctps ftctps
        let ftinfo = ILTypeInfo.FromType g (TType_app(tcref,ftctptys))
        let fmtps = copyTypars minfo.FormalMethodTypars
        let _,fmtptys = FixupNewTypars m ftctps ftctptys minfo.FormalMethodTypars fmtps
        let frty, fparams = 
            match minfo with
            | ILMeth(_,ilminfo,_) -> 
                let (ILMethInfo(_,_,_,_filmtps)) = ilminfo
                let frty = ImportReturnTypeFromMetaData amap m ilminfo.RawMetadata.Return.Type ftinfo.ILScopeRef ftinfo.TypeInst fmtptys
                let fparams = [ ilminfo.RawMetadata.Parameters |> List.map (fun p -> TSlotParam(p.Name, ImportTypeFromMetadata amap m ftinfo.ILScopeRef ftinfo.TypeInst fmtptys p.Type,p.IsIn, p.IsOut, p.IsOptional,[])) ]
                frty, fparams
            | _ -> failwith "unreachable"
        mkSlotSig(minfo.LogicalName,minfo.EnclosingType,ftctps, fmtps,fparams, frty)
    

// The slotsig returned by SlotSigOfMethodInfo is in terms of the type parameters on the parent type of the overriding method,
//
// Reverse-map the slotsig so it is in terms of the type parameters for the overriding method 
let ReparentSlotSigToUseMethodTypars g _amap m ovByMethValRef slotsig = 

    match PartitionValRefTypars g ovByMethValRef with
    | Some(_,ctps,_,_,_) -> 
        let parentToMemberInst,_ = mkTyparToTyparRenaming (ovByMethValRef.MemberApparentParent.Typars(m)) ctps
        let res = instSlotSig parentToMemberInst slotsig
        if verbose then dprintf "adjust slot %s, #parentToMemberInst = %d, before = %s, after = %s\n" (Layout.showL (valRefL ovByMethValRef)) (List.length parentToMemberInst) (Layout.showL(slotSigL slotsig)) (Layout.showL(slotSigL res));
        res
    | None -> 
        (* Note: it appears PartitionValRefTypars should never return 'None' *)
        slotsig

type MethInfo with 
    member x.NumArgs = 
        match x with 
        | ILMeth(_g,x,_) -> [x.NumParams]
        | FSMeth(g,_,vref,_) -> ParamsOfMember  g vref |> List.map List.length 
        | DefaultStructCtor _ -> [0]

    member x.IsCurried = x.NumArgs.Length > 1

    /// Does the method appear to the user as an instance method?
    member x.IsInstance = 
        match x with 
        | ILMeth(_,x,_) -> x.IsInstance
        | FSMeth(_,_,vref,_) -> vref.IsInstanceMember
        | DefaultStructCtor _ -> false


    member x.GenericArity = 
        match x with 
        | ILMeth(_g,x,_) -> x.GenericArity
        | FSMeth(g,typ,vref,_) -> 
            let _,mtps,_,_ = AnalyzeTypeOfMemberVal g (typ,vref)
            mtps.Length
        | DefaultStructCtor _ -> 0

    member x.IsProtectedAccessiblity = 
        match x with 
        | ILMeth(_g,x,_) -> x.IsProtectedAccessibility
        | FSMeth _ -> false
        | DefaultStructCtor _ -> false

    member x.IsVirtual =
        match x with 
        | ILMeth(_,x,_) -> x.IsVirtual
        | FSMeth(_,_,vref,_) -> MemberRefIsVirtual vref
        | DefaultStructCtor _ -> false

    member x.IsConstructor = 
        match x with 
        | ILMeth(_g,x,_) -> x.IsConstructor
        | FSMeth(_g,_,vref,_) ->
            let flags = (the (vref.MemberInfo)).MemberFlags
            (flags.MemberKind = MemberKind.Constructor)
        | DefaultStructCtor _ -> true

    member x.IsClassConstructor =
        match x with 
        | ILMeth(_g,x,_) -> x.IsClassConstructor
        | FSMeth _ -> false
        | DefaultStructCtor _ -> false

    member meth.IsDispatchSlot = 
        match meth with 
        | ILMeth(_g,x,_) -> 
            x.IsVirtual
        | FSMeth(g,_,vref,_) as x -> 
            isInterfaceTy g x.EnclosingType  || 
            (let membInfo = (the (vref.MemberInfo))
             membInfo.MemberFlags.IsDispatchSlot)
        | DefaultStructCtor _ -> false


    member x.IsFinal = 
        not x.IsVirtual || 
        match x with 
        | ILMeth(_g,x,_) -> x.IsFinal
        | FSMeth(_g,_,_vref,_) -> false
        | DefaultStructCtor _ -> true

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

    member x.IsNewSlot = 
        isInterfaceTy x.TcGlobals x.EnclosingType  || 
        (x.IsVirtual && 
          (match x with 
           | ILMeth(_,x,_) -> x.IsNewSlot
           | FSMeth(_,_,vref,_) -> MemberRefIsDispatchSlot vref
           | DefaultStructCtor _ -> false))


    member x.IsDefiniteFSharpOverride = 
        match x with 
        | ILMeth(_,_x,_) -> false
        | FSMeth(_,_,vref,_) -> MemberRefIsDefiniteFSharpOverride vref
        | DefaultStructCtor _ -> false

    member x.IsExtensionMember = 
        match x with 
        | ILMeth(_,x,_) -> x.ExtensionMethodInfo.IsSome
        | FSMeth(_,_,vref,_) -> vref.IsExtensionMember
        | DefaultStructCtor _ -> false

    member x.IsFSharpEventProperty = 
        match x with 
        | FSMeth(g,_,vref,_)  -> vref.IsFSharpEventProperty(g)
        | _ -> false

    member x.IsNullary = (x.NumArgs = [0])

    member x.IsStruct = 
        x.EnclosingType|> isStructTy x.TcGlobals


/// Type-qualified static property accessors for properties commonly used as first-class values
///
/// Cleanup: this is not good F# style, remove it
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MethInfo =
    let IsVirtual(m:MethInfo) = m.IsVirtual
    let IsNewSlot(m:MethInfo) = m.IsNewSlot
    let IsDefiniteFSharpOverride(m:MethInfo) = m.IsDefiniteFSharpOverride
    let LogicalName(m:MethInfo) = m.LogicalName


type PropInfo with 
    member x.PropertyName = 
        match x with 
        | ILProp(_,x) -> x.PropertyName
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.PropertyName
        | FSProp _ -> failwith "unreachable"

    member x.GetterMethod = 
        match x with
        | ILProp(g,x) -> ILMeth(g,x.GetterMethod,None)
        | FSProp(g,typ,Some vref,_) -> FSMeth(g,typ,vref,None) 
        | FSProp _ -> failwith "no getter method"

    member x.SetterMethod = 
        match x with
        | ILProp(g,x) -> ILMeth(g,x.SetterMethod,None)
        | FSProp(g,typ,_,Some vref) -> FSMeth(g,typ,vref,None)
        | FSProp _ -> failwith "no setter method"

    member x.HasGetter = 
        match x with
        | ILProp(_,x) -> x.HasGetter
        | FSProp(_,_,x,_) -> isSome x 

    member x.HasSetter = 
        match x with
        | ILProp(_,x) -> x.HasSetter
        | FSProp(_,_,_,x) -> isSome x 

    member x.EnclosingType = 
        match x with 
        | ILProp(_,x) -> x.ILTypeInfo.ToType
        | FSProp(_,typ,_,_) -> typ


    /// True if the getter (or, if absent, the setter) is a virtual method
    member x.IsVirtualProperty = 
        match x with 
        | ILProp(_,x) -> x.IsVirtual
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> MemberRefIsVirtual vref
        | FSProp _-> failwith "unreachable"

    
    member x.IsNewSlot = 
        match x with 
        | ILProp(_,x) -> x.IsNewSlot
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> MemberRefIsDispatchSlot vref
        | FSProp(_,_,None,None) -> failwith "unreachable"


    /// True if the getter (or, if absent, the setter) for the property is a dispatch slot
    member x.IsDispatchSlot = 
        match x with 
        | ILProp(_,x) -> x.IsVirtual

        | FSProp(g,typ,Some vref,_) 
        | FSProp(g,typ,_, Some vref) ->
            isInterfaceTy g typ  || 
            (let membInfo = (the (vref.MemberInfo))
             membInfo.MemberFlags.IsDispatchSlot)
        | FSProp _ -> failwith "unreachable"

    member x.IsStatic =
        match x with 
        | ILProp(_,x) -> x.IsStatic
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> not vref.IsInstanceMember
        | FSProp(_,_,None,None) -> failwith "unreachable"

    member x.IsDefiniteFSharpOverride = 
        match x with 
        | ILProp _ -> false
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_,Some vref) -> MemberRefIsDefiniteFSharpOverride vref
        | FSProp(_,_,None,None) -> failwith "unreachable"

    member x.IsIndexer = 
        match x with 
        | ILProp(_,ILPropInfo(_,pdef)) -> pdef.Args <> []
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

    member x.IsFSharpEventProperty = 
        match x with 
        | FSProp(g,_,Some vref,None)  -> vref.IsFSharpEventProperty(g)
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
        | ILProp(_,_) -> XmlDoc.Empty
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> vref.XmlDoc
        | FSProp(_,_,None,None) -> failwith "unreachable"

    member x.IsValueType =
        match x with 
        | ILProp(g,_) -> x.EnclosingType |> isStructTy g 
        | FSProp(g,_,_,_) -> x.EnclosingType |> isStructTy g

    member x.ArbitraryValRef = 
        match x with 
        | ILProp(_,_) -> None
        | FSProp(_,_,Some vref,_) 
        | FSProp(_,_,_, Some vref) -> Some(vref)
        | FSProp(_,_,None,None) -> failwith "unreachable"

    member x.ParamNamesAndTypes(amap,m) = 
        match x with 
        | ILProp (_,ilpinfo) -> ilpinfo.ParamNamesAndTypes(amap,m)
        | FSProp (g,typ,Some vref,_) 
        | FSProp (g,typ,_,Some vref) -> 
            let ttps,_mtps,_retTy,tinst = AnalyzeTypeOfMemberVal amap.g (typ,vref)
            let inst = mkTyparInst ttps tinst
            ArgInfosOfPropertyVal g vref.Deref |> List.map (ParamOfArgInfo >> InstParam inst)
        | FSProp _ -> failwith "unreachable"
     
    member x.PropertyType (amap,m) = 
        match x with
        | ILProp (_,ilpinfo) -> ilpinfo.PropertyType (amap,m)
        | FSProp (g,typ,Some vref,_) 
        | FSProp (g,typ,_,Some vref) -> 
            let ttps,_mtps,_retTy,tinst = AnalyzeTypeOfMemberVal amap.g (typ,vref)
            let inst = mkTyparInst ttps tinst
            ReturnTypeOfPropertyVal g vref.Deref
            |> instType inst
            
        | FSProp _ -> failwith "unreachable"


    member x.ParamTypes(amap,m) = 
      x.ParamNamesAndTypes(amap,m) |> List.map snd


/// Type-qualified static property accessors for properties commonly used as first-class values
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PropInfo = 
    let HasGetter(m:PropInfo) = m.HasGetter
    let IsVirtualProperty(m:PropInfo) = m.IsVirtualProperty
    let IsDefiniteFSharpOverride(m:PropInfo) = m.IsDefiniteFSharpOverride
    let IsNewSlot(m:PropInfo) = m.IsNewSlot
    let HasSetter(m:PropInfo) = m.HasSetter
    let EnclosingType(m:PropInfo) = m.EnclosingType
    let PropertyName(m:PropInfo) = m.PropertyName


/// Used to hide/filter members from super classes based on signature *)
let PropInfosEquivByNameAndPartialSig erasureFlag g amap m (pinfo:PropInfo) (pinfo2:PropInfo) = 
    pinfo.PropertyName = pinfo2.PropertyName &&
    let argtys = pinfo.ParamTypes(amap,m)
    let argtys2 = pinfo2.ParamTypes(amap,m)
    List.lengthsEqAndForall2 (typeEquivAux erasureFlag g) argtys argtys2 
  
//-------------------------------------------------------------------------
// events
//------------------------------------------------------------------------- 

exception BadEventTransformation of range

/// Properties compatible with type IDelegateEvent and atributed with CLIEvent are special: we generate metadata and add/remove methods 
/// to make them into a .NET event, and mangle the name of a property.  
/// We don't handle static, indexer or abstract properties correctly. 
/// Note the name mangling doesn't affect the name of the get/set methods for the property 
/// and so doesn't affect how we compile F# accesses to the property. 
let TypConformsToIDelegateEvent g ty = 
   isIDelegateEventType g ty && isDelegateTy g (destIDelegateEventType g ty) 
   

/// Create an error object to raise should an event not have the shape expected by the .NET idiom described further below 
let nonStandardEventError nm m = 
    Error ((FSComp.SR.eventHasNonStandardType(nm,("add_"^nm),("remove_"^nm))),m)

let FindDelegateTypeOfPropertyEvent g amap nm m ty =
    match SearchEntireHierarchyOfType (TypConformsToIDelegateEvent g) g amap m ty with
    | None -> error(nonStandardEventError nm m)
    | Some ty -> destIDelegateEventType g ty

type EventInfo with
    member x.EventName = 
        match x with 
        | ILEvent(_,e) -> e.Name 
        | FSEvent (_,p,_,_) -> p.PropertyName

    member x.IsStatic = 
        match x with 
        | ILEvent(_,e) -> e.IsStatic 
        | FSEvent (_,p,_,_) -> p.IsStatic

    member x.GetDelegateType(amap,m) = 
        match x with 
        | ILEvent(_,e) -> 
            if isNone e.RawMetadata.Type then error (nonStandardEventError x.EventName m);

            DelegateTypeOfILEventInfo amap m e
        | FSEvent(g,p,_,_) -> 
            FindDelegateTypeOfPropertyEvent g amap x.EventName m (p.PropertyType(amap,m))
        
    member x.IsValueType = 
        match x with 
        | ILEvent(_,e) -> e.ILTypeInfo.IsValueType 
        | FSEvent (_,p,_,_) -> p.IsValueType

    member x.GetAddMethod(m) = 
        match x with 
        | ILEvent(g,e) -> ILMeth(g,e.AddMethod,None)
        | FSEvent(g,p,addValRef,_) -> FSMeth(g,p.EnclosingType,addValRef,None)

    member x.GetRemoveMethod(m) = 
        match x with 
        | ILEvent(g,e) -> ILMeth(g,e.RemoveMethod,None)
        | FSEvent(g,p,_,removeValRef) -> FSMeth(g,p.EnclosingType,removeValRef,None)
    


type ParamData = ParamData of bool * bool * OptionalArgInfo * string option * TType

let ParamDatasOfMethInfo amap m minfo minst = 
    let paramInfos = 
        match minfo with 
        | ILMeth(_g,ilminfo,_) -> 
            [ ilminfo.ParamInfos(amap,m,minst)  ]
        | FSMeth(g,typ,vref,_) -> 
            let ttps,mtps,_,tinst = AnalyzeTypeOfMemberVal g (typ,vref)
            let paramTypes = ParamsOfMember g vref
            let inst = (CombineMethInsts ttps mtps tinst minst)
            paramTypes |> InstParams inst 
        | DefaultStructCtor _ -> 
            [[]]

    let paramAttribs = ParamAttribsOfMethInfo amap m minfo
    (paramAttribs,paramInfos) ||> List.map2 (List.map2 (fun (isParamArrayArg,isOutArg,optArgInfo) (nmOpt,pty)-> 
         ParamData(isParamArrayArg,isOutArg,optArgInfo,nmOpt,pty)))


//-------------------------------------------------------------------------
// Printing
//------------------------------------------------------------------------- 

let FormatMethArgToBuffer denv os (ParamData(_isParamArrayArg,_isOutArg,optArgInfo,nmOpt,pty)) =
    let isOptArg = optArgInfo.IsOptional
    match nmOpt, isOptArg, tryDestOptionTy denv.g pty with 
    // Layout an optional argument 
    | Some(nm), true, Some(pty) -> 
        bprintf os "?%s: " nm 
        NicePrint.outputTy denv os pty
    // Layout an unnamed argument 
    | None, _,_ -> 
        NicePrint.outputTy denv os pty;
    // Layout a named argument 
    | Some nm,_,_ -> 
        bprintf os "%s: " nm 
        NicePrint.outputTy denv os pty

let FormatMethInfoToBuffer amap m denv os minfo =
    match minfo with 
    | DefaultStructCtor(g,_typ) -> 
        NicePrint.outputTyconRef denv os (tcrefOfAppTy g minfo.EnclosingType);
        bprintf os "()" 
    | FSMeth(_g,_,vref,_) -> 
        NicePrint.outputQualifiedValSpec denv os vref.Deref
    | ILMeth(g,ilminfo,_) -> 
        // Prettify this baby
        let minfo,minst = 
            let (ILMethInfo(ILTypeInfo(tcref,tref,tinst,tdef),extInfo,mdef,_filmtps)) = ilminfo
            let _,tys,_ = PrettyTypes.PrettifyTypesN g (tinst @ minfo.FormalMethodInst)
            let tinst,minst = List.chop tinst.Length tys
            let minfo = mkILMethInfo amap m (ILTypeInfo(tcref,tref,tinst,tdef)) extInfo None mdef
            minfo,minst
        
        let retTy = FSharpReturnTyOfMeth amap m minfo minst
        NicePrint.outputTyconRef denv os (tcrefOfAppTy g minfo.EnclosingType);
        if minfo.LogicalName = ".ctor" then  
          bprintf os "("
        else
          bprintf os "."
          NicePrint.outputTypars denv minfo.LogicalName os minfo.FormalMethodTypars;
          bprintf os "(" 
        let paramDatas = ParamDatasOfMethInfo amap m minfo minst
        paramDatas |> List.iter (List.iteri (fun i arg -> 
              if i > 0 then bprintf os ", "; 
              FormatMethArgToBuffer denv os arg))
        bprintf os ") : "  
        NicePrint.outputTy denv os retTy


let stringOfMethInfo amap m denv d = bufs (fun buf -> FormatMethInfoToBuffer amap m denv buf d)
let stringOfParamData denv paramData = bufs (fun buf -> FormatMethArgToBuffer denv buf paramData)


//-------------------------------------------------------------------------
// Basic accessibility logic
//------------------------------------------------------------------------- 

/// What keys do we have to access other constructs? 
[<NoEquality; NoComparison>]
type AccessorDomain = 
    | AccessibleFrom of 
        CompilationPath list * (* we have the keys to access any members private to the given paths *)
        TyconRef option        (* we have the keys to access any protected members of the super types of 'TyconRef' *)
    | AccessibleFromEverywhere
    // An AccessorDomain which returns everything but .NET private/internal items
    // This is used 
    //    - when solving member trait constraints, which are solved independently of accessibility 
    //    - for failure paths in error reporting, e.g. to produce an error that an F# item is not accessible
    //    - an adhoc use in service.fs to look up a delegate signature
    | AccessibleFromSomeFSharpCode 
    | AccessibleFromSomewhere // everything

    // Hashing and comparison is used for the memoization tables keyed by an accessor domain.
    // It is dependent on a TcGlobals because of the TyconRef in the data structure
    static member CustomGetHashCode(g:TcGlobals, ad:AccessorDomain) = 
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

    let private  IsAccessible ad taccess = 
        match ad with 
        | AccessibleFromEverywhere -> canAccessFromEverywhere taccess
        | AccessibleFromSomeFSharpCode -> canAccessFromSomewhere taccess
        | AccessibleFromSomewhere -> true
        | AccessibleFrom (cpaths,_tcrefViewedFromOption) -> 
            List.exists (canAccessFrom taccess) cpaths

    let private CheckILMemberAccess g amap m (ILTypeInfo(tcrefOfViewedItem,_,_,_)) ad access = 
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
        isILTypeInfoAccessible ad tinfo && CheckILMemberAccess g amap m tinfo ad access

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

    and IsTypeInstAccessible g ad tinst = 
        match tinst with 
        | [] -> true 
        | _ -> List.forall (IsTypeAccessible g ad) tinst

    let IsILFieldInfoAccessible g amap m ad x = 
        match x with 
        | ILFieldInfo (tinfo,fd) -> isILMemberAccessible g amap m ad tinfo fd.Access

    let IsILEventInfoAccessible g amap m ad (ILEventInfo (tinfo,edef)) =
        let access = (resolveILMethodRef tinfo.RawMetadata edef.AddMethod).Access 
        isILMemberAccessible g amap m ad tinfo access

    let IsILMethInfoAccessible g amap m ad (ILMethInfo (tinfo,_,mdef,_)) =
        isILMemberAccessible g amap m ad tinfo mdef.Access 

    let IsILPropInfoAccessible g amap m ad (ILPropInfo(tinfo,pdef)) =
        isILMemberAccessible g amap m ad tinfo (getILPropDefAccessibility tinfo.RawMetadata pdef)

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


    let IsPropInfoAccessible g amap m ad = function 
        | ILProp (_,x) -> IsILPropInfoAccessible g amap m ad x
        | FSProp (_,_,Some vref,_) 
        | FSProp (_,_,_,Some vref) -> IsValAccessible ad vref
        | _ -> false


open AccessibilityLogic
//-------------------------------------------------------------------------
// Check custom attributes
//------------------------------------------------------------------------- 

exception ObsoleteWarning of string * range
exception ObsoleteError of string * range

module AttributeChecking = 


    let private bindMethInfoAttributes minfo f1 f2 = 
        match minfo with 
        | ILMeth (_,x,_) -> f1 x.RawMetadata.CustomAttrs 
        | FSMeth (_,_,vref,_) -> f2 vref.Attribs
        | DefaultStructCtor _ -> f2 []


    let private checkILAttributes g cattrs m = 
        let (AttribInfo(tref,_)) = g.attrib_SystemObsolete
        match ILThingDecodeILAttrib g tref (Some(tref.Scope)) cattrs with 
        | Some ([ILAttribElem.String (Some(msg)) ],_) -> 
             WarnD(ObsoleteWarning(msg,m))
        | Some ([ILAttribElem.String (Some(msg)); ILAttribElem.Bool isError ],_) -> 
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

    let TryBindMethInfoAttribute g (AttribInfo(atref,_) as attribSpec) minfo f1 f2 = 
        bindMethInfoAttributes minfo 
            (fun ilAttribs -> ILThingDecodeILAttrib g atref (Some(atref.Scope)) ilAttribs |> Option.bind f1)
            (fun fsAttribs -> TryFindAttrib g attribSpec fsAttribs |> Option.bind f2)

      
    let CheckFSharpAttributes g attribs m = 
        if isNil attribs then CompleteD 
        else 
            (match TryFindAttrib g g.attrib_SystemObsolete attribs with
            | Some(Attrib(_,_,[ AttribStringArg(s) ],_,_,_)) ->
                WarnD(ObsoleteWarning(s,m))
            | Some(Attrib(_,_,[ AttribStringArg(s); AttribBoolArg(isError) ],_,_,_)) -> 
                if isError then 
                    ErrorD (ObsoleteError(s,m))
                else 
                    WarnD (ObsoleteWarning(s,m))
            | Some _ -> 
                WarnD(ObsoleteWarning("", m))
            | None -> 
                CompleteD
            ) ++ (fun () -> 
            
            match TryFindAttrib g g.attrib_CompilerMessageAttribute attribs with
            | Some(Attrib(_,_,[ AttribStringArg s ; AttribInt32Arg n ],namedArgs,_,_)) -> 
                let msg = UserCompilerMessage(s,n,m)
                let isError = 
                    match namedArgs with 
                    | ExtractAttribNamedArg "IsError" (AttribBoolArg v) -> v 
                    | _ -> false 
                if isError then ErrorD msg else WarnD msg
                 
            | _ -> 
                CompleteD
            ) ++ (fun () -> 
            
            match TryFindAttrib g g.attrib_ExperimentalAttribute attribs with
            | Some(Attrib(_,_,[ AttribStringArg(s) ],_,_,_)) -> 
                WarnD(Experimental(s,m))
            | Some _ -> 
                WarnD(Experimental(FSComp.SR.experimentalConstruct (), m))
            | _ ->  
                CompleteD
            ) ++ (fun () -> 

            match TryFindAttrib g g.attrib_UnverifiableAttribute attribs with
            | Some _ -> 
                WarnD(PossibleUnverifiableCode(m))
            | _ ->  
                CompleteD
            )

    let CheckILAttribsForUnseen g cattrs _m = 
        let (AttribInfo(tref,_)) = g.attrib_SystemObsolete
        isSome (ILThingDecodeILAttrib g tref (Some(tref.Scope)) cattrs)

    let CheckAttribsForUnseen g attribs _m = 
        nonNil attribs && 
        (let isObsolete = isSome (TryFindAttrib g g.attrib_SystemObsolete attribs) 
         let isHidden = 
             (match TryFindAttrib g g.attrib_CompilerMessageAttribute attribs with
              | Some(Attrib(_,_,[AttribStringArg _; AttribInt32Arg messageNumber],
                            ExtractAttribNamedArg "IsHidden" (AttribBoolArg v),_,_)) -> 
                  // Message number 62 is for "ML Compatibility". Items labelled with this are visible in intellisense
                  // when mlCompatibility is set.
                  v && not (messageNumber = 62 && g.mlCompatibility)
              | _ -> false)
         isObsolete || isHidden
        )
      
    let CheckPropInfoAttributes pinfo m = 
        match pinfo with
        | ILProp(g,ILPropInfo(_,pdef)) -> checkILAttributes g pdef.CustomAttrs m
        | FSProp(g,_,Some vref,_) 
        | FSProp(g,_,_,Some vref) -> CheckFSharpAttributes g vref.Attribs m
        | FSProp _ -> failwith "CheckPropInfoAttributes: unreachable"

      
    let CheckILFieldAttributes g (finfo:ILFieldInfo) m = 
        match finfo with 
        | ILFieldInfo(_,pd) -> checkILAttributes g pd.CustomAttrs m |> CommitOperationResult

    let CheckMethInfoAttributes g m tyargsOpt minfo = 
        match bindMethInfoAttributes minfo 
                  (fun ilAttribs -> Some(checkILAttributes g ilAttribs m)) 
                  (fun fsAttribs -> 
                      let res = 
                          CheckFSharpAttributes g fsAttribs m ++ (fun () -> 
                              if isNone tyargsOpt && HasAttrib g g.attrib_RequiresExplicitTypeArgumentsAttribute fsAttribs then
                                 ErrorD(Error(FSComp.SR.tcFunctionRequiresExplicitTypeArguments(minfo.LogicalName),m));
                              else
                                 CompleteD)
                      Some res) with
        | Some res -> res
        | None -> CompleteD (* no attribute = no errors *)

    let MethInfoIsUnseen g m minfo = 
        match bindMethInfoAttributes minfo 
                  (fun ilAttribs -> Some(CheckILAttribsForUnseen g ilAttribs m)) 
                  (fun fsAttribs -> Some(CheckAttribsForUnseen g fsAttribs m)) with
        | Some res -> res
        | None -> false

    let PropInfoIsUnseen m pinfo = 
        match pinfo with
        | ILProp (g,ILPropInfo(_,pdef)) -> CheckILAttribsForUnseen g pdef.CustomAttrs m
        | FSProp (g,_,Some vref,_) 
        | FSProp (g,_,_,Some vref) -> CheckAttribsForUnseen g vref.Attribs m
        | FSProp _ -> failwith "CheckPropInfoAttributes: unreachable"
     
    let CheckEntityAttributes g (x:TyconRef) m = 
        if x.IsILTycon then 
            let tdef = x.ILTyconRawMetadata
            checkILAttributes g tdef.CustomAttrs m
        else CheckFSharpAttributes g x.Attribs m

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
// Build calls to F# methods
//------------------------------------------------------------------------- 

/// Consume the arguments in chunks and build applications.  This copes with various F# calling signatures
/// all of which ultimately become 'methods'.
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
                if args.Length < arity then error(InternalError("internal error in getting arguments, n = "^string arity^", #args = "^string args.Length,m));
                let tupargs,argst = List.chop arity args
                let tuptys = tupargs |> List.map (tyOfExpr g) 
                (mkTupled g m tupargs tuptys),
                (argst, rangeOfFunTy g fty) )
    if not leftover.IsEmpty then error(InternalError("Unexpected "^string(leftover.Length)^" remaining arguments in method application",m));
    mkApps g ((vexp,vexprty),[],args3,m), 
    retTy
    
let BuildFSharpMethodCall g m (typ,vref:ValRef) valUseFlags minst args =
    let vexp = Expr.Val (vref,valUseFlags,m)
    let vexpty = vref.Type
    let tpsorig,tau =  vref.TypeScheme
    let vtinst = argsOfAppTy g typ @ minst
    if tpsorig.Length <> vtinst.Length then error(InternalError("BuildFSharpMethodCall: unexpected List.length mismatch",m));
    let expr = mkTyAppExpr m (vexp,vexpty) vtinst
    let exprty = instType (mkTyparInst tpsorig vtinst) tau
    BuildFSharpMethodApp g m vref expr exprty args
    
//-------------------------------------------------------------------------
// Sets of methods up the hierarchy, ignoring duplicates by name and sig.
// Used to collect sets of virtual methods, protected methods, protected
// properties etc. 
//------------------------------------------------------------------------- 


let SelectFromMemberVals g optFilter f (tcref:TyconRef) = 
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

let checkFilter optFilter nm = match optFilter with None -> true | Some n2 -> nm = n2

let TrySelectMemberVal g optFilter typ pri _membInfo (vref:ValRef) =
    if checkFilter optFilter vref.LogicalName then 
        Some(FSMeth(g,typ,vref,pri))
    else 
        None

let GetImmediateIntrinsicMethInfosOfType (optFilter,ad) g amap m typ =
    let minfos =
        if isILAppTy g typ then 
            let tinfo = ILTypeInfo.FromType g typ
            let mdefs = tinfo.RawMetadata.Methods
            let mdefs = (match optFilter with None -> mdefs.AsList | Some nm -> mdefs.FindByName nm)
            mdefs |> List.map (mkILMethInfo amap m  tinfo None None) 
        elif not (isAppTy g typ) then []
        else SelectFromMemberVals g optFilter (TrySelectMemberVal g optFilter typ None) (tcrefOfAppTy g typ)
    let minfos = minfos |> List.filter (IsMethInfoAccessible amap m ad)
    minfos

/// Join up getters and setters which are not associated in the F# data structure 
type PropertyCollector(g,amap,m,typ,optFilter,ad) = 

    let hashIdentity = 
        Microsoft.FSharp.Collections.HashIdentity.FromFunctions 
            (PropInfo.PropertyName >> hash) 
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

let GetImmediateIntrinsicPropInfosOfType (optFilter,ad) g amap m typ =
    let pinfos =
        if isILAppTy g typ then 
            let tinfo = ILTypeInfo.FromType g typ
            let pdefs = tinfo.RawMetadata.Properties
            let pdefs = match optFilter with None -> pdefs.AsList | Some nm -> pdefs.LookupByName nm
            pdefs |> List.map (fun pd -> ILProp(g,ILPropInfo(tinfo,pd))) 
        elif not (isAppTy g typ) then []
        else
                let propCollector = new PropertyCollector(g,amap,m,typ,optFilter,ad)
                SelectFromMemberVals g None
                           (fun membInfo vref -> propCollector.Collect(membInfo,vref); None)
                           (tcrefOfAppTy g typ) |> ignore
                propCollector.Close()
         
    let pinfos = pinfos |> List.filter (IsPropInfoAccessible g amap m ad)
    pinfos




//---------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

type HierarchyItem = 
    | MethodItem of MethInfo list list
    | PropertyItem of PropInfo list list
    | RecdFieldItem of RecdFieldInfo
    | EventItem of EventInfo list
    | ILFieldItem of ILFieldInfo list

/// An InfoReader is an object to help us read and cache infos. 
/// We create one of these for each file we typecheck. 
type InfoReader(g:TcGlobals, amap:Import.ImportMap) =

    let getImmediateIntrinsicILFieldsOfType (optFilter,ad) m typ =
        let infos =
            if isILAppTy g typ then 
                let tinfo = ILTypeInfo.FromType g typ
                let fdefs = tinfo.RawMetadata.Fields
                let fdefs = match optFilter with None -> fdefs.AsList | Some(nm) -> fdefs.LookupByName nm
                List.map (fun pd -> ILFieldInfo(tinfo,pd)) fdefs
            elif not (isAppTy g typ) then []
            else []
        let infos = infos |> List.filter (IsILFieldInfoAccessible g amap m  ad)
        infos           

    let getImmediateIntrinsicEventsOfType (optFilter,ad) m typ =
        let infos =
            if isILAppTy g typ then 
                let tinfo = ILTypeInfo.FromType g typ
                let edefs = tinfo.RawMetadata.Events
                let edefs = match optFilter with None -> edefs.AsList | Some(nm) -> edefs.LookupByName nm
                [ for edef in edefs   do
                    let einfo = ILEventInfo(tinfo,edef)
                    if IsILEventInfoAccessible g amap m ad einfo then 
                        yield ILEvent(g,einfo) ]
            elif not (isAppTy g typ) then []
            else []
        infos 


    let mkRecdFieldInfo g typ tcref fspec = 
        RecdFieldInfo(argsOfAppTy g typ,mkNestedRecdFieldRef tcref fspec)

    let getImmediateIntrinsicRecdFieldsOfType nm typ =
        match tryDestAppTy g typ with 
        | None -> None
        | Some tcref -> 
           // Note: hidden fields are not allowed in lookups here, as we're only looking 
           // up user-visible fields in name resolution. 
           match tcref.GetFieldByName nm with
           | Some rfield when not rfield.IsCompilerGenerated -> Some (mkRecdFieldInfo g typ tcref rfield)
           | _ -> None


    /// The primitive reader for the method info sets up a hierarchy
    let readRawIntrinsicMethodSetsUncached ((optFilter,ad,allowMultiIntfInst),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> GetImmediateIntrinsicMethInfosOfType (optFilter,ad) g amap m typ :: acc) g amap m allowMultiIntfInst typ []

    /// The primitive reader for the property info sets up a hierarchy
    let readRawIntrinsicPropertySetsUncached ((optFilter,ad,allowMultiIntfInst),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> GetImmediateIntrinsicPropInfosOfType (optFilter,ad) g amap m typ :: acc) g amap m allowMultiIntfInst typ []

    let readIlFieldInfosUncached ((optFilter,ad),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> getImmediateIntrinsicILFieldsOfType (optFilter,ad) m typ @ acc) g amap m FirstIntfInst typ []

    let readEventInfosUncached ((optFilter,ad),m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> getImmediateIntrinsicEventsOfType (optFilter,ad) m typ @ acc) g amap m FirstIntfInst typ []

    let findRecdFieldInfoUncached (nm,m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> match getImmediateIntrinsicRecdFieldsOfType nm typ with None -> acc | Some v -> Some v) g amap m FirstIntfInst typ None
    
    let readEntireTypeHierachyUncached (allowMultiIntfInst,m,typ) =
        FoldEntireHierarchyOfType (fun typ acc -> typ :: acc) g amap m allowMultiIntfInst typ  [] 

    let readPrimaryTypeHierachyUncached (allowMultiIntfInst,m,typ) =
        FoldPrimaryHierarchyOfType (fun typ acc -> typ :: acc) g amap m allowMultiIntfInst typ [] 

    /// The primitive reader for the named items up a hierarchy
    let readRawIntrinsicNamedItemsUncached ((nm,ad),m,typ) =
        let optFilter = Some nm
        FoldPrimaryHierarchyOfType (fun typ acc -> 
             let minfos = GetImmediateIntrinsicMethInfosOfType (optFilter,ad) g amap m typ
             let pinfos = GetImmediateIntrinsicPropInfosOfType (optFilter,ad) g amap m typ 
             let finfos = getImmediateIntrinsicILFieldsOfType (optFilter,ad) m typ 
             let einfos = getImmediateIntrinsicEventsOfType (optFilter,ad) m typ 
             let rfinfos = getImmediateIntrinsicRecdFieldsOfType nm typ 
             match acc with 
             | Some(MethodItem(inheritedMethSets)) when nonNil minfos -> Some(MethodItem (minfos::inheritedMethSets))
             | _ when nonNil minfos -> Some(MethodItem ([minfos]))
             | Some(PropertyItem(inheritedPropSets)) when nonNil pinfos -> Some(PropertyItem(pinfos::inheritedPropSets))
             | _ when nonNil pinfos -> Some(PropertyItem([pinfos]))
             | _ when nonNil finfos -> Some(ILFieldItem(finfos))
             | _ when nonNil einfos -> Some(EventItem(einfos))
             | _ when isSome rfinfos -> Some(RecdFieldItem(rfinfos.Value))
             | _ -> acc)
          g amap m 
          FirstIntfInst
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
               member x.GetHashCode((filter: string option, ad: AccessorDomain, _allowMultiIntfInst1)) = hash filter + AccessorDomain.CustomGetHashCode(g,ad)
               member x.Equals((filter1, ad1, allowMultiIntfInst1), (filter2,ad2, allowMultiIntfInst2)) = 
                   (filter1 = filter2) && AccessorDomain.CustomEquals(g,ad1,ad2) && allowMultiIntfInst1 = allowMultiIntfInst2 }

    let hashFlags1 = 
        { new System.Collections.Generic.IEqualityComparer<_> with 
               member x.GetHashCode((filter: string option,ad: AccessorDomain)) = hash filter + AccessorDomain.CustomGetHashCode(g,ad)
               member x.Equals((filter1,ad1), (filter2,ad2)) = (filter1 = filter2) && AccessorDomain.CustomEquals(g,ad1,ad2) }

    let hashFlags2 = 
        { new System.Collections.Generic.IEqualityComparer<_> with 
               member x.GetHashCode((nm: string,ad: AccessorDomain)) = hash nm + AccessorDomain.CustomGetHashCode(g,ad)
               member x.Equals((nm1,ad1), (nm2,ad2)) = (nm1 = nm2) && AccessorDomain.CustomEquals(g,ad1,ad2) }
                         
    let methodInfoCache = makeInfoCache g readRawIntrinsicMethodSetsUncached hashFlags0
    let propertyInfoCache = makeInfoCache g readRawIntrinsicPropertySetsUncached hashFlags0
    let ilFieldInfoCache = makeInfoCache g readIlFieldInfosUncached hashFlags1
    let eventInfoCache = makeInfoCache g readEventInfosUncached hashFlags1
    let namedItemsCache = makeInfoCache g readRawIntrinsicNamedItemsUncached hashFlags2

    let recdFieldInfoCache = makeInfoCache g findRecdFieldInfoUncached HashIdentity.Structural
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

    member x.GetILFieldInfosOfType (optFilter,ad,m,typ) =
        ilFieldInfoCache.Apply(((optFilter,ad),m,typ))

    member x.GetEventInfosOfType (optFilter,ad,m,typ) =
        eventInfoCache.Apply(((optFilter,ad),m,typ))

    member x.TryFindRecdFieldInfoOfType (nm,m,typ) =
        recdFieldInfoCache.Apply((nm,m,typ))

    member x.TryFindNamedItemOfType (nm,ad,m,typ) =
        namedItemsCache.Apply(((nm,ad),m,typ))

    member x.ReadEntireTypeHierachy (allowMultiIntfInst,m,typ) =
        entireTypeHierarchyCache.Apply((allowMultiIntfInst,m,typ))

    member x.ReadPrimaryTypeHierachy (allowMultiIntfInst,m,typ) =
        primaryTypeHierarchyCache.Apply((allowMultiIntfInst,m,typ))


//-------------------------------------------------------------------------
// Constructor infos
//------------------------------------------------------------------------- 


let private ConstructorInfosOfILType g amap m typ = 
    let tinfo = ILTypeInfo.FromType g typ 
    tinfo.RawMetadata.Methods.FindByName ".ctor" 
    |> List.filter (fun md -> match md.mdKind with MethodKind.Ctor -> true | _ -> false) 
    |> List.map (mkILMethInfo amap m tinfo None None) 
    
let GetIntrinsicConstructorInfosOfType (infoReader:InfoReader) m ty = 
    let g = infoReader.g
    let amap = infoReader.amap 
    if verbose then   dprintf "--> GetIntrinsicConstructorInfosOfType\n"; 
    if isAppTy g ty then
            if isILAppTy g ty then 
                ConstructorInfosOfILType g amap m ty
            else 
                let tcref = tcrefOfAppTy g ty
                let nm = ".ctor"
                let _aug = tcref.TypeContents
                // Possible cleanup: his should select from all accessible/available vrefs 
                // that are part of any augmentation of this type. That's assuming that constructors can 
                // be in augmentations. 
                let vrefs = NameMultiMap.find nm tcref.MembersOfFSharpTyconByName
                vrefs 
                |> List.choose(fun vref -> 
                    match vref.MemberInfo with 
                    | Some membInfo when (membInfo.MemberFlags.MemberKind = MemberKind.Constructor) -> Some(vref) 
                    | _ -> None) 
                |> List.map (fun x -> FSMeth(g,ty,x,None)) 
    else []
    

//-------------------------------------------------------------------------
// Method signatures
//------------------------------------------------------------------------- 

let FormalTyparsOfEnclosingTypeOfMethInfo m minfo = 
    match minfo with 
    | ILMeth(_g,ilminfo,_) -> 
        // For extension methods all type variables are on the method
        if ilminfo.IsCSharpExtensionMethod then 
            [] 
        else
             ilminfo.ILTypeInfo.FormalTypars m
    | FSMeth(g,typ,vref,_) -> 
        let ttps,_,_,_ = AnalyzeTypeOfMemberVal g (typ,vref)
        ttps
    | DefaultStructCtor(g,typ) -> 
        (tcrefOfAppTy g typ).Typars(m)

let CompiledSigOfMeth g amap m (minfo:MethInfo) = 
    let fmtps = minfo.FormalMethodTypars
    let fminst = generalizeTypars fmtps
    let vargtys = ParamTypesOfMethInfo amap m minfo fminst
    let vrty = CompiledReturnTyOfMeth amap m minfo fminst

    // The formal method typars returned are completely formal - they don't take into account the instantiation 
    // of the enclosing type. For example, they may have constraints involving the _formal_ type parameters 
    // of the enclosing type. This instaniations can be used to interpret those type parameters 
    let fmtpinst = 
        let tinst = argsOfAppTy g minfo.EnclosingType
        let ttps  = FormalTyparsOfEnclosingTypeOfMethInfo m minfo
        mkTyparInst ttps tinst
            
    vargtys,vrty,fmtps,fmtpinst

/// Used to hide/filter members from super classes based on signature 
let MethInfosEquivByNameAndPartialSig erasureFlag g amap m (minfo:MethInfo) (minfo2:MethInfo) = 
    (minfo.LogicalName = minfo2.LogicalName) &&
    (minfo.GenericArity = minfo2.GenericArity) &&
    let fmtps = minfo.FormalMethodTypars
    let fminst = generalizeTypars fmtps
    let fmtps2 = minfo2.FormalMethodTypars
    let fminst2 = generalizeTypars fmtps2
    let argtys = ParamTypesOfMethInfo amap m minfo fminst
    let argtys2 = ParamTypesOfMethInfo amap m minfo2 fminst2
    (argtys,argtys2) ||> List.lengthsEqAndForall2 (List.lengthsEqAndForall2 (typeAEquivAux erasureFlag g (TypeEquivEnv.FromEquivTypars fmtps fmtps2)))

/// Used to hide/filter members from super classes based on signature 
let MethInfosEquivByNameAndSig erasureFlag g amap m minfo minfo2 = 
    MethInfosEquivByNameAndPartialSig erasureFlag g amap m minfo minfo2 &&
    let _argtys,retTy,fmtps,_ = CompiledSigOfMeth g amap m minfo
    let _argtys2,retTy2,fmtps2,_ = CompiledSigOfMeth g amap m minfo2
    match retTy,retTy2 with 
    | None,None -> true
    | Some retTy,Some retTy2 -> typeAEquivAux erasureFlag g (TypeEquivEnv.FromEquivTypars fmtps fmtps2) retTy retTy2 
    | _ -> false

/// Used to hide/filter members from super classes based on signature *)
let PropInfosEquivByNameAndSig erasureFlag g amap m (pinfo:PropInfo) (pinfo2:PropInfo) = 
    PropInfosEquivByNameAndPartialSig erasureFlag g amap m pinfo pinfo2 &&
    let retTy = pinfo.PropertyType(amap,m)
    let retTy2 = pinfo2.PropertyType(amap,m) 
    typeEquivAux erasureFlag g retTy retTy2
  
/// nb. Prefer items toward the top of the hierarchy if the items are virtual 
/// but not when resolving base calls. Also get overrides instead 
/// of abstract slots when measuring whether a class/interface implements all its 
/// required slots. 

type FindMemberFlag = 
  | IgnoreOverrides 
  | PreferOverrides

/// The input list is sorted from most-derived to least-derived type, so any System.Object methods 
/// are at the end of the list. Return a filtered list where prior/subsequent members matching by name and 
/// that are in the same equivalence class have been removed. We keep a name-indexed table to 
/// be more efficient when we check to see if we've already seen a particular named method. 
type IndexedList<'a>(itemLists: 'a list list, itemsByName: 'a NameMultiMap) = 
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

let private filterItemsInSubtypesBasedOnItemsInSupertypes nmf keepTest itemLists = 
    let rec loop itemLists = 
        match itemLists with
        | [] -> emptyIndexedList()
        | items :: itemsInSuperTypes -> 
            let ilist = loop itemsInSuperTypes
            let itemsToAdd = excludeItems keepTest nmf items ilist
            ilist.AddItems(itemsToAdd,nmf)
    (loop itemLists).Items

let private filterItemsInSupertypesBasedOnItemsInSubtypes nmf keepTest itemLists  = 
    let rec loop itemLists (indexedItemsInSubTypes:IndexedList<_>) = 
        match itemLists with
        | [] -> List.rev indexedItemsInSubTypes.Items
        | items :: itemsInSuperTypes -> 
            let itemsToAdd = items |> List.filter (fun item -> keepTest item (indexedItemsInSubTypes.ItemsWithName(nmf item)))            
            let ilist = indexedItemsInSubTypes.AddItems(itemsToAdd,nmf)
            loop itemsInSuperTypes ilist

    loop itemLists (emptyIndexedList())

let private excludeItemsInSupertypesBasedOnEquivTestWithItemsInSubtypes nmf equivTest itemLists = 
    filterItemsInSupertypesBasedOnItemsInSubtypes nmf (fun item1 items -> not (items |> List.exists (fun item2 -> equivTest item1 item2))) itemLists 

let private filterOverrides findFlag (isVirt:'a->bool,isNewSlot,isDefiniteOverride,equivSigs,nmf:'a->string) items = 
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
        |> excludeItemsInSupertypesBasedOnEquivTestWithItemsInSubtypes nmf equivVirts 
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
          |> filterItemsInSubtypesBasedOnItemsInSupertypes nmf (fun newItem priorItem  ->
                 not (isVirt newItem) || isNewSlot newItem || not (equivVirts newItem priorItem) )

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
          //       // Note: in future we may give a friendly error at this point
          // 
          //   type PD() = 
          //       inherit PC()
          //       override this.M(x:int) = ()

          |> filterItemsInSupertypesBasedOnItemsInSubtypes nmf (fun item1 superTypeItems -> 
                  not (isNewSlot item1 && 
                       superTypeItems |> List.exists (equivNewSlots item1) &&
                       superTypeItems |> List.exists (fun item2 -> isDefiniteOverride item1 && equivVirts item1 item2))) 

    
let FilterOverridesOfMethInfos findFlag g amap m minfos = 
    filterOverrides findFlag (MethInfo.IsVirtual,MethInfo.IsNewSlot,MethInfo.IsDefiniteFSharpOverride,MethInfosEquivByNameAndSig EraseNone g amap m,MethInfo.LogicalName) minfos

let FilterOverridesOfPropInfos findFlag g amap m props = 
    filterOverrides findFlag (PropInfo.IsVirtualProperty,PropInfo.IsNewSlot,PropInfo.IsDefiniteFSharpOverride,PropInfosEquivByNameAndSig EraseNone g amap m, PropInfo.PropertyName) props

let ExcludeHiddenOfMethInfos g amap m (minfos:MethInfo list list) = 
    minfos
    |> excludeItemsInSupertypesBasedOnEquivTestWithItemsInSubtypes 
        MethInfo.LogicalName
        (fun m1 m2 -> 
             (* only hide those truly from super classes *)
             not (tyconRefEq g (tcrefOfAppTy g m1.EnclosingType) (tcrefOfAppTy g m2.EnclosingType)) &&
             MethInfosEquivByNameAndPartialSig EraseNone g amap m m1 m2)
        
    |> List.concat

let ExcludeHiddenOfPropInfos g amap m pinfos = 
    pinfos 
    |> excludeItemsInSupertypesBasedOnEquivTestWithItemsInSubtypes PropInfo.PropertyName (PropInfosEquivByNameAndPartialSig EraseNone g amap m) 
    |> List.concat

let GetIntrinsicMethInfoSetsOfType (infoReader:InfoReader) (optFilter,ad,allowMultiIntfInst) findFlag m typ = 
    infoReader.GetRawIntrinsicMethodSetsOfType(optFilter,ad,allowMultiIntfInst,m,typ)
    |> FilterOverridesOfMethInfos findFlag infoReader.g infoReader.amap m
  
let GetIntrinsicPropInfoSetsOfType (infoReader:InfoReader) (optFilter,ad,allowMultiIntfInst) findFlag m typ = 
    infoReader.GetRawIntrinsicPropertySetsOfType(optFilter,ad,allowMultiIntfInst,m,typ) 
    |> FilterOverridesOfPropInfos findFlag infoReader.g infoReader.amap m

let GetIntrinsicMethInfosOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ = 
    GetIntrinsicMethInfoSetsOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ |> List.concat
  
let GetIntrinsicPropInfosOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ = 
    GetIntrinsicPropInfoSetsOfType infoReader (optFilter,ad,allowMultiIntfInst)  findFlag m typ  |> List.concat

let TryFindIntrinsicNamedItemOfType (infoReader:InfoReader) (nm,ad) findFlag m typ = 
    match infoReader.TryFindNamedItemOfType(nm,ad, m,typ) with
    | Some item -> 
        match item with 
        | PropertyItem psets -> Some(PropertyItem (psets |> FilterOverridesOfPropInfos findFlag infoReader.g infoReader.amap m))
        | MethodItem msets -> Some(MethodItem (msets |> FilterOverridesOfMethInfos findFlag infoReader.g infoReader.amap m))
        | _ -> Some(item)
    | None -> None

/// Try to detect the existence of a method on a type 
/// Used for 
///     -- getting the GetEnumerator, get_Current, MoveNext methods for enumerable types 
///     -- getting the Dispose method when resolving the 'use' construct 
///     -- getting the various methods used to desugar the computation expression syntax 
let TryFindIntrinsicMethInfo infoReader m ad nm ty = 
    GetIntrinsicMethInfosOfType infoReader (Some(nm),ad,FirstIntfInst) IgnoreOverrides m ty 

let TryFindPropInfo infoReader m ad nm ty = 
    GetIntrinsicPropInfosOfType infoReader (Some(nm),ad,FirstIntfInst) IgnoreOverrides m ty 


/// Make a call to a method info. Used by the optimizer only to build 
/// calls to the type-directed resolutions of overloaded operators 
let MakeMethInfoCall amap m minfo minst args =
    let valUseFlags = NormalValUse // correct unless if we allow wild trait constraints like "T has a ctor and can be used as a parent class" 
    match minfo with 
    | ILMeth(g,ilminfo,_) -> 
        let direct = not minfo.IsVirtual
        let isProp = false in (* not necessarily correct, but this is only used post-creflect where this flag is irrelevant *)
        BuildILMethInfoCall g amap m isProp ilminfo valUseFlags minst  direct args |> fst
    | FSMeth(g,typ,vref,_) -> 
        BuildFSharpMethodCall g m (typ,vref) valUseFlags minst args |> fst
    | DefaultStructCtor(_,typ) -> 
       mkDefault (m,typ)

/// Given a delegate type work out the minfo, argument types, return type 
/// and F# function type by looking at the Invoke signature of the delegate. 
let GetSigOfFunctionForDelegate (infoReader:InfoReader) delty m ad =
    let g = infoReader.g
    let amap = infoReader.amap
    let minfo = 
        match GetIntrinsicMethInfosOfType infoReader (Some "Invoke",ad,FirstIntfInst) IgnoreOverrides m delty with 
        | [h] -> h
        | [] -> error(Error(FSComp.SR.noInvokeMethodsFound (),m))
        | h :: _ -> warning(InternalError(FSComp.SR.moreThanOneInvokeMethodFound (),m)); h
    
    let minst = []   // a delegate's Invoke method is never generic 
    let basicDelArgTys = ParamTypesOfMethInfo amap m minfo minst
    if basicDelArgTys.Length <> 1 then error(Error(FSComp.SR.delegatesNotAllowedToHaveCurriedSignatures (),m))
    let basicDelArgTys = basicDelArgTys.Head
    let delArgTys = if isNil basicDelArgTys then [g.unit_ty] else basicDelArgTys
    let delRetTy = FSharpReturnTyOfMeth amap m minfo minst
        
    CheckMethInfoAttributes g m None minfo |> CommitOperationResult;
    let fty = mkIteratedFunTy delArgTys delRetTy
    minfo,basicDelArgTys,delRetTy,fty

let TryDestStandardDelegateTyp (infoReader:InfoReader) m ad delTy =
    let g = infoReader.g
    let _amap = infoReader.amap
    let _minfo,delArgTys,delRetTy,_ = GetSigOfFunctionForDelegate infoReader delTy m ad
    match delArgTys with 
    | senderTy :: argTys when isObjTy g senderTy  -> Some(mkTupledTy g argTys,delRetTy)
    | _ -> None


(* We take advantage of the following idiom to simplify away the bogus "object" parameter of the 
   of the "Add" methods associated with events.  If you want to access it you
   can use AddHandler instead.
   
   The .NET Framework guidelines indicate that the delegate type used for
   an event should take two parameters, an "object source" parameter
   indicating the source of the event, and an "e" parameter that
   encapsulates any additional information about the event. The type of
   the "e" parameter should derive from the EventArgs class. For events
   that do not use any additional information, the .NET Framework has
   already defined an appropriate delegate type: EventHandler.
   (from http://msdn.microsoft.com/library/default.asp?url=/library/en-us/csref/html/vcwlkEventsTutorial.asp) 
 *)
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
