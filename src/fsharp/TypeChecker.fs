// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// The typechecker.  Left-to-right constrained type checking 
/// with generalization at appropriate points.
module internal Microsoft.FSharp.Compiler.TypeChecker

open System
open System.Collections.Generic

open Internal.Utilities
open Internal.Utilities.Collections

open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library.ResultOrException
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Rational
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.PatternMatchCompilation
open Microsoft.FSharp.Compiler.TcGlobals
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.AccessibilityLogic
open Microsoft.FSharp.Compiler.AttributeChecking
open Microsoft.FSharp.Compiler.TypeRelations
open Microsoft.FSharp.Compiler.MethodCalls
open Microsoft.FSharp.Compiler.MethodOverrides
open Microsoft.FSharp.Compiler.ConstraintSolver
open Microsoft.FSharp.Compiler.NameResolution
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.InfoReader

#if EXTENSIONTYPING
open Microsoft.FSharp.Compiler.ExtensionTyping
#endif

//-------------------------------------------------------------------------
// Helpers that should be elsewhere
//------------------------------------------------------------------------- 

let isThreadOrContextStatic g attrs = 
    HasFSharpAttributeOpt g g.attrib_ThreadStaticAttribute attrs ||
    HasFSharpAttributeOpt g g.attrib_ContextStaticAttribute attrs 

let mkNilListPat (g: TcGlobals) m ty = TPat_unioncase(g.nil_ucref,[ty],[],m)
let mkConsListPat (g: TcGlobals) ty ph pt = TPat_unioncase(g.cons_ucref,[ty],[ph;pt],unionRanges ph.Range pt.Range)

let mkCompGenLetIn m nm ty e f = 
    let v,ve = mkCompGenLocal m nm ty
    mkCompGenLet m v e (f (v,ve))

let mkUnitDelayLambda (g: TcGlobals) m e =
    let uv,_ = mkCompGenLocal m "unitVar" g.unit_ty
    mkLambda m uv (e,tyOfExpr g e) 


//-------------------------------------------------------------------------
// Errors.
//------------------------------------------------------------------------- 

exception BakedInMemberConstraintName of string * range
exception FunctionExpected of DisplayEnv * TType * range
exception NotAFunction of DisplayEnv * TType * range * range
exception Recursion of DisplayEnv * Ident * TType * TType  * range
exception RecursiveUseCheckedAtRuntime of DisplayEnv * ValRef * range
exception LetRecEvaluatedOutOfOrder of DisplayEnv * ValRef * ValRef * range
exception LetRecCheckedAtRuntime of range
exception LetRecUnsound of DisplayEnv * ValRef list * range
exception TyconBadArgs of DisplayEnv * TyconRef * int * range
exception UnionCaseWrongArguments of DisplayEnv * int * int * range
exception UnionCaseWrongNumberOfArgs of DisplayEnv * int * int * range
exception FieldsFromDifferentTypes of DisplayEnv * RecdFieldRef * RecdFieldRef * range
exception FieldGivenTwice of DisplayEnv * Tast.RecdFieldRef * range
exception MissingFields of string list * range
exception FunctionValueUnexpected of DisplayEnv * TType * range
exception UnitTypeExpected of DisplayEnv * TType * range
exception UnitTypeExpectedWithEquality of DisplayEnv * TType * range
exception UnitTypeExpectedWithPossibleAssignment of DisplayEnv * TType * bool * string * range
exception UnitTypeExpectedWithPossiblePropertySetter of DisplayEnv * TType * string * string * range
exception UnionPatternsBindDifferentNames of range
exception VarBoundTwice of Ident
exception ValueRestriction of DisplayEnv * bool * Val * Typar * range
exception FieldNotMutable of DisplayEnv * Tast.RecdFieldRef * range
exception ValNotMutable of DisplayEnv * ValRef * range
exception ValNotLocal of DisplayEnv * ValRef * range
exception InvalidRuntimeCoercion of DisplayEnv * TType * TType * range
exception IndeterminateRuntimeCoercion of DisplayEnv * TType * TType * range
exception IndeterminateStaticCoercion of DisplayEnv * TType * TType * range
exception RuntimeCoercionSourceSealed of DisplayEnv * TType * range
exception CoercionTargetSealed of DisplayEnv * TType * range
exception UpcastUnnecessary of range
exception TypeTestUnnecessary of range
exception StaticCoercionShouldUseBox of DisplayEnv * TType * TType * range
exception SelfRefObjCtor of bool * range
exception VirtualAugmentationOnNullValuedType of range
exception NonVirtualAugmentationOnNullValuedType of range
exception UseOfAddressOfOperator of range
exception DeprecatedThreadStaticBindingWarning of range
exception IntfImplInIntrinsicAugmentation of range
exception IntfImplInExtrinsicAugmentation of range
exception OverrideInIntrinsicAugmentation of range
exception OverrideInExtrinsicAugmentation of range
exception NonUniqueInferredAbstractSlot of TcGlobals * DisplayEnv * string * MethInfo * MethInfo * range
exception StandardOperatorRedefinitionWarning of string * range
exception InvalidInternalsVisibleToAssemblyName of (*badName*)string * (*fileName option*) string option


// Identify any security attributes
let IsSecurityAttribute (g: TcGlobals) amap (casmap : Dictionary<Stamp,bool>) (Attrib(tcref,_,_,_,_,_,_)) m =
    // There's no CAS on Silverlight, so we have to be careful here
    match g.attrib_SecurityAttribute with
    | None -> false
    | Some attr ->
        match attr.TyconRef.TryDeref with
        | VSome _ -> 
           let tcs = tcref.Stamp
           if casmap.ContainsKey(tcs) then
             casmap.[tcs]
           else
             let exists = ExistsInEntireHierarchyOfType (fun t -> typeEquiv g t (mkAppTy attr.TyconRef [])) g amap m AllowMultiIntfInstantiations.Yes (mkAppTy tcref [])
             casmap.[tcs] <- exists
             exists
        | VNone -> false  

let IsSecurityCriticalAttribute g (Attrib(tcref,_,_,_,_,_,_)) =
    (tyconRefEq g tcref g.attrib_SecurityCriticalAttribute.TyconRef || tyconRefEq g tcref g.attrib_SecuritySafeCriticalAttribute.TyconRef)

let RecdFieldInstanceChecks g amap ad m (rfinfo:RecdFieldInfo) = 
    if rfinfo.IsStatic then error (Error (FSComp.SR.tcStaticFieldUsedWhenInstanceFieldExpected(),m))
    CheckRecdFieldInfoAttributes g rfinfo m |> CommitOperationResult        
    CheckRecdFieldInfoAccessible amap m ad rfinfo

let ILFieldInstanceChecks  g amap ad m (finfo :ILFieldInfo) =
    if finfo.IsStatic then error (Error (FSComp.SR.tcStaticFieldUsedWhenInstanceFieldExpected(),m))
    CheckILFieldInfoAccessible g amap m ad finfo
    CheckILFieldAttributes g finfo m

let MethInfoChecks g amap isInstance tyargsOpt objArgs ad m (minfo:MethInfo)  =
    if minfo.IsInstance <> isInstance then
      if isInstance then 
        error (Error (FSComp.SR.csMethodIsNotAnInstanceMethod(minfo.LogicalName),m))
      else        
        error (Error (FSComp.SR.csMethodIsNotAStaticMethod(minfo.LogicalName),m))

    // keep the original accessibility domain to determine type accessibility
    let adOriginal = ad
    // Eliminate the 'protected' portion of the accessibility domain for instance accesses    
    let ad = 
        match objArgs,ad with 
        | [objArg],AccessibleFrom(paths,Some tcref) -> 
            let objArgTy = tyOfExpr g objArg 
            let ty = generalizedTyconRef tcref
            // We get to keep our rights if the type we're in subsumes the object argument type
            if TypeFeasiblySubsumesType 0 g amap m ty CanCoerce objArgTy then
                ad
            // We get to keep our rights if this is a base call
            elif IsBaseCall objArgs then 
                ad
            else
                AccessibleFrom(paths, None) 
        | _ -> ad

    if not (IsTypeAndMethInfoAccessible amap m adOriginal ad minfo) then 
      error (Error (FSComp.SR.tcMethodNotAccessible(minfo.LogicalName),m))
    CheckMethInfoAttributes g m tyargsOpt minfo |> CommitOperationResult


let CheckRecdFieldMutation m denv (rfinfo:RecdFieldInfo) = 
    if not rfinfo.RecdField.IsMutable then error (FieldNotMutable(denv,rfinfo.RecdFieldRef,m))

//-------------------------------------------------------------------------
// Information about object constructors
//------------------------------------------------------------------------- 

type SafeInitData = 
    | SafeInitField of RecdFieldRef * RecdField
    | NoSafeInitInfo 
            
type CtorInfo = 
    { // Object model constructors have a very specific form to satisfy .NET limitations.
      // For "new = \arg. { new C with ... }" 
      //     ctor = 3 indicates about to type check "\arg. (body)", 
      //     ctor = 2 indicates about to type check "body" 
      //     ctor = 1 indicates actually type checking the body expression 
      // 0 indicates everywhere else, including auxiliary expressions such e1 in "let x = e1 in { new ... }" 
      // REVIEW: clean up this rather odd approach ... 
      ctorShapeCounter: int
      /// A handle to the ref cell to hold results of 'this' for 'type X() as x = ...' and 'new() as x = ...' constructs
      /// in case 'x' is used in the arguments to the 'inherits' call.
      safeThisValOpt: Val option 
      /// A handle to the boolean ref cell to hold success of initialized 'this' for 'type X() as x = ...' constructs 
      safeInitInfo: SafeInitData 
      ctorIsImplicit: bool  
    }
    
//-------------------------------------------------------------------------
// Type environments. 
//    - Named items in scope (values)
//    - Record of type variables that can't be generalized
//    - Our 'location' as a concrete compilation path
//    - mutable accumulator for the module type currently being accumulated 
//------------------------------------------------------------------------- 

[<NoEquality; NoComparison>]
type UngeneralizableItem(computeFreeTyvars : (unit -> FreeTyvars)) = 

    // Flag is for: have we determined that this item definitely has 
    // no free type inference variables? This implies that
    //   (a)  it will _never_ have any free type inference variables as further constraints are added to the system.
    //   (b)  its set of FreeTycons will not change as further constraints are added to the system
    let mutable willNeverHaveFreeTypars = false
    // If WillNeverHaveFreeTypars then we can cache the computation of FreeTycons, since they are invariant.
    let mutable cachedFreeLocalTycons = emptyFreeTycons
    // If WillNeverHaveFreeTypars then we can cache the computation of FreeTraitSolutions, since they are invariant.
    let mutable cachedFreeTraitSolutions = emptyFreeLocals

    member item.GetFreeTyvars() = 
        let fvs = computeFreeTyvars()
        if fvs.FreeTypars.IsEmpty then 
            willNeverHaveFreeTypars <- true 
            cachedFreeLocalTycons <- fvs.FreeTycons
            cachedFreeTraitSolutions <- fvs.FreeTraitSolutions
        fvs

    member item.WillNeverHaveFreeTypars = willNeverHaveFreeTypars
    member item.CachedFreeLocalTycons = cachedFreeLocalTycons 
    member item.CachedFreeTraitSolutions = cachedFreeTraitSolutions
 
[<NoEquality; NoComparison>]
type TcEnv =
    { /// Name resolution information 
      eNameResEnv : NameResolutionEnv 

      /// The list of items in the environment that may contain free inference 
      /// variables (which may not be generalized). The relevant types may 
      /// change as a result of inference equations being asserted, hence may need to 
      /// be recomputed. 
      eUngeneralizableItems: UngeneralizableItem list
      
      // Two (!) versions of the current module path 
      // These are used to: 
      //    - Look up the appropriate point in the corresponding signature 
      //      see if an item is public or not 
      //    - Change fslib canonical module type to allow compiler references to these items 
      //    - Record the cpath for concrete modul_specs, tycon_specs and excon_specs so they can cache their generated IL representation where necessary 
      //    - Record the pubpath of public, concrete {val,tycon,modul,excon}_specs.  
      //      This information is used mainly when building non-local references 
      //      to public items. 
      // 
      // Of the two, 'ePath' is the one that's barely used. It's only 
      // used by UpdateAccModuleOrNamespaceType to modify the CCU while compiling FSharp.Core
      ePath: Ident list 
      eCompPath: CompilationPath 
      eAccessPath: CompilationPath 
      /// This field is computed from other fields, but we amortize the cost of computing it.
      eAccessRights: AccessorDomain 

      /// Internals under these should be accessible
      eInternalsVisibleCompPaths: CompilationPath list 

      /// Mutable accumulator for the current module type 
      eModuleOrNamespaceTypeAccumulator: ModuleOrNamespaceType ref

      /// Context information for type checker
      eContextInfo : ContextInfo 

      /// Here Some tcref indicates we can access protected members in all super types 
      eFamilyType: TyconRef option 

      // Information to enforce special restrictions on valid expressions 
      // for .NET constructors. 
      eCtorInfo : CtorInfo option

      eCallerMemberName : string option
    } 
    member tenv.DisplayEnv = tenv.eNameResEnv.DisplayEnv
    member tenv.NameEnv = tenv.eNameResEnv
    member tenv.AccessRights = tenv.eAccessRights

/// Compute the value of this computed, cached field
let computeAccessRights eAccessPath eInternalsVisibleCompPaths eFamilyType = 
    AccessibleFrom (eAccessPath :: eInternalsVisibleCompPaths, eFamilyType) // env.eAccessRights 

let emptyTcEnv g  =
    let cpath = compPathInternal // allow internal access initially
    { eNameResEnv = NameResolutionEnv.Empty g
      eUngeneralizableItems = []
      ePath = []
      eCompPath = cpath // dummy 
      eAccessPath = cpath // dummy 
      eAccessRights = computeAccessRights cpath [] None // compute this field 
      eInternalsVisibleCompPaths = []
      eContextInfo = ContextInfo.NoContext
      eModuleOrNamespaceTypeAccumulator = ref (NewEmptyModuleOrNamespaceType Namespace)
      eFamilyType = None
      eCtorInfo = None
      eCallerMemberName = None }

//-------------------------------------------------------------------------
// Helpers related to determining if we're in a constructor and/or a class
// that may be able to access "protected" members.
//------------------------------------------------------------------------- 

let InitialExplicitCtorInfo (safeThisValOpt, safeInitInfo) =
    { ctorShapeCounter = 3 
      safeThisValOpt = safeThisValOpt
      safeInitInfo = safeInitInfo
      ctorIsImplicit = false} 

let InitialImplicitCtorInfo () =
    { ctorShapeCounter = 0 
      safeThisValOpt = None 
      safeInitInfo = NoSafeInitInfo
      ctorIsImplicit = true }
      
let EnterFamilyRegion tcref env = 
    let eFamilyType = Some tcref
    { env with 
        eAccessRights = computeAccessRights env.eAccessPath env.eInternalsVisibleCompPaths eFamilyType // update this computed field
        eFamilyType = eFamilyType }

let ExitFamilyRegion env = 
    let eFamilyType = None
    match env.eFamilyType with 
    | None -> env  // optimization to avoid reallocation  
    | _ -> 
        { env with 
            eAccessRights = computeAccessRights env.eAccessPath env.eInternalsVisibleCompPaths eFamilyType  // update this computed field
            eFamilyType = eFamilyType }

let AreWithinCtorShape         env = match env.eCtorInfo with None -> false    | Some ctorInfo -> ctorInfo.ctorShapeCounter > 0
let AreWithinImplicitCtor      env = match env.eCtorInfo with None -> false    | Some ctorInfo -> ctorInfo.ctorIsImplicit
let GetCtorShapeCounter        env = match env.eCtorInfo with None -> 0        | Some ctorInfo -> ctorInfo.ctorShapeCounter
let GetRecdInfo                env = match env.eCtorInfo with None -> RecdExpr | Some ctorInfo -> if ctorInfo.ctorShapeCounter = 1 then RecdExprIsObjInit else RecdExpr

let AdjustCtorShapeCounter      f env = {env with eCtorInfo = Option.map (fun ctorInfo -> { ctorInfo with ctorShapeCounter = f ctorInfo.ctorShapeCounter }) env.eCtorInfo }
let ExitCtorShapeRegion           env = AdjustCtorShapeCounter (fun _ -> 0) env

//-------------------------------------------------------------------------
// Add stuff to environments and register things as ungeneralizeable.
//------------------------------------------------------------------------- 

let isEmptyFreeTyvars ftyvs = 
    Zset.isEmpty ftyvs.FreeTypars &&
    Zset.isEmpty ftyvs.FreeTycons 

let addFreeItemOfTy typ eUngeneralizableItems = 
    let fvs = freeInType CollectAllNoCaching typ
    if isEmptyFreeTyvars fvs then eUngeneralizableItems 
    else UngeneralizableItem(fun () -> freeInType CollectAllNoCaching typ) :: eUngeneralizableItems

let rec addFreeInModuleTy (mtyp:ModuleOrNamespaceType) acc =
    QueueList.foldBack (typeOfVal >> accFreeInType CollectAllNoCaching) mtyp.AllValsAndMembers
      (QueueList.foldBack (fun (mspec:ModuleOrNamespace) acc -> addFreeInModuleTy mspec.ModuleOrNamespaceType acc) mtyp.AllEntities acc)
let freeInModuleTy mtyp = addFreeInModuleTy mtyp emptyFreeTyvars

let addFreeItemOfModuleTy mtyp eUngeneralizableItems = 
    let fvs = freeInModuleTy mtyp
    if isEmptyFreeTyvars fvs then eUngeneralizableItems 
    else UngeneralizableItem(fun () -> freeInModuleTy mtyp) :: eUngeneralizableItems

let AddValMapToNameEnv vs nenv = 
    NameMap.foldBackRange (fun v nenv -> AddValRefToNameEnv nenv (mkLocalValRef v)) vs nenv

let AddValListToNameEnv vs nenv = 
    List.foldBack (fun v nenv -> AddValRefToNameEnv nenv (mkLocalValRef v)) vs nenv
    
    
let addInternalsAccessibility env (ccu:CcuThunk) =
    let compPath = CompPath (ccu.ILScopeRef,[])    
    let eInternalsVisibleCompPaths = compPath :: env.eInternalsVisibleCompPaths
    { env with 
        eAccessRights = computeAccessRights env.eAccessPath eInternalsVisibleCompPaths env.eFamilyType // update this computed field
        eInternalsVisibleCompPaths = compPath :: env.eInternalsVisibleCompPaths }

let ModifyNameResEnv f env = { env with eNameResEnv = f env.eNameResEnv } 

let AddLocalValPrimitive (v:Val) env =
    let env = ModifyNameResEnv (fun nenv -> AddValRefToNameEnv nenv (mkLocalValRef v)) env
    { env with eUngeneralizableItems = addFreeItemOfTy v.Type env.eUngeneralizableItems } 


let AddLocalValMap tcSink scopem (vals:Val NameMap) env =
    let env = 
        if vals.IsEmpty then 
            env
        else
            let env = ModifyNameResEnv (AddValMapToNameEnv vals) env
            { env with eUngeneralizableItems = NameMap.foldBackRange (typeOfVal >> addFreeItemOfTy) vals env.eUngeneralizableItems }
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

let AddLocalVals tcSink scopem (vals:Val list) env =
    let env = 
        if isNil vals then 
            env
        else
            let env = ModifyNameResEnv (AddValListToNameEnv vals) env
            { env with eUngeneralizableItems = List.foldBack (typeOfVal >> addFreeItemOfTy) vals env.eUngeneralizableItems }
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

let AddLocalVal tcSink scopem v env = 
    let env = ModifyNameResEnv (fun nenv -> AddValRefToNameEnv nenv (mkLocalValRef v)) env
    let env = {env with eUngeneralizableItems = addFreeItemOfTy v.Type env.eUngeneralizableItems }
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

let AddLocalExnDefnAndReport tcSink scopem env (exnc:Tycon) =
    let env = ModifyNameResEnv (fun nenv -> AddExceptionDeclsToNameEnv BulkAdd.No nenv (mkLocalEntityRef exnc)) env
    (* Also make VisualStudio think there is an identifier in scope at the range of the identifier text of its binding location *)
    CallEnvSink tcSink (exnc.Range,env.NameEnv,env.eAccessRights)
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env
 
let AddLocalTyconRefs ownDefinition g amap m tcrefs env =
    if isNil tcrefs then env else
    env |> ModifyNameResEnv (fun nenv -> AddTyconRefsToNameEnv BulkAdd.No ownDefinition g amap m false nenv tcrefs) 

let AddLocalTycons g amap m (tycons: Tycon list) env =
    if isNil tycons then env else
    env |> AddLocalTyconRefs false g amap m (List.map mkLocalTyconRef tycons) 

let AddLocalTyconsAndReport tcSink scopem g amap m tycons env = 
    let env = AddLocalTycons g amap m tycons env
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

//-------------------------------------------------------------------------
// Open a structure or an IL namespace 
//------------------------------------------------------------------------- 

let OpenModulesOrNamespaces tcSink g amap scopem root env mvvs =
    let env =
        if isNil mvvs then env else
        ModifyNameResEnv (fun nenv -> AddModulesAndNamespacesContentsToNameEnv g amap env.eAccessRights scopem root nenv mvvs) env
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

let AddRootModuleOrNamespaceRefs g amap m env modrefs =
    if isNil modrefs then env else
    ModifyNameResEnv (fun nenv -> AddModuleOrNamespaceRefsToNameEnv g amap m true env.eAccessRights nenv modrefs) env 

let AddNonLocalCcu g amap scopem env assemblyName  (ccu:CcuThunk, internalsVisibleToAttributes)  = 

    let internalsVisible = 
        internalsVisibleToAttributes 
        |> List.exists (fun visibleTo ->             
            try
                System.Reflection.AssemblyName(visibleTo).Name = assemblyName                
            with e ->
                warning(InvalidInternalsVisibleToAssemblyName(visibleTo,ccu.FileName))
                false)

    let env = if internalsVisible then addInternalsAccessibility env ccu else env
    // Compute the top-rooted module or namespace references
    let modrefs = ccu.RootModulesAndNamespaces |> List.map (mkNonLocalCcuRootEntityRef ccu) 
    // Compute the top-rooted type definitions
    let tcrefs = ccu.RootTypeAndExceptionDefinitions |> List.map (mkNonLocalCcuRootEntityRef ccu) 
    let env = AddRootModuleOrNamespaceRefs g amap scopem env modrefs
    let env =
        if isNil tcrefs then env else
        ModifyNameResEnv (fun nenv -> AddTyconRefsToNameEnv BulkAdd.Yes false g amap scopem true nenv tcrefs) env
    //CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

let AddLocalRootModuleOrNamespace tcSink g amap scopem env (mtyp:ModuleOrNamespaceType) = 
    // Compute the top-rooted module or namespace references
    let modrefs = mtyp.ModuleAndNamespaceDefinitions |> List.map mkLocalModRef
    // Compute the top-rooted type definitions
    let tcrefs = mtyp.TypeAndExceptionDefinitions |> List.map mkLocalTyconRef
    let env = AddRootModuleOrNamespaceRefs g amap scopem env modrefs
    let env =
        if isNil tcrefs then env else
        ModifyNameResEnv (fun nenv -> AddTyconRefsToNameEnv BulkAdd.No false g amap scopem true nenv tcrefs) env
    let env = { env with eUngeneralizableItems = addFreeItemOfModuleTy mtyp env.eUngeneralizableItems }
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env

let AddModuleAbbreviationAndReport tcSink scopem id modrefs env =
    let env = 
        if isNil modrefs then env else
        ModifyNameResEnv (fun nenv -> AddModuleAbbrevToNameEnv id nenv modrefs) env
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    let item = Item.ModuleOrNamespaces modrefs
    CallNameResolutionSink tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
    env

let AddLocalSubModule g amap m env (modul:ModuleOrNamespace) =
    let env = ModifyNameResEnv (fun nenv -> AddModuleOrNamespaceRefToNameEnv g amap m false env.eAccessRights nenv (mkLocalModRef modul)) env
    let env = { env with eUngeneralizableItems = addFreeItemOfModuleTy modul.ModuleOrNamespaceType env.eUngeneralizableItems }
    env
 
let AddLocalSubModuleAndReport tcSink scopem g amap m env (modul:ModuleOrNamespace) =
    let env = AddLocalSubModule g amap m env modul 
    CallEnvSink tcSink (scopem,env.NameEnv,env.eAccessRights)
    env
 
let RegisterDeclaredTypars typars env =
    if isNil typars then env else
    { env with eUngeneralizableItems = List.foldBack (mkTyparTy >> addFreeItemOfTy) typars env.eUngeneralizableItems }

let AddDeclaredTypars check typars env =
    if isNil typars then env else
    let env = ModifyNameResEnv (fun nenv -> AddDeclaredTyparsToNameEnv check nenv typars) env
    RegisterDeclaredTypars typars env

/// Compilation environment for typechecking a compilation unit. Contains the
/// F# and .NET modules loaded from disk, the search path, a table indicating
/// how to List.map F# modules to assembly names, and some nasty globals 
/// related to type inference. These are:
///   - all the type variables generated for this compilation unit
///   - the set of active fixups for "letrec" type inference 
[<NoEquality; NoComparison>]
type cenv = 
    { g: TcGlobals

      /// Push an entry every time a recursive value binding is used, 
      /// in order to be able to fix up recursive type applications as 
      /// we infer type parameters 
      mutable recUses: ValMultiMap<(Expr ref * range * bool)>
      
      /// Checks to run after all inference is complete. 
      mutable postInferenceChecks: ResizeArray<unit -> unit>

      /// Are we in a script? if so relax the reporting of discarded-expression warnings at the top level
      isScript: bool 

      /// Environment needed to convert IL types to F# types in the importer. 
      amap: Import.ImportMap 

      /// Used to generate new syntactic argument names in post-parse syntactic processing
      synArgNameGenerator: SynArgNameGenerator

      tcSink: TcResultsSink

      /// Holds a reference to the component being compiled. 
      /// This field is very rarely used (mainly when fixing up forward references to fslib. 
      topCcu: CcuThunk 
      
      /// Holds the current inference constraints 
      css: ConstraintSolverState
      
      /// Are we compiling the signature of a module from fslib? 
      compilingCanonicalFslibModuleType: bool
      isSig: bool
      haveSig: bool
      
      niceNameGen: NiceNameGenerator
      infoReader: InfoReader
      nameResolver: NameResolver
      
      conditionalDefines: string list
            
    } 

    static member Create (g,isScript,niceNameGen,amap,topCcu,isSig,haveSig,conditionalDefines,tcSink, tcVal) =
        let infoReader = new InfoReader(g,amap)
        let instantiationGenerator m tpsorig = ConstraintSolver.FreshenTypars m tpsorig
        let nameResolver = new NameResolver(g,amap,infoReader,instantiationGenerator)
        { g = g
          amap = amap
          recUses = ValMultiMap<_>.Empty
          postInferenceChecks = ResizeArray()
          topCcu = topCcu
          isScript = isScript
          css = ConstraintSolverState.New(g,amap,infoReader,tcVal)
          infoReader = infoReader
          tcSink = tcSink
          nameResolver = nameResolver
          niceNameGen = niceNameGen
          synArgNameGenerator = SynArgNameGenerator()
          isSig = isSig
          haveSig = haveSig
          compilingCanonicalFslibModuleType = (isSig || not haveSig) && g.compilingFslib
          conditionalDefines = conditionalDefines }

let CopyAndFixupTypars m rigid tpsorig = 
    ConstraintSolver.FreshenAndFixupTypars m rigid [] [] tpsorig

let UnifyTypes cenv (env: TcEnv) m expectedTy actualTy = 
    ConstraintSolver.AddCxTypeEqualsType env.eContextInfo env.DisplayEnv cenv.css m (tryNormalizeMeasureInType cenv.g expectedTy) (tryNormalizeMeasureInType cenv.g actualTy)

//-------------------------------------------------------------------------
// Generate references to the module being generated - used for
// public items.
//------------------------------------------------------------------------- 

let MakeInitialEnv env = 
    // Note: here we allocate a new module type accumulator 
    let mtypeAcc = ref (NewEmptyModuleOrNamespaceType Namespace)
    { env with eModuleOrNamespaceTypeAccumulator = mtypeAcc },mtypeAcc

let MakeInnerEnvWithAcc env nm mtypeAcc modKind = 
    let path = env.ePath @ [nm]
    let cpath = env.eCompPath.NestedCompPath nm.idText modKind
    { env with 
        ePath = path 
        eCompPath = cpath
        eAccessPath = cpath
        eAccessRights = computeAccessRights cpath env.eInternalsVisibleCompPaths env.eFamilyType // update this computed field
        eNameResEnv = { env.eNameResEnv with eDisplayEnv = env.DisplayEnv.AddOpenPath (pathOfLid path) }
        eModuleOrNamespaceTypeAccumulator = mtypeAcc }

let MakeInnerEnv env nm modKind = 
    // Note: here we allocate a new module type accumulator 
    let mtypeAcc = ref (NewEmptyModuleOrNamespaceType modKind)
    MakeInnerEnvWithAcc env nm mtypeAcc modKind,mtypeAcc


let MakeInnerEnvForTyconRef _cenv env tcref isExtrinsicExtension = 
    if isExtrinsicExtension then 
        // Extension members don't get access to protected stuff 
        env  
    else
        // Regular members get access to protected stuff 
        let env = EnterFamilyRegion tcref env
        // Note: assumes no nesting 
        let eAccessPath = env.eCompPath.NestedCompPath tcref.LogicalName ModuleOrType
        { env with 
             eAccessRights = computeAccessRights eAccessPath env.eInternalsVisibleCompPaths env.eFamilyType // update this computed field
             eAccessPath = eAccessPath }

let MakeInnerEnvForMember cenv env (v:Val) = 
    match v.MemberInfo with 
    | None -> env
    | Some _ -> MakeInnerEnvForTyconRef cenv env v.MemberApparentParent v.IsExtensionMember 

let GetCurrAccumulatedModuleOrNamespaceType env = !(env.eModuleOrNamespaceTypeAccumulator) 
let SetCurrAccumulatedModuleOrNamespaceType env x =  env.eModuleOrNamespaceTypeAccumulator := x

/// Set up the initial environment 
let LocateEnv ccu env enclosingNamespacePath =
    let cpath = compPathOfCcu ccu
    let env = 
        {env with 
            ePath = []
            eCompPath = cpath 
            eAccessPath = cpath 
            // update this computed field
            eAccessRights = computeAccessRights cpath env.eInternalsVisibleCompPaths env.eFamilyType }
    let env = List.fold (fun env id -> MakeInnerEnv env id Namespace |> fst) env enclosingNamespacePath
    env

let BuildRootModuleType enclosingNamespacePath (cpath:CompilationPath) mtyp = 
    (enclosingNamespacePath,(cpath, (mtyp, None))) 
        ||> List.foldBack (fun id (cpath, (mtyp, mspec)) ->
            let a,b = wrapModuleOrNamespaceTypeInNamespace  id cpath.ParentCompPath mtyp 
            cpath.ParentCompPath, (a, match mspec with Some _ -> mspec | None -> Some b))
        |> snd
        
let BuildRootModuleExpr enclosingNamespacePath (cpath:CompilationPath) mexpr = 
    (enclosingNamespacePath,(cpath, mexpr)) 
        ||> List.foldBack (fun id (cpath, mexpr) -> (cpath.ParentCompPath, wrapModuleOrNamespaceExprInNamespace id cpath.ParentCompPath mexpr))
        |> snd

let TryStripPrefixPath (g:TcGlobals) (enclosingNamespacePath: Ident list) = 
    match enclosingNamespacePath with 
    | p::rest when
        g.isInteractive &&
        not (isNil rest) &&
        p.idText.StartsWith(FsiDynamicModulePrefix,System.StringComparison.Ordinal) && 
        p.idText.[FsiDynamicModulePrefix.Length..] |> String.forall System.Char.IsDigit 
        -> Some(p,rest)
    | _ -> None

let ImplicitlyOpenOwnNamespace tcSink g amap scopem enclosingNamespacePath env = 
    if isNil enclosingNamespacePath then 
        env
    else
        // For F# interactive, skip "FSI_0002" prefixes when determining the path to open implicitly
        let enclosingNamespacePathToOpen = 
            match TryStripPrefixPath g enclosingNamespacePath with 
            | Some(_,rest) -> rest
            | None -> enclosingNamespacePath

        let ad = env.eAccessRights
        match ResolveLongIndentAsModuleOrNamespace ResultCollectionSettings.AllResults amap scopem OpenQualified env.eNameResEnv ad enclosingNamespacePathToOpen with 
        | Result modrefs -> OpenModulesOrNamespaces tcSink g amap scopem false env (List.map p23 modrefs)
        | Exception _ ->  env


//-------------------------------------------------------------------------
// Helpers for unification
//------------------------------------------------------------------------- 


/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyRefTupleType contextInfo cenv denv m ty ps = 
    let ptys = 
        if isRefTupleTy cenv.g ty then 
            let ptys = destRefTupleTy cenv.g ty
            if (List.length ps) = (List.length ptys) then ptys 
            else NewInferenceTypes ps
        else NewInferenceTypes ps

    let contextInfo =
        match contextInfo with
        | ContextInfo.RecordFields -> ContextInfo.TupleInRecordFields
        | _ -> contextInfo

    AddCxTypeEqualsType contextInfo denv cenv.css m ty (TType_tuple (tupInfoRef, ptys))
    ptys

/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyStructTupleType contextInfo cenv denv m ty ps = 
    let ptys = 
        if isStructTupleTy cenv.g ty then 
            let ptys = destStructTupleTy cenv.g ty
            if (List.length ps) = (List.length ptys) then ptys 
            else NewInferenceTypes ps
        else NewInferenceTypes ps
    AddCxTypeEqualsType contextInfo denv cenv.css m ty (TType_tuple (tupInfoStruct, ptys))

    ptys

/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyFunctionTypeUndoIfFailed cenv denv m ty =
    match tryDestFunTy cenv.g ty with
    | None ->
        let domainTy = NewInferenceType ()
        let resultTy = NewInferenceType ()
        if AddCxTypeEqualsTypeUndoIfFailed denv cenv.css m ty (domainTy --> resultTy) then 
            Some(domainTy,resultTy)
        else 
            None
    | r -> r

/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyFunctionType extraInfo cenv denv mFunExpr ty =
    match UnifyFunctionTypeUndoIfFailed cenv denv mFunExpr ty with
    | Some res -> res
    | None -> 
        match extraInfo with 
        | Some argm -> error (NotAFunction(denv,ty,mFunExpr,argm))
        | None ->    error (FunctionExpected(denv,ty,mFunExpr))

let ReportImplicitlyIgnoredBoolExpression denv m ty expr =
    let checkExpr m exprOpt =
        match exprOpt with 
        | Expr.App(Expr.Val(vf,_,_),_,_,exprs,_) when vf.LogicalName = opNameEquals ->
            match exprs with 
            | Expr.App(Expr.Val(propRef,_,_),_,_,Expr.Val(vf,_,_) :: _,_) :: _ ->
                if propRef.IsPropertyGetterMethod then
                    let propertyName = propRef.PropertyName
                    let hasCorrespondingSetter =
                        match propRef.ActualParent with
                        | Parent entityRef ->
                            entityRef.MembersOfFSharpTyconSorted
                            |> List.exists (fun valRef -> valRef.IsPropertySetterMethod && valRef.PropertyName = propertyName)
                        | _ -> false

                    if hasCorrespondingSetter then
                        UnitTypeExpectedWithPossiblePropertySetter (denv,ty,vf.DisplayName,propertyName,m)
                    else
                        UnitTypeExpectedWithEquality (denv,ty,m)
                else
                    UnitTypeExpectedWithEquality (denv,ty,m)
            | Expr.Op(TOp.ILCall(_,_,_,_,_,_,_,methodRef,_,_,_),_,Expr.Val(vf,_,_) :: _,_) :: _ when methodRef.Name.StartsWith "get_"->
                UnitTypeExpectedWithPossiblePropertySetter (denv,ty,vf.DisplayName,PrettyNaming.ChopPropertyName(methodRef.Name),m)
            | Expr.Val(vf,_,_) :: _ -> 
                UnitTypeExpectedWithPossibleAssignment (denv,ty,vf.IsMutable,vf.DisplayName,m)
            | _ -> UnitTypeExpectedWithEquality (denv,ty,m)
        | _ -> UnitTypeExpected (denv,ty,m)

    match expr with 
    | Some(Expr.Let(_,Expr.Sequential(_,inner,_,_,_),_,_))
    | Some(Expr.Sequential(_,inner,_,_,_)) ->
        let rec extractNext expr =
            match expr with
            | Expr.Sequential(_,inner,_,_,_) -> extractNext inner
            | _ -> checkExpr expr.Range expr
        extractNext inner
    | Some expr -> checkExpr m expr
    | _ -> UnitTypeExpected (denv,ty,m)

let UnifyUnitType cenv denv m ty exprOpt =
    if AddCxTypeEqualsTypeUndoIfFailed denv cenv.css m ty cenv.g.unit_ty then 
        true
    else
        let domainTy = NewInferenceType ()
        let resultTy = NewInferenceType ()
        if AddCxTypeEqualsTypeUndoIfFailed denv cenv.css m ty (domainTy --> resultTy) then 
            warning (FunctionValueUnexpected(denv,ty,m))
        else        
            if not (typeEquiv cenv.g cenv.g.bool_ty ty) then 
                warning (UnitTypeExpected (denv,ty,m)) 
            else
                warning (ReportImplicitlyIgnoredBoolExpression denv m ty exprOpt)
        false

//-------------------------------------------------------------------------
// Attribute target flags
//------------------------------------------------------------------------- 

// Logically extends System.AttributeTargets
module AttributeTargets =
    let FieldDecl  = AttributeTargets.Field ||| AttributeTargets.Property
    let FieldDeclRestricted = AttributeTargets.Field
    let UnionCaseDecl = AttributeTargets.Method    ||| AttributeTargets.Property
    let TyconDecl  = AttributeTargets.Class    ||| AttributeTargets.Interface ||| AttributeTargets.Delegate ||| AttributeTargets.Struct ||| AttributeTargets.Enum
    let ExnDecl    = AttributeTargets.Class
    let ModuleDecl = AttributeTargets.Class
    let Top        = AttributeTargets.Assembly ||| AttributeTargets.Module    ||| AttributeTargets.Method


let SettersOfPropInfos (pinfos:PropInfo list) = pinfos |> List.choose (fun pinfo -> if pinfo.HasSetter then Some(pinfo.SetterMethod,Some pinfo) else None) 
let GettersOfPropInfos (pinfos:PropInfo list) = pinfos |> List.choose (fun pinfo -> if pinfo.HasGetter then Some(pinfo.GetterMethod,Some pinfo) else None) 

// Specifies if overload resolution needs to notify Language Service of overload resolution result.
// In contrast with similar types in nameres, this type is in terms of infos instead of items.
// Convertors from Items to infos for methods and properties are provided.
[<RequireQualifiedAccess>]
type AfterTcOverloadResolution =
    // Notification is not needed
    |   DoNothing
    // Notify the tcSink
    |   SendToSink of ((MethInfo * PropInfo option) -> unit) * IfOverloadResolutionFails 
    // Find override among given overrides and notify the tcSink
    // The list contains candidate overrides.
    |   ReplaceWithOverrideAndSendToSink of (MethInfo * PropInfo option) list * ((MethInfo * PropInfo option) -> unit) * IfOverloadResolutionFails

    static member ForMethods afterOverloadResolution =
          match afterOverloadResolution with
          |   AfterOverloadResolution.DoNothing -> 
                  AfterTcOverloadResolution.DoNothing
          |   AfterOverloadResolution.SendToSink(callSink,fallback) ->
                  AfterTcOverloadResolution.SendToSink ((fun (minfo,_) -> Item.MethodGroup(minfo.LogicalName,[minfo],None) |> callSink), fallback)
          |   AfterOverloadResolution.ReplaceWithOverrideAndSendToSink (Item.MethodGroup(_,overridenMinfos,_orig), callSink,fallback) ->
                  AfterTcOverloadResolution.ReplaceWithOverrideAndSendToSink 
                      ((overridenMinfos |> List.map (fun minfo -> minfo,None)),(fun (minfo,_) -> Item.MethodGroup(minfo.LogicalName,[minfo],None) |> callSink),fallback)
          |   _ -> error(InternalError("Name resolution does not match overriden for method groups", range0))
    
    static member ForProperties name gettersOrSetters afterOverloadResolution = 
          let sendPropertyToSink callSink  =
              fun (_,pinfoOpt) ->
                  match pinfoOpt with
                  |   Some pinfo -> Item.Property(name,[pinfo]) |> callSink
                  |   _ -> ()

          match afterOverloadResolution with
          |   AfterOverloadResolution.DoNothing -> AfterTcOverloadResolution.DoNothing
          |   AfterOverloadResolution.SendToSink(callSink,fallback) -> AfterTcOverloadResolution.SendToSink(sendPropertyToSink callSink,fallback)
          |   AfterOverloadResolution.ReplaceWithOverrideAndSendToSink (Item.Property(_,pinfos),callSink,fallback) ->
                  AfterTcOverloadResolution.ReplaceWithOverrideAndSendToSink(gettersOrSetters pinfos, sendPropertyToSink callSink,fallback)
          |   AfterOverloadResolution.ReplaceWithOverrideAndSendToSink (_,_,_) ->
                  error(InternalError("Name resolution does not match overriden for properties",range0))

    static member ForConstructors afterOverloadResolution =
          match afterOverloadResolution with
          |   AfterOverloadResolution.DoNothing -> 
                  AfterTcOverloadResolution.DoNothing
          |   AfterOverloadResolution.SendToSink(callSink,fallback) ->
                  AfterTcOverloadResolution.SendToSink ((fun (minfo,_) -> Item.CtorGroup(minfo.LogicalName,[minfo]) |> callSink), fallback)
          |   _ -> error(InternalError("Name resolution does not match overriden for constructor groups", range0))

    static member ForNewConstructors tcSink (env:TcEnv) mObjTy methodName minfos =
          let sendToSink refinedMinfos =
              CallNameResolutionSink tcSink (mObjTy,env.NameEnv,Item.CtorGroup(methodName,refinedMinfos),Item.CtorGroup(methodName,minfos),ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)          
          match minfos with
          |  [] -> AfterTcOverloadResolution.DoNothing
          |  [_] -> 
              sendToSink minfos
              AfterTcOverloadResolution.DoNothing
          |   _ -> 
              AfterTcOverloadResolution.SendToSink ((fun (minfo,_) -> sendToSink [minfo]), (fun () -> sendToSink minfos) |> IfOverloadResolutionFails)

    member this.OnOverloadResolutionFailure() =
        match this with
        |   AfterTcOverloadResolution.DoNothing -> ()
        |   AfterTcOverloadResolution.SendToSink(_,IfOverloadResolutionFails f) -> f()
        |   AfterTcOverloadResolution.ReplaceWithOverrideAndSendToSink(_,_,IfOverloadResolutionFails f) -> f()


/// Typecheck rational constant terms in units-of-measure exponents
let rec TcSynRationalConst c =
  match c with
  | SynRationalConst.Integer i -> intToRational i
  | SynRationalConst.Negate c' -> NegRational (TcSynRationalConst c')
  | SynRationalConst.Rational(p,q,_) -> DivRational (intToRational p) (intToRational q)

/// Typecheck constant terms in expressions and patterns
let TcConst cenv ty m env c =
    let rec tcMeasure ms =
        match ms with
        | SynMeasure.One -> Measure.One      
        | SynMeasure.Named(tc,m) ->
            let ad = env.eAccessRights
            let tcref = ForceRaise(ResolveTypeLongIdent cenv.tcSink cenv.nameResolver ItemOccurence.Use OpenQualified env.eNameResEnv ad tc TypeNameResolutionStaticArgsInfo.DefiniteEmpty PermitDirectReferenceToGeneratedType.No)
            match tcref.TypeOrMeasureKind with
            | TyparKind.Type -> error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m))
            | TyparKind.Measure -> Measure.Con tcref

        | SynMeasure.Power(ms, exponent, _) -> Measure.RationalPower (tcMeasure ms, TcSynRationalConst exponent)
        | SynMeasure.Product(ms1,ms2,_) -> Measure.Prod(tcMeasure ms1, tcMeasure ms2)     
        | SynMeasure.Divide(ms1, ((SynMeasure.Seq (_::(_::_), _)) as ms2), m) -> 
            warning(Error(FSComp.SR.tcImplicitMeasureFollowingSlash(),m))
            Measure.Prod(tcMeasure ms1, Measure.Inv (tcMeasure ms2))
        | SynMeasure.Divide(ms1,ms2,_) -> 
            Measure.Prod(tcMeasure ms1, Measure.Inv (tcMeasure ms2))
        | SynMeasure.Seq(mss,_) -> ProdMeasures (List.map tcMeasure mss)
        | SynMeasure.Anon _ -> error(Error(FSComp.SR.tcUnexpectedMeasureAnon(),m))
        | SynMeasure.Var(_,m) -> error(Error(FSComp.SR.tcNonZeroConstantCannotHaveGenericUnit(),m))
   
    let unif ty2 = UnifyTypes cenv env m ty ty2
    let unif_measure_arg iszero tcr c =
        let measureTy = 
            match c with 
            | SynConst.Measure(_, SynMeasure.Anon _) ->
              (mkAppTy tcr [TType_measure (Measure.Var (NewAnonTypar (TyparKind.Measure,m,TyparRigidity.Anon,(if iszero then NoStaticReq else HeadTypeStaticReq),TyparDynamicReq.No)))])

            | SynConst.Measure(_, ms) -> mkAppTy tcr  [TType_measure (tcMeasure ms)]
            | _ -> mkAppTy tcr [TType_measure Measure.One]
        unif measureTy

   
    match c with 
    | SynConst.Unit         -> unif cenv.g.unit_ty;       Const.Unit
    | SynConst.Bool i       -> unif cenv.g.bool_ty;       Const.Bool i
    | SynConst.SByte i       -> unif cenv.g.sbyte_ty;      Const.SByte i
    | SynConst.Int16 i      -> unif cenv.g.int16_ty;      Const.Int16 i
    | SynConst.Int32 i      -> unif cenv.g.int_ty;        Const.Int32 i
    | SynConst.Int64 i      -> unif cenv.g.int64_ty;      Const.Int64 i
    | SynConst.IntPtr i  -> unif cenv.g.nativeint_ty;  Const.IntPtr i
    | SynConst.Byte i      -> unif cenv.g.byte_ty;       Const.Byte i
    | SynConst.UInt16 i     -> unif cenv.g.uint16_ty;     Const.UInt16 i
    | SynConst.UInt32 i     -> unif cenv.g.uint32_ty;     Const.UInt32 i
    | SynConst.UInt64 i     -> unif cenv.g.uint64_ty;     Const.UInt64 i
    | SynConst.UIntPtr i -> unif cenv.g.unativeint_ty; Const.UIntPtr i
    | SynConst.Measure(SynConst.Single f, _) | SynConst.Single f -> unif_measure_arg (f=0.0f) cenv.g.pfloat32_tcr c; Const.Single f
    | SynConst.Measure(SynConst.Double   f, _) | SynConst.Double   f -> unif_measure_arg (f=0.0)  cenv.g.pfloat_tcr c; Const.Double f
    | SynConst.Measure(SynConst.Decimal s, _) | SynConst.Decimal s -> unif_measure_arg false    cenv.g.pdecimal_tcr c; Const.Decimal s
    | SynConst.Measure(SynConst.SByte   i, _)  | SynConst.SByte    i -> unif_measure_arg (i=0y)   cenv.g.pint8_tcr c; Const.SByte i
    | SynConst.Measure(SynConst.Int16   i, _) | SynConst.Int16   i -> unif_measure_arg (i=0s)   cenv.g.pint16_tcr c; Const.Int16 i
    | SynConst.Measure(SynConst.Int32   i, _) | SynConst.Int32   i -> unif_measure_arg (i=0)    cenv.g.pint_tcr c; Const.Int32 i
    | SynConst.Measure(SynConst.Int64   i, _) | SynConst.Int64   i -> unif_measure_arg (i=0L)   cenv.g.pint64_tcr c; Const.Int64 i
    | SynConst.Char c       -> unif cenv.g.char_ty;       Const.Char c
    | SynConst.String (s,_) -> unif cenv.g.string_ty;     Const.String s
    | SynConst.UserNum _     -> error(InternalError(FSComp.SR.tcUnexpectedBigRationalConstant(), m))
    | SynConst.Measure _    -> error(Error(FSComp.SR.tcInvalidTypeForUnitsOfMeasure(), m))

    | SynConst.UInt16s _ -> error(InternalError(FSComp.SR.tcUnexpectedConstUint16Array(),m))
    | SynConst.Bytes _ -> error(InternalError(FSComp.SR.tcUnexpectedConstByteArray(),m))
 
/// Convert an Abstract IL ILFieldInit value read from .NET metadata to a TAST constant
let TcFieldInit (_m:range) lit = 
    match lit with 
    | ILFieldInit.String s   -> Const.String s
    | ILFieldInit.Null       -> Const.Zero
    | ILFieldInit.Bool    b -> Const.Bool b
    | ILFieldInit.Char    c -> Const.Char (char (int c))
    | ILFieldInit.Int8    x -> Const.SByte x
    | ILFieldInit.Int16   x -> Const.Int16 x
    | ILFieldInit.Int32   x -> Const.Int32 x
    | ILFieldInit.Int64   x -> Const.Int64 x
    | ILFieldInit.UInt8   x -> Const.Byte x
    | ILFieldInit.UInt16  x -> Const.UInt16 x
    | ILFieldInit.UInt32  x -> Const.UInt32 x
    | ILFieldInit.UInt64  x -> Const.UInt64 x
    | ILFieldInit.Single f -> Const.Single f
    | ILFieldInit.Double f -> Const.Double f 


//-------------------------------------------------------------------------
// Arities. These serve two roles in the system: 
//  1. syntactic arities come from the syntactic forms found
//     signature files and the syntactic forms of function and member definitions.
//  2. compiled arities representing representation choices w.r.t. internal representations of
//     functions and members.
//------------------------------------------------------------------------- 

// Adjust the arities that came from the parsing of the toptyp (arities) to be a valSynData. 
// This means replacing the "[unitArg]" arising from a "unit -> ty" with a "[]".
let AdjustValSynInfoInSignature g ty (SynValInfo(argsData,retData) as sigMD) = 
    if argsData.Length = 1 && argsData.Head.Length = 1 && isFunTy g ty && typeEquiv g g.unit_ty (domainOfFunTy g ty) then 
        SynValInfo(argsData.Head.Tail :: argsData.Tail, retData)
    else 
        sigMD 

/// The ValReprInfo for a value, except the number of typars is not yet inferred 
type PartialValReprInfo = PartialValReprInfo of ArgReprInfo list list * ArgReprInfo 

let TranslateTopArgSynInfo isArg m tcAttributes (SynArgInfo(attrs,isOpt,nm)) = 
    // Synthesize an artificial "OptionalArgument" attribute for the parameter
    let optAttrs = 
        if isOpt then 
            [ ( { TypeName=LongIdentWithDots(pathToSynLid m ["Microsoft";"FSharp";"Core";"OptionalArgument"],[]) 
                  ArgExpr=mkSynUnit m 
                  Target=None 
                  AppliesToGetterAndSetter=false 
                  Range=m} : SynAttribute) ] 
         else 
            []

    if isArg && not (isNil attrs) && Option.isNone nm then 
        errorR(Error(FSComp.SR.tcParameterRequiresName(),m))

    if not isArg && Option.isSome nm then 
        errorR(Error(FSComp.SR.tcReturnValuesCannotHaveNames(),m))
       
    // Call the attribute checking function 
    let attribs = tcAttributes (optAttrs@attrs)
    ({ Attribs = attribs; Name = nm } : ArgReprInfo)

/// Members have an arity inferred from their syntax. This "valSynData" is not quite the same as the arities 
/// used in the middle and backends of the compiler ("topValInfo"). 
/// "0" in a valSynData (see Ast.arity_of_pat) means a "unit" arg in a topValInfo 
/// Hence remove all "zeros" from arity and replace them with 1 here. 
/// Note we currently use the compiled form for choosing unique names, to distinguish overloads because this must match up 
/// between signature and implementation, and the signature just has "unit". 
let TranslateTopValSynInfo m tcAttributes (SynValInfo(argsData,retData)) = 
    PartialValReprInfo (argsData |> List.mapSquared (TranslateTopArgSynInfo true m (tcAttributes AttributeTargets.Parameter)), 
                       retData |> TranslateTopArgSynInfo false m (tcAttributes AttributeTargets.ReturnValue))

let TranslatePartialArity tps (PartialValReprInfo (argsData,retData)) = 
    ValReprInfo(ValReprInfo.InferTyparInfo tps,argsData,retData)


//-------------------------------------------------------------------------
// Members
//------------------------------------------------------------------------- 

let ComputeLogicalName (id:Ident) memberFlags = 
    match memberFlags.MemberKind with 
    | MemberKind.ClassConstructor -> ".cctor"
    | MemberKind.Constructor -> ".ctor"
    | MemberKind.Member -> 
        match id.idText with 
        | (".ctor" | ".cctor") as r ->  errorR(Error(FSComp.SR.tcInvalidMemberNameCtor(),id.idRange)); r
        | r -> r
    | MemberKind.PropertyGetSet ->  error(InternalError(FSComp.SR.tcMemberKindPropertyGetSetNotExpected(),id.idRange))
    | MemberKind.PropertyGet ->  "get_" + id.idText
    | MemberKind.PropertySet ->  "set_" + id.idText 

/// ValMemberInfoTransient(memberInfo,logicalName,compiledName)
type ValMemberInfoTransient = ValMemberInfoTransient of ValMemberInfo * string * string 


/// Make the unique "name" for a member.
//
// optImplSlotTy = None (for classes) or Some ty (when implementing interface type ty) 
let MakeMemberDataAndMangledNameForMemberVal(g,tcref,isExtrinsic,attrs,optImplSlotTys,memberFlags,valSynData,id,isCompGen)  =
    let logicalName = ComputeLogicalName id memberFlags
    let optIntfSlotTys = if optImplSlotTys |> List.forall (isInterfaceTy g) then optImplSlotTys else []
    let memberInfo : ValMemberInfo = 
        { ApparentParent=tcref 
          MemberFlags=memberFlags 
          IsImplemented=false
          // NOTE: This value is initially only set for interface implementations and those overrides 
          // where we manage to pre-infer which abstract is overriden by the method. It is filled in  
          // properly when we check the allImplemented implementation checks at the end of the inference scope. 
          ImplementedSlotSigs=optImplSlotTys |> List.map (fun ity -> TSlotSig(logicalName,ity,[],[],[],None)) }
    let isInstance = MemberIsCompiledAsInstance g tcref isExtrinsic memberInfo attrs
    if (memberFlags.IsDispatchSlot || not (isNil optIntfSlotTys)) then 
        if not isInstance then
          errorR(VirtualAugmentationOnNullValuedType(id.idRange))
    elif not memberFlags.IsOverrideOrExplicitImpl && memberFlags.IsInstance then 
        if not isExtrinsic && not isInstance then
            warning(NonVirtualAugmentationOnNullValuedType(id.idRange))

    let compiledName = 
        if isExtrinsic then 
             let tname = tcref.LogicalName
             let text = tname + "." + logicalName
             let text = if memberFlags.MemberKind <> MemberKind.Constructor && memberFlags.MemberKind <> MemberKind.ClassConstructor && not memberFlags.IsInstance then text^".Static" else text
             let text = if memberFlags.IsOverrideOrExplicitImpl then text^".Override" else text
             text
        else
            List.foldBack (tcrefOfAppTy g >> qualifiedMangledNameOfTyconRef) optIntfSlotTys logicalName
    
    if not isCompGen && IsMangledOpName id.idText && IsInfixOperator id.idText then 
        let m = id.idRange
        let name = DecompileOpName id.idText
        // Check symbolic members. Expect valSynData implied arity to be [[2]].
        match SynInfo.AritiesOfArgs valSynData with
        | [] | [0] -> warning(Error(FSComp.SR.memberOperatorDefinitionWithNoArguments(name),m))
        | n :: otherArgs ->
            let opTakesThreeArgs = PrettyNaming.IsTernaryOperator name
            if n<>2 && not opTakesThreeArgs then warning(Error(FSComp.SR.memberOperatorDefinitionWithNonPairArgument(name,n),m))
            if n<>3 && opTakesThreeArgs then warning(Error(FSComp.SR.memberOperatorDefinitionWithNonTripleArgument(name,n),m))
            if not (isNil otherArgs) then warning(Error(FSComp.SR.memberOperatorDefinitionWithCurriedArguments(name),m))

    if IsMangledOpName id.idText && isExtrinsic then 
        warning(Error(FSComp.SR.tcMemberOperatorDefinitionInExtrinsic(),id.idRange))

    ValMemberInfoTransient(memberInfo,logicalName,compiledName)


type OverridesOK = 
    | OverridesOK 
    | WarnOnOverrides
    | ErrorOnOverrides

/// A type to represent information associated with values to indicate what explicit (declared) type parameters
/// are given and what additional type parameters can be inferred, if any.
///
/// The declared type parameters, e.g. let f<'a> (x:'a) = x, plus an indication 
/// of whether additional polymorphism may be inferred, e.g. let f<'a,..> (x:'a) y = x 
type ExplicitTyparInfo = ExplicitTyparInfo of Tast.Typars * Tast.Typars * bool 

let permitInferTypars = ExplicitTyparInfo ([], [], true)
let dontInferTypars = ExplicitTyparInfo ([], [], false)

type ArgAndRetAttribs = ArgAndRetAttribs of Tast.Attribs list list * Tast.Attribs
let noArgOrRetAttribs = ArgAndRetAttribs ([],[])

/// A flag to represent the sort of bindings are we processing.
/// Processing "declaration" and "class" bindings that make up a module (such as "let x = 1 let y = 2") 
/// shares the same code paths (e.g. TcLetBinding and TcLetrec) as processing expression bindings (such as "let x = 1 in ...") 
/// Member bindings also use this path. 
//
/// However there are differences in how different bindings get processed,
/// i.e. module bindings get published to the implicitly accumulated module type, but expression 'let' bindings don't. 
type DeclKind = 
    | ModuleOrMemberBinding 
    /// Extensions to a type within the same assembly
    | IntrinsicExtensionBinding 
    /// Extensions to a type in a different assembly
    | ExtrinsicExtensionBinding 
    | ClassLetBinding of (* isStatic *) bool
    | ObjectExpressionOverrideBinding
    | ExpressionBinding 

    static member IsModuleOrMemberOrExtensionBinding x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding _ -> false
        | ObjectExpressionOverrideBinding -> false
        | ExpressionBinding -> false

    static member MustHaveArity x =  DeclKind.IsModuleOrMemberOrExtensionBinding x

    member x.CanBeDllImport = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding _ -> true
        | ObjectExpressionOverrideBinding -> false
        | ExpressionBinding -> false

    static member IsAccessModifierPermitted  x = DeclKind.IsModuleOrMemberOrExtensionBinding x

    static member ImplicitlyStatic  x = DeclKind.IsModuleOrMemberOrExtensionBinding x

    static member AllowedAttribTargets memberFlagsOpt x = 
        match x with 
        | ModuleOrMemberBinding | ObjectExpressionOverrideBinding -> 
            match memberFlagsOpt with
            | Some flags when flags.MemberKind = MemberKind.Constructor -> AttributeTargets.Constructor
            | Some flags when flags.MemberKind = MemberKind.PropertyGetSet -> AttributeTargets.Event ||| AttributeTargets.Property
            | Some flags when flags.MemberKind = MemberKind.PropertyGet -> AttributeTargets.Event ||| AttributeTargets.Property
            | Some flags when flags.MemberKind = MemberKind.PropertySet -> AttributeTargets.Property
            | Some _ -> AttributeTargets.Method
            | None -> AttributeTargets.Field ||| AttributeTargets.Method ||| AttributeTargets.Property
        | IntrinsicExtensionBinding  -> AttributeTargets.Method ||| AttributeTargets.Property
        | ExtrinsicExtensionBinding -> AttributeTargets.Method ||| AttributeTargets.Property
        | ClassLetBinding _ -> AttributeTargets.Field ||| AttributeTargets.Method
        | ExpressionBinding -> enum 0 // indicates attributes not allowed on expression 'let' bindings

    // Note: now always true
    static member CanGeneralizeConstrainedTypars  x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding _ -> true
        | ObjectExpressionOverrideBinding -> true
        | ExpressionBinding -> true
        
    static member ConvertToLinearBindings  x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding _ -> true
        | ObjectExpressionOverrideBinding -> true
        | ExpressionBinding -> false 

    static member CanOverrideOrImplement x = 
        match x with 
        | ModuleOrMemberBinding -> OverridesOK
        | IntrinsicExtensionBinding -> WarnOnOverrides
        | ExtrinsicExtensionBinding -> ErrorOnOverrides
        | ClassLetBinding _ -> ErrorOnOverrides 
        | ObjectExpressionOverrideBinding -> OverridesOK
        | ExpressionBinding -> ErrorOnOverrides 

//-------------------------------------------------------------------------
// Data structures that track the gradual accumualtion of information
// about values and members during inference.
//------------------------------------------------------------------------- 

/// The results of preliminary pass over patterns to extract variables being declared.
type PrelimValScheme1 = 
    | PrelimValScheme1 of 
        Ident * 
        ExplicitTyparInfo * 
        TType * 
        PartialValReprInfo option *
        ValMemberInfoTransient option * 
        bool * 
        ValInline * 
        ValBaseOrThisInfo * 
        ArgAndRetAttribs * 
        SynAccess option * 
        bool
    member x.Type = let (PrelimValScheme1(_,_,ty,_,_,_,_,_,_,_,_)) = x in ty
    member x.Ident = let (PrelimValScheme1(id,_,_,_,_,_,_,_,_,_,_)) = x in id
    
/// The results of applying let-style generalization after type checking. 
type PrelimValScheme2 = 
    PrelimValScheme2 of 
        Ident * 
        TypeScheme * 
        PartialValReprInfo option *
        ValMemberInfoTransient option  * 
        bool * 
        ValInline * 
        ValBaseOrThisInfo * 
        ArgAndRetAttribs * 
        SynAccess option * 
        bool *
        bool (* hasDeclaredTypars *) 
        

/// The results of applying arity inference to PrelimValScheme2 
type ValScheme = 
    | ValScheme of 
        Ident * 
        TypeScheme * 
        ValReprInfo option * 
        ValMemberInfoTransient option  * 
        bool *  // isMutable
        ValInline * 
        ValBaseOrThisInfo * 
        SynAccess option * 
        bool * // compgen *
        bool * // isIncrClass 
        bool * // isTyFunc 
        bool   // hasDeclaredTypars 
    member x.GeneralizedTypars = let (ValScheme(_,TypeScheme(gtps,_),_,_,_,_,_,_,_,_,_,_)) = x in gtps
    member x.TypeScheme = let (ValScheme(_,ts,_,_,_,_,_,_,_,_,_,_)) = x in ts
        
//-------------------------------------------------------------------------
// Data structures that track the whole process of taking a syntactic binding and
// checking it.
//------------------------------------------------------------------------- 

/// Translation of patterns is List.unzip into three phases. The first collects names. 
/// The second is run after val_specs have been created for those names and inference 
/// has been resolved. The second phase is run by applying a function returned by the 
/// first phase. The input to the second phase is a List.map that gives the Val and type scheme 
/// for each value bound by the pattern. 
type TcPatPhase2Input = 
    | TcPatPhase2Input of (Val * TypeScheme) NameMap * bool
    // Get an input indicating we are no longer on the left-most path through a disjunctive "or" pattern
    member x.RightPath = (let (TcPatPhase2Input(a,_)) = x in TcPatPhase2Input(a,false))

/// The first phase of checking and elaborating a binding leaves a whole goop of information. 
/// This is a bit of a mess: much of this information is carried on a per-value basis by the 
/// "NameMap<PrelimValScheme1>". 
type CheckedBindingInfo = 
    | CheckedBindingInfo of 
       ValInline * 
       Tast.Attribs * 
       XmlDoc * 
       (TcPatPhase2Input -> PatternMatchCompilation.Pattern) * 
       ExplicitTyparInfo * 
       NameMap<PrelimValScheme1> * 
       Expr * 
       ArgAndRetAttribs * 
       TType * 
       range *
       SequencePointInfoForBinding * 
       bool * // compiler generated? 
       Const option * // literal value? 
       bool // fixed?
    member x.Expr = let (CheckedBindingInfo(_,_,_,_,_,_,expr,_,_,_,_,_,_,_)) = x in expr
    member x.SeqPoint = let (CheckedBindingInfo(_,_,_,_,_,_,_,_,_,_,spBind,_,_,_)) = x in spBind

//-------------------------------------------------------------------------
// Helpers related to type schemes
//------------------------------------------------------------------------- 

let GeneralizedTypeForTypeScheme typeScheme = 
    let (TypeScheme(generalizedTypars,tau)) = typeScheme
    tryMkForallTy generalizedTypars tau

let NonGenericTypeScheme ty = TypeScheme([],ty)

//-------------------------------------------------------------------------
// Helpers related to publishing values, types and members into the
// elaborated representation.
//------------------------------------------------------------------------- 

let UpdateAccModuleOrNamespaceType cenv env f = 
    // When compiling FSharp.Core, modify the fslib CCU to ensure forward stable references used by 
    // the compiler can be resolved ASAP. Not at all pretty but it's hard to 
    // find good ways to do references from the compiler into a term graph.
    if cenv.compilingCanonicalFslibModuleType then 
        let nleref = mkNonLocalEntityRef cenv.topCcu (arrPathOfLid env.ePath)
        let modul = nleref.Deref
        modul.entity_modul_contents <- MaybeLazy.Strict (f true modul.ModuleOrNamespaceType)
    SetCurrAccumulatedModuleOrNamespaceType env (f false (GetCurrAccumulatedModuleOrNamespaceType env))
  
let PublishModuleDefn cenv env mspec = 
    UpdateAccModuleOrNamespaceType cenv env (fun intoFslibCcu mty -> 
       if intoFslibCcu then mty
       else mty.AddEntity mspec)
    let item = Item.ModuleOrNamespaces([mkLocalModRef mspec])
    CallNameResolutionSink cenv.tcSink (mspec.Range,env.NameEnv,item,item,ItemOccurence.Binding,env.DisplayEnv,env.eAccessRights)

let PublishTypeDefn cenv env tycon = 
    UpdateAccModuleOrNamespaceType cenv env (fun _ mty -> 
       mty.AddEntity tycon)

let PublishValueDefnPrim cenv env (vspec:Val) = 
    UpdateAccModuleOrNamespaceType cenv env (fun _ mty -> 
        mty.AddVal vspec)

let PublishValueDefn cenv env declKind (vspec:Val) =
    if (declKind = ModuleOrMemberBinding) && 
       ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace) && 
       (Option.isNone vspec.MemberInfo) then 
           errorR(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),vspec.Range))

    if (declKind = ExtrinsicExtensionBinding) && 
       ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace) then 
           errorR(Error(FSComp.SR.tcNamespaceCannotContainExtensionMembers(),vspec.Range))

    // Publish the value to the module type being generated. 
    match declKind with 
    | ModuleOrMemberBinding
    | ExtrinsicExtensionBinding
    | IntrinsicExtensionBinding -> PublishValueDefnPrim cenv env vspec
    | _ -> ()

    match vspec.MemberInfo with 
    | Some _ when 
        (not vspec.IsCompilerGenerated && 
         // Extrinsic extensions don't get added to the tcaug
         not (declKind = ExtrinsicExtensionBinding)) -> 
         // // Static initializers don't get published to the tcaug 
         // not (memberInfo.MemberFlags.MemberKind = MemberKind.ClassConstructor)) -> 
        
        let tcaug = vspec.MemberApparentParent.TypeContents
        let vref = mkLocalValRef vspec
        tcaug.tcaug_adhoc <- NameMultiMap.add vspec.LogicalName vref tcaug.tcaug_adhoc
        tcaug.tcaug_adhoc_list.Add (ValRefIsExplicitImpl cenv.g vref, vref)
    |  _ -> ()

let CombineVisibilityAttribs vis1 vis2 m = 
    match vis1 with
    | Some _ ->
        if Option.isSome vis2 then 
            errorR(Error(FSComp.SR.tcMultipleVisibilityAttributes(),m))
        vis1
    | _ -> vis2

let ComputeAccessAndCompPath env declKindOpt m vis overrideVis actualParent = 
    let accessPath = env.eAccessPath
    let accessModPermitted = 
        match declKindOpt with 
        | None -> true
        | Some declKind -> DeclKind.IsAccessModifierPermitted declKind

    if Option.isSome vis && not accessModPermitted then 
        errorR(Error(FSComp.SR.tcMultipleVisibilityAttributesWithLet(),m))

    let vis = 
        match overrideVis, vis with 
        | Some v,_ -> v
        | _, None -> taccessPublic (* a module or member binding defaults to "public" *)
        | _, Some SynAccess.Public -> taccessPublic
        | _, Some SynAccess.Private -> taccessPrivate accessPath
        | _, Some SynAccess.Internal -> taccessInternal 

    let vis = 
        match actualParent with 
        | ParentNone -> vis 
        | Parent tcref -> combineAccess vis tcref.Accessibility
        
    let cpath = if accessModPermitted then Some env.eCompPath else None
    vis,cpath 

let CheckForAbnormalOperatorNames cenv (idRange:range) opName isMember =    
    if (idRange.EndColumn - idRange.StartColumn <= 5) && 
        not cenv.g.compilingFslib 
    then
        match opName with 
        | PrettyNaming.Relational ->
            if isMember then
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMethodNameForRelationalOperator(opName, (CompileOpName opName)),idRange))
            else
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidOperatorDefinitionRelational(opName),idRange))
        | PrettyNaming.Equality -> 
            if isMember then
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMethodNameForEquality(opName, (CompileOpName opName)),idRange))
            else
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidOperatorDefinitionEquality(opName),idRange))
        | PrettyNaming.Control -> 
            if isMember then
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMemberName(opName, (CompileOpName opName)),idRange))
            else
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidOperatorDefinition(opName),idRange))
        | PrettyNaming.Indexer -> 
            if not isMember then
                error(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidIndexOperatorDefinition(opName),idRange))
        | PrettyNaming.FixedTypes -> 
            if isMember then
                warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMemberNameFixedTypes(opName),idRange))
        | PrettyNaming.Other -> ()

let MakeAndPublishVal cenv env (altActualParent,inSig,declKind,vrec,(ValScheme(id,typeScheme,topValData,memberInfoOpt,isMutable,inlineFlag,baseOrThis,vis,compgen,isIncrClass,isTyFunc,hasDeclaredTypars)),attrs,doc,konst,isGeneratedEventVal) =
    let ty = GeneralizedTypeForTypeScheme typeScheme
    let m = id.idRange

    let isTopBinding = 
        match declKind with 
        | ModuleOrMemberBinding -> true
        | ExtrinsicExtensionBinding -> true
        | IntrinsicExtensionBinding -> true
        | _ -> false

    let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
    let actualParent, overrideVis = 
        // Use the parent of the member if it's available 
        // If it's an extrinsic extension member or not a member then use the containing module.
        match memberInfoOpt with 
        | Some (ValMemberInfoTransient(memberInfo,_,_)) when not isExtrinsic -> 
            if memberInfo.ApparentParent.IsModuleOrNamespace then 
                errorR(InternalError(FSComp.SR.tcExpectModuleOrNamespaceParent(id.idText),m))

            // Members of interface implementations have the accessibility of the interface
            // they are implementing.
            let vis =
                if Tastops.MemberIsExplicitImpl cenv.g memberInfo then
                    let slotSig = List.head memberInfo.ImplementedSlotSigs
                    match slotSig.ImplementedType with
                    | TType_app (tyconref,_) -> Some tyconref.Accessibility
                    | _ -> None
                else
                    None
            Parent(memberInfo.ApparentParent), vis
        | _ -> altActualParent, None
                    
    let vis,_ = ComputeAccessAndCompPath env (Some declKind) id.idRange vis overrideVis actualParent

    let inlineFlag = 
        if HasFSharpAttributeOpt cenv.g cenv.g.attrib_DllImportAttribute attrs then 
            if inlineFlag = ValInline.PseudoVal || inlineFlag = ValInline.Always then 
              errorR(Error(FSComp.SR.tcDllImportStubsCannotBeInlined(),m)) 
            ValInline.Never 
        else 
            let implflags = 
                match TryFindFSharpAttribute cenv.g cenv.g.attrib_MethodImplAttribute attrs with
                | Some (Attrib(_,_,[ AttribInt32Arg flags ],_,_,_,_))  -> flags
                | _ -> 0x0
            // MethodImplOptions.NoInlining = 0x8
            let NO_INLINING = 0x8
            if (implflags &&& NO_INLINING) <> 0x0 then
                ValInline.Never
            else
                inlineFlag

    // CompiledName not allowed on virtual/abstract/override members        
    let compiledNameAttrib = TryFindFSharpStringAttribute cenv.g cenv.g.attrib_CompiledNameAttribute attrs
    if Option.isSome compiledNameAttrib then
        match memberInfoOpt with 
        | Some (ValMemberInfoTransient(memberInfo,_,_)) -> 
            if memberInfo.MemberFlags.IsDispatchSlot || memberInfo.MemberFlags.IsOverrideOrExplicitImpl then
                errorR(Error(FSComp.SR.tcCompiledNameAttributeMisused(),m))
        | None -> 
            match altActualParent with
            | ParentNone -> errorR(Error(FSComp.SR.tcCompiledNameAttributeMisused(),m)) 
            | _ -> ()

    let compiledNameIsOnProp =
        match memberInfoOpt with
        | Some (ValMemberInfoTransient(memberInfo,_,_)) ->
            memberInfo.MemberFlags.MemberKind = MemberKind.PropertyGet || 
            memberInfo.MemberFlags.MemberKind = MemberKind.PropertySet ||
            memberInfo.MemberFlags.MemberKind = MemberKind.PropertyGetSet
        | _ -> false

    let compiledName = 
        match compiledNameAttrib with 
        // We fix up CompiledName on properties during codegen
        | Some _ when not compiledNameIsOnProp -> compiledNameAttrib 
        | _ -> 
            match memberInfoOpt with 
            | Some (ValMemberInfoTransient(_,_,compiledName)) -> 
                Some compiledName
            | None ->  
                None

    let logicalName = 
        match memberInfoOpt with 
        | Some (ValMemberInfoTransient(_,logicalName,_)) -> 
            logicalName
        | None ->  
            id.idText

    let memberInfoOpt = 
        match memberInfoOpt with 
        | Some (ValMemberInfoTransient(memberInfo,_,_)) -> 
            Some memberInfo
        | None ->  
            None

    let vspec = 
        NewVal (logicalName,id.idRange,compiledName,ty,
                      (if ((* (isByrefTy cenv.g ty) || *) isMutable) then Mutable else Immutable),
                      compgen,topValData,vis,vrec,memberInfoOpt,baseOrThis,attrs,inlineFlag,doc,isTopBinding,isExtrinsic,isIncrClass,isTyFunc,
                      (hasDeclaredTypars || inSig),isGeneratedEventVal,konst,actualParent)

    
    CheckForAbnormalOperatorNames cenv id.idRange (DecompileOpName vspec.CoreDisplayName) (Option.isSome memberInfoOpt)

    PublishValueDefn cenv env declKind vspec

    match cenv.tcSink.CurrentSink with 
    | None -> ()
    | Some _ -> 
        if not vspec.IsCompilerGenerated then
            match vspec.MemberInfo with
            // `this` reference named `__`. It's either:
            // * generated by compiler for auto properties or 
            // * provided by source code (i.e. `member __.Method = ...`)
            // We don't notify sink about it to prevent generating `FSharpSymbol` for it and appearing in completion list.
            | None when vspec.BaseOrThisInfo = ValBaseOrThisInfo.MemberThisVal && vspec.LogicalName = "__" -> () 
            | _ ->
                let nenv = AddFakeNamedValRefToNameEnv vspec.DisplayName env.NameEnv (mkLocalValRef vspec) 
                CallEnvSink cenv.tcSink (vspec.Range,nenv,env.eAccessRights)
                let item = Item.Value(mkLocalValRef vspec)
                CallNameResolutionSink cenv.tcSink (vspec.Range,nenv,item,item,ItemOccurence.Binding,env.DisplayEnv,env.eAccessRights)
    vspec

let MakeAndPublishVals cenv env (altActualParent,inSig,declKind,vrec,valSchemes,attrs,doc,konst) =
    Map.foldBack
        (fun name (valscheme:ValScheme) values -> 
          Map.add name (MakeAndPublishVal cenv env (altActualParent,inSig,declKind,vrec,valscheme,attrs,doc,konst,false), valscheme.TypeScheme) values)
        valSchemes
        Map.empty

let MakeAndPublishBaseVal cenv env baseIdOpt ty = 
    baseIdOpt
    |> Option.map (fun (id:Ident) ->
       let valscheme = ValScheme(id,NonGenericTypeScheme(ty),None,None,false,ValInline.Never,BaseVal,None,false,false,false,false)
       MakeAndPublishVal cenv env (ParentNone,false,ExpressionBinding,ValNotInRecScope,valscheme,[],XmlDoc.Empty,None,false))

let InstanceMembersNeedSafeInitCheck cenv m thisTy = 
    ExistsInEntireHierarchyOfType 
        (fun ty -> not (isStructTy cenv.g ty) && (match tryDestAppTy cenv.g ty with Some tcref when tcref.HasSelfReferentialConstructor -> true | _ -> false))
        cenv.g 
        cenv.amap
        m 
        AllowMultiIntfInstantiations.Yes
        thisTy
        
let MakeSafeInitField (g: TcGlobals) env m isStatic = 
    let id = ident(globalNng.FreshCompilerGeneratedName("init",m),m)
    let taccess = TAccess [env.eAccessPath]
    NewRecdField isStatic None id g.int_ty true true [] [] XmlDoc.Empty taccess true

// Make the "delayed reference" boolean value recording the safe initialization of a type in a hierarchy where there is a HasSelfReferentialConstructor
let ComputeInstanceSafeInitInfo cenv env m thisTy = 
    if InstanceMembersNeedSafeInitCheck cenv m thisTy then 
        let rfield =  MakeSafeInitField cenv.g env m false
        let tcref = tcrefOfAppTy cenv.g thisTy
        SafeInitField (mkRecdFieldRef tcref rfield.Name, rfield)
    else
        NoSafeInitInfo

// Make the "delayed reference" value where the this pointer will reside after calling the base class constructor
// Make the value for the 'this' pointer for use within a constructor
let MakeAndPublishSafeThisVal cenv env (thisIdOpt: Ident option) thisTy = 
    match thisIdOpt with 
    | Some thisId -> 
        // for structs, thisTy is a byref 
        if not (isFSharpObjModelTy cenv.g thisTy) then 
            errorR(Error(FSComp.SR.tcStructsCanOnlyBindThisAtMemberDeclaration(),thisId.idRange))

        let valScheme = ValScheme(thisId,NonGenericTypeScheme(mkRefCellTy cenv.g thisTy),None,None,false,ValInline.Never,CtorThisVal,None,false,false,false,false)
        Some(MakeAndPublishVal cenv env (ParentNone, false, ExpressionBinding, ValNotInRecScope, valScheme, [], XmlDoc.Empty, None, false))

    | None -> 
        None 


//-------------------------------------------------------------------------
// Helpers for type inference for recursive bindings
//------------------------------------------------------------------------- 

/// Fixup the type instantiation at recursive references. Used after the bindings have been
/// checked. The fixups are applied by using mutation.
let AdjustAndForgetUsesOfRecValue cenv (vrefTgt: ValRef) (valScheme : ValScheme) =
    let (TypeScheme(generalizedTypars,_)) = valScheme.TypeScheme
    let fty = GeneralizedTypeForTypeScheme valScheme.TypeScheme
    let lvrefTgt = vrefTgt.Deref
    if not (isNil generalizedTypars) then 
        // Find all the uses of this recursive binding and use mutation to adjust the expressions 
        // at those points in order to record the inferred type parameters. 
        let recUses = cenv.recUses.Find lvrefTgt
        recUses 
        |> List.iter (fun (fixupPoint,m,isComplete) -> 
              if not isComplete then 
                  // Keep any values for explicit type arguments 
                  let fixedUpExpr = 
                      let vrefFlags,tyargs0 = 
                          match !fixupPoint with 
                          | Expr.App(Expr.Val (_,vrefFlags,_),_,tyargs0,[],_) -> vrefFlags,tyargs0
                          | Expr.Val(_,vrefFlags,_) -> vrefFlags,[] 
                          | _ -> 
                              errorR(Error(FSComp.SR.tcUnexpectedExprAtRecInfPoint(),m)) 
                              NormalValUse,[]
                  
                      let ityargs = generalizeTypars (List.drop (List.length tyargs0) generalizedTypars)
                      primMkApp (Expr.Val (vrefTgt,vrefFlags,m),fty) (tyargs0 @ ityargs) [] m
                  fixupPoint := fixedUpExpr)

    vrefTgt.Deref.SetValRec ValNotInRecScope
    cenv.recUses <- cenv.recUses.Remove vrefTgt.Deref 
     

/// Set the properties of recursive values that are only fully known after inference is complete 
let AdjustRecType _cenv (vspec:Val) (ValScheme(_,typeScheme,topValData,_,_,_,_,_,_,_,_,_)) =
    let fty = GeneralizedTypeForTypeScheme typeScheme
    vspec.SetType fty
    vspec.SetValReprInfo topValData
    vspec.SetValRec (ValInRecScope true)
       
/// Record the generated value expression as a place where we will have to 
/// adjust using AdjustAndForgetUsesOfRecValue at a letrec point. Every use of a value 
/// under a letrec gets used at the _same_ type instantiation. 
let RecordUseOfRecValue cenv vrec (vrefTgt: ValRef) vexp m = 
    match vrec with 
    | ValInRecScope isComplete -> 
        let fixupPoint = ref vexp
        cenv.recUses <- cenv.recUses.Add (vrefTgt.Deref, (fixupPoint,m,isComplete)) 
        Expr.Link (fixupPoint)
    | ValNotInRecScope -> 
        vexp

type RecursiveUseFixupPoints = RecursiveUseFixupPoints of (Expr ref * range) list

/// Get all recursive references, for fixing up delayed recursion using laziness 
let GetAllUsesOfRecValue cenv vrefTgt = 
    RecursiveUseFixupPoints (cenv.recUses.Find vrefTgt |> List.map (fun (fixupPoint,m,_) -> (fixupPoint,m)))


//-------------------------------------------------------------------------
// Helpers for Generalization
//------------------------------------------------------------------------- 

let ChooseCanonicalDeclaredTyparsAfterInference g denv declaredTypars m =
    declaredTypars |> List.iter (fun tp -> 
      let ty = mkTyparTy tp
      if not (isAnyParTy g ty) then 
          error(Error(FSComp.SR.tcLessGenericBecauseOfAnnotation(tp.Name,NicePrint.prettyStringOfTy denv ty),tp.Range)))
    
    let declaredTypars = NormalizeDeclaredTyparsForEquiRecursiveInference g declaredTypars

    if (ListSet.setify typarEq declaredTypars).Length <> declaredTypars.Length then 
        errorR(Error(FSComp.SR.tcConstrainedTypeVariableCannotBeGeneralized(),m))

    declaredTypars

let ChooseCanonicalValSchemeAfterInference g denv valscheme m =
    let (ValScheme(id,typeScheme,arityInfo,memberInfoOpt,isMutable,inlineFlag,baseOrThis,vis,compgen,isIncrClass,isTyFunc,hasDeclaredTypars)) = valscheme
    let (TypeScheme(generalizedTypars,ty)) = typeScheme
    let generalizedTypars = ChooseCanonicalDeclaredTyparsAfterInference g denv generalizedTypars m
    let typeScheme = TypeScheme(generalizedTypars,ty)
    let valscheme = ValScheme(id,typeScheme,arityInfo,memberInfoOpt,isMutable,inlineFlag,baseOrThis,vis,compgen,isIncrClass,isTyFunc,hasDeclaredTypars)
    valscheme

let PlaceTyparsInDeclarationOrder declaredTypars generalizedTypars  =
    declaredTypars @ (generalizedTypars |> List.filter (fun tp -> not (ListSet.contains typarEq tp declaredTypars)))

let SetTyparRigid _g denv m (tp:Typar) = 
    match tp.Solution with 
    | None -> ()
    | Some ty -> 
        if tp.IsCompilerGenerated then 
            errorR(Error(FSComp.SR.tcGenericParameterHasBeenConstrained(NicePrint.prettyStringOfTy denv ty),m))
        else 
            errorR(Error(FSComp.SR.tcTypeParameterHasBeenConstrained(NicePrint.prettyStringOfTy denv ty),tp.Range))
    tp.SetRigidity TyparRigidity.Rigid

let GeneralizeVal cenv denv enclosingDeclaredTypars generalizedTyparsForThisBinding 
        (PrelimValScheme1(id,iflex,ty,partialValReprInfo,memberInfoOpt,isMutable,inlineFlag,baseOrThis,argAttribs,vis,compgen)) = 

    let (ExplicitTyparInfo(_rigidCopyOfDeclaredTypars,declaredTypars,_)) = iflex

    let m = id.idRange

    let allDeclaredTypars = enclosingDeclaredTypars@declaredTypars
    let allDeclaredTypars = ChooseCanonicalDeclaredTyparsAfterInference cenv.g denv allDeclaredTypars m

    // Trim out anything not in type of the value (as opposed to the type of the r.h.s) 
    // This is important when a single declaration binds 
    // multiple generic items, where each item does not use all the polymorphism 
    // of the r.h.s. , e.g. let x,y = None,[] 
    let computeRelevantTypars thruFlag = 
        let ftps = freeInTypeLeftToRight cenv.g thruFlag ty
        let generalizedTypars = generalizedTyparsForThisBinding |> List.filter (fun tp -> ListSet.contains typarEq tp ftps)
        // Put declared typars first 
        let generalizedTypars = PlaceTyparsInDeclarationOrder allDeclaredTypars generalizedTypars  
        generalizedTypars

    let generalizedTypars = computeRelevantTypars false

    // Check stability of existence and ordering of type parameters under erasure of type abbreviations
    let generalizedTyparsLookingThroughTypeAbbreviations = computeRelevantTypars true
    if not (generalizedTypars.Length = generalizedTyparsLookingThroughTypeAbbreviations.Length && 
            List.forall2 typarEq generalizedTypars generalizedTyparsLookingThroughTypeAbbreviations)
    then
        warning(Error(FSComp.SR.tcTypeParametersInferredAreNotStable(),m))

    let hasDeclaredTypars = not (isNil declaredTypars)
    // This is just about the only place we form a TypeScheme 
    let tyScheme = TypeScheme(generalizedTypars, ty)
    PrelimValScheme2(id,tyScheme,partialValReprInfo,memberInfoOpt,isMutable,inlineFlag,baseOrThis,argAttribs,vis,compgen,hasDeclaredTypars)

let GeneralizeVals cenv denv enclosingDeclaredTypars generalizedTypars types = 
    NameMap.map (GeneralizeVal cenv denv enclosingDeclaredTypars generalizedTypars) types

let DontGeneralizeVals types = 
    let dontGeneralizeVal (PrelimValScheme1(id,_,ty,partialValReprInfoOpt,memberInfoOpt,isMutable,inlineFlag,baseOrThis,argAttribs,vis,compgen)) = 
        PrelimValScheme2(id, NonGenericTypeScheme(ty), partialValReprInfoOpt,memberInfoOpt,isMutable,inlineFlag,baseOrThis,argAttribs,vis,compgen,false)
    NameMap.map dontGeneralizeVal types

let InferGenericArityFromTyScheme (TypeScheme(generalizedTypars,_)) partialValReprInfo =
    TranslatePartialArity generalizedTypars partialValReprInfo

let ComputeIsTyFunc(id:Ident,hasDeclaredTypars,arityInfo:ValReprInfo option) = 
    hasDeclaredTypars && 
    (match arityInfo with 
     | None -> error(Error(FSComp.SR.tcExplicitTypeParameterInvalid(),id.idRange)) 
     | Some info -> info.NumCurriedArgs = 0) 

let UseSyntacticArity declKind typeScheme partialValReprInfo = 
    if DeclKind.MustHaveArity declKind then 
        Some(InferGenericArityFromTyScheme typeScheme partialValReprInfo)
    else 
        None

/// Combine the results of InferSynValData and InferArityOfExpr. 
//
// The F# spec says that we infer arities from declaration forms and types.
//
// For example
//     let f (a,b) c = 1                  // gets arity [2;1] 
//     let f (a:int*int) = 1              // gets arity [2], based on type
//     let f () = 1                       // gets arity [0]
//     let f = (fun (x:int) (y:int) -> 1) // gets arity [1;1]
//     let f = (fun (x:int*int) y -> 1)   // gets arity [2;1]
//
// Some of this arity inference is purely syntax directed and done in InferSynValData in ast.fs
// Some is done by InferArityOfExpr. 
//
// However, there are some corner cases in this specification. In particular, consider
//   let f () () = 1             // [0;1] or [0;0]?  Answer: [0;1]
//   let f (a:unit) = 1          // [0] or [1]?      Answer: [1]
//   let f = (fun () -> 1)       // [0] or [1]?      Answer: [0]
//   let f = (fun (a:unit) -> 1) // [0] or [1]?      Answer: [1]
//
// The particular choice of [1] for
//   let f (a:unit) = 1          
// is intended to give a disambiguating form for members that override methods taking a single argument 
// instantiated to type "unit", e.g.
//    type Base<'a> = 
//        abstract M : 'a -> unit
//
//    { new Base<int> with 
//        member x.M(v:int) = () }
//
//    { new Base<unit> with 
//        member x.M(v:unit) = () }
//
let CombineSyntacticAndInferredArities g declKind rhsExpr prelimScheme = 
    let (PrelimValScheme2(_,typeScheme,partialValReprInfoOpt,memberInfoOpt,isMutable,_,_,ArgAndRetAttribs(argAttribs,retAttribs),_,_,_)) = prelimScheme
    match partialValReprInfoOpt, DeclKind.MustHaveArity declKind with
    | _        ,false -> None
    | None     ,true  -> Some(PartialValReprInfo([],ValReprInfo.unnamedRetVal))
    // Don't use any expression information for members, where syntax dictates the arity completely
    | _ when memberInfoOpt.IsSome -> 
        partialValReprInfoOpt
    | Some(partialValReprInfoFromSyntax),true  -> 
        let (PartialValReprInfo(curriedArgInfosFromSyntax,retInfoFromSyntax)) = partialValReprInfoFromSyntax
        let partialArityInfo = 
            if isMutable then 
                PartialValReprInfo ([],retInfoFromSyntax)
            else
            
                let (ValReprInfo (_,curriedArgInfosFromExpression,_)) = 
                    InferArityOfExpr g (GeneralizedTypeForTypeScheme typeScheme) argAttribs retAttribs rhsExpr

                // Choose between the syntactic arity and the expression-inferred arity
                // If the syntax specifies an eliminated unit arg, then use that
                let choose ai1 ai2 = 
                    match ai1,ai2 with 
                    | [],_ -> []
                    // Dont infer eliminated unit args from the expression if they don't occur syntactically.
                    | ai,[] -> ai
                    // If we infer a tupled argument from the expression and/or type then use that
                    | _ when ai1.Length < ai2.Length -> ai2
                    | _ -> ai1
                let rec loop ais1 ais2 =
                    match ais1,ais2 with 
                    // If the expression infers additional arguments then use those (this shouldn't happen, since the
                    // arity inference done on the syntactic form should give identical results)
                    | [],ais | ais,[] -> ais
                    | (h1::t1),(h2::t2) -> choose h1 h2 :: loop t1 t2
                let curriedArgInfos = loop curriedArgInfosFromSyntax curriedArgInfosFromExpression
                PartialValReprInfo (curriedArgInfos,retInfoFromSyntax)

        Some(partialArityInfo)

let BuildValScheme declKind partialArityInfoOpt prelimScheme = 
    let (PrelimValScheme2(id,typeScheme,_,memberInfoOpt,isMutable,inlineFlag,baseOrThis,_,vis,compgen,hasDeclaredTypars)) = prelimScheme
    let topValInfo = 
        if DeclKind.MustHaveArity declKind then 
            Option.map (InferGenericArityFromTyScheme typeScheme) partialArityInfoOpt
        else
            None
    let isTyFunc = ComputeIsTyFunc(id,hasDeclaredTypars,topValInfo)
    ValScheme(id,typeScheme,topValInfo,memberInfoOpt,isMutable,inlineFlag,baseOrThis,vis,compgen,false,isTyFunc,hasDeclaredTypars)

let UseCombinedArity g declKind rhsExpr prelimScheme = 
    let partialArityInfoOpt = CombineSyntacticAndInferredArities g declKind rhsExpr prelimScheme 
    BuildValScheme declKind partialArityInfoOpt prelimScheme
    
let UseNoArity prelimScheme = 
    BuildValScheme ExpressionBinding None prelimScheme

let MakeSimpleVals cenv env names =
    let tyschemes  = DontGeneralizeVals names
    let valSchemes = NameMap.map UseNoArity tyschemes
    let values     = MakeAndPublishVals cenv env (ParentNone,false,ExpressionBinding,ValNotInRecScope,valSchemes,[],XmlDoc.Empty,None)
    let vspecMap   = NameMap.map fst values
    values,vspecMap
    
let MakeAndPublishSimpleVals cenv env m names mergeNamesInOneNameresEnv =
    
    let values,vspecMap = 
        if not mergeNamesInOneNameresEnv then MakeSimpleVals cenv env names
        else
            // reason: now during typecheck we create new name resolution environment for all components of tupled arguments in lambda. 
            // When trying to find best environment for the given position first we pick the most deeply nested scope that contains given position 
            // (and that will be lambda body - correct one), then we look for the better subtree on the left hand side 
            // (and that will be name resolution environment containing second parameter parameter - without the first one).
            // fix: I've tried to make fix as local as possible to reduce overall impact on the source code. 
            // Idea of the fix: replace existing typecheck results sink and capture all reported name resolutions (this will be all parameters in lambda). 
            // After that - we restore the sink back, generate new name resolution environment that contains all captured names and report generated environment 
            // to the old sink.


            // default behavior - send EnvWithScope notification for every resolved name
            // what we do here is override this default behavior and capture only all name resolution notifications
            // later we'll process them and create one name resolution env that will contain names from all notifications
            let nameResolutions = ResizeArray()
            let values,vspecMap = 
                let sink =
                    { new ITypecheckResultsSink with
                        member this.NotifyEnvWithScope(_, _, _) = () // ignore EnvWithScope reports
                        member this.NotifyNameResolution(pos, a, b, occurence, denv, nenv, ad, m, replacing) = 
                            if not m.IsSynthetic then
                                nameResolutions.Add(pos, a, b, occurence, denv, nenv, ad, m, replacing)
                        member this.NotifyExprHasType(_, _, _, _, _, _) = assert false // no expr typings in MakeSimpleVals
                        member this.NotifyFormatSpecifierLocation _ = ()
                        member this.CurrentSource = None } 

                use _h = WithNewTypecheckResultsSink(sink, cenv.tcSink)
                MakeSimpleVals cenv env names
    
            if nameResolutions.Count <> 0 then 
                let (_, _, _, _, _, _, ad, m1, _replacing) = nameResolutions.[0]
                // mergedNameEnv - name resolution env that contains all names
                // mergedRange - union of ranges of names
                let mergedNameEnv, mergedRange = 
                    ((env.NameEnv, m1), nameResolutions) ||> Seq.fold (fun (nenv, merged) (_pos, item, _b, _occurence, _denv, _nenv, _ad, m, _) ->
                        // MakeAndPublishVal creates only Item.Value
                        let item = match item with Item.Value(item) -> item | _ -> failwith "impossible"
                        (AddFakeNamedValRefToNameEnv item.DisplayName nenv item), (unionRanges m merged)
                        )
                // send notification about mergedNameEnv
                CallEnvSink cenv.tcSink (mergedRange, mergedNameEnv, ad)
                // call CallNameResolutionSink for all captured name resolutions using mergedNameEnv
                for (_, item, b, occurence, denv, _nenv, ad, m, _replacing) in nameResolutions do
                    CallNameResolutionSink cenv.tcSink (m, mergedNameEnv, item, b, occurence, denv,  ad)

            values,vspecMap

    let envinner   = AddLocalValMap cenv.tcSink m vspecMap env
    envinner,values,vspecMap



//-------------------------------------------------------------------------
// Helpers to freshen existing types and values, i.e. when a reference
// to C<_> occurs then generate C<?ty> for a fresh type inference variable ?ty.
//------------------------------------------------------------------------- 
   
let FreshenTyconRef m rigid (tcref:TyconRef) declaredTyconTypars = 
    let tpsorig = declaredTyconTypars
    let tps = copyTypars tpsorig
    if rigid <> TyparRigidity.Rigid then 
      tps |> List.iter (fun tp -> tp.SetRigidity rigid)  
        
    let renaming,tinst = FixupNewTypars m [] [] tpsorig tps
    (TType_app(tcref,List.map mkTyparTy tpsorig), tps, renaming, TType_app(tcref,tinst))
    
let FreshenPossibleForallTy g m rigid ty = 
    let tpsorig,tau = tryDestForallTy g ty
    if isNil tpsorig then 
        [],[],tau
    else
        // tps may be have been equated to other tps in equi-recursive type inference and units-of-measure type inference. Normalize them here 
        let tpsorig = NormalizeDeclaredTyparsForEquiRecursiveInference g tpsorig
        let tps,renaming,tinst = CopyAndFixupTypars m rigid tpsorig
        tps,tinst,instType renaming tau

let infoOfTyconRef m (tcref:TyconRef) = 
    let tps,renaming,tinst = FreshenTypeInst m (tcref.Typars m)
    tps,renaming,tinst,TType_app (tcref,tinst)


/// Given a abstract method, which may be a generic method, freshen the type in preparation 
/// to apply it as a constraint to the method that implements the abstract slot 
let FreshenAbstractSlot g amap m synTyparDecls absMethInfo = 

    // Work out if an explicit instantiation has been given. If so then the explicit type 
    // parameters will be made rigid and checked for generalization. If not then auto-generalize 
    // by making the copy of the type parameters on the virtual being overriden rigid. 

    let typarsFromAbsSlotAreRigid = 
        
        match synTyparDecls with 
        | SynValTyparDecls(synTypars,infer,_) -> 
            if infer && not (isNil synTypars) then 
                errorR(Error(FSComp.SR.tcOverridingMethodRequiresAllOrNoTypeParameters(),m))

            isNil synTypars
            
    let (CompiledSig (argtys,retTy,fmtps,_)) = CompiledSigOfMeth g amap m absMethInfo
    
    // If the virual method is a generic method then copy its type parameters 
    let typarsFromAbsSlot,typarInstFromAbsSlot,_ = 
        let ttps = absMethInfo.GetFormalTyparsOfDeclaringType m 
        let ttinst = argsOfAppTy g absMethInfo.EnclosingType
        let rigid = if typarsFromAbsSlotAreRigid then TyparRigidity.Rigid else TyparRigidity.Flexible
        ConstraintSolver.FreshenAndFixupTypars m rigid ttps ttinst fmtps

    // Work out the required type of the member 
    let argTysFromAbsSlot = argtys |> List.mapSquared (instType typarInstFromAbsSlot) 
    let retTyFromAbsSlot = retTy |> GetFSharpViewOfReturnType g |> instType typarInstFromAbsSlot 
    typarsFromAbsSlotAreRigid,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot


//-------------------------------------------------------------------------
// Helpers to typecheck expressions and patterns
//------------------------------------------------------------------------- 

let BuildFieldMap cenv env isPartial ty flds m = 
    let ad = env.eAccessRights
    if isNil flds then invalidArg "flds" "BuildFieldMap"
   
    let frefSets = 
        let allFields = flds |> List.map (fun ((_,ident),_) -> ident)
        flds 
        |> List.map (fun (fld,fldExpr) ->
            let frefSet = ResolveField cenv.tcSink cenv.nameResolver env.eNameResEnv ad ty fld allFields
            fld,frefSet,fldExpr)

    let relevantTypeSets = 
        frefSets |> List.map (fun (_,frefSet,_) -> frefSet |> List.map (fun (FieldResolution(rfref,_)) -> rfref.TyconRef))
    
    let tcref = 
        match List.fold (ListSet.intersect (tyconRefEq cenv.g)) (List.head relevantTypeSets) (List.tail relevantTypeSets) with
        | [tcref] -> tcref
        | tcrefs -> 
            if isPartial then 
                warning (Error(FSComp.SR.tcFieldsDoNotDetermineUniqueRecordType(),m))

            // try finding a record type with the same number of fields as the ones that are given.
            match tcrefs |> List.tryFind (fun tc -> tc.TrueFieldsAsList.Length = flds.Length) with
            | Some tcref -> tcref            
            | _ -> 
                // OK, there isn't a unique, good type dictated by the intersection for the field refs. 
                // We're going to get an error of some kind below. 
                // Just choose one field ref and let the error come later 
                let (_,frefSet1,_) = List.head frefSets
                let (FieldResolution(fref1,_)) = List.head frefSet1
                fref1.TyconRef
    
    let fldsmap,rfldsList = 
        ((Map.empty,[]), frefSets) ||> List.fold (fun (fs,rfldsList) (fld,frefs,fldExpr) -> 
                match frefs |> List.filter (fun (FieldResolution(fref2,_)) -> tyconRefEq cenv.g tcref fref2.TyconRef) with
                | [FieldResolution(fref2,showDeprecated)] -> 

                    // Record the precise resolution of the field for intellisense
                    let item = FreshenRecdFieldRef cenv.nameResolver m fref2
                    CallNameResolutionSink cenv.tcSink ((snd fld).idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,ad)

                    CheckRecdFieldAccessible cenv.amap m env.eAccessRights fref2 |> ignore
                    CheckFSharpAttributes cenv.g fref2.PropertyAttribs m |> CommitOperationResult
                    if Map.containsKey fref2.FieldName fs then 
                        errorR (Error(FSComp.SR.tcFieldAppearsTwiceInRecord(fref2.FieldName),m))
                    if showDeprecated then
                        warning(Deprecated(FSComp.SR.nrRecordTypeNeedsQualifiedAccess(fref2.FieldName,fref2.Tycon.DisplayName) |> snd,m))
                        
                    if not (tyconRefEq cenv.g tcref fref2.TyconRef) then 
                        let (_,frefSet1,_) = List.head frefSets
                        let (FieldResolution(fref1,_)) = List.head frefSet1
                        errorR (FieldsFromDifferentTypes(env.DisplayEnv,fref1,fref2,m))
                        fs,rfldsList
                    else
                        Map.add fref2.FieldName fldExpr fs,(fref2.FieldName,fldExpr)::rfldsList

                | _ -> error(Error(FSComp.SR.tcRecordFieldInconsistentTypes(),m)))
    tcref,fldsmap,List.rev rfldsList

let rec ApplyUnionCaseOrExn (makerForUnionCase,makerForExnTag) m cenv env overallTy item =
    let ad = env.eAccessRights
    match item with 
    | Item.ExnCase ecref -> 
        CheckEntityAttributes cenv.g ecref m  |> CommitOperationResult
        UnifyTypes cenv env m overallTy cenv.g.exn_ty
        CheckTyconAccessible cenv.amap m ad ecref |> ignore
        let mkf = makerForExnTag ecref
        mkf,recdFieldTysOfExnDefRef ecref, [ for f in (recdFieldsOfExnDefRef ecref) -> f.Id ]

    | Item.UnionCase(ucinfo,showDeprecated) ->
        if showDeprecated then
            warning(Deprecated(FSComp.SR.nrUnionTypeNeedsQualifiedAccess(ucinfo.Name,ucinfo.Tycon.DisplayName) |> snd,m))
 
        let ucref = ucinfo.UnionCaseRef 
        CheckUnionCaseAttributes cenv.g ucref m |> CommitOperationResult
        CheckUnionCaseAccessible cenv.amap m ad ucref |> ignore
        let gtyp2 = actualResultTyOfUnionCase ucinfo.TypeInst ucref 
        let inst = mkTyparInst ucref.TyconRef.TyparsNoRange ucinfo.TypeInst
        UnifyTypes cenv env m overallTy gtyp2
        let mkf = makerForUnionCase(ucref,ucinfo.TypeInst)
        mkf,actualTysOfUnionCaseFields inst ucref, [ for f in ucref.AllFieldsAsList -> f.Id ]
    | _ -> invalidArg "item" "not a union case or exception reference"

let ApplyUnionCaseOrExnTypes m cenv env overallTy c = 
  ApplyUnionCaseOrExn ((fun (a,b) mArgs args -> mkUnionCaseExpr(a,b,args,unionRanges m mArgs)),
                       (fun a mArgs args -> mkExnExpr (a,args,unionRanges m mArgs))) m cenv env overallTy c
      
let ApplyUnionCaseOrExnTypesForPat m cenv env overallTy c = 
  ApplyUnionCaseOrExn ((fun (a,b) mArgs args -> TPat_unioncase(a,b,args,unionRanges m mArgs)),
                       (fun a mArgs args -> TPat_exnconstr(a,args,unionRanges m mArgs))) m cenv env overallTy c

let UnionCaseOrExnCheck (env: TcEnv) nargtys nargs m =
  if nargs <> nargtys then error (UnionCaseWrongArguments(env.DisplayEnv,nargtys,nargs,m))

let TcUnionCaseOrExnField cenv (env: TcEnv) ty1 m c n funcs =
    let ad = env.eAccessRights
    let mkf,argtys, _argNames = 
      match ResolvePatternLongIdent cenv.tcSink cenv.nameResolver AllIdsOK false m ad env.eNameResEnv TypeNameResolutionInfo.Default c with
      | (Item.UnionCase _ | Item.ExnCase _) as item ->
        ApplyUnionCaseOrExn funcs m cenv env ty1 item
      | _ -> error(Error(FSComp.SR.tcUnknownUnion(),m))
    if n >= List.length argtys then 
      error (UnionCaseWrongNumberOfArgs(env.DisplayEnv,List.length argtys,n,m))
    let ty2 = List.item n argtys
    mkf,ty2

//-------------------------------------------------------------------------
// Environment of explicit type parameters, e.g. 'a in "(x : 'a)"
//------------------------------------------------------------------------- 

type SyntacticUnscopedTyparEnv = UnscopedTyparEnv of NameMap<Typar>

let emptyUnscopedTyparEnv : SyntacticUnscopedTyparEnv = UnscopedTyparEnv Map.empty

let AddUnscopedTypar n p (UnscopedTyparEnv tab) = UnscopedTyparEnv (Map.add n p tab)

let TryFindUnscopedTypar n (UnscopedTyparEnv tab) = Map.tryFind n tab

let HideUnscopedTypars typars (UnscopedTyparEnv tab) = 
    UnscopedTyparEnv (List.fold (fun acc (tp:Typar) -> Map.remove tp.Name acc) tab typars)

//-------------------------------------------------------------------------
// Helpers for generalizing type variables
//------------------------------------------------------------------------- 

type GeneralizeConstrainedTyparOptions = 
    | CanGeneralizeConstrainedTypars 
    | DoNotGeneralizeConstrainedTypars


module GeneralizationHelpers = 
    let ComputeUngeneralizableTypars env = 
        
        // This is just a List.fold. Unfolded here to enable better profiling 
        let rec loop acc (items: UngeneralizableItem list) =
             match items with 
             | [] -> acc
             | item::rest -> 
                 let acc = 
                     if item.WillNeverHaveFreeTypars then 
                         acc 
                     else
                         let ftps = item.GetFreeTyvars().FreeTypars
                         if ftps.IsEmpty then 
                             acc 
                         else 
                             // These union operations are a performance sore point
                             unionFreeTypars ftps acc
                 loop acc rest

        loop emptyFreeTypars env.eUngeneralizableItems 

    let ComputeUnabstractableTycons env = 
        let acc_in_free_item acc (item: UngeneralizableItem) = 
            let ftycs = 
                if item.WillNeverHaveFreeTypars then item.CachedFreeLocalTycons else 
                let ftyvs = item.GetFreeTyvars()
                ftyvs.FreeTycons
            if ftycs.IsEmpty then acc else unionFreeTycons ftycs acc

        List.fold acc_in_free_item emptyFreeTycons env.eUngeneralizableItems 

    let ComputeUnabstractableTraitSolutions env = 
        let acc_in_free_item acc (item: UngeneralizableItem) = 
            let ftycs = 
                if item.WillNeverHaveFreeTypars then item.CachedFreeTraitSolutions else 
                let ftyvs = item.GetFreeTyvars()
                ftyvs.FreeTraitSolutions
            if ftycs.IsEmpty then acc else unionFreeLocals ftycs acc

        List.fold acc_in_free_item emptyFreeLocals env.eUngeneralizableItems 

    let rec IsGeneralizableValue g t = 
        match t with 
        | Expr.Lambda _ | Expr.TyLambda _ | Expr.Const _ | Expr.Val _ -> true

        // Look through coercion nodes corresponding to introduction of subsumption 
        | Expr.Op(TOp.Coerce,[inputTy;actualTy],[e1],_) when isFunTy g actualTy && isFunTy g inputTy -> 
            IsGeneralizableValue g e1

        | Expr.Op(op,_,args,_) ->
            match op with 
            | TOp.Tuple _ -> true
            | TOp.UnionCase uc -> not (isUnionCaseRefAllocObservable uc)
            | TOp.Recd(ctorInfo,tcref) -> 
                match ctorInfo with 
                | RecdExpr -> not (isRecdOrUnionOrStructTyconRefAllocObservable g tcref)
                | RecdExprIsObjInit -> false
            | TOp.Array -> isNil args
            | TOp.ExnConstr ec -> not (isExnAllocObservable ec)

            | TOp.ILAsm([],_) -> true

            | _ -> false
            && List.forall (IsGeneralizableValue g) args

        | Expr.LetRec(binds,body,_,_)  ->
            binds |> List.forall (fun b -> IsGeneralizableValue g b.Expr) &&
            IsGeneralizableValue g body
        | Expr.Let(bind,body,_,_) -> 
            IsGeneralizableValue g bind.Expr &&
            IsGeneralizableValue g body

        // Applications of type functions are _not_ normally generalizable unless explicitly marked so 
        | Expr.App(Expr.Val (vref,_,_),_,_,[],_) when vref.IsTypeFunction -> 
            HasFSharpAttribute g g.attrib_GeneralizableValueAttribute vref.Attribs
        
        | Expr.App(e1,_,_,[],_) -> IsGeneralizableValue g e1
        | Expr.TyChoose(_,b,_) -> IsGeneralizableValue g b
        | Expr.Obj (_,ty,_,_,_,_,_) -> isInterfaceTy g ty || isDelegateTy g ty
        | Expr.Link eref -> IsGeneralizableValue g !eref

        | _ -> false  

    let CanGeneralizeConstrainedTyparsForDecl declKind = 
        if DeclKind.CanGeneralizeConstrainedTypars declKind 
        then CanGeneralizeConstrainedTypars 
        else DoNotGeneralizeConstrainedTypars
        
    /// Recursively knock out typars we can't generalize. 
    /// For non-generalized type variables be careful to iteratively knock out 
    /// both the typars and any typars free in the constraints of the typars
    /// into the set that are considered free in the environment. 
    let rec TrimUngeneralizableTypars genConstrainedTyparFlag inlineFlag (generalizedTypars:Typar list) freeInEnv = 
        // Do not generalize type variables with a static requirement unless function is marked 'inline' 
        let generalizedTypars,ungeneralizableTypars1 =  
            if inlineFlag = ValInline.PseudoVal then generalizedTypars,[]
            else generalizedTypars |> List.partition (fun tp -> tp.StaticReq = NoStaticReq) 

        // Do not generalize type variables which would escape their scope 
        // because they are free in the environment 
        let generalizedTypars,ungeneralizableTypars2 = 
            List.partition (fun x -> not (Zset.contains x freeInEnv)) generalizedTypars

        // Some situations, e.g. implicit class constructions that represent functions as fields, 
        // do not allow generalisation over constrained typars. (since they can not be represented as fields)
        let generalizedTypars,ungeneralizableTypars3 = 
            generalizedTypars 
            |> List.partition (fun tp -> 
                genConstrainedTyparFlag = CanGeneralizeConstrainedTypars || 
                tp.Constraints.IsEmpty) 

        if isNil ungeneralizableTypars1 && isNil ungeneralizableTypars2 && isNil ungeneralizableTypars3 then
            generalizedTypars, freeInEnv
        else 
            let freeInEnv = 
                unionFreeTypars 
                    (accFreeInTypars CollectAllNoCaching ungeneralizableTypars1 
                        (accFreeInTypars CollectAllNoCaching ungeneralizableTypars2 
                            (accFreeInTypars CollectAllNoCaching ungeneralizableTypars3 emptyFreeTyvars))).FreeTypars 
                    freeInEnv
            TrimUngeneralizableTypars genConstrainedTyparFlag inlineFlag generalizedTypars freeInEnv

    /// Condense type variables in positive position
    let CondenseTypars (cenv, denv:DisplayEnv, generalizedTypars: Typars, tauTy, m) =

        // The type of the value is ty11 * ... * ty1N -> ... -> tyM1 * ... * tyMM -> retTy
        // This is computed REGARDLESS of the arity of the expression.
        let curriedArgTys,retTy = stripFunTy cenv.g tauTy
        let allUntupledArgTys = curriedArgTys |> List.collect (tryDestRefTupleTy cenv.g)

        // Compute the type variables in 'retTy'
        let returnTypeFreeTypars = freeInTypeLeftToRight cenv.g false retTy
        let allUntupledArgTysWithFreeVars = allUntupledArgTys |> List.map (fun ty -> (ty, freeInTypeLeftToRight cenv.g false ty))

        let relevantUniqueSubtypeConstraint (tp:Typar) = 
            // Find a single subtype constraint
            match tp.Constraints |> List.partition (function (TyparConstraint.CoercesTo _) -> true | _ -> false) with 
            | [TyparConstraint.CoercesTo(cxty,_)], others -> 
                 // Throw away null constraints if they are implied 
                 if others |> List.exists (function (TyparConstraint.SupportsNull(_)) -> not (TypeSatisfiesNullConstraint cenv.g m cxty) | _ -> true) 
                 then None
                 else Some cxty
            | _ -> None
                 

        // Condensation typars can't be used in the constraints of any candidate condensation typars. So compute all the
        // typars free in the constraints of tyIJ

        let lhsConstraintTypars = 
            allUntupledArgTys |> List.collect (fun ty -> 
                match tryDestTyparTy cenv.g ty with
                | Some tp ->
                    match relevantUniqueSubtypeConstraint tp with 
                    | Some cxty -> freeInTypeLeftToRight cenv.g false cxty
                    | None -> []
                | None -> [])

        let IsCondensationTypar (tp:Typar) = 
            // A condensation typar may not a user-generated type variable nor has it been unified with any user type variable
            (tp.DynamicReq = TyparDynamicReq.No) && 
            // A condensation typar must have a single constraint "'a :> A"
            (Option.isSome (relevantUniqueSubtypeConstraint tp)) &&
            // This is type variable is not used on the r.h.s. of the type
            not (ListSet.contains typarEq tp returnTypeFreeTypars) &&
            // A condensation typar can't be used in the constraints of any candidate condensation typars
            not (ListSet.contains typarEq tp lhsConstraintTypars) &&
            // A condensation typar must occur precisely once in tyIJ, and must not occur free in any other tyIJ
            (match allUntupledArgTysWithFreeVars |> List.partition (fun (ty,_) -> match tryDestTyparTy cenv.g ty with Some destTypar -> typarEq destTypar tp | _ -> false) with
             | [_], rest -> not (rest |> List.exists (fun (_,fvs) -> ListSet.contains typarEq tp fvs))
             | _ -> false)
             
        let condensationTypars, generalizedTypars = generalizedTypars |> List.partition IsCondensationTypar

        // Condensation solves type variables eagerly and removes them from the generalization set 
        condensationTypars |> List.iter (fun tp -> 
            ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css denv tp)
        generalizedTypars

    let CanonicalizePartialInferenceProblem (cenv,denv,m) tps =
        // Canonicalize constraints prior to generalization 
        let csenv = (MakeConstraintSolverEnv ContextInfo.NoContext cenv.css m denv)
        TryD (fun () -> ConstraintSolver.CanonicalizeRelevantMemberConstraints csenv 0 NoTrace tps)
             (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m))) 
        |> RaiseOperationResult

    let ComputeAndGeneralizeGenericTypars (cenv,
                                           denv:DisplayEnv,
                                           m,
                                           freeInEnv:FreeTypars,
                                           canInferTypars,
                                           genConstrainedTyparFlag,
                                           inlineFlag,
                                           exprOpt,
                                           allDeclaredTypars: Typars,
                                           maxInferredTypars: Typars,
                                           tauTy,
                                           resultFirst) =

        let allDeclaredTypars = NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g allDeclaredTypars
        let typarsToAttemptToGeneralize = 
            if (match exprOpt with None -> true | Some e -> IsGeneralizableValue cenv.g e) 
            then (ListSet.unionFavourLeft typarEq allDeclaredTypars maxInferredTypars)
            else allDeclaredTypars

        let generalizedTypars,freeInEnv = 
            TrimUngeneralizableTypars genConstrainedTyparFlag inlineFlag typarsToAttemptToGeneralize freeInEnv

        allDeclaredTypars 
        |> List.iter (fun tp -> 
            if Zset.memberOf freeInEnv tp then
                let ty = mkTyparTy tp
                error(Error(FSComp.SR.tcNotSufficientlyGenericBecauseOfScope(NicePrint.prettyStringOfTy denv ty),m)))
            
        let generalizedTypars = CondenseTypars(cenv, denv, generalizedTypars, tauTy, m)    

        let generalizedTypars =  
            if canInferTypars then generalizedTypars 
            else generalizedTypars |> List.filter (fun tp -> ListSet.contains typarEq tp allDeclaredTypars)

        let allConstraints = List.collect (fun (tp:Typar) -> tp.Constraints) generalizedTypars 
        let generalizedTypars = ConstraintSolver.SimplifyMeasuresInTypeScheme cenv.g resultFirst generalizedTypars tauTy allConstraints

        // Generalization turns inference type variables into rigid, quantified type variables,
        // (they may be rigid already)
        generalizedTypars |> List.iter (SetTyparRigid cenv.g denv m)
        
        // Generalization removes constraints related to generalized type variables
        let csenv = MakeConstraintSolverEnv ContextInfo.NoContext cenv.css m denv
        EliminateConstraintsForGeneralizedTypars csenv NoTrace generalizedTypars
        
        generalizedTypars

    //-------------------------------------------------------------------------
    // Helpers to freshen existing types and values, i.e. when a reference
    // to C<_> occurs then generate C<?ty> for a fresh type inference variable ?ty.
    //------------------------------------------------------------------------- 



    let CheckDeclaredTyparsPermitted (memFlagsOpt, declaredTypars, m) = 
        match memFlagsOpt with 
        | None -> ()
        | Some memberFlags -> 
            match memberFlags.MemberKind with 
            // can't infer extra polymorphism for properties 
            | MemberKind.PropertyGet 
            | MemberKind.PropertySet -> 
                 if not (isNil declaredTypars) then 
                     errorR(Error(FSComp.SR.tcPropertyRequiresExplicitTypeParameters(),m))
            | MemberKind.Constructor -> 
                 if not (isNil declaredTypars) then 
                     errorR(Error(FSComp.SR.tcConstructorCannotHaveTypeParameters(),m))
            | _ -> ()

    /// Properties and Constructors may only generalize the variables associated with the containing class (retrieved from the 'this' pointer) 
    /// Also check they don't declare explicit typars. 
    let ComputeCanInferExtraGeneralizableTypars (parentRef, canInferTypars, memFlagsOpt) =  
        canInferTypars &&
        (match memFlagsOpt with 
         | None -> true
         | Some memberFlags -> 
            match memberFlags.MemberKind with 
            // can't infer extra polymorphism for properties 
            | MemberKind.PropertyGet | MemberKind.PropertySet -> false
            // can't infer extra polymorphism for class constructors 
            | MemberKind.ClassConstructor ->  false
            // can't infer extra polymorphism for constructors 
            | MemberKind.Constructor -> false
            // feasible to infer extra polymorphism 
            | _ -> true) &&
        (match parentRef with 
         | Parent tcref -> not tcref.IsFSharpDelegateTycon 
         | _ -> true) // no generic paramters inferred for 'Invoke' method

        

//-------------------------------------------------------------------------
// ComputeInlineFlag 
//-------------------------------------------------------------------------

let ComputeInlineFlag memFlagsOption isInline isMutable m = 
    let inlineFlag = 
        // Mutable values may never be inlined 
        // Constructors may never be inlined 
        // Calls to virtual/abstract slots may never be inlined 
        if isMutable || 
           (match memFlagsOption with 
            | None -> false
            | Some x -> (x.MemberKind = MemberKind.Constructor) || x.IsDispatchSlot || x.IsOverrideOrExplicitImpl) 
        then ValInline.Never 
        elif isInline then ValInline.PseudoVal 
        else ValInline.Optional
    if isInline && (inlineFlag <> ValInline.PseudoVal) then 
        errorR(Error(FSComp.SR.tcThisValueMayNotBeInlined(),m))
    inlineFlag


//-------------------------------------------------------------------------
// Binding normalization.
//
// Determine what sort of value is being bound (normal value, instance
// member, normal function, static member etc.) and make some
// name-resolution-sensitive adjustments to the syntax tree.
//
// One part of this "normalization" ensures: 
//        "let SynPat.LongIdent(f) = e" when f not a datatype constructor --> let Pat_var(f) = e" 
//        "let SynPat.LongIdent(f) pat = e" when f not a datatype constructor --> let Pat_var(f) = \pat. e" 
//        "let (SynPat.LongIdent(f) : ty) = e" when f not a datatype constructor --> let (Pat_var(f) : ty) = e" 
//        "let (SynPat.LongIdent(f) : ty) pat = e" when f not a datatype constructor --> let (Pat_var(f) : ty) = \pat. e" 
// 
// This is because the first lambda in a function definition "let F x = e" 
// now looks like a constructor application, i.e. let (F x) = e ... 
//  also let A.F x = e ... 
//  also let f x = e ... 
//
// The other parts turn property definitions into method definitions.
//------------------------------------------------------------------------- 

 
// NormalizedBindingRhs records the r.h.s. of a binding after some munging just before type checking.
// NOTE: This is a bit of a mess.  In the early implementation of F# we decided 
// to have the parser convert "let f x = e" into 
// "let f = fun x -> e".  This is called "pushing" a pattern across to the right hand side. Complex 
// patterns (e.g. non-tuple patterns) result in a computation on the right. 
// However, this approach really isn't that great - especially since 
// the language is now considerably more complex, e.g. we use 
// type information from the first (but not the second) form in 
// type inference for recursive bindings, and the first form 
// may specify .NET attributes for arguments. There are still many 
// relics of this approach around, e.g. the expression in BindingRhs 
// below is of the second form. However, to extract relevant information 
// we keep a record of the pats and optional explicit return type already pushed 
// into expression so we can use any user-given type information from these 
type NormalizedBindingRhs = 
    | NormalizedBindingRhs of SynSimplePats list * SynBindingReturnInfo option * SynExpr 

let PushOnePatternToRhs (cenv:cenv) isMember p (NormalizedBindingRhs(spatsL,rtyOpt,rhsExpr)) = 
    let spats,rhsExpr = PushPatternToExpr cenv.synArgNameGenerator isMember p rhsExpr
    NormalizedBindingRhs(spats::spatsL, rtyOpt,rhsExpr)

type NormalizedBindingPatternInfo = 
    NormalizedBindingPat of SynPat * NormalizedBindingRhs * SynValData * SynValTyparDecls 


/// Represents a syntactic, unchecked binding after the resolution of the name resolution status of pattern
/// constructors and after "pushing" all complex patterns to the right hand side.
type NormalizedBinding = 
  | NormalizedBinding of 
      SynAccess option *
      SynBindingKind *
      bool *  (* pesudo/mustinline value? *)
      bool *  (* mutable *)
      SynAttributes * 
      XmlDoc *
      SynValTyparDecls * 
      SynValData * 
      SynPat * 
      NormalizedBindingRhs *
      range *
      SequencePointInfoForBinding


type IsObjExprBinding = 
    | ObjExprBinding 
    | ValOrMemberBinding



module BindingNormalization =
    /// Push a bunch of pats at once. They may contain patterns, e.g. let f (A x) (B y) = ... 
    /// In this case the sematnics is let f a b = let A x = a in let B y = b 
    let private PushMultiplePatternsToRhs (cenv:cenv) isMember ps (NormalizedBindingRhs(spatsL,rtyOpt,rhsExpr)) = 
        let spatsL2,rhsExpr = PushCurriedPatternsToExpr cenv.synArgNameGenerator rhsExpr.Range isMember ps rhsExpr
        NormalizedBindingRhs(spatsL2@spatsL, rtyOpt, rhsExpr)


    let private MakeNormalizedStaticOrValBinding cenv isObjExprBinding id vis typars args rhsExpr valSynData = 
        let (SynValData(memberFlagsOpt,_,_)) = valSynData 
        NormalizedBindingPat(mkSynPatVar vis id, PushMultiplePatternsToRhs cenv ((isObjExprBinding = ObjExprBinding) || Option.isSome memberFlagsOpt) args rhsExpr,valSynData,typars)

    let private MakeNormalizedInstanceMemberBinding cenv thisId memberId toolId vis m typars args rhsExpr valSynData = 
        NormalizedBindingPat(SynPat.InstanceMember(thisId,memberId,toolId,vis,m), PushMultiplePatternsToRhs cenv true args rhsExpr,valSynData,typars)

    let private NormalizeStaticMemberBinding cenv memberFlags valSynData id vis typars args m rhsExpr = 
        let (SynValData(_,valSynInfo,thisIdOpt)) = valSynData 
        if memberFlags.IsInstance then 
            // instance method without adhoc "this" argument 
            error(Error(FSComp.SR.tcInstanceMemberRequiresTarget(),m))
        match args, memberFlags.MemberKind  with 
        | _,MemberKind.PropertyGetSet    -> error(Error(FSComp.SR.tcUnexpectedPropertyInSyntaxTree(),m))
        | [],MemberKind.ClassConstructor -> error(Error(FSComp.SR.tcStaticInitializerRequiresArgument(),m))
        | [],MemberKind.Constructor     -> error(Error(FSComp.SR.tcObjectConstructorRequiresArgument(),m))
        | [_],MemberKind.ClassConstructor  
        | [_],MemberKind.Constructor  -> MakeNormalizedStaticOrValBinding cenv ValOrMemberBinding id vis typars args rhsExpr valSynData
        // Static property declared using 'static member P = expr': transformed to a method taking a "unit" argument 
        // static property: these transformed into methods taking one "unit" argument 
        | [],MemberKind.Member -> 
            let memberFlags = {memberFlags with MemberKind = MemberKind.PropertyGet} 
            let valSynData = SynValData(Some memberFlags,valSynInfo,thisIdOpt)
            NormalizedBindingPat(mkSynPatVar vis id,
                                 PushOnePatternToRhs cenv true (SynPat.Const(SynConst.Unit,m)) rhsExpr,
                                 valSynData,
                                 typars)
        | _ -> MakeNormalizedStaticOrValBinding cenv ValOrMemberBinding id vis typars args rhsExpr valSynData

    let private NormalizeInstanceMemberBinding cenv memberFlags valSynData thisId memberId (toolId:Ident option) vis typars args m rhsExpr = 
        let (SynValData(_,valSynInfo,thisIdOpt)) = valSynData 
        if not memberFlags.IsInstance then 
            // static method with adhoc "this" argument 
            error(Error(FSComp.SR.tcStaticMemberShouldNotHaveThis(),m))
        match args, memberFlags.MemberKind  with 
        | _,MemberKind.ClassConstructor  -> error(Error(FSComp.SR.tcExplicitStaticInitializerSyntax(),m))
        | _,MemberKind.Constructor  -> error(Error(FSComp.SR.tcExplicitObjectConstructorSyntax(),m))
        | _,MemberKind.PropertyGetSet  -> error(Error(FSComp.SR.tcUnexpectedPropertySpec(),m))
        // Instance property declared using 'x.Member': transformed to methods taking a "this" and a "unit" argument 
        // We push across the 'this' arg in mk_rec_binds 
        | [],MemberKind.Member -> 
            let memberFlags = {memberFlags with MemberKind = MemberKind.PropertyGet}
            NormalizedBindingPat
                (SynPat.InstanceMember(thisId,memberId,toolId,vis,m), 
                 PushOnePatternToRhs cenv true (SynPat.Const(SynConst.Unit,m)) rhsExpr,
                 // Update the member info to record that this is a MemberKind.PropertyGet 
                 SynValData(Some memberFlags,valSynInfo,thisIdOpt),
                 typars)

        | _ -> MakeNormalizedInstanceMemberBinding cenv thisId memberId toolId vis m typars args rhsExpr valSynData

    let private NormalizeBindingPattern cenv nameResolver isObjExprBinding (env: TcEnv) valSynData pat rhsExpr =
        let ad = env.eAccessRights
        let (SynValData(memberFlagsOpt,_,_)) = valSynData 
        let rec normPattern pat = 
            // One major problem with versions of F# prior to 1.9.x was that data constructors easily 'pollute' the namespace 
            // of available items, to the point that you can't even define a function with the same name as an existing union case. 
            match pat with 
            | SynPat.FromParseError(p,_) -> normPattern p
            | SynPat.LongIdent (LongIdentWithDots(longId,_), toolId, tyargs, SynConstructorArgs.Pats args, vis, m) ->
                let typars = match tyargs with None -> inferredTyparDecls | Some typars -> typars
                match memberFlagsOpt with 
                | None ->                
                    match ResolvePatternLongIdent cenv.tcSink nameResolver AllIdsOK true m ad env.eNameResEnv TypeNameResolutionInfo.Default longId with
                    | Item.NewDef id -> 
                        if id.idText = opNameCons  then
                            NormalizedBindingPat(pat,rhsExpr,valSynData,typars)
                        else
                            if isObjExprBinding = ObjExprBinding then 
                                errorR(Deprecated(FSComp.SR.tcObjectExpressionFormDeprecated(),m))
                            MakeNormalizedStaticOrValBinding cenv isObjExprBinding id vis typars args rhsExpr valSynData
                    | _ -> 
                        error(Error(FSComp.SR.tcInvalidDeclaration(),m))

                | Some memberFlags ->                
                    match longId with 
                    // x.Member in member binding patterns. 
                    | [thisId;memberId] -> NormalizeInstanceMemberBinding cenv memberFlags valSynData thisId memberId toolId vis typars args m rhsExpr 
                    | [memberId]        -> NormalizeStaticMemberBinding cenv memberFlags valSynData memberId vis typars args m rhsExpr 
                    | _                 -> NormalizedBindingPat(pat,rhsExpr,valSynData,typars)

            // Object constructors are normalized in TcLetrec 
            // Here we are normalizing member definitions with simple (not long) ids, 
            // e.g. "static member x = 3" and "member x = 3" (instance with missing "this." comes through here. It is trapped and generates a warning) 
            | SynPat.Named (SynPat.Wild _, id, false, vis, m) 
                when 
                   (match memberFlagsOpt with 
                    | None -> false 
                    | Some memberFlags -> 
                         not (memberFlags.MemberKind = MemberKind.Constructor) &&
                         not (memberFlags.MemberKind = MemberKind.ClassConstructor)) ->            
                NormalizeStaticMemberBinding cenv (Option.get memberFlagsOpt) valSynData id vis inferredTyparDecls [] m rhsExpr 

            | SynPat.Typed(pat',x,y) ->             
                let (NormalizedBindingPat(pat'',e'',valSynData,typars)) = normPattern pat'
                NormalizedBindingPat(SynPat.Typed(pat'',x,y), e'',valSynData,typars)

            | SynPat.Attrib(_,_,m) ->             
                error(Error(FSComp.SR.tcAttributesInvalidInPatterns(),m))

            | _ ->
                NormalizedBindingPat(pat,rhsExpr,valSynData,inferredTyparDecls) 
        normPattern pat

    let NormalizeBinding isObjExprBinding cenv (env: TcEnv) b = 
        match b with 
        | Binding (vis,bkind,isInline,isMutable,attrs,doc,valSynData,p,retInfo,rhsExpr,mBinding,spBind) ->
            let (NormalizedBindingPat(pat,rhsExpr,valSynData,typars)) = 
                NormalizeBindingPattern cenv cenv.nameResolver isObjExprBinding env valSynData p (NormalizedBindingRhs ([], retInfo, rhsExpr))
            NormalizedBinding(vis,bkind,isInline,isMutable,attrs,doc.ToXmlDoc(),typars,valSynData,pat,rhsExpr,mBinding,spBind)

//-------------------------------------------------------------------------
// input is:
//    [<CompileAsEvent>]
//    member x.P with get = fun () -> e
// --> 
//    member x.add_P< >(argName) = (e).AddHandler(argName)
//    member x.remove_P< >(argName) = (e).RemoveHandler(argName)

module EventDeclarationNormalization = 
    let ConvertSynInfo m (SynValInfo(argInfos,retInfo)) = 
       // reconstitute valSynInfo by adding the argument
       let argInfos = 
           match argInfos with 
           | [[thisArgInfo];[]] ->  [[thisArgInfo];SynInfo.unnamedTopArg] // instance property getter
           | [[]] -> [SynInfo.unnamedTopArg] // static property getter
           | _ -> error(BadEventTransformation(m))

       // reconstitute valSynInfo
       SynValInfo(argInfos,retInfo)

    // THe property x.P becomes methods x.add_P and x.remove_P
    let ConvertMemberFlags memberFlags = { memberFlags with MemberKind = MemberKind.Member } 

    let private ConvertMemberFlagsOpt m memberFlagsOpt =
        match memberFlagsOpt with 
        | Some memberFlags -> Some (ConvertMemberFlags memberFlags)
        | _ -> error(BadEventTransformation(m))

    let private ConvertSynData m valSynData =
        let (SynValData(memberFlagsOpt,valSynInfo,thisIdOpt)) = valSynData 
        let memberFlagsOpt = ConvertMemberFlagsOpt m memberFlagsOpt
        let valSynInfo = ConvertSynInfo m valSynInfo
        SynValData(memberFlagsOpt,valSynInfo,thisIdOpt)
     
    let rec private RenameBindingPattern f declPattern = 
        match declPattern with  
        | SynPat.FromParseError(p,_) -> RenameBindingPattern f p
        | SynPat.Typed(pat',_,_) -> RenameBindingPattern f pat'
        | SynPat.Named (SynPat.Wild m1, id,x2,vis2,m) -> SynPat.Named (SynPat.Wild m1, ident(f id.idText,id.idRange) ,x2,vis2,m) 
        | SynPat.InstanceMember(thisId,id,toolId,vis2,m) -> SynPat.InstanceMember(thisId,ident(f id.idText,id.idRange),toolId,vis2,m)
        | _ -> error(Error(FSComp.SR.tcOnlySimplePatternsInLetRec(),declPattern.Range))

    /// Some F# bindings syntactically imply additional bindings, notably properties
    /// annotated with [<CLIEvent>]
    let GenerateExtraBindings cenv (bindingAttribs,binding) =
        let (NormalizedBinding(vis1, bindingKind, isInline, isMutable, _, bindingXmlDoc, _synTyparDecls, valSynData, declPattern, bindingRhs, mBinding, spBind)) = binding
        if CompileAsEvent cenv.g bindingAttribs then 

            let MakeOne (prefix,target) = 
                let declPattern = RenameBindingPattern (fun s -> prefix^s) declPattern
                let argName = "handler"
                // modify the rhs and argument data
                let bindingRhs,valSynData = 
                   let (NormalizedBindingRhs(_,_,rhsExpr)) = bindingRhs
                   let m = rhsExpr.Range
                   // reconstitute valSynInfo by adding the argument
                   let valSynData = ConvertSynData m valSynData

                   match rhsExpr with 
                   // Detect 'fun () -> e' which results from the compilation of a property getter
                   | SynExpr.Lambda (_,_,SynSimplePats.SimplePats([],_), trueRhsExpr,m) ->
                       let rhsExpr = mkSynApp1 (SynExpr.DotGet(SynExpr.Paren(trueRhsExpr,range0,None,m),range0,LongIdentWithDots([ident(target,m)],[]),m)) (SynExpr.Ident(ident(argName,m))) m
                       
                       // reconstitute rhsExpr
                       let bindingRhs = NormalizedBindingRhs([],None,rhsExpr)

                       // add the argument to the expression 
                       let bindingRhs = PushOnePatternToRhs cenv true (mkSynPatVar None (ident (argName,mBinding))) bindingRhs 
                       
                       bindingRhs,valSynData
                   | _ -> 
                       error(BadEventTransformation(m))

                // reconstitute the binding
                NormalizedBinding(vis1,bindingKind,isInline,isMutable,[],bindingXmlDoc,noInferredTypars,valSynData,declPattern,bindingRhs,mBinding,spBind) 

            [ MakeOne ("add_","AddHandler"); MakeOne ("remove_","RemoveHandler") ]
        else 
            []



/// Make a copy of the "this" type for a generic object type, e.g. List<'T> --> List<'?> for a fresh inference variable.
/// Also adjust the "this" type to take into account whether the type is a struct.
let FreshenObjectArgType cenv m rigid tcref isExtrinsic declaredTyconTypars = 
#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
    let tcrefObjTy,enclosingDeclaredTypars,renaming,objTy = FreshenTyconRef m (if isExtrinsic then TyparRigidity.Flexible else rigid) tcref declaredTyconTypars
#else
    let tcrefObjTy,enclosingDeclaredTypars,renaming,objTy = FreshenTyconRef m rigid tcref declaredTyconTypars
#endif
    // Struct members have a byref 'this' type (unless they are extrinsic extension members)
    let thisTy = 
        if tcref.IsStructOrEnumTycon && not isExtrinsic then 
            mkByrefTy cenv.g objTy 
        else 
            objTy
    tcrefObjTy,enclosingDeclaredTypars,renaming,objTy,thisTy


// The early generalization rule of F# 2.0 can be unsound for members in generic types (Bug DevDiv2 10649).
// It gives rise to types like "Forall T. ?X -> ?Y" where ?X and ?Y are later discovered to involve T. 
//
// For example:
//      type C<'T>() = 
//          let mutable x = Unchecked.defaultof<_> // unknown inference variable ?X
//          static member A() = x 
//                     // At this point A is generalized early to "Forall T. unit -> ?X"
//          static member B1() = C<string>.A() 
//                     // At this point during type inference, the return type of C<string>.A() is '?X'
//                     // After type inference,  the return type of C<string>.A() is 'string'
//          static member B2() = C<int>.A() 
//                     // At this point during type inference, the return type of C<int>.A() is '?X'
//                     // After type inference,  the return type of C<int>.A() is 'int'
//          member this.C() = (x : 'T)
//                     // At this point during type inference the type of 'x' is inferred to be 'T'
//
// Here "A" is generalized too early. 
//
// Ideally we would simply generalize "A" later, when it is known to be
// sound. However, that can lead to other problems (e.g. some programs that typecheck today would no longer
// be accepted). As a result, we deal with this unsoundness by an adhoc post-type-checking
// consistency check for recursive uses of "A" with explicit instantiations within the recursive 
// scope of "A".
let TcValEarlyGeneralizationConsistencyCheck cenv (env:TcEnv) (v:Val, vrec, tinst, vty, tau, m) =
    match vrec with 
    | ValInRecScope isComplete when isComplete && not (isNil tinst) ->
        //printfn "pushing post-inference check for '%s', vty = '%s'" v.DisplayName (DebugPrint.showType vty)
        cenv.postInferenceChecks.Add (fun () -> 
            //printfn "running post-inference check for '%s'" v.DisplayName
            //printfn "tau = '%s'" (DebugPrint.showType tau)
            //printfn "vty = '%s'" (DebugPrint.showType vty)
            let tpsorig,tau2 =  tryDestForallTy cenv.g vty
            //printfn "tau2 = '%s'" (DebugPrint.showType tau2)
            if not (isNil tpsorig) then 
              let tpsorig = NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g tpsorig
              let tau3 = instType (mkTyparInst tpsorig tinst) tau2
              //printfn "tau3 = '%s'" (DebugPrint.showType tau3)
              if not (AddCxTypeEqualsTypeUndoIfFailed env.DisplayEnv cenv.css m tau tau3) then
                  let txt = bufs (fun buf -> NicePrint.outputQualifiedValSpec env.DisplayEnv buf v)
                  error(Error(FSComp.SR.tcInferredGenericTypeGivesRiseToInconsistency(v.DisplayName, txt),m)))
    | _ -> ()


/// TcVal. "Use" a value, normally at a fresh type instance (unless optInst is
/// given). optInst is set when an explicit type instantiation is given, e.g. 
///     Seq.empty<string>
/// In this case the vrefFlags inside optInst are just NormalValUse.
///
/// optInst is is also set when building the final call for a reference to an
/// F# object model member, in which case the optInst is the type instantiation
/// inferred by member overload resolution, and vrefFlags indicate if the
/// member is being used in a special way, i.e. may be one of:
///    | CtorValUsedAsSuperInit    "inherit Panel()"
///    | CtorValUsedAsSelfInit     "new() = new OwnType(3)"
///    | VSlotDirectCall           "base.OnClick(eventArgs)"
let TcVal checkAttributes cenv env tpenv (vref:ValRef) optInst m =
    let v = vref.Deref
    let vrec = v.RecursiveValInfo
    v.SetHasBeenReferenced() 
    CheckValAccessible m env.eAccessRights vref
    if checkAttributes then 
        CheckValAttributes cenv.g vref m  |> CommitOperationResult
    let vty = vref.Type
    // byref-typed values get dereferenced 
    if isByrefTy cenv.g vty then 
        let isSpecial = true
        mkAddrGet m vref, isSpecial, destByrefTy cenv.g vty, [], tpenv
    else 
      match v.LiteralValue with 
      | Some c -> 
          // Literal values go to constants 
          let isSpecial = true
          // The value may still be generic, e.g. 
          //   [<Literal>]
          //   let Null = null
          let _,tinst,tau = FreshenPossibleForallTy cenv.g m TyparRigidity.Flexible vty 
          Expr.Const(c,m,tau),isSpecial,tau,tinst,tpenv

      | None -> 
            // References to 'this' in classes get dereferenced from their implicit reference cell and poked
          if v.BaseOrThisInfo = CtorThisVal && isRefCellTy cenv.g vty  then 
              let exprForVal = exprForValRef m vref
              //if AreWithinCtorPreConstruct env then 
              //    warning(SelfRefObjCtor(AreWithinImplicitCtor env, m))

              let ty = destRefCellTy cenv.g vty
              let isSpecial = true
              mkCallCheckThis cenv.g m ty (mkRefCellGet cenv.g m ty exprForVal), isSpecial, ty, [], tpenv
          else 
              // Instantiate the value 
              let vrefFlags,tinst,tau,tpenv = 
                  // Have we got an explicit instantiation? 
                  match optInst with 
                  // No explicit instantiation (the normal case)
                  | None -> 
                      if HasFSharpAttribute cenv.g cenv.g.attrib_RequiresExplicitTypeArgumentsAttribute v.Attribs then
                           errorR(Error(FSComp.SR.tcFunctionRequiresExplicitTypeArguments(v.DisplayName),m))
                  
                      match vrec with 
                      | ValInRecScope false -> 
                          let tps,tau =  vref.TypeScheme
                          let tinst = tps |> List.map mkTyparTy
                          NormalValUse,tinst,tau,tpenv
                      | ValInRecScope true 
                      | ValNotInRecScope ->
                          let _,tinst,tau = FreshenPossibleForallTy cenv.g m TyparRigidity.Flexible vty 
                          NormalValUse,tinst,tau,tpenv

                  // If we have got an explicit instantiation then use that 
                  | Some(vrefFlags,checkTys) -> 
                          let checkInst (tinst:TypeInst) = 
                              if not v.IsMember && not v.PermitsExplicitTypeInstantiation && tinst.Length > 0 && v.Typars.Length > 0 then 
                                   warning(Error(FSComp.SR.tcDoesNotAllowExplicitTypeArguments(v.DisplayName),m))
                          match vrec with 
                          | ValInRecScope false -> 
                              let tpsorig,tau =  vref.TypeScheme
                              let (tinst:TypeInst),tpenv = checkTys tpenv (tpsorig |> List.map (fun tp -> tp.Kind))
                              checkInst tinst
                              if tpsorig.Length <> tinst.Length then error(Error(FSComp.SR.tcTypeParameterArityMismatch(tpsorig.Length, tinst.Length),m))
                              let tau2 = instType (mkTyparInst tpsorig tinst) tau
                              (tpsorig, tinst) ||> List.iter2 (fun tp ty -> 
                                  try UnifyTypes cenv env m (mkTyparTy tp) ty
                                  with _ -> error (Recursion(env.DisplayEnv,v.Id,tau2,tau,m))) 
                              vrefFlags,tinst,tau2,tpenv  
                          | ValInRecScope true 
                          | ValNotInRecScope ->
                              let tps,tptys,tau = FreshenPossibleForallTy cenv.g m TyparRigidity.Flexible vty 
                              //dprintfn "After Freshen: tau = %s" (Layout.showL (typeL tau))
                              let (tinst:TypeInst),tpenv = checkTys tpenv (tps |> List.map (fun tp -> tp.Kind))
                              checkInst tinst
                              //dprintfn "After Check: tau = %s" (Layout.showL (typeL tau))
                              if tptys.Length <> tinst.Length then error(Error(FSComp.SR.tcTypeParameterArityMismatch(tps.Length, tinst.Length),m))
                              List.iter2 (UnifyTypes cenv env m) tptys tinst
                              TcValEarlyGeneralizationConsistencyCheck cenv env (v, vrec, tinst, vty, tau, m)

                              //dprintfn "After Unify: tau = %s" (Layout.showL (typeL tau))
                              vrefFlags,tinst,tau,tpenv  
                      
              let exprForVal = Expr.Val (vref,vrefFlags,m)
              let exprForVal = mkTyAppExpr m (exprForVal,vty) tinst
              let isSpecial = 
                  (match vrefFlags with NormalValUse | PossibleConstrainedCall _ -> false | _ -> true) ||  
                  valRefEq cenv.g vref cenv.g.splice_expr_vref || 
                  valRefEq cenv.g vref cenv.g.splice_raw_expr_vref 
              
              let exprForVal =  RecordUseOfRecValue cenv vrec vref exprForVal m

              exprForVal, isSpecial, tau, tinst, tpenv

/// simplified version of TcVal used in calls to BuildMethodCall (typrelns.fs)
/// this function is used on typechecking step for making calls to provided methods and on optimization step (for the same purpose).
let LightweightTcValForUsingInBuildMethodCall g (vref:ValRef) vrefFlags (vrefTypeInst : TTypes) m = 
    let v = vref.Deref 
    let vty = vref.Type 
    // byref-typed values get dereferenced 
    if isByrefTy g vty then 
        mkAddrGet m vref, destByrefTy g vty 
    else 
      match v.LiteralValue with 
      | Some c -> 
          let _,_,tau = FreshenPossibleForallTy g m TyparRigidity.Flexible vty 
          Expr.Const(c,m,tau),tau
      | None -> 
              // Instantiate the value 
              let tau = 
                  // If we have got an explicit instantiation then use that 
                  let tps,tptys,tau = FreshenPossibleForallTy g m TyparRigidity.Flexible vty 
                  if tptys.Length <> vrefTypeInst.Length then error(Error(FSComp.SR.tcTypeParameterArityMismatch(tps.Length, vrefTypeInst.Length),m));
                  instType (mkTyparInst tps vrefTypeInst) tau 
                      
              let exprForVal = Expr.Val (vref,vrefFlags,m) 
              let exprForVal = mkTyAppExpr m (exprForVal,vty) vrefTypeInst 
              exprForVal, tau

/// Mark points where we decide whether an expression will support automatic
/// decondensation or not. This is somewhat a relic of a previous implementation of decondensation and could
/// be removed

type ApplicableExpr = 
    | ApplicableExpr of 
           // context
           cenv * 
           // the function-valued expression
           Expr *
           // is this the first in an application series
           bool 
    member x.Range = 
        match x with 
        | ApplicableExpr (_,e,_) -> e.Range
    member x.Type = 
        match x with 
        | ApplicableExpr (cenv,e,_) -> tyOfExpr cenv.g e 
    member x.SupplyArgument(e2,m) =
        let (ApplicableExpr (cenv,fe,first)) = x 
        let combinedExpr = 
            match fe with 
            | Expr.App(e1,e1ty,tyargs1,args1,e1m) when 
                       (not first || isNil args1) &&
                       (not (isForallTy cenv.g e1ty) || isFunTy cenv.g (applyTys cenv.g e1ty (tyargs1,args1))) -> 
                Expr.App(e1,e1ty,tyargs1,args1@[e2],unionRanges e1m m)
            | _ -> 
                Expr.App(fe,tyOfExpr cenv.g fe,[],[e2],m) 
        ApplicableExpr(cenv, combinedExpr,false)
    member x.Expr =
        match x with 
        | ApplicableExpr(_,e,_) ->  e
 
let MakeApplicableExprNoFlex cenv expr =
    ApplicableExpr (cenv,expr,true)

/// This function reverses the effect of condensation for a named function value (indeed it can
/// work for any expression, though we only invoke it immediately after a call to TcVal).
///
/// De-condensation is determined BEFORE any arguments are checked. Thus
///      let f (x:'a) (y:'a) = ()
///
///      f  (new obj()) "string"
///
/// does not type check (the argument instantiates 'a to "obj" but there is no flexibility on the
/// second argument position.
///
/// De-condensation is applied AFTER taking into account an explicit type instantiation. This
///      let f<'a> (x:'a) = ()
///
///      f<obj>("string)"
///
/// will type check but
///
/// Sealed types and 'obj' do not introduce generic flexibility when functions are used as first class
/// values. 
///
/// For 'obj' this is because introducing this flexibility would NOT be the reverse of condensation,
/// since we don't condense 
///     f : 'a -> unit
/// to
///     f : obj -> unit
///
/// We represent the flexibility in the TAST by leaving a function-to-function coercion node in the tree
/// This "special" node is immediately eliminated by the use of IteratedFlexibleAdjustArityOfLambdaBody as soon as we 
/// first transform the tree (currently in optimization)

let MakeApplicableExprWithFlex cenv (env: TcEnv) expr =
    let exprTy = tyOfExpr cenv.g expr
    let m = expr.Range
    
    let isNonFlexibleType ty = isSealedTy cenv.g ty 
    
    let argTys,retTy = stripFunTy cenv.g exprTy
    let curriedActualTypes = argTys |> List.map (tryDestRefTupleTy cenv.g)
    if (curriedActualTypes.IsEmpty ||
        curriedActualTypes |> List.exists (List.exists (isByrefTy cenv.g)) ||
        curriedActualTypes |> List.forall (List.forall isNonFlexibleType)) then 
       
        ApplicableExpr (cenv,expr,true)
    else
        let curriedFlexibleTypes = 
            curriedActualTypes |> List.mapSquared (fun actualType -> 
                if isNonFlexibleType actualType 
                then actualType 
                else 
                   let flexibleType = NewInferenceType ()
                   AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m NoTrace actualType flexibleType;
                   flexibleType)

        // Create a coercion to represent the expansion of the application
        let expr = mkCoerceExpr (expr,mkIteratedFunTy (List.map (mkRefTupledTy cenv.g) curriedFlexibleTypes) retTy,m,exprTy)
        ApplicableExpr (cenv,expr,true)


///  Checks, warnings and constraint assertions for downcasts 
let TcRuntimeTypeTest isCast isOperator cenv denv m tgty srcTy =
    if TypeDefinitelySubsumesTypeNoCoercion 0 cenv.g cenv.amap m tgty srcTy then 
      warning(TypeTestUnnecessary(m))

    if isTyparTy cenv.g srcTy then 
        error(IndeterminateRuntimeCoercion(denv,srcTy,tgty,m))

    if isSealedTy cenv.g srcTy then 
        error(RuntimeCoercionSourceSealed(denv,srcTy,m))

    if isSealedTy cenv.g tgty || isTyparTy cenv.g tgty || not (isInterfaceTy cenv.g srcTy) then 
        if isCast then
            AddCxTypeMustSubsumeType (ContextInfo.RuntimeTypeTest isOperator) denv cenv.css m NoTrace srcTy tgty
        else
            AddCxTypeMustSubsumeType ContextInfo.NoContext denv cenv.css m NoTrace srcTy tgty

    if isErasedType cenv.g tgty then
        if isCast then
            warning(Error(FSComp.SR.tcTypeCastErased(NicePrint.minimalStringOfType denv tgty, NicePrint.minimalStringOfType denv (stripTyEqnsWrtErasure EraseAll cenv.g tgty)), m))
        else
            error(Error(FSComp.SR.tcTypeTestErased(NicePrint.minimalStringOfType denv tgty, NicePrint.minimalStringOfType denv (stripTyEqnsWrtErasure EraseAll cenv.g tgty)), m))
    else
        getErasedTypes cenv.g tgty |> 
        List.iter (fun ety -> if isMeasureTy cenv.g ety 
                              then warning(Error(FSComp.SR.tcTypeTestLosesMeasures(NicePrint.minimalStringOfType denv ety), m))
                              else warning(Error(FSComp.SR.tcTypeTestLossy(NicePrint.minimalStringOfType denv ety, NicePrint.minimalStringOfType denv (stripTyEqnsWrtErasure EraseAll cenv.g ety)),m)))

///  Checks, warnings and constraint assertions for upcasts 
let TcStaticUpcast cenv denv m tgty srcTy =
    if isTyparTy cenv.g tgty then 
        error(IndeterminateStaticCoercion(denv,srcTy,tgty,m)) 

    if isSealedTy cenv.g tgty then 
        warning(CoercionTargetSealed(denv,tgty,m))

    if typeEquiv cenv.g srcTy tgty then 
        warning(UpcastUnnecessary(m)) 

    AddCxTypeMustSubsumeType ContextInfo.NoContext denv cenv.css m NoTrace tgty srcTy





let BuildPossiblyConditionalMethodCall cenv env isMutable m isProp minfo valUseFlags minst objArgs args =

    let conditionalCallDefineOpt = TryFindMethInfoStringAttribute cenv.g m cenv.g.attrib_ConditionalAttribute minfo 

    match conditionalCallDefineOpt with 
    | Some d when not (List.contains d cenv.conditionalDefines) -> 

        // Methods marked with 'Conditional' must return 'unit' 
        UnifyTypes cenv env m cenv.g.unit_ty (minfo.GetFSharpReturnTy(cenv.amap, m, minst))
        mkUnit cenv.g m, cenv.g.unit_ty

    | _ -> 
#if EXTENSIONTYPING
        match minfo with
        | ProvidedMeth(_, mi, _,_) -> 
            // BuildInvokerExpressionForProvidedMethodCall converts references to F# intrinsics back to values
            // and uses TcVal to do this. However we don't want to check attributes again for provided references to values,
            // so we pass 'false' for 'checkAttributes'.
            let tcVal = LightweightTcValForUsingInBuildMethodCall cenv.g
            let _, retExpt, retTy = ProvidedMethodCalls.BuildInvokerExpressionForProvidedMethodCall tcVal (cenv.g, cenv.amap, mi, objArgs, isMutable, isProp, valUseFlags, args, m)
            retExpt, retTy

        | _ -> 
#endif    
        let tcVal valref valUse ttypes m = 
            let a,_, b, _, _ = TcVal true cenv env emptyUnscopedTyparEnv valref (Some (valUse, (fun x _ -> ttypes, x))) m
            a, b
        BuildMethodCall tcVal cenv.g cenv.amap isMutable m isProp minfo valUseFlags minst objArgs args


let TryFindIntrinsicOrExtensionMethInfo (cenv:cenv) (env: TcEnv) m ad nm ty = 
    AllMethInfosOfTypeInScope cenv.infoReader env.NameEnv (Some(nm),ad) IgnoreOverrides m ty

/// Build the 'test and dispose' part of a 'use' statement 
let BuildDisposableCleanup cenv env m (v:Val) = 
    v.SetHasBeenReferenced() 
    let ad = env.eAccessRights
    let disposeMethod = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Dispose" cenv.g.system_IDisposable_typ with 
        | [x] ->  x 
        | _ -> error(InternalError(FSComp.SR.tcCouldNotFindIDisposable(),m)) 


    // For struct types the test is simpler: we can determine if IDisposable is supported, and even when it is, we can avoid doing the type test 
    // Note this affects the elaborated form seen by quotations etc.
    if isStructTy cenv.g v.Type then 
        if TypeFeasiblySubsumesType 0 cenv.g cenv.amap m cenv.g.system_IDisposable_typ CanCoerce v.Type then
            // We can use NeverMutates here because the variable is going out of scope, there is no need to take a defensive
            // copy of it.
            let disposeExpr,_ = BuildPossiblyConditionalMethodCall cenv env NeverMutates m false disposeMethod NormalValUse [] [exprForVal v.Range v] []    
            disposeExpr
        else
            mkUnit cenv.g m
    else
        let disposeObjVar,disposeObjExpr = Tastops.mkCompGenLocal m "objectToDispose" cenv.g.system_IDisposable_typ
        let disposeExpr,_ = BuildPossiblyConditionalMethodCall cenv env PossiblyMutates m false disposeMethod NormalValUse [] [disposeObjExpr] []
        let inpe = mkCoerceExpr(exprForVal v.Range v,cenv.g.obj_ty,m,v.Type)
        mkIsInstConditional cenv.g m cenv.g.system_IDisposable_typ inpe disposeObjVar disposeExpr (mkUnit cenv.g m) 

/// Build call to get_OffsetToStringData as part of 'fixed'
let BuildOffsetToStringData cenv env m = 
    let ad = env.eAccessRights
    let offsetToStringDataMethod = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "get_OffsetToStringData" cenv.g.system_RuntimeHelpers_typ with 
        | [x] ->  x 
        | _ -> error(Error(FSComp.SR.tcCouldNotFindOffsetToStringData(),m)) 

    let offsetExpr,_ = BuildPossiblyConditionalMethodCall cenv env NeverMutates m false offsetToStringDataMethod NormalValUse [] [] []    
    offsetExpr

let BuildILFieldGet g amap m objExpr (finfo:ILFieldInfo) = 
    let fref = finfo.ILFieldRef
    let isValueType = finfo.IsValueType
    let valu = if isValueType then AsValue else AsObject
    let tinst = finfo.TypeInst
    let fieldType = finfo.FieldType (amap,m)
#if EXTENSIONTYPING
    let ty = tyOfExpr g objExpr
    match finfo with
    | ProvidedField _ when (isErasedType g ty) ->
        // we know it's accessible, and there are no attributes to check for now...
        match finfo.LiteralValue with 
        | None -> 
            error (Error(FSComp.SR.tcTPFieldMustBeLiteral(), m))
        | Some lit -> 
            Expr.Const(TcFieldInit m lit,m,fieldType)
    | _ ->
#endif
    let wrap,objExpr = mkExprAddrOfExpr g isValueType false NeverMutates objExpr None m
      // The empty instantiation on the AbstractIL fspec is OK, since we make the correct fspec in IlxGen.GenAsm 
      // This ensures we always get the type instantiation right when doing this from 
      // polymorphic code, after inlining etc. *
    let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])
    // Add an I_nop if this is an initonly field to make sure we never recognize it as an lvalue. See mkExprAddrOfExpr. 
    wrap (mkAsmExpr (([ mkNormalLdfld fspec ] @ (if finfo.IsInitOnly then [ AI_nop ] else [])), tinst,[objExpr],[fieldType],m)) 

let BuildILFieldSet g m objExpr (finfo:ILFieldInfo) argExpr = 
    let fref = finfo.ILFieldRef
    let isValueType = finfo.IsValueType
    let valu = if isValueType then AsValue else AsObject
    let tinst = finfo.TypeInst
      // The empty instantiation on the AbstractIL fspec is OK, since we make the correct fspec in IlxGen.GenAsm 
      // This ensures we always get the type instantiation right when doing this from 
      // polymorphic code, after inlining etc. *
    let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])
    if finfo.IsInitOnly then error (Error (FSComp.SR.tcFieldIsReadonly(),m))
    let wrap,objExpr = mkExprAddrOfExpr g isValueType false DefinitelyMutates objExpr None m 
    wrap (mkAsmExpr ([ mkNormalStfld fspec ], tinst,[objExpr; argExpr],[],m)) 

let BuildILStaticFieldSet m (finfo:ILFieldInfo) argExpr = 
    let fref = finfo.ILFieldRef
    let isValueType = finfo.IsValueType
    let valu = if isValueType then AsValue else AsObject
    let tinst = finfo.TypeInst
      // The empty instantiation on the AbstractIL fspec is OK, since we make the correct fspec in IlxGen.GenAsm 
      // This ensures we always get the type instantiation right when doing this from 
      // polymorphic code, after inlining etc. 
    let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])
    if finfo.IsInitOnly then error (Error (FSComp.SR.tcFieldIsReadonly(),m))
    mkAsmExpr ([ mkNormalStsfld fspec ], tinst,[argExpr],[],m)
    
let BuildRecdFieldSet g m objExpr (rfinfo:RecdFieldInfo) argExpr = 
    let tgty = rfinfo.EnclosingType
    let valu = isStructTy g tgty
    let objExpr = if valu then objExpr else mkCoerceExpr(objExpr,tgty,m,tyOfExpr g objExpr)
    let wrap,objExpr = mkExprAddrOfExpr g valu false DefinitelyMutates objExpr None m
    wrap (mkRecdFieldSetViaExprAddr (objExpr,rfinfo.RecdFieldRef,rfinfo.TypeInst,argExpr,m) )
    
    
//-------------------------------------------------------------------------
// Helpers dealing with named and optional args at callsites
//------------------------------------------------------------------------- 

let (|BinOpExpr|_|) e = 
    match e with 
    | SynExpr.App (_, _, SynExpr.App(_, _, SingleIdent opId, a, _), b, _) -> Some (opId,a,b)
    | _ -> None

let (|SimpleEqualsExpr|_|) e = 
    match e with 
    | BinOpExpr(opId,a,b) when opId.idText = opNameEquals ->  Some (a,b)
    | _ -> None

// For join clauses that join on nullable, we syntactically insert the creation of nullable values on the appropriate side of the condition,
// then pull the syntax apart again
let (|JoinRelation|_|) cenv env (e:SynExpr) = 
    let m = e.Range
    let ad = env.eAccessRights

    let isOpName opName vref s =
        (s = opName) &&
        match ResolveExprLongIdent cenv.tcSink cenv.nameResolver m ad env.eNameResEnv TypeNameResolutionInfo.Default [ident(opName,m)] with
        | Item.Value vref2, [] -> valRefEq cenv.g vref vref2
        | _ -> false

    match e with 
    | BinOpExpr(opId,a,b) when isOpName opNameEquals cenv.g.equals_operator_vref opId.idText -> Some (a,b)

    | BinOpExpr(opId,a,b) when isOpName opNameEqualsNullable cenv.g.equals_nullable_operator_vref opId.idText -> 

        let a = SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet a.Range [MangledGlobalName;"System"] "Nullable",a,a.Range)
        Some (a,b)

    | BinOpExpr(opId,a,b) when isOpName opNameNullableEquals cenv.g.nullable_equals_operator_vref opId.idText -> 

        let b = SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet b.Range [MangledGlobalName;"System"] "Nullable",b,b.Range)
        Some (a,b)

    | BinOpExpr(opId,a,b) when isOpName opNameNullableEqualsNullable cenv.g.nullable_equals_nullable_operator_vref opId.idText -> 

        Some (a,b)

    | _ -> None


/// Detect a named argument at a callsite
let TryGetNamedArg e = 
    match e with 
    | SimpleEqualsExpr(LongOrSingleIdent(isOpt,LongIdentWithDots([a],_),None,_),b) -> Some(isOpt,a,b)
    | _ -> None 

let inline IsNamedArg e = 
    match e with 
    | SimpleEqualsExpr(LongOrSingleIdent(_,LongIdentWithDots([_],_),None,_),_) -> true
    | _ -> false

/// Get the method arguments at a callsite, taking into account named and optional arguments
let GetMethodArgs arg =
    let args = 
        match arg with 
        | SynExpr.Const (SynConst.Unit,_) -> []
        | SynExprParen(SynExpr.Tuple (args,_,_),_,_,_) | SynExpr.Tuple (args,_,_) -> args
        | SynExprParen(arg,_,_,_) | arg -> [arg]
    let unnamedCallerArgs,namedCallerArgs = 
        args |> List.takeUntil IsNamedArg
    let namedCallerArgs = 
        namedCallerArgs 
        |> List.choose (fun e -> 
              match TryGetNamedArg e with
              | None ->
                  // ignore errors to avoid confusing error messages in cases like foo(a = 1,) 
                  // do not abort overload resolution in case if named arguments are mixed with errors
                  match e with
                  | SynExpr.ArbitraryAfterError _ -> None
                  | _ -> error(Error(FSComp.SR.tcNameArgumentsMustAppearLast(), e.Range)) 
              | namedArg -> namedArg)
    unnamedCallerArgs, namedCallerArgs


//-------------------------------------------------------------------------
// Helpers dealing with pattern match compilation
//------------------------------------------------------------------------- 

let CompilePatternForMatch cenv (env: TcEnv) mExpr matchm warnOnUnused actionOnFailure (v,generalizedTypars) clauses inputTy resultTy =
    let dtree,targets = CompilePattern cenv.g env.DisplayEnv cenv.amap mExpr matchm warnOnUnused actionOnFailure (v,generalizedTypars) clauses inputTy resultTy
    mkAndSimplifyMatch NoSequencePointAtInvisibleBinding mExpr matchm resultTy dtree targets

/// Compile a pattern
let CompilePatternForMatchClauses cenv env mExpr matchm warnOnUnused actionOnFailure inputTy resultTy tclauses = 
    // Avoid creating a dummy in the common cases where we are about to bind a name for the expression 
    // CLEANUP: avoid code duplication with code further below, i.e.all callers should call CompilePatternForMatch 
    match tclauses with 
    | [TClause(TPat_as (pat1,PBind (v,TypeScheme(generalizedTypars,_)),_),None,TTarget(vs,e,spTarget),m2)] ->
        let expr = CompilePatternForMatch cenv env mExpr matchm warnOnUnused actionOnFailure (v,generalizedTypars) [TClause(pat1,None,TTarget(ListSet.remove valEq v vs,e,spTarget),m2)] inputTy resultTy
        v,expr
    | _ -> 
        let idv,_ = Tastops.mkCompGenLocal mExpr "matchValue" inputTy
        let expr = CompilePatternForMatch cenv env mExpr matchm warnOnUnused actionOnFailure (idv,[]) tclauses inputTy resultTy
        idv,expr



//-------------------------------------------------------------------------
// Helpers dealing with sequence expressions
//------------------------------------------------------------------------- 

   
/// Get the fragmentary expressions resulting from turning 
/// an expression into an enumerable value, e.g. at 'for' loops 

// localAlloc is relevant if the enumerator is a mutable struct and indicates 
// if the enumerator can be allocated as a mutable local variable 
let AnalyzeArbitraryExprAsEnumerable cenv (env: TcEnv) localAlloc m exprty expr =
    let ad = env.eAccessRights

    let err k ty = 
        let txt = NicePrint.minimalStringOfType env.DisplayEnv ty
        let msg = if k then FSComp.SR.tcTypeCannotBeEnumerated(txt) else FSComp.SR.tcEnumTypeCannotBeEnumerated(txt)
        Exception(Error(msg,m))

    let findMethInfo k m nm ty = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env m ad nm ty with 
        | [] -> err k ty
        | res :: _ -> Result res  
       
      
    // Ensure there are no curried arguments, and indeed no arguments at all
    let hasArgs (minfo:MethInfo) minst = 
        match minfo.GetParamTypes(cenv.amap, m, minst) with 
        | [[]] -> false
        | _ -> true

    let tryType (exprToSearchForGetEnumeratorAndItem,tyToSearchForGetEnumeratorAndItem) = 
        match findMethInfo true m "GetEnumerator" tyToSearchForGetEnumeratorAndItem with 
        | Exception e -> Exception e
        | Result getEnumerator_minfo  ->

        let getEnumerator_minst = FreshenMethInfo m getEnumerator_minfo
        let retTypeOfGetEnumerator = getEnumerator_minfo.GetFSharpReturnTy(cenv.amap, m, getEnumerator_minst)
        if hasArgs getEnumerator_minfo getEnumerator_minst then err true tyToSearchForGetEnumeratorAndItem else

        match findMethInfo false m "MoveNext" retTypeOfGetEnumerator with 
        | Exception e -> Exception e
        | Result moveNext_minfo        ->

        let moveNext_minst = FreshenMethInfo m moveNext_minfo
        let retTypeOfMoveNext = moveNext_minfo.GetFSharpReturnTy(cenv.amap, m, moveNext_minst)
        if not (typeEquiv cenv.g cenv.g.bool_ty retTypeOfMoveNext) then err false  retTypeOfGetEnumerator else   
        if hasArgs moveNext_minfo moveNext_minst then err false retTypeOfGetEnumerator else

        match findMethInfo false m "get_Current" retTypeOfGetEnumerator with 
        | Exception e -> Exception e
        | Result get_Current_minfo ->

        let get_Current_minst = FreshenMethInfo m get_Current_minfo
        if hasArgs get_Current_minfo get_Current_minst then err false retTypeOfGetEnumerator else
        let enumElemTy  = get_Current_minfo.GetFSharpReturnTy(cenv.amap, m, get_Current_minst)
        
        // Compute the element type of the strongly typed enumerator
        //
        // Like C#, we detect the 'GetEnumerator' pattern for .NET version 1.x abstractions that don't 
        // support the correct generic interface. However unlike C# we also go looking for a 'get_Item' or 'Item' method
        // with a single integer indexer argument to try to get a strong type for the enumeration should the Enumerator
        // not provide anything useful. To enable interop with some legacy COM APIs,
        // the single integer indexer argument is allowed to have type 'object'.

        let enumElemTy = 

            if isObjTy cenv.g enumElemTy then
                // Look for an 'Item' property, or a set of these with consistent return types 
                let allEquivReturnTypes (minfo:MethInfo) (others:MethInfo list) = 
                    let returnTy = minfo.GetFSharpReturnTy(cenv.amap, m, [])
                    others |> List.forall (fun other -> typeEquiv cenv.g (other.GetFSharpReturnTy(cenv.amap, m, [])) returnTy)
                
                let isInt32OrObjectIndexer (minfo:MethInfo) = 
                    match minfo.GetParamTypes(cenv.amap, m, []) with
                    | [[ty]] -> 
                        // e.g. MatchCollection
                        typeEquiv cenv.g cenv.g.int32_ty ty || 
                        // e.g. EnvDTE.Documents.Item
                        typeEquiv cenv.g cenv.g.obj_ty ty
                    | _ -> false
                
                match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "get_Item" tyToSearchForGetEnumeratorAndItem with
                | (minfo :: others) when (allEquivReturnTypes minfo others &&
                                          List.exists isInt32OrObjectIndexer (minfo :: others)) -> 
                    minfo.GetFSharpReturnTy(cenv.amap, m, [])
                
                | _ -> 
                
                // Some types such as XmlNodeList have only an Item method  
                match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Item" tyToSearchForGetEnumeratorAndItem with
                | (minfo :: others) when (allEquivReturnTypes minfo others &&
                                          List.exists isInt32OrObjectIndexer (minfo :: others)) -> 
                    minfo.GetFSharpReturnTy(cenv.amap, m, [])
                
                | _ -> enumElemTy
            else 
                enumElemTy

        let isEnumeratorTypeStruct = isStructTy cenv.g retTypeOfGetEnumerator
        let originalRetTypeOfGetEnumerator = retTypeOfGetEnumerator

        let (enumeratorVar,enumeratorExpr), retTypeOfGetEnumerator = 
            if isEnumeratorTypeStruct then 
               if localAlloc then 
                  Tastops.mkMutableCompGenLocal m "enumerator" retTypeOfGetEnumerator, retTypeOfGetEnumerator
               else
                  let refCellTyForRetTypeOfGetEnumerator = mkRefCellTy cenv.g retTypeOfGetEnumerator
                  let v,e = Tastops.mkMutableCompGenLocal m "enumerator" refCellTyForRetTypeOfGetEnumerator
                  (v, mkRefCellGet cenv.g m retTypeOfGetEnumerator e), refCellTyForRetTypeOfGetEnumerator
                  
            else
               Tastops.mkCompGenLocal m "enumerator" retTypeOfGetEnumerator, retTypeOfGetEnumerator
            
        let getEnumExpr, getEnumTy  = 
            let (getEnumExpr, getEnumTy) as res = BuildPossiblyConditionalMethodCall cenv env PossiblyMutates   m false getEnumerator_minfo NormalValUse getEnumerator_minst [exprToSearchForGetEnumeratorAndItem] []
            if not isEnumeratorTypeStruct || localAlloc then res                
            else 
                // wrap enumerators that are represented as mutable structs into ref cells 
                let getEnumExpr = mkRefCell cenv.g m originalRetTypeOfGetEnumerator getEnumExpr 
                let getEnumTy = mkRefCellTy cenv.g getEnumTy
                getEnumExpr, getEnumTy

        let guardExpr  ,guardTy      = BuildPossiblyConditionalMethodCall cenv env DefinitelyMutates m false moveNext_minfo      NormalValUse moveNext_minst [enumeratorExpr] []
        let currentExpr,currentTy    = BuildPossiblyConditionalMethodCall cenv env DefinitelyMutates m true get_Current_minfo   NormalValUse get_Current_minst [enumeratorExpr] []
        let betterCurrentExpr  = mkCoerceExpr(currentExpr,enumElemTy,currentExpr.Range,currentTy)
        Result(enumeratorVar, enumeratorExpr,retTypeOfGetEnumerator,enumElemTy,getEnumExpr,getEnumTy, guardExpr,guardTy, betterCurrentExpr)

    // First try the original known static type
    match (if isArray1DTy cenv.g exprty then Exception (Failure "") else tryType (expr,exprty)) with 
    | Result res  -> res
    | Exception e -> 

    let probe ty =
        if (AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m ty exprty) then 
            match tryType (mkCoerceExpr(expr,ty,expr.Range,exprty),ty) with 
            | Result res  -> Some res
            | Exception e -> raise e
        else None

    // Next try to typecheck the thing as a sequence
    let enumElemTy = NewInferenceType ()
    let exprTyAsSeq = mkSeqTy cenv.g enumElemTy
    
    match probe exprTyAsSeq with
    | Some res -> res
    | None ->
    let ienumerable = mkAppTy cenv.g.tcref_System_Collections_IEnumerable []
    match probe ienumerable with
    | Some res -> res
    | None ->
    raise e


// Used inside sequence expressions
let ConvertArbitraryExprToEnumerable cenv ty (env: TcEnv) (expr:Expr) =
    let m = expr.Range
    let enumElemTy = NewInferenceType ()
    if AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m ( mkSeqTy cenv.g enumElemTy) ty then 
        expr,enumElemTy
    else          
        let enumerableVar,enumerableExpr = mkCompGenLocal m "inputSequence" ty
        let enumeratorVar, _,retTypeOfGetEnumerator,enumElemTy,getEnumExpr,_,guardExpr,guardTy,betterCurrentExpr = 
            AnalyzeArbitraryExprAsEnumerable cenv env false m ty enumerableExpr
        
        let expr = 
           mkCompGenLet m enumerableVar expr 
               (mkCallSeqOfFunctions cenv.g m retTypeOfGetEnumerator enumElemTy 
                   (mkUnitDelayLambda cenv.g m getEnumExpr)
                   (mkLambda m enumeratorVar (guardExpr,guardTy)) 
                   (mkLambda m enumeratorVar (betterCurrentExpr,enumElemTy)))
        expr,enumElemTy           

let mkSeqEmpty cenv env m genTy =
    // We must discover the 'zero' of the monadic algebra being generated in order to compile failing matches.
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    mkCallSeqEmpty cenv.g m genResultTy 

let mkSeqCollect cenv env m enumElemTy genTy lam enumExpr =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    let enumExpr = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g enumElemTy) (tyOfExpr cenv.g enumExpr) enumExpr
    mkCallSeqCollect cenv.g m enumElemTy genResultTy lam enumExpr

let mkSeqUsing cenv (env: TcEnv) m resourceTy genTy resourceExpr lam =
    AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m NoTrace cenv.g.system_IDisposable_typ resourceTy
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    mkCallSeqUsing cenv.g m resourceTy genResultTy resourceExpr lam 

let mkSeqDelay cenv env m genTy lam =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    mkCallSeqDelay cenv.g m genResultTy (mkUnitDelayLambda cenv.g m lam) 


let mkSeqAppend cenv env m genTy e1 e2 =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    let e1 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e1) e1
    let e2 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e2) e2
    mkCallSeqAppend cenv.g m genResultTy e1 e2 

let mkSeqFromFunctions cenv env m genTy e1 e2 =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    let e2 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e2) e2
    mkCallSeqGenerated cenv.g m genResultTy e1 e2 

let mkSeqFinally cenv env m genTy e1 e2 =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy)
    let e1 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e1) e1
    mkCallSeqFinally cenv.g m genResultTy e1 e2 

let mkSeqExprMatchClauses (pat',vspecs) innerExpr = 
    [TClause(pat',None,TTarget(vspecs, innerExpr,SequencePointAtTarget),pat'.Range) ] 

let compileSeqExprMatchClauses cenv env inputExprMark (pat':Pattern, vspecs) innerExpr bindPatTy genInnerTy = 
    let patMark = pat'.Range
    let tclauses = mkSeqExprMatchClauses (pat',vspecs) innerExpr 
    CompilePatternForMatchClauses cenv env inputExprMark patMark false ThrowIncompleteMatchException bindPatTy genInnerTy tclauses 


let elimFastIntegerForLoop (spBind,id,start,dir,finish,innerExpr,m) = 
    let pseudoEnumExpr = 
        if dir then mkSynInfix m start ".." finish
        else mkSynTrifix m ".. .." start (SynExpr.Const(SynConst.Int32 -1, start.Range)) finish
    SynExpr.ForEach (spBind,SeqExprOnly false,true,mkSynPatVar None id,pseudoEnumExpr,innerExpr,m)

let (|ExprAsPat|_|) (f:SynExpr) =    
    match f with 
    | SingleIdent v1 | SynExprParen(SingleIdent v1, _, _, _) -> Some (mkSynPatVar None v1)
    | SynExprParen(SynExpr.Tuple (elems, _, _), _, _, _) -> 
        let elems = elems |> List.map (|SingleIdent|_|) 
        if elems |> List.forall (fun x -> x.IsSome) then 
            Some (SynPat.Tuple((elems |> List.map (fun x -> mkSynPatVar None x.Value)), f.Range))
        else
            None
    | _ -> None

/// Determine if a syntactic expression inside 'seq { ... }' or '[...]' counts as a "simple sequence
/// of semicolon separated values". For example [1;2;3].
/// 'acceptDeprecated' is true for the '[ ... ]' case, where we allow the syntax '[ if g then t else e ]' but ask it to be parenthesized
///
let (|SimpleSemicolonSequence|_|) acceptDeprecated c = 

    let rec YieldFree expr = 
        match expr with 
        | SynExpr.Sequential (_,_,e1,e2,_) -> YieldFree e1 && YieldFree e2
        | SynExpr.IfThenElse (_,e2,e3opt,_,_,_,_) -> YieldFree e2 && Option.forall YieldFree e3opt
        | SynExpr.TryWith (e1,_,clauses,_,_,_,_) -> YieldFree e1 && clauses |> List.forall (fun (Clause(_,_,e,_,_)) -> YieldFree e)
        | SynExpr.Match (_,_,clauses,_,_) -> clauses |> List.forall (fun (Clause(_,_,e,_,_)) -> YieldFree e)
        | SynExpr.For (_,_,_,_,_,body,_) 
        | SynExpr.TryFinally (body,_,_,_,_)
        | SynExpr.LetOrUse (_,_,_,body,_) 
        | SynExpr.While (_,_,body,_) 
        | SynExpr.ForEach (_,_,_,_,_,body,_) -> YieldFree body
        | SynExpr.YieldOrReturnFrom _ 
        | SynExpr.YieldOrReturn _ 
        | SynExpr.LetOrUseBang _ 
        | SynExpr.ImplicitZero _ 
        | SynExpr.Do _ -> false
        | _ -> true

    let rec IsSimpleSemicolonSequenceElement expr = 
        match expr with 
        | SynExpr.IfThenElse _ when acceptDeprecated && YieldFree expr -> true
        | SynExpr.IfThenElse _ 
        | SynExpr.TryWith _ 
        | SynExpr.Match _ 
        | SynExpr.For _ 
        | SynExpr.ForEach _ 
        | SynExpr.TryFinally _ 
        | SynExpr.YieldOrReturnFrom _ 
        | SynExpr.YieldOrReturn _ 
        | SynExpr.LetOrUse _ 
        | SynExpr.Do _ 
        | SynExpr.LetOrUseBang _ 
        | SynExpr.ImplicitZero _ 
        | SynExpr.While _ -> false
        | _ -> true

    let rec GetSimpleSemicolonSequenceOfComprehension expr acc = 
        match expr with 
        | SynExpr.Sequential(_,true,e1,e2,_) -> 
            if IsSimpleSemicolonSequenceElement e1 then 
                GetSimpleSemicolonSequenceOfComprehension e2 (e1::acc)
            else
                None 
        | e -> 
            if IsSimpleSemicolonSequenceElement e then 
                Some(List.rev (e::acc))
            else 
                None 

    if YieldFree c then 
        GetSimpleSemicolonSequenceOfComprehension c []
    else
        None

//-------------------------------------------------------------------------
// Mutually recursive shapes
//------------------------------------------------------------------------- 

/// Represents the shape of a mutually recursive group of declarations including nested modules
[<RequireQualifiedAccess>]
type MutRecShape<'TypeData, 'LetsData, 'ModuleData, 'ModuleAbbrevData, 'OpenData> = 
    | Tycon of 'TypeData
    | Lets of 'LetsData
    | Module of 'ModuleData * MutRecShapes<'TypeData, 'LetsData, 'ModuleData, 'ModuleAbbrevData, 'OpenData> 
    | ModuleAbbrev of 'ModuleAbbrevData 
    | Open of 'OpenData

and MutRecShapes<'TypeData, 'LetsData, 'ModuleData, 'ModuleAbbrevData, 'OpenData> = MutRecShape<'TypeData, 'LetsData, 'ModuleData, 'ModuleAbbrevData, 'OpenData> list

module MutRecShapes = 
   let rec map f1 f2 f3 x = 
       x |> List.map (function 
           | MutRecShape.Open a -> MutRecShape.Open a
           | MutRecShape.ModuleAbbrev b -> MutRecShape.ModuleAbbrev b
           | MutRecShape.Tycon a -> MutRecShape.Tycon (f1 a)
           | MutRecShape.Lets b -> MutRecShape.Lets (f2 b)
           | MutRecShape.Module (c,d) -> MutRecShape.Module (f3 c, map f1 f2 f3 d))


   let mapTycons f1 xs = map f1 id id xs
   let mapTyconsAndLets f1 f2 xs = map f1 f2 id xs
   let mapLets f2 xs = map id f2 id xs
   let mapModules f1 xs = map id id f1 xs

   let rec mapWithEnv fTycon fLets (env: 'Env) x = 
       x |> List.map (function 
           | MutRecShape.Open a -> MutRecShape.Open a
           | MutRecShape.ModuleAbbrev a -> MutRecShape.ModuleAbbrev a
           | MutRecShape.Tycon a -> MutRecShape.Tycon (fTycon env a)
           | MutRecShape.Lets b -> MutRecShape.Lets (fLets env b)
           | MutRecShape.Module ((c, env2),d) -> MutRecShape.Module ((c,env2), mapWithEnv fTycon fLets env2 d))

   let mapTyconsWithEnv f1 env xs = mapWithEnv f1 (fun _env x -> x) env xs

   let rec mapWithParent parent f1 f2 f3 xs = 
       xs |> List.map (function 
           | MutRecShape.Open a -> MutRecShape.Open a
           | MutRecShape.ModuleAbbrev a -> MutRecShape.ModuleAbbrev a
           | MutRecShape.Tycon a -> MutRecShape.Tycon (f2 parent a)
           | MutRecShape.Lets b -> MutRecShape.Lets (f3 parent b)
           | MutRecShape.Module (c,d) -> 
               let c2, parent2 = f1 parent c d
               MutRecShape.Module (c2, mapWithParent parent2 f1 f2 f3 d))

   let rec computeEnvs f1 f2 (env: 'Env) xs = 
       let env = f2 env xs
       env, 
       xs |> List.map (function 
           | MutRecShape.Open a -> MutRecShape.Open a
           | MutRecShape.ModuleAbbrev a -> MutRecShape.ModuleAbbrev a
           | MutRecShape.Tycon a -> MutRecShape.Tycon a
           | MutRecShape.Lets b -> MutRecShape.Lets b
           | MutRecShape.Module (c,ds) -> 
               let env2 = f1 env c
               let env3, ds2 = computeEnvs f1 f2 env2 ds
               MutRecShape.Module ((c,env3), ds2))

   let rec extendEnvs f1 (env: 'Env) xs = 
       let env = f1 env xs
       env, 
       xs |> List.map (function 
           | MutRecShape.Module ((c,env),ds) -> 
               let env2, ds2 = extendEnvs f1 env ds
               MutRecShape.Module ((c,env2), ds2)
           | x -> x)

   let dropEnvs xs = xs |> mapModules fst

   let rec expandTyconsWithEnv f1 env xs = 
       let preBinds, postBinds = 
           xs |> List.map (fun elem -> 
               match elem with 
               | MutRecShape.Tycon a -> f1 env a
               | _ -> [], [])
            |> List.unzip
       [MutRecShape.Lets (List.concat preBinds)] @ 
       (xs |> List.map (fun elem -> 
           match elem with 
           | MutRecShape.Module ((c,env2),d) -> MutRecShape.Module ((c,env2), expandTyconsWithEnv f1 env2 d)
           | _ -> elem)) @
       [MutRecShape.Lets (List.concat postBinds)] 

   let rec mapFoldWithEnv f1 z env xs = 
       (z,xs) ||> List.mapFold (fun z x ->
           match x with
           | MutRecShape.Module ((c,env2),ds) -> let ds2,z = mapFoldWithEnv f1 z env2 ds in MutRecShape.Module ((c, env2), ds2),z
           | _ -> let x2,z = f1 z env x in x2, z)


   let rec collectTycons x = 
       x |> List.collect (function 
           | MutRecShape.Tycon a -> [a]
           | MutRecShape.Module (_,d) -> collectTycons d
           | _ -> [])

   let topTycons x = 
       x |> List.choose (function MutRecShape.Tycon a -> Some a | _ -> None)

   let rec iter f1 f2 f3 f4 f5 x = 
       x |> List.iter (function 
           | MutRecShape.Tycon a -> f1 a
           | MutRecShape.Lets b -> f2 b
           | MutRecShape.Module (c,d) -> f3 c; iter f1 f2 f3 f4 f5 d
           | MutRecShape.Open a -> f4 a
           | MutRecShape.ModuleAbbrev a -> f5 a)

   let iterTycons f1 x = iter f1 ignore ignore ignore ignore x
   let iterTyconsAndLets f1 f2 x = iter f1 f2 ignore ignore ignore x
   let iterModules f1 x = iter ignore ignore f1 ignore ignore x

   let rec iterWithEnv f1 f2 f3 f4 env x = 
       x |> List.iter (function 
           | MutRecShape.Tycon a -> f1 env a
           | MutRecShape.Lets b -> f2 env b
           | MutRecShape.Module ((_,env),d) -> iterWithEnv f1 f2 f3 f4 env d
           | MutRecShape.Open a -> f3 env a
           | MutRecShape.ModuleAbbrev a -> f4 env a)

   let iterTyconsWithEnv f1 env xs = iterWithEnv f1 (fun _env _x -> ()) (fun _env _x -> ()) (fun _env _x -> ()) env xs

//-------------------------------------------------------------------------
// Post-transform initialization graphs using the 'lazy' interpretation.
// See ML workshop paper.
//------------------------------------------------------------------------- 

type InitializationGraphAnalysisState = 
    | Top
    | InnerTop
    | DefinitelyStrict
    | MaybeLazy
    | DefinitelyLazy

type PreInitializationGraphEliminationBinding = 
    { FixupPoints : RecursiveUseFixupPoints
      Binding: Tast.Binding }


/// Check for safety and determine if we need to insert lazy thunks 
let EliminateInitializationGraphs 
      (getTyconBinds: 'TyconDataIn -> PreInitializationGraphEliminationBinding list) 
      (morphTyconBinds: (PreInitializationGraphEliminationBinding list -> Binding list)  -> 'TyconDataIn -> 'TyconDataOut)
      (getLetBinds: 'LetDataIn list -> PreInitializationGraphEliminationBinding list) 
      (morphLetBinds: (PreInitializationGraphEliminationBinding list -> Binding list)  -> 'LetDataIn list -> Binding list)
      g mustHaveArity denv 
      (fixupsAndBindingsWithoutLaziness : MutRecShape<_,_,_,_,_> list) bindsm =

    let recursiveVals = 
        let hash = ValHash<Val>.Create()
        let add (pgrbind: PreInitializationGraphEliminationBinding) = let c = pgrbind.Binding.Var in hash.Add(c,c)
        fixupsAndBindingsWithoutLaziness |> MutRecShapes.iterTyconsAndLets (getTyconBinds >> List.iter add) (getLetBinds >> List.iter add) 
        hash

    // The output of the analysis
    let outOfOrder = ref false
    let runtimeChecks = ref false
    let directRecursiveData = ref false
    let reportedEager = ref false
    let definiteDependencies = ref []

    let rec stripChooseAndExpr e = 
        match stripExpr e with 
        | Expr.TyChoose(_,b,_) -> stripChooseAndExpr b
        | e -> e

    let availIfInOrder = ValHash<_>.Create()
    let check boundv expr = 
        let strict = function
            | MaybeLazy -> MaybeLazy
            | DefinitelyLazy -> DefinitelyLazy
            | Top | DefinitelyStrict | InnerTop -> DefinitelyStrict
        let lzy = function 
            | Top | InnerTop | DefinitelyLazy -> DefinitelyLazy 
            | MaybeLazy | DefinitelyStrict -> MaybeLazy
        let fixable = function 
            | Top | InnerTop -> InnerTop
            | DefinitelyStrict -> DefinitelyStrict
            | MaybeLazy -> MaybeLazy
            | DefinitelyLazy -> DefinitelyLazy

        let rec CheckExpr st e = 
            match stripChooseAndExpr e with 
              // Expressions with some lazy parts 
            | Expr.Lambda (_,_,_,_,b,_,_) -> checkDelayed st b

            // Type-lambdas are analyzed as if they are strict.
            //
            // This is a design decision (See bug 6496), so that generalized recursive bindings such as
            //   let rec x = x
            // are analyzed. Although we give type "x : 'T" to these, from the users point of view
            // any use of "x" will result in an infinite recursion. Type instantiation is implicit in F#
            // because of type inference, which makes it reasonable to check generic bindings strictly.
            | Expr.TyLambda (_,_,b,_,_) -> CheckExpr st b

            | Expr.Obj (_,ty,_,e,overrides,extraImpls,_) ->
                // NOTE: we can't fixup recursive references inside delegates since the closure delegee of a delegate is not accessible 
                // from outside. Object expressions implementing interfaces can, on the other hand, be fixed up. See FSharp 1.0 bug 1469 
                if isInterfaceTy g ty then 
                    List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  checkDelayed st e) overrides
                    List.iter (snd >> List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  checkDelayed st e)) extraImpls
                else 
                    CheckExpr (strict st) e
                    List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  CheckExpr (lzy (strict st)) e) overrides
                    List.iter (snd >> List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  CheckExpr (lzy (strict st)) e)) extraImpls
                
              // Expressions where fixups may be needed 
            | Expr.Val (v,_,m) -> CheckValRef st v m

             // Expressions where subparts may be fixable 
            | Expr.Op((TOp.Tuple _ | TOp.UnionCase _ | TOp.Recd _),_,args,_) -> 
                List.iter (CheckExpr (fixable st)) args

              // Composite expressions 
            | Expr.Const _ -> ()
            | Expr.LetRec (binds,e,_,_)  ->
                binds |> List.iter (CheckBinding (strict st))  
                CheckExpr (strict st) e
            | Expr.Let (bind,e,_,_) ->  
                CheckBinding (strict st) bind 
                CheckExpr (strict st) e
            | Expr.Match (_,_,pt,targets,_,_) -> 
                CheckDecisionTree (strict st) pt 
                Array.iter (CheckDecisionTreeTarget (strict st)) targets 
            | Expr.App(e1,_,_,args,_) -> 
                CheckExpr (strict st) e1  
                List.iter (CheckExpr (strict st)) args 
          // Binary expressions 
            | Expr.Sequential (e1,e2,_,_,_)
            | Expr.StaticOptimization (_,e1,e2,_) ->
                 CheckExpr (strict st) e1;  CheckExpr (strict st) e2
          // n-ary expressions 
            | Expr.Op(op,_,args,m)  -> CheckExprOp st op m;  List.iter (CheckExpr (strict st)) args
          // misc 
            | Expr.Link(eref) -> CheckExpr st !eref
            | Expr.TyChoose (_,b,_)  -> CheckExpr st b
            | Expr.Quote _  -> ()

        and CheckBinding st (TBind(_,e,_)) = CheckExpr st e 
        and CheckDecisionTree st = function
            | TDSwitch(e1,csl,dflt,_) -> CheckExpr st e1; List.iter (fun (TCase(_,d)) -> CheckDecisionTree st d) csl; Option.iter (CheckDecisionTree st) dflt
            | TDSuccess (es,_) -> es |> List.iter (CheckExpr st) 
            | TDBind(bind,e) -> CheckBinding st bind; CheckDecisionTree st e
        and CheckDecisionTreeTarget st (TTarget(_,e,_)) = CheckExpr st e

        and CheckExprOp st op m = 
            match op with 
            | TOp.LValueOp (_,lvr) -> CheckValRef (strict st) lvr m
            | _ -> ()
          
        and CheckValRef st (v: ValRef) m = 
            match st with 
            | MaybeLazy -> 
                if recursiveVals.TryFind v.Deref |> Option.isSome then 
                    warning (RecursiveUseCheckedAtRuntime (denv,v,m)) 
                    if not !reportedEager then 
                      (warning (LetRecCheckedAtRuntime m); reportedEager := true)
                    runtimeChecks := true

            | Top | DefinitelyStrict ->
                if recursiveVals.TryFind v.Deref |> Option.isSome then 
                    if availIfInOrder.TryFind v.Deref |> Option.isNone then 
                        warning (LetRecEvaluatedOutOfOrder (denv,boundv,v,m)) 
                        outOfOrder := true
                        if not !reportedEager then 
                          (warning (LetRecCheckedAtRuntime m); reportedEager := true)
                    definiteDependencies := (boundv,v) :: !definiteDependencies
            | InnerTop -> 
                if recursiveVals.TryFind v.Deref |> Option.isSome then 
                    directRecursiveData := true
            | DefinitelyLazy -> () 
        and checkDelayed st b = 
            match st with 
            | MaybeLazy | DefinitelyStrict -> CheckExpr MaybeLazy b
            | DefinitelyLazy | Top | InnerTop -> () 
          
       
        CheckExpr Top expr
   

    // Check the bindings one by one, each w.r.t. the previously available set of binding
    begin
        let checkBind (pgrbind: PreInitializationGraphEliminationBinding) = 
            let (TBind(v,e,_)) = pgrbind.Binding
            check (mkLocalValRef v) e 
            availIfInOrder.Add(v, 1)
        fixupsAndBindingsWithoutLaziness |> MutRecShapes.iterTyconsAndLets (getTyconBinds >> List.iter checkBind) (getLetBinds >> List.iter checkBind) 
    end
    
    // ddg = definiteDependencyGraph 
    let ddgNodes = recursiveVals.Values |> Seq.toList |> List.map mkLocalValRef
    let ddg = Graph<ValRef, Stamp>((fun v -> v.Stamp), ddgNodes, !definiteDependencies )
    ddg.IterateCycles (fun path -> error (LetRecUnsound (denv,path,path.Head.Range))) 

    let requiresLazyBindings = !runtimeChecks || !outOfOrder
    if !directRecursiveData && requiresLazyBindings then 
        error(Error(FSComp.SR.tcInvalidMixtureOfRecursiveForms(),bindsm))

    if requiresLazyBindings then 
        let morphBinding (pgrbind: PreInitializationGraphEliminationBinding) =
            let (RecursiveUseFixupPoints(fixupPoints)) = pgrbind.FixupPoints
            let (TBind(v,e,seqPtOpt)) = pgrbind.Binding
            match stripChooseAndExpr e with
            | Expr.Lambda _ | Expr.TyLambda _ -> 
                [],[mkInvisibleBind v e] 
            | _ -> 
                let ty = v.Type
                let m = v.Range
                let vty = (mkLazyTy g ty)

                let fty = (g.unit_ty --> ty)
                let flazy,felazy = Tastops.mkCompGenLocal m  v.LogicalName fty 
                let frhs = mkUnitDelayLambda g m e
                if mustHaveArity then flazy.SetValReprInfo (Some(InferArityOfExpr g fty [] [] frhs))

                let vlazy,velazy = Tastops.mkCompGenLocal m  v.LogicalName vty 
                let vrhs = (mkLazyDelayed g m ty felazy)
                       
                if mustHaveArity then vlazy.SetValReprInfo (Some(InferArityOfExpr g vty [] [] vrhs))
                fixupPoints |> List.iter (fun (fp,_) -> fp := mkLazyForce g (!fp).Range ty velazy)

                [mkInvisibleBind flazy frhs; mkInvisibleBind vlazy vrhs],
                [mkBind seqPtOpt v (mkLazyForce g m ty velazy)]

        let newTopBinds = ResizeArray<_>()
        let morphBindings pgrbinds = pgrbinds |> List.map morphBinding |> List.unzip |> (fun (a,b) -> newTopBinds.Add (List.concat a); List.concat b)

        let res = fixupsAndBindingsWithoutLaziness |> MutRecShapes.map (morphTyconBinds morphBindings) (morphLetBinds morphBindings) id 
        if newTopBinds.Count = 0 then res
        else MutRecShape.Lets (List.concat newTopBinds) :: res
    else
        let noMorph (pgrbinds : PreInitializationGraphEliminationBinding list) = pgrbinds |> List.map (fun pgrbind -> pgrbind.Binding) 
        fixupsAndBindingsWithoutLaziness |> MutRecShapes.map (morphTyconBinds noMorph) (morphLetBinds noMorph) id 

//-------------------------------------------------------------------------
// Check the shape of an object constructor and rewrite calls 
//------------------------------------------------------------------------- 

let CheckAndRewriteObjectCtor g env (ctorLambaExpr:Expr) =

    let m = ctorLambaExpr.Range
    let tps,vsl,body,returnTy = stripTopLambda (ctorLambaExpr,tyOfExpr g ctorLambaExpr)

    // Rewrite legitimate self-construction calls to CtorValUsedAsSelfInit 
    let error (expr:Expr) = 
        errorR(Error(FSComp.SR.tcInvalidObjectConstructionExpression(),expr.Range))
        expr

    // Build an assignment into the safeThisValOpt mutable reference cell that holds recursive references to 'this' 
    // Build an assignment into the safeInitInfo mutable field that indicates that partial initialization is successful
    let rewriteConstruction recdExpr = 
       match env.eCtorInfo with 
       | None -> recdExpr
       | Some ctorInfo -> 
           let recdExpr = 
               match ctorInfo.safeThisValOpt with 
               | None -> recdExpr
               | Some safeInitVal -> 
                   let ty = tyOfExpr g recdExpr
                   let thisExpr = mkGetArg0 m ty
                   let setExpr = mkRefCellSet g  m ty (exprForValRef m (mkLocalValRef safeInitVal)) thisExpr
                   Expr.Sequential(recdExpr,setExpr,ThenDoSeq,SuppressSequencePointOnExprOfSequential,m)
           let recdExpr =                        
               match ctorInfo.safeInitInfo with 
               | NoSafeInitInfo -> recdExpr
               | SafeInitField (rfref, _) -> 
                   let thisTy = tyOfExpr g recdExpr
                   let thisExpr = mkGetArg0 m thisTy
                   let thisTyInst = argsOfAppTy g thisTy
                   let setExpr = mkRecdFieldSetViaExprAddr (thisExpr, rfref, thisTyInst, mkOne g m, m)
                   Expr.Sequential(recdExpr,setExpr,ThenDoSeq,SuppressSequencePointOnExprOfSequential,m)
           recdExpr
       

    let rec checkAndRewrite (expr:Expr) = 
        match expr with 
        // <ctor-body> = { fields } 
        // The constructor ends in an object initialization expression - good 
        | Expr.Op(TOp.Recd(RecdExprIsObjInit,_),_,_,_) -> rewriteConstruction expr

        // <ctor-body> = "a; <ctor-body>" 
        | Expr.Sequential(a,body,NormalSeq,spSeq,b)  -> Expr.Sequential(a,checkAndRewrite body,NormalSeq,spSeq,b) 

        // <ctor-body> = "<ctor-body> then <expr>" 
        | Expr.Sequential(body,a,ThenDoSeq,spSeq,b) -> Expr.Sequential(checkAndRewrite body,a,ThenDoSeq,spSeq,b)

        // <ctor-body> = "let pat = expr in <ctor-body>" 
        | Expr.Let(bind,body,m,_)  -> mkLetBind m bind (checkAndRewrite body)

        // The constructor is a sequence "let pat = expr in <ctor-body>" 
        | Expr.Match(spBind,a,b,targets,c,d)  -> Expr.Match(spBind,a,b, (targets |> Array.map (fun (TTarget(vs,body,spTarget)) -> TTarget(vs, checkAndRewrite body,spTarget))),c,d)

        // <ctor-body> = "let rec binds in <ctor-body>" 
        | Expr.LetRec(a,body,_,_) -> Expr.LetRec (a,checkAndRewrite body ,m,NewFreeVarsCache())

        // <ctor-body> = "new C(...)" 
        | Expr.App(f,b,c,d,m) -> 
            // The application had better be an application of a ctor 
            let f = checkAndRewriteCtorUsage f
            let expr = Expr.App(f,b,c,d,m)
            rewriteConstruction expr 

        | _ -> 
            error(expr)

    and checkAndRewriteCtorUsage expr = 
         match expr with 
         | Expr.Link eref -> 
               let e = checkAndRewriteCtorUsage !eref
               eref := e
               expr
               
         // Type applications are ok, e.g. 
         //     type C<'a>(x:int) = 
         //         new() = C<'a>(3) 
         | Expr.App(f,fty,tyargs,[],m) -> 
             let f = checkAndRewriteCtorUsage f
             Expr.App(f,fty,tyargs,[],m)

         // Self-calls are OK and get rewritten. 
         | Expr.Val(vref,NormalValUse,a) ->
           let isCtor = 
               match vref.MemberInfo with 
               | None -> false
               | Some memberInfo -> memberInfo.MemberFlags.MemberKind = MemberKind.Constructor

           if not isCtor then 
               error expr 
           else
               Expr.Val(vref,CtorValUsedAsSelfInit,a)
         | _ -> 
            error(expr)
    
    let body = checkAndRewrite body
    mkMultiLambdas m tps vsl (body, returnTy) 
    


/// Post-typechecking normalizations to enforce semantic constraints
/// lazy and, lazy or, rethrow, address-of
let buildApp cenv expr exprty arg m = 
    match expr,arg with        
    | ApplicableExpr(_, Expr.App(Expr.Val(vf,_,_),_,_,[x0],_),_) , _ 
         when valRefEq cenv.g vf cenv.g.and_vref 
           || valRefEq cenv.g vf cenv.g.and2_vref  -> 
        MakeApplicableExprNoFlex cenv (mkLazyAnd cenv.g m x0 arg)
    | ApplicableExpr(_, Expr.App(Expr.Val(vf,_,_),_,_,[x0],_),_), _ 
         when valRefEq cenv.g vf cenv.g.or_vref
           || valRefEq cenv.g vf cenv.g.or2_vref -> 
        MakeApplicableExprNoFlex cenv (mkLazyOr cenv.g m x0 arg )
    | ApplicableExpr(_, Expr.App(Expr.Val(vf,_,_),_,_,[],_),_), _ 
         when valRefEq cenv.g vf cenv.g.reraise_vref -> 
        // exprty is of type: "unit -> 'a". Break it and store the 'a type here, used later as return type. 
        let _unit_ty,rtn_ty = destFunTy cenv.g exprty 
        MakeApplicableExprNoFlex cenv (mkCompGenSequential m arg (mkReraise m rtn_ty))
    | ApplicableExpr(_, Expr.App(Expr.Val(vf,_,_),_,_,[],_),_), _ 
         when (valRefEq cenv.g vf cenv.g.addrof_vref || 
               valRefEq cenv.g vf cenv.g.addrof2_vref) -> 
        if valRefEq cenv.g vf cenv.g.addrof2_vref then warning(UseOfAddressOfOperator(m))
        let wrap,e1a' = mkExprAddrOfExpr cenv.g true false DefinitelyMutates arg (Some(vf)) m
        MakeApplicableExprNoFlex cenv (wrap(e1a'))
    | _ -> 
        expr.SupplyArgument(arg,m)             

//-------------------------------------------------------------------------
// Additional data structures used by type checking
//------------------------------------------------------------------------- 

type DelayedItem = 
  /// DelayedTypeApp (typeArgs, mTypeArgs, mExprAndTypeArgs)
  ///
  /// Represents the <tyargs> in "item<tyargs>"
  | DelayedTypeApp of Ast.SynType list * range * range

  /// DelayedApp (isAtomic, argExpr, mFuncAndArg) 
  ///
  /// Represents the args in "item args", or "item.[args]".
  | DelayedApp of ExprAtomicFlag * Ast.SynExpr * range

  /// Represents the long identifiers in "item.Ident1", or "item.Ident1.Ident2" etc.
  | DelayedDotLookup of Ast.Ident list * range

  /// Represents an incomplete "item."
  | DelayedDot
 
  /// Represents the valueExpr in "item <- valueExpr", also "item.[indexerArgs] <- valueExpr" etc.
  | DelayedSet of Ast.SynExpr * range

let MakeDelayedSet(e: SynExpr, m) = 
    // We have longId <- e. Wrap 'e' in another pair of parentheses to ensure it's never interpreted as 
    // a named argument, e.g. for "el.Checked <- (el = el2)" 
    DelayedSet (SynExpr.Paren(e, range0, None, e.Range), m)

type NewSlotsOK = 
    | NewSlotsOK 
    | NoNewSlots


type ImplictlyBoundTyparsAllowed = 
    | NewTyparsOKButWarnIfNotRigid 
    | NewTyparsOK 
    | NoNewTypars

type CheckConstraints = 
    | CheckCxs 
    | NoCheckCxs

type TypeRealizationPass = 
    | FirstPass 
    | SecondPass 

type MemberOrValContainerInfo = 
    | MemberOrValContainerInfo of
          TyconRef *                        // tcref: The logical apparent parent of a value/member, either a module, type or exception 
          (TType * SlotImplSet) option * // optIntfSlotTy
          Val option *                      // baseValOpt
          SafeInitData *                      // safeInitInfo
          Typars                            // declaredTyconTypars

/// Provides information about the context for a value or member definition 
type ContainerInfo = 
    | ContainerInfo of 
          // The nearest containing module. Used as the 'actual' parent for extension members and values 
          ParentRef *  
          // For members:
          MemberOrValContainerInfo option
    member x.ParentRef =
        let (ContainerInfo(v,_)) = x 
        v
    
/// Indicates a declaration is contained in an expression 
let ExprContainerInfo = ContainerInfo(ParentNone,None)
/// Indicates a declaration is contained in the given module 
let ModuleOrNamespaceContainerInfo modref = ContainerInfo(Parent(modref),Some(MemberOrValContainerInfo(modref,None,None,NoSafeInitInfo,[])))
/// Indicates a declaration is contained in the given type definition in the given module 
let TyconContainerInfo (parent, tcref, declaredTyconTypars, safeInitInfo) = ContainerInfo(parent,Some(MemberOrValContainerInfo(tcref,None,None,safeInitInfo,declaredTyconTypars)))

type NormalizedRecBindingDefn = NormalizedRecBindingDefn of ContainerInfo * NewSlotsOK * DeclKind * NormalizedBinding

type TyconBindingDefn = TyconBindingDefn of ContainerInfo * NewSlotsOK * DeclKind * SynMemberDefn * range

type ValSpecResult = ValSpecResult of ParentRef * ValMemberInfoTransient option * Ident * Typars * Typars * TType * PartialValReprInfo * DeclKind 

//-------------------------------------------------------------------------
// Additional data structures used by checking recursive bindings
//------------------------------------------------------------------------- 


type RecDefnBindingInfo = RecDefnBindingInfo of ContainerInfo * NewSlotsOK * DeclKind * SynBinding

type MutRecDataForOpen = MutRecDataForOpen of LongIdent * range
type MutRecDataForModuleAbbrev = MutRecDataForModuleAbbrev of Ident * LongIdent * range

type MutRecSigsInitialData = MutRecShape<SynTypeDefnSig, SynValSig, SynComponentInfo, MutRecDataForModuleAbbrev, MutRecDataForOpen > list
type MutRecDefnsInitialData = MutRecShape<SynTypeDefn, SynBinding list, SynComponentInfo, MutRecDataForModuleAbbrev, MutRecDataForOpen > list

type MutRecDefnsPhase1DataForTycon = MutRecDefnsPhase1DataForTycon of SynComponentInfo * SynTypeDefnSimpleRepr * (SynType * range) list * preEstablishedHasDefaultCtor: bool * hasSelfReferentialCtor: bool * isAtOriginalTyconDefn: bool
type MutRecDefnsPhase1Data = MutRecShape<MutRecDefnsPhase1DataForTycon * SynMemberDefn list, RecDefnBindingInfo list, SynComponentInfo, MutRecDataForModuleAbbrev, MutRecDataForOpen > list

type MutRecDefnsPhase2DataForTycon = MutRecDefnsPhase2DataForTycon of Tycon option * ParentRef * DeclKind * TyconRef * Val option * SafeInitData * Typars * SynMemberDefn list * range * NewSlotsOK * fixupFinalAttribs: (unit -> unit)
type MutRecDefnsPhase2DataForModule = MutRecDefnsPhase2DataForModule of ModuleOrNamespaceType ref * ModuleOrNamespace
type MutRecDefnsPhase2Data = MutRecShape<MutRecDefnsPhase2DataForTycon, RecDefnBindingInfo list,  MutRecDefnsPhase2DataForModule * TcEnv, MutRecDataForModuleAbbrev, MutRecDataForOpen > list

type MutRecDefnsPhase2InfoForTycon = MutRecDefnsPhase2InfoForTycon of Tycon option * TyconRef * Typars * DeclKind * TyconBindingDefn list * fixupFinalAttrs: (unit -> unit)
type MutRecDefnsPhase2Info = MutRecShape<MutRecDefnsPhase2InfoForTycon, RecDefnBindingInfo list, MutRecDefnsPhase2DataForModule * TcEnv, MutRecDataForModuleAbbrev, MutRecDataForOpen > list


/// RecursiveBindingInfo - flows through initial steps of TcLetrec 
type RecursiveBindingInfo =
    | RBInfo of
          int * // index of the binding in the recursive group
          ContainerInfo * 
          Typars * 
          ValInline * 
          Val * 
          ExplicitTyparInfo * 
          PartialValReprInfo * 
          ValMemberInfoTransient option  * 
          Val option * 
          Val option * 
          SafeInitData * 
          SynAccess option * 
          TType * 
          DeclKind

    member x.EnclosingDeclaredTypars = let (RBInfo(_,_,enclosingDeclaredTypars,_,_,_,_,_,_,_,_,_,_,_)) = x in enclosingDeclaredTypars
    member x.Val                     = let (RBInfo(_,_,_,_,vspec,_,_,_,_,_,_,_,_,_)) = x in vspec
    member x.ExplicitTyparInfo       = let (RBInfo(_,_,_,_,_,flex,_,_,_,_,_,_,_,_)) = x in flex
    member x.DeclaredTypars          = let (ExplicitTyparInfo(_,declaredTypars,_)) = x.ExplicitTyparInfo in declaredTypars
    member x.Index                   = let (RBInfo(i,_,_,_,_,_,_,_,_,_,_,_,_,_)) = x in i
    member x.ContainerInfo           = let (RBInfo(_,c,_,_,_,_,_,_,_,_,_,_,_,_)) = x in c
    member x.DeclKind                = let (RBInfo(_,_,_,_,_,_,_,_,_,_,_,_,_,declKind)) = x in declKind

type PreCheckingRecursiveBinding = 
    { SyntacticBinding : NormalizedBinding 
      RecBindingInfo : RecursiveBindingInfo }


type PreGeneralizationRecursiveBinding = 
    { ExtraGeneralizableTypars : Typars
      CheckedBinding: CheckedBindingInfo
      RecBindingInfo : RecursiveBindingInfo }

type PostGeneralizationRecursiveBinding = 
    { ValScheme : ValScheme
      CheckedBinding: CheckedBindingInfo
      RecBindingInfo : RecursiveBindingInfo }
    member x.GeneralizedTypars = x.ValScheme.GeneralizedTypars

type PostBindCtorThisVarRefCellRecursiveBinding = 
    { ValScheme: ValScheme
      Binding: Tast.Binding }


let CanInferExtraGeneralizedTyparsForRecBinding (pgrbind: PreGeneralizationRecursiveBinding) = 
    let flex = pgrbind.RecBindingInfo.ExplicitTyparInfo
    let (ExplicitTyparInfo(_,_,canInferTypars)) = flex
    let memFlagsOpt = pgrbind.RecBindingInfo.Val.MemberInfo |> Option.map (fun memInfo -> memInfo.MemberFlags)
    let canInferTypars = GeneralizationHelpers.ComputeCanInferExtraGeneralizableTypars (pgrbind.RecBindingInfo.ContainerInfo.ParentRef, canInferTypars, memFlagsOpt)
    canInferTypars 
    

/// Get the "this" variable from an instance member binding
let GetInstanceMemberThisVariable (v:Val,x) =
    // Skip over LAM tps. Choose 'a. 
    if v.IsInstanceMember then
        let rec firstArg e =
          match e with
            | Expr.TyLambda (_,_,b,_,_) -> firstArg b
            | Expr.TyChoose (_,b,_) -> firstArg b
            | Expr.Lambda  (_,_,_,[v],_,_,_) -> Some v
            | _ -> failwith "GetInstanceMemberThisVariable: instance member did not have expected internal form"
       
        firstArg x
    else
        None

//-------------------------------------------------------------------------
// Checking types and type constraints
//------------------------------------------------------------------------- 
/// Check specifications of contraints on type parameters 
let rec TcTyparConstraint ridx cenv newOk checkCxs occ (env: TcEnv) tpenv c = 
    let checkSimpleConstraint tp m constraintAdder =
        let tp',tpenv = TcTypar cenv env newOk tpenv tp
        constraintAdder env.DisplayEnv cenv.css m NoTrace (mkTyparTy tp') 
        tpenv 

    match c with 
    | WhereTyparDefaultsToType(tp,ty,m) ->
        let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv ty
        let tp',tpenv = TcTypar cenv env newOk tpenv tp
        let csenv = MakeConstraintSolverEnv env.eContextInfo cenv.css m env.DisplayEnv
        AddConstraint csenv 0 m NoTrace tp' (TyparConstraint.DefaultsTo(ridx,ty',m)) |> CommitOperationResult
        tpenv

    | WhereTyparSubtypeOfType(tp,ty,m) ->
        let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs ItemOccurence.UseInType env tpenv ty
        let tp',tpenv = TcTypar cenv env newOk tpenv tp
        if newOk = NoNewTypars && isSealedTy cenv.g ty' then 
            errorR(Error(FSComp.SR.tcInvalidConstraintTypeSealed(),m))
        AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m NoTrace ty' (mkTyparTy tp') 
        tpenv

    | WhereTyparSupportsNull(tp,m) -> checkSimpleConstraint tp m AddCxTypeMustSupportNull

    | WhereTyparIsComparable(tp,m) -> checkSimpleConstraint tp m AddCxTypeMustSupportComparison

    | WhereTyparIsEquatable(tp,m) -> checkSimpleConstraint tp m AddCxTypeMustSupportEquality 

    | WhereTyparIsReferenceType(tp,m) -> checkSimpleConstraint tp m AddCxTypeIsReferenceType

    | WhereTyparIsValueType(tp,m) -> checkSimpleConstraint tp m AddCxTypeIsValueType
       
    | WhereTyparIsUnmanaged(tp,m) -> checkSimpleConstraint tp m AddCxTypeIsUnmanaged

    | WhereTyparIsEnum(tp,tyargs,m) ->
        let tp',tpenv = TcTypar cenv env newOk tpenv tp
        let tpenv = 
            match tyargs with 
            | [underlying] -> 
                let underlying',tpenv = TcTypeAndRecover cenv newOk checkCxs ItemOccurence.UseInType env tpenv underlying
                AddCxTypeIsEnum env.DisplayEnv cenv.css m NoTrace (mkTyparTy tp') underlying'
                tpenv
            | _ -> 
                errorR(Error(FSComp.SR.tcInvalidEnumConstraint(),m))
                tpenv
        tpenv

    | WhereTyparIsDelegate(tp,tyargs,m) ->
        let tp',tpenv = TcTypar cenv env newOk tpenv tp
        match tyargs with 
        | [a;b] -> 
            let a',tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv a
            let b',tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv b
            AddCxTypeIsDelegate env.DisplayEnv cenv.css m NoTrace (mkTyparTy tp') a' b'
            tpenv
        | _ -> 
            errorR(Error(FSComp.SR.tcInvalidEnumConstraint(),m))
            tpenv

    | WhereTyparSupportsMember(tps,memSpfn,m) ->
        let traitInfo,tpenv = TcPseudoMemberSpec cenv newOk env tps tpenv memSpfn m
        match traitInfo with 
        | TTrait(objtys,".ctor",memberFlags,argtys,returnTy,_) when  memberFlags.MemberKind = MemberKind.Constructor ->
            match objtys,argtys with 
            | [ty],[] when typeEquiv cenv.g ty (GetFSharpViewOfReturnType cenv.g returnTy) ->
                AddCxTypeMustSupportDefaultCtor env.DisplayEnv cenv.css m NoTrace ty 
                tpenv
            | _ ->            
                errorR(Error(FSComp.SR.tcInvalidNewConstraint(),m))
                tpenv
        | _ ->  
            AddCxMethodConstraint env.DisplayEnv cenv.css m NoTrace traitInfo
            tpenv
      
and TcPseudoMemberSpec cenv newOk env synTypes tpenv memSpfn m = 
#if ALLOW_MEMBER_CONSTRAINTS_ON_MEASURES
    let tps,tpenv = List.mapFold (TcTyparOrMeasurePar None cenv env newOk) tpenv synTypars
#else
    let tys,tpenv = List.mapFold (TcTypeAndRecover cenv newOk CheckCxs ItemOccurence.UseInType env) tpenv synTypes
#endif
    match memSpfn with 
    | SynMemberSig.Member (valSpfn,memberFlags,m) ->
        // REVIEW: Test pseudo constraints cannot refer to polymorphic methods.
        // REVIEW: Test pseudo constraints cannot be curried. 
        let members,tpenv = TcValSpec cenv env ModuleOrMemberBinding newOk (ExprContainerInfo) (Some memberFlags) (Some (List.head tys)) tpenv valSpfn []
        match members with 
        | [ValSpecResult(_,_,id,_,_,memberConstraintTy,partialValReprInfo,_)] -> 
            let memberConstraintTypars,_ = tryDestForallTy cenv.g memberConstraintTy
            let topValInfo = TranslatePartialArity memberConstraintTypars partialValReprInfo
            let _,curriedArgInfos,returnTy,_ = GetTopValTypeInCompiledForm cenv.g topValInfo memberConstraintTy m
            //if curriedArgInfos.Length > 1 then  error(Error(FSComp.SR.tcInvalidConstraint(),m))
            let argtys = List.concat curriedArgInfos
            let argtys = List.map fst argtys
            let logicalCompiledName = ComputeLogicalName id memberFlags

            let item = Item.ArgName (id, memberConstraintTy, None)
            CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)

            TTrait(tys,logicalCompiledName,memberFlags,argtys,returnTy, ref None),tpenv
        | _ -> error(Error(FSComp.SR.tcInvalidConstraint(),m))
    | _ -> error(Error(FSComp.SR.tcInvalidConstraint(),m))


/// Check a value specification, e.g. in a signature, interface declaration or a constraint
and TcValSpec cenv env declKind newOk containerInfo memFlagsOpt thisTyOpt tpenv valSpfn attrs =
    let (ValSpfn(_, id, SynValTyparDecls(synTypars, _, synTyparConstraints), ty, valSynInfo, _, _, _, _, _, m)) = valSpfn 
    let declaredTypars = TcTyparDecls cenv env synTypars
    let (ContainerInfo(altActualParent,tcrefContainerInfo)) = containerInfo
    let enclosingDeclaredTypars,memberContainerInfo,thisTyOpt,declKind = 
        match tcrefContainerInfo with 
        | Some(MemberOrValContainerInfo(tcref,_,_,_,declaredTyconTypars)) -> 
            let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
            let _,enclosingDeclaredTypars,_,_,thisTy = FreshenObjectArgType cenv m TyparRigidity.Rigid tcref isExtrinsic declaredTyconTypars
            // An implemented interface type is in terms of the type's type parameters. 
            // We need a signature in terms of the values' type parameters. 
            // let optIntfSlotTy = Option.map (instType renaming) optIntfSlotTy in  
            enclosingDeclaredTypars,Some(tcref),Some thisTy,declKind
        | None -> 
            [],None,thisTyOpt, ModuleOrMemberBinding
    let allDeclaredTypars = enclosingDeclaredTypars @ declaredTypars
    let envinner = AddDeclaredTypars NoCheckForDuplicateTypars allDeclaredTypars env
    let checkCxs = CheckCxs
    let tpenv = TcTyparConstraints cenv newOk checkCxs ItemOccurence.UseInType envinner tpenv synTyparConstraints
    
    // Treat constraints at the "end" of the type as if they are declared.
    // This is by far the most convenient place to locate the constraints.
    // e.g. 
    //    val FastGenericComparer<'T>  : IComparer<'T> when 'T : comparison 
    let tpenv = 
        match ty with 
        | SynType.WithGlobalConstraints(_,wcs,_) ->
            TcTyparConstraints cenv newOk checkCxs ItemOccurence.UseInType envinner tpenv wcs
        | _ -> 
            tpenv

    // Enforce "no undeclared constraints allowed on declared typars"
    allDeclaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m)
    // Process the type, including any constraints 
    let declaredTy,tpenv = TcTypeAndRecover cenv newOk checkCxs ItemOccurence.UseInType envinner tpenv ty  

    match memFlagsOpt,thisTyOpt with 
    | Some memberFlags, Some thisTy -> 
        let generateOneMember(memberFlags) = 
        
            // Decode members in the signature
            let ty',valSynInfo = 
                match memberFlags.MemberKind with 
                | MemberKind.ClassConstructor
                | MemberKind.Constructor
                | MemberKind.Member -> 
                    declaredTy,valSynInfo
                | MemberKind.PropertyGet 
                | MemberKind.PropertySet ->  
                    let fakeArgReprInfos = [ for n in SynInfo.AritiesOfArgs valSynInfo do yield [ for _ in 1 .. n do yield ValReprInfo.unnamedTopArg1 ] ]
                    let arginfos,returnTy = GetTopTauTypeInFSharpForm cenv.g fakeArgReprInfos declaredTy m
                    if arginfos.Length > 1 then error(Error(FSComp.SR.tcInvalidPropertyType(),m))
                    match memberFlags.MemberKind with 
                    | MemberKind.PropertyGet ->
                        if SynInfo.HasNoArgs valSynInfo then 
                          (cenv.g.unit_ty --> declaredTy), (SynInfo.IncorporateEmptyTupledArgForPropertyGetter valSynInfo)
                        else
                          declaredTy,valSynInfo
                    | _ -> 
                        let setterTy = (mkRefTupledTy cenv.g (List.map fst (List.concat arginfos) @ [returnTy]) --> cenv.g.unit_ty)
                        let synInfo = SynInfo.IncorporateSetterArg valSynInfo
                        setterTy, synInfo
                | MemberKind.PropertyGetSet -> 
                    error(InternalError("Unexpected MemberKind.PropertyGetSet from signature parsing",m))

            // Take "unit" into account in the signature
            let valSynInfo = AdjustValSynInfoInSignature cenv.g ty' valSynInfo

            let ty',valSynInfo = 
                if memberFlags.IsInstance then 
                  (thisTy --> ty'), (SynInfo.IncorporateSelfArg valSynInfo)
                else
                  ty',valSynInfo

            let reallyGenerateOneMember(id:Ident,valSynInfo,ty',memberFlags) = 
                let (PartialValReprInfo(argsData,_)) as partialValReprInfo = 
                    TranslateTopValSynInfo id.idRange (TcAttributes cenv env) valSynInfo


                // Fold in the optional argument information 
                // Resort to using the syntactic argument information since that is what tells us 
                // what is optional and what is not. 
                let ty' = 

                    if SynInfo.HasOptionalArgs valSynInfo then 
                        let argtysl,returnTy = GetTopTauTypeInFSharpForm cenv.g argsData ty' m
                        let argtysl = 
                            (List.zip (List.mapSquared fst argtysl) valSynInfo.ArgInfos) 
                            |> List.map (fun (argtys,argInfos) ->
                                 (List.zip argtys argInfos)
                                 |> List.map (fun (argty,argInfo) ->
                                     if SynInfo.IsOptionalArg argInfo then mkOptionTy cenv.g argty
                                     else argty))
                        mkIteratedFunTy (List.map (mkRefTupledTy cenv.g) argtysl) returnTy
                    else ty' 
                        
                let memberInfoOpt = 
                    match memberContainerInfo with 
                    | Some tcref -> 
                        let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
                        let memberInfoTransient = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,isExtrinsic,attrs,[],memberFlags,valSynInfo,id,false)
                        Some memberInfoTransient
                    | None -> 
                        None
            
                ValSpecResult(altActualParent,memberInfoOpt,id,enclosingDeclaredTypars,declaredTypars,ty',partialValReprInfo,declKind)

            [ yield reallyGenerateOneMember(id,valSynInfo,ty',memberFlags)
              if CompileAsEvent cenv.g attrs then 
                    let valSynInfo = EventDeclarationNormalization.ConvertSynInfo id.idRange valSynInfo
                    let memberFlags = EventDeclarationNormalization.ConvertMemberFlags memberFlags
                    let delTy = FindDelegateTypeOfPropertyEvent cenv.g cenv.amap id.idText id.idRange declaredTy 
                    let ty = 
                       if memberFlags.IsInstance then 
                         thisTy --> (delTy --> cenv.g.unit_ty)
                       else 
                         (delTy --> cenv.g.unit_ty)
                    yield reallyGenerateOneMember(ident("add_" + id.idText,id.idRange),valSynInfo,ty,memberFlags)
                    yield reallyGenerateOneMember(ident("remove_" + id.idText,id.idRange),valSynInfo,ty,memberFlags) ]
                
              
            
        match memberFlags.MemberKind with 
        | MemberKind.ClassConstructor
        | MemberKind.Constructor
        | MemberKind.Member 
        | MemberKind.PropertyGet 
        | MemberKind.PropertySet ->
            generateOneMember(memberFlags), tpenv
        | MemberKind.PropertyGetSet ->
            [ yield! generateOneMember({memberFlags with MemberKind=MemberKind.PropertyGet})
              yield! generateOneMember({memberFlags with MemberKind=MemberKind.PropertySet}) ], tpenv
    | _ ->
        let valSynInfo = AdjustValSynInfoInSignature cenv.g declaredTy valSynInfo
        let partialValReprInfo = TranslateTopValSynInfo id.idRange (TcAttributes cenv env) valSynInfo
        [ ValSpecResult(altActualParent,None,id,enclosingDeclaredTypars,declaredTypars,declaredTy,partialValReprInfo,declKind) ], tpenv


//-------------------------------------------------------------------------
// Bind types 
//------------------------------------------------------------------------- 

/// Check and elaborate a type or measure parameter occurrence
/// If optKind=Some kind, then this is the kind we're expecting (we're in *analysis* mode)
/// If optKind=None, we need to determine the kind (we're in *synthesis* mode)
///
and TcTyparOrMeasurePar optKind cenv (env:TcEnv) newOk tpenv (Typar(id,_,_) as tp) =
    let checkRes (res:Typar) =
        match optKind, res.Kind with
        | Some TyparKind.Measure, TyparKind.Type -> error (Error(FSComp.SR.tcExpectedUnitOfMeasureMarkWithAttribute(), id.idRange)); res, tpenv
        | Some TyparKind.Type, TyparKind.Measure -> error (Error(FSComp.SR.tcExpectedTypeParameter(), id.idRange)); res, tpenv
        | _, _ -> 
            let item = Item.TypeVar(id.idText, res)
            CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.UseInType,env.DisplayEnv,env.eAccessRights)
            // record the ' as well for tokenization
            // CallNameResolutionSink cenv.tcSink (tp.Range.StartRange,env.NameEnv,item,item,ItemOccurence.UseInType,env.DisplayEnv,env.eAccessRights)
            res, tpenv
    let key = id.idText
    match env.eNameResEnv.eTypars.TryFind key with
    | Some res -> checkRes res
    | None -> 
    match TryFindUnscopedTypar key tpenv with
    | Some res -> checkRes res
    | None -> 
        if newOk = NoNewTypars then
            let predictTypeParameters() =
                let predictions1 =
                    env.eNameResEnv.eTypars
                    |> Seq.map (fun p -> "'" + p.Key)
                    |> Set.ofSeq

                let predictions2 =
                    match tpenv with
                    | UnscopedTyparEnv elements ->
                        elements
                        |> Seq.map (fun p -> "'" + p.Key)
                        |> Set.ofSeq

                Set.union predictions1 predictions2
            
            let reportedId = Ident("'" + id.idText,id.idRange)
            error (UndefinedName(0,FSComp.SR.undefinedNameTypeParameter,reportedId,predictTypeParameters))
        
        // OK, this is an implicit declaration of a type parameter 
        // The kind defaults to Type
        let tp' = NewTypar ((match optKind with None -> TyparKind.Type | Some kind -> kind), TyparRigidity.WarnIfNotRigid,tp,false,TyparDynamicReq.Yes,[],false,false)
        let item = Item.TypeVar(id.idText, tp')
        CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.UseInType,env.DisplayEnv,env.eAccessRights)
        tp',AddUnscopedTypar key tp' tpenv

and TcTypar cenv env newOk tpenv tp =
    TcTyparOrMeasurePar (Some TyparKind.Type) cenv env newOk tpenv tp

and TcTyparDecl cenv env (TyparDecl(synAttrs,(Typar(id,_,_) as stp))) =
    let attrs = TcAttributes cenv env AttributeTargets.GenericParameter synAttrs
    let hasMeasureAttr = HasFSharpAttribute cenv.g cenv.g.attrib_MeasureAttribute attrs
    let hasEqDepAttr = HasFSharpAttribute cenv.g cenv.g.attrib_EqualityConditionalOnAttribute attrs
    let hasCompDepAttr = HasFSharpAttribute cenv.g cenv.g.attrib_ComparisonConditionalOnAttribute attrs
    let attrs = attrs |> List.filter (IsMatchingFSharpAttribute cenv.g cenv.g.attrib_MeasureAttribute >> not)
    let tp = NewTypar ((if hasMeasureAttr then TyparKind.Measure else TyparKind.Type), TyparRigidity.WarnIfNotRigid,stp,false,TyparDynamicReq.Yes,attrs,hasEqDepAttr,hasCompDepAttr)
    match TryFindFSharpStringAttribute cenv.g cenv.g.attrib_CompiledNameAttribute attrs with 
    | Some compiledName -> 
        tp.typar_il_name <- Some compiledName
    | None ->  
        ()
    let item = Item.TypeVar(id.idText, tp)
    CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.UseInType,env.DisplayEnv,env.eAccessRights)
    tp
    

and TcTyparDecls cenv env synTypars = List.map (TcTyparDecl cenv env) synTypars

/// Check and elaborate a syntactic type or measure
/// If optKind=Some kind, then this is the kind we're expecting (we're in *analysis* mode)
/// If optKind=None, we need to determine the kind (we're in *synthesis* mode)
///
and TcTypeOrMeasure optKind cenv newOk checkCxs occ env (tpenv:SyntacticUnscopedTyparEnv) ty =

    match ty with 
    | SynType.LongIdent(LongIdentWithDots([],_)) -> 
        // special case when type name is absent - i.e. empty inherit part in type declaration
        cenv.g.obj_ty, tpenv
    | SynType.LongIdent(LongIdentWithDots(tc,_) as lidwd) -> 
        let m = lidwd.Range
        let ad = env.eAccessRights
        let tcref = ForceRaise(ResolveTypeLongIdent cenv.tcSink cenv.nameResolver occ OpenQualified env.eNameResEnv ad tc TypeNameResolutionStaticArgsInfo.DefiniteEmpty  PermitDirectReferenceToGeneratedType.No)
        match optKind, tcref.TypeOrMeasureKind with
        | Some TyparKind.Type, TyparKind.Measure ->
            error(Error(FSComp.SR.tcExpectedTypeNotUnitOfMeasure(), m)) 
            NewErrorType (), tpenv
        | Some TyparKind.Measure, TyparKind.Type ->
            error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m)) 
            TType_measure (NewErrorMeasure ()), tpenv
        | _, TyparKind.Measure ->
            TType_measure (Measure.Con tcref), tpenv
        | _, TyparKind.Type ->
            TcTypeApp cenv newOk checkCxs occ env tpenv m tcref [] []

    | SynType.App (SynType.LongIdent(LongIdentWithDots(tc,_)),_,args,_commas,_,postfix,m) -> 
        let ad = env.eAccessRights
        let tcref = ForceRaise(ResolveTypeLongIdent cenv.tcSink cenv.nameResolver ItemOccurence.UseInType OpenQualified env.eNameResEnv ad tc (TypeNameResolutionStaticArgsInfo.FromTyArgs args.Length) PermitDirectReferenceToGeneratedType.No)
        match optKind, tcref.TypeOrMeasureKind with
        | Some TyparKind.Type, TyparKind.Measure ->
            error(Error(FSComp.SR.tcExpectedTypeNotUnitOfMeasure(), m)) 
            NewErrorType (), tpenv
        | Some TyparKind.Measure, TyparKind.Type ->
            error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m)) 
            TType_measure (NewErrorMeasure ()), tpenv
        | _, TyparKind.Type ->
            if postfix && tcref.Typars(m) |> List.exists (fun tp -> match tp.Kind with TyparKind.Measure -> true | _ -> false) 
            then error(Error(FSComp.SR.tcInvalidUnitsOfMeasurePrefix(), m))
            TcTypeApp cenv newOk checkCxs occ env tpenv m tcref [] args 
        | _, TyparKind.Measure ->
            match args,postfix with
            | [arg], true ->
                let ms,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv arg m
                TType_measure (Measure.Prod(Measure.Con tcref, ms)), tpenv
              
            | _, _ ->
                errorR(Error(FSComp.SR.tcUnitsOfMeasureInvalidInTypeConstructor(), m))
                NewErrorType (), tpenv          

    | SynType.LongIdentApp (ltyp,LongIdentWithDots(longId,_),_,args,_commas,_,m) -> 
        let ad = env.eAccessRights
        let ltyp,tpenv = TcType cenv newOk checkCxs occ env tpenv ltyp
        if not (isAppTy cenv.g ltyp) then error(Error(FSComp.SR.tcTypeHasNoNestedTypes(),m))
        let tcref,tinst = destAppTy cenv.g ltyp
        let tcref = ResolveTypeLongIdentInTyconRef cenv.tcSink cenv.nameResolver env.eNameResEnv (TypeNameResolutionInfo.ResolveToTypeRefs (TypeNameResolutionStaticArgsInfo.FromTyArgs args.Length)) ad m tcref longId 
        TcTypeApp cenv newOk checkCxs occ env tpenv m tcref tinst args 

    | SynType.Tuple(args,m) ->   
        let isMeasure = match optKind with Some TyparKind.Measure -> true | None -> List.exists (fun (isquot,_) -> isquot) args | _ -> false
        if isMeasure then
            let ms,tpenv = TcMeasuresAsTuple cenv newOk checkCxs occ env tpenv args m
            TType_measure ms,tpenv
        else
            let args',tpenv = TcTypesAsTuple cenv newOk checkCxs occ env tpenv args m
            TType_tuple(tupInfoRef,args'),tpenv

    | SynType.StructTuple(args,m) ->   
        let args',tpenv = TcTypesAsTuple cenv newOk checkCxs occ env tpenv args m
        TType_tuple(tupInfoStruct,args'),tpenv

    | SynType.Fun(domainTy,resultTy,_) -> 
        let domainTy',tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv domainTy
        let resultTy',tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv resultTy
        (domainTy' --> resultTy'), tpenv

    | SynType.Array (n,elemTy,m) -> 
        let elemTy,tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv elemTy
        mkArrayTy cenv.g n elemTy m, tpenv

    | SynType.Var (tp,_) -> 
        let tp',tpenv = TcTyparOrMeasurePar optKind cenv env newOk tpenv tp
        match tp'.Kind with
        | TyparKind.Measure -> TType_measure (Measure.Var tp'), tpenv
        | TyparKind.Type -> mkTyparTy tp',tpenv

    // _ types
    | SynType.Anon m ->           
        let tp:Typar = TcAnonTypeOrMeasure optKind cenv TyparRigidity.Anon TyparDynamicReq.No newOk m
        match tp.Kind with
        | TyparKind.Measure -> TType_measure (Measure.Var tp), tpenv
        | TyparKind.Type -> mkTyparTy tp,tpenv

    | SynType.WithGlobalConstraints(ty,wcs,_) ->
        let cty,tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv ty
        let tpenv = TcTyparConstraints cenv newOk checkCxs occ env tpenv wcs
        cty,tpenv

    // #typ 
    | SynType.HashConstraint(ty,m) ->  
        let tp = TcAnonTypeOrMeasure (Some TyparKind.Type) cenv TyparRigidity.WarnIfNotRigid TyparDynamicReq.Yes newOk m
        let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv ty
        AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m NoTrace  ty' (mkTyparTy tp) 
        tp.AsType, tpenv

    | SynType.StaticConstant (c, m) ->
        match c, optKind with
        | _, Some TyparKind.Type -> 
            errorR(Error(FSComp.SR.parsInvalidLiteralInType(), m)) 
            NewErrorType (), tpenv
        | SynConst.Int32 1, _ -> 
            TType_measure Measure.One, tpenv
        | _ -> 
            errorR(Error(FSComp.SR.parsInvalidLiteralInType(), m)) 
            NewErrorType (), tpenv
    | SynType.StaticConstantNamed (_,_,m)
    | SynType.StaticConstantExpr (_,m) ->
        errorR(Error(FSComp.SR.parsInvalidLiteralInType(), m)) 
        NewErrorType (), tpenv


    | SynType.MeasurePower(typ, exponent, m) ->
        match optKind with
        | Some TyparKind.Type -> 
            errorR(Error(FSComp.SR.tcUnexpectedSymbolInTypeExpression("^"), m)) 
            NewErrorType (), tpenv
        | _ ->          
            let ms,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv typ m
            TType_measure (Measure.RationalPower (ms, TcSynRationalConst exponent)), tpenv

    | SynType.MeasureDivide(typ1, typ2, m) -> 
        match optKind with
        | Some TyparKind.Type -> 
            errorR(Error(FSComp.SR.tcUnexpectedSymbolInTypeExpression("/"), m)) 
            NewErrorType (), tpenv
        | _ ->
            let ms1,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv typ1 m
            let ms2,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv typ2 m
            TType_measure (Measure.Prod(ms1,Measure.Inv ms2)), tpenv

    | SynType.App((SynType.Var(_,m1) | SynType.MeasurePower(_,_,m1)) as arg1,_,args,_commas,_,postfix,m) ->
        match optKind, args, postfix with
        | (None | Some TyparKind.Measure), [arg2], true ->
            let ms1,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv arg1 m1
            let ms2,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv arg2 m
            TType_measure (Measure.Prod(ms1, ms2)), tpenv

        | _, _, _ ->
            errorR(Error(FSComp.SR.tcTypeParameterInvalidAsTypeConstructor(), m)) 
            NewErrorType (), tpenv

    | SynType.App(_, _, _, _, _, _, m) ->
        errorR(Error(FSComp.SR.tcIllegalSyntaxInTypeExpression(), m))
        NewErrorType (), tpenv

and TcType cenv newOk checkCxs occ env (tpenv:SyntacticUnscopedTyparEnv) ty = 
    TcTypeOrMeasure (Some TyparKind.Type) cenv newOk checkCxs occ env tpenv ty

and TcMeasure cenv newOk checkCxs occ env (tpenv:SyntacticUnscopedTyparEnv) ty m = 
    match ty with
    | SynType.Anon m ->
        error(Error(FSComp.SR.tcAnonymousUnitsOfMeasureCannotBeNested(), m))
        NewErrorMeasure (), tpenv
    | _ ->
        match TcTypeOrMeasure (Some TyparKind.Measure) cenv newOk checkCxs occ env tpenv ty with
        | TType_measure ms, tpenv -> ms,tpenv
        | _, _ -> 
            error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m)) 
            NewErrorMeasure (), tpenv


and TcAnonTypeOrMeasure optKind _cenv rigid dyn newOk m =
    if newOk = NoNewTypars then errorR (Error(FSComp.SR.tcAnonymousTypeInvalidInDeclaration(),m))
    let rigid = (if rigid = TyparRigidity.Anon && newOk = NewTyparsOKButWarnIfNotRigid then TyparRigidity.WarnIfNotRigid else rigid)
    let kind = match optKind with Some TyparKind.Measure -> TyparKind.Measure | _ -> TyparKind.Type
    NewAnonTypar (kind,m,rigid,NoStaticReq,dyn)
 
and TcTypes cenv newOk checkCxs occ env tpenv args =
    List.mapFold (TcTypeAndRecover cenv newOk checkCxs occ env) tpenv args 

and TcTypesAsTuple cenv newOk checkCxs occ env tpenv args m = 
    match args with
    | [] -> error(InternalError("empty tuple type",m))
    | [(_,typ)] -> let typ,tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv typ in [typ],tpenv
    | (isquot,typ)::args -> 
        let ty,tpenv = TcTypeAndRecover cenv newOk checkCxs occ env tpenv typ
        let tys,tpenv = TcTypesAsTuple cenv newOk checkCxs occ env tpenv args m
        if isquot then errorR(Error(FSComp.SR.tcUnexpectedSlashInType(),m))
        ty::tys,tpenv

// Type-check a list of measures separated by juxtaposition, * or /
and TcMeasuresAsTuple cenv newOk checkCxs occ env (tpenv:SyntacticUnscopedTyparEnv) args m = 
    let rec gather args tpenv isquot acc =
        match args with
        | [] -> acc,tpenv
        | (nextisquot,typ)::args -> 
            let ms1,tpenv = TcMeasure cenv newOk checkCxs occ env tpenv typ m
            gather args tpenv nextisquot (if isquot then Measure.Prod(acc,Measure.Inv ms1) else Measure.Prod(acc,ms1))
    gather args tpenv false Measure.One


and TcTypesOrMeasures optKinds cenv newOk checkCxs occ env tpenv args m =
    match optKinds with
    | None ->
        List.mapFold (TcTypeOrMeasure None cenv newOk checkCxs occ env) tpenv args
    | Some kinds ->
        if List.length kinds = List.length args then 
            List.mapFold (fun tpenv (arg,kind) -> TcTypeOrMeasure (Some kind) cenv newOk checkCxs occ env tpenv arg) tpenv (List.zip args kinds)
        elif isNil kinds then error(Error(FSComp.SR.tcUnexpectedTypeArguments(), m))
        else error(Error(FSComp.SR.tcTypeParameterArityMismatch((List.length kinds), (List.length args)), m))

and TcTyparConstraints cenv newOk checkCxs occ env tpenv wcs =
    // Mark up default constraints with a priority in reverse order: last gets 0, second 
    // last gets 1 etc. See comment on TyparConstraint.DefaultsTo 
    let _,tpenv = List.fold (fun (ridx,tpenv) tc -> ridx - 1, TcTyparConstraint ridx cenv newOk checkCxs occ env tpenv tc) (List.length wcs - 1, tpenv) wcs
    tpenv

#if EXTENSIONTYPING
and TcStaticConstantParameter cenv (env:TcEnv) tpenv kind (v:SynType) idOpt container =
    let fail() = error(Error(FSComp.SR.etInvalidStaticArgument(NicePrint.minimalStringOfType env.DisplayEnv kind),v.Range)) 
    let record ttype =
        match idOpt with
        | Some id ->
            let item = Item.ArgName (id, ttype, Some container)
            CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
        | _ -> ()

    match v with 
    | SynType.StaticConstant(sc, _) ->
        let v =
            match sc with
            | SynConst.Byte n       when typeEquiv cenv.g cenv.g.byte_ty kind    -> record(cenv.g.byte_ty); box (n:byte)
            | SynConst.Int16 n      when typeEquiv cenv.g cenv.g.int16_ty kind   -> record(cenv.g.int16_ty); box (n:int16)
            | SynConst.Int32 n      when typeEquiv cenv.g cenv.g.int32_ty kind   -> record(cenv.g.int32_ty); box (n:int)
            | SynConst.Int64 n      when typeEquiv cenv.g cenv.g.int64_ty kind   -> record(cenv.g.int64_ty); box (n:int64)
            | SynConst.SByte n      when typeEquiv cenv.g cenv.g.sbyte_ty kind   -> record(cenv.g.sbyte_ty); box (n:sbyte)
            | SynConst.UInt16 n     when typeEquiv cenv.g cenv.g.uint16_ty kind  -> record(cenv.g.uint16_ty); box (n:uint16)
            | SynConst.UInt32 n     when typeEquiv cenv.g cenv.g.uint32_ty kind  -> record(cenv.g.uint32_ty); box (n:uint32)
            | SynConst.UInt64 n     when typeEquiv cenv.g cenv.g.uint64_ty kind  -> record(cenv.g.uint64_ty); box (n:uint64)
            | SynConst.Decimal n    when typeEquiv cenv.g cenv.g.decimal_ty kind -> record(cenv.g.decimal_ty); box (n:decimal)
            | SynConst.Single n     when typeEquiv cenv.g cenv.g.float32_ty kind -> record(cenv.g.float32_ty); box (n:single)
            | SynConst.Double n     when typeEquiv cenv.g cenv.g.float_ty kind   -> record(cenv.g.float_ty); box (n:double)
            | SynConst.Char n       when typeEquiv cenv.g cenv.g.char_ty kind    -> record(cenv.g.char_ty); box (n:char)
            | SynConst.String (s,_) when s <> null && typeEquiv cenv.g cenv.g.string_ty kind  -> record(cenv.g.string_ty); box (s:string)
            | SynConst.Bool b       when typeEquiv cenv.g cenv.g.bool_ty kind    -> record(cenv.g.bool_ty); box (b:bool)
            | _ -> fail()
        v, tpenv
    | SynType.StaticConstantExpr(e, _ ) ->

        // If an error occurs, don't try to recover, since the constant expression will be nothing like what we need
        let te,tpenv' = TcExprNoRecover cenv kind env tpenv e

        // Evaluate the constant expression using static attribute argument rules
        let te = EvalLiteralExprOrAttribArg cenv.g te
        let v = 
            match stripExpr te with 
            // Check we have a residue constant. We know the type was correct because we checked the expression with this type.
            | Expr.Const(c,_,_) -> 
                match c with
                | Const.Byte n     -> record(cenv.g.byte_ty); box (n:byte)
                | Const.Int16 n    -> record(cenv.g.int16_ty); box (n:int16)
                | Const.Int32 n    -> record(cenv.g.int32_ty); box (n:int)
                | Const.Int64 n    -> record(cenv.g.int64_ty); box (n:int64)
                | Const.SByte n    -> record(cenv.g.sbyte_ty); box (n:sbyte)
                | Const.UInt16 n   -> record(cenv.g.uint16_ty); box (n:uint16)
                | Const.UInt32 n   -> record(cenv.g.uint32_ty); box (n:uint32)
                | Const.UInt64 n   -> record(cenv.g.uint64_ty); box (n:uint64)
                | Const.Decimal n  -> record(cenv.g.decimal_ty); box (n:decimal)
                | Const.Single n   -> record(cenv.g.float32_ty); box (n:single)
                | Const.Double n   -> record(cenv.g.float_ty); box (n:double)
                | Const.Char n     -> record(cenv.g.char_ty); box (n:char)
                | Const.String null   -> fail() 
                | Const.String s   -> record(cenv.g.string_ty); box (s:string)
                | Const.Bool b     -> record(cenv.g.bool_ty); box (b:bool)
                | _ ->  fail()
            | _ -> error(Error(FSComp.SR.tcInvalidConstantExpression(),v.Range))
        v, tpenv'
    | SynType.LongIdent(lidwd) ->
        let m = lidwd.Range
        TcStaticConstantParameter cenv env tpenv kind (SynType.StaticConstantExpr(SynExpr.LongIdent(false,lidwd,None,m),m)) idOpt container
    | _ ->  
        fail()

and CrackStaticConstantArgs cenv env tpenv (staticParameters: Tainted<ProvidedParameterInfo>[], args: SynType list, container, containerName, m) =
    let args = 
        args |> List.map (function 
            | SynType.StaticConstantNamed(SynType.LongIdent(LongIdentWithDots([id],_)),v,_) -> Some id, v
            | v -> None, v)

    let unnamedArgs = args |> Seq.takeWhile (fst >> Option.isNone) |> Seq.toArray |> Array.map snd
    let otherArgs = args |> List.skipWhile (fst >> Option.isNone)
    let namedArgs = otherArgs |> List.takeWhile (fst >> Option.isSome) |> List.map (map1Of2 Option.get)
    let otherArgs = otherArgs |> List.skipWhile (fst >> Option.isSome)
    if not otherArgs.IsEmpty then 
        error (Error(FSComp.SR.etBadUnnamedStaticArgs(),m))

    let indexedStaticParameters = staticParameters |> Array.toList |> List.indexed
    for (n,_) in namedArgs do
         match indexedStaticParameters |> List.filter (fun (j,sp) -> j >= unnamedArgs.Length && n.idText = sp.PUntaint((fun sp -> sp.Name), m)) with
         | [] -> 
             if staticParameters |> Array.exists (fun sp -> n.idText = sp.PUntaint((fun sp -> sp.Name), n.idRange)) then 
                 error (Error(FSComp.SR.etStaticParameterAlreadyHasValue n.idText,n.idRange))
             else
                 error (Error(FSComp.SR.etNoStaticParameterWithName n.idText,n.idRange))
         | [_] -> ()
         | _ -> error (Error(FSComp.SR.etMultipleStaticParameterWithName n.idText,n.idRange))

    if staticParameters.Length < namedArgs.Length + unnamedArgs.Length then 
        error (Error(FSComp.SR.etTooManyStaticParameters(staticParameters.Length,unnamedArgs.Length,namedArgs.Length),m))

    let argsInStaticParameterOrderIncludingDefaults = 
        staticParameters |> Array.mapi (fun i sp -> 
            let spKind = Import.ImportProvidedType cenv.amap m (sp.PApply((fun x -> x.ParameterType), m))
            let spName = sp.PUntaint((fun sp -> sp.Name), m)
            if i < unnamedArgs.Length then 
                let v = unnamedArgs.[i]
                let v, _tpenv = TcStaticConstantParameter cenv env tpenv spKind v None container
                v
            else
                match namedArgs |> List.filter (fun (n,_) -> n.idText = spName) with 
                | [(n,v)] -> 
                    let v, _tpenv = TcStaticConstantParameter cenv env tpenv spKind v (Some n) container
                    v
                | [] -> 
                    if sp.PUntaint((fun sp -> sp.IsOptional), m) then
                         match sp.PUntaint((fun sp -> sp.RawDefaultValue), m) with
                         | null -> error (Error(FSComp.SR.etStaticParameterRequiresAValue (spName, containerName, containerName, spName) ,m))
                         | v -> v
                    else
                      error (Error(FSComp.SR.etStaticParameterRequiresAValue (spName, containerName, containerName, spName),m))
                 | ps -> 
                      error (Error(FSComp.SR.etMultipleStaticParameterWithName spName,(fst (List.last ps)).idRange)))

    argsInStaticParameterOrderIncludingDefaults

and TcProvidedTypeAppToStaticConstantArgs cenv env optGeneratedTypePath tpenv (tcref:TyconRef) (args: SynType list) m =
    let typeBeforeArguments = 
        match tcref.TypeReprInfo with 
        | TProvidedTypeExtensionPoint info -> info.ProvidedType
        | _ -> failwith "unreachable"

    let staticParameters = typeBeforeArguments.PApplyWithProvider((fun (typeBeforeArguments,provider) -> typeBeforeArguments.GetStaticParameters(provider)), range=m) 
    let staticParameters = staticParameters.PApplyArray(id, "GetStaticParameters", m)

    let argsInStaticParameterOrderIncludingDefaults = CrackStaticConstantArgs cenv env tpenv (staticParameters, args, ArgumentContainer.Type tcref, tcref.DisplayName, m)
 
    // Take the static arguments (as SynType's) and convert them to objects of the appropriate type, based on the expected kind.
    let providedTypeAfterStaticArguments, checkTypeName = 
        match ExtensionTyping.TryApplyProvidedType(typeBeforeArguments, optGeneratedTypePath, argsInStaticParameterOrderIncludingDefaults, m) with 
        | None -> error(Error(FSComp.SR.etErrorApplyingStaticArgumentsToType(),m))
        | Some (ty,checkTypeName) -> (ty, checkTypeName)

    let hasNoArgs = (argsInStaticParameterOrderIncludingDefaults.Length = 0)
    hasNoArgs, providedTypeAfterStaticArguments, checkTypeName        


and TryTcMethodAppToStaticConstantArgs cenv env tpenv (minfos: MethInfo list, argsOpt, mExprAndArg, mItem) =
    match minfos, argsOpt with 
    | [minfo], Some (args,_) -> 
        match minfo.ProvidedStaticParameterInfo with 
        | Some (methBeforeArguments, staticParams) -> 
            let providedMethAfterStaticArguments = TcProvidedMethodAppToStaticConstantArgs cenv env tpenv (minfo, methBeforeArguments, staticParams, args, mExprAndArg)
            let minfoAfterStaticArguments = ProvidedMeth(cenv.amap,providedMethAfterStaticArguments,minfo.ExtensionMemberPriorityOption,mItem)
            Some minfoAfterStaticArguments
        | _ -> None
    | _ -> None

and TcProvidedMethodAppToStaticConstantArgs cenv env tpenv (minfo, methBeforeArguments, staticParams, args, m) =

    let argsInStaticParameterOrderIncludingDefaults = CrackStaticConstantArgs cenv env tpenv (staticParams, args, ArgumentContainer.Method minfo, minfo.DisplayName, m)
 
    let providedMethAfterStaticArguments = 
        match ExtensionTyping.TryApplyProvidedMethod(methBeforeArguments, argsInStaticParameterOrderIncludingDefaults, m) with 
        | None -> error(Error(FSComp.SR.etErrorApplyingStaticArgumentsToMethod(),m))
        | Some meth -> meth

    providedMethAfterStaticArguments        

and TcProvidedTypeApp cenv env tpenv tcref args m = 
    let hasNoArgs,providedTypeAfterStaticArguments,checkTypeName = TcProvidedTypeAppToStaticConstantArgs cenv env None tpenv tcref args m

    let isGenerated = providedTypeAfterStaticArguments.PUntaint((fun st -> not st.IsErased),m)

    //printfn "adding entity for provided type '%s', isDirectReferenceToGenerated = %b, isGenerated = %b" (st.PUntaint((fun st -> st.Name), m)) isDirectReferenceToGenerated isGenerated
    let isDirectReferenceToGenerated = isGenerated && ExtensionTyping.IsGeneratedTypeDirectReference (providedTypeAfterStaticArguments, m)
    if isDirectReferenceToGenerated then 
        error(Error(FSComp.SR.etDirectReferenceToGeneratedTypeNotAllowed(tcref.DisplayName),m))

    // We put the type name check after the 'isDirectReferenceToGenerated' check because we need the 'isDirectReferenceToGenerated' error to be shown for generated types
    checkTypeName() 
    if hasNoArgs then 
        mkAppTy tcref [], tpenv
    else
        let typ = Import.ImportProvidedType cenv.amap m providedTypeAfterStaticArguments
        typ,tpenv 
#endif

/// Typecheck an application of a generic type to type arguments.
///
/// Note that the generic type may be a nested generic type List<T>.ListEnumerator<U>.
/// In this case, 'args' is only the instantation of the suffix type arguments, and pathTypeArgs gives
/// the prefix of type arguments. 
and TcTypeApp cenv newOk checkCxs occ env tpenv m tcref pathTypeArgs (args: SynType list) =
    CheckTyconAccessible cenv.amap m env.eAccessRights tcref |> ignore
    CheckEntityAttributes cenv.g tcref m |> CommitOperationResult
    
#if EXTENSIONTYPING
    // Provided types are (currently) always non-generic. Their names may include mangled 
    // static parameters, which are passed by the provider.
    if tcref.Deref.IsProvided then TcProvidedTypeApp cenv env tpenv tcref args m else
#endif

    let tps,_,tinst,_ = infoOfTyconRef m tcref
    // If we're not checking constraints, i.e. when we first assert the super/interfaces of a type definition, then just 
    // clear the constraint lists of the freshly generated type variables. A little ugly but fairly localized. 
    if checkCxs = NoCheckCxs then tps |> List.iter (fun tp -> tp.typar_constraints <- [])
    if tinst.Length <> pathTypeArgs.Length + args.Length then 
        error (TyconBadArgs(env.DisplayEnv,tcref,pathTypeArgs.Length + args.Length,m))
    let args',tpenv = 
        // Get the suffix of typars
        let tpsForArgs = List.drop (tps.Length - args.Length) tps
        let kindsForArgs = tpsForArgs |> List.map (fun tp -> tp.Kind)
        TcTypesOrMeasures (Some kindsForArgs) cenv newOk checkCxs occ env tpenv args m
    let args' = pathTypeArgs @ args'
    if checkCxs = CheckCxs then
        List.iter2 (UnifyTypes cenv env m) tinst args'
    mkAppTy tcref args', tpenv

and TcTypeOrMeasureAndRecover optKind cenv newOk checkCxs occ env tpenv ty   =
    try TcTypeOrMeasure optKind cenv newOk checkCxs occ env tpenv ty 
    with e -> 
        errorRecovery e ty.Range 
        let rty = 
            match optKind, newOk with 
            | Some TyparKind.Measure, NoNewTypars -> TType_measure Measure.One
            | Some TyparKind.Measure, _ -> TType_measure (NewErrorMeasure ())
            | _, NoNewTypars -> cenv.g.obj_ty
            | _ -> NewErrorType () 
        rty,tpenv 


and TcTypeAndRecover cenv newOk checkCxs occ env tpenv ty   =
    TcTypeOrMeasureAndRecover (Some TyparKind.Type) cenv newOk checkCxs occ env tpenv ty

and TcNestedTypeApplication cenv newOk checkCxs occ env tpenv mWholeTypeApp typ tyargs =
    if not (isAppTy cenv.g typ) then error(Error(FSComp.SR.tcTypeHasNoNestedTypes(),mWholeTypeApp))
    match typ with 
    | TType_app(tcref,tinst) -> 
        let pathTypeArgs = List.take (max (tinst.Length - tcref.Typars(mWholeTypeApp).Length) 0) tinst
        TcTypeApp cenv newOk checkCxs occ env tpenv mWholeTypeApp tcref pathTypeArgs tyargs 
    | _ -> error(InternalError("TcNestedTypeApplication: expected type application",mWholeTypeApp))


and TryAdjustHiddenVarNameToCompGenName cenv env (id:Ident) altNameRefCellOpt =
      match altNameRefCellOpt with 
      | Some ({contents = Undecided altId } as altNameRefCell) -> 
          match ResolvePatternLongIdent cenv.tcSink cenv.nameResolver AllIdsOK false id.idRange env.eAccessRights env.eNameResEnv TypeNameResolutionInfo.Default [id] with 
          | Item.NewDef _ -> None // the name is not in scope as a pattern identifier (e.g. union case), so do not use the alternate ID
          | _ -> altNameRefCell :=  Decided altId; Some altId  // the name is in scope as a pattern identifier, so use the alternate ID
      | Some ({contents = Decided altId }) -> Some altId
      | None -> None

/// Bind the patterns used in a lambda. Not clear why we don't use TcPat.
and TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) p = 
    match p with 
    | SynSimplePat.Id (id,altNameRefCellOpt,compgen,isMemberThis,isOpt,m) -> 
        // Check to see if pattern translation decides to use an alternative identifier.
        match TryAdjustHiddenVarNameToCompGenName cenv env id altNameRefCellOpt with 
        | Some altId -> TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) (SynSimplePat.Id (altId,None,compgen,isMemberThis,isOpt,m) )
        | None -> 

            if isOpt && not optArgsOK then errorR(Error(FSComp.SR.tcOptionalArgsOnlyOnMembers(),m))
            if isOpt then 
                let tyarg = NewInferenceType ()
                UnifyTypes cenv env m ty (mkOptionTy cenv.g tyarg)
                    
            let _,names,takenNames = TcPatBindingName cenv env id ty isMemberThis None None (ValInline.Optional,permitInferTypars,noArgOrRetAttribs,false,None,compgen) (names,takenNames)
            id.idText, 
            (tpenv,names,takenNames)

    | SynSimplePat.Typed (p,cty,m) ->
        let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK checkCxs ItemOccurence.UseInType env tpenv cty
        match p with 
        // Optional arguments on members 
        | SynSimplePat.Id(_,_,_,_,true,_) -> UnifyTypes cenv env m ty (mkOptionTy cenv.g cty')
        | _ -> UnifyTypes cenv env m ty cty'

        TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) p

    | SynSimplePat.Attrib (p,_,_) ->
        TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) p

// raise an error if any optional args precede any non-optional args 
and ValidateOptArgOrder (spats : SynSimplePats) =

    let rec getPats spats =
        match spats with
        | SynSimplePats.SimplePats(p,m) -> p,m
        | SynSimplePats.Typed(p,_,_) -> getPats p
        
    let rec isOptArg pat =
        match pat with
        | SynSimplePat.Id (_,_,_,_,isOpt,_) -> isOpt
        | SynSimplePat.Typed (p,_,_) -> isOptArg p
        | SynSimplePat.Attrib (p,_,_) -> isOptArg p        
        
    let pats,m = getPats spats 
        
    let hitOptArg = ref false
    
    List.iter (fun pat -> if isOptArg pat then hitOptArg := true elif !hitOptArg then error(Error(FSComp.SR.tcOptionalArgsMustComeAfterNonOptionalArgs(),m))) pats
            
                    
/// Bind the patterns used in argument position for a function, method or lambda. 
and TcSimplePats cenv optArgsOK  checkCxs ty env (tpenv,names,takenNames:Set<_>) p =
    
    // validate optional argument declaration
    ValidateOptArgOrder p
               
    match p with 
    | SynSimplePats.SimplePats ([],m) -> 
        // Unit "()" patterns in argument position become SynSimplePats.SimplePats([],_) in the
        // syntactic translation when building bindings. This is done because the
        // use of "()" has special significance for arity analysis and argument counting.
        //
        // Here we give a name to the single argument implied by those patterns.
        // This is a little awkward since it would be nice if this was
        // uniform with the process where we give names to other (more complex)
        // patterns used in argument position, e.g. "let f (D(x)) = ..."
        let id = ident("unitVar" + string takenNames.Count,m)
        UnifyTypes cenv env m ty cenv.g.unit_ty
        let _,names,takenNames = TcPatBindingName cenv env id ty false None None (ValInline.Optional,permitInferTypars,noArgOrRetAttribs,false,None,true) (names,takenNames)
        [id.idText],(tpenv,names,takenNames)

    | SynSimplePats.SimplePats ([p],_) -> 
        let v,(tpenv,names,takenNames) = TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) p
        [v],(tpenv,names,takenNames)

    | SynSimplePats.SimplePats (ps,m) -> 
        let ptys = UnifyRefTupleType env.eContextInfo cenv env.DisplayEnv m ty ps
        let ps',(tpenv,names,takenNames) = List.mapFold (fun tpenv (ty,e) -> TcSimplePat optArgsOK checkCxs cenv ty env tpenv e) (tpenv,names,takenNames) (List.zip ptys ps)
        ps',(tpenv,names,takenNames)

    | SynSimplePats.Typed (p,cty,m) ->
        let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv cty

        match p with 
        // Solitary optional arguments on members 
        | SynSimplePats.SimplePats([SynSimplePat.Id(_,_,_,_,true,_)],_) -> UnifyTypes cenv env m ty (mkOptionTy cenv.g cty')
        | _ -> UnifyTypes cenv env m ty cty'

        TcSimplePats cenv optArgsOK  checkCxs ty env (tpenv,names,takenNames) p

and TcSimplePatsOfUnknownType cenv optArgsOK checkCxs env tpenv spats =
    let argty = NewInferenceType ()
    TcSimplePats cenv optArgsOK checkCxs argty env (tpenv,NameMap.empty,Set.empty) spats

and TcPatBindingName cenv env id ty isMemberThis vis1 topValData (inlineFlag,declaredTypars,argAttribs,isMutable,vis2,compgen) (names,takenNames:Set<string>) = 
    let vis = if Option.isSome vis1 then vis1 else vis2
    if takenNames.Contains id.idText then errorR (VarBoundTwice id)
    let baseOrThis = if isMemberThis then MemberThisVal else NormalVal
    let names = Map.add id.idText (PrelimValScheme1(id,declaredTypars,ty,topValData,None,isMutable,inlineFlag,baseOrThis,argAttribs,vis,compgen)) names
    let takenNames = Set.add id.idText takenNames
    (fun (TcPatPhase2Input (values, isLeftMost)) -> 
        let (vspec,typeScheme) = 
            match values.TryFind id.idText with
            | Some value ->
                let name = id.idText
                if not (String.IsNullOrEmpty name) && Char.IsLower(name.[0]) then
                    match TryFindPatternByName name env.eNameResEnv with
                    | Some (Item.Value vref) when vref.LiteralValue.IsSome ->
                        warning(Error(FSComp.SR.checkLowercaseLiteralBindingInPattern(id.idText),id.idRange))
                    | Some _ | None -> ()
                value
            | None -> error(Error(FSComp.SR.tcNameNotBoundInPattern(id.idText),id.idRange))

        // isLeftMost indcates we are processing the left-most path through a disjunctive or pattern.
        // For those binding locations, CallNameResolutionSink is called in MakeAndPublishValue, like all other bindings
        // For non-left-most paths, we register the name resolutions here
        if not isLeftMost && not vspec.IsCompilerGenerated && not (String.hasPrefix vspec.LogicalName "_") then 
            let item = Item.Value(mkLocalValRef vspec)
            CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Binding,env.DisplayEnv,env.eAccessRights)

        PBind(vspec,typeScheme)),
    names,takenNames

and TcPatAndRecover warnOnUpper cenv (env:TcEnv) topValInfo vFlags (tpenv,names,takenNames) ty (pat:SynPat) = 
    try 
       TcPat warnOnUpper cenv env topValInfo vFlags (tpenv,names,takenNames) ty pat
    with e ->
        // Error recovery - return some rubbish expression, but replace/annotate 
        // the type of the current expression with a type variable that indicates an error 
        let m = pat.Range 
        errorRecovery e m
        //solveTypAsError cenv env.DisplayEnv m ty
        (fun _ -> TPat_wild m), (tpenv,names,takenNames)

/// Typecheck a pattern. Patterns are type-checked in three phases: 
/// 1. TcPat builds a List.map from simple variable names to inferred types for 
///   those variables. It also returns a function to perform the second phase.
/// 2. The second phase assumes the caller has built the actual value_spec's 
///    for the values being defined, and has decided if the types of these 
///    variables are to be generalized. The caller hands this information to
///    the second-phase function in terms of a List.map from names to actual
///    value specifications. 
and TcPat warnOnUpper cenv env topValInfo vFlags (tpenv,names,takenNames) ty pat = 
    let ad = env.eAccessRights
    match pat with 
    | SynPat.Const (c,m) -> 
        match c with 
        | SynConst.Bytes (bytes,m) -> 
            UnifyTypes cenv env m ty (mkByteArrayTy cenv.g) 
            TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty (SynPat.ArrayOrList (true,[ for b in bytes -> SynPat.Const(SynConst.Byte b,m) ],m))
        | SynConst.UserNum _ -> 
            error(Error(FSComp.SR.tcInvalidNonPrimitiveLiteralInPatternMatch(),m))
        | _ -> 
            let c' = TcConst cenv ty m env c
            (fun (_:TcPatPhase2Input) -> TPat_const(c',m)),(tpenv,names,takenNames)
        
    | SynPat.Wild m ->
        (fun _ -> TPat_wild m), (tpenv,names,takenNames)

    | SynPat.IsInst(cty,m) 
    | SynPat.Named (SynPat.IsInst(cty,m),_,_,_,_) -> 
        let srcTy = ty
        let tgty,tpenv = TcTypeAndRecover cenv NewTyparsOKButWarnIfNotRigid CheckCxs ItemOccurence.UseInType env tpenv cty
        TcRuntimeTypeTest (*isCast*)false (*isOperator*)true cenv env.DisplayEnv m tgty srcTy
        match pat with 
        | SynPat.IsInst(_,m) ->
            (fun _ -> TPat_isinst (srcTy,tgty,None,m)),(tpenv,names,takenNames)
        | SynPat.Named (SynPat.IsInst _,id,isMemberThis,vis,m) -> 
            let bindf,names,takenNames = TcPatBindingName cenv env id tgty isMemberThis vis None vFlags (names,takenNames)
            (fun values -> TPat_isinst (srcTy,tgty,Some(bindf values),m)),
            (tpenv,names,takenNames)
        | _ -> failwith "TcPat"

    | SynPat.OptionalVal (_,m) -> 
        error(Error(FSComp.SR.tcOptionalArgsOnlyOnMembers(),m))

    | SynPat.Named (p,id,isMemberThis,vis,m) -> 
        let bindf,names,takenNames = TcPatBindingName cenv env id ty isMemberThis vis topValInfo vFlags (names,takenNames)
        let pat',acc = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty p
        (fun values -> TPat_as (pat' values,bindf values,m)), 
        acc

    | SynPat.Typed (p,cty,m) ->
        let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv cty
        UnifyTypes cenv env m ty cty'
        TcPat warnOnUpper cenv env topValInfo vFlags (tpenv,names,takenNames) ty p

    | SynPat.Attrib (_,_,m) ->
        error(Error(FSComp.SR.tcAttributesInvalidInPatterns(),m))

    | SynPat.Or (pat1,pat2,m) ->
        let pat1',(tpenv,names1,takenNames1) = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty pat1
        let pat2',(tpenv,names2,takenNames2) = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty pat2
        if not (takenNames1 = takenNames2) then
          // We don't try to recover from this error since we get later bad internal errors during pattern
          // matching 
          error (UnionPatternsBindDifferentNames m)
        names1 |> Map.iter (fun _ (PrelimValScheme1(id1,_,ty1,_,_,_,_,_,_,_,_)) -> 
          match Map.tryFind id1.idText names2 with 
          | None -> () 
          | Some (PrelimValScheme1(_,_,ty2,_,_,_,_,_,_,_,_)) -> 
              UnifyTypes cenv env m ty1 ty2)
        (fun values -> TPat_disjs ([pat1' values;pat2' values.RightPath],m)), (tpenv,names1,takenNames1)

    | SynPat.Ands (pats,m) ->
        let pats',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) (List.map (fun _ -> ty) pats) pats
        (fun values -> TPat_conjs(List.map (fun f -> f values) pats',m)), acc

    | SynPat.LongIdent (LongIdentWithDots(longId,_),_,tyargs,args,vis,m) ->
        if Option.isSome tyargs then errorR(Error(FSComp.SR.tcInvalidTypeArgumentUsage(),m))
        let warnOnUpperForId = 
            match args with
            | SynConstructorArgs.Pats [] -> warnOnUpper
            | _ -> AllIdsOK

        let checkNoArgsForLiteral() = 
            let nargs =
                match args with
                | SynConstructorArgs.Pats args -> args.Length
                | SynConstructorArgs.NamePatPairs (pairs, _) -> pairs.Length
            if nargs <> 0 then error(Error(FSComp.SR.tcLiteralDoesNotTakeArguments(),m)) 

        match ResolvePatternLongIdent cenv.tcSink cenv.nameResolver warnOnUpperForId false m ad env.eNameResEnv TypeNameResolutionInfo.Default longId with
        | Item.NewDef id -> 
            match args with 
            | SynConstructorArgs.Pats [] 
            | SynConstructorArgs.NamePatPairs ([], _)-> TcPat warnOnUpperForId cenv env topValInfo vFlags (tpenv,names,takenNames) ty (mkSynPatVar vis id)
            | _ -> error (UndefinedName(0,FSComp.SR.undefinedNamePatternDiscriminator,id,NoSuggestions))

        | Item.ActivePatternCase(APElemRef(apinfo,vref,idx)) as item -> 
            let args = match args with SynConstructorArgs.Pats args -> args | _ -> error(Error(FSComp.SR.tcNamedActivePattern(apinfo.ActiveTags.[idx]),m))
            // TOTAL/PARTIAL ACTIVE PATTERNS 
            let vexp, _, _, tinst, _ = TcVal true cenv env tpenv vref None m
            let vexp = MakeApplicableExprWithFlex cenv env vexp
            let vexpty = vexp.Type

            let activePatArgsAsSynPats,patarg = 
                match args with 
                | [] -> [],SynPat.Const(SynConst.Unit,m) 
                | _ -> 
                    // This bit of type-directed analysis ensures that parameterized partial active patterns returning unit do not need to take an argument
                    // See FSharp 1.0 3502
                    let dtys,rty = stripFunTy cenv.g vexpty
                    
                    if dtys.Length = args.Length + 1 && isOptionTy cenv.g rty && isUnitTy cenv.g (destOptionTy cenv.g rty) then 
                        args,SynPat.Const(SynConst.Unit,m) 
                    else 
                        List.frontAndBack args

            if not (isNil activePatArgsAsSynPats) && apinfo.ActiveTags.Length <> 1 then 
                error(Error(FSComp.SR.tcRequireActivePatternWithOneResult(),m))

            // Parse the arguments to an active pattern
            // Note we parse arguments to parameterized pattern labels as patterns, not expressions. 
            // This means the range of syntactic expression forms that can be used here is limited. 
            let rec convSynPatToSynExpr x = 
                match x with
                | SynPat.FromParseError(p,_) -> convSynPatToSynExpr p
                | SynPat.Const (c,m) -> SynExpr.Const(c,m)
                | SynPat.Named (SynPat.Wild _,id,_,None,_) -> SynExpr.Ident(id)
                | SynPat.Typed (p,cty,m) -> SynExpr.Typed (convSynPatToSynExpr p,cty,m)
                | SynPat.LongIdent (LongIdentWithDots(longId,dotms) as lidwd,_,_tyargs,args,None,m) -> 
                    let args = match args with SynConstructorArgs.Pats args -> args  | _ -> failwith "impossible: active patterns can be used only with SynConstructorArgs.Pats"
                    let e =
                        if dotms.Length = longId.Length then
                            let e = SynExpr.LongIdent(false,LongIdentWithDots(longId, List.take (dotms.Length - 1) dotms),None,m)
                            SynExpr.DiscardAfterMissingQualificationAfterDot(e, unionRanges e.Range (List.last dotms))
                        else SynExpr.LongIdent(false,lidwd,None,m)
                    List.fold (fun f x -> mkSynApp1 f (convSynPatToSynExpr x) m) e args
                | SynPat.Tuple (args,m) -> SynExpr.Tuple(List.map convSynPatToSynExpr args,[],m)
                | SynPat.Paren (p,_) -> convSynPatToSynExpr p
                | SynPat.ArrayOrList (isArray,args,m) -> SynExpr.ArrayOrList(isArray,List.map convSynPatToSynExpr args,m)
                | SynPat.QuoteExpr (e,_) -> e
                | SynPat.Null m -> SynExpr.Null(m)
                | _ -> error(Error(FSComp.SR.tcInvalidArgForParameterizedPattern(),x.Range))
            let activePatArgsAsSynExprs = List.map convSynPatToSynExpr activePatArgsAsSynPats

            let activePatResTys = NewInferenceTypes apinfo.Names
            let activePatType = apinfo.OverallType cenv.g m ty activePatResTys 

            let delayed = activePatArgsAsSynExprs |> List.map (fun arg -> DelayedApp(ExprAtomicFlag.NonAtomic, arg, unionRanges (rangeOfLid longId) arg.Range)) 
            let activePatExpr, tpenv = PropagateThenTcDelayed cenv activePatType env tpenv m vexp vexpty ExprAtomicFlag.NonAtomic delayed

            if idx >= activePatResTys.Length then error(Error(FSComp.SR.tcInvalidIndexIntoActivePatternArray(),m))
            let argty = List.item idx activePatResTys
                
            let arg',(tpenv,names,takenNames) = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) argty patarg
            
            // The identity of an active pattern consists of its value and the types it is applied to.
            // If there are any expression args then we've lost identity. 
            let activePatIdentity = if isNil activePatArgsAsSynExprs then Some (vref, tinst) else None
            (fun values -> 
                // Report information about the 'active recognizer' occurence to IDE
                CallNameResolutionSink cenv.tcSink (rangeOfLid longId,env.NameEnv,item,item,ItemOccurence.Pattern,env.DisplayEnv,env.eAccessRights)
                TPat_query((activePatExpr, activePatResTys, activePatIdentity, idx, apinfo), arg' values, m)), 
            (tpenv,names,takenNames)

        | (Item.UnionCase _ | Item.ExnCase _) as item ->
            // DATA MATCH CONSTRUTORS 
            let mkf,argtys, argNames = ApplyUnionCaseOrExnTypesForPat m cenv env ty item
            let nargtys = argtys.Length

            let args =
                match args with
                | SynConstructorArgs.Pats args -> args
                | SynConstructorArgs.NamePatPairs (pairs, m) ->
                    // rewrite patterns from the form (name-N = pat-N...) to (..._, pat-N, _...)
                    // so type T = Case of name : int * value : int
                    // | Case(value = v)
                    // will become
                    // | Case(_, v)
                    let result = Array.zeroCreate nargtys
                    for (id, pat) in pairs do
                        match argNames |> List.tryFindIndex (fun id2 -> id.idText = id2.idText)  with
                        | None -> 
                            let caseName = 
                                match item with
                                | Item.UnionCase(uci,_) -> uci.Name
                                | Item.ExnCase tcref -> tcref.DisplayName
                                | _ -> failwith "impossible"
                            error(Error(FSComp.SR.tcUnionCaseConstructorDoesNotHaveFieldWithGivenName(caseName, id.idText), id.idRange))
                        | Some idx ->
                            match box result.[idx] with
                            | null -> 
                                result.[idx] <- pat
                                let argContainerOpt = match item with
                                                      | Item.UnionCase(uci,_) -> Some(ArgumentContainer.UnionCase(uci))
                                                      | Item.ExnCase tref -> Some(ArgumentContainer.Type(tref))
                                                      | _ -> None
                                let argItem = Item.ArgName (argNames.[idx], argtys.[idx], argContainerOpt)   
                                CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,argItem,argItem,ItemOccurence.Pattern,env.DisplayEnv,ad)
                            | _ ->
                                error(Error(FSComp.SR.tcUnionCaseFieldCannotBeUsedMoreThanOnce(id.idText), id.idRange))
                    for i = 0 to nargtys - 1 do
                        if isNull (box result.[i]) then
                            result.[i] <- SynPat.Wild(m.MakeSynthetic())

                    let args = List.ofArray result
                    if result.Length = 1 then args
                    else [ SynPat.Tuple(args, m) ]

            let args = 
              match args with 
              | []-> []
              // note: the next will always be parenthesized 
              | [SynPatErrorSkip(SynPat.Tuple (args,_)) | SynPatErrorSkip(SynPat.Paren(SynPatErrorSkip(SynPat.Tuple (args,_)),_))] when nargtys > 1 -> args

              // note: we allow both 'C _' and 'C (_)' regardless of number of argument of the pattern 
              | [SynPatErrorSkip(SynPat.Wild _ as e) | SynPatErrorSkip(SynPat.Paren(SynPatErrorSkip(SynPat.Wild _ as e),_))] -> Array.toList (Array.create nargtys e)
              | [arg] -> [arg] 
              | _ when nargtys = 0 -> error(Error(FSComp.SR.tcUnionCaseDoesNotTakeArguments(),m)) 
              | _ when nargtys = 1 -> error(Error(FSComp.SR.tcUnionCaseRequiresOneArgument(),m)) 
              | _ -> error(Error(FSComp.SR.tcUnionCaseExpectsTupledArguments(nargtys),m))
            UnionCaseOrExnCheck env nargtys args.Length m

            let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) argtys args
            (fun values -> 
                // Report information about the case occurence to IDE
                CallNameResolutionSink cenv.tcSink (rangeOfLid longId,env.NameEnv,item,item,ItemOccurence.Pattern,env.DisplayEnv,env.eAccessRights)
                mkf m (List.map (fun f -> f values) args')), acc
                
        | Item.ILField finfo ->
            // LITERAL .NET FIELDS 
            CheckILFieldInfoAccessible cenv.g cenv.amap m env.eAccessRights finfo
            if not finfo.IsStatic then errorR (Error (FSComp.SR.tcFieldIsNotStatic(finfo.FieldName),m))
            CheckILFieldAttributes cenv.g finfo m
            match finfo.LiteralValue with 
            | None -> error (Error(FSComp.SR.tcFieldNotLiteralCannotBeUsedInPattern(), m))
            | Some lit -> 
                checkNoArgsForLiteral()
                UnifyTypes cenv env m ty (finfo.FieldType(cenv.amap,m))
                let c' = TcFieldInit m lit
                (fun _ -> TPat_const (c',m)),(tpenv,names,takenNames)             
            
        | Item.RecdField rfinfo ->
            // LITERAL F# FIELDS 
            CheckRecdFieldInfoAccessible cenv.amap m env.eAccessRights rfinfo
            if not rfinfo.IsStatic then errorR (Error (FSComp.SR.tcFieldIsNotStatic(rfinfo.Name),m))
            CheckRecdFieldInfoAttributes cenv.g rfinfo m  |> CommitOperationResult        
            match rfinfo.LiteralValue with 
            | None -> error (Error(FSComp.SR.tcFieldNotLiteralCannotBeUsedInPattern(), m))
            | Some lit -> 
                checkNoArgsForLiteral()
                UnifyTypes cenv env m ty rfinfo.FieldType
                let item = Item.RecdField(rfinfo)
                CallNameResolutionSink cenv.tcSink (m,env.NameEnv,item,item,ItemOccurence.Pattern,env.DisplayEnv,env.AccessRights)
                (fun _ -> TPat_const (lit,m)),(tpenv,names,takenNames)             

        | Item.Value vref ->
            match vref.LiteralValue with 
            | None -> error (Error(FSComp.SR.tcNonLiteralCannotBeUsedInPattern(), m))
            | Some lit -> 
                let (_, _, vexpty, _, _) = TcVal true cenv env tpenv vref None m
                CheckValAccessible m env.eAccessRights vref
                CheckFSharpAttributes cenv.g vref.Attribs m |> CommitOperationResult
                checkNoArgsForLiteral()
                UnifyTypes cenv env m ty vexpty
                (fun _ -> TPat_const (lit,m)),(tpenv,names,takenNames)             

        |  _ -> error (Error(FSComp.SR.tcRequireVarConstRecogOrLiteral(),m))

    | SynPat.QuoteExpr(_,m) -> error (Error(FSComp.SR.tcInvalidPattern(),m))
          
    | SynPat.Tuple (args,m) ->
        let argtys = NewInferenceTypes args
        UnifyTypes cenv env m ty (TType_tuple (tupInfoRef, argtys))
        let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) argtys args
        (fun values -> TPat_tuple(tupInfoRef,List.map (fun f -> f values) args',argtys,m)), acc

    | SynPat.StructTuple (args,m) ->
        let argtys = NewInferenceTypes args
        UnifyTypes cenv env m ty (TType_tuple (tupInfoStruct, argtys))
        let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) argtys args
        (fun values -> TPat_tuple(tupInfoStruct,List.map (fun f -> f values) args',argtys,m)), acc

    | SynPat.Paren (p,_) ->
        TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty p

    | SynPat.ArrayOrList (isArray,args,m) ->
        let argty = NewInferenceType ()
        UnifyTypes cenv env m ty (if isArray then mkArrayType cenv.g argty else Tastops.mkListTy cenv.g argty)
        let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) (List.map (fun _ -> argty) args) args
        (fun values -> 
            let args' = List.map (fun f -> f values) args'
            if isArray then TPat_array(args', argty, m)
            else List.foldBack (mkConsListPat cenv.g argty) args' (mkNilListPat cenv.g m argty)), acc

    | SynPat.Record (flds,m) ->
        let tcref,fldsmap,_fldsList  = BuildFieldMap cenv env true ty flds m
        // REVIEW: use _fldsList to type check pattern in code order not field defn order 
        let _,inst,tinst,gtyp = infoOfTyconRef m tcref
        UnifyTypes cenv env m ty gtyp
        let fields = tcref.TrueInstanceFieldsAsList
        let ftys = fields |> List.map (fun fsp -> actualTyOfRecdField inst fsp,fsp) 
        let fldsmap',acc = 
          ((tpenv,names,takenNames), ftys) ||> List.mapFold (fun s (ty,fsp) -> 
              match Map.tryFind fsp.rfield_id.idText fldsmap with
              | Some v -> TcPat warnOnUpper cenv env None vFlags s ty v
              | None -> (fun _ -> TPat_wild m),s)
        (fun values -> TPat_recd (tcref,tinst,List.map (fun f -> f values) fldsmap',m)), 
        acc

    | SynPat.DeprecatedCharRange (c1,c2,m) -> 
        errorR(Deprecated(FSComp.SR.tcUseWhenPatternGuard(),m))
        UnifyTypes cenv env m ty (cenv.g.char_ty)
        (fun _ -> TPat_range(c1,c2,m)),(tpenv,names,takenNames)

    | SynPat.Null m -> 
        AddCxTypeMustSupportNull env.DisplayEnv cenv.css m NoTrace ty
        (fun _ -> TPat_null m),(tpenv,names,takenNames)

    | SynPat.InstanceMember (_,_,_,_,m) -> 
        errorR(Error(FSComp.SR.tcIllegalPattern(),pat.Range))
        (fun _ -> TPat_wild m), (tpenv,names,takenNames)
    | SynPat.FromParseError (pat,_) ->
        suppressErrorReporting (fun () -> TcPatAndRecover warnOnUpper cenv env topValInfo vFlags (tpenv,names,takenNames) (NewErrorType()) pat)

and TcPatterns warnOnUpper cenv env vFlags s argtys args = 
    assert (List.length args  = List.length argtys)
    List.mapFold (fun s (ty,pat) -> TcPat warnOnUpper cenv env None vFlags s ty pat) s (List.zip argtys args)


and solveTypAsError cenv denv m ty =
    let ty2 = NewErrorType ()
    assert((destTyparTy cenv.g ty2).IsFromError)
    SolveTypEqualsTypKeepAbbrevs (MakeConstraintSolverEnv ContextInfo.NoContext cenv.css m denv) 0 m NoTrace ty ty2 |> ignore

and RecordNameAndTypeResolutions_IdeallyWithoutHavingOtherEffects cenv env tpenv expr =
    // This function is motivated by cases like
    //    query { for ... join(for x in f(). }
    // where there is incomplete code in a query, and we are current just dropping a piece of the AST on the floor (above, the bit inside the 'join').
    // 
    // The problem with dropping the AST on the floor is that we get no captured resolutions, which means no Intellisense/QuickInfo/ParamHelp.
    //
    // The idea behind the fix is to semi-typecheck this AST-fragment, just to get resolutions captured.
    //
    // The tricky bit is to not also have any other effects from typechecking, namely producing error diagnostics (which may be spurious) or having 
    // side-effects on the typecheck environment.
    //
    // REVIEW: We are yet to deal with the tricky bit.  As it stands, we turn off error logging, but still have typechecking environment effects.  As a result, 
    // at the very least, you cannot call this function unless you're already reported a typechecking error (the 'worst' possible outcome would be 
    // to incorrectly solve typecheck constraints as a result of effects in this function, and then have the code compile successfully and behave 
    // in some weird way; so ensure the code can't possibly compile before calling this function as an expedient way to get better IntelliSense).
    suppressErrorReporting (fun () -> 
        try ignore(TcExprOfUnknownType cenv env tpenv expr)
        with e -> ())

and RecordNameAndTypeResolutions_IdeallyWithoutHavingOtherEffects_Delayed cenv env tpenv delayed =

    let rec dummyCheckedDelayed delayed =
        match delayed with 
        | DelayedApp (_hpa, arg, _mExprAndArg) :: otherDelayed ->
            RecordNameAndTypeResolutions_IdeallyWithoutHavingOtherEffects cenv env tpenv arg
            dummyCheckedDelayed otherDelayed
        | _ -> ()
    dummyCheckedDelayed delayed

// Calls UnifyTypes, but upon error only does the minimal error recovery
// so that IntelliSense information can continue to be collected.
and UnifyTypesAndRecover cenv env m expectedTy actualTy =
    try
        UnifyTypes cenv env m expectedTy actualTy
    with e ->
        errorRecovery e m

and TcExprOfUnknownType cenv env tpenv expr =
    let exprty = NewInferenceType ()
    let expr',tpenv = TcExpr cenv exprty env tpenv expr
    expr',exprty,tpenv

and TcExprFlex cenv flex ty (env: TcEnv) tpenv (e: SynExpr) =
    if flex then
        let argty = NewInferenceType ()
        AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css e.Range NoTrace ty argty 
        let e',tpenv  = TcExpr cenv argty env tpenv e 
        let e' = mkCoerceIfNeeded cenv.g ty argty e'
        e',tpenv
    else
        TcExpr cenv ty env tpenv e
    

and TcExpr cenv ty (env: TcEnv) tpenv (expr: SynExpr) =
    // Start an error recovery handler 
    // Note the try/catch can lead to tail-recursion problems for iterated constructs, e.g. let... in... 
    // So be careful! 
    try 
        TcExprNoRecover cenv ty env tpenv expr 
    with e -> 
        let m = expr.Range
        // Error recovery - return some rubbish expression, but replace/annotate 
        // the type of the current expression with a type variable that indicates an error 
        errorRecovery e m 
        solveTypAsError cenv env.DisplayEnv m ty
        mkThrow m ty (mkOne cenv.g m), tpenv

and TcExprNoRecover cenv ty (env: TcEnv) tpenv (expr: SynExpr) =

    // Count our way through the expression shape that makes up an object constructor 
    // See notes at definition of "ctor" re. object model constructors. 
    let env = 
        if GetCtorShapeCounter env > 0 then AdjustCtorShapeCounter (fun x -> x - 1) env 
        else env

    TcExprThen cenv ty env tpenv expr []


// This recursive entry is only used from one callsite (DiscardAfterMissingQualificationAfterDot)
// and has been added relatively late in F# 4.0 to preserve the structure of previous code.  It pushes a 'delayed' parameter
// through TcExprOfUnknownType, TcExpr and TcExprNoRecover
and TcExprOfUnknownTypeThen cenv env tpenv expr delayed =
    let exprty = NewInferenceType ()
    let expr',tpenv = 
      try
        TcExprThen cenv exprty env tpenv expr delayed
      with e -> 
        let m = expr.Range
        errorRecovery e m 
        solveTypAsError cenv env.DisplayEnv m exprty
        mkThrow m exprty (mkOne cenv.g m), tpenv
    expr',exprty,tpenv

/// This is used to typecheck legitimate 'main body of constructor' expressions 
and TcExprThatIsCtorBody safeInitInfo cenv overallTy env tpenv expr =
    let env = {env with eCtorInfo = Some (InitialExplicitCtorInfo safeInitInfo) }
    let expr,tpenv = TcExpr cenv overallTy env tpenv expr
    let expr = CheckAndRewriteObjectCtor cenv.g env expr
    expr,tpenv

/// This is used to typecheck all ordinary expressions including constituent 
/// parts of ctor. 
and TcExprThatCanBeCtorBody cenv overallTy env tpenv expr =
    let env = if AreWithinCtorShape env then AdjustCtorShapeCounter (fun x -> x + 1) env else env
    TcExpr cenv overallTy env tpenv expr

/// This is used to typecheck legitimate 'non-main body of object constructor' expressions 
and TcExprThatCantBeCtorBody cenv overallTy env tpenv expr =
    let env = if AreWithinCtorShape env then ExitCtorShapeRegion env else env
    TcExpr cenv overallTy env tpenv expr

/// This is used to typecheck legitimate 'non-main body of object constructor' expressions 
and TcStmtThatCantBeCtorBody cenv env tpenv expr =
    let env = if AreWithinCtorShape env then ExitCtorShapeRegion env else env
    TcStmt cenv env tpenv expr

and TcStmt cenv env tpenv synExpr =
    let expr,ty,tpenv = TcExprOfUnknownType cenv env tpenv synExpr
    let m = synExpr.Range
    let wasUnit = UnifyUnitType cenv env.DisplayEnv m ty (Some expr)
    if wasUnit then
        expr,tpenv
    else
        mkCompGenSequential m expr (mkUnit cenv.g m),tpenv

/// During checking of expressions of the form (x(y)).z(w1,w2) 
/// keep a stack of things on the right. This lets us recognize 
/// method applications and other item-based syntax. 
and TcExprThen cenv overallTy env tpenv synExpr delayed =
    match synExpr with 

    | LongOrSingleIdent (isOpt,longId,altNameRefCellOpt,mLongId) ->
        if isOpt then errorR(Error(FSComp.SR.tcSyntaxErrorUnexpectedQMark(),mLongId))
        // Check to see if pattern translation decided to use an alternative identifier.
        match altNameRefCellOpt with 
        | Some {contents = Decided altId} -> TcExprThen cenv overallTy env tpenv (SynExpr.LongIdent(isOpt,LongIdentWithDots([altId],[]),None,mLongId)) delayed
        | _ -> TcLongIdentThen cenv overallTy env tpenv longId delayed

    // f x
    | SynExpr.App (hpa,_,func,arg,mFuncAndArg) ->
        TcExprThen cenv overallTy env tpenv func ((DelayedApp (hpa, arg, mFuncAndArg)):: delayed)

    // e<tyargs>
    | SynExpr.TypeApp (func, _, typeArgs, _, _, mTypeArgs, mFuncAndTypeArgs) ->
        TcExprThen cenv overallTy env tpenv func ((DelayedTypeApp (typeArgs, mTypeArgs, mFuncAndTypeArgs)):: delayed)

    // e1.id1
    // e1.id1.id2
    // etc.
    | SynExpr.DotGet (e1,_,LongIdentWithDots(longId,_),_) ->
        TcExprThen cenv overallTy env tpenv e1 ((DelayedDotLookup (longId,synExpr.RangeSansAnyExtraDot))::delayed)
           
    // e1.[e2]
    // e1.[e21,...,e2n]
    // etc.
    | SynExpr.DotIndexedGet (e1,e2,mDot,mWholeExpr) ->
        TcIndexerThen cenv env overallTy mWholeExpr mDot tpenv synExpr e1 e2 delayed

    // e1.[e2] <- e3
    // e1.[e21,...,e2n] <- e3
    // etc.
    | SynExpr.DotIndexedSet (e1,e2,_,_,mDot,mWholeExpr) ->
        TcIndexerThen cenv env overallTy mWholeExpr mDot tpenv synExpr e1 e2 delayed
    
    | _  ->
        match delayed with 
        | [] -> TcExprUndelayed cenv overallTy env tpenv synExpr
        | _ -> 
            let expr,exprty,tpenv = TcExprUndelayedNoType cenv env tpenv synExpr
            PropagateThenTcDelayed cenv overallTy env tpenv synExpr.Range (MakeApplicableExprNoFlex cenv expr) exprty ExprAtomicFlag.NonAtomic delayed

and TcExprs cenv env m tpenv flexes argtys args = 
    if (List.length args  <> List.length argtys) then error(Error(FSComp.SR.tcExpressionCountMisMatch((List.length argtys), (List.length args)),m))
    (tpenv, List.zip3 flexes argtys args) ||> List.mapFold (fun tpenv (flex,ty,e) -> 
         TcExprFlex cenv flex ty env tpenv e)

and CheckSuperInit cenv objTy m = 
        // Check the type is not abstract
        if isAppTy cenv.g objTy && (let tcref = tcrefOfAppTy cenv.g objTy in isAbstractTycon tcref.Deref) then 
            errorR(Error(FSComp.SR.tcAbstractTypeCannotBeInstantiated(),m))
        

//-------------------------------------------------------------------------
// TcExprUndelayed
//------------------------------------------------------------------------- 

and TcExprUndelayedNoType cenv env tpenv expr : Expr * TType * _ =
    let exprty = NewInferenceType ()
    let expr',tpenv = TcExprUndelayed cenv exprty env tpenv expr
    expr',exprty,tpenv

and TcExprUndelayed cenv overallTy env tpenv (expr: SynExpr) =
    match expr with 
    | SynExpr.Paren (expr2,_,_,mWholeExprIncludingParentheses) -> 
        // We invoke CallExprHasTypeSink for every construct which is atomic in the syntax, i.e. where a '.' immediately following the 
        // construct is a dot-lookup for the result of the construct. 
        CallExprHasTypeSink cenv.tcSink (mWholeExprIncludingParentheses,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)
        TcExpr cenv overallTy env tpenv expr2

    | SynExpr.DotIndexedGet _ | SynExpr.DotIndexedSet _
    | SynExpr.TypeApp _ | SynExpr.Ident _ | SynExpr.LongIdent _ | SynExpr.App _ | SynExpr.DotGet _ -> error(Error(FSComp.SR.tcExprUndelayed(), expr.Range))

    | SynExpr.Const (SynConst.String (s,m),_) -> 
        CallExprHasTypeSink cenv.tcSink (m,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)
        TcConstStringExpr cenv overallTy env m tpenv s

    | SynExpr.Const (c,m) -> 
        CallExprHasTypeSink cenv.tcSink (m,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)
        TcConstExpr cenv overallTy env m tpenv c

    | SynExpr.Lambda _ -> TcIteratedLambdas cenv true env overallTy Set.empty tpenv expr

    | SynExpr.Match (spMatch,x,matches,isExnMatch,_m) ->

        let x',inputTy,tpenv = TcExprOfUnknownType cenv env tpenv x
        let mExpr = x'.Range
        let v,e, tpenv = TcAndPatternCompileMatchClauses mExpr mExpr (if isExnMatch then Throw else ThrowIncompleteMatchException) cenv inputTy overallTy env tpenv matches
        (mkLet spMatch mExpr v x'  e,tpenv)

    // (function[spMatch] pat1 -> expr1 ... | patN -> exprN)
    //
    //  -->   
    //      (fun anonArg -> let[spMatch] anonVal = anonArg in pat1 -> expr1 ... | patN -> exprN)
    //
    // Note the presence of the "let" is visible in quotations regardless of the presence of sequence points, so 
    //     <@ function x -> (x:int) @>
    // is
    //     Lambda (_arg2, Let (x, _arg2, x))
    
    | SynExpr.MatchLambda (isExnMatch,argm,clauses,spMatch,m) -> // (spMatch,x,matches,isExnMatch,m) ->

        let domainTy,resultTy = UnifyFunctionType None cenv env.DisplayEnv m overallTy
        let idv1,idve1 = mkCompGenLocal argm (cenv.synArgNameGenerator.New()) domainTy
        let envinner = ExitFamilyRegion env
        let idv2,matchExpr, tpenv = TcAndPatternCompileMatchClauses m argm (if isExnMatch then Throw else ThrowIncompleteMatchException) cenv domainTy resultTy envinner tpenv clauses
        let overallExpr = mkMultiLambda m [idv1] ((mkLet spMatch m idv2 idve1  matchExpr),resultTy)
        overallExpr,tpenv

    | SynExpr.Assert (x,m) ->
        TcAssertExpr cenv overallTy env m tpenv x

    | SynExpr.Fixed (_,m) ->
        error(Error(FSComp.SR.tcFixedNotAllowed(),m))

    // e : ty
    | SynExpr.Typed (e,cty,m) ->
        let tgty,tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs ItemOccurence.UseInType  env tpenv cty
        UnifyTypes cenv env m overallTy tgty
        let e',tpenv = TcExpr cenv overallTy env tpenv e 
        e',tpenv

    // e :? ty
    | SynExpr.TypeTest (e,tgty,m) ->
        let e',srcTy,tpenv = TcExprOfUnknownType cenv env tpenv e 
        UnifyTypes cenv env m overallTy cenv.g.bool_ty
        let tgty,tpenv = TcType cenv NewTyparsOK CheckCxs ItemOccurence.UseInType  env tpenv tgty
        TcRuntimeTypeTest (*isCast*)false (*isOperator*)true cenv env.DisplayEnv m tgty srcTy        
        let e' = mkCallTypeTest cenv.g m tgty  e'
        e', tpenv
    
    // SynExpr.AddressOf is noted in the syntax ast in order to recognize it as concrete type information 
    // during type checking, in particular prior to resolving overloads. This helps distinguish 
    // its use at method calls from the use of the conflicting 'ref' mechanism for passing byref parameters 
    | SynExpr.AddressOf(byref,e,opm,m) -> 
        TcExpr cenv overallTy env tpenv (mkSynPrefix opm m (if byref then "~&" else "~&&") e) 
        
    | SynExpr.Upcast (e,_,m) | SynExpr.InferredUpcast (e,m) -> 
        let e',srcTy,tpenv = TcExprOfUnknownType cenv env tpenv e 
        let tgty,tpenv = 
          match expr with
          | SynExpr.Upcast (_,tgty,m) -> 
              let tgty,tpenv = TcType cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv tgty
              UnifyTypes cenv env m tgty overallTy
              tgty,tpenv
          | SynExpr.InferredUpcast _ -> 
              overallTy,tpenv 
          | _ -> failwith "upcast"
        TcStaticUpcast cenv env.DisplayEnv m tgty srcTy
        mkCoerceExpr(e',tgty,m,srcTy),tpenv

    | SynExpr.Downcast(e,_,m) | SynExpr.InferredDowncast (e,m) ->
        let e',srcTy,tpenv = TcExprOfUnknownType cenv env tpenv e 
        let tgty,tpenv,isOperator = 
          match expr with
          | SynExpr.Downcast (_,tgty,m) -> 
              let tgty,tpenv = TcType cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv tgty
              UnifyTypes cenv env m tgty overallTy
              tgty,tpenv,true
          | SynExpr.InferredDowncast _ -> overallTy,tpenv,false
          | _ -> failwith "downcast"
        TcRuntimeTypeTest (*isCast*)true isOperator cenv env.DisplayEnv m tgty srcTy

        // TcRuntimeTypeTest ensures tgty is a nominal type. Hence we can insert a check here 
        // based on the nullness semantics of the nominal type. 
        let e' = mkCallUnbox cenv.g m tgty  e'
        e',tpenv

    | SynExpr.Null m ->
        AddCxTypeMustSupportNull env.DisplayEnv cenv.css m NoTrace overallTy
        mkNull m overallTy,tpenv

    | SynExpr.Lazy (e,m) ->
        let ety = NewInferenceType ()
        UnifyTypes cenv env m overallTy (mkLazyTy cenv.g ety)
        let e',tpenv = TcExpr cenv ety env tpenv e 
        mkLazyDelayed cenv.g m ety (mkUnitDelayLambda cenv.g m e'), tpenv

    | SynExpr.Tuple (args,_,m) -> 
        let argtys = UnifyRefTupleType env.eContextInfo cenv env.DisplayEnv m overallTy args
        // No subsumption at tuple construction
        let flexes = argtys |> List.map (fun _ -> false)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        mkRefTupled cenv.g m args' argtys, tpenv

    | SynExpr.StructTuple (args,_,m) -> 
        let argtys = UnifyStructTupleType env.eContextInfo cenv env.DisplayEnv m overallTy args
        // No subsumption at tuple construction
        let flexes = argtys |> List.map (fun _ -> false)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        mkAnyTupled cenv.g m tupInfoStruct args' argtys, tpenv

    | SynExpr.ArrayOrList (isArray,args,m) -> 
        CallExprHasTypeSink cenv.tcSink (m,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)

        let argty = NewInferenceType ()
        UnifyTypes cenv env m overallTy (if isArray then mkArrayType cenv.g argty else Tastops.mkListTy cenv.g argty)

        // Always allow subsumption if a nominal type is known prior to type checking any arguments
        let flex = not (isTyparTy cenv.g argty)
        let args',tpenv = List.mapFold (TcExprFlex cenv flex argty env) tpenv args
        
        let expr = 
            if isArray then Expr.Op(TOp.Array, [argty],args',m)
            else List.foldBack (mkCons cenv.g argty) args' (mkNil cenv.g m argty)
        expr,tpenv

    | SynExpr.New (superInit,synObjTy,arg,mNewExpr) -> 
        let objTy,tpenv = TcType cenv NewTyparsOK CheckCxs ItemOccurence.Use env tpenv synObjTy
        UnifyTypes cenv env mNewExpr overallTy objTy        
        TcNewExpr cenv env tpenv objTy (Some synObjTy.Range) superInit arg mNewExpr

    | SynExpr.ObjExpr(objTy,argopt,binds,extraImpls,mNewExpr,m) ->
        CallExprHasTypeSink cenv.tcSink (m,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)
        TcObjectExpr cenv overallTy env tpenv (objTy,argopt,binds,extraImpls,mNewExpr,m)
            
    | SynExpr.Record (inherits, optOrigExpr, flds, mWholeExpr) -> 
        CallExprHasTypeSink cenv.tcSink (mWholeExpr,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)
        TcRecdExpr cenv overallTy env tpenv (inherits,optOrigExpr,flds,mWholeExpr)

    | SynExpr.While (spWhile,e1,e2,m) ->
        UnifyTypes cenv env m overallTy cenv.g.unit_ty
        let e1',tpenv = TcExpr cenv (cenv.g.bool_ty) env tpenv e1
        let e2',tpenv = TcStmt cenv env tpenv e2
        mkWhile cenv.g (spWhile,NoSpecialWhileLoopMarker,e1',e2',m),tpenv

    | SynExpr.For (spBind,id,start,dir,finish,body,m) ->
        UnifyTypes cenv env m overallTy cenv.g.unit_ty
        let startExpr ,tpenv = TcExpr cenv (cenv.g.int_ty) env tpenv start
        let finishExpr,tpenv = TcExpr cenv (cenv.g.int_ty) env tpenv finish
        let idv,_ = mkLocal id.idRange  id.idText cenv.g.int_ty
        let envinner = AddLocalVal cenv.tcSink m idv env

        // notify name resolution sink about loop variable
        let item = Item.Value(mkLocalValRef idv)
        CallNameResolutionSink cenv.tcSink (idv.Range, env.NameEnv, item, item, ItemOccurence.Binding, env.DisplayEnv, env.eAccessRights)
        
        let bodyExpr,tpenv = TcStmt cenv envinner tpenv body
        mkFastForLoop  cenv.g (spBind,m,idv,startExpr,dir,finishExpr,bodyExpr), tpenv
        
    | SynExpr.ForEach (spForLoop, SeqExprOnly seqExprOnly, isFromSource, pat, enumSynExpr, bodySynExpr, m) ->
        assert isFromSource
        if seqExprOnly then warning (Error(FSComp.SR.tcExpressionRequiresSequence(),m))
        TcForEachExpr cenv overallTy env tpenv (pat,enumSynExpr,bodySynExpr,m,spForLoop)

    | SynExpr.CompExpr (isArrayOrList,isNotNakedRefCell,comp,m) ->
        let env = ExitFamilyRegion env
        if not isArrayOrList then 
            match comp with 
            | SynExpr.New _ -> 
                errorR(Error(FSComp.SR.tcInvalidObjectExpressionSyntaxForm(),m))
            | SimpleSemicolonSequence false _ -> 
                errorR(Error(FSComp.SR.tcInvalidObjectSequenceOrRecordExpression(),m))
            | _ -> 
                ()
        if not !isNotNakedRefCell && not cenv.g.compilingFslib then 
            error(Error(FSComp.SR.tcInvalidSequenceExpressionSyntaxForm(),m))
        
        TcComputationOrSequenceExpression cenv env overallTy m None tpenv comp
        
    | SynExpr.ArrayOrListOfSeqExpr (isArray,comp,m)  ->
        CallExprHasTypeSink cenv.tcSink (m,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)

        match comp with 
        | SynExpr.CompExpr(_,_,(SimpleSemicolonSequence true elems as body),_) -> 
            match body with 
            | SimpleSemicolonSequence false _ -> 
                ()
            | _ -> 
                errorR(Deprecated(FSComp.SR.tcExpressionWithIfRequiresParenthesis(),m))

            let replacementExpr = 
                if isArray then 
                    // This are to improve parsing/processing speed for parser tables by converting to an array blob ASAP 
                    let nelems = elems.Length 
                    if nelems > 0 && List.forall (function SynExpr.Const(SynConst.UInt16 _,_) -> true | _ -> false) elems 
                    then SynExpr.Const (SynConst.UInt16s (Array.ofList (List.map (function SynExpr.Const(SynConst.UInt16 x,_) -> x | _ -> failwith "unreachable") elems)), m)
                    elif nelems > 0 && List.forall (function SynExpr.Const(SynConst.Byte _,_) -> true | _ -> false) elems 
                    then SynExpr.Const (SynConst.Bytes (Array.ofList (List.map (function SynExpr.Const(SynConst.Byte x,_) -> x | _ -> failwith "unreachable") elems), m), m)
                    else SynExpr.ArrayOrList(isArray, elems, m)
                else 
                    if elems.Length > 500 then 
                        error(Error(FSComp.SR.tcListLiteralMaxSize(),m))
                    SynExpr.ArrayOrList(isArray, elems, m)

            TcExprUndelayed cenv overallTy env tpenv replacementExpr
        | _ -> 
            let genCollElemTy = NewInferenceType ()
            let genCollTy =  (if isArray then mkArrayType else mkListTy) cenv.g genCollElemTy
            UnifyTypes cenv env m overallTy genCollTy
            let exprty = NewInferenceType ()
            let genEnumTy =  mkSeqTy cenv.g genCollElemTy
            AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m NoTrace genEnumTy exprty 
            let expr,tpenv = TcExpr cenv exprty env tpenv comp
            let expr = mkCoerceIfNeeded cenv.g genEnumTy (tyOfExpr cenv.g expr) expr
            (if isArray then mkCallSeqToArray else mkCallSeqToList) cenv.g m genCollElemTy 
                // We add a call to 'seq ... ' to make sure sequence expression compilation gets applied to the contents of the
                // comprehension. But don't do this in FSharp.Core.dll since 'seq' may not yet be defined.
                ((if cenv.g.compilingFslib then id else mkCallSeq cenv.g m genCollElemTy)
                    (mkCoerceExpr(expr,genEnumTy,expr.Range,exprty))),tpenv

    | SynExpr.LetOrUse _ ->
        TcLinearExprs (TcExprThatCanBeCtorBody cenv) cenv env overallTy tpenv false expr (fun x -> x) 

    | SynExpr.TryWith (e1,_mTryToWith,clauses,mWithToLast,mTryToLast,spTry,spWith) ->
        let e1',tpenv = TcExpr cenv overallTy env tpenv e1
        // Compile the pattern twice, once as a List.filter with all succeeding targets returning "1", and once as a proper catch block. 
        let filterClauses = clauses |> List.map (function (Clause(pat,optWhenExpr,_,m,_)) -> Clause(pat,optWhenExpr,(SynExpr.Const(SynConst.Int32 1,m)),m,SuppressSequencePointAtTarget))
        let checkedFilterClauses, tpenv = TcMatchClauses cenv cenv.g.exn_ty cenv.g.int_ty env tpenv filterClauses
        let checkedHandlerClauses, tpenv = TcMatchClauses cenv cenv.g.exn_ty overallTy env tpenv clauses
        let v1,filter_expr = CompilePatternForMatchClauses cenv env mWithToLast mWithToLast true FailFilter cenv.g.exn_ty cenv.g.int_ty checkedFilterClauses
        let v2,handler_expr = CompilePatternForMatchClauses cenv env mWithToLast mWithToLast true Rethrow cenv.g.exn_ty overallTy checkedHandlerClauses
        mkTryWith cenv.g (e1',v1,filter_expr,v2,handler_expr,mTryToLast,overallTy,spTry,spWith),tpenv

    | SynExpr.TryFinally (e1,e2,mTryToLast,spTry,spFinally) ->
        let e1',tpenv = TcExpr cenv overallTy env tpenv e1
        let e2',tpenv = TcStmt cenv env tpenv e2
        mkTryFinally cenv.g (e1',e2',mTryToLast,overallTy,spTry,spFinally),tpenv

    | SynExpr.JoinIn(e1,mInToken,e2,mAll) -> 
        errorR(Error(FSComp.SR.parsUnfinishedExpression("in"),mInToken))
        let _,_,tpenv = suppressErrorReporting (fun () -> TcExprOfUnknownType cenv env tpenv e1)  
        let _,_,tpenv = suppressErrorReporting (fun () -> TcExprOfUnknownType cenv env tpenv e2)  
        mkDefault(mAll,overallTy), tpenv

    | SynExpr.ArbitraryAfterError(_debugStr, m) -> 
        //solveTypAsError cenv env.DisplayEnv m overallTy
        mkDefault(m,overallTy), tpenv

    // expr. (already reported as an error)
    | SynExpr.DiscardAfterMissingQualificationAfterDot (e1,m) ->
        let _,_,tpenv = suppressErrorReporting (fun () -> TcExprOfUnknownTypeThen cenv env tpenv e1 [DelayedDot])
        mkDefault(m,overallTy),tpenv

    | SynExpr.FromParseError (e1,m) -> 
        //solveTypAsError cenv env.DisplayEnv m overallTy
        let _,tpenv = suppressErrorReporting (fun () -> TcExpr cenv overallTy env tpenv e1)
        mkDefault(m,overallTy),tpenv

    | SynExpr.Sequential (sp,dir,e1,e2,m) ->
        if dir then 
            TcLinearExprs (TcExprThatCanBeCtorBody cenv) cenv env overallTy tpenv false expr (fun x -> x) 
        else 
            // Constructors using "new (...) = <ctor-expr> then <expr>" 
            let e1',tpenv = TcExprThatCanBeCtorBody cenv overallTy env tpenv e1
            if (GetCtorShapeCounter env) <> 1 then 
                errorR(Error(FSComp.SR.tcExpressionFormRequiresObjectConstructor(),m))
            let e2',tpenv = TcStmtThatCantBeCtorBody cenv env tpenv e2
            Expr.Sequential(e1',e2',ThenDoSeq,sp,m),tpenv

    | SynExpr.Do (e1,m) ->
          UnifyTypes cenv env m overallTy cenv.g.unit_ty
          TcStmtThatCantBeCtorBody cenv env tpenv e1

    | SynExpr.IfThenElse (e1,e2,e3opt,spIfToThen,isRecovery,mIfToThen,m) ->
        let e1',tpenv = TcExprThatCantBeCtorBody cenv cenv.g.bool_ty env tpenv e1
        let e2',tpenv =
            if not isRecovery && Option.isNone e3opt then
                let env = { env with eContextInfo = ContextInfo.OmittedElseBranch e2.Range}
                UnifyTypes cenv env m cenv.g.unit_ty overallTy
                TcExprThatCanBeCtorBody cenv overallTy env tpenv e2
            else
                TcExprThatCanBeCtorBody cenv overallTy env tpenv e2
        let e3',sp2,tpenv = 
            match e3opt with 
            | None ->
                mkUnit cenv.g mIfToThen,SuppressSequencePointAtTarget, tpenv // the fake 'unit' value gets exactly the same range as spIfToThen
            | Some e3 ->
                let env = { env with eContextInfo = ContextInfo.ElseBranchResult e3.Range }
                let e3',tpenv = TcExprThatCanBeCtorBody cenv overallTy env tpenv e3 
                e3',SequencePointAtTarget,tpenv
        primMkCond spIfToThen SequencePointAtTarget sp2 m overallTy e1' e2' e3', tpenv

    // This is for internal use in the libraries only 
    | SynExpr.LibraryOnlyStaticOptimization (constraints,e2,e3,m) ->
        let constraints',tpenv = List.mapFold (TcStaticOptimizationConstraint cenv env) tpenv constraints
        // Do not force the types of the two expressions to be equal 
        // This means uses of this construct have to be very carefully written
        let e2',_, tpenv = TcExprOfUnknownType cenv env tpenv e2
        let e3',tpenv = TcExpr cenv overallTy env tpenv e3
        Expr.StaticOptimization(constraints',e2',e3',m), tpenv

    /// e1.longId <- e2
    | SynExpr.DotSet (e1,(LongIdentWithDots(longId,_) as lidwd),e2,mStmt) ->
        if lidwd.ThereIsAnExtraDotAtTheEnd then
            // just drop rhs on the floor
            let mExprAndDotLookup = unionRanges e1.Range (rangeOfLid longId)
            TcExprThen cenv overallTy env tpenv e1 [DelayedDotLookup(longId,mExprAndDotLookup)]
        else
            let mExprAndDotLookup = unionRanges e1.Range (rangeOfLid longId)
            TcExprThen cenv overallTy env tpenv e1 [DelayedDotLookup(longId,mExprAndDotLookup); MakeDelayedSet(e2,mStmt)]

    /// e1.longId(e2) <- e3, very rarely used named property setters
    | SynExpr.DotNamedIndexedPropertySet (e1,(LongIdentWithDots(longId,_) as lidwd),e2,e3,mStmt) ->
        if lidwd.ThereIsAnExtraDotAtTheEnd then
            // just drop rhs on the floor
            let mExprAndDotLookup = unionRanges e1.Range (rangeOfLid longId)
            TcExprThen cenv overallTy env tpenv e1 [DelayedDotLookup(longId,mExprAndDotLookup)]
        else
            let mExprAndDotLookup = unionRanges e1.Range (rangeOfLid longId)
            TcExprThen cenv overallTy env tpenv e1 [DelayedDotLookup(longId,mExprAndDotLookup); DelayedApp(ExprAtomicFlag.Atomic, e2, mStmt); MakeDelayedSet(e3,mStmt)]

    | SynExpr.LongIdentSet (lidwd,e2,m) -> 
        if lidwd.ThereIsAnExtraDotAtTheEnd then
            // just drop rhs on the floor
            TcLongIdentThen cenv overallTy env tpenv lidwd [ ]
        else
            TcLongIdentThen cenv overallTy env tpenv lidwd [ MakeDelayedSet(e2, m) ]
    
    // Type.Items(e1) <- e2 
    | SynExpr.NamedIndexedPropertySet (lidwd,e1,e2,mStmt) ->
        if lidwd.ThereIsAnExtraDotAtTheEnd then
            // just drop rhs on the floor
            TcLongIdentThen cenv overallTy env tpenv lidwd [ ]
        else
            TcLongIdentThen cenv overallTy env tpenv lidwd [ DelayedApp(ExprAtomicFlag.Atomic, e1, mStmt); MakeDelayedSet(e2,mStmt) ]

    | SynExpr.TraitCall(tps,memSpfn,arg,m) ->
        let synTypes =  tps |> List.map (fun tp -> SynType.Var(tp,m))
        let (TTrait(_,logicalCompiledName,_,argtys,returnTy,_) as traitInfo),tpenv = TcPseudoMemberSpec cenv NewTyparsOK env synTypes tpenv memSpfn m
        if BakedInTraitConstraintNames.Contains logicalCompiledName then 
            warning(BakedInMemberConstraintName(logicalCompiledName,m))
        
        let returnTy = GetFSharpViewOfReturnType cenv.g returnTy
        let args,namedCallerArgs = GetMethodArgs arg 
        if not (isNil namedCallerArgs) then errorR(Error(FSComp.SR.tcNamedArgumentsCannotBeUsedInMemberTraits(),m))
        // Subsumption at trait calls if arguments have nominal type prior to unification of any arguments or return type
        let flexes = argtys |> List.map (isTyparTy cenv.g >> not)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        AddCxMethodConstraint env.DisplayEnv cenv.css m NoTrace traitInfo
        UnifyTypes cenv env m overallTy returnTy      
        Expr.Op(TOp.TraitCall(traitInfo), [], args', m), tpenv
          
    | SynExpr.LibraryOnlyUnionCaseFieldGet (e1,c,n,m) ->
        let e1',ty1,tpenv = TcExprOfUnknownType cenv env tpenv e1
        let mkf,ty2 = TcUnionCaseOrExnField cenv env ty1 m c n 
                          ((fun (a,b) n -> mkUnionCaseFieldGetUnproven cenv.g (e1',a,b,n,m)),
                           (fun a n -> mkExnCaseFieldGet(e1',a,n,m)))
        UnifyTypes cenv env m overallTy ty2
        mkf n,tpenv

    | SynExpr.LibraryOnlyUnionCaseFieldSet (e1,c,n,e2,m) ->
        UnifyTypes cenv env m overallTy cenv.g.unit_ty
        let e1',ty1,tpenv = TcExprOfUnknownType cenv env tpenv e1
        let mkf,ty2 = TcUnionCaseOrExnField cenv  env ty1 m c n
                          ((fun (a,b) n e2' -> 
                             if not (isUnionCaseFieldMutable cenv.g a n) then errorR(Error(FSComp.SR.tcFieldIsNotMutable(),m))
                             mkUnionCaseFieldSet(e1',a,b,n,e2',m)),
                           (fun a n e2' -> 
                             if not (isExnFieldMutable a n) then errorR(Error(FSComp.SR.tcFieldIsNotMutable(),m))
                             mkExnCaseFieldSet(e1',a,n,e2',m)))
        let e2',tpenv = TcExpr cenv ty2 env tpenv e2
        mkf n e2',tpenv

    | SynExpr.LibraryOnlyILAssembly (s,tyargs,args,rtys,m) ->
        let argtys = NewInferenceTypes args
        let tyargs',tpenv = TcTypes cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv tyargs
        // No subsumption at uses of IL assembly code
        let flexes = argtys |> List.map (fun _ -> false)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        let rtys',tpenv = TcTypes cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv rtys
        let returnTy = 
            match rtys' with 
            | [] -> cenv.g.unit_ty
            | [ returnTy ] -> returnTy
            | _ -> error(InternalError("Only zero or one pushed items are permitted in IL assembly code",m))
        UnifyTypes cenv env m overallTy returnTy
        mkAsmExpr(Array.toList s,tyargs',args',rtys',m),tpenv

    | SynExpr.Quote(oper,raw,ast,isFromQueryExpression,m) ->
        CallExprHasTypeSink cenv.tcSink (m,env.NameEnv,overallTy, env.DisplayEnv,env.eAccessRights)
        TcQuotationExpr cenv overallTy env tpenv (oper,raw,ast,isFromQueryExpression,m) 

    | SynExpr.YieldOrReturn ((isTrueYield,_),_,m)
    | SynExpr.YieldOrReturnFrom ((isTrueYield,_),_,m) when isTrueYield -> 
         error(Error(FSComp.SR.tcConstructRequiresListArrayOrSequence(),m))
    | SynExpr.YieldOrReturn ((_,isTrueReturn),_,m)
    | SynExpr.YieldOrReturnFrom ((_,isTrueReturn),_,m) when isTrueReturn -> 
         error(Error(FSComp.SR.tcConstructRequiresComputationExpressions(),m))
    | SynExpr.YieldOrReturn (_,_,m)
    | SynExpr.YieldOrReturnFrom (_,_,m) 
    | SynExpr.ImplicitZero m ->
         error(Error(FSComp.SR.tcConstructRequiresSequenceOrComputations(),m))
    | SynExpr.DoBang  (_,m) 
    | SynExpr.LetOrUseBang  (_,_,_,_,_,_,m) -> 
         error(Error(FSComp.SR.tcConstructRequiresComputationExpression(),m))

/// Check lambdas as a group, to catch duplicate names in patterns
and TcIteratedLambdas cenv isFirst (env: TcEnv) overallTy takenNames tpenv e = 
    match e with 
    | SynExpr.Lambda (isMember,isSubsequent,spats,bodyExpr,m) when isMember || isFirst || isSubsequent ->
        let domainTy,resultTy = UnifyFunctionType None cenv env.DisplayEnv m overallTy
        let vs, (tpenv,names,takenNames) = TcSimplePats cenv isMember CheckCxs domainTy env (tpenv,Map.empty,takenNames) spats
        let envinner,_,vspecMap = MakeAndPublishSimpleVals cenv env m names true
        let byrefs = vspecMap |> Map.map (fun _ v -> isByrefTy cenv.g v.Type, v)
        let envinner = if isMember then envinner else ExitFamilyRegion envinner
        let bodyExpr,tpenv = TcIteratedLambdas cenv false envinner resultTy takenNames tpenv bodyExpr
        // See bug 5758: Non-monontonicity in inference: need to ensure that parameters are never inferred to have byref type, instead it is always declared
        byrefs  |> Map.iter (fun _ (orig,v) -> 
            if not orig && isByrefTy cenv.g v.Type then errorR(Error(FSComp.SR.tcParameterInferredByref v.DisplayName,v.Range)))
        mkMultiLambda m (List.map (fun nm -> NameMap.find nm vspecMap) vs) (bodyExpr,resultTy),tpenv 
    | e -> 
        // Dive into the expression to check for syntax errors and suppress them if they show.
        conditionallySuppressErrorReporting (not isFirst && synExprContainsError e) (fun () ->
            TcExpr cenv overallTy env tpenv e)
        

// Check expr.[idx] 
// This is a little over complicated for my liking. Basically we want to intepret e1.[idx] as e1.Item(idx). 
// However it's not so simple as all that. First "Item" can have a different name according to an attribute in 
// .NET metadata.  This means we manually typecheck 'e1' and look to see if it has a nominal type. We then 
// do the right thing in each case. 
and TcIndexerThen cenv env overallTy mWholeExpr mDot tpenv wholeExpr e1 indexArgs delayed = 
    let ad = env.eAccessRights
    let e1',e1ty,tpenv = TcExprOfUnknownType cenv env tpenv e1
    
    // Find the first type in the effective hierarchy that either has a DefaultMember attribute OR 
    // has a member called 'Item' 
    let propName = 
        match indexArgs with 
        | [SynIndexerArg.One _] -> 
            FoldPrimaryHierarchyOfType (fun typ acc -> 
                match acc with
                | None ->
                    match tryDestAppTy cenv.g typ with
                    | Some tcref ->
                        TryFindTyconRefStringAttribute cenv.g mWholeExpr cenv.g.attrib_DefaultMemberAttribute tcref 
                    | None ->
                        match AllPropInfosOfTypeInScope cenv.infoReader env.NameEnv (Some "Item", ad) IgnoreOverrides mWholeExpr typ with
                        | [] -> None
                        | _ -> Some "Item"
                 | _ -> acc)
              cenv.g 
              cenv.amap 
              mWholeExpr 
              AllowMultiIntfInstantiations.Yes
              e1ty
              None
        | _ -> Some "GetSlice"

    let isNominal = isAppTy cenv.g e1ty
    
    let isArray = isArrayTy cenv.g e1ty 
    let isString = typeEquiv cenv.g cenv.g.string_ty e1ty 

    let idxRange = indexArgs |> List.map (fun e -> e.Range) |> List.reduce unionRanges 
    let GetIndexArgs (es: SynIndexerArg list) = [ for e in es do yield! e.Exprs ]
    let MakeIndexParam vopt = 
        match indexArgs with 
        | []  -> failwith "unexpected empty index list"
        | [SynIndexerArg.One h] -> SynExpr.Paren(h,range0,None,idxRange)
        | _ -> SynExpr.Paren(SynExpr.Tuple(GetIndexArgs indexArgs @ Option.toList vopt,[],idxRange),range0,None,idxRange)

    let attemptArrayString = 
        if isArray || isString then 

            let indexOpPath = ["Microsoft";"FSharp";"Core";"LanguagePrimitives";"IntrinsicFunctions"]
            let sliceOpPath = ["Microsoft";"FSharp";"Core";"Operators";"OperatorIntrinsics"]
            let info = 
                match isString,isArray,wholeExpr with 
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.One(SynExpr.Tuple ([_;_] as idxs,_,_))],_,_)           -> Some (indexOpPath,"GetArray2D", idxs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.One(SynExpr.Tuple ([_;_;_] as idxs,_,_))],_,_)         -> Some (indexOpPath,"GetArray3D", idxs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.One(SynExpr.Tuple ([_;_;_;_] as idxs,_,_))],_,_)       -> Some (indexOpPath,"GetArray4D", idxs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.One idx],_,_)                                          -> Some (indexOpPath,"GetArray", [idx])
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.One(SynExpr.Tuple ([_;_] as idxs,_,_))] ,e3,_,_,_)     -> Some (indexOpPath,"SetArray2D", (idxs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.One(SynExpr.Tuple ([_;_;_] as idxs,_,_))] ,e3,_,_,_)   -> Some (indexOpPath,"SetArray3D", (idxs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.One(SynExpr.Tuple ([_;_;_;_] as idxs,_,_))] ,e3,_,_,_) -> Some (indexOpPath,"SetArray4D", (idxs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.One _],e3,_,_,_)                                       -> Some (indexOpPath,"SetArray", (GetIndexArgs indexArgs @ [e3]))
                | true,false,SynExpr.DotIndexedGet(_,[SynIndexerArg.Two _],_,_)                                            -> Some (sliceOpPath,"GetStringSlice", GetIndexArgs indexArgs)
                | true,false,SynExpr.DotIndexedGet(_,[SynIndexerArg.One _],_,_)                                            -> Some (indexOpPath,"GetString", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.Two _],_,_)                                            -> Some (sliceOpPath,"GetArraySlice", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.One _;SynIndexerArg.Two _],_,_)                        -> Some (sliceOpPath,"GetArraySlice2DFixed1", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.Two _;SynIndexerArg.One _],_,_)                        -> Some (sliceOpPath,"GetArraySlice2DFixed2", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.Two _;SynIndexerArg.Two _],_,_)                        -> Some (sliceOpPath,"GetArraySlice2D", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.Two _;SynIndexerArg.Two _;SynIndexerArg.Two _],_,_)    -> Some (sliceOpPath,"GetArraySlice3D", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedGet(_,[SynIndexerArg.Two _;SynIndexerArg.Two _;SynIndexerArg.Two _;SynIndexerArg.Two _],_,_)      -> Some (sliceOpPath,"GetArraySlice4D", GetIndexArgs indexArgs)
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.Two _],e3,_,_,_)                                                             -> Some (sliceOpPath,"SetArraySlice", (GetIndexArgs indexArgs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.Two _;SynIndexerArg.Two _],e3,_,_,_)                                         -> Some (sliceOpPath,"SetArraySlice2D", (GetIndexArgs indexArgs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.One _;SynIndexerArg.Two _],e3,_,_,_)                                         -> Some (sliceOpPath,"SetArraySlice2DFixed1", (GetIndexArgs indexArgs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.Two _;SynIndexerArg.One _],e3,_,_,_)                                         -> Some (sliceOpPath,"SetArraySlice2DFixed2", (GetIndexArgs indexArgs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.Two _;SynIndexerArg.Two _;SynIndexerArg.Two _],e3,_,_,_)                     -> Some (sliceOpPath,"SetArraySlice3D", (GetIndexArgs indexArgs @ [e3]))
                | false,true,SynExpr.DotIndexedSet(_,[SynIndexerArg.Two _;SynIndexerArg.Two _;SynIndexerArg.Two _;SynIndexerArg.Two _],e3,_,_,_) -> Some (sliceOpPath,"SetArraySlice4D", (GetIndexArgs indexArgs @ [e3]))
                | _ -> None // error(Error(FSComp.SR.tcInvalidIndexerExpression(),mWholeExpr))
            match info with 
            | None -> None
            | Some (path,functionName,indexArgs) -> 
                let operPath = mkSynLidGet mDot path (CompileOpName functionName)
                let f,fty,tpenv = TcExprOfUnknownType cenv env tpenv operPath
                let domainTy,resultTy = UnifyFunctionType (Some mWholeExpr) cenv env.DisplayEnv mWholeExpr fty
                UnifyTypes cenv env mWholeExpr domainTy e1ty 
                let f' = buildApp cenv (MakeApplicableExprNoFlex cenv f) fty e1' mWholeExpr
                let delayed = List.foldBack (fun idx acc -> DelayedApp(ExprAtomicFlag.Atomic,idx,mWholeExpr) :: acc) indexArgs delayed // atomic, otherwise no ar.[1] <- xyz
                Some (PropagateThenTcDelayed cenv overallTy env tpenv mWholeExpr f' resultTy ExprAtomicFlag.Atomic delayed )
        else None

    match attemptArrayString with 
    | Some res -> res
    | None -> 
    if isNominal || Option.isSome propName then
        let nm = 
            match propName with 
            | None -> "Item"
            | Some nm -> nm
        let delayed = 
            match wholeExpr with 
            // e1.[e2] 
            | SynExpr.DotIndexedGet _ -> 
                DelayedDotLookup([ident(nm,mWholeExpr)],mWholeExpr) :: DelayedApp(ExprAtomicFlag.Atomic,MakeIndexParam None,mWholeExpr) :: delayed
            // e1.[e2] <- e3
            | SynExpr.DotIndexedSet(_,_,e3,mOfLeftOfSet,_,_) -> 
                match indexArgs with 
                | [SynIndexerArg.One(_)] -> DelayedDotLookup([ident(nm,mOfLeftOfSet)],mOfLeftOfSet) :: DelayedApp(ExprAtomicFlag.Atomic,MakeIndexParam None,mOfLeftOfSet) :: MakeDelayedSet(e3,mWholeExpr) :: delayed
                | _ -> DelayedDotLookup([ident("SetSlice",mOfLeftOfSet)],mOfLeftOfSet) :: DelayedApp(ExprAtomicFlag.Atomic,MakeIndexParam (Some e3),mWholeExpr) :: delayed
                
            | _ -> error(InternalError("unreachable",mWholeExpr))
        PropagateThenTcDelayed cenv overallTy env tpenv mDot (MakeApplicableExprNoFlex cenv e1') e1ty ExprAtomicFlag.Atomic delayed 

    else 
        // deprecated constrained lookup 
        error(Error(FSComp.SR.tcObjectOfIndeterminateTypeUsedRequireTypeConstraint(),mWholeExpr))


/// Check a 'new Type(args)' expression, also an 'inheritedTys declaration in an implicit or explicit class 
/// For 'new Type(args)', mWholeExprOrObjTy is the whole expression
/// For 'inherit Type(args)', mWholeExprOrObjTy is the whole expression
/// For an implicit inherit from System.Object or a default constructor, mWholeExprOrObjTy is the type name of the type being defined
and TcNewExpr cenv env tpenv objTy mObjTyOpt superInit arg mWholeExprOrObjTy =
    let ad = env.eAccessRights
    // Handle the case 'new 'a()' 
    if (isTyparTy cenv.g objTy) then 
        if superInit then error(Error(FSComp.SR.tcCannotInheritFromVariableType(),mWholeExprOrObjTy))
        AddCxTypeMustSupportDefaultCtor env.DisplayEnv cenv.css mWholeExprOrObjTy NoTrace objTy
        
        match arg with 
        | SynExpr.Const (SynConst.Unit,_) -> ()
        | _ -> errorR(Error(FSComp.SR.tcObjectConstructorsOnTypeParametersCannotTakeArguments(),mWholeExprOrObjTy))
        
        mkCallCreateInstance cenv.g mWholeExprOrObjTy objTy ,tpenv
    else 
        if not (isAppTy cenv.g objTy) then error(Error(FSComp.SR.tcNamedTypeRequired(if superInit then "inherit" else "new"),mWholeExprOrObjTy))
        let item = ForceRaise (ResolveObjectConstructor cenv.nameResolver env.DisplayEnv mWholeExprOrObjTy ad objTy)
        
        TcCtorCall false cenv env tpenv objTy objTy mObjTyOpt item superInit [arg] mWholeExprOrObjTy [] None

/// Check an 'inheritedTys declaration in an implicit or explicit class 
and TcCtorCall isNaked cenv env tpenv overallTy objTy mObjTyOpt item superInit args mWholeCall delayed afterTcOverloadResolutionOpt =
    let ad = env.eAccessRights
    let isSuperInit = (if superInit then CtorValUsedAsSuperInit else NormalValUse)
    let mItem = match mObjTyOpt with Some m -> m | None -> mWholeCall

    if isInterfaceTy cenv.g objTy then 
        error(Error((if superInit then FSComp.SR.tcInheritCannotBeUsedOnInterfaceType() else FSComp.SR.tcNewCannotBeUsedOnInterfaceType()),mWholeCall))

    match item, args with 
    | Item.CtorGroup(methodName,minfos), _ ->
        let meths = List.map (fun minfo -> minfo,None) minfos
        if isNaked && TypeFeasiblySubsumesType 0 cenv.g cenv.amap mWholeCall cenv.g.system_IDisposable_typ NoCoerce objTy then
            warning(Error(FSComp.SR.tcIDisposableTypeShouldUseNew(),mWholeCall))

        // Check the type is not abstract
        // skip this check if this ctor call is either 'inherit(...)' or call is located within constructor shape
        if not (superInit || AreWithinCtorShape env)
            then CheckSuperInit cenv objTy mWholeCall

        let afterTcOverloadResolution =
            match mObjTyOpt,afterTcOverloadResolutionOpt with
            |   _,Some action -> action
            |   Some mObjTy,None -> AfterTcOverloadResolution.ForNewConstructors cenv.tcSink env mObjTy methodName minfos 
            |   None, _ -> AfterTcOverloadResolution.DoNothing

        TcMethodApplicationThen cenv env overallTy (Some objTy) tpenv None [] mWholeCall mItem methodName ad PossiblyMutates false meths afterTcOverloadResolution isSuperInit args ExprAtomicFlag.NonAtomic delayed 

    | Item.DelegateCtor typ, [arg] ->
        // Re-record the name resolution since we now know it's a constructor call
        match mObjTyOpt with 
        | Some mObjTy -> CallNameResolutionSink cenv.tcSink (mObjTy,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
        | None -> ()
        TcNewDelegateThen cenv objTy env tpenv mItem mWholeCall typ arg ExprAtomicFlag.NonAtomic delayed

    | _ -> 
        error(Error(FSComp.SR.tcSyntaxCanOnlyBeUsedToCreateObjectTypes(if superInit then "inherit" else "new"),mWholeCall))


//-------------------------------------------------------------------------
// TcRecordConstruction
//------------------------------------------------------------------------- 
  
// Check a record construction expression 
and TcRecordConstruction cenv overallTy env tpenv optOrigExpr objTy fldsList m =
    let tcref = tcrefOfAppTy cenv.g objTy
    let tycon = tcref.Deref
    let tinst = argsOfAppTy cenv.g objTy
    UnifyTypes cenv env m overallTy objTy

    // Types with implicit constructors can't use record or object syntax: all constructions must go through the implicit constructor 
    if tycon.MembersOfFSharpTyconByName |> NameMultiMap.existsInRange (fun v -> v.IsIncrClassConstructor) then 
        errorR(Error(FSComp.SR.tcConstructorRequiresCall(tycon.DisplayName),m))
                
    let fspecs = tycon.TrueInstanceFieldsAsList
    // Freshen types and work out their subtype flexibility
    let fldsList = 
        [ for (fname, fexpr) in fldsList do 
              let fspec = 
                  try  
                      fspecs |> List.find (fun fspec -> fspec.Name = fname) 
                  with :? KeyNotFoundException -> 
                      error (Error(FSComp.SR.tcUndefinedField(fname, NicePrint.minimalStringOfType env.DisplayEnv objTy),m))
              let fty = actualTyOfRecdFieldForTycon tycon tinst fspec
              let flex = not (isTyparTy cenv.g fty)
              yield (fname,fexpr,fty,flex) ]

    // Type check and generalize the supplied bindings 
    let fldsList,tpenv = 
        let env = { env with eContextInfo = ContextInfo.RecordFields }
        (tpenv,fldsList) ||> List.mapFold (fun tpenv (fname,fexpr,fty,flex) ->
              let fieldExpr,tpenv = TcExprFlex cenv flex fty env tpenv fexpr
              (fname,fieldExpr),tpenv)
              
    // Add rebindings for unbound field when an "old value" is available 
    // Effect order: mutable fields may get modified by other bindings... 
    let oldFldsList, wrap = 
        match optOrigExpr with
        | None -> [], id
        | Some (_,_,oldve) -> 
            let wrap,oldveaddr = mkExprAddrOfExpr cenv.g tycon.IsStructOrEnumTycon false NeverMutates oldve None m
            let fieldNameUnbound nom = List.forall (fun (name,_) -> name <> nom) fldsList
            let flds = 
                fspecs |> List.choose (fun rfld -> 
                    if fieldNameUnbound rfld.Name && not rfld.IsZeroInit 
                    then Some(rfld.Name, mkRecdFieldGetViaExprAddr (oldveaddr,tcref.MakeNestedRecdFieldRef rfld,tinst,m))
                    else None)
            flds, wrap

    let fldsList = fldsList @ oldFldsList

    // From now on only interested in fspecs that truly need values. 
    let fspecs = fspecs |> List.filter (fun f -> not f.IsZeroInit)
    
    // Check all fields are bound
    fspecs |> List.iter (fun fspec ->
      if not (fldsList |> List.exists (fun (fname,_) -> fname = fspec.Name)) then
        error(Error(FSComp.SR.tcFieldRequiresAssignment(fspec.rfield_id.idText, fullDisplayTextOfTyconRef tcref),m)))

    // Other checks (overlap with above check now clear)
    let ns1 = NameSet.ofList (List.map fst fldsList)
    let ns2 = NameSet.ofList (List.map (fun x -> x.rfield_id.idText) fspecs)
    
    if Option.isNone optOrigExpr && not (Zset.subset ns2 ns1) then
        error (MissingFields(Zset.elements (Zset.diff ns2 ns1),m))
    
    if  not (Zset.subset ns1 ns2) then 
        error (Error(FSComp.SR.tcExtraneousFieldsGivenValues(),m))
    
    // Build record 
    let rfrefs = List.map (fst >> mkRecdFieldRef tcref) fldsList

    // Check accessibility: this is also done in BuildFieldMap, but also need to check 
    // for fields in { new R with a=1 and b=2 } constructions and { r with a=1 }  copy-and-update expressions 
    rfrefs |> List.iter (fun rfref -> 
        CheckRecdFieldAccessible cenv.amap m env.eAccessRights rfref |> ignore
        CheckFSharpAttributes cenv.g rfref.PropertyAttribs m |> CommitOperationResult)        

    let args   = List.map snd fldsList
    
    let expr = wrap (mkRecordExpr cenv.g (GetRecdInfo env, tcref, tinst, rfrefs, args, m))

    let expr = 
      match optOrigExpr with 
      | None ->
          // '{ recd fields }'. //
          expr
          
      | Some (old,oldv,_) -> 
          // '{ recd with fields }'. 
          // Assign the first object to a tmp and then construct 
          mkCompGenLet m oldv old expr

    expr, tpenv

//-------------------------------------------------------------------------
// TcObjectExpr
//------------------------------------------------------------------------- 

and GetNameAndArityOfObjExprBinding _cenv _env b =
    let (NormalizedBinding (_,_,_,_,_,_,_,valSynData,pat,rhsExpr,mBinding,_)) = b
    let (SynValData(memberFlagsOpt,valSynInfo,_)) = valSynData 
    match pat,memberFlagsOpt with 

    // This is the normal case for F# 'with member x.M(...) = ...'
    | SynPat.InstanceMember(_thisId,memberId,_,None,_),Some memberFlags ->
         let logicalMethId = ident (ComputeLogicalName memberId memberFlags,memberId.idRange)
         logicalMethId.idText,valSynInfo

    | _ -> 
        // This is for the deprecated form 'with M(...) = ...'
        let rec lookPat pat =
            match pat with 
            | SynPat.Typed(pat,_,_) -> lookPat pat
            | SynPat.FromParseError(pat,_) -> lookPat pat
            | SynPat.Named (SynPat.Wild _, id,_,None,_) -> 
                let (NormalizedBindingRhs(pushedPats,_,_)) = rhsExpr
                let infosForExplicitArgs = pushedPats |> List.map SynInfo.InferSynArgInfoFromSimplePats
                let infosForExplicitArgs = SynInfo.AdjustMemberArgs MemberKind.Member infosForExplicitArgs
                let infosForExplicitArgs = SynInfo.AdjustArgsForUnitElimination infosForExplicitArgs 
                let argInfos = [SynInfo.selfMetadata] @ infosForExplicitArgs
                let retInfo = SynInfo.unnamedRetVal //SynInfo.InferSynReturnData pushedRetInfoOpt
                let valSynData = SynValInfo(argInfos,retInfo)
                (id.idText,valSynData)
            | _ -> error(Error(FSComp.SR.tcObjectExpressionsCanOnlyOverrideAbstractOrVirtual(),mBinding)) 

        lookPat pat


and FreshenObjExprAbstractSlot cenv (env: TcEnv) (implty:TType) virtNameAndArityPairs (bind,bindAttribs,bindName,absSlots:(_ * MethInfo) list) = 
    let (NormalizedBinding (_,_,_,_,_,_,synTyparDecls,_,_,_,mBinding,_)) = bind
    match absSlots with 
    | [] when not (CompileAsEvent cenv.g bindAttribs) -> 
        let absSlotsByName = List.filter (fst >> fst >> (=) bindName) virtNameAndArityPairs
        let getSignature absSlot = (NicePrint.stringOfMethInfo cenv.amap mBinding env.DisplayEnv absSlot).Replace("abstract ","")
        let getDetails (absSlot:MethInfo) = 
            if absSlot.GetParamTypes(cenv.amap,mBinding,[]) |> List.existsSquared (isAnyTupleTy cenv.g) then
                FSComp.SR.tupleRequiredInAbstractMethod()
            else ""

        // Compute the argument counts of the member arguments
        let _,synValInfo = GetNameAndArityOfObjExprBinding cenv env bind
        let arity = 
            match SynInfo.AritiesOfArgs synValInfo with            
            | _::x::_ -> x
            | _ -> 0
        
        match absSlotsByName with 
        | [] ->
            let tcref = tcrefOfAppTy cenv.g implty
            let containsNonAbstractMemberWithSameName = 
                tcref.MembersOfFSharpTyconByName
                |> Seq.exists (fun kv -> kv.Value |> List.exists (fun valRef -> valRef.DisplayName = bindName))

            let suggestVirtualMembers() =
                virtNameAndArityPairs
                |> List.map (fst >> fst)
                |> Set.ofList

            if containsNonAbstractMemberWithSameName then
                errorR(ErrorWithSuggestions(FSComp.SR.tcMemberFoundIsNotAbstractOrVirtual(tcref.DisplayName, bindName),mBinding,bindName,suggestVirtualMembers))
            else
                errorR(ErrorWithSuggestions(FSComp.SR.tcNoAbstractOrVirtualMemberFound(bindName),mBinding,bindName,suggestVirtualMembers))
        | [(_,absSlot:MethInfo)]     ->
            errorR(Error(FSComp.SR.tcArgumentArityMismatch(bindName, List.sum absSlot.NumArgs, arity, getSignature absSlot, getDetails absSlot),mBinding))
        | (_,absSlot:MethInfo) :: _  ->
            errorR(Error(FSComp.SR.tcArgumentArityMismatchOneOverload(bindName, List.sum absSlot.NumArgs, arity, getSignature absSlot, getDetails absSlot),mBinding))
        
        None
        
    | [(_,absSlot)] -> 
        
        let typarsFromAbsSlotAreRigid,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot
           = FreshenAbstractSlot cenv.g cenv.amap mBinding synTyparDecls absSlot

        // Work out the required type of the member 
        let bindingTy = implty --> (mkMethodTy cenv.g argTysFromAbsSlot retTyFromAbsSlot) 
        
        Some(typarsFromAbsSlotAreRigid,typarsFromAbsSlot,bindingTy)
        
    | _ -> 
        None


and TcObjectExprBinding cenv (env: TcEnv) implty tpenv (absSlotInfo,bind) =
    // 4a1. normalize the binding (note: needlessly repeating what we've done above) 
    let (NormalizedBinding(vis,bkind,isInline,isMutable,attrs,doc,synTyparDecls,valSynData,p,bindingRhs,mBinding,spBind)) = bind
    let (SynValData(memberFlagsOpt,_,_)) = valSynData 
    // 4a2. adjust the binding, especially in the "member" case, a subset of the logic of AnalyzeAndMakeAndPublishRecursiveValue 
    let bindingRhs,logicalMethId,memberFlags = 
        let rec lookPat p = 
            match p,memberFlagsOpt with  
            | SynPat.FromParseError(pat,_),_ -> lookPat pat
            | SynPat.Named (SynPat.Wild _, id,_,_,_),None -> 
                let bindingRhs = PushOnePatternToRhs cenv true (mkSynThisPatVar (ident (CompilerGeneratedName "this",id.idRange))) bindingRhs 
                let logicalMethId = id
                let memberFlags = OverrideMemberFlags MemberKind.Member
                bindingRhs,logicalMethId,memberFlags

            | SynPat.InstanceMember(thisId,memberId,_,_,_),Some memberFlags -> 
                CheckMemberFlags cenv.g None  NewSlotsOK OverridesOK memberFlags mBinding
                let bindingRhs = PushOnePatternToRhs cenv true (mkSynThisPatVar thisId) bindingRhs
                let logicalMethId = ident (ComputeLogicalName memberId memberFlags,memberId.idRange)
                bindingRhs,logicalMethId,memberFlags
            | _ -> 
                error(InternalError("unexpected member binding",mBinding))
        lookPat p
    let bind = NormalizedBinding (vis,bkind,isInline,isMutable,attrs,doc,synTyparDecls,valSynData,mkSynPatVar vis logicalMethId,bindingRhs,mBinding,spBind) 
    
    // 4b. typecheck the binding 
    let bindingTy = 
        match absSlotInfo with
        | Some(_,_,memberTyFromAbsSlot) -> 
            memberTyFromAbsSlot
        | _ -> 
            implty --> NewInferenceType ()

    let (CheckedBindingInfo(inlineFlag,bindingAttribs,_,_,ExplicitTyparInfo(_,declaredTypars,_),nameToPrelimValSchemeMap,rhsExpr,_,_,m,_,_,_,_),tpenv) = 
        let flex, tpenv = TcNonrecBindingTyparDecls cenv env tpenv bind
        TcNormalizedBinding ObjectExpressionOverrideBinding cenv env tpenv bindingTy None NoSafeInitInfo ([],flex) bind

    // 4c. generalize the binding - only relevant when implementing a generic virtual method 
    
    match NameMap.range nameToPrelimValSchemeMap with 
    | [PrelimValScheme1(id,_,_,_,_,_,_,_,_,_,_)] -> 
        let denv = env.DisplayEnv

        let declaredTypars = 
            match absSlotInfo with
            | Some(typarsFromAbsSlotAreRigid,typarsFromAbsSlot,_) -> 
                if typarsFromAbsSlotAreRigid then typarsFromAbsSlot else declaredTypars
            | _ -> 
                declaredTypars
        // Canonicalize constraints prior to generalization 
        GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,m) declaredTypars

        let freeInEnv = GeneralizationHelpers.ComputeUngeneralizableTypars env

        let generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv,m,freeInEnv,false,CanGeneralizeConstrainedTypars,inlineFlag,Some(rhsExpr),declaredTypars,[],bindingTy,false)
        let declaredTypars = ChooseCanonicalDeclaredTyparsAfterInference cenv.g  env.DisplayEnv declaredTypars m

        let generalizedTypars = PlaceTyparsInDeclarationOrder declaredTypars generalizedTypars  

        (id,memberFlags,(generalizedTypars +-> bindingTy),bindingAttribs,rhsExpr),tpenv
    | _ -> 
        error(Error(FSComp.SR.tcSimpleMethodNameRequired(),m))
    
and ComputeObjectExprOverrides cenv (env: TcEnv) tpenv impls =

    // Compute the method sets each implemented type needs to implement
    let slotImplSets = DispatchSlotChecking.GetSlotImplSets cenv.infoReader env.DisplayEnv true (impls |> List.map (fun (m,ty,_) -> ty,m))

    let allImpls = 
        (impls,slotImplSets) ||>  List.map2 (fun (m,ty,binds) implTySet -> 
            let binds = binds |> List.map (BindingNormalization.NormalizeBinding ObjExprBinding cenv env)
            m, ty,binds,implTySet) 

    let overridesAndVirts,tpenv = 
        (tpenv,allImpls) ||>  List.mapFold (fun tpenv (m,implty,binds, SlotImplSet(reqdSlots,dispatchSlotsKeyed,availPriorOverrides,_) ) ->
                
            // Generate extra bindings fo object expressions with bindings using the CLIEvent attribute
            let binds, bindsAttributes = 
               [ for binding in binds do
                     let (NormalizedBinding(_,_,_,_,bindingSynAttribs,_,_,valSynData,_,_,_,_)) = binding
                     let (SynValData(memberFlagsOpt,_,_)) = valSynData 
                     let attrTgt = DeclKind.AllowedAttribTargets memberFlagsOpt ObjectExpressionOverrideBinding
                     let bindingAttribs = TcAttributes cenv env attrTgt bindingSynAttribs
                     yield binding, bindingAttribs
                     for extraBinding in EventDeclarationNormalization.GenerateExtraBindings cenv (bindingAttribs, binding) do
                         yield extraBinding, [] ]
               |> List.unzip
                    
            // 2. collect all name/arity of all overrides 
            let dispatchSlots = reqdSlots |> List.map (fun (RequiredSlot(dispatchSlot,_)) -> dispatchSlot)
            let virtNameAndArityPairs = dispatchSlots |> List.map (fun virt -> 
                let vkey = (virt.LogicalName,virt.NumArgs) 
                //dprintfn "vkey = %A" vkey
                (vkey,virt)) 
            let bindNameAndSynInfoPairs = binds |> List.map (GetNameAndArityOfObjExprBinding cenv env) 
            let bindNames = bindNameAndSynInfoPairs |> List.map fst
            let bindKeys = 
                bindNameAndSynInfoPairs |> List.map (fun (name,valSynData) -> 
                    // Compute the argument counts of the member arguments
                    let argCounts = (SynInfo.AritiesOfArgs valSynData).Tail
                    //dprintfn "name = %A, argCounts = %A" name argCounts
                    (name,argCounts))

            // 3. infer must-have types by name/arity 
            let preAssignedVirtsPerBinding = 
                bindKeys |> List.map (fun bkey  -> List.filter (fst >> (=) bkey) virtNameAndArityPairs) 

            let absSlotInfo = 
               (List.zip4 binds bindsAttributes bindNames preAssignedVirtsPerBinding)  
               |> List.map (FreshenObjExprAbstractSlot cenv env implty virtNameAndArityPairs)

            // 4. typecheck/typeinfer/generalizer overrides using this information 
            let overrides,tpenv = (tpenv,List.zip absSlotInfo binds) ||> List.mapFold (TcObjectExprBinding cenv env implty)

            // Convert the syntactic info to actual info 
            let overrides = 
                (overrides,bindNameAndSynInfoPairs) ||> List.map2 (fun (id:Ident,memberFlags,ty,bindingAttribs,bindingBody) (_,valSynData) -> 
                    let partialValInfo = TranslateTopValSynInfo id.idRange (TcAttributes cenv env) valSynData
                    let tps,_ = tryDestForallTy cenv.g ty
                    let valInfo = TranslatePartialArity tps partialValInfo
                    DispatchSlotChecking.GetObjectExprOverrideInfo cenv.g cenv.amap (implty,id,memberFlags,ty,valInfo,bindingAttribs,bindingBody))

            (m,implty,reqdSlots,dispatchSlotsKeyed,availPriorOverrides,overrides),tpenv)

    overridesAndVirts,tpenv

and CheckSuperType cenv typ m = 
    if typeEquiv cenv.g typ cenv.g.system_Value_typ ||
       typeEquiv cenv.g typ cenv.g.system_Enum_typ ||
       typeEquiv cenv.g typ cenv.g.system_Array_typ ||
       typeEquiv cenv.g typ cenv.g.system_MulticastDelegate_typ ||
       typeEquiv cenv.g typ cenv.g.system_Delegate_typ then 
         error(Error(FSComp.SR.tcPredefinedTypeCannotBeUsedAsSuperType(),m))
    if isErasedType cenv.g typ then
        errorR(Error(FSComp.SR.tcCannotInheritFromErasedType(),m))
       
   
and TcObjectExpr cenv overallTy env tpenv (synObjTy,argopt,binds,extraImpls,mNewExpr,mWholeExpr) = 
    let mObjTy = synObjTy.Range

    let objTy,tpenv = TcType cenv NewTyparsOK  CheckCxs ItemOccurence.UseInType  env tpenv synObjTy
    match tryDestAppTy cenv.g objTy with
    | None -> error(Error(FSComp.SR.tcNewMustBeUsedWithNamedType(),mNewExpr))
    | Some tcref ->
    let isRecordTy = isRecdTy cenv.g objTy
    if not isRecordTy && not (isInterfaceTy cenv.g objTy) && isSealedTy cenv.g objTy then errorR(Error(FSComp.SR.tcCannotCreateExtensionOfSealedType(),mNewExpr))
    
    CheckSuperType cenv objTy synObjTy.Range 

    // Add the object type to the ungeneralizable items 
    let env = {env with eUngeneralizableItems =  addFreeItemOfTy objTy env.eUngeneralizableItems } 
       
    // Object expression members can access protected members of the implemented type 
    let env = EnterFamilyRegion tcref env
    let ad = env.eAccessRights
    
    if // record construction ?
       isRecordTy || 
       // object construction?
       (isFSharpObjModelTy cenv.g objTy && not (isInterfaceTy cenv.g objTy) && Option.isNone argopt) then  

        if Option.isSome argopt then error(Error(FSComp.SR.tcNoArgumentsForRecordValue(),mWholeExpr))
        if not (isNil extraImpls) then error(Error(FSComp.SR.tcNoInterfaceImplementationForConstructionExpression(),mNewExpr))
        if isFSharpObjModelTy cenv.g objTy && GetCtorShapeCounter env <> 1 then 
            error(Error(FSComp.SR.tcObjectConstructionCanOnlyBeUsedInClassTypes(),mNewExpr))
        let fldsList = 
            binds |> List.map (fun b -> 
                match BindingNormalization.NormalizeBinding ObjExprBinding cenv env b with 
                | NormalizedBinding (_,_,_,_,[],_,_,_,SynPat.Named(SynPat.Wild _, id,_,_,_),NormalizedBindingRhs(_,_,rhsExpr),_,_) -> id.idText,rhsExpr
                | _ -> error(Error(FSComp.SR.tcOnlySimpleBindingsCanBeUsedInConstructionExpressions(),b.RangeOfBindingSansRhs)))
        
        TcRecordConstruction cenv overallTy env tpenv None objTy fldsList mWholeExpr
    else
        let item = ForceRaise (ResolveObjectConstructor cenv.nameResolver env.DisplayEnv mObjTy ad objTy)

        if isFSharpObjModelTy cenv.g objTy && GetCtorShapeCounter env = 1 then 
            error(Error(FSComp.SR.tcObjectsMustBeInitializedWithObjectExpression(),mNewExpr))

      // Work out the type of any interfaces to implement 
        let extraImpls,tpenv = 
          (tpenv , extraImpls) ||> List.mapFold (fun tpenv (InterfaceImpl(synIntfTy,overrides,m)) -> 
              let intfTy,tpenv = TcType cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv synIntfTy
              if not (isInterfaceTy cenv.g intfTy) then
                error(Error(FSComp.SR.tcExpectedInterfaceType(),m))
              if isErasedType cenv.g intfTy then
                  errorR(Error(FSComp.SR.tcCannotInheritFromErasedType(),m))
              (m,intfTy,overrides),tpenv)

        let realObjTy = if isObjTy cenv.g objTy && not (isNil extraImpls) then (p23 (List.head extraImpls)) else objTy
        UnifyTypes cenv env mWholeExpr overallTy realObjTy

        let ctorCall,baseIdOpt,tpenv =
            match item,argopt with 
            | Item.CtorGroup(methodName,minfos),Some (arg,baseIdOpt) -> 
                let meths = minfos |> List.map (fun minfo -> minfo,None) 
                let afterTcOverloadResolution = AfterTcOverloadResolution.ForNewConstructors cenv.tcSink env synObjTy.Range methodName minfos
                let ad = env.eAccessRights

                let expr,tpenv = TcMethodApplicationThen cenv env objTy None tpenv None [] mWholeExpr mObjTy methodName ad PossiblyMutates false meths afterTcOverloadResolution CtorValUsedAsSuperInit [arg] ExprAtomicFlag.Atomic [] 
                // The 'base' value is always bound
                let baseIdOpt = (match baseIdOpt with None -> Some(ident("base",mObjTy)) | Some id -> Some(id))
                expr,baseIdOpt,tpenv
            | Item.FakeInterfaceCtor intfTy,None -> 
                UnifyTypes cenv env mWholeExpr objTy intfTy
                let expr = BuildObjCtorCall cenv.g mWholeExpr
                expr,None,tpenv
            | Item.FakeInterfaceCtor _,Some _ -> 
                error(Error(FSComp.SR.tcConstructorForInterfacesDoNotTakeArguments(),mNewExpr))
            | Item.CtorGroup _,None -> 
                error(Error(FSComp.SR.tcConstructorRequiresArguments(),mNewExpr))
            | _ -> error(Error(FSComp.SR.tcNewRequiresObjectConstructor(),mNewExpr))

        let baseValOpt = MakeAndPublishBaseVal cenv env baseIdOpt objTy
        let env = Option.foldBack (AddLocalVal cenv.tcSink mNewExpr) baseValOpt env
        
        
        let impls = (mWholeExpr,objTy,binds) :: extraImpls
        
        
        // 1. collect all the relevant abstract slots for each type we have to implement 
        
        let overridesAndVirts,tpenv = ComputeObjectExprOverrides cenv env tpenv impls

    
        overridesAndVirts |> List.iter (fun (m,implty,dispatchSlots,dispatchSlotsKeyed,availPriorOverrides,overrides) -> 
            let overrideSpecs = overrides |> List.map fst

            DispatchSlotChecking.CheckOverridesAreAllUsedOnce (env.DisplayEnv, cenv.g, cenv.amap, true, implty, dispatchSlotsKeyed, availPriorOverrides, overrideSpecs)

            DispatchSlotChecking.CheckDispatchSlotsAreImplemented (env.DisplayEnv, cenv.g, cenv.amap, m, env.NameEnv, cenv.tcSink, false, implty, dispatchSlots, availPriorOverrides, overrideSpecs) |> ignore)
        
        // 6c. create the specs of overrides 
        let allTypeImpls = 
          overridesAndVirts |> List.map (fun (m,implty,_,dispatchSlotsKeyed,_,overrides) -> 
              let overrides' = 
                  [ for overrideMeth in overrides do 
                        let (Override(_,_, id,(mtps,_),_,_,isFakeEventProperty,_) as ovinfo),(_, thisVal, methodVars, bindingAttribs, bindingBody) = overrideMeth
                        if not isFakeEventProperty then 
                            let searchForOverride = 
                                dispatchSlotsKeyed 
                                |> NameMultiMap.find id.idText 
                                |> List.tryPick (fun (RequiredSlot(virt,_)) -> 
                                     if DispatchSlotChecking.IsExactMatch cenv.g cenv.amap m virt ovinfo then 
                                         Some virt 
                                     else 
                                         None)

                            let overridden = 
                                match searchForOverride with 
                                | Some x -> x
                                | None -> error(Error(FSComp.SR.tcAtLeastOneOverrideIsInvalid(),synObjTy.Range))

                            yield TObjExprMethod(overridden.GetSlotSig(cenv.amap, m), bindingAttribs, mtps, [thisVal]::methodVars, bindingBody, id.idRange) ]
              (implty,overrides'))
            
        let (objTy',overrides') = allTypeImpls.Head
        let extraImpls = allTypeImpls.Tail
        
        // 7. Build the implementation 
        let expr = mkObjExpr(objTy', baseValOpt, ctorCall, overrides',extraImpls,mWholeExpr)
        let expr = mkCoerceIfNeeded cenv.g realObjTy objTy' expr
        expr,tpenv



//-------------------------------------------------------------------------
// TcConstStringExpr
//------------------------------------------------------------------------- 

/// Check a constant string expression. It might be a 'printf' format string 
and TcConstStringExpr cenv overallTy env m tpenv s  =

    if (AddCxTypeEqualsTypeUndoIfFailed env.DisplayEnv cenv.css m overallTy cenv.g.string_ty) then 
      mkString cenv.g m s,tpenv
    else 
      let aty = NewInferenceType ()
      let bty = NewInferenceType ()
      let cty = NewInferenceType ()
      let dty = NewInferenceType ()
      let ety = NewInferenceType ()
      let ty' = mkPrintfFormatTy cenv.g aty bty cty dty ety
      if (not (isObjTy cenv.g overallTy) && AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m overallTy ty') then 
        // Parse the format string to work out the phantom types 
        let source = match cenv.tcSink.CurrentSink with None -> None | Some sink -> sink.CurrentSource
        let normalizedString = (s.Replace("\r\n", "\n").Replace("\r", "\n"))
        
        let (aty',ety'), specifierLocations = (try CheckFormatStrings.ParseFormatString m cenv.g source normalizedString bty cty dty with Failure s -> error (Error(FSComp.SR.tcUnableToParseFormatString(s),m)))

        match cenv.tcSink.CurrentSink with 
        | None -> () 
        | Some sink  -> 
            for specifierLocation in specifierLocations do
                sink.NotifyFormatSpecifierLocation specifierLocation

        UnifyTypes cenv env m aty aty'
        UnifyTypes cenv env m ety ety'
        mkCallNewFormat cenv.g m aty bty cty dty ety (mkString cenv.g m s),tpenv
      else 
        UnifyTypes cenv env m overallTy cenv.g.string_ty
        mkString cenv.g m s,tpenv

//-------------------------------------------------------------------------
// TcConstExpr
//------------------------------------------------------------------------- 

/// Check a constant expression. 
and TcConstExpr cenv overallTy env m tpenv c  =
    match c with 

    // NOTE: these aren't "really" constants 
    | SynConst.Bytes (bytes,m) -> 
       UnifyTypes cenv env m overallTy (mkByteArrayTy cenv.g) 
       Expr.Op(TOp.Bytes bytes,[],[],m),tpenv

    | SynConst.UInt16s arr -> 
       UnifyTypes cenv env m overallTy (mkArrayType cenv.g cenv.g.uint16_ty); Expr.Op(TOp.UInt16s arr,[],[],m),tpenv

    | SynConst.UserNum (s,suffix) -> 
        let expr = 
            let modName = ("NumericLiteral" + suffix)
            let ad = env.eAccessRights
            match ResolveLongIndentAsModuleOrNamespace ResultCollectionSettings.AtMostOneResult cenv.amap m OpenQualified env.eNameResEnv ad [ident (modName,m)] with 
            | Result []
            | Exception _ -> error(Error(FSComp.SR.tcNumericLiteralRequiresModule(modName),m))
            | Result ((_,mref,_) :: _) -> 
                let expr = 
                    try 
                        let i32 = int32 s  
                        if i32 = 0 then SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet m [modName] "FromZero",SynExpr.Const(SynConst.Unit,m),m)
                        elif i32 = 1 then SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet m [modName] "FromOne",SynExpr.Const(SynConst.Unit,m),m)
                        else SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet m [modName] "FromInt32",SynExpr.Const(SynConst.Int32 i32,m),m)
                    with _ -> 
                      try 
                         let i64 = int64 s  
                         SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet m [modName] "FromInt64",SynExpr.Const(SynConst.Int64 i64,m),m)
                      with _ ->             
                        SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet m [modName] "FromString",SynExpr.Const(SynConst.String (s,m),m),m) 
                let ccu = ccuOfTyconRef mref
                if Option.isSome ccu && ccuEq ccu.Value cenv.g.fslibCcu && suffix = "I" then 
                    SynExpr.Typed(expr,SynType.LongIdent(LongIdentWithDots(pathToSynLid m ["System";"Numerics";"BigInteger"],[])),m)
                else
                    expr

        TcExpr cenv overallTy env tpenv expr

    | _ -> 
        let c' = TcConst cenv overallTy m env c
        Expr.Const (c',m,overallTy),tpenv


//-------------------------------------------------------------------------
// TcAssertExpr
//------------------------------------------------------------------------- 

// Check an 'assert(x)' expression. 
and TcAssertExpr cenv overallTy env (m:range) tpenv x  =
    let synm = m.MakeSynthetic() // Mark as synthetic so the language service won't pick it up.
    let callDiagnosticsExpr = SynExpr.App(ExprAtomicFlag.Atomic, false, mkSynLidGet synm ["System";"Diagnostics";"Debug"] "Assert", 
                                           // wrap an extra parentheses so 'assert(x=1) isn't considered a named argument to a method call 
                                           SynExpr.Paren(x,range0,None,synm), synm)

    TcExpr cenv overallTy env tpenv callDiagnosticsExpr



//-------------------------------------------------------------------------
// TcRecdExpr
//------------------------------------------------------------------------- 

and TcRecdExpr cenv overallTy env tpenv (inherits, optOrigExpr, flds, mWholeExpr) =

    let requiresCtor = (GetCtorShapeCounter env = 1) // Get special expression forms for constructors 
    let haveCtor = Option.isSome inherits

    let optOrigExpr,tpenv = 
      match optOrigExpr with 
      | None -> None, tpenv 
      | Some (origExpr, _) -> 
          match inherits with 
          | Some (_,_,mInherits, _, _) -> error(Error(FSComp.SR.tcInvalidRecordConstruction(),mInherits))
          | None -> 
              let olde,tpenv = TcExpr cenv overallTy env tpenv origExpr
              let oldv,oldve = mkCompGenLocal mWholeExpr "inputRecord" overallTy
              Some (olde,oldv,oldve), tpenv

    let fldsList = 
        let flds = 
            [
                // if we met at least one field that is not syntactically correct - raise ReportedError to transfer control to the recovery routine
                for ((lidwd, isOk), v, _) in flds do
                    if not isOk then
                        // raising ReportedError None transfers control to the closest errorRecovery point but do not make any records into log
                        // we assume that parse errors were already reported
                        raise (ReportedError None)

                    yield (List.frontAndBack lidwd.Lid, v)
            ]
        match flds with 
        | [] -> []
        | _ ->
            let tcref,_,fldsList = BuildFieldMap cenv env (Option.isSome optOrigExpr) overallTy flds mWholeExpr
            let _,_,_,gtyp = infoOfTyconRef mWholeExpr tcref
            UnifyTypes cenv env mWholeExpr overallTy gtyp      
            fldsList

    if Option.isSome optOrigExpr && not (isRecdTy cenv.g overallTy) then 
        errorR(Error(FSComp.SR.tcExpressionFormRequiresRecordTypes(),mWholeExpr))

    if requiresCtor || haveCtor then 
        if not (isFSharpObjModelTy cenv.g overallTy) then 
            // Deliberate no-recovery failure here to prevent cascading internal errors
            error(Error(FSComp.SR.tcInheritedTypeIsNotObjectModelType(),mWholeExpr))
        if not requiresCtor then 
            errorR(Error(FSComp.SR.tcObjectConstructionExpressionCanOnlyImplementConstructorsInObjectModelTypes(),mWholeExpr))
    else
        if isNil flds then 
            let errorInfo = 
                if Option.isSome optOrigExpr then FSComp.SR.tcEmptyCopyAndUpdateRecordInvalid()
                else FSComp.SR.tcEmptyRecordInvalid()
            error(Error(errorInfo,mWholeExpr))

        if isFSharpObjModelTy cenv.g overallTy then errorR(Error(FSComp.SR.tcTypeIsNotARecordTypeNeedConstructor(),mWholeExpr))
        elif not (isRecdTy cenv.g overallTy) then errorR(Error(FSComp.SR.tcTypeIsNotARecordType(),mWholeExpr))

    let superTy,tpenv = 
        match inherits, GetSuperTypeOfType cenv.g cenv.amap mWholeExpr overallTy with 
        | Some (superTyp,arg,m, _, _), Some realSuperTyp ->
            // Constructor expression, with an explicit 'inheritedTys clause. Check the inherits clause. 
            let e,tpenv = TcExpr cenv realSuperTyp  env tpenv (SynExpr.New(true,superTyp,arg,m))
            Some e, tpenv
        | None, Some realSuperTyp when requiresCtor -> 
            // Constructor expression, No 'inherited' clause, hence look for a default constructor 
            let e,tpenv = TcNewExpr cenv env tpenv realSuperTyp None true (SynExpr.Const (SynConst.Unit,mWholeExpr)) mWholeExpr
            Some e, tpenv
        | None,_ -> 
            None,tpenv
        | _, None -> 
            errorR(InternalError("Unexpected failure in getting super type",mWholeExpr))
            None,tpenv

    let expr,tpenv = 
        let fldsList = fldsList |> List.choose (fun (n, v) -> if v.IsSome then Some (n, v.Value) else None)
        TcRecordConstruction cenv overallTy env tpenv optOrigExpr  overallTy fldsList mWholeExpr

    let expr = 
        match superTy with 
        | _ when isStructTy cenv.g overallTy -> expr
        | Some e -> mkCompGenSequential mWholeExpr e expr
        | None -> expr
    expr,tpenv


//-------------------------------------------------------------------------
// TcForEachExpr 
//------------------------------------------------------------------------- 
 
and TcForEachExpr cenv overallTy env tpenv (pat,enumSynExpr,bodySynExpr,mWholeExpr,spForLoop)  =
    UnifyTypes cenv env mWholeExpr overallTy cenv.g.unit_ty

    let mPat = pat.Range
    //let mBodyExpr = bodySynExpr.Range
    let mEnumExpr = enumSynExpr.Range
    let mForLoopStart = match spForLoop with SequencePointAtForLoop(mStart) -> mStart | NoSequencePointAtForLoop -> mEnumExpr

    // Check the expression being enumerated
    let enumExpr,enumExprTy,tpenv = TcExprOfUnknownType cenv env tpenv enumSynExpr

    // Depending on its type we compile it in different ways
    let enumElemTy, bodyExprFixup, overallExprFixup, iterationTechnique = 
        match enumExpr with 

        // optimize 'for i in n .. m do' 
        | Expr.App(Expr.Val(vf,_,_),_,[tyarg],[startExpr;finishExpr],_) 
             when valRefEq cenv.g vf cenv.g.range_op_vref && typeEquiv cenv.g tyarg cenv.g.int_ty -> 
               (cenv.g.int32_ty, (fun _ x -> x), id, Choice1Of3 (startExpr,finishExpr))

        // optimize 'for i in arr do' 
        | _ when isArray1DTy cenv.g enumExprTy  -> 
            let arrVar,arrExpr = mkCompGenLocal mEnumExpr "arr" enumExprTy
            let idxVar,idxExpr = mkCompGenLocal mPat "idx" cenv.g.int32_ty
            let elemTy = destArrayTy cenv.g enumExprTy
            
            // Evaluate the array index lookup
            let bodyExprFixup elemVar bodyExpr = mkCompGenLet mForLoopStart elemVar (mkLdelem cenv.g mForLoopStart elemTy arrExpr idxExpr) bodyExpr

            // Evaluate the array expression once and put it in arrVar
            let overallExprFixup overallExpr = mkCompGenLet mForLoopStart arrVar enumExpr overallExpr

            // Ask for a loop over integers for the given range
            (elemTy, bodyExprFixup, overallExprFixup, Choice2Of3 (idxVar,mkZero cenv.g mForLoopStart,mkDecr cenv.g mForLoopStart (mkLdlen cenv.g mForLoopStart arrExpr)))

        | _ -> 

            let enumerableVar,enumerableExprInVar = mkCompGenLocal mEnumExpr "inputSequence" enumExprTy
            let enumeratorVar, enumeratorExpr,_,enumElemTy,getEnumExpr,getEnumTy,guardExpr,_,currentExpr = 
                AnalyzeArbitraryExprAsEnumerable cenv env true mEnumExpr enumExprTy enumerableExprInVar
            (enumElemTy, (fun _ x -> x), id, Choice3Of3(enumerableVar,enumeratorVar, enumeratorExpr,getEnumExpr,getEnumTy,guardExpr,currentExpr))
            
    let pat,_,vspecs,envinner,tpenv = TcMatchPattern cenv enumElemTy env tpenv (pat,None)
    let elemVar,pat =      
        // nice: don't introduce awful temporary for r.h.s. in the 99% case where we know what we're binding it to 
        match pat with
        | TPat_as (pat1,PBind(v,TypeScheme([],_)),_) -> 
              v,pat1
        | _ -> 
              let tmp,_ = mkCompGenLocal pat.Range "forLoopVar" enumElemTy
              tmp,pat

    // Check the body of the loop
    let bodyExpr,tpenv = TcStmt cenv envinner tpenv bodySynExpr

    // Add the pattern match compilation
    let bodyExpr = 
        let valsDefinedByMatching = ListSet.remove valEq elemVar vspecs
        CompilePatternForMatch 
            cenv env enumSynExpr.Range pat.Range false IgnoreWithWarning (elemVar,[]) 
            [TClause(pat,None,TTarget(valsDefinedByMatching,bodyExpr,SequencePointAtTarget),mForLoopStart)] 
            enumElemTy 
            overallTy

    // Apply the fixup to bind the elemVar if needed
    let bodyExpr = bodyExprFixup elemVar bodyExpr
    
    // Build the overall loop
    let overallExpr =  

        match iterationTechnique with 

        // Build iteration as a for loop
        | Choice1Of3(startExpr,finishExpr) -> 
            mkFastForLoop cenv.g (spForLoop,mWholeExpr,elemVar,startExpr,true,finishExpr,bodyExpr)

        // Build iteration as a for loop with a specific index variable that is not the same as the elemVar
        | Choice2Of3(idxVar,startExpr,finishExpr) -> 
            mkFastForLoop cenv.g (spForLoop,mWholeExpr,idxVar,startExpr,true,finishExpr,bodyExpr)

        // Build iteration as a while loop with a try/finally disposal
        | Choice3Of3(enumerableVar,enumeratorVar, _,getEnumExpr,_,guardExpr,currentExpr) -> 

            // This compiled for must be matched EXACTLY by CompiledForEachExpr in opt.fs and creflect.fs
            mkCompGenLet mForLoopStart enumerableVar enumExpr
              (let cleanupE = BuildDisposableCleanup cenv env mWholeExpr enumeratorVar
               let spBind = match spForLoop with SequencePointAtForLoop(spStart) -> SequencePointAtBinding(spStart) | NoSequencePointAtForLoop -> NoSequencePointAtStickyBinding
               (mkLet spBind mForLoopStart enumeratorVar getEnumExpr
                   (mkTryFinally cenv.g 
                       (mkWhile cenv.g 
                           (NoSequencePointAtWhileLoop, 
                            WhileLoopForCompiledForEachExprMarker, guardExpr,
                            mkCompGenLet mForLoopStart elemVar currentExpr bodyExpr,
                            mForLoopStart),
                        cleanupE,mForLoopStart,cenv.g.unit_ty,NoSequencePointAtTry,NoSequencePointAtFinally))))

    let overallExpr = overallExprFixup  overallExpr
    overallExpr, tpenv

//-------------------------------------------------------------------------
// TcQuotationExpr
//------------------------------------------------------------------------- 

and TcQuotationExpr cenv overallTy env tpenv (_oper,raw,ast,isFromQueryExpression,m) =
    let astTy = NewInferenceType ()

    // Assert the overall type for the domain of the quotation template
    UnifyTypes cenv env m overallTy (if raw then mkRawQuotedExprTy cenv.g else mkQuotedExprTy cenv.g astTy) 

    // Check the expression 
    let expr,tpenv = TcExpr cenv astTy env tpenv ast  
    
    // Wrap the expression
    let expr = Expr.Quote(expr, ref None, isFromQueryExpression, m, overallTy)

    // Coerce it if needed
    let expr = if raw then mkCoerceExpr(expr,(mkRawQuotedExprTy cenv.g),m,(tyOfExpr cenv.g expr)) else expr

    // We serialize the quoted expression to bytes in IlxGen after type inference etc. is complete. 
    expr,tpenv

//-------------------------------------------------------------------------
// TcComputationOrSequenceExpression
//------------------------------------------------------------------------- 

and TcComputationOrSequenceExpression cenv (env: TcEnv) overallTy m interpValOpt tpenv comp = 
    match interpValOpt with 
    | Some (interpExpr:Expr,builderTy) -> 
        TcComputationExpression cenv env overallTy m interpExpr builderTy tpenv comp
    | None -> 
        TcSequenceExpression cenv env tpenv comp overallTy m
 
/// Ignores an attribute
and IgnoreAttribute _ = None

// Used for all computation expressions except sequence expressions
and TcComputationExpression cenv env overallTy mWhole interpExpr builderTy tpenv comp = 

    //dprintfn "TcComputationOrSequenceExpression, comp = \n%A\n-------------------\n" comp
    let ad = env.eAccessRights

    let mkSynDelay2 (e: SynExpr) = mkSynDelay (e.Range.MakeSynthetic()) e
    
    let builderValName = CompilerGeneratedName "builder"
    let mBuilderVal = interpExpr.Range
    
    // Give bespoke error messages for the FSharp.Core "query" builder
    let isQuery = 
        match interpExpr with 
        | Expr.Val(vf,_,m) -> 
            let item = Item.CustomBuilder (vf.DisplayName, vf)
            CallNameResolutionSink cenv.tcSink (m,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
            valRefEq cenv.g vf cenv.g.query_value_vref 
        | _ -> false

    /// Make a builder.Method(...) call
    let mkSynCall nm (m:range) args = 
        let m = m.MakeSynthetic() // Mark as synthetic so the language service won't pick it up.
        let args = 
            match args with 
            | [] -> SynExpr.Const(SynConst.Unit,m)
            | [arg] -> SynExpr.Paren(SynExpr.Paren(arg,range0,None,m),range0,None,m)
            | args -> SynExpr.Paren(SynExpr.Tuple(args,[],m),range0,None,m)
                
        let builderVal = mkSynIdGet m builderValName
        mkSynApp1 (SynExpr.DotGet(builderVal,range0,LongIdentWithDots([mkSynId m nm],[]), m)) args m

    let sourceMethInfo = TryFindIntrinsicOrExtensionMethInfo cenv env mBuilderVal ad "Source" builderTy 
    // Optionally wrap sources of "let!", "yield!", "use!" in "query.Source"
    let mkSourceExpr callExpr = 
        match sourceMethInfo with 
        | [] -> callExpr
        | _ -> mkSynCall "Source" callExpr.Range [callExpr]


    /// Decide if the builder is an auto-quote builder
    let isAutoQuote = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env mBuilderVal ad "Quote" builderTy with 
        | [] -> false
        | _ -> true

    let customOperationMethods = 
        AllMethInfosOfTypeInScope cenv.infoReader env.NameEnv (None,ad) IgnoreOverrides mBuilderVal builderTy
        |> List.choose (fun methInfo -> 
                if not (IsMethInfoAccessible cenv.amap mBuilderVal ad methInfo) then None else
                let nameSearch = 
                    TryBindMethInfoAttribute cenv.g mBuilderVal cenv.g.attrib_CustomOperationAttribute methInfo 
                        IgnoreAttribute // We do not respect this attribute for IL methods
                        (function (Attrib(_,_,[ AttribStringArg msg ],_,_,_,_)) -> Some msg | _ -> None)
                        IgnoreAttribute // We do not respect this attribute for provided methods

                match nameSearch with
                | None -> None
                | Some nm ->
                    let joinConditionWord =
                        TryBindMethInfoAttribute cenv.g mBuilderVal cenv.g.attrib_CustomOperationAttribute methInfo 
                            IgnoreAttribute // We do not respect this attribute for IL methods
                            (function (Attrib(_,_,_,ExtractAttribNamedArg "JoinConditionWord" (AttribStringArg s),_,_,_)) -> Some s | _ -> None)
                            IgnoreAttribute // We do not respect this attribute for provided methods

                    let flagSearch (propName:string) = 
                        TryBindMethInfoAttribute cenv.g mBuilderVal cenv.g.attrib_CustomOperationAttribute methInfo 
                            IgnoreAttribute // We do not respect this attribute for IL methods
                            (function (Attrib(_,_,_,ExtractAttribNamedArg propName (AttribBoolArg b),_,_,_)) -> Some b | _ -> None)
                            IgnoreAttribute // We do not respect this attribute for provided methods

                    let maintainsVarSpaceUsingBind = defaultArg (flagSearch "MaintainsVariableSpaceUsingBind") false
                    let maintainsVarSpace = defaultArg (flagSearch "MaintainsVariableSpace") false
                    let allowInto = defaultArg (flagSearch "AllowIntoPattern") false
                    let isLikeZip = defaultArg (flagSearch "IsLikeZip") false
                    let isLikeJoin = defaultArg (flagSearch "IsLikeJoin") false
                    let isLikeGroupJoin = defaultArg (flagSearch "IsLikeGroupJoin") false

                    Some (nm, maintainsVarSpaceUsingBind, maintainsVarSpace, allowInto, isLikeZip, isLikeJoin, isLikeGroupJoin, joinConditionWord, methInfo))

    let customOperationMethodsIndexedByKeyword = 
        customOperationMethods
        |> Seq.groupBy (fun (nm, _, _, _, _, _, _, _, _) -> nm)
        |> Seq.map (fun (nm,g) -> (nm, Seq.toList g))
        |> dict

    // Check for duplicates by method name (keywords and method names must be 1:1)
    let customOperationMethodsIndexedByMethodName = 
        customOperationMethods
        |> Seq.groupBy (fun (_, _, _, _, _, _, _, _, methInfo) -> methInfo.LogicalName)
        |> Seq.map (fun (nm,g) -> (nm, Seq.toList g))
        |> dict

        
    /// Decide if the identifier represents a use of a custom query operator
    let tryGetDataForCustomOperation (nm:Ident) = 
        match customOperationMethodsIndexedByKeyword.TryGetValue nm.idText with 
        | true, [opData] -> 
            let (opName, maintainsVarSpaceUsingBind, maintainsVarSpace, _allowInto, isLikeZip, isLikeJoin, isLikeGroupJoin, _joinConditionWord, methInfo) = opData
            if (maintainsVarSpaceUsingBind && maintainsVarSpace) || (isLikeZip && isLikeJoin) || (isLikeZip && isLikeGroupJoin) || (isLikeJoin && isLikeGroupJoin)  then 
                 errorR(Error(FSComp.SR.tcCustomOperationInvalid opName,nm.idRange))
            match customOperationMethodsIndexedByMethodName.TryGetValue methInfo.LogicalName with 
            | true, [_] -> ()
            | _ -> errorR(Error(FSComp.SR.tcCustomOperationMayNotBeOverloaded nm.idText,nm.idRange))
            Some opData
        | true, opData::_ -> errorR(Error(FSComp.SR.tcCustomOperationMayNotBeOverloaded nm.idText,nm.idRange)); Some opData
        | _ -> None

    /// Decide if the identifier represents a use of a custom query operator
    let hasCustomOperations () = not (isNil customOperationMethods)

    let isCustomOperation  nm = tryGetDataForCustomOperation nm |> Option.isSome

    // Check for the MaintainsVariableSpace on custom operation
    let customOperationMaintainsVarSpace (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> false
        | Some (_nm, _maintainsVarSpaceUsingBind, maintainsVarSpace, _allowInto, _isLikeZip, _isLikeJoin, _isLikeGroupJoin, _joinConditionWord, _methInfo) ->  maintainsVarSpace

    let customOperationMaintainsVarSpaceUsingBind (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> false
        | Some (_nm, maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, _isLikeZip, _isLikeJoin, _isLikeGroupJoin, _joinConditionWord, _methInfo) ->  maintainsVarSpaceUsingBind

    let customOperationIsLikeZip (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> false
        | Some (_nm, _maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, isLikeZip, _isLikeJoin, _isLikeGroupJoin, _joinConditionWord, _methInfo) ->  isLikeZip

    let customOperationIsLikeJoin (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> false
        | Some (_nm, _maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, _isLikeZip, isLikeJoin, _isLikeGroupJoin, _joinConditionWord, _methInfo) ->  isLikeJoin

    let customOperationIsLikeGroupJoin (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> false
        | Some (_nm, _maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, _isLikeZip, _isLikeJoin, isLikeGroupJoin, _joinConditionWord, _methInfo) ->  isLikeGroupJoin 

    let customOperationJoinConditionWord (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | Some (_nm, _maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, _isLikeZip, _isLikeJoin, _isLikeGroupJoin, Some joinConditionWord, _methInfo) ->  joinConditionWord 
        | _ -> "on"  

    let customOperationAllowsInto (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> false
        | Some (_nm, _maintainsVarSpaceUsingBind, _maintainsVarSpace, allowInto, _isLikeZip, _isLikeJoin, _isLikeGroupJoin, _joinConditionWord, _methInfo) ->  allowInto 

    let customOpUsageText nm = 
        match tryGetDataForCustomOperation nm with
        | None -> None 
        | Some (_nm, _maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, isLikeZip, isLikeJoin, isLikeGroupJoin, _joinConditionWord, _methInfo) ->
            if isLikeGroupJoin then
                Some (FSComp.SR.customOperationTextLikeGroupJoin(nm.idText,customOperationJoinConditionWord nm,customOperationJoinConditionWord nm))
            elif isLikeJoin then
                Some (FSComp.SR.customOperationTextLikeJoin(nm.idText,customOperationJoinConditionWord nm,customOperationJoinConditionWord nm))
            elif isLikeZip then
                Some (FSComp.SR.customOperationTextLikeZip(nm.idText))
            else
                None

    /// Inside the 'query { ... }' use a modified name environment that contains fake 'CustomOperation' entries
    /// for all custom operations. This adds them to the completion lists and prevents them being used as values inside
    /// the query.
    let env = 
        env |> ModifyNameResEnv (fun nenv -> (nenv, customOperationMethods) ||> Seq.fold (fun nenv (nm, _, _, _, _, _, _, _, methInfo) -> 
                 AddFakeNameToNameEnv nm nenv (Item.CustomOperation (nm, (fun () -> customOpUsageText (ident (nm,mBuilderVal))), Some methInfo))))

    // Environment is needed for completions
    CallEnvSink cenv.tcSink (comp.Range, env.NameEnv, ad)

    // Check for the [<ProjectionParameter>] attribute on an argument position
    let tryGetArgInfosForCustomOperator (nm:Ident) = 
        match tryGetDataForCustomOperation nm with 
        | None -> None
        | Some (_nm, __maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, _isLikeZip, _isLikeJoin, _isLikeGroupJoin, _joinConditionWord, methInfo) -> 
            match methInfo with 
            | FSMeth(_,_,vref,_) -> 
                match ArgInfosOfMember cenv.g vref with
                | [curriedArgInfo] -> Some curriedArgInfo // one for the actual argument group
                | _ -> None
            | _ -> None

    let expectedArgCountForCustomOperator (nm:Ident) = 
        match tryGetArgInfosForCustomOperator nm with 
        | None -> 0
        | Some argInfos -> max (argInfos.Length - 1) 0  // drop the computation context argument

    // Check for the [<ProjectionParameter>] attribute on an argument position
    let isCustomOperationProjectionParameter i (nm:Ident) = 
        match tryGetArgInfosForCustomOperator nm with
        | None -> false
        | Some argInfos ->
            i < argInfos.Length && 
            let (_,argInfo) = List.item i argInfos
            HasFSharpAttribute cenv.g cenv.g.attrib_ProjectionParameterAttribute argInfo.Attribs


    let (|ForEachThen|_|) e = 
        match e with 
        | SynExpr.ForEach (_spBind, SeqExprOnly false, isFromSource, pat1, expr1, SynExpr.Sequential(_,true,clause,rest,_),_) -> Some (isFromSource,pat1,expr1,clause,rest)
        | _ -> None

    let (|CustomOpId|_|) predicate e = 
        match e with 
        | SingleIdent nm when isCustomOperation nm && predicate nm -> Some nm
        | _ -> None

    // e1 in e2  ('in' is parsed as 'JOIN_IN')
    let (|InExpr|_|) (e:SynExpr) = 
        match e with 
        | SynExpr.JoinIn(e1,_,e2,mApp) -> Some (e1,e2,mApp)
        | _ -> None

    // e1 on e2 (note: 'on' is the 'JoinConditionWord')
    let (|OnExpr|_|) nm (e:SynExpr) = 
        match tryGetDataForCustomOperation nm with 
        | None -> None
        | Some _ -> 
            match e with 
            | SynExpr.App(_,_,SynExpr.App(_,_,e1,SingleIdent opName,_), e2, _) when opName.idText = customOperationJoinConditionWord nm -> 
                let item = Item.CustomOperation (opName.idText, (fun () -> None), None)
                CallNameResolutionSink cenv.tcSink (opName.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
                Some (e1,e2)
            | _ -> None

    // e1 into e2
    let (|IntoSuffix|_|) (e:SynExpr) = 
        match e with 
        | SynExpr.App(_,_,SynExpr.App(_,_,x,SingleIdent nm2,_), ExprAsPat intoPat, _) when nm2.idText = CustomOperations.Into -> 
            Some (x,nm2.idRange,intoPat)
        | _ -> 
            None

    let arbPat (m: range) = mkSynPatVar None (mkSynId (m.MakeSynthetic()) "_missingVar")

    let MatchIntoSuffixOrRecover alreadyGivenError (nm:Ident) (e:SynExpr) = 
        match e with 
        | IntoSuffix (x,intoWordRange,intoPat) -> 
            // record the "into" as a custom operation for colorization
            let item = Item.CustomOperation ("into", (fun () -> None), None)
            CallNameResolutionSink cenv.tcSink (intoWordRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
            (x,intoPat,alreadyGivenError)
        | _ -> 
            if not alreadyGivenError then 
                errorR(Error(FSComp.SR.tcOperatorIncorrectSyntax(nm.idText, Option.get (customOpUsageText nm)),nm.idRange))
            (e,arbPat e.Range,true)

    let MatchOnExprOrRecover alreadyGivenError nm (onExpr:SynExpr) = 
        match onExpr with 
        | OnExpr nm (innerSource, SynExprParen(keySelectors,_,_,_)) -> 
            (innerSource, keySelectors)
        | _ -> 
            if not alreadyGivenError then 
                suppressErrorReporting (fun () -> TcExprOfUnknownType cenv env tpenv onExpr) |> ignore
                errorR(Error(FSComp.SR.tcOperatorIncorrectSyntax(nm.idText, Option.get (customOpUsageText nm)),nm.idRange))
            (arbExpr("_innerSource",onExpr.Range), mkSynBifix onExpr.Range "=" (arbExpr("_keySelectors",onExpr.Range)) (arbExpr("_keySelector2",onExpr.Range)))

    let JoinOrGroupJoinOp detector e = 
        match e with 
        | SynExpr.App(_,_,CustomOpId detector nm,ExprAsPat innerSourcePat,mJoinCore) ->
            Some(nm, innerSourcePat, mJoinCore, false)
        // join with bad pattern (gives error on "join" and continues)
        | SynExpr.App(_,_,CustomOpId detector nm,_innerSourcePatExpr,mJoinCore) ->
            errorR(Error(FSComp.SR.tcBinaryOperatorRequiresVariable(nm.idText, Option.get (customOpUsageText nm)), nm.idRange))
            Some(nm, arbPat mJoinCore, mJoinCore, true)
        // join (without anything after - gives error on "join" and continues)
        | CustomOpId detector nm -> 
            errorR(Error(FSComp.SR.tcBinaryOperatorRequiresVariable(nm.idText, Option.get (customOpUsageText nm)), nm.idRange))
            Some(nm, arbPat e.Range, e.Range, true)
        | _ -> 
            None
            // JoinOrGroupJoinOp customOperationIsLikeJoin

    let (|JoinOp|_|) (e:SynExpr) = JoinOrGroupJoinOp customOperationIsLikeJoin e
    let (|GroupJoinOp|_|) (e:SynExpr) = JoinOrGroupJoinOp customOperationIsLikeGroupJoin e

    let arbKeySelectors m = mkSynBifix m "=" (arbExpr("_keySelectors",m)) (arbExpr("_keySelector2",m))

    let (|JoinExpr|_|) (e:SynExpr) = 
        match e with 
        | InExpr (JoinOp(nm, innerSourcePat, _, alreadyGivenError), onExpr, mJoinCore)  -> 
            let (innerSource, keySelectors) = MatchOnExprOrRecover alreadyGivenError nm onExpr
            Some(nm, innerSourcePat, innerSource, keySelectors, mJoinCore)
        | JoinOp (nm, innerSourcePat, mJoinCore, alreadyGivenError) ->
            if alreadyGivenError then 
                errorR(Error(FSComp.SR.tcOperatorRequiresIn(nm.idText, Option.get (customOpUsageText nm)), nm.idRange))
            Some (nm, innerSourcePat, arbExpr("_innerSource",e.Range), arbKeySelectors e.Range, mJoinCore)
        | _ -> None

    let (|GroupJoinExpr|_|) (e:SynExpr) = 
        match e with 
        | InExpr (GroupJoinOp (nm, innerSourcePat, _, alreadyGivenError), intoExpr, mGroupJoinCore) ->
            let onExpr,intoPat,alreadyGivenError = MatchIntoSuffixOrRecover alreadyGivenError nm intoExpr 
            let innerSource, keySelectors = MatchOnExprOrRecover alreadyGivenError nm onExpr
            Some (nm, innerSourcePat, innerSource, keySelectors, intoPat, mGroupJoinCore)
        | GroupJoinOp (nm, innerSourcePat, mGroupJoinCore, alreadyGivenError) ->
            if alreadyGivenError then 
               errorR(Error(FSComp.SR.tcOperatorRequiresIn(nm.idText, Option.get (customOpUsageText nm)),nm.idRange))
            Some (nm, innerSourcePat, arbExpr("_innerSource",e.Range), arbKeySelectors e.Range, arbPat e.Range, mGroupJoinCore)
        | _ -> 
            None


    let (|JoinOrGroupJoinOrZipClause|_|) (e:SynExpr) = 
        match e with 

        // join innerSourcePat in innerSource on (keySelector1 = keySelector2)
        | JoinExpr (nm, innerSourcePat, innerSource, keySelectors, mJoinCore) -> 
                Some(nm, innerSourcePat, innerSource, Some keySelectors, None, mJoinCore)

        // groupJoin innerSourcePat in innerSource on (keySelector1 = keySelector2) into intoPat
        | GroupJoinExpr (nm, innerSourcePat, innerSource, keySelectors, intoPat, mGroupJoinCore) -> 
                Some(nm, innerSourcePat, innerSource, Some keySelectors, Some intoPat, mGroupJoinCore)

        // zip intoPat in secondSource 
        | InExpr (SynExpr.App(_,_,CustomOpId customOperationIsLikeZip nm,ExprAsPat secondSourcePat,_),secondSource,mZipCore) -> 
                Some(nm, secondSourcePat, secondSource, None, None, mZipCore)

        // zip (without secondSource or in - gives error)
        | CustomOpId customOperationIsLikeZip nm  -> 
                errorR(Error(FSComp.SR.tcOperatorIncorrectSyntax(nm.idText, Option.get (customOpUsageText nm)),nm.idRange))
                Some(nm, arbPat e.Range, arbExpr("_secondSource",e.Range), None, None, e.Range)

        // zip secondSource (without in - gives error)
        | SynExpr.App(_,_,CustomOpId customOperationIsLikeZip nm,ExprAsPat secondSourcePat,mZipCore) -> 
                errorR(Error(FSComp.SR.tcOperatorIncorrectSyntax(nm.idText, Option.get (customOpUsageText nm)),mZipCore))
                Some(nm, secondSourcePat, arbExpr("_innerSource",e.Range), None, None, mZipCore)

        | _ -> 
            None

    let (|ForEachThenJoinOrGroupJoinOrZipClause|_|) e = 
        match e with 
        | ForEachThen (isFromSource, firstSourcePat, firstSource, JoinOrGroupJoinOrZipClause(nm, secondSourcePat, secondSource, keySelectorsOpt, pat3opt, mOpCore), innerComp) 
            when 
               (let _firstSourceSimplePats,later1 = 
                    use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                    SimplePatsOfPat cenv.synArgNameGenerator firstSourcePat 
                Option.isNone later1)

             -> Some (isFromSource, firstSourcePat, firstSource, nm, secondSourcePat, secondSource, keySelectorsOpt, pat3opt, mOpCore, innerComp)

        | JoinOrGroupJoinOrZipClause(nm, pat2, expr2, expr3, pat3opt, mOpCore) -> 
            errorR(Error(FSComp.SR.tcBinaryOperatorRequiresBody(nm.idText, Option.get (customOpUsageText nm)),nm.idRange))
            Some (true, arbPat e.Range, arbExpr("_outerSource",e.Range), nm, pat2, expr2, expr3, pat3opt, mOpCore, arbExpr("_innerComp",e.Range))

        | _ -> 
            None


    let (|StripApps|) e = 
        let rec strip e = 
            match e with 
            | SynExpr.FromParseError(SynExpr.App(_,_,f,arg,_),_)
            | SynExpr.App(_,_,f,arg,_)  -> 
                let g,acc = strip f 
                g,(arg::acc) 
            | _ -> e,[]
        let g,acc = strip e
        g,List.rev acc

    let (|OptionalIntoSuffix|) e = 
        match e with 
        | IntoSuffix (body,intoWordRange,optInfo) -> (body,Some (intoWordRange, optInfo))
        | body -> (body,None)

    let (|CustomOperationClause|_|) e = 
        match e with 
        | OptionalIntoSuffix(StripApps(SingleIdent nm, _) as core,optInto) when isCustomOperation  nm ->  
            // Now we know we have a custom operation, commit the name resolution
            let optIntoInfo = 
                match optInto with 
                | Some (intoWordRange,optInfo) -> 
                    let item = Item.CustomOperation ("into", (fun () -> None), None)
                    CallNameResolutionSink cenv.tcSink (intoWordRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
                    Some optInfo
                | None -> None

            Some (nm, Option.get (tryGetDataForCustomOperation nm), core, core.Range, optIntoInfo)
        | _ -> None

    let mkSynLambda p e m = SynExpr.Lambda(false,false,p,e,m)

    let mkExprForVarSpace m (patvs: Val list) = 
        match patvs with 
        | [] -> SynExpr.Const(SynConst.Unit,m)
        | [v] -> SynExpr.Ident v.Id
        | vs -> SynExpr.Tuple((vs |> List.map (fun v -> SynExpr.Ident v.Id)), [], m)  

    let mkSimplePatForVarSpace m (patvs: Val list) = 
        let spats = 
            match patvs with 
            | [] -> []
            | [v] -> [mkSynSimplePatVar false v.Id]
            | vs -> vs |> List.map (fun v -> mkSynSimplePatVar false v.Id)
        SynSimplePats.SimplePats (spats, m)

    let mkPatForVarSpace m (patvs: Val list) = 
        match patvs with 
        | [] -> SynPat.Const (SynConst.Unit, m)
        | [v] -> mkSynPatVar None v.Id
        | vs -> SynPat.Tuple((vs |> List.map (fun x -> mkSynPatVar None x.Id)), m)

    let (|OptionalSequential|) e = 
        match e with 
        | SynExpr.Sequential(_sp, true,  dataComp1, dataComp2,_) -> (dataComp1, Some dataComp2)
        | _ -> (e, None)

    // Check for 'where x > y', 'select x,y' and other mis-applications of infix operators, give a good error message, and retun a flag
    let checkForBinaryApp comp = 
        match comp with 
        | StripApps(SingleIdent nm, [StripApps(SingleIdent nm2, args); arg2]) when 
                  PrettyNaming.IsInfixOperator nm.idText && 
                  expectedArgCountForCustomOperator nm2 > 0 &&
                  args.Length > 0 -> 
            let estimatedRangeOfIntendedLeftAndRightArguments = unionRanges (List.last args).Range arg2.Range
            errorR(Error(FSComp.SR.tcUnrecognizedQueryBinaryOperator(),estimatedRangeOfIntendedLeftAndRightArguments))
            true
        | SynExpr.Tuple( (StripApps(SingleIdent nm2, args) :: _), _, m) when 
                  expectedArgCountForCustomOperator nm2 > 0 &&
                  args.Length > 0 -> 
            let estimatedRangeOfIntendedLeftAndRightArguments = unionRanges (List.last args).Range m.EndRange
            errorR(Error(FSComp.SR.tcUnrecognizedQueryBinaryOperator(),estimatedRangeOfIntendedLeftAndRightArguments))
            true
        | _ ->  
            false
                    
    let addVarsToVarSpace (varSpace: LazyWithContext<Val list * TcEnv, range>) f = 
        LazyWithContext.Create
            ((fun m ->
                  let (patvs: Val list, env) = varSpace.Force m 
                  let vs, envinner = f m env 
                  let patvs = List.append patvs (vs |> List.filter (fun v -> not (patvs |> List.exists (fun v2 -> v.LogicalName = v2.LogicalName))))
                  patvs, envinner), 
              id)

    let emptyVarSpace = LazyWithContext.NotLazy ([], env)

    // q              - a flag indicating if custom operators are allowed. They are not allowed inside try/with, try/finally, if/then/else etc.
    // varSpace       - a lazy data structure indicating the variables bound so far in the overall computation
    // comp           - the computation expression being analyzed
    // translatedCtxt - represents the translation of the context in which the computation expression 'comp' occurs, up to a
    //                  hole to be filled by (part of) the results of translating 'comp'.
    let rec tryTrans firstTry q varSpace comp translatedCtxt =

        match comp with 

        // for firstSourcePat in firstSource do 
        // join secondSourcePat in expr2 on (expr3 = expr4)
        // ...
        //    --> 
        // join expr1 expr2 (fun firstSourcePat -> expr3) (fun secondSourcePat -> expr4) (fun firstSourcePat secondSourcePat -> ...)

        // for firstSourcePat in firstSource do 
        // groupJoin secondSourcePat in expr2 on (expr3 = expr4) into groupPat
        // ...
        //    --> 
        // groupJoin expr1 expr2 (fun firstSourcePat -> expr3) (fun secondSourcePat -> expr4) (fun firstSourcePat groupPat -> ...)

        // for firstSourcePat in firstSource do 
        // zip secondSource into secondSourcePat
        // ...
        //    --> 
        // zip expr1 expr2 (fun pat1 pat3 -> ...)
        | ForEachThenJoinOrGroupJoinOrZipClause (isFromSource, firstSourcePat, firstSource, nm, secondSourcePat, secondSource, keySelectorsOpt, secondResultPatOpt, mOpCore, innerComp)  -> 


            if not q then error(Error(FSComp.SR.tcCustomOperationMayNotBeUsedHere(),nm.idRange))
            let firstSource = if isFromSource then mkSourceExpr firstSource else firstSource
            let secondSource = mkSourceExpr secondSource

            // Add the variables to the variable space, on demand
            let varSpaceWithFirstVars = 
                addVarsToVarSpace varSpace (fun _mCustomOp env -> 
                        use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                        let _,_,vspecs,envinner,_ = TcMatchPattern cenv (NewInferenceType()) env tpenv (firstSourcePat, None)
                        vspecs, envinner)

            let varSpaceWithSecondVars = 
                addVarsToVarSpace varSpaceWithFirstVars (fun _mCustomOp env -> 
                        use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                        let _,_,vspecs,envinner,_ = TcMatchPattern cenv (NewInferenceType()) env tpenv (secondSourcePat, None)
                        vspecs, envinner)

            let varSpaceWithGroupJoinVars = 
                match secondResultPatOpt with 
                | Some pat3 -> 
                    addVarsToVarSpace varSpaceWithFirstVars (fun _mCustomOp env -> 
                        use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                        let _,_,vspecs,envinner,_ = TcMatchPattern cenv (NewInferenceType()) env tpenv (pat3, None)
                        vspecs, envinner)
                | None -> varSpace

            let firstSourceSimplePats,later1 = SimplePatsOfPat cenv.synArgNameGenerator firstSourcePat 
            let secondSourceSimplePats,later2 = SimplePatsOfPat cenv.synArgNameGenerator secondSourcePat

            if Option.isSome later1 then errorR (Error (FSComp.SR.tcJoinMustUseSimplePattern(nm.idText), firstSourcePat.Range))
            if Option.isSome later2 then errorR (Error (FSComp.SR.tcJoinMustUseSimplePattern(nm.idText), secondSourcePat.Range))

              // check 'join' or 'groupJoin' or 'zip' is permitted for this builder
            match tryGetDataForCustomOperation nm with 
            | None -> error(Error(FSComp.SR.tcMissingCustomOperation(nm.idText),nm.idRange))
            | Some (opName, _, _, _, _, _, _, _, methInfo) -> 

            // Record the resolution of the custom operation for posterity
            let item = Item.CustomOperation (opName, (fun () -> customOpUsageText nm), Some methInfo)
            CallNameResolutionSink cenv.tcSink (nm.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)

            let mkJoinExpr keySelector1 keySelector2 innerPat e = 
                let mSynthetic = mOpCore.MakeSynthetic()
                mkSynCall methInfo.DisplayName mOpCore
                        [ firstSource
                          secondSource
                          (mkSynLambda firstSourceSimplePats keySelector1 mSynthetic)
                          (mkSynLambda secondSourceSimplePats keySelector2 mSynthetic)
                          (mkSynLambda firstSourceSimplePats (mkSynLambda innerPat e mSynthetic) mSynthetic)  ]

            let mkZipExpr e = 
                let mSynthetic = mOpCore.MakeSynthetic()
                mkSynCall methInfo.DisplayName mOpCore
                        [ firstSource
                          secondSource
                          (mkSynLambda firstSourceSimplePats (mkSynLambda secondSourceSimplePats e mSynthetic) mSynthetic)  ]
            
            // wraps given expression into sequence with result produced by arbExpr so result will look like: 
            // l; SynExpr.ArbitraryAfterError(...)
            // this allows to handle cases like 'on (a > b)' // '>' is not permitted as correct join relation
            // after wrapping a and b can still be typechecked (so we'll have correct completion inside 'on' part)
            // but presence of SynExpr.ArbitraryAfterError allows to avoid errors about incompatible types in cases like
            // query { 
            //      for a in [1] do
            //      join b in [""] on (a > b)
            //      }
            // if we typecheck raw 'a' and 'b' then we'll end up with 2 errors:
            // 1. incorrect join relation
            // 2. incompatible types: int and string
            // with SynExpr.ArbitraryAfterError we have only first one
            let wrapInArbErrSequence l caption = 
                SynExpr.Sequential(SequencePointInfoForSeq.SequencePointsAtSeq, true, l, (arbExpr(caption,l.Range.EndRange)), l.Range)

            let mkOverallExprGivenVarSpaceExpr, varSpaceInner =
                let isNullableOp opId =
                    match DecompileOpName opId with "?=" | "=?" | "?=?" -> true | _ -> false
                match secondResultPatOpt, keySelectorsOpt with 
                // groupJoin 
                | Some secondResultPat, Some relExpr when customOperationIsLikeGroupJoin nm -> 
                    let secondResultSimplePats,later3 = SimplePatsOfPat cenv.synArgNameGenerator secondResultPat
                    if Option.isSome later3 then errorR (Error (FSComp.SR.tcJoinMustUseSimplePattern(nm.idText), secondResultPat.Range))
                    match relExpr with 
                    | JoinRelation cenv env (keySelector1, keySelector2) -> 
                        mkJoinExpr keySelector1 keySelector2 secondResultSimplePats, varSpaceWithGroupJoinVars
                    | BinOpExpr (opId, l, r) ->
                        if isNullableOp opId.idText then 
                            // When we cannot resolve NullableOps, recommend the relevant namespace to be added
                            errorR(Error(FSComp.SR.cannotResolveNullableOperators(DecompileOpName opId.idText),relExpr.Range))
                        else
                            errorR(Error(FSComp.SR.tcInvalidRelationInJoin(nm.idText),relExpr.Range))
                        let l = wrapInArbErrSequence l "_keySelector1"
                        let r = wrapInArbErrSequence r "_keySelector2"
                        // this is not correct JoinRelation but it is still binary operation
                        // we've already reported error now we can use operands of binary operation as join components
                        mkJoinExpr l r secondResultSimplePats, varSpaceWithGroupJoinVars
                    | _ ->
                        errorR(Error(FSComp.SR.tcInvalidRelationInJoin(nm.idText),relExpr.Range))
                        // since the shape of relExpr doesn't match our expectations (JoinRelation) 
                        // then we assume that this is l.h.s. of the join relation 
                        // so typechecker will treat relExpr as body of outerKeySelector lambda parameter in GroupJoin method
                        mkJoinExpr relExpr (arbExpr("_keySelector2",relExpr.Range)) secondResultSimplePats, varSpaceWithGroupJoinVars
                        
                | None, Some relExpr when customOperationIsLikeJoin nm -> 
                    match relExpr with 
                    | JoinRelation cenv env (keySelector1, keySelector2) -> 
                        mkJoinExpr keySelector1 keySelector2 secondSourceSimplePats, varSpaceWithSecondVars
                    | BinOpExpr (opId, l, r) ->
                        if isNullableOp opId.idText then
                            // When we cannot resolve NullableOps, recommend the relevant namespace to be added
                            errorR(Error(FSComp.SR.cannotResolveNullableOperators(DecompileOpName opId.idText),relExpr.Range))
                        else
                            errorR(Error(FSComp.SR.tcInvalidRelationInJoin(nm.idText),relExpr.Range))
                        // this is not correct JoinRelation but it is still binary operation
                        // we've already reported error now we can use operands of binary operation as join components
                        let l = wrapInArbErrSequence l "_keySelector1"
                        let r = wrapInArbErrSequence r "_keySelector2"
                        mkJoinExpr l r secondSourceSimplePats, varSpaceWithGroupJoinVars
                    | _ -> 
                        errorR(Error(FSComp.SR.tcInvalidRelationInJoin(nm.idText),relExpr.Range))
                        // since the shape of relExpr doesn't match our expectations (JoinRelation) 
                        // then we assume that this is l.h.s. of the join relation 
                        // so typechecker will treat relExpr as body of outerKeySelector lambda parameter in Join method
                        mkJoinExpr relExpr (arbExpr("_keySelector2",relExpr.Range)) secondSourceSimplePats, varSpaceWithGroupJoinVars

                | None, None when customOperationIsLikeZip nm -> 
                    mkZipExpr, varSpaceWithSecondVars

                | _ -> 
                    assert false
                    failwith "unreachable"


            // Case from C# spec: A query expression with a join clause with an into followed by something other than a select clause
            // Case from C# spec: A query expression with a join clause without an into followed by something other than a select clause
            let valsInner,_env = varSpaceInner.Force mOpCore
            let varSpaceExpr =  mkExprForVarSpace mOpCore valsInner
            let varSpacePat = mkPatForVarSpace mOpCore valsInner
            let joinExpr = mkOverallExprGivenVarSpaceExpr varSpaceExpr
            Some (trans true q varSpaceInner (SynExpr.ForEach (NoSequencePointAtForLoop, SeqExprOnly false, false, varSpacePat, joinExpr, innerComp, mOpCore)) translatedCtxt)


        | SynExpr.ForEach (spForLoop, SeqExprOnly _seqExprOnly, isFromSource, pat, sourceExpr, innerComp,_) -> 
            let wrappedSourceExpr = if isFromSource then mkSourceExpr sourceExpr else sourceExpr
            let mFor = match spForLoop with SequencePointAtForLoop(m) -> m | _ -> pat.Range
            let mPat = pat.Range
            let spBind = match spForLoop with SequencePointAtForLoop(m) -> SequencePointAtBinding(m) | NoSequencePointAtForLoop -> NoSequencePointAtStickyBinding
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mFor ad "For" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("For"),mFor))

            // Add the variables to the query variable space, on demand
            let varSpace = 
                addVarsToVarSpace varSpace (fun _mCustomOp env -> 
                    use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                    let _,_,vspecs,envinner,_ = TcMatchPattern cenv (NewInferenceType()) env tpenv (pat,None) 
                    vspecs, envinner)

            Some (trans true q varSpace innerComp (fun holeFill -> translatedCtxt (mkSynCall "For" mFor [wrappedSourceExpr; SynExpr.MatchLambda(false,sourceExpr.Range,[Clause(pat,None, holeFill,mPat,SequencePointAtTarget)],spBind,mFor) ])) )

        | SynExpr.For (spBind,id,start,dir,finish,innerComp,m) ->
            let mFor = match spBind with SequencePointAtForLoop m -> m | _ -> m
            if isQuery then errorR(Error(FSComp.SR.tcNoIntegerForLoopInQuery(),mFor))
            Some (trans true q varSpace (elimFastIntegerForLoop (spBind,id,start,dir,finish,innerComp,m)) translatedCtxt )

        | SynExpr.While (spWhile,guardExpr,innerComp,_) -> 
            let mGuard = guardExpr.Range
            let mWhile = match spWhile with SequencePointAtWhileLoop(m) -> m | _ -> mGuard
            if isQuery then error(Error(FSComp.SR.tcNoWhileInQuery(),mWhile))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mWhile ad "While" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("While"),mWhile))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mWhile ad "Delay" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),mWhile))
            Some(trans true q varSpace innerComp (fun holeFill -> translatedCtxt (mkSynCall "While" mWhile [mkSynDelay2 guardExpr; mkSynCall "Delay" mWhile [mkSynDelay innerComp.Range holeFill]])) )

        | SynExpr.TryFinally (innerComp,unwindExpr,mTryToLast,spTry,_spFinally) ->

            let mTry = match spTry with SequencePointAtTry(m) -> m | _ -> mTryToLast
            if isQuery then error(Error(FSComp.SR.tcNoTryFinallyInQuery(),mTry))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mTry ad "TryFinally" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("TryFinally"),mTry))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mTry ad "Delay" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),mTry))
            Some (translatedCtxt (mkSynCall "TryFinally" mTry [mkSynCall "Delay" mTry [mkSynDelay innerComp.Range (transNoQueryOps innerComp)]; mkSynDelay2 unwindExpr]))

        | SynExpr.Paren (_,_,_,m) -> 
            error(Error(FSComp.SR.tcConstructIsAmbiguousInComputationExpression(),m))

        | SynExpr.ImplicitZero m -> 
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Zero" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Zero"),m))
            Some (translatedCtxt (mkSynCall "Zero" m []))
            
        | OptionalSequential (JoinOrGroupJoinOrZipClause (_, _, _, _, _, mClause), _) 
                      when firstTry -> 

            // 'join' clauses preceded by 'let' and other constructs get processed by repackaging with a 'for' loop.
            let patvs,_env = varSpace.Force comp.Range
            let varSpaceExpr =  mkExprForVarSpace mClause patvs
            let varSpacePat = mkPatForVarSpace mClause patvs
            
            let dataCompPrior = 
                translatedCtxt (transNoQueryOps (SynExpr.YieldOrReturn((true,false), varSpaceExpr, mClause)))

            // Rebind using for ... 
            let rebind = 
                SynExpr.ForEach (NoSequencePointAtForLoop, SeqExprOnly false, false, varSpacePat, dataCompPrior, comp, comp.Range)
                    
            // Retry with the 'for' loop pacakging. Set firstTry=false just in case 'join' processing fails
            tryTrans false q varSpace rebind id


        | OptionalSequential (CustomOperationClause (nm, _, opExpr, mClause, _), _) -> 

            if not q then error(Error(FSComp.SR.tcCustomOperationMayNotBeUsedHere(),opExpr.Range))

            let patvs,_env = varSpace.Force comp.Range
            let varSpaceExpr =  mkExprForVarSpace mClause patvs
            
            let dataCompPriorToOp = 
                let isYield = not (customOperationMaintainsVarSpaceUsingBind nm)
                translatedCtxt (transNoQueryOps (SynExpr.YieldOrReturn((isYield,false), varSpaceExpr, mClause)))
            
            let rec consumeClauses (varSpace:LazyWithContext<_,_>) dataCompPrior compClausesExpr lastUsesBind =

                // Substitute 'yield <var-space>' into the context

                let patvs,_env = varSpace.Force comp.Range
                let varSpaceSimplePat = mkSimplePatForVarSpace mClause patvs
                let varSpacePat = mkPatForVarSpace mClause patvs

                match compClausesExpr with 
                
                // Detect one custom operation... This clause will always match at least once...
                | OptionalSequential (CustomOperationClause (nm, (opName, _maintainsVarSpaceUsingBind, _maintainsVarSpace, _allowInto, isLikeZip, isLikeJoin, isLikeGroupJoin, _, methInfo), opExpr, mClause, optionalIntoPat), optionalCont) ->

                    // Record the resolution of the custom operation for posterity
                    let item = Item.CustomOperation (opName, (fun () -> customOpUsageText nm), Some methInfo)
                    CallNameResolutionSink cenv.tcSink (nm.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)

                    if isLikeZip || isLikeJoin || isLikeGroupJoin then
                        errorR(Error(FSComp.SR.tcBinaryOperatorRequiresBody(nm.idText, Option.get (customOpUsageText nm)),nm.idRange))
                        match optionalCont with 
                        | None -> 
                            // we are about to drop the 'opExpr' AST on the floor.  we've already reported an error.  attempt to get name resolutions before dropping it
                            RecordNameAndTypeResolutions_IdeallyWithoutHavingOtherEffects cenv env tpenv opExpr
                            dataCompPrior
                        | Some contExpr -> consumeClauses varSpace dataCompPrior contExpr lastUsesBind 
                    else

                        let maintainsVarSpace = customOperationMaintainsVarSpace nm
                        let maintainsVarSpaceUsingBind = customOperationMaintainsVarSpaceUsingBind nm

                        let expectedArgCount = expectedArgCountForCustomOperator nm 

                        let dataCompAfterOp = 
                            match opExpr with 
                            | StripApps(SingleIdent nm, args) -> 
                                if args.Length = expectedArgCount then 
                                    // Check for the [<ProjectionParameter>] attribute on each argument position
                                    let args = args |> List.mapi (fun i arg -> if isCustomOperationProjectionParameter (i+1) nm then SynExpr.Lambda (false, false, varSpaceSimplePat, arg, arg.Range.MakeSynthetic()) else arg)
                                    mkSynCall methInfo.DisplayName mClause (dataCompPrior :: args)
                                else 
                                    errorR(Error(FSComp.SR.tcCustomOperationHasIncorrectArgCount(nm.idText,expectedArgCount,args.Length),nm.idRange))
                                    mkSynCall methInfo.DisplayName mClause ([ dataCompPrior ] @ List.init expectedArgCount (fun i -> arbExpr("_arg" + string i, mClause))) 
                            | _ -> failwith "unreachable"

                        match optionalCont with 
                        | None -> 
                            match optionalIntoPat  with 
                            | Some intoPat -> errorR(Error(FSComp.SR.tcIntoNeedsRestOfQuery(),intoPat.Range))
                            | None -> ()
                            dataCompAfterOp

                        | Some contExpr -> 

                                // select a.Name into name; ...
                                // distinct into d; ...
                                //
                                // Rebind the into pattern and process the rest of the clauses
                                match optionalIntoPat with 
                                | Some intoPat -> 
                                    if not (customOperationAllowsInto nm) then 
                                        error(Error(FSComp.SR.tcOperatorDoesntAcceptInto(nm.idText),intoPat.Range))

                                    // Rebind using either for ... or let!....
                                    let rebind = 
                                        if maintainsVarSpaceUsingBind then 
                                            SynExpr.LetOrUseBang(NoSequencePointAtLetBinding,false,false,intoPat,dataCompAfterOp,contExpr,intoPat.Range) 
                                        else 
                                            SynExpr.ForEach (NoSequencePointAtForLoop, SeqExprOnly false, false, intoPat, dataCompAfterOp, contExpr, intoPat.Range)

                                    trans true q emptyVarSpace rebind id

                                // select a.Name; ...
                                // distinct; ...
                                //
                                // Process the rest of the clauses
                                | None -> 
                                    if maintainsVarSpace || maintainsVarSpaceUsingBind then
                                        consumeClauses varSpace dataCompAfterOp contExpr maintainsVarSpaceUsingBind
                                    else
                                        consumeClauses emptyVarSpace dataCompAfterOp contExpr false 

                // No more custom operator clauses in compClausesExpr, but there may be clauses like join, yield etc. 
                // Bind/iterate the dataCompPrior and use compClausesExpr as the body.
                | _ -> 
                    // Rebind using either for ... or let!....
                    let rebind = 
                        if lastUsesBind then 
                            SynExpr.LetOrUseBang(NoSequencePointAtLetBinding, false, false, varSpacePat, dataCompPrior, compClausesExpr, compClausesExpr.Range) 
                        else 
                            SynExpr.ForEach (NoSequencePointAtForLoop, SeqExprOnly false, false, varSpacePat, dataCompPrior, compClausesExpr, compClausesExpr.Range)
                    
                    trans true q varSpace rebind id

            // Now run the consumeClauses
            Some (consumeClauses varSpace dataCompPriorToOp comp false)

        | SynExpr.Sequential(sp,true,innerComp1,innerComp2,m) -> 

            // Check for 'where x > y' and other mis-applications of infix operators. If detected, give a good error message, and just ignore innerComp1
          if isQuery && checkForBinaryApp innerComp1 then 
            Some (trans true q varSpace innerComp2 translatedCtxt) 

          else
            
            if isQuery && not(innerComp1.IsArbExprAndThusAlreadyReportedError) then 
                match innerComp1 with 
                | SynExpr.JoinIn _ ->  () // an error will be reported later when we process innerComp1 as a sequential
                | _ -> errorR(Error(FSComp.SR.tcUnrecognizedQueryOperator(),innerComp1.RangeOfFirstPortion))

            match tryTrans true false varSpace innerComp1 id with 
            | Some c -> 
                // "cexpr; cexpr" is treated as builder.Combine(cexpr1,cexpr1)
                // This is not pretty - we have to decide which range markers we use for the calls to Combine and Delay
                // NOTE: we should probably suppress these sequence points altogether
                let m1 = 
                    match innerComp1 with 
                    | SynExpr.IfThenElse (_,_,_,_,_,mIfToThen,_m) -> mIfToThen
                    | SynExpr.Match (SequencePointAtBinding mMatch,_,_,_,_) -> mMatch
                    | SynExpr.TryWith (_,_,_,_,_,SequencePointAtTry mTry,_) -> mTry
                    | SynExpr.TryFinally (_,_,_,SequencePointAtTry mTry,_)  -> mTry
                    | SynExpr.For (SequencePointAtForLoop mBind,_,_,_,_,_,_) -> mBind
                    | SynExpr.ForEach (SequencePointAtForLoop mBind,_,_,_,_,_,_) -> mBind
                    | SynExpr.While (SequencePointAtWhileLoop mWhile,_,_,_) -> mWhile
                    | _ -> innerComp1.Range
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Combine" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Combine"),m))
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Delay" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),m))
                Some (translatedCtxt (mkSynCall "Combine" m1 [c; mkSynCall "Delay" m1 [mkSynDelay innerComp2.Range (transNoQueryOps innerComp2)]]))
            | None -> 
                // "do! expr; cexpr" is treated as { let! () = expr in cexpr }
                match innerComp1 with 
                | SynExpr.DoBang(rhsExpr,m) -> 
                    let sp = 
                        match sp with 
                        | SuppressSequencePointOnStmtOfSequential -> SequencePointAtBinding m
                        | SuppressSequencePointOnExprOfSequential -> NoSequencePointAtDoBinding 
                        | SequencePointsAtSeq -> SequencePointAtBinding m
                    Some(trans true q varSpace (SynExpr.LetOrUseBang(sp, false, true, SynPat.Const(SynConst.Unit, rhsExpr.Range), rhsExpr, innerComp2, m)) translatedCtxt)
                // "expr; cexpr" is treated as sequential execution
                | _ -> 
                    Some (trans true q varSpace innerComp2 (fun holeFill -> translatedCtxt (SynExpr.Sequential(sp,true, innerComp1, holeFill, m))))

        | SynExpr.IfThenElse (guardExpr,thenComp,elseCompOpt,spIfToThen,isRecovery,mIfToThen,mIfToEndOfElseBranch) ->
            match elseCompOpt with 
            | Some elseComp -> 
                if isQuery then error(Error(FSComp.SR.tcIfThenElseMayNotBeUsedWithinQueries(),mIfToThen))
                Some (translatedCtxt (SynExpr.IfThenElse(guardExpr, transNoQueryOps thenComp, Some(transNoQueryOps elseComp), spIfToThen,isRecovery,mIfToThen,mIfToEndOfElseBranch)))
            | None -> 
                let elseComp = 
                    if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mIfToThen ad "Zero" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Zero"),mIfToThen))
                    mkSynCall "Zero" mIfToThen []
                Some (trans true q varSpace thenComp (fun holeFill -> translatedCtxt (SynExpr.IfThenElse(guardExpr, holeFill, Some elseComp, spIfToThen,isRecovery,mIfToThen,mIfToEndOfElseBranch))))

        // 'let binds in expr'
        | SynExpr.LetOrUse (isRec,false,binds,innerComp,m) ->

            // For 'query' check immediately
            if isQuery then
                match (List.map (BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env) binds) with 
                | [NormalizedBinding(_,NormalBinding,(*inline*)false,(*mutable*)false,_,_,_,_,_,_,_,_)] when not isRec -> 
                    ()
                | normalizedBindings -> 
                    let failAt m = error(Error(FSComp.SR.tcNonSimpleLetBindingInQuery(),m))
                    match normalizedBindings with 
                    | NormalizedBinding(_,_,_,_,_,_,_,_,_,_,mBinding,_) :: _ -> failAt mBinding 
                    | _ -> failAt m

            // Add the variables to the query variable space, on demand
            let varSpace = 
                addVarsToVarSpace varSpace (fun mQueryOp env -> 
                    // Normalize the bindings before detecting the bound variables
                    match (List.map (BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env) binds) with 
                    | [NormalizedBinding(_vis,NormalBinding,false,false,_,_,_,_,pat,_,_,_)] -> 
                        // successful case
                        use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                        let _,_,vspecs,envinner,_ = TcMatchPattern cenv (NewInferenceType()) env tpenv (pat,None) 
                        vspecs, envinner
                    | _ -> 
                        // error case
                        error(Error(FSComp.SR.tcCustomOperationMayNotBeUsedInConjunctionWithNonSimpleLetBindings(),mQueryOp)))


            Some (trans true q varSpace innerComp (fun holeFill -> translatedCtxt (SynExpr.LetOrUse (isRec,false,binds,holeFill,m))))

        // 'use x = expr in expr'
        | SynExpr.LetOrUse (_,true,[Binding (_,NormalBinding,_,_,_,_,_,pat,_,rhsExpr,_,spBind)],innerComp,_) ->
            let bindRange = match spBind with SequencePointAtBinding m -> m | _ -> rhsExpr.Range
            if isQuery then error(Error(FSComp.SR.tcUseMayNotBeUsedInQueries(),bindRange))
            let innerCompRange = innerComp.Range
            let consumeExpr = SynExpr.MatchLambda(false,innerCompRange,[Clause(pat,None, transNoQueryOps innerComp,innerCompRange,SequencePointAtTarget)],spBind,innerCompRange)
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Using" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Using"),bindRange))
            Some (translatedCtxt (mkSynCall "Using" bindRange [rhsExpr; consumeExpr ]))

        // 'let! pat = expr in expr' --> build.Bind(e1,(function  _argN -> match _argN with pat -> expr))
        | SynExpr.LetOrUseBang(spBind, false, isFromSource, pat, rhsExpr, innerComp,_) -> 

            let bindRange = match spBind with SequencePointAtBinding(m) -> m | _ -> rhsExpr.Range
            if isQuery then error(Error(FSComp.SR.tcBindMayNotBeUsedInQueries(),bindRange))
            let innerRange = innerComp.Range
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Bind" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Bind"),bindRange))
                
            // Add the variables to the query variable space, on demand
            let varSpace = 
                addVarsToVarSpace varSpace (fun _mCustomOp env -> 
                        use _holder = TemporarilySuspendReportingTypecheckResultsToSink cenv.tcSink
                        let _,_,vspecs,envinner,_ = TcMatchPattern cenv (NewInferenceType()) env tpenv (pat,None) 
                        vspecs, envinner)

            let rhsExpr = if isFromSource then mkSourceExpr rhsExpr else rhsExpr
            Some (trans true q varSpace innerComp (fun holeFill -> 
                        let consumeExpr = SynExpr.MatchLambda(false,pat.Range,[Clause(pat,None, holeFill,innerRange,SequencePointAtTarget)],spBind,innerRange)
                        translatedCtxt (mkSynCall "Bind"  bindRange [rhsExpr; consumeExpr])))

        // 'use! pat = e1 in e2' --> build.Bind(e1,(function  _argN -> match _argN with pat -> build.Using(x,(fun _argN -> match _argN with pat -> e2))))
        | SynExpr.LetOrUseBang(spBind, true, isFromSource, (SynPat.Named (SynPat.Wild _, id, false, _, _) as pat) ,rhsExpr,innerComp,_)
        | SynExpr.LetOrUseBang(spBind, true, isFromSource, (SynPat.LongIdent (LongIdentWithDots([id],_),_,_,_,_,_) as pat), rhsExpr, innerComp,_) ->

            let bindRange = match spBind with SequencePointAtBinding(m) -> m | _ -> rhsExpr.Range
            if isQuery then error(Error(FSComp.SR.tcBindMayNotBeUsedInQueries(),bindRange))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Using" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Using"),bindRange))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Bind" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Bind"),bindRange))
            let consumeExpr = SynExpr.MatchLambda(false,bindRange,[Clause(pat,None, transNoQueryOps innerComp, innerComp.Range, SequencePointAtTarget)],spBind,bindRange)
            let consumeExpr = mkSynCall "Using" bindRange [SynExpr.Ident(id); consumeExpr ]
            let consumeExpr = SynExpr.MatchLambda(false,bindRange,[Clause(pat,None, consumeExpr,id.idRange,SequencePointAtTarget)],spBind,bindRange)
            let rhsExpr = if isFromSource then mkSourceExpr rhsExpr else rhsExpr
            Some(translatedCtxt (mkSynCall "Bind" bindRange [rhsExpr; consumeExpr]))

        // 'use! pat = e1 in e2' where 'pat' is not a simple name --> error
        | SynExpr.LetOrUseBang(_spBind, true, _isFromSource, pat, _rhsExpr, _innerComp,_) -> 
            error(Error(FSComp.SR.tcInvalidUseBangBinding(),pat.Range))

        | SynExpr.Match (spMatch,expr,clauses,false,m) ->
            let mMatch = match spMatch with SequencePointAtBinding mMatch -> mMatch | _ -> m
            if isQuery then error(Error(FSComp.SR.tcMatchMayNotBeUsedWithQuery(),mMatch))
            let clauses = clauses |> List.map (fun (Clause(pat,cond,innerComp,patm,sp)) -> Clause(pat,cond,transNoQueryOps innerComp,patm,sp))
            Some(translatedCtxt (SynExpr.Match(spMatch,expr, clauses, false,m)))

        | SynExpr.TryWith (innerComp,_mTryToWith,clauses,_mWithToLast,mTryToLast,spTry,_spWith) ->
            let mTry = match spTry with SequencePointAtTry(m) -> m | _ -> mTryToLast
            
            if isQuery then error(Error(FSComp.SR.tcTryWithMayNotBeUsedInQueries(),mTry))
            let clauses = clauses |> List.map (fun (Clause(pat,cond,clauseComp,patm,sp)) -> Clause(pat,cond,transNoQueryOps clauseComp,patm,sp))
            let consumeExpr = SynExpr.MatchLambda(true,mTryToLast,clauses,NoSequencePointAtStickyBinding,mTryToLast)
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env  mTry ad "TryWith" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("TryWith"),mTry))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env mTry ad "Delay" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),mTry))
            Some(translatedCtxt (mkSynCall "TryWith" mTry [mkSynCall "Delay" mTry [mkSynDelay2 (transNoQueryOps innerComp)]; consumeExpr]))

        | SynExpr.YieldOrReturnFrom((isYield,_),yieldExpr,m) -> 
            let yieldExpr = mkSourceExpr yieldExpr
            if isYield then 
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "YieldFrom" builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("YieldFrom"),m))
                Some (translatedCtxt (mkSynCall "YieldFrom" m [yieldExpr]))
  
            else
                if isQuery then error(Error(FSComp.SR.tcReturnMayNotBeUsedInQueries(),m))
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "ReturnFrom" builderTy) then 
                    errorR(Error(FSComp.SR.tcRequireBuilderMethod("ReturnFrom"),m))
                    Some (translatedCtxt yieldExpr)
                else
                    Some (translatedCtxt (mkSynCall "ReturnFrom" m [yieldExpr]))
                

        | SynExpr.YieldOrReturn((isYield,_),yieldExpr,m) -> 
            let methName = (if isYield then "Yield" else "Return")
            if isQuery && not isYield then error(Error(FSComp.SR.tcReturnMayNotBeUsedInQueries(),m))
            if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad methName builderTy) then error(Error(FSComp.SR.tcRequireBuilderMethod(methName),m))
            Some(translatedCtxt (mkSynCall methName m [yieldExpr]))

        | _ -> None

    and transNoQueryOps comp = trans true false emptyVarSpace comp id
    and trans firstTry q varSpace comp translatedCtxt = 
        match tryTrans firstTry q varSpace comp translatedCtxt with 
        | Some e -> e
        | None -> 
            // This only occurs in final position in a sequence
            match comp with 
            // "do! expr;" in final position is treated as { let! () = expr in return () } when Return is provided or as { let! () = expr in zero } otherwise
            | SynExpr.DoBang(rhsExpr,m) -> 
                let mUnit = rhsExpr.Range
                let rhsExpr = mkSourceExpr rhsExpr
                if isQuery then error(Error(FSComp.SR.tcBindMayNotBeUsedInQueries(),m))
                let bodyExpr =
                    if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Return" builderTy) then
                        SynExpr.ImplicitZero m
                    else
                        SynExpr.YieldOrReturn((false,true), SynExpr.Const(SynConst.Unit, m), m)
                trans true q varSpace (SynExpr.LetOrUseBang(NoSequencePointAtDoBinding, false, false, SynPat.Const(SynConst.Unit, mUnit), rhsExpr, bodyExpr, m)) translatedCtxt
            // "expr;" in final position is treated as { expr; zero }
            // Suppress the sequence point on the "zero"
            | _ -> 
                // Check for 'where x > y' and other mis-applications of infix operators. If detected, give a good error message, and just ignore comp
                if isQuery && checkForBinaryApp comp then 
                    trans true q varSpace (SynExpr.ImplicitZero comp.Range) translatedCtxt  
                else
                    if isQuery && not comp.IsArbExprAndThusAlreadyReportedError then 
                        match comp with 
                        | SynExpr.JoinIn _ ->  () // an error will be reported later when we process innerComp1 as a sequential
                        | _ -> errorR(Error(FSComp.SR.tcUnrecognizedQueryOperator(),comp.RangeOfFirstPortion))
                    trans true q varSpace (SynExpr.ImplicitZero comp.Range) (fun holeFill -> translatedCtxt (SynExpr.Sequential(SuppressSequencePointOnStmtOfSequential,true, comp, holeFill,comp.Range))) 

    let basicSynExpr = trans true (hasCustomOperations ()) (LazyWithContext.NotLazy ([],env)) comp (fun holeFill -> holeFill) 

    let delayedExpr = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env mBuilderVal ad "Delay" builderTy with 
        | [] -> basicSynExpr
        | _ -> mkSynCall "Delay" mBuilderVal [(mkSynDelay2 basicSynExpr)]

            
    let quotedSynExpr = 
        if isAutoQuote then 
            SynExpr.Quote(mkSynIdGet (mBuilderVal.MakeSynthetic()) (CompileOpName "<@ @>"), (*isRaw=*)false, delayedExpr, (*isFromQueryExpression=*)true, mWhole) 
        else delayedExpr

            
    let runExpr = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env mBuilderVal ad "Run" builderTy with 
        | [] -> quotedSynExpr
        | _ -> mkSynCall "Run" mBuilderVal [quotedSynExpr]

    let lambdaExpr = 
        let mBuilderVal = mBuilderVal.MakeSynthetic()
        SynExpr.Lambda (false,false,SynSimplePats.SimplePats ([mkSynSimplePatVar false (mkSynId mBuilderVal builderValName)],mBuilderVal), runExpr, mBuilderVal)

    let env =
        match comp with     
        | SynExpr.YieldOrReturn ((true,_),_,_) -> { env with eContextInfo = ContextInfo.YieldInComputationExpression }
        | SynExpr.YieldOrReturn ((_,true),_,_) -> { env with eContextInfo = ContextInfo.ReturnInComputationExpression }
        | _  -> env

    let lambdaExpr ,tpenv= TcExpr cenv (builderTy --> overallTy) env tpenv lambdaExpr
    // beta-var-reduce to bind the builder using a 'let' binding
    let coreExpr = mkApps cenv.g ((lambdaExpr,tyOfExpr cenv.g lambdaExpr),[],[interpExpr],mBuilderVal)

    coreExpr,tpenv


/// This case is used for computation expressions which are sequence expressions. Technically the code path is different because it
/// typechecks rather than doing a shallow syntactic translation, and generates calls into the Seq.* library
/// and helpers rather than to the builder methods (there is actually no builder for 'seq' in the library). 
/// These are later detected by state machine compilation. 
///
/// Also "ienumerable extraction" is performed on arguments to "for".
and TcSequenceExpression cenv env tpenv comp overallTy m = 

        let mkDelayedExpr (coreExpr:Expr) = 
            let m = coreExpr.Range
            let overallTy = tyOfExpr cenv.g coreExpr
            mkSeqDelay cenv env m overallTy coreExpr

        let rec tryTcSequenceExprBody env genOuterTy tpenv comp =
            match comp with 
            | SynExpr.ForEach (_spBind, SeqExprOnly _seqExprOnly, _isFromSource, pat, pseudoEnumExpr, innerComp, m) -> 
                // This expression is not checked with the knowledge it is an IEnumerable, since we permit other enumerable types with GetEnumerator/MoveNext methods, as does C# 
                let pseudoEnumExpr,arb_ty,tpenv = TcExprOfUnknownType cenv env tpenv pseudoEnumExpr
                let enumExpr,enumElemTy = ConvertArbitraryExprToEnumerable cenv arb_ty env pseudoEnumExpr
                let pat',_,vspecs,envinner,tpenv = TcMatchPattern cenv enumElemTy env tpenv (pat,None)
                let innerExpr,tpenv = tcSequenceExprBody envinner genOuterTy tpenv innerComp
                
                match pat', vspecs, innerExpr with 
                // peephole optimization: "for x in e1 -> e2" == "e1 |> List.map (fun x -> e2)" *)
                | (TPat_as (TPat_wild _,PBind (v,_),_), 
                   vs,  
                   Expr.App(Expr.Val(vf,_,_),_,[genEnumElemTy],[yexpr],_)) 
                      when vs.Length = 1 && valRefEq cenv.g vf cenv.g.seq_singleton_vref ->
          
                    let enumExprMark = enumExpr.Range
                    let lam = mkLambda enumExprMark v (yexpr,genEnumElemTy)
                    
                    // SEQUENCE POINTS: need to build a let here consuming spBind
                    let enumExpr = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g enumElemTy) (tyOfExpr cenv.g enumExpr) enumExpr
                    Some(mkCallSeqMap cenv.g m enumElemTy genEnumElemTy lam enumExpr,tpenv)

                | _ -> 
                    let enumExprMark = enumExpr.Range

                    // SEQUENCE POINTS: need to build a let here consuming spBind

                    let matchv,matchExpr = compileSeqExprMatchClauses cenv env enumExprMark (pat',vspecs) innerExpr enumElemTy genOuterTy
                    let lam = mkLambda enumExprMark matchv (matchExpr,tyOfExpr cenv.g matchExpr)
                    Some(mkSeqCollect cenv env m enumElemTy genOuterTy lam enumExpr , tpenv)

            | SynExpr.For (spBind,id,start,dir,finish,innerComp,m) ->
                Some(tcSequenceExprBody env genOuterTy tpenv (elimFastIntegerForLoop (spBind,id,start,dir,finish,innerComp,m)))

            | SynExpr.While (_spWhile,guardExpr,innerComp,_m) -> 
                let guardExpr,tpenv = TcExpr cenv cenv.g.bool_ty env tpenv guardExpr
                let innerExpr,tpenv = tcSequenceExprBody env genOuterTy tpenv innerComp
    
                let guardExprMark = guardExpr.Range
                let guardExpr = mkUnitDelayLambda cenv.g guardExprMark guardExpr
                let innerExpr = mkDelayedExpr innerExpr
                Some(mkSeqFromFunctions cenv env guardExprMark genOuterTy guardExpr innerExpr, tpenv)

            | SynExpr.TryFinally (innerComp,unwindExpr,_mTryToLast,_spTry,_spFinally) ->
                let innerExpr,tpenv = tcSequenceExprBody env genOuterTy tpenv innerComp
                let unwindExpr,tpenv = TcExpr cenv cenv.g.unit_ty env tpenv unwindExpr
            
                let unwindExprMark = unwindExpr.Range
                let unwindExpr = mkUnitDelayLambda cenv.g unwindExprMark unwindExpr
                let innerExpr = mkDelayedExpr innerExpr
                let innerExprMark = innerExpr.Range
                
                Some(mkSeqFinally cenv env innerExprMark genOuterTy innerExpr unwindExpr, tpenv)
            | SynExpr.Paren (_,_,_,m) -> 
                error(Error(FSComp.SR.tcConstructIsAmbiguousInSequenceExpression(),m))

            | SynExpr.ImplicitZero m -> 
                Some(mkSeqEmpty cenv env m genOuterTy,tpenv )

            | SynExpr.DoBang(_rhsExpr,m) -> 
                error(Error(FSComp.SR.tcDoBangIllegalInSequenceExpression(),m))

            | SynExpr.Sequential(sp,true,innerComp1, innerComp2,m) -> 
                // "expr; cexpr" is treated as sequential execution
                // "cexpr; cexpr" is treated as append
                match tryTcSequenceExprBody env genOuterTy tpenv innerComp1 with 
                | None -> 
                    let innerExpr1,tpenv = TcStmtThatCantBeCtorBody cenv env tpenv innerComp1
                    let innerExpr2,tpenv = tcSequenceExprBody env genOuterTy tpenv innerComp2

                    Some(Expr.Sequential(innerExpr1,innerExpr2,NormalSeq,sp,m),tpenv)

                | Some (innerExpr1,tpenv) ->
                    let innerExpr2,tpenv = tcSequenceExprBody env genOuterTy tpenv innerComp2
                    let innerExpr2 = mkDelayedExpr innerExpr2
                    Some(mkSeqAppend cenv env innerComp1.Range genOuterTy innerExpr1 innerExpr2, tpenv)

            | SynExpr.IfThenElse (guardExpr,thenComp,elseCompOpt,spIfToThen,_isRecovery,mIfToThen,mIfToEndOfElseBranch) ->
                let guardExpr',tpenv = TcExpr cenv cenv.g.bool_ty env tpenv guardExpr
                let thenExpr,tpenv = tcSequenceExprBody env genOuterTy tpenv thenComp
                let elseComp = (match elseCompOpt with Some c -> c | None -> SynExpr.ImplicitZero mIfToThen)
                let elseExpr,tpenv = tcSequenceExprBody env genOuterTy tpenv elseComp
                Some(mkCond spIfToThen SequencePointAtTarget mIfToEndOfElseBranch genOuterTy guardExpr' thenExpr elseExpr, tpenv)

            // 'let x = expr in expr'
            | SynExpr.LetOrUse (_,false (* not a 'use' binding *),_,_,_) ->
                TcLinearExprs 
                    (fun ty envinner tpenv e -> tcSequenceExprBody envinner ty tpenv e) 
                    cenv env overallTy 
                    tpenv 
                    true
                    comp 
                    (fun x -> x)  |> Some

            // 'use x = expr in expr'
            | SynExpr.LetOrUse (_isRec,true,[Binding (_vis,NormalBinding,_,_,_,_,_,pat,_,rhsExpr,_,_spBind)],innerComp,wholeExprMark) ->

                let bindPatTy = NewInferenceType ()
                let inputExprTy = NewInferenceType ()
                let pat',_,vspecs,envinner,tpenv = TcMatchPattern cenv bindPatTy env tpenv (pat,None)
                UnifyTypes cenv env m inputExprTy bindPatTy
                let inputExpr,tpenv = TcExpr cenv inputExprTy env tpenv rhsExpr
                let innerExpr,tpenv = tcSequenceExprBody envinner genOuterTy tpenv innerComp
                let inputExprMark = inputExpr.Range
                let matchv,matchExpr = compileSeqExprMatchClauses cenv env inputExprMark (pat',vspecs) innerExpr bindPatTy genOuterTy 
                let consumeExpr = mkLambda wholeExprMark matchv (matchExpr,genOuterTy)
                //SEQPOINT NEEDED - we must consume spBind on this path
                Some(mkSeqUsing cenv env wholeExprMark bindPatTy genOuterTy inputExpr consumeExpr, tpenv)

            | SynExpr.LetOrUseBang(_,_,_,_,_,_,m) -> 
                error(Error(FSComp.SR.tcUseForInSequenceExpression(),m))

            | SynExpr.Match (spMatch,expr,clauses,false,_) ->
                let inputExpr,matchty,tpenv = TcExprOfUnknownType cenv env tpenv expr
                let tclauses,tpenv = 
                    List.mapFold 
                        (fun tpenv (Clause(pat,cond,innerComp,_,sp)) ->
                              let pat',cond',vspecs,envinner,tpenv = TcMatchPattern cenv matchty env tpenv (pat,cond)
                              let innerExpr,tpenv = tcSequenceExprBody envinner genOuterTy tpenv innerComp
                              TClause(pat',cond',TTarget(vspecs, innerExpr,sp),pat'.Range),tpenv)
                        tpenv
                        clauses
                let inputExprTy = tyOfExpr cenv.g inputExpr
                let inputExprMark = inputExpr.Range
                let matchv,matchExpr = CompilePatternForMatchClauses cenv env inputExprMark inputExprMark true ThrowIncompleteMatchException inputExprTy genOuterTy tclauses 
                Some(mkLet spMatch inputExprMark matchv inputExpr matchExpr, tpenv)

            | SynExpr.TryWith (_,mTryToWith,_,_,_,_,_) ->
                error(Error(FSComp.SR.tcTryIllegalInSequenceExpression(),mTryToWith))

            | SynExpr.YieldOrReturnFrom((isYield,_),yieldExpr,m) -> 
                let resultExpr,genExprTy,tpenv = TcExprOfUnknownType cenv env tpenv yieldExpr

                if not isYield then errorR(Error(FSComp.SR.tcUseYieldBangForMultipleResults(),m)) 

                AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m  NoTrace genOuterTy genExprTy
                Some(mkCoerceExpr(resultExpr,genOuterTy,m,genExprTy), tpenv)

            | SynExpr.YieldOrReturn((isYield,_),yieldExpr,m) -> 
                let genResultTy = NewInferenceType ()
                if not isYield then errorR(Error(FSComp.SR.tcSeqResultsUseYield(),m)) 
                UnifyTypes cenv env m genOuterTy (mkSeqTy cenv.g genResultTy)

                let resultExpr,tpenv = TcExpr cenv genResultTy env tpenv yieldExpr
                Some(mkCallSeqSingleton cenv.g m genResultTy resultExpr, tpenv )

            | _ -> None
                
        and tcSequenceExprBody env genOuterTy tpenv comp =
            match tryTcSequenceExprBody env genOuterTy tpenv comp with 
            | Some e -> e
            | None -> 
                // seq { ...; expr } is treated as 'seq { ... ; expr; yield! Seq.empty }'
                // Note this means seq { ...; () } is treated as 'seq { ... ; (); yield! Seq.empty }'
                let m = comp.Range
                let expr,tpenv = TcStmtThatCantBeCtorBody cenv env tpenv comp
                Expr.Sequential(expr,mkSeqEmpty cenv env m genOuterTy,NormalSeq,SuppressSequencePointOnStmtOfSequential,m),tpenv

        let genEnumElemTy = NewInferenceType ()
        UnifyTypes cenv env m overallTy (mkSeqTy cenv.g genEnumElemTy)

        let coreExpr,tpenv = tcSequenceExprBody env overallTy tpenv comp
        let delayedExpr = mkDelayedExpr coreExpr
        delayedExpr,tpenv

//-------------------------------------------------------------------------
// Typecheck "expr ... " constructs where "..." is a sequence of applications,
// type applications and dot-notation projections. First extract known
// type information from the "..." part to use during type checking.
//
// 'overallTy' is the type expected for the entire chain of expr + lookups.
// 'exprty' is the type of the expression on the left of the lookup chain.
//
// Unsophisticated applications can propagate information from the expected overall type 'overallTy' 
// through to the leading function type 'exprty'. This is because the application 
// unambiguously implies a function type 
//------------------------------------------------------------------------- 

and PropagateThenTcDelayed cenv overallTy env tpenv mExpr expr exprty (atomicFlag:ExprAtomicFlag) delayed = 
    
    let rec propagate delayedList mExpr exprty = 
        match delayedList with 
        | [] -> 
            // Avoid unifying twice: we're about to unify in TcDelayed 
            if not (isNil delayed) then 
                UnifyTypesAndRecover cenv env mExpr overallTy exprty
        | DelayedDot :: _
        | DelayedSet _ :: _
        | DelayedDotLookup _ :: _ -> ()
        | DelayedTypeApp (_, _mTypeArgs, mExprAndTypeArgs) :: delayedList' ->
            // Note this case should not occur: would eventually give an "Unexpected type application" error in TcDelayed 
            propagate delayedList' mExprAndTypeArgs exprty 

        | DelayedApp (_, arg, mExprAndArg) :: delayedList' ->
            let denv = env.DisplayEnv
            match UnifyFunctionTypeUndoIfFailed cenv denv mExpr exprty with
            | Some (_,resultTy) -> 
                propagate delayedList' mExprAndArg resultTy 
            | None -> 
                let mArg = arg.Range
                match arg with 
                | SynExpr.CompExpr _ -> ()
                | _ -> 
                    // 'delayed' is about to be dropped on the floor, first do rudimentary checking to get name resolutions in its body
                    RecordNameAndTypeResolutions_IdeallyWithoutHavingOtherEffects_Delayed cenv env tpenv delayed
                    error (NotAFunction(denv,overallTy,mExpr,mArg)) 

    propagate delayed expr.Range exprty
    TcDelayed cenv overallTy env tpenv mExpr expr exprty atomicFlag delayed


/// Typecheck "expr ... " constructs where "..." is a sequence of applications,
/// type applications and dot-notation projections.
and TcDelayed cenv overallTy env tpenv mExpr expr exprty (atomicFlag:ExprAtomicFlag) delayed = 

    // OK, we've typechecked the thing on the left of the delayed lookup chain. 
    // We can now record for posterity the type of this expression and the location of the expression. 
    if (atomicFlag = ExprAtomicFlag.Atomic) then
        CallExprHasTypeSink cenv.tcSink (mExpr,env.NameEnv,exprty, env.DisplayEnv,env.eAccessRights)

    match delayed with 
    | []  
    | DelayedDot :: _ -> 
        UnifyTypesAndRecover cenv env mExpr overallTy exprty
        expr.Expr,tpenv
    // expr.M(args) where x.M is a .NET method or index property 
    // expr.M<tyargs>(args) where x.M is a .NET method or index property 
    // expr.M where x.M is a .NET method or index property 
    | DelayedDotLookup (longId,mDotLookup) :: otherDelayed ->
         TcLookupThen cenv overallTy env tpenv mExpr expr.Expr exprty longId otherDelayed mDotLookup
    // f x 
    | DelayedApp (hpa, arg, mExprAndArg) :: otherDelayed ->
        TcFunctionApplicationThen cenv overallTy env tpenv mExprAndArg expr exprty arg hpa otherDelayed
    // f<tyargs> 
    | DelayedTypeApp (_, mTypeArgs, _mExprAndTypeArgs) :: _ ->
        error(Error(FSComp.SR.tcUnexpectedTypeArguments(), mTypeArgs))
    | DelayedSet _ :: _ ->      
        error(Error(FSComp.SR.tcInvalidAssignment(),mExpr))


/// Convert the delayed identifiers to a dot-lookup.
///
/// TcItemThen:   For StaticItem [.Lookup]          , mPrior is the range of StaticItem 
/// TcLookupThen: For expr.InstanceItem [.Lookup]   , mPrior is the range of expr.InstanceItem
and delayRest rest mPrior delayed = 
    match rest with 
    | [] -> delayed 
    | longId -> 
        let mPriorAndLongId = unionRanges mPrior (rangeOfLid longId)
        DelayedDotLookup (rest,mPriorAndLongId) :: delayed


//-------------------------------------------------------------------------
// TcFunctionApplicationThen: Typecheck "expr x" + projections
//------------------------------------------------------------------------- 

and TcFunctionApplicationThen cenv overallTy env tpenv mExprAndArg expr exprty (synArg: SynExpr) atomicFlag delayed = 
    
    let denv = env.DisplayEnv
    let mArg = synArg.Range
    let mFunExpr = expr.Range
    // If the type of 'synArg' unifies as a function type, then this is a function application, otherwise
    // it is an error or a computation expression
    match UnifyFunctionTypeUndoIfFailed cenv denv mFunExpr exprty with
    | Some (domainTy,resultTy) -> 

        // Notice the special case 'seq { ... }'. In this case 'seq' is actually a function in the F# library.
        // Set a flag in the syntax tree to say we noticed a leading 'seq'
        match synArg with 
        | SynExpr.CompExpr (false,isNotNakedRefCell,_comp,_m) -> 
            isNotNakedRefCell := 
                !isNotNakedRefCell
                || 
                (match expr with 
                 | ApplicableExpr(_,Expr.Op(TOp.Coerce,_,[Expr.App(Expr.Val(vf,_,_),_,_,_,_)],_),_) when valRefEq cenv.g vf cenv.g.seq_vref -> true 
                 | _ -> false)
        | _ -> ()

        let arg,tpenv = TcExpr cenv domainTy env tpenv synArg
        let exprAndArg = buildApp cenv expr exprty arg mExprAndArg
        TcDelayed cenv overallTy env tpenv mExprAndArg exprAndArg resultTy atomicFlag delayed
    | None -> 
        // OK, 'expr' doesn't have function type, but perhaps 'expr' is a computation expression builder, and 'arg' is '{ ... }' 
        match synArg with 
        | SynExpr.CompExpr (false,_isNotNakedRefCell,comp,_m) -> 
            let bodyOfCompExpr,tpenv = TcComputationOrSequenceExpression cenv env overallTy mFunExpr (Some(expr.Expr,exprty)) tpenv comp
            TcDelayed cenv overallTy env tpenv mExprAndArg (MakeApplicableExprNoFlex cenv bodyOfCompExpr) (tyOfExpr cenv.g bodyOfCompExpr) ExprAtomicFlag.NonAtomic delayed 
        | _ -> 
            error (NotAFunction(denv,overallTy,mFunExpr,mArg)) 

//-------------------------------------------------------------------------
// TcLongIdentThen : Typecheck "A.B.C<D>.E.F ... " constructs
//------------------------------------------------------------------------- 

and TcLongIdentThen cenv overallTy env tpenv (LongIdentWithDots(longId,_)) delayed =

    let ad = env.eAccessRights
    let typeNameResInfo = 
        // Given 'MyOverloadedType<int>.MySubType...' use arity of #given type arguments to help 
        // resolve type name lookup of 'MyOverloadedType' 
        // Also determine if type names should resolve to Item.Types or Item.CtorGroup 
        match delayed with 
        | DelayedTypeApp (tyargs, _, _) :: (DelayedDot | DelayedDotLookup _) :: _ -> 
            // cases like 'MyType<int>.Sth' 
            TypeNameResolutionInfo(ResolveTypeNamesToTypeRefs, TypeNameResolutionStaticArgsInfo.FromTyArgs tyargs.Length)

        | DelayedTypeApp (tyargs, _, _) :: _ -> 
            // Note, this also covers the case 'MyType<int>.' (without LValue_get), which is needed for VS (when typing)
            TypeNameResolutionInfo(ResolveTypeNamesToCtors, TypeNameResolutionStaticArgsInfo.FromTyArgs tyargs.Length)

        | _ -> 
            TypeNameResolutionInfo.Default

    let nameResolutionResult = ResolveLongIdentAsExprAndComputeRange cenv.tcSink cenv.nameResolver (rangeOfLid longId) ad env.eNameResEnv typeNameResInfo longId
    TcItemThen cenv overallTy env tpenv nameResolutionResult delayed

//-------------------------------------------------------------------------
// Typecheck "item+projections" 
//------------------------------------------------------------------------- *)
// mItem is the textual range covered by the long identifiers that make up the item
and TcItemThen cenv overallTy env tpenv (item,mItem,rest,afterOverloadResolution) delayed =
    let delayed = delayRest rest mItem delayed
    let ad = env.eAccessRights
    match item with 
    // x where x is a union case or active pattern result tag. 
    | (Item.UnionCase _ | Item.ExnCase _ | Item.ActivePatternResult _) as item -> 
        // ucaseAppTy is the type of the union constructor applied to its (optional) argument 
        let ucaseAppTy = NewInferenceType ()
        let mkConstrApp,argtys, argNames = 
          match item with 
          | Item.ActivePatternResult(apinfo, _, n, _) -> 
              let aparity = apinfo.Names.Length
              match aparity with 
              | 0 | 1 -> 
                  let mkConstrApp _mArgs = function [arg] -> arg | _ -> error(InternalError("ApplyUnionCaseOrExn",mItem))
                  mkConstrApp, [ucaseAppTy], [ for (s,m) in apinfo.ActiveTagsWithRanges -> mkSynId m s ]
              | _ ->
                  let ucref = mkChoiceCaseRef cenv.g mItem aparity n
                  let _,_,tinst,_ = infoOfTyconRef mItem ucref.TyconRef
                  let ucinfo = UnionCaseInfo(tinst,ucref)
                  ApplyUnionCaseOrExnTypes mItem cenv env ucaseAppTy (Item.UnionCase(ucinfo,false))
          | _ -> 
              ApplyUnionCaseOrExnTypes mItem cenv env ucaseAppTy item
        let nargtys = List.length argtys
        // Subsumption at data constructions if argument type is nominal prior to equations for any arguments or return types
        let flexes = argtys |> List.map (isTyparTy cenv.g >> not)
        
        let (|FittedArgs|_|) arg = 
            match arg with 
            | SynExprParen(SynExpr.Tuple(args,_,_),_,_,_)
            | SynExpr.Tuple(args,_,_)     when nargtys > 1 -> Some args
            | SynExprParen(arg,_,_,_)
            | arg when nargtys = 1 -> Some [arg]
            | _ -> None

        match delayed with 
        // This is where the constructor is applied to an argument 
        | ((DelayedApp (atomicFlag, (FittedArgs args as origArg), mExprAndArg))::otherDelayed) ->
            // assert the overall result type if possible
            if isNil otherDelayed then 
                UnifyTypes cenv env mExprAndArg overallTy ucaseAppTy 

            let nargs = List.length args
            UnionCaseOrExnCheck env nargtys nargs mExprAndArg
            
            // if we manage to get here - number of formal arguments = number of actual arguments
            // apply named parameters
            let args =
                // GetMethodArgs checks that no named parameters are located before positional
                let unnamedArgs,namedCallerArgs = GetMethodArgs origArg
                match namedCallerArgs with
                | [] -> 
                    args
                | _ ->
                    let fittedArgs = Array.zeroCreate nargtys
                    
                    // first: put all positional arguments
                    let mutable currentIndex = 0
                    for arg in unnamedArgs do
                        fittedArgs.[currentIndex] <- arg
                        currentIndex <- currentIndex + 1

                    let SEEN_NAMED_ARGUMENT = -1

                    // dealing with named arguments is a bit tricky since prior to these changes we have an ambiguous situation:
                    // regular notation for named parameters Some(Value = 5) can mean either 1) create option<bool> with value - result of equality operation or 2) create option<int> using named arg syntax.
                    // so far we've used 1) so we cannot immediately switch to 2) since it will be a definite breaking change.
                    
                    for (_, id, arg) in namedCallerArgs do
                        match argNames |> List.tryFindIndex (fun id2 -> id.idText = id2.idText) with
                        | Some i -> 
                            if isNull(box fittedArgs.[i]) then
                                fittedArgs.[i] <- arg
                                let argContainerOpt = match item with
                                                      | Item.UnionCase(uci,_) -> Some(ArgumentContainer.UnionCase(uci))
                                                      | Item.ExnCase tref -> Some(ArgumentContainer.Type(tref))
                                                      | _ -> None
                                let argItem = Item.ArgName (argNames.[i], argtys.[i], argContainerOpt)   
                                CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,argItem,argItem,ItemOccurence.Use,env.DisplayEnv,ad)
                            else error(Error(FSComp.SR.tcUnionCaseFieldCannotBeUsedMoreThanOnce(id.idText), id.idRange))
                            currentIndex <- SEEN_NAMED_ARGUMENT
                        | None ->
                            // ambiguity may appear only when if argument is boolean\generic.
                            // if 
                            // - we didn't find argument with specified name AND 
                            // - we have not seen any named arguments so far AND 
                            // - type of current argument is bool\generic 
                            // then we'll favor old behavior and treat current argument as positional.
                            let isSpecialCaseForBackwardCompatibility = 
                                (currentIndex <> SEEN_NAMED_ARGUMENT) &&
                                (currentIndex < nargtys) &&
                                match stripTyEqns cenv.g argtys.[currentIndex] with
                                | TType_app(tcref, _) -> tyconRefEq cenv.g cenv.g.bool_tcr tcref || tyconRefEq cenv.g cenv.g.system_Bool_tcref tcref
                                | TType_var(_) -> true
                                | _ -> false

                            if isSpecialCaseForBackwardCompatibility then
                                assert (isNull(box fittedArgs.[currentIndex]))
                                fittedArgs.[currentIndex] <- List.item currentIndex args // grab original argument, not item from the list of named parametere
                                currentIndex <- currentIndex + 1
                            else
                                let caseName = 
                                    match item with
                                    | Item.UnionCase(uci,_) -> uci.Name
                                    | Item.ExnCase tcref -> tcref.DisplayName
                                    | _ -> failwith "impossible"
                                error(Error(FSComp.SR.tcUnionCaseConstructorDoesNotHaveFieldWithGivenName(caseName, id.idText),  id.idRange))

                    assert (Seq.forall (box >> ((<>) null) ) fittedArgs)
                    List.ofArray fittedArgs

            let args',tpenv = TcExprs cenv env mExprAndArg tpenv flexes argtys args
            PropagateThenTcDelayed cenv overallTy env tpenv mExprAndArg (MakeApplicableExprNoFlex cenv (mkConstrApp mExprAndArg args')) ucaseAppTy atomicFlag otherDelayed

        | DelayedTypeApp (_x, mTypeArgs, _mExprAndTypeArgs) :: _delayed' ->
            error(Error(FSComp.SR.tcUnexpectedTypeArguments(),mTypeArgs))
        | _ -> 
            // Work out how many syntactic arguments we really expect. Also return a function that builds the overall 
            // expression, but don't apply this function until after we've checked that the number of arguments is OK 
            // (or else we would be building an invalid expression) 
            
            // Unit-taking active pattern result can be applied to no args 
            let nargs,mkExpr = 
                // This is where the constructor is an active pattern result applied to no argument 
                // Unit-taking active pattern result can be applied to no args 
                if (nargtys = 1 && match item with Item.ActivePatternResult _ -> true | _ -> false) then 
                    UnifyTypes cenv env mItem (List.head argtys) cenv.g.unit_ty
                    1,(fun () -> mkConstrApp mItem [mkUnit cenv.g mItem])

                // This is where the constructor expects no arguments and is applied to no argument 
                elif nargtys = 0 then 
                    0,(fun () -> mkConstrApp mItem []) 
                else 
                    // This is where the constructor expects arguments but is not applied to arguments, hence build a lambda 
                    nargtys, 
                    (fun () -> 
                        let vs,args = argtys |> List.mapi (fun i ty -> mkCompGenLocal mItem ("arg" + string i) ty) |> List.unzip
                        let constrApp = mkConstrApp mItem args
                        let lam = mkMultiLambda mItem vs (constrApp, tyOfExpr cenv.g constrApp)
                        lam)
            UnionCaseOrExnCheck env nargtys nargs mItem
            let expr = mkExpr()
            let exprTy = tyOfExpr cenv.g expr
            PropagateThenTcDelayed cenv overallTy env tpenv mItem (MakeApplicableExprNoFlex cenv expr) exprTy ExprAtomicFlag.Atomic delayed 

    | Item.Types(nm,(typ::_)) -> 
    
        match delayed with 
        | ((DelayedTypeApp(tyargs, _mTypeArgs, mExprAndTypeArgs))::(DelayedDotLookup (longId,mLongId))::otherDelayed) ->

            // If Item.Types is returned then the typ will be of the form TType_app(tcref,genericTyargs) where tyargs 
            // is a fresh instantiation for tcref. TcNestedTypeApplication will chop off precisely #genericTyargs args 
            // and replace them by 'tyargs' 
            let typ,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv mExprAndTypeArgs typ tyargs

            // Report information about the whole expression including type arguments to VS
            let item = Item.Types(nm, [typ])
            CallNameResolutionSink cenv.tcSink (mExprAndTypeArgs,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
            TcItemThen cenv overallTy env tpenv (ResolveExprDotLongIdentAndComputeRange cenv.tcSink cenv.nameResolver (unionRanges mExprAndTypeArgs mLongId) ad env.eNameResEnv typ longId IgnoreOverrides true) otherDelayed
            
        | ((DelayedTypeApp(tyargs, _mTypeArgs, mExprAndTypeArgs))::_delayed') ->
            // A case where we have an incomplete name e.g. 'Foo<int>.' - we still want to report it to VS!
            let typ,_ = TcNestedTypeApplication cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv mExprAndTypeArgs typ tyargs
            let item = Item.Types(nm, [typ])
            CallNameResolutionSink cenv.tcSink (mExprAndTypeArgs,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)
            
            // Same error as in the following case
            error(Error(FSComp.SR.tcInvalidUseOfTypeName(),mItem))
            
        | _ -> 
            // In this case the type is not generic, and indeed we should never have returned Item.Types. 
            // That's because ResolveTypeNamesToCtors should have been set at the original 
            // call to ResolveLongIdentAsExprAndComputeRange 
            error(Error(FSComp.SR.tcInvalidUseOfTypeName(),mItem))

    | Item.MethodGroup (methodName,minfos,_) -> 
        // Static method calls Type.Foo(arg1,...,argn) 
        let meths = List.map (fun minfo -> minfo,None) minfos
        let afterTcOverloadResolution = afterOverloadResolution |> AfterTcOverloadResolution.ForMethods
        match delayed with 
        | (DelayedApp (atomicFlag, arg, mExprAndArg)::otherDelayed) ->
            TcMethodApplicationThen cenv env overallTy None tpenv None [] mExprAndArg mItem methodName ad NeverMutates false meths afterTcOverloadResolution NormalValUse [arg] atomicFlag otherDelayed

        | (DelayedTypeApp(tys, mTypeArgs, mExprAndTypeArgs) :: otherDelayed) ->

#if EXTENSIONTYPING
            match TryTcMethodAppToStaticConstantArgs cenv env tpenv (minfos, Some (tys, mTypeArgs), mExprAndTypeArgs, mItem) with 
            | Some minfoAfterStaticArguments ->

              // Replace the resolution including the static parameters, plus the extra information about the original method info
              let item = Item.MethodGroup(methodName, [minfoAfterStaticArguments], Some minfos.[0])
              CallNameResolutionSinkReplacing cenv.tcSink (mItem, env.NameEnv, item, item, ItemOccurence.Use, env.DisplayEnv, env.eAccessRights)                        

              match otherDelayed with 
              | DelayedApp(atomicFlag, arg, mExprAndArg) :: otherDelayed -> 
                  TcMethodApplicationThen cenv env overallTy None tpenv None [] mExprAndArg mItem methodName ad NeverMutates false [(minfoAfterStaticArguments, None)] afterTcOverloadResolution NormalValUse [arg] atomicFlag otherDelayed
              | _ -> 
                  TcMethodApplicationThen cenv env overallTy None tpenv None [] mExprAndTypeArgs mItem methodName ad NeverMutates false [(minfoAfterStaticArguments, None)] afterTcOverloadResolution NormalValUse [] ExprAtomicFlag.Atomic otherDelayed

            | None ->
#endif

            let tyargs,tpenv = TcTypesOrMeasures None cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv tys mTypeArgs
            
            // NOTE: This doesn't take instantiation into account
            CallNameResolutionSink cenv.tcSink (mExprAndTypeArgs,env.NameEnv,item (* ! *), item, ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)                        
            match otherDelayed with 
            | DelayedApp(atomicFlag, arg, mExprAndArg) :: otherDelayed -> 
                TcMethodApplicationThen cenv env overallTy None tpenv (Some tyargs) [] mExprAndArg mItem methodName ad NeverMutates false meths afterTcOverloadResolution NormalValUse [arg] atomicFlag otherDelayed
            | _ -> 
                TcMethodApplicationThen cenv env overallTy None tpenv (Some tyargs) [] mExprAndTypeArgs mItem methodName ad NeverMutates false meths afterTcOverloadResolution NormalValUse [] ExprAtomicFlag.Atomic otherDelayed

        | _ -> 
#if EXTENSIONTYPING
            if not minfos.IsEmpty && minfos.[0].ProvidedStaticParameterInfo.IsSome then 
                error(Error(FSComp.SR.etMissingStaticArgumentsToMethod(),mItem))
#endif
            TcMethodApplicationThen cenv env overallTy None tpenv None [] mItem mItem methodName ad NeverMutates false meths afterTcOverloadResolution NormalValUse [] ExprAtomicFlag.Atomic delayed 

    | Item.CtorGroup(nm,minfos) ->
        let objTy = 
            match minfos with 
            | (minfo :: _) -> minfo.EnclosingType
            | [] -> error(Error(FSComp.SR.tcTypeHasNoAccessibleConstructor(),mItem))
        let afterTcOverloadResolution = AfterTcOverloadResolution.ForConstructors afterOverloadResolution
        match delayed with 
        | ((DelayedApp (_, arg, mExprAndArg))::otherDelayed) ->

            CallExprHasTypeSink cenv.tcSink (mExprAndArg, env.NameEnv,objTy, env.DisplayEnv, env.eAccessRights)
            TcCtorCall true cenv env tpenv overallTy objTy (Some mItem) item false [arg] mExprAndArg otherDelayed (Some afterTcOverloadResolution)

        | ((DelayedTypeApp(tyargs, _mTypeArgs, mExprAndTypeArgs))::(DelayedApp (_, arg, mExprAndArg))::otherDelayed) ->

            let objTyAfterTyArgs,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv mExprAndTypeArgs objTy tyargs
            CallExprHasTypeSink cenv.tcSink (mExprAndArg, env.NameEnv, objTyAfterTyArgs, env.DisplayEnv, env.eAccessRights)
            let itemAfterTyArgs, minfosAfterTyArgs = 
#if EXTENSIONTYPING
                // If the type is provided and took static arguments then the constructor will have changed
                // to a provided constructor on the statically instantiated type. Re-resolve that constructor.
                match objTyAfterTyArgs with 
                | AppTy cenv.g (tcref,_) when tcref.Deref.IsProvided ->
                    let newItem = ForceRaise (ResolveObjectConstructor cenv.nameResolver env.DisplayEnv mExprAndArg ad objTyAfterTyArgs)
                    match newItem with 
                    | Item.CtorGroup(_,newMinfos) -> newItem, newMinfos
                    | _ -> item, minfos
                | _ ->
#endif
                    item, minfos

            minfosAfterTyArgs |> List.iter (fun minfo -> UnifyTypes cenv env mExprAndTypeArgs minfo.EnclosingType objTyAfterTyArgs)
            TcCtorCall true cenv env tpenv overallTy objTyAfterTyArgs (Some mExprAndTypeArgs) itemAfterTyArgs false [arg] mExprAndArg otherDelayed (Some afterTcOverloadResolution)

        | ((DelayedTypeApp(tyargs, _mTypeArgs, mExprAndTypeArgs))::otherDelayed) ->

            let objTy,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv mExprAndTypeArgs objTy tyargs

            // A case where we have an incomplete name e.g. 'Foo<int>.' - we still want to report it to VS!
            let resolvedItem = Item.Types(nm, [objTy])
            CallNameResolutionSink cenv.tcSink (mExprAndTypeArgs,env.NameEnv,resolvedItem,resolvedItem,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)

            minfos |> List.iter (fun minfo -> UnifyTypes cenv env mExprAndTypeArgs minfo.EnclosingType objTy)
            TcCtorCall true cenv env tpenv overallTy objTy (Some mExprAndTypeArgs) item false [] mExprAndTypeArgs otherDelayed (Some afterTcOverloadResolution)

        | _ ->

            TcCtorCall true cenv env tpenv overallTy objTy (Some mItem) item false [] mItem delayed (Some afterTcOverloadResolution)

    | Item.FakeInterfaceCtor _ ->
        error(Error(FSComp.SR.tcInvalidUseOfInterfaceType(),mItem))
    | Item.ImplicitOp(id, sln) ->

        let isPrefix = PrettyNaming.IsPrefixOperator id.idText
        let isTernary = PrettyNaming.IsTernaryOperator id.idText

        let argData = 
            if isPrefix then 
                [ Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true) ]
            elif isTernary then 
                [ Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true)
                  Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true)
                  Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true) ]
            else
                [ Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true)
                  Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true) ]
                
        let retTyData = Typar(mkSynId mItem (cenv.synArgNameGenerator.New()), HeadTypeStaticReq,true)
        let argTypars = argData |> List.map (fun d -> NewTypar (TyparKind.Type, TyparRigidity.Flexible,d,false,TyparDynamicReq.Yes,[],false,false))
        let retTypar = NewTypar (TyparKind.Type, TyparRigidity.Flexible,retTyData,false,TyparDynamicReq.Yes,[],false,false)
        let argTys = argTypars |> List.map mkTyparTy 
        let retTy = mkTyparTy retTypar

        let vs,ves = argTys |> List.mapi (fun i ty -> mkCompGenLocal mItem ("arg" + string i) ty) |> List.unzip

        let memberFlags = StaticMemberFlags MemberKind.Member
        let logicalCompiledName = ComputeLogicalName id memberFlags
        let traitInfo = TTrait(argTys,logicalCompiledName,memberFlags,argTys,Some retTy, sln)

        let expr = Expr.Op(TOp.TraitCall(traitInfo), [], ves, mItem)
        let expr = mkLambdas mItem [] vs (expr,retTy)
        let resultExpr = PropagateThenTcDelayed cenv overallTy env tpenv mItem (MakeApplicableExprNoFlex cenv expr) (tyOfExpr cenv.g expr) ExprAtomicFlag.NonAtomic delayed
        // Add the constraint after the arguments have been checked to allow annotations to kick in on rigid type parameters
        AddCxMethodConstraint env.DisplayEnv cenv.css mItem NoTrace traitInfo
        resultExpr
      
        
    | Item.DelegateCtor typ ->
        match delayed with 
        | ((DelayedApp (atomicFlag, arg, mItemAndArg))::otherDelayed) ->
            TcNewDelegateThen cenv overallTy env tpenv mItem mItemAndArg typ arg atomicFlag otherDelayed
        | ((DelayedTypeApp(tyargs, _mTypeArgs, mItemAndTypeArgs))::(DelayedApp (atomicFlag, arg, mItemAndArg))::otherDelayed) ->
            let typ,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv mItemAndTypeArgs typ tyargs
            
            // Report information about the whole expression including type arguments to VS
            let item = Item.DelegateCtor typ
            CallNameResolutionSink cenv.tcSink (mItemAndTypeArgs,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,env.eAccessRights)            
            TcNewDelegateThen cenv overallTy env tpenv mItem mItemAndArg typ arg atomicFlag otherDelayed
        | _ -> 
            error(Error(FSComp.SR.tcInvalidUseOfDelegate(),mItem))

    | Item.Value vref -> 

        match delayed with 
        // Mutable value set: 'v <- e' 
        | DelayedSet(e2,mStmt) :: otherDelayed ->
            if not (isNil otherDelayed) then error(Error(FSComp.SR.tcInvalidAssignment(),mStmt))
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            vref.Deref.SetHasBeenReferenced() 
            CheckValAccessible mItem env.eAccessRights vref
            CheckValAttributes cenv.g vref mItem  |> CommitOperationResult
            let vty = vref.Type
            let vty2 = 
                if isByrefTy cenv.g vty then 
                    destByrefTy cenv.g vty 
                else 
                    if not vref.IsMutable then error (ValNotMutable(env.DisplayEnv,vref,mStmt))
                    vty 
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true vty2 env tpenv e2
            let vexp = 
                if isByrefTy cenv.g vty then 
                  mkAddrSet mStmt vref e2'
                else 
                  mkValSet mStmt vref e2'
                
            PropagateThenTcDelayed cenv overallTy env tpenv mStmt (MakeApplicableExprNoFlex cenv vexp) (tyOfExpr cenv.g vexp) ExprAtomicFlag.NonAtomic otherDelayed

        // Value instantiation: v<tyargs> ... 
        | (DelayedTypeApp(tys, _mTypeArgs, mExprAndTypeArgs)::otherDelayed) ->
            // Note: we know this is a NormalValUse or PossibleConstrainedCall because: 
            //   - it isn't a CtorValUsedAsSuperInit 
            //   - it isn't a CtorValUsedAsSelfInit 
            //   - it isn't a VSlotDirectCall (uses of base values do not take type arguments 
            let checkTys tpenv kinds = TcTypesOrMeasures (Some kinds) cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv tys mItem
            let (vexp, isSpecial, _, _, tpenv) = TcVal true cenv env tpenv vref (Some (NormalValUse, checkTys)) mItem
            let vexp = (if isSpecial then MakeApplicableExprNoFlex cenv vexp else MakeApplicableExprWithFlex cenv env vexp)
            // type of the expression (e.g. For the source text "sizeof<float>" vexpty will be the TAST type for int32)
            let vexpty = vexp.Type 
            
            // We need to eventually record the type resolution for an expression, but this is done
            // inside PropagateThenTcDelayed, so we don't have to explicitly call 'CallExprHasTypeSink' here            
            PropagateThenTcDelayed cenv overallTy env tpenv mExprAndTypeArgs vexp vexpty ExprAtomicFlag.Atomic otherDelayed

        // Value get 
        | _ ->  
            let (vexp, isSpecial, _, _, tpenv) = TcVal true cenv env tpenv vref None mItem
            let vexp = (if isSpecial then MakeApplicableExprNoFlex cenv vexp else MakeApplicableExprWithFlex cenv env vexp)
            let vexpty = vexp.Type
            PropagateThenTcDelayed cenv overallTy env tpenv mItem vexp vexpty ExprAtomicFlag.Atomic delayed
        
    | Item.Property (nm,pinfos) ->
        if isNil pinfos then error (InternalError ("Unexpected error: empty property list",mItem))
        // if there are both intrinsics and extensions in pinfos, intrinsics will be listed first.
        // by looking at List.Head we are letting the intrinsics determine indexed/non-indexed
        let pinfo = List.head pinfos
        let _, tyargsOpt,args,delayed,tpenv = 
            if pinfo.IsIndexer 
            then GetMemberApplicationArgs delayed cenv env tpenv 
            else ExprAtomicFlag.Atomic,None,[mkSynUnit mItem],delayed,tpenv
        if not pinfo.IsStatic then error (Error (FSComp.SR.tcPropertyIsNotStatic(nm),mItem))
        match delayed with 
        | DelayedSet(e2,mStmt) :: otherDelayed ->
            let args = if pinfo.IsIndexer then args else []
            if not (isNil otherDelayed) then error(Error(FSComp.SR.tcInvalidAssignment(),mStmt))
            // Static Property Set (possibly indexer) 
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            let meths = pinfos |> SettersOfPropInfos
            if isNil meths then error (Error (FSComp.SR.tcPropertyCannotBeSet1 nm,mItem))
            let afterTcOverloadResolution = afterOverloadResolution |> AfterTcOverloadResolution.ForProperties nm SettersOfPropInfos
            // Note: static calls never mutate a struct object argument
            TcMethodApplicationThen cenv env overallTy None tpenv tyargsOpt [] mStmt mItem nm ad NeverMutates true meths afterTcOverloadResolution NormalValUse (args@[e2]) ExprAtomicFlag.NonAtomic otherDelayed
        | _ -> 
            // Static Property Get (possibly indexer) 
            let meths = pinfos |> GettersOfPropInfos
            let afterTcOverloadResolution = afterOverloadResolution |> AfterTcOverloadResolution.ForProperties nm GettersOfPropInfos
            if isNil meths then error (Error (FSComp.SR.tcPropertyIsNotReadable(nm),mItem))
            // Note: static calls never mutate a struct object argument
            TcMethodApplicationThen cenv env overallTy None tpenv tyargsOpt [] mItem mItem nm ad NeverMutates true meths afterTcOverloadResolution NormalValUse args ExprAtomicFlag.Atomic delayed

    | Item.ILField finfo -> 

        CheckILFieldInfoAccessible cenv.g cenv.amap mItem ad finfo
        if not finfo.IsStatic then error (Error (FSComp.SR.tcFieldIsNotStatic(finfo.FieldName),mItem))
        CheckILFieldAttributes cenv.g finfo mItem
        let fref = finfo.ILFieldRef
        let exprty = finfo.FieldType(cenv.amap,mItem)
        match delayed with 
        | DelayedSet(e2,mStmt) :: _delayed' ->
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true exprty env tpenv e2
            let expr = BuildILStaticFieldSet mStmt finfo e2'
            expr,tpenv
        | _ -> 
           // Get static IL field 
            let expr = 
              match finfo.LiteralValue with 
              | Some lit -> 
                  Expr.Const(TcFieldInit mItem lit,mItem,exprty) 
              | None -> 
                let isValueType = finfo.IsValueType
                let valu = if isValueType then AsValue else AsObject

                // The empty instantiation on the fspec is OK, since we make the correct fspec in IlxGen.GenAsm 
                // This ensures we always get the type instantiation right when doing this from 
                // polymorphic code, after inlining etc. 
                let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])

                // Add an I_nop if this is an initonly field to make sure we never recognize it as an lvalue. See mkExprAddrOfExpr. 
                mkAsmExpr ([ mkNormalLdsfld fspec ] @ (if finfo.IsInitOnly then [ AI_nop ] else []), finfo.TypeInst,[],[exprty],mItem)
            PropagateThenTcDelayed cenv overallTy env tpenv mItem (MakeApplicableExprWithFlex cenv env expr) exprty ExprAtomicFlag.Atomic delayed

    | Item.RecdField rfinfo -> 
        // Get static F# field or literal 
        CheckRecdFieldInfoAccessible cenv.amap mItem ad rfinfo
        if not rfinfo.IsStatic then error (Error (FSComp.SR.tcFieldIsNotStatic(rfinfo.Name),mItem))
        CheckRecdFieldInfoAttributes cenv.g rfinfo mItem |> CommitOperationResult        
        let fref = rfinfo.RecdFieldRef
        let fieldTy = rfinfo.FieldType
        match delayed with 
        | DelayedSet(e2,mStmt) :: otherDelayed ->
            if not (isNil otherDelayed) then error(Error(FSComp.SR.tcInvalidAssignment(),mStmt))
        
            // Set static F# field 
            CheckRecdFieldMutation mItem env.DisplayEnv rfinfo
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            let fieldTy = rfinfo.FieldType
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true fieldTy env tpenv e2
            let expr = mkStaticRecdFieldSet (rfinfo.RecdFieldRef,rfinfo.TypeInst,e2',mStmt)
            expr,tpenv
        | _  ->
            let exprty = fieldTy
            let expr = 
              match rfinfo.LiteralValue with 
              // Get literal F# field 
              | Some lit -> Expr.Const(lit,mItem,exprty)
              // Get static F# field 
              | None -> mkStaticRecdFieldGet (fref,rfinfo.TypeInst,mItem) 
            PropagateThenTcDelayed cenv overallTy env tpenv mItem (MakeApplicableExprWithFlex cenv env expr) exprty ExprAtomicFlag.Atomic delayed

    | Item.Event einfo -> 
        // Instance IL event (fake up event-as-value) 
        TcEventValueThen cenv overallTy env tpenv mItem mItem None einfo delayed
     
    | Item.CustomOperation (nm,usageTextOpt,_) -> 
        // 'delayed' is about to be dropped on the floor, first do rudimentary checking to get name resolutions in its body 
        RecordNameAndTypeResolutions_IdeallyWithoutHavingOtherEffects_Delayed cenv env tpenv delayed
        match usageTextOpt() with
        | None -> error(Error(FSComp.SR.tcCustomOperationNotUsedCorrectly(nm), mItem))
        | Some usageText -> error(Error(FSComp.SR.tcCustomOperationNotUsedCorrectly2(nm,usageText), mItem))
    | _ -> error(Error(FSComp.SR.tcLookupMayNotBeUsedHere(), mItem))


//-------------------------------------------------------------------------
// Typecheck "expr.A.B.C ... " constructs
//------------------------------------------------------------------------- 

and GetSynMemberApplicationArgs delayed tpenv =
    match delayed with 
    | DelayedApp (atomicFlag, arg, _) :: otherDelayed -> 
        atomicFlag, None, [arg], otherDelayed, tpenv
    | DelayedTypeApp(tyargs, mTypeArgs, _) :: DelayedApp (atomicFlag, arg, _mExprAndArg) :: otherDelayed ->
        (atomicFlag, Some (tyargs,mTypeArgs), [arg], otherDelayed, tpenv)
    | DelayedTypeApp(tyargs, mTypeArgs, _) :: otherDelayed ->
        (ExprAtomicFlag.Atomic, Some (tyargs,mTypeArgs), [], otherDelayed, tpenv)
    | otherDelayed ->
        (ExprAtomicFlag.NonAtomic, None, [], otherDelayed, tpenv)


and TcMemberTyArgsOpt cenv env tpenv tyargsOpt =
    match tyargsOpt with 
    | None -> None, tpenv
    | Some (tyargs, mTypeArgs) -> 
        let tyargsChecked, tpenv = TcTypesOrMeasures None cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv tyargs mTypeArgs
        Some tyargsChecked, tpenv

and GetMemberApplicationArgs delayed cenv env tpenv =
    let atomicFlag,tyargsOpt,args,delayed,tpenv = GetSynMemberApplicationArgs delayed tpenv  
    let tyArgsOptChecked, tpenv = TcMemberTyArgsOpt cenv env tpenv tyargsOpt
    atomicFlag,tyArgsOptChecked,args,delayed,tpenv 

and TcLookupThen cenv overallTy env tpenv mObjExpr objExpr objExprTy longId delayed mExprAndLongId =
    let objArgs = [objExpr]
    let ad = env.eAccessRights

    // 'base' calls use a different resolution strategy when finding methods. 
    let findFlag = 
        let baseCall = IsBaseCall objArgs
        (if baseCall then PreferOverrides else IgnoreOverrides)
        
    // Canonicalize inference problem prior to '.' lookup on variable types 
    if isTyparTy cenv.g objExprTy then 
        GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,env.DisplayEnv,mExprAndLongId) (freeInTypeLeftToRight cenv.g false objExprTy)
    
    let item,mItem,rest,afterOverloadResolution = ResolveExprDotLongIdentAndComputeRange cenv.tcSink cenv.nameResolver mExprAndLongId ad env.eNameResEnv objExprTy longId findFlag false
    let mExprAndItem = unionRanges mObjExpr mItem
    let delayed = delayRest rest mExprAndItem delayed

    match item with
    | Item.MethodGroup (methodName,minfos,_) -> 
        let atomicFlag,tyargsOpt,args,delayed,tpenv = GetSynMemberApplicationArgs delayed tpenv 
        let afterTcOverloadResolution = afterOverloadResolution |> AfterTcOverloadResolution.ForMethods

        // We pass PossiblyMutates here because these may actually mutate a value type object 
        // To get better warnings we special case some of the few known mutate-a-struct method names 
        let mutates = (if methodName = "MoveNext" || methodName = "GetNextArg" then DefinitelyMutates else PossiblyMutates)

#if EXTENSIONTYPING
        match TryTcMethodAppToStaticConstantArgs cenv env tpenv (minfos, tyargsOpt, mExprAndItem, mItem) with 
        | Some minfoAfterStaticArguments -> 
            // Replace the resolution including the static parameters, plus the extra information about the original method info
            let item = Item.MethodGroup(methodName, [minfoAfterStaticArguments], Some minfos.[0])
            CallNameResolutionSinkReplacing cenv.tcSink (mExprAndItem,env.NameEnv, item, item, ItemOccurence.Use, env.DisplayEnv, env.eAccessRights)                        

            TcMethodApplicationThen cenv env overallTy None tpenv None objArgs mExprAndItem mItem methodName ad mutates false [(minfoAfterStaticArguments, None)] afterTcOverloadResolution NormalValUse args atomicFlag delayed 
        | None -> 
        if not minfos.IsEmpty && minfos.[0].ProvidedStaticParameterInfo.IsSome then 
            error(Error(FSComp.SR.etMissingStaticArgumentsToMethod(),mItem))
#endif

        let tyargsOpt,tpenv = TcMemberTyArgsOpt cenv env tpenv tyargsOpt
        let meths = minfos |> List.map (fun minfo -> minfo,None) 

        TcMethodApplicationThen cenv env overallTy None tpenv tyargsOpt objArgs mExprAndItem mItem methodName ad mutates false meths afterTcOverloadResolution NormalValUse args atomicFlag delayed 

    | Item.Property (nm,pinfos) ->
        // Instance property 
        if isNil pinfos then error (InternalError ("Unexpected error: empty property list",mItem))
        // if there are both intrinsics and extensions in pinfos, intrinsics will be listed first.
        // by looking at List.Head we are letting the intrinsics determine indexed/non-indexed
        let pinfo = List.head pinfos
        let atomicFlag,tyargsOpt,args,delayed,tpenv = 
            if pinfo.IsIndexer
            then GetMemberApplicationArgs delayed cenv env tpenv 
            else ExprAtomicFlag.Atomic,None,[mkSynUnit mItem],delayed,tpenv
        if pinfo.IsStatic then error (Error (FSComp.SR.tcPropertyIsStatic(nm),mItem))
        

        match delayed with 
        | DelayedSet(e2,mStmt) :: otherDelayed ->
            let args = if pinfo.IsIndexer then args else []
            if not (isNil otherDelayed) then error(Error(FSComp.SR.tcInvalidAssignment(),mStmt))
            // Instance property setter 
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            let meths = SettersOfPropInfos pinfos
            if isNil meths then error (Error (FSComp.SR.tcPropertyCannotBeSet1 nm,mItem))
            let afterTcOverloadResolution = afterOverloadResolution |> AfterTcOverloadResolution.ForProperties nm SettersOfPropInfos
            let mut = (if isStructTy cenv.g (tyOfExpr cenv.g objExpr) then DefinitelyMutates else PossiblyMutates)
            TcMethodApplicationThen cenv env overallTy None tpenv tyargsOpt objArgs mStmt mItem nm ad mut true meths afterTcOverloadResolution NormalValUse (args @ [e2]) atomicFlag [] 
        | _ ->                   
            // Instance property getter
            let meths = GettersOfPropInfos pinfos
            if isNil meths then error (Error (FSComp.SR.tcPropertyIsNotReadable(nm),mItem))
            let afterTcOverloadResolution = afterOverloadResolution |> AfterTcOverloadResolution.ForProperties nm GettersOfPropInfos
            TcMethodApplicationThen cenv env overallTy None tpenv tyargsOpt objArgs mExprAndItem mItem nm ad PossiblyMutates true meths afterTcOverloadResolution NormalValUse args atomicFlag delayed 
        
    | Item.RecdField rfinfo ->
        // Get or set instance F# field or literal 
        RecdFieldInstanceChecks cenv.g cenv.amap ad mItem rfinfo
        let tgty = rfinfo.EnclosingType
        let valu = isStructTy cenv.g tgty
        AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css mItem NoTrace tgty objExprTy 
        let objExpr = if valu then objExpr else mkCoerceExpr(objExpr,tgty,mExprAndItem,objExprTy)
        let fieldTy = rfinfo.FieldType
        match delayed with 
        | DelayedSet(e2,mStmt) :: otherDelayed ->
            // Mutable value set: 'v <- e' 
            if not (isNil otherDelayed) then error(Error(FSComp.SR.tcInvalidAssignment(),mItem))
            CheckRecdFieldMutation mItem env.DisplayEnv rfinfo
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true fieldTy env tpenv e2
            BuildRecdFieldSet cenv.g mStmt objExpr rfinfo e2',tpenv

        | _ ->

            // Instance F# Record or Class field 
            let objExpr' = mkRecdFieldGet cenv.g (objExpr,rfinfo.RecdFieldRef,rfinfo.TypeInst,mExprAndItem)
            PropagateThenTcDelayed cenv overallTy env tpenv mExprAndItem (MakeApplicableExprWithFlex cenv env objExpr') fieldTy ExprAtomicFlag.Atomic delayed 
        
    | Item.ILField finfo -> 
        // Get or set instance IL field 
        ILFieldInstanceChecks  cenv.g cenv.amap ad mItem finfo
        let exprty = finfo.FieldType(cenv.amap,mItem)
        
        match delayed with 
        // Set instance IL field 
        | DelayedSet(e2,mStmt) :: _delayed' ->
            UnifyTypes cenv env mStmt overallTy cenv.g.unit_ty
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true exprty env tpenv e2
            let expr = BuildILFieldSet cenv.g mStmt objExpr finfo e2'
            expr,tpenv
        | _ ->        
            let expr = BuildILFieldGet cenv.g cenv.amap mExprAndItem objExpr finfo 
            PropagateThenTcDelayed cenv overallTy env tpenv mExprAndItem (MakeApplicableExprWithFlex cenv env expr) exprty ExprAtomicFlag.Atomic delayed 

    | Item.Event einfo -> 
        // Instance IL event (fake up event-as-value) 
        TcEventValueThen cenv overallTy env tpenv mItem mExprAndItem (Some(objExpr,objExprTy)) einfo delayed
     
    | (Item.FakeInterfaceCtor _ | Item.DelegateCtor _) -> error (Error (FSComp.SR.tcConstructorsCannotBeFirstClassValues(), mItem))
    | _ -> error (Error (FSComp.SR.tcSyntaxFormUsedOnlyWithRecordLabelsPropertiesAndFields(), mItem))

and TcEventValueThen cenv overallTy env tpenv mItem mExprAndItem objDetails (einfo:EventInfo) delayed = 
    // Instance IL event (fake up event-as-value) 
    let nm = einfo.EventName
    let ad = env.eAccessRights
    match objDetails, einfo.IsStatic with 
    | Some _, true -> error (Error (FSComp.SR.tcEventIsStatic(nm),mItem))
    | None, false -> error (Error (FSComp.SR.tcEventIsNotStatic(nm),mItem))
    | _ -> ()

    let delegateType = einfo.GetDelegateType(cenv.amap,mItem)
    let (SigOfFunctionForDelegate(invokeMethInfo,compiledViewOfDelArgTys,_,_)) = GetSigOfFunctionForDelegate cenv.infoReader delegateType mItem ad
    let objArgs = Option.toList (Option.map fst objDetails)
    MethInfoChecks cenv.g cenv.amap true None objArgs env.eAccessRights mItem invokeMethInfo
    
    // This checks for and drops the 'object' sender 
    let argsTy = ArgsTypOfEventInfo cenv.infoReader mItem ad einfo
    if not (slotSigHasVoidReturnTy (invokeMethInfo.GetSlotSig(cenv.amap, mItem))) then errorR (nonStandardEventError einfo.EventName mItem)
    let delEventTy = mkIEventType cenv.g delegateType argsTy

    let bindObjArgs f =
        match objDetails with 
        | None -> f []
        | Some (objExpr,objExprTy) -> mkCompGenLetIn mItem "eventTarget" objExprTy objExpr (fun (_,ve) -> f [ve]) 

    // Bind the object target expression to make sure we only run its sdie effects once, and to make 
    // sure if it's a mutable reference then we dereference it - see FSharp 1.0 bug 942 
    let expr = 
        bindObjArgs (fun objVars -> 
             //     EventHelper ((fun d -> e.add_X(d)), (fun d -> e.remove_X(d)), (fun f -> new 'Delegate(f)))
            mkCallCreateEvent cenv.g mItem delegateType argsTy
               (let dv,de = mkCompGenLocal mItem "eventDelegate" delegateType
                let callExpr,_ = BuildPossiblyConditionalMethodCall cenv env PossiblyMutates mItem false (einfo.GetAddMethod()) NormalValUse [] objVars [de]
                mkLambda mItem dv (callExpr, cenv.g.unit_ty))
               (let dv,de = mkCompGenLocal mItem "eventDelegate" delegateType
                let callExpr,_ = BuildPossiblyConditionalMethodCall cenv env PossiblyMutates mItem false (einfo.GetRemoveMethod()) NormalValUse [] objVars [de]
                mkLambda mItem dv (callExpr, cenv.g.unit_ty))
               (let fvty = (cenv.g.obj_ty --> (argsTy --> cenv.g.unit_ty))
                let fv,fe = mkCompGenLocal mItem "callback" fvty
                let createExpr = BuildNewDelegateExpr (Some einfo, cenv.g, cenv.amap, delegateType, invokeMethInfo, compiledViewOfDelArgTys, fe, fvty, mItem)
                mkLambda mItem fv (createExpr, delegateType)))

    let exprty = delEventTy
    PropagateThenTcDelayed cenv overallTy env tpenv mExprAndItem (MakeApplicableExprNoFlex cenv expr) exprty ExprAtomicFlag.Atomic delayed 
 

//-------------------------------------------------------------------------
// Method uses can calls
//------------------------------------------------------------------------- 

/// Typecheck method/member calls and uses of members as first-class values.
and TcMethodApplicationThen 
       cenv 
       env
       overallTy           // The type of the overall expression including "delayed". THe method "application" may actually be a use of a member as 
                    // a first-class function value, when this would be a function type. 
       objTyOpt   // methodType
       tpenv 
       callerTyArgs // The return type of the overall expression including "delayed" 
       objArgs      // The 'obj' arguments in obj.M(...) and obj.M, if any 
       m           // The range of the object argument or whole application. We immediately union this with the range of the arguments
       mItem       // The range of the item that resolved to the method name
       methodName  // string, name of the method 
       ad          // accessibility rights of the caller 
       mut         // what do we know/assume about whether this method will mutate or not? 
       isProp      // is this a property call? Used for better error messages and passed to BuildMethodCall 
       meths       // the set of methods we may be calling 
       afterTcOverloadResolution // do we need to notify sink after overload resolution
       isSuperInit // is this a special invocation, e.g. a super-class constructor call. Passed through to BuildMethodCall 
       args        // the _syntactic_ method arguments, not yet type checked. 
       atomicFlag  // is the expression atomic or not? 
       delayed     // further lookups and applications that follow this 
     =

    // Nb. args is always of List.length <= 1 except for indexed setters, when it is 2  
    let mWholeExpr = (m,args) ||> List.fold (fun m arg -> unionRanges m arg.Range) 

    // Work out if we know anything about the return type of the overall expression. If there are any delayed 
    // lookups then we don't know anything. 
    let exprTy = if isNil delayed then overallTy else NewInferenceType ()

    // Call the helper below to do the real checking 
    let (expr,attributeAssignedNamedItems,delayed),tpenv = 
        TcMethodApplication false cenv env tpenv callerTyArgs objArgs mWholeExpr mItem methodName objTyOpt ad mut isProp meths afterTcOverloadResolution isSuperInit args exprTy delayed

    // Give errors if some things couldn't be assigned 
    if not (isNil attributeAssignedNamedItems) then 
        let (CallerNamedArg(id,_)) = List.head attributeAssignedNamedItems
        errorR(Error(FSComp.SR.tcNamedArgumentDidNotMatch(id.idText),id.idRange))


    // Resolve the "delayed" lookups 
    let exprty = (tyOfExpr cenv.g expr)

    PropagateThenTcDelayed cenv overallTy env tpenv mWholeExpr (MakeApplicableExprNoFlex cenv expr) exprty atomicFlag delayed 

/// Infer initial type information at the callsite from the syntax of an argument, prior to overload resolution.
and GetNewInferenceTypeForMethodArg cenv env tpenv x =
    match x with 
    | SynExprParen(a,_,_,_) -> GetNewInferenceTypeForMethodArg cenv env tpenv a
    | SynExpr.AddressOf(true,a,_,_) -> mkByrefTy cenv.g (GetNewInferenceTypeForMethodArg cenv env tpenv a)
    | SynExpr.Lambda(_,_,_,a,_) -> mkFunTy (NewInferenceType ()) (GetNewInferenceTypeForMethodArg cenv env tpenv a)
    | SynExpr.Quote(_,raw,a,_,_) -> 
        if raw then mkRawQuotedExprTy cenv.g
        else mkQuotedExprTy cenv.g (GetNewInferenceTypeForMethodArg cenv env tpenv a)
    | _ -> NewInferenceType ()

/// Method calls, property lookups, attribute constructions etc. get checked through here 
and TcMethodApplication 
        checkingAttributeCall 
        cenv 
        env 
        tpenv 
        tyargsOpt
        objArgs 
        mMethExpr  // range of the entire method expression 
        mItem
        methodName 
         (objTyOpt : TType option)
        ad 
        mut 
        isProp 
        calledMethsAndProps 
        afterTcOverloadResolution
        isSuperInit 
        curriedCallerArgs 
        exprTy 
        delayed
    =

    let denv = env.DisplayEnv

    let isSimpleFormalArg (isParamArrayArg, isOutArg, optArgInfo: OptionalArgInfo, callerInfoInfo: CallerInfoInfo, _reflArgInfo: ReflectedArgInfo) = 
        not isParamArrayArg && not isOutArg && not optArgInfo.IsOptional && callerInfoInfo = NoCallerInfo
    
    let callerObjArgTys = objArgs |> List.map (tyOfExpr cenv.g)

    let calledMeths = calledMethsAndProps |> List.map fst

    // Uses of curried members are ALWAYS treated as if they are first class uses of members. 
    // Curried members may not be overloaded (checked at use-site for curried members brought into scope through extension members)
    let curriedCallerArgs,exprTy,delayed = 
        match calledMeths with 
        | [calledMeth] when not isProp && calledMeth.NumArgs.Length > 1 ->
            [], NewInferenceType (),[ for x in curriedCallerArgs -> DelayedApp(ExprAtomicFlag.NonAtomic, x, x.Range) ] @ delayed
        | _ when not isProp && calledMeths |> List.exists (fun calledMeth -> calledMeth.NumArgs.Length > 1) ->
            // This condition should only apply when multiple conflicting curried extension members are brought into scope
            error(Error(FSComp.SR.tcOverloadsCannotHaveCurriedArguments(),mMethExpr))
        | _ -> 
            curriedCallerArgs,exprTy,delayed

    let candidateMethsAndProps = 
        match calledMethsAndProps |> List.filter (fun (meth,_prop) -> IsMethInfoAccessible cenv.amap mItem ad meth) with 
        | [] -> calledMethsAndProps 
        | accessibleMeths -> accessibleMeths            

    let candidates = candidateMethsAndProps |> List.map fst


    // Split the syntactic arguments (if any) into named and unnamed parameters 
    //
    // In one case (the second "single named item" rule) we delay the application of a
    // argument until we've produced a lambda that detuples an input tuple
    let curriedCallerArgsOpt, unnamedDelayedCallerArgExprOpt, exprTy = 
      match curriedCallerArgs with 
      | [] -> 
          None,None,exprTy
      | _ -> 
          let unnamedCurriedCallerArgs,namedCurriedCallerArgs = curriedCallerArgs |> List.map GetMethodArgs |> List.unzip 
          
          // There is an mismatch when _uses_ of indexed property setters in the tc.fs code that calls this function. 
          // The arguments are passed as if they are curried with arity [numberOfIndexParameters;1], however in the TAST, indexed property setters
          // are uncurried and have arity [numberOfIndexParameters+1].
          //
          // Here we work around this mismatch by crunching all property argument lists to uncirred form. 
          // Ideally the problem needs to be solved at its root cause at the callsites to this function
          let unnamedCurriedCallerArgs,namedCurriedCallerArgs = 
              if isProp then 
                  [List.concat unnamedCurriedCallerArgs], [List.concat namedCurriedCallerArgs]
              else 
                  unnamedCurriedCallerArgs,namedCurriedCallerArgs
          
          let MakeUnnamedCallerArgInfo x = (x, GetNewInferenceTypeForMethodArg cenv env tpenv x, x.Range)

          // "single named item" rule. This is where we have a single accessible method 
          //      member x.M(arg1) 
          // being used with  
          //      x.M (x,y) 
          // Without this rule this requires 
          //      x.M ((x,y)) 
          match candidates with 
          | [calledMeth] 
                when (namedCurriedCallerArgs |> List.forall isNil && 
                      let curriedCalledArgs = calledMeth.GetParamAttribs(cenv.amap, mItem)
                      curriedCalledArgs.Length = 1 &&
                      curriedCalledArgs.Head.Length = 1 && 
                      curriedCalledArgs.Head.Head |> isSimpleFormalArg) ->
              let unnamedCurriedCallerArgs = curriedCallerArgs |> List.map (MakeUnnamedCallerArgInfo >> List.singleton)
              let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.map (fun _ -> [])
              (Some (unnamedCurriedCallerArgs,namedCurriedCallerArgs), None, exprTy)

          // "single named item" rule. This is where we have a single accessible method 
          //      member x.M(arg1,arg2) 
          // being used with  
          //      x.M p
          // We typecheck this as if it has been written "(fun (v1,v2) -> x.M(v1,v2))  p" 
          // Without this rule this requires 
          //      x.M (fst p,snd p) 
          | [calledMeth] 
                when (namedCurriedCallerArgs |> List.forall isNil && 
                      unnamedCurriedCallerArgs.Length = 1 &&
                      unnamedCurriedCallerArgs.Head.Length = 1 && 
                      let curriedCalledArgs = calledMeth.GetParamAttribs(cenv.amap, mItem)
                      curriedCalledArgs.Length = 1 &&
                      curriedCalledArgs.Head.Length > 1 &&
                      curriedCalledArgs.Head |> List.forall isSimpleFormalArg) ->

              // The call lambda has function type
              let exprTy = mkFunTy (NewInferenceType ()) exprTy
              
              (None, Some unnamedCurriedCallerArgs.Head.Head, exprTy)

          | _ ->
              let unnamedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.mapSquared MakeUnnamedCallerArgInfo
              let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.mapSquared (fun (isOpt,nm,x) -> 
                let ty = GetNewInferenceTypeForMethodArg cenv env tpenv x
                // #435263 : compiler crash with .net optional parameters and F# optional syntax
                // named optional arguments should always have option type
                let ty = if isOpt then mkOptionTy denv.g ty else ty
                nm,isOpt,x,ty, x.Range
                )

              (Some (unnamedCurriedCallerArgs, namedCurriedCallerArgs), None, exprTy)
    

    let CalledMethHasSingleArgumentGroupOfThisLength n (calledMeth:MethInfo) =
       let curriedMethodArgAttribs = calledMeth.GetParamAttribs(cenv.amap, mItem)
       curriedMethodArgAttribs.Length = 1 && 
       curriedMethodArgAttribs.Head.Length = n

    let GenerateMatchingSimpleArgumentTypes (calledMeth:MethInfo) =
        let curriedMethodArgAttribs = calledMeth.GetParamAttribs(cenv.amap, mItem)
        curriedMethodArgAttribs
        |> List.map (List.filter isSimpleFormalArg >> NewInferenceTypes)

    let UnifyMatchingSimpleArgumentTypes exprTy (calledMeth:MethInfo) =
        let curriedArgTys = GenerateMatchingSimpleArgumentTypes calledMeth
        let returnTy = 
            (exprTy,curriedArgTys) ||>  List.fold (fun exprTy argTys -> 
                let domainTy,resultTy = UnifyFunctionType None cenv denv mMethExpr exprTy
                UnifyTypes cenv env mMethExpr  domainTy (mkRefTupledTy cenv.g argTys)
                resultTy)
        curriedArgTys,returnTy

    if isProp && Option.isNone curriedCallerArgsOpt then 
        error(Error(FSComp.SR.parsIndexerPropertyRequiresAtLeastOneArgument(),mItem))

    // STEP 1. UnifyUniqueOverloading. This happens BEFORE we type check the arguments. 
    // Extract what we know about the caller arguments, either type-directed if 
    // no arguments are given or else based on the syntax of the arguments. 
    let uniquelyResolved,preArgumentTypeCheckingCalledMethGroup = 
        let dummyExpr = mkSynUnit mItem
      
        // Build the CallerArg values for the caller's arguments. 
        // Fake up some arguments if this is the use of a method as a first class function 
        let unnamedCurriedCallerArgs,namedCurriedCallerArgs,returnTy = 

            match curriedCallerArgsOpt,candidates with 
            // "single named item" rule. This is where we have a single accessible method 
            //      memeber x.M(arg1,...,argN) 
            // being used in a first-class way, i.e. 
            //      x.M  
            // Because there is only one accessible method info available based on the name of the item 
            // being accessed we know the number of arguments the first class use of this 
            // method will take. Optional and out args are _not_ included, which means they will be resolved 
            // to their default values (for optionals) and be part of the return tuple (for out args). 
            | None,[calledMeth] -> 
                let curriedArgTys,returnTy = UnifyMatchingSimpleArgumentTypes exprTy calledMeth
                let unnamedCurriedCallerArgs = curriedArgTys |> List.mapSquared (fun ty -> CallerArg(ty,mMethExpr,false,dummyExpr))  
                let namedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.map (fun _ -> [])
                unnamedCurriedCallerArgs, namedCurriedCallerArgs,returnTy
                
            // "type directed" rule for first-class uses of ambiguous methods. 
            // By context we know a type for the input argument. If it's a tuple 
            // this gives us the a potential number of arguments expected. Indeed even if it's a variable 
            // type we assume the number of arguments is just "1". 
            | None,_ ->
            
                let domainTy,returnTy = UnifyFunctionType None cenv denv mMethExpr exprTy
                let argTys = if isUnitTy cenv.g domainTy then [] else  tryDestRefTupleTy cenv.g domainTy
                // Only apply this rule if a candidate method exists with this number of arguments
                let argTys = 
                    if candidates |> List.exists (CalledMethHasSingleArgumentGroupOfThisLength argTys.Length) then 
                       argTys
                    else 
                       [domainTy]
                let unnamedCurriedCallerArgs = [argTys |> List.map (fun ty -> CallerArg(ty,mMethExpr,false,dummyExpr)) ]
                let namedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.map (fun _ -> [])
                unnamedCurriedCallerArgs, namedCurriedCallerArgs, returnTy

            | Some (unnamedCurriedCallerArgs,namedCurriedCallerArgs),_ -> 
                let unnamedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.mapSquared (fun (argExpr,argTy,mArg) -> CallerArg(argTy,mArg,false,argExpr))
                let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.mapSquared (fun (id,isOpt,argExpr,argTy,mArg) -> CallerNamedArg(id,CallerArg(argTy,mArg,isOpt,argExpr))) 
                unnamedCurriedCallerArgs, namedCurriedCallerArgs, exprTy

        let callerArgCounts = (List.sumBy List.length unnamedCurriedCallerArgs, List.sumBy List.length namedCurriedCallerArgs)

        let makeOneCalledMeth (minfo,pinfoOpt,usesParamArrayConversion) = 
            let minst = FreshenMethInfo mItem minfo
            let callerTyArgs = 
                match tyargsOpt with 
                | Some tyargs -> minfo.AdjustUserTypeInstForFSharpStyleIndexedExtensionMembers(tyargs)
                | None -> minst
            let allArgs = List.zip unnamedCurriedCallerArgs namedCurriedCallerArgs
            CalledMeth<SynExpr>(cenv.infoReader,Some(env.NameEnv),checkingAttributeCall, FreshenMethInfo, mMethExpr,ad,minfo,minst,callerTyArgs,pinfoOpt,callerObjArgTys,allArgs,usesParamArrayConversion,true,objTyOpt)

        let preArgumentTypeCheckingCalledMethGroup = 
            [ for (minfo,pinfoOpt) in candidateMethsAndProps do
                let meth = makeOneCalledMeth (minfo,pinfoOpt,true) 
                yield meth
                if meth.UsesParamArrayConversion then 
                    yield makeOneCalledMeth (minfo,pinfoOpt,false) ]

        let uniquelyResolved = 
            let csenv = MakeConstraintSolverEnv ContextInfo.NoContext cenv.css mMethExpr denv
            let res = UnifyUniqueOverloading csenv callerArgCounts methodName ad preArgumentTypeCheckingCalledMethGroup returnTy
            match res with
            |   ErrorResult _ -> afterTcOverloadResolution.OnOverloadResolutionFailure()
            |   _ -> ()
            res |> CommitOperationResult

        uniquelyResolved,preArgumentTypeCheckingCalledMethGroup

    // STEP 2. Type check arguments 
    let unnamedCurriedCallerArgs,namedCurriedCallerArgs,lambdaVars,returnTy,tpenv =  

        // STEP 2a. First extract what we know about the caller arguments, either type-directed if 
        // no arguments are given or else based on the syntax of the arguments. 
        match curriedCallerArgsOpt with 
        | None ->
            let curriedArgTys,returnTy = 
                match candidates with 
                // "single named item" rule. This is where we have a single accessible method 
                //      member x.M(arg1,...,argN) 
                // being used in a first-class way, i.e. 
                //      x.M  
                // Because there is only one accessible method info available based on the name of the item 
                // being accessed we know the number of arguments the first class use of this 
                // method will take. Optional and out args are _not_ included, which means they will be resolved 
                // to their default values (for optionals) and be part of the return tuple (for out args). 
                | [calledMeth] -> 
                    UnifyMatchingSimpleArgumentTypes exprTy calledMeth
                | _ -> 
                    let domainTy,returnTy = UnifyFunctionType None cenv denv mMethExpr exprTy
                    let argTys = if isUnitTy cenv.g domainTy then [] else  tryDestRefTupleTy cenv.g domainTy
                    // Only apply this rule if a candidate method exists with this number of arguments
                    let argTys = 
                        if candidates |> List.exists (CalledMethHasSingleArgumentGroupOfThisLength argTys.Length) then 
                            argTys                                  
                        else
                            [domainTy]
                    [argTys],returnTy
                        
            let lambdaVarsAndExprs = curriedArgTys |> List.mapiSquared (fun i j ty -> mkCompGenLocal mMethExpr ("arg"+string i+string j) ty)
            let unnamedCurriedCallerArgs = lambdaVarsAndExprs |> List.mapSquared (fun (_,e) -> CallerArg(tyOfExpr cenv.g e,e.Range,false,e))
            let namedCurriedCallerArgs = lambdaVarsAndExprs |> List.map (fun _ -> [])
            let lambdaVars = List.mapSquared fst lambdaVarsAndExprs
            unnamedCurriedCallerArgs, namedCurriedCallerArgs, Some lambdaVars, returnTy, tpenv

        | Some (unnamedCurriedCallerArgs,namedCurriedCallerArgs) ->
            // This is the case where some explicit aguments have been given.

            let unnamedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.mapSquared (fun (argExpr,argTy,mArg) -> CallerArg(argTy,mArg,false,argExpr)) 
            let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.mapSquared (fun (id,isOpt,argExpr,argTy,mArg) -> CallerNamedArg(id,CallerArg(argTy,mArg,isOpt,argExpr))) 

            // Collect the information for F# 3.1 lambda propagation rule, and apply the caller's object type to the method's object type if the rule is relevant.
            let lambdaPropagationInfo = 
                if preArgumentTypeCheckingCalledMethGroup.Length > 1 then 
                    [| for meth in preArgumentTypeCheckingCalledMethGroup do 
                        match ExamineMethodForLambdaPropagation meth with
                        | Some (unnamedInfo, namedInfo) ->
                            let calledObjArgTys = meth.CalledObjArgTys(mMethExpr)
                            if (calledObjArgTys, callerObjArgTys) ||> Seq.forall2 (fun calledTy callerTy -> AddCxTypeMustSubsumeTypeMatchingOnlyUndoIfFailed denv cenv.css mMethExpr calledTy callerTy)  then
                                yield (List.toArraySquared unnamedInfo, List.toArraySquared namedInfo)
                        | None -> () |]
                else
                    [| |]

            // Now typecheck the argument expressions
            let unnamedCurriedCallerArgs,(lambdaPropagationInfo,tpenv) =  TcUnnamedMethodArgs cenv env lambdaPropagationInfo tpenv unnamedCurriedCallerArgs 
            let namedCurriedCallerArgs,(_,tpenv) =  TcMethodNamedArgs cenv env lambdaPropagationInfo tpenv namedCurriedCallerArgs 
            unnamedCurriedCallerArgs, namedCurriedCallerArgs, None, exprTy, tpenv

    let preArgumentTypeCheckingCalledMethGroup = 
       preArgumentTypeCheckingCalledMethGroup |> List.map (fun cmeth -> (cmeth.Method, cmeth.CalledTyArgs, cmeth.AssociatedPropertyInfo, cmeth.UsesParamArrayConversion))
    
    // STEP 3. Resolve overloading 
    /// Select the called method that's the result of overload resolution
    let finalCalledMeth = 

        let postArgumentTypeCheckingCalledMethGroup = 
            preArgumentTypeCheckingCalledMethGroup |> List.map (fun (minfo:MethInfo,minst,pinfoOpt,usesParamArrayConversion) ->
                let callerTyArgs = 
                    match tyargsOpt with 
                    | Some tyargs -> minfo.AdjustUserTypeInstForFSharpStyleIndexedExtensionMembers(tyargs)
                    | None -> minst
                let callerArgs = List.zip unnamedCurriedCallerArgs namedCurriedCallerArgs
                CalledMeth<Expr>(cenv.infoReader,Some(env.NameEnv),checkingAttributeCall,FreshenMethInfo, mMethExpr,ad,minfo,minst,callerTyArgs,pinfoOpt,callerObjArgTys,callerArgs,usesParamArrayConversion,true,objTyOpt))
          
        let callerArgCounts = (unnamedCurriedCallerArgs.Length, namedCurriedCallerArgs.Length)
        let csenv = MakeConstraintSolverEnv ContextInfo.NoContext cenv.css mMethExpr denv
        
        // Commit unassociated constraints prior to member overload resolution where there is ambiguity 
        // about the possible target of the call. 
        if not uniquelyResolved then 
            GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,mItem)
                 (//freeInTypeLeftToRight cenv.g false returnTy @
                  (unnamedCurriedCallerArgs |> List.collectSquared  (fun callerArg -> freeInTypeLeftToRight cenv.g false callerArg.Type)))

        let result, errors = 
            ResolveOverloading csenv NoTrace methodName 0 None callerArgCounts ad postArgumentTypeCheckingCalledMethGroup true (Some returnTy) 

        match afterTcOverloadResolution with
        |   AfterTcOverloadResolution.DoNothing -> ()
        |   AfterTcOverloadResolution.SendToSink(callSink,_) ->
                match result with
                |   Some result ->
                        (result.Method,result.AssociatedPropertyInfo) |> callSink
                |   None ->
                        afterTcOverloadResolution.OnOverloadResolutionFailure()
        |   AfterTcOverloadResolution.ReplaceWithOverrideAndSendToSink(overriding, callSink,_) ->
                match result with
                |   Some result ->
                    if result.Method.IsVirtual then 
                        let resultMinfo = result.Method
                        let overridingInfo =
                            overriding
                            |> List.tryFind (fun (minfo,_) -> minfo.IsVirtual && MethInfosEquivByNameAndSig EraseNone true cenv.g cenv.amap range0 resultMinfo minfo)
                        match overridingInfo with
                        |   Some r -> r |> callSink
                        |   None -> (result.Method,result.AssociatedPropertyInfo) |> callSink
                    else
                        (result.Method,result.AssociatedPropertyInfo) |> callSink
                |   None ->
                        afterTcOverloadResolution.OnOverloadResolutionFailure()


        // Raise the errors from the constraint solving 
        RaiseOperationResult errors
        match result with 
        | None -> error(InternalError("at least one error should be returned by failed method overloading",mItem))
        | Some res ->  res

    let finalCalledMethInfo = finalCalledMeth.Method
    let finalCalledMethInst = finalCalledMeth.CalledTyArgs
    let finalArgSets = finalCalledMeth.ArgSets
    let finalAssignedItemSetters = finalCalledMeth.AssignedItemSetters
    let finalCalledPropInfoOpt = finalCalledMeth.AssociatedPropertyInfo
    let finalAttributeAssignedNamedItems = finalCalledMeth.AttributeAssignedNamedArgs
    let finalUnnamedCalledOptArgs = finalCalledMeth.UnnamedCalledOptArgs
    let finalUnnamedCalledOutArgs = finalCalledMeth.UnnamedCalledOutArgs

    let finalAssignedNamedArgs = finalArgSets |> List.collect (fun argSet -> argSet.AssignedNamedArgs)
    let finalParamArrayCallerArgs = finalArgSets |> List.collect (fun argSet -> argSet.ParamArrayCallerArgs)
    let finalUnnamedCalledArgs = finalArgSets |> List.collect (fun argSet -> argSet.UnnamedCalledArgs)
    let finalUnnamedCallerArgs = finalArgSets |> List.collect (fun argSet -> argSet.UnnamedCallerArgs)
    
    // STEP 4. Check the attributes on the method and the corresponding event/property, if any 

    finalCalledPropInfoOpt |> Option.iter (fun pinfo -> CheckPropInfoAttributes pinfo mItem |> CommitOperationResult) 

    let isInstance = not (isNil objArgs)
    MethInfoChecks cenv.g cenv.amap isInstance tyargsOpt objArgs ad mItem finalCalledMethInfo

    // Adhoc constraints on use of .NET methods
    begin 
        // Uses of Object.GetHashCode and Object.Equals imply an equality constraint on the object argument
        //
        if (isInstance && 
            finalCalledMethInfo.IsInstance &&
            typeEquiv cenv.g finalCalledMethInfo.EnclosingType cenv.g.obj_ty && 
            (finalCalledMethInfo.LogicalName = "GetHashCode" ||  finalCalledMethInfo.LogicalName = "Equals")) then 
           
            objArgs |> List.iter (fun expr -> ConstraintSolver.AddCxTypeMustSupportEquality env.DisplayEnv cenv.css mMethExpr NoTrace (tyOfExpr cenv.g expr))

        // Uses of a Dictionary() constructor without an IEqualityComparer argument imply an equality constraint 
        // on the first type argument.
        if HasHeadType cenv.g cenv.g.tcref_System_Collections_Generic_Dictionary finalCalledMethInfo.EnclosingType  &&
           finalCalledMethInfo.IsConstructor &&
           not (finalCalledMethInfo.GetParamDatas(cenv.amap, mItem, finalCalledMeth.CallerTyArgs) 
                |> List.existsSquared (fun (ParamData(_,_,_,_,_,_,ty)) ->  
                    HasHeadType cenv.g cenv.g.tcref_System_Collections_Generic_IEqualityComparer ty)) then 
            
            match argsOfAppTy cenv.g finalCalledMethInfo.EnclosingType with 
            | [dty; _] -> ConstraintSolver.AddCxTypeMustSupportEquality env.DisplayEnv cenv.css mMethExpr NoTrace dty
            | _ -> ()
    end

    if (finalArgSets |> List.existsi (fun i argSet -> argSet.UnnamedCalledArgs |> List.existsi (fun j ca -> ca.Position <> (i,j)))) then
        errorR(Deprecated(FSComp.SR.tcUnnamedArgumentsDoNotFormPrefix(),mMethExpr))


    // STEP 5. Build the argument list. Adjust for optional arguments, byref arguments and coercions.
    // For example, if you pass an F# reference cell to a byref then we must get the address of the 
    // contents of the ref. Likewise lots of adjustments are made for optional arguments etc.

    // Some of the code below must allocate temporary variables or bind other variables to particular values. 
    // As usual we represent variable allocators by expr -> expr functions 
    // which we then use to wrap the whole expression. These will either do nothing or pre-bind a variable. It doesn't
    // matter what order they are applied in as long as they are all composed together.
    let emptyPreBinder (e: Expr) = e
    
    // For unapplied 'e.M' we first evaluate 'e' outside the lambda, i.e. 'let v = e in (fun arg -> v.M(arg))' 
    let objArgPreBinder,objArgs = 
        match objArgs,lambdaVars with 
        | [objArg],Some _   -> 
            let objArgTy = tyOfExpr cenv.g objArg
            let v,ve = mkCompGenLocal mMethExpr "objectArg" objArgTy
            (fun body -> mkCompGenLet mMethExpr v objArg body), [ve]

        | _ -> 
            emptyPreBinder,objArgs

    // Handle adhoc argument conversions
    let coerceExpr isOutArg calledArgTy (reflArgInfo: ReflectedArgInfo) callerArgTy m callerArgExpr = 

       if isByrefTy cenv.g calledArgTy && isRefCellTy cenv.g callerArgTy then 
           Expr.Op(TOp.RefAddrGet,[destRefCellTy cenv.g callerArgTy],[callerArgExpr],m) 

       elif isDelegateTy cenv.g calledArgTy && isFunTy cenv.g callerArgTy then 
           CoerceFromFSharpFuncToDelegate cenv.g cenv.amap cenv.infoReader ad callerArgTy m callerArgExpr calledArgTy

       elif isLinqExpressionTy cenv.g calledArgTy &&  isDelegateTy cenv.g (destLinqExpressionTy cenv.g calledArgTy) && isFunTy cenv.g callerArgTy then 
           let delegateTy = destLinqExpressionTy cenv.g calledArgTy
           let expr = CoerceFromFSharpFuncToDelegate cenv.g cenv.amap cenv.infoReader ad callerArgTy m callerArgExpr delegateTy
           mkCallQuoteToLinqLambdaExpression cenv.g m delegateTy   (Expr.Quote(expr, ref None, false, m, mkQuotedExprTy cenv.g delegateTy))

       // auto conversions to quotations (to match auto conversions to LINQ expressions)
       elif reflArgInfo.AutoQuote && isQuotedExprTy cenv.g calledArgTy &&  not (isQuotedExprTy cenv.g callerArgTy) then 
           match reflArgInfo with 
           | ReflectedArgInfo.Quote true -> 
               mkCallLiftValueWithDefn cenv.g m calledArgTy callerArgExpr
           | ReflectedArgInfo.Quote false -> 
               Expr.Quote(callerArgExpr, ref None, false, m, calledArgTy)
           | ReflectedArgInfo.None -> failwith "unreachable" // unreachable due to reflArgInfo.AutoQuote condition

       // Note: out args do not need to be coerced 
       elif isOutArg then 
           callerArgExpr

       // Note: not all these casts are reported in quotations 
       else 
           mkCoerceIfNeeded cenv.g calledArgTy callerArgTy callerArgExpr

    // Handle optional arguments
    let optArgPreBinder,allArgs,outArgExprs,outArgTmpBinds = 

        let normalUnnamedArgs = 
          (finalUnnamedCalledArgs,finalUnnamedCallerArgs) ||> List.map2 (fun called caller -> { NamedArgIdOpt = None; CalledArg=called; CallerArg=caller })

        let paramArrayArgs = 
          match finalCalledMeth.ParamArrayCalledArgOpt with 
          | None -> []
          | Some paramArrayCalledArg -> 
               let paramArrayCalledArgElementType = destArrayTy cenv.g paramArrayCalledArg.CalledArgumentType

               let es = 
                   finalParamArrayCallerArgs  |> List.map (fun callerArg -> 
                       let (CallerArg(callerArgTy,m,isOutArg,callerArgExpr)) = callerArg
                       coerceExpr isOutArg paramArrayCalledArgElementType paramArrayCalledArg.ReflArgInfo callerArgTy m callerArgExpr)

               [ { NamedArgIdOpt = None; CalledArg=paramArrayCalledArg; CallerArg=CallerArg(paramArrayCalledArg.CalledArgumentType,mMethExpr,false,Expr.Op(TOp.Array,[paramArrayCalledArgElementType], es ,mMethExpr)) } ]

        // CLEANUP: Move all this code into some isolated file, e.g. "optional.fs"
        //
        // Handle CallerSide optional arguments. 
        //
        // CallerSide optional arguments are largely for COM interop, e.g. to PIA assemblies for Word etc.
        // As a result we follow the VB and C# behavior here.
        //
        //   "1.        If the parameter is statically typed as System.Object and does not have a value, then there are four cases:
        //       a.     The parameter is marked with MarshalAs(IUnknown), MarshalAs(Interface), or MarshalAs(IDispatch). In this case we pass null.
        //       b.     Else if the parameter is marked with IUnknownConstantAttribute. In this case we pass new System.Runtime.InteropServices.UnknownWrapper(null)
        //       c.     Else if the parameter is marked with IDispatchConstantAttribute. In this case we pass new System.Runtime.InteropServices.DispatchWrapper(null)
        //       d.     Else, we will pass Missing.Value.
        //    2.        Otherwise, if there is a value attribute, then emit the default value.
        //    3.        Otherwise, we emit default(T).
        //    4.        Finally, we apply conversions from the value to the parameter type. This is where the nullable conversions take place for VB.
        //    - VB allows you to mark ref parameters as optional. The semantics of this is that we create a temporary 
        //        with type = type of parameter, load the optional value to it, and call the method. 
        //    - VB also allows you to mark arrays with Nothing as the optional value.
        //    - VB also allows you to pass intrinsic values as optional values to parameters 
        //        typed as Object. What we do in this case is we box the intrinsic value."
        //
        let optArgs,optArgPreBinder = 
          (emptyPreBinder,finalUnnamedCalledOptArgs) ||> List.mapFold (fun wrapper calledArg -> 
              let calledArgTy = calledArg.CalledArgumentType
              let wrapper2,expr = 
                  match calledArg.OptArgInfo with 
                  | NotOptional -> 
                      error(InternalError("Unexpected NotOptional",mItem))
                  | CallerSide dfltVal ->
                      let rec build currCalledArgTy currDfltVal =
                          match currDfltVal with
                          | MissingValue -> 
                              // Add an I_nop if this is an initonly field to make sure we never recognize it as an lvalue. See mkExprAddrOfExpr. 
                              emptyPreBinder,mkAsmExpr ([ mkNormalLdsfld (fspec_Missing_Value cenv.g); AI_nop ],[],[],[currCalledArgTy],mMethExpr)
                          | DefaultValue -> 
                              emptyPreBinder,mkDefault(mMethExpr,currCalledArgTy)
                          | Constant fieldInit -> 
                                match currCalledArgTy with
                                | NullableTy cenv.g inst when fieldInit <> ILFieldInit.Null ->
                                    let nullableTy = mkILNonGenericBoxedTy(cenv.g.FindSysILTypeRef "System.Nullable`1")
                                    let ctor = mkILCtorMethSpecForTy(nullableTy, [ILType.TypeVar 0us]).MethodRef
                                    let ctorArgs = [Expr.Const(TcFieldInit mMethExpr fieldInit,mMethExpr, inst)]
                                    emptyPreBinder,Expr.Op(TOp.ILCall(false, false, true, true, NormalValUse, false, false, ctor, [inst], [], [currCalledArgTy]), [], ctorArgs, mMethExpr)
                                | ByrefTy cenv.g inst ->
                                    build inst (PassByRef(inst, currDfltVal))
                                | _ ->
                                    match calledArg.CallerInfoInfo, env.eCallerMemberName with
                                    | CallerLineNumber, _ when typeEquiv cenv.g currCalledArgTy cenv.g.int_ty ->
                                        emptyPreBinder,Expr.Const(Const.Int32(mMethExpr.StartLine), mMethExpr, currCalledArgTy)
                                    | CallerFilePath, _ when typeEquiv cenv.g currCalledArgTy cenv.g.string_ty ->
                                        emptyPreBinder,Expr.Const(Const.String(System.IO.Path.GetFullPath(mMethExpr.FileName)), mMethExpr, currCalledArgTy)
                                    | CallerMemberName, Some(callerName) when (typeEquiv cenv.g currCalledArgTy cenv.g.string_ty) ->
                                        emptyPreBinder,Expr.Const(Const.String(callerName), mMethExpr, currCalledArgTy)
                                    | _ ->
                                        emptyPreBinder,Expr.Const(TcFieldInit mMethExpr fieldInit,mMethExpr,currCalledArgTy)
                                    
                          | WrapperForIDispatch ->
                              match cenv.g.TryFindSysILTypeRef "System.Runtime.InteropServices.DispatchWrapper" with
                              | None -> error(Error(FSComp.SR.fscSystemRuntimeInteropServicesIsRequired(), mMethExpr))
                              | Some tref ->
                                  let ty = mkILNonGenericBoxedTy tref
                                  let mref = mkILCtorMethSpecForTy(ty,[cenv.g.ilg.typ_Object]).MethodRef
                                  let expr = Expr.Op(TOp.ILCall(false,false,false,true,NormalValUse,false,false,mref,[],[],[cenv.g.obj_ty]),[],[mkDefault(mMethExpr,currCalledArgTy)],mMethExpr)
                                  emptyPreBinder,expr
                          | WrapperForIUnknown ->
                              match cenv.g.TryFindSysILTypeRef "System.Runtime.InteropServices.UnknownWrapper" with
                              | None -> error(Error(FSComp.SR.fscSystemRuntimeInteropServicesIsRequired(), mMethExpr))
                              | Some tref ->
                                  let ty = mkILNonGenericBoxedTy tref
                                  let mref = mkILCtorMethSpecForTy(ty,[cenv.g.ilg.typ_Object]).MethodRef
                                  let expr = Expr.Op(TOp.ILCall(false,false,false,true,NormalValUse,false,false,mref,[],[],[cenv.g.obj_ty]),[],[mkDefault(mMethExpr,currCalledArgTy)],mMethExpr)
                                  emptyPreBinder,expr
                          | PassByRef (ty, dfltVal2) ->
                              let v,_ = mkCompGenLocal mMethExpr "defaultByrefArg" ty
                              let wrapper2,rhs = build currCalledArgTy dfltVal2
                              (wrapper2 >> mkCompGenLet mMethExpr v rhs), mkValAddr mMethExpr (mkLocalValRef v)
                      build calledArgTy dfltVal
                  | CalleeSide ->
                      let calledNonOptTy = 
                          if isOptionTy cenv.g calledArgTy then 
                              destOptionTy cenv.g calledArgTy 
                          else
                              calledArgTy // should be unreachable

                      match calledArg.CallerInfoInfo, env.eCallerMemberName with
                      | CallerLineNumber, _ when typeEquiv cenv.g calledNonOptTy cenv.g.int_ty ->
                          let lineExpr = Expr.Const(Const.Int32(mMethExpr.StartLine), mMethExpr, calledNonOptTy)
                          emptyPreBinder,mkUnionCaseExpr(mkSomeCase cenv.g,[calledNonOptTy],[lineExpr],mMethExpr)
                      | CallerFilePath, _ when typeEquiv cenv.g calledNonOptTy cenv.g.string_ty ->
                          let filePathExpr = Expr.Const(Const.String(System.IO.Path.GetFullPath(mMethExpr.FileName)), mMethExpr, calledNonOptTy)
                          emptyPreBinder,mkUnionCaseExpr(mkSomeCase cenv.g,[calledNonOptTy],[filePathExpr],mMethExpr)
                      | CallerMemberName, Some(callerName) when typeEquiv cenv.g calledNonOptTy cenv.g.string_ty ->
                          let memberNameExpr = Expr.Const(Const.String(callerName), mMethExpr, calledNonOptTy)
                          emptyPreBinder,mkUnionCaseExpr(mkSomeCase cenv.g,[calledNonOptTy],[memberNameExpr],mMethExpr)
                      | _ ->
                          emptyPreBinder,mkUnionCaseExpr(mkNoneCase cenv.g,[calledNonOptTy],[],mMethExpr)

              // Combine the variable allocators (if any)
              let wrapper = (wrapper >> wrapper2)
              let callerArg = CallerArg(calledArgTy,mMethExpr,false,expr)
              { NamedArgIdOpt = None; CalledArg = calledArg; CallerArg = callerArg },wrapper)


        // Handle optional arguments
        let wrapOptionalArg (assignedArg: AssignedCalledArg<_>) =
            let (CallerArg(callerArgTy,m,isOptCallerArg,expr)) = assignedArg.CallerArg
            match assignedArg.CalledArg.OptArgInfo with 
            | NotOptional -> 
                if isOptCallerArg then errorR(Error(FSComp.SR.tcFormalArgumentIsNotOptional(),m))
                assignedArg
            | _ -> 
                let expr = 
                    match assignedArg.CalledArg.OptArgInfo with 
                    | CallerSide _ -> 
                        if isOptCallerArg then 
                            // STRUCT OPTIONS: if we allow struct options as optional arguments then we should take
                            // the address correctly. 
                            mkUnionCaseFieldGetUnprovenViaExprAddr (expr,mkSomeCase cenv.g,[destOptionTy cenv.g callerArgTy],0,m) 
                        else 
                            expr
                    | CalleeSide -> 
                        if isOptCallerArg then 
                            // M(?x=bopt) when M(A) --> M(?x=Some(b.Value))
                            expr 
                        else                            
                            // M(x=b) when M(A) --> M(?x=Some(b :> A))
                            let calledArgTy = assignedArg.CalledArg.CalledArgumentType
                            if isOptionTy cenv.g calledArgTy then 
                                let calledNonOptTy = destOptionTy cenv.g calledArgTy 
                                mkUnionCaseExpr(mkSomeCase cenv.g,[calledNonOptTy],[mkCoerceIfNeeded cenv.g calledNonOptTy callerArgTy expr],m)
                            else 
                                expr // should be unreachable 
                            
                    | _ -> failwith "Unreachable"
                { assignedArg with CallerArg=CallerArg((tyOfExpr cenv.g expr),m,isOptCallerArg,expr) }

        let outArgsAndExprs,outArgTmpBinds = 
            finalUnnamedCalledOutArgs |> List.map (fun calledArg -> 
                let calledArgTy = calledArg.CalledArgumentType
                let outArgTy = destByrefTy cenv.g calledArgTy
                let outv,outArgExpr = mkMutableCompGenLocal mMethExpr "outArg" outArgTy // mutable! 
                let expr = mkDefault(mMethExpr,outArgTy)
                let callerArg = CallerArg(calledArgTy,mMethExpr,false,mkValAddr mMethExpr (mkLocalValRef outv))
                let outArg = { NamedArgIdOpt=None;CalledArg=calledArg;CallerArg=callerArg }
                (outArg, outArgExpr), mkCompGenBind outv expr) 
              |> List.unzip

        let outArgs, outArgExprs = List.unzip outArgsAndExprs

        let allArgs =
            List.map wrapOptionalArg normalUnnamedArgs @ 
            List.map wrapOptionalArg finalAssignedNamedArgs @ 
            paramArrayArgs @
            optArgs @ 
            outArgs
        
        let allArgs = 
            allArgs |> List.sortBy (fun x -> x.Position)

        optArgPreBinder,allArgs,outArgExprs,outArgTmpBinds

    let coerce (assignedArg: AssignedCalledArg<_>) = 
        let isOutArg = assignedArg.CalledArg.IsOutArg
        let reflArgInfo = assignedArg.CalledArg.ReflArgInfo
        let calledArgTy = assignedArg.CalledArg.CalledArgumentType
        let (CallerArg(callerArgTy,m,_,e)) = assignedArg.CallerArg
    
        coerceExpr isOutArg calledArgTy reflArgInfo callerArgTy m e

    // Record the resolution of the named argument for the Language Service
    allArgs |> List.iter (fun assignedArg ->
        match assignedArg.NamedArgIdOpt with 
        | None -> ()
        | Some id -> 
            let item = Item.ArgName (defaultArg assignedArg.CalledArg.NameOpt id, assignedArg.CalledArg.CalledArgumentType, Some(ArgumentContainer.Method(finalCalledMethInfo)))
            CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,ad))

    let allArgsCoerced = List.map coerce  allArgs


    // Make the call expression 
    let expr,exprty = 
        BuildPossiblyConditionalMethodCall cenv env mut mMethExpr isProp finalCalledMethInfo isSuperInit finalCalledMethInst objArgs allArgsCoerced
        

    // Bind "out" parameters as part of the result tuple 
    let expr,exprty = 
        if isNil outArgTmpBinds then expr,exprty
        else 
            let outArgTys = outArgExprs |> List.map (tyOfExpr cenv.g)
            let expr = if isUnitTy cenv.g exprty then mkCompGenSequential mMethExpr expr  (mkRefTupled cenv.g  mMethExpr outArgExprs outArgTys)
                       else  mkRefTupled cenv.g  mMethExpr (expr :: outArgExprs) (exprty :: outArgTys)
            let expr = mkLetsBind mMethExpr outArgTmpBinds expr
            expr, tyOfExpr cenv.g expr

    // Handle post-hoc property assignments 
    let expr = 
        if isNil finalAssignedItemSetters then expr else 
        // This holds the result of the call 
        let objv,objExpr = mkMutableCompGenLocal mMethExpr "returnVal" exprty // mutable in case it's a struct 
        // This expression  mutates the properties on the result of the call
        let propSetExpr = 
            (mkUnit cenv.g mMethExpr, finalAssignedItemSetters) ||> List.fold (fun acc (AssignedItemSetter(id,setter,CallerArg(callerArgTy,m,isOptCallerArg,argExpr))) ->
                    if isOptCallerArg then error(Error(FSComp.SR.tcInvalidOptionalAssignmentToPropertyOrField(),m))
                    
                    let action, defnItem = 
                        match setter with 
                        | AssignedPropSetter (pinfo,pminfo,pminst) -> 
                            MethInfoChecks cenv.g cenv.amap true None [objExpr] ad m pminfo
                            let calledArgTy = List.head (List.head (pminfo.GetParamTypes(cenv.amap, m, pminst)))
                            let argExpr = coerceExpr false calledArgTy ReflectedArgInfo.None callerArgTy m argExpr
                            let mut = (if isStructTy cenv.g (tyOfExpr cenv.g objExpr) then DefinitelyMutates else PossiblyMutates)
                            let action = BuildPossiblyConditionalMethodCall cenv env mut m true pminfo NormalValUse pminst [objExpr] [argExpr] |> fst 
                            action, Item.Property (pinfo.PropertyName, [pinfo])

                        | AssignedILFieldSetter finfo ->
                            // Get or set instance IL field 
                            ILFieldInstanceChecks  cenv.g cenv.amap ad m finfo
                            let calledArgTy = finfo.FieldType (cenv.amap, m)
                            let argExpr = coerceExpr false calledArgTy ReflectedArgInfo.None callerArgTy m argExpr
                            let action = BuildILFieldSet cenv.g m objExpr finfo argExpr 
                            action, Item.ILField finfo
                        
                        | AssignedRecdFieldSetter rfinfo ->
                            RecdFieldInstanceChecks cenv.g cenv.amap ad m rfinfo 
                            let calledArgTy = rfinfo.FieldType
                            CheckRecdFieldMutation m denv rfinfo
                            let argExpr = coerceExpr false calledArgTy ReflectedArgInfo.None callerArgTy m argExpr
                            let action = BuildRecdFieldSet cenv.g m objExpr rfinfo argExpr 
                            action, Item.RecdField rfinfo

                    // Record the resolution for the Language Service
                    let item = Item.SetterArg (id, defnItem)
                    CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Use,env.DisplayEnv,ad)

                    mkCompGenSequential m acc action)

        // now put them together 
        let expr = mkCompGenLet mMethExpr objv expr  (mkCompGenSequential mMethExpr propSetExpr objExpr)
        expr

    // Build the lambda expression if any 
    let expr = 
        match lambdaVars with 
        | None -> expr
        | Some curriedLambdaVars -> 
            let mkLambda vs expr = 
                match vs with 
                | [] -> mkUnitDelayLambda cenv.g mMethExpr expr 
                | _ -> mkMultiLambda mMethExpr vs (expr, tyOfExpr cenv.g expr)
            List.foldBack mkLambda curriedLambdaVars expr

    let expr, tpenv = 
        match unnamedDelayedCallerArgExprOpt with 
        | Some synArgExpr -> 
            match lambdaVars with 
            | Some [lambdaVars] -> 
                let argExpr,tpenv = TcExpr cenv (mkRefTupledVarsTy cenv.g lambdaVars) env tpenv synArgExpr 
                mkApps cenv.g ((expr,tyOfExpr cenv.g expr),[],[argExpr],mMethExpr), tpenv
            | _ -> 
                error(InternalError("unreachable - expected some lambda vars for a tuple mismatch",mItem))
        | None -> 
            expr, tpenv

    // Apply the PreBinders, if any 
    let expr = optArgPreBinder expr
    let expr = objArgPreBinder expr
    
    (expr,finalAttributeAssignedNamedItems,delayed),tpenv
            
and TcUnnamedMethodArgs cenv env lambdaPropagationInfo tpenv args =  
    List.mapiFoldSquared (TcUnnamedMethodArg cenv env) (lambdaPropagationInfo,tpenv) args 

and TcUnnamedMethodArg  cenv env  (lambdaPropagationInfo,tpenv) (i,j,CallerArg(argTy,mArg,isOpt,argExpr)) = 
    // Try to find the lambda propagation info for the corresponding unnamed argument at this position
    let lambdaPropagationInfoForArg = 
        [| for (unnamedInfo,_) in lambdaPropagationInfo -> 
             if i < unnamedInfo.Length && j < unnamedInfo.[i].Length then unnamedInfo.[i].[j] else NoInfo |] 
    TcMethodArg cenv env (lambdaPropagationInfo,tpenv) (lambdaPropagationInfoForArg,CallerArg(argTy,mArg,isOpt,argExpr))

and TcMethodNamedArgs cenv env lambdaPropagationInfo tpenv args  =  
    List.mapFoldSquared (TcMethodNamedArg cenv env) (lambdaPropagationInfo,tpenv) args

and TcMethodNamedArg cenv env (lambdaPropagationInfo,tpenv) (CallerNamedArg(id,arg)) = 
    // Try to find the lambda propagation info for the corresponding named argument
    let lambdaPropagationInfoForArg = 
        [| for (_,namedInfo) in lambdaPropagationInfo -> 
             namedInfo |> Array.tryPick (fun namedInfoForArgSet -> 
                namedInfoForArgSet |> Array.tryPick (fun (nm,info) -> 
                        if nm.idText = id.idText then Some info else None)) |]
        |> Array.map (fun x -> defaultArg x NoInfo)

    let arg',(lambdaPropagationInfo,tpenv) = TcMethodArg cenv env (lambdaPropagationInfo,tpenv) (lambdaPropagationInfoForArg,arg)
    CallerNamedArg(id,arg'),(lambdaPropagationInfo,tpenv)

and TcMethodArg  cenv env  (lambdaPropagationInfo,tpenv) (lambdaPropagationInfoForArg,CallerArg(argTy,mArg,isOpt,argExpr)) = 

    // Apply the F# 3.1 rule for extracting information for lambdas
    //
    // Before we check the argument, check to see if we can propagate info from a called lambda expression into the arguments of a received lambda
    begin
        if lambdaPropagationInfoForArg.Length > 0 then 
            let allOverloadsAreFuncOrMismatchForThisArg = 
                lambdaPropagationInfoForArg |> Array.forall (function ArgDoesNotMatch | CallerLambdaHasArgTypes _ -> true | NoInfo | CalledArgMatchesType _ -> false)

            if allOverloadsAreFuncOrMismatchForThisArg then 
              let overloadsWhichAreFuncAtThisPosition = lambdaPropagationInfoForArg |> Array.choose (function CallerLambdaHasArgTypes r -> Some (List.toArray r) | _ -> None)
              if overloadsWhichAreFuncAtThisPosition.Length > 0 then 
                let minFuncArity = overloadsWhichAreFuncAtThisPosition |> Array.minBy Array.length |> Array.length
                let prefixOfLambdaArgsForEachOverload = overloadsWhichAreFuncAtThisPosition |> Array.map (Array.take minFuncArity)
          
                if prefixOfLambdaArgsForEachOverload.Length > 0 then 
                    let numLambdaVars = prefixOfLambdaArgsForEachOverload.[0].Length
                    // Fold across the lambda var positions checking if all method overloads imply the same argument type for a lambda variable.
                    // If so, force the caller to have a function type that looks like the calledLambdaArgTy.
                    // The loop variable callerLambdaTyOpt becomes None if something failed.
                    let rec loop callerLambdaTy lambdaVarNum = 
                        if lambdaVarNum < numLambdaVars then
                            let col = [ for row in prefixOfLambdaArgsForEachOverload -> row.[lambdaVarNum] ]
                            // Check if all the rows give the same argument type
                            if col |> ListSet.setify (typeEquiv cenv.g) |> List.length |> ((=) 1) then 
                                let calledLambdaArgTy = col.[0]
                                // Force the caller to be a function type. 
                                match UnifyFunctionTypeUndoIfFailed cenv env.DisplayEnv mArg callerLambdaTy with 
                                | Some (callerLambdaDomainTy,callerLambdaRangeTy) ->
                                    if AddCxTypeEqualsTypeUndoIfFailed env.DisplayEnv cenv.css mArg calledLambdaArgTy callerLambdaDomainTy then 
                                        loop callerLambdaRangeTy (lambdaVarNum + 1)
                                | None -> ()
                    loop argTy 0
    end

    let e',tpenv = TcExpr cenv argTy env tpenv argExpr

    // After we have checked, propagate the info from argument into the overloads that receive it. 
    //
    // Filter out methods where an argument doesn't match. This just filters them from lambda propagation but not from 
    // later method overload resolution.
    let lambdaPropagationInfo = 
        [| for (info, argInfo) in Array.zip lambdaPropagationInfo lambdaPropagationInfoForArg do 
              match argInfo with 
              | ArgDoesNotMatch  _ -> ()
              | NoInfo  | CallerLambdaHasArgTypes _ -> 
                  yield info
              | CalledArgMatchesType adjustedCalledTy -> 
                  if AddCxTypeMustSubsumeTypeMatchingOnlyUndoIfFailed env.DisplayEnv cenv.css mArg adjustedCalledTy argTy then
                     yield info |]

    CallerArg(argTy,mArg,isOpt,e'),(lambdaPropagationInfo,tpenv)

/// Typecheck "new Delegate(fun x y z -> ...)" constructs
and TcNewDelegateThen cenv overallTy env tpenv mDelTy mExprAndArg delegateTy arg atomicFlag delayed =
    let ad = env.eAccessRights
    UnifyTypes cenv env mExprAndArg overallTy delegateTy
    let (SigOfFunctionForDelegate(invokeMethInfo,delArgTys,_,fty)) = GetSigOfFunctionForDelegate cenv.infoReader delegateTy mDelTy ad
    // We pass isInstance = true here because we're checking the rights to access the "Invoke" method
    MethInfoChecks cenv.g cenv.amap true None [] env.eAccessRights mExprAndArg invokeMethInfo
    let args = GetMethodArgs arg
    match args with 
    | [farg],[] -> 
        let m = arg.Range
        let callerArg,(_,tpenv) =  TcMethodArg cenv env (Array.empty,tpenv) (Array.empty,CallerArg(fty,m,false,farg))
        let expr = BuildNewDelegateExpr (None, cenv.g, cenv.amap, delegateTy, invokeMethInfo, delArgTys, callerArg.Expr, fty, m)
        PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprNoFlex cenv expr) delegateTy atomicFlag delayed  
    | _ ->  
        error(Error(FSComp.SR.tcDelegateConstructorMustBePassed(),mExprAndArg))


and bindLetRec (binds:Bindings) m e = 
    if isNil binds then 
        e 
    else 
        Expr.LetRec (binds,e,m,NewFreeVarsCache()) 

/// Check for duplicate bindings in simple recursive patterns
and CheckRecursiveBindingIds binds =
    let hashOfBinds = new Dictionary<string,_>()
            
    for (SynBinding.Binding(_,_,_,_,_,_,_,b,_,_,m,_)) in binds do
        let nm =
            match b with
            | SynPat.Named(_,id,_,_,_) -> id.idText
            | SynPat.LongIdent(LongIdentWithDots([id],_),_,_,_,_,_) -> id.idText
            | _ -> ""
        if nm <> "" then
            if hashOfBinds.ContainsKey(nm) then
                error(Duplicate("value",nm,m))
            else hashOfBinds.[nm] <- b

/// Process a sequence of seqeuntials mixed with iterated lets "let ... in let ... in ..." in a tail recursive way 
/// This avoids stack overflow on really large "let" and "letrec" lists
and TcLinearExprs bodyChecker cenv env overallTy tpenv isCompExpr expr cont = 
    match expr with 
    | SynExpr.Sequential (sp,true,e1,e2,m) when not isCompExpr ->
        let e1',_ = TcStmtThatCantBeCtorBody cenv env tpenv e1
        // tailcall
        TcLinearExprs bodyChecker cenv env overallTy tpenv isCompExpr e2 (fun (e2',tpenv) -> 
            cont (Expr.Sequential(e1',e2',NormalSeq,sp,m),tpenv))

    | SynExpr.LetOrUse (isRec,isUse,binds,body,m) when not (isUse && isCompExpr) ->
                
        if isRec then 
            // TcLinearExprs processes at most one recursive binding, this is not tailcalling
            CheckRecursiveBindingIds binds
            let binds = List.map (fun x -> RecDefnBindingInfo(ExprContainerInfo,NoNewSlots,ExpressionBinding,x)) binds
            if isUse then errorR(Error(FSComp.SR.tcBindingCannotBeUseAndRec(),m))
            let binds,envinner,tpenv = TcLetrec ErrorOnOverrides cenv env tpenv (binds,m,m)
            let bodyExpr,tpenv = bodyChecker overallTy envinner tpenv body 
            let bodyExpr = bindLetRec binds m bodyExpr
            cont (bodyExpr,tpenv)
        else 
            // TcLinearExprs processes multiple 'let' bindings in a tail recursive way
            let mkf,envinner,tpenv = TcLetBinding cenv isUse env ExprContainerInfo ExpressionBinding tpenv (binds,m,body.Range)
            TcLinearExprs bodyChecker cenv envinner overallTy tpenv isCompExpr body (fun (x,tpenv) -> 
                cont (fst (mkf (x,overallTy)), tpenv))
    | _ -> 
        cont (bodyChecker overallTy env tpenv expr)

/// Typecheck and compile pattern-matching constructs
and TcAndPatternCompileMatchClauses mExpr matchm actionOnFailure cenv inputTy resultTy env tpenv clauses =
    let tclauses, tpenv = TcMatchClauses cenv inputTy resultTy env tpenv clauses
    let v,expr = CompilePatternForMatchClauses cenv env mExpr matchm true actionOnFailure inputTy resultTy tclauses
    v,expr,tpenv

and TcMatchPattern cenv inputTy env tpenv (pat:SynPat,optWhenExpr) =
    let m = pat.Range
    let patf',(tpenv,names,_) = TcPat WarnOnUpperCase cenv env None (ValInline.Optional,permitInferTypars,noArgOrRetAttribs,false,None,false) (tpenv,Map.empty,Set.empty) inputTy pat
    let envinner,values,vspecMap = MakeAndPublishSimpleVals cenv env m names false
    let optWhenExpr',tpenv = Option.mapFold (TcExpr cenv cenv.g.bool_ty envinner) tpenv optWhenExpr
    patf' (TcPatPhase2Input (values, true)),optWhenExpr', NameMap.range vspecMap,envinner,tpenv

and TcMatchClauses cenv inputTy resultTy env tpenv clauses =
    List.mapFold (TcMatchClause cenv inputTy resultTy env) tpenv clauses 

and TcMatchClause cenv inputTy resultTy env tpenv (Clause(pat,optWhenExpr,e,patm,spTgt)) =
    let pat',optWhenExpr',vspecs,envinner,tpenv = TcMatchPattern cenv inputTy env tpenv (pat,optWhenExpr)
    let e',tpenv = TcExprThatCanBeCtorBody cenv resultTy envinner tpenv e
    TClause(pat',optWhenExpr',TTarget(vspecs, e',spTgt),patm),tpenv

and TcStaticOptimizationConstraint cenv env tpenv c = 
    match c with 
    | WhenTyparTyconEqualsTycon(tp,ty,m) ->
        if not cenv.g.compilingFslib then 
            errorR(Error(FSComp.SR.tcStaticOptimizationConditionalsOnlyForFSharpLibrary(),m))
        let ty',tpenv = TcType cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv ty
        let tp',tpenv = TcTypar cenv env NewTyparsOK tpenv tp
        TTyconEqualsTycon(mkTyparTy tp', ty'),tpenv
    | WhenTyparIsStruct(tp,m) ->
        if not cenv.g.compilingFslib then 
            errorR(Error(FSComp.SR.tcStaticOptimizationConditionalsOnlyForFSharpLibrary(),m))
        let tp',tpenv = TcTypar cenv env NewTyparsOK tpenv tp
        TTyconIsStruct(mkTyparTy tp'),tpenv

/// Emit a conv.i instruction
and mkConvToNativeInt (g:TcGlobals) e m = Expr.Op (TOp.ILAsm ([ AI_conv ILBasicType.DT_I], [ g.nativeint_ty ]),[],[e],m)

/// Fix up the r.h.s. of a 'use x = fixed expr' 
and TcAndBuildFixedExpr cenv env (overallPatTy, fixedExpr, overallExprTy, mBinding) =
    warning(PossibleUnverifiableCode(mBinding))
    match overallExprTy with 
    | ty when isByrefTy cenv.g ty -> 
        let okByRef = 
            match stripExpr fixedExpr  with 
            | Expr.Op (op,tyargs,args,_) ->
                    match op,tyargs,args with 
                    | TOp.ValFieldGetAddr rfref,_,[_] -> not rfref.Tycon.IsStructOrEnumTycon
                    | TOp.ILAsm ([ I_ldflda (fspec)],_),_,_  -> fspec.EnclosingType.Boxity = ILBoxity.AsObject
                    | TOp.ILAsm ([ I_ldelema _],_),_,_ -> true
                    | TOp.RefAddrGet _,_,_ -> true
                    | _ -> false
            | _ -> false
        if not okByRef then 
            error(Error(FSComp.SR.tcFixedNotAllowed(),mBinding))

        let elemTy = destByrefTy cenv.g overallExprTy
        UnifyTypes cenv env mBinding (mkNativePtrTy cenv.g elemTy) overallPatTy
        mkCompGenLetIn mBinding "pinnedByref" ty fixedExpr  (fun (v,ve) -> 
            v.SetIsFixed()
            mkConvToNativeInt cenv.g ve mBinding)
          
    | ty when isStringTy cenv.g ty -> 
        let charPtrTy = mkNativePtrTy cenv.g cenv.g.char_ty
        UnifyTypes cenv env mBinding charPtrTy overallPatTy
        //
        //    let ptr : nativeptr<char> = 
        //        let pinned s = str
        //        (nativeptr)s + get_OffsettoStringData()

        mkCompGenLetIn mBinding "pinnedString" cenv.g.string_ty fixedExpr (fun (v,ve) -> 
            v.SetIsFixed()
            let addrOffset = BuildOffsetToStringData cenv env mBinding
            let stringAsNativeInt = mkConvToNativeInt cenv.g ve mBinding
            let plusOffset = Expr.Op (TOp.ILAsm ([ AI_add ], [ cenv.g.nativeint_ty ]),[],[stringAsNativeInt; addrOffset],mBinding)
            // check for non-null
            mkNullTest cenv.g mBinding ve plusOffset ve)

    | ty when isArray1DTy cenv.g ty -> 
        let elemTy = destArrayTy cenv.g overallExprTy
        let elemPtrTy = mkNativePtrTy cenv.g elemTy
        UnifyTypes cenv env mBinding elemPtrTy overallPatTy

        // let ptr : nativeptr<elem> = 
        //   let tmpArray : elem[] = arr
        //   if nonNull tmpArray then
        //      if tmpArray.Length <> 0 then
        //         let pinned tmpArrayByref : byref<elem> = &arr.[0]
        //         (nativeint) tmpArrayByref
        //      else 
        //         (nativeint) 0
        //   else 
        //      (nativeint) 0
        //
        mkCompGenLetIn mBinding "tmpArray" overallExprTy fixedExpr (fun (_,ve) -> 
            // This is &arr.[0]
            let elemZeroAddress = mkArrayElemAddress cenv.g (ILReadonly.NormalAddress,false,ILArrayShape.SingleDimensional,elemTy,ve,mkInt32 cenv.g mBinding 0,mBinding)
            // check for non-null and non-empty
            let zero = mkConvToNativeInt cenv.g (mkInt32 cenv.g mBinding 0) mBinding  
            // This is arr.Length
            let arrayLengthExpr =  mkCallArrayLength  cenv.g mBinding elemTy ve 
            mkNullTest cenv.g mBinding ve 
                (mkNullTest cenv.g mBinding arrayLengthExpr 
                    (mkCompGenLetIn mBinding "pinnedByref" (mkByrefTy cenv.g elemTy) elemZeroAddress (fun (v,ve) -> 
                       v.SetIsFixed()
                       (mkConvToNativeInt cenv.g ve mBinding)))
                    zero) 
                zero)

    | _ -> error(Error(FSComp.SR.tcFixedNotAllowed(),mBinding))


/// Binding checking code, for all bindings including let bindings, let-rec bindings, member bindings and object-expression bindings and 
and TcNormalizedBinding declKind (cenv:cenv) env tpenv overallTy safeThisValOpt safeInitInfo (enclosingDeclaredTypars,(ExplicitTyparInfo(_,declaredTypars,_) as flex)) bind =
    let envinner = AddDeclaredTypars NoCheckForDuplicateTypars (enclosingDeclaredTypars@declaredTypars) env

    match bind with 

    | NormalizedBinding(vis,bkind,isInline,isMutable,attrs,doc,_,valSynData,pat,NormalizedBindingRhs(spatsL,rtyOpt,rhsExpr),mBinding,spBind) ->
        let (SynValData(memberFlagsOpt,valSynInfo,_)) = valSynData 

        let callerName = 
            match declKind, bkind, pat with
            | ExpressionBinding, _, _ -> envinner.eCallerMemberName
            | _, _, SynPat.Named(_,name,_,_,_) -> 
                match memberFlagsOpt with
                | Some(memberFlags) ->
                    match memberFlags.MemberKind with
                    | MemberKind.PropertyGet | MemberKind.PropertySet | MemberKind.PropertyGetSet -> Some(name.idText.Substring(4))
                    | MemberKind.ClassConstructor -> Some(".ctor")
                    | MemberKind.Constructor -> Some(".ctor")
                    | _ -> Some(name.idText)
                | _ -> Some(name.idText)
            | ClassLetBinding(false), DoBinding, _ -> Some(".ctor")
            | ClassLetBinding(true), DoBinding, _ -> Some(".cctor")
            | ModuleOrMemberBinding, StandaloneExpression, _ -> Some(".cctor")
            | _, _, _ -> envinner.eCallerMemberName

        let envinner = {envinner with eCallerMemberName = callerName }

        let attrTgt = DeclKind.AllowedAttribTargets memberFlagsOpt declKind 

        let isFixed,rhsExpr,overallPatTy,overallExprTy = 
            match rhsExpr with 
            | SynExpr.Fixed (e,_) -> true, e, NewInferenceType(), overallTy
            | e -> false, e, overallTy, overallTy

        // Check the attributes of the binding, parameters or return value
        let TcAttrs tgt attrs = 
            let attrs = TcAttributes cenv envinner tgt attrs 
            if attrTgt = enum 0 && not (isNil attrs) then 
                errorR(Error(FSComp.SR.tcAttributesAreNotPermittedOnLetBindings(),mBinding))
            attrs
            
        let valAttribs = TcAttrs attrTgt attrs
        let isVolatile = HasFSharpAttribute cenv.g cenv.g.attrib_VolatileFieldAttribute valAttribs
        
        let inlineFlag = ComputeInlineFlag memberFlagsOpt isInline isMutable mBinding

        let argAttribs = 
            spatsL |> List.map (SynInfo.InferSynArgInfoFromSimplePats >> List.map (SynInfo.AttribsOfArgData >> TcAttrs AttributeTargets.Parameter))
        let retAttribs = 
            match rtyOpt with 
            | Some (SynBindingReturnInfo(_,_,retAttrs)) -> TcAttrs AttributeTargets.ReturnValue retAttrs 
            | None -> [] 

        let argAndRetAttribs = ArgAndRetAttribs(argAttribs, retAttribs)

        if HasFSharpAttribute cenv.g cenv.g.attrib_DefaultValueAttribute valAttribs then 
            errorR(Error(FSComp.SR.tcDefaultValueAttributeRequiresVal(),mBinding))
        
        let isThreadStatic = isThreadOrContextStatic cenv.g valAttribs
        if isThreadStatic then errorR(DeprecatedThreadStaticBindingWarning(mBinding))

        if isVolatile then 
            match declKind with
            | ClassLetBinding(_) -> ()
            | _ -> errorR(Error(FSComp.SR.tcVolatileOnlyOnClassLetBindings(),mBinding))

            if (not isMutable || isThreadStatic) then 
                errorR(Error(FSComp.SR.tcVolatileFieldsMustBeMutable(),mBinding))

        if isFixed then 
            if declKind <> ExpressionBinding || isInline || isMutable then 
                errorR(Error(FSComp.SR.tcFixedNotAllowed(),mBinding))

        if HasFSharpAttributeOpt cenv.g cenv.g.attrib_DllImportAttribute valAttribs then 
            if not declKind.CanBeDllImport || (match memberFlagsOpt with Some memberFlags -> memberFlags.IsInstance | _ -> false) then 
                errorR(Error(FSComp.SR.tcDllImportNotAllowed(),mBinding))
            
        if HasFSharpAttribute cenv.g cenv.g.attrib_ConditionalAttribute valAttribs && Option.isNone memberFlagsOpt then 
            errorR(Error(FSComp.SR.tcConditionalAttributeRequiresMembers(),mBinding))

        if HasFSharpAttribute cenv.g cenv.g.attrib_EntryPointAttribute valAttribs then 
            if Option.isSome memberFlagsOpt then 
                errorR(Error(FSComp.SR.tcEntryPointAttributeRequiresFunctionInModule(),mBinding))
            else 
                UnifyTypes cenv env mBinding overallPatTy (mkArrayType cenv.g cenv.g.string_ty --> cenv.g.int_ty)

        if isMutable && isInline then errorR(Error(FSComp.SR.tcMutableValuesCannotBeInline(),mBinding))

        if isMutable && not (isNil declaredTypars) then errorR(Error(FSComp.SR.tcMutableValuesMayNotHaveGenericParameters(),mBinding))

        let flex = if isMutable then dontInferTypars else flex

        if isMutable && not (isNil spatsL) then errorR(Error(FSComp.SR.tcMutableValuesSyntax(),mBinding))

        let isInline = 
            if isInline && isNil spatsL && isNil declaredTypars then 
                errorR(Error(FSComp.SR.tcOnlyFunctionsCanBeInline(),mBinding))
                false
            else 
                isInline 

        let compgen = false
        
        // Use the syntactic arity if we're defining a function 
        let partialValReprInfo = TranslateTopValSynInfo mBinding (TcAttributes cenv env) valSynInfo

        // Check the pattern of the l.h.s. of the binding 
        let tcPatPhase2,(tpenv,nameToPrelimValSchemeMap,_) = 
            TcPat AllIdsOK cenv envinner (Some(partialValReprInfo)) (inlineFlag,flex,argAndRetAttribs,isMutable,vis,compgen) (tpenv,NameMap.empty,Set.empty) overallPatTy pat
        

        // Add active pattern result names to the environment 
        let apinfoOpt = 
            match NameMap.range nameToPrelimValSchemeMap with 
            | [PrelimValScheme1(id,_,ty,_,_,_,_,_,_,_,_) ] -> 
                match ActivePatternInfoOfValName id.idText id.idRange  with 
                | Some apinfo ->  Some (apinfo,ty, id.idRange)
                | None -> None
            | _ -> None

        // Add active pattern result names to the environment 
        let envinner = 
            match apinfoOpt with 
            | Some (apinfo,ty,m) ->
                if Option.isSome memberFlagsOpt || (not apinfo.IsTotal && apinfo.ActiveTags.Length > 1) then 
                    error(Error(FSComp.SR.tcInvalidActivePatternName(),mBinding))

                apinfo.ActiveTagsWithRanges |> List.iteri (fun i (_tag,tagRange) ->
                    let item = Item.ActivePatternResult(apinfo, cenv.g.unit_ty, i, tagRange)
                    CallNameResolutionSink cenv.tcSink (tagRange,env.NameEnv,item,item,ItemOccurence.Binding,env.DisplayEnv,env.eAccessRights))

                ModifyNameResEnv (fun nenv -> AddActivePatternResultTagsToNameEnv apinfo nenv ty m) envinner 
            | None -> 
                envinner
        
        // Now tc the r.h.s. 
        // If binding a ctor then set the ugly counter that permits us to write ctor expressions on the r.h.s. 
        let isCtor = (match memberFlagsOpt with Some memberFlags -> memberFlags.MemberKind = MemberKind.Constructor | _ -> false)

        // At each module binding, dive into the expression to check for syntax errors and suppress them if they show.
        // Don't do this for lambdas, because we always check for suppression for all lambda bodies in TcIteratedLambdas
        let rhsExprChecked,tpenv = 
            let atTopNonLambdaDefn = 
                DeclKind.IsModuleOrMemberOrExtensionBinding declKind && 
                (match rhsExpr with SynExpr.Lambda _ -> false | _ -> true) && 
                synExprContainsError rhsExpr

            conditionallySuppressErrorReporting atTopNonLambdaDefn (fun () -> 

                if isCtor then TcExprThatIsCtorBody (safeThisValOpt, safeInitInfo) cenv overallExprTy envinner tpenv rhsExpr
                else TcExprThatCantBeCtorBody cenv overallExprTy envinner tpenv rhsExpr)

        if bkind = StandaloneExpression && not cenv.isScript then 
            UnifyUnitType cenv env.DisplayEnv mBinding overallPatTy (Some rhsExprChecked) |> ignore<bool>

        // Fix up the r.h.s. expression for 'fixed'
        let rhsExprChecked =
            if isFixed then TcAndBuildFixedExpr cenv env (overallPatTy, rhsExprChecked, overallExprTy, mBinding)
            else rhsExprChecked

        // Assert the return type of an active pattern
        match apinfoOpt with 
        | Some (apinfo,ty,_) ->
            let activePatResTys = NewInferenceTypes apinfo.ActiveTags
            let _,rty = stripFunTy cenv.g ty
            UnifyTypes cenv env mBinding (apinfo.ResultType cenv.g rhsExpr.Range activePatResTys) rty
        | None -> 
            ()

        // Check other attributes
        let hasLiteralAttr,konst = TcLiteral cenv overallExprTy env tpenv (valAttribs,rhsExpr)
        if hasLiteralAttr && isThreadStatic then 
            errorR(Error(FSComp.SR.tcIllegalAttributesForLiteral(),mBinding))
        if hasLiteralAttr && isMutable then 
            errorR(Error(FSComp.SR.tcLiteralCannotBeMutable(),mBinding))
        if hasLiteralAttr && isInline then 
            errorR(Error(FSComp.SR.tcLiteralCannotBeInline(),mBinding))
        if hasLiteralAttr && not (isNil declaredTypars) then 
            errorR(Error(FSComp.SR.tcLiteralCannotHaveGenericParameters(),mBinding))

        CheckedBindingInfo(inlineFlag,valAttribs,doc,tcPatPhase2,flex,nameToPrelimValSchemeMap,rhsExprChecked,argAndRetAttribs,overallPatTy,mBinding,spBind,compgen,konst,isFixed),tpenv

and TcLiteral cenv overallTy env tpenv (attrs,synLiteralValExpr) = 
    let hasLiteralAttr = HasFSharpAttribute cenv.g cenv.g.attrib_LiteralAttribute attrs
    if hasLiteralAttr then 
        let literalValExpr,_ = TcExpr cenv overallTy env tpenv synLiteralValExpr
        match EvalLiteralExprOrAttribArg cenv.g literalValExpr with 
        | Expr.Const(c,_, ty) ->             
            if c = Const.Zero && isStructTy cenv.g ty then
                warning(Error(FSComp.SR.tcIllegalStructTypeForConstantExpression(), synLiteralValExpr.Range))
                false, None
            else
                true, Some c
        | _ -> 
            errorR(Error(FSComp.SR.tcInvalidConstantExpression(),synLiteralValExpr.Range))
            true, Some Const.Unit
    
    else hasLiteralAttr, None 
    
and TcBindingTyparDecls alwaysRigid cenv env tpenv (SynValTyparDecls(synTypars,infer,synTyparConstraints)) = 
    let declaredTypars = TcTyparDecls cenv env synTypars
    let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTypars env
    let tpenv = TcTyparConstraints cenv NoNewTypars CheckCxs ItemOccurence.UseInType envinner tpenv synTyparConstraints

    let rigidCopyOfDeclaredTypars = 
        if alwaysRigid then 
            declaredTypars |> List.iter (fun tp -> SetTyparRigid cenv.g env.DisplayEnv tp.Range tp)
            declaredTypars
        else
            let rigidCopyOfDeclaredTypars = copyTypars declaredTypars
            // The type parameters used to check rigidity after inference are marked rigid straight away
            rigidCopyOfDeclaredTypars |> List.iter (fun tp -> SetTyparRigid cenv.g env.DisplayEnv tp.Range tp)
            // The type parameters using during inference will be marked rigid after inference
            declaredTypars |> List.iter (fun tp -> tp.SetRigidity TyparRigidity.WillBeRigid)
            rigidCopyOfDeclaredTypars
            
    ExplicitTyparInfo(rigidCopyOfDeclaredTypars,declaredTypars,infer) , tpenv

and TcNonrecBindingTyparDecls cenv env tpenv bind = 
    let (NormalizedBinding(_,_,_,_,_,_,synTyparDecls,_,_,_,_,_)) = bind
    TcBindingTyparDecls true cenv env tpenv synTyparDecls

and TcNonRecursiveBinding declKind cenv env tpenv ty b =
    let b = BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env b
    let flex, tpenv = TcNonrecBindingTyparDecls cenv env tpenv b
    TcNormalizedBinding declKind cenv env tpenv ty None NoSafeInitInfo ([],flex) b 

//-------------------------------------------------------------------------
// TcAttribute*
//------------------------------------------------------------------------

and TcAttribute canFail cenv (env: TcEnv) attrTgt (synAttr: SynAttribute)  =
    let (LongIdentWithDots(tycon,_))= synAttr.TypeName
    let arg                       = synAttr.ArgExpr
    let targetIndicator           = synAttr.Target
    let isAppliedToGetterOrSetter = synAttr.AppliesToGetterAndSetter
    let mAttr                     = synAttr.Range
    let (typath,tyid) = List.frontAndBack tycon
    let tpenv = emptyUnscopedTyparEnv

    // if we're checking an attribute that was applied directly to a getter or a setter, then
    // what we're really checking against is a method, not a property
    let attrTgt = if isAppliedToGetterOrSetter then ((attrTgt ^^^ AttributeTargets.Property) ||| AttributeTargets.Method) else attrTgt
    let ty,tpenv =  
        let try1 n = 
            let tyid = mkSynId tyid.idRange n
            let tycon = (typath @ [tyid])
            let ad = env.eAccessRights
            match ResolveTypeLongIdent cenv.tcSink cenv.nameResolver ItemOccurence.UseInAttribute OpenQualified env.eNameResEnv ad tycon TypeNameResolutionStaticArgsInfo.DefiniteEmpty  PermitDirectReferenceToGeneratedType.No with
            | Exception err -> raze(err)
            | _ ->  success(TcTypeAndRecover cenv NoNewTypars CheckCxs ItemOccurence.UseInAttribute env tpenv (SynType.App(SynType.LongIdent(LongIdentWithDots(tycon,[])),None,[],[],None,false,mAttr)) )
        ForceRaise ((try1 (tyid.idText + "Attribute")) |> ResultOrException.otherwise (fun () -> (try1 tyid.idText)))

    let ad = env.eAccessRights

    if not (IsTypeAccessible cenv.g cenv.amap mAttr ad ty) then  errorR(Error(FSComp.SR.tcTypeIsInaccessible(),mAttr))

    let tcref = tcrefOfAppTy cenv.g ty

    let conditionalCallDefineOpt = TryFindTyconRefStringAttribute cenv.g mAttr cenv.g.attrib_ConditionalAttribute tcref 

    match conditionalCallDefineOpt with 
    | Some d when not (List.contains d cenv.conditionalDefines) -> 
        [], false
    | _ ->

         // REVIEW: take notice of inherited? 
        let validOn,_inherited = 
            let validOnDefault = 0x7fff
            let inheritedDefault = true
            if tcref.IsILTycon then 
                let tdef = tcref.ILTyconRawMetadata
                let tref = cenv.g.attrib_AttributeUsageAttribute.TypeRef
                
                match TryDecodeILAttribute cenv.g tref tdef.CustomAttrs with 
                | Some ([ILAttribElem.Int32 validOn ],named) -> 
                    let inherited = 
                        match List.tryPick (function ("Inherited",_,_,ILAttribElem.Bool res) -> Some res | _ -> None) named with 
                        | None -> inheritedDefault
                        | Some x -> x
                    (validOn, inherited)
                | Some ([ILAttribElem.Int32 validOn; ILAttribElem.Bool _allowMultiple; ILAttribElem.Bool inherited ],_) -> 
                    (validOn, inherited)
                | _ -> 
                    (validOnDefault, inheritedDefault)
            else
                match (TryFindFSharpAttribute cenv.g cenv.g.attrib_AttributeUsageAttribute tcref.Attribs) with
                | Some(Attrib(_,_,[ AttribInt32Arg(validOn) ],_,_,_,_)) ->
                    (validOn, inheritedDefault)
                | Some(Attrib(_,_,[ AttribInt32Arg(validOn)
                                    AttribBoolArg(_allowMultiple)
                                    AttribBoolArg(inherited)],_,_,_,_)) ->
                    (validOn, inherited)
                | Some _  ->
                    warning(Error(FSComp.SR.tcUnexpectedConditionInImportedAssembly(),mAttr))
                    (validOnDefault, inheritedDefault)                    
                | _ -> 
                    (validOnDefault, inheritedDefault)
        let possibleTgts = enum validOn &&& attrTgt
        let directedTgts = 
            match targetIndicator with
            | Some id when id.idText = "assembly" -> AttributeTargets.Assembly
            | Some id when id.idText = "module" -> AttributeTargets.Module
            | Some id when id.idText = "return" -> AttributeTargets.ReturnValue
            | Some id when id.idText = "field" -> AttributeTargets.Field
            | Some id when id.idText = "property" -> AttributeTargets.Property
            | Some id when id.idText = "method" -> AttributeTargets.Method
            | Some id when id.idText = "param" -> AttributeTargets.Parameter
            | Some id when id.idText = "type"    -> AttributeTargets.TyconDecl
            | Some id when id.idText = "constructor"    -> AttributeTargets.Constructor
            | Some id when id.idText = "event"    -> AttributeTargets.Event
            | Some id     -> 
                errorR(Error(FSComp.SR.tcUnrecognizedAttributeTarget(),id.idRange)) 
                possibleTgts
            | _ -> possibleTgts
        let constrainedTgts = possibleTgts &&& directedTgts
        if constrainedTgts = enum 0 then 
            if (directedTgts = AttributeTargets.Assembly || directedTgts = AttributeTargets.Module) then 
                error(Error(FSComp.SR.tcAttributeIsNotValidForLanguageElementUseDo(),mAttr))
            else
                error(Error(FSComp.SR.tcAttributeIsNotValidForLanguageElement(),mAttr))

        match ResolveObjectConstructor cenv.nameResolver env.DisplayEnv mAttr ad ty with 
        | Exception _ when canFail -> [ ], true
        | res -> 
        let item = ForceRaise res
        let attrib = 
            match item with 
            | Item.CtorGroup(methodName,minfos) ->
                let meths = minfos |> List.map (fun minfo -> minfo,None) 
                let afterTcOverloadResolution = AfterTcOverloadResolution.ForNewConstructors cenv.tcSink env tyid.idRange methodName minfos
                let (expr,namedCallerArgs,_),_ = 
                  TcMethodApplication true cenv env tpenv None [] mAttr mAttr methodName None ad PossiblyMutates false meths afterTcOverloadResolution NormalValUse [arg] (NewInferenceType ())  []

                UnifyTypes cenv env mAttr ty (tyOfExpr cenv.g expr)
                
                let mkAttribExpr e = 
                    AttribExpr(e,EvalLiteralExprOrAttribArg cenv.g e)

                let namedAttribArgMap = 
                  namedCallerArgs |> List.map (fun (CallerNamedArg(id,CallerArg(argtyv,m,isOpt,expr))) ->
                    if isOpt then error(Error(FSComp.SR.tcOptionalArgumentsCannotBeUsedInCustomAttribute(),m))
                    let m = expr.Range
                    let setterItem, _ = ResolveLongIdentInType cenv.tcSink cenv.nameResolver env.NameEnv LookupKind.Expr m ad [id] IgnoreOverrides TypeNameResolutionInfo.Default ty
                    let nm, isProp, argty = 
                      match setterItem with   
                      | Item.Property (_,[pinfo]) -> 
                          if not pinfo.HasSetter then 
                            errorR(Error(FSComp.SR.tcPropertyCannotBeSet0(),m))
                          id.idText, true, pinfo.GetPropertyType(cenv.amap,m) 
                      | Item.ILField finfo -> 
                          CheckILFieldInfoAccessible cenv.g cenv.amap m ad finfo
                          CheckILFieldAttributes cenv.g finfo m
                          id.idText,false, finfo.FieldType(cenv.amap, m)
                      | Item.RecdField rfinfo when not rfinfo.IsStatic -> 
                          CheckRecdFieldInfoAttributes cenv.g rfinfo m  |> CommitOperationResult        
                          CheckRecdFieldInfoAccessible cenv.amap m ad rfinfo
                          // This uses the F# backend name mangling of fields.... 
                          let nm =  ComputeFieldName rfinfo.Tycon rfinfo.RecdField
                          nm,false,rfinfo.FieldType
                      |  _ -> 
                          errorR(Error(FSComp.SR.tcPropertyOrFieldNotFoundInAttribute(),m)) 
                          id.idText,false,cenv.g.unit_ty
                    let propNameItem = Item.SetterArg(id, setterItem)
                    CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,propNameItem,propNameItem,ItemOccurence.Use,env.DisplayEnv,ad)

                    AddCxTypeMustSubsumeType ContextInfo.NoContext env.DisplayEnv cenv.css m NoTrace argty argtyv

                    AttribNamedArg(nm,argty,isProp,mkAttribExpr expr))

                match expr with 
                | Expr.Op(TOp.ILCall(_,_,valu,_,_,_,_,ilMethRef,[],[],_rtys),[],args,m) -> 
                    if valu then error (Error(FSComp.SR.tcCustomAttributeMustBeReferenceType(),m))
                    if args.Length <> ilMethRef.ArgTypes.Length then error (Error(FSComp.SR.tcCustomAttributeArgumentMismatch(),m))
                    let args = args |> List.map mkAttribExpr
                    Attrib(tcref,ILAttrib(ilMethRef),args,namedAttribArgMap,isAppliedToGetterOrSetter,Some constrainedTgts,m)

                | Expr.App((InnerExprPat(ExprValWithPossibleTypeInst(vref,_,_,_))),_,_,args,_) -> 
                    let args = args |> List.collect (function Expr.Const(Const.Unit,_,_) -> [] | expr -> tryDestRefTupleExpr expr)  |> List.map mkAttribExpr
                    Attrib(tcref,FSAttrib(vref),args,namedAttribArgMap,isAppliedToGetterOrSetter,Some constrainedTgts,mAttr)

                | _ -> 
                    error (Error(FSComp.SR.tcCustomAttributeMustInvokeConstructor(),mAttr))

            | _ -> 
                error(Error(FSComp.SR.tcAttributeExpressionsMustBeConstructorCalls(),mAttr))

        [ (constrainedTgts, attrib) ], false

and TcAttributesWithPossibleTargets canFail cenv env attrTgt synAttribs = 

    (false,synAttribs) ||> List.collectFold (fun didFail synAttrib -> 
        try 
            let attribsAndTargets, didFail2 = TcAttribute canFail cenv env attrTgt synAttrib
            
            // This is where we place any checks that completely exclude the use of some particular 
            // attributes from F#.
            let attribs = List.map snd attribsAndTargets
            if HasFSharpAttribute cenv.g cenv.g.attrib_TypeForwardedToAttribute attribs ||
               HasFSharpAttribute cenv.g cenv.g.attrib_CompilationArgumentCountsAttribute attribs ||
               HasFSharpAttribute cenv.g cenv.g.attrib_CompilationMappingAttribute attribs then 
                errorR(Error(FSComp.SR.tcUnsupportedAttribute(),synAttrib.Range))

            attribsAndTargets, didFail || didFail2

        with e -> 
            errorRecovery e synAttrib.Range 
            [], false) 

and TcAttributesMaybeFail canFail cenv env attrTgt synAttribs = 
    let attribsAndTargets, didFail = TcAttributesWithPossibleTargets canFail cenv env attrTgt synAttribs 
    attribsAndTargets |> List.map snd, didFail

and TcAttributesCanFail cenv env attrTgt synAttribs = 
    let attrs, didFail = TcAttributesMaybeFail true cenv env attrTgt synAttribs
    attrs, (fun () -> if didFail then TcAttributes cenv env attrTgt synAttribs else attrs)

and TcAttributes cenv env attrTgt synAttribs = 
    TcAttributesMaybeFail false cenv env attrTgt synAttribs |> fst

//-------------------------------------------------------------------------
// TcLetBinding
//------------------------------------------------------------------------

and TcLetBinding cenv isUse env containerInfo declKind tpenv (binds,bindsm,scopem) =

    // Typecheck all the bindings...
    let binds',tpenv = List.mapFold (fun tpenv b -> TcNonRecursiveBinding declKind cenv env tpenv (NewInferenceType ()) b) tpenv binds
    let (ContainerInfo(altActualParent,_)) = containerInfo
    
    // Canonicalize constraints prior to generalization 
    let denv = env.DisplayEnv
    GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,bindsm) 
        (binds' |> List.collect (fun tbinfo -> 
            let (CheckedBindingInfo(_,_,_,_,flex,_,_,_,tauTy,_,_,_,_,_)) = tbinfo
            let (ExplicitTyparInfo(_,declaredTypars,_)) = flex
            let maxInferredTypars = (freeInTypeLeftToRight cenv.g false tauTy)
            declaredTypars @ maxInferredTypars))

    let lazyFreeInEnv = lazy (GeneralizationHelpers.ComputeUngeneralizableTypars env)

    // Generalize the bindings...
    (((fun x -> x), env, tpenv), binds') ||> List.fold (fun (mkf_sofar,env,tpenv) tbinfo -> 
        let (CheckedBindingInfo(inlineFlag,attrs,doc,tcPatPhase2,flex,nameToPrelimValSchemeMap,rhsExpr,_,tauTy,m,spBind,_,konst,isFixed)) = tbinfo
        let enclosingDeclaredTypars  = []
        let (ExplicitTyparInfo(_,declaredTypars,canInferTypars)) = flex
        let allDeclaredTypars  =  enclosingDeclaredTypars @ declaredTypars
        let generalizedTypars,prelimValSchemes2 = 
            let canInferTypars = GeneralizationHelpers. ComputeCanInferExtraGeneralizableTypars (containerInfo.ParentRef, canInferTypars, None)

            let maxInferredTypars = freeInTypeLeftToRight cenv.g false tauTy

            let generalizedTypars = 
                if isNil maxInferredTypars && isNil allDeclaredTypars then 
                   [] 
                else 
                   let freeInEnv = lazyFreeInEnv.Force()
                   GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv, m, freeInEnv, canInferTypars, GeneralizationHelpers.CanGeneralizeConstrainedTyparsForDecl(declKind), inlineFlag, Some rhsExpr, allDeclaredTypars, maxInferredTypars,tauTy,false)

            let prelimValSchemes2 = GeneralizeVals cenv denv enclosingDeclaredTypars  generalizedTypars nameToPrelimValSchemeMap

            generalizedTypars,prelimValSchemes2

        // REVIEW: this scopes generalized type variables. Ensure this is handled properly 
        // on all other paths. 
        let tpenv = HideUnscopedTypars generalizedTypars tpenv
        let valSchemes = NameMap.map (UseCombinedArity cenv.g declKind rhsExpr) prelimValSchemes2
        let values = MakeAndPublishVals cenv env (altActualParent,false,declKind,ValNotInRecScope,valSchemes,attrs,doc,konst)
        let pat' = tcPatPhase2 (TcPatPhase2Input (values, true))
        let prelimRecValues = NameMap.map fst values
        
        // Now bind the r.h.s. to the l.h.s. 
        let rhsExpr = mkTypeLambda m generalizedTypars (rhsExpr,tauTy)

        match pat' with 
        // Don't introduce temporary or 'let' for 'match against wild' or 'match against unit' 

        | (TPat_wild _ | TPat_const (Const.Unit,_)) when not isUse && not isFixed && isNil generalizedTypars ->
            let mk_seq_bind (tm,tmty) = (mkSequential SequencePointsAtSeq m rhsExpr tm, tmty)
            (mk_seq_bind << mkf_sofar,env,tpenv)
        | _ -> 

        // nice: don't introduce awful temporary for r.h.s. in the 99% case where we know what we're binding it to 
        let tmp,pat'' = 
                match pat' with 
                // nice: don't introduce awful temporary for r.h.s. in the 99% case where we know what we're binding it to 
                | TPat_as (pat1,PBind(v,TypeScheme(generalizedTypars',_)),_) 
                    when List.lengthsEqAndForall2 typarRefEq generalizedTypars generalizedTypars' -> 
                      
                      v, pat1

                | _ when inlineFlag.MustInline  -> error(Error(FSComp.SR.tcInvalidInlineSpecification(),m))

                | _ -> 
                    let tmp,_ = mkCompGenLocal m "patternInput" (generalizedTypars +-> tauTy)
                    if isUse || isFixed then 
                        errorR(Error(FSComp.SR.tcInvalidUseBinding(),m))
                    
                    // This assignment forces representation as module value, to maintain the invariant from the 
                    // type checker that anything related to binding module-level values is marked with an 
                    // val_repr_info, val_actual_parent and is_topbind
                    if (DeclKind.MustHaveArity declKind) then 
                        AdjustValToTopVal tmp altActualParent (InferArityOfExprBinding cenv.g tmp rhsExpr)
                    tmp,pat'

        let mkRhsBind (bodyExpr,bodyExprTy) = 
            let letExpr = mkLet spBind m tmp rhsExpr bodyExpr
            letExpr,bodyExprTy

        let allValsDefinedByPattern = NameMap.range prelimRecValues
        let mkPatBind (bodyExpr,bodyExprTy) =
            let valsDefinedByMatching = ListSet.remove valEq tmp allValsDefinedByPattern
            let matchx = CompilePatternForMatch cenv env m m true ThrowIncompleteMatchException (tmp,generalizedTypars) [TClause(pat'',None,TTarget(valsDefinedByMatching,bodyExpr,SuppressSequencePointAtTarget),m)] tauTy bodyExprTy
            let matchx = if (DeclKind.ConvertToLinearBindings declKind) then LinearizeTopMatch cenv.g altActualParent matchx else matchx
            matchx,bodyExprTy

        let mkCleanup (bodyExpr,bodyExprTy) =
            if isUse && not isFixed then 
                (allValsDefinedByPattern,(bodyExpr,bodyExprTy)) ||> List.foldBack (fun v (bodyExpr,bodyExprTy) ->
                    AddCxTypeMustSubsumeType ContextInfo.NoContext denv cenv.css v.Range NoTrace cenv.g.system_IDisposable_typ v.Type
                    let cleanupE = BuildDisposableCleanup cenv env m v
                    mkTryFinally cenv.g (bodyExpr,cleanupE,m,bodyExprTy,SequencePointInBodyOfTry,NoSequencePointAtFinally),bodyExprTy)
            else 
                (bodyExpr,bodyExprTy)
                
        ((mkf_sofar >> mkCleanup  >> mkPatBind  >> mkRhsBind),
         AddLocalValMap cenv.tcSink scopem prelimRecValues env,
         tpenv))

/// Return binds corresponding to the linearised let-bindings.
/// This reveals the bound items, e.g. when the lets occur in incremental object defns.
/// RECAP:
///   The LHS of let-bindings are patterns.
///   These patterns could fail, e.g. "let Some x = ...".
///   So letbindings could contain a fork at a match construct, with one branch being the match failure.
///   If bindings are linearised, then this fork is pushed to the RHS.
///   In this case, the let bindings type check to a sequence of bindings.
and TcLetBindings cenv env containerInfo declKind tpenv (binds,bindsm,scopem) =
    assert(DeclKind.ConvertToLinearBindings declKind)
    let mkf,env,tpenv = TcLetBinding cenv false env containerInfo declKind tpenv (binds,bindsm,scopem)
    let unite = mkUnit cenv.g bindsm
    let expr,_ = mkf (unite,cenv.g.unit_ty)
    let rec stripLets acc = function
        | Expr.Let (bind,body,m,_)      ->  stripLets (TMDefLet(bind,m) :: acc) body
        | Expr.Sequential (e1,e2,NormalSeq,_,m)      ->  stripLets (TMDefDo(e1,m) :: acc) e2
        | Expr.Const (Const.Unit,_,_) -> List.rev acc
        | _ -> failwith "TcLetBindings: let sequence is non linear. Maybe a LHS pattern was not linearised?"
    let binds = stripLets [] expr
    binds,env,tpenv

and CheckMemberFlags _g optIntfSlotTy newslotsOK overridesOK memberFlags m = 
    if newslotsOK = NoNewSlots && memberFlags.IsDispatchSlot then 
      errorR(Error(FSComp.SR.tcAbstractMembersIllegalInAugmentation(),m))
    if overridesOK = ErrorOnOverrides && memberFlags.MemberKind = MemberKind.Constructor then 
      errorR(Error(FSComp.SR.tcConstructorsIllegalInAugmentation(),m))
    if overridesOK = WarnOnOverrides && memberFlags.IsOverrideOrExplicitImpl && Option.isNone optIntfSlotTy then 
      warning(OverrideInIntrinsicAugmentation(m))
    if overridesOK = ErrorOnOverrides && memberFlags.IsOverrideOrExplicitImpl then 
      error(Error(FSComp.SR.tcMethodOverridesIllegalHere(),m))
    
/// Apply the pre-assumed knowledge available to type inference prior to looking at 
/// the _body_ of the binding. For example, in a letrec we may assume this knowledge 
/// for each binding in the letrec prior to any type inference. This might, for example, 
/// tell us the type of the arguments to a recursive function. 
and ApplyTypesFromArgumentPatterns (cenv, env, optArgsOK, ty, m, tpenv, NormalizedBindingRhs (pushedPats, retInfoOpt, e), memberFlagsOpt:MemberFlags option) =  
    match pushedPats with
    | [] ->
        match retInfoOpt with 
        | None -> ()
        | Some (SynBindingReturnInfo (retInfoTy, m, _)) -> 
            let retInfoTy,_ = TcTypeAndRecover cenv NewTyparsOK CheckCxs ItemOccurence.UseInType env tpenv retInfoTy
            UnifyTypes cenv env m ty retInfoTy
        // Property setters always have "unit" return type
        match memberFlagsOpt with 
        | Some memFlags when memFlags.MemberKind = MemberKind.PropertySet -> 
            UnifyTypes cenv env m ty cenv.g.unit_ty
        | _ -> ()
            
    | pushedPat :: morePushedPats -> 
        let domainTy,resultTy = UnifyFunctionType None cenv env.DisplayEnv m ty
        // We apply the type information from the patterns by type checking the
        // "simple" patterns against 'domainTy'. They get re-typechecked later. 
        ignore (TcSimplePats cenv optArgsOK CheckCxs domainTy env (tpenv,Map.empty,Set.empty) pushedPat)
        ApplyTypesFromArgumentPatterns (cenv, env, optArgsOK, resultTy, m, tpenv, NormalizedBindingRhs (morePushedPats, retInfoOpt, e), memberFlagsOpt)


/// Do the type annotations give the full and complete generic type? If so, enable generic recursion 
and ComputeIsComplete enclosingDeclaredTypars declaredTypars ty = 
    Zset.isEmpty (List.fold (fun acc v -> Zset.remove v acc) 
                                  (freeInType CollectAllNoCaching ty).FreeTypars 
                                  (enclosingDeclaredTypars@declaredTypars)) 


/// Determine if a uniquely-identified-abstract-slot exists for an override member (or interface member implementation) based on the information available 
/// at the syntactic definition of the member (i.e. prior to type inference). If so, we know the expected signature of the override, and the full slotsig 
/// it implements. Apply the inferred slotsig. 
and ApplyAbstractSlotInference (cenv:cenv) (envinner:TcEnv) (bindingTy,m,synTyparDecls,declaredTypars,memberId,tcrefObjTy,renaming,_objTy,optIntfSlotTy,valSynData,memberFlags,attribs) = 

    let ad = envinner.eAccessRights
    let typToSearchForAbstractMembers = 
        match optIntfSlotTy with 
        | Some (ty, abstractSlots) -> 
            // The interface type is in terms of the type's type parameters. 
            // We need a signature in terms of the values' type parameters. 
            ty,Some(abstractSlots) 
        | None -> 
            tcrefObjTy,None

    // Determine if a uniquely-identified-override exists based on the information 
    // at the member signature. If so, we know the type of this member, and the full slotsig 
    // it implements. Apply the inferred slotsig. 
    if memberFlags.IsOverrideOrExplicitImpl then 
        
        // for error detection, we want to compare finality when testing for equivalence
        let makeUniqueBySig meths = meths |> ListSet.setify (MethInfosEquivByNameAndSig EraseNone false cenv.g cenv.amap m)
        match memberFlags.MemberKind with 
        | MemberKind.Member -> 
             let dispatchSlots,dispatchSlotsArityMatch = 
                 GetAbstractMethInfosForSynMethodDecl(cenv.infoReader,ad,memberId,m,typToSearchForAbstractMembers,valSynData)

             let uniqueAbstractMethSigs = 
                 match dispatchSlots with 
                 | [] -> 
                     errorR(Error(FSComp.SR.tcNoMemberFoundForOverride(),memberId.idRange))
                     []

                 | slots -> 
                     match dispatchSlotsArityMatch with 
                     | meths when meths |> makeUniqueBySig |> List.length = 1 -> meths
                     | [] -> 
                         let details =
                             slots
                             |> List.map (NicePrint.stringOfMethInfo cenv.amap m envinner.DisplayEnv)
                             |> Seq.map (sprintf "%s   %s" System.Environment.NewLine)
                             |> String.concat ""

                         errorR(Error(FSComp.SR.tcOverrideArityMismatch(details),memberId.idRange))
                         []
                     | _ -> [] // check that method to override is sealed is located at CheckOverridesAreAllUsedOnce (typrelns.fs)
                      // We hit this case when it is ambiguous which abstract method is being implemented. 

               
             
             // If we determined a unique member then utilize the type information from the slotsig 
             let declaredTypars = 
                 match uniqueAbstractMethSigs with 
                 | uniqueAbstractMeth :: _ -> 

                     let uniqueAbstractMeth = uniqueAbstractMeth.Instantiate(cenv.amap, m, renaming)
                     
                     let typarsFromAbsSlotAreRigid,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot = 
                         FreshenAbstractSlot cenv.g cenv.amap m synTyparDecls uniqueAbstractMeth

                     let declaredTypars = (if typarsFromAbsSlotAreRigid then typarsFromAbsSlot else declaredTypars)

                     let absSlotTy = mkMethodTy cenv.g argTysFromAbsSlot retTyFromAbsSlot

                     UnifyTypes cenv envinner m bindingTy absSlotTy
                     declaredTypars
                 | _ -> declaredTypars 

                 // Retained to ensure use of an FSComp.txt entry, can be removed at a later date: errorR(Error(FSComp.SR.tcDefaultAmbiguous(),memberId.idRange))

             // What's the type containing the abstract slot we're implementing? Used later on in MakeMemberDataAndMangledNameForMemberVal. 
             // This type must be in terms of the enclosing type's formal type parameters, hence the application of revRenaming 

             let optInferredImplSlotTys = 
                 match optIntfSlotTy with 
                 | Some (x,_) -> [x]
                 | None -> uniqueAbstractMethSigs |> List.map (fun x -> x.EnclosingType)

             optInferredImplSlotTys,declaredTypars

        | MemberKind.PropertyGet 
        | MemberKind.PropertySet as k ->
           let dispatchSlots = GetAbstractPropInfosForSynPropertyDecl(cenv.infoReader,ad,memberId,m,typToSearchForAbstractMembers,k,valSynData)

           // Only consider those abstract slots where the get/set flags match the value we're defining 
           let dispatchSlots = 
               dispatchSlots 
               |> List.filter (fun pinfo -> 
                     (pinfo.HasGetter && k=MemberKind.PropertyGet) ||
                     (pinfo.HasSetter && k=MemberKind.PropertySet))
                                       
           // Find the unique abstract slot if it exists 
           let uniqueAbstractPropSigs = 
               match dispatchSlots with 
               | [] when not (CompileAsEvent cenv.g attribs) -> 
                   errorR(Error(FSComp.SR.tcNoPropertyFoundForOverride(),memberId.idRange)) 
                   []
               | [uniqueAbstractProp] -> [uniqueAbstractProp]
               | _ -> 
                   // We hit this case when it is ambiguous which abstract property is being implemented. 
                   []

           // If we determined a unique member then utilize the type information from the slotsig 
           uniqueAbstractPropSigs |> List.iter (fun uniqueAbstractProp -> 

               let kIsGet = (k = MemberKind.PropertyGet)

               if not (if kIsGet then uniqueAbstractProp.HasGetter else uniqueAbstractProp.HasSetter) then 
                   error(Error(FSComp.SR.tcAbstractPropertyMissingGetOrSet(if kIsGet then "getter" else "setter"),memberId.idRange))

               let uniqueAbstractMeth = if kIsGet then uniqueAbstractProp.GetterMethod else uniqueAbstractProp.SetterMethod

               let uniqueAbstractMeth = uniqueAbstractMeth.Instantiate(cenv.amap, m, renaming)

               let _,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot = 
                    FreshenAbstractSlot cenv.g cenv.amap m synTyparDecls uniqueAbstractMeth

               if not (isNil typarsFromAbsSlot) then 
                   errorR(InternalError("Unexpected generic property",memberId.idRange))

               let absSlotTy = 
                   if (memberFlags.MemberKind = MemberKind.PropertyGet) 
                   then mkMethodTy cenv.g argTysFromAbsSlot retTyFromAbsSlot 
                   else 
                     match argTysFromAbsSlot with 
                     | [argTysFromAbsSlot] -> mkRefTupledTy cenv.g argTysFromAbsSlot --> cenv.g.unit_ty
                     | _ -> 
                         error(Error(FSComp.SR.tcInvalidSignatureForSet(),memberId.idRange)) 
                         retTyFromAbsSlot --> cenv.g.unit_ty

               UnifyTypes cenv envinner m bindingTy absSlotTy)
           
           // What's the type containing the abstract slot we're implementing? Used later on in MakeMemberDataAndMangledNameForMemberVal. 
           // This type must be in terms of the enclosing type's formal type parameters, hence the application of revRenaming.
           
           let optInferredImplSlotTys = 
               match optIntfSlotTy with 
               | Some (x,_) -> [ x ]
               | None -> uniqueAbstractPropSigs |> List.map (fun pinfo -> pinfo.EnclosingType) 

           optInferredImplSlotTys,declaredTypars

        | _ -> 
           match optIntfSlotTy with 
           | Some (x,_) -> [x], declaredTypars 
           | None -> [], declaredTypars

    else

       [], declaredTypars 

and CheckForNonAbstractInterface declKind tcref memberFlags m =
    if isInterfaceTyconRef tcref then 
        if memberFlags.MemberKind = MemberKind.ClassConstructor then 
            error(Error(FSComp.SR.tcStaticInitializersIllegalInInterface(),m))
        elif memberFlags.MemberKind = MemberKind.Constructor then 
            error(Error(FSComp.SR.tcObjectConstructorsIllegalInInterface(),m))
        elif memberFlags.IsOverrideOrExplicitImpl then 
            error(Error(FSComp.SR.tcMemberOverridesIllegalInInterface(),m))
        elif not (declKind=ExtrinsicExtensionBinding || memberFlags.IsDispatchSlot ) then
            error(Error(FSComp.SR.tcConcreteMembersIllegalInInterface(),m))

//-------------------------------------------------------------------------
// TcLetrec - AnalyzeAndMakeAndPublishRecursiveValue(s)
//------------------------------------------------------------------------

and AnalyzeRecursiveStaticMemberOrValDecl (cenv, envinner: TcEnv, tpenv, declKind, newslotsOK, overridesOK, tcrefContainerInfo, vis1, id:Ident, vis2, declaredTypars, memberFlagsOpt, thisIdOpt, bindingAttribs, valSynInfo, ty, bindingRhs, mBinding, flex) =
    let vis = CombineVisibilityAttribs vis1 vis2 mBinding

    // Check if we're defining a member, in which case generate the internal unique 
    // name for the member and the information about which type it is agumenting 
      
    match tcrefContainerInfo, memberFlagsOpt with 
    | (Some(MemberOrValContainerInfo(tcref, optIntfSlotTy, baseValOpt, _safeInitInfo, declaredTyconTypars)),Some memberFlags) -> 
        assert (Option.isNone optIntfSlotTy)
      
        CheckMemberFlags cenv.g None newslotsOK overridesOK memberFlags id.idRange
        CheckForNonAbstractInterface declKind tcref memberFlags id.idRange

        if tcref.Deref.IsExceptionDecl && 
           (memberFlags.MemberKind = MemberKind.Constructor) then 
            error(Error(FSComp.SR.tcConstructorsDisallowedInExceptionAugmentation(),id.idRange))                  

        let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
        let _,enclosingDeclaredTypars,_,objTy,thisTy = FreshenObjectArgType cenv mBinding TyparRigidity.WillBeRigid tcref isExtrinsic declaredTyconTypars
        let envinner = AddDeclaredTypars CheckForDuplicateTypars enclosingDeclaredTypars envinner
        let envinner = MakeInnerEnvForTyconRef cenv envinner tcref isExtrinsic 

        let safeThisValOpt, baseValOpt = 
            match memberFlags.MemberKind with 
            
            // Explicit struct or class constructor
            | MemberKind.Constructor  ->
                // A fairly adhoc place to put this check 
                if tcref.IsStructOrEnumTycon && (match valSynInfo with SynValInfo([[]],_) -> true | _ -> false) then
                    errorR(Error(FSComp.SR.tcStructsCannotHaveConstructorWithNoArguments(),mBinding))

                if not tcref.IsFSharpObjectModelTycon then 
                    errorR(Error(FSComp.SR.tcConstructorsIllegalForThisType(),id.idRange))

                let safeThisValOpt = MakeAndPublishSafeThisVal cenv envinner thisIdOpt thisTy
                  
                // baseValOpt is the 'base' variable associated with the inherited portion of a class 
                // It is declared once on the 'inheritedTys clause, but a fresh binding is made for 
                // each member that may use it. 
                let baseValOpt = 
                    match GetSuperTypeOfType cenv.g cenv.amap mBinding objTy with 
                    | Some superTy -> MakeAndPublishBaseVal cenv envinner (match baseValOpt with None -> None | Some v -> Some v.Id) superTy 
                    | None -> None

                let domainTy = NewInferenceType ()

                // This is the type we pretend a constructor has, because its implementation must ultimately appear to return a value of the given type 
                // This is somewhat awkward later in codegen etc. 
                UnifyTypes cenv envinner mBinding ty (domainTy --> objTy)

                safeThisValOpt, baseValOpt
                
            | _ -> 
                None,None
          
        let memberInfo = 
            let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
            MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,isExtrinsic,bindingAttribs,[],memberFlags,valSynInfo,id,false)

        envinner,tpenv,id,None,Some(memberInfo),vis,vis2,safeThisValOpt,enclosingDeclaredTypars,baseValOpt,flex,bindingRhs,declaredTypars
        
    // non-member bindings. How easy. 
    | _ -> 
        envinner,tpenv,id,None,None,vis,vis2,None,[],None,flex,bindingRhs,declaredTypars
    

and AnalyzeRecursiveInstanceMemberDecl (cenv,envinner: TcEnv, tpenv, declKind, synTyparDecls, valSynInfo, flex:ExplicitTyparInfo, newslotsOK, overridesOK, vis1, thisId, memberId:Ident, toolId:Ident option, bindingAttribs, vis2, tcrefContainerInfo, memberFlagsOpt, ty, bindingRhs, mBinding) =
    let vis = CombineVisibilityAttribs vis1 vis2 mBinding
    let (ExplicitTyparInfo(_,declaredTypars,infer)) = flex
    match tcrefContainerInfo,memberFlagsOpt with 
     // Normal instance members. 
     | Some(MemberOrValContainerInfo(tcref, optIntfSlotTy, baseValOpt, _safeInitInfo, declaredTyconTypars)), Some memberFlags -> 
       
         CheckMemberFlags cenv.g optIntfSlotTy newslotsOK overridesOK memberFlags mBinding

         if Option.isSome vis && memberFlags.IsOverrideOrExplicitImpl then 
            errorR(Error(FSComp.SR.tcOverridesCannotHaveVisibilityDeclarations(),memberId.idRange))

         // Syntactically push the "this" variable across to be a lambda on the right 
         let bindingRhs = PushOnePatternToRhs cenv true (mkSynThisPatVar thisId) bindingRhs
       
         // The type being augmented tells us the type of 'this' 
         let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
         let tcrefObjTy,enclosingDeclaredTypars,renaming,objTy,thisTy = FreshenObjectArgType cenv mBinding TyparRigidity.WillBeRigid tcref isExtrinsic declaredTyconTypars

         let envinner = AddDeclaredTypars CheckForDuplicateTypars enclosingDeclaredTypars envinner

         // If private, the member's accessibility is related to 'tcref' 
         let envinner = MakeInnerEnvForTyconRef cenv envinner tcref isExtrinsic 

         let baseValOpt = if tcref.IsFSharpObjectModelTycon then baseValOpt else None

         // Apply the known type of 'this' 
         let bindingTy = NewInferenceType ()
         UnifyTypes cenv envinner mBinding ty (thisTy --> bindingTy)

         CheckForNonAbstractInterface declKind tcref memberFlags memberId.idRange 
         
         // Determine if a uniquely-identified-override List.exists based on the information 
         // at the member signature. If so, we know the type of this member, and the full slotsig 
         // it implements. Apply the inferred slotsig. 
         let optInferredImplSlotTys, declaredTypars = 
             ApplyAbstractSlotInference cenv envinner (bindingTy,mBinding,synTyparDecls,declaredTypars,memberId,tcrefObjTy,renaming,objTy,optIntfSlotTy,valSynInfo,memberFlags,bindingAttribs)

         // Update the ExplicitTyparInfo to reflect the declaredTypars inferred from the abstract slot 
         let flex = ExplicitTyparInfo(declaredTypars,declaredTypars,infer)

         // baseValOpt is the 'base' variable associated with the inherited portion of a class 
         // It is declared once on the 'inheritedTys clause, but a fresh binding is made for 
         // each member that may use it. 
         let baseValOpt = 
             match GetSuperTypeOfType cenv.g cenv.amap mBinding objTy with 
             | Some(superTy) -> MakeAndPublishBaseVal cenv envinner (match baseValOpt with None -> None | Some v -> Some v.Id) superTy 
             | None -> None

         let memberInfo = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,isExtrinsic,bindingAttribs,optInferredImplSlotTys,memberFlags,valSynInfo,memberId,false)
         // This line factored in the 'get' or 'set' as the identifier for a property declaration using "with get () = ... and set v = ..."
         // It has been removed from FSharp.Compiler.Service because we want the property name to be the location of
         // the definition of these symbols.
         //
         // See https://github.com/fsharp/FSharp.Compiler.Service/issues/79.
         //let memberId = match toolId with Some tid -> ident(memberId.idText, tid.idRange) | None -> memberId
         //ignore toolId

         envinner, tpenv, memberId, toolId, Some memberInfo, vis, vis2, None, enclosingDeclaredTypars, baseValOpt, flex, bindingRhs, declaredTypars
     | _ -> 
         error(Error(FSComp.SR.tcRecursiveBindingsWithMembersMustBeDirectAugmentation(),mBinding)) 

and AnalyzeRecursiveDecl (cenv,envinner,tpenv,declKind,synTyparDecls,declaredTypars,thisIdOpt,valSynInfo,flex,newslotsOK,overridesOK,vis1,declPattern,bindingAttribs,tcrefContainerInfo,memberFlagsOpt,ty,bindingRhs,mBinding) =
    let rec analyzeRecursiveDeclPat tpenv p = 
        match p with  
        | SynPat.FromParseError(pat',_) -> analyzeRecursiveDeclPat tpenv pat'
        | SynPat.Typed(pat',cty,_) -> 
            let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs ItemOccurence.UseInType envinner tpenv cty
            UnifyTypes cenv envinner mBinding ty cty'
            analyzeRecursiveDeclPat tpenv pat' 
        | SynPat.Attrib(_pat',_attribs,m) -> 
            error(Error(FSComp.SR.tcAttributesInvalidInPatterns(),m))
            //analyzeRecursiveDeclPat pat' 

        // This is for the construct 'let rec x = ... and do ... and y = ...' (DEPRECATED IN pars.mly )
        //
        // Also for 
        //    module rec M = 
        //        printfn "hello" // side effects in recursive modules
        //        let x = 1
        | SynPat.Const (SynConst.Unit, m) | SynPat.Wild m -> 
             let id = ident (cenv.niceNameGen.FreshCompilerGeneratedName("doval",m),m)
             analyzeRecursiveDeclPat tpenv (SynPat.Named (SynPat.Wild m, id, false, None, m))
             
        | SynPat.Named (SynPat.Wild _, id,_,vis2,_) -> 
            AnalyzeRecursiveStaticMemberOrValDecl (cenv,envinner,tpenv,declKind,newslotsOK,overridesOK,tcrefContainerInfo,vis1,id,vis2,declaredTypars,memberFlagsOpt,thisIdOpt,bindingAttribs,valSynInfo,ty,bindingRhs,mBinding,flex)
            
        | SynPat.InstanceMember(thisId,memberId,toolId,vis2,_) -> 
            AnalyzeRecursiveInstanceMemberDecl (cenv,envinner,tpenv,declKind,synTyparDecls,valSynInfo,flex,newslotsOK,overridesOK,vis1,thisId,memberId,toolId,bindingAttribs,vis2,tcrefContainerInfo,memberFlagsOpt,ty,bindingRhs,mBinding)

        | _ -> error(Error(FSComp.SR.tcOnlySimplePatternsInLetRec(),mBinding))

    analyzeRecursiveDeclPat tpenv declPattern


/// This is a major routine that generates the Val for a recursive binding 
/// prior to the analysis of the definition of the binding. This includes
/// members of all flavours (including properties, implicit class constructors
/// and overrides). At this point we perform override inference, to infer
/// which method we are overriding, in order to add constraints to the
/// implementation of the method.
and AnalyzeAndMakeAndPublishRecursiveValue overridesOK isGeneratedEventVal cenv (env:TcEnv) (tpenv,recBindIdx) (NormalizedRecBindingDefn(containerInfo,newslotsOK,declKind,binding)) =

    // Pull apart the inputs
    let (NormalizedBinding(vis1,bindingKind,isInline,isMutable,bindingSynAttribs,bindingXmlDoc,synTyparDecls,valSynData,declPattern,bindingRhs,mBinding,spBind)) = binding
    let (NormalizedBindingRhs(_,_,bindingExpr)) = bindingRhs
    let (SynValData(memberFlagsOpt,valSynInfo,thisIdOpt)) = valSynData 
    let (ContainerInfo(altActualParent,tcrefContainerInfo)) = containerInfo
    
    let attrTgt = DeclKind.AllowedAttribTargets memberFlagsOpt declKind 

    // Check the attributes on the declaration
    let bindingAttribs = TcAttributes cenv env attrTgt bindingSynAttribs

    // Allocate the type inference variable for the inferred type
    let ty = NewInferenceType () 
        
        
    let inlineFlag = ComputeInlineFlag memberFlagsOpt isInline isMutable mBinding
    if isMutable then errorR(Error(FSComp.SR.tcOnlyRecordFieldsAndSimpleLetCanBeMutable(),mBinding))


    // Typecheck the typar decls, if any
    let flex, tpenv = TcBindingTyparDecls false cenv env tpenv synTyparDecls
    let (ExplicitTyparInfo(_,declaredTypars,_)) = flex
    let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTypars env
    
    // OK, analyze the declaration and return lots of information about it
    let envinner,tpenv,bindingId,toolIdOpt,memberInfoOpt,vis,vis2,safeThisValOpt,enclosingDeclaredTypars,baseValOpt,flex,bindingRhs,declaredTypars = 

        AnalyzeRecursiveDecl (cenv, envinner, tpenv, declKind, synTyparDecls, declaredTypars, thisIdOpt, valSynInfo, flex,
                              newslotsOK, overridesOK, vis1, declPattern, bindingAttribs, tcrefContainerInfo,
                              memberFlagsOpt, ty, bindingRhs, mBinding)

    let optArgsOK = Option.isSome memberFlagsOpt

    // Assert the types given in the argument patterns
    ApplyTypesFromArgumentPatterns(cenv,envinner,optArgsOK,ty,mBinding,tpenv,bindingRhs,memberFlagsOpt)

    // Do the type annotations give the full and complete generic type? 
    // If so, generic recursion can be used when using this type. 
    let isComplete =  ComputeIsComplete enclosingDeclaredTypars declaredTypars ty
    
    // NOTE: The type scheme here is normally not 'complete'!!!! The type is more or less just a type variable at this point. 
    // NOTE: toparity, type and typars get fixed-up after inference 
    let prelimTyscheme = TypeScheme(enclosingDeclaredTypars@declaredTypars,ty)
    let partialValReprInfo = TranslateTopValSynInfo mBinding (TcAttributes cenv envinner) valSynInfo
    let topValInfo = UseSyntacticArity declKind prelimTyscheme partialValReprInfo
    let hasDeclaredTypars = declaredTypars.Length > 0
    let prelimValScheme = ValScheme(bindingId,prelimTyscheme,topValInfo,memberInfoOpt,false,inlineFlag,NormalVal,vis,false,false,false,hasDeclaredTypars)

    // Check the literal r.h.s., if any
    let _, konst = TcLiteral cenv ty env tpenv (bindingAttribs,bindingExpr)

    let extraBindings,extraValues,tpenv,recBindIdx = 
       let extraBindings = 
          [ for extraBinding in EventDeclarationNormalization.GenerateExtraBindings cenv (bindingAttribs,binding) do
               yield (NormalizedRecBindingDefn(containerInfo,newslotsOK,declKind,extraBinding)) ]
       let res,(tpenv,recBindIdx) = List.mapFold (AnalyzeAndMakeAndPublishRecursiveValue overridesOK true cenv env) (tpenv,recBindIdx) extraBindings
       let extraBindings, extraValues = List.unzip res
       List.concat extraBindings, List.concat extraValues, tpenv,recBindIdx
    
    // Create the value 
    let vspec = MakeAndPublishVal cenv envinner (altActualParent,false,declKind,ValInRecScope(isComplete),prelimValScheme,bindingAttribs,bindingXmlDoc,konst,isGeneratedEventVal)

    // Suppress hover tip for "get" and "set" at property definitions, where toolId <> bindingId
    match toolIdOpt with 
    | Some tid when not tid.idRange.IsSynthetic && tid.idRange <> bindingId.idRange  -> 
        let item = Item.Value (mkLocalValRef vspec)
        CallNameResolutionSink cenv.tcSink (tid.idRange,env.NameEnv,item,item,ItemOccurence.RelatedText,env.DisplayEnv,env.eAccessRights)
    | _ -> ()

    
    let mangledId = ident(vspec.LogicalName,vspec.Range)
    // Reconstitute the binding with the unique name
    let revisedBinding = NormalizedBinding (vis1,bindingKind,isInline,isMutable,bindingSynAttribs,bindingXmlDoc,synTyparDecls,valSynData,mkSynPatVar vis2 mangledId,bindingRhs,mBinding,spBind)

    // Create the RBInfo to use in later phases
    let rbinfo = 
        let safeInitInfo = 
            match tcrefContainerInfo with 
            | Some(MemberOrValContainerInfo(_, _, _, safeInitInfo, _)) -> safeInitInfo
            | _ -> NoSafeInitInfo
        
        RBInfo(recBindIdx,containerInfo,enclosingDeclaredTypars,inlineFlag,vspec,flex,partialValReprInfo,memberInfoOpt,baseValOpt,safeThisValOpt,safeInitInfo,vis,ty,declKind)

    let recBindIdx = recBindIdx + 1

    // Done - add the declared name to the List.map and return the bundle for use by TcLetrec 
    let primaryBinding : PreCheckingRecursiveBinding = 
        { SyntacticBinding = revisedBinding
          RecBindingInfo = rbinfo }

    ((primaryBinding::extraBindings),(vspec::extraValues)),(tpenv,recBindIdx)


and AnalyzeAndMakeAndPublishRecursiveValues overridesOK cenv env tpenv binds = 
    let recBindIdx = 0
    let res,tpenv = List.mapFold (AnalyzeAndMakeAndPublishRecursiveValue overridesOK false cenv env) (tpenv,recBindIdx) binds
    let bindings, values = List.unzip res
    List.concat bindings, List.concat values, tpenv


//-------------------------------------------------------------------------
// TcLetrecBinding
//-------------------------------------------------------------------------

and TcLetrecBinding 
         (cenv, envRec: TcEnv, scopem, extraGeneralizableTypars: Typars, reqdThisValTyOpt: TType option)
         
         // The state of the left-to-right iteration through the bindings
         (envNonRec: TcEnv, 
          generalizedRecBinds : PostGeneralizationRecursiveBinding list,
          preGeneralizationRecBinds: PreGeneralizationRecursiveBinding list,
          tpenv,
          uncheckedRecBindsTable : Map<Stamp,PreCheckingRecursiveBinding>) 
         
         // This is the actual binding to check
         (rbind : PreCheckingRecursiveBinding) = 

    let (RBInfo(_,_,enclosingDeclaredTypars,_,vspec,flex,_,_,baseValOpt,safeThisValOpt,safeInitInfo,_,tau,declKind)) = rbind.RecBindingInfo
    
    let allDeclaredTypars = enclosingDeclaredTypars @ rbind.RecBindingInfo.DeclaredTypars

    // Notes on FSharp 1.0, 3187:
    //    - Progressively collect the "eligible for early generalization" set of bindings  -- DONE
    //    - After checking each binding, check this set to find generalizable bindings
    //    - The only reason we can't generalize is if a binding refers to type variables to which 
    //      additional constraints may be applied as part of checking a later binding
    //    - Compute the set by iteratively knocking out bindings that refer to type variables free in later bindings
    //    - Implementation notes:
    //         - Generalize by remap/substitution
    //         - Pass in "free in later bindings" by passing in the set of inference variables for the bindings, i.e. the binding types
    //         - For classes the bindings will include all members in a recursive group of types
    //
    
    //  Example 1: 
    //    let f() = g()   f : unit -> ?b
    //    and g() = 1     f : unit -> int, can generalize (though now monomorphic)

    //  Example 2: 
    //    let f() = g()   f : unit -> ?b
    //    and g() = []    f : unit -> ?c list, can generalize
    
    //  Example 3: 
    //    let f() = []   f : unit -> ?b, can generalize immediately
    //    and g() = []
    let envRec = Option.foldBack (AddLocalVal cenv.tcSink scopem) baseValOpt envRec
    let envRec = Option.foldBack (AddLocalVal cenv.tcSink scopem) safeThisValOpt envRec

    // Members can access protected members of parents of the type, and private members in the type 
    let envRec = MakeInnerEnvForMember cenv envRec vspec 

    let checkedBind,tpenv = 
        TcNormalizedBinding declKind cenv envRec tpenv tau safeThisValOpt safeInitInfo (enclosingDeclaredTypars,flex) rbind.SyntacticBinding

    (try UnifyTypes cenv envRec vspec.Range (allDeclaredTypars +-> tau) vspec.Type 
     with e -> error (Recursion(envRec.DisplayEnv,vspec.Id,tau,vspec.Type,vspec.Range)))

    // Inside the incremental class sytntax we assert the type of the 'this' variable to be precisely the same type as the 
    // this variable for the implicit class constructor. For static members, we assert the type variables associated
    // for the class to be identical to those used for the implicit class constructor and the static class constructor.
    match reqdThisValTyOpt with 
    | None -> ()          
    | Some reqdThisValTy -> 
        let reqdThisValTy, actualThisValTy, rangeForCheck =
            match GetInstanceMemberThisVariable (vspec, checkedBind.Expr) with
            | None -> 
                let reqdThisValTy = if isByrefTy cenv.g reqdThisValTy then destByrefTy cenv.g reqdThisValTy else reqdThisValTy
                let enclosingTyconRef = tcrefOfAppTy cenv.g reqdThisValTy
                reqdThisValTy, (mkAppTy enclosingTyconRef (List.map mkTyparTy enclosingDeclaredTypars)), vspec.Range
            | Some thisVal -> 
                reqdThisValTy, thisVal.Type, thisVal.Range
        if not (AddCxTypeEqualsTypeUndoIfFailed envRec.DisplayEnv cenv.css rangeForCheck actualThisValTy reqdThisValTy) then 
            errorR (Error(FSComp.SR.tcNonUniformMemberUse vspec.DisplayName,vspec.Range))

    let preGeneralizationRecBind =  
        { RecBindingInfo = rbind.RecBindingInfo 
          CheckedBinding= checkedBind
          ExtraGeneralizableTypars= extraGeneralizableTypars }

    // Remove one binding from the unchecked list
    let uncheckedRecBindsTable = 
        assert (uncheckedRecBindsTable.ContainsKey rbind.RecBindingInfo.Val.Stamp)
        uncheckedRecBindsTable.Remove rbind.RecBindingInfo.Val.Stamp
    
    // Add one binding to the candidates eligible for generalization
    let preGeneralizationRecBinds = (preGeneralizationRecBind::preGeneralizationRecBinds)

    // Incrementally generalize as many bindings as we can
    TcIncrementalLetRecGeneralization cenv scopem (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, tpenv, uncheckedRecBindsTable) 

and TcIncrementalLetRecGeneralization cenv scopem
         // The state of the left-to-right iteration through the bindings
         (envNonRec: TcEnv, 
          generalizedRecBinds : PostGeneralizationRecursiveBinding list,
          preGeneralizationRecBinds: PreGeneralizationRecursiveBinding list,
          tpenv,
          uncheckedRecBindsTable : Map<Stamp,PreCheckingRecursiveBinding>) =

    let denv = envNonRec.DisplayEnv
    // recompute the free-in-environment in case any type variables have been instantiated
    let freeInEnv = GeneralizationHelpers.ComputeUngeneralizableTypars envNonRec

    // Attempt to actually generalize some of the candidates eligible for generalization.
    // Compute which bindings are now eligible for early generalization.
    // Do this by computing a greatest fixed point by iteratively knocking out bindings that refer 
    // to type variables free in later bindings. Look for ones whose type doesn't involve any of the other types
    let newGeneralizedRecBinds,preGeneralizationRecBinds, tpenv = 

        //printfn  "\n---------------------\nConsidering early generalization after type checking binding %s" vspec.DisplayName

        // Get the type variables free in bindings that have not yet been checked.
        // 
        // The naive implementation of this is to iterate all the forward bindings, but this is quadratic.
        //
        // It turns out we can remove the quadratic behaviour as follows. 
        // -    During type checking we already keep a table of recursive uses of values, indexed by target value. 
        // -    This table is usually much smaller than the number of remaining forward declarations ? e.g. in the pathological case you mentioned below this table is size 1. 
        // -    If a forward declaration does not have an entry in this table then its type can't involve any inference variables from the declarations we have already checked. 
        // -    So by scanning the domain of this table we can reduce the complexity down to something like O(n * average-number-of-forward-calls). 
        // -    For a fully connected programs or programs where every forward declaration is subject to a forward call, this would be quadratic. However we do not expect callgraphs to be like this in practice
        // 
        // Hence we use the recursive-uses table to guide the process of scraping forward references for frozen types
        // If the is no entry in the recursive use table then a forward binding has never been used and
        // the type of a binding will not contain any inference variables.
        //
        // We do this lazily in case it is "obvious" that a binding can be generalized (e.g. its type doesn't
        // involve any type inference variables)
        //
        // The forward uses table will always be smaller than the number of potential forward bindings except in extremely
        // pathological situations
        let freeInUncheckedRecBinds = 
            lazy ((emptyFreeTyvars, cenv.recUses.Contents) ||> Map.fold (fun acc vStamp _ -> 
                       if uncheckedRecBindsTable.ContainsKey vStamp then 
                           let fwdBind = uncheckedRecBindsTable.[vStamp]  
                           accFreeInType CollectAllNoCaching  fwdBind.RecBindingInfo.Val.Type acc
                       else
                           acc))

        let rec loop (preGeneralizationRecBinds: PreGeneralizationRecursiveBinding list,
                      frozenBindings: PreGeneralizationRecursiveBinding list) = 

            let frozenBindingTypes = frozenBindings |> List.map (fun pgrbind -> pgrbind.RecBindingInfo.Val.Type) 

            let freeInFrozenAndLaterBindings = 
                if frozenBindingTypes.IsEmpty then  
                    freeInUncheckedRecBinds
                else 
                    lazy (accFreeInTypes CollectAllNoCaching frozenBindingTypes (freeInUncheckedRecBinds.Force()))

            let preGeneralizationRecBinds,newFrozenBindings = 

                preGeneralizationRecBinds |> List.partition (fun pgrbind ->

                    //printfn "(testing binding %s)" pgrbind.RecBindingInfo.Val.DisplayName

                    // Get the free type variables in the binding
                    //
                    // We use the TauType here because the binding may already have been pre-generalized because it has
                    // a fully type-annotated type signature. We effectively want to generalize the binding 
                    // again here, properly - for example this means adjusting the expression for the binding to include
                    // a Expr_tlambda. If we use Val.Type then the type will appear closed.
                    let freeInBinding = (freeInType CollectAllNoCaching pgrbind.RecBindingInfo.Val.TauType).FreeTypars
                    
                    // Is the binding free of type inference variables? If so, it can be generalized immediately
                    if freeInBinding.IsEmpty then true else

                    //printfn "(failed generalization test 1 for binding for %s)" pgrbind.RecBindingInfo.Val.DisplayName
                    // Any declared type parameters in an type are always generalizable
                    let freeInBinding = Zset.diff  freeInBinding (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.ExtraGeneralizableTypars))

                    if freeInBinding.IsEmpty then true else

                    //printfn "(failed generalization test 2 for binding for %s)" pgrbind.RecBindingInfo.Val.DisplayName

                    // Any declared method parameters can always be generalized
                    let freeInBinding = Zset.diff  freeInBinding (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.RecBindingInfo.DeclaredTypars))

                    if freeInBinding.IsEmpty then true else

                    //printfn "(failed generalization test 3 for binding for %s)" pgrbind.RecBindingInfo.Val.DisplayName

                    // Type variables free in the non-recursive environment do not stop us generalizing the binding,
                    // since they can't be generalized anyway
                    let freeInBinding = Zset.diff  freeInBinding freeInEnv

                    if freeInBinding.IsEmpty then true else

                    //printfn "(failed generalization test 4 for binding for %s)" pgrbind.RecBindingInfo.Val.DisplayName

                    // Type variables free in unchecked bindings do stop us generalizing
                    let freeInBinding = Zset.inter (freeInFrozenAndLaterBindings.Force().FreeTypars) freeInBinding

                    if freeInBinding.IsEmpty then true else

                    //printfn "(failed generalization test 5 for binding for %s)" pgrbind.RecBindingInfo.Val.DisplayName
                    
                    false)
                    //if canGeneralize then 
                    //    printfn "YES: binding for %s can be generalized early" pgrbind.RecBindingInfo.Val.DisplayName
                    //else 
                    //    printfn "NO: binding for %s can't be generalized early" pgrbind.RecBindingInfo.Val.DisplayName

            // Have we reached a fixed point?
            if newFrozenBindings.IsEmpty then 
                preGeneralizationRecBinds,frozenBindings
            else
                // if not, then repeat
                loop(preGeneralizationRecBinds,newFrozenBindings@frozenBindings)
            
        // start with no frozen bindings
        let newGeneralizableBindings,preGeneralizationRecBinds = loop(preGeneralizationRecBinds,[])
        
        // Some of the bindings may now have been marked as 'generalizable' (which means they now transition
        // from PreGeneralization --> PostGeneralization, since we won't get any more information on 
        // these bindings by processing later bindings). But this doesn't mean we 
        // actually generalize all the individual type variables occuring in these bindings - for example, some 
        // type variables may be free in the environment, and some definitions
        // may be value definitions which can't be generalized, e.g. 
        //   let rec f x = g x 
        //   and g = id f 
        // Here the type variables in 'g' can't be generalized because it's a computation on the right.
        //
        // Note that in the normal case each binding passes IsGeneralizableValue. Properties and 
        // constructors do not pass CanInferExtraGeneralizedTyparsForRecBinding.

        let freeInEnv =
            (freeInEnv,newGeneralizableBindings) ||> List.fold (fun freeInEnv pgrbind -> 
                if GeneralizationHelpers.IsGeneralizableValue cenv.g pgrbind.CheckedBinding.Expr then 
                    freeInEnv 
                else 
                    let freeInBinding = (freeInType CollectAllNoCaching pgrbind.RecBindingInfo.Val.TauType).FreeTypars
                    let freeInBinding = Zset.diff  freeInBinding (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.ExtraGeneralizableTypars))
                    let freeInBinding = Zset.diff  freeInBinding (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.RecBindingInfo.DeclaredTypars))
                    Zset.union freeInBinding freeInEnv)

        // Process the bindings marked for transition from PreGeneralization --> PostGeneralization
        let newGeneralizedRecBinds,tpenv = 
            if newGeneralizableBindings.IsEmpty then 
                [], tpenv
            else
                
                let supportForBindings = newGeneralizableBindings |> List.collect (TcLetrecComputeSupportForBinding cenv)
                GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,scopem) supportForBindings 
                 
                let generalizedTyparsL = newGeneralizableBindings |> List.map (TcLetrecComputeAndGeneralizeGenericTyparsForBinding cenv denv freeInEnv) 
                
                // Generalize the bindings. 
                let newGeneralizedRecBinds = (generalizedTyparsL,newGeneralizableBindings) ||> List.map2 (TcLetrecGeneralizeBinding cenv denv ) 
                let tpenv = HideUnscopedTypars (List.concat generalizedTyparsL) tpenv
                newGeneralizedRecBinds,tpenv
        
    
        newGeneralizedRecBinds, preGeneralizationRecBinds, tpenv

    let envNonRec = envNonRec |> AddLocalVals cenv.tcSink scopem (newGeneralizedRecBinds |> List.map (fun b -> b.RecBindingInfo.Val))  
    let generalizedRecBinds = newGeneralizedRecBinds @ generalizedRecBinds        

    (envNonRec,generalizedRecBinds,preGeneralizationRecBinds,tpenv,uncheckedRecBindsTable)

//-------------------------------------------------------------------------
// TcLetrecComputeAndGeneralizeGenericTyparsForBinding
//-------------------------------------------------------------------------

/// Compute the type variables which may be generalized and perform the generalization 
and TcLetrecComputeAndGeneralizeGenericTyparsForBinding cenv denv freeInEnv (pgrbind : PreGeneralizationRecursiveBinding)  =

    let freeInEnv = Zset.diff freeInEnv (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.ExtraGeneralizableTypars))

    let rbinfo = pgrbind.RecBindingInfo
    let vspec = rbinfo.Val
    let (CheckedBindingInfo(inlineFlag,_,_,_,_,_,expr,_,_,m,_,_,_,_)) = pgrbind.CheckedBinding
    let (ExplicitTyparInfo(rigidCopyOfDeclaredTypars,declaredTypars,_)) = rbinfo.ExplicitTyparInfo
    let allDeclaredTypars = rbinfo.EnclosingDeclaredTypars @ declaredTypars

     
    // The declared typars were not marked rigid to allow equi-recursive type inference to unify
    // two declared type variables. So we now check that, for each binding, the declared
    // type variables can be unified with a rigid version of the same and undo the results
    // of this unification.
    ConstraintSolver.CheckDeclaredTypars denv cenv.css m rigidCopyOfDeclaredTypars declaredTypars 

    let memFlagsOpt = vspec.MemberInfo |> Option.map (fun memInfo -> memInfo.MemberFlags)
    let isCtor = (match memFlagsOpt with None -> false | Some memberFlags -> memberFlags.MemberKind = MemberKind.Constructor)

    GeneralizationHelpers.CheckDeclaredTyparsPermitted(memFlagsOpt,declaredTypars,m)
    let canInferTypars = CanInferExtraGeneralizedTyparsForRecBinding pgrbind

    let tau = vspec.TauType
    let maxInferredTypars = freeInTypeLeftToRight cenv.g false tau

    let canGeneralizeConstrained = GeneralizationHelpers.CanGeneralizeConstrainedTyparsForDecl rbinfo.DeclKind
    let generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars (cenv,denv,m,freeInEnv,canInferTypars,canGeneralizeConstrained,inlineFlag, Some(expr), allDeclaredTypars, maxInferredTypars,tau,isCtor)
    generalizedTypars

/// Compute the type variables which may have member constraints that need to be canonicalized prior to generalization 
and TcLetrecComputeSupportForBinding cenv (pgrbind : PreGeneralizationRecursiveBinding) =
    let rbinfo = pgrbind.RecBindingInfo
    let allDeclaredTypars = rbinfo.EnclosingDeclaredTypars @ rbinfo.DeclaredTypars
    let maxInferredTypars = freeInTypeLeftToRight cenv.g false rbinfo.Val.TauType
    allDeclaredTypars @ maxInferredTypars

//-------------------------------------------------------------------------
// TcLetrecGeneralizeBinding
//------------------------------------------------------------------------

// Generalise generalizedTypars from checkedBind.
and TcLetrecGeneralizeBinding cenv denv generalizedTypars (pgrbind : PreGeneralizationRecursiveBinding) : PostGeneralizationRecursiveBinding =

    let (RBInfo(_,_,enclosingDeclaredTypars,_,vspec,flex,partialValReprInfo,memberInfoOpt,_,_,_,vis,_,declKind)) = pgrbind.RecBindingInfo
    let (CheckedBindingInfo(inlineFlag,_,_,_,_,_,expr,argAttribs,_,_,_,compgen,_,isFixed)) = pgrbind.CheckedBinding
     
    if isFixed then 
        errorR(Error(FSComp.SR.tcFixedNotAllowed(),expr.Range))

    let _,tau = vspec.TypeScheme

    let pvalscheme1 = PrelimValScheme1(vspec.Id,flex,tau,Some(partialValReprInfo),memberInfoOpt,false,inlineFlag,NormalVal,argAttribs,vis,compgen)
    let pvalscheme2 = GeneralizeVal cenv denv enclosingDeclaredTypars generalizedTypars pvalscheme1

    let valscheme = UseCombinedArity cenv.g declKind expr pvalscheme2 
    AdjustRecType cenv vspec valscheme

    { ValScheme = valscheme
      CheckedBinding = pgrbind.CheckedBinding
      RecBindingInfo = pgrbind.RecBindingInfo }


and TcLetrecComputeCtorSafeThisValBind cenv safeThisValOpt =
    match safeThisValOpt with 
    | None -> None
    | Some (v:Val) -> 
        let m = v.Range
        let ty = destRefCellTy cenv.g v.Type
        Some (mkCompGenBind v (mkRefCell cenv.g m ty (mkNull m ty)))

and MakeCheckSafeInitField g tinst thisValOpt rfref reqExpr (expr:Expr) =
    let m = expr.Range
    let availExpr =
        match thisValOpt with 
        | None -> mkStaticRecdFieldGet (rfref, tinst, m)
        | Some thisVar -> 
            // This is an instance method, it must have a 'this' var
            mkRecdFieldGetViaExprAddr (exprForVal m thisVar, rfref, tinst, m)
    let failureExpr = match thisValOpt with None -> mkCallFailStaticInit g m | Some _ -> mkCallFailInit g m
    mkCompGenSequential m (mkIfThen g m (mkILAsmClt g m availExpr reqExpr) failureExpr) expr

and MakeCheckSafeInit g tinst safeInitInfo reqExpr expr =
    match safeInitInfo with 
    | SafeInitField (rfref, _) -> MakeCheckSafeInitField g tinst None rfref reqExpr expr
    | NoSafeInitInfo -> expr

// Given a method binding (after generalization)
//
//    method M = (fun <tyargs> <args> -> <body>)
// 
// wrap the following around it if needed
//
//    method M = (fun <tyargs> baseVal <args> -> 
//                      check ctorSafeInitInfo 
//                      let ctorSafeThisVal = ref null
//                      <body>)
//
// The "check ctorSafeInitInfo" is only added for non-constructor instance members in a class where at least one type in the 
// hierarchy has HasSelfReferentialConstructor
//
// The "let ctorSafeThisVal = ref null" is only added for explicit constructors with a self-reference parameter (Note: check later code for exact conditions)
// For implicit constructors the binding is added to the bindings of the implicit constructor

and TcLetrecAdjustMemberForSpecialVals cenv (pgrbind: PostGeneralizationRecursiveBinding) : PostBindCtorThisVarRefCellRecursiveBinding =
 
    let (RBInfo(_,_,_,_,vspec,_,_,_,baseValOpt,safeThisValOpt,safeInitInfo,_,_,_)) = pgrbind.RecBindingInfo
    let expr = pgrbind.CheckedBinding.Expr
    let spBind = pgrbind.CheckedBinding.SeqPoint
      
    let expr = 
        match TcLetrecComputeCtorSafeThisValBind cenv safeThisValOpt with 
        | None -> expr
        | Some bind -> 
            let m = expr.Range
            let tps,vsl,body,returnTy = stripTopLambda (expr,vspec.Type)
            mkMultiLambdas m tps vsl (mkLetBind m bind body, returnTy)

    // Add a call to CheckInit if necessary for instance members
    let expr = 
        if vspec.IsInstanceMember && not vspec.IsExtensionMember && not vspec.IsConstructor then
            match safeInitInfo with 
            | SafeInitField (rfref, _) ->  
                let m = expr.Range
                let tps,vsl,body,returnTy = stripTopLambda (expr,vspec.Type)
                // This is an instance member, it must have a 'this'
                let thisVar = vsl.Head.Head
                let thisTypeInst = argsOfAppTy cenv.g thisVar.Type
                let newBody = MakeCheckSafeInitField cenv.g thisTypeInst (Some thisVar) rfref (mkOne cenv.g m) body
                mkMultiLambdas m tps vsl (newBody, returnTy)
            | NoSafeInitInfo -> 
                expr
            
        else
            expr

    let expr = 
        match baseValOpt with 
        | None -> expr
        | _ -> 
            let m = expr.Range
            let tps,vsl,body,returnTy = stripTopLambda (expr,vspec.Type)
            mkMemberLambdas m tps None baseValOpt vsl (body, returnTy)
              
    { ValScheme = pgrbind.ValScheme
      Binding = TBind(vspec,expr,spBind) }

and FixupLetrecBind cenv denv generalizedTyparsForRecursiveBlock (bind : PostBindCtorThisVarRefCellRecursiveBinding) = 
    let (TBind(vspec,expr,spBind)) = bind.Binding

    // Check coherence of generalization of variables for memberInfo members in generic classes 
    match vspec.MemberInfo with 
#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
    | Some _ when not vspec.IsExtensionMember -> 
#else
    | Some _ -> 
#endif
       match PartitionValTyparsForApparentEnclosingType cenv.g vspec with
       | Some(parentTypars,memberParentTypars,_,_,_)  -> 
          ignore(SignatureConformance.Checker(cenv.g, cenv.amap, denv, SignatureRepackageInfo.Empty, false).CheckTypars vspec.Range TypeEquivEnv.Empty memberParentTypars parentTypars)
       | None -> 
          errorR(Error(FSComp.SR.tcMemberIsNotSufficientlyGeneric(),vspec.Range))
    | _ -> ()

    // Fixup recursive references... 
    let fixupPoints = GetAllUsesOfRecValue cenv vspec

    AdjustAndForgetUsesOfRecValue cenv (mkLocalValRef vspec) bind.ValScheme

    let expr = mkGenericBindRhs cenv.g vspec.Range generalizedTyparsForRecursiveBlock bind.ValScheme.TypeScheme expr

    { FixupPoints = fixupPoints
      Binding = TBind(vspec,expr,spBind) }
    
//-------------------------------------------------------------------------
// TcLetrec
//------------------------------------------------------------------------

and unionGeneralizedTypars typarSets = List.foldBack (ListSet.unionFavourRight typarEq) typarSets [] 
    

and TcLetrec  overridesOK cenv env tpenv (binds,bindsm,scopem) =

    // Create prelimRecValues for the recursive items (includes type info from LHS of bindings) *)
    let binds = binds |> List.map (fun (RecDefnBindingInfo(a,b,c,bind)) -> NormalizedRecBindingDefn(a,b,c,BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env bind))
    let uncheckedRecBinds,prelimRecValues,(tpenv,_) = AnalyzeAndMakeAndPublishRecursiveValues overridesOK cenv env tpenv binds

    let envRec = AddLocalVals cenv.tcSink scopem prelimRecValues env 
    
    // Typecheck bindings 
    let uncheckedRecBindsTable = uncheckedRecBinds  |> List.map (fun rbind  ->  rbind.RecBindingInfo.Val.Stamp, rbind) |> Map.ofList 

    let (_,generalizedRecBinds,preGeneralizationRecBinds,tpenv,_) = 
        ((env,[],[],tpenv,uncheckedRecBindsTable),uncheckedRecBinds) ||> List.fold (TcLetrecBinding (cenv,envRec,scopem,[],None)) 

    // There should be no bindings that have not been generalized since checking the vary last binding always
    // results in the generalization of all remaining ungeneralized bindings, since there are no remaining unchecked bindings
    // to prevent the generalization 
    assert preGeneralizationRecBinds.IsEmpty
    
    let generalizedRecBinds = generalizedRecBinds |> List.sortBy (fun pgrbind -> pgrbind.RecBindingInfo.Index)
    let generalizedTyparsForRecursiveBlock = 
         generalizedRecBinds 
            |> List.map (fun pgrbind -> pgrbind.GeneralizedTypars)
            |> unionGeneralizedTypars


    let vxbinds = generalizedRecBinds |> List.map (TcLetrecAdjustMemberForSpecialVals cenv) 

    // Now that we know what we've generalized we can adjust the recursive references 
    let vxbinds = vxbinds |> List.map (FixupLetrecBind cenv env.DisplayEnv generalizedTyparsForRecursiveBlock) 
    
    // Now eliminate any initialization graphs 
    let binds = 
        let bindsWithoutLaziness = vxbinds
        let mustHaveArity = 
            match uncheckedRecBinds with 
            | [] -> false
            | (rbind :: _) -> DeclKind.MustHaveArity rbind.RecBindingInfo.DeclKind
            
        let results = 
           EliminateInitializationGraphs 
             (fun _ -> failwith "unreachable 2 - no type definitions in recursivve group")
             (fun _ _ -> failwith "unreachable 3 - no type definitions in recursivve group")
             id
             (fun morpher oldBinds -> morpher oldBinds)
             cenv.g mustHaveArity env.DisplayEnv [MutRecShape.Lets bindsWithoutLaziness] bindsm
        match results with 
        | [MutRecShape.Lets newBinds; MutRecShape.Lets newBinds2] -> newBinds @ newBinds2
        | [MutRecShape.Lets newBinds] -> newBinds
        | _ -> failwith "unreachable 4 - gave a Lets shape, expected at most one pre-lets shape back"
    
    // Post letrec env 
    let envbody = AddLocalVals cenv.tcSink scopem prelimRecValues env 
    binds,envbody,tpenv



//-------------------------------------------------------------------------
// Bind specifications of values
//------------------------------------------------------------------------- 

let TcAndPublishValSpec (cenv, env, containerInfo: ContainerInfo, declKind, memFlagsOpt, tpenv, valSpfn) = 

  let (ValSpfn (synAttrs, _, SynValTyparDecls (synTypars, synCanInferTypars, _), _, _, isInline, mutableFlag, doc, vis, literalExprOpt, m)) = valSpfn 

  GeneralizationHelpers.CheckDeclaredTyparsPermitted(memFlagsOpt,synTypars,m)
  let canInferTypars = GeneralizationHelpers.ComputeCanInferExtraGeneralizableTypars (containerInfo.ParentRef, synCanInferTypars, memFlagsOpt)
  
  let attrTgt = DeclKind.AllowedAttribTargets memFlagsOpt declKind 

  let attrs = TcAttributes cenv env attrTgt synAttrs
  let newOk = if canInferTypars then NewTyparsOK else NoNewTypars

  let valinfos, tpenv = TcValSpec cenv env declKind newOk containerInfo memFlagsOpt None tpenv valSpfn attrs
  let denv = env.DisplayEnv
    
  (tpenv, valinfos) ||> List.mapFold (fun tpenv valSpecResult -> 

            let (ValSpecResult (altActualParent, memberInfoOpt, id, enclosingDeclaredTypars, declaredTypars, ty, partialValReprInfo, declKind)) = valSpecResult
            
            let inlineFlag = ComputeInlineFlag (memberInfoOpt |> Option.map (fun (ValMemberInfoTransient(memberInfo,_,_)) -> memberInfo.MemberFlags)) isInline mutableFlag m
            
            let freeInType = freeInTypeLeftToRight cenv.g false ty

            let allDeclaredTypars = enclosingDeclaredTypars @ declaredTypars

            let flex = ExplicitTyparInfo(declaredTypars,declaredTypars,synCanInferTypars)
            
            let generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv,id.idRange,emptyFreeTypars,canInferTypars,CanGeneralizeConstrainedTypars,inlineFlag,None,allDeclaredTypars,freeInType,ty,false)
            
            let valscheme1 = PrelimValScheme1(id,flex,ty,Some(partialValReprInfo),memberInfoOpt,mutableFlag,inlineFlag,NormalVal,noArgOrRetAttribs,vis,false)

            let valscheme2 = GeneralizeVal cenv denv enclosingDeclaredTypars generalizedTypars valscheme1

            let tpenv = HideUnscopedTypars generalizedTypars tpenv

            let valscheme = BuildValScheme declKind (Some(partialValReprInfo)) valscheme2 

            let konst = 
                match literalExprOpt with 
                | None -> 
                    let hasLiteralAttr = HasFSharpAttribute cenv.g cenv.g.attrib_LiteralAttribute attrs
                    if hasLiteralAttr then 
                        errorR(Error(FSComp.SR.tcLiteralAttributeRequiresConstantValue(),m))
                    None
                
                | Some e -> 
                    let hasLiteralAttr,konst = TcLiteral cenv ty env tpenv (attrs,e)
                    if not hasLiteralAttr then 
                        errorR(Error(FSComp.SR.tcValueInSignatureRequiresLiteralAttribute(), e.Range))
                    konst

            let vspec = MakeAndPublishVal cenv env (altActualParent,true,declKind,ValNotInRecScope,valscheme,attrs,doc.ToXmlDoc(),konst,false)
            assert(vspec.InlineInfo = inlineFlag)

            vspec,tpenv)


//-------------------------------------------------------------------------
// Bind elements of data definitions for exceptions and types (fields, etc.)
//------------------------------------------------------------------------- 

exception NotUpperCaseConstructor of range

let CheckNamespaceModuleOrTypeName (g:TcGlobals) (id:Ident) = 
    // type names '[]' etc. are used in fslib
    if not g.compilingFslib &&  id.idText.IndexOfAny(IllegalCharactersInTypeAndNamespaceNames) <> -1 then 
        errorR(Error(FSComp.SR.tcInvalidNamespaceModuleTypeUnionName(),id.idRange))

let CheckDuplicates (idf : _ -> Ident) k elems = 
    elems |> List.iteri (fun i uc1 -> 
        elems |> List.iteri (fun j uc2 -> 
            let id1 = (idf uc1)
            let id2 = (idf uc2)
            if j > i &&  id1.idText = id2.idText then 
                errorR (Duplicate(k,id1.idText,id1.idRange))))
    elems


module TcRecdUnionAndEnumDeclarations = begin

    let CombineReprAccess parent vis = 
        match parent with 
        | ParentNone -> vis 
        | Parent tcref -> combineAccess vis tcref.TypeReprAccessibility

    let MakeRecdFieldSpec _cenv env parent (isStatic,konst,ty',attrsForProperty,attrsForField,id,isMutable,vol,xmldoc,vis,m) =
        let vis,_ = ComputeAccessAndCompPath env None m vis None parent
        let vis = CombineReprAccess parent vis
        NewRecdField isStatic konst id ty' isMutable vol attrsForProperty attrsForField xmldoc vis false

    let TcFieldDecl cenv env parent isIncrClass tpenv (isStatic,synAttrs,id,ty,isMutable,xmldoc,vis,m) =
        let attrs, _ = TcAttributesWithPossibleTargets false cenv env AttributeTargets.FieldDecl synAttrs
        let attrsForProperty,attrsForField = attrs |> List.partition (fun (attrTargets,_) -> (attrTargets &&& AttributeTargets.Property) <> enum 0) 
        let attrsForProperty = (List.map snd attrsForProperty) 
        let attrsForField = (List.map snd attrsForField)
        let ty',_ = TcTypeAndRecover cenv NoNewTypars CheckCxs ItemOccurence.UseInType env tpenv ty
        let zeroInit = HasFSharpAttribute cenv.g cenv.g.attrib_DefaultValueAttribute attrsForField
        let isVolatile = HasFSharpAttribute cenv.g cenv.g.attrib_VolatileFieldAttribute attrsForField
        
        let isThreadStatic = isThreadOrContextStatic cenv.g attrsForField
        if isThreadStatic && (not zeroInit || not isStatic) then 
            error(Error(FSComp.SR.tcThreadStaticAndContextStaticMustBeStatic(),m))

        if isVolatile then 
            error(Error(FSComp.SR.tcVolatileOnlyOnClassLetBindings(),m))

        if isIncrClass  && (not zeroInit || not isMutable) then errorR(Error(FSComp.SR.tcUninitializedValFieldsMustBeMutable(),m))
        if isStatic && (not zeroInit || not isMutable || vis <> Some SynAccess.Private ) then errorR(Error(FSComp.SR.tcStaticValFieldsMustBeMutableAndPrivate(),m))
        let konst = if zeroInit then Some Const.Zero else None
        let rfspec = MakeRecdFieldSpec cenv env parent  (isStatic,konst,ty',attrsForProperty,attrsForField,id,isMutable,isVolatile,xmldoc,vis,m)
        match parent with
        | Parent tcref when useGenuineField tcref.Deref rfspec ->
            // Recheck the attributes for errors if the definition only generates a field
            TcAttributesWithPossibleTargets false cenv env AttributeTargets.FieldDeclRestricted synAttrs |> ignore
        | _ -> ()
        rfspec


    let TcAnonFieldDecl cenv env parent tpenv nm (Field(attribs,isStatic,idOpt,ty,isMutable,xmldoc,vis,m)) =
        let id = (match idOpt with None -> mkSynId m nm | Some id -> id)
        let f = TcFieldDecl cenv env parent false tpenv (isStatic,attribs,id,ty,isMutable,xmldoc.ToXmlDoc(),vis,m) 
        match idOpt with 
        | None -> ()
        | Some id -> 
            let item = Item.ArgName(id, f.FormalType, None)
            CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Binding,env.DisplayEnv,env.AccessRights)
        f


    let TcNamedFieldDecl cenv env parent isIncrClass tpenv (Field(attribs,isStatic,id,ty,isMutable,xmldoc,vis,m)) =
        match id with 
        | None -> error (Error(FSComp.SR.tcFieldRequiresName(),m))
        | Some(id) -> TcFieldDecl cenv env parent isIncrClass tpenv (isStatic,attribs,id,ty,isMutable,xmldoc.ToXmlDoc(),vis,m) 

    let TcNamedFieldDecls cenv env parent isIncrClass tpenv fields =
        fields |> List.map (TcNamedFieldDecl cenv env parent isIncrClass tpenv) 


    //-------------------------------------------------------------------------
    // Bind other elements of type definitions (constructors etc.)
    //------------------------------------------------------------------------- 

    let CheckUnionCaseName cenv realUnionCaseName m =    
        CheckNamespaceModuleOrTypeName cenv.g (mkSynId m realUnionCaseName)
        if not (String.isUpper realUnionCaseName) && realUnionCaseName <> opNameCons && realUnionCaseName <> opNameNil then 
            errorR(NotUpperCaseConstructor(m))
            
    let ValidateFieldNames (synFields : SynField list, tastFields : RecdField list) = 
        let seen = Dictionary()
        for (sf, f) in List.zip synFields tastFields do
            let mutable synField = Unchecked.defaultof<_>
            if seen.TryGetValue(f.Name, &synField) then
                match sf, synField with
                | Field(_, _, Some(id), _, _, _, _, _), Field(_, _, Some(_), _, _, _, _, _) ->
                    error(Error(FSComp.SR.tcFieldNameIsUsedModeThanOnce(id.idText), id.idRange))
                | Field(_, _, Some(id), _, _, _, _, _), Field(_, _, None, _, _, _, _, _)
                | Field(_, _, None, _, _, _, _, _), Field(_, _, Some(id), _, _, _, _, _) ->
                    error(Error(FSComp.SR.tcFieldNameConflictsWithGeneratedNameForAnonymousField(id.idText), id.idRange))
                | _ -> assert false
            else
                seen.Add(f.Name, sf)
                
    let TcUnionCaseDecl cenv env parent thisTy tpenv  (UnionCase (synAttrs,id,args,xmldoc,vis,m)) =
        let attrs = TcAttributes cenv env AttributeTargets.UnionCaseDecl synAttrs // the attributes of a union case decl get attached to the generated "static factory" method
        let vis,_ = ComputeAccessAndCompPath env None m vis None parent
        let vis = CombineReprAccess parent vis
        let realUnionCaseName =  
            if id.idText = opNameCons then "Cons" 
            elif id.idText = opNameNil then "Empty"
            else id.idText
            
        if realUnionCaseName = "Tags" then
            errorR(Error(FSComp.SR.tcUnionCaseNameConflictsWithGeneratedType(realUnionCaseName,"Tags"),m))
                        
        CheckUnionCaseName cenv realUnionCaseName id.idRange 
        
        let mkName nFields i = if nFields <= 1 then "Item" else "Item"+string (i+1)
        let rfields,recordTy = 
            match args with
            | UnionCaseFields flds -> 
                let nFields = flds.Length
                let rfields = flds |> List.mapi (fun i fld -> TcAnonFieldDecl cenv env parent tpenv (mkName nFields i) fld)
                ValidateFieldNames(flds, rfields)
                
                rfields,thisTy
            | UnionCaseFullType (ty,arity) -> 
                let ty',_ = TcTypeAndRecover cenv NoNewTypars CheckCxs ItemOccurence.UseInType env tpenv ty
                let argtysl,recordTy = GetTopTauTypeInFSharpForm cenv.g (arity |> TranslateTopValSynInfo m (TcAttributes cenv env) |> TranslatePartialArity []).ArgInfos ty' m
                if argtysl.Length > 1 then 
                    errorR(Error(FSComp.SR.tcIllegalFormForExplicitTypeDeclaration(),m))   
                let argtys = argtysl |> List.concat
                let nFields = argtys.Length
                let rfields = 
                    argtys |> List.mapi (fun i (argty,argInfo) ->
                        let id = (match argInfo.Name with Some id -> id | None -> mkSynId m (mkName nFields i))
                        MakeRecdFieldSpec cenv env parent (false,None,argty,[],[],id,false,false,XmlDoc.Empty,None,m))
                if not (typeEquiv cenv.g recordTy thisTy) then 
                    error(Error(FSComp.SR.tcReturnTypesForUnionMustBeSameAsType(),m))
                rfields,recordTy
        NewUnionCase id realUnionCaseName rfields recordTy attrs (xmldoc.ToXmlDoc()) vis


    let TcUnionCaseDecls cenv env parent (thisTy : TType) tpenv unionCases =
        let unionCases' = unionCases |> List.map (TcUnionCaseDecl cenv env parent thisTy tpenv) 
        unionCases' |> CheckDuplicates (fun uc -> uc.Id) "union case" 

    let TcEnumDecl cenv env parent thisTy fieldTy (EnumCase (synAttrs,id,v,xmldoc,m)) =
        let attrs = TcAttributes cenv env AttributeTargets.Field synAttrs
        match v with 
        | SynConst.Bytes _
        | SynConst.UInt16s _
        | SynConst.UserNum _ -> error(Error(FSComp.SR.tcInvalidEnumerationLiteral(),m))
        | _ -> 
            let v = TcConst cenv fieldTy m env v
            let vis,_ = ComputeAccessAndCompPath env None m None None parent
            let vis = CombineReprAccess parent vis
            if id.idText = "value__" then errorR(Error(FSComp.SR.tcNotValidEnumCaseName(),id.idRange))
            NewRecdField true (Some v) id thisTy false false [] attrs (xmldoc.ToXmlDoc()) vis false
      
    let TcEnumDecls cenv env parent thisTy enumCases =
        let fieldTy = NewInferenceType ()
        let enumCases' = enumCases |> List.map (TcEnumDecl cenv env parent thisTy fieldTy)  |> CheckDuplicates (fun f -> f.Id) "enum element"
        fieldTy,enumCases'

end

//-------------------------------------------------------------------------
// Bind elements of classes
//------------------------------------------------------------------------- 

let PublishInterface cenv denv (tcref:TyconRef) m compgen ty' = 
    if not (isInterfaceTy cenv.g ty') then errorR(Error(FSComp.SR.tcTypeIsNotInterfaceType1(NicePrint.minimalStringOfType denv ty'),m))
    let tcaug = tcref.TypeContents
    if tcref.HasInterface cenv.g ty'  then 
        errorR(Error(FSComp.SR.tcDuplicateSpecOfInterface(),m))
    tcaug.tcaug_interfaces <- (ty',compgen,m) :: tcaug.tcaug_interfaces

let TcAndPublishMemberSpec cenv env containerInfo declKind tpenv memb = 
    match memb with 
    | SynMemberSig.ValField(_,m) -> error(Error(FSComp.SR.tcFieldValIllegalHere(),m))
    | SynMemberSig.Inherit(_,m) -> error(Error(FSComp.SR.tcInheritIllegalHere(),m))
    | SynMemberSig.NestedType(_,m) -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m))
    | SynMemberSig.Member(valSpfn,memberFlags,_) -> 
        TcAndPublishValSpec (cenv,env,containerInfo,declKind,Some memberFlags,tpenv,valSpfn)
    | SynMemberSig.Interface _ -> 
        // These are done in TcMutRecDefns_Phase1
        [],tpenv

  
let TcTyconMemberSpecs cenv env containerInfo declKind tpenv (augSpfn: SynMemberSigs)  =
    let members,tpenv = List.mapFold (TcAndPublishMemberSpec cenv env containerInfo declKind) tpenv augSpfn
    List.concat members,tpenv


//-------------------------------------------------------------------------
// Bind 'open' declarations
//------------------------------------------------------------------------- 

let TcModuleOrNamespaceLidAndPermitAutoResolve env amap (longId : Ident list) =
    let ad = env.eAccessRights
    let m = longId |> List.map (fun id -> id.idRange) |> List.reduce unionRanges
    match ResolveLongIndentAsModuleOrNamespace ResultCollectionSettings.AllResults amap m OpenQualified env.eNameResEnv ad longId  with 
    | Result res -> Result res
    | Exception err ->  raze err

let TcOpenDecl tcSink (g:TcGlobals) amap m scopem env (longId : Ident list)  = 
    let modrefs = ForceRaise (TcModuleOrNamespaceLidAndPermitAutoResolve env amap longId)

    // validate opened namespace names
    for id in longId do
        if id.idText <> MangledGlobalName then
            CheckNamespaceModuleOrTypeName g id

    let IsPartiallyQualifiedNamespace (modref: ModuleOrNamespaceRef) = 
        let (CompPath(_,p)) = modref.CompilationPath 
        // Bug FSharp 1.0 3274: FSI paths don't count when determining this warning
        let p = 
            match p with 
            | [] -> []
            | (h,_):: t -> if h.StartsWith(FsiDynamicModulePrefix,System.StringComparison.Ordinal) then t else p

        // See https://fslang.uservoice.com/forums/245727-f-language/suggestions/6107641-make-microsoft-prefix-optional-when-using-core-f
        let isFSharpCoreSpecialCase =
            match ccuOfTyconRef modref with 
            | None -> false
            | Some ccu -> 
                ccuEq ccu g.fslibCcu &&
                // Check if we're using a reference one string shorter than what we expect.
                //
                // "p" is the fully qualified path _containing_ the thing we're opening, e.g. "Microsoft.FSharp" when opening "Microsoft.FSharp.Data"
                // "longId" is the text being used, e.g. "FSharp.Data"
                //    Length of thing being opened = p.Length + 1
                //    Length of reference = longId.Length
                // So the reference is a "shortened" reference if (p.Length + 1) - 1 = longId.Length
                (p.Length + 1) - 1 = longId.Length && 
                fst p.[0] = "Microsoft" 

        modref.IsNamespace && 
        p.Length >= longId.Length &&
        not isFSharpCoreSpecialCase
        // Allow "open Foo" for "Microsoft.Foo" from FSharp.Core

    modrefs |> List.iter (fun (_,modref,_) ->
       if modref.IsModule && HasFSharpAttribute g g.attrib_RequireQualifiedAccessAttribute modref.Attribs then 
           errorR(Error(FSComp.SR.tcModuleRequiresQualifiedAccess(fullDisplayTextOfModRef modref),m)))

    // Bug FSharp 1.0 3133: 'open Lexing'. Skip this warning if we successfully resolved to at least a module name
    if not (modrefs |> List.exists (fun (_,modref,_) -> modref.IsModule && not (HasFSharpAttribute g g.attrib_RequireQualifiedAccessAttribute modref.Attribs))) then
        modrefs |> List.iter (fun (_,modref,_) ->
            if IsPartiallyQualifiedNamespace modref  then 
                 errorR(Error(FSComp.SR.tcOpenUsedWithPartiallyQualifiedPath(fullDisplayTextOfModRef modref),m)))
        
    modrefs |> List.iter (fun (_,modref,_) -> CheckEntityAttributes g modref m |> CommitOperationResult)        

    let env = OpenModulesOrNamespaces tcSink g amap scopem false env (List.map p23 modrefs)
    env    


exception ParameterlessStructCtor of range

/// Incremental class definitions
module IncrClassChecking = 

    /// Represents a single group of bindings in a class with an implicit constructor
    type IncrClassBindingGroup = 
      | IncrClassBindingGroup of Tast.Binding list * (*isStatic:*) bool* (*recursive:*) bool
      | IncrClassDo of Expr * (*isStatic:*) bool

    /// Typechecked info for implicit constructor and it's arguments 
    type IncrClassCtorLhs = 
        {/// The TyconRef for the type being defined
         TyconRef                         : TyconRef
         /// The type parameters allocated for the implicit instance constructor. 
         /// These may be equated with other (WillBeRigid) type parameters through equirecursive inference, and so 
         /// should always be renormalized/canonicalized when used.
         InstanceCtorDeclaredTypars       : Typars     
         /// The value representing the static implicit constructor.
         /// Lazy to ensure the static ctor value is only published if needed.
         StaticCtorValInfo                : Lazy<(Val list * Val * ValScheme)>
         /// The value representing the implicit constructor.
         InstanceCtorVal                  : Val
         /// The type of the implicit constructor, representing as a ValScheme.
         InstanceCtorValScheme            : ValScheme
         /// The values representing the arguments to the implicit constructor.
         InstanceCtorArgs                 : Val list
         /// The reference cell holding the 'this' parameter within the implicit constructor so it can be referenced in the
         /// arguments passed to the base constructor
         InstanceCtorSafeThisValOpt       : Val option
         /// Data indicating if safe-initialization checks need to be inserted for this type.
         InstanceCtorSafeInitInfo         : SafeInitData
         /// The value representing the 'base' variable within the implicit instance constructor.
         InstanceCtorBaseValOpt           : Val option
         /// The value representing the 'this' variable within the implicit instance constructor.
         InstanceCtorThisVal              : Val
         /// The name generator used to generate the names of fields etc. within the type.
         NameGenerator                    : NiceNameGenerator
        }
        /// Get the type parameters of the implicit constructor, after taking equi-recursive inference into account.
        member ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv denv m = 
            let ctorDeclaredTypars = ctorInfo.InstanceCtorDeclaredTypars
            let ctorDeclaredTypars = ChooseCanonicalDeclaredTyparsAfterInference cenv.g denv ctorDeclaredTypars m
            ctorDeclaredTypars

    /// Check and elaborate the "left hand side" of the implicit class construction 
    /// syntax.
    let TcImplictCtorLhs_Phase2A(cenv, env, tpenv, tcref:TyconRef, vis, attrs, spats, thisIdOpt, baseValOpt: Val option, safeInitInfo, m, copyOfTyconTypars, objTy, thisTy) =

        let baseValOpt = 
            match GetSuperTypeOfType cenv.g cenv.amap m objTy with 
            | Some superTy -> MakeAndPublishBaseVal cenv env (match baseValOpt with None -> None | Some v -> Some v.Id) superTy
            | None -> None

        // Add class typars to env 
        let env = AddDeclaredTypars CheckForDuplicateTypars copyOfTyconTypars env

        // Type check arguments by processing them as 'simple' patterns 
        //     NOTE: if we allow richer patterns here this is where we'd process those patterns 
        let ctorArgNames,(_,names,_) = TcSimplePatsOfUnknownType cenv true CheckCxs env tpenv (SynSimplePats.SimplePats (spats,m))
        
        // Create the values with the given names 
        let _,vspecs = MakeSimpleVals cenv env names

        if tcref.IsStructOrEnumTycon && isNil spats then 
            errorR (ParameterlessStructCtor(tcref.Range))
        
        // Put them in order 
        let ctorArgs = List.map (fun v -> NameMap.find v vspecs) ctorArgNames
        let safeThisValOpt = MakeAndPublishSafeThisVal cenv env thisIdOpt thisTy
        
        // NOTE: the type scheme here is not complete!!! The ctorTy is more or less 
        // just a type variable. The type and typars get fixed-up after inference 
        let ctorValScheme,ctorVal = 
            let argty = mkRefTupledTy cenv.g (typesOfVals ctorArgs)
            // Initial type has known information 
            let ctorTy = mkFunTy argty objTy    
            // REVIEW: no attributes can currently be specified for the implicit constructor 
            let attribs = TcAttributes cenv env (AttributeTargets.Constructor ||| AttributeTargets.Method) attrs
            let memberFlags      = CtorMemberFlags 
                                  
            let synArgInfos   = List.map (SynInfo.InferSynArgInfoFromSimplePat []) spats
            let valSynData = SynValInfo([synArgInfos],SynInfo.unnamedRetVal)
            let id            = ident ("new",m)

            CheckForNonAbstractInterface ModuleOrMemberBinding tcref memberFlags id.idRange
            let memberInfo  = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,false,attribs,[],memberFlags,valSynData,id,false)
            let partialValReprInfo = TranslateTopValSynInfo m (TcAttributes cenv env) valSynData
            let prelimTyschemeG = TypeScheme(copyOfTyconTypars,ctorTy)
            let isComplete = ComputeIsComplete copyOfTyconTypars [] ctorTy
            let topValInfo = InferGenericArityFromTyScheme prelimTyschemeG partialValReprInfo
            let ctorValScheme = ValScheme(id,prelimTyschemeG,Some(topValInfo),Some(memberInfo),false,ValInline.Never,NormalVal,vis,false,true,false,false)
            let ctorVal = MakeAndPublishVal cenv env (Parent(tcref),false,ModuleOrMemberBinding,ValInRecScope(isComplete),ctorValScheme,attribs,XmlDoc.Empty,None,false) 
            ctorValScheme,ctorVal

        // We only generate the cctor on demand, because wew don't need it if there are no cctor actions. 
        // The code below has a side-effect (MakeAndPublishVal), so we only want to run it once if at all. 
        // The .cctor is never referenced by any other code.
        let cctorValInfo = 
            lazy 
               (let cctorArgs = [ fst(mkCompGenLocal m "unitVar" cenv.g.unit_ty) ]

                let cctorTy = mkFunTy cenv.g.unit_ty cenv.g.unit_ty
                let valSynData = SynValInfo([[]],SynInfo.unnamedRetVal)
                let id = ident ("cctor",m)
                CheckForNonAbstractInterface ModuleOrMemberBinding tcref ClassCtorMemberFlags id.idRange
                let memberInfo  = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,false,[(*no attributes*)],[],ClassCtorMemberFlags,valSynData,id,false)
                let partialValReprInfo = TranslateTopValSynInfo m (TcAttributes cenv env) valSynData
                let prelimTyschemeG = TypeScheme(copyOfTyconTypars,cctorTy)
                let topValInfo = InferGenericArityFromTyScheme prelimTyschemeG partialValReprInfo
                let cctorValScheme = ValScheme(id,prelimTyschemeG,Some(topValInfo),Some(memberInfo),false,ValInline.Never,NormalVal,Some SynAccess.Private,false,true,false,false)
                 
                let cctorVal = MakeAndPublishVal cenv env (Parent(tcref),false,ModuleOrMemberBinding,ValNotInRecScope,cctorValScheme,[(* no attributes*)],XmlDoc.Empty,None,false) 
                cctorArgs,cctorVal,cctorValScheme)

        let thisVal = 
            // --- Create this for use inside constructor 
            let thisId  = ident ("this",m)
            let thisValScheme  = ValScheme(thisId,NonGenericTypeScheme(thisTy),None,None,false,ValInline.Never,CtorThisVal,None,true,false,false,false)
            let thisVal    = MakeAndPublishVal cenv env (ParentNone,false,ClassLetBinding(false),ValNotInRecScope,thisValScheme,[],XmlDoc.Empty,None,false)
            thisVal

        {TyconRef                         = tcref
         InstanceCtorDeclaredTypars    = copyOfTyconTypars
         StaticCtorValInfo             = cctorValInfo
         InstanceCtorArgs              = ctorArgs
         InstanceCtorVal               = ctorVal
         InstanceCtorValScheme         = ctorValScheme
         InstanceCtorBaseValOpt        = baseValOpt
         InstanceCtorSafeThisValOpt    = safeThisValOpt
         InstanceCtorSafeInitInfo    = safeInitInfo
         InstanceCtorThisVal           = thisVal
         // For generating names of local fields
         NameGenerator                 = NiceNameGenerator()

        }


    // Partial class defns - local val mapping to fields
      
    /// Create the field for a "let" binding in a type definition.
    ///
    /// The "v" is the local typed w.r.t. tyvars of the implicit ctor.
    /// The formalTyparInst does the formal-typars/implicit-ctor-typars subst.
    /// Field specifications added to a tcref must be in terms of the tcrefs formal typars.
    let private MakeIncrClassField(g, cpath, formalTyparInst:TyparInst, v:Val, isStatic, rfref:RecdFieldRef) =
        let name = rfref.FieldName
        let id  = ident (name,v.Range)
        let ty  = v.Type |> instType formalTyparInst
        let taccess = TAccess [cpath]
        let isVolatile = HasFSharpAttribute g g.attrib_VolatileFieldAttribute v.Attribs

        NewRecdField isStatic None id ty v.IsMutable isVolatile [(*no property attributes*)] v.Attribs v.XmlDoc taccess (*compiler generated:*)true

    /// Indicates how is a 'let' bound value in a class with implicit construction is represented in
    /// the TAST ultimately produced by type checking.    
    type IncrClassValRepr = 
        // e.g representation for 'let v = 3' if it is not used in anything given a method representation
        | InVar of (* isArg: *) bool 
        // e.g representation for 'let v = 3'
        | InField of (*isStatic:*)bool * (*staticCountForSafeInit:*) int * RecdFieldRef
        // e.g representation for 'let f x = 3'
        | InMethod of (*isStatic:*)bool * Val * ValReprInfo

    /// IncrClassReprInfo represents the decisions we make about the representation of 'let' and 'do' bindings in a
    /// type defined with implicit class construction.
    type IncrClassReprInfo = 
        { /// Indicates the set of field names taken within one incremental class
          TakenFieldNames:Set<string>
          RepInfoTcGlobals:TcGlobals
          /// vals mapped to representations
          ValReprs  : Zmap<Val,IncrClassValRepr> 
          /// vals represented as fields or members from this point on 
          ValsWithRepresentation  : Zset<Val> }

        static member Empty(g,names) = 
            { TakenFieldNames=Set.ofList names
              RepInfoTcGlobals=g
              ValReprs = Zmap.empty valOrder 
              ValsWithRepresentation = Zset.empty valOrder }

        /// Find the representation of a value
        member localRep.LookupRepr (v:Val) = 
            match Zmap.tryFind v localRep.ValReprs with 
            | None -> error(InternalError("LookupRepr: failed to find representation for value",v.Range))
            | Some res -> res

        static member IsMethodRepr cenv (bind:Binding) = 
            let v = bind.Var
            // unit fields are not stored, just run rhs for effects
            if isUnitTy cenv.g v.Type then 
                false
            else 
                let arity = InferArityOfExprBinding cenv.g v bind.Expr 
                not arity.HasNoArgs && not v.IsMutable


        /// Choose how a binding is represented
        member localRep.ChooseRepresentation (cenv,env: TcEnv,isStatic,isCtorArg,
                                              ctorInfo:IncrClassCtorLhs,
                                              /// The vars forced to be fields due to static member bindings, instance initialization expressions or instance member bindings
                                              staticForcedFieldVars:FreeLocals,
                                              /// The vars forced to be fields due to instance member bindings
                                              instanceForcedFieldVars:FreeLocals,
                                              takenFieldNames: Set<string>,
                                              bind:Binding) = 
            let g = cenv.g 
            let v = bind.Var
            let relevantForcedFieldVars = (if isStatic then staticForcedFieldVars else instanceForcedFieldVars)
            
            let tcref = ctorInfo.TyconRef
            let name,takenFieldNames = 

                let isNameTaken = 
                    // Check if a implicit field already exists with this name
                    takenFieldNames.Contains(v.LogicalName) ||
                    // Check if a user-defined field already exists with this name. Struct fields have already been created - see bug FSharp 1.0 5304
                    (tcref.GetFieldByName(v.LogicalName).IsSome && (isStatic || not tcref.IsFSharpStructOrEnumTycon)) 

                let nm = 
                    if isNameTaken then 
                        ctorInfo.NameGenerator.FreshCompilerGeneratedName (v.LogicalName,v.Range)
                    else 
                        v.LogicalName
                nm, takenFieldNames.Add(nm)
                 
            let reportIfUnused() = 
                if not v.HasBeenReferenced && not v.IsCompiledAsTopLevel && not (v.DisplayName.StartsWith "_") && not v.IsCompilerGenerated then 
                    warning (Error(FSComp.SR.chkUnusedValue(v.DisplayName), v.Range))

            let repr = 
                match InferArityOfExprBinding g v bind.Expr with 
                | arity when arity.HasNoArgs || v.IsMutable -> 
                    // all mutable variables are forced into fields, since they may escape into closures within the implicit constructor
                    // e.g. 
                    //     type C() =  
                    //        let mutable m = 1
                    //        let n = ... (fun () -> m) ....
                    //
                    // All struct variables are forced into fields. Structs may not contain "let" bindings, so no new variables can be 
                    // introduced.
                    
                    if v.IsMutable || relevantForcedFieldVars.Contains v || tcref.IsStructOrEnumTycon then 
                        //dprintfn "Representing %s as a field %s" v.LogicalName name
                        let rfref = RFRef(tcref, name)
                        reportIfUnused()
                        InField (isStatic, localRep.ValReprs.Count, rfref)
                    else
                        //if not v.Attribs.IsEmpty then 
                        //    warning(Error(FSComp.SR.tcAttributesIgnoredOnLetBinding(), v.Range))
                        //dprintfn 
                        //    "Representing %s as a local variable %s, staticForcedFieldVars = %s, instanceForcedFieldVars = %s" 
                        //    v.LogicalName name 
                        //    (staticForcedFieldVars |> Seq.map (fun v -> v.LogicalName) |> String.concat ",")
                        //    (instanceForcedFieldVars |> Seq.map (fun v -> v.LogicalName) |> String.concat ",")
                        InVar isCtorArg
                | topValInfo -> 
                    //dprintfn "Representing %s as a method %s" v.LogicalName name
                    let tps, argInfos, _, _ = GetTopValTypeInCompiledForm g topValInfo v.Type v.Range

                    let valSynInfo = SynValInfo(argInfos |> List.mapSquared (fun (_,argInfo) -> SynArgInfo([],false,argInfo.Name)),SynInfo.unnamedRetVal)
                    let memberFlags = (if isStatic then StaticMemberFlags else NonVirtualMemberFlags) MemberKind.Member
                    let id = mkSynId v.Range name
                    let memberInfo = MakeMemberDataAndMangledNameForMemberVal(g,tcref,false,[],[],memberFlags,valSynInfo,mkSynId v.Range name,true)

                    let copyOfTyconTypars = ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv env.DisplayEnv ctorInfo.TyconRef.Range
                    // Add the 'this' pointer on to the function
                    let memberTauTy,topValInfo = 
                        let tauTy = v.TauType
                        if isStatic then 
                            tauTy,topValInfo 
                        else 
                            let tauTy = ctorInfo.InstanceCtorThisVal.Type --> v.TauType
                            let (ValReprInfo(tpNames,args,ret)) = topValInfo
                            let topValInfo = ValReprInfo(tpNames, ValReprInfo.selfMetadata::args, ret)
                            tauTy, topValInfo
                    // Add the enclosing type parameters on to the function
                    let topValInfo = 
                        let (ValReprInfo(tpNames,args,ret)) = topValInfo
                        ValReprInfo(tpNames@ValReprInfo.InferTyparInfo(copyOfTyconTypars), args, ret)
                                          
                    let prelimTyschemeG = TypeScheme(copyOfTyconTypars@tps,memberTauTy)
                    let memberValScheme = ValScheme(id,prelimTyschemeG,Some(topValInfo),Some(memberInfo),false,ValInline.Never,NormalVal,None,true (* isCompilerGenerated *) ,true (* isIncrClass *) ,false, false)
                    let methodVal = MakeAndPublishVal cenv env (Parent(tcref),false,ModuleOrMemberBinding,ValNotInRecScope,memberValScheme,v.Attribs,XmlDoc.Empty,None,false) 
                    reportIfUnused()
                    InMethod(isStatic,methodVal,topValInfo)

            repr, takenFieldNames

        /// Extend the known local representations by choosing a representation for a binding
        member localRep.ChooseAndAddRepresentation(cenv,env: TcEnv,isStatic,isCtorArg,ctorInfo:IncrClassCtorLhs,staticForcedFieldVars:FreeLocals,instanceForcedFieldVars: FreeLocals,bind:Binding) = 
            let v = bind.Var
            let repr,takenFieldNames = localRep.ChooseRepresentation (cenv,env,isStatic,isCtorArg,ctorInfo,staticForcedFieldVars,instanceForcedFieldVars,localRep.TakenFieldNames,bind )
            // OK, representation chosen, now add it 
            {localRep with 
                TakenFieldNames=takenFieldNames 
                ValReprs = Zmap.add v repr localRep.ValReprs}  

        member localRep.ValNowWithRepresentation (v:Val) = 
            {localRep with ValsWithRepresentation = Zset.add v localRep.ValsWithRepresentation}

        member localRep.IsValWithRepresentation (v:Val) = 
                localRep.ValsWithRepresentation.Contains(v) 

        /// Make the elaborated expression that represents a use of a 
        /// a "let v = ..." class binding
        member localRep.MakeValueLookup thisValOpt tinst safeStaticInitInfo v tyargs m =
            let g = localRep.RepInfoTcGlobals 
            match localRep.LookupRepr v, thisValOpt with 
            | InVar _,_ -> 
                exprForVal m v
            | InField(false, _idx, rfref),Some(thisVal) -> 
                let thise = exprForVal m thisVal
                mkRecdFieldGetViaExprAddr(thise,rfref,tinst,m)
            | InField(false, _idx, _rfref),None -> 
                error(InternalError("Unexpected missing 'this' variable in MakeValueLookup",m))
            | InField(true, idx, rfref),_ -> 
                let expr = mkStaticRecdFieldGet(rfref,tinst,m)
                MakeCheckSafeInit g tinst safeStaticInitInfo (mkInt g m idx) expr
                
            | InMethod(isStatic,methodVal,topValInfo),_ -> 
                //dprintfn "Rewriting application of %s to be call to method %s" v.LogicalName methodVal.LogicalName
                let expr,exprty = AdjustValForExpectedArity g m (mkLocalValRef methodVal) NormalValUse topValInfo 
                // Prepend the the type arguments for the class
                let tyargs = tinst @ tyargs 
                let thisArgs =
                    if isStatic then []
                    else Option.toList (Option.map (exprForVal m) thisValOpt)
                    
                MakeApplicationAndBetaReduce g (expr,exprty,[tyargs],thisArgs,m) 

        /// Make the elaborated expression that represents an assignment 
        /// to a "let mutable v = ..." class binding
        member localRep.MakeValueAssign thisValOpt tinst safeStaticInitInfo v expr m =
            let g = localRep.RepInfoTcGlobals 
            match localRep.LookupRepr v, thisValOpt with 
            | InField(false,_,rfref),Some(thisVal) -> 
                let thise = exprForVal m thisVal
                mkRecdFieldSetViaExprAddr(thise,rfref,tinst,expr,m)
            | InField(false,_,_rfref),None -> 
                error(InternalError("Unexpected missing 'this' variable in MakeValueAssign",m))
            | InVar _,_ -> 
                mkValSet m (mkLocalValRef v) expr
            | InField (true, idx, rfref),_ -> 
                let expr = mkStaticRecdFieldSet(rfref,tinst,expr,m)
                MakeCheckSafeInit g tinst safeStaticInitInfo (mkInt g m idx) expr
            | InMethod _,_ -> 
                error(InternalError("Local was given method storage, yet later it's been assigned to",m))
          
        member localRep.MakeValueGetAddress thisValOpt tinst safeStaticInitInfo v m =
            let g = localRep.RepInfoTcGlobals 
            match localRep.LookupRepr v,thisValOpt with 
            | InField(false, _, rfref),Some(thisVal) -> 
                let thise = exprForVal m thisVal
                mkRecdFieldGetAddrViaExprAddr(thise,rfref,tinst,m)
            | InField(false, _, _rfref),None -> 
                error(InternalError("Unexpected missing 'this' variable in MakeValueGetAddress",m))
            | InField(true, idx, rfref),_ -> 
                let expr = mkStaticRecdFieldGetAddr(rfref,tinst,m)
                MakeCheckSafeInit g tinst safeStaticInitInfo (mkInt g m idx) expr
            | InVar _,_ -> 
                mkValAddr m (mkLocalValRef v)
            | InMethod _,_ -> 
                error(InternalError("Local was given method storage, yet later it's address was required",m))

        /// Mutate a type definition by adding fields 
        /// Used as part of processing "let" bindings in a type definition. 
        member localRep.PublishIncrClassFields (cenv, denv, cpath, ctorInfo:IncrClassCtorLhs, safeStaticInitInfo) =    
            let tcref = ctorInfo.TyconRef
            let rfspecs   = 
                [ for KeyValue(v,repr) in localRep.ValReprs do
                      match repr with 
                      | InField(isStatic, _, rfref) -> 
                          // Instance fields for structs are published earlier because the full set of fields is determined syntactically from the implicit
                          // constructor arguments. This is important for the "default value" and "does it have an implicit default constructor" 
                          // semantic conditions for structs -  see bug FSharp 1.0 5304.
                          if isStatic || not tcref.IsFSharpStructOrEnumTycon then 
                              let ctorDeclaredTypars = ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv denv ctorInfo.TyconRef.Range

                              // Note: tcrefObjTy contains the original "formal" typars, thisTy is the "fresh" one... f<>fresh. 
                              let revTypeInst = List.zip ctorDeclaredTypars (tcref.TyparsNoRange |> List.map mkTyparTy)

                              yield MakeIncrClassField(localRep.RepInfoTcGlobals, cpath, revTypeInst, v, isStatic, rfref)
                      | _ -> 
                          () 
                  match safeStaticInitInfo with 
                  | SafeInitField (_, fld) -> yield fld
                  | NoSafeInitInfo -> () ]

            let recdFields = MakeRecdFieldsTable (rfspecs @ tcref.AllFieldsAsList)

            // Mutate the entity_tycon_repr to publish the fields
            tcref.Deref.entity_tycon_repr <- TFSharpObjectRepr { tcref.FSharpObjectModelTypeInfo with fsobjmodel_rfields = recdFields}  


        /// Given localRep saying how locals have been represented, e.g. as fields.
        /// Given an expr under a given thisVal context.
        //
        /// Fix up the references to the locals, e.g. 
        ///     v -> this.fieldv
        ///     f x -> this.method x
        member localRep.FixupIncrClassExprPhase2C thisValOpt safeStaticInitInfo (thisTyInst:TypeInst) expr = 
            // fixup: intercept and expr rewrite
            let FixupExprNode rw e =
                //dprintfn "Fixup %s" (showL (exprL e))
                match e with
                // Rewrite references to applied let-bound-functions-compiled-as-methods
                | Expr.App(Expr.Val (ValDeref(v),_,_),_,tyargs,args,m) 
                    when (localRep.IsValWithRepresentation(v) &&
                          (match localRep.LookupRepr(v) with 
                           | InMethod _  -> true //(methodVal.Typars.Length > thisTyInst.Length)
                           | _ -> false )) -> 

                        //dprintfn "Found application of %s" v.LogicalName
                        let g = localRep.RepInfoTcGlobals
                        let expr = localRep.MakeValueLookup thisValOpt thisTyInst safeStaticInitInfo v tyargs m
                        let args = args |> List.map rw
                        Some (MakeApplicationAndBetaReduce g (expr,(tyOfExpr g expr),[],args,m)) 
                        

                // Rewrite references to values stored as fields and first class uses of method values
                | Expr.Val (ValDeref(v),_,m)                         
                    when localRep.IsValWithRepresentation(v) -> 

                        //dprintfn "Found use of %s" v.LogicalName
                        Some (localRep.MakeValueLookup thisValOpt thisTyInst safeStaticInitInfo v [] m)

                // Rewrite assignments to mutable values stored as fields 
                | Expr.Op(TOp.LValueOp (LSet, ValDeref(v))    ,[],[arg],m) 
                    when localRep.IsValWithRepresentation(v) ->
                        let arg = rw arg 
                        Some (localRep.MakeValueAssign thisValOpt thisTyInst safeStaticInitInfo v arg m)

                // Rewrite taking the address of mutable values stored as fields 
                | Expr.Op(TOp.LValueOp (LGetAddr,ValDeref(v)),[],[]   ,m) 
                    when localRep.IsValWithRepresentation(v) ->
                        Some (localRep.MakeValueGetAddress thisValOpt thisTyInst safeStaticInitInfo v m)

                | _ -> None
            Tastops.RewriteExpr { PreIntercept = Some FixupExprNode 
                                  PostTransform = (fun _ -> None)
                                  PreInterceptBinding = None
                                  IsUnderQuotations=true } expr 


    type IncrClassConstructionBindingsPhase2C =
      | Phase2CBindings of IncrClassBindingGroup list
      | Phase2CCtorJustAfterSuperInit     
      | Phase2CCtorJustAfterLastLet    

    /// Given a set of 'let' bindings (static or not, recursive or not) that make up a class, 
    /// generate their initialization expression(s).  
    let MakeCtorForIncrClassConstructionPhase2C 
               (cenv,
                env: TcEnv,
                /// The lhs information about the implicit constructor
                ctorInfo:IncrClassCtorLhs,
                /// The call to the super class constructor
                inheritsExpr,
                /// Should we place a sequence point at the 'inheritedTys call?
                inheritsIsVisible,
                /// The declarations
                decs : IncrClassConstructionBindingsPhase2C list,
                memberBinds : Binding list,
                /// Record any unconstrained type parameters generalized for the outer members as "free choices" in the let bindings 
                generalizedTyparsForRecursiveBlock,
                safeStaticInitInfo : SafeInitData) = 


        let denv = env.DisplayEnv 
        let thisVal      = ctorInfo.InstanceCtorThisVal 

        let m = thisVal.Range
        let ctorDeclaredTypars = ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv denv m

        ctorDeclaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m)  

        // Reconstitute the type with the correct quantified type variables.
        ctorInfo.InstanceCtorVal.SetType (tryMkForallTy ctorDeclaredTypars ctorInfo.InstanceCtorVal.TauType)

        let freeChoiceTypars = ListSet.subtract typarEq generalizedTyparsForRecursiveBlock ctorDeclaredTypars

        let thisTyInst = List.map mkTyparTy ctorDeclaredTypars

        let accFreeInExpr acc expr =
            unionFreeVars acc (freeInExpr CollectLocalsNoCaching expr) 
            
        let accFreeInBinding acc (bind:Binding) = 
            accFreeInExpr acc bind.Expr
            
        let accFreeInBindings acc (binds:Binding list) = 
            (acc,binds) ||> List.fold accFreeInBinding

        // Find all the variables used in any method. These become fields.
        //   staticForcedFieldVars:FreeLocals: the vars forced to be fields due to static member bindings, instance initialization expressions or instance member bindings
        //   instanceForcedFieldVars: FreeLocals: the vars forced to be fields due to instance member bindings
                                            
        let staticForcedFieldVars,instanceForcedFieldVars = 
             let (staticForcedFieldVars,instanceForcedFieldVars) = 
                 ((emptyFreeVars,emptyFreeVars),decs) ||> List.fold (fun (staticForcedFieldVars,instanceForcedFieldVars) dec -> 
                    match dec with 
                    | Phase2CCtorJustAfterLastLet
                    | Phase2CCtorJustAfterSuperInit ->  
                        (staticForcedFieldVars,instanceForcedFieldVars)
                    | Phase2CBindings decs ->
                        ((staticForcedFieldVars,instanceForcedFieldVars),decs) ||> List.fold (fun (staticForcedFieldVars,instanceForcedFieldVars) dec -> 
                            match dec with 
                            | IncrClassBindingGroup(binds,isStatic,_) -> 
                                let methodBinds = binds |> List.filter (IncrClassReprInfo.IsMethodRepr cenv) 
                                let staticForcedFieldVars = 
                                    if isStatic then 
                                        // Any references to static variables in any static method force the variable to be represented as a field
                                        (staticForcedFieldVars,methodBinds) ||> accFreeInBindings
                                    else
                                        // Any references to static variables in any instance bindings force the variable to be represented as a field
                                        (staticForcedFieldVars,binds) ||> accFreeInBindings
                                        
                                let instanceForcedFieldVars = 
                                    // Any references to instance variables in any methods force the variable to be represented as a field
                                    (instanceForcedFieldVars,methodBinds) ||> accFreeInBindings
                                        
                                (staticForcedFieldVars,instanceForcedFieldVars)
                            | IncrClassDo (e,isStatic) -> 
                                let staticForcedFieldVars = 
                                    if isStatic then 
                                        staticForcedFieldVars
                                    else
                                        unionFreeVars staticForcedFieldVars (freeInExpr CollectLocalsNoCaching e)
                                (staticForcedFieldVars,instanceForcedFieldVars)))
             let staticForcedFieldVars  = (staticForcedFieldVars,memberBinds) ||> accFreeInBindings 
             let instanceForcedFieldVars = (instanceForcedFieldVars,memberBinds) ||> accFreeInBindings 
             
             // Any references to static variables in the 'inherits' expression force those static variables to be represented as fields
             let staticForcedFieldVars = (staticForcedFieldVars,inheritsExpr) ||> accFreeInExpr

             (staticForcedFieldVars.FreeLocals,instanceForcedFieldVars.FreeLocals)


        // Compute the implicit construction side effects of single 
        // 'let' or 'let rec' binding in the implicit class construction sequence 
        let TransBind (reps:IncrClassReprInfo) (TBind(v,rhsExpr,spBind)) =
            if v.MustInline then
                error(Error(FSComp.SR.tcLocalClassBindingsCannotBeInline(),v.Range))
            let rhsExpr = reps.FixupIncrClassExprPhase2C (Some thisVal) safeStaticInitInfo thisTyInst rhsExpr
            
            // The initialization of the 'ref cell' variable for 'this' is the only binding which comes prior to the super init
            let isPriorToSuperInit = 
                match ctorInfo.InstanceCtorSafeThisValOpt with 
                | None -> false
                | Some v2 -> valEq v v2
                            
            match reps.LookupRepr v with
            | InMethod(isStatic,methodVal,_) -> 
                let _,chooseTps,tauExpr,tauTy,m = 
                    match rhsExpr with 
                    | Expr.TyChoose(chooseTps,b,_) -> [],chooseTps,b,(tyOfExpr cenv.g b),m 
                    | Expr.TyLambda (_,tps,Expr.TyChoose(chooseTps,b,_),m,returnTy) -> tps,chooseTps,b,returnTy,m 
                    | Expr.TyLambda (_,tps,b,m,returnTy) -> tps,[],b,returnTy,m 
                    | e -> [],[],e,(tyOfExpr cenv.g e),e.Range
                    
                let chooseTps = chooseTps @ freeChoiceTypars
                // Add the 'this' variable as an argument
                let tauExpr,tauTy = 
                    if isStatic then 
                        tauExpr,tauTy
                    else
                        let e = mkLambda m thisVal (tauExpr,tauTy)
                        e, tyOfExpr cenv.g e
                // Replace the type parameters that used to be on the rhs with 
                // the full set of type parameters including the type parameters of the enclosing class
                let rhsExpr = mkTypeLambda m methodVal.Typars (mkTypeChoose m chooseTps tauExpr,tauTy)
                (isPriorToSuperInit, (fun e -> e)), [TBind (methodVal,rhsExpr,spBind)]
            
            // If it's represented as a non-escaping local variable then just bind it to its value
            // If it's represented as a non-escaping local arg then no binding necessary (ctor args are already bound)
            
            | InVar isArg ->
                (isPriorToSuperInit, (fun e -> if isArg then e else mkLetBind m (TBind(v,rhsExpr,spBind)) e)), []

            | InField (isStatic, idx, _) ->
                 // Use spBind if it available as the span for the assignment into the field
                let m =
                     match spBind,rhsExpr with 
                     // Don't generate big sequence points for functions in classes
                     | _, (Expr.Lambda  _ | Expr.TyLambda _) -> v.Range
                     | SequencePointAtBinding m,_ -> m 
                     | _ -> v.Range
                let assignExpr = reps.MakeValueAssign (Some thisVal) thisTyInst NoSafeInitInfo v rhsExpr m
                let adjustSafeInitFieldExprOpt = 
                    if isStatic then 
                        match safeStaticInitInfo with 
                        | SafeInitField (rfref, _) -> 
                            let setExpr = mkStaticRecdFieldSet (rfref, thisTyInst, mkInt cenv.g m idx, m)
                            let setExpr = reps.FixupIncrClassExprPhase2C (Some(thisVal)) NoSafeInitInfo thisTyInst setExpr
                            Some setExpr
                        | NoSafeInitInfo -> 
                            None
                    else
                        None

                (isPriorToSuperInit, (fun e -> 
                     let e = match adjustSafeInitFieldExprOpt with None -> e | Some ae -> mkCompGenSequential m ae e
                     mkSequential SequencePointsAtSeq m assignExpr e)), []

        /// Work out the implicit construction side effects of a 'let', 'let rec' or 'do' 
        /// binding in the implicit class construction sequence 
        let TransTrueDec isCtorArg (reps:IncrClassReprInfo) dec = 
              match dec with 
              | (IncrClassBindingGroup(binds,isStatic,isRec)) ->
                  let actions,reps,methodBinds = 
                      let reps     = (reps,binds) ||> List.fold (fun rep bind -> rep.ChooseAndAddRepresentation(cenv,env,isStatic,isCtorArg,ctorInfo,staticForcedFieldVars,instanceForcedFieldVars,bind)) // extend
                      if isRec then
                          // Note: the recursive calls are made via members on the object
                          // or via access to fiels. THis means the recursive loop is "broken", 
                          // and we can collapse to sequential bindings 
                          let reps     = (reps,binds) ||> List.fold (fun rep bind -> rep.ValNowWithRepresentation bind.Var) // inscope before
                          let actions,methodBinds = binds |> List.map (TransBind reps) |> List.unzip // since can occur in RHS of own defns 
                          actions,reps,methodBinds
                      else 
                          let actions,methodBinds = binds |> List.map (TransBind reps)  |> List.unzip
                          let reps     = (reps,binds) ||> List.fold (fun rep bind -> rep.ValNowWithRepresentation bind.Var) // inscope after
                          actions,reps,methodBinds
                  let methodBinds = List.concat methodBinds
                  if isStatic then 
                      (actions,[],methodBinds),reps
                  else 
                      ([],actions,methodBinds),reps

              | IncrClassDo (doExpr,isStatic) -> 
                  let doExpr = reps.FixupIncrClassExprPhase2C (Some(thisVal)) safeStaticInitInfo thisTyInst doExpr
                  let binder = (fun e -> mkSequential SequencePointsAtSeq doExpr.Range doExpr e)
                  let isPriorToSuperInit = false
                  if isStatic then 
                      ([(isPriorToSuperInit,binder)],[],[]),reps
                  else 
                      ([],[(isPriorToSuperInit,binder)],[]),reps


        /// Work out the implicit construction side effects of each declaration 
        /// in the implicit class construction sequence 
        let TransDec (reps:IncrClassReprInfo) dec = 
            match dec with 
            // The call to the base class constructor is done so we can set the ref cell 
            | Phase2CCtorJustAfterSuperInit ->  
                let binders = 
                    [ match ctorInfo.InstanceCtorSafeThisValOpt with 
                      | None ->  ()
                      | Some v -> 
                        let setExpr = mkRefCellSet cenv.g m ctorInfo.InstanceCtorThisVal.Type (exprForVal m v) (exprForVal m ctorInfo.InstanceCtorThisVal)
                        let setExpr = reps.FixupIncrClassExprPhase2C (Some(thisVal)) safeStaticInitInfo thisTyInst setExpr
                        let binder = (fun e -> mkSequential SequencePointsAtSeq setExpr.Range setExpr e)
                        let isPriorToSuperInit = false
                        yield (isPriorToSuperInit,binder) ]

                ([],binders,[]),reps

            // The last 'let' binding is done so we can set the initialization condition for the collection of object fields
            // which now allows members to be called.
            | Phase2CCtorJustAfterLastLet ->  
                let binders = 
                    [ match ctorInfo.InstanceCtorSafeInitInfo with 
                      | SafeInitField (rfref, _) ->  
                        let setExpr = mkRecdFieldSetViaExprAddr (exprForVal m thisVal, rfref, thisTyInst, mkOne cenv.g m, m)
                        let setExpr = reps.FixupIncrClassExprPhase2C (Some(thisVal)) safeStaticInitInfo thisTyInst setExpr
                        let binder = (fun e -> mkSequential SequencePointsAtSeq setExpr.Range setExpr e)
                        let isPriorToSuperInit = false
                        yield (isPriorToSuperInit,binder)  
                      | NoSafeInitInfo ->  
                        () ]

                ([],binders,[]),reps
                
            | Phase2CBindings decs -> 
                let initActions, reps = List.mapFold (TransTrueDec false) reps decs 
                let cctorInitActions, ctorInitActions, methodBinds = List.unzip3 initActions
                (List.concat cctorInitActions, List.concat ctorInitActions, List.concat methodBinds), reps 

                

        let takenFieldNames = 
            [ for b in memberBinds do 
                  yield b.Var.CompiledName 
                  yield b.Var.DisplayName 
                  yield b.Var.CoreDisplayName 
                  yield b.Var.LogicalName ] 
        let reps = IncrClassReprInfo.Empty(cenv.g, takenFieldNames)

        // Bind the IsArg(true) representations of the object constructor arguments and assign them to fields
        // if they escape to the members. We do this by running the instance bindings 'let x = x' through TransTrueDec
        // for each constructor argument 'x', but with the special flag 'isCtorArg', which helps TransBind know that 
        // the value is already available as an argument, and that nothing special needs to be done unless the 
        // value is being stored into a field.
        let (cctorInitActions1, ctorInitActions1,methodBinds1),reps = 
            let binds = ctorInfo.InstanceCtorArgs |> List.map (fun v -> mkInvisibleBind v (exprForVal v.Range v))
            TransTrueDec true reps (IncrClassBindingGroup(binds,false,false))

        // We expect that only ctorInitActions1 will be non-empty here, and even then only if some elements are stored in the field
        assert (isNil cctorInitActions1)
        assert (isNil methodBinds1)

        // Now deal with all the 'let' and 'member' declarations
        let initActions,reps = List.mapFold TransDec reps decs
        let cctorInitActions2, ctorInitActions2,methodBinds2 = List.unzip3 initActions
        let cctorInitActions = cctorInitActions1 @ List.concat cctorInitActions2
        let ctorInitActions = ctorInitActions1 @ List.concat ctorInitActions2
        let methodBinds = methodBinds1 @ List.concat methodBinds2

        let ctorBody =
            // Build the elements of the implicit constructor body, starting from the bottome
            //     <optional-this-ref-cell-init>
            //     <super init>
            //     <let/do bindings>
            //     return ()
            let ctorInitActionsPre,ctorInitActionsPost = ctorInitActions |> List.partition (fun (isPriorToSuperInit,_) -> isPriorToSuperInit)

            // This is the return result
            let ctorBody = mkUnit cenv.g m

            // Add <optional-this-ref-cell-init>.
            // That is, add any <let/do bindings> that come prior to the super init constructor call,
            // This is only ever at most the init of the InstanceCtorSafeThisValOpt and InstanceCtorSafeInitInfo var/field
            let ctorBody = List.foldBack (fun (_,binder) acc -> binder acc) ctorInitActionsPost ctorBody
            
            // Add the <super init>
            let ctorBody = 
                // The inheritsExpr may refer to the this variable or to incoming arguments, e.g. in closure fields.
                // References to the this variable go via the ref cell that gets created to help ensure coherent initialization.
                // This ref cell itself may be stored in a field of the object and accessed via arg0.
                // Likewise the incoming arguments will eventually be stored in fields and accessed via arg0.
                // 
                // As a result, the most natural way to implement this would be to simply capture arg0  if needed
                // and access all variables via that. This would be done by rewriting the inheritsExpr as follows:
                //    let inheritsExpr = reps.FixupIncrClassExprPhase2C (Some(thisVal)) thisTyInst inheritsExpr
                // However, the rules of IL mean we are not actually allowed to capture arg0 
                // and store it as a closure field before the base class constructor is called.
                // 
                // As a result we do not rewrite the inheritsExpr and instead 
                //    (a) wrap a let binding for the ref cell around the inheritsExpr if needed
                //    (b) rely on the fact that the input arguments are in scope and can be accessed from as argument variables
                //    (c) rely on the fact that there are no 'let' bindings prior to the inherits expr.
                let inheritsExpr = 
                    match ctorInfo.InstanceCtorSafeThisValOpt with 
                    | None -> 
                        inheritsExpr
                    | Some v -> 
                        // Rewrite the expression to convert it to a load of a field if needed.
                        // We are allowed to load fields from our own object even though we haven't called
                        // the super class cosntructor yet.
                        let ldexpr = reps.FixupIncrClassExprPhase2C (Some(thisVal)) safeStaticInitInfo thisTyInst (exprForVal m v) 
                        mkInvisibleLet m v ldexpr inheritsExpr

                let spAtSuperInit = (if inheritsIsVisible then SequencePointsAtSeq else SuppressSequencePointOnExprOfSequential)
                mkSequential spAtSuperInit m inheritsExpr ctorBody

            // Add the normal <let/do bindings> 
            let ctorBody = List.foldBack (fun (_,binder) acc -> binder acc) ctorInitActionsPre ctorBody

            // Add the final wrapping to make this into a method
            let ctorBody = mkMemberLambdas m [] (Some(thisVal)) ctorInfo.InstanceCtorBaseValOpt [ctorInfo.InstanceCtorArgs] (ctorBody,cenv.g.unit_ty)

            ctorBody

        let cctorBodyOpt =
            /// Omit the .cctor if it's empty 
            match cctorInitActions with
            | [] -> None 
            | _ -> 
                let cctorInitAction = List.foldBack (fun (_,binder) acc -> binder acc) cctorInitActions (mkUnit cenv.g m)
                let m = thisVal.Range
                let cctorArgs,cctorVal,_ = ctorInfo.StaticCtorValInfo.Force()
                // Reconstitute the type of the implicit class constructor with the correct quantified type variables.
                cctorVal.SetType (tryMkForallTy ctorDeclaredTypars cctorVal.TauType)
                let cctorBody = mkMemberLambdas m [] None None [cctorArgs] (cctorInitAction,cenv.g.unit_ty)
                Some(cctorBody)
        
        ctorBody,cctorBodyOpt,methodBinds,reps




// Checking of mutually recursive types, members and 'let' bindings in classes
//
// Technique: multiple passes.
//   Phase1: create and establish type definitions and core representation information
//   Phase2A: create Vals for recursive items given names and args
//   Phase2B-D: type check AST to TAST collecting (sufficient) type constraints, 
//              generalize definitions, fix up recursive instances, build ctor binding
module MutRecBindingChecking = 

    open IncrClassChecking 

    /// Represents one element in a type definition, after the first phase    
    type TyconBindingPhase2A =
      /// An entry corresponding to the definition of the implicit constructor for a class
      | Phase2AIncrClassCtor     of IncrClassCtorLhs
      /// An 'inherit' declaration in an incremental class
      ///
      /// Phase2AInherit (typ,arg,baseValOpt,m)
      | Phase2AInherit           of SynType * SynExpr * Val option * range
      /// A set of value or function definitions in an incremental class
      ///
      /// Phase2AIncrClassBindings (tcref,letBinds,isStatic,isRec,m)
      | Phase2AIncrClassBindings of TyconRef * Ast.SynBinding list * bool * bool * range
      /// A 'member' definition in a class
      | Phase2AMember            of PreCheckingRecursiveBinding
#if OPEN_IN_TYPE_DECLARATIONS
      /// A dummy declaration, should we ever support 'open' in type definitions
      | Phase2AOpen              of LongIdent * range
#endif
      /// Indicates the super init has just been called, 'this' may now be published
      | Phase2AIncrClassCtorJustAfterSuperInit 
      /// Indicates the last 'field' has been initialized, only 'do' comes after 
      | Phase2AIncrClassCtorJustAfterLastLet

    /// The collected syntactic input definitions for a single type or type-extension definition
    type TyconBindingsPhase2A = 
      | TyconBindingsPhase2A of Tycon option * DeclKind * Val list * TyconRef * Typar list * TType * TyconBindingPhase2A list

    /// The collected syntactic input definitions for a recursive group of type or type-extension definitions
    type MutRecDefnsPhase2AData = MutRecShape<TyconBindingsPhase2A, PreCheckingRecursiveBinding list, MutRecDefnsPhase2DataForModule * TcEnv, MutRecDataForModuleAbbrev, MutRecDataForOpen>  list

    /// Represents one element in a type definition, after the second phase
    type TyconBindingPhase2B =
      | Phase2BIncrClassCtor     of IncrClassCtorLhs * Tast.Binding option 
      | Phase2BInherit           of Expr * Val option
      /// A set of value of function definitions in a class definition with an implicit consructor.
      | Phase2BIncrClassBindings of IncrClassBindingGroup list
      | Phase2BMember       of int
      /// An intermediate definition that represent the point in an implicit class definition where
      /// the super type has been initialized.
      | Phase2BIncrClassCtorJustAfterSuperInit
      /// An intermediate definition that represent the point in an implicit class definition where
      /// the last 'field' has been initialized, i.e. only 'do' and 'member' definitions come after 
      /// this point.
      | Phase2BIncrClassCtorJustAfterLastLet

    type TyconBindingsPhase2B = TyconBindingsPhase2B of Tycon option * TyconRef * TyconBindingPhase2B list

    type MutRecDefnsPhase2BData = MutRecShape<TyconBindingsPhase2B, int list, MutRecDefnsPhase2DataForModule * TcEnv, MutRecDataForModuleAbbrev, MutRecDataForOpen> list

    /// Represents one element in a type definition, after the third phase
    type TyconBindingPhase2C =
      | Phase2CIncrClassCtor     of IncrClassCtorLhs * Tast.Binding option 
      | Phase2CInherit           of Expr * Val option
      | Phase2CIncrClassBindings of IncrClassBindingGroup list
      | Phase2CMember            of PreInitializationGraphEliminationBinding
      // Indicates the last 'field' has been initialized, only 'do' comes after 
      | Phase2CIncrClassCtorJustAfterSuperInit     
      | Phase2CIncrClassCtorJustAfterLastLet     

    type TyconBindingsPhase2C = TyconBindingsPhase2C of Tycon option * TyconRef * TyconBindingPhase2C list

    type MutRecDefnsPhase2CData = MutRecShape<TyconBindingsPhase2C, PreInitializationGraphEliminationBinding list, MutRecDefnsPhase2DataForModule * TcEnv, MutRecDataForModuleAbbrev, MutRecDataForOpen> list



    // Phase2A: create member prelimRecValues for "recursive" items, i.e. ctor val and member vals 
    // Phase2A: also processes their arg patterns - collecting type assertions 
    let TcMutRecBindings_Phase2A_CreateRecursiveValuesAndCheckArgumentPatterns cenv tpenv (envMutRec, mutRecDefns : MutRecDefnsPhase2Info) =

        // The basic iteration over the declarations in a single type definition
        // State:
        //    tpenv:               floating type parameter environment
        //    recBindIdx:          index of the recursive binding
        //    prelimRecValuesRev:  accumulation of prelim value entries
        //    uncheckedBindsRev:   accumulation of unchecked bindings
        let (defnsAs: MutRecDefnsPhase2AData), (tpenv,_,uncheckedBindsRev) =
            let initialOuterState = (tpenv, 0, ([]: PreCheckingRecursiveBinding list))
            (initialOuterState, envMutRec, mutRecDefns) |||> MutRecShapes.mapFoldWithEnv (fun outerState envForDecls defn -> 
              let (tpenv,recBindIdx,uncheckedBindsRev) = outerState
              match defn with 
              | MutRecShape.Module _ ->  failwith "unreachable"
              | MutRecShape.Open x ->  MutRecShape.Open x, outerState 
              | MutRecShape.ModuleAbbrev x ->  MutRecShape.ModuleAbbrev x, outerState 
              | MutRecShape.Lets recBinds -> 
                let normRecDefns = 
                   [ for (RecDefnBindingInfo(a,b,c,bind)) in recBinds do 
                       yield NormalizedRecBindingDefn(a,b,c,BindingNormalization.NormalizeBinding ValOrMemberBinding cenv envForDecls bind) ]
                let bindsAndValues,(tpenv,recBindIdx) = ((tpenv,recBindIdx), normRecDefns) ||> List.mapFold (AnalyzeAndMakeAndPublishRecursiveValue ErrorOnOverrides false cenv envForDecls) 
                let binds = bindsAndValues |> List.collect fst

                let defnAs = MutRecShape.Lets binds
                defnAs,(tpenv,recBindIdx,List.rev binds @ uncheckedBindsRev)

              | MutRecShape.Tycon (MutRecDefnsPhase2InfoForTycon(tyconOpt, tcref, declaredTyconTypars, declKind, binds, _)) ->

                // Class members can access protected members of the implemented type 
                // Class members can access private members in the typ
                let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
                let innitalEnvForTycon = MakeInnerEnvForTyconRef cenv envForDecls tcref isExtrinsic 

                // Re-add the type constructor to make it take precedence for record label field resolutions
                // This does not apply to extension members: in those cases the relationship between the record labels
                // and the type is too extruded
                let envForTycon = 
                    if isExtrinsic then 
                        innitalEnvForTycon
                    else
                        AddLocalTyconRefs true cenv.g cenv.amap tcref.Range [tcref] innitalEnvForTycon

                // Make fresh version of the class type for type checking the members and lets *
                let _,copyOfTyconTypars,_,objTy,thisTy = FreshenObjectArgType cenv tcref.Range TyparRigidity.WillBeRigid tcref isExtrinsic declaredTyconTypars


                // The basic iteration over the declarations in a single type definition
                let initialInnerState = (None,envForTycon,tpenv,recBindIdx,uncheckedBindsRev)
                let defnAs,(_,_envForTycon,tpenv,recBindIdx,uncheckedBindsRev) = 

                    (initialInnerState,binds) ||> List.collectFold (fun innerState defn ->

                        let (TyconBindingDefn(containerInfo,newslotsOK,declKind,classMemberDef,m)) = defn
                        let (incrClassCtorLhsOpt,envForTycon,tpenv,recBindIdx,uncheckedBindsRev) = innerState

                        if tcref.IsTypeAbbrev then
                            // ideally we'd have the 'm' of the type declaration stored here, to avoid needing to trim to line to approx
                            error(Error(FSComp.SR.tcTypeAbbreviationsMayNotHaveMembers(),(trimRangeToLine m)))

                        if tcref.IsEnumTycon && (declKind <> ExtrinsicExtensionBinding) then error(Error(FSComp.SR.tcEnumerationsMayNotHaveMembers(),(trimRangeToLine m))) // ideally we'd have the 'm' of the type declaration stored here, to avoid needing to trim to line to approx

                        match classMemberDef, containerInfo with
                        | SynMemberDefn.ImplicitCtor (vis,attrs,spats,thisIdOpt, m), ContainerInfo(_,Some(MemberOrValContainerInfo(tcref, _, baseValOpt, safeInitInfo, _))) ->
                            if tcref.TypeOrMeasureKind = TyparKind.Measure then
                                error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m))

                            // Phase2A: make incrClassCtorLhs - ctorv, thisVal etc, type depends on argty(s) 
                            let incrClassCtorLhs = TcImplictCtorLhs_Phase2A(cenv,envForTycon,tpenv,tcref,vis,attrs,spats,thisIdOpt,baseValOpt,safeInitInfo,m,copyOfTyconTypars,objTy,thisTy)
                            // Phase2A: Add copyOfTyconTypars from incrClassCtorLhs - or from tcref 
                            let envForTycon = AddDeclaredTypars CheckForDuplicateTypars incrClassCtorLhs.InstanceCtorDeclaredTypars envForTycon
                            let innerState = (Some incrClassCtorLhs, envForTycon, tpenv, recBindIdx, uncheckedBindsRev)

                            [Phase2AIncrClassCtor incrClassCtorLhs],innerState
                              
                        | SynMemberDefn.ImplicitInherit (typ,arg,_baseIdOpt,m),_ ->
                            if tcref.TypeOrMeasureKind = TyparKind.Measure then
                                error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m))

                            // Phase2A: inherit typ(arg) as base - pass through 
                            // Phase2A: pick up baseValOpt! 
                            let baseValOpt = incrClassCtorLhsOpt |> Option.bind (fun x -> x.InstanceCtorBaseValOpt)
                            let innerState = (incrClassCtorLhsOpt,envForTycon,tpenv,recBindIdx,uncheckedBindsRev)
                            [Phase2AInherit (typ,arg,baseValOpt,m); Phase2AIncrClassCtorJustAfterSuperInit], innerState

                        | SynMemberDefn.LetBindings (letBinds,isStatic,isRec,m),_ ->
                            match tcref.TypeOrMeasureKind,isStatic with 
                            | TyparKind.Measure,false -> error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m)) 
                            | _ -> ()

                            if tcref.IsStructOrEnumTycon && not isStatic then 
                                let allDo = letBinds |> List.forall (function (Binding(_,DoBinding,_,_,_,_,_,_,_,_,_,_)) -> true | _ -> false)
                                // Code for potential future design change to allow functions-compiled-as-members in structs
                                if allDo then 
                                    errorR(Deprecated(FSComp.SR.tcStructsMayNotContainDoBindings(),(trimRangeToLine m)))
                                else
                                // Code for potential future design change to allow functions-compiled-as-members in structs
                                    errorR(Error(FSComp.SR.tcStructsMayNotContainLetBindings(),(trimRangeToLine m)))

                            if isStatic && Option.isNone incrClassCtorLhsOpt then 
                                errorR(Error(FSComp.SR.tcStaticLetBindingsRequireClassesWithImplicitConstructors(),m))
                              
                            // Phase2A: let-bindings - pass through 
                            let innerState = (incrClassCtorLhsOpt,envForTycon,tpenv,recBindIdx,uncheckedBindsRev)     
                            [Phase2AIncrClassBindings (tcref,letBinds,isStatic,isRec,m)], innerState
                              
                        | SynMemberDefn.Member (bind,m),_ ->
                            // Phase2A: member binding - create prelim valspec (for recursive reference) and RecursiveBindingInfo 
                            let (NormalizedBinding(_,_,_,_,_,_,_,valSynData,_,_,_,_)) as bind = BindingNormalization.NormalizeBinding ValOrMemberBinding cenv envForTycon bind
                            let (SynValData(memberFlagsOpt,_,_)) = valSynData 
                            match tcref.TypeOrMeasureKind with
                            | TyparKind.Type -> ()
                            | TyparKind.Measure ->
                                match memberFlagsOpt with 
                                | None -> () 
                                | Some memberFlags -> 
                                    if memberFlags.IsInstance then error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m))
                                    match memberFlags.MemberKind with 
                                    | MemberKind.Constructor -> error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembersNotConstructors(), m))
                                    | _ -> ()
                            let rbind = NormalizedRecBindingDefn(containerInfo,newslotsOK,declKind,bind)
                            let overridesOK  = DeclKind.CanOverrideOrImplement(declKind)
                            let (binds,_values),(tpenv,recBindIdx) = AnalyzeAndMakeAndPublishRecursiveValue overridesOK false cenv envForTycon (tpenv,recBindIdx) rbind
                            let cbinds = [ for rbind in binds -> Phase2AMember rbind ]

                            let innerState = (incrClassCtorLhsOpt, envForTycon, tpenv, recBindIdx, List.rev binds @ uncheckedBindsRev)
                            cbinds,innerState
                        
#if OPEN_IN_TYPE_DECLARATIONS
                        | SynMemberDefn.Open (mp,m),_ ->
                            let innerState = (incrClassCtorLhsOpt,env,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev)
                            [ Phase2AOpen (mp,m) ], innerState
#endif
                        
                        | _ -> 
                            error(InternalError("Unexpected definition",m)))

                // If no constructor call, insert Phase2AIncrClassCtorJustAfterSuperInit at start
                let defnAs = 
                    match defnAs with 
                    | (Phase2AIncrClassCtor _ as b1) :: rest -> 
                        let rest = 
                            if rest |> List.exists (function Phase2AIncrClassCtorJustAfterSuperInit -> true | _ -> false) then 
                                rest
                            else
                                Phase2AIncrClassCtorJustAfterSuperInit :: rest
                        // Insert Phase2AIncrClassCtorJustAfterLastLet at the point where local construction is known to have been finished 
                        let rest = 
                            let isAfter b = 
                                match b with 
#if OPEN_IN_TYPE_DECLARATIONS
                                | Phase2AOpen _ 
#endif
                                | Phase2AIncrClassCtor _ | Phase2AInherit _ | Phase2AIncrClassCtorJustAfterSuperInit -> false
                                | Phase2AIncrClassBindings (_,binds,_,_,_) -> binds |> List.exists (function (Binding (_,DoBinding,_,_,_,_,_,_,_,_,_,_)) -> true | _ -> false)
                                | Phase2AIncrClassCtorJustAfterLastLet
                                | Phase2AMember _ -> true
                            let restRev = List.rev rest
                            let afterRev = restRev |> List.takeWhile isAfter
                            let beforeRev = restRev |> List.skipWhile isAfter
                            
                            [ yield!  List.rev beforeRev
                              yield Phase2AIncrClassCtorJustAfterLastLet
                              yield! List.rev afterRev ]
                        b1 :: rest

                    // Cover the case where this is not a type with an implicit constructor.
                    | rest -> rest

                let prelimRecValues = [ for x in defnAs do match x with Phase2AMember bind -> yield bind.RecBindingInfo.Val | _ -> () ]
                let defnAs = MutRecShape.Tycon(TyconBindingsPhase2A(tyconOpt,declKind,prelimRecValues,tcref,copyOfTyconTypars,thisTy,defnAs))
                defnAs,(tpenv,recBindIdx,uncheckedBindsRev))

        let uncheckedRecBinds = List.rev uncheckedBindsRev

        (defnsAs, uncheckedRecBinds, tpenv)

    /// Phase2B: check each of the bindings, convert from ast to tast and collects type assertions.
    /// Also generalize incrementally.
    let TcMutRecBindings_Phase2B_TypeCheckAndIncrementalGeneralization cenv tpenv envInitial (envMutRec, defnsAs:MutRecDefnsPhase2AData, uncheckedRecBinds: PreCheckingRecursiveBinding list, scopem) : MutRecDefnsPhase2BData * _ * _ =

        let (defnsBs: MutRecDefnsPhase2BData), (tpenv, generalizedRecBinds, preGeneralizationRecBinds, _, _) = 

            let uncheckedRecBindsTable = uncheckedRecBinds  |> List.map (fun rbind  ->  rbind.RecBindingInfo.Val.Stamp, rbind) |> Map.ofList 

            // Loop through the types being defined...
            //
            // The envNonRec is the environment used to limit generalization to prevent leakage of type
            // variables into the types of 'let' bindings. It gets accumulated across type definitions, e.g.
            // consider
            //
            //   type A<'T>() =  
            //       let someFuncValue : 'A = A<'T>.Meth2()
            //       static member Meth2() = A<'T>.Meth2() 
            //   and B<'T>() =
            //       static member Meth1() = A<'T>.Meth2()
            //
            // Here 'A can't be generalized, even at 'Meth1'.
            //
            // The envForTycon is the environment used for name resolution within the let and member bindings
            // of the type definition. This becomes 'envStatic' and 'envInstance' for the two 
             
            let initialOuterState = (tpenv,([]: PostGeneralizationRecursiveBinding list),([]: PreGeneralizationRecursiveBinding list),uncheckedRecBindsTable,envInitial)

            (initialOuterState,envMutRec,defnsAs) |||> MutRecShapes.mapFoldWithEnv (fun outerState envForDecls defnsA -> 

              let (tpenv,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable,envNonRec) = outerState

              match defnsA with 
              | MutRecShape.Module _ -> failwith "unreachable"
              | MutRecShape.Open x ->  MutRecShape.Open x, outerState 
              | MutRecShape.ModuleAbbrev x ->  MutRecShape.ModuleAbbrev x, outerState 
              | MutRecShape.Lets binds ->
                
                let defnBs,(tpenv,_,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable) = 

                    let initialInnerState = (tpenv,envForDecls,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable)
                    (initialInnerState,binds) ||> List.mapFold  (fun innerState rbind -> 

                        let (tpenv,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable) = innerState

                        let (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, _, uncheckedRecBindsTable) = TcLetrecBinding (cenv,envStatic,scopem,[],None) (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, tpenv, uncheckedRecBindsTable) rbind
                             
                        let innerState = (tpenv, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable)
                        rbind.RecBindingInfo.Index, innerState)
                
                let outerState = (tpenv, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, envNonRec)
                MutRecShape.Lets defnBs, outerState

              | MutRecShape.Tycon (TyconBindingsPhase2A(tyconOpt, declKind, _, tcref, copyOfTyconTypars, thisTy, defnAs)) ->
                
                let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
                let envForTycon = MakeInnerEnvForTyconRef cenv envForDecls tcref isExtrinsic 
                let envForTycon = if isExtrinsic then envForTycon else AddLocalTyconRefs true cenv.g cenv.amap tcref.Range [tcref] envForTycon
                // Set up the environment so use-before-definition warnings are given, at least 
                // until we reach a Phase2AIncrClassCtorJustAfterSuperInit. 
                let envForTycon = { envForTycon with eCtorInfo = Some (InitialImplicitCtorInfo())  }

                let reqdThisValTyOpt = Some thisTy
                
                // Loop through the definition elements in a type...
                // State: 
                //      envInstance: the environment in scope in instance members
                //      envStatic: the environment in scope in static members
                //      envNonRec: the environment relevant to generalization
                //      generalizedRecBinds: part of the incremental generalization state
                //      preGeneralizationRecBinds: part of the incremental generalization state
                //      uncheckedRecBindsTable: part of the incremental generalization state
                let defnBs,(tpenv,_,_,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable) = 

                    let initialInnerState = (tpenv,envForTycon,envForTycon,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable)
                    (initialInnerState,defnAs) ||> List.mapFold  (fun innerState defnA -> 

                        let (tpenv,envInstance,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable) = innerState

                        match defnA with
                        // Phase2B for the definition of an implicit constructor. Enrich the instance environments
                        // with the implicit ctor args.
                        | Phase2AIncrClassCtor incrClassCtorLhs ->

                            let envInstance = AddDeclaredTypars CheckForDuplicateTypars incrClassCtorLhs.InstanceCtorDeclaredTypars envInstance
                            let envStatic = AddDeclaredTypars CheckForDuplicateTypars incrClassCtorLhs.InstanceCtorDeclaredTypars envStatic
                            let envInstance = match incrClassCtorLhs.InstanceCtorSafeThisValOpt with Some v -> AddLocalVal cenv.tcSink scopem v envInstance | None -> envInstance
                            let envInstance = List.foldBack AddLocalValPrimitive incrClassCtorLhs.InstanceCtorArgs envInstance 
                            let envNonRec   = match incrClassCtorLhs.InstanceCtorSafeThisValOpt with Some v -> AddLocalVal cenv.tcSink scopem v envNonRec | None -> envNonRec
                            let envNonRec   = List.foldBack AddLocalValPrimitive incrClassCtorLhs.InstanceCtorArgs envNonRec
                            let safeThisValBindOpt = TcLetrecComputeCtorSafeThisValBind cenv incrClassCtorLhs.InstanceCtorSafeThisValOpt

                            let innerState = (tpenv,envInstance,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable)
                            Phase2BIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt), innerState
                            
                        // Phase2B: typecheck the argument to an 'inherits' call and build the new object expr for the inherit-call 
                        | Phase2AInherit (synBaseTy,arg,baseValOpt,m) ->
                            let baseTy,tpenv = TcType cenv NoNewTypars CheckCxs ItemOccurence.Use envInstance tpenv synBaseTy
                            let inheritsExpr,tpenv = TcNewExpr cenv envInstance tpenv baseTy (Some synBaseTy.Range) true arg m
                            let envInstance = match baseValOpt with Some baseVal -> AddLocalVal cenv.tcSink scopem baseVal envInstance | None -> envInstance
                            let envNonRec   = match baseValOpt with Some baseVal -> AddLocalVal cenv.tcSink scopem baseVal envNonRec   | None -> envNonRec
                            let innerState = (tpenv,envInstance,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable)
                            Phase2BInherit (inheritsExpr,baseValOpt), innerState
                            
                        // Phase2B: let and let rec value and function definitions
                        | Phase2AIncrClassBindings (tcref,binds,isStatic,isRec,bindsm) ->
                            let envForBinding = if isStatic then envStatic else envInstance
                            let binds,bindRs,env,tpenv = 
                                if isRec then
                                
                                    // Type check local recursive binding 
                                    let binds = binds |> List.map (fun bind -> RecDefnBindingInfo(ExprContainerInfo,NoNewSlots,ClassLetBinding(isStatic),bind))
                                    let binds,env,tpenv = TcLetrec ErrorOnOverrides cenv envForBinding tpenv (binds,scopem(*bindsm*),scopem)
                                    let bindRs = [IncrClassBindingGroup(binds,isStatic,true)]
                                    binds,bindRs,env,tpenv 
                                else

                                    // Type check local binding 
                                    let binds,env,tpenv = TcLetBindings cenv envForBinding ExprContainerInfo (ClassLetBinding(isStatic)) tpenv (binds,bindsm,scopem)
                                    let binds,bindRs = 
                                        binds 
                                        |> List.map (function
                                            | TMDefLet(bind,_) -> [bind],IncrClassBindingGroup([bind],isStatic,false)
                                            | TMDefDo(e,_) -> [],IncrClassDo(e,isStatic)
                                            | _ -> error(InternalError("unexpected definition kind",tcref.Range)))
                                        |> List.unzip
                                    List.concat binds,bindRs,env,tpenv

                            let envNonRec = (envNonRec,binds) ||> List.fold (fun acc bind -> AddLocalValPrimitive bind.Var acc)

                            // Check to see that local bindings and members don't have the same name and check some other adhoc conditions
                            for bind in binds do 

                                if HasFSharpAttributeOpt cenv.g cenv.g.attrib_DllImportAttribute bind.Var.Attribs && not isStatic then 
                                    errorR(Error(FSComp.SR.tcDllImportNotAllowed(),bind.Var.Range))
                                    
                                let nm = bind.Var.DisplayName
                                let ty = generalizedTyconRef tcref
                                let ad = envNonRec.eAccessRights
                                match TryFindIntrinsicMethInfo cenv.infoReader bind.Var.Range ad nm ty,
                                      TryFindPropInfo cenv.infoReader bind.Var.Range ad nm ty with 
                                | [],[] -> ()
                                | _ -> errorR (Error(FSComp.SR.tcMemberAndLocalClassBindingHaveSameName(nm),bind.Var.Range))

                            // Also add static entries to the envInstance if necessary 
                            let envInstance = (if isStatic then (binds,envInstance) ||> List.foldBack (fun b e -> AddLocalVal cenv.tcSink scopem b.Var e)  else env)
                            let envStatic = (if isStatic then env else envStatic)
                            let innerState = (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable)
                            Phase2BIncrClassBindings bindRs,innerState
                              
                        | Phase2AIncrClassCtorJustAfterSuperInit -> 
                            let innerState = (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable)
                            Phase2BIncrClassCtorJustAfterSuperInit, innerState
                            
                        | Phase2AIncrClassCtorJustAfterLastLet -> 
                            let innerState = (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable)
                            Phase2BIncrClassCtorJustAfterLastLet , innerState
                            
                            
#if OPEN_IN_TYPE_DECLARATIONS
                        | Phase2AOpen(mp,m) -> 
                            let envInstance = TcOpenDecl cenv.tcSink cenv.g cenv.amap m scopem envInstance mp
                            let envStatic = TcOpenDecl cenv.tcSink cenv.g cenv.amap m scopem envStatic mp
                            let innerState = (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable)
                            Phase2BOpen,innerState
#endif


                        // Note: this path doesn't add anything the environment, because the member is already available off via its type 
                        
                        | Phase2AMember rbind ->

                            // Phase2B: Typecheck member binding, generalize them later, when all type constraints are known 
                            // static members are checked under envStatic.
                            // envStatic contains class typars and the (ungeneralized) members on the class(es).
                            // envStatic has no instance-variables (local let-bindings or ctor args). 

                            let v = rbind.RecBindingInfo .Val
                            let envForBinding = if v.IsInstanceMember then envInstance else envStatic

                            // Type variables derived from the type definition (or implicit constructor) are always generalizable (we check their generalizability later).
                            // Note they may be solved to be equi-recursive.
                            let extraGeneralizableTypars =  copyOfTyconTypars

                            // Inside the incremental class sytntax we assert the type of the 'this' variable to be precisely the same type as the 
                            // this variable for the implicit class constructor. For static members, we assert the type variables associated
                            // for the class to be identical to those used for the implicit class constructor and the static class constructor.
                            //
                            // See TcLetrecBinding where this information is consumed.

                            // Type check the member and apply early generalization.
                            // We ignore the tpenv returned by checking each member. Each member gets checked in a fresh, clean tpenv
                            let (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, _, uncheckedRecBindsTable) = 
                                TcLetrecBinding (cenv,envForBinding,scopem,extraGeneralizableTypars,reqdThisValTyOpt) (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, tpenv, uncheckedRecBindsTable) rbind
                             
                            let innerState = (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable)
                            Phase2BMember rbind.RecBindingInfo.Index, innerState)
                
                let defnBs = MutRecShape.Tycon (TyconBindingsPhase2B(tyconOpt, tcref, defnBs))
                let outerState = (tpenv, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, envNonRec)
                defnBs, outerState)

        // There should be no bindings that have not been generalized since checking the vary last binding always
        // results in the generalization of all remaining ungeneralized bindings, since there are no remaining unchecked bindings
        // to prevent the generalization 
        assert preGeneralizationRecBinds.IsEmpty

        defnsBs, generalizedRecBinds, tpenv


    // Choose type scheme implicit constructors and adjust their recursive types.
    // Fixup recursive references to members.
    let TcMutRecBindings_Phase2C_FixupRecursiveReferences cenv (denv, defnsBs: MutRecDefnsPhase2BData, generalizedTyparsForRecursiveBlock: Typar list, generalizedRecBinds: PostGeneralizationRecursiveBinding list, scopem) =
        // Build an index ---> binding map
        let generalizedBindingsMap = generalizedRecBinds |> List.map (fun pgrbind -> (pgrbind.RecBindingInfo.Index, pgrbind)) |> Map.ofList

        defnsBs |> MutRecShapes.mapTyconsAndLets 

            // Phase2C: Fixup member bindings 
            (fun (TyconBindingsPhase2B(tyconOpt, tcref, defnBs)) -> 

                let defnCs = 
                    defnBs |> List.map (fun defnB -> 

                        // Phase2C: Generalise implicit ctor val 
                        match defnB with
                        | Phase2BIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt) ->
                            let valscheme = incrClassCtorLhs.InstanceCtorValScheme
                            let valscheme = ChooseCanonicalValSchemeAfterInference cenv.g denv valscheme scopem
                            AdjustRecType cenv incrClassCtorLhs.InstanceCtorVal valscheme
                            Phase2CIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt)

                        | Phase2BInherit (inheritsExpr,basevOpt) -> 
                            Phase2CInherit (inheritsExpr,basevOpt)

                        | Phase2BIncrClassBindings bindRs             -> 
                            Phase2CIncrClassBindings bindRs

                        | Phase2BIncrClassCtorJustAfterSuperInit -> 
                            Phase2CIncrClassCtorJustAfterSuperInit

                        | Phase2BIncrClassCtorJustAfterLastLet -> 
                            Phase2CIncrClassCtorJustAfterLastLet

                        | Phase2BMember idx  ->
                            // Phase2C: Fixup member bindings 
                            let generalizedBinding = generalizedBindingsMap.[idx] 
                            let vxbind = TcLetrecAdjustMemberForSpecialVals cenv generalizedBinding
                            let pgbrind = FixupLetrecBind cenv denv generalizedTyparsForRecursiveBlock  vxbind
                            Phase2CMember pgbrind)
                TyconBindingsPhase2C(tyconOpt, tcref, defnCs))

            // Phase2C: Fixup let bindings 
            (fun bindIdxs -> 
                    [ for idx in bindIdxs do 
                        let generalizedBinding = generalizedBindingsMap.[idx] 
                        let vxbind = TcLetrecAdjustMemberForSpecialVals cenv generalizedBinding
                        yield FixupLetrecBind cenv denv generalizedTyparsForRecursiveBlock  vxbind ])


    // --- Extract field bindings from let-bindings 
    // --- Extract method bindings from let-bindings 
    // --- Extract bindings for implicit constructors
    let TcMutRecBindings_Phase2D_ExtractImplicitFieldAndMethodBindings cenv envMutRec tpenv (denv, generalizedTyparsForRecursiveBlock, defnsCs: MutRecDefnsPhase2CData) =

      //  let (fixupValueExprBinds, methodBinds) = 
            (envMutRec, defnsCs) ||> MutRecShapes.mapTyconsWithEnv (fun envForDecls (TyconBindingsPhase2C(tyconOpt, tcref,defnCs)) -> 
                match defnCs with 
                | Phase2CIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt) :: defnCs -> 

                    // Determine is static fields in this type need to be "protected" against invalid recursive initialization
                    let safeStaticInitInfo = 
                        // Safe static init checks are not added to FSharp.Core. The FailInit helper is not defined in some places, and 
                        // there are some minor concerns about performance w.r.t. these static bindings:
                        //
                        // set.fs (also map.fs)
                        //       static let empty : Set<'T> = 
                        //           let comparer = LanguagePrimitives.FastGenericComparer<'T> 
                        //           new Set<'T>(comparer, SetEmpty)
                        //
                        // prim-types.fs:
                        //       type TypeInfo<'T>() = 
                        //          static let info = 
                        //              let ty = typeof<'T>
                        //              ...
                        // and some others in prim-types.fs
                        //
                        // REVIEW: consider also turning them off for FSharp.Compiler: && cenv.topCcu.AssemblyName <> "FSharp.Compiler" and more
                        // generally allowing an optimization switch to turn off these checks

                        let needsSafeStaticInit = not cenv.g.compilingFslib
                        
                        // We only need safe static init checks if there are some static field bindings (actually, we look for non-method bindings)
                        let hasStaticBindings = 
                            defnCs |> List.exists (function 
                                | Phase2CIncrClassBindings groups -> 
                                    groups |> List.exists (function 
                                        | IncrClassBindingGroup(binds,isStatic,_) ->
                                            isStatic && (binds |> List.exists (IncrClassReprInfo.IsMethodRepr cenv >> not)) 
                                        | _ -> false) 
                                | _ -> false)

                        if needsSafeStaticInit && hasStaticBindings then
                            let rfield =  MakeSafeInitField cenv.g envForDecls tcref.Range true
                            SafeInitField(mkRecdFieldRef tcref rfield.Name, rfield)
                        else
                            NoSafeInitInfo


                    // This is the type definition we're processing  
                    let tcref = incrClassCtorLhs.TyconRef

                    // Assumes inherit call immediately follows implicit ctor. Checked by CheckMembersForm 
                    let inheritsExpr,inheritsIsVisible,_,defnCs = 
                        match defnCs |> List.partition (function Phase2CInherit _ -> true | _ -> false) with
                        | [Phase2CInherit (inheritsExpr,baseValOpt)], defnCs -> 
                            inheritsExpr,true,baseValOpt,defnCs

                        | _ ->
                            if tcref.IsStructOrEnumTycon then 
                                mkUnit cenv.g tcref.Range, false,None, defnCs
                            else
                                let inheritsExpr,_ = TcNewExpr cenv envForDecls tpenv cenv.g.obj_ty None true (SynExpr.Const(SynConst.Unit,tcref.Range)) tcref.Range
                                inheritsExpr,false,None,defnCs
                       
                    let envForTycon = MakeInnerEnvForTyconRef cenv envForDecls tcref false 

                    // Compute the cpath used when creating the hidden fields 
                    let cpath = envForTycon.eAccessPath

                    let localDecs  = 
                        defnCs |> List.filter (function 
                            | Phase2CIncrClassBindings _ 
                            | Phase2CIncrClassCtorJustAfterSuperInit 
                            | Phase2CIncrClassCtorJustAfterLastLet -> true 
                            | _ -> false)
                    let memberBindsWithFixups = defnCs |> List.choose (function Phase2CMember pgrbind -> Some pgrbind | _ -> None) 

                    // Extend localDecs with "let safeThisVal = ref null" if there is a safeThisVal
                    let localDecs  = 
                        match safeThisValBindOpt with 
                        | None -> localDecs 
                        | Some bind -> Phase2CIncrClassBindings [IncrClassBindingGroup([bind],false,false)] :: localDecs
                        
                    // Carve out the initialization sequence and decide on the localRep 
                    let ctorBodyLambdaExpr,cctorBodyLambdaExprOpt,methodBinds,localReps = 
                        
                        let localDecs = 
                            [ for localDec in localDecs do 
                                  match localDec with 
                                  | Phase2CIncrClassBindings(binds) -> yield Phase2CBindings binds
                                  | Phase2CIncrClassCtorJustAfterSuperInit  -> yield Phase2CCtorJustAfterSuperInit
                                  | Phase2CIncrClassCtorJustAfterLastLet -> yield Phase2CCtorJustAfterLastLet
                                  | _ -> () ]
                        let memberBinds = memberBindsWithFixups |> List.map (fun x -> x.Binding) 
                        MakeCtorForIncrClassConstructionPhase2C(cenv,envForTycon,incrClassCtorLhs,inheritsExpr,inheritsIsVisible,localDecs,memberBinds,generalizedTyparsForRecursiveBlock,safeStaticInitInfo)

                    // Generate the (value,expr) pairs for the implicit 
                    // object constructor and implicit static initializer 
                    let ctorValueExprBindings = 
                        [ (let ctorValueExprBinding = TBind(incrClassCtorLhs.InstanceCtorVal,ctorBodyLambdaExpr,NoSequencePointAtStickyBinding)
                           let rbind = { ValScheme = incrClassCtorLhs.InstanceCtorValScheme ; Binding = ctorValueExprBinding }
                           FixupLetrecBind cenv envForDecls.DisplayEnv generalizedTyparsForRecursiveBlock rbind) ]
                        @ 
                        ( match cctorBodyLambdaExprOpt with 
                          | None -> []
                          | Some(cctorBodyLambdaExpr) -> 
                              [ (let _,cctorVal, cctorValScheme = incrClassCtorLhs.StaticCtorValInfo.Force()
                                 let cctorValueExprBinding = TBind(cctorVal,cctorBodyLambdaExpr,NoSequencePointAtStickyBinding)
                                 let rbind = { ValScheme = cctorValScheme; Binding =  cctorValueExprBinding  }
                                 FixupLetrecBind cenv envForDecls.DisplayEnv generalizedTyparsForRecursiveBlock rbind) ] ) 

                    // Publish the fields of the representation to the type 
                    localReps.PublishIncrClassFields (cenv, denv, cpath, incrClassCtorLhs, safeStaticInitInfo) (* mutation *)    
                    
                    // Fixup members
                    let memberBindsWithFixups = 
                        memberBindsWithFixups |> List.map (fun pgrbind -> 
                            let (TBind(v,x,spBind)) = pgrbind.Binding

                            // Work out the 'this' variable and type instantiation for field fixups. 
                            // We use the instantiation from the instance member if any. Note: It is likely this is not strictly needed 
                            // since we unify the types of the 'this' variables with those of the ctor declared typars. 
                            let thisValOpt = GetInstanceMemberThisVariable (v,x)

                            // Members have at least as many type parameters as the enclosing class. Just grab the type variables for the type.
                            let thisTyInst = List.map mkTyparTy (List.take (tcref.Typars(v.Range).Length) v.Typars)
                                    
                            let x = localReps.FixupIncrClassExprPhase2C thisValOpt safeStaticInitInfo thisTyInst x 

                            { pgrbind with Binding = TBind(v,x,spBind) } )
                        
                    tyconOpt, ctorValueExprBindings @ memberBindsWithFixups, methodBinds  
                
                // Cover the case where this is not a class with an implicit constructor
                | defnCs -> 
                    let memberBindsWithFixups = defnCs |> List.choose (function Phase2CMember pgrbind -> Some pgrbind | _ -> None) 
                    tyconOpt, memberBindsWithFixups,[])

    /// Check a "module X = A.B.C" module abbreviation declaration
    let TcModuleAbbrevDecl (cenv:cenv) scopem env (id,p,m) = 
        let ad = env.eAccessRights
        let mvvs = ForceRaise (ResolveLongIndentAsModuleOrNamespace ResultCollectionSettings.AllResults cenv.amap m OpenQualified env.eNameResEnv ad p)
        let modrefs = mvvs |> List.map p23 
        if modrefs.Length > 0 && modrefs |> List.forall (fun modref -> modref.IsNamespace) then 
            errorR(Error(FSComp.SR.tcModuleAbbreviationForNamespace(fullDisplayTextOfModRef (List.head modrefs)),m))
        let modrefs = modrefs |> List.filter (fun mvv -> not mvv.IsNamespace)
        modrefs |> List.iter (fun modref -> CheckEntityAttributes cenv.g modref m |> CommitOperationResult)        
        let env = (if modrefs.Length > 0 then AddModuleAbbreviationAndReport cenv.tcSink scopem id modrefs env else env)
        env


    /// Update the contents accessible via the recursive namespace declaration, if any
    let TcMutRecDefns_UpdateNSContents mutRecNSInfo =
        match mutRecNSInfo with 
        | Some (Some (mspecNS: ModuleOrNamespace), mtypeAcc) -> 
            mspecNS.entity_modul_contents <- MaybeLazy.Strict !mtypeAcc
        | _ -> ()  

    /// Updates the types of the modules to contain the contents so far
    let TcMutRecDefns_UpdateModuleContents mutRecNSInfo defns =
        defns |> MutRecShapes.iterModules (fun (MutRecDefnsPhase2DataForModule (mtypeAcc, mspec), _) -> 
              mspec.entity_modul_contents <- MaybeLazy.Strict !mtypeAcc)  

        TcMutRecDefns_UpdateNSContents mutRecNSInfo
    
    /// Compute the active environments within each nested module.
    let TcMutRecDefns_ComputeEnvs getTyconOpt getVals (cenv: cenv) report scopem m envInitial mutRecShape =
        (envInitial, mutRecShape) ||> MutRecShapes.computeEnvs 
            (fun envAbove (MutRecDefnsPhase2DataForModule (mtypeAcc, mspec)) ->  MakeInnerEnvWithAcc envAbove mspec.Id mtypeAcc mspec.ModuleOrNamespaceType.ModuleOrNamespaceKind)
            (fun envAbove decls -> 

                // Collect the type definitions, exception definitions, modules and "open" declarations
                let tycons =  decls |> List.choose (function MutRecShape.Tycon d -> getTyconOpt d | _ -> None) 
                let mspecs = decls |> List.choose (function MutRecShape.Module (MutRecDefnsPhase2DataForModule (_, mspec),_) -> Some mspec | _ -> None)
                let moduleAbbrevs = decls |> List.choose (function MutRecShape.ModuleAbbrev (MutRecDataForModuleAbbrev (id,mp,m)) -> Some (id,mp,m) | _ -> None)
                let opens = decls |> List.choose (function MutRecShape.Open (MutRecDataForOpen (mp,m)) -> Some (mp,m) | _ -> None)
                let lets = decls |> List.collect (function MutRecShape.Lets binds -> getVals binds | _ -> [])
                let exns = tycons |> List.filter (fun (tycon:Tycon) -> tycon.IsExceptionDecl)

                // Add the type definitions, exceptions, modules and "open" declarations.
                // The order here is sensitive.  The things added first will be resolved in an environment
                // where not everything has been added.  The things added last will be preferred in name 
                // resolution.
                //
                // 'open' declarations ('open M') may refer to modules being defined ('M') and so must be
                // processed in an environment where 'M' is present.  However, in later processing the names of 
                // modules being defined ('M') take precedence over those coming from 'open' declarations.  
                // So add the names of the modules being defined to the environment twice - once to allow 
                // the processing of 'open M', and once to allow the correct name resolution of 'M'.
                //
                // Module abbreviations being defined ('module M = A.B.C') are not available for use in 'open'
                // declarations.  So
                //    namespace rec N = 
                //       open M
                //       module M = FSharp.Core.Operators
                // is not allowed.

                let envForDecls = envAbove
                // Add the modules being defined
                let envForDecls = (envForDecls, mspecs) ||> List.fold ((if report then AddLocalSubModuleAndReport cenv.tcSink scopem else AddLocalSubModule) cenv.g cenv.amap m)
                // Process the 'open' declarations                
                let envForDecls = (envForDecls, opens) ||> List.fold (fun env (mp,m) -> TcOpenDecl cenv.tcSink cenv.g cenv.amap m scopem env mp)
                // Add the type definitions being defined
                let envForDecls = (if report then AddLocalTyconsAndReport cenv.tcSink scopem else AddLocalTycons) cenv.g cenv.amap m tycons envForDecls 
                // Add the exception definitions being defined
                let envForDecls = (envForDecls, exns) ||> List.fold (AddLocalExnDefnAndReport cenv.tcSink scopem)
                // Add the modules again (but don't report them a second time)
                let envForDecls = (envForDecls, mspecs) ||> List.fold (AddLocalSubModule cenv.g cenv.amap m)
                // Add the module abbreviations
                let envForDecls = (envForDecls, moduleAbbrevs) ||> List.fold (TcModuleAbbrevDecl cenv scopem)
                // Add the values and members
                let envForDecls = AddLocalVals cenv.tcSink scopem lets envForDecls
                envForDecls)

    /// Phase 2: Check the members and 'let' definitions in a mutually recursive group of definitions.
    let TcMutRecDefns_Phase2_Bindings cenv envInitial tpenv bindsm scopem mutRecNSInfo (envMutRecPrelimWithReprs: TcEnv) (mutRecDefns: MutRecDefnsPhase2Info) =
        let g = cenv.g
        let denv = envMutRecPrelimWithReprs.DisplayEnv
        
        // Phase2A: create member prelimRecValues for "recursive" items, i.e. ctor val and member vals 
        // Phase2A: also processes their arg patterns - collecting type assertions 
        let (defnsAs, uncheckedRecBinds, tpenv) =  TcMutRecBindings_Phase2A_CreateRecursiveValuesAndCheckArgumentPatterns cenv tpenv (envMutRecPrelimWithReprs, mutRecDefns)

        // Now basic member values are created we can compute the final attributes (i.e. in the case where attributes refer to constructors being defined)
        mutRecDefns |> MutRecShapes.iterTycons (fun (MutRecDefnsPhase2InfoForTycon(_, _, _, _, _, fixupFinalAttrs)) -> 
                fixupFinalAttrs())  

        // Updates the types of the modules to contain the contents so far, which now includes values and members
        TcMutRecDefns_UpdateModuleContents mutRecNSInfo defnsAs

        // Updates the environments to include the values
        // We must open all modules from scratch again because there may be extension methods and/or AutoOpen
        let envMutRec, defnsAs =  
            (envInitial, MutRecShapes.dropEnvs defnsAs) 
            ||> TcMutRecDefns_ComputeEnvs 
                   (fun (TyconBindingsPhase2A(tyconOpt,_,_,_,_,_,_)) -> tyconOpt) 
                   (fun binds ->  [ for bind in binds -> bind.RecBindingInfo.Val ]) 
                   cenv false scopem scopem 
            ||> MutRecShapes.extendEnvs (fun envForDecls decls -> 

                let prelimRecValues =  
                    decls |> List.collect (function 
                        | MutRecShape.Tycon (TyconBindingsPhase2A(_,_,prelimRecValues,_,_,_,_)) -> prelimRecValues 
                        | MutRecShape.Lets binds -> [ for bind in binds -> bind.RecBindingInfo.Val ] 
                        | _ -> [])

                let ctorVals = 
                    decls |> MutRecShapes.topTycons |> List.collect (fun (TyconBindingsPhase2A(_, _, _, _, _, _, defnAs)) ->
                    [ for defnB in defnAs do
                        match defnB with
                        | Phase2AIncrClassCtor (incrClassCtorLhs) -> yield incrClassCtorLhs.InstanceCtorVal
                        | _ -> ()  ])

                let envForDeclsUpdated = 
                    envForDecls
                    |> AddLocalVals cenv.tcSink scopem prelimRecValues 
                    |> AddLocalVals cenv.tcSink scopem ctorVals 

                envForDeclsUpdated)

        // Phase2B: type check pass, convert from ast to tast and collects type assertions, and generalize
        let defnsBs, generalizedRecBinds, tpenv = TcMutRecBindings_Phase2B_TypeCheckAndIncrementalGeneralization cenv tpenv envInitial (envMutRec, defnsAs, uncheckedRecBinds, scopem)


        let generalizedTyparsForRecursiveBlock = 
             generalizedRecBinds 
                |> List.map (fun pgrbind -> pgrbind.GeneralizedTypars)
                |> unionGeneralizedTypars

        // Check the escape condition for all extraGeneralizableTypars.
        // First collect up all the extraGeneralizableTypars.
        let allExtraGeneralizableTypars = 
            defnsAs |> MutRecShapes.collectTycons |> List.collect (fun (TyconBindingsPhase2A(_, _, _, _, copyOfTyconTypars, _, defnAs)) ->
                [ yield! copyOfTyconTypars
                  for defnA in defnAs do 
                      match defnA with
                      | Phase2AMember rbind -> yield! rbind.RecBindingInfo.EnclosingDeclaredTypars
                      | _ ->  ()   ])

        // Now check they don't escape the overall scope of the recursive set of types
        if not (isNil allExtraGeneralizableTypars) then         
            let freeInInitialEnv = GeneralizationHelpers.ComputeUngeneralizableTypars envInitial
            for extraTypar in allExtraGeneralizableTypars do 
                if Zset.memberOf freeInInitialEnv extraTypar then
                    let ty =  mkTyparTy extraTypar
                    error(Error(FSComp.SR.tcNotSufficientlyGenericBecauseOfScope(NicePrint.prettyStringOfTy denv ty),extraTypar.Range))                                

        // Solve any type variables in any part of the overall type signature of the class whose
        // constraints involve generalized type variables.
        //
        // This includes property, member and constructor argument types that couldn't be fully generalized because they
        // involve generalized copies of class type variables.
        let unsolvedTyparsForRecursiveBlockInvolvingGeneralizedVariables = 
             let genSet = (freeInTypes CollectAllNoCaching [ for tp in generalizedTyparsForRecursiveBlock -> mkTyparTy tp ]).FreeTypars
             //printfn "genSet.Count = %d" genSet.Count
             let allTypes = 
                 [ for pgrbind in generalizedRecBinds do 
                      yield pgrbind.RecBindingInfo.Val.Type 
                   for (TyconBindingsPhase2B(_tyconOpt, _tcref, defnBs)) in MutRecShapes.collectTycons defnsBs do
                      for defnB in defnBs do
                        match defnB with
                        | Phase2BIncrClassCtor (incrClassCtorLhs, _) ->
                            yield incrClassCtorLhs.InstanceCtorVal.Type
                        | _ -> 
                            ()
                  ]
             //printfn "allTypes.Length = %d" allTypes.Length
             let unsolvedTypars = freeInTypesLeftToRight g true allTypes
             //printfn "unsolvedTypars.Length = %d" unsolvedTypars.Length
             //for x in unsolvedTypars do 
             //    printfn "unsolvedTypar : %s #%d" x.DisplayName x.Stamp
             let unsolvedTyparsInvolvingGeneralizedVariables =
                 unsolvedTypars |> List.filter (fun tp -> 
                     let freeInTypar = (freeInType CollectAllNoCaching (mkTyparTy tp)).FreeTypars
                     // Check it is not one of the generalized variables...
                     not (genSet.Contains tp) && 
                     // Check it involves a generalized variable in one of its constraints...
                     freeInTypar.Exists(fun otherTypar -> genSet.Contains otherTypar))
             //printfn "unsolvedTyparsInvolvingGeneralizedVariables.Length = %d" unsolvedTyparsInvolvingGeneralizedVariables.Length
             //for x in unsolvedTypars do 
             //    printfn "unsolvedTyparsInvolvingGeneralizedVariable : %s #%d" x.DisplayName x.Stamp
             unsolvedTyparsInvolvingGeneralizedVariables

        for tp in unsolvedTyparsForRecursiveBlockInvolvingGeneralizedVariables do
            //printfn "solving unsolvedTyparsInvolvingGeneralizedVariable : %s #%d" tp.DisplayName tp.Stamp
            if (tp.Rigidity <> TyparRigidity.Rigid) && not tp.IsSolved then 
                ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css denv tp
          
        // Now that we know what we've generalized we can adjust the recursive references 
        let defnsCs = TcMutRecBindings_Phase2C_FixupRecursiveReferences cenv (denv, defnsBs, generalizedTyparsForRecursiveBlock, generalizedRecBinds, scopem)

        // --- Extract field bindings from let-bindings 
        // --- Extract method bindings from let-bindings 
        // --- Extract bindings for implicit constructors
        let defnsDs = TcMutRecBindings_Phase2D_ExtractImplicitFieldAndMethodBindings cenv envMutRec tpenv (denv, generalizedTyparsForRecursiveBlock, defnsCs)
        
        // Phase2E - rewrite values to initialization graphs
        let defnsEs = 
           EliminateInitializationGraphs 
             p23
             (fun morpher (tyconOpt,fixupValueExprBinds,methodBinds) -> (tyconOpt, morpher fixupValueExprBinds @ methodBinds))
             id
             (fun morpher oldBinds -> morpher oldBinds)
             g true denv defnsDs bindsm 
        
        defnsEs,envMutRec

/// Check and generalize the interface implementations, members, 'let' definitions in a mutually recursive group of definitions.
let TcMutRecDefns_Phase2 cenv envInitial bindsm scopem mutRecNSInfo (envMutRec: TcEnv) (mutRecDefns: MutRecDefnsPhase2Data) = 
    let interfacesFromTypeDefn envForTycon tyconMembersData = 
        let (MutRecDefnsPhase2DataForTycon(_, _, declKind, tcref, _, _, declaredTyconTypars, members, _, _, _)) = tyconMembersData
        let overridesOK  = DeclKind.CanOverrideOrImplement(declKind)
        members |> List.collect (function 
            | SynMemberDefn.Interface(ity,defnOpt,_) -> 
                  let _,typ = if tcref.Deref.IsExceptionDecl then [],cenv.g.exn_ty else generalizeTyconRef tcref
                  let m = ity.Range
                  if tcref.IsTypeAbbrev then error(Error(FSComp.SR.tcTypeAbbreviationsCannotHaveInterfaceDeclaration(),m))
                  if tcref.IsEnumTycon then error(Error(FSComp.SR.tcEnumerationsCannotHaveInterfaceDeclaration(),m))

                  let ity' = 
                      let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTyconTypars envForTycon
                      TcTypeAndRecover cenv NoNewTypars CheckCxs ItemOccurence.UseInType  envinner emptyUnscopedTyparEnv ity |> fst
                  if not (isInterfaceTy cenv.g ity') then errorR(Error(FSComp.SR.tcTypeIsNotInterfaceType0(),ity.Range))
                  
                  if not (tcref.HasInterface cenv.g ity') then 
                      error(Error(FSComp.SR.tcAllImplementedInterfacesShouldBeDeclared(),ity.Range))
                  if (typeEquiv cenv.g ity' cenv.g.mk_IComparable_ty && Option.isSome tcref.GeneratedCompareToValues) || 
                      (typeEquiv cenv.g ity' cenv.g.mk_IStructuralComparable_ty && Option.isSome tcref.GeneratedCompareToWithComparerValues) ||
                      (typeEquiv cenv.g ity' ((mkAppTy cenv.g.system_GenericIComparable_tcref [typ])) && Option.isSome tcref.GeneratedCompareToValues) ||
                      (typeEquiv cenv.g ity' ((mkAppTy cenv.g.system_GenericIEquatable_tcref [typ])) && Option.isSome tcref.GeneratedHashAndEqualsWithComparerValues) ||
                      (typeEquiv cenv.g ity' cenv.g.mk_IStructuralEquatable_ty && Option.isSome tcref.GeneratedHashAndEqualsWithComparerValues) then
                      errorR(Error(FSComp.SR.tcDefaultImplementationForInterfaceHasAlreadyBeenAdded(),ity.Range))
                  if overridesOK = WarnOnOverrides then  
                      warning(IntfImplInIntrinsicAugmentation(ity.Range))
                  if overridesOK = ErrorOnOverrides then  
                      errorR(IntfImplInExtrinsicAugmentation(ity.Range))
                  match defnOpt with 
                  | Some(defn) -> [ (ity',defn,m) ]
                  | _-> []
                  
            | _ -> []) 

    let interfaceMembersFromTypeDefn tyconMembersData (ity',defn,_) implTySet  = 
        let (MutRecDefnsPhase2DataForTycon(_, parent, declKind, tcref, baseValOpt, safeInitInfo, declaredTyconTypars, _, _, newslotsOK, _)) = tyconMembersData
        let containerInfo = ContainerInfo(parent, Some(MemberOrValContainerInfo(tcref, Some(ity',implTySet), baseValOpt, safeInitInfo, declaredTyconTypars)))
        defn  |> List.choose (fun mem ->
            match mem with
            | SynMemberDefn.Member(_,m) -> Some(TyconBindingDefn(containerInfo,newslotsOK,declKind,mem,m))
            | SynMemberDefn.AutoProperty(_,_,_,_,_,_,_,_,_,_,m) -> Some(TyconBindingDefn(containerInfo,newslotsOK,declKind,mem,m))
            | _ -> errorR(Error(FSComp.SR.tcMemberNotPermittedInInterfaceImplementation(),mem.Range)); None)

    let tyconBindingsOfTypeDefn (MutRecDefnsPhase2DataForTycon(_, parent, declKind, tcref, baseValOpt, safeInitInfo, declaredTyconTypars, members, _, newslotsOK, _)) = 
        let containerInfo = ContainerInfo(parent,Some(MemberOrValContainerInfo(tcref, None, baseValOpt, safeInitInfo, declaredTyconTypars)))
        members 
        |> List.choose (fun memb ->
            match memb with 
            | SynMemberDefn.ImplicitCtor _
            | SynMemberDefn.ImplicitInherit _
            | SynMemberDefn.LetBindings _
            | SynMemberDefn.AutoProperty _
            | SynMemberDefn.Member _
            | SynMemberDefn.Open _
                -> Some(TyconBindingDefn(containerInfo,newslotsOK,declKind,memb,memb.Range))

            // Interfaces exist in the member list - handled above in interfaceMembersFromTypeDefn 
            | SynMemberDefn.Interface _  -> None

            // The following should have been List.unzip out already in SplitTyconDefn 
            | SynMemberDefn.AbstractSlot _
            | SynMemberDefn.ValField _             
            | SynMemberDefn.Inherit _    -> error(InternalError("Unexpected declaration element",memb.Range))
            | SynMemberDefn.NestedType _  -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),memb.Range)))
          
    let tpenv = emptyUnscopedTyparEnv

    try
      // Some preliminary checks 
      mutRecDefns |> MutRecShapes.iterTycons (fun tyconData ->
             let (MutRecDefnsPhase2DataForTycon(_, _, declKind, tcref, _, _, _, members, m, newslotsOK, _)) = tyconData
             let tcaug = tcref.TypeContents
             if tcaug.tcaug_closed && declKind <> ExtrinsicExtensionBinding then 
               error(InternalError("Intrinsic augmentations of types are only permitted in the same file as the definition of the type",m))
             members |> List.iter (fun mem ->
                    match mem with
                    | SynMemberDefn.Member _ -> ()
                    | SynMemberDefn.Interface _ -> () 
                    | SynMemberDefn.Open _ 
                    | SynMemberDefn.AutoProperty _
                    | SynMemberDefn.LetBindings _  // accept local definitions 
                    | SynMemberDefn.ImplicitCtor _ // accept implicit ctor pattern, should be first! 
                    | SynMemberDefn.ImplicitInherit _ when newslotsOK = NewSlotsOK -> () // accept implicit ctor pattern, should be first! 
                    // The rest should have been removed by splitting, they belong to "core" (they are "shape" of type, not implementation) 
                    | _ -> error(Error(FSComp.SR.tcDeclarationElementNotPermittedInAugmentation(),mem.Range))))


      let binds : MutRecDefnsPhase2Info = 
          (envMutRec, mutRecDefns) ||> MutRecShapes.mapTyconsWithEnv (fun envForDecls tyconData -> 
              let (MutRecDefnsPhase2DataForTycon(tyconOpt, _, declKind, tcref, _, _, declaredTyconTypars, _, _, _, fixupFinalAttrs)) = tyconData
              let obinds = tyconBindingsOfTypeDefn tyconData
              let ibinds  = 
                      let intfTypes = interfacesFromTypeDefn envForDecls tyconData
                      let slotImplSets = DispatchSlotChecking.GetSlotImplSets cenv.infoReader envForDecls.DisplayEnv false (List.map (fun (ity,_,m) -> (ity,m)) intfTypes)
                      (intfTypes, slotImplSets) ||> List.map2 (interfaceMembersFromTypeDefn tyconData) |> List.concat
              MutRecDefnsPhase2InfoForTycon(tyconOpt, tcref, declaredTyconTypars, declKind, obinds @ ibinds, fixupFinalAttrs))
      
      MutRecBindingChecking.TcMutRecDefns_Phase2_Bindings cenv envInitial tpenv bindsm scopem mutRecNSInfo envMutRec binds

    with e -> errorRecovery e scopem; [], envMutRec

//-------------------------------------------------------------------------
// Build augmentation declarations
//------------------------------------------------------------------------- 

module AddAugmentationDeclarations = 
    let tcaugHasNominalInterface g (tcaug: TyconAugmentation) tcref =
        tcaug.tcaug_interfaces |> List.exists (fun (x,_,_) -> 
            isAppTy g x && tyconRefEq g (tcrefOfAppTy g x) tcref)

        
    let AddGenericCompareDeclarations cenv (env: TcEnv) (scSet:Set<Stamp>) (tycon:Tycon) =
        if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && scSet.Contains tycon.Stamp then 
            let tcref = mkLocalTyconRef tycon
            let tcaug = tycon.TypeContents
            let _,typ = if tcref.Deref.IsExceptionDecl then [],cenv.g.exn_ty else generalizeTyconRef tcref
            let m = tycon.Range
            let genericIComparableTy = mkAppTy cenv.g.system_GenericIComparable_tcref [typ]


            let hasExplicitIComparable = tycon.HasInterface cenv.g cenv.g.mk_IComparable_ty 
            let hasExplicitGenericIComparable = tcaugHasNominalInterface cenv.g tcaug cenv.g.system_GenericIComparable_tcref    
            let hasExplicitIStructuralComparable = tycon.HasInterface cenv.g cenv.g.mk_IStructuralComparable_ty

            if hasExplicitIComparable then 
                errorR(Error(FSComp.SR.tcImplementsIComparableExplicitly(tycon.DisplayName),m)) 
      
            elif hasExplicitGenericIComparable then 
                errorR(Error(FSComp.SR.tcImplementsGenericIComparableExplicitly(tycon.DisplayName),m)) 
            elif hasExplicitIStructuralComparable then
                errorR(Error(FSComp.SR.tcImplementsIStructuralComparableExplicitly(tycon.DisplayName),m)) 
            else
                let hasExplicitGenericIComparable = tycon.HasInterface cenv.g genericIComparableTy
                let cvspec1,cvspec2 = AugmentWithHashCompare.MakeValsForCompareAugmentation cenv.g tcref
                let cvspec3 = AugmentWithHashCompare.MakeValsForCompareWithComparerAugmentation cenv.g tcref

                PublishInterface cenv env.DisplayEnv tcref m true cenv.g.mk_IStructuralComparable_ty
                PublishInterface cenv env.DisplayEnv tcref m true cenv.g.mk_IComparable_ty
                if not tycon.IsExceptionDecl && not hasExplicitGenericIComparable then 
                    PublishInterface cenv env.DisplayEnv tcref m true genericIComparableTy
                tcaug.SetCompare (mkLocalValRef cvspec1, mkLocalValRef cvspec2)
                tcaug.SetCompareWith (mkLocalValRef cvspec3)
                PublishValueDefn cenv env ModuleOrMemberBinding cvspec1
                PublishValueDefn cenv env ModuleOrMemberBinding cvspec2
                PublishValueDefn cenv env ModuleOrMemberBinding cvspec3

               

    let AddGenericEqualityWithComparerDeclarations cenv (env: TcEnv) (seSet:Set<Stamp>) (tycon:Tycon) =
        if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon && seSet.Contains tycon.Stamp then 
            let tcref = mkLocalTyconRef tycon
            let tcaug = tycon.TypeContents
            let m = tycon.Range

            let hasExplicitIStructuralEquatable = tycon.HasInterface cenv.g cenv.g.mk_IStructuralEquatable_ty

            if hasExplicitIStructuralEquatable then
                errorR(Error(FSComp.SR.tcImplementsIStructuralEquatableExplicitly(tycon.DisplayName),m)) 
            else
                let evspec1,evspec2,evspec3 = AugmentWithHashCompare.MakeValsForEqualityWithComparerAugmentation cenv.g tcref
                PublishInterface cenv env.DisplayEnv tcref m true cenv.g.mk_IStructuralEquatable_ty                
                tcaug.SetHashAndEqualsWith (mkLocalValRef evspec1, mkLocalValRef evspec2, mkLocalValRef evspec3)
                PublishValueDefn cenv env ModuleOrMemberBinding evspec1
                PublishValueDefn cenv env ModuleOrMemberBinding evspec2
                PublishValueDefn cenv env ModuleOrMemberBinding evspec3

                
    let AddGenericCompareBindings cenv (tycon:Tycon) =
        if (* AugmentWithHashCompare.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && *) Option.isSome tycon.GeneratedCompareToValues then 
            AugmentWithHashCompare.MakeBindingsForCompareAugmentation cenv.g tycon
        else
            []
            
    let AddGenericCompareWithComparerBindings cenv (tycon:Tycon) =
        if (* AugmentWithHashCompare.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && *) Option.isSome tycon.GeneratedCompareToWithComparerValues then
             (AugmentWithHashCompare.MakeBindingsForCompareWithComparerAugmentation cenv.g tycon)
         else
            []
             
    let AddGenericEqualityWithComparerBindings cenv (tycon:Tycon) =
        if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon && Option.isSome tycon.GeneratedHashAndEqualsWithComparerValues then
            (AugmentWithHashCompare.MakeBindingsForEqualityWithComparerAugmentation cenv.g tycon)
        else
            []

    let AddGenericHashAndComparisonDeclarations cenv env scSet seSet tycon =
        AddGenericCompareDeclarations cenv env scSet tycon
        AddGenericEqualityWithComparerDeclarations cenv env seSet tycon


    let AddGenericHashAndComparisonBindings cenv tycon =
        AddGenericCompareBindings cenv tycon @ AddGenericCompareWithComparerBindings cenv tycon @ AddGenericEqualityWithComparerBindings cenv tycon


    // We can only add the Equals override after we've done the augmentation becuase we have to wait until 
    // tycon.HasOverride can give correct results 
    let AddGenericEqualityBindings cenv (env: TcEnv) tycon =
        if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
            let tcref = mkLocalTyconRef tycon
            let tcaug = tycon.TypeContents
            let _,typ = if tcref.Deref.IsExceptionDecl then [],cenv.g.exn_ty else generalizeTyconRef tcref
            let m = tycon.Range
            
            // Note: tycon.HasOverride only gives correct results after we've done the type augmentation 
            let hasExplicitObjectEqualsOverride = tycon.HasOverride cenv.g "Equals" [cenv.g.obj_ty]
            let hasExplicitGenericIEquatable = tcaugHasNominalInterface cenv.g tcaug cenv.g.system_GenericIEquatable_tcref
            
            if hasExplicitGenericIEquatable then 
                errorR(Error(FSComp.SR.tcImplementsIEquatableExplicitly(tycon.DisplayName),m)) 

            // Note: only provide the equals method if Equals is not implemented explicitly, and
            // we're actually generating Hash/Equals for this type
            if not hasExplicitObjectEqualsOverride &&
                Option.isSome tycon.GeneratedHashAndEqualsWithComparerValues then

                 let vspec1,vspec2 = AugmentWithHashCompare.MakeValsForEqualsAugmentation cenv.g tcref
                 tcaug.SetEquals (mkLocalValRef vspec1, mkLocalValRef vspec2)
                 if not tycon.IsExceptionDecl then 
                    PublishInterface cenv env.DisplayEnv tcref m true  (mkAppTy cenv.g.system_GenericIEquatable_tcref [typ])
                 PublishValueDefn cenv env ModuleOrMemberBinding vspec1
                 PublishValueDefn cenv env ModuleOrMemberBinding vspec2
                 AugmentWithHashCompare.MakeBindingsForEqualsAugmentation cenv.g tycon
            else []
        else []



/// Infer 'comparison' and 'equality' constraints from type definitions
module TyconConstraintInference = 

    /// Infer 'comparison' constraints from type definitions
    let InferSetOfTyconsSupportingComparable cenv (denv: DisplayEnv) tyconsWithStructuralTypes =

        let g = cenv.g 
        let tab = tyconsWithStructuralTypes |> List.map (fun (tycon:Tycon, structuralTypes) -> tycon.Stamp, (tycon,structuralTypes)) |> Map.ofList 

        // Initially, assume the equality relation is available for all structural type definitions 
        let initialAssumedTycons = 
            set [ for (tycon,_) in tyconsWithStructuralTypes do 
                       if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithCompare cenv.g tycon then 
                           yield tycon.Stamp ]

        // Initially, don't assume that the equality relation is dependent on any type varaibles
        let initialAsssumedTypars = Set.empty

        // Repeatedly eliminate structural type definitions whose structural component types no longer support 
        // comparison. On the way record type variables which are support the comparison relation.
        let rec loop (assumedTycons : Set<Stamp>) (assumedTypars: Set<Stamp>) =
            let assumedTyparsAcc = ref assumedTypars

            // Checks if a field type supports the 'comparison' constraint based on the assumptions about the type constructors
            // and type parameters.
            let rec checkIfFieldTypeSupportsComparison (tycon: Tycon) (ty: TType) =
                
                // Is the field type a type parameter?
                match tryDestTyparTy cenv.g ty with
                | Some tp ->
                    // Look for an explicit 'comparison' constraint
                    if tp.Constraints |> List.exists (function TyparConstraint.SupportsComparison _ -> true | _ -> false) then 
                        true
                    
                    // Within structural types, type parameters can be optimistically assumed to have comparison
                    // We record the ones for which we have made this assumption.
                    elif tycon.TyparsNoRange |> List.exists (fun tp2 -> typarRefEq tp tp2) then 
                        assumedTyparsAcc := (!assumedTyparsAcc).Add(tp.Stamp)
                        true
                    
                    else
                        false
                | None ->
                    match ty with 
                    // Look for array, UIntPtr and IntPtr types
                    | SpecialComparableHeadType g tinst -> 
                        tinst |> List.forall (checkIfFieldTypeSupportsComparison  tycon)

                    // Otherwise it's a nominal type
                    | _ -> 

                        if isAppTy g ty then 
                            let tcref,tinst = destAppTy g ty 
                            // Check the basic requirement - IComparable/IStructuralComparable or assumed-comparable
                            (if initialAssumedTycons.Contains tcref.Stamp then 
                                assumedTycons.Contains tcref.Stamp
                             else
                                ExistsSameHeadTypeInHierarchy g cenv.amap range0 ty g.mk_IComparable_ty   || 
                                ExistsSameHeadTypeInHierarchy g cenv.amap range0 ty g.mk_IStructuralComparable_ty)
                            &&
                            // Check it isn't ruled out by the user
                            not (HasFSharpAttribute g g.attrib_NoComparisonAttribute tcref.Attribs)
                            &&
                            // Check the structural dependencies
                            (tinst, tcref.TyparsNoRange) ||> List.lengthsEqAndForall2 (fun ty tp -> 
                                if tp.ComparisonConditionalOn || assumedTypars.Contains tp.Stamp then 
                                    checkIfFieldTypeSupportsComparison  tycon ty 
                                else 
                                    true) 
                        else
                            false

            let newSet = 
                assumedTycons |> Set.filter (fun tyconStamp -> 
                   let (tycon,structuralTypes) = tab.[tyconStamp] 
                   if cenv.g.compilingFslib && AugmentWithHashCompare.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && not (HasFSharpAttribute g g.attrib_StructuralComparisonAttribute tycon.Attribs) && not (HasFSharpAttribute g g.attrib_NoComparisonAttribute tycon.Attribs) then 
                       errorR(Error(FSComp.SR.tcFSharpCoreRequiresExplicit(),tycon.Range)) 

                   let res = (structuralTypes |> List.forall (fst >> checkIfFieldTypeSupportsComparison tycon))

                   // If the type was excluded, say why
                   if not res then 
                       match TryFindFSharpBoolAttribute g g.attrib_StructuralComparisonAttribute tycon.Attribs with
                       | Some(true) -> 
                           match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsComparison tycon >> not) with
                           | None -> 
                               assert false
                               failwith "unreachable"
                           | Some (ty,_) -> 
                               if isTyparTy g ty then 
                                   errorR(Error(FSComp.SR.tcStructuralComparisonNotSatisfied1(tycon.DisplayName,NicePrint.prettyStringOfTy denv ty),tycon.Range)) 
                               else 
                                   errorR(Error(FSComp.SR.tcStructuralComparisonNotSatisfied2(tycon.DisplayName,NicePrint.prettyStringOfTy denv ty),tycon.Range)) 
                       | Some(false) -> 
                           ()
                       
                       | None -> 
                           match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsComparison tycon >> not) with
                           | None -> 
                               assert false
                               failwith "unreachable"
                           | Some (ty,_) -> 
                               // NOTE: these warnings are off by default - they are level 4 informational warnings
                               // PERF: this call to prettyStringOfTy is always being executed, even when the warning
                               // is not being reported (the normal case).
                               if isTyparTy g ty then 
                                   warning(Error(FSComp.SR.tcNoComparisonNeeded1(tycon.DisplayName, NicePrint.prettyStringOfTy denv ty, tycon.DisplayName),tycon.Range)) 
                               else 
                                   warning(Error(FSComp.SR.tcNoComparisonNeeded2(tycon.DisplayName, NicePrint.prettyStringOfTy denv ty, tycon.DisplayName),tycon.Range)) 

                                                      
                   res)

            if newSet = assumedTycons && assumedTypars = !assumedTyparsAcc then 
                newSet, !assumedTyparsAcc
            else 
                loop newSet !assumedTyparsAcc

        let uneliminatedTycons, assumedTyparsActual = loop initialAssumedTycons initialAsssumedTypars

        // OK, we're done, Record the results for the type variable which provide the support
        for tyconStamp in uneliminatedTycons do
            let (tycon,_) = tab.[tyconStamp] 
            for tp in tycon.Typars(tycon.Range) do
                if assumedTyparsActual.Contains(tp.Stamp) then 
                    tp.SetComparisonDependsOn true

        // Return the set of structural type definitions which support the relation
        uneliminatedTycons

    /// Infer 'equality' constraints from type definitions
    let InferSetOfTyconsSupportingEquatable cenv (denv: DisplayEnv)  (tyconsWithStructuralTypes:(Tycon * _) list) =

        let g = cenv.g 
        let tab = tyconsWithStructuralTypes |> List.map (fun (tycon,c) -> tycon.Stamp, (tycon,c)) |> Map.ofList 

        // Initially, assume the equality relation is available for all structural type definitions 
        let initialAssumedTycons = 
            set [ for (tycon,_) in tyconsWithStructuralTypes do 
                       if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
                           yield tycon.Stamp ]
                           
        // Initially, don't assume that the equality relation is dependent on any type varaibles
        let initialAssumedTypars = Set.empty

        // Repeatedly eliminate structural type definitions whose structural component types no longer support 
        // equality. On the way add type variables which are support the equality relation
        let rec loop (assumedTycons : Set<Stamp>) (assumedTypars: Set<Stamp>) =
            let assumedTyparsAcc = ref assumedTypars
            
            // Checks if a field type supports the 'equality' constraint based on the assumptions about the type constructors
            // and type parameters.
            let rec checkIfFieldTypeSupportsEquality (tycon:Tycon) (ty: TType) =
                match tryDestTyparTy cenv.g ty with
                | Some tp ->
                    // Look for an explicit 'equality' constraint
                    if tp.Constraints |> List.exists (function TyparConstraint.SupportsEquality _ -> true | _ -> false) then 
                        true

                    // Within structural types, type parameters can be optimistically assumed to have ewquality
                    // We record the ones for which we have made this assumption.
                    elif tycon.Typars(tycon.Range) |> List.exists (fun tp2 -> typarRefEq tp tp2) then                     
                        assumedTyparsAcc := (!assumedTyparsAcc).Add(tp.Stamp)
                        true
                    else
                        false
                | None ->
                    match ty with 
                    | SpecialEquatableHeadType g tinst -> 
                        tinst |> List.forall (checkIfFieldTypeSupportsEquality tycon)
                    | SpecialNotEquatableHeadType g -> 
                        false
                    | _ -> 
                        // Check the basic requirement - any types except those eliminated
                        if isAppTy g ty then
                            let tcref,tinst = destAppTy g ty
                            (if initialAssumedTycons.Contains tcref.Stamp then 
                                assumedTycons.Contains tcref.Stamp
                             elif AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals g tcref.Deref then
                                Option.isSome tcref.GeneratedHashAndEqualsWithComparerValues
                             else
                                true) 
                             &&
                             // Check it isn't ruled out by the user
                             not (HasFSharpAttribute g g.attrib_NoEqualityAttribute tcref.Attribs)
                             &&
                             // Check the structural dependencies
                             (tinst, tcref.TyparsNoRange) ||> List.lengthsEqAndForall2 (fun ty tp -> 
                                 if tp.EqualityConditionalOn || assumedTypars.Contains tp.Stamp then 
                                     checkIfFieldTypeSupportsEquality  tycon ty 
                                 else 
                                     true) 
                        else
                            false

            let newSet = 
                assumedTycons |> Set.filter (fun tyconStamp -> 
                   let (tycon,structuralTypes) = tab.[tyconStamp] 
                   if cenv.g.compilingFslib && AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon && not (HasFSharpAttribute g g.attrib_StructuralEqualityAttribute tycon.Attribs) && not (HasFSharpAttribute g g.attrib_NoEqualityAttribute tycon.Attribs) then 
                       errorR(Error(FSComp.SR.tcFSharpCoreRequiresExplicit(),tycon.Range)) 

                   // Remove structural types with incomparable elements from the assumedTycons
                   let res = (structuralTypes |> List.forall (fst >> checkIfFieldTypeSupportsEquality tycon))

                   // If the type was excluded, say why
                   if not res then 
                       match TryFindFSharpBoolAttribute g g.attrib_StructuralEqualityAttribute tycon.Attribs with
                       | Some(true) -> 
                           if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
                               match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsEquality tycon >> not) with
                               | None -> 
                                   assert false
                                   failwith "unreachable"
                               | Some (ty,_) -> 
                                   if isTyparTy g ty then 
                                       errorR(Error(FSComp.SR.tcStructuralEqualityNotSatisfied1(tycon.DisplayName,NicePrint.prettyStringOfTy denv ty),tycon.Range)) 
                                   else 
                                       errorR(Error(FSComp.SR.tcStructuralEqualityNotSatisfied2(tycon.DisplayName,NicePrint.prettyStringOfTy denv ty),tycon.Range)) 
                           else
                               ()
                       | Some(false) -> 
                           ()
                       | None -> 
                           if AugmentWithHashCompare.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
                               match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsEquality tycon >> not) with
                               | None -> 
                                   assert false
                                   failwith "unreachable"
                               | Some (ty,_) -> 
                                   if isTyparTy g ty then 
                                       warning(Error(FSComp.SR.tcNoEqualityNeeded1(tycon.DisplayName, NicePrint.prettyStringOfTy denv ty, tycon.DisplayName),tycon.Range)) 
                                   else 
                                       warning(Error(FSComp.SR.tcNoEqualityNeeded2(tycon.DisplayName, NicePrint.prettyStringOfTy denv ty, tycon.DisplayName),tycon.Range)) 

                                                      
                   res)

            if newSet = assumedTycons && assumedTypars = !assumedTyparsAcc then 
                newSet, !assumedTyparsAcc
            else 
                loop newSet !assumedTyparsAcc

        let uneliminatedTycons, assumedTyparsActual = loop initialAssumedTycons initialAssumedTypars

        // OK, we're done, Record the results for the type variable which provide the support
        for tyconStamp in uneliminatedTycons do
            let (tycon,_) = tab.[tyconStamp] 
            for tp in tycon.Typars(tycon.Range) do
                if assumedTyparsActual.Contains(tp.Stamp) then 
                    tp.SetEqualityDependsOn true

        // Return the set of structural type definitions which support the relation
        uneliminatedTycons


//-------------------------------------------------------------------------
// Helpers for modules, types and exception declarations
//------------------------------------------------------------------------- 

let ComputeModuleName (longPath: Ident list) = 
    if longPath.Length <> 1 then error(Error(FSComp.SR.tcInvalidModuleName(),(List.head longPath).idRange))
    longPath.Head 

let CheckForDuplicateConcreteType env nm m  = 
    let curr = GetCurrAccumulatedModuleOrNamespaceType env
    if Map.containsKey nm curr.AllEntitiesByCompiledAndLogicalMangledNames then 
        // Use 'error' instead of 'errorR' here to avoid cascading errors - see bug 1177 in FSharp 1.0 
        error (Duplicate(FSComp.SR.tcTypeExceptionOrModule(),nm,m))

let CheckForDuplicateModule env nm m  = 
    let curr = GetCurrAccumulatedModuleOrNamespaceType env
    if curr.ModulesAndNamespacesByDemangledName.ContainsKey(nm) then 
        errorR (Duplicate(FSComp.SR.tcTypeOrModule(),nm,m))


//-------------------------------------------------------------------------
// Bind exception definitions
//------------------------------------------------------------------------- 

/// Check 'exception' declarations in implementations and signatures
module TcExceptionDeclarations = 

    let TcExnDefnCore_Phase1A cenv env parent (SynExceptionDefnRepr(synAttrs,UnionCase(_,id,_,_,_,_),_,doc,vis,m)) =
        let attrs = TcAttributes cenv env AttributeTargets.ExnDecl synAttrs
        if not (String.isUpper id.idText) then errorR(NotUpperCaseConstructor(m))
        let vis,cpath = ComputeAccessAndCompPath env None m vis None parent
        let vis = TcRecdUnionAndEnumDeclarations.CombineReprAccess parent vis
        CheckForDuplicateConcreteType env (id.idText + "Exception") id.idRange
        CheckForDuplicateConcreteType env id.idText id.idRange
        NewExn cpath id vis (TExnFresh (MakeRecdFieldsTable [])) attrs (doc.ToXmlDoc())

    let TcExnDefnCore_Phase1G_EstablishRepresentation cenv env parent (exnc: Entity) (SynExceptionDefnRepr(_,UnionCase(_,_,args,_,_,_),reprIdOpt,_,_,m)) =
        let args = match args with (UnionCaseFields args) -> args | _ -> error(Error(FSComp.SR.tcExplicitTypeSpecificationCannotBeUsedForExceptionConstructors(),m))
        let ad = env.eAccessRights
        let id = exnc.Id
        
        let args' = List.mapi (fun i fdef -> TcRecdUnionAndEnumDeclarations.TcAnonFieldDecl cenv env parent emptyUnscopedTyparEnv ("Data" + string i) fdef) args
        TcRecdUnionAndEnumDeclarations.ValidateFieldNames(args, args')
        let repr = 
          match reprIdOpt with 
          | Some longId ->
              match ResolveExprLongIdent cenv.tcSink cenv.nameResolver m ad env.eNameResEnv TypeNameResolutionInfo.Default longId with
              | Item.ExnCase exnc, [] -> 
                  CheckTyconAccessible cenv.amap m env.eAccessRights exnc |> ignore
                  if List.length args' <> 0 then 
                    errorR (Error(FSComp.SR.tcExceptionAbbreviationsShouldNotHaveArgumentList(),m))
                  TExnAbbrevRepr exnc
              | Item.CtorGroup(_,meths) , [] -> 
                  // REVIEW: check this really is an exception type 
                  match args' with 
                  | [] -> ()
                  | _ -> error (Error(FSComp.SR.tcAbbreviationsFordotNetExceptionsCannotTakeArguments(),m))
                  let candidates = 
                      meths |> List.filter (fun minfo -> 
                          minfo.NumArgs = [args'.Length] &&
                          minfo.GenericArity = 0) 
                  match candidates with 
                  | [minfo] -> 
                      match minfo.EnclosingType with 
                      | AppTy cenv.g (tcref,_) as ety when (TypeDefinitelySubsumesTypeNoCoercion 0 cenv.g cenv.amap m cenv.g.exn_ty ety) ->
                          let tref = tcref.CompiledRepresentationForNamedType
                          TExnAsmRepr tref
                      | _ -> 
                          error(Error(FSComp.SR.tcExceptionAbbreviationsMustReferToValidExceptions(),m))
                  | _ -> 
                      error (Error(FSComp.SR.tcAbbreviationsFordotNetExceptionsMustHaveMatchingObjectConstructor(),m))
              | _ ->
                  error (Error(FSComp.SR.tcNotAnException(),m))
          | None -> 
             TExnFresh (MakeRecdFieldsTable args')
        
        exnc.entity_exn_info <- repr 

        let item = Item.ExnCase(mkLocalTyconRef exnc)
        CallNameResolutionSink cenv.tcSink (id.idRange,env.NameEnv,item,item,ItemOccurence.Binding,env.DisplayEnv,env.eAccessRights)
        args'

    let private TcExnDefnCore cenv env parent synExnDefnRepr =
        let exnc = TcExnDefnCore_Phase1A cenv env parent synExnDefnRepr
        let args' = TcExnDefnCore_Phase1G_EstablishRepresentation cenv env parent exnc synExnDefnRepr
        exnc.TypeContents.tcaug_super <- Some cenv.g.exn_ty

        PublishTypeDefn cenv env exnc

        let structuralTypes = args' |> List.map (fun rf -> (rf.FormalType, rf.Range))
        let scSet = TyconConstraintInference.InferSetOfTyconsSupportingComparable cenv env.DisplayEnv [(exnc,structuralTypes)]
        let seSet = TyconConstraintInference.InferSetOfTyconsSupportingEquatable cenv env.DisplayEnv [(exnc,structuralTypes)] 

        // Augment the exception constructor with comparison and hash methods if needed 
        let binds = 
          match exnc.ExceptionInfo with 
          | TExnAbbrevRepr _ | TExnNone | TExnAsmRepr _ -> []
          | TExnFresh _ -> 
              AddAugmentationDeclarations.AddGenericHashAndComparisonDeclarations cenv env scSet seSet exnc
              AddAugmentationDeclarations.AddGenericHashAndComparisonBindings cenv exnc

        binds,exnc


    let TcExnDefn cenv envInitial parent (SynExceptionDefn(core,aug,m),scopem) = 
        let binds1,exnc = TcExnDefnCore cenv envInitial parent core
        let envMutRec =  AddLocalExnDefnAndReport cenv.tcSink scopem (AddLocalTycons cenv.g cenv.amap scopem [exnc] envInitial) exnc 

        let defns = [MutRecShape.Tycon(MutRecDefnsPhase2DataForTycon(Some exnc, parent, ModuleOrMemberBinding, mkLocalEntityRef exnc, None, NoSafeInitInfo, [], aug, m, NoNewSlots, (fun () -> ())))]
        let binds2,envFinal = TcMutRecDefns_Phase2 cenv envInitial m scopem None envMutRec defns
        let binds2flat = binds2 |> MutRecShapes.collectTycons |> List.collect snd
        // Augment types with references to values that implement the pre-baked semantics of the type
        let binds3 = AddAugmentationDeclarations.AddGenericEqualityBindings cenv envFinal exnc
        binds1 @ binds2flat @ binds3,exnc,envFinal

    let TcExnSignature cenv envInitial parent tpenv (SynExceptionSig(core,aug,_),scopem) = 
        let binds,exnc = TcExnDefnCore cenv envInitial parent core
        let envMutRec =  AddLocalExnDefnAndReport cenv.tcSink scopem (AddLocalTycons cenv.g cenv.amap scopem [exnc] envInitial) exnc 
        let ecref = mkLocalEntityRef exnc
        let vals,_ = TcTyconMemberSpecs cenv envMutRec (ContainerInfo(parent,Some(MemberOrValContainerInfo(ecref,None,None,NoSafeInitInfo,[])))) ModuleOrMemberBinding tpenv aug
        binds,vals,ecref,envMutRec



/// Bind type definitions
///
/// We first establish the cores of a set of type definitions (i.e. everything
/// about the type definitions that doesn't involve values or expressions)
///
/// This is a non-trivial multi-phase algorithm. The technique used
/// is to gradually "fill in" the fields of the type constructors. 
///
/// This use of mutation is very problematic. This has many dangers, 
/// since the process of filling in the fields
/// involves creating, traversing and analyzing types that may recursively
/// refer to the types being defined. However a functional version of this
/// would need to re-implement certain type relations to work over a 
/// partial representation of types.
module EstablishTypeDefinitionCores = 
 
    /// Compute the mangled name of a type definition. 'doErase' is true for all type definitions except type abbreviations.
    let private ComputeTyconName (longPath: Ident list, doErase:bool, typars: Typars) = 
        if longPath.Length <> 1 then error(Error(FSComp.SR.tcInvalidTypeExtension(),longPath.Head.idRange))
        let id = longPath.Head
        let erasedArity = 
            if doErase then typars |> Seq.sumBy (fun tp -> if tp.IsErased then 0 else 1) 
            else typars.Length
        mkSynId id.idRange (if erasedArity = 0 then id.idText else id.idText + "`" + string erasedArity)
 
    let private GetTyconAttribs g attrs = 
        let hasClassAttr         = HasFSharpAttribute g g.attrib_ClassAttribute attrs
        let hasAbstractClassAttr = HasFSharpAttribute g g.attrib_AbstractClassAttribute attrs
        let hasInterfaceAttr     = HasFSharpAttribute g g.attrib_InterfaceAttribute attrs
        let hasStructAttr        = HasFSharpAttribute g g.attrib_StructAttribute attrs
        let hasMeasureAttr       = HasFSharpAttribute g g.attrib_MeasureAttribute attrs
        (hasClassAttr,hasAbstractClassAttr,hasInterfaceAttr,hasStructAttr,hasMeasureAttr)

    //-------------------------------------------------------------------------
    // Type kind inference 
    //------------------------------------------------------------------------- 
       
    let private InferTyconKind g (kind,attrs,slotsigs,fields,inSig,isConcrete,m) =
        let (hasClassAttr,hasAbstractClassAttr,hasInterfaceAttr,hasStructAttr,hasMeasureAttr) = GetTyconAttribs g attrs
        let bi b = (if b then 1 else 0)
        if (bi hasClassAttr + bi hasInterfaceAttr + bi hasStructAttr + bi hasMeasureAttr) > 1 ||
           (bi hasAbstractClassAttr + bi hasInterfaceAttr + bi hasStructAttr + bi hasMeasureAttr) > 1 then
           error(Error(FSComp.SR.tcAttributesOfTypeSpecifyMultipleKindsForType(),m))
        
        match kind with 
        | TyconUnspecified ->
            if hasClassAttr || hasAbstractClassAttr || hasMeasureAttr then TyconClass        
            elif hasInterfaceAttr then TyconInterface
            elif hasStructAttr then TyconStruct
            elif isConcrete || not (isNil fields) then TyconClass
            elif isNil slotsigs && inSig then TyconHiddenRepr
            else TyconInterface
        | k -> 
            if hasClassAttr && not (match k with TyconClass -> true | _ -> false) || 
               hasMeasureAttr && not (match k with TyconClass | TyconAbbrev | TyconHiddenRepr -> true | _ -> false)  || 
               hasInterfaceAttr && not (match k with TyconInterface -> true | _ -> false) || 
               hasStructAttr && not (match k with TyconStruct | TyconRecord | TyconUnion -> true | _ -> false) then 
                error(Error(FSComp.SR.tcKindOfTypeSpecifiedDoesNotMatchDefinition(),m))
            k


    let private (|TyconCoreAbbrevThatIsReallyAUnion|_|) (hasMeasureAttr,envinner,id:Ident) synTyconRepr =
        match synTyconRepr with 
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, SynType.LongIdent(LongIdentWithDots([unionCaseName],_)),m) 
                              when 
                                (not hasMeasureAttr && 
                                 (isNil (LookupTypeNameInEnvNoArity OpenQualified unionCaseName.idText envinner.eNameResEnv) || 
                                  id.idText = unionCaseName.idText)) -> 
            Some(unionCaseName,m)
        | _ -> 
            None

    /// Get the component types that make a record, union or struct type.
    ///
    /// Used when determining if a structural type supports structural comparison.
    let private GetStructuralElementsOfTyconDefn cenv env tpenv (MutRecDefnsPhase1DataForTycon(_,synTyconRepr,_,_,_,_)) tycon = 
        let thisTyconRef = mkLocalTyconRef tycon
        let m = tycon.Range
        let env = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) env
        let env = MakeInnerEnvForTyconRef cenv env thisTyconRef false 
        [ match synTyconRepr with 
          | SynTypeDefnSimpleRepr.None _ -> ()
          | SynTypeDefnSimpleRepr.Union (_,unionCases,_) -> 

              for (UnionCase (_,_,args,_,_,m)) in unionCases do 
                match args with
                | UnionCaseFields flds -> 
                  for (Field(_,_,_,ty,_,_,_,m)) in flds do 
                      let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs ItemOccurence.UseInType env tpenv ty
                      yield (ty',m)
                | UnionCaseFullType (ty,arity) -> 
                  let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs ItemOccurence.UseInType env tpenv ty
                  let argtysl,_ = GetTopTauTypeInFSharpForm cenv.g (arity |> TranslateTopValSynInfo m (TcAttributes cenv env) |> TranslatePartialArity []).ArgInfos ty' m
                  if argtysl.Length > 1 then 
                      errorR(Error(FSComp.SR.tcIllegalFormForExplicitTypeDeclaration(),m))   
                  for argtys in argtysl do
                    for (argty,_) in argtys do
                      yield (argty ,m)

          | SynTypeDefnSimpleRepr.General (_,_,_,fields,_,_,implicitCtorSynPats,_) when tycon.IsFSharpStructOrEnumTycon -> // for structs
              for (Field(_,isStatic,_,ty,_,_,_,m)) in fields do 
                  if not isStatic  then 
                      let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs ItemOccurence.UseInType env tpenv ty
                      yield (ty',m)

              match implicitCtorSynPats with
              | None -> ()
              | Some spats -> 
                  let ctorArgNames,(_,names,_) = TcSimplePatsOfUnknownType cenv true NoCheckCxs env tpenv (SynSimplePats.SimplePats (spats,m))
                  for arg in ctorArgNames do
                      let ty = names.[arg].Type
                      let m = names.[arg].Ident.idRange
                      if not (isNil (ListSet.subtract typarEq (freeInTypeLeftToRight cenv.g false ty) tycon.TyparsNoRange)) then
                          errorR(Error(FSComp.SR.tcStructsMustDeclareTypesOfImplicitCtorArgsExplicitly(),m))   
                      yield (ty, m)

          | SynTypeDefnSimpleRepr.Record (_,fields,_) -> 
              for (Field(_,_,_,ty,_,_,_,m)) in fields do 
                  let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs ItemOccurence.UseInType env tpenv ty
                  yield (ty',m)

          | _ ->
              () ]

    let ComputeModuleOrNamespaceKind g isModule typeNames attribs nm = 
        if not isModule then Namespace 
        elif ModuleNameIsMangled g attribs || Set.contains nm typeNames then FSharpModuleWithSuffix 
        else ModuleOrType

    let AdjustModuleName modKind nm = (match modKind with FSharpModuleWithSuffix -> nm+FSharpModuleSuffix | _ -> nm)


    let TypeNamesInMutRecDecls (compDecls: MutRecShapes<MutRecDefnsPhase1DataForTycon * 'MemberInfo, 'LetInfo, SynComponentInfo, _, _>) =
        [ for d in compDecls do 
                match d with 
                | MutRecShape.Tycon (MutRecDefnsPhase1DataForTycon(ComponentInfo(_,_,_,ids,_,_,_,_),_,_,_,_,isAtOriginalTyconDefn),_) -> 
                    if isAtOriginalTyconDefn then 
                        yield (List.last ids).idText
                | _ -> () ]
         |> set

    let TcTyconDefnCore_Phase1A_BuildInitialModule cenv envInitial parent typeNames compInfo compDecls =
        let (ComponentInfo(attribs,_parms, _constraints,longPath,xml,_,vis,im)) = compInfo 
        let id = ComputeModuleName longPath
        let modAttrs = TcAttributes cenv envInitial AttributeTargets.ModuleDecl attribs 
        let modKind = ComputeModuleOrNamespaceKind cenv.g true typeNames modAttrs id.idText
        let modName = AdjustModuleName modKind id.idText

        let vis,_ = ComputeAccessAndCompPath envInitial None id.idRange vis None parent
             
        CheckForDuplicateModule envInitial id.idText id.idRange
        let id = ident (modName, id.idRange)
        CheckForDuplicateConcreteType envInitial id.idText im
        CheckNamespaceModuleOrTypeName cenv.g id

        let envForDecls, mtypeAcc = MakeInnerEnv envInitial id modKind    
        let mspec = NewModuleOrNamespace (Some envInitial.eCompPath) vis id (xml.ToXmlDoc()) modAttrs (MaybeLazy.Strict (NewEmptyModuleOrNamespaceType modKind))
        let innerParent = Parent (mkLocalModRef mspec)
        let typeNames = TypeNamesInMutRecDecls compDecls
        MutRecDefnsPhase2DataForModule (mtypeAcc, mspec), (innerParent, typeNames, envForDecls)

    /// Establish 'type <vis1> C < T1... TN >  = <vis2> ...' including 
    ///    - computing the mangled name for C
    /// but 
    ///    - we don't yet 'properly' establish constraints on type parameters
    let private TcTyconDefnCore_Phase1A_BuildInitialTycon cenv env parent (MutRecDefnsPhase1DataForTycon(synTyconInfo,synTyconRepr,_,preEstablishedHasDefaultCtor,hasSelfReferentialCtor, _)) = 
        let (ComponentInfo (_, synTypars, _,id, doc, preferPostfix, synVis,_)) = synTyconInfo
        let checkedTypars = TcTyparDecls cenv env synTypars
        id |> List.iter (CheckNamespaceModuleOrTypeName cenv.g)
        match synTyconRepr with 
        | SynTypeDefnSimpleRepr.Exception synExnDefnRepr -> 
          TcExceptionDeclarations.TcExnDefnCore_Phase1A cenv env parent synExnDefnRepr
        | _ ->
        let id = ComputeTyconName (id, (match synTyconRepr with SynTypeDefnSimpleRepr.TypeAbbrev _ -> false | _ -> true), checkedTypars)

        // Augmentations of type definitions are allowed within the same file as long as no new type representation or abbreviation is given 
        CheckForDuplicateConcreteType env id.idText id.idRange
        let vis,cpath = ComputeAccessAndCompPath env None id.idRange synVis None parent

        // Establish the visibility of the representation, e.g.
        //   type R = 
        //      private { f:int }
        //      member x.P = x.f + x.f
        let synVisOfRepr = 
            match synTyconRepr with 
            | SynTypeDefnSimpleRepr.None _ -> None
            | SynTypeDefnSimpleRepr.TypeAbbrev _ -> None
            | SynTypeDefnSimpleRepr.Union (vis,_,_) -> vis
            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> None
            | SynTypeDefnSimpleRepr.Record (vis,_,_) -> vis
            | SynTypeDefnSimpleRepr.General _ -> None
            | SynTypeDefnSimpleRepr.Enum _ -> None
            | SynTypeDefnSimpleRepr.Exception _ -> None
         
        let visOfRepr,_ = ComputeAccessAndCompPath env None id.idRange synVisOfRepr None parent
        let visOfRepr = combineAccess vis visOfRepr 
        // If we supported nested types and modules then additions would be needed here
        let lmtyp = MaybeLazy.Strict (NewEmptyModuleOrNamespaceType ModuleOrType)

        NewTycon(cpath, id.idText, id.idRange, vis, visOfRepr, TyparKind.Type, LazyWithContext.NotLazy checkedTypars, doc.ToXmlDoc(), preferPostfix, preEstablishedHasDefaultCtor, hasSelfReferentialCtor, lmtyp)

    //-------------------------------------------------------------------------
    /// Establishing type definitions: early phase: work out the basic kind of the type definition
    ///
    ///    On entry: the Tycon for the type definition has been created but many of its fields are not
    ///              yet filled in.
    ///    On exit: the entity_tycon_repr field of the tycon has been filled in with a dummy value that
    ///             indicates the kind of the type constructor
    /// Also, some adhoc checks are made.
    ///
    ///  synTyconInfo: Syntactic AST for the name, attributes etc. of the type constructor
    ///  synTyconRepr: Syntactic AST for the RHS of the type definition
    let private TcTyconDefnCore_Phase1B_EstablishBasicKind cenv inSig envinner (MutRecDefnsPhase1DataForTycon(synTyconInfo,synTyconRepr,_,_,_,_)) (tycon:Tycon) = 
        let (ComponentInfo(synAttrs,typars, _,_, _, _,_,_)) = synTyconInfo
        let m = tycon.Range
        let id = tycon.Id

        // 'Check' the attributes. We return the results to avoid having to re-check them in all other phases. 
        // Allow failure of constructor resolution because Vals for members in the same recursive group are not yet available
        let attrs, getFinalAttrs = TcAttributesCanFail cenv envinner AttributeTargets.TyconDecl synAttrs
        let hasMeasureAttr = HasFSharpAttribute cenv.g cenv.g.attrib_MeasureAttribute attrs

        let isStructRecordOrUnionType = 
            match synTyconRepr with
            | SynTypeDefnSimpleRepr.Record _ 
            | SynTypeDefnSimpleRepr.Union _ -> 
                HasFSharpAttribute cenv.g cenv.g.attrib_StructAttribute attrs
            | _ -> 
                false

        tycon.SetIsStructRecordOrUnion isStructRecordOrUnionType

        // Set the compiled name, if any
        tycon.entity_compiled_name <- TryFindFSharpStringAttribute cenv.g cenv.g.attrib_CompiledNameAttribute attrs 

        if hasMeasureAttr then 
            tycon.entity_kind <- TyparKind.Measure
            if not (isNil typars) then error(Error(FSComp.SR.tcMeasureDefinitionsCannotHaveTypeParameters(),m))

        let repr = 
            match synTyconRepr with 
            | SynTypeDefnSimpleRepr.Exception _ ->  TNoRepr
            | SynTypeDefnSimpleRepr.None m -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconHiddenRepr,attrs,[],[],inSig,true,m)  |> ignore
                if not inSig && not hasMeasureAttr then 
                    errorR(Error(FSComp.SR.tcTypeRequiresDefinition(),m))
                if hasMeasureAttr then 
                    TFSharpObjectRepr { fsobjmodel_kind=TTyconClass 
                                        fsobjmodel_vslots=[]
                                        fsobjmodel_rfields=MakeRecdFieldsTable [] }
                else 
                    TNoRepr

            | TyconCoreAbbrevThatIsReallyAUnion (hasMeasureAttr,envinner,id) (_,m)
            | SynTypeDefnSimpleRepr.Union (_,_,m) -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconUnion,attrs,[],[],inSig,true,m) |> ignore
                // Note: the table of union cases is initially empty
                MakeUnionRepr []

            | SynTypeDefnSimpleRepr.TypeAbbrev _  -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconAbbrev,attrs,[],[],inSig,true,m) |> ignore
                TNoRepr

            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (s,m) -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconILAssemblyCode,attrs,[],[],inSig,true,m) |> ignore
                TAsmRepr s

            | SynTypeDefnSimpleRepr.Record (_,_,m) -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconRecord,attrs,[],[],inSig,true,m) |> ignore
                // Note: the table of record fields is initially empty
                TRecdRepr (MakeRecdFieldsTable  [])

            | SynTypeDefnSimpleRepr.General (kind,_,slotsigs,fields,isConcrete,_,_,_) ->
                let kind = InferTyconKind cenv.g (kind,attrs,slotsigs,fields,inSig,isConcrete,m)
                match kind with 
                | TyconHiddenRepr -> 
                    TNoRepr
                | _ -> 
                    let kind = 
                        match kind with
                        | TyconClass               -> TTyconClass
                        | TyconInterface           -> TTyconInterface
                        | TyconDelegate _          -> TTyconDelegate (MakeSlotSig("Invoke",cenv.g.unit_ty,[],[],[], None))
                        | TyconStruct              -> TTyconStruct 
                        | _ -> error(InternalError("should have inferred tycon kind",m))

                    let repr = { fsobjmodel_kind=kind 
                                 fsobjmodel_vslots=[]
                                 fsobjmodel_rfields=MakeRecdFieldsTable [] }
                    TFSharpObjectRepr repr

            | SynTypeDefnSimpleRepr.Enum _ -> 
                let kind = TTyconEnum
                let repr = { fsobjmodel_kind=kind 
                             fsobjmodel_vslots=[]
                             fsobjmodel_rfields=MakeRecdFieldsTable [] }
                TFSharpObjectRepr repr

        // OK, now fill in the (partially computed) type representation
        tycon.entity_tycon_repr <- repr
        attrs, getFinalAttrs

#if EXTENSIONTYPING
    /// Get the items on the r.h.s. of a 'type X = ABC<...>' definition
    let private TcTyconDefnCore_GetGenerateDeclaration_Rhs rhsType =
        match rhsType with 
        | SynType.App (SynType.LongIdent(LongIdentWithDots(tc,_)),_,args,_commas,_,_postfix,m) -> Some(tc,args,m)
        | SynType.LongIdent (LongIdentWithDots(tc,_) as lidwd) -> Some(tc,[],lidwd.Range)
        | SynType.LongIdentApp (SynType.LongIdent (LongIdentWithDots(tc,_)),LongIdentWithDots(longId,_),_,args,_commas,_,m) -> Some(tc@longId,args,m)
        | _ -> None

    /// Check whether 'type X = ABC<...>' is a generative provided type definition
    let private TcTyconDefnCore_TryAsGenerateDeclaration cenv envinner tpenv (tycon:Tycon, rhsType) =

        let tcref = mkLocalTyconRef tycon
        match TcTyconDefnCore_GetGenerateDeclaration_Rhs rhsType with 
        | None -> None
        | Some (tc,args,m) -> 
            let ad = envinner.eAccessRights
            match ResolveTypeLongIdent cenv.tcSink cenv.nameResolver ItemOccurence.UseInType OpenQualified envinner.eNameResEnv ad tc TypeNameResolutionStaticArgsInfo.DefiniteEmpty PermitDirectReferenceToGeneratedType.Yes with
            | Result tcrefBeforeStaticArguments when 
                  tcrefBeforeStaticArguments.IsProvided && 
                  not tcrefBeforeStaticArguments.IsErased -> 

                    let typeBeforeArguments = 
                        match tcrefBeforeStaticArguments.TypeReprInfo with 
                        | TProvidedTypeExtensionPoint info -> info.ProvidedType
                        | _ -> failwith "unreachable"

                    if ExtensionTyping.IsGeneratedTypeDirectReference (typeBeforeArguments, m) then 
                        let optGeneratedTypePath = Some (tcref.CompilationPath.MangledPath @ [ tcref.LogicalName ])
                        let _hasNoArgs,providedTypeAfterStaticArguments,checkTypeName = TcProvidedTypeAppToStaticConstantArgs cenv envinner optGeneratedTypePath tpenv tcrefBeforeStaticArguments args m
                        let isGenerated = providedTypeAfterStaticArguments.PUntaint((fun st -> not st.IsErased),m)
                        if isGenerated  then 
                           Some (tcrefBeforeStaticArguments, providedTypeAfterStaticArguments, checkTypeName, args, m)
                        else
                           None  // The provided type (after ApplyStaticArguments) must also be marked 'IsErased=false' 
                    else 
                        // This must be a direct reference to a generated type, otherwise it is a type abbreviation
                        None
            | _ -> 
                None


    /// Check and establish a 'type X = ABC<...>' provided type definition
    let private TcTyconDefnCore_Phase1C_EstablishDeclarationForGeneratedSetOfTypes cenv inSig (tycon:Tycon, rhsType:SynType, tcrefForContainer:TyconRef, theRootType:Tainted<ProvidedType>, checkTypeName, args, m) =
        // Explanation: We are definitely on the compilation thread here, we just have not propagated the token this far.
        let ctok = AssumeCompilationThreadWithoutEvidence()

        let tcref = mkLocalTyconRef tycon
        try 
            let resolutionEnvironment =
                if not (isNil args) then 
                   checkTypeName()
                let resolutionEnvironment = 
                    match tcrefForContainer.TypeReprInfo with 
                    | TProvidedTypeExtensionPoint info -> info.ResolutionEnvironment
                    | _ -> failwith "unreachable"
                resolutionEnvironment

            // Build up a mapping from System.Type --> TyconRef/ILTypeRef, to allow reverse-mapping
            // of types

            let previousContext = (theRootType.PApply ((fun x -> x.Context), m)).PUntaint ((fun x -> x), m)
            let lookupILTypeRef, lookupTyconRef = previousContext.GetDictionaries()
                    
            let ctxt = ProvidedTypeContext.Create(lookupILTypeRef, lookupTyconRef)

            // Create a new provided type which captures the reverse-reampping tables.
            let theRootTypeWithRemapping = theRootType.PApply ((fun x -> ProvidedType.ApplyContext(x,ctxt)), m)

            let isRootGenerated,rootProvAssemStaticLinkInfoOpt = 
                let stRootAssembly = theRootTypeWithRemapping.PApply((fun st -> st.Assembly),m)

                cenv.amap.assemblyLoader.GetProvidedAssemblyInfo (ctok, m, stRootAssembly)

            let isRootGenerated = isRootGenerated || theRootTypeWithRemapping.PUntaint((fun st -> not st.IsErased),m)

            if not isRootGenerated then 
                let desig = theRootTypeWithRemapping.TypeProviderDesignation
                let nm = theRootTypeWithRemapping.PUntaint((fun st -> st.FullName),m)
                error(Error(FSComp.SR.etErasedTypeUsedInGeneration(desig,nm),m))

            // In compiled code, all types in the set of generated types end up being both generated and relocated, unless relocation is suppressed
            let isForcedSuppressRelocate = theRootTypeWithRemapping.PUntaint((fun st -> st.IsSuppressRelocate),m) 
            if isForcedSuppressRelocate && canAccessFromEverywhere tycon.Accessibility && not cenv.isScript then 
                errorR(Error(FSComp.SR.tcGeneratedTypesShouldBeInternalOrPrivate(),tcref.Range))

            let isSuppressRelocate = cenv.g.isInteractive || isForcedSuppressRelocate
    
            // Adjust the representation of the container type
            let repr = Construct.NewProvidedTyconRepr(resolutionEnvironment,theRootTypeWithRemapping,
                                                      Import.ImportProvidedType cenv.amap m,
                                                      isSuppressRelocate, 
                                                      m=m)
            tycon.entity_tycon_repr <- repr
            // Record the details so we can map System.Type --> TyconRef
            let ilOrigRootTypeRef = GetOriginalILTypeRefOfProvidedType (theRootTypeWithRemapping, m)
            theRootTypeWithRemapping.PUntaint ((fun st -> ignore(lookupTyconRef.Remove(st.RawSystemType)) ; lookupTyconRef.Add(st.RawSystemType, tcref)), m)

            // Record the details so we can map System.Type --> ILTypeRef, including the relocation if any
            if not isSuppressRelocate then 
                let ilTgtRootTyRef = tycon.CompiledRepresentationForNamedType
                theRootTypeWithRemapping.PUntaint ((fun st -> ignore(lookupILTypeRef.Remove(st.RawSystemType)) ; lookupILTypeRef.Add(st.RawSystemType, ilTgtRootTyRef)), m)

            // Iterate all nested types and force their embedding, to populate the mapping from System.Type --> TyconRef/ILTypeRef.
            // This is only needed for generated types, because for other types the System.Type objects self-describe
            // their corresponding F# type.
            let rec doNestedType (eref: EntityRef) (st: Tainted<ProvidedType>) = 

                // Check the type is a generated type
                let isGenerated,provAssemStaticLinkInfoOpt = 
                    let stAssembly = st.PApply((fun st -> st.Assembly),m)
                    cenv.amap.assemblyLoader.GetProvidedAssemblyInfo (ctok, m, stAssembly)

                let isGenerated = isGenerated || st.PUntaint((fun st -> not st.IsErased),m)

                if not isGenerated then 
                    let desig = st.TypeProviderDesignation
                    let nm = st.PUntaint((fun st -> st.FullName),m)
                    error(Error(FSComp.SR.etErasedTypeUsedInGeneration(desig,nm),m))

                // Embed the type into the module we're compiling
                let cpath = eref.CompilationPath.NestedCompPath eref.LogicalName ModuleOrNamespaceKind.ModuleOrType
                let access = combineAccess tycon.Accessibility (if st.PUntaint((fun st -> st.IsPublic || st.IsNestedPublic), m) then taccessPublic else taccessPrivate cpath)

                let nestedTycon = Construct.NewProvidedTycon(resolutionEnvironment, st, 
                                                             Import.ImportProvidedType cenv.amap m, 
                                                             isSuppressRelocate, 
                                                             m=m, cpath=cpath, access = access)
                eref.ModuleOrNamespaceType.AddProvidedTypeEntity(nestedTycon)

                let nestedTyRef = eref.NestedTyconRef nestedTycon
                let ilOrigTypeRef = GetOriginalILTypeRefOfProvidedType (st, m)
                                
                // Record the details so we can map System.Type --> TyconRef
                st.PUntaint ((fun st -> ignore(lookupTyconRef.Remove(st.RawSystemType)) ; lookupTyconRef.Add(st.RawSystemType, nestedTyRef)), m)

                if isGenerated then 
                    let ilTgtTyRef = nestedTycon.CompiledRepresentationForNamedType
                    // Record the details so we can map System.Type --> ILTypeRef
                    st.PUntaint ((fun st -> ignore(lookupILTypeRef.Remove(st.RawSystemType)) ; lookupILTypeRef.Add(st.RawSystemType, ilTgtTyRef)), m)

                    // Record the details so we can build correct ILTypeDefs during static linking rewriting
                    if not isSuppressRelocate then 
                        match provAssemStaticLinkInfoOpt with 
                        | Some provAssemStaticLinkInfo -> provAssemStaticLinkInfo.ILTypeMap.[ilOrigTypeRef] <- ilTgtTyRef
                        | None -> ()
                       
                    ProviderGeneratedType(ilOrigTypeRef, ilTgtTyRef, doNestedTypes nestedTyRef st)
                else
                    ProviderGeneratedType(ilOrigTypeRef, ilOrigTypeRef, doNestedTypes nestedTyRef st)


                //System.Diagnostics.Debug.Assert eref.TryDeref.IsSome

            and doNestedTypes (eref: EntityRef) (st: Tainted<ProvidedType>) =
                st.PApplyArray((fun st -> st.GetAllNestedTypes()), "GetAllNestedTypes", m)
                |> Array.map (doNestedType eref)
                |> Array.toList

            let nested = doNestedTypes tcref theRootTypeWithRemapping 
            if not isSuppressRelocate then 

                let ilTgtRootTyRef = tycon.CompiledRepresentationForNamedType
                match rootProvAssemStaticLinkInfoOpt with 
                | Some provAssemStaticLinkInfo -> provAssemStaticLinkInfo.ILTypeMap.[ilOrigRootTypeRef] <- ilTgtRootTyRef
                | None -> ()

                if not inSig then 
                    cenv.amap.assemblyLoader.RecordGeneratedTypeRoot (ProviderGeneratedType(ilOrigRootTypeRef, ilTgtRootTyRef, nested))

        with e -> 
            errorRecovery e rhsType.Range 
#endif

    /// Establish any type abbreviations
    ///
    /// e.g. for  
    ///    type B<'a when 'a :  C> = DDD of C
    ///    and  C = B<int>
    ///
    /// we establish
    ///
    ///   Entity('B) 
    ///       TypeAbbrev = TType_app(Entity('int'),[])
    ///
    /// and for
    ///
    ///    type C = B
    ///
    /// we establish
    ///       TypeAbbrev = TType_app(Entity('B'),[])
    ///
    /// Note that for 
    ///              type PairOfInts = int * int
    /// then after running this phase and checking for cycles, operations 
    // such as 'isRefTupleTy' will return reliable results, e.g. isRefTupleTy on the 
    /// TAST type for 'PairOfInts' will report 'true' 
    //
    let private TcTyconDefnCore_Phase1C_Phase1E_EstablishAbbreviations cenv envinner inSig tpenv pass (MutRecDefnsPhase1DataForTycon(_,synTyconRepr,_,_,_,_)) (tycon:Tycon) (attrs:Attribs) =
        let m = tycon.Range
        let checkCxs = if (pass = SecondPass) then CheckCxs else NoCheckCxs
        let firstPass = (pass = FirstPass)
        try 
            let id = tycon.Id
            let thisTyconRef = mkLocalTyconRef tycon

            let hasMeasureAttr = HasFSharpAttribute cenv.g cenv.g.attrib_MeasureAttribute attrs
            let hasMeasureableAttr = HasFSharpAttribute cenv.g cenv.g.attrib_MeasureableAttribute attrs
            let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
            let envinner = MakeInnerEnvForTyconRef cenv envinner thisTyconRef false 

            match synTyconRepr with 

            // This unfortunate case deals with "type x = A" 
            // In F# this only defines a new type if A is not in scope 
            // as a type constructor, or if the form type A = A is used. 
            // "type x = | A" can always be used instead. 
            | TyconCoreAbbrevThatIsReallyAUnion (hasMeasureAttr,envinner,id) _ -> ()
            
            | SynTypeDefnSimpleRepr.TypeAbbrev(ParserDetail.Ok, rhsType,m) ->

#if EXTENSIONTYPING
              // Check we have not already decided that this is a generative provided type definition. If we have already done this (i.e. this is the second pass
              // for a generative provided type definition, then there is no more work to do).
              if (match tycon.entity_tycon_repr with TNoRepr -> true | _ -> false) then 

                // Determine if this is a generative type definition.
                match TcTyconDefnCore_TryAsGenerateDeclaration cenv envinner tpenv (tycon, rhsType) with 
                | Some (tcrefForContainer, providedTypeAfterStaticArguments, checkTypeName, args, m) ->
                   // If this is a generative provided type definition then establish the provided type and all its nested types. Only do this on the first pass.
                   if firstPass then 
                       TcTyconDefnCore_Phase1C_EstablishDeclarationForGeneratedSetOfTypes cenv inSig (tycon, rhsType, tcrefForContainer, providedTypeAfterStaticArguments, checkTypeName, args, m)
                | None -> 
#else
                  ignore inSig 
#endif

                  // This case deals with ordinary type and measure abbreviations 
                  if not hasMeasureableAttr then 
                    let kind = if hasMeasureAttr then TyparKind.Measure else TyparKind.Type
                    let ty,_ = TcTypeOrMeasureAndRecover (Some kind) cenv NoNewTypars checkCxs ItemOccurence.UseInType envinner tpenv rhsType

                    if not firstPass then 
                        let ftyvs = freeInTypeLeftToRight cenv.g false ty 
                        let typars = tycon.Typars(m)
                        if ftyvs.Length <> typars.Length then 
                            errorR(Deprecated(FSComp.SR.tcTypeAbbreviationHasTypeParametersMissingOnType(),tycon.Range))

                    if firstPass then
                        tycon.entity_tycon_abbrev <- Some ty

            | _ -> ()
        
        with e -> 
            errorRecovery e m

    // Third phase: check and publish the supr types. Run twice, once before constraints are established
    // and once after
    let private TcTyconDefnCore_Phase1D_Phase1F_EstablishSuperTypesAndInterfaceTypes cenv tpenv inSig pass (envMutRec, mutRecDefns:MutRecShape<(_ * (Tycon * (Attribs * _)) option),_,_,_,_> list) = 
        let checkCxs = if (pass = SecondPass) then CheckCxs else NoCheckCxs
        let firstPass = (pass = FirstPass)

        // Publish the immediately declared interfaces. 
        let tyconWithImplementsL = 
            (envMutRec, mutRecDefns) ||> MutRecShapes.mapTyconsWithEnv (fun envinner (origInfo,tyconAndAttrsOpt)  -> 
               match origInfo, tyconAndAttrsOpt with 
               | (typeDefCore,_,_), Some (tycon, (attrs,_)) ->
                let (MutRecDefnsPhase1DataForTycon(_,synTyconRepr,explicitImplements,_,_,_)) = typeDefCore
                let m = tycon.Range
                let tcref = mkLocalTyconRef tycon
                let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
                let envinner = MakeInnerEnvForTyconRef cenv envinner tcref false 
                
                let implementedTys,_ = List.mapFold (mapFoldFst (TcTypeAndRecover cenv NoNewTypars checkCxs ItemOccurence.UseInType envinner)) tpenv explicitImplements

                if firstPass then 
                    tycon.entity_attribs <- attrs

                let implementedTys,inheritedTys = 
                    match synTyconRepr with 
                    | SynTypeDefnSimpleRepr.Exception _ -> [], []
                    | SynTypeDefnSimpleRepr.General (kind,inherits,slotsigs,fields,isConcrete,_,_,m) ->
                        let kind = InferTyconKind cenv.g (kind,attrs,slotsigs,fields,inSig,isConcrete,m)

                        let inherits = inherits |> List.map (fun (ty,m,_) -> (ty,m)) 
                        let inheritedTys = fst (List.mapFold (mapFoldFst (TcTypeAndRecover cenv NoNewTypars checkCxs ItemOccurence.UseInType envinner)) tpenv inherits)
                        let implementedTys,inheritedTys =   
                            match kind with 
                            | TyconInterface -> 
                                explicitImplements |> List.iter (fun (_,m) -> errorR(Error(FSComp.SR.tcInterfacesShouldUseInheritNotInterface(),m)))
                                (implementedTys @ inheritedTys),[] 
                            | _ -> implementedTys, inheritedTys
                        implementedTys,inheritedTys 
                    | SynTypeDefnSimpleRepr.Enum _ | SynTypeDefnSimpleRepr.None _ | SynTypeDefnSimpleRepr.TypeAbbrev _
                    
                    | SynTypeDefnSimpleRepr.Union _ | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ | SynTypeDefnSimpleRepr.Record _ -> 
                        // REVIEW: we could do the IComparable/IStructuralHash interface analysis here. 
                        // This would let the type satisfy more recursive IComparable/IStructuralHash constraints 
                        implementedTys,[]

                for (implementedTy,m) in implementedTys do
                    if firstPass && isErasedType cenv.g implementedTy then 
                        errorR(Error(FSComp.SR.tcCannotInheritFromErasedType(),m)) 

                // Publish interfaces, but only on the first pass, to avoid a duplicate interface check 
                if firstPass then 
                    implementedTys |> List.iter (fun (ty,m) -> PublishInterface cenv envinner.DisplayEnv tcref m false ty) 

                Some (attrs,inheritedTys,synTyconRepr,tycon)
               | _ -> None)

        // Publish the attributes and supertype  
        tyconWithImplementsL |> MutRecShapes.iterTycons (Option.iter (fun (attrs,inheritedTys, synTyconRepr, tycon) -> 
          let m = tycon.Range
          try 
              let super = 
                  match synTyconRepr with 
                  | SynTypeDefnSimpleRepr.Exception _ -> Some cenv.g.exn_ty
                  | SynTypeDefnSimpleRepr.None _ -> None
                  | SynTypeDefnSimpleRepr.TypeAbbrev _ -> None
                  | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> None
                  | SynTypeDefnSimpleRepr.Union _ 
                  | SynTypeDefnSimpleRepr.Record _ ->
                      if tycon.IsStructRecordOrUnionTycon then Some(cenv.g.system_Value_typ)
                      else None
                  | SynTypeDefnSimpleRepr.General (kind,_,slotsigs,fields,isConcrete,_,_,_) ->
                      let kind = InferTyconKind cenv.g (kind,attrs,slotsigs,fields,inSig,isConcrete,m)
                                           
                      match inheritedTys with 
                      | [] -> 
                          match kind with 
                          | TyconStruct -> Some(cenv.g.system_Value_typ)
                          | TyconDelegate _ -> Some(cenv.g.system_MulticastDelegate_typ )
                          | TyconHiddenRepr | TyconClass | TyconInterface -> None
                          | _ -> error(InternalError("should have inferred tycon kind",m)) 

                      | [(ty,m)] -> 
                          if not firstPass && not (match kind with TyconClass -> true | _ -> false) then 
                              errorR (Error(FSComp.SR.tcStructsInterfacesEnumsDelegatesMayNotInheritFromOtherTypes(),m)) 
                          CheckSuperType cenv ty m 
                          if isTyparTy cenv.g ty then 
                              if firstPass  then 
                                  errorR(Error(FSComp.SR.tcCannotInheritFromVariableType(),m)) 
                              Some cenv.g.obj_ty // a "super" that is a variable type causes grief later
                          else                          
                              Some ty 
                      | _ -> 
                          error(Error(FSComp.SR.tcTypesCannotInheritFromMultipleConcreteTypes(),m))

                  | SynTypeDefnSimpleRepr.Enum _ -> 
                      Some(cenv.g.system_Enum_typ) 

              // Publish the super type
              tycon.TypeContents.tcaug_super <- super
              
           with e -> errorRecovery e m))

    /// Establish the fields, dispatch slots and union cases of a type
    let private TcTyconDefnCore_Phase1G_EstablishRepresentation cenv envinner tpenv inSig (MutRecDefnsPhase1DataForTycon(_,synTyconRepr,_,_,_,_)) (tycon:Tycon) (attrs:Attribs) =
        let m = tycon.Range
        try 
            let id = tycon.Id
            let thisTyconRef = mkLocalTyconRef tycon
            let innerParent = Parent thisTyconRef
            let thisTyInst,thisTy = generalizeTyconRef thisTyconRef

            let hasAbstractAttr = HasFSharpAttribute cenv.g cenv.g.attrib_AbstractClassAttribute attrs
            let hasSealedAttr = 
                // The special case is needed for 'unit' because the 'Sealed' attribute is not yet available when this type is defined.
                if cenv.g.compilingFslib && id.idText = "Unit" then 
                    Some true
                else
                    TryFindFSharpBoolAttribute cenv.g cenv.g.attrib_SealedAttribute attrs
            let hasMeasureAttr = HasFSharpAttribute cenv.g cenv.g.attrib_MeasureAttribute attrs
            
            // REVIEW: for hasMeasureableAttr we need to be stricter about checking these
            // are only used on exactly the right kinds of type definitions and not inconjunction with other attributes.
            let hasMeasureableAttr = HasFSharpAttribute cenv.g cenv.g.attrib_MeasureableAttribute attrs
            let hasCLIMutable = HasFSharpAttribute cenv.g cenv.g.attrib_CLIMutableAttribute attrs
            
            let hasStructLayoutAttr = HasFSharpAttribute cenv.g cenv.g.attrib_StructLayoutAttribute attrs
            let hasAllowNullLiteralAttr = TryFindFSharpBoolAttribute cenv.g cenv.g.attrib_AllowNullLiteralAttribute attrs = Some(true)

            if hasAbstractAttr then 
                tycon.TypeContents.tcaug_abstract <- true

            tycon.entity_attribs <- attrs
            let noAbstractClassAttributeCheck() = 
                if hasAbstractAttr then errorR (Error(FSComp.SR.tcOnlyClassesCanHaveAbstract(),m))
                
            let noAllowNullLiteralAttributeCheck() = 
                if hasAllowNullLiteralAttr then errorR (Error(FSComp.SR.tcRecordsUnionsAbbreviationsStructsMayNotHaveAllowNullLiteralAttribute(),m))
                
                
            let allowNullLiteralAttributeCheck() = 
                if hasAllowNullLiteralAttr then 
                    tycon.TypeContents.tcaug_super |> Option.iter (fun ty -> if not (TypeNullIsExtraValue cenv.g m ty) then errorR (Error(FSComp.SR.tcAllowNullTypesMayOnlyInheritFromAllowNullTypes(),m)))
                    tycon.ImmediateInterfaceTypesOfFSharpTycon |> List.iter (fun ty -> if not (TypeNullIsExtraValue cenv.g m ty) then errorR (Error(FSComp.SR.tcAllowNullTypesMayOnlyInheritFromAllowNullTypes(),m)))
                
                
            let structLayoutAttributeCheck(allowed) = 
                if hasStructLayoutAttr  then 
                    if allowed then 
                        warning(PossibleUnverifiableCode(m))
                    elif thisTyconRef.Typars(m).Length > 0 then 
                        errorR (Error(FSComp.SR.tcGenericTypesCannotHaveStructLayout(),m))
                    else
                        errorR (Error(FSComp.SR.tcOnlyStructsCanHaveStructLayout(),m))
                
            let hiddenReprChecks(hasRepr) =
                 structLayoutAttributeCheck(false)
                 if hasSealedAttr = Some(false) || (hasRepr && hasSealedAttr <> Some(true) && not (id.idText = "Unit" && cenv.g.compilingFslib) ) then 
                    errorR(Error(FSComp.SR.tcRepresentationOfTypeHiddenBySignature(),m))
                 if hasAbstractAttr then 
                     errorR (Error(FSComp.SR.tcOnlyClassesCanHaveAbstract(),m))

            let noMeasureAttributeCheck() = 
                if hasMeasureAttr then errorR (Error(FSComp.SR.tcOnlyTypesRepresentingUnitsOfMeasureCanHaveMeasure(),m))

            let noCLIMutableAttributeCheck() = 
                if hasCLIMutable then errorR (Error(FSComp.SR.tcThisTypeMayNotHaveACLIMutableAttribute(),m))

            let noSealedAttributeCheck(k) = 
                if hasSealedAttr = Some(true) then errorR (Error(k(),m))

            let noFieldsCheck(fields':RecdField list) = 
                match fields' with 
                | (rf :: _) -> errorR (Error(FSComp.SR.tcInterfaceTypesAndDelegatesCannotContainFields(),rf.Range))
                | _ -> ()

                
            let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
            let envinner = MakeInnerEnvForTyconRef cenv envinner thisTyconRef false 


            // Notify the Language Service about field names in record/class declaration
            let ad = envinner.eAccessRights
            let writeFakeRecordFieldsToSink (fields:RecdField list) =
                let nenv = envinner.NameEnv
                // Record fields should be visible from IntelliSense, so add fake names for them (similarly to "let a = ..")
                for fspec in fields do
                    if not fspec.IsCompilerGenerated then
                        let info = RecdFieldInfo(thisTyInst, thisTyconRef.MakeNestedRecdFieldRef fspec)
                        let nenv' = AddFakeNameToNameEnv fspec.Name nenv (Item.RecdField info) 
                        // Name resolution gives better info for tooltips
                        let item = FreshenRecdFieldRef cenv.nameResolver m (thisTyconRef.MakeNestedRecdFieldRef fspec)
                        CallNameResolutionSink cenv.tcSink (fspec.Range,nenv,item,item,ItemOccurence.Binding,envinner.DisplayEnv,ad)
                        // Environment is needed for completions
                        CallEnvSink cenv.tcSink (fspec.Range, nenv', ad)

            // Notify the Language Service about constructors in discriminated union declaration
            let writeFakeUnionCtorsToSink (unionCases: UnionCase list) = 
                let nenv = envinner.NameEnv
                // Constructors should be visible from IntelliSense, so add fake names for them 
                for unionCase in unionCases do
                    let info = UnionCaseInfo(thisTyInst,mkUnionCaseRef thisTyconRef unionCase.Id.idText)
                    let nenv' = AddFakeNameToNameEnv unionCase.Id.idText nenv (Item.UnionCase(info,false)) 
                    // Report to both - as in previous function
                    let item = Item.UnionCase(info,false)
                    CallNameResolutionSink cenv.tcSink (unionCase.Range,nenv,item,item,ItemOccurence.Binding,envinner.DisplayEnv,ad)
                    CallEnvSink cenv.tcSink (unionCase.Id.idRange, nenv', ad)
            
            let typeRepr, baseValOpt, safeInitInfo = 
                match synTyconRepr with 

                | SynTypeDefnSimpleRepr.Exception synExnDefnRepr -> 
                    let parent = Parent (mkLocalTyconRef tycon)
                    TcExceptionDeclarations.TcExnDefnCore_Phase1G_EstablishRepresentation cenv envinner parent tycon synExnDefnRepr |> ignore
                    TNoRepr, None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.None _ -> 
                    hiddenReprChecks(false)
                    noAllowNullLiteralAttributeCheck()
                    if hasMeasureAttr then 
                        let repr = TFSharpObjectRepr { fsobjmodel_kind=TTyconClass 
                                                       fsobjmodel_vslots=[]
                                                       fsobjmodel_rfields= MakeRecdFieldsTable [] }
                        repr, None, NoSafeInitInfo
                    else 
                        TNoRepr, None, NoSafeInitInfo

                // This unfortunate case deals with "type x = A" 
                // In F# this only defines a new type if A is not in scope 
                // as a type constructor, or if the form type A = A is used. 
                // "type x = | A" can always be used instead. 
                | TyconCoreAbbrevThatIsReallyAUnion (hasMeasureAttr,envinner,id) (unionCaseName,_) ->
                          
                    structLayoutAttributeCheck(false)
                    noAllowNullLiteralAttributeCheck()
                    TcRecdUnionAndEnumDeclarations.CheckUnionCaseName  cenv unionCaseName.idText unionCaseName.idRange
                    let unionCase = NewUnionCase unionCaseName unionCaseName.idText [] thisTy [] XmlDoc.Empty tycon.Accessibility
                    writeFakeUnionCtorsToSink [ unionCase ]
                    MakeUnionRepr [ unionCase ], None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.TypeAbbrev(ParserDetail.ThereWereSignificantParseErrorsSoDoNotTypecheckThisNode, _rhsType,_) ->
                    TNoRepr, None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.TypeAbbrev(ParserDetail.Ok, rhsType,_) ->
                    if hasSealedAttr = Some(true) then 
                        errorR (Error(FSComp.SR.tcAbbreviatedTypesCannotBeSealed(),m))
                    noAbstractClassAttributeCheck()
                    noAllowNullLiteralAttributeCheck()
                    if hasMeasureableAttr  then 
                        let kind = if hasMeasureAttr then TyparKind.Measure else TyparKind.Type
                        let theTypeAbbrev,_ = TcTypeOrMeasureAndRecover (Some kind) cenv NoNewTypars CheckCxs ItemOccurence.UseInType envinner tpenv rhsType

                        TMeasureableRepr theTypeAbbrev, None, NoSafeInitInfo
                    // If we already computed a representation, e.g. for a generative type definition, then don't change it here.
                    elif (match tycon.TypeReprInfo with TNoRepr -> false | _ -> true)  then 
                        tycon.TypeReprInfo , None, NoSafeInitInfo
                    else 
                        TNoRepr, None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.Union (_,unionCases,_) -> 
                    noCLIMutableAttributeCheck()
                    noMeasureAttributeCheck()
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedDU
                    noAbstractClassAttributeCheck()
                    noAllowNullLiteralAttributeCheck()
                    structLayoutAttributeCheck(false)
                    let unionCases = TcRecdUnionAndEnumDeclarations.TcUnionCaseDecls cenv envinner innerParent thisTy tpenv unionCases
                        
                    if tycon.IsStructRecordOrUnionTycon && unionCases.Length > 1 then 
                      let fieldNames = [ for uc in unionCases do for ft in uc.FieldTable.TrueInstanceFieldsAsList do yield ft.Name ]
                      if fieldNames |> List.distinct |> List.length <> fieldNames.Length then 
                          errorR(Error(FSComp.SR.tcStructUnionMultiCaseDistinctFields(),m))

                    writeFakeUnionCtorsToSink unionCases
                    MakeUnionRepr unionCases, None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.Record (_,fields,_) -> 
                    noMeasureAttributeCheck()
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedRecord
                    noAbstractClassAttributeCheck()
                    noAllowNullLiteralAttributeCheck()
                    structLayoutAttributeCheck(true)  // these are allowed for records
                    let recdFields = TcRecdUnionAndEnumDeclarations.TcNamedFieldDecls cenv envinner innerParent false tpenv fields
                    recdFields |> CheckDuplicates (fun f -> f.Id) "field"  |> ignore
                    writeFakeRecordFieldsToSink recdFields
                    TRecdRepr (MakeRecdFieldsTable recdFields), None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (s,_) -> 
                    noCLIMutableAttributeCheck()
                    noMeasureAttributeCheck()
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedAssemblyCode
                    noAllowNullLiteralAttributeCheck()
                    structLayoutAttributeCheck(false)
                    noAbstractClassAttributeCheck()
                    TAsmRepr s, None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.General (kind,inherits,slotsigs,fields,isConcrete,isIncrClass,implicitCtorSynPats,_) ->
                    let userFields = TcRecdUnionAndEnumDeclarations.TcNamedFieldDecls cenv envinner innerParent isIncrClass tpenv fields
                    let implicitStructFields = 
                        [ // For structs with an implicit ctor, determine the fields immediately based on the arguments
                          match implicitCtorSynPats with 
                          | None -> 
                              ()
                          | Some spats -> 
                              if tycon.IsFSharpStructOrEnumTycon then 
                                  let ctorArgNames,(_,names,_) = TcSimplePatsOfUnknownType cenv true CheckCxs envinner tpenv (SynSimplePats.SimplePats (spats,m))
                                  for arg in ctorArgNames do
                                      let ty = names.[arg].Type
                                      let id = names.[arg].Ident
                                      let taccess = TAccess [envinner.eAccessPath]
                                      yield NewRecdField false None id ty false false [(*no property attributes*)] [(*no field attributes *)] XmlDoc.Empty taccess (*compiler generated:*)true ]
                    
                    (userFields @ implicitStructFields) |> CheckDuplicates (fun f -> f.Id) "field"  |> ignore
                    writeFakeRecordFieldsToSink userFields
                    
                    let superTy = tycon.TypeContents.tcaug_super
                    let containerInfo = TyconContainerInfo(innerParent, thisTyconRef, thisTyconRef.Typars(m), NoSafeInitInfo)
                    let kind = InferTyconKind cenv.g (kind,attrs,slotsigs,fields,inSig,isConcrete,m)
                    match kind with 
                    | TyconHiddenRepr  -> 
                        hiddenReprChecks(true)
                        noAllowNullLiteralAttributeCheck()
                        TNoRepr, None, NoSafeInitInfo
                    | _ ->

                        // Note: for a mutually recursive set we can't check this condition 
                        // until "isSealedTy" and "isClassTy" give reliable results. 
                        superTy |> Option.iter (fun ty -> 
                            let m = match inherits with | [] -> m | ((_,m,_) :: _) -> m
                            if isSealedTy cenv.g ty then 
                                errorR(Error(FSComp.SR.tcCannotInheritFromSealedType(),m))
                            elif not (isClassTy cenv.g ty) then 
                                errorR(Error(FSComp.SR.tcCannotInheritFromInterfaceType(),m)))

                        let kind = 
                            match kind with 
                              | TyconStruct -> 
                                  noCLIMutableAttributeCheck()
                                  noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedStruct
                                  noAbstractClassAttributeCheck()
                                  noAllowNullLiteralAttributeCheck()
                                  if not (isNil slotsigs) then 
                                    errorR (Error(FSComp.SR.tcStructTypesCannotContainAbstractMembers(),m)) 
                                  structLayoutAttributeCheck(true)

                                  TTyconStruct
                              | TyconInterface -> 
                                  if hasSealedAttr = Some(true) then errorR (Error(FSComp.SR.tcInterfaceTypesCannotBeSealed(),m))
                                  noCLIMutableAttributeCheck()
                                  structLayoutAttributeCheck(false)
                                  noAbstractClassAttributeCheck()
                                  allowNullLiteralAttributeCheck()
                                  noFieldsCheck(userFields)
                                  TTyconInterface
                              | TyconClass -> 
                                  noCLIMutableAttributeCheck()
                                  structLayoutAttributeCheck(not isIncrClass)
                                  allowNullLiteralAttributeCheck()
                                  TTyconClass
                              | TyconDelegate (ty,arity) -> 
                                  noCLIMutableAttributeCheck()
                                  noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedDelegate
                                  structLayoutAttributeCheck(false)
                                  noAllowNullLiteralAttributeCheck()
                                  noAbstractClassAttributeCheck()
                                  noFieldsCheck(userFields)
                                  let ty',_ = TcTypeAndRecover cenv NoNewTypars CheckCxs ItemOccurence.UseInType envinner tpenv ty
                                  let _,curriedArgInfos,returnTy,_ = GetTopValTypeInCompiledForm cenv.g (arity |> TranslateTopValSynInfo m (TcAttributes cenv envinner)  |> TranslatePartialArity []) ty' m
                                  if curriedArgInfos.Length < 1 then error(Error(FSComp.SR.tcInvalidDelegateSpecification(),m))
                                  if curriedArgInfos.Length > 1 then error(Error(FSComp.SR.tcDelegatesCannotBeCurried(),m))
                                  let ttps = thisTyconRef.Typars(m)
                                  let fparams = curriedArgInfos.Head |> List.map MakeSlotParam 
                                  TTyconDelegate (MakeSlotSig("Invoke",thisTy,ttps,[],[fparams], returnTy))
                              | _ -> 
                                  error(InternalError("should have inferred tycon kind",m))

                        let baseIdOpt = 
                            match synTyconRepr with 
                            | SynTypeDefnSimpleRepr.None _ -> None
                            | SynTypeDefnSimpleRepr.Exception _ -> None
                            | SynTypeDefnSimpleRepr.TypeAbbrev _ -> None
                            | SynTypeDefnSimpleRepr.Union _ -> None
                            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _ -> None
                            | SynTypeDefnSimpleRepr.Record _ -> None
                            | SynTypeDefnSimpleRepr.Enum _ -> None
                            | SynTypeDefnSimpleRepr.General (_,inherits,_,_,_,_,_,_) ->
                                match inherits with 
                                | [] -> None
                                | ((_,m,baseIdOpt) :: _) -> 
                                    match baseIdOpt with 
                                    | None -> Some(ident("base",m)) 
                                    | Some id -> Some(id)
                            
                        let abstractSlots = 
                            [ for (valSpfn,memberFlags) in slotsigs do 

                                  let (ValSpfn(_, _, _, _, _valSynData, _, _, _, _,_, m)) = valSpfn 

                                  CheckMemberFlags cenv.g None NewSlotsOK OverridesOK memberFlags m
                                  
                                  let slots = fst (TcAndPublishValSpec (cenv,envinner,containerInfo,ModuleOrMemberBinding,Some memberFlags,tpenv,valSpfn))
                                  // Multiple slots may be returned, e.g. for 
                                  //    abstract P : int with get,set
                                  
                                  for slot in slots do 
                                      yield mkLocalValRef slot ]

                        let baseValOpt = MakeAndPublishBaseVal cenv envinner baseIdOpt (superOfTycon cenv.g tycon)
                        let safeInitInfo = ComputeInstanceSafeInitInfo cenv envinner thisTyconRef.Range thisTy
                        let safeInitFields = match safeInitInfo with SafeInitField (_, fld) -> [fld] | NoSafeInitInfo -> []
                        
                        let repr = 
                            TFSharpObjectRepr 
                                { fsobjmodel_kind=kind 
                                  fsobjmodel_vslots= abstractSlots
                                  fsobjmodel_rfields=MakeRecdFieldsTable (userFields @ implicitStructFields  @ safeInitFields) } 
                        repr, baseValOpt, safeInitInfo

                | SynTypeDefnSimpleRepr.Enum (decls,m) -> 
                    let fieldTy,fields' = TcRecdUnionAndEnumDeclarations.TcEnumDecls cenv envinner innerParent thisTy decls
                    let kind = TTyconEnum
                    structLayoutAttributeCheck(false)
                    noCLIMutableAttributeCheck()
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedEnum
                    noAllowNullLiteralAttributeCheck()
                    let vfld = NewRecdField false None (ident("value__",m))  fieldTy false false [] [] XmlDoc.Empty taccessPublic true
                    
                    if not (ListSet.contains (typeEquiv cenv.g) fieldTy [ cenv.g.int32_ty; cenv.g.int16_ty; cenv.g.sbyte_ty; cenv.g.int64_ty; cenv.g.char_ty; cenv.g.bool_ty; cenv.g.uint32_ty; cenv.g.uint16_ty; cenv.g.byte_ty; cenv.g.uint64_ty ]) then 
                        errorR(Error(FSComp.SR.tcInvalidTypeForLiteralEnumeration(),m))

                    writeFakeRecordFieldsToSink fields' 
                    let repr = 
                        TFSharpObjectRepr 
                            { fsobjmodel_kind=kind 
                              fsobjmodel_vslots=[]
                              fsobjmodel_rfields= MakeRecdFieldsTable (vfld :: fields') }
                    repr, None, NoSafeInitInfo
            
            tycon.entity_tycon_repr <- typeRepr
            // We check this just after establishing the representation
            if TyconHasUseNullAsTrueValueAttribute cenv.g tycon && not (CanHaveUseNullAsTrueValueAttribute cenv.g tycon) then 
                errorR(Error(FSComp.SR.tcInvalidUseNullAsTrueValue(),m))
                
            // validate ConditionalAttribute, should it be applied (it's only valid on a type if the type is an attribute type)
            match attrs |> List.tryFind (IsMatchingFSharpAttribute cenv.g cenv.g.attrib_ConditionalAttribute) with
            | Some _ ->
                if not(ExistsInEntireHierarchyOfType (fun t -> typeEquiv cenv.g t (mkAppTy cenv.g.tcref_System_Attribute [])) cenv.g cenv.amap m AllowMultiIntfInstantiations.Yes thisTy) then
                    errorR(Error(FSComp.SR.tcConditionalAttributeUsage(),m))
            | _ -> ()         
                   
            (baseValOpt, safeInitInfo)
        with e -> 
            errorRecovery e m 
            None, NoSafeInitInfo

    /// Check that a set of type definitions is free of cycles in abbreviations
    let private TcTyconDefnCore_CheckForCyclicAbbreviations tycons = 

        let edgesFrom (tycon:Tycon) =

            let rec accInAbbrevType ty acc  = 
                match stripTyparEqns ty with 
                | TType_tuple (_,l) -> accInAbbrevTypes l acc
                | TType_ucase (UCRef(tc,_),tinst) 
                | TType_app (tc,tinst) -> 
                    let tycon2 = tc.Deref
                    let acc = accInAbbrevTypes tinst acc
                    // Record immediate recursive references 
                    if ListSet.contains (===) tycon2 tycons  then 
                        (tycon,tycon2) :: acc 
                    // Expand the representation of abbreviations 
                    elif tc.IsTypeAbbrev then
                        accInAbbrevType (reduceTyconRefAbbrev tc tinst) acc
                    // Otherwise H<inst> - explore the instantiation. 
                    else 
                        acc

                | TType_fun (d,r) -> 
                    accInAbbrevType d (accInAbbrevType r acc)
                
                | TType_var _ -> acc
                
                | TType_forall (_,r) -> accInAbbrevType r acc
                
                | TType_measure ms -> accInMeasure ms acc

            and accInMeasure ms acc =
                match stripUnitEqns ms with
                | Measure.Con tc when ListSet.contains (===) tc.Deref tycons  ->  
                    (tycon, tc.Deref) :: acc
                | Measure.Con tc when tc.IsTypeAbbrev  ->              
                    accInMeasure (reduceTyconRefAbbrevMeasureable tc) acc
                | Measure.Prod (ms1, ms2) -> accInMeasure ms1 (accInMeasure ms2 acc)
                | Measure.Inv ms -> accInMeasure ms acc
                | _ -> acc

            and accInAbbrevTypes tys acc = 
                List.foldBack accInAbbrevType tys acc
            
            match tycon.TypeAbbrev with 
            | None -> []
            | Some ty -> accInAbbrevType ty []

        let edges = List.collect edgesFrom tycons
        let graph = Graph<Tycon, Stamp> ((fun tc -> tc.Stamp), tycons, edges)
        graph.IterateCycles (fun path -> 
            let tycon = path.Head 
            // The thing is cyclic. Set the abbreviation and representation to be "None" to stop later VS crashes
            tycon.entity_tycon_abbrev <- None
            tycon.entity_tycon_repr <- TNoRepr
            errorR(Error(FSComp.SR.tcTypeDefinitionIsCyclic(),tycon.Range)))


    /// Check that a set of type definitions is free of inheritance cycles
    let TcTyconDefnCore_CheckForCyclicStructsAndInheritance cenv tycons =
        // Overview:
        // Given several tycons now being defined (the "intial" tycons).
        // Look for cycles in inheritance and struct-field-containment.
        //
        // The graph is on the (initial) type constructors (not types (e.g. tycon instantiations)).
        // Closing under edges:
        // 1. (tycon,superTycon)     -- tycon (initial) to the tycon of its super type.
        // 2. (tycon,interfaceTycon) -- tycon (initial) to the tycon of an interface it implements.
        // 3. (tycon,T)              -- tycon (initial) is a struct with a field (static or instance) that would store a T<_>
        //                              where storing T<_> means is T<_>
        //                                                    or is a struct<instantiation> with an instance field that stores T<_>.
        // The implementation only stores edges between (initial) tycons.
        //
        // The special case "S<'a> static field on S<'a>" is allowed, so no #3 edge is collected for this.
        // Only static fields for current tycons need to be followed. Previous tycons are assumed (previously checked) OK.
        //
        // BEGIN: EARLIER COMMENT
        //        Of course structs are not allowed to contain instance fields of their own type:
        //         type S = struct { field x : S } 
        //
        //        In addition, see bug 3429. In the .NET IL structs are allowed to contain 
        //        static fields of their exact generic type, e.g.
        //         type S    = struct { static field x : S    } 
        //         type S<T> = struct { static field x : S<T> } 
        //        but not
        //         type S<T> = struct { static field x : S<int> } 
        //         type S<T> = struct { static field x : S<T[]> } 
        //        etc.
        //
        //        Ideally structs would allow static fields of any type. However
        //        this is a restriction and exemption that originally stems from 
        //        the way the Microsoft desktop CLR class loader works.
        // END: EARLIER COMMENT

        // edgesFrom tycon collects (tycon,tycon2) edges, for edges as described above.
        let edgesFrom (tycon:Tycon) =
            // Record edge (tycon,tycon2), only when tycon2 is an "initial" tycon.
            let insertEdgeToTycon tycon2 acc = 
                if ListSet.contains (===) tycon2 tycons && // note: only add if tycon2 is initial
                    not (List.exists (fun (tc,tc2) -> tc === tycon && tc2 === tycon2) acc)  // note: only add if (tycon,tycon2) not already an edge
                then
                    (tycon,tycon2)::acc
                else acc // note: all edges added are (tycon,_)
            let insertEdgeToType  ty     acc = 
                if isAppTy cenv.g ty then // guard against possible earlier failure
                    insertEdgeToTycon (tyconOfAppTy cenv.g ty) acc
                else
                    acc

            // collect edges from an a struct field (which is struct-contained in tycon)
            let rec accStructField (structTycon:Tycon) structTyInst (fspec:RecdField) (doneTypes,acc)  =
                let fieldTy = actualTyOfRecdFieldForTycon structTycon structTyInst fspec
                accStructFieldType structTycon structTyInst fspec fieldTy (doneTypes,acc)

            // collect edges from an a struct field (given the field type, which may be expanded if it is a type abbreviation)
            and accStructFieldType structTycon structTyInst fspec fieldTy (doneTypes,acc) =
                let fieldTy = stripTyparEqns fieldTy
                match fieldTy with
                | TType_app (tcref2 ,tinst2) when tcref2.IsStructOrEnumTycon ->
                    // The field is a struct.
                    // An edge (tycon,tycon2) should be recorded, unless it is the "static self-typed field" case.
                    let tycon2 = tcref2.Deref
                    let specialCaseStaticField =
                        // The special case of "static field S<'a> in struct S<'a>" is permitted. (so no (S,S) edge to be collected).
                        fspec.IsStatic &&
                        (structTycon === tycon2) && 
                        (structTyInst,tinst2) ||> List.lengthsEqAndForall2 (fun ty1 ty2 -> match tryDestTyparTy cenv.g ty1 with
                                                                                           | Some destTypar1 ->
                                                                                              match tryDestTyparTy cenv.g ty2 with
                                                                                              | Some destTypar2 -> typarEq destTypar1 destTypar2
                                                                                              | _ -> false
                                                                                           | _ -> false)
                    if specialCaseStaticField then
                        doneTypes,acc // no edge collected, no recursion.
                    else
                        let acc = insertEdgeToTycon tycon2 acc // collect edge (tycon,tycon2), if tycon2 is initial.
                        accStructInstanceFields fieldTy tycon2 tinst2 (doneTypes,acc) // recurse through struct field looking for more edges
                | TType_app (tcref2 ,tinst2) when tcref2.IsTypeAbbrev  ->
                    // The field is a type abbreviation. Expand and repeat.
                    accStructFieldType structTycon structTyInst fspec (reduceTyconRefAbbrev tcref2 tinst2) (doneTypes,acc)
                | _ ->
                    doneTypes,acc

            // collect edges from the fields of a given struct type.
            and accStructFields includeStaticFields ty (structTycon:Tycon) tinst (doneTypes,acc) =
                if List.exists (typeEquiv cenv.g ty) doneTypes then
                    // This type (type instance) has been seen before, so no need to collect the same edges again (and avoid loops!)
                    doneTypes,acc 
                else
                    // Only collect once from each type instance.
                    let doneTypes = ty :: doneTypes 
                    let fspecs = 
                        if structTycon.IsUnionTycon then 
                            [ for uc in structTycon.UnionCasesArray do 
                                 for c in uc.FieldTable.AllFieldsAsList  do
                                    yield c]
                        else
                            structTycon.AllFieldsAsList 
                    let fspecs = fspecs |> List.filter (fun fspec -> includeStaticFields || not fspec.IsStatic)
                    let doneTypes,acc = List.foldBack (accStructField structTycon tinst) fspecs (doneTypes,acc)
                    doneTypes,acc
            and accStructInstanceFields ty structTycon tinst (doneTypes,acc) = accStructFields false ty structTycon tinst (doneTypes,acc)
            and accStructAllFields      ty (structTycon: Tycon) tinst (doneTypes,acc) = accStructFields true  ty structTycon tinst (doneTypes,acc)

            let acc = []
            let acc = 
                if tycon.IsStructOrEnumTycon then
                    let tinst,ty = generalizeTyconRef (mkLocalTyconRef tycon)
                    let _,acc = accStructAllFields ty tycon tinst ([],acc)
                    acc
                else
                    acc

            let acc =
                // Note: only the nominal type counts 
                let super = superOfTycon cenv.g tycon
                insertEdgeToType super acc
            let acc =
                // Note: only the nominal type counts 
                List.foldBack insertEdgeToType tycon.ImmediateInterfaceTypesOfFSharpTycon acc
            acc
        let edges = (List.collect edgesFrom tycons)
        let graph = Graph<Tycon, Stamp> ((fun tc -> tc.Stamp), tycons, edges)
        graph.IterateCycles (fun path -> 
            let tycon = path.Head 
            // The thing is cyclic. Set the abbreviation and representation to be "None" to stop later VS crashes
            tycon.entity_tycon_abbrev <- None
            tycon.entity_tycon_repr <- TNoRepr
            errorR(Error(FSComp.SR.tcTypeDefinitionIsCyclicThroughInheritance(),tycon.Range)))
        

    // Interlude between Phase1D and Phase1E - Check and publish the explicit constraints. 
    let TcMutRecDefns_CheckExplicitConstraints cenv tpenv m checkCxs envMutRecPrelim withEnvs = 
            (envMutRecPrelim,withEnvs) ||> MutRecShapes.iterTyconsWithEnv (fun envForDecls (origInfo, tyconOpt) -> 
               match origInfo, tyconOpt with 
               | (typeDefCore,_,_), Some (tycon:Tycon) -> 
                let (MutRecDefnsPhase1DataForTycon(synTyconInfo,_,_,_,_,_)) = typeDefCore
                let (ComponentInfo(_,_, synTyconConstraints,_,_,_, _,_)) = synTyconInfo
                let envForTycon = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envForDecls
                let thisTyconRef = mkLocalTyconRef tycon
                let envForTycon = MakeInnerEnvForTyconRef cenv envForTycon thisTyconRef false 
                try TcTyparConstraints cenv NoNewTypars checkCxs ItemOccurence.UseInType envForTycon tpenv  synTyconConstraints |> ignore
                with e -> errorRecovery e m
               | _ -> ())


    let TcMutRecDefns_Phase1 mkLetInfo cenv envInitial parent inSig tpenv m scopem mutRecNSInfo (mutRecDefns:MutRecShapes<MutRecDefnsPhase1DataForTycon * 'MemberInfo, 'LetInfo, SynComponentInfo, _, _>) =

        // Phase1A - build Entity for type definitions, exception definitions and module definitions.
        // Also for abbreviations of any of these.  Augmentations are skipped in this phase.
        let withEntities = 
            mutRecDefns 
            |> MutRecShapes.mapWithParent 
                 (parent, TypeNamesInMutRecDecls mutRecDefns, envInitial)
                 // Build the initial entity for each module definition
                 (fun (innerParent, typeNames, envForDecls) compInfo decls -> 
                     TcTyconDefnCore_Phase1A_BuildInitialModule cenv envForDecls innerParent typeNames compInfo decls) 

                 // Build the initial Tycon for each type definition
                 (fun (innerParent, _, envForDecls) (typeDefCore,tyconMemberInfo) -> 
                     let (MutRecDefnsPhase1DataForTycon(_,_,_,_,_,isAtOriginalTyconDefn)) = typeDefCore
                     let tyconOpt = 
                         if isAtOriginalTyconDefn then 
                             Some (TcTyconDefnCore_Phase1A_BuildInitialTycon cenv envForDecls innerParent typeDefCore)
                         else 
                             None 
                     (typeDefCore, tyconMemberInfo, innerParent), tyconOpt) 

                 // Bundle up the data for each 'val', 'member' or 'let' definition (just package up the data, no processing yet)
                 (fun (innerParent, _, _) synBinds ->               
                    let containerInfo = ModuleOrNamespaceContainerInfo(match innerParent with Parent p -> p | _ -> failwith "unreachable")
                    mkLetInfo containerInfo synBinds)

        // Phase1AB - Publish modules
        let envTmp, withEnvs =  
            (envInitial, withEntities) ||> MutRecShapes.computeEnvs 
              (fun envAbove (MutRecDefnsPhase2DataForModule (mtypeAcc, mspec)) ->  
                  PublishModuleDefn cenv envAbove mspec 
                  MakeInnerEnvWithAcc envAbove mspec.Id mtypeAcc mspec.ModuleOrNamespaceType.ModuleOrNamespaceKind)
              (fun envAbove _ -> envAbove)

        // Updates the types of the modules to contain the contents so far, which now includes the nested modules and types
        MutRecBindingChecking.TcMutRecDefns_UpdateModuleContents mutRecNSInfo withEnvs 

        // Publish tycons
        (envTmp, withEnvs) ||> MutRecShapes.iterTyconsWithEnv
                (fun envAbove (_, tyconOpt)  -> 
                    tyconOpt |> Option.iter (fun tycon -> 
                        // recheck these in case type is a duplicate in a mutually recursive set
                        CheckForDuplicateConcreteType envAbove tycon.LogicalName tycon.Range
                        PublishTypeDefn cenv envAbove tycon))

        // Updates the types of the modules to contain the contents so far
        MutRecBindingChecking.TcMutRecDefns_UpdateModuleContents mutRecNSInfo withEnvs 

        // Phase1AB - Compute the active environments within each nested module.
        //
        // Add the types to the environment. This does not add the fields and union cases (because we haven't established them yet). 
        // We re-add them to the original environment later on. We don't report them to the Language Service yet as we don't know if 
        // they are well-formed (e.g. free of abbreviation cycles) 
        let envMutRecPrelim, withEnvs =  (envInitial, withEntities) ||> MutRecBindingChecking.TcMutRecDefns_ComputeEnvs snd (fun _ -> []) cenv false scopem m 

        // Phase 1B. Establish the kind of each type constructor 
        // Here we run InferTyconKind and record partial information about the kind of the type constructor. 
        // This means TyconObjModelKind is set, which means isSealedTy, isInterfaceTy etc. give accurate results. 
        let withAttrs = 
            (envMutRecPrelim, withEnvs) ||> MutRecShapes.mapTyconsWithEnv (fun envForDecls (origInfo,tyconOpt) -> 
                let res = 
                    match origInfo, tyconOpt with 
                    | (typeDefCore,_,_), Some tycon -> Some (tycon,TcTyconDefnCore_Phase1B_EstablishBasicKind cenv inSig envForDecls typeDefCore tycon)
                    | _ -> None
                origInfo, res)
            
        // Phase 1C. Establish the abbreviations (no constraint checking, because constraints not yet established)
        (envMutRecPrelim, withAttrs) ||>  MutRecShapes.iterTyconsWithEnv (fun envForDecls (origInfo,tyconAndAttrsOpt) -> 
            match origInfo, tyconAndAttrsOpt with 
            | (typeDefCore, _,_), Some (tycon,(attrs,_)) -> TcTyconDefnCore_Phase1C_Phase1E_EstablishAbbreviations cenv envForDecls inSig tpenv FirstPass typeDefCore tycon attrs
            | _ -> ()) 

        // Check for cyclic abbreviations. If this succeeds we can start reducing abbreviations safely.
        let tycons = withEntities |> MutRecShapes.collectTycons |> List.choose snd

        TcTyconDefnCore_CheckForCyclicAbbreviations tycons

        // Phase 1D. Establish the super type and interfaces  (no constraint checking, because constraints not yet established)     
        (envMutRecPrelim, withAttrs) |> TcTyconDefnCore_Phase1D_Phase1F_EstablishSuperTypesAndInterfaceTypes cenv tpenv inSig FirstPass 

        // Interlude between Phase1D and Phase1E - Add the interface and member declarations for 
        // hash/compare. Because this adds interfaces, this may let constraints 
        // be satisfied, so we have to do this prior to checking any constraints.
        //
        // First find all the field types in all the structrual types
        let tyconsWithStructuralTypes = 
            (envMutRecPrelim,withEnvs) 
            ||> MutRecShapes.mapTyconsWithEnv (fun envForDecls (origInfo, tyconOpt) -> 
                   match origInfo, tyconOpt with 
                   | (typeDefCore,_,_), Some tycon -> Some (tycon,GetStructuralElementsOfTyconDefn cenv envForDecls tpenv typeDefCore tycon)
                   | _ -> None) 
            |> MutRecShapes.collectTycons 
            |> List.choose id
        
        let scSet = TyconConstraintInference.InferSetOfTyconsSupportingComparable cenv envMutRecPrelim.DisplayEnv tyconsWithStructuralTypes
        let seSet = TyconConstraintInference.InferSetOfTyconsSupportingEquatable cenv envMutRecPrelim.DisplayEnv tyconsWithStructuralTypes

        (envMutRecPrelim,withEnvs) 
        ||> MutRecShapes.iterTyconsWithEnv (fun envForDecls (_, tyconOpt) -> 
               tyconOpt |> Option.iter (AddAugmentationDeclarations.AddGenericHashAndComparisonDeclarations cenv envForDecls scSet seSet))

        TcMutRecDefns_CheckExplicitConstraints cenv tpenv m NoCheckCxs envMutRecPrelim withEnvs 

        // No inferred constraints allowed on declared typars 
        (envMutRecPrelim,withEnvs) ||> MutRecShapes.iterTyconsWithEnv (fun envForDecls (_, tyconOpt) -> 
            tyconOpt |> Option.iter (fun tycon -> tycon.Typars(m) |> List.iter (SetTyparRigid cenv.g envForDecls.DisplayEnv m)))
        
        // Phase1E. OK, now recheck the abbreviations, super/interface and explicit constraints types (this time checking constraints)
        (envMutRecPrelim, withAttrs) ||>  MutRecShapes.iterTyconsWithEnv (fun envForDecls (origInfo,tyconAndAttrsOpt) -> 
            match origInfo, tyconAndAttrsOpt with 
            | (typeDefCore, _, _), Some (tycon,(attrs,_)) -> TcTyconDefnCore_Phase1C_Phase1E_EstablishAbbreviations cenv envForDecls inSig tpenv SecondPass typeDefCore tycon attrs
            | _ -> ()) 

        // Phase1F. Establish inheritance hierarchy
        (envMutRecPrelim, withAttrs) |> TcTyconDefnCore_Phase1D_Phase1F_EstablishSuperTypesAndInterfaceTypes cenv tpenv inSig SecondPass 

        TcMutRecDefns_CheckExplicitConstraints cenv tpenv m CheckCxs envMutRecPrelim withEnvs 

        // Add exception definitions to the environments, which are used for checking exception abbreviations in representations
        let envMutRecPrelim, withAttrs =  
            (envMutRecPrelim, withAttrs) 
            ||> MutRecShapes.extendEnvs (fun envForDecls decls -> 
                    let tycons =  decls |> List.choose (function MutRecShape.Tycon (_, Some (tycon,_)) -> Some tycon | _ -> None) 
                    let exns =  tycons |> List.filter (fun tycon -> tycon.IsExceptionDecl) 
                    let envForDecls = (envForDecls, exns) ||> List.fold (AddLocalExnDefnAndReport cenv.tcSink scopem)
                    envForDecls)

        // Phase1G. Establish inheritance hierarchy
        // Now all the type parameters, abbreviations, constraints and kind information is established.
        // Now do the representations. Each baseValOpt is a residue from the representation which is potentially available when
        // checking the members.
        let withBaseValsAndSafeInitInfos = 
            (envMutRecPrelim,withAttrs) ||> MutRecShapes.mapTyconsWithEnv (fun envForDecls (origInfo,tyconAndAttrsOpt) -> 
                let info = 
                    match origInfo, tyconAndAttrsOpt with 
                    | (typeDefCore,_,_), Some (tycon,(attrs,_)) -> TcTyconDefnCore_Phase1G_EstablishRepresentation cenv envForDecls tpenv inSig typeDefCore tycon attrs
                    | _ -> None, NoSafeInitInfo 
                let tyconOpt, fixupFinalAttrs = 
                    match tyconAndAttrsOpt with
                    | None -> None, (fun () -> ())
                    | Some (tycon, (_prelimAttrs, getFinalAttrs)) -> Some tycon, (fun () -> tycon.entity_attribs <- getFinalAttrs())

                (origInfo, tyconOpt, fixupFinalAttrs, info))
                
        // Now check for cyclic structs and inheritance. It's possible these should be checked as separate conditions. 
        // REVIEW: checking for cyclic inheritance is happening too late. See note above.
        TcTyconDefnCore_CheckForCyclicStructsAndInheritance cenv tycons


        (tycons, envMutRecPrelim, withBaseValsAndSafeInitInfos)


/// Bind declarations in implementation and signature files
module TcDeclarations = 

    /// Given a type definition, compute whether its members form an extension of an existing type, and if so if it is an 
    /// intrinsic or extrinsic extension
    let private ComputeTyconDeclKind tyconOpt isAtOriginalTyconDefn cenv envForDecls inSig m (typars:SynTyparDecl list) cs longPath = 
        let ad = envForDecls.eAccessRights
        
        let tcref = 
          match tyconOpt with
          | Some tycon when isAtOriginalTyconDefn -> 

            // This records a name resolution of the type at the location
            let resInfo = TypeNameResolutionStaticArgsInfo.FromTyArgs typars.Length
            ResolveTypeLongIdent cenv.tcSink cenv.nameResolver ItemOccurence.Binding OpenQualified envForDecls.eNameResEnv ad longPath resInfo PermitDirectReferenceToGeneratedType.No 
               |> ignore

            mkLocalTyconRef tycon

          | _ ->
            let resInfo = TypeNameResolutionStaticArgsInfo.FromTyArgs typars.Length
            match ResolveTypeLongIdent cenv.tcSink cenv.nameResolver ItemOccurence.Binding OpenQualified envForDecls.eNameResEnv ad longPath resInfo PermitDirectReferenceToGeneratedType.No with
            | Result res -> res
            | res when inSig && longPath.Length = 1 ->
                errorR(Deprecated(FSComp.SR.tcReservedSyntaxForAugmentation(),m))
                ForceRaise res
            | res -> ForceRaise res            

        let isInterfaceOrDelegateOrEnum = 
            tcref.Deref.IsFSharpInterfaceTycon || 
            tcref.Deref.IsFSharpDelegateTycon ||
            tcref.Deref.IsFSharpEnumTycon

        let reqTypars = tcref.Typars(m)

        // Member definitions are intrinsic (added directly to the type) if:
        // a) For interfaces, only if it is in the original defn.
        //    Augmentations to interfaces via partial type defns will always be extensions, e.g. extension members on interfaces.
        // b) For other types, if the type is isInSameModuleOrNamespace
        let declKind,typars = 
          if isAtOriginalTyconDefn then 
              ModuleOrMemberBinding, reqTypars

          else
            let isInSameModuleOrNamespace = 
                 match envForDecls.eModuleOrNamespaceTypeAccumulator.Value.TypesByMangledName.TryFind(tcref.LogicalName) with 
                  | Some tycon -> (tyconOrder.Compare(tcref.Deref,tycon) = 0)
                  | None -> 
                        //false
                        // There is a special case we allow when compiling FSharp.Core.dll which permits interface implementations across namespace fragments
                        (cenv.g.compilingFslib && tcref.LogicalName.StartsWith("Tuple`"))
        
            if isInSameModuleOrNamespace && not isInterfaceOrDelegateOrEnum then 
                IntrinsicExtensionBinding, reqTypars
            else 
                if isInSameModuleOrNamespace && isInterfaceOrDelegateOrEnum then 
                    errorR(Error(FSComp.SR.tcMembersThatExtendInterfaceMustBePlacedInSeparateModule(),tcref.Range))
                let nReqTypars = reqTypars.Length
                if nReqTypars <> typars.Length then 
                    // not recoverable
                    error(Error(FSComp.SR.tcDeclaredTypeParametersForExtensionDoNotMatchOriginal(tcref.DisplayNameWithStaticParametersAndUnderscoreTypars), m))

                let declaredTypars = TcTyparDecls cenv envForDecls typars
                let envForTycon = AddDeclaredTypars CheckForDuplicateTypars declaredTypars envForDecls
                let _tpenv = TcTyparConstraints cenv NoNewTypars CheckCxs ItemOccurence.UseInType envForTycon emptyUnscopedTyparEnv cs
                declaredTypars |> List.iter (SetTyparRigid cenv.g envForDecls.DisplayEnv m)
                if not (typarsAEquiv cenv.g TypeEquivEnv.Empty reqTypars declaredTypars) then 
                    errorR(Error(FSComp.SR.tcDeclaredTypeParametersForExtensionDoNotMatchOriginal(tcref.DisplayNameWithStaticParametersAndUnderscoreTypars), m))
                ExtrinsicExtensionBinding, declaredTypars


        declKind, tcref, typars


    let private isAugmentationTyconDefnRepr = function (SynTypeDefnSimpleRepr.General(TyconAugmentation,_,_,_,_,_,_,_)) -> true | _ -> false
    let private isAutoProperty  = function SynMemberDefn.AutoProperty _ -> true | _ -> false
    let private isMember          = function SynMemberDefn.Member _   -> true | _ -> false
    let private isImplicitCtor    = function SynMemberDefn.ImplicitCtor _    -> true | _ -> false
    let private isImplicitInherit = function SynMemberDefn.ImplicitInherit _ -> true | _ -> false
    let private isAbstractSlot    = function SynMemberDefn.AbstractSlot _          -> true | _ -> false
    let private isInterface       = function SynMemberDefn.Interface _        -> true | _ -> false
    let private isInherit         = function SynMemberDefn.Inherit _          -> true | _ -> false
    let private isField           = function SynMemberDefn.ValField (_,_)   -> true | _ -> false
    let private isTycon           = function SynMemberDefn.NestedType _            -> true | _ -> false

    let private allFalse ps x = List.fold (fun acc p -> acc && not (p x)) true ps

    /// Check the ordering on the bindings and members in a class construction
    // Accepted forms:
    //
    // Implicit Construction:
    //   implicit_ctor
    //   optional implicit_inherit
    //   multiple bindings
    //   multiple member-binding(includes-overrides) or abstract-slot-declaration or interface-bindings
    //
    // Classic construction:
    //   multiple (binding or slotsig or field or interface or inherit).
    //   i.e. not local-bindings, implicit ctor or implicit inherit (or tycon?).
    //   atMostOne inherit.
    let private CheckMembersForm ds = 
        match ds with
        | d::ds when isImplicitCtor d ->
            // Implicit construction 
            let ds = 
                match ds with
                | d::ds when isImplicitInherit d -> ds  // skip inherit call if it comes next 
                | _ -> ds

            // Skip over 'let' and 'do' bindings
            let _,ds = ds |> List.takeUntil (function SynMemberDefn.LetBindings _ -> false | _ -> true) 

            // Skip over 'let' and 'do' bindings
            let _,ds = ds |> List.takeUntil (allFalse [isMember;isAbstractSlot;isInterface;isAutoProperty]) 

            match ds with
             | SynMemberDefn.Member (_,m)       :: _ -> errorR(InternalError("List.takeUntil is wrong, have binding",m))
             | SynMemberDefn.AbstractSlot (_,_,m)            :: _ -> errorR(InternalError("List.takeUntil is wrong, have slotsig",m))
             | SynMemberDefn.Interface (_,_,m)          :: _ -> errorR(InternalError("List.takeUntil is wrong, have interface",m))
             | SynMemberDefn.ImplicitCtor (_,_,_,_,m)  :: _ -> errorR(InternalError("implicit class construction with two implicit constructions",m))
             | SynMemberDefn.AutoProperty (_,_,_,_,_,_,_,_,_,_,m) :: _ -> errorR(InternalError("List.takeUntil is wrong, have auto property",m))
             | SynMemberDefn.ImplicitInherit (_,_,_,m) :: _ -> errorR(Error(FSComp.SR.tcTypeDefinitionsWithImplicitConstructionMustHaveOneInherit(),m))
             | SynMemberDefn.LetBindings (_,_,_,m)     :: _ -> errorR(Error(FSComp.SR.tcTypeDefinitionsWithImplicitConstructionMustHaveLocalBindingsBeforeMembers(),m))
             | SynMemberDefn.Inherit (_,_,m)            :: _ -> errorR(Error(FSComp.SR.tcInheritDeclarationMissingArguments(),m))
             | SynMemberDefn.NestedType (_,_,m)              :: _ -> errorR(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m))
             | _ -> ()
        | ds ->
            // Classic class construction 
            let _,ds = List.takeUntil (allFalse [isMember;isAbstractSlot;isInterface;isInherit;isField;isTycon]) ds
            match ds with
             | SynMemberDefn.Member (_,m)       :: _ -> errorR(InternalError("CheckMembersForm: List.takeUntil is wrong",m))
             | SynMemberDefn.ImplicitCtor (_,_,_,_,m)  :: _ -> errorR(InternalError("CheckMembersForm: implicit ctor line should be first",m))
             | SynMemberDefn.ImplicitInherit (_,_,_,m) :: _ -> errorR(Error(FSComp.SR.tcInheritConstructionCallNotPartOfImplicitSequence(),m))
             | SynMemberDefn.AutoProperty(_,_,_,_,_,_,_,_,_,_,m) :: _  -> errorR(Error(FSComp.SR.tcAutoPropertyRequiresImplicitConstructionSequence(),m))
             | SynMemberDefn.LetBindings (_,false,_,m) :: _ -> errorR(Error(FSComp.SR.tcLetAndDoRequiresImplicitConstructionSequence(),m))
             | SynMemberDefn.AbstractSlot (_,_,m)            :: _ 
             | SynMemberDefn.Interface (_,_,m)          :: _ 
             | SynMemberDefn.Inherit (_,_,m)            :: _ 
             | SynMemberDefn.ValField (_,m)                :: _ 
             | SynMemberDefn.NestedType (_,_,m)              :: _ -> errorR(InternalError("CheckMembersForm: List.takeUntil is wrong",m))
             | _ -> ()
                     

    /// Separates the definition into core (shape) and body.
    ///
    /// core = synTyconInfo,simpleRepr,interfaceTypes
    ///        where simpleRepr can contain inherit type, declared fields and virtual slots.
    /// body = members
    ///        where members contain methods/overrides, also implicit ctor, inheritCall and local definitions.
    let rec private SplitTyconDefn (TypeDefn(synTyconInfo,trepr,extraMembers,_)) = 
        let implements1 = List.choose (function SynMemberDefn.Interface (ty,_,_) -> Some(ty,ty.Range) | _ -> None) extraMembers
        match trepr with
        | SynTypeDefnRepr.ObjectModel(kind,cspec,m) ->
            CheckMembersForm cspec
            let fields      = cspec |> List.choose (function SynMemberDefn.ValField (f,_) -> Some(f) | _ -> None)
            let implements2 = cspec |> List.choose (function SynMemberDefn.Interface (ty,_,_) -> Some(ty,ty.Range) | _ -> None)
            let inherits    = cspec |> List.choose (function 
                                                          | SynMemberDefn.Inherit          (ty,idOpt,m)     -> Some(ty,m,idOpt)
                                                          | SynMemberDefn.ImplicitInherit (ty,_,idOpt,m) -> Some(ty,m,idOpt)
                                                          | _ -> None)
            //let nestedTycons = cspec |> List.choose (function SynMemberDefn.NestedType (x,_,_) -> Some(x) | _ -> None)
            let slotsigs    = cspec |> List.choose (function SynMemberDefn.AbstractSlot (x,y,_) -> Some(x,y) | _ -> None)
           
            let members = 
                let membersIncludingAutoProps     = 
                    cspec |> List.filter (fun memb -> 
                      match memb with 
                      | SynMemberDefn.Interface _
                      | SynMemberDefn.Member _ 
                      | SynMemberDefn.LetBindings _
                      | SynMemberDefn.ImplicitCtor _
                      | SynMemberDefn.AutoProperty _ 
                      | SynMemberDefn.Open _
                      | SynMemberDefn.ImplicitInherit _ -> true
                      | SynMemberDefn.NestedType  (_,_,m)  -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m)); false
                      // covered above 
                      | SynMemberDefn.ValField _   
                      | SynMemberDefn.Inherit _ 
                      | SynMemberDefn.AbstractSlot _ -> false)

                // Convert autoproperties to let bindings in the pre-list
                let rec preAutoProps memb =
                    match memb with 
                    | SynMemberDefn.AutoProperty (attribs, isStatic, id, tyOpt, propKind, _, xmlDoc, _access, synExpr, _mGetSet, mWholeAutoProp) -> 
                        // Only the keep the field-targeted attributes
                        let attribs = attribs |> List.filter (fun a -> match a.Target with Some t when t.idText = "field" -> true | _ -> false)
                        let mLetPortion = synExpr.Range
                        let fldId = ident (CompilerGeneratedName id.idText, mLetPortion)
                        let headPat = SynPat.LongIdent (LongIdentWithDots([fldId],[]),None,Some noInferredTypars, SynConstructorArgs.Pats [],None,mLetPortion)
                        let retInfo = match tyOpt with None -> None | Some ty -> Some (SynReturnInfo((ty,SynInfo.unnamedRetVal),ty.Range))
                        let isMutable = 
                            match propKind with 
                            | MemberKind.PropertySet 
                            | MemberKind.PropertyGetSet -> true 
                            | _ -> false
                        let binding = mkSynBinding (xmlDoc,headPat) (None,false,isMutable,mLetPortion,NoSequencePointAtInvisibleBinding,retInfo,synExpr,synExpr.Range,[],attribs,None)

                        [(SynMemberDefn.LetBindings ([binding], isStatic, false, mWholeAutoProp))]

                    | SynMemberDefn.Interface (_, Some membs, _) -> membs |> List.collect preAutoProps
                    | SynMemberDefn.LetBindings _
                    | SynMemberDefn.ImplicitCtor _ 
                    | SynMemberDefn.Open _
                    | SynMemberDefn.ImplicitInherit _ -> [memb]
                    | _ -> []

                // Convert autoproperties to member bindings in the post-list
                let rec postAutoProps memb =
                    match memb with 
                    | SynMemberDefn.AutoProperty (attribs,isStatic,id,tyOpt,propKind,memberFlags,xmlDoc,access,_synExpr,mGetSetOpt,_mWholeAutoProp)  ->
                        let mMemberPortion = id.idRange
                        // Only the keep the non-field-targeted attributes
                        let attribs = attribs |> List.filter (fun a -> match a.Target with Some t when t.idText = "field" -> false | _ -> true)
                        let fldId = ident (CompilerGeneratedName id.idText, mMemberPortion)
                        let headPatIds = if isStatic then [id] else [ident ("__",mMemberPortion);id]
                        let headPat = SynPat.LongIdent (LongIdentWithDots(headPatIds,[]),None,Some noInferredTypars, SynConstructorArgs.Pats [],None,mMemberPortion)

                        match propKind,mGetSetOpt with 
                        | MemberKind.PropertySet,Some m -> errorR(Error(FSComp.SR.parsMutableOnAutoPropertyShouldBeGetSetNotJustSet(),m))
                        | _ -> ()
       
                        [ 
                            match propKind with 
                            | MemberKind.Member
                            | MemberKind.PropertyGet 
                            | MemberKind.PropertyGetSet -> 
                                let getter = 
                                    let rhsExpr = SynExpr.Ident fldId
                                    let retInfo = match tyOpt with None -> None | Some ty -> Some (SynReturnInfo((ty,SynInfo.unnamedRetVal),ty.Range))
                                    let binding = mkSynBinding (xmlDoc,headPat) (access,false,false,mMemberPortion,NoSequencePointAtInvisibleBinding,retInfo,rhsExpr,rhsExpr.Range,[],attribs,Some (memberFlags MemberKind.Member))
                                    SynMemberDefn.Member (binding,mMemberPortion) 
                                yield getter
                            | _ -> ()

                            match propKind with 
                            | MemberKind.PropertySet 
                            | MemberKind.PropertyGetSet -> 
                                let setter = 
                                    let vId = ident("v",mMemberPortion)
                                    let headPat = SynPat.LongIdent (LongIdentWithDots(headPatIds,[]),None,Some noInferredTypars, SynConstructorArgs.Pats [mkSynPatVar None vId],None,mMemberPortion)
                                    let rhsExpr = mkSynAssign (SynExpr.Ident fldId) (SynExpr.Ident vId)
                                    //let retInfo = match tyOpt with None -> None | Some ty -> Some (SynReturnInfo((ty,SynInfo.unnamedRetVal),ty.Range))
                                    let binding = mkSynBinding (xmlDoc,headPat) (access,false,false,mMemberPortion,NoSequencePointAtInvisibleBinding,None,rhsExpr,rhsExpr.Range,[],[],Some (memberFlags MemberKind.PropertySet))
                                    SynMemberDefn.Member (binding,mMemberPortion) 
                                yield setter 
                            | _ -> ()]
                    | SynMemberDefn.Interface (ty, Some membs, m) -> 
                        let membs' = membs |> List.collect postAutoProps
                        [SynMemberDefn.Interface (ty, Some membs', m)]
                    | SynMemberDefn.LetBindings _
                    | SynMemberDefn.ImplicitCtor _ 
                    | SynMemberDefn.Open _
                    | SynMemberDefn.ImplicitInherit _ -> []
                    | _ -> [memb]

                let preMembers = membersIncludingAutoProps |> List.collect preAutoProps
                let postMembers = membersIncludingAutoProps |> List.collect postAutoProps

                preMembers @ postMembers

            let isConcrete = 
                members |> List.exists (function 
                    | SynMemberDefn.Member(Binding(_,_,_,_,_,_,SynValData(Some memberFlags,_,_),_,_,_,_,_),_) -> not memberFlags.IsDispatchSlot 
                    | SynMemberDefn.Interface (_,defOpt,_) -> Option.isSome defOpt
                    | SynMemberDefn.LetBindings _ -> true
                    | SynMemberDefn.ImplicitCtor _ -> true
                    | SynMemberDefn.ImplicitInherit _ -> true
                    | _ -> false)

            let isIncrClass = 
                members |> List.exists (function 
                    | SynMemberDefn.ImplicitCtor _ -> true
                    | _ -> false)

            let hasSelfReferentialCtor = 
                members |> List.exists (function 
                    | SynMemberDefn.ImplicitCtor (_,_,_,thisIdOpt,_) 
                    | SynMemberDefn.Member(Binding(_,_,_,_,_,_,SynValData(_,_,thisIdOpt),_,_,_,_,_),_) -> thisIdOpt.IsSome
                    | _ -> false)

            let implicitCtorSynPats = 
                members |> List.tryPick (function 
                    | SynMemberDefn.ImplicitCtor (_,_,spats,_, _)  -> Some spats
                    | _ -> None)

            // An ugly bit of code to pre-determine if a type has a nullary constructor, prior to establishing the 
            // members of the type
            let preEstablishedHasDefaultCtor = 
                members |> List.exists (function 
                    | SynMemberDefn.Member(Binding(_,_,_,_,_,_,SynValData(Some memberFlags,_,_),SynPatForConstructorDecl(SynPatForNullaryArgs),_,_,_,_),_) -> 
                        memberFlags.MemberKind=MemberKind.Constructor 
                    | SynMemberDefn.ImplicitCtor (_,_,spats,_, _) -> isNil spats
                    | _ -> false)
            let repr = SynTypeDefnSimpleRepr.General(kind,inherits,slotsigs,fields,isConcrete,isIncrClass,implicitCtorSynPats,m)
            let isAtOriginalTyconDefn = not (isAugmentationTyconDefnRepr repr)
            let core = MutRecDefnsPhase1DataForTycon(synTyconInfo, repr, implements2@implements1, preEstablishedHasDefaultCtor, hasSelfReferentialCtor, isAtOriginalTyconDefn)

            core, members @ extraMembers

        | SynTypeDefnRepr.Simple(repr,_) -> 
            let members = []
            let isAtOriginalTyconDefn = true
            let core = MutRecDefnsPhase1DataForTycon(synTyconInfo, repr, implements1, false, false, isAtOriginalTyconDefn)
            core, members @ extraMembers

        | SynTypeDefnRepr.Exception(r) -> 
            let isAtOriginalTyconDefn = true
            let core = MutRecDefnsPhase1DataForTycon(synTyconInfo, SynTypeDefnSimpleRepr.Exception r, implements1, false, false, isAtOriginalTyconDefn)
            core, extraMembers

    //-------------------------------------------------------------------------

    /// Bind a collection of mutually recursive definitions in an implementation file
    let TcMutRecDefinitions cenv envInitial parent tpenv m scopem mutRecNSInfo (mutRecDefns: MutRecDefnsInitialData) =

        // Split the definitions into "core representations" and "members".  The code to process core representations
        // is shared between processing of signature files and implementation files.
        let mutRecDefnsAfterSplit = mutRecDefns |> MutRecShapes.mapTycons SplitTyconDefn

        // Create the entities for each module and type definition, and process the core representation of each type definition.
        let tycons, envMutRecPrelim, mutRecDefnsAfterCore = 
            EstablishTypeDefinitionCores.TcMutRecDefns_Phase1 
               (fun containerInfo synBinds -> [ for synBind in synBinds -> RecDefnBindingInfo(containerInfo,NoNewSlots,ModuleOrMemberBinding,synBind) ])
               cenv envInitial parent false tpenv m scopem mutRecNSInfo mutRecDefnsAfterSplit

        // Package up the phase two information for processing members.
        let mutRecDefnsAfterPrep = 
            (envMutRecPrelim,mutRecDefnsAfterCore)
            ||> MutRecShapes.mapTyconsWithEnv (fun envForDecls ((typeDefnCore, members, innerParent), tyconOpt, fixupFinalAttrs, (baseValOpt, safeInitInfo)) -> 
                let (MutRecDefnsPhase1DataForTycon(synTyconInfo,_,_,_,_,isAtOriginalTyconDefn)) = typeDefnCore
                let tyDeclRange = synTyconInfo.Range
                let (ComponentInfo(_,typars, cs,longPath, _, _, _,_)) = synTyconInfo
                let declKind, tcref, declaredTyconTypars = ComputeTyconDeclKind tyconOpt isAtOriginalTyconDefn cenv envForDecls false tyDeclRange typars cs longPath
                let newslotsOK = (if isAtOriginalTyconDefn && tcref.IsFSharpObjectModelTycon then NewSlotsOK else NoNewSlots) 
                if not (isNil members) && tcref.IsTypeAbbrev then 
                    errorR(Error(FSComp.SR.tcTypeAbbreviationsCannotHaveAugmentations(), tyDeclRange))
                MutRecDefnsPhase2DataForTycon(tyconOpt, innerParent, declKind, tcref, baseValOpt, safeInitInfo, declaredTyconTypars, members, tyDeclRange, newslotsOK, fixupFinalAttrs))

        // By now we've established the full contents of type definitions apart from their
        // members and any fields determined by implicit construction.  We know the kinds and 
        // representations of types and have established them as valid.
        //
        // We now reconstruct the active environments all over again - this will add the union cases and fields. 
        //
        // Note: This environment reconstruction doesn't seem necessary. We're about to create Val's for all members,
        // which does require type checking, but no more information than is already available.
        let envMutRecPrelimWithReprs, withEnvs =  
            (envInitial, MutRecShapes.dropEnvs mutRecDefnsAfterPrep) 
                ||> MutRecBindingChecking.TcMutRecDefns_ComputeEnvs 
                       (fun (MutRecDefnsPhase2DataForTycon(tyconOpt, _, _, _, _, _, _, _, _, _, _)) -> tyconOpt)  
                       (fun _binds ->  [  (* no values are available yet *) ]) 
                       cenv true scopem m 

        // Check the members and decide on representations for types with implicit constructors.
        let withBindings,envFinal = TcMutRecDefns_Phase2 cenv envInitial m scopem mutRecNSInfo  envMutRecPrelimWithReprs withEnvs

        // Generate the hash/compare/equality bindings for all tycons.
        //
        // Note: generating these bindings must come after generating the members, since some in the case of structs some fields
        // may be added by generating the implicit construction syntax 
        let withExtraBindings = 
            (envFinal,withBindings) ||> MutRecShapes.expandTyconsWithEnv (fun envForDecls (tyconOpt, _) -> 
                match tyconOpt with 
                | None -> [],[] 
                | Some tycon -> 
                    // We put the hash/compare bindings before the type definitions and the
                    // equality bindings after because tha is the order they've always been generated
                    // in, and there are code generation tests to check that.
                    let binds = AddAugmentationDeclarations.AddGenericHashAndComparisonBindings cenv tycon 
                    let binds3 = AddAugmentationDeclarations.AddGenericEqualityBindings cenv envForDecls tycon
                    binds, binds3)

        // Check for cyclic structs and inheritance all over again, since we may have added some fields to the struct when generating the implicit construction syntax 
        EstablishTypeDefinitionCores.TcTyconDefnCore_CheckForCyclicStructsAndInheritance cenv tycons

        withExtraBindings,envFinal  


    //-------------------------------------------------------------------------

    /// Separates the signature declaration into core (shape) and body.
    let rec private SplitTyconSignature (TypeDefnSig(synTyconInfo,trepr,extraMembers,_)) = 

        let implements1 = 
            extraMembers |> List.choose (function SynMemberSig.Interface (f,m) -> Some(f,m) | _ -> None) 

        match trepr with
        | SynTypeDefnSigRepr.ObjectModel(kind,cspec,m) -> 
            let fields      = cspec |> List.choose (function SynMemberSig.ValField (f,_) -> Some(f) | _ -> None)
            let implements2 = cspec |> List.choose (function SynMemberSig.Interface (ty,m) -> Some(ty,m) | _ -> None)
            let inherits    = cspec |> List.choose (function SynMemberSig.Inherit (ty,_) -> Some(ty,m,None) | _ -> None)
            //let nestedTycons = cspec |> List.choose (function SynMemberSig.NestedType (x,_) -> Some(x) | _ -> None)
            let slotsigs    = cspec |> List.choose (function SynMemberSig.Member (v,fl,_) when fl.IsDispatchSlot -> Some(v,fl) | _ -> None)
            let members     = cspec |> List.filter (function   
                                                          | SynMemberSig.Interface _ -> true
                                                          | SynMemberSig.Member (_,memberFlags,_) when not memberFlags.IsDispatchSlot -> true
                                                          | SynMemberSig.NestedType  (_,m) -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m)); false
                                                          | _ -> false)
            let isConcrete = 
                members |> List.exists (function 
                    | SynMemberSig.Member (_,memberFlags,_) -> memberFlags.MemberKind=MemberKind.Constructor 
                    | _ -> false)

            // An ugly bit of code to pre-determine if a type has a nullary constructor, prior to establishing the 
            // members of the type
            let preEstablishedHasDefaultCtor = 
                members |> List.exists (function 
                    | SynMemberSig.Member (valSpfn,memberFlags,_) -> 
                        memberFlags.MemberKind=MemberKind.Constructor  && 
                        // REVIEW: This is a syntactic approximation
                        (match valSpfn.SynType, valSpfn.SynInfo.ArgInfos with 
                         | SynType.Fun (SynType.LongIdent (LongIdentWithDots([id],_)), _, _), [[_]] when id.idText = "unit" ->  true
                         | _ -> false) 
                    | _ -> false) 

            let hasSelfReferentialCtor = false
            
            let repr = SynTypeDefnSimpleRepr.General(kind,inherits,slotsigs,fields,isConcrete,false,None,m)
            let isAtOriginalTyconDefn = true
            let tyconCore = MutRecDefnsPhase1DataForTycon (synTyconInfo, repr, implements2@implements1, preEstablishedHasDefaultCtor, hasSelfReferentialCtor, isAtOriginalTyconDefn)

            tyconCore, (synTyconInfo,members@extraMembers)

        // 'type X with ...' in a signature is always interpreted as an extrinsic extension.
        // Representation-hidden types with members and interfaces are written 'type X = ...' 
        | SynTypeDefnSigRepr.Simple((SynTypeDefnSimpleRepr.None _ as r),_) when not (isNil extraMembers) -> 
            let isAtOriginalTyconDefn = false
            let tyconCore = MutRecDefnsPhase1DataForTycon (synTyconInfo, r, implements1, false, false, isAtOriginalTyconDefn)
            tyconCore,  (synTyconInfo,extraMembers)

        | SynTypeDefnSigRepr.Exception(r) -> 
            let isAtOriginalTyconDefn = true
            let core = MutRecDefnsPhase1DataForTycon(synTyconInfo,SynTypeDefnSimpleRepr.Exception r,implements1,false,false,isAtOriginalTyconDefn)
            core, (synTyconInfo,extraMembers)

        | SynTypeDefnSigRepr.Simple(r,_) -> 
            let isAtOriginalTyconDefn = true
            let tyconCore = MutRecDefnsPhase1DataForTycon (synTyconInfo, r, implements1, false, false, isAtOriginalTyconDefn)
            tyconCore, (synTyconInfo,extraMembers) 


    let private TcMutRecSignatureDecls_Phase2 cenv scopem envMutRec mutRecDefns =
        (envMutRec,mutRecDefns) ||> MutRecShapes.mapWithEnv 
            // Do this for the members in each 'type' declaration 
            (fun envForDecls ((tyconCore, (synTyconInfo,members), innerParent), tyconOpt, _fixupFinalAttrs, _) -> 
                let tpenv = emptyUnscopedTyparEnv
                let (MutRecDefnsPhase1DataForTycon (_, _, _, _, _, isAtOriginalTyconDefn)) = tyconCore
                let (ComponentInfo(_,typars,cs,longPath, _, _, _,m)) = synTyconInfo
                let declKind,tcref,declaredTyconTypars = ComputeTyconDeclKind tyconOpt isAtOriginalTyconDefn cenv envForDecls true m typars cs longPath

                let envForTycon = AddDeclaredTypars CheckForDuplicateTypars declaredTyconTypars envForDecls
                let envForTycon = MakeInnerEnvForTyconRef cenv envForTycon tcref (declKind = ExtrinsicExtensionBinding) 

                TcTyconMemberSpecs cenv envForTycon (TyconContainerInfo(innerParent, tcref, declaredTyconTypars, NoSafeInitInfo)) declKind tpenv members)
            
            // Do this for each 'val' declaration in a module
            (fun envForDecls (containerInfo, valSpec) -> 
                let tpenv = emptyUnscopedTyparEnv
                let idvs,_ = TcAndPublishValSpec (cenv,envForDecls,containerInfo,ModuleOrMemberBinding,None,tpenv,valSpec)
                let env = List.foldBack (AddLocalVal cenv.tcSink scopem) idvs envForDecls
                env)


    /// Bind a collection of mutually recursive declarations in a signature file
    let TcMutRecSignatureDecls cenv envInitial parent tpenv m scopem mutRecNSInfo (mutRecSigs:MutRecSigsInitialData) =
        let mutRecSigsAfterSplit = mutRecSigs |> MutRecShapes.mapTycons SplitTyconSignature
        let _tycons, envMutRec, mutRecDefnsAfterCore = EstablishTypeDefinitionCores.TcMutRecDefns_Phase1 (fun containerInfo valDecl -> (containerInfo, valDecl)) cenv envInitial parent true tpenv m scopem mutRecNSInfo mutRecSigsAfterSplit

        // Updates the types of the modules to contain the contents so far, which now includes values and members
        MutRecBindingChecking.TcMutRecDefns_UpdateModuleContents mutRecNSInfo mutRecDefnsAfterCore

        // By now we've established the full contents of type definitions apart from their
        // members and any fields determined by implicit construction.  We know the kinds and 
        // representations of types and have established them as valid.
        //
        // We now reconstruct the active environments all over again - this will add the union cases and fields. 
        //
        // Note: This environment reconstruction doesn't seem necessary. We're about to create Val's for all members,
        // which does require type checking, but no more information than is already available.
        let envMutRecPrelimWithReprs, withEnvs =  
            (envInitial, MutRecShapes.dropEnvs mutRecDefnsAfterCore) 
                ||> MutRecBindingChecking.TcMutRecDefns_ComputeEnvs 
                       (fun (_, tyconOpt, _, _) -> tyconOpt)  
                       (fun _binds ->  [  (* no values are available yet *) ]) 
                       cenv true scopem m 

        let _ = TcMutRecSignatureDecls_Phase2 cenv scopem envMutRecPrelimWithReprs withEnvs
        envMutRec

//-------------------------------------------------------------------------
// Bind module types
//------------------------------------------------------------------------- 

let rec TcSignatureElementNonMutRec cenv parent typeNames endm (env: TcEnv) synSigDecl : Eventually<TcEnv> =
  eventually {
    try 
        match synSigDecl with 
        | SynModuleSigDecl.Exception (edef,m) ->
            let scopem = unionRanges m.EndRange endm
            let _,_,_,env = TcExceptionDeclarations.TcExnSignature cenv env parent emptyUnscopedTyparEnv (edef,scopem)
            return env

        | SynModuleSigDecl.Types (typeSpecs,m) -> 
            let scopem = unionRanges m endm
            let mutRecDefns = typeSpecs |> List.map MutRecShape.Tycon
            let env = TcDeclarations.TcMutRecSignatureDecls cenv env parent emptyUnscopedTyparEnv m scopem None mutRecDefns
            return env 

        | SynModuleSigDecl.Open (mp,m) -> 
            let scopem = unionRanges m.EndRange endm
            let env = TcOpenDecl cenv.tcSink cenv.g cenv.amap m scopem env mp
            return env

        | SynModuleSigDecl.Val (vspec,m) -> 
            let parentModule = 
                match parent with 
                | ParentNone -> error(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),vspec.RangeOfId)) 
                | Parent p -> p
            let containerInfo = ModuleOrNamespaceContainerInfo(parentModule)
            let idvs,_ = TcAndPublishValSpec (cenv,env,containerInfo,ModuleOrMemberBinding,None,emptyUnscopedTyparEnv,vspec)
            let scopem = unionRanges m endm
            let env = List.foldBack (AddLocalVal cenv.tcSink scopem) idvs env
            return env

        | SynModuleSigDecl.NestedModule(ComponentInfo(attribs,_parms, _constraints,longPath,xml,_,vis,im) as compInfo,isRec,mdefs,m) ->
            if isRec then 
                // Treat 'module rec M = ...' as a single mutually recursive definition group 'module M = ...'
                let modDecl = SynModuleSigDecl.NestedModule(compInfo,false,mdefs,m)
                return! TcSignatureElementsMutRec cenv parent endm None env [modDecl]
            else
                let id = ComputeModuleName longPath
                let vis,_ = ComputeAccessAndCompPath env None im vis None parent
                let attribs = TcAttributes cenv env AttributeTargets.ModuleDecl attribs
                CheckNamespaceModuleOrTypeName cenv.g id
                let modKind = EstablishTypeDefinitionCores.ComputeModuleOrNamespaceKind cenv.g true typeNames attribs id.idText
                let modName = EstablishTypeDefinitionCores.AdjustModuleName modKind id.idText
                CheckForDuplicateConcreteType env modName id.idRange

                // Now typecheck the signature, accumulating and then recording the submodule description. 
                let id = ident (modName, id.idRange)

                let mspec = NewModuleOrNamespace  (Some env.eCompPath) vis id (xml.ToXmlDoc()) attribs (MaybeLazy.Strict (NewEmptyModuleOrNamespaceType modKind)) 

                let! (mtyp,_) = TcModuleOrNamespaceSignatureElementsNonMutRec cenv (Parent (mkLocalModRef mspec)) env (id,modKind,mdefs,m,xml)

                mspec.entity_modul_contents <- MaybeLazy.Strict mtyp 
                let scopem = unionRanges m endm
                PublishModuleDefn cenv env mspec
                let env = AddLocalSubModuleAndReport cenv.tcSink scopem cenv.g cenv.amap m env mspec
                return env
            
        | SynModuleSigDecl.ModuleAbbrev (id,p,m) -> 
            let ad = env.eAccessRights
            let mvvs = ForceRaise (ResolveLongIndentAsModuleOrNamespace ResultCollectionSettings.AllResults cenv.amap m OpenQualified env.eNameResEnv ad p)
            let scopem = unionRanges m endm
            let modrefs = mvvs |> List.map p23 
            if modrefs.Length > 0 && modrefs |> List.forall (fun modref -> modref.IsNamespace) then 
                errorR(Error(FSComp.SR.tcModuleAbbreviationForNamespace(fullDisplayTextOfModRef (List.head modrefs)),m))
            let modrefs = modrefs |> List.filter (fun modref -> not modref.IsNamespace)
            modrefs |> List.iter (fun modref -> CheckEntityAttributes cenv.g modref m |> CommitOperationResult)        
            
            let env = 
                if modrefs.Length > 0 then AddModuleAbbreviationAndReport cenv.tcSink scopem id modrefs env 
                else env
            return env

        | SynModuleSigDecl.HashDirective _ -> 
            return env


        | SynModuleSigDecl.NamespaceFragment (SynModuleOrNamespaceSig(longId,isRec,isModule,defs,xml,attribs,vis,m)) -> 

            do for id in longId do 
                 CheckNamespaceModuleOrTypeName cenv.g id

            // Logically speaking, this changes 
            //    module [rec] A.B.M
            //    ...
            // to 
            //    namespace [rec] A.B
            //      module M = ...
            let enclosingNamespacePath, defs = 
                if isModule then 
                    let nsp, modName = List.frontAndBack longId
                    let modDecl = [SynModuleSigDecl.NestedModule(ComponentInfo(attribs,[], [],[modName],xml,false,vis,m),false,defs,m)] 
                    nsp, modDecl
                else 
                    longId, defs

            let envNS = LocateEnv cenv.topCcu env enclosingNamespacePath
            let envNS = ImplicitlyOpenOwnNamespace cenv.tcSink cenv.g cenv.amap m enclosingNamespacePath envNS

            // For 'namespace rec' and 'module rec' we add the thing being defined 
            let mtypNS = !(envNS.eModuleOrNamespaceTypeAccumulator)
            let mtypRoot, mspecNSOpt = BuildRootModuleType enclosingNamespacePath envNS.eCompPath mtypNS

            // For 'namespace rec' and 'module rec' we add the thing being defined 
            let envNS = if isRec then AddLocalRootModuleOrNamespace cenv.tcSink cenv.g cenv.amap m envNS mtypRoot else envNS
            let nsInfo = Some (mspecNSOpt, envNS.eModuleOrNamespaceTypeAccumulator) 
            let mutRecNSInfo = if isRec then nsInfo else None

            let! envAtEnd = TcSignatureElements cenv ParentNone m.EndRange envNS xml mutRecNSInfo defs

            MutRecBindingChecking.TcMutRecDefns_UpdateNSContents nsInfo

            let env = 
                if isNil enclosingNamespacePath then 
                    envAtEnd
                else
                    let env = AddLocalRootModuleOrNamespace cenv.tcSink cenv.g cenv.amap m env mtypRoot

                    // If the namespace is an interactive fragment e.g. FSI_0002, then open FSI_0002 in the subsequent environment.
                    let env = 
                        match TryStripPrefixPath cenv.g enclosingNamespacePath with 
                        | Some(p,_) -> TcOpenDecl cenv.tcSink cenv.g cenv.amap m.EndRange m.EndRange env [p]
                        | None -> env

                    // Publish the combined module type
                    env.eModuleOrNamespaceTypeAccumulator := CombineCcuContentFragments m [!(env.eModuleOrNamespaceTypeAccumulator); mtypRoot]
                    env

            return env
            
    with e -> 
        errorRecovery e endm 
        return env
  }


and TcSignatureElements cenv parent endm env xml mutRecNSInfo defs = 
    eventually {
        // Ensure the .Deref call in UpdateAccModuleOrNamespaceType succeeds 
        if cenv.compilingCanonicalFslibModuleType then 
            ensureCcuHasModuleOrNamespaceAtPath cenv.topCcu env.ePath env.eCompPath (xml.ToXmlDoc())

        match mutRecNSInfo with 
        | Some _ ->
            return! TcSignatureElementsMutRec cenv parent endm mutRecNSInfo env defs
        | None ->
            return! TcSignatureElementsNonMutRec cenv parent endm env defs
    }

and TcSignatureElementsNonMutRec cenv parent endm env defs = 
    eventually {
        // Collect the type names so we can implicitly add the compilation suffix to module names
        let typeNames = 
            [ for def in defs do 
               match def with 
               | SynModuleSigDecl.Types (typeSpecs,_) -> 
                  for (TypeDefnSig(ComponentInfo(_,typars,_,ids,_,_,_,_),trepr,extraMembers,_)) in typeSpecs do 
                      if isNil typars then
                          match trepr with 
                          | SynTypeDefnSigRepr.Simple((SynTypeDefnSimpleRepr.None _),_) when not (isNil extraMembers) -> ()
                          | _ -> yield (List.last ids).idText
               | _ -> () ]
            |> set

        return! Eventually.fold (TcSignatureElementNonMutRec cenv parent typeNames endm) env defs
    }

and TcSignatureElementsMutRec cenv parent endm mutRecNSInfo envInitial (defs: SynModuleSigDecl list) =
    eventually {
        let m = match defs with [] -> endm | _ -> defs |> List.map (fun d -> d.Range) |> List.reduce unionRanges
        let scopem = (defs, endm) ||> List.foldBack (fun h m -> unionRanges h.Range m) 

        let mutRecDefns = 
          let rec loop isNamespace defs : MutRecSigsInitialData = 
            ((true, true), defs) ||> List.collectFold (fun (openOk, moduleAbbrevOk) def -> 
                match def with 
                | SynModuleSigDecl.Types (typeSpecs,_) -> 
                    let decls = typeSpecs |> List.map MutRecShape.Tycon
                    decls, (false, false)

                | SynModuleSigDecl.Open (lid,m) -> 
                      if not openOk then errorR(Error(FSComp.SR.tcOpenFirstInMutRec(),m))
                      let decls = [ MutRecShape.Open (MutRecDataForOpen(lid, m)) ]
                      decls, (openOk, moduleAbbrevOk)

                | SynModuleSigDecl.Exception (SynExceptionSig(exnRepr,members,_),_) ->
                      let ( SynExceptionDefnRepr(synAttrs,UnionCase(_,id,_args,_,_,_),_,doc,vis,m)) = exnRepr
                      let compInfo = ComponentInfo(synAttrs,[],[],[id],doc,false,vis,id.idRange)
                      let decls = [ MutRecShape.Tycon(SynTypeDefnSig.TypeDefnSig(compInfo, SynTypeDefnSigRepr.Exception exnRepr, members, m)) ]
                      decls, (false, false)

                | SynModuleSigDecl.Val (vspec,_) -> 
                    if isNamespace then error(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),vspec.RangeOfId)) 
                    let decls = [ MutRecShape.Lets(vspec) ]
                    decls, (false, false)

                | SynModuleSigDecl.NestedModule(compInfo,isRec,synDefs,_) ->
                      if isRec then warning(Error(FSComp.SR.tcRecImplied(),compInfo.Range))
                      let mutRecDefs = loop false synDefs 
                      let decls = [MutRecShape.Module (compInfo, mutRecDefs)]
                      decls, (false, false)

                | SynModuleSigDecl.HashDirective _ -> 
                      [], (openOk, moduleAbbrevOk)

                | SynModuleSigDecl.ModuleAbbrev (id, p, m) ->
                      if not moduleAbbrevOk then errorR(Error(FSComp.SR.tcModuleAbbrevFirstInMutRec(),m))
                      let decls = [ MutRecShape.ModuleAbbrev (MutRecDataForModuleAbbrev(id, p, m)) ]
                      decls, (false, moduleAbbrevOk)

                | SynModuleSigDecl.NamespaceFragment _ ->
                    error(Error(FSComp.SR.tcUnsupportedMutRecDecl(),def.Range)))

              |> fst
          loop (match parent with ParentNone -> true | Parent _ -> false) defs
        return TcDeclarations.TcMutRecSignatureDecls cenv envInitial parent emptyUnscopedTyparEnv m scopem mutRecNSInfo mutRecDefns
    }



and TcModuleOrNamespaceSignatureElementsNonMutRec cenv parent env (id,modKind,defs,m:range,xml) =

  eventually {
    let endm = m.EndRange // use end of range for errors 

    // Create the module type that will hold the results of type checking.... 
    let envForModule,mtypeAcc = MakeInnerEnv env id modKind

    // Now typecheck the signature, using mutation to fill in the submodule description. 
    let! envAtEnd = TcSignatureElements cenv parent endm envForModule xml None defs
    
    // mtypeAcc has now accumulated the module type 
    return !mtypeAcc, envAtEnd
  }
    
//-------------------------------------------------------------------------
// Bind definitions within modules
//------------------------------------------------------------------------- 


let ElimModuleDoBinding bind =
    match bind with 
    | SynModuleDecl.DoExpr (spExpr,expr, m) -> 
        let bind2 = Binding (None,StandaloneExpression,false,false,[],PreXmlDoc.Empty,SynInfo.emptySynValData,SynPat.Wild m,None,expr,m,spExpr)
        SynModuleDecl.Let(false,[bind2],m)
    | _ -> bind

let TcMutRecDefnsEscapeCheck (binds: MutRecShapes<_,_,_,_,_>) env = 
    let freeInEnv = GeneralizationHelpers.ComputeUnabstractableTycons env
    let checkTycon (tycon: Tycon) = 
        if not tycon.IsTypeAbbrev && Zset.contains tycon freeInEnv then 
            let nm = tycon.DisplayName
            errorR(Error(FSComp.SR.tcTypeUsedInInvalidWay(nm, nm, nm), tycon.Range))

    binds |> MutRecShapes.iterTycons (fst >> Option.iter checkTycon) 

    let freeInEnv = GeneralizationHelpers.ComputeUnabstractableTraitSolutions env
    let checkBinds (binds: Binding list) = 
        for bind in binds do 
            if Zset.contains bind.Var freeInEnv then 
                let nm = bind.Var.DisplayName 
                errorR(Error(FSComp.SR.tcMemberUsedInInvalidWay(nm, nm, nm), bind.Var.Range))

    binds |> MutRecShapes.iterTyconsAndLets (snd >> checkBinds) checkBinds 

// ignore solitary '()' expressions and 'do ()' bindings, since these are allowed in namespaces
// for the purposes of attaching attributes to an assembly, e.g. 
//   namespace A.B.C
//     [<assembly : Foo >]
//     do()

let CheckLetOrDoInNamespace binds m =
    match binds with 
    | [ Binding (None,(StandaloneExpression | DoBinding),false,false,[],_,_,_,None,(SynExpr.Do (SynExpr.Const (SynConst.Unit,_),_) | SynExpr.Const (SynConst.Unit,_)),_,_) ] ->
        ()
    | [] -> 
        error(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),m)) 
    | _ -> 
        error(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),binds.Head.RangeOfHeadPat)) 

/// The non-mutually recursive case for a declaration
let rec TcModuleOrNamespaceElementNonMutRec (cenv:cenv) parent typeNames scopem env synDecl = 
  eventually {
    cenv.synArgNameGenerator.Reset()
    let tpenv = emptyUnscopedTyparEnv

    //printfn "----------\nCHECKING, e = %+A\n------------------\n" e
    try 
      match ElimModuleDoBinding synDecl with 

      | SynModuleDecl.ModuleAbbrev (id,p,m) -> 
          let env = MutRecBindingChecking.TcModuleAbbrevDecl cenv scopem env (id,p,m)
          return ((fun e -> e), []), env, env

      | SynModuleDecl.Exception (edef,m) -> 
          let binds,decl,env = TcExceptionDeclarations.TcExnDefn cenv env parent (edef,scopem)
          return ((fun e -> TMDefRec(true,[decl], binds |> List.map ModuleOrNamespaceBinding.Binding,m) :: e),[]), env, env

      | SynModuleDecl.Types (typeDefs,m) -> 
          let scopem = unionRanges m scopem
          let mutRecDefns = typeDefs |> List.map MutRecShape.Tycon
          let mutRecDefnsChecked,envAfter = TcDeclarations.TcMutRecDefinitions cenv env parent tpenv m scopem None mutRecDefns
          // Check the non-escaping condition as we build the expression on the way back up 
          let exprfWithEscapeCheck e = 
              TcMutRecDefnsEscapeCheck mutRecDefnsChecked env
              TcMutRecDefsFinish cenv mutRecDefnsChecked m :: e 

          return (exprfWithEscapeCheck, []), envAfter, envAfter

      | SynModuleDecl.Open (LongIdentWithDots(mp,_),m) -> 
          let scopem = unionRanges m.EndRange scopem
          let env = TcOpenDecl cenv.tcSink cenv.g cenv.amap m scopem env mp
          return ((fun e -> e),[]), env, env

      | SynModuleDecl.Let (letrec, binds, m) -> 

          match parent with
          | ParentNone ->
                CheckLetOrDoInNamespace binds m
                return (id,[]), env, env

          | Parent parentModule -> 
              let containerInfo = ModuleOrNamespaceContainerInfo(parentModule)
              if letrec then 
                let scopem = unionRanges m scopem
                let binds = binds |> List.map (fun bind -> RecDefnBindingInfo(containerInfo,NoNewSlots,ModuleOrMemberBinding,bind))
                let binds,env,_ = TcLetrec  WarnOnOverrides cenv env tpenv (binds,m, scopem)
                return ((fun e -> TMDefRec(true,[],binds |> List.map ModuleOrNamespaceBinding.Binding,m) :: e),[]), env, env
              else 
                let binds,env,_ = TcLetBindings cenv env containerInfo ModuleOrMemberBinding tpenv (binds,m,scopem)
                return ((fun e -> binds@e),[]), env, env 

      | SynModuleDecl.DoExpr _ -> return! failwith "unreachable"

      | SynModuleDecl.Attributes (synAttrs,_) -> 
          let attrs, _ = TcAttributesWithPossibleTargets false cenv env AttributeTargets.Top synAttrs
          return ((fun e -> e), attrs), env, env

      | SynModuleDecl.HashDirective _ -> 
          return ((fun e -> e), []), env, env

      | SynModuleDecl.NestedModule(compInfo, isRec, mdefs, isContinuingModule, m) ->

          // Treat 'module rec M = ...' as a single mutully recursive definition group 'module M = ...'
          if isRec then 
              assert (not isContinuingModule)
              let modDecl = SynModuleDecl.NestedModule(compInfo, false, mdefs, isContinuingModule, m)
              return! TcModuleOrNamespaceElementsMutRec cenv parent m env None [modDecl]
          else
              let (ComponentInfo(attribs,_parms, _constraints,longPath,xml,_,vis,im)) = compInfo
              let id = ComputeModuleName longPath

              let modAttrs = TcAttributes cenv env AttributeTargets.ModuleDecl attribs
              let modKind = EstablishTypeDefinitionCores.ComputeModuleOrNamespaceKind cenv.g true typeNames modAttrs id.idText
              let modName = EstablishTypeDefinitionCores.AdjustModuleName modKind id.idText
              CheckForDuplicateConcreteType env modName im
              CheckForDuplicateModule env id.idText id.idRange
              let vis,_ = ComputeAccessAndCompPath env None id.idRange vis None parent
             
              let endm = m.EndRange
              let id = ident (modName, id.idRange)

              CheckNamespaceModuleOrTypeName cenv.g id

              let envForModule, mtypeAcc = MakeInnerEnv env id modKind
    
              // Create the new module specification to hold the accumulated results of the type of the module 
              // Also record this in the environment as the accumulator 
              let mspec = NewModuleOrNamespace (Some env.eCompPath) vis id (xml.ToXmlDoc()) modAttrs (MaybeLazy.Strict (NewEmptyModuleOrNamespaceType modKind))

              // Now typecheck. 
              let! mexpr, topAttrsNew, envAtEnd = TcModuleOrNamespaceElements cenv (Parent (mkLocalModRef mspec)) endm envForModule xml None mdefs 

              // Get the inferred type of the decls and record it in the mspec. 
              mspec.entity_modul_contents <- MaybeLazy.Strict !mtypeAcc  
              let modDefn = TMDefRec(false,[],[ModuleOrNamespaceBinding.Module(mspec,mexpr)],m)
              PublishModuleDefn cenv env mspec 
              let env = AddLocalSubModuleAndReport cenv.tcSink scopem cenv.g cenv.amap m env mspec
          
              // isContinuingModule is true for all of the following
              //   - the implicit module of a script 
              //   - the major 'module' declaration for a file stating with 'module X.Y' 
              //   - an interactive entry for F# Interactive 
              //
              // In this case the envAtEnd is the environment at the end of this module, which doesn't contain the module definition itself
              // but does contain the results of all the 'open' declarations and so on.
              let envAtEnd = (if isContinuingModule then envAtEnd  else env)
          
              return ((fun modDefs -> modDefn :: modDefs),topAttrsNew), env, envAtEnd
      

      | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(longId,isRec,isModule,defs,xml,attribs,vis,m)) ->

          if !progress then dprintn ("Typecheck implementation " + textOfLid longId)
          let endm = m.EndRange

          do for id in longId do 
               CheckNamespaceModuleOrTypeName cenv.g id

          // Logically speaking, this changes 
          //    module [rec] A.B.M
          //    ...
          // to 
          //    namespace [rec] A.B
          //      module M = ...
          let enclosingNamespacePath, defs = 
              if isModule then 
                  let nsp, modName = List.frontAndBack longId
                  let modDecl = [SynModuleDecl.NestedModule(ComponentInfo(attribs,[], [],[modName],xml,false,vis,m),false,defs,true,m)] 
                  nsp, modDecl
              else 
                  longId, defs

          let envNS = LocateEnv cenv.topCcu env enclosingNamespacePath
          let envNS = ImplicitlyOpenOwnNamespace cenv.tcSink cenv.g cenv.amap m enclosingNamespacePath envNS

          let mtypNS = !(envNS.eModuleOrNamespaceTypeAccumulator)
          let mtypRoot, mspecNSOpt = BuildRootModuleType enclosingNamespacePath envNS.eCompPath mtypNS

          // TODO: test 'namespace rec global'

          // For 'namespace rec' and 'module rec' we add the thing being defined 
          let envNS = if isRec then AddLocalRootModuleOrNamespace cenv.tcSink cenv.g cenv.amap m envNS mtypRoot else envNS
          let nsInfo = Some (mspecNSOpt, envNS.eModuleOrNamespaceTypeAccumulator)
          let mutRecNSInfo = if isRec then nsInfo else None

          let! modExpr, topAttrs, envAtEnd = TcModuleOrNamespaceElements cenv parent endm envNS xml mutRecNSInfo defs

          MutRecBindingChecking.TcMutRecDefns_UpdateNSContents nsInfo
          
          let env = 
              if isNil enclosingNamespacePath then 
                  envAtEnd
              else
                  let env = AddLocalRootModuleOrNamespace cenv.tcSink cenv.g cenv.amap m env mtypRoot

                  // If the namespace is an interactive fragment e.g. FSI_0002, then open FSI_0002 in the subsequent environment
                  let env = 
                      match TryStripPrefixPath cenv.g enclosingNamespacePath with 
                      | Some(p,_) -> TcOpenDecl cenv.tcSink cenv.g cenv.amap m.EndRange m.EndRange env [p]
                      | None -> env

                  // Publish the combined module type
                  env.eModuleOrNamespaceTypeAccumulator := CombineCcuContentFragments m [!(env.eModuleOrNamespaceTypeAccumulator); mtypRoot]
                  env
          
          let modExprRoot = BuildRootModuleExpr enclosingNamespacePath envNS.eCompPath modExpr

          return ((fun modExprs -> modExprRoot :: modExprs),topAttrs), env, envAtEnd

    with exn -> 
        errorRecovery exn synDecl.Range 
        return ((fun e -> e), []), env, env
 }
 
/// The non-mutually recursive case for a sequence of declarations
and TcModuleOrNamespaceElementsNonMutRec cenv parent typeNames endm (defsSoFar, env, envAtEnd) (moreDefs: SynModuleDecl list) =
 eventually {
    match moreDefs with 
    | (firstDef :: otherDefs) ->
        // Lookahead one to find out the scope of the next declaration.
        let scopem = 
            if isNil otherDefs then unionRanges firstDef.Range endm
            else unionRanges (List.head otherDefs).Range endm

        // Possibly better:
        //let scopem = unionRanges h1.Range.EndRange endm
        
        let! firstDef',env', envAtEnd' = TcModuleOrNamespaceElementNonMutRec cenv parent typeNames scopem env firstDef
        // tail recursive 
        return! TcModuleOrNamespaceElementsNonMutRec  cenv parent typeNames endm ( (firstDef' :: defsSoFar), env', envAtEnd') otherDefs
    | [] -> 
        return List.rev defsSoFar, envAtEnd
 }

/// The mutually recursive case for a sequence of declarations (and nested modules)
and TcModuleOrNamespaceElementsMutRec cenv parent endm envInitial mutRecNSInfo (defs: SynModuleDecl list) =
 eventually {

    let m = match defs with [] -> endm | _ -> defs |> List.map (fun d -> d.Range) |> List.reduce unionRanges
    let scopem = (defs, endm) ||> List.foldBack (fun h m -> unionRanges h.Range m) 

    let (mutRecDefns, (_, _, synAttrs)) = 
      let rec loop isNamespace attrs defs : (MutRecDefnsInitialData * _) = 
        ((true, true, attrs),defs) ||> List.collectFold (fun (openOk,moduleAbbrevOk,attrs) def -> 
            match ElimModuleDoBinding def with

              | SynModuleDecl.Types (typeDefs,_) -> 
                  let decls = typeDefs |> List.map MutRecShape.Tycon
                  decls, (false, false, attrs)

              | SynModuleDecl.Let (letrec, binds, m) -> 
                  let binds = 
                      if isNamespace then 
                          CheckLetOrDoInNamespace binds m; []
                      else
                          if letrec then [MutRecShape.Lets binds]
                          else List.map (List.singleton >> MutRecShape.Lets) binds
                  binds, (false, false, attrs)

              | SynModuleDecl.NestedModule(compInfo, isRec, synDefs,_isContinuingModule,_) -> 
                  if isRec then warning(Error(FSComp.SR.tcRecImplied(),compInfo.Range))
                  let mutRecDefs, (_, _, attrs) = loop false attrs synDefs 
                  let decls = [MutRecShape.Module (compInfo, mutRecDefs)]
                  decls, (false, false, attrs)

              | SynModuleDecl.Open (LongIdentWithDots(lid,_), m) ->  
                  if not openOk then errorR(Error(FSComp.SR.tcOpenFirstInMutRec(),m))
                  let decls = [ MutRecShape.Open (MutRecDataForOpen(lid, m)) ]
                  decls, (openOk, moduleAbbrevOk, attrs)

              | SynModuleDecl.Exception (SynExceptionDefn(repr,members,_),_m) -> 
                  let (SynExceptionDefnRepr(synAttrs,UnionCase(_,id,_args,_,_,_),_repr,doc,vis,m)) = repr
                  let compInfo = ComponentInfo(synAttrs,[],[],[id],doc,false,vis,id.idRange)
                  let decls = [ MutRecShape.Tycon(SynTypeDefn.TypeDefn(compInfo, SynTypeDefnRepr.Exception repr, members, m)) ]
                  decls, (false, false, attrs)

              | SynModuleDecl.HashDirective _ -> 
                  [ ], (openOk, moduleAbbrevOk, attrs)

              | SynModuleDecl.Attributes (synAttrs,_) -> 
                  [ ], (false, false, synAttrs)

              | SynModuleDecl.ModuleAbbrev (id, p, m) ->
                  if not moduleAbbrevOk then errorR(Error(FSComp.SR.tcModuleAbbrevFirstInMutRec(),m))
                  let decls = [ MutRecShape.ModuleAbbrev (MutRecDataForModuleAbbrev(id, p, m)) ]
                  decls, (false, moduleAbbrevOk, attrs)

              | SynModuleDecl.DoExpr _ -> failwith "unreachable: SynModuleDecl.DoExpr - ElimModuleDoBinding"

              | (SynModuleDecl.NamespaceFragment _ as d) ->  error(Error(FSComp.SR.tcUnsupportedMutRecDecl(),d.Range)))

      loop (match parent with ParentNone -> true | Parent _ -> false) [] defs

    let tpenv = emptyUnscopedTyparEnv 
    let mutRecDefnsChecked,envAfter = TcDeclarations.TcMutRecDefinitions cenv envInitial parent tpenv m scopem mutRecNSInfo mutRecDefns 

    // Check the assembly attributes
    let attrs, _ = TcAttributesWithPossibleTargets false cenv envAfter AttributeTargets.Top synAttrs

    // Check the non-escaping condition as we build the list of module expressions on the way back up 
    let exprfWithEscapeCheck modExprs = 
        TcMutRecDefnsEscapeCheck mutRecDefnsChecked envInitial
        let modExpr = TcMutRecDefsFinish cenv mutRecDefnsChecked m 
        modExpr :: modExprs

    return (exprfWithEscapeCheck,attrs),envAfter, envAfter

 }

and TcMutRecDefsFinish cenv defs m =
    let tycons = defs |> List.choose (function MutRecShape.Tycon (Some tycon,_) -> Some tycon | _ -> None)
    let binds = 
        defs |> List.collect (function 
            | MutRecShape.Open _ -> []
            | MutRecShape.ModuleAbbrev _ -> []
            | MutRecShape.Tycon (_,binds) 
            | MutRecShape.Lets binds -> 
                binds |> List.map ModuleOrNamespaceBinding.Binding 
            | MutRecShape.Module ((MutRecDefnsPhase2DataForModule(mtypeAcc, mspec), _),mdefs) -> 
                let mexpr = TcMutRecDefsFinish cenv mdefs m
                mspec.entity_modul_contents <- MaybeLazy.Strict !mtypeAcc  
                [ ModuleOrNamespaceBinding.Module(mspec,mexpr) ])

    TMDefRec(true,tycons,binds,m)

and TcModuleOrNamespaceElements cenv parent endm env xml mutRecNSInfo defs =
  eventually {
    // Ensure the deref_nlpath call in UpdateAccModuleOrNamespaceType succeeds 
    if cenv.compilingCanonicalFslibModuleType then 
        ensureCcuHasModuleOrNamespaceAtPath cenv.topCcu env.ePath env.eCompPath (xml.ToXmlDoc())

    match mutRecNSInfo with 
    | Some _ -> 
        let! (exprf, topAttrsNew), _, envAtEnd = TcModuleOrNamespaceElementsMutRec cenv parent endm env mutRecNSInfo defs
        // Apply the functions for each declaration to build the overall expression-builder 
        let mexpr = TMDefs(exprf []) 
        return (mexpr, topAttrsNew, envAtEnd)

    | None -> 
        // Collect the type names so we can implicitly add the compilation suffix to module names
        let typeNames = 
            [ for def in defs do 
                match def with 
                | SynModuleDecl.Types (typeSpecs,_) -> 
                    for (TypeDefn(ComponentInfo(_,typars,_,ids,_,_,_,_),trepr,_,_)) in typeSpecs do 
                        if isNil typars then
                            match trepr with 
                            | SynTypeDefnRepr.ObjectModel(TyconAugmentation,_,_) -> ()
                            | _ -> yield (List.last ids).idText
                | _ -> () ]
            |> set

        let! compiledDefs, envAtEnd = TcModuleOrNamespaceElementsNonMutRec cenv parent typeNames endm ([], env, env) defs 

        // Apply the functions for each declaration to build the overall expression-builder 
        let mexpr = TMDefs(List.foldBack (fun (f,_) x -> f x) compiledDefs []) 

        // Collect up the attributes that are global to the file 
        let topAttrsNew = List.foldBack (fun (_,y) x -> y@x) compiledDefs []
        return (mexpr, topAttrsNew, envAtEnd)
  }  
    

//--------------------------------------------------------------------------
// TypeCheckOneImplFile - Typecheck all the namespace fragments in a file.
//-------------------------------------------------------------------------- 


let ApplyAssemblyLevelAutoOpenAttributeToTcEnv g amap (ccu: CcuThunk) scopem env (p, root)  = 
    let warn() = 
        warning(Error(FSComp.SR.tcAttributeAutoOpenWasIgnored(p, ccu.AssemblyName),scopem))
        env
    let p = splitNamespace p 
    if isNil p then warn() else
    let h,t = List.frontAndBack p 
    let modref = mkNonLocalTyconRef (mkNonLocalEntityRef ccu (Array.ofList h))  t
    match modref.TryDeref with 
    | VNone ->  warn()
    | VSome _ -> OpenModulesOrNamespaces TcResultsSink.NoSink g amap scopem root env [modref]

// Add the CCU and apply the "AutoOpen" attributes
let AddCcuToTcEnv(g,amap,scopem,env,assemblyName,ccu,autoOpens,internalsVisible) = 
    let env = AddNonLocalCcu g amap scopem env assemblyName (ccu,internalsVisible)

    // See https://fslang.uservoice.com/forums/245727-f-language/suggestions/6107641-make-microsoft-prefix-optional-when-using-core-f
    // "Microsoft" is opened by default in FSharp.Core
    let autoOpens = 
        let autoOpens = autoOpens  |> List.map (fun p -> (p,false))
        if ccuEq ccu g.fslibCcu then 
            // Auto open 'Microsoft' in FSharp.Core.dll. Even when using old versions of FSharp.Core.dll that do
            // not have this attribute. The 'true' means 'treat all namespaces so revealed as "roots" accessible via
            // global, e.g. global.FSharp.Collections'
            ("Microsoft", true) :: autoOpens
        else 
            autoOpens

    let env = (env,autoOpens) ||> List.fold (ApplyAssemblyLevelAutoOpenAttributeToTcEnv g amap ccu scopem)
    env

let CreateInitialTcEnv(g,amap,scopem,assemblyName,ccus) =
    (emptyTcEnv g, ccus) ||> List.fold (fun env (ccu,autoOpens,internalsVisible) -> 
        try 
            AddCcuToTcEnv(g,amap,scopem,env,assemblyName,ccu,autoOpens,internalsVisible)
        with e -> 
            errorRecovery e scopem 
            env) 

type ConditionalDefines = 
    string list


/// The attributes that don't get attached to any declaration
type TopAttribs =
    { mainMethodAttrs: Attribs
      netModuleAttrs: Attribs
      assemblyAttrs : Attribs  }

let EmptyTopAttrs =
    { mainMethodAttrs=[]
      netModuleAttrs=[]
      assemblyAttrs =[]  }

let CombineTopAttrs topAttrs1 topAttrs2 =
    { mainMethodAttrs = topAttrs1.mainMethodAttrs @ topAttrs2.mainMethodAttrs
      netModuleAttrs  = topAttrs1.netModuleAttrs @ topAttrs2.netModuleAttrs
      assemblyAttrs   = topAttrs1.assemblyAttrs @ topAttrs2.assemblyAttrs } 

let rec IterTyconsOfModuleOrNamespaceType f (mty:ModuleOrNamespaceType) = 
    mty.AllEntities |> QueueList.iter (fun tycon -> f tycon)
    mty.ModuleAndNamespaceDefinitions |> List.iter (fun v -> 
        IterTyconsOfModuleOrNamespaceType f v.ModuleOrNamespaceType)


// Defaults get applied before the module signature is checked and before the implementation conditions on virtuals/overrides. 
// Defaults get applied in priority order. Defaults listed last get priority 0 (lowest), 2nd last priority 1 etc. 
let ApplyDefaults cenv g denvAtEnd m mexpr extraAttribs = 
    try
        let unsolved = Microsoft.FSharp.Compiler.FindUnsolved.UnsolvedTyparsOfModuleDef g cenv.amap denvAtEnd (mexpr,extraAttribs)

        GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denvAtEnd,m) unsolved

        let applyDefaults priority =
              unsolved |> List.iter (fun tp -> 
                if not tp.IsSolved then 
                    // Apply the first default. If we're defaulting one type variable to another then 
                    // the defaults will be propagated to the new type variable. 
                    tp.Constraints |> List.iter (fun tpc -> 
                        match tpc with 
                        | TyparConstraint.DefaultsTo(priority2,ty2,m) when priority2 = priority -> 
                            let ty1 = mkTyparTy tp
                            if not tp.IsSolved  && not (typeEquiv cenv.g ty1 ty2) then
                                let csenv = MakeConstraintSolverEnv ContextInfo.NoContext cenv.css m denvAtEnd
                                TryD (fun () -> ConstraintSolver.SolveTyparEqualsTyp csenv 0 m NoTrace ty1 ty2)
                                      (fun e -> solveTypAsError cenv denvAtEnd m ty1
                                                ErrorD(ErrorFromApplyingDefault(g,denvAtEnd,tp,ty2,e,m)))
                                |> RaiseOperationResult
                        | _ -> ()))
                    
        for priority = 10 downto 0 do
            applyDefaults priority

        // OK, now apply defaults for any unsolved HeadTypeStaticReq 
        unsolved |> List.iter (fun tp ->     
            if not tp.IsSolved then 
                if (tp.StaticReq <> NoStaticReq) then
                    ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css denvAtEnd tp)
    with e -> errorRecovery e m

let CheckValueRestriction denvAtEnd rootSigOpt implFileTypePriorToSig m = 
    if Option.isNone rootSigOpt then
      let rec check (mty:ModuleOrNamespaceType) =
          for v in mty.AllValsAndMembers do
              let ftyvs = (freeInVal CollectTyparsNoCaching v).FreeTypars |> Zset.elements
              if (not v.IsCompilerGenerated && 
                  not (ftyvs |> List.exists (fun tp -> tp.IsFromError)) && 
                  // Do not apply the value restriction to methods and functions
                  // Note, normally these completely generalize their argument types anyway. However,
                  // some methods (property getters/setters, constructors) can't be as generic
                  // as they might naturally be, and these can leave type variables unsolved. See
                  // for example FSharp 1.0 3661.
                  (match v.ValReprInfo with None -> true | Some tvi -> tvi.HasNoArgs)) then 
                match ftyvs with 
                | tp :: _ -> errorR (ValueRestriction(denvAtEnd,false,v, tp,v.Range))
                | _ -> ()
          mty.ModuleAndNamespaceDefinitions |> List.iter (fun v -> check v.ModuleOrNamespaceType) 
      try check implFileTypePriorToSig with e -> errorRecovery e m


let SolveInternalUnknowns g cenv denvAtEnd mexpr extraAttribs =
    let unsolved = Microsoft.FSharp.Compiler.FindUnsolved.UnsolvedTyparsOfModuleDef g cenv.amap denvAtEnd (mexpr,extraAttribs)

    unsolved |> List.iter (fun tp -> 
            if (tp.Rigidity <> TyparRigidity.Rigid) && not tp.IsSolved then 
                ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css denvAtEnd tp)

let CheckModuleSignature g cenv m denvAtEnd rootSigOpt implFileTypePriorToSig implFileSpecPriorToSig mexpr =
        match rootSigOpt with 
        | None -> 
            // Deep copy the inferred type of the module 
            let implFileTypePriorToSigCopied = copyModuleOrNamespaceType g CloneAll implFileTypePriorToSig

            ModuleOrNamespaceExprWithSig(implFileTypePriorToSigCopied,mexpr,m)
            
        | Some sigFileType -> 

            // We want to show imperative type variables in any types in error messages at this late point 
            let denv = { denvAtEnd with showImperativeTyparAnnotations=true }
            begin 
                try 
                
                    // As typechecked the signature and implementation use different tycons etc. 
                    // Here we (a) check there are enough names, (b) match them up to build a renaming and   
                    // (c) check signature conformance up to this renaming. 
                    if not (SignatureConformance.CheckNamesOfModuleOrNamespace denv (mkLocalTyconRef implFileSpecPriorToSig) sigFileType) then 
                        raise (ReportedError None)

                    // Compute the remapping from implementation to signature
                    let remapInfo ,_ = ComputeRemappingFromInferredSignatureToExplicitSignature cenv.g implFileTypePriorToSig sigFileType
                     
                    let aenv = { TypeEquivEnv.Empty with EquivTycons = TyconRefMap.OfList remapInfo.mrpiEntities }
                    
                    if not (SignatureConformance.Checker(cenv.g, cenv.amap, denv, remapInfo, true).CheckSignature  aenv (mkLocalModRef implFileSpecPriorToSig) sigFileType) then  (
                        // We can just raise 'ReportedError' since CheckModuleOrNamespace raises its own error 
                        raise (ReportedError None)
                    )
                with e -> errorRecovery e m
            end
            
            ModuleOrNamespaceExprWithSig(sigFileType,mexpr,m)


/// Check an entire implementation file
/// Typecheck, then close the inference scope and then check the file meets its signature (if any)
let TypeCheckOneImplFile 
       // checkForErrors: A function to help us stop reporting cascading errors 
       (g, niceNameGen, amap, topCcu, checkForErrors, conditionalDefines, tcSink) 
       env 
       (rootSigOpt : ModuleOrNamespaceType option)
       (ParsedImplFileInput(_,isScript,qualNameOfFile,scopedPragmas,_,implFileFrags,isLastCompiland)) =

 eventually {
    let cenv = cenv.Create (g, isScript, niceNameGen, amap, topCcu, false, Option.isSome rootSigOpt, conditionalDefines, tcSink, (LightweightTcValForUsingInBuildMethodCall g))    

    let envinner, mtypeAcc = MakeInitialEnv env 

    let defs = [ for x in implFileFrags -> SynModuleDecl.NamespaceFragment(x) ]
    let! mexpr, topAttrs, envAtEnd = TcModuleOrNamespaceElements cenv ParentNone qualNameOfFile.Range envinner PreXmlDocEmpty None defs

    let implFileTypePriorToSig = !mtypeAcc 

    let topAttrs = 
        let mainMethodAttrs,others = topAttrs |> List.partition (fun (possTargets,_) -> possTargets &&& AttributeTargets.Method <> enum 0) 
        let assemblyAttrs,others = others |> List.partition (fun (possTargets,_) -> possTargets &&& AttributeTargets.Assembly <> enum 0) 
        // REVIEW: consider checking if '_others' is empty
        let netModuleAttrs, _others = others |> List.partition (fun (possTargets,_) -> possTargets &&& AttributeTargets.Module <> enum 0)
        { mainMethodAttrs = List.map snd mainMethodAttrs
          netModuleAttrs  = List.map snd netModuleAttrs
          assemblyAttrs   = List.map snd assemblyAttrs}
    let denvAtEnd = envAtEnd.DisplayEnv
    let m = qualNameOfFile.Range
    
    // This is a fake module spec
    let implFileSpecPriorToSig = wrapModuleOrNamespaceType qualNameOfFile.Id (compPathOfCcu topCcu) implFileTypePriorToSig

    let extraAttribs = topAttrs.mainMethodAttrs@topAttrs.netModuleAttrs@topAttrs.assemblyAttrs
    
    conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
        ApplyDefaults cenv g denvAtEnd m mexpr extraAttribs)

    // Check completion of all classes defined across this file. 
    // NOTE: this is not a great technique if inner signatures are permitted to hide 
    // virtual dispatch slots. 
    conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
        try implFileTypePriorToSig |> IterTyconsOfModuleOrNamespaceType (FinalTypeDefinitionChecksAtEndOfInferenceScope (cenv.infoReader, envAtEnd.NameEnv, cenv.tcSink, true, denvAtEnd))
        with e -> errorRecovery e m)

    // Check the value restriction. Only checked if there is no signature.
    conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
      CheckValueRestriction denvAtEnd rootSigOpt implFileTypePriorToSig m)

    // Solve unsolved internal type variables 
    conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
        SolveInternalUnknowns g cenv denvAtEnd mexpr extraAttribs)

    // Check the module matches the signature 
    let implFileExprAfterSig = 
      conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
        CheckModuleSignature g cenv m denvAtEnd rootSigOpt implFileTypePriorToSig implFileSpecPriorToSig mexpr)

    // Run any additional checks registered for post-type-inference
    do 
      conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
         for check in cenv.postInferenceChecks do
            try  
                check()
            with e -> 
                errorRecovery e m)

    // We ALWAYS run the PostTypeCheckSemanticChecks phase, though we if we have already encountered some
    // errors we turn off error reporting. THis is because it performs various fixups over the TAST, e.g. 
    // assigning nice names for inference variables.
    let hasExplicitEntryPoint = 
        conditionallySuppressErrorReporting (checkForErrors()) (fun () ->
            try  
                let reportErrors = not (checkForErrors())
                Microsoft.FSharp.Compiler.PostTypeCheckSemanticChecks.CheckTopImpl (g,cenv.amap,reportErrors,cenv.infoReader,env.eInternalsVisibleCompPaths,cenv.topCcu,envAtEnd.DisplayEnv, implFileExprAfterSig,extraAttribs,isLastCompiland)
            with e -> 
                errorRecovery e m
                false)

    let implFile = TImplFile(qualNameOfFile,scopedPragmas, implFileExprAfterSig, hasExplicitEntryPoint,isScript)

    return (topAttrs,implFile,envAtEnd)
 } 
   


/// Check an entire signature file
let TypeCheckOneSigFile  (g,niceNameGen,amap,topCcu,checkForErrors,conditionalDefines,tcSink) tcEnv (ParsedSigFileInput(_,qualNameOfFile,_, _,sigFileFrags)) = 
 eventually {     
    let cenv = cenv.Create (g,false,niceNameGen,amap,topCcu,true,false,conditionalDefines,tcSink, (LightweightTcValForUsingInBuildMethodCall g))
    let envinner,mtypeAcc = MakeInitialEnv tcEnv 

    let specs = [ for x in sigFileFrags -> SynModuleSigDecl.NamespaceFragment(x) ]
    let! tcEnv = TcSignatureElements cenv ParentNone qualNameOfFile.Range envinner PreXmlDocEmpty None specs
    
    let sigFileType = !mtypeAcc 
    
    if not (checkForErrors()) then  
        try sigFileType |> IterTyconsOfModuleOrNamespaceType (FinalTypeDefinitionChecksAtEndOfInferenceScope(cenv.infoReader, tcEnv.NameEnv, cenv.tcSink, false, tcEnv.DisplayEnv))
        with e -> errorRecovery e qualNameOfFile.Range

    return (tcEnv,tcEnv,sigFileType)
 }
