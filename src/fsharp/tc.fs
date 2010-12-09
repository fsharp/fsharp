//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2010 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


/// The typechecker.  Left-to-right constrained type checking 
/// with generalization at appropriate points.
module internal Microsoft.FSharp.Compiler.TypeChecker

open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.Patcompile
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Outcome
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Infos.AccessibilityLogic
open Microsoft.FSharp.Compiler.Infos.AttributeChecking
open Microsoft.FSharp.Compiler.Typrelns
open Microsoft.FSharp.Compiler.ConstraintSolver
open Microsoft.FSharp.Compiler.Nameres
open Microsoft.FSharp.Compiler.PrettyNaming
open System
open System.Collections.Generic

//-------------------------------------------------------------------------
// Helpers that should be elsewhere
//------------------------------------------------------------------------- 

let isThreadOrContextStatic g attrs' = 
    HasAttrib g g.attrib_ThreadStaticAttribute attrs' ||
    HasAttrib g g.attrib_ContextStaticAttribute attrs' 

let mkNilListPat g m ty = TPat_unioncase(g.nil_ucref,[ty],[],m)
let mkConsListPat g ty ph pt = TPat_unioncase(g.cons_ucref,[ty],[ph;pt],unionRanges (rangeOfPat ph) (rangeOfPat pt))

let mkCompGenLetIn m nm ty e f = 
    let v,ve = mkCompGenLocal m nm ty
    mkCompGenLet m v e (f (v,ve))

let mkUnitDelayLambda g m e =
    let uv,_ = mkCompGenLocal m "unitVar" g.unit_ty
    mkLambda m uv (e,tyOfExpr g e) 

let mkCoerceIfNeeded g tgtTy srcTy expr =
    if typeEquiv g tgtTy srcTy then 
        expr
    else 
        mkCoerceExpr(expr,tgtTy,expr.Range,srcTy)


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
exception UnitTypeExpected of DisplayEnv * TType * bool * range
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

/// Is this a 'base' call (in the sense of C#) 
let IsBaseCall objArgs = 
    match objArgs with 
    | [Expr.Val(v,_,_)] when v.BaseOrThisInfo  = BaseVal -> true
    | _ -> false

let RecdFieldInstanceChecks g ad m (rfinfo:RecdFieldInfo) = 
    if rfinfo.IsStatic then error (Error (FSComp.SR.tcStaticFieldUsedWhenInstanceFieldExpected(),m));
    CheckRecdFieldInfoAttributes g rfinfo m |> CommitOperationResult;        
    CheckRecdFieldInfoAccessible m ad rfinfo

let ILFieldInstanceChecks  g amap ad m (finfo :ILFieldInfo) =
    if finfo.IsStatic then error (Error (FSComp.SR.tcStaticFieldUsedWhenInstanceFieldExpected(),m));
    CheckILFieldInfoAccessible g amap m ad finfo;
    CheckILFieldAttributes g finfo m

let MethInfoChecks g amap isInstance tyargsOpt objArgs ad m (minfo:MethInfo)  =
    if minfo.IsInstance <> isInstance then
      if isInstance then 
        error (Error (FSComp.SR.csMethodIsNotAnInstanceMethod(minfo.LogicalName),m));
      else        
        error (Error (FSComp.SR.csMethodIsNotAStaticMethod(minfo.LogicalName),m));

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
                AccessibleFrom(paths,None) 
        | _ -> ad

    if not (IsMethInfoAccessible amap m  ad minfo) then 
      error (Error (FSComp.SR.tcMethodNotAccessible(minfo.LogicalName),m));
    CheckMethInfoAttributes g m tyargsOpt minfo |> CommitOperationResult


let CheckRecdFieldMutation m denv (rfinfo:RecdFieldInfo) ftinst = 
    if not rfinfo.RecdField.IsMutable then error (FieldNotMutable(denv,rfinfo.RecdFieldRef,m));
    if nonNil(ftinst) then error (Error(FSComp.SR.tcTypeFunctionFieldsCannotBeMutated(),m))

//-------------------------------------------------------------------------
// Information about object constructors
//------------------------------------------------------------------------- 

type SafeInitData = 
    | SafeInitField of RecdFieldRef * RecdField
    | NoSafeInitInfo 
            
type CtorInfo = 
    { // Object model constructors have a very specific form to satisfy .NET limitations.
      // For "new = \arg. { new C with ... }"; 
      //     ctor = 3 indicates about to type check "\arg. (body)", 
      //     ctor = 2 indicates about to type check "body" 
      //     ctor = 1 indicates actually type checking the body expression 
      // 0 indicates everywhere else, including auxiliary expressions such e1 in "let x = e1 in { new ... }" 
      // Ideally we would clean up this rather odd approach ... 
      ctorShapeCounter: int;
      /// A handle to the ref cell to hold results of 'this' for 'type X() as x = ...' and 'new() as x = ...' constructs
      /// in case 'x' is used in the arguments to the 'inherits' call.
      safeThisValOpt: Val option; 
      /// A handle to the boolean ref cell to hold success of initialized 'this' for 'type X() as x = ...' constructs 
      safeInitInfo: SafeInitData; 
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
            willNeverHaveFreeTypars <- true; 
            cachedFreeLocalTycons <- fvs.FreeTycons
            cachedFreeTraitSolutions <- fvs.FreeTraitSolutions
        fvs

    member item.WillNeverHaveFreeTypars = willNeverHaveFreeTypars
    member item.CachedFreeLocalTycons = cachedFreeLocalTycons 
    member item.CachedFreeTraitSolutions = cachedFreeTraitSolutions
      
[<NoEquality; NoComparison>]
type TcEnv =
    { /// Name resoultion information 
      eNameResEnv : NameResolutionEnv; 

      /// The list of items in the environment that may contain free inference 
      /// variables (which may not be generalized). The relevant types may 
      /// change as a result of inference equations being asserted, hence may need to 
      /// be recomputed. 
      eUngeneralizableItems: UngeneralizableItem list;
      
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
      ePath: Ident list; 
      eCompPath: CompilationPath; 
      eAccessPath: CompilationPath; 
      eInternalsVisibleCompPaths: CompilationPath list; // internals under these should be accessible

      /// Mutable accumulator for the current module type 
      eMtypeAcc: ModuleOrNamespaceType ref; 

      /// Here Some(tcref) indicates we can access protected members in all super types 
      eFamilyType: TyconRef option; 

      // Information to enforce special restrictions on valid expressions 
      // for .NET constructors. 
      eCtorInfo : CtorInfo option
    } 
    member tenv.DisplayEnv = tenv.eNameResEnv.DisplayEnv
    member tenv.NameEnv = tenv.eNameResEnv

let empty_tenv g  =
    let cpath = CompPath (IL.ecmaMscorlibScopeRef,[])
    { eNameResEnv = NameResolutionEnv.Empty(g);
      eUngeneralizableItems=[];
      ePath=[];
      eCompPath=cpath; (* dummy *)
      eAccessPath=cpath; (* dummy *)
      eInternalsVisibleCompPaths=[];
      eMtypeAcc= ref (NewEmptyModuleOrNamespaceType Namespace);
      eFamilyType=None;
      eCtorInfo=None; }

//-------------------------------------------------------------------------
// Helpers related to determining if we're in a constructor and/or a class
// that may be able to access "protected" members.
//------------------------------------------------------------------------- 

let InitialExplicitCtorInfo (safeThisValOpt, safeInitInfo) =
    { ctorShapeCounter=3; 
      safeThisValOpt = safeThisValOpt;
      safeInitInfo = safeInitInfo;
      ctorIsImplicit=false} 

let InitialImplicitCtorInfo () =
    { ctorShapeCounter=0; 
      safeThisValOpt = None; 
      safeInitInfo = NoSafeInitInfo;
      ctorIsImplicit=true }
      
let EnterFamilyRegion tcref env = { env with eFamilyType = Some tcref }
let ExitFamilyRegion env = 
    match env.eFamilyType with 
    | None -> env  // optimization to avoid reallocation  
    | _ -> { env with eFamilyType = None }

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

let add_free_item_of_typ typ eUngeneralizableItems = 
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
    {env with eInternalsVisibleCompPaths = compPath :: env.eInternalsVisibleCompPaths }

let curr_access_cpath env = env.eAccessPath
let AccessRightsOfEnv env = AccessibleFrom (curr_access_cpath env :: env.eInternalsVisibleCompPaths ,env.eFamilyType)

let ModifyNameResEnv f env = { env with eNameResEnv = f env.eNameResEnv } 

let AddLocalValPrimitive (v:Val) env =
    let env = ModifyNameResEnv (fun nenv -> AddValRefToNameEnv nenv (mkLocalValRef v)) env
    {env with eUngeneralizableItems =  add_free_item_of_typ v.Type env.eUngeneralizableItems;   } 


let AddLocalValMap scopem (vals:Val NameMap) env =
    let env = ModifyNameResEnv (AddValMapToNameEnv vals) env
    let env = {env with eUngeneralizableItems =  NameMap.foldBackRange (typeOfVal >> add_free_item_of_typ) vals env.eUngeneralizableItems;   }
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

let AddLocalVals scopem (vals:Val list) env =
    let env = ModifyNameResEnv (AddValListToNameEnv vals) env
    let env = {env with eUngeneralizableItems =  List.foldBack (typeOfVal >> add_free_item_of_typ) vals env.eUngeneralizableItems;   }
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

let AddLocalVal scopem v env = 
    let env = ModifyNameResEnv (fun nenv -> AddValRefToNameEnv nenv (mkLocalValRef v)) env
    let env = {env with eUngeneralizableItems =  add_free_item_of_typ v.Type env.eUngeneralizableItems;   }
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

let AddLocalExnDefn scopem (exnc:Tycon) env =
    let env = ModifyNameResEnv (fun nenv -> AddExceptionDeclsToNameEnv nenv (mkLocalEntityRef exnc)) env
    (* Also make VisualStudio think there is an identifier in scope at the range of the identifier text of its binding location *)
    CallEnvSink(exnc.Range,env.NameEnv,AccessRightsOfEnv env);
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env
 
let AddLocalTyconRefs ownDefinition g amap m tcrefs env = 
     ModifyNameResEnv (fun nenv -> AddTyconRefsToNameEnv ownDefinition g amap m false nenv tcrefs) env 

let AddLocalTycons g amap m tycons env = 
     AddLocalTyconRefs false g amap m (List.map mkLocalTyconRef tycons) env 

let AddLocalTyconsAndReport g amap scopem tycons env = 
    let env = AddLocalTycons g amap scopem tycons env
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

//-------------------------------------------------------------------------
// Open a structure or an IL namespace 
//------------------------------------------------------------------------- 



let OpenModuleOrNamespace g amap scopem env modref =
    let env = ModifyNameResEnv (fun nenv -> AddModuleOrNamespaceContentsToNameEnv g amap (AccessRightsOfEnv env) scopem nenv modref)  env
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

let OpenModulesOrNamespaces g amap scopem env mvvs =
    List.foldBack (fun (_,modref,_) acc -> OpenModuleOrNamespace g amap scopem acc modref) mvvs env

let AddRootModuleOrNamespaceRefs g amap m env modrefs  = 
    ModifyNameResEnv (fun nenv -> AddModuleOrNamespaceRefsToNameEnv g amap m true (AccessRightsOfEnv env) nenv modrefs) env 

let AddNonLocalCcu g amap scopem env (ccu:CcuThunk,internalsVisible) = 
    // Compute the top-rooted module or namespace references
    let modrefs = ccu.RootModulesAndNamespaces |> List.map (mkNonLocalCcuRootEntityRef ccu)
    // Compute the top-rooted type definitions
    let tcrefs = ccu.RootTypeAndExceptionDefinitions |> List.map (mkNonLocalCcuRootEntityRef ccu)
    let env = AddRootModuleOrNamespaceRefs g amap scopem env modrefs
    let env = ModifyNameResEnv (fun nenv -> AddTyconRefsToNameEnv false g amap scopem true nenv tcrefs) env
    let env = if internalsVisible then addInternalsAccessibility env ccu else env
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

let AddLocalRootModuleOrNamespace g amap scopem env (mtyp:ModuleOrNamespaceType) = 
    // Compute the top-rooted module or namespace references
    let modrefs = mtyp.ModuleAndNamespaceDefinitions |> List.map mkLocalModRef
    // Compute the top-rooted type definitions
    let tcrefs = mtyp.TypeAndExceptionDefinitions |> List.map mkLocalTyconRef
    let env = AddRootModuleOrNamespaceRefs g amap scopem env modrefs
    let env = ModifyNameResEnv (fun nenv -> AddTyconRefsToNameEnv false g amap scopem true nenv tcrefs) env
    let env = {env with eUngeneralizableItems = addFreeItemOfModuleTy mtyp env.eUngeneralizableItems}
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env

let AddModuleAbbreviation scopem id modrefs env =
    let env = ModifyNameResEnv (fun nenv -> AddModuleAbbrevToNameEnv id nenv modrefs) env
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    CallNameResolutionSink(id.idRange,env.NameEnv,Item.ModuleOrNamespaces(modrefs),ItemOccurence.Use,env.DisplayEnv,AccessRightsOfEnv env)
    env

let AddLocalSubModule g amap m scopem env (modul:ModuleOrNamespace) =
    let env = ModifyNameResEnv (fun nenv -> AddModrefToNameEnv g amap m false (AccessRightsOfEnv env) nenv (mkLocalModRef modul)) env
    let env = {env with eUngeneralizableItems = addFreeItemOfModuleTy modul.ModuleOrNamespaceType env.eUngeneralizableItems}
    CallEnvSink(scopem,env.NameEnv,AccessRightsOfEnv env);
    env
 
let RegisterDeclaredTypars typars env = 
    {env with eUngeneralizableItems =  List.foldBack (mkTyparTy >> add_free_item_of_typ) typars env.eUngeneralizableItems }

let AddDeclaredTypars check typars env = 
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
    { g: Env.TcGlobals;

      /// Push an entry every time a recursive value binding is used, 
      /// in order to be able to fix up recursive type applications as 
      /// we infer type parameters 
      mutable recUses: ValMultiMap<(Expr ref * range * bool)>;
      
      /// Are we in a script? if so relax the reporting of discarded-expression warnings at the top level
      isScript: bool; 
      isInteractive : bool;

      /// Environment needed to convert IL types to F# types in the importer. 
      amap: Import.ImportMap; 

      /// Holds a reference to the component being compiled. 
      /// This field is very rarely used (mainly when fixing up forward references to fslib. 
      topCcu: CcuThunk; 
      
      /// Holds the current inference constraints 
      css: ConstraintSolverState;
      
      /// Are we compiling the signature of a module from fslib? 
      compilingCanonicalFslibModuleType: bool;
      isSig: bool;
      haveSig: bool;
      
      niceNameGen: NiceNameGenerator;
      infoReader: InfoReader;
      nameResolver: NameResolver;
      
      conditionalDefines: string list;
            
    } 

    static member Create (g,isScript,niceNameGen,amap,topCcu,isSig,haveSig,conditionalDefines,isInteractive) =
        let infoReader = new InfoReader(g,amap)
        let instantiationGenerator m tpsorig = ConstraintSolver.FreshenTypars m tpsorig
        let nameResolver = new NameResolver(g,amap,infoReader,instantiationGenerator)
        { g=g;
          amap= amap;
          recUses=ValMultiMap<_>.Empty; 
          topCcu = topCcu;
          isScript=isScript;
          css= ConstraintSolverState.New(g,amap,infoReader)
          infoReader=infoReader;
          nameResolver=nameResolver;
          niceNameGen=niceNameGen;
          isSig=isSig;
          haveSig=haveSig;
          compilingCanonicalFslibModuleType=(isSig || not haveSig) && g.compilingFslib;
          conditionalDefines=conditionalDefines 
          isInteractive=isInteractive}


let CopyAndFixupTypars m rigid tpsorig = 
    ConstraintSolver.FreshenAndFixupTypars m rigid [] [] tpsorig

let UnifyTypes cenv (env: TcEnv) m expectedTy actualTy = 
    ConstraintSolver.AddCxTypeEqualsType env.DisplayEnv cenv.css m (tryNormalizeMeasureInType cenv.g expectedTy) (tryNormalizeMeasureInType cenv.g actualTy)



//-------------------------------------------------------------------------
// Generate references to the module being generated - used for
// public items.
//------------------------------------------------------------------------- 

let MakeInitialEnv env = 
    (* Note: here we allocate a new module type accumulator *)
    let mtypeAcc = ref (NewEmptyModuleOrNamespaceType Namespace)
    { env with eMtypeAcc = mtypeAcc  },mtypeAcc

let MakeInnerEnv env nm modKind = 
    let path = env.ePath @ [nm]
    (* Note: here we allocate a new module type accumulator *)
    let mtypeAcc = ref (NewEmptyModuleOrNamespaceType modKind)
    let cpath = mkNestedCPath env.eCompPath nm.idText modKind
    { env with ePath = path; 
               eCompPath = cpath;
               eAccessPath = cpath;
               eNameResEnv = { env.eNameResEnv with eDisplayEnv = env.DisplayEnv.AddOpenPath (pathOfLid path) };
               eMtypeAcc = mtypeAcc  },mtypeAcc


let MakeInnerEnvForTyconRef _cenv env tcref isExtrinsicExtension = 
    if isExtrinsicExtension then 
        // Extension members don't get access to protected stuff 
        env  
    else
        // Regular members get access to protected stuff 
        let env = (EnterFamilyRegion tcref env) 
        // Note: assumes no nesting 
        let env = { env with eAccessPath = mkNestedCPath env.eCompPath tcref.LogicalName FSharpModule }

        env

let MakeInnerEnvForMember cenv env (v:Val) = 
    match v.MemberInfo with 
    | None -> env
    | Some _ -> MakeInnerEnvForTyconRef cenv env v.MemberApparentParent v.IsExtensionMember 

let GetCurrAccumulatedModuleOrNamespaceType env = !(env.eMtypeAcc) 
let SetCurrAccumulatedModuleOrNamespaceType env x =  env.eMtypeAcc := x

/// Set up the initial environment 
let LocateEnv ccu env enclosingNamespacePath =
    let cpath = compPathOfCcu ccu
    let env = {env with ePath = []; eCompPath = cpath; eAccessPath=cpath }
    let env = List.fold (fun env id -> MakeInnerEnv env id Namespace |> fst) env enclosingNamespacePath
    env

let BuildRootModuleType enclosingNamespacePath cpath mtyp = 
    (enclosingNamespacePath,(cpath, mtyp)) 
        ||> List.foldBack (fun id (cpath, mtyp) -> (parentCompPath cpath, wrapModuleOrNamespaceTypeInNamespace  id (parentCompPath cpath) mtyp))
        |> snd
        
let BuildRootModuleExpr enclosingNamespacePath cpath mexpr = 
    (enclosingNamespacePath,(cpath, mexpr)) 
        ||> List.foldBack (fun id (cpath, mexpr) -> (parentCompPath cpath, wrapModuleOrNamespaceExprInNamespace id (parentCompPath cpath) mexpr))
        |> snd

let ImplicitlyOpenOwnNamespace g amap scopem enclosingNamespacePath env = 
    if isNil enclosingNamespacePath then 
        env
    else
        let ad = AccessRightsOfEnv env
        match ResolveLongIndentAsModuleOrNamespace OpenQualified env.eNameResEnv ad enclosingNamespacePath with 
        | Result modrefs -> OpenModulesOrNamespaces g amap scopem env modrefs 
        | Exception _ ->  env


//-------------------------------------------------------------------------
// Helpers for unification
//------------------------------------------------------------------------- 


/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyTupleType cenv denv m ty ps = 
    let ptys = 
        if isTupleTy cenv.g ty then 
            let ptys = destTupleTy cenv.g ty
            if (List.length ps) = (List.length ptys) then ptys 
            else NewInferenceTypes ps
        else NewInferenceTypes ps
    AddCxTypeEqualsType denv cenv.css m ty (TType_tuple ptys);
    ptys

/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyFunctionTypeUndoIfFailed cenv denv m ty =
    if isFunTy cenv.g ty then Some(destFunTy cenv.g ty) else
    let domainTy = NewInferenceType ()
    let resultTy = NewInferenceType ()
    if AddCxTypeEqualsTypeUndoIfFailed denv cenv.css m ty (domainTy --> resultTy) then 
        Some(domainTy,resultTy)
    else 
        None

/// Optimized unification routine that avoids creating new inference 
/// variables unnecessarily
let UnifyFunctionType extraInfo cenv denv funm ty =
    match UnifyFunctionTypeUndoIfFailed cenv denv funm ty with
    | Some res -> res
    | None -> 
        match extraInfo with 
        | Some argm -> error (NotAFunction(denv,ty,funm,argm))
        | None ->    error (FunctionExpected(denv,ty,funm))


let UnifyUnitType cenv denv m ty exprOpt =
    if not (AddCxTypeEqualsTypeUndoIfFailed denv cenv.css m ty cenv.g.unit_ty) then 
        let domainTy = NewInferenceType ()
        let resultTy = NewInferenceType ()
        if AddCxTypeEqualsTypeUndoIfFailed denv cenv.css m ty (domainTy --> resultTy) then 
            warning (FunctionValueUnexpected(denv,ty,m))
        else
            let perhapsProp = 
                typeEquiv cenv.g cenv.g.bool_ty ty &&
                match exprOpt with 
                | Some(Expr.App(Expr.Val(vf,_,_),_,_,[_;_],_)) when vf.LogicalName = opNameEquals -> true
                | _ -> false
            warning (UnitTypeExpected (denv,ty,perhapsProp,m)); 
        false
    else
        true

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


/// Typecheck constant terms in expressions and patterns
let TcConst cenv ty m env c =
    let rec TcMeasure ms =
        match ms with
        | SynMeasure.One -> MeasureOne      
        | SynMeasure.Named(tc,m) ->
            let ad = AccessRightsOfEnv env
            let tcref = ForceRaise(ResolveTypeLongIdent cenv.nameResolver ItemOccurence.Use OpenQualified GenerateEstTypeFlag.No env.eNameResEnv ad tc 0)
            match tcref.TypeOrMeasureKind with
            | KindType -> error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m))
            | KindMeasure -> MeasureCon tcref

        | SynMeasure.Power(ms, exponent, _) -> MeasurePower (TcMeasure ms) exponent
        | SynMeasure.Product(ms1,ms2,_) -> MeasureProd(TcMeasure ms1, TcMeasure ms2)     
        | SynMeasure.Divide(ms1, ((SynMeasure.Seq (_::(_::_), _)) as ms2), m) -> 
            warning(Error(FSComp.SR.tcImplicitMeasureFollowingSlash(),m));
            MeasureProd(TcMeasure ms1, MeasureInv (TcMeasure ms2))
        | SynMeasure.Divide(ms1,ms2,_) -> 
            MeasureProd(TcMeasure ms1, MeasureInv (TcMeasure ms2))
        | SynMeasure.Seq(mss,_) -> ProdMeasures (List.map TcMeasure mss)
        | SynMeasure.Anon _ -> error(Error(FSComp.SR.tcUnexpectedMeasureAnon(),m))
        | SynMeasure.Var(_,m) -> error(Error(FSComp.SR.tcNonZeroConstantCannotHaveGenericUnit(),m))
   
    let unif ty2 = UnifyTypes cenv env m ty ty2
    let unif_measure_arg iszero tcr c =
        let measureTy = 
            match c with 
            | SynConst.Measure(_, SynMeasure.Anon _) ->
              (mkAppTy tcr [TType_measure (MeasureVar (NewAnonTypar (KindMeasure,m,TyparAnon,(if iszero then NoStaticReq else HeadTypeStaticReq),NoDynamicReq)))])

            | SynConst.Measure(_, ms) -> TType_app(tcr, [TType_measure (TcMeasure ms)])
            | _ -> TType_app(tcr, [TType_measure MeasureOne])
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
    if isFunTy g ty && typeEquiv g g.unit_ty (domainOfFunTy g ty) && argsData.Length = 1 && argsData.Head.Length = 1  then 
        SynValInfo(argsData.Head.Tail :: argsData.Tail, retData)
    else 
        sigMD 

/// The ValReprInfo for a value, except the number of typars is not yet inferred 
type PartialValReprInfo = PartialValReprInfo of ArgReprInfo list list * ArgReprInfo 

let TranslateTopArgSynInfo isArg m tcAttribute (SynArgInfo(attrs,isOpt,nm)) = 
    // Synthesize an artificial "OptionalArgument" attribute for the parameter
    let optAttrs = if isOpt then [ {TypeName=pathToSynLid m ["Microsoft";"FSharp";"Core";"OptionalArgument"]; ArgExpr=mkSynUnit m; Target=None; AppliesToGetterAndSetter=false; Range=m} ] else []

    if isArg && nonNil attrs && isNone nm then 
        errorR(Error(FSComp.SR.tcParameterRequiresName(),m));

    if not isArg && isSome nm then 
        errorR(Error(FSComp.SR.tcReturnValuesCannotHaveNames(),m));
       
    // Call the attribute checking function 
    let attribs = tcAttribute (optAttrs@attrs)
    ({ Attribs = attribs; Name = nm } : ArgReprInfo)

/// Members have an arity inferred from their syntax. This "valSynData" is not quite the same as the arities 
/// used in the middle and backends of the compiler ("topValInfo"). 
/// "0" in a valSynData (see Ast.arity_of_pat) means a "unit" arg in a topValInfo 
/// Hence remove all "zeros" from arity and replace them with 1 here. 
/// Note we currently use the compiled form for choosing unique names, to distinguish overloads because this must match up 
/// between signature and implementation, and the signature just has "unit". 
let TranslateTopValSynInfo m tcAttribute (SynValInfo(argsData,retData)) = 
    PartialValReprInfo (argsData |> List.mapSquared (TranslateTopArgSynInfo true m (tcAttribute AttributeTargets.Parameter)), 
                       retData |> TranslateTopArgSynInfo false m (tcAttribute AttributeTargets.ReturnValue))

let TranslatePartialArity tps (PartialValReprInfo (argsData,retData)) = 
    ValReprInfo(ValReprInfo.InferTyparInfo tps,argsData,retData)


//-------------------------------------------------------------------------
// Members
//------------------------------------------------------------------------- 

let ComputeLogicalName (id:Ident) memberFlags = 
    match memberFlags.MemberKind with 
    | MemberKind.ClassConstructor -> ".cctor"
    | MemberKind.Constructor -> ".ctor"
    | MemberKind.Member -> id.idText 
    | MemberKind.PropertyGetSet ->  error(InternalError(FSComp.SR.tcMemberKindPropertyGetSetNotExpected(),id.idRange))
    | MemberKind.PropertyGet ->  "get_"^id.idText
    | MemberKind.PropertySet ->  "set_"^id.idText 

type ValMemberInfoTransient = ValMemberInfoTransient of ValMemberInfo * string * string 


/// Make the unique "name" for a member.
//
// optImplSlotTy = None (for classes) or Some ty (when implementing interface type ty) 
let MakeMemberDataAndMangledNameForMemberVal(g,tcref,isExtrinsic,attrs,optImplSlotTys,memberFlags,valSynData,id,isCompGen)  =
    let logicalName = ComputeLogicalName id memberFlags
    let optIntfSlotTys = if optImplSlotTys |> List.forall (isInterfaceTy g) then optImplSlotTys else []
    let memberInfo = 
        { ApparentParent=tcref; 
          MemberFlags=memberFlags; 
          IsImplemented=false;
          // NOTE: This value is initially only set for interface implementations and those overrides 
          // where we manage to pre-infer which abstract is overriden by the method. It is filled in  
          // properly when we check the allImplemented implementation checks at the end of the inference scope. 
          ImplementedSlotSigs=optImplSlotTys |> List.map (fun ity -> TSlotSig(logicalName,ity,[],[],[],None)) }
    let isInstance = MemberIsCompiledAsInstance g tcref isExtrinsic memberInfo attrs
    if (memberFlags.IsDispatchSlot || nonNil optIntfSlotTys) then 
        if not isInstance then
          errorR(VirtualAugmentationOnNullValuedType(id.idRange));
    elif not memberFlags.IsOverrideOrExplicitImpl && memberFlags.IsInstance then 
        if not isExtrinsic && not isInstance then
            warning(NonVirtualAugmentationOnNullValuedType(id.idRange))

    let compiledName = 
        if isExtrinsic then 
             let tname = tcref.LogicalName
             let text = tname^"."^logicalName
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
                if n<>2               then warning(Error(FSComp.SR.memberOperatorDefinitionWithNonPairArgument(name,n),m))
                if otherArgs.Length>0 then warning(Error(FSComp.SR.memberOperatorDefinitionWithCurriedArguments(name),m))

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
let permitInferTypars = ExplicitTyparInfo ([],[],true) 
let dontInferTypars = ExplicitTyparInfo ([],[],false) 

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
    | ClassLetBinding 
    | ObjectExpressionOverrideBinding
    | ExpressionBinding 

    static member MustHaveArity x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding -> false
        | ObjectExpressionOverrideBinding -> false
        | ExpressionBinding -> false

    static member IsAccessModifierPermitted  x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding 
        | ObjectExpressionOverrideBinding 
        | ExpressionBinding -> false

    static member ImplicitlyStatic  x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding -> false
        | ObjectExpressionOverrideBinding -> false
        | ExpressionBinding -> false

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
        | ClassLetBinding -> AttributeTargets.Field ||| AttributeTargets.Method
        | ExpressionBinding -> enum 0 // indicates attributes not allowed on expression 'let' bindings

    // Note: now always true
    static member CanGeneralizeConstrainedTypars  x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding -> true
        | ObjectExpressionOverrideBinding -> true
        | ExpressionBinding -> true
        
    static member ConvertToLinearBindings  x = 
        match x with 
        | ModuleOrMemberBinding -> true
        | IntrinsicExtensionBinding  -> true
        | ExtrinsicExtensionBinding -> true
        | ClassLetBinding -> true
        | ObjectExpressionOverrideBinding -> true
        | ExpressionBinding -> false 

    static member CanOverrideOrImplement x = 
        match x with 
        | ModuleOrMemberBinding -> OverridesOK
        | IntrinsicExtensionBinding -> WarnOnOverrides
        | ExtrinsicExtensionBinding -> ErrorOnOverrides
        | ClassLetBinding -> ErrorOnOverrides 
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
        ValInlineInfo * 
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
        ValInlineInfo * 
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
        ValInlineInfo * 
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
    TcPatPhase2Input of (Val * TypeScheme) NameMap


/// The first phase of checking and elaborating a binding leaves a whole goop of information. 
/// This is a bit unpleasant: much of this information is carried on a per-value basis by the 
/// "PrelimValScheme1 NameMap". 
type CheckedBindingInfo = 
    CheckedBindingInfo of 
       ValInlineInfo * 
       bool *   (* immutable? *)
       Tast.Attribs * 
       XmlDoc * 
       (TcPatPhase2Input -> Patcompile.Pattern) * 
       ExplicitTyparInfo * 
       NameMap<PrelimValScheme1> * 
       Expr * 
       ArgAndRetAttribs * 
       TType * 
       range *
       SequencePointInfoForBinding * 
       bool * (* compiler generated? *)
       Const option (* literal value? *)

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
        modul.Data.entity_modul_contents <- notlazy (f true modul.ModuleOrNamespaceType);
    SetCurrAccumulatedModuleOrNamespaceType env (f false (GetCurrAccumulatedModuleOrNamespaceType env))
  
let PublishModuleDefn cenv env mspec = 
    UpdateAccModuleOrNamespaceType cenv env (fun intoFslibCcu mty -> 
       if intoFslibCcu then mty
       else mty.AddEntity(mspec))
    CallNameResolutionSink(mspec.Range,env.NameEnv,Item.ModuleOrNamespaces([mkLocalModRef mspec]),ItemOccurence.Binding,env.DisplayEnv,AccessRightsOfEnv env)

let PublishTypeDefn cenv env tycon = 
    UpdateAccModuleOrNamespaceType cenv env (fun _ mty -> 
       mty.AddEntity(tycon))

let PublishValueDefnPrim cenv env (vspec:Val) = 
    UpdateAccModuleOrNamespaceType cenv env (fun _ mty -> 
        mty.AddVal(vspec))

let PublishValueDefn cenv env declKind (vspec:Val) =
    if (declKind = ModuleOrMemberBinding) && 
       ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace) && 
       (isNone vspec.MemberInfo) then 
           errorR(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),vspec.Range));

    if (declKind = ExtrinsicExtensionBinding) && 
       ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace) then 
           errorR(Error(FSComp.SR.tcNamespaceCannotContainExtensionMembers(),vspec.Range));

    // Publish the value to the module type being generated. 
    if (match declKind with 
        | ModuleOrMemberBinding -> true
        | ExtrinsicExtensionBinding -> true
        | IntrinsicExtensionBinding -> true
        | _ -> false) then 
        PublishValueDefnPrim cenv env vspec

    match vspec.MemberInfo with 
    | Some _memberInfo when 
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
   if isSome vis1 && isSome vis2 then 
        errorR(Error(FSComp.SR.tcMultipleVisibilityAttributes(),m));
   if isSome vis1 then vis1 else vis2

let ComputeAccessAndCompPath env declKindOpt m vis actualParent = 
    let accessPath = curr_access_cpath env
    let accessModPermitted = 
        match declKindOpt with 
        | None -> true
        | Some declKind -> DeclKind.IsAccessModifierPermitted declKind

    if isSome vis && not accessModPermitted then 
        errorR(Error(FSComp.SR.tcMultipleVisibilityAttributesWithLet(),m)); 
    let vis = 
        match vis with 
        | None -> taccessPublic (* a module or member binding defaults to "public" *)
        | Some SynAccess.Public -> taccessPublic
        | Some SynAccess.Private -> TAccess [accessPath]
        | Some SynAccess.Internal -> 
            let (CompPath(scoref,_)) = accessPath
            TAccess [CompPath(scoref,[])]
            (* errorR(InternalError(FSComp.SR.tcUnrecognizedAccessibilitySpec(),m)); *)

    let vis = 
        match actualParent with 
        | ParentNone -> vis 
        | Parent tcref -> 
             combineAccess vis tcref.Accessibility
    let cpath = env.eCompPath
    let cpath = (if accessModPermitted then Some cpath else None)
    vis,cpath 

let CheckForAbnormalOperatorNames cenv (idRange:range) opName isMember =
    

    if (idRange.EndColumn - idRange.StartColumn <= 5) && not cenv.g.compilingFslib  then 
        
        match opName, isMember with 
        | PrettyNaming.Relational   ,true  -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMethodNameForRelationalOperator(opName, (CompileOpName opName)),idRange))
        | PrettyNaming.Equality ,true  -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMethodNameForEquality(opName, (CompileOpName opName)),idRange))
        | PrettyNaming.Control,true  -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMemberName(opName, (CompileOpName opName)),idRange))
        | PrettyNaming.FixedTypes,true  -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidMemberNameFixedTypes(opName),idRange))
        | PrettyNaming.Indexer,true  -> ()
        | PrettyNaming.Relational  ,false -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidOperatorDefinitionRelational(opName),idRange))
        | PrettyNaming.Equality ,false -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidOperatorDefinitionEquality(opName),idRange))
        | PrettyNaming.Control,false -> warning(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidOperatorDefinition(opName),idRange))
        | PrettyNaming.Indexer,false -> error(StandardOperatorRedefinitionWarning(FSComp.SR.tcInvalidIndexOperatorDefinition(opName),idRange))
        | PrettyNaming.FixedTypes,_ -> ()
        | PrettyNaming.Other,_ -> ()

let MakeAndPublishVal cenv env (altActualParent,inSig,declKind,vrec,(ValScheme(id,typeScheme,topValData,memberInfoOpt,isMutable,inlineFlag,baseOrThis,vis,compgen,isIncrClass,isTyFunc,hasDeclaredTypars)),attrs,doc,konst) =
    let ty = GeneralizedTypeForTypeScheme typeScheme
    let m = id.idRange

    let isTopBinding = 
        match declKind with 
        | ModuleOrMemberBinding -> true
        | ExtrinsicExtensionBinding -> true
        | IntrinsicExtensionBinding -> true
        | _ -> false

    let isExtrinsic = (declKind=ExtrinsicExtensionBinding)
    let actualParent = 
        // Use the parent of the member if it's available 
        // If it's an extrinsic extension member or not a member then use the containing module. 
        match memberInfoOpt with 
        | Some (ValMemberInfoTransient(memberInfo,_,_)) when not isExtrinsic -> 
            if memberInfo.ApparentParent.IsModuleOrNamespace then 
                errorR(InternalError(FSComp.SR.tcExpectModuleOrNamespaceParent(id.idText),m));

            Parent(memberInfo.ApparentParent)
        | _ -> altActualParent
             
    let vis,_ = ComputeAccessAndCompPath env (Some declKind) id.idRange vis actualParent

    let inlineFlag = 
        if HasAttrib cenv.g cenv.g.attrib_DllImportAttribute attrs then begin 
            if inlineFlag = PseudoValue || inlineFlag = AlwaysInline then 
              errorR(Error(FSComp.SR.tcDllImportStubsCannotBeInlined(),m)); 
            NeverInline 
        end else 
            let implflags = 
                match TryFindAttrib cenv.g cenv.g.attrib_MethodImplAttribute attrs with
                | Some (Attrib(_,_,[ AttribInt32Arg flags ],_,_,_))  -> flags
                | _ -> 0x0
            // MethodImplOptions.NoInlining = 0x8
            let NO_INLINING = 0x8
            if (implflags &&& NO_INLINING) <> 0x0 then
                NeverInline
            else
                inlineFlag

    // CompiledName not allowed on virtual/abstract/override members        
    let compiledNameAttrib  = TryFindStringAttrib cenv.g cenv.g.attrib_CompiledNameAttribute attrs
    if isSome compiledNameAttrib && (   (   match memberInfoOpt with 
                                            | Some (ValMemberInfoTransient(memberInfo,_,_)) -> 
                                                memberInfo.MemberFlags.IsDispatchSlot 
                                                || memberInfo.MemberFlags.IsOverrideOrExplicitImpl 
                                            | None -> false)
                                     || (match altActualParent with ParentNone -> true | _ -> false)) then 
        errorR(Error(FSComp.SR.tcCompiledNameAttributeMisused(),m))

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
                      compgen,topValData,vis,vrec,memberInfoOpt,baseOrThis,attrs,inlineFlag,doc, isTopBinding, isExtrinsic,isIncrClass,isTyFunc,
                      (hasDeclaredTypars || inSig), konst,actualParent)

    
    CheckForAbnormalOperatorNames cenv id.idRange (DecompileOpName vspec.CoreDisplayName) (isSome memberInfoOpt)

    PublishValueDefn cenv env declKind vspec;

    begin 
        match !GlobalTypecheckResultsSink with 
        | None -> ()
        | Some _ -> 
            if not vspec.IsCompilerGenerated && not (String.hasPrefix vspec.LogicalName "_") then 
                let nenv = AddFakeNamedValRefToNameEnv vspec.DisplayName (env.NameEnv) (mkLocalValRef vspec) 
                CallEnvSink(vspec.Range,nenv,AccessRightsOfEnv env)
                CallNameResolutionSink(vspec.Range,nenv,Item.Value(mkLocalValRef vspec),ItemOccurence.Binding,nenv.eDisplayEnv,AccessRightsOfEnv env);
    end;

    vspec

let MakeAndPublishVals cenv env (altActualParent,inSig,declKind,vrec,valSchemes,attrs,doc,konst) =
    Map.foldBack
        (fun name (valscheme:ValScheme) values -> 
          Map.add name (MakeAndPublishVal cenv env (altActualParent,inSig,declKind,vrec,valscheme,attrs,doc,konst), valscheme.TypeScheme) values)
        valSchemes
        Map.empty

let MakeAndPublishBaseVal cenv env baseIdOpt ty = 
    baseIdOpt |> Option.map (fun (id:Ident) ->
       let valscheme = ValScheme(id,NonGenericTypeScheme(ty),None,None,false,NeverInline,BaseVal,None,false,false,false,false)
       MakeAndPublishVal cenv env (ParentNone,false,ExpressionBinding,ValNotInRecScope,valscheme,[],XmlDoc.Empty,None))

let InstanceMembersNeedSafeInitCheck cenv m thisTy = 
    ExistsInEntireHierarchyOfType 
        (fun ty -> isAppTy cenv.g ty && (tcrefOfAppTy cenv.g ty).HasSelfReferentialConstructor)
        cenv.g 
        cenv.amap
        m 
        AllowMultiIntfInst
        thisTy
        
let MakeSafeInitField g env m isStatic = 
    let id = ident(globalNng.FreshCompilerGeneratedName("init",m),m)
    let taccess = TAccess [curr_access_cpath env]
    NewRecdField isStatic None id g.int_ty true true [] [] XmlDoc.Empty taccess true

// Make the "delayed reference" boolean value recording the safe initialization of a type in a hierarchy where there is a HasSelfReferentialConstructor
let ComputeInstanceSafeInitInfo cenv env m  thisTy = 
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
            errorR(Error(FSComp.SR.tcStructsCanOnlyBindThisAtMemberDeclaration(),thisId.idRange));

        let valScheme = ValScheme(thisId,NonGenericTypeScheme(mkRefCellTy cenv.g thisTy),None,None,false,NeverInline,CtorThisVal,None,false,false,false,false)
        Some(MakeAndPublishVal cenv env (ParentNone, false, ExpressionBinding, ValNotInRecScope, valScheme, [], XmlDoc.Empty, None))

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
    if nonNil(generalizedTypars) then begin
        // Find all the uses of this recursive binding and use mutation to adjust the expressions 
        // at those points in order to record the inferred type parameters. 
        let recUses = cenv.recUses.Find lvrefTgt
        recUses |> List.iter  (fun (fixupPoint,m,isComplete) -> 
          if not isComplete then 
              // Keep any values for explicit type arguments 
              let fixedUpExpr = 
                  let vrefFlags,tyargs0 = 
                      match !fixupPoint with 
                      | Expr.App(Expr.Val (_,vrefFlags,_),_,tyargs0,[],_) -> vrefFlags,tyargs0
                      | Expr.Val(_,vrefFlags,_) -> vrefFlags,[] 
                      | _ -> 
                          errorR(Error(FSComp.SR.tcUnexpectedExprAtRecInfPoint(),m)); 
                          NormalValUse,[]
                  
                  let ityargs = generalizeTypars (List.drop (List.length tyargs0) generalizedTypars)
                  primMkApp (Expr.Val (vrefTgt,vrefFlags,m),fty) (tyargs0 @ ityargs) [] m
              fixupPoint :=   fixedUpExpr)
    end;
    cenv.recUses <- cenv.recUses.Remove lvrefTgt 
     

/// Set the properties of recursive values that are only fully known after inference is complete 
let AdjustRecType _cenv (vspec:Val) (ValScheme(_,typeScheme,topValData,_,_,_,_,_,_,_,_,_)) =
    let fty = GeneralizedTypeForTypeScheme typeScheme
    vspec.SetType fty;
    vspec.SetValReprInfo topValData;
    vspec.SetValRec ValNotInRecScope
       
/// Record the generated value expression as a place where we will have to 
/// adjust using AdjustAndForgetUsesOfRecValue at a letrec point. Every use of a value 
/// under a letrec gets used at the _same_ type instantiation. 
let RecordUseOfRecValue cenv vrec (vrefTgt: ValRef) vexp m = 
    match vrec with 
    | ValInRecScope isComplete -> 
        let fixupPoint = ref vexp
        cenv.recUses <- cenv.recUses.Add (vrefTgt.Deref, (fixupPoint,m,isComplete)) ;
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
          error(Error(FSComp.SR.tcLessGenericBecauseOfAnnotation(tp.Name,NicePrint.prettyStringOfTy denv ty),tp.Range)));
    
    let declaredTypars = NormalizeDeclaredTyparsForEquiRecursiveInference g declaredTypars

    if (ListSet.setify typarEq declaredTypars).Length <> declaredTypars.Length then 
        errorR(Error(FSComp.SR.tcConstrainedTypeVariableCannotBeGeneralized(),m));

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
    tp.SetRigidity TyparRigid

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
        let ftps = (freeInTypeLeftToRight cenv.g thruFlag ty)
        let generalizedTypars = generalizedTyparsForThisBinding |> List.filter (fun tp -> ListSet.contains typarEq tp ftps)
        (* Put declared typars first *)
        let generalizedTypars = PlaceTyparsInDeclarationOrder allDeclaredTypars generalizedTypars  
        generalizedTypars

    let generalizedTypars = computeRelevantTypars false

    // Check stability of existence and ordering of type parameters under erasure of type abbreviations
    let generalizedTyparsLookingThroughTypeAbbreviations = computeRelevantTypars true
    if not (generalizedTypars.Length = generalizedTyparsLookingThroughTypeAbbreviations.Length && 
            List.forall2 typarEq generalizedTypars generalizedTyparsLookingThroughTypeAbbreviations) then
        warning(Error(FSComp.SR.tcTypeParametersInferredAreNotStable(),m));

    let hasDeclaredTypars = nonNil declaredTypars
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

/// Combine the results of InferValSynData and InferArityOfExpr. 
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
// Some of this arity inference is purely syntax directed and done in InferValSynData in ast.fs
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
    
let MakeAndPublishSimpleVals cenv env m names =
    let values,vspecMap   = MakeSimpleVals cenv env names
    let envinner   = AddLocalValMap m vspecMap env
    envinner,values,vspecMap



//-------------------------------------------------------------------------
// Helpers to freshen existing types and values, i.e. when a reference
// to C<_> occurs then generate C<?ty> for a fresh type inference variable ?ty.
//------------------------------------------------------------------------- 
   
let FreshenTyconRef m rigid (tcref:TyconRef) declaredTyconTypars = 
    let tpsorig = declaredTyconTypars
    let tps = copyTypars tpsorig
    if rigid <> TyparRigid then 
      tps |> List.iter (fun tp -> tp.SetRigidity rigid);  
        
    let renaming,tinst = FixupNewTypars m [] [] tpsorig tps
    TType_app(tcref,List.map mkTyparTy tpsorig),tps,renaming,TType_app(tcref,tinst)
    
let FreshenPossibleForallTy g m rigid ty = 
    let tpsorig,tau =  tryDestForallTy g ty
    if isNil tpsorig then [],[],tau
    else
        // tps may be have been equated to other tps in equi-recursive type inference and units-of-measure type inference. Normalize them here 
        let tpsorig = NormalizeDeclaredTyparsForEquiRecursiveInference g tpsorig
        let tps,renaming,tinst = CopyAndFixupTypars m rigid tpsorig
        tps,tinst,instType renaming tau

let info_of_tcref m (tcref:TyconRef) = 
    let tps,renaming,tinst = FreshenTypeInst m (tcref.Typars(m))
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
            if nonNil synTypars && infer then errorR(Error(FSComp.SR.tcOverridingMethodRequiresAllOrNoTypeParameters(),m));
            isNil synTypars
            
    let argtys,retTy,fmtps,_ = CompiledSigOfMeth g amap m absMethInfo
    
    // If the virual method is a generic method then copy its type parameters 
    let typarsFromAbsSlot,typarInstFromAbsSlot,_ = 
        let ttps = (FormalTyparsOfEnclosingTypeOfMethInfo m absMethInfo)
        let ttinst = argsOfAppTy g absMethInfo.EnclosingType
        let rigid = (if typarsFromAbsSlotAreRigid then TyparRigid else TyparFlexible)
        ConstraintSolver.FreshenAndFixupTypars m rigid ttps ttinst fmtps

    // Work out the required type of the member 
    let argTysFromAbsSlot = argtys |> List.mapSquared (instType typarInstFromAbsSlot) 
    let retTyFromAbsSlot = retTy  |> GetFSharpViewOfReturnType g |> instType typarInstFromAbsSlot 
    typarsFromAbsSlotAreRigid,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot


//-------------------------------------------------------------------------
// Helpers to typecheck expressions and patterns
//------------------------------------------------------------------------- 

let BuildFieldMap cenv env isPartial ty flds m = 
    let ad = AccessRightsOfEnv env
    if isNil flds then invalidArg "flds" "BuildFieldMap";
   
    let frefSets = 
        flds |> List.map (fun (fld,fldExpr) -> 
            let frefSet = ResolveField cenv.nameResolver env.eNameResEnv ad ty fld
            fld,frefSet, fldExpr)
    let relevantTypeSets = 
        frefSets |> List.map (fun (_,frefSet,_) -> frefSet |> List.choose (fun rfref -> Some rfref.TyconRef))
    
    let tcref = 
        match List.fold (ListSet.intersect (tyconRefEq cenv.g)) (List.head relevantTypeSets) (List.tail relevantTypeSets) with
        | [tcref] -> tcref
        | _ -> 
            if isPartial then 
                warning (Error(FSComp.SR.tcFieldsDoNotDetermineUniqueRecordType(),m));
            // OK, there isn't a unique type dictated by the intersection for the field refs. 
            // We're going to get an error of some kind below. 
            // Just choose one field ref and let the error come later 
            let (_,frefSet1,_) = List.head frefSets
            let fref1 = List.head frefSet1
            fref1.TyconRef
    
    let fldsmap,rfldsList = 
        ((Map.empty,[]), frefSets) ||> List.fold (fun (fs,rfldsList) (fld,frefs,fldExpr) -> 
                match frefs |> List.filter (fun fref2 -> tyconRefEq cenv.g tcref fref2.TyconRef) with
                | [fref2] -> 

                    // Record the precise resolution of the field for intellisense
                    CallNameResolutionSink(snd(fld).idRange,env.NameEnv,FreshenRecdFieldRef cenv.nameResolver m fref2,ItemOccurence.Use,env.DisplayEnv,ad)

                    CheckRecdFieldAccessible m (AccessRightsOfEnv env) fref2 |> ignore;
                    CheckFSharpAttributes cenv.g fref2.PropertyAttribs m |> CommitOperationResult;        
                    if  Map.containsKey fref2.FieldName fs then 
                        errorR (Error(FSComp.SR.tcFieldAppearsTwiceInRecord(fref2.FieldName),m));
                    if  not (tyconRefEq cenv.g tcref fref2.TyconRef) then 
                        let (_,frefSet1,_) = List.head frefSets
                        let fref1 = List.head frefSet1
                        errorR (FieldsFromDifferentTypes(env.DisplayEnv,fref1,fref2,m));
                        (fs,rfldsList)
                    else (Map.add fref2.FieldName fldExpr fs,
                          (fref2.FieldName,fldExpr)::rfldsList)
                | _ -> error(Error(FSComp.SR.tcRecordFieldInconsistentTypes(),m)))
    tcref,fldsmap,List.rev rfldsList

let rec genConstrUnify (mk_ucasef,mk_exnconstrf) m cenv env ty item =
    let ad = AccessRightsOfEnv env
    match item with 
    | Item.ExnCase ecref -> 
        CheckEntityAttributes cenv.g ecref m  |> CommitOperationResult;
        UnifyTypes cenv env m ty cenv.g.exn_ty;
        CheckTyconAccessible m ad ecref |> ignore;
        let mkf = mk_exnconstrf(ecref)
        mkf,recdFieldTysOfExnDefRef ecref

    | Item.UnionCase ucinfo ->   
        let ucref = ucinfo.UnionCaseRef 
        CheckUnionCaseAttributes cenv.g ucref m  |> CommitOperationResult;
        CheckUnionCaseAccessible m ad ucref |> ignore;
        let gtyp2 = actualResultTyOfUnionCase ucinfo.TypeInst ucref 
        let inst = mkTyparInst ucref.TyconRef.TyparsNoRange ucinfo.TypeInst
        UnifyTypes cenv env m ty gtyp2;
        let mkf = mk_ucasef(ucref,ucinfo.TypeInst)
        mkf,actualTysOfUnionCaseFields inst ucref 
    | _ -> invalidArg "item" "not a union case or exception reference"

let ApplyUnionCaseOrExnTypes m cenv env ty c = 
  genConstrUnify ((fun (a,b) args -> mkUnionCaseExpr(a,b,args,m)),
                  (fun a args -> mkExnExpr (a,args,m))) m cenv env ty c
      
let ApplyUnionCaseOrExnTypesForPat  m cenv env ty c = 
  genConstrUnify ((fun (a,b) args -> TPat_unioncase(a,b,args,m)),
                  (fun a args -> TPat_exnconstr(a,args,m))) m cenv env ty c

let UnionCaseOrExnCheck (env: TcEnv) nargtys nargs m =
  if nargs <> nargtys then error (UnionCaseWrongArguments(env.DisplayEnv,nargtys,nargs,m))

let TcUnionCaseField cenv (env: TcEnv) ty1 m c n funcs =
    let ad = AccessRightsOfEnv env
    let mkf,argtys = 
      match ResolvePatternLongIdent cenv.nameResolver AllIdsOK false m ad env.eNameResEnv DefaultTypeNameResInfo c with
      | (Item.UnionCase _ | Item.ExnCase _) as item ->
        genConstrUnify funcs m cenv env ty1 item
      | _ -> error(Error(FSComp.SR.tcUnknownUnion(),m))
    if n >= List.length argtys then 
      error (UnionCaseWrongNumberOfArgs(env.DisplayEnv,List.length argtys,n,m));
    let ty2 = List.nth argtys n
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
            | TOp.Tuple  -> true
            | TOp.UnionCase uc -> not (isUnionCaseAllocObservable uc)
            | TOp.Recd(ctorInfo,tcref) -> 
                match ctorInfo with 
                | RecdExpr ->  not (isTyconRefAllocObservable tcref)
                | RecdExprIsObjInit -> false
            | TOp.Array ->  isNil args
            | TOp.ExnConstr ec -> not (isExnAllocObservable ec)

            | TOp.ILAsm([],_) -> true

            | _ -> false
            && List.forall (IsGeneralizableValue g) args

        | Expr.LetRec(binds,body,_,_)  ->
            binds |> FlatList.forall (fun b -> IsGeneralizableValue g b.Expr) &&
            IsGeneralizableValue g body
        | Expr.Let(bind,body,_,_) -> 
            IsGeneralizableValue g bind.Expr &&
            IsGeneralizableValue g body


        // Applications of type functions are _not_ normally generalizable unless explicitly marked so 
        | Expr.App(Expr.Val (vref,_,_),_,_,[],_) when vref.IsTypeFunction -> 
            HasAttrib g g.attrib_GeneralizableValueAttribute vref.Attribs
             
        
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
            if inlineFlag = PseudoValue then generalizedTypars,[]
            else generalizedTypars |> List.partition (fun tp -> tp.StaticReq = NoStaticReq) 

        // Do not generalize type variables which would escape their scope 
        // because they are free in the environment 
        let generalizedTypars,ungeneralizableTypars2 = 
            List.partition (fun x -> not (Zset.contains x freeInEnv)) generalizedTypars

        // Some situations, e.g. implicit class constructions that represent functions as fields, 
        // do not allow generalisation over constrained typars. (since they can not be represented as fields 
        let generalizedTypars,ungeneralizableTypars3 = 
            generalizedTypars |> List.partition (fun tp -> 
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
    let CondenseTypars (cenv, denv:DisplayEnv, generalizedTypars: Typars, tauTy) =

        // The type of the value is ty11 * ... * ty1N -> ... -> tyM1 * ... * tyMM -> retTy
        // This is computed REGARDLESS of the arity of the expression.
        let curriedArgTys,retTy = stripFunTy cenv.g tauTy
        let allUntupledArgTys = curriedArgTys |> List.collect (tryDestTupleTy cenv.g)

        // Compute the type variables in 'retTy'
        let returnTypeFreeTypars = freeInTypeLeftToRight cenv.g false retTy
        let allUntupledArgTysWithFreeVars = allUntupledArgTys |> List.map (fun ty -> (ty, freeInTypeLeftToRight cenv.g false ty))

        let relevantUniqueSubtypeConstraint (tp:Typar) = 
            // Find a single subtype constraint
            match tp.Constraints |> List.partition (function (TTyparCoercesToType _) -> true | _ -> false) with 
            | [TTyparCoercesToType(cxty,_)], others -> 
                 // Throw away null constraints if they are implied 
                 match others |> List.filter (function (TTyparSupportsNull(_)) -> not (TypeSatisfiesNullConstraint cenv.g cxty) | _ -> true) with
                 | [] -> Some cxty
                 | _ -> None
            | _ -> None
                 

        // Condensation typars can't be used in the constraints of any candidate condensation typars. So compute all the
        // typars free in the constraints of tyIJ

        let lhsConstraintTypars = 
            allUntupledArgTys |> List.collect (fun ty -> 
                if isTyparTy cenv.g ty then 
                    let tp = destTyparTy cenv.g ty 
                    match relevantUniqueSubtypeConstraint tp with 
                    | Some cxty -> freeInTypeLeftToRight cenv.g false cxty
                    | None -> []
                else [])

        let IsCondensationTypar (tp:Typar) = 
            // A condensation typar may not a user-generated type variable nor has it been unified with any user type variable
            (tp.DynamicReq = NoDynamicReq) && 
            // A condensation typar must have a single constraint "'a :> A"
            (isSome (relevantUniqueSubtypeConstraint tp)) &&
            // This is type variable is not used on the r.h.s. of the type
            not (ListSet.contains typarEq tp returnTypeFreeTypars) &&
            // A condensation typar can't be used in the constraints of any candidate condensation typars
            not (ListSet.contains typarEq tp lhsConstraintTypars) &&
            // A condensation typar must occur precisely once in tyIJ, and must not occur free in any other tyIJ
            (match allUntupledArgTysWithFreeVars |> List.partition (fun (ty,_) -> isTyparTy cenv.g ty && typarEq (destTyparTy cenv.g ty) tp) with
             | [_], rest -> not (rest |> List.exists (fun (_,fvs) -> ListSet.contains typarEq tp fvs))
             | _ -> false)
             
        let condensationTypars, generalizedTypars = generalizedTypars |> List.partition IsCondensationTypar

        // Condensation solves type variables eagerly and removes them from the generalization set 
        condensationTypars |> List.iter (fun tp -> 
            ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css denv tp);
        generalizedTypars

    let CanonicalizePartialInferenceProblem (cenv,denv,m) tps =
        // Canonicalize constraints prior to generalization 
        let csenv = (MakeConstraintSolverEnv cenv.css m denv)
        TryD (fun () -> ConstraintSolver.CanonicalizeRelevantMemberConstraints csenv 0 NoTrace tps)
             (fun res -> ErrorD (ErrorFromAddingConstraint(denv,res,m))) 
        |> RaiseOperationResult

    let ComputeAndGeneralizeGenericTypars (cenv,
                                           denv:DisplayEnv,
                                           m,
                                           immut,
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
            if immut && (match exprOpt with None -> true | Some e -> IsGeneralizableValue cenv.g e) 
            then (ListSet.unionFavourLeft typarEq  allDeclaredTypars maxInferredTypars)
            else allDeclaredTypars

        let generalizedTypars,freeInEnv = 
            TrimUngeneralizableTypars genConstrainedTyparFlag inlineFlag typarsToAttemptToGeneralize freeInEnv

        allDeclaredTypars |> List.iter (fun tp -> 
              if Zset.memberOf freeInEnv tp then
                let ty =  mkTyparTy tp
                error(Error(FSComp.SR.tcNotSufficientlyGenericBecauseOfScope(NicePrint.prettyStringOfTy denv ty),m)));
            
        let generalizedTypars = CondenseTypars(cenv,denv,generalizedTypars,tauTy)    

        let generalizedTypars =  
            if canInferTypars then generalizedTypars 
            else generalizedTypars |> List.filter (fun tp -> ListSet.contains typarEq tp allDeclaredTypars)

        let allConstraints = List.collect (fun (tp:Typar) -> tp.Constraints) generalizedTypars 
        let generalizedTypars = ConstraintSolver.SimplifyMeasuresInTypeScheme cenv.g resultFirst generalizedTypars tauTy allConstraints

        // Generalization turns inference type variables into rigid, quantified type variables,
        // (they may be rigid already)
        generalizedTypars |> List.iter (SetTyparRigid cenv.g denv m);
        
        // Generalization removes constraints related to generalized type variables
        let csenv = MakeConstraintSolverEnv cenv.css m denv
        EliminateConstraintsForGeneralizedTypars csenv NoTrace generalizedTypars;
        
        generalizedTypars

    //-------------------------------------------------------------------------
    // Helpers to freshen existing types and values, i.e. when a reference
    // to C<_> occurs then generate C<?ty> for a fresh type inference variable ?ty.
    //------------------------------------------------------------------------- 


    let private ComputeExtraTyparsFeasible(memFlagsOpt,declaredTypars,m) = 
        // Properties and Constructors may only generalize the variables associated with the containing class (retrieved from the 'this' pointer) 
        // Also check they don't declare explicit typars. 
        match memFlagsOpt with 
        | None -> true
        | Some(memberFlags) -> 
            match memberFlags.MemberKind with 
            // can't infer extra polymorphism for properties 
            | MemberKind.PropertyGet 
            | MemberKind.PropertySet  -> 
                 if nonNil(declaredTypars) then 
                     errorR(Error(FSComp.SR.tcPropertyRequiresExplicitTypeParameters(),m));
                 false
            // can't infer extra polymorphism for class constructors 
            | MemberKind.ClassConstructor ->  
                 false
            // can't infer extra polymorphism for constructors 
            | MemberKind.Constructor -> 
                 if nonNil(declaredTypars) then 
                     errorR(Error(FSComp.SR.tcConstructorCannotHaveTypeParameters(),m));
                 false 
            | _ -> 
                 // feasible to infer extra polymorphism 
                 true                     

    let ComputeCanInferTypars(parentRef,canInferTypars,memFlagsOpt,declaredTypars,m) =  
            (match parentRef with Parent tcref -> not tcref.IsFSharpDelegateTycon | _ -> true) &&  // no generic paramters inferred for 'Invoke' method
            ComputeExtraTyparsFeasible(memFlagsOpt,declaredTypars,m) &&
            canInferTypars


//-------------------------------------------------------------------------
// ComputeInlineFlag 
//-------------------------------------------------------------------------

let ComputeInlineFlag memFlagsOption pseudo isMutable _attrs = 
    // Mutable values may never be inlined 
    // Constructors may never be inlined 
    // Calls to virtual/abstract slots may never be inlined 
    if isMutable || 
       (match memFlagsOption with 
        | None -> false
        | Some x -> (x.MemberKind = MemberKind.Constructor) || x.IsDispatchSlot) 
    then NeverInline 
    elif pseudo then PseudoValue 
    else OptionalInline


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
// NOTE: This is a bit unpleasant.  In the early implementation of F# we decided 
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

let PushOnePatternToRhs isMember p (NormalizedBindingRhs(spatsL,rtyOpt,rhsExpr)) = 
    let spats,rhsExpr = PushPatternToExpr isMember p rhsExpr
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
    let private PushMultiplePatternsToRhs isMember ps (NormalizedBindingRhs(spatsL,rtyOpt,rhsExpr)) = 
        let spatsL2,rhsExpr = PushCurriedPatternsToExpr rhsExpr.Range isMember ps rhsExpr
        NormalizedBindingRhs(spatsL2@spatsL, rtyOpt, rhsExpr)


    let private MakeNormalizedStaticOrValBinding isObjExprBinding id vis typars args rhsExpr valSynData : NormalizedBindingPatternInfo = 
        let (SynValData(memberFlagsOpt,_,_)) = valSynData 
        NormalizedBindingPat(mkSynPatVar vis id, PushMultiplePatternsToRhs ((isObjExprBinding = ObjExprBinding) || isSome memberFlagsOpt) args rhsExpr,valSynData,typars)

    let private MakeNormalizedInstanceMemberBinding thisId memberId toolId vis m typars args rhsExpr valSynData = 
        NormalizedBindingPat(SynPat.InstanceMember(thisId,memberId,toolId,vis,m), PushMultiplePatternsToRhs true args rhsExpr,valSynData,typars)

    let private NormalizeStaticMemberBinding memberFlags valSynData id vis typars args m rhsExpr = 
        let (SynValData(_,valSynInfo,thisIdOpt)) = valSynData 
        if memberFlags.IsInstance then 
            // instance method without adhoc "this" argument 
            error(Error(FSComp.SR.tcInstanceMemberRequiresTarget(),m));
        match args, memberFlags.MemberKind  with 
        | _,MemberKind.PropertyGetSet    -> error(Error(FSComp.SR.tcUnexpectedPropertyInSyntaxTree(),m))
        | [],MemberKind.ClassConstructor -> error(Error(FSComp.SR.tcStaticInitializerRequiresArgument(),m))
        | [],MemberKind.Constructor     -> error(Error(FSComp.SR.tcObjectConstructorRequiresArgument(),m))
        | [_],MemberKind.ClassConstructor  
        | [_],MemberKind.Constructor  -> MakeNormalizedStaticOrValBinding ValOrMemberBinding id vis typars args rhsExpr valSynData
        // Static property declared using 'static member P = expr': transformed to a method taking a "unit" argument 
        // static property: these transformed into methods taking one "unit" argument 
        | [],MemberKind.Member -> 
            let memberFlags = {memberFlags with MemberKind = MemberKind.PropertyGet} 
            let valSynData = SynValData(Some(memberFlags),valSynInfo,thisIdOpt)
            NormalizedBindingPat(mkSynPatVar vis id,
                                 PushOnePatternToRhs true (SynPat.Const(SynConst.Unit,m)) rhsExpr,
                                 valSynData,
                                 typars)
        | _ -> MakeNormalizedStaticOrValBinding ValOrMemberBinding id vis typars args rhsExpr valSynData

    let private NormalizeInstanceMemberBinding memberFlags valSynData thisId memberId (toolId:Ident option) vis typars args m rhsExpr = 
        let (SynValData(_,valSynInfo,thisIdOpt)) = valSynData 
        if not memberFlags.IsInstance then 
            // static method with adhoc "this" argument 
            error(Error(FSComp.SR.tcStaticMemberShouldNotHaveThis(),m));
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
                 PushOnePatternToRhs true (SynPat.Const(SynConst.Unit,m)) rhsExpr,
                 // Update the member info to record that this is a MemberKind.PropertyGet 
                 SynValData(Some(memberFlags),valSynInfo,thisIdOpt),
                 typars)

        | _ -> MakeNormalizedInstanceMemberBinding thisId memberId toolId vis m typars args rhsExpr valSynData

    let private NormalizeBindingPattern nameResolver isObjExprBinding (env: TcEnv) valSynData pat rhsExpr =
        let ad = AccessRightsOfEnv env
        let (SynValData(memberFlagsOpt,_,_)) = valSynData 
        let rec normPattern pat = 
            // One major problem with versions of F# prior to 1.9.x was that data constructors easily 'pollute' the namespace 
            // of available items, to the point that you can't even define a function with the same name as an existing union case. 
            match pat with 
            | SynPat.LongIdent (lid, toolId, tyargs, args, vis, m) ->
                let typars = (match tyargs with None -> inferredTyparDecls | Some typars -> typars)
                match memberFlagsOpt with 
                | None ->                
                    match ResolvePatternLongIdent nameResolver AllIdsOK true m ad env.eNameResEnv DefaultTypeNameResInfo lid with
                    | Item.NewDef id -> 
                        if id.idText = opNameCons  then
                            NormalizedBindingPat(pat,rhsExpr,valSynData,typars)
                        else
                            if (isObjExprBinding = ObjExprBinding) then 
                                errorR(Deprecated(FSComp.SR.tcObjectExpressionFormDeprecated(),m))
                            MakeNormalizedStaticOrValBinding isObjExprBinding id vis typars args rhsExpr valSynData
                    | _ -> 
                        error(Error(FSComp.SR.tcInvalidDeclaration(),m))

                | Some memberFlags ->                
                    match lid with 
                    // x.Member in member binding patterns. 
                    | [thisId;memberId] -> NormalizeInstanceMemberBinding memberFlags valSynData thisId memberId toolId vis typars args m rhsExpr 
                    | [memberId]        -> NormalizeStaticMemberBinding memberFlags valSynData memberId vis typars args m rhsExpr 
                    | _                 -> NormalizedBindingPat(pat,rhsExpr,valSynData,typars)

            // Object constructors are normalized in TcLetrec 
            // Here we are normalizing member definitions with simple (not long) ids, 
            // e.g. "static member x = 3" and "member x = 3" (instance with missing "this." comes through here. It is trapped and generates a warning) 
            | SynPat.Named (SynPat.Wild _, id, false, vis, m) 
                when 
                   (match memberFlagsOpt with 
                    | None -> false 
                    | Some(memberFlags) -> 
                         not (memberFlags.MemberKind = MemberKind.Constructor) &&
                         not (memberFlags.MemberKind = MemberKind.ClassConstructor)) ->            
                NormalizeStaticMemberBinding (the memberFlagsOpt) valSynData id vis inferredTyparDecls [] m rhsExpr 

            | SynPat.Typed(pat',x,y) ->             
                let (NormalizedBindingPat(pat'',e'',valSynData,typars)) = normPattern pat'
                NormalizedBindingPat(SynPat.Typed(pat'',x,y), e'',valSynData,typars)

            | SynPat.Attrib(_,_,m) ->             
                error(Error(FSComp.SR.tcAttributesInvalidInPatterns(),m));
                //let (NormalizedBindingPat(pat'',e'',valSynData,typars)) = normPattern pat'
                //NormalizedBindingPat(SynPat.Attrib(pat'',x,m), e'',valSynData,typars)

            | _ ->
                NormalizedBindingPat(pat,rhsExpr,valSynData,inferredTyparDecls) 
        normPattern pat

    let NormalizeBinding isObjExprBinding cenv (env: TcEnv) b = 
        match b with 
        | Binding (vis,bkind,pseudo,isMutable,attrs,doc,valSynData,p,retInfo,rhsExpr,bindingRange,spBind) ->
            let (NormalizedBindingPat(pat,rhsExpr,valSynData,typars)) = 
                NormalizeBindingPattern cenv.nameResolver isObjExprBinding env valSynData p (NormalizedBindingRhs ([], retInfo, rhsExpr))
            NormalizedBinding(vis,bkind,pseudo,isMutable,attrs,doc.ToXmlDoc(),typars,valSynData,pat,rhsExpr,bindingRange,spBind)

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
    let ConvertMemberFlags  memberFlags = { memberFlags with MemberKind= MemberKind.Member } 

    let private ConvertMemberFlagsOpt m memberFlagsOpt =
        match memberFlagsOpt with 
        | Some memberFlags -> Some (ConvertMemberFlags memberFlags)
        | _ -> error(BadEventTransformation(m))

    let private ConvertSynData m valSynData =
        let (SynValData(memberFlagsOpt,valSynInfo,thisIdOpt)) = valSynData 
        let memberFlagsOpt = ConvertMemberFlagsOpt m memberFlagsOpt
        let valSynInfo = ConvertSynInfo m valSynInfo
        SynValData(memberFlagsOpt,valSynInfo,thisIdOpt)
     
    let rec private  RenameBindingPattern f declPattern = 
        match declPattern with  
        | SynPat.Typed(pat',_,_) -> RenameBindingPattern f pat'
        | SynPat.Named (SynPat.Wild m1, id,x2,vis2,m) -> SynPat.Named (SynPat.Wild m1, ident(f id.idText,id.idRange) ,x2,vis2,m) 
        | SynPat.InstanceMember(thisId,id,toolId,vis2,m) -> SynPat.InstanceMember(thisId,ident(f id.idText,id.idRange),toolId,vis2,m)
        | _ -> error(Error(FSComp.SR.tcOnlySimplePatternsInLetRec(),declPattern.Range))

    let GenerateExtraBindings (g,bindingAttribs,binding) =
        let (NormalizedBinding(vis1, bindingKind, isInline, isMutable, _, bindingXmlDoc, _synTyparDecls, valSynData, declPattern, bindingRhs, bindingRange, spBind)) = binding
        if CompileAsEvent g bindingAttribs then 

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
                       let rhsExpr = SynExpr.App(ExprAtomicFlag.NonAtomic,SynExpr.DotGet(SynExpr.Paren(trueRhsExpr,m),[ident(target,m)],m),SynExpr.Ident(ident(argName,m)),m)
                       
                       // reconstitute rhsExpr
                       let bindingRhs = NormalizedBindingRhs([],None,rhsExpr)

                       // add the argument to the expression 
                       let bindingRhs = PushOnePatternToRhs true (mkSynPatVar None (ident (argName,bindingRange))) bindingRhs 
                       
                       bindingRhs,valSynData
                   | _ -> 
                       error(BadEventTransformation(m))

                // reconstitute the binding
                NormalizedBinding(vis1,bindingKind,isInline,isMutable,[],bindingXmlDoc,noInferredTypars,valSynData,declPattern,bindingRhs,bindingRange,spBind) 

            [ MakeOne ("add_","AddHandler"); MakeOne ("remove_","RemoveHandler") ]
        else 
            []

//-------------------------------------------------------------------------
// Helpers to adjust the 'this' pointer before making a call.
//------------------------------------------------------------------------- 


/// Compute whether we insert a 'coerce' on the 'this' pointer for an object model call 
/// For example, when calling an interface method on a struct, or a method on a constrained 
/// variable type. 
let computeConstrainedCallInfo cenv m (objArgs,minfo:MethInfo) =
    match objArgs with 
    | [objArgExpr] when not minfo.IsExtensionMember -> 
        let methObjTy = minfo.EnclosingType
        let objArgTy = tyOfExpr cenv.g objArgExpr
        if TypeDefinitelySubsumesTypeNoCoercion 0 cenv.g cenv.amap m methObjTy objArgTy 
           // Constrained calls to class types can only ever be needed for the three class types that 
           // are base types of value types
           || (isClassTy cenv.g methObjTy && 
                 (not (typeEquiv cenv.g methObjTy cenv.g.system_Object_typ || 
                       typeEquiv cenv.g methObjTy cenv.g.system_Value_typ ||
                       typeEquiv cenv.g methObjTy cenv.g.system_Enum_typ))) then 
            None
        else
            // The object argument is a value type or variable type and the target method is an interface or System.Object
            // type. A .NET 2.0 generic constrained call is required
            Some objArgTy
    | _ -> 
        None

/// Adjust the 'this' pointer before making a call 
/// Take the address of a struct, and coerce to an interface/base/constraint type if necessary 
let TakeObjAddrForMethodCall cenv (minfo:MethInfo) isMutable m objArgs f =
    let ccallInfo = computeConstrainedCallInfo cenv m (objArgs,minfo) 
    let mustTakeAddress = 
        (minfo.IsStruct && not minfo.IsExtensionMember)  // don't take the address of a struct when passing to an extension member
        ||
        (match ccallInfo with 
         | Some _ -> true 
         | None -> false) 
    let wrap,objArgs = 
        match objArgs with
        | [objArgExpr] -> 
            let objArgTy = tyOfExpr cenv.g objArgExpr
            let wrap,objArgExpr' = mkExprAddrOfExpr cenv.g mustTakeAddress (isSome ccallInfo) isMutable objArgExpr m 
            
            // Extension members and calls to class constraints may need a coercion for their object argument
            let objArgExpr' = 
              if isNone ccallInfo && // minfo.IsExtensionMember && minfo.IsStruct && 
                 not (TypeDefinitelySubsumesTypeNoCoercion 0 cenv.g cenv.amap m minfo.EnclosingType objArgTy) then 
                  mkCoerceExpr(objArgExpr',minfo.EnclosingType,m,objArgTy)
              else
                  objArgExpr'

            wrap,[objArgExpr'] 

        | _ -> 
            (fun x -> x), objArgs
    let e,ety = f ccallInfo objArgs
    wrap e,ety

let FreshenObjectArgType cenv m rigid tcref isExtrinsic declaredTyconTypars = 
    let tcrefObjTy,enclosingDeclaredTypars,renaming,objTy = FreshenTyconRef m rigid tcref declaredTyconTypars
    // Struct members have a byref 'this' type (unless they are extrinsic extension members)
    let thisTy = 
        if tcref.IsStructOrEnumTycon && not isExtrinsic then 
            mkByrefTy cenv.g objTy 
        else 
            objTy
    tcrefObjTy,enclosingDeclaredTypars,renaming,objTy,thisTy



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
let TcVal cenv env tpenv (vref:ValRef) optInst m =
    let v = vref.Deref
    let vrec = v.RecursiveValInfo
    v.SetHasBeenReferenced() 
    CheckValAccessible m (AccessRightsOfEnv env) vref;
    CheckValAttributes cenv.g vref m  |> CommitOperationResult;
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
          let _,tinst,tau = FreshenPossibleForallTy cenv.g m TyparFlexible vty 
          Expr.Const(c,m,tau),isSpecial,tau,tinst,tpenv

      | None -> 
            // References to 'this' in classes get dereferenced from their implicit reference cell and poked
          if v.BaseOrThisInfo = CtorThisVal && isRefCellTy cenv.g vty  then 
              let exprForVal = exprForValRef m vref
              //if AreWithinCtorPreConstruct env then 
              //    warning(SelfRefObjCtor(AreWithinImplicitCtor env, m));

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
                      if HasAttrib cenv.g cenv.g.attrib_RequiresExplicitTypeArgumentsAttribute v.Attribs then
                           errorR(Error(FSComp.SR.tcFunctionRequiresExplicitTypeArguments(v.DisplayName),m));
                  
                      match vrec with 
                      | ValInRecScope(false) -> 
                          let tps,tau =  vref.TypeScheme
                          let tinst = tps |> List.map mkTyparTy
                          NormalValUse,tinst,tau,tpenv
                      | ValInRecScope(true) 
                      | ValNotInRecScope ->
                          let _,tinst,tau = FreshenPossibleForallTy cenv.g m TyparFlexible vty 
                          NormalValUse,tinst,tau,tpenv

                  // If we have got an explicit instantiation then use that 
                  | Some(vrefFlags,checkTys) -> 
                          let checkInst (tinst:TypeInst) = 
                              if not v.IsMember && not v.PermitsExplicitTypeInstantiation && tinst.Length > 0 && v.Typars.Length > 0 then 
                                   warning(Error(FSComp.SR.tcDoesNotAllowExplicitTypeArguments(v.DisplayName),m));
                          match vrec with 
                          | ValInRecScope(false) -> 
                              let tpsorig,tau =  vref.TypeScheme
                              let (tinst:TypeInst),tpenv = checkTys tpenv (tpsorig |> List.map (fun tp -> tp.Kind))
                              checkInst tinst;
                              if tpsorig.Length <> tinst.Length then error(Error(FSComp.SR.tcTypeParameterArityMismatch(tpsorig.Length, tinst.Length),m));
                              let tau2 = instType (mkTyparInst tpsorig tinst) tau
                              List.iter2
                                (fun tp ty -> 
                                  try UnifyTypes cenv env m (mkTyparTy tp) ty
                                  with _ -> error (Recursion(env.DisplayEnv,v.Id,tau2,tau,m))) 
                                tpsorig 
                                tinst;
                              vrefFlags,tinst,tau2,tpenv  
                          | ValInRecScope(true) 
                          | ValNotInRecScope ->
                              let tps,tptys,tau = FreshenPossibleForallTy cenv.g m TyparFlexible vty 
                              //dprintfn "After Freshen: tau = %s" (Layout.showL (typeL tau));
                              let (tinst:TypeInst),tpenv = checkTys tpenv (tps |> List.map (fun tp -> tp.Kind))
                              checkInst tinst;
                              //dprintfn "After Check: tau = %s" (Layout.showL (typeL tau));
                              if tptys.Length <> tinst.Length then error(Error(FSComp.SR.tcTypeParameterArityMismatch((List.length tps), (List.length tinst)),m));
                              List.iter2 (UnifyTypes cenv env m) tptys tinst;
                              //dprintfn "After Unify: tau = %s" (Layout.showL (typeL tau));
                              vrefFlags,tinst,tau,tpenv  
                      
              let exprForVal = Expr.Val (vref,vrefFlags,m)
              let exprForVal = mkTyAppExpr m (exprForVal,vty) tinst
              let isSpecial = 
                  (match vrefFlags with NormalValUse | PossibleConstrainedCall _ -> false | _ -> true) ||  
                  valRefEq cenv.g vref cenv.g.splice_expr_vref || 
                  valRefEq cenv.g vref cenv.g.splice_raw_expr_vref 
              
              let exprForVal =  RecordUseOfRecValue cenv vrec vref exprForVal m

              exprForVal, isSpecial, tau, tinst, tpenv

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
    let curriedActualTypes = argTys |> List.map (tryDestTupleTy cenv.g)
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
                   AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace actualType flexibleType;
                   flexibleType)

        // Create a coercion to represent the expansion of the application
        let expr = mkCoerceExpr (expr,mkIteratedFunTy (List.map (mkTupledTy cenv.g) curriedFlexibleTypes) retTy,m,exprTy)
        ApplicableExpr (cenv,expr,true)


///  Checks, warnings and constraint assertions for downcasts 
let TcRuntimeTypeTest cenv denv m tgty srcTy =
    if TypeDefinitelySubsumesTypeNoCoercion 0 cenv.g cenv.amap m tgty srcTy then 
      warning(TypeTestUnnecessary(m));

    if isTyparTy cenv.g srcTy then 
        error(IndeterminateRuntimeCoercion(denv,srcTy,tgty,m));

    if isSealedTy cenv.g srcTy then 
        error(RuntimeCoercionSourceSealed(denv,srcTy,m));

    if isSealedTy cenv.g tgty ||
       isTyparTy cenv.g tgty ||
       not (isInterfaceTy cenv.g srcTy) then 
        AddCxTypeMustSubsumeType denv cenv.css m NoTrace srcTy tgty

///  Checks, warnings and constraint assertions for upcasts 
let TcStaticUpcast cenv denv m tgty srcTy =
    if isTyparTy cenv.g tgty then 
        error(IndeterminateStaticCoercion(denv,srcTy,tgty,m)); 

    if isSealedTy cenv.g tgty then 
        warning(CoercionTargetSealed(denv,tgty,m));

    if typeEquiv cenv.g srcTy tgty then 
        warning(UpcastUnnecessary(m)); 

    AddCxTypeMustSubsumeType denv cenv.css m NoTrace tgty srcTy




/// Build an expression that calls a given method info. 
/// This is called after overload resolution, and also to call other 
/// methods such as 'setters' for properties. 
//   isProp: is it a property get? 
//   minst: the instantiation to apply for a generic method 
//   objArgs: the 'this' argument, if any 
//   args: the arguments, if any 
let BuildMethodCall cenv env isMutable m isProp minfo valUseFlags minst objArgs args =

    let direct = IsBaseCall objArgs

    let conditionalCallDefine = 
        TryBindMethInfoAttribute cenv.g cenv.g.attrib_ConditionalAttribute minfo 
                     (function ([ILAttribElem.String (Some(msg)) ],_) -> Some(msg) | _ -> None) 
                     (function (Attrib(_,_,[ AttribStringArg(msg) ],_,_,_)) -> Some(msg) | _ -> None)

    match conditionalCallDefine with 
    | Some(d) when not (List.mem d cenv.conditionalDefines) -> 

        // Methods marked with 'Conditional' must return 'unit' 
        UnifyTypes cenv env m cenv.g.unit_ty (FSharpReturnTyOfMeth cenv.amap m minfo minst);
        mkUnit cenv.g m, cenv.g.unit_ty

    | _ -> 

        TakeObjAddrForMethodCall cenv minfo isMutable m objArgs (fun ccallInfo objArgs -> 
            let allArgs = (objArgs @ args)
            let valUseFlags = 
                if (direct && (match valUseFlags with NormalValUse -> true | _ -> false)) then 
                    VSlotDirectCall 
                else 
                    match ccallInfo with
                    | Some ty -> 
                        // printfn "possible constrained call to '%s' at %A" minfo.LogicalName m
                        PossibleConstrainedCall ty
                    | None -> 
                        valUseFlags

            match minfo with 
            
            // Build a call to a .NET method 
            | ILMeth(_,ilMethInfo,_) -> 
                BuildILMethInfoCall cenv.g cenv.amap m isProp ilMethInfo valUseFlags minst direct allArgs

            // Build a call to an F# method 
            | FSMeth(_,typ,vref,_) -> 

                // Go see if this is a use of a recursive definition... Note we know the value instantiation 
                // we want to use so we pass that in in order not to create a new one. 
                let vexp, _, vexpty, _, _ = TcVal cenv env emptyUnscopedTyparEnv vref (Some(valUseFlags,fun tpenv _ -> (argsOfAppTy cenv.g typ @ minst, tpenv))) m

                BuildFSharpMethodApp cenv.g m vref vexp vexpty allArgs

            // Build a 'call' to a struct default constructor 
            | DefaultStructCtor (g,typ) -> 
                if not (TypeHasDefaultValue g typ) then 
                    errorR(Error(FSComp.SR.tcDefaultStructConstructorCall(),m));
                mkDefault (m,typ), typ)



let TryFindIntrinsicOrExtensionMethInfo (cenv:cenv) (env: TcEnv) m ad nm ty = 
    AllMethInfosOfTypeInScope cenv.infoReader (env.NameEnv).eExtensionMembers (Some(nm),ad) IgnoreOverrides m ty

/// Build the 'test and dispose' part of a 'use' statement 
let BuildDisposableCleanup cenv env m (v:Val) = 
    v.SetHasBeenReferenced() 
    let ad = AccessRightsOfEnv env
    let disposeMethod = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Dispose" cenv.g.system_IDisposable_typ with 
        | [x] ->  x 
        | _ -> error(InternalError(FSComp.SR.tcCouldNotFindIDisposable(),m)) 

    // For sealed types the test is simpler: we can determine if IDisposable is supported, and even when it is, we can avoid doing the type test 
    // Note this affects the elaborated form seen by quotations etc.
    if isSealedTy cenv.g v.Type then 

        if TypeFeasiblySubsumesType 0 cenv.g cenv.amap m cenv.g.system_IDisposable_typ CanCoerce v.Type then
            
            let disposeExpr,_ = BuildMethodCall cenv env PossiblyMutates   m false disposeMethod NormalValUse [] [mkCoerceExpr(exprForVal v.Range v, cenv.g.system_IDisposable_typ, m, v.Type)] []
            disposeExpr

        else
            mkUnit cenv.g m

    else
        let disposeObjVar,disposeObjExpr = Tastops.mkCompGenLocal m "objectToDispose" cenv.g.system_IDisposable_typ
        let disposeExpr,_ = BuildMethodCall cenv env PossiblyMutates   m false disposeMethod NormalValUse [] [disposeObjExpr] []
        let inpe = mkCoerceExpr(exprForVal v.Range v,cenv.g.obj_ty,m,v.Type)
        mkIsInstConditional cenv.g m cenv.g.system_IDisposable_typ inpe disposeObjVar disposeExpr (mkUnit cenv.g m) 

let BuildILFieldGet g amap m objExpr (finfo:ILFieldInfo) = 
    let fref = finfo.ILFieldRef
    let isValueType = finfo.IsValueType
    let valu = if isValueType then AsValue else AsObject
    let tinst = finfo.TypeInst
    let fieldType = finfo.FieldType (amap,m)

    let wrap,objExpr = mkExprAddrOfExpr g isValueType false NeverMutates objExpr m 
      // The empty instantiation on the AbstractIL fspec is OK, since we make the correct fspec in Ilxgen.GenAsm 
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
      // The empty instantiation on the AbstractIL fspec is OK, since we make the correct fspec in Ilxgen.gen_asm 
      // This ensures we always get the type instantiation right when doing this from 
      // polymorphic code, after inlining etc. *
    let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])
    if finfo.IsInitOnly then error (Error (FSComp.SR.tcFieldIsReadonly(),m));
    let wrap,objExpr = mkExprAddrOfExpr g isValueType false DefinitelyMutates objExpr m 
    wrap (mkAsmExpr ([ mkNormalStfld fspec ], tinst,[objExpr; argExpr],[],m)) 

let BuildILStaticFieldSet m (finfo:ILFieldInfo) argExpr = 
    let fref = finfo.ILFieldRef
    let isValueType = finfo.IsValueType
    let valu = if isValueType then AsValue else AsObject
    let tinst = finfo.TypeInst
      // The empty instantiation on the AbstractIL fspec is OK, since we make the correct fspec in Ilxgen.gen_asm 
      // This ensures we always get the type instantiation right when doing this from 
      // polymorphic code, after inlining etc. 
    let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])
    if finfo.IsInitOnly then error (Error (FSComp.SR.tcFieldIsReadonly(),m));
    mkAsmExpr ([ mkNormalStsfld fspec ], tinst,[argExpr],[],m)
    
let BuildRecdFieldSet g m objExpr (rfinfo:RecdFieldInfo) argExpr = 
    let tgty = rfinfo.EnclosingType
    let valu = isStructTy g tgty
    let objExpr = if valu then objExpr else mkCoerceExpr(objExpr,tgty,m,tyOfExpr g objExpr)
    mkRecdFieldSet g (objExpr,rfinfo.RecdFieldRef,rfinfo.TypeInst,argExpr,m) 
    
    
//-------------------------------------------------------------------------
// Helpers dealing with named and optional args at callsites
//------------------------------------------------------------------------- 

/// Detect a named argument at a callsite
let TryGetNamedArg e = 
    match e with 
    | SynExpr.App (_, SynExpr.App(_, SingleIdent(nm), 
                         LongOrSingleIdent(isOpt,[a],_),_),b,_) when nm = opNameEquals ->
        Some(isOpt,a,b)
    | _ -> None 

let IsNamedArg e = isSome (TryGetNamedArg e)

/// Get the method arguments at a callsite, taking into account named and optional arguments
let GetMethodArgs arg =
    let args = 
        match arg with 
        | SynExpr.Const (SynConst.Unit,_) -> []
        | SynExpr.Paren(SynExpr.Tuple (args,_),_) | SynExpr.Tuple (args,_) -> args
        | SynExpr.Paren(arg,_) | arg -> [arg]
    let unnamedCallerArgs,namedCallerArgs = List.takeUntil IsNamedArg args
    let namedCallerArgs = 
        namedCallerArgs |> List.choose (fun e -> 
          if not (IsNamedArg e) then 
              error(Error(FSComp.SR.tcNameArgumentsMustAppearLast(), e.Range)); 
          TryGetNamedArg e)
    unnamedCallerArgs, namedCallerArgs


//-------------------------------------------------------------------------
// Helpers dealing with adhoc conversions (functions to delegates)
//------------------------------------------------------------------------- 

/// Implements the elaborated form of adhoc conversions from functions to delegates at member callsites
let BuildNewDelegateExpr (eventInfoOpt:EventInfo option) cenv delty (minfo,delArgTys) (f,fty) m =
    let slotsig = SlotSigOfMethodInfo cenv.amap m minfo
    let delArgVals,expr = 
        let topValInfo = ValReprInfo([],List.replicate (List.length delArgTys) ValReprInfo.unnamedTopArg, ValReprInfo.unnamedRetVal)

        // Try to pull apart an explicit lambda and use it directly 
        // Don't do this in the case where we're adjusting the arguments of a function used to build a .NET-compatible event handler 
        let lambdaContents = 
            if isSome eventInfoOpt then 
                None 
            else 
                tryDestTopLambda cenv.g cenv.amap topValInfo (f, fty)        
        match lambdaContents with 
        | None -> 
        
            if List.exists (isByrefTy cenv.g) delArgTys then
                    error(Error(FSComp.SR.tcFunctionRequiresExplicitLambda(List.length delArgTys),m)); 

            let delArgVals = delArgTys |> List.map (fun argty -> fst (mkCompGenLocal m "delegateArg" argty)) 
            let expr = 
                let args = 
                    match eventInfoOpt with 
                    | Some einfo -> 
                        match delArgVals with 
                        | [] -> error(nonStandardEventError einfo.EventName m)
                        | h :: _ when not (isObjTy cenv.g h.Type) -> error(nonStandardEventError einfo.EventName m)
                        | h :: t -> [exprForVal m h; mkTupledVars cenv.g m t] 
                    | None -> 
                        if isNil delArgTys then [mkUnit cenv.g m] else List.map (exprForVal m) delArgVals
                mkApps cenv.g ((f,fty),[],args,m)
            delArgVals,expr
            
        | Some _ -> 
           if isNil delArgTys then [], mkApps cenv.g ((f,fty),[],[mkUnit cenv.g m],m) 
           else
               let _,_,_,vsl,body,_ = IteratedAdjustArityOfLambda cenv.g cenv.amap topValInfo f
               List.concat vsl, body
            
    let meth = TObjExprMethod(slotsig, [], [], [delArgVals], expr, m)
    mkObjExpr(delty,None,BuildObjCtorCall cenv.g m,[meth],[],m)


//-------------------------------------------------------------------------
// Helpers dealing with pattern match compilation
//------------------------------------------------------------------------- 

let CompilePatternForMatch cenv (env: TcEnv) exprm matchm warnOnUnused actionOnFailure (v,generalizedTypars) clauses resultTy =
    let dtree,targets = CompilePattern cenv.g env.DisplayEnv cenv.amap exprm matchm warnOnUnused actionOnFailure (v,generalizedTypars) clauses resultTy
    mkAndSimplifyMatch NoSequencePointAtInvisibleBinding exprm matchm resultTy dtree targets

/// Compile a pattern
let CompilePatternForMatchClauses cenv env exprm matchm warnOnUnused actionOnFailure inputTy resultTy tclauses = 
    // Avoid creating a dummy in the common cases where we are about to bind a name for the expression 
    // CLEANUP: avoid code duplication with code further below, i.e.all callers should call CompilePatternForMatch 
    match tclauses with 
    | [TClause(TPat_as (pat1,PBind (v,TypeScheme(generalizedTypars,_)),_),None,TTarget(vs,e,spTarget),m2)] ->
        let expr = CompilePatternForMatch cenv env exprm matchm warnOnUnused actionOnFailure (v,generalizedTypars) [TClause(pat1,None,TTarget(FlatListSet.remove valEq v vs,e,spTarget),m2)] resultTy
        v,expr
    | _ -> 
        let idv,_ = Tastops.mkCompGenLocal exprm "matchValue" inputTy
        let expr = CompilePatternForMatch cenv env exprm matchm warnOnUnused actionOnFailure (idv,[]) tclauses resultTy
        idv,expr


//-------------------------------------------------------------------------
// Helpers dealing with sequence expressions
//------------------------------------------------------------------------- 

   
/// Get the fragmentary expressions resulting from turning 
/// an expression into an enumerable value, e.g. at 'for' loops 

// localAlloc is relevant if the enumerator is a mutable struct and indicates 
// if the enumerator can be allocated as a mutable local variable 
let AnalyzeArbitraryExprAsEnumerable cenv (env: TcEnv) localAlloc m exprty expr =
    let ad = AccessRightsOfEnv env

    let findMethInfo m nm ty = 
        match TryFindIntrinsicOrExtensionMethInfo cenv env m ad nm ty with 
        | [] -> ResultOrException.Exception(Error(FSComp.SR.tcTypeCannotBeEnumerated(NicePrint.prettyStringOfTy env.DisplayEnv ty),m));
        | res :: _ -> ResultOrException.Result res  
       
      
    let tryType (exprToSearchForGetEnumeratorAndItem,tyToSearchForGetEnumeratorAndItem) = 
        match findMethInfo m "GetEnumerator" tyToSearchForGetEnumeratorAndItem with 
        | ResultOrException.Exception e -> ResultOrException.Exception e
        | ResultOrException.Result getEnumerator_minfo  ->

        let retTypeOfGetEnumerator = FSharpReturnTyOfMeth cenv.amap m getEnumerator_minfo []

        match findMethInfo m "MoveNext" retTypeOfGetEnumerator with 
        | ResultOrException.Exception e -> ResultOrException.Exception e
        | ResultOrException.Result moveNext_minfo        ->

        match findMethInfo m "get_Current" retTypeOfGetEnumerator with 
        | ResultOrException.Exception e -> ResultOrException.Exception e
        | ResultOrException.Result get_Current_minfo ->

        let enumElemTy                  = FSharpReturnTyOfMeth cenv.amap m get_Current_minfo []
        
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
                let allEquivReturnTypes minfo others = 
                    let returnTy = FSharpReturnTyOfMeth cenv.amap m minfo []
                    others |> List.forall (fun other -> typeEquiv cenv.g (FSharpReturnTyOfMeth cenv.amap m other []) returnTy)
                
                let isInt32OrObjectIndexer minfo = 
                    match ParamTypesOfMethInfo cenv.amap m minfo [] with
                    | [[ty]] -> 
                        // e.g. MatchCollection
                        typeEquiv cenv.g cenv.g.int32_ty ty || 
                        // e.g. EnvDTE.Documents.Item
                        typeEquiv cenv.g cenv.g.obj_ty ty
                    | _ -> false
                
                match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "get_Item" tyToSearchForGetEnumeratorAndItem with
                | (minfo :: others) when (allEquivReturnTypes minfo others &&
                                          List.exists isInt32OrObjectIndexer (minfo :: others)) -> 
                    FSharpReturnTyOfMeth cenv.amap m minfo []
                
                | _ -> 
                
                // Some types such as XmlNodeList have only an Item method  
                match TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Item" tyToSearchForGetEnumeratorAndItem with
                | (minfo :: others) when (allEquivReturnTypes minfo others &&
                                          List.exists isInt32OrObjectIndexer (minfo :: others)) -> 
                    FSharpReturnTyOfMeth cenv.amap m minfo []
                
                | _ -> enumElemTy
            else 
                enumElemTy

        let enumeratorVar,enumeratorExpr = 
            if isStructTy cenv.g retTypeOfGetEnumerator then 
               if localAlloc then 
                  Tastops.mkMutableCompGenLocal m "enumerator" retTypeOfGetEnumerator
               else
                  let v,e = Tastops.mkMutableCompGenLocal m "enumerator" (mkRefCellTy cenv.g retTypeOfGetEnumerator)
                  v,mkRefCellGet cenv.g m retTypeOfGetEnumerator e
                  
            else
               Tastops.mkCompGenLocal m "enumerator" retTypeOfGetEnumerator
            
        let getEnumExpr  ,getEnumTy  = BuildMethodCall cenv env PossiblyMutates   m false getEnumerator_minfo NormalValUse [] [exprToSearchForGetEnumeratorAndItem] []
        let guardExpr  ,guardTy      = BuildMethodCall cenv env DefinitelyMutates m false moveNext_minfo      NormalValUse [] [enumeratorExpr] []
        let currentExpr,currentTy    = BuildMethodCall cenv env DefinitelyMutates m true get_Current_minfo   NormalValUse [] [enumeratorExpr] []
        let betterCurrentExpr  = mkCoerceExpr(currentExpr,enumElemTy,currentExpr.Range,currentTy)
        ResultOrException.Result(enumeratorVar, enumeratorExpr,retTypeOfGetEnumerator,enumElemTy,getEnumExpr,getEnumTy, guardExpr,guardTy, betterCurrentExpr)

    //let exprToSearchForGetEnumeratorAndItem,tyToSearchForGetEnumeratorAndItem = 
    //    let enumElemTy = NewInferenceType ()
    //    let exprTyAsSeq = mkSeqTy cenv.g enumElemTy
    //    if (AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m exprTyAsSeq exprty) then 
    //       mkCoerceExpr(expr,exprTyAsSeq,expr.Range,exprty),exprTyAsSeq
    //    else
   //        expr,exprty
    
    // First try the original known static type
    match (if isArray1DTy cenv.g exprty then ResultOrException.Exception (Failure "") else tryType (expr,exprty)) with 
    | ResultOrException.Result res  -> res
    | ResultOrException.Exception e -> 
    // Next try to typecheck the thing as a sequence
    let enumElemTy = NewInferenceType ()
    let exprTyAsSeq = mkSeqTy cenv.g enumElemTy
    if (AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m exprTyAsSeq exprty) then 
       match tryType (mkCoerceExpr(expr,exprTyAsSeq,expr.Range,exprty),exprTyAsSeq) with 
       | ResultOrException.Result res  -> res
       | ResultOrException.Exception e -> raise e
    else
       raise e
    
    
// Used inside sequence expressions
let ConvertArbitraryExprToEnumerable cenv ty (env: TcEnv) (expr:Expr) =
    let m = expr.Range
    let enumElemTy = NewInferenceType ()
    if (AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m ( mkSeqTy cenv.g enumElemTy) ty) then 
        expr,enumElemTy
    else          
        let enumerableVar,enumerableExpr = mkCompGenLocal m "inputSequence" ty
        let enumeratorVar, _,retTypeOfGetEnumerator,enumElemTy,getEnumExpr,_,guardExpr,guardTy,betterCurrentExpr = 
            AnalyzeArbitraryExprAsEnumerable cenv env false m ty enumerableExpr
        
        // if isStructTy cenv.g getEnumTy then errorR(Error(FSComp.SR.tcBadReturnTypeForGetEnumerator(),m));
        
        let expr = 
           mkCompGenLet m enumerableVar expr 
               (mkCallSeqOfFunctions cenv.g m retTypeOfGetEnumerator enumElemTy 
                   (mkUnitDelayLambda cenv.g m getEnumExpr)
                   (mkLambda m enumeratorVar (guardExpr,guardTy)) 
                   (mkLambda m enumeratorVar (betterCurrentExpr,enumElemTy)))
        expr,enumElemTy           

let mk_seq_empty cenv env m genTy =
    // We must discover the 'zero' of the monadic algebra being generated in order to compile failing matches.
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy);
    mkCallSeqEmpty cenv.g m genResultTy 

let mk_seq_collect cenv env m enumElemTy genTy lam enumExpr =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy);
    let enumExpr = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g enumElemTy) (tyOfExpr cenv.g enumExpr) enumExpr
    mkCallSeqCollect cenv.g m enumElemTy genResultTy lam enumExpr

let mk_seq_using cenv (env: TcEnv) m resourceTy genTy resourceExpr lam =
    AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace cenv.g.system_IDisposable_typ resourceTy;
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv  env m genTy (mkSeqTy cenv.g genResultTy);
    mkCallSeqUsing cenv.g m resourceTy genResultTy resourceExpr lam 

let mk_seq_delay cenv env m genTy lam =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy);
    mkCallSeqDelay cenv.g m genResultTy (mkUnitDelayLambda cenv.g m lam) 


let mk_seq_append cenv env m genTy e1 e2 =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy);
    let e1 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e1) e1
    let e2 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e2) e2
    mkCallSeqAppend cenv.g m genResultTy e1 e2 

let mk_seq_generated cenv env m genTy e1 e2 =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy);
    let e2 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e2) e2
    mkCallSeqGenerated cenv.g m genResultTy e1 e2 

let mk_seq_finally cenv env m genTy e1 e2 =
    let genResultTy = NewInferenceType ()
    UnifyTypes cenv env m genTy (mkSeqTy cenv.g genResultTy);
    let e1 = mkCoerceIfNeeded cenv.g (mkSeqTy cenv.g genResultTy) (tyOfExpr cenv.g e1) e1
    mkCallSeqFinally cenv.g m genResultTy e1 e2 

let mk_monadic_match_clauses (pat',vspecs) innerExpr = 
    [ TClause(pat',None,TTarget(vspecs, innerExpr,SequencePointAtTarget),rangeOfPat pat') ]

let mk_seq_match_clauses (pat',vspecs) innerExpr = 
    [TClause(pat',None,TTarget(vspecs, innerExpr,SequencePointAtTarget),rangeOfPat pat') ] 

let conv_tcomp_match_clauses cenv env inputExprMark (pat',vspecs) innerExpr bindPatTy genInnerTy = 
    let patMark = (rangeOfPat pat')
    let tclauses = mk_seq_match_clauses (pat',vspecs) innerExpr 
    CompilePatternForMatchClauses cenv env inputExprMark patMark false ThrowIncompleteMatchException bindPatTy genInnerTy tclauses 


let elim_fast_integer_for_loop (spBind,id,start,dir,finish,innerExpr,m) = 
    let pseudoEnumExpr = 
        if dir then mkSynInfix m m start ".." finish
        else  mkSynTrifix m ".. .." start (SynExpr.Const(SynConst.Int32 -1, start.Range)) finish
    SynExpr.ForEach (spBind,SeqExprOnly(false),mkSynPatVar None id,pseudoEnumExpr,innerExpr,m)

/// Determine if a syntactic expression inside 'seq { ... }' or '[...]' counts as a "simple sequence
/// of semicolon separated values". For example [1;2;3].
/// 'acceptDeprecated' is true for the '[ ... ]' case, where we allow the syntax '[ if g then t else e ]' but ask it to be parenthesized
///
let (|SimpleSemicolonSequence|_|) acceptDeprecated c = 

    let rec YieldFree expr = 
        match expr with 
        | SynExpr.Seq (_,_,e1,e2,_) -> YieldFree e1 && YieldFree e2
        | SynExpr.IfThenElse (_,e2,e3opt,_,_,_) -> YieldFree e2 && Option.forall YieldFree e3opt
        | SynExpr.TryWith (e1,_,clauses,_,_,_,_) -> YieldFree e1 && clauses |> List.forall (fun (Clause(_,_,e,_,_)) -> YieldFree e)
        | SynExpr.Match (_,_,clauses,_,_) -> clauses |> List.forall (fun (Clause(_,_,e,_,_)) -> YieldFree e)
        | SynExpr.For (_,_,_,_,_,body,_) 
        | SynExpr.TryFinally (body,_,_,_,_)
        | SynExpr.LetOrUse (_,_,_,body,_) 
        | SynExpr.While (_,_,body,_) 
        | SynExpr.ForEach (_,_,_,_,body,_) -> YieldFree body
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
        | SynExpr.Seq(_,true,e1,e2,_) -> 
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

let rec stripChooseAndExpr e = 
    match stripExpr e with 
    | Expr.TyChoose(_,b,_) -> stripChooseAndExpr b
    | e -> e

// This recursive walk exposed a non-termination bug in mono when the F# compiler is run using mono 2.6 & 2.8, at least on 64-bit.
// We found that when we simplifid the code to use an explicit object for the closure, and 
// members for the recursion, then the recursive walk terminates. There is clearly a lurking bug in mono code
// generation or GC here, but it is extremely hard to isolate based on this code.
type InitGraphAnalysis (g,denv,rvs,outOfOrder,runtimeChecks,directRecursiveData,reportedEager,definiteDependencies,availIfInOrder,boundv) = 

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
        | Expr.Lambda (_,_,_,_,b,_,_) | Expr.TyLambda (_,_,b,_,_) -> checkDelayed st b
        | Expr.Obj (_,ty,_,e,overrides,extraImpls,_) -> 
            // NOTE: we can't fixup recursive references inside delegates since the closure delegee of a delegate is not accessible 
            // from outside. Object expressions implementing interfaces can, on the other hand, be fixed up. See FSharp 1.0 bug 1469 
            if isInterfaceTy g ty (* || isDelegateTy ty *) then 
                List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  checkDelayed st e) overrides;
                List.iter (snd >> List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  checkDelayed st e)) extraImpls;
            else 
                CheckExpr (strict st) e;
                List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  CheckExpr (lzy (strict st)) e) overrides;
                List.iter (snd >> List.iter (fun (TObjExprMethod(_,_,_,_,e,_)) ->  CheckExpr (lzy (strict st)) e)) extraImpls;
            
          // Expressions where fixups may be needed 
        | Expr.Val (v,_,m) -> CheckValSpec st v m

         // Expressions where subparts may be fixable 
        | Expr.Op((TOp.Tuple | TOp.UnionCase _ | TOp.Recd _),_,args,_) -> 
            List.iter (CheckExpr (fixable st)) args

          // Composite expressions 
        | Expr.Const _ -> ()
        | Expr.LetRec (binds,e,_,_)  ->
            binds |> FlatList.iter (fun b -> CheckExpr (strict st) b.Expr) ; 
            CheckExpr (strict st) e
        | Expr.Let (bind,e,_,_) ->  
            CheckExpr (strict st) bind.Expr; 
            CheckExpr (strict st) e
        | Expr.Match (_,_,pt,targets,_,_) -> 
            CheckDecisionTree (strict st) pt; 
            Array.iter (CheckDecisionTreeTarget (strict st)) targets 
        | Expr.App(e1,_,_,args,_) -> 
            CheckExpr (strict st) e1;  
            List.iter (CheckExpr (strict st)) args 
      // Binary expressions 
        | Expr.Seq (e1,e2,_,_,_)
        | Expr.StaticOptimization (_,e1,e2,_) ->
             CheckExpr (strict st) e1;  CheckExpr (strict st) e2
      // n-ary expressions 
        | Expr.Op(op,_,args,m)  -> CheckExprOp st op m;  List.iter (CheckExpr (strict st)) args
      // misc 
        | Expr.Link(eref) -> CheckExpr st !eref
        | Expr.TyChoose (_,b,_)  -> CheckExpr st b
        | Expr.Quote _  -> ()

    and CheckDecisionTree st = function
        | TDSwitch(e1,csl,dflt,_) -> CheckExpr st e1; List.iter (fun (TCase(_,d)) -> CheckDecisionTree st d) csl; Option.iter (CheckDecisionTree st) dflt
        | TDSuccess (es,_) -> es |> FlatList.iter (CheckExpr st) 
        | TDBind(bind,e) -> CheckExpr st bind.Expr; CheckDecisionTree st e
    and CheckDecisionTreeTarget st (TTarget(_,e,_)) = CheckExpr st e

    and CheckExprOp st op m = 
        match op with 
        | TOp.LValueOp (_,lvr) -> CheckValSpec (strict st) lvr m
        | _ -> ()
      
    and CheckValSpec st v m = 
        match st with 
        | MaybeLazy -> 
            if ListSet.contains g.valRefEq v rvs then 
                warning (RecursiveUseCheckedAtRuntime (denv,v,m)); 
                if not !reportedEager then 
                  (warning (LetRecCheckedAtRuntime m); reportedEager := true);
                runtimeChecks := true;

        | Top | DefinitelyStrict ->
            if ListSet.contains g.valRefEq v rvs then 
                if not (ListSet.contains g.valRefEq v availIfInOrder) then 
                    warning (LetRecEvaluatedOutOfOrder (denv,boundv,v,m)); 
                    outOfOrder := true;
                    if not !reportedEager then 
                      (warning (LetRecCheckedAtRuntime m); reportedEager := true);
                definiteDependencies := (boundv,v) :: !definiteDependencies
        | InnerTop -> 
            if ListSet.contains g.valRefEq v rvs then 
                directRecursiveData := true
        | DefinitelyLazy -> () 
    and checkDelayed st b = 
        match st with 
        | MaybeLazy | DefinitelyStrict -> CheckExpr MaybeLazy b
        | DefinitelyLazy | Top | InnerTop -> () 
      
   
    member x.Check expr = CheckExpr Top expr
    

let EliminateInitializationGraphs g mustHaveArity denv (fixupsAndBindingsWithoutLaziness : PreInitializationGraphEliminationBinding list) bindsm =
    // BEGIN INITIALIZATION GRAPHS 
    // Check for safety and determine if we need to insert lazy thunks 
    let fixupsl =  fixupsAndBindingsWithoutLaziness |> List.map (fun b -> b.FixupPoints)
    let bindsWithoutLaziness =  fixupsAndBindingsWithoutLaziness |> List.map (fun b -> b.Binding)
    let rvs = bindsWithoutLaziness |> List.map (fun (TBind(v,_,_)) -> mkLocalValRef v) 

    // The output of the analysis
    let outOfOrder = ref false
    let runtimeChecks = ref false
    let directRecursiveData = ref false
    let reportedEager = ref false
    let definiteDependencies = ref []

    List.fold 
         (fun availIfInOrder (TBind(v,e,_)) -> 
           let analyzer = InitGraphAnalysis (g,denv,rvs,outOfOrder,runtimeChecks,directRecursiveData,reportedEager,definiteDependencies,availIfInOrder,(mkLocalValRef v))
           analyzer.Check e; 
           (mkLocalValRef v::availIfInOrder))
         [] bindsWithoutLaziness |> ignore;
    
     // ddg = definiteDependencyGraph 
    let ddgNodes = bindsWithoutLaziness |> List.map (fun (TBind(v,_,_)) -> mkLocalValRef v) 
    let ddg = Graph<ValRef, Stamp>((fun v -> v.Stamp), ddgNodes, !definiteDependencies )
    ddg.IterateCycles (fun path -> error (LetRecUnsound (denv,path,path.Head.Range))) ;

    let requiresLazyBindings = !runtimeChecks || !outOfOrder
    if !directRecursiveData && requiresLazyBindings then 
        error(Error(FSComp.SR.tcInvalidMixtureOfRecursiveForms(),bindsm));

    let bindsBefore, bindsAfter = 
      if requiresLazyBindings then 
          let bindsBeforeL, bindsAfterL = 
            
              (fixupsl, bindsWithoutLaziness) 
              ||> List.map2 (fun (RecursiveUseFixupPoints(fixupPoints)) (TBind(v,e,seqPtOpt)) -> 
                   match stripChooseAndExpr e with
                   | Expr.Lambda _ | Expr.TyLambda _ -> 
                       [mkInvisibleBind v e],[] 
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
                       
                       if mustHaveArity then vlazy.SetValReprInfo (Some(InferArityOfExpr g vty [] [] vrhs));
                       fixupPoints |> List.iter (fun (fp,_) -> fp := mkLazyForce g (!fp).Range ty velazy);

                       [mkInvisibleBind flazy frhs; mkInvisibleBind vlazy vrhs],
                       [mkBind seqPtOpt v (mkLazyForce g m ty velazy)])
               |> List.unzip
          List.concat bindsBeforeL, List.concat bindsAfterL
      else
          bindsWithoutLaziness,[]
    bindsBefore @ bindsAfter

//-------------------------------------------------------------------------
// Check the shape of an object constructor and rewrite calls 
//------------------------------------------------------------------------- 

let CheckAndRewriteObjectCtor g env (ctorLambaExpr:Expr) =

    let m = ctorLambaExpr.Range
    let tps,vsl,body,returnTy = stripTopLambda (ctorLambaExpr,tyOfExpr g ctorLambaExpr)

    // Rewrite legitimate self-construction calls to CtorValUsedAsSelfInit 
    let error (expr:Expr) = 
        errorR(Error(FSComp.SR.tcInvalidObjectConstructionExpression(),expr.Range));
        expr

    // Build an assignment into the safeThisValOpt mutable reference cell that holds recursive references to 'this' 
    // Build an assignment into the safeInitInfo mutable field that indicates that partial initialization is successful
    let rewriteContruction recdExpr = 
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
                   Expr.Seq(recdExpr,setExpr,ThenDoSeq,SuppressSequencePointOnExprOfSequential,m)
           let recdExpr =                        
               match ctorInfo.safeInitInfo with 
               | NoSafeInitInfo ->  recdExpr
               | SafeInitField (rfref, _) -> 
                   let thisTy = tyOfExpr g recdExpr
                   let thisExpr = mkGetArg0 m thisTy
                   let thisTyInst = argsOfAppTy g thisTy
                   let setExpr = mkRecdFieldSet g (thisExpr, rfref, thisTyInst, mkOne g m, m)
                   Expr.Seq(recdExpr,setExpr,ThenDoSeq,SuppressSequencePointOnExprOfSequential,m)
           recdExpr
       

    let rec checkAndRewrite (expr:Expr) = 
        match expr with 
        // <ctor-body> = { fields } 
        // The constructor ends in an object initialization expression - good 
        | Expr.Op(TOp.Recd(RecdExprIsObjInit,_),_,_,_) -> rewriteContruction expr

        // <ctor-body> = "a; <ctor-body>" 
        | Expr.Seq(a,body,NormalSeq,spSeq,b)  -> Expr.Seq(a,checkAndRewrite body,NormalSeq,spSeq,b) 

        // <ctor-body> = "<ctor-body> then <expr>" 
        | Expr.Seq(body,a,ThenDoSeq,spSeq,b) -> Expr.Seq(checkAndRewrite body,a,ThenDoSeq,spSeq,b)

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
            rewriteContruction expr 

        | _ -> 
            error(expr)

    and checkAndRewriteCtorUsage expr = 
         match expr with 
         | Expr.Link eref -> 
               let e = checkAndRewriteCtorUsage !eref
               eref := e;
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
               | Some(memberInfo) -> (memberInfo.MemberFlags.MemberKind = MemberKind.Constructor)

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
        MakeApplicableExprNoFlex cenv (mkCompGenSeq m arg (mkReraise m rtn_ty))
    | ApplicableExpr(_, Expr.App(Expr.Val(vf,_,_),_,_,[],_),_), _ 
         when (valRefEq cenv.g vf cenv.g.addrof_vref || 
               valRefEq cenv.g vf cenv.g.addrof2_vref) -> 
        if valRefEq cenv.g vf cenv.g.addrof2_vref then warning(UseOfAddressOfOperator(m));
        let wrap,e1a' = mkExprAddrOfExpr cenv.g true false DefinitelyMutates arg m 
        MakeApplicableExprNoFlex cenv (wrap(e1a'))
    | _ -> 
        expr.SupplyArgument(arg,m)             

//-------------------------------------------------------------------------
// Additional data structures used by type checking
//------------------------------------------------------------------------- 

type DelayedItem = 
  | DelayedTypeApp of Ast.SynType list * Range.range
  | DelayedApp of ExprAtomicFlag * Ast.SynExpr * Range.range
  | DelayedDotLookup of Ast.Ident list * Range.range
  | DelayedSet of Ast.SynExpr * Range.range

let MakeDelayedSet(e: SynExpr, m) = 
    // We have lid <- e. Wrap 'e' in another pair of parentheses to ensure it's never interpreted as 
    // a named argument, e.g. for "el.Checked <- (el = el2)" 
    DelayedSet (SynExpr.Paren (e, e.Range), m)

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
    member x.ParentRef = (let (ContainerInfo(v,_)) = x in v)
    
/// Indicates a declaration is contained in an expression 
let ExprContainerInfo = ContainerInfo(ParentNone,None)
/// Indicates a declaration is contained in the given module 
let ModuleOrNamespaceContainerInfo modref = ContainerInfo(Parent(modref),Some(MemberOrValContainerInfo(modref,None,None,NoSafeInitInfo,[])))
/// Indicates a declaration is contained in the given type definition in the given module 
let TyconContainerInfo (parent, tcref, declaredTyconTypars, safeInitInfo) = ContainerInfo(parent,Some(MemberOrValContainerInfo(tcref,None,None,safeInitInfo,declaredTyconTypars)))

type NormalizedRecBindingDefn = NormalizedRecBindingDefn of ContainerInfo * NewSlotsOK * DeclKind * NormalizedBinding

type TyconBindingDefn = TyconBindingDefn of ContainerInfo * NewSlotsOK * DeclKind * SynMemberDefn * range

type TyconBindingDefns = TyconBindingDefns of TyconRef * DeclKind * TyconBindingDefn list

type TyconMemberData = TyconMemberData of DeclKind * TyconRef * Val option * SafeInitData * Typars * SynMemberDefn list * range * NewSlotsOK

type ValSpecResult = ValSpecResult of ParentRef * ValMemberInfoTransient option * Ident * Typars * Typars * TType * PartialValReprInfo * DeclKind 

//-------------------------------------------------------------------------
// Additional data structures used by checking recursive bindings
//------------------------------------------------------------------------- 

type RecursiveBindingDefnInfo = RecBindingDefn of ContainerInfo * NewSlotsOK * DeclKind * SynBinding

/// RecursiveBindingInfo - flows through initial steps of TcLetrec 
type RecursiveBindingInfo =
    | RBInfo of
          int * // index of the binding in the recursive group
          ContainerInfo * 
          Typars * 
          ValInlineInfo * 
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
    member x.Val = let (RBInfo(_,_,_,_,vspec,_,_,_,_,_,_,_,_,_)) = x in vspec
    member x.ExplicitTyparInfo = let (RBInfo(_,_,_,_,_,flex,_,_,_,_,_,_,_,_)) = x in flex
    member x.DeclaredTypars = let (ExplicitTyparInfo(_,declaredTypars,_)) = x.ExplicitTyparInfo in declaredTypars
    member x.Index = let (RBInfo(i,_,_,_,_,_,_,_,_,_,_,_,_,_)) = x in i

type PreCheckingRecursiveBinding = 
    { SyntacticBinding : NormalizedBinding 
      RecBindingInfo : RecursiveBindingInfo }


type PreGeneralizationRecursiveBinding = 
    { ExtraGeneralizableTypars : Typars
      CheckedBinding: CheckedBindingInfo;
      RecBindingInfo : RecursiveBindingInfo }

type PostGeneralizationRecursiveBinding = 
    { ValScheme : ValScheme
      CheckedBinding: CheckedBindingInfo
      RecBindingInfo : RecursiveBindingInfo }
    member x.GeneralizedTypars = x.ValScheme.GeneralizedTypars

type PostBindCtorThisVarRefCellRecursiveBinding = 
    { ValScheme: ValScheme;
      Binding: Tast.Binding }


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
let rec TcTyparConstraint ridx cenv newOk checkCxs (env: TcEnv) tpenv c = 
    let checkSimpleConstraint tp m constraintAdder =
       let tp',tpenv = TcTypar cenv env newOk tpenv tp
       constraintAdder env.DisplayEnv cenv.css m NoTrace (mkTyparTy tp') ;
       tpenv 

    match c with 
    | WhereTyparEqualsType(tp,ty,m) ->
       let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv ty
       let tp',tpenv = TcTypar cenv env newOk tpenv tp
       if (newOk = NoNewTypars) then errorR(Error(FSComp.SR.tcInvalidConstraint(),m));
       UnifyTypes cenv env m (mkTyparTy tp') ty';
       tpenv

    | WhereTyparDefaultsToType(tp,ty,m) ->
       let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv ty
       let tp',tpenv = TcTypar cenv env newOk tpenv tp
       let csenv = (MakeConstraintSolverEnv cenv.css m env.DisplayEnv)
       AddConstraint csenv 0 m NoTrace tp' (TTyparDefaultsToType(ridx,ty',m)) |> CommitOperationResult;
       tpenv

    | WhereTyparSubtypeOfType(tp,ty,m) ->
       let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv ty
       let tp',tpenv = TcTypar cenv env newOk tpenv tp
       if (newOk = NoNewTypars) && isSealedTy cenv.g ty' then 
           errorR(Error(FSComp.SR.tcInvalidConstraintTypeSealed(),m));
       AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace  ty' (mkTyparTy tp') ;
       tpenv

    | WhereTyparSupportsNull(tp,m) -> checkSimpleConstraint tp m AddCxTypeMustSupportNull

    | WhereTyparIsComparable(tp,m) -> checkSimpleConstraint tp m AddCxTypeMustSupportComparison

    | WhereTyparIsEquatable(tp,m) -> checkSimpleConstraint tp m AddCxTypeMustSupportEquality 

    | WhereTyparIsReferenceType(tp,m) ->checkSimpleConstraint tp m AddCxTypeIsReferenceType

    | WhereTyparIsValueType(tp,m) -> checkSimpleConstraint tp m AddCxTypeIsValueType
       
    | WhereTyparIsUnmanaged(tp,m) -> checkSimpleConstraint tp m AddCxTypeIsUnmanaged

    | WhereTyparIsEnum(tp,tyargs,m) ->
       let tp',tpenv = TcTypar cenv env newOk tpenv tp
       let tpenv = 
           match tyargs with 
           | [underlying] -> 
               let underlying',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv underlying
               AddCxTypeIsEnum env.DisplayEnv cenv.css m NoTrace (mkTyparTy tp') underlying';
               tpenv
           | _ -> 
               errorR(Error(FSComp.SR.tcInvalidEnumConstraint(),m));
               tpenv
       tpenv

    | WhereTyparIsDelegate(tp,tyargs,m) ->
       let tp',tpenv = TcTypar cenv env newOk tpenv tp
       match tyargs with 
       | [a;b] -> 
           let a',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv a
           let b',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv b
           AddCxTypeIsDelegate env.DisplayEnv cenv.css m NoTrace (mkTyparTy tp') a' b';
           tpenv
       | _ -> 
           errorR(Error(FSComp.SR.tcInvalidEnumConstraint(),m));
           tpenv

    | WhereTyparSupportsMember(tps,memSpfn,m) ->
        let traitInfo,tpenv = TcPseudoMemberSpec cenv newOk env tps tpenv memSpfn m
        match traitInfo with 
        | TTrait(objtys,".ctor",memberFlags,argtys,returnTy,_) when (memberFlags.MemberKind=MemberKind.Constructor) ->
            match objtys,argtys with 
            | [ty],[] when typeEquiv cenv.g ty (GetFSharpViewOfReturnType cenv.g returnTy) ->
                AddCxTypeMustSupportDefaultCtor env.DisplayEnv cenv.css m NoTrace ty ;
                tpenv
            | _ ->            
                errorR(Error(FSComp.SR.tcInvalidNewConstraint(),m));
                tpenv
        | _ ->  
            AddCxMethodConstraint env.DisplayEnv cenv.css m NoTrace traitInfo;
            tpenv
      
and TcPseudoMemberSpec cenv newOk env tps tpenv memSpfn m = 
    let tps',tpenv = List.mapFold (TcTypar cenv env newOk) tpenv tps
    let tys = List.map mkTyparTy tps'
    match memSpfn with 
    | SynMemberSig.Member (valSpfn,memberFlags,m) ->
        let members,tpenv = TcValSpec cenv env ModuleOrMemberBinding (ExprContainerInfo) (Some memberFlags) (Some (List.head tys)) tpenv valSpfn []
        match members with 
        | [ValSpecResult(_,_,id,_,_,ty',partialValReprInfo,_)] -> 
            let tps,_ = tryDestForallTy cenv.g ty'
            let topValInfo = TranslatePartialArity tps partialValReprInfo
            let _,curriedArgInfos,returnTy,_ = GetTopValTypeInCompiledForm cenv.g topValInfo ty' m
            let argtys = List.concat curriedArgInfos
            let argtys = List.map fst argtys
            let logicalCompiledName = ComputeLogicalName id memberFlags
            TTrait(tys,logicalCompiledName,memberFlags,argtys,returnTy, ref None),tpenv
        | _ -> error(Error(FSComp.SR.tcInvalidConstraint(),m))
    | _ -> error(Error(FSComp.SR.tcInvalidConstraint(),m))


/// Check a value specification, e.g. in a signature or a constraint
and TcValSpec cenv env declKind containerInfo memFlagsOpt thisTyOpt tpenv valSpfn attrs' =
    let (ValSpfn(_, id, SynValTyparDecls(synTypars, _, synTyparConstraints), ty, valSynInfo, _, _, _, _, _, m)) = valSpfn 
    let declaredTypars = TcTyparDecls cenv env synTypars
    let (ContainerInfo(altActualParent,tcrefContainerInfo)) = containerInfo
    let enclosingDeclaredTypars,memberContainerInfo,thisTyOpt,declKind = 
        match tcrefContainerInfo with 
        | Some(MemberOrValContainerInfo(tcref,_,_,_,declaredTyconTypars)) -> 
            let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
            let _,enclosingDeclaredTypars,_,_,thisTy = FreshenObjectArgType cenv m TyparRigid tcref isExtrinsic declaredTyconTypars
            // An implemented interface type is in terms of the type's type parameters. 
            // We need a signature in terms of the values' type parameters. 
            // let optIntfSlotTy = Option.map (instType renaming) optIntfSlotTy in  
            enclosingDeclaredTypars,Some(tcref),Some(thisTy),declKind
        | None -> 
            [],None,thisTyOpt, ModuleOrMemberBinding
    let allDeclaredTypars = (enclosingDeclaredTypars@declaredTypars)
    let envinner = AddDeclaredTypars NoCheckForDuplicateTypars allDeclaredTypars env
    let newOk = NewTyparsOK
    let checkCxs = CheckCxs
    let tpenv = TcTyparConstraints cenv newOk checkCxs envinner tpenv synTyparConstraints
    
    // Treat constraints at the "end" of the type as if they are declared.
    // This is by far the most convenient place to locate the constraints.
    // e.g. 
    //    val FastGenericComparer<'T>  : IComparer<'T> when 'T : comparison 
    let tpenv = 
        match ty with 
        | SynType.WithGlobalConstraints(_,wcs,_) ->
            TcTyparConstraints cenv newOk checkCxs envinner tpenv wcs
        | _ -> 
            tpenv

    // Enforce "no undeclared constraints allowed on declared typars"
    allDeclaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m);
    // Process the type, including any constraints 
    let declaredTy,tpenv = TcTypeAndRecover cenv newOk checkCxs envinner tpenv ty  

    match memFlagsOpt,thisTyOpt with 
    | Some(memberFlags),Some(thisTy) -> 
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
                          (cenv.g.unit_ty --> declaredTy), (SynInfo.IncorporateEmptyTupledArg valSynInfo)
                        else
                          declaredTy,valSynInfo
                    | _ -> 
                        let setterTy = (mkTupledTy cenv.g (List.map fst (List.concat arginfos) @ [returnTy]) --> cenv.g.unit_ty)
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

            let reallyGenerateOneMember(id,valSynInfo,ty',memberFlags) = 
                let (PartialValReprInfo(argsData,_)) as partialValReprInfo = 
                    TranslateTopValSynInfo m (TcAttributes cenv env) valSynInfo


                // Fold in the optional arugment information 
                // Resort to using the syntactic arugment information since that is what tells us 
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
                        mkIteratedFunTy (List.map (mkTupledTy cenv.g) argtysl) returnTy
                    else ty' 
                        
                let memberInfoOpt = 
                    match memberContainerInfo with 
                    | Some(tcref) -> 
                        let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
                        let memberInfoTransient = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,isExtrinsic,attrs',[],memberFlags,valSynInfo,id,false)
                        Some(memberInfoTransient)
                    | None -> 
                        None
            
                ValSpecResult(altActualParent,memberInfoOpt,id,enclosingDeclaredTypars,declaredTypars,ty',partialValReprInfo,declKind)

            [ yield reallyGenerateOneMember(id,valSynInfo,ty',memberFlags)
              if CompileAsEvent cenv.g attrs' then 
                    let valSynInfo = EventDeclarationNormalization.ConvertSynInfo m valSynInfo
                    let memberFlags = EventDeclarationNormalization.ConvertMemberFlags memberFlags
                    let delTy = FindDelegateTypeOfPropertyEvent cenv.g cenv.amap id.idText m declaredTy 
                    let ty = 
                       if memberFlags.IsInstance then 
                         thisTy --> (delTy --> cenv.g.unit_ty)
                       else 
                         (delTy --> cenv.g.unit_ty)
                    yield reallyGenerateOneMember(ident("add_"^id.idText,id.idRange),valSynInfo,ty,memberFlags)
                    yield reallyGenerateOneMember(ident("remove_"^id.idText,id.idRange),valSynInfo,ty,memberFlags) ]
                
              
            
        match memberFlags.MemberKind with 
        | MemberKind.ClassConstructor
        | MemberKind.Constructor
        | MemberKind.Member 
        | MemberKind.PropertyGet 
        | MemberKind.PropertySet ->
            generateOneMember(memberFlags), tpenv
        | MemberKind.PropertyGetSet ->
            [ yield! generateOneMember({memberFlags with MemberKind=MemberKind.PropertyGet});
              yield! generateOneMember({memberFlags with MemberKind=MemberKind.PropertySet}); ], tpenv
    | _ ->
        let valSynInfo = AdjustValSynInfoInSignature cenv.g declaredTy valSynInfo
        let partialValReprInfo = TranslateTopValSynInfo m (TcAttributes cenv env) valSynInfo
        [ ValSpecResult(altActualParent,None,id,enclosingDeclaredTypars,declaredTypars,declaredTy,partialValReprInfo,declKind) ], tpenv


//-------------------------------------------------------------------------
// Bind types 
//------------------------------------------------------------------------- 

/// Check and elaborate a type or measure parameter occurrence
/// If optKind=Some kind, then this is the kind we're expecting (we're in *analysis* mode)
/// If optKind=None, we need to determine the kind (we're in *synthesis* mode)
///
and TcTyparOrMeasurePar optKind _cenv env newOk tpenv (Typar(id,_,_) as tp) =
    let checkRes (res:Typar) =
        match optKind, res.Kind with
        | Some KindMeasure, KindType -> error (Error(FSComp.SR.tcExpectedUnitOfMeasureMarkWithAttribute(), id.idRange)); res, tpenv
        | Some KindType, KindMeasure -> error (Error(FSComp.SR.tcExpectedTypeParameter(), id.idRange)); res, tpenv
        | _, _ -> res, tpenv
    let key = id.idText
    match env.eNameResEnv.eTypars.TryFind key with
    | Some res -> checkRes res
    | None -> 
    match TryFindUnscopedTypar key tpenv with
    | Some res -> checkRes res
    | None -> 
        if newOk = NoNewTypars then error (UndefinedName(0,FSComp.SR.undefinedNameTypeParameter,id,["<unimplemented>"]));
        // OK, this is an implicit declaration of a type parameter 
        // The kind defaults to Type
        let tp' = NewTypar ((match optKind with None -> KindType | Some kind -> kind), TyparWarnIfNotRigid,tp,false,DynamicReq,[],false,false)
        tp',AddUnscopedTypar key tp' tpenv

and TcTypar cenv env newOk tpenv tp =
    TcTyparOrMeasurePar (Some KindType) cenv env newOk tpenv tp

and TcTyparDecl cenv env (TyparDecl(attrs,tp)) =
    let attrs' = TcAttributes cenv env AttributeTargets.GenericParameter  attrs
    let hasMeasureAttr = HasAttrib cenv.g cenv.g.attrib_MeasureAttribute attrs'
    let hasEqDepAttr = HasAttrib cenv.g cenv.g.attrib_EqualityConditionalOnAttribute attrs'
    let hasCompDepAttr = HasAttrib cenv.g cenv.g.attrib_ComparisonConditionalOnAttribute attrs'
    let attrs' = attrs' |> List.filter (IsMatchingAttrib cenv.g cenv.g.attrib_MeasureAttribute >> not)
    let tp = NewTypar ((if hasMeasureAttr then KindMeasure else KindType), TyparWarnIfNotRigid,tp,false,DynamicReq,attrs',hasEqDepAttr,hasCompDepAttr)
    match TryFindStringAttrib cenv.g cenv.g.attrib_CompiledNameAttribute attrs' with 
    | Some compiledName -> 
        tp.Data.typar_il_name <- Some compiledName
    | None ->  
        ()
    tp
    

and TcTyparDecls cenv env synTypars = List.map (TcTyparDecl cenv env) synTypars

/// Check and elaborate a syntactic type or measure
/// If optKind=Some kind, then this is the kind we're expecting (we're in *analysis* mode)
/// If optKind=None, we need to determine the kind (we're in *synthesis* mode)
///
and TcTypeOrMeasure optKind cenv newOk checkCxs env (tpenv:SyntacticUnscopedTyparEnv) ty =

    match ty with 
    | SynType.LongIdent(tc,m) -> 
        let ad = AccessRightsOfEnv env
        let tcref = ForceRaise(ResolveTypeLongIdent cenv.nameResolver ItemOccurence.Use OpenQualified GenerateEstTypeFlag.No env.eNameResEnv ad tc 0)
        match optKind, tcref.TypeOrMeasureKind with
        | Some KindType, KindMeasure ->
            error(Error(FSComp.SR.tcExpectedTypeNotUnitOfMeasure(), m)); 
            NewErrorType (), tpenv
        | Some KindMeasure, KindType ->
            error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m)); 
            TType_measure (NewErrorMeasure ()), tpenv
        | _, KindMeasure ->
            TType_measure (MeasureCon tcref), tpenv
        | _, KindType ->
            TcTypeApp cenv newOk checkCxs env tpenv m tcref [] []

    | SynType.App (SynType.LongIdent(tc,_),args,postfix,m) -> 
        let ad = AccessRightsOfEnv env
        let tcref = ForceRaise(ResolveTypeLongIdent cenv.nameResolver ItemOccurence.Use OpenQualified GenerateEstTypeFlag.No env.eNameResEnv ad tc (List.length args))
        match optKind, tcref.TypeOrMeasureKind with
        | Some KindType, KindMeasure ->
            error(Error(FSComp.SR.tcExpectedTypeNotUnitOfMeasure(), m)); 
            NewErrorType (), tpenv
        | Some KindMeasure, KindType ->
            error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m)); 
            TType_measure (NewErrorMeasure ()), tpenv
        | _, KindType ->
            if postfix && tcref.Typars(m) |> List.exists (fun tp -> match tp.Kind with KindMeasure -> true | _ -> false) 
            then error(Error(FSComp.SR.tcInvalidUnitsOfMeasurePrefix(), m));
            TcTypeApp cenv newOk checkCxs env tpenv m tcref [] args 
        | _, KindMeasure ->
            match args,postfix with
            | [arg], true ->
                let ms,tpenv = TcMeasure cenv newOk checkCxs env tpenv arg m
                TType_measure (MeasureProd(MeasureCon tcref, ms)), tpenv
              
            | _, _ ->
                errorR(Error(FSComp.SR.tcUnitsOfMeasureInvalidInTypeConstructor(), m));
                NewErrorType (), tpenv          

    | SynType.LongIdentApp (ltyp,lid,args,m) -> 
        let ad = AccessRightsOfEnv env
        let ltyp,tpenv = TcType cenv newOk checkCxs env tpenv ltyp
        if not (isAppTy cenv.g ltyp) then error(Error(FSComp.SR.tcTypeHasNoNestedTypes(),m));
        let tcref,tinst = destAppTy cenv.g ltyp
        let tcref = ResolveTypeLongIdentInTyconRef cenv.nameResolver env.eNameResEnv (ResolveTypeNamesToTypeRefs,Some(List.length args)) ad m tcref lid 
        TcTypeApp cenv newOk checkCxs env tpenv m tcref tinst args 

    | SynType.Tuple(args,m) ->   
        let isMeasure = match optKind with Some KindMeasure -> true | None -> List.exists (fun (isquot,_) -> isquot) args | _ -> false
        if isMeasure then
            let ms,tpenv = TcMeasuresAsTuple cenv newOk checkCxs env tpenv args m
            TType_measure ms,tpenv
        else
            let args',tpenv = TcTypesAsTuple cenv newOk checkCxs env tpenv args m
            TType_tuple(args'),tpenv

    | SynType.Fun(domainTy,resultTy,_) -> 
        let domainTy',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv domainTy
        let resultTy',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv resultTy
        (domainTy' --> resultTy'), tpenv

    | SynType.Array (n,elemTy,_) -> 
        let elemTy,tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv elemTy
        mkArrayTy cenv.g n elemTy, tpenv

    | SynType.Lazy (elemTy,_) -> 
        let elemTy,tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv elemTy
        mkLazyTy cenv.g elemTy, tpenv

    | SynType.Var (tp,_) -> 
        let tp',tpenv = TcTyparOrMeasurePar optKind cenv env newOk tpenv tp
        match tp'.Kind with
        | KindMeasure -> TType_measure (MeasureVar tp'), tpenv
        | KindType -> mkTyparTy tp',tpenv

    // _ types
    | SynType.Anon m ->           
        let tp:Typar = TcAnonTypeOrMeasure optKind cenv TyparAnon NoDynamicReq newOk m
        match tp.Kind with
        | KindMeasure -> TType_measure (MeasureVar tp), tpenv
        | KindType -> mkTyparTy tp,tpenv

    | SynType.WithGlobalConstraints(ty,wcs,_) ->
        let cty,tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv ty
        let tpenv = TcTyparConstraints cenv newOk checkCxs env tpenv wcs
        cty,tpenv

    // #typ 
    | SynType.HashConstraint(ty,m) ->  
        let tp' = TcAnonTypeOrMeasure (Some KindType) cenv TyparWarnIfNotRigid DynamicReq newOk m
        let ty',tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv ty
        AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace  ty' (mkTyparTy tp') ;
        TType_var tp', tpenv

    | SynType.MeasureOne m ->
        match optKind with
        | Some KindType -> 
            errorR(Error(FSComp.SR.parsUnexpectedIntegerLiteral(), m)); 
            NewErrorType (), tpenv
        | _ -> 
            TType_measure MeasureOne, tpenv

    | SynType.MeasurePower(typ, exponent, m) ->
        match optKind with
        | Some KindType -> 
            errorR(Error(FSComp.SR.tcUnexpectedSymbolInTypeExpression("^"), m)); 
            NewErrorType (), tpenv
        | _ ->          
            let ms,tpenv = TcMeasure cenv newOk checkCxs env tpenv typ m
            TType_measure (Tastops.MeasurePower ms exponent), tpenv

    | SynType.MeasureDivide(typ1, typ2, m) -> 
        match optKind with
        | Some KindType -> 
            errorR(Error(FSComp.SR.tcUnexpectedSymbolInTypeExpression("/"), m)); 
            NewErrorType (), tpenv
        | _ ->
            let ms1,tpenv = TcMeasure cenv newOk checkCxs env tpenv typ1 m
            let ms2,tpenv = TcMeasure cenv newOk checkCxs env tpenv typ2 m
            TType_measure (MeasureProd(ms1,MeasureInv ms2)), tpenv

    | SynType.App((SynType.Var(_,m1) | SynType.MeasurePower(_,_,m1)) as arg1,args,postfix,m) ->
        match optKind, args, postfix with
        | (None | Some KindMeasure), [arg2], true ->
            let ms1,tpenv = TcMeasure cenv newOk checkCxs env tpenv arg1 m1
            let ms2,tpenv = TcMeasure cenv newOk checkCxs env tpenv arg2 m
            TType_measure (MeasureProd(ms1, ms2)), tpenv

        | _, _, _ ->
            errorR(Error(FSComp.SR.tcTypeParameterInvalidAsTypeConstructor(), m)); 
            NewErrorType (), tpenv

    | SynType.App(_, _, _, m) ->
        errorR(Error(FSComp.SR.tcIllegalSyntaxInTypeExpression(), m));
        NewErrorType (), tpenv

and TcType cenv newOk checkCxs env (tpenv:SyntacticUnscopedTyparEnv) ty = 
    TcTypeOrMeasure (Some KindType) cenv newOk checkCxs env tpenv ty

and TcMeasure cenv newOk checkCxs env (tpenv:SyntacticUnscopedTyparEnv) ty m = 
    match ty with
    | SynType.Anon m ->
        error(Error(FSComp.SR.tcAnonymousUnitsOfMeasureCannotBeNested(), m));
        NewErrorMeasure (), tpenv
    | _ ->
        match TcTypeOrMeasure (Some KindMeasure) cenv newOk checkCxs env tpenv ty with
        | TType_measure ms, tpenv -> ms,tpenv
        | _, _ -> 
            error(Error(FSComp.SR.tcExpectedUnitOfMeasureNotType(), m)); 
            NewErrorMeasure (), tpenv


and TcAnonTypeOrMeasure optKind _cenv rigid dyn newOk m =
    if newOk = NoNewTypars then errorR (Error(FSComp.SR.tcAnonymousTypeInvalidInDeclaration(),m));
    let rigid = (if rigid = TyparAnon && newOk = NewTyparsOKButWarnIfNotRigid then TyparWarnIfNotRigid else rigid)
    let kind = match optKind with Some KindMeasure -> KindMeasure | _ -> KindType
    NewAnonTypar (kind,m,rigid,NoStaticReq,dyn)
 
and TcTypes cenv newOk checkCxs env tpenv args =
    List.mapFold (TcTypeAndRecover cenv newOk checkCxs env) tpenv args 

and TcTypesAsTuple cenv newOk checkCxs env tpenv args m = 
    match args with
    | [] -> error(InternalError("empty tuple type",m))
    | [(_,typ)] -> let typ,tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv typ in [typ],tpenv
    | (isquot,typ)::args -> 
        let ty,tpenv = TcTypeAndRecover cenv newOk checkCxs env tpenv typ
        let tys,tpenv = TcTypesAsTuple cenv newOk checkCxs env tpenv args m
        if isquot then errorR(Error(FSComp.SR.tcUnexpectedSlashInType(),m));
        ty::tys,tpenv

// Type-check a list of measures separated by juxtaposition, * or /
and TcMeasuresAsTuple cenv newOk checkCxs env (tpenv:SyntacticUnscopedTyparEnv) args m = 
  let rec gather args tpenv isquot acc =
    match args with
    | [] -> acc,tpenv
    | (nextisquot,typ)::args -> 
        let ms1,tpenv = TcMeasure cenv newOk checkCxs env tpenv typ m
        gather args tpenv nextisquot (if isquot then MeasureProd(acc,MeasureInv ms1) else MeasureProd(acc,ms1))
  gather args tpenv false MeasureOne


and TcTypesOrMeasures optKinds cenv newOk checkCxs env tpenv args m =
    match optKinds with
    | None ->
        List.mapFold (TcTypeOrMeasure None cenv newOk checkCxs env) tpenv args
    | Some kinds ->
        if List.length kinds = List.length args
        then List.mapFold (fun tpenv (arg,kind) -> TcTypeOrMeasure (Some kind) cenv newOk checkCxs env tpenv arg) tpenv (List.zip args kinds)
        else if kinds.Length = 0
        then error(Error(FSComp.SR.tcUnexpectedTypeArguments(), m))
        else error(Error(FSComp.SR.tcTypeParameterArityMismatch((List.length kinds), (List.length args)), m))

and TcTyparConstraints cenv newOk checkCxs env tpenv wcs =
    // Mark up default constraints with a priority in reverse order: last gets 0, second 
    // last gets 1 etc. See comment on TTyparDefaultsToType 
    let _,tpenv = List.fold (fun (ridx,tpenv) tc -> ridx - 1, TcTyparConstraint ridx cenv newOk checkCxs env tpenv tc) (List.length wcs - 1, tpenv) wcs
    tpenv

// Note the args may only be the instantation of a suffix of the tps. In this case, pathTypeArgs should 
// contain the right instantation for the prefix. However we have to check that the right number of arguments 
// are given!
and TcTypeApp cenv newOk checkCxs env tpenv m tcref pathTypeArgs args =
    CheckTyconAccessible m (AccessRightsOfEnv env) tcref |> ignore;
    CheckEntityAttributes cenv.g tcref m |> CommitOperationResult;
    let tps,_,tinst,_ = info_of_tcref m tcref
    // If we're not checking constraints, i.e. when we first assert the super/interfaces of a type definition, then just 
    // clear the constaint lists of the freshly generated type variables. A little ugly but fairly localized. 
    if checkCxs = NoCheckCxs then tps |> List.iter (fun tp -> tp.Data.typar_constraints <- []);
    if tinst.Length <> pathTypeArgs.Length + args.Length then 
        error (TyconBadArgs(env.DisplayEnv,tcref,pathTypeArgs.Length + args.Length,m));
    let args',tpenv = 
        // Get the suffix of typars
        let tpsForArgs = List.drop (tps.Length - args.Length) tps
        let kindsForArgs = tpsForArgs |> List.map (fun tp -> tp.Kind)
        TcTypesOrMeasures (Some kindsForArgs) cenv newOk checkCxs env tpenv args m
    let args' = pathTypeArgs @ args'
    List.iter2 (UnifyTypes cenv env m) tinst args';
    TType_app(tcref,args'),tpenv
    
and TcTypeOrMeasureAndRecover optKind cenv newOk checkCxs env tpenv ty   =
    try TcTypeOrMeasure optKind cenv newOk checkCxs env tpenv ty 
    with e -> 
        errorRecovery e (ty).Range; 
        (if newOk <> NoNewTypars then NewErrorType () else cenv.g.obj_ty),tpenv 

and TcTypeAndRecover cenv newOk checkCxs env tpenv ty   =
    TcTypeOrMeasureAndRecover (Some KindType) cenv newOk checkCxs env tpenv ty

and TcNestedTypeApplication cenv newOk checkCxs env tpenv m typ tyargs =
    if not (isAppTy cenv.g typ) then error(Error(FSComp.SR.tcTypeHasNoNestedTypes(),m));
    match typ with 
    | TType_app(tcref,tinst) -> 
        let pathTypeArgs,_ = List.chop (max (tinst.Length - tcref.Typars(m).Length) 0) tinst
        TcTypeApp cenv newOk checkCxs env tpenv m tcref pathTypeArgs tyargs 
    | _ -> error(InternalError("TcNestedTypeApplication: expected type application",m))


/// Bind the patterns used in a lambda. Not clear why we don't use TcPat.
and TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) p = 
    match p with 
    | SynSimplePat.Id (id,compgen,isMemberThis,isOpt,m) -> 
        if isOpt && not optArgsOK then errorR(Error(FSComp.SR.tcOptionalArgsOnlyOnMembers(),m));
        if isOpt then 
            let tyarg = NewInferenceType ()
            UnifyTypes cenv env m ty (mkOptionTy cenv.g tyarg);
                
        let _,names,takenNames = TcPatBindingName cenv env id ty isMemberThis None None (OptionalInline,permitInferTypars,noArgOrRetAttribs,false,None,compgen) (names,takenNames)
        id.idText, 
        (tpenv,names,takenNames)

    | SynSimplePat.Typed (p,cty,m) ->
        let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK checkCxs env tpenv cty
        match p with 
        // Optional arguments on members 
        | SynSimplePat.Id(_,_,_,true,_) -> UnifyTypes cenv env m ty (mkOptionTy cenv.g cty');
        | _ -> UnifyTypes cenv env m ty cty';

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
        | SynSimplePat.Id (_,_,_,isOpt,_) -> isOpt
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
        let id = ident("unitVar"^string takenNames.Count,m)
        UnifyTypes cenv env m ty cenv.g.unit_ty;
        let _,names,takenNames = TcPatBindingName cenv env id ty false None None (OptionalInline,permitInferTypars,noArgOrRetAttribs,false,None,true) (names,takenNames)
        [id.idText],(tpenv,names,takenNames)

    | SynSimplePats.SimplePats ([p],_) -> 
        let v,(tpenv,names,takenNames) = TcSimplePat optArgsOK checkCxs cenv ty env (tpenv,names,takenNames) p
        [v],(tpenv,names,takenNames)

    | SynSimplePats.SimplePats (ps,m) -> 
        let ptys = UnifyTupleType cenv env.DisplayEnv m ty ps
        let ps',(tpenv,names,takenNames) = List.mapFold (fun tpenv (ty,e) -> TcSimplePat optArgsOK checkCxs cenv ty env tpenv e) (tpenv,names,takenNames) (List.zip ptys ps)
        ps',(tpenv,names,takenNames)

    | SynSimplePats.Typed (p,cty,m) ->
        let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs env tpenv cty

        match p with 
        // Solitary optional arguments on members 
        | SynSimplePats.SimplePats([SynSimplePat.Id(_,_,_,true,_)],_) -> UnifyTypes cenv env m ty (mkOptionTy cenv.g cty');
        | _ -> UnifyTypes cenv env m ty cty';

        TcSimplePats cenv optArgsOK  checkCxs ty env (tpenv,names,takenNames) p

and TcSimplePatsOfUnknownType cenv optArgsOK checkCxs env tpenv spats =
    let argty = NewInferenceType ()
    TcSimplePats cenv optArgsOK checkCxs argty env (tpenv,NameMap.empty,Set.empty) spats

and TcPatBindingName _cenv _env id ty isMemberThis vis1 topValData (inlineFlag,declaredTypars,argAttribs,isMutable,vis2,compgen) (names,takenNames:Set<string>) = 
    let vis = if isSome vis1 then vis1 else vis2
    if takenNames.Contains id.idText then errorR (VarBoundTwice id);
    let baseOrThis = if isMemberThis then MemberThisVal else NormalVal
    let names = Map.add id.idText (PrelimValScheme1(id,declaredTypars,ty,topValData,None,isMutable,inlineFlag,baseOrThis,argAttribs,vis,compgen)) names
    let takenNames = Set.add id.idText takenNames
    (fun (TcPatPhase2Input values) -> 
        let (vspec,typeScheme) = 
            match values.TryFind id.idText with
            | Some x -> x
            | None -> error(Error(FSComp.SR.tcNameNotBoundInPattern(id.idText),id.idRange))
        PBind(vspec,typeScheme)),
    names,takenNames

/// Typecheck a pattern. Patterns are type-checked in three phases: 
/// 1. TcPat builds a List.map from simple variable names to inferred types for 
///   those variables. It also returns a function to perform the second phase.
/// 2. The second phase assumes the caller has built the actual value_spec's 
///    for the values being defined, and has decided if the types of these 
///    variables are to be generalized. The caller hands this information to
///    the second-phase function in terms of a List.map from names to actual
///    value specifications. 
and TcPat warnOnUpper cenv env topValInfo vFlags (tpenv,names,takenNames) ty pat = 
    let ad = AccessRightsOfEnv env
    match pat with 
    | SynPat.Const (c,m) -> 
        match c with 
        | SynConst.Bytes (bytes,m) -> 
            UnifyTypes cenv env m ty (mkByteArrayTy cenv.g); 
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
        let tgty,tpenv = TcTypeAndRecover cenv NewTyparsOKButWarnIfNotRigid CheckCxs env tpenv cty
        TcRuntimeTypeTest cenv env.DisplayEnv m tgty srcTy;
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
        let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs env tpenv cty
        UnifyTypes cenv env m ty cty';
        TcPat warnOnUpper cenv env topValInfo vFlags (tpenv,names,takenNames) ty p

    | SynPat.Attrib (_,_,m) ->
        error(Error(FSComp.SR.tcAttributesInvalidInPatterns(),m));

    | SynPat.Or (pat1,pat2,m) ->
        let pat1',(tpenv,names1,takenNames1) = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty pat1
        let pat2',(tpenv,names2,takenNames2) = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty pat2
        if not (takenNames1 = takenNames2) then
          // We don't try to recover from this error since we get later bad internal errors during pattern
          // matching 
          error (UnionPatternsBindDifferentNames m);
        names1 |> Map.iter (fun _ (PrelimValScheme1(id1,_,ty1,_,_,_,_,_,_,_,_)) -> 
          match Map.tryFind id1.idText names2 with 
          | None -> () 
          | Some (PrelimValScheme1(_,_,ty2,_,_,_,_,_,_,_,_)) -> 
              UnifyTypes cenv env m ty1 ty2);
        (fun values -> TPat_disjs ([pat1' values;pat2' values],m)), (tpenv,names1,takenNames1)

    | SynPat.Ands (pats,m) ->
        let pats',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) (List.map (fun _ -> ty) pats) pats
        (fun values -> TPat_conjs(List.map (fun f -> f values) pats',m)), acc

    | SynPat.LongIdent (lid,_,tyargs,args,vis,m) ->
        if isSome tyargs then errorR(Error(FSComp.SR.tcInvalidTypeArgumentUsage(),m));
        let warnOnUpperForId = if isNil args then warnOnUpper else AllIdsOK
        begin match ResolvePatternLongIdent cenv.nameResolver warnOnUpperForId false m ad env.eNameResEnv DefaultTypeNameResInfo lid with
        | Item.NewDef id -> 
            match args with 
            | [] -> TcPat warnOnUpperForId cenv env topValInfo vFlags (tpenv,names,takenNames) ty (mkSynPatVar vis id)
            | _ -> error (UndefinedName(0,FSComp.SR.undefinedNamePatternDiscriminator,id,[]))

        | Item.ActivePatternCase(APElemRef(apinfo,vref,idx)) as item -> 
            // TOTAL/PARTIAL ACTIVE PATTERNS 
            let vexp, _, _, tinst, _ = TcVal cenv env tpenv vref None m
            let vexp = MakeApplicableExprWithFlex cenv env vexp
            let vexpty = vexp.Type

            let activePatArgsAsSynPats,patarg = 
                match args with 
                | [] -> [],SynPat.Const(SynConst.Unit,m) 
                | _ -> 
                    // This bit of type-directed analysis ensures that parameterized partial active patterns returning unit do not need to take an argument
                    // See FSharp 1.0 3502
                    let dtys,rty = stripFunTy cenv.g vexpty
                    
                    if dtys.Length = args.Length + 1 && isOptionTy cenv.g rty &&  isUnitTy cenv.g (destOptionTy cenv.g rty)  then 
                        args,SynPat.Const(SynConst.Unit,m) 
                    else 
                        List.frontAndBack args

            if nonNil activePatArgsAsSynPats && apinfo.ActiveTags.Length <> 1 then 
                error(Error(FSComp.SR.tcRequireActivePatternWithOneResult(),m));

            // Parse the arguments to an active pattern
            // Note we parse arguments to parameterized pattern labels as patterns, not expressions. 
            // This means the range of syntactic expression forms that can be used here is limited. 
            let rec convSynPatToSynExpr x = 
                match x with
                | SynPat.Const (c,m) -> SynExpr.Const(c,m)
                | SynPat.Named (SynPat.Wild _,id,_,None,_) -> SynExpr.Ident(id)
                | SynPat.Typed (p,cty,m) -> SynExpr.Typed (convSynPatToSynExpr p,cty,m)
                | SynPat.LongIdent (lid,_,_tyargs,args,None,m) -> List.fold (fun f x -> SynExpr.App(ExprAtomicFlag.NonAtomic, f,convSynPatToSynExpr x,m)) (SynExpr.LongIdent(false,lid,m)) args
                | SynPat.Tuple (args,m) -> SynExpr.Tuple(List.map convSynPatToSynExpr args,m)
                | SynPat.Paren (p,_) -> convSynPatToSynExpr p
                | SynPat.ArrayOrList (isArray,args,m) -> SynExpr.ArrayOrList(isArray,List.map convSynPatToSynExpr args,m)
                | SynPat.QuoteExpr (e,_) -> e
                | SynPat.Null m -> SynExpr.Null(m)
                | _ -> error(Error(FSComp.SR.tcInvalidArgForParameterizedPattern(),x.Range))
            let activePatArgsAsSynExprs = List.map convSynPatToSynExpr activePatArgsAsSynPats

            let activePatResTys = NewInferenceTypes apinfo.Names
            let activePatType = apinfo.OverallType cenv.g m ty activePatResTys 

            let delayed = activePatArgsAsSynExprs |> List.map (fun e -> DelayedApp(ExprAtomicFlag.NonAtomic, e, e.Range)) 
            let activePatExpr, tpenv = PropagateThenTcDelayed cenv activePatType env tpenv m vexp vexpty ExprAtomicFlag.NonAtomic delayed

            if idx >= activePatResTys.Length then error(Error(FSComp.SR.tcInvalidIndexIntoActivePatternArray(),m));
            let argty = List.nth activePatResTys idx 
                
            let arg',(tpenv,names,takenNames) = TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) argty patarg
            
            // The identity of an active pattern consists of its value and the types it is applied to.
            // If there are any expression args then we've lost identity. 
            let activePatIdentity = (if nonNil activePatArgsAsSynExprs then None else Some (vref, tinst))
            (fun values -> 
                // Report information about the 'active recognizer' occurence to IDE
                CallNameResolutionSink(rangeOfLid lid,env.NameEnv,item,ItemOccurence.Pattern,env.DisplayEnv,AccessRightsOfEnv env)
                TPat_query((activePatExpr, activePatResTys, activePatIdentity, idx, apinfo), arg' values, m)), 
            (tpenv,names,takenNames)

        | (Item.UnionCase _ | Item.ExnCase _) as item ->
            // DATA MATCH CONSTRUTORS 
            let mkf,argtys = ApplyUnionCaseOrExnTypesForPat m cenv env ty item
            let nargtys = argtys.Length

            let args = 
              match args with 
              | []-> []
              // note: the next will always be parenthesized 
              | [(SynPat.Tuple (args,_)) | SynPat.Paren(SynPat.Tuple (args,_),_)] when nargtys > 1 -> args

              // note: we allow both 'C _' and 'C (_)' regardless of number of argument of the pattern 
              | [(SynPat.Wild _ as e) | SynPat.Paren(SynPat.Wild _ as e,_)] -> Array.toList (Array.create nargtys e)
              | [arg] -> [arg] 
              | _ when nargtys = 0 -> error(Error(FSComp.SR.tcUnionCaseDoesNotTakeArguments(),m)) 
              | _ when nargtys = 1 -> error(Error(FSComp.SR.tcUnionCaseRequiresOneArgument(),m)) 
              | _ -> error(Error(FSComp.SR.tcUnionCaseExpectsTupledArguments(nargtys),m))
            UnionCaseOrExnCheck env nargtys args.Length m;

            let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) argtys args
            (fun values -> 
                // Report information about the case occurence to IDE
                CallNameResolutionSink(rangeOfLid lid,env.NameEnv,item,ItemOccurence.Pattern,env.DisplayEnv,AccessRightsOfEnv env)
                mkf(List.map (fun f -> f values) args')), acc
                
        | Item.ILField finfo ->
            // LITERAL .NET FIELDS 
            CheckILFieldInfoAccessible cenv.g cenv.amap m (AccessRightsOfEnv env) finfo;
            if not finfo.IsStatic then errorR (Error (FSComp.SR.tcFieldIsNotStatic(finfo.FieldName),m));
            CheckILFieldAttributes cenv.g finfo m;
            match finfo.LiteralValue with 
            | None -> error (Error(FSComp.SR.tcFieldNotLiteralCannotBeUsedInPattern(), m));
            | Some lit -> 
                UnifyTypes cenv env m ty (finfo.FieldType(cenv.amap,m))
                let c' = TcFieldInit m lit
                (fun _ -> TPat_const (c',m)),(tpenv,names,takenNames)             
            
        | Item.RecdField rfinfo ->
            // LITERAL F# FIELDS 
            CheckRecdFieldInfoAccessible m (AccessRightsOfEnv env) rfinfo;
            if not rfinfo.IsStatic then errorR (Error (FSComp.SR.tcFieldIsNotStatic(rfinfo.Name),m));
            CheckRecdFieldInfoAttributes cenv.g rfinfo m  |> CommitOperationResult;        
            match rfinfo.LiteralValue with 
            | None -> error (Error(FSComp.SR.tcFieldNotLiteralCannotBeUsedInPattern(), m));
            | Some lit -> 
                UnifyTypes cenv env m ty rfinfo.FieldType;
                (fun _ -> TPat_const (lit,m)),(tpenv,names,takenNames)             

        | Item.Value vref ->
            match vref.LiteralValue with 
            | None -> error (Error(FSComp.SR.tcNonLiteralCannotBeUsedInPattern(), m));
            | Some lit -> 
                let (_, _, vexpty, _, _) = TcVal cenv env tpenv vref None m
                CheckValAccessible m (AccessRightsOfEnv env) vref;
                CheckFSharpAttributes cenv.g vref.Attribs m |> CommitOperationResult;
                UnifyTypes cenv env m ty vexpty;
                (fun _ -> TPat_const (lit,m)),(tpenv,names,takenNames)             

        |  _ -> error (Error(FSComp.SR.tcRequireVarConstRecogOrLiteral(),m))
        end

    | SynPat.QuoteExpr(_,m) -> error (Error(FSComp.SR.tcInvalidPattern(),m))
          
    | SynPat.Tuple (args,m) ->
        let argtys = NewInferenceTypes args
        UnifyTypes cenv env m ty (TType_tuple argtys);
        let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) argtys args
        (fun values -> TPat_tuple(List.map (fun f -> f values) args',argtys,m)), acc

    | SynPat.Paren (p,_) ->
        TcPat warnOnUpper cenv env None vFlags (tpenv,names,takenNames) ty p

    | SynPat.ArrayOrList (isArray,args,m) ->
        let argty = NewInferenceType ()
        UnifyTypes cenv env m ty (if isArray then mkArrayType cenv.g argty else Tastops.mkListTy cenv.g argty);
        let args',acc = TcPatterns warnOnUpper cenv env vFlags (tpenv,names,takenNames) (List.map (fun _ -> argty) args) args
        (fun values -> 
            let args' = List.map (fun f -> f values) args'
            if isArray then TPat_array(args', argty, m)
            else List.foldBack (mkConsListPat cenv.g argty) args' (mkNilListPat cenv.g m argty)), acc

    | SynPat.Record (flds,m) ->
        let tcref,fldsmap,_fldsList  = BuildFieldMap cenv env true ty flds m
        let _,inst,tinst,gtyp = info_of_tcref m tcref
        UnifyTypes cenv env m ty gtyp;
        let fields = tcref.TrueInstanceFieldsAsList
        let ftys = List.map (fun fsp -> FreshenPossibleForallTy cenv.g m TyparFlexible (actualTyOfRecdField inst fsp),fsp) fields
        let fldsmap',acc = 
          List.mapFold 
            (fun s ((_,ftinst,ty),fsp) -> 
              if Map.containsKey fsp.rfield_id.idText  fldsmap then 
                let f,s = TcPat warnOnUpper cenv env None vFlags s ty (Map.find fsp.rfield_id.idText fldsmap)
                (ftinst,f),s
              else ([],(fun _ -> TPat_wild m)),s)
            (tpenv,names,takenNames)
            ftys
        (fun values -> TPat_recd (tcref,tinst,List.map (fun (ftinst,f) -> (ftinst,f values)) fldsmap',m)), 
        acc

    | SynPat.DeprecatedCharRange (c1,c2,m) -> 
        errorR(Deprecated(FSComp.SR.tcUseWhenPatternGuard(),m));
        UnifyTypes cenv env m ty (cenv.g.char_ty);
        (fun _ -> TPat_range(c1,c2,m)),(tpenv,names,takenNames)

    | SynPat.Null m -> 
        AddCxTypeMustSupportNull env.DisplayEnv cenv.css m NoTrace ty;
        (fun _ -> TPat_null m),(tpenv,names,takenNames)

    | SynPat.InstanceMember (_,_,_,_,m) -> 
        errorR(Error(FSComp.SR.tcIllegalPattern(),pat.Range));
        (fun _ -> TPat_wild m), (tpenv,names,takenNames)

and TcPatterns warnOnUpper cenv env vFlags s argtys args = 
    assert (List.length args  = List.length argtys);
    List.mapFold (fun s (ty,pat) -> TcPat warnOnUpper cenv env None vFlags s ty pat) s (List.zip argtys args)


and solveTypAsError cenv denv m ty =
    let ty2 = NewErrorType ()
    assert((destTyparTy cenv.g ty2).IsFromError);
    SolveTypEqualsTypKeepAbbrevs (MakeConstraintSolverEnv cenv.css m denv) 0 m NoTrace ty ty2 |> ignore

and TcExprOfUnknownType cenv env tpenv expr =
    let exprty = NewInferenceType ()
    let expr',tpenv = TcExpr cenv exprty env tpenv expr
    expr',exprty,tpenv

and TcExprFlex cenv flex ty (env: TcEnv) tpenv (e: SynExpr) =
    if flex then
        let argty = NewInferenceType ()
        AddCxTypeMustSubsumeType env.DisplayEnv cenv.css e.Range NoTrace ty argty ;
        let e',tpenv  = TcExpr cenv argty env tpenv e 
        let e' = mkCoerceIfNeeded cenv.g ty argty e'
        e',tpenv
    else
        TcExpr cenv ty env tpenv e
    

and TcExpr cenv ty (env: TcEnv) tpenv (expr: SynExpr) =

    let m = expr.Range

    // Start an error recovery handler 
    // Note the try/catch can lead to tail-recursion problems for iterated constructs, e.g. let... in... 
    // So be careful! 
    try 
        // Count our way through the expression shape that makes up an object constructor 
        // See notes at definition of "ctor" re. object model constructors. 
        let env = 
            if GetCtorShapeCounter env > 0 then AdjustCtorShapeCounter (fun x -> x - 1) env 
            else env

        let tm,tpenv = TcExprThen cenv ty env tpenv expr []

        tm,tpenv
    with e -> 

        // Error recovery - return some rubbish expression, but replace/annotate 
        // the type of the current expression with a type variable that indicates an error 
        errorRecovery e m; 
        solveTypAsError cenv env.DisplayEnv m ty;
        mkThrow m ty (mkOne cenv.g m), tpenv


/// This is used to typecheck legitimate 'main body of constructor' expressions 
and TcExprThatIsCtorBody safeInitInfo cenv ty env tpenv expr =
    let env = {env with eCtorInfo = Some (InitialExplicitCtorInfo safeInitInfo) }
    let expr,tpenv = TcExpr cenv ty env tpenv expr
    let expr = CheckAndRewriteObjectCtor cenv.g env expr
    expr,tpenv

/// This is used to typecheck all ordinary expressions including constituent 
/// parts of ctor. 
and TcExprThatCanBeCtorBody cenv ty env tpenv expr =
    let env = if AreWithinCtorShape env then AdjustCtorShapeCounter (fun x -> x + 1) env else env
    TcExpr cenv ty env tpenv expr

/// This is used to typecheck legitimate 'non-main body of object constructor' expressions 
and TcExprThatCantBeCtorBody cenv ty env tpenv expr =
    let env = if AreWithinCtorShape env then ExitCtorShapeRegion env else env
    TcExpr cenv ty env tpenv expr

/// This is used to typecheck legitimate 'non-main body of object constructor' expressions 
and TcStmtThatCantBeCtorBody cenv env tpenv expr =
    let env = if AreWithinCtorShape env then ExitCtorShapeRegion env else env
    TcStmt cenv env tpenv expr

and TcStmt cenv env tpenv expr =
    let expr',ty,tpenv = TcExprOfUnknownType cenv env tpenv expr
    let m = expr.Range
    let wasUnit = UnifyUnitType cenv env.DisplayEnv m ty (Some expr');
    if wasUnit then
      expr',tpenv
    else
      mkCompGenSeq m expr' (mkUnit cenv.g m),tpenv



/// During checking of expressions of the form (x(y)).z(w1,w2) 
/// keep a stack of things on the right. This lets us recognize 
/// method applications. 
and TcExprThen cenv ty env tpenv expr delayed =
    match expr with 

    | LongOrSingleIdent (isOpt,lid,m) ->
        if isOpt then errorR(Error(FSComp.SR.tcSyntaxErrorUnexpectedQMark(),m));
        TcLongIdentThen cenv ty env tpenv lid delayed

    | SynExpr.App (hpa,f,x,m) ->
        TcExprThen cenv ty env tpenv f ((DelayedApp (hpa,x,m)):: delayed)

    | SynExpr.TypeApp (f,x,m) ->
        TcExprThen cenv ty env tpenv f ((DelayedTypeApp (x,m)):: delayed)

    | SynExpr.DotGet (e1,lid,m) ->
        TcExprThen cenv ty env tpenv e1 ((DelayedDotLookup (lid,m))::delayed)
           
    | SynExpr.DotIndexedGet (e1,idx,mDot,m) 
    | SynExpr.DotIndexedSet (e1,idx,_,mDot,m) ->
        TcIndexerThen cenv env ty m mDot tpenv expr e1 idx delayed
    
    | _  ->
        match delayed with 
        | [] -> TcExprUndelayed cenv ty env tpenv expr
        | _ -> 
            let expr',exprty,tpenv = TcExprUndelayedNoType cenv env tpenv expr
            PropagateThenTcDelayed cenv ty env tpenv expr'.Range (MakeApplicableExprNoFlex cenv expr') exprty ExprAtomicFlag.NonAtomic delayed

and TcExprs cenv env m tpenv flexes argtys args = 
    if (List.length args  <> List.length argtys) then error(Error(FSComp.SR.tcExpressionCountMisMatch((List.length argtys), (List.length args)),m));
    (tpenv, List.zip3 flexes argtys args) ||> List.mapFold (fun tpenv (flex,ty,e) -> 
         TcExprFlex cenv flex ty env tpenv e)


//-------------------------------------------------------------------------
// TcExprUndelayed
//------------------------------------------------------------------------- 

and TcExprUndelayedNoType cenv env tpenv expr : Expr * TType * _ =
    let exprty = NewInferenceType ()
    let expr',tpenv = TcExprUndelayed cenv exprty env tpenv expr
    expr',exprty,tpenv

and TcExprUndelayed cenv ty env tpenv (expr: SynExpr) =
    let m = expr.Range

    match expr with 
    | SynExpr.Paren (expr2,_) -> 
        // We invoke CallExprHasTypeSink for every construct which is atomic in the syntax, i.e. where a '.' immediately following the 
        // construct is a dot-lookup for the result of the construct. 
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);
        TcExpr cenv ty env tpenv expr2

    | SynExpr.DotIndexedGet _ | SynExpr.DotIndexedSet _
    | SynExpr.TypeApp _ | SynExpr.Ident _ | SynExpr.LongIdent _ | SynExpr.App _ | SynExpr.DotGet _ -> error(Error(FSComp.SR.tcExprUndelayed(), m))

    | SynExpr.Const (SynConst.String (s,m),_) -> 
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);
        TcConstStringExpr cenv ty env m tpenv s

    | SynExpr.Const (c,m) -> 
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);
        TcConstExpr cenv ty env m tpenv c

    | SynExpr.Lambda _ -> TcIteratedLambdas cenv true env ty Set.empty tpenv expr

    | SynExpr.Match (spMatch,x,matches,isExnMatch,m) ->
        let x',inputTy,tpenv = TcExprOfUnknownType cenv env tpenv x
        let exprm = x'.Range
        let v,e, tpenv = TcAndPatternCompileMatchClauses exprm m (if isExnMatch then Throw else ThrowIncompleteMatchException) cenv inputTy ty env tpenv matches
        (mkLet spMatch exprm v x'  e,tpenv)

    | SynExpr.Assert (x,m) ->
        TcAssertExpr cenv ty env m tpenv x

    | SynExpr.Typed (e,cty,m) ->
        let tgty,tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs env tpenv cty
        UnifyTypes cenv env m ty tgty;
        let e',tpenv = TcExpr cenv ty env tpenv e 
        e',tpenv

    | SynExpr.TypeTest (e,tgty,m) ->
        let e',srcTy,tpenv = TcExprOfUnknownType cenv env tpenv e 
        UnifyTypes cenv env m ty cenv.g.bool_ty;
        let tgty,tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv tgty
        TcRuntimeTypeTest cenv env.DisplayEnv m tgty srcTy;        
        let e' = mkCallTypeTest cenv.g m tgty  e'
        e', tpenv
    
    // SynExpr.AddressOf is noted in the syntax ast in order to recognize it as concrete type information 
    // during type checking, in particular prior to resolving overloads. This helps distinguish 
    // its use at method calls from the use of the conflicting 'ref' mechanism for passing byref parameters 
    | SynExpr.AddressOf(byref,e,opm,m) -> 
        TcExpr cenv ty env tpenv (mkSynPrefix opm m (if byref then "~&" else "~&&") e) 
        
    | SynExpr.Upcast (e,_,m) | SynExpr.InferredUpcast (e,m) -> 
        let e',srcTy,tpenv = TcExprOfUnknownType cenv env tpenv e 
        let tgty,tpenv = 
          match expr with
          | SynExpr.Upcast (_,tgty,m) -> 
              let tgty,tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv tgty
              UnifyTypes cenv env m tgty ty;
              tgty,tpenv
          | SynExpr.InferredUpcast _ -> 
              ty,tpenv 
          | _ -> failwith "upcast"
        TcStaticUpcast cenv env.DisplayEnv m tgty srcTy;
        mkCoerceExpr(e',tgty,m,srcTy),tpenv

    | SynExpr.Downcast(e,_,m) | SynExpr.InferredDowncast (e,m) ->
        let e',srcTy,tpenv = TcExprOfUnknownType cenv env tpenv e 
        let tgty,tpenv = 
          match expr with
          | SynExpr.Downcast (_,tgty,m) -> 
              let tgty,tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv tgty
              UnifyTypes cenv env m tgty ty;
              tgty,tpenv
          | SynExpr.InferredDowncast _ -> ty,tpenv 
          | _ -> failwith "downcast"
        TcRuntimeTypeTest cenv env.DisplayEnv m tgty srcTy;

        // TcRuntimeTypeTest ensures tgty is a nominal type. Hence we can insert a check here 
        // based on the nullness semantics of the nominal type. 
        let e' = mkCallUnbox cenv.g m tgty  e'
        e',tpenv

    | SynExpr.Null (m) ->
        AddCxTypeMustSupportNull env.DisplayEnv cenv.css m NoTrace ty;
        mkNull m ty,tpenv

    | SynExpr.Lazy (e,m) ->
        let ety = NewInferenceType ()
        UnifyTypes cenv env m ty (mkLazyTy cenv.g ety);
        let e',tpenv = TcExpr cenv ety env tpenv e 
        mkLazyDelayed cenv.g m ety (mkUnitDelayLambda cenv.g m e'), tpenv

    | SynExpr.Tuple (args,m) -> 
        let argtys = UnifyTupleType cenv env.DisplayEnv m ty args
        // No subsumption at tuple construction
        let flexes = argtys |> List.map (fun _ -> false)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        mkTupled cenv.g m args' argtys, tpenv

    | SynExpr.ArrayOrList (isArray,args,m) -> 
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);

        let argty = NewInferenceType ()
        UnifyTypes cenv env m ty (if isArray then mkArrayType cenv.g argty else Tastops.mkListTy cenv.g argty);

        // Always allow subsumption if a nominal type is known prior to type checking any arguments
        let flex = not (isTyparTy cenv.g argty)
        let args',tpenv = List.mapFold (TcExprFlex cenv flex argty env) tpenv args
        
        let expr = 
            if isArray then Expr.Op(TOp.Array, [argty],args',m)
            else List.foldBack (mkCons cenv.g argty) args' (mkNil cenv.g m argty)
        expr,tpenv

    | SynExpr.New (superInit,objTy,arg,m) -> 
        let objTy',tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv objTy
        UnifyTypes cenv env m ty objTy';
        TcNewExpr cenv env tpenv objTy' (Some objTy.Range) superInit arg m

    | SynExpr.ObjExpr(objTy,argopt,binds,extraImpls,m) ->
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);
        TcObjectExpr cenv ty env tpenv (objTy,argopt,binds,extraImpls,m)
            
    | SynExpr.Record (inherits,optOrigExpr, flds, m) -> 
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);
        TcRecdExpr cenv ty env tpenv (inherits,optOrigExpr,flds,m)

    | SynExpr.While (spWhile,e1,e2,m) ->
        UnifyTypes cenv env m ty cenv.g.unit_ty;
        let e1',tpenv = TcExpr cenv (cenv.g.bool_ty) env tpenv e1
        let e2',tpenv = TcStmt cenv env tpenv e2
        mkWhile cenv.g (spWhile,NoSpecialWhileLoopMarker,e1',e2',m),tpenv

    | SynExpr.For (spBind,id,start,dir,finish,body,m) ->
        UnifyTypes cenv env m ty cenv.g.unit_ty;
        let startExpr ,tpenv = TcExpr cenv (cenv.g.int_ty) env tpenv start
        let finishExpr,tpenv = TcExpr cenv (cenv.g.int_ty) env tpenv finish
        let idv,_ = mkLocal id.idRange  id.idText cenv.g.int_ty
        let envinner = AddLocalVal m idv env
        let bodyExpr,tpenv = TcStmt cenv envinner tpenv body
        mkFastForLoop  cenv.g (spBind,m,idv,startExpr,dir,finishExpr,bodyExpr), tpenv
        
    | SynExpr.ForEach (spBind,SeqExprOnly(seqExprOnly),pat,enumExpr,body,m) ->
        if seqExprOnly then warning (Error(FSComp.SR.tcExpressionRequiresSequence(),m));
        TcForEachExpr cenv ty env tpenv (pat,enumExpr,body,m,spBind)

    | SynExpr.CompExpr (isArrayOrList,isNotNakedRefCell,comp,m) ->
        let env = ExitFamilyRegion env
        if not isArrayOrList then 
            match comp with 
            | SynExpr.New _ -> 
                errorR(Error(FSComp.SR.tcInvalidObjectExpressionSyntaxForm(),m));
            | SimpleSemicolonSequence false _ -> 
                errorR(Error(FSComp.SR.tcInvalidObjectSequenceOrRecordExpression(),m));
            | _ -> 
                ()
        if not !isNotNakedRefCell && not cenv.g.compilingFslib then 
            error(Error(FSComp.SR.tcInvalidSequenceExpressionSyntaxForm(),m));
        
        TcComputationExpression cenv env ty m None tpenv comp
        
    | SynExpr.ArrayOrListOfSeqExpr (isArray,comp,m)  ->
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);

        
        match comp with 
        | SynExpr.CompExpr(_,_,(SimpleSemicolonSequence true elems as body),_) -> 
            match body with 
            | SimpleSemicolonSequence false _ -> 
                ()
            | _ -> 
                errorR(Deprecated(FSComp.SR.tcExpressionWithIfRequiresParenthesis(),m));

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
                    if List.length elems > 500 then 
                        error(Error(FSComp.SR.tcListLiteralMaxSize(),m));
                    SynExpr.ArrayOrList(isArray, elems, m)

            TcExprUndelayed cenv ty env tpenv replacementExpr
        | _ -> 
            let genCollElemTy = NewInferenceType ()
            let genCollTy =  (if isArray then mkArrayType else mkListTy) cenv.g genCollElemTy
            UnifyTypes cenv env m ty genCollTy;
            let exprty = NewInferenceType ()
            let genEnumTy =  mkSeqTy cenv.g genCollElemTy
            AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace genEnumTy exprty; 
            let expr,tpenv = TcExpr cenv exprty env tpenv comp
            let expr = mkCoerceIfNeeded cenv.g genEnumTy (tyOfExpr cenv.g expr) expr
            (if isArray then mkCallSeqToArray else mkCallSeqToList) cenv.g m genCollElemTy 
                // We add a call to 'seq ... ' to make sure sequence expression compilation gets applied to the contents of the
                // comprehension. But don't do this in FSharp.Core.dll since 'seq' may not yet be defined.
                ((if cenv.g.compilingFslib then id else mkCallSeq cenv.g m genCollElemTy)
                    (mkCoerceExpr(expr,genEnumTy,expr.Range,exprty))),tpenv

    | SynExpr.LetOrUse (isRec,isUse,binds,body,m) ->
        TcLinearLetExprs (TcExprThatCanBeCtorBody cenv) cenv env ty (fun x -> x) tpenv (true(*consume use bindings*),isRec,isUse,binds,body,m) 

    | SynExpr.TryWith (e1,_mTryToWith,clauses,mWithToLast,mTryToLast,spTry,spWith) ->
        let e1',tpenv = TcExpr cenv ty env tpenv e1
        // Compile the pattern twice, once as a List.filter with all succeeding targets returning "1", and once as a proper catch block. 
        let filterClauses = clauses |> List.map (function (Clause(pat,optWhenExpr,_,m,_)) -> Clause(pat,optWhenExpr,(SynExpr.Const(SynConst.Int32 1,m)),m,SuppressSequencePointAtTarget))
        let checkedFilterClauses, tpenv = TcMatchClauses cenv cenv.g.exn_ty cenv.g.int_ty env tpenv filterClauses
        let checkedHandlerClauses, tpenv = TcMatchClauses cenv cenv.g.exn_ty ty env tpenv clauses
        let v1,filter_expr = CompilePatternForMatchClauses cenv env mWithToLast mWithToLast true FailFilter cenv.g.exn_ty cenv.g.int_ty checkedFilterClauses
        let v2,handler_expr = CompilePatternForMatchClauses cenv env mWithToLast mWithToLast true Rethrow cenv.g.exn_ty ty checkedHandlerClauses
        mkTryWith cenv.g (e1',v1,filter_expr,v2,handler_expr,mTryToLast,ty,spTry,spWith),tpenv

    | SynExpr.TryFinally (e1,e2,mTryToLast,spTry,spFinally) ->
        let e1',tpenv = TcExpr cenv ty env tpenv e1
        let e2',tpenv = TcStmt cenv env tpenv e2
        mkTryFinally cenv.g (e1',e2',mTryToLast,ty,spTry,spFinally),tpenv

    | SynExpr.ArbitraryAfterError m -> 
        mkDefault(m,ty), tpenv

    | SynExpr.DiscardAfterError (e1,m) -> 
        let _,_,tpenv = TcExprOfUnknownType cenv env tpenv e1
        mkDefault(m,ty),tpenv

    | SynExpr.Seq (sp,dir,e1,e2,m) ->
        if dir then 
            // Use continuations to cope with long linear sequences 
            let rec TcLinearSeqs expr cont = 
                match expr with 
                | SynExpr.Seq (sp,true,e1,e2,m) ->
                  let e1',_ = TcStmtThatCantBeCtorBody cenv env tpenv e1
                  TcLinearSeqs e2 (fun (e2',tpenv) -> 
                      cont (Expr.Seq(e1',e2',NormalSeq,sp,m),tpenv))

                | _ -> 
                  cont (TcExprThatCanBeCtorBody cenv ty env tpenv expr)
            TcLinearSeqs expr (fun res -> res)
        else 
          // Constructors using "new (...) = <ctor-expr> then <expr>" 
          let e1',tpenv = TcExprThatCanBeCtorBody cenv ty env tpenv e1
          if (GetCtorShapeCounter env) <> 1 then 
              errorR(Error(FSComp.SR.tcExpressionFormRequiresObjectConstructor(),m));
          let e2',tpenv = TcStmtThatCantBeCtorBody cenv env tpenv e2
          Expr.Seq(e1',e2',ThenDoSeq,sp,m),tpenv
    | SynExpr.Do (e1,m) ->
          UnifyTypes cenv env m ty cenv.g.unit_ty;
          TcStmtThatCantBeCtorBody cenv env tpenv e1

    | SynExpr.IfThenElse (e1,e2,e3opt,spIfToThen,mIfToThen,m) ->
        let e1',tpenv = TcExprThatCantBeCtorBody cenv cenv.g.bool_ty env tpenv e1  
        (if isNone e3opt then UnifyTypes cenv env m ty cenv.g.unit_ty);
        let e2',tpenv = TcExprThatCanBeCtorBody cenv ty env tpenv e2
        let e3',sp2,tpenv = 
            match e3opt with 
            | None -> 
                mkUnit cenv.g mIfToThen,SuppressSequencePointAtTarget, tpenv // the fake 'unit' value gets exactly the same range as spIfToThen
            | Some e3 -> 
                let e3',tpenv = TcExprThatCanBeCtorBody cenv ty env tpenv e3 
                e3',SequencePointAtTarget,tpenv
        primMkCond spIfToThen SequencePointAtTarget sp2 m ty e1' e2' e3', tpenv

    // This is for internal use in the libraries only 
    | SynExpr.LibraryOnlyStaticOptimization (constraints,e2,e3,m) ->
        let constraints',tpenv = List.mapFold (TcStaticOptimizationConstraint cenv env) tpenv constraints
        // Do not force the types of the two expressions to be equal 
        // This means uses of this construct have to be very carefully written
        let e2',_, tpenv = TcExprOfUnknownType cenv env tpenv e2
        let e3',tpenv = TcExpr cenv ty env tpenv e3
        Expr.StaticOptimization(constraints',e2',e3',m), tpenv

    | SynExpr.DotSet (e1,f,e2,m) ->
        TcExprThen cenv ty env tpenv e1 [DelayedDotLookup(f,unionRanges e1.Range (rangeOfLid f)); MakeDelayedSet(e2,m)]

    | SynExpr.DotNamedIndexedPropertySet (e1,f,e2,e3,m) ->
        TcExprThen cenv ty env tpenv e1 [DelayedDotLookup(f,unionRanges e1.Range (rangeOfLid f)); DelayedApp(ExprAtomicFlag.Atomic,e2,m); MakeDelayedSet(e3,m)]

    | SynExpr.LongIdentSet (lid,e2,m) -> 
        TcLongIdentThen cenv ty env tpenv lid [ MakeDelayedSet(e2, m) ]
    
    // Type.Items(e1) <- e2 
    | SynExpr.NamedIndexedPropertySet (lid,e1,e2,m) ->
        TcLongIdentThen cenv ty env tpenv lid [ DelayedApp(ExprAtomicFlag.Atomic,e1,m); MakeDelayedSet(e2,m) ]

    | SynExpr.TraitCall(tps,memSpfn,arg,m) ->
        let (TTrait(_,logicalCompiledName,_,argtys,returnTy,_) as traitInfo),tpenv = TcPseudoMemberSpec cenv NewTyparsOK env tps  tpenv memSpfn m
        if List.mem logicalCompiledName BakedInTraitConstraintNames then 
            warning(BakedInMemberConstraintName(logicalCompiledName,m))
        
        let returnTy = GetFSharpViewOfReturnType cenv.g returnTy
        let args,namedCallerArgs = GetMethodArgs arg 
        if nonNil namedCallerArgs then errorR(Error(FSComp.SR.tcNamedArgumentsCannotBeUsedInMemberTraits(),m));
        // Subsumption at trait calls if arguments have nominal type prior to unification of any arguments or return type
        let flexes = argtys |> List.map (isTyparTy cenv.g >> not)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        AddCxMethodConstraint env.DisplayEnv cenv.css m NoTrace traitInfo;
        UnifyTypes cenv env m ty returnTy;      
        Expr.Op(TOp.TraitCall(traitInfo), [], args', m), tpenv
          
    | SynExpr.DeprecatedTypeOf(sty,m) ->
        if not cenv.g.compilingFslib then 
            errorR(Deprecated(FSComp.SR.tcUseTypeOf(),m));
        let sty',tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv sty
        UnifyTypes cenv env m ty cenv.g.system_Type_typ;      
        mkCallTypeOf cenv.g m sty', tpenv

    | SynExpr.LibraryOnlyUnionCaseFieldGet (e1,c,n,m) ->
        let e1',ty1,tpenv = TcExprOfUnknownType cenv env tpenv e1
        let mkf,ty2 = TcUnionCaseField cenv env ty1 m c n 
                          ((fun (a,b) n -> mkUnionCaseFieldGetUnproven(e1',a,b,n,m)),
                           (fun a n -> mkExnCaseFieldGet(e1',a,n,m)))
        UnifyTypes cenv env m ty ty2;
        mkf n,tpenv

    | SynExpr.LibraryOnlyUnionCaseFieldSet (e1,c,n,e2,m) ->
        UnifyTypes cenv env m ty cenv.g.unit_ty;
        let e1',ty1,tpenv = TcExprOfUnknownType cenv env tpenv e1
        let mkf,ty2 = TcUnionCaseField cenv  env ty1 m c n
                          ((fun (a,b) n e2' -> 
                             if not (isUnionCaseFieldMutable cenv.g a n) then errorR(Error(FSComp.SR.tcFieldIsNotMutable(),m));
                             mkUnionCaseFieldSet(e1',a,b,n,e2',m)),
                           (fun a n e2' -> 
                             if not (isExnFieldMutable a n) then errorR(Error(FSComp.SR.tcFieldIsNotMutable(),m));
                             mkExnCaseFieldSet(e1',a,n,e2',m)))
        let e2',tpenv = TcExpr cenv ty2 env tpenv e2
        mkf n e2',tpenv

    | SynExpr.LibraryOnlyILAssembly (s,tyargs,args,rtys,m) ->
        let argtys = NewInferenceTypes args
        let tyargs',tpenv = TcTypes cenv NewTyparsOK CheckCxs env tpenv tyargs
        // No subsumption at uses of IL assembly code
        let flexes = argtys |> List.map (fun _ -> false)
        let args',tpenv = TcExprs cenv env m tpenv flexes argtys args
        let rtys',tpenv = TcTypes cenv NewTyparsOK CheckCxs env tpenv rtys
        let returnTy = 
            match rtys' with 
            | [] -> cenv.g.unit_ty
            | [ returnTy ] -> returnTy
            | _ -> error(InternalError("Only zero or one pushed items are permitted in IL assembly code",m))
        UnifyTypes cenv env m ty returnTy;
        mkAsmExpr(Array.toList s,tyargs',args',rtys',m),tpenv

    | SynExpr.Quote(oper,raw,ast,m) ->
        CallExprHasTypeSink(m,env.NameEnv,ty, env.DisplayEnv,AccessRightsOfEnv env);
        TcQuotationExpr cenv ty env tpenv (oper,raw,ast,m) 

    | SynExpr.YieldOrReturn ((isTrueYield,_),_,m)
    | SynExpr.YieldOrReturnFrom ((isTrueYield,_),_,m) when isTrueYield -> 
         error(Error(FSComp.SR.tcConstructRequiresListArrayOrSequence(),m))
    | SynExpr.YieldOrReturn ((_,isTrueReturn),_,m)
    | SynExpr.YieldOrReturnFrom ((_,isTrueReturn),_,m) when isTrueReturn -> 
         error(Error(FSComp.SR.tcConstructRequiresComputationExpressions(),m))
    | SynExpr.YieldOrReturn (_,_,m)
    | SynExpr.YieldOrReturnFrom (_,_,m) 
    | SynExpr.ImplicitZero (m) ->
         error(Error(FSComp.SR.tcConstructRequiresSequenceOrComputations(),m))
    | SynExpr.DoBang  (_,m) 
    | SynExpr.LetOrUseBang  (_,_,_,_,_,m) -> 
         error(Error(FSComp.SR.tcConstructRequiresComputationExpression(),m))

/// Check lambdas as a group, to catch duplicate names in patterns
and TcIteratedLambdas cenv isFirst (env: TcEnv) ty takenNames tpenv e = 
    match e with 
    | SynExpr.Lambda (isMember,isSubsequent,spats,bodyExpr,m) when isMember || isFirst || isSubsequent ->
        let domainTy,resultTy = UnifyFunctionType None cenv env.DisplayEnv m ty
        let vs, (tpenv,names,takenNames) = TcSimplePats cenv isMember CheckCxs domainTy env (tpenv,Map.empty,takenNames) spats
        let envinner,_,vspecMap = MakeAndPublishSimpleVals cenv env m names
        let byrefs = vspecMap |> Map.map (fun _ v -> isByrefTy cenv.g v.Type, v)
        let envinner = if isMember then envinner else ExitFamilyRegion envinner
        let bodyExpr,tpenv = TcIteratedLambdas cenv false envinner resultTy takenNames tpenv bodyExpr
        // See bug 5758: Non-monontonicity in inference: need to ensure that parameters are never inferred to have byref type, instead it is always declared
        byrefs  |> Map.iter (fun _ (orig,v) -> 
            if not orig && isByrefTy cenv.g v.Type then errorR(Error(FSComp.SR.tcParameterInferredByref v.DisplayName,v.Range)))
        mkMultiLambda m (List.map (fun nm -> NameMap.find nm vspecMap) vs) (bodyExpr,resultTy),tpenv 
    | e -> 
        TcExpr cenv ty env tpenv e

// Check expr.[idx] 
// This is a little over complicated for my liking. Basically we want to intepret e1.[idx] as e1.Item(idx). 
// However it's not so simple as all that. First "Item" can have a different name according to an attribute in 
// .NET metadata.  This means we manually typecheck 'e1' and look to see if it has a nominal type. We then 
// do the right thing in each case. 
and TcIndexerThen cenv env ty m mDot tpenv expr e1 idxs delayed = 
    let ad = AccessRightsOfEnv env
    let e1',e1ty,tpenv = TcExprOfUnknownType cenv env tpenv e1
    
    // Find the first type in the effective hierarchy that either has a DefaultMember attribute OR 
    // has a member called 'Item' 
    let propName = 
        match idxs with 
        | [_] -> 
            FoldPrimaryHierarchyOfType (fun typ acc -> 
                match acc with
                | None ->
                    let isNominal = isAppTy cenv.g typ
                    if isNominal then 
                        let tcref = tcrefOfAppTy cenv.g typ
                        TyconRefTryBindAttrib cenv.g cenv.g.attrib_DefaultMemberAttribute tcref 
                                 (function ([ILAttribElem.String (Some(msg)) ],_) -> Some msg | _ -> None)
                                 (function (Attrib(_,_,[ AttribStringArg(msg) ],_,_,_))  -> Some(msg) | _ -> None)
                     else
                        match AllPropInfosOfTypeInScope cenv.infoReader (env.NameEnv).eExtensionMembers (Some("Item"), ad) IgnoreOverrides m typ with
                        | [] -> None
                        | _ -> Some("Item")
                 | _ -> acc)
              cenv.g 
              cenv.amap 
              m 
              FirstIntfInst
              e1ty
              None
        | _ -> Some "GetSlice"

    let isNominal = isAppTy cenv.g e1ty
    
    let isArray = isArrayTy cenv.g e1ty 
    let isString = typeEquiv cenv.g cenv.g.string_ty e1ty 

    let idxRange = idxs |> List.map (fun e -> e.Range) |> List.reduce unionRanges 
    let MakeIndexParam vopt = 
        match idxs @ Option.toList vopt with 
        | []  -> failwith "unexpected empty index list"
        | [h] -> SynExpr.Paren(h,idxRange)
        | es -> SynExpr.Paren(SynExpr.Tuple(es,idxRange),idxRange)

    if isArray || isString then 

        let indexOpPath = ["Microsoft";"FSharp";"Core";"LanguagePrimitives";"IntrinsicFunctions"]
        let sliceOpPath = ["Microsoft";"FSharp";"Core";"Operators";"OperatorIntrinsics"]
        let path,fnm,idxs = 
            match isString,isArray,expr with 
            | false,true,SynExpr.DotIndexedGet(_,[SynExpr.Tuple ([_;_] as idxs,_)],_,_)         -> indexOpPath,"GetArray2D", idxs
            | false,true,SynExpr.DotIndexedGet(_,[SynExpr.Tuple ([_;_;_] as idxs,_)],_,_)       -> indexOpPath,"GetArray3D", idxs
            | false,true,SynExpr.DotIndexedGet(_,[SynExpr.Tuple ([_;_;_;_] as idxs,_)],_,_)     -> indexOpPath,"GetArray4D", idxs
            | false,true,SynExpr.DotIndexedGet(_,[_],_,_)                                    -> indexOpPath,"GetArray", idxs
            | false,true,SynExpr.DotIndexedSet(_,[SynExpr.Tuple ([_;_] as idxs,_)] ,e3,_,_)    -> indexOpPath,"SetArray2D", (idxs @ [e3])
            | false,true,SynExpr.DotIndexedSet(_,[SynExpr.Tuple ([_;_;_] as idxs,_)] ,e3,_,_)   -> indexOpPath,"SetArray3D", (idxs @ [e3])
            | false,true,SynExpr.DotIndexedSet(_,[SynExpr.Tuple ([_;_;_;_] as idxs,_)] ,e3,_,_) -> indexOpPath,"SetArray4D", (idxs @ [e3])
            | false,true,SynExpr.DotIndexedSet(_,[_],e3,_,_)                        -> indexOpPath,"SetArray", (idxs @ [e3])
            | true,false,SynExpr.DotIndexedGet(_,[_;_],_,_)                -> sliceOpPath,"GetStringSlice", idxs
            | true,false,SynExpr.DotIndexedGet(_,[_],_,_)                  -> indexOpPath,"GetString", idxs
            | false,true,SynExpr.DotIndexedGet(_,[_;_],_,_)                -> sliceOpPath,"GetArraySlice", idxs
            | false,true,SynExpr.DotIndexedGet(_,[_;_;_;_],_,_)            -> sliceOpPath,"GetArraySlice2D", idxs
            | false,true,SynExpr.DotIndexedGet(_,[_;_;_;_;_;_],_,_)        -> sliceOpPath,"GetArraySlice3D", idxs
            | false,true,SynExpr.DotIndexedGet(_,[_;_;_;_;_;_;_;_],_,_)    -> sliceOpPath,"GetArraySlice4D", idxs
            | false,true,SynExpr.DotIndexedSet(_,[_;_],e3,_,_)             -> sliceOpPath,"SetArraySlice", (idxs @ [e3])
            | false,true,SynExpr.DotIndexedSet(_,[_;_;_;_],e3,_,_)         -> sliceOpPath,"SetArraySlice2D", (idxs @ [e3])
            | false,true,SynExpr.DotIndexedSet(_,[_;_;_;_;_;_],e3,_,_)     -> sliceOpPath,"SetArraySlice3D", (idxs @ [e3])
            | false,true,SynExpr.DotIndexedSet(_,[_;_;_;_;_;_;_;_],e3,_,_) -> sliceOpPath,"SetArraySlice4D", (idxs @ [e3])
            | _ -> error(Error(FSComp.SR.tcInvalidIndexerExpression(),m))
        let operPath = (mkSynLidGet mDot path (CompileOpName fnm))
        let f,fty,tpenv = TcExprOfUnknownType cenv env tpenv operPath
        let domainTy,resultTy = UnifyFunctionType (Some m) cenv env.DisplayEnv m fty
        UnifyTypes cenv env m domainTy e1ty; 
        let f' = buildApp cenv (MakeApplicableExprNoFlex cenv f) fty e1' m
        let delayed = List.foldBack (fun idx acc -> DelayedApp(ExprAtomicFlag.Atomic,idx,m) :: acc) idxs delayed // atomic, otherwise no ar.[1] <- xyz
        PropagateThenTcDelayed cenv ty env tpenv m f' resultTy ExprAtomicFlag.Atomic delayed 

    elif (isNominal || isSome propName) then 

        let nm = 
            match propName with 
            | None -> "Item"
            | Some(nm) -> nm
        let delayed = 
            match expr with 
            | SynExpr.DotIndexedGet _ -> 
                DelayedDotLookup([ident(nm,m)],m) :: DelayedApp(ExprAtomicFlag.Atomic,MakeIndexParam None,m) :: delayed
            | SynExpr.DotIndexedSet(_,_,e3,_,_) -> 
                match idxs with 
                | [_] -> DelayedDotLookup([ident(nm,m)],m) :: DelayedApp(ExprAtomicFlag.Atomic,MakeIndexParam None,m) :: MakeDelayedSet(e3,m) :: delayed
                | _ -> DelayedDotLookup([ident("SetSlice",m)],m) :: DelayedApp(ExprAtomicFlag.Atomic,MakeIndexParam (Some e3),m) :: delayed
                
            | _ -> error(InternalError("unreachable",m))
        PropagateThenTcDelayed cenv ty env tpenv mDot (MakeApplicableExprNoFlex cenv e1') e1ty ExprAtomicFlag.Atomic delayed 

    else 
        // deprecated constrained lookup 
        error(Error(FSComp.SR.tcObjectOfIndeterminateTypeUsedRequireTypeConstraint(),m));


/// Check a 'new Type(args)' expression, also an 'inheritedTys declaration in an implicit or explicit class 
and TcNewExpr cenv env tpenv objTy objTyRangeOpt superInit arg m =
    let ad = AccessRightsOfEnv env
    // Handle the case 'new 'a()' 
    if (isTyparTy cenv.g objTy) then 
        if superInit then error(Error(FSComp.SR.tcCannotInheritFromVariableType(),m));
        AddCxTypeMustSupportDefaultCtor env.DisplayEnv cenv.css m NoTrace objTy;
        
        match arg with 
        | SynExpr.Const (SynConst.Unit,_) -> ()
        | _ -> errorR(Error(FSComp.SR.tcObjectConstructorsOnTypeParametersCannotTakeArguments(),m))
        
        mkCallCreateInstance cenv.g m objTy ,tpenv
    else 
        if not (isAppTy cenv.g objTy) then error(Error(FSComp.SR.tcNamedTypeRequired(if superInit then "inherit" else "new"),m));
        let item,rest = ForceRaise (ResolveObjectConstructor cenv.nameResolver env.DisplayEnv m ad objTy)
        
        let nenv = env.NameEnv
        // Re-record the name resolution since we now know it's a constructor call
        match objTyRangeOpt with 
        | Some objTyRange -> CallNameResolutionSink(objTyRange,nenv,item,ItemOccurence.Use,nenv.eDisplayEnv,AccessRightsOfEnv env);
        | None -> ()
        
        TcCtorCall false cenv env tpenv objTy objTy item superInit arg m (delay_rest rest m [])

/// Check an 'inheritedTys declaration in an implicit or explicit class 
and TcCtorCall isNaked cenv env tpenv typ objTy item superInit arg m delayed =
    let ad = AccessRightsOfEnv env
    let isSuperInit = (if superInit then CtorValUsedAsSuperInit else NormalValUse)

    if isInterfaceTy cenv.g objTy then 
      error(Error((if superInit then FSComp.SR.tcInheritCannotBeUsedOnInterfaceType() 
                   else            FSComp.SR.tcNewCannotBeUsedOnInterfaceType()),m));

    match item with 
    | Item.CtorGroup(methodName,minfos) ->
        let meths = List.map (fun minfo -> minfo,None) minfos
        if isNaked && TypeFeasiblySubsumesType 0 cenv.g cenv.amap m cenv.g.system_IDisposable_typ NoCoerce objTy then
          warning(Error(FSComp.SR.tcIDisposableTypeShouldUseNew(),m));
        TcMethodApplicationThen cenv env typ tpenv None [] m methodName ad PossiblyMutates false meths isSuperInit [arg] ExprAtomicFlag.NonAtomic delayed 

    | Item.DelegateCtor typ ->
        TcNewDelegateThen cenv objTy env tpenv m typ arg ExprAtomicFlag.NonAtomic delayed

    | _ -> error(Error(FSComp.SR.tcSyntaxCanOnlyBeUsedToCreateObjectTypes(if superInit then "inherit" else "new"),m))


//-------------------------------------------------------------------------
// TcRecordConstruction
//------------------------------------------------------------------------- 
  
// Check a record consutrction expression 
and TcRecordConstruction cenv ty env tpenv optOrigExpr objTy fldsList m =
    let tcref = tcrefOfAppTy cenv.g objTy
    let tycon = tcref.Deref
    let tinst = Tastops.argsOfAppTy cenv.g objTy
    UnifyTypes cenv env m ty objTy;

    // Types with implicit constructors can't use record or object syntax: all constructions must go through the implicit constructor 
    if tycon.MembersOfFSharpTyconByName |> NameMultiMap.existsInRange (fun v -> v.IsIncrClassConstructor) then 
        errorR(Error(FSComp.SR.tcConstructorRequiresCall(tycon.DisplayName),m));
                
    let fspecs = tycon.TrueInstanceFieldsAsList
    // Freshen types and work out their subtype flexibility
    let fldsList = 
        [ for (fname, fexpr) in fldsList do 
              let fspec = 
                  try  
                      fspecs |> List.find (fun fspec -> fspec.Name = fname) 
                  with :? KeyNotFoundException -> 
                      error (Error(FSComp.SR.tcUndefinedField(fname, NicePrint.prettyStringOfTy env.DisplayEnv objTy),m))
              let (declaredTypars,ftinst,fty) = FreshenPossibleForallTy cenv.g m TyparRigid (actualTyOfRecdFieldForTycon tycon tinst fspec)
              let flex = not (isTyparTy cenv.g fty)
              yield (fname,fexpr,declaredTypars,ftinst,fty,flex) ]

    // Type check and generalize the supplied bindings 
    let fldsList,tpenv = 
        (tpenv,fldsList) ||> List.mapFold (fun tpenv (fname,fexpr,declaredTypars,_,fty,flex) -> 
              let fieldExpr,tpenv = TcExprFlex cenv flex fty env tpenv fexpr
              // Polymorphic fields require generializeable expressions. 
              // NOTE: polymorphic fields are now removed from the language
              // As a result some of this code can be cleaned up and the generalization removed
              if nonNil(declaredTypars) then
              
                  // Canonicalize constraints prior to generalization 
                  let denv = env.DisplayEnv
                  GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,m) declaredTypars;

                  let freeInEnv = GeneralizationHelpers.ComputeUngeneralizableTypars env
                  let _generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv,m,true,freeInEnv,false,CanGeneralizeConstrainedTypars,OptionalInline,Some(fieldExpr),declaredTypars,[],fty,false)
                  (fname,mkTypeLambda m declaredTypars (fieldExpr,fty)), tpenv
              else  
                  (fname,fieldExpr),tpenv)
              
    // Add rebindings for unbound field when an "old value" is available 
    let oldFldsList = 
      match optOrigExpr with
      | None -> []
      | Some (_,_,oldve') -> 
             // When we have an "old" value, append bindings for the unbound fields. 
             // Effect order - mutable fields may get modified by other bindings... 
             let fieldNameUnbound nom = List.forall (fun (name,_) -> name <> nom) fldsList
             fspecs 
             |> List.filter (fun rfld -> rfld.Name |> fieldNameUnbound)
             |> List.filter (fun f -> not f.IsZeroInit)
             |> List.map (fun fspec ->
                 let (_,ftinst,_) = FreshenPossibleForallTy cenv.g m TyparRigid (actualTyOfRecdFieldForTycon tycon tinst fspec)
                 fspec.Name, mkRecdFieldGet cenv.g (oldve',mkNestedRecdFieldRef tcref fspec,tinst,ftinst,m))

    let fldsList = fldsList @ oldFldsList

    // From now on only interested in fspecs that truly need values. 
    let fspecs = fspecs |> List.filter (fun f -> not f.IsZeroInit)
    
    // Check all fields are bound
    fspecs |> List.iter (fun fspec ->
      if not (fldsList |> List.exists (fun (fname,_) -> fname = fspec.Name)) then
        error(Error(FSComp.SR.tcFieldRequiresAssignment(fspec.rfield_id.idText, fullDisplayTextOfTyconRef tcref),m)));

    // Other checks (overlap with above check now clear)
    let ns1 = NameSet.ofList (List.map fst fldsList)
    let ns2 = NameSet.ofList (List.map (fun x -> x.rfield_id.idText) fspecs)
    
    if isNone optOrigExpr && not (Zset.subset ns2 ns1) then
        error (MissingFields(Zset.elements (Zset.diff ns2 ns1),m))
    
    if  not (Zset.subset ns1 ns2) then 
        error (Error(FSComp.SR.tcExtraneousFieldsGivenValues(),m))
    
    // Build record 
    let rfrefs = List.map (fst >> mkRecdFieldRef tcref) fldsList

    // Check accessibility: this is also done in BuildFieldMap, but also need to check 
    // for fields in { new R with a=1 and b=2 } constructions and { r with a=1 }  copy-and-update expressions 
    rfrefs |> List.iter (fun rfref -> 
        CheckRecdFieldAccessible m (AccessRightsOfEnv env) rfref |> ignore;
        CheckFSharpAttributes cenv.g rfref.PropertyAttribs m |> CommitOperationResult);        

    let args   = List.map snd fldsList
    
    let expr = mkRecordExpr cenv.g (GetRecdInfo env, tcref, tinst, rfrefs, args, m)

    let expr = 
      match optOrigExpr with 
      | None ->
          // '{ recd fields }'. //
          expr
          
      | Some (old',oldv',_) -> 
          // '{ recd with fields }'. 
          // Assign the first object to a tmp and then construct 
          mkCompGenLet m oldv' old' expr

    expr, tpenv

//-------------------------------------------------------------------------
// TcObjectExpr
//------------------------------------------------------------------------- 

and GetNameAndArityOfObjExprBinding _cenv _env b =
    let (NormalizedBinding (_,_,_,_,_,_,_,valSynData,pat,rhsExpr,bindingRange,_)) = b
    let (SynValData(memberFlagsOpt,valSynInfo,_)) = valSynData 
    match pat,memberFlagsOpt with 

    // This is the normal case for F# 'with member x.M(...) = ...'
    | SynPat.InstanceMember(_thisId,memberId,_,None,_),Some(memberFlags) ->
         let logicalMethId = ident (ComputeLogicalName memberId memberFlags,memberId.idRange)
         logicalMethId.idText,valSynInfo

    | _ -> 
        // This is for the deprecated form 'with M(...) = ...'
        let rec lookPat pat =
            match pat with 
            | SynPat.Typed(pat,_,_) -> lookPat pat
            | SynPat.Named (SynPat.Wild _, id,_,None,_) -> 
                 // let e = PushOnePatternToRhs (mkSynThisPatVar (ident "_this" id.idRange)) e in  
                let (NormalizedBindingRhs(pushedPats,_,_)) = rhsExpr
                let infosForExplicitArgs = pushedPats |> List.map SynInfo.InferArgSynInfoFromSimplePats
                let infosForExplicitArgs = SynInfo.AdjustMemberArgs MemberKind.Member infosForExplicitArgs
                let infosForExplicitArgs = SynInfo.AdjustArgsForUnitElimination infosForExplicitArgs 
                let argInfos = [SynInfo.selfMetadata] @ infosForExplicitArgs
                let retInfo = SynInfo.unnamedRetVal //SynInfo.InferSynReturnData pushedRetInfoOpt
                let valSynData = SynValInfo(argInfos,retInfo)
                (id.idText,valSynData)

            | _ -> error(Error(FSComp.SR.tcObjectExpressionsCanOnlyOverrideAbstractOrVirtual(),bindingRange)); 

        lookPat pat


and FreshenObjExprAbstractSlot cenv (_env: TcEnv) implty virtNameAndArityPairs (bind,bindAttribs,bindName,absSlots:(_ * MethInfo) list) = 
    let (NormalizedBinding (_,_,_,_,_,_,synTyparDecls,_,_,_,bindingRange,_)) = bind 
    match absSlots with 
    | [] when not (CompileAsEvent cenv.g bindAttribs) -> 
        let absSlotsByName = List.filter (fst >> fst >> (=) bindName) virtNameAndArityPairs
        
        match absSlotsByName with 
        | []              -> errorR(Error(FSComp.SR.tcNoAbstractOrVirtualMemberFound(bindName),bindingRange));
        | [(_,absSlot:MethInfo)]     -> errorR(Error(FSComp.SR.tcArgumentArityMismatch(bindName, (List.sum absSlot.NumArgs)),bindingRange));
        | (_,absSlot:MethInfo) :: _  -> errorR(Error(FSComp.SR.tcArgumentArityMismatchOneOverload(bindName, (List.sum absSlot.NumArgs)),bindingRange));
        
        None
        
    | [(_,absSlot)] -> 
        
        let typarsFromAbsSlotAreRigid,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot
           = FreshenAbstractSlot cenv.g cenv.amap bindingRange synTyparDecls absSlot

        // Work out the required type of the member 
        let bindingTy = implty --> (mkMethodTy cenv.g argTysFromAbsSlot retTyFromAbsSlot) 
        
        Some(typarsFromAbsSlotAreRigid,typarsFromAbsSlot,bindingTy)
        
    | _ -> //(_,absSlot1) :: (_,absSlot2) :: _ -> 
        //warning(NonUniqueInferredAbstractSlot(cenv.g,env.DisplayEnv, bindName, absSlot1, absSlot2,bindingRange));
        //fail()
        None


and TcObjectExprBinding cenv (env: TcEnv) implty tpenv (absSlotInfo,bind) =
    // 4a1. normalize the binding (note: needlessly repeating what we've done above) 
    let (NormalizedBinding(vis,bkind,pseudo,isMutable,attrs,doc,synTyparDecls,valSynData,p,bindingRhs,bindingRange,spBind)) = bind
    let (SynValData(memberFlagsOpt,_,_)) = valSynData 
    // 4a2. adjust the binding, especially in the "member" case, a subset of the logic of AnalyzeAndMakeRecursiveValue 
    let bindingRhs,logicalMethId,memberFlags = 
        let rec pat p = 
            match p,memberFlagsOpt with  
            | SynPat.Named (SynPat.Wild _, id,_,_,_),None -> 
                let bindingRhs = PushOnePatternToRhs true (mkSynThisPatVar (ident (CompilerGeneratedName "this",id.idRange))) bindingRhs 
                let logicalMethId = id
                let memberFlags = OverrideMemberFlags MemberKind.Member
                bindingRhs,logicalMethId,memberFlags

            | SynPat.InstanceMember(thisId,memberId,_,_,_),Some(memberFlags) -> 
                CheckMemberFlags cenv.g None  NewSlotsOK OverridesOK memberFlags bindingRange;
                let bindingRhs = PushOnePatternToRhs true (mkSynThisPatVar thisId) bindingRhs
                let logicalMethId = ident (ComputeLogicalName memberId memberFlags,memberId.idRange)
                bindingRhs,logicalMethId,memberFlags
            | _ -> 
                error(InternalError("unexpect member binding",bindingRange))
        pat p
    let bind = NormalizedBinding (vis,bkind,pseudo,isMutable,attrs,doc,synTyparDecls,valSynData,mkSynPatVar vis logicalMethId,bindingRhs,bindingRange,spBind) 
    
    // 4b. typecheck the binding 
    let bindingTy = 
        match absSlotInfo with
        | Some(_,_,memberTyFromAbsSlot) -> 
            memberTyFromAbsSlot
        | _ -> 
            implty --> NewInferenceType ()

    let (CheckedBindingInfo(inlineFlag,immut,bindingAttribs,_,_,ExplicitTyparInfo(_,declaredTypars,_),nameToPrelimValSchemeMap,rhsExpr,_,_,m,_,_,_),tpenv) = 
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
        GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,m) declaredTypars;

        let freeInEnv = GeneralizationHelpers.ComputeUngeneralizableTypars env

        let generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv,m,immut,freeInEnv,false,CanGeneralizeConstrainedTypars,inlineFlag,Some(rhsExpr),declaredTypars,[],bindingTy,false)
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
                     for extraBinding in EventDeclarationNormalization.GenerateExtraBindings(cenv.g, bindingAttribs, binding) do
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
         error(Error(FSComp.SR.tcPredefinedTypeCannotBeUsedAsSuperType(),m));
       
   
and TcObjectExpr cenv ty env tpenv (objTy,argopt,binds,extraImpls,m) = 
    let objTy',tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv objTy
    if not (isAppTy cenv.g objTy') then error(Error(FSComp.SR.tcNewMustBeUsedWithNamedType(),m));
    if not (isRecdTy cenv.g objTy') && not (isInterfaceTy cenv.g objTy') && isSealedTy cenv.g objTy' then errorR(Error(FSComp.SR.tcCannotCreateExtensionOfSealedType(),m));
    
    CheckSuperType cenv objTy' objTy.Range; 
       
    // Object expression members can access protected members of the implemented type 
    let env = EnterFamilyRegion (tcrefOfAppTy cenv.g objTy') env
    let ad = AccessRightsOfEnv env
    
    if // record construction ?
       (isRecdTy cenv.g objTy') || 
       // object construction?
       (isFSharpObjModelTy cenv.g objTy' && not (isInterfaceTy cenv.g objTy') && isNone argopt) then  

        if isSome argopt then error(Error(FSComp.SR.tcNoArgumentsForRecordValue(),m));
        if nonNil extraImpls then error(Error(FSComp.SR.tcNoInterfaceImplementationForConstructionExpression(),m));
        if isFSharpObjModelTy cenv.g objTy' && GetCtorShapeCounter env <> 1 then 
            error(Error(FSComp.SR.tcObjectConstructionCanOnlyBeUsedInClassTypes(),m));
        let fldsList = 
            binds |> List.map (fun b -> 
                match BindingNormalization.NormalizeBinding ObjExprBinding cenv env b with 
                | NormalizedBinding (_,_,_,_,[],_,_,_,SynPat.Named(SynPat.Wild _, id,_,_,_),NormalizedBindingRhs(_,_,rhsExpr),_,_) -> id.idText,rhsExpr
                | _ -> error(Error(FSComp.SR.tcOnlySimpleBindingsCanBeUsedInConstructionExpressions(),m)))
        
        TcRecordConstruction cenv ty env tpenv None objTy' fldsList m
    else
        let item,rest = ForceRaise (ResolveObjectConstructor cenv.nameResolver env.DisplayEnv m ad objTy')

        if nonNil rest then error(InternalError("Unexpected rest from ResolveObjectConstructor",m));

        if isFSharpObjModelTy cenv.g objTy' && GetCtorShapeCounter env = 1 then 
            error(Error(FSComp.SR.tcObjectsMustBeInitializedWithObjectExpression(),m));

      // Work out the type of any interfaces to implement 
        let extraImpls,tpenv = 
          (tpenv , extraImpls) ||> List.mapFold (fun tpenv (InterfaceImpl(ity,overrides,m)) -> 
              let ity',tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv ity
              if not (isInterfaceTy cenv.g ity') then
                error(Error(FSComp.SR.tcExpectedInterfaceType(),m));
              (m,ity',overrides),tpenv)

        let realObjTy = (if isObjTy cenv.g objTy' && nonNil extraImpls then (p23 (List.head extraImpls)) else objTy')
        UnifyTypes cenv env m ty realObjTy;

        let ctorCall,baseIdOpt,tpenv =
            match item,argopt with 
            | Item.CtorGroup(methodName,minfos),Some (arg,baseIdOpt) -> 
                let meths = minfos |> List.map (fun minfo -> minfo,None) 
                let ad = AccessRightsOfEnv env
                let expr,tpenv = TcMethodApplicationThen cenv env objTy' tpenv None [] m methodName ad PossiblyMutates false meths CtorValUsedAsSuperInit [arg] ExprAtomicFlag.Atomic [] 
                // The 'base' value is always bound
                let baseIdOpt = (match baseIdOpt with None -> Some(ident("base",m)) | Some id -> Some(id))
                expr,baseIdOpt,tpenv
            | Item.FakeInterfaceCtor ityp,None -> 
                UnifyTypes cenv env m objTy' ityp;
                let expr = BuildObjCtorCall cenv.g m
                expr,None,tpenv
            | Item.FakeInterfaceCtor _,Some _ -> 
                error(Error(FSComp.SR.tcConstructorForInterfacesDoNotTakeArguments(),m));
            | Item.CtorGroup _,None -> 
                error(Error(FSComp.SR.tcConstructorRequiresArguments(),m));
            | _ -> error(Error(FSComp.SR.tcNewRequiresObjectConstructor(),m))

        let baseValOpt = MakeAndPublishBaseVal cenv env baseIdOpt objTy'
        let env = Option.foldBack (AddLocalVal m) baseValOpt env
        
        
        let impls = (m,objTy',binds) :: extraImpls
        
        
        // 1. collect all the relevant abstract slots for each type we have to implement 
        
        let overridesAndVirts,tpenv = ComputeObjectExprOverrides cenv env tpenv impls

    
        overridesAndVirts |> List.iter (fun (m,implty,dispatchSlots,dispatchSlotsKeyed,availPriorOverrides,overrides) -> 
            let overrideSpecs = overrides |> List.map fst

            DispatchSlotChecking.CheckOverridesAreAllUsedOnce env.DisplayEnv cenv.g cenv.amap (implty, dispatchSlotsKeyed, availPriorOverrides, overrideSpecs);

            DispatchSlotChecking.CheckDispatchSlotsAreImplemented (env.DisplayEnv,cenv.g,cenv.amap,m,false,implty,dispatchSlots,availPriorOverrides,overrideSpecs) |> ignore);
        
        // 6c. create the specs of overrides 
        let allTypeImpls = 
          overridesAndVirts |> List.map (fun (m,implty,_,dispatchSlotsKeyed,_,overrides) -> 
              let overrides' = 
                  [ for overrideMeth in overrides do 
                        let (Override(_,_, id,(mtps,_),_,_,isFakeEventProperty) as ovinfo),(_, thisVal, methodVars, bindingAttribs, bindingBody) = overrideMeth
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
                                | None -> error(Error(FSComp.SR.tcAtLeastOneOverrideIsInvalid(),objTy.Range))

                            //let _,_,methodVars,bindingBodyExpr,_ = IteratedAdjustArityOfLambdaBody cenv.g (List.map List.length vs) vs bindingBody 
                            yield TObjExprMethod(SlotSigOfMethodInfo cenv.amap m overridden, bindingAttribs, mtps, [thisVal]::methodVars, bindingBody, id.idRange) ]
              (implty,overrides'))
            
        let (objTy',overrides') = allTypeImpls.Head
        let extraImpls = allTypeImpls.Tail
        
        // 7. Build the implementation 
        let expr = mkObjExpr(objTy', baseValOpt, ctorCall, overrides',extraImpls,m)
        let expr = mkCoerceIfNeeded cenv.g realObjTy objTy' expr
        expr,tpenv



//-------------------------------------------------------------------------
// TcConstStringExpr
//------------------------------------------------------------------------- 

/// Check a constant string expression. It might be a 'printf' format string 
and TcConstStringExpr cenv ty env m tpenv s  =

    if (AddCxTypeEqualsTypeUndoIfFailed env.DisplayEnv cenv.css m ty cenv.g.string_ty) then 
      mkString cenv.g m s,tpenv
    else 
      let aty = NewInferenceType ()
      let bty = NewInferenceType ()
      let cty = NewInferenceType ()
      let dty = NewInferenceType ()
      let ety = NewInferenceType ()
      let ty' = mkPrintfFormatTy cenv.g aty bty cty dty ety
      if (not (isObjTy cenv.g ty) && AddCxTypeMustSubsumeTypeUndoIfFailed env.DisplayEnv cenv.css m ty ty') then 
        // Parse the format string to work out the phantom types 
        let aty',ety' = (try Formats.ParseFormatString m cenv.g s bty cty dty with Failure s -> error (Error(FSComp.SR.tcUnableToParseFormatString(s),m)))
        UnifyTypes cenv env m aty aty';
        UnifyTypes cenv env m ety ety';
        mkCallNewFormat cenv.g m aty bty cty dty ety (mkString cenv.g m s),tpenv
      else 
        UnifyTypes cenv env m ty cenv.g.string_ty;
        mkString cenv.g m s,tpenv

//-------------------------------------------------------------------------
// TcConstExpr
//------------------------------------------------------------------------- 

/// Check a constant expression. 
and TcConstExpr cenv ty env m tpenv c  =
    match c with 

    // NOTE: these aren't "really" constants 
    | SynConst.Bytes (bytes,m) -> 
       UnifyTypes cenv env m ty (mkByteArrayTy cenv.g); 
       Expr.Op(TOp.Bytes bytes,[],[],m),tpenv

    | SynConst.UInt16s arr -> 
       UnifyTypes cenv env m ty (mkArrayType cenv.g cenv.g.uint16_ty); Expr.Op(TOp.UInt16s arr,[],[],m),tpenv

    | SynConst.UserNum (s,suffix) -> 
        let expr = 
            let modName = ("NumericLiteral"^suffix)
            let ad = AccessRightsOfEnv env
            match ResolveLongIndentAsModuleOrNamespace OpenQualified env.eNameResEnv ad [ident (modName,m)] with 
            | Result []
            | Exception _ -> error(Error(FSComp.SR.tcNumericLiteralRequiresModule(modName),m))
            | Result ((_,mref,_) :: _) -> 
                let expr = 
                    try 
                        let i32 = int32 s  
                        if i32 = 0 then SynExpr.App(ExprAtomicFlag.Atomic, mkSynLidGet m [modName] "FromZero",SynExpr.Const(SynConst.Unit,m),m)
                        elif i32 = 1 then SynExpr.App(ExprAtomicFlag.Atomic, mkSynLidGet m [modName] "FromOne",SynExpr.Const(SynConst.Unit,m),m)
                        else SynExpr.App(ExprAtomicFlag.Atomic, mkSynLidGet m [modName] "FromInt32",SynExpr.Const(SynConst.Int32 i32,m),m)
                    with _ -> 
                      try 
                         let i64 = int64 s  
                         SynExpr.App(ExprAtomicFlag.Atomic, mkSynLidGet m [modName] "FromInt64",SynExpr.Const(SynConst.Int64 i64,m),m)
                      with _ ->             
                        SynExpr.App(ExprAtomicFlag.Atomic, mkSynLidGet m [modName] "FromString",SynExpr.Const(SynConst.String (s,m),m),m) 
                let ccu = ccuOfTyconRef mref
                if isSome ccu && ccuEq ccu.Value cenv.g.fslibCcu && suffix = "I" then 
                    SynExpr.Typed(expr,SynType.LongIdent(pathToSynLid m ["System";"Numerics";"BigInteger"],m),m)
                else
                    expr

        TcExpr cenv ty env tpenv expr

    | _ -> 
        let c' = TcConst cenv ty m env c
        Expr.Const (c',m,ty),tpenv


//-------------------------------------------------------------------------
// TcAssertExpr
//------------------------------------------------------------------------- 

// Check an 'assert(x)' expression. 
and TcAssertExpr cenv ty env m tpenv x  =
    let callDiagnosticsExpr = SynExpr.App(ExprAtomicFlag.Atomic, mkSynLidGet m ["System";"Diagnostics";"Debug"] "Assert", 
                                       // wrap an extra parentheses so 'assert(x=1) isn't considered a named argument to a method call 
                                       SynExpr.Paren(x,m), m)

    TcExpr cenv ty env tpenv callDiagnosticsExpr



//-------------------------------------------------------------------------
// TcRecdExpr
//------------------------------------------------------------------------- 

and TcRecdExpr cenv ty env tpenv (inherits,optOrigExpr,flds,m) =

    let requiresCtor = (GetCtorShapeCounter env = 1) // Get special expression forms for constructors 
    let haveCtor = (isSome inherits)

    let optOrigExpr,tpenv = 
      match optOrigExpr with 
      | None -> None, tpenv 
      | Some e -> 
          if isSome inherits then error(Error(FSComp.SR.tcInvalidRecordConstruction(),m));
          let e',tpenv = TcExpr cenv ty env tpenv e
          let v',ve' = Tastops.mkCompGenLocal m "inputRecord" ty
          Some (e',v',ve'), tpenv

    let fldsList = 
        match flds with 
        | [] -> []
        | _ -> 
            let tcref,_,fldsList = BuildFieldMap cenv env (isSome(optOrigExpr)) ty flds m
            let _,_,_,gtyp = info_of_tcref m tcref
            UnifyTypes cenv env m ty gtyp;      
            fldsList

    if isSome optOrigExpr && not (isRecdTy cenv.g ty) then 
        errorR(Error(FSComp.SR.tcExpressionFormRequiresRecordTypes(),m));

    if requiresCtor || haveCtor then 
        if not (isFSharpObjModelTy cenv.g ty) then 
            // Deliberate no-recovery failure here to prevent cascading internal errors
            error(Error(FSComp.SR.tcInheritedTypeIsNotObjectModelType(),m));
        if not requiresCtor then 
            errorR(Error(FSComp.SR.tcObjectConstructionExpressionCanOnlyImplementConstructorsInObjectModelTypes(),m));
    else
        if isNil flds then error(Error(FSComp.SR.tcEmptyRecordInvalid(),m));
        if isFSharpObjModelTy cenv.g ty then errorR(Error(FSComp.SR.tcTypeIsNotARecordTypeNeedConstructor(),m))
        elif not (isRecdTy cenv.g ty) then errorR(Error(FSComp.SR.tcTypeIsNotARecordType(),m));

    let superTy,tpenv = 
        match inherits, SuperTypeOfType cenv.g cenv.amap m ty with 
        | Some (superTyp,arg,m), Some(realSuperTyp) ->
            // Constructor expression, with an explicit 'inheritedTys clause. Check the inherits clause. 
            let e,tpenv = TcExpr cenv realSuperTyp  env tpenv (SynExpr.New(true,superTyp,arg,m))
            Some e, tpenv
        | None, Some(realSuperTyp) when requiresCtor -> 
            // Constructor expression, No 'inheritedTys clause, hence look for a default constructor 
            let e,tpenv = TcNewExpr cenv env tpenv realSuperTyp None true (SynExpr.Const (SynConst.Unit,m)) m
            Some e, tpenv
        | None,_ -> 
            None,tpenv
        | _, None -> 
            errorR(InternalError("Unexpected failure in getting super type",m));
            None,tpenv

    let expr,tpenv = 
        TcRecordConstruction cenv ty env tpenv optOrigExpr  ty fldsList m

    let expr = 
        match superTy with 
        | _ when isStructTy cenv.g ty -> expr
        | Some(e) -> mkCompGenSeq m e expr
        | None -> expr
    expr,tpenv


//-------------------------------------------------------------------------
// TcForEachExpr 
//------------------------------------------------------------------------- 
 
and TcForEachExpr cenv ty env tpenv (pat,enumSynExpr,body,m,spForLoop)  =
    UnifyTypes cenv env m ty cenv.g.unit_ty;

    let enumExpr,enumExprTy,tpenv = 
        //let env = ExitFamilyRegion env
        TcExprOfUnknownType cenv env tpenv enumSynExpr

    let enumElemTy, bodyExprFixup, overallExprFixup, iterationTechnique = 
        match enumExpr with 

        // optimize 'for i in n .. m do' 
        | Expr.App(Expr.Val(vf,_,_),_,[tyarg],[startExpr;finishExpr],_) 
             when valRefEq cenv.g vf cenv.g.range_op_vref && typeEquiv cenv.g tyarg cenv.g.int_ty -> 
               (cenv.g.int32_ty, (fun _ x -> x), id, Choice1Of3 (startExpr,finishExpr))

        // optimize 'for i in arr do' 
        | _ when isArray1DTy cenv.g enumExprTy  -> 
            let arrVar,arrExpr = Tastops.mkCompGenLocal m "arr" enumExprTy
            let idxVar,idxExpr = Tastops.mkCompGenLocal m "idx" cenv.g.int32_ty
            let elemTy = destArrayTy cenv.g enumExprTy
            
            // Evaluate the array index lookup
            let bodyExprFixup = (fun elemVar bodyExpr -> mkCompGenLet m elemVar (mkLdelem cenv.g m elemTy arrExpr idxExpr) bodyExpr)

            // Evaluate the array expression once and put it in arrVar
            let overallExprFixup = (fun overallExpr -> mkCompGenLet m arrVar enumExpr overallExpr)

            // Ask for a loop over integers for the given range
            (elemTy, bodyExprFixup, overallExprFixup, Choice2Of3 (idxVar,mkZero cenv.g m,mkDecr cenv.g m (mkLdlen cenv.g m arrExpr)))

        | _ -> 

            let enumerableVar,enumerableExprInVar = mkCompGenLocal enumExpr.Range "inputSequence" enumExprTy
            let enumeratorVar, enumeratorExpr,_,enumElemTy,getEnumExpr,getEnumTy,guardExpr,_,currentExpr = 
                    AnalyzeArbitraryExprAsEnumerable cenv env true enumExpr.Range enumExprTy enumerableExprInVar
            (enumElemTy, (fun _ x -> x), id, Choice3Of3(enumerableVar,enumeratorVar, enumeratorExpr,getEnumExpr,getEnumTy,guardExpr,currentExpr))
            
    let pat,_,vspecs,envinner,tpenv = TcMatchPattern cenv enumElemTy env tpenv (pat,None)
    let elemVar,pat =      
        // nice: don't introduce awful temporary for r.h.s. in the 99% case where we know what we're binding it to 
        match pat with
        | TPat_as (pat1,PBind(v,TypeScheme([],_)),_) -> 
              v,pat1
        | _ -> 
              let tmp,_ = Tastops.mkCompGenLocal m "forLoopVar" enumElemTy
              tmp,pat

    let bodyExpr,tpenv = TcStmt cenv envinner tpenv body

    let bodyExpr = 
        let valsDefinedByMatching = FlatListSet.remove valEq elemVar vspecs
        CompilePatternForMatch cenv env m m false IgnoreWithWarning (elemVar,[]) 
            [TClause(pat,None,TTarget(valsDefinedByMatching,bodyExpr,SequencePointAtTarget),m)] ty

    // Apply the fixup to bind the elemVar if needed
    let bodyExpr = bodyExprFixup elemVar bodyExpr
    
    let overallExpr =  

        match iterationTechnique with 

        // Build iteration as a for loop
        | Choice1Of3(startExpr,finishExpr) -> 
            mkFastForLoop  cenv.g (spForLoop,m,elemVar,startExpr,true,finishExpr,bodyExpr)

        // Build iteration as a for loop with a specific index variable that is not the same as the elemVar
        | Choice2Of3(idxVar,startExpr,finishExpr) -> 
            mkFastForLoop  cenv.g (spForLoop,m,idxVar,startExpr,true,finishExpr,bodyExpr)

        // Build iteration as a while loop with a try/finally disposal
        | Choice3Of3(enumerableVar,enumeratorVar, _,getEnumExpr,_,guardExpr,currentExpr) -> 

            // This compiled for must be matched EXACTLY by DetectFastIntegerForLoops in opt.fs and creflect.fs
            mkCompGenLet enumExpr.Range enumerableVar enumExpr
              (let cleanupE = BuildDisposableCleanup cenv env m enumeratorVar
               let spBind = (match spForLoop with SequencePointAtForLoop(spStart) -> SequencePointAtBinding(spStart) | NoSequencePointAtForLoop -> NoSequencePointAtStickyBinding)
               (mkLet spBind getEnumExpr.Range  enumeratorVar getEnumExpr
                   (mkTryFinally cenv.g 
                       (mkWhile cenv.g 
                           (NoSequencePointAtWhileLoop, WhileLoopForCompiledForEachExprMarker, guardExpr,
                               mkCompGenLet bodyExpr.Range elemVar currentExpr bodyExpr,m),
                        cleanupE,m,cenv.g.unit_ty,NoSequencePointAtTry,NoSequencePointAtFinally))))

    let overallExpr = overallExprFixup  overallExpr
    overallExpr, tpenv

//-------------------------------------------------------------------------
// TcQuotationExpr
//------------------------------------------------------------------------- 

and TcQuotationExpr cenv ty env tpenv (_oper,raw,ast,m) =
    let astTy = NewInferenceType ()

    // Assert the overall type for the domain of the quotation template
    UnifyTypes cenv env m ty (if raw then mkRawQuotedExprTy cenv.g else mkQuotedExprTy cenv.g astTy); 

    // Check the expression 
    let expr,tpenv = TcExpr cenv astTy env tpenv ast  
    
    // Wrap the expression
    let expr = Expr.Quote(expr,ref None, m,ty)

    // Coerce it if needed
    let expr = if raw then mkCoerceExpr(expr,(mkRawQuotedExprTy cenv.g),m,(tyOfExpr cenv.g expr)) else expr

    // We serialize the quoted expression to bytes in Ilxgen after type inference etc. is complete. 
    expr,tpenv


//-------------------------------------------------------------------------
// TcComputationExpression
//------------------------------------------------------------------------- 

and TcComputationExpression cenv (env: TcEnv) ty m interpValOpt tpenv comp = 
    //dprintfn "TcComputationExpression, comp = \n%A\n-------------------\n" comp
    let ad = AccessRightsOfEnv env

    let mkSynDelay2 (e: SynExpr) =  mkSynDelay e.Range e
    match interpValOpt with 
    
    // This case is used for all computation expressions except sequence expressions
    | Some (interpExpr:Expr,interpExprTy) -> 

        let interpVarName = CompilerGeneratedName "builder"
        let interpVarRange = interpExpr.Range
        let interpVar = mkSynIdGet interpVarRange interpVarName

        let mksynCall nm m args = 
            let args = 
                match args with 
                | [] -> SynExpr.Const(SynConst.Unit,m)
                | [arg] -> SynExpr.Paren(SynExpr.Paren(arg,m),m)
                | args -> SynExpr.Paren(SynExpr.Tuple(args,m),m)
                
            SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.DotGet(interpVar,[mkSynId m nm], m), args,m)

        let rec tryTrans comp =
            match comp with 
            | SynExpr.ForEach (spForLoop,SeqExprOnly(_seqExprOnly),pat,pseudoEnumExpr,innerComp,_) -> 
                let forRange = match spForLoop with SequencePointAtForLoop(m) -> m | _ -> pat.Range
                let patRange = pat.Range
                let spBind = match spForLoop with SequencePointAtForLoop(m) -> SequencePointAtBinding(m) | NoSequencePointAtForLoop -> NoSequencePointAtStickyBinding
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env forRange ad "For" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("For"),forRange))
                Some(mksynCall "For" forRange [pseudoEnumExpr; mkSynMatchLambda(false,false,forRange,[Clause(pat,None, trans innerComp,patRange,SequencePointAtTarget)],spBind) ])

            | SynExpr.For (spBind,id,start,dir,finish,innerComp,m) ->
                Some(trans (elim_fast_integer_for_loop (spBind,id,start,dir,finish,innerComp,m)))

            | SynExpr.While (spWhile,guardExpr,innerComp,_) -> 
                let guardRange = guardExpr.Range
                let whileRange = match spWhile with SequencePointAtWhileLoop(m) -> m | _ -> guardRange
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env whileRange ad "While" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("While"),whileRange))
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env whileRange ad "Delay" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),whileRange))
                Some(mksynCall "While" whileRange [mkSynDelay2 guardExpr; mksynCall "Delay" whileRange [mkSynDelay innerComp.Range (trans innerComp)]])

            | SynExpr.TryFinally (innerComp,unwindExpr,mTryToLast,spTry,_spFinally) ->
                let tryRange = match spTry with SequencePointAtTry(m) -> m | _ -> mTryToLast
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env tryRange ad "TryFinally" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("TryFinally"),tryRange))
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env tryRange ad "Delay" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),tryRange))
                Some(mksynCall "TryFinally" tryRange [mksynCall "Delay" tryRange [mkSynDelay innerComp.Range (trans innerComp)]; mkSynDelay2 unwindExpr])

            | SynExpr.Paren (_,m) -> 
                error(Error(FSComp.SR.tcConstructIsAmbiguousInComputationExpression(),m))

            | SynExpr.ImplicitZero m -> 
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Zero" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Zero"),m))
                Some(mksynCall "Zero" m [])

            // "do! expr; cexpr" is treated as { let! () = expr in cexpr }
            // "expr; cexpr" is treated as sequential execution
            // "cexpr; cexpr" is treated as builder.Combine(cexpr1,cexpr1)
            | SynExpr.Seq(sp,true,innerComp1, innerComp2,m) -> 
                match tryTrans innerComp1 with 
                | Some c -> 
                    // This is not pretty - we have to decide which range markers we use for the calls to Combine and Delay
                    // NOTE: we should probably suppress these sequence points altogether
                    let m1 = 
                        match innerComp1 with 
                        | SynExpr.IfThenElse (_,_,_,_,mIfToThen,_m) -> mIfToThen
                        | SynExpr.Match (SequencePointAtBinding mMatch,_,_,_,_) -> mMatch
                        | SynExpr.TryWith (_,_,_,_,_,SequencePointAtTry mTry,_) -> mTry
                        | SynExpr.TryFinally (_,_,_,SequencePointAtTry mTry,_)  -> mTry
                        | SynExpr.For (SequencePointAtForLoop mBind,_,_,_,_,_,_) -> mBind
                        | SynExpr.ForEach (SequencePointAtForLoop mBind,_,_,_,_,_) -> mBind
                        | SynExpr.While (SequencePointAtWhileLoop mWhile,_,_,_) -> mWhile
                        | _ -> innerComp1.Range
                    if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Combine" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Combine"),m))
                    if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "Delay" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),m))
                    Some(mksynCall "Combine" m1 [c; mksynCall "Delay" m1 [mkSynDelay innerComp2.Range (trans innerComp2)]])
                | None -> 
                    match innerComp1 with 
                    | SynExpr.DoBang(rhsExpr,m) -> 
                        let sp = 
                           match sp with 
                           | SuppressSequencePointOnStmtOfSequential -> SequencePointAtBinding m
                           | SuppressSequencePointOnExprOfSequential -> NoSequencePointAtDoBinding 
                           | SequencePointsAtSeq -> SequencePointAtBinding m
                        Some(trans (SynExpr.LetOrUseBang(sp, false, SynPat.Const(SynConst.Unit, rhsExpr.Range), rhsExpr, innerComp2, m)))
                    | _ -> 
                        SynExpr.Seq(sp,true, innerComp1, trans innerComp2,m) |> Some

            | SynExpr.IfThenElse (guardExpr,thenComp,elseCompOpt,spIfToThen,mIfToThen,mIfToEndOfElseBranch) ->
                //if seqExprOnly then warning (Error("'when' clauses should only be used inside compact sequence expressions. Consider using 'if guardExpr then ...' in computation expressions",m));
                let elseComp = (match elseCompOpt with Some c -> c | None -> SynExpr.ImplicitZero mIfToThen)
                Some(SynExpr.IfThenElse(guardExpr, trans thenComp, Some(trans elseComp), spIfToThen,mIfToThen,mIfToEndOfElseBranch))

            // 'let binds in expr'
            | SynExpr.LetOrUse (isRec,false,binds,innerComp,m) ->
                Some(SynExpr.LetOrUse (isRec,false,binds,trans innerComp,m))

            // 'use x = expr in expr'
            | SynExpr.LetOrUse (_,true,[Binding (_,NormalBinding,_,_,_,_,_,pat,_,rhsExpr,_,spBind)],innerComp,_) ->
                let bindRange = match spBind with SequencePointAtBinding(m) -> m | _ -> rhsExpr.Range
                let innerCompRange = innerComp.Range
                let consumeExpr = mkSynMatchLambda(false,false,innerCompRange,[Clause(pat,None, trans innerComp,innerCompRange,SequencePointAtTarget)],spBind)
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Using" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Using"),bindRange))
                Some(mksynCall "Using" bindRange [rhsExpr; consumeExpr ])

            // 'let! x = expr in expr'
            | SynExpr.LetOrUseBang(spBind,false,pat,rhsExpr,innerComp,_) -> 

                let bindRange = match spBind with SequencePointAtBinding(m) -> m | _ -> rhsExpr.Range
                let innerRange = innerComp.Range
                let consumeExpr = mkSynMatchLambda(false,false,innerRange,[Clause(pat,None, trans innerComp,innerRange,SequencePointAtTarget)],spBind)
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Bind" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Bind"),bindRange))
                Some(mksynCall "Bind"  bindRange [rhsExpr; consumeExpr ])

            // 'use! x = e1 in e2' --> build.Bind(e1,(fun x -> build.Using(x,(fun () -> e2))))
            | SynExpr.LetOrUseBang(spBind,true,(SynPat.Named (SynPat.Wild _, id, false, _, _) as pat) ,rhsExpr,innerComp,_) -> 

                let bindRange = match spBind with SequencePointAtBinding(m) -> m | _ -> rhsExpr.Range
                let consumeExpr = mkSynMatchLambda(false,false,bindRange,[Clause(pat,None, trans innerComp, innerComp.Range, SequencePointAtTarget)],spBind)
                let consumeExpr = mksynCall "Using" bindRange [SynExpr.Ident(id); consumeExpr ]
                let consumeExpr = mkSynMatchLambda(false,false,bindRange,[Clause(pat,None, consumeExpr,id.idRange,SequencePointAtTarget)],spBind)
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Using" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Using"),bindRange))
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env bindRange ad "Bind" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Bind"),bindRange))
                Some(mksynCall "Bind" bindRange [rhsExpr; consumeExpr])

            | SynExpr.Match (spMatch,expr,clauses,false,m) ->
                let clauses = clauses |> List.map (fun (Clause(pat,cond,innerComp,patm,sp)) -> Clause(pat,cond,trans innerComp,patm,sp))
                Some(SynExpr.Match(spMatch,expr, clauses, false,m))

            | SynExpr.TryWith (innerComp,_mTryToWith,clauses,_mWithToLast,mTryToLast,spTry,_spWith) ->
                let tryRange = match spTry with SequencePointAtTry(m) -> m | _ -> mTryToLast
                let clauses = clauses |> List.map (fun (Clause(pat,cond,innerComp,patm,sp)) -> Clause(pat,cond,trans innerComp,patm,sp))
                let consumeExpr = mkSynMatchLambda(false,true,mTryToLast,clauses,NoSequencePointAtStickyBinding)
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env  tryRange ad "TryWith" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("TryWith"),tryRange))
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env tryRange ad "Delay" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("Delay"),tryRange))
                Some(mksynCall "TryWith" tryRange [mksynCall "Delay" tryRange [mkSynDelay innerComp.Range (trans innerComp)]; consumeExpr])

            | SynExpr.YieldOrReturnFrom((_isTrueYield,isTrueReturn),yieldExpr,m) -> 
                if isTrueReturn then 
                    if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "ReturnFrom" interpExprTy) then 
                        errorR(Error(FSComp.SR.tcRequireBuilderMethod("ReturnFrom"),m))
                        Some yieldExpr
                    else
                        Some (mksynCall "ReturnFrom" m [yieldExpr])
                else
                    if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad "YieldFrom" interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod("YieldFrom"),m))
                    Some (mksynCall "YieldFrom" m [yieldExpr])
                

            | SynExpr.YieldOrReturn((isTrueYield,_isTrueReturn),yieldExpr,m) -> 
                let methName = (if isTrueYield then "Yield" else "Return")
                if isNil (TryFindIntrinsicOrExtensionMethInfo cenv env m ad methName interpExprTy) then error(Error(FSComp.SR.tcRequireBuilderMethod(methName),m))
                Some(mksynCall methName m [yieldExpr])

            | _ -> None
        and trans c = 
            match tryTrans c with 
            | Some e -> e
            | None -> 
                // This only occurs in final position in a sequence
                match c with 
                // "do! expr;" in final position is treated as { let! () = expr in return () }
                | SynExpr.DoBang(rhsExpr,m) -> 
                    trans (SynExpr.LetOrUseBang(NoSequencePointAtDoBinding, false,SynPat.Const(SynConst.Unit, rhsExpr.Range), rhsExpr, SynExpr.YieldOrReturn((false,true), SynExpr.Const(SynConst.Unit,m), m),m))
                // "expr;" in final position is treated as { expr; zero }
                // Suppress the sequence point on the "zero"
                | _ -> 
                    SynExpr.Seq(SuppressSequencePointOnStmtOfSequential,true, c,trans (SynExpr.ImplicitZero m),m) 

        let coreSynExpr = trans comp

        let delayedExpr = 
            match TryFindIntrinsicOrExtensionMethInfo cenv env interpVarRange ad "Delay" interpExprTy with 
            | [] -> coreSynExpr
            | _ -> mksynCall "Delay" interpVarRange [(mkSynDelay2 coreSynExpr)]
            
        let runExpr = 
            match TryFindIntrinsicOrExtensionMethInfo cenv env interpVarRange ad "Run" interpExprTy with 
            | [] -> delayedExpr
            | _ -> mksynCall "Run" interpVarRange [delayedExpr]

        let lambdaExpr = SynExpr.Lambda (false,false,SynSimplePats.SimplePats ([mkSynSimplePatVar false (mkSynId interpVarRange interpVarName)],interpVarRange), runExpr, interpVarRange)

        let lambdaExpr ,tpenv= TcExpr cenv (interpExprTy --> ty) env tpenv lambdaExpr
        // beta-var-reduce to bind the builder using a 'let' binding
        let coreExpr = mkApps cenv.g ((lambdaExpr,tyOfExpr cenv.g lambdaExpr),[],[interpExpr],interpVarRange)

        coreExpr,tpenv

    // This case is used for sequence expressions
    | None -> 

        let mkDelayedExpr (coreExpr:Expr) = 
            let m = coreExpr.Range
            let ty = tyOfExpr cenv.g coreExpr
            mk_seq_delay cenv env m ty coreExpr

        let rec try_tc_comp env genOuterTy tpenv comp =
            match comp with 
            | SynExpr.ForEach (_spBind,SeqExprOnly(_seqExprOnly),pat,pseudoEnumExpr,innerComp,m) -> 
                // This expression is not checked with the knowledge it is an IEnumerable, since we permit other enumerable types with GetEnumerator/MoveNext methods, as does C# 
                let pseudoEnumExpr,arb_ty,tpenv = TcExprOfUnknownType cenv env tpenv pseudoEnumExpr
                let enumExpr,enumElemTy = ConvertArbitraryExprToEnumerable cenv arb_ty env pseudoEnumExpr
                let pat',_,vspecs,envinner,tpenv = TcMatchPattern cenv enumElemTy env tpenv (pat,None)
                let innerExpr,tpenv = tc_comp envinner genOuterTy tpenv innerComp
                
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

                    let matchv,matchExpr = conv_tcomp_match_clauses cenv env enumExprMark (pat',vspecs) innerExpr enumElemTy genOuterTy
                    let lam = mkLambda enumExprMark matchv (matchExpr,tyOfExpr cenv.g matchExpr)
                    Some(mk_seq_collect cenv env m enumElemTy genOuterTy lam enumExpr , tpenv)

            | SynExpr.For (spBind,id,start,dir,finish,innerComp,m) ->
                Some(tc_comp env genOuterTy tpenv (elim_fast_integer_for_loop (spBind,id,start,dir,finish,innerComp,m)))

            | SynExpr.While (_spWhile,guardExpr,innerComp,_m) -> 
                let guardExpr,tpenv = TcExpr cenv cenv.g.bool_ty env tpenv guardExpr
                let innerExpr,tpenv = tc_comp env genOuterTy tpenv innerComp
    
                let guardExprMark = guardExpr.Range
                let guardExpr = mkUnitDelayLambda cenv.g guardExprMark guardExpr
                let innerExpr = mkDelayedExpr innerExpr
                Some(mk_seq_generated cenv env guardExprMark genOuterTy guardExpr innerExpr, tpenv)

            | SynExpr.TryFinally (innerComp,unwindExpr,_mTryToLast,_spTry,_spFinally) ->
                let innerExpr,tpenv = tc_comp env genOuterTy tpenv innerComp
                let unwindExpr,tpenv = TcExpr cenv cenv.g.unit_ty env tpenv unwindExpr
            
                let unwindExprMark = unwindExpr.Range
                let unwindExpr = mkUnitDelayLambda cenv.g unwindExprMark unwindExpr
                let innerExpr = mkDelayedExpr innerExpr
                let innerExprMark = innerExpr.Range
                
                Some(mk_seq_finally cenv env innerExprMark genOuterTy innerExpr unwindExpr, tpenv)
            | SynExpr.Paren (_,m) -> 
                error(Error(FSComp.SR.tcConstructIsAmbiguousInSequenceExpression(),m))

            | SynExpr.ImplicitZero m -> 
                Some(mk_seq_empty cenv env m genOuterTy,tpenv )

            | SynExpr.DoBang(_rhsExpr,m) -> 
                error(Error(FSComp.SR.tcDoBangIllegalInSequenceExpression(),m))

            | SynExpr.Seq(sp,true,innerComp1, innerComp2,m) -> 
                // "expr; cexpr" is treated as sequential execution
                // "cexpr; cexpr" is treated as append
                match try_tc_comp env genOuterTy tpenv innerComp1 with 
                | None -> 
                    let innerExpr1,tpenv = TcStmtThatCantBeCtorBody cenv env tpenv innerComp1
                    let innerExpr2,tpenv = tc_comp env genOuterTy tpenv innerComp2

                    Some(Expr.Seq(innerExpr1,innerExpr2,NormalSeq,sp,m),tpenv)

                | Some (innerExpr1,tpenv) ->
                    let innerExpr2,tpenv = tc_comp env genOuterTy tpenv innerComp2
                    let innerExpr2 = mkDelayedExpr innerExpr2
                    Some(mk_seq_append cenv env innerComp1.Range genOuterTy innerExpr1 innerExpr2, tpenv)

            | SynExpr.IfThenElse (guardExpr,thenComp,elseCompOpt,spIfToThen,mIfToThen,mIfToEndOfElseBranch) ->
                let guardExpr',tpenv = TcExpr cenv cenv.g.bool_ty env tpenv guardExpr
                let thenExpr,tpenv = tc_comp env genOuterTy tpenv thenComp
                let elseComp = (match elseCompOpt with Some c -> c | None -> SynExpr.ImplicitZero mIfToThen)
                let elseExpr,tpenv = tc_comp env genOuterTy tpenv elseComp
                Some(mkCond spIfToThen SequencePointAtTarget mIfToEndOfElseBranch genOuterTy guardExpr' thenExpr elseExpr, tpenv)

            // 'let x = expr in expr'
            | SynExpr.LetOrUse (isRec,false (* not a 'use' binding *),binds,body,m) ->
                TcLinearLetExprs 
                    (fun ty envinner tpenv e -> tc_comp envinner ty tpenv e) 
                    cenv env ty 
                    (fun x -> x) 
                    tpenv 
                    (false(* don't consume 'use' bindings*),isRec,false,binds,body,m)  |> Some

            // 'use x = expr in expr'
            | SynExpr.LetOrUse (_isRec,true,[Binding (_vis,NormalBinding,_,_,_,_,_,pat,_,rhsExpr,_,_spBind)],innerComp,wholeExprMark) ->

                let bindPatTy = NewInferenceType ()
                let inputExprTy = NewInferenceType ()
                let pat',_,vspecs,envinner,tpenv = TcMatchPattern cenv bindPatTy env tpenv (pat,None)
                UnifyTypes cenv env m inputExprTy bindPatTy;
                let inputExpr,tpenv = TcExpr cenv inputExprTy env tpenv rhsExpr
                let innerExpr,tpenv = tc_comp envinner genOuterTy tpenv innerComp
                let inputExprMark = inputExpr.Range
                let matchv,matchExpr = conv_tcomp_match_clauses cenv env inputExprMark (pat',vspecs) innerExpr bindPatTy genOuterTy 
                let consumeExpr = mkLambda wholeExprMark matchv (matchExpr,genOuterTy)
                //SEQPOINT NEEDED - we must consume spBind on this path
                Some(mk_seq_using cenv env wholeExprMark bindPatTy genOuterTy inputExpr consumeExpr, tpenv)

            | SynExpr.LetOrUseBang(_,_,_,_,_,m) -> 
                error(Error(FSComp.SR.tcUseForInSequenceExpression(),m))

            | SynExpr.Match (spMatch,expr,clauses,false,_) ->
                let inputExpr,matchty,tpenv = TcExprOfUnknownType cenv env tpenv expr
                let tclauses,tpenv = 
                    List.mapFold 
                        (fun tpenv (Clause(pat,cond,innerComp,_,sp)) ->
                              let pat',cond',vspecs,envinner,tpenv = TcMatchPattern cenv matchty env tpenv (pat,cond)
                              let innerExpr,tpenv = tc_comp envinner genOuterTy tpenv innerComp
                              TClause(pat',cond',TTarget(vspecs, innerExpr,sp),rangeOfPat pat'),tpenv)
                        tpenv
                        clauses
                let inputExprTy = tyOfExpr cenv.g inputExpr
                let inputExprMark = inputExpr.Range
                let matchv,matchExpr = CompilePatternForMatchClauses cenv env inputExprMark inputExprMark true ThrowIncompleteMatchException inputExprTy genOuterTy tclauses 
                Some(mkLet spMatch inputExprMark matchv inputExpr matchExpr, tpenv)

            | SynExpr.TryWith (_,mTryToWith,_,_,_,_,_) ->
                error(Error(FSComp.SR.tcTryIllegalInSequenceExpression(),mTryToWith))

            | SynExpr.YieldOrReturnFrom((isTrueYield,_isTrueReturn),yieldExpr,m) -> 
                let resultExpr,genExprTy,tpenv = TcExprOfUnknownType cenv env tpenv yieldExpr

                if not isTrueYield then errorR(Error(FSComp.SR.tcUseYieldBangForMultipleResults(),m)) ;

                AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m  NoTrace genOuterTy genExprTy;
                Some(mkCoerceExpr(resultExpr,genOuterTy,m,genExprTy), tpenv)

            | SynExpr.YieldOrReturn((isTrueYield,_isTrueReturn),yieldExpr,m) -> 
                let genResultTy = NewInferenceType ()
                if not isTrueYield then errorR(Error(FSComp.SR.tcSeqResultsUseYield(),m)) ;
                UnifyTypes cenv env m genOuterTy (mkSeqTy cenv.g genResultTy);

                let resultExpr,tpenv = TcExpr cenv genResultTy env tpenv yieldExpr
                Some(mkCallSeqSingleton cenv.g m genResultTy resultExpr, tpenv )

            | _ -> None
                
        and tc_comp env genOuterTy tpenv comp =
            match try_tc_comp env genOuterTy tpenv comp with 
            | Some e -> e
            | None -> 
                // seq { ...; expr } is treated as 'seq { ... ; expr; yield! Seq.empty }'
                // Note this means seq { ...; () } is treated as 'seq { ... ; (); yield! Seq.empty }'
                let m = comp.Range
                let expr,tpenv = TcStmtThatCantBeCtorBody cenv env tpenv comp
                Expr.Seq(expr,mk_seq_empty cenv env m genOuterTy,NormalSeq,SuppressSequencePointOnStmtOfSequential,m),tpenv

        let genEnumElemTy = NewInferenceType ()
        UnifyTypes cenv env m ty (mkSeqTy cenv.g genEnumElemTy);

        let coreExpr,tpenv = tc_comp env ty tpenv comp
        let delayedExpr = mkDelayedExpr coreExpr
        delayedExpr,tpenv

//-------------------------------------------------------------------------
// Typecheck "expr ... " constructs where "..." is a sequence of applications,
// type applications and dot-notation projections. First extract known
// type information from the "..." part to use during type checking.
//
// 'ty' is the type expected for the entire chain of expr + lookups.
// 'exprty' is the type of the expression on the left of the lookup chain.
//
// Unsophisticated applications can propagate information from the expected overall type 'ty' 
// through to the leading function type 'exprty'. This is because the application 
// unambiguously implies a function type 
//------------------------------------------------------------------------- 

and PropagateThenTcDelayed cenv ty env tpenv m expr exprty (atomicFlag:ExprAtomicFlag) delayed = 
    
    let rec propagate delayed' exprm exprty = 
      match delayed' with 
      | [] -> 
          // Avoid unifying twice: we're about to unify in TcDelayed 
          if nonNil delayed then 
              UnifyTypes cenv env exprm ty exprty
      | DelayedSet _ :: _
      | DelayedDotLookup _ :: _ -> ()
      | DelayedTypeApp (_,appm) :: delayed'' ->
          // Note this case should not occur: would eventually give an "Unexpected type application" error in TcDelayed 
          propagate delayed'' appm exprty 

      | DelayedApp (_, arg,appm) :: delayed'' ->
          let denv = env.DisplayEnv
          match UnifyFunctionTypeUndoIfFailed cenv denv m exprty with
          | Some (_,resultTy) -> 
              propagate delayed'' appm resultTy 
          | None -> 
              let argm = arg.Range
              match arg with 
              | SynExpr.CompExpr _ -> ()
              | _ -> 
                 error (NotAFunction(denv,ty,exprm,argm)) 
              
    propagate delayed expr.Range exprty;
    TcDelayed cenv ty env tpenv m expr exprty atomicFlag delayed


/// Typecheck "expr ... " constructs where "..." is a sequence of applications,
/// type applications and dot-notation projections.
and TcDelayed cenv ty env tpenv m expr exprty (atomicFlag:ExprAtomicFlag) delayed = 

    // OK, we've typechecked the thing on the left of the delayed lookup chain. 
    // We can now record for posterity the type of this expression and the location of the expression. 
    if (atomicFlag = ExprAtomicFlag.Atomic) then
        CallExprHasTypeSink(m,env.NameEnv,exprty, env.DisplayEnv,AccessRightsOfEnv env);

    match delayed with 
    | [] -> UnifyTypes cenv env m ty exprty; expr.Expr,tpenv
    // expr.m(args) where x.m is a .NET method or index property 
    // expr.m<tyargs>(args) where x.m is a .NET method or index property 
    // expr.m where x.m is a .NET method or index property 
    | DelayedDotLookup (lid,m) :: delayed' ->
         TcLookupThen cenv ty env tpenv expr.Expr exprty lid delayed' m
    // f x 
    | DelayedApp (hpa,arg,appm) :: delayed' ->
        TcFunctionApplicationThen cenv ty env tpenv appm expr exprty arg hpa delayed'
    // f<tyargs> 
    | DelayedTypeApp (_,m) :: _delayed' ->
        error(Error(FSComp.SR.tcUnexpectedTypeApplication(),m))
    | DelayedSet _ :: _delayed' ->      
        error(Error(FSComp.SR.tcInvalidAssignment(),m))


and delay_rest rest m delayed = 
    match rest with 
    | [] -> delayed 
    | lid -> (DelayedDotLookup (rest,unionRanges m (rangeOfLid lid)) :: delayed)


//-------------------------------------------------------------------------
// TcFunctionApplicationThen: Typecheck "expr x" + projections
//------------------------------------------------------------------------- 

and TcFunctionApplicationThen cenv ty env tpenv appm expr exprty (arg: SynExpr) atomicFlag delayed' = 
    
    let denv = env.DisplayEnv
    let argm = arg.Range
    let funm = expr.Range
    match UnifyFunctionTypeUndoIfFailed cenv denv funm exprty with
    | Some (domainTy,resultTy) -> 

        // Notice the special case 'seq { ... }'
        // Set a flag in the syntax tree to say we noticed a leading 'seq'
        match arg with 
        | SynExpr.CompExpr (false,isNotNakedRefCell,_comp,_m) -> 
            isNotNakedRefCell := 
                !isNotNakedRefCell
                || 
                (match expr with 
                 | ApplicableExpr(_,Expr.Op(TOp.Coerce,_,[Expr.App(Expr.Val(vf,_,_),_,_,_,_)],_),_) when valRefEq cenv.g vf cenv.g.seq_vref -> 
                    //dprintfn "FOUND 'seq { ... }'"
                    true 
                 | _ -> 
                    //dprintfn "DID NOT FIND 'seq { ... }'"
                    false)
        | _ -> ()

        let arg',tpenv = TcExpr cenv domainTy env tpenv arg
        let expr' = buildApp cenv expr exprty arg' appm
        TcDelayed cenv ty env tpenv appm expr' resultTy atomicFlag delayed'
    | None -> 
        match arg with 
        | SynExpr.CompExpr (false,_isNotNakedRefCell,comp,_m) -> 
            let expr',tpenv = TcComputationExpression cenv env ty funm (Some(expr.Expr,exprty)) tpenv comp
            TcDelayed cenv ty env tpenv appm (MakeApplicableExprNoFlex cenv expr') (tyOfExpr cenv.g expr') ExprAtomicFlag.NonAtomic delayed' 
        | _ -> 
            error (NotAFunction(denv,ty,funm,argm)) 

//-------------------------------------------------------------------------
// TcLongIdentThen : Typecheck "A.B.C<D>.E.F ... " constructs
//------------------------------------------------------------------------- 

and TcLongIdentThen cenv ty env tpenv lid delayed =

    let ad = AccessRightsOfEnv env
    let typeNameResInfo = 
        // Given 'MyOverloadedType<int>.MySubType...' use arity of #given type arguments to help 
        // resolve type name lookup of 'MyOverloadedType' 
        // Also determine if type names should resolve to Item.Types or Item.CtorGroup 
        match delayed with 
        | DelayedTypeApp (tyargs,_) :: DelayedApp _ :: _ -> 
            (ResolveTypeNamesToCtors, Some(List.length tyargs))

        | DelayedTypeApp (tyargs,_) :: _ -> 
            // cases like 'MyType<int>.Sth' but also only 'MyType<int>.' 
            // (without LValue_get), which is needed for VS (when typing)
            (ResolveTypeNamesToTypeRefs, Some(List.length tyargs)) 

        | _ -> DefaultTypeNameResInfo

    let itemAndRest = ResolveLongIdentAsExprAndComputeRange cenv.nameResolver (rangeOfLid lid) ad env.eNameResEnv typeNameResInfo lid
    TcItemThen cenv ty env tpenv itemAndRest delayed

//-------------------------------------------------------------------------
// Typecheck "item+projections" 
//------------------------------------------------------------------------- *)
// itemRange is the textual range covered by the long identifiers that make up the item
and TcItemThen cenv overallTy env tpenv (item,itemRange,rest) delayed =
    let delayed = delay_rest rest itemRange delayed
    let ad = AccessRightsOfEnv env
    match item with 
    // x where x is a union case or active pattern result tag. 
    | (Item.UnionCase _ | Item.ExnCase _ | Item.ActivePatternResult _) as item -> 
        // ucaseAppTy is the type of the union constructor applied to its (optional) argument 
        let ucaseAppTy = NewInferenceType ()
        let mkConstrApp,argtys = 
          match item with 
          | Item.ActivePatternResult(apinfo, _, n, _) -> 
              let aparity = apinfo.Names.Length
              match aparity with 
              | 0 | 1 -> 
                  let mkConstrApp = function [arg] -> arg | _ -> error(InternalError("genConstrUnify",itemRange))
                  mkConstrApp, [ucaseAppTy]
              | _ ->
                  let ucref = mkChoiceCaseRef cenv.g itemRange aparity n
                  let _,_,tinst,_ = info_of_tcref itemRange ucref.TyconRef
                  let ucinfo = UnionCaseInfo(tinst,ucref)
                  ApplyUnionCaseOrExnTypes itemRange cenv env ucaseAppTy (Item.UnionCase ucinfo)
          | _ -> 
              ApplyUnionCaseOrExnTypes itemRange cenv env ucaseAppTy item
        let nargtys = List.length argtys
        // Subsumption at data constructions if argument type is nominal prior to equations for any arguments or return types
        let flexes = argtys |> List.map (isTyparTy cenv.g >> not)
        
        let (|FittedArgs|_|) arg = 
            match arg with 
            | SynExpr.Paren(SynExpr.Tuple(args,_),_)
            | SynExpr.Tuple(args,_)     when nargtys > 1 -> Some args
            | SynExpr.Paren(arg,_)
            | arg when nargtys = 1 -> Some [arg]
            | _ -> None

        match delayed with 
        // This is where the constructor is applied to an argument 
        | ((DelayedApp (atomicFlag, (FittedArgs args as origArg),appm))::delayed') ->

            // assert the overall result type if possible
            if isNil(delayed') then 
                UnifyTypes cenv env appm overallTy ucaseAppTy; 

                  
            let nargs = List.length args
            UnionCaseOrExnCheck env nargtys nargs appm;

            if nargtys > 1 then 
                let _,namedCallerArgs = GetMethodArgs origArg
                match namedCallerArgs with 
                | (_,id,_)::_ -> warning(Error(FSComp.SR.tcNamedArgumentsCannotBeUsedInUnionCaseConstructions(), id.idRange));
                | [] -> ()

            let args',tpenv = TcExprs cenv env appm tpenv flexes argtys args
            PropagateThenTcDelayed cenv overallTy env tpenv appm (MakeApplicableExprNoFlex cenv (mkConstrApp args')) ucaseAppTy atomicFlag delayed'

        | DelayedTypeApp (_x,tyappm) :: _delayed' ->
            error(Error(FSComp.SR.tcUnexpectedTypeApplication(),tyappm))
        | _ -> 
            // Work out how many syntactic arguments we really expect. Also return a function that builds the overall 
            // expression, but don't apply this function until after we've checked that the number of arguments is OK 
            // (or else we would be building an invalid expression) 
            
            // Unit-taking active pattern result can be applied to no args 
            let nargs,mkExpr = 
                // This is where the constructor is an active pattern result applied to no argument 
                // Unit-taking active pattern result can be applied to no args 
                if (nargtys = 1 && match item with Item.ActivePatternResult _ -> true | _ -> false) then 
                    UnifyTypes cenv env itemRange (List.head argtys) cenv.g.unit_ty;
                    1,(fun () -> mkConstrApp [mkUnit cenv.g itemRange])

                // This is where the constructor expects no arguments and is applied to no argument 
                elif nargtys = 0 then 
                    0,(fun () -> mkConstrApp []) 
                else 
                    // This is where the constructor expects arguments but is not applied to arguments, hence build a lambda 
                    nargtys, 
                    (fun () -> 
                        let vs,args = argtys |> List.mapi (fun i ty -> mkCompGenLocal itemRange ("arg"^string i) ty) |> List.unzip
                        let constrApp = mkConstrApp args
                        let lam = mkMultiLambda itemRange vs (constrApp, tyOfExpr cenv.g constrApp)
                        lam)
            UnionCaseOrExnCheck env nargtys nargs itemRange;
            let expr = mkExpr()
            let exprTy = tyOfExpr cenv.g expr
            PropagateThenTcDelayed cenv overallTy env tpenv itemRange (MakeApplicableExprNoFlex cenv expr) exprTy ExprAtomicFlag.Atomic delayed 

    | Item.Types(nm,(typ::_)) -> 
    
        match delayed with 
        | ((DelayedTypeApp(tyargs,tyappm))::(DelayedDotLookup (lid,_))::delayed') ->

            // If Item.Types is returned then the typ will be of the form TType_app(tcref,genericTyargs) where tyargs 
            // is a fresh instantiation for tcref. TcNestedTypeApplication will chop off precisely #genericTyargs args 
            // and replace them by 'tyargs' 
            let typ,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs env tpenv tyappm typ tyargs

            // Report information about the whole expression including type arguments to VS
            CallNameResolutionSink(tyappm,env.NameEnv,Item.Types(nm, [typ]),ItemOccurence.Use,env.DisplayEnv,AccessRightsOfEnv env)
            TcItemThen cenv overallTy env tpenv (ResolveExprDotLongIdentAndComputeRange cenv.nameResolver itemRange ad env.eNameResEnv typ lid IgnoreOverrides) delayed'
            
        | ((DelayedTypeApp(tyargs,tyappm))::_delayed') ->
            // A case where we have an incomplete name e.g. 'Foo<int>.' - we still want to report it to VS!
            let typ,_ = TcNestedTypeApplication cenv NewTyparsOK CheckCxs env tpenv tyappm typ tyargs
            CallNameResolutionSink(tyappm,env.NameEnv,Item.Types(nm, [typ]),ItemOccurence.Use,env.DisplayEnv,AccessRightsOfEnv env)
            
            // Same error as in the following case
            error(Error(FSComp.SR.tcInvalidUseOfTypeName(),itemRange));
            
        | _ -> 
            // In this case the type is not generic, and indeed we should never have returned Item.Types. 
            // That's because ResolveTypeNamesToCtors should have been set at the original 
            // call to ResolveLongIdentAsExprAndComputeRange 
            error(Error(FSComp.SR.tcInvalidUseOfTypeName(),itemRange));

    | Item.MethodGroup (methodName,minfos) -> 
        // Static method calls Type.Foo(arg1,...,argn) 
        let meths = List.map (fun minfo -> minfo,None) minfos
        match delayed with 
        | (DelayedApp (atomicFlag, arg,m)::delayed') ->
            TcMethodApplicationThen cenv env overallTy tpenv None [] m methodName ad NeverMutates false meths NormalValUse [arg] atomicFlag delayed'

        | (DelayedTypeApp(tys,tyappm)::DelayedApp (atomicFlag, arg,m)::delayed') ->
            let tyargs,tpenv = TcTypesOrMeasures None cenv NewTyparsOK CheckCxs env tpenv tys m
            
            // NOTE: This doesn't take instantiation into account
            CallNameResolutionSink(tyappm,env.NameEnv,item (* ! *),ItemOccurence.Use,env.DisplayEnv,AccessRightsOfEnv env)                        
            TcMethodApplicationThen cenv env overallTy tpenv (Some tyargs) [] m methodName ad NeverMutates false meths NormalValUse [arg] atomicFlag delayed'
        | _ -> 
            TcMethodApplicationThen cenv env overallTy tpenv None [] itemRange methodName ad NeverMutates false meths NormalValUse [] ExprAtomicFlag.Atomic delayed 

    | Item.CtorGroup(_,minfos) ->
        let objTy = 
            match minfos with 
            | (minfo :: _) -> minfo.EnclosingType
            | [] -> error(Error(FSComp.SR.tcTypeHasNoAccessibleConstructor(),itemRange))
        match delayed with 
        | ((DelayedApp (_, arg,argm))::delayed') ->
            TcCtorCall true cenv env tpenv overallTy objTy item false arg argm delayed'

        | ((DelayedTypeApp(tyargs,tyappm))::(DelayedApp (_, arg,m))::delayed') ->
            let objTy,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs env tpenv tyappm objTy tyargs
            minfos |> List.iter (fun minfo -> UnifyTypes cenv env tyappm minfo.EnclosingType objTy);
            
            // NOTE: This doesn't take instantiation into account
            CallNameResolutionSink(tyappm,env.NameEnv,item (* ! *),ItemOccurence.Use,env.DisplayEnv,AccessRightsOfEnv env)
            TcCtorCall true cenv env tpenv overallTy objTy item false arg m delayed'

        | _ -> 
            let text = List.map (stringOfMethInfo cenv.amap itemRange env.DisplayEnv) minfos
            if nonNil minfos then error(Error(FSComp.SR.tcInvalidUseOfTypeNameOrConstructorWithOverloads(String.concat "\n\r" text),itemRange))
            else error(Error(FSComp.SR.tcInvalidUseOfTypeNameOrConstructor(),itemRange))

    | Item.FakeInterfaceCtor _ ->
        error(Error(FSComp.SR.tcInvalidUseOfInterfaceType(),itemRange))
    | Item.ImplicitOp id ->

        let isPrefix = PrettyNaming.IsPrefixOperator id.idText
        let isTernary = PrettyNaming.IsTernaryOperator id.idText

        let argData = 
            if isPrefix then 
                [ Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true) ]
            elif isTernary then 
                [ Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true);
                  Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true);
                  Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true) ]
            else
                [ Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true);
                  Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true) ]
                
        let retTyData = Typar(mkSynId itemRange (GlobalSynArgNameGenerator.New()), HeadTypeStaticReq,true)
        let argTypars = argData |> List.map (fun d -> NewTypar (KindType, TyparFlexible,d,false,DynamicReq,[],false,false))
        let retTypar = NewTypar (KindType, TyparFlexible,retTyData,false,DynamicReq,[],false,false)
        let argTys = argTypars |> List.map mkTyparTy 
        let retTy = mkTyparTy retTypar

        let vs,ves = argTys |> List.mapi (fun i ty -> mkCompGenLocal itemRange ("arg"^string i) ty) |> List.unzip

        let memberFlags = StaticMemberFlags MemberKind.Member
        let logicalCompiledName = ComputeLogicalName id memberFlags
        let traitInfo = TTrait(argTys,logicalCompiledName,memberFlags,argTys,Some retTy,ref None)

        AddCxMethodConstraint env.DisplayEnv cenv.css itemRange NoTrace traitInfo;
      
        let expr = Expr.Op(TOp.TraitCall(traitInfo), [], ves, itemRange)
        let expr = mkLambdas itemRange [] vs (expr,retTy)
        PropagateThenTcDelayed cenv overallTy env tpenv itemRange (MakeApplicableExprNoFlex cenv expr) (tyOfExpr cenv.g expr) ExprAtomicFlag.NonAtomic delayed
        
    | Item.DelegateCtor typ ->
        match delayed with 
        | ((DelayedApp (atomicFlag, arg,m))::delayed') ->
            TcNewDelegateThen cenv overallTy env tpenv m typ arg atomicFlag delayed'
        | ((DelayedTypeApp(tyargs,tyappm))::(DelayedApp (atomicFlag, arg,m))::delayed') ->
            let typ,tpenv = TcNestedTypeApplication cenv NewTyparsOK CheckCxs env tpenv tyappm typ tyargs
            
            // NOTE: This doesn't take instantiation into account
            CallNameResolutionSink(tyappm,env.NameEnv,item (* ! *),ItemOccurence.Use,env.DisplayEnv,AccessRightsOfEnv env)            
            TcNewDelegateThen cenv overallTy env tpenv m typ arg atomicFlag delayed'
        | _ -> error(Error(FSComp.SR.tcInvalidUseOfDelegate(),itemRange))

    | Item.Value vref -> 

        match delayed with 
        // Mutable value set: 'v <- e' 
        | DelayedSet(e2,m) :: delayed' ->
            if nonNil delayed' then error(Error(FSComp.SR.tcInvalidAssignment(),m));
            UnifyTypes cenv env m overallTy cenv.g.unit_ty;
            let vty = vref.Type
            let vty2 = 
                if isByrefTy cenv.g vty then 
                    destByrefTy cenv.g vty 
                else 
                    if not vref.IsMutable then error (ValNotMutable(env.DisplayEnv,vref,m));
                    vty 
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true vty2 env tpenv e2
            let vexp = 
                if isByrefTy cenv.g vty then 
                  mkAddrSet m vref e2'
                else 
                  mkValSet m vref e2'
                
            PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprNoFlex cenv vexp) (tyOfExpr cenv.g vexp) ExprAtomicFlag.NonAtomic delayed'

        // Value instantiation: v<tyargs> ... 
        | (DelayedTypeApp(tys,tyappm)::delayed') ->
            // Note: we know this is a NormalValUse or PossibleConstrainedCall because: 
            //   - it isn't a CtorValUsedAsSuperInit 
            //   - it isn't a CtorValUsedAsSelfInit 
            //   - it isn't a VSlotDirectCall (uses of base values do not take type arguments 
            let checkTys tpenv kinds = TcTypesOrMeasures (Some kinds) cenv NewTyparsOK CheckCxs env tpenv tys itemRange
            let (vexp, isSpecial, _, _, tpenv) = TcVal cenv env tpenv vref (Some (NormalValUse, checkTys)) itemRange
            let vexp = (if isSpecial then MakeApplicableExprNoFlex cenv vexp else MakeApplicableExprWithFlex cenv env vexp)
            // type of the expression (e.g. For the source text "sizeof<float>" vexpty will be the TAST type for int32)
            let vexpty = vexp.Type 
            
            // We need to eventually record the type resolution for an expression, but this is done
            // inside PropagateThenTcDelayed, so we don't have to explicitly call 'CallExprHasTypeSink' here            
            PropagateThenTcDelayed cenv overallTy env tpenv tyappm vexp vexpty ExprAtomicFlag.Atomic delayed'

        // Value get 
        | _ ->  
            let (vexp, isSpecial, _, _, tpenv) = TcVal cenv env tpenv vref None itemRange
            let vexp = (if isSpecial then MakeApplicableExprNoFlex cenv vexp else MakeApplicableExprWithFlex cenv env vexp)
            let vexpty = vexp.Type
            PropagateThenTcDelayed cenv overallTy env tpenv itemRange vexp vexpty ExprAtomicFlag.Atomic delayed
        
    | Item.Property (nm,pinfos) ->
        if isNil pinfos then error (InternalError ("Unexpected error: empty property list",itemRange));
        let pinfo = List.head pinfos
        let _, tyargsOpt,args,delayed,tpenv = 
            if pinfo.IsIndexer 
            then GetMemberApplicationArgs delayed cenv env tpenv 
            else ExprAtomicFlag.Atomic,None,[mkSynUnit itemRange],delayed,tpenv
        if not pinfo.IsStatic then error (Error (FSComp.SR.tcPropertyIsNotStatic(nm),itemRange));
        match delayed with 
        | DelayedSet(e2,m) :: delayed' ->
            let args = if pinfo.IsIndexer then args else []
            if nonNil(delayed') then error(Error(FSComp.SR.tcInvalidAssignment(),m));
            // Static Property Set (possibly indexer) 
            UnifyTypes cenv env m overallTy cenv.g.unit_ty;
            let meths = pinfos |> List.choose (fun pinfo -> if pinfo.HasSetter then Some(pinfo.SetterMethod,Some pinfo) else None)
            // Note: static calls never mutate a struct object argument
            TcMethodApplicationThen cenv env overallTy tpenv tyargsOpt [] m nm ad NeverMutates true meths NormalValUse (args@[e2]) ExprAtomicFlag.NonAtomic delayed'
        | _ -> 
            // Static Property Get (possibly indexer) 
            let meths = pinfos |> List.choose (fun pinfo -> if pinfo.HasGetter then Some(pinfo.GetterMethod,Some pinfo) else None) 
            if isNil(meths) then error (Error (FSComp.SR.tcPropertyIsNotReadable(nm),itemRange));
            // Note: static calls never mutate a struct object argument
            TcMethodApplicationThen cenv env overallTy tpenv tyargsOpt [] itemRange nm ad NeverMutates true meths NormalValUse args ExprAtomicFlag.Atomic delayed

    | Item.ILField finfo -> 

        CheckILFieldInfoAccessible cenv.g cenv.amap itemRange ad finfo;
        if not finfo.IsStatic then error (Error (FSComp.SR.tcFieldIsNotStatic(finfo.FieldName),itemRange));
        CheckILFieldAttributes cenv.g finfo itemRange;
        let fref = finfo.ILFieldRef
        let exprty = finfo.FieldType(cenv.amap,itemRange)
        match delayed with 
        | DelayedSet(e2,m) :: _delayed' ->
            UnifyTypes cenv env m overallTy (cenv.g.unit_ty);
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true exprty env tpenv e2
            let expr = BuildILStaticFieldSet m finfo e2'
            expr,tpenv
        | _ -> 
           // Get static IL field 
            let expr = 
              match finfo.LiteralValue with 
              | Some lit -> 
                  Expr.Const(TcFieldInit itemRange lit,itemRange,exprty) 
              | None -> 
                let isValueType = finfo.IsValueType
                let valu = if isValueType then AsValue else AsObject

                // The empty instantiation on the fspec is OK, since we make the correct fspec in Ilxgen.gen_asm 
                // This ensures we always get the type instantiation right when doing this from 
                // polymorphic code, after inlining etc. 
                let fspec = mkILFieldSpec(fref,mkILNamedTy valu fref.EnclosingTypeRef [])

                // Add an I_nop if this is an initonly field to make sure we never recognize it as an lvalue. See mkExprAddrOfExpr. 
                mkAsmExpr ([ mkNormalLdsfld fspec ] @ (if finfo.IsInitOnly then [ AI_nop ] else []), finfo.TypeInst,[],[exprty],itemRange)
            PropagateThenTcDelayed cenv overallTy env tpenv itemRange (MakeApplicableExprWithFlex cenv env expr) exprty ExprAtomicFlag.Atomic delayed

    | Item.RecdField rfinfo -> 
        // Get static F# field or literal 
        CheckRecdFieldInfoAccessible itemRange ad rfinfo;
        if not rfinfo.IsStatic then error (Error (FSComp.SR.tcFieldIsNotStatic(rfinfo.Name),itemRange));
        CheckRecdFieldInfoAttributes cenv.g rfinfo itemRange |> CommitOperationResult;        
        let fref = rfinfo.RecdFieldRef
        let fieldTy = rfinfo.FieldType
        match delayed with 
        | DelayedSet(e2,m) :: delayed' ->
            if nonNil(delayed') then error(Error(FSComp.SR.tcInvalidAssignment(),m));
        
            // Set static F# field 
            CheckRecdFieldMutation m env.DisplayEnv rfinfo [];
            UnifyTypes cenv env m overallTy cenv.g.unit_ty;
            let fieldTy = rfinfo.FieldType
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true fieldTy env tpenv e2
            let expr = mkStaticRecdFieldSet (rfinfo.RecdFieldRef,rfinfo.TypeInst,e2',m)
            expr,tpenv
            
        | _  ->
            let exprty = fieldTy
            let expr = 
              match rfinfo.LiteralValue with 
              // Get literal F# field 
              | Some lit -> Expr.Const(lit,itemRange,exprty)
              // Get static F# field 
              | None -> mkStaticRecdFieldGet (fref,rfinfo.TypeInst,itemRange) 
            PropagateThenTcDelayed cenv overallTy env tpenv itemRange (MakeApplicableExprWithFlex cenv env expr) exprty ExprAtomicFlag.Atomic delayed

    | Item.Event einfo -> 
        // Instance IL event (fake up event-as-value) 
        TcEventValueThen cenv overallTy env tpenv itemRange None einfo delayed
     
    | _ -> error(Error(FSComp.SR.tcLookupMayNotBeUsedHere(), itemRange))


//-------------------------------------------------------------------------
// Typecheck "expr.A.B.C ... " constructs
//------------------------------------------------------------------------- 

and GetMemberApplicationArgs delayed cenv env tpenv =
    match delayed with 
    | DelayedApp (atomicFlag, arg,_) :: delayed' -> atomicFlag,None,[arg],delayed',tpenv
    | DelayedTypeApp(tyargs,_) :: DelayedApp (atomicFlag, arg,m) :: delayed' ->
        let tyargs,tpenv = TcTypesOrMeasures None cenv NewTyparsOK CheckCxs env tpenv tyargs m
        atomicFlag,Some(tyargs),[arg],delayed',tpenv
    | delayed' ->
        ExprAtomicFlag.NonAtomic,None,[],delayed',tpenv


and TcLookupThen cenv overallTy env tpenv objExpr objExprTy lid delayed m =
    let objArgs = [objExpr]
    let ad = AccessRightsOfEnv env

    // 'base' calls use a different resolution strategy when finding methods. 
    let findFlag = 
        let baseCall = IsBaseCall objArgs
        (if baseCall then PreferOverrides else IgnoreOverrides)
        
    // Canonicalize inference problem prior to '.' lookup on variable types 
    if isTyparTy cenv.g objExprTy then 
        GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,env.DisplayEnv,m) (freeInTypeLeftToRight cenv.g false objExprTy);
    
    let item,m,rest = ResolveExprDotLongIdentAndComputeRange cenv.nameResolver m ad env.eNameResEnv objExprTy lid findFlag
    let delayed = delay_rest rest m delayed

    match item with
    | Item.MethodGroup (methodName,minfos) -> 
        let atomicFlag,tyargsOpt,args,delayed,tpenv = GetMemberApplicationArgs delayed cenv env tpenv 
        let meths = List.map (fun minfo -> minfo,None) minfos
        // We pass PossiblyMutates here because these may actually mutate a value type object 
        // To get better warnings we special case some of the few known mutate-a-struct method names 
        let mutates = (if methodName = "MoveNext" || methodName = "GetNextArg" then DefinitelyMutates else PossiblyMutates)
        TcMethodApplicationThen cenv env overallTy tpenv tyargsOpt objArgs m methodName ad mutates false meths NormalValUse args atomicFlag delayed 

    | Item.Property (nm,pinfos) ->
        // Instance property 
        if isNil pinfos then error (InternalError ("Unexpected error: empty property list",m)); // timng : made a change here
        let pinfo = List.head pinfos
        let atomicFlag,tyargsOpt,args,delayed,tpenv = 
            if pinfo.IsIndexer
            then GetMemberApplicationArgs delayed cenv env tpenv 
            else ExprAtomicFlag.Atomic,None,[mkSynUnit m],delayed,tpenv
        if pinfo.IsStatic then error (Error (FSComp.SR.tcPropertyIsStatic(nm),m));
        match delayed with 
        | DelayedSet(e2,m) :: delayed' ->
            let args = if pinfo.IsIndexer then args else []
            if nonNil(delayed') then error(Error(FSComp.SR.tcInvalidAssignment(),m));
        
            // Instance property setter 
            UnifyTypes cenv env m overallTy (cenv.g.unit_ty);
            let meths = pinfos |> List.choose (fun pinfo -> if pinfo.HasSetter then Some(pinfo.SetterMethod,Some pinfo) else None) 
            if isNil meths then error (Error (FSComp.SR.tcPropertyCannotBeSet1(nm),m));
            let mut = (if isStructTy cenv.g (tyOfExpr cenv.g objExpr) then DefinitelyMutates else PossiblyMutates)
            TcMethodApplicationThen cenv env overallTy tpenv tyargsOpt objArgs m nm ad mut true meths NormalValUse (args @ [e2]) atomicFlag [] 
        | _ ->                   
            // Instance property getter
            let meths = pinfos |> List.choose (fun pinfo -> if pinfo.HasGetter then Some(pinfo.GetterMethod,Some pinfo) else None) 
            if isNil meths then error (Error (FSComp.SR.tcPropertyIsNotReadable(nm),m));
            TcMethodApplicationThen cenv env overallTy tpenv tyargsOpt objArgs m nm ad PossiblyMutates true meths NormalValUse args atomicFlag delayed 
        
    | Item.RecdField rfinfo ->
        // Get or set instance F# field or literal 
        RecdFieldInstanceChecks cenv.g ad m rfinfo;
        let tgty = rfinfo.EnclosingType
        let valu = isStructTy cenv.g tgty
        AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace tgty objExprTy; 
        let objExpr = if valu then objExpr else mkCoerceExpr(objExpr,tgty,m,objExprTy)
        let _,ftinst,fieldTy = FreshenPossibleForallTy cenv.g m TyparFlexible (rfinfo.FieldType)
        match delayed with 
        | DelayedSet(e2,stmtRange) :: delayed' ->
            // Mutable value set: 'v <- e' 
            if nonNil(delayed') then error(Error(FSComp.SR.tcInvalidAssignment(),m));
            CheckRecdFieldMutation m env.DisplayEnv rfinfo ftinst;
            UnifyTypes cenv env stmtRange overallTy (cenv.g.unit_ty);
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true fieldTy env tpenv e2
            BuildRecdFieldSet cenv.g stmtRange objExpr rfinfo e2',tpenv

        | _ ->

            // Instance F# Record or Class field 
            let objExpr' = mkRecdFieldGet cenv.g (objExpr,rfinfo.RecdFieldRef,rfinfo.TypeInst,ftinst,m)
            PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprWithFlex cenv env objExpr') fieldTy ExprAtomicFlag.Atomic delayed 
        
    | Item.ILField finfo -> 
        // Get or set instance IL field 
        ILFieldInstanceChecks  cenv.g cenv.amap ad m finfo;
        let exprty = finfo.FieldType(cenv.amap,m)
        
        match delayed with 
        // Set instance IL field 
        | DelayedSet(e2,m) :: _delayed' ->
            UnifyTypes cenv env m overallTy (cenv.g.unit_ty)
            // Always allow subsumption on assignment to fields
            let e2',tpenv = TcExprFlex cenv true exprty env tpenv e2
            let expr = BuildILFieldSet cenv.g m objExpr finfo e2'
            expr,tpenv
        | _ ->        
            let expr = BuildILFieldGet cenv.g cenv.amap m objExpr finfo 
            PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprWithFlex cenv env expr) exprty ExprAtomicFlag.Atomic delayed 

    | Item.Event einfo -> 
        // Instance IL event (fake up event-as-value) 
        TcEventValueThen cenv overallTy env tpenv m (Some(objExpr,objExprTy)) einfo delayed
     
    | (Item.FakeInterfaceCtor _ | Item.DelegateCtor _) -> error (Error (FSComp.SR.tcConstructorsCannotBeFirstClassValues(), m))
    | _ -> error (Error (FSComp.SR.tcSyntaxFormUsedOnlyWithRecordLabelsPropertiesAndFields(), m))

and TcEventValueThen cenv overallTy env tpenv m objDetails (einfo:EventInfo) delayed = 
    // Instance IL event (fake up event-as-value) 
    //let (ILEventInfo(tinfo,edef)) = einfo 
    let nm = einfo.EventName
    let ad = AccessRightsOfEnv env
    match objDetails, einfo.IsStatic with 
    | Some _, true -> error (Error (FSComp.SR.tcEventIsStatic(nm),m));
    | None, false -> error (Error (FSComp.SR.tcEventIsNotStatic(nm),m));
    | _ -> ()

    let delegateType = einfo.GetDelegateType(cenv.amap,m)
    let invokeMethInfo,delArgTys,_,_ = GetSigOfFunctionForDelegate cenv.infoReader delegateType m ad
    let objArgs = (Option.toList (Option.map fst objDetails))
    MethInfoChecks cenv.g cenv.amap true None objArgs (AccessRightsOfEnv env) m invokeMethInfo;
    
    // This checks for and drops the 'object' sender 
    let args_ty = ArgsTypOfEventInfo cenv.infoReader m ad einfo
    if not (slotSigHasVoidReturnTy (SlotSigOfMethodInfo cenv.amap m invokeMethInfo)) then errorR (nonStandardEventError einfo.EventName m);
    let devent_ty = mkIEventType cenv.g delegateType args_ty

    let bindObjArgs f =
        match objDetails with 
        | None -> f []
        | Some (objExpr,objExprTy) -> mkCompGenLetIn m "eventTarget" objExprTy objExpr (fun (_,ve) -> f [ve]) 

    // Bind the object target expression to make sure we only run its sdie effects once, and to make 
    // sure if it's a mutable reference then we dereference it - see FSharp 1.0 bug 942 
    let expr = 
        bindObjArgs (fun objVars -> 
             //     EventHelper ((fun d -> e.add_X(d)), (fun d -> e.remove_X(d)), (fun f -> new 'Delegate(f)))
            mkCallCreateEvent cenv.g m delegateType args_ty
               (let dv,de = mkCompGenLocal m "eventDelegate" delegateType
                let callExpr,_ = BuildMethodCall cenv env PossiblyMutates m false (einfo.GetAddMethod(m)) NormalValUse [] objVars [de]
                mkLambda m dv (callExpr, cenv.g.unit_ty))
               (let dv,de = mkCompGenLocal m "eventDelegate" delegateType
                let callExpr,_ = BuildMethodCall cenv env PossiblyMutates m false (einfo.GetRemoveMethod(m)) NormalValUse [] objVars [de]
                mkLambda m dv (callExpr, cenv.g.unit_ty))
               (let fvty = (cenv.g.obj_ty --> (args_ty --> cenv.g.unit_ty))
                let fv,fe = mkCompGenLocal m "callback" fvty
                let createExpr = BuildNewDelegateExpr (Some einfo) cenv delegateType (invokeMethInfo,delArgTys) (fe,fvty) m
                mkLambda m fv (createExpr, delegateType)))

    let exprty = devent_ty
    PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprNoFlex cenv expr) exprty ExprAtomicFlag.Atomic delayed 
 

//-------------------------------------------------------------------------
// Method uses can calls
//------------------------------------------------------------------------- 

/// Typecheck method/member calls and uses of members as first-class values.
and TcMethodApplicationThen 
       cenv 
       env
       overallTy           // The type of the overall expression including "delayed". THe method "application" may actually be a use of a member as 
                    // a first-class function value, when this would be a function type. 
       tpenv 
       userTypeArgs // The return type of the overall expression including "delayed" 
       objArgs      // The 'obj' arguments in obj.M(...) and obj.M, if any 
       m           // The range of the whole application
       methodName  // string, name of the method 
       ad          // accessibility rights of the caller 
       mut         // what do we know/assume about whether this method will mutate or not? 
       isProp      // is this a property call? Used for better error messages and passed to BuildMethodCall 
       meths       // the set of methods we may be calling 
       isSuperInit // is this a special invocation, e.g. a super-class constructor call. Passed through to BuildMethodCall 
       args        // the _syntactic_ method arguments, not yet type checked. 
       atomicFlag  // is the expression atomic or not? 
       delayed     // further lookups and applications that follow this 
     =

    // Nb. args is always of List.length <= 1 except for indexed setters, when it is 2  
    let m = (m,args) ||> List.fold (fun m arg -> unionRanges m arg.Range) 

    // Work out if we know anything about the return type of the overall expression. If there are any delayed 
    // lookups then we don't know anything. 
    let exprTy = if isNil delayed then overallTy else NewInferenceType ()

    // Call the helper below to do the real checking 
    let (expr,attributeAssignedNamedItems,delayed),tpenv = 
        TcMethodApplication false cenv env tpenv userTypeArgs objArgs m methodName ad mut isProp meths isSuperInit args exprTy delayed

    // Give errors if some things couldn't be assigned 
    if nonNil attributeAssignedNamedItems then (
        let (CallerNamedArg(id,_)) = List.head attributeAssignedNamedItems
        errorR(Error(FSComp.SR.tcNamedArgumentDidNotMatch(id.idText),id.idRange));
    );


    // Resolve the "delayed" lookups 
    let exprty = (tyOfExpr cenv.g expr)

    PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprNoFlex cenv expr) exprty atomicFlag delayed 

and GetNewInferenceTypeForMethodArg cenv x =
    match x with 
    | SynExpr.Paren(a,_) -> GetNewInferenceTypeForMethodArg cenv a
    | SynExpr.AddressOf(true,a,_,_) -> mkByrefTy cenv.g (GetNewInferenceTypeForMethodArg cenv a)
    | SynExpr.Lambda(_,_,_,a,_) -> (NewInferenceType () --> GetNewInferenceTypeForMethodArg cenv a)
    | _ -> NewInferenceType ()

/// Method calls, property lookups, attribute constructions etc. get checked through here 
and TcMethodApplication 
        checkingAttributeCall 
        cenv 
        env 
        tpenv 
        tyargsOpt
        objArgs 
        m 
        methodName 
        ad 
        mut 
        isProp 
        calledMethsAndProps 
        isSuperInit 
        curriedCallerArgs 
        exprTy 
        delayed
    =

    let denv = env.DisplayEnv
    let nenv = env.NameEnv

    let isSimpleFormalArg (isParamArrayArg,isOutArg,optArgInfo: OptionalArgInfo) = 
        not isParamArrayArg && not isOutArg && not optArgInfo.IsOptional    
    
    let objArgTys = objArgs |> List.map (tyOfExpr cenv.g)

    let calledMeths = calledMethsAndProps |> List.map fst

    // Uses of curried members are ALWAYS treated as if they are first class uses of members. 
    // Curried members may not be overloaded (checked at use-site for curried members brought into scope through extension members)
    let curriedCallerArgs,exprTy,delayed = 
        match calledMeths with 
        | [calledMeth] when not isProp && calledMeth.NumArgs.Length > 1 ->
            [], NewInferenceType (),[ for x in curriedCallerArgs -> DelayedApp(ExprAtomicFlag.NonAtomic, x, x.Range) ] @ delayed
        | _ when not isProp && calledMeths |> List.exists (fun calledMeth -> calledMeth.NumArgs.Length > 1) ->
            // This condition should only apply when multiple conflicting curried extension members are brought into scope
            error(Error(FSComp.SR.tcOverloadsCannotHaveCurriedArguments(),m))
        | _ -> 
            curriedCallerArgs,exprTy,delayed

    let candidates = calledMeths |> List.filter (IsMethInfoAccessible cenv.amap m ad)

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
          
          let MakeUnnamedCallerArgInfo x = (x, GetNewInferenceTypeForMethodArg cenv x, x.Range)

          // "single named item" rule. This is where we have a single accessible method 
          //      member x.M(arg1) 
          // being used with  
          //      x.M (x,y) 
          // Without this rule this requires 
          //      x.M ((x,y)) 
          match candidates with 
          | [calledMeth] 
                when (namedCurriedCallerArgs |> List.forall isNil && 
                      let curriedCalledArgs = calledMeth |> ParamAttribsOfMethInfo cenv.amap m 
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
                      let curriedCalledArgs = calledMeth |> ParamAttribsOfMethInfo cenv.amap m 
                      curriedCalledArgs.Length = 1 &&
                      curriedCalledArgs.Head.Length > 1 &&
                      curriedCalledArgs.Head |> List.forall isSimpleFormalArg) ->

              // The call lambda has function type
              let exprTy = mkFunTy (NewInferenceType ()) exprTy
              
              (None, Some unnamedCurriedCallerArgs.Head.Head, exprTy)

          | _ ->
              let unnamedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.mapSquared MakeUnnamedCallerArgInfo
              let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.mapSquared (fun (isOpt,nm,x) -> nm,isOpt,x,GetNewInferenceTypeForMethodArg cenv x, x.Range)

              (Some (unnamedCurriedCallerArgs, namedCurriedCallerArgs), None, exprTy)
    

    let CalledMethHasSingleArgumentGroupOfThisLength n (calledMeth:MethInfo) =
       let curriedMethodArgAttribs = ParamAttribsOfMethInfo cenv.amap m calledMeth
       curriedMethodArgAttribs.Length = 1 && 
       curriedMethodArgAttribs.Head.Length = n

    let GenerateMatchingSimpleArgumentTypes (calledMeth:MethInfo) =
        let curriedMethodArgAttribs = ParamAttribsOfMethInfo cenv.amap m calledMeth
        curriedMethodArgAttribs 
        |> List.map (List.filter isSimpleFormalArg)
        |> List.map (NewInferenceTypes)

    let UnifyMatchingSimpleArgumentTypes exprTy (calledMeth:MethInfo) =
        let curriedArgTys = GenerateMatchingSimpleArgumentTypes calledMeth
        let returnTy = 
            (exprTy,curriedArgTys) ||>  List.fold (fun exprTy argTys -> 
                let domainTy,resultTy = UnifyFunctionType None cenv denv m exprTy
                UnifyTypes cenv env m  domainTy (mkTupledTy cenv.g argTys);
                resultTy);
        curriedArgTys,returnTy

    if isProp && isNone curriedCallerArgsOpt then 
        error(Error(FSComp.SR.parsIndexerPropertyRequiresAtLeastOneArgument(),m))

    // STEP 1. UnifyUniqueOverloading. This happens BEFORE we type check the arguments. 
    // Extract what we know about the caller arguments, either type-directed if 
    // no arguments are given or else based on the syntax of the arguments. 
    let uniquelyResolved,preArgumentTypeCheckingCalledMethGroup = 
        let dummyExpr = mkUnit cenv.g m
      
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
                let unnamedCurriedCallerArgs = curriedArgTys |> List.mapSquared (fun ty -> CallerArg(ty,m,false,dummyExpr))  
                let namedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.map (fun _ -> [])
                unnamedCurriedCallerArgs, namedCurriedCallerArgs,returnTy
                
            // "type directed" rule for first-class uses of ambiguous methods. 
            // By context we know a type for the input argument. If it's a tuple 
            // this gives us the a potential number of arguments expected. Indeed even if it's a variable 
            // type we assume the number of arguments is just "1". 
            | None,_ ->
            
                let domainTy,returnTy = UnifyFunctionType None cenv denv m exprTy
                let argTys = if isUnitTy cenv.g domainTy then [] else  tryDestTupleTy cenv.g domainTy
                // Only apply this rule if a candidate method exists with this number of arguments
                let argTys = 
                    if candidates |> List.exists (CalledMethHasSingleArgumentGroupOfThisLength argTys.Length) then 
                       argTys
                    else 
                       [domainTy]
                let unnamedCurriedCallerArgs = [argTys |> List.map (fun ty -> CallerArg(ty,m,false,dummyExpr)) ]
                let namedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.map (fun _ -> [])
                unnamedCurriedCallerArgs, namedCurriedCallerArgs, returnTy
                

            | Some (unnamedCurriedCallerArgs,namedCurriedCallerArgs),_ -> 
                let unnamedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.mapSquared (fun (_,xty,xm) -> CallerArg(xty,xm,false,dummyExpr))
                let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.mapSquared (fun (id,isOpt,_,xty,xm) -> CallerNamedArg(id,CallerArg(xty,xm,isOpt,dummyExpr))) 
                unnamedCurriedCallerArgs, namedCurriedCallerArgs, exprTy

        let callerArgCounts = (List.sumBy List.length unnamedCurriedCallerArgs, List.sumBy List.length namedCurriedCallerArgs)

        let mk_CalledMeth (minfo,pinfoOpt,usesParamArrayConversion) = 
            let minst = FreshenMethInfo m minfo
            let userTypeArgs = Option.otherwise tyargsOpt minst
            let allArgs = List.zip unnamedCurriedCallerArgs namedCurriedCallerArgs
            MakeCalledMeth(cenv.infoReader,checkingAttributeCall, FreshenMethInfo, m,ad,minfo,minst,userTypeArgs,pinfoOpt,objArgTys,allArgs,usesParamArrayConversion,true)

        let preArgumentTypeCheckingCalledMethGroup = 
            [ for (minfo,pinfoOpt) in calledMethsAndProps do
                let meth = mk_CalledMeth (minfo,pinfoOpt,true) 
                yield meth
                if meth.UsesParamArrayConversion then 
                    yield mk_CalledMeth (minfo,pinfoOpt,false) ]
                    
        let csenv = (MakeConstraintSolverEnv cenv.css m denv)
        let uniquelyResolved = UnifyUniqueOverloading csenv callerArgCounts methodName ad preArgumentTypeCheckingCalledMethGroup returnTy |> CommitOperationResult
        uniquelyResolved,preArgumentTypeCheckingCalledMethGroup

    // STEP 2. Type check arguments 
    let unnamedCurriedCallerArgs,namedCurriedCallerArgs,lambdaVars,returnTy,tpenv =  
    
        // STEP 2a. First extract what we know about the caller arguments, either type-directed if 
        // no arguments are given or else based on the syntax of the arguments. 
        let unnamedCurriedCallerArgs,namedCurriedCallerArgs,lambdaVars,returnTy,tpenv = 
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
                        let domainTy,returnTy = UnifyFunctionType None cenv denv m exprTy
                        let argTys = if isUnitTy cenv.g domainTy then [] else  tryDestTupleTy cenv.g domainTy
                        // Only apply this rule if a candidate method exists with this number of arguments
                        let argTys = 
                            if candidates |> List.exists (CalledMethHasSingleArgumentGroupOfThisLength argTys.Length) then 
                                argTys                                  
                            else
                                [domainTy]
                        [argTys],returnTy
                        
                let lambdaVarsAndExprs = curriedArgTys |> List.mapiSquared (fun i j ty -> mkCompGenLocal m ("arg"^string i^string j) ty)
                let unnamedCurriedCallerArgs = lambdaVarsAndExprs |> List.mapSquared (fun (_,e) -> CallerArg(tyOfExpr cenv.g e,e.Range,false,e))
                let namedCurriedCallerArgs = lambdaVarsAndExprs |> List.map (fun _ -> [])
                unnamedCurriedCallerArgs,namedCurriedCallerArgs,Some(List.map (List.map fst) lambdaVarsAndExprs), returnTy,tpenv

            | Some (unnamedCurriedCallerArgs,namedCurriedCallerArgs) ->
                let unnamedCurriedCallerArgs = unnamedCurriedCallerArgs |> List.mapSquared (fun (x,xty,xm) -> CallerArg(xty,xm,false,x)) 
                let unnamedCurriedCallerArgs,tpenv =  TcMethodArgs cenv env tpenv unnamedCurriedCallerArgs
                unnamedCurriedCallerArgs,namedCurriedCallerArgs,None,exprTy,tpenv

        // Now check the named arguments
        let namedCurriedCallerArgs = namedCurriedCallerArgs |> List.mapSquared (fun (id,isOpt,x,xty,xm) -> CallerNamedArg(id,CallerArg(xty,xm,isOpt,x))) 
        let namedCurriedCallerArgs,tpenv =  TcMethodNamedArgs cenv env tpenv namedCurriedCallerArgs
        unnamedCurriedCallerArgs,namedCurriedCallerArgs,lambdaVars,returnTy,tpenv

    let preArgumentTypeCheckingCalledMethGroup = 
       preArgumentTypeCheckingCalledMethGroup |> List.map (fun cmeth -> (cmeth.Method, cmeth.CalledTyArgs, cmeth.AssociatedPropertyInfo, cmeth.UsesParamArrayConversion))
    
    // STEP 3. Resolve overloading 
    /// Select the called method that's the result of overload resolution
    let (CalledMeth(finalCalledMethInfo,
                    finalCalledMethInst,
                    _,
                    _,
                    argSets,
                    _,
                    assignedNamedProps,
                    finalCalledPropInfoOpt,_, 
                    attributeAssignedNamedItems,
                    unnamedCalledOptArgs,
                    unnamedCalledOutArgs) as finalCalledMeth) = 

        let mk_CalledMeth2 (minfo:MethInfo,minst,pinfoOpt,usesParamArrayConversion) = 
            let userTypeArgs = Option.otherwise tyargsOpt minst
                        
            let callerArgs = List.zip unnamedCurriedCallerArgs namedCurriedCallerArgs
            MakeCalledMeth(cenv.infoReader,checkingAttributeCall, FreshenMethInfo, m,ad,minfo,minst,userTypeArgs,pinfoOpt,objArgTys,callerArgs,usesParamArrayConversion,true)
          
        let postArgumentTypeCheckingCalledMethGroup = List.map mk_CalledMeth2 preArgumentTypeCheckingCalledMethGroup

        let callerArgCounts = (unnamedCurriedCallerArgs.Length, namedCurriedCallerArgs.Length)
        let csenv = (MakeConstraintSolverEnv cenv.css m denv)
        
        // Commit unassociated constraints prior to member overload resolution where there is ambiguity 
        // about the possible target of the call. 
        if not uniquelyResolved then 
            GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,m)
                 (//freeInTypeLeftToRight cenv.g false returnTy @
                  (unnamedCurriedCallerArgs |> List.collectSquared  (fun (CallerArg(xty,_,_,_)) -> freeInTypeLeftToRight cenv.g false xty)));

        let result, errors = 
            ResolveOverloading csenv NoTrace methodName 0 false callerArgCounts ad postArgumentTypeCheckingCalledMethGroup true (Some returnTy) 
        
        // Raise the errors from the constraint solving 
        RaiseOperationResult errors;
        match result with 
        | None -> error(InternalError("at least one error should be returned by failed method overloading",m))
        | Some res ->  res

    let assignedNamedArgs = argSets |> List.collect (fun argSet -> argSet.AssignedNamedArgs)
    let paramArrayCallerArgs = argSets |> List.collect (fun argSet -> argSet.ParamArrayCallerArgs)
    let unnamedCalledArgs = argSets |> List.collect (fun argSet -> argSet.UnnamedCalledArgs)
    let unnamedCallerArgs = argSets |> List.collect (fun argSet -> argSet.UnnamedCallerArgs)
    
    // STEP 4. Check the attributes on the method and the corresponding event/property, if any 

    finalCalledPropInfoOpt |> Option.iter (fun pinfo -> CheckPropInfoAttributes pinfo m |> CommitOperationResult) ;

    let isInstance = nonNil objArgs
    MethInfoChecks cenv.g cenv.amap isInstance tyargsOpt objArgs ad m finalCalledMethInfo;

    // Adhoc constraints on use of .NET methods
    begin 
        // Uses of Object.GetHashCode and Object.Equals imply an equality constraint on the object argument
        //
        if (isInstance && 
            finalCalledMethInfo.IsInstance &&
            typeEquiv cenv.g finalCalledMethInfo.EnclosingType cenv.g.obj_ty && 
            (finalCalledMethInfo.LogicalName = "GetHashCode" ||  finalCalledMethInfo.LogicalName = "Equals")) then 
           
            objArgs |> List.iter (fun expr -> ConstraintSolver.AddCxTypeMustSupportEquality env.DisplayEnv cenv.css m NoTrace (tyOfExpr cenv.g expr));

        // Uses of a Dictionary() constructor without an IEqualityComparer argument imply an equality constraint 
        // on the first type argument.
        if HasHeadType cenv.g cenv.g.tcref_System_Collections_Generic_Dictionary finalCalledMethInfo.EnclosingType  &&
           finalCalledMethInfo.IsConstructor &&
           not (ParamDatasOfMethInfo cenv.amap m finalCalledMethInfo finalCalledMeth.CallerTyArgs |> List.existsSquared (fun (ParamData(_,_,_,_,ty)) ->  
                HasHeadType cenv.g cenv.g.tcref_System_Collections_Generic_IEqualityComparer ty)) then 
            
            match argsOfAppTy cenv.g finalCalledMethInfo.EnclosingType with 
            | [dty; _] -> ConstraintSolver.AddCxTypeMustSupportEquality env.DisplayEnv cenv.css m NoTrace dty;
            | _ -> ()
    end;

    if (argSets |> List.existsi (fun i argSet -> argSet.UnnamedCalledArgs |> List.existsi (fun j ca -> ca.Position <> (i,j)))) then
        errorR(Deprecated(FSComp.SR.tcUnnamedArgumentsDoNotFormPrefix(),m));


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
            let v,ve = mkCompGenLocal m "objectArg" objArgTy
            (fun body -> mkCompGenLet m v objArg body), [ve]

        | _ -> 
            emptyPreBinder,objArgs

  
    // Handle optional arguments
    let optArgPreBinder,allArgs,outArgExprs,outArgTmpBinds = 

        let normalUnnamedArgs = 
          (unnamedCalledArgs,unnamedCallerArgs) ||> List.map2 (fun called caller -> AssignedCalledArg(None,called,caller)) 

        let paramArrayArgs = 
          match finalCalledMeth.ParamArrayCalledArgOpt with 
          | None -> []
          | Some paramArrayCalledArg -> 
               let paramArrayCalledArgElementType = destArrayTy cenv.g paramArrayCalledArg.Type
               let es = paramArrayCallerArgs  |> List.map (fun (CallerArg(callerArgTy,_,_,callerArgExpr)) -> mkCoerceIfNeeded cenv.g paramArrayCalledArgElementType callerArgTy callerArgExpr)
               [ AssignedCalledArg(None,paramArrayCalledArg,CallerArg(paramArrayCalledArg.Type,m,false,Expr.Op(TOp.Array,[paramArrayCalledArgElementType], es ,m))) ]

        // CLEANUP: Move all this code into some isolated file, e.g. "optional.fs"
        //
        // Handle CallerSide optional arguments. 
        //
        // CallerSide optional arguments are largely for COM interop, e.g. to PIA assemblies for Word etc.
        // As a result we follow the VB spec here. To quote from an email exchange between the C# and VB teams.
        //
        //   "1.        If the parameter is statically typed as System.Object and does not have a value, then there are two cases:
        //       a.     The parameter may have the IDispatchConstantAttribute or IUnknownConstantAttribute attribute. If this is the case, the VB compiler then create an instance of the System.Runtime.InteropServices.DispatchWrapper /System.Runtime.InteropServices.UnknownWrapper type at the call site to wrap the value Nothing/null.
        //       b.     If the parameter does not have those two attributes, we will emit Missing.Value.
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
          (emptyPreBinder,unnamedCalledOptArgs)
            ||> List.mapFold (fun wrapper (CalledArg(_,_,optArgInfo,_,_,calledArgTy) as calledArg) -> 
                  let wrapper2,expr = 

                      match optArgInfo with 
                      | NotOptional -> 
                          error(InternalError("Unexpected NotOptional",m))
                      | CallerSide dfltVal ->
                          let rec build = function 
                              | MissingValue -> 
                                  // Add an I_nop if this is an initonly field to make sure we never recognize it as an lvalue. See mkExprAddrOfExpr. 
                                  emptyPreBinder,mkAsmExpr ([ mkNormalLdsfld (fspec_Missing_Value cenv.g.ilg); AI_nop ],[],[],[calledArgTy],m)
                              | DefaultValue -> 
                                  emptyPreBinder,mkDefault(m,calledArgTy)
                              | Constant fieldInit -> 
                                  emptyPreBinder,Expr.Const(TcFieldInit m fieldInit,m,calledArgTy)  
                              | WrapperForIDispatch ->
                                  let tref = mkILNonGenericBoxedTy(mkILTyRef(cenv.g.ilg.mscorlibScopeRef,"System.Runtime.InteropServices.DispatchWrapper"))
                                  let mref = mkILCtorMethSpecForTy(tref,[cenv.g.ilg.typ_Object]).MethodRef
                                  let expr = Expr.Op(TOp.ILCall(false,false,false,false,CtorValUsedAsSuperInit,false,false,mref,[],[],[cenv.g.obj_ty]),[],[mkDefault(m,calledArgTy)],m)
                                  emptyPreBinder,expr
                              | WrapperForIUnknown ->
                                  let tref = mkILNonGenericBoxedTy(mkILTyRef(cenv.g.ilg.mscorlibScopeRef,"System.Runtime.InteropServices.UnknownWrapper"))
                                  let mref = mkILCtorMethSpecForTy(tref,[cenv.g.ilg.typ_Object]).MethodRef
                                  let expr = Expr.Op(TOp.ILCall(false,false,false,false,CtorValUsedAsSuperInit,false,false,mref,[],[],[cenv.g.obj_ty]),[],[mkDefault(m,calledArgTy)],m)
                                  emptyPreBinder,expr
                              | PassByRef (ty, dfltVal2) -> 
                                  let v,_ = mkCompGenLocal m "defaultByrefArg" ty
                                  let wrapper2,rhs = build dfltVal2 
                                  (wrapper2 >> mkCompGenLet m v rhs), mkValAddr m (mkLocalValRef v)
                          build dfltVal

                      | CalleeSide -> 
                          let calledNonOptTy = 
                              if isOptionTy cenv.g calledArgTy then 
                                  destOptionTy cenv.g calledArgTy 
                              else
                                  calledArgTy // should be unreachable
                          emptyPreBinder,mkUnionCaseExpr(mkNoneCase cenv.g,[calledNonOptTy],[],m)

                  // Combine the variable allocators (if any)
                  let wrapper = (wrapper >> wrapper2)
                  let callerArg = CallerArg(calledArgTy,m,false,expr)
                  AssignedCalledArg(None,calledArg,callerArg),wrapper)


        // Handle optional arguments
        let wrapOptionalArg (AssignedCalledArg(idOpt,(CalledArg(_,_,optArgInfo,_,_,calledArgTy) as calledArg) ,CallerArg(callerArgTy,m,isOptCallerArg,expr)) as assignedArg) = 
            match optArgInfo with 
            | NotOptional -> 
                if isOptCallerArg then errorR(Error(FSComp.SR.tcFormalArgumentIsNotOptional(),m));
                assignedArg

            | _ -> 
                let expr = 
                    match optArgInfo with 
                    | CallerSide _ -> 
                        if isOptCallerArg then 
                            mkUnionCaseFieldGetUnproven(expr,mkSomeCase cenv.g,[destOptionTy cenv.g callerArgTy],0,m) 
                        else 
                            expr
                    | CalleeSide -> 
                        if isOptCallerArg then 
                            // M(?x=bopt) when M(A) --> M(?x=Some(b.Value))
                            expr 
                        else                            
                            // M(x=b) when M(A) --> M(?x=Some(b :> A))
                            if isOptionTy cenv.g calledArgTy then 
                                let calledNonOptTy = destOptionTy cenv.g calledArgTy 
                                mkUnionCaseExpr(mkSomeCase cenv.g,[calledNonOptTy],[mkCoerceIfNeeded cenv.g calledNonOptTy callerArgTy expr],m)
                            else 
                                expr // should be unreachable 
                            
                    | _ -> failwith "Unreachable"
                AssignedCalledArg(idOpt,calledArg,CallerArg((tyOfExpr cenv.g expr),m,isOptCallerArg,expr))

        let outArgsAndExprs,outArgTmpBinds = 
            unnamedCalledOutArgs 
              |> List.map (fun (CalledArg(_,_,_,_,_,calledArgTy) as calledArg) -> 
                let outArgTy = destByrefTy cenv.g calledArgTy
                let outv,outArgExpr = Tastops.mkMutableCompGenLocal m "outArg" outArgTy // mutable! 
                let expr = mkDefault(m,outArgTy)
                let callerArg = CallerArg(calledArgTy,m,false,mkValAddr m (mkLocalValRef outv))
                (AssignedCalledArg(None,calledArg,callerArg), outArgExpr), mkCompGenBind outv expr) 
              |> List.unzip

        let outArgs, outArgExprs = List.unzip outArgsAndExprs

        let allArgs =
            List.map wrapOptionalArg normalUnnamedArgs @ 
            List.map wrapOptionalArg assignedNamedArgs @ 
            paramArrayArgs @
            optArgs @ 
            outArgs
        
        let allArgs = 
            allArgs |> List.sortBy (fun x -> x.Position)

        optArgPreBinder,allArgs,outArgExprs,outArgTmpBinds
  
    // Handle adhoc argument conversions
    let coerce (AssignedCalledArg(_,CalledArg(_,_,_,isOutArg,_,calledArgTy),CallerArg(callerArgTy,m,_,e))) = 
    
       if isByrefTy cenv.g calledArgTy && isRefCellTy cenv.g callerArgTy then 
           Expr.Op(TOp.RefAddrGet,[destRefCellTy cenv.g callerArgTy],[e],m) 

       elif isDelegateTy cenv.g calledArgTy && isFunTy cenv.g callerArgTy then 
           let minfo,delArgTys,_,_ = GetSigOfFunctionForDelegate cenv.infoReader calledArgTy m ad
           BuildNewDelegateExpr None cenv calledArgTy (minfo,delArgTys)  (e,callerArgTy) m

       // Note: out args do not need to be coerced 
       elif isOutArg then 
           e
       // Note: not all these casts are not reported in quotations 
       else 
           mkCoerceIfNeeded cenv.g calledArgTy callerArgTy e 

    // Record the resolution of the named argument for the Language Service
    allArgs |> List.iter (fun (AssignedCalledArg(idOpt,_,_)) ->
        match idOpt with 
        | None -> ()
        | Some id -> CallNameResolutionSink(id.idRange,nenv,Item.ArgName(id),ItemOccurence.Use,nenv.eDisplayEnv,ad));

    let allArgsCoerced = List.map coerce  allArgs

    // Make the call expression 
    let expr,exprty = 
        BuildMethodCall cenv env mut m isProp finalCalledMethInfo isSuperInit finalCalledMethInst objArgs allArgsCoerced
        

    // Bind "out" parameters as part of the result tuple 
    let expr,exprty = 
        if isNil outArgTmpBinds then expr,exprty
        else 
            let outArgTys = outArgExprs |> List.map (tyOfExpr cenv.g)
            let expr = if isUnitTy cenv.g exprty then mkCompGenSeq m expr  (mkTupled cenv.g  m outArgExprs outArgTys)
                       else  mkTupled cenv.g  m (expr :: outArgExprs) (exprty :: outArgTys)
            let expr = mkLetsBind m outArgTmpBinds expr
            expr, tyOfExpr cenv.g expr

    // Handle post-hoc property assignments 
    let expr = 
        if isNil assignedNamedProps then expr else 
        // This holds the result of the call 
        let objv,objExpr = Tastops.mkMutableCompGenLocal m "returnVal" exprty // mutable in case it's a struct 
        // This expression  mutates the properties on the result of the call
        let propSetExpr = 
            (mkUnit cenv.g m, assignedNamedProps) ||> List.fold (fun acc (AssignedItemSetter(id,setter,CallerArg(callerArgTy,m,isOptCallerArg,argExpr))) ->
                    if isOptCallerArg then error(Error(FSComp.SR.tcInvalidOptionalAssignmentToPropertyOrField(),m));
                    
                    let action = 
                        match setter with 
                        | AssignedPropSetter(_,pminfo,pminst) -> 
                            MethInfoChecks cenv.g cenv.amap true None [objExpr] ad m pminfo;
                            let calledArgTy = List.head (List.head (ParamTypesOfMethInfo cenv.amap m pminfo pminst))
                            let argExpr = mkCoerceExpr(argExpr,calledArgTy,m,callerArgTy)
                            let mut = (if isStructTy cenv.g (tyOfExpr cenv.g objExpr) then DefinitelyMutates else PossiblyMutates)
                            BuildMethodCall cenv env mut m true pminfo NormalValUse pminst [objExpr] [argExpr] |> fst 

                        | AssignedIlFieldSetter(finfo) ->
                            // Get or set instance IL field 
                            ILFieldInstanceChecks  cenv.g cenv.amap ad m finfo;
                            BuildILFieldSet cenv.g m objExpr finfo argExpr 
                        
                        | AssignedRecdFieldSetter(rfinfo) ->
                            RecdFieldInstanceChecks cenv.g ad m rfinfo; 
                            let _,ftinst,_ = FreshenPossibleForallTy cenv.g m TyparFlexible (rfinfo.FieldType)
                            CheckRecdFieldMutation m denv rfinfo ftinst;
                            BuildRecdFieldSet cenv.g m objExpr rfinfo argExpr 

                    // Record the resolution for the Language Service
                    CallNameResolutionSink(id.idRange,nenv,Item.PropName(id),ItemOccurence.Use,nenv.eDisplayEnv,ad);

                    mkCompGenSeq m acc action)

        // now put them together 
        let expr = mkCompGenLet m objv expr  (mkCompGenSeq m propSetExpr objExpr)
        expr

    // Build the lambda expression if any 
    let expr = 
        match lambdaVars with 
        | None -> expr
        | Some curriedLambdaVars -> 
            let mkLambda vs expr = 
                match vs with 
                | [] -> mkUnitDelayLambda cenv.g m expr 
                | _ -> mkMultiLambda m vs (expr, tyOfExpr cenv.g expr)
            List.foldBack mkLambda curriedLambdaVars expr

    let expr, tpenv = 
        match unnamedDelayedCallerArgExprOpt with 
        | Some synArgExpr -> 
            match lambdaVars with 
            | Some [lambdaVars] -> 
                let argExpr,tpenv = TcExpr cenv (mkTupledVarsTy cenv.g lambdaVars) env tpenv synArgExpr 
                mkApps cenv.g ((expr,tyOfExpr cenv.g expr),[],[argExpr],m), tpenv
            | _ -> 
                error(InternalError("unreachable - expected some lambda vars for a tuple mismatch",m))
        | None -> 
            expr, tpenv

    // Apply the PreBinders, if any 
    let expr = optArgPreBinder expr
    let expr = objArgPreBinder expr
    
    (expr,attributeAssignedNamedItems,delayed),tpenv
            
and TcMethodArgs cenv env tpenv args =  
    List.mapfoldSquared (TcMethodArg cenv env) tpenv args

and TcMethodArg  cenv env tpenv (CallerArg(ty,m,isOpt,e)) = 
    let e',tpenv = TcExpr cenv ty env tpenv e 
    CallerArg(ty,m,isOpt,e'),tpenv

and TcMethodNamedArgs cenv env tpenv args =  
    List.mapfoldSquared (TcMethodNamedArg cenv env) tpenv args

and TcMethodNamedArg  cenv env tpenv (CallerNamedArg(id,arg)) = 
    let arg',tpenv = TcMethodArg cenv env tpenv arg 
    CallerNamedArg(id,arg'),tpenv

/// Typecheck "new Delegate(fun x y z -> ...)" constructs
and TcNewDelegateThen cenv overallTy env tpenv m delty arg atomicFlag delayed =
    let ad = AccessRightsOfEnv env
    UnifyTypes cenv env m overallTy delty;
    let minfo,delArgTys,_,fty = GetSigOfFunctionForDelegate cenv.infoReader delty m ad
    // We pass isInstance = true here because we're checking the rights to access the "Invoke" method
    MethInfoChecks cenv.g cenv.amap true None [] (AccessRightsOfEnv env) m minfo;
    let args = GetMethodArgs arg
    match args with 
    | [farg],[] -> 
        let m = arg.Range
        let (CallerArg(_,_,_,farg')),tpenv =  TcMethodArg cenv env tpenv (CallerArg(fty,m,false,farg))
        let expr = BuildNewDelegateExpr None cenv delty (minfo,delArgTys) (farg',fty) m 
        PropagateThenTcDelayed cenv overallTy env tpenv m (MakeApplicableExprNoFlex cenv expr) delty atomicFlag delayed  
    | _ ->  error(Error(FSComp.SR.tcDelegateConstructorMustBePassed(),m))



and bind_letrec (binds:Bindings) m e = 
    if FlatList.isEmpty binds then 
        e 
    else 
        Expr.LetRec (binds,e,m,NewFreeVarsCache()) 

/// Process a sequence of iterated lets "let ... in let ... in ..." in a tail recursive way 
/// This avoids stack overflow on really larger "let" and "letrec" lists
and TcLinearLetExprs bodyChecker cenv env overallTy builder tpenv (processUseBindings,isRec,isUse,binds,body,m) =
    assert (not isUse || processUseBindings)

    if isRec then 
        // TcLinearLetExprs processes at most one recursive binding
        let binds = List.map (fun x -> RecBindingDefn(ExprContainerInfo,NoNewSlots,ExpressionBinding,x)) binds
        if isUse then errorR(Error(FSComp.SR.tcBindingCannotBeUseAndRec(),m));
        let binds,envinner,tpenv = TcLetrec ErrorOnOverrides cenv env tpenv (binds,m,m)
        let bodyExpr,tpenv = bodyChecker overallTy envinner tpenv body 
        let bodyExpr = bind_letrec (FlatList.ofList binds) m bodyExpr
        fst (builder (bodyExpr,overallTy)),tpenv
    else 
        // TcLinearLetExprs processes multiple 'let' bindings in a tail recursive way
        // We process one binding, then look for additional linear bindings and accumulate the builder continuation.
        // Don't processes 'use' bindings (e.g. in sequence expressions) unless directed to.
        let mkf,envinner,tpenv =
          TcLetBinding cenv isUse env ExprContainerInfo ExpressionBinding tpenv (binds,m,body.Range)
        let builder' x = builder (mkf x)
        match body with 
        | SynExpr.LetOrUse (isRec',isUse',binds',bodyExpr,m') when (not isUse' || processUseBindings) ->
            TcLinearLetExprs bodyChecker cenv envinner overallTy builder' tpenv (processUseBindings,isRec',isUse',binds',bodyExpr,m')
        | _ -> 
            let bodyExpr,tpenv = bodyChecker overallTy envinner tpenv body 
            fst (builder' (bodyExpr,overallTy)),tpenv

/// Typecheck and compile pattern-matching constructs
and TcAndPatternCompileMatchClauses exprm matchm actionOnFailure cenv inputTy resultTy env tpenv clauses =
    let tclauses, tpenv = TcMatchClauses cenv inputTy resultTy env tpenv clauses
    let v,expr = CompilePatternForMatchClauses cenv env exprm matchm true actionOnFailure inputTy resultTy tclauses
    v,expr,tpenv

and TcMatchPattern cenv inputTy env tpenv (pat:SynPat,optWhenExpr) =
    let m = pat.Range
    let patf',(tpenv,names,_) = TcPat WarnOnUpperCase cenv env None (OptionalInline,permitInferTypars,noArgOrRetAttribs,false,None,false) (tpenv,Map.empty,Set.empty) inputTy pat
    let envinner,values,vspecMap = MakeAndPublishSimpleVals cenv env m names
    let optWhenExpr',tpenv = Option.mapFold (TcExpr cenv cenv.g.bool_ty envinner) tpenv optWhenExpr
    patf' (TcPatPhase2Input values),optWhenExpr',FlatList.ofList (NameMap.range vspecMap),envinner,tpenv

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
            errorR(Error(FSComp.SR.tcStaticOptimizationConditionalsOnlyForFSharpLibrary(),m));
        let ty',tpenv = TcType cenv NewTyparsOK CheckCxs env tpenv ty
        let tp',tpenv = TcTypar cenv env NewTyparsOK tpenv tp
        TTyconEqualsTycon(mkTyparTy tp', ty'),tpenv
    | WhenTyparIsStruct(tp,m) ->
        if not cenv.g.compilingFslib then 
            errorR(Error(FSComp.SR.tcStaticOptimizationConditionalsOnlyForFSharpLibrary(),m));
        let tp',tpenv = TcTypar cenv env NewTyparsOK tpenv tp
        TTyconIsStruct(mkTyparTy tp'),tpenv
    | WhenInlined(m) ->
        if not cenv.g.compilingFslib then 
            errorR(Error(FSComp.SR.tcStaticOptimizationConditionalsOnlyForFSharpLibrary(),m));
        TTyconEqualsTycon(cenv.g.obj_ty, cenv.g.obj_ty),tpenv

/// Binding checking code, for all bindings including let bindings, let-rec bindings, member bindings and object-expression bindings and 
and TcNormalizedBinding declKind cenv env tpenv overallTy safeThisValOpt safeInitInfo (enclosingDeclaredTypars,(ExplicitTyparInfo(_,declaredTypars,_) as flex)) bind =
    let envinner = AddDeclaredTypars NoCheckForDuplicateTypars (enclosingDeclaredTypars@declaredTypars) env

    match bind with 

    | NormalizedBinding(vis,bkind,pseudo,isMutable,attrs,doc,_,valSynData,pat,NormalizedBindingRhs(spatsL,rtyOpt,rhsExpr),bindingRange,spBind) ->
        
        let (SynValData(memberFlagsOpt,valSynInfo,_)) = valSynData 

        let attrTgt = DeclKind.AllowedAttribTargets memberFlagsOpt declKind 

        // Check the attributes of the binding, parameters or return value
        let TcAttrs tgt attrs = 
            let attrs = TcAttributes cenv envinner tgt attrs 
            if attrTgt = enum 0 && nonNil attrs then 
                errorR(Error(FSComp.SR.tcAttributesAreNotPermittedOnLetBindings(),bindingRange))
            attrs
            
        let valAttribs = TcAttrs attrTgt attrs
        let isVolatile = HasAttrib cenv.g cenv.g.attrib_VolatileFieldAttribute valAttribs
        
        let inlineFlag = ComputeInlineFlag memberFlagsOpt pseudo isMutable valAttribs
        let argAttribs = 
            spatsL |> List.map (SynInfo.InferArgSynInfoFromSimplePats >> List.map (SynInfo.AttribsOfArgData >> TcAttrs AttributeTargets.Parameter))
        let retAttribs = 
            match rtyOpt with 
            | Some (SynBindingReturnInfo(_,_,retAttrs)) -> TcAttrs AttributeTargets.ReturnValue retAttrs 
            | None -> [] 

        let argAndRetAttribs = ArgAndRetAttribs(argAttribs, retAttribs)

        if HasAttrib cenv.g cenv.g.attrib_DefaultValueAttribute valAttribs then 
            errorR(Error(FSComp.SR.tcDefaultValueAttributeRequiresVal(),bindingRange));
        
        let isThreadStatic = isThreadOrContextStatic cenv.g valAttribs
        if isThreadStatic then errorR(DeprecatedThreadStaticBindingWarning(bindingRange));

        if isVolatile then 
            if declKind <> ClassLetBinding then 
                errorR(Error(FSComp.SR.tcVolatileOnlyOnClassLetBindings(),bindingRange));
            if (not isMutable || isThreadStatic) then 
                errorR(Error(FSComp.SR.tcVolatileFieldsMustBeMutable(),bindingRange));

        if HasAttrib cenv.g cenv.g.attrib_ConditionalAttribute valAttribs && isNone(memberFlagsOpt) then 
            errorR(Error(FSComp.SR.tcConditionalAttributeRequiresMembers(),bindingRange));

        if HasAttrib cenv.g cenv.g.attrib_EntryPointAttribute valAttribs then 
            if isSome(memberFlagsOpt) then 
                errorR(Error(FSComp.SR.tcEntryPointAttributeRequiresFunctionInModule(),bindingRange))
            else 
                UnifyTypes cenv env bindingRange overallTy (mkArrayType cenv.g cenv.g.string_ty --> cenv.g.int_ty)

        if isMutable && pseudo then errorR(Error(FSComp.SR.tcMutableValuesCannotBeInline(),bindingRange));
        if isMutable && nonNil declaredTypars then errorR(Error(FSComp.SR.tcMutableValuesMayNotHaveGenericParameters(),bindingRange));
        let flex = if isMutable then dontInferTypars else flex
        if isMutable && nonNil spatsL then errorR(Error(FSComp.SR.tcMutableValuesSyntax(),bindingRange));
        let pseudo = 
            if pseudo && isNil spatsL && isNil declaredTypars then 
                errorR(Error(FSComp.SR.tcOnlyFunctionsCanBeInline(),bindingRange));
                false
            else 
                pseudo 

        let compgen = false
        
        // Use the syntactic arity if we're defining a function 
        let partialValReprInfo = TranslateTopValSynInfo bindingRange (TcAttributes cenv env) valSynInfo

        // Check the pattern of the l.h.s. of the binding 
        let tcPatPhase2,(tpenv,nameToPrelimValSchemeMap,_) = 
            TcPat AllIdsOK cenv envinner (Some(partialValReprInfo)) (inlineFlag,flex,argAndRetAttribs,isMutable,vis,compgen) (tpenv,NameMap.empty,Set.empty) overallTy pat
        

        // Add active pattern result names to the environment 
        let apinfoOpt = 
            match NameMap.range nameToPrelimValSchemeMap with 
            | [PrelimValScheme1(id,_,ty,_,_,_,_,_,_,_,_) ] -> 
                match ActivePatternInfoOfValName id.idText  with 
                | Some apinfo ->  Some (apinfo,ty, id.idRange)
                | None -> None
            | _ -> None

        // Add active pattern result names to the environment 
        let envinner = 
            match apinfoOpt with 
            | Some (apinfo,ty,m) ->
                if isSome memberFlagsOpt || (not apinfo.IsTotal && apinfo.ActiveTags.Length > 1) then 
                    error(Error(FSComp.SR.tcInvalidActivePatternName(),bindingRange));

                ModifyNameResEnv (fun nenv -> AddActivePatternResultTagsToNameEnv apinfo nenv ty m) envinner 
            | None -> 
                envinner
        
        // Now tc the r.h.s. 
        // If binding a ctor then set the counter that permits us to write ctor expressions on the r.h.s. 
        let isCtor = (match memberFlagsOpt with Some(memberFlags) -> memberFlags.MemberKind = MemberKind.Constructor | _ -> false)
        let tc = 
            if isCtor then TcExprThatIsCtorBody (safeThisValOpt, safeInitInfo)
            else TcExprThatCantBeCtorBody

        let rhsExpr',tpenv = tc cenv overallTy envinner tpenv rhsExpr

        if bkind = StandaloneExpression && not cenv.isScript then 
            UnifyUnitType cenv env.DisplayEnv bindingRange overallTy (Some rhsExpr') |> ignore<bool>;

        // Assert the return type of an active pattern
        match apinfoOpt with 
        | Some (apinfo,ty,_) ->
            let activePatResTys = NewInferenceTypes apinfo.ActiveTags
            let _,rty = stripFunTy cenv.g ty
            UnifyTypes cenv env bindingRange (apinfo.ResultType cenv.g rhsExpr.Range activePatResTys) rty;
        | None -> 
            ()

        // Check other attributes
        let hasLiteralAttr,konst = TcLiteral cenv overallTy env tpenv (valAttribs,rhsExpr)
        if hasLiteralAttr && isThreadStatic then 
            errorR(Error(FSComp.SR.tcIllegalAttributesForLiteral(),bindingRange));
        if hasLiteralAttr && isMutable then 
            errorR(Error(FSComp.SR.tcLiteralCannotBeMutable(),bindingRange));
        if hasLiteralAttr && pseudo then 
            errorR(Error(FSComp.SR.tcLiteralCannotBeInline(),bindingRange));
        if hasLiteralAttr && nonNil declaredTypars then 
            errorR(Error(FSComp.SR.tcLiteralCannotHaveGenericParameters(),bindingRange));

        CheckedBindingInfo(inlineFlag,true,valAttribs,doc,tcPatPhase2,flex,nameToPrelimValSchemeMap,rhsExpr',argAndRetAttribs,overallTy,bindingRange,spBind,compgen,konst),tpenv

and TcLiteral cenv overallTy env tpenv (attrs',e) = 
    let hasLiteralAttr = HasAttrib cenv.g cenv.g.attrib_LiteralAttribute attrs'
    if not hasLiteralAttr then  hasLiteralAttr,None else 
        let expr',_ = TcExpr cenv overallTy env tpenv e
        let rec eval e = 
            match stripExpr e with 
            | Expr.Const(c,_,_) -> c
            | _ -> 
                errorR(Error(FSComp.SR.tcInvalidConstantExpression(),e.Range));
                Const.Unit
        hasLiteralAttr,Some(eval expr') 
    
and TcBindingTyparDecls alwaysRigid cenv env m tpenv (SynValTyparDecls(synTypars,infer,synTyparConstraints)) = 
    let declaredTypars = TcTyparDecls cenv env synTypars
    let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTypars env
    let tpenv = TcTyparConstraints cenv NoNewTypars CheckCxs envinner tpenv synTyparConstraints

    let rigidCopyOfDeclaredTypars = 
        if alwaysRigid then 
            declaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m);
            declaredTypars
        else
            let rigidCopyOfDeclaredTypars = copyTypars declaredTypars
            // The type parameters used to check rigidity after inference are marked rigid straight away
            rigidCopyOfDeclaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m);
            // The type parameters using during inference will be marked rigid after inference
            declaredTypars |> List.iter (fun tp -> tp.SetRigidity TyparWillBeRigid);
            rigidCopyOfDeclaredTypars
            
    ExplicitTyparInfo(rigidCopyOfDeclaredTypars,declaredTypars,infer) , tpenv

and TcNonrecBindingTyparDecls cenv env tpenv bind = 
    let (NormalizedBinding(_,_,_,_,_,_,synTyparDecls,_,_,_,m,_)) = bind
    TcBindingTyparDecls true cenv env m tpenv synTyparDecls

and TcNonRecursiveBinding declKind cenv env tpenv ty b =
    let b = BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env b
    let flex, tpenv = TcNonrecBindingTyparDecls cenv env tpenv b
    TcNormalizedBinding declKind cenv env tpenv ty None NoSafeInitInfo ([],flex) b 

//-------------------------------------------------------------------------
// TcAttribute*
//------------------------------------------------------------------------

and TcAttribute cenv (env: TcEnv) attrTgt (attr: SynAttribute)  =
    let tycon = attr.TypeName
    let arg = attr.ArgExpr
    let targetIndicator = attr.Target
    let isAppliedToGetterOrSetter = attr.AppliesToGetterAndSetter
    let m = attr.Range
    let (typath,tyid) = List.frontAndBack tycon
    let tpenv = emptyUnscopedTyparEnv

    // if we're checking an attribute that was applied directly to a getter or a setter, then
    // what we're really checking against is a method, not a property
    let attrTgt = if isAppliedToGetterOrSetter then ((attrTgt ^^^ AttributeTargets.Property) ||| AttributeTargets.Method) else attrTgt
    let ty,tpenv =  
        let try1 n = 
            let tyid = mkSynId tyid.idRange n
            let tycon = (typath @ [tyid])
            let ad = AccessRightsOfEnv env
            match ResolveTypeLongIdent cenv.nameResolver ItemOccurence.Use OpenQualified GenerateEstTypeFlag.No env.eNameResEnv ad tycon 0 with
            | Exception err -> raze(err)
            | _ ->  success(TcTypeAndRecover cenv NoNewTypars CheckCxs env tpenv (SynType.App(SynType.LongIdent(tycon,m),[],false,m)) )in
        ForceRaise ((try1 (tyid.idText^"Attribute")) |> Outcome.otherwise (fun () -> (try1 tyid.idText)))

    let ad = AccessRightsOfEnv env

    if not (IsTypeAccessible cenv.g ad ty) then  errorR(Error(FSComp.SR.tcTypeIsInaccessible(),m));

    let tcref = tcrefOfAppTy cenv.g ty

    let conditionalCallDefine = 
        TyconRefTryBindAttrib cenv.g cenv.g.attrib_ConditionalAttribute tcref 
                 (function ([ILAttribElem.String (Some(msg)) ],_) -> Some msg | _ -> None)
                 (function (Attrib(_,_,[ AttribStringArg(msg) ],_,_,_))  -> Some(msg) | _ -> None)

    match conditionalCallDefine with 
    | Some(d) when not (List.mem d cenv.conditionalDefines) -> 
        []
    | _ ->

        let validOn,_inherited = 
            let validOnDefault = 0x7fff
            let inheritedDefault = true
            if tcref.IsILTycon then 
                let tdef = tcref.ILTyconRawMetadata
                let tref = cenv.g.attrib_AttributeUsageAttribute.TypeRef
                
                match ILThingDecodeILAttrib cenv.g tref (Some(tref.Scope)) tdef.CustomAttrs with 
                | Some ([ILAttribElem.Int32 validOn ],named) -> 
                    let inherited = 
                      (List.tryPick (function ("Inherited",_,_,ILAttribElem.Bool res) -> Some res | _ -> None) named) 
                      +? (fun () -> inheritedDefault)
                    (validOn, inherited)
                | Some ([ILAttribElem.Int32 validOn; ILAttribElem.Bool _allowMultiple; ILAttribElem.Bool inherited ],_) -> 
                    (validOn, inherited)
                | _ -> 
                    (validOnDefault, inheritedDefault)
            else
                match (TryFindAttrib cenv.g cenv.g.attrib_AttributeUsageAttribute tcref.Attribs) with
                | Some(Attrib(_,_,[ AttribInt32Arg(validOn) ],_,_,_)) ->
                    (validOn, inheritedDefault)
                | Some(Attrib(_,_,[ AttribInt32Arg(validOn);
                                    AttribBoolArg(_allowMultiple);
                                    AttribBoolArg(inherited)],_,_,_)) ->
                    (validOn, inherited)
                | Some _  ->
                    warning(Error(FSComp.SR.tcUnexpectedConditionInImportedAssembly(),m))
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
                errorR(Error(FSComp.SR.tcUnrecognizedAttributeTarget(),id.idRange)); 
                possibleTgts
            | _ -> possibleTgts
        let constrainedTgts = possibleTgts &&& directedTgts
        if constrainedTgts = enum 0 then 
            if (directedTgts = AttributeTargets.Assembly || directedTgts = AttributeTargets.Module) then 
                error(Error(FSComp.SR.tcAttributeIsNotValidForLanguageElementUseDo(),m))
            else
                error(Error(FSComp.SR.tcAttributeIsNotValidForLanguageElement(),m));

        let item, _ = ForceRaise (ResolveObjectConstructor cenv.nameResolver env.DisplayEnv m ad ty)
        let attrib = 
            match item with 
            | Item.CtorGroup(methodName,minfos) ->
                let meths = minfos |> List.map (fun minfo -> minfo,None) 

                let (expr,namedCallerArgs,_),_ = 
                  TcMethodApplication true cenv env tpenv None [] m methodName ad PossiblyMutates false meths NormalValUse [arg] (NewInferenceType ())  []

                UnifyTypes cenv env m ty (tyOfExpr cenv.g expr);
                
                let mkAttribExpr e = 
                    AttribExpr(e,EvalAttribArg cenv.g e)

                let namedAttribArgMap = 
                  namedCallerArgs |> List.map (fun (CallerNamedArg(id,CallerArg(argtyv,m,isOpt,expr))) ->
                    if isOpt then error(Error(FSComp.SR.tcOptionalArgumentsCannotBeUsedInCustomAttribute(),m));
                    let m = expr.Range
                    let nm,isProp,argty = 
                      let item, _ = ResolveLongIdentInType cenv.nameResolver (env.NameEnv) Nameres.LookupKind.Expr m ad [id] IgnoreOverrides DefaultTypeNameResInfo ty
                      let nenv = env.NameEnv 
                      CallNameResolutionSink(id.idRange,nenv,Item.PropName(id),ItemOccurence.Use,nenv.eDisplayEnv,ad);
                      match item with   
                      | Item.Property (_,[pinfo]) -> 
                          if not pinfo.HasSetter then 
                            errorR(Error(FSComp.SR.tcPropertyCannotBeSet0(),m));
                          id.idText,true, pinfo.PropertyType(cenv.amap,m) 
                      | Item.ILField finfo -> 
                          CheckILFieldInfoAccessible cenv.g cenv.amap m ad finfo;
                          CheckILFieldAttributes cenv.g finfo m;
                          id.idText,false, finfo.FieldType(cenv.amap, m)
                      | Item.RecdField rfinfo when not rfinfo.IsStatic -> 
                          CheckRecdFieldInfoAttributes cenv.g rfinfo m  |> CommitOperationResult;        
                          CheckRecdFieldInfoAccessible m ad rfinfo;
                          // This uses the F# backend name mangling of fields.... 
                          let nm =  ComputeFieldName rfinfo.Tycon rfinfo.RecdField
                          nm,false,rfinfo.FieldType
                      |  _ -> 
                          errorR(Error(FSComp.SR.tcPropertyOrFieldNotFoundInAttribute(),m)); 
                          id.idText,false,cenv.g.unit_ty

                    AddCxTypeMustSubsumeType env.DisplayEnv cenv.css m NoTrace argty argtyv;

                    AttribNamedArg(nm,argty,isProp,mkAttribExpr expr))

                //if tyconRefEq cenv.g tcref cenv.g.

                match expr with 
                | Expr.Op(TOp.ILCall(_virt,_protect,valu,_,_,_,_,mref,[],[],_rtys),[],args,m) -> 
                    if valu then error (Error(FSComp.SR.tcCustomAttributeMustBeReferenceType(),m));
                    if args.Length <> mref.ArgTypes.Length then error (Error(FSComp.SR.tcCustomAttributeArgumentMismatch(),m));
                    let args = args |> List.map mkAttribExpr
                    Attrib(tcref,ILAttrib(mref),args,namedAttribArgMap,isAppliedToGetterOrSetter,m)

                | Expr.App(Expr.Val(vref,_,_),_,_,args,_) -> 
                    let try_dest_unit_or_tuple = function Expr.Const(Const.Unit,_,_) -> [] | expr -> tryDestTuple expr
                    let args = args |> List.collect (try_dest_unit_or_tuple)  |> List.map mkAttribExpr
                    Attrib(tcref,FSAttrib(vref),args,namedAttribArgMap,isAppliedToGetterOrSetter,m)

                | _ -> 
                    error (Error(FSComp.SR.tcCustomAttributeMustInvokeConstructor(),m))

            | _ -> 
                error(Error(FSComp.SR.tcAttributeExpressionsMustBeConstructorCalls(),m))

        [ (constrainedTgts, attrib) ]

and TcAttributesWithPossibleTargets cenv env attrTgt attrs = 

    attrs |> List.collect (fun attr -> 
        try 
            let attribs = TcAttribute cenv env attrTgt attr
            
            // This is where we place any checks that completely exclude the use of some particular 
            // attributes from F#.
            let attribs' = (List.map snd attribs)
            if HasAttrib cenv.g cenv.g.attrib_TypeForwardedToAttribute attribs' ||
               HasAttrib cenv.g cenv.g.attrib_CompilationArgumentCountsAttribute attribs' ||
               HasAttrib cenv.g cenv.g.attrib_CompilationMappingAttribute attribs' then 
                errorR(Error(FSComp.SR.tcUnsupportedAttribute(),attr.Range));
            attribs
        with e -> 
            errorRecovery e attr.Range; 
            []) 

and TcAttributes cenv env attrTgt attrs = 
    TcAttributesWithPossibleTargets cenv env attrTgt attrs |> List.map snd

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
            let (CheckedBindingInfo(_,_,_,_,_,flex,_,_,_,tauTy,_,_,_,_)) = tbinfo
            let (ExplicitTyparInfo(_,declaredTypars,_)) = flex
            let maxInferredTypars = (freeInTypeLeftToRight cenv.g false tauTy)
            declaredTypars @ maxInferredTypars));

    let lazyFreeInEnv = lazy (GeneralizationHelpers.ComputeUngeneralizableTypars env)

    // Generalize the bindings...
    (((fun x -> x), env, tpenv), binds') ||> List.fold (fun (mkf_sofar,env,tpenv) tbinfo -> 
        let (CheckedBindingInfo(inlineFlag,immut,attrs,doc,tcPatPhase2,flex,nameToPrelimValSchemeMap,rhsExpr,_,tauTy,m,spBind,_,konst)) = tbinfo
        let enclosingDeclaredTypars  = []
        let (ExplicitTyparInfo(_,declaredTypars,canInferTypars)) = flex
        let allDeclaredTypars  =  enclosingDeclaredTypars @ declaredTypars
        let generalizedTypars,prelimValSchemes2 = 
            let canInferTypars = GeneralizationHelpers. ComputeCanInferTypars(containerInfo.ParentRef,canInferTypars,None,declaredTypars,m)

            let maxInferredTypars = (freeInTypeLeftToRight cenv.g false tauTy)

            let generalizedTypars = 
                if isNil maxInferredTypars && isNil allDeclaredTypars then 
                   [] 
                else 
                   let freeInEnv = lazyFreeInEnv.Force()
                   GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv, m, immut, freeInEnv, canInferTypars, GeneralizationHelpers.CanGeneralizeConstrainedTyparsForDecl(declKind), inlineFlag, Some rhsExpr, allDeclaredTypars, maxInferredTypars,tauTy,false)

            let prelimValSchemes2 = GeneralizeVals cenv denv enclosingDeclaredTypars  generalizedTypars nameToPrelimValSchemeMap

            generalizedTypars,prelimValSchemes2

        let tpenv = HideUnscopedTypars generalizedTypars tpenv
        let valSchemes = NameMap.map (UseCombinedArity cenv.g declKind rhsExpr) prelimValSchemes2
        let values = MakeAndPublishVals cenv env (altActualParent,false,declKind,ValNotInRecScope,valSchemes,attrs,doc,konst)
        let pat' = tcPatPhase2 (TcPatPhase2Input values)
        let prelimRecValues = NameMap.map fst values
        
        // Now bind the r.h.s. to the l.h.s. 
        let rhse = mkTypeLambda m generalizedTypars (rhsExpr,tauTy)

        match pat' with 
        // Don't introduce temporary or 'let' for 'match against wild' or 'match against unit' 

        | (TPat_wild _ | TPat_const (Const.Unit,_)) when not isUse && isNil generalizedTypars ->
            let mk_seq_bind (tm,tmty) = (mkSeq SequencePointsAtSeq m rhse tm, tmty)
            (mk_seq_bind << mkf_sofar,env,tpenv)
            
        | _ -> 

        // nice: don't introduce awful temporary for r.h.s. in the 99% case where we know what we're binding it to 
        let tmp,_,pat'' = 
                match pat' with 
                // nice: don't introduce awful temporary for r.h.s. in the 99% case where we know what we're binding it to 
                | TPat_as (pat1,PBind(v,TypeScheme(generalizedTypars',_)),_) 
                    when List.lengthsEqAndForall2 typarRefEq generalizedTypars generalizedTypars' -> 
                      v,exprForVal v.Range v, pat1

                | _ when mustinline(inlineFlag)  -> error(Error(FSComp.SR.tcInvalidInlineSpecification(),m))

                | _ -> 
                    let tmp,tmpe = Tastops.mkCompGenLocal m "patternInput" (generalizedTypars +-> tauTy)
                    if isUse then 
                        errorR(Error(FSComp.SR.tcInvalidUseBinding(),m));
                    
                    // This assignement forces representation as module value, to maintain the invariant from the 
                    // type checker that anything related to binding module-level values is marked with an 
                    // val_repr_info, val_actual_parent and is_topbind
                    if (DeclKind.MustHaveArity declKind) then 
                        AdjustValToTopVal tmp altActualParent (InferArityOfExprBinding cenv.g tmp rhse);
                    tmp,tmpe,pat'
        let mk_rhs_bind (tm,tmty) = (mkLet spBind m tmp rhse tm),tmty
        let allValsDefinedByPattern = (NameMap.range prelimRecValues |> FlatList.ofList)
        let mk_pat_bind (tm,tmty) =
            let valsDefinedByMatching = FlatListSet.remove valEq tmp allValsDefinedByPattern
            let matchx = CompilePatternForMatch cenv env m m true ThrowIncompleteMatchException (tmp,generalizedTypars) [TClause(pat'',None,TTarget(valsDefinedByMatching,tm,SuppressSequencePointAtTarget),m)] tmty
            let matchx = if (DeclKind.ConvertToLinearBindings declKind) then LinearizeTopMatch cenv.g altActualParent matchx else matchx
            matchx,tmty

        let mk_cleanup (tm,tmty) =
            if isUse then 
                (allValsDefinedByPattern,(tm,tmty)) ||> FlatList.foldBack (fun v (tm,tmty) ->
                    AddCxTypeMustSubsumeType denv cenv.css v.Range NoTrace cenv.g.system_IDisposable_typ v.Type;
                    let cleanupE = BuildDisposableCleanup cenv env m v
                    mkTryFinally cenv.g (tm,cleanupE,m,tmty,SequencePointInBodyOfTry,NoSequencePointAtFinally),tmty)
            else (tm,tmty) 
                
        ((mk_rhs_bind << mk_pat_bind << mk_cleanup << mkf_sofar),
         AddLocalValMap scopem prelimRecValues env,
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
    assert(DeclKind.ConvertToLinearBindings declKind);
    let mkf,env,tpenv = TcLetBinding cenv false env containerInfo declKind tpenv (binds,bindsm,scopem)
    let unite = mkUnit cenv.g bindsm
    let expr,_ = mkf (unite,cenv.g.unit_ty)
    let rec stripLets acc = function
        | Expr.Let (bind,body,m,_)      ->  stripLets (TMDefLet(bind,m) :: acc) body
        | Expr.Seq (e1,e2,NormalSeq,_,m)      ->  stripLets (TMDefDo(e1,m) :: acc) e2
        | Expr.Const (Const.Unit,_,_) -> List.rev acc
        | _ -> failwith "TcLetBindings: let sequence is non linear. Maybe a LHS pattern was not linearised?"
    let binds = stripLets [] expr
    binds,env,tpenv

and CheckMemberFlags _g optIntfSlotTy newslotsOK overridesOK memberFlags m = 
    if newslotsOK = NoNewSlots && memberFlags.IsDispatchSlot then 
      errorR(Error(FSComp.SR.tcAbstractMembersIllegalInAugmentation(),m));
    if overridesOK = WarnOnOverrides && memberFlags.IsOverrideOrExplicitImpl && isNone optIntfSlotTy then 
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
            let retInfoTy,_ = TcTypeAndRecover cenv NewTyparsOK CheckCxs env tpenv retInfoTy
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
        ignore (TcSimplePats cenv optArgsOK CheckCxs domainTy env (tpenv,Map.empty,Set.empty) pushedPat);
        ApplyTypesFromArgumentPatterns (cenv, env, optArgsOK, resultTy, m, tpenv, NormalizedBindingRhs (morePushedPats, retInfoOpt, e), memberFlagsOpt)


/// Do the type annotations give the full and complete generic type? If so, enable generic recursion 
and ComputeIsComplete enclosingDeclaredTypars declaredTypars ty = 
    Zset.isEmpty (List.fold (fun acc v -> Zset.remove v acc) 
                                  (freeInType CollectAllNoCaching ty).FreeTypars 
                                  (enclosingDeclaredTypars@declaredTypars)) 


/// Determine if a uniquely-identified-abstract-slot exists for an override member (or interface member implementation) based on the information available 
/// at the syntactic definition of the member (i.e. prior to type inference). If so, we know the expected signature of the override, and the full slotsig 
/// it implements. Apply the inferred slotsig. 
and ApplyAbstractSlotInference cenv envinner (bindingTy,m,synTyparDecls,declaredTypars,memberId,tcrefObjTy,renaming,_objTy,optIntfSlotTy,valSynData,memberFlags,attribs) = 

    let ad = AccessRightsOfEnv envinner
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
         
        let makeUniqueBySig meths = meths |> ListSet.setify (MethInfosEquivByNameAndSig EraseNone cenv.g cenv.amap m)
        match memberFlags.MemberKind with 
        | MemberKind.Member -> 
             let dispatchSlots,dispatchSlotsArityMatch = 
                 GetAbstractMethInfosForSynMethodDecl(cenv.infoReader,ad,memberId,m,typToSearchForAbstractMembers,valSynData)

             let uniqueAbstractMethSigs = 
                 match dispatchSlots with 
                 | [] -> 
                     errorR(Error(FSComp.SR.tcNoMemberFoundForOverride(),memberId.idRange));
                     []

                 | _ -> 
                     match dispatchSlotsArityMatch with 
                     | meths when meths |> makeUniqueBySig |> List.length = 1 -> 
                         meths
                     | [] -> 
                         errorR(Error(FSComp.SR.tcOverrideArityMismatch(),memberId.idRange));
                         []
                     | _ -> 
                         //printfn "no unique matching dispatch slot for %s at %s" memberId.idText (stringOfRange memberId.idRange)
                         // We hit this case when it is ambiguous which abstract method is being implemented. 
                         []
               
             
             // If we determined a unique member then utilize the type information from the slotsig 
             let declaredTypars = 
                 match uniqueAbstractMethSigs with 
                 | uniqueAbstractMeth :: _ -> 

                     let uniqueAbstractMeth = InstMethInfo cenv.amap m renaming uniqueAbstractMeth
                     
                     let typarsFromAbsSlotAreRigid,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot = 
                         FreshenAbstractSlot cenv.g cenv.amap m synTyparDecls uniqueAbstractMeth

                     let declaredTypars = (if typarsFromAbsSlotAreRigid then typarsFromAbsSlot else declaredTypars)

                     let absSlotTy = mkMethodTy cenv.g argTysFromAbsSlot retTyFromAbsSlot

                     UnifyTypes cenv envinner m bindingTy absSlotTy;
                     declaredTypars
                 | _ -> declaredTypars 

                 // Retained to ensure use of an FSComp.txt entry, can be removed at a later date: errorR(Error(FSComp.SR.tcDefaultAmbiguous(),memberId.idRange));

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
                   errorR(Error(FSComp.SR.tcNoPropertyFoundForOverride(),memberId.idRange)); 
                   []
               | [uniqueAbstractProp] -> [uniqueAbstractProp]
               | _ -> 
                   // We hit this case when it is ambiguous which abstract property is being implemented. 
                   []

           // If we determined a unique member then utilize the type information from the slotsig 
           uniqueAbstractPropSigs |> List.iter (fun uniqueAbstractProp -> 

               let kIsGet = (k = MemberKind.PropertyGet)

               if not (if kIsGet then uniqueAbstractProp.HasGetter else uniqueAbstractProp.HasSetter) then 
                   error(Error(FSComp.SR.tcAbstractPropertyMissingGetOrSet(if kIsGet then "getter" else "setter"),memberId.idRange));

               let uniqueAbstractMeth = if kIsGet then uniqueAbstractProp.GetterMethod else uniqueAbstractProp.SetterMethod

               let uniqueAbstractMeth = InstMethInfo cenv.amap m renaming uniqueAbstractMeth

               let _,typarsFromAbsSlot,argTysFromAbsSlot, retTyFromAbsSlot = 
                    FreshenAbstractSlot cenv.g cenv.amap m synTyparDecls uniqueAbstractMeth

               if nonNil(typarsFromAbsSlot) then 
                   errorR(InternalError("Unexpected generic property",memberId.idRange));

               let absSlotTy = 
                   if (memberFlags.MemberKind = MemberKind.PropertyGet) 
                   then mkMethodTy cenv.g argTysFromAbsSlot retTyFromAbsSlot 
                   else 
                     match argTysFromAbsSlot with 
                     | [argTysFromAbsSlot] -> mkTupledTy cenv.g argTysFromAbsSlot --> cenv.g.unit_ty
                     | _ -> 
                         error(Error(FSComp.SR.tcInvalidSignatureForSet(),memberId.idRange)); 
                         retTyFromAbsSlot --> cenv.g.unit_ty

               UnifyTypes cenv envinner m bindingTy absSlotTy);
           

                   // Retained to ensure use of an FSComp.txt entry, can be removed at a later date: errorR(Error(FSComp.SR.tcPropertyAlreadyHasDefaultImplementation(),memberId.idRange));
                   // Retained to ensure use of an FSComp.txt entry, can be removed at a later date: errorR(Error(FSComp.SR.tcPropertyImplementedIsAmbiguous(),memberId.idRange));
           
           // What's the type containing the abstract slot we're implementing? Used later on in MakeMemberDataAndMangledNameForMemberVal. 
           // This type must be in terms of the enclosing type's formal type parameters, hence the application of revRenaming.
           
           let optInferredImplSlotTys = 
               match optIntfSlotTy with 
               | Some (x,_) -> [ x ]
               | None -> List.map PropInfo.EnclosingType uniqueAbstractPropSigs

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
// TcLetrec - AnalyzeAndMakeRecursiveValue(s)
//------------------------------------------------------------------------

and AnalyzeRecursiveStaticMemberOrValDecl (cenv, envinner: TcEnv, tpenv, declKind, newslotsOK, overridesOK, tcrefContainerInfo, vis1, id:Ident, vis2, declaredTypars, memberFlagsOpt, thisIdOpt, bindingAttribs, valSynInfo, ty, bindingRhs, bindingRange, flex) =
    let vis = CombineVisibilityAttribs vis1 vis2 bindingRange

    // Check if we're defining a member, in which case generate the internal unique 
    // name for the member and the information about which type it is agumenting 
      
    match tcrefContainerInfo, memberFlagsOpt with 
    | (Some(MemberOrValContainerInfo(tcref, optIntfSlotTy, baseValOpt, _safeInitInfo, declaredTyconTypars)),Some(memberFlags)) -> 
        assert (isNone(optIntfSlotTy))
      
        CheckMemberFlags cenv.g None newslotsOK overridesOK memberFlags id.idRange;
        CheckForNonAbstractInterface declKind tcref memberFlags id.idRange;

        if tcref.Deref.IsExceptionDecl && 
           (memberFlags.MemberKind = MemberKind.Constructor) then 
            error(Error(FSComp.SR.tcConstructorsDisallowedInExceptionAugmentation(),id.idRange));                  

        let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
        let _,enclosingDeclaredTypars,_,objTy,thisTy = FreshenObjectArgType cenv bindingRange TyparWillBeRigid tcref isExtrinsic declaredTyconTypars
        let envinner = AddDeclaredTypars CheckForDuplicateTypars enclosingDeclaredTypars envinner
        let envinner = MakeInnerEnvForTyconRef cenv envinner tcref isExtrinsic 

        let safeThisValOpt, baseValOpt = 
            match memberFlags.MemberKind with 
            
            // Explicit struct or class constructor
            | MemberKind.Constructor  ->
                // A fairly adhoc place to put this check 
                if tcref.IsStructOrEnumTycon && (match valSynInfo with SynValInfo([[]],_) -> true | _ -> false) then
                    errorR(Error(FSComp.SR.tcStructsCannotHaveConstructorWithNoArguments(),bindingRange));

                if not tcref.IsFSharpObjectModelTycon then 
                    errorR(Error(FSComp.SR.tcConstructorsIllegalForThisType(),id.idRange));

                let safeThisValOpt = MakeAndPublishSafeThisVal cenv envinner thisIdOpt thisTy
                  
                // baseValOpt is the 'base' variable associated with the inherited portion of a class 
                // It is declared once on the 'inheritedTys clause, but a fresh binding is made for 
                // each member that may use it. 
                let baseValOpt = 
                    match SuperTypeOfType cenv.g cenv.amap bindingRange objTy with 
                    | Some superTy -> MakeAndPublishBaseVal cenv envinner (match baseValOpt with None -> None | Some v -> Some v.Id) superTy 
                    | None -> None

                let domainTy = NewInferenceType ()

                // This is the type we pretend a constructor has, because its implementation must ultimately appear to return a value of the given type 
                // This is somewhat awkward later in codegen etc. 
                UnifyTypes cenv envinner bindingRange ty (domainTy --> objTy);

                safeThisValOpt, baseValOpt
                
            | _ -> 
                None,None
          
        let memberInfo = 
            let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
            MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,isExtrinsic,bindingAttribs,[],memberFlags,valSynInfo,id,false)

        envinner,tpenv,id,Some(memberInfo),vis,vis2,safeThisValOpt,enclosingDeclaredTypars,baseValOpt,flex,bindingRhs,declaredTypars
        
    // non-member bindings. How easy. 
    | _ -> 
        envinner,tpenv,id,None,vis,vis2,None,[],None,flex,bindingRhs,declaredTypars
    

and AnalyzeRecursiveInstanceMemberDecl (cenv,envinner: TcEnv,tpenv,declKind,synTyparDecls,valSynInfo,flex,newslotsOK,overridesOK,vis1,thisId,memberId:Ident,toolId:Ident option,bindingAttribs,vis2,tcrefContainerInfo,memberFlagsOpt,ty,bindingRhs,bindingRange) =
    let vis = CombineVisibilityAttribs vis1 vis2 bindingRange
    let (ExplicitTyparInfo(_,declaredTypars,infer)) = flex
    match tcrefContainerInfo,memberFlagsOpt with 
     // Normal instance members. 
     | Some(MemberOrValContainerInfo(tcref, optIntfSlotTy, baseValOpt, _safeInitInfo, declaredTyconTypars)), Some(memberFlags) -> 
       
         CheckMemberFlags cenv.g optIntfSlotTy newslotsOK overridesOK memberFlags bindingRange;

         if isSome vis && memberFlags.IsOverrideOrExplicitImpl then 
            errorR(Error(FSComp.SR.tcOverridesCannotHaveVisibilityDeclarations(),memberId.idRange));
              
       
         // Syntactically push the "this" variable across to be a lambda on the right 
         let bindingRhs = PushOnePatternToRhs true (mkSynThisPatVar thisId) bindingRhs
       
         // The type being augmented tells us the type of 'this' 
         let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
         let tcrefObjTy,enclosingDeclaredTypars,renaming,objTy,thisTy = FreshenObjectArgType cenv bindingRange TyparWillBeRigid tcref isExtrinsic declaredTyconTypars

         let envinner = AddDeclaredTypars CheckForDuplicateTypars enclosingDeclaredTypars envinner

         // If private, the member's accessibility is related to 'tcref' 
         let envinner = MakeInnerEnvForTyconRef cenv envinner tcref isExtrinsic 

         let baseValOpt = if tcref.IsFSharpObjectModelTycon then baseValOpt else None

         // Apply the known type of 'this' 
         let bindingTy = NewInferenceType ()
         UnifyTypes cenv envinner bindingRange ty (thisTy --> bindingTy);

         CheckForNonAbstractInterface declKind tcref memberFlags memberId.idRange; 
         
         // Determine if a uniquely-identified-override List.exists based on the information 
         // at the member signature. If so, we know the type of this member, and the full slotsig 
         // it implements. Apply the inferred slotsig. 
         let optInferredImplSlotTys, declaredTypars = 
             ApplyAbstractSlotInference cenv envinner (bindingTy,bindingRange,synTyparDecls,declaredTypars,memberId,tcrefObjTy,renaming,objTy,optIntfSlotTy,valSynInfo,memberFlags,bindingAttribs)

         // Update the ExplicitTyparInfo to reflect the declaredTypars inferred from the abstract slot 
         let flex = ExplicitTyparInfo(declaredTypars,declaredTypars,infer)

         // baseValOpt is the 'base' variable associated with the inherited portion of a class 
         // It is declared once on the 'inheritedTys clause, but a fresh binding is made for 
         // each member that may use it. 
         let baseValOpt = 
             match SuperTypeOfType cenv.g cenv.amap bindingRange objTy with 
             | Some(superTy) -> MakeAndPublishBaseVal cenv envinner (match baseValOpt with None -> None | Some v -> Some v.Id) superTy 
             | None -> None

         let memberInfo = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,isExtrinsic,bindingAttribs,optInferredImplSlotTys,memberFlags,valSynInfo,memberId,false)
         let memberId = match toolId with Some tid -> ident(memberId.idText, tid.idRange) | None -> memberId

         envinner,tpenv,memberId,Some(memberInfo),vis,vis2,None,enclosingDeclaredTypars,baseValOpt,flex,bindingRhs,declaredTypars
     | _ -> 
         error(Error(FSComp.SR.tcRecursiveBindingsWithMembersMustBeDirectAugmentation(),bindingRange)) 

and AnalyzeRecursiveDecl (cenv,envinner,tpenv,declKind,synTyparDecls,declaredTypars,thisIdOpt,valSynInfo,flex,newslotsOK,overridesOK,vis1,declPattern,bindingAttribs,tcrefContainerInfo,memberFlagsOpt,ty,bindingRhs,bindingRange) =
    let rec AnalyzeRecursiveDeclPat tpenv p = 
        match p with  
        | SynPat.Typed(pat',cty,_) -> 
            let cty',tpenv = TcTypeAndRecover cenv NewTyparsOK CheckCxs envinner tpenv cty
            UnifyTypes cenv envinner bindingRange ty cty';
            AnalyzeRecursiveDeclPat tpenv pat' 
        | SynPat.Attrib(_pat',_attribs,m) -> 
            error(Error(FSComp.SR.tcAttributesInvalidInPatterns(),m));
            //AnalyzeRecursiveDeclPat pat' 

        // This is for the construct
        //    'let rec x = ... and do ... and y = ...' 
        // DEPRECATED IN pars.mly 
        | SynPat.Const (SynConst.Unit, m) -> 
             let id = ident ("doval",m)
             AnalyzeRecursiveDeclPat tpenv (SynPat.Named (SynPat.Wild m, id,false,None,m))
             
        | SynPat.Named (SynPat.Wild _, id,_,vis2,_) -> 
            AnalyzeRecursiveStaticMemberOrValDecl (cenv,envinner,tpenv,declKind,newslotsOK,overridesOK,tcrefContainerInfo,vis1,id,vis2,declaredTypars,memberFlagsOpt,thisIdOpt,bindingAttribs,valSynInfo,ty,bindingRhs,bindingRange,flex)
            
        | SynPat.InstanceMember(thisId,memberId,toolId,vis2,_) -> 
            AnalyzeRecursiveInstanceMemberDecl (cenv,envinner,tpenv,declKind,synTyparDecls,valSynInfo,flex,newslotsOK,overridesOK,vis1,thisId,memberId,toolId,bindingAttribs,vis2,tcrefContainerInfo,memberFlagsOpt,ty,bindingRhs,bindingRange)

        | _ -> error(Error(FSComp.SR.tcOnlySimplePatternsInLetRec(),bindingRange))

    AnalyzeRecursiveDeclPat tpenv declPattern


/// This is a major routine that generates the Val for a recursive binding 
/// prior to the analysis of the definition of the binding. This includes
/// members of all flavours (including properties, implicit class constructors
/// and overrides). At this point we perform override inference, to infer
/// which method we are overriding, in order to add constraints to the
/// implementation of the method.
and AnalyzeAndMakeRecursiveValue overridesOK cenv (env: TcEnv) (tpenv,recBindIdx) (NormalizedRecBindingDefn(containerInfo,newslotsOK,declKind,binding)) =

    // Pull apart the inputs
    let (NormalizedBinding(vis1,bindingKind,isInline,isMutable,bindingSynAttribs,bindingXmlDoc,synTyparDecls,valSynData,declPattern,bindingRhs,bindingRange,spBind)) = binding
    let (NormalizedBindingRhs(_,_,bindingExpr)) = bindingRhs
    let (SynValData(memberFlagsOpt,valSynInfo,thisIdOpt)) = valSynData 
    let (ContainerInfo(altActualParent,tcrefContainerInfo)) = containerInfo
    
    let attrTgt = DeclKind.AllowedAttribTargets memberFlagsOpt declKind 

    // Check the attributes on the declaration
    let bindingAttribs = TcAttributes cenv env attrTgt bindingSynAttribs

    // Allocate the type inference variable for the inferred type
    let ty = NewInferenceType () 
        
        
    let inlineFlag = ComputeInlineFlag memberFlagsOpt isInline isMutable bindingAttribs
    if isMutable then errorR(Error(FSComp.SR.tcOnlyRecordFieldsAndSimpleLetCanBeMutable(),bindingRange));


    // Typecheck the typar decls, if any
    let flex, tpenv = TcBindingTyparDecls false cenv env bindingRange tpenv synTyparDecls
    let (ExplicitTyparInfo(_,declaredTypars,_)) = flex
    let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTypars env
    
    // OK, analyze the declaration and return lots of information about it
    let envinner,tpenv,bindingId,memberInfoOpt,vis,vis2,safeThisValOpt,enclosingDeclaredTypars,baseValOpt,flex,bindingRhs,declaredTypars = 

        AnalyzeRecursiveDecl (cenv, envinner, tpenv, declKind, synTyparDecls, declaredTypars, thisIdOpt, valSynInfo, flex,
                              newslotsOK, overridesOK, vis1, declPattern, bindingAttribs, tcrefContainerInfo,
                              memberFlagsOpt, ty, bindingRhs, bindingRange)


    let optArgsOK = isSome(memberFlagsOpt)

    // Assert the types given in the argument patterns
    ApplyTypesFromArgumentPatterns(cenv,envinner,optArgsOK,ty,bindingRange,tpenv,bindingRhs,memberFlagsOpt);

    // Do the type annotations give the full and complete generic type? 
    // If so, generic recursion can be used when using this type. 
    let isComplete =  ComputeIsComplete enclosingDeclaredTypars declaredTypars ty
    
    // NOTE: The type scheme here is normally not 'complete'!!!! The type is more or less just a type variable at this point. 
    // NOTE: toparity, type and typars get fixed-up after inference 
    let prelimTyscheme = TypeScheme(enclosingDeclaredTypars@declaredTypars,ty)
    let partialValReprInfo = TranslateTopValSynInfo bindingRange (TcAttributes cenv envinner) valSynInfo
    let topValInfo = UseSyntacticArity declKind prelimTyscheme partialValReprInfo
    let hasDeclaredTypars = declaredTypars.Length > 0
    let prelimValScheme = ValScheme(bindingId,prelimTyscheme,topValInfo,memberInfoOpt,false,inlineFlag,NormalVal,vis,false,false,false,hasDeclaredTypars)

    // Check the literal r.h.s., if any
    let _, konst = TcLiteral cenv ty env tpenv (bindingAttribs,bindingExpr)

    // Create the value 
    let vspec = MakeAndPublishVal cenv envinner (altActualParent,false,declKind,ValInRecScope(isComplete),prelimValScheme,bindingAttribs,bindingXmlDoc ,konst)

    let extraBindings,extraValues,tpenv,recBindIdx = 
       let extraBindings = 
          [ for extraBinding in EventDeclarationNormalization.GenerateExtraBindings(cenv.g,bindingAttribs,binding) do
               yield (NormalizedRecBindingDefn(containerInfo,newslotsOK,declKind,extraBinding)) ]
       let res,(tpenv,recBindIdx) = List.mapFold (AnalyzeAndMakeRecursiveValue overridesOK cenv env) (tpenv,recBindIdx) extraBindings
       let extraBindings, extraValues = List.unzip res
       List.concat extraBindings, List.concat extraValues, tpenv,recBindIdx

    let mangledId = ident(vspec.LogicalName,vspec.Range)
    // Reconstitute the binding with the unique name
    let revisedBinding = NormalizedBinding (vis1,bindingKind,isInline,isMutable,bindingSynAttribs,bindingXmlDoc,synTyparDecls,valSynData,mkSynPatVar vis2 mangledId,bindingRhs,bindingRange,spBind)

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


and AnalyzeAndMakeRecursiveValues  overridesOK cenv env tpenv binds = 
    let recBindIdx = 0
    let res,tpenv = List.mapFold (AnalyzeAndMakeRecursiveValue  overridesOK cenv env) (tpenv,recBindIdx) binds
    let bindings, values = List.unzip res
    List.concat bindings, List.concat values, tpenv


//-------------------------------------------------------------------------
// TcLetrecBinding
//-------------------------------------------------------------------------

and TcLetrecBinding 
         (cenv, 
          envRec: TcEnv,
          scopem,
          extraGeneralizableTypars: Typars,
          reqdThisValTyOpt: TType option)
         
         // The state of the left-to-right iteration through the bindings
         (envNonRec: TcEnv, 
          generalizedRecBinds : PostGeneralizationRecursiveBinding list,
          preGeneralizationRecBinds: PreGeneralizationRecursiveBinding list,
          tpenv,
          uncheckedRecBindsTable : Map<Stamp,PreCheckingRecursiveBinding>) 
         
         // This is the actual binding to check
         (rbind : PreCheckingRecursiveBinding) = 

    let (RBInfo(_,_,enclosingDeclaredTypars,_,vspec,flex,_,_,baseValOpt,safeThisValOpt,safeInitInfo,_,tau,declKind)) = rbind.RecBindingInfo
    let denv = envRec.DisplayEnv
    
    let allDeclaredTypars = enclosingDeclaredTypars @ rbind.RecBindingInfo.DeclaredTypars

    // dprintf "TcLetrec (before): tau = %s\n" (Layout.showL  (typeL tau));  

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
    let envRec = Option.foldBack (AddLocalVal scopem) baseValOpt envRec
    let envRec = Option.foldBack (AddLocalVal scopem) safeThisValOpt envRec

    // Members can access protected members of parents of the type, and private members in the type 
    let envRec = MakeInnerEnvForMember cenv envRec vspec 

    let checkedBind,tpenv = 
        TcNormalizedBinding declKind cenv envRec tpenv tau safeThisValOpt safeInitInfo (enclosingDeclaredTypars,flex) rbind.SyntacticBinding

    // dprintf "TcLetrec (%s, after): tau = %s\n" vspec.LogicalName (Layout.showL  (typeL tau));    
    (try UnifyTypes cenv envRec vspec.Range (allDeclaredTypars +-> tau) vspec.Type 
     with e -> error (Recursion(envRec.DisplayEnv,vspec.Id,tau,vspec.Type,vspec.Range)));

    // Inside the incremental class sytntax we assert the type of the 'this' variable to be precisely the same type as the 
    // this variable for the implicit class constructor. For static members, we assert the type variables associated
    // for the class to be identical to those used for the implicit class constructor and the static class constructor.
    match reqdThisValTyOpt with 
    | None -> ()
    | Some reqdThisValTy -> 
        let (CheckedBindingInfo(_,_,_,_,_,_,_,expr,_,_,_,_,_,_)) = checkedBind
        let reqdThisValTy, actualThisValTy, rangeForCheck =
            match GetInstanceMemberThisVariable (vspec, expr) with
               | None -> 
                   let reqdThisValTy = if isByrefTy cenv.g reqdThisValTy then destByrefTy cenv.g reqdThisValTy else reqdThisValTy
                   let enclosingTyconRef = tcrefOfAppTy cenv.g reqdThisValTy
                   reqdThisValTy, (mkAppTy enclosingTyconRef (List.map mkTyparTy enclosingDeclaredTypars)), vspec.Range
               | Some thisVal -> 
                   reqdThisValTy, thisVal.Type, thisVal.Range
        if not (AddCxTypeEqualsTypeUndoIfFailed envRec.DisplayEnv cenv.css rangeForCheck actualThisValTy reqdThisValTy) then 
            errorR (Error(FSComp.SR.tcNonUniformMemberUse vspec.DisplayName,vspec.Range));

    // dprintf "TcLetrec (%s, after unify): typeOfVal v = %s\n" v.LogicalName (Layout.showL  (typeL v.Type));  
    
    let preGeneralizationRecBind =  { RecBindingInfo = rbind.RecBindingInfo; 
                                      CheckedBinding= checkedBind; 
                                      ExtraGeneralizableTypars= extraGeneralizableTypars }

    // Remove one binding from the unchecked list
    let uncheckedRecBindsTable = 
        assert (uncheckedRecBindsTable.ContainsKey rbind.RecBindingInfo.Val.Stamp)
        uncheckedRecBindsTable.Remove rbind.RecBindingInfo.Val.Stamp
    
    // Add one binding to the candidates eligible for generalization
    let preGeneralizationRecBinds = (preGeneralizationRecBind::preGeneralizationRecBinds)
    
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
                    // Any declared type parameters in an implicit class construction are always generalizable
                    let freeInBinding = Zset.diff  freeInBinding (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.ExtraGeneralizableTypars))

                    if freeInBinding.IsEmpty then true else

                    //printfn "(failed generalization test 2 for binding for %s)" pgrbind.RecBindingInfo.Val.DisplayName

                    // Any declared method parameters can always be generalized
                    let freeInBinding = Zset.diff  freeInBinding (Zset.ofList typarOrder pgrbind.RecBindingInfo.DeclaredTypars)

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
        // Note that in the normal case each binding passes IsGeneralizableValue.

        let freeInEnv =
            (freeInEnv,newGeneralizableBindings) ||> List.fold (fun freeInEnv pgrbind -> 
                let (CheckedBindingInfo(_,_,_,_,_,_,_,expr,_,_,_,_,_,_)) = pgrbind.CheckedBinding
                if GeneralizationHelpers.IsGeneralizableValue cenv.g expr then 
                    freeInEnv 
                else 
                    let freeInBinding = (freeInType CollectAllNoCaching pgrbind.RecBindingInfo.Val.TauType).FreeTypars
                    Zset.union freeInBinding freeInEnv)

        // Process the bindings marked for transition from PreGeneralization --> PostGeneralization
        let newGeneralizedRecBinds,tpenv = 
            if newGeneralizableBindings.IsEmpty then 
                [], tpenv
            else
                
                let supportForBindings = newGeneralizableBindings |> List.collect (TcLetrecComputeSupportForBinding cenv)
                GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denv,vspec.Range) supportForBindings; 
                 
                let generalizedTyparsL = newGeneralizableBindings |> List.map (TcLetrecComputeAndGeneralizeGenericTyparsForBinding cenv denv freeInEnv) 
                
                // Generalize the bindings. 
                let newGeneralizedRecBinds = (generalizedTyparsL,newGeneralizableBindings) ||> List.map2 (TcLetrecGeneralizeBinding cenv denv ) 
                let tpenv = HideUnscopedTypars (List.concat generalizedTyparsL) tpenv
                newGeneralizedRecBinds,tpenv
        
    
        newGeneralizedRecBinds, preGeneralizationRecBinds, tpenv

    let envNonRec = envNonRec |> AddLocalVals scopem (newGeneralizedRecBinds |> List.map (fun b -> b.RecBindingInfo.Val))  
    let generalizedRecBinds = newGeneralizedRecBinds @ generalizedRecBinds        

    (envNonRec,generalizedRecBinds,preGeneralizationRecBinds,tpenv,uncheckedRecBindsTable)

//-------------------------------------------------------------------------
// TcLetrecComputeAndGeneralizeGenericTyparsForBinding
//-------------------------------------------------------------------------

/// Compute the type variables which may be generalized and perform the generalization 
and TcLetrecComputeAndGeneralizeGenericTyparsForBinding cenv denv freeInEnv (pgrbind : PreGeneralizationRecursiveBinding)  =

    let freeInEnv = Zset.diff freeInEnv (Zset.ofList typarOrder (NormalizeDeclaredTyparsForEquiRecursiveInference cenv.g pgrbind.ExtraGeneralizableTypars))

    let (RBInfo(_,containerInfo,enclosingDeclaredTypars,_,vspec,flex,_,_,_,_,_,_,_,declKind)) = pgrbind.RecBindingInfo
    let (CheckedBindingInfo(inlineFlag,immut,_,_,_,_,_,expr,_,_,m,_,_,_)) = pgrbind.CheckedBinding
    let (ExplicitTyparInfo(rigidCopyOfDeclaredTypars,declaredTypars,canInferTypars)) = flex
    let allDeclaredTypars = enclosingDeclaredTypars @ pgrbind.RecBindingInfo.DeclaredTypars

     
    // The declared typars were not marked rigid to allow equi-recursive type inference to unify
    // two declared type variables. So we now check that, for each binding, the declared
    // type variables can be unified with a rigid version of the same and undo the results
    // of this unification.
    ConstraintSolver.CheckDeclaredTypars denv cenv.css m rigidCopyOfDeclaredTypars declaredTypars 

    let memFlagsOpt = vspec.MemberInfo |> Option.map (fun memInfo -> memInfo.MemberFlags)
    let isCtor = (match memFlagsOpt with None -> false | Some memberFlags -> memberFlags.MemberKind = MemberKind.Constructor)

    let canInferTypars = GeneralizationHelpers.ComputeCanInferTypars(containerInfo.ParentRef,canInferTypars,memFlagsOpt,declaredTypars,m)

    let tau = vspec.TauType
    let maxInferredTypars = (freeInTypeLeftToRight cenv.g false tau)

    let generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars (cenv,denv,m,immut,freeInEnv,canInferTypars,GeneralizationHelpers.CanGeneralizeConstrainedTyparsForDecl(declKind),inlineFlag, Some(expr), allDeclaredTypars, maxInferredTypars,tau,isCtor)
    generalizedTypars

/// Compute the type variables which may have member constraints that need to be canonicalized prior to generalization 
and TcLetrecComputeSupportForBinding cenv (pgrbind : PreGeneralizationRecursiveBinding) =
    let (RBInfo(_,_,enclosingDeclaredTypars,_,vspec,_,_,_,_,_,_,_,_,_)) = pgrbind.RecBindingInfo
    let allDeclaredTypars = enclosingDeclaredTypars @ pgrbind.RecBindingInfo.DeclaredTypars
    let maxInferredTypars = freeInTypeLeftToRight cenv.g false vspec.TauType
    allDeclaredTypars @ maxInferredTypars

//-------------------------------------------------------------------------
// TcLetrecGeneralizeBinding
//------------------------------------------------------------------------

// Generalise generalizedTypars from checkedBind.
and TcLetrecGeneralizeBinding cenv denv generalizedTypars (pgrbind : PreGeneralizationRecursiveBinding) : PostGeneralizationRecursiveBinding =

    let (RBInfo(_,_,enclosingDeclaredTypars,_,vspec,flex,partialValReprInfo,memberInfoOpt,_,_,_,vis,_,declKind)) = pgrbind.RecBindingInfo
    let (CheckedBindingInfo(inlineFlag,_,_,_,_,_,_,expr,argAttribs,_,_,_,compgen,_)) = pgrbind.CheckedBinding
     
    let _,tau = vspec.TypeScheme

    let pvalscheme1 = PrelimValScheme1(vspec.Id,flex,tau,Some(partialValReprInfo),memberInfoOpt,false,inlineFlag,NormalVal,argAttribs,vis,compgen)
    let pvalscheme2 = GeneralizeVal cenv denv enclosingDeclaredTypars generalizedTypars pvalscheme1

    let valscheme = UseCombinedArity cenv.g declKind expr pvalscheme2 
    AdjustRecType cenv vspec valscheme;

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
            mkRecdFieldGet g (exprForVal m thisVar, rfref, tinst, [], m)
    let failureExpr = match thisValOpt with None -> mkCallFailStaticInit g m | Some _ -> mkCallFailInit g m
    mkCompGenSeq m (mkIfThen g m (mkILAsmClt g m availExpr reqExpr) failureExpr) expr

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
    let (CheckedBindingInfo(_,_,_,_,_,_,_,expr,_,_,_,spBind,_,_)) = pgrbind.CheckedBinding
      
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
              
    { ValScheme = pgrbind.ValScheme;
      Binding = TBind(vspec,expr,spBind) }

and FixupLetrecBind cenv denv generalizedTyparsForRecursiveBlock (bind : PostBindCtorThisVarRefCellRecursiveBinding) = 
    let (TBind(vspec,expr,spBind)) = bind.Binding

    // Check coherence of generalization of variables for memberInfo members in generic classes 
    match vspec.MemberInfo with 
    | Some _ -> 
       match PartitionValTypars cenv.g vspec with
       | Some(parentTypars,memberParentTypars,_,_,_) -> 
          ignore(SignatureConformance.Checker(cenv.g, cenv.amap, denv, SignatureRepackageInfo.Empty, false).CheckTypars vspec.Range TypeEquivEnv.Empty memberParentTypars parentTypars)
       | None -> 
          errorR(Error(FSComp.SR.tcMemberIsNotSufficientlyGeneric(),vspec.Range))
    | None -> ()

    // Fixup recursive references... 
    let fixupPoints = GetAllUsesOfRecValue cenv vspec

    AdjustAndForgetUsesOfRecValue cenv (mkLocalValRef vspec) bind.ValScheme;

    // dprintf "TcLetrec (%s, after gen): #fixupPoints = %d, ty = %s\n" vspec.LogicalName (List.length fixupPoints) (Layout.showL  (typeL vspec.Type)); 

    let expr = mkGenericBindRhs cenv.g vspec.Range generalizedTyparsForRecursiveBlock bind.ValScheme.TypeScheme expr

    { FixupPoints = fixupPoints;
      Binding = TBind(vspec,expr,spBind) }
    
//-------------------------------------------------------------------------
// TcLetrec
//------------------------------------------------------------------------

and unionGeneralizedTypars typarSets = List.foldBack (ListSet.unionFavourRight typarEq) typarSets [] 
    

and TcLetrec  overridesOK cenv env tpenv (binds,bindsm,scopem) =

    // create prelimRecValues for the recursive items (includes type info from LHS of bindings) *)
    let binds = binds |> List.map (fun (RecBindingDefn(a,b,c,bind)) -> NormalizedRecBindingDefn(a,b,c,BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env bind))
    let uncheckedRecBinds,prelimRecValues,(tpenv,_) = AnalyzeAndMakeRecursiveValues  overridesOK cenv env tpenv binds

    let envRec = AddLocalVals scopem prelimRecValues env 
    // typecheck bindings 
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
            | (rbind :: _) -> 
                let (RBInfo(_,_,_,_,_,_,_,_,_,_,_,_,_,declKind)) = rbind.RecBindingInfo
                DeclKind.MustHaveArity declKind
            
        EliminateInitializationGraphs cenv.g mustHaveArity env.DisplayEnv bindsWithoutLaziness bindsm
    
    // Post letrec env 
    let envbody = AddLocalVals scopem prelimRecValues env 
    binds,envbody,tpenv



//-------------------------------------------------------------------------
// Bind specifications of values
//------------------------------------------------------------------------- 

let TcAndPublishValSpec (cenv, env, containerInfo, declKind, memFlagsOpt, tpenv, valSpfn) = 

  let (ValSpfn (attrs, _, SynValTyparDecls (_, canInferTypars, _), _, _, pseudo, mutableFlag, doc, vis, literalExprOpt, m)) = valSpfn 
  
  let attrTgt = DeclKind.AllowedAttribTargets memFlagsOpt declKind 

  let attrs' = TcAttributes cenv env attrTgt attrs
  let valinfos, tpenv = TcValSpec cenv env declKind containerInfo memFlagsOpt None tpenv valSpfn attrs'
  let denv = env.DisplayEnv
    
  (tpenv, valinfos) ||> List.mapFold (fun tpenv valSpecResult -> 

            let (ValSpecResult (altActualParent, memberInfoOpt, id, enclosingDeclaredTypars, declaredTypars, ty, partialValReprInfo, declKind)) = valSpecResult
            
            let inlineFlag = ComputeInlineFlag (memberInfoOpt |> Option.map (fun (ValMemberInfoTransient(memberInfo,_,_)) -> memberInfo.MemberFlags)) pseudo mutableFlag attrs'
            
            let freeInType = freeInTypeLeftToRight cenv.g false ty

            let allDeclaredTypars = enclosingDeclaredTypars @ declaredTypars

            let flex = ExplicitTyparInfo(declaredTypars,declaredTypars,canInferTypars)
            
            let canInferTypars = GeneralizationHelpers.ComputeCanInferTypars(containerInfo.ParentRef,canInferTypars,memFlagsOpt,declaredTypars,m)
            
            let generalizedTypars = GeneralizationHelpers.ComputeAndGeneralizeGenericTypars(cenv,denv,id.idRange,canInferTypars,emptyFreeTypars,canInferTypars,CanGeneralizeConstrainedTypars,inlineFlag,None,allDeclaredTypars,freeInType,ty,false)
            
            let valscheme1 = PrelimValScheme1(id,flex,ty,Some(partialValReprInfo),memberInfoOpt,mutableFlag,inlineFlag,NormalVal,noArgOrRetAttribs,vis,false)

            let valscheme2 = GeneralizeVal cenv denv enclosingDeclaredTypars generalizedTypars valscheme1

            let tpenv = HideUnscopedTypars generalizedTypars tpenv

            let valscheme = BuildValScheme declKind (Some(partialValReprInfo)) valscheme2 

            let konst = 
                match literalExprOpt with 
                | None -> 
                    let hasLiteralAttr = HasAttrib cenv.g cenv.g.attrib_LiteralAttribute attrs'
                    if hasLiteralAttr then 
                        errorR(Error(FSComp.SR.tcLiteralAttributeRequiresConstantValue(),m));
                    None

                
                | Some(e) -> 
                    let hasLiteralAttr,konst = TcLiteral cenv ty env tpenv (attrs',e)
                    if not hasLiteralAttr then 
                        errorR(Error(FSComp.SR.tcValueInSignatureRequiresLiteralAttribute(), e.Range));
                    konst

            let vspec = MakeAndPublishVal cenv env (altActualParent,true,declKind,ValNotInRecScope,valscheme,attrs',doc.ToXmlDoc(),konst)
            assert(vspec.InlineInfo = inlineFlag);

            vspec,tpenv)


//-------------------------------------------------------------------------
// Bind elements of data definitions for exceptions and types (fields, etc.)
//------------------------------------------------------------------------- 

exception NotUpperCaseConstructor of range

let CheckNamespaceModuleOrTypeName cenv (id:Ident) = 
    // type names '[]' etc. are used in fslib
    if not cenv.g.compilingFslib &&  id.idText.IndexOfAny(IllegalCharactersInTypeAndNamespaceNames) <> -1 then 
        errorR(Error(FSComp.SR.tcInvalidNamespaceModuleTypeUnionName(),id.idRange))

let CheckDuplicates (idf : _ -> Ident) k elems = 
    elems |> List.iteri (fun i uc1 -> 
        elems |> List.iteri (fun j uc2 -> 
            let id1 = (idf uc1)
            let id2 = (idf uc2)
            if j > i &&  id1.idText = id2.idText then 
                errorR (Duplicate(k,id1.idText,id1.idRange))));
    elems


module TcRecdUnionAndEnumDeclarations = begin

    let CombineReprAccess parent vis = 
        match parent with 
        | ParentNone -> vis 
        | Parent tcref -> combineAccess vis tcref.TypeReprAccessibility

    let MakeRecdFieldSpec _cenv env parent (stat,konst,ty',attrsForProperty,attrsForField,id,isMutable,vol,xmldoc,vis,m) =
        let vis,_ = ComputeAccessAndCompPath env None m vis parent
        let vis = CombineReprAccess parent vis
        NewRecdField stat konst id ty' isMutable vol attrsForProperty attrsForField xmldoc vis false

    let TcFieldDecl cenv env parent  isIncrClass tpenv (isStatic,attrs,id,ty,isMutable,xmldoc,vis,m) =
        let attrs' = TcAttributesWithPossibleTargets cenv env AttributeTargets.FieldDecl attrs
        let attrsForProperty,attrsForField = attrs' |> List.partition (fun (attrTargets,_) -> (attrTargets &&& AttributeTargets.Property) <> enum 0) 
        let attrsForProperty = (List.map snd attrsForProperty) 
        let attrsForField = (List.map snd attrsForField)
        let ty',_ = TcTypeAndRecover cenv NoNewTypars CheckCxs env tpenv ty
        let zeroInit = HasAttrib cenv.g cenv.g.attrib_DefaultValueAttribute attrsForField
        let isVolatile = HasAttrib cenv.g cenv.g.attrib_VolatileFieldAttribute attrsForField
        
        let isThreadStatic = isThreadOrContextStatic cenv.g attrsForField
        if isThreadStatic && (not zeroInit || not isStatic) then 
            error(Error(FSComp.SR.tcThreadStaticAndContextStaticMustBeStatic(),m));

        if isVolatile then 
            error(Error(FSComp.SR.tcVolatileOnlyOnClassLetBindings(),m));

        if isIncrClass && (not zeroInit || not isMutable) then errorR(Error(FSComp.SR.tcUninitializedValFieldsMustBeMutable(),m));
        if isStatic && (not zeroInit || not isMutable || vis <> Some SynAccess.Private ) then errorR(Error(FSComp.SR.tcStaticValFieldsMustBeMutableAndPrivate(),m));
        let konst = if zeroInit then Some(Const.Zero) else None
        let rfspec = MakeRecdFieldSpec cenv env parent  (isStatic,konst,ty',attrsForProperty,attrsForField,id,isMutable,isVolatile,xmldoc,vis,m)
        match parent with
        | Parent tcref when useGenuineField tcref.Deref rfspec ->
            // Recheck the attributes for errors if the definition only generates a field
            TcAttributesWithPossibleTargets cenv env AttributeTargets.FieldDeclRestricted attrs |> ignore
        | _ -> ()
        rfspec


    let TcAnonFieldDecl cenv env parent tpenv nm (Field(attribs,stat,id,ty,isMutable,xmldoc,vis,m)) =
        let id = (match id with None -> mkSynId m nm | Some id -> id)
        TcFieldDecl cenv env parent false tpenv (stat,attribs,id,ty,isMutable,xmldoc.ToXmlDoc(),vis,m) 

    let TcNamedFieldDecl cenv env parent isIncrClass tpenv (Field(attribs,stat,id,ty,isMutable,xmldoc,vis,m)) =
        match id with 
        | None -> error (Error(FSComp.SR.tcFieldRequiresName(),m))
        | Some(id) -> TcFieldDecl cenv env parent isIncrClass  tpenv (stat,attribs,id,ty,isMutable,xmldoc.ToXmlDoc(),vis,m) 

    let TcNamedFieldDecls cenv env parent isIncrClass tpenv fields =
        fields |> List.map (TcNamedFieldDecl cenv env parent isIncrClass tpenv) 


    //-------------------------------------------------------------------------
    // Bind other elements of type definitions (constructors etc.)
    //------------------------------------------------------------------------- 

    let CheckUnionCaseName cenv realUnionCaseName m =
        CheckNamespaceModuleOrTypeName cenv (mkSynId m realUnionCaseName);
        if not (String.isUpper realUnionCaseName) && realUnionCaseName <> opNameCons && realUnionCaseName <> opNameNil then 
            errorR(NotUpperCaseConstructor(m));

    let TcUnionCaseDecl cenv env parent thisTy tpenv (UnionCase (attrs,id,args,xmldoc,vis,m)) =
        let attrs' = TcAttributes cenv env AttributeTargets.UnionCaseDecl attrs // the attributes of a union case decl get attached to the generated "static factory" method
        let vis,_ = ComputeAccessAndCompPath env None m vis parent
        let vis = CombineReprAccess parent vis
        let realUnionCaseName =  
            if id.idText = opNameCons then "Cons" 
            elif id.idText = opNameNil then "Empty"
            else id.idText
        
        CheckUnionCaseName cenv realUnionCaseName id.idRange;
        let mkName nFields i = if nFields <= 1 then "Item" else "Item"+string (i+1)
        let rfields,recordTy = 
            match args with
            | UnionCaseFields flds -> 
                let nFields = flds.Length
                let rfields = flds |> List.mapi (fun i fld -> TcAnonFieldDecl cenv env parent tpenv (mkName nFields i) fld) 
                rfields,thisTy
            | UnionCaseFullType (ty,arity) -> 
                let ty',_ = TcTypeAndRecover cenv NoNewTypars CheckCxs env tpenv ty
                let argtysl,recordTy = GetTopTauTypeInFSharpForm cenv.g (arity |> TranslateTopValSynInfo m (TcAttributes cenv env) |> TranslatePartialArity []).ArgInfos ty' m
                if argtysl.Length > 1 then 
                    errorR(Error(FSComp.SR.tcIllegalFormForExplicitTypeDeclaration(),m));   
                let argtys = argtysl |> List.concat
                let nFields = argtys.Length
                let rfields = 
                    argtys |> List.mapi (fun i (argty,argInfo) ->
                        let id = (match argInfo.Name with Some id -> id | None -> mkSynId m (mkName nFields i))
                        MakeRecdFieldSpec cenv env parent (false,None,argty,[],[],id,false,false,XmlDoc.Empty,None,m))
                if not (typeEquiv cenv.g recordTy thisTy) then 
                    error(Error(FSComp.SR.tcReturnTypesForUnionMustBeSameAsType(),m))
                rfields,recordTy
        NewUnionCase id realUnionCaseName rfields recordTy attrs' (xmldoc.ToXmlDoc()) vis


    let TcUnionCaseDecls cenv env parent thisTy tpenv unionCases =
        let unionCases' = unionCases |> List.map (TcUnionCaseDecl cenv env parent thisTy tpenv) 
        unionCases' |> CheckDuplicates (fun uc -> uc.Id) "union case" 

    let TcEnumDecl cenv env parent thisTy fieldTy (EnumCase (attrs,id,v,xmldoc,m)) =
        let attrs' = TcAttributes cenv env AttributeTargets.Field attrs
        match v with 
        | SynConst.Bytes _
        | SynConst.UInt16s _
        | SynConst.UserNum _ -> error(Error(FSComp.SR.tcInvalidEnumerationLiteral(),m))
        | _ -> 
            let v = TcConst cenv fieldTy m env v
            let vis,_ = ComputeAccessAndCompPath env None m None parent
            let vis = CombineReprAccess parent vis
            if id.idText = "value__" then errorR(Error(FSComp.SR.tcNotValidEnumCaseName(),id.idRange));
            NewRecdField true (Some v) id thisTy false false [] attrs' (xmldoc.ToXmlDoc()) vis false
      
    let TcEnumDecls cenv env parent thisTy enumCases =
        let fieldTy = NewInferenceType ()
        let enumCases' = enumCases |> List.map (TcEnumDecl cenv env parent thisTy fieldTy)  |> CheckDuplicates (fun f -> f.Id) "enum element"
        fieldTy,enumCases'

end

//-------------------------------------------------------------------------
// Bind elements of classes
//------------------------------------------------------------------------- 

let PublishInterface cenv denv (tcref:TyconRef) m compgen ty' = 
    if not (isInterfaceTy cenv.g ty') then errorR(Error(FSComp.SR.tcTypeIsNotInterfaceType1(NicePrint.prettyStringOfTy denv ty'),m));
    let tcaug = tcref.TypeContents
    if tcref.HasInterface cenv.g ty'  then 
        errorR(Error(FSComp.SR.tcDuplicateSpecOfInterface(),m));
    tcaug.tcaug_interfaces <- (ty',compgen,m) :: tcaug.tcaug_interfaces

let TcAndPublishMemberSpec cenv env containerInfo declKind tpenv memb = 
    match memb with 
    | SynMemberSig.ValField(_,m) -> error(Error(FSComp.SR.tcFieldValIllegalHere(),m))
    | SynMemberSig.Inherit(_,m) -> error(Error(FSComp.SR.tcInheritIllegalHere(),m))
    | SynMemberSig.NestedType(_,m) -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m))
    | SynMemberSig.Member(valSpfn,memberFlags,_) -> 
        TcAndPublishValSpec (cenv,env,containerInfo,declKind,Some(memberFlags),tpenv,valSpfn)
    | SynMemberSig.Interface _ -> 
        // These are done in TcTyconDefnCores
        [],tpenv

  
let TcTyconMemberSpecs cenv env containerInfo declKind tpenv (augSpfn: SynMemberSigs)  =
    let members,tpenv = List.mapFold (TcAndPublishMemberSpec cenv env containerInfo declKind) tpenv augSpfn
    List.concat members,tpenv


//-------------------------------------------------------------------------
// Bind 'open' declarations
//------------------------------------------------------------------------- 

let TcModuleOrNamespaceLidAndPermitAutoResolve env lid =
    let ad = AccessRightsOfEnv env
    match ResolveLongIndentAsModuleOrNamespace OpenQualified env.eNameResEnv ad lid  with 
    | Result res -> Result res
    | Exception err ->  raze err

let TcOpenDecl g amap m scopem env (lid : Ident list)  = 
    let modrefs = ForceRaise (TcModuleOrNamespaceLidAndPermitAutoResolve env lid)

    let IsPartiallyQualifiedNamespace (modref: ModuleOrNamespaceRef) = 
        let (CompPath(_,p)) = modref.CompilationPath 
        // Bug FSharp 1.0 3274: FSI paths don't count when determining this warning
        let p = 
            match p with 
            | [] -> []
            | (h,_):: t -> if h.StartsWith(FsiDynamicModulePrefix,System.StringComparison.Ordinal) then t else p
        modref.IsNamespace && p.Length >= lid.Length 

    modrefs |> List.iter (fun (_,modref,_) ->
       if modref.IsModule && HasAttrib g g.attrib_RequireQualifiedAccessAttribute modref.Attribs then 
           errorR(Error(FSComp.SR.tcModuleRequiresQualifiedAccess(fullDisplayTextOfModRef modref),m)))

    // Bug FSharp 1.0 3133: 'open Lexing'. Skip this warning if we successfully resolved to at least a module name
    if not (modrefs |> List.exists (fun (_,modref,_) -> modref.IsModule && not (HasAttrib g g.attrib_RequireQualifiedAccessAttribute modref.Attribs))) then
        modrefs |> List.iter (fun (_,modref,_) ->
            if IsPartiallyQualifiedNamespace modref  then 
                 errorR(Error(FSComp.SR.tcOpenUsedWithPartiallyQualifiedPath(fullDisplayTextOfModRef modref),m)))
        
    modrefs |> List.iter (fun (_,modref,_) -> CheckEntityAttributes g modref m |> CommitOperationResult);        

    let env = OpenModulesOrNamespaces g amap scopem env modrefs 
    env    


exception ParameterlessStructCtor of range

/// Incremental class definitions
module IncrClassChecking = begin

    /// Represents a single group of bindings in a class with an implicit constructor
    type IncrClassBindingGroup = 
      | IncrClassBindingGroup of Tast.Binding list * (*isStatic:*) bool* (*recursive:*) bool
      | IncrClassDo of Expr * (*isStatic:*) bool

    /// Typechecked info for implicit constructor and it's arguments 
    type IncrClassCtorLhs = 
        {/// The TyconRef for the type being defined
         TyconRef                         : TyconRef;
         /// The type parameters allocated for the implicit instance constructor. 
         /// These may be equated with other (WillBeRigid) type parameters through equirecursive inference, and so 
         /// should always be renormalized/canonicalized when used.
         InstanceCtorDeclaredTypars       : Typars;     
         /// The value representing the static implicit constructor.
         /// Lazy to ensure the static ctor value is ony published if needed.
         StaticCtorValInfo                : Lazy<(Val list * Val * ValScheme)>;
         /// The value representing the implicit constructor.
         InstanceCtorVal                  : Val;
         /// The type of the implicit constructor, representing as a ValScheme.
         InstanceCtorValScheme            : ValScheme;
         /// The values representing the arguments to the implicit constructor.
         InstanceCtorArgs                 : Val list;
         /// The reference cell holding the 'this' parameter within the implicit constructor so it can be referenced in the
         /// arguments passed to the base constructor
         InstanceCtorSafeThisValOpt       : Val option;
         /// Data indicating if safe-initialization checks need to be inserted for this type.
         InstanceCtorSafeInitInfo         : SafeInitData;
         /// The value representing the 'base' variable within the implicit instance constructor.
         InstanceCtorBaseValOpt           : Val option;
         /// The value representing the 'this' variable within the implicit instance constructor.
         InstanceCtorThisVal              : Val;
         /// The name generator used to generate the names of fields etc. within the type.
         NameGenerator                    : NiceNameGenerator;
        }
        /// Get the type parameters of the implicit constructor, after taking equi-recursive inference into account.
        member ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv denv m = 
            let ctorDeclaredTypars = ctorInfo.InstanceCtorDeclaredTypars
            let ctorDeclaredTypars = ChooseCanonicalDeclaredTyparsAfterInference cenv.g denv ctorDeclaredTypars m
            ctorDeclaredTypars

    /// Check and elaborate the "left hand side" of the implicit class construction 
    /// syntax.
    let TcImplictCtorLhsPassA(cenv, env, tpenv, tcref:TyconRef, vis, attrs, spats, thisIdOpt, baseValOpt: Val option, safeInitInfo, m) =

        // Make fresh version of the class type for type checking the members and lets *
        let isExtrinsic = false
        // These typars should not be instantiated.
        let _,ctorDeclaredTypars,_,objTy,thisTy = FreshenObjectArgType cenv m TyparWillBeRigid tcref isExtrinsic (tcref.Typars(m))

        let baseValOpt = 
            match SuperTypeOfType cenv.g cenv.amap m objTy with 
            | Some superTy -> MakeAndPublishBaseVal cenv env (match baseValOpt with None -> None | Some v -> Some v.Id) superTy
            | None -> None

        // Add class typars to env 
        let env = AddDeclaredTypars CheckForDuplicateTypars ctorDeclaredTypars env

        // Type check arguments by processing them as 'simple' patterns 
        //     NOTE: if we allow richer patterns here this is where we'd process those patterns 
        let ctorArgNames,(_,names,_) = TcSimplePatsOfUnknownType cenv true CheckCxs env tpenv (SynSimplePats.SimplePats (spats,m))
        
        // Create the values with the given names 
        let _,vspecs = MakeSimpleVals cenv env names

        if tcref.IsStructOrEnumTycon && isNil spats then 
            errorR (ParameterlessStructCtor(tcref.Range));
        
        // Put them in order 
        let ctorArgs = List.map (fun v -> NameMap.find v vspecs) ctorArgNames
        let safeThisValOpt = MakeAndPublishSafeThisVal cenv env thisIdOpt thisTy
        
        // NOTE: the type scheme here is not complete!!! The ctorTy is more or less 
        // just a type variable. The type and typars get fixed-up after inference 
        let ctorValScheme,ctorVal = 
            let argty = mkTupledTy cenv.g (typesOfVals ctorArgs)
            // Initial type has known information 
            let ctorTy = mkFunTy argty objTy    
            let attribs = TcAttributes cenv env (AttributeTargets.Constructor ||| AttributeTargets.Method) attrs
            let memberFlags      = CtorMemberFlags 
                                  
            let synArgInfos   = List.map (SynInfo.InferArgSynInfoFromSimplePat []) spats
            let valSynData = SynValInfo([synArgInfos],SynInfo.unnamedRetVal)
            let id            = ident ("new",m)

            CheckForNonAbstractInterface ModuleOrMemberBinding tcref memberFlags id.idRange;
            let memberInfo  = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,false,attribs,[],memberFlags,valSynData,id,false)
            let partialValReprInfo = TranslateTopValSynInfo m (TcAttributes cenv env) valSynData
            let prelimTyschemeG = TypeScheme(ctorDeclaredTypars,ctorTy)
            let isComplete = ComputeIsComplete ctorDeclaredTypars [] ctorTy
            let topValInfo = InferGenericArityFromTyScheme prelimTyschemeG partialValReprInfo
            let ctorValScheme = ValScheme(id,prelimTyschemeG,Some(topValInfo),Some(memberInfo),false,NeverInline,NormalVal,vis,false,true,false,false)
            let ctorVal = MakeAndPublishVal cenv env (Parent(tcref),false,ModuleOrMemberBinding,ValInRecScope(isComplete),ctorValScheme,attribs,XmlDoc.Empty,None) 
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
                CheckForNonAbstractInterface ModuleOrMemberBinding tcref ClassCtorMemberFlags id.idRange;
                let memberInfo  = MakeMemberDataAndMangledNameForMemberVal(cenv.g,tcref,false,[(*no attributes*)],[],ClassCtorMemberFlags,valSynData,id,false)
                let partialValReprInfo = TranslateTopValSynInfo m (TcAttributes cenv env) valSynData
                let prelimTyschemeG = TypeScheme(ctorDeclaredTypars,cctorTy)
                let topValInfo = InferGenericArityFromTyScheme prelimTyschemeG partialValReprInfo
                let cctorValScheme = ValScheme(id,prelimTyschemeG,Some(topValInfo),Some(memberInfo),false,NeverInline,NormalVal,None,false,true,false,false)
                 
                let cctorVal = MakeAndPublishVal cenv env (Parent(tcref),false,ModuleOrMemberBinding,ValNotInRecScope,cctorValScheme,[(* no attributes*)],XmlDoc.Empty,None) 
                cctorArgs,cctorVal,cctorValScheme)

        let thisVal = 
            // --- Create this for use inside constructor 
            let thisId  = ident ("this",m)
            let thisValScheme  = ValScheme(thisId,NonGenericTypeScheme(thisTy),None,None,false,NeverInline,CtorThisVal,None,true,false,false,false)
            let thisVal    = MakeAndPublishVal cenv env (ParentNone,false,ClassLetBinding,ValNotInRecScope,thisValScheme,[],XmlDoc.Empty,None)
            thisVal

        {TyconRef                         = tcref;
         InstanceCtorDeclaredTypars    = ctorDeclaredTypars;
         StaticCtorValInfo             = cctorValInfo;
         InstanceCtorArgs              = ctorArgs;
         InstanceCtorVal               = ctorVal;
         InstanceCtorValScheme         = ctorValScheme;
         InstanceCtorBaseValOpt        = baseValOpt;
         InstanceCtorSafeThisValOpt    = safeThisValOpt;
         InstanceCtorSafeInitInfo    = safeInitInfo;
         InstanceCtorThisVal           = thisVal;
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
        let isVolatile = HasAttrib g g.attrib_VolatileFieldAttribute v.Attribs

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
          TakenFieldNames:Set<string>;
          RepInfoTcGlobals:TcGlobals;
          /// vals mapped to representations
          ValReprs  : Zmap<Val,IncrClassValRepr>; 
          /// vals represented as fields or members from this point on 
          ValsWithRepresentation  : Val Zset; }

        static member Empty(g,names) = 
            { TakenFieldNames=Set.ofList names;
              RepInfoTcGlobals=g;
              ValReprs = Zmap.empty valOrder; 
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

                    let ctorDeclaredTypars = ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv env.DisplayEnv ctorInfo.TyconRef.Range
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
                        ValReprInfo(tpNames@ValReprInfo.InferTyparInfo(ctorDeclaredTypars), args, ret)
                                          
                    //let synArgInfos   = List.map (SynInfo.InferArgSynInfoFromSimplePat []) spats
                    //let valSynData = SynValInfo([synArgInfos],SynInfo.unnamedRetVal)

                    let prelimTyschemeG = TypeScheme(ctorDeclaredTypars@tps,memberTauTy)
                    let memberValScheme = ValScheme(id,prelimTyschemeG,Some(topValInfo),Some(memberInfo),false,NeverInline,NormalVal,None,true (* isCompilerGenerated *) ,true (* isIncrClass *) ,false, false)
                    let methodVal = MakeAndPublishVal cenv env (Parent(tcref),false,ModuleOrMemberBinding,ValNotInRecScope,memberValScheme,v.Attribs,XmlDoc.Empty,None) 
                    reportIfUnused()
                    InMethod(isStatic,methodVal,topValInfo)

            repr, takenFieldNames

        /// Extend the known local representations by choosing a representation for a binding
        member localRep.ChooseAndAddRepresentation(cenv,env: TcEnv,isStatic,isCtorArg,ctorInfo:IncrClassCtorLhs,staticForcedFieldVars:FreeLocals,instanceForcedFieldVars: FreeLocals,bind:Binding) = 
            let v = bind.Var
            let repr,takenFieldNames = localRep.ChooseRepresentation (cenv,env,isStatic,isCtorArg,ctorInfo,staticForcedFieldVars,instanceForcedFieldVars,localRep.TakenFieldNames,bind )
            // OK, representation chosen, now add it 
            {localRep with 
                TakenFieldNames=takenFieldNames; 
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
        member localRep.PublishIncrClassFields cenv denv cpath (ctorInfo:IncrClassCtorLhs) safeStaticInitInfo =    
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
            tcref.Deref.Data.entity_tycon_repr <- Some (TFsObjModelRepr { tcref.FSharpObjectModelTypeInfo with fsobjmodel_rfields = recdFields})  


        /// Given localRep saying how locals have been represented, e.g. as fields.
        /// Given an expr under a given thisVal context.
        //
        /// Fix up the references to the locals, e.g. 
        ///     v -> this.fieldv
        ///     f x -> this.method x
        member localRep.FixupIncrClassExprPassC thisValOpt safeStaticInitInfo (thisTyInst:TypeInst) expr = 
            // fixup: intercept and expr rewrite
            let FixupExprNode e =
                //dprintfn "Fixup %s" (showL (exprL e))
                match e with
                // Rewrite references to generic methods
                | Expr.App(Expr.Val (ValDeref(v),_,_),_,tyargs,args,m) 
                    when (localRep.IsValWithRepresentation(v) &&
                          (match localRep.LookupRepr(v) with 
                           | InMethod (_,methodVal,_)  -> (methodVal.Typars.Length > thisTyInst.Length)
                           | _ -> false )) -> 

                        //dprintfn "Found application of %s" v.LogicalName
                        let g = localRep.RepInfoTcGlobals
                        let expr = localRep.MakeValueLookup thisValOpt thisTyInst safeStaticInitInfo v tyargs m
                        Some (MakeApplicationAndBetaReduce g (expr,(tyOfExpr g expr),[],args,m)) 
                        

                // Rewrite references to values stored as fields and non-generic methods
                | Expr.Val (ValDeref(v),_,m)                         
                    when (localRep.IsValWithRepresentation(v) &&
                          (match localRep.LookupRepr(v) with 
                           | InMethod (_,methodVal,_) -> (methodVal.Typars.Length <= thisTyInst.Length)
                           | _ -> true)) -> 

                        //dprintfn "Found use of %s" v.LogicalName
                        Some (localRep.MakeValueLookup thisValOpt thisTyInst safeStaticInitInfo v [] m)

                // Rewrite assignments to mutable values stored as fields 
                | Expr.Op(TOp.LValueOp (LSet, ValDeref(v))    ,[],[arg],m) 
                    when localRep.IsValWithRepresentation(v) ->
                        Some (localRep.MakeValueAssign thisValOpt thisTyInst safeStaticInitInfo v arg m)

                // Rewrite taking the address of mutable values stored as fields 
                | Expr.Op(TOp.LValueOp (LGetAddr,ValDeref(v)),[],[]   ,m) 
                    when localRep.IsValWithRepresentation(v) ->
                        Some (localRep.MakeValueGetAddress thisValOpt thisTyInst safeStaticInitInfo v m)

                | _ -> None
            Tastops.RewriteExpr { PreIntercept= None; 
                                   PostTransform = FixupExprNode;
                                   IsUnderQuotations=true } expr 


    type IncrClassConstructionBindingsPassC =
      | PassCBindings of IncrClassBindingGroup list
      | PassCCtorJustAfterSuperInit     
      | PassCCtorJustAfterLastLet    

    /// Given a set of 'let' bindings (static or not, recursive or not) that make up a class, 
    /// generate their initialization expression(s).  
    let MakeCtorForIncrClassConstructionPassC 
               (cenv,
                env: TcEnv,
                _tpenv ,
                /// The lhs information about the implicit constructor
                ctorInfo:IncrClassCtorLhs,
                /// The call to the super class constructor
                inheritsExpr,
                /// Should we place a sequence point at the 'inheritedTys call?
                inheritsIsVisible,
                /// The declarations
                decs : IncrClassConstructionBindingsPassC list,
                memberBinds : Binding list,
                /// Record any unconstrained type parameters generalized for the outer members as "free choices" in the let bindings 
                generalizedTyparsForRecursiveBlock,
                safeStaticInitInfo : SafeInitData) = 


        let denv = env.DisplayEnv 
        let thisVal      = ctorInfo.InstanceCtorThisVal 

        let m = thisVal.Range
        let ctorDeclaredTypars = ctorInfo.GetNormalizedInstanceCtorDeclaredTypars cenv denv m

        ctorDeclaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m) ; 

        // Reconstitute the type with the correct quantified type variables.
        ctorInfo.InstanceCtorVal.SetType (tryMkForallTy ctorDeclaredTypars ctorInfo.InstanceCtorVal.TauType)

        let freeChoiceTypars = ListSet.subtract typarEq generalizedTyparsForRecursiveBlock ctorDeclaredTypars

        let thisTyInst = List.map mkTyparTy ctorDeclaredTypars

        let acc_freeInExpr acc expr =
            unionFreeVars acc (freeInExpr CollectLocalsNoCaching expr) 
            
        let acc_free_in_binding acc (bind:Binding) = 
            acc_freeInExpr acc bind.Expr
            
        let acc_free_in_bindings acc (binds:Binding list) = 
            (acc,binds) ||> List.fold acc_free_in_binding

        // Find all the variables used in any method. These become fields.
        //   staticForcedFieldVars:FreeLocals: the vars forced to be fields due to static member bindings, instance initialization expressions or instance member bindings
        //   instanceForcedFieldVars: FreeLocals: the vars forced to be fields due to instance member bindings
                                            
        let staticForcedFieldVars,instanceForcedFieldVars = 
             let (staticForcedFieldVars,instanceForcedFieldVars) = 
                 ((emptyFreeVars,emptyFreeVars),decs) ||> List.fold (fun (staticForcedFieldVars,instanceForcedFieldVars) dec -> 
                    match dec with 
                    | PassCCtorJustAfterLastLet
                    | PassCCtorJustAfterSuperInit ->  
                        (staticForcedFieldVars,instanceForcedFieldVars)
                    | PassCBindings decs ->
                        ((staticForcedFieldVars,instanceForcedFieldVars),decs) ||> List.fold (fun (staticForcedFieldVars,instanceForcedFieldVars) dec -> 
                            match dec with 
                            | IncrClassBindingGroup(binds,isStatic,_) -> 
                                let methodBinds = binds |> List.filter (IncrClassReprInfo.IsMethodRepr cenv) 
                                let staticForcedFieldVars = 
                                    if isStatic then 
                                        // Any references to static variables in any static method force the variable to be represented as a field
                                        (staticForcedFieldVars,methodBinds) ||> acc_free_in_bindings
                                    else
                                        // Any references to static variables in any instance bindings force the variable to be represented as a field
                                        (staticForcedFieldVars,binds) ||> acc_free_in_bindings
                                        
                                let instanceForcedFieldVars = 
                                    // Any references to instance variables in any methods force the variable to be represented as a field
                                    (instanceForcedFieldVars,methodBinds) ||> acc_free_in_bindings
                                        
                                (staticForcedFieldVars,instanceForcedFieldVars)
                            | IncrClassDo (e,isStatic) -> 
                                let staticForcedFieldVars = 
                                    if isStatic then 
                                        staticForcedFieldVars
                                    else
                                        unionFreeVars staticForcedFieldVars (freeInExpr CollectLocalsNoCaching e)
                                (staticForcedFieldVars,instanceForcedFieldVars)))
             let staticForcedFieldVars  = (staticForcedFieldVars,memberBinds) ||> acc_free_in_bindings 
             let instanceForcedFieldVars = (instanceForcedFieldVars,memberBinds) ||> acc_free_in_bindings 
             
             // Any references to static variables in the 'inherits' expression force those static variables to be represented as fields
             let staticForcedFieldVars = (staticForcedFieldVars,inheritsExpr) ||> acc_freeInExpr

             (staticForcedFieldVars.FreeLocals,instanceForcedFieldVars.FreeLocals)


        // Compute the implicit construction side effects of single 
        // 'let' or 'let rec' binding in the implicit class construction sequence 
        let TransBind (reps:IncrClassReprInfo) (TBind(v,rhsExpr,spBind)) =
            // move to CheckMembersForm?? 
            if v.MustInline then
                error(Error(FSComp.SR.tcLocalClassBindingsCannotBeInline(),v.Range));
            let rhsExpr = reps.FixupIncrClassExprPassC (Some thisVal) safeStaticInitInfo thisTyInst rhsExpr
            
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
                            let setExpr = reps.FixupIncrClassExprPassC (Some(thisVal)) NoSafeInitInfo thisTyInst setExpr
                            Some setExpr
                        | NoSafeInitInfo -> 
                            None
                    else
                        None

                (isPriorToSuperInit, (fun e -> 
                     let e = match adjustSafeInitFieldExprOpt with None -> e | Some ae -> mkCompGenSeq m ae e
                     mkSeq SequencePointsAtSeq m assignExpr e)), []

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
                          if debug then dprintf "TransDec: %d bindings, isRec=%b\n" binds.Length isRec;
                          let actions,methodBinds = binds |> List.map (TransBind reps)  |> List.unzip
                          let reps     = (reps,binds) ||> List.fold (fun rep bind -> rep.ValNowWithRepresentation bind.Var) // inscope after
                          actions,reps,methodBinds
                  let methodBinds = List.concat methodBinds
                  if isStatic then 
                      (actions,[],methodBinds),reps
                  else 
                      ([],actions,methodBinds),reps

              | IncrClassDo (doExpr,isStatic) -> 
                  let doExpr = reps.FixupIncrClassExprPassC (Some(thisVal)) safeStaticInitInfo thisTyInst doExpr
                  let binder = (fun e -> mkSeq SequencePointsAtSeq doExpr.Range doExpr e)
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
            | PassCCtorJustAfterSuperInit ->  
                let binders = 
                    [ match ctorInfo.InstanceCtorSafeThisValOpt with 
                      | None ->  ()
                      | Some v -> 
                        let setExpr = mkRefCellSet cenv.g m ctorInfo.InstanceCtorThisVal.Type (exprForVal m v) (exprForVal m ctorInfo.InstanceCtorThisVal)
                        let setExpr = reps.FixupIncrClassExprPassC (Some(thisVal)) safeStaticInitInfo thisTyInst setExpr
                        let binder = (fun e -> mkSeq SequencePointsAtSeq setExpr.Range setExpr e)
                        let isPriorToSuperInit = false
                        yield (isPriorToSuperInit,binder) ]

                ([],binders,[]),reps

            // The last 'let' binding is done so we can set the initialization condition for the collection of object fields
            // which now allows members to be called.
            | PassCCtorJustAfterLastLet ->  
                let binders = 
                    [ match ctorInfo.InstanceCtorSafeInitInfo with 
                      | SafeInitField (rfref, _) ->  
                        let setExpr = mkRecdFieldSet cenv.g (exprForVal m thisVal, rfref, thisTyInst, mkOne cenv.g m, m)
                        let setExpr = reps.FixupIncrClassExprPassC (Some(thisVal)) safeStaticInitInfo thisTyInst setExpr
                        let binder = (fun e -> mkSeq SequencePointsAtSeq setExpr.Range setExpr e)
                        let isPriorToSuperInit = false
                        yield (isPriorToSuperInit,binder)  
                      | NoSafeInitInfo ->  
                        () ]

                ([],binders,[]),reps
                
            | PassCBindings decs -> 
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
        let cctorInitActions = cctorInitActions1 @  List.concat cctorInitActions2
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
                //    let inheritsExpr = reps.FixupIncrClassExprPassC (Some(thisVal)) thisTyInst inheritsExpr
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
                        let ldexpr = reps.FixupIncrClassExprPassC (Some(thisVal)) safeStaticInitInfo thisTyInst (exprForVal m v) 
                        mkInvisibleLet m v ldexpr inheritsExpr

                let spAtSuperInit = (if inheritsIsVisible then SequencePointsAtSeq else SuppressSequencePointOnExprOfSequential)
                mkSeq spAtSuperInit m inheritsExpr ctorBody

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

end


// Checking of members and 'let' bindings in classes
module TyconBindingChecking = begin 

    open IncrClassChecking 

    // Technique: multiple passes.
    //   - create val_specs for recursive items given names and args
    //   - type check AST to TAST collecting (sufficient) type constraints
    //   - determine typars to generalize over
    //   - generalize definitions (fixing up recursive instances)
    //   - build ctor binding
    //   - Yields set of recursive bindings for the ctors and members of the types.

    /// Represents one element in a type definition, during the first phase    
    type TyconBindingsPassA =
      | PassAIncrClassCtor     of IncrClassCtorLhs
      | PassAInherit           of SynType * SynExpr * (*base:*)Val option * range
      | PassAIncrClassBindings of TyconRef * Ast.SynBinding list * (* isStatic:*) bool * (*recursive:*) bool * range
      | PassAMember            of PreCheckingRecursiveBinding
      | PassAOpen              of LongIdent * range
      // Indiates the super init has just been called, 'this' may now be published
      | PassAIncrClassCtorJustAfterSuperInit 
      // Indiates the last 'field' has been initialized, only 'do' comes after 
      | PassAIncrClassCtorJustAfterLastLet

    /// Represents one element in a type definition, during the second phase
    type TyconBindingsPassB =
      | PassBIncrClassCtor     of IncrClassCtorLhs * Tast.Binding option 
      | PassBInherit           of Expr * Val option
      | PassBIncrClassBindings of IncrClassBindingGroup list
      | PassBMember            of int
      // Indicates the last 'field' has been initialized, only 'do' comes after 
      | PassBIncrClassCtorJustAfterSuperInit
      | PassBIncrClassCtorJustAfterLastLet
      | PassBOpen              of LongIdent * range

    /// Represents one element in a type definition, during the third phase
    type TyconBindingsPassC =
      | PassCIncrClassCtor     of IncrClassCtorLhs * Tast.Binding option 
      | PassCInherit           of Expr * Val option
      | PassCIncrClassBindings of IncrClassBindingGroup list
      | PassCMember            of PreInitializationGraphEliminationBinding
      | PassCOpen              of LongIdent * range
      // Indicates the last 'field' has been initialized, only 'do' comes after 
      | PassCIncrClassCtorJustAfterSuperInit     
      | PassCIncrClassCtorJustAfterLastLet     


    /// Main routine
    let TcTyconBindings cenv (env: TcEnv) tpenv bindsm scopem (bindsl : TyconBindingDefns list) =
        let ad = AccessRightsOfEnv env
        let denv = env.DisplayEnv
        let envInitial = env
        let env = () // hide this to make sure it is not used inadvertently
        env |> ignore // mark it as used
        
        // PassA: create member prelimRecValues for "recursive" items, i.e. ctor val and member vals 
        // PassA: also processes their arg patterns - collecting type assertions 
        let defnsAs, (tpenv,_,prelimRecValuesRev,uncheckedBindsRev) =

            ((tpenv,0,[],[]),bindsl) 
              ||> List.mapFold (fun (tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev) (TyconBindingDefns(tcref, declKind, binds)) -> 

                // Class members can access protected members of the implemented type 
                // Class members can access private members in the typ
                let isExtrinsic = (declKind = ExtrinsicExtensionBinding)
                let envForTycon = MakeInnerEnvForTyconRef cenv envInitial tcref isExtrinsic 


                // Re-add the type constructor to make it take precedence for record label field resolutions
                // This does not apply to extension members: in those cases the relationship between the record labels
                // and the type is too extruded
                let envForTycon = 
                    if isExtrinsic then envForTycon
                    else AddLocalTyconRefs true cenv.g cenv.amap tcref.Range [tcref] envForTycon

                let defnAs,(_,envForTycon,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev) = 
                    ((None,envForTycon,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev),binds) 
                      ||> List.collectFold (fun (incrClassCtorLhsOpt,env,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev) 
                                                (TyconBindingDefn(containerInfo,newslotsOK,declKind,classMemberDef,m)) ->

                        if tcref.IsTypeAbbrev then error(Error(FSComp.SR.tcTypeAbbreviationsMayNotHaveMembers(),(trimRangeToLine m)));
                        if tcref.IsEnumTycon then error(Error(FSComp.SR.tcEnumerationsMayNotHaveMembers(),(trimRangeToLine m)));

                        match classMemberDef, containerInfo with
                        
                          | SynMemberDefn.ImplicitCtor (vis,attrs,spats,thisIdOpt, m), ContainerInfo(_,Some(MemberOrValContainerInfo(tcref, _, baseValOpt, safeInitInfo, _))) ->
                              match tcref.TypeOrMeasureKind with KindMeasure -> error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m)) | _ -> ();

                              // PassA: make incrClassCtorLhs - ctorv, thisVal etc, type depends on argty(s) 
                              let incrClassCtorLhs = TcImplictCtorLhsPassA(cenv,env,tpenv,tcref,vis,attrs,spats,thisIdOpt,baseValOpt,safeInitInfo,m)
                              // PassA: Add ctorDeclaredTypars from incrClassCtorLhs - or from tcref 
                              let env = AddDeclaredTypars CheckForDuplicateTypars incrClassCtorLhs.InstanceCtorDeclaredTypars env
                              [PassAIncrClassCtor incrClassCtorLhs],
                              (Some(incrClassCtorLhs),env,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev)
                              
                          | SynMemberDefn.ImplicitInherit (typ,arg,_baseIdOpt,m),_ ->
                              match tcref.TypeOrMeasureKind with KindMeasure -> error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m)) | _ -> ();
                              // PassA: inherit typ(arg) as base - pass through 
                              // PassA: pick up baseValOpt! 
                              let baseValOpt = incrClassCtorLhsOpt |> Option.bind (fun x -> x.InstanceCtorBaseValOpt)
                              [PassAInherit (typ,arg,baseValOpt,m); PassAIncrClassCtorJustAfterSuperInit],   
                              (incrClassCtorLhsOpt,env,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev)
                              
                          | SynMemberDefn.LetBindings (letBinds,isStatic,isRec,m),_ ->
                              match tcref.TypeOrMeasureKind,isStatic with KindMeasure,false -> error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m)) | _,_ -> ();

                              if tcref.IsStructOrEnumTycon && not isStatic then 
                                   let allDo = letBinds |> List.forall (function (Binding(_,DoBinding,_,_,_,_,_,_,_,_,_,_)) -> true | _ -> false)
                                   if allDo then 
                                      errorR(Deprecated(FSComp.SR.tcStructsMayNotContainDoBindings(),(trimRangeToLine m)));
                                   else
                                      errorR(Error(FSComp.SR.tcStructsMayNotContainLetBindings(),(trimRangeToLine m)));

                              if isStatic && isNone incrClassCtorLhsOpt then 
                                  errorR(Error(FSComp.SR.tcStaticLetBindingsRequireClassesWithImplicitConstructors(),m));
                              
                              // PassA: let-bindings - pass through 
                              [PassAIncrClassBindings (tcref,letBinds,isStatic,isRec,m)],
                              (incrClassCtorLhsOpt,env,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev)     
                              
                          | SynMemberDefn.Member (bind,m),_ ->
                              // PassA: member binding - create prelim valspec (for recursive reference) and RecursiveBindingInfo 
                              let (NormalizedBinding(_,_,_,_,_,_,_,valSynData,_,_,_,_)) as bind = BindingNormalization.NormalizeBinding ValOrMemberBinding cenv env bind
                              let (SynValData(memberFlagsOpt,_,_)) = valSynData 
                              match tcref.TypeOrMeasureKind with
                              | KindType -> ()
                              | KindMeasure ->
                                (match memberFlagsOpt with 
                                | None -> () 
                                | Some memberFlags -> 
                                  if memberFlags.IsInstance then error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembers(), m));
                                  match memberFlags.MemberKind with 
                                  | MemberKind.Constructor -> error(Error(FSComp.SR.tcMeasureDeclarationsRequireStaticMembersNotConstructors(), m))
                                  | _ -> ()
                                );
                              let rbind = NormalizedRecBindingDefn(containerInfo,newslotsOK,declKind,bind)
                              let overridesOK  = DeclKind.CanOverrideOrImplement(declKind)
                              let (binds,values),(tpenv,recBindIdx) = AnalyzeAndMakeRecursiveValue overridesOK cenv env (tpenv,recBindIdx) rbind
                              let cbinds = [ for rbind in binds -> PassAMember rbind ]
                              cbinds,(incrClassCtorLhsOpt, env, tpenv, recBindIdx, List.rev values @ prelimRecValuesRev,List.rev binds @ uncheckedBindsRev)
                        
                          | SynMemberDefn.Open (mp,m),_ ->
                              [ PassAOpen (mp,m) ],
                              (incrClassCtorLhsOpt,env,tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev)
                        
                          | _ -> 
                              error(InternalError("Unexpected definition",m)))

                // If no constructor call, insert PassAIncrClassCtorJustAfterSuperInit at start
                let defnAs = 
                    match defnAs with 
                    | (PassAIncrClassCtor _ as b1) :: rest -> 
                        let rest = 
                            if rest |> List.exists (function PassAIncrClassCtorJustAfterSuperInit -> true | _ -> false) then 
                                rest
                            else
                                PassAIncrClassCtorJustAfterSuperInit :: rest
                        // Insert PassAIncrClassCtorJustAfterLastLet at the point where local construction is known to have been finished 
                        let rest = 
                            let isBefore = function
                                | PassAOpen _ | PassAIncrClassCtor _ | PassAInherit _ | PassAIncrClassCtorJustAfterSuperInit -> true
                                | PassAIncrClassBindings (_,binds,_,_,_) -> binds |> List.exists (function (Binding (_,DoBinding,_,_,_,_,_,_,_,_,_,_)) -> false | _ -> true)
                                | PassAIncrClassCtorJustAfterLastLet
                                | PassAMember _ -> false
                            
                            [ yield!  rest |> Seq.takeWhile isBefore
                              yield PassAIncrClassCtorJustAfterLastLet
                              yield! rest  |> Seq.skipWhile isBefore ]
                        b1 :: rest

                    // Cover the case where this is not a type with an implicit constructor.
                    | rest -> rest

                (envForTycon,tcref,defnAs),(tpenv,recBindIdx,prelimRecValuesRev,uncheckedBindsRev))


        let prelimRecValues = List.rev prelimRecValuesRev
        let uncheckedRecBinds = List.rev uncheckedBindsRev
        let uncheckedRecBindsTable = uncheckedRecBinds  |> List.map (fun rbind  ->  rbind.RecBindingInfo.Val.Stamp, rbind) |> Map.ofList 

        // PassB: type check pass, convert from ast to tast and collects type assertions 

        let defnsBs,(tpenv,generalizedRecBinds,preGeneralizationRecBinds,_,_) = 


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
             
            ((tpenv,[],[],uncheckedRecBindsTable,envInitial),defnsAs)
              ||> List.mapFold (fun (tpenv,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable,envNonRec) (envForTycon, tcref, defnAs) -> 
                // Add prelimRecValues to env (breaks recursion) and vrec=true 
                let envForTycon = AddLocalVals scopem prelimRecValues envForTycon
                
                // Set up the environment so use-before-definition warnings are given, at least 
                // until we reach a PassAIncrClassCtorJustAfterSuperInit. 
                let envForTycon = { envForTycon with eCtorInfo = Some (InitialImplicitCtorInfo());  }
                
                // Loop through the definition elements in a type...
                let defnBs,(tpenv,_,_,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable,_) = 
                    ((tpenv,envForTycon,envForTycon,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable,None),defnAs) 
                      ||> List.mapFold  (fun (tpenv,envInstance,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable,incrClassCtorLhsOpt) defnA -> 

                        match defnA with
                        | PassAIncrClassCtor incrClassCtorLhs ->

                            // Enrich envInstance with implicit ctor args 
                            let envInstance = match incrClassCtorLhs.InstanceCtorSafeThisValOpt with Some v -> AddLocalVal scopem v envInstance | None -> envInstance
                            let envInstance = List.foldBack AddLocalValPrimitive incrClassCtorLhs.InstanceCtorArgs envInstance 
                            let envNonRec = match incrClassCtorLhs.InstanceCtorSafeThisValOpt with Some v -> AddLocalVal scopem v envNonRec | None -> envNonRec
                            let envNonRec = List.foldBack AddLocalValPrimitive incrClassCtorLhs.InstanceCtorArgs envNonRec
                            let safeThisValBindOpt = TcLetrecComputeCtorSafeThisValBind cenv incrClassCtorLhs.InstanceCtorSafeThisValOpt
                            PassBIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt),
                            (tpenv,envInstance,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable, Some incrClassCtorLhs)
                            
                        // PassB: build new object expr for the inherit-call 
                        | PassAInherit (sty,arg,baseValOpt,m) ->
                            let ty,tpenv = TcType cenv NoNewTypars CheckCxs envInstance tpenv sty
                            let inheritsExpr,tpenv = TcNewExpr cenv envInstance tpenv ty (Some sty.Range) true arg m
                            let envInstance = match baseValOpt with Some baseVal -> AddLocalVal scopem baseVal envInstance | None -> envInstance
                            let envNonRec   = match baseValOpt with Some baseVal -> AddLocalVal scopem baseVal envNonRec   | None -> envNonRec
                            PassBInherit (inheritsExpr,baseValOpt),
                            (tpenv,envInstance,envStatic,envNonRec,generalizedRecBinds,preGeneralizationRecBinds,uncheckedRecBindsTable,incrClassCtorLhsOpt)
                            
                        // PassB: let bindings 
                        | PassAIncrClassBindings (tcref,binds,isStatic,isRec,bindsm) ->
                            let envForBinding = if isStatic then envStatic else envInstance
                            let binds,bindRs,env,tpenv = 
                                if isRec then
                                
                                    // Type check local recursive binding 
                                    let binds = binds |> List.map (fun bind -> RecBindingDefn(ExprContainerInfo,NoNewSlots,ClassLetBinding,bind))
                                    let binds,env,tpenv = TcLetrec ErrorOnOverrides cenv envForBinding tpenv (binds,scopem(*bindsm*),scopem)
                                    let bindRs = [IncrClassBindingGroup(binds,isStatic,true)]
                                    binds,bindRs,env,tpenv 
                                else

                                    // Type check local binding 
                                    let binds,env,tpenv = TcLetBindings cenv envForBinding ExprContainerInfo ClassLetBinding tpenv (binds,bindsm,scopem)
                                    let binds,bindRs = 
                                        binds 
                                        |> List.map (function
                                            | TMDefLet(bind,_) -> [bind],IncrClassBindingGroup([bind],isStatic,false)
                                            | TMDefDo(e,_) -> [],IncrClassDo(e,isStatic)
                                            | _ -> error(InternalError("unexpected definition kind",tcref.Range)))
                                        |> List.unzip
                                    List.concat binds,bindRs,env,tpenv

                            let envNonRec = (envNonRec,binds) ||> List.fold (fun acc bind -> AddLocalValPrimitive bind.Var acc)

                            // Check to see that local bindings and members don't have the same name
                            for bind in binds do 
                                let nm = bind.Var.DisplayName
                                let ty = generalizedTyconRef tcref
                                match TryFindIntrinsicMethInfo cenv.infoReader bind.Var.Range ad nm ty,
                                      TryFindPropInfo cenv.infoReader bind.Var.Range ad nm ty with 
                                | [],[] -> ()
                                | _ -> errorR (Error(FSComp.SR.tcMemberAndLocalClassBindingHaveSameName(nm),bind.Var.Range));

                            // Also add static entries to the envInstance if necessary 
                            let envInstance = (if isStatic then (binds,envInstance) ||> List.foldBack (fun b e -> AddLocalVal scopem b.Var e)  else env)
                            let envStatic = (if isStatic then env else envStatic)
                            PassBIncrClassBindings bindRs,
                            (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, incrClassCtorLhsOpt)
                              
                        | PassAIncrClassCtorJustAfterSuperInit -> 
                            PassBIncrClassCtorJustAfterSuperInit, 
                            (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, incrClassCtorLhsOpt)
                            
                        | PassAIncrClassCtorJustAfterLastLet -> 
                            PassBIncrClassCtorJustAfterLastLet , 
                            (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, incrClassCtorLhsOpt)
                            
                            
                        | PassAOpen(mp,m) -> 
                            let envInstance = TcOpenDecl cenv.g cenv.amap m scopem envInstance mp
                            let envStatic = TcOpenDecl cenv.g cenv.amap m scopem envStatic mp
                            PassBOpen(mp,m),
                            (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, incrClassCtorLhsOpt)

                        // Note: this path doesn't add anything the environment, because the member is already available off via its type 
                        
                        | PassAMember rbind ->

                            // PassB: Typecheck member binding, generalize them later, when all type constraints are known 
                            // static members are checked under envStatic.
                            // envStatic contains class typars and the (ungeneralized) members on the class(es).
                            // envStatic has no instance-variables (local let-bindings or ctor args). 

                            let v = rbind.RecBindingInfo .Val
                            let envForBinding = if v.IsInstanceMember then envInstance else envStatic

                            // Type variables derived from the implicit constructor are always generalizable (we check their generalizability later).
                            // Note they may be solved to be equi-recursive.
                            let extraGeneralizableTypars = 
                                match incrClassCtorLhsOpt with 
                                | None -> rbind.RecBindingInfo.EnclosingDeclaredTypars  
                                | Some incrClassCtorLhs -> incrClassCtorLhs.InstanceCtorDeclaredTypars

                            let reqdThisValTy = 
                                match incrClassCtorLhsOpt with 
                                | None -> None
                                | Some incrClassCtorLhs -> Some incrClassCtorLhs.InstanceCtorThisVal.Type

                            // Type check the member and apply early generalization.
                            // We ignore the tpenv returned by checking each member. Each member gets checked in a fresh, clean tpenv
                            let (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, _, uncheckedRecBindsTable) = 
                                TcLetrecBinding (cenv,envForBinding,scopem,extraGeneralizableTypars,reqdThisValTy) (envNonRec, generalizedRecBinds, preGeneralizationRecBinds, tpenv, uncheckedRecBindsTable) rbind
                             
                            //let tpenv = HideUnscopedTypars  generalizedTypars tpenv
                            
                            PassBMember rbind.RecBindingInfo.Index ,
                            (tpenv, envInstance, envStatic, envNonRec, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, incrClassCtorLhsOpt))
                
                (tcref, defnBs), (tpenv, generalizedRecBinds, preGeneralizationRecBinds, uncheckedRecBindsTable, envNonRec))

        // There should be no bindings that have not been generalized since checking the vary last binding always
        // results in the generalization of all remaining ungeneralized bindings, since there are no remaining unchecked bindings
        // to prevent the generalization 
        assert preGeneralizationRecBinds.IsEmpty

        // Now that we know what we've generalized we can adjust the recursive references 
        let generalizedTyparsForRecursiveBlock = 
             generalizedRecBinds 
                |> List.map (fun pgrbind -> pgrbind.GeneralizedTypars)
                |> unionGeneralizedTypars

        // Check the escape condition for all extraGeneralizableTypars.
        // First collect up all the extraGeneralizableTypars.
        let allExtraGeneralizableTypars = 
            [ for (_, _, defnAs) in defnsAs do
                  for defnA in defnAs do 
                      match defnA with
                      | PassAIncrClassCtor incrClassCtorLhs ->
                           yield! incrClassCtorLhs.InstanceCtorDeclaredTypars 
                      | PassAMember rbind ->
                           yield! rbind.RecBindingInfo.EnclosingDeclaredTypars
                      | _ -> 
                           ()   ]
        // Now check they don't escape the overall scope of the recursive set of types
        if nonNil allExtraGeneralizableTypars then         
            let freeInInitialEnv = GeneralizationHelpers.ComputeUngeneralizableTypars envInitial
            for extraTypar in allExtraGeneralizableTypars do 
                if Zset.memberOf freeInInitialEnv extraTypar then
                    let ty =  mkTyparTy extraTypar
                    error(Error(FSComp.SR.tcNotSufficientlyGenericBecauseOfScope(NicePrint.prettyStringOfTy denv ty),extraTypar.Range));                                

        // Build an index ---> binding map
        let generalizedBindingsMap = generalizedRecBinds |> List.map (fun pgrbind -> (pgrbind.RecBindingInfo.Index, pgrbind)) |> Map.ofList

          
        let defnsCs,tpenv = 
            (tpenv, defnsBs) ||> List.mapFold (fun tpenv (tcref, defnBs) -> 

                let defnCs, tpenv = 
                    (tpenv,defnBs) ||> List.mapFold (fun tpenv defnB -> 

                        // PassC: Generalise implicit ctor val 
                        match defnB with
                        | PassBIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt) ->
                            let valscheme = incrClassCtorLhs.InstanceCtorValScheme
                            let valscheme = ChooseCanonicalValSchemeAfterInference cenv.g denv valscheme scopem
                            AdjustRecType cenv incrClassCtorLhs.InstanceCtorVal valscheme;
                            PassCIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt),tpenv

                        | PassBInherit (inheritsExpr,basevOpt) -> 
                            PassCInherit (inheritsExpr,basevOpt),tpenv

                        | PassBIncrClassBindings bindRs             -> 
                            PassCIncrClassBindings bindRs,tpenv

                        | PassBIncrClassCtorJustAfterSuperInit -> 
                            PassCIncrClassCtorJustAfterSuperInit, tpenv

                        | PassBIncrClassCtorJustAfterLastLet -> 
                            PassCIncrClassCtorJustAfterLastLet, tpenv

                        | PassBOpen(mp,m) -> 
                            PassCOpen(mp,m), tpenv

                        | PassBMember idx  ->
                            // PassC: Fixup member bindings 
                            let generalizedBinding = generalizedBindingsMap.[idx] 
                            let vxbind = TcLetrecAdjustMemberForSpecialVals cenv generalizedBinding
                            let pgbrind = FixupLetrecBind cenv envInitial.DisplayEnv generalizedTyparsForRecursiveBlock  vxbind
                            PassCMember pgbrind,
                            tpenv)
                (tcref,defnCs), tpenv)
                        
        // --- Extract local vals from let-bindings 
        let fixupValueExprBinds,methodBinds =
            defnsCs |> List.map (fun (tcref,defnCs) -> 
                match defnCs with 

                // Cover the case where this is not a class with an implicit constructor
                | PassCIncrClassCtor (incrClassCtorLhs, safeThisValBindOpt) :: defnCs -> 


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

                        let needsSafeStaticInit = not cenv.g.compilingFslib
                        
                        // We only need safe static init checks if there are some static field bindings (actually, we look for non-method bindings)
                        let hasStaticBindings = 
                            defnCs |> List.exists (function 
                                | PassCIncrClassBindings groups -> 
                                    groups |> List.exists (function 
                                        | IncrClassBindingGroup(binds,isStatic,_) -> 
                                            let nonMethodBinds = binds |> List.filter (IncrClassReprInfo.IsMethodRepr cenv >> not) 
                                            isStatic && not nonMethodBinds.IsEmpty 
                                        | _ -> false) 
                                | _ -> false)

                        if needsSafeStaticInit && hasStaticBindings then 
                            let rfield =  MakeSafeInitField cenv.g envInitial tcref.Range true
                            SafeInitField(mkRecdFieldRef tcref rfield.Name, rfield)
                        else
                            NoSafeInitInfo


                    // This is the type definition we're processing  
                    let tcref = incrClassCtorLhs.TyconRef

                    // Assumes inhert call immediately follows implicit ctor. Checked by CheckMembersForm 
                    let inheritsExpr,inheritsIsVisible,_,defnCs = 
                        match defnCs |> List.partition (function PassCInherit _ -> true | _ -> false) with
                        | [PassCInherit (inheritsExpr,baseValOpt)], defnCs -> 
                            inheritsExpr,true,baseValOpt,defnCs

                        | _ ->
                            if tcref.IsStructOrEnumTycon then 
                                mkUnit cenv.g tcref.Range, false,None, defnCs
                            else
                                let inheritsExpr,_ = TcNewExpr cenv envInitial tpenv cenv.g.obj_ty None true (SynExpr.Const(SynConst.Unit,tcref.Range)) tcref.Range
                                inheritsExpr,false,None,defnCs
                       
                    let envForTycon = MakeInnerEnvForTyconRef cenv envInitial tcref false 

                    // Compute the cpath used when creating the hidden fields 
                    let cpath = curr_access_cpath envForTycon

                    let localDecs  = defnCs |> List.filter (function PassCIncrClassBindings _ | PassCIncrClassCtorJustAfterSuperInit | PassCIncrClassCtorJustAfterLastLet -> true | _ -> false)
                    let memberBindsWithFixups = defnCs |> List.choose (function PassCMember pgrbind -> Some pgrbind | _ -> None) 

                    // Extend localDecs with "let safeThisVal = ref null" if there is a safeThisVal
                    let localDecs  = 
                        match safeThisValBindOpt with 
                        | None -> localDecs 
                        | Some bind -> PassCIncrClassBindings [IncrClassBindingGroup([bind],false,false)] :: localDecs
                        
                    // Carve out the initialization sequence and decide on the localRep 
                    let ctorBodyLambdaExpr,cctorBodyLambdaExprOpt,methodBinds,localReps = 
                        
                        let localDecs = 
                            [ for localDec in localDecs do 
                                  match localDec with 
                                  | PassCIncrClassBindings(binds) -> yield PassCBindings binds
                                  | PassCIncrClassCtorJustAfterSuperInit  -> yield PassCCtorJustAfterSuperInit
                                  | PassCIncrClassCtorJustAfterLastLet -> yield PassCCtorJustAfterLastLet
                                  | _ -> () ]
                        let memberBinds = memberBindsWithFixups |> List.map (fun x -> x.Binding) 
                        MakeCtorForIncrClassConstructionPassC(cenv,envForTycon,tpenv,incrClassCtorLhs,inheritsExpr,inheritsIsVisible,localDecs,memberBinds,generalizedTyparsForRecursiveBlock,safeStaticInitInfo)

                    // Generate the (value,expr) pairs for the implicit 
                    // object constructor and implicit static initializer 
                    let ctorValueExprBindings = 
                        [ (let ctorValueExprBinding = TBind(incrClassCtorLhs.InstanceCtorVal,ctorBodyLambdaExpr,NoSequencePointAtStickyBinding)
                           let rbind = { ValScheme = incrClassCtorLhs.InstanceCtorValScheme ; Binding = ctorValueExprBinding }
                           FixupLetrecBind cenv envInitial.DisplayEnv generalizedTyparsForRecursiveBlock rbind) ]
                        @ 
                        ( match cctorBodyLambdaExprOpt with 
                          | None -> []
                          | Some(cctorBodyLambdaExpr) -> 
                             [ (let _,cctorVal, cctorValScheme = incrClassCtorLhs.StaticCtorValInfo.Force()
                                let cctorValueExprBinding = TBind(cctorVal,cctorBodyLambdaExpr,NoSequencePointAtStickyBinding)
                                let rbind = { ValScheme = cctorValScheme; Binding =  cctorValueExprBinding  }
                                FixupLetrecBind cenv envInitial.DisplayEnv generalizedTyparsForRecursiveBlock rbind) ] ) 

                    // Publish the fields of the representation to the type 
                    localReps.PublishIncrClassFields cenv denv cpath incrClassCtorLhs safeStaticInitInfo; (* mutation *)    
                    
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
                                    
                            let x = localReps.FixupIncrClassExprPassC thisValOpt safeStaticInitInfo thisTyInst x 

                            { pgrbind with Binding = TBind(v,x,spBind) } )
                        
                    ctorValueExprBindings @ memberBindsWithFixups, methodBinds  
                
                // Cover the case where this is not a class with an implicit constructor
                | defnCs -> 
                    let memberBindsWithFixups = defnCs |> List.choose (function PassCMember pgrbind -> Some pgrbind | _ -> None) 
                    memberBindsWithFixups,[])
            |> List.unzip

        let fixupValueExprBinds = List.concat fixupValueExprBinds
        let methodBinds = List.concat methodBinds 
        
        // INITIALIZATION GRAPHS 
        let binds = EliminateInitializationGraphs cenv.g true envInitial.DisplayEnv fixupValueExprBinds bindsm

        let binds = binds @ methodBinds
        
        // Post letrec env 
        let envbody = AddLocalVals scopem prelimRecValues envInitial 
        binds,envbody,tpenv

end

//-------------------------------------------------------------------------
// The member portions of class defns
//------------------------------------------------------------------------- 
    
let TcTyconMemberDefns cenv env parent bindsm scopem tyconDefnMembers = 
    let interfacesFromTypeDefn (TyconMemberData(declKind, tcref, _, _, declaredTyconTypars, members, _, _)) =
        let overridesOK  = DeclKind.CanOverrideOrImplement(declKind)
        members |> List.collect (function 
            | SynMemberDefn.Interface(ity,defnOpt,_) -> 
                  let _,typ = if tcref.Deref.IsExceptionDecl then [],cenv.g.exn_ty else generalizeTyconRef tcref
                  let m = ity.Range
                  if tcref.IsTypeAbbrev then error(Error(FSComp.SR.tcTypeAbbreviationsCannotHaveInterfaceDeclaration(),m));
                  if tcref.IsEnumTycon then error(Error(FSComp.SR.tcEnumerationsCannotHaveInterfaceDeclaration(),m));

                  begin match defnOpt with 
                  | Some(defn) -> 
                      let ity' = 
                          let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTyconTypars env
                          TcTypeAndRecover cenv NoNewTypars CheckCxs envinner emptyUnscopedTyparEnv ity |> fst
                      if not (isInterfaceTy cenv.g ity') then errorR(Error(FSComp.SR.tcTypeIsNotInterfaceType0(),ity.Range));
                      
                      if not (tcref.HasInterface cenv.g ity') then 
                          error(Error(FSComp.SR.tcAllImplementedInterfacesShouldBeDeclared(),ity.Range));
                      if (typeEquiv cenv.g ity' cenv.g.mk_IComparable_ty && isSome tcref.GeneratedCompareToValues) || 
                          (typeEquiv cenv.g ity' cenv.g.mk_IStructuralComparable_ty && isSome tcref.GeneratedCompareToWithComparerValues) ||
                          (typeEquiv cenv.g ity' ((mkAppTy cenv.g.system_GenericIComparable_tcref [typ])) && isSome tcref.GeneratedCompareToValues) ||
                          (typeEquiv cenv.g ity' ((mkAppTy cenv.g.system_GenericIEquatable_tcref [typ])) && isSome tcref.GeneratedHashAndEqualsWithComparerValues) ||
                          (typeEquiv cenv.g ity' cenv.g.mk_IStructuralEquatable_ty && isSome tcref.GeneratedHashAndEqualsWithComparerValues) then
                          errorR(Error(FSComp.SR.tcDefaultImplementationForInterfaceHasAlreadyBeenAdded(),ity.Range));
                      if overridesOK = WarnOnOverrides then  
                          warning(IntfImplInIntrinsicAugmentation(ity.Range));
                      if overridesOK = ErrorOnOverrides then  
                          errorR(IntfImplInExtrinsicAugmentation(ity.Range));
                      [ (ity',defn,m) ]
                  | _-> []
                  end
                  
            | _ -> []) 

    let interfaceMembersFromTypeDefn (TyconMemberData(declKind, tcref, baseValOpt, safeInitInfo, declaredTyconTypars, _, _, newslotsOK)) (ity',defn,_) implTySet =
        let containerInfo = ContainerInfo(parent, Some(MemberOrValContainerInfo(tcref, Some(ity',implTySet), baseValOpt, safeInitInfo, declaredTyconTypars)))
        defn  |> List.choose (fun mem ->
                match mem with
                | SynMemberDefn.Member(_,m) -> 
                    Some(TyconBindingDefn(containerInfo,newslotsOK,declKind,mem,m))
                | SynMemberDefn.LetBindings(_,_,_,m)    // <-- possible design suggestion: relax this 
                | SynMemberDefn.ImplicitCtor(_,_,_,_,m)
                | SynMemberDefn.ImplicitInherit(_,_,_,m)
                | SynMemberDefn.Interface(_,_,m) 
                | SynMemberDefn.AbstractSlot(_,_,m)
                | SynMemberDefn.Inherit(_,_,m)
                | SynMemberDefn.ValField(_,m)
                | SynMemberDefn.Open (_,m)
                | SynMemberDefn.NestedType(_,_,m) -> errorR(Error(FSComp.SR.tcMemberNotPermittedInInterfaceImplementation(),m)); None)

    let tpenv = emptyUnscopedTyparEnv

    try
      // Some preliminary checks 
      tyconDefnMembers |> List.iter (fun (TyconMemberData(declKind, tcref, _, _, _, members, m, newslotsOK)) -> 
             let tcaug = tcref.TypeContents
             if tcaug.tcaug_closed && declKind <> ExtrinsicExtensionBinding then 
               error(InternalError("Intrinsic augmentations of types are only permitted in the same file as the definition of the type",m));
             members |> List.iter (function 
                    | SynMemberDefn.Member _ -> ()
                    | SynMemberDefn.Interface _ -> () 
                    | SynMemberDefn.Open _ 
                    | SynMemberDefn.LetBindings _  // accept local definitions 
                    | SynMemberDefn.ImplicitCtor _ // accept implicit ctor pattern, should be first! 
                    | SynMemberDefn.ImplicitInherit _ when newslotsOK = NewSlotsOK -> () // accept implicit ctor pattern, should be first! 
                    // The follow should have been removed by splitting, they belong to "core" (they are "shape" of type, not implementation) 
                    | SynMemberDefn.Open (_,m) 
                    | SynMemberDefn.LetBindings(_,_,_,m) 
                    | SynMemberDefn.ImplicitCtor(_,_,_,_,m)
                    | SynMemberDefn.ImplicitInherit(_,_,_,m) 
                    | SynMemberDefn.AbstractSlot(_,_,m)
                    | SynMemberDefn.Inherit(_,_,m)
                    | SynMemberDefn.ValField(_,m)
                    | SynMemberDefn.NestedType(_,_,m) -> error(Error(FSComp.SR.tcDeclarationElementNotPermittedInAugmentation(),m))));

      let tyconBindingsOfTypeDefn (TyconMemberData(declKind, tcref, baseValOpt, safeInitInfo, declaredTyconTypars, members, _, newslotsOK)) =
          let containerInfo = ContainerInfo(parent,Some(MemberOrValContainerInfo(tcref, None, baseValOpt, safeInitInfo, declaredTyconTypars)))
          members 
          |> List.choose (fun memb ->
              match memb with 
              | SynMemberDefn.ImplicitCtor(_,_,_,_,m)
              | SynMemberDefn.ImplicitInherit(_,_,_,m) 
              | SynMemberDefn.LetBindings(_,_,_,m) 
              | SynMemberDefn.Member(_,m) 
              | SynMemberDefn.Open (_,m) 
                  -> Some(TyconBindingDefn(containerInfo,newslotsOK,declKind,memb,m))

              // Interfaces exist in the member list - handled above in interfaceMembersFromTypeDefn 
              | SynMemberDefn.Interface _  -> None

              // The following should have been List.unzip out already in SplitTyconDefn 
              | SynMemberDefn.AbstractSlot (_,_,m) 
              | SynMemberDefn.ValField (_,m)             
              | SynMemberDefn.Inherit (_,_,m)    -> error(InternalError("Unexpected declaration element",m))
              | SynMemberDefn.NestedType (_,_,m)      -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m)))
          
      let binds  = 
          tyconDefnMembers |> List.map (fun (TyconMemberData(declKind, tcref, _, _, _, _, _, _) as tyconMemberData) -> 
              let obinds = tyconBindingsOfTypeDefn tyconMemberData
              let ibinds  = 
                      let intfTypes = interfacesFromTypeDefn tyconMemberData
                      let slotImplSets = DispatchSlotChecking.GetSlotImplSets cenv.infoReader env.DisplayEnv false (List.map (fun (ity,_,m) -> (ity,m)) intfTypes)
                      List.concat (List.map2 (interfaceMembersFromTypeDefn tyconMemberData) intfTypes slotImplSets)
              TyconBindingDefns(tcref, declKind, obinds @ ibinds))
      
      let results = TyconBindingChecking.TcTyconBindings cenv env tpenv bindsm scopem binds
      let binds,envbody,_ = results
      binds,envbody

    with e -> errorRecovery e scopem; [], env

//-------------------------------------------------------------------------
// Build augmentation declarations
//------------------------------------------------------------------------- 

module AddAugmentationDeclarations = begin
    let tcaug_has_nominal_interface g tcaug tcref =
        tcaug.tcaug_interfaces |> List.exists (fun (x,_,_) -> 
            isAppTy g x && tyconRefEq g (tcrefOfAppTy g x) tcref)

        
    let AddGenericCompareDeclarations cenv (env: TcEnv) (scSet:Set<Stamp>) (tycon:Tycon) =
        if Augment.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && scSet.Contains tycon.Stamp then 
            let tcref = mkLocalTyconRef tycon
            let tcaug = tycon.TypeContents
            let _,typ = if tcref.Deref.IsExceptionDecl then [],cenv.g.exn_ty else generalizeTyconRef tcref
            let m = tycon.Range
            let genericIComparableTy = mkAppTy cenv.g.system_GenericIComparable_tcref [typ]


            let hasExplicitIComparable = tycon.HasInterface cenv.g cenv.g.mk_IComparable_ty 
            let hasExplicitGenericIComparable = tcaug_has_nominal_interface cenv.g tcaug cenv.g.system_GenericIComparable_tcref    
            let hasExplicitIStructuralComparable = tycon.HasInterface cenv.g cenv.g.mk_IStructuralComparable_ty

            if hasExplicitIComparable then 
                errorR(Error(FSComp.SR.tcImplementsIComparableExplicitly(tycon.DisplayName),m)); 
      
            elif hasExplicitGenericIComparable then 
                errorR(Error(FSComp.SR.tcImplementsGenericIComparableExplicitly(tycon.DisplayName),m)); 
            elif hasExplicitIStructuralComparable then
                errorR(Error(FSComp.SR.tcImplementsIStructuralComparableExplicitly(tycon.DisplayName),m)); 
            else
                let hasExplicitGenericIComparable = tycon.HasInterface cenv.g genericIComparableTy
                let cvspec1,cvspec2 = Augment.MakeValsForCompareAugmentation cenv.g tcref
                let cvspec3 = Augment.MakeValsForCompareWithComparerAugmentation cenv.g tcref

                PublishInterface cenv env.DisplayEnv tcref m true cenv.g.mk_IStructuralComparable_ty;
                PublishInterface cenv env.DisplayEnv tcref m true cenv.g.mk_IComparable_ty;
                if not tycon.IsExceptionDecl && not hasExplicitGenericIComparable then 
                    PublishInterface cenv env.DisplayEnv tcref m true genericIComparableTy;
                tcaug.SetCompare (mkLocalValRef cvspec1, mkLocalValRef cvspec2);
                tcaug.SetCompareWith (mkLocalValRef cvspec3);
                PublishValueDefn cenv env ModuleOrMemberBinding cvspec1
                PublishValueDefn cenv env ModuleOrMemberBinding cvspec2
                PublishValueDefn cenv env ModuleOrMemberBinding cvspec3

               

    let AddGenericEqualityWithComparerDeclarations cenv (env: TcEnv) (seSet:Set<Stamp>) (tycon:Tycon) =
        if Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon && seSet.Contains tycon.Stamp then 
            let tcref = mkLocalTyconRef tycon
            let tcaug = tycon.TypeContents
            let m = tycon.Range

            let hasExplicitIStructuralEquatable = tycon.HasInterface cenv.g cenv.g.mk_IStructuralEquatable_ty

            if hasExplicitIStructuralEquatable then
                errorR(Error(FSComp.SR.tcImplementsIStructuralEquatableExplicitly(tycon.DisplayName),m)); 
            else
                let evspec1,evspec2,evspec3 = Augment.MakeValsForEqualityWithComparerAugmentation cenv.g tcref
                PublishInterface cenv env.DisplayEnv tcref m true cenv.g.mk_IStructuralEquatable_ty;                
                tcaug.SetHashAndEqualsWith (mkLocalValRef evspec1, mkLocalValRef evspec2, mkLocalValRef evspec3)
                PublishValueDefn cenv env ModuleOrMemberBinding evspec1
                PublishValueDefn cenv env ModuleOrMemberBinding evspec2
                PublishValueDefn cenv env ModuleOrMemberBinding evspec3

                
    let AddGenericCompareBindings cenv (tycon:Tycon) =
        if (* Augment.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && *) isSome tycon.GeneratedCompareToValues then 
            Augment.MakeBindingsForCompareAugmentation cenv.g tycon
        else
            []
            
    let AddGenericCompareWithComparerBindings cenv (tycon:Tycon) =
        if (* Augment.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && *) isSome tycon.GeneratedCompareToWithComparerValues then
             (Augment.MakeBindingsForCompareWithComparerAugmentation cenv.g tycon)
         else
            []
             
    let AddGenericEqualityWithComparerBindings cenv (tycon:Tycon) =
        if Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon  && isSome tycon.GeneratedHashAndEqualsWithComparerValues then
            (Augment.MakeBindingsForEqualityWithComparerAugmentation cenv.g tycon)
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
        if Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
            let tcref = mkLocalTyconRef tycon
            let tcaug = tycon.TypeContents
            let _,typ = if tcref.Deref.IsExceptionDecl then [],cenv.g.exn_ty else generalizeTyconRef tcref
            let m = tycon.Range
            
            // Note: tycon.HasOverride only gives correct results after we've done the type augmentation 
            let hasExplicitObjectEqualsOverride = tycon.HasOverride cenv.g "Equals" [cenv.g.obj_ty]
            let hasExplicitGenericIEquatable = tcaug_has_nominal_interface cenv.g tcaug cenv.g.system_GenericIEquatable_tcref
            
            if hasExplicitGenericIEquatable then 
                errorR(Error(FSComp.SR.tcImplementsIEquatableExplicitly(tycon.DisplayName),m)); 

            // Note: only provide the equals method if Equals is not implemented explicitly, and
            // we're actually generating Hash/Equals for this type
            if not hasExplicitObjectEqualsOverride &&
                isSome tycon.GeneratedHashAndEqualsWithComparerValues then

                 let vspec1,vspec2 = Augment.MakeValsForEqualsAugmentation cenv.g tcref
                 tcaug.SetEquals (mkLocalValRef vspec1, mkLocalValRef vspec2);
                 if not tycon.IsExceptionDecl then 
                    PublishInterface cenv env.DisplayEnv tcref m true  (mkAppTy cenv.g.system_GenericIEquatable_tcref [typ])
                 PublishValueDefn cenv env ModuleOrMemberBinding vspec1;
                 PublishValueDefn cenv env ModuleOrMemberBinding vspec2;
                 Augment.MakeBindingsForEqualsAugmentation cenv.g tycon
            else []
        else []

end

module TyconConstraintInference = begin

    let InferSetOfTyconsSupportingComparable cenv (env: TcEnv)  structuralTypes (tycons:Tycon list) =

        let g = cenv.g 
        let tab = (tycons,structuralTypes) ||> List.map2 (fun tycon c -> tycon.Stamp, (tycon,c)) |> Map.ofList 

        // Initially, assume the equality relation is available for all structural type definitions 
        let initialAssumedTycons = 
            set [ for tycon in tycons do 
                       if Augment.TyconIsCandidateForAugmentationWithCompare cenv.g tycon then 
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
                if isTyparTy cenv.g ty then 
                    let tp = (destTyparTy cenv.g ty)

                    // Look for an explicit 'comparison' constraint
                    if tp.Constraints |> List.exists (function TTyparSupportsComparison _ -> true | _ -> false) then 
                        true
                    
                    // Within structural types, type parameters can be optimistically assumed to have comparison
                    // We record the ones for which we have made this assumption.
                    elif tycon.TyparsNoRange |> List.exists (fun tp2 -> typarRefEq tp tp2) then 
                        assumedTyparsAcc := (!assumedTyparsAcc).Add(tp.Stamp);
                        true
                    
                    else
                        false
                
                else 
                    match ty with 
                    // Look for array, UIntPtr and IntPtr types
                    | SpecialComparableHeadType g tinst -> 
                        tinst |> List.forall (checkIfFieldTypeSupportsComparison  tycon)

                    // Otherwise its a nominal type
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
                            not (HasAttrib g g.attrib_NoComparisonAttribute tcref.Attribs)
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
                   if cenv.g.compilingFslib && Augment.TyconIsCandidateForAugmentationWithCompare cenv.g tycon && not (HasAttrib g g.attrib_StructuralComparisonAttribute tycon.Attribs) && not (HasAttrib g g.attrib_NoComparisonAttribute tycon.Attribs) then 
                       errorR(Error(FSComp.SR.tcFSharpCoreRequiresExplicit(),tycon.Range)); 

                   let res = (structuralTypes |> List.forall (fst >> checkIfFieldTypeSupportsComparison tycon))

                   // If the type was excluded, say why
                   if not res then 
                       match TryFindBoolAttrib g g.attrib_StructuralComparisonAttribute tycon.Attribs with
                       | Some(true) -> 
                           match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsComparison tycon >> not) with
                           | None -> 
                               assert false
                               failwith "unreachble"
                           | Some (ty,_) -> 
                               if isTyparTy g ty then 
                                   errorR(Error(FSComp.SR.tcStructuralComparisonNotSatisfied1(tycon.DisplayName,NicePrint.prettyStringOfTy env.DisplayEnv ty),tycon.Range)); 
                               else 
                                   errorR(Error(FSComp.SR.tcStructuralComparisonNotSatisfied2(tycon.DisplayName,NicePrint.prettyStringOfTy env.DisplayEnv ty),tycon.Range)); 
                       | Some(false) -> 
                           ()
                       
                       | None -> 
                           match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsComparison tycon >> not) with
                           | None -> 
                               assert false
                               failwith "unreachble"
                           | Some (ty,_) -> 
                               // NOTE: these warnings are off by default - they are level 4 informational warnings
                               if isTyparTy g ty then 
                                   warning(Error(FSComp.SR.tcNoComparisonNeeded1(tycon.DisplayName, NicePrint.prettyStringOfTy env.DisplayEnv ty, tycon.DisplayName),tycon.Range)); 
                               else 
                                   warning(Error(FSComp.SR.tcNoComparisonNeeded2(tycon.DisplayName, NicePrint.prettyStringOfTy env.DisplayEnv ty, tycon.DisplayName),tycon.Range)); 

                                                      
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

    let InferSetOfTyconsSupportingEquatable cenv (env: TcEnv)  structuralTypes (tycons:Tycon list) =

        let g = cenv.g 
        let tab = (tycons,structuralTypes) ||> List.map2 (fun tycon c -> tycon.Stamp, (tycon,c)) |> Map.ofList 

        // Initially, assume the equality relation is available for all structural type definitions 
        let initialAssumedTycons = 
            set [ for tycon in tycons do 
                       if Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
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
                if isTyparTy cenv.g ty then 
                    let tp = (destTyparTy cenv.g ty)

                    // Look for an explicit 'equality' constraint
                    if tp.Constraints |> List.exists (function TTyparSupportsEquality _ -> true | _ -> false) then 
                        true

                    // Within structural types, type parameters can be optimistically assumed to have ewquality
                    // We record the ones for which we have made this assumption.
                    elif tycon.Typars(tycon.Range) |> List.exists (fun tp2 -> typarRefEq tp tp2) then                     
                        assumedTyparsAcc := (!assumedTyparsAcc).Add(tp.Stamp);
                        true
                    else
                        false

                else 
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
                             elif Augment.TyconIsCandidateForAugmentationWithEquals g tcref.Deref then
                                isSome tcref.GeneratedHashAndEqualsWithComparerValues
                             else
                                true) 
                             &&
                             // Check it isn't ruled out by the user
                             not (HasAttrib g g.attrib_NoEqualityAttribute tcref.Attribs)
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
                   if cenv.g.compilingFslib && Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon && not (HasAttrib g g.attrib_StructuralEqualityAttribute tycon.Attribs) && not (HasAttrib g g.attrib_NoEqualityAttribute tycon.Attribs) then 
                       errorR(Error(FSComp.SR.tcFSharpCoreRequiresExplicit(),tycon.Range)); 

                   // Remove structural types with incomparable elements from the assumedTycons
                   let res = (structuralTypes |> List.forall (fst >> checkIfFieldTypeSupportsEquality tycon))

                   // If the type was excluded, say why
                   if not res then 
                       match TryFindBoolAttrib g g.attrib_StructuralEqualityAttribute tycon.Attribs with
                       | Some(true) -> 
                           if Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
                               match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsEquality tycon >> not) with
                               | None -> 
                                   assert false
                                   failwith "unreachble"
                               | Some (ty,_) -> 
                                   if isTyparTy g ty then 
                                       errorR(Error(FSComp.SR.tcStructuralEqualityNotSatisfied1(tycon.DisplayName,NicePrint.prettyStringOfTy env.DisplayEnv ty),tycon.Range)); 
                                   else 
                                       errorR(Error(FSComp.SR.tcStructuralEqualityNotSatisfied2(tycon.DisplayName,NicePrint.prettyStringOfTy env.DisplayEnv ty),tycon.Range)); 
                           else
                               ()
                       | Some(false) -> 
                           ()
                       | None -> 
                           if Augment.TyconIsCandidateForAugmentationWithEquals cenv.g tycon then 
                               match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsEquality tycon >> not) with
                               | None -> 
                                   assert false
                                   failwith "unreachble"
                               | Some (ty,_) -> 
                                   if isTyparTy g ty then 
                                       warning(Error(FSComp.SR.tcNoEqualityNeeded1(tycon.DisplayName, NicePrint.prettyStringOfTy env.DisplayEnv ty, tycon.DisplayName),tycon.Range)); 
                                   else 
                                       warning(Error(FSComp.SR.tcNoEqualityNeeded2(tycon.DisplayName, NicePrint.prettyStringOfTy env.DisplayEnv ty, tycon.DisplayName),tycon.Range)); 

                                                      
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

end
    

//-------------------------------------------------------------------------
// Helpers for modules, types and exception declarations
//------------------------------------------------------------------------- 

let ComputeModuleName (longPath: Ident list) = 
    if longPath.Length <> 1 then error(Error(FSComp.SR.tcInvalidModuleName(),(List.head longPath).idRange));
    longPath.Head 

let CheckForDuplicateConcreteType _cenv env nm m  = 
    let curr = GetCurrAccumulatedModuleOrNamespaceType env
    if Map.containsKey nm curr.AllEntitiesByCompiledAndLogicalMangledNames then 
        // Use 'error' instead of 'errorR' here to avoid cascading errors - see bug 1177 in FSharp 1.0 
        error (Duplicate(FSComp.SR.tcTypeExceptionOrModule(),nm,m))

let CheckForDuplicateModule _cenv env nm m  = 
    let curr = GetCurrAccumulatedModuleOrNamespaceType env
    if curr.ModulesAndNamespacesByDemangledName.ContainsKey(nm) then 
        errorR (Duplicate(FSComp.SR.tcTypeOrModule(),nm,m))


//-------------------------------------------------------------------------
// Bind exception definitions
//------------------------------------------------------------------------- 

module TcExceptionDeclarations = begin

    let private TcExnDefnCore cenv env parent tpenv (ExceptionDefnRepr(attrs,UnionCase(_,id,args,_,_,_),repr,doc,vis,m), scopem) =
        let attrs' = TcAttributes cenv env AttributeTargets.ExnDecl attrs
        let args = match args with (UnionCaseFields args) -> args | _ -> error(Error(FSComp.SR.tcExplicitTypeSpecificationCannotBeUsedForExceptionConstructors(),m))
        let ad = AccessRightsOfEnv env
        
        let args' = List.mapi (fun i fdef -> TcRecdUnionAndEnumDeclarations.TcAnonFieldDecl cenv env parent tpenv ("Data"^string i) fdef) args
        if not (String.isUpper id.idText) then errorR(NotUpperCaseConstructor(m));
        let vis,cpath = ComputeAccessAndCompPath env None m vis parent
        let vis = TcRecdUnionAndEnumDeclarations.CombineReprAccess parent vis
        let exnc = 
          match repr with 
          | Some lid ->
              match ResolveExprLongIdent cenv.nameResolver m ad env.eNameResEnv DefaultTypeNameResInfo lid with
              | Item.ExnCase exnc, [] -> 
                  CheckTyconAccessible m (AccessRightsOfEnv env) exnc |> ignore;
                  if List.length args' <> 0 then 
                    errorR (Error(FSComp.SR.tcExceptionAbbreviationsShouldNotHaveArgumentList(),m));
                  NewExn cpath id vis (TExnAbbrevRepr exnc) attrs' (doc.ToXmlDoc())
              | Item.CtorGroup(_,meths) , [] -> 
                  match args' with 
                  | [] -> ()
                  | _ -> error (Error(FSComp.SR.tcAbbreviationsFordotNetExceptionsCannotTakeArguments(),m));
                  let candidates = 
                      meths |> List.filter (fun minfo -> 
                          minfo.NumArgs = [args'.Length] &&
                          minfo.GenericArity = 0) 
                  match candidates with 
                  | [minfo] -> 
                      let err() = 
                          Error(FSComp.SR.tcExceptionAbbreviationsMustReferToValidExceptions(),m)
                      if not (TypeDefinitelySubsumesTypeNoCoercion 0 cenv.g cenv.amap m cenv.g.exn_ty minfo.EnclosingType) then 
                        errorR(err());
                      let tref = 
                          match minfo with 
                          | ILMeth(_,minfo,_) -> minfo.ILTypeRef
                          | FSMeth _ -> 
                              match (tcrefOfAppTy cenv.g minfo.EnclosingType).CompiledRepresentation with 
                              | TyrepNamed (tref,_,_) -> tref
                              | _ -> 
                                  error (err()) 
                          | _ -> error (err()) 
                      NewExn  cpath id vis (TExnAsmRepr tref) attrs' (doc.ToXmlDoc())
                  | _ -> 
                      error (Error(FSComp.SR.tcAbbreviationsFordotNetExceptionsMustHaveMatchingObjectConstructor(),m))
              | _ ->
                  error (Error(FSComp.SR.tcNotAnException(),m))
          | None -> 
             NewExn cpath id vis (TExnFresh (MakeRecdFieldsTable args')) attrs' (doc.ToXmlDoc())
        
        let tcaug = exnc.TypeContents
        tcaug.tcaug_super <- Some cenv.g.exn_ty;

        CheckForDuplicateConcreteType cenv env (id.idText ^ "Exception") id.idRange;
        CheckForDuplicateConcreteType cenv env id.idText id.idRange;
        PublishTypeDefn cenv env exnc;

        let structuralTypes = args' |> List.map (fun rf -> (rf.FormalType, rf.Range))
        let scSet = TyconConstraintInference.InferSetOfTyconsSupportingComparable cenv env [structuralTypes] [exnc]
        let seSet = TyconConstraintInference.InferSetOfTyconsSupportingEquatable cenv env [structuralTypes] [exnc]

        // Augment the exception constructor with comparison and hash methods if needed 
        let binds = 
          match exnc.ExceptionInfo with 
          | TExnAbbrevRepr _ | TExnNone | TExnAsmRepr _ -> []
          | TExnFresh _ -> 
              AddAugmentationDeclarations.AddGenericHashAndComparisonDeclarations cenv env scSet seSet exnc
              AddAugmentationDeclarations.AddGenericHashAndComparisonBindings cenv exnc
        binds,
        exnc,
        AddLocalExnDefn scopem exnc (AddLocalTycons cenv.g cenv.amap scopem [exnc] env)

    let TcExnDefn cenv env parent tpenv (ExceptionDefn(core,aug,m),scopem) = 
        let binds1,exnc,env = TcExnDefnCore cenv env parent tpenv (core,scopem)
        let binds2,env = TcTyconMemberDefns cenv env parent m scopem [TyconMemberData(ModuleOrMemberBinding, (mkLocalEntityRef exnc), None, NoSafeInitInfo, [], aug, m, NoNewSlots)]
        // Augment types with references to values that implement the pre-baked semantics of the type
        let binds3 = AddAugmentationDeclarations.AddGenericEqualityBindings cenv env exnc
        binds1 @ binds2 @ binds3,exnc,env

    let TcExnSignature cenv env parent tpenv (ExceptionSig(core,aug,_),scopem) = 
        let binds,exnc,env = TcExnDefnCore cenv env parent tpenv (core,scopem)
        let ecref = mkLocalEntityRef exnc
        let vals,_ = TcTyconMemberSpecs cenv env (ContainerInfo(parent,Some(MemberOrValContainerInfo(ecref,None,None,NoSafeInitInfo,[])))) ModuleOrMemberBinding tpenv aug
        binds,vals,ecref,env

end

///-------------------------------------------------------------------------
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
///------------------------------------------------------------------------- 

type TyconDefnCore = 
    TyconDefnCore of 
       SynComponentInfo * 
       SynTypeDefnSimpleRepr * 
       (SynType * range) list * 
       bool * (*preEstablishedHasDefaultCtor*) 
       bool (*hasSelfReferentialCtor:*) 

type TyconDefnCoreIndexed = 
    TyconDefnCoreIndexed of 
        SynComponentInfo * 
        SynTypeDefnSimpleRepr * 
        (SynType * range) list * 
        bool * (*preEstablishedHasDefaultCtor*) 
        bool * (*hasSelfReferentialCtor:*) 
        int (*index:*) 

module EstablishTypeDefinitionCores = begin
 
    let private ComputeTyconName (longPath: Ident list) doErase (typars: Typars) = 
        if longPath.Length <> 1 then error(Error(FSComp.SR.tcInvalidTypeExtension(),longPath.Head.idRange));
        let id = List.head longPath
        let erasedArity = 
            if doErase then 
                List.foldBack (fun (tp:Typar) n -> if tp.IsErased then n else n+1) typars 0 
            else typars.Length
        mkSynId id.idRange (if erasedArity = 0 then id.idText else id.idText ^ "`" ^string erasedArity)
 
    let private GetTyconAttribs g attrs = 
        let hasClassAttr         = HasAttrib g g.attrib_ClassAttribute attrs
        let hasAbstractClassAttr = HasAttrib g g.attrib_AbstractClassAttribute attrs
        let hasInterfaceAttr     = HasAttrib g g.attrib_InterfaceAttribute attrs
        let hasStructAttr        = HasAttrib g g.attrib_StructAttribute attrs
        let hasMeasureAttr       = HasAttrib g g.attrib_MeasureAttribute attrs
        (hasClassAttr,hasAbstractClassAttr,hasInterfaceAttr,hasStructAttr,hasMeasureAttr)

    //-------------------------------------------------------------------------
    // Type kind inference 
    //------------------------------------------------------------------------- 
       
    let private InferTyconKind g (kind,attrs',slotsigs,fields,inSig ,isConcrete,m) =
        let (hasClassAttr,hasAbstractClassAttr,hasInterfaceAttr,hasStructAttr,hasMeasureAttr) = GetTyconAttribs g attrs'
        let bi b = (if b then 1 else 0)
        if (bi hasClassAttr + bi hasInterfaceAttr + bi hasStructAttr + bi hasMeasureAttr) > 1 ||
           (bi hasAbstractClassAttr + bi hasInterfaceAttr + bi hasStructAttr + bi hasMeasureAttr) > 1 then
           error(Error(FSComp.SR.tcAttributesOfTypeSpecifyMultipleKindsForType(),m));
        
        match kind with 
        | TyconUnspecified ->
            if hasClassAttr || hasAbstractClassAttr || hasMeasureAttr then TyconClass        
            elif hasInterfaceAttr then TyconInterface
            elif hasStructAttr then TyconStruct
            elif isConcrete || nonNil fields then TyconClass
            elif isNil slotsigs && inSig  then TyconHiddenRepr
            else TyconInterface
        | k -> 
            if hasClassAttr && not (match k with TyconClass -> true | _ -> false) || 
               hasMeasureAttr && not (match k with TyconClass | TyconAbbrev | TyconHiddenRepr -> true | _ -> false)  || 
               hasInterfaceAttr && not (match k with TyconInterface -> true | _ -> false) || 
               hasStructAttr && not (match k with TyconStruct -> true | _ -> false) then 
                error(Error(FSComp.SR.tcKindOfTypeSpecifiedDoesNotMatchDefinition(),m));
            k


    let private (|TyconCoreAbbrevThatIsReallyAUnion|_|) (hasMeasureAttr,envinner,id:Ident) synTyconRepr =
        match synTyconRepr with 
        | SynTypeDefnSimpleRepr.TypeAbbrev(SynType.LongIdent([unionCaseName],_),m) 
                              when 
                                (not hasMeasureAttr && 
                                 (isNil (LookupTypeNameInEnvNoArity OpenQualified unionCaseName.idText envinner.eNameResEnv) || 
                                  id.idText = unionCaseName.idText)) -> 
            Some(unionCaseName,m)
        | _ -> 
            None

    // Used when determining if a structural type supports structual comparison
    let private GetStructuralElementsOfTyconDefn cenv env tpenv (TyconDefnCoreIndexed(_,synTyconRepr,_,_,_,_)) tycon = 
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
                      let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs env tpenv ty
                      yield (ty',m)
              | UnionCaseFullType (ty,arity) -> 
                  let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs env tpenv ty
                  let argtysl,_ = GetTopTauTypeInFSharpForm cenv.g (arity |> TranslateTopValSynInfo m (TcAttributes cenv env) |> TranslatePartialArity []).ArgInfos ty' m
                  if argtysl.Length > 1 then 
                      errorR(Error(FSComp.SR.tcIllegalFormForExplicitTypeDeclaration(),m));   
                  for argtys in argtysl do
                    for (argty,_) in argtys do
                      yield (argty ,m)

          | SynTypeDefnSimpleRepr.General (_,_,_,fields,_,_,implicitCtorSynPats,_) when tycon.IsFSharpStructOrEnumTycon -> // for structs
              for (Field(_,stat,_,ty,_,_,_,m)) in fields do 
                  if not stat then 
                      let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs env tpenv ty
                      yield (ty',m)

              match implicitCtorSynPats with
              | None -> ()
              | Some spats -> 
                  let ctorArgNames,(_,names,_) = TcSimplePatsOfUnknownType cenv true NoCheckCxs env tpenv (SynSimplePats.SimplePats (spats,m))
                  for arg in ctorArgNames do
                      let ty = names.[arg].Type
                      let m = names.[arg].Ident.idRange
                      if nonNil (ListSet.subtract typarEq  (freeInTypeLeftToRight cenv.g false ty) tycon.TyparsNoRange) then
                          errorR(Error(FSComp.SR.tcStructsMustDeclareTypesOfImplicitCtorArgsExplicitly(),m));   
                      yield (ty, m)

                  
              ()

          | SynTypeDefnSimpleRepr.Record (_,fields,_) -> 
              for (Field(_,_,_,ty,_,_,_,m)) in fields do 
                  let ty',_ = TcTypeAndRecover cenv NoNewTypars NoCheckCxs env tpenv ty
                  yield (ty',m)
          | _ ->
              () ]

    // Establish 'type <vis1> C < T1... TN >  = <vis2> ...' including 
    //    - computing the mangled name for C
    // but 
    //    - we don't yet 'properly' establish constraints on type parameters
    let private TcTyconDefnCore_Phase0_BuildInitialTycon cenv env parent (TyconDefnCoreIndexed(synTyconInfo,synTyconRepr,_,preEstablishedHasDefaultCtor,hasSelfReferentialCtor,_)) = 
        let (ComponentInfo(_,typars, _,id,doc,preferPostfix, vis,_)) = synTyconInfo
        let typars' = TcTyparDecls cenv env typars
        id |> List.iter (CheckNamespaceModuleOrTypeName cenv);
        let id = ComputeTyconName id (match synTyconRepr with SynTypeDefnSimpleRepr.TypeAbbrev _ -> false | _ -> true) typars'

        // Augmentations of type definitions are allowed within the same file as long as no new type representation or abbreviation is given 
        CheckForDuplicateConcreteType cenv env id.idText id.idRange;
        CheckForDuplicateModule cenv env id.idText id.idRange;
        let vis,cpath = ComputeAccessAndCompPath env None id.idRange vis parent

        // Establish the visibility of the representation, e.g.
        //   type R = 
        //      private { f:int }
        //      member x.P = x.f + x.f
        let visOfRepr = 
            match synTyconRepr with 
            | SynTypeDefnSimpleRepr.None _ -> None
            | SynTypeDefnSimpleRepr.TypeAbbrev _ -> None
            | SynTypeDefnSimpleRepr.Union (vis,_,_) -> vis
            | SynTypeDefnSimpleRepr.ILAssembly _ -> None
            | SynTypeDefnSimpleRepr.Record (vis,_,_) -> vis
            | SynTypeDefnSimpleRepr.General _ -> None
            | SynTypeDefnSimpleRepr.Enum _ -> None
         
        let visOfRepr,_ = ComputeAccessAndCompPath env None id.idRange visOfRepr parent
        let visOfRepr = combineAccess vis visOfRepr 
        // If we supported nested types and modules then additions would be needed here
        let lmtyp = notlazy (NewEmptyModuleOrNamespaceType FSharpModule)
        NewTycon(cpath, id.idText, id.idRange, vis, visOfRepr, KindType, LazyWithContext.NotLazy typars', doc.ToXmlDoc(), preferPostfix, preEstablishedHasDefaultCtor, hasSelfReferentialCtor, lmtyp)

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
    let private TcTyconDefnCore_Phase1_EstablishBasicKind cenv inSig envinner (TyconDefnCoreIndexed(synTyconInfo,synTyconRepr,_,_,_,_)) (tycon:Tycon) = 
        let (ComponentInfo(attrs,typars, _,_, _, _,_,_)) = synTyconInfo
        let m = tycon.Range
        let id = tycon.Id
        // 'Check' the attributes. We end up discarding the results from this 
        // particular check since we re-check them in all other phases. 
        let attrs' = TcAttributes cenv envinner AttributeTargets.TyconDecl attrs
        let hasMeasureAttr = HasAttrib cenv.g cenv.g.attrib_MeasureAttribute attrs'

        // Set the compiled name, if any
        tycon.Data.entity_compiled_name <- TryFindStringAttrib cenv.g cenv.g.attrib_CompiledNameAttribute attrs' 

        if hasMeasureAttr then 
            tycon.Data.entity_kind <- KindMeasure;
            if nonNil typars then error(Error(FSComp.SR.tcMeasureDefinitionsCannotHaveTypeParameters(),m));

        let repr = 
            match synTyconRepr with 
            | SynTypeDefnSimpleRepr.None m -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconHiddenRepr,attrs',[],[],inSig,true,m)  |> ignore
                if not inSig && not hasMeasureAttr then 
                    errorR(Error(FSComp.SR.tcTypeRequiresDefinition(),m));
                if hasMeasureAttr then 
                    Some(TFsObjModelRepr { fsobjmodel_kind=TTyconClass; 
                                           fsobjmodel_vslots=[];
                                           fsobjmodel_rfields=MakeRecdFieldsTable [] })
                else 
                    None

            | TyconCoreAbbrevThatIsReallyAUnion (hasMeasureAttr,envinner,id) (_,m)
            | SynTypeDefnSimpleRepr.Union (_,_,m) -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconUnion,attrs',[],[],inSig,true,m) |> ignore
                // Note: the table of union cases is initially empty
                Some (MakeUnionRepr [])

            | SynTypeDefnSimpleRepr.TypeAbbrev _  -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconAbbrev,attrs',[],[],inSig,true,m) |> ignore
                None

            | SynTypeDefnSimpleRepr.ILAssembly (s,m) -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconILAssemblyCode,attrs',[],[],inSig,true,m) |> ignore
                Some (TAsmRepr s)

            | SynTypeDefnSimpleRepr.Record (_,_,m) -> 
                // Run InferTyconKind to raise errors on inconsistent attribute sets
                InferTyconKind cenv.g (TyconRecord,attrs',[],[],inSig,true,m) |> ignore
                // Note: the table of record fields is initially empty
                Some (TRecdRepr (MakeRecdFieldsTable  []) )

            | SynTypeDefnSimpleRepr.General (kind,_,slotsigs,fields,isConcrete,_,_,_) ->
                let kind = InferTyconKind cenv.g (kind,attrs',slotsigs,fields,inSig,isConcrete,m)
                match kind with 
                | TyconHiddenRepr -> 
                    None
                | _ -> 
                    let kind = 
                        match kind with
                        | TyconClass               -> TTyconClass
                        | TyconInterface           -> TTyconInterface
                        | TyconDelegate _          -> TTyconDelegate (mkSlotSig("Invoke",cenv.g.unit_ty,[],[],[], None))
                        | TyconStruct              -> TTyconStruct 
                        | _ -> error(InternalError("should have inferred tycon kind",m))

                    let repr = { fsobjmodel_kind=kind; 
                                 fsobjmodel_vslots=[];
                                 fsobjmodel_rfields=MakeRecdFieldsTable [] }
                    Some(TFsObjModelRepr repr)

            | SynTypeDefnSimpleRepr.Enum _ -> 
                let kind = TTyconEnum
                let repr = { fsobjmodel_kind=kind; 
                             fsobjmodel_vslots=[];
                             fsobjmodel_rfields=MakeRecdFieldsTable [] }
                Some(TFsObjModelRepr repr)

        // OK, now fill in the (partially computed) type representation
        tycon.Data.entity_tycon_repr <- repr

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
    // such as 'isTupleTy' will return reliable results, e.g. isTupleTy on the 
    /// TAST type for 'PairOfInts' will report 'true' 
    //
    let private TcTyconDefnCore_Phase2_Phase4_EstablishAbbreviations cenv envinner tpenv pass (TyconDefnCoreIndexed(tinfo,synTyconRepr,_,_,_,_)) (tycon:Tycon ) =
        let m = tycon.Range
        let checkCxs = if (pass = SecondPass) then CheckCxs else NoCheckCxs
        let firstPass = (pass = FirstPass)
        try 
            let (ComponentInfo(attrs,_, _,_, _, _,_,_)) = tinfo
            let id = tycon.Id
            let thisTyconRef = mkLocalTyconRef tycon
            let attrs' = TcAttributes cenv envinner AttributeTargets.TyconDecl attrs

            let hasMeasureAttr = HasAttrib cenv.g cenv.g.attrib_MeasureAttribute attrs'
            let hasMeasureableAttr = HasAttrib cenv.g cenv.g.attrib_MeasureableAttribute attrs'
            let hasGeneratedEstTypeAttr = HasAttrib cenv.g cenv.g.attrib_GeneratedEstTypeAttribute attrs'

            let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
            let envinner = MakeInnerEnvForTyconRef cenv envinner thisTyconRef false 

            match synTyconRepr with 

            // This unfortunate case deals with "type x = A" 
            // In F# this only defines a new type if A is not in scope 
            // as a type constructor, or if the form type A = A is used. 
            // "type x = | A" can always be used instead. 
            | TyconCoreAbbrevThatIsReallyAUnion (hasMeasureAttr,envinner,id) _ -> ()
            
            | SynTypeDefnSimpleRepr.TypeAbbrev(rhsType,_) ->

                if hasGeneratedEstTypeAttr  then 
                    let theGeneratedEstTyconRef = 
                        match rhsType with 
                        | SynType.LongIdent(tc,_m) -> 
                            let ad = AccessRightsOfEnv envinner
                            ForceRaise(ResolveTypeLongIdent cenv.nameResolver ItemOccurence.Use OpenQualified (GenerateEstTypeFlag.Yes cenv.amap.assemMap) envinner.eNameResEnv ad tc 0)
                            // check the tcref is an EST type
                        | _ -> 
                       
                             error(Failure "fail - 'Generate' must have a A.B.C on the right")

                    // record the type that has been abbreviated
                    if firstPass then
                        tycon.Data.entity_tycon_abbrev <- Some (mkAppTy theGeneratedEstTyconRef [])

                /// This case deals with ordinary type and measure abbreviations 
                elif not hasMeasureableAttr then 
                    let kind = if hasMeasureAttr then KindMeasure else KindType
                    let ty,_ = TcTypeOrMeasureAndRecover (Some kind) cenv NoNewTypars checkCxs envinner tpenv rhsType

                    if not firstPass then 
                        let ftyvs = freeInTypeLeftToRight cenv.g false ty 
                        let typars = tycon.Typars(m)
                        if ftyvs.Length <> typars.Length then 
                            errorR(Deprecated(FSComp.SR.tcTypeAbbreviationHasTypeParametersMissingOnType(),tycon.Range))
                        //elif not ((ftyvs,typars) ||> List.forall2 typarEq) then 
                        //    warning(Deprecated("The declared type parameters of this type abbreviation are not declared in the same order they are used in the type being abbreviated. Consider reordering the type parameters, or use a concrete type definition that wraps an underlying type, such as 'type C<'a,'b> = C of ...'",tycon.Range))

                    if firstPass then
                        tycon.Data.entity_tycon_abbrev <- Some ty

            | _ -> ()
        
        with e -> 
            errorRecovery e m

    // Third phase: check and publish the supr types. Run twice, once before constraints are established
    // and once after
    let private TcTyconDefnCore_Phase3_Phase5_EstablishSuperTypesAndInterfaceTypes cenv envinner tpenv inSig typeDefCores (tycons:Tycon list) pass = 
        let checkCxs = if (pass = SecondPass) then CheckCxs else NoCheckCxs
        let firstPass = (pass = FirstPass)

        // Publish the immediately declared interfaces. 
        let implementsL = 
            (typeDefCores,tycons) ||> List.map2 (fun (TyconDefnCoreIndexed(synTyconInfo,synTyconRepr,explicitImplements,_,_,_)) tycon -> 
                let (ComponentInfo(attrs,_, _,_, _, _,_,_)) = synTyconInfo
                let m = tycon.Range
                let tcref = mkLocalTyconRef tycon
                let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
                let envinner = MakeInnerEnvForTyconRef cenv envinner tcref false 
                
                let implementedTys,_ = List.mapFold (mapFoldFst (TcTypeAndRecover cenv NoNewTypars checkCxs envinner)) tpenv explicitImplements

                // Review: should skip checking constraints while checking attributes on first pass, though it's hard to imagine when that would matter 
                let attrs' = TcAttributes cenv envinner AttributeTargets.TyconDecl attrs

                if firstPass then 
                    tycon.Data.entity_attribs <- attrs';

                let implementedTys,inheritedTys = 
                    match synTyconRepr with 
                    | SynTypeDefnSimpleRepr.General (kind,inherits,slotsigs,fields,isConcrete,_,_,m) ->
                        let kind = InferTyconKind cenv.g (kind,attrs',slotsigs,fields,inSig,isConcrete,m)

                        let inherits = inherits |> List.map (fun (ty,m,_) -> (ty,m)) 
                        let inheritedTys = fst (List.mapFold (mapFoldFst (TcTypeAndRecover cenv NoNewTypars checkCxs envinner)) tpenv inherits)
                        let implementedTys,inheritedTys =   
                            match kind with 
                            | TyconInterface -> 
                                explicitImplements |> List.iter (fun (_,m) -> errorR(Error(FSComp.SR.tcInterfacesShouldUseInheritNotInterface(),m)))
                                (implementedTys @ inheritedTys),[] 
                            | _ -> implementedTys, inheritedTys
                        implementedTys,inheritedTys 
                    | SynTypeDefnSimpleRepr.Enum _ | SynTypeDefnSimpleRepr.None _ | SynTypeDefnSimpleRepr.TypeAbbrev _
                    
                    | SynTypeDefnSimpleRepr.Union _ | SynTypeDefnSimpleRepr.ILAssembly _ | SynTypeDefnSimpleRepr.Record _ -> 
                        implementedTys,[]


                // Publish interfaces, but only on the first pass, to avoid a duplicate interface check 
                if firstPass then 
                    implementedTys |> List.iter (fun (ty,m) -> PublishInterface cenv envinner.DisplayEnv tcref m false ty) ;

                attrs',inheritedTys)

        // Publish the attributes and supertype  
        (implementsL,typeDefCores,tycons) |||> List.iter3 (fun (attrs',inheritedTys) (TyconDefnCoreIndexed(_,synTyconRepr,_,_,_,_)) tycon -> 
          let m = tycon.Range
          try 
              let super = 
                  match synTyconRepr with 
                  | SynTypeDefnSimpleRepr.None _ -> None
                  | SynTypeDefnSimpleRepr.TypeAbbrev _ -> None
                  | SynTypeDefnSimpleRepr.Union _ -> None
                  | SynTypeDefnSimpleRepr.ILAssembly _ -> None
                  | SynTypeDefnSimpleRepr.Record _ -> None
                  | SynTypeDefnSimpleRepr.General (kind,_,slotsigs,fields,isConcrete,_,_,_) ->
                      let kind = InferTyconKind cenv.g (kind,attrs',slotsigs,fields,inSig,isConcrete,m)
                                           
                      match inheritedTys with 
                      | [] -> 
                          match kind with 
                          | TyconStruct -> Some(cenv.g.system_Value_typ)
                          | TyconDelegate _ -> Some(cenv.g.system_MulticastDelegate_typ )
                          | TyconHiddenRepr | TyconClass | TyconInterface -> None
                          | _ -> error(InternalError("should have inferred tycon kind",m)) 

                      | [(ty,m)] -> 
                          if not firstPass && not (match kind with TyconClass -> true | _ -> false) then 
                              errorR (Error(FSComp.SR.tcStructsInterfacesEnumsDelegatesMayNotInheritFromOtherTypes(),m)); 
                          CheckSuperType cenv ty m; 
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
              
           with e -> errorRecovery e m)

    /// Establish the fields, dispatch slots and union cases of a type
    let private TcTyconDefnCore_Phase6_EstablishRepresentation cenv envinner tpenv inSig (TyconDefnCoreIndexed(tinfo,synTyconRepr,_,_,_,tyconIdx)) (tycon:Tycon ) =
        let m = tycon.Range
        try 
            let (ComponentInfo(attrs,_, _,_, _, _,_,_)) = tinfo
            let id = tycon.Id
            let thisTyconRef = mkLocalTyconRef tycon
            let innerParent = Parent(thisTyconRef)
            let thisTyInst,thisTy = generalizeTyconRef thisTyconRef
            let attrs' = TcAttributes cenv envinner AttributeTargets.TyconDecl attrs


            let hasAbstractAttr = HasAttrib cenv.g cenv.g.attrib_AbstractClassAttribute attrs'
            let hasSealedAttr = 
                // The special case is needed for 'unit' because the 'Sealed' attribute is not yet available when this type is defined.
                if cenv.g.compilingFslib && id.idText = "Unit" then 
                    Some true
                else
                    TryFindBoolAttrib cenv.g cenv.g.attrib_SealedAttribute attrs'
            let hasMeasureAttr = HasAttrib cenv.g cenv.g.attrib_MeasureAttribute attrs'
            
            let hasMeasureableAttr = HasAttrib cenv.g cenv.g.attrib_MeasureableAttribute attrs'
            
            let hasStructLayoutAttr = HasAttrib cenv.g cenv.g.attrib_StructLayoutAttribute attrs'
            let hasAllowNullLiteralAttr = HasAttrib cenv.g cenv.g.attrib_AllowNullLiteralAttribute attrs'

            if hasAbstractAttr then 
                tycon.TypeContents.tcaug_abstract <- true;

            tycon.Data.entity_attribs <- attrs';
            let noAbstractClassAttributeCheck() = 
                if hasAbstractAttr then errorR (Error(FSComp.SR.tcOnlyClassesCanHaveAbstract(),m))
                
            let noAllowNullLiteralAttributeCheck() = 
                if hasAllowNullLiteralAttr then errorR (Error(FSComp.SR.tcRecordsUnionsAbbreviationsStructsMayNotHaveAllowNullLiteralAttribute(),m))
                
                
            let allowNullLiteralAttributeCheck() = 
                if hasAllowNullLiteralAttr then 
                    tycon.TypeContents.tcaug_super |> Option.iter (fun ty -> if not (TypeNullIsExtraValue cenv.g ty) then errorR (Error(FSComp.SR.tcAllowNullTypesMayOnlyInheritFromAllowNullTypes(),m)))
                    tycon.InterfaceTypesOfFSharpTycon |> List.iter (fun ty -> if not (TypeNullIsExtraValue cenv.g ty) then errorR (Error(FSComp.SR.tcAllowNullTypesMayOnlyInheritFromAllowNullTypes(),m)))
                
                
            let structLayoutAttributeCheck(allowed) = 
                if hasStructLayoutAttr  then 
                    if allowed then 
                        warning(PossibleUnverifiableCode(m));
                    elif thisTyconRef.Typars(m).Length > 0 then 
                        errorR (Error(FSComp.SR.tcGenericTypesCannotHaveStructLayout(),m))
                    else
                        errorR (Error(FSComp.SR.tcOnlyStructsCanHaveStructLayout(),m))
                
            let hiddenReprChecks(hasRepr) =
                 structLayoutAttributeCheck(false);
                 if hasSealedAttr = Some(false) || (hasRepr && hasSealedAttr <> Some(true) && not (id.idText = "Unit" && cenv.g.compilingFslib) ) then 
                    errorR(Error(FSComp.SR.tcRepresentationOfTypeHiddenBySignature(),m));
                 if hasAbstractAttr then 
                     errorR (Error(FSComp.SR.tcOnlyClassesCanHaveAbstract(),m))

            let noMeasureAttributeCheck() = 
                if hasMeasureAttr then errorR (Error(FSComp.SR.tcOnlyTypesRepresentingUnitsOfMeasureCanHaveMeasure(),m))
                
            let noSealedAttributeCheck(k) = 
                if hasSealedAttr = Some(true) then errorR (Error(k(),m));

            let noFieldsCheck(fields':RecdField list) = 
                match fields' with 
                | (rf :: _) -> errorR (Error(FSComp.SR.tcInterfaceTypesAndDelegatesCannotContainFields(),rf.Range))
                | _ -> ()

                
            let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
            let envinner = MakeInnerEnvForTyconRef cenv envinner thisTyconRef false 


            // Notify the Language Service about field names in record/class declaration
            let ad = AccessRightsOfEnv envinner
            let writeFakeRecordFieldsToSink (fields:RecdField list) =
                let nenv = envinner.NameEnv
                // Record fields should be visible from IntelliSense, so add fake names for them (similarly to "let a = ..")
                for fspec in (fields |> List.filter (fun fspec -> not fspec.IsCompilerGenerated)) do
                    let info = RecdFieldInfo(thisTyInst, mkNestedRecdFieldRef thisTyconRef fspec)
                    let nenv' = AddFakeNameToNameEnv fspec.Name nenv (Item.RecdField info) 
                    // Name resolution gives better info for tooltips
                    CallNameResolutionSink(fspec.Range,nenv,FreshenRecdFieldRef cenv.nameResolver m (mkNestedRecdFieldRef thisTyconRef fspec),ItemOccurence.Binding,nenv.eDisplayEnv,ad)
                    // Environment is needed for completions
                    CallEnvSink(fspec.Range, nenv', ad)

            // Notify the Language Service about constructors in discriminated union declaration
            let writeFakeUnionCtorsToSink unionCases = 
                let nenv = envinner.NameEnv
                // Constructors should be visible from IntelliSense, so add fake names for them 
                for unionCase in unionCases do
                    let info = UnionCaseInfo(thisTyInst,mkUnionCaseRef thisTyconRef unionCase.Id.idText)
                    let nenv' = AddFakeNameToNameEnv unionCase.Id.idText nenv (Item.UnionCase info) 
                    // Report to both - as in previous function
                    CallNameResolutionSink(unionCase.Range,nenv,Item.UnionCase info,ItemOccurence.Binding,nenv.eDisplayEnv,ad)
                    CallEnvSink(unionCase.Id.idRange, nenv', ad)
            
            let theTypeRepresentation, baseValOpt, safeInitInfo = 
                match synTyconRepr with 

                | SynTypeDefnSimpleRepr.None _ -> 
                    hiddenReprChecks(false)
                    noAllowNullLiteralAttributeCheck()
                    if hasMeasureAttr then 
                        let repr = TFsObjModelRepr { fsobjmodel_kind=TTyconClass; 
                                                     fsobjmodel_vslots=[];
                                                     fsobjmodel_rfields= MakeRecdFieldsTable [] }
                        Some repr, None, NoSafeInitInfo
                    else 
                        None, None, NoSafeInitInfo

                // This unfortunate case deals with "type x = A" 
                // In F# this only defines a new type if A is not in scope 
                // as a type constructor, or if the form type A = A is used. 
                // "type x = | A" can always be used instead. 
                | TyconCoreAbbrevThatIsReallyAUnion (hasMeasureAttr,envinner,id) (unionCaseName,_) ->
                          
                    structLayoutAttributeCheck(false);
                    noAllowNullLiteralAttributeCheck();
                    TcRecdUnionAndEnumDeclarations.CheckUnionCaseName  cenv unionCaseName.idText unionCaseName.idRange;
                    let unionCase = NewUnionCase unionCaseName unionCaseName.idText [] thisTy [] XmlDoc.Empty tycon.Accessibility
                    Some (MakeUnionRepr [ unionCase ]), None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.TypeAbbrev(rhsType,_) ->
                    if hasSealedAttr = Some(true) then 
                        errorR (Error(FSComp.SR.tcAbbreviatedTypesCannotBeSealed(),m));
                    noAbstractClassAttributeCheck();
                    noAllowNullLiteralAttributeCheck();
                    if hasMeasureableAttr  then 
                        let kind = if hasMeasureAttr then KindMeasure else KindType
                        let theTypeAbbrev,_ = TcTypeOrMeasureAndRecover (Some kind) cenv NoNewTypars CheckCxs envinner tpenv rhsType

                        Some(TMeasureableRepr theTypeAbbrev), None, NoSafeInitInfo
                    else 
                        None, None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.Union (_,unionCases,_) -> 
                    noMeasureAttributeCheck();
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedDU;
                    noAbstractClassAttributeCheck();
                    noAllowNullLiteralAttributeCheck();
                    structLayoutAttributeCheck(false);
                    let unionCases = TcRecdUnionAndEnumDeclarations.TcUnionCaseDecls cenv envinner innerParent thisTy tpenv unionCases
                    writeFakeUnionCtorsToSink unionCases
                    Some (MakeUnionRepr unionCases), None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.Record (_,fields,_) -> 
                    noMeasureAttributeCheck();
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedRecord;
                    noAbstractClassAttributeCheck();
                    noAllowNullLiteralAttributeCheck();
                    structLayoutAttributeCheck(true);  // these are allowed for records
                    let recdFields = TcRecdUnionAndEnumDeclarations.TcNamedFieldDecls cenv envinner innerParent false tpenv fields
                    recdFields |> CheckDuplicates (fun f -> f.Id) "field"  |> ignore
                    writeFakeRecordFieldsToSink recdFields
                    Some (TRecdRepr (MakeRecdFieldsTable recdFields)), None, NoSafeInitInfo

                | SynTypeDefnSimpleRepr.ILAssembly (s,_) -> 
                    noMeasureAttributeCheck();
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedAssemblyCode;
                    noAllowNullLiteralAttributeCheck();
                    structLayoutAttributeCheck(false);
                    noAbstractClassAttributeCheck();
                    Some (TAsmRepr s), None, NoSafeInitInfo

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
                                      let taccess = TAccess [curr_access_cpath envinner]
                                      yield NewRecdField false None id ty false false [(*no property attributes*)] [(*no field attributes *)] XmlDoc.Empty taccess (*compiler generated:*)true ]
                    
                    (userFields @ implicitStructFields) |> CheckDuplicates (fun f -> f.Id) "field"  |> ignore
                    writeFakeRecordFieldsToSink userFields
                    
                    let superTy = tycon.TypeContents.tcaug_super
                    let containerInfo = TyconContainerInfo(innerParent, thisTyconRef, thisTyconRef.Typars(m), NoSafeInitInfo)
                    let kind = InferTyconKind cenv.g (kind,attrs',slotsigs,fields,inSig,isConcrete,m)
                    match kind with 
                    | TyconHiddenRepr  -> 
                        hiddenReprChecks(true)
                        noAllowNullLiteralAttributeCheck();
                        None, None, NoSafeInitInfo
                    | _ ->

                        // Note: for a mutually recursive set we can't check this condition 
                        // until "isSealedTy" and "isClassTy" give reliable results. 
                        superTy |> Option.iter (fun ty -> 
                            let m = match inherits with | [] -> m | ((_,m,_) :: _) -> m
                            if isSealedTy cenv.g ty then 
                                errorR(Error(FSComp.SR.tcCannotInheritFromSealedType(),m))
                            elif not (isClassTy cenv.g ty) then 
                                errorR(Error(FSComp.SR.tcCannotInheritFromInterfaceType(),m)));

                        let kind = 
                            match kind with 
                              | TyconStruct -> 
                                  noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedStruct;
                                  noAbstractClassAttributeCheck();
                                  noAllowNullLiteralAttributeCheck();
                                  if nonNil slotsigs then 
                                    errorR (Error(FSComp.SR.tcStructTypesCannotContainAbstractMembers(),m)); 
                                  structLayoutAttributeCheck(true);

                                  TTyconStruct
                              | TyconInterface -> 
                                  if hasSealedAttr = Some(true) then errorR (Error(FSComp.SR.tcInterfaceTypesCannotBeSealed(),m))
                                  structLayoutAttributeCheck(false);
                                  noAbstractClassAttributeCheck();
                                  allowNullLiteralAttributeCheck();
                                  noFieldsCheck(userFields);
                                  TTyconInterface
                              | TyconClass -> 
                                  structLayoutAttributeCheck(not isIncrClass);
                                  allowNullLiteralAttributeCheck();
                                  TTyconClass
                              | TyconDelegate (ty,arity) -> 
                                  noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedDelegate;
                                  structLayoutAttributeCheck(false);
                                  noAllowNullLiteralAttributeCheck();
                                  noAbstractClassAttributeCheck();
                                  noFieldsCheck(userFields);
                                  let ty',_ = TcTypeAndRecover cenv NoNewTypars CheckCxs envinner tpenv ty
                                  let _,curriedArgInfos,returnTy,_ = GetTopValTypeInCompiledForm cenv.g (arity |> TranslateTopValSynInfo m (TcAttributes cenv envinner)  |> TranslatePartialArity []) ty' m
                                  if curriedArgInfos.Length < 1 then error(Error(FSComp.SR.tcInvalidDelegateSpecification(),m));
                                  if curriedArgInfos.Length > 1 then error(Error(FSComp.SR.tcDelegatesCannotBeCurried(),m));
                                  let ttps = thisTyconRef.Typars(m)
                                  let fparams = curriedArgInfos.Head |> List.map mkSlotParam 
                                  TTyconDelegate (mkSlotSig("Invoke",thisTy,ttps,[],[fparams], returnTy))
                              | _ -> 
                                  error(InternalError("should have inferred tycon kind",m))

                        let baseIdOpt = 
                            match synTyconRepr with 
                            | SynTypeDefnSimpleRepr.None _ -> None
                            | SynTypeDefnSimpleRepr.TypeAbbrev _ -> None
                            | SynTypeDefnSimpleRepr.Union _ -> None
                            | SynTypeDefnSimpleRepr.ILAssembly _ -> None
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

                                  CheckMemberFlags cenv.g None NewSlotsOK OverridesOK memberFlags m;
                                  
                                  let slots = fst (TcAndPublishValSpec (cenv,envinner,containerInfo,ModuleOrMemberBinding,Some(memberFlags),tpenv,valSpfn))
                                  // Multiple slots may be returned, e.g. for 
                                  //    abstract P : int with get,set
                                  
                                  for slot in slots do 
                                      yield mkLocalValRef slot ]

                        let baseValOpt = MakeAndPublishBaseVal cenv envinner baseIdOpt (superOfTycon cenv.g tycon)
                        let safeInitInfo = ComputeInstanceSafeInitInfo cenv envinner thisTyconRef.Range thisTy
                        let safeInitFields = match safeInitInfo with SafeInitField (_, fld) -> [fld] | NoSafeInitInfo -> []
                        
                        let repr = 
                            TFsObjModelRepr 
                                { fsobjmodel_kind=kind; 
                                  fsobjmodel_vslots= abstractSlots;
                                  fsobjmodel_rfields=MakeRecdFieldsTable (userFields @ implicitStructFields  @ safeInitFields) } 
                        Some(repr), baseValOpt, safeInitInfo

                | SynTypeDefnSimpleRepr.Enum (decls,m) -> 
                    let fieldTy,fields' = TcRecdUnionAndEnumDeclarations.TcEnumDecls cenv envinner innerParent thisTy decls
                    let kind = TTyconEnum
                    structLayoutAttributeCheck(false);
                    noSealedAttributeCheck FSComp.SR.tcTypesAreAlwaysSealedEnum;
                    noAllowNullLiteralAttributeCheck();
                    let vfld = NewRecdField false None (ident("value__",m))  fieldTy false false [] [] XmlDoc.Empty taccessPublic true
                    
                    if not (ListSet.contains (typeEquiv cenv.g) fieldTy [ cenv.g.int32_ty; cenv.g.int16_ty; cenv.g.sbyte_ty; cenv.g.int64_ty; cenv.g.char_ty; cenv.g.bool_ty; cenv.g.uint32_ty; cenv.g.uint16_ty; cenv.g.byte_ty; cenv.g.uint64_ty ]) then 
                        errorR(Error(FSComp.SR.tcInvalidTypeForLiteralEnumeration(),m));

                    writeFakeRecordFieldsToSink fields' 
                    let repr = 
                        TFsObjModelRepr 
                            { fsobjmodel_kind=kind; 
                              fsobjmodel_vslots=[];
                              fsobjmodel_rfields= MakeRecdFieldsTable (vfld :: fields') }
                    Some(repr), None, NoSafeInitInfo
            
            tycon.Data.entity_tycon_repr <- theTypeRepresentation;
            // We check this just after establishing the representation
            if TyconHasUseNullAsTrueValueAttribute cenv.g tycon && not (CanHaveUseNullAsTrueValueAttribute cenv.g tycon) then 
                errorR(Error(FSComp.SR.tcInvalidUseNullAsTrueValue(),m))
                
            // validate ConditionalAttribute, should it be applied (it's only valid on a type if the type is an attribute type)
            match attrs' |> List.tryFind (IsMatchingAttrib cenv.g cenv.g.attrib_ConditionalAttribute) with
            | Some _ ->
                if not(ExistsInEntireHierarchyOfType (fun t -> typeEquiv cenv.g t (TType_app(cenv.g.tcref_System_Attribute,[]))) cenv.g cenv.amap m FirstIntfInst thisTy) then
                    errorR(Error(FSComp.SR.tcConditionalAttributeUsage(),m));
            | _ -> ()         
                   
            (baseValOpt, safeInitInfo, tyconIdx)
        with e -> 
            errorRecovery e m; 
            None, NoSafeInitInfo, tyconIdx

    /// Check that a set of type definitions is free of cycles in abbreviations
    let private CheckForCyclicAbbreviations _cenv tycons = 

        let edgesFrom (tycon:Tycon) =

            let rec accInAbbrevType ty acc  = 
                match stripTyparEqns ty with 
                | TType_tuple l -> accInAbbrevTypes l acc
                | TType_ucase (UCRef(tc,_),tinst) 
                | TType_app (tc,tinst) -> 
                    let tycon2 = tc.Deref
                    let acc = accInAbbrevTypes tinst  acc
                    // Record immediate recursive references 
                    if ListSet.contains (===) tycon2 tycons  then 
                        (tycon,tycon2) ::acc 
                    // Expand the representation of abbreviations 
                    elif tc.IsTypeAbbrev  then
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
                | MeasureCon tc when ListSet.contains (===) tc.Deref tycons  ->  
                    (tycon, tc.Deref) :: acc
                | MeasureCon tc when tc.IsTypeAbbrev  ->              
                    accInMeasure (reduceTyconRefAbbrevMeasureable tc) acc
                | MeasureProd (ms1, ms2) -> accInMeasure ms1 (accInMeasure ms2 acc)
                | MeasureInv ms -> accInMeasure ms acc
                | _ -> acc

            and accInAbbrevTypes tys acc = 
                List.foldBack accInAbbrevType tys acc
                
            let acc = []
            let acc = 
                match tycon.TypeAbbrev with 
                | None -> acc
                | Some ty -> 
                    //if not cenv.isSig && not cenv.haveSig && (tycon.Accessibility <> taccessPublic || tycon.TypeReprAccessibility <> taccessPublic) then 
                    //   errorR(Error(FSComp.SR.tcTypeAbbreviationMustBePublic(),tycon.Range));
                    accInAbbrevType ty acc

            acc

        let edges = List.collect edgesFrom tycons
        let graph = Graph<Tycon, Stamp> ((fun tc -> tc.Stamp), tycons, edges)
        graph.IterateCycles (fun path -> 
            let tycon = path.Head 
            // The thing is cyclic. Set the abbreviation and representation to be "None" to stop later VS crashes
            tycon.Data.entity_tycon_abbrev <- None
            tycon.Data.entity_tycon_repr <- None 
            errorR(Error(FSComp.SR.tcTypeDefinitionIsCyclic(),tycon.Range)));


    /// Check that a set of type definitions is free of inheritance cycles
    let CheckForCyclicStructsAndInheritance cenv tycons =
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
                        (structTyInst,tinst2) ||> List.lengthsEqAndForall2 (fun ty1 ty2 -> isTyparTy cenv.g ty1 &&
                                                                                           isTyparTy cenv.g ty2 &&
                                                                                           typarEq (destTyparTy cenv.g ty1) (destTyparTy cenv.g ty2))
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
                    let fspecs = structTycon.AllFieldsAsList |> List.filter (fun fspec -> includeStaticFields || not fspec.IsStatic)
                    let doneTypes,acc = List.foldBack (accStructField structTycon tinst) fspecs (doneTypes,acc)
                    doneTypes,acc
            and accStructInstanceFields ty structTycon tinst (doneTypes,acc) = accStructFields false ty structTycon tinst (doneTypes,acc)
            and accStructAllFields      ty structTycon tinst (doneTypes,acc) = accStructFields true  ty structTycon tinst (doneTypes,acc)

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
                List.foldBack insertEdgeToType tycon.InterfaceTypesOfFSharpTycon acc
            acc
        let edges = (List.collect edgesFrom tycons)
        let graph = Graph<Tycon, Stamp> ((fun tc -> tc.Stamp), tycons, edges)
        graph.IterateCycles (fun path -> 
            let tycon = path.Head 
            // The thing is cyclic. Set the abbreviation and representation to be "None" to stop later VS crashes
            tycon.Data.entity_tycon_abbrev <- None
            tycon.Data.entity_tycon_repr <- None 
            errorR(Error(FSComp.SR.tcTypeDefinitionIsCyclicThroughInheritance(),tycon.Range)));
        
    let isAugmentationTyconDefnRepr x = match x with (SynTypeDefnSimpleRepr.General(TyconAugmentation,_,_,_,_,_,_,_)) -> true | _ -> false
    
    let TcTyconDefnCores cenv env inSig parent tpenv (typeDefCores:TyconDefnCore list, m, scopem) =

        // Add indexes
        let typeDefCores = typeDefCores |> List.mapi (fun i (TyconDefnCore(info,repr,m,preEstablishedHasDefaultCtor,hasSelfReferentialCtor)) -> TyconDefnCoreIndexed(info,repr,m,preEstablishedHasDefaultCtor,hasSelfReferentialCtor,i))
        // Skip augmentations
        let tdefsForAugmentations, typeDefCores = typeDefCores |> List.partition (fun (TyconDefnCoreIndexed(_,repr,_,_,_,_)) -> isAugmentationTyconDefnRepr repr)

        // First define the type constructors and the abbreviations, if any. 
        let tycons = typeDefCores |> List.map (TcTyconDefnCore_Phase0_BuildInitialTycon cenv env parent)

        // Publish the preliminary tycons 
        tycons |> List.iter (fun tycon -> 
            // recheck these in case type is a duplicate in a mutually recursive set
            CheckForDuplicateConcreteType cenv env tycon.LogicalName tycon.Range
            CheckForDuplicateModule cenv env tycon.LogicalName tycon.Range
            PublishTypeDefn cenv env tycon);

        // Add them to the environment, though this does not add the fields and 
        // constructors (because we haven't established them yet). 
        // We re-add them to the original environment later on. 
        // We don't report them to the Language Service yet as we don't know if 
        // they are well-formed (e.g. free of abbreviation cycles - see bug 952) 
        let envinner = AddLocalTycons cenv.g cenv.amap scopem tycons env


        // Establish the kind of each type constructor 
        // Here we run InferTyconKind and record partial information about the kind of the type constructor. 
        // This means TyconObjModelKind is set, which means isSealedTy, isInterfaceTy etc. give accurate results. 
        (typeDefCores,tycons) ||> List.iter2 (TcTyconDefnCore_Phase1_EstablishBasicKind cenv inSig envinner)
            
        // Establish the abbreviations (no constraint checking, because constraints not yet established)
        (typeDefCores,tycons) ||> List.iter2 (TcTyconDefnCore_Phase2_Phase4_EstablishAbbreviations cenv envinner tpenv FirstPass)

        // Check for cyclic abbreviations. If this succeeds we can start reducing abbreviations safely.
        CheckForCyclicAbbreviations cenv tycons;

        // Establish the super type and interfaces  (no constraint checking, because constraints not yet established)     
        TcTyconDefnCore_Phase3_Phase5_EstablishSuperTypesAndInterfaceTypes cenv envinner tpenv inSig typeDefCores tycons FirstPass;

        // Add the interface and member declarations for hash/compare. Because this adds interfaces, this may let constraints 
        // be satisfied, so we have to do this prior to checking any constraints.

        // Find all the field types in all the structrual types
        let structuralTypes = (typeDefCores,tycons) ||> List.map2 (GetStructuralElementsOfTyconDefn cenv envinner tpenv)
        
        let scSet = TyconConstraintInference.InferSetOfTyconsSupportingComparable cenv envinner structuralTypes tycons
        let seSet = TyconConstraintInference.InferSetOfTyconsSupportingEquatable cenv envinner structuralTypes tycons

        tycons |> List.iter (AddAugmentationDeclarations.AddGenericHashAndComparisonDeclarations cenv env scSet seSet)

        // Check and publish the explicit constraints. 
        let checkExplicitConstraints checkCxs = 
            (typeDefCores,tycons) ||> List.iter2 (fun (TyconDefnCoreIndexed(synTyconInfo,_,_,_,_,_)) tycon -> 
                let (ComponentInfo(_,_, wcs,_,_,_, _,_)) = synTyconInfo
                let envinner = AddDeclaredTypars CheckForDuplicateTypars (tycon.Typars(m)) envinner
                let thisTyconRef = mkLocalTyconRef tycon
                let envinner = MakeInnerEnvForTyconRef cenv envinner thisTyconRef false 
                try TcTyparConstraints cenv NoNewTypars checkCxs envinner tpenv  wcs |> ignore
                with e -> errorRecovery e m);

        checkExplicitConstraints NoCheckCxs;

        // No inferred constraints allowed on declared typars 
        tycons |> List.iter (fun tc -> tc.Typars(m) |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m));
        
        // OK, now recheck the abbreviations, super/interface and explicit constraints types (this time checking constraints)
        (typeDefCores,tycons) ||> List.iter2 (TcTyconDefnCore_Phase2_Phase4_EstablishAbbreviations cenv envinner tpenv SecondPass)
        TcTyconDefnCore_Phase3_Phase5_EstablishSuperTypesAndInterfaceTypes cenv envinner tpenv inSig typeDefCores tycons SecondPass;
        checkExplicitConstraints CheckCxs;

        // Now all the type parameters, abbreviations, constraints and kind information is established.
        // Now do the representations. Each baseValOpt is a residue from the representation which is potentially available when
        // checking the members.
        let baseValOpts, safeInitValOpts = 
            let baseValOptsForTycons = (typeDefCores,tycons) ||> List.map2 (TcTyconDefnCore_Phase6_EstablishRepresentation cenv envinner tpenv inSig)
            // Make sure we return a 'None' for each augmentation as well. These can't use 'base'
            let baseValOptsForAugmentations = tdefsForAugmentations |> List.map (fun (TyconDefnCoreIndexed(_,_,_,_,_,idx)) -> (None, NoSafeInitInfo, idx))
            // Collect them up, sort them by index
            (baseValOptsForAugmentations @ baseValOptsForTycons) |> List.sortBy p33 |> List.map (fun (a,b,_) -> (a,b)) |> List.unzip
                
        // Now check for cyclic structs and inheritance. It's possible these should be checked as separate conditions. 
        CheckForCyclicStructsAndInheritance cenv tycons;

        // Add the tycons again to the environment (again) - this will add the constructors and fields. 
        let env = AddLocalTyconsAndReport cenv.g cenv.amap scopem tycons env

        (tycons, env, baseValOpts, safeInitValOpts)

end // module EstablishTypeDefinitionCores


module TcTypeDeclarations = begin

    /// Given a type definition, compute whether its members form an extension of an existing type, and if so if it is an 
    /// intrinsic or extrinsic extension
    let private ComputeTyconDeclKind isAtOriginalTyconDefn cenv env inSig m typars cs longPath = 
        let ad = AccessRightsOfEnv env
        let tcref = 
            match ResolveTypeLongIdent cenv.nameResolver ItemOccurence.Binding OpenQualified GenerateEstTypeFlag.No env.eNameResEnv ad longPath (List.length typars) with
            | Result res -> res
            | res when inSig && longPath.Length = 1 ->
                errorR(Deprecated(FSComp.SR.tcReservedSyntaxForAugmentation(),m));
                ForceRaise res
            | res -> ForceRaise res            

        let isInterfaceOrDelegateOrEnum = 
            tcref.Deref.IsFSharpInterfaceTycon || 
            tcref.Deref.IsFSharpDelegateTycon ||
            tcref.Deref.IsFSharpEnumTycon

        let isInSameModuleOrNamespace = 
             match env.eMtypeAcc.Value.TypesByMangledName.TryFind(tcref.LogicalName) with 
              | Some tycon -> (tyconOrder.Compare(tcref.Deref,tycon) = 0)
              | None -> 
                    //false
                    // There is a special case we allow when compiling FSharp.Core.dll which permits interface implementations across namespace fragments
                    (cenv.g.compilingFslib && tcref.LogicalName.StartsWith("Tuple`"))
        
        let reqTypars = tcref.Typars(m)

        // Member definitions are intrinsic (added directly to the type) if:
        // a) For interfaces, only if it is in the original defn.
        //    Augmentations to interfaces via partial type defns will always be extensions, e.g. extension members on interfaces.
        // b) For other types, if the type is isInSameModuleOrNamespace
        let declKind,typars = 
            if isAtOriginalTyconDefn then 
                ModuleOrMemberBinding, reqTypars

            elif isInSameModuleOrNamespace && not isInterfaceOrDelegateOrEnum then 
                IntrinsicExtensionBinding, reqTypars
            else 
                if isInSameModuleOrNamespace && isInterfaceOrDelegateOrEnum then 
                    errorR(Error(FSComp.SR.tcMembersThatExtendInterfaceMustBePlacedInSeparateModule(),tcref.Range))
                let nReqTypars = reqTypars.Length
                if nReqTypars <> typars.Length then 
                    // not recoverable
                    error(Error(FSComp.SR.tcDeclaredTypeParametersForExtensionDoNotMatchOriginal(tcref.DisplayNameWithUnderscoreTypars),m))

                let declaredTypars = TcTyparDecls cenv env typars
                let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTypars env
                let _tpenv = TcTyparConstraints cenv NoNewTypars CheckCxs envinner emptyUnscopedTyparEnv cs
                declaredTypars |> List.iter (SetTyparRigid cenv.g env.DisplayEnv m);
                if not (typarsAEquiv cenv.g TypeEquivEnv.Empty reqTypars declaredTypars) then 
                    errorR(Error(FSComp.SR.tcDeclaredTypeParametersForExtensionDoNotMatchOriginal(tcref.DisplayNameWithUnderscoreTypars),m))
                ExtrinsicExtensionBinding, declaredTypars


        declKind, tcref, typars


    let private isMember          = function SynMemberDefn.Member _   -> true | _ -> false
    let private isImplicitCtor    = function SynMemberDefn.ImplicitCtor _    -> true | _ -> false
    let private isImplicitInherit = function SynMemberDefn.ImplicitInherit _ -> true | _ -> false
    let private isLetBindings     = function SynMemberDefn.LetBindings _     -> true | _ -> false
    let private isAbstractSlot    = function SynMemberDefn.AbstractSlot _          -> true | _ -> false
    let private isInterface       = function SynMemberDefn.Interface _        -> true | _ -> false
    let private isInherit         = function SynMemberDefn.Inherit _          -> true | _ -> false
    let private isField           = function SynMemberDefn.ValField _            -> true | _ -> false
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
            let ds = match ds with
                     | d::ds when isImplicitInherit d -> ds  // skip inherit call if it comes next 
                     | ds -> ds
            let _ ,ds = ds |> List.takeUntil (isLetBindings >> not) 
            let _,ds = ds |> List.takeUntil (allFalse [isMember;isAbstractSlot;isInterface]) 
            match ds with
             | SynMemberDefn.Member (_,m)       :: _ -> errorR(InternalError("List.takeUntil is wrong, have binding",m))
             | SynMemberDefn.AbstractSlot (_,_,m)            :: _ -> errorR(InternalError("List.takeUntil is wrong, have slotsig",m))
             | SynMemberDefn.Interface (_,_,m)          :: _ -> errorR(InternalError("List.takeUntil is wrong, have interface",m))
             | SynMemberDefn.ImplicitCtor (_,_,_,_,m)  :: _ -> errorR(InternalError("implicit class construction with two implicit constructions",m))
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
             | SynMemberDefn.LetBindings (_,false,_,m) :: _ -> errorR(Error(FSComp.SR.tcLetAndDoRequiresImplicitConstructionSequence(),m))
             | SynMemberDefn.AbstractSlot (_,_,m)            :: _ 
             | SynMemberDefn.Interface (_,_,m)          :: _ 
             | SynMemberDefn.Inherit (_,_,m)            :: _ 
             | SynMemberDefn.ValField (_,m)                :: _ 
             | SynMemberDefn.NestedType (_,_,m)              :: _ -> errorR(InternalError("CheckMembersForm: List.takeUntil is wrong",m))
             | _ -> ()
                     

    /// Parallels SplitTyconSignature/SplitTyconDefn]
    /// Separates the definition into core (shape) and body.
    /// core = synTyconInfo,simpleRepr,interfaceTypes
    ///        where simpleRepr can contain inherit type, declared fields and virtual slots.
    /// body = members
    ///        where members contain methods/overrides, also implicit ctor, inheritCall and local definitions.
    ///------
    /// The tinfos arg are the enclosing types when processing nested types...
    /// The tinfos arg is not currently used... just stacked up.
    let rec private SplitTyconDefn tinfos (TypeDefn(synTyconInfo,trepr,extraMembers,_)) =
        let implements1 = List.choose (function SynMemberDefn.Interface (ty,_,_) -> Some(ty,ty.Range) | _ -> None) extraMembers
        match trepr with
        | SynTypeDefnRepr.ObjectModel(kind,cspec,m) ->
            CheckMembersForm cspec;
            let fields      = cspec |> List.choose (function SynMemberDefn.ValField (f,_) -> Some(f) | _ -> None)
            let implements2 = cspec |> List.choose (function SynMemberDefn.Interface (ty,_,_) -> Some(ty,ty.Range) | _ -> None)
            let inherits    = cspec |> List.choose (function 
                                                          | SynMemberDefn.Inherit          (ty,idOpt,m)     -> Some(ty,m,idOpt)
                                                          | SynMemberDefn.ImplicitInherit (ty,_,idOpt,m) -> Some(ty,m,idOpt)
                                                          | _ -> None)
            let tycons      = cspec |> List.choose (function SynMemberDefn.NestedType (x,_,_) -> Some(x) | _ -> None)
            let slotsigs    = cspec |> List.choose (function SynMemberDefn.AbstractSlot (x,y,_) -> Some(x,y) | _ -> None)
            let members     = cspec |> List.filter (function 
                                                        | SynMemberDefn.Interface _
                                                        | SynMemberDefn.Member _ 
                                                        | SynMemberDefn.LetBindings _
                                                        | SynMemberDefn.ImplicitCtor _
                                                        | SynMemberDefn.Open _
                                                        | SynMemberDefn.ImplicitInherit _ -> true
                                                        | SynMemberDefn.NestedType  (_,_,m)  -> error(Error(FSComp.SR.tcTypesCannotContainNestedTypes(),m)); false
                                                        | SynMemberDefn.ValField _   -> false // covered above 
                                                        | SynMemberDefn.Inherit _ -> false // covered above 
                                                        | SynMemberDefn.AbstractSlot _ -> false // covered above 
                                                        )
            let a,b = SplitTyconDefns (tinfos @ [synTyconInfo]) tycons

            let isConcrete = 
                members |> List.exists (function 
                    | SynMemberDefn.Member(Binding(_,_,_,_,_,_,SynValData(Some(memberFlags),_,_),_,_,_,_,_),_) -> not memberFlags.IsDispatchSlot 
                    | SynMemberDefn.Interface (_,defOpt,_) -> isSome defOpt
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

            // An bit of code to pre-determine if a type has a nullary constructor, prior to establishing the 
            // members of the type
            let preEstablishedHasDefaultCtor = 
                members |> List.exists (function 
                    | SynMemberDefn.Member(Binding(_,_,_,_,_,_,SynValData(Some(memberFlags),_,_),SynPatForConstructorDecl(SynPatForNullaryArgs),_,_,_,_),_) -> 
                        memberFlags.MemberKind=MemberKind.Constructor 
                    | SynMemberDefn.ImplicitCtor (_,_,spats,_, _) -> isNil spats
                    | _ -> false)
                    
            let core = TyconDefnCore(synTyconInfo, SynTypeDefnSimpleRepr.General(kind,inherits,slotsigs,fields,isConcrete,isIncrClass,implicitCtorSynPats,m), implements2@implements1, preEstablishedHasDefaultCtor, hasSelfReferentialCtor)

            core :: a, members :: b

        | SynTypeDefnRepr.Simple(r,_) -> 
            let members = []
            let core = TyconDefnCore(synTyconInfo,r,implements1,false,false)
            [ core ],[ members ]

    and private SplitTyconDefns tinfos tycons = 
        let a,b = List.unzip (List.map (SplitTyconDefn tinfos) tycons)
        List.concat a, List.concat b 

    let private PrepareTyconMemberDefns isAtOriginalTyconDefn cenv env  (synTyconInfo, baseValOpt, safeInitInfo, members, m) =
        let (ComponentInfo(_,typars, cs,longPath, _, _, _,_)) = synTyconInfo

        let declKind,tcref, declaredTyconTypars = ComputeTyconDeclKind isAtOriginalTyconDefn  cenv env false m typars cs longPath

        let newslotsOK = (if isAtOriginalTyconDefn && tcref.IsFSharpObjectModelTycon then NewSlotsOK else NoNewSlots) // NewSlotsOK only on fsobjs 

        if nonNil(members) && tcref.IsTypeAbbrev then errorR(Error(FSComp.SR.tcTypeAbbreviationsCannotHaveAugmentations(),(trimRangeToLine m)));

        TyconMemberData(declKind, tcref, baseValOpt, safeInitInfo, declaredTyconTypars, members, m, newslotsOK)

    //-------------------------------------------------------------------------
    // Bind type definitions - main
    //------------------------------------------------------------------------- 

    let TcTyconDefns cenv env parent tpenv (typeDefs: SynTypeDefn list,m,scopem) =
        let typeDefCores,tyconDefnMembers = SplitTyconDefns [] typeDefs
        let tycons, env, baseValOpts, safeInitValOpts = EstablishTypeDefinitionCores.TcTyconDefnCores cenv env false parent tpenv (typeDefCores,m,scopem)
        let augments = 
            (List.zip typeDefs typeDefCores, List.zip baseValOpts safeInitValOpts, tyconDefnMembers) |||> List.map3 (fun (TypeDefn(synTyconInfo,_,extraMembers,m), TyconDefnCore(_,repr,_,_,_)) (baseValOpt, safeInitInfo) members -> 
                   let isAtOriginalTyconDefn = not (EstablishTypeDefinitionCores.isAugmentationTyconDefnRepr repr)
                   PrepareTyconMemberDefns isAtOriginalTyconDefn cenv env (synTyconInfo, baseValOpt, safeInitInfo, members@extraMembers,m))
              
        let valExprBuilders,env = TcTyconMemberDefns cenv env parent m scopem augments

        // Note: generating these bindings must come after generating the members, since some in the case of structs some fields
        // may be added by generating the implicit construction syntax 
        let binds = tycons |> List.collect (AddAugmentationDeclarations.AddGenericHashAndComparisonBindings cenv)
        let binds3 = tycons |> List.collect (AddAugmentationDeclarations.AddGenericEqualityBindings cenv env)

        // Check for cyclic structs and inheritance all over again, since we may have added some fields to the struct when generating the implicit construction syntax 
        EstablishTypeDefinitionCores.CheckForCyclicStructsAndInheritance cenv tycons;

        (binds @ valExprBuilders @ binds3),tycons,env  


    //-------------------------------------------------------------------------
    // Bind type specifications
    //------------------------------------------------------------------------- 

    /// Parallels split_tycon[Spfn/Defn] 
    let rec private SplitTyconSignature tinfos (TypeDefnSig(synTyconInfo,trepr,extraMembers,_)) = 
        let implements1 = 
            extraMembers |> List.choose (function SynMemberSig.Interface (f,m) -> Some(f,m) | _ -> None) 
        match trepr with
        | SynTypeDefnSigRepr.ObjectModel(kind,cspec,m) -> 
            let fields      = cspec |> List.choose (function SynMemberSig.ValField (f,_) -> Some(f) | _ -> None)
            let implements2 = cspec |> List.choose (function SynMemberSig.Interface (ty,m) -> Some(ty,m) | _ -> None)
            let inherits    = cspec |> List.choose (function SynMemberSig.Inherit (ty,_) -> Some(ty,m,None) | _ -> None)
            let nestedTycons = cspec |> List.choose (function SynMemberSig.NestedType (x,_) -> Some(x) | _ -> None)
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

            // An bit of code to pre-determine if a type has a nullary constructor, prior to establishing the 
            // members of the type
            let preEstablishedHasDefaultCtor = 
                members |> List.exists (function 
                    | SynMemberSig.Member (valSpfn,memberFlags,_) -> 
                        memberFlags.MemberKind=MemberKind.Constructor  && 
                        // Note, this is a syntactic approximation
                        (match valSpfn.SynType, valSpfn.SynInfo.ArgInfos with 
                         | SynType.Fun (SynType.LongIdent ([id], _), _, _), [[_]] when id.idText = "unit" ->  true
                         | _ -> false) 
                    | _ -> false) 

            let hasSelfReferentialCtor = false
            
            let a,b = nestedTycons |> SplitTyconSignatures (tinfos @ [synTyconInfo]) 
            
            let tyconCore = TyconDefnCore (synTyconInfo, SynTypeDefnSimpleRepr.General(kind,inherits,slotsigs,fields,isConcrete,false,None,m),implements2@implements1,preEstablishedHasDefaultCtor,hasSelfReferentialCtor)

            [ tyconCore ] @ a,
            [ (synTyconInfo,true,members@extraMembers) ] @ b

        // 'type X with ...' in a signature is always interpreted as an extrinsic extension.
        // Representation-hidden types with members and interfaces are written 'type X = ...' 
        | SynTypeDefnSigRepr.Simple(SynTypeDefnSimpleRepr.None _,_) when nonNil extraMembers -> 
            let isAtOriginalTyconDefn = false
            [],[ (synTyconInfo,isAtOriginalTyconDefn,extraMembers) ]

        | SynTypeDefnSigRepr.Simple(r,_) -> 
            let tyconCore = TyconDefnCore (synTyconInfo, r, implements1, false, false)
            [ tyconCore ],[ (synTyconInfo,true,extraMembers) ] 

    and private SplitTyconSignatures tinfos tycons = 
        let a,b = tycons |> List.map (SplitTyconSignature tinfos) |> List.unzip 
        List.concat a, List.concat b 

    let private TcTyconSignatureMemberSpecs cenv env parent tpenv tyconDefnMembers =
        (tpenv, tyconDefnMembers) ||> List.mapFold (fun tpenv (synTyconInfo,isAtOriginalTyconDefn,members) -> 
            let (ComponentInfo(_,typars,cs,longPath, _, _, _,m)) = synTyconInfo
            let declKind,tcref,declaredTyconTypars = ComputeTyconDeclKind isAtOriginalTyconDefn cenv env true m typars cs longPath

            let envinner = AddDeclaredTypars CheckForDuplicateTypars declaredTyconTypars env
            let envinner = MakeInnerEnvForTyconRef cenv envinner tcref (declKind = ExtrinsicExtensionBinding) 

            TcTyconMemberSpecs cenv envinner (TyconContainerInfo(parent, tcref, declaredTyconTypars, NoSafeInitInfo)) declKind tpenv members)

    let TcTyconSignatures cenv env parent tpenv (tspecs:SynTypeDefnSig list,m,scopem) =
        let typeDefCores,tyconDefnMembers = SplitTyconSignatures [] tspecs
        let _, env, _, _ = EstablishTypeDefinitionCores.TcTyconDefnCores cenv env true parent tpenv (typeDefCores,m,scopem)
        let _ = TcTyconSignatureMemberSpecs cenv env parent tpenv tyconDefnMembers
        env
end

//-------------------------------------------------------------------------
// Bind module types
//------------------------------------------------------------------------- 

let AdjustModuleName modKind nm = (match modKind with FSharpModuleWithSuffix -> nm^FSharpModuleSuffix | _ -> nm)


let rec TcSignatureElement cenv parent endm (env: TcEnv) e : Eventually<TcEnv> =
  eventually {
    try 
        match e with 
        | SynModuleSigDecl.Exception (edef,m) ->
            let scopem = unionRanges m.EndRange endm
            let _,_,_,env = TcExceptionDeclarations.TcExnSignature cenv env parent emptyUnscopedTyparEnv (edef,scopem)
            return env

        | SynModuleSigDecl.Types (tspecs,m) -> 
            let scopem = unionRanges m endm
            let env = TcTypeDeclarations.TcTyconSignatures cenv env parent emptyUnscopedTyparEnv (tspecs,m,scopem)
            return env 

        | SynModuleSigDecl.Open (mp,m) -> 
            let scopem = unionRanges m.EndRange endm
            let env = TcOpenDecl cenv.g cenv.amap m scopem env mp
            return env

        | SynModuleSigDecl.Val (vspec,m) -> 
            let parentModule = 
                match parent with 
                | ParentNone -> error(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),vspec.RangeOfId)) 
                | Parent p -> p
            let containerInfo = ModuleOrNamespaceContainerInfo(parentModule)
            let idvs,_ = TcAndPublishValSpec (cenv,env,containerInfo,ModuleOrMemberBinding,None,emptyUnscopedTyparEnv,vspec)
            let scopem = unionRanges m endm
            let env = List.foldBack (AddLocalVal scopem) idvs env
            return env

        | SynModuleSigDecl.NestedModule(ComponentInfo(attribs,_parms, _constraints,longPath,xml,_,vis,im),mdefs,m) ->
            let id = ComputeModuleName longPath
            let vis,_ = ComputeAccessAndCompPath env None im vis parent
            let! (mspec,_) = TcModuleOrNamespaceSignature cenv env (id,true,mdefs,xml,attribs,vis,m)
            let scopem = unionRanges m endm
            PublishModuleDefn cenv env mspec; 
            let env = AddLocalSubModule cenv.g cenv.amap m  scopem env mspec
            return env
            
        | SynModuleSigDecl.ModuleAbbrev (id,p,m) -> 
            let ad = AccessRightsOfEnv env
            let mvvs = ForceRaise (ResolveLongIndentAsModuleOrNamespace OpenQualified env.eNameResEnv ad p)
            let scopem = unionRanges m endm
            let modrefs = mvvs |> List.map p23 
            if modrefs.Length > 0 && modrefs |> List.forall (fun modref -> modref.IsNamespace) then 
                errorR(Error(FSComp.SR.tcModuleAbbreviationForNamespace(fullDisplayTextOfModRef (List.head modrefs)),m));
            let modrefs = modrefs |> List.filter (fun modref -> not modref.IsNamespace)
            modrefs |> List.iter (fun modref -> CheckEntityAttributes cenv.g modref m |> CommitOperationResult);        
            
            let env = 
                if modrefs.Length > 0 then AddModuleAbbreviation scopem id modrefs env 
                else env
            return env

        | SynModuleSigDecl.HashDirective _ -> 
            return env


        | SynModuleSigDecl.NamespaceFragment (ModuleOrNamespaceSig(lid,isModule,defs,xml,attribs,vis,m)) -> 

            do for id in lid do 
                 CheckNamespaceModuleOrTypeName cenv id;

            let enclosingNamespacePath = if isModule then fst (List.frontAndBack lid) else lid
            let defs = 
                if isModule then 
                    [SynModuleSigDecl.NestedModule(ComponentInfo(attribs,[], [],[snd(List.frontAndBack lid)],xml,false,vis,m),defs,m)] 
                else 
                    defs
            let envinner = LocateEnv cenv.topCcu env enclosingNamespacePath
            let envinner = ImplicitlyOpenOwnNamespace cenv.g cenv.amap m enclosingNamespacePath envinner
            
            let! envAtEnd = TcSignatureElements cenv ParentNone m.EndRange envinner xml defs
            let env = 
                if isNil enclosingNamespacePath then 
                    envAtEnd
                else
                    let modulTypeRoot = BuildRootModuleType enclosingNamespacePath envinner.eCompPath !(envinner.eMtypeAcc)

                    let env = AddLocalRootModuleOrNamespace cenv.g cenv.amap m env modulTypeRoot
                    // Publish the combined module type
                    env.eMtypeAcc := combineModuleOrNamespaceTypeList [] m [!(env.eMtypeAcc); modulTypeRoot]
                    env

            return env
            
    with e -> 
        errorRecovery e endm; 
        return env
  }

and TcSignatureElements cenv parent endm env xml defs = 
    eventually {
        // Ensure the deref_nlpath call in UpdateAccModuleOrNamespaceType succeeds 
        if cenv.compilingCanonicalFslibModuleType then 
            ensureCcuHasModuleOrNamespaceAtPath cenv.topCcu env.ePath env.eCompPath (xml.ToXmlDoc());

        return! Eventually.fold (TcSignatureElement cenv parent endm) env defs
    }

and ComputeModuleOrNamespaceKind g isModule attribs = 
    if not isModule then Namespace 
    elif ModuleNameIsMangled g attribs then FSharpModuleWithSuffix 
    else FSharpModule

and TcModuleOrNamespaceSignature cenv env (id:Ident,isModule,defs,xml,attribs,vis,m) =
  eventually {
    let attribs = TcAttributes cenv env AttributeTargets.ModuleDecl attribs
    CheckNamespaceModuleOrTypeName cenv id;
    let modKind = ComputeModuleOrNamespaceKind cenv.g isModule attribs
    if isModule then CheckForDuplicateConcreteType cenv env (AdjustModuleName modKind id.idText) id.idRange;
    if isModule then CheckForDuplicateModule cenv env id.idText id.idRange;

    // Now typecheck the signature, accumulating and then recording the submodule description. 
    let id = ident (AdjustModuleName modKind id.idText, id.idRange)

    let mspec = NewModuleOrNamespace  (Some env.eCompPath) vis id (xml.ToXmlDoc()) attribs (notlazy (NewEmptyModuleOrNamespaceType modKind)) 

    let innerParent = mkLocalModRef mspec
    
    let! (mtyp,envAtEnd) = TcModuleOrNamespaceSignatureElements cenv (Parent innerParent) env (id,modKind,defs,m,xml)

#if DEBUG
    if !verboseStamps then 
        dprintf "TcModuleOrNamespaceSignature: %s#%d, vis = %s\n" mspec.LogicalName mspec.Stamp (stringOfAccess vis);
#endif

    mspec.Data.entity_modul_contents <- notlazy mtyp; 
    
    return (mspec, envAtEnd)
  }

and TcModuleOrNamespaceSignatureElements cenv parent env (id,modKind,defs,m:range,xml) =

  eventually {
    let endm = m.EndRange // use end of range for errors 

    // Create the module type that will hold the results of type checking.... 
    let envinner,mtypeAcc = MakeInnerEnv env id modKind

    // Now typecheck the signature, using mutation to fill in the submodule description. 
    let! envAtEnd = TcSignatureElements cenv parent endm envinner xml defs
    
    // mtypeAcc has now accumulated the module type 
    return !mtypeAcc, envAtEnd
  }
    
//-------------------------------------------------------------------------
// Bind definitions within modules
//------------------------------------------------------------------------- 

let rec TcModuleOrNamespaceElement cenv parent scopem env e = // : ((ModuleOrNamespaceExpr list -> ModuleOrNamespaceExpr list) * _) * tcEnv =
  eventually {
    let tpenv = emptyUnscopedTyparEnv
    //printfn "----------\nCHECKING, e = %+A\n------------------\n" e
    try 
      match e with 

      | SynModuleDecl.ModuleAbbrev (id,p,m) -> 
          let ad = AccessRightsOfEnv env
          let mvvs = ForceRaise (ResolveLongIndentAsModuleOrNamespace OpenQualified env.eNameResEnv ad p)
          let modrefs = mvvs |> List.map p23 
          if modrefs.Length > 0 && modrefs |> List.forall (fun modref -> modref.IsNamespace) then 
              errorR(Error(FSComp.SR.tcModuleAbbreviationForNamespace(fullDisplayTextOfModRef (List.head modrefs)),m));
          let modrefs = modrefs |> List.filter (fun mvv -> not mvv.IsNamespace)
          modrefs |> List.iter (fun modref -> CheckEntityAttributes cenv.g modref m |> CommitOperationResult);        
          let env = (if modrefs.Length > 0 then AddModuleAbbreviation scopem id modrefs env else env)
          return ((fun e -> e), []), env, env

      | SynModuleDecl.Exception (edef,m) -> 
          let binds,decl,env = TcExceptionDeclarations.TcExnDefn cenv env parent tpenv (edef,scopem)
          return ((fun e -> TMDefRec([decl], FlatList.ofList binds, [],m) :: e),[]), env, env

      | SynModuleDecl.Types (typeDefs,m) -> 
          let scopem = unionRanges m scopem
          let binds,tycons,env' = TcTypeDeclarations.TcTyconDefns cenv env parent tpenv (typeDefs,m,scopem)
          // Check the non-escaping condition as we build the expression on the way back up 
          let exprfWithEscapeCheck e = 
              let freeInEnv = GeneralizationHelpers.ComputeUnabstractableTycons env
              tycons |> List.iter(fun tycon -> 
                  let nm = tycon.DisplayName
                  if not tycon.IsTypeAbbrev && Zset.contains tycon freeInEnv then 
                     errorR(Error(FSComp.SR.tcTypeUsedInInvalidWay(nm, nm, nm), tycon.Range)));

              let freeInEnv = GeneralizationHelpers.ComputeUnabstractableTraitSolutions env
              binds |> List.iter(fun bind -> 
                  let nm = bind.Var.DisplayName
                  if Zset.contains bind.Var freeInEnv then errorR(Error(FSComp.SR.tcMemberUsedInInvalidWay(nm, nm, nm), bind.Var.Range)));

              TMDefRec(tycons,FlatList.ofList binds,[],m) :: e

          return (exprfWithEscapeCheck,[]),env', env'

      | SynModuleDecl.Open (mp,m) -> 
          let scopem = unionRanges m.EndRange scopem
          let env = TcOpenDecl cenv.g cenv.amap m scopem env mp
          return ((fun e -> e),[]), env, env

      | SynModuleDecl.Let (letrec, binds, m) -> 

          // ignore solitary '()' expressions and 'do ()' bindings, since these are allowed in namespaces
          // for the purposes of attaching attributes to an assembly, e.g. 
          //   namespace A.B.C
          //     [<assembly : Foo >]
          //     do()
          match binds with 
          | [ Binding (None,(StandaloneExpression | DoBinding),false,false,[],_,_,_,
                       None,(SynExpr.Do (SynExpr.Const (SynConst.Unit,_),_) | SynExpr.Const (SynConst.Unit,_)),
                       _,_) ] ->
              return (id,[]), env, env


          | _ -> 
              //do 
              //    for b in binds do
              //        printfn "----------\nb = %+A\n------------------\n" b
              //        match b with 
              //        | Binding (None,DoBinding,_,_,_,_,_,_,BindingRhs(_,_,e),_,_) ->
              //            printfn "----------\ne = %+A, #binds = %d\n------------------\n" e binds.Length
              //        | _ -> 
              //            ()
              let parentModule = 
                  match parent with 
                  | ParentNone -> 
                       error(NumberedError(FSComp.SR.tcNamespaceCannotContainValues(),binds.Head.RangeOfHeadPat)) 
                       
                  | Parent p -> p

              let containerInfo = ModuleOrNamespaceContainerInfo(parentModule)
              if letrec then 
                let scopem = unionRanges m scopem
                let binds = binds |> List.map (fun bind -> RecBindingDefn(containerInfo,NoNewSlots,ModuleOrMemberBinding,bind))
                let binds,env,_ = TcLetrec  WarnOnOverrides cenv env tpenv (binds,m, scopem)
                return ((fun e -> TMDefRec([],FlatList.ofList binds,[],m) :: e),[]), env, env
              else 
                let binds,env,_ = TcLetBindings cenv env containerInfo ModuleOrMemberBinding tpenv (binds,m,scopem)
                return ((fun e -> binds@e),[]), env, env 

      | SynModuleDecl.DoExpr (spExpr,expr, m) -> 

          let bind = 
              Binding (None,
                       StandaloneExpression,
                       false,false,[],PreXmlDoc.Empty,SynInfo.emptyValSynData,
                       SynPat.Wild m,
                       None,expr,m,spExpr)

          return! TcModuleOrNamespaceElement cenv parent scopem env (SynModuleDecl.Let(false,[bind],m))

      | SynModuleDecl.Attributes (attrs,_) -> 
          let attrs' = TcAttributesWithPossibleTargets cenv env AttributeTargets.Top attrs
          return ((fun e -> e), attrs'), env, env

      | SynModuleDecl.HashDirective _ -> 
          return ((fun e -> e), []), env, env

      | SynModuleDecl.NestedModule(ComponentInfo(attribs,_parms, _constraints,longPath,xml,_,vis,im),mdefs,isContinuingModule,m) ->
          let id = ComputeModuleName longPath

          let modAttrs = TcAttributes cenv env AttributeTargets.ModuleDecl attribs
          let modKind = ComputeModuleOrNamespaceKind cenv.g true modAttrs

          CheckForDuplicateConcreteType cenv env (AdjustModuleName modKind id.idText) im;
          CheckForDuplicateModule cenv env id.idText id.idRange;
          let vis,_ = ComputeAccessAndCompPath env None id.idRange vis parent
             
          let! (topAttrsNew, _,ModuleOrNamespaceBinding(mspecPriorToOuterOrExplicitSig,mexpr)),_,envAtEnd =
              TcModuleOrNamespace cenv env (id,true,mdefs,xml,modAttrs,vis,m)

          let mspec = mspecPriorToOuterOrExplicitSig
          let mdef = TMDefRec([],FlatList.empty,[ModuleOrNamespaceBinding(mspecPriorToOuterOrExplicitSig,mexpr)],m)
          PublishModuleDefn cenv env mspec; 
          let env = AddLocalSubModule cenv.g cenv.amap m scopem env mspec
          
          // isContinuingModule is true for all of the following
          //   - the implicit module of a script 
          //   - the major 'module' declaration for a file stating with 'module X.Y' 
          //   - an interactive entry for F# Interactive 
          // In this case the envAtEnd is the environment at the end of this module
          let envAtEnd = (if isContinuingModule then envAtEnd  else env)
          
          return ((fun e -> mdef :: e),topAttrsNew), env, envAtEnd
      

      | SynModuleDecl.NamespaceFragment(ModuleOrNamespace(lid,isModule,defs,xml,attribs,vis,m)) ->

          if !progress then dprintn ("Typecheck implementation "^textOfLid lid);
          let endm = m.EndRange

          do for id in lid do 
               CheckNamespaceModuleOrTypeName cenv id;

          let enclosingNamespacePath = if isModule then fst (List.frontAndBack lid) else lid
          let defs = 
              if isModule then 
                  [SynModuleDecl.NestedModule(ComponentInfo(attribs,[], [],[snd(List.frontAndBack lid)],xml,false,vis,m),defs,true,m)] 
              else 
                  defs
          let envinner = LocateEnv cenv.topCcu env enclosingNamespacePath
          let envinner = ImplicitlyOpenOwnNamespace cenv.g cenv.amap m enclosingNamespacePath envinner

          let! mexpr, topAttrs, _, envAtEnd = TcModuleOrNamespaceElements cenv parent endm envinner xml defs
          
          let env = 
              if isNil enclosingNamespacePath then 
                  envAtEnd
              else
                  let modulTypeRoot = BuildRootModuleType enclosingNamespacePath envinner.eCompPath !(envinner.eMtypeAcc)

                  let env = AddLocalRootModuleOrNamespace cenv.g cenv.amap m env modulTypeRoot
                  // Publish the combined module type
                  env.eMtypeAcc := combineModuleOrNamespaceTypeList [] m [!(env.eMtypeAcc); modulTypeRoot]
                  env
          
          let mexprRoot = BuildRootModuleExpr enclosingNamespacePath envinner.eCompPath mexpr

          return ((fun e -> mexprRoot :: e),topAttrs), env, envAtEnd

    with exn -> 
        errorRecovery exn e.Range; 
        return ((fun e -> e), []), env, env
 }
 
and TcModuleOrNamespaceElementsAux cenv parent endm (defsSoFar, env, envAtEnd) (moreDefs: SynModuleDecl list) =
 eventually {
    match moreDefs with 
    | (h1 :: t) ->
        // Lookahead one to find out the scope of the next declaration.
        let scopem = 
            if isNil t then unionRanges h1.Range endm
            else unionRanges (List.head t).Range endm

        // Possibly better:
        //let scopem = unionRanges h1.Range.EndRange endm
        
        let! h1',env', envAtEnd' = TcModuleOrNamespaceElement cenv parent scopem env h1
        // tail recursive 
        return! TcModuleOrNamespaceElementsAux  cenv parent endm ( (h1' :: defsSoFar), env', envAtEnd') t
    | [] -> 
        return List.rev defsSoFar,env, envAtEnd
 }

and TcModuleOrNamespaceElements cenv parent endm env xml defs =
  eventually {
    // Ensure the deref_nlpath call in UpdateAccModuleOrNamespaceType succeeds 
    if cenv.compilingCanonicalFslibModuleType then 
        ensureCcuHasModuleOrNamespaceAtPath cenv.topCcu env.ePath env.eCompPath (xml.ToXmlDoc());

    let! compiledDefs, env, envAtEnd = TcModuleOrNamespaceElementsAux cenv parent endm ([], env, env) defs
    // Apply the functions for each declaration to build the overall expression-builder 
    let mexpr = TMDefs(List.foldBack (fun (f,_) x -> f x) compiledDefs []) 

    // Collect up the attributes that are global to the file 
    let topAttrsNew = List.foldBack (fun (_,y) x -> y@x) compiledDefs []
    return (mexpr, topAttrsNew, env, envAtEnd)
  }  
    
and TcModuleOrNamespace cenv env (id,isModule,defs,xml,modAttrs,vis,m:range) =
  eventually {
    let endm = m.EndRange
    let modKind = ComputeModuleOrNamespaceKind cenv.g isModule modAttrs
    let id = ident (AdjustModuleName modKind id.idText, id.idRange)

    CheckNamespaceModuleOrTypeName cenv id;

    let envinner, mtypeAcc = MakeInnerEnv env id modKind
    
    // Create the new module specification to hold the accumulated results of the type of the module 
    // Also record this in the environment as the accumulator 
    let mspec = NewModuleOrNamespace (Some env.eCompPath) vis id (xml.ToXmlDoc()) modAttrs (notlazy (NewEmptyModuleOrNamespaceType modKind))

#if DEBUG
    if !verboseStamps then 
        dprintf "TcModuleOrNamespace: %s#%d\n" mspec.LogicalName mspec.Stamp;
#endif

    let innerParent = mkLocalModRef mspec

    // Now typecheck. 
    let! mexpr, topAttrs, env, envAtEnd = TcModuleOrNamespaceElements cenv (Parent innerParent) endm envinner xml defs 

    // Get the inferred type of the decls. It's precisely the one we created before checking 
    // and mutated as we went. Record it in the mspec. 
    mspec.Data.entity_modul_contents <- notlazy !mtypeAcc ; 

    return (topAttrs,mspec,ModuleOrNamespaceBinding(mspec,mexpr)), env, envAtEnd
 }


//--------------------------------------------------------------------------
// TypecheckOneImplFile - Typecheck all the namespace fragments in a file.
//-------------------------------------------------------------------------- 

let AddCcuToTcEnv(g,amap,scopem,env,ccu,autoOpens,internalsVisible) = 
    let env = AddNonLocalCcu g amap scopem env (ccu,internalsVisible)

#if AUTO_OPEN_ATTRIBUTES_AS_OPEN
    let env = List.fold (fun env p -> TcOpenDecl g amap scopem scopem env (pathToSynLid scopem (splitNamespace p))) env autoOpens
#else
    let env = (env,autoOpens) ||> List.fold (fun env p -> 
                  let warn() = 
                      warning(Error(FSComp.SR.tcAttributeAutoOpenWasIgnored(p, ccu.AssemblyName),scopem));
                      env
                  let p = splitNamespace p 
                  if isNil p then warn() else
                  let h,t = List.frontAndBack p 
                  let modref = mkNonLocalTyconRef (mkNonLocalEntityRef ccu (Array.ofList h))  t
                  match modref.TryDeref with 
                  | None ->  warn()
                  | Some _ -> OpenModuleOrNamespace g amap scopem env modref) 
#endif
    env

let CreateInitialTcEnv(g,amap,scopem,ccus) =
    List.fold (fun env (ccu,autoOpens,internalsVisible) -> AddCcuToTcEnv(g,amap,scopem,env,ccu,autoOpens,internalsVisible)) (empty_tenv g) ccus

type ConditionalDefines = 
    string list


/// The attributes that don't get attached to any declaration
type TopAttribs =
    { mainMethodAttrs: Attribs;
      netModuleAttrs: Attribs;
      assemblyAttrs : Attribs  }

let EmptyTopAttrs =
    { mainMethodAttrs=[];
      netModuleAttrs=[];
      assemblyAttrs =[]  }

let CombineTopAttrs topAttrs1 topAttrs2 =
    { mainMethodAttrs = topAttrs1.mainMethodAttrs @ topAttrs2.mainMethodAttrs;
      netModuleAttrs  = topAttrs1.netModuleAttrs @ topAttrs2.netModuleAttrs;
      assemblyAttrs   = topAttrs1.assemblyAttrs @ topAttrs2.assemblyAttrs } 

let rec IterTyconsOfModuleOrNamespaceType f (mty:ModuleOrNamespaceType) = 
    mty.AllEntities |> QueueList.iter (fun tycon -> f tycon);
    mty.ModuleAndNamespaceDefinitions |> List.iter (fun v -> 
        IterTyconsOfModuleOrNamespaceType f v.ModuleOrNamespaceType)


/// Check an entire implementation file
/// Typecheck, then close the inference scope and then check the file meets its signature (if any)
let TypecheckOneImplFile 
       // checkWeShouldContinue: A function to help us stop reporting cascading errors 
       (g,niceNameGen,amap,topCcu,checkWeShouldContinue,conditionalDefines,isInteractive) 
       env 
       (rootSigOpt : ModuleOrNamespaceType option)
       (ImplFile(_,isScript,qualNameOfFile,scopedPragmas,_,implFileFrags,isLastCompiland)) =

 eventually {
    let cenv = cenv.Create (g,isScript,niceNameGen,amap,topCcu,false,isSome(rootSigOpt),conditionalDefines,isInteractive)    

    let envinner, mtypeAcc = MakeInitialEnv env 

    let defs = [ for x in implFileFrags -> SynModuleDecl.NamespaceFragment(x) ]
    let! mexpr, topAttrs, env, envAtEnd = TcModuleOrNamespaceElements cenv ParentNone qualNameOfFile.Range envinner PreXmlDocEmpty defs

    let implFileTypePriorToSig = !mtypeAcc 

    let topAttrs = 
        let mainMethodAttrs,others = topAttrs |> List.partition (fun (possTargets,_) -> possTargets &&& AttributeTargets.Method <> enum 0) 
        let assemblyAttrs,others = others |> List.partition (fun (possTargets,_) -> possTargets &&& AttributeTargets.Assembly <> enum 0) 
        let netModuleAttrs, _others = others |> List.partition (fun (possTargets,_) -> possTargets &&& AttributeTargets.Module <> enum 0)
        { mainMethodAttrs = List.map snd mainMethodAttrs;
          netModuleAttrs  = List.map snd netModuleAttrs;
          assemblyAttrs   = List.map snd assemblyAttrs}
    let denvAtEnd = envAtEnd.DisplayEnv
    let m = qualNameOfFile.Range
    
    // This is a fake module spec
    let implFileSpecPriorToSig = wrapModuleOrNamespaceType qualNameOfFile.Id (compPathOfCcu topCcu) implFileTypePriorToSig

    let extraAttribs = topAttrs.mainMethodAttrs@topAttrs.netModuleAttrs@topAttrs.assemblyAttrs
    
    // Defaults get applied before the module signature is checked and before the implementation conditions on virtuals/overrides. 
    // Defaults get applied in priority order. Defaults listed last get priority 0 (lowest), 2nd last priority 1 etc. 
    let _ = 
        try
            let unsolved = Microsoft.FSharp.Compiler.FindUnsolved.UnsolvedTyparsOfModuleDef g cenv.amap denvAtEnd (mexpr,extraAttribs)

            GeneralizationHelpers.CanonicalizePartialInferenceProblem (cenv,denvAtEnd,m) unsolved;

            let applyDefaults priority =
                 unsolved |> List.iter (fun tp -> 
                    if not tp.IsSolved then 
                        // Apply the first default. If we're defaulting one type variable to another then 
                        // the defaults will be propagated to the new type variable. 
                        tp.Constraints |> List.iter (fun tpc -> 
                            match tpc with 
                            | TTyparDefaultsToType(priority2,ty2,m) when priority2 = priority -> 
                                let ty1 = mkTyparTy tp
                                if not tp.IsSolved  && not (typeEquiv cenv.g ty1 ty2) then
                                    if verbose then dprintf "assigning default '%s' for variable '%s' near %a at priority %d\n" ((DebugPrint.showType ty2)) ((DebugPrint.showType ty1)) outputRange m priority2;
                                    let csenv = (MakeConstraintSolverEnv cenv.css m denvAtEnd)
                                    TryD (fun () -> ConstraintSolver.SolveTyparEqualsTyp csenv 0 m NoTrace ty1 ty2)
                                         (fun e -> solveTypAsError cenv denvAtEnd m ty1;
                                                   ErrorD(ErrorFromApplyingDefault(g,denvAtEnd,tp,ty2,e,m)))
                                    |> RaiseOperationResult;
                            | _ -> ()))
                    
            for priority = 10 downto 0 do
                applyDefaults priority
            done;

            // OK, now apply defaults for any unsolved HeadTypeStaticReq 
            unsolved |> List.iter (fun tp ->     
                if not tp.IsSolved then 
                    if (tp.StaticReq <> NoStaticReq) then
                        ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css envAtEnd.DisplayEnv tp);
        with e -> errorRecovery e m

    // Check completion of all classes defined across this file. 
    // NOTE: this is not a great technique if inner signatures are permitted to hide 
    // virtual dispatch slots. 
    if (checkWeShouldContinue()) then  
        try implFileTypePriorToSig |> IterTyconsOfModuleOrNamespaceType (FinalTypeDefinitionChecksAtEndOfInferenceScope cenv.infoReader true denvAtEnd);
        with e -> errorRecovery e m 

    // Check the value restriction. Only checked if there is no signature.
    if (checkWeShouldContinue() && isNone rootSigOpt) then 

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
                  | tp :: _ -> errorR (ValueRestriction(envAtEnd.eNameResEnv.eDisplayEnv,false,v, tp,v.Range))
                  | _ -> ();
            mty.ModuleAndNamespaceDefinitions |> List.iter (fun v -> check v.ModuleOrNamespaceType) 
        try check implFileTypePriorToSig with e -> errorRecovery e m


    // Solve unsolved internal type variables 
    if (checkWeShouldContinue()) then  

        let unsolved = Microsoft.FSharp.Compiler.FindUnsolved.UnsolvedTyparsOfModuleDef g cenv.amap envAtEnd.DisplayEnv (mexpr,extraAttribs)

        unsolved |> List.iter (fun tp -> 
                if (tp.Rigidity <> TyparRigid) && not tp.IsSolved then 
                    ConstraintSolver.ChooseTyparSolutionAndSolve cenv.css envAtEnd.DisplayEnv tp);

    // Check the module matches the signature 
    let implFileExprAfterSig = 
        match rootSigOpt with 
        | None -> 
            // Deep copy the inferred type of the module 
            let implFileTypePriorToSigCopied = 
#if DEBUG
                if !verboseStamps then dprintf "Compilation unit type before copy:\n%s\n" (Layout.showL (Layout.squashTo 192 (entityTypeL implFileTypePriorToSig)));
#endif
                let res = copyModuleOrNamespaceType g CloneAll implFileTypePriorToSig
#if DEBUG
                if !verboseStamps then dprintf "Compilation unit type after copy:\n%s\n" (Layout.showL (Layout.squashTo 192 (entityTypeL res)));
#endif
                res

            ModuleOrNamespaceExprWithSig(implFileTypePriorToSigCopied,mexpr,m)
            
        | Some sigFileType -> 

            // We want to show imperative type variables in any types in error messages at this late point 
            let denv = { denvAtEnd with showImperativeTyparAnnotations=true; }
            begin 
                try 
                
                    // As typechecked the signature and implementation use different tycons etc. 
                    // Here we (a) check there are enough names, (b) match them up to build a renaming and   
                    // (c) check signature conformance up to this renaming. 
                    if not (SignatureConformance.CheckNamesOfModuleOrNamespace denv (mkLocalTyconRef implFileSpecPriorToSig) sigFileType) then 
                        raise (ReportedError None);

                    // Compute the remapping from implementation to signature
                    let remapInfo ,_ = ComputeRemappingFromInferredSignatureToExplicitSignature cenv.g implFileTypePriorToSig sigFileType
                     
                    let aenv = { TypeEquivEnv.Empty with EquivTycons = TyconRefMap.OfList remapInfo.mrpiEntities }
                    
                    if not (SignatureConformance.Checker(cenv.g, cenv.amap, denv, remapInfo, true).CheckSignature  aenv (mkLocalModRef implFileSpecPriorToSig) sigFileType) then  (
                        // We can just raise 'ReportedError' since CheckModuleOrNamespace raises its own error 
                        raise (ReportedError None);
                    )
                with e -> errorRecovery e m;
            end;
            
            ModuleOrNamespaceExprWithSig(sigFileType,mexpr,m)

    // We ALWAYS run the PostTypecheckSemanticChecks phase, though we if we have already encountered some
    // errors we turn off error reporting. THis is because it performs various fixups over the TAST, e.g. 
    // assigning nice names for inference variables.
    let hasExplicitEntryPoint = 
        try  
            let reportErrors = checkWeShouldContinue()
            Microsoft.FSharp.Compiler.PostTypecheckSemanticChecks.CheckTopImpl (g,cenv.amap,reportErrors,cenv.infoReader,env.eInternalsVisibleCompPaths,cenv.topCcu,envAtEnd.DisplayEnv, implFileExprAfterSig,extraAttribs,isLastCompiland,cenv.isInteractive);
        with e -> 
            errorRecovery e m
            false

    let implFile = TImplFile(qualNameOfFile,scopedPragmas, implFileExprAfterSig, hasExplicitEntryPoint,isScript)

    return (topAttrs,implFile,envAtEnd)
 } 
   


/// Check an entire sginature file
let TypecheckOneSigFile  
       (g,niceNameGen,amap,topCcu,checkWeShouldContinue,conditionalDefines,isInteractive) 
       tcEnv 
       (SigFile(_,qualNameOfFile,_, _,sigFileFrags)) = 
 eventually {     
    let cenv = cenv.Create (g,false,niceNameGen,amap,topCcu,true,false,conditionalDefines,isInteractive)
    let envinner,mtypeAcc = MakeInitialEnv tcEnv 

    let specs = [ for x in sigFileFrags -> SynModuleSigDecl.NamespaceFragment(x) ]
    let! tcEnv = TcSignatureElements cenv ParentNone qualNameOfFile.Range envinner PreXmlDocEmpty specs
    
    let sigFileType = !mtypeAcc 
    
    if (checkWeShouldContinue()) then  
        try sigFileType |> IterTyconsOfModuleOrNamespaceType (FinalTypeDefinitionChecksAtEndOfInferenceScope cenv.infoReader false tcEnv.DisplayEnv);
        with e -> errorRecovery e qualNameOfFile.Range

    return (tcEnv,tcEnv,sigFileType)
 }

