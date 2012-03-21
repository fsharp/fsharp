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


module internal Microsoft.FSharp.Compiler.Nameres

//-------------------------------------------------------------------------
// Name environment and name resolution 
//------------------------------------------------------------------------- 

open Internal.Utilities
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Import
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.IL // Abstract IL 
open Microsoft.FSharp.Compiler.Outcome
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Infos.AccessibilityLogic
open Microsoft.FSharp.Compiler.Infos.AttributeChecking
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.PrettyNaming
open System.Collections.Generic

type NameResolver(g:TcGlobals, 
                  amap: Import.ImportMap, 
                  infoReader: InfoReader, 
                  instantiationGenerator: (range -> Typars -> TypeInst)) =
    /// Used to transform typars into new inference typars 
    // instantiationGenerator is a function to help us create the
    // type parameters by copying them from type parameter specifications read
    // from IL code.  
    //
    // When looking up items in generic types we create a fresh instantiation 
    // of the type, i.e. instantiate the type with inference variables. 
    // This means the item is returned ready for use by the type inference engine 
    // without further freshening. However it does mean we end up plumbing 'instantiationGenerator' 
    // around a bit more than we would like to, which is a bit annoying. 
    member nr.instantiationGenerator = instantiationGenerator
    member nr.g = g
    member nr.amap = amap
    member nr.InfoReader = infoReader
    
//-------------------------------------------------------------------------
// Helpers for unionconstrs and recdfields
//------------------------------------------------------------------------- 

let UnionCaseRefsInTycon (modref: ModuleOrNamespaceRef) (tycon:Tycon) = 
    tycon.UnionCasesAsList |> List.map (mkModuleUnionCaseRef modref tycon)

let UnionCaseRefsInModuleOrNamespace (modref:ModuleOrNamespaceRef) = 
    [ for x in modref.ModuleOrNamespaceType.AllEntities do yield! UnionCaseRefsInTycon modref x ]

let TryFindTypeWithUnionCase (modref:ModuleOrNamespaceRef) (id: Ident) = 
    modref.ModuleOrNamespaceType.AllEntities
    |> QueueList.tryFind (fun tycon -> tycon.GetUnionCaseByName id.idText |> isSome) 

let TryFindTypeWithRecdField (modref:ModuleOrNamespaceRef) (id: Ident) = 
    modref.ModuleOrNamespaceType.AllEntities
    |> QueueList.tryFind (fun tycon -> tycon.GetFieldByName id.idText |> isSome)

let ActivePatternElemsOfValRef vref = 
    match TryGetActivePatternInfo vref with
    | Some (APInfo(_,nms) as apinfo) -> List.mapi (fun i _ -> APElemRef(apinfo,vref, i)) nms
    | None -> [] 


// mkNestedValRef may fail if the assembly load set is
// incomplete and the value is an extension member of a type that is not
// available. In some cases we can reasonably recover from this, e.g. by simply not adding 
// an entry to a table. Callsites have to cope with the error (None) condition
// sensibly, e.g. in a way that won't change the way things are compiled as the 
// assembly set is completed.
let tryMkValRefInModRef modref vspec =
    protectAssemblyExploration
        None
        (fun () -> Some (mkNestedValRef modref vspec))

let ActivePatternElemsOfVal modref vspec = 
    // If the assembly load set is incomplete then dont add anything to the table
    match tryMkValRefInModRef modref vspec with 
    | None -> []
    | Some vref -> ActivePatternElemsOfValRef vref


let ActivePatternElemsOfModuleOrNamespace (modref:ModuleOrNamespaceRef) : ActivePatternElemRef NameMap = 
    let mtyp = modref.ModuleOrNamespaceType
    cacheOptRef mtyp.ActivePatternElemRefLookupTable (fun () ->
       let aprefs = [ for x in mtyp.AllValsAndMembers do yield! ActivePatternElemsOfVal modref x ]
       (Map.empty,aprefs) ||> List.fold (fun acc apref -> NameMap.add apref.Name apref acc) )

//---------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

// Note: Active patterns are encoded like this:
//   let (|A|B|) x = if x < 0 then A else B    // A and B are reported as results using 'Item.ActivePatternResult' 
//   match () with | A | B -> ()               // A and B are reported using 'Item.ActivePatternCase'

[<NoEquality; NoComparison>]
type Item = 
  // These exist in the "eUnqualifiedItems" List.map in the type environment. 
  | Value of  ValRef
  | UnionCase of UnionCaseInfo
  | ActivePatternResult of ActivePatternInfo * TType * int  * range
  | ActivePatternCase of ActivePatternElemRef 
  | ExnCase of TyconRef 
  | RecdField of RecdFieldInfo

  // The following are never in the items table but are valid results of binding 
  // an identitifer in different circumstances. 
  | NewDef of Ident
  | ILField of ILFieldInfo
  | Event of EventInfo
  | Property of string * PropInfo list
  | MethodGroup of string * MethInfo list
  | CtorGroup of string * MethInfo list
  | FakeInterfaceCtor of TType
  | DelegateCtor of TType
  | Types of string * TType list
  | ModuleOrNamespaces of Tast.ModuleOrNamespaceRef list
  | ImplicitOp of Ident
  | ArgName of Ident 
  | PropName of Ident 
  | UnqualifiedType of TyconRef list

let MakeMethGroup (nm,minfos:MethInfo list) = 
    let minfos = minfos |> List.sortBy (fun minfo -> minfo.NumArgs |> List.sum)
    Item.MethodGroup (nm,minfos)

let MakeCtorGroup (nm,minfos:MethInfo list) = 
    let minfos = minfos |> List.sortBy (fun minfo -> minfo.NumArgs |> List.sum)
    Item.CtorGroup (nm,minfos)

  
//---------------------------------------------------------------------------
//
//------------------------------------------------------------------------- 

type ExtensionMember = 
   | FSExtMem of ValRef * ExtensionMethodPriority
   | ILExtMem of ILTypeRef * ILMethodDef * ExtensionMethodPriority
   static member Equality g e1 e2 = 
       match e1, e2 with 
       | FSExtMem (vref1,_), FSExtMem (vref2,_) -> valRefEq g vref1 vref2
       | ILExtMem (_,md1,_), ILExtMem (_,md2,_) -> md1 === md2
       | _ -> false
   member x.Priority = 
       match x with 
       | FSExtMem (_,pri) -> pri
       | ILExtMem (_,_,pri) -> pri
       
type FullyQualifiedFlag = 
    // Only resolve full paths.
    | FullyQualified 
    | OpenQualified 


[<NoEquality; NoComparison>]
type NameResolutionEnv =
    { /// Display environment information for output 
      eDisplayEnv: DisplayEnv;  

      /// Values and Data Tags available by unqualified name 
      eUnqualifiedItems: NameMap<Item>;

      /// Data Tags and Active Pattern Tags available by unqualified name 
      ePatItems: NameMap<Item>;

      /// Modules accessible via "." notation. Note this is a multi-map. 
      /// Adding a module abbreviation adds it a local entry to this List.map. 
      /// Likewise adding a ccu or opening a path adds entries to this List.map. 
      
      
      /// "The boolean flag is means the namespace or module entry shouldn't 'really' be in the 
      ///  map, and if it is everr used to resolve a name then we give a warning. 
      ///  This is used to give warnings on unqualified namespace accesses, e.g. 
      ///    open System 
      ///    open Collections                            <--- give a warning 
      ///    let v = new Collections.Generic.List<int>() <--- give a warning" 
      
      eModulesAndNamespaces:  NameMultiMap<Tast.ModuleOrNamespaceRef>; 
      
      /// Fully qualified modules and namespaces. 'open' does not change this. 
      eFullyQualifiedModulesAndNamespaces:  NameMultiMap<Tast.ModuleOrNamespaceRef>; 
      
      /// RecdField labels in scope.  RecdField labels are those where type are inferred 
      /// by label rather than by known type annotation. 
      /// Bools indicate if from a record, where no warning is given on indeterminate lookup 
      eFieldLabels: NameMultiMap<Tast.RecdFieldRef>; 

      /// Tycons indexed by the various names that may be used to access them, e.g. 
      ///     "List" --> multiple TyconRef's for the various tycons accessible by this name. 
      ///     "List`1" --> TyconRef 
      eTyconsByAccessNames: NameMultiMap<TyconRef>; 

      eFullyQualifiedTyconsByAccessNames: NameMultiMap<TyconRef>; 

      /// Tycons available by unqualified, demangled names (i.e. (List,1) --> TyconRef) 
      eTyconsByDemangledNameAndArity: Map<NameArityPair,TyconRef>; 

      /// Tycons available by unqualified, demangled names (i.e. (List,1) --> TyconRef) 
      eFullyQualifiedTyconsByDemangledNameAndArity: Map<NameArityPair,TyconRef>; 

      /// Extension members by type and name 
      eExtensionMembers: TyconRefMultiMap<ExtensionMember>; 

      /// Typars (always available by unqualified names). Further typars can be 
      /// in the tpenv, a structure folded through each top-level definition. 
      eTypars: NameMap<Typar>; 

    } 

    static member Empty(g) =
        { eDisplayEnv=DisplayEnv.Empty g;
          eModulesAndNamespaces=Map.empty;
          eFullyQualifiedModulesAndNamespaces = Map.empty;
          eFieldLabels=Map.empty;
          eUnqualifiedItems=Map.empty;
          ePatItems=Map.empty;
          eTyconsByAccessNames=Map.empty;
          eTyconsByDemangledNameAndArity=Map.empty;
          eFullyQualifiedTyconsByAccessNames=Map.empty;
          eFullyQualifiedTyconsByDemangledNameAndArity=Map.empty;
          eExtensionMembers=TyconRefMultiMap<_>.Empty;      
          eTypars=Map.empty; }

    member nenv.DisplayEnv = nenv.eDisplayEnv

    member nenv.UnqualifiedItems = nenv.eUnqualifiedItems

    member nenv.TyconsByDemangledNameAndArity fq = 
        match fq with 
        | FullyQualified -> nenv.eFullyQualifiedTyconsByDemangledNameAndArity
        | OpenQualified  -> nenv.eTyconsByDemangledNameAndArity

    member nenv.TyconsByAccessNames fq = 
        match fq with 
        | FullyQualified -> nenv.eFullyQualifiedTyconsByAccessNames
        | OpenQualified  -> nenv.eTyconsByAccessNames

    member nenv.ModulesAndNamespaces fq = 
        match fq with 
        | FullyQualified -> nenv.eFullyQualifiedModulesAndNamespaces 
        | OpenQualified -> nenv.eModulesAndNamespaces 

/// ++GLOBAL MUTABLE STATE
// Note: global state, incrementing sequence of integers during type checking
let mutable extensionMethodPriorityCount = 0UL
let nextExtensionMethodPriority() = 
    extensionMethodPriorityCount <- extensionMethodPriorityCount + 1UL; 
    extensionMethodPriorityCount


//-------------------------------------------------------------------------
// Item functions
//------------------------------------------------------------------------- 

let DisplayNameOfItem g d = 
    match d with
    | Item.Value v -> v.DisplayName
    | Item.ActivePatternCase apref -> apref.Name
    | Item.UnionCase(ucr) -> DecompileOpName ucr.UnionCase.DisplayName
    | Item.ExnCase(ecr) -> ecr.LogicalName
    | Item.RecdField(rfinfo) -> DecompileOpName rfinfo.RecdField.Name
    | Item.NewDef(id) -> id.idText
    | Item.ILField(finfo) -> finfo.FieldName
    | Item.Event(einfo) -> einfo.EventName
    | Item.Property(nm,_) -> nm
    | Item.MethodGroup(nm,_) -> nm
    | Item.CtorGroup(nm,_) -> DemangleGenericTypeName nm
    | Item.FakeInterfaceCtor typ 
    | Item.DelegateCtor typ -> DemangleGenericTypeName (tcrefOfAppTy g typ).LogicalName
    | Item.Types(nm,_) -> DemangleGenericTypeName nm
    | Item.ModuleOrNamespaces(modref :: _) ->  modref.DemangledModuleOrNamespaceName
    | Item.ArgName(id) -> id.idText
    | Item.PropName(id) -> id.idText
    | _ ->  ""


// Add a value to the relevant table
//
// Object model members are not added to the name resolution environment 
let AddValRefToItems (vref:ValRef) eUnqualifiedItems =
    match vref.MemberInfo with 
    | Some _ -> eUnqualifiedItems
    | None -> NameMap.add vref.LogicalName (Item.Value vref) eUnqualifiedItems

let AddValRefToExtensionMembers (vref:ValRef) pri (eExtensionMembers: TyconRefMultiMap<_>) =
    if vref.IsMember && vref.IsExtensionMember then
        eExtensionMembers.Add (vref.MemberApparentParent, FSExtMem (vref,pri)) 
    else
        eExtensionMembers


/// This entrypoint is used to add some extra items to the environment for Visual Studio, e.g. static members 
let AddFakeNamedValRefToNameEnv nm nenv vref =
    {nenv with eUnqualifiedItems= NameMap.add nm (Item.Value vref) nenv.eUnqualifiedItems }

/// This entrypoint is used to add some extra items to the environment for Visual Studio, e.g. record members
let AddFakeNameToNameEnv nm nenv item =
    {nenv with eUnqualifiedItems= NameMap.add nm item nenv.eUnqualifiedItems }

let AddValRefToNameEnvWithPriority pri nenv vref =
    {nenv with eUnqualifiedItems= AddValRefToItems vref nenv.eUnqualifiedItems;
               eExtensionMembers = AddValRefToExtensionMembers vref pri nenv.eExtensionMembers;
               ePatItems = 
                   (let ePatItems = 
                      (ActivePatternElemsOfValRef vref, nenv.ePatItems) ||> List.foldBack (fun apref tab -> 
                          NameMap.add apref.Name (Item.ActivePatternCase apref) tab)

                    // Add literal constants to the environment available for resolving items in patterns 
                    let ePatItems = 
                        match vref.LiteralValue with 
                        | None -> ePatItems 
                        | Some _ -> NameMap.add vref.LogicalName (Item.Value vref) ePatItems

                    ePatItems) }

let AddValRefToNameEnv nenv vref = 
    AddValRefToNameEnvWithPriority (nextExtensionMethodPriority()) nenv vref

let AddActivePatternResultTagsToNameEnv (apinfo: PrettyNaming.ActivePatternInfo) nenv ty m =
    let nms = apinfo.Names
    let apresl = nms |> List.mapi (fun j nm -> nm, j)
    { nenv with  eUnqualifiedItems= List.foldBack (fun (nm,j) acc -> Map.add nm (Item.ActivePatternResult(apinfo,ty,j, m)) acc) apresl nenv.eUnqualifiedItems; } 

let GeneralizeUnionCaseRef (ucref:UnionCaseRef) = 
    UnionCaseInfo (fst (generalizeTyconRef ucref.TyconRef), ucref)
    
let private AddTyconRefToNameEnv ownDefinition (g:TcGlobals) amap m  root nenv (tcref:TyconRef) = 
    let AddRecdField (rfref:RecdFieldRef) tab = NameMultiMap.add rfref.FieldName rfref tab
    let AddUnionCase tab (ucref:UnionCaseRef)  = Map.add ucref.CaseName (Item.UnionCase (GeneralizeUnionCaseRef ucref)) tab
    let AddUnionCases tab ucrefs = List.fold AddUnionCase tab ucrefs
    let isIL = tcref.IsILTycon
    let ucrefs = if isIL then [] else tcref.UnionCasesAsList |> List.map (mkNestedUnionCaseRef tcref) 
    let flds =  if isIL then [| |] else tcref.AllFieldsArray
    let pri = nextExtensionMethodPriority()

    let eExtensionMembers = 
        let csharpExtensionMeths =
            if isIL  then 
                let scoref,enc,tdef = tcref.ILTyconInfo
                if ILThingHasExtensionAttribute tdef.CustomAttrs then 
                    let tref = ILTypeInfo(tcref,mkRefForNestedILTypeDef scoref (enc,tdef),[],tdef)
                    
                    // found extension attribute on type 'tcref.LogicalName'
                    
                    tdef.Methods.AsList |> List.collect (fun md ->
                          if ILThingHasExtensionAttribute md.CustomAttrs then
                            match md.Parameters with 
                            | thisParam :: _ -> 
                                let ilty = thisParam.Type
                                match ilty with 
                                | ILType.Boxed tspec 
                                | ILType.Value tspec -> 
                                    let tcref = (tspec |> rescopeILTypeSpec scoref).TypeRef |> Import.ImportILTypeRef amap m
                                    // found extension method 'md.Name' on type 'tcref.LogicalName'
                                    
                                    [(tcref, tref, md)]
                                // Do not import extension members whose 'this' is only a type parameter
                                | _ ->
                                    []
                            | _ -> 
                                []
                          else
                              [])
                else
                    []
            else 
                []

        (nenv.eExtensionMembers,csharpExtensionMeths) ||> List.fold (fun tab (tcref,tref,md) -> 
            tab.Add (tcref, ILExtMem (tref.ILTypeRef, md, pri)))  
        
    
    { nenv with 
        eFieldLabels= 
            (if not tcref.IsRecordTycon || isIL || flds.Length = 0 || (not ownDefinition && HasAttrib g g.attrib_RequireQualifiedAccessAttribute tcref.Attribs) then 
                nenv.eFieldLabels 
             else (nenv.eFieldLabels,flds) ||> Array.fold (fun acc f -> 
                       if f.IsStatic || f.IsCompilerGenerated then acc 
                       else AddRecdField (mkNestedRecdFieldRef tcref f) acc)) ;
        eUnqualifiedItems    = 
            (let tab = nenv.eUnqualifiedItems
             // add the type name for potential use as a constructor
             // The rules are
             // -	The unqualified lookup table in the environment can contain map names to a set of type names (the set of type names is a new kind of "item"). 
             // -	When the contents of a type definition is added to the environment, an entry is added in this table for all class and struct types.
             // -	When opening a module, types are added first to the environment, then values, then auto-opened sub-modules. 
             // -	When a value is added by an "open" previously available type names will become inaccessible by this table. 
             let tab = 

                 // This may explore into an unreferenced assembly if the name
                 // is a type abbreviation. If it does, assume the name does not
                 // have a constructor.
                 let mayHaveConstruction = 
                     protectAssemblyExploration 
                         false 
                         (fun () -> 
                              let typ = generalizedTyconRef tcref
                              isClassTy g typ || isStructTy g typ)

                 if mayHaveConstruction then 
                     match nenv.eUnqualifiedItems.TryFind tcref.DisplayName with 
                     | Some (Item.UnqualifiedType tcrefs) ->  tab.Add(tcref.DisplayName, Item.UnqualifiedType (tcref::tcrefs))
                     | _ ->  tab.Add(tcref.DisplayName, Item.UnqualifiedType [tcref])
                 else
                     tab
             if isIL || ucrefs.Length = 0  || (not ownDefinition && HasAttrib g g.attrib_RequireQualifiedAccessAttribute tcref.Attribs) then 
                 tab 
             else 
                 AddUnionCases tab ucrefs);
        ePatItems = 
            (if isIL || ucrefs.Length = 0  || (not ownDefinition && HasAttrib g g.attrib_RequireQualifiedAccessAttribute tcref.Attribs) then 
                nenv.ePatItems 
             else 
                AddUnionCases nenv.ePatItems ucrefs);
        eExtensionMembers = 
            eExtensionMembers;
        eFullyQualifiedTyconsByDemangledNameAndArity= 
            (if root  then AddTyconsByDemangledNameAndArity tcref.LogicalName (tcref.Typars(m)) tcref nenv.eFullyQualifiedTyconsByDemangledNameAndArity else nenv.eFullyQualifiedTyconsByDemangledNameAndArity); 
        eFullyQualifiedTyconsByAccessNames= 
            (if root then AddTyconsByAccessNames tcref.LogicalName tcref nenv.eFullyQualifiedTyconsByAccessNames else nenv.eFullyQualifiedTyconsByAccessNames);
        eTyconsByDemangledNameAndArity= 
            AddTyconsByDemangledNameAndArity tcref.LogicalName (tcref.Typars(m)) tcref nenv.eTyconsByDemangledNameAndArity; 
        eTyconsByAccessNames= 
            AddTyconsByAccessNames tcref.LogicalName tcref nenv.eTyconsByAccessNames } 
    
let AddTyconRefsToNameEnv ownDefinition g amap m  root nenv tcrefs = 
    List.fold (AddTyconRefToNameEnv ownDefinition g amap m root) nenv tcrefs

let AddExceptionDeclsToNameEnv nenv (ecref:TyconRef) = 
    assert ecref.IsExceptionDecl
    let add tab = NameMap.add ecref.LogicalName (Item.ExnCase ecref) tab
    {nenv with 
       eUnqualifiedItems=add nenv.eUnqualifiedItems;
       ePatItems = add nenv.ePatItems }

let AddModuleAbbrevToNameEnv (id:Ident) nenv modrefs = 
    {nenv with
       eModulesAndNamespaces=
         let add old nw = nw @ old
         NameMap.layerAdditive add (Map.add id.idText modrefs Map.empty) nenv.eModulesAndNamespaces }


//-------------------------------------------------------------------------
// Open a structure or an IL namespace 
//------------------------------------------------------------------------- 

let nestedModuleRefs (modref: ModuleOrNamespaceRef) = 
  modref.ModuleOrNamespaceType.ModuleAndNamespaceDefinitions  
     |> List.map modref.MkNestedTyconRef

// Recursive because of "AutoOpen", i.e. adding a module reference may automatically open further modules

let rec AddModuleOrNamespaceRefsToNameEnv g amap m root ad nenv (modrefs: ModuleOrNamespaceRef list) =
    let modrefsMap = modrefs |> NameMap.ofKeyedList (fun modref -> modref.DemangledModuleOrNamespaceName)
    let addModrefs tab = 
         let add old nw = 
             if IsEntityAccessible ad nw then  
                 nw :: old
             else 
                 old
         NameMap.layerAdditive add modrefsMap tab
    let nenv = 
        {nenv with
           eModulesAndNamespaces= addModrefs  nenv.eModulesAndNamespaces;
           eFullyQualifiedModulesAndNamespaces =
             (if root  
              then addModrefs  nenv.eFullyQualifiedModulesAndNamespaces
              else nenv.eFullyQualifiedModulesAndNamespaces) } 
    let nenv = 
        (nenv,modrefs) ||> List.fold (fun nenv modref ->  
            if modref.IsModule && TryFindBoolAttrib g g.attrib_AutoOpenAttribute modref.Attribs = Some(true) then
                AddModuleOrNamespaceContentsToNameEnv g amap ad m nenv modref 
            else
                nenv)
    nenv

and AddModuleOrNamespaceContentsToNameEnv (g:TcGlobals) amap (ad:AccessorDomain) m nenv (modref:ModuleOrNamespaceRef) = 
    let pri = nextExtensionMethodPriority()
    let mty = modref.ModuleOrNamespaceType
    let tycons = mty.TypeAndExceptionDefinitions
    let exncs = mty.ExceptionDefinitions
    let nenv = { nenv with eDisplayEnv= nenv.eDisplayEnv.AddOpenModuleOrNamespace modref }
    let tcrefs = tycons |> List.map modref.MkNestedTyconRef |> List.filter (IsEntityAccessible ad) 
    let exrefs = exncs |> List.map modref.MkNestedTyconRef |> List.filter (IsEntityAccessible ad) 
    let nenv = (nenv,exrefs) ||> List.fold AddExceptionDeclsToNameEnv 
    let nenv = (nenv,tcrefs) ||> AddTyconRefsToNameEnv false g amap m false 
    let nenv = (nenv,mty.AllValsAndMembers) ||> Seq.fold (fun acc x -> 
                    // If the assembly load set is incomplete and the value is an extension member
                    // then tryMkValRefInModRef may fail. In this case dont add anything to 
                    // the table since we are extending a type that is not available.
                    match  tryMkValRefInModRef modref x with 
                    | None -> acc
                    | Some vref -> 
                        if IsValAccessible ad vref then 
                            AddValRefToNameEnvWithPriority pri acc vref
                        else acc)  
    let nenv = (nenv,nestedModuleRefs modref) ||> AddModuleOrNamespaceRefsToNameEnv g amap m false ad 
    nenv

let AddModrefToNameEnv g amap m root ad nenv (modref:EntityRef) =  
    AddModuleOrNamespaceRefsToNameEnv g amap m root ad nenv [modref] 

  
type CheckForDuplicateTyparFlag = 
    | CheckForDuplicateTypars 
    | NoCheckForDuplicateTypars

let AddDeclaredTyparsToNameEnv check nenv typars = 
    let typarmap = 
      List.foldBack 
        (fun (tp:Typar) sofar -> 
          begin match check with 
          | CheckForDuplicateTypars -> 
              if Map.containsKey tp.Name sofar then errorR (Duplicate("type parameter",tp.DisplayName,tp.Range))
          | NoCheckForDuplicateTypars -> 
              ()
          end;
          Map.add tp.Name tp sofar) typars Map.empty 
    {nenv with eTypars=NameMap.layer typarmap nenv.eTypars }


//-------------------------------------------------------------------------
// FreshenTycon and instantiationGenerator.  
//------------------------------------------------------------------------- 

let FreshenTycon (ncenv: NameResolver) m (tcref:TyconRef) = 
    let tinst = ncenv.instantiationGenerator m (tcref.Typars(m))
    TType_app(tcref,tinst)

let FreshenUnionCaseRef (ncenv: NameResolver) m (ucref:UnionCaseRef) = 
    let tinst = ncenv.instantiationGenerator m (ucref.TyconRef.Typars(m))
    UnionCaseInfo(tinst,ucref)

/// This must be called after fetching unqualified items that may need to be freshened
let FreshenUnqualifiedItem (ncenv: NameResolver) m res = 
    match res with 
    | Item.UnionCase (UnionCaseInfo(_,ucref)) -> Item.UnionCase (FreshenUnionCaseRef ncenv m ucref)
    | _ -> res


//-------------------------------------------------------------------------
// Resolve module paths, value, field etc. lookups.  Doing this involves
// searching through many possibilities and disambiguating.  Hence first
// define some ways of combining multiple results and for carrying
// error information.  Errors are generally undefined names and are
// reported by returning the error that occurs at greatest depth in the
// sequence of Identifiers. 
//------------------------------------------------------------------------- 

// Accumulate a set of possible results. 
// If neither operations succeed, return an approximate error. 
// If one succeeds, return that one. 
// Prefer the error associated with the first argument. 
let OneResult res = 
    match res with 
    | Result x -> Result [x]
    | Exception e -> Exception e

let OneSuccess x = Result [x]

let AddResults res1 res2 =
    match res1, res2 with 
    | Result [],_ -> res2
    | _,Result [] -> res1
    | Result x,Result l -> Result (x @ l)
    | Exception _,Result l -> Result l
    | Result x,Exception _ -> Result x
     // This prefers error messages coming from deeper failing long identifier paths 
    | Exception (UndefinedName(n1,_,_,_) as e1),Exception (UndefinedName(n2,_,_,_) as e2) -> 
        if n1 < n2 then Exception e2 else Exception e1
    // Prefer more concrete errors about things being undefined 
    | Exception (UndefinedName _ as e1),Exception (Error _) -> Exception e1
    | Exception (Error _),Exception (UndefinedName _ as e2) -> Exception e2
    | Exception e1,Exception _ -> Exception e1

let (+++) x y = AddResults x y
let NoResultsOrUsefulErrors = Result []

let rec CollectResults f = function
    | [] -> NoResultsOrUsefulErrors
    | [h] -> OneResult (f h)
    | h :: t -> AddResults (OneResult (f h)) (CollectResults f t)

let AtMostOneResult m res = 
    match res with 
    | Exception err -> raze err
    | Result [] -> raze (Error(FSComp.SR.nrInvalidModuleExprType(),m))
    | Result [res] -> success res
    | Result (res :: _) -> success res 

//-------------------------------------------------------------------------
// Resolve (possibly mangled) type names 
//------------------------------------------------------------------------- 
 
/// Qualified lookups where the number of generic arguments is known 
/// from context, e.g. Module.Type<args>.  In theory the full names suh as ``List`1`` can 
/// be used to qualify access if needed 
let LookupTypeNameInEntityHaveArity nm ntyargs (mty:ModuleOrNamespaceType) = 
    if IsMangledGenericName nm || ntyargs = 0 then 
        mty.TypesByMangledName.TryFind nm
    else
        mty.TypesByMangledName.TryFind (nm+"`"+string ntyargs)

/// Unqualified lookups where the number of generic arguments is known 
/// from context, e.g. List<arg>.  Rebindings due to 'open' may have rebound identifiers.
let LookupTypeNameInEnvHaveArity fq nm ntyargs (nenv:NameResolutionEnv) = 
    if IsMangledGenericName nm then 
      nenv.TyconsByDemangledNameAndArity(fq).TryFind(DecodeGenericTypeName nm) 
      +?? (fun () -> nenv.TyconsByAccessNames(fq).TryFind nm |> Option.map List.head)
    else 
      nenv.TyconsByDemangledNameAndArity(fq).TryFind(NameArityPair(nm,ntyargs)) 
      +?? (fun () -> nenv.TyconsByAccessNames(fq).TryFind nm |> Option.map List.head)

/// Unqualified lookups where the number of generic arguments is NOT known 
/// from context. This is used in five places: 
///     -  static member lookups, e.g. MyType.StaticMember(3) 
///     -                         e.g. MyModule.MyType.StaticMember(3) 
///     -  type-qualified field names, e.g. { RecordType.field = 3 } 
///     -  type-qualified constructor names, e.g. match x with UnionType.A -> 3 
///     -  identifiers to constructors for better error messages, e.g. 'String(3)' after 'open System' 
///     -  the special single-constructor rule in TcTyconCores
/// 
/// Because of the potential ambiguity multiple results can be returned. 
/// Explicit type annotations can be added where needed to specify the generic arity. 
///  
/// In theory the full names such as ``RecordType`1`` can 
/// also be used to qualify access if needed, though this is almost never needed.  

let LookupTypeNameNoArity nm byDemangledNameAndArity byAccessNames = 
    if IsMangledGenericName nm then 
      match Map.tryFind (DecodeGenericTypeName nm) byDemangledNameAndArity with 
      | Some res -> [res]
      | None -> 
          match Map.tryFind nm byAccessNames with
          | Some res -> res
          | None -> []
    else 
      NameMultiMap.find nm byAccessNames

let LookupTypeNameInEnvNoArity fq nm (nenv: NameResolutionEnv) = 
    LookupTypeNameNoArity nm (nenv.TyconsByDemangledNameAndArity(fq)) (nenv.TyconsByAccessNames(fq))

let LookupTypeNameInEntityNoArity m nm (mtyp:ModuleOrNamespaceType) = 
    LookupTypeNameNoArity nm (mtyp.TypesByDemangledNameAndArity(m)) mtyp.TypesByAccessNames 

type TypeNameInExprOrPatFlag = 
     | ResolveTypeNamesToCtors 
     | ResolveTypeNamesToTypeRefs

type TypeNameResInfo = TypeNameInExprOrPatFlag * int option

let DefaultTypeNameResInfo : TypeNameResInfo = (ResolveTypeNamesToCtors,None) 

let LookupTypeNameInEnvMaybeHaveArity fq nm ((_,numTyargsOpt):TypeNameResInfo)  nenv = 
    match numTyargsOpt with 
    | None -> LookupTypeNameInEnvNoArity fq nm nenv
    | Some ntyargs -> LookupTypeNameInEnvHaveArity fq nm ntyargs nenv |> Option.toList

[<RequireQualifiedAccess>]
type GenerateEstTypeFlag = 
    | Yes of Import.AssemblyLoader //Used to find and/or inject assemblies as needed 
    | No
    
let LookupTypeNameInEntityMaybeHaveArity ad m nm numTyargsOpt estInfo (modref: ModuleOrNamespaceRef) = 
    let mtyp = modref.ModuleOrNamespaceType    
    let tycons = 
        match numTyargsOpt with 
        | None -> 
            match LookupTypeNameInEntityNoArity m nm mtyp with
            | [] -> 
                estInfo |> ignore
                []
            | tycons -> tycons |> List.map modref.MkNestedTyconRef 
        | Some ntyargs -> 
            match LookupTypeNameInEntityHaveArity nm ntyargs mtyp with
            | Some(tycon) -> [modref.MkNestedTyconRef tycon] 
            | None -> 
                []

    tycons 
       |> List.filter (IsEntityAccessible ad)


let GetNestedTypesOfType ad (ncenv:NameResolver) (optFilter,numTyargsOpt) m typ =
    let g = ncenv.g
    ncenv.InfoReader.ReadPrimaryTypeHierachy(FirstIntfInst,m,typ) |> List.collect (fun typ -> 
        if isAppTy g typ then 
            let tcref,tinst = destAppTy g typ
            let tycon = tcref.Deref
            let mty = tycon.ModuleOrNamespaceType
            // Handle the .NET/C# business where nested generic types implictly accumulate the type parameters 
            // from their enclosing types.
            let MakeNestedType (tcrefNested:TyconRef) = 
                let _,tps = List.chop tinst.Length (tcrefNested.Typars(m))
                let tinstNested = ncenv.instantiationGenerator m tps
                mkAppTy tcrefNested (tinst @ tinstNested)

            match optFilter with 
            | Some nm -> 
                LookupTypeNameInEntityMaybeHaveArity ad m nm numTyargsOpt GenerateEstTypeFlag.No tcref 
                    |> List.map MakeNestedType 
            | None -> 
                    mty.TypesByAccessNames 
                        |> NameMultiMap.range 
                        |> List.map (tcref.MkNestedTyconRef >> MakeNestedType)
                        |> List.filter (IsTypeAccessible g ad)
        else [])

//-------------------------------------------------------------------------
// Report environments to visual studio. We stuff intermediary results 
// into a global variable. A little unpleasant. 
//------------------------------------------------------------------------- 

// Represents a type of the occurence when reporting name in name resolution
type ItemOccurence = 
  // This is a binding / declaration of the item
  | Binding = 0
  // This is a usage of the item
  | Use = 1
  // Inside pattern matching
  | Pattern = 2
  
type ITypecheckResultsSink =
    abstract NotifyEnvWithScope : range * NameResolutionEnv * AccessorDomain -> unit
    abstract NotifyExprHasType : pos * TType * Tastops.DisplayEnv * NameResolutionEnv * AccessorDomain * range -> unit
    abstract NotifyNameResolution : pos * Item * ItemOccurence * Tastops.DisplayEnv * NameResolutionEnv * AccessorDomain * range -> unit

// ++GLOBAL MUTABLE STATE
let GlobalTypecheckResultsSink : ITypecheckResultsSink option ref  = ref None

let CallEnvSink(scopem,nenv,ad) = 
    match !GlobalTypecheckResultsSink with 
    | None -> () 
    | Some sink -> sink.NotifyEnvWithScope(scopem,nenv,ad)

let CallNameResolutionSink(m:range,nenv,item,occurenceType,denv,ad) = 
    match !GlobalTypecheckResultsSink with 
    | None -> () 
    | Some sink -> sink.NotifyNameResolution(m.End,item,occurenceType,denv,nenv,ad,m)  

let CallExprHasTypeSink(m:range,nenv,typ,denv,ad) = 
    match !GlobalTypecheckResultsSink with 
    | None -> () 
    | Some sink -> sink.NotifyExprHasType(m.End,typ,denv,nenv,ad,m)

/// Checks if the type variables associated with the result of a resolution are inferrable,
/// i.e. occur in the arguments or return type of the resolution. If not give a warning
/// about a type instantiation being needed.
type ResultTyparChecker = unit -> bool

let CheckAllTyparsInferrable amap m item = 
    match item with
    | Item.Value _ -> true
    | Item.ActivePatternCase _ -> true
    | Item.UnionCase _ -> true
    | Item.ExnCase _ -> true
    | Item.RecdField _ -> true
    | Item.NewDef _ -> true
    
    | Item.ILField _ -> true
    | Item.Event _ -> true

    | Item.Property(_,pinfos) -> 
        pinfos |> List.forall (fun pinfo -> 
            let freeInEnclosingType = freeInType CollectTyparsNoCaching pinfo.EnclosingType
            let freeInArgsAndRetType = 
                accFreeInTypes CollectTyparsNoCaching (pinfo.ParamTypes(amap,m)) 
                       (freeInType CollectTyparsNoCaching (pinfo.PropertyType(amap,m)))
            let free = Zset.diff freeInEnclosingType.FreeTypars  freeInArgsAndRetType.FreeTypars
            free.IsEmpty)

    | Item.MethodGroup(_,minfos) -> 
        minfos |> List.forall (fun minfo -> 
            let fminst = minfo.FormalMethodInst
            let freeInEnclosingType = freeInType CollectTyparsNoCaching minfo.EnclosingType
            let freeInArgsAndRetType = 
                List.foldBack (accFreeInTypes CollectTyparsNoCaching) (ParamTypesOfMethInfo amap m minfo fminst) 
                   (accFreeInTypes CollectTyparsNoCaching (ObjTypesOfMethInfo amap m minfo fminst) 
                       (freeInType CollectTyparsNoCaching (FSharpReturnTyOfMeth amap m minfo fminst)))
            let free = Zset.diff freeInEnclosingType.FreeTypars  freeInArgsAndRetType.FreeTypars
            free.IsEmpty)

    | Item.CtorGroup _ -> true
    | Item.FakeInterfaceCtor _ 
    | Item.DelegateCtor _ -> true
    | Item.Types _ -> true
    | Item.ModuleOrNamespaces(_ :: _) -> true
    | Item.ArgName _ -> true
    | Item.PropName _ -> true
    | _ ->  true
    
/// Keeps track of information relevant to the chosen resolution of a long identifier
///
/// When we resolve an item such as System.Console.In we
/// resolve it in one step to a property/val/method etc. item. However
/// Visual Studio needs to know about the exact resolutions of the names
/// System and Console, i.e. the 'entity path' of the resolution. 
///
/// Each of the resolution routines keeps track of the entity path and 
/// ultimately calls ResolutionInfo.SendToSink to record it for 
/// later use by Visual Studio.
type ResolutionInfo = 
    | ResolutionInfo of (*entityPath, reversed*)(range * EntityRef) list * (*warnings/errors*)(ResultTyparChecker -> unit)

    static member SendToSink(ncenv: NameResolver,nenv,ad,ResolutionInfo(entityPath,warnings),typarChecker) = 
        entityPath |> List.iter (fun (m,eref:EntityRef) -> 
            CheckEntityAttributes ncenv.g eref m |> CommitOperationResult;        
            CheckTyconAccessible m ad eref |> ignore;
            let item = if eref.IsModuleOrNamespace then Item.ModuleOrNamespaces([eref]) else Item.Types(eref.DisplayName,[FreshenTycon ncenv m eref])
            CallNameResolutionSink(m,nenv,item,ItemOccurence.Use,nenv.eDisplayEnv,ad))
        warnings(typarChecker)
 
    static member Empty = 
        ResolutionInfo([],(fun _ -> ()))

    member x.AddEntity info = 
        let (ResolutionInfo(entityPath,warnings)) = x
        ResolutionInfo(info::entityPath,warnings)

    member x.AddWarning f = 
        let (ResolutionInfo(entityPath,warnings)) = x
        ResolutionInfo(entityPath,(fun typarChecker -> f typarChecker; warnings typarChecker))



let CheckForMultipleGenericTypeAmbiguities (tcrefs:(ResolutionInfo * TyconRef) list) ((typeNameResFlag,numTyargsOpt):TypeNameResInfo) m = 
    // Given ambiguous C<>, C<_>    we resolve the ambiguous 'C.M' to C<> without warning
    // Given ambiguous C<_>, C<_,_> we resolve the ambiguous 'C.M' to C<_> with an ambiguity error
    // Given C<_>                   we resolve the ambiguous 'C.M' to C<_> with a warning if the argument or return types can't be inferred

    // Given ambiguous C<>, C<_>    we resolve the ambiguous 'C()' to C<> without warning
    // Given ambiguous C<_>, C<_,_> we resolve the ambiguous 'C()' to C<_> with an ambiguity error
    // Given C<_>                   we resolve the ambiguous 'C()' to C<_> with a warning if the argument or return types can't be inferred

    let tcrefs = 
        tcrefs 
        // remove later duplicates (if we've opened the same module more than once)
        |> Seq.distinctBy (fun (_,tcref) -> tcref.Stamp) 
        |> Seq.toList                     
        // List.sortBy is a STABLE sort (the order matters!)
        |> List.sortBy (fun (_,tcref) -> tcref.Typars(m).Length)

    match tcrefs with 
    | ((_resInfo,tcref) :: _) when 
            // multiple types
            tcrefs.Length > 1 && 
            // no explicit type instantiation
            isNone numTyargsOpt && 
            // some type arguments required on all types (note sorted by typar count above)
            tcref.Typars(m).Length > 0 && 
            // plausible types have different arities
            (tcrefs |> Seq.distinctBy (fun (_,tcref) -> tcref.Typars(m).Length) |> Seq.length > 1)  ->
        [ for (resInfo,tcref) in tcrefs do 
            let resInfo = resInfo.AddWarning (fun _typarChecker -> errorR(Error(FSComp.SR.nrTypeInstantiationNeededToDisambiguateTypesWithSameName(tcref.DisplayName, tcref.DisplayNameWithUnderscoreTypars),m)))
            yield (resInfo,tcref) ]

    | [(resInfo,tcref)] when isNone numTyargsOpt && tcref.Typars(m).Length > 0 && typeNameResFlag = ResolveTypeNamesToTypeRefs ->
        let resInfo = 
            resInfo.AddWarning (fun typarChecker -> 
                if not (typarChecker()) then 
                    warning(Error(FSComp.SR.nrTypeInstantiationIsMissingAndCouldNotBeInferred(tcref.DisplayName, tcref.DisplayNameWithUnderscoreTypars),m)))
        [(resInfo,tcref)]

    | _ -> 
        tcrefs
    


//-------------------------------------------------------------------------
// Consume ids that refer to a namespace
//------------------------------------------------------------------------- 

let rec ResolveLongIndentAsModuleOrNamespace fullyQualified (nenv:NameResolutionEnv) ad (lid:Ident list) =
    match lid with 
    | [] -> NoResultsOrUsefulErrors

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         

    | id :: lid when id.idText = MangledGlobalName -> 
        ResolveLongIndentAsModuleOrNamespace FullyQualified nenv ad lid

    | id:: rest -> 
        match nenv.ModulesAndNamespaces(fullyQualified).TryFind(id.idText) with
        | Some modrefs -> 
            
            /// Look through the sub-namespaces and/or modules
            let rec look depth  (modref: ModuleOrNamespaceRef) (mty:ModuleOrNamespaceType) (lid:Ident list) =
                match lid with 
                | [] -> success (depth,modref,mty)
                | id:: rest ->
                    match mty.ModulesAndNamespacesByDemangledName.TryFind id.idText with
                    | Some mspec when IsEntityAccessible ad (modref.MkNestedTyconRef mspec) -> 
                        let subref = modref.MkNestedTyconRef mspec
                        look (depth+1) subref mspec.ModuleOrNamespaceType rest
                    | _ -> raze (UndefinedName(depth,FSComp.SR.undefinedNameNamespace,id,[]))

            modrefs |> CollectResults (fun modref -> 
                if IsEntityAccessible ad modref then 
                    look 1 modref modref.ModuleOrNamespaceType rest
                else 
                    raze (UndefinedName(0,FSComp.SR.undefinedNameNamespaceOrModule,id,[]))) 
        | None -> 
            raze (UndefinedName(0,FSComp.SR.undefinedNameNamespaceOrModule,id,[]))


let ResolveLongIndentAsModuleOrNamespaceThen fullyQualified (nenv:NameResolutionEnv) ad lid f =
    match lid with 
    | [] -> NoResultsOrUsefulErrors
    | id :: rest -> 
        match ResolveLongIndentAsModuleOrNamespace fullyQualified nenv ad [id] with
        |  Result modrefs -> 
              modrefs |> CollectResults (fun (depth,modref,mty) ->  
                  let resInfo = ResolutionInfo.Empty.AddEntity(id.idRange,modref) 
                  f resInfo (depth+1) id.idRange modref mty rest) 
        |  Exception err -> Exception err 

//-------------------------------------------------------------------------
// Bind name used in "new Foo.Bar(...)" constructs
//------------------------------------------------------------------------- 

let private ResolveObjectConstructorPrim (ncenv:NameResolver) edenv resInfo m ad typ = 
    let g = ncenv.g
    let amap = ncenv.amap
    if isDelegateTy g typ then 
        success (resInfo,Item.DelegateCtor typ,[])
    else 
        let cinfos =  GetIntrinsicConstructorInfosOfType ncenv.InfoReader m typ
        if isInterfaceTy g typ && isNil cinfos then 
            success (resInfo,Item.FakeInterfaceCtor typ, [])
        else 
            let defaultStructCtorInfo = 
                if (isStructTy g typ && not(cinfos |> List.exists (fun x -> x.IsNullary))) then 
                    [DefaultStructCtor(g,typ)] 
                else []
            if (isNil defaultStructCtorInfo && isNil cinfos) || not (isAppTy g typ) then 
                raze (Error(FSComp.SR.nrNoConstructorsAvailableForType(NicePrint.prettyStringOfTy edenv typ),m))
            else 
                let cinfos = cinfos |> List.filter (IsMethInfoAccessible amap m ad)  
                success (resInfo,MakeCtorGroup ((tcrefOfAppTy g typ).LogicalName, (defaultStructCtorInfo@cinfos)),[]) 

let ResolveObjectConstructor (ncenv:NameResolver) edenv m ad typ = 
    ResolveObjectConstructorPrim (ncenv:NameResolver) edenv [] m ad typ  |?> (fun (_resInfo,item,rest) -> (item,rest))

//-------------------------------------------------------------------------
// Bind IL "." notation (member lookup or lookup in a type)
//------------------------------------------------------------------------- 

let IntrinsicPropInfosOfTypeInScope (infoReader:InfoReader) (optFilter, ad) findFlag m typ =
    let g = infoReader.g
    let amap = infoReader.amap
    let pinfos = GetIntrinsicPropInfoSetsOfType infoReader (optFilter, ad, FirstIntfInst) findFlag m typ
    let pinfos = pinfos |> ExcludeHiddenOfPropInfos g amap m 
    pinfos

let ExtensionPropInfosOfTypeInScope (infoReader:InfoReader) (eExtensionMembers: TyconRefMultiMap<_>) (optFilter, ad) _findFlag m typ =
    let g = infoReader.g
    let amap = infoReader.amap
    infoReader.ReadEntireTypeHierachy(FirstIntfInst,m,typ) |> List.collect (fun typ -> 
         if (isAppTy g typ) then 
            let tcref = tcrefOfAppTy g typ
            // NOTE: multiple "open"'s push multiple duplicate values into eExtensionMembers 
            let extValRefs = 
                tcref 
                |> eExtensionMembers.Find 
                |> ListSet.setify (ExtensionMember.Equality g) 
            let propCollector = new PropertyCollector(g,amap,m,typ,optFilter,ad)
            extValRefs |> List.iter (fun emem ->
                match emem with 
                | FSExtMem (vref,_pri) -> 
                   match vref.MemberInfo with 
                   | None -> ()
                   | Some(membInfo) -> propCollector.Collect(membInfo,vref)
                | ILExtMem _ -> 
                   // No extension properties coming from .NET
                   ())
            propCollector.Close()
         else [])

let AllPropInfosOfTypeInScope infoReader eExtensionMembers (optFilter, ad) findFlag m typ =
    IntrinsicPropInfosOfTypeInScope infoReader (optFilter, ad) findFlag m typ
    @ ExtensionPropInfosOfTypeInScope infoReader eExtensionMembers (optFilter, ad) findFlag m typ 

let IntrinsicMethInfosOfType (infoReader:InfoReader) (optFilter,ad,allowMultiIntfInst) findFlag m typ =
    let g = infoReader.g
    let amap = infoReader.amap
    let minfos = GetIntrinsicMethInfoSetsOfType infoReader (optFilter,ad,allowMultiIntfInst) findFlag m typ
    let minfos = minfos |> ExcludeHiddenOfMethInfos g amap m
    minfos

let ImmediateExtensionMethInfosOfTypeInScope (infoReader:InfoReader) (eExtensionMembers: TyconRefMultiMap<_>) (optFilter,_ad) _findFlag m typ =
    let g = infoReader.g
    if (isAppTy g typ) then 
        let tcref = tcrefOfAppTy g typ
        // NOTE: multiple "open"'s push multiple duplicate values into eExtensionMembers 
        let extValRefs = 
            tcref 
            |> eExtensionMembers.Find
            |> ListSet.setify (ExtensionMember.Equality g) 
            // Mark the extension members up with a number indicating priority (in open order, 0 = most recent)
        extValRefs |> List.choose (fun emem -> 
                match emem with 
                | FSExtMem (vref,pri) -> 
                    match vref.MemberInfo with 
                    | None -> None
                    | Some(membInfo) -> TrySelectMemberVal g optFilter typ (Some pri) membInfo vref
                | ILExtMem (actualParent,md,pri) when (match optFilter with None -> true | Some(nm) -> nm = md.Name) ->
                    if isILAppTy g typ then
                        // 'typ' is the logical parent 
                        let tinfo = ILTypeInfo.FromType g typ
                        Some(mkILMethInfo infoReader.amap m tinfo (Some(actualParent)) (Some pri) md)
                    else
                        error(Error(FSComp.SR.nrNoNonFSharpExtensionMembersOnFSharpTypes(),m))                                        
                | _ -> 
                    None) 
    else []

let ExtensionMethInfosOfTypeInScope (infoReader:InfoReader) eExtensionMembers (optFilter,ad) findFlag m typ =
    infoReader.ReadEntireTypeHierachy(FirstIntfInst,m,typ) |> List.collect (fun typ -> 
        ImmediateExtensionMethInfosOfTypeInScope infoReader eExtensionMembers (optFilter,ad) findFlag m typ) 

let AllMethInfosOfTypeInScope infoReader eExtensionMembers (optFilter,ad) findFlag m typ =
    IntrinsicMethInfosOfType infoReader (optFilter,ad,FirstIntfInst) findFlag m typ 
    @ ExtensionMethInfosOfTypeInScope infoReader eExtensionMembers (optFilter,ad) findFlag m typ          


exception IndeterminateType of range

type LookupKind = 
   | RecdField
   | Pattern
   | Expr
   | Type
   | Ctor


let TryFindUnionCaseOfType g typ nm =
    if isAppTy g typ then 
        let tcref,tinst = destAppTy g typ
        match tcref.GetUnionCaseByName nm with 
        | None -> None
        | Some ucase -> Some(UnionCaseInfo(tinst,mkNestedUnionCaseRef tcref ucase))
    else 
        None

let CoreDisplayName(pinfo:PropInfo) =   
    match pinfo with
    | FSProp(_,_,_,Some(set)) -> set.CoreDisplayName
    | FSProp(_,_,Some(get),_) -> get.CoreDisplayName
    | FSProp _ -> failwith "unexpected (property must have either getter or setter)"
    | ILProp(_,ILPropInfo(_,def))  -> def.Name

let DecodeFSharpEvent (pinfos:PropInfo list) ad g (ncenv:NameResolver) m =
    match pinfos with 
    | [pinfo] when pinfo.IsFSharpEventProperty -> 
        let nm = CoreDisplayName(pinfo)
        let minfos1 = GetImmediateIntrinsicMethInfosOfType (Some("add_"+nm),ad) g ncenv.amap m pinfo.EnclosingType 
        let minfos2 = GetImmediateIntrinsicMethInfosOfType (Some("remove_"+nm),ad) g ncenv.amap m pinfo.EnclosingType
        match  minfos1,minfos2 with 
        | [FSMeth(_,_,addValRef,_)],[FSMeth(_,_,removeValRef,_)] -> 
            // FOUND PROPERTY-AS-EVENT AND CORRESPONDING ADD/REMOVE METHODS
            Some(Item.Event(FSEvent(g,pinfo,addValRef,removeValRef)))
        | _ -> 
            // FOUND PROPERTY-AS-EVENT BUT DIDN'T FIND CORRESPONDING ADD/REMOVE METHODS
            Some(Item.Property (nm,pinfos))
    | pinfo::_ when nonNil pinfos -> 
        let nm = CoreDisplayName(pinfo)
        Some(Item.Property (nm,pinfos))
    | _ -> 
        None

// This shows up on performance logs. Consider for example endles resolutions  of "List.map" to 
// the empty set of results, or "x.Length" for a list or array type. This indicates it could be worth adding a cache here.
let rec ResolveLongIdentInTypePrim (ncenv:NameResolver) nenv lookupKind (resInfo:ResolutionInfo) depth m ad (lid:Ident list) findFlag typeNameResInfo typ =
    let g = ncenv.g
    match lid with 
    | [] -> error(InternalError("ResolveLongIdentInTypePrim",m))
    | id :: rest -> 
        let nm = id.idText // used to filter the searches of the tables 
        let optFilter = Some(nm) // used to filter the searches of the tables 
        let contentsSearchAccessible = 
           let unionCaseSearch = 
               if (match lookupKind with Expr | Pattern -> true | _ -> false) then 
                   TryFindUnionCaseOfType g typ nm 
               else 
                   None
           // Lookup: datatype constructors take precedence 
           match unionCaseSearch with 
           | Some ucase -> 
               success(resInfo,Item.UnionCase(ucase),rest)
           | None -> 
                match TryFindIntrinsicNamedItemOfType ncenv.InfoReader (nm,ad) findFlag m typ with
                | Some (PropertyItem psets) when (match lookupKind with Expr  -> true | _ -> false) -> 
                    let pinfos = psets |> ExcludeHiddenOfPropInfos g ncenv.amap m
                    match DecodeFSharpEvent pinfos ad g ncenv m with
                    | Some(x) ->  success (resInfo, x, rest)
                    | None->  raze (UndefinedName (depth,FSComp.SR.undefinedNameFieldConstructorOrMember, id,[]))
                | Some(MethodItem msets) when (match lookupKind with Expr  -> true | _ -> false) -> 
                    let minfos = msets |> ExcludeHiddenOfMethInfos g ncenv.amap m
                    // fold the available extension members into the overload resolution
                    let extensionMethInfos = 
                       match lookupKind with 
                       | Expr -> ExtensionMethInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (optFilter,ad) findFlag m typ
                       | _ -> []
                    //if nonNil(extensionMethInfos) && (match lookupKind with Expr -> true | _ -> false) then 
                    success (resInfo,MakeMethGroup (nm,minfos@extensionMethInfos),rest) 
                
                    //success (resInfo,MakeMethGroup (nm,minfos),rest)

                | Some (ILFieldItem (finfo:: _))  when (match lookupKind with Expr | Pattern -> true | _ -> false) -> 
                    success (resInfo,Item.ILField finfo,rest)

                | Some (EventItem (einfo :: _)) when (match lookupKind with Expr -> true | _ -> false)  -> 
                    success (resInfo,Item.Event einfo,rest)
                | Some (RecdFieldItem (rfinfo)) when (match lookupKind with Expr | RecdField | Pattern -> true | _ -> false) -> 
                    success(resInfo,Item.RecdField(rfinfo),rest)
                | _ ->
                let pinfos = ExtensionPropInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (optFilter, ad) findFlag m typ
                if nonNil pinfos && (match lookupKind with Expr -> true | _ -> false)  then 
                    success (resInfo,Item.Property (nm,pinfos),rest) else
                
                let minfos = ExtensionMethInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (optFilter,ad) findFlag m typ
                if nonNil minfos && (match lookupKind with Expr -> true | _ -> false) then 
                    success (resInfo,MakeMethGroup (nm,minfos),rest) else 
                                
                if isTyparTy g typ then raze (IndeterminateType(unionRanges m id.idRange))
                else raze (UndefinedName (depth,FSComp.SR.undefinedNameFieldConstructorOrMember, id,[]))
              
        let nestedSearchAccessible = 
            let nestedTypes = GetNestedTypesOfType ad ncenv (Some nm,(if isNil rest then snd typeNameResInfo else None)) m typ
            let typeNameResFlag,_numTyargsOpt = typeNameResInfo
            if isNil rest then 
                if isNil nestedTypes then 
                    NoResultsOrUsefulErrors
                else 
                    match typeNameResFlag with 
                    | ResolveTypeNamesToCtors -> 
                        nestedTypes |> CollectResults (ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo m ad) 
                    | ResolveTypeNamesToTypeRefs -> 
                        OneSuccess (resInfo,Item.Types (nm,nestedTypes),rest)
            else 
                ResolveLongIdentInTypes ncenv nenv lookupKind resInfo (depth+1) m ad rest findFlag typeNameResInfo nestedTypes
        (OneResult contentsSearchAccessible +++ nestedSearchAccessible)
        
and ResolveLongIdentInTypes (ncenv:NameResolver) nenv lookupKind resInfo depth m ad lid findFlag typeNameResInfo typs = 
    typs |> CollectResults (ResolveLongIdentInTypePrim ncenv nenv lookupKind resInfo depth m ad lid findFlag typeNameResInfo >> AtMostOneResult m) 

let ResolveLongIdentInType ncenv nenv lookupKind m ad lid findFlag typeNameResInfo typ =
    let resInfo,item,rest = 
        ResolveLongIdentInTypePrim (ncenv:NameResolver) nenv lookupKind ResolutionInfo.Empty 0 m ad lid findFlag typeNameResInfo typ
        |> AtMostOneResult m
        |> ForceRaise
    ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
    item,rest

// QUERY (instantiationGenerator cleanup): it would be really nice not to flow instantiationGenerator to here. 
// This would help make it the separation between name resolution and 
// type inference more obvious. However this would mean each caller 
// would have to freshen. 
let private ResolveLongIdentInTyconRef (ncenv:NameResolver) nenv lookupKind resInfo depth m ad lid typeNameResInfo tcref =
    let typ = (FreshenTycon ncenv m tcref)
    typ |> ResolveLongIdentInTypePrim ncenv nenv lookupKind resInfo depth m ad lid IgnoreOverrides typeNameResInfo  

let private ResolveLongIdentInTyconRefs (ncenv:NameResolver) nenv lookupKind depth m ad lid typeNameResInfo idRange tcrefs = 
    // The basic search
    tcrefs |> CollectResults (fun (resInfo:ResolutionInfo,tcref) -> 
        let resInfo = resInfo.AddEntity(idRange,tcref)
        tcref |> ResolveLongIdentInTyconRef ncenv nenv lookupKind resInfo depth m ad lid typeNameResInfo |> AtMostOneResult m) 

//-------------------------------------------------------------------------
// ResolveExprLongIdentInModuleOrNamespace 
//------------------------------------------------------------------------- 

let (|AccessibleEntityRef|_|) ad (modref: ModuleOrNamespaceRef) mspec = 
    let eref = modref.MkNestedTyconRef mspec
    if IsEntityAccessible ad eref then Some(eref) else None

let rec ResolveExprLongIdentInModuleOrNamespace (ncenv:NameResolver) nenv typeNameResInfo ad resInfo depth m modref (mty:ModuleOrNamespaceType) (lid :Ident list) =
    // resInfo records the modules or namespaces actually relevant to a resolution
    match lid with 
    | [] -> raze(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))
    | id :: rest ->
        match mty.AllValsByLogicalName.TryFind(id.idText) with
        | Some vspec when IsValAccessible ad (mkNestedValRef modref vspec) -> 
            success(resInfo,Item.Value (mkNestedValRef modref vspec),rest)
        | _->
        match  TryFindTypeWithUnionCase modref id with
        | Some tycon when IsTyconReprAccessible ad (modref.MkNestedTyconRef tycon) -> 
            let ucref = mkUnionCaseRef (modref.MkNestedTyconRef tycon) id.idText 
            let ucinfo = FreshenUnionCaseRef ncenv m ucref
            success (resInfo,Item.UnionCase ucinfo,rest)
        | _ -> 
        match mty.ExceptionDefinitionsByDemangledName.TryFind(id.idText) with
        | Some excon when IsTyconReprAccessible ad (modref.MkNestedTyconRef excon) -> 
            success (resInfo,Item.ExnCase (modref.MkNestedTyconRef excon),rest)
        | _ ->

            // Something in a type? 
            let tyconSearch = 
                let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText (if nonNil rest then None else snd typeNameResInfo) GenerateEstTypeFlag.No modref  
                let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
                if (nonNil rest) then 
                    let tcrefs = CheckForMultipleGenericTypeAmbiguities tcrefs (ResolveTypeNamesToTypeRefs,None) m 
                    ResolveLongIdentInTyconRefs ncenv nenv  LookupKind.Expr (depth+1) m ad rest typeNameResInfo id.idRange tcrefs
                // Check if we've got some explicit type arguments 
                else 
                    let tcrefs = CheckForMultipleGenericTypeAmbiguities tcrefs typeNameResInfo m 
                    match fst typeNameResInfo with 
                    | ResolveTypeNamesToTypeRefs -> 
                        success [ for (resInfo,tcref) in tcrefs do 
                                      let typ = FreshenTycon ncenv m tcref
                                      let item = (resInfo,Item.Types(id.idText,[typ]),[])
                                      yield item ]
                    | ResolveTypeNamesToCtors -> 
                        let typs = tcrefs |> List.map (fun (resInfo, tcref) -> resInfo, FreshenTycon ncenv m tcref) 
                        typs |> CollectResults (fun (resInfo,typ) -> ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo id.idRange ad typ) 

            // Something in a sub-namespace or sub-module 
            let moduleSearch = 
                if (nonNil rest) then 
                    match mty.ModulesAndNamespacesByDemangledName.TryFind(id.idText) with
                    | Some(AccessibleEntityRef ad modref submodref) -> 
                        let resInfo = resInfo.AddEntity(id.idRange,submodref)

                        OneResult (ResolveExprLongIdentInModuleOrNamespace ncenv nenv typeNameResInfo ad resInfo (depth+1) m submodref submodref.ModuleOrNamespaceType rest)
                    | _ -> 
                        NoResultsOrUsefulErrors
                else 
                    NoResultsOrUsefulErrors

            AtMostOneResult id.idRange ( tyconSearch +++   moduleSearch +++ raze (UndefinedName(depth,FSComp.SR.undefinedNameValueConstructorNamespaceOrType,id,[])))


//-------------------------------------------------------------------------

let ChooseTyconRefInExpr ncenv m ad nenv (id:Ident) typeNameResInfo resInfo tcrefs =

      let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
      let tcrefs = CheckForMultipleGenericTypeAmbiguities tcrefs typeNameResInfo m 
      match fst typeNameResInfo with 
      | ResolveTypeNamesToCtors ->
          //let tcrefs = tcrefs |> List.filter (fun (_,tcref) -> tcref.IsILTycon || tcref.IsFSharpObjectModelTycon)
          let typs = tcrefs |> List.map (fun (resInfo,tcref) -> (resInfo,FreshenTycon ncenv m tcref)) 
          typs 
              |> CollectResults (fun (resInfo,typ) -> ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo id.idRange ad typ) 
      | ResolveTypeNamesToTypeRefs ->
          let typs = tcrefs |> List.map (fun (resInfo,tcref) -> (resInfo,FreshenTycon ncenv m tcref)) 
          success (typs |> List.map (fun (resInfo,typ) -> (resInfo,Item.Types(id.idText,[typ]),[])))



/// Resolve F# "A.B.C" syntax in expressions
/// Not all of the sequence will necessarily be swallowed, i.e. we return some identifiers 
/// that may represent further actions, e.g. further lookups. 

let rec ResolveExprLongIdentPrim (ncenv:NameResolver) fullyQualified m ad nenv typeNameResInfo lid =
    let resInfo = ResolutionInfo.Empty
    match lid with 
    | [] -> error (Error(FSComp.SR.nrInvalidExpression(textOfLid lid), m))

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         
    | id :: lid when id.idText = MangledGlobalName ->
          ResolveExprLongIdentPrim ncenv FullyQualified m ad nenv typeNameResInfo lid

    | [id] when fullyQualified <> FullyQualified ->

          let (_,numTyargsOpt) = typeNameResInfo

          // Single identifier.  Lookup the unqualified names in the environment
          let envSearch = 
              match nenv.eUnqualifiedItems.TryFind(id.idText) with

              // The name is a type name and it has not been clobbered by some other name
              | Some (Item.UnqualifiedType tcrefs) -> 
                  
                  // Do not use type names from the environment if an explicit type instantiation is 
                  // given and the number of type parameters do not match
                  let tcrefs = 
                      tcrefs |> List.filter (fun  tcref ->
                          match numTyargsOpt with 
                          | None -> true
                          | Some ntyargs -> ntyargs = tcref.Typars(m).Length)
                  
                  let search = ChooseTyconRefInExpr ncenv m ad nenv (id:Ident) typeNameResInfo resInfo tcrefs
                  match AtMostOneResult m search with 
                  | Result _ as res -> 
                      let resInfo,item,rest = ForceRaise res
                      ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
                      Some(item,rest)
                  | _ ->  
                      None

              | Some res -> 
                  Some (FreshenUnqualifiedItem ncenv m res, [])
              | None -> 
                  None
          match envSearch with 
          | Some res -> res
          | None ->
              // Check if it's a type name, e.g. a constructor call or a type instantiation 
              let ctorSearch = 
                  let tcrefs = LookupTypeNameInEnvMaybeHaveArity fullyQualified id.idText typeNameResInfo nenv
                  ChooseTyconRefInExpr ncenv m ad nenv (id:Ident) typeNameResInfo resInfo tcrefs

              let implicitOpSearch = 
                  if IsMangledOpName id.idText then 
                      success [(resInfo,Item.ImplicitOp id,[])] 
                  else NoResultsOrUsefulErrors

              let failingCase = raze (UndefinedName(0,FSComp.SR.undefinedNameValueOfConstructor,id,[]))
              let search = ctorSearch +++ implicitOpSearch +++ failingCase 
              let resInfo,item,rest = ForceRaise (AtMostOneResult m search) 
              ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
              item,rest
              
            
    // A compound identifier. 
    // It still might be a value in the environment, or something in an F# module, namespace, typ, or nested type 
    | id :: rest -> 
    
        // Values in the environment take total priority, but contructors do NOT for compound lookups, e.g. if someone in some imported  
        // module has defined a constructor "String" (common enough) then "String.foo" doesn't give an error saying 'constructors have no members' 
        // Instead we go lookup the String module or type.
        let ValIsInEnv nm = 
            match fullyQualified with 
            | FullyQualified -> false
            | _ -> 
                match nenv.eUnqualifiedItems.TryFind(nm) with 
                | Some(Item.Value _) -> true 
                | _ -> false

        if ValIsInEnv id.idText then
          nenv.eUnqualifiedItems.[id.idText], rest
        else
          // Otherwise modules are searched first. 
          // For each module referenced by 'id', search the module as if it were an F# module and/or a .NET namespace. 
          let moduleSearch ad = 
               ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv ad lid 
                   (ResolveExprLongIdentInModuleOrNamespace ncenv nenv typeNameResInfo ad)

          // Somewhat surprisingly, this shows up on performance traces, with tcrefs non-nil.
          // This seems strange since we would expect in the vast majority of cases tcrefs is empty here.
          let tyconSearch ad = 
              let tcrefs = LookupTypeNameInEnvNoArity fullyQualified id.idText nenv
              let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
              let tcrefs  = CheckForMultipleGenericTypeAmbiguities tcrefs (ResolveTypeNamesToTypeRefs,None) m 
              ResolveLongIdentInTyconRefs ncenv nenv LookupKind.Expr 1 m ad rest typeNameResInfo id.idRange tcrefs

          let envSearch = 
              match fullyQualified with 
              | FullyQualified -> 
                  NoResultsOrUsefulErrors
              | OpenQualified -> 
                  match Map.tryFind id.idText nenv.eUnqualifiedItems with
                  | Some (Item.UnqualifiedType _) 
                  | None -> NoResultsOrUsefulErrors
                  | Some res -> OneSuccess (resInfo,FreshenUnqualifiedItem ncenv m res,rest)

          let search = moduleSearch ad +++ tyconSearch ad +++ envSearch

          let resInfo,item,rest = 
              match AtMostOneResult m search with 
              | Result _ as res -> 
                  ForceRaise res
              | _ ->  
                  let failingCase = raze (UndefinedName(0,FSComp.SR.undefinedNameValueNamespaceTypeOrModule,id,[]))               
                  ForceRaise (AtMostOneResult m (search +++ moduleSearch AccessibleFromSomeFSharpCode +++ tyconSearch AccessibleFromSomeFSharpCode +++ failingCase))
          ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
          item,rest

let ResolveExprLongIdent (ncenv:NameResolver) m ad nenv typeNameResInfo lid =
    ResolveExprLongIdentPrim ncenv OpenQualified m ad nenv typeNameResInfo lid 

//-------------------------------------------------------------------------
// Resolve F#/IL "." syntax in patterns
//------------------------------------------------------------------------- 

let rec ResolvePatternLongIdentInModuleOrNamespace (ncenv:NameResolver) nenv numTyArgsOpt ad resInfo depth m modref (mty:ModuleOrNamespaceType) lid =
    match lid with 
    | [] -> raze (InternalError("ResolvePatternLongIdentInModuleOrNamespace",m))
    | id :: rest ->
        match TryFindTypeWithUnionCase modref id with
        | Some tycon when IsTyconReprAccessible ad (modref.MkNestedTyconRef tycon) -> 
            let tcref = modref.MkNestedTyconRef tycon
            let ucref = mkUnionCaseRef tcref id.idText
            let ucinfo = FreshenUnionCaseRef ncenv m ucref
            success (resInfo,Item.UnionCase ucinfo,rest)
        | _ -> 
        match mty.ExceptionDefinitionsByDemangledName.TryFind(id.idText) with
        | Some exnc when IsEntityAccessible ad (modref.MkNestedTyconRef exnc) -> 
            success (resInfo,Item.ExnCase (modref.MkNestedTyconRef exnc),rest)
        | _ ->
        // An active pattern constructor in a module 
        match (ActivePatternElemsOfModuleOrNamespace modref).TryFind(id.idText) with
        | Some ( APElemRef(_,vref,_) as apref) when IsValAccessible ad vref -> 
            success (resInfo,Item.ActivePatternCase apref,rest)
        | _ -> 
        match mty.AllValsByLogicalName.TryFind(id.idText) with
        | Some vspec  when IsValAccessible ad (mkNestedValRef modref vspec) -> 
            success(resInfo,Item.Value (mkNestedValRef modref vspec),rest)
        | _ ->
        // Something in a type? e.g. a literal field 
        let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText None GenerateEstTypeFlag.No modref
        let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
        let tyconSearch = 
            match lid with 
            | _tn:: rest when nonNil rest ->
                ResolveLongIdentInTyconRefs (ncenv:NameResolver) nenv LookupKind.Pattern (depth+1) m ad rest numTyArgsOpt id.idRange tcrefs
            | _ -> 
                NoResultsOrUsefulErrors
        // Constructor of a type? 
        let ctorSearch = 
            let typs = tcrefs |> List.map (fun (resInfo,tcref) -> (resInfo,FreshenTycon ncenv m tcref)) 
            typs |> CollectResults (fun (resInfo,typ) -> ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo id.idRange ad typ) 
        // Something in a sub-namespace or sub-module or nested-type 
        let moduleSearch = 
            if (nonNil rest) then 
                match mty.ModulesAndNamespacesByDemangledName.TryFind(id.idText) with
                | Some(AccessibleEntityRef ad modref submodref) -> 
                    let resInfo = resInfo.AddEntity(id.idRange,submodref)
                    OneResult (ResolvePatternLongIdentInModuleOrNamespace ncenv nenv numTyArgsOpt ad resInfo (depth+1) m submodref submodref.ModuleOrNamespaceType rest)
                | _ -> 
                    NoResultsOrUsefulErrors
             else NoResultsOrUsefulErrors
        let res = AtMostOneResult id.idRange ( tyconSearch +++   ctorSearch +++ moduleSearch +++ raze (UndefinedName(depth,FSComp.SR.undefinedNameConstructorModuleOrNamespace,id,[])))
        res
        
exception UpperCaseIdentifierInPattern of range
type WarnOnUpperFlag = WarnOnUpperCase | AllIdsOK

// Long ID in a pattern 
let rec ResolvePatternLongIdentPrim (ncenv:NameResolver) fullyQualified warnOnUpper newDef m ad nenv numTyArgsOpt (lid:Ident list) =
    match lid with 

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         
    | id :: lid when id.idText = MangledGlobalName ->
        ResolvePatternLongIdentPrim ncenv FullyQualified warnOnUpper newDef m ad nenv numTyArgsOpt lid

    // Single identifiers in patterns 
    | [id] when fullyQualified <> FullyQualified ->
          // Single identifiers in patterns - bind to constructors and active patterns 
          // For the special case of 
          //   let C = x 
          match nenv.ePatItems.TryFind(id.idText) with
          | Some res when not newDef  -> FreshenUnqualifiedItem ncenv m res
          | _ -> 
          // Single identifiers in patterns - variable bindings 
          if not newDef &&
             (warnOnUpper = WarnOnUpperCase) && 
             id.idText.Length >= 3 && 
             System.Char.ToLowerInvariant id.idText.[0] <> id.idText.[0] then 
            warning(UpperCaseIdentifierInPattern(m));
          Item.NewDef id
        
    // Long identifiers in patterns 
    | _ -> 
        let moduleSearch ad = 
            ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv ad lid 
                (ResolvePatternLongIdentInModuleOrNamespace ncenv nenv numTyArgsOpt ad)
        let tyconSearch ad = 
            match lid with 
            | tn:: rest when nonNil rest ->
                let tcrefs = LookupTypeNameInEnvNoArity fullyQualified tn.idText nenv
                let tcrefs = tcrefs |> List.map (fun tcref -> (ResolutionInfo.Empty,tcref))
                ResolveLongIdentInTyconRefs ncenv nenv LookupKind.Pattern 1 tn.idRange ad rest numTyArgsOpt tn.idRange tcrefs 
            | _ -> 
                NoResultsOrUsefulErrors
        let resInfo,res,rest = 
            match AtMostOneResult m (tyconSearch ad +++  moduleSearch ad) with 
            | Result _ as res -> ForceRaise res
            | _ ->  
                ForceRaise (AtMostOneResult m (tyconSearch AccessibleFromSomeFSharpCode +++ moduleSearch AccessibleFromSomeFSharpCode))
        ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> true));
  
        if nonNil rest then error(Error(FSComp.SR.nrIsNotConstructorOrLiteral(),(List.head rest).idRange));
        res


let ResolvePatternLongIdent (ncenv:NameResolver) warnOnUpper newDef m ad nenv numTyArgsOpt (lid:Ident list) =
    ResolvePatternLongIdentPrim ncenv OpenQualified warnOnUpper newDef m ad nenv numTyArgsOpt lid

//-------------------------------------------------------------------------
// Resolve F#/IL "." syntax in types
//------------------------------------------------------------------------- 

let DerefAbbrevTyconRef_WORKAROUND (ncenv:NameResolver) (tcref: TyconRef) m =
    // HANDLE NON-GENERIC CASE OF BUG REPORTED TO FSBUGS: Nested types are not found when you abbreviate a .NET type
    //
    // Handling the generic case is harder, e.g. for 
    //    type X = List<int>
    //
    // X.ListEnumerator // should resolve
    if tcref.IsTypeAbbrev && tcref.Typars(m).IsEmpty && isAppTy ncenv.g tcref.TypeAbbrev.Value && isNil (argsOfAppTy ncenv.g tcref.TypeAbbrev.Value) then 
        tcrefOfAppTy ncenv.g tcref.TypeAbbrev.Value
    else
        tcref

let rec ResolveTypeLongIdentInTyconRefPrim (ncenv:NameResolver) typeNameResInfo estInfo ad resInfo depth m (tcref: TyconRef) (lid: Ident list) =
    let tcref = DerefAbbrevTyconRef_WORKAROUND ncenv tcref m
    match lid with 
    | [] -> error(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))
    | [id] -> 
        let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText (snd typeNameResInfo) estInfo tcref 
        let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
        let tcrefs = CheckForMultipleGenericTypeAmbiguities tcrefs typeNameResInfo m 
        match tcrefs with 
        | tcref :: _ -> success tcref
        | [] -> raze (UndefinedName(depth,FSComp.SR.undefinedNameType,id,[]))
    | id::rest ->
        // Search nested types
        let tyconSearch = 
            let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText None estInfo tcref
            let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
            let tcrefs  = CheckForMultipleGenericTypeAmbiguities tcrefs (fst typeNameResInfo,None) m 
            tcrefs |> CollectResults (fun (resInfo,tcref) -> ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo estInfo ad resInfo (depth+1) m tcref rest)
        AtMostOneResult m tyconSearch

let ResolveTypeLongIdentInTyconRef (ncenv:NameResolver) nenv typeNameResInfo ad m tcref (lid: Ident list) =
    let resInfo,tcref = ForceRaise (ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo GenerateEstTypeFlag.No ad ResolutionInfo.Empty 0 m tcref lid)
    ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> true));
    tcref


let rec private ResolveTypeLongIdentInModuleOrNamespace (ncenv:NameResolver) typeNameResInfo estInfo ad (resInfo:ResolutionInfo) depth m modref _mty (lid: Ident list) =
    match lid with 
    | [] -> error(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))
    | [id] -> 
        // On all paths except error reporting we have isSome(numTyargsOpt), hence get at most one result back 
        let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText (snd typeNameResInfo) estInfo modref  
        if nonNil tcrefs then
            tcrefs |> CollectResults (fun tcref -> success(resInfo,tcref))
        else 
            raze (UndefinedName(depth,FSComp.SR.undefinedNameType,id,[]))
    | id::rest ->
        let modulSearch = 
            match modref.ModuleOrNamespaceType.ModulesAndNamespacesByDemangledName.TryFind(id.idText) with
            | Some(AccessibleEntityRef ad modref submodref) -> 
                let resInfo = resInfo.AddEntity(id.idRange,submodref)
                ResolveTypeLongIdentInModuleOrNamespace ncenv typeNameResInfo estInfo ad resInfo (depth+1) m submodref submodref.ModuleOrNamespaceType rest
            | _ ->  
                raze (UndefinedName(depth,FSComp.SR.undefinedNameNamespaceOrModule,id,[]))
        let tyconSearch = 
            let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText None estInfo modref
            tcrefs |> CollectResults (fun tcref -> ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo estInfo ad resInfo (depth+1) m tcref rest)
        tyconSearch +++ modulSearch

let rec ResolveTypeLongIdentPrim (ncenv:NameResolver) fullyQualified estInfo m nenv ad (lid: Ident list) ntyargs =
    match lid with 
    | [] -> error(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         
    | id :: lid when id.idText = MangledGlobalName ->
        ResolveTypeLongIdentPrim ncenv FullyQualified estInfo m nenv ad lid ntyargs

    | [id]  ->  
        match estInfo with 
        | GenerateEstTypeFlag.Yes _ -> error(InternalError("EST types must be referenced by long path",m))
        | _ -> ()
           
        match LookupTypeNameInEnvHaveArity fullyQualified id.idText ntyargs nenv with
        | Some res -> success(ResolutionInfo.Empty,res)
        | None -> 
            // For Good Error Reporting! 
            let tcrefs = LookupTypeNameInEnvNoArity fullyQualified id.idText nenv
            match tcrefs with
            | tcref :: _tcrefs -> 
                // Note: This path is only for error reporting
                //CheckForMultipleGenericTypeAmbiguities tcref rest typeNameResInfo m;
                success(ResolutionInfo.Empty,tcref)
            | [] -> 
                raze (UndefinedName(0,FSComp.SR.undefinedNameType,id,NameMap.domainL nenv.eTyconsByAccessNames))

    | id::rest ->
        let typeNameResInfo = (ResolveTypeNamesToTypeRefs,Some(ntyargs))
        let tyconSearch = 
            match fullyQualified with 
            | FullyQualified ->
                NoResultsOrUsefulErrors
            | OpenQualified -> 
                match LookupTypeNameInEnvHaveArity fullyQualified id.idText ntyargs nenv with
                | Some tcref when IsEntityAccessible ad tcref -> 
                    OneResult (ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo estInfo ad ResolutionInfo.Empty 1 m tcref rest)
                | _ -> 
                    NoResultsOrUsefulErrors
        let modulSearch = 
            ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv ad lid 
                (ResolveTypeLongIdentInModuleOrNamespace ncenv typeNameResInfo estInfo ad)
            |?> (fun res -> List.concat res)

        let modulSearchFailed = 
            ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv AccessibleFromSomeFSharpCode lid 
                (ResolveTypeLongIdentInModuleOrNamespace ncenv (ResolveTypeNamesToTypeRefs,None) estInfo ad)
            |?> (fun res -> List.concat res)
        match tyconSearch +++ modulSearch with 
        | Result results -> 
            // NOTE: we delay checking the CheckForMultipleGenericTypeAmbiguities condition until right at the end after we've
            // collected all possible resolutions of the type
            let tcrefs = CheckForMultipleGenericTypeAmbiguities results typeNameResInfo m 
            match tcrefs with 
            | (resInfo,tcref) :: _ -> 
                // We've already reported the ambiguity, possibly as an error. Now just take the first possible result.
                success(resInfo,tcref)
            | [] -> 
                // failing case - report nice ambiguity errors even in this case
                AtMostOneResult m ((tyconSearch +++ modulSearch +++ modulSearchFailed) |?> (fun tcrefs -> CheckForMultipleGenericTypeAmbiguities tcrefs typeNameResInfo m))
            
        | _ ->  
            // failing case - report nice ambiguity errors even in this case
            AtMostOneResult m ((tyconSearch +++ modulSearch +++ modulSearchFailed) |?> (fun tcrefs -> CheckForMultipleGenericTypeAmbiguities tcrefs typeNameResInfo m))


let ResolveTypeLongIdent (ncenv:NameResolver) occurence fullyQualified estInfo nenv ad (lid: Ident list) ntyargs =
    let m = rangeOfLid lid
    let res = ResolveTypeLongIdentPrim ncenv fullyQualified estInfo m nenv ad lid ntyargs 
    // Register the result as a name resolution
    match res with 
    | Result (resInfo,tcref) -> 
        ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> true));
        CallNameResolutionSink(m,nenv,Item.Types(tcref.DisplayName,[FreshenTycon ncenv m tcref]),occurence,nenv.eDisplayEnv,ad)
    | _ -> ()
    res |?> snd

//-------------------------------------------------------------------------
// Resolve F#/IL "." syntax in records etc.
//------------------------------------------------------------------------- 

let rec ResolveFieldInModuleOrNamespace (ncenv:NameResolver) nenv ad (resInfo:ResolutionInfo) depth m (modref: ModuleOrNamespaceRef) _mty lid = 
    let typeNameResInfo = DefaultTypeNameResInfo
    match lid with 
    | id::rest -> 
        let error = raze (UndefinedName(depth,FSComp.SR.undefinedNameRecordLabelOrNamespace,id,[]))
        // search for module-qualified names, e.g. { Microsoft.FSharp.Core.contents = 1 } 
        let modulScopedFieldNames = 
            match TryFindTypeWithRecdField modref id  with
            | Some tycon when IsEntityAccessible ad (modref.MkNestedTyconRef tycon) -> 
                success(modref.MkNestedRecdFieldRef tycon id, rest)
            | _ -> error
        // search for type-qualified names, e.g. { Microsoft.FSharp.Core.Ref.contents = 1 } 
        let tyconSearch = 
            match lid with 
            | _tn:: rest when nonNil rest ->
                let tcrefs = LookupTypeNameInEntityMaybeHaveArity ad id.idRange id.idText None GenerateEstTypeFlag.No modref
                let tcrefs = tcrefs |> List.map (fun tcref -> (ResolutionInfo.Empty,tcref))
                let tyconSearch = ResolveLongIdentInTyconRefs ncenv nenv LookupKind.RecdField  (depth+1) m ad rest typeNameResInfo id.idRange tcrefs
                // choose only fields 
                let tyconSearch = tyconSearch |?> List.choose (function (_,Item.RecdField(RecdFieldInfo(_,rfref)),rest) -> Some(rfref,rest) | _ -> None)
                tyconSearch
            | _ -> 
                NoResultsOrUsefulErrors
        // search for names in nested modules, e.g. { Microsoft.FSharp.Core.contents = 1 } 
        let modulSearch = 
            if nonNil rest then 
                match modref.ModuleOrNamespaceType.ModulesAndNamespacesByDemangledName.TryFind(id.idText) with
                | Some(AccessibleEntityRef ad modref submodref) -> 
                    let resInfo = resInfo.AddEntity(id.idRange,submodref)
                    ResolveFieldInModuleOrNamespace ncenv nenv ad resInfo (depth+1) m submodref submodref.ModuleOrNamespaceType  rest 
                | _ -> 
                    error
            else error
        AtMostOneResult m (OneResult modulScopedFieldNames +++ tyconSearch +++ OneResult modulSearch)
    | [] -> 
        error(InternalError("ResolveFieldInModuleOrNamespace",m))

let ResolveField (ncenv:NameResolver) nenv ad typ (mp,id:Ident) =
    let typeNameResInfo = DefaultTypeNameResInfo
    let g = ncenv.g
    let m = id.idRange
    match mp with 
    | [] -> 
        if isAppTy g typ then 
            match ncenv.InfoReader.TryFindRecdFieldInfoOfType(id.idText,m,typ) with
            | Some (RecdFieldInfo(_,rfref)) -> [rfref]
            | None -> error(Error(FSComp.SR.nrTypeDoesNotContainSuchField((NicePrint.prettyStringOfTy nenv.eDisplayEnv typ), id.idText),m))
        else 
            let frefs = 
                try Map.find id.idText nenv.eFieldLabels 
                with :? KeyNotFoundException -> error (UndefinedName(0,FSComp.SR.undefinedNameRecordLabel,id,NameMap.domainL nenv.eFieldLabels))
            // Eliminate duplicates arising from multiple 'open' 
            let frefs = frefs |> ListSet.setify (fun fref1 fref2 -> tyconRefEq g fref1.TyconRef fref2.TyconRef)
            frefs
                        
    | _ -> 
        let lid = (mp@[id])
        let tyconSearch ad = 
            match lid with 
            | tn:: (_ :: _ as rest) -> 
                let m = tn.idRange
                let tcrefs = LookupTypeNameInEnvNoArity OpenQualified tn.idText nenv
                let tcrefs = tcrefs |> List.map (fun tcref -> (ResolutionInfo.Empty,tcref))
                let tyconSearch = ResolveLongIdentInTyconRefs ncenv nenv LookupKind.RecdField 1 m ad rest typeNameResInfo tn.idRange tcrefs
                // choose only fields 
                let tyconSearch = tyconSearch |?> List.choose (function (_,Item.RecdField(RecdFieldInfo(_,rfref)),rest) -> Some(rfref,rest) | _ -> None)
                tyconSearch
            | _ -> NoResultsOrUsefulErrors
        let modulSearch ad = 
            ResolveLongIndentAsModuleOrNamespaceThen OpenQualified nenv ad lid 
                (ResolveFieldInModuleOrNamespace ncenv nenv ad)
        let item,rest = ForceRaise (AtMostOneResult m (modulSearch ad +++ tyconSearch ad +++ modulSearch AccessibleFromSomeFSharpCode +++ tyconSearch AccessibleFromSomeFSharpCode))
        if nonNil rest then errorR(Error(FSComp.SR.nrInvalidFieldLabel(),(List.head rest).idRange));
        [item]

/// Generate a new reference to a record field with a fresh type instantiation
let FreshenRecdFieldRef (ncenv:NameResolver) m (rfref:RecdFieldRef) =
    Item.RecdField(RecdFieldInfo(ncenv.instantiationGenerator m (rfref.Tycon.Typars(m)), rfref))



/// Resolve F#/IL "." syntax in expressions (2).
/// We have an expr. on the left, and we do an access, e.g. 
/// (f obj).field or (f obj).meth.  The basic rule is that if l-r type 
/// inference has determined the outer type then we can proceed in a simple fashion. The exception 
/// to the rule is for field types, which applies if l-r was insufficient to 
/// determine any valid members 
//
// QUERY (instantiationGenerator cleanup): it would be really nice not to flow instantiationGenerator to here. 
let private ResolveExprDotLongIdent (ncenv:NameResolver) m ad nenv typ lid findFlag =
    let typeNameResInfo = DefaultTypeNameResInfo
    let adhoctDotSearchAccessible = AtMostOneResult m (ResolveLongIdentInTypePrim ncenv nenv LookupKind.Expr ResolutionInfo.Empty 1 m ad lid findFlag typeNameResInfo typ)
    match adhoctDotSearchAccessible with 
    | Exception _ ->
        // If the dot is not resolved by adhoc overloading then look for a record field 
        // that can resolve the name. 
        let dotFieldIdSearch = 
            match lid with 
            // A unique record label access, e.g  expr.field  
            | id::rest when nenv.eFieldLabels.ContainsKey(id.idText) -> 
                match nenv.eFieldLabels.[id.idText] with
                | [] -> NoResultsOrUsefulErrors
                | rfref :: _ ->
                    // NOTE (instantiationGenerator cleanup): we need to freshen here because we don't know the type. 
                    // But perhaps the caller should freshen?? 
                    let item = FreshenRecdFieldRef ncenv m rfref
                    OneSuccess (ResolutionInfo.Empty,item,rest)
            | _ -> NoResultsOrUsefulErrors 
        
        let search = dotFieldIdSearch 
        match AtMostOneResult m search with 
        | Result _ as res -> ForceRaise res
        | _ -> 
            let adhoctDotSearchAll = ResolveLongIdentInTypePrim ncenv nenv LookupKind.Expr ResolutionInfo.Empty 1 m AccessibleFromSomeFSharpCode lid findFlag typeNameResInfo typ 
            ForceRaise (AtMostOneResult m (search +++ adhoctDotSearchAll))

    | Result _ -> 
        ForceRaise adhoctDotSearchAccessible

let ComputeItemRange wholem (lid: Ident list) rest =
    match rest with
    | [] -> wholem
    | _ -> 
        let ids,_ = List.chop (max 0 (lid.Length - rest.Length)) lid
        match ids with 
        | [] -> wholem
        | _ -> rangeOfLid ids

/// Filters method groups that will be sent to Visual Studio IntelliSense
/// to include only static/instance members

let filterMethodGroups (ncenv:NameResolver) itemRange item staticOnly =
    match item with
    | Item.MethodGroup(nm, minfos) -> 
        let minfos = minfos |> List.filter  (fun minfo -> 
           staticOnly = (ObjTypesOfMethInfo ncenv.amap itemRange minfo minfo.FormalMethodInst |> isNil))
        Item.MethodGroup(nm, minfos)
    | item -> item

/// Called for 'TypeName.Bar' - for VS IntelliSense, we can filter out instance members from method groups
let ResolveLongIdentAsExprAndComputeRange (ncenv:NameResolver) wholem ad nenv typeNameResInfo lid = 
    let item,rest = ResolveExprLongIdent ncenv wholem ad nenv typeNameResInfo lid
    let itemRange = ComputeItemRange wholem lid rest
    
    // Record the precise resolution of the field for intellisense
    CallNameResolutionSink(itemRange, nenv, filterMethodGroups ncenv itemRange item true, ItemOccurence.Use, nenv.DisplayEnv, ad);
    item, itemRange, rest

/// Called for 'expression.Bar' - for VS IntelliSense, we can filter out static members from method groups
let ResolveExprDotLongIdentAndComputeRange (ncenv:NameResolver) wholem ad nenv typ lid findFlag = 
    let resInfo,item,rest = ResolveExprDotLongIdent ncenv wholem ad nenv typ lid findFlag
    let itemRange = ComputeItemRange wholem lid rest
    ResolutionInfo.SendToSink(ncenv,nenv,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap itemRange item));
    
    // Record the precise resolution of the field for intellisense
    CallNameResolutionSink(itemRange, nenv, filterMethodGroups ncenv itemRange item false, ItemOccurence.Use, nenv.DisplayEnv, ad);
    item, itemRange, rest


//-------------------------------------------------------------------------
// Given an nenv resolve partial paths to sets of names, used by interactive
// environments (Visual Studio)
//
// ptc = partial type check
// ptci = partial type check item
//
// There are some inefficiencies in this code - e.g. we often 
// create potentially large lists of methods/fields/properties and then
// immiediately List.filter them.  We also use lots of "map/concats".  Dosen't
// seem to hit the interactive experience too badly though.
//------------------------------------------------------------------------- 

let FakeInstantiationGenerator (_m:range) gps = List.map mkTyparTy gps 

// note: making local refs is ok since it is only used by VS 
let ItemForModuleOrNamespaceRef v = Item.ModuleOrNamespaces [v]
let ItemForPropInfo (pinfo:PropInfo) = Item.Property (pinfo.PropertyName, [pinfo])
let ItemForMethInfos (nm,minfos) = MakeMethGroup(nm, minfos)

let IsTyconUnseenObsoleteSpec ad g m (x:TyconRef) allowObsolete = 
    not (IsEntityAccessible ad x) ||
    ((not allowObsolete) &&
      (if x.IsILTycon then 
          CheckILAttribsForUnseen g x.ILTyconRawMetadata.CustomAttrs m
       else 
          CheckAttribsForUnseen g x.Attribs m))

let IsTyconUnseen ad g m (x:TyconRef) = IsTyconUnseenObsoleteSpec ad g m x false

let IsValUnseen ad g m (v:ValRef) = 
    not (IsValAccessible ad v) ||
    v.IsCompilerGenerated ||
    v.Deref.IsClassConstructor ||
    CheckAttribsForUnseen g v.Attribs m

let IsUnionCaseUnseen ad g m (ucref:UnionCaseRef) = 
    not (IsUnionCaseAccessible ad ucref) ||
    IsTyconUnseen ad g m ucref.TyconRef || 
    CheckAttribsForUnseen g ucref.Attribs m

let ItemIsUnseen ad g m item = 
    match item with 
    | Item.Value x -> IsValUnseen ad  g m x
    | Item.UnionCase x -> IsUnionCaseUnseen ad g m x.UnionCaseRef
    | Item.ExnCase x -> IsTyconUnseen ad g m x
    | _ -> false

let ItemOfTyconRef ncenv m (x:TyconRef) = 
    Item.Types (x.DisplayName,[FreshenTycon ncenv m x])

let ItemOfTy g x = 
    let nm = if isAppTy g x then (tcrefOfAppTy g x).DisplayName else "?"
    Item.Types (nm,[x])

// Filter out 'PrivateImplementationDetail' classes 
let IsInterestingModuleName nm =
    String.length nm >= 1 &&
    String.sub nm 0 1 <> "<"

let rec PartialResolveLookupInModuleOrNamespaceAsModuleOrNamespaceThen f plid (modref:ModuleOrNamespaceRef) =
    let mty = modref.ModuleOrNamespaceType
    match plid with 
    | [] -> f modref
    | id:: rest -> 
        match mty.ModulesAndNamespacesByDemangledName.TryFind(id) with
        | Some mty -> PartialResolveLookupInModuleOrNamespaceAsModuleOrNamespaceThen f rest (modref.MkNestedTyconRef mty) 
        | None -> []

let PartialResolveLongIndentAsModuleOrNamespaceThen (nenv:NameResolutionEnv) plid f =
    match plid with 
    | id:: rest -> 
        match Map.tryFind id nenv.eModulesAndNamespaces with
        | Some(modrefs) -> 
            List.collect (PartialResolveLookupInModuleOrNamespaceAsModuleOrNamespaceThen f rest) modrefs
        | None ->
            []
    | [] -> []

let ResolveCompletionsInType (ncenv: NameResolver) nenv m ad statics typ =
    let g = ncenv.g
    let amap = ncenv.amap
    let rfinfos = 
        if isAppTy g typ then 
            let tc,tinst = destAppTy g typ
            tc.AllFieldAsRefList
            |> List.filter (IsRecdFieldAccessible ad)
            |> List.filter (fun fref -> fref.RecdField.IsStatic = statics)
            |> List.filter (fun fref -> not fref.RecdField.IsCompilerGenerated)
            |> List.map (fun fref -> RecdFieldInfo(tinst,fref)) 
        else []

    let ucinfos = 
        if statics  && isAppTy g typ then 
            let tc,tinst = destAppTy g typ
            tc.UnionCasesAsRefList 
            |> List.filter (IsUnionCaseUnseen ad g m >> not)
            |> List.map (fun ucref ->  Item.UnionCase(UnionCaseInfo(tinst,ucref)))
        else []

    let einfos = 
        ncenv.InfoReader.GetEventInfosOfType(None,ad,m,typ)
        |> List.filter (fun x -> 
            IsStandardEventInfo ncenv.InfoReader m ad x &&
            x.IsStatic = statics)

    let nestedTypes = 
        typ
        |> GetNestedTypesOfType ad ncenv (None,None) m 

    let finfos = 
        ncenv.InfoReader.GetILFieldInfosOfType(None,ad,m,typ)
        |> List.filter (fun x -> 
            not x.IsSpecialName &&
            x.IsStatic = statics && 
            IsILFieldInfoAccessible g amap m ad x)

    let pinfos = 
        AllPropInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (None,ad) IgnoreOverrides m typ
        |> List.filter (fun x -> 
            x.IsStatic = statics && 
            not (PropInfoIsUnseen m x) &&
            IsPropInfoAccessible g amap m ad x)


    // Exclude get_ and set_ methods accessed by properties 
    let pinfoMethNames = 
      (pinfos 
       |> List.filter PropInfo.HasGetter
       |> List.map (fun pinfo -> pinfo.GetterMethod.LogicalName))
      @
      (pinfos 
       |> List.filter PropInfo.HasSetter
       |> List.map (fun pinfo -> pinfo.SetterMethod.LogicalName))
    
    let einfoMethNames = 
        [ for einfo in einfos do 
             match einfo with 
             | ILEvent(_, e) -> 
                 let delegateType = einfo.GetDelegateType(amap,m)
                 let invokeMethInfo,_,_,_ = GetSigOfFunctionForDelegate ncenv.InfoReader delegateType m ad 
                 // Only "standard" events are suppressed in intellisense
                 if slotSigHasVoidReturnTy (SlotSigOfMethodInfo amap m invokeMethInfo) then 
                     yield e.AddMethod.ILName
                     yield e.RemoveMethod.ILName
             | _ -> 
                 () ]

    let suppressedMethNames = Zset.ofList String.order (pinfoMethNames @ einfoMethNames)

    let minfoFilter (minfo:MethInfo) = 
        // Only show the Finalize, MemberwiseClose etc. methods on System.Object for values whose static type really is 
        // System.Object. Few of these are typically used from F#.  
        //
        // Don't show GetHashCode or Equals for F# types that admit equality as an abnormal operation
        (typeEquiv g typ g.obj_ty 
         || minfo.LogicalName = "GetType" 
         || (minfo.LogicalName = "GetHashCode"  && Augment.ApproxTypeHasEquality g typ)
         || minfo.LogicalName = "ToString" 
         || (minfo.IsInstance && minfo.LogicalName = "Equals" && Augment.ApproxTypeHasEquality g typ)
         || not (typeEquiv g minfo.EnclosingType g.obj_ty)) &&
        not minfo.IsInstance = statics &&
        IsMethInfoAccessible amap m ad minfo &&
        not (MethInfoIsUnseen g m minfo) &&
        not minfo.IsConstructor &&
        not minfo.IsClassConstructor &&
        not (suppressedMethNames.Contains minfo.LogicalName)

    let pinfoItems = 
        pinfos
        |> List.map (fun pinfo -> DecodeFSharpEvent [pinfo] ad g ncenv m)
        |> List.filter (fun pinfo->pinfo.IsSome)
        |> List.map (fun pinfo->pinfo.Value)

    let addersAndRemovers = 
        pinfoItems 
        |> List.map (function Item.Event(FSEvent(_,_,addValRef,removeValRef)) -> [addValRef.LogicalName;removeValRef.LogicalName] | _ -> [])
        |> List.concat
    
    let minfos = 
        AllMethInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (None,ad) IgnoreOverrides m typ 
        |> List.filter minfoFilter
        |> List.filter (fun minfo -> not(addersAndRemovers|>List.exists (fun ar-> ar = minfo.LogicalName)))

    // Partition methods into overload sets
    let rec partitionl (l:MethInfo list) acc = 
        match l with
        | [] -> acc
        | h::t -> 
            let nm = h.LogicalName
            partitionl t (NameMultiMap.add nm h acc)



    // Build the results
    ucinfos @
    List.map Item.RecdField rfinfos @
    pinfoItems @
    List.map Item.ILField finfos @
    List.map Item.Event einfos @
    List.map (ItemOfTy g) nestedTypes @
    List.map ItemForMethInfos (NameMap.toList (partitionl minfos Map.empty))
      

let rec ResolvePartialLongIdentInType (ncenv: NameResolver) nenv m ad statics plid typ =
    let g = ncenv.g
    let amap = ncenv.amap
    match plid with
    | [] -> ResolveCompletionsInType ncenv nenv m ad statics typ
    | id :: rest ->
  
      let rfinfos = 
        if isAppTy g typ then 
            let tc,tinst = destAppTy g typ
            tc.AllFieldAsRefList
            |> List.filter (IsRecdFieldAccessible ad)
            |> List.filter (fun fref -> fref.RecdField.IsStatic = statics)
            |> List.filter (fun fref -> not fref.RecdField.IsCompilerGenerated)
            |> List.map (fun fref -> RecdFieldInfo(tinst,fref)) 
        else 
            []
  
      let nestedTypes = 
          typ 
          |> GetNestedTypesOfType ad ncenv (Some(id),None) m  

      // e.g. <val-id>.<recdfield-id>.<more> 
      (rfinfos |> List.filter (fun x -> x.Name = id)
               |> List.collect (fun x -> x.FieldType |> ResolvePartialLongIdentInType ncenv nenv m ad false rest)) @

      // e.g. <val-id>.<property-id>.<more> 
      let FullTypeOfPinfo(pinfo:PropInfo) = 
        let rty = pinfo.PropertyType(amap,m) 
        let rty = if pinfo.IsIndexer then mkTupledTy g (List.map snd (pinfo.ParamNamesAndTypes(amap, m))) --> rty else  rty 
        rty      
      (typ
         |> AllPropInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (Some(id),ad) IgnoreOverrides m
         |> List.filter (fun x -> x.IsStatic = statics)
         |> List.filter (IsPropInfoAccessible g amap m ad) 
         |> List.collect (fun pinfo -> (FullTypeOfPinfo pinfo) |> ResolvePartialLongIdentInType ncenv nenv m ad false rest)) @

      // e.g. <val-id>.<event-id>.<more> 
      (ncenv.InfoReader.GetEventInfosOfType(Some(id),ad,m,typ)
         |> List.collect (PropTypOfEventInfo ncenv.InfoReader m ad >> ResolvePartialLongIdentInType ncenv nenv m ad false rest)) @

      // nested types! 
      (nestedTypes 
         |> List.collect (ResolvePartialLongIdentInType ncenv nenv m ad statics rest)) @

      // e.g. <val-id>.<il-field-id>.<more> 
      (ncenv.InfoReader.GetILFieldInfosOfType(Some(id),ad,m,typ)
         |> List.filter (fun x -> 
             not x.IsSpecialName &&
             x.IsStatic = statics && 
             IsILFieldInfoAccessible g amap m ad x)
         |> List.collect (fun x -> x.FieldType(amap,m) |> ResolvePartialLongIdentInType ncenv nenv m ad false rest))
     
let InfosForTyconConstructors (ncenv:NameResolver) m ad (tcref:TyconRef) = 
    let g = ncenv.g
    let amap = ncenv.amap
    // Don't show constructors for type abbreviations. See FSharp 1.0 bug 2881
    if tcref.IsTypeAbbrev then 
        []
    else 
        let typ = FreshenTycon ncenv m tcref
        match ResolveObjectConstructor ncenv (DisplayEnv.Empty g) m ad typ with 
        | Result (item,_) -> 
            begin match item with 
            | Item.CtorGroup(nm,cinfos) -> 
                cinfos 
                |> List.filter (IsMethInfoAccessible amap m ad)
                |> List.filter (MethInfoIsUnseen g m >> not)
                |> List.map (fun minfo -> MakeCtorGroup(nm,[minfo])) 
            | item -> 
                [item]
            end
        | Exception _ -> []

/// import.fs creates somewhat fake modules for nested members of types (so that 
/// types never contain other types) 
let notFakeContainerModule tyconNames nm = 
    not (Set.contains nm tyconNames)

/// Check is a namesapce or module contains something accessible 
let rec private EntityRefContainsSomethingAccessible (ncenv: NameResolver) m ad (modref:ModuleOrNamespaceRef) = 
    let g = ncenv.g
    let mty = modref.ModuleOrNamespaceType

    // Search the values in the module for an accessible value 
    (mty.AllValsAndMembers
     |> Seq.exists (fun v -> 
         // This may explore assemblies that are not in the reference set,
         // e.g. for extension members that extend a type not in the reference set. 
         // In this case assume it is accessible. The user may later explore this module 
         // but will not see the extension members anyway.
         //
         // Note: this is the only use of protectAssemblyExplorationNoReraise.
         protectAssemblyExplorationNoReraise  true false
             (fun () -> 
                 let vref = mkNestedValRef modref v
                 not vref.IsCompilerGenerated && 
                 not (IsValUnseen ad g m vref) &&
                 (vref.IsExtensionMember || vref.MemberInfo.IsNone)))) ||

    // Search the types in the namespace/module for an accessible tycon 
    (mty.AllEntities
     |> QueueList.exists (fun tc ->  
          not tc.IsModuleOrNamespace && 
          not (IsTyconUnseen ad g m (modref.MkNestedTyconRef tc)))) ||

    // Search the sub-modules of the namespace/modulefor something accessible 
    (mty.ModulesAndNamespacesByDemangledName 
     |> NameMap.exists (fun _ submod -> 
        let submodref = modref.MkNestedTyconRef submod
        EntityRefContainsSomethingAccessible ncenv m ad submodref)) 

let rec ResolvePartialLongIdentInModuleOrNamespace (ncenv: NameResolver) nenv m ad (modref:ModuleOrNamespaceRef) plid allowObsolete =
    let g = ncenv.g
    let mty = modref.ModuleOrNamespaceType
    
    let tycons = 
        mty.TypeDefinitions
        |> List.filter (fun tycon -> not (IsTyconUnseen ad g m (modref.MkNestedTyconRef tycon)))

    let iltyconNames = 
        mty.TypesByAccessNames
        |> NameMultiMap.range
        |> List.choose (fun (tycon:Tycon) -> if tycon.IsILTycon then Some(tycon.DisplayName) else None)
        |> Set.ofSeq      
    
    match plid with 
    | [] -> 

         // Collect up the accessible values in the module, excluding the members
         (mty.AllValsAndMembers
          |> Seq.toList
          |> List.choose (tryMkValRefInModRef modref) // if the assembly load set is incomplete and we get a None value here, then ignore the value
          |> List.filter (fun v -> v.MemberInfo.IsNone)
          |> List.filter (IsValUnseen ad g m >> not) 
          |> List.map Item.Value)

         // Collect up the accessible discriminated union cases in the module 
       @ (UnionCaseRefsInModuleOrNamespace modref 
          |> List.filter (IsUnionCaseUnseen ad g m >> not)
          |> List.map GeneralizeUnionCaseRef 
          |> List.map Item.UnionCase)

         // Collect up the accessible active patterns in the module 
       @ (ActivePatternElemsOfModuleOrNamespace modref 
          |> NameMap.range
          |> List.filter (fun apref -> apref.ActivePatternVal |> IsValUnseen ad g m |> not) 
          |> List.map Item.ActivePatternCase)


         // Collect up the accessible F# exception declarations in the module 
       @ (mty.ExceptionDefinitionsByDemangledName 
          |> NameMap.range 
          |> List.map modref.MkNestedTyconRef
          |> List.filter (IsTyconUnseen ad g m >> not)
          |> List.map Item.ExnCase)

         // Collect up the accessible sub-modules 
       @ (mty.ModulesAndNamespacesByDemangledName 
          |> NameMap.range 
          |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> notFakeContainerModule iltyconNames)
          |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> IsInterestingModuleName)
          |> List.map modref.MkNestedTyconRef
          |> List.filter (IsTyconUnseen ad g m >> not)
          |> List.filter (EntityRefContainsSomethingAccessible ncenv m ad)
          |> List.map ItemForModuleOrNamespaceRef)

    // Get all the types and .NET constructor groups accessible from here 
       @ (tycons 
          |> List.map (modref.MkNestedTyconRef >> ItemOfTyconRef ncenv m) )

       @ (tycons 
          |> List.map (modref.MkNestedTyconRef >> InfosForTyconConstructors ncenv m ad) |> List.concat)

    | id :: rest  -> 
        (match mty.ModulesAndNamespacesByDemangledName.TryFind(id) with
         | Some mspec 
             when not (IsTyconUnseenObsoleteSpec ad g m (modref.MkNestedTyconRef mspec) allowObsolete) -> 
             let allowObsolete = rest <> [] && allowObsolete
             ResolvePartialLongIdentInModuleOrNamespace ncenv nenv m ad (modref.MkNestedTyconRef mspec) rest allowObsolete
         | _ -> [])

      @ (LookupTypeNameInEntityNoArity m id modref.ModuleOrNamespaceType
         |> List.collect (fun tycon ->
             let tcref = modref.MkNestedTyconRef tycon 
             if not (IsTyconUnseenObsoleteSpec ad g m tcref allowObsolete) then 
                 tcref |> generalizedTyconRef |> ResolvePartialLongIdentInType ncenv nenv m ad true rest
             else 
                 []))

/// allowObsolete - specifies whether we should return obsolete types & modules 
///   as (no other obsolete items are returned)
let rec ResolvePartialLongIdentPrim (ncenv: NameResolver) (nenv: NameResolutionEnv) fullyQualified m ad plid allowObsolete = 
    let g = ncenv.g

    match  plid with
    |  id :: plid when id = "global" -> // this is deliberately not the mangled name

       ResolvePartialLongIdentPrim ncenv nenv FullyQualified m ad plid allowObsolete

    |  [] -> 
    
       let iltyconNames =
          nenv.TyconsByAccessNames(fullyQualified)
          |> NameMultiMap.range
          |> List.choose (fun tyconRef -> if tyconRef.IsILTycon then Some(tyconRef.DisplayName) else None)
          |> Set.ofSeq      
       
       let items = 
           match fullyQualified with 
           | FullyQualified -> []
           | OpenQualified ->
               nenv.eUnqualifiedItems
               |> NameMap.range
               |> List.filter (function Item.UnqualifiedType _ -> false | _ -> true)
               |> List.filter (ItemIsUnseen ad g m >> not)

       let apats = 
           match fullyQualified with 
           | FullyQualified -> []
           | OpenQualified ->
               nenv.ePatItems
               |> NameMap.range
               |> List.filter (function Item.ActivePatternCase _v -> true | _ -> false)

       let mods = 
           nenv.ModulesAndNamespaces(fullyQualified)
           |> NameMultiMap.range 
           |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> IsInterestingModuleName  )
           |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> notFakeContainerModule iltyconNames)
           |> List.filter (EntityRefContainsSomethingAccessible ncenv m ad)
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.map ItemForModuleOrNamespaceRef

       let tycons = 
           nenv.TyconsByDemangledNameAndArity(fullyQualified)
           |> NameMap.range
           |> List.filter (fun tcref -> not tcref.IsExceptionDecl) 
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.map (ItemOfTyconRef ncenv m)

       // Get all the constructors accessible from here
       let constructors =  
           nenv.TyconsByDemangledNameAndArity(fullyQualified)
           |> NameMap.range
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.collect (InfosForTyconConstructors ncenv m ad)

       items @ apats @ mods @ tycons @ constructors 

    | id :: rest -> 
    
        // Look in the namespaces 'id' 
        let namespaces = 
            PartialResolveLongIndentAsModuleOrNamespaceThen nenv [id] (fun modref -> 
              let allowObsolete = rest <> [] && allowObsolete
              if EntityRefContainsSomethingAccessible ncenv m ad modref then 
                ResolvePartialLongIdentInModuleOrNamespace ncenv nenv m ad modref rest allowObsolete
              else 
                [])
        // Look for values called 'id' that accept the dot-notation 
        let values,isItemVal = 
            (if nenv.eUnqualifiedItems.ContainsKey(id) then 
                     // v.lookup : member of a value
              let v = Map.find id nenv.eUnqualifiedItems
              match v with 
              | Item.Value x -> 
                  let typ = x.Type
                  let typ = if x.BaseOrThisInfo = CtorThisVal && isRefCellTy g typ then destRefCellTy g typ else typ
                  (ResolvePartialLongIdentInType ncenv nenv m ad false rest  typ),true
              | _ -> [],false
             else [],false)
        let staticSometingInType = 
            [ if not isItemVal then 
                // type.lookup : lookup a static something in a type 
                for tcref in LookupTypeNameInEnvNoArity OpenQualified id nenv do
                    let tcref = DerefAbbrevTyconRef_WORKAROUND ncenv tcref m
                    let typ = FreshenTycon ncenv m tcref
                    yield! ResolvePartialLongIdentInType ncenv nenv m ad true rest typ ]
        namespaces @ values @ staticSometingInType
        

let ResolvePartialLongIdent ncenv nenv m ad plid allowObsolete = 
    ResolvePartialLongIdentPrim ncenv nenv OpenQualified m ad plid allowObsolete 

