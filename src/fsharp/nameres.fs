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

//-------------------------------------------------------------------------
// Name environment and name resolution 
//------------------------------------------------------------------------- 


module internal Microsoft.FSharp.Compiler.Nameres

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

#if EXTENSIONTYPING
open Microsoft.FSharp.Compiler.ExtensionTyping
#endif

/// An object that captures the logical context for name resolution.
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
    member nr.InstantiationGenerator = instantiationGenerator
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
  // These exist in the "eUnqualifiedItems" map in the type environment. 
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
  /// CustomOperation(nm, helpText, methInfo)
  /// 
  /// Used to indicate the availability or resolution of a custom query operation such as 'sortBy' or 'where' in computation expression syntax
  | CustomOperation of string * (unit -> string option) * MethInfo option
  | CustomBuilder of string * ValRef
  | TypeVar of string 
  | ModuleOrNamespaces of Tast.ModuleOrNamespaceRef list
  | ImplicitOp of Ident
  | ArgName of Ident * TType
  | SetterArg of Ident * Item 
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
      eDisplayEnv: DisplayEnv 

      /// Values and Data Tags available by unqualified name 
      eUnqualifiedItems: LayeredMap<string,Item>

      /// Data Tags and Active Pattern Tags available by unqualified name 
      ePatItems: NameMap<Item>

      /// Modules accessible via "." notation. Note this is a multi-map. 
      /// Adding a module abbreviation adds it a local entry to this List.map. 
      /// Likewise adding a ccu or opening a path adds entries to this List.map. 
      
      
      /// REVIEW (old comment)
      /// "The boolean flag is means the namespace or module entry shouldn't 'really' be in the 
      ///  map, and if it is everr used to resolve a name then we give a warning. 
      ///  This is used to give warnings on unqualified namespace accesses, e.g. 
      ///    open System 
      ///    open Collections                            <--- give a warning 
      ///    let v = new Collections.Generic.List<int>() <--- give a warning" 
      
      eModulesAndNamespaces:  NameMultiMap<Tast.ModuleOrNamespaceRef>
      
      /// Fully qualified modules and namespaces. 'open' does not change this. 
      eFullyQualifiedModulesAndNamespaces:  NameMultiMap<Tast.ModuleOrNamespaceRef>
      
      /// RecdField labels in scope.  RecdField labels are those where type are inferred 
      /// by label rather than by known type annotation. 
      /// Bools indicate if from a record, where no warning is given on indeterminate lookup 
      eFieldLabels: NameMultiMap<Tast.RecdFieldRef>

      /// Tycons indexed by the various names that may be used to access them, e.g. 
      ///     "List" --> multiple TyconRef's for the various tycons accessible by this name. 
      ///     "List`1" --> TyconRef 
      eTyconsByAccessNames: LayeredMultiMap<string,TyconRef>

      eFullyQualifiedTyconsByAccessNames: LayeredMultiMap<string,TyconRef>

      /// Tycons available by unqualified, demangled names (i.e. (List,1) --> TyconRef) 
      eTyconsByDemangledNameAndArity: LayeredMap<NameArityPair,TyconRef>

      /// Tycons available by unqualified, demangled names (i.e. (List,1) --> TyconRef) 
      eFullyQualifiedTyconsByDemangledNameAndArity: LayeredMap<NameArityPair,TyconRef>

      /// Extension members by type and name 
      eExtensionMembers: TyconRefMultiMap<ExtensionMember>

      /// Typars (always available by unqualified names). Further typars can be 
      /// in the tpenv, a structure folded through each top-level definition. 
      eTypars: NameMap<Typar>

    } 

    static member Empty(g) =
        { eDisplayEnv=DisplayEnv.Empty g
          eModulesAndNamespaces=Map.empty
          eFullyQualifiedModulesAndNamespaces = Map.empty
          eFieldLabels=Map.empty
          eUnqualifiedItems=LayeredMap.Empty
          ePatItems=Map.empty
          eTyconsByAccessNames= LayeredMultiMap.Empty
          eTyconsByDemangledNameAndArity=LayeredMap.Empty
          eFullyQualifiedTyconsByAccessNames=LayeredMultiMap.Empty
          eFullyQualifiedTyconsByDemangledNameAndArity=LayeredMap.Empty
          eExtensionMembers=TyconRefMultiMap<_>.Empty
          eTypars=Map.empty }

    member nenv.DisplayEnv = nenv.eDisplayEnv

    member nenv.FindUnqualifiedItem nm = nenv.eUnqualifiedItems.[nm]

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

// Note: incrementing sequence of integers during type checking
let nextExtensionMethodPriority() = uint64 (newStamp())


//-------------------------------------------------------------------------
// Item functions
//------------------------------------------------------------------------- 

let DisplayNameOfItem g d = 
    match d with
    | Item.Value v -> v.DisplayName
    | Item.ActivePatternCase apref -> apref.Name
    | Item.UnionCase uinfo -> DecompileOpName uinfo.UnionCase.DisplayName
    | Item.ExnCase tcref -> tcref.LogicalName
    | Item.RecdField rfinfo -> DecompileOpName rfinfo.RecdField.Name
    | Item.NewDef id -> id.idText
    | Item.ILField finfo -> finfo.FieldName
    | Item.Event einfo -> einfo.EventName
    | Item.Property(nm,_) -> nm
    | Item.MethodGroup(nm,_) -> nm
    | Item.CtorGroup(nm,_) -> DemangleGenericTypeName nm
    | Item.FakeInterfaceCtor typ 
    | Item.DelegateCtor typ -> DemangleGenericTypeName (tcrefOfAppTy g typ).LogicalName
    | Item.Types(nm,_) -> DemangleGenericTypeName nm
    | Item.TypeVar nm -> nm
    | Item.ModuleOrNamespaces(modref :: _) ->  modref.DemangledModuleOrNamespaceName
    | Item.ArgName (id,_)  -> id.idText
    | Item.SetterArg (id, _) -> id.idText
    | Item.CustomOperation (customOpName,_,_) -> customOpName
    | Item.CustomBuilder (nm,_) -> nm
    | _ ->  ""

/// For the operations that build the overall name resolution 
/// tables, BulkAdd.Yes is set to true when "opening" a 
/// namespace. If BulkAdd is true then add-and-collapse 
/// is used for the backing maps.Multiple "open" operations are 
/// thus coalesced, and the first subsequent lookup after a sequence 
/// of opens will collapse the maps and build the backing dictionary. 
[<RequireQualifiedAccess>]
type BulkAdd = Yes | No


/// bulkAddMode: true when adding the values from the 'open' of a namespace
/// or module, when we collapse the value table down to a dictionary.
let AddValRefsToItems (bulkAddMode: BulkAdd) (eUnqualifiedItems: LayeredMap<_,_>) (vrefs:ValRef[]) =
    // Object model members are not added to the unqualified name resolution environment 
    let vrefs = vrefs |> Array.filter (fun vref -> vref.MemberInfo.IsNone)

    if vrefs.Length = 0 then eUnqualifiedItems else

    match bulkAddMode with 
    | BulkAdd.Yes -> 
        eUnqualifiedItems.AddAndMarkAsCollapsible(vrefs |> Array.map (fun vref -> KeyValuePair(vref.LogicalName, Item.Value vref)))
    | BulkAdd.No -> 
        assert (vrefs.Length = 1)
        let vref = vrefs.[0]
        eUnqualifiedItems.Add (vref.LogicalName, Item.Value vref)  

let AddValRefToExtensionMembers pri (eExtensionMembers: TyconRefMultiMap<_>)  (vref:ValRef) =
    if vref.IsMember && vref.IsExtensionMember then
        eExtensionMembers.Add (vref.MemberApparentParent, FSExtMem (vref,pri)) 
    else
        eExtensionMembers


/// This entrypoint is used to add some extra items to the environment for Visual Studio, e.g. static members 
let AddFakeNamedValRefToNameEnv nm nenv vref =
    {nenv with eUnqualifiedItems= nenv.eUnqualifiedItems.Add (nm, Item.Value vref) }

/// This entrypoint is used to add some extra items to the environment for Visual Studio, e.g. record members
let AddFakeNameToNameEnv nm nenv item =
    {nenv with eUnqualifiedItems= nenv.eUnqualifiedItems.Add (nm, item) }

let AddValRefsToNameEnvWithPriority bulkAddMode pri nenv vrefs =
    {nenv with eUnqualifiedItems= AddValRefsToItems bulkAddMode nenv.eUnqualifiedItems vrefs;
               eExtensionMembers = (nenv.eExtensionMembers,vrefs) ||> Array.fold (AddValRefToExtensionMembers pri);
               ePatItems = 
                   (nenv.ePatItems,vrefs) ||> Array.fold (fun acc vref ->
                       let ePatItems = 
                         (ActivePatternElemsOfValRef vref, acc) ||> List.foldBack (fun apref tab -> 
                             NameMap.add apref.Name (Item.ActivePatternCase apref) tab)

                       // Add literal constants to the environment available for resolving items in patterns 
                       let ePatItems = 
                           match vref.LiteralValue with 
                           | None -> ePatItems 
                           | Some _ -> NameMap.add vref.LogicalName (Item.Value vref) ePatItems

                       ePatItems) }

let AddValRefToNameEnv nenv vref = 
    AddValRefsToNameEnvWithPriority BulkAdd.No (nextExtensionMethodPriority()) nenv [| vref |]

let AddActivePatternResultTagsToNameEnv (apinfo: PrettyNaming.ActivePatternInfo) nenv ty m =
    let nms = apinfo.Names
    let apresl = nms |> List.mapi (fun j nm -> nm, j)
    { nenv with  eUnqualifiedItems= (apresl,nenv.eUnqualifiedItems) ||> List.foldBack (fun (nm,j) acc -> acc.Add(nm, Item.ActivePatternResult(apinfo,ty,j, m))); } 

let GeneralizeUnionCaseRef (ucref:UnionCaseRef) = 
    UnionCaseInfo (fst (generalizeTyconRef ucref.TyconRef), ucref)
    
    
let AddTyconsByDemangledNameAndArity (bulkAddMode: BulkAdd) (tcrefs: TyconRef[]) (tab: LayeredMap<NameArityPair,TyconRef>) = 
    let entries = tcrefs |> Array.map (fun tcref -> KeyTyconByDemangledNameAndArity tcref.LogicalName tcref.TyparsNoRange tcref)
    match bulkAddMode with
    | BulkAdd.Yes -> tab.AddAndMarkAsCollapsible entries
    | BulkAdd.No -> (tab,entries) ||> Array.fold (fun tab (KeyValue(k,v)) -> tab.Add(k,v))

let AddTyconByAccessNames bulkAddMode (tcrefs:TyconRef[]) (tab: LayeredMultiMap<string,_>) = 
    let entries = tcrefs |> Array.collect (fun tcref -> KeyTyconByAccessNames tcref.LogicalName tcref)
    match bulkAddMode with
    | BulkAdd.Yes -> tab.AddAndMarkAsCollapsible entries
    | BulkAdd.No -> (tab,entries) ||> Array.fold (fun tab (KeyValue(k,v)) -> tab.Add (k,v))

// Get the info for all the .NET-style extension members listed as static members in the type
let private CSharpExtensionMemberInfosForTyconRef amap m  (tcref:TyconRef) = 
    let scoref,enc,tdef = tcref.ILTyconInfo
    if ILThingHasExtensionAttribute tdef.CustomAttrs then 
        let pri = nextExtensionMethodPriority()
        let tref = ILTypeInfo(tcref,mkRefForNestedILTypeDef scoref (enc,tdef),[],tdef)
                    
        // found extension attribute on type 'tcref.LogicalName'
                    
        tdef.Methods.AsList |> List.collect (fun md ->
              if ILThingHasExtensionAttribute md.CustomAttrs && md.Parameters.Length > 0 then 
                let thisParam =  ILList.nth md.Parameters 0
                let ilty = thisParam.Type
                match ilty with 
                | ILType.Boxed tspec 
                | ILType.Value tspec -> 
                    let tcref = (tspec |> rescopeILTypeSpec scoref).TypeRef |> Import.ImportILTypeRef amap m
                    // found extension method 'md.Name' on type 'tcref.LogicalName'
                                    
                    [(tcref, ILExtMem (tref.ILTypeRef, md, pri))]
                // Do not import extension members whose 'this' is only a type parameter
                | _ ->
                    []
              else
                  [])
    else
        []
       
let private AddPartsOfTyconRefToNameEnv bulkAddMode ownDefinition (g:TcGlobals) amap m  nenv (tcref:TyconRef) = 
    let AddRecdField (rfref:RecdFieldRef) tab = NameMultiMap.add rfref.FieldName rfref tab

    let AddUnionCases1 (tab:Map<_,_>) (ucrefs:UnionCaseRef list)= 
        (tab, ucrefs) ||> List.fold (fun acc ucref -> 
                let item = Item.UnionCase (GeneralizeUnionCaseRef ucref)
                acc.Add (ucref.CaseName, item))

    let AddUnionCases2 (eUnqualifiedItems: LayeredMap<_,_>) (ucrefs :UnionCaseRef list) = 
        match bulkAddMode with 
        | BulkAdd.Yes -> 
            let items = 
                ucrefs |> Array.ofList |> Array.map (fun ucref -> 
                    let item = Item.UnionCase (GeneralizeUnionCaseRef ucref)
                    KeyValuePair(ucref.CaseName,item))
            eUnqualifiedItems.AddAndMarkAsCollapsible items

        | BulkAdd.No -> 
            (eUnqualifiedItems,ucrefs) ||> List.fold (fun acc ucref -> 
               let item = Item.UnionCase (GeneralizeUnionCaseRef ucref)
               acc.Add (ucref.CaseName, item))


    let isIL = tcref.IsILTycon
    let ucrefs = if isIL then [] else tcref.UnionCasesAsList |> List.map (mkNestedUnionCaseRef tcref) 
    let flds =  if isIL then [| |] else tcref.AllFieldsArray

    let eExtensionMembers = 
        if isIL then 
            let csharpExtensionMeths = CSharpExtensionMemberInfosForTyconRef amap m  tcref 
            (nenv.eExtensionMembers,csharpExtensionMeths) ||> List.fold (fun tab (tcref,extMemInfo) -> tab.Add (tcref, extMemInfo))  
        else 
            nenv.eExtensionMembers
    
    { nenv with 
        eFieldLabels= 
            (if not tcref.IsRecordTycon || isIL || flds.Length = 0 || (not ownDefinition && HasFSharpAttribute g g.attrib_RequireQualifiedAccessAttribute tcref.Attribs) then 
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
                     tab.LinearTryModifyThenLaterFlatten (tcref.DisplayName, (fun prev ->
                         match prev with 
                         | Some (Item.UnqualifiedType tcrefs) ->  Item.UnqualifiedType (tcref::tcrefs)
                         | _ ->  Item.UnqualifiedType [tcref]))

                     //match nenv.eUnqualifiedItems.TryFind tcref.DisplayName with 
                     //| Some (Item.UnqualifiedType tcrefs) ->  tab.Add(tcref.DisplayName, Item.UnqualifiedType (tcref::tcrefs))
                     //| _ ->  tab.Add(tcref.DisplayName, Item.UnqualifiedType [tcref])
                 else
                     tab
             if isIL || ucrefs.Length = 0  || (not ownDefinition && HasFSharpAttribute g g.attrib_RequireQualifiedAccessAttribute tcref.Attribs) then 
                 tab 
             else 
                 AddUnionCases2 tab ucrefs);
        ePatItems = 
            (if isIL || ucrefs.Length = 0  || (not ownDefinition && HasFSharpAttribute g g.attrib_RequireQualifiedAccessAttribute tcref.Attribs) then 
                nenv.ePatItems 
             else 
                AddUnionCases1 nenv.ePatItems ucrefs);
        eExtensionMembers = 
            eExtensionMembers; }
    
let AddTyconRefsToNameEnv bulkAddMode ownDefinition g amap m  root nenv tcrefs = 
    let env = List.fold (AddPartsOfTyconRefToNameEnv bulkAddMode ownDefinition g amap m) nenv tcrefs
    // Add most of the contents of the tycons en-masse, then flatten the tables if we're opening a module or namespace
    let tcrefs = Array.ofList tcrefs
    { env with
        eFullyQualifiedTyconsByDemangledNameAndArity= 
            (if root  then AddTyconsByDemangledNameAndArity bulkAddMode tcrefs nenv.eFullyQualifiedTyconsByDemangledNameAndArity else nenv.eFullyQualifiedTyconsByDemangledNameAndArity); 
        eFullyQualifiedTyconsByAccessNames= 
            (if root then AddTyconByAccessNames bulkAddMode tcrefs nenv.eFullyQualifiedTyconsByAccessNames else nenv.eFullyQualifiedTyconsByAccessNames);
        eTyconsByDemangledNameAndArity= 
            AddTyconsByDemangledNameAndArity bulkAddMode tcrefs nenv.eTyconsByDemangledNameAndArity; 
        eTyconsByAccessNames= 
            AddTyconByAccessNames bulkAddMode tcrefs nenv.eTyconsByAccessNames } 

let AddExceptionDeclsToNameEnv bulkAddMode nenv (ecref:TyconRef) = 
    assert ecref.IsExceptionDecl
    let item = Item.ExnCase ecref
    {nenv with 
       eUnqualifiedItems=
            match bulkAddMode with 
            | BulkAdd.Yes -> 
                nenv.eUnqualifiedItems.AddAndMarkAsCollapsible [| KeyValuePair(ecref.LogicalName, item) |]
            | BulkAdd.No -> 
                nenv.eUnqualifiedItems.Add (ecref.LogicalName, item)
                
       ePatItems = nenv.ePatItems.Add (ecref.LogicalName, item) }

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
            if modref.IsModule && TryFindFSharpBoolAttribute g g.attrib_AutoOpenAttribute modref.Attribs = Some true then
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
     let nenv = (nenv,exrefs) ||> List.fold (AddExceptionDeclsToNameEnv BulkAdd.Yes)
     let nenv = (nenv,tcrefs) ||> AddTyconRefsToNameEnv BulkAdd.Yes false g amap m false 
     let vrefs = 
         mty.AllValsAndMembers.ToFlatList() 
         |> FlatList.choose (fun x -> 
             if IsAccessible ad x.Accessibility then tryMkValRefInModRef modref x 
             else None)
         |> FlatList.toArray
     let nenv = AddValRefsToNameEnvWithPriority BulkAdd.Yes pri nenv vrefs
     let nenv = (nenv,nestedModuleRefs modref) ||> AddModuleOrNamespaceRefsToNameEnv g amap m false ad 
     nenv

// Note this is a 'foldBack' - the most recently added modules come first in the list, e.g.
//    module M1 = ... // M1a
//    module M1 = ... // M1b
//    open M1
// 
// The list contains [M1b; M1a]
and AddModulesAndNamespacesContentsToNameEnv g amap ad m nenv modrefs = 
   (modrefs, nenv) ||> List.foldBack (fun modref acc -> AddModuleOrNamespaceContentsToNameEnv g amap ad m acc modref)


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

/// Convert a reference to a named type into a type that includes
/// a fresh set of inference type variables for the type parameters of the union type.
let FreshenTycon (ncenv: NameResolver) m (tcref:TyconRef) = 
    let tinst = ncenv.InstantiationGenerator m (tcref.Typars m)
    TType_app(tcref,tinst)

/// Convert a reference to a union case into a UnionCaseInfo that includes
/// a fresh set of inference type variables for the type parameters of the union type.
let FreshenUnionCaseRef (ncenv: NameResolver) m (ucref:UnionCaseRef) = 
    let tinst = ncenv.InstantiationGenerator m (ucref.TyconRef.Typars m)
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

// REVIEW: make this tail recursive
let rec CollectResults f = function
    | [] -> NoResultsOrUsefulErrors
    | [h] -> OneResult (f h)
    | h :: t -> AddResults (OneResult (f h)) (CollectResults f t)

// REVIEW: make this tail recursive
let MapResults f = function
    | Result xs -> Result (List.map f xs)
    | Exception err -> Exception err

let AtMostOneResult m res = 
    match res with 
    | Exception err -> raze err
    | Result [] -> raze (Error(FSComp.SR.nrInvalidModuleExprType(),m))
    | Result [res] -> success res
    | Result (res :: _) -> success res 

//-------------------------------------------------------------------------
// TypeNameResolutionInfo
//------------------------------------------------------------------------- 

type TypeNameResolutionFlag = 
     | ResolveTypeNamesToCtors 
     | ResolveTypeNamesToTypeRefs

[<RequireQualifiedAccess>]
[<NoEquality; NoComparison>]
type TypeNameResolutionStaticArgsInfo = 
    /// Indicates indefinite knowledge of type arguments
    | Indefinite 
    /// Indicates definite knowledge of type arguments
    | Definite of int
    /// Indicates definite knowledge of empty type arguments
    static member DefiniteEmpty = TypeNameResolutionStaticArgsInfo.Definite 0
    static member FromTyArgs (tyargs: SynType list) = TypeNameResolutionStaticArgsInfo.Definite tyargs.Length
    member x.HasNoStaticArgsInfo = match x with TypeNameResolutionStaticArgsInfo.Indefinite -> true | _-> false
    member x.NumStaticArgs = match x with TypeNameResolutionStaticArgsInfo.Indefinite -> 0 | TypeNameResolutionStaticArgsInfo.Definite n -> n

    // Get the first possible mangled name of the type, assuming the args are generic args
    member x.MangledNameForType nm = 
        if IsMangledGenericName nm || x.NumStaticArgs = 0 then nm
        else nm+"`"+string x.NumStaticArgs



[<NoEquality; NoComparison>]
type TypeNameResolutionInfo = 
    | TypeNameResolutionInfo of TypeNameResolutionFlag * TypeNameResolutionStaticArgsInfo

    static member Default = TypeNameResolutionInfo (ResolveTypeNamesToCtors,TypeNameResolutionStaticArgsInfo.Indefinite) 
    static member ResolveToTypeRefs statResInfo = TypeNameResolutionInfo (ResolveTypeNamesToTypeRefs,statResInfo) 
    member x.StaticArgsInfo = match x with TypeNameResolutionInfo(_,staticResInfo) -> staticResInfo 
    member x.ResolutionFlag = match x with TypeNameResolutionInfo(flag,_) -> flag
    member x.DropStaticArgsInfo = match x with TypeNameResolutionInfo(flag2,_) -> TypeNameResolutionInfo(flag2,TypeNameResolutionStaticArgsInfo.Indefinite)


//-------------------------------------------------------------------------
// Resolve (possibly mangled) type names 
//------------------------------------------------------------------------- 
 
/// Qualified lookups where the number of generic arguments is known 
/// from context, e.g. Module.Type<args>.  The full names suh as ``List`1`` can 
/// be used to qualify access if needed 
let LookupTypeNameInEntityHaveArity nm (staticResInfo: TypeNameResolutionStaticArgsInfo) (mty:ModuleOrNamespaceType) = 
    let attempt1 = mty.TypesByMangledName.TryFind (staticResInfo.MangledNameForType nm)
    match attempt1 with 
    | Some _ as r ->  r
    | None -> mty.TypesByMangledName.TryFind nm

/// Unqualified lookups where the number of generic arguments is known 
/// from context, e.g. List<arg>.  Rebindings due to 'open' may have rebound identifiers.
let LookupTypeNameInEnvHaveArity fq nm numTyArgs (nenv:NameResolutionEnv) = 
    let key = if IsMangledGenericName nm then DecodeGenericTypeName nm else NameArityPair(nm,numTyArgs)
    match nenv.TyconsByDemangledNameAndArity(fq).TryFind(key)  with
    | Some res -> Some res
    | None -> nenv.TyconsByAccessNames(fq).TryFind nm |> Option.map List.head

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

let LookupTypeNameNoArity nm (byDemangledNameAndArity: LayeredMap<NameArityPair,_>) (byAccessNames: LayeredMultiMap<string,_>) = 
    if IsMangledGenericName nm then 
      match byDemangledNameAndArity.TryFind (DecodeGenericTypeName nm) with 
      | Some res -> [res]
      | None -> 
          match byAccessNames.TryFind nm with
          | Some res -> res
          | None -> []
    else 
      byAccessNames.[nm]

let LookupTypeNameInEnvNoArity fq nm (nenv: NameResolutionEnv) = 
    LookupTypeNameNoArity nm (nenv.TyconsByDemangledNameAndArity(fq)) (nenv.TyconsByAccessNames(fq))

let LookupTypeNameInEntityNoArity m nm (mtyp:ModuleOrNamespaceType) = 
    LookupTypeNameNoArity nm (mtyp.TypesByDemangledNameAndArity(m)) mtyp.TypesByAccessNames 

let LookupTypeNameInEnvMaybeHaveArity fq nm (typeNameResInfo: TypeNameResolutionInfo) nenv = 
    if typeNameResInfo.StaticArgsInfo.HasNoStaticArgsInfo then 
        LookupTypeNameInEnvNoArity fq nm nenv
    else 
        LookupTypeNameInEnvHaveArity fq nm typeNameResInfo.StaticArgsInfo.NumStaticArgs nenv |> Option.toList


[<RequireQualifiedAccess>]
type PermitDirectReferenceToGeneratedType = 
    | Yes 
    | No
    

#if EXTENSIONTYPING


/// Generated types may not be returned from name resolution. 
let CheckForDirectReferenceToGeneratedType (tcref: TyconRef, genOk, m) =
  match genOk with 
  | PermitDirectReferenceToGeneratedType.Yes -> ()
  | PermitDirectReferenceToGeneratedType.No -> 
    match tcref.TypeReprInfo with 
    | TProvidedTypeExtensionPoint info when not info.IsErased -> 
         //printfn "checking direct reference to generated type '%s'" tcref.DisplayName
        if ExtensionTyping.IsGeneratedTypeDirectReference (info.ProvidedType, m) then 
            error (Error(FSComp.SR.etDirectReferenceToGeneratedTypeNotAllowed(tcref.DisplayName),m))
    |  _ -> ()


/// This adds a new entity for a lazily discovered provided type into the TAST structure.
let AddEntityForProvidedType (amap: Import.ImportMap, modref: ModuleOrNamespaceRef, resolutionEnvironment, st:Tainted<ProvidedType>, m) = 
    let importProvidedType t = Import.ImportProvidedType amap m t
    let isSuppressRelocate = amap.g.isInteractive || st.PUntaint((fun st -> st.IsSuppressRelocate),m) 
    let tycon = Construct.NewProvidedTycon(resolutionEnvironment, st, importProvidedType, isSuppressRelocate, m)
    modref.ModuleOrNamespaceType.AddProvidedTypeEntity(tycon)
    let tcref = modref.MkNestedTyconRef tycon
    System.Diagnostics.Debug.Assert modref.TryDeref.IsSome
    tcref


let ResolveProvidedTypeNameInEntity (amap, m, typeName, staticResInfo: TypeNameResolutionStaticArgsInfo, modref: ModuleOrNamespaceRef) = 
    match modref.TypeReprInfo with
    | TProvidedNamespaceExtensionPoint(resolutionEnvironment,resolvers) ->
        match modref.Deref.PublicPath with
        | Some(PubPath(path)) ->
            let matches = resolvers |> List.map (fun r->ExtensionTyping.TryResolveProvidedType(resolutionEnvironment,r,m,path,typeName)) 
            let tcrefs = 
                [ for st in matches do 
                      match st with 
                      | None -> ()
                      | Some st -> 
                          yield AddEntityForProvidedType (amap, modref, resolutionEnvironment, st, m) ]
            tcrefs
        | None -> []

    // We have a provided type, look up its nested types (populating them on-demand if necessary)
    | TProvidedTypeExtensionPoint info ->
        let sty = info.ProvidedType
        let resolutionEnvironment = info.ResolutionEnvironment
        if staticResInfo.NumStaticArgs > 0 then 
            error(Error(FSComp.SR.etNestedProvidedTypesDoNotTakeStaticArgumentsOrGenericParameters(),m))
            
        if resolutionEnvironment.showResolutionMessages then
            dprintfn "resolving name '%s' in SingleTypeExtensionPoint '%s'" typeName (sty.PUntaint((fun sty -> sty.FullName), m))

        match sty.PApply((fun sty -> sty.GetNestedType(typeName)), m) with
        | Tainted.Null -> []
        | nestedSty -> 
            [AddEntityForProvidedType (amap, modref, resolutionEnvironment, nestedSty, m) ]
    | _ -> []
#endif

let LookupTypeNameInEntityMaybeHaveArity (amap, m, nm, staticResInfo:TypeNameResolutionStaticArgsInfo, modref: ModuleOrNamespaceRef) = 
    let mtyp = modref.ModuleOrNamespaceType    
    let tcrefs = 
        match staticResInfo with 
        | TypeNameResolutionStaticArgsInfo.Indefinite -> 
            match LookupTypeNameInEntityNoArity m nm mtyp with
            | [] -> []
            | tycons -> tycons |> List.map modref.MkNestedTyconRef 
        | TypeNameResolutionStaticArgsInfo.Definite _ -> 
            match LookupTypeNameInEntityHaveArity nm staticResInfo mtyp with
            | Some tycon -> [modref.MkNestedTyconRef tycon] 
            | None -> []
#if EXTENSIONTYPING
    let tcrefs =
        match tcrefs with 
        | [] -> ResolveProvidedTypeNameInEntity (amap, m, nm, staticResInfo, modref)
        | _ -> tcrefs
#else
    amap |> ignore
#endif
    //let tcrefs = tcrefs |> List.filter (IsEntityAccessible ad)
    tcrefs


let GetNestedTypesOfType (ad, ncenv:NameResolver, optFilter, staticResInfo, checkForGenerated, m) typ =
    let g = ncenv.g
    ncenv.InfoReader.ReadPrimaryTypeHierachy(AllowMultiIntfInstantiations.No,m,typ) |> List.collect (fun typ -> 
        if isAppTy g typ then 
            let tcref,tinst = destAppTy g typ
            let tycon = tcref.Deref
            let mty = tycon.ModuleOrNamespaceType
            // No dotting through type generators to get to a nested type!
#if EXTENSIONTYPING
            if checkForGenerated then 
                CheckForDirectReferenceToGeneratedType (tcref, PermitDirectReferenceToGeneratedType.No, m)
#else
            checkForGenerated |> ignore
#endif
            let MakeNestedType (tcrefNested:TyconRef) = 
                // Handle the .NET/C# business where nested generic types implictly accumulate the type parameters 
                // from their enclosing types.
                let tps = List.drop tinst.Length (tcrefNested.Typars m)
                let tinstNested = ncenv.InstantiationGenerator m tps
                mkAppTy tcrefNested (tinst @ tinstNested)

            match optFilter with 
            | Some nm -> 
                LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, m, nm, staticResInfo, tcref)
                    |> List.map MakeNestedType 
            | None -> 
#if EXTENSIONTYPING
                match tycon.TypeReprInfo with 
                | TProvidedTypeExtensionPoint info ->
                    [ for nestedType in info.ProvidedType.PApplyArray((fun sty -> sty.GetNestedTypes()), "GetNestedTypes", m) do 
                        let nestedTypeName = nestedType.PUntaint((fun t -> t.Name), m)
                        for nestedTcref in LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, m, nestedTypeName, staticResInfo, tcref)  do
                             yield  MakeNestedType nestedTcref ]
                
                | _ -> 
#endif
                    mty.TypesByAccessNames.Values 
                        |> Seq.toList
                        |> List.map (tcref.MkNestedTyconRef >> MakeNestedType)
                        |> List.filter (IsTypeAccessible g ad)
        else [])

//-------------------------------------------------------------------------
// Report environments to visual studio. We stuff intermediary results 
// into a global variable. A little unpleasant. 
// REVIEW: We could at least put the global in cenv!!!
//------------------------------------------------------------------------- 

// Represents a type of the occurence when reporting name in name resolution
[<RequireQualifiedAccess>]
type ItemOccurence = 
    // This is a binding / declaration of the item
    | Binding = 0
    // This is a usage of the item 
    | Use = 1
    // This is a usage of a type name in a type
    | UseInType = 2
    // This is a usage of a type name in an attribute
    | UseInAttribute = 3
    // Inside pattern matching
    | Pattern = 4
  
type ITypecheckResultsSink =
    abstract NotifyEnvWithScope : range * NameResolutionEnv * AccessorDomain -> unit
    abstract NotifyExprHasType : pos * TType * Tastops.DisplayEnv * NameResolutionEnv * AccessorDomain * range -> unit
    abstract NotifyNameResolution : pos * Item * Item * ItemOccurence * Tastops.DisplayEnv * NameResolutionEnv * AccessorDomain * range -> unit

type TcResultsSink = 
    { mutable CurrentSink : ITypecheckResultsSink option }
    static member NoSink =  { CurrentSink = None }
    static member WithSink sink = { CurrentSink = Some sink }

let WithNewTypecheckResultsSink (newSink : ITypecheckResultsSink, sink:TcResultsSink) = 
    let old = sink.CurrentSink
    sink.CurrentSink <- Some newSink
    { new System.IDisposable with member x.Dispose() = sink.CurrentSink <- old }

let TemporarilySuspendReportingTypecheckResultsToSink (sink:TcResultsSink) = 
    let old = sink.CurrentSink
    sink.CurrentSink <- None
    { new System.IDisposable with member x.Dispose() = sink.CurrentSink <- old }


let CallEnvSink (sink:TcResultsSink) (scopem,nenv,ad) = 
    match sink.CurrentSink with 
    | None -> () 
    | Some sink -> sink.NotifyEnvWithScope(scopem,nenv,ad)

let CallNameResolutionSink (sink:TcResultsSink) (m:range,nenv,item,itemMethodGroup,occurenceType,denv,ad) = 
    match sink.CurrentSink with 
    | None -> () 
    | Some sink -> sink.NotifyNameResolution(m.End,item,itemMethodGroup,occurenceType,denv,nenv,ad,m)  

let CallExprHasTypeSink (sink:TcResultsSink) (m:range,nenv,typ,denv,ad) = 
    match sink.CurrentSink with 
    | None -> () 
    | Some sink -> sink.NotifyExprHasType(m.End,typ,denv,nenv,ad,m)

/// Checks if the type variables associated with the result of a resolution are inferrable,
/// i.e. occur in the arguments or return type of the resolution. If not give a warning
/// about a type instantiation being needed.
type ResultTyparChecker = unit -> bool

let CheckAllTyparsInferrable amap m item = 
    match item with
    | Item.Property(_,pinfos) -> 
        pinfos |> List.forall (fun pinfo -> 
            let freeInEnclosingType = freeInType CollectTyparsNoCaching pinfo.EnclosingType
            let freeInArgsAndRetType = 
                accFreeInTypes CollectTyparsNoCaching (pinfo.GetParamTypes(amap,m)) 
                       (freeInType CollectTyparsNoCaching (pinfo.GetPropertyType(amap,m)))
            let free = Zset.diff freeInEnclosingType.FreeTypars  freeInArgsAndRetType.FreeTypars
            free.IsEmpty)

    | Item.MethodGroup(_,minfos) -> 
        minfos |> List.forall (fun minfo -> 
            let fminst = minfo.FormalMethodInst
            let freeInEnclosingType = freeInType CollectTyparsNoCaching minfo.EnclosingType
            let freeInArgsAndRetType = 
                List.foldBack (accFreeInTypes CollectTyparsNoCaching) (minfo.GetParamTypes(amap, m, fminst)) 
                   (accFreeInTypes CollectTyparsNoCaching (minfo.GetObjArgTypes(amap, m, fminst)) 
                       (freeInType CollectTyparsNoCaching (minfo.GetFSharpReturnTy(amap, m, fminst))))
            let free = Zset.diff freeInEnclosingType.FreeTypars  freeInArgsAndRetType.FreeTypars
            free.IsEmpty)

    | Item.CtorGroup _ 
    | Item.FakeInterfaceCtor _ 
    | Item.DelegateCtor _ 
    | Item.Types _ 
    | Item.ModuleOrNamespaces _
    | Item.CustomOperation _ 
    | Item.CustomBuilder _ 
    | Item.TypeVar _ 
    | Item.ArgName _ 
    | Item.ActivePatternResult _
    | Item.Value _ 
    | Item.ActivePatternCase _ 
    | Item.UnionCase _ 
    | Item.ExnCase _ 
    | Item.RecdField _ 
    | Item.NewDef _ 
    | Item.ILField _ 
    | Item.Event _ 
    | Item.ImplicitOp _ 
    | Item.UnqualifiedType _
    | Item.SetterArg _ -> true
    
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

    static member SendToSink(sink, ncenv: NameResolver, nenv, occ, ad, ResolutionInfo(entityPath,warnings), typarChecker) = 
        entityPath |> List.iter (fun (m,eref:EntityRef) -> 
            CheckEntityAttributes ncenv.g eref m |> CommitOperationResult;        
            CheckTyconAccessible m ad eref |> ignore;
            let item = 
                if eref.IsModuleOrNamespace then 
                    Item.ModuleOrNamespaces [eref] 
                else 
                    Item.Types(eref.DisplayName,[FreshenTycon ncenv m eref])
            CallNameResolutionSink sink (m,nenv,item,item,occ,nenv.eDisplayEnv,ad))
        warnings(typarChecker)
 
    static member Empty = 
        ResolutionInfo([],(fun _ -> ()))

    member x.AddEntity info = 
        let (ResolutionInfo(entityPath,warnings)) = x
        ResolutionInfo(info::entityPath,warnings)

    member x.AddWarning f = 
        let (ResolutionInfo(entityPath,warnings)) = x
        ResolutionInfo(entityPath,(fun typarChecker -> f typarChecker; warnings typarChecker))



/// Resolve ambiguities between types overloaded by generic arity, based on number of type arguments.
/// Also check that we're not returning direct references to generated provided types.
//
// Given ambiguous C<>, C<_>    we resolve the ambiguous 'C.M' to C<> without warning
// Given ambiguous C<_>, C<_,_> we resolve the ambiguous 'C.M' to C<_> with an ambiguity error
// Given C<_>                   we resolve the ambiguous 'C.M' to C<_> with a warning if the argument or return types can't be inferred

// Given ambiguous C<>, C<_>    we resolve the ambiguous 'C()' to C<> without warning
// Given ambiguous C<_>, C<_,_> we resolve the ambiguous 'C()' to C<_> with an ambiguity error
// Given C<_>                   we resolve the ambiguous 'C()' to C<_> with a warning if the argument or return types can't be inferred

let CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs:(ResolutionInfo * TyconRef) list, 
                                                             typeNameResInfo:TypeNameResolutionInfo, 
                                                             genOk:PermitDirectReferenceToGeneratedType, 
                                                             m) = 

    let tcrefs = 
        tcrefs 
        // remove later duplicates (if we've opened the same module more than once)
        |> Seq.distinctBy (fun (_,tcref) -> tcref.Stamp) 
        |> Seq.toList                     
        // List.sortBy is a STABLE sort (the order matters!)
        |> List.sortBy (fun (_,tcref) -> tcref.Typars(m).Length)

    let tcrefs = 
        match tcrefs with 
        | ((_resInfo,tcref) :: _) when 
                // multiple types
                tcrefs.Length > 1 && 
                // no explicit type instantiation
                typeNameResInfo.StaticArgsInfo.HasNoStaticArgsInfo && 
                // some type arguments required on all types (note sorted by typar count above)
                tcref.Typars(m).Length > 0 && 
                // plausible types have different arities
                (tcrefs |> Seq.distinctBy (fun (_,tcref) -> tcref.Typars(m).Length) |> Seq.length > 1)  ->
            [ for (resInfo,tcref) in tcrefs do 
                let resInfo = resInfo.AddWarning (fun _typarChecker -> errorR(Error(FSComp.SR.nrTypeInstantiationNeededToDisambiguateTypesWithSameName(tcref.DisplayName, tcref.DisplayNameWithStaticParametersAndUnderscoreTypars),m)))
                yield (resInfo,tcref) ]

        | [(resInfo,tcref)] when  typeNameResInfo.StaticArgsInfo.HasNoStaticArgsInfo && tcref.Typars(m).Length > 0 && typeNameResInfo.ResolutionFlag = ResolveTypeNamesToTypeRefs ->
            let resInfo = 
                resInfo.AddWarning (fun typarChecker -> 
                    if not (typarChecker()) then 
                        warning(Error(FSComp.SR.nrTypeInstantiationIsMissingAndCouldNotBeInferred(tcref.DisplayName, tcref.DisplayNameWithStaticParametersAndUnderscoreTypars),m)))
            [(resInfo,tcref)]

        | _ -> 
            tcrefs

#if EXTENSIONTYPING
    for (_,tcref) in tcrefs do 
        // Type generators can't be returned by name resolution, unless PermitDirectReferenceToGeneratedType.Yes
        CheckForDirectReferenceToGeneratedType (tcref, genOk, m)
#else
    genOk |> ignore
#endif

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
        success (resInfo,Item.DelegateCtor typ)
    else 
        let ctorInfos =  GetIntrinsicConstructorInfosOfType ncenv.InfoReader m typ
        if isInterfaceTy g typ && isNil ctorInfos then 
            success (resInfo, Item.FakeInterfaceCtor typ)
        else 
            let defaultStructCtorInfo = 
                if (isStructTy g typ && not(ctorInfos |> List.exists (fun x -> x.IsNullary))) then 
                    [DefaultStructCtor(g,typ)] 
                else []
            if (isNil defaultStructCtorInfo && isNil ctorInfos) || not (isAppTy g typ) then 
                raze (Error(FSComp.SR.nrNoConstructorsAvailableForType(NicePrint.minimalStringOfType edenv typ),m))
            else 
                let ctorInfos = ctorInfos |> List.filter (IsMethInfoAccessible amap m ad)  
                success (resInfo,MakeCtorGroup ((tcrefOfAppTy g typ).LogicalName, (defaultStructCtorInfo@ctorInfos))) 

let ResolveObjectConstructor (ncenv:NameResolver) edenv m ad typ = 
    ResolveObjectConstructorPrim (ncenv:NameResolver) edenv [] m ad typ  |?> (fun (_resInfo,item) -> item)

//-------------------------------------------------------------------------
// Bind IL "." notation (member lookup or lookup in a type)
//------------------------------------------------------------------------- 

let IntrinsicPropInfosOfTypeInScope (infoReader:InfoReader) (optFilter, ad) findFlag m typ =
    let g = infoReader.g
    let amap = infoReader.amap
    let pinfos = GetIntrinsicPropInfoSetsOfType infoReader (optFilter, ad, AllowMultiIntfInstantiations.No) findFlag m typ
    let pinfos = pinfos |> ExcludeHiddenOfPropInfos g amap m 
    pinfos

let ExtensionPropInfosOfTypeInScope (infoReader:InfoReader) (eExtensionMembers: TyconRefMultiMap<_>) (optFilter, ad) _findFlag m typ =
    let g = infoReader.g
    let amap = infoReader.amap
    infoReader.ReadEntireTypeHierachy(AllowMultiIntfInstantiations.No,m,typ) |> List.collect (fun typ -> 
         if (isAppTy g typ) then 
            let tcref = tcrefOfAppTy g typ
            // NOTE: multiple "open"'s push multiple duplicate values into eExtensionMembers 
            // REVIEW: this looks a little slow: ListSet.setify is quadratic. 
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
                   | Some membInfo -> propCollector.Collect(membInfo,vref)
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
        // REVIEW: this looks a little slow: ListSet.setify is quadratic. 
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
                    | Some membInfo -> TrySelectMemberVal g optFilter typ (Some pri) membInfo vref
                | ILExtMem (actualParent,md,pri) when (match optFilter with None -> true | Some nm -> nm = md.Name) ->
                    if Tastops.isILAppTy g typ then
                        // 'typ' is the logical parent 
                        let tinfo = ILTypeInfo.FromType g typ
                        Some(ILMethInfo.Create (infoReader.amap, m, tinfo, Some actualParent, Some pri, md))
                    else
                       /// then, this is not a type definition backed by Abstract IL metadata.    
                        let tcref,_ = destAppTy g typ
                        if tcref.IsFSharpObjectModelTycon then
                            // case for C# extension method on an F# type
                            let fsObjKind = tcref.FSharpObjectModelTypeInfo.fsobjmodel_kind 
                            Some(ILMeth(infoReader.amap.g,ILFSMethInfo(tcref,fsObjKind,Some actualParent,md),Some pri))
                        else 
                            failwith "cannot happen: ILExtMem"
                | _ -> 
                    None) 
    else []

let ExtensionMethInfosOfTypeInScope (infoReader:InfoReader) eExtensionMembers (optFilter,ad) findFlag m typ =
    infoReader.ReadEntireTypeHierachy(AllowMultiIntfInstantiations.No,m,typ) |> List.collect (fun typ -> 
        ImmediateExtensionMethInfosOfTypeInScope infoReader eExtensionMembers (optFilter,ad) findFlag m typ) 

let AllMethInfosOfTypeInScope infoReader eExtensionMembers (optFilter,ad) findFlag m typ =
    IntrinsicMethInfosOfType infoReader (optFilter,ad,AllowMultiIntfInstantiations.No) findFlag m typ 
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
    | FSProp(_,_,_,Some set) -> set.CoreDisplayName
    | FSProp(_,_,Some get,_) -> get.CoreDisplayName
    | FSProp _ -> failwith "unexpected (property must have either getter or setter)"
    | ILProp(_,ILPropInfo(_,def))  -> def.Name
#if EXTENSIONTYPING
    | ProvidedProp(_,pi,_,m) -> pi.PUntaint((fun pi -> pi.Name), m)
#endif

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


// REVIEW: this shows up on performance logs. Consider for example endles resolutions  of "List.map" to 
// the empty set of results, or "x.Length" for a list or array type. This indicates it could be worth adding a cache here.
let rec ResolveLongIdentInTypePrim (ncenv:NameResolver) nenv lookupKind (resInfo:ResolutionInfo) depth m ad (lid:Ident list) findFlag (typeNameResInfo: TypeNameResolutionInfo) typ =
    let g = ncenv.g
    match lid with 
    | [] -> error(InternalError("ResolveLongIdentInTypePrim",m))
    | id :: rest -> 
        let m = unionRanges m id.idRange
        let nm = id.idText // used to filter the searches of the tables 
        let optFilter = Some nm // used to filter the searches of the tables 
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
                    | Some x ->  success (resInfo, x, rest)
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
            let nestedTypes = GetNestedTypesOfType (ad, ncenv, Some nm, (if isNil rest then typeNameResInfo.StaticArgsInfo else TypeNameResolutionStaticArgsInfo.Indefinite), true, m) typ
            if isNil rest then 
                if isNil nestedTypes then 
                    NoResultsOrUsefulErrors
                else 
                    match typeNameResInfo.ResolutionFlag with 
                    | ResolveTypeNamesToCtors -> 
                        nestedTypes 
                        |> CollectResults (ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo m ad) 
                        |> MapResults (fun (resInfo,item) -> (resInfo,item,[]))
                    | ResolveTypeNamesToTypeRefs -> 
                        OneSuccess (resInfo,Item.Types (nm,nestedTypes),rest)
            else 
                ResolveLongIdentInTypes ncenv nenv lookupKind resInfo (depth+1) m ad rest findFlag typeNameResInfo nestedTypes
        (OneResult contentsSearchAccessible +++ nestedSearchAccessible)
        
and ResolveLongIdentInTypes (ncenv:NameResolver) nenv lookupKind resInfo depth m ad lid findFlag typeNameResInfo typs = 
    typs |> CollectResults (ResolveLongIdentInTypePrim ncenv nenv lookupKind resInfo depth m ad lid findFlag typeNameResInfo >> AtMostOneResult m) 

let ResolveLongIdentInType sink ncenv nenv lookupKind m ad lid findFlag typeNameResInfo typ =
    let resInfo,item,rest = 
        ResolveLongIdentInTypePrim (ncenv:NameResolver) nenv lookupKind ResolutionInfo.Empty 0 m ad lid findFlag typeNameResInfo typ
        |> AtMostOneResult m
        |> ForceRaise
    ResolutionInfo.SendToSink (sink,ncenv,nenv,ItemOccurence.UseInType,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
    item,rest

let private ResolveLongIdentInTyconRef (ncenv:NameResolver) nenv lookupKind resInfo depth m ad lid typeNameResInfo tcref =
#if EXTENSIONTYPING
    // No dotting through type generators to get to a member!
    CheckForDirectReferenceToGeneratedType (tcref, PermitDirectReferenceToGeneratedType.No, m)
#endif
    let typ = FreshenTycon ncenv m tcref
    typ |> ResolveLongIdentInTypePrim ncenv nenv lookupKind resInfo depth m ad lid IgnoreOverrides typeNameResInfo  

let private ResolveLongIdentInTyconRefs (ncenv:NameResolver) nenv lookupKind depth m ad lid typeNameResInfo idRange tcrefs = 
    tcrefs |> CollectResults (fun (resInfo:ResolutionInfo,tcref) -> 
        let resInfo = resInfo.AddEntity(idRange,tcref)
        tcref |> ResolveLongIdentInTyconRef ncenv nenv lookupKind resInfo depth m ad lid typeNameResInfo |> AtMostOneResult m) 

//-------------------------------------------------------------------------
// ResolveExprLongIdentInModuleOrNamespace 
//------------------------------------------------------------------------- 

let (|AccessibleEntityRef|_|) ad (modref: ModuleOrNamespaceRef) mspec = 
    let eref = modref.MkNestedTyconRef mspec
    if IsEntityAccessible ad eref then Some eref else None

let rec ResolveExprLongIdentInModuleOrNamespace (ncenv:NameResolver) nenv (typeNameResInfo: TypeNameResolutionInfo) ad resInfo depth m modref (mty:ModuleOrNamespaceType) (lid :Ident list) =
    // resInfo records the modules or namespaces actually relevant to a resolution
    match lid with 
    | [] -> raze(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))
    | id :: rest ->
        let m = unionRanges m id.idRange
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
                let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, (if isNil rest then typeNameResInfo.StaticArgsInfo else TypeNameResolutionStaticArgsInfo.Indefinite), modref)
                let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
                if nonNil rest then 
                    let tcrefs = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, TypeNameResolutionInfo (ResolveTypeNamesToTypeRefs,TypeNameResolutionStaticArgsInfo.Indefinite), PermitDirectReferenceToGeneratedType.No, unionRanges m id.idRange)
                    ResolveLongIdentInTyconRefs ncenv nenv  LookupKind.Expr (depth+1) m ad rest typeNameResInfo id.idRange tcrefs
                // Check if we've got some explicit type arguments 
                else 
                    let tcrefs = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, typeNameResInfo, PermitDirectReferenceToGeneratedType.No, unionRanges m id.idRange)
                    match typeNameResInfo.ResolutionFlag with 
                    | ResolveTypeNamesToTypeRefs -> 
                        success [ for (resInfo,tcref) in tcrefs do 
                                      let typ = FreshenTycon ncenv m tcref
                                      let item = (resInfo,Item.Types(id.idText,[typ]),[])
                                      yield item ]
                    | ResolveTypeNamesToCtors -> 
                        tcrefs 
                        |> List.map (fun (resInfo, tcref) -> resInfo, FreshenTycon ncenv m tcref) 
                        |> CollectResults (fun (resInfo,typ) -> ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo id.idRange ad typ) 
                        |> MapResults (fun (resInfo,item) -> (resInfo,item,[]))

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


/// An identifier has resolved to a type name in an expression (corresponding to one or more TyconRefs). 
/// Return either a set of constructors (later refined by overload resolution), or a set of TyconRefs.
let ChooseTyconRefInExpr (ncenv:NameResolver, m, ad, nenv, id:Ident, typeNameResInfo:TypeNameResolutionInfo, resInfo:ResolutionInfo, tcrefs) =

      let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
      let tcrefs = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, typeNameResInfo, PermitDirectReferenceToGeneratedType.No, m)
      match typeNameResInfo.ResolutionFlag with 
      | ResolveTypeNamesToCtors ->
          let typs = tcrefs |> List.map (fun (resInfo,tcref) -> (resInfo,FreshenTycon ncenv m tcref)) 
          typs 
              |> CollectResults (fun (resInfo,typ) -> ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo id.idRange ad typ) 
              |> MapResults (fun (resInfo,item) -> (resInfo,item,[]))
      | ResolveTypeNamesToTypeRefs ->
          let typs = tcrefs |> List.map (fun (resInfo,tcref) -> (resInfo,FreshenTycon ncenv m tcref)) 
          success (typs |> List.map (fun (resInfo,typ) -> (resInfo,Item.Types(id.idText,[typ]),[])))



/// Resolve F# "A.B.C" syntax in expressions
/// Not all of the sequence will necessarily be swallowed, i.e. we return some identifiers 
/// that may represent further actions, e.g. further lookups. 

let rec ResolveExprLongIdentPrim sink (ncenv:NameResolver) fullyQualified m ad nenv (typeNameResInfo:TypeNameResolutionInfo) lid =
    let resInfo = ResolutionInfo.Empty
    match lid with 
    | [] -> error (Error(FSComp.SR.nrInvalidExpression(textOfLid lid), m))

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         
    | [id;next] when id.idText = MangledGlobalName ->
          ResolveExprLongIdentPrim sink ncenv fullyQualified m ad nenv typeNameResInfo [next]

    | id :: lid when id.idText = MangledGlobalName ->
          ResolveExprLongIdentPrim sink ncenv FullyQualified m ad nenv typeNameResInfo lid

    | [id] when fullyQualified <> FullyQualified ->

          // Single identifier.  Lookup the unqualified names in the environment
          let envSearch = 
              match nenv.eUnqualifiedItems.TryFind(id.idText) with

              // The name is a type name and it has not been clobbered by some other name
              | Some (Item.UnqualifiedType tcrefs) -> 
                  
                  // Do not use type names from the environment if an explicit type instantiation is 
                  // given and the number of type parameters do not match
                  let tcrefs = 
                      tcrefs |> List.filter (fun  tcref ->
                          typeNameResInfo.StaticArgsInfo.HasNoStaticArgsInfo || 
                          typeNameResInfo.StaticArgsInfo.NumStaticArgs = tcref.Typars(m).Length)
                  
                  let search = ChooseTyconRefInExpr (ncenv, m, ad, nenv, id, typeNameResInfo, resInfo, tcrefs)
                  match AtMostOneResult m search with 
                  | Result _ as res -> 
                      let resInfo,item,rest = ForceRaise res
                      ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.Use,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
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
                  ChooseTyconRefInExpr (ncenv, m, ad, nenv, id, typeNameResInfo, resInfo, tcrefs)

              let implicitOpSearch = 
                  if IsMangledOpName id.idText then 
                      success [(resInfo,Item.ImplicitOp id,[])] 
                  else NoResultsOrUsefulErrors

              let failingCase = raze (UndefinedName(0,FSComp.SR.undefinedNameValueOfConstructor,id,[]))
              let search = ctorSearch +++ implicitOpSearch +++ failingCase 
              let resInfo,item,rest = ForceRaise (AtMostOneResult m search) 
              ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.Use,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
              item,rest
              
            
    // A compound identifier. 
    // It still might be a value in the environment, or something in an F# module, namespace, typ, or nested type 
    | id :: rest -> 
    
        let m = unionRanges m id.idRange
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
          // Otherwise modules are searched first. REVIEW: modules and types should be searched together. 
          // For each module referenced by 'id', search the module as if it were an F# module and/or a .NET namespace. 
          let moduleSearch ad = 
               ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv ad lid 
                   (ResolveExprLongIdentInModuleOrNamespace ncenv nenv typeNameResInfo ad)

          // REVIEW: somewhat surprisingly, this shows up on performance traces, with tcrefs non-nil.
          // This seems strange since we would expect in the vast majority of cases tcrefs is empty here.
          let tyconSearch ad = 
              let tcrefs = LookupTypeNameInEnvNoArity fullyQualified id.idText nenv
              let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
              let tcrefs  = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, TypeNameResolutionInfo.ResolveToTypeRefs (TypeNameResolutionStaticArgsInfo.Indefinite), PermitDirectReferenceToGeneratedType.No, unionRanges m id.idRange)
              ResolveLongIdentInTyconRefs ncenv nenv LookupKind.Expr 1 m ad rest typeNameResInfo id.idRange tcrefs

          let envSearch = 
              match fullyQualified with 
              | FullyQualified -> 
                  NoResultsOrUsefulErrors
              | OpenQualified -> 
                  match nenv.eUnqualifiedItems.TryFind id.idText with
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
          ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.Use,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap m item));
          item,rest

let ResolveExprLongIdent sink (ncenv:NameResolver) m ad nenv typeNameResInfo lid =
    ResolveExprLongIdentPrim sink ncenv OpenQualified m ad nenv typeNameResInfo lid 

//-------------------------------------------------------------------------
// Resolve F#/IL "." syntax in patterns
//------------------------------------------------------------------------- 

let rec ResolvePatternLongIdentInModuleOrNamespace (ncenv:NameResolver) nenv numTyArgsOpt ad resInfo depth m modref (mty:ModuleOrNamespaceType) (lid: Ident list) =
    match lid with 
    | [] -> raze (InternalError("ResolvePatternLongIdentInModuleOrNamespace",m))
    | id :: rest ->
        let m = unionRanges m id.idRange
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
        let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, TypeNameResolutionStaticArgsInfo.Indefinite, modref)
        let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
        let tyconSearch = 
            match lid with 
            | _tn:: rest when nonNil rest ->
                ResolveLongIdentInTyconRefs (ncenv:NameResolver) nenv LookupKind.Pattern (depth+1) m ad rest numTyArgsOpt id.idRange tcrefs
            | _ -> 
                NoResultsOrUsefulErrors
        // Constructor of a type? 
        let ctorSearch = 
            if isNil rest then 
                tcrefs 
                |> List.map (fun (resInfo,tcref) -> (resInfo,FreshenTycon ncenv m tcref)) 
                |> CollectResults (fun (resInfo,typ) -> ResolveObjectConstructorPrim ncenv nenv.eDisplayEnv resInfo id.idRange ad typ) 
                |> MapResults (fun (resInfo,item) -> (resInfo,item,[]))
            else
                NoResultsOrUsefulErrors

        // Something in a sub-namespace or sub-module or nested-type 
        let moduleSearch = 
            if nonNil rest then 
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
let rec ResolvePatternLongIdentPrim sink (ncenv:NameResolver) fullyQualified warnOnUpper newDef m ad nenv numTyArgsOpt (lid:Ident list) =
    match lid with 

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         
    | id :: lid when id.idText = MangledGlobalName ->
        ResolvePatternLongIdentPrim sink ncenv FullyQualified warnOnUpper newDef m ad nenv numTyArgsOpt lid

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
        ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.Use,ad,resInfo,(fun () -> true));
  
        if nonNil rest then error(Error(FSComp.SR.nrIsNotConstructorOrLiteral(),(List.head rest).idRange));
        res


let ResolvePatternLongIdent sink (ncenv:NameResolver) warnOnUpper newDef m ad nenv numTyArgsOpt (lid:Ident list) =
    ResolvePatternLongIdentPrim sink ncenv OpenQualified warnOnUpper newDef m ad nenv numTyArgsOpt lid

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

let rec ResolveTypeLongIdentInTyconRefPrim (ncenv:NameResolver) (typeNameResInfo:TypeNameResolutionInfo) ad resInfo genOk depth m (tcref: TyconRef) (lid: Ident list) =
    let tcref = DerefAbbrevTyconRef_WORKAROUND ncenv tcref m
    match lid with 
    | [] -> error(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))
    | [id] -> 
#if EXTENSIONTYPING
        // No dotting through type generators to get to a nested type!
        CheckForDirectReferenceToGeneratedType (tcref, PermitDirectReferenceToGeneratedType.No, m)
#endif
        let m = unionRanges m id.idRange
        let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, typeNameResInfo.StaticArgsInfo, tcref)
        let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
        let tcrefs = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, typeNameResInfo, genOk, m) 
        match tcrefs with 
        | tcref :: _ -> success tcref
        | [] -> raze (UndefinedName(depth,FSComp.SR.undefinedNameType,id,[]))
    | id::rest ->
#if EXTENSIONTYPING
        // No dotting through type generators to get to a nested type!
        CheckForDirectReferenceToGeneratedType (tcref, PermitDirectReferenceToGeneratedType.No, m)
#endif
        let m = unionRanges m id.idRange
        // Search nested types
        let tyconSearch = 
            let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, TypeNameResolutionStaticArgsInfo.Indefinite, tcref)
            let tcrefs = tcrefs |> List.map (fun tcref -> (resInfo,tcref))
            let tcrefs  = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, typeNameResInfo.DropStaticArgsInfo, genOk, m)
            match tcrefs with 
            | _ :: _ -> tcrefs |> CollectResults (fun (resInfo,tcref) -> ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo ad resInfo genOk (depth+1) m tcref rest)
            | [] -> raze (UndefinedName(depth,FSComp.SR.undefinedNameType,id,[]))
            
        AtMostOneResult m tyconSearch

let ResolveTypeLongIdentInTyconRef sink (ncenv:NameResolver) nenv typeNameResInfo ad m tcref (lid: Ident list) =
    let resInfo,tcref = ForceRaise (ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo ad ResolutionInfo.Empty PermitDirectReferenceToGeneratedType.No 0 m tcref lid)
    ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.Use,ad,resInfo,(fun () -> true));
    tcref


let rec private ResolveTypeLongIdentInModuleOrNamespace (ncenv:NameResolver) (typeNameResInfo: TypeNameResolutionInfo) ad genOk (resInfo:ResolutionInfo) depth m modref _mty (lid: Ident list) =
    match lid with 
    | [] -> error(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))
    | [id] -> 
        // On all paths except error reporting we have isSome(staticResInfo), hence get at most one result back 
        let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, typeNameResInfo.StaticArgsInfo, modref)
        match tcrefs with 
        | _ :: _ -> tcrefs |> CollectResults (fun tcref -> success(resInfo,tcref))
        | [] -> raze (UndefinedName(depth,FSComp.SR.undefinedNameType,id,[]))
    | id::rest ->
        let m = unionRanges m id.idRange
        let modulSearch = 
            match modref.ModuleOrNamespaceType.ModulesAndNamespacesByDemangledName.TryFind(id.idText) with
            | Some(AccessibleEntityRef ad modref submodref) -> 
                let resInfo = resInfo.AddEntity(id.idRange,submodref)
                ResolveTypeLongIdentInModuleOrNamespace ncenv typeNameResInfo ad genOk resInfo (depth+1) m submodref submodref.ModuleOrNamespaceType rest
            | _ ->  
                raze (UndefinedName(depth,FSComp.SR.undefinedNameNamespaceOrModule,id,[]))
        let tyconSearch = 
            let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, TypeNameResolutionStaticArgsInfo.Indefinite, modref)
            match tcrefs with 
            | _ :: _ -> tcrefs |> CollectResults (fun tcref -> ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo ad resInfo genOk (depth+1) m tcref rest)
            | [] -> raze (UndefinedName(depth,FSComp.SR.undefinedNameType,id,[]))
        tyconSearch +++ modulSearch

let rec ResolveTypeLongIdentPrim (ncenv:NameResolver) fullyQualified m nenv ad (lid: Ident list) (staticResInfo: TypeNameResolutionStaticArgsInfo) genOk =
    let typeNameResInfo = TypeNameResolutionInfo.ResolveToTypeRefs staticResInfo
    match lid with 
    | [] -> error(Error(FSComp.SR.nrUnexpectedEmptyLongId(),m))

    | [id] when id.idText = MangledGlobalName ->
         error (Error(FSComp.SR.nrGlobalUsedOnlyAsFirstName(), id.idRange))
         
    | id :: lid when id.idText = MangledGlobalName ->
        ResolveTypeLongIdentPrim ncenv FullyQualified m nenv ad lid staticResInfo genOk

    | [id]  ->  
        match LookupTypeNameInEnvHaveArity fullyQualified id.idText staticResInfo.NumStaticArgs nenv with
        | Some res -> 
            let res = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities ([(ResolutionInfo.Empty,res)], typeNameResInfo, genOk, unionRanges m id.idRange)
            assert (res.Length = 1)
            success res.Head
        | None -> 
            // For Good Error Reporting! 
            let tcrefs = LookupTypeNameInEnvNoArity fullyQualified id.idText nenv
            match tcrefs with
            | tcref :: _tcrefs -> 
                // Note: This path is only for error reporting
                //CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities tcref rest typeNameResInfo m;
                success(ResolutionInfo.Empty,tcref)
            | [] -> 
                raze (UndefinedName(0,FSComp.SR.undefinedNameType,id,[]))

    | id::rest ->
        let m = unionRanges m id.idRange
        let tyconSearch = 
            match fullyQualified with 
            | FullyQualified ->
                NoResultsOrUsefulErrors
            | OpenQualified -> 
                match LookupTypeNameInEnvHaveArity fullyQualified id.idText staticResInfo.NumStaticArgs nenv with
                | Some tcref when IsEntityAccessible ad tcref -> 
                    OneResult (ResolveTypeLongIdentInTyconRefPrim ncenv typeNameResInfo ad ResolutionInfo.Empty genOk 1 m tcref rest)
                | _ -> 
                    NoResultsOrUsefulErrors
        let modulSearch = 
            ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv ad lid 
                (ResolveTypeLongIdentInModuleOrNamespace ncenv typeNameResInfo ad genOk)
            |?> List.concat 

        let modulSearchFailed() = 
            ResolveLongIndentAsModuleOrNamespaceThen fullyQualified nenv AccessibleFromSomeFSharpCode lid 
                (ResolveTypeLongIdentInModuleOrNamespace ncenv typeNameResInfo.DropStaticArgsInfo ad genOk)
            |?> List.concat 
        match tyconSearch +++ modulSearch with 
        | Result results -> 
            // NOTE: we delay checking the CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities condition until right at the end after we've
            // collected all possible resolutions of the type
            let tcrefs = CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (results, typeNameResInfo, genOk, rangeOfLid lid)
            match tcrefs with 
            | (resInfo,tcref) :: _ -> 
                // We've already reported the ambiguity, possibly as an error. Now just take the first possible result.
                success(resInfo,tcref)
            | [] -> 
                        // failing case - report nice ambiguity errors even in this case
                AtMostOneResult m ((tyconSearch +++ modulSearch +++ modulSearchFailed()) |?> (fun tcrefs -> CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, typeNameResInfo, genOk, rangeOfLid lid)))
            
        | _ ->  
            // failing case - report nice ambiguity errors even in this case
            AtMostOneResult m ((tyconSearch +++ modulSearch +++ modulSearchFailed()) |?> (fun tcrefs -> CheckForTypeLegitimacyAndMultipleGenericTypeAmbiguities (tcrefs, typeNameResInfo, genOk, rangeOfLid lid)))


let ResolveTypeLongIdent sink (ncenv:NameResolver) occurence fullyQualified nenv ad (lid: Ident list) staticResInfo genOk =
    let m = rangeOfLid lid
    let res = ResolveTypeLongIdentPrim ncenv fullyQualified m nenv ad lid staticResInfo genOk 
    // Register the result as a name resolution
    match res with 
    | Result (resInfo,tcref) -> 
        ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.UseInType, ad,resInfo,(fun () -> true));
        let item = Item.Types(tcref.DisplayName,[FreshenTycon ncenv m tcref])
        CallNameResolutionSink sink (m,nenv,item,item,occurence,nenv.eDisplayEnv,ad)
    | _ -> ()
    res |?> snd

//-------------------------------------------------------------------------
// Resolve F#/IL "." syntax in records etc.
//------------------------------------------------------------------------- 

let rec ResolveFieldInModuleOrNamespace (ncenv:NameResolver) nenv ad (resInfo:ResolutionInfo) depth m (modref: ModuleOrNamespaceRef) _mty (lid: Ident list) = 
    let typeNameResInfo = TypeNameResolutionInfo.Default
    match lid with 
    | id::rest -> 
        let m = unionRanges m id.idRange
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
                let tcrefs = LookupTypeNameInEntityMaybeHaveArity (ncenv.amap, id.idRange, id.idText, TypeNameResolutionStaticArgsInfo.Indefinite, modref)
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
    let typeNameResInfo = TypeNameResolutionInfo.Default
    let g = ncenv.g
    let m = id.idRange
    match mp with 
    | [] -> 
        if isAppTy g typ then 
            match ncenv.InfoReader.TryFindRecdOrClassFieldInfoOfType(id.idText,m,typ) with
            | Some (RecdFieldInfo(_,rfref)) -> [rfref]
            | None -> error(Error(FSComp.SR.nrTypeDoesNotContainSuchField((NicePrint.minimalStringOfType nenv.eDisplayEnv typ), id.idText),m))
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
    Item.RecdField(RecdFieldInfo(ncenv.InstantiationGenerator m (rfref.Tycon.Typars m), rfref))



/// Resolve F#/IL "." syntax in expressions (2).
/// We have an expr. on the left, and we do an access, e.g. 
/// (f obj).field or (f obj).meth.  The basic rule is that if l-r type 
/// inference has determined the outer type then we can proceed in a simple fashion. The exception 
/// to the rule is for field types, which applies if l-r was insufficient to 
/// determine any valid members 
//
// QUERY (instantiationGenerator cleanup): it would be really nice not to flow instantiationGenerator to here. 
let private ResolveExprDotLongIdent (ncenv:NameResolver) m ad nenv typ lid findFlag =
    let typeNameResInfo = TypeNameResolutionInfo.Default
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
        let ids = List.take (max 0 (lid.Length - rest.Length)) lid
        match ids with 
        | [] -> wholem
        | _ -> rangeOfLid ids

/// Filters method groups that will be sent to Visual Studio IntelliSense
/// to include only static/instance members

let filterMethodGroups (ncenv:NameResolver) itemRange item staticOnly =
    match item with
    | Item.MethodGroup(nm, minfos) -> 
        let minfos = minfos |> List.filter  (fun minfo -> 
           staticOnly = (minfo.GetObjArgTypes(ncenv.amap, itemRange, minfo.FormalMethodInst) |> isNil))
        Item.MethodGroup(nm, minfos)
    | item -> item

let needsOverloadResolution namedItem =
  match namedItem with
  | Item.MethodGroup(_,_::_::_) 
  | Item.CtorGroup(_,_::_::_)
  | Item.Property(_,_::_::_) -> true
  | _ -> false

type IfOverloadResolutionFails = IfOverloadResolutionFails of (unit -> unit)
// Specifies if overload resolution needs to notify Language Service of overload resolution
[<RequireQualifiedAccess>]
type AfterOverloadResolution =
    // Notfication is not needed
    |   DoNothing
    // Notfy the sink
    |   SendToSink of (Item -> unit) * IfOverloadResolutionFails // Overload resolution failure fallback
    // Find override among given overrides and notify the sink
    // 'Item' contains the candidate overrides.
    |   ReplaceWithOverrideAndSendToSink of Item * (Item -> unit) * IfOverloadResolutionFails // Overload resolution failure fallback


/// Called for 'TypeName.Bar' - for VS IntelliSense, we can filter out instance members from method groups
let ResolveLongIdentAsExprAndComputeRange (sink:TcResultsSink) (ncenv:NameResolver) wholem ad nenv typeNameResInfo lid = 
    let item,rest = ResolveExprLongIdent sink ncenv wholem ad nenv typeNameResInfo lid
    let itemRange = ComputeItemRange wholem lid rest
    
    // Record the precise resolution of the field for intellisense
    let item = filterMethodGroups ncenv itemRange item true
    let callSink refinedItem =
        CallNameResolutionSink sink (itemRange, nenv, refinedItem, item, ItemOccurence.Use, nenv.DisplayEnv, ad);
    let afterOverloadResolution =
        match sink.CurrentSink with
        |   None -> AfterOverloadResolution.DoNothing
        |   Some _ ->
              if needsOverloadResolution item then
                  AfterOverloadResolution.SendToSink(callSink, (fun () -> callSink item) |> IfOverloadResolutionFails)
              else
                 callSink item
                 AfterOverloadResolution.DoNothing
    item, itemRange, rest, afterOverloadResolution

let (|NonOverridable|_|) namedItem =
    match namedItem with
    |   Item.MethodGroup(_,minfos) when minfos |> List.exists(fun minfo -> minfo.IsVirtual || minfo.IsAbstract) -> None
    |   Item.Property(_,pinfos) when pinfos |> List.exists(fun pinfo -> pinfo.IsVirtualProperty) -> None
    |   _ -> Some ()



/// Called for 'expression.Bar' - for VS IntelliSense, we can filter out static members from method groups
/// Also called for 'GenericType<Args>.Bar' - for VS IntelliSense, we can filter out non-static members from method groups
let ResolveExprDotLongIdentAndComputeRange (sink:TcResultsSink) (ncenv:NameResolver) wholem ad nenv typ lid findFlag thisIsActuallyATyAppNotAnExpr = 
    let resolveExpr findFlag =
        let resInfo,item,rest = ResolveExprDotLongIdent ncenv wholem ad nenv typ lid findFlag
        let itemRange = ComputeItemRange wholem lid rest
        resInfo,item,rest,itemRange
    // "true" resolution
    let resInfo,item,rest,itemRange = resolveExpr findFlag 
    ResolutionInfo.SendToSink(sink,ncenv,nenv,ItemOccurence.Use,ad,resInfo,(fun () -> CheckAllTyparsInferrable ncenv.amap itemRange item));
    
    // Record the precise resolution of the field for intellisense/goto definition
    let afterOverloadResolution =
      match sink.CurrentSink with 
      |   None -> AfterOverloadResolution.DoNothing // do not retypecheck if nobody listens
      |   Some _ ->
              // resolution for goto definition
              let unrefinedItem,itemRange,overrides = 
                  match findFlag, item with
                  |   FindMemberFlag.PreferOverrides, _ 
                  |   _,                              NonOverridable() -> item,itemRange,false
                  |   FindMemberFlag.IgnoreOverrides,_ -> 
                          let _,item,_,itemRange = resolveExpr FindMemberFlag.PreferOverrides                
                          item, itemRange,true
              let sendToSink refinedItem = 
                  let staticOnly = thisIsActuallyATyAppNotAnExpr
                  let refinedItem = filterMethodGroups ncenv itemRange refinedItem staticOnly
                  let unrefinedItem = filterMethodGroups ncenv itemRange unrefinedItem staticOnly
                  CallNameResolutionSink sink (itemRange, nenv, refinedItem, unrefinedItem, ItemOccurence.Use, nenv.DisplayEnv, ad)                                
              match overrides,needsOverloadResolution unrefinedItem with
              |     false, true -> 
                        AfterOverloadResolution.SendToSink(sendToSink, IfOverloadResolutionFails(fun () -> sendToSink unrefinedItem))
              |     true, true  -> 
                        AfterOverloadResolution.ReplaceWithOverrideAndSendToSink(unrefinedItem,sendToSink, IfOverloadResolutionFails(fun () -> sendToSink unrefinedItem))
              |     _ , false   -> 
                        sendToSink unrefinedItem
                        AfterOverloadResolution.DoNothing
    item, itemRange, rest, afterOverloadResolution


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
          CheckILAttributesForUnseen g x.ILTyconRawMetadata.CustomAttrs m
       else 
          CheckFSharpAttributesForUnseen g x.Attribs m))

let IsTyconUnseen ad g m (x:TyconRef) = IsTyconUnseenObsoleteSpec ad g m x false

let IsValUnseen ad g m (v:ValRef) = 
    not (IsValAccessible ad v) ||
    v.IsCompilerGenerated ||
    v.Deref.IsClassConstructor ||
    CheckFSharpAttributesForUnseen g v.Attribs m

let IsUnionCaseUnseen ad g m (ucref:UnionCaseRef) = 
    not (IsUnionCaseAccessible ad ucref) ||
    IsTyconUnseen ad g m ucref.TyconRef || 
    CheckFSharpAttributesForUnseen g ucref.Attribs m

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
        | Some modrefs -> 
            List.collect (PartialResolveLookupInModuleOrNamespaceAsModuleOrNamespaceThen f rest) modrefs
        | None ->
            []
    | [] -> []

/// returns fields for the given class or record
let ResolveRecordOrClassFieldsOfType (ncenv: NameResolver) m ad typ statics = 
    GetRecordOrClassFieldsOfType ncenv.InfoReader (None,ad) m typ
    |> List.filter (fun rfref -> rfref.IsStatic = statics  &&  IsFieldInfoAccessible ad rfref)
    |> List.map Item.RecdField

let ResolveCompletionsInType (ncenv: NameResolver) nenv isApplicableMeth m ad statics typ =
    let g = ncenv.g
    let amap = ncenv.amap
    
    let rfinfos = 
        GetRecordOrClassFieldsOfType ncenv.InfoReader (None,ad) m typ
        |> List.filter (fun rfref -> rfref.IsStatic = statics  &&  IsFieldInfoAccessible ad rfref)

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
        if statics then
            typ
            |> GetNestedTypesOfType (ad, ncenv, None, TypeNameResolutionStaticArgsInfo.Indefinite, false, m) 
        else 
            []

    let finfos = 
        ncenv.InfoReader.GetILFieldInfosOfType(None,ad,m,typ)
        |> List.filter (fun x -> 
            not x.IsSpecialName &&
            x.IsStatic = statics && 
            IsILFieldInfoAccessible g amap m ad x)
    let pinfosIncludingUnseen = 
        AllPropInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (None,ad) PreferOverrides m typ
        |> List.filter (fun x -> 
            x.IsStatic = statics && 
            IsPropInfoAccessible g amap m ad x)


    // Exclude get_ and set_ methods accessed by properties 
    let pinfoMethNames = 
      (pinfosIncludingUnseen 
       |> List.filter (fun pinfo -> pinfo.HasGetter)
       |> List.map (fun pinfo -> pinfo.GetterMethod.LogicalName))
      @
      (pinfosIncludingUnseen 
       |> List.filter (fun pinfo -> pinfo.HasSetter)
       |> List.map (fun pinfo -> pinfo.SetterMethod.LogicalName))
    
    let einfoMethNames = 
        [ for einfo in einfos do 
            let delegateType = einfo.GetDelegateType(amap,m)
            let (SigOfFunctionForDelegate(invokeMethInfo,_,_,_)) = GetSigOfFunctionForDelegate ncenv.InfoReader delegateType m ad 
            // Only events with void return types are suppressed in intellisense.
            if slotSigHasVoidReturnTy (invokeMethInfo.GetSlotSig(amap, m)) then 
              yield einfo.GetAddMethod().DisplayName
              yield einfo.GetRemoveMethod().DisplayName ]

    let suppressedMethNames = Zset.ofList String.order (pinfoMethNames @ einfoMethNames)

    let pinfos = 
        pinfosIncludingUnseen
        |> List.filter (fun x -> not (PropInfoIsUnseen m x))

    let minfoFilter (minfo:MethInfo) = 
        // Only show the Finalize, MemberwiseClose etc. methods on System.Object for values whose static type really is 
        // System.Object. Few of these are typically used from F#.  
        //
        // Don't show GetHashCode or Equals for F# types that admit equality as an abnormal operation
        let isUnseenDueToBasicObjRules = 
            not (isObjTy g typ) &&
            match minfo.LogicalName with
            | "GetType"  -> false
            | "GetHashCode"  -> not (Augment.TypeDefinitelyHasEquality g typ)
            | "ToString" -> false
            | "Equals" -> not (minfo.IsInstance && Augment.TypeDefinitelyHasEquality g typ)
            | _ -> isObjTy g minfo.EnclosingType 
        let result = 
            not isUnseenDueToBasicObjRules &&
            not minfo.IsInstance = statics &&
            IsMethInfoAccessible amap m ad minfo &&
            not (MethInfoIsUnseen g m typ minfo) &&
            not minfo.IsConstructor &&
            not minfo.IsClassConstructor &&
            not (minfo.LogicalName = ".cctor") &&
            not (minfo.LogicalName = ".ctor") &&
            isApplicableMeth minfo typ &&
            not (suppressedMethNames.Contains minfo.LogicalName)
        result

    let pinfoItems = 
        pinfos
        |> List.map (fun pinfo -> DecodeFSharpEvent [pinfo] ad g ncenv m)
        |> List.filter (fun pinfo->pinfo.IsSome)
        |> List.map (fun pinfo->pinfo.Value)

    let addersAndRemovers = 
        pinfoItems 
        |> List.map (function Item.Event(FSEvent(_,_,addValRef,removeValRef)) -> [addValRef.LogicalName;removeValRef.LogicalName] | _ -> [])
        |> List.concat
    
    // REVIEW: add a name filter here in the common cases?
    let minfos = 
        AllMethInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (None,ad) PreferOverrides m typ 
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
      

let rec ResolvePartialLongIdentInType (ncenv: NameResolver) nenv isApplicableMeth m ad statics plid typ =
    let g = ncenv.g
    let amap = ncenv.amap
    match plid with
    | [] -> ResolveCompletionsInType ncenv nenv isApplicableMeth m ad statics typ
    | id :: rest ->
  
      let rfinfos = 
        GetRecordOrClassFieldsOfType ncenv.InfoReader (None,ad) m typ
        |> List.filter (fun fref -> IsRecdFieldAccessible ad fref.RecdFieldRef)
        |> List.filter (fun fref -> fref.RecdField.IsStatic = statics)
  
      let nestedTypes = 
          typ 
          |> GetNestedTypesOfType (ad, ncenv, Some id, TypeNameResolutionStaticArgsInfo.Indefinite, false, m)  

      // e.g. <val-id>.<recdfield-id>.<more> 
      (rfinfos |> List.filter (fun x -> x.Name = id)
               |> List.collect (fun x -> x.FieldType |> ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad false rest)) @

      // e.g. <val-id>.<property-id>.<more> 
      let FullTypeOfPinfo(pinfo:PropInfo) = 
        let rty = pinfo.GetPropertyType(amap,m) 
        let rty = if pinfo.IsIndexer then mkTupledTy g (pinfo.GetParamTypes(amap, m)) --> rty else  rty 
        rty      
      (typ
         |> AllPropInfosOfTypeInScope ncenv.InfoReader nenv.eExtensionMembers (Some id,ad) IgnoreOverrides m
         |> List.filter (fun x -> x.IsStatic = statics)
         |> List.filter (IsPropInfoAccessible g amap m ad) 
         |> List.collect (fun pinfo -> (FullTypeOfPinfo pinfo) |> ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad false rest)) @

      // e.g. <val-id>.<event-id>.<more> 
      (ncenv.InfoReader.GetEventInfosOfType(Some id,ad,m,typ)
         |> List.collect (PropTypOfEventInfo ncenv.InfoReader m ad >> ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad false rest)) @

      // nested types! 
      (nestedTypes 
         |> List.collect (ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad statics rest)) @

      // e.g. <val-id>.<il-field-id>.<more> 
      (ncenv.InfoReader.GetILFieldInfosOfType(Some id,ad,m,typ)
         |> List.filter (fun x -> 
             not x.IsSpecialName &&
             x.IsStatic = statics && 
             IsILFieldInfoAccessible g amap m ad x)
         |> List.collect (fun x -> x.FieldType(amap,m) |> ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad false rest))
     
let InfosForTyconConstructors (ncenv:NameResolver) m ad (tcref:TyconRef) = 
    let g = ncenv.g
    let amap = ncenv.amap
    // Don't show constructors for type abbreviations. See FSharp 1.0 bug 2881
    if tcref.IsTypeAbbrev then 
        []
    else 
        let typ = FreshenTycon ncenv m tcref
        match ResolveObjectConstructor ncenv (DisplayEnv.Empty g) m ad typ with 
        | Result item -> 
            match item with 
            | Item.FakeInterfaceCtor _ -> []
            | Item.CtorGroup(nm,ctorInfos) -> 
                let ctors = 
                    ctorInfos 
                    |> List.filter (IsMethInfoAccessible amap m ad)
                    |> List.filter (MethInfoIsUnseen g m typ >> not)
                match ctors with 
                | [] -> []
                | _ -> [MakeCtorGroup(nm,ctors)]
            | item -> 
                [item]
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
         // REVIEW: consider changing this to protectAssemblyExploration. We shouldn't need
         // to catch arbitrary exceptions here.
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

let rec ResolvePartialLongIdentInModuleOrNamespace (ncenv: NameResolver) nenv isApplicableMeth m ad (modref:ModuleOrNamespaceRef) plid allowObsolete =
    let g = ncenv.g
    let mty = modref.ModuleOrNamespaceType
    
    let tycons = 
        mty.TypeDefinitions
        |> List.filter (fun tcref -> not (tcref.LogicalName.Contains(",")))
        |> List.filter (fun tycon -> not (IsTyconUnseen ad g m (modref.MkNestedTyconRef tycon)))

    let ilTyconNames = 
        mty.TypesByAccessNames.Values
        |> Seq.toList
        |> List.choose (fun (tycon:Tycon) -> if tycon.IsILTycon then Some tycon.DisplayName else None)
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
          |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> notFakeContainerModule ilTyconNames)
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
             ResolvePartialLongIdentInModuleOrNamespace ncenv nenv isApplicableMeth m ad (modref.MkNestedTyconRef mspec) rest allowObsolete
         | _ -> [])

      @ (LookupTypeNameInEntityNoArity m id modref.ModuleOrNamespaceType
         |> List.collect (fun tycon ->
             let tcref = modref.MkNestedTyconRef tycon 
             if not (IsTyconUnseenObsoleteSpec ad g m tcref allowObsolete) then 
                 tcref |> generalizedTyconRef |> ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad true rest
             else 
                 []))

/// allowObsolete - specifies whether we should return obsolete types & modules 
///   as (no other obsolete items are returned)
let rec ResolvePartialLongIdentPrim (ncenv: NameResolver) (nenv: NameResolutionEnv) isApplicableMeth fullyQualified m ad plid allowObsolete = 
    let g = ncenv.g

    match  plid with
    |  id :: plid when id = "global" -> // this is deliberately not the mangled name

       ResolvePartialLongIdentPrim ncenv nenv isApplicableMeth FullyQualified m ad plid allowObsolete

    |  [] -> 
    
       let ilTyconNames =
          nenv.TyconsByAccessNames(fullyQualified).Values
          |> Seq.toList
          |> List.choose (fun tyconRef -> if tyconRef.IsILTycon then Some tyconRef.DisplayName else None)
          |> Set.ofSeq      
       
       /// Include all the entries in the eUnqualifiedItems table. 
       let unqualifiedItems = 
           match fullyQualified with 
           | FullyQualified -> []
           | OpenQualified ->
               nenv.eUnqualifiedItems.Values
               |> Seq.toList
               |> List.filter (function Item.UnqualifiedType _ -> false | _ -> true)
               |> List.filter (ItemIsUnseen ad g m >> not)

       let activePatternItems = 
           match fullyQualified with 
           | FullyQualified -> []
           | OpenQualified ->
               nenv.ePatItems
               |> NameMap.range
               |> List.filter (function Item.ActivePatternCase _v -> true | _ -> false)

       let moduleAndNamespaceItems = 
           nenv.ModulesAndNamespaces(fullyQualified)
           |> NameMultiMap.range 
           |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> IsInterestingModuleName  )
           |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> notFakeContainerModule ilTyconNames)
           |> List.filter (EntityRefContainsSomethingAccessible ncenv m ad)
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.map ItemForModuleOrNamespaceRef

       let tycons = 
           nenv.TyconsByDemangledNameAndArity(fullyQualified).Values
           |> Seq.toList
           |> List.filter (fun tcref -> not (tcref.LogicalName.Contains(",")))
           |> List.filter (fun tcref -> not tcref.IsExceptionDecl) 
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.map (ItemOfTyconRef ncenv m)

       // Get all the constructors accessible from here
       let constructors =  
           nenv.TyconsByDemangledNameAndArity(fullyQualified).Values
           |> Seq.toList
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.collect (InfosForTyconConstructors ncenv m ad)

       unqualifiedItems @ activePatternItems @ moduleAndNamespaceItems @ tycons @ constructors 

    | id :: rest -> 
    
        // Look in the namespaces 'id' 
        let namespaces = 
            PartialResolveLongIndentAsModuleOrNamespaceThen nenv [id] (fun modref -> 
              let allowObsolete = rest <> [] && allowObsolete
              if EntityRefContainsSomethingAccessible ncenv m ad modref then 
                ResolvePartialLongIdentInModuleOrNamespace ncenv nenv isApplicableMeth m ad modref rest allowObsolete
              else 
                [])
        // Look for values called 'id' that accept the dot-notation 
        let values,isItemVal = 
            (if nenv.eUnqualifiedItems.ContainsKey(id) then 
                     // v.lookup : member of a value
              let v = nenv.eUnqualifiedItems.[id]
              match v with 
              | Item.Value x -> 
                  let typ = x.Type
                  let typ = if x.BaseOrThisInfo = CtorThisVal && isRefCellTy g typ then destRefCellTy g typ else typ
                  (ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad false rest  typ),true
              | _ -> [],false
             else [],false)
        let staticSometingInType = 
            [ if not isItemVal then 
                // type.lookup : lookup a static something in a type 
                for tcref in LookupTypeNameInEnvNoArity OpenQualified id nenv do
                    let tcref = DerefAbbrevTyconRef_WORKAROUND ncenv tcref m
                    let typ = FreshenTycon ncenv m tcref
                    yield! ResolvePartialLongIdentInType ncenv nenv isApplicableMeth m ad true rest typ ]
        namespaces @ values @ staticSometingInType
        

let ResolvePartialLongIdent ncenv nenv isApplicableMeth m ad plid allowObsolete = 
    ResolvePartialLongIdentPrim ncenv nenv isApplicableMeth OpenQualified m ad plid allowObsolete 

// REVIEW: has much in common with ResolvePartialLongIdentInModuleOrNamespace - probably they should be united
let rec ResolvePartialLongIdentInModuleOrNamespaceForRecordFields (ncenv: NameResolver) nenv m ad (modref:ModuleOrNamespaceRef) plid allowObsolete =
    let g = ncenv.g
    let mty = modref.ModuleOrNamespaceType
    
    // get record type constructors
    let tycons = 
        mty.TypeDefinitions
        |> List.filter (fun tcref -> not (tcref.LogicalName.Contains(",")))
        |> List.filter (fun tycon -> tycon.IsRecordTycon)
        |> List.filter (fun tycon -> not (IsTyconUnseen ad g m (modref.MkNestedTyconRef tycon)))

    let ilTyconNames = 
        mty.TypesByAccessNames.Values
        |> Seq.toList
        |> List.choose (fun (tycon:Tycon) -> if tycon.IsILTycon then Some tycon.DisplayName else None)
        |> Set.ofSeq      
    
    match plid with 
    | [] -> 
        // Collect up the accessible sub-modules 
       (mty.ModulesAndNamespacesByDemangledName 
          |> NameMap.range 
          |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> notFakeContainerModule ilTyconNames)
          |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> IsInterestingModuleName)
          |> List.map modref.MkNestedTyconRef
          |> List.filter (IsTyconUnseen ad g m >> not)
          |> List.filter (EntityRefContainsSomethingAccessible ncenv m ad)
          |> List.map ItemForModuleOrNamespaceRef)

       // Collect all accessible record types
       @ (tycons |> List.map (modref.MkNestedTyconRef >> ItemOfTyconRef ncenv m) )
       @ [ // accessible record fields
            for tycon in tycons do
                if IsEntityAccessible ad (modref.MkNestedTyconRef tycon) then
                    let ttype = FreshenTycon ncenv m (modref.MkNestedTyconRef tycon)
                    yield! 
                        GetRecordOrClassFieldsOfType ncenv.InfoReader (None, ad) m ttype
                        |> List.map Item.RecdField
         ]

    | id :: rest  -> 
        (match mty.ModulesAndNamespacesByDemangledName.TryFind(id) with
         | Some mspec 
             when not (IsTyconUnseenObsoleteSpec ad g m (modref.MkNestedTyconRef mspec) allowObsolete) -> 
             let allowObsolete = rest <> [] && allowObsolete
             ResolvePartialLongIdentInModuleOrNamespaceForRecordFields ncenv nenv m ad (modref.MkNestedTyconRef mspec) rest allowObsolete
         | _ -> [])
        @ (
            match rest with
            | [] -> 
                // get all fields from the type named 'id' located in current modref
                let tycons = LookupTypeNameInEntityNoArity m id modref.ModuleOrNamespaceType
                tycons
                |> List.filter (fun tc -> tc.IsRecordTycon)
                |> List.collect (fun tycon ->
                    let tcref = modref.MkNestedTyconRef tycon
                    let ttype = FreshenTycon ncenv m tcref
                    GetRecordOrClassFieldsOfType ncenv.InfoReader (None, ad) m ttype                    
                    )
                |> List.map Item.RecdField
            | _ -> []
        )

/// allowObsolete - specifies whether we should return obsolete types & modules 
///   as (no other obsolete items are returned)
let rec ResolvePartialLongIdentToClassOrRecdFields (ncenv: NameResolver) (nenv: NameResolutionEnv) m ad plid (allowObsolete : bool) = 
    ResolvePartialLongIdentToClassOrRecdFieldsImpl ncenv nenv OpenQualified m ad plid allowObsolete

and ResolvePartialLongIdentToClassOrRecdFieldsImpl (ncenv: NameResolver) (nenv: NameResolutionEnv) fullyQualified m ad plid allowObsolete = 
    let g = ncenv.g

    match  plid with
    |  id :: plid when id = "global" -> // this is deliberately not the mangled name
       // dive deeper
       ResolvePartialLongIdentToClassOrRecdFieldsImpl ncenv nenv FullyQualified m ad plid allowObsolete
    |  [] ->     
        
        // empty plid - return namespaces\modules\record types\accessible fields
       let iltyconNames =
          nenv.TyconsByAccessNames(fullyQualified).Values
          |> Seq.toList
          |> List.choose (fun tyconRef -> if tyconRef.IsILTycon then Some tyconRef.DisplayName else None)
          |> Set.ofSeq

       let mods = 
           nenv.ModulesAndNamespaces(fullyQualified)
           |> NameMultiMap.range 
           |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> IsInterestingModuleName  )
           |> List.filter (fun x -> x.DemangledModuleOrNamespaceName |> notFakeContainerModule iltyconNames)
           |> List.filter (EntityRefContainsSomethingAccessible ncenv m ad)
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.map ItemForModuleOrNamespaceRef

       let recdTyCons = 
           nenv.TyconsByDemangledNameAndArity(fullyQualified).Values
           |> Seq.toList
           |> List.filter (fun tcref -> not (tcref.LogicalName.Contains(",")))
           |> List.filter (fun tcref -> tcref.IsRecordTycon) 
           |> List.filter (IsTyconUnseen ad g m >> not)
           |> List.map (ItemOfTyconRef ncenv m)

       let recdFields = 
           nenv.eFieldLabels
           |> Seq.collect (fun (KeyValue(_, v)) -> v)
           |> Seq.map (fun fref -> 
                let typeInsts = fref.TyconRef.TyparsNoRange |> List.map (fun tyar -> tyar.AsType)
                Item.RecdField(RecdFieldInfo(typeInsts, fref))
            )
           |> List.ofSeq

       mods @ recdTyCons @ recdFields

    | id::rest -> 
        // Get results
        let modsOrNs = 
            PartialResolveLongIndentAsModuleOrNamespaceThen nenv [id] (fun modref -> 
              let allowObsolete = rest <> [] && allowObsolete
              if EntityRefContainsSomethingAccessible ncenv m ad modref then 
                ResolvePartialLongIdentInModuleOrNamespaceForRecordFields ncenv nenv m ad modref rest allowObsolete // obsolette??
              else 
                [])
        let qualifiedFields = 
            match rest with
            | [] ->
                // get record types accessible in given nenv
                let tycons = LookupTypeNameInEnvNoArity OpenQualified id nenv
                tycons
                |> List.collect (fun tcref ->
                    let ttype = FreshenTycon ncenv m tcref
                    GetRecordOrClassFieldsOfType ncenv.InfoReader (None, ad) m ttype                    
                    )
                |> List.map Item.RecdField
            | _-> []
        modsOrNs @ qualifiedFields