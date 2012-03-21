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


module internal Microsoft.FSharp.Compiler.Import

open System.Collections.Generic
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler 

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger


type AssemblyLoader = 
    abstract LoadAssembly : range * ILAssemblyRef -> CcuResolutionResult
        


//-------------------------------------------------------------------------
// Import an IL types as F# types.
//------------------------------------------------------------------------- 

// This is the context used for converting AbstractIL .NET types to F# internal compiler data structures.
// We currently cache the conversion of AbstractIL ILTypeRef nodes, based on hashes of these.
[<Sealed>]
type ImportMap(g:TcGlobals,assemMap:AssemblyLoader) =
    let typeRefToTyconRefCache = new System.Collections.Generic.Dictionary<ILTypeRef,TyconRef>()
    member this.g = g
    member this.assemMap = assemMap
    member this.IlTypeRefToTyconRefCache = typeRefToTyconRefCache

let ImportTypeRefData (env:ImportMap) m (scoref,path,typeName) = 
    let ccu =  
        match scoref with 
        | ILScopeRef.Local    -> error(InternalError("ImportILTypeRef: unexpected local scope",m))
        | ILScopeRef.Module _ -> error(InternalError("ImportILTypeRef: reference found to a type in an auxiliary module",m))
        | ILScopeRef.Assembly assref -> env.assemMap.LoadAssembly (m,assref)  // NOTE: only assemMap callsite

    // Do a dereference of a fake tcref for the type just to check it exists in the target assembly and to find
    // the corresponding Tycon.
    let ccu = 
        match ccu with
        | ResolvedCcu(ccu)->ccu
        | UnresolvedCcu(ccuName) -> 
            error (Error(FSComp.SR.impTypeRequiredUnavailable(typeName, ccuName),m))
    let fakeTyconRef = mkNonLocalTyconRef (mkNonLocalEntityRef ccu path) typeName
    let tycon = 
        try   
            fakeTyconRef.Deref
        with _ ->
            error (Error(FSComp.SR.impReferencedTypeCouldNotBeFoundInAssembly(String.concat "." (Array.append path  [| typeName |]), ccu.AssemblyName),m))
    match tryRescopeEntity ccu tycon with 
    | None -> error (Error(FSComp.SR.impImportedAssemblyUsesNotPublicType(String.concat "." (Array.toList path@[typeName])),m));
    | Some tcref -> tcref
    

/// Import an IL type ref as an F# type constructor.
let ImportIlTypeRefUncached (env:ImportMap) m (tref:ILTypeRef) = 
    let tname = tref.Name
    let encl = tref.Enclosing
    let path,typeName = (match encl with [] -> splitTypeNameToArray tname | h :: t -> Array.append (splitNamespaceToArray h) (Array.ofList t), tname)
    ImportTypeRefData (env:ImportMap) m (tref.Scope,path,typeName)

    
let ImportILTypeRef (env:ImportMap) m (tref:ILTypeRef) =
    if env.IlTypeRefToTyconRefCache.ContainsKey(tref) then
        env.IlTypeRefToTyconRefCache.[tref]
    else 
        let tcref = ImportIlTypeRefUncached  env m tref
        env.IlTypeRefToTyconRefCache.[tref] <- tcref;
        tcref

let ImportTyconRefApp (env:ImportMap) tcref tyargs = 
    // Prefer the F# abbreviation for some built-in types, e.g. 
    // 'string' rather than 'System.String', since users don't 
    // on the whole realise that these are defined in terms of their .NET equivalents 
    // Also on import we decompile uses of FSharpFunc and Tuple. 
    match env.g.better_tcref_map tcref tyargs with 
    | Some res -> res
    | None -> TType_app (tcref,tyargs) 

/// Import an IL type as an F# type
/// - The F# type check does the job of making the "void" into a "unit" value, whatever the repr. of "unit" is. 
let rec ImportILType (env:ImportMap) m tinst typ =  
    match typ with
    | ILType.Void -> 
        env.g.unit_ty

    | ILType.Array(bounds,ty) -> 
        let n = bounds.Rank
        let elementType = ImportILType env m tinst ty
        mkArrayTy env.g n elementType 

    | ILType.Boxed  tspec | ILType.Value tspec ->
        let tcref = ImportILTypeRef env m tspec.TypeRef 
        let inst = tspec.GenericArgs |> List.map (ImportILType env m tinst) 
        ImportTyconRefApp env tcref inst

    | ILType.Byref ty -> mkByrefTy env.g (ImportILType env m tinst ty)
    | ILType.Ptr ty  -> mkNativePtrType env.g (ImportILType env m tinst ty)
    | ILType.FunctionPointer _ -> env.g.nativeint_ty (* failwith "cannot import this kind of type (ptr, fptr)" *)
    | ILType.Modified(_,_,ty) -> 
         // All custom modifiers are ignored
         ImportILType env m tinst ty
    | ILType.TypeVar u16 -> 
         try List.nth tinst (int u16) 
         with _ -> 
              error(Error(FSComp.SR.impNotEnoughTypeParamsInScopeWhileImporting(),m))

//-------------------------------------------------------------------------
// Load an IL assembly into the compiler's internal data structures
// Careful use is made of laziness here to ensure we don't read the entire IL
// assembly on startup.
//-------------------------------------------------------------------------- 


// tinst gives the type parameters for the enclosing type when converting the type parameters of a generic method
let ImportIlTypars amap m scoref tinst (gps: ILGenericParameterDefs) = 
    match gps with 
    | [] -> []
    | _ -> 
        let amap = amap()
        let tps = gps |> List.map (fun gp -> NewRigidTypar gp.Name m) 

        let tptys = tps |> List.map mkTyparTy
        let importInst = tinst@tptys
        (tps,gps) ||> List.iter2 (fun tp gp -> 
            let constraints = gp.Constraints |> List.map (fun ilty -> TTyparCoercesToType(ImportILType amap m importInst (rescopeILType scoref ilty),m) )
            let constraints = if gp.HasReferenceTypeConstraint then (TTyparIsReferenceType(m)::constraints) else constraints
            let constraints = if gp.HasNotNullableValueTypeConstraint then (TTyparIsNotNullableValueType(m)::constraints) else constraints
            let constraints = if gp.HasDefaultConstructorConstraint then (TTyparRequiresDefaultConstructor(m)::constraints) else constraints
            tp.FixupConstraints constraints);
        tps


let multisetDiscriminateAndMap nodef tipf (items: ('Key list * 'Value) list) = 
    // Find all the items with an empty key list and call 'tipf' 
    let tips = 
        [ for (keylist,v) in items do 
             match keylist with 
             | [] -> yield tipf v
             | _ -> () ]

    // Find all the items with a non-empty key list. Bucket them together by
    // the first key. For each bucket, call 'nodef' on that head key and the bucket.
    let nodes = 
        let buckets = new Dictionary<_,_>(10)
        for (keylist,v) in items do
            match keylist with 
            | [] -> ()
            | key::rest -> 
                buckets.[key] <- (rest,v) :: (if buckets.ContainsKey key then buckets.[key] else []);

        [ for (KeyValue(key,items)) in buckets -> nodef key items ]

    tips,nodes
 

let rec importIlTypeDef amap m scoref cpath enc nm (lazyTypeDef:Lazy<ILTypeDef>)  =
    let tdef = lazyTypeDef.Force()
    let lazyModuleOrNamespaceTypeForNestedTypes = 
        let nested = tdef.NestedTypes
        lazy 
            (let cpath = (mkNestedCPath cpath nm FSharpModule)
             importIlTypeDefs amap m scoref cpath (enc@[tdef]) nested) 
    // Add the type itself. 
    NewILTycon 
        (Some cpath) 
        (nm,m) 
        // The read of the type parameters may fail to resolve types. We pick up a new range from the point where that read is forced
        // Make sure we reraise the original exception one occurs - see findOriginalException.
        (LazyWithContext<_,_>.Create((fun m -> ImportIlTypars amap m scoref [] tdef.GenericParams), ErrorLogger.findOriginalException))
        (scoref,enc,tdef) 
        lazyModuleOrNamespaceTypeForNestedTypes 
       

and importIlTypeDefList amap m _scoref cpath enc items =
    // Split into the ones with namespaces and without 
    // This is a non-trivial function.  
    // Add the ones with namespaces in buckets 
    // That is, multi-set discriminate based in the first element of the namespace list (e.g. "System") 
    // and for each bag resulting from the discrimination fold-in a lazy computation to add the types under that bag 
    let rec add cpath items = 
        let tycons,namespaceModules = 
           items 
           |> multisetDiscriminateAndMap 
              // nodef - called for each bucket, where 'n' is the head element of the namespace used
              // as a key in the discrimination, tgs is the remaining descriptors.  We create a sub-module for 'n' 
              (fun n tgs ->
                  let modty = lazy (add (mkNestedCPath cpath n Namespace) tgs)
                  let mspec = NewModuleOrNamespace (Some cpath) taccessPublic (mkSynId m n) XmlDoc.Empty [] modty
                  mspec)

              // tipf - called if there are no namespace items left to discriminate on. 
              (fun (n,info:Lazy<_>) -> 
                 //Note: this scoref looks like it will always be identical to 'scoref'
                 let (scoref2,_,lazyTypeDef) = info.Force()
                 importIlTypeDef amap m scoref2 cpath enc n lazyTypeDef)

        let kind = match enc with [] -> Namespace | _ -> FSharpModule
        (NewModuleOrNamespaceType kind (tycons@namespaceModules) [] )
      
    add cpath items

and importIlTypeDefs amap m scoref cpath enc (tdefs: ILTypeDefs) =
    // We be very careful not to force a read of the type defs here
    tdefs.AsListOfLazyTypeDefs
    |> List.map (fun (ns,n,attrs,lazyTypeDef) -> (ns,(n,notlazy(scoref,attrs,lazyTypeDef))))
    |> importIlTypeDefList amap m scoref cpath enc

let importIlAssemblyMainTypeDefs amap m scoref modul = 
    modul.TypeDefs |> importIlTypeDefs amap m scoref (CompPath(scoref,[])) [] 

/// Read the "exported types" table for multi-module assemblies. 
let importIlAssemblyExportedType amap m auxModLoader (scoref:ILScopeRef) (exportedType:ILExportedTypeOrForwarder) = 
    // Forwarders are dealt with separately in the ref->def dereferencing logic in tast.fs as they effectively give rise to type equivalences
    if exportedType.IsForwarder then 
        []
    else
        let info = 
            lazy (match 
                    (try 
                        let modul = auxModLoader exportedType.ScopeRef
                        Some (lazy modul.TypeDefs.FindByName exportedType.Name) 
                     with :? System.Collections.Generic.KeyNotFoundException -> None)
                    with 
                  | None -> 
                     error(Error(FSComp.SR.impReferenceToDllRequiredByAssembly(exportedType.ScopeRef.QualifiedName, scoref.QualifiedName, exportedType.Name),m))
                  | Some lazyTypeDef -> 
                     scoref,exportedType.CustomAttrs,lazyTypeDef)
              
        let ns,n = splitTypeName exportedType.Name
        [ importIlTypeDefList amap m scoref (CompPath(scoref,[])) [] [(ns,(n,info))]  ]

/// Read the "exported types" table for multi-module assemblies. 
let ImportIlAssemblyExportedTypes amap m auxModLoader scoref (exportedTypes: ILExportedTypesAndForwarders) = 
    [ for exportedType in exportedTypes.AsList do 
         yield! importIlAssemblyExportedType amap m auxModLoader scoref exportedType ]

let ImportIlAssemblyTypeDefs (amap, m, auxModLoader, aref, mainmod:ILModuleDef) = 
    let scoref = ILScopeRef.Assembly aref
    let mtypsForExportedTypes = ImportIlAssemblyExportedTypes amap m auxModLoader scoref mainmod.ManifestOfAssembly.ExportedTypes
    let mainmod = importIlAssemblyMainTypeDefs amap m scoref mainmod
    combineModuleOrNamespaceTypeList [] m (mainmod :: mtypsForExportedTypes)

// Read the type forwarder table for an assembly
let ImportIlAssemblyTypeForwarders (amap, m, exportedTypes:ILExportedTypesAndForwarders) = 
    // Note 'td' may be in another module or another assembly!
    // Note: it is very important that we call auxModLoader lazily
    lazy 
      ([ //printfn "reading forwarders..." 
         for exportedType in exportedTypes.AsList do 
             let ns,n = splitTypeName exportedType.Name
             //printfn "found forwarder for %s..." n
             let tcref = ImportIlTypeRefUncached (amap()) m (ILTypeRef.Create(exportedType.ScopeRef,[],exportedType.Name))
             yield (Array.ofList ns,n),tcref
             let rec nested (nets:ILNestedExportedTypes) enc = 
                 [ for net in nets.AsList do 
                    
                       //printfn "found nested forwarder for %s..." net.Name
                       let tcref = ImportIlTypeRefUncached (amap()) m (ILTypeRef.Create (exportedType.ScopeRef,enc,net.Name))
                       yield (Array.ofList enc,exportedType.Name),tcref 
                       yield! nested net.Nested (enc @ [ net.Name ]) ]
             yield! nested exportedType.Nested (ns@[n]) ] 
       |> Map.ofList) 
  

let ImportIlAssembly(amap,m,auxModuleLoader,sref,sourceDir,filename,ilModule:ILModuleDef) = 
        let aref =   
            match sref with 
            | ILScopeRef.Assembly aref -> aref 
            | _ -> error(InternalError("PrepareToImportReferencedIlDll: cannot reference .NET netmodules directly, reference the containing assembly instead",m))
        let nm = aref.Name
        let mty = ImportIlAssemblyTypeDefs(amap,m,auxModuleLoader,aref,ilModule)
        let ccuData = 
          { IsFSharp=false;
            UsesQuotations=false;
            QualifiedName= Some sref.QualifiedName;
            Contents = NewCcuContents sref m nm mty ;
            IsEstGenerated = false;
            ILScopeRef = sref;
            Stamp = newStamp();
            SourceCodeDirectory = sourceDir;  // note: not an accurate value, but IL assemblies don't give us this information in any attributes. 
            FileName = filename
            MemberSignatureEquality= (fun ty1 ty2 -> Tastops.typeEquivAux EraseAll (amap()).g ty1 ty2)
            TypeForwarders = 
               (match ilModule.Manifest with 
                | None -> lazy Map.empty
                | Some manifest -> ImportIlAssemblyTypeForwarders(amap,m,manifest.ExportedTypes)) }
                
        CcuThunk.Create(nm,ccuData)
