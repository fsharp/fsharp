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

// Type providers, validation of provided types, etc.

namespace Microsoft.FSharp.Compiler

#if EXTENSIONTYPING

module internal ExtensionTyping =
    open System
    open System.IO
    open System.Reflection
    open System.Collections.Generic
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Compiler.ErrorLogger
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.AbstractIL.IL
    open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics // dprintfn
    open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library // frontAndBack
    open Internal.Utilities.FileSystem

#if TYPE_PROVIDER_SECURITY
    module internal GlobalsTheLanguageServiceCanPoke =
        //+++ GLOBAL STATE
        // This is the LS dialog, it is only poked once, when the LS is first constructed.  It lives here so the compiler can invoke it at the right moment in time.
        let mutable displayLSTypeProviderSecurityDialogBlockingUI = None : (string -> unit) option
        // This is poked by the LS (service.fs:UntypedParseImpl) and read later when the LS dialog pops up (code in servicem.fs:CreateService), called via displayLSTypeProviderSecurityDialogBlockingUI.
        // It would be complicated to plumb this info through end-to-end, so we use a global.  Since the LS only checks one file at a time, it is safe from race conditions.
        let mutable theMostRecentFileNameWeChecked = None : string option

    module internal ApprovalIO =
        let ApprovalsAbsoluteFileName = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData), @"Microsoft\FSharp\3.0\type-providers.txt")

        let partiallyCanonicalizeFileName fn = 
            (new FileInfo(fn)).FullName // avoid some trivialities like double backslashes or spaces before slashes (but preserves others like casing distinctions), see also bug 206595

        [<RequireQualifiedAccess>]
        type TypeProviderApprovalStatus =
            | NotTrusted of string
            | Trusted of string
            member this.FileName = 
#if DEBUG
                let verifyIsPartiallyCanonicalized(fn) =
                    assert(partiallyCanonicalizeFileName fn = fn)
                    fn
#else
                let verifyIsPartiallyCanonicalized = id
#endif
                match this with
                | TypeProviderApprovalStatus.NotTrusted(fn) -> verifyIsPartiallyCanonicalized fn
                | TypeProviderApprovalStatus.Trusted(fn) -> verifyIsPartiallyCanonicalized fn
            member this.isTrusted = 
                match this with
                | TypeProviderApprovalStatus.NotTrusted(_) -> false
                | TypeProviderApprovalStatus.Trusted(_) -> true

        let tryDoWithFileStreamUnderExclusiveLock(filename, f) =
            use file = File.Open(filename, FileMode.OpenOrCreate, FileAccess.ReadWrite)
            file.Lock(0L, 0L)
            f file

        // will retry 5 times with 100ms sleeps in between
        // may throw System.IO.IOException if it fails after that
        let tryDoWithFileStreamUnderExclusiveLockWithRetryFor500ms(filename, f) =
            let SLEEP_PER_TRY = 100
            let MAX_TRIES = 5
            let mutable retryCount = 0
            let mutable ok = false
            let mutable result = Unchecked.defaultof<_>
            while not(ok) do
                try
                    let r = tryDoWithFileStreamUnderExclusiveLock(filename, f)
                    ok <- true
                    result <- r
                with
                    | :? IOException when retryCount < MAX_TRIES -> 
                        retryCount <- retryCount + 1
                        System.Threading.Thread.Sleep(SLEEP_PER_TRY)
            result

        let doWithApprovalsFile (fileStreamOpt : FileStream option) f =
            match fileStreamOpt with
            | None -> 
                if not(FileSystem.SafeExists(ApprovalsAbsoluteFileName)) then
                    assert(fileStreamOpt = None)
                    let directoryName = Path.GetDirectoryName(ApprovalsAbsoluteFileName)
                    if not(Directory.Exists(directoryName)) then
                        Directory.CreateDirectory(directoryName) |> ignore // this creates multiple directory levels if needed
                    tryDoWithFileStreamUnderExclusiveLockWithRetryFor500ms(ApprovalsAbsoluteFileName, fun file ->
                        let text = 
#if DEBUG
                            System.String.Join(System.Environment.NewLine, 
                               ["""# This file is normally edited by Visual Studio, via Tools\Options\F# Tools\Type Provider Approvals"""
                                """# or by referencing a Type Provider from F# code for the first time."""
                                """# Each line should be one of these general forms:"""
                                """#     NOT_TRUSTED c:\path\filename.dll"""
                                """#     TRUSTED c:\path\filename.dll"""
                                """# Lines starting with a '#' are ignored as comments.""" ]) + System.Environment.NewLine
#else
                            ""
#endif
                        let bytes = System.Text.Encoding.UTF8.GetBytes(text)
                        file.Write(bytes, 0, bytes.Length)
                        f file)
                else
                    tryDoWithFileStreamUnderExclusiveLockWithRetryFor500ms(ApprovalsAbsoluteFileName, f)
            | Some fs -> f fs

        /// read all TP approval data.  does not throw, will swallow exceptions and return empty list if there's trouble.
        let readApprovalsFile fileStreamOpt : TypeProviderApprovalStatus list =
            try
                doWithApprovalsFile fileStreamOpt (fun file ->
                    file.Seek(0L, SeekOrigin.Begin) |> ignore
                    let sr = new StreamReader(file, System.Text.Encoding.UTF8)  // Note: we use 'let', not 'use' here, as closing the reader would close the file, and we don't want that
                    let lines = 
                            let text = sr.ReadToEnd()
                            text.Split([| System.Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
                            |> Array.filter (fun s -> not(s.StartsWith("#")))
                    let result = ResizeArray<TypeProviderApprovalStatus>()
                    let mutable bad = false
                    for s in lines do
                        if s.StartsWith("NOT_TRUSTED ") then
                            let partiallyCanonicalizedFileName = partiallyCanonicalizeFileName(s.Substring(12))
                            match result |> Seq.tryFind (fun r -> String.Compare(r.FileName, partiallyCanonicalizedFileName, StringComparison.CurrentCultureIgnoreCase) = 0) with
                            | None ->
                                result.Add(TypeProviderApprovalStatus.NotTrusted(partiallyCanonicalizedFileName))
                            | Some(r) ->  // there is another line of the file with the same filename
                                if r.isTrusted then
                                    bad <- true  // if conflicting status, then declare the file to be bad; if just duplicating same info, is ok
                        elif s.StartsWith("TRUSTED ") then
                            let partiallyCanonicalizedFileName = partiallyCanonicalizeFileName(s.Substring(8))
                            match result |> Seq.tryFind (fun r -> String.Compare(r.FileName, partiallyCanonicalizedFileName, StringComparison.CurrentCultureIgnoreCase) = 0) with
                            | None ->
                                result.Add(TypeProviderApprovalStatus.Trusted(partiallyCanonicalizedFileName))
                            | Some(r) ->  // there is another line of the file with the same filename
                                if not(r.isTrusted) then
                                    bad <- true  // if conflicting status, then declare the file to be bad; if just duplicating same info, is ok
                        else
                            bad <- true
                    if bad then
                        // The file is corrupt, just delete it 
                        file.SetLength(0L)
                        result.Clear()
                        try 
                            failwith "approvals file is corrupt, deleting"  // just to produce a first-chance exception for debugging
                        with 
                            _ -> ()
                    result |> List.ofSeq)
            with
                | :? System.IO.IOException ->
                    []
                | e ->
                    System.Diagnostics.Debug.Assert(false, e.ToString())  // what other exceptions might occur?
                    []

        /// append one piece of TP approval info.  may throw if trouble with file IO.
        let appendApprovalStatus fileStreamOpt (status:TypeProviderApprovalStatus) =
            let ok,line = 
                let partiallyCanonicalizedFileName = partiallyCanonicalizeFileName status.FileName
                match status with
                | TypeProviderApprovalStatus.NotTrusted(_) -> 
                    if Path.IsInvalidPath(partiallyCanonicalizedFileName) then 
                        assert(false)
                        false, ""
                    else
                        true, "NOT_TRUSTED "+partiallyCanonicalizedFileName
                | TypeProviderApprovalStatus.Trusted(_) -> 
                    if Path.IsInvalidPath(partiallyCanonicalizedFileName) then 
                        assert(false)
                        false, ""
                    else
                        true, "TRUSTED "+partiallyCanonicalizedFileName
            if ok then
                doWithApprovalsFile fileStreamOpt (fun file ->
                    let bytes = System.Text.Encoding.UTF8.GetBytes(line + System.Environment.NewLine)
                    file.Seek(0L, SeekOrigin.End) |> ignore
                    file.Write(bytes, 0, bytes.Length)
                    )

        /// replace one piece of TP approval info.  may throw if trouble with file IO.
        let replaceApprovalStatus fileStreamOpt (status : TypeProviderApprovalStatus) =
            let partiallyCanonicalizedFileName = partiallyCanonicalizeFileName status.FileName
            doWithApprovalsFile fileStreamOpt (fun file ->
                let priorApprovals = readApprovalsFile(Some file)
                let keepers = priorApprovals |> List.filter (fun app -> String.Compare(app.FileName, partiallyCanonicalizedFileName, StringComparison.CurrentCultureIgnoreCase) <> 0)
                file.SetLength(0L) // delete file
                keepers |> List.iter (appendApprovalStatus (Some file))
                appendApprovalStatus (Some file) status
            )

    module internal ApprovalsChecking =
        let discoverIfIsApprovedAndPopupDialogIfUnknown(runTimeAssemblyFileName : string, approvals : ApprovalIO.TypeProviderApprovalStatus list ref, popupDialogCallback : (string->unit) option) : bool=
            let partiallyCanonicalizedFileName = ApprovalIO.partiallyCanonicalizeFileName runTimeAssemblyFileName
            let rec coreLogic(whatToDoIfAssemblyIsUnknown) =
                match !approvals |> List.tryFind (function 
                                    | ApprovalIO.TypeProviderApprovalStatus.Trusted(s) -> String.Compare(partiallyCanonicalizedFileName,s,StringComparison.CurrentCultureIgnoreCase)=0 
                                    | ApprovalIO.TypeProviderApprovalStatus.NotTrusted(s) -> String.Compare(partiallyCanonicalizedFileName,s,StringComparison.CurrentCultureIgnoreCase)=0) with
                | Some(ApprovalIO.TypeProviderApprovalStatus.Trusted _) -> true
                | Some(ApprovalIO.TypeProviderApprovalStatus.NotTrusted _) -> false
                | None -> whatToDoIfAssemblyIsUnknown()

            coreLogic(fun() ->
                // This assembly is unknown. If we're in VS, pop up the dialog
                match popupDialogCallback with
                | None -> ()
                | Some callback -> 
                    // The callback had UI thread affinity.  But this code path runs as part of the VS background interactive checker, which must never block on the UI
                    // thread (or else it may deadlock, see bug 380608).  
                    System.Threading.ThreadPool.QueueUserWorkItem(fun _ ->
                        // the callback will pop up the dialog
                        callback(runTimeAssemblyFileName)
                    ) |> ignore
                // Behave like a 'NotTrusted'.  If the user trusts the assembly via the UI in a moment, the callback is responsible for requesting a re-typecheck.
                false)
#endif

    type TypeProviderDesignation = TypeProviderDesignation of string

    exception ProvidedTypeResolution of range * System.Exception 
    exception ProvidedTypeResolutionNoRange of System.Exception 

    type ResolutionEnvironment =
        { 
          resolutionFolder          : string
          outputFile                : string option
          showResolutionMessages    : bool
          referencedAssemblies      : string[]
          temporaryFolder           : string
        } 


    let private getTypeProviderImplementationTypes (runTimeAssemblyFileName:string, 
                                                    designTimeAssemblyNameString:string, 
                                                    m:range) : Type list =

        let raiseError (e:exn) =
            raise <| TypeProviderError(FSComp.SR.etProviderHasWrongDesignerAssembly(typeof<TypeProviderAssemblyAttribute>.Name, designTimeAssemblyNameString,e.Message), runTimeAssemblyFileName, m)

        let designTimeAssemblyOpt = 
            // Designer assembly resolution logic:
            // If the assembly name ends with .dll, or is just a simple name, we look in the directory next to runtime assembly
            // Else we only look in the GAC
            let loadFromDir fileName =
                let runTimeAssemblyPath = Path.GetDirectoryName runTimeAssemblyFileName
                let designTimeAssemblyPath = Path.Combine (runTimeAssemblyPath, fileName)
                try
                    Some (FileSystem.AssemblyLoadFrom designTimeAssemblyPath)
                with e ->
                    raiseError e
            let loadFromGac () =
                try
                    let asmName = System.Reflection.AssemblyName designTimeAssemblyNameString
                    Some (FileSystem.AssemblyLoad (asmName))
                with e ->
                    raiseError e


            if designTimeAssemblyNameString.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) then
                loadFromDir designTimeAssemblyNameString
            else
                let name = AssemblyName designTimeAssemblyNameString
                
                if name.Name.Equals(name.FullName, StringComparison.OrdinalIgnoreCase) then
                    let fileName = designTimeAssemblyNameString+".dll"
                    loadFromDir fileName
                else
                    loadFromGac()

        match designTimeAssemblyOpt with
        | Some loadedDesignTimeAssembly ->
            try
                let exportedTypes = loadedDesignTimeAssembly.GetExportedTypes() 
                let filtered = 
                    [ for t in exportedTypes do 
                          let ca = t.GetCustomAttributes(typeof<TypeProviderAttribute>, true)
                          if ca <> null && ca.Length > 0 then 
                              yield t ]
                filtered
            with e ->
                raiseError e
        | None -> []

    /// Create an instance of a type provider from the implementation type for the type provider in the
    /// design-time assembly by using reflection-invoke on a constructor for the type provider.
    let createTypeProvider (typeProviderImplementationType:System.Type, 
                            runtimeAssemblyPath, 
                            resolutionEnvironment:ResolutionEnvironment,
                            isInvalidationSupported:bool, 
                            isInteractive:bool,
                            systemRuntimeContainsType : string -> bool,
                            systemRuntimeAssemblyVersion : System.Version,
                            m:range) =
        let protect f =
            try 
                f ()
            with
            |   err ->
                    let strip (e:exn) =
                        match e with
                        |   :? TargetInvocationException as e -> e.InnerException
                        |   :? TypeInitializationException as e -> e.InnerException
                        |   _ -> e
                    let e = strip (strip err)
                    raise <| TypeProviderError(FSComp.SR.etTypeProviderConstructorException(e.Message), typeProviderImplementationType.FullName, m)
        if typeProviderImplementationType.GetConstructor([| typeof<TypeProviderConfig> |]) <> null then
            let e = new TypeProviderConfig(systemRuntimeContainsType,
                                           ResolutionFolder=resolutionEnvironment.resolutionFolder, 
                                           RuntimeAssembly=runtimeAssemblyPath, 
                                           ReferencedAssemblies=Array.copy resolutionEnvironment.referencedAssemblies, 
                                           TemporaryFolder=resolutionEnvironment.temporaryFolder,
                                           IsInvalidationSupported=isInvalidationSupported,
                                           IsHostedExecution= isInteractive,
                                           SystemRuntimeAssemblyVersion = systemRuntimeAssemblyVersion)
            protect (fun () -> Activator.CreateInstance(typeProviderImplementationType, [| box e|]) :?> ITypeProvider )
        elif typeProviderImplementationType.GetConstructor [| |] <> null then 
            protect (fun () -> Activator.CreateInstance(typeProviderImplementationType) :?> ITypeProvider )
        else
            raise (TypeProviderError(FSComp.SR.etProviderDoesNotHaveValidConstructor(), typeProviderImplementationType.FullName, m))

    let GetTypeProvidersOfAssembly(displayPSTypeProviderSecurityDialogBlockingUI : (string->unit) option, 
                                   validateTypeProviders:bool, 
#if TYPE_PROVIDER_SECURITY
                                   approvals, 
#endif
                                   runTimeAssemblyFileName:string, 
                                   ilScopeRefOfRuntimeAssembly:ILScopeRef,
                                   designTimeAssemblyNameString:string, 
                                   resolutionEnvironment:ResolutionEnvironment, 
                                   isInvalidationSupported:bool,
                                   isInteractive:bool,
                                   systemRuntimeContainsType : string -> bool,
                                   systemRuntimeAssemblyVersion : System.Version,
                                   m:range) : bool * Tainted<ITypeProvider> list  =         
        let ok = 
#if TYPE_PROVIDER_SECURITY
            if not validateTypeProviders then 
                true  // if not validating, then everything is ok
            else
                // pick the PS dialog if available (if so, we are definitely being called from a 'Build' from the PS), else use the LS one if available
                let dialog = match displayPSTypeProviderSecurityDialogBlockingUI with
                             | None -> GlobalsTheLanguageServiceCanPoke.displayLSTypeProviderSecurityDialogBlockingUI
                             | _    -> displayPSTypeProviderSecurityDialogBlockingUI
                let r = ApprovalsChecking.discoverIfIsApprovedAndPopupDialogIfUnknown(runTimeAssemblyFileName, approvals, dialog)
                if not r then
                    warning(Error(FSComp.SR.etTypeProviderNotApproved(runTimeAssemblyFileName), m))
                r
#else
            true
#endif
        let providerSpecs = 
            if ok then 
                try
                    let designTimeAssemblyName = 
                        try
                            AssemblyName designTimeAssemblyNameString |> Some 
                        with 
                        | :? ArgumentException ->
                            errorR(Error(FSComp.SR.etInvalidTypeProviderAssemblyName(runTimeAssemblyFileName,designTimeAssemblyNameString),m))
                            None
                    match designTimeAssemblyName,resolutionEnvironment.outputFile with
                    |   Some designTimeAssemblyName, Some path when String.Compare(designTimeAssemblyName.Name, Path.GetFileNameWithoutExtension path, StringComparison.OrdinalIgnoreCase) = 0 ->
                            []
                    |   Some _, _ ->
                            [ for t in getTypeProviderImplementationTypes (runTimeAssemblyFileName,designTimeAssemblyNameString,m) do
                                      let resolver = createTypeProvider (t, runTimeAssemblyFileName, resolutionEnvironment, isInvalidationSupported, isInteractive, systemRuntimeContainsType, systemRuntimeAssemblyVersion, m)
                                      match box resolver with 
                                      | null -> ()
                                      | _ -> yield (resolver,ilScopeRefOfRuntimeAssembly) ]
                    |   None, _ -> 
                            []
                with
                |   :? TypeProviderError as tpe ->
                        tpe.Iter(fun e -> errorR(NumberedError((e.Number,e.ContextualErrorMessage),m)) )                        
                        []
            else
                []
        let providers = Tainted<_>.CreateAll(providerSpecs)

        ok,providers

    let private unmarshal (t:Tainted<_>) = t.PUntaintNoFailure id

    let private tryTypeMember(st:Tainted<_>, fullName,memberName,m,recover,f) =
        try
            st.PApply (f,m)
        with
            :? TypeProviderError as tpe -> 
                tpe.Iter (fun e -> 
                    errorR(Error(FSComp.SR.etUnexpectedExceptionFromProvidedTypeMember(fullName,memberName,e.ContextualErrorMessage),m))  
                )

                st.PApplyNoFailure(fun _ -> recover)

    let private tryTypeMemberArray(st:Tainted<_>, fullName,memberName,m,f) =
        let result =
            try
                st.PApplyArray(f, memberName,m)
            with
                :? TypeProviderError as tpe ->
                    tpe.Iter (fun e -> 
                        errorR(Error(FSComp.SR.etUnexpectedExceptionFromProvidedTypeMember(fullName,memberName,e.ContextualErrorMessage),m))  
                    )

                    [||]

        match result with 
        | null -> errorR(Error(FSComp.SR.etUnexpectedNullFromProvidedTypeMember(fullName,memberName),m)); [||]
        | r -> r

    let private tryTypeMemberNonNull(st:Tainted<_>, fullName,memberName,m,recover,f) =
        match tryTypeMember(st,fullName,memberName,m,recover,f) with 
        | Tainted.Null -> 
            errorR(Error(FSComp.SR.etUnexpectedNullFromProvidedTypeMember(fullName,memberName),m)); 
            st.PApplyNoFailure(fun _ -> recover)
        | r -> r

    let tryMemberMember(mi:Tainted<_>,typeName,memberName,memberMemberName,m,recover,f) = 
        try
            mi.PApply (f,m)
        with
            :? TypeProviderError as tpe ->
                tpe.Iter (fun e -> 
                    errorR(Error(FSComp.SR.etUnexpectedExceptionFromProvidedMemberMember(memberMemberName,typeName,memberName,e.ContextualErrorMessage),m))  
                )
                
                mi.PApplyNoFailure(fun _ -> recover)

    let DisplayNameOfTypeProvider(resolver:Tainted<ITypeProvider>, m:range) =
        resolver.PUntaint((fun tp -> tp.GetType().Name),m)

    let isWhitespace (s:string) = s |> Seq.forall (fun c -> Char.IsWhiteSpace(c))
    
    let private validateNamespaceName(name,typeProvider:Tainted<ITypeProvider>,m,``namespace``:string) =
        if ``namespace``<>null then // Null namespace designates the global namespace.
            if isWhitespace(``namespace``) then
                // Empty namespace is not allowed
                errorR(Error(FSComp.SR.etEmptyNamespaceOfTypeNotAllowed(name,typeProvider.PUntaint((fun tp -> tp.GetType().Name),m)),m))
            else
                for s in ``namespace``.Split([|'.'|]) do
                    match s.IndexOfAny(PrettyNaming.IllegalCharactersInTypeAndNamespaceNames) with
                    | -1 -> ()
                    | n -> errorR(Error(FSComp.SR.etIllegalCharactersInNamespaceName(string s.[n],s),m))  


    let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public

    // NOTE: for the purposes of remapping the closure of generated types, the FullName is sufficient.
    // We do _not_ rely on object identity or any other notion of equivalence provided by System.Type itself.

    let providedSystemTypeComparer = 
        let key (ty:System.Type) = (ty.Assembly.FullName, ty.FullName)
        { new IEqualityComparer<Type> with 
            member __.GetHashCode(ty:Type) = hash (key ty)
            member __.Equals(ty1:Type,ty2:Type) = (key ty1 = key ty2) }

     /// The context used to interpret information in the closure of System.Type, System.MethodInfo and other 
     /// info objects coming from the type provider.
     ///
     /// At the moment this is the "Type --> Tycon" remapping context of the type. This is only present if  generated, and contains
     /// all the entries in the remappings for the [<Generate>] declaration containing the definition.
     ///
     /// At later points this could be expanded to contain more context information specific to the [<Generate>] declaration 
     /// if needed.
     ///
     /// (Lazy to prevent needless computation for every type during remapping.)
    type ProvidedTypeContext = 
        | NoEntries
        | Entries of Dictionary<System.Type,ILTypeRef> * Lazy<Dictionary<System.Type,obj>>
        static member Empty = NoEntries
        static member Create(d1,d2) = Entries(d1,notlazy d2)
        member ctxt.TryGetILTypeRef st = 
            match ctxt with 
            | NoEntries -> None 
            | Entries(d,_) -> 
                let mutable res = Unchecked.defaultof<_>
                if d.TryGetValue(st,&res) then Some res else None
        member ctxt.TryGetTyconRef(st) = 
            match ctxt with 
            | NoEntries -> None 
            | Entries(_,d) -> 
                let d = d.Force()
                let mutable res = Unchecked.defaultof<_>
                if d.TryGetValue(st,&res) then Some res else None

        member ctxt.RemapTyconRefs (f:obj->obj) = 
            match ctxt with 
            | NoEntries -> NoEntries
            | Entries(d1,d2) ->
                Entries(d1, lazy (let dict = new Dictionary<System.Type,obj>(providedSystemTypeComparer)
                                  for KeyValue (st, tcref) in d2.Force() do dict.Add(st, f tcref)
                                  dict))

#if FX_NO_CUSTOMATTRIBUTEDATA
    type CustomAttributeData = Microsoft.FSharp.Core.CompilerServices.IProvidedCustomAttributeData
    type CustomAttributeNamedArgument = Microsoft.FSharp.Core.CompilerServices.IProvidedCustomAttributeNamedArgument
    type CustomAttributeTypedArgument = Microsoft.FSharp.Core.CompilerServices.IProvidedCustomAttributeTypedArgument
#endif


    [<AllowNullLiteral; Sealed>]
    type ProvidedType private (x:System.Type, ctxt: ProvidedTypeContext) =
        inherit ProvidedMemberInfo(x,ctxt)
#if FX_NO_CUSTOMATTRIBUTEDATA
        let provide () = ProvidedCustomAttributeProvider.Create (fun provider -> provider.GetMemberCustomAttributesData(x))
#else
        let provide () = ProvidedCustomAttributeProvider.Create (fun _provider -> x.GetCustomAttributesData())
#endif
        interface IProvidedCustomAttributeProvider with 
            member __.GetHasTypeProviderEditorHideMethodsAttribute(provider) = provide().GetHasTypeProviderEditorHideMethodsAttribute(provider)
            member __.GetDefinitionLocationAttribute(provider) = provide().GetDefinitionLocationAttribute(provider)
            member __.GetXmlDocAttributes(provider) = provide().GetXmlDocAttributes(provider)
        
        // The type provider spec distinguishes between 
        //   - calls that can be made on provided types (i.e. types given by ReturnType, ParameterType, and generic argument types)
        //   - calls that can be made on provided type definitions (types returned by ResolveTypeName, GetTypes etc.)
        // Ideally we would enforce this decision structurally by having both ProvidedType and ProvidedTypeDefinition.
        // Alternatively we could use assertions to enforce this.

        // Suppress relocation of generated types
        member __.IsSuppressRelocate = (x.Attributes &&& enum (int32 TypeProviderTypeAttributes.SuppressRelocate)) <> enum 0  
        member __.IsErased = (x.Attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased)) <> enum 0  
        member __.IsGenericType = x.IsGenericType
        member __.Namespace = x.Namespace
        member pt.FullName = x.FullName
        member __.IsArray = x.IsArray
        member __.Assembly = x.Assembly |> ProvidedAssembly.Create ctxt
        member __.GetInterfaces() = x.GetInterfaces() |> ProvidedType.CreateArray ctxt
        member __.GetMethods() = x.GetMethods(bindingFlags) |> ProvidedMethodInfo.CreateArray ctxt
        member __.GetEvents() = x.GetEvents(bindingFlags) |> ProvidedEventInfo.CreateArray ctxt
        member __.GetEvent nm = x.GetEvent(nm, bindingFlags) |> ProvidedEventInfo.Create ctxt
        member __.GetProperties() = x.GetProperties(bindingFlags) |> ProvidedPropertyInfo.CreateArray ctxt
        member __.GetProperty nm = x.GetProperty(nm, bindingFlags) |> ProvidedPropertyInfo.Create ctxt
        member __.GetConstructors() = x.GetConstructors(bindingFlags) |> ProvidedConstructorInfo.CreateArray ctxt
        member __.GetFields() = x.GetFields(bindingFlags) |> ProvidedFieldInfo.CreateArray ctxt
        member __.GetField nm = x.GetField(nm, bindingFlags) |> ProvidedFieldInfo.Create ctxt
        member __.GetAllNestedTypes() = x.GetNestedTypes(bindingFlags ||| System.Reflection.BindingFlags.NonPublic) |> ProvidedType.CreateArray ctxt
        member __.GetNestedTypes() = x.GetNestedTypes(bindingFlags) |> ProvidedType.CreateArray ctxt
        /// Type.GetNestedType(string) can return null if there is no nested type with given name
        member __.GetNestedType nm = x.GetNestedType (nm, bindingFlags) |> ProvidedType.Create ctxt
        /// Type.GetGenericTypeDefinition() either returns type or throws exception, null is not permitted
        member __.GetGenericTypeDefinition() = x.GetGenericTypeDefinition() |> ProvidedType.CreateWithNullCheck ctxt "GenericTypeDefinition"
        /// Type.BaseType can be null when Type is interface or object
        member __.BaseType = x.BaseType |> ProvidedType.Create ctxt
        member __.GetStaticParameters(provider: ITypeProvider) = provider.GetStaticParameters(x) |> ProvidedParameterInfo.CreateArray ctxt
        /// Type.GetElementType can be null if i.e. Type is not array\pointer\byref type
        member __.GetElementType() = x.GetElementType() |> ProvidedType.Create ctxt
        member __.GetGenericArguments() = x.GetGenericArguments() |> ProvidedType.CreateArray ctxt
        member __.ApplyStaticArguments(provider: ITypeProvider, fullTypePathAfterArguments, staticArgs: obj[]) = 
            provider.ApplyStaticArguments(x, fullTypePathAfterArguments,  staticArgs) |> ProvidedType.Create ctxt
        member __.IsVoid = (typeof<System.Void>.Equals(x))
        member __.IsGenericParameter = x.IsGenericParameter
        member __.IsValueType = x.IsValueType
        member __.IsByRef = x.IsByRef
        member __.IsPointer = x.IsPointer
        member __.IsPublic = x.IsPublic
        member __.IsNestedPublic = x.IsNestedPublic
        member __.IsEnum = x.IsEnum
        member __.IsClass = x.IsClass
        member __.IsSealed = x.IsSealed
        member __.IsInterface = x.IsInterface
        member __.GetArrayRank() = x.GetArrayRank()
        member __.GenericParameterPosition = x.GenericParameterPosition
        member __.RawSystemType = x
        /// Type.GetEnumUnderlyingType either returns type or raises exception, null is not permitted
        member __.GetEnumUnderlyingType() = 
#if SILVERLIGHT
            x.UnderlyingSystemType
#else
            x.GetEnumUnderlyingType() 
#endif
            |> ProvidedType.CreateWithNullCheck ctxt "EnumUnderlyingType"
        static member Create ctxt x = match x with null -> null | t -> ProvidedType (t,ctxt)
        static member CreateWithNullCheck ctxt name x = match x with null -> nullArg name | t -> ProvidedType (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedType.Create ctxt)
        static member CreateNoContext (x:Type) = ProvidedType.Create ProvidedTypeContext.Empty x
        static member Void = ProvidedType.CreateNoContext typeof<System.Void>
        member __.Handle = x
        override __.Equals y = assert false; match y with :? ProvidedType as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()
        member __.TryGetILTypeRef() = ctxt.TryGetILTypeRef x
        member __.TryGetTyconRef() = ctxt.TryGetTyconRef x
        member __.Context = ctxt
        static member ApplyContext (pt:ProvidedType, ctxt) = ProvidedType(pt.Handle, ctxt)
        static member TaintedEquals (pt1:Tainted<ProvidedType>, pt2:Tainted<ProvidedType>) = 
           Tainted.EqTainted (pt1.PApplyNoFailure(fun st -> st.Handle)) (pt2.PApplyNoFailure(fun st -> st.Handle))

    and [<AllowNullLiteral>] 
        IProvidedCustomAttributeProvider =
        abstract GetDefinitionLocationAttribute : provider:ITypeProvider -> (string * int * int) option 
        abstract GetXmlDocAttributes : provider:ITypeProvider -> string[]
        abstract GetHasTypeProviderEditorHideMethodsAttribute : provider:ITypeProvider -> bool
        abstract GetAttributeConstructorArgs: provider:ITypeProvider * attribName:string -> obj option list option

    and ProvidedCustomAttributeProvider =
        static member Create (attributes :(ITypeProvider -> System.Collections.Generic.IList<CustomAttributeData>)) : IProvidedCustomAttributeProvider = 
            let (|Member|_|) (s:string) (x: CustomAttributeNamedArgument) = if x.MemberInfo.Name = s then Some x.TypedValue else None
            let (|Arg|_|) (x: CustomAttributeTypedArgument) = match x.Value with null -> None | v -> Some v
            let findAttribByName tyFullName (a:CustomAttributeData) = (a.Constructor.DeclaringType.FullName = tyFullName)  
            let findAttrib (ty:System.Type) a = findAttribByName ty.FullName a
            { new IProvidedCustomAttributeProvider with 
                  member __.GetAttributeConstructorArgs (provider,attribName) = 
                      attributes(provider) 
                        |> Seq.tryFind (findAttribByName  attribName)  
                        |> Option.map (fun a -> 
                            a.ConstructorArguments 
                            |> Seq.toList 
                            |> List.map (function Arg null -> None | Arg obj -> Some obj | _ -> None))

                  member __.GetHasTypeProviderEditorHideMethodsAttribute provider = 
                      attributes(provider) 
                        |> Seq.exists (findAttrib typeof<Microsoft.FSharp.Core.CompilerServices.TypeProviderEditorHideMethodsAttribute>) 

                  member __.GetDefinitionLocationAttribute(provider) = 
                      attributes(provider) 
                        |> Seq.tryFind (findAttrib  typeof<Microsoft.FSharp.Core.CompilerServices.TypeProviderDefinitionLocationAttribute>)  
                        |> Option.map (fun a -> 
                               (defaultArg (a.NamedArguments |> Seq.tryPick (function Member "FilePath" (Arg (:? string as v)) -> Some v | _ -> None)) null,
                                defaultArg (a.NamedArguments |> Seq.tryPick (function Member "Line" (Arg (:? int as v)) -> Some v | _ -> None)) 0,
                                defaultArg (a.NamedArguments |> Seq.tryPick (function Member "Column" (Arg (:? int as v)) -> Some v | _ -> None)) 0))

                  member __.GetXmlDocAttributes(provider) = 
                      attributes(provider) 
                        |> Seq.choose (fun a -> 
                             if findAttrib  typeof<Microsoft.FSharp.Core.CompilerServices.TypeProviderXmlDocAttribute> a then 
                                match a.ConstructorArguments |> Seq.toList with 
                                | [ Arg(:? string as s) ] -> Some s
                                | _ -> None
                             else 
                                None) 
                        |> Seq.toArray  }

    and [<AllowNullLiteral; AbstractClass>] 
        ProvidedMemberInfo (x: System.Reflection.MemberInfo, ctxt) = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        let provide () = ProvidedCustomAttributeProvider.Create (fun provider -> provider.GetMemberCustomAttributesData(x))
#else
        let provide () = ProvidedCustomAttributeProvider.Create (fun _provider -> x.GetCustomAttributesData())
#endif
        member __.Name = x.Name
        /// DeclaringType can be null if MemberInfo belongs to Module, not to Type
        member __.DeclaringType = ProvidedType.Create ctxt x.DeclaringType
        interface IProvidedCustomAttributeProvider with 
            member __.GetHasTypeProviderEditorHideMethodsAttribute(provider) = provide().GetHasTypeProviderEditorHideMethodsAttribute(provider)
            member __.GetDefinitionLocationAttribute(provider) = provide().GetDefinitionLocationAttribute(provider)
            member __.GetXmlDocAttributes(provider) = provide().GetXmlDocAttributes(provider)
            member __.GetAttributeConstructorArgs (provider,attribName) = provide().GetAttributeConstructorArgs (provider,attribName)

    and [<AllowNullLiteral; Sealed>] 
        ProvidedParameterInfo private (x: System.Reflection.ParameterInfo, ctxt) = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        let provide () = ProvidedCustomAttributeProvider.Create (fun provider -> provider.GetParameterCustomAttributesData(x))
#else
        let provide () = ProvidedCustomAttributeProvider.Create (fun _provider -> x.GetCustomAttributesData())
#endif
        member __.Name = x.Name
        member __.IsOut = x.IsOut
#if FX_NO_ISIN_ON_PARAMETER_INFO 
        member __.IsIn = not x.IsOut
#else
        member __.IsIn = x.IsIn
#endif
        member __.IsOptional = x.IsOptional
        member __.RawDefaultValue = x.RawDefaultValue
        /// ParameterInfo.ParameterType cannot be null
        member __.ParameterType = ProvidedType.CreateWithNullCheck ctxt "ParameterType" x.ParameterType 
        static member Create ctxt x = match x with null -> null | t -> ProvidedParameterInfo (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedParameterInfo.Create ctxt)  // TODO null wrong?
        interface IProvidedCustomAttributeProvider with 
            member __.GetHasTypeProviderEditorHideMethodsAttribute(provider) = provide().GetHasTypeProviderEditorHideMethodsAttribute(provider)
            member __.GetDefinitionLocationAttribute(provider) = provide().GetDefinitionLocationAttribute(provider)
            member __.GetXmlDocAttributes(provider) = provide().GetXmlDocAttributes(provider)
            member __.GetAttributeConstructorArgs (provider,attribName) = provide().GetAttributeConstructorArgs (provider,attribName)
        member __.Handle = x
        override __.Equals y = assert false; match y with :? ProvidedParameterInfo as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()

    and [<AllowNullLiteral; Sealed>] 
        ProvidedAssembly private (x: System.Reflection.Assembly, _ctxt) = 
#if SILVERLIGHT
        member __.GetName() = System.Reflection.AssemblyName(x.FullName)
#else
        member __.GetName() = x.GetName()
#endif
        member __.FullName = x.FullName
        member __.GetManifestModuleContents(provider: ITypeProvider) = provider.GetGeneratedAssemblyContents(x)
        static member Create ctxt x = match x with null -> null | t -> ProvidedAssembly (t,ctxt)
        member __.Handle = x
        override __.Equals y = assert false; match y with :? ProvidedAssembly as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()

    and [<AllowNullLiteral; AbstractClass>] 
        ProvidedMethodBase (x: System.Reflection.MethodBase, ctxt) = 
        inherit ProvidedMemberInfo(x, ctxt)
        member __.Context = ctxt
        member __.IsGenericMethod = x.IsGenericMethod
        member __.IsStatic  = x.IsStatic
        member __.IsFamily  = x.IsFamily
        member __.IsFamilyOrAssembly = x.IsFamilyOrAssembly
        member __.IsFamilyAndAssembly = x.IsFamilyAndAssembly
        member __.IsVirtual  = x.IsVirtual
        member __.IsFinal = x.IsFinal
        member __.IsPublic = x.IsPublic
        member __.IsAbstract  = x.IsAbstract
        member __.IsHideBySig = x.IsHideBySig
        member __.IsConstructor  = x.IsConstructor
        member __.GetParameters() = x.GetParameters() |> ProvidedParameterInfo.CreateArray ctxt 
        member __.GetGenericArguments() = x.GetGenericArguments() |> ProvidedType.CreateArray ctxt
        member __.Handle = x
        static member TaintedGetHashCode (x:Tainted<ProvidedMethodBase>) =            
           Tainted.GetHashCodeTainted (x.PApplyNoFailure(fun st -> (st.Name, st.DeclaringType.Assembly.FullName, st.DeclaringType.FullName))) 
        static member TaintedEquals (pt1:Tainted<ProvidedMethodBase>, pt2:Tainted<ProvidedMethodBase>) = 
           Tainted.EqTainted (pt1.PApplyNoFailure(fun st -> st.Handle)) (pt2.PApplyNoFailure(fun st -> st.Handle))

    and [<AllowNullLiteral; Sealed>] 
        ProvidedFieldInfo private (x: System.Reflection.FieldInfo,ctxt) = 
        inherit ProvidedMemberInfo(x,ctxt)
        static member Create ctxt x = match x with null -> null | t -> ProvidedFieldInfo (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedFieldInfo.Create ctxt)
        member __.IsInitOnly = x.IsInitOnly
        member __.IsStatic = x.IsStatic
        member __.IsSpecialName = x.IsSpecialName
        member __.IsLiteral = x.IsLiteral
        member __.GetRawConstantValue() = x.GetRawConstantValue()
        /// FieldInfo.FieldType cannot be null
        member __.FieldType = x.FieldType |> ProvidedType.CreateWithNullCheck ctxt "FieldType" 
        member __.Handle = x
        member __.IsPublic = x.IsPublic
        member __.IsFamily = x.IsFamily
        member __.IsPrivate = x.IsPrivate
        member __.IsFamilyOrAssembly = x.IsFamilyOrAssembly
        member __.IsFamilyAndAssembly = x.IsFamilyAndAssembly
        override __.Equals y = assert false; match y with :? ProvidedFieldInfo as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()

    and [<AllowNullLiteral; Sealed>] 
        ProvidedMethodInfo private (x: System.Reflection.MethodInfo, ctxt) = 
        inherit ProvidedMethodBase(x,ctxt)
        /// MethodInfo.ReturnType cannot be null
        member __.ReturnType = x.ReturnType |> ProvidedType.CreateWithNullCheck ctxt "ReturnType"
        static member Create ctxt x = match x with null -> null | t -> ProvidedMethodInfo (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedMethodInfo.Create ctxt)
        member __.Handle = x
        member __.MetadataToken = x.MetadataToken
        override __.Equals y = assert false; match y with :? ProvidedMethodInfo as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()

    and [<AllowNullLiteral; Sealed>] 
        ProvidedPropertyInfo private (x: System.Reflection.PropertyInfo,ctxt) = 
        inherit ProvidedMemberInfo(x,ctxt)
        member __.GetGetMethod() = x.GetGetMethod() |> ProvidedMethodInfo.Create ctxt
        member __.GetSetMethod() = x.GetSetMethod() |> ProvidedMethodInfo.Create ctxt
        member __.CanRead = x.CanRead
        member __.CanWrite = x.CanWrite
        member __.GetIndexParameters() = x.GetIndexParameters() |> ProvidedParameterInfo.CreateArray ctxt
        /// PropertyInfo.PropertyType cannot be null
        member __.PropertyType = x.PropertyType |> ProvidedType.CreateWithNullCheck ctxt "PropertyType"
        static member Create ctxt x = match x with null -> null | t -> ProvidedPropertyInfo (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedPropertyInfo.Create ctxt)
        member __.Handle = x
        override __.Equals y = assert false; match y with :? ProvidedPropertyInfo as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()
        static member TaintedGetHashCode (x:Tainted<ProvidedPropertyInfo>) = 
           Tainted.GetHashCodeTainted (x.PApplyNoFailure(fun st -> (st.Name, st.DeclaringType.Assembly.FullName, st.DeclaringType.FullName))) 
        static member TaintedEquals (pt1:Tainted<ProvidedPropertyInfo>, pt2:Tainted<ProvidedPropertyInfo>) = 
           Tainted.EqTainted (pt1.PApplyNoFailure(fun st -> st.Handle)) (pt2.PApplyNoFailure(fun st -> st.Handle))

    and [<AllowNullLiteral; Sealed>] 
        ProvidedEventInfo private (x: System.Reflection.EventInfo,ctxt) = 
        inherit ProvidedMemberInfo(x,ctxt)
        member __.GetAddMethod() = x.GetAddMethod() |> ProvidedMethodInfo.Create  ctxt
        member __.GetRemoveMethod() = x.GetRemoveMethod() |> ProvidedMethodInfo.Create ctxt
        /// EventInfo.EventHandlerType cannot be null
        member __.EventHandlerType = x.EventHandlerType |> ProvidedType.CreateWithNullCheck ctxt "EventHandlerType"
        static member Create ctxt x = match x with null -> null | t -> ProvidedEventInfo (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedEventInfo.Create ctxt)
        member __.Handle = x
        override __.Equals y = assert false; match y with :? ProvidedEventInfo as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()
        static member TaintedGetHashCode (x:Tainted<ProvidedEventInfo>) = 
           Tainted.GetHashCodeTainted (x.PApplyNoFailure(fun st -> (st.Name, st.DeclaringType.Assembly.FullName, st.DeclaringType.FullName))) 
        static member TaintedEquals (pt1:Tainted<ProvidedEventInfo>, pt2:Tainted<ProvidedEventInfo>) = 
           Tainted.EqTainted (pt1.PApplyNoFailure(fun st -> st.Handle)) (pt2.PApplyNoFailure(fun st -> st.Handle))

    and [<AllowNullLiteral; Sealed>] 
        ProvidedConstructorInfo private (x: System.Reflection.ConstructorInfo, ctxt) = 
        inherit ProvidedMethodBase(x,ctxt)
        static member Create ctxt x = match x with null -> null | t -> ProvidedConstructorInfo (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedConstructorInfo.Create ctxt)
        member __.Handle = x
        override __.Equals y = assert false; match y with :? ProvidedConstructorInfo as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = assert false; x.GetHashCode()

    [<RequireQualifiedAccess; Class; AllowNullLiteral; Sealed>]
    type ProvidedExpr private (x:Quotations.Expr, ctxt) =
        member __.Type = x.Type |> ProvidedType.Create ctxt
        member __.Handle = x
        member __.Context = ctxt
        member __.UnderlyingExpressionString = x.ToString()
        static member Create ctxt t = match box t with null -> null | _ -> ProvidedExpr (t,ctxt)
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedExpr.Create ctxt)
        override __.Equals y = match y with :? ProvidedExpr as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = x.GetHashCode()

    [<RequireQualifiedAccess; Class; AllowNullLiteral; Sealed>]
    type ProvidedVar private (x:Quotations.Var, ctxt) =
        member __.Type = x.Type |> ProvidedType.Create ctxt
        member __.Name = x.Name
        member __.IsMutable = x.IsMutable
        member __.Handle = x
        member __.Context = ctxt
        static member Create ctxt t = match box t with null -> null | _ -> ProvidedVar (t,ctxt)
        static member Fresh (nm,ty:ProvidedType) = ProvidedVar.Create ty.Context (new Quotations.Var(nm,ty.Handle))
        static member CreateArray ctxt xs = match xs with null -> null | _ -> xs |> Array.map (ProvidedVar.Create ctxt)
        override __.Equals y = match y with :? ProvidedVar as y -> x.Equals y.Handle | _ -> false
        override __.GetHashCode() = x.GetHashCode()


    let (|ProvidedNewObjectExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.NewObject(ctor,args)  -> 
            Some (ProvidedConstructorInfo.Create x.Context ctor, [| for a in args -> ProvidedExpr.Create x.Context a |])
        | _ -> None

    let (|ProvidedWhileLoopExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.WhileLoop(guardExpr,bodyExpr)  -> 
            Some (ProvidedExpr.Create x.Context guardExpr,ProvidedExpr.Create x.Context bodyExpr)
        | _ -> None

    let (|ProvidedNewDelegateExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.NewDelegate(ty,vs,expr)  -> 
            Some (ProvidedType.Create x.Context ty,ProvidedVar.CreateArray x.Context (List.toArray vs), ProvidedExpr.Create x.Context expr)
        | _ -> None

    let (|ProvidedCallExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Call(objOpt,meth,args) -> 
            Some ((match objOpt with None -> None | Some obj -> Some (ProvidedExpr.Create  x.Context obj)),
                  ProvidedMethodInfo.Create x.Context meth,
                  [| for a in args -> ProvidedExpr.Create  x.Context a |])
        | _ -> None

    let (|ProvidedDefaultExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.DefaultValue ty   -> Some (ProvidedType.Create x.Context ty)
        | _ -> None

    let (|ProvidedConstantExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Value(obj,ty)   -> Some (obj, ProvidedType.Create x.Context ty)
        | _ -> None

    let (|ProvidedTypeAsExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Coerce(arg,ty) -> Some (ProvidedExpr.Create x.Context arg, ProvidedType.Create  x.Context ty)
        | _ -> None

    let (|ProvidedNewTupleExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.NewTuple(args) -> Some (ProvidedExpr.CreateArray x.Context (Array.ofList args))
        | _ -> None

    let (|ProvidedTupleGetExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.TupleGet(arg,n) -> Some (ProvidedExpr.Create x.Context arg, n)
        | _ -> None

    let (|ProvidedNewArrayExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.NewArray(ty,args) -> Some (ProvidedType.Create  x.Context ty, ProvidedExpr.CreateArray x.Context (Array.ofList args))
        | _ -> None

    let (|ProvidedSequentialExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Sequential(e1,e2) -> Some (ProvidedExpr.Create x.Context e1, ProvidedExpr.Create x.Context e2)
        | _ -> None

    let (|ProvidedLambdaExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Lambda(v,body) -> Some (ProvidedVar.Create x.Context v,  ProvidedExpr.Create x.Context body)
        | _ -> None

    let (|ProvidedTryFinallyExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.TryFinally(b1,b2) -> Some (ProvidedExpr.Create x.Context b1, ProvidedExpr.Create x.Context b2)
        | _ -> None

    let (|ProvidedTryWithExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.TryWith(b,v1,e1,v2,e2) -> Some (ProvidedExpr.Create x.Context b, ProvidedVar.Create x.Context v1, ProvidedExpr.Create x.Context e1, ProvidedVar.Create x.Context v2, ProvidedExpr.Create x.Context e2)
        | _ -> None

#if PROVIDED_ADDRESS_OF
    let (|ProvidedAddressOfExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.AddressOf(e) -> Some (ProvidedExpr.Create x.Context e)
        | _ -> None
#endif

    let (|ProvidedTypeTestExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.TypeTest(e,ty) -> Some (ProvidedExpr.Create x.Context e, ProvidedType.Create x.Context ty)
        | _ -> None

    let (|ProvidedLetExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Let(v,e,b) -> Some (ProvidedVar.Create x.Context v, ProvidedExpr.Create x.Context e, ProvidedExpr.Create x.Context b)
        | _ -> None


    let (|ProvidedForIntegerRangeLoopExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.ForIntegerRangeLoop (v,e1,e2,e3) -> 
            Some (ProvidedVar.Create x.Context v, 
                  ProvidedExpr.Create x.Context e1, 
                  ProvidedExpr.Create x.Context e2, 
                  ProvidedExpr.Create x.Context e3)
        | _ -> None

    let (|ProvidedVarSetExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.VarSet(v,e) -> Some (ProvidedVar.Create x.Context v, ProvidedExpr.Create x.Context e)
        | _ -> None


    //Quotations.Patterns.WhileLoop

    let (|ProvidedIfThenElseExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.IfThenElse(g,t,e) ->  Some (ProvidedExpr.Create x.Context g, ProvidedExpr.Create x.Context t, ProvidedExpr.Create x.Context e)
        | _ -> None

    let (|ProvidedVarExpr|_|) (x:ProvidedExpr) = 
        match x.Handle with 
        |  Quotations.Patterns.Var v  -> Some (ProvidedVar.Create x.Context v)
        | _ -> None

    let GetInvokerExpression (provider: ITypeProvider, methodBase: ProvidedMethodBase, paramExprs: ProvidedVar[]) = 
        provider.GetInvokerExpression(methodBase.Handle,[| for p in paramExprs -> Quotations.Expr.Var(p.Handle) |]) |> ProvidedExpr.Create methodBase.Context

    let private computeName(m,st:Tainted<ProvidedType>,proj,propertyString) =
        let name = 
            try st.PUntaint(proj,m) 
            with :? TypeProviderError as tpe -> 
                let newError = tpe.MapText((fun msg -> FSComp.SR.etProvidedTypeWithNameException(propertyString, msg)), st.TypeProviderDesignation, m)
                raise newError
        if System.String.IsNullOrEmpty name then
            raise <| TypeProviderError(FSComp.SR.etProvidedTypeWithNullOrEmptyName(propertyString), st.TypeProviderDesignation, m)
        name

    /// Verify that this type provider has supported attributes
    let private validateAttributesOfProvidedType(m,st:Tainted<ProvidedType>) =         
        let fullName = computeName(m, st, (fun st -> st.FullName), "FullName")
        if tryTypeMember(st,fullName,"IsGenericType", m, false, fun st->st.IsGenericType) |> unmarshal then  
            errorR(Error(FSComp.SR.etMustNotBeGeneric(fullName),m))  
        if tryTypeMember(st,fullName,"IsArray", m, false, fun st->st.IsArray) |> unmarshal then 
            errorR(Error(FSComp.SR.etMustNotBeAnArray(fullName),m))  
        tryTypeMemberNonNull(st, fullName,"GetInterfaces", m, [||], fun st -> st.GetInterfaces()) |> ignore

    let private validateExpectedName m expectedPath expectedName (st : Tainted<ProvidedType>) =
        let name = computeName(m, st, (fun st -> st.Name), "Name")
        if name <> expectedName then
            raise <| TypeProviderError(FSComp.SR.etProvidedTypeHasUnexpectedName(expectedName,name), st.TypeProviderDesignation, m)

        let namespaceName = tryTypeMember(st, name,"Namespace",m,"",fun st -> st.Namespace) |> unmarshal
        let rec declaringTypes (st:Tainted<ProvidedType>) accu =
            match tryTypeMember(st,name,"DeclaringType",m,null,fun st -> st.DeclaringType) with
            |   Tainted.Null -> accu
            |   dt -> declaringTypes dt (computeName(m, dt, (fun dt -> dt.Name), "Name")::accu)
        let path = 
            [|  match namespaceName with 
                | null -> ()
                | _ -> yield! namespaceName.Split([|'.'|])
                yield! declaringTypes st [] |]
        
        if path <> expectedPath then
            let expectedPath = String.Join(".",expectedPath)
            let path = String.Join(".",path)
            errorR(Error(FSComp.SR.etProvidedTypeHasUnexpectedPath(expectedPath,path), m))

    let ValidateProvidedTypeAfterStaticInstantiation(m,st:Tainted<ProvidedType>, expectedPath : string[], expectedName : string) = 
        // Do all the calling into st up front with recovery
        let fullName, namespaceName, usedMembers =
            let name = computeName(m, st, (fun st -> st.Name), "Name")
            let namespaceName = tryTypeMember(st,name,"Namespace",m,FSComp.SR.invalidNamespaceForProvidedType(),fun st -> st.Namespace) |> unmarshal
            let fullName = tryTypeMemberNonNull(st,name,"FullName",m,FSComp.SR.invalidFullNameForProvidedType(),fun st -> st.FullName) |> unmarshal
            validateExpectedName m expectedPath expectedName st
            // Must be able to call (GetMethods|GetEvents|GetPropeties|GetNestedTypes|GetConstructors)(bindingFlags).
            let usedMembers : Tainted<ProvidedMemberInfo>[] = 
                // These are the members the compiler will actually use
                [| for x in tryTypeMemberArray(st,fullName,"GetMethods",m,fun st -> st.GetMethods()) -> x.Coerce(m)
                   for x in tryTypeMemberArray(st,fullName,"GetEvents",m,fun st -> st.GetEvents()) -> x.Coerce(m)
                   for x in tryTypeMemberArray(st,fullName,"GetFields",m,fun st -> st.GetFields()) -> x.Coerce(m)
                   for x in tryTypeMemberArray(st,fullName,"GetProperties",m,fun st -> st.GetProperties()) -> x.Coerce(m)
                   // These will be validated on-demand
                   //for x in tryTypeMemberArray(st,fullName,"GetNestedTypes",m,fun st -> st.GetNestedTypes(bindingFlags)) -> x.Coerce()
                   for x in tryTypeMemberArray(st,fullName,"GetConstructors",m,fun st -> st.GetConstructors()) -> x.Coerce(m) |]
            fullName, namespaceName, usedMembers       

        // We scrutinize namespaces for invalid characters on open, but this provides better diagnostics
        validateNamespaceName(fullName,st.TypeProvider,m,namespaceName)

        validateAttributesOfProvidedType(m,st)

        // Those members must have this type.
        // This needs to be a *shallow* exploration. Otherwise, as in Freebase sample the entire database could be explored.
        for mi in usedMembers do
            match mi with 
            | Tainted.Null -> errorR(Error(FSComp.SR.etNullMember(fullName),m))  
            | _ -> 
                let memberName = tryMemberMember(mi,fullName,"Name","Name",m,"invalid provided type member name",fun mi -> mi.Name) |> unmarshal
                if String.IsNullOrEmpty(memberName) then 
                    errorR(Error(FSComp.SR.etNullOrEmptyMemberName(fullName),m))  
                else 
                    let miDeclaringType = tryMemberMember(mi,fullName,memberName,"DeclaringType",m,ProvidedType.CreateNoContext(typeof<obj>),fun mi -> mi.DeclaringType)
                    match miDeclaringType with 
                        // Generated nested types may have null DeclaringType
                    | Tainted.Null when (mi.OfType<ProvidedType>().IsSome) -> ()
                    | Tainted.Null -> 
                        errorR(Error(FSComp.SR.etNullMemberDeclaringType(fullName,memberName),m))   
                    | _ ->     
                        let miDeclaringTypeFullName = 
                            tryMemberMember(miDeclaringType,fullName,memberName,"FullName",m,"invalid declaring type full name",fun miDeclaringType -> miDeclaringType.FullName)
                            |> unmarshal
                        if not (ProvidedType.TaintedEquals (st, miDeclaringType)) then 
                            errorR(Error(FSComp.SR.etNullMemberDeclaringTypeDifferentFromProvidedType(fullName,memberName,miDeclaringTypeFullName),m))   

                    match mi.OfType<ProvidedMethodInfo>() with
                    | Some mi ->
                        let isPublic = tryMemberMember(mi,fullName,memberName,"IsPublic",m,true,fun mi->mi.IsPublic) |> unmarshal
                        let isGenericMethod = tryMemberMember(mi,fullName,memberName,"IsGenericMethod",m,true,fun mi->mi.IsGenericMethod) |> unmarshal
                        if not isPublic || isGenericMethod then
                            errorR(Error(FSComp.SR.etMethodHasRequirements(fullName,memberName),m))   
                    |   None ->
                    match mi.OfType<ProvidedType>() with
                    |   Some subType -> validateAttributesOfProvidedType(m,subType)
                    |   None ->
                    match mi.OfType<ProvidedPropertyInfo>() with
                    | Some pi ->
                        // Property must have a getter or setter
                        // TODO: Property must be public etc.
                        let expectRead =
                             match tryMemberMember(pi,fullName,memberName,"GetGetMethod",m,null,fun pi -> pi.GetGetMethod()) with 
                             |  Tainted.Null -> false 
                             | _ -> true
                        let expectWrite = 
                            match tryMemberMember(pi, fullName,memberName,"GetSetMethod",m,null,fun pi-> pi.GetSetMethod()) with 
                            |   Tainted.Null -> false 
                            |   _ -> true
                        let canRead = tryMemberMember(pi,fullName,memberName,"CanRead",m,expectRead,fun pi-> pi.CanRead) |> unmarshal
                        let canWrite = tryMemberMember(pi,fullName,memberName,"CanWrite",m,expectWrite,fun pi-> pi.CanWrite) |> unmarshal
                        match expectRead,canRead with
                        | false,false | true,true-> ()
                        | false,true -> errorR(Error(FSComp.SR.etPropertyCanReadButHasNoGetter(memberName,fullName),m))   
                        | true,false -> errorR(Error(FSComp.SR.etPropertyHasGetterButNoCanRead(memberName,fullName),m))   
                        match expectWrite,canWrite with
                        | false,false | true,true-> ()
                        | false,true -> errorR(Error(FSComp.SR.etPropertyCanWriteButHasNoSetter(memberName,fullName),m))   
                        | true,false -> errorR(Error(FSComp.SR.etPropertyHasSetterButNoCanWrite(memberName,fullName),m))   
                        if not canRead && not canWrite then 
                            errorR(Error(FSComp.SR.etPropertyNeedsCanWriteOrCanRead(memberName,fullName),m))   

                    | None ->
                    match mi.OfType<ProvidedEventInfo>() with 
                    | Some ei ->
                        // Event must have adder and remover
                        // TODO: Event must be public etc.
                        let adder = tryMemberMember(ei,fullName,memberName,"GetAddMethod",m,null,fun ei-> ei.GetAddMethod())
                        let remover = tryMemberMember(ei,fullName,memberName,"GetRemoveMethod",m,null,fun ei-> ei.GetRemoveMethod())
                        match adder, remover with
                        | Tainted.Null,_ -> errorR(Error(FSComp.SR.etEventNoAdd(memberName,fullName),m))   
                        | _,Tainted.Null -> errorR(Error(FSComp.SR.etEventNoRemove(memberName,fullName),m))   
                        | _,_ -> ()
                    | None ->
                    match mi.OfType<ProvidedConstructorInfo>() with
                    | Some _  -> () // TODO: Constructors must be public etc.
                    | None ->
                    match mi.OfType<ProvidedFieldInfo>() with
                    | Some _ -> () // TODO: Fields must be public, literals must have a value etc.
                    | None ->
                        errorR(Error(FSComp.SR.etUnsupportedMemberKind(memberName,fullName),m))   

    let ValidateProvidedTypeDefinition(m,st:Tainted<ProvidedType>, expectedPath : string[], expectedName : string) = 
        begin
            let name = computeName(m, st, (fun st -> st.Name), "Name")
            let _namespaceName = tryTypeMember(st,name,"Namespace",m,FSComp.SR.invalidNamespaceForProvidedType(),fun st -> st.Namespace) |> unmarshal
            let _fullname = tryTypeMemberNonNull(st,name,"FullName",m,FSComp.SR.invalidFullNameForProvidedType(),fun st -> st.FullName)  |> unmarshal
            validateExpectedName m expectedPath expectedName st
        end
        validateAttributesOfProvidedType(m,st)
        // This excludes, for example, types with '.' in them which would not be resolvable during name resolution.
        match expectedName.IndexOfAny(PrettyNaming.IllegalCharactersInTypeAndNamespaceNames) with
        | -1 -> ()
        | n -> errorR(Error(FSComp.SR.etIllegalCharactersInTypeName(string expectedName.[n],expectedName),m))  

        let staticParameters = st.PApplyWithProvider((fun (st,provider) -> st.GetStaticParameters(provider)), range=m) 
        if staticParameters.PUntaint((fun a -> a.Length),m)  = 0 then 
            ValidateProvidedTypeAfterStaticInstantiation(m, st, expectedPath, expectedName)



    /// Get a simple display name for the resolver.
    let private displayNameOfModuleOrNamespace(moduleOrNamespace:string array) =
        String.Join(".", moduleOrNamespace)
    
    /// Resolve a (non-nested) provided type given a full namespace name and a type name. 
    /// May throw an exception which will be turned into an error message by one of the 'Try' function below.
    /// If resolution is successful the type is then validated.
    let private resolveType(resolutionEnvironment:ResolutionEnvironment,resolver:Tainted<ITypeProvider>,m,moduleOrNamespace,typeName) =
        let displayName = displayNameOfModuleOrNamespace moduleOrNamespace
        let rec tryNamespace (providedNamespace: Tainted<IProvidedNamespace>) = 
            let resolverNamespaceName = providedNamespace.PUntaint((fun providedNamespace -> providedNamespace.NamespaceName), range=m)
            if displayName = resolverNamespaceName then
                let resolvedType = providedNamespace.PApply((fun providedNamespace -> ProvidedType.CreateNoContext(providedNamespace.ResolveTypeName typeName)), range=m) 
                match resolvedType with
                |   Tainted.Null ->
                    if resolutionEnvironment.showResolutionMessages then
                        dprintfn " resolution via GetType(typeName=%s) in %s failed" typeName displayName
                    None

                |   result -> 
                    if resolutionEnvironment.showResolutionMessages then
                        dprintfn " provided type '%s' was resolved" (result.PUntaint((fun r -> r.FullName), range=m))

                    ValidateProvidedTypeDefinition(m, result, moduleOrNamespace, typeName)
                    Some result
            else
                let providedNamespaces = providedNamespace.PApplyArray((fun providedNamespace -> providedNamespace.GetNestedNamespaces()), "GetNestedNamespaces", range=m)
                tryNamespaces providedNamespaces
        and tryNamespaces (providedNamespaces: Tainted<IProvidedNamespace>[]) = 
            providedNamespaces |> Array.tryPick tryNamespace

        let providedNamespaces = resolver.PApplyArray((fun resolver -> resolver.GetNamespaces()), "GetNamespaces", range=m)
        match tryNamespaces providedNamespaces with 
        | None -> resolver.PApply((fun _ -> null),m)
        | Some res -> res
                    
    /// Try to resolve a type against the given host with the given resolution environment.
    let TryResolveProvidedType(resolutionEnvironment:ResolutionEnvironment,resolver:Tainted<ITypeProvider>,m,moduleOrNamespace,typeName) =
        try 
            match resolveType(resolutionEnvironment,resolver,m,moduleOrNamespace,typeName) with
            | Tainted.Null -> None
            | typ -> Some typ
        with e -> 
            errorRecovery e m
            None

    let ILPathToProvidedType  (st:Tainted<ProvidedType>,m) = 
        let nameContrib (st:Tainted<ProvidedType>) = 
            let typeName = st.PUntaint((fun st -> st.Name),m)
            match st.PApply((fun st -> st.DeclaringType),m) with 
            | Tainted.Null -> 
               match st.PUntaint((fun st -> st.Namespace),m) with 
               | null -> typeName
               | ns -> ns + "." + typeName
            | _ -> typeName

        let rec encContrib (st:Tainted<ProvidedType>) = 
            match st.PApply((fun st ->st.DeclaringType),m) with 
            | Tainted.Null -> []
            | enc -> encContrib enc @ [ nameContrib enc ]

        encContrib st, nameContrib st

    /// Apply the given provided type to the given static arguments (the arguments are assumed to have been sorted into application order
    let TryApplyProvidedType(typeBeforeArguments:Tainted<ProvidedType>, (optGeneratedTypePath: string list option), staticArgs:obj[], m:range) =
        if staticArgs.Length = 0 then 
            Some (typeBeforeArguments , (fun () -> ()))
        else 
            
            let fullTypePathAfterArguments = 
                // If there is a generated type name, then use that
                match optGeneratedTypePath with 
                | Some path -> path
                | None -> 
                    // Otherwise, use the full path of the erased type, including mangled arguments
                    let nm = typeBeforeArguments.PUntaint((fun x -> x.Name),m)
                    let enc,_ = ILPathToProvidedType (typeBeforeArguments,m)
                    let defaultArgValues = 
                        typeBeforeArguments.PApplyWithProvider((fun (typeBeforeArguments,resolver) -> 
                            typeBeforeArguments.GetStaticParameters(resolver) 
                            |> Array.map (fun sp -> sp.Name, (if sp.IsOptional then Some (string sp.RawDefaultValue) else None ))),range=m)
                    let defaultArgValues = defaultArgValues.PUntaint(id,m)

                    let nonDefaultArgs = 
                        (staticArgs,defaultArgValues) 
                        ||> Array.zip 
                        |> Array.choose (fun (staticArg, (defaultArgName, defaultArgValue)) -> 
                            let actualArgValue = string  staticArg 
                            match defaultArgValue with 
                            | Some v when v = actualArgValue -> None
                            | _ -> Some (defaultArgName, actualArgValue))
                    let mangledName = PrettyNaming.mangleProvidedTypeName (nm, nonDefaultArgs)
                    enc @ [ mangledName ]
 
            match typeBeforeArguments.PApplyWithProvider((fun (typeBeforeArguments,provider) -> typeBeforeArguments.ApplyStaticArguments(provider, Array.ofList fullTypePathAfterArguments, staticArgs)),range=m) with 
            | Tainted.Null -> None
            | typeWithArguments -> 
                let actualName = typeWithArguments.PUntaint((fun x -> x.Name),m)
                let checkTypeName() = 
                    let expectedTypeNameAfterArguments = fullTypePathAfterArguments.[fullTypePathAfterArguments.Length-1]
                    if actualName <> expectedTypeNameAfterArguments then 
                        error(Error(FSComp.SR.etProvidedAppliedTypeHadWrongName(typeWithArguments.TypeProviderDesignation, expectedTypeNameAfterArguments, actualName),m))
                Some (typeWithArguments, checkTypeName)

    /// Given a mangled name reference to a non-nested provided type, resolve it.
    /// If necessary, demangle its static arguments before applying them.
    let TryLinkProvidedType(resolutionEnvironment:ResolutionEnvironment,resolver:Tainted<ITypeProvider>,moduleOrNamespace:string[],typeLogicalName:string,m:range) =
        
        // Demangle the static parameters
        let typeName, argNamesAndValues = 
            try 
                PrettyNaming.demangleProvidedTypeName typeLogicalName 
            with PrettyNaming.InvalidMangledStaticArg piece -> 
                error(Error(FSComp.SR.etProvidedTypeReferenceInvalidText(piece),range0)) 

        let argSpecsTable = dict argNamesAndValues
        let typeBeforeArguments = resolveType(resolutionEnvironment,resolver,range0,moduleOrNamespace,typeName) 

        match typeBeforeArguments with 
        | Tainted.Null -> None
        | _ -> 
            // Take the static arguments (as strings, taken from the text in the reference we're relinking),
            // and convert them to objects of the appropriate type, based on the expected kind.
            let staticParameters = typeBeforeArguments.PApplyWithProvider((fun (typeBeforeArguments,resolver) -> typeBeforeArguments.GetStaticParameters(resolver)),range=range0)

            let staticParameters = staticParameters.PApplyArray(id, "",m)
            
            let staticArgs = 
                staticParameters |> Array.map (fun sp -> 
                      let typeBeforeArgumentsName = typeBeforeArguments.PUntaint ((fun st -> st.Name),m)
                      let spName = sp.PUntaint ((fun sp -> sp.Name),m)
                      if not (argSpecsTable.ContainsKey spName) then 
                          if sp.PUntaint ((fun sp -> sp.IsOptional),m) then 
                              match sp.PUntaint((fun sp -> sp.RawDefaultValue),m) with
                              | null -> error (Error(FSComp.SR.etStaticParameterRequiresAValue (spName, typeBeforeArgumentsName, typeBeforeArgumentsName, spName),range0))
                              | v -> v
                          else
                              error(Error(FSComp.SR.etProvidedTypeReferenceMissingArgument(spName),range0))
                      else
                          let arg = argSpecsTable.[spName]
                      
                          /// Find the name of the representation type for the static parameter
                          let spReprTypeName = 
                              sp.PUntaint((fun sp -> 
                                  let pt = sp.ParameterType 
                                  let ut = pt.RawSystemType
#if SILVERLIGHT
                                  let uet = if pt.IsEnum then ut.UnderlyingSystemType else ut
#else
                                  let uet = if pt.IsEnum then ut.GetEnumUnderlyingType() else ut
#endif
                                  uet.FullName),m)

                          match spReprTypeName with 
                          | "System.SByte" -> box (sbyte arg)
                          | "System.Int16" -> box (int16 arg)
                          | "System.Int32" -> box (int32 arg)
                          | "System.Int64" -> box (int64 arg)
                          | "System.Byte" -> box (byte arg)
                          | "System.UInt16" -> box (uint16 arg)
                          | "System.UInt32" -> box (uint32 arg)
                          | "System.UInt64" -> box (uint64 arg)
                          | "System.Decimal" -> box (decimal arg)
                          | "System.Single" -> box (single arg)
                          | "System.Double" -> box (double arg)
                          | "System.Char" -> box (char arg)
                          | "System.Boolean" -> box (arg = "True")
                          | "System.String" -> box (string arg)
                          | s -> error(Error(FSComp.SR.etUnknownStaticArgumentKind(s,typeLogicalName),range0)))
            match TryApplyProvidedType(typeBeforeArguments, None, staticArgs,range0) with 
            | Some (typeWithArguments, checkTypeName) -> 
                checkTypeName() 
                Some typeWithArguments
            | None -> None

    /// Get the parts of a .NET namespace. Special rules: null means global, empty is not allowed.
    let getPartsOfDotNetNamespaceRecover(namespaceName:string) = 
        if namespaceName=null then []
        elif  namespaceName.Length = 0 then ["<NonExistentNamespace>"]
        else splitNamespace namespaceName

    /// Get the parts of a .NET namespace. Special rules: null means global, empty is not allowed.
    let GetPartsOfDotNetNamespace(m,resolver:Tainted<ITypeProvider>,namespaceName:string) = 
        if namespaceName<>null && namespaceName.Length = 0 then
            errorR(Error(FSComp.SR.etEmptyNamespaceNotAllowed(DisplayNameOfTypeProvider(resolver.TypeProvider,m)),m))  
        getPartsOfDotNetNamespaceRecover namespaceName

    /// Get the parts of the name that encloses the .NET type including nested types. 
    let GetFSharpPathToProvidedType (st:Tainted<ProvidedType>,m) = 
        // Can't use st.Fullname because it may be like IEnumerable<Something>
        // We want [System;Collections;Generic]
        let namespaceParts = getPartsOfDotNetNamespaceRecover(st.PUntaint((fun st -> st.Namespace),m))
        let rec walkUpNestedClasses(st:Tainted<ProvidedType>,soFar) =
            match st with
            | Tainted.Null -> soFar
            | st -> walkUpNestedClasses(st.PApply((fun st ->st.DeclaringType),m),soFar) @ [st.PUntaint((fun st -> st.Name),m)]
        walkUpNestedClasses(st.PApply((fun st ->st.DeclaringType),m),namespaceParts)


    let ILAssemblyRefFromProvidedAssembly (assembly:Tainted<ProvidedAssembly>, m) =
        let aname = assembly.PUntaint((fun assembly -> assembly.GetName()),m)
        ILAssemblyRef.FromAssemblyName aname

    /// Get the ILTypeRef for the provided type (including for nested types). Do not take into account
    /// any type relocations or static linking for generated types.
    let GetOriginalILTypeRefOfProvidedType (st:Tainted<ProvidedType>,m) = 
        
        let aref = ILAssemblyRefFromProvidedAssembly (st.PApply((fun st -> st.Assembly),m),m)
        let scoperef = ILScopeRef.Assembly aref
        let enc, nm = ILPathToProvidedType (st, m)
        let tref = ILTypeRef.Create(scoperef, enc, nm)
        tref

    /// Get the ILTypeRef for the provided type (including for nested types). Do not take into account
    /// any type relocations or static linking for generated types.
    let GetILTypeRefOfProvidedType (st:Tainted<ProvidedType>,m) = 
        match st.PUntaint((fun st -> st.TryGetILTypeRef()),m) with 
        | Some ilTypeRef -> ilTypeRef
        | None -> GetOriginalILTypeRefOfProvidedType (st, m)

    type ProviderGeneratedType = ProviderGeneratedType of (*ilOrigTyRef*)ILTypeRef * (*ilRenamedTyRef*)ILTypeRef * ProviderGeneratedType list

    /// The table of information recording remappings from type names in the provided assembly to type
    /// names in the statically linked, embedded assembly, plus what types are nested in side what types.
    type ProvidedAssemblyStaticLinkingMap = 
        { ILTypeMap: System.Collections.Generic.Dictionary<ILTypeRef, ILTypeRef> }
        static member CreateNew() = 
            { ILTypeMap = System.Collections.Generic.Dictionary() }

    /// Check if this is a direct reference to a non-embedded generated type. This is not permitted at any name resolution.
    /// We check by seeing if the type is absent from the remapping context.
    let IsGeneratedTypeDirectReference (st: Tainted<ProvidedType>, m) =
        st.PUntaint((fun st -> st.TryGetTyconRef() |> isNone), m)

#endif
