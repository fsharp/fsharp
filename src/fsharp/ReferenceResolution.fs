﻿namespace Viz

/// This type exists to have a concrete 'Target' type for a DebuggerVisualizerAttribute.
/// Ideally it would be out in its own assembly, but then the compiler would need to take a dependency on that assembly, so instead we 
/// pragmatically just shove this into the compiler assembly itself.
type internal Visualizable(o:obj) =
    member this.Data = o
    /// assuming this assembly is already in the debuggee process, then Viz.Visualiable.Make(foo) in the Watch window will make a visualizer for foo
    static member Make(o:obj) = new Visualizable(o)

namespace Microsoft.FSharp.Compiler

module internal MSBuildResolver = 

    open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
    exception ResolutionFailure
    
    type ResolvedFrom =
        | AssemblyFolders
        | AssemblyFoldersEx
        | TargetFrameworkDirectory
        | RawFileName
        | GlobalAssemblyCache
        | Path of string
        | Unknown
            
    type ResolutionEnvironment = CompileTimeLike | RuntimeLike | DesigntimeLike
    
#if SILVERLIGHT
#else
    open System
    open System.Reflection
    open Microsoft.Build.Tasks
    open Microsoft.Build.Utilities
    open Microsoft.Build.Framework
    open Microsoft.Build.BuildEngine
    open System.IO

    type ResolvedFile = {
            itemSpec:string
            resolvedFrom:ResolvedFrom
            fusionName:string
            version:string
            redist:string        
            baggage:string
        }
        with override this.ToString() = sprintf "ResolvedFile(%s)" this.itemSpec
    
    type ResolutionResults = {
        resolvedFiles:ResolvedFile array
        referenceDependencyPaths:string array
        relatedPaths:string array
        referenceSatellitePaths:string array
        referenceScatterPaths:string array
        referenceCopyLocalPaths:string array
        suggestedBindingRedirects:string array
        }

    let DotNetFrameworkReferenceAssembliesRootDirectory = 
        // Note that ProgramFilesX86 is correct for both x86 and x64 architectures (the reference assemblies are always in the 32-bit location, which is PF(x86) on an x64 machine)
        let PF = 
            //System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) // This API is not available to bootstrap compiler
            match System.Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
            | null -> System.Environment.GetEnvironmentVariable("ProgramFiles")  // if PFx86 is null, then we are 32-bit and just get PF
            | s -> s 
        PF + @"\Reference Assemblies\Microsoft\Framework\.NETFramework"
        
    let ReplaceFrameworkVariables(dirs) =
        let windowsFramework = System.Environment.GetEnvironmentVariable("windir")+ @"\Microsoft.NET\Framework"
        let referenceAssemblies = DotNetFrameworkReferenceAssembliesRootDirectory
        dirs|>List.map(fun (d:string)->d.Replace("{WindowsFramework}",windowsFramework).Replace("{ReferenceAssemblies}",referenceAssemblies))
               

    /// Derive the target framework directories.        
    let DeriveTargetFrameworkDirectories
                (targetFrameworkVersion:string,             // e.g. v2.0, v3.0, v3.5, v4.0 etc
                 excludeNonExecutableAssemblies:bool,       // True when the assembly must be executable and not just a stub meta assembly.
                 logmessage:string->unit) =
        let targetFrameworkVersion =
            if not(targetFrameworkVersion.StartsWith("v",StringComparison.Ordinal)) then "v"^targetFrameworkVersion
            else targetFrameworkVersion
        let FrameworkStartsWith(short) =
            targetFrameworkVersion.StartsWith(short,StringComparison.Ordinal)
        let result =
            if FrameworkStartsWith("v1.0") then ReplaceFrameworkVariables([@"{WindowsFramework}\v1.0.3705"])
            else if FrameworkStartsWith("v1.1") then ReplaceFrameworkVariables([@"{WindowsFramework}\v1.1.4322"])
            else if FrameworkStartsWith("v2.0") then ReplaceFrameworkVariables([@"{WindowsFramework}\v2.0.50727"])
            else if FrameworkStartsWith("v3.0") then ReplaceFrameworkVariables([@"{ReferenceAssemblies}\v3.0"; @"{WindowsFramework}\v3.0"; @"{WindowsFramework}\v2.0.50727"])
            else if FrameworkStartsWith("v3.5") then ReplaceFrameworkVariables([@"{ReferenceAssemblies}\v3.5"; @"{WindowsFramework}\v3.5"; @"{ReferenceAssemblies}\v3.0"; @"{WindowsFramework}\v3.0"; @"{WindowsFramework}\v2.0.50727"])
            else if FrameworkStartsWith("v4.0") then ReplaceFrameworkVariables([@"{ReferenceAssemblies}\v4.0"])  // starting with .Net 4.0, the runtime dirs (WindowsFramework) are never used by MSBuild RAR
            else if FrameworkStartsWith("v4.5") then ReplaceFrameworkVariables([@"{ReferenceAssemblies}\v4.5"])         
            else (ignore(excludeNonExecutableAssemblies); [])

        let result = result |> Array.ofList                
        logmessage (sprintf "Derived target framework directories for version %s are: %s" targetFrameworkVersion (String.Join(",", result)))                
        result    
 
    /// Decode the ResolvedFrom code from MSBuild.
    let DecodeResolvedFrom(resolvedFrom:string) : ResolvedFrom = 
        let Same a b = 
            String.CompareOrdinal(a,b) = 0            
        match resolvedFrom with
        | r when Same "{RawFileName}" r -> RawFileName
        | r when Same "{GAC}" r -> GlobalAssemblyCache
        | r when Same "{TargetFrameworkDirectory}" r -> TargetFrameworkDirectory
        | r when Same "{AssemblyFolders}" r -> AssemblyFolders
        | r when r.Length >= 10 && Same "{Registry:" (r.Substring(0,10)) -> AssemblyFoldersEx
        | r -> ResolvedFrom.Path r
        

    type ErrorWarningCallbackSig = ((*code:*)string->(*message*)string->unit)
                      
    type Foregrounded =
        | ForegroundedMessage of string 
        | ForegroundedError of string * string
        | ForegroundedWarning of string * string

    let ResolveCore(
                    resolutionEnvironment: ResolutionEnvironment,
                    references:(string*(*baggage*)string)[], 
                    targetFrameworkVersion:string, 
                    targetFrameworkDirectories:string list,
                    targetProcessorArchitecture:string,                
                    outputDirectory:string, 
                    fsharpCoreExplicitDirOrFSharpBinariesDir:string,
                    explicitIncludeDirs:string list,
                    implicitIncludeDir:string,
                    frameworkRegistryBase:string, 
                    assemblyFoldersSuffix:string, 
                    assemblyFoldersConditions:string, 
                    allowRawFileName:bool,
                    logmessage:string->unit, 
                    logwarning:ErrorWarningCallbackSig, 
                    logerror:ErrorWarningCallbackSig ) =

        // Message Foregrounding:
        //   In version 4.0 MSBuild began calling log methods on a background (non-UI) thread. If there is an exception thrown from 
        //   logmessage, logwarning or logerror then it would kill the process.
        //   The fix is to catch these exceptions and log the rest of the messages to a list to output at the end.
        //   It looks simpler to always just accumulate the messages during resolution and show them all at the end, but then 
        //   we couldn't see the messages as resolution progresses.
        let foregrounded = ref []                
        let backgroundException : exn option ref = ref None
        
        let logmessage message = 
            match !backgroundException with
            | Some _ -> foregrounded := ForegroundedMessage(message) :: !foregrounded
            | None -> 
                try 
                    logmessage message
                with e ->
                    backgroundException := Some(e)
                    foregrounded := ForegroundedMessage(message) :: !foregrounded
                
        let logwarning code message = 
            match !backgroundException with
            | Some _ -> foregrounded := ForegroundedWarning(code,message) :: !foregrounded
            | None -> 
                try 
                    logwarning code message
                with e ->
                    backgroundException := Some(e)     
                    foregrounded := ForegroundedWarning(code,message) :: !foregrounded      
                    
        let logerror code message = 
            match !backgroundException with
            | Some _ -> foregrounded := ForegroundedError(code,message) :: !foregrounded
            | None -> 
                try 
                    logerror code message
                with e ->
                    backgroundException := Some(e)     
                    foregrounded := ForegroundedError(code,message) :: !foregrounded                             
                
                
        let engine = { new IBuildEngine with 
                    member be.BuildProjectFile(projectFileName, targetNames, globalProperties, targetOutputs) = true
                    member be.LogCustomEvent(e) = logmessage e.Message
                    member be.LogErrorEvent(e) = logerror e.Code e.Message
                    member be.LogMessageEvent(e) = logmessage e.Message
                    member be.LogWarningEvent(e) = logwarning e.Code e.Message
                    member be.ColumnNumberOfTaskNode with get() = 1
                    member be.LineNumberOfTaskNode with get() = 1
                    member be.ContinueOnError with get() = true
                    member be.ProjectFileOfTaskNode with get() = "" }
                    
        let rar = new ResolveAssemblyReference()
        rar.BuildEngine <- engine
        
        // Derive target framework directory if none was supplied.
        let excludeNonExecutableAssemblies = (resolutionEnvironment = RuntimeLike)
        let targetFrameworkDirectories =
            if targetFrameworkDirectories=[] then DeriveTargetFrameworkDirectories(targetFrameworkVersion,excludeNonExecutableAssemblies,logmessage) 
            else targetFrameworkDirectories |> Array.ofList
            
        // Filter for null and zero length, and escape backslashes so legitimate path characters aren't mistaken for
        // escape characters (E.g., ".\r.dll")            
        let explicitIncludeDirs = explicitIncludeDirs |> List.filter(fun eid->not(String.IsNullOrEmpty(eid)))
        let references = references |> Array.filter(fun (path,_)->not(String.IsNullOrEmpty(path))) // |> Array.map (fun (path,baggage) -> (path.Replace("\\","\\\\"),baggage))
       
        rar.TargetFrameworkDirectories <- targetFrameworkDirectories 
        rar.FindRelatedFiles <- false
        rar.FindDependencies <- false
        rar.FindSatellites <- false
        rar.FindSerializationAssemblies <- false
#if BUILDING_WITH_LKG
        ignore targetProcessorArchitecture
#else       
        let targetedRuntimeVersionValue = typeof<obj>.Assembly.ImageRuntimeVersion
#if CROSS_PLATFORM_COMPILER 
        // The properties TargetedRuntimeVersion and CopyLocalDependenciesWhenParentReferenceInGac 
        // are not available to the cross-platform compiler since they are Windows only (not defined in the Mono  
        // 4.0 XBuild support). So we only set them if available (to avoid a compile-time dependency). 
        let runningOnMono = try System.Type.GetType("Mono.Runtime") <> null with e-> false         
        if not runningOnMono then  
            typeof<ResolveAssemblyReference>.InvokeMember("TargetedRuntimeVersion",(BindingFlags.Instance ||| BindingFlags.SetProperty ||| BindingFlags.Public),null,rar,[| box targetedRuntimeVersionValue |])  |> ignore 
            typeof<ResolveAssemblyReference>.InvokeMember("CopyLocalDependenciesWhenParentReferenceInGac",(BindingFlags.Instance ||| BindingFlags.SetProperty ||| BindingFlags.Public),null,rar,[| box true |])  |> ignore 
#else
        rar.TargetedRuntimeVersion <- targetedRuntimeVersionValue
        rar.CopyLocalDependenciesWhenParentReferenceInGac <- true
#endif
#endif        
        rar.TargetProcessorArchitecture <- targetProcessorArchitecture
        rar.Assemblies <- [|for (referenceName,baggage) in references -> 
                                        let item = new Microsoft.Build.Utilities.TaskItem(referenceName)
                                        item.SetMetadata("Baggage", baggage)
                                        item:>ITaskItem|]

        let rawFileNamePath = if allowRawFileName then ["{RawFileName}"] else []
        let searchPaths = 
            match resolutionEnvironment with
            | DesigntimeLike
            | RuntimeLike ->
                logmessage("Using scripting resolution precedence.")                      
                // These are search paths for runtime-like or scripting resolution. GAC searching is present.
                rawFileNamePath @        // Quick-resolve straight to filename first 
                explicitIncludeDirs @    // From -I, #I
                [implicitIncludeDir] @   // Usually the project directory
                [fsharpCoreExplicitDirOrFSharpBinariesDir] @    // Location of explicit reference to FSharp.Core, otherwise location of fsc.exe
                ["{TargetFrameworkDirectory}"] @
                [sprintf "{Registry:%s,%s,%s%s}" frameworkRegistryBase targetFrameworkVersion assemblyFoldersSuffix assemblyFoldersConditions] @
                ["{AssemblyFolders}"] @
                ["{GAC}"] 
            | CompileTimeLike -> 
                logmessage("Using compilation resolution precedence.")                      
                // These are search paths for compile-like resolution. GAC searching is not present.
                ["{TargetFrameworkDirectory}"] @
                rawFileNamePath @        // Quick-resolve straight to filename first
                explicitIncludeDirs @    // From -I, #I
                [implicitIncludeDir] @   // Usually the project directory
                [fsharpCoreExplicitDirOrFSharpBinariesDir] @    // Location of explicit reference to FSharp.Core, otherwise location of fsc.exe
                [sprintf "{Registry:%s,%s,%s%s}" frameworkRegistryBase targetFrameworkVersion assemblyFoldersSuffix assemblyFoldersConditions] @ // Like {Registry:Software\Microsoft\.NETFramework,v2.0,AssemblyFoldersEx}
                ["{AssemblyFolders}"] @
                [outputDirectory] @
                ["{GAC}"]     
    
        rar.SearchPaths <- searchPaths |> Array.ofList
                                  
        rar.AllowedAssemblyExtensions <- [| ".dll" ; ".exe" |]     
        
        let succeeded = rar.Execute()
        
        // Unroll any foregrounded messages
        match !backgroundException with
        | Some(backGroundException) ->
            logwarning "" "Saw error on logger thread during resolution."
            logwarning "" (sprintf "%A" backGroundException)
            logwarning "" "Showing messages seen after exception."

            !foregrounded
            |> List.iter(fun message-> 
               match message with 
               | ForegroundedMessage(message) -> logmessage message
               | ForegroundedWarning(code,message) -> logwarning code message
               | ForegroundedError(code,message) -> logerror code message )
        | None -> ()            

        if not succeeded then 
            raise ResolutionFailure

        {
            resolvedFiles = [| for p in rar.ResolvedFiles -> {itemSpec = p.ItemSpec; 
                                                              resolvedFrom = DecodeResolvedFrom(p.GetMetadata("ResolvedFrom"));
                                                              fusionName = p.GetMetadata("FusionName"); 
                                                              version = p.GetMetadata("Version"); 
                                                              redist = p.GetMetadata("Redist"); 
                                                              baggage = p.GetMetadata("Baggage") } |]
            referenceDependencyPaths = [| for p in rar.ResolvedDependencyFiles -> p.ItemSpec |]
            relatedPaths = [| for p in rar.RelatedFiles -> p.ItemSpec |]
            referenceSatellitePaths = [| for p in rar.SatelliteFiles -> p.ItemSpec |]
            referenceScatterPaths = [| for p in rar.ScatterFiles -> p.ItemSpec |]
            referenceCopyLocalPaths = [| for p in rar.CopyLocalFiles -> p.ItemSpec |]
            suggestedBindingRedirects = [| for p in rar.SuggestedRedirects -> p.ItemSpec |]
        }

    let Resolve(
                resolutionEnvironment: ResolutionEnvironment,
                references:(string*(*baggage*)string)[], 
                targetFrameworkVersion:string, 
                targetFrameworkDirectories:string list,
                targetProcessorArchitecture:string,                
                outputDirectory:string, 
                fsharpCoreExplicitDirOrFSharpBinariesDir:string,
                explicitIncludeDirs:string list,
                implicitIncludeDir:string,
                frameworkRegistryBase:string, 
                assemblyFoldersSuffix:string, 
                assemblyFoldersConditions:string, 
                logmessage:string->unit, 
                logwarning:ErrorWarningCallbackSig, 
                logerror:ErrorWarningCallbackSig ) =
        // The {RawFileName} target is 'dangerous', in the sense that is uses Directory.GetCurrentDirectory() to resolve unrooted file paths.
        // It is unreliable to use this mutable global state inside Visual Studio.  As a result, we partition all references into a "rooted" set
        // (which contains e.g. C:\MyDir\MyAssem.dll) and "unrooted" (everything else).  We only allow "rooted" to use {RawFileName}.  Note that
        // unrooted may still find 'local' assemblies by virtue of the fact that "implicitIncludeDir" is one of the places searched during 
        // assembly resolution.
        let references = references |> Array.map (fun ((file,baggage) as data) -> 
            // However, MSBuild will not resolve 'relative' paths, even when e.g. implicitIncludeDir is part of the search.  As a result,
            // if we have an unrooted path+filename, we'll assume this is relative to the project directory and root it.
            if FileSystem.IsPathRootedShim(file) then
                data  // fine, e.g. "C:\Dir\foo.dll"
            elif not(file.Contains("\\") || file.Contains("/")) then
                data  // fine, e.g. "System.Transactions.dll"
            else
                // we have a 'relative path', e.g. "bin/Debug/foo.exe" or "..\Yadda\bar.dll"
                // turn it into an absolute path based at implicitIncludeDir
                (System.IO.Path.Combine(implicitIncludeDir, file), baggage)
        )
        let rooted, unrooted = references |> Array.partition (fun (file,_baggage) -> FileSystem.IsPathRootedShim(file))

        let CallResolveCore(references, allowRawFileName) =    
            if Array.isEmpty references then 
                {
                    resolvedFiles = [| |]
                    referenceDependencyPaths = [| |]
                    relatedPaths = [| |]
                    referenceSatellitePaths = [| |]
                    referenceScatterPaths = [| |]
                    referenceCopyLocalPaths = [| |]
                    suggestedBindingRedirects = [| |]
                }
            else
                // all the params are the same...
                ResolveCore(
                    resolutionEnvironment,
                    references, // ... except this
                    targetFrameworkVersion, 
                    targetFrameworkDirectories,
                    targetProcessorArchitecture,                
                    outputDirectory, 
                    fsharpCoreExplicitDirOrFSharpBinariesDir,
                    explicitIncludeDirs,
                    implicitIncludeDir,
                    frameworkRegistryBase, 
                    assemblyFoldersSuffix, 
                    assemblyFoldersConditions, 
                    allowRawFileName, // ... and this
                    logmessage, 
                    logwarning, 
                    logerror)

        let rootedResults = CallResolveCore(rooted, true)
        let unrootedResults = CallResolveCore(unrooted, false)
        // now unify the two sets of results
        {
            resolvedFiles = Array.concat [| rootedResults.resolvedFiles; unrootedResults.resolvedFiles |]
            referenceDependencyPaths = set rootedResults.referenceDependencyPaths |> Set.union (set unrootedResults.referenceDependencyPaths) |> Set.toArray 
            relatedPaths = set rootedResults.relatedPaths |> Set.union (set unrootedResults.relatedPaths) |> Set.toArray 
            referenceSatellitePaths = set rootedResults.referenceSatellitePaths |> Set.union (set unrootedResults.referenceSatellitePaths) |> Set.toArray 
            referenceScatterPaths = set rootedResults.referenceScatterPaths |> Set.union (set unrootedResults.referenceScatterPaths) |> Set.toArray 
            referenceCopyLocalPaths = set rootedResults.referenceCopyLocalPaths |> Set.union (set unrootedResults.referenceCopyLocalPaths) |> Set.toArray 
            suggestedBindingRedirects = set rootedResults.suggestedBindingRedirects |> Set.union (set unrootedResults.suggestedBindingRedirects) |> Set.toArray 
        }

#endif
