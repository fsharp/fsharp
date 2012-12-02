﻿namespace UnitTests.TestLib.ProjectSystem

open System 
open System.CodeDom
open System.CodeDom.Compiler
open System.Runtime.Serialization
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Diagnostics
open Internal.Utilities.Debug
open System.IO
open System.Text
open System.Xml.Linq
open Salsa

open Microsoft.Win32

open Microsoft.VisualStudio.FSharp.ProjectSystem
open Microsoft.VisualStudio.Shell.Interop

open Microsoft.Build.BuildEngine
open Microsoft.Build.Execution
open Microsoft.Build.Framework
        
#nowarn "52" // The value has been copied to ensure the original is not mutated
open NUnit.Framework

open UnitTests.TestLib.Utils
open UnitTests.TestLib.Utils.Asserts
open UnitTests.TestLib.Utils.FilesystemHelpers

type internal UnitTestingFSharpProjectNode(package:FSharpProjectPackage) as this =
    inherit FSharpProjectNode(package) 

    do this.InteropSafeIVsHierarchy <- this
       this.InteropSafeIVsUIHierarchy <- this
       this.InteropSafeIVsProject <- this
       this.InteropSafeIVsSccProject2 <- this

type AddReferenceDialogTab =
    | DotNetTab = 0
    | BrowseTab = 1

// helper type for representing solution-explorer tree
[<StructuralEquality; NoComparison>]
type Tree<'T> =
    | Tree of (*data*)'T * (*firstChild*)Tree<'T> * (*nextSibling*)Tree<'T>
    | Nil

type TheTests() = 
    static let Net35RefAssemPathOnThisMachine() =
        let key = @"SOFTWARE\Microsoft\.NETFramework\AssemblyFolders\Microsoft .NET Framework 3.5 Reference Assemblies"
        let hklm = Registry.LocalMachine
        let rkey = hklm.OpenSubKey(key)
        rkey.GetValue("") :?> string

    static let ANYTREE = Tree("",Nil,Nil)
   
            
    /////////////////////////////////
    // project helpers
    static let SaveProject(project : UnitTestingFSharpProjectNode) =
        project.Save(null, 1, 0u) |> ignore

    static let DefaultBuildActionOfFilename(filename) : Salsa.BuildAction = 
        match Path.GetExtension(filename) with 
        | ".fsx" -> Salsa.BuildAction.None
        | ".resx"
        | ".resources" -> Salsa.BuildAction.EmbeddedResource
        | _ -> Salsa.BuildAction.Compile            

    static let GetReferenceContainerNode(project : ProjectNode) =
        let l = new List<ReferenceContainerNode>()
        project.FindNodesOfType(l)
        l.[0]            
        
    /////////////////////////////////
    /// Called per test
    [<SetUp>]
    member this.Setup() =
        ()

        
    [<TearDown>]
    member this.TearDown() =
        // help find leaks per-test
        System.GC.Collect()  
        System.GC.WaitForPendingFinalizers()
#if DEBUG
        printfn "TearDown..."
        if Microsoft.VisualStudio.FSharp.LanguageService.TaskReporter.AliveCount <> 0 then
            Debug.Assert(false, sprintf "There are %d TaskReporters still alive" Microsoft.VisualStudio.FSharp.LanguageService.TaskReporter.AliveCount)
#endif
        ()

    /////////////////////////////////
    /// helpers
    static member AssertMatches (r : Regex) (s:string) =
        if not (r.IsMatch(s)) then
            let msg = sprintf "Expected regex '%s' to match '%s'." (r.ToString()) s
            printfn "%s" msg
            Assert.Fail(msg)
    // Like AssertMatches, but runs for every prefix of regex up to each occurence of 'c'
    // Is helpful so that, if long regex match fails, you see first prefix that fails
    static member HelpfulAssertMatches (c : char) (regexStr : string) (s:string) =
        let mutable i = regexStr.IndexOf(c, 0)
        while i <> -1 do
            let r = regexStr.Substring(0,i)
            let regex = new Regex(r)
            TheTests.AssertMatches regex s
            i <- regexStr.IndexOf(c, i+1)
        TheTests.AssertMatches (new Regex(regexStr)) s

    static member PrintHierarchy(node : HierarchyNode) =
        let rec nSpaces n = 
            if n = 0 then "" else "    " + nSpaces (n-1)
        let rec Print(node : HierarchyNode, level : int) =
            printfn "%s%s" (nSpaces level) node.Caption
            if node.FirstChild <> null then
                Print(node.FirstChild, level + 1)
            if node.NextSibling <> null then
                Print(node.NextSibling, level)
        Print(node, 0)

    static member internal CreateProject(filename : string, forceUTF8 : string, configChangeNotifier, serviceProvider) =
        UIStuff.SetupSynchronizationContext()
        let buildEngine = Utilities.InitializeMsBuildEngine(null)
        let buildProject = Utilities.InitializeMsBuildProject(buildEngine, filename)
        let package = new FSharpProjectPackage()
        let project = new UnitTestingFSharpProjectNode(package)
        try
            project.SetSite(serviceProvider) |> ignore
            project.BuildProject <- buildProject
            let _ = project.BaseURI // This property needs to be touched at least once while the .BuildProject is populated
            let mutable cancelled = 0
            let mutable guid = Guid.NewGuid()
            printfn "about to load .fsproj"
            project.Load(filename, null, null, 2u, &guid, &cancelled)
            printfn "loaded"
            let slfpe = new SolutionListenerForProjectEvents(project.Site)
            (project :> IProjectEventsProvider).ProjectEventsProvider <- (slfpe :> IProjectEvents)
            slfpe.OnAfterOpenProject((project :> IVsHierarchy), 0) |> ignore
            MSBuildProject.SetGlobalProperty(project.BuildProject, "UTF8Output", forceUTF8)
            project
        with 
        | e -> project.Close() |> ignore
               reraise()

    static member internal CreateProject(filename : string) =
        let sp, configChangeNotifier = VsMocks.MakeMockServiceProviderAndConfigChangeNotifier()
        let p = TheTests.CreateProject(filename, "false", configChangeNotifier, sp)
        // ensure that vs-style encoding is off
        p
        
    static member internal CreateProjectWithUTF8Output(filename : string) =
        let sp, configChangeNotifier = VsMocks.MakeMockServiceProviderAndConfigChangeNotifier()
        let p = TheTests.CreateProject(filename, "true", configChangeNotifier, sp)
        p
      
    static member internal FindNodeWithCaption(project : UnitTestingFSharpProjectNode, caption) =
        let node = project.FirstChild
        let rec TryFind (n : HierarchyNode) =
            if n = null then None 
            elif n.Caption = caption then Some(n) 
            else match TryFind(n.FirstChild) with
                 | None -> TryFind(n.NextSibling)
                 | x -> x
        match TryFind node with
        | Some(x) -> x
        | None -> failwith "did not find node with caption %s" caption
       
    static member MoveDown(node : HierarchyNode) =
        match node with
        | :? FSharpFileNode 
        | :? FSharpFolderNode -> 
            TheTests.EnsureMoveDownEnabled(node)
            node.ExecCommandOnNode(FSharpProjectFileConstants.guidFSharpProjectCmdSet, 
                                   uint32 FSharpProjectFileConstants.MoveDownCmd.ID,
                                   uint32 0, new IntPtr(0), new IntPtr(0)) |> ignore
        | _ -> failwith "unexpected node type"
        ()

    static member MoveUp(node : HierarchyNode) =
        match node with
        | :? FSharpFileNode 
        | :? FSharpFolderNode ->
            TheTests.EnsureMoveUpEnabled(node)
            node.ExecCommandOnNode(FSharpProjectFileConstants.guidFSharpProjectCmdSet, 
                                   uint32 FSharpProjectFileConstants.MoveUpCmd.ID,
                                   uint32 0, new IntPtr(0), new IntPtr(0)) |> ignore
        | _ -> failwith "unexpected node type"
        ()

    static member EnsureMoveDownDisabled(node : HierarchyNode) =
        // Move Down appears on menu, but is greyed out
        let mutable qsr = new QueryStatusResult()
        ValidateOK(node.QueryStatusOnNode(FSharpProjectFileConstants.guidFSharpProjectCmdSet, uint32 FSharpProjectFileConstants.MoveDownCmd.ID, 0n, &qsr))
        let expected = QueryStatusResult.SUPPORTED
        AssertEqual expected qsr
        
    static member EnsureMoveDownEnabled(node : HierarchyNode) =
        // Move Down appears on menu, and can be clicked
        let mutable qsr = new QueryStatusResult()
        ValidateOK(node.QueryStatusOnNode(FSharpProjectFileConstants.guidFSharpProjectCmdSet, uint32 FSharpProjectFileConstants.MoveDownCmd.ID, 0n, &qsr))
        let expected = QueryStatusResult.SUPPORTED ||| QueryStatusResult.ENABLED
        AssertEqual expected qsr
             
    static member EnsureMoveUpDisabled(node : HierarchyNode) =
        // Move Up appears on menu, but is greyed out
        let mutable qsr = new QueryStatusResult()
        ValidateOK(node.QueryStatusOnNode(FSharpProjectFileConstants.guidFSharpProjectCmdSet, uint32 FSharpProjectFileConstants.MoveUpCmd.ID, 0n, &qsr))
        let expected = QueryStatusResult.SUPPORTED
        AssertEqual expected qsr
        
    static member EnsureMoveUpEnabled(node : HierarchyNode) =
        // Move Up appears on menu, and can be clicked
        let mutable qsr = new QueryStatusResult()
        ValidateOK(node.QueryStatusOnNode(FSharpProjectFileConstants.guidFSharpProjectCmdSet, uint32 FSharpProjectFileConstants.MoveUpCmd.ID, 0n, &qsr))
        let expected = QueryStatusResult.SUPPORTED ||| QueryStatusResult.ENABLED
        AssertEqual expected qsr
             
    static member SimpleFsprojText(compileItems : string list, references : string list, other : string) = 
        TheTests.FsprojTextWithProjectReferences(compileItems, references, [], other)
        
    static member SimpleFsprojTextOtherFlags(compileItems : string list, references : string list, otherflags : string, other : string) = 
        TheTests.FsprojTextWithProjectReferencesAndOtherFlags(compileItems, references, [], otherflags, other)
    
    static member public FsprojTextWithProjectReferences(compileItems : string list, references : string list, projectReferences : string list, other : string) = 
        let vsops = fst (Salsa.Salsa.Models.MSBuild())
        let references = references |> List.map (fun r->r,false)
        let text = vsops.CreatePhysicalProjectFileInMemory [for i in compileItems -> (i,DefaultBuildActionOfFilename i, None)] references projectReferences [] [] null null other null
        printfn "%s" text
        text
        
    static member public FsprojTextWithProjectReferencesAndOtherFlags(compileItems : string list, references : string list, projectReferences : string list, otherflags : string, other : string, targetFramework : string) = 
        let vsops = fst (Salsa.Salsa.Models.MSBuild())
        let references = references |> List.map (fun r->r,false)
        let text = vsops.CreatePhysicalProjectFileInMemory [for i in compileItems -> (i,DefaultBuildActionOfFilename i, None)] references projectReferences [] [] null otherflags other targetFramework
        printfn "%s" text
        text

    static member public FsprojTextWithProjectReferencesAndOtherFlags(compileItems : string list, references : string list, projectReferences : string list, otherflags : string, other : string) = 
        TheTests.FsprojTextWithProjectReferencesAndOtherFlags(compileItems, references, projectReferences, otherflags, other, null)

    static member AssertSameTree(expectation : Tree<string>, node : HierarchyNode) =
        printfn "actual hierarchy in solution explorer:"
        TheTests.PrintHierarchy(node)
        TheTests.AssertSameTreeHelper(expectation, node)

    static member AssertSameTreeHelper(expectation : Tree<string>, node : HierarchyNode) =
        match expectation with
        | Tree _ as x when Object.Equals(x, ANYTREE) -> ()
        | Nil -> 
            AssertEqual null node
        | Tree(name,firstChild,nextSibling) -> 
            AssertEqual name node.Caption
            TheTests.AssertSameTreeHelper(firstChild, node.FirstChild)
            TheTests.AssertSameTreeHelper(nextSibling, node.NextSibling)
        ()

    static member AssertSimilarXml(expectation : XElement, actual : XElement) =
        // compares element/attribute structure, LocalName, and Value, but ignores Namespace
        // can expect attribute value "ANY", which matches anything
        let rec Match(expectation : XElement, actual : XElement, outerContext : string) =
            let TryLookup(d : IDictionary<_,_>, key : 'k) =
                let mutable value = Unchecked.defaultof<'v>
                if d.TryGetValue(key, &value) then
                    Some value
                else
                    None
            let initialContext = 
                sprintf "%sWhile expecting '\n%s\n' to match '\n%s\n', " 
                    outerContext 
                    (expectation.ToString(SaveOptions.None)) 
                    (actual.ToString(SaveOptions.None))
            let Err s = 
                sprintf "%sfound error:\n%s" initialContext s
            if expectation.Name.LocalName <> actual.Name.LocalName then
                Assert.Fail(Err <| sprintf "Expected element name '%s' but got '%s'" expectation.Name.LocalName actual.Name.LocalName)
            let exAttrs = expectation.Attributes() |> Seq.map (fun a -> a.Name.LocalName, a.Value) |> dict
            let actAttrs = actual.Attributes() |> Seq.map (fun a -> a.Name.LocalName, a.Value) |> dict
            if exAttrs.Count <> actAttrs.Count then
                Assert.Fail(Err <| sprintf "Expected '%d' attributes, but found '%d'" exAttrs.Count actAttrs.Count)
            for kvp in exAttrs do
                match TryLookup(actAttrs, kvp.Key) with
                | None -> Assert.Fail(Err <| sprintf "Expected attribute with localname '%s', but none present" kvp.Key)
                | Some(v) -> if kvp.Value <> "ANY" && kvp.Value <> v then
                                 Assert.Fail(Err <| sprintf "Expected attribute '%s' to have value '%s', but actually had '%s'" kvp.Key kvp.Value v)
            let exLen = Seq.length (expectation.Elements())
            let actLen = Seq.length (actual.Elements())
            if exLen <> actLen then
                Assert.Fail(Err <| sprintf "Expected '%d' sub-elements, but found '%d'" exLen actLen)
            (expectation.Elements(), actual.Elements()) ||> Seq.iter2 (fun x y -> Match(x,y,initialContext+"\n"))
        Match(expectation, actual, "")
    
    static member internal GetCompileItems (project : UnitTestingFSharpProjectNode) =
        let MakeRelativePath (file : string) =
            let projDir = project.ProjectFolder + "\\"
            if file.StartsWith(projDir) then 
                file.Substring(projDir.Length)
            else
                file
        project.GetCompileItems() |> Array.toList |> List.map MakeRelativePath
 
    member internal this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifierDispose(dispose : bool, compileItems : string list, references : string list, other : string, targetFramework : string, f : UnitTestingFSharpProjectNode -> _ -> string -> unit) =
        UIStuff.SetupSynchronizationContext()

        DoWithTempFile "Test.fsproj" (fun file ->
            File.AppendAllText(file, TheTests.FsprojTextWithProjectReferencesAndOtherFlags(compileItems, references, [], null, other, targetFramework))
            let sp, cnn = 
                match targetFramework with
                | "v4.0" -> VsMocks.MakeMockServiceProviderAndConfigChangeNotifier40()
                | "v3.5" -> VsMocks.MakeMockServiceProviderAndConfigChangeNotifier35()
                | "v3.0" -> VsMocks.MakeMockServiceProviderAndConfigChangeNotifier30()
                | "v2.0" -> VsMocks.MakeMockServiceProviderAndConfigChangeNotifier20()
                | null -> VsMocks.MakeMockServiceProviderAndConfigChangeNotifier40()
                | _ -> failwithf "unexpected targetFramework %s" targetFramework
            let project = TheTests.CreateProject(file, "false", cnn, sp)
            try
                f project cnn file
            finally
                if dispose then project.Close() |> ignore
        )

    member internal this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifierDispose(dispose : bool, compileItems : string list, references : string list, other : string, f : UnitTestingFSharpProjectNode -> _ -> string -> unit) =
        this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifierDispose(dispose, compileItems, references, other, null, f)

    member internal this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifier(compileItems : string list, references : string list, other : string, targetFramework : string, f : UnitTestingFSharpProjectNode -> _ -> string -> unit) =
        this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifierDispose(true, compileItems, references, other, targetFramework, f)

    member internal this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifier(compileItems : string list, references : string list, other : string, f : UnitTestingFSharpProjectNode -> _ -> string -> unit) =
        this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifier(compileItems, references, other, null, f)
        
    member internal this.MakeProjectAndDoWithProjectFile(compileItems : string list, references: string list, other : string, targetFramework : string, f : UnitTestingFSharpProjectNode -> string -> unit) =
        this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifier(compileItems, references, other, targetFramework, fun proj _ s -> f proj s)
    
    member internal this.MakeProjectAndDoWithProjectFile(compileItems : string list, references: string list, other : string, f : UnitTestingFSharpProjectNode -> string -> unit) =
        this.MakeProjectAndDoWithProjectFile(compileItems, references, other, null, f)

    /// Create a project with the given "compileItems" and "other", then 
    /// call "f" on that project while the project file still exists on disk.
    member internal this.MakeProjectAndDo(compileItems : string list, references : string list, other : string, f : UnitTestingFSharpProjectNode -> unit) =
        this.MakeProjectAndDo(compileItems, references, other, null, f)

    member internal this.MakeProjectAndDo(compileItems : string list, references : string list, other : string, targetFramework : string, f : UnitTestingFSharpProjectNode -> unit) =
        this.MakeProjectAndDoWithProjectFile(compileItems, references, other, targetFramework, fun proj _ -> f proj)

    /// Create a project with the given "compileItems" and "other".
    member internal this.MakeProject(compileItems : string list, references : string list, other : string) =
        let project = ref (Unchecked.defaultof<UnitTestingFSharpProjectNode>)
        this.MakeProjectAndDoWithProjectFileAndConfigChangeNotifierDispose(false, compileItems, references, other, (fun p _ _ ->
            project := p
        ))
        !project

    
    member this.``FsprojFileToSolutionExplorer.PositiveTest``(compileItems : string list, other : string, expectations : Tree<string>) =
        use project = this.MakeProject(compileItems, [], other) :> HierarchyNode
        let node = project.FirstChild
        TheTests.AssertSameTree(expectations, node)
        () 

    member internal this.EnsureCausesNotification(project, code) =
        let ipsf = project :> Microsoft.VisualStudio.FSharp.LanguageService.IProvideProjectSite
        let ips = ipsf.GetProjectSite()
        let changed = ref false
        let handle = ips.AdviseProjectSiteChanges("EnsureCausesNotificationTest", new Microsoft.VisualStudio.FSharp.LanguageService.AdviseProjectSiteChanges(fun () -> changed := true))
        code()
        AssertEqual true (!changed)
    static member MsBuildCompileItems(project : Microsoft.Build.Evaluation.Project) =
        [for bi in project.Items do
            if bi.ItemType = "Compile" then
                yield bi.EvaluatedInclude] 
    member this.MSBuildProjectBoilerplate (outputType : string) : string =
        let template = @"
  <PropertyGroup>
    <Configuration Condition="" '$(Configuration)' == '' "">Debug</Configuration>
    <Platform Condition="" '$(Platform)' == '' "">AnyCPU</Platform>
    <OutputType>{0}</OutputType>
    <RootNamespace>Blah</RootNamespace>
    <AssemblyName>Blah</AssemblyName>
    <TargetFrameworkVersion>v3.5</TargetFrameworkVersion>
    <FileAlignment>512</FileAlignment>
    <Name>Blah</Name>
  </PropertyGroup>
  <PropertyGroup Condition="" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' "">
    <DebugSymbols>true</DebugSymbols>
    <DebugType>full</DebugType>
    <Optimize>false</Optimize>
    <OutputPath>bin\Debug\</OutputPath>
    <DefineConstants>DEBUG;TRACE</DefineConstants>
    <ErrorReport>prompt</ErrorReport>
    <WarningLevel>3</WarningLevel>
  </PropertyGroup>           
           "
        String.Format(template, outputType)
        
    member this.MSBuildProjectMulitplatBoilerplate (outputType : string) : string =
        let template = @"
  <PropertyGroup>
    <Configuration Condition="" '$(Configuration)' == '' "">Debug</Configuration>
    <Platform Condition="" '$(Platform)' == '' "">AnyCPU</Platform>
    <OutputType>{0}</OutputType>
    <RootNamespace>Blah</RootNamespace>
    <AssemblyName>Blah</AssemblyName>
    <TargetFrameworkVersion>v3.5</TargetFrameworkVersion>
    <FileAlignment>512</FileAlignment>
    <Name>Blah</Name>
  </PropertyGroup>
  <PropertyGroup Condition="" '$(Configuration)|$(Platform)' == 'Debug|x86' "">
    <PlatformTarget>x86</PlatformTarget>
    <DebugSymbols>true</DebugSymbols>
    <DebugType>full</DebugType>
    <Optimize>false</Optimize>
    <OutputPath>bin\Debug\</OutputPath>
    <DefineConstants>DEBUG;TRACE</DefineConstants>
    <ErrorReport>prompt</ErrorReport>
    <WarningLevel>3</WarningLevel>
  </PropertyGroup>             
  <PropertyGroup Condition="" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' "">
    <DebugSymbols>true</DebugSymbols>
    <DebugType>full</DebugType>
    <Optimize>false</Optimize>
    <OutputPath>bin\Debug\</OutputPath>
    <DefineConstants>DEBUG;TRACE</DefineConstants>
    <ErrorReport>prompt</ErrorReport>
    <WarningLevel>3</WarningLevel>
  </PropertyGroup>           
           "
        String.Format(template, outputType)
        
    member this.MSBuildProjectMultiConfigBoilerplate  (configs : (string*string) list): string =
        let template = @"
              <PropertyGroup>
                <Configuration Condition="" '$(Configuration)' == '' "">Debug</Configuration>
                <Platform Condition="" '$(Platform)' == '' "">AnyCPU</Platform>
                <OutputType>Library</OutputType>
                <RootNamespace>Blah</RootNamespace>
                <AssemblyName>Blah</AssemblyName>
                <TargetFrameworkVersion>v3.5</TargetFrameworkVersion>
                <FileAlignment>512</FileAlignment>
                <Name>Blah</Name>
              </PropertyGroup>"
        let sb = new StringBuilder(template)     
        for (configName,customStr) in configs do
            let platTemplate =
                @"<PropertyGroup Condition="" '$(Configuration)|$(Platform)' == '{0}|x86' "">
                    <PlatformTarget>x86</PlatformTarget>
                    {1}
                    <OutputPath>bin\{0}\</OutputPath>
                  </PropertyGroup>             
                  <PropertyGroup Condition="" '$(Configuration)|$(Platform)' == '{0}|AnyCPU' "">
                    <PlatformTarget>AnyCPU</PlatformTarget>                  
                    {1}
                    <OutputPath>bin\{0}\</OutputPath>                  
                  </PropertyGroup>"           
            String.Format(platTemplate, configName, customStr) |> sb.Append |> ignore
        sb.ToString()
        
    member this.MSBuildProjectMultiPlatform  (platforms : (string*string) list,?defaultPlatform): string =
        let platform = defaultArg defaultPlatform "AnyCPU"
        let template = @"
              <PropertyGroup>
                <Configuration Condition="" '$(Configuration)' == '' "">Debug</Configuration>
                <Platform Condition="" '$(Platform)' == '' "">{0}</Platform>
                <OutputType>Library</OutputType>
                <RootNamespace>Blah</RootNamespace>
                <AssemblyName>Blah</AssemblyName>
                <TargetFrameworkVersion>v3.5</TargetFrameworkVersion>
                <FileAlignment>512</FileAlignment>
                <Name>Blah</Name>
              </PropertyGroup>"
        let sb = new StringBuilder(String.Format(template,platform))     
        for (platformName,customStr) in platforms do
            let platTemplate =
                @"<PropertyGroup Condition="" '$(Configuration)|$(Platform)' == 'Release|{0}' "">
                    <OutputPath>bin\Release\</OutputPath>                    
                    {1}
                    <PlatformTarget>{0}</PlatformTarget>
                  </PropertyGroup>             
                  <PropertyGroup Condition="" '$(Configuration)|$(Platform)' == 'Debug|{0}' "">
                    <OutputPath>bin\Debug\</OutputPath>                    
                    {1}                                      
                    <PlatformTarget>{0}</PlatformTarget>                  
                  </PropertyGroup>"           
            String.Format(platTemplate, platformName, customStr) |> sb.Append |> ignore
        sb.ToString()
    member internal this.CheckConfigNames (project:UnitTestingFSharpProjectNode, expectedNames : string[])=
        let cfgNames = Array.create expectedNames.Length ""
        let actual = [| 0u |]
        project.ConfigProvider.GetCfgNames(uint32 cfgNames.Length, cfgNames, actual) |> AssertEqual VSConstants.S_OK
        AssertEqualMsg expectedNames cfgNames "List of config names is different"
        AssertEqualMsg expectedNames.Length (int actual.[0]) "List of config names is ok, but reported lengths disagree"
                
    member internal this.CheckPlatformNames(project:UnitTestingFSharpProjectNode, expectedNames : string[])=
        let platformNames = Array.create expectedNames.Length ""
        let actual = [| 0u |]
        project.ConfigProvider.GetPlatformNames(uint32 platformNames.Length, platformNames, actual) |> AssertEqual VSConstants.S_OK
        AssertEqualMsg expectedNames platformNames "List of platform names is different"
        AssertEqualMsg expectedNames.Length (int actual.[0]) "List of platfrom names is ok, but reported lengths disagree"


    member internal this.HelperEnsureAtLeastOne projFileBoilerplate expectedConfigs expectedPlatforms =
        this.MakeProjectAndDoWithProjectFile(["foo.fs"], [], projFileBoilerplate,
            (fun project projFileName ->
                this.CheckPlatformNames(project, expectedPlatforms)
                this.CheckConfigNames(project, expectedConfigs)
            ))
 

and (*type*) MSBuildItem =
    | FolderItem of string      // Include="relativeDir"
    | CompileItem of string     // Include="relDir\filename.fs"
    | LinkedCompileItem of (*include:*)string * (*link:*)string   // Include="relDir\filename" Link:nameInSolnExplorer
    | OtherItem of (*name:*)string * (*include:*)string           // <name Include="include"/>
    override this.ToString() =
        match this with
        | FolderItem(s) -> sprintf @"<Folder Include=""%s"" />" s
        | CompileItem(s) -> sprintf @"<Compile Include=""%s"" />" s
        | LinkedCompileItem(inc,n) -> sprintf @"<Compile Include=""%s""><Link>%s</Link></Compile>" inc n
        | OtherItem(name,inc) -> sprintf @"<%s Include=""%s"" />" name inc
    member this.AsRegexString() =
        match this with
        | FolderItem(s) -> sprintf @"\s*<Folder Include=""%s"" />\s*" (Regex.Escape s)
        | CompileItem(s) -> sprintf @"\s*<Compile Include=""%s"" />\s*" (Regex.Escape s)
        | LinkedCompileItem(inc,n) -> sprintf @"\s*<Compile Include=""%s"">\s*<Link>%s</Link>\s*</Compile>\s*" (Regex.Escape inc) (Regex.Escape n)
        | OtherItem(name,inc) -> sprintf @"\s*<%s Include=""%s"" />\s*" (Regex.Escape name) (Regex.Escape inc)
    member this.Caption() =  // how will appear in solution explorer
        match this with
        | FolderItem(s) ->  // folder might render as multiple nodes for each path part, return caption for final one
            Debug.Assert(s.EndsWith("\\"), "folders should end with slash")
            let pathParts = s.Split([| '\\' |], StringSplitOptions.RemoveEmptyEntries)
            pathParts.[pathParts.Length - 1]
        | CompileItem(s) -> Path.GetFileName(s)
        | LinkedCompileItem(inc,n) -> Path.GetFileName(n)
        | OtherItem(name,inc) -> Path.GetFileName(inc)
    member this.IntoFolder(folder : string) =  // return new copy of item in the folder
        Debug.Assert(folder.EndsWith("\\"), "folders should end with slash")
        match this with
        | FolderItem(s) ->  
            Debug.Assert(s.EndsWith("\\"), "folders should end with slash")
            FolderItem(folder + s)
        | CompileItem(s) -> CompileItem(folder + s)
        | LinkedCompileItem(inc,n) -> LinkedCompileItem(inc, folder + n)
        | OtherItem(name,inc) -> OtherItem(name, folder + inc)
and (*type*) MSBuildItems =
    | MSBuildItems of list<MSBuildItem>
    override this.ToString() =
        match this with
        | MSBuildItems(l) ->
            "<ItemGroup>\n" +
            (l |> List.map (fun x -> x.ToString()) |> List.fold (fun acc x -> sprintf "  %s%s\n" acc x) "") +
            "</ItemGroup>"
    member this.AsRegexString() =
        match this with
        | MSBuildItems(l) ->
            "\s*<ItemGroup>\s*" +
            (l |> List.map (fun x -> x.AsRegexString()) |> List.fold (fun acc x -> acc + x) "") +
            "\s*</ItemGroup>\s*"
    member this.Items() =
        match this with
        | MSBuildItems(l) -> l
        
(*
when first click to start drag
        public override int GetDropInfo(out uint pdwOKEffects, out IOleDataObject ppDataObject, out IDropSource ppDropSource)
ensure pdwOKEffects are right and return S_OK

when drag onto new entity
        public override int DragEnter(IOleDataObject pDataObject, uint grfKeyState, uint itemid, ref uint pdwEffect)
ensure is same thing(pDataObj, itemid) being dragged, pdwEffect is properly updated

not sure how differs from DragEnter...
        public override int DragOver(uint grfKeyState, uint itemid, ref uint pdwEffect)
ensure right pdwEffect

at end of drag...
        public override int OnBeforeDropNotify(IOleDataObject o, uint dwEffect, out int fCancelDrop)
can prompt to save dirty dragged items... nothing to verify?

        public override int OnDropNotify(int fDropped, uint dwEffects)
unsure...

        public override int Drop(IOleDataObject pDataObject, uint grfKeyState, uint itemid, ref uint pdwEffect)
unsure...

        /*internal, but public for FSharp.Project.dll*/ public DropDataType ProcessSelectionDataObject(IOleDataObject dataObject, HierarchyNode targetNode)
called by Drop, does the actual action of adding dropped files to new location        

        public override int DragLeave()
nothing to ensure, but call if leave hierarchy or drag is canceled or finishes


called at various times:
                public /*protected, but public for FSharp.Project.dll*/ override HierarchyNode GetDragTargetHandlerNode()
ensure FSharpFileNode does right thing, whatever that may be, same for folder, project

        /*internal, but public for FSharp.Project.dll*/ public static DropDataType QueryDropDataType(IOleDataObject pDataObject)
maybe ref (shortcut) versus value (real file/dir on disk)?

        /*internal, but public for FSharp.Project.dll*/ public DropEffect QueryDropEffect(DropDataType dropDataType, uint grfKeyState)
given keyboard and object type, return right effect

        public /*protected internal, but public for FSharp.Project.dll*/ virtual bool CanTargetNodeAcceptDrop(uint itemId)
can't drop onto 'references'
*)        

module LanguageServiceExtension =
    open UnitTests.TestLib.LanguageService
    open Salsa.Salsa

    type internal ProjInfo() =
        let mutable proj = Unchecked.defaultof<UnitTestingFSharpProjectNode>
        let mutable createProjectHookIsEnabled = true
        member this.Project with get() = proj and set(x) = proj <- x
        member this.CreateProjectHookIsEnabled with get() = createProjectHookIsEnabled and set(x) = createProjectHookIsEnabled <- x

    let internal ProjectSystem = 
        let msbuild,hooks = Models.MSBuild()
        let projectDict = new Dictionary<OpenProject,ProjInfo>()
        { msbuild with 
                OutOfConeFilesAreAddedAsLinks=true;
                SupportsOutputWindowPane=true;
                AddAssemblyReference=(fun (project, assem, specificVersion) -> 
                    let projInfo = projectDict.[project]
                    let referencesFolder = projInfo.Project.FindChild(ReferenceContainerNode.ReferencesNodeVirtualName) :?> ReferenceContainerNode
                    let assem = 
                        // mimic logic in ReferenceResolution.fs:MSBuildResolver.Resolve()
                        if Path.IsPathRooted(assem) then 
                            assem 
                        elif not(assem.Contains("\\") || assem.Contains("/")) then
                            assem
                        else 
                            Path.Combine(projInfo.Project.ProjectFolder, assem)
                    let assem, isFullPath =
                        if Path.IsPathRooted(assem) then
                            assem, true
                        elif assem.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) || assem.EndsWith(".exe", StringComparison.OrdinalIgnoreCase) then
                            // some unit tests pass relative paths, make them absolute
                            Path.Combine(projInfo.Project.ProjectFolder, assem), true
                        else
                            assem, false  // assem is a simple/fusion name of an assembly
                    let node = referencesFolder.CreateAssemblyReferenceNode(assem, Microsoft.VisualStudio.FSharp.ProjectSystem.AddReferenceDialogTab.BrowseTab, isFullPath)
                    if node <> null then  // node may be null if reference was to non-existent file
                        if node.AddReference() then
                            // still need to add it to underlying representation (SimpleOpenProject) so that
                            // subsequent Reload() calls will have right info
                            projInfo.CreateProjectHookIsEnabled <- false
                            msbuild.AddAssemblyReference(project, node.Url, specificVersion)
                            projInfo.CreateProjectHookIsEnabled <- true
                            ());
                CreateProject=(fun (solution,projectBaseName)->
                    let configChangeNotifier = ref None
                    let projInfo = new ProjInfo()
                    let NULL = Unchecked.defaultof<UnitTestingFSharpProjectNode>
                    let newHooks = { 
                        // Note: CreateProjectHook will callback MakeHierarcyHook and then InitializeProjectHook
                        CreateProjectHook = 
                            fun (projectFilename:string) (files:(string*BuildAction*string option) list) (references:(string*bool) list) (projReferences:string list)
                                (disabledWarnings:string list) (defines:string list) (versionFile:string) (otherFlags:string) (otherMSBuildStuff:string) (targetFrameworkVersion : string) ->
                                    if projInfo.CreateProjectHookIsEnabled then
                                        hooks.CreateProjectHook projectFilename files references projReferences disabledWarnings defines versionFile otherFlags otherMSBuildStuff targetFrameworkVersion
                                        if projInfo.Project = NULL then
                                            ()
                                        else
                                            // REVIEW: this is a workaround to get everything working for now; ideally we want to implement the VS gestures below
                                            // so that they really happen in the project system, rather than just poking the .fsproj file and then doing 
                                            // a 'reload' each time.  But for now, this is good.
                                            projInfo.Project.Reload()
                        InitializeProjectHook = (fun(openProject) ->
                            hooks.InitializeProjectHook(openProject)
                            projectDict.Add(openProject, projInfo))
                        MakeHierarchyHook = 
                                        fun projdir fullname projectname ccn serviceProvider -> 
                                            if projInfo.Project = NULL then
                                                let p = TheTests.CreateProject(fullname, "false", ccn, serviceProvider)
                                                projInfo.Project <- p
                                                configChangeNotifier := Some(fun s -> ccn((p :> IVsHierarchy),s))
                                            else
                                                failwith "oops, did not expect MakeHierarchy to be called more than once"
                                            projInfo.Project :> IVsHierarchy
                        AddFileToHierarchyHook = fun filename hier -> ()
                        BuildHook = fun projFileName target vsOutputWindowPane -> 
                                        if projInfo.Project = NULL then
                                            failwith "tried to build not-yet-created project"
                                        else
                                            let target = if target <> null then target else "Build"
                                            projInfo.Project.BuildToOutput(target,vsOutputWindowPane) |> ignore   // force build through project system for code coverage
                                            hooks.BuildHook projFileName target vsOutputWindowPane      // use MSBuild to build and also return MainAssembly value
                        GetMainOutputAssemblyHook = hooks.GetMainOutputAssemblyHook                                            
                        SaveHook = fun() -> if projInfo.Project = NULL then () else projInfo.Project.Save(null, 1, 0u) |> ignore
                        DestroyHook = fun () ->
                                            if projInfo.Project = NULL
                                               then ()
                                               else projInfo.Project.Close () |> ignore
                                                    match projectDict |> Seq.tryFind(fun (KeyValue(k,v)) -> obj.ReferenceEquals(v, projInfo)) with
                                                    | Some(KeyValue(k,v)) -> projectDict.Remove(k) |> ignore
                                                    | None -> failwith "uh-oh, where was it in the dict?"
                                                    projInfo.Project <- NULL
                        ModifyConfigurationAndPlatformHook = fun s ->
                            match !configChangeNotifier with
                            | Some(ccn) -> ccn(s)
                            | None -> ()
                    }

                    msbuild.CreateProjectWithHooks(solution,newHooks,projectBaseName));
        }

    


      

                


