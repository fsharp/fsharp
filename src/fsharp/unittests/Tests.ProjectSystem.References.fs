namespace UnitTests.Tests.ProjectSystem

open System
open System.Collections.Generic
open System.IO
open System.Reflection

open NUnit.Framework
open Salsa
open UnitTests.TestLib.Utils.Asserts
open UnitTests.TestLib.Utils.FilesystemHelpers
open UnitTests.TestLib.ProjectSystem

open Microsoft.VisualStudio.FSharp.ProjectSystem
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.Win32

[<TestFixture>]
type References() = 
    inherit TheTests()

    //TODO: look for a way to remove the helper functions
    static let Net35RefAssemPathOnThisMachine() =
        let key = @"SOFTWARE\Microsoft\.NETFramework\AssemblyFolders\Microsoft .NET Framework 3.5 Reference Assemblies"
        let hklm = Registry.LocalMachine
        let rkey = hklm.OpenSubKey(key)
        rkey.GetValue("") :?> string
    static let Net20AssemExPathOnThisMachine() =
        let key = @"SOFTWARE\Microsoft\.NETFramework\v2.0.50727\AssemblyFoldersEx\Public Assemblies (Common Files)"
        let hklm = Registry.LocalMachine
        let rkey = hklm.OpenSubKey(key)
        rkey.GetValue("") :?> string

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


    [<Test>]
    member this.``BasicAssemblyReferences1``() =
        this.MakeProjectAndDo([], ["System"], "", (fun proj ->
            let systemRef = proj.FirstChild.FirstChild :?> AssemblyReferenceNode
            Assert.IsTrue(systemRef.CanShowDefaultIcon())
        ))

    [<Test>]
    member this.``BasicAssemblyReferences2``() =
        this.MakeProjectAndDo([], ["System.Net"], "", (fun proj ->
            let systemRef = proj.FirstChild.FirstChild :?> AssemblyReferenceNode
            Assert.IsTrue(systemRef.CanShowDefaultIcon())
        ))
            
    [<Test>]
    member public this.``AddReference.StarredAssemblyName`` () = 
        DoWithTempFile "Test.fsproj" (fun projFile ->
            File.AppendAllText(projFile, TheTests.SimpleFsprojText([], [], ""))
            use project = TheTests.CreateProject(projFile) 
            let assName = new AssemblyName(typeof<System.Windows.Forms.Form>.Assembly.FullName)
            let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus, bstrFile = "*" + assName.FullName)
            let refContainer = GetReferenceContainerNode(project)
            refContainer.AddReferenceFromSelectorData(selectorData) |> Assert.IsNotNull
            let l = new List<AssemblyReferenceNode>()
            project.FindNodesOfType(l)
            Assert.AreEqual(1, l.Count)
            Assert.AreEqual("System.Windows.Forms", l.[0].Caption)            
            SaveProject(project)
            let fsprojFileText = File.ReadAllText(project.FileName)
            printfn "%s" fsprojFileText
            let expectedFsprojRegex = @"<Reference Include=""System.Windows.Forms"" />"
            TheTests.HelpfulAssertMatches '<' expectedFsprojRegex fsprojFileText
            )

    [<Test>]
    member public this.``References.Bug787899.AddDuplicateUnresolved``() =
        // Let's create a run-of-the-mill project just to have a spare assembly around
        this.CreateDummyTestProjectBuildItAndDo(fun exe ->
            Assert.IsTrue(File.Exists exe, "failed to build exe")
            this.MakeProjectAndDoWithProjectFile(["doesNotMatter.fs"], ["mscorlib"; "System"; "System.Core"; "System.Net"], 
                                                    "<ItemGroup><Reference Include=\"Test\"><HintPath>.\\Test.dll</HintPath></Reference></ItemGroup>", "v3.5", (fun project file ->
                let assemRef = TheTests.FindNodeWithCaption(project, "Test") :?> AssemblyReferenceNode
                Assert.IsFalse(assemRef.CanShowDefaultIcon(), "reference should be banged out, does not resolve")
                // add reference to Test.exe
                let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File, bstrFile = exe)
                let refContainer = GetReferenceContainerNode(project)
                refContainer.AddReferenceFromSelectorData(selectorData) |> (fun x -> Assert.IsNotNull(x, "expected AddReference to succeed"))
                // it should have succeeded (no throw)
                ))
            )

    [<Test>]
    member public this.``References.Bug787899.AddDuplicateResolved``() =
        // Let's create a run-of-the-mill project just to have a spare assembly around
        this.CreateDummyTestProjectBuildItAndDo(fun exe ->
            Assert.IsTrue(File.Exists exe, "failed to build exe")
            this.MakeProjectAndDoWithProjectFile(["doesNotMatter.fs"], ["mscorlib"; "System"; "System.Core"; "System.Net"], 
                                                    sprintf "<ItemGroup><Reference Include=\"Test\"><HintPath>%s</HintPath></Reference></ItemGroup>" exe, "v3.5", (fun project file ->
                let assemRef = TheTests.FindNodeWithCaption(project, "Test") :?> AssemblyReferenceNode
                Assert.IsTrue(assemRef.CanShowDefaultIcon(), "reference should not be banged out, does resolve")
                // add reference to Test.exe
                let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File, bstrFile = exe)
                let refContainer = GetReferenceContainerNode(project)
                try
                    refContainer.AddReferenceFromSelectorData(selectorData) |> ignore
                    Assert.Fail("expected AddReference to Fail")
                with :? InvalidOperationException as e ->
                    Assert.AreEqual("A reference to 'Test' (with assembly name 'Test') could not be added. A reference to the component 'Test' with the same assembly name already exists in the project.", e.Message)
                ))
            )

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.LoadedFsProj.Works``() =
        this.MakeProjectAndDo(["doesNotMatter.fs"], ["mscorlib"; "System"; "System.Core"; "System.Net"], "", "v3.5", (fun project ->
            let expectedRefInfo = [ "mscorlib", true
                                    "System", true
                                    "System.Core", true
                                    "System.Net", true ]
            let refContainer = GetReferenceContainerNode(project)
            let actualRefInfo = [
                let r = ref(refContainer.FirstChild :?> ReferenceNode)
                while !r <> null do
                    yield ((!r).Caption, ((!r).CanShowDefaultIcon()))
                    r := (!r).NextSibling :?> ReferenceNode
                ]
            AssertEqual expectedRefInfo actualRefInfo
            ))


    [<Test>]
    member public this.``ReferenceResolution.Bug4423.LoadedFsProj.WithExactDuplicates``() =
        this.MakeProjectAndDo(["doesNotMatter.fs"], ["System"; "System"], "", "v3.5", (fun project ->
            let expectedRefInfo = [ "System", true  // In C#, one will be banged out, whereas
                                    "System", true] // one will be ok, but in F# both show up as ok.  Bug?  Not worth the effort to fix.
            let refContainer = GetReferenceContainerNode(project)
            let actualRefInfo = [
                let r = ref(refContainer.FirstChild :?> ReferenceNode)
                while !r <> null do
                    yield ((!r).Caption, ((!r).CanShowDefaultIcon()))
                    r := (!r).NextSibling :?> ReferenceNode
                ]
            AssertEqual expectedRefInfo actualRefInfo
            ))

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.LoadedFsProj.WithBadDuplicates``() =
        this.MakeProjectAndDo(["doesNotMatter.fs"], ["System"; "System.dll"], "", "v3.5", (fun project ->
            let expectedRefInfo = [ "System", false     // one will be banged out
                                    "System.dll", true] // one will be ok
            let refContainer = GetReferenceContainerNode(project)
            let actualRefInfo = [
                let r = ref(refContainer.FirstChild :?> ReferenceNode)
                while !r <> null do
                    yield ((!r).Caption, ((!r).CanShowDefaultIcon()))
                    r := (!r).NextSibling :?> ReferenceNode
                ]
            AssertEqual expectedRefInfo actualRefInfo
            ))

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.LoadedFsProj.WorksWithFilenames``() =
        let edte = Path.Combine(Net20AssemExPathOnThisMachine(), "EnvDTE80.dll")
        let ssmw = Path.Combine(Net35RefAssemPathOnThisMachine(), "System.ServiceModel.Web.dll")
        this.MakeProjectAndDo(["doesNotMatter.fs"], [edte; ssmw], "", "v3.5", (fun project ->
            let expectedRefInfo = [ edte, true 
                                    ssmw, true ]
            let refContainer = GetReferenceContainerNode(project)
            let actualRefInfo = [
                let r = ref(refContainer.FirstChild :?> ReferenceNode)
                while !r <> null do
                    yield ((!r).Caption, ((!r).CanShowDefaultIcon()))
                    r := (!r).NextSibling :?> ReferenceNode
                ]
            AssertEqual expectedRefInfo actualRefInfo
            ))

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.LoadedFsProj.WeirdCases``() =
        this.MakeProjectAndDo(["doesNotMatter.fs"], ["mscorlib, Version=4.0.0.0"; "System, Version=4.0.0.0"; "System.Core, Version=4.0.0.0"; "System.Net, Version=4.0.0.0"], "", "v4.0", (fun project ->
            let expectedRefInfo = [ "mscorlib", true
                                    "System", true
                                    "System.Core, Version=4.0.0.0", false // msbuild does funny things for System.Core (TODO bug number)
                                    "System.Net", true ]
            let refContainer = GetReferenceContainerNode(project)
            let actualRefInfo = [
                let r = ref(refContainer.FirstChild :?> ReferenceNode)
                while !r <> null do
                    yield ((!r).Caption, ((!r).CanShowDefaultIcon()))
                    r := (!r).NextSibling :?> ReferenceNode
                ]
            AssertEqual expectedRefInfo actualRefInfo
            ))

    member public this.ReferenceResolutionHelper(tab : AddReferenceDialogTab, fullPath : string, expectedFsprojRegex : string) =
        this.ReferenceResolutionHelper(tab, fullPath, expectedFsprojRegex, "v3.5", [])
        
    member public this.ReferenceResolutionHelper(tab : AddReferenceDialogTab, fullPath : string, expectedFsprojRegex : string, targetFrameworkVersion : string, originalReferences : string list) =
        // Trace.Log <- "ProjectSystemReferenceResolution" // can be useful
        this.MakeProjectAndDo(["doesNotMatter.fs"], originalReferences, "", targetFrameworkVersion, (fun project ->
            let cType = match tab with
                        | AddReferenceDialogTab.DotNetTab -> VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus
                        | AddReferenceDialogTab.BrowseTab -> VSCOMPONENTTYPE.VSCOMPONENTTYPE_File
                        | _ -> failwith "unexpected"
            let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = cType, bstrFile = fullPath)
            let refContainer = GetReferenceContainerNode(project)
            refContainer.AddReferenceFromSelectorData(selectorData) |> Assert.IsNotNull
            SaveProject(project)
            let fsprojFileText = File.ReadAllText(project.FileName)
            printfn "%s" fsprojFileText
            TheTests.HelpfulAssertMatches '<' expectedFsprojRegex fsprojFileText
            ))

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.FxAssembly.NetTab.AddDuplicate1``() =
        try
            this.ReferenceResolutionHelper(AddReferenceDialogTab.DotNetTab, 
                                           Path.Combine(Net35RefAssemPathOnThisMachine(), "System.ServiceModel.Web.dll"), 
                                           @"whatever, expectation does not matter, will throw before then",
                                           "v3.5",
                                           ["System.ServiceModel.Web"])  // assembly name
            Assert.Fail("adding a duplicate reference should have failed")
        with e ->                                           
            TheTests.HelpfulAssertMatches ' ' "A reference to '.*' \\(with assembly name '.*'\\) could not be added. A reference to the component '.*' with the same assembly name already exists in the project." e.Message


    // see 5491 [<Test>]
    member public this.``ReferenceResolution.Bug4423.FxAssembly.NetTab.AddDuplicate2``() =
        try
            this.ReferenceResolutionHelper(AddReferenceDialogTab.DotNetTab, 
                                           Path.Combine(Net35RefAssemPathOnThisMachine(), "System.ServiceModel.Web.dll"), 
                                           @"whatever, expectation does not matter, will throw before then",
                                           "v3.5",
                                           ["System.ServiceModel.Web.dll"]) // filename
            Assert.Fail("adding a duplicate reference should have failed")
        with e ->                                           
            TheTests.HelpfulAssertMatches ' ' "A reference to '.*' could not be added. A reference to the component '.*' already exists in the project." e.Message

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.FxAssembly.NetTab``() =
        this.ReferenceResolutionHelper(AddReferenceDialogTab.DotNetTab, 
                                       Path.Combine(Net35RefAssemPathOnThisMachine(), "System.ServiceModel.Web.dll"), 
                                       // TODO the intent here is to match whatever C# does; below is a snapshot from July 7, 2009
                                       @"<Reference Include=""System.ServiceModel.Web"" />")

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.FxAssembly.BrowseTab.SameVersion``() =
        let sysCoreRefAssemPath = Path.Combine(Net35RefAssemPathOnThisMachine(), "System.ServiceModel.Web.dll")
        let dirName = Path.GetTempPath()
        let copy = Path.Combine(dirName, "System.ServiceModel.Web.dll")
        try
            File.Copy(sysCoreRefAssemPath, copy, true)
            this.ReferenceResolutionHelper(AddReferenceDialogTab.BrowseTab, 
                                           copy,
                                           // TODO the intent here is to match whatever C# does; below is a snapshot from July 7, 2009
                                           @"<Reference Include=""System.ServiceModel.Web"" />")
        finally
            File.Delete(copy)

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.FxAssembly.BrowseTab.DifferentVersion``() =
        let sysCoreRefAssemPath = Path.Combine(Net35RefAssemPathOnThisMachine(), "System.ServiceModel.Web.dll")
        let dirName = Path.GetTempPath()
        let copy = Path.Combine(dirName, "System.ServiceModel.Web.dll")
        try
            File.Copy(sysCoreRefAssemPath, copy, true)
            this.ReferenceResolutionHelper(AddReferenceDialogTab.BrowseTab, 
                                           copy,
                                           // TODO the intent here is to match whatever C# does; below is a snapshot from July 7, 2009
                                           @"<Reference Include=""System.ServiceModel.Web"" />",
                                           "v4.0",  // TargetFramework is 4.0, but browsing to 3.5 reference assembly
                                           [])
        finally
            File.Delete(copy)


    [<Test>]
    member public this.``ReferenceResolution.Bug4423.NonFxAssembly.NetTab``() =
        this.ReferenceResolutionHelper(AddReferenceDialogTab.DotNetTab, 
                                       Path.Combine(Net20AssemExPathOnThisMachine(), "EnvDTE80.dll"),
                               (* TODO, no NoPIA support yet, so *)
                                       @"<Reference Include=""EnvDTE80, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"" />")
                               (* instead of below
                                       // TODO the intent here is to match whatever C# does; below is a snapshot from July 7, 2009
                                       @"<Reference Include=""EnvDTE80, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"">"
                                     + @"\s*<EmbedInteropTypes>True</EmbedInteropTypes>"
                                     + @"\s*</Reference>")
                               *)

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.NonFxAssembly.BrowseTab.SameVersion``() =
        let envDte80RefAssemPath = Path.Combine(Net20AssemExPathOnThisMachine(), "EnvDTE80.dll")
        let dirName = Path.GetTempPath()
        let copy = Path.Combine(dirName, "EnvDTE80.dll")
        try
            File.Copy(envDte80RefAssemPath, copy, true)
            this.ReferenceResolutionHelper(AddReferenceDialogTab.BrowseTab, 
                                           copy,
                                   (*
                                   For other cases, we mimic C#, but C# has a bug in this case.  Correct result is
                                   *)
                                           @"<Reference Include=""EnvDTE80"">"
                                         // TODO no NoPIA support yet: + @"\s*<EmbedInteropTypes>True</EmbedInteropTypes>"
                                         + @"\s*<HintPath>\.\.\\EnvDTE80.dll</HintPath>"
                                         + @"\s*</Reference>")
                                   (* whereas C# has this: 
                                           // TODO the intent here is to match whatever C# does; below is a snapshot from July 7, 2009
                                           @"<Reference Include=""EnvDTE80, Version=8.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"">"
                                         + @"\s*<SpecificVersion>False</SpecificVersion>"
                                         + @"\s*<EmbedInteropTypes>True</EmbedInteropTypes>"
                                         + (sprintf @"\s*<HintPath>%s</HintPath>" (Regex.Escape copy))
                                         + @"\s*</Reference>")
                                   *)
        finally
            File.Delete(copy)

    /// Create a dummy project named 'Test', build it, and then call k with the full path to the resulting exe
    member public this.CreateDummyTestProjectBuildItAndDo(k : string -> unit) =
        this.MakeProjectAndDo(["foo.fs"], [], "", (fun project ->
        // Let's create a run-of-the-mill project just to have a spare assembly around
        let fooPath = Path.Combine(project.ProjectFolder, "foo.fs")
        File.AppendAllText(fooPath, "namespace Foo\nmodule Bar =\n  let x = 42")
        let buildResult = project.Build("Build")
        Assert.IsTrue buildResult.IsSuccessful
        let exe = Path.Combine(project.ProjectFolder, "bin\\Debug\\Test.exe")
        k exe))

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.NonFxAssembly.BrowseTab.RelativeHintPath.InsideProjectDir``() =
        // Let's create a run-of-the-mill project just to have a spare assembly around
        this.CreateDummyTestProjectBuildItAndDo(fun exe ->
            Assert.IsTrue(File.Exists exe, "failed to build exe")
            // Now let's create an assembly reference to it and ensure we get expected relative HintPath
            let expectedFsprojRegex = @"<Reference Include=""Test"">"
                                         + @"\s*<HintPath>Test.exe</HintPath>"  // in this directory
                                         + @"\s*</Reference>"
            this.MakeProjectAndDo(["bar.fs"], [], "", "v3.5", (fun project ->
                let exeCopy = Path.Combine(project.ProjectFolder, "Test.exe")
                File.Copy(exe, exeCopy, true)
                Assert.IsTrue(File.Exists exeCopy, "failed to build exe")
                let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File, bstrFile = exeCopy)
                let refContainer = GetReferenceContainerNode(project)
                refContainer.AddReferenceFromSelectorData(selectorData) |> Assert.IsNotNull
                SaveProject(project)
                let fsprojFileText = File.ReadAllText(project.FileName)
                printfn "%s" fsprojFileText
                TheTests.HelpfulAssertMatches '<' expectedFsprojRegex fsprojFileText
                // Finally, ensure that the reference works as expected
                project.Reload()
                let assemRef = TheTests.FindNodeWithCaption(project, "Test") :?> AssemblyReferenceNode
                Assert.IsTrue(assemRef.CanShowDefaultIcon(), "the reference could not be resolved")  
                // Use the referenced DLL as a double-check
                let barPath = Path.Combine(project.ProjectFolder, "bar.fs")
                File.AppendAllText(barPath, "printfn \"%d\" Foo.Bar.x")  // code that requires the referenced assembly to successfully compile
                let buildResult = project.Build("Build")
                Assert.IsTrue buildResult.IsSuccessful
                ))
        )
        
    [<Test>]
    member public this.``ReferenceResolution.Bug4423.NonFxAssembly.BrowseTab.RelativeHintPath.OutsideProjectDir``() =
        this.MakeProjectAndDo(["foo.fs"], [], "", (fun project ->
            // Let's create a run-of-the-mill 
            let fooPath = Path.Combine(project.ProjectFolder, "foo.fs")
            File.AppendAllText(fooPath, "namespace Foo\nmodule Bar =\n  let x = 42")
            let buildResult = project.Build("Build")
            Assert.IsTrue buildResult.IsSuccessful
            let exe = Path.Combine(project.ProjectFolder, "bin\\Debug\\Test.exe")
            Assert.IsTrue(File.Exists exe, "failed to build exe")
            // Now let's create an assembly reference to it and ensure we get expected relative HintPath
            let expectedFsprojRegex = @"<Reference Include=""Test"">"
                                         + @"\s*<HintPath>\.\.\\.*?</HintPath>"  // the point is, some path start with "..\", since both projects are rooted somewhere in the temp directory (where unit tests create them)
                                         + @"\s*</Reference>"
            this.MakeProjectAndDo(["bar.fs"], [], "", "v3.5", (fun project ->
                let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File, bstrFile = exe)
                let refContainer = GetReferenceContainerNode(project)
                refContainer.AddReferenceFromSelectorData(selectorData) |> Assert.IsNotNull
                SaveProject(project)
                let fsprojFileText = File.ReadAllText(project.FileName)
                printfn "%s" fsprojFileText
                TheTests.HelpfulAssertMatches '<' expectedFsprojRegex fsprojFileText
                // Finally, ensure that the reference works as expected
                project.Reload()
                let assemRef = TheTests.FindNodeWithCaption(project, "Test") :?> AssemblyReferenceNode
                Assert.IsTrue(assemRef.CanShowDefaultIcon(), "the reference could not be resolved")  
                // Use the referenced DLL as a double-check
                let barPath = Path.Combine(project.ProjectFolder, "bar.fs")
                File.AppendAllText(barPath, "printfn \"%d\" Foo.Bar.x")  // code that requires the referenced assembly to successfully compile
                let buildResult = project.Build("Build")
                Assert.IsTrue buildResult.IsSuccessful
                ))
        ))

    [<Test>]
    member public this.``ReferenceResolution.Bug4423.NotAValidDll.BrowseTab``() =
        let dirName = Path.GetTempPath()
        let dll = Path.Combine(dirName, "Foo.dll")
        File.AppendAllText(dll, "This is not actually a valid dll")
        try
            this.MakeProjectAndDo(["doesNotMatter.fs"], [], "", "v3.5", (fun project ->
                let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File, bstrFile = dll)
                let refContainer = GetReferenceContainerNode(project)
                try
                    refContainer.AddReferenceFromSelectorData(selectorData) |> Assert.IsNotNull
                    Assert.Fail("this should not have succeeded")
                with e ->
                    AssertContains e.Message "could not be added. Please make sure that the file is accessible, and that it is a valid assembly or COM component."
            ))
        finally
            File.Delete(dll)

    [<Test>]
    member public this.``PathReferences.Existing`` () =
        DoWithTempFile "Test.fsproj"(fun projFile ->
            let dirName = Path.GetDirectoryName(projFile)
            let libDirName = Directory.CreateDirectory(Path.Combine(dirName, "lib")).FullName
            let codeBase = (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath |> Path.GetDirectoryName
            let refLibPath = Path.Combine(libDirName, "nunit.core.dll")
            File.Copy(Path.Combine(codeBase, "nunit.core.dll"), refLibPath)
            File.AppendAllText(projFile, TheTests.SimpleFsprojText([], [refLibPath], ""))
            use project = TheTests.CreateProject(projFile) 
            let l = new List<AssemblyReferenceNode>()
            project.FindNodesOfType(l)
            AssertEqual 1 l.Count
            AssertEqual refLibPath l.[0].Url
            AssertEqual refLibPath l.[0].Caption  // when Include is a filename, entirety is caption
            Assert.IsNotNull(l.[0].ResolvedAssembly)
            let refContainer =
                let l = new List<ReferenceContainerNode>()
                project.FindNodesOfType(l)
                l.[0]
            let mscorlibPath = (new Uri("".GetType().Assembly.CodeBase)).LocalPath
            let selectorData = new VSCOMPONENTSELECTORDATA(``type`` = VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus, bstrFile = mscorlibPath)
            refContainer.AddReferenceFromSelectorData(selectorData) |> Assert.IsNotNull
            let l = new List<AssemblyReferenceNode>()
            project.FindNodesOfType(l)
            AssertEqual 2 l.Count
            AssertEqual refLibPath l.[0].Url
            AssertEqual refLibPath l.[0].Caption
            AssertEqual "mscorlib" l.[1].Caption
        )

    [<Test>]
    member public this.``PathReferences.Existing.Captions`` () =
        DoWithTempFile "Test.fsproj"(fun projFile ->
            File.AppendAllText(projFile, TheTests.FsprojTextWithProjectReferences(
                [], // <Compile>
                ["$(LetterS)ystem.dll"; "System.Net.dll"], // <Reference>
                [], // <ProjectReference>
                "<PropertyGroup><LetterS>S</LetterS></PropertyGroup>"))  // other stuff
            use project = TheTests.CreateProject(projFile) 
            let l = new List<AssemblyReferenceNode>()
            project.FindNodesOfType(l)
            AssertEqual 2 l.Count
            AssertEqual "System.dll" l.[0].Caption
            Assert.IsNotNull(l.[0].ResolvedAssembly)
            AssertEqual "System.Net.dll" l.[1].Caption
            Assert.IsNotNull(l.[1].ResolvedAssembly)
        )
        
    [<Test>]
    member public this.``PathReferences.NonExistent`` () =
        DoWithTempFile "Test.fsproj"(fun projFile ->
            let refLibPath = @"c:\foo\bar\blahblah.dll"
            File.AppendAllText(projFile, TheTests.SimpleFsprojText([], [refLibPath], ""))
            use project = TheTests.CreateProject(projFile) 
            let l = new List<AssemblyReferenceNode>()
            project.FindNodesOfType(l)
            AssertEqual 1 l.Count
            AssertEqual refLibPath l.[0].Caption
            Assert.IsNull(l.[0].ResolvedAssembly)
        )

        
    [<Test>]
    member public this.``FsprojPreferencePage.ProjSupportsPrefReadWrite``() =
        let testProp = "AssemblyName"
        let compileItem = [@"foo.fs"]
        
        DoWithTempFile "Test.fsproj" (fun projFile ->
            File.AppendAllText(projFile, TheTests.SimpleFsprojText(compileItem, [], "")) 
            use project = TheTests.CreateProject(projFile) 
            // Read a known property from the project node - AssemblyName
            let propertyVal = project.GetProjectProperty(testProp, false)
            // Set the project property to something different (is currently "MyAssembly")
            let newPropVal = "Foo_PROPVAL_Foo" // hopefully unique?
            project.SetProjectProperty(testProp, newPropVal)
            // get the (hopefully) modified property name
            let propertyVal' = project.GetProjectProperty(testProp, false)
            let newProjFileName = (Path.GetDirectoryName projFile) + "\\" + "fooProj.fsproj"
            
            printfn "%s before modification: %s" testProp propertyVal 
            printfn "%s after modification:  %s" testProp propertyVal' 
            
            // Assert that the value has changed
            AssertNotEqual propertyVal propertyVal'
            // Assert that the new value is what we expect it to be 
            AssertEqual newPropVal propertyVal'
            
            // Save as a new project file
            project.SaveMSBuildProjectFileAs(newProjFileName) ; // cleaned up by parent call to DoWithTempFile
            
            // look for the new property inside of the project file
            let contents = File.ReadAllText(newProjFileName)
            AssertContains contents newPropVal
        )