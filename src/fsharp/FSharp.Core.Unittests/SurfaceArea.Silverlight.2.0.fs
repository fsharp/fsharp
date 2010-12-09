namespace FSharp.Core.Unittests.SurfaceArea

open NUnit.Framework

(* This is unused yet, as I could not get it working (always a problem with System.dll)

   Will need to add to .proj file:

  <PropertyGroup>
    <FSharpSuiteBin>$(_NTTREE)\SuiteBin\FSharp</FSharpSuiteBin>
  </PropertyGroup>

  <Target Name="CopySilverlightDlls" AfterTargets="CopyToSuiteBin" >
    <!-- SurfaceArea.Silverlight.2.0.fs needs below DLLs alongside it to do its job -->
    <MakeDir Directories="$(FSharpSuiteBin)"/>
    <Copy SourceFiles="..\..\devdiv\tools\Silverlight 2.0.30523.8\System.dll;..\..\devdiv\tools\Silverlight 2.0.30523.8\System.Net.dll" 
          DestinationFolder="$(FSharpSuiteBin)" SkipUnchangedFiles="true"/>
  </Target>

*)

// We don't have auotmated unit tests for Silverlight, so do a reflection-only version of silverlight checking as part of
// the 4.0 tests.
[<TestFixture>]
type SilverlightSurfaceAreaTest() =
    [<Test>]
    member this.VerifyArea() =
        let file = typeof<int list>.Assembly.Location 
        let asm = System.Reflection.Assembly.ReflectionOnlyLoadFrom(file)
        let referenced = asm.GetReferencedAssemblies()

        let resolveSpecial = ref false
        System.AppDomain.CurrentDomain.add_ReflectionOnlyAssemblyResolve(fun sender args ->
            printfn "got here"
            if !resolveSpecial then
                resolveSpecial := false
                System.Reflection.Assembly.ReflectionOnlyLoadFrom(@".\System.dll")
            else null
            )

        // src\fsharp\projects\FSharp.Core.Unittests\FSharp.Core.Unittests.fsproj will copy these silverlight DLLs
        // alongside the unit tests, so this can work
        System.Reflection.Assembly.ReflectionOnlyLoadFrom(@".\System.Net.dll") |> ignore
        // there's already a System.dll in the process, do magic
        resolveSpecial := true
        //System.Reflection.Assembly.ReflectionOnlyLoadFrom(@".\System.dll") |> ignore

        let types = asm.GetExportedTypes()

        let actual = new System.Text.StringBuilder()
        actual.Append("\r\n") |> ignore

        let values = 
            types 
            |> Array.collect (fun t -> t.GetMembers())
            |> Array.map (fun v -> sprintf "%s: %s" (v.ReflectedType.ToString()) (v.ToString()))
            |> Array.sort
            |> Array.iter (fun s -> actual.Append(s) |> ignore
                                    actual.Append("\r\n") |> ignore)

        let expected = @"
"

        let act = actual.ToString()
        if expected <> act then
            printf "%s" act
        Assert.AreEqual(expected, act)
