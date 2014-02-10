#r "packages/FAKE/tools/FakeLib.dll"
#load "packages/SourceLink.Fake/tools/SourceLink.fsx"

open System
open System.IO
open Fake
open Fake.AssemblyInfoFile
open SourceLink

Target "SourceLink" (fun _ ->
    use repo = new GitRepo(__SOURCE_DIRECTORY__)
    !! "lib/release/*.pdb" 
    |> Seq.iter (fun pdb ->
        let nm = Path.GetFileNameWithoutExtension pdb
        let proj = VsProj.LoadRelease (sprintf @"src\fsharp\%s\%s.fsproj" nm nm)
        logfn "source linking %s" pdb
        let compiles = proj.Compiles.SetBaseDirectory __SOURCE_DIRECTORY__
        let gitFiles =
            compiles
            -- "src/assemblyinfo/assemblyinfo*.fs" // not source indexed
            // generated and in fsproj as Compile, but in .gitignore, not source indexed
            -- "src/fsharp/FSharp.Compiler/illex.fs" // <FsLex Include="..\..\absil\illex.fsl">
            -- "src/fsharp/FSharp.Compiler/ilpars.fs"
            -- "src/fsharp/FSharp.Compiler/lex.fs"
            -- "src/fsharp/FSharp.Compiler/pars.fs"
        repo.VerifyChecksums gitFiles
        let pdbFiles =
            compiles // generated, not in the fsproj as Compile, not source indexed
            ++ "src/fsharp/fscmain.fs" //<FsSrGen Include="FSBuild.txt">
            ++ "src/fsharp/fsharp.build/obj/release/fsbuild.fs"
            ++ "src/fsharp/fsharp.compiler.interactive.settings/obj/release/fsinteractivesettings.fs"
            ++ "src/absil/illex.fsl"
            ++ "src/absil/ilpars.fsy"
            ++ "src/fsharp/fsharp.compiler/obj/release/fscomp.fs"
            ++ "src/fsharp/lex.fsl"
            ++ "src/fsharp/pars.fsy"
            ++ "src/fsharp/fsharp.compiler.server.shared/obj/release/fsservershared.fs"
            ++ "src/fsharp/fsharp.data.typeproviders/obj/release/fsdata.fs"
            ++ "src/fsharp/fsi/obj/release/fsistrings.fs"
        proj.VerifyPdbChecksums pdbFiles
        proj.CreateSrcSrv "https://raw.github.com/fsharp/fsharp/{0}/%var2%" repo.Revision (repo.Paths gitFiles)
        Pdbstr.exec pdb proj.OutputFilePdbSrcSrv
    )
)

RunTargetOrDefault "SourceLink"