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


module internal Microsoft.FSharp.Compiler.CommandLineMain

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL.IL (* runningOnMono *)
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Driver
open Internal.Utilities
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Build
open System.Runtime.CompilerServices

type TypeInThisAssembly() = member x.Dummy = 1

module Driver = 
    let main argv = 
        // Check for --pause as the very first step so that a compiler can be attached here.
        if argv |> Array.exists  (fun x -> x = "/pause" || x = "--pause") then 
            System.Console.WriteLine("Press any key to continue...")
            System.Console.ReadKey() |> ignore
        if argv |> Array.exists  (fun x -> x = "/resident" || x = "--resident") then 
            let argv = argv |> Array.filter (fun x -> x <> "/resident" && x <> "--resident")

            if not (argv |> Array.exists (fun x -> x = "/nologo" || x = "--nologo")) then 
                printfn "%s" (FSComp.SR.buildProductName(FSharpEnvironment.DotNetBuildString))
                printfn "%s" (FSComp.SR.optsCopyright())

            let fscServerExe = typeof<TypeInThisAssembly>.Assembly.Location
            let exitCodeOpt = FSharpResidentCompiler.FSharpCompilationServer.TryCompileUsingServer(fscServerExe,argv)
            match exitCodeOpt with 
            | Some exitCode -> exitCode
            | None -> 
                let exiter = QuitProcessExiter
                let createErrorLogger = (fun tcConfigB -> ErrorLoggerThatQuitsAfterMaxErrors(tcConfigB, exiter))
                mainCompile (argv, true, exiter, createErrorLogger)
                0

        elif argv |> Array.exists  (fun x -> x = "/server" || x = "--server") then 
            // Install the right exiter so we can catch "StopProcessing" without exiting the server
            let exiter = { new Exiter with member x.Exit n = raise StopProcessing }
            FSharpResidentCompiler.FSharpCompilationServer.RunServer(exiter)        
            0
        
        else
            let exiter = QuitProcessExiter
            let createErrorLogger = (fun tcConfigB -> ErrorLoggerThatQuitsAfterMaxErrors(tcConfigB, exiter))
            mainCompile (argv, false, QuitProcessExiter, createErrorLogger)
            0 




[<Dependency("FSharp.Compiler",LoadHint.Always)>] 
do ()

[<EntryPoint>]
let main(argv) =
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parameter)    
    if not runningOnMono then Lib.UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption() (* SDL recommendation *)

    try 
        Driver.main(Array.append [| "fsc.exe" |] argv); 
    with e -> 
        errorRecovery e Microsoft.FSharp.Compiler.Range.range0; 
        1

