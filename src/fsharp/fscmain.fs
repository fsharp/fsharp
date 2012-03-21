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


module internal Microsoft.FSharp.Compiler.CommandLineMain

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL.IL (* runningOnMono *)
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Driver
open System.Runtime.CompilerServices


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

