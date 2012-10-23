//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module internal Microsoft.FSharp.Compiler.Interactive.Main

open System
open Microsoft.FSharp.Compiler.Interactive.Shell
open Internal.Utilities

#nowarn "55"

[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: System.CLSCompliant(true)>]  
do()

#if SILVERLIGHT
#else   

// Mark the main thread as STAThread since it is a GUI thread
[<EntryPoint>]
[<STAThread()>]    
let MainMain argv = 
    ignore argv
    let argv = System.Environment.GetCommandLineArgs()

    // When VFSI is running, set the input/output encoding to UTF8.
    // Otherwise, unicode gets lost during redirection.
    // It is required only under Net4.5 or above (with unicode console feature).
    if FSharpEnvironment.IsRunningOnNetFx45OrAbove && 
        argv |> Array.exists (fun x -> x.Contains "fsi-server") then
        Console.InputEncoding <- System.Text.Encoding.UTF8 
        Console.OutputEncoding <- System.Text.Encoding.UTF8

#if DEBUG  
    if argv |> Array.exists  (fun x -> x = "/pause" || x = "--pause") then 
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey() |> ignore

    try
      let fsi = FsiEvaluationSession (argv, Console.In, Console.Out, Console.Error)
      fsi.Run() 
    with e -> printf "Exception by fsi.exe:\n%+A\n" e
#else
    let fsi = FsiEvaluationSession (argv, Console.In, Console.Out, Console.Error)
    fsi.Run() 
#endif

    0
#endif // SILVERLIGHT





