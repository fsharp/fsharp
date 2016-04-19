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


// Mark the main thread as STAThread since it is a GUI thread
[<EntryPoint>]
[<STAThread()>]
#if !NO_LOADER_OPTIMIZATION
[<LoaderOptimization(LoaderOptimization.MultiDomainHost)>]     
#endif
let FsiMain argv = 
    MainMain argv





