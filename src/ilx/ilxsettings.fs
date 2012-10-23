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

module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.IlxSettings 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX

type IlxCallImplementation = 
  | VirtEntriesVirtCode

//++GLOBAL MUTABLE STATE
let ilxCompilingFSharpCoreLib = ref false

//++GLOBAL MUTABLE STATE
let ilxFsharpCoreLibAssemRef = ref (None : ILAssemblyRef option)

/// Scope references for FSharp.Core.dll
let ilxFsharpCoreLibScopeRef () =
    if !ilxCompilingFSharpCoreLib then 
        ILScopeRef.Local 
    else 
        let assref = 
            match !ilxFsharpCoreLibAssemRef with 
            | Some o -> o
            | None -> 
                 // The exact public key token and version used here don't actually matter, or shouldn't.
                 // ilxFsharpCoreLibAssemRef is only 'None' for startup code paths such as
                 // IsSignatureDataVersionAttr, where matching is done by assembly name strings
                 // rather then versions and tokens.
                ILAssemblyRef.Create("FSharp.Core", None, 
                                     Some (PublicKeyToken(Bytes.ofInt32Array [| 0xb0; 0x3f; 0x5f; 0x7f; 0x11; 0xd5; 0x0a; 0x3a |])),
                                     false, 
                                     Some (IL.parseILVersion "0.0.0.0"), None)
        ILScopeRef.Assembly assref

let ilxNamespace () =  "Microsoft.FSharp.Core"