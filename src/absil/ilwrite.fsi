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

/// The IL Binary writer 
module internal Microsoft.FSharp.Compiler.AbstractIL.ILBinaryWriter 

open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.IL 

[<Sealed>]
type ILStrongNameSigner =
    member PublicKey: byte[]
    static member OpenPublicKeyFile: string -> ILStrongNameSigner
    static member OpenPublicKey: byte[] -> ILStrongNameSigner
    static member OpenKeyPairFile: string -> ILStrongNameSigner
    static member OpenKeyContainer: string -> ILStrongNameSigner

type options =
 { mscorlib: ILScopeRef;
   pdbfile: string option;
   signer : ILStrongNameSigner option;
   fixupOverlappingSequencePoints : bool;
   emitTailcalls: bool;
   showTimes : bool;
   dumpDebugInfo : bool }

/// Write a binary to the file system. Extra configuration parameters can also be specified. 
val WriteILBinary: 
    filename: string ->
    options:  options ->
    input:    ILModuleDef -> 
    noDebugData: bool ->
    unit



