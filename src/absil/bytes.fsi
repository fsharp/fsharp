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


/// Blobs of bytes, cross-compiling 
namespace Microsoft.FSharp.Compiler.AbstractIL.Internal

open Internal.Utilities

open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 


module internal Bytes = 
    /// returned int will be 0 <= x <= 255
    val get: byte[] -> int -> int    
    val zeroCreate: int -> byte[]
    /// each int must be 0 <= x <= 255 
    val ofInt32Array: int[] ->  byte[] 
    /// each int will be 0 <= x <= 255 

    val blit: byte[] -> int -> byte[] -> int -> int -> unit

    val stringAsUnicodeNullTerminated: string -> byte[]
    val stringAsUtf8NullTerminated: string -> byte[]


/// Imperative buffers and streams of byte[]
[<Sealed>]
type internal ByteBuffer = 
    member Close : unit -> byte[] 
    member EmitIntAsByte : int -> unit
    member EmitIntsAsBytes : int[] -> unit
    member EmitByte : byte -> unit
    member EmitBytes : byte[] -> unit
    member EmitInt32 : int32 -> unit
    member EmitInt64 : int64 -> unit
    member FixupInt32 : pos: int -> value: int32 -> unit
    member EmitInt32AsUInt16 : int32 -> unit
    member EmitBoolAsByte : bool -> unit
    member EmitUInt16 : uint16 -> unit
    member Position : int
    static member Create : int -> ByteBuffer


[<Sealed>]
type internal ByteStream =
    member ReadByte : unit -> byte
    member ReadBytes : int -> byte[]
    member ReadUtf8String : int -> string
    member Position : int 
    static member FromBytes : byte[] * start:int * length:int -> ByteStream
    
#if LAZY_UNPICKLE
    member CloneAndSeek : int -> ByteStream
    member Skip : int -> unit
#endif
