// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

/// The ILPdbWriter 
module internal Microsoft.FSharp.Compiler.AbstractIL.ILPdbWriter 

open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Range
open System.Collections.Generic 
open System.IO
open System.Reflection.Metadata

type PdbDocumentData = ILSourceDocument

type PdbLocalVar = 
    { Name: string
      Signature: byte[] 
      /// the local index the name corresponds to
      Index: int32  }

type PdbMethodScope = 
    { Children: PdbMethodScope array
      StartOffset: int
      EndOffset: int
      Locals: PdbLocalVar array
      (* REVIEW open_namespaces: pdb_namespace array *) }

type PdbSourceLoc = 
    { Document: int
      Line: int
      Column: int }
      
type PdbSequencePoint = 
    { Document: int
      Offset: int
      Line: int
      Column: int
      EndLine: int
      EndColumn: int }
     override ToString: unit -> string

type PdbMethodData = 
    { MethToken: int32
      MethName:string
      LocalSignatureToken: int32
      Params: PdbLocalVar array
      RootScope: PdbMethodScope
      Range: (PdbSourceLoc * PdbSourceLoc) option
      SequencePoints: PdbSequencePoint array }

[<NoEquality; NoComparison>]
type PdbData = 
    { EntryPoint: int32 option
      Timestamp: int32
      ModuleID: byte[]                                              // MVID of the generated .NET module (used by MDB files to identify debug info)
      Documents: PdbDocumentData[]
      Methods: PdbMethodData[] 
      TableRowCounts: int[] }


/// Takes the output file name and returns debug file name.
val getDebugFileName: string -> bool -> string

/// 28 is the size of the IMAGE_DEBUG_DIRECTORY in ntimage.h 
val sizeof_IMAGE_DEBUG_DIRECTORY : System.Int32
val logDebugInfo : string -> PdbData -> unit

#if ENABLE_MONO_SUPPORT
val writeMdbInfo<'a> : string -> string -> PdbData -> 'a
#endif

type BinaryChunk = 
    { size: int32 
      addr: int32 }

type idd =
    { iddCharacteristics: int32;
      iddMajorVersion: int32; (* actually u16 in IMAGE_DEBUG_DIRECTORY *)
      iddMinorVersion: int32; (* actually u16 in IMAGE_DEBUG_DIRECTORY *)
      iddType: int32;
      iddTimestamp: int32;
      iddData: byte[];
      iddChunk: BinaryChunk }

val generatePortablePdb : embedAllSource:bool -> embedSourceList:string list -> sourceLink: string -> showTimes:bool -> info:PdbData -> (int64 * BlobContentId * MemoryStream) 
val compressPortablePdbStream : uncompressedLength:int64 -> contentId:BlobContentId -> stream:MemoryStream -> (int64 * BlobContentId * MemoryStream)
val embedPortablePdbInfo : uncompressedLength:int64 -> contentId:BlobContentId -> stream:MemoryStream -> showTimes:bool -> fpdb:string -> cvChunk:BinaryChunk -> pdbChunk:BinaryChunk -> idd[]
val writePortablePdbInfo : contentId:BlobContentId -> stream:MemoryStream -> showTimes:bool -> fpdb:string -> cvChunk:BinaryChunk -> idd[]

#if !FX_NO_PDB_WRITER
val writePdbInfo : showTimes:bool -> f:string -> fpdb:string -> info:PdbData -> cvChunk:BinaryChunk -> idd[]
#endif
