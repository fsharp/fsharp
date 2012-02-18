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


module internal Microsoft.FSharp.Compiler.AbstractIL.ILBinaryWriter 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types  
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.BinaryConstants 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Support 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.DiagnosticMessage
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Range

open System.Collections.Generic 
open System.IO

#if DEBUG
let showEntryLookups = false
#endif

//---------------------------------------------------------------------
// Library
//---------------------------------------------------------------------

let reportTime =
#if SILVERLIGHT
    (fun _ _ -> ())
#else
    let tFirst = ref None     
    let tPrev = ref None     
    fun showTimes descr ->
        if showTimes then 
            let t = System.Diagnostics.Process.GetCurrentProcess().UserProcessorTime.TotalSeconds
            let prev = match !tPrev with None -> 0.0 | Some t -> t
            let first = match !tFirst with None -> (tFirst := Some t; t) | Some t -> t
            dprintf "ilwrite: TIME %10.3f (total)   %10.3f (delta) - %s\n" (t - first) (t - prev) descr;
            tPrev := Some t
#endif

//---------------------------------------------------------------------
// Byte, byte array fragments and other concrete representations
// manipulations.
//---------------------------------------------------------------------

// Little-endian encoding of int32 
let b0 n =  byte (n &&& 0xFF)
let b1 n =  byte ((n >>> 8) &&& 0xFF)
let b2 n =  byte ((n >>> 16) &&& 0xFF)
let b3 n =  byte ((n >>> 24) &&& 0xFF)

// Little-endian encoding of int64 
let dw7 n = byte ((n >>> 56) &&& 0xFFL)
let dw6 n = byte ((n >>> 48) &&& 0xFFL)
let dw5 n = byte ((n >>> 40) &&& 0xFFL)
let dw4 n = byte ((n >>> 32) &&& 0xFFL)
let dw3 n = byte ((n >>> 24) &&& 0xFFL)
let dw2 n = byte ((n >>> 16) &&& 0xFFL)
let dw1 n = byte ((n >>> 8)  &&& 0xFFL)
let dw0 n = byte (n &&& 0xFFL)

let bitsOfSingle (x:float32) = System.BitConverter.ToInt32(System.BitConverter.GetBytes(x),0)
let bitsOfDouble (x:float) = System.BitConverter.DoubleToInt64Bits(x)

let emitBytesViaBuffer f = let bb = ByteBuffer.Create 10 in f bb; bb.Close()

/// Alignment and padding
let align alignment n = ((n + alignment - 1) / alignment) * alignment

//---------------------------------------------------------------------
// Concrete token representations etc. used in PE files
//---------------------------------------------------------------------


type ByteBuffer with 

    /// Z32 = compressed unsigned integer 
    static member Z32Size n = 
      if n <= 0x7F then 1
      elif n <= 0x3FFF then 2
      else 4

    /// Emit int32 as compressed unsigned integer
    member buf.EmitZ32 n = 
        if n >= 0 &&  n <= 0x7F then 
            buf.EmitIntAsByte n  
        elif n >= 0x80 && n <= 0x3FFF then 
            buf.EmitIntAsByte (0x80 ||| (n >>> 8));
            buf.EmitIntAsByte (n &&& 0xFF) 
        else 
            buf.EmitIntAsByte (0xc0l ||| ((n >>> 24) &&& 0xFF));
            buf.EmitIntAsByte (           (n >>> 16) &&& 0xFF);
            buf.EmitIntAsByte (           (n >>> 8)  &&& 0xFF);
            buf.EmitIntAsByte (            n         &&& 0xFF)

    member buf.EmitPadding n = 
        for i = 0 to n-1 do
            buf.EmitByte 0x0uy

    // Emit compressed untagged integer
    member buf.EmitZUntaggedIndex big idx = 
        if big then buf.EmitInt32 idx
        elif idx > 0xffff then failwith "EmitZUntaggedIndex: too big for small address or simple index"
        else buf.EmitInt32AsUInt16 idx

    // Emit compressed tagged integer
    member buf.EmitZTaggedIndex tag nbits big idx =
        let idx2 = (idx <<< nbits) ||| tag
        if big then buf.EmitInt32 idx2
        else buf.EmitInt32AsUInt16 idx2

let getUncodedToken (tab:TableName) idx = ((tab.Index <<< 24) ||| idx)

// From ECMA for UserStrings:
// This final byte holds the value 1 if and only if any UTF16 character within the string has any bit set in its top byte, or its low byte is any of the following:
// 0x01�0x08, 0x0E�0x1F, 0x27, 0x2D,
// 0x7F. Otherwise, it holds 0. The 1 signifies Unicode characters that require handling beyond that normally provided for 8-bit encoding sets.

// HOWEVER, there is a discrepancy here between the ECMA spec and the Microsoft C# implementation. The code below follows the latter. We�ve raised the issue with both teams. See Dev10 bug 850073 for details.

let markerForUnicodeBytes (b:byte[]) = 
    let len = b.Length
    let rec scan i = 
        i < len/2 && 
        (let b1 = Bytes.get b (i*2)
         let b2 = Bytes.get b (i*2+1)
         (b2 <> 0)
         || (b1 >= 0x01 && b1 <= 0x08)   // as per ECMA and C#
         || (b1 >= 0xE && b1 <= 0x1F)    // as per ECMA and C#
         || (b1 = 0x27)                  // as per ECMA and C#
         || (b1 = 0x2D)                  // as per ECMA and C#
         || (b1 > 0x7F)                  // as per C# (but ECMA omits this)
         || scan (i+1))
    let marker = if scan 0 then 0x01 else 0x00
    marker


// -------------------------------------------------------------------- 
// Fixups
// -------------------------------------------------------------------- 

/// Check that the data held at a fixup is some special magic value, as a sanity check
/// to ensure the fixup is being placed at a ood lcoation.
let checkFixup32 (data: byte[]) offset exp = 
    if data.[offset + 3] <> b3 exp then failwith "fixup sanity check failed";
    if data.[offset + 2] <> b2 exp then failwith "fixup sanity check failed";
    if data.[offset + 1] <> b1 exp then failwith "fixup sanity check failed";
    if data.[offset] <> b0 exp then failwith "fixup sanity check failed"

let applyFixup32 (data:byte[]) offset v = 
    data.[offset] <-   b0 v;
    data.[offset+1] <- b1 v;
    data.[offset+2] <- b2 v;
    data.[offset+3] <- b3 v

// -------------------------------------------------------------------- 
// PDB data
// --------------------------------------------------------------------  

type PdbDocumentData = ILSourceDocument

type PdbLocalVar = 
    { Name: string;
      Signature: byte[]; 
      /// the local index the name corresponds to
      Index: int32  }

type PdbMethodScope = 
    { Children: PdbMethodScope array;
      StartOffset: int;
      EndOffset: int;
      Locals: PdbLocalVar array; }

type PdbSourceLoc = 
    { Document: int;
      Line: int;
      Column: int; }
      
type PdbSequencePoint = 
    { Document: int;
      Offset: int;
      Line: int;
      Column: int;
      EndLine: int;
      EndColumn: int; }
    override x.ToString() = sprintf "(%d,%d)-(%d,%d)" x.Line x.Column x.EndLine x.EndColumn

type PdbMethodData = 
    { MethToken: int32;
      MethName:string;
      Params: PdbLocalVar array;
      RootScope: PdbMethodScope;
      Range: (PdbSourceLoc * PdbSourceLoc) option;
      SequencePoints: PdbSequencePoint array; }

module SequencePoint = 
    let orderBySource sp1 sp2 = 
        let c1 = compare sp1.Document sp2.Document
        if c1 <> 0 then c1 else 
        let c1 = compare sp1.Line sp2.Line
        if c1 <> 0 then c1 else 
        compare sp1.Column sp2.Column 
        
    let orderByOffset sp1 sp2 = 
        compare sp1.Offset sp2.Offset 

/// 28 is the size of the IMAGE_DEBUG_DIRECTORY in ntimage.h 
let sizeof_IMAGE_DEBUG_DIRECTORY = 28 

[<NoEquality; NoComparison>]
type PdbData = 
    { EntryPoint: int32 option;
      // MVID of the generated .NET module (used by MDB files to identify debug info)
      ModuleID: byte[];
      Documents: PdbDocumentData[];
      Methods: PdbMethodData[] }

//---------------------------------------------------------------------
// PDB Writer.  The function [WritePdbInfo] abstracts the 
// imperative calls to the Symbol Writer API.
//---------------------------------------------------------------------

#if SILVERLIGHT
#else
let WritePdbInfo fixupOverlappingSequencePoints showTimes f fpdb info = 
    (try System.IO.File.Delete(fpdb) with _ -> ());
    let pdbw = ref Unchecked.defaultof<PdbWriter>
    
    try
        pdbw := pdbInitialize f fpdb
    with _ -> error(Error(FSComp.SR.ilwriteErrorCreatingPdb(fpdb), rangeCmdArgs))

    match info.EntryPoint with 
    | None -> () 
    | Some x -> pdbSetUserEntryPoint !pdbw x 

    let docs = info.Documents |> Array.map (fun doc -> pdbDefineDocument !pdbw doc.File)
    let getDocument i = 
      if i < 0 || i > docs.Length then failwith "getDocument: bad doc number";
      docs.[i]
    reportTime showTimes (sprintf "PDB: Defined %d documents" info.Documents.Length);
    Array.sortInPlaceBy (fun x -> x.MethToken) info.Methods;

    reportTime showTimes (sprintf "PDB: Sorted %d methods" info.Methods.Length);

    // This next bit is a workaround.  The sequence points we get 
    // from F# (which has nothing to do with this module) are actually expression 
    // marks, i.e. the source ranges they denote are typically 
    // nested, and each point indicates where the  
    // code for an expression with a particular range begins.  
    // This is in many ways a much more convenient form to emit. 
    // However, it is not the form that debug tools accept nicely. 
    // However, sequence points are really a non-overlapping, non-nested 
    // partition of the source code of a method.  So here we shorten the 
    // length of all sequence point marks so they do not go further than 
    // the next sequence point in the source. 
    let spCounts =  info.Methods |> Array.map (fun x -> x.SequencePoints.Length)
    let allSps = Array.concat (Array.map (fun x -> x.SequencePoints) info.Methods |> Array.toList)
    let allSps = Array.mapi (fun i sp -> (i,sp)) allSps
    if fixupOverlappingSequencePoints then 
        // sort the sequence points into source order 
        Array.sortInPlaceWith (fun (_,sp1) (_,sp2) -> SequencePoint.orderBySource sp1 sp2) allSps;
        // shorten the ranges of any that overlap with following sequence points 
        // sort the sequence points back into offset order 
        for i = 0 to Array.length allSps - 2 do
            let n,sp1 = allSps.[i]
            let _,sp2 = allSps.[i+1]
            if (sp1.Document = sp2.Document) && 
               (sp1.EndLine > sp2.Line || 
                (sp1.EndLine = sp2.Line &&
                 sp1.EndColumn >= sp2.Column)) then
              let adjustToPrevLine = (sp1.Line < sp2.Line)
              allSps.[i] <-  n,{sp1 with EndLine = (if adjustToPrevLine then sp2.Line-1 else sp2.Line);
                                         EndColumn = (if adjustToPrevLine then 80 else sp2.Column); }
        Array.sortInPlaceBy fst allSps;


    
    let spOffset = ref 0
    info.Methods |> Array.iteri (fun i minfo ->

          let sps = Array.sub allSps !spOffset spCounts.[i]
          spOffset := !spOffset + spCounts.[i];
          begin match minfo.Range with 
          | None -> () 
          | Some (a,b) ->
              pdbOpenMethod !pdbw minfo.MethToken;

              pdbSetMethodRange !pdbw 
                (getDocument a.Document) a.Line a.Column
                (getDocument b.Document) b.Line b.Column;

              // Partition the sequence points by document 
              let spsets =
                let res = (Map.empty : Map<int,PdbSequencePoint list ref>)
                let add res (_,sp) = 
                  let k = sp.Document
                  match Map.tryFind k res with
                      Some xsR -> xsR := sp :: !xsR; res
                    | None     -> Map.add k (ref [sp]) res
               
                let res = Array.fold add res sps
                let res = Map.toList res  // ordering may not be stable 
                List.map (fun (_,x) -> Array.ofList !x) res

              spsets |> List.iter (fun spset -> 
                  if spset.Length > 0 then 
                    Array.sortInPlaceWith SequencePoint.orderByOffset spset;
                    let sps = 
                      spset |> Array.map (fun sp -> 
                           // Ildiag.dprintf "token 0x%08lx has an sp at offset 0x%08x\n" minfo.MethToken sp.Offset; 
                           (sp.Offset, sp.Line, sp.Column,sp.EndLine, sp.EndColumn)) 
                  // Use of alloca in implementation of pdbDefineSequencePoints can give stack overflow here 
                    if sps.Length < 5000 then 
                      pdbDefineSequencePoints !pdbw (getDocument spset.[0].Document) sps;);

              // Write the scopes 
              let rec writePdbScope top sco = 
                  if top || sco.Locals.Length <> 0 || sco.Children.Length <> 0 then 
                      pdbOpenScope !pdbw sco.StartOffset;
                      sco.Locals |> Array.iter (fun v -> pdbDefineLocalVariable !pdbw v.Name v.Signature v.Index);
                      sco.Children |> Array.iter (writePdbScope false);
                      pdbCloseScope !pdbw sco.EndOffset;
              writePdbScope true minfo.RootScope; 

              pdbCloseMethod !pdbw
          end);
    reportTime showTimes "PDB: Wrote methods";
    let res = pdbGetDebugInfo !pdbw
    pdbClose !pdbw;
    reportTime showTimes "PDB: Closed";
    res
#endif

//---------------------------------------------------------------------
// Support functions for calling 'Mono.CompilerServices.SymbolWriter'
// assembly dynamically if it is available to the compiler
//---------------------------------------------------------------------

open System.Reflection
open Microsoft.FSharp.Reflection

// Dynamic invoke operator. Implements simple overload resolution based 
// on the name and number of parameters only.
// Supports the following cases:
//   obj?Foo()        // call with no arguments
//   obj?Foo(1, "a")  // call with two arguments (extracted from tuple)
// NOTE: This doesn�t actually handle all overloads.  It just picks first entry with right 
// number of arguments.
let (?) this memb (args:'Args) : 'R = 
    // Get array of 'obj' arguments for the reflection call
    let args = 
        if typeof<'Args> = typeof<unit> then [| |]
        elif FSharpType.IsTuple typeof<'Args> then Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields(args)
        else [| box args |]
    
    // Get methods and perform overload resolution
    let methods = this.GetType().GetMethods()
    let bestMatch = methods |> Array.tryFind (fun mi -> mi.Name = memb && mi.GetParameters().Length = args.Length)
    match bestMatch with
    | Some(mi) -> unbox(mi.Invoke(this, args))        
    | None -> error(Error(FSComp.SR.ilwriteMDBMemberMissing(memb), rangeCmdArgs))

// Creating instances of needed classes from 'Mono.CompilerServices.SymbolWriter' assembly

let monoCompilerSvc = "Mono.CompilerServices.SymbolWriter, Version=2.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756"
let ctor (asmName:string) (clsName:string) (args:obj[]) = 
    let asm = Assembly.Load(asmName)
    let ty = asm.GetType(clsName)
    System.Activator.CreateInstance(ty, args)

let createSourceMethodImpl (name:string) (token:int) (namespaceID:int) = 
    ctor monoCompilerSvc "Mono.CompilerServices.SymbolWriter.SourceMethodImpl" [| box name; box token; box namespaceID |]

let createWriter (f:string) = 
    ctor monoCompilerSvc "Mono.CompilerServices.SymbolWriter.MonoSymbolWriter" [| box f |]
    
//---------------------------------------------------------------------
// MDB Writer.  Generate debug symbols using the MDB format
//---------------------------------------------------------------------

let WriteMdbInfo fmdb f info = 
    // Note, if we can�t delete it code will fail later
    (try System.IO.File.Delete(fmdb) with _ -> ());

    // Try loading the MDB symbol writer from an assembly available on Mono dynamically
    // Report an error if the assembly is not available.    
    let wr = 
        try createWriter f
        with e -> error(Error(FSComp.SR.ilwriteErrorCreatingMdb(), rangeCmdArgs))

    // NOTE: MonoSymbolWriter doesn't need information about entrypoints, so 'info.EntryPoint' is unused here.
    // Write information about Documents. Returns '(SourceFileEntry*CompileUnitEntry)[]'
    let docs =
        [| for doc in info.Documents do
             let doc = wr?DefineDocument(doc.File)
             let unit = wr?DefineCompilationUnit(doc)
             yield doc, unit |]
           
    let getDocument i = 
        if i < 0 || i >= Array.length docs then failwith "getDocument: bad doc number" else docs.[i]
    
    // Sort methods and write them to the MDB file
    Array.sortInPlaceBy (fun x -> x.MethToken) info.Methods
    for meth in info.Methods do
        // Creates an instance of 'SourceMethodImpl' which is a private class that implements 'IMethodDef' interface
        // We need this as an argument to 'OpenMethod' below. Using private class is ugly, but since we don't reference
        // the assembly, the only way to implement 'IMethodDef' interface would be dynamically using Reflection.Emit...
        let sm = createSourceMethodImpl meth.MethName meth.MethToken 0
        match meth.Range with
        | Some(mstart, _) ->
            // NOTE: 'meth.Params' is not needed, Mono debugger apparently reads this from meta-data
            let _, cue = getDocument mstart.Document
            wr?OpenMethod(cue, 0, sm) |> ignore
            
            // Write sequence points
            for sp in meth.SequencePoints do
                wr?MarkSequencePoint(sp.Offset, cue?get_SourceFile(), sp.Line, sp.Column, false)
            
            // Walk through the tree of scopes and write all variables
            let rec writeScope (scope:PdbMethodScope) = 
                wr?OpenScope(scope.StartOffset) |> ignore
                for local in scope.Locals do
                    wr?DefineLocalVariable(local.Index, local.Name)
                for child in scope.Children do 
                    writeScope(child)
                wr?CloseScope(scope.EndOffset)          
            writeScope(meth.RootScope)
            
            // Finished generating debug information for the curretn method
            wr?CloseMethod()
        | _ -> ()
    
    // Finalize - MDB requires the MVID of the generated .NET module
    let moduleGuid = new System.Guid(info.ModuleID |> Array.map byte)
    wr?WriteSymbolFile(moduleGuid)

//---------------------------------------------------------------------
// Dumps debug info into a text file for testing purposes
//---------------------------------------------------------------------
open Printf

let DumpDebugInfo (outfile:string) (info:PdbData) = 
    use sw = new StreamWriter(outfile + ".debuginfo")

    fprintfn sw "ENTRYPOINT\r\n  %b\r\n" info.EntryPoint.IsSome
    fprintfn sw "DOCUMENTS"
    for i, doc in Seq.zip [0 .. info.Documents.Length-1] info.Documents do
      fprintfn sw " [%d] %s" i doc.File
      fprintfn sw "     Type: %A" doc.DocumentType
      fprintfn sw "     Language: %A" doc.Language
      fprintfn sw "     Vendor: %A" doc.Vendor
    
    // Sort methods (because they are sorted in PDBs/MDBs too)
    fprintfn sw "\r\nMETHODS"
    Array.sortInPlaceBy (fun x -> x.MethToken) info.Methods
    for meth in info.Methods do
      fprintfn sw " %s" meth.MethName
      fprintfn sw "     Params: %A" [ for p in meth.Params -> sprintf "%d: %s" p.Index p.Name ]
      fprintfn sw "     Range: %A" (meth.Range |> Option.map (fun (f, t) -> 
                                      sprintf "[%d,%d:%d] - [%d,%d:%d]" f.Document f.Line f.Column t.Document t.Line t.Column))
      fprintfn sw "     Points:"
      
      for sp in meth.SequencePoints do
        fprintfn sw "      - Doc: %d Offset:%d [%d:%d]-[%d-%d]" sp.Document sp.Offset sp.Line sp.Column sp.EndLine sp.EndColumn
      
      // Walk through the tree of scopes and write all variables
      fprintfn sw "     Scopes:"
      let rec writeScope offs (scope:PdbMethodScope) = 
        fprintfn sw "      %s- [%d-%d]" offs scope.StartOffset scope.EndOffset
        if scope.Locals.Length > 0 then
          fprintfn sw "      %s  Locals: %A" offs [ for p in scope.Locals -> sprintf "%d: %s" p.Index p.Name ]
        for child in scope.Children do writeScope (offs + "  ") child
      writeScope "" meth.RootScope
      fprintfn sw ""
    

//---------------------------------------------------------------------
// Strong name signing
//---------------------------------------------------------------------

#if SILVERLIGHT
type ILStrongNameSigner =  
    | NeverImplemented
    static member OpenPublicKeyFile (_s:string) = NeverImplemented
    static member OpenPublicKey (_pubkey:byte[]) = NeverImplemented
    static member OpenKeyPairFile (_s:string) = NeverImplemented
    static member OpenKeyContainer (_s:string) = NeverImplemented
    member s.Close() = ()      
    member s.IsFullySigned = true
    member s.PublicKey =  [| |]
    member s.SignatureSize = 0x80 
    member s.SignFile _file = ()
#else
type ILStrongNameSigner =  
    | PublicKeySigner of Support.pubkey
    | KeyPair of Support.keyPair
    | KeyContainer of Support.keyContainerName

    static member OpenPublicKeyFile s = PublicKeySigner(Support.signerOpenPublicKeyFile s)
      
    static member OpenPublicKey pubkey = PublicKeySigner(pubkey)

    static member OpenKeyPairFile s = KeyPair(Support.signerOpenKeyPairFile s)

    static member OpenKeyContainer s = KeyContainer(s)

    member s.Close() = 
        match s with 
        | PublicKeySigner _
        | KeyPair _ -> ()
        | KeyContainer containerName -> Support.signerCloseKeyContainer(containerName)
      
    member s.IsFullySigned =
        match s with 
        | PublicKeySigner _ -> false
        | KeyPair _ | KeyContainer _ -> true

    member s.PublicKey = 
        match s with 
        | PublicKeySigner p -> p
        | KeyPair kp -> Support.signerGetPublicKeyForKeyPair kp
        | KeyContainer kn -> Support.signerGetPublicKeyForKeyContainer kn

    member s.SignatureSize = 
        try Support.signerSignatureSize(s.PublicKey)
        with e -> 
          failwith ("A call to StrongNameSignatureSize failed ("+e.Message+")");
          0x80 

    member s.SignFile file = 
        match s with 
        | PublicKeySigner _ -> ()
        | KeyPair kp -> Support.signerSignFileWithKeyPair file kp
        | KeyContainer kn -> Support.signerSignFileWithKeyContainer file kn

#endif

//---------------------------------------------------------------------
// TYPES FOR TABLES
//---------------------------------------------------------------------

type RowElement = 
  | UShort of uint16
  | ULong of int32
  | Data of int * bool // Index into cenv.data or cenv.resources.  Will be adjusted later in writing once we fix an overall location for the data section.  flag indicates if offset is relative to cenv.resources. 
  | Guid of int // pos. in guid array 
  | Blob of int // pos. in blob array 
  | String of int // pos. in string array 
  | SimpleIndex of TableName * int // pos. in some table 
  | TypeDefOrRefOrSpec of TypeDefOrRefTag * int
  | TypeOrMethodDef of TypeOrMethodDefTag * int
  | HasConstant of HasConstantTag * int
  | HasCustomAttribute of HasCustomAttributeTag * int
  | HasFieldMarshal of HasFieldMarshalTag * int
  | HasDeclSecurity of HasDeclSecurityTag * int
  | MemberRefParent of MemberRefParentTag * int
  | HasSemantics of HasSemanticsTag * int
  | MethodDefOrRef of MethodDefOrRefTag * int
  | MemberForwarded of  MemberForwardedTag * int
  | Implementation of ImplementationTag * int
  | CustomAttributeType of CustomAttributeTypeTag * int
  | ResolutionScope of ResolutionScopeTag * int

type BlobIndex = int
type StringIndex = int

let BlobIndex (x:BlobIndex) : int = x
let StringIndex (x:StringIndex) : int = x

/// Abstract, general type of metadata table rows
type IGenericRow = 
    abstract GetGenericRow : unit -> array<RowElement>

/// Shared rows are used for the ILTypeRef, ILMethodRef, ILMethodSpec, etc. tables
/// where entries can be shared and need to be made unique through hash-cons'ing
type ISharedRow = 
    inherit IGenericRow
    
/// This is the representation of shared rows is used for most shared row types.
/// Rows ILAssemblyRef and ILMethodRef are very common and are given their own
/// representations.
type SimpleSharedRow(elems: array<RowElement>) =
    let hashCode = hash elems // precompute to give more efficient hashing and equality comparisons
    interface ISharedRow with 
        member x.GetGenericRow() = elems
    member x.GenericRow = elems
    override x.GetHashCode() = hashCode
    override x.Equals(obj:obj) = 
        match obj with 
        | :? SimpleSharedRow as y -> elems = y.GenericRow
        | _ -> false

/// Unshared rows are used for definitional tables where elements do not need to be made unique
/// e.g. ILMethodDef and ILTypeDef. Most tables are like this. We don't precompute a 
/// hash code for these rows, and indeed the GetHashCode and Equals should not be needed.
type UnsharedRow(elems: array<RowElement>) =
    interface IGenericRow with 
        member x.GetGenericRow() = elems
    member x.GenericRow = elems
    override x.GetHashCode() = hash elems
    override x.Equals(obj:obj) = 
        match obj with 
        | :? UnsharedRow as y -> elems = y.GenericRow
        | _ -> false
             
let inline combineHash x2 acc = 37 * acc + x2 // (acc <<< 6 + acc >>> 2 + x2 + 0x9e3779b9)

/// Special representation for ILAssemblyRef rows
type AssemblyRefRow(s1,s2,s3,s4,l1,b1,nameIdx,str2,b2) = 
    let hashCode = hash nameIdx
    let genericRow = [| UShort s1; UShort s2; UShort s3; UShort s4; ULong l1; Blob b1; String nameIdx; String str2; Blob b2 |]
    interface ISharedRow with 
        member x.GetGenericRow() =  genericRow            
    member x.GenericRow = genericRow
    override x.GetHashCode() = hashCode
    override x.Equals(obj:obj) = 
        match obj with 
        | :? AssemblyRefRow as y -> genericRow = y.GenericRow
        | _ -> false

/// Special representation of a very common kind of row
type MemberRefRow(mrp:RowElement,nmIdx:StringIndex,blobIdx:BlobIndex) = 
    let hash =  hash mrp |> combineHash (hash nmIdx) |> combineHash (hash blobIdx)
    let genericRow = [| mrp; String nmIdx; Blob blobIdx |]
    interface ISharedRow with 
        member x.GetGenericRow() = genericRow
    member x.GenericRow = genericRow
    override x.GetHashCode() = hash
    override x.Equals(obj:obj) = 
        match obj with 
        | :? MemberRefRow as y -> genericRow = y.GenericRow
        | _ -> false

//=====================================================================
//=====================================================================
// IL --> TABLES+CODE
//=====================================================================
//=====================================================================

// This environment keeps track of how many generic parameters are in scope. 
// This lets us translate AbsIL type variable number to IL type variable numbering 
type ILTypeWriterEnv = { EnclosingTyparCount: int }
let envForTypeDef (td:ILTypeDef)              = { EnclosingTyparCount=td.GenericParams.Length }
let envForMethodRef env (typ:ILType)         = { EnclosingTyparCount=(match typ with ILType.Array _ -> env.EnclosingTyparCount | _ -> typ.GenericArgs.Length) }
let envForNonGenericMethodRef _mref          = { EnclosingTyparCount=System.Int32.MaxValue }
let envForFieldSpec (fspec:ILFieldSpec)      = { EnclosingTyparCount=fspec.EnclosingType.GenericArgs.Length }
let envForOverrideSpec (ospec:ILOverridesSpec) = { EnclosingTyparCount=ospec.EnclosingType.GenericArgs.Length }

//---------------------------------------------------------------------
// TABLES
//---------------------------------------------------------------------

[<NoEquality; NoComparison>]
type MetadataTable<'T> = 
    { name: string;
      dict: Dictionary<'T, int>; // given a row, find its entry number
#if DEBUG
      mutable lookups: int;
#endif
      mutable rows: ResizeArray<'T> ; }
    member x.Count = x.rows.Count

    static member New(nm,hashEq) = 
        { name=nm;
#if DEBUG
          lookups=0;
#endif
          dict = new Dictionary<_,_>(100, hashEq);
          rows= new ResizeArray<_>(); }

    member tbl.Entries = 
#if DEBUG
        if showEntryLookups then dprintf "--> table %s had %d entries and %d lookups\n" tbl.name tbl.Count tbl.lookups;
#endif
        tbl.rows |> ResizeArray.toList

    member tbl.AddSharedEntry x =
        let n = tbl.rows.Count + 1
        tbl.dict.[x] <- n;
        tbl.rows.Add(x);
        n

    member tbl.AddUnsharedEntry x =
        let n = tbl.rows.Count + 1
        tbl.rows.Add(x);
        n

    member tbl.FindOrAddSharedEntry x =
#if DEBUG
        tbl.lookups <- tbl.lookups + 1; 
#endif
        let mutable res = Unchecked.defaultof<_>
        let ok = tbl.dict.TryGetValue(x,&res)
        if ok then res
        else tbl.AddSharedEntry x


    /// This is only used in one special place - see furthre below. 
    member tbl.SetRowsOfTable t = 
        tbl.rows <- ResizeArray.ofList t;  
        let h = tbl.dict
        h.Clear();
        t |> List.iteri (fun i x -> h.[x] <- (i+1))

    member tbl.AddUniqueEntry nm geterr x =
        if tbl.dict.ContainsKey x then failwith ("duplicate entry '"+geterr x+"' in "+nm+" table")
        else tbl.AddSharedEntry x

    member tbl.GetTableEntry x = tbl.dict.[x] 

//---------------------------------------------------------------------
// Keys into some of the tables
//---------------------------------------------------------------------

/// We use this key type to help find ILMethodDefs for MethodRefs 
type MethodDefKey(tidx:int,garity:int,nm:string,rty:ILType,argtys:ILType list,isStatic:bool) =
    // Precompute the hash. The hash doesn't include the return type or 
    // argument types (only argument type count). This is very important, since
    // hashing these is way too expensive
    let hashCode = 
       hash tidx 
       |> combineHash (hash garity) 
       |> combineHash (hash nm) 
       |> combineHash (hash argtys.Length)
       |> combineHash (hash isStatic)
    member key.TypeIdx = tidx
    member key.GenericArity = garity
    member key.Name = nm
    member key.ReturnType = rty
    member key.ArgTypes = argtys
    member key.IsStatic = isStatic
    override x.GetHashCode() = hashCode
    override x.Equals(obj:obj) = 
        match obj with 
        | :? MethodDefKey as y -> 
            tidx = y.TypeIdx && 
            garity = y.GenericArity && 
            nm = y.Name && 
            // note: these next two use structural equality on AbstractIL ILType values
            rty = y.ReturnType && 
            argtys = y.ArgTypes &&
            isStatic = y.IsStatic
        | _ -> false

/// We use this key type to help find ILFieldDefs for FieldRefs
type FieldDefKey(tidx:int,nm:string,ty:ILType) = 
    // precompute the hash. hash doesn't include the type 
    let hashCode = hash tidx |> combineHash (hash nm) 
    member key.TypeIdx = tidx
    member key.Name = nm
    member key.Type = ty
    override x.GetHashCode() = hashCode
    override x.Equals(obj:obj) = 
        match obj with 
        | :? FieldDefKey as y -> 
            tidx = y.TypeIdx && 
            nm = y.Name && 
            ty = y.Type 
        | _ -> false

type PropertyTableKey = PropKey of int (* type. def. idx. *) * string * ILType * ILType list
type EventTableKey = EventKey of int (* type. def. idx. *) * string
type TypeDefTableKey = TdKey of string list (* enclosing *) * string (* type name *)

//---------------------------------------------------------------------
// The Writer Context
//---------------------------------------------------------------------

[<NoEquality; NoComparison>]
type cenv = 
    { mscorlib: ILScopeRef;
      ilg: ILGlobals;
      emitTailcalls: bool;
      showTimes: bool;
      desiredMetadataVersion: ILVersionInfo;
      requiredDataFixups: (int32 * (int * bool)) list ref;
      /// References to strings in codestreams: offset of code and a (fixup-location , string token) list) 
      mutable requiredStringFixups: (int32 * (int * int) list) list; 
      codeChunks: ByteBuffer; 
      mutable nextCodeAddr: int32;
      
      // Collected debug information
      mutable moduleGuid: byte[]
      generatePdb: bool;
      pdbinfo: ResizeArray<PdbMethodData>;
      documents: MetadataTable<PdbDocumentData>;
      /// Raw data, to go into the data section 
      data: ByteBuffer; 
      /// Raw resource data, to go into the data section 
      resources: ByteBuffer; 
      mutable entrypoint: (bool * int) option; 

      /// Caches
      trefCache: Dictionary<ILTypeRef,int>;

      /// The following are all used to generate unique items in the output 
      tables: array<MetadataTable<IGenericRow>>;
      AssemblyRefs: MetadataTable<AssemblyRefRow>;
      fieldDefs: MetadataTable<FieldDefKey>;
      methodDefIdxsByKey:  MetadataTable<MethodDefKey>;
      methodDefIdxs:  Dictionary<ILMethodDef,int>;
      propertyDefs: MetadataTable<PropertyTableKey>;
      eventDefs: MetadataTable<EventTableKey>;
      typeDefs: MetadataTable<TypeDefTableKey>; 
      guids: MetadataTable<byte[]>; 
      blobs: MetadataTable<byte[]>; 
      strings: MetadataTable<string>; 
      userStrings: MetadataTable<string>;
    }
    member cenv.GetTable (tab:TableName) = cenv.tables.[tab.Index]


    member cenv.AddCode ((reqdStringFixupsOffset,requiredStringFixups),code) = 
        if align 4 cenv.nextCodeAddr <> cenv.nextCodeAddr then dprintn "warning: code not 4-byte aligned";
        cenv.requiredStringFixups <- (cenv.nextCodeAddr + reqdStringFixupsOffset, requiredStringFixups) :: cenv.requiredStringFixups;
        cenv.codeChunks.EmitBytes code;
        cenv.nextCodeAddr <- cenv.nextCodeAddr + code.Length

    member cenv.GetCode() = cenv.codeChunks.Close()


let FindOrAddRow (cenv:cenv) tbl (x:IGenericRow) = cenv.GetTable(tbl).FindOrAddSharedEntry x

// Shared rows must be hash-cons'd to be made unique (no duplicates according to contents)
let AddSharedRow (cenv:cenv) tbl (x:ISharedRow) = cenv.GetTable(tbl).AddSharedEntry (x :> IGenericRow)

// Unshared rows correspond to definition elements (e.g. a ILTypeDef or a ILMethodDef)
let AddUnsharedRow (cenv:cenv) tbl (x:UnsharedRow) = cenv.GetTable(tbl).AddUnsharedEntry  (x :> IGenericRow)

let metadataSchemaVersionSupportedByCLRVersion v = 
    // Whidbey Beta 1 version numbers are between 2.0.40520.0 and 2.0.40607.0 
    // Later Whidbey versions are post 2.0.40607.0.. However we assume 
    // internal builds such as 2.0.x86chk are Whidbey Beta 2 or later 
    if compareILVersions v (parseILVersion ("2.0.40520.0")) >= 0 &&
       compareILVersions  v (parseILVersion ("2.0.40608.0")) < 0 then 1,1
    elif compareILVersions v (parseILVersion ("2.0.0.0")) >= 0 then 2,0
    else 1,0 

let headerVersionSupportedByCLRVersion v = 
   // The COM20HEADER version number 
   // Whidbey version numbers are 2.5 
   // Earlier are 2.0 
   // From an email from jeffschw: "Be built with a compiler that marks the COM20HEADER with Major >=2 and Minor >= 5.  The V2.0 compilers produce images with 2.5, V1.x produces images with 2.0." 
    if compareILVersions v (parseILVersion ("2.0.0.0")) >= 0 then 2,5
    else 2,0 

let peOptionalHeaderByteByCLRVersion v = 
   //  A flag in the PE file optional header seems to depend on CLI version 
   // Whidbey version numbers are 8 
   // Earlier are 6 
   // Tools are meant to ignore this, but the VS Profiler wants it to have the right value 
    if compareILVersions v (parseILVersion ("2.0.0.0")) >= 0 then 8
    else 6

// returned by writeBinaryAndReportMappings 
[<NoEquality; NoComparison>]
type ILTokenMappings =  
    { TypeDefTokenMap: ILTypeDef list * ILTypeDef -> int32;
      FieldDefTokenMap: ILTypeDef list * ILTypeDef -> ILFieldDef -> int32;
      MethodDefTokenMap: ILTypeDef list * ILTypeDef -> ILMethodDef -> int32;
      PropertyTokenMap: ILTypeDef list * ILTypeDef -> ILPropertyDef -> int32;
      EventTokenMap: ILTypeDef list * ILTypeDef -> ILEventDef -> int32 }

let recordRequiredDataFixup requiredDataFixups (buf: ByteBuffer) pos lab =
    requiredDataFixups :=  (pos,lab) :: !requiredDataFixups;
    // Write a special value in that we check later when applying the fixup 
    buf.EmitInt32 0xdeaddddd

//---------------------------------------------------------------------
// The UserString, BlobHeap, GuidHeap tables
//---------------------------------------------------------------------

let GetUserStringHeapIdx cenv s = 
    cenv.userStrings.FindOrAddSharedEntry s

let GetBytesAsBlobIdx cenv (bytes:byte[]) = 
    if bytes.Length = 0 then 0 
    else cenv.blobs.FindOrAddSharedEntry bytes

let GetStringHeapIdx cenv s = 
    if s = "" then 0 
    else cenv.strings.FindOrAddSharedEntry s

let GetGuidIdx cenv info = cenv.guids.FindOrAddSharedEntry info

let GetStringHeapIdxOption cenv sopt =
    match sopt with 
    | Some ns -> GetStringHeapIdx cenv ns
    | None -> 0

let GetTypeNameAsElemPair cenv n =
    let (n1,n2) = splitTypeNameRight n
    String (GetStringHeapIdxOption cenv n1),
    String (GetStringHeapIdx cenv n2)

//=====================================================================
// Pass 1 - allocate indexes for types 
//=====================================================================

let rec GenTypeDefPass1 enc cenv (td:ILTypeDef) = 
  ignore (cenv.typeDefs.AddUniqueEntry "type index" (fun (TdKey (_,n)) -> n) (TdKey (enc,td.Name)));
  GenTypeDefsPass1 (enc@[td.Name]) cenv td.NestedTypes.AsList

and GenTypeDefsPass1 enc cenv tds = List.iter (GenTypeDefPass1 enc cenv) tds


//=====================================================================
// Pass 2 - allocate indexes for methods and fields and write rows for types 
//=====================================================================

let rec GetIdxForTypeDef cenv key  = 
    try cenv.typeDefs.GetTableEntry key
    with 
      :? KeyNotFoundException -> 
        let (TdKey (enc,n) ) = key
        failwith ("One of your modules expects the type '"+String.concat "." (enc@[n])+"' to be defined within the module being emitted.  You may be missing an input file")
    
// -------------------------------------------------------------------- 
// Assembly and module references
// -------------------------------------------------------------------- 

let rec GetAssemblyRefAsRow cenv (aref:ILAssemblyRef) =
    AssemblyRefRow 
        ((match aref.Version with None -> 0us | Some (x,_,_,_) -> x),
         (match aref.Version with None -> 0us | Some (_,y,_,_) -> y),
         (match aref.Version with None -> 0us | Some (_,_,z,_) -> z),
         (match aref.Version with None -> 0us | Some (_,_,_,w) -> w),
         ((match aref.PublicKey with Some (PublicKey _) -> 0x0001 | _ -> 0x0000)
          ||| (if aref.Retargetable then 0x0100 else 0x0000)),
         BlobIndex (match aref.PublicKey with 
                    | None ->  0 
                    | Some (PublicKey b | PublicKeyToken b) -> GetBytesAsBlobIdx cenv b),
         StringIndex (GetStringHeapIdx cenv aref.Name),
         StringIndex (match aref.Locale with None -> 0 | Some s -> GetStringHeapIdx cenv s),
         BlobIndex (match aref.Hash with None -> 0 | Some s -> GetBytesAsBlobIdx cenv s))
  
and GetAssemblyRefAsIdx cenv aref = 
    FindOrAddRow cenv TableNames.AssemblyRef (GetAssemblyRefAsRow cenv aref)

and GetModuleRefAsRow cenv (mref:ILModuleRef) =
    SimpleSharedRow 
        [| String (GetStringHeapIdx cenv mref.Name) |]

and GetModuleRefAsFileRow cenv (mref:ILModuleRef) =
    SimpleSharedRow 
        [|  ULong (if mref.HasMetadata then 0x0000 else 0x0001);
            String (GetStringHeapIdx cenv mref.Name);
            (match mref.Hash with None -> Blob 0 | Some s -> Blob (GetBytesAsBlobIdx cenv s)); |]

and GetModuleRefAsIdx cenv mref = 
    FindOrAddRow cenv TableNames.ModuleRef (GetModuleRefAsRow cenv mref)

and GetModuleRefAsFileIdx cenv mref = 
    FindOrAddRow cenv TableNames.File (GetModuleRefAsFileRow cenv mref)

// -------------------------------------------------------------------- 
// Does a ILScopeRef point to this module?
// -------------------------------------------------------------------- 

let isScopeRefLocal scoref = (scoref = ILScopeRef.Local) 
let isTypeRefLocal (tref:ILTypeRef) = isScopeRefLocal tref.Scope
let isTypeLocal (typ:ILType) = typ.IsNominal && typ.GenericArgs.IsEmpty && isTypeRefLocal typ.TypeRef

// -------------------------------------------------------------------- 
// Scopes to Implementation elements.
// -------------------------------------------------------------------- 

let GetScopeRefAsImplementationElem cenv scoref = 
    match scoref with 
    | ILScopeRef.Local ->  (i_AssemblyRef, 0)
    | ILScopeRef.Assembly aref -> (i_AssemblyRef, GetAssemblyRefAsIdx cenv aref)
    | ILScopeRef.Module mref -> (i_File, GetModuleRefAsFileIdx cenv mref)
 
// -------------------------------------------------------------------- 
// Type references, types etc.
// -------------------------------------------------------------------- 

let rec GetTypeRefAsTypeRefRow cenv (tref:ILTypeRef) = 
    let nselem,nelem = GetTypeNameAsElemPair cenv tref.Name
    let rs1,rs2 = GetResolutionScopeAsElem cenv (tref.Scope,tref.Enclosing)
    SimpleSharedRow [| ResolutionScope (rs1,rs2); nelem; nselem |]

and GetTypeRefAsTypeRefIdx cenv tref = 
    let mutable res = 0
    if cenv.trefCache.TryGetValue(tref,&res) then res else 
    let res = FindOrAddRow cenv TableNames.TypeRef (GetTypeRefAsTypeRefRow cenv tref)
    cenv.trefCache.[tref] <- res;
    res

and GetTypeDescAsTypeRefIdx cenv (scoref,enc,n) =  
    GetTypeRefAsTypeRefIdx cenv (mkILNestedTyRef (scoref,enc,n))

and GetResolutionScopeAsElem cenv (scoref,enc) = 
    if isNil enc then 
        match scoref with 
        | ILScopeRef.Local -> (rs_Module, 1) 
        | ILScopeRef.Assembly aref -> (rs_AssemblyRef, GetAssemblyRefAsIdx cenv aref)
        | ILScopeRef.Module mref -> (rs_ModuleRef, GetModuleRefAsIdx cenv mref)
    else
        let enc2,n2 = List.frontAndBack enc
        (rs_TypeRef, GetTypeDescAsTypeRefIdx cenv (scoref,enc2,n2))
 

let emitTypeInfoAsTypeDefOrRefEncoded cenv (bb: ByteBuffer) (scoref,enc,nm) = 
    if isScopeRefLocal scoref then 
        let idx = GetIdxForTypeDef cenv (TdKey(enc,nm))
        bb.EmitZ32 (idx <<< 2) // ECMA 22.2.8 TypeDefOrRefEncoded - ILTypeDef 
    else 
        let idx = GetTypeDescAsTypeRefIdx cenv (scoref,enc,nm)
        bb.EmitZ32 ((idx <<< 2) ||| 0x01) // ECMA 22.2.8 TypeDefOrRefEncoded - ILTypeRef 

let getTypeDefOrRefAsUncodedToken (tag,idx) =
    let tab = 
        if tag = tdor_TypeDef then TableNames.TypeDef 
        elif tag = tdor_TypeRef then TableNames.TypeRef  
        elif tag = tdor_TypeSpec then TableNames.TypeSpec
        else failwith "getTypeDefOrRefAsUncodedToken"
    getUncodedToken tab idx

let EmitArrayShape (bb: ByteBuffer) (ILArrayShape shape) = 
    let sized = List.filter (function (_,Some _) -> true | _ -> false) shape
    let lobounded = List.filter (function (Some _,_) -> true | _ -> false) shape
    bb.EmitZ32 shape.Length;
    bb.EmitZ32 sized.Length;
    sized |> List.iter (function (_,Some sz) -> bb.EmitZ32 sz | _ -> failwith "?");
    bb.EmitZ32 lobounded.Length;
    lobounded |> List.iter (function (Some low,_) -> bb.EmitZ32 low | _ -> failwith "?") 
        
let hasthisToByte hasthis =
     match hasthis with 
     | ILThisConvention.Instance -> e_IMAGE_CEE_CS_CALLCONV_INSTANCE
     | ILThisConvention.InstanceExplicit -> e_IMAGE_CEE_CS_CALLCONV_INSTANCE_EXPLICIT
     | ILThisConvention.Static -> 0x00uy

let callconvToByte ntypars (Callconv (hasthis,bcc)) = 
    hasthisToByte hasthis |||
    (if ntypars > 0 then e_IMAGE_CEE_CS_CALLCONV_GENERIC else 0x00uy) |||
    (match bcc with 
    | ILArgConvention.FastCall -> e_IMAGE_CEE_CS_CALLCONV_FASTCALL
    | ILArgConvention.StdCall -> e_IMAGE_CEE_CS_CALLCONV_STDCALL
    | ILArgConvention.ThisCall -> e_IMAGE_CEE_CS_CALLCONV_THISCALL
    | ILArgConvention.CDecl -> e_IMAGE_CEE_CS_CALLCONV_CDECL
    | ILArgConvention.Default -> 0x00uy
    | ILArgConvention.VarArg -> e_IMAGE_CEE_CS_CALLCONV_VARARG)
  

let rec EmitTypeSpec cenv env (bb: ByteBuffer) (et,tspec:ILTypeSpec) = 
    if isNil tspec.GenericArgs then 
        bb.EmitByte et;
        emitTypeInfoAsTypeDefOrRefEncoded cenv bb (tspec.Scope,tspec.Enclosing,tspec.Name)
    else  
        bb.EmitByte et_WITH;
        bb.EmitByte et;
        emitTypeInfoAsTypeDefOrRefEncoded cenv bb (tspec.Scope,tspec.Enclosing,tspec.Name);
        bb.EmitZ32 tspec.GenericArgs.Length;
        EmitTypes cenv env bb tspec.GenericArgs

and GetTypeAsTypeDefOrRef cenv env (ty:ILType) = 
    if isTypeLocal ty then 
        let tref = ty.TypeRef
        (tdor_TypeDef, GetIdxForTypeDef cenv (TdKey(tref.Enclosing,tref.Name)))
    elif ty.IsNominal && ty.GenericArgs.IsEmpty then
        (tdor_TypeRef, GetTypeRefAsTypeRefIdx cenv ty.TypeRef)
    else 
        (tdor_TypeSpec, GetTypeAsTypeSpecIdx cenv env ty)

and GetTypeAsBytes cenv env ty = emitBytesViaBuffer (fun bb -> EmitType cenv env bb ty)

and GetTypeAsBlobIdx cenv env (ty:ILType) = 
    GetBytesAsBlobIdx cenv (GetTypeAsBytes cenv env ty)

and GetTypeAsTypeSpecRow cenv env (ty:ILType) = 
    SimpleSharedRow [| Blob (GetTypeAsBlobIdx cenv env ty) |]

and GetTypeAsTypeSpecIdx cenv env ty = 
    FindOrAddRow cenv TableNames.TypeSpec (GetTypeAsTypeSpecRow cenv env ty)

and EmitType cenv env bb ty =
    let ilg = cenv.ilg
    match ty with 
    | ILType.Value tspec when tspec.Name = "System.String" ->   bb.EmitByte et_STRING 
    | ILType.Value tspec when tspec.Name = "System.Object" ->   bb.EmitByte et_OBJECT 
    | typ when isILSByteTy ilg typ ->   bb.EmitByte et_I1 
    | typ when isILInt16Ty ilg typ ->   bb.EmitByte et_I2 
    | typ when isILInt32Ty ilg typ ->    bb.EmitByte et_I4 
    | typ when isILInt64Ty ilg typ ->     bb.EmitByte et_I8 
    | typ when isILByteTy ilg typ ->     bb.EmitByte et_U1 
    | typ when isILUInt16Ty ilg typ ->     bb.EmitByte et_U2 
    | typ when isILUInt32Ty ilg typ ->     bb.EmitByte et_U4 
    | typ when isILUInt64Ty ilg typ ->     bb.EmitByte et_U8 
    | typ when isILDoubleTy ilg typ ->     bb.EmitByte et_R8 
    | typ when isILSingleTy ilg typ ->     bb.EmitByte et_R4 
    | typ when isILBoolTy ilg typ ->     bb.EmitByte et_BOOLEAN 
    | typ when isILCharTy ilg typ ->     bb.EmitByte et_CHAR 
    | typ when isILStringTy ilg typ ->     bb.EmitByte et_STRING 
    | typ when isILObjectTy ilg typ ->     bb.EmitByte et_OBJECT 
    | typ when isILIntPtrTy ilg typ ->     bb.EmitByte et_I 
    | typ when isILUIntPtrTy ilg typ ->     bb.EmitByte et_U 
    | typ when isILTypedReferenceTy ilg typ ->     bb.EmitByte et_TYPEDBYREF 

    | ILType.Boxed tspec ->  EmitTypeSpec cenv env bb (et_CLASS,tspec)
    | ILType.Value tspec ->  EmitTypeSpec cenv env bb (et_VALUETYPE,tspec)
    | ILType.Array (shape,ty) ->  
        if shape = ILArrayShape.SingleDimensional then (bb.EmitByte et_SZARRAY ; EmitType cenv env bb ty)
        else (bb.EmitByte et_ARRAY; EmitType cenv env bb ty; EmitArrayShape bb shape)
    | ILType.TypeVar tv ->  
        let cgparams = env.EnclosingTyparCount
        if int32 tv <  cgparams then 
            bb.EmitByte et_VAR;
            bb.EmitZ32 (int32 tv)
        else
            bb.EmitByte et_MVAR;
            bb.EmitZ32 (int32 tv -  cgparams)

    | ILType.Byref typ -> 
        bb.EmitByte et_BYREF;
        EmitType cenv env bb typ
    | ILType.Ptr typ ->  
        bb.EmitByte et_PTR;
        EmitType cenv env bb typ
    | ILType.Void ->   
        bb.EmitByte et_VOID 
    | ILType.FunctionPointer x ->
        bb.EmitByte et_FNPTR;
        EmitCallsig cenv env bb (x.CallingConv,x.ArgTypes,x.ReturnType,None,0)
    | ILType.Modified (req,tref,ty) ->
        bb.EmitByte (if req then et_CMOD_REQD else et_CMOD_OPT);
        emitTypeInfoAsTypeDefOrRefEncoded cenv bb (tref.Scope, tref.Enclosing,tref.Name);
        EmitType cenv env bb ty
     | _ -> failwith "EmitType"

and EmitCallsig cenv env bb (callconv,args:ILType list,ret,varargs:ILVarArgs,genarity) = 
    bb.EmitByte (callconvToByte genarity callconv);
    if genarity > 0 then bb.EmitZ32 (genarity);
    bb.EmitZ32 ((args.Length + (match varargs with None -> 0 | Some l -> l.Length)));
    EmitType cenv env bb ret;
    args |> List.iter (EmitType cenv env bb);
    match varargs with 
     | None | Some [] -> () // no extra arg = no sentinel 
     | Some tys -> 
         bb.EmitByte et_SENTINEL;
         List.iter (EmitType cenv env bb) tys

and GetCallsigAsBytes cenv env x = emitBytesViaBuffer (fun bb -> EmitCallsig cenv env bb x)

and EmitTypes cenv env bb (inst: ILType list) = 
    inst |> List.iter (EmitType cenv env bb) 

let GetTypeAsMemberRefParent cenv env ty =
    match GetTypeAsTypeDefOrRef cenv env ty with 
    | (tag,_) when tag = tdor_TypeDef -> dprintn "GetTypeAsMemberRefParent: mspec should have been encoded as mdtMethodDef?"; MemberRefParent (mrp_TypeRef, 1)
    | (tag,tok) when tag = tdor_TypeRef -> MemberRefParent (mrp_TypeRef, tok)
    | (tag,tok) when tag = tdor_TypeSpec -> MemberRefParent (mrp_TypeSpec, tok)
    | _ -> failwith "GetTypeAsMemberRefParent"


// -------------------------------------------------------------------- 
// Native types
// -------------------------------------------------------------------- 

let rec GetVariantTypeAsInt32 ty = 
    if List.memAssoc ty (Lazy.force ILVariantTypeMap) then 
        (List.assoc ty (Lazy.force ILVariantTypeMap ))
    else 
        match ty with 
        | ILNativeVariant.Array  vt -> vt_ARRAY  ||| GetVariantTypeAsInt32  vt
        | ILNativeVariant.Vector vt -> vt_VECTOR ||| GetVariantTypeAsInt32  vt
        | ILNativeVariant.Byref  vt -> vt_BYREF  ||| GetVariantTypeAsInt32  vt
        | _ -> failwith "Unexpected variant type"

// based on information in ECMA and asmparse.y in the CLR codebase 
let rec GetNativeTypeAsBlobIdx cenv (ty:ILNativeType) = 
    GetBytesAsBlobIdx cenv (GetNativeTypeAsBytes ty)

and GetNativeTypeAsBytes ty = emitBytesViaBuffer (fun bb -> EmitNativeType bb ty)

and EmitNativeType bb ty = 
    if List.memAssoc ty (Lazy.force ILNativeTypeRevMap) then 
        bb.EmitByte (List.assoc ty (Lazy.force ILNativeTypeRevMap))
    else 
      match ty with 
      | ILNativeType.Empty -> ()
      | ILNativeType.Custom (guid,nativeTypeName,custMarshallerName,cookieString) ->
          let u1 = System.Text.Encoding.UTF8.GetBytes nativeTypeName
          let u2 = System.Text.Encoding.UTF8.GetBytes custMarshallerName
          let u3 = cookieString
          bb.EmitByte nt_CUSTOMMARSHALER; 
          bb.EmitZ32 guid.Length;
          bb.EmitBytes guid;
          bb.EmitZ32 u1.Length; bb.EmitBytes u1;
          bb.EmitZ32 u2.Length; bb.EmitBytes u2;
          bb.EmitZ32 u3.Length; bb.EmitBytes u3
      | ILNativeType.FixedSysString i -> 
          bb.EmitByte nt_FIXEDSYSSTRING; 
          bb.EmitZ32 i

      | ILNativeType.FixedArray i -> 
          bb.EmitByte nt_FIXEDARRAY;
          bb.EmitZ32 i
      | (* COM interop *) ILNativeType.SafeArray (vt,name) -> 
          bb.EmitByte nt_SAFEARRAY;
          bb.EmitZ32 (GetVariantTypeAsInt32 vt);
          match name with 
          | None -> () 
          | Some n -> 
               let u1 = Bytes.stringAsUtf8NullTerminated n
               bb.EmitZ32 (Array.length u1) ; bb.EmitBytes  u1
      | ILNativeType.Array (nt,sizeinfo) ->  
          bb.EmitByte nt_ARRAY; 
          match nt with 
          | None -> bb.EmitZ32 (int nt_MAX)
          | Some ntt ->
             (if ntt = ILNativeType.Empty then 
               bb.EmitZ32 (int nt_MAX)
              else 
                EmitNativeType bb ntt); 
          match sizeinfo with 
          | None -> ()  // chunk out with zeroes because some tools (e.g. asmmeta) read these poorly and expect further elements. 
          | Some (pnum,additive) ->
              // ParamNum 
              bb.EmitZ32 pnum;
            (* ElemMul *) (* z_u32 0x1l *) 
              match additive with 
              |  None -> ()
              |  Some n ->  (* NumElem *) bb.EmitZ32 n
      | _ -> failwith "Unexpected native type"

// -------------------------------------------------------------------- 
// Native types
// -------------------------------------------------------------------- 

let rec GetFieldInitAsBlobIdx cenv (x:ILFieldInit) = 
    GetBytesAsBlobIdx cenv (emitBytesViaBuffer (fun bb -> GetFieldInit bb x))

and GetFieldInit (bb: ByteBuffer) x = 
    match x with 
    | ILFieldInit.String b -> bb.EmitBytes (System.Text.Encoding.Unicode.GetBytes b)
    | ILFieldInit.Bool b ->  bb.EmitByte (if b then 0x01uy else 0x00uy)
    | ILFieldInit.Char x -> bb.EmitUInt16 x
    | ILFieldInit.Int8 x -> bb.EmitByte (byte  x)
    | ILFieldInit.Int16 x -> bb.EmitUInt16 (uint16 x)
    | ILFieldInit.Int32 x -> bb.EmitInt32 x
    | ILFieldInit.Int64 x -> bb.EmitInt64 x
    | ILFieldInit.UInt8 x -> bb.EmitByte x
    | ILFieldInit.UInt16 x -> bb.EmitUInt16 x
    | ILFieldInit.UInt32 x -> bb.EmitInt32 (int32 x)
    | ILFieldInit.UInt64 x -> bb.EmitInt64 (int64 x)
    | ILFieldInit.Single x -> bb.EmitInt32 (bitsOfSingle x)
    | ILFieldInit.Double x -> bb.EmitInt64 (bitsOfDouble x)
    | ILFieldInit.Null  -> bb.EmitInt32 0

and GetFieldInitFlags i = 
    UShort 
      (uint16
        (match i with 
         | ILFieldInit.String _ -> et_STRING
         | ILFieldInit.Bool _ -> et_BOOLEAN
         | ILFieldInit.Char _ -> et_CHAR
         | ILFieldInit.Int8 _ -> et_I1
         | ILFieldInit.Int16 _ -> et_I2
         | ILFieldInit.Int32 _ -> et_I4
         | ILFieldInit.Int64 _ -> et_I8
         | ILFieldInit.UInt8 _ -> et_U1
         | ILFieldInit.UInt16 _ -> et_U2
         | ILFieldInit.UInt32 _ -> et_U4
         | ILFieldInit.UInt64 _ -> et_U8
         | ILFieldInit.Single _ -> et_R4
         | ILFieldInit.Double _ -> et_R8
         | ILFieldInit.Null -> et_CLASS))
                  
// -------------------------------------------------------------------- 
// Type definitions
// -------------------------------------------------------------------- 

let GetMemberAccessFlags access = 
    match access with 
    | ILMemberAccess.CompilerControlled -> 0x00000000
    | ILMemberAccess.Public -> 0x00000006
    | ILMemberAccess.Private  -> 0x00000001
    | ILMemberAccess.Family  -> 0x00000004
    | ILMemberAccess.FamilyAndAssembly -> 0x00000002
    | ILMemberAccess.FamilyOrAssembly -> 0x00000005
    | ILMemberAccess.Assembly -> 0x00000003

let GetTypeAccessFlags  access = 
    match access with 
    | ILTypeDefAccess.Public -> 0x00000001
    | ILTypeDefAccess.Private  -> 0x00000000
    | ILTypeDefAccess.Nested ILMemberAccess.Public -> 0x00000002
    | ILTypeDefAccess.Nested ILMemberAccess.Private  -> 0x00000003
    | ILTypeDefAccess.Nested ILMemberAccess.Family  -> 0x00000004
    | ILTypeDefAccess.Nested ILMemberAccess.FamilyAndAssembly -> 0x00000006
    | ILTypeDefAccess.Nested ILMemberAccess.FamilyOrAssembly -> 0x00000007
    | ILTypeDefAccess.Nested ILMemberAccess.Assembly -> 0x00000005
    | ILTypeDefAccess.Nested ILMemberAccess.CompilerControlled -> failwith "bad type acccess"

let rec GetTypeDefAsRow cenv env _enc (td:ILTypeDef) = 
    let nselem,nelem = GetTypeNameAsElemPair cenv td.Name
    let flags = 
      if (isTypeNameForGlobalFunctions td.Name) then 0x00000000
      else
        
        GetTypeAccessFlags td.Access |||
        begin 
          match td.Layout with 
          | ILTypeDefLayout.Auto ->  0x00000000
          | ILTypeDefLayout.Sequential _  -> 0x00000008
          | ILTypeDefLayout.Explicit _ -> 0x00000010
        end |||
        begin 
          match td.tdKind with
          | ILTypeDefKind.Interface -> 0x00000020
          | _ -> 0x00000000
        end |||
        (if td.IsAbstract then 0x00000080l else 0x00000000) |||
        (if td.IsSealed then 0x00000100l else 0x00000000) ||| 
        (if td.IsComInterop then 0x00001000l else 0x00000000)  |||
        (if td.IsSerializable then 0x00002000l else 0x00000000) |||
        begin 
          match td.Encoding with 
          | ILDefaultPInvokeEncoding.Ansi -> 0x00000000
          | ILDefaultPInvokeEncoding.Auto -> 0x00020000
          | ILDefaultPInvokeEncoding.Unicode ->  0x00010000
        end |||
        begin 
          match td.InitSemantics with
          |  ILTypeInit.BeforeField when not (match td.tdKind with ILTypeDefKind.Interface -> true | _ -> false) -> 0x00100000 
          | _ -> 0x00000000
        end |||
        (if td.IsSpecialName then 0x00000400 else 0x00000000) |||
        (if td.HasSecurity || not td.SecurityDecls.AsList.IsEmpty then 0x00040000 else 0x00000000)

    let tdorTag, tdorRow = GetTypeOptionAsTypeDefOrRef cenv env td.Extends
    UnsharedRow 
       [| ULong flags ; 
          nelem; 
          nselem; 
          TypeDefOrRefOrSpec (tdorTag, tdorRow); 
          SimpleIndex (TableNames.Field, cenv.fieldDefs.Count + 1); 
          SimpleIndex (TableNames.Method,cenv.methodDefIdxsByKey.Count + 1) |]  

and GetTypeOptionAsTypeDefOrRef cenv env tyOpt = 
    match tyOpt with
    | None -> (tdor_TypeDef, 0)
    | Some ty -> (GetTypeAsTypeDefOrRef cenv env ty)

and GetTypeDefAsPropertyMapRow cenv tidx = 
    UnsharedRow
        [| SimpleIndex (TableNames.TypeDef,  tidx);
           SimpleIndex (TableNames.Property, cenv.propertyDefs.Count + 1) |]  

and GetTypeDefAsEventMapRow cenv tidx = 
    UnsharedRow
        [| SimpleIndex (TableNames.TypeDef,  tidx);
           SimpleIndex (TableNames.Event, cenv.eventDefs.Count + 1) |]  
    
and GetKeyForFieldDef tidx (fd: ILFieldDef) = 
    FieldDefKey (tidx,fd.Name, fd.Type)

and GenFieldDefPass2 cenv tidx fd = 
    ignore (cenv.fieldDefs.AddUniqueEntry "field" (fun (fdkey:FieldDefKey) -> fdkey.Name) (GetKeyForFieldDef tidx fd))

and GetKeyForMethodDef tidx (md: ILMethodDef) = 
    MethodDefKey (tidx,md.GenericParams.Length, md.Name, md.Return.Type, md.ParameterTypes, md.CallingConv.IsStatic)

and GenMethodDefPass2 cenv tidx md = 
    let idx = 
      cenv.methodDefIdxsByKey.AddUniqueEntry
         "method" 
         (fun (key:MethodDefKey) -> 
           dprintn "Duplicate in method table is:";
           dprintn ("  Type index: "+string key.TypeIdx);
           dprintn ("  Method name: "+key.Name);
           dprintn ("  Method arity (num generic params): "+string key.GenericArity);
           key.Name
         )
         (GetKeyForMethodDef tidx md) 
    
    cenv.methodDefIdxs.[md] <- idx

and GetKeyForPropertyDef tidx (x: ILPropertyDef)  = 
    PropKey (tidx, x.Name, x.Type, x.Args)

and GenPropertyDefPass2 cenv tidx x = 
    ignore (cenv.propertyDefs.AddUniqueEntry "property" (fun (PropKey (_,n,_,_)) -> n) (GetKeyForPropertyDef tidx x))

and GetTypeAsImplementsRow cenv env tidx ty =
    let tdorTag,tdorRow = GetTypeAsTypeDefOrRef cenv env ty
    UnsharedRow 
        [| SimpleIndex (TableNames.TypeDef, tidx); 
           TypeDefOrRefOrSpec (tdorTag,tdorRow) |]

and GenImplementsPass2 cenv env tidx ty =
    AddUnsharedRow cenv TableNames.InterfaceImpl (GetTypeAsImplementsRow cenv env tidx ty) |> ignore
      
and GetKeyForEvent tidx (x: ILEventDef) = 
    EventKey (tidx, x.Name)

and GenEventDefPass2 cenv tidx x = 
    ignore (cenv.eventDefs.AddUniqueEntry "event" (fun (EventKey(_,b)) -> b) (GetKeyForEvent tidx x))

and GenTypeDefPass2 pidx enc cenv (td:ILTypeDef) =
   try 
      let env = envForTypeDef td
      let tidx = GetIdxForTypeDef cenv (TdKey(enc,td.Name))
      let tidx2 = AddUnsharedRow cenv TableNames.TypeDef (GetTypeDefAsRow cenv env enc td)
      if tidx <> tidx2 then failwith "index of typedef on second pass does not match index on first pass";

      // Add entries to auxiliary mapping tables, e.g. Nested, PropertyMap etc. 
      // Note Nested is organised differntly to the others... 
      if nonNil enc then 
          AddUnsharedRow cenv TableNames.Nested 
              (UnsharedRow 
                  [| SimpleIndex (TableNames.TypeDef, tidx); 
                     SimpleIndex (TableNames.TypeDef, pidx) |]) |> ignore;
      let props = td.Properties.AsList
      if nonNil props then 
          AddUnsharedRow cenv TableNames.PropertyMap (GetTypeDefAsPropertyMapRow cenv tidx) |> ignore; 
      let events = td.Events.AsList
      if nonNil events then 
          AddUnsharedRow cenv TableNames.EventMap (GetTypeDefAsEventMapRow cenv tidx) |> ignore;

      // Now generate or assign index numbers for tables referenced by the maps. 
      // Don't yet generate contents of these tables - leave that to pass3, as 
      // code may need to embed these entries. 
      td.Implements |> List.iter (GenImplementsPass2 cenv env tidx);
      props |> List.iter (GenPropertyDefPass2 cenv tidx);
      events |> List.iter (GenEventDefPass2 cenv tidx);
      td.Fields.AsList |> List.iter (GenFieldDefPass2 cenv tidx);
      td.Methods |> Seq.iter (GenMethodDefPass2 cenv tidx);
      td.NestedTypes.AsList |> GenTypeDefsPass2 tidx (enc@[td.Name]) cenv
   with e ->
     failwith ("Error in pass2 for type "+td.Name+", error: "+e.Message);
     reraise()
     raise e

and GenTypeDefsPass2 pidx enc cenv tds =
    List.iter (GenTypeDefPass2 pidx enc cenv) tds

//=====================================================================
// Pass 3 - write details of methods, fields, IL code, custom attrs etc.
//=====================================================================

exception MethodDefNotFound
let FindMethodDefIdx cenv mdkey = 
    try cenv.methodDefIdxsByKey.GetTableEntry mdkey
    with :? KeyNotFoundException -> 
      let typeNameOfIdx i = 
        match 
           (cenv.typeDefs.dict 
             |> Seq.fold (fun  sofar kvp -> 
                let tkey2 = kvp.Key 
                let tidx2 = kvp.Value 
                if i = tidx2 then 
                    if sofar = None then 
                        Some tkey2 
                    else failwith "mutiple type names map to index" 
                else sofar)  None) with 
          | Some x -> x
          | None -> raise MethodDefNotFound 
      let (TdKey (tenc,tname)) = typeNameOfIdx mdkey.TypeIdx
      dprintn ("The local method '"+(String.concat "." (tenc@[tname]))+"'::'"+mdkey.Name+"' was referenced but not declared");
      dprintn ("generic arity: "+string mdkey.GenericArity);
      cenv.methodDefIdxsByKey.dict |> Seq.iter (fun (KeyValue(mdkey2,_)) -> 
          if mdkey2.TypeIdx = mdkey.TypeIdx && mdkey.Name = mdkey2.Name then 
              let (TdKey (tenc2,tname2)) = typeNameOfIdx mdkey2.TypeIdx
              dprintn ("A method in '"+(String.concat "." (tenc2@[tname2]))+"' had the right name but the wrong signature:");
              dprintn ("generic arity: "+string mdkey2.GenericArity) ;
              dprintn (sprintf "mdkey2: %A" mdkey2)) ;
      raise MethodDefNotFound


let rec GetMethodDefIdx cenv md = 
    cenv.methodDefIdxs.[md]

and FindFieldDefIdx cenv fdkey = 
    try cenv.fieldDefs.GetTableEntry fdkey 
    with :? KeyNotFoundException -> 
      failwith ("The local field "+fdkey.Name+" was referenced but not declared");
      1

and GetFieldDefAsFieldDefIdx cenv tidx fd = 
    FindFieldDefIdx cenv (GetKeyForFieldDef tidx fd) 

// -------------------------------------------------------------------- 
// ILMethodRef --> ILMethodDef.  
// 
// Only successfuly converts ILMethodRef's referring to 
// methods in the module being emitted.
// -------------------------------------------------------------------- 

let GetMethodRefAsMethodDefIdx cenv (mref:ILMethodRef) =
    try 
        let tref = mref.EnclosingTypeRef
        if not (isTypeRefLocal tref) then
             failwithf "method referred to by method impl, event or property is not in a type defined in this module, method ref is %A" mref;
        let tidx = GetIdxForTypeDef cenv (TdKey(tref.Enclosing,tref.Name))
        let mdkey = MethodDefKey (tidx,mref.GenericArity, mref.Name, mref.ReturnType, mref.ArgTypes, mref.CallingConv.IsStatic)
        FindMethodDefIdx cenv mdkey
    with e ->
        failwithf "Error in GetMethodRefAsMethodDefIdx for mref = %A, error: %s" mref.Name  e.Message;
        reraise()
        raise e

let rec MethodRefInfoAsMemberRefRow cenv env fenv (nm,typ,callconv,args,ret,varargs,genarity) =
    MemberRefRow(GetTypeAsMemberRefParent cenv env typ,
                 GetStringHeapIdx cenv nm,
                 GetMethodRefInfoAsBlobIdx cenv fenv (callconv,args,ret,varargs,genarity))

and GetMethodRefInfoAsBlobIdx cenv env info = 
    GetBytesAsBlobIdx cenv (GetCallsigAsBytes cenv env info)

let GetMethodRefInfoAsMemberRefIdx cenv env  ((_,typ,_,_,_,_,_) as minfo) = 
    let fenv = envForMethodRef env typ
    FindOrAddRow cenv TableNames.MemberRef 
      (MethodRefInfoAsMemberRefRow cenv env fenv  minfo)

let GetMethodRefInfoAsMethodRefOrDef isAlwaysMethodDef cenv env ((nm,typ:ILType,cc,args,ret,varargs,genarity) as minfo) =
    if isNone varargs && (isAlwaysMethodDef || isTypeLocal typ) then
        if not typ.IsNominal then failwith "GetMethodRefInfoAsMethodRefOrDef: unexpected local tref-typ";
        try (mdor_MethodDef, GetMethodRefAsMethodDefIdx cenv (mkILMethRef(typ.TypeRef, cc, nm, genarity, args,ret)))
        with MethodDefNotFound -> (mdor_MemberRef, GetMethodRefInfoAsMemberRefIdx cenv env minfo)
    else (mdor_MemberRef, GetMethodRefInfoAsMemberRefIdx cenv env minfo)


// -------------------------------------------------------------------- 
// ILMethodSpec --> ILMethodRef/ILMethodDef/ILMethodSpec
// -------------------------------------------------------------------- 

let rec GetMethodSpecInfoAsMethodSpecIdx cenv env (nm,typ,cc,args,ret,varargs,minst:ILGenericArgs) = 
    let mdorTag,mdorRow = GetMethodRefInfoAsMethodRefOrDef false cenv env (nm,typ,cc,args,ret,varargs,minst.Length)
    let blob = 
        emitBytesViaBuffer (fun bb -> 
            bb.EmitByte e_IMAGE_CEE_CS_CALLCONV_GENERICINST;
            bb.EmitZ32 minst.Length;
            minst |> List.iter (EmitType cenv env bb))
    FindOrAddRow cenv TableNames.MethodSpec 
      (SimpleSharedRow 
          [| MethodDefOrRef (mdorTag,mdorRow);
             Blob (GetBytesAsBlobIdx cenv blob) |])

and GetMethodDefOrRefAsUncodedToken (tag,idx) =
    let tab = 
        if tag = mdor_MethodDef then TableNames.Method
        elif tag = mdor_MemberRef then TableNames.MemberRef  
        else failwith "GetMethodDefOrRefAsUncodedToken"
    getUncodedToken tab idx

and GetMethodSpecInfoAsUncodedToken cenv env ((_,_,_,_,_,_,minst) as minfo) =
    if minst <> [] then 
      getUncodedToken TableNames.MethodSpec (GetMethodSpecInfoAsMethodSpecIdx cenv env minfo)
    else 
      GetMethodDefOrRefAsUncodedToken (GetMethodRefInfoAsMethodRefOrDef false cenv env (GetMethodRefInfoOfMethodSpecInfo minfo))

and GetMethodSpecAsUncodedToken cenv env mspec = 
    GetMethodSpecInfoAsUncodedToken cenv env (InfoOfMethodSpec mspec)

and GetMethodRefInfoOfMethodSpecInfo (nm,typ,cc,args,ret,varargs,minst:ILGenericArgs) = 
    (nm,typ,cc,args,ret,varargs,minst.Length)

and GetMethodSpecAsMethodDefOrRef cenv env (mspec,varargs) =
    GetMethodRefInfoAsMethodRefOrDef false cenv env (GetMethodRefInfoOfMethodSpecInfo (InfoOfMethodSpec (mspec,varargs)))

and GetMethodSpecAsMethodDef cenv env (mspec,varargs) =
    GetMethodRefInfoAsMethodRefOrDef true cenv env (GetMethodRefInfoOfMethodSpecInfo (InfoOfMethodSpec (mspec,varargs)))

and InfoOfMethodSpec (mspec:ILMethodSpec,varargs) = 
      (mspec.Name,
       mspec.EnclosingType,
       mspec.CallingConv,
       mspec.FormalArgTypes,
       mspec.FormalReturnType,
       varargs,
       mspec.GenericArgs)

// -------------------------------------------------------------------- 
// method_in_parent --> ILMethodRef/ILMethodDef
// 
// Used for MethodImpls.
// --------------------------------------------------------------------

let rec GetOverridesSpecAsMemberRefIdx cenv env ospec = 
    let fenv = envForOverrideSpec ospec
    let row = 
        MethodRefInfoAsMemberRefRow cenv env fenv  
            (ospec.MethodRef.Name,
             ospec.EnclosingType,
             ospec.MethodRef.CallingConv,
             ospec.MethodRef.ArgTypes,
             ospec.MethodRef.ReturnType,
             None,
             ospec.MethodRef.GenericArity)
    FindOrAddRow cenv TableNames.MemberRef  row
     
and GetOverridesSpecAsMethodDefOrRef cenv env (ospec:ILOverridesSpec) =
    let typ = ospec.EnclosingType
    if isTypeLocal typ then 
        if not typ.IsNominal then failwith "GetOverridesSpecAsMethodDefOrRef: unexpected local tref-typ"; 
        try (mdor_MethodDef, GetMethodRefAsMethodDefIdx cenv ospec.MethodRef)
        with MethodDefNotFound ->  (mdor_MemberRef, GetOverridesSpecAsMemberRefIdx cenv env ospec) 
    else 
        (mdor_MemberRef, GetOverridesSpecAsMemberRefIdx cenv env ospec) 

// -------------------------------------------------------------------- 
// ILMethodRef --> ILMethodRef/ILMethodDef
// 
// Used for Custom Attrs.
// -------------------------------------------------------------------- 

let rec GetMethodRefAsMemberRefIdx cenv env fenv (mref:ILMethodRef) = 
    let row = 
        MethodRefInfoAsMemberRefRow cenv env fenv 
            (mref.Name,
             mkILNonGenericBoxedTy mref.EnclosingTypeRef,
             mref.CallingConv,
             mref.ArgTypes,
             mref.ReturnType,
             None,
             mref.GenericArity)
    FindOrAddRow cenv TableNames.MemberRef row

and GetMethodRefAsCustomAttribType cenv (mref:ILMethodRef) =
    let fenv = envForNonGenericMethodRef mref
    let tref = mref.EnclosingTypeRef
    if isTypeRefLocal tref then
        try (cat_MethodDef, GetMethodRefAsMethodDefIdx cenv mref)
        with MethodDefNotFound -> (cat_MemberRef, GetMethodRefAsMemberRefIdx cenv fenv fenv mref)
    else
        (cat_MemberRef, GetMethodRefAsMemberRefIdx cenv fenv fenv mref)

// -------------------------------------------------------------------- 
// ILAttributes --> CustomAttribute rows
// -------------------------------------------------------------------- 

let rec GetCustomAttrDataAsBlobIdx cenv (data:byte[]) = 
    if data.Length = 0 then 0 else GetBytesAsBlobIdx cenv data

and GetCustomAttrRow cenv hca attr = 
    let cat = GetMethodRefAsCustomAttribType cenv attr.Method.MethodRef
    UnsharedRow 
        [| HasCustomAttribute (fst hca, snd hca);
           CustomAttributeType (fst cat, snd cat); 
           Blob (GetCustomAttrDataAsBlobIdx cenv attr.Data); |]  

and GenCustomAttrPass3 cenv hca attr = 
    AddUnsharedRow cenv TableNames.CustomAttribute (GetCustomAttrRow cenv hca attr) |> ignore

and GenCustomAttrsPass3 cenv hca (attrs: ILAttributes) = 
    attrs.AsList |> List.iter (GenCustomAttrPass3 cenv hca) 

// -------------------------------------------------------------------- 
// ILPermissionSet --> DeclSecurity rows
// -------------------------------------------------------------------- *)

let rec GetSecurityDeclRow cenv hds (PermissionSet (action, s)) = 
    UnsharedRow 
        [| UShort (uint16 (List.assoc action (Lazy.force ILSecurityActionMap)));
           HasDeclSecurity (fst hds, snd hds);
           Blob (GetBytesAsBlobIdx cenv s); |]  

and GenSecurityDeclPass3 cenv hds attr = 
    AddUnsharedRow cenv TableNames.Permission (GetSecurityDeclRow cenv hds attr) |> ignore

and GenSecurityDeclsPass3 cenv hds attrs = 
    List.iter (GenSecurityDeclPass3 cenv hds) attrs 

// -------------------------------------------------------------------- 
// ILFieldSpec --> FieldRef  or ILFieldDef row
// -------------------------------------------------------------------- 

let rec GetFieldSpecAsMemberRefRow cenv env fenv (fspec:ILFieldSpec) = 
    MemberRefRow (GetTypeAsMemberRefParent cenv env fspec.EnclosingType,
                  GetStringHeapIdx cenv fspec.Name,
                  GetFieldSpecSigAsBlobIdx cenv fenv fspec)

and GetFieldSpecAsMemberRefIdx cenv env fspec = 
    let fenv = envForFieldSpec fspec
    FindOrAddRow cenv TableNames.MemberRef (GetFieldSpecAsMemberRefRow cenv env fenv fspec)

and EmitFieldSpecSig cenv env (bb: ByteBuffer) (fspec:ILFieldSpec) = 
    bb.EmitByte e_IMAGE_CEE_CS_CALLCONV_FIELD;
    EmitType cenv env bb fspec.FormalType

and GetFieldSpecSigAsBytes cenv env x = 
    emitBytesViaBuffer (fun bb -> EmitFieldSpecSig cenv env bb x) 

and GetFieldSpecSigAsBlobIdx cenv env x = 
    GetBytesAsBlobIdx cenv (GetFieldSpecSigAsBytes cenv env x)

and GetFieldSpecAsFieldDefOrRef cenv env (fspec:ILFieldSpec) =
    let typ = fspec.EnclosingType
    if isTypeLocal typ then
        if not typ.IsNominal then failwith "GetFieldSpecAsFieldDefOrRef: unexpected local tref-typ";
        let tref = typ.TypeRef
        let tidx = GetIdxForTypeDef cenv (TdKey(tref.Enclosing,tref.Name))
        let fdkey = FieldDefKey (tidx,fspec.Name, fspec.FormalType)
        (true, FindFieldDefIdx cenv fdkey)
    else 
        (false, GetFieldSpecAsMemberRefIdx cenv env fspec)

and GetFieldDefOrRefAsUncodedToken (tag,idx) =
    let tab = if tag then TableNames.Field else TableNames.MemberRef
    getUncodedToken tab idx

// -------------------------------------------------------------------- 
// callsig --> StandAloneSig
// -------------------------------------------------------------------- 

let GetCallsigAsBlobIdx cenv env (callsig:ILCallingSignature,varargs) = 
    GetBytesAsBlobIdx cenv 
      (GetCallsigAsBytes cenv env (callsig.CallingConv,
                                      callsig.ArgTypes,
                                      callsig.ReturnType,varargs,0))
    
let GetCallsigAsStandAloneSigRow cenv env x = 
    SimpleSharedRow [| Blob (GetCallsigAsBlobIdx cenv env x) |]

let GetCallsigAsStandAloneSigIdx cenv env info = 
    FindOrAddRow cenv TableNames.StandAloneSig (GetCallsigAsStandAloneSigRow cenv env info)

// -------------------------------------------------------------------- 
// local signatures --> BlobHeap idx
// -------------------------------------------------------------------- 

let EmitLocalSig cenv env (bb: ByteBuffer) (locals: ILLocal list) = 
    bb.EmitByte e_IMAGE_CEE_CS_CALLCONV_LOCAL_SIG;
    bb.EmitZ32 locals.Length;
    locals |> List.iter (fun l -> EmitType cenv env bb l.Type) 

let GetLocalSigAsBlobHeapIdx cenv env locals = 
    GetBytesAsBlobIdx cenv (emitBytesViaBuffer (fun bb -> EmitLocalSig cenv env bb locals))

let GetLocalSigAsStandAloneSigIdx cenv env locals = 
    SimpleSharedRow [| Blob (GetLocalSigAsBlobHeapIdx cenv env locals) |]



type ExceptionClauseKind = 
  | FinallyClause 
  | FaultClause 
  | TypeFilterClause of int32 
  | FilterClause of int

type ExceptionClauseSpec = (int * int * int * int * ExceptionClauseKind)

type CodeBuffer = 

    // -------------------------------------------------------------------- 
    // Buffer to write results of emitting code into.  Also record:
    //   - branch sources (where fixups will occur)
    //   - possible branch destinations
    //   - locations of embedded handles into the string table
    //   - the exception table
    // -------------------------------------------------------------------- 
    { code: ByteBuffer; 
      /// (instruction; optional short form); start of instr in code buffer; code loc for the end of the instruction the fixup resides in ; where is the destination of the fixup 
      mutable reqdBrFixups: ((int * int option) * int * ILCodeLabel list) list; 
      availBrFixups: Dictionary<ILCodeLabel, int> ;
      /// code loc to fixup in code buffer 
      mutable reqdStringFixupsInMethod: (int * int) list; 
      /// data for exception handling clauses 
      mutable seh: ExceptionClauseSpec list; 
      seqpoints: ResizeArray<PdbSequencePoint>; }

    static member Create _nm = 
        { seh = [];
          code= ByteBuffer.Create 200;
          reqdBrFixups=[];
          reqdStringFixupsInMethod=[];
          availBrFixups = Dictionary<_,_>(10, HashIdentity.Structural) 
          seqpoints = new ResizeArray<_>(10)
        }

    member codebuf.EmitExceptionClause seh = codebuf.seh <- seh :: codebuf.seh

    member codebuf.EmitSeqPoint cenv (m:ILSourceMarker)  = 
        if cenv.generatePdb then 
          // table indexes are 1-based, document array indexes are 0-based 
          let doc = (cenv.documents.FindOrAddSharedEntry m.Document) - 1  
          codebuf.seqpoints.Add 
            { Document=doc;
              Offset= codebuf.code.Position;
              Line=m.Line;
              Column=m.Column;
              EndLine=m.EndLine;
              EndColumn=m.EndColumn; }
              
    member codebuf.EmitByte x = codebuf.code.EmitIntAsByte x
    member codebuf.EmitUInt16 x = codebuf.code.EmitUInt16 x
    member codebuf.EmitInt32 x = codebuf.code.EmitInt32 x
    member codebuf.EmitInt64 x = codebuf.code.EmitInt64 x

    member codebuf.EmitUncodedToken u = codebuf.EmitInt32 u

    member codebuf.RecordReqdStringFixup stringidx = 
        codebuf.reqdStringFixupsInMethod <- (codebuf.code.Position, stringidx) :: codebuf.reqdStringFixupsInMethod;
        // Write a special value in that we check later when applying the fixup 
        codebuf.EmitInt32 0xdeadbeef

    member codebuf.RecordReqdBrFixups i tgs = 
        codebuf.reqdBrFixups <- (i, codebuf.code.Position, tgs) :: codebuf.reqdBrFixups;
        // Write a special value in that we check later when applying the fixup 
        // Value is 0x11 {deadbbbb}* where 11 is for the instruction and deadbbbb is for each target 
        codebuf.EmitByte 0x11; // for the instruction 
        (if fst i = i_switch then 
          codebuf.EmitInt32 tgs.Length);
        List.iter (fun _ -> codebuf.EmitInt32 0xdeadbbbb) tgs

    member codebuf.RecordReqdBrFixup i tg = codebuf.RecordReqdBrFixups i [tg]
    member codebuf.RecordAvailBrFixup tg = 
        codebuf.availBrFixups.[tg] <- codebuf.code.Position

module Codebuf = begin
     // -------------------------------------------------------------------- 
     // Applying branch fixups.  Use short versions of instructions
     // wherever possible.  Sadly we can only determine if we can use a short
     // version after we've layed out the code for all other instructions.  
     // This in turn means that using a short version may change 
     // the various offsets into the code.
     // -------------------------------------------------------------------- 

    let binaryChop p (arr: 'T[]) = 
        let rec go n m =
            if n > m then raise (KeyNotFoundException("binary chop did not find element"))
            else 
                let i = (n+m)/2
                let c = p arr.[i] 
                if c = 0 then i elif c < 0 then go n (i-1) else go (i+1) m
        go 0 (Array.length arr)

    let applyBrFixups (origCode :byte[]) origExnClauses origReqdStringFixups (origAvailBrFixups: Dictionary<ILCodeLabel,int>) origReqdBrFixups origSeqPoints origScopes = 
      let orderedOrigReqdBrFixups = origReqdBrFixups |> List.sortBy (fun (_,fixuploc,_) -> fixuploc)

      let newCode = ByteBuffer.Create origCode.Length

      // Copy over all the code, working out whether the branches will be short 
      // or long and adjusting the branch destinations.  Record an adjust function to adjust all the other 
      // gumpf that refers to fixed offsets in the code stream. 
      let newCode, newReqdBrFixups,adjuster = 
          let remainingReqdFixups = ref orderedOrigReqdBrFixups
          let origWhere = ref 0
          let newWhere = ref 0
          let doneLast = ref false
          let newReqdBrFixups = ref []

          let adjustments = ref []

          while (!remainingReqdFixups <> [] || not !doneLast) do
              let doingLast = isNil !remainingReqdFixups  
              let origStartOfNoBranchBlock = !origWhere
              let newStartOfNoBranchBlock = !newWhere

              let origEndOfNoBranchBlock = 
                if doingLast then origCode.Length
                else 
                  let (_,origStartOfInstr,_) = List.head !remainingReqdFixups
                  origStartOfInstr

              // Copy over a chunk of non-branching code 
              let nobranch_len = origEndOfNoBranchBlock - origStartOfNoBranchBlock
              newCode.EmitBytes origCode.[origStartOfNoBranchBlock..origStartOfNoBranchBlock+nobranch_len-1];
                
              // Record how to adjust addresses in this range, including the branch instruction 
              // we write below, or the end of the method if we're doing the last bblock 
              adjustments := (origStartOfNoBranchBlock,origEndOfNoBranchBlock,newStartOfNoBranchBlock) :: !adjustments;
             
              // Increment locations to the branch instruction we're really interested in  
              origWhere := origEndOfNoBranchBlock;
              newWhere := !newWhere + nobranch_len;
                
              // Now do the branch instruction.  Decide whether the fixup will be short or long in the new code 
              if doingLast then 
                  doneLast := true
              else 
                  let (i,origStartOfInstr,tgs:ILCodeLabel list) = List.head !remainingReqdFixups
                  remainingReqdFixups := List.tail !remainingReqdFixups;
                  if origCode.[origStartOfInstr] <> 0x11uy then failwith "br fixup sanity check failed (1)";
                  let i_length = if fst i = i_switch then 5 else 1
                  origWhere := !origWhere + i_length;

                  let origEndOfInstr = origStartOfInstr + i_length + 4 * tgs.Length
                  let newEndOfInstrIfSmall = !newWhere + i_length + 1
                  let newEndOfInstrIfBig = !newWhere + i_length + 4 * tgs.Length
                  
                  let short = 
                    match i,tgs with 
                    | (_,Some i_short),[tg] 
                        when
                          begin 
                            // Use the original offsets to compute if the branch is small or large.  This is 
                            // a safe approximation because code only gets smaller. 
                            if not (origAvailBrFixups.ContainsKey tg) then 
                                dprintn ("branch target " + formatCodeLabel tg + " not found in code");
                            let origDest = 
                                if origAvailBrFixups.ContainsKey tg then origAvailBrFixups.[tg]
                                else 666666
                            let origRelOffset = origDest - origEndOfInstr
                            -128 <= origRelOffset && origRelOffset <= 127
                          end 
                      ->
                        newCode.EmitIntAsByte i_short;
                        true
                    | (i_long,_),_ ->
                        newCode.EmitIntAsByte i_long;
                        (if i_long = i_switch then 
                          newCode.EmitInt32 tgs.Length);
                        false
                  
                  newWhere := !newWhere + i_length;
                  if !newWhere <> newCode.Position then dprintn "mismatch between newWhere and newCode";

                  tgs |> List.iter (fun tg ->
                        let origFixupLoc = !origWhere
                        checkFixup32 origCode origFixupLoc 0xdeadbbbb;
                        
                        if short then 
                            newReqdBrFixups := (!newWhere, newEndOfInstrIfSmall, tg, true) :: !newReqdBrFixups;
                            newCode.EmitIntAsByte 0x98; (* sanity check *)
                            newWhere := !newWhere + 1;
                        else 
                            newReqdBrFixups := (!newWhere, newEndOfInstrIfBig, tg, false) :: !newReqdBrFixups;
                            newCode.EmitInt32 0xf00dd00f; (* sanity check *)
                            newWhere := !newWhere + 4;
                        if !newWhere <> newCode.Position then dprintn "mismatch between newWhere and newCode";
                        origWhere := !origWhere + 4);
                  
                  if !origWhere <> origEndOfInstr then dprintn "mismatch between origWhere and origEndOfInstr";

          let adjuster  = 
            let arr = Array.ofList (List.rev !adjustments)
            fun addr -> 
              let i = 
                  try binaryChop (fun (a1,a2,_) -> if addr < a1 then -1 elif addr > a2 then 1 else 0) arr 
                  with 
                     :? KeyNotFoundException -> 
                         failwith ("adjuster: address "+string addr+" is out of range")
              let (origStartOfNoBranchBlock,_,newStartOfNoBranchBlock) = arr.[i]
              addr - (origStartOfNoBranchBlock - newStartOfNoBranchBlock) 

          newCode.Close(), 
          !newReqdBrFixups, 
          adjuster

      // Now adjust everything 
      let newAvailBrFixups = 
          let tab = Dictionary<_,_>(10, HashIdentity.Structural) 
          for (KeyValue(tglab,origBrDest)) in origAvailBrFixups do 
              tab.[tglab]  <- adjuster origBrDest
          tab
      let newReqdStringFixups = List.map (fun (origFixupLoc,stok) -> adjuster origFixupLoc,stok) origReqdStringFixups
      let newSeqPoints = Array.map (fun (sp:PdbSequencePoint) -> {sp with Offset=adjuster sp.Offset}) origSeqPoints
      let newExnClauses = 
          origExnClauses |> List.map (fun (st1,sz1,st2,sz2,kind) ->
              (adjuster st1,(adjuster (st1 + sz1) - adjuster st1),
               adjuster st2,(adjuster (st2 + sz2) - adjuster st2),
               (match kind with 
               | FinallyClause | FaultClause | TypeFilterClause _ -> kind
               | FilterClause n -> FilterClause (adjuster n))))
            
      let newScopes =
        let rec remap scope =
          {scope with StartOffset = adjuster scope.StartOffset;
                      EndOffset = adjuster scope.EndOffset;
                      Children = Array.map remap scope.Children }
        List.map remap origScopes
      
      // Now apply the adjusted fixups in the new code 
      newReqdBrFixups |> List.iter (fun (newFixupLoc,endOfInstr,tg, small) ->
            if not (newAvailBrFixups.ContainsKey tg) then 
              failwith ("target "+formatCodeLabel tg+" not found in new fixups");
            try 
                let n = newAvailBrFixups.[tg]
                let relOffset = (n - endOfInstr)
                if small then 
                    if Bytes.get newCode newFixupLoc <> 0x98 then failwith "br fixupsanity check failed";
                    newCode.[newFixupLoc] <- b0 relOffset;
                else 
                    checkFixup32 newCode newFixupLoc 0xf00dd00fl;
                    applyFixup32 newCode newFixupLoc relOffset
            with :? KeyNotFoundException -> ());

      newCode, newReqdStringFixups, newExnClauses, newSeqPoints, newScopes


    // -------------------------------------------------------------------- 
    // Structured residue of emitting instructions: SEH exception handling
    // and scopes for local variables.
    // -------------------------------------------------------------------- 

    // Emitting instructions generates a tree of seh specifications 
    // We then emit the exception handling specs separately. 
    // nb. ECMA spec says the SEH blocks must be returned inside-out 
    type SEHTree = 
      | Tip 
      | Node of (ExceptionClauseSpec option * SEHTree list) list
        

    // -------------------------------------------------------------------- 
    // Table of encodings for instructions without arguments, also indexes
    // for all instructions.
    // -------------------------------------------------------------------- 

    let encodingsForNoArgInstrs = System.Collections.Generic.Dictionary<_,_>(300, HashIdentity.Structural)
    let _ = 
      List.iter 
        (fun (x,mk) -> encodingsForNoArgInstrs.[mk] <- x)
        (noArgInstrs.Force())
    let encodingsOfNoArgInstr si = encodingsForNoArgInstrs.[si]

    // -------------------------------------------------------------------- 
    // Emit instructions
    // -------------------------------------------------------------------- 

    /// Emit the code for an instruction
    let emitInstrCode (codebuf: CodeBuffer) i = 
        if i > 0xFF then 
            assert (i >>> 8 = 0xFE); 
            codebuf.EmitByte ((i >>> 8)  &&& 0xFF); 
            codebuf.EmitByte (i &&& 0xFF); 
        else 
            codebuf.EmitByte i

    let emitTypeInstr cenv codebuf env i ty = 
        emitInstrCode codebuf i; 
        codebuf.EmitUncodedToken (getTypeDefOrRefAsUncodedToken (GetTypeAsTypeDefOrRef cenv env ty))

    let emitMethodSpecInfoInstr cenv codebuf env i mspecinfo = 
        emitInstrCode codebuf i; 
        codebuf.EmitUncodedToken (GetMethodSpecInfoAsUncodedToken cenv env mspecinfo)

    let emitMethodSpecInstr cenv codebuf env i mspec = 
        emitInstrCode codebuf i; 
        codebuf.EmitUncodedToken (GetMethodSpecAsUncodedToken cenv env mspec)

    let emitFieldSpecInstr cenv codebuf env i fspec = 
        emitInstrCode codebuf i; 
        codebuf.EmitUncodedToken (GetFieldDefOrRefAsUncodedToken (GetFieldSpecAsFieldDefOrRef cenv env fspec))

    let emitShortUInt16Instr codebuf (i_short,i) x = 
        let n = int32 x
        if n <= 255 then 
            emitInstrCode codebuf i_short; 
            codebuf.EmitByte n;
        else 
            emitInstrCode codebuf i; 
            codebuf.EmitUInt16 x;

    let emitShortInt32Instr codebuf (i_short,i) x = 
        if x >= (-128) && x <= 127 then 
            emitInstrCode codebuf i_short; 
            codebuf.EmitByte (if x < 0x0 then x + 256 else x);
        else 
            emitInstrCode codebuf i; 
            codebuf.EmitInt32 x;

    let emitTailness (cenv: cenv) codebuf tl = 
        if tl = Tailcall && cenv.emitTailcalls then emitInstrCode codebuf i_tail

    let emitAfterTailcall codebuf tl =
        if tl = Tailcall then emitInstrCode codebuf i_ret

    let emitVolatility codebuf tl = 
        if tl = Volatile then emitInstrCode codebuf i_volatile

    let emitConstrained cenv codebuf env ty = 
        emitInstrCode codebuf i_constrained;
        codebuf.EmitUncodedToken (getTypeDefOrRefAsUncodedToken (GetTypeAsTypeDefOrRef cenv env ty))

    let emitAlignment codebuf tl = 
        match tl with 
        | Aligned -> ()
        | Unaligned1 -> emitInstrCode codebuf i_unaligned; codebuf.EmitByte 0x1
        | Unaligned2 -> emitInstrCode codebuf i_unaligned; codebuf.EmitByte 0x2
        | Unaligned4 -> emitInstrCode codebuf i_unaligned; codebuf.EmitByte 0x4

    let rec emitInstr cenv codebuf env instr =
        match instr with
        | si when isNoArgInstr si ->
             emitInstrCode codebuf (encodingsOfNoArgInstr si)
        | I_brcmp (cmp,tg1,_)  -> 
            codebuf.RecordReqdBrFixup ((Lazy.force ILCmpInstrMap).[cmp], Some (Lazy.force ILCmpInstrRevMap).[cmp]) tg1
        | I_br _ -> ()
        | I_seqpoint s ->   codebuf.EmitSeqPoint cenv s
        | I_leave tg -> codebuf.RecordReqdBrFixup (i_leave,Some i_leave_s) tg
        | I_call  (tl,mspec,varargs)      -> 
            emitTailness cenv codebuf tl;
            emitMethodSpecInstr cenv codebuf env i_call (mspec,varargs);
            emitAfterTailcall codebuf tl
        | I_callvirt      (tl,mspec,varargs)      -> 
            emitTailness cenv codebuf tl;
            emitMethodSpecInstr cenv codebuf env i_callvirt (mspec,varargs);
            emitAfterTailcall codebuf tl
        | I_callconstraint        (tl,ty,mspec,varargs)   -> 
            emitTailness cenv codebuf tl;
            emitConstrained cenv codebuf env ty;
            emitMethodSpecInstr cenv codebuf env i_callvirt (mspec,varargs);
            emitAfterTailcall codebuf tl
        | I_newobj        (mspec,varargs) -> 
            emitMethodSpecInstr cenv codebuf env i_newobj (mspec,varargs)
        | I_ldftn mspec   -> 
            emitMethodSpecInstr cenv codebuf env i_ldftn (mspec,None)
        | I_ldvirtftn     mspec   -> 
            emitMethodSpecInstr cenv codebuf env i_ldvirtftn (mspec,None)

        | I_calli (tl,callsig,varargs)    -> 
            emitTailness cenv codebuf tl;
            emitInstrCode codebuf i_calli; 
            codebuf.EmitUncodedToken (getUncodedToken TableNames.StandAloneSig (GetCallsigAsStandAloneSigIdx cenv env (callsig,varargs)));
            emitAfterTailcall codebuf tl

        | I_ldarg u16 ->  emitShortUInt16Instr codebuf (i_ldarg_s,i_ldarg) u16 
        | I_starg u16 ->  emitShortUInt16Instr codebuf (i_starg_s,i_starg) u16 
        | I_ldarga u16 ->  emitShortUInt16Instr codebuf (i_ldarga_s,i_ldarga) u16 
        | I_ldloc u16 ->  emitShortUInt16Instr codebuf (i_ldloc_s,i_ldloc) u16 
        | I_stloc u16 ->  emitShortUInt16Instr codebuf (i_stloc_s,i_stloc) u16 
        | I_ldloca u16 ->  emitShortUInt16Instr codebuf (i_ldloca_s,i_ldloca) u16 

        | I_cpblk (al,vol)        -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitInstrCode codebuf i_cpblk
        | I_initblk       (al,vol)        -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitInstrCode codebuf i_initblk

        | (AI_ldc (DT_I4, ILConst.I4 x)) -> 
            emitShortInt32Instr codebuf (i_ldc_i4_s,i_ldc_i4) x
        | (AI_ldc (DT_I8, ILConst.I8 x)) -> 
            emitInstrCode codebuf i_ldc_i8; 
            codebuf.EmitInt64 x;
        | (AI_ldc (_, ILConst.R4 x)) -> 
            emitInstrCode codebuf i_ldc_r4; 
            codebuf.EmitInt32 (bitsOfSingle x)
        | (AI_ldc (_, ILConst.R8 x)) -> 
            emitInstrCode codebuf i_ldc_r8; 
            codebuf.EmitInt64 (bitsOfDouble x)

        | I_ldind (al,vol,dt)     -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitInstrCode codebuf 
              (match dt with 
              | DT_I -> i_ldind_i
              | DT_I1  -> i_ldind_i1     
              | DT_I2  -> i_ldind_i2     
              | DT_I4  -> i_ldind_i4     
              | DT_U1  -> i_ldind_u1     
              | DT_U2  -> i_ldind_u2     
              | DT_U4  -> i_ldind_u4     
              | DT_I8  -> i_ldind_i8     
              | DT_R4  -> i_ldind_r4     
              | DT_R8  -> i_ldind_r8     
              | DT_REF  -> i_ldind_ref
              | _ -> failwith "ldind")

        | I_stelem dt     -> 
            emitInstrCode codebuf 
              (match dt with 
              | DT_I | DT_U -> i_stelem_i
              | DT_U1 | DT_I1  -> i_stelem_i1     
              | DT_I2 | DT_U2  -> i_stelem_i2     
              | DT_I4 | DT_U4  -> i_stelem_i4     
              | DT_I8 | DT_U8  -> i_stelem_i8     
              | DT_R4  -> i_stelem_r4     
              | DT_R8  -> i_stelem_r8     
              | DT_REF  -> i_stelem_ref
              | _ -> failwith "stelem")

        | I_ldelem dt     -> 
            emitInstrCode codebuf 
              (match dt with 
              | DT_I -> i_ldelem_i
              | DT_I1  -> i_ldelem_i1     
              | DT_I2  -> i_ldelem_i2     
              | DT_I4  -> i_ldelem_i4     
              | DT_I8  -> i_ldelem_i8     
              | DT_U1  -> i_ldelem_u1     
              | DT_U2  -> i_ldelem_u2     
              | DT_U4  -> i_ldelem_u4     
              | DT_R4  -> i_ldelem_r4     
              | DT_R8  -> i_ldelem_r8     
              | DT_REF  -> i_ldelem_ref
              | _ -> failwith "ldelem")

        | I_stind (al,vol,dt)     -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitInstrCode codebuf 
              (match dt with 
              | DT_U | DT_I -> i_stind_i
              | DT_U1 | DT_I1  -> i_stind_i1     
              | DT_U2 | DT_I2  -> i_stind_i2     
              | DT_U4 | DT_I4  -> i_stind_i4     
              | DT_U8 | DT_I8  -> i_stind_i8     
              | DT_R4  -> i_stind_r4     
              | DT_R8  -> i_stind_r8     
              | DT_REF  -> i_stind_ref
              | _ -> failwith "stelem")

        | I_switch (labs,_)    ->  codebuf.RecordReqdBrFixups (i_switch,None) labs

        | I_ldfld (al,vol,fspec)  -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitFieldSpecInstr cenv codebuf env i_ldfld fspec
        | I_ldflda        fspec   -> 
            emitFieldSpecInstr cenv codebuf env i_ldflda fspec
        | I_ldsfld        (vol,fspec)     -> 
            emitVolatility codebuf vol;
            emitFieldSpecInstr cenv codebuf env i_ldsfld fspec
        | I_ldsflda       fspec   -> 
            emitFieldSpecInstr cenv codebuf env i_ldsflda fspec
        | I_stfld (al,vol,fspec)  -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitFieldSpecInstr cenv codebuf env i_stfld fspec
        | I_stsfld        (vol,fspec)     -> 
            emitVolatility codebuf vol;
            emitFieldSpecInstr cenv codebuf env i_stsfld fspec

        | I_ldtoken  tok  -> 
            emitInstrCode codebuf i_ldtoken;
            codebuf.EmitUncodedToken 
              (match tok with 
              | ILToken.ILType typ -> 
                  match GetTypeAsTypeDefOrRef cenv env typ with 
                  | (tag,idx) when tag = tdor_TypeDef -> getUncodedToken TableNames.TypeDef idx
                  | (tag,idx) when tag = tdor_TypeRef -> getUncodedToken TableNames.TypeRef idx
                  | (tag,idx) when tag = tdor_TypeSpec -> getUncodedToken TableNames.TypeSpec idx
                  | _ -> failwith "?"
              | ILToken.ILMethod mspec ->
                  match GetMethodSpecAsMethodDefOrRef cenv env (mspec,None) with 
                  | (tag,idx) when tag = mdor_MethodDef -> getUncodedToken TableNames.Method idx
                  | (tag,idx) when tag = mdor_MemberRef -> getUncodedToken TableNames.MemberRef idx
                  | _ -> failwith "?"

              | ILToken.ILField fspec ->
                  match GetFieldSpecAsFieldDefOrRef cenv env fspec with 
                  | (true,idx) -> getUncodedToken TableNames.Field idx
                  | (false,idx)  -> getUncodedToken TableNames.MemberRef idx)
        | I_ldstr s       -> 
            emitInstrCode codebuf i_ldstr;
            codebuf.RecordReqdStringFixup (GetUserStringHeapIdx cenv s)

        | I_box  ty       -> emitTypeInstr cenv codebuf env i_box ty
        | I_unbox  ty     -> emitTypeInstr cenv codebuf env i_unbox ty
        | I_unbox_any  ty -> emitTypeInstr cenv codebuf env i_unbox_any ty 

        | I_newarr (shape,ty) -> 
            if (shape = ILArrayShape.SingleDimensional) then   
                emitTypeInstr cenv codebuf env i_newarr ty
            else
                let rank = shape.Rank
                let args = Array.toList (Array.create rank cenv.ilg.typ_int32)
                emitMethodSpecInfoInstr cenv codebuf env i_newobj (".ctor",mkILArrTy(ty,shape),ILCallingConv.Instance,args,ILType.Void,None,[])

        | I_stelem_any (shape,ty) -> 
            if (shape = ILArrayShape.SingleDimensional) then   
                emitTypeInstr cenv codebuf env i_stelem_any ty  
            else 
                let rank = shape.Rank
                let args = Array.toList (Array.create rank cenv.ilg.typ_int32) @ [ty]
                emitMethodSpecInfoInstr cenv codebuf env i_call ("Set",mkILArrTy(ty,shape),ILCallingConv.Instance,args,ILType.Void,None,[])

        | I_ldelem_any (shape,ty) -> 
            if (shape = ILArrayShape.SingleDimensional) then   
                emitTypeInstr cenv codebuf env i_ldelem_any ty  
            else 
                let rank = shape.Rank
                let args = Array.toList (Array.create rank cenv.ilg.typ_int32)
                emitMethodSpecInfoInstr cenv codebuf env i_call ("Get",mkILArrTy(ty,shape),ILCallingConv.Instance,args,ty,None,[])

        | I_ldelema  (ro,shape,ty) -> 
            if (ro = ReadonlyAddress) then
                emitInstrCode codebuf i_readonly;
            if (shape = ILArrayShape.SingleDimensional) then   
                emitTypeInstr cenv codebuf env i_ldelema ty
            else 
                let rank = shape.Rank
                let args = Array.toList (Array.create rank cenv.ilg.typ_int32)
                emitMethodSpecInfoInstr cenv codebuf env i_call ("Address",mkILArrTy(ty,shape),ILCallingConv.Instance,args,ILType.Byref ty,None,[])

        | I_castclass  ty -> emitTypeInstr cenv codebuf env i_castclass ty
        | I_isinst  ty -> emitTypeInstr cenv codebuf env i_isinst ty
        | I_refanyval  ty -> emitTypeInstr cenv codebuf env i_refanyval ty
        | I_mkrefany  ty -> emitTypeInstr cenv codebuf env i_mkrefany ty
        | I_initobj  ty -> emitTypeInstr cenv codebuf env i_initobj ty
        | I_ldobj  (al,vol,ty) -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitTypeInstr cenv codebuf env i_ldobj ty
        | I_stobj  (al,vol,ty) -> 
            emitAlignment codebuf al; 
            emitVolatility codebuf vol;
            emitTypeInstr cenv codebuf env i_stobj ty
        | I_cpobj  ty -> emitTypeInstr cenv codebuf env i_cpobj ty
        | I_sizeof  ty -> emitTypeInstr cenv codebuf env i_sizeof ty
        | EI_ldlen_multi (_,m)    -> 
            emitShortInt32Instr codebuf (i_ldc_i4_s,i_ldc_i4) m;
            emitInstr cenv codebuf env (mkNormalCall(mkILNonGenericMethSpecInTy(cenv.ilg.typ_Array, ILCallingConv.Instance, "GetLength", [(cenv.ilg.typ_int32)], (cenv.ilg.typ_int32))))

        |  _ -> failwith "an IL instruction cannot be emitted"


    let mkScopeNode cenv (localSigs: _[]) (a,b,ls,ch) = 
        if (isNil ls || not cenv.generatePdb) then ch
        else
          [ { Children= Array.ofList ch;
              StartOffset=a;
              EndOffset=b;
              Locals=
                  Array.ofList
                    (List.map
                       (fun x -> { Name=x.LocalName;
                                   Signature= (try localSigs.[x.LocalIndex] with _ -> failwith ("local variable index "+string x.LocalIndex+"in debug info does not reference a valid local"));
                                   Index= x.LocalIndex } ) 
                       (List.filter (fun v -> v.LocalName <> "") ls)) } ]
            
    let rec emitCode cenv localSigs codebuf env (susp,code) = 
        match code with 
        | TryBlock (c,seh) -> 
            commitSusp codebuf susp (uniqueEntryOfCode c);
            let tryStart = codebuf.code.Position
            let susp,child1,scope1 = emitCode cenv localSigs codebuf env (None,c)
            commitSuspNoDest codebuf susp;
            let tryFinish = codebuf.code.Position
            let exnBranches = 
                match seh with 
                | FaultBlock flt -> 
                    let handlerStart = codebuf.code.Position
                    let susp,child2,scope2 = emitCode cenv localSigs codebuf env (None,flt)
                    commitSuspNoDest codebuf susp;
                    let handlerFinish = codebuf.code.Position
                    [ Some (tryStart,(tryFinish - tryStart),
                            handlerStart,(handlerFinish - handlerStart),
                            FaultClause), 
                      [(child2,scope2)] ]
                      
                | FinallyBlock flt -> 
                    let handlerStart = codebuf.code.Position
                    let susp,child2,scope2 = emitCode cenv localSigs codebuf env (None,flt)
                    commitSuspNoDest codebuf susp;
                    let handlerFinish = codebuf.code.Position
                    [ Some (tryStart,(tryFinish - tryStart),
                            handlerStart,(handlerFinish - handlerStart),
                            FinallyClause),
                      [(child2,scope2)] ]
                      
                | FilterCatchBlock clauses -> 
                    clauses |> List.map (fun (flt,ctch) -> 
                        match flt with 
                        | TypeFilter typ ->
                            let handlerStart = codebuf.code.Position
                            let susp,child2,scope2 = emitCode cenv localSigs codebuf env (None,ctch)
                            commitSuspNoDest codebuf susp;
                            let handlerFinish = codebuf.code.Position
                            Some (tryStart,(tryFinish - tryStart),
                                  handlerStart,(handlerFinish - handlerStart),
                                  TypeFilterClause (getTypeDefOrRefAsUncodedToken (GetTypeAsTypeDefOrRef cenv env typ))),
                            [(child2,scope2)]
                        | CodeFilter fltcode -> 
                            
                            let filterStart = codebuf.code.Position
                            let susp,child2,scope2 = emitCode cenv localSigs codebuf env (None,fltcode)
                            commitSuspNoDest codebuf susp;
                            let handlerStart = codebuf.code.Position
                            let susp,child3,scope3 = emitCode cenv localSigs codebuf env (None,ctch)
                            commitSuspNoDest codebuf susp;
                            let handlerFinish = codebuf.code.Position
                            
                            Some (tryStart,
                                  (tryFinish - tryStart),
                                  handlerStart,
                                  (handlerFinish - handlerStart),
                                  FilterClause filterStart),
                            [(child2,scope2); (child3,scope3)])
                      
            (None,
             Node((None,[child1])::List.map (fun (a,b) -> (a,List.map fst b)) exnBranches), 
             scope1 @ List.concat ((List.collect (fun (_,b) -> List.map snd b) exnBranches)))

        | RestrictBlock _ | GroupBlock _ -> 
            // NOTE: ensure tailcalls for critical linear loop using standard continuation technique
            let rec emitCodeLinear (susp,b) cont =
                match b with 
                | RestrictBlock (_,code2) -> 
                    emitCodeLinear (susp,code2) cont
                | GroupBlock (locs,codes) -> 
                    let start = codebuf.code.Position
                    
                    // Imperative collectors for the sub-blocks
                    let newSusp = ref susp
                    let childSEH = ref []
                    let childScopes = ref []
                    // Push the results of collecting one sub-block into the reference cells
                    let collect (susp,seh,scopes) = 
                        newSusp := susp;
                        childSEH := seh :: !childSEH;
                        childScopes := scopes :: !childScopes
                    // Close the collection by generating the (susp,node,scope-node) triple
                    let close () = 
                        let fin = codebuf.code.Position
                        (!newSusp, 
                         Node([(None,(List.rev !childSEH))]), 
                         mkScopeNode cenv localSigs (start,fin,locs,List.concat (List.rev !childScopes)))

                    match codes with 
                    | [c] -> 
                        // emitCodeLinear sequence of nested blocks
                        emitCodeLinear (!newSusp,c) (fun results -> 
                            collect results;
                            cont (close()))

                    | codes -> 
                        // Multiple blocks: leave the linear sequence and process each seperately
                        codes |> List.iter (fun c -> collect (emitCode cenv localSigs codebuf env (!newSusp,c)));
                        cont(close())
                | c -> 
                    // leave the linear sequence
                    cont (emitCode cenv localSigs codebuf env (susp,c))

            // OK, process the linear sequence
            emitCodeLinear (susp,code) (fun x -> x)

        | ILBasicBlock bb ->  
            // Leaf case: one basic block
            commitSusp codebuf susp bb.Label;
            codebuf.RecordAvailBrFixup bb.Label;
            let instrs = bb.Instructions
            for i = 0 to instrs.Length - 1 do
                emitInstr cenv codebuf env instrs.[i];
            bb.Fallthrough, Tip, []
            
    and brToSusp (codebuf: CodeBuffer) dest = codebuf.RecordReqdBrFixup (i_br,Some i_br_s) dest
              
    and commitSusp codebuf susp lab = 
        match susp with 
        | Some dest when dest <> lab -> brToSusp codebuf dest
        | _ -> ()

    and commitSuspNoDest codebuf susp = 
        match susp with 
        | Some dest -> brToSusp codebuf dest
        | _ -> ()
     
    // Flatten the SEH tree 
    let rec emitExceptionHandlerTree codebuf sehTree = 
        match sehTree with 
        | Tip -> ()
        | Node clauses -> List.iter (emitExceptionHandlerTree2 codebuf) clauses

    and emitExceptionHandlerTree2 (codebuf: CodeBuffer) (x,childSEH) = 
        List.iter (emitExceptionHandlerTree codebuf) childSEH; // internal first 
        match x with 
        | None -> () 
        | Some clause -> codebuf.EmitExceptionClause clause

    let EmitTopCode cenv localSigs env nm code = 
        let codebuf = CodeBuffer.Create nm
        let finalSusp, SEHTree, origScopes = 
            emitCode cenv localSigs codebuf env (Some (uniqueEntryOfCode code),code)
        (match finalSusp with Some dest  -> brToSusp codebuf dest | _ -> ());
        emitExceptionHandlerTree codebuf SEHTree;
        let origCode = codebuf.code.Close()
        let origExnClauses = List.rev codebuf.seh
        let origReqdStringFixups = codebuf.reqdStringFixupsInMethod
        let origAvailBrFixups = codebuf.availBrFixups
        let origReqdBrFixups = codebuf.reqdBrFixups
        let origSeqPoints = codebuf.seqpoints.ToArray()

        let newCode, newReqdStringFixups, newExnClauses, newSeqPoints, newScopes = 
            applyBrFixups origCode origExnClauses origReqdStringFixups origAvailBrFixups origReqdBrFixups origSeqPoints origScopes

        let rootScope = 
            { Children= Array.ofList newScopes;
              StartOffset=0;
              EndOffset=newCode.Length;
              Locals=[| |]; }

        (newReqdStringFixups,newExnClauses, newCode, newSeqPoints, rootScope)

end

// -------------------------------------------------------------------- 
// ILMethodBody --> bytes
// -------------------------------------------------------------------- 
let GetFieldDefTypeAsBlobIdx cenv env ty = 
    let bytes = emitBytesViaBuffer (fun bb -> bb.EmitByte e_IMAGE_CEE_CS_CALLCONV_FIELD; 
                                              EmitType cenv env bb ty)
    GetBytesAsBlobIdx cenv bytes

let GenILMethodBody mname cenv env (il: ILMethodBody) =
    let localSigs = 
      if cenv.generatePdb then 
        il.Locals |> Array.ofList |> Array.map (fun l -> 
            // Write a fake entry for the local signature headed by e_IMAGE_CEE_CS_CALLCONV_FIELD. This is referenced by the PDB file
            ignore (FindOrAddRow cenv TableNames.StandAloneSig (SimpleSharedRow [| Blob (GetFieldDefTypeAsBlobIdx cenv env l.Type) |]));
            // Now write the type
            GetTypeAsBytes cenv env l.Type) 
      else [| |]

    let requiredStringFixups,seh,code,seqpoints, scopes = Codebuf.EmitTopCode cenv localSigs env mname il.Code
    let codeSize = code.Length
    let methbuf = ByteBuffer.Create (codeSize * 3)
    // Do we use the tiny format? 
    // REVIEW-HOST: port to fsharp branch
    if isNil il.Locals && il.MaxStack <= 8 && isNil seh && codeSize < 64 then
        // Use Tiny format 
        let alignedCodeSize = align 4 (codeSize + 1)
        let codePadding =  (alignedCodeSize - (codeSize + 1))
        let requiredStringFixups' = (1,requiredStringFixups)
        methbuf.EmitByte (byte codeSize <<< 2 ||| e_CorILMethod_TinyFormat);
        methbuf.EmitBytes code;
        methbuf.EmitPadding codePadding;
        (requiredStringFixups', methbuf.Close()), seqpoints, scopes
    else
        // Use Fat format 
        let flags = 
            e_CorILMethod_FatFormat |||
            (if seh <> [] then e_CorILMethod_MoreSects else 0x0uy) ||| 
            (if il.IsZeroInit then e_CorILMethod_InitLocals else 0x0uy)

        let localToken = 
            if isNil il.Locals then 0x0 else 
            getUncodedToken TableNames.StandAloneSig
              (FindOrAddRow cenv TableNames.StandAloneSig (GetLocalSigAsStandAloneSigIdx cenv env il.Locals))

        let alignedCodeSize = align 0x4 codeSize
        let codePadding =  (alignedCodeSize - codeSize)
        
        methbuf.EmitByte flags; 
        methbuf.EmitByte 0x30uy; // last four bits record size of fat header in 4 byte chunks - this is always 12 bytes = 3 four word chunks 
        methbuf.EmitUInt16 (uint16 il.MaxStack);
        methbuf.EmitInt32 codeSize;
        methbuf.EmitInt32 localToken;
        methbuf.EmitBytes code;
        methbuf.EmitPadding codePadding;

        if nonNil seh then 
            // Can we use the small exception handling table format? 
            let smallSize = (seh.Length * 12 + 4)
            let canUseSmall = 
              smallSize <= 0xFF &&
              seh |> List.forall (fun (st1,sz1,st2,sz2,_) -> 
                  st1 <= 0xFFFF && st2 <= 0xFFFF && sz1 <= 0xFF && sz2 <= 0xFF) 
            
            let kindAsInt32 k = 
              match k with 
              | FinallyClause -> e_COR_ILEXCEPTION_CLAUSE_FINALLY
              | FaultClause -> e_COR_ILEXCEPTION_CLAUSE_FAULT
              | FilterClause _ -> e_COR_ILEXCEPTION_CLAUSE_FILTER
              | TypeFilterClause _ -> e_COR_ILEXCEPTION_CLAUSE_EXCEPTION
            let kindAsExtraInt32 k = 
              match k with 
              | FinallyClause | FaultClause -> 0x0
              | FilterClause i -> i
              | TypeFilterClause uncoded -> uncoded
            
            if canUseSmall then     
                methbuf.EmitByte e_CorILMethod_Sect_EHTable;
                methbuf.EmitByte (b0 smallSize); 
                methbuf.EmitByte 0x00uy; 
                methbuf.EmitByte 0x00uy;
                seh |> List.iter (fun (st1,sz1,st2,sz2,kind) -> 
                    let k32 = kindAsInt32 kind
                    methbuf.EmitInt32AsUInt16 k32; 
                    methbuf.EmitInt32AsUInt16 st1; 
                    methbuf.EmitByte (b0 sz1); 
                    methbuf.EmitInt32AsUInt16 st2; 
                    methbuf.EmitByte (b0 sz2);
                    methbuf.EmitInt32 (kindAsExtraInt32 kind))
            else 
                let bigSize = (seh.Length * 24 + 4)
                methbuf.EmitByte (e_CorILMethod_Sect_EHTable ||| e_CorILMethod_Sect_FatFormat);
                methbuf.EmitByte (b0 bigSize);
                methbuf.EmitByte (b1 bigSize);
                methbuf.EmitByte (b2 bigSize);
                seh |> List.iter (fun (st1,sz1,st2,sz2,kind) -> 
                    let k32 = kindAsInt32 kind
                    methbuf.EmitInt32 k32;
                    methbuf.EmitInt32 st1;
                    methbuf.EmitInt32 sz1;
                    methbuf.EmitInt32 st2;
                    methbuf.EmitInt32 sz2;
                    methbuf.EmitInt32 (kindAsExtraInt32 kind))
        
        let requiredStringFixups' = (12,requiredStringFixups)

        (requiredStringFixups', methbuf.Close()), seqpoints, scopes

// -------------------------------------------------------------------- 
// ILFieldDef --> FieldDef Row
// -------------------------------------------------------------------- 

let rec GetFieldDefAsFieldDefRow cenv env (fd: ILFieldDef) = 
    let flags = 
        GetMemberAccessFlags fd.Access |||
        (if fd.IsStatic then 0x0010 else 0x0) |||
        (if fd.IsInitOnly then 0x0020 else 0x0) |||
        (if fd.IsLiteral then 0x0040 else 0x0) |||
        (if fd.NotSerialized then 0x0080 else 0x0) |||
        (if fd.IsSpecialName then 0x0200 else 0x0) |||
        (if fd.IsSpecialName then 0x0400 else 0x0) ||| 
        (if (fd.LiteralValue <> None) then 0x8000 else 0x0) |||
        (if (fd.Marshal <> None) then 0x1000 else 0x0) |||
        (if (fd.Data <> None) then 0x0100 else 0x0)
    UnsharedRow 
        [| UShort (uint16 flags); 
           String (GetStringHeapIdx cenv fd.Name);
           Blob (GetFieldDefSigAsBlobIdx cenv env fd ); |]

and GetFieldDefSigAsBlobIdx cenv env fd = GetFieldDefTypeAsBlobIdx cenv env fd.Type

and GenFieldDefPass3 cenv env fd = 
    let fidx = AddUnsharedRow cenv TableNames.Field (GetFieldDefAsFieldDefRow cenv env fd)
    GenCustomAttrsPass3 cenv (hca_FieldDef,fidx) fd.CustomAttrs;
    // Write FieldRVA table - fixups into data section done later 
    match fd.Data with 
    | None -> () 
    | Some b -> 
        let offs = cenv.data.Position
        cenv.data.EmitBytes b;
        AddUnsharedRow cenv TableNames.FieldRVA 
            (UnsharedRow [| Data (offs, false); 
                                  SimpleIndex (TableNames.Field,fidx) |]) |> ignore
    // Write FieldMarshal table 
    match fd.Marshal with 
    | None -> ()
    | Some ntyp -> 
        AddUnsharedRow cenv TableNames.FieldMarshal 
              (UnsharedRow [| HasFieldMarshal (hfm_FieldDef, fidx);
                              Blob (GetNativeTypeAsBlobIdx cenv ntyp) |]) |> ignore
    // Write Contant table 
    match fd.LiteralValue with 
    | None -> ()
    | Some i -> 
        AddUnsharedRow cenv TableNames.Constant 
              (UnsharedRow 
                  [| GetFieldInitFlags i;
                     HasConstant (hc_FieldDef, fidx);
                     Blob (GetFieldInitAsBlobIdx cenv i) |]) |> ignore
    // Write FieldLayout table 
    match fd.Offset with 
    | None -> ()
    | Some offset -> 
        AddUnsharedRow cenv TableNames.FieldLayout 
              (UnsharedRow [| ULong offset;
                                    SimpleIndex (TableNames.Field, fidx) |]) |> ignore

                
// -------------------------------------------------------------------- 
// ILGenericParameterDef --> GenericParam Row
// -------------------------------------------------------------------- 

let rec GetGenericParamAsGenericParamRow cenv _env idx owner gp = 
    let flags = 
        (match  gp.Variance with 
           | NonVariant -> 0x0000
           | CoVariant -> 0x0001
           | ContraVariant -> 0x0002) |||
        (if gp.HasReferenceTypeConstraint then 0x0004 else 0x0000) |||
        (if gp.HasNotNullableValueTypeConstraint then 0x0008 else 0x0000) |||
        (if gp.HasDefaultConstructorConstraint then 0x0010 else 0x0000)

    let mdVersionMajor,_ = metadataSchemaVersionSupportedByCLRVersion cenv.desiredMetadataVersion
    if (mdVersionMajor = 1) then 
        SimpleSharedRow 
            [| UShort (uint16 idx); 
               UShort (uint16 flags);   
               TypeOrMethodDef (fst owner, snd owner);
               String (GetStringHeapIdx cenv gp.Name);
               TypeDefOrRefOrSpec (tdor_TypeDef, 0); (* empty kind field in deprecated metadata *) |]
    else
        SimpleSharedRow 
            [| UShort (uint16 idx); 
               UShort (uint16 flags);   
               TypeOrMethodDef (fst owner, snd owner);
               String (GetStringHeapIdx cenv gp.Name) |]

and GenTypeAsGenericParamConstraintRow cenv env gpidx ty = 
    let tdorTag,tdorRow = GetTypeAsTypeDefOrRef cenv env ty
    UnsharedRow 
        [| SimpleIndex (TableNames.GenericParam, gpidx);
           TypeDefOrRefOrSpec (tdorTag,tdorRow) |]

and GenGenericParamConstraintPass4 cenv env gpidx ty =
    AddUnsharedRow cenv TableNames.GenericParamConstraint (GenTypeAsGenericParamConstraintRow cenv env gpidx ty) |> ignore

and GenGenericParamPass3 cenv env idx owner gp = 
    // shared since we look it up again below in GenGenericParamPass4
    let gpidx = AddSharedRow cenv TableNames.GenericParam (GetGenericParamAsGenericParamRow cenv env idx owner gp) 
    GenCustomAttrsPass3 cenv (hca_GenericParam,gpidx) gp.CustomAttrs;

and GenGenericParamPass4 cenv env idx owner gp = 
    let gpidx = FindOrAddRow cenv TableNames.GenericParam (GetGenericParamAsGenericParamRow cenv env idx owner gp)
    gp.Constraints |> List.iter (GenGenericParamConstraintPass4 cenv env gpidx) 

// -------------------------------------------------------------------- 
// param and return --> Param Row
// -------------------------------------------------------------------- 

let rec GetParamAsParamRow cenv _env seq param = 
    let flags = 
        (if param.IsIn then 0x0001 else 0x0000) |||
        (if param.IsOut then 0x0002 else 0x0000) |||
        (if param.IsOptional then 0x0010 else 0x0000) |||
        (if param.Default <> None then 0x1000 else 0x0000) |||
        (if param.Marshal <> None then 0x2000 else 0x0000)
    
    UnsharedRow 
        [| UShort (uint16 flags); 
           UShort (uint16 seq); 
           String (GetStringHeapIdxOption cenv param.Name) |]  

and GenParamPass3 cenv env seq param = 
    if param.IsIn=false && param.IsOut=false && param.IsOptional=false && isNone param.Default && isNone param.Name && isNone param.Marshal 
    then ()
    else    
      let pidx = AddUnsharedRow cenv TableNames.Param (GetParamAsParamRow cenv env seq param)
      GenCustomAttrsPass3 cenv (hca_ParamDef,pidx) param.CustomAttrs;
      // Write FieldRVA table - fixups into data section done later 
      match param.Marshal with 
      | None -> ()
      | Some ntyp -> 
          AddUnsharedRow cenv TableNames.FieldMarshal 
                (UnsharedRow [| HasFieldMarshal (hfm_ParamDef, pidx);
                                      Blob (GetNativeTypeAsBlobIdx cenv ntyp) |]) |> ignore

let GenReturnAsParamRow (returnv : ILReturn) = 
    let flags = (if returnv.Marshal <> None then 0x2000 else 0x0000)
    UnsharedRow 
        [| UShort (uint16 flags); 
           UShort 0us; (* sequence num. *)
           String 0 |]  

let GenReturnPass3 cenv (returnv: ILReturn) = 
    if isSome returnv.Marshal || nonNil  returnv.CustomAttrs.AsList then
        let pidx = AddUnsharedRow cenv TableNames.Param (GenReturnAsParamRow returnv)
        GenCustomAttrsPass3 cenv (hca_ParamDef,pidx) returnv.CustomAttrs;
        match returnv.Marshal with 
        | None -> ()
        | Some ntyp -> 
            AddUnsharedRow cenv TableNames.FieldMarshal   
                (UnsharedRow 
                    [| HasFieldMarshal (hfm_ParamDef, pidx);
                       Blob (GetNativeTypeAsBlobIdx cenv ntyp) |]) |> ignore

// -------------------------------------------------------------------- 
// ILMethodDef --> ILMethodDef Row
// -------------------------------------------------------------------- 

let GetMethodDefSigAsBytes cenv env (mdef: ILMethodDef) = 
    emitBytesViaBuffer (fun bb -> 
      bb.EmitByte (callconvToByte mdef.GenericParams.Length mdef.CallingConv);
      if mdef.GenericParams.Length > 0 then bb.EmitZ32 mdef.GenericParams.Length;
      bb.EmitZ32 mdef.Parameters.Length;
      EmitType cenv env bb mdef.Return.Type;
      mdef.ParameterTypes |> List.iter (EmitType cenv env bb))

let GenMethodDefSigAsBlobIdx cenv env mdef = 
    GetBytesAsBlobIdx cenv (GetMethodDefSigAsBytes cenv env mdef)

let GenMethodDefAsRow cenv env midx (md: ILMethodDef) = 
    let flags = 
        GetMemberAccessFlags md.Access |||
        (if (match md.mdKind with
              | MethodKind.Static | MethodKind.Cctor -> true
              | _ -> false) then 0x0010 else 0x0) |||
        (if (match md.mdKind with MethodKind.Virtual vinfo -> vinfo.IsFinal | _ -> false) then 0x0020 else 0x0) |||
        (if (match md.mdKind with MethodKind.Virtual _ -> true | _ -> false) then 0x0040 else 0x0) |||
        (if md.IsHideBySig then 0x0080 else 0x0) |||
        (if (match md.mdKind with MethodKind.Virtual vinfo -> vinfo.IsCheckAccessOnOverride | _ -> false) then 0x0200 else 0x0) |||
        (if (match md.mdKind with MethodKind.Virtual vinfo -> vinfo.IsNewSlot | _ -> false) then 0x0100 else 0x0) |||
        (if (match md.mdKind with MethodKind.Virtual vinfo -> vinfo.IsAbstract | _ -> false) then 0x0400 else 0x0) |||
        (if md.IsSpecialName then 0x0800 else 0x0) |||
        (if (match md.mdBody.Contents with MethodBody.PInvoke _ -> true | _ -> false) then 0x2000 else 0x0) |||
        (if md.IsUnmanagedExport then 0x0008 else 0x0) |||
        (if 
          (match md.mdKind with
          | MethodKind.Ctor | MethodKind.Cctor -> true 
          | _ -> false) then 0x1000 else 0x0) ||| // RTSpecialName 
        (if md.IsReqSecObj then 0x8000 else 0x0) |||
        (if md.HasSecurity || not md.SecurityDecls.AsList.IsEmpty then 0x4000 else 0x0)
    let implflags = 
        (match  md.mdCodeKind with 
         | MethodCodeKind.Native -> 0x0001
         | MethodCodeKind.Runtime -> 0x0003
         | MethodCodeKind.IL  -> 0x0000) |||
        (if md.IsInternalCall then 0x1000 else 0x0000) |||
        (if md.IsManaged then 0x0000 else 0x0004) |||
        (if md.IsForwardRef then 0x0010 else 0x0000) |||
        (if md.IsPreserveSig then 0x0080 else 0x0000) |||
        (if md.IsSynchronized then 0x0020 else 0x0000) |||
        (if md.IsMustRun then 0x0040 else 0x0000) |||
        (if (match md.mdBody.Contents with MethodBody.IL il -> il.NoInlining | _ -> false) then 0x0008 else 0x0000)

    if md.IsEntryPoint then 
        if cenv.entrypoint <> None then failwith "duplicate entrypoint"
        else cenv.entrypoint <- Some (true, midx);
    let codeAddr = 
      (match md.mdBody.Contents with 
      | MethodBody.IL ilmbody -> 
          let addr = cenv.nextCodeAddr
          let (code, seqpoints, rootScope) = GenILMethodBody md.Name cenv env ilmbody

          // Now record the PDB record for this method - we write this out later. 
          if cenv.generatePdb then 
            cenv.pdbinfo.Add  
              { MethToken=getUncodedToken TableNames.Method midx;
                MethName=md.Name;
                Params= [| |]; 
                RootScope = rootScope;
                Range=  
                  match ilmbody.SourceMarker with 
                  | Some m  when cenv.generatePdb -> 
                      // table indexes are 1-based, document array indexes are 0-based 
                      let doc = (cenv.documents.FindOrAddSharedEntry m.Document) - 1 

                      Some ({ Document=doc;
                              Line=m.Line;
                              Column=m.Column; },
                            { Document=doc;
                              Line=m.EndLine;
                              Column=m.EndColumn; })
                  | _ -> None
                SequencePoints=seqpoints; };
         
          cenv.AddCode code;
          addr 
      | MethodBody.Native -> 
          failwith "cannot write body of native method - Abstract IL cannot roundtrip mixed native/managed binaries";
      | _  -> 0x0000)

    UnsharedRow 
       [| ULong  codeAddr ; 
          UShort (uint16 implflags); 
          UShort (uint16 flags); 
          String (GetStringHeapIdx cenv md.Name); 
          Blob (GenMethodDefSigAsBlobIdx cenv env md); 
          SimpleIndex(TableNames.Param,cenv.GetTable(TableNames.Param).Count + 1) |]  

let GenMethodImplPass3 cenv env _tgparams tidx mimpl =
    let midxTag, midxRow = GetMethodSpecAsMethodDef cenv env (mimpl.OverrideBy,None)
    let midx2Tag, midx2Row = GetOverridesSpecAsMethodDefOrRef cenv env mimpl.Overrides
    AddUnsharedRow cenv TableNames.MethodImpl
        (UnsharedRow 
             [| SimpleIndex (TableNames.TypeDef, tidx);
                MethodDefOrRef (midxTag, midxRow);
                MethodDefOrRef (midx2Tag, midx2Row) |]) |> ignore
    
let GenMethodDefPass3 cenv env (md:ILMethodDef) = 
    let midx = GetMethodDefIdx cenv md
    let idx2 = AddUnsharedRow cenv TableNames.Method (GenMethodDefAsRow cenv env midx md)
    if midx <> idx2 then failwith "index of method def on pass 3 does not match index on pass 2";
    GenReturnPass3 cenv md.Return;  
    md.Parameters |> List.iteri (fun n param -> GenParamPass3 cenv env (n+1) param) ;
    md.CustomAttrs |> GenCustomAttrsPass3 cenv (hca_MethodDef,midx) ;
    md.SecurityDecls.AsList |> GenSecurityDeclsPass3 cenv (hds_MethodDef,midx);
    md.GenericParams |> List.iteri (fun n gp -> GenGenericParamPass3 cenv env n (tomd_MethodDef, midx) gp) ;
    match md.mdBody.Contents with 
    | MethodBody.PInvoke attr ->
        let flags = 
          begin match attr.CallingConv with 
          | PInvokeCallingConvention.None ->     0x0000
          | PInvokeCallingConvention.Cdecl ->    0x0200
          | PInvokeCallingConvention.Stdcall ->  0x0300
          | PInvokeCallingConvention.Thiscall -> 0x0400
          | PInvokeCallingConvention.Fastcall -> 0x0500
          | PInvokeCallingConvention.WinApi ->   0x0100
          end |||
          begin match attr.CharEncoding with 
          | PInvokeCharEncoding.None ->    0x0000
          | PInvokeCharEncoding.Ansi ->    0x0002
          | PInvokeCharEncoding.Unicode -> 0x0004
          | PInvokeCharEncoding.Auto ->    0x0006
          end |||
          begin match attr.CharBestFit with 
          | PInvokeCharBestFit.UseAssembly -> 0x0000
          | PInvokeCharBestFit.Enabled ->  0x0010
          | PInvokeCharBestFit.Disabled -> 0x0020
          end |||
          begin match attr.ThrowOnUnmappableChar with 
          | PInvokeThrowOnUnmappableChar.UseAssembly -> 0x0000
          | PInvokeThrowOnUnmappableChar.Enabled ->  0x1000
          | PInvokeThrowOnUnmappableChar.Disabled -> 0x2000
          end |||
          (if attr.NoMangle then 0x0001 else 0x0000) |||
          (if attr.LastError then 0x0040 else 0x0000)
        AddUnsharedRow cenv TableNames.ImplMap
            (UnsharedRow 
               [| UShort (uint16 flags); 
                  MemberForwarded (mf_MethodDef,midx);
                  String (GetStringHeapIdx cenv attr.Name); 
                  SimpleIndex (TableNames.ModuleRef, GetModuleRefAsIdx cenv attr.Where); |]) |> ignore
    | _ -> ()

let GenMethodDefPass4 cenv env  md = 
    let midx = GetMethodDefIdx cenv md
    List.iteri (fun n gp -> GenGenericParamPass4 cenv env n (tomd_MethodDef, midx) gp) md.GenericParams

let GenPropertyMethodSemanticsPass3 cenv pidx kind mref =
    let midx = try GetMethodRefAsMethodDefIdx cenv mref with MethodDefNotFound -> 1
    AddUnsharedRow cenv TableNames.MethodSemantics
        (UnsharedRow 
           [| UShort (uint16 kind);
              SimpleIndex (TableNames.Method,midx);
              HasSemantics (hs_Property, pidx) |]) |> ignore
    
let rec GetPropertySigAsBlobIdx cenv env prop = 
    GetBytesAsBlobIdx cenv (GetPropertySigAsBytes cenv env prop)

and GetPropertySigAsBytes cenv env prop = 
    emitBytesViaBuffer (fun bb -> 
        let b =  ((hasthisToByte prop.CallingConv) ||| e_IMAGE_CEE_CS_CALLCONV_PROPERTY)
        bb.EmitByte b;
        bb.EmitZ32 prop.Args.Length;
        EmitType cenv env bb prop.Type;
        prop.Args |> List.iter (EmitType cenv env bb))

and GetPropertyAsPropertyRow cenv env (prop:ILPropertyDef) = 
    let flags = 
      (if prop.IsSpecialName then 0x0200 else 0x0) ||| 
      (if prop.IsRTSpecialName then 0x0400 else 0x0) ||| 
      (if prop.Init <> None then 0x1000 else 0x0)
    UnsharedRow 
       [| UShort (uint16 flags); 
          String (GetStringHeapIdx cenv prop.Name); 
          Blob (GetPropertySigAsBlobIdx cenv env prop); |]  

/// ILPropertyDef --> Property Row + MethodSemantics entries
and GenPropertyPass3 cenv env prop = 
    let pidx = AddUnsharedRow cenv TableNames.Property (GetPropertyAsPropertyRow cenv env prop)
    prop.SetMethod |> Option.iter (GenPropertyMethodSemanticsPass3 cenv pidx 0x0001) ;
    prop.GetMethod |> Option.iter (GenPropertyMethodSemanticsPass3 cenv pidx 0x0002) ;
    // Write Constant table 
    match prop.Init with 
    | None -> ()
    | Some i -> 
        AddUnsharedRow cenv TableNames.Constant 
            (UnsharedRow 
                [| GetFieldInitFlags i;
                   HasConstant (hc_Property, pidx);
                   Blob (GetFieldInitAsBlobIdx cenv i) |]) |> ignore
    GenCustomAttrsPass3 cenv (hca_Property,pidx) prop.CustomAttrs

let rec GenEventMethodSemanticsPass3 cenv eidx kind mref =
    let addIdx = try GetMethodRefAsMethodDefIdx cenv mref with MethodDefNotFound -> 1
    AddUnsharedRow cenv TableNames.MethodSemantics
        (UnsharedRow 
            [| UShort (uint16 kind);
               SimpleIndex (TableNames.Method,addIdx);
               HasSemantics (hs_Event, eidx) |]) |> ignore

/// ILEventDef --> Event Row + MethodSemantics entries
and GenEventAsEventRow cenv env (md: ILEventDef) = 
    let flags = 
      (if md.IsSpecialName then 0x0200 else 0x0) ||| 
      (if md.IsRTSpecialName then 0x0400 else 0x0)
    let tdorTag, tdorRow = GetTypeOptionAsTypeDefOrRef cenv env md.Type
    UnsharedRow 
       [| UShort (uint16 flags); 
          String (GetStringHeapIdx cenv md.Name); 
          TypeDefOrRefOrSpec (tdorTag,tdorRow) |]

and GenEventPass3 cenv env (md: ILEventDef) = 
    let eidx = AddUnsharedRow cenv TableNames.Event (GenEventAsEventRow cenv env md)
    md.AddMethod |> GenEventMethodSemanticsPass3 cenv eidx 0x0008  
    md.RemoveMethod |> GenEventMethodSemanticsPass3 cenv eidx 0x0010 
    Option.iter (GenEventMethodSemanticsPass3 cenv eidx 0x0020) md.FireMethod  
    List.iter (GenEventMethodSemanticsPass3 cenv eidx 0x0004) md.OtherMethods;
    GenCustomAttrsPass3 cenv (hca_Event,eidx) md.CustomAttrs


// -------------------------------------------------------------------- 
// resource --> generate ...
// -------------------------------------------------------------------- 

let rec GetResourceAsManifestResourceRow cenv r = 
    let data,impl = 
      match r.Location with
      | ILResourceLocation.Local bf ->
          let b = bf()
          // Embedded managed resources must be word-aligned.  However resource format is 
          // not specified in ECMA.  Some mscorlib resources appear to be non-aligned - it seems it doesn't matter.. 
          let offset = cenv.resources.Position
          let alignedOffset =  (align 0x8 offset)
          let pad = alignedOffset - offset
          let resourceSize = b.Length
          cenv.resources.EmitPadding pad;
          cenv.resources.EmitInt32 resourceSize;
          cenv.resources.EmitBytes b;
          Data (alignedOffset,true),  (i_File, 0) 
      | ILResourceLocation.File (mref,offset) -> ULong offset, (i_File, GetModuleRefAsFileIdx cenv mref)
      | ILResourceLocation.Assembly aref -> ULong 0x0, (i_AssemblyRef, GetAssemblyRefAsIdx cenv aref)
    UnsharedRow 
       [| data; 
          ULong (match r.Access with ILResourceAccess.Public -> 0x01 | ILResourceAccess.Private -> 0x02);
          String (GetStringHeapIdx cenv r.Name);    
          Implementation (fst impl, snd impl); |]

and GenResourcePass3 cenv r = 
  let idx = AddUnsharedRow cenv TableNames.ManifestResource (GetResourceAsManifestResourceRow cenv r)
  GenCustomAttrsPass3 cenv (hca_ManifestResource,idx) r.CustomAttrs

// -------------------------------------------------------------------- 
// ILTypeDef --> generate ILFieldDef, ILMethodDef, ILPropertyDef etc. rows
// -------------------------------------------------------------------- 

let rec GenTypeDefPass3 enc cenv (td:ILTypeDef) = 
   try
      let env = envForTypeDef td
      let tidx = GetIdxForTypeDef cenv (TdKey(enc,td.Name))
      td.Properties.AsList |> List.iter (GenPropertyPass3 cenv env);
      td.Events.AsList |> List.iter (GenEventPass3 cenv env);
      td.Fields.AsList |> List.iter (GenFieldDefPass3 cenv env);
      td.Methods |> Seq.iter (GenMethodDefPass3 cenv env);
      td.MethodImpls.AsList |> List.iter (GenMethodImplPass3 cenv env  td.GenericParams.Length tidx);
    // ClassLayout entry if needed 
      match td.Layout with 
      | ILTypeDefLayout.Auto -> ()
      | ILTypeDefLayout.Sequential layout | ILTypeDefLayout.Explicit layout ->  
          if isSome layout.Pack || isSome layout.Size then 
            AddUnsharedRow cenv TableNames.ClassLayout
                (UnsharedRow 
                    [| UShort (match layout.Pack with None -> uint16 0x0 | Some p -> p);
                       ULong (match layout.Size with None -> 0x0 | Some p -> p);
                       SimpleIndex (TableNames.TypeDef, tidx) |]) |> ignore
                       
      td.SecurityDecls.AsList |> GenSecurityDeclsPass3 cenv (hds_TypeDef,tidx);
      td.CustomAttrs |> GenCustomAttrsPass3 cenv (hca_TypeDef,tidx);
      td.GenericParams |> List.iteri (fun n gp -> GenGenericParamPass3 cenv env n (tomd_TypeDef,tidx) gp) ; 
      td.NestedTypes.AsList |> GenTypeDefsPass3 (enc@[td.Name]) cenv;
   with e ->
      failwith  ("Error in pass3 for type "+td.Name+", error: "+e.Message);
      reraise()
      raise e

and GenTypeDefsPass3 enc cenv tds =
  List.iter (GenTypeDefPass3 enc cenv) tds

/// ILTypeDef --> generate generic params on ILMethodDef: ensures
/// GenericParam table is built sorted by owner.

let rec GenTypeDefPass4 enc cenv (td:ILTypeDef) = 
   try
       let env = envForTypeDef td
       let tidx = GetIdxForTypeDef cenv (TdKey(enc,td.Name))
       td.Methods |> Seq.iter (GenMethodDefPass4 cenv env) ;
       List.iteri (fun n gp -> GenGenericParamPass4 cenv env n (tomd_TypeDef,tidx) gp) td.GenericParams; 
       GenTypeDefsPass4 (enc@[td.Name]) cenv td.NestedTypes.AsList;
   with e ->
       failwith ("Error in pass4 for type "+td.Name+", error: "+e.Message);
       reraise()
       raise e

and GenTypeDefsPass4 enc cenv tds =
    List.iter (GenTypeDefPass4 enc cenv) tds

// -------------------------------------------------------------------- 
// ILExportedTypesAndForwarders --> ILExportedTypeOrForwarder table 
// -------------------------------------------------------------------- 

let rec GenNestedExportedTypePass3 cenv cidx (ce: ILNestedExportedType) = 
    let flags =  GetMemberAccessFlags ce.Access
    let nidx = 
      AddUnsharedRow cenv TableNames.ExportedType 
        (UnsharedRow 
            [| ULong flags ; 
               ULong 0x0;
               String (GetStringHeapIdx cenv ce.Name); 
               String 0; 
               Implementation (i_ExportedType, cidx) |])
    GenCustomAttrsPass3 cenv (hca_ExportedType,nidx) ce.CustomAttrs;
    GenNestedExportedTypesPass3 cenv nidx ce.Nested

and GenNestedExportedTypesPass3 cenv nidx (nce: ILNestedExportedTypes) =
    nce.AsList |> List.iter (GenNestedExportedTypePass3 cenv nidx)

and GenExportedTypePass3 cenv (ce: ILExportedTypeOrForwarder) = 
    let nselem,nelem = GetTypeNameAsElemPair cenv ce.Name
    let flags =  GetTypeAccessFlags ce.Access
    let flags = if ce.IsForwarder then 0x00200000 ||| flags else flags
    let impl = GetScopeRefAsImplementationElem cenv ce.ScopeRef
    let cidx = 
      AddUnsharedRow cenv TableNames.ExportedType 
        (UnsharedRow 
            [| ULong flags ; 
               ULong 0x0;
               nelem; 
               nselem; 
               Implementation (fst impl, snd impl); |])
    GenCustomAttrsPass3 cenv (hca_ExportedType,cidx) ce.CustomAttrs;
    GenNestedExportedTypesPass3 cenv cidx ce.Nested

and GenExportedTypesPass3 cenv (ce: ILExportedTypesAndForwarders) = 
    List.iter (GenExportedTypePass3 cenv) ce.AsList;

// -------------------------------------------------------------------- 
// manifest --> generate Assembly row
// -------------------------------------------------------------------- 

and GetManifsetAsAssemblyRow cenv m = 
    UnsharedRow 
        [|ULong m.AuxModuleHashAlgorithm;
          UShort (match m.Version with None -> 0us | Some (x,_,_,_) -> x);
          UShort (match m.Version with None -> 0us | Some (_,y,_,_) -> y);
          UShort (match m.Version with None -> 0us | Some (_,_,z,_) -> z);
          UShort (match m.Version with None -> 0us | Some (_,_,_,w) -> w);
          ULong 
            ( (match m.AssemblyLongevity with 
              | ILAssemblyLongevity.Unspecified -> 0x0000
              | ILAssemblyLongevity.Library -> 0x0002 
              | ILAssemblyLongevity.PlatformAppDomain -> 0x0004
              | ILAssemblyLongevity.PlatformProcess -> 0x0006
              | ILAssemblyLongevity.PlatformSystem -> 0x0008) |||
              (if m.Retargetable then 0xff else 0x0) |||
              (if m.JitTracking then 0x8000 else 0x0) |||
              (if m.DisableJitOptimizations then 0x4000 else 0x0) |||
              (match m.PublicKey with None -> 0x0000 | Some _ -> 0x0001) ||| 
              0x0000);
          (match m.PublicKey with None -> Blob 0 | Some x -> Blob (GetBytesAsBlobIdx cenv x));
          String (GetStringHeapIdx cenv m.Name);
          (match m.Locale with None -> String 0 | Some x -> String (GetStringHeapIdx cenv x)); |]

and GenManifestPass3 cenv m = 
    let aidx = AddUnsharedRow cenv TableNames.Assembly (GetManifsetAsAssemblyRow cenv m)
    GenSecurityDeclsPass3 cenv (hds_Assembly,aidx) m.SecurityDecls.AsList;
    GenCustomAttrsPass3 cenv (hca_Assembly,aidx) m.CustomAttrs;
    GenExportedTypesPass3 cenv m.ExportedTypes;
    // Record the entrypoint decl if needed. 
    match m.EntrypointElsewhere with
    | Some mref -> 
        if cenv.entrypoint <> None then failwith "duplicate entrypoint"
        else cenv.entrypoint <- Some (false, GetModuleRefAsIdx cenv mref);
    | None -> ()

and newGuid (modul: ILModuleDef) = 
    let n = absilWriteGetTimeStamp ()
    let m = hash n
    let m2 = hash modul.Name
    [| b0 m; b1 m; b2 m; b3 m; b0 m2; b1 m2; b2 m2; b3 m2; 0xa7uy; 0x45uy; 0x03uy; 0x83uy; b0 n; b1 n; b2 n; b3 n |]

and GetModuleAsRow cenv (modul: ILModuleDef) = 
    // Store the generated MVID in the environment (needed for generating debug information)
    let modulGuid = newGuid modul
    cenv.moduleGuid <- modulGuid
    UnsharedRow 
        [| UShort (uint16 0x0); 
           String (GetStringHeapIdx cenv modul.Name); 
           Guid (GetGuidIdx cenv modulGuid); 
           Guid 0; 
           Guid 0 |]


let rowElemCompare e1 e2 = 
    match e1,e2 with 
    | SimpleIndex (tab1,n1), SimpleIndex(tab2,n2) -> 
        let c1 = compare n1 n2
        if c1 <> 0 then c1 else compare tab1.Index tab2.Index
    | TypeDefOrRefOrSpec(TypeDefOrRefOrSpecTag tag1,n1),
        TypeDefOrRefOrSpec(TypeDefOrRefOrSpecTag tag2,n2)
    | TypeOrMethodDef(TypeOrMethodDefTag tag1,n1),
        TypeOrMethodDef(TypeOrMethodDefTag tag2,n2)
    | HasConstant (HasConstantTag tag1,n1),
        HasConstant (HasConstantTag tag2,n2) 
    | HasCustomAttribute (HasCustomAttributeTag tag1,n1),
        HasCustomAttribute (HasCustomAttributeTag tag2,n2) 
    | HasFieldMarshal (HasFieldMarshalTag tag1,n1),
        HasFieldMarshal (HasFieldMarshalTag tag2,n2) 
    | HasDeclSecurity (HasDeclSecurityTag tag1,n1),
        HasDeclSecurity (HasDeclSecurityTag tag2,n2)
    | MemberRefParent (MemberRefParentTag tag1,n1),
        MemberRefParent (MemberRefParentTag tag2,n2) 
    | HasSemantics (HasSemanticsTag tag1,n1),
        HasSemantics (HasSemanticsTag tag2,n2) 
    | MethodDefOrRef (MethodDefOrRefTag tag1,n1),
        MethodDefOrRef (MethodDefOrRefTag tag2,n2) 
    | MemberForwarded (MemberForwardedTag tag1,n1),
        MemberForwarded (MemberForwardedTag tag2,n2)
    | Implementation (ImplementationTag tag1,n1),
        Implementation (ImplementationTag tag2,n2)
    | CustomAttributeType (CustomAttributeTypeTag tag1,n1),
        CustomAttributeType (CustomAttributeTypeTag tag2,n2) 
    | (ResolutionScope (ResolutionScopeTag tag1,n1),
        ResolutionScope (ResolutionScopeTag tag2,n2)) -> 
          let c1 = compare n1 n2
          if c1 <> 0 then c1 else compare tag1 tag2  
    | ULong _,ULong _ 
    | UShort _, UShort _ 
    | Guid _,Guid _ 
    | Blob _, Blob _
    | String _, String _
    | Data _,Data _ -> failwith "should not have to sort tables on this element"
    | _ -> failwith "sorting on Column where two rows have different kinds of element in this Column" 

let SortTableRows tab rows = 
    if List.memAssoc tab sortedTableInfo then
        let rows = rows |> List.map (fun (row:IGenericRow) -> row.GetGenericRow())
        let col = List.assoc tab sortedTableInfo
        rows 
           |> List.sortWith (fun r1 r2 -> rowElemCompare r1.[col] r2.[col]) 
           |> List.map (fun arr -> (SimpleSharedRow arr) :> IGenericRow)
    else 
        rows

let GenModule (cenv : cenv) (modul: ILModuleDef) = 
    let midx = AddUnsharedRow cenv TableNames.Module (GetModuleAsRow cenv modul)
    List.iter (GenResourcePass3 cenv) modul.Resources.AsList; 
    let tds = destTypeDefsWithGlobalFunctionsFirst cenv.ilg modul.TypeDefs
    reportTime cenv.showTimes "Module Generation Preparation";
    GenTypeDefsPass1 [] cenv tds;
    reportTime cenv.showTimes "Module Generation Pass 1";
    GenTypeDefsPass2 0 [] cenv tds;
    reportTime cenv.showTimes "Module Generation Pass 2";
    (match modul.Manifest with None -> () | Some m -> GenManifestPass3 cenv m);
    GenTypeDefsPass3 [] cenv tds;
    reportTime cenv.showTimes "Module Generation Pass 3";
    GenCustomAttrsPass3 cenv (hca_Module,midx) modul.CustomAttrs;
    // GenericParam is the only sorted table indexed by Columns in other tables (GenericParamConstraint). 
    // Hence we need to sort it before we emit any entries in GenericParamConstraint. 
    // Note this mutates the rows in a table.  'SetRowsOfTable' clears 
    // the key --> index map since it is no longer valid 
    cenv.GetTable(TableNames.GenericParam).SetRowsOfTable (SortTableRows TableNames.GenericParam (cenv.GetTable(TableNames.GenericParam).Entries));
    GenTypeDefsPass4 [] cenv tds;
    reportTime cenv.showTimes "Module Generation Pass 4"

let generateIL requiredDataFixups (desiredMetadataVersion,generatePdb,mscorlib,emitTailcalls,showTimes)  (m : ILModuleDef) noDebugData cilStartAddress =
    let isDll = m.IsDLL

    let cenv = 
        { mscorlib=mscorlib;
          emitTailcalls=emitTailcalls;
          showTimes=showTimes;
          ilg = mkILGlobals mscorlib None noDebugData; // assumes mscorlib is Scope_assembly _ ILScopeRef 
          desiredMetadataVersion=desiredMetadataVersion;
          requiredDataFixups= requiredDataFixups;
          requiredStringFixups = [];
          codeChunks=ByteBuffer.Create 40000;
          nextCodeAddr = cilStartAddress;
          data = ByteBuffer.Create 200;
          resources = ByteBuffer.Create 200;
          tables= Array.init 64 (fun i -> MetadataTable<_>.New ("row table "+string i,System.Collections.Generic.EqualityComparer.Default));
          AssemblyRefs = MetadataTable<_>.New("ILAssemblyRef",System.Collections.Generic.EqualityComparer.Default);
          documents=MetadataTable<_>.New("pdbdocs",System.Collections.Generic.EqualityComparer.Default);
          trefCache=new Dictionary<_,_>(100);
          pdbinfo= new ResizeArray<_>(200);
          moduleGuid= Array.zeroCreate 16;
          fieldDefs= MetadataTable<_>.New("field defs",System.Collections.Generic.EqualityComparer.Default);
          methodDefIdxsByKey = MetadataTable<_>.New("method defs",System.Collections.Generic.EqualityComparer.Default);
          // This uses reference identity on ILMethodDef objects
          methodDefIdxs = new Dictionary<_,_>(100, HashIdentity.Reference);
          propertyDefs = MetadataTable<_>.New("property defs",System.Collections.Generic.EqualityComparer.Default);
          eventDefs = MetadataTable<_>.New("event defs",System.Collections.Generic.EqualityComparer.Default);
          typeDefs = MetadataTable<_>.New("type defs",System.Collections.Generic.EqualityComparer.Default);
          entrypoint=None;
          generatePdb=generatePdb;
          // These must use structural comparison since they are keyed by arrays
          guids=MetadataTable<_>.New("guids",HashIdentity.Structural);
          blobs= MetadataTable<_>.New("blobs",HashIdentity.Structural);
          strings= MetadataTable<_>.New("strings",System.Collections.Generic.EqualityComparer.Default); 
          userStrings= MetadataTable<_>.New("user strings",System.Collections.Generic.EqualityComparer.Default); }

    // Now the main compilation step 
    GenModule cenv  m;

    // Fetch out some of the results  
    let entryPointToken = 
        match cenv.entrypoint with 
        | Some (epHere,tok) -> 
            getUncodedToken (if epHere then TableNames.Method else TableNames.File) tok 
        | None -> 
            if not isDll then dprintn "warning: no entrypoint specified in executable binary";
            0x0

    let pdbData = 
        { EntryPoint= (if isDll then None else Some entryPointToken);
          ModuleID = cenv.moduleGuid;
          Documents = Array.ofList cenv.documents.Entries;
          Methods= cenv.pdbinfo.ToArray() }

    let idxForNextedTypeDef (tds:ILTypeDef list, td:ILTypeDef) =
        let enc = tds |> List.map (fun td -> td.Name)
        GetIdxForTypeDef cenv (TdKey(enc, td.Name))

    let strings =     Array.map Bytes.stringAsUtf8NullTerminated (Array.ofList cenv.strings.Entries)
    let userStrings = Array.ofList cenv.userStrings.Entries |> Array.map System.Text.Encoding.Unicode.GetBytes
    let blobs =       Array.ofList cenv.blobs.Entries
    let guids =       Array.ofList cenv.guids.Entries
    let tables =      cenv.tables |> Array.map (fun t -> t.Entries)
    let code =        cenv.GetCode() 
    // turn idx tbls into token maps 
    let mappings =
     { TypeDefTokenMap = (fun t ->
        getUncodedToken TableNames.TypeDef (idxForNextedTypeDef t));
       FieldDefTokenMap = (fun t fd ->
        let tidx = idxForNextedTypeDef t
        getUncodedToken TableNames.Field (GetFieldDefAsFieldDefIdx cenv tidx fd));
       MethodDefTokenMap = (fun t md ->
        let tidx = idxForNextedTypeDef t
        getUncodedToken TableNames.Method (FindMethodDefIdx cenv (GetKeyForMethodDef tidx md)));
       PropertyTokenMap = (fun t pd ->
        let tidx = idxForNextedTypeDef t
        getUncodedToken TableNames.Property (cenv.propertyDefs.GetTableEntry (GetKeyForPropertyDef tidx pd)));
       EventTokenMap = (fun t ed ->
        let tidx = idxForNextedTypeDef t
        getUncodedToken TableNames.Event (cenv.eventDefs.GetTableEntry (EventKey (tidx, ed.Name)))) }
    reportTime cenv.showTimes "Finalize Module Generation Results";
    // New return the results 
    let data = cenv.data.Close()
    let resources = cenv.resources.Close()
    (strings,userStrings,blobs,guids,tables,entryPointToken,code,cenv.requiredStringFixups,data,resources,pdbData,mappings)


//=====================================================================
// TABLES+BLOBS --> PHYSICAL METADATA+BLOBS
//=====================================================================

type BinaryChunk = 
    { size: int32; 
      addr: int32 }

let chunk sz next = ({addr=next; size=sz},next + sz) 
let nochunk next = ({addr= 0x0;size= 0x0; } ,next)

let count f arr = 
    Array.fold (fun x y -> x + f y) 0x0 arr 

module FileSystemUtilites = 
    open System.Reflection
#if SILVERLIGHT
    let progress = false
    let setExecutablePermission _filename = ()
#else
    let progress = try System.Environment.GetEnvironmentVariable("FSharp_DebugSetFilePermissions") <> null with _ -> false
    let setExecutablePermission filename =

      if runningOnMono then 
        try 
            let monoPosix = Assembly.Load("Mono.Posix, Version=2.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756")
            if progress then eprintf "loading type Mono.Unix.UnixFileInfo...\n";
            let monoUnixFileInfo = monoPosix.GetType("Mono.Unix.UnixFileSystemInfo") 
            let fileEntry = monoUnixFileInfo.InvokeMember("GetFileSystemEntry", (BindingFlags.InvokeMethod ||| BindingFlags.Static ||| BindingFlags.Public), null, null, [| box filename |],System.Globalization.CultureInfo.InvariantCulture)
            let prevPermissions = monoUnixFileInfo.InvokeMember("get_FileAccessPermissions", (BindingFlags.InvokeMethod ||| BindingFlags.Instance ||| BindingFlags.Public), null, fileEntry, [| |],System.Globalization.CultureInfo.InvariantCulture) |> unbox<int>
            // Add 0x000001ED (UserReadWriteExecute, GroupReadExecute, OtherReadExecute) to the access permissions on Unix
            monoUnixFileInfo.InvokeMember("set_FileAccessPermissions", (BindingFlags.InvokeMethod ||| BindingFlags.Instance ||| BindingFlags.Public), null, fileEntry, [| box (prevPermissions ||| 0x000001ED) |],System.Globalization.CultureInfo.InvariantCulture) |> ignore
        with e -> 
            if progress then eprintf "failure: %s...\n" (e.ToString());
            // Fail silently
#endif
        
let writeILMetadataAndCode (generatePdb,desiredMetadataVersion,mscorlib,emitTailcalls,showTimes) modul noDebugData cilStartAddress = 

    // When we know the real RVAs of the data section we fixup the references for the FieldRVA table. 
    // These references are stored as offsets into the metadata we return from this function 
    let requiredDataFixups = ref []

    let next = cilStartAddress

    let strings,userStrings,blobs,guids,tables,entryPointToken,code,requiredStringFixups,data,resources,pdbData,mappings = 
      generateIL requiredDataFixups (desiredMetadataVersion,generatePdb,mscorlib,emitTailcalls,showTimes) modul noDebugData cilStartAddress

    reportTime showTimes "Generated Tables and Code";
    let tableSize (tab: TableName) = tables.[tab.Index].Length

   // Now place the code 
    let codeSize = code.Length
    let alignedCodeSize = align 0x4 codeSize
    let codep,next = chunk codeSize next
    let codePadding = Array.create (alignedCodeSize - codeSize) 0x0uy
    let _codePaddingChunk,next = chunk codePadding.Length next

   // Now layout the chunks of metadata and IL 
    let metadataHeaderStartChunk,_next = chunk 0x10 next

    let numStreams = 0x05

    let (mdtableVersionMajor, mdtableVersionMinor) = metadataSchemaVersionSupportedByCLRVersion desiredMetadataVersion

    let version = 
      let (a,b,c,_) = desiredMetadataVersion
      System.Text.Encoding.UTF8.GetBytes (sprintf "v%d.%d.%d" a b c)


    let paddedVersionLength = align 0x4 (Array.length version)

    // Most addresses after this point are measured from the MD root 
    // Switch to md-rooted addresses 
    let next = metadataHeaderStartChunk.size
    let _metadataHeaderVersionChunk,next   = chunk paddedVersionLength next
    let _metadataHeaderEndChunk,next       = chunk 0x04 next
    let _tablesStreamHeaderChunk,next      = chunk (0x08 + (align 4 ("#~".Length + 0x01))) next
    let _stringsStreamHeaderChunk,next     = chunk (0x08 + (align 4 ("#Strings".Length + 0x01))) next
    let _userStringsStreamHeaderChunk,next = chunk (0x08 + (align 4 ("#US".Length + 0x01))) next
    let _guidsStreamHeaderChunk,next       = chunk (0x08 + (align 4 ("#GUID".Length + 0x01))) next
    let _blobsStreamHeaderChunk,next       = chunk (0x08 + (align 4 ("#Blob".Length + 0x01))) next

    let tablesStreamStart = next

    let stringsStreamUnpaddedSize = count (fun (s:byte[]) -> s.Length) strings + 1
    let stringsStreamPaddedSize = align 4 stringsStreamUnpaddedSize
    
    let userStringsStreamUnpaddedSize = count (fun (s:byte[]) -> let n = s.Length + 1 in n + ByteBuffer.Z32Size n) userStrings + 1
    let userStringsStreamPaddedSize = align 4 userStringsStreamUnpaddedSize
    
    let guidsStreamUnpaddedSize = (Array.length guids) * 0x10
    let guidsStreamPaddedSize = align 4 guidsStreamUnpaddedSize
    
    let blobsStreamUnpaddedSize = count (fun (blob:byte[]) -> let n = blob.Length in n + ByteBuffer.Z32Size n) blobs + 1
    let blobsStreamPaddedSize = align 4 blobsStreamUnpaddedSize

    let guidsBig = guidsStreamPaddedSize >= 0x10000
    let stringsBig = stringsStreamPaddedSize >= 0x10000
    let blobsBig = blobsStreamPaddedSize >= 0x10000

    // 64bit bitvector indicating which tables are in the metadata. 
    let (valid1,valid2),_ = 
      Array.fold 
        (fun ((valid1,valid2) as valid,n) rows -> 
          let valid = 
            if isNil rows then valid else
            ( (if n < 32 then  valid1 ||| (1 <<< n     ) else valid1),
              (if n >= 32 then valid2 ||| (1 <<< (n-32)) else valid2) )
          (valid,n+1))
        ((0,0), 0)
        tables

    // 64bit bitvector indicating which tables are sorted. 
    let sorted1 = 0x3301fa00
    let sorted2 = 
      // If there are any generic parameters in the binary we're emitting then mark that 
      // table as sorted, otherwise don't.  This maximizes the number of assemblies we emit 
      // which have an ECMA-v.1. compliant set of sorted tables. 
      (if tableSize (TableNames.GenericParam) > 0 then 0x00000400 else 0x00000000) ||| 
      (if tableSize (TableNames.GenericParamConstraint) > 0 then 0x00001000 else 0x00000000) ||| 
      0x00000200
    
    reportTime showTimes "Layout Header of Tables";

    let guidAddress n =   (if n = 0 then 0 else (n - 1) * 0x10 + 0x01)

    let stringAddressTable = 
        let tab = Array.create (strings.Length + 1) 0
        let pos = ref 1
        for i = 1 to strings.Length do
            tab.[i] <- !pos;
            let s = strings.[i - 1]
            pos := !pos + s.Length
        tab

    let stringAddress n = 
        if n >= Array.length stringAddressTable then failwith ("string index "+string n+" out of range");
        stringAddressTable.[n]
    
    let userStringAddressTable = 
        let tab = Array.create (Array.length userStrings + 1) 0
        let pos = ref 1
        for i = 1 to Array.length userStrings do
            tab.[i] <- !pos;
            let s = userStrings.[i - 1]
            let n = s.Length + 1
            pos := !pos + n + ByteBuffer.Z32Size n
        tab

    let userStringAddress n = 
        if n >= Array.length userStringAddressTable then failwith "userString index out of range";
        userStringAddressTable.[n]
    
    let blobAddressTable = 
        let tab = Array.create (blobs.Length + 1) 0
        let pos = ref 1
        for i = 1 to blobs.Length do
            tab.[i] <- !pos;
            let blob = blobs.[i - 1]
            pos := !pos + blob.Length + ByteBuffer.Z32Size blob.Length
        tab

    let blobAddress n = 
        if n >= blobAddressTable.Length then failwith "blob index out of range";
        blobAddressTable.[n]
    
    reportTime showTimes "Build String/Blob Address Tables";

    let sortedTables = 
      Array.init 64 (fun i -> tables.[i] |>  SortTableRows (TableName.FromIndex i) |> Array.ofList)
      
    reportTime showTimes "Sort Tables";

    let codedTables = 
          
        let bignessTable = Array.map (fun rows -> Array.length rows >= 0x10000) sortedTables
        let bigness (tab:TableName) = bignessTable.[tab.Index]
        
        let codedBigness nbits tab =
          (tableSize tab) >= (0x10000 >>> nbits)
        
        let tdorBigness = 
            codedBigness 2 TableNames.TypeDef || 
            codedBigness 2 TableNames.TypeRef || 
            codedBigness 2 TableNames.TypeSpec
        
        let tomdBigness = 
            codedBigness 1 TableNames.TypeDef || 
            codedBigness 1 TableNames.Method
        
        let hcBigness = 
            codedBigness 2 TableNames.Field ||
            codedBigness 2 TableNames.Param ||
            codedBigness 2 TableNames.Property
        
        let hcaBigness = 
            codedBigness 5 TableNames.Method ||
            codedBigness 5 TableNames.Field ||
            codedBigness 5 TableNames.TypeRef  ||
            codedBigness 5 TableNames.TypeDef ||
            codedBigness 5 TableNames.Param ||
            codedBigness 5 TableNames.InterfaceImpl ||
            codedBigness 5 TableNames.MemberRef ||
            codedBigness 5 TableNames.Module ||
            codedBigness 5 TableNames.Permission ||
            codedBigness 5 TableNames.Property ||
            codedBigness 5 TableNames.Event ||
            codedBigness 5 TableNames.StandAloneSig ||
            codedBigness 5 TableNames.ModuleRef ||
            codedBigness 5 TableNames.TypeSpec ||
            codedBigness 5 TableNames.Assembly ||
            codedBigness 5 TableNames.AssemblyRef ||
            codedBigness 5 TableNames.File ||
            codedBigness 5 TableNames.ExportedType ||
            codedBigness 5 TableNames.ManifestResource  ||
            codedBigness 5 TableNames.GenericParam ||
            codedBigness 5 TableNames.GenericParamConstraint ||
            codedBigness 5 TableNames.MethodSpec

        
        let hfmBigness = 
            codedBigness 1 TableNames.Field || 
            codedBigness 1 TableNames.Param
        
        let hdsBigness = 
            codedBigness 2 TableNames.TypeDef || 
            codedBigness 2 TableNames.Method ||
            codedBigness 2 TableNames.Assembly
        
        let mrpBigness = 
            codedBigness 3 TableNames.TypeRef ||
            codedBigness 3 TableNames.ModuleRef ||
            codedBigness 3 TableNames.Method ||
            codedBigness 3 TableNames.TypeSpec
        
        let hsBigness = 
            codedBigness 1 TableNames.Event || 
            codedBigness 1 TableNames.Property 
        
        let mdorBigness =
            codedBigness 1 TableNames.Method ||    
            codedBigness 1 TableNames.MemberRef 
        
        let mfBigness =
            codedBigness 1 TableNames.Field ||
            codedBigness 1 TableNames.Method 
        
        let iBigness =
            codedBigness 2 TableNames.File || 
            codedBigness 2 TableNames.AssemblyRef ||    
            codedBigness 2 TableNames.ExportedType 
        
        let catBigness =  
            codedBigness 3 TableNames.Method ||    
            codedBigness 3 TableNames.MemberRef 
        
        let rsBigness = 
            codedBigness 2 TableNames.Module ||    
            codedBigness 2 TableNames.ModuleRef || 
            codedBigness 2 TableNames.AssemblyRef  ||
            codedBigness 2 TableNames.TypeRef
        
        let tablesBuf =  ByteBuffer.Create 20000
        
        // Now the coded tables themselves  - first the schemata header 
        tablesBuf.EmitIntsAsBytes    
            [| 0x00; 0x00; 0x00; 0x00; 
               mdtableVersionMajor; // major version of table schemata 
               mdtableVersionMinor; // minor version of table schemata 
               
                ((if stringsBig then 0x01 else 0x00) |||  // bit vector for heap size 
                 (if guidsBig then 0x02 else 0x00) |||  
                 (if blobsBig then 0x04 else 0x00));
               0x01; (* reserved, always 1 *) |];
 
        tablesBuf.EmitInt32 valid1;
        tablesBuf.EmitInt32 valid2;
        tablesBuf.EmitInt32 sorted1;
        tablesBuf.EmitInt32 sorted2;
        
        // Numbers of rows in various tables 
        for rows in sortedTables do 
            if rows.Length <> 0 then 
                tablesBuf.EmitInt32 rows.Length 
        
        
        reportTime showTimes "Write Header of tablebuf";

      // The tables themselves 
        for rows in sortedTables do
            for row in rows do 
                let row = row.GetGenericRow()
                for x in row do 
                    // Emit the coded token for the array element 
                    match x with 
                    | ULong n -> tablesBuf.EmitInt32 n
                    | UShort n -> tablesBuf.EmitUInt16 n
                    | Guid n -> tablesBuf.EmitZUntaggedIndex guidsBig (guidAddress n)
                    | Blob n -> tablesBuf.EmitZUntaggedIndex blobsBig  (blobAddress n)
                    | Data (offset,kind) -> recordRequiredDataFixup requiredDataFixups tablesBuf (tablesStreamStart + tablesBuf.Position) (offset, kind)
                    | String n -> tablesBuf.EmitZUntaggedIndex stringsBig (stringAddress n)
                    | SimpleIndex (tab,n) -> tablesBuf.EmitZUntaggedIndex (bigness tab) n
                    | TypeDefOrRefOrSpec(TypeDefOrRefOrSpecTag tag,n) ->  
                        tablesBuf.EmitZTaggedIndex tag 2 tdorBigness n
                    | TypeOrMethodDef(TypeOrMethodDefTag tag,n) ->  
                        tablesBuf.EmitZTaggedIndex tag 1 tomdBigness n
                    | HasConstant (HasConstantTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 2 hcBigness n
                    | HasCustomAttribute (HasCustomAttributeTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 5 hcaBigness n
                    | HasFieldMarshal (HasFieldMarshalTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 1   hfmBigness n
                    | HasDeclSecurity (HasDeclSecurityTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 2  hdsBigness n
                    | MemberRefParent (MemberRefParentTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 3  mrpBigness n 
                    | HasSemantics (HasSemanticsTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 1  hsBigness n 
                    | MethodDefOrRef (MethodDefOrRefTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 1  mdorBigness n
                    | MemberForwarded (MemberForwardedTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 1  mfBigness n
                    | Implementation (ImplementationTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 2  iBigness n
                    | CustomAttributeType (CustomAttributeTypeTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag  3  catBigness n
                    | ResolutionScope (ResolutionScopeTag tag,n) -> 
                        tablesBuf.EmitZTaggedIndex tag 2  rsBigness n
        tablesBuf.Close()

    reportTime showTimes "Write Tables to tablebuf";

    let tablesStreamUnpaddedSize = codedTables.Length
    // QUERY: extra 4 empty bytes in array.exe - why? Include some extra padding after 
    // the tables just in case there is a mistake in the ECMA spec. 
    let tablesStreamPaddedSize = align 4 (tablesStreamUnpaddedSize + 4)
    let tablesChunk,next = chunk tablesStreamPaddedSize next
    let tablesStreamPadding = tablesChunk.size - tablesStreamUnpaddedSize

    let stringsChunk,next = chunk stringsStreamPaddedSize next
    let stringsStreamPadding = stringsChunk.size - stringsStreamUnpaddedSize
    let userStringsChunk,next = chunk userStringsStreamPaddedSize next
    let userStringsStreamPadding = userStringsChunk.size - userStringsStreamUnpaddedSize
    let guidsChunk,next = chunk (0x10 * guids.Length) next
    let blobsChunk,_next = chunk blobsStreamPaddedSize next
    let blobsStreamPadding = blobsChunk.size - blobsStreamUnpaddedSize
    
    reportTime showTimes "Layout Metadata";

    let metadata = 
      let mdbuf =  ByteBuffer.Create 500000 
      mdbuf.EmitIntsAsBytes 
        [| 0x42; 0x53; 0x4a; 0x42; // Magic signature 
           0x01; 0x00; // Major version 
           0x01; 0x00; // Minor version 
        |];
      mdbuf.EmitInt32 0x0; // Reservered 

      mdbuf.EmitInt32 paddedVersionLength;
      mdbuf.EmitBytes version;
      for i = 1 to (paddedVersionLength - Array.length version) do 
          mdbuf.EmitIntAsByte 0x00;

      mdbuf.EmitBytes 
        [| 0x00uy; 0x00uy; // flags, reserved 
          b0 numStreams; b1 numStreams; |];
      mdbuf.EmitInt32 tablesChunk.addr;
      mdbuf.EmitInt32 tablesChunk.size;
      mdbuf.EmitIntsAsBytes [| 0x23; 0x7e; 0x00; 0x00; (* #~00 *)|];
      mdbuf.EmitInt32 stringsChunk.addr;
      mdbuf.EmitInt32 stringsChunk.size;
      mdbuf.EmitIntsAsBytes  [| 0x23; 0x53; 0x74; 0x72; 0x69; 0x6e; 0x67; 0x73; 0x00; 0x00; 0x00; 0x00 (* "#Strings0000" *)|];
      mdbuf.EmitInt32 userStringsChunk.addr;
      mdbuf.EmitInt32 userStringsChunk.size;
      mdbuf.EmitIntsAsBytes [| 0x23; 0x55; 0x53; 0x00; (* #US0*) |];
      mdbuf.EmitInt32 guidsChunk.addr;
      mdbuf.EmitInt32 guidsChunk.size;
      mdbuf.EmitIntsAsBytes [| 0x23; 0x47; 0x55; 0x49; 0x44; 0x00; 0x00; 0x00; (* #GUID000 *)|];
      mdbuf.EmitInt32 blobsChunk.addr;
      mdbuf.EmitInt32 blobsChunk.size;
      mdbuf.EmitIntsAsBytes [| 0x23; 0x42; 0x6c; 0x6f; 0x62; 0x00; 0x00; 0x00; (* #Blob000 *)|];
      
      reportTime showTimes "Write Metadata Header";
     // Now the coded tables themselves 
      mdbuf.EmitBytes codedTables;    
      for i = 1 to tablesStreamPadding do 
          mdbuf.EmitIntAsByte 0x00;
      reportTime showTimes "Write Metadata Tables";

     // The string stream 
      mdbuf.EmitByte 0x00uy;
      for s in strings do
          mdbuf.EmitBytes s;
      for i = 1 to stringsStreamPadding do 
          mdbuf.EmitIntAsByte 0x00;
      reportTime showTimes "Write Metadata Strings";
     // The user string stream 
      mdbuf.EmitByte  0x00uy;
      for s in userStrings do
          mdbuf.EmitZ32 (s.Length + 1);
          mdbuf.EmitBytes s;
          mdbuf.EmitIntAsByte (markerForUnicodeBytes s)
      for i = 1 to userStringsStreamPadding do 
          mdbuf.EmitIntAsByte 0x00;

      reportTime showTimes "Write Metadata User Strings";
    // The GUID stream 
      Array.iter mdbuf.EmitBytes guids;
      
    // The blob stream 
      mdbuf.EmitByte 0x00uy;
      for s in blobs do 
          mdbuf.EmitZ32 s.Length;
          mdbuf.EmitBytes s
      for i = 1 to blobsStreamPadding do 
          mdbuf.EmitIntAsByte 0x00;
      reportTime showTimes "Write Blob Stream";
     // Done - close the buffer and return the result. 
      mdbuf.Close()
    

   // Now we know the user string tables etc. we can fixup the 
   // uses of strings in the code 
    for (codeStartAddr, l) in requiredStringFixups do
        for (codeOffset,userStringIndex) in l do 
              if codeStartAddr < codep.addr || codeStartAddr >= codep.addr + codep.size  then failwith "strings-in-code fixup: a group of fixups is located outside the code array";
              let locInCode =  ((codeStartAddr + codeOffset) - codep.addr)
              checkFixup32 code locInCode 0xdeadbeef;
              let token = getUncodedToken TableNames.UserStrings (userStringAddress userStringIndex)
              if (Bytes.get code (locInCode-1) <> i_ldstr) then failwith "strings-in-code fixup: not at ldstr instruction!";
              applyFixup32 code locInCode token
    reportTime showTimes "Fixup Metadata";

    entryPointToken,code, codePadding,metadata,data,resources,!requiredDataFixups,pdbData,mappings



//---------------------------------------------------------------------
// PHYSICAL METADATA+BLOBS --> PHYSICAL PE FORMAT
//---------------------------------------------------------------------

// THIS LAYS OUT A 2-SECTION .NET PE BINARY 
// SECTIONS 
// TEXT: physical 0x0200 --> RVA 0x00020000
//         e.g. raw size 0x9600, 
//         e.g. virt size 0x9584
// RELOC: physical 0x9800 --> RVA 0x0000c000
//    i.e. physbase --> rvabase
//    where physbase = textbase + text raw size
//         phsrva = roundup(0x2000, 0x0002000 + text virt size)

let msdosHeader : byte[] = 
     [| 0x4duy; 0x5auy; 0x90uy; 0x00uy; 0x03uy; 0x00uy; 0x00uy; 0x00uy
        0x04uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0x00uy; 0x00uy
        0xb8uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
        0x40uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy
        0x0euy; 0x1fuy; 0xbauy; 0x0euy; 0x00uy; 0xb4uy; 0x09uy; 0xcduy
        0x21uy; 0xb8uy; 0x01uy; 0x4cuy; 0xcduy; 0x21uy; 0x54uy; 0x68uy
        0x69uy; 0x73uy; 0x20uy; 0x70uy; 0x72uy; 0x6fuy; 0x67uy; 0x72uy
        0x61uy; 0x6duy; 0x20uy; 0x63uy; 0x61uy; 0x6euy; 0x6euy; 0x6fuy
        0x74uy; 0x20uy; 0x62uy; 0x65uy; 0x20uy; 0x72uy; 0x75uy; 0x6euy
        0x20uy; 0x69uy; 0x6euy; 0x20uy; 0x44uy; 0x4fuy; 0x53uy; 0x20uy
        0x6duy; 0x6fuy; 0x64uy; 0x65uy; 0x2euy; 0x0duy; 0x0duy; 0x0auy
        0x24uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]

let writeInt64 (os: BinaryWriter) x =
    os.Write (dw0 x);
    os.Write (dw1 x);
    os.Write (dw2 x);
    os.Write (dw3 x);
    os.Write (dw4 x);
    os.Write (dw5 x);
    os.Write (dw6 x);
    os.Write (dw7 x)

let writeInt32 (os: BinaryWriter) x = 
    os.Write  (byte (b0 x));
    os.Write  (byte (b1 x));
    os.Write  (byte (b2 x));
    os.Write  (byte (b3 x))  

let writeInt32AsUInt16 (os: BinaryWriter) x = 
    os.Write  (byte (b0 x));
    os.Write  (byte (b1 x))
      
let writeDirectory os dict =
    writeInt32 os (if dict.size = 0x0 then 0x0 else dict.addr);
    writeInt32 os dict.size

let writeBytes (os: BinaryWriter) (chunk:byte[]) = os.Write(chunk,0,chunk.Length)  

let writeBinaryAndReportMappings (outfile, mscorlib, pdbfile: string option, signer: ILStrongNameSigner option, fixupOverlappingSequencePoints, emitTailcalls, showTimes, dumpDebugInfo) modul noDebugData =
    // Store the public key from the signer into the manifest.  This means it will be written 
    // to the binary and also acts as an indicator to leave space for delay sign 

    reportTime showTimes "Write Started";
    let isDll = modul.IsDLL
    
    let signer = 
        match signer,modul.Manifest with
        | Some _, _ -> signer
        | _, None -> signer
        | None, Some {PublicKey=Some pubkey} -> 
            (dprintn "Note: The output assembly will be delay-signed using the original public";
             dprintn "Note: key. In order to load it you will need to either sign it with";
             dprintn "Note: the original private key or to turn off strong-name verification";
             dprintn "Note: (use sn.exe from the .NET Framework SDK to do this, e.g. 'sn -Vr *').";
             dprintn "Note: Alternatively if this tool supports it you can provide the original";
             dprintn "Note: private key when converting the assembly, assuming you have access to";
             dprintn "Note: it.";
             Some (ILStrongNameSigner.OpenPublicKey pubkey))
        | _ -> signer

    let modul = 
        let pubkey =
          match signer with 
          | None -> None
          | Some s -> 
             try Some s.PublicKey  
             with e ->     
               failwith ("A call to StrongNameGetPublicKey failed ("+e.Message+")"); 
               None
        begin match modul.Manifest with 
        | None -> () 
        | Some m -> 
           if m.PublicKey <> None && m.PublicKey <> pubkey then 
             dprintn "Warning: The output assembly is being signed or delay-signed with a strong name that is different to the original."
        end;
        { modul with Manifest = match modul.Manifest with None -> None | Some m -> Some {m with PublicKey = pubkey} }

    let timestamp = absilWriteGetTimeStamp ()

    let os = 
        try  
#if SILVERLIGHT
            let fs1 = System.IO.IsolatedStorage.IsolatedStorageFile.GetUserStoreForApplication().CreateFile(outfile)
#else
            let fs1 = new FileStream(outfile,FileMode.Create,FileAccess.Write,FileShare.Read ,0x1000,false) 
#endif
            new BinaryWriter(fs1)
        with e -> 
            failwith ("Could not open file for writing (binary mode): " + outfile)    

    let  pdbData,debugDirectoryChunk,debugDataChunk,textV2P,mappings =
        try 
      
          let imageBaseReal = modul.ImageBase // FIXED CHOICE
          let alignVirt = modul.VirtualAlignment // FIXED CHOICE
          let alignPhys = modul.PhysicalAlignment // FIXED CHOICE
          
          let isItanium = modul.Platform = Some(IA64)
          
          let numSections = 3 // .text, .sdata, .reloc 


          // HEADERS 

          let next = 0x0
          let headerSectionPhysLoc = 0x0
          let headerAddr = next
          let next = headerAddr
          
          let msdosHeaderSize = 0x80
          let msdosHeaderChunk,next = chunk msdosHeaderSize next
          
          let peSignatureSize = 0x04
          let peSignatureChunk,next = chunk peSignatureSize next
          
          let peFileHeaderSize = 0x14
          let peFileHeaderChunk,next = chunk peFileHeaderSize next
          
          let peOptionalHeaderSize = if modul.Is64Bit then 0xf0 else 0xe0
          let peOptionalHeaderChunk,next = chunk peOptionalHeaderSize next
          
          let textSectionHeaderSize = 0x28
          let textSectionHeaderChunk,next = chunk textSectionHeaderSize next
          
          let dataSectionHeaderSize = 0x28
          let dataSectionHeaderChunk,next = chunk dataSectionHeaderSize next
          
          let relocSectionHeaderSize = 0x28
          let relocSectionHeaderChunk,next = chunk relocSectionHeaderSize next
          
          let headerSize = next - headerAddr
          let nextPhys = align alignPhys (headerSectionPhysLoc + headerSize)
          let headerSectionPhysSize = nextPhys - headerSectionPhysLoc
          let next = align alignVirt (headerAddr + headerSize)
          
          // TEXT SECTION:  8 bytes IAT table 72 bytes CLI header 

          let textSectionPhysLoc = nextPhys
          let textSectionAddr = next
          let next = textSectionAddr
          
          let importAddrTableChunk,next = chunk 0x08 next
          let cliHeaderPadding = (if isItanium then (align 16 next) else next) - next
          let next = next + cliHeaderPadding
          let cliHeaderChunk,next = chunk 0x48 next
          
          let desiredMetadataVersion = 
            if modul.MetadataVersion <> "" then
                parseILVersion modul.MetadataVersion
            else
                match mscorlib with 
                | ILScopeRef.Local -> failwith "Expected mscorlib to be ILScopeRef.Assembly was ILScopeRef.Local" 
                | ILScopeRef.Module(_) -> failwith "Expected mscorlib to be ILScopeRef.Assembly was ILScopeRef.Module"
                | ILScopeRef.Assembly(aref) ->
                    match aref.Version with
                    | Some (2us,_,_,_) -> parseILVersion "2.0.50727.0"
                    | Some v -> v
                    | None -> failwith "Expected msorlib to have a version number"

          let entryPointToken,code,codePadding,metadata,data,resources,requiredDataFixups,pdbData,mappings = 
            writeILMetadataAndCode ((pdbfile <> None), desiredMetadataVersion,mscorlib,emitTailcalls,showTimes) modul noDebugData next

          reportTime showTimes "Generated IL and metadata";
          let _codeChunk,next = chunk code.Length next
          let _codePaddingChunk,next = chunk codePadding.Length next
          
          let metadataChunk,next = chunk metadata.Length next
          
          let strongnameChunk,next = 
            match signer with 
            | None -> nochunk next
            | Some s -> chunk s.SignatureSize next

          let resourcesChunk,next = chunk resources.Length next
         
          let rawdataChunk,next = chunk data.Length next

          let vtfixupsChunk,next = nochunk next   // Note: only needed for mixed mode assemblies
          let importTableChunkPrePadding = (if isItanium then (align 16 next) else next) - next
          let next = next + importTableChunkPrePadding
          let importTableChunk,next = chunk 0x28 next
          let importLookupTableChunk,next = chunk 0x14 next
          let importNameHintTableChunk,next = chunk 0x0e next
          let mscoreeStringChunk,next = chunk 0x0c next
          
          let next = align 0x10 (next + 0x05) - 0x05
          let importTableChunk = { addr=importTableChunk.addr; size = next - importTableChunk.addr}
          let importTableChunkPadding = importTableChunk.size - (0x28 + 0x14 + 0x0e + 0x0c)
          
          let next = next + 0x03
          let entrypointCodeChunk,next = chunk 0x06 next
          let globalpointerCodeChunk,next = chunk (if isItanium then 0x8 else 0x0) next
          
          let debugDirectoryChunk,next = chunk (if pdbfile = None then 0x0 else sizeof_IMAGE_DEBUG_DIRECTORY) next
          // The debug data is given to us by the PDB writer and appears to 
          // typically be the type of the data plus the PDB file name.  We fill 
          // this in after we've written the binary. We approximate the size according 
          // to what PDB writers seem to require and leave extra space just in case... 
          let debugDataJustInCase = 40
          let debugDataChunk,next = 
              chunk (align 0x4 (match pdbfile with 
                                | None -> 0x0 
                                | Some f -> (24 
                                            + System.Text.Encoding.Unicode.GetByteCount(f) // See bug 748444
                                            + debugDataJustInCase))) next


          let textSectionSize = next - textSectionAddr
          let nextPhys = align alignPhys (textSectionPhysLoc + textSectionSize)
          let textSectionPhysSize = nextPhys - textSectionPhysLoc
          let next = align alignVirt (textSectionAddr + textSectionSize)
          
          // .RSRC SECTION (DATA) 
          let dataSectionPhysLoc =  nextPhys
          let dataSectionAddr = next
          let dataSectionVirtToPhys v = v - dataSectionAddr + dataSectionPhysLoc
          
#if SILVERLIGHT
          let nativeResources = [| |]
#else
          let resourceFormat = if modul.Is64Bit then Support.X64 else Support.X86
          
          let nativeResources = 
            match modul.NativeResources with
            | [] -> [||]
            | resources ->
                if runningOnMono then
                  [||]
                else
                  let unlinkedResources = List.map Lazy.force resources
                  begin
                    try linkNativeResources unlinkedResources next resourceFormat (Path.GetDirectoryName(outfile))
                    with e -> failwith ("Linking a native resource failed: "+e.Message+"")
                  end
#endif
                
          let nativeResourcesSize = nativeResources.Length

          let nativeResourcesChunk,next = chunk nativeResourcesSize next
        
          let dummydatap,next = chunk (if next = dataSectionAddr then 0x01 else 0x0) next
          
          let dataSectionSize = next - dataSectionAddr
          let nextPhys = align alignPhys (dataSectionPhysLoc + dataSectionSize)
          let dataSectionPhysSize = nextPhys - dataSectionPhysLoc
          let next = align alignVirt (dataSectionAddr + dataSectionSize)
          
          // .RELOC SECTION  base reloc table: 0x0c size 
          let relocSectionPhysLoc =  nextPhys
          let relocSectionAddr = next
          let baseRelocTableChunk,next = chunk 0x0c next

          let relocSectionSize = next - relocSectionAddr
          let nextPhys = align alignPhys (relocSectionPhysLoc + relocSectionSize)
          let relocSectionPhysSize = nextPhys - relocSectionPhysLoc
          let next = align alignVirt (relocSectionAddr + relocSectionSize)

         // Now we know where the data section lies we can fix up the  
         // references into the data section from the metadata tables. 
          begin 
            requiredDataFixups |> List.iter
              (fun (metadataOffset32,(dataOffset,kind)) -> 
                let metadataOffset =  metadataOffset32
                if metadataOffset < 0 || metadataOffset >= metadata.Length - 4  then failwith "data RVA fixup: fixup located outside metadata";
                checkFixup32 metadata metadataOffset 0xdeaddddd;
                let dataRva = 
                  if kind then
                      let res = dataOffset
                      if res >= resourcesChunk.size then dprintn ("resource offset bigger than resource data section");
                      res
                  else 
                      let res = rawdataChunk.addr + dataOffset
                      if res < rawdataChunk.addr then dprintn ("data rva before data section");
                      if res >= rawdataChunk.addr + rawdataChunk.size then dprintn ("data rva after end of data section, dataRva = "+string res+", rawdataChunk.addr = "+string rawdataChunk.addr+", rawdataChunk.size = "+string rawdataChunk.size);
                      res
                applyFixup32 metadata metadataOffset dataRva);
          end;
          
         // IMAGE TOTAL SIZE 
          let imageEndSectionPhysLoc =  nextPhys
          let imageEndAddr = next

          reportTime showTimes "Layout image";

          let write p (os: BinaryWriter) chunkName chunk = 
              match p with 
              | None -> () 
              | Some pExpected -> 
                  os.Flush(); 
                  let pCurrent =  int32 os.BaseStream.Position
                  if pCurrent <> pExpected then 
                    failwith ("warning: "+chunkName+" not where expected, pCurrent = "+string pCurrent+", p.addr = "+string pExpected) 
              writeBytes os chunk 
          
          let writePadding (os: BinaryWriter) _comment sz =
              if sz < 0 then failwith "writePadding: size < 0";
              for i = 0 to sz - 1 do 
                  os.Write 0uy
          
          // Now we've computed all the offsets, write the image 
          
          write (Some msdosHeaderChunk.addr) os "msdos header" msdosHeader;
          
          write (Some peSignatureChunk.addr) os "pe signature" [| |];
          
          writeInt32 os 0x4550;
          
          write (Some peFileHeaderChunk.addr) os "pe file header" [| |];
          
          if (modul.Platform = Some(AMD64)) then
            writeInt32AsUInt16 os 0x8664 // Machine - IMAGE_FILE_MACHINE_AMD64 
          elif isItanium then
            writeInt32AsUInt16 os 0x200
          else
            writeInt32AsUInt16 os 0x014c; // Machine - IMAGE_FILE_MACHINE_I386 
            
          writeInt32AsUInt16 os numSections; 
          writeInt32 os timestamp; // date since 1970 
          writeInt32 os 0x00; // Pointer to Symbol Table Always 0 
       // 00000090 
          writeInt32 os 0x00; // Number of Symbols Always 0 
          writeInt32AsUInt16 os peOptionalHeaderSize; // Size of the optional header, the format is described below. 
          
          // 64bit: IMAGE_FILE_32BIT_MACHINE ||| IMAGE_FILE_LARGE_ADDRESS_AWARE
          // 32bit: IMAGE_FILE_32BIT_MACHINE
          // Yes, 32BIT_MACHINE is set for AMD64...
          let iMachineCharacteristic = match modul.Platform with | Some IA64 -> 0x20 | Some AMD64 -> 0x0120 | _ -> 0x0100
          
          writeInt32AsUInt16 os ((if isDll then 0x2000 else 0x0000) ||| 0x0002 ||| 0x0004 ||| 0x0008 ||| iMachineCharacteristic);
          
     // Now comes optional header 

          let peOptionalHeaderByte = peOptionalHeaderByteByCLRVersion desiredMetadataVersion

          write (Some peOptionalHeaderChunk.addr) os "pe optional header" [| |];
          if modul.Is64Bit then
            writeInt32AsUInt16 os 0x020B // Magic number is 0x020B for 64-bit 
          else
            writeInt32AsUInt16 os 0x010b; // Always 0x10B (see Section 23.1). 
          writeInt32AsUInt16 os peOptionalHeaderByte; // ECMA spec says 6, some binaries, e.g. fscmanaged.exe say 7, Whidbey binaries say 8 
          writeInt32 os textSectionPhysSize;          // Size of the code (text) section, or the sum of all code sections if there are multiple sections. 
       // 000000a0 
          writeInt32 os dataSectionPhysSize;          // Size of the initialized data section, or the sum of all such sections if there are multiple data sections. 
          writeInt32 os 0x00;                         // Size of the uninitialized data section, or the sum of all such sections if there are multiple unitinitalized data sections. 
          writeInt32 os entrypointCodeChunk.addr;     // RVA of entry point , needs to point to bytes 0xFF 0x25 followed by the RVA+!0x4000000 in a section marked execute/read for EXEs or 0 for DLLs e.g. 0x0000b57e 
          writeInt32 os textSectionAddr;              // e.g. 0x0002000 
       // 000000b0 
          if modul.Is64Bit then
            writeInt64 os ((int64)imageBaseReal)    
          else             
            writeInt32 os dataSectionAddr; // e.g. 0x0000c000           
            writeInt32 os imageBaseReal; // Image Base Always 0x400000 (see Section 23.1). - QUERY : no it's not always 0x400000, e.g. 0x034f0000 
            
          writeInt32 os alignVirt;  //  Section Alignment Always 0x2000 (see Section 23.1). 
          writeInt32 os alignPhys; // File Alignment Either 0x200 or 0x1000. 
       // 000000c0  
          writeInt32AsUInt16 os 0x04; //  OS Major Always 4 (see Section 23.1). 
          writeInt32AsUInt16 os 0x00; // OS Minor Always 0 (see Section 23.1). 
          writeInt32AsUInt16 os 0x00; // User Major Always 0 (see Section 23.1). 
          writeInt32AsUInt16 os 0x00; // User Minor Always 0 (see Section 23.1). 
          writeInt32AsUInt16 os 0x04; // SubSys Major Always 4 (see Section 23.1). 
          writeInt32AsUInt16 os 0x00; // SubSys Minor Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Reserved Always 0 (see Section 23.1). 
       // 000000d0  
          writeInt32 os imageEndAddr; // Image Size: Size, in bytes, of image, including all headers and padding; shall be a multiple of Section Alignment. e.g. 0x0000e000 
          writeInt32 os headerSectionPhysSize; // Header Size Combined size of MS-DOS Header, PE Header, PE Optional Header and padding; shall be a multiple of the file alignment. 
          writeInt32 os 0x00; // File Checksum Always 0 (see Section 23.1). QUERY: NOT ALWAYS ZERO 
          writeInt32AsUInt16 os modul.SubSystemFlags; // SubSystem Subsystem required to run this image. Shall be either IMAGE_SUBSYSTEM_WINDOWS_CE_GUI (0x3) or IMAGE_SUBSYSTEM_WINDOWS_GUI (0x2). QUERY: Why is this 3 on the images ILASM produces 
          // DLL Flags Always 0x400 (no unmanaged windows exception handling - see Section 23.1).
          //  Itanium: see notes at end of file 
          //  IMAGE_DLLCHARACTERISTICS_NX_COMPAT: See FSharp 1.0 bug 5019 and http://blogs.msdn.com/ed_maurer/archive/2007/12/14/nxcompat-and-the-c-compiler.aspx 
          // Itanium is IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE | IMAGE_DLLCHARACTERISTICS_ NO_SEH | IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE | IMAGE_DLLCHARACTERISTICS_NX_COMPAT
          // All others are IMAGE_DLLCHARACTERISTICS_ NO_SEH | IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE | IMAGE_DLLCHARACTERISTICS_NX_COMPAT
          writeInt32AsUInt16 os (if isItanium then 0x8540 else 0x540);  
       // 000000e0 
          // Note that the defaults differ between x86 and x64
          if modul.Is64Bit then
            let size = defaultArg modul.StackReserveSize 0x400000 |> int64
            writeInt64 os size; // Stack Reserve Size Always 0x400000 (4Mb) (see Section 23.1). 
            writeInt64 os 0x4000L; // Stack Commit Size Always 0x4000 (16Kb) (see Section 23.1). 
            writeInt64 os 0x100000L; // Heap Reserve Size Always 0x100000 (1Mb) (see Section 23.1). 
            writeInt64 os 0x2000L // Heap Commit Size Always 0x800 (8Kb) (see Section 23.1). 
          else
            let size = defaultArg modul.StackReserveSize 0x100000
            writeInt32 os size; // Stack Reserve Size Always 0x100000 (1Mb) (see Section 23.1). 
            writeInt32 os 0x1000; // Stack Commit Size Always 0x1000 (4Kb) (see Section 23.1). 
            writeInt32 os 0x100000; // Heap Reserve Size Always 0x100000 (1Mb) (see Section 23.1). 
            writeInt32 os 0x1000; // Heap Commit Size Always 0x1000 (4Kb) (see Section 23.1).             
       // 000000f0 - x86 location, moving on, for x64, add 0x10  
          writeInt32 os 0x00; // Loader Flags Always 0 (see Section 23.1) 
          writeInt32 os 0x10; // Number of Data Directories: Always 0x10 (see Section 23.1). 
          writeInt32 os 0x00; 
          writeInt32 os 0x00; // Export Table Always 0 (see Section 23.1). 
       // 00000100  
          writeDirectory os importTableChunk; // Import Table RVA of Import Table, (see clause 24.3.1). e.g. 0000b530  
          // Native Resource Table: ECMA says Always 0 (see Section 23.1), but mscorlib and other files with resources bound into executable do not.  For the moment assume the resources table is always the first resource in the file. 
          writeDirectory os nativeResourcesChunk;

       // 00000110  
          writeInt32 os 0x00; // Exception Table Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Exception Table Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Certificate Table Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Certificate Table Always 0 (see Section 23.1). 
       // 00000120  
          writeDirectory os baseRelocTableChunk; 
          writeDirectory os debugDirectoryChunk; // Debug Directory 
       // 00000130  
          writeInt32 os 0x00; //  Copyright Always 0 (see Section 23.1). 
          writeInt32 os 0x00; //  Copyright Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Global Ptr Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Global Ptr Always 0 (see Section 23.1). 
       // 00000140  
          writeInt32 os 0x00; // Load Config Table Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Load Config Table Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // TLS Table Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // TLS Table Always 0 (see Section 23.1). 
       // 00000150   
          writeInt32 os 0x00; // Bound Import Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Bound Import Always 0 (see Section 23.1). 
          writeDirectory os importAddrTableChunk; // Import Addr Table, (see clause 24.3.1). e.g. 0x00002000  
       // 00000160   
          writeInt32 os 0x00; // Delay Import Descriptor Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Delay Import Descriptor Always 0 (see Section 23.1). 
          writeDirectory os cliHeaderChunk;
       // 00000170  
          writeInt32 os 0x00; // Reserved Always 0 (see Section 23.1). 
          writeInt32 os 0x00; // Reserved Always 0 (see Section 23.1). 
          
          write (Some textSectionHeaderChunk.addr) os "text section header" [| |];
          
       // 00000178  
          writeBytes os  [| 0x2euy; 0x74uy; 0x65uy; 0x78uy; 0x74uy; 0x00uy; 0x00uy; 0x00uy; |]; // ".text\000\000\000" 
       // 00000180  
          writeInt32 os textSectionSize; // VirtualSize: Total size of the section when loaded into memory in bytes rounded to Section Alignment. If this value is greater than Size of Raw Data, the section is zero-padded. e.g. 0x00009584 
          writeInt32 os textSectionAddr; //  VirtualAddress For executable images this is the address of the first byte of the section, when loaded into memory, relative to the image base. e.g. 0x00020000 
          writeInt32 os textSectionPhysSize; //  SizeOfRawData Size of the initialized data on disk in bytes, shall be a multiple of FileAlignment from the PE header. If this is less than VirtualSize the remainder of the section is zero filled. Because this field is rounded while the VirtualSize field is not it is possible for this to be greater than VirtualSize as well. When a section contains only uninitialized data, this field should be 0. 0x00009600 
          writeInt32 os textSectionPhysLoc; // PointerToRawData RVA to section�s first page within the PE file. This shall be a multiple of FileAlignment from the optional header. When a section contains only uninitialized data, this field should be 0. e.g. 00000200 
       // 00000190  
          writeInt32 os 0x00; // PointerToRelocations RVA of Relocation section. 
          writeInt32 os 0x00; // PointerToLinenumbers Always 0 (see Section 23.1). 
       // 00000198  
          writeInt32AsUInt16 os 0x00;// NumberOfRelocations Number of relocations, set to 0 if unused. 
          writeInt32AsUInt16 os 0x00;  //  NumberOfLinenumbers Always 0 (see Section 23.1). 
          writeBytes os [| 0x20uy; 0x00uy; 0x00uy; 0x60uy |]; //  Characteristics Flags describing section�s characteristics, see below. IMAGE_SCN_CNT_CODE || IMAGE_SCN_MEM_EXECUTE || IMAGE_SCN_MEM_READ 
          
          write (Some dataSectionHeaderChunk.addr) os "data section header" [| |];
          
       // 000001a0  
          writeBytes os [| 0x2euy; 0x72uy; 0x73uy; 0x72uy; 0x63uy; 0x00uy; 0x00uy; 0x00uy; |]; // ".rsrc\000\000\000" 
    //  writeBytes os [| 0x2e; 0x73; 0x64; 0x61; 0x74; 0x61; 0x00; 0x00; |]; // ".sdata\000\000"  
          writeInt32 os dataSectionSize; // VirtualSize: Total size of the section when loaded into memory in bytes rounded to Section Alignment. If this value is greater than Size of Raw Data, the section is zero-padded. e.g. 0x0000000c 
          writeInt32 os dataSectionAddr; //  VirtualAddress For executable images this is the address of the first byte of the section, when loaded into memory, relative to the image base. e.g. 0x0000c000
       // 000001b0  
          writeInt32 os dataSectionPhysSize; //  SizeOfRawData Size of the initialized data on disk in bytes, shall be a multiple of FileAlignment from the PE header. If this is less than VirtualSize the remainder of the section is zero filled. Because this field is rounded while the VirtualSize field is not it is possible for this to be greater than VirtualSize as well. When a section contains only uninitialized data, this field should be 0. e.g. 0x00000200 
          writeInt32 os dataSectionPhysLoc; // PointerToRawData QUERY: Why does ECMA say "RVA" here? Offset to section�s first page within the PE file. This shall be a multiple of FileAlignment from the optional header. When a section contains only uninitialized data, this field should be 0. e.g. 0x00009800 
       // 000001b8  
          writeInt32 os 0x00; // PointerToRelocations RVA of Relocation section. 
          writeInt32 os 0x00; // PointerToLinenumbers Always 0 (see Section 23.1). 
       // 000001c0  
          writeInt32AsUInt16 os 0x00; // NumberOfRelocations Number of relocations, set to 0 if unused. 
          writeInt32AsUInt16 os 0x00;  //  NumberOfLinenumbers Always 0 (see Section 23.1). 
          writeBytes os [| 0x40uy; 0x00uy; 0x00uy; 0x40uy |]; //  Characteristics Flags: IMAGE_SCN_MEM_READ |  IMAGE_SCN_CNT_INITIALIZED_DATA 
          
          write (Some relocSectionHeaderChunk.addr) os "reloc section header" [| |];
       // 000001a0  
          writeBytes os [| 0x2euy; 0x72uy; 0x65uy; 0x6cuy; 0x6fuy; 0x63uy; 0x00uy; 0x00uy; |]; // ".reloc\000\000" 
          writeInt32 os relocSectionSize; // VirtualSize: Total size of the section when loaded into memory in bytes rounded to Section Alignment. If this value is greater than Size of Raw Data, the section is zero-padded. e.g. 0x0000000c 
          writeInt32 os relocSectionAddr; //  VirtualAddress For executable images this is the address of the first byte of the section, when loaded into memory, relative to the image base. e.g. 0x0000c000
       // 000001b0  
          writeInt32 os relocSectionPhysSize; //  SizeOfRawData Size of the initialized reloc on disk in bytes, shall be a multiple of FileAlignment from the PE header. If this is less than VirtualSize the remainder of the section is zero filled. Because this field is rounded while the VirtualSize field is not it is possible for this to be greater than VirtualSize as well. When a section contains only uninitialized reloc, this field should be 0. e.g. 0x00000200 
          writeInt32 os relocSectionPhysLoc; // PointerToRawData QUERY: Why does ECMA say "RVA" here? Offset to section�s first page within the PE file. This shall be a multiple of FileAlignment from the optional header. When a section contains only uninitialized reloc, this field should be 0. e.g. 0x00009800 
       // 000001b8  
          writeInt32 os 0x00; // PointerToRelocations RVA of Relocation section. 
          writeInt32 os 0x00; // PointerToLinenumbers Always 0 (see Section 23.1). 
       // 000001c0  
          writeInt32AsUInt16 os 0x00; // NumberOfRelocations Number of relocations, set to 0 if unused. 
          writeInt32AsUInt16 os 0x00;  //  NumberOfLinenumbers Always 0 (see Section 23.1). 
          writeBytes os [| 0x40uy; 0x00uy; 0x00uy; 0x42uy |]; //  Characteristics Flags: IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ |  
          
          writePadding os "pad to text begin" (textSectionPhysLoc - headerSize);
          
          // TEXT SECTION: e.g. 0x200 
          
          let textV2P v = v - textSectionAddr + textSectionPhysLoc
          
          // e.g. 0x0200 
          write (Some (textV2P importAddrTableChunk.addr)) os "import addr table" [| |];
          writeInt32 os importNameHintTableChunk.addr; 
          writeInt32 os 0x00;  // QUERY 4 bytes of zeros not 2 like ECMA  24.3.1 says 
          
          // e.g. 0x0208 

          let flags = 
            (if modul.IsILOnly then 0x01 else 0x00) ||| 
            (if modul.Is32Bit then 0x02 else 0x00) ||| 
            (if (match signer with None -> false | Some s -> s.IsFullySigned) then 0x08 else 0x00)

          let headerVersionMajor,headerVersionMinor = headerVersionSupportedByCLRVersion desiredMetadataVersion

          writePadding os "pad to cli header" cliHeaderPadding 
          write (Some (textV2P cliHeaderChunk.addr)) os "cli header"  [| |];
          writeInt32 os 0x48; // size of header 
          writeInt32AsUInt16 os headerVersionMajor; // Major part of minimum version of CLR reqd. 
          writeInt32AsUInt16 os headerVersionMinor; // Minor part of minimum version of CLR reqd. ... 
          // e.g. 0x0210 
          writeDirectory os metadataChunk;
          writeInt32 os flags;
          
          writeInt32 os entryPointToken; 
          write None os "rest of cli header" [| |];
          
          // e.g. 0x0220 
          writeDirectory os resourcesChunk;
          writeDirectory os strongnameChunk;
          // e.g. 0x0230 
          writeInt32 os 0x00; // code manager table, always 0 
          writeInt32 os 0x00; // code manager table, always 0 
          writeDirectory os vtfixupsChunk; 
          // e.g. 0x0240 
          writeInt32 os 0x00;  // export addr table jumps, always 0 
          writeInt32 os 0x00;  // export addr table jumps, always 0 
          writeInt32 os 0x00;  // managed native header, always 0 
          writeInt32 os 0x00;  // managed native header, always 0 
          
          writeBytes os code;
          write None os "code padding" codePadding;
          
          writeBytes os metadata;
          
          // write 0x80 bytes of empty space for encrypted SHA1 hash, written by SN.EXE or call to signing API 
          if signer <> None then 
            write (Some (textV2P strongnameChunk.addr)) os "strongname" (Array.create strongnameChunk.size 0x0uy);
          
          write (Some (textV2P resourcesChunk.addr)) os "raw resources" [| |];
          writeBytes os resources;
          write (Some (textV2P rawdataChunk.addr)) os "raw data" [| |];
          writeBytes os data;

          writePadding os "start of import table" importTableChunkPrePadding

          // vtfixups would go here 
          write (Some (textV2P importTableChunk.addr)) os "import table" [| |];
          
          writeInt32 os importLookupTableChunk.addr;
          writeInt32 os 0x00;
          writeInt32 os 0x00;
          writeInt32 os mscoreeStringChunk.addr;
          writeInt32 os importAddrTableChunk.addr;
          writeInt32 os 0x00;
          writeInt32 os 0x00;
          writeInt32 os 0x00;
          writeInt32 os 0x00;
          writeInt32 os 0x00; 
        
          write (Some (textV2P importLookupTableChunk.addr)) os "import lookup table" [| |];
          writeInt32 os importNameHintTableChunk.addr; 
          writeInt32 os 0x00; 
          writeInt32 os 0x00; 
          writeInt32 os 0x00; 
          writeInt32 os 0x00; 
          

          write (Some (textV2P importNameHintTableChunk.addr)) os "import name hint table" [| |];
          // Two zero bytes of hint, then Case sensitive, null-terminated ASCII string containing name to import. 
          // Shall _CorExeMain a .exe file _CorDllMain for a .dll file.
          if isDll then 
              writeBytes os [| 0x00uy;  0x00uy;  0x5fuy;  0x43uy ;  0x6fuy;  0x72uy;  0x44uy;  0x6cuy;  0x6cuy;  0x4duy;  0x61uy;  0x69uy;  0x6euy;  0x00uy |]
          else 
              writeBytes os [| 0x00uy;  0x00uy;  0x5fuy;  0x43uy;  0x6fuy;  0x72uy;  0x45uy;  0x78uy;  0x65uy;  0x4duy;  0x61uy;  0x69uy;  0x6euy;  0x00uy |];
          
          write (Some (textV2P mscoreeStringChunk.addr)) os "mscoree string"
            [| 0x6duy;  0x73uy;  0x63uy;  0x6fuy ;  0x72uy;  0x65uy ;  0x65uy;  0x2euy ;  0x64uy;  0x6cuy ;  0x6cuy;  0x00uy ; |];
          
          writePadding os "end of import tab" importTableChunkPadding;
          
          writePadding os "head of entrypoint" 0x03;
          let ep = (imageBaseReal + textSectionAddr)
          write (Some (textV2P entrypointCodeChunk.addr)) os " entrypoint code"
                 [| 0xFFuy; 0x25uy; (* x86 Instructions for entry *) b0 ep; b1 ep; b2 ep; b3 ep |];
          if isItanium then 
              write (Some (textV2P globalpointerCodeChunk.addr)) os " itanium global pointer"
                   [| 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy; 0x0uy |];
          
          if pdbfile.IsSome then 
              write (Some (textV2P debugDirectoryChunk.addr)) os "debug directory" (Array.create sizeof_IMAGE_DEBUG_DIRECTORY 0x0uy);
              write (Some (textV2P debugDataChunk.addr)) os "debug data" (Array.create debugDataChunk.size 0x0uy);
          
          writePadding os "end of .text" (dataSectionPhysLoc - textSectionPhysLoc - textSectionSize);
          
          // DATA SECTION 
          match nativeResources with
          | [||] -> ()
          | resources ->
                write (Some (dataSectionVirtToPhys nativeResourcesChunk.addr)) os "raw native resources" [| |];
                writeBytes os resources;

          if dummydatap.size <> 0x0 then
              write (Some (dataSectionVirtToPhys dummydatap.addr)) os "dummy data" [| 0x0uy |];

          writePadding os "end of .rsrc" (relocSectionPhysLoc - dataSectionPhysLoc - dataSectionSize);            
          
          // RELOC SECTION 

          // See ECMA 24.3.2 
          let relocV2P v = v - relocSectionAddr + relocSectionPhysLoc
          
          let entrypointFixupAddr = entrypointCodeChunk.addr + 0x02
          let entrypointFixupBlock = (entrypointFixupAddr / 4096) * 4096
          let entrypointFixupOffset = entrypointFixupAddr - entrypointFixupBlock
          let reloc = (if modul.Is64Bit then 0xA000 (* IMAGE_REL_BASED_DIR64 *) else 0x3000 (* IMAGE_REL_BASED_HIGHLOW *)) ||| entrypointFixupOffset
          // For the itanium, you need to set a relocation entry for the global pointer
          let reloc2 = 
              if not isItanium then 
                  0x0
              else
                  0xA000 ||| (globalpointerCodeChunk.addr - ((globalpointerCodeChunk.addr / 4096) * 4096))
               
          write (Some (relocV2P baseRelocTableChunk.addr)) os "base reloc table" 
              [| b0 entrypointFixupBlock; b1 entrypointFixupBlock; b2 entrypointFixupBlock; b3 entrypointFixupBlock;
                 0x0cuy; 0x00uy; 0x00uy; 0x00uy;
                 b0 reloc; b1 reloc; 
                 b0 reloc2; b1 reloc2; |];
          writePadding os "end of .reloc" (imageEndSectionPhysLoc - relocSectionPhysLoc - relocSectionSize);

          os.Close();
          
          try 
              FileSystemUtilites.setExecutablePermission outfile
          with _ -> 
              ()
          pdbData,debugDirectoryChunk,debugDataChunk,textV2P,mappings
          
        // Looks like a finally...
        with e ->   
            (try 
                os.Close(); 
                System.IO.File.Delete outfile 
             with _ -> ()); 
            reraise()
            raise e // is this really needed?
   

    reportTime showTimes "Writing Image";
     
    if dumpDebugInfo then 
        DumpDebugInfo outfile pdbData

#if SILVERLIGHT
#else
    // Now we've done the bulk of the binary, do the PDB file and fixup the binary. 
    begin match pdbfile with
    | None -> ()
    | Some fmdb when runningOnMono -> 
        WriteMdbInfo fmdb outfile pdbData
            
    | Some fpdb -> 
        try 
            let idd = WritePdbInfo fixupOverlappingSequencePoints showTimes outfile fpdb pdbData
            reportTime showTimes "Generate PDB Info";
            
          // Now we have the debug data we can go back and fill in the debug directory in the image 
            let fs2 = new FileStream(outfile, FileMode.OpenOrCreate, FileAccess.Write, FileShare.Read, 0x1000, false)
            let os2 = new BinaryWriter(fs2)
            try 
                // write the IMAGE_DEBUG_DIRECTORY 
                os2.BaseStream.Seek (int64 (textV2P debugDirectoryChunk.addr), SeekOrigin.Begin) |> ignore;
                writeInt32 os2 idd.iddCharacteristics; // IMAGE_DEBUG_DIRECTORY.Characteristics 
                writeInt32 os2 timestamp;
                writeInt32AsUInt16 os2 idd.iddMajorVersion;
                writeInt32AsUInt16 os2 idd.iddMinorVersion;
                writeInt32 os2 idd.iddType;
                writeInt32 os2 idd.iddData.Length;  // IMAGE_DEBUG_DIRECTORY.SizeOfData 
                writeInt32 os2 debugDataChunk.addr;  // IMAGE_DEBUG_DIRECTORY.AddressOfRawData 
                writeInt32 os2 (textV2P debugDataChunk.addr);// IMAGE_DEBUG_DIRECTORY.PointerToRawData 

                (* dprintf "idd.iddCharacteristics = %ld\n" idd.iddCharacteristics;
                dprintf "iddMajorVersion = %ld\n" idd.iddMajorVersion;
                dprintf "iddMinorVersion = %ld\n" idd.iddMinorVersion;
                dprintf "iddType = %ld\n" idd.iddType;
                dprintf "iddData = (%A) = %s\n" idd.iddData (System.Text.Encoding.UTF8.GetString idd.iddData); *)
                  
                // write the debug raw data as given us by the PDB writer 
                os2.BaseStream.Seek (int64 (textV2P debugDataChunk.addr), SeekOrigin.Begin) |> ignore;
                if debugDataChunk.size < idd.iddData.Length then 
                    failwith "Debug data area is not big enough.  Debug info may not be usable";
                writeBytes os2 idd.iddData;
                os2.Close()
            with e -> 
                failwith ("Error while writing debug directory entry: "+e.Message);
                (try os2.Close(); System.IO.File.Delete outfile with _ -> ()); 
                reraise()
        with e -> 
            reraise()
            
    end;
#endif
    reportTime showTimes "Finalize PDB";

    /// Sign the binary.  No further changes to binary allowed past this point! 
    match signer with 
    | None -> ()
    | Some s -> 
        try 
            s.SignFile outfile;
            s.Close() 
        with e -> 
            failwith ("Warning: A call to StrongNameSignatureGeneration failed ("+e.Message+")");
            (try s.Close() with _ -> ());
            (try System.IO.File.Delete outfile with _ -> ()); 
            ()

    reportTime showTimes "Signing Image";
    //Finished writing and signing the binary and debug info...

    mappings


type options =
   { mscorlib: ILScopeRef;
     pdbfile: string option;
     signer: ILStrongNameSigner option;
     fixupOverlappingSequencePoints: bool;
     emitTailcalls : bool;
     showTimes: bool;
     dumpDebugInfo:bool }

// REVIEW-HOSTING: move noDebugData into options/args
let WriteILBinary outfile (args: options) modul noDebugData =
    ignore (writeBinaryAndReportMappings (outfile, args.mscorlib, args.pdbfile, args.signer, args.fixupOverlappingSequencePoints, args.emitTailcalls, args.showTimes, args.dumpDebugInfo) modul noDebugData)



(******************************************************
** Notes on supporting the Itanium (jopamer)         **
*******************************************************
IA64 codegen on the CLR isn�t documented, and getting it working involved a certain amount of reverse-engineering 
peverify.exe and various binaries generated by ILAsm and other managed compiles.  Here are some lessons learned, 
documented for posterity and the 0 other people writing managed compilers for the Itanium:

- Even if you�re not utilizing the global pointer in your Itanium binary, 
you should be setting aside space for it in .text.  (Preferably near the native stub.)
- PEVerify checks for two .reloc table entries on the Itanium - one for the native stub, and one 
for the global pointer RVA.  It doesn�t matter what you set these values to - 
their addresses can be zeroed out, but they must have IMAGE_REL_BASED_DIR64 set!  
(So, yes, you may find yourself setting this flag on an empty, unnecessary table slot!)
- On the Itanium, it�s best to have your tables qword aligned.  (Though, peverify checks for dword alignment.)
- A different, weird set of DLL characteristics are necessary for the Itanium.  
I won�t detail them here, but it�s interesting given that this field isn�t supposed to vary between platforms, 
and is supposedly marked as deprecated.
- There are two schools to generating CLR binaries on for the Itanium - I�ll call them the �ALink� school 
and the �ILAsm� school.
                - The ALink school relies on some quirks in the CLR to omit a lot of stuff that, admittedly, isn�t necessary.  The binaries are basically IL-only, with some flags set to make them nominally Itanium:
                                - It omits the .reloc table
                                - It doesn�t set aside memory for global pointer storage
                                - There�s no native stub
                                - There�s no import table, mscoree reference / startup symbol hint
                                - A manifest is inserted by default. 
                These omissions are understandable, given the platform/jitting/capabilities of the language, 
                but they�re basically relying on an idiosyncracy of the runtime to get away with creating a �bad� binary.

                - The ILAsm school actually writes everything out:
                                - It has a reloc table with the requisite two entries
                                - It sets aside memory for a global pointer, even if it doesn�t utilize one
                                - It actually inserts a native stub for the Itanium!  (Though, I have no idea what 
                                instructions, specifically, are emitted, and I couldn�t dig up the sources to ILAsm to 
                                find out)
                                - There�s the requisite mscoree reference, etc.
                                - No manifest is inserted
*******************************************************)
