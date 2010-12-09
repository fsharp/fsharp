//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2010 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


/// Anything to do with special names of identifiers and other lexical rules 
module internal Microsoft.FSharp.Compiler.Range

#nowarn "60"  // Override implementations in augmentations are now deprecated. Override implementations should be given as part of the initial declaration of a type.

open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Core.Printf

open Internal.Utilities
 
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler  
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Lib.Bits



type FileIndex = int32 

let col_nbits  = 9
let line_nbits  = 16

let pos_nbits = line_nbits + col_nbits
let _ = assert (pos_nbits <= 32)
let pos_col_mask  = mask32 0         col_nbits
let line_col_mask = mask32 col_nbits line_nbits
let inline (lsr)  (x:int) (y:int)  = int32 (uint32 x >>> y)

[<Struct>]
[<System.Diagnostics.DebuggerDisplay("{Line},{Column}")>]
type pos(code:int32) =
    new (l,c) = 
        let l = max 0 l 
        let c = max 0 c 
        let p = ( c &&& pos_col_mask)
                ||| ((l <<< col_nbits) &&& line_col_mask)
        pos p

    member p.Line = (code lsr col_nbits)
    member p.Column = (code &&& pos_col_mask)

    member r.Encoding = code
    static member EncodingSize = pos_nbits
    static member Decode (code:int32) : pos = pos code

let file_idx_nbits = 14
let start_line_nbits = line_nbits
let start_col_nbits = col_nbits
let end_line_nbits = line_nbits
let end_col_nbits = col_nbits
let _ = assert (file_idx_nbits + start_line_nbits + start_col_nbits + end_line_nbits + end_col_nbits = 64)

 
let file_idx_mask   = mask64 0 file_idx_nbits
let start_line_mask = mask64 (file_idx_nbits) start_line_nbits
let start_col_mask  = mask64 (file_idx_nbits + start_line_nbits) start_col_nbits
let end_line_mask   = mask64 (file_idx_nbits + start_line_nbits + start_col_nbits) end_line_nbits
let end_col_mask    = mask64 (file_idx_nbits + start_line_nbits + start_col_nbits + end_line_nbits) end_col_nbits

[<Struct>]
[<System.Diagnostics.DebuggerDisplay("{FileName} ({StartLine},{StartColumn}-{EndLine},{EndColumn})")>]
type range(code:int64) =
    static member Zero = range(0L)
    new (fidx, b:pos, e:pos) = 
        range(  int64 fidx
                ||| (int64 b.Line    <<< file_idx_nbits) 
                ||| (int64 b.Column  <<< (file_idx_nbits + start_line_nbits))
                ||| (int64 e.Line    <<< (file_idx_nbits + start_line_nbits + start_col_nbits))
                ||| (int64 e.Column  <<< (file_idx_nbits + start_line_nbits + start_col_nbits + end_line_nbits)) )

    member r.StartLine = int32((code &&& start_line_mask) >>> file_idx_nbits)
    member r.StartColumn = int32((code &&& start_col_mask)  >>> (file_idx_nbits + start_line_nbits)) 
    member r.EndLine = int32((code &&& end_line_mask)   >>> (file_idx_nbits + start_line_nbits + start_col_nbits)) 
    member r.EndColumn = int32((code &&& end_col_mask)    >>> (file_idx_nbits + start_line_nbits + start_col_nbits + end_line_nbits)) 
    member r.Start = pos (r.StartLine, r.StartColumn)
    member r.End = pos (r.EndLine, r.EndColumn)
    member r.FileIndex = int32(code &&& file_idx_mask)
    member m.StartRange = range (m.FileIndex, m.Start, m.Start)
    member m.EndRange = range (m.FileIndex, m.End, m.End)


// This is just a standard unique-index table
type FileIndexTable() = 
    let indexToFileTable = new ResizeArray<_>(11)
    let fileToIndexTable = new Dictionary<string,int>(11)
    member t.FileToIndex f = 
        let mutable res = 0 in
        let ok = fileToIndexTable.TryGetValue(f,&res) in
        if ok then res 
        else
            lock fileToIndexTable (fun () -> 
                let mutable res = 0 in
                let ok = fileToIndexTable.TryGetValue(f,&res) in
                if ok then res 
                else
                    let n = indexToFileTable.Count in
                    indexToFileTable.Add(f);
                    fileToIndexTable.[f] <- n;
                    n)

    member t.IndexToFile n = 
        (if n < 0 then failwithf "fileOfFileIndex: negative argument: n = %d\n" n);
        (if n >= indexToFileTable.Count then failwithf "fileOfFileIndex: invalid argument: n = %d\n" n);
        indexToFileTable.[n]
    
let maxFileIndex = pown32 file_idx_nbits

// ++GLOBAL MUTBALE STATE
// WARNING: Global Mutable State, holding a mapping between integers and filenames
let fileIndexTable = new FileIndexTable()

// Note if we exceed the maximum number of files we'll start to report incorrect file names
let fileIndexOfFile f = fileIndexTable.FileToIndex(f) % maxFileIndex 
let fileOfFileIndex n = fileIndexTable.IndexToFile(n)

let mkPos l c = pos (l,c)
let mkRange f b e = range (fileIndexOfFile f, b, e)
let mkFileIndexRange fi b e = range (fi, b, e)

type range with 
    member r.FileName = fileOfFileIndex r.FileIndex
    override r.ToString() =
        let sb = new System.Text.StringBuilder();
        sb
         .AppendFormat("{0} ({1},{2}-{3},{4})", r.FileName, r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
         .ToString()

(* end representation, start derived ops *)

let dest_file_idx_range (r:range) = r.FileIndex,r.Start,r.End

let trimRangeRight (r:range) n = 
    range (r.FileIndex, r.Start, pos (r.End.Line, max 0 (r.End.Column - n)))
                 
let posOrder   = Order.orderOn (fun (p:pos) -> p.Line, p.Column) (Pair.order (Int32.order,Int32.order))
(* rangeOrder: not a total order, but enough to sort on ranges *)      
let rangeOrder = Order.orderOn (fun (r:range) -> r.FileName, r.Start) (Pair.order (String.order,posOrder))

let outputPos   (os:TextWriter) (m:pos)   = fprintf os "(%d,%d)" m.Line m.Column
let outputRange (os:TextWriter) (m:range) = fprintf os "%s%a-%a" m.FileName outputPos m.Start outputPos m.End
let boutputPos   os (m:pos)   = bprintf os "(%d,%d)" m.Line m.Column
let boutputRange os (m:range) = bprintf os "%s%a-%a" m.FileName boutputPos m.Start boutputPos m.End
    
let posGt (p1:pos) (p2:pos) = (p1.Line > p2.Line || (p1.Line = p2.Line && p1.Column > p2.Column))
let posEq (p1:pos) (p2:pos) = (p1.Line = p2.Line &&  p1.Column = p2.Column)
let posGeq p1 p2 = posEq p1 p2 || posGt p1 p2

let unionRanges (m1:range) (m2:range) = 
    if m1.FileIndex <> m2.FileIndex then m2 else
    let b = 
      if posGeq m1.Start m2.Start then m2.Start
      else m1.Start 
    let e = 
      if posGeq m1.End m2.End then m1.End
      else m2.End 
    range (m1.FileIndex, b, e)

let rangeContainsRange (m1:range) (m2:range) =
    m1.FileName = m2.FileName &&
    posGeq m2.Start m1.Start &&
    posGeq m1.End m2.End

let rangeContainsPos (m1:range) p =
    posGeq p m1.Start &&
    posGeq m1.End p

let rangeBeforePos (m1:range) p =
    posGeq p m1.End

let rangeN filename line =  mkRange filename (mkPos line 0) (mkPos line 80)
let pos0 = mkPos 1 0
let range0 =  rangeN "unknown" 1
let rangeStartup = rangeN "startup" 1
let rangeCmdArgs = rangeN "commandLineArgs" 0

let trimRangeToLine (r:range) =
    let filename = r.FileName
    let startL,startC = r.StartLine,r.StartColumn
    let endL ,_endC   = r.EndLine,r.EndColumn
    if endL <= startL then
      r
    else
      let endL,endC = startL+1,0   (* Trim to the start of the next line (we do not know the end of the current line) *)
      mkRange filename (mkPos startL startC) (mkPos endL endC)

(* For Diagnostics *)
let stringOfPos   (pos:pos) = sprintf "(%d,%d)" pos.Line pos.Column
let stringOfRange (r:range) = sprintf "%s%s-%s" r.FileName (stringOfPos r.Start) (stringOfPos r.End)


