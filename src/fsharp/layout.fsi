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

module internal Microsoft.FSharp.Compiler.Layout

open System.Text
open System.IO
open Internal.Utilities.StructuredFormat

type layout = Internal.Utilities.StructuredFormat.Layout

val emptyL                : Layout
val isEmptyL              : Layout -> bool
  
val wordL                 : string -> Layout
val sepL                  : string -> Layout
val rightL                : string -> Layout
val leftL                 : string -> Layout

val ( ^^ )                : Layout -> Layout -> Layout   (* never break "glue" *)
val ( ++ )                : Layout -> Layout -> Layout   (* if break, indent=0 *)
val ( -- )                : Layout -> Layout -> Layout   (* if break, indent=1 *)
val ( --- )               : Layout -> Layout -> Layout   (* if break, indent=2 *)
val ( ---- )              : Layout -> Layout -> Layout   (* if break, indent=2 *)
val ( ----- )             : Layout -> Layout -> Layout   (* if break, indent=2 *)
val ( @@ )                : Layout -> Layout -> Layout   (* broken ident=0 *)
val ( @@- )               : Layout -> Layout -> Layout   (* broken ident=1 *)
val ( @@-- )              : Layout -> Layout -> Layout   (* broken ident=2 *)

val commaListL            : Layout list -> Layout
val spaceListL            : Layout list -> Layout
val semiListL             : Layout list -> Layout
val sepListL              : Layout -> Layout list -> Layout

val bracketL              : Layout -> Layout
val tupleL                : Layout list -> Layout
val aboveL                : Layout -> Layout -> Layout
val aboveListL            : Layout list -> Layout

val optionL               : ('a -> Layout) -> 'a option -> Layout    
val listL                 : ('a -> Layout) -> 'a list   -> Layout

val linkL                 : string -> Layout -> Layout

val squashTo              : int -> Layout -> Layout

val showL                 : Layout -> string
val outL                  : TextWriter -> Layout -> unit
val bufferL               : StringBuilder -> Layout -> unit

(* render a Layout yielding an 'a using a 'b (hidden state) type *)
type ('a,'b) render =
    abstract Start    : unit -> 'b;
    abstract AddText  : 'b -> string -> 'b;
    abstract AddBreak : 'b -> int -> 'b;
    abstract AddTag   : 'b -> string * (string * string) list * bool -> 'b;
    abstract Finish   : 'b -> 'a

(* Run a render on a Layout *)      
val renderL  : ('b,'a) render -> Layout -> 'b

(* Primitive renders *)
val stringR  : (string,string list) render
type NoState = NoState
type NoResult = NoResult
val channelR : TextWriter -> (NoResult,NoState) render
val bufferR  : StringBuilder -> (NoResult,NoState) render

(* Combinator renders *)  
val htmlR    :        ('a,'b) render -> ('a,'b) render (* assumes in <pre> context *)
val indentR  : int -> ('a,'b) render -> ('a,'b) render
