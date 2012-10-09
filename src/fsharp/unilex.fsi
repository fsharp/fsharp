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

module Microsoft.FSharp.Compiler.UnicodeLexing

open Microsoft.FSharp.Text
open Internal.Utilities.Text.Lexing

type Lexbuf =  LexBuffer<char>
val internal StringAsLexbuf : string -> Lexbuf
val public FunctionAsLexbuf : (char [] * int * int -> int) -> Lexbuf
val public UnicodeFileAsLexbuf :string * int option * (*retryLocked*) bool -> Lexbuf
