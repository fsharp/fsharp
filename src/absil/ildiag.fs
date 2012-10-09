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

/// Configurable AppDomain-global diagnostics channel for the Abstract IL library
///
/// REVIEW: review if we should just switch to System.Diagnostics
module internal Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 

open Internal.Utilities

open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 

let diagnosticsLog = ref (Some stdout)
let dflushn () = match !diagnosticsLog with None -> () | Some d -> d.WriteLine(); d.Flush()
let dflush () = match !diagnosticsLog with None -> () | Some d -> d.Flush()
let dprintn (s:string) = 
  match !diagnosticsLog with None -> () | Some d -> d.Write s; d.Write "\n"; dflush()

let dprintf (fmt: Format<_,_,_,_>) = 
    Printf.kfprintf dflush (match !diagnosticsLog with None -> System.IO.TextWriter.Null | Some d -> d) fmt

let dprintfn (fmt: Format<_,_,_,_>) = 
    Printf.kfprintf dflushn (match !diagnosticsLog with None -> System.IO.TextWriter.Null | Some d -> d) fmt

let setDiagnosticsChannel s = diagnosticsLog := s
