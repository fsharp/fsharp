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

/// Diagnostics from the AbsIL toolkit. You can reset the diagnostics 
/// stream to point elsewhere, or turn it
/// off altogether by setting it to 'None'.  The logging channel initally
/// points to stderr.  All functions call flush() automatically.
///
/// REVIEW: review if we should just switch to System.Diagnostics
module internal Microsoft.FSharp.Compiler.AbstractIL.Diagnostics

open System.IO
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Core.Printf

val public setDiagnosticsChannel: TextWriter option -> unit

val public dprintfn: TextWriterFormat<'a> -> 'a 
val public dprintf: TextWriterFormat<'a> -> 'a 

val public dprintn: string -> unit

