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

module internal Microsoft.FSharp.Compiler.Outcome

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

val success : 'T -> ResultOrException<'T>
val raze : exn -> ResultOrException<'T>
val ( ||?> ) : ResultOrException<'T> -> ('T -> ResultOrException<'U>) -> ResultOrException<'U>
val ( |?> ) : ResultOrException<'T> -> ('T -> 'U) -> ResultOrException<'U>
val ForceRaise : ResultOrException<'T> -> 'T
val otherwise : (unit -> ResultOrException<'T>) -> ResultOrException<'T> -> ResultOrException<'T>
