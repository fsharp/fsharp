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

/// Definitions internal for this library.
namespace Microsoft.FSharp.Primitives.Basics 

open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections

module internal List =
    val init : int -> (int -> 'T) -> 'T list
    val iter : ('T -> unit) -> 'T list -> unit
    val filter : predicate:('T -> bool) -> 'T list -> 'T list
    val collect : ('T -> 'U list) -> 'T list -> 'U list
    val partition : predicate:('T -> bool) -> 'T list -> 'T list * 'T list
    val map : mapping:('T -> 'U) -> 'T list -> 'U list
    val map2 : mapping:('T1 -> 'T2 -> 'U) -> 'T1 list -> 'T2 list -> 'U list
    val mapi : (int -> 'T -> 'U) -> 'T list -> 'U list
    val forall : predicate:('T -> bool) -> 'T list -> bool
    val exists : predicate:('T -> bool) -> 'T list -> bool
    val rev: 'T list -> 'T list
    val concat : seq<'T list> -> 'T list
    val iteri : action:(int -> 'T -> unit) -> 'T list -> unit
    val unzip : ('T1 * 'T2) list -> 'T1 list * 'T2 list
    val unzip3 : ('T1 * 'T2 * 'T3) list -> 'T1 list * 'T2 list * 'T3 list
    val zip : 'T1 list -> 'T2 list -> ('T1 * 'T2) list
    val zip3 : 'T1 list -> 'T2 list -> 'T3 list -> ('T1 * 'T2 * 'T3) list
    val ofArray : 'T[] -> 'T list
    val toArray : 'T list -> 'T[]
    val sortWith : ('T -> 'T -> int) -> 'T list -> 'T list

module internal Array =
    // The input parameter should be checked by callers if necessary
    val inline zeroCreateUnchecked : int -> 'T[]

    val inline init : int -> (int -> 'T) -> 'T[]

    val permute : indexMap:(int -> int) -> 'T[] -> 'T[]

    val unstableSortInPlaceBy: projection:('T -> 'Key) -> array:'T[] -> unit when 'Key : comparison 

    val unstableSortInPlace: array:'T[] -> unit when 'T : comparison 

    val stableSortInPlaceBy: projection:('T -> 'Key) -> array:'T[] -> unit when 'Key : comparison 

    val stableSortInPlace: array:'T[] -> unit when 'T : comparison 
