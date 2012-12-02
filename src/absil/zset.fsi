(* (c) Microsoft Corporation. Apache 2.0 License  *)

namespace Microsoft.FSharp.Compiler.AbstractIL.Internal

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open System.Collections.Generic

/// Sets with a specific comparison function
type internal Zset<'T> = Internal.Utilities.Collections.Tagged.Set<'T>


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Zset = 

    val empty     : IComparer<'T> -> Zset<'T>
    val isEmpty  : Zset<'T> -> bool
    val contains  : 'T -> Zset<'T> -> bool
    val memberOf  : Zset<'T> -> 'T -> bool
    val add       : 'T -> Zset<'T> -> Zset<'T>
    val addList   : 'T list -> Zset<'T> -> Zset<'T>
    val addFlatList : FlatList<'T> -> Zset<'T> -> Zset<'T>
    val singleton : IComparer<'T> -> 'T -> Zset<'T>
    val remove    : 'T -> Zset<'T> -> Zset<'T>

    val count     : Zset<'T> -> int
    val union     : Zset<'T> -> Zset<'T> -> Zset<'T>
    val inter     : Zset<'T> -> Zset<'T> -> Zset<'T>
    val diff      : Zset<'T> -> Zset<'T> -> Zset<'T>
    val equal     : Zset<'T> -> Zset<'T> -> bool
    val subset    : Zset<'T> -> Zset<'T> -> bool
    val forall    : predicate:('T -> bool) -> Zset<'T> -> bool
    val exists    : predicate:('T -> bool) -> Zset<'T> -> bool
    val filter    : predicate:('T -> bool) -> Zset<'T> -> Zset<'T>   

    val fold      : ('T -> 'State -> 'State) -> Zset<'T> -> 'State -> 'State
    val iter      : ('T -> unit) -> Zset<'T> -> unit

    val elements  : Zset<'T> -> 'T list



