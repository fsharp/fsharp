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

namespace Microsoft.FSharp.Core

    open System
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Collections

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    /// <summary>Basic operations on options.</summary>
    module Option =

        /// <summary>Returns true if the option is not None.</summary>
        /// <param name="option">The input option.</param>
        /// <returns>True if the option is not None.</returns>
        [<CompiledName("IsSome")>]
        val isSome: option:'T option -> bool

        /// <summary>Returns true if the option is None.</summary>
        /// <param name="option">The input option.</param>
        /// <returns>True if the option is None.</returns>
        [<CompiledName("IsNone")>]
        val isNone: option:'T option -> bool

        /// <summary>Gets the value associated with the option.</summary>
        /// <param name="option">The input option.</param>
        /// <returns>The value within the option.</returns>
        /// <exception href="System.ArgumentException">Thrown when the option is None.</exception>
        [<CompiledName("GetValue")>]
        val get: option:'T option -> 'T

        /// <summary><c>count inp</c> evaluates to <c>match inp with None -> 0 | Some _ -> 1</c>.</summary>
        /// <param name="option">The input option.</param>
        /// <returns>A zero if the option is None, a one otherwise.</returns>
        [<CompiledName("Count")>]
        val count: option:'T option -> int

        /// <summary><c>fold f s inp</c> evaluates to <c>match inp with None -> s | Some x -> f s x</c>.</summary>
        /// <param name="folder">A function to update the state data when given a value from an option.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="option">The input option.</param>
        /// <returns>The original state if the option is None, otherwise it returns the updated state with the folder
        /// and the option value.</returns>
        [<CompiledName("Fold")>]
        val fold<'T,'State> : folder:('State -> 'T -> 'State) -> state:'State -> option:'T option -> 'State

        /// <summary><c>fold f inp s</c> evaluates to <c>match inp with None -> s | Some x -> f x s</c>.</summary>
        /// <param name="folder">A function to update the state data when given a value from an option.</param>
        /// <param name="option">The input option.</param>
        /// <param name="state">The initial state.</param>
        /// <returns>The original state if the option is None, otherwise it returns the updated state with the folder
        /// and the option value.</returns>
        [<CompiledName("FoldBack")>]
        val foldBack<'T,'State> : folder:('T -> 'State -> 'State) -> option:'T option -> state:'State -> 'State

        /// <summary><c>exists p inp</c> evaluates to <c>match inp with None -> false | Some x -> p x</c>.</summary>
        /// <param name="predicate">A function that evaluates to a boolean when given a value from the option type.</param>
        /// <param name="option">The input option.</param>
        /// <returns>False if the option is None, otherwise it returns the result of applying the predicate
        /// to the option value.</returns>
        [<CompiledName("Exists")>]
        val exists: predicate:('T -> bool) -> option:'T option -> bool

        /// <summary><c>forall p inp" evaluates to "match inp with None -> true | Some x -> p x</c>.</summary>
        /// <param name="predicate">A function that evaluates to a boolean when given a value from the option type.</param>
        /// <param name="option">The input option.</param>
        /// <returns>True if the option is None, otherwise it returns the result of applying the predicate
        /// to the option value.</returns>
        [<CompiledName("ForAll")>]
        val forall: predicate:('T -> bool) -> option:'T option -> bool

        /// <summary><c>iter f inp</c> executes <c>match inp with None -> () | Some x -> f x</c>.</summary>
        /// <param name="action">A function to apply to the option value.</param>
        /// <param name="option">The input option.</param>
        /// <returns>Unit if the option is None, otherwise it returns the result of applying the predicate
        /// to the option value.</returns>
        [<CompiledName("Iterate")>]
        val iter: action:('T -> unit) -> option:'T option -> unit

        /// <summary><c>map f inp</c> evaluates to <c>match inp with None -> None | Some x -> Some (f x)</c>.</summary>
        /// <param name="mapping">A function to apply to the option value.</param>
        /// <param name="option">The input option.</param>
        /// <returns>An option of the input value after applying the mapping function, or None if the input is None.</returns>
        [<CompiledName("Map")>]
        val map: mapping:('T -> 'U) -> option:'T option -> 'U option

        /// <summary><c>bind f inp</c> evaluates to <c>match inp with None -> None | Some x -> f x</c></summary>
        /// <param name="binder">A function that takes the value of type T from an option and transforms it into
        /// an option containing a value of type U.</param>
        /// <param name="option">The input option.</param>
        /// <returns>An option of the output type of the binder.</returns>
        [<CompiledName("Bind")>]
        val bind: binder:('T -> 'U option) -> option:'T option -> 'U option


        /// <summary>Convert the option to an array of length 0 or 1.</summary>
        /// <param name="option">The input option.</param>
        /// <returns>The result array.</returns>
        [<CompiledName("ToArray")>]
        val toArray: option:'T option -> 'T[]

        /// <summary>Convert the option to a list of length 0 or 1.</summary>
        /// <param name="option">The input option.</param>
        /// <returns>The result list.</returns>
        [<CompiledName("ToList")>]
        val toList: option:'T option -> 'T list


