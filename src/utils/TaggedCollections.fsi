//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


/// This namespace contains FSharp.PowerPack extensions for the F# collection types
namespace Internal.Utilities.Collections.Tagged

    open System
    open System.Collections.Generic

    /// Immutable sets based on binary trees, default tag

    /// Immutable sets where a constraint tag carries information about the class of key-comparer being used.  
    [<Sealed>]
    type internal Set<'T,'ComparerTag> when 'ComparerTag :> IComparer<'T> =

        /// A useful shortcut for Set.add.  Note this operation prodcues a new set
        /// and does not mutate the original set.  The new set will share many storage
        /// nodes with the original.  See the Set module for further operations on sets.
        member Add : 'T -> Set<'T,'ComparerTag>
        
        /// A useful shortcut for Set.remove.  Note this operation produces a new set
        /// and does not mutate the original set.  The new set will share many storage
        /// nodes with the original.  See the Set module for further operations on sets.
        member Remove : 'T -> Set<'T,'ComparerTag>
        
        /// Return the number of elements in the set
        member Count : int
        
        /// A useful shortcut for Set.contains.  See the Set module for further operations on sets.
        member Contains : 'T -> bool
        
        /// A useful shortcut for Set.isEmpty.  See the Set module for further operations on sets.
        member IsEmpty  : bool

        /// Apply the given function to each binding in the collection
        member Iterate : ('T -> unit) -> unit

        /// Apply the given accumulating function to all the elements of the set
        member Fold    : ('T -> 'State -> 'State) -> 'State -> 'State

        /// Build two new sets, one containing the elements for which the given predicate returns 'true',
        /// and the other the remaining elements.
        member Partition: predicate:('T -> bool) -> Set<'T,'ComparerTag> * Set<'T,'ComparerTag>

        /// Return a new collection containing only the elements of the collection
        /// for which the given predicate returns "true"
        member Filter: predicate:('T -> bool) -> Set<'T,'ComparerTag> 

        /// Test if any element of the collection satisfies the given predicate.
        /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
        /// <c>p i0 or ... or p iN</c>.
        member Exists: predicate:('T -> bool) -> bool

        /// Test if all elements of the collection satisfy the given predicate.
        /// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and <c>j0...jN</c> then 
        /// computes <c>p i0 && ... && p iN</c>.
        member ForAll: predicate:('T -> bool) -> bool

        /// A set based on the given comparer containing the given initial elements
        static member Create: 'ComparerTag * seq<'T> -> Set<'T,'ComparerTag> 
        
        /// The empty set based on the given comparer
        static member Empty: 'ComparerTag -> Set<'T,'ComparerTag> 
        
        /// A singleton set based on the given comparison operator
        static member Singleton: 'ComparerTag * 'T -> Set<'T,'ComparerTag> 
        
        /// Compares two sets and returns true if they are equal or false otherwise
        static member Equality : Set<'T,'ComparerTag> * Set<'T,'ComparerTag> -> bool
        
        /// Compares a and b and returns 1 if a > b, -1 if b < a and 0 if a = b        
        static member Compare : a:Set<'T,'ComparerTag> * b:Set<'T,'ComparerTag> -> int

        /// Return a new set with the elements of the second set removed from the first.
        static member (-) : Set<'T,'ComparerTag> * Set<'T,'ComparerTag> -> Set<'T,'ComparerTag> 

        /// Compute the union of the two sets.
        static member (+) : Set<'T,'ComparerTag> * Set<'T,'ComparerTag> -> Set<'T,'ComparerTag> 

        /// Compute the intersection of the two sets.
        static member Intersection : Set<'T,'ComparerTag> * Set<'T,'ComparerTag> -> Set<'T,'ComparerTag> 

        /// Compute the union of the two sets.
        static member Union : Set<'T,'ComparerTag> * Set<'T,'ComparerTag> -> Set<'T,'ComparerTag>

        /// Return a new set with the elements of the second set removed from the first.
        static member Difference: Set<'T,'ComparerTag> * Set<'T,'ComparerTag> -> Set<'T,'ComparerTag> 

        /// The number of elements in the set
        member Choose : 'T 

        /// Returns the lowest element in the set according to the ordering being used for the set
        member MinimumElement: 'T

        /// Returns the highest element in the set according to the ordering being used for the set
        member MaximumElement: 'T

        /// Evaluates to "true" if all elements of the second set are in the first
        member IsSubsetOf: Set<'T,'ComparerTag> -> bool

        /// Evaluates to "true" if all elements of the first set are in the second
        member IsSupersetOf: Set<'T,'ComparerTag> -> bool

        /// The elements of the set as a list.
        member ToList : unit -> 'T list
        
        /// The elements of the set as an array.
        member ToArray: unit -> 'T array 

        interface ICollection<'T> 
        interface IEnumerable<'T> 
        interface System.Collections.IEnumerable

        interface System.IComparable
        override Equals : obj -> bool

    type internal Set<'T> = Set<'T, IComparer<'T>>    

    /// Immutable maps.  Keys are ordered by construction function specified
    /// when creating empty maps or by F# structural comparison if no
    /// construction function is specified.
    ///
    /// <performance> 
    ///   Maps based on structural comparison are  
    ///   efficient for small keys. They are not a suitable choice if keys are recursive data structures 
    ///   or require non-structural comparison semantics.
    /// </performance>

    /// Immutable maps.  A constraint tag carries information about the class of key-comparers being used.  
    [<Sealed>]
    type internal Map<'Key,'Value,'ComparerTag>  when 'ComparerTag :> IComparer<'Key> =
        /// Return a new map with the binding added to the given map.
        member Add: 'Key * 'Value -> Map<'Key,'Value,'ComparerTag>

        /// Return true if there are no bindings in the map.
        member IsEmpty: bool
        
        //member Comparer : 'ComparerTag

        /// The empty map, and use the given comparer comparison function for all operations associated
        /// with any maps built from this map.
        static member Empty: 'ComparerTag -> Map<'Key,'Value,'ComparerTag>

        static member FromList : 'ComparerTag * ('Key * 'Value) list -> Map<'Key,'Value,'ComparerTag>

        /// Build a map that contains the bindings of the given IEnumerable
        /// and where comparison of elements is based on the given comparison function
        static member Create: 'ComparerTag * seq<'Key * 'Value> -> Map<'Key,'Value,'ComparerTag> 

        /// Test is an element is in the domain of the map
        member ContainsKey: 'Key -> bool

        /// The number of bindings in the map
        member Count: int

        /// Lookup an element in the map. Raise <c>KeyNotFoundException</c> if no binding
        /// exists in the map.
        member Item : 'Key -> 'Value with get

        /// Search the map looking for the first element where the given function returns a <c>Some</c> value
        member First: ('Key -> 'Value -> 'T option) -> 'T option

        /// Return true if the given predicate returns true for all of the
        /// bindings in the map. Always returns true if the map is empty.
        member ForAll: ('Key -> 'Value -> bool) -> bool

        /// Return true if the given predicate returns true for one of the
        /// bindings in the map. Always returns false if the map is empty.
        member Exists: ('Key -> 'Value -> bool) -> bool

        /// Build a new map containing the bindings for which the given predicate returns 'true'.
        member Filter: ('Key -> 'Value -> bool) -> Map<'Key,'Value,'ComparerTag> 

        /// Fold over the bindings in the map.  
        member Fold: folder:('Key -> 'Value -> 'State -> 'State) -> 'State -> 'State

        /// Given the start and end points of a key range,
        /// Fold over the bindings in the map that are in the range,
        /// and the end points are included if present (the range is considered a closed interval).
        member FoldSection: 'Key -> 'Key -> ('Key -> 'Value -> 'State -> 'State) -> 'State -> 'State

        /// Fold over the bindings in the map.  
        member FoldAndMap: ('Key -> 'Value -> 'State -> 'T * 'State) -> 'State -> Map<'Key,'T,'ComparerTag> * 'State

        /// Apply the given function to each binding in the dictionary
        member Iterate: action:('Key -> 'Value -> unit) -> unit

        /// Build a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection. The index passed to the
        /// function indicates the index of element being transformed.
        member Map: mapping:('Key -> 'Value -> 'T) -> Map<'Key,'T,'ComparerTag>

        /// Build a new collection whose elements are the results of applying the given function
        /// to each of the elements of the collection.
        member MapRange: mapping:('Value -> 'T) -> Map<'Key,'T,'ComparerTag>

        /// Build two new maps, one containing the bindings for which the given predicate returns 'true',
        /// and the other the remaining bindings.
        member Partition: ('Key -> 'Value -> bool) -> Map<'Key,'Value,'ComparerTag> * Map<'Key,'Value,'ComparerTag>

        /// Remove an element from the domain of the map.  No exception is raised if the element is not present.
        member Remove: 'Key -> Map<'Key,'Value,'ComparerTag>

        /// Lookup an element in the map, returning a <c>Some</c> value if the element is in the domain 
        /// of the map and <c>None</c> if not.
        member TryFind: 'Key -> 'Value option

        /// The elements of the set as a list.
        member ToList : unit -> ('Key * 'Value) list
    
        /// The elements of the set as an array
        member ToArray: unit -> ('Key * 'Value) array 

        interface IEnumerable<KeyValuePair<'Key, 'Value>>
        
        interface System.Collections.IEnumerable 
        interface System.IComparable
        override Equals : obj -> bool

    type internal Map<'Key,'Value> = Map<'Key, 'Value, IComparer<'Key>>    

