(*

Copyright 2002-2012 Microsoft Corporation
Copyright 2013 Jack Pappas

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

(* NOTE :   This file has been heavily modified from the original version which
            shipped in the F# 3.0 code drop. However, it is still a drop-in replacement
            for that code. *)

namespace Microsoft.FSharp.Collections

open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections
open System.Collections
open System.Collections.Generic
open System.Diagnostics
//open System.Diagnostics.Contracts
open Microsoft.FSharp.Core.OptimizedClosures


[<AutoOpen>]
module private SetConstants =
    //
    let [<Literal>] defaultStackCapacity = 16
    //
    let [<Literal>] debugViewMaxElementCount = 1000


/// SetTree which serves as the internal representation of the Set type.
[<NoEquality; NoComparison>]
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type internal SetTree<'T when 'T : comparison> =
    /// Empty tree.
    | Empty
    /// Node.
    // Left-Child, Right-Child, Value, Height
    | Node of SetTree<'T> * SetTree<'T> * 'T * uint32

    //
    static member private CompareStacks (comparer : IComparer<'T>, l1 : SetTree<'T> list, l2 : SetTree<'T> list) : int =
        match l1, l2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        
        // OPTIMIZATION : If two trees are identical, there's no need to compare them.
        | t1 :: l1, t2 :: l2
            when System.Object.ReferenceEquals (t1, t2) ->
            // Continue comparing the lists.
            SetTree.CompareStacks (comparer, l1, l2)
        
        | (Empty :: t1), (Empty :: t2) ->
            SetTree.CompareStacks (comparer, t1, t2)
        | (Node (Empty, Empty, n1k, _) :: t1), (Node (Empty, Empty, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, t1, t2)
            | c -> c

        | (Node (Empty, Empty, n1k, _) :: t1), (Node (Empty, n2r, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, Empty :: t1, n2r :: t2)
            | c -> c

        | (Node (Empty, n1r, n1k, _) :: t1), (Node (Empty, Empty, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, n1r :: t1, Empty :: t2)
            | c -> c

        | (Node (Empty, n1r, n1k, _) :: t1), (Node (Empty, n2r, n2k, _) :: t2) ->
            match comparer.Compare (n1k, n2k) with
            | 0 ->
                SetTree.CompareStacks (comparer, n1r :: t1, n2r :: t2)
            | c -> c

        | ((Node (Empty, Empty, n1k, _) :: t1) as l1), _ ->
            SetTree.CompareStacks (comparer, Empty :: l1, l2)
        
        | (Node (n1l, n1r, n1k, _) :: t1), _ ->
            SetTree.CompareStacks (comparer, n1l :: Node (Empty, n1r, n1k, 0u) :: t1, l2)
        
        | _, ((Node (Empty, Empty, n2k, _) :: t2) as l2) ->
            SetTree.CompareStacks (comparer, l1, Empty :: l2)
        
        | _, (Node (n2l, n2r, n2k, _) :: t2) ->
            SetTree.CompareStacks (comparer, l1, n2l :: Node (Empty, n2r, n2k, 0u) :: t2)
                
    //
    static member Compare (comparer : IComparer<'T>, s1 : SetTree<'T>, s2 : SetTree<'T>) : int =
        // OPTIMIZATION : If two trees are identical, there's no need to compare them.
        if System.Object.ReferenceEquals (s1, s2) then 0
        else
            match s1, s2 with
            | Empty, Empty -> 0
            | Empty, _ -> -1
            | _, Empty -> 1
            | _ ->
                SetTree<'T>.CompareStacks (comparer, [s1], [s2])

    /// Computes the height of a SetTree (rather than returning the height value stored in it's root).
    //[<Pure>]
    static member private ComputeHeight (tree : SetTree<'T>) =
        match tree with
        | Empty -> 0u
        | Node (l, r, _, _) ->
            1u + max (SetTree.ComputeHeight l) (SetTree.ComputeHeight r)
        
    /// Determines if a SetTree is correctly formed, i.e., it respects the AVL balancing rules.
    //[<Pure; ContractInvariantMethod>]
    static member private AvlInvariant (tree : SetTree<'T>) =
        match tree with
        | Empty -> true
        | Node (l, r, _, h) ->
            let height_l = SetTree.ComputeHeight l
            let height_r = SetTree.ComputeHeight r
            height_l = height_r
            || (height_l = (1u + height_r) || height_r = (1u + height_l))
            && h = ((max height_l height_r) + 1u)
            && SetTree.AvlInvariant l
            && SetTree.AvlInvariant r

    /// Returns the height of a SetTree.
    //[<Pure>]
    static member (*inline*) Height (tree : SetTree<'T>) =
        match tree with
        | Empty -> 0u
        | Node (_,_,_,h) -> h

    /// Returns the absolute difference in heights between two SetTrees.
    //[<Pure>]
    static member private HeightDiff (t1, t2 : SetTree<'T>) =
        (max (SetTree.Height t1) (SetTree.Height t2)) - (min (SetTree.Height t1) (SetTree.Height t2))

    /// Determines if a SetTree is empty.
    //[<Pure>]
    static member (*inline*) IsEmptyTree (tree : SetTree<'T>) =
        match tree with
        | Empty -> true
        | Node (_,_,_,_) -> false

    /// Gets the maximum (greatest) value stored in the SetTree.
    static member MaxElement (tree : SetTree<'T>) =
        match tree with
        | Empty ->
            invalidArg "tree" "The tree is empty."
        | Node (_, Empty, n, _) ->
            n
        | Node (_, right, _, _) ->
            SetTree.MaxElement right

    /// Gets the minimum (least) value stored in the SetTree.
    static member MinElement (tree : SetTree<'T>) =
        match tree with
        | Empty ->
            invalidArg "tree" "The tree is empty."
        | Node (Empty, _, n, _) ->
            n
        | Node (left, _, _, _) ->
            SetTree.MinElement left

    /// Determines if a SetTree contains a specified value.
    //[<Pure>]
    static member Contains (comparer : IComparer<'T>, tree : SetTree<'T>, value : 'T) =
        match tree with
        | Empty ->
            false
        | Node (l, r, n, _) ->
            let comparison = comparer.Compare (value, n)
            if comparison = 0 then      // value = n
                true
            elif comparison < 0 then    // value < n
                SetTree.Contains (comparer, l, value)
            else                        // value > n
                SetTree.Contains (comparer, r, value)

    /// Creates a SetTree whose root node holds the specified value
    /// and the specified left and right subtrees.
    static member inline Create (value, l, r : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing
        Node (l, r, value, (max (SetTree.Height l) (SetTree.Height r)) + 1u)

    /// Creates a SetTree containing the specified value.
    static member Singleton value : SetTree<'T> =
        SetTree.Create (value, Empty, Empty)

    //
    static member private mkt_bal_l (n, l, r : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing

        if SetTree.Height l = SetTree.Height r + 2u then
            match l with
            | Empty ->
                failwith "mkt_bal_l"
            | Node (ll, lr, ln, _) ->
                if SetTree.Height ll < SetTree.Height lr then
                    match lr with
                    | Empty ->
                        failwith "mkt_bal_l"
                    | Node (lrl, lrr, lrn, _) ->
                        SetTree.Create (lrn, SetTree.Create (ln, ll, lrl), SetTree.Create (n, lrr, r))
                else
                    SetTree.Create (ln, ll, SetTree.Create (n, lr, r))
        else
            SetTree.Create (n, l, r)

    //
    static member private mkt_bal_r (n, l, r : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing

        if SetTree.Height r = SetTree.Height l + 2u then
            match r with
            | Empty ->
                failwith "mkt_bal_r"
            | Node (rl, rr, rn, _) ->
                if SetTree.Height rr < SetTree.Height rl then
                    match rl with
                    | Empty ->
                        failwith "mkt_bal_r"
                    | Node (rll, rlr, rln, _) ->
                        SetTree.Create (rln, SetTree.Create (n, l, rll), SetTree.Create (rn, rlr, rr))
                else
                    SetTree.Create (rn, SetTree.Create (n, l, rl), rr)
        else
            SetTree.Create (n, l, r)

(* NOTE :   The DeleteMin, DeleteMax, DeleteRoot, TryDeleteMin, and TryDeleteMax methods use the
            'private' modifier here because they're not implemented/exposed by the standard F# Set type,
            but some of them are used by this implementation.
            If you are using a custom FSharp.Core implementation and would like to expose these
            functions for use in your own code, just remove the 'private' modifiers. *)

    /// Removes the minimum (least) value from an SetTree,
    /// returning the value along with the updated tree.
    static member private DeleteMin (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            invalidArg "tree" "Cannot delete the minimum value from an empty tree."
        | Node (l, Empty, n, _) ->
            n, l
        | Node (left, r, n, _) ->
            let na, l = SetTree.DeleteMin left
            na, SetTree.mkt_bal_r (n, l, r)

    /// Removes the maximum (greatest) value from an SetTree,
    /// returning the value along with the updated tree.
    static member private DeleteMax (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            invalidArg "tree" "Cannot delete the maximum value from an empty tree."
        | Node (l, Empty, n, _) ->
            n, l
        | Node (l, right, n, _) ->
            let na, r = SetTree.DeleteMax right
            na, SetTree.mkt_bal_l (n, l, r)

    /// Removes the root (median) value from an SetTree,
    /// returning the value along with the updated tree.
    static member private DeleteRoot (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            invalidArg "tree" "Cannot delete the root of an empty tree."
        | Node (Empty, r, _, _) -> r
        | Node (left, Empty, _, _) ->
            left
        | Node (left, r, _, _) ->
            let root, l = SetTree.DeleteMax left
            SetTree.mkt_bal_r (root, l, r)

    /// Removes the minimum (least) value from a SetTree,
    /// returning the value along with the updated tree.
    /// No exception is thrown if the tree is empty.
    static member private TryDeleteMin (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            None, tree
        | Node (l, Empty, n, _) ->
            Some n, l
        | Node (left, r, n, _) ->
            let na, l = SetTree.TryDeleteMin left
            match na with
            | None ->
                na, l
            | Some _ ->
                na, SetTree.mkt_bal_r (n, l, r)

    /// Removes the maximum (greatest) value from a SetTree,
    /// returning the value along with the updated tree.
    /// No exception is thrown if the tree is empty.
    static member private TryDeleteMax (tree : SetTree<'T>) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            None, tree
        | Node (l, Empty, n, _) ->
            Some n, l
        | Node (l, right, n, _) ->
            let na, r = SetTree.TryDeleteMax right
            match na with
            | None ->
                na, l
            | Some _ ->
                na, SetTree.mkt_bal_l (n, l, r)

    /// Removes the specified value from the tree.
    /// If the tree doesn't contain the value, no exception is thrown;
    /// the tree will be returned without modification.
    static member Delete (comparer : IComparer<'T>, tree : SetTree<'T>, value : 'T) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            Empty
        | Node (l, r, n, _) ->
            let comparison = comparer.Compare (value, n)
            if comparison = 0 then
                // x = n
                SetTree.DeleteRoot tree
            elif comparison < 0 then
                // x < n
                let l' = SetTree.Delete (comparer, l, value)

                // Only rebalance the tree if an element was actually deleted.
                if System.Object.ReferenceEquals (l', l) then tree
                else SetTree.mkt_bal_r (n, l', r)
            else
                // x > n
                let r' = SetTree.Delete (comparer, r, value)
                
                // Only rebalance the tree if an element was actually deleted.
                if System.Object.ReferenceEquals (r', r) then tree
                else SetTree.mkt_bal_l (n, l, r')

    /// Adds a value to a SetTree.
    /// If the tree already contains the value, no exception is thrown;
    /// the tree will be returned without modification.
    static member Insert (comparer : IComparer<'T>, tree : SetTree<'T>, value : 'T) =
        // Preconditions
        // TODO : Add assertions for debugging/testing.

        match tree with
        | Empty ->
            Node (Empty, Empty, value, 1u)
        | Node (l, r, n, _) ->
            let comparison = comparer.Compare (value, n)
            if comparison = 0 then
                // x = n
                tree
            elif comparison < 0 then
                // x < n
                let l' = SetTree.Insert (comparer, l, value)

                // Only rebalance the tree if an element was actually inserted.
                if System.Object.ReferenceEquals (l', l) then tree
                else SetTree.mkt_bal_l (n, l', r)
            else
                // x > n
                let r' = SetTree.Insert (comparer, r, value)
                
                // Only rebalance the tree if an element was actually inserted.
                if System.Object.ReferenceEquals (r', r) then tree
                else SetTree.mkt_bal_r (n, l, r')

    /// Counts the number of elements in the tree.
    static member Count (tree : SetTree<'T>) =
        match tree with
        | Empty -> 0u
        | Node (Empty, Empty, _, _) -> 1u
        | Node (l, r, _, _) ->
            /// Mutable stack. Holds the trees which still need to be traversed.
            let stack = Stack<SetTree<'T>> (defaultStackCapacity)

            /// The number of elements discovered in the tree so far.
            let mutable count = 1u   // Start at one (1) to include this root node.

            // Traverse the tree using the mutable stack, incrementing the counter at each node.
            stack.Push r
            stack.Push l

            while stack.Count > 0 do
                match stack.Pop () with
                | Empty -> ()
                (* OPTIMIZATION: Handle a few of the cases specially here to avoid pushing empty
                   nodes on the stack. *)
                | Node (Empty, Empty, _, _) ->
                    // Increment the element count.
                    count <- count + 1u

                | Node (Empty, z, _, _)
                | Node (z, Empty, _, _) ->
                    // Increment the element count.
                    count <- count + 1u

                    // Push the non-empty child onto the stack.
                    stack.Push z

                | Node (l, r, _, _) ->
                    // Increment the element count.
                    count <- count + 1u

                    // Push the children onto the stack.
                    stack.Push r
                    stack.Push l

            // Return the element count.
            count

    //
    static member Iter (action : 'T -> unit) (tree : SetTree<'T>) : unit =
        match tree with
        | Empty -> ()
        | Node (Empty, Empty, x, _) ->
            // Invoke the action with this single element.
            action x
        | Node (l, r, x, _) ->
            /// Mutable stack. Holds the trees which still need to be traversed.
            let stack = Stack<SetTree<'T>> (defaultStackCapacity)

            // Traverse the tree using the mutable stack, applying the folder function to
            // each value to update the state value.
            stack.Push r
            stack.Push <| SetTree.Singleton x
            stack.Push l

            while stack.Count > 0 do
                match stack.Pop () with
                | Empty -> ()
                | Node (Empty, Empty, x, _) ->
                    // Apply this value to the action function.
                    action x

                | Node (Empty, z, x, _) ->
                    // Apply this value to the action function.
                    action x

                    // Push the non-empty child onto the stack.
                    stack.Push z

                | Node (l, r, x, _) ->
                    // Push the children onto the stack.
                    // Also push a new Node onto the stack which contains the value from
                    // this Node, so it'll be processed in the correct order.
                    stack.Push r
                    stack.Push <| SetTree.Singleton x
                    stack.Push l

    /// Applies the given accumulating function to all elements in a SetTree.
    static member Fold (folder : 'State -> 'T -> 'State) (state : 'State) (tree : SetTree<'T>) =
        match tree with
        | Empty -> state
        | Node (Empty, Empty, x, _) ->
            // Invoke the folder function on this single element and return the result.
            folder state x
        | Node (l, r, x, _) ->
            // Adapt the folder function since we'll always supply all of the arguments at once.
            let folder = FSharpFunc<_,_,_>.Adapt folder

            /// Mutable stack. Holds the trees which still need to be traversed.
            let stack = Stack<SetTree<'T>> (defaultStackCapacity)

            /// The current state value.
            let mutable state = state

            // Traverse the tree using the mutable stack, applying the folder function to
            // each value to update the state value.
            stack.Push r
            stack.Push <| SetTree.Singleton x
            stack.Push l

            while stack.Count > 0 do
                match stack.Pop () with
                | Empty -> ()
                | Node (Empty, Empty, x, _) ->
                    // Apply this value to the folder function.
                    state <- folder.Invoke (state, x)

                | Node (Empty, z, x, _) ->
                    // Apply this value to the folder function.
                    state <- folder.Invoke (state, x)

                    // Push the non-empty child onto the stack.
                    stack.Push z

                | Node (l, r, x, _) ->
                    // Push the children onto the stack.
                    // Also push a new Node onto the stack which contains the value from
                    // this Node, so it'll be processed in the correct order.
                    stack.Push r
                    stack.Push <| SetTree.Singleton x
                    stack.Push l

            // Return the final state value.
            state

    /// Applies the given accumulating function to all elements in a SetTree.
    static member FoldBack (folder : 'T -> 'State -> 'State) (state : 'State) (tree : SetTree<'T>) =
        match tree with
        | Empty -> state
        | Node (Empty, Empty, x, _) ->
            // Invoke the folder function on this single element and return the result.
            folder x state
        | Node (l, r, x, _) ->
            // Adapt the folder function since we'll always supply all of the arguments at once.
            let folder = FSharpFunc<_,_,_>.Adapt folder

            /// Mutable stack. Holds the trees which still need to be traversed.
            let stack = Stack<SetTree<'T>> (defaultStackCapacity)

            /// The current state value.
            let mutable state = state

            // Traverse the tree using the mutable stack, applying the folder function to
            // each value to update the state value.
            stack.Push l
            stack.Push <| SetTree.Singleton x
            stack.Push r

            while stack.Count > 0 do
                match stack.Pop () with
                | Empty -> ()
                | Node (Empty, Empty, x, _) ->
                    // Apply this value to the folder function.
                    state <- folder.Invoke (x, state)

                | Node (z, Empty, x, _) ->
                    // Apply this value to the folder function.
                    state <- folder.Invoke (x, state)

                    // Push the non-empty child onto the stack.
                    stack.Push z

                | Node (l, r, x, _) ->
                    // Push the children onto the stack.
                    // Also push a new Node onto the stack which contains the value from
                    // this Node, so it'll be processed in the correct order.
                    stack.Push l
                    stack.Push <| SetTree.Singleton x
                    stack.Push r

            // Return the final state value.
            state

    /// Tests if any element of the collection satisfies the given predicate.
    static member Exists (predicate : 'T -> bool) (tree : SetTree<'T>) : bool =
        match tree with
        | Empty -> false
        | Node (Empty, Empty, x, _) ->
            // Apply the predicate function to this element and return the result.
            predicate x
        | Node (l, r, x, _) ->
            /// Mutable stack. Holds the trees which still need to be traversed.
            let stack = Stack<SetTree<'T>> (defaultStackCapacity)

            /// Have we found a matching element?
            let mutable foundMatch = false

            // Traverse the tree using the mutable stack, applying the folder function to
            // each value to update the state value.
            stack.Push r
            stack.Push <| SetTree.Singleton x
            stack.Push l

            while stack.Count > 0 && not foundMatch do
                match stack.Pop () with
                | Empty -> ()
                | Node (Empty, Empty, x, _) ->
                    // Apply the predicate to this element.
                    foundMatch <- predicate x

                | Node (Empty, z, x, _) ->
                    // Apply the predicate to this element.
                    foundMatch <- predicate x

                    // Push the non-empty child onto the stack.
                    stack.Push z

                | Node (l, r, x, _) ->
                    // Push the children onto the stack.
                    // Also push a new Node onto the stack which contains the value from
                    // this Node, so it'll be processed in the correct order.
                    stack.Push r
                    stack.Push <| SetTree.Singleton x
                    stack.Push l

            // Return the value indicating whether or not a matching element was found.
            foundMatch

    /// Tests if all elements of the collection satisfy the given predicate.
    static member Forall (predicate : 'T -> bool) (tree : SetTree<'T>) : bool =
        match tree with
        | Empty -> true
        | Node (Empty, Empty, x, _) ->
            // Apply the predicate function to this element and return the result.
            predicate x
        | Node (l, r, x, _) ->
            /// Mutable stack. Holds the trees which still need to be traversed.
            let stack = Stack<SetTree<'T>> (defaultStackCapacity)

            /// Have all of the elements we've seen so far matched the predicate?
            let mutable allElementsMatch = true

            // Traverse the tree using the mutable stack, applying the folder function to
            // each value to update the state value.
            stack.Push r
            stack.Push <| SetTree.Singleton x
            stack.Push l

            while stack.Count > 0 && allElementsMatch do
                match stack.Pop () with
                | Empty -> ()
                | Node (Empty, Empty, x, _) ->
                    // Apply the predicate to this element.
                    allElementsMatch <- predicate x

                | Node (Empty, z, x, _) ->
                    // Apply the predicate to this element.
                    allElementsMatch <- predicate x

                    // Push the non-empty child onto the stack.
                    stack.Push z

                | Node (l, r, x, _) ->
                    // Push the children onto the stack.
                    // Also push a new Node onto the stack which contains the value from
                    // this Node, so it'll be processed in the correct order.
                    stack.Push r
                    stack.Push <| SetTree.Singleton x
                    stack.Push l

            // Return the value indicating if all elements matched the predicate.
            allElementsMatch

    /// Builds a new SetTree from the elements of a sequence.
    static member OfSeq (comparer : IComparer<'T>, sequence : seq<'T>) : SetTree<'T> =
        (Empty, sequence)
        ||> Seq.fold (fun tree el ->
            SetTree.Insert (comparer, tree, el))

    /// Builds a new SetTree from the elements of an list.
    static member OfList (comparer : IComparer<'T>, list : 'T list) : SetTree<'T> =
        (Empty, list)
        ||> List.fold (fun tree el ->
            SetTree.Insert (comparer, tree, el))

    /// Builds a new SetTree from the elements of an array.
    static member OfArray (comparer : IComparer<'T>, array : 'T[]) : SetTree<'T> =
        (Empty, array)
        ||> Array.fold (fun tree el ->
            SetTree.Insert (comparer, tree, el))

    (* NOTE : This works, but has been disabled for now because the existing F# Set
                implementation uses a custom IEnumerator implementation which has different
                characteristics; the unit tests expect to see these, so that implementation
                is used instead of this one (at least for now). *)
//    /// Returns a sequence containing the elements stored
//    /// in a SetTree, ordered from least to greatest.
//    static member ToSeq (tree : SetTree<'T>) =
//        seq {
//        match tree with
//        | Empty -> ()
//        | Node (l, r, n, _) ->
//            yield! SetTree.ToSeq l
//            yield n
//            yield! SetTree.ToSeq r
//        }

    /// Returns a list containing the elements stored in
    /// a SetTree, ordered from least to greatest. 
    static member ToList (tree : SetTree<'T>) =
        ([], tree)
        ||> SetTree.FoldBack (fun el lst ->
            el :: lst)

    /// Returns an array containing the elements stored in
    /// a SetTree, ordered from least to greatest.
    static member ToArray (tree : SetTree<'T>) =
        let elements = ResizeArray ()
        SetTree.Iter elements.Add tree
        elements.ToArray ()

    //
    // TODO : This could be replaced by 'mkt_bal_l' and 'mkt_bal_r'.
    static member private Rebalance (t1, t2, k) : SetTree<'T> =
        let t1h = SetTree.Height t1
        let t2h = SetTree.Height t2
        if t2h > t1h + 2u then // right is heavier than left
            match t2 with
            | Node (t2l, t2r, t2k, _) ->
                // one of the nodes must have height > height t1 + 1
                if SetTree.Height t2l > t1h + 1u then  // balance left: combination
                    match t2l with
                    | Node (t2ll, t2lr, t2lk, _) ->
                        SetTree.Create (
                            t2lk,
                            SetTree.Create (k, t1, t2ll),
                            SetTree.Create (t2k, t2lr, t2r))
                    | _ -> failwith "rebalance"
                else // rotate left
                    SetTree.Create (
                        t2k,
                        SetTree.Create (k, t1, t2l),
                        t2r)
            | _ -> failwith "rebalance"

        elif t1h > t2h + 2u then // left is heavier than right
            match t1 with
            | Node (t1l, t1r, t1k, _) ->
                // one of the nodes must have height > height t2 + 1
                if SetTree.Height t1r > t2h + 1u then
                    // balance right: combination
                    match t1r with
                    | Node (t1rl, t1rr, t1rk, _) ->
                        SetTree.Create (
                            t1rk,
                            SetTree.Create (t1k, t1l, t1rl),
                            SetTree.Create (k, t1rr, t2))
                    | _ -> failwith "rebalance"
                else
                    SetTree.Create (
                        t1k,
                        t1l,
                        SetTree.Create (k, t1r, t2))
            | _ -> failwith "rebalance"

        else
            SetTree.Create (k, t1, t2)

    //
    static member private Balance (comparer : IComparer<'T>, t1, t2, k) =
        // Given t1 < k < t2 where t1 and t2 are "balanced",
        // return a balanced tree for <t1,k,t2>.
        // Recall: balance means subtrees heights differ by at most "tolerance"
        match t1, t2 with
        // TODO : The first two patterns can be merged to use the same handler.
        | Empty, t2 ->
            // drop t1 = empty
            SetTree.Insert (comparer, t2, k)
        | t1, Empty ->
            // drop t2 = empty
            SetTree.Insert (comparer, t1, k)

        // TODO : The next two patterns can be merged to use the same handler.
        | Node (Empty, Empty, k1, _), t2 ->
            let t' = SetTree.Insert (comparer, t2, k1)
            SetTree.Insert (comparer, t', k)
        | t1, Node (Empty, Empty, k2, _) ->
            let t' = SetTree.Insert (comparer, t1, k2)
            SetTree.Insert (comparer, t', k)

        | Node (t11, t12, k1, h1), Node (t21, t22, k2, h2) ->
            // Have:  (t11 < k1 < t12) < k < (t21 < k2 < t22)
            // Either (a) h1,h2 differ by at most 2 - no rebalance needed.
            //        (b) h1 too small, i.e. h1+2 < h2
            //        (c) h2 too small, i.e. h2+2 < h1
            if   h1+2u < h2 then
                // case: b, h1 too small
                // push t1 into low side of t2, may increase height by 1 so rebalance
                SetTree.Rebalance (SetTree.Balance (comparer, t1, t21, k), t22, k2)
            elif h2+2u < h1 then
                // case: c, h2 too small
                // push t2 into high side of t1, may increase height by 1 so rebalance
                SetTree.Rebalance (t11, SetTree.Balance (comparer, t12, t2, k), k1)
            else
                // case: a, h1 and h2 meet balance requirement
                SetTree.Create (k, t1, t2)

    //
    static member private Split (comparer : IComparer<'T>, t, pivot) : SetTree<'T> * bool * SetTree<'T> =
        // Given a pivot and a set t
        // Return { x in t s.t. x < pivot }, pivot in t? , { x in t s.t. x > pivot }
        match t with
        | Empty  ->
            Empty, false, Empty
        | Node (Empty, Empty, k1, _) ->
            let c = comparer.Compare (k1, pivot)
            if   c < 0 then t    ,false,Empty // singleton under pivot
            elif c = 0 then Empty,true ,Empty // singleton is    pivot
            else            Empty,false,t     // singleton over  pivot
        | Node (t11, t12, k1, _) ->
            let c = comparer.Compare (pivot, k1)
            if   c < 0 then // pivot t1
                let t11Lo, havePivot, t11Hi = SetTree.Split (comparer, t11, pivot)
                t11Lo, havePivot, SetTree.Balance (comparer, t11Hi, t12, k1)
            elif c = 0 then // pivot is k1
                t11,true,t12
            else            // pivot t2
                let t12Lo, havePivot, t12Hi = SetTree.Split (comparer, t12, pivot)
                SetTree.Balance (comparer, t11, t12Lo, k1), havePivot, t12Hi

    /// Computes the union of two SetTrees.
    static member Union (comparer : IComparer<'T>, t1 : SetTree<'T>, t2 : SetTree<'T>) : SetTree<'T> =
        // Perf: tried bruteForce for low heights, but nothing significant 
        match t1, t2 with
        | Empty, t -> t
        | t, Empty -> t
        | Node (Empty, Empty, k1, _), t2 ->
            SetTree.Insert (comparer, t2, k1)
        | t1, Node (Empty, Empty, k2, _) ->
            SetTree.Insert (comparer, t1, k2)

        | Node (t11, t12, k1, h1), Node (t21, t22, k2, h2) -> // (t11 < k < t12) AND (t21 < k2 < t22) 
            // Divide and Quonquer:
            //   Suppose t1 is largest.
            //   Split t2 using pivot k1 into lo and hi.
            //   Union disjoint subproblems and then combine. 
            if h1 > h2 then
                let lo, _, hi = SetTree.Split (comparer, t2, k1)

                // OPTIMIZE : It might not hurt to add a reference-equality check here to determine
                // if t11 = lo or t12 = hi -- if we know they're the same, there's no need to perform
                // the union operation on them.
                let lo' = SetTree.Union (comparer, t11, lo)
                let hi' = SetTree.Union (comparer, t12, hi)

                // OPTIMIZE : After the union operations, lo' and hi' could possibly be the same
                // (physically equal) as their originals (lo and hi). Can we exploit this to avoid
                // balancing the tree? What would that tell us about the two sets?
                SetTree.Balance (comparer, lo', hi', k1)
            else
                let lo, _, hi = SetTree.Split (comparer, t1, k2)

                // OPTIMIZE : It might not hurt to add a reference-equality check here to determine
                // if t11 = lo or t12 = hi -- if we know they're the same, there's no need to perform
                // the union operation on them.
                let lo' = SetTree.Union (comparer, t21, lo)
                let hi' = SetTree.Union (comparer, t22, hi)

                // OPTIMIZE : After the union operations, lo' and hi' could possibly be the same
                // (physically equal) as their originals (lo and hi). Can we exploit this to avoid
                // balancing the tree? What would that tell us about the two sets?
                SetTree.Balance (comparer, lo', hi', k2)

    /// Implementation. Computes the intersection of two SetTrees.
    static member private IntersectionAux (comparer : IComparer<'T>, b, m, acc) : SetTree<'T> =
        match m with
        | Empty -> acc
        | Node (Empty, Empty, k, _) ->
            if SetTree.Contains (comparer, b, k) then
                SetTree.Insert (comparer, acc, k)
            else acc
        | Node (l, r, k, _) ->
            let acc =
                let acc = SetTree.IntersectionAux (comparer, b, r, acc)
                if SetTree.Contains (comparer, b, k) then
                    SetTree.Insert (comparer, acc, k)
                else acc 
            SetTree.IntersectionAux (comparer, b, l, acc)

    /// Computes the intersection of two SetTrees.
    static member Intersection (comparer : IComparer<'T>, tree1 : SetTree<'T>, tree2 : SetTree<'T>) : SetTree<'T> =
        SetTree.IntersectionAux (comparer, tree2, tree1, Empty)

    /// Returns a new SetTree created by removing the elements of the
    /// second SetTree from the first.
    static member Difference (comparer : IComparer<'T>, tree1 : SetTree<'T>, tree2 : SetTree<'T>) : SetTree<'T> =
        (* OPTIMIZE :   This function should be re-implemented to use the linear-time
                        algorithm which traverses both trees simultaneously and merges
                        them in a single pass. *)

        // Fold over tree2, removing it's elements from tree1
        (tree1, tree2)
        ||> SetTree.Fold (fun tree el ->
            SetTree.Delete (comparer, tree, el))

    //
    static member IsSubset (comparer : IComparer<'T>, set1 : SetTree<'T>, set2 : SetTree<'T>) : bool =
        SetTree.Forall (fun x -> SetTree.Contains (comparer, set2, x)) set1

    //
    static member IsProperSubset (comparer : IComparer<'T>, set1 : SetTree<'T>, set2 : SetTree<'T>) : bool =
        SetTree.Forall (fun x -> SetTree.Contains (comparer, set2, x)) set1
        && SetTree.Exists (fun x -> not (SetTree.Contains (comparer, set1, x))) set2


(*** Imperative left-to-right iterators. ***)

[<NoEquality; NoComparison>]
type internal SetIterator<'T when 'T : comparison> = {
    // invariant: always collapseLHS result
    mutable stack: SetTree<'T> list;
    // true when MoveNext has been called
    mutable started : bool;
} with
    // collapseLHS:
    // a) Always returns either [] or a list starting with a 'leaf' node.
    // b) The "fringe" of the set stack is unchanged.
    static member private CollapseLHS stack =
        match stack with
        | [] -> []
        | Empty :: rest ->
            SetIterator<'T>.CollapseLHS rest
        | Node (Empty, Empty, _, _) :: _ ->
            stack
        | Node (l, r, k, _) :: rest ->
            SetIterator<'T>.CollapseLHS (l :: (Node (Empty, Empty, k, 0u)) :: r :: rest)

    //
    static member private MkIterator (s : SetTree<'T>) = {
        stack = SetIterator<'T>.CollapseLHS [s];
        started = false; }

    //
    static member private Current i =
        if i.started then
            match i.stack with
            | [] ->
                //raise (new System.InvalidOperationException(SR.GetString(SR.enumerationAlreadyFinished)))
                invalidOp "enumerationAlreadyFinished"
            | Node (Empty, Empty, k, _) :: _ -> k
            | _ -> failwith "Please report error: Set iterator, unexpected stack for current"
        else
            //raise (new System.InvalidOperationException(SR.GetString(SR.enumerationNotStarted)))
            invalidOp "enumerationNotStarted"

    //
    static member private MoveNext i =
        if i.started then
            match i.stack with
            | Node (Empty, Empty, _, _) :: rest ->
                i.stack <- SetIterator<'T>.CollapseLHS rest
                not i.stack.IsEmpty
            | [] -> false
            | _ -> failwith "Please report error: Set iterator, unexpected stack for moveNext"
        else
            i.started <- true       // The first call to MoveNext "starts" the enumeration.
            not i.stack.IsEmpty

    //
    static member mkIEnumerator (s : SetTree<'T>) =
        let i = ref (SetIterator.MkIterator s)
        { new System.Collections.Generic.IEnumerator<'T> with
                member __.Current =
                    SetIterator<'T>.Current !i
            interface System.Collections.IEnumerator with
                member __.Current =
                    box <| SetIterator<'T>.Current !i
                member __.MoveNext () =
                    SetIterator<'T>.MoveNext !i
                member __.Reset () =
                    i := SetIterator<'T>.MkIterator s
            interface System.IDisposable with
                member __.Dispose () = () }


//
[<Sealed; CompiledName("FSharpSet`1")>]
#if FX_NO_DEBUG_PROXIES
#else
[<DebuggerTypeProxy(typedefof<SetDebugView<_>>)>]
#endif
#if FX_NO_DEBUG_DISPLAYS
#else
[<DebuggerDisplay("Count = {Count}")>]
#endif
[<CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix")>]
type Set<[<EqualityConditionalOn>] 'T when 'T : comparison> private (tree : SetTree<'T>) =
    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).
    /// The empty set instance.
    static let empty : Set<'T> = Set Empty

    /// The comparer for the type of the values contained in this collection.
    /// It is cached here for fast access.
    //[<System.NonSerialized>]
    static let comparer = LanguagePrimitives.FastGenericComparer<'T>

#if FX_NO_BINARY_SERIALIZATION
#else
    // NOTE: This type is logically immutable. This field is only mutated during deserialization. 
    [<System.NonSerialized>]
    let mutable tree = tree
        
    // NOTE: This type is logically immutable. This field is only mutated during serialization and deserialization. 
    //
    // WARNING: The compiled name of this field may never be changed because it is part of the logical 
    // WARNING: permanent serialization format for this type.
    let mutable serializedData = null
#endif

#if FX_NO_BINARY_SERIALIZATION
#else
    [<System.Runtime.Serialization.OnSerializingAttribute>]
    member __.OnSerializing (_ : System.Runtime.Serialization.StreamingContext) =
        //ignore(context)
        serializedData <- SetTree.ToArray tree

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member __.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    [<System.Runtime.Serialization.OnDeserializedAttribute>]
    member __.OnDeserialized (_ : System.Runtime.Serialization.StreamingContext) =
        //ignore(context)
        tree <- SetTree.OfArray (comparer, serializedData)
        serializedData <- null
#endif

    /// The empty set instance.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    static member internal Empty =
        empty

    //
    new (elements : seq<'T>) =
        // Preconditions
        // TODO : Check for null input.

        // OPTIMIZE : Try to cast the sequence to array or list;
        // if it succeeds use the specialized method for that type for better performance.
        Set (SetTree.OfSeq (comparer, elements))

    /// The SetTree which is the internal representation of this collection.
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member private __.Tree
        with get () = tree

    //
    member __.Count
        with get () =
            int <| SetTree.Count tree

    //
    member __.IsEmpty
        with get () =
            match tree with
            | Empty -> true
            | Node (_,_,_,_) -> false

    //
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MinimumElement
        with get () =
            SetTree.MinElement tree

    //
#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
#endif
    member __.MaximumElement
        with get () =
            SetTree.MaxElement tree

    //
    member __.Contains (value : 'T) : bool =
        SetTree.Contains (comparer, tree, value)

    //
    member this.Add (value : 'T) : Set<'T> =
        // Add the element to the SetTree; if the result is the same (i.e., the tree
        // already contained the element), return this set instead of creating a new one.
        let tree' = SetTree.Insert (comparer, tree, value)
        if System.Object.ReferenceEquals (tree, tree') then this
        else Set (tree')

    //
    member this.Remove (value : 'T) : Set<'T> =
        // Remove the element from the SetTree; if the result is the same (i.e., the tree
        // did not contain the element), return this set instead of creating a new one.
        let tree' = SetTree.Delete (comparer, tree, value)
        if System.Object.ReferenceEquals (tree, tree') then this
        else Set (tree')

    //
    static member internal Singleton (value : 'T) : Set<'T> =
        Set (SetTree.Singleton value)

    //
    static member internal Union (set1 : Set<'T>, set2 : Set<'T>) : Set<'T> =
        // Compute the union of the trees.
        // If the result is the same as either tree (i.e., one set was a subset of the other)
        // return that tree's corresponding set instead of creating a new one.
        let result = SetTree.Union (comparer, set1.Tree, set2.Tree)
        if System.Object.ReferenceEquals (set1.Tree, result) then set1
        elif System.Object.ReferenceEquals (set2.Tree, result) then set2
        else Set (result)

    //
    static member internal Intersection (set1 : Set<'T>, set2 : Set<'T>) : Set<'T> =
        // Compute the intersection of the trees.
        // If the result is the same as either tree (i.e., one set was a subset of the other)
        // return that tree's corresponding set instead of creating a new one.
        let result = SetTree.Intersection (comparer, set1.Tree, set2.Tree)
        if System.Object.ReferenceEquals (set1.Tree, result) then set1
        elif System.Object.ReferenceEquals (set2.Tree, result) then set2
        else Set (result)

    //
    static member internal Difference (set1 : Set<'T>, set2 : Set<'T>) : Set<'T> =
        // Remove the elements in set2 from set1.
        // If the result is the same as set1's tree (i.e., set2 did not contain any elements
        // from set1), return set1 instead of creating a new set.
        let result = SetTree.Difference (comparer, set1.Tree, set2.Tree)
        if System.Object.ReferenceEquals (set1.Tree, result) then set1
        else Set (result)

    //
    static member internal UnionMany (sets : seq<Set<'T>>) : Set<'T> =
        // Preconditions
        // TODO : Check input for null.

        let combinedSetTree =
            (SetTree.Empty, sets)
            ||> Seq.fold (fun combinedSetTree set ->
                SetTree.Union (comparer, combinedSetTree, set.Tree))

        Set (combinedSetTree)

    //
    static member internal IntersectMany (sets : seq<Set<'T>>) : Set<'T> =
        Seq.reduce (fun s1 s2 -> Set<_>.Intersection(s1,s2)) sets

    //
    static member internal FromSeq (sequence : seq<'T>) : Set<'T> =
        // Preconditions
        // TODO

        // Try to use an optimized implementation based on the actual type of the sequence.
        match sequence with
        | :? ('T[]) as arr ->
            Set (SetTree.OfArray (comparer, arr))
        | :? ('T list) as lst ->
            Set (SetTree.OfList (comparer, lst))
        | _ ->
            Set (SetTree.OfSeq (comparer, sequence))

    //
    static member internal FromList (list : 'T list) : Set<'T> =
        // Preconditions
        // TODO

        Set (SetTree.OfList (comparer, list))

    //
    static member internal FromArray (arr : 'T[]) : Set<'T> =
        // Preconditions
        // TODO

        Set (SetTree.OfArray (comparer, arr))

    //
    member internal this.ToSeq () : seq<'T> =
        //SetTree.ToSeq tree
        this :> seq<_>

    //
    member internal __.ToList () =
        SetTree.ToList tree

    //
    member internal __.ToArray () =
        SetTree.ToArray tree

    //
    static member op_Addition (set1 : Set<'T>, set2 : Set<'T>) : Set<'T> =
        Set<'T>.Union (set1, set2)

    //
    static member op_Subtraction (set1 : Set<'T>, set2 : Set<'T>) : Set<'T> =
        Set<'T>.Difference (set1, set2)

    //
    member __.IsSubsetOf (otherSet : Set<'T>) : bool =
        SetTree.IsSubset (comparer, tree, otherSet.Tree)

    //
    member __.IsProperSubsetOf (otherSet : Set<'T>) : bool =
        SetTree.IsProperSubset (comparer, tree, otherSet.Tree)

    //
    member __.IsSupersetOf (otherSet : Set<'T>) : bool =
        SetTree.IsSubset (comparer, otherSet.Tree, tree)

    //
    member __.IsProperSupersetOf (otherSet : Set<'T>) : bool =
        SetTree.IsProperSubset (comparer, otherSet.Tree, tree)

    //
    member internal __.Iterate (action : 'T -> unit) : unit =
        SetTree.Iter action tree

    //
    member internal __.Exists (predicate : 'T -> bool) : bool =
        SetTree.Exists predicate tree

    //
    member internal __.ForAll (predicate : 'T -> bool) : bool =
        SetTree.Forall predicate tree

    //
    member internal __.Fold (folder : 'State -> 'T -> 'State) (state : 'State) : 'State =
        SetTree.Fold folder state tree

    //
    member internal __.FoldBack (folder : 'T -> 'State -> 'State) (state : 'State) : 'State =
        SetTree.FoldBack folder state tree

    //
    member internal __.Map (mapping : 'T -> 'U) : Set<'U> =
        let mappedTree =
            /// The comparer for the 'U type.
            let comparer = LanguagePrimitives.FastGenericComparer<'U>

            (SetTree.Empty, tree)
            ||> SetTree.Fold (fun mappedTree el ->
                SetTree.Insert (comparer, mappedTree, mapping el))

        Set (mappedTree)

    //
    member internal __.Filter (predicate : 'T -> bool) : Set<'T> =
        let filteredTree =
            (tree, tree)
            ||> SetTree.Fold (fun filteredTree el ->
                if predicate el then filteredTree
                else SetTree.Delete (comparer, filteredTree, el))

        Set (filteredTree)

    //
    member internal this.Partition (predicate : 'T -> bool) : Set<'T> * Set<'T> =
        let trueTree, falseTree =
            ((tree, tree), tree)
            ||> SetTree.Fold (fun (trueTree, falseTree) el ->
                if predicate el then
                    trueTree,
                    SetTree.Delete (comparer, falseTree, el)
                else
                    SetTree.Delete (comparer, trueTree, el),
                    falseTree)

        // If either of the 'true' or 'false' trees are equivalent to the input tree,
        // return this set as one component of the returned tuple -- this avoids creating
        // an additional set for no reason.
        if System.Object.ReferenceEquals (tree, trueTree) then
            this, empty
        elif System.Object.ReferenceEquals (tree, falseTree) then
            empty, this
        else
            Set (trueTree), Set (falseTree)

    // OPTIMIZE : Instead of computing this repeatedly -- this type is immutable so we should
    // lazily compute the hashcode once instead; however, we do need to account for the case
    // where an instance is created via deserialization, so it may make sense to use a 'ref'
    // field (which is excluded from serialization) with Interlocked.Exchange instead of using
    // a 'lazy' value.
    member __.ComputeHashCode () =
        let inline combineHash x y = (x <<< 1) + y + 631
        (0, tree)
        ||> SetTree.Fold (fun res x ->
            combineHash res (hash x))
        |> abs

    override this.GetHashCode () =
        this.ComputeHashCode ()

    // OPTIMIZE : Would it be significantly faster if we re-implemented this to work
    // directly on the SetTrees instead of using enumerators? Or, at least using an
    // imperative loop instead of a recursive function?
    override this.Equals other =
        match other with
        | :? Set<'T> as other ->
            use e1 = (this :> seq<_>).GetEnumerator ()
            use e2 = (other :> seq<_>).GetEnumerator ()
            let rec loop () =
                let m1 = e1.MoveNext ()
                let m2 = e2.MoveNext ()
                (m1 = m2) && (not m1 || ((e1.Current = e2.Current) && loop ()))
            loop ()
        | _ -> false

    override x.ToString () =
        match List.ofSeq (Seq.truncate 4 x) with
        | [] -> "set []"
        | [h1] ->
            System.Text.StringBuilder()
                .Append("set [")
                .Append(LanguagePrimitives.anyToStringShowingNull h1)
                .Append("]")
                .ToString()
        | [h1; h2] ->
            System.Text.StringBuilder()
                .Append("set [")
                .Append(LanguagePrimitives.anyToStringShowingNull h1)
                .Append("; ")
                .Append(LanguagePrimitives.anyToStringShowingNull h2)
                .Append("]")
                .ToString()
        | [h1; h2; h3] ->
            System.Text.StringBuilder()
                .Append("set [")
                .Append(LanguagePrimitives.anyToStringShowingNull h1)
                .Append("; ")
                .Append(LanguagePrimitives.anyToStringShowingNull h2)
                .Append("; ")
                .Append(LanguagePrimitives.anyToStringShowingNull h3)
                .Append("]")
                .ToString()
        | h1 :: h2 :: h3 :: _ ->
            System.Text.StringBuilder()
                .Append("set [")
                .Append(LanguagePrimitives.anyToStringShowingNull h1)
                .Append("; ")
                .Append(LanguagePrimitives.anyToStringShowingNull h2)
                .Append("; ")
                .Append(LanguagePrimitives.anyToStringShowingNull h3)
                .Append("; ... ]")
                .ToString()

    interface System.IComparable with
        /// <inherit />
        member __.CompareTo other =
            SetTree.Compare (comparer, tree, (other :?> Set<'T>).Tree)

    interface System.Collections.IEnumerable with
        /// <inherit />
        member __.GetEnumerator () =
//            (SetTree.ToSeq tree).GetEnumerator ()
            SetIterator.mkIEnumerator tree
            :> System.Collections.IEnumerator

    interface IEnumerable<'T> with
        /// <inherit />
        member __.GetEnumerator () =
            //(SetTree.ToSeq tree).GetEnumerator ()
            SetIterator.mkIEnumerator tree

    interface ICollection<'T> with
        /// <inherit />
        member __.Count
            with get () =
                int <| SetTree.Count tree

        /// <inherit />
        member __.IsReadOnly
            with get () = true

        /// <inherit />
        member __.Add _ =
            raise <| System.NotSupportedException "ReadOnlyCollection"

        /// <inherit />
        member __.Clear () =
            raise <| System.NotSupportedException "ReadOnlyCollection"

        /// <inherit />
        member __.Contains (item : 'T) =
            SetTree.Contains (comparer, tree, item)

        /// <inherit />
        member this.CopyTo (array, arrayIndex) =
            // Preconditions
            if System.Object.ReferenceEquals (null, array) then
                nullArg "array"
            elif arrayIndex < 0 then
                raise <| System.ArgumentOutOfRangeException "arrayIndex"

            let count = int <| SetTree.Count tree
            if arrayIndex + count > Array.length array then
                invalidArg "arrayIndex"
                    "There is not enough room in the array to copy the elements when starting at the specified index."

            this.Fold (fun index el ->
                array.[index] <- el
                index + 1) arrayIndex
            |> ignore

        /// <inherit />
        member __.Remove _ : bool =
            raise <| System.NotSupportedException "ReadOnlyCollection"

and [<Sealed>]
    internal SetDebugView<'T when 'T : comparison> (set : Set<'T>) =

#if FX_NO_DEBUG_DISPLAYS
#else
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
#endif
    member __.Items
        with get () : 'T[] =
            //set |> Seq.truncate debugViewMaxElementCount |> Seq.toArray
            set.ToArray ()

//
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    [<CompiledName("IsEmpty")>]
    let (*inline*) isEmpty (s : Set<'T>) = s.IsEmpty

    [<CompiledName("Contains")>]
    let (*inline*) contains x (s : Set<'T>) = s.Contains(x)

    [<CompiledName("Add")>]
    let (*inline*) add x (s : Set<'T>) = s.Add(x)

    [<CompiledName("Singleton")>]
    let singleton x = Set<'T>.Singleton(x)

    [<CompiledName("Remove")>]
    let (*inline*) remove x (s : Set<'T>) = s.Remove(x)

    [<CompiledName("Union")>]
    let (*inline*) union (s1 : Set<'T>) (s2 : Set<'T>) = s1 + s2

    [<CompiledName("UnionMany")>]
    let unionMany sets = Set<_>.UnionMany(sets)

    [<CompiledName("Intersect")>]
    let intersect (s1 : Set<'T>) (s2 : Set<'T>) = Set<'T>.Intersection(s1,s2)

    [<CompiledName("IntersectMany")>]
    let intersectMany sets = Set<_>.IntersectMany(sets)

    [<CompiledName("Iterate")>]
    let iter f (s : Set<'T>) = s.Iterate(f)

    [<GeneralizableValue>]
    [<CompiledName("Empty")>]
    let empty<'T when 'T : comparison> : Set<'T> = Set<'T>.Empty

    [<CompiledName("ForAll")>]
    let forall f (s : Set<'T>) = s.ForAll f

    [<CompiledName("Exists")>]
    let exists f (s : Set<'T>) = s.Exists f

    [<CompiledName("Filter")>]
    let filter f (s : Set<'T>) = s.Filter f

    [<CompiledName("Partition")>]
    let partition f (s : Set<'T>) = s.Partition f 

    [<CompiledName("Fold")>]
    let fold<'T, 'State when 'T : comparison> f (z : 'State) (s : Set<'T>) = s.Fold f z

    [<CompiledName("FoldBack")>]
    let foldBack<'T, 'State when 'T : comparison> f (s : Set<'T>) (z : 'State) = s.FoldBack f z

    [<CompiledName("Map")>]
    let map f (s : Set<'T>) = s.Map f

    [<CompiledName("Count")>]
    let count (s : Set<'T>) = s.Count

    [<CompiledName("MinumumElement")>]
    let minimumElement (s : Set<'T>) = s.MinimumElement

    [<CompiledName("MaximumElement")>]
    let maximumElement (s : Set<'T>) = s.MaximumElement

    [<CompiledName("OfList")>]
    let ofList l = Set<_>.FromList l

    [<CompiledName("OfArray")>]
    let ofArray (l : 'T array) = Set<'T>.FromArray l

    [<CompiledName("ToList")>]
    let toList (s : Set<'T>) = s.ToList()
 
    [<CompiledName("ToArray")>]
    let toArray (s : Set<'T>) = s.ToArray()

    [<CompiledName("ToSeq")>]
    let (*inline*) toSeq (s : Set<'T>) : seq<'T> =
        //s.ToSeq ()
        (s :> seq<_>)

    [<CompiledName("OfSeq")>]
    let ofSeq (c : seq<_>) = Set<_>.FromSeq c

    [<CompiledName("Difference")>]
    let (*inline*) difference (s1: Set<'T>) (s2: Set<'T>) = s1 - s2

    [<CompiledName("IsSubset")>]
    let (*inline*) isSubset (x:Set<'T>) (y: Set<'T>) =
        x.IsSubsetOf y

    [<CompiledName("IsSuperset")>]
    let (*inline*) isSuperset (x:Set<'T>) (y: Set<'T>) =
        x.IsSupersetOf y

    [<CompiledName("IsProperSubset")>]
    let (*inline*) isProperSubset (x:Set<'T>) (y: Set<'T>) =
        x.IsProperSubsetOf y

    [<CompiledName("IsProperSuperset")>]
    let (*inline*) isProperSuperset (x:Set<'T>) (y: Set<'T>) =
        x.IsProperSupersetOf y

    [<CompiledName("MinElement")>]
    let (*inline*) minElement (s : Set<'T>) = s.MinimumElement

    [<CompiledName("MaxElement")>]
    let (*inline*) maxElement (s : Set<'T>) = s.MaximumElement

