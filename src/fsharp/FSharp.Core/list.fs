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

namespace Microsoft.FSharp.Collections

    open System.Diagnostics
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.LanguagePrimitives
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics
    open System.Collections.Generic
    open Microsoft.FSharp.Core.SR

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module List = 

        [<CompiledName("Length")>]
        let length (list: 'T list) = list.Length

        [<CompiledName("Reverse")>]
        let rev list = Microsoft.FSharp.Primitives.Basics.List.rev list

        [<CompiledName("Concat")>]
        let concat lists = Microsoft.FSharp.Primitives.Basics.List.concat lists

        [<CompiledName("Map")>]
        let map f list = Microsoft.FSharp.Primitives.Basics.List.map f list

        [<CompiledName("MapIndexed")>]
        let mapi f list = Microsoft.FSharp.Primitives.Basics.List.mapi f list

        [<CompiledName("Iterate")>]
        let iter f list = Microsoft.FSharp.Primitives.Basics.List.iter f list

        [<CompiledName("OfArray")>]
        let ofArray (array:'T array) = Microsoft.FSharp.Primitives.Basics.List.ofArray array

        [<CompiledName("ToArray")>]
        let toArray (list:'T list) = Microsoft.FSharp.Primitives.Basics.List.toArray list

        [<CompiledName("Empty")>]
        let empty<'T> = ([ ] : 'T list)

        [<CompiledName("Head")>]
        let head list = match list with (x:: _) -> x | [] -> invalidArg "list" (SR.GetString(SR.inputListWasEmpty))

        [<CompiledName("Tail")>]
        let tail list = match list with (_ :: t) -> t | [] -> invalidArg "list" (SR.GetString(SR.inputListWasEmpty))

        [<CompiledName("IsEmpty")>]
        let isEmpty list = match list with [] -> true | _ -> false
        
        [<CompiledName("Append")>]
        let append list1 list2 = list1 @ list2

        [<CompiledName("Get")>]
        let rec nth list index = 
            match list with 
            | h::t when index >= 0 -> 
                if index = 0 then h else nth t (index - 1)
            | _ ->  
                invalidArg "index" (SR.GetString(SR.indexOutOfBounds))

        let rec chooseAllAcc f xs acc =
            match xs with 
            | [] -> rev acc
            | h :: t -> 
                 match f h with 
                 | None -> chooseAllAcc f t acc 
                 | Some x -> chooseAllAcc f t (x::acc)

        [<CompiledName("Choose")>]
        let choose f xs = chooseAllAcc f xs []

        [<CompiledName("IterateIndexed")>]
        let iteri f list = Microsoft.FSharp.Primitives.Basics.List.iteri f list

        [<CompiledName("Initialize")>]
        let init count f = Microsoft.FSharp.Primitives.Basics.List.init count f

        let rec initConstAcc n x acc = 
            if n <= 0 then acc else initConstAcc (n-1) x (x::acc)
            
        [<CompiledName("Replicate")>]
        let replicate count x = 
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            initConstAcc count x []        

        [<CompiledName("Iterate2")>]
        let iter2 f list1 list2 = 
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let rec loop list1 list2 = 
                match list1,list2 with
                | [],[] -> () 
                | (h1::t1), (h2::t2) -> f.Invoke(h1,h2); loop t1 t2 
                | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))
            loop list1 list2

        [<CompiledName("IterateIndexed2")>]
        let iteri2 f list1 list2 = 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let rec loop n list1 list2 = 
                match list1,list2 with
                | [],[] -> () 
                | (h1::t1), (h2::t2) -> f.Invoke(n,h1,h2); loop (n+1) t1 t2 
                | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))
            loop 0 list1 list2
          
        let rec map3aux (f:OptimizedClosures.FSharpFunc<_,_,_,_>) list1 list2 list3 acc = 
            match list1,list2,list3 with
            | [],[],[] -> rev acc
            | (h1::t1), (h2::t2),(h3::t3) -> let x = f.Invoke(h1,h2,h3) in map3aux f t1 t2 t3 (x :: acc)
            | _ -> invalidArg "list3" (SR.GetString(SR.listsHadDifferentLengths))

        [<CompiledName("Map3")>]
        let map3 f list1 list2 list3 = 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            map3aux f list1 list2 list3 []

        let rec mapi2aux n (f:OptimizedClosures.FSharpFunc<_,_,_,_>) list1 list2 acc = 
            match list1,list2 with
            | [],[] -> rev acc
            | (h1::t1), (h2::t2) -> let x = f.Invoke(n,h1,h2) in mapi2aux (n+1) f t1 t2 (x :: acc)
            | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))

        [<CompiledName("MapIndexed2")>]
        let mapi2 f list1 list2 = 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            mapi2aux 0 f list1 list2 []

        [<CompiledName("Map2")>]
        let map2 f list1 list2 = Microsoft.FSharp.Primitives.Basics.List.map2 f list1 list2

        [<CompiledName("Fold")>]
        let fold<'T,'State> f (s:'State) (list: 'T list) = 
            match list with 
            | [] -> s
            | _ -> 
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let rec loop s xs = 
                    match xs with 
                    | [] -> s
                    | h::t -> loop (f.Invoke(s,h)) t
                loop s list

        [<CompiledName("Reduce")>]
        let reduce f list = 
            match list with 
            | [] -> invalidArg "list" (SR.GetString(SR.inputListWasEmpty))
            | (h::t) -> fold f h t

        [<CompiledName("Scan")>]
        let scan<'T,'State> f (s:'State) (list:'T list) = 
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let rec loop s xs acc = 
                match xs with 
                | [] -> rev acc
                | (h::t) -> let s = f.Invoke(s,h) in loop s t (s :: acc)
            loop s list [s]

        [<CompiledName("Fold2")>]
        let fold2<'T1,'T2,'State> f (acc:'State) (list1:list<'T1>) (list2:list<'T2>) = 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let rec loop acc list1 list2 =
                match list1,list2 with 
                | [],[] -> acc
                | (h1::t1),(h2::t2) -> loop (f.Invoke(acc,h1,h2)) t1 t2
                | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))
            loop acc list1 list2

        let foldArraySubRight (f:OptimizedClosures.FSharpFunc<'T,_,_>) (arr: 'T[]) start fin acc = 
            let mutable state = acc
            for i = fin downto start do
                state <- f.Invoke(arr.[i], state)
            state

        // this version doesn't causes stack overflow - it uses a private stack 
        [<CompiledName("FoldBack")>]
        let foldBack<'T,'State> f (list:'T list) (acc:'State) = 
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            match list with 
            | [] -> acc
            | [h] -> f.Invoke(h,acc)
            | [h1;h2] -> f.Invoke(h1,f.Invoke(h2,acc))
            | [h1;h2;h3] -> f.Invoke(h1,f.Invoke(h2,f.Invoke(h3,acc)))
            | [h1;h2;h3;h4] -> f.Invoke(h1,f.Invoke(h2,f.Invoke(h3,f.Invoke(h4,acc))))
            | _ -> 
                // It is faster to allocate and iterate an array than to create all those 
                // highly nested stacks.  It also means we won't get stack overflows here. 
                let arr = toArray list
                let arrn = arr.Length
                foldArraySubRight f arr 0 (arrn - 1) acc

        [<CompiledName("ReduceBack")>]
        let reduceBack f list = 
            match list with 
            | [] -> invalidArg "list" (SR.GetString(SR.inputListWasEmpty))
            | _ -> 
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let arr = toArray list
                let arrn = arr.Length
                foldArraySubRight f arr 0 (arrn - 2) arr.[arrn - 1]

        let scanArraySubRight<'T,'State> (f:OptimizedClosures.FSharpFunc<'T,'State,'State>) (arr:_[]) start fin initState = 
            let mutable state = initState
            let mutable res = [state]
            for i = fin downto start do
                state <- f.Invoke(arr.[i], state);
                res <- state :: res
            res

        [<CompiledName("ScanBack")>]
        let scanBack<'T,'State> f (list:'T list) (s:'State) = 
            match list with 
            | [] -> [s]
            | [h] -> 
                [f h s; s]
            | _ -> 
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                // It is faster to allocate and iterate an array than to create all those 
                // highly nested stacks.  It also means we won't get stack overflows here. 
                let arr = toArray list
                let arrn = arr.Length
                scanArraySubRight f arr 0 (arrn - 1) s

        let foldBack2UsingArrays (f:OptimizedClosures.FSharpFunc<_,_,_,_>) list1 list2 acc = 
            let arr1 = toArray list1
            let arr2 = toArray list2
            let n1 = arr1.Length
            let n2 = arr2.Length
            if n1 <> n2 then invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths));
            let mutable res = acc
            for i = n1 - 1 downto 0 do
                res <- f.Invoke(arr1.[i],arr2.[i],res)
            res

        [<CompiledName("FoldBack2")>]
        let rec foldBack2<'T1,'T2,'State> f (list1:'T1 list) (list2:'T2 list) (acc:'State) = 
            match list1,list2 with 
            | [],[] -> acc
            | h1::rest1, k1::rest2 -> 
                let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
                match rest1, rest2 with 
                | [],[] -> f.Invoke(h1,k1,acc)
                | [h2],[k2] -> f.Invoke(h1,k1,f.Invoke(h2,k2,acc))
                | [h2;h3],[k2;k3] -> f.Invoke(h1,k1,f.Invoke(h2,k2,f.Invoke(h3,k3,acc)))
                | [h2;h3;h4],[k2;k3;k4] -> f.Invoke(h1,k1,f.Invoke(h2,k2,f.Invoke(h3,k3,f.Invoke(h4,k4,acc))))
                | _ -> foldBack2UsingArrays f list1 list2 acc
            | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))

        let rec forall2aux (f:OptimizedClosures.FSharpFunc<_,_,_>) list1 list2 = 
            match list1,list2 with 
            | [],[] -> true
            | (h1::t1),(h2::t2) -> f.Invoke(h1,h2)  && forall2aux f t1 t2
            | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))

        [<CompiledName("ForAll2")>]
        let forall2 f list1 list2 = 
            match list1,list2 with 
            | [],[] -> true
            | _ -> 
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                forall2aux f list1 list2

        [<CompiledName("ForAll")>]
        let forall f list1 = Microsoft.FSharp.Primitives.Basics.List.forall f list1

        [<CompiledName("Exists")>]
        let exists f list1 = Microsoft.FSharp.Primitives.Basics.List.exists f list1

        let rec exists2aux (f:OptimizedClosures.FSharpFunc<_,_,_>) list1 list2 = 
            match list1,list2 with 
            | [],[] -> false
            | (h1::t1),(h2::t2) ->f.Invoke(h1,h2)  || exists2aux f t1 t2
            | _ -> invalidArg "list2" (SR.GetString(SR.listsHadDifferentLengths))

        [<CompiledName("Exists2")>]
        let rec exists2 f list1 list2 = 
            match list1,list2 with 
            | [],[] -> false
            | _ -> 
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                exists2aux f list1 list2

        [<CompiledName("Find")>]
        let rec find f list = match list with [] -> raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  | h::t -> if f h then h else find f t

        [<CompiledName("TryFind")>]
        let rec tryFind f list = match list with [] -> None | h::t -> if f h then Some h else tryFind f t

        [<CompiledName("TryPick")>]
        let rec tryPick f list = 
            match list with 
            | [] -> None 
            | h::t -> 
                match f h with 
                | None -> tryPick f t 
                | r -> r

        [<CompiledName("Pick")>]
        let rec pick f list = 
            match list with 
            | [] -> raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  
            | h::t -> 
                match f h with 
                | None -> pick f t 
                | Some r -> r

        [<CompiledName("Filter")>]
        let filter f x = Microsoft.FSharp.Primitives.Basics.List.filter f x

        [<CompiledName("Partition")>]
        let partition p x = Microsoft.FSharp.Primitives.Basics.List.partition p x
            
        [<CompiledName("Unzip")>]
        let unzip x = Microsoft.FSharp.Primitives.Basics.List.unzip x

        [<CompiledName("Unzip3")>]
        let unzip3 x = Microsoft.FSharp.Primitives.Basics.List.unzip3 x

        [<CompiledName("Zip")>]
        let zip x1 x2 =  Microsoft.FSharp.Primitives.Basics.List.zip x1 x2

        [<CompiledName("Zip3")>]
        let zip3 x1 x2 x3 =  Microsoft.FSharp.Primitives.Basics.List.zip3 x1 x2 x3

        [<CompiledName("SortWith")>]
        let sortWith cmp xs = Microsoft.FSharp.Primitives.Basics.List.sortWith cmp xs

        [<CompiledName("SortBy")>]
        let sortBy f xs =
            match xs with 
            | [] | [_] -> xs
            | _ -> 
                let array = List.toArray xs
                Microsoft.FSharp.Primitives.Basics.Array.stableSortInPlaceBy f array
                List.ofArray array
            
        [<CompiledName("Sort")>]
        let sort xs =
            match xs with 
            | [] | [_] -> xs
            | _ -> 
                let array = List.toArray xs
                Microsoft.FSharp.Primitives.Basics.Array.stableSortInPlace array
                List.ofArray array

        [<CompiledName("OfSeq")>]
        let ofSeq source = Seq.toList source

        [<CompiledName("ToSeq")>]
        let toSeq list = Seq.ofList list

        [<CompiledName("FindIndex")>]
        let findIndex f list = 
            let rec loop n = function[] -> raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  | h::t -> if f h then n else loop (n+1) t
            loop 0 list

        [<CompiledName("TryFindIndex")>]
        let tryFindIndex f list = 
            let rec loop n = function[] -> None | h::t -> if f h then Some n else loop (n+1) t
            loop 0 list
        
        [<CompiledName("Sum")>]
        let inline sum          (list:list<_>) = Seq.sum list

        [<CompiledName("SumBy")>]
        let inline sumBy f     (list:list<_>) = Seq.sumBy f list

        [<CompiledName("Max")>]
        let inline max          (list:list<_>) = Seq.max list

        [<CompiledName("MaxBy")>]
        let inline maxBy f (list:list<_>) = Seq.maxBy f list

        [<CompiledName("Min")>]
        let inline min          (list:list<_>) = Seq.min list

        [<CompiledName("MinBy")>]
        let inline minBy f (list:list<_>) = Seq.minBy f list

        [<CompiledName("Average")>]
        let inline average      (list:list<_>) = Seq.average list

        [<CompiledName("AverageBy")>]
        let inline averageBy f (list:list<_>) = Seq.averageBy f list

        [<CompiledName("Collect")>]
        let collect f list = Microsoft.FSharp.Primitives.Basics.List.collect f list

        [<CompiledName("Permute")>]
        let permute indexMap list = list |> toArray |> Array.permute indexMap |> ofArray

