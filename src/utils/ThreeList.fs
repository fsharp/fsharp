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


/// A working implementation of vectors that chunks the vector in groups of 3 giving 7 words per allocation (a 3/7 = 42% maximum utilization rate instead of 1/4 = 25%)
/// No cons, head, tail or other access-from-the-left operations (apart from nth) are supported
namespace Internal.Utilities 

open System.Collections
open System.Collections.Generic

(*
[<Sealed>]
type ThreeList<'T> = 
    member Length : int
    interface IEnumerable<'T> 
    interface System.Collections.IEnumerable 
    interface System.IComparable
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ThreeList = 
    val map : ('T -> 'U) -> ThreeList<'T> -> ThreeList<'U>
    val mapi : (int -> 'T -> 'U) -> ThreeList<'T> -> ThreeList<'U>
    val isEmpty : ThreeList<'T> -> bool
    val toList : ThreeList<'T> -> 'T list
    val ofList : 'T list -> ThreeList<'T> 
    val lengthsEqAndForall2 : ('T -> 'U -> bool) -> ThreeList<'T> -> ThreeList<'U> -> bool
    val init : int -> (int -> 'T) -> ThreeList<'T>
    val empty<'T> : ThreeList<'T>
    val toArray : ThreeList<'T> -> 'T[]
    val ofArray : 'T[] -> ThreeList<'T>
    val nth : ThreeList<'T> -> int -> 'T
    val iter : ('T -> unit) -> ThreeList<'T> -> unit
    val iteri : (int -> 'T -> unit) -> ThreeList<'T> -> unit
    val foldBack : ('T -> 'State -> 'State) -> ThreeList<'T> -> 'State -> 'State
    val exists : ('T -> bool) -> ThreeList<'T> -> bool
*)

[<CustomComparison; CustomEquality>]
type ThreeList<[<ComparisonConditionalOn; EqualityConditionalOn>] 'T> = 
    {n:int;x1: 'T;x2: 'T;x3: 'T; mutable t: ThreeList<'T>}
    interface IEnumerable<'T> with 
        member x.GetEnumerator() : IEnumerator<'T> = (new ThreeListEnumerator<'T>(x) :> IEnumerator<'T>)
    interface IEnumerable with 
        member x.GetEnumerator() : IEnumerator =  ((x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator)
    interface System.IComparable with 
        member x.CompareTo(yobj:obj) = 
            match yobj with 
            | :? ThreeList<'T> as y -> 
                let rec loop x y = 
                    let c = compare x.n y.n 
                    if c <> 0 then c else
                    if x.n = 0 then 0 else
                    let c = Unchecked.compare x.x1 y.x1
                    if c <> 0 then c else
                    if x.n = 1 then 0 else
                    let c = Unchecked.compare x.x2 y.x2
                    if c <> 0 then c else
                    if x.n = 2 then 0 else
                    let c = Unchecked.compare x.x2 y.x2
                    if c <> 0 then c else
                    let c = Unchecked.compare x.x3 y.x3
                    if c <> 0 then c else
                    loop x.t y.t
                loop x y
            | _ -> invalidArg "yobj" "incorrect type"
                
    override x.Equals(yobj:obj) = 
        match yobj with 
        | :? ThreeList<'T> as y ->  
            let rec loop x y = 
                x.n = y.n && 
                match x.n with 
                | 0 -> true 
                | 1 -> Unchecked.equals x.x1  y.x1 
                | 2 -> Unchecked.equals x.x1 y.x1 && Unchecked.equals x.x2 y.x2 
                | _ -> Unchecked.equals x.x1 y.x1 && Unchecked.equals x.x2 y.x2 && Unchecked.equals x.x3 y.x3 && Unchecked.equals x.x1 y.x1 && loop x.t y.t
            loop x y
        | _ -> false
    override x.GetHashCode() = 
        let rec loop acc x = 
            let acc = acc <<< 1 + Unchecked.hash x.x1 + 3
            if x.n = 0 then acc else
            let acc = acc <<< 1 + Unchecked.hash x.x1 + 7
            if x.n = 1 then acc else
            let acc = acc <<< 1 + Unchecked.hash x.x2 + 17
            if x.n = 2 then acc else
            loop (acc <<< 1 + Unchecked.hash x.x3 + 31) x.t 
        loop 0 x

and ThreeListEnumerator<'T>(tl:ThreeList<'T>) = 
    let mutable tl = tl
    let mutable p = -1
    interface IEnumerator with 
        member x.Current = box (x :> IEnumerator<'T>).Current
        member x.MoveNext() =  
            if p = tl.n - 1 then 
                if tl.n < 3 || tl.t.n = 0 then false else tl <- tl.t; p <- 0; true 
            else p <- p + 1; true
        member x.Reset() = invalidOp "reset not permitted"
    interface IEnumerator<'T> with 
        member x.Current = match p with 0 -> tl.x1 | 1 -> tl.x2 | _ -> tl.x3
        member x.Dispose() = ()

type ThreeListStatics<'T>() = 
    static let emptyTL = {n=0;x1=Unchecked.defaultof<'T>;x2=Unchecked.defaultof<'T>;x3=Unchecked.defaultof<'T>;t=Unchecked.defaultof<_>}
    static member Empty : ThreeList<'T> = emptyTL

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ThreeList = 
    let inline TL(n,x1,x2,x3,t) = {n=n;x1=x1;x2=x2;x3=x3;t=t}
    let inline e<'T> = Unchecked.defaultof<'T>

    let rec mapToFreshConsTail f r prev {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
        let c = 
            match n with 
            | 0 -> ThreeListStatics.Empty 
            | 1 -> TL(1, f x1, e, e, e)
            | 2 -> TL(2, f x1, f x2, e, e)
            | _ -> TL(3, f x1, f x2, f x3, e)
        prev.t <- c
        if n = 3 then mapToFreshConsTail f r c t else r

    let map f {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
        match n with 
        | 0 -> ThreeListStatics.Empty 
        | 1 -> TL(1, f x1, e, e, e)
        | 2 -> TL(2, f x1, f x2, e, e)
        | _ -> let r = TL(3, f x1, f x2, f x3, e) in mapToFreshConsTail f r r t

    let rec mapiToFreshConsTail f i r prev {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
        let c = 
            match n with 
            | 0 -> ThreeListStatics.Empty 
            | 1 -> TL(1, f i x1, e, e, e)
            | 2 -> TL(2, f i x1, f (i + 1) x2, e, e)
            | _ -> TL(3, f i x1, f (i + 1) x2, f (i + 2) x3, e)
        prev.t <- c
        if n = 3 then mapiToFreshConsTail f (i+3) r c t else r

    let mapi f {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
        match n with 
        | 0 -> ThreeListStatics.Empty 
        | 1 -> TL(1, f 0 x1, e, e, e)
        | 2 -> TL(2, f 0 x1, f 1 x2, e, e)
        | _ -> let r = (TL(3, f 0 x1, f 1 x2, f 2 x3, e)) in mapiToFreshConsTail f 3 r r t

    let iteri f xs = 
        let rec loop i {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
            match n with 
            | 0 -> ()
            | 1 -> f i x1
            | 2 -> f i x1; f (i+1) x2; 
            | _ -> f i x1; f (i+1) x2; f (i+2) x3; loop (i+3) t
        loop 0 xs

    let rec nth {n=n;x1=x1;x2=x2;x3=x3;t=t} i = 
        if i < 0 then invalidArg "k" "must be non-negative"
        elif i >= n then invalidArg "k" "too big for list"
        match i with 
        | 0 -> x1
        | 1 -> x2
        | 2 -> x3
        | _ -> nth t (i-3) 

    let rec iter f {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
        match n with 
        | 0 -> ()
        | 1 -> f x1
        | 2 -> f x1; f x2; 
        | _ -> f x1; f x2; f x3; iter f t

    let  isEmpty t = (t.n = 0)

    let length x =  
        if x.n < 3 then x.n else
        let rec loop acc t = if t.n = 3 then loop (acc + t.n) t.t else acc + t.n
        loop 3 x.t

    let toArray xs =  
        let res = Array.zeroCreate (length xs)
        let rec loop i {n=n;x1=x1;x2=x2;x3=x3;t=t} = 
            match n with 
            | 0 -> ()
            | 1 -> res.[i] <- x1
            | 2 -> res.[i] <- x1; res.[i+1] <- x2
            | _ -> res.[i] <- x1; res.[i+1] <- x2; res.[i+2] <- x3; loop (i+3) t
        loop 0 xs
        res

    let ofArray (arr:'T[]) =  
        let n = arr.Length
        let acc = 
            match n % 3 with
            | 0 -> ThreeListStatics.Empty
            | 1 -> TL(1, arr.[n-1], e, e, e)
            | _ -> TL(2, arr.[n-2], arr.[n-1], e, e)
        let rec loop acc i = if i = 0 then acc else loop (TL(3, arr.[i-3], arr.[i-2], arr.[i-1], acc)) (i-3) 
        loop acc (n-n%3) 

    let toList {n=n;x1=x1;x2=x2;x3=x3;t=t} =  
        match n with 
        | 0 -> []
        | 1 -> [x1]
        | 2 -> [x1; x2]
        | _ -> x1::x2::x3::Array.toList (toArray t)

    let rec ofList (x:'T list)  = 
        match x with 
        | [] -> ThreeListStatics.Empty
        | [x1] -> TL(1, x1, e, e, e)
        | [x1;x2] -> TL(2, x1, x2, e, e)
        | x1::x2::x3::t -> 
            let rec loop r prev x = 
                match x with 
                | [] -> prev.t <- ThreeListStatics.Empty; r
                | [x1] -> prev.t <- TL(1, x1, e, e, e); r
                | [x1;x2] -> prev.t <- TL(2, x1, x2, e, e); r
                | x1::x2::x3::t -> let c = TL(3, x1, x2, x3, e) in prev.t <- c; loop r c t

            let r = TL(3, x1, x2, x3, e)
            loop r r t

    let lengthsEqAndForall2 f (x: ThreeList<'T>) (y: ThreeList<'U>) = 
        length x = length y &&
        let rec loop x y = 
            let {n=xn;x1=x1;x2=x2;x3=x3;t=xt}  = x
            let {n=_;x1=y1;x2=y2;x3=y3;t=yt} = y
            match xn with 
            | 0 -> true
            | 1 -> f x1 y1 
            | 2 -> f x1 y1 && f x2 y2 
            | _ -> f x1 y1 && f x2 y2 && f x3 y3 && loop xt yt
        loop x y

    let empty<'T> = ThreeListStatics<'T>.Empty

    let init n f = 
        let acc = 
            match n % 3 with
            | 0 -> ThreeListStatics.Empty
            | 1 -> TL(1, f (n-1), e, e, e)
            | _ -> TL(2, f (n-2), f (n-1), e, e)
        let rec loop acc i = if i = 0 then acc else loop (TL(3, f (i-3), f (i-2), f (i-1), acc)) (i-3) 
        loop acc (n-n%3) 

    let rec exists f {n=n;x1=x1;x2=x2;x3=x3;t=t}  =  
        match n with 
        | 0 -> false
        | 1 -> f x1
        | 2 -> f x1 || f x2
        | _ -> f x1 || f x2 || f x3 || exists f t

    let rec foldBack f {n=n;x1=x1;x2=x2;x3=x3;t=t} z =  
        match n with 
        | 0 -> z
        | 1 -> f x1 z
        | 2 -> f x1 (f x2 z)
        | _ -> f x1 (f x2 (f x3 (Array.foldBack f (toArray t) z)))

type ThreeList<'T> with 
    member x.Length = ThreeList.length x

(*
#time "on"

let check s v1 v2 = if (v1 <> v2) then printfn "FAIL: %s" s
for i in 0 .. 100 do
    check ("3lkcewoeiwvX" + string i) ([1..i] |> ThreeList.ofList |> ThreeList.toList) [1..i]
    check ("3lkcewoeiwvA" + string i) ([1..i] |> ThreeList.ofList |> ThreeList.map (fun i -> i + 1) |> ThreeList.toList) [2..i+1]
    check ("3lkcewoeiwvA" + string i) ([1..i] |> ThreeList.ofList |> Seq.map (fun i -> i + 1) |> Seq.toList) [2..i+1]
    check ("3lkcewoeiwvT" + string i) ([1..i] |> ThreeList.ofList |> ThreeList.mapi (fun i j -> (i, j)) |> ThreeList.toList) [ for i in 0..i-1 -> (i,i+1) ]
    check ("3lkcewoeiwvF" + string i) ([1..i] |> ThreeList.ofList |> ThreeList.toArray) [| 1..i |]
    check ("3lkcewoeiwvQ" + string i) (ThreeList.init i (fun i -> i + 1) |> ThreeList.toArray) [| 1..i |]
    check ("3lkcewoeiwvW" + string i) ([| 1..i |] |> ThreeList.ofArray |> ThreeList.toArray) [| 1..i |]
    check ("3lkcewoeiwvG" + string i) ([| 1..i |] |> ThreeList.ofArray |> ThreeList.exists (fun i -> i = 10)) (i >= 10)
    check ("3lkcewoeiwvH" + string i) (let x = ref 0 in [| 1..i |] |> ThreeList.ofArray |> ThreeList.iter (fun i -> x := !x + i); !x) (List.sum [ 1 .. i])
    check ("3lkcewoeiwvJ" + string i) (let x = ref 0 in [| 1..i |] |> ThreeList.ofArray |> ThreeList.iteri (fun j i -> x := !x + i); !x) (List.sum [ 1 .. i])
    check ("3lkcewoeiwvK" + string i) (let x = ref 0 in [| 1..i |] |> ThreeList.ofArray |> ThreeList.iteri (fun j i -> x := !x + j); !x) (List.sum [ 0 .. i-1])
    check ("3lkcewoeiwvK" + string i) (compare (ThreeList.ofList [0..i]) (ThreeList.ofList [0..i])) 0
    check ("3lkcewoeiwvK" + string i) (compare (ThreeList.ofList [0..i]) (ThreeList.ofList [0..i+1])) -1
    check ("3lkcewoeiwvK" + string i) (compare (ThreeList.ofList [0..i]) (ThreeList.ofList [0..i-1])) 1
    check ("3lkcewoeiwvK" + string i) (compare (ThreeList.ofList [0..i]) (ThreeList.ofList [1..i+1])) -1
    check ("3lkcewoeiwvK" + string i) (compare (ThreeList.ofList [0..i]) (ThreeList.ofList ([0..i-1] @ [i+1]))) -1

    check ("3lkcewoeiwvK" + string i) ((ThreeList.ofList [0..i]) = (ThreeList.ofList [0..i])) true
    check ("3lkcewoeiwvK" + string i) ((ThreeList.ofList [0..i]) = (ThreeList.ofList ([0..i-1] @ [i+1]))) false

module SpeedTestMapBigLIntist = 
    let fl1 = FourList.init 100000 (fun i -> i + 1)
    let tl1 = ThreeList.init 100000 (fun i -> i + 1)
    let l1 = List.init 100000 (fun i -> i + 1)
    let al1 = Array.init 100000 (fun i -> i + 1)


    //Real: 00:00:01.028, CPU: 00:00:00.982, GC gen0: 145, gen1: 67, gen2: 0
    //Real: 00:00:00.986, CPU: 00:00:00.967, GC gen0: 142, gen1: 65, gen2: 0
    for i in 0 .. 1000 do 
       fl1 |> FourList.map (fun i -> i + 1) |> ignore

    //Real: 00:00:01.157, CPU: 00:00:01.138, GC gen0: 165, gen1: 83, gen2: 0
    //Real: 00:00:01.115, CPU: 00:00:01.092, GC gen0: 163, gen1: 106, gen2: 0
    for i in 0 .. 1000 do 
       tl1 |> ThreeList.map (fun i -> i + 1) |> ignore


    // Real: 00:00:02.740, CPU: 00:00:02.714, GC gen0: 268, gen1: 136, gen2: 0
    // Real: 00:00:02.344, CPU: 00:00:02.324, GC gen0: 266, gen1: 266, gen2: 0
    for i in 0 .. 1000 do 
       l1 |> List.map (fun i -> i + 1) |> ignore

    // Real: 00:00:01.420, CPU: 00:00:01.575, GC gen0: 22, gen1: 22, gen2: 22
    // Real: 00:00:00.553, CPU: 00:00:00.655, GC gen0: 7, gen1: 6, gen2: 5
    // Real: 00:00:00.918, CPU: 00:00:01.092, GC gen0: 14, gen1: 13, gen2: 13
    // Real: 00:00:02.431, CPU: 00:00:02.636, GC gen0: 57, gen1: 57, gen2: 57
    // Real: 00:00:04.541, CPU: 00:00:04.773, GC gen0: 111, gen1: 111, gen2: 111
    // Real: 00:00:00.965, CPU: 00:00:01.107, GC gen0: 21, gen1: 17, gen2: 17
    // Real: 00:00:00.878, CPU: 00:00:00.998, GC gen0: 17, gen1: 16, gen2: 16
    for i in 0 .. 1000 do 
       al1 |> Array.map (fun i -> i + 1) |> ignore

module SpeedTestMapSmallIntList = 
    let fl1 = FourList.init 2 (fun i -> i + 1)
    let tl1 = ThreeList.init 2 (fun i -> i + 1)
    let l1 = List.init 2 (fun i -> i + 1)
    let al1 = Array.init 2 (fun i -> i + 1)
    let N = 20000000

    // Real: 00:00:00.579, CPU: 00:00:00.561, GC gen0: 279, gen1: 0, gen2: 0
    // Real: 00:00:00.599, CPU: 00:00:00.592, GC gen0: 279, gen1: 0, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.map (fun i -> i + 1) |> ignore


    // Real: 00:00:00.475, CPU: 00:00:00.483, GC gen0: 255, gen1: 0, gen2: 0
    // Real: 00:00:00.475, CPU: 00:00:00.468, GC gen0: 254, gen1: 0, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.map (fun i -> i + 1) |> ignore

    // Real: 00:00:00.893, CPU: 00:00:00.889, GC gen0: 280, gen1: 0, gen2: 0
    // Real: 00:00:00.896, CPU: 00:00:00.904, GC gen0: 280, gen1: 1, gen2: 0
    for i in 0 .. N do 
       l1 |> List.map (fun i -> i + 1) |> ignore

    // Real: 00:00:00.248, CPU: 00:00:00.249, GC gen0: 127, gen1: 0, gen2: 0
    // Real: 00:00:00.248, CPU: 00:00:00.249, GC gen0: 127, gen1: 0, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.map (fun i -> i + 1) |> ignore

module SpeedTestMapSmallStringList = 
    let fl1 = FourList.init 2 (fun i -> string i)
    let tl1 = ThreeList.init 2 (fun i -> string i)
    let l1 = List.init 2 (fun i -> string i)
    let al1 = Array.init 2 (fun i -> string i )
    let N = 10000000

    // Real: 00:00:00.311, CPU: 00:00:00.312, GC gen0: 140, gen1: 0, gen2: 0
    // Real: 00:00:00.313, CPU: 00:00:00.312, GC gen0: 139, gen1: 0, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.map (fun i -> i) |> ignore

    // Real: 00:00:00.285, CPU: 00:00:00.280, GC gen0: 127, gen1: 0, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.map (fun i -> i) |> ignore

    // Real: 00:00:00.699, CPU: 00:00:00.686, GC gen0: 141, gen1: 1, gen2: 1
    for i in 0 .. N do 
       l1 |> List.map (fun i -> i) |> ignore

    // Real: 00:00:00.225, CPU: 00:00:00.218, GC gen0: 76, gen1: 0, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.map (fun i -> i) |> ignore


module SpeedTestMapSmallRefListSize2 = 
    type X = A of int | B of int
    let fl1 = FourList.init 2 (fun i -> A i)
    let tl1 = ThreeList.init 2 (fun i -> A i)
    let l1 = List.init 2 (fun i -> A i)
    let al1 = Array.init 2 (fun i -> A i )
    let N = 10000000

    //Real: 00:00:00.528, CPU: 00:00:00.530, GC gen0: 216, gen1: 1, gen2: 0
    //Real: 00:00:00.538, CPU: 00:00:00.530, GC gen0: 216, gen1: 0, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:00.615, CPU: 00:00:00.624, GC gen0: 204, gen1: 0, gen2: 0
    //Real: 00:00:00.528, CPU: 00:00:00.514, GC gen0: 204, gen1: 1, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:00.932, CPU: 00:00:00.904, GC gen0: 216, gen1: 0, gen2: 0
    for i in 0 .. N do 
       l1 |> List.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:00.807, CPU: 00:00:00.811, GC gen0: 153, gen1: 0, gen2: 0
    // Real: 00:00:00.812, CPU: 00:00:00.811, GC gen0: 153, gen1: 0, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.map (function A i -> B i | B i -> A i) |> ignore

module SpeedTestMapSmallRefListSize10 = 
    type X = A of int | B of int
    let size = 10
    let tl1 = ThreeList.init size (fun i -> A i)
    let l1 = List.init size (fun i -> A i)
    let al1 = Array.init size (fun i -> A i )
    let N = 10000000

    // Real: 00:00:02.562, CPU: 00:00:02.527, GC gen0: 775, gen1: 0, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:03.372, CPU: 00:00:03.385, GC gen0: 928, gen1: 0, gen2: 0
    for i in 0 .. N do 
       l1 |> List.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:03.524, CPU: 00:00:03.510, GC gen0: 559, gen1: 0, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.map (function A i -> B i | B i -> A i) |> ignore

module SpeedTestMapSmallRefListSize1 = 
    type X = A of int | B of int
    let size = 1
    let fl1 = FourList.init size (fun i -> A i)
    let tl1 = ThreeList.init size (fun i -> A i)
    let l1 = List.init size (fun i -> A i)
    let al1 = Array.init size (fun i -> A i )
    let N = 100000000

    //Real: 00:00:04.161, CPU: 00:00:04.087, GC gen0: 1780, gen1: 1, gen2: 0
    //Real: 00:00:04.202, CPU: 00:00:04.165, GC gen0: 1780, gen1: 0, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:03.944, CPU: 00:00:03.915, GC gen0: 1653, gen1: 1, gen2: 0
    // Real: 00:00:03.983, CPU: 00:00:03.900, GC gen0: 1653, gen1: 1, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:04.629, CPU: 00:00:04.586, GC gen0: 1272, gen1: 1, gen2: 0
    // Real: 00:00:04.725, CPU: 00:00:04.664, GC gen0: 1271, gen1: 0, gen2: 0
    for i in 0 .. N do 
       l1 |> List.map (function A i -> B i | B i -> A i) |> ignore

    // Real: 00:00:04.500, CPU: 00:00:04.430, GC gen0: 1018, gen1: 1, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.map (function A i -> B i | B i -> A i) |> ignore

module SpeedTestOfListSmallRefListSize2 = 
    type X = A of int | B of int
    let fl1 = FourList.init 2 (fun i -> A i)
    let tl1 = ThreeList.init 2 (fun i -> A i)
    let l1 = List.init 2 (fun i -> A i)
    let al1 = Array.init 2 (fun i -> A i )
    let N = 20000000

    //Real: 00:00:00.465, CPU: 00:00:00.468, GC gen0: 204, gen1: 204, gen2: 0
    for i in 0 .. N do 
       l1 |> FourList.ofList |> ignore

    // Real: 00:00:00.407, CPU: 00:00:00.390, GC gen0: 178, gen1: 178, gen2: 0
    for i in 0 .. N do 
       l1 |> ThreeList.ofList |> ignore

    // Real: 00:00:01.652, CPU: 00:00:01.591, GC gen0: 153, gen1: 153, gen2: 0
    for i in 0 .. N do 
       l1 |> Array.ofList |> ignore

module SpeedTestToListSmallRefListSize2 = 
    type X = A of int | B of int
    let fl1 = FourList.init 2 (fun i -> A i)
    let tl1 = ThreeList.init 2 (fun i -> A i)
    let l1 = List.init 2 (fun i -> A i)
    let al1 = Array.init 2 (fun i -> A i )
    let N = 20000000

    //Real: 00:00:00.895, CPU: 00:00:00.889, GC gen0: 202, gen1: 0, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.toList |> ignore

    // Real: 00:00:00.868, CPU: 00:00:00.873, GC gen0: 203, gen1: 1, gen2: 0
    // Real: 00:00:00.895, CPU: 00:00:00.889, GC gen0: 204, gen1: 1, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.toList |> ignore

    // Real: 00:00:01.043, CPU: 00:00:01.045, GC gen0: 204, gen1: 0, gen2: 0
    // Real: 00:00:01.071, CPU: 00:00:01.060, GC gen0: 203, gen1: 1, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.toList |> ignore


module SpeedTestToListSmallRefListSize0 = 
    type X = A of int | B of int
    let size = 0
    let fl1 = FourList.init size (fun i -> A i)
    let tl1 = ThreeList.init size (fun i -> A i)
    let l1 = List.init size (fun i -> A i)
    let al1 = Array.init size (fun i -> A i )
    let N = 30000000

    //Real: 00:00:00.530, CPU: 00:00:00.530, GC gen0: 0, gen1: 0, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.toList |> ignore

    // Real: 00:00:00.528, CPU: 00:00:00.514, GC gen0: 0, gen1: 0, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.toList |> ignore

    // Real: 00:00:00.635, CPU: 00:00:00.624, GC gen0: 0, gen1: 0, gen2: 0
    for i in 0 .. N do 
       al1 |> Array.toList |> ignore

module SpeedTestToArraySmallRefListSize2 = 
    type X = A of int | B of int
    let size = 2
    let fl1 = FourList.init size (fun i -> A i)
    let tl1 = ThreeList.init size (fun i -> A i)
    let l1 = List.init size (fun i -> A i)
    let al1 = Array.init size (fun i -> A i )
    let N = 30000000

    // Real: 00:00:01.970, CPU: 00:00:01.950, GC gen0: 229, gen1: 1, gen2: 0
    for i in 0 .. N do 
       fl1 |> FourList.toArray |> ignore

    // Real: 00:00:02.043, CPU: 00:00:02.043, GC gen0: 229, gen1: 1, gen2: 0
    for i in 0 .. N do 
       tl1 |> ThreeList.toArray |> ignore

    // Real: 00:00:02.199, CPU: 00:00:02.106, GC gen0: 230, gen1: 1, gen2: 1
    for i in 0 .. N do 
       l1 |> List.toArray |> ignore

*)
