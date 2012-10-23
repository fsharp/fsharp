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


module (* internal *) Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
#nowarn "1178" // The struct, record or union type 'internal_instr_extension' is not structurally comparable because the type


open System
open System.Collections
open System.Collections.Generic
open Internal.Utilities
open Internal.Utilities.Collections

// Logical shift right treating int32 as unsigned integer.
// Code that uses this should probably be adjusted to use unsigned integer types.
let (>>>&) (x:int32) (n:int32) = int32 (uint32 x >>> n)

let notlazy v = Lazy.CreateFromValue v

let isSome x = match x with None -> false | _ -> true
let isNone x = match x with None -> true | _ -> false
let isNil x = match x with [] -> true | _ -> false
let nonNil x = match x with [] -> false | _ -> true
let isNull (x : 'T) = match (x :> obj) with null -> true | _ -> false
let isNonNull (x : 'T) = match (x :> obj) with null -> false | _ -> true
let nonNull msg x = if isNonNull x then x else failwith ("null: " ^ msg) 
let (===) x y = LanguagePrimitives.PhysicalEquality x y

//-------------------------------------------------------------------------
// Library: projections
//------------------------------------------------------------------------

let foldOn p f z x = f z (p x)

let notFound() = raise (KeyNotFoundException())

module Order = 
    let orderBy (p : 'T -> 'U) = 
        { new IComparer<'T> with member __.Compare(x,xx) = compare (p x) (p xx) }

    let orderOn p (pxOrder: IComparer<'U>) = 
        { new IComparer<'T> with member __.Compare(x,xx) = pxOrder.Compare (p x, p xx) }

    let toFunction (pxOrder: IComparer<'U>) x y = pxOrder.Compare(x,y)

//-------------------------------------------------------------------------
// Library: arrays,lists,options
//-------------------------------------------------------------------------

module Array = 

    let mapq f inp =
        match inp with
        | [| |] -> inp
        | _ -> 
            let res = Array.map f inp 
            let len = inp.Length 
            let mutable eq = true
            let mutable i = 0 
            while eq && i < len do 
                if not (inp.[i] === res.[i]) then eq <- false;
                i <- i + 1
            if eq then inp else res

    let forall2 f (arr1:'T array) (arr2:'T array) =
        let len1 = arr1.Length 
        let len2 = arr2.Length 
        if len1 <> len2 then invalidArg "Array.forall2" "len1"
        let rec loop i = (i >= len1) || (f arr1.[i] arr2.[i] && loop (i+1))
        loop 0

    let lengthsEqAndForall2 p l1 l2 = 
        Array.length l1 = Array.length l2 &&
        Array.forall2 p l1 l2

    let mapFold f s l = 
        let mutable acc = s
        let n = Array.length l
        let mutable res = Array.zeroCreate n
        for i = 0 to n - 1 do
            let h',s' = f acc l.[i]
            res.[i] <- h';
            acc <- s'
        res, acc


    // REVIEW: systematically eliminate fmap/mapFold duplication. 
    // They only differ by the tuple returned by the function.
    let fmap f s l = 
        let mutable acc = s
        let n = Array.length l
        let mutable res = Array.zeroCreate n
        for i = 0 to n - 1 do
            let s',h' = f acc l.[i]
            res.[i] <- h'
            acc <- s'
        acc, res

    let order (eltOrder: IComparer<'T>) = 
        { new IComparer<array<'T>> with 
              member __.Compare(xs,ys) = 
                  let c = compare xs.Length ys.Length 
                  if c <> 0 then c else
                  let rec loop i = 
                      if i >= xs.Length then 0 else
                      let c = eltOrder.Compare(xs.[i], ys.[i])
                      if c <> 0 then c else
                      loop (i+1)
                  loop 0 }

    let existsOne p l = 
        let rec forallFrom p l n =
          (n >= Array.length l) || (p l.[n] && forallFrom p l (n+1))

        let rec loop p l n =
            (n < Array.length l) && 
            (if p l.[n] then forallFrom (fun x -> not (p x)) l (n+1) else loop p l (n+1))
          
        loop p l 0

    
    let findFirstIndexWhereTrue (arr: _[]) p = 
        let rec look lo hi = 
            assert ((lo >= 0) && (hi >= 0))
            assert ((lo <= arr.Length) && (hi <= arr.Length))
            if lo = hi then lo
            else
                let i = (lo+hi)/2
                if p arr.[i] then 
                    if i = 0 then i 
                    else
                        if p arr.[i-1] then 
                            look lo i
                        else 
                            i
                else
                    // not true here, look after
                    look (i+1) hi
        look 0 arr.Length
      
        
module Option = 
    let mapFold f s opt = 
        match opt with 
        | None -> None,s 
        | Some x -> let x',s' = f s x in Some x',s'

    let otherwise opt dflt = 
        match opt with 
        | None -> dflt 
        | Some x -> x

    // REVIEW: systematically eliminate fmap/mapFold duplication
    let fmap f z l = 
        match l with 
        | None   -> z,None
        | Some x -> let z,x = f z x
                    z,Some x

    let fold f z x = 
        match x with 
        | None -> z 
        | Some x -> f z x


module List = 

    let sortWithOrder (c: IComparer<'T>) elements = List.sortWith (Order.toFunction c) elements
    
    let splitAfter n l = 
        let rec split_after_acc n l1 l2 = if n <= 0 then List.rev l1,l2 else split_after_acc (n-1) ((List.head l2):: l1) (List.tail l2) 
        split_after_acc n [] l

    let existsi f xs = 
       let rec loop i xs = match xs with [] -> false | h::t -> f i h || loop (i+1) t
       loop 0 xs
    
    let lengthsEqAndForall2 p l1 l2 = 
        List.length l1 = List.length l2 &&
        List.forall2 p l1 l2

    let rec findi n f l = 
        match l with 
        | [] -> None
        | h::t -> if f h then Some (h,n) else findi (n+1) f t

    let chop n l = 
        if n = List.length l then (l,[]) else // avoids allocation unless necessary 
        let rec loop n l acc = 
            if n <= 0 then (List.rev acc,l) else 
            match l with 
            | [] -> failwith "List.chop: overchop"
            | (h::t) -> loop (n-1) t (h::acc) 
        loop n l [] 

    let take n l = 
        if n = List.length l then l else 
        let rec loop acc n l = 
            match l with
            | []    -> List.rev acc
            | x::xs -> if n<=0 then List.rev acc else loop (x::acc) (n-1) xs

        loop [] n l

    let rec drop n l = 
        match l with 
        | []    -> []
        | _::xs -> if n=0 then l else drop (n-1) xs


    let splitChoose select l =
        let rec ch acc1 acc2 l = 
            match l with 
            | [] -> List.rev acc1,List.rev acc2
            | x::xs -> 
                match select x with
                | Choice1Of2 sx -> ch (sx::acc1) acc2 xs
                | Choice2Of2 sx -> ch acc1 (sx::acc2) xs

        ch [] [] l

    let mapq (f: 'T -> 'T) inp =
        assert not (typeof<'T>.IsValueType) 
        match inp with
        | [] -> inp
        | _ -> 
            let res = List.map f inp 
            let rec check l1 l2 = 
                match l1,l2 with 
                | h1::t1,h2::t2 -> 
                    System.Runtime.CompilerServices.RuntimeHelpers.Equals(h1,h2) && check t1 t2
                | _ -> true
            if check inp res then inp else res
        
    let frontAndBack l = 
        let rec loop acc l = 
            match l with
            | [] -> 
                System.Diagnostics.Debug.Assert(false, "empty list")
                invalidArg "l" "empty list" 
            | [h] -> List.rev acc,h
            | h::t -> loop  (h::acc) t
        loop [] l

    let tryRemove f inp = 
        let rec loop acc l = 
            match l with
            | [] -> None
            | h :: t -> if f h then Some (h, List.rev acc @ t) else loop (h::acc) t
        loop [] inp            
    //tryRemove  (fun x -> x = 2) [ 1;2;3] = Some (2, [1;3])
    //tryRemove  (fun x -> x = 3) [ 1;2;3;4;5] = Some (3, [1;2;4;5])
    //tryRemove  (fun x -> x = 3) [] = None
            
    let headAndTail l =
        match l with 
        | [] -> 
            System.Diagnostics.Debug.Assert(false, "empty list")
            failwith "List.headAndTail"
        | h::t -> h,t

    let zip4 l1 l2 l3 l4 = 
        List.zip l1 (List.zip3 l2 l3 l4) |> List.map (fun (x1,(x2,x3,x4)) -> (x1,x2,x3,x4))

    let unzip4 l = 
        let a,b,cd = List.unzip3 (List.map (fun (x,y,z,w) -> (x,y,(z,w))) l)
        let c,d = List.unzip cd
        a,b,c,d

    let rec iter3 f l1 l2 l3 = 
        match l1,l2,l3 with 
        | h1::t1, h2::t2, h3::t3 -> f h1 h2 h3; iter3 f t1 t2 t3
        | [], [], [] -> ()
        | _ -> failwith "iter3"

    let takeUntil p l =
        let rec loop acc l =
            match l with
            | [] -> List.rev acc,[]
            | x::xs -> if p x then List.rev acc, l else loop (x::acc) xs
        loop [] l

    let order (eltOrder: IComparer<'T>) =
        { new IComparer<list<'T>> with 
              member __.Compare(xs,ys) = 
                  let rec loop xs ys = 
                      match xs,ys with
                      | [],[]       ->  0
                      | [],_        -> -1
                      | _,[]       ->  1
                      | x::xs,y::ys -> let cxy = eltOrder.Compare(x,y)
                                       if cxy=0 then loop xs ys else cxy 
                  loop xs ys }


    let rec last l = match l with [] -> failwith "last" | [h] -> h | _::t -> last t
    module FrontAndBack = 
        let (|NonEmpty|Empty|) l = match l with [] -> Empty | _ -> NonEmpty(frontAndBack l)

    let replicate x n = 
        Array.toList (Array.create x n)

    let range n m = [ n .. m ]

    let indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException("An index satisfying the predicate was not found in the collection"))

    let rec assoc x l = 
        match l with 
        | [] -> indexNotFound()
        | ((h,r)::t) -> if x = h then r else assoc x t

    let rec memAssoc x l = 
        match l with 
        | [] -> false
        | ((h,_)::t) -> x = h || memAssoc x t

    let rec contains x l = match l with [] -> false | h::t -> x = h || contains x t

    let rec memq x l = 
        match l with 
        | [] -> false 
        | h::t -> LanguagePrimitives.PhysicalEquality x h || memq x t

    let mem x l = contains x l

    // must be tail recursive 
    let mapFold f s l = 
        // microbenchmark suggested this implementation is faster than the simpler recursive one, and this function is called a lot
        let mutable s = s
        let mutable r = []
        let mutable l = l
        let mutable finished = false
        while not finished do
          match l with
          | x::xs -> let x',s' = f s x
                     s <- s'
                     r <- x' :: r
                     l <- xs
          | _ -> finished <- true
        List.rev r, s

    // note: not tail recursive 
    let rec mapfoldBack f l s = 
        match l with 
        | [] -> ([],s)
        | h::t -> 
           let t',s = mapfoldBack f t s
           let h',s = f h s
           (h'::t', s)

    let mapNth n f xs =
        let rec mn i = function
          | []    -> []
          | x::xs -> if i=n then f x::xs else x::mn (i+1) xs
       
        mn 0 xs

    let rec until p l = match l with [] -> [] | h::t -> if p h then [] else h :: until p t 

    let count pred xs = List.fold (fun n x -> if pred x then n+1 else n) 0 xs

    let rec private repeatA n x acc = if n <= 0 then acc else repeatA (n-1) x (x::acc)
    let repeat n x = repeatA n x []

    (* WARNING: not tail-recursive *)
    let mapHeadTail fhead ftail = function
      | []    -> []
      | [x]   -> [fhead x]
      | x::xs -> fhead x :: List.map ftail xs

    let collectFold f s l = 
      let l, s = mapFold f s l
      List.concat l, s

    let singleton x = [x]

    // note: must be tail-recursive 
    let rec private fmapA f z l acc =
      match l with
      | []    -> z,List.rev acc
      | x::xs -> let z,x = f z x
                 fmapA f z xs (x::acc)
                 
    // note: must be tail-recursive 
    // REVIEW: systematically eliminate fmap/mapFold duplication
    let fmap f z l = fmapA f z l []

    let collect2 f xs ys = List.concat (List.map2 f xs ys)

    let iterSquared f xss = xss |> List.iter (List.iter f)
    let collectSquared f xss = xss |> List.collect (List.collect f)
    let mapSquared f xss = xss |> List.map (List.map f)
    let mapfoldSquared f xss = xss |> mapFold (mapFold f)
    let forallSquared f xss = xss |> List.forall (List.forall f)
    let mapiSquared f xss = xss |> List.mapi (fun i xs -> xs |> List.mapi (fun j x -> f i j x))
    let existsSquared f xss = xss |> List.exists (fun xs -> xs |> List.exists (fun x -> f x))

module String = 
    let indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException("An index for the character was not found in the string"))

    let make (n: int) (c: char) : string = new System.String(c, n)

    let get (str:string) i = str.[i]

    let sub (s:string) (start:int) (len:int) = s.Substring(start,len)

    let index (s:string) (c:char) =  
        let r = s.IndexOf(c) 
        if r = -1 then indexNotFound() else r

    let rindex (s:string) (c:char) =
        let r =  s.LastIndexOf(c) 
        if r = -1 then indexNotFound() else r

    let contains (s:string) (c:char) = 
        s.IndexOf(c,0,String.length s) <> -1

    let order = LanguagePrimitives.FastGenericComparer<string>
   
    let lowercase (s:string) =
        s.ToLowerInvariant()

    let uppercase (s:string) =
        s.ToUpperInvariant()

    let isUpper (s:string) = 
        s.Length >= 1 && System.Char.IsUpper s.[0] && not (System.Char.IsLower s.[0])
        
    let capitalize (s:string) =
        if s.Length = 0 then s 
        else uppercase s.[0..0] + s.[ 1.. s.Length - 1 ]

    let uncapitalize (s:string) =
        if s.Length = 0 then  s
        else lowercase s.[0..0] + s.[ 1.. s.Length - 1 ]


    let tryDropPrefix (s:string) (t:string) = 
        if s.StartsWith t then 
            Some s.[t.Length..s.Length - 1]
        else 
            None

    let tryDropSuffix (s:string) (t:string) = 
        if s.EndsWith t then
            Some s.[0..s.Length - t.Length - 1]
        else
            None

    let hasPrefix s t = isSome (tryDropPrefix s t)
    let dropPrefix s t = match (tryDropPrefix s t) with Some(res) -> res | None -> failwith "dropPrefix"

    let dropSuffix s t = match (tryDropSuffix s t) with Some(res) -> res | None -> failwith "dropSuffix"

module Dictionary = 

    let inline ofList l = 
        let dict = new System.Collections.Generic.Dictionary<_,_>(List.length l, HashIdentity.Structural)
        l |> List.iter (fun (k,v) -> dict.Add(k,v))
        dict


// FUTURE CLEANUP: remove this adhoc collection
type Hashset<'T> = Dictionary<'T,int>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Hashset = 
    let create (n:int) = new Hashset<'T>(n, HashIdentity.Structural)
    let add (t: Hashset<'T>) x = if not (t.ContainsKey x) then t.[x] <- 0
    let fold f (t:Hashset<'T>) acc = Seq.fold (fun z (KeyValue(x,_)) -> f x z) acc t 
    let ofList l = 
        let t = new Hashset<'T>(List.length l, HashIdentity.Structural)
        l |> List.iter (fun x -> t.[x] <- 0)
        t
        
module Lazy = 
    let force (x: Lazy<'T>) = x.Force()

//---------------------------------------------------
// Lists as sets. This is almost always a bad data structure and should be eliminated from the compiler.  

module ListSet =
    let insert e l =
        if List.mem e l then l else e::l

//---------------------------------------------------
// Misc

/// Get an initialization hole 
let getHole r = match !r with None -> failwith "getHole" | Some x -> x

module Map = 
    let tryFindMulti k map = match Map.tryFind k map with Some res -> res | None -> []

type ResultOrException<'TResult> =
    | Result of 'TResult
    | Exception of System.Exception
                     

//-------------------------------------------------------------------------
// Library: extensions to flat list  (immutable arrays)
//------------------------------------------------------------------------
#if FLAT_LIST_AS_ARRAY_STRUCT
//#else
module FlatList =

    let order (eltOrder: IComparer<_>) =
        { new IComparer<FlatList<_>> with 
            member __.Compare(xs,ys) =
                  match xs.array,ys.array with 
                  | null,null -> 0
                  | _,null -> 1
                  | null,_ -> -1
                  | arr1,arr2 -> Array.order eltOrder arr1 arr2 }

    let mapq f (x:FlatList<_>) = 
        match x.array with 
        | null -> x
        | arr -> 
            let arr' = Array.map f arr in 
            let n = arr.Length in 
            let rec check i = if i >= n then true else arr.[i] === arr'.[i] && check (i+1) 
            if check 0 then x else FlatList(arr')

    let mapFold f acc (x:FlatList<_>) = 
        match x.array with
        | null -> 
            FlatList.Empty,acc
        | arr -> 
            let  arr,acc = Array.mapFold f acc x.array
            FlatList(arr),acc

    // REVIEW: systematically eliminate fmap/mapFold duplication
    let fmap f acc (x:FlatList<_>) = 
        match x.array with
        | null -> 
            acc,FlatList.Empty
        | arr -> 
            let  acc,arr = Array.fmap f acc x.array
            acc,FlatList(arr)
#endif
#if FLAT_LIST_AS_LIST

#else

module FlatList =
    let toArray xs = List.toArray xs
    let choose f xs = List.choose f xs
    let order eltOrder = List.order eltOrder 
    let mapq f (x:FlatList<_>) = List.mapq f x
    let mapFold f acc (x:FlatList<_>) =  List.mapFold f acc x
    let fmap f acc (x:FlatList<_>) =  List.fmap f acc x

#endif

#if FLAT_LIST_AS_ARRAY
//#else
module FlatList =
    let order eltOrder = Array.order eltOrder 
    let mapq f x = Array.mapq f x
    let mapFold f acc x =  Array.mapFold f acc x
    let fmap f acc x =  Array.fmap f acc x
#endif



/// Computations that can cooperatively yield by returning a continuation
///
///    - Any yield of a NotYetDone should typically be "abandonable" without adverse consequences. No resource release
///      will be called when the computation is abandoned.
///
///    - Computations suspend via a NotYetDone may use local state (mutables), where these are
///      captured by the NotYetDone closure. Computations do not need to be restartable.
///
///    - The key thing is that you can take an Eventually value and run it with 
///      Eventually.repeatedlyProgressUntilDoneOrTimeShareOver
type Eventually<'T> = 
    | Done of 'T 
    | NotYetDone of (unit -> Eventually<'T>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Eventually = 
    let rec box e = 
        match e with 
        | Done x -> Done (Operators.box x) 
        | NotYetDone (work) -> NotYetDone (fun () -> box (work()))

    let rec forceWhile check e  = 
        match e with 
        | Done x -> Some(x)
        | NotYetDone (work) -> 
            if not(check()) 
            then None
            else forceWhile check (work()) 

    let force e = Option.get (forceWhile (fun () -> true) e)
        
    /// Keep running the computation bit by bit until a time limit is reached.
#if SILVERLIGHT
    // There is no Stopwatch on Silverlight, so use DateTime.Now. I'm not sure of the pros and cons of this.
    // An alternative is just to always force the computation all the way to the end.
    //let repeatedlyProgressUntilDoneOrTimeShareOver _timeShareInMilliseconds runner e = 
    //    Done (runner (fun () -> force e))
    let repeatedlyProgressUntilDoneOrTimeShareOver (timeShareInMilliseconds:int64) runner e = 
        let rec runTimeShare e = 
          runner (fun () -> 
            let sw = System.DateTime.Now
            let rec loop e = 
                match e with 
                | Done _ -> e
                | NotYetDone (work) -> 
                    let ts = System.DateTime.Now - sw 
                    if ts.TotalMilliseconds > float timeShareInMilliseconds then 
                        NotYetDone(fun () -> runTimeShare e) 
                    else 
                        loop(work())
            loop e)
        runTimeShare e
#else    
    /// The runner gets called each time the computation is restarted
    let repeatedlyProgressUntilDoneOrTimeShareOver timeShareInMilliseconds runner e = 
        let sw = new System.Diagnostics.Stopwatch() 
        let rec runTimeShare e = 
          runner (fun () -> 
            sw.Reset()
            sw.Start(); 
            let rec loop(e) = 
                match e with 
                | Done _ -> e
                | NotYetDone work -> 
                    if sw.ElapsedMilliseconds > timeShareInMilliseconds then 
                        sw.Stop();
                        NotYetDone(fun () -> runTimeShare e) 
                    else 
                        loop(work())
            loop(e))
        runTimeShare e
#endif

    let rec bind k e = 
        match e with 
        | Done x -> k x 
        | NotYetDone work -> NotYetDone (fun () -> bind k (work()))

    let fold f acc seq = 
        (Done acc,seq) ||> Seq.fold  (fun acc x -> acc |> bind (fun acc -> f acc x))
        
    let rec catch e = 
        match e with 
        | Done x -> Done(Result x)
        | NotYetDone work -> 
            NotYetDone (fun () -> 
                let res = try Result(work()) with | e -> Exception e 
                match res with 
                | Result cont -> catch cont
                | Exception e -> Done(Exception e))
    
    let delay f = NotYetDone (fun () -> f())

    let tryFinally e compensation =    
        catch (e) 
        |> bind (fun res ->  compensation();
                             match res with 
                             | Result v -> Eventually.Done v
                             | Exception e -> raise e)

    let tryWith e handler =    
        catch e
        |> bind (function Result v -> Done v | Exception e -> handler e)
    
type EventuallyBuilder() = 
    member x.Bind(e,k) = Eventually.bind k e
    member x.Return(v) = Eventually.Done v
    member x.ReturnFrom(v) = v
    member x.Combine(e1,e2) = e1 |> Eventually.bind (fun () -> e2)
    member x.TryWith(e,handler) = Eventually.tryWith e handler
    member x.TryFinally(e,compensation) =  Eventually.tryFinally e compensation
    member x.Delay(f) = Eventually.delay f
    member x.Zero() = Eventually.Done ()


let eventually = new EventuallyBuilder()

(*
let _ = eventually { return 1 }
let _ = eventually { let x = 1 in return 1 }
let _ = eventually { let! x = eventually { return 1 } in return 1 }
let _ = eventually { try return (failwith "") with _ -> return 1 }
let _ = eventually { use x = null in return 1 }
*)

//---------------------------------------------------------------------------
// generate unique stamps
//---------------------------------------------------------------------------

type UniqueStampGenerator<'T when 'T : equality>() = 
    let encodeTab = new Dictionary<'T,int>(HashIdentity.Structural)
    let mutable nItems = 0
    let encode str = 
        if encodeTab.ContainsKey(str)
        then
            encodeTab.[str]
        else
            let idx = nItems
            encodeTab.[str] <- idx
            nItems <- nItems + 1
            idx
    member this.Encode(str)  = encode str

//---------------------------------------------------------------------------
// memoize tables (all entries cached, never collected)
//---------------------------------------------------------------------------
    
type MemoizationTable<'T,'U>(compute: 'T -> 'U, keyComparer: IEqualityComparer<'T>, ?canMemoize) = 
    
    let table = new System.Collections.Generic.Dictionary<'T,'U>(keyComparer) 
    member t.Apply(x) = 
        if (match canMemoize with None -> true | Some f -> f x) then 
            let mutable res = Unchecked.defaultof<'U>
            let ok = table.TryGetValue(x,&res)
            if ok then res 
            else
                lock table (fun () -> 
                    let mutable res = Unchecked.defaultof<'U> 
                    let ok = table.TryGetValue(x,&res)
                    if ok then res 
                    else
                        let res = compute x
                        table.[x] <- res;
                        res)
        else compute x


exception UndefinedException

type LazyWithContextFailure(exn:exn) =
    static let undefined = new LazyWithContextFailure(UndefinedException)
    member x.Exception = exn
    static member Undefined = undefined
        
/// Just like "Lazy" but EVERY forcer must provide an instance of "ctxt", e.g. to help track errors
/// on forcing back to at least one sensible user location
[<DefaultAugmentation(false)>]
[<NoEquality; NoComparison>]
type LazyWithContext<'T,'ctxt> = 
    { /// This field holds the result of a successful computation. It's initial value is Unchecked.defaultof
      mutable value : 'T
      /// This field holds either the function to run or a LazyWithContextFailure object recording the exception raised 
      /// from running the function. It is null if the thunk has been evaluated successfully.
      mutable funcOrException: obj;
      /// A helper to ensure we rethrow the "original" exception
      findOriginalException : exn -> exn }
    static member Create(f: ('ctxt->'T), findOriginalException) : LazyWithContext<'T,'ctxt> = 
        { value = Unchecked.defaultof<'T>;
          funcOrException = box f;
          findOriginalException = findOriginalException }
    static member NotLazy(x:'T) : LazyWithContext<'T,'ctxt> = 
        { value = x;
          funcOrException = null;
          findOriginalException = id }
    member x.IsDelayed = (match x.funcOrException with null -> false | :? LazyWithContextFailure -> false | _ -> true)
    member x.IsForced = (match x.funcOrException with null -> true | _ -> false)
    member x.Force(ctxt:'ctxt) =  
        match x.funcOrException with 
        | null -> x.value 
        | _ -> 
            // Enter the lock in case another thread is in the process of evaluting the result
            System.Threading.Monitor.Enter(x);
            try 
                x.UnsynchronizedForce(ctxt)
            finally
                System.Threading.Monitor.Exit(x)

    member x.UnsynchronizedForce(ctxt) = 
        match x.funcOrException with 
        | null -> x.value 
        | :? LazyWithContextFailure as res -> 
              // Re-raise the original exception 
              raise (x.findOriginalException res.Exception)
        | :? ('ctxt -> 'T) as f -> 
              x.funcOrException <- box(LazyWithContextFailure.Undefined)
              try 
                  let res = f ctxt 
                  x.value <- res; 
                  x.funcOrException <- null; 
                  res
              with e -> 
                  x.funcOrException <- box(new LazyWithContextFailure(e)); 
                  reraise()
        | _ -> 
            failwith "unreachable"

    

// --------------------------------------------------------------------
// Intern tables to save space.
// -------------------------------------------------------------------- 

module Tables = 
    let memoize f = 
        let t = new Dictionary<_,_>(1000, HashIdentity.Structural)
        fun x -> 
            let mutable res = Unchecked.defaultof<_>
            if t.TryGetValue(x, &res) then 
                res 
            else
                res <- f x; t.[x] <- res;  res

//-------------------------------------------------------------------------
// Library: Name maps
//------------------------------------------------------------------------

type NameMap<'T> = Map<string,'T>
type NameMultiMap<'T> = NameMap<'T list>
type MultiMap<'T,'U when 'T : comparison> = Map<'T,'U list>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameMap = 

    let empty = Map.empty
    let range m = List.rev (Map.foldBack (fun _ x sofar -> x :: sofar) m [])
    let foldBack f (m:NameMap<'T>) z = Map.foldBack f m z
    let forall f m = Map.foldBack (fun x y sofar -> sofar && f x y) m true
    let exists f m = Map.foldBack (fun x y sofar -> sofar || f x y) m false
    let ofKeyedList f l = List.foldBack (fun x acc -> Map.add (f x) x acc) l Map.empty
    let ofList l : NameMap<'T> = Map.ofList l
    let ofFlatList (l:FlatList<_>) : NameMap<'T> = FlatList.toMap l
    let toList (l: NameMap<'T>) = Map.toList l
    let layer (m1 : NameMap<'T>) m2 = Map.foldBack Map.add m1 m2

    (* not a very useful function - only called in one place - should be changed *)
    let layerAdditive addf m1 m2 = 
      Map.foldBack (fun x y sofar -> Map.add x (addf (Map.tryFindMulti x sofar) y) sofar) m1 m2

    // Union entries by identical key, using the provided function to union sets of values
    let union unionf (ms: NameMap<_> seq) = 
        seq { for m in ms do yield! m } 
           |> Seq.groupBy (fun (KeyValue(k,_v)) -> k) 
           |> Seq.map (fun (k,es) -> (k,unionf (Seq.map (fun (KeyValue(_k,v)) -> v) es))) 
           |> Map.ofSeq

    (* For every entry in m2 find an entry in m1 and fold *)
    let subfold2 errf f m1 m2 acc =
        Map.foldBack (fun n x2 acc -> try f n (Map.find n m1) x2 acc with :? KeyNotFoundException -> errf n x2) m2 acc

    let suball2 errf p m1 m2 = subfold2 errf (fun _ x1 x2 acc -> p x1 x2 && acc) m1 m2 true

    let mapFold f s (l: NameMap<'T>) = 
        Map.foldBack (fun x y (l',s') -> let y',s'' = f s' x y in Map.add x y' l',s'') l (Map.empty,s)

    let foldBackRange f (l: NameMap<'T>) acc = Map.foldBack (fun _ y acc -> f y acc) l acc

    let filterRange f (l: NameMap<'T>) = Map.foldBack (fun x y acc -> if f y then Map.add x y acc else acc) l Map.empty

    let mapFilter f (l: NameMap<'T>) = Map.foldBack (fun x y acc -> match f y with None -> acc | Some y' -> Map.add x y' acc) l Map.empty

    let map f (l : NameMap<'T>) = Map.map (fun _ x -> f x) l

    let iter f (l : NameMap<'T>) = Map.iter (fun _k v -> f v) l

    let iteri f (l : NameMap<'T>) = Map.iter f l

    let mapi f (l : NameMap<'T>) = Map.map f l

    let partition f (l : NameMap<'T>) = Map.filter (fun _ x-> f x) l, Map.filter (fun _ x -> not (f x)) l

    let mem v (m: NameMap<'T>) = Map.containsKey v m

    let find v (m: NameMap<'T>) = Map.find v m

    let tryFind v (m: NameMap<'T>) = Map.tryFind v m 

    let add v x (m: NameMap<'T>) = Map.add v x m

    let isEmpty (m: NameMap<'T>) = (Map.isEmpty  m)

    let existsInRange p m =  Map.foldBack (fun _ y acc -> acc || p y) m false 

    let tryFindInRange p m = 
        Map.foldBack (fun _ y acc -> 
             match acc with 
             | None -> if p y then Some y else None 
             | _ -> acc) m None 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameMultiMap = 
    let existsInRange f (m: NameMultiMap<'T>) = NameMap.exists (fun _ l -> List.exists f l) m
    let find v (m: NameMultiMap<'T>) = match Map.tryFind v m with None -> [] | Some r -> r
    let add v x (m: NameMultiMap<'T>) = NameMap.add v (x :: find v m) m
    let range (m: NameMultiMap<'T>) = Map.foldBack (fun _ x sofar -> x @ sofar) m []
    let rangeReversingEachBucket (m: NameMultiMap<'T>) = Map.foldBack (fun _ x sofar -> List.rev x @ sofar) m []
    
    let chooseRange f (m: NameMultiMap<'T>) = Map.foldBack (fun _ x sofar -> List.choose f x @ sofar) m []
    let map f (m: NameMultiMap<'T>) = NameMap.map (List.map f) m 
    let empty : NameMultiMap<'T> = Map.empty
    let initBy f xs : NameMultiMap<'T> = xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k,List.ofSeq v)) |> Map.ofSeq 
    let ofList (xs: (string * 'T) list) : NameMultiMap<'T> = xs |> Seq.groupBy fst |> Seq.map (fun (k,v) -> (k,List.ofSeq (Seq.map snd v))) |> Map.ofSeq 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MultiMap = 
    let existsInRange f (m: MultiMap<_,_>) = Map.exists (fun _ l -> List.exists f l) m
    let find v (m: MultiMap<_,_>) = match Map.tryFind v m with None -> [] | Some r -> r
    let add v x (m: MultiMap<_,_>) = Map.add v (x :: find v m) m
    let range (m: MultiMap<_,_>) = Map.foldBack (fun _ x sofar -> x @ sofar) m []
    //let chooseRange f (m: MultiMap<_,_>) = Map.foldBack (fun _ x sofar -> List.choose f x @ sofar) m []
    let empty : MultiMap<_,_> = Map.empty
    let initBy f xs : MultiMap<_,_> = xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k,List.ofSeq v)) |> Map.ofSeq 

#if LAYERED_MAPS
/// State of immutable map collection, converted to a dictionary on first lookup.
[<RequireQualifiedAccess>]
type LayeredMapState<'Key,'Value when 'Key : equality and 'Key : comparison> = 
   /// Collapsible(entries, size)
   | Collapsible of list<seq<KeyValuePair<'Key,'Value>>> * int
   /// Collapsed(frontMap, backingDict)
   | Collapsed of (Map<'Key,'Value> * Dictionary<'Key,'Value>)   

/// Immutable map collection, with explicit flattening to a backing dictionary 
///
/// A layered map is still an immutable map containing a "front" 
/// F# Map, but the layered map collapses its entries to a "backing"
/// dictionary at specific "add-and-collapse" points. 
///
/// For maps built from multiple "add-and-collapse" operations,
/// the process of building the collapsed maps is coalesced.
[<Sealed>]
type LayeredMap<'Key,'Value when 'Key : equality and 'Key : comparison>(state:LayeredMapState<'Key,'Value>) =
    let mutable state = state
    static let empty = LayeredMap<'Key,'Value>(LayeredMapState.Collapsible ([],0))

    let entries() = 
        match state with 
        | LayeredMapState.Collapsible (el,n) -> (el,n)
        | LayeredMapState.Collapsed (m,d) ->  [(m :> seq<_>); (d :> seq<_>)], m.Count + d.Count

    let markAsCollapsible() = 
        match state with 
        | LayeredMapState.Collapsible _ -> ()
        | LayeredMapState.Collapsed _ -> state <- LayeredMapState.Collapsible (entries())

    let collapse() = 
        match state with 
        | LayeredMapState.Collapsible (el, n) -> 
            let d = Dictionary<_,_>(n)
            for e in List.rev el do
               for (KeyValue(k,v)) in e do
                  d.[k] <- v
            let p = (Map.empty, d)
            state <- LayeredMapState.Collapsed p
            p
        | LayeredMapState.Collapsed p  -> p

    let dict() = 
        markAsCollapsible()
        let (_,dict) = collapse() 
        dict
    
    static member Empty : LayeredMap<'Key,'Value> = empty

    member x.TryGetValue (key,res:byref<'Value>) = 
        let (m,d) = collapse() 
        match m.TryFind key with 
        | None -> d.TryGetValue (key,&res)
        | Some r -> res <- r; true

    member x.ContainsKey k = 
        let (map,dict) = collapse() 
        map.ContainsKey k || dict.ContainsKey k

    member x.Item 
     with get key = 
        // collapse on first lookup
        let (map,dict) = collapse() 
        match map.TryFind key with 
        | None -> 
            let mutable res = Unchecked.defaultof<_>
            if dict.TryGetValue (key, &res) then res 
            else raise <| KeyNotFoundException("the key was not found in the LayerdNameMap")
        | Some v -> v

    member x.TryFind key =
        let (map,dict) = collapse() 
        match map.TryFind key with 
        | None -> 
            let mutable res = Unchecked.defaultof<_>
            if dict.TryGetValue (key, &res) then Some res else None
        | res -> res
        
    member x.Values = dict().Values

    member x.Elements = dict() |> Seq.readonly

    member x.Add (key, value) = 
        match state with 
        | LayeredMapState.Collapsible (el,n) -> LayeredMap<_,_>(LayeredMapState.Collapsible ((([| KeyValuePair(key,value) |] :> seq<_>) :: el), n + 1))
        | LayeredMapState.Collapsed (map,dict) -> LayeredMap (LayeredMapState.Collapsed (map.Add (key,value), dict))

    member x.AddAndMarkAsCollapsible (kvs: _[])   =
        let el,n = entries()
        LayeredMap<_,_>(LayeredMapState.Collapsible (((kvs :> seq<_>) :: el), n + kvs.Length))
        
    member x.MarkAsCollapsible ()  =
        markAsCollapsible() 
        x
#endif

#if LAYERED_MULTI_MAP
/// State of immutable map collection, converted to a dictionary on first lookup.
[<RequireQualifiedAccess>]
type LayeredMultiMapState<'Key,'Value when 'Key : equality and 'Key : comparison> = 
   /// Collapsible(entries, size)
   | Collapsible of list<seq<KeyValuePair<'Key,'Value list>>> * int
   /// Collapsed(frontMap, backingDict)
   | Collapsed of (MultiMap<'Key,'Value> * Dictionary<'Key,'Value list>)   

/// Immutable map collection, with explicit flattening to a backing dictionary 
[<Sealed>]
type LayeredMultiMap<'Key,'Value when 'Key : equality and 'Key : comparison>(state:LayeredMultiMapState<'Key,'Value>) = 

    let mutable state = state
    static let empty = LayeredMultiMap<'Key,'Value>(LayeredMultiMapState.Collapsible ([],0))

    let entries() = 
        match state with 
        | LayeredMultiMapState.Collapsible (el,n) -> (el,n)
        | LayeredMultiMapState.Collapsed (m,d) ->  [(m :> seq<_>); (d :> seq<_>)], m.Count + d.Count

    let markAsCollapsible() = 
        match state with 
        | LayeredMultiMapState.Collapsible _ -> ()
        | LayeredMultiMapState.Collapsed _ -> state <- LayeredMultiMapState.Collapsible (entries())

    let collapse() = 
        match state with 
        | LayeredMultiMapState.Collapsible (el, n) -> 
            let d = Dictionary<_,_>(n)
            for e in List.rev el do
               for (KeyValue(k,vs)) in e do
                  for v in List.rev vs do 
                      let prev = 
                         let mutable res = Unchecked.defaultof<'Value list>
                         let ok = d.TryGetValue(k,&res)
                         if ok then res else []
                      d.[k] <- v::prev
            let p = (MultiMap.empty, d)
            state <- LayeredMultiMapState.Collapsed p
            p
        | LayeredMultiMapState.Collapsed p  -> p

    let dict() = 
        markAsCollapsible()
        let (_,dict) = collapse() 
        dict
    
    static member Empty : LayeredMultiMap<'Key,'Value> = empty

    member x.TryGetValue (key,res:byref<'Value list>) = 
        let (m,d) = collapse() 
        match m.TryFind key with 
        | None -> d.TryGetValue (key,&res)
        | Some res1 -> 
            let mutable res2 = Unchecked.defaultof<'Value list>
            let ok = d.TryGetValue (key,&res2) 
            if ok then res <- (res1@res2); true
            else res <- res1; true

    member x.ContainsKey k = 
        let (map,dict) = collapse() 
        map.ContainsKey k || dict.ContainsKey k

    member x.Item 
     with get key = 
        let mutable res = Unchecked.defaultof<_>
        if x.TryGetValue (key, &res) then res 
        else [] 

    member x.TryFind key =
        let mutable res = Unchecked.defaultof<_>
        if x.TryGetValue (key, &res) then Some res 
        else None
        
    member x.Values = dict().Values |> Seq.concat

    member x.Add (key, value) = 
        match state with 
        | LayeredMultiMapState.Collapsible (el,n) -> LayeredMultiMap<_,_>(LayeredMultiMapState.Collapsible ((([| KeyValuePair(key,[value]) |] :> seq<_>) :: el), n + 1))
        | LayeredMultiMapState.Collapsed (map,dict) -> LayeredMultiMap (LayeredMultiMapState.Collapsed (MultiMap.add key value map, dict))

    member x.AddAndMarkAsCollapsible (kvs: _[])   =
        let el,n = entries()
        LayeredMultiMap<_,_>(LayeredMultiMapState.Collapsible ((([| for KeyValue(k,v) in kvs -> KeyValuePair(k,[v]) |] :> seq<_>) :: el), n + kvs.Length))
        
    member x.MarkAsCollapsible ()  =
        markAsCollapsible() 
        x

#endif
//#if NEW_LAYERED_MAP

/// Immutable map collection, with explicit flattening to a backing dictionary 
///
/// A layered map is still an immutable map containing a "front" 
/// F# Map, but the layered map collapses its treeMap to a "backing"
/// dictionary at specific "add-and-tryCollapseToDictAndNothingElse" points. 
///
/// For maps built from multiple "add-and-tryCollapseToDictAndNothingElse" operations,
/// the process of building the collapsed maps is coalesced.
type LayeredMap<'Key,'Value when 'Key : equality and 'Key : comparison>
                (// The queue of operations to build the full map, empty except during bulk-add operations
                 xqueue: list<Choice<KeyValuePair<'Key,'Value>[], 
                                     ('Key * ('Value option -> 'Value))>>, 
                 // The existing backing tree map (which is looked up in preference to the dictionary)
                 xentries: Map<'Key,'Value>, 
                 // The existing backing dictionary (which may be null)
                 xdict: Dictionary<'Key,'Value>) =
    static let empty = LayeredMap<'Key,'Value>([], Map.empty, null)
    let mutable state = (xqueue,xentries,xdict)

    let tryCollapseToDictAndNothingElse force = 
        let (bulkQueue,treeMap,fastDict) = state 
        if not bulkQueue.IsEmpty || force then
            // bulkQueue.Length + 
            let d = Dictionary<_,_>(treeMap.Count + (match fastDict with null -> 0 | _ -> fastDict.Count))
            begin
                match fastDict with 
                | null -> ()
                | _ -> 
                    for (KeyValue(k,v)) in fastDict do
                      d.[k] <- v
            end
            treeMap |> Map.iter (fun k v -> d.[k] <- v)
            for kvsOrModify in List.rev bulkQueue do 
              match kvsOrModify with 
              | Choice1Of2 kvs -> 
                  for (KeyValue(k,v)) in kvs do
                    d.[k] <- v
              | Choice2Of2 (k,updatef) -> 
                  let mutable prev = Unchecked.defaultof<_>
                  let n = updatef (if d.TryGetValue(k,&prev) then Some prev else None)
                  d.[k] <- n

            state <- ([], Map.empty, d)
            d
        elif treeMap.IsEmpty then fastDict 
        else null

    static member Empty : LayeredMap<'Key,'Value> = empty

    member x.TryGetValue (key,res:byref<'Value>) = 
        match tryCollapseToDictAndNothingElse false with
        | null ->
            let (_,treeMap,fastDict) = state 
            match treeMap.TryFind key with 
            | None -> 
                match fastDict with 
                | null -> false
                | _ -> fastDict.TryGetValue (key,&res)
            | Some r -> res <- r; true
        | fastDict -> 
            //printfn "collapsed"
            match fastDict with 
            | null -> false
            | _ -> fastDict.TryGetValue (key, &res)

    member x.ContainsKey key = 
        let mutable res = Unchecked.defaultof<_>
        x.TryGetValue(key, &res)

    member x.Item 
      with get key = 
        let mutable res = Unchecked.defaultof<_>
        let ok = x.TryGetValue(key, &res)
        if ok then res 
        else raise <| KeyNotFoundException("the key was not found in the LayerdNameMap")

    member x.TryFind key =
        let mutable res = Unchecked.defaultof<_>
        let ok = x.TryGetValue(key, &res)
        if ok then Some res  else None
        
    member x.Values = (tryCollapseToDictAndNothingElse true).Values

    member x.Elements = (tryCollapseToDictAndNothingElse true) |> Seq.readonly


    member x.Add (key, value) = 
        let (bulkQueue,treeMap,fastDict) = state 
        if bulkQueue.IsEmpty then 
            let treeMap = treeMap.Add (key, value)
            LayeredMap(bulkQueue, treeMap, fastDict)
        else 
            // There are elements in the bulk queue, squash them down (mutating map "x"),
            // then use a one-element treemap
            let newFastDict = tryCollapseToDictAndNothingElse false
            match newFastDict with
            | null -> failwith "unreachable, bulkQueue was non empty, newFastDict should not be null"
            | _ -> LayeredMap([], Map.empty.Add(key,value), newFastDict)

    member x.AddAndMarkAsCollapsible (kvs: _[])   =
        if kvs.Length = 0 then x else
        let (bulkQueue,treeMap,fastDict) = state 
        let state = (Choice1Of2 kvs::bulkQueue,treeMap,fastDict)
        LayeredMap state
        
    /// Push an item that transforms a possible existing entry. This is used for the bulk updates
    /// in nameres.fs, where, for each type we push during an "open", we must combine the
    /// type with any existing entries for types in the eUnqualifiedItems table. 
    member x.LinearTryModifyThenLaterFlatten (key, f: 'Value option -> 'Value) =
        let (bulkQueue,treeMap,fastDict) = state 
        let state = (Choice2Of2 (key,f)::bulkQueue,treeMap,fastDict)
        LayeredMap state
        

    member x.MarkAsCollapsible ()  = //x.AddAndMarkAsCollapsible [| |] 
        let (bulkQueue,treeMap,fastDict) = state 
        let state = (Choice1Of2 [| |]::bulkQueue,treeMap,fastDict)
        LayeredMap state


//#endif

//#if NEW_LAYERED_MULTI_MAP
type LayeredMultiMap<'Key,'Value when 'Key : equality and 'Key : comparison>
                (xqueue: list<KeyValuePair<'Key,'Value>[]>, 
                  xentries: Map<'Key,'Value list>, 
                  xdict: Dictionary<'Key,'Value list>) =
    static let empty = LayeredMultiMap<'Key,'Value>([], Map.empty, null)
    let mutable state = (xqueue,xentries,xdict)

    let tryCollapseToDictAndNothingElse force = 
        let (bulkQueue,treeMap,fastDict) = state 
        if not bulkQueue.IsEmpty || force then
            // bulkQueue.Length + 
            let d = Dictionary<_,_>(treeMap.Count + (match fastDict with null -> 0 | _ -> fastDict.Count))
            begin
                match fastDict with 
                | null -> ()
                | _ -> 
                    for (KeyValue(k,vs)) in fastDict do
                      d.[k] <- vs
            end
            treeMap |> Map.iter (fun k vs -> 
              let mutable prev = Unchecked.defaultof<_>
              if d.TryGetValue(k,&prev) then 
                    d.[k] <- vs@prev
              else
                    d.[k] <- vs)
            //printfn "collapsing, bulkQueue = %A" bulkQueue
            for kvs in List.rev bulkQueue do 
              //printfn "collapsing, bulkQueue.i] = %A" bulkQueue.[i]
              for (KeyValue(k,v)) in kvs do
                  let mutable prev = Unchecked.defaultof<_>
                  if d.TryGetValue(k,&prev) then 
                        d.[k] <- (v::prev)
                  else
                        d.[k] <- [v]
            state <- ([], Map.empty, d)
            d
        elif treeMap.IsEmpty then fastDict 
        else null

    static member Empty : LayeredMultiMap<'Key,'Value> = empty

    member x.TryGetValue (key,res:byref<'Value list>) = 
        match tryCollapseToDictAndNothingElse false with
        | null ->
            let (_,treeMap,fastDict) = state 
            match treeMap.TryFind key with 
            | None -> 
                match fastDict with 
                | null -> false
                | _ -> fastDict.TryGetValue (key,&res)
            | Some r -> 
                match fastDict with 
                | null -> 
                    res <- r
                    true
                | _ -> 
                    let mutable res2 = Unchecked.defaultof<_>
                    if fastDict.TryGetValue (key,&res2) then 
                        res <- r@res2
                    else
                        res <- r
                    true
        | fastDict -> 
            //printfn "collapsed"
            match fastDict with 
            | null -> false
            | _ -> fastDict.TryGetValue (key, &res)

    member x.ContainsKey key = 
        let mutable res = Unchecked.defaultof<_>
        x.TryGetValue(key, &res)

    member x.Item 
      with get key = 
        let mutable res = Unchecked.defaultof<_>
        let ok = x.TryGetValue(key, &res)
        if ok then res else []

    member x.TryFind key =
        let mutable res = Unchecked.defaultof<_>
        let ok = x.TryGetValue(key, &res)
        if ok then Some res  else None

    member x.Values = (tryCollapseToDictAndNothingElse true).Values |> Seq.concat

    member x.Elements = (tryCollapseToDictAndNothingElse true) |> Seq.readonly

    member x.Add (key, value) = 
        let (bulkQueue,treeMap,fastDict) = state 
        if bulkQueue.IsEmpty then 
            let prev = match treeMap.TryFind key with None -> [] | Some vs -> vs
            let treeMap = treeMap.Add (key, value::prev)
            LayeredMultiMap(bulkQueue, treeMap, fastDict)
        else 
            // There are elements in the bulk queue, squash them down (mutating map "x"),
            // then use a one-element treemap
            let newFastDict = tryCollapseToDictAndNothingElse false
            match newFastDict with
            | null -> failwith "unreachable, bulkQueue was non empty, newFastDict should not be null"
            | _ -> LayeredMultiMap([], Map.empty.Add(key,[value]), newFastDict)

    member x.AddAndMarkAsCollapsible (kvs: _[])   =
        if kvs.Length = 0 then x else
        let (bulkQueue,treeMap,fastDict) = state 
        let state = (kvs::bulkQueue,treeMap,fastDict)
        LayeredMultiMap state
        
    member x.MarkAsCollapsible ()  = //x.AddAndMarkAsCollapsible [| |] 
        let (bulkQueue,treeMap,fastDict) = state 
        let state = ([| |]::bulkQueue,treeMap,fastDict)
        LayeredMultiMap state

//#endif

#if NO_LAYERED_MAP
type LayeredMap<'Key,'Value  when 'Key : comparison> = Map<'Key,'Value>

type Map<'Key,'Value when 'Key : comparison> with
    static member Empty : Map<'Key,'Value> = Map.empty

    member m.TryGetValue (key,res:byref<'Value>) = 
        match m.TryFind key with 
        | None -> false
        | Some r -> res <- r; true

    member x.Values = [ for (KeyValue(_,v)) in x -> v ]
    member x.Elements = [ for kvp in x -> kvp ]
    member x.AddAndMarkAsCollapsible (kvs: _[])   = (x,kvs) ||> Array.fold (fun x (KeyValue(k,v)) -> x.Add(k,v))
    member x.LinearTryModifyThenLaterFlatten (key, f: 'Value option -> 'Value) = x.Add (key, f (x.TryFind key))

    member x.MarkAsCollapsible ()  = x

//#endif


//#if NO_LAYERED_MULTI_MAP
/// Immutable map collection, with explicit flattening to a backing dictionary 
[<Sealed>]
type LayeredMultiMap<'Key,'Value when 'Key : equality and 'Key : comparison>(contents : Map<'Key,'Value list>) = 
    static let empty : LayeredMultiMap<'Key,'Value> = LayeredMultiMap Map.empty
    member x.Add (k,v) = LayeredMultiMap(contents.Add(k,v :: x.[k]))
    member x.Item with get k = match contents.TryFind k with None -> [] | Some l -> l
    member x.AddAndMarkAsCollapsible (kvs: _[])  = 
        let x = (x,kvs) ||> Array.fold (fun x (KeyValue(k,v)) -> x.Add(k,v))
        x.MarkAsCollapsible()
    member x.MarkAsCollapsible() = x //LayeredMultiMap(contents)
    member x.TryFind k = contents.TryFind k
    member x.Values = [ for (KeyValue(_,v)) in contents -> v ] |> Seq.concat
    static member Empty : LayeredMultiMap<'Key,'Value> = empty
    
#endif

[<AutoOpen>]
module Shim =

    open System.IO
    [<AbstractClass>]
    type FileSystem() = 
        abstract ReadAllBytesShim: fileName:string -> byte[] 
        default this.ReadAllBytesShim (fileName:string) = 
            use stream = this.FileStreamReadShim fileName
            let len = stream.Length
            let buf = Array.zeroCreate<byte> (int len)
            stream.Read(buf, 0, (int len)) |> ignore                                            
            buf

        abstract FileStreamReadShim: fileName:string -> System.IO.Stream
        abstract FileStreamCreateShim: fileName:string -> System.IO.Stream
        abstract GetFullPathShim: fileName:string -> string
        /// Take in a filename with an absolute path, and return the same filename
        /// but canonicalized with respect to extra path separators (e.g. C:\\\\foo.txt) 
        /// and '..' portions
        abstract SafeGetFullPath: fileName:string -> string
        abstract IsPathRootedShim: path:string -> bool

        abstract IsInvalidFilename: filename:string -> bool
        abstract GetTempPathShim : unit -> string
        abstract GetLastWriteTimeShim: fileName:string -> System.DateTime
        abstract SafeExists: fileName:string -> bool
        abstract FileDelete: fileName:string -> unit
        abstract AssemblyLoadFrom: fileName:string -> System.Reflection.Assembly 
        abstract AssemblyLoad: assemblyName:System.Reflection.AssemblyName -> System.Reflection.Assembly 

#if SILVERLIGHT
        default this.AssemblyLoadFrom(fileName:string) = 
              let load() = 
                  let assemblyPart = System.Windows.AssemblyPart()
                  let assemblyStream = this.FileStreamReadShim(fileName)
                  assemblyPart.Load(assemblyStream)
              if System.Windows.Deployment.Current.Dispatcher.CheckAccess() then 
                  load() 
              else
                  let resultTask = System.Threading.Tasks.TaskCompletionSource<System.Reflection.Assembly>()
                  System.Windows.Deployment.Current.Dispatcher.BeginInvoke(Action(fun () -> resultTask.SetResult (load()))) |> ignore
                  resultTask.Task.Result

        default this.AssemblyLoad(assemblyName:System.Reflection.AssemblyName) = 
            try 
               System.Reflection.Assembly.Load(assemblyName.FullName)
            with e -> 
                this.AssemblyLoadFrom(assemblyName.Name + ".dll")
#else
        default this.AssemblyLoadFrom(fileName:string) = System.Reflection.Assembly.LoadFrom fileName
        default this.AssemblyLoad(assemblyName:System.Reflection.AssemblyName) = System.Reflection.Assembly.Load assemblyName
#endif


#if SILVERLIGHT
    open System.IO.IsolatedStorage
    open System.Windows
    open System

    let mutable FileSystem = 
        { new FileSystem() with 
            member __.FileStreamReadShim (fileName:string) = 
                match Application.GetResourceStream(System.Uri(fileName,System.UriKind.Relative)) with 
                | null -> IsolatedStorageFile.GetUserStoreForApplication().OpenFile(fileName, System.IO.FileMode.Open) :> System.IO.Stream 
                | resStream -> resStream.Stream

            member __.FileStreamCreateShim (fileName:string) = 
                System.IO.IsolatedStorage.IsolatedStorageFile.GetUserStoreForApplication().CreateFile(fileName) :> Stream

            member __.GetFullPathShim (fileName:string) = fileName
            member __.IsPathRootedShim (pathName:string) = true
            member __.SafeGetFullPath (fileName:string) = fileName
            member __.IsInvalidFilename(filename:string) = 
                String.IsNullOrEmpty(filename) || filename.IndexOfAny(System.IO.Path.GetInvalidPathChars()) <> -1

            member __.GetTempPathShim() = "." 

            member __.GetLastWriteTimeShim (fileName:string) = 
                match Application.GetResourceStream(System.Uri(fileName,System.UriKind.Relative)) with 
                | null -> IsolatedStorageFile.GetUserStoreForApplication().GetLastAccessTime(fileName).LocalDateTime
                | _resStream -> System.DateTime.Today.Date
            member __.SafeExists (fileName:string) = 
                match Application.GetResourceStream(System.Uri(fileName,System.UriKind.Relative)) with 
                | null -> IsolatedStorageFile.GetUserStoreForApplication().FileExists fileName
                | resStream -> resStream.Stream <> null
            member __.FileDelete (fileName:string) = 
                match Application.GetResourceStream(System.Uri(fileName,System.UriKind.Relative)) with 
                | null -> IsolatedStorageFile.GetUserStoreForApplication().DeleteFile fileName
                | _resStream -> ()
            
          }
#else

    let mutable FileSystem = 
        { new FileSystem() with 
            override __.ReadAllBytesShim (fileName:string) = File.ReadAllBytes fileName
            member __.FileStreamReadShim (fileName:string) = new FileStream(fileName,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)  :> Stream
            member __.FileStreamCreateShim (fileName:string) = new FileStream(fileName,FileMode.Create,FileAccess.Write,FileShare.Read ,0x1000,false) :> Stream
            member __.GetFullPathShim (fileName:string) = System.IO.Path.GetFullPath fileName
            member __.SafeGetFullPath (fileName:string) = 
                //System.Diagnostics.Debug.Assert(Path.IsPathRooted(fileName), sprintf "SafeGetFullPath: '%s' is not absolute" fileName)
                Path.GetFullPath fileName

            member __.IsPathRootedShim (path:string) = Path.IsPathRooted path

            member __.IsInvalidFilename(filename:string) = 
                String.IsNullOrEmpty(filename) || filename.IndexOfAny(Path.GetInvalidFileNameChars()) <> -1

            member __.GetTempPathShim() = System.IO.Path.GetTempPath()

            member __.GetLastWriteTimeShim (fileName:string) = File.GetLastWriteTime fileName
            member __.SafeExists (fileName:string) = System.IO.File.Exists fileName 
            member __.FileDelete (fileName:string) = System.IO.File.Delete fileName }

#endif        

    type System.Text.Encoding with 
        static member GetEncodingShim(n:int) = 
#if SILVERLIGHT
                System.Text.Encoding.GetEncoding(n.ToString())
#else                
                System.Text.Encoding.GetEncoding(n)
#endif                

