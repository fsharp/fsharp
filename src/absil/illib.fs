//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2010 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module internal Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
#nowarn "1178" // The struct, record or union type 'internal_instr_extension' is not structurally comparable because the type


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
let maxOn f x1 x2 = if f x1 > f x2 then x1 else x2


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

    let mapFold f s l = 
        let mutable acc = s
        let n = Array.length l
        let mutable res = Array.zeroCreate n
        for i = 0 to n - 1 do
            let h',s' = f acc l.[i]
            res.[i] <- h';
            acc <- s'
        res, acc


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

    let fmap f z l = 
        match l with 
        | None   -> z,None
        | Some x -> let z,x = f z x
                    z,Some x

    let fold f z x = 
        match x with 
        | None -> z 
        | Some x -> f z x


let (+??) opt dflt = match opt with None -> dflt() | Some _ -> opt
let (+?) opt dflt = match opt with None -> dflt() | Some x -> x

let the = function None -> failwith "the"  | Some x -> x

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

    /// warning: not tail recursive 
    let rec takeUntil p l = 
      match l with
      | []    -> [],[]
      | x::xs -> if p x then [],l else let a,b = takeUntil p xs in x::a,b

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
        
    let repeatedlyProgressUntilDoneOrTimeShareOver timeShareInMilliseconds runner e = 
        let sw = new System.Diagnostics.Stopwatch() 
        let rec runTimeShare e = 
          runner (fun () -> 
            sw.Reset()
            sw.Start(); 
            let rec loop(e) = 
                match e with 
                | Done _ -> e
                | NotYetDone (work) -> 
                    if sw.ElapsedMilliseconds > timeShareInMilliseconds then 
                        sw.Stop();
                        NotYetDone(fun () -> runTimeShare e) 
                    else 
                        loop(work())
            loop(e))
        runTimeShare e

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
    //member x.Using(resource:System.IDisposable,e) = Eventually.tryFinally (e resource)  resource.Dispose
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
