// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
#nowarn "1178" // The struct, record or union type 'internal_instr_extension' is not structurally comparable because the type


open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics

#if FX_RESHAPED_REFLECTION
open Microsoft.FSharp.Core.ReflectionAdapters
#endif

// Logical shift right treating int32 as unsigned integer.
// Code that uses this should probably be adjusted to use unsigned integer types.
let (>>>&) (x:int32) (n:int32) = int32 (uint32 x >>> n)

let notlazy v = Lazy<_>.CreateFromValue v

let inline isNil l = List.isEmpty l
let inline isNonNull x = not (isNull x)
let inline nonNull msg x = if isNull x then failwith ("null: " ^ msg) else x
let (===) x y = LanguagePrimitives.PhysicalEquality x y

//---------------------------------------------------------------------
// Library: ReportTime
//---------------------------------------------------------------------
let reportTime =
    let tFirst = ref None     
    let tPrev = ref None     
    fun showTimes descr ->
        if showTimes then 
            let t = System.Diagnostics.Process.GetCurrentProcess().UserProcessorTime.TotalSeconds
            let prev = match !tPrev with None -> 0.0 | Some t -> t
            let first = match !tFirst with None -> (tFirst := Some t; t) | Some t -> t
            dprintf "ilwrite: TIME %10.3f (total)   %10.3f (delta) - %s\n" (t - first) (t - prev) descr
            tPrev := Some t

//-------------------------------------------------------------------------
// Library: projections
//------------------------------------------------------------------------

[<Struct>]
/// An efficient lazy for inline storage in a class type. Results in fewer thunks.
type InlineDelayInit<'T when 'T : not struct> = 
    new (f: unit -> 'T) = {store = Unchecked.defaultof<'T>; func = System.Func<_>(f) } 
    val mutable store : 'T
    val mutable func : System.Func<'T>
    member x.Value = 
        match x.func with 
        | null -> x.store 
        | _ -> 
        let res = System.Threading.LazyInitializer.EnsureInitialized(&x.store, x.func) 
        x.func <- Unchecked.defaultof<_>
        res

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
      
    /// pass an array byref to reverse it in place
    let revInPlace (array: 'T []) =
        if Array.isEmpty array then () else
        let arrlen, revlen = array.Length-1, array.Length/2 - 1
        for idx in 0 .. revlen do
            let t1 = array.[idx] 
            let t2 = array.[arrlen-idx]
            array.[idx] <- t2
            array.[arrlen-idx] <- t1

    /// Async implementation of Array.map.
    let mapAsync (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
        let len = Array.length array
        let result = Array.zeroCreate len

        async { // Apply the mapping function to each array element.
            for i in 0 .. len - 1 do
                let! mappedValue = mapping array.[i]
                result.[i] <- mappedValue

            // Return the completed results.
            return result
        }
        
    /// Returns a new array with an element replaced with a given value.
    let replace index value (array: _ []) =
        if index >= array.Length then raise (IndexOutOfRangeException "index")
        let res = Array.copy array
        res.[index] <- value
        res

    /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
    /// ~2x faster for floats
    /// ~0.8x slower for ints
    let inline areEqual (xs: 'T []) (ys: 'T []) =
        match xs, ys with
        | null, null -> true
        | [||], [||] -> true
        | null, _ | _, null -> false
        | _ when xs.Length <> ys.Length -> false
        | _ ->
            let mutable break' = false
            let mutable i = 0
            let mutable result = true
            while i < xs.Length && not break' do
                if xs.[i] <> ys.[i] then 
                    break' <- true
                    result <- false
                i <- i + 1
            result

    /// Returns all heads of a given array.
    /// For [|1;2;3|] it returns [|[|1; 2; 3|]; [|1; 2|]; [|1|]|]
    let heads (array: 'T []) =
        let res = Array.zeroCreate<'T[]> array.Length
        for i = array.Length - 1 downto 0 do
            res.[i] <- array.[0..i]
        res

    /// check if subArray is found in the wholeArray starting 
    /// at the provided index
    let inline isSubArray (subArray: 'T []) (wholeArray:'T []) index = 
        if isNull subArray || isNull wholeArray then false
        elif subArray.Length = 0 then true
        elif subArray.Length > wholeArray.Length then false
        elif subArray.Length = wholeArray.Length then areEqual subArray wholeArray else
        let rec loop subidx idx =
            if subidx = subArray.Length then true 
            elif subArray.[subidx] = wholeArray.[idx] then loop (subidx+1) (idx+1) 
            else false
        loop 0 index
        
    /// Returns true if one array has another as its subset from index 0.
    let startsWith (prefix: _ []) (whole: _ []) =
        isSubArray prefix whole 0
        
    /// Returns true if one array has trailing elements equal to another's.
    let endsWith (suffix: _ []) (whole: _ []) =
        isSubArray suffix whole (whole.Length-suffix.Length)
        
module Option = 
    let mapFold f s opt = 
        match opt with 
        | None -> None,s 
        | Some x -> let x',s' = f s x in Some x',s'

    let otherwise opt dflt = 
        match opt with 
        | None -> dflt 
        | Some x -> x

    let fold f z x = 
        match x with 
        | None -> z 
        | Some x -> f z x

    let attempt (f: unit -> 'T) = try Some (f()) with _ -> None        

module List = 

#if FX_RESHAPED_REFLECTION
    open PrimReflectionAdapters
    open Microsoft.FSharp.Core.ReflectionAdapters
#endif

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

    let rec checkq l1 l2 = 
        match l1,l2 with 
        | h1::t1,h2::t2 -> h1 === h2 && checkq t1 t2
        | _ -> true

    let mapq (f: 'T -> 'T) inp =
        assert not (typeof<'T>.IsValueType) 
        match inp with
        | [] -> inp
        | [h1a] -> 
            let h2a = f h1a
            if h1a === h2a then inp else [h2a]
        | [h1a; h1b] -> 
            let h2a = f h1a
            let h2b = f h1b
            if h1a === h2a && h1b === h2b then inp else [h2a; h2b]
        | [h1a; h1b; h1c] -> 
            let h2a = f h1a
            let h2b = f h1b
            let h2c = f h1c
            if h1a === h2a && h1b === h2b && h1c === h2c then inp else [h2a; h2b; h2c]
        | _ -> 
            let res = List.map f inp 
            if checkq inp res then inp else res
        
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
    
    module FrontAndBack = 
        let (|NonEmpty|Empty|) l = match l with [] -> Empty | _ -> NonEmpty(frontAndBack l)

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

    let rec memq x l = 
        match l with 
        | [] -> false 
        | h::t -> LanguagePrimitives.PhysicalEquality x h || memq x t

    // must be tail recursive 
    let mapFold (f:'a -> 'b -> 'c * 'a) (s:'a) (l:'b list) : 'c list * 'a = 
        match l with
        | [] -> [], s
        | [h] -> let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                 let h',s' = f.Invoke(s, h)
                 [h'], s'
        | _ -> 
            List.mapFold f s l

    // Not tail recursive 
    let rec mapFoldBack f l s = 
        match l with 
        | [] -> ([],s)
        | h::t -> 
           let t',s = mapFoldBack f t s
           let h',s = f h s
           (h'::t', s)

    let mapNth n f xs =
        let rec mn i = function
          | []    -> []
          | x::xs -> if i=n then f x::xs else x::mn (i+1) xs
       
        mn 0 xs

    let rec until p l = match l with [] -> [] | h::t -> if p h then [] else h :: until p t 

    let count pred xs = List.fold (fun n x -> if pred x then n+1 else n) 0 xs

    // WARNING: not tail-recursive 
    let mapHeadTail fhead ftail = function
      | []    -> []
      | [x]   -> [fhead x]
      | x::xs -> fhead x :: List.map ftail xs

    let collectFold f s l = 
      let l, s = mapFold f s l
      List.concat l, s

    let collect2 f xs ys = List.concat (List.map2 f xs ys)

    let toArraySquared xss = xss |> List.map List.toArray |> List.toArray
    let iterSquared f xss = xss |> List.iter (List.iter f)
    let collectSquared f xss = xss |> List.collect (List.collect f)
    let mapSquared f xss = xss |> List.map (List.map f)
    let mapFoldSquared f z xss = mapFold (mapFold f) z xss
    let forallSquared f xss = xss |> List.forall (List.forall f)
    let mapiSquared f xss = xss |> List.mapi (fun i xs -> xs |> List.mapi (fun j x -> f i j x))
    let existsSquared f xss = xss |> List.exists (fun xs -> xs |> List.exists (fun x -> f x))
    let mapiFoldSquared f z xss =  mapFoldSquared f z (xss |> mapiSquared (fun i j x -> (i,j,x)))

[<Struct>]
type ValueOption<'T> =
    | VSome of 'T
    | VNone
    member x.IsSome = match x with VSome _ -> true | VNone -> false
    member x.IsNone = match x with VSome _ -> false | VNone -> true
    member x.Value = match x with VSome r -> r | VNone -> failwith "ValueOption.Value: value is None"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ValueOption =
    let inline ofOption x = match x with Some x -> VSome x | None -> VNone
    let inline bind f x = match x with VSome x -> f x | VNone -> VNone

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

    let hasPrefix s t = Option.isSome (tryDropPrefix s t)
    let dropPrefix s t = match (tryDropPrefix s t) with Some(res) -> res | None -> failwith "dropPrefix"

    let dropSuffix s t = match (tryDropSuffix s t) with Some(res) -> res | None -> failwith "dropSuffix"

    open System
    open System.IO

    let inline toCharArray (str: string) = str.ToCharArray()

    let lowerCaseFirstChar (str: string) =
        if String.IsNullOrEmpty str 
         || Char.IsLower(str, 0) then str else 
        let strArr = toCharArray str
        match Array.tryHead strArr with
        | None -> str
        | Some c  -> 
            strArr.[0] <- Char.ToLower c
            String (strArr)

    let extractTrailingIndex (str: string) =
        match str with
        | null -> null, None
        | _ ->
            let charr = str.ToCharArray() 
            Array.revInPlace charr
            let digits = Array.takeWhile Char.IsDigit charr
            Array.revInPlace digits
            String digits
            |> function
               | "" -> str, None
               | index -> str.Substring (0, str.Length - index.Length), Some (int index)

    /// Remove all trailing and leading whitespace from the string
    /// return null if the string is null
    let trim (value: string) = if isNull value then null else value.Trim()
    
    /// Splits a string into substrings based on the strings in the array separators
    let split options (separator: string []) (value: string) = 
        if isNull value  then null else value.Split(separator, options)

    let (|StartsWith|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.StartsWith pattern then
            Some()
        else None

    let (|Contains|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.Contains pattern then
            Some()
        else None

    let getLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while not (isNull !line) do
            yield !line
            line := reader.ReadLine()
        if str.EndsWith("\n") then
            // last trailing space not returned
            // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
            yield String.Empty
        |]
module Dictionary = 

    let inline newWithSize (size: int) = System.Collections.Generic.Dictionary<_,_>(size, HashIdentity.Structural)
        

module Lazy = 
    let force (x: Lazy<'T>) = x.Force()

//----------------------------------------------------------------------------
// Singe threaded execution and mutual exclusion

/// Represents a permission active at this point in execution
type ExecutionToken = interface end

/// Represents a token that indicates execution on the compilation thread, i.e. 
///   - we have full access to the (partially mutable) TAST and TcImports data structures
///   - compiler execution may result in type provider invocations when resolving types and members
///   - we can access various caches in the SourceCodeServices
///
/// Like other execution tokens this should be passed via argument passing and not captured/stored beyond
/// the lifetime of stack-based calls. This is not checked, it is a discipline withinn the compiler code. 
type CompilationThreadToken() = interface ExecutionToken

/// Represnts a place where we are stating that execution on the compilation thread is required.  The
/// reason why will be documented in a comment in the code at the callsite.
let RequireCompilationThread (_ctok: CompilationThreadToken) = ()

/// Represnts a place in the compiler codebase where we are passed a CompilationThreadToken unnecessarily.
/// This reprents code that may potentially not need to be executed on the compilation thread.
let DoesNotRequireCompilerThreadTokenAndCouldPossiblyBeMadeConcurrent  (_ctok: CompilationThreadToken) = ()

/// Represnts a place in the compiler codebase where we assume we are executing on a compilation thread
let AssumeCompilationThreadWithoutEvidence () = Unchecked.defaultof<CompilationThreadToken>

/// Represents a token that indicates execution on a any of several potential user threads calling the F# compiler services.
type AnyCallerThreadToken() = interface ExecutionToken
let AssumeAnyCallerThreadWithoutEvidence () = Unchecked.defaultof<AnyCallerThreadToken>

/// A base type for various types of tokens that must be passed when a lock is taken.
/// Each different static lock should declare a new subtype of this type.
type LockToken = inherit ExecutionToken
let AssumeLockWithoutEvidence<'LockTokenType when 'LockTokenType :> LockToken> () = Unchecked.defaultof<'LockTokenType>

/// Encapsulates a lock associated with a particular token-type representing the acquisition of that lock.
type Lock<'LockTokenType when 'LockTokenType :> LockToken>() = 
    let lockObj = obj()
    member __.AcquireLock f = lock lockObj (fun () -> f (AssumeLockWithoutEvidence<'LockTokenType>()))

//---------------------------------------------------
// Misc

/// Get an initialization hole 
let getHole r = match !r with None -> failwith "getHole" | Some x -> x

module Map = 
    let tryFindMulti k map = match Map.tryFind k map with Some res -> res | None -> []

type ResultOrException<'TResult> =
    | Result of 'TResult
    | Exception of System.Exception
                     
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResultOrException = 

    let success a = Result a

    let raze (b:exn) = Exception b

    // map
    let (|?>) res f = 
        match res with 
        | Result x -> Result(f x )
        | Exception err -> Exception err
  
    let ForceRaise res = 
        match res with 
        | Result x -> x
        | Exception err -> raise err

    let otherwise f x =
        match x with 
        | Result x -> success x
        | Exception _err -> f()

[<RequireQualifiedAccess>] 
type ValueOrCancelled<'TResult> =
    | Value of 'TResult
    | Cancelled of OperationCanceledException

/// Represents a cancellable computation with explicit representation of a cancelled result.
///
/// A cancellable computation is passed may be cancelled via a CancellationToken, which is propagated implicitly.  
/// If cancellation occurs, it is propagated as data rather than by raising an OperationCancelledException.  
type Cancellable<'TResult> = Cancellable of (System.Threading.CancellationToken -> ValueOrCancelled<'TResult>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellable = 

    /// Run a cancellable computation using the given cancellation token
    let run (ct: System.Threading.CancellationToken) (Cancellable oper) = 
        if ct.IsCancellationRequested then 
            ValueOrCancelled.Cancelled (OperationCanceledException()) 
        else
            oper ct 

    /// Bind the result of a cancellable computation
    let bind f comp1 = 
       Cancellable (fun ct -> 
            match run ct comp1 with 
            | ValueOrCancelled.Value v1 -> run ct (f v1) 
            | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1)

    /// Map the result of a cancellable computation
    let map f oper = 
       Cancellable (fun ct -> 
           match run ct oper with 
           | ValueOrCancelled.Value res -> ValueOrCancelled.Value (f res)
           | ValueOrCancelled.Cancelled err -> ValueOrCancelled.Cancelled err)
                    
    /// Return a simple value as the result of a cancellable computation
    let ret x = Cancellable (fun _ -> ValueOrCancelled.Value x)

    /// Fold a cancellable computation along a sequence of inputs
    let fold f acc seq = 
        Cancellable (fun ct -> 
           (ValueOrCancelled.Value acc, seq) 
           ||> Seq.fold (fun acc x -> 
               match acc with 
               | ValueOrCancelled.Value accv -> run ct (f accv x)
               | res -> res))
    
    /// Iterate a cancellable computation over a collection
    let each f seq = 
        Cancellable (fun ct -> 
           (ValueOrCancelled.Value [], seq) 
           ||> Seq.fold (fun acc x -> 
               match acc with 
               | ValueOrCancelled.Value acc -> 
                   match run ct (f x) with 
                   | ValueOrCancelled.Value x2 -> ValueOrCancelled.Value (x2 :: acc)
                   | ValueOrCancelled.Cancelled err1 -> ValueOrCancelled.Cancelled err1
               | canc -> canc)
           |> function 
               | ValueOrCancelled.Value acc -> ValueOrCancelled.Value (List.rev acc)
               | canc -> canc)
    
    /// Delay a cancellable computation
    let delay (f: unit -> Cancellable<'T>) = Cancellable (fun ct -> let (Cancellable g) = f() in g ct)

    /// Run the computation in a mode where it may not be cancelled. The computation never results in a 
    /// ValueOrCancelled.Cancelled.
    let runWithoutCancellation comp = 
        let res = run System.Threading.CancellationToken.None comp 
        match res with 
        | ValueOrCancelled.Cancelled _ -> failwith "unexpected cancellation" 
        | ValueOrCancelled.Value r -> r

    /// Bind the cancellation token associated with the computation
    let token () = Cancellable (fun ct -> ValueOrCancelled.Value ct)

    /// Represents a canceled computation
    let canceled() = Cancellable (fun _ -> ValueOrCancelled.Cancelled (new OperationCanceledException()))

    /// Catch exceptions in a computation
    let private catch (Cancellable e) = 
        Cancellable (fun ct -> 
            try 
                match e ct with 
                | ValueOrCancelled.Value r -> ValueOrCancelled.Value (Choice1Of2 r) 
                | ValueOrCancelled.Cancelled e -> ValueOrCancelled.Cancelled e 
            with err -> 
                ValueOrCancelled.Value (Choice2Of2 err))

    /// Implement try/finally for a cancellable computation
    let tryFinally e compensation =    
        catch e |> bind (fun res ->  
            compensation();
            match res with Choice1Of2 r -> ret r | Choice2Of2 err -> raise err)

    /// Implement try/with for a cancellable computation
    let tryWith e handler =    
        catch e |> bind (fun res ->  
            match res with Choice1Of2 r -> ret r | Choice2Of2 err -> handler err)
    
    // /// Run the cancellable computation within an Async computation.  This isn't actaully used in the codebase, but left
    // here in case we need it in the future 
    //
    // let toAsync e =    
    //     async { 
    //       let! ct = Async.CancellationToken
    //       return! 
    //          Async.FromContinuations(fun (cont, econt, ccont) -> 
    //            // Run the computation synchronously using the given cancellation token
    //            let res = try Choice1Of2 (run ct e) with err -> Choice2Of2 err
    //            match res with 
    //            | Choice1Of2 (ValueOrCancelled.Value v) -> cont v
    //            | Choice1Of2 (ValueOrCancelled.Cancelled err) -> ccont err
    //            | Choice2Of2 err -> econt err) 
    //     }
    
type CancellableBuilder() = 
    member x.Bind(e,k) = Cancellable.bind k e
    member x.Return(v) = Cancellable.ret v
    member x.ReturnFrom(v) = v
    member x.Combine(e1,e2) = e1 |> Cancellable.bind (fun () -> e2)
    member x.TryWith(e,handler) = Cancellable.tryWith e handler
    member x.Using(resource,e) = Cancellable.tryFinally (e resource) (fun () -> (resource :> System.IDisposable).Dispose())
    member x.TryFinally(e,compensation) =  Cancellable.tryFinally e compensation
    member x.Delay(f) = Cancellable.delay f
    member x.Zero() = Cancellable.ret ()

let cancellable = CancellableBuilder()

/// Computations that can cooperatively yield by returning a continuation
///
///    - Any yield of a NotYetDone should typically be "abandonable" without adverse consequences. No resource release
///      will be called when the computation is abandoned.
///
///    - Computations suspend via a NotYetDone may use local state (mutables), where these are
///      captured by the NotYetDone closure. Computations do not need to be restartable.
///
///    - The key thing is that you can take an Eventually value and run it with 
///      Eventually.repeatedlyProgressUntilDoneOrTimeShareOverOrCanceled
///
///    - Cancellation results in a suspended computation rather than complete abandonment
type Eventually<'T> = 
    | Done of 'T 
    | NotYetDone of (CompilationThreadToken -> Eventually<'T>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Eventually = 
    open System.Threading

    let rec box e = 
        match e with 
        | Done x -> Done (Operators.box x) 
        | NotYetDone (work) -> NotYetDone (fun ctok -> box (work ctok))

    let rec forceWhile ctok check e  = 
        match e with 
        | Done x -> Some(x)
        | NotYetDone (work) -> 
            if not(check()) 
            then None
            else forceWhile ctok check (work ctok) 

    let force ctok e = Option.get (forceWhile ctok (fun () -> true) e)

        
    /// Keep running the computation bit by bit until a time limit is reached.
    /// The runner gets called each time the computation is restarted
    ///
    /// If cancellation happens, the operation is left half-complete, ready to resume.
    let repeatedlyProgressUntilDoneOrTimeShareOverOrCanceled timeShareInMilliseconds (ct: CancellationToken) runner e = 
        let sw = new System.Diagnostics.Stopwatch() 
        let rec runTimeShare ctok e = 
          runner ctok (fun ctok -> 
            sw.Reset()
            sw.Start(); 
            let rec loop ctok ev2 = 
                match ev2 with 
                | Done _ -> ev2
                | NotYetDone work ->
                    if ct.IsCancellationRequested || sw.ElapsedMilliseconds > timeShareInMilliseconds then 
                        sw.Stop();
                        NotYetDone(fun ctok -> runTimeShare ctok ev2) 
                    else 
                        loop ctok (work ctok)
            loop ctok e)
        NotYetDone (fun ctok -> runTimeShare ctok e)
    
    /// Keep running the asynchronous computation bit by bit. The runner gets called each time the computation is restarted.
    /// Can be cancelled as an Async in the normal way.
    let forceAsync (runner: (CompilationThreadToken -> Eventually<'T>) -> Async<Eventually<'T>>) (e: Eventually<'T>) : Async<'T option> =
        let rec loop (e: Eventually<'T>) =
            async {
                match e with 
                | Done x -> return Some x
                | NotYetDone work ->
                    let! r = runner work
                    return! loop r
            }
        loop e

    let rec bind k e = 
        match e with 
        | Done x -> k x 
        | NotYetDone work -> NotYetDone (fun ctok -> bind k (work ctok))

    let fold f acc seq = 
        (Done acc,seq) ||> Seq.fold  (fun acc x -> acc |> bind (fun acc -> f acc x))
        
    let rec catch e = 
        match e with 
        | Done x -> Done(Result x)
        | NotYetDone work -> 
            NotYetDone (fun ctok -> 
                let res = try Result(work ctok) with | e -> Exception e 
                match res with 
                | Result cont -> catch cont
                | Exception e -> Done(Exception e))
    
    let delay (f: unit -> Eventually<'T>) = NotYetDone (fun _ctok -> f())

    let tryFinally e compensation =    
        catch (e) 
        |> bind (fun res ->  compensation();
                             match res with 
                             | Result v -> Eventually.Done v
                             | Exception e -> raise e)

    let tryWith e handler =    
        catch e 
        |> bind (function Result v -> Done v | Exception e -> handler e)
    
    // All eventually computations carry a CompiationThreadToken
    let token =    
        NotYetDone (fun ctok -> Done ctok)
    
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
    member this.Table = encodeTab.Keys

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
        { value = x
          funcOrException = null
          findOriginalException = id }
    member x.IsDelayed = (match x.funcOrException with null -> false | :? LazyWithContextFailure -> false | _ -> true)
    member x.IsForced = (match x.funcOrException with null -> true | _ -> false)
    member x.Force(ctxt:'ctxt) =  
        match x.funcOrException with 
        | null -> x.value 
        | _ -> 
            // Enter the lock in case another thread is in the process of evaluating the result
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
    let ofSeq l : NameMap<'T> = Map.ofSeq l
    let toList (l: NameMap<'T>) = Map.toList l
    let layer (m1 : NameMap<'T>) m2 = Map.foldBack Map.add m1 m2

    /// Not a very useful function - only called in one place - should be changed 
    let layerAdditive addf m1 m2 = 
      Map.foldBack (fun x y sofar -> Map.add x (addf (Map.tryFindMulti x sofar) y) sofar) m1 m2

    /// Union entries by identical key, using the provided function to union sets of values
    let union unionf (ms: NameMap<_> seq) = 
        seq { for m in ms do yield! m } 
           |> Seq.groupBy (fun (KeyValue(k,_v)) -> k) 
           |> Seq.map (fun (k,es) -> (k,unionf (Seq.map (fun (KeyValue(_k,v)) -> v) es))) 
           |> Map.ofSeq

    /// For every entry in m2 find an entry in m1 and fold 
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

type LayeredMap<'Key,'Value  when 'Key : comparison> = Map<'Key,'Value>

type Map<'Key,'Value when 'Key : comparison> with
    static member Empty : Map<'Key,'Value> = Map.empty

    member m.TryGetValue (key,res:byref<'Value>) = 
        match m.TryFind key with 
        | None -> false
        | Some r -> res <- r; true

    member x.Values = [ for (KeyValue(_,v)) in x -> v ]
    member x.AddAndMarkAsCollapsible (kvs: _[])   = (x,kvs) ||> Array.fold (fun x (KeyValue(k,v)) -> x.Add(k,v))
    member x.LinearTryModifyThenLaterFlatten (key, f: 'Value option -> 'Value) = x.Add (key, f (x.TryFind key))
    member x.MarkAsCollapsible ()  = x

/// Immutable map collection, with explicit flattening to a backing dictionary 
[<Sealed>]
type LayeredMultiMap<'Key,'Value when 'Key : equality and 'Key : comparison>(contents : LayeredMap<'Key,'Value list>) = 
    member x.Add (k,v) = LayeredMultiMap(contents.Add(k,v :: x.[k]))
    member x.Item with get k = match contents.TryFind k with None -> [] | Some l -> l
    member x.AddAndMarkAsCollapsible (kvs: _[])  = 
        let x = (x,kvs) ||> Array.fold (fun x (KeyValue(k,v)) -> x.Add(k,v))
        x.MarkAsCollapsible()
    member x.MarkAsCollapsible() = LayeredMultiMap(contents.MarkAsCollapsible())
    member x.TryFind k = contents.TryFind k
    member x.Values = contents.Values |> List.concat
    static member Empty : LayeredMultiMap<'Key,'Value> = LayeredMultiMap LayeredMap.Empty

[<AutoOpen>]
module Shim =

    open System.IO

#if FX_RESHAPED_REFLECTION
    open PrimReflectionAdapters
    open Microsoft.FSharp.Core.ReflectionAdapters
#endif

    type IFileSystem = 
        abstract ReadAllBytesShim: fileName:string -> byte[] 
        abstract FileStreamReadShim: fileName:string -> System.IO.Stream
        abstract FileStreamCreateShim: fileName:string -> System.IO.Stream
        abstract FileStreamWriteExistingShim: fileName:string -> System.IO.Stream
        /// Take in a filename with an absolute path, and return the same filename
        /// but canonicalized with respect to extra path separators (e.g. C:\\\\foo.txt) 
        /// and '..' portions
        abstract GetFullPathShim: fileName:string -> string
        abstract IsPathRootedShim: path:string -> bool
        abstract IsInvalidPathShim: filename:string -> bool
        abstract GetTempPathShim : unit -> string
        abstract GetLastWriteTimeShim: fileName:string -> System.DateTime
        abstract SafeExists: fileName:string -> bool
        abstract FileDelete: fileName:string -> unit
        abstract AssemblyLoadFrom: fileName:string -> System.Reflection.Assembly 
        abstract AssemblyLoad: assemblyName:System.Reflection.AssemblyName -> System.Reflection.Assembly 


    type DefaultFileSystem() =
        interface IFileSystem with
            member __.AssemblyLoadFrom(fileName:string) = 
                Assembly.LoadFrom fileName
            member __.AssemblyLoad(assemblyName:System.Reflection.AssemblyName) = 
                Assembly.Load assemblyName

            member __.ReadAllBytesShim (fileName:string) = File.ReadAllBytes fileName
            member __.FileStreamReadShim (fileName:string) = new FileStream(fileName,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)  :> Stream
            member __.FileStreamCreateShim (fileName:string) = new FileStream(fileName,FileMode.Create,FileAccess.Write,FileShare.Read ,0x1000,false) :> Stream
            member __.FileStreamWriteExistingShim (fileName:string) = new FileStream(fileName,FileMode.Open,FileAccess.Write,FileShare.Read ,0x1000,false) :> Stream
            member __.GetFullPathShim (fileName:string) = System.IO.Path.GetFullPath fileName

            member __.IsPathRootedShim (path:string) = Path.IsPathRooted path

            member __.IsInvalidPathShim(path:string) = 
                let isInvalidPath(p:string) = 
                    String.IsNullOrEmpty(p) || p.IndexOfAny(System.IO.Path.GetInvalidPathChars()) <> -1

                let isInvalidFilename(p:string) = 
                    String.IsNullOrEmpty(p) || p.IndexOfAny(System.IO.Path.GetInvalidFileNameChars()) <> -1

                let isInvalidDirectory(d:string) = 
                    d=null || d.IndexOfAny(Path.GetInvalidPathChars()) <> -1

                isInvalidPath (path) || 
                let directory = Path.GetDirectoryName(path)
                let filename = Path.GetFileName(path)
                isInvalidDirectory(directory) || isInvalidFilename(filename)

            member __.GetTempPathShim() = System.IO.Path.GetTempPath()

            member __.GetLastWriteTimeShim (fileName:string) = File.GetLastWriteTime fileName
            member __.SafeExists (fileName:string) = System.IO.File.Exists fileName 
            member __.FileDelete (fileName:string) = System.IO.File.Delete fileName

    type System.Text.Encoding with 
        static member GetEncodingShim(n:int) = 
                System.Text.Encoding.GetEncoding(n)

    let mutable FileSystem = DefaultFileSystem() :> IFileSystem 
