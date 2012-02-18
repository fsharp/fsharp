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
    #nowarn "52" // The value has been copied to ensure the original is not mutated by this operation

    open System
    open System.Diagnostics
    open System.Collections
    open System.Collections.Generic
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics

    module IEnumerator = 


      let noReset() = raise (new System.NotSupportedException(SR.GetString(SR.resetNotSupported)))
      let notStarted() = raise (new System.InvalidOperationException(SR.GetString(SR.enumerationNotStarted)))
      let alreadyFinished() = raise (new System.InvalidOperationException(SR.GetString(SR.enumerationAlreadyFinished)))
      let check started = if not started then notStarted()
      let dispose (r : System.IDisposable) = r.Dispose()

      let cast (e : IEnumerator) : IEnumerator<'T> = 
          { new IEnumerator<'T> with 
                member x.Current = unbox e.Current
            interface IEnumerator with 
                member x.Current = unbox  e.Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = noReset();
            interface System.IDisposable with 
                member x.Dispose() = 
                    match e with 
                    | :? System.IDisposable as e -> e.Dispose()
                    | _ -> ()   }
      
      /// A concrete implementation of an enumerator that returns no values
      [<Sealed>]
      type EmptyEnumerator<'T>() = 
          let mutable started = false 
          interface IEnumerator<'T> with 
                member x.Current = 
                  check started; 
                  (alreadyFinished() : 'T)
                  
          interface System.Collections.IEnumerator with 
              member x.Current = 
                  check started; 
                  (alreadyFinished() : obj)
              member x.MoveNext() = 
                  if not started then started <- true;
                  false
              member x.Reset() = noReset()
          interface System.IDisposable with 
                member x.Dispose() = () 
        
      let Empty<'T> () = (new EmptyEnumerator<'T>() :> IEnumerator<'T>)

      let rec nth index (e : IEnumerator<'T>) = 
          if not (e.MoveNext()) then  invalidArg "index" (SR.GetString(SR.notEnoughElements));
          if index < 0 then invalidArg "index" (SR.GetString(SR.inputMustBeNonNegative));
          if index = 0 then e.Current
          else nth (index-1) e

      [<NoEquality; NoComparison>]
      type MapEnumeratorState = 
          | NotStarted 
          | InProcess 
          | Finished

      [<AbstractClass>]
      type MapEnumerator<'T> () =
          let mutable state = NotStarted
          [<DefaultValue(false)>]
          val mutable private curr : 'T
          
          member this.GetCurrent () =
              match state with
              |   NotStarted -> notStarted()
              |   Finished -> alreadyFinished()
              |   InProcess -> ()
              this.curr
          
          abstract DoMoveNext : byref<'T> -> bool
          abstract Dispose : unit -> unit
          
          interface IEnumerator<'T> with
              member this.Current = this.GetCurrent()
          
          interface IEnumerator with
              member this.Current = box(this.GetCurrent())
              member this.MoveNext () =
                  state <- InProcess
                  if this.DoMoveNext(&this.curr) then
                      true
                  else
                      state <- Finished
                      false
              member this.Reset() = noReset()
          interface System.IDisposable with
              member this.Dispose() = this.Dispose()
       
      let map f (e : IEnumerator<_>) : IEnumerator<_>=
          upcast 
              { new MapEnumerator<_>() with
                    member this.DoMoveNext (curr : byref<_>) =
                        if e.MoveNext() then
                            curr <- (f e.Current)
                            true
                        else
                            false
                    member this.Dispose() = e.Dispose()
              }
          
      let mapi f (e : IEnumerator<_>) : IEnumerator<_> =
          let i = ref (-1) 
          upcast
              {  new MapEnumerator<_>() with
                     member this.DoMoveNext curr =
                        i := !i + 1
                        if e.MoveNext() then
                           curr <- f !i e.Current
                           true
                        else
                           false
                     member this.Dispose() = e.Dispose()
              }
              
      let map2 f (e1 : IEnumerator<_>) (e2 : IEnumerator<_>) : IEnumerator<_>=
          upcast 
              {  new MapEnumerator<_>() with
                     member this.DoMoveNext curr = 
                        let n1 = e1.MoveNext()
                        let n2 = e2.MoveNext()
                        if n1 && n2 then
                           curr <- f e1.Current e2.Current
                           true
                        else 
                           false
                     member this.Dispose() = 
                        try 
                            e1.Dispose() 
                        finally
                            e2.Dispose()

              }


      let choose f (e : IEnumerator<'T>) = 
          let started = ref false 
          let curr = ref None 
          let get() =  check !started; (match !curr with None -> alreadyFinished() | Some x -> x) 
          { new IEnumerator<'U> with 
                member x.Current = get()
            interface IEnumerator with 
                member x.Current = box (get())
                member x.MoveNext() = 
                    if not !started then started := true;
                    curr := None; 
                    while ((!curr).IsNone && e.MoveNext()) do 
                        curr := f e.Current;
                    Option.isSome !curr
                member x.Reset() = noReset()
            interface System.IDisposable with 
                member x.Dispose() = e.Dispose()  }

      let filter f (e : IEnumerator<'T>) = 
          let started = ref false 
          let this = 
              { new IEnumerator<'T> with 
                    member x.Current = check !started; e.Current
                interface IEnumerator with 
                    member x.Current = check !started; box e.Current
                    member x.MoveNext() = 
                        let rec next() =
                            if not !started then started := true;
                            e.MoveNext() && (f  e.Current || next()) 
                        next()
                    member x.Reset() = noReset()
                interface System.IDisposable with 
                    member x.Dispose() = e.Dispose()  } 
          this
          
      let unfold f x : IEnumerator<_> =
          let state = ref x
          upcast 
              {  new MapEnumerator<_>() with
                    member this.DoMoveNext curr =
                        match f !state with
                        |   None -> false
                        |   Some(r,s) ->
                                curr <- r
                                state := s
                                true
                    member this.Dispose() = ()
              }

      let upto lastOption f =
          match lastOption with
          | Some b when b<0 -> Empty()    // a request for -ve length returns empty sequence
          | _ ->          
              let unstarted   = -1  // index value means unstarted (and no valid index)
              let completed   = -2  // index value means completed (and no valid index)
              let unreachable = -3  // index is unreachable from 0,1,2,3,...
              let finalIndex  = match lastOption with
                                | Some b -> b             // here b>=0, a valid end value.
                                | None   -> unreachable   // run "forever", well as far as Int32.MaxValue since indexing with a bounded type.                    
              // The Current value for a valid index is "f i".
              // Lazy<_> values are used as caches, to store either the result or an exception if thrown.
              // These "Lazy<_>" caches are created only on the first call to current and forced immediately.
              // The lazy creation of the cache nodes means enumerations that skip many Current values are not delayed by GC.
              // For example, the full enumeration of Seq.initInfinite in the tests.
              // state
              let index   = ref unstarted
              // a Lazy node to cache the result/exception
              let current = ref (Unchecked.defaultof<_>)
              let setIndex i = index := i; current := (Unchecked.defaultof<_>) // cache node unprimed, initialised on demand.
              let getCurrent() = 
                  if !index = unstarted then notStarted()
                  if !index = completed then alreadyFinished()
                  match box !current with 
                  | null -> current := Lazy.Create(fun () -> f !index); 
                  | _ ->  ()
                  // forced or re-forced immediately.          
                  (!current).Force() 
              { new IEnumerator<'U> with 
                    member x.Current = getCurrent()
                interface IEnumerator with 
                    member x.Current = box (getCurrent())
                    member x.MoveNext() = 
                        if !index = completed then
                            false
                        elif !index = unstarted then
                            setIndex 0
                            true
                        else (
                            if !index = System.Int32.MaxValue then raise <| System.InvalidOperationException (SR.GetString(SR.enumerationPastIntMaxValue))
                            if !index = finalIndex then
                                false
                            else
                                setIndex (!index + 1)
                                true
                        )
                    member self.Reset() = noReset()
                interface System.IDisposable with 
                    member x.Dispose() = () } 
                
      let readAndClear r = 
          lock r (fun () -> match !r with None -> None | Some _ as res -> r := None; res)
      
      let generateWhileSome openf compute closef : IEnumerator<'U> = 
          let started = ref false 
          let curr = ref None
          let state = ref (Some(openf())) 
          let getCurr() = 
              check !started;
              match !curr with None -> alreadyFinished() | Some x -> x 
          let start() = if not !started then (started := true) 

          let dispose() = readAndClear state |> Option.iter closef
          let finish() = (try dispose() finally curr := None)
          {  new IEnumerator<'U> with 
                 member x.Current = getCurr()
             interface IEnumerator with 
                 member x.Current = box (getCurr())
                 member x.MoveNext() = 
                     start();
                     match !state with 
                     | None -> false (* we started, then reached the end, then got another MoveNext *)
                     | Some s -> 
                         match (try compute s with e -> finish(); reraise()) with 
                         | None -> finish(); false
                         | Some _ as x -> curr := x; true

                 member x.Reset() = noReset()
             interface System.IDisposable with 
                 member x.Dispose() = dispose() } 

      [<Sealed>]
      type ArrayEnumerator<'T>(arr: 'T array) = 
          let mutable curr = -1
          let mutable len = arr.Length
          member x.Get() =
               if curr >= 0 then 
                 if curr >= len then alreadyFinished()
                 else arr.[curr]
               else 
                 notStarted()
          interface IEnumerator<'T> with 
                member x.Current = x.Get()
          interface System.Collections.IEnumerator with 
                member x.MoveNext() = 
                       if curr >= len then false
                       else 
                         curr <- curr + 1;
                         (curr < len)
                member x.Current = box(x.Get())
                member x.Reset() = noReset()
          interface System.IDisposable with 
                member x.Dispose() = () 

      let ofArray arr = (new ArrayEnumerator<'T>(arr) :> IEnumerator<'T>)

      [<Sealed>]    
      type Singleton<'T>(v:'T) = 
          let mutable started = false
          interface IEnumerator<'T> with
                member x.Current = v
          interface IEnumerator with
              member x.Current = box v
              member x.MoveNext() = if started then false else (started <- true; true)
              member x.Reset() = noReset()
          interface System.IDisposable with
              member x.Dispose() = () 

      let Singleton x = (new Singleton<'T>(x) :> IEnumerator<'T>)

      let EnumerateThenFinally f (e : IEnumerator<'T>) = 
          { new IEnumerator<'T> with
                member x.Current = e.Current
            interface IEnumerator with
                member x.Current = (e :> IEnumerator).Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = noReset()
            interface IDisposable with
                member x.Dispose() = 
                    try
                        e.Dispose()  
                    finally
                        f()
          }

    // Use generators for some implementations of IEnumerables.
    //
    module Generator = 

        open System.Collections
        open System.Collections.Generic
        
        [<NoEquality; NoComparison>]
        type Step<'T> = 
            | Stop
            | Yield of 'T
            | Goto of Generator<'T>

        and Generator<'T> = 
            abstract Apply: (unit -> Step<'T>)
            abstract Disposer: (unit -> unit) option

        let disposeG (g:Generator<'T>) = 
            match g.Disposer with 
            | None -> () 
            | Some f -> f()
        
        let chainDisposeG d1 (g:Generator<'T>) =
            let app = g.Apply
            let disp = match g.Disposer with Some f2 -> Some(fun () -> f2(); d1()) | None -> Some d1
            { new Generator<_> with 
                  member x.Apply = app 
                  member x.Disposer = disp }

        let appG (g:Generator<_>) =
            //System.Console.WriteLine("{0}.appG", box g)
            let res = g.Apply()
            match res with 
            | Goto(next) -> 
                Goto(next)
            | Yield _ ->  
                res
            | Stop -> 
                //System.Console.WriteLine("appG: Stop")
                disposeG g; 
                res
        
        // Binding. 
        //
        // We use a type defintion to apply a local dynamic optimization. 
        // We automatically right-associate binding, i.e. push the continuations to the right.
        // That is, bindG (bindG G1 cont1) cont2 --> bindG G1 (cont1 o cont2)
        // This makes constructs such as the following linear rather than quadratic:
        //
        //  let rec rwalk n = { if n > 0 then 
        //                         yield! rwalk (n-1)
        //                         yield n }

        type GenerateThen<'T>(g:Generator<'T>, cont : unit -> Generator<'T>) =
            member self.Generator = g
            member self.Cont = cont
            interface Generator<'T> with 
                 member x.Apply = (fun () -> 
                      match appG g with 
                      | Stop -> 
                          // OK, move onto the generator given by the continuation
                          Goto(cont())

                      | Yield _ as res -> 
                          res
                      
                      | Goto next -> 
                          Goto(GenerateThen<_>.Bind(next,cont)))
                 member x.Disposer = 
                      g.Disposer


            static member Bind (g:Generator<'T>, cont) = 
                match g with
                | :? GenerateThen<'T> as g -> GenerateThen<_>.Bind(g.Generator,(fun () -> GenerateThen<_>.Bind (g.Cont(), cont)))
                | g -> (new GenerateThen<'T>(g, cont) :> Generator<'T>)


        let bindG g cont = GenerateThen<_>.Bind(g,cont)

        //let emptyG () = 
        //    { new Generator<_> with 
        //           member x.Apply = (fun () -> Stop)
        //           member x.Disposer = None }
        //
        //let delayG f  = 
        //    { new Generator<_> with 
        //           member x.Apply = fun () -> Goto(f())
        //           member x.Disposer = None }
        //
        //let useG (v: System.IDisposable) f = 
        //    { new Generator<_> with 
        //           member x.Apply = (fun () -> 
        //               let g = f v in 
        //               // We're leaving this generator but want to maintain the disposal on the target.
        //               // Hence chain it into the disposer of the target
        //               Goto(chainDisposeG v.Dispose g))
        //           member x.Disposer = Some (fun () -> v.Dispose()) }
        //
        //let yieldG (v:'T) = 
        //    let yielded = ref false
        //    { new Generator<_> with 
        //           member x.Apply = fun () -> if !yielded then Stop else (yielded := true; Yield(v))
        //           member x.Disposer = None }
        //
        //let rec whileG gd b = if gd() then bindG (b()) (fun () -> whileG gd b) else emptyG()
        //
        //let yieldThenG x b = bindG (yieldG x) b
        //
        //let forG (v: seq<'T>) f = 
        //    let e = v.GetEnumerator() in 
        //    whileG e.MoveNext (fun () -> f e.Current)

        // Internal type. Drive an underlying generator. Crucially when the generator returns
        // a new generator we simply update our current generator and continue. Thus the enumerator
        // effectively acts as a reference cell holding the current generator. This means that
        // infinite or large generation chains (e.g. caused by long sequences of append's, including 
        // possible delay loops) can be referenced via a single enumerator.
        //
        // A classic case where this arises in this sort of sequence expression:
        //    let rec data s = { yield s; 
        //                       yield! data (s + random()) }
        //
        // This translates to 
        //    let rec data s = Seq.delay (fun () -> Seq.append (Seq.singleton s) (Seq.delay (fun () -> data (s+random()))))
        //
        // When you unwind through all the Seq, IEnumerator and Generator objects created, 
        // you get (data s).GetEnumerator being an "GenerateFromEnumerator(EnumeratorWrappingLazyGenerator(...))" for the append.
        // After one element is yielded, we move on to the generator for the inner delay, which in turn
        // comes back to be a "GenerateFromEnumerator(EnumeratorWrappingLazyGenerator(...))".
        //
        // Defined as a type so we can optimize Enumerator/Generator chains in enumerateFromLazyGenerator
        // and GenerateFromEnumerator.


        [<Sealed>]
        type EnumeratorWrappingLazyGenerator<'T>(g:Generator<'T>) =
            let mutable g = g
            let mutable curr = None
            let mutable finished = false
            member e.Generator = g
            interface IEnumerator<'T> with
                member x.Current= match curr with Some(v) -> v | None -> raise <| System.InvalidOperationException (SR.GetString(SR.moveNextNotCalledOrFinished))
            interface System.Collections.IEnumerator with
                member x.Current = box (x :> IEnumerator<_>).Current
                member x.MoveNext() = 
                    not finished && 
                    (match appG g with 
                     | Stop -> 
                        curr <- None; 
                        finished <- true; 
                        false
                     | Yield(v) ->
                        curr <- Some(v); 
                        true
                     | Goto(next) ->
                        (g <- next);
                        (x :> IEnumerator).MoveNext())
                member x.Reset() = IEnumerator.noReset()
            interface System.IDisposable with
                member x.Dispose() =
                    if not finished then disposeG g

        // Internal type, used to optimize Enumerator/Generator chains
        type LazyGeneratorWrappingEnumerator<'T>(e:System.Collections.Generic.IEnumerator<'T>) =
            member g.Enumerator = e
            interface Generator<'T> with
                member g.Apply = (fun () -> 
                    if e.MoveNext() then 
                        Yield(e.Current) 
                    else 
                        Stop)
                member g.Disposer= Some(e.Dispose)

        let EnumerateFromGenerator(g:Generator<'T>) = 
            match g with 
            | :? LazyGeneratorWrappingEnumerator<'T> as g -> g.Enumerator
            | _ -> (new EnumeratorWrappingLazyGenerator<_>(g) :> System.Collections.Generic.IEnumerator<_>)

        let GenerateFromEnumerator (e:System.Collections.Generic.IEnumerator<'T>) =
            match e with 
            | :? EnumeratorWrappingLazyGenerator<'T> as e ->  e.Generator
            | _ -> (new LazyGeneratorWrappingEnumerator<'T>(e) :> Generator<'T>)
      
namespace Microsoft.FSharp.Core.CompilerServices

    open System
    open System.Diagnostics
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics
    open System.Collections
    open System.Collections.Generic

    module RuntimeHelpers =
        
        [<Struct; NoComparison; NoEquality>]
        type internal StructBox<'T when 'T : equality>(value:'T) = 
            member x.Value = value
            static member Comparer =
                let gcomparer = HashIdentity.Structural<'T>
                { new IEqualityComparer<StructBox<'T>> with 
                       member __.GetHashCode(v) = gcomparer.GetHashCode(v.Value)
                       member __.Equals(v1,v2) = gcomparer.Equals(v1.Value,v2.Value) }

        let inline checkNonNull argName arg = 
            match box arg with 
            | null -> nullArg argName 
            | _ -> ()

        let mkSeq f = 
            { new IEnumerable<'U> with 
                member x.GetEnumerator() = f()
              interface IEnumerable with 
                member x.GetEnumerator() = (f() :> IEnumerator) }

        [<NoEquality; NoComparison>]
        type EmptyEnumerable<'T> = 
            | EmptyEnumerable
            interface IEnumerable<'T> with 
                member x.GetEnumerator() = IEnumerator.Empty<'T>()
            interface IEnumerable with 
                member x.GetEnumerator() = (IEnumerator.Empty<'T>() :> IEnumerator) 

        let Generate openf compute closef = 
            mkSeq (fun () -> IEnumerator.generateWhileSome openf compute closef) 
        
        let GenerateUsing (openf : unit -> ('U :> System.IDisposable)) compute = 
            Generate openf compute (fun (s:'U) -> s.Dispose())

        let EnumerateFromFunctions opener moveNext current = 
            Generate 
                opener 
                (fun x -> if moveNext x then Some(current x) else None) 
                (fun x -> match box(x) with :? System.IDisposable as id -> id.Dispose() | _ -> ())

        // A family of enumerators that can have additional 'finally' actions added to the enumerator through
        // the use of mutation. This is used to 'push' the disposal action for a 'use' into the next enumerator.
        // For example,
        //    seq { use x = ...
        //          while ... }
        // results in the 'while' loop giving an adjustable enumerator. This is then adjusted by adding the disposal action
        // from the 'use' into the enumerator. This means that we avoid constructing a two-deep enumerator chain in this
        // common case.
        type IFinallyEnumerator = 
            abstract AppendFinallyAction : (unit -> unit) -> unit

        /// A concrete implementation of IEnumerable that adds the given compensation to the "Dispose" chain of any
        /// enumerators returned by the enumerable.
        [<Sealed>]
        type FinallyEnumerable<'T>(compensation: unit -> unit, restf: unit -> seq<'T>) = 
            interface IEnumerable<'T> with 
                member x.GetEnumerator() = 
                    try 
                        let ie = restf().GetEnumerator()
                        match ie with 
                        | :? IFinallyEnumerator as a -> 
                            a.AppendFinallyAction(compensation); 
                            ie
                        | _ -> 
                            IEnumerator.EnumerateThenFinally compensation ie 
                    with e -> 
                        compensation(); 
                        reraise() 
            interface IEnumerable with 
                member x.GetEnumerator() = ((x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator) 
              
        /// An optimized object for concatenating a sequence of enumerables
        [<Sealed>]
        type ConcatEnumerator<'T,'U when 'U :> seq<'T>>(sources: seq<'U>) = 
            let mutable outerEnum = sources.GetEnumerator()
            let mutable currInnerEnum = IEnumerator.Empty()

            let mutable started = false 
            let mutable finished = false 
            let mutable compensations = []
            
            [<DefaultValue(false)>] // false = unchecked
            val mutable private currElement : 'T

            member x.Finish() = 
                finished <- true
                try
                    match currInnerEnum with 
                    | null -> ()
                    | _ -> 
                        try
                            currInnerEnum.Dispose()
                        finally
                            currInnerEnum <- null
                finally
                    try
                        match outerEnum with 
                        | null -> ()
                        | _ -> 
                            try
                                outerEnum.Dispose()
                            finally
                                outerEnum <- null
                    finally
                        let rec iter comps =
                            match comps with
                            |   [] -> ()
                            |   h::t ->
                                    try h() finally iter t
                        try
                            compensations |> List.rev |> iter
                        finally
                            compensations <- []
                
            member x.GetCurrent() = 
                IEnumerator.check started;
                if finished then IEnumerator.alreadyFinished() else x.currElement
            
            interface IFinallyEnumerator with 
                member x.AppendFinallyAction(f) = 
                    compensations <- f :: compensations
                    
            interface IEnumerator<'T> with 
                member x.Current = x.GetCurrent()

            interface IEnumerator with 
                member x.Current = box (x.GetCurrent())
                   
                member x.MoveNext() = 
                   if not started then (started <- true) 
                   if finished then false
                   else 
                      let rec takeInner () = 
                        // check the inner list
                        if currInnerEnum.MoveNext() then 
                            x.currElement <- currInnerEnum.Current; 
                            true
                        else
                            // check the outer list
                            let rec takeOuter() = 
                                if outerEnum.MoveNext() then 
                                    let ie = outerEnum.Current 
                                    // Optimization to detect the statically-allocated empty IEnumerables
                                    match box ie with
                                    | :? EmptyEnumerable<'T> -> 
                                         // This one is empty, just skip, don't call GetEnumerator, try again
                                         takeOuter()
                                    | _ -> 
                                         // OK, this one may not be empty.
                                         // Don't forget to dispose of the enumerator for the inner list now we're done with it
                                         currInnerEnum.Dispose(); 
                                         currInnerEnum <- ie.GetEnumerator(); 
                                         takeInner ()
                                else 
                                    // We're done
                                    x.Finish()
                                    false
                            takeOuter()
                      takeInner () 

                member x.Reset() = IEnumerator.noReset()

            interface System.IDisposable with 
                member x.Dispose() = 
                    if not finished then 
                        x.Finish() 
          
        let EnumerateUsing (resource : 'T :> System.IDisposable) (rest: 'T -> #seq<'U>) = 
            (FinallyEnumerable((fun () -> match box resource with null -> () | _ -> resource.Dispose()), 
                               (fun () -> rest resource :> seq<_>)) :> seq<_>)

        let mkConcatSeq (sources: seq<'U :> seq<'T>>) = 
            mkSeq (fun () -> new ConcatEnumerator<_,_>(sources) :> IEnumerator<'T>)

        let EnumerateWhile (g : unit -> bool) (b: seq<'T>) : seq<'T> = 
            let started = ref false 
            let curr = ref None
            let getCurr() = 
                IEnumerator.check !started;
                match !curr with None -> IEnumerator.alreadyFinished() | Some x -> x 
            let start() = if not !started then (started := true) 

            let finish() = (curr := None) 
            mkConcatSeq 
               (mkSeq (fun () -> 
                    { new IEnumerator<_> with 
                          member x.Current = getCurr()
                       interface IEnumerator with 
                          member x.Current = box (getCurr())
                          member x.MoveNext() = 
                               start();
                               let keepGoing = (try g() with e -> finish (); reraise ()) in
                               if keepGoing then 
                                   curr := Some(b); true
                               else 
                                   finish(); false
                          member x.Reset() = IEnumerator.noReset()
                       interface System.IDisposable with 
                          member x.Dispose() = () }))

        let EnumerateThenFinally (rest : seq<'T>) (compensation : unit -> unit)  =
            (FinallyEnumerable(compensation, (fun () -> rest)) :> seq<_>)

        let CreateEvent (add : 'Delegate -> unit) (remove : 'Delegate -> unit) (create : (obj -> 'Args -> unit) -> 'Delegate ) :IEvent<'Delegate,'Args> = 
            { new obj() with
                  member x.ToString() = "<published event>"
              interface IEvent<'Delegate,'Args> 
              interface IDelegateEvent<'Delegate> with 
                 member x.AddHandler(h) = add h 
                 member x.RemoveHandler(h) = remove h 
              interface System.IObservable<'Args> with 
                 member x.Subscribe(r:IObserver<'Args>) = 
                     let h = create (fun _ args -> r.OnNext(args))
                     add h 
                     { new System.IDisposable with 
                          member x.Dispose() = remove h } }


    [<AbstractClass>]
    type GeneratedSequenceBase<'T>() =
        let mutable redirectTo : GeneratedSequenceBase<'T> = Unchecked.defaultof<_>
        let mutable redirect : bool = false
        
        abstract GetFreshEnumerator : unit -> IEnumerator<'T>
        abstract GenerateNext : next:byref<IEnumerable<'T>> -> int // 0 = Stop, 1 = Yield, 2 = Goto
        abstract Close: unit -> unit
        abstract CheckClose: bool
        abstract LastGenerated : 'T
        
        member x.MoveNextImpl() = 
             let active = 
                 if redirect then redirectTo
                 else x
             let mutable target = null
             match active.GenerateNext(&target) with 
             | 1 -> 
                 true
             | 2 -> 
                 match target.GetEnumerator() with 
                 | :? GeneratedSequenceBase<'T> as g when not active.CheckClose -> 
                     redirectTo <- g
                 | e -> 
                     redirectTo <- 
                           { new GeneratedSequenceBase<'T>() with 
                                 member x.GetFreshEnumerator() = e
                                 member x.GenerateNext(_) = if e.MoveNext() then 1 else 0 
                                 member x.Close() = try e.Dispose() finally active.Close()
                                 member x.CheckClose = true
                                 member x.LastGenerated = e.Current }
                 redirect <- true
                 x.MoveNextImpl()
             | _ (* 0 *)  -> 
                 false
        
        interface IEnumerable<'T> with 
            member x.GetEnumerator() = x.GetFreshEnumerator()
        interface IEnumerable with
            member x.GetEnumerator() = (x.GetFreshEnumerator() :> IEnumerator);
        interface IEnumerator<'T> with 
            member x.Current = if redirect then redirectTo.LastGenerated else x.LastGenerated
            member x.Dispose() = if redirect then redirectTo.Close() else x.Close()
        interface IEnumerator with
            member x.Current = box (if redirect then redirectTo.LastGenerated else x.LastGenerated)
            member x.MoveNext() = x.MoveNextImpl() 
               
            member x.Reset() = raise <| new System.NotSupportedException();


namespace Microsoft.FSharp.Collections

    open System
    open System.Diagnostics
    open System.Collections
    open System.Collections.Generic
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics

    [<Sealed>]
    type CachedSeq<'T>(cleanup,res:seq<'T>) =
        interface System.IDisposable with 
            member x.Dispose() = cleanup()
        interface System.Collections.Generic.IEnumerable<'T> with 
            member x.GetEnumerator() = res.GetEnumerator()
        interface System.Collections.IEnumerable with 
            member x.GetEnumerator() = (res :> System.Collections.IEnumerable).GetEnumerator()
        member obj.Clear() = cleanup()
           

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Seq = 

        open Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers
        
        let mkDelayedSeq (f: unit -> IEnumerable<'T>) = mkSeq (fun () -> f().GetEnumerator())
        let mkUnfoldSeq f x = mkSeq (fun () -> IEnumerator.unfold f x) 
        
        [<CompiledName("Delay")>]
        let delay f = mkDelayedSeq f

        [<CompiledName("Unfold")>]
        let unfold f x = mkUnfoldSeq f x
        
        [<CompiledName("Empty")>]
        let empty<'T> = (EmptyEnumerable :> seq<'T>)

        [<CompiledName("InitializeInfinite")>]
        let initInfinite f = mkSeq (fun () -> IEnumerator.upto None f)

        [<CompiledName("Initialize")>]
        let init count f =
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            mkSeq (fun () -> IEnumerator.upto (Some (count-1)) f)

        [<CompiledName("Iterate")>]
        let iter f (source : seq<'T>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            while e.MoveNext() do
                f e.Current;

        [<CompiledName("Get")>]
        let nth     i (source : seq<'T>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            IEnumerator.nth i e

        [<CompiledName("IterateIndexed")>]
        let iteri f (source : seq<'T>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            let mutable i = 0 
            while e.MoveNext() do
                f i e.Current;
                i <- i + 1;

        [<CompiledName("Exists")>]
        let exists f (source : seq<'T>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            let mutable state = false
            while (not state && e.MoveNext()) do
                state <- f e.Current
            state
            
        [<CompiledName("ForAll")>]
        let forall f (source : seq<'T>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            let mutable state = true 
            while (state && e.MoveNext()) do
                state <- f e.Current
            state
            
            
        [<CompiledName("Iterate2")>]
        let iter2 f (source1 : seq<_>) (source2 : seq<_>)    = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            while (e1.MoveNext() && e2.MoveNext()) do
                f e1.Current e2.Current;


        // Build an IEnumerble by wrapping/transforming iterators as they get generated.
        let revamp f (ie : seq<_>) = mkSeq (fun () -> f (ie.GetEnumerator()))
        let revamp2 f (ie1 : seq<_>) (source2 : seq<_>) = 
            mkSeq (fun () -> f (ie1.GetEnumerator()) (source2.GetEnumerator()))

        [<CompiledName("Filter")>]
        let filter f source      = 
            checkNonNull "source" source
            revamp  (IEnumerator.filter f) source

        [<CompiledName("Map")>]
        let map    f source      = 
            checkNonNull "source" source
            revamp  (IEnumerator.map    f) source

        [<CompiledName("MapIndexed")>]
        let mapi f source      = 
            checkNonNull "source" source
            revamp  (IEnumerator.mapi   f) source

        [<CompiledName("Map2")>]
        let map2 f source1 source2 = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            revamp2 (IEnumerator.map2    f) source1 source2

        [<CompiledName("Choose")>]
        let choose f source      = 
            checkNonNull "source" source
            revamp  (IEnumerator.choose f) source

        [<CompiledName("Zip")>]
        let zip source1 source2  = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            map2 (fun x y -> x,y) source1 source2

        [<CompiledName("Zip3")>]
        let zip3 source1 source2  source3 = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            checkNonNull "source3" source3
            map2 (fun x (y,z) -> x,y,z) source1 (zip source2 source3)

        [<CompiledName("Cast")>]
        let cast (source: IEnumerable) = 
            checkNonNull "source" source
            mkSeq (fun () -> IEnumerator.cast (source.GetEnumerator()))

        [<CompiledName("TryPick")>]
        let tryPick f (source : seq<'T>)  = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            let mutable res = None 
            while (Option.isNone res && e.MoveNext()) do
                res <-  f e.Current;
            res

        [<CompiledName("Pick")>]
        let pick f source  = 
            checkNonNull "source" source
            match tryPick f source with 
            | None -> raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  
            | Some x -> x
          
        [<CompiledName("TryFind")>]
        let tryFind f (source : seq<'T>)  = 
            checkNonNull "source" source
            use e = source.GetEnumerator()
            let mutable res = None 
            while (Option.isNone res && e.MoveNext()) do
                let c = e.Current 
                if f c then res <- Some(c)
            res

        [<CompiledName("Find")>]
        let find f source = 
            checkNonNull "source" source
            match tryFind f source with 
            | None -> raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  
            | Some x -> x

        [<CompiledName("Take")>]
        let take count (source : seq<'T>)    = 
            checkNonNull "source" source
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            (* Note: don't create or dispose any IEnumerable if n = 0 *)
            if count = 0 then empty else  
            seq { use e = source.GetEnumerator() 
                  for _ in 0 .. count - 1 do
                      if not (e.MoveNext()) then
                          raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
                      yield e.Current }

        [<CompiledName("IsEmpty")>]
        let isEmpty (source : seq<'T>)  = 
            checkNonNull "source" source
            match source with 
            | :? ('T[]) as a -> a.Length = 0
            | :? list<'T> as a -> a.IsEmpty
            | :? ICollection<'T> as a -> a.Count = 0
            | _ -> 
                use ie = source.GetEnumerator()
                not (ie.MoveNext())


        [<CompiledName("Concat")>]
        let concat sources = 
            checkNonNull "sources" sources
            mkConcatSeq sources

        [<CompiledName("Length")>]
        let length (source : seq<'T>)    = 
            checkNonNull "source" source
            match source with 
            | :? ('T[]) as a -> a.Length
            | :? ('T list) as a -> a.Length
            | :? ICollection<'T> as a -> a.Count
            | _ -> 
                use e = source.GetEnumerator() 
                let mutable state = 0 
                while e.MoveNext() do
                    state <-  state + 1;
                state

        [<CompiledName("Fold")>]
        let fold<'T,'State> f (x:'State) (source : seq<'T>)  = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            let mutable state = x 
            while e.MoveNext() do
                state <- f state  e.Current;
            state

        [<CompiledName("Reduce")>]
        let reduce f (source : seq<'T>)  = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            if not (e.MoveNext()) then invalidArg "source" InputSequenceEmptyString;
            let mutable state = e.Current 
            while e.MoveNext() do
                state <- f state  e.Current;
            state

        let fromGenerator f = mkSeq(fun () -> Generator.EnumerateFromGenerator (f()))
        let toGenerator (ie : seq<_>) = Generator.GenerateFromEnumerator (ie.GetEnumerator())


        [<CompiledName("Append")>]
        let append (source1: seq<'T>) (source2: seq<'T>) = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            fromGenerator(fun () -> Generator.bindG (toGenerator source1) (fun () -> toGenerator source2))

        
        [<CompiledName("Collect")>]
        let collect f sources = map f sources |> concat

        [<CompiledName("CompareWith")>]
        let compareWith (f:'T -> 'T -> int) (source1 : seq<'T>) (source2: seq<'T>) = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let rec go () = 
                let e1ok = e1.MoveNext() 
                let e2ok = e2.MoveNext() 
                let c = (if e1ok = e2ok then 0 else if e1ok then 1 else -1) 
                if c <> 0 then c else
                if not e1ok || not e2ok then 0 
                else
                    let c = f e1.Current e2.Current 
                    if c <> 0 then c else
                    go () 
            go()

        [<CompiledName("OfList")>]
        let ofList (source : 'T list) = 
            (source :> seq<'T>)

        [<CompiledName("ToList")>]
        let toList (source : seq<'T>) = 
            checkNonNull "source" source
            match source with 
            | :? ('T list) as res -> res
            | :? ('T[]) as res -> List.ofArray res
            | _ -> 
                use e = source.GetEnumerator()
                let mutable res = [] 
                while e.MoveNext() do
                    res <- e.Current :: res
                List.rev res

        // Create a new object to ensure underlying array may not be mutated by a backdoor cast 
        [<CompiledName("OfArray")>]
        let ofArray (source : 'T array) = 
            checkNonNull "source" source
            mkSeq (fun () -> IEnumerator.ofArray source)        
            
        [<CompiledName("ToArray")>]
        let toArray (source : seq<'T>)  = 
            checkNonNull "source" source
            match source with 
            | :? ('T[]) as res -> (res.Clone() :?> 'T[])
            | :? ('T list) as res -> List.toArray res
            //| :? ICollection<'T> as res -> ...
            | _ -> 
                use e = source.GetEnumerator()
                let res = new ResizeArray<_>()
                while e.MoveNext() do
                    res.Add(e.Current)
                res.ToArray()


        [<CompiledName("Singleton")>]
        let singleton x = mkSeq (fun () -> IEnumerator.Singleton x)


        [<CompiledName("Truncate")>]
        let truncate n (source: seq<'T>) =
            checkNonNull "source" source
            seq { let i = ref 0
                  use ie = source.GetEnumerator() 
                  while !i < n && ie.MoveNext() do
                     i := !i + 1
                     yield ie.Current }

        [<CompiledName("Pairwise")>]
        let pairwise (source: seq<'T>) =
            checkNonNull "source" source
            seq { use ie = source.GetEnumerator() 
                  if ie.MoveNext() then
                      let iref = ref ie.Current
                      while ie.MoveNext() do
                          let j = ie.Current 
                          yield (!iref, j)
                          iref := j }

        [<CompiledName("Scan")>]
        let scan<'T,'State> f (z:'State) (source : seq<'T>) = 
            checkNonNull "source" source
            seq { let zref = ref z
                  yield !zref
                  use ie = source.GetEnumerator() 
                  while ie.MoveNext() do
                      zref := f !zref ie.Current 
                      yield !zref }

        [<CompiledName("FindIndex")>]
        let findIndex p (source:seq<_>) = 
            checkNonNull "source" source
            use ie = source.GetEnumerator() 
            let rec loop i = 
                if ie.MoveNext() then 
                    if p ie.Current then
                        i
                    else loop (i+1)
                else
                    raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt))) 
            loop 0

        [<CompiledName("TryFindIndex")>]
        let tryFindIndex p (source:seq<_>) = 
            checkNonNull "source" source
            use ie = source.GetEnumerator() 
            let rec loop i = 
                if ie.MoveNext() then 
                    if p ie.Current then
                        Some i
                    else loop (i+1)
                else
                    None
            loop 0

        

        // windowed : int -> seq<'T> -> seq<'T[]>
        [<CompiledName("Windowed")>]
        let windowed windowSize (source: seq<_>) =    
            checkNonNull "source" source
            if windowSize <= 0 then invalidArg "windowSize" (SR.GetString(SR.inputMustBeNonNegative))
            seq { let arr = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked windowSize 
                  let r = ref (windowSize-1)
                  let i = ref 0 
                  use e = source.GetEnumerator() 
                  while e.MoveNext() do 
                      arr.[!i] <- e.Current
                      i := (!i + 1) % windowSize
                      if !r = 0 then 
                          yield Array.init windowSize (fun j -> arr.[(!i+j) % windowSize])
                      else 
                      r := (!r - 1) }

        [<CompiledName("Cache")>]
        let cache (source : seq<'T>) = 
            checkNonNull "source" source
            // Wrap a seq to ensure that it is enumerated just once and only as far as is necessary.
            //
            // This code is required to be thread safe.
            // The necessary calls should be called at most once (include .MoveNext() = false).
            // The enumerator should be disposed (and dropped) when no longer required.
            //------
            // The state is (prefix,enumerator) with invariants:
            //   * the prefix followed by elts from the enumerator are the initial sequence.
            //   * the prefix contains only as many elements as the longest enumeration so far.
            let prefix      = ResizeArray<_>()
            let enumeratorR = ref None : IEnumerator<'T> option option ref // nested options rather than new type...
                               // None          = Unstarted.
                               // Some(Some e)  = Started.
                               // Some None     = Finished.
            let oneStepTo i = 
              // If possible, step the enumeration to prefix length i (at most one step).
              // Be speculative, since this could have already happened via another thread.
              if not (i < prefix.Count) then // is a step still required?
                  // If not yet started, start it (create enumerator).
                  match !enumeratorR with 
                  | None -> enumeratorR := Some (Some (source.GetEnumerator()))
                  | Some _ -> ()
                  match (!enumeratorR).Value with
                  | Some enumerator -> if enumerator.MoveNext() then
                                          prefix.Add(enumerator.Current)
                                       else
                                          enumerator.Dispose()     // Move failed, dispose enumerator,
                                          enumeratorR := Some None // drop it and record finished.
                  | None -> ()
            let result = 
                unfold (fun i -> 
                              // i being the next position to be returned
                              // A lock is needed over the reads to prefix.Count since the list may be being resized
                              // NOTE: we could change to a reader/writer lock here
                              lock enumeratorR (fun () -> 
                                  if i < prefix.Count then
                                    Some (prefix.[i],i+1)
                                  else
                                    oneStepTo i
                                    if i < prefix.Count then
                                      Some (prefix.[i],i+1)
                                    else
                                      None)) 0
            let cleanup() = 
               lock enumeratorR (fun () -> 
                   prefix.Clear()
                   begin match !enumeratorR with 
                   | Some (Some e) -> IEnumerator.dispose e; 
                   | _ -> ()
                   end;
                   enumeratorR := None)
            (new CachedSeq<_>(cleanup, result) :> seq<_>)

        [<CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1709:IdentifiersShouldBeCasedCorrectly"); CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1707:IdentifiersShouldNotContainUnderscores"); CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1704:IdentifiersShouldBeSpelledCorrectly")>]             
        [<CompiledName("ReadOnly")>]
        let readonly (source:seq<_>) = 
            checkNonNull "source" source
            mkSeq (fun () -> source.GetEnumerator())



        [<CompiledName("GroupBy")>]
        let groupBy keyf seq =

            mkDelayedSeq (fun () -> 
                // Wrap a StructBox(_) around all keys in case the key type is itself a type using null as a representation
                let dict = new Dictionary<StructBox<'Key>,ResizeArray<'T>>(StructBox<'Key>.Comparer)

                // Build the groupings
                seq |> iter (fun v -> 
                    let key = StructBox (keyf v)
                    let ok,prev = dict.TryGetValue(key)
                    if ok then 
                        prev.Add(v)
                    else 
                        let prev = new ResizeArray<'T>(1)
                        dict.[key] <- prev
                        prev.Add(v))

                // Trim the size of each result group.
                dict |> iter (fun group -> group.Value.TrimExcess())
                         
                // Return the sequence-of-sequences. Don't reveal the 
                // internal collections: just reveal them as sequences
                dict |> map (fun group -> (group.Key.Value, readonly group.Value)))

        [<CompiledName("Distinct")>]
        let distinct source =
            checkNonNull "source" source
            seq {   // Wrap a StructBox(_) aroud all keys in case the key type is itself a type using null as a representation
                let dict = new Dictionary<StructBox<'T>,obj>(StructBox<'T>.Comparer)
                for v in source do 
                    let key = StructBox v
                    if not (dict.ContainsKey(key)) then 
                        dict.[key] <- null; 
                        yield v }

        [<CompiledName("DistinctBy")>]
        let distinctBy keyf source =
            checkNonNull "source" source
            seq { // Wrap a StructBox(_) aroud all keys in case the key type is itself a type using null as a representation
                  let dict = new Dictionary<StructBox<'Key>,obj>(StructBox<'Key>.Comparer)
                  for v in source do
                    let key = StructBox (keyf v)
                    if not (dict.ContainsKey(key)) then 
                        dict.[key] <- null; 
                        yield v }

        [<CompiledName("SortBy")>]
        let sortBy keyf source =
            checkNonNull "source" source
            mkDelayedSeq (fun () -> 
                let array = source |> toArray 
                Array.stableSortInPlaceBy keyf array
                array :> seq<_>)

        [<CompiledName("Sort")>]
        let sort source =
            checkNonNull "source" source
            mkDelayedSeq (fun () -> 
                let array = source |> toArray 
                Array.stableSortInPlace array
                array :> seq<_>)

        [<CompiledName("CountBy")>]
        let countBy keyf source =
            checkNonNull "source" source
            mkDelayedSeq (fun () -> 
                let dict = new Dictionary<StructBox<'Key>,int>(StructBox<'Key>.Comparer)

                // Build the groupings
                source |> iter (fun v -> 
                    let key = StructBox (keyf v )
                    let mutable prev = Unchecked.defaultof<_>
                    let ok = dict.TryGetValue(key, &prev)
                    if ok then dict.[key] <- prev + 1
                    else dict.[key] <- 1)

                dict |> map (fun group -> (group.Key.Value, group.Value)))

        [<CompiledName("Sum")>]
        let inline sum (source: seq< (^a) >) : ^a = 
            use e = source.GetEnumerator() 
            let mutable acc = LanguagePrimitives.GenericZero< (^a) >
            while e.MoveNext() do
                acc <- Checked.(+) acc e.Current
            acc

        [<CompiledName("SumBy")>]
        let inline sumBy (f : 'T -> ^U) (source: seq<'T>) : ^U = 
            use e = source.GetEnumerator() 
            let mutable acc = LanguagePrimitives.GenericZero< (^U) >
            while e.MoveNext() do
                acc <- Checked.(+) acc (f e.Current)
            acc

        [<CompiledName("Average")>]
        let inline average (source: seq< (^a) >) : ^a = 
            checkNonNull "source" source
            use e = source.GetEnumerator()     
            let mutable acc = LanguagePrimitives.GenericZero< (^a) >
            let mutable count = 0
            while e.MoveNext() do
                acc <- Checked.(+) acc e.Current
                count <- count + 1
            if count = 0 then 
                invalidArg "source" InputSequenceEmptyString
            LanguagePrimitives.DivideByInt< (^a) > acc count

        [<CompiledName("AverageBy")>]
        let inline averageBy (f : 'T -> ^U) (source: seq< 'T >) : ^U = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            let mutable acc = LanguagePrimitives.GenericZero< (^U) >
            let mutable count = 0
            while e.MoveNext() do
                acc <- Checked.(+) acc (f e.Current)
                count <- count + 1
            if count = 0 then 
                invalidArg "source" InputSequenceEmptyString;
            LanguagePrimitives.DivideByInt< (^U) > acc count
            
        [<CompiledName("Min")>]
        let inline min (source: seq<_>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            if not (e.MoveNext()) then 
                invalidArg "source" InputSequenceEmptyString;
            let mutable acc = e.Current
            while e.MoveNext() do
                let curr = e.Current 
                if curr < acc then 
                    acc <- curr
            acc

        [<CompiledName("MinBy")>]
        let inline minBy (f : 'T -> 'U) (source: seq<'T>) : 'T = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            if not (e.MoveNext()) then 
                invalidArg "source" InputSequenceEmptyString;
            let first = e.Current
            let mutable acc = f first
            let mutable accv = first
            while e.MoveNext() do
                let currv = e.Current
                let curr = f currv
                if curr < acc then
                    acc <- curr
                    accv <- currv
            accv

        [<CompiledName("Max")>]
        let inline max (source: seq<_>) = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            if not (e.MoveNext()) then 
                invalidArg "source" InputSequenceEmptyString;
            let mutable acc = e.Current
            while e.MoveNext() do
                let curr = e.Current 
                if curr > acc then 
                    acc <- curr
            acc

        [<CompiledName("MaxBy")>]
        let inline maxBy (f : 'T -> 'U) (source: seq<'T>) : 'T = 
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            if not (e.MoveNext()) then 
                invalidArg "source" InputSequenceEmptyString;
            let first = e.Current
            let mutable acc = f first
            let mutable accv = first
            while e.MoveNext() do
                let currv = e.Current
                let curr = f currv
                if curr > acc then
                    acc <- curr
                    accv <- currv
            accv

        [<CompiledName("TakeWhile")>]
        let takeWhile p (source: seq<_>) = 
            checkNonNull "source" source
            seq { use e = source.GetEnumerator() 
                  let latest = ref Unchecked.defaultof<_>
                  while e.MoveNext() && (latest := e.Current; p !latest) do 
                      yield !latest }

        [<CompiledName("Skip")>]
        let skip count (source: seq<_>) =
            checkNonNull "source" source
            seq { use e = source.GetEnumerator() 
                  for _ in 1 .. count do
                      if not (e.MoveNext()) then 
                          raise <| System.InvalidOperationException (SR.GetString(SR.notEnoughElements))
                  while e.MoveNext() do
                      yield e.Current }

        [<CompiledName("SkipWhile")>]
        let skipWhile p (source: seq<_>) = 
            checkNonNull "source" source
            seq { use e = source.GetEnumerator() 
                  let latest = ref (Unchecked.defaultof<_>)
                  let ok = ref false
                  while e.MoveNext() do
                      if (latest := e.Current; (!ok || not (p !latest))) then
                          ok := true
                          yield !latest }


        [<CompiledName("ForAll2")>]
        let forall2 p (source1: seq<_>) (source2: seq<_>) = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let mutable ok = true
            while (ok && e1.MoveNext() && e2.MoveNext()) do
                ok <- p e1.Current e2.Current;
            ok

        
        [<CompiledName("Exists2")>]
        let exists2 p (source1: seq<_>) (source2: seq<_>) = 
            checkNonNull "source1" source1
            checkNonNull "source2" source2
            use e1 = source1.GetEnumerator()
            use e2 = source2.GetEnumerator()
            let mutable ok = false
            while (not ok && e1.MoveNext() && e2.MoveNext()) do
                ok <- p e1.Current e2.Current;
            ok

        [<CompiledName("Head")>]
        let head (source : seq<_>) =
            checkNonNull "source" source
            use e = source.GetEnumerator() 
            if (e.MoveNext()) then e.Current
            else invalidArg "source" InputSequenceEmptyString

