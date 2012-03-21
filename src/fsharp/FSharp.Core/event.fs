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

namespace Microsoft.FSharp.Control

    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Control
    open System.Reflection
    open System.Diagnostics

#if FX_NO_DELEGATE_DYNAMIC_METHOD 
#else

#if FX_NO_DELEGATE_DYNAMIC_INVOKE 
    module Impl = 
        type System.Delegate with 
            member d.DynamicInvoke(args: obj[]) =
                d.Method.Invoke(d.Target, BindingFlags.Default |||  BindingFlags.Public ||| BindingFlags.NonPublic , null, args, null)

    open Impl
#endif
    
    [<CompiledName("FSharpDelegateEvent`1")>]
    type DelegateEvent<'Delegate when 'Delegate :> System.Delegate>() = 
        let mutable multicast : System.Delegate = null
        member x.Trigger(args:obj[]) = 
            match multicast with 
            | null -> ()
            | d -> d.DynamicInvoke(args) |> ignore
        member x.Publish = 
            { new IDelegateEvent<'Delegate> with 
                member x.AddHandler(d) =
                    multicast <- System.Delegate.Combine(multicast, d)
                member x.RemoveHandler(d) = 
                    multicast <- System.Delegate.Remove(multicast, d) }
    
    type EventDelegee<'Args>(observer: System.IObserver<'Args>) =
        member x.Invoke(sender:obj, args: 'Args) = observer.OnNext args

    type EventWrapper<'Delegate,'Args> = delegate of 'Delegate * obj * 'Args -> unit

    [<CompiledName("FSharpEvent`2")>]
    type Event<'Delegate,'Args when 'Delegate : delegate<'Args,unit> and 'Delegate :> System.Delegate >() = 
        let mutable multicast : 'Delegate = Unchecked.defaultof<_>     

#if FX_NO_DELEGATE_CREATE_DELEGATE_FROM_STATIC_METHOD
#else
        static let argTypes = 
            let instanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
            let mi = typeof<'Delegate>.GetMethod("Invoke",instanceBindingFlags)
            mi.GetParameters() |> (fun arr -> arr.[1..]) |> Array.map (fun p -> p.ParameterType)

        // For the one-argument case, use an optimization that allows a fast call. 
        // CreateDelegate creates a delegate that is fast to invoke.
        static let invoker = 
            if argTypes.Length = 1 then 
                (System.Delegate.CreateDelegate(typeof<EventWrapper<'Delegate,'Args>>, typeof<'Delegate>.GetMethod("Invoke")) :?> EventWrapper<'Delegate,'Args>)
            else
                null
#endif

        // For the multi-arg case, use a slower DynamicInvoke.
        static let invokeInfo =
            let instanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
            typeof<EventDelegee<'Args>>.GetMethod("Invoke",instanceBindingFlags)
        
        member x.Trigger(sender:obj,args:'Args) = 
            match box multicast with 
            | null -> ()
            | _ -> 
#if FX_NO_DELEGATE_CREATE_DELEGATE_FROM_STATIC_METHOD
#else
                match invoker with 
                | null ->  
#endif
                    let args = Array.append [| sender |] (Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields(box args))
                    multicast.DynamicInvoke(args) |> ignore
#if FX_NO_DELEGATE_CREATE_DELEGATE_FROM_STATIC_METHOD
#else
                | _ -> 
                    // For the one-argument case, use an optimization that allows a fast call. 
                    // CreateDelegate creates a delegate that is fast to invoke.
                    invoker.Invoke(multicast, sender, args) |> ignore
#endif
        
        member x.Publish =
            // Note, we implement each interface explicitly: this works around a bug in the CLR 
            // implementation on CompactFramework 3.7, used on Windows Phone 7
            { new obj() with
                  member x.ToString() = "<published event>"
              interface IEvent<'Delegate,'Args> 
              interface IDelegateEvent<'Delegate> with 
                member e.AddHandler(d) =
                    multicast <- System.Delegate.Combine(multicast, d) :?> 'Delegate 
                member e.RemoveHandler(d) = 
                    multicast <- System.Delegate.Remove(multicast, d)  :?> 'Delegate 
              interface System.IObservable<'Args> with 
                member e.Subscribe(observer) = 
                   let obj = new EventDelegee<'Args>(observer)
                   let h = System.Delegate.CreateDelegate(typeof<'Delegate>, obj, invokeInfo) :?> 'Delegate
                   (e :?> IDelegateEvent<'Delegate>).AddHandler(h)
                   { new System.IDisposable with 
                        member x.Dispose() = (e :?> IDelegateEvent<'Delegate>).RemoveHandler(h) } } 

#endif

    [<CompiledName("FSharpEvent`1")>]
    type Event<'T>() = 
        let mutable multicast : Handler<'T> option = None
        member x.Delegate
                 with get () =  match multicast with None -> null | Some(d) -> (d :> System.Delegate)
                 and  set v =  multicast <- (match v with null -> None | d -> Some d)
        member x.Trigger(arg:'T) = 
            match multicast with 
            | None -> ()
            | Some d -> d.Invoke(null,arg) |> ignore
        member x.Publish =
            // Note, we implement each interface explicitly: this works around a bug in the CLR 
            // implementation on CompactFramework 3.7, used on Windows Phone 7
            { new obj() with
                  member x.ToString() = "<published event>"
              interface IEvent<'T> 
              interface IDelegateEvent<Handler<'T>> with 
                member e.AddHandler(d) =
                    x.Delegate <- (System.Delegate.Combine(x.Delegate, d) :?> Handler<'T>)
                member e.RemoveHandler(d) = 
                    x.Delegate <- (System.Delegate.Remove(x.Delegate, d) :?> Handler<'T>)
              interface System.IObservable<'T> with 
                member e.Subscribe(observer) = 
                   let h = new Handler<_>(fun sender args -> observer.OnNext(args))
                   (e :?> IEvent<_,_>).AddHandler(h)
                   { new System.IDisposable with 
                        member x.Dispose() = (e :?> IEvent<_,_>).RemoveHandler(h) } }
