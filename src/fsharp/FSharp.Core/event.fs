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

    [<CompiledName("FSharpEvent`2")>]
    type Event<'Delegate,'Args when 'Delegate : delegate<'Args,unit> and 'Delegate :> System.Delegate >() = 
        let mutable multicast : System.Delegate = null
        
        static let argTypes = 
            let instanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
            let mi = typeof<'Delegate>.GetMethod("Invoke",instanceBindingFlags)
            mi.GetParameters() |> (fun arr -> arr.[1..]) |> Array.map (fun p -> p.ParameterType)

        static let invokeInfo =
            let instanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
            typeof<EventDelegee<'Args>>.GetMethod("Invoke",instanceBindingFlags)
        
        member x.Trigger(sender:obj,args:'Args) = 
            match multicast with 
            | null -> ()
            | d -> 
                if argTypes.Length = 1 then 
                    d.DynamicInvoke([| sender; box args |]) |> ignore
                else
                    d.DynamicInvoke(Array.append [| sender |] (Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields(box args))) |> ignore
        
        member x.Publish = 
            // Note, we implement each interface explicitly to workaround a WP7 bug
            { new obj() with
                  member x.ToString() = "<published event>"
              interface IEvent<'Delegate,'Args> 
              interface IDelegateEvent<'Delegate> with 
                member e.AddHandler(d) =
                    multicast <- System.Delegate.Combine(multicast, d)
                member e.RemoveHandler(d) = 
                    multicast <- System.Delegate.Remove(multicast, d) 
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
            // Note, we implement each interface explicitly to workaround a WP7 bug
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


