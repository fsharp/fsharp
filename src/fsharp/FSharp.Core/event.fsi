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

    open System
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections

#if FX_NO_DELEGATE_DYNAMIC_METHOD // not available on CompactFramework 2.0
#else
    /// <summary>Event implementations for an arbitrary type of delegate.</summary>
    [<CompiledName("FSharpDelegateEvent`1")>]
    type DelegateEvent<'Delegate when 'Delegate :> System.Delegate> = 
        /// <summary>Creates an event object suitable for implementing an arbitrary type of delegate.</summary>
        /// <returns>The event object.</returns>
        new : unit -> DelegateEvent<'Delegate>
        /// <summary>Triggers the event using the given parameters.</summary>
        /// <param name="args">The parameters for the event.</param>
        member Trigger : args:obj[] -> unit
        /// <summary>Publishes the event as a first class event value.</summary>
        member Publish : IDelegateEvent<'Delegate>
        

    /// <summary>Event implementations for a delegate types following the standard .NET Framework convention of a first 'sender' argument.</summary>
    [<CompiledName("FSharpEvent`2")>]
    type Event<'Delegate,'Args when 'Delegate : delegate<'Args,unit> and 'Delegate :> System.Delegate > = 
        /// <summary>Creates an event object suitable for delegate types following the standard .NET Framework convention of a first 'sender' argument.</summary>
        /// <returns>The created event.</returns>
        new : unit -> Event<'Delegate,'Args>
        /// <summary>Triggers the event using the given sender object and parameters. The sender object may be <c>null</c>.</summary>
        /// <param name="sender">The object triggering the event.</param>
        /// <param name="args">The parameters for the event.</param>
        member Trigger : sender:obj * args:'Args -> unit
        /// <summary>Publishes the event as a first class event value.</summary>
        member Publish : IEvent<'Delegate,'Args> 

#endif
    /// <summary>Event implementations for the IEvent&lt;_&gt; type.</summary>
    [<CompiledName("FSharpEvent`1")>]
    type Event<'T> = 
        /// <summary>Creates an observable object.</summary>
        /// <returns>The created event.</returns>
        new : unit -> Event<'T>
        /// <summary>Triggers an observation using the given parameters.</summary>
        /// <param name="arg">The event parameters.</param>
        member Trigger : arg:'T -> unit
        /// <summary>Publishes an observation  as a first class value.</summary>
        member Publish : IEvent<'T> 
