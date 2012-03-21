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

// Various tests for the:
// Microsoft.FSharp.Control.Async type

#nowarn "21"
#nowarn "40"

namespace SystematicUnitTests.FSharp_Core.Microsoft_FSharp_Control

open System
open SystematicUnitTests.LibraryTestFx
open NUnit.Framework
open System.Threading

#if FX_ATLEAST_40
open System.Threading.Tasks
#endif

type RunWithContinuationsTest_WhatToDo =
    | Exit
    | Cancel
    | Throw

[<TestFixture>]
type AsyncType() =

    [<Test>]
    member this.RunWithContinuations() =

        let whatToDo = ref Exit

        let asyncWorkflow() =
            async {
                let currentState = !whatToDo

                // Act
                let result =
                    match currentState with
                    | Exit   -> 1
                    | Cancel -> Async.CancelDefaultGroup()
                                System.Threading.Thread.Sleep(1 * 1000)
                                0
                    | Throw  -> raise <| System.Exception("You asked me to do it!")

                return result
            }

        let onSuccess x   =
            match !whatToDo with
            | Cancel | Throw  
                -> Assert.Fail("Expected onSuccess but whatToDo was not Exit", [| whatToDo |])
            | Exit
                -> ()

        let onException x =
            match !whatToDo with
            | Exit | Cancel
                -> Assert.Fail("Expected onException but whatToDo was not Throw", [| whatToDo |])
            | Throw  -> ()

        let onCancel x    =
            match !whatToDo with
            | Exit | Throw
                -> Assert.Fail("Expected onCancel but whatToDo was not Cancel", [| whatToDo |])
            | Cancel -> ()

        // Run it once.
        whatToDo := Exit
        Async.RunWithContinuations(onSuccess, onException, onCancel, asyncWorkflow())

        whatToDo := Cancel
        Async.RunWithContinuations(onSuccess, onException, onCancel, asyncWorkflow())

        whatToDo := Throw
        Async.RunWithContinuations(onSuccess, onException, onCancel, asyncWorkflow())

        ()

#if FX_ATLEAST_40        
    
    member private this.WaitASec (t:Task) =
        let result = t.Wait(TimeSpan(hours=0,minutes=0,seconds=1))
        Assert.IsTrue(result)        
      
    
    [<Test>]
    member this.CreateTask () =
        let s = "Hello tasks!"
        let a = async { return s }
        let t : Task<string> = Async.CreateTask a
        t.Start()
        this.WaitASec t
        Assert.IsTrue (t.IsCompleted)
        Assert.AreEqual(s, t.Result)    
        
    [<Test>]
    member this.StartTask () =
        let s = "Hello tasks!"
        let a = async { return s }
        let t = Async.StartAsTask a
        this.WaitASec t
        Assert.IsTrue (t.IsCompleted)
        Assert.AreEqual(s, t.Result)    
      
    [<Test>]  
    member this.ExceptionPropagatesToTask () =
        let a = async { 
            do raise (Exception ())
         }
        let t = Async.StartAsTask a
        let mutable exceptionThrown = false
        try 
            this.WaitASec t
        with 
            e -> exceptionThrown <- true
        Assert.IsTrue (t.IsFaulted)
        Assert.IsTrue(exceptionThrown)
        
    [<Test>]
    member this.CancellationPropagatesToTask () =
        let a = async {
                while true do ()
            }
        let t = Async.StartAsTask a
        Async.CancelDefaultGroup () 
        let mutable exceptionThrown = false
        try
            this.WaitASec t
        with e -> exceptionThrown <- true
        Assert.IsTrue (exceptionThrown)   
        Assert.IsTrue(t.IsCanceled)            

//  Commenting out per Dmitry's recommendation. The test was ignored already, now the code needs to be brought back by the F# team with updates for CT
//         
//    [<Test>][<Ignore("Will have to wait until we support CancellationTokens")>]
//    member this.CancellationPropagatesToGroup () =
//        let ewh = new EventWaitHandle(false, EventResetMode.ManualReset)
//        let a = async { 
//                use! holder = Async.OnCancel (fun _ -> ewh.Set() |> Assert.IsTrue)
//                while true do ()
//            }
//        let g = new AsyncGroup()
//        let t = g.StartAsTask a
//        printfn "%A" t.Status
//        t.CancelAndWait()
//        printfn "%A" t.Status        
//        let result = ewh.WaitOne(1000) 
//        Assert.IsTrue(result)


    [<Test>]
    member this.TaskAsyncValue () =
        let s = "Test"
        let t = Task.Factory.StartNew(Func<_>(fun () -> s))
        let a = async {
                let! s1 = t.AsyncValue
                return s = s1
            }
        Async.RunSynchronously(a, 1000) |> Assert.IsTrue        
        
    [<Test>]
    member this.TaskAsyncValueException () =
        let t = Task.Factory.StartNew(Func<unit>(fun () -> raise <| Exception()))
        let a = async {
                try
                    let! v = t.AsyncValue
                    return false
                with e -> return true
              }
        Async.RunSynchronously(a, 1000) |> Assert.IsTrue  
        
    [<Test>]
    member this.TaskAsyncValueCancellation () =
        let ewh = new EventWaitHandle(false, EventResetMode.ManualReset)
        let cts = new CancellationTokenSource()       
        let rec t =  Task.Factory.StartNew(Func<unit>(
            fun () -> 
                while not cts.IsCancellationRequested do ();
                raise(new OperationCanceledException(cts.Token))
            ), cts.Token)
        let a = async {
                    use! holder = Async.OnCancel (fun _ -> ewh.Set() |> Assert.IsTrue)
                    let! v = t.AsyncValue
                    return v
            }
        Async.Start a
        cts.Cancel()
        try t.Wait() with _ -> ()
        ewh.WaitOne(1000) |> Assert.IsTrue        
#endif