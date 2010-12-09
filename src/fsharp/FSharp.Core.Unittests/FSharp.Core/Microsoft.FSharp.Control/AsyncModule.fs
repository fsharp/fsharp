// Various tests for the:
// Microsoft.FSharp.Control.Async module

namespace FSharp.Core.Unittests.FSharp_Core.Microsoft_FSharp_Control

open System
open FSharp.Core.Unittests.LibraryTestFx
open NUnit.Framework


// ---------------------------------------------------

[<TestFixture>]
type AsyncModule() =
    
    /// Simple asynchronous task that delays 200ms and returns a list of the current tick count
    let getTicksTask =
        async {
            do! Async.SwitchToThreadPool()
            let tickstamps = ref [] // like timestamps but for ticks :)
            
            for i = 1 to 10 do
                tickstamps := DateTime.Now.Ticks :: !tickstamps
                do! Async.Sleep(20)
                
            return !tickstamps
        }

    [<Test>]
    member this.AwaitIAsyncResult() =

        let beginOp, endOp, cancelOp = Async.AsBeginEnd(fun() -> getTicksTask)

        // Begin the async operation and wait
        let operationIAR = beginOp ((), new AsyncCallback(fun iar -> ()), null)
        match Async.AwaitIAsyncResult(operationIAR) |> Async.RunSynchronously with
        | true  -> ()
        | false -> Assert.Fail("Timed out. Expected to succeed.")

        // When the operation has already completed
        let operationIAR = beginOp ((), new AsyncCallback(fun iar -> ()), null)
        System.Threading.Thread.Sleep(250)
        
        let result = Async.AwaitIAsyncResult(operationIAR) |> Async.RunSynchronously        
        match result with
        | true  -> ()
        | false -> Assert.Fail("Timed out. Expected to succeed.")

        // Now with a timeout
        let operationIAR = beginOp ((), new AsyncCallback(fun iar -> ()), null)
        let result = Async.AwaitIAsyncResult(operationIAR, 1) |> Async.RunSynchronously
        match result with
        | true  -> Assert.Fail("Expected timeout")
        | false -> ()

        ()
        
