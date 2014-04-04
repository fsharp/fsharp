
// Various tests for the:
// Microsoft.FSharp.Collections.seq type

namespace FSharp.Core.Unittests.FSharp_Core.Microsoft_FSharp_Collections

open System
open System.Collections
open System.Collections.Generic
open FSharp.Core.Unittests.LibraryTestFx
open NUnit.Framework


[<TestFixture>]
type SeqType() =
    
    // Interfaces
    [<Test>]
    member this.IEnumerable() =
        
        // Legit IE
        let ie = seq { yield 1; yield 2; yield 3 } :> IEnumerable
        let enum = ie.GetEnumerator()
        
        let testStepping() =
            CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(true, enum.MoveNext())
            Assert.AreEqual(1, enum.Current)
            Assert.AreEqual(true, enum.MoveNext())
            Assert.AreEqual(2, enum.Current)
            Assert.AreEqual(true, enum.MoveNext())
            Assert.AreEqual(3, enum.Current)
            Assert.AreEqual(false, enum.MoveNext())
            CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
    
        CheckThrowsNotSupportedException(fun () -> enum.Reset() |> ignore)
        testStepping()
        CheckThrowsNotSupportedException(fun () -> enum.Reset() |> ignore)
    
        // Empty IE
        let ie = Seq.empty :> IEnumerable  // Note no type args
        let enum = ie.GetEnumerator()
        
        CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(false, enum.MoveNext())
        CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        CheckThrowsNotSupportedException(fun () -> enum.Reset() |> ignore)

    [<Test>]
    member this.IEnumerable_T() =
        
        // Legit IE
        let ie = seq { yield 'a'; yield 'b'; yield 'c'}
        let enum = ie.GetEnumerator()
        
        let testStepping() =
            CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
            Assert.AreEqual(true,   enum.MoveNext())
            Assert.AreEqual('a',    enum.Current)
            Assert.AreEqual(true,   enum.MoveNext())
            Assert.AreEqual('b',    enum.Current)
            Assert.AreEqual(true,   enum.MoveNext())
            Assert.AreEqual('c',    enum.Current)
            Assert.AreEqual(false, enum.MoveNext())
            CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        
        CheckThrowsNotSupportedException(fun () -> enum.Reset() |> ignore)
        testStepping()
        CheckThrowsNotSupportedException(fun () -> enum.Reset() |> ignore)
    
        // Empty IE
        let ie = Seq.empty :> IEnumerable<int>  // Note no type args
        let enum = ie.GetEnumerator()
        
        CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        Assert.AreEqual(false, enum.MoveNext())
        CheckThrowsInvalidOperationExn(fun () -> enum.Current |> ignore)
        CheckThrowsNotSupportedException(fun () -> enum.Reset() |> ignore)

