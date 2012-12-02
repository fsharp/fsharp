﻿
namespace FSharp.Core.Unittests.FSharp_Core.Microsoft_FSharp_Collections

open System
open NUnit.Framework

open FSharp.Core.Unittests.LibraryTestFx

[<TestFixture>]
type SeqModule2() =

    [<Test>]
    member this.Hd() =
             
        let IntSeq =
            seq { for i in 0 .. 9 do
                    yield i }
                    
        if Seq.head IntSeq <> 0 then Assert.Fail()
                 
        // string Seq
        let strSeq = seq ["first"; "second";  "third"]
        if Seq.head strSeq <> "first" then Assert.Fail()
         
        // Empty Seq
        let emptySeq = Seq.empty
        CheckThrowsArgumentException ( fun() -> Seq.head emptySeq)
      
        // null Seq
        let nullSeq:seq<'a> = null
        CheckThrowsArgumentNullException (fun () ->Seq.head nullSeq) 
        () 
        
        
    [<Test>]
    member this.Last() =
             
        let IntSeq =
            seq { for i in 0 .. 9 do
                    yield i }
                    
        if Seq.last IntSeq <> 9 then Assert.Fail()
                 
        // string Seq
        let strSeq = seq ["first"; "second";  "third"]
        if Seq.last strSeq <> "third" then Assert.Fail()
         
        // Empty Seq
        let emptySeq = Seq.empty
        CheckThrowsArgumentException ( fun() -> Seq.last emptySeq)
      
        // null Seq
        let nullSeq:seq<'a> = null
        CheckThrowsArgumentNullException (fun () ->Seq.last nullSeq) 
        () 
        
    [<Test>]
    member this.ExactlyOne() =
             
        let IntSeq =
            seq { for i in 7 .. 7 do
                    yield i }
                    
        if Seq.exactlyOne IntSeq <> 7 then Assert.Fail()
                 
        // string Seq
        let strSeq = seq ["second"]
        if Seq.exactlyOne strSeq <> "second" then Assert.Fail()
         
        // Empty Seq
        let emptySeq = Seq.empty
        CheckThrowsArgumentException ( fun() -> Seq.exactlyOne emptySeq)
      
        // non-singleton Seq
        let emptySeq = Seq.empty
        CheckThrowsArgumentException ( fun() -> Seq.exactlyOne [ 0 .. 1 ] |> ignore )
      
        // null Seq
        let nullSeq:seq<'a> = null
        CheckThrowsArgumentNullException (fun () ->Seq.exactlyOne nullSeq) 
        () 
        
                
    [<Test>]
    member this.Init() =

        let funcInt x = x
        let init_finiteInt = Seq.init 9 funcInt
        let expectedIntSeq = seq [ 0..8]
      
        VerifySeqsEqual expectedIntSeq  init_finiteInt
        
             
        // string Seq
        let funcStr x = x.ToString()
        let init_finiteStr = Seq.init 5  funcStr
        let expectedStrSeq = seq ["0";"1";"2";"3";"4"]

        VerifySeqsEqual expectedStrSeq init_finiteStr
        
        // null Seq
        let funcNull x = null
        let init_finiteNull = Seq.init 3 funcNull
        let expectedNullSeq = seq [ null;null;null]
        
        VerifySeqsEqual expectedNullSeq init_finiteNull
        () 
        
    [<Test>]
    member this.InitInfinite() =

        let funcInt x = x
        let init_infiniteInt = Seq.initInfinite funcInt
        let resultint = Seq.find (fun x -> x =100) init_infiniteInt
        
        Assert.AreEqual(100,resultint)
        
             
        // string Seq
        let funcStr x = x.ToString()
        let init_infiniteStr = Seq.initInfinite  funcStr
        let resultstr = Seq.find (fun x -> x = "100") init_infiniteStr
        
        Assert.AreEqual("100",resultstr)
       
       
    [<Test>]
    member this.IsEmpty() =
        
        //seq int
        let seqint = seq [1;2;3]
        let is_emptyInt = Seq.isEmpty seqint
        
        Assert.IsFalse(is_emptyInt)
              
        //seq str
        let seqStr = seq["first";"second"]
        let is_emptyStr = Seq.isEmpty  seqStr

        Assert.IsFalse(is_emptyInt)
        
        //seq empty
        let seqEmpty = Seq.empty
        let is_emptyEmpty = Seq.isEmpty  seqEmpty
        Assert.IsTrue(is_emptyEmpty) 
        
        //seq null
        let seqnull:seq<'a> = null
        CheckThrowsArgumentNullException (fun () -> Seq.isEmpty seqnull |> ignore)
        ()
        
    [<Test>]
    member this.Iter() =
        //seq int
        let seqint =  seq [ 1..3]
        let cacheint = ref 0
       
        let funcint x = cacheint := !cacheint + x
        Seq.iter funcint seqint
        Assert.AreEqual(6,!cacheint)
              
        //seq str
        let seqStr = seq ["first";"second"]
        let cachestr =ref ""
        let funcstr x = cachestr := !cachestr+x
        Seq.iter funcstr seqStr
         
        Assert.AreEqual("firstsecond",!cachestr)
        
         // empty array    
        let emptyseq = Seq.empty
        let resultEpt = ref 0
        Seq.iter (fun x -> Assert.Fail()) emptyseq   

        // null seqay
        let nullseq:seq<'a> =  null
        
        CheckThrowsArgumentNullException (fun () -> Seq.iter funcint nullseq |> ignore)  
        ()
        
    [<Test>]
    member this.Iter2() =
    
        //seq int
        let seqint =  seq [ 1..3]
        let cacheint = ref 0
       
        let funcint x y = cacheint := !cacheint + x+y
        Seq.iter2 funcint seqint seqint
        Assert.AreEqual(12,!cacheint)
              
        //seq str
        let seqStr = seq ["first";"second"]
        let cachestr =ref ""
        let funcstr x y = cachestr := !cachestr+x+y
        Seq.iter2 funcstr seqStr seqStr
         
        Assert.AreEqual("firstfirstsecondsecond",!cachestr)
        
         // empty array    
        let emptyseq = Seq.empty
        let resultEpt = ref 0
        Seq.iter2 (fun x y-> Assert.Fail()) emptyseq  emptyseq 

        // null seqay
        let nullseq:seq<'a> =  null
        CheckThrowsArgumentNullException (fun () -> Seq.iter2 funcint nullseq nullseq |> ignore)  
        
        ()
        
    [<Test>]
    member this.Iteri() =
    
        // seq int
        let seqint =  seq [ 1..10]
        let cacheint = ref 0
       
        let funcint x y = cacheint := !cacheint + x+y
        Seq.iteri funcint seqint
        Assert.AreEqual(100,!cacheint)
              
        // seq str
        let seqStr = seq ["first";"second"]
        let cachestr =ref 0
        let funcstr (x:int) (y:string) = cachestr := !cachestr+ x + y.Length
        Seq.iteri funcstr seqStr
         
        Assert.AreEqual(12,!cachestr)
        
         // empty array    
        let emptyseq = Seq.empty
        let resultEpt = ref 0
        Seq.iteri funcint emptyseq
        Assert.AreEqual(0,!resultEpt)

        // null seqay
        let nullseq:seq<'a> =  null
        CheckThrowsArgumentNullException (fun () -> Seq.iteri funcint nullseq |> ignore)  
        ()
        
    [<Test>]
    member this.Length() =

         // integer seq  
        let resultInt = Seq.length {1..8}
        if resultInt <> 8 then Assert.Fail()
        
        // string Seq    
        let resultStr = Seq.length (seq ["Lists"; "are";  "commonly" ; "list" ])
        if resultStr <> 4 then Assert.Fail()
        
        // empty Seq     
        let resultEpt = Seq.length Seq.empty
        if resultEpt <> 0 then Assert.Fail()

        // null Seq
        let nullSeq:seq<'a> = null     
        CheckThrowsArgumentNullException (fun () -> Seq.length  nullSeq |> ignore)  
        
        ()
        
    [<Test>]
    member this.Map() =

         // integer Seq
        let funcInt x = 
                match x with
                | _ when x % 2 = 0 -> 10*x            
                | _ -> x
       
        let resultInt = Seq.map funcInt { 1..10 }
        let expectedint = seq [1;20;3;40;5;60;7;80;9;100]
        
        VerifySeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (x:string) = x.ToLower()
        let resultStr = Seq.map funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        let expectedSeq = seq ["lists"; "are";  "commonly" ; "list"]
        
        VerifySeqsEqual expectedSeq resultStr
        
        // empty Seq
        let resultEpt = Seq.map funcInt Seq.empty
        VerifySeqsEqual Seq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
        CheckThrowsArgumentNullException (fun () -> Seq.map funcStr nullSeq |> ignore)
        
        ()
        
    [<Test>]
    member this.Map2() =
         // integer Seq
        let funcInt x y = x+y
        let resultInt = Seq.map2 funcInt { 1..10 } {2..2..20} 
        let expectedint = seq [3;6;9;12;15;18;21;24;27;30]
        
        VerifySeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (x:int) (y:string) = x+y.Length
        let resultStr = Seq.map2 funcStr (seq[3;6;9;11]) (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        let expectedSeq = seq [8;9;17;15]
        
        VerifySeqsEqual expectedSeq resultStr
        
        // empty Seq
        let resultEpt = Seq.map2 funcInt Seq.empty Seq.empty
        VerifySeqsEqual Seq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
        let validSeq = seq [1]
        CheckThrowsArgumentNullException (fun () -> Seq.map2 funcInt nullSeq validSeq |> ignore)
        
        ()
        
        
    member private this.MapWithSideEffectsTester (map : (int -> int) -> seq<int> -> seq<int>) expectExceptions =
        let i = ref 0
        let f x = i := !i + 1; x*x
        let e = ([1;2] |> map f).GetEnumerator()
        
        if expectExceptions then
            CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
            Assert.AreEqual(0, !i)
        if not (e.MoveNext()) then Assert.Fail()
        Assert.AreEqual(1, !i)
        let _ = e.Current
        Assert.AreEqual(1, !i)
        let _ = e.Current
        Assert.AreEqual(1, !i)
        
        if not (e.MoveNext()) then Assert.Fail()
        Assert.AreEqual(2, !i)
        let _ = e.Current
        Assert.AreEqual(2, !i)
        let _ = e.Current
        Assert.AreEqual(2, !i)

        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(2, !i)
        if expectExceptions then
            CheckThrowsInvalidOperationExn (fun _ -> e.Current |> ignore)
            Assert.AreEqual(2, !i)

        
        i := 0
        let e = ([] |> map f).GetEnumerator()
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(0,!i)
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(0,!i)
        
        
    member private this.MapWithExceptionTester (map : (int -> int) -> seq<int> -> seq<int>) =
        let raiser x = if x > 0 then raise(NotSupportedException()) else x
        let e = (map raiser [0; 1]).GetEnumerator()
        Assert.IsTrue(e.MoveNext()) // should not throw
        Assert.AreEqual(0, e.Current)
        CheckThrowsNotSupportedException(fun _ -> e.MoveNext() |> ignore)
        Assert.AreEqual(0, e.Current) // should not throw

    [<Test>]
    member this.MapWithSideEffects () =
        this.MapWithSideEffectsTester Seq.map true
        
    [<Test>]
    member this.MapWithException () =
        this.MapWithExceptionTester Seq.map

        
    [<Test>]
    member this.SingletonCollectWithSideEffects () =
        this.MapWithSideEffectsTester (fun f-> Seq.collect (f >> Seq.singleton)) true
        
    [<Test>]
    member this.SingletonCollectWithException () =
        this.MapWithExceptionTester (fun f-> Seq.collect (f >> Seq.singleton))

#if FX_NO_LINQ
#else     
    [<Test>]
    member this.SystemLinqSelectWithSideEffects () =
        this.MapWithSideEffectsTester (fun f s -> System.Linq.Enumerable.Select(s, Func<_,_>(f))) false
        
    [<Test>]
    member this.SystemLinqSelectWithException () =
        this.MapWithExceptionTester (fun f s -> System.Linq.Enumerable.Select(s, Func<_,_>(f)))
#endif
        
    [<Test>]
    member this.MapiWithSideEffects () =
        let i = ref 0
        let f _ x = i := !i + 1; x*x
        let e = ([1;2] |> Seq.mapi f).GetEnumerator()
        
        CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
        Assert.AreEqual(0, !i)
        if not (e.MoveNext()) then Assert.Fail()
        Assert.AreEqual(1, !i)
        let _ = e.Current
        Assert.AreEqual(1, !i)
        let _ = e.Current
        Assert.AreEqual(1, !i)
        
        if not (e.MoveNext()) then Assert.Fail()
        Assert.AreEqual(2, !i)
        let _ = e.Current
        Assert.AreEqual(2, !i)
        let _ = e.Current
        Assert.AreEqual(2, !i)
        
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(2, !i)
        CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
        Assert.AreEqual(2, !i)
        
        i := 0
        let e = ([] |> Seq.mapi f).GetEnumerator()
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(0,!i)
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(0,!i)
        
    [<Test>]
    member this.Map2WithSideEffects () =
        let i = ref 0
        let f x y = i := !i + 1; x*x
        let e = (Seq.map2 f [1;2] [1;2]).GetEnumerator()
        
        CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
        Assert.AreEqual(0, !i)
        if not (e.MoveNext()) then Assert.Fail()
        Assert.AreEqual(1, !i)
        let _ = e.Current
        Assert.AreEqual(1, !i)
        let _ = e.Current
        Assert.AreEqual(1, !i)
        
        if not (e.MoveNext()) then Assert.Fail()
        Assert.AreEqual(2, !i)
        let _ = e.Current
        Assert.AreEqual(2, !i)
        let _ = e.Current
        Assert.AreEqual(2, !i)

        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(2,!i)
        CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
        Assert.AreEqual(2, !i)
        
        i := 0
        let e = (Seq.map2 f [] []).GetEnumerator()
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(0,!i)
        if e.MoveNext() then Assert.Fail()
        Assert.AreEqual(0,!i)
        
    [<Test>]
    member this.Collect() =
         // integer Seq
        let funcInt x = seq [x+1]
        let resultInt = Seq.collect funcInt { 1..10 } 
       
        let expectedint = seq {2..11}
        
        VerifySeqsEqual expectedint resultInt

#if FX_NO_CHAR_PARSE
#else        
        // string Seq
        let funcStr (y:string) = y+"ist"
       
        let resultStr = Seq.collect funcStr (seq ["L"])
        
        
        let expectedSeq = seq ['L';'i';'s';'t']
        
        VerifySeqsEqual expectedSeq resultStr
#endif        
        // empty Seq
        let resultEpt = Seq.collect funcInt Seq.empty
        VerifySeqsEqual Seq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
       
        CheckThrowsArgumentNullException (fun () -> Seq.collect funcInt nullSeq |> ignore)
        
        ()
        
    [<Test>]
    member this.Mapi() =

         // integer Seq
        let funcInt x y = x+y
        let resultInt = Seq.mapi funcInt { 10..2..20 } 
        let expectedint = seq [10;13;16;19;22;25]
        
        VerifySeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (x:int) (y:string) =x+y.Length
       
        let resultStr = Seq.mapi funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        let expectedStr = seq [5;4;10;7]
         
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq
        let resultEpt = Seq.mapi funcInt Seq.empty
        VerifySeqsEqual Seq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
       
        CheckThrowsArgumentNullException (fun () -> Seq.mapi funcInt nullSeq |> ignore)
        
        ()
        
    [<Test>]
    member this.Max() =
         // integer Seq
        let resultInt = Seq.max { 10..20 } 
        
        Assert.AreEqual(20,resultInt)
        
        // string Seq
       
        let resultStr = Seq.max (seq ["Lists"; "Are";  "MaxString" ; "List" ])
        Assert.AreEqual("MaxString",resultStr)
          
        // empty Seq
        CheckThrowsArgumentException(fun () -> Seq.max ( Seq.empty : seq<float>) |> ignore)
        
        // null Seq
        let nullSeq:seq<'a> = null 
        CheckThrowsArgumentNullException (fun () -> Seq.max nullSeq |> ignore)
        
        ()
        
    [<Test>]
    member this.MaxBy() =
    
        // integer Seq
        let funcInt x = x % 8
        let resultInt = Seq.maxBy funcInt { 2..2..20 } 
        Assert.AreEqual(6,resultInt)
        
        // string Seq
        let funcStr (x:string)  =x.Length 
        let resultStr = Seq.maxBy funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        Assert.AreEqual("Commonly",resultStr)
          
        // empty Seq
        CheckThrowsArgumentException (fun () -> Seq.maxBy funcInt (Seq.empty : seq<int>) |> ignore)
        
        // null Seq
        let nullSeq:seq<'a> = null 
        CheckThrowsArgumentNullException (fun () ->Seq.maxBy funcInt nullSeq |> ignore)
        
        ()
        
    [<Test>]
    member this.MinBy() =
    
        // integer Seq
        let funcInt x = x % 8
        let resultInt = Seq.minBy funcInt { 2..2..20 } 
        Assert.AreEqual(8,resultInt)
        
        // string Seq
        let funcStr (x:string)  =x.Length 
        let resultStr = Seq.minBy funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        Assert.AreEqual("Are",resultStr)
          
        // empty Seq
        CheckThrowsArgumentException (fun () -> Seq.minBy funcInt (Seq.empty : seq<int>) |> ignore) 
        
        // null Seq
        let nullSeq:seq<'a> = null 
        CheckThrowsArgumentNullException (fun () ->Seq.minBy funcInt nullSeq |> ignore)
        
        ()
        
          
    [<Test>]
    member this.Min() =

         // integer Seq
        let resultInt = Seq.min { 10..20 } 
        Assert.AreEqual(10,resultInt)
        
        // string Seq
        let resultStr = Seq.min (seq ["Lists"; "Are";  "minString" ; "List" ])
        Assert.AreEqual("Are",resultStr)
          
        // empty Seq
        CheckThrowsArgumentException (fun () -> Seq.min (Seq.empty : seq<int>) |> ignore) 
        
        // null Seq
        let nullSeq:seq<'a> = null 
        CheckThrowsArgumentNullException (fun () -> Seq.min nullSeq |> ignore)
        
        ()
        
    [<Test>]
    member this.Nth() =
         
        // Negative index
        for i = -1 downto -10 do
           CheckThrowsArgumentException (fun () -> Seq.nth i { 10 .. 20 } |> ignore)
            
        // Out of range
        for i = 11 to 20 do
           CheckThrowsArgumentException (fun () -> Seq.nth i { 10 .. 20 } |> ignore)
         
         // integer Seq
        let resultInt = Seq.nth 3 { 10..20 } 
        Assert.AreEqual(13, resultInt)
        
        // string Seq
        let resultStr = Seq.nth 3 (seq ["Lists"; "Are";  "nthString" ; "List" ])
        Assert.AreEqual("List",resultStr)
          
        // empty Seq
        CheckThrowsArgumentException(fun () -> Seq.nth 0 (Seq.empty : seq<decimal>) |> ignore)
       
        // null Seq
        let nullSeq:seq<'a> = null 
        CheckThrowsArgumentNullException (fun () ->Seq.nth 3 nullSeq |> ignore)
        
        ()
         
    [<Test>]
    member this.Of_Array() =
         // integer Seq
        let resultInt = Seq.ofArray [|1..10|]
        let expectedInt = {1..10}
         
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr = Seq.ofArray [|"Lists"; "Are";  "ofArrayString" ; "List" |]
        let expectedStr = seq ["Lists"; "Are";  "ofArrayString" ; "List" ]
        VerifySeqsEqual expectedStr resultStr
          
        // empty Seq 
        let resultEpt = Seq.ofArray [| |] 
        VerifySeqsEqual resultEpt Seq.empty
       
        ()
        
    [<Test>]
    member this.Of_List() =
         // integer Seq
        let resultInt = Seq.ofList [1..10]
        let expectedInt = {1..10}
         
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
       
        let resultStr =Seq.ofList ["Lists"; "Are";  "ofListString" ; "List" ]
        let expectedStr = seq ["Lists"; "Are";  "ofListString" ; "List" ]
        VerifySeqsEqual expectedStr resultStr
          
        // empty Seq 
        let resultEpt = Seq.ofList [] 
        VerifySeqsEqual resultEpt Seq.empty
        ()
        
          
    [<Test>]
    member this.Pairwise() =
         // integer Seq
        let resultInt = Seq.pairwise {1..3}
       
        let expectedInt = seq [1,2;2,3]
         
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.pairwise ["str1"; "str2";"str3" ]
        let expectedStr = seq ["str1","str2";"str2","str3"]
        VerifySeqsEqual expectedStr resultStr
          
        // empty Seq 
        let resultEpt = Seq.pairwise [] 
        VerifySeqsEqual resultEpt Seq.empty
       
        ()
        
    [<Test>]
    member this.Reduce() =
         
        // integer Seq
        let resultInt = Seq.reduce (fun x y -> x/y) (seq [5*4*3*2; 4;3;2;1])
        Assert.AreEqual(5,resultInt)
        
        // string Seq
        let resultStr = Seq.reduce (fun (x:string) (y:string) -> x.Remove(0,y.Length)) (seq ["ABCDE";"A"; "B";  "C" ; "D" ])
        Assert.AreEqual("E",resultStr) 
       
        // empty Seq 
        CheckThrowsArgumentException (fun () -> Seq.reduce (fun x y -> x/y)  Seq.empty |> ignore)
        
        // null Seq
        let nullSeq : seq<'a> = null
        CheckThrowsArgumentNullException (fun () -> Seq.reduce (fun (x:string) (y:string) -> x.Remove(0,y.Length))  nullSeq  |> ignore)   
        ()

         
    [<Test>]
    member this.Scan() =
        // integer Seq
        let funcInt x y = x+y
        let resultInt = Seq.scan funcInt 9 {1..10}
        let expectedInt = seq [9;10;12;15;19;24;30;37;45;54;64]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr x y = x+y
        let resultStr =Seq.scan funcStr "x" ["str1"; "str2";"str3" ]
        
        let expectedStr = seq ["x";"xstr1"; "xstr1str2";"xstr1str2str3"]
        VerifySeqsEqual expectedStr resultStr
          
        // empty Seq 
        let resultEpt = Seq.scan funcInt 5 Seq.empty 
       
        VerifySeqsEqual resultEpt (seq [ 5])
       
        // null Seq
        let seqNull:seq<'a> = null
        CheckThrowsArgumentNullException(fun() -> Seq.scan funcInt 5 seqNull |> ignore)
        ()
        
    [<Test>]
    member this.Singleton() =
        // integer Seq
        let resultInt = Seq.singleton 1
       
        let expectedInt = seq [1]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.singleton "str1"
        let expectedStr = seq ["str1"]
        VerifySeqsEqual expectedStr resultStr
         
        // null Seq
        let resultNull = Seq.singleton null
        let expectedNull = seq [null]
        VerifySeqsEqual expectedNull resultNull
        ()
    
        
    [<Test>]
    member this.Skip() =
    
        // integer Seq
        let resultInt = Seq.skip 2 (seq [1;2;3;4])
        let expectedInt = seq [3;4]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.skip 2 (seq ["str1";"str2";"str3";"str4"])
        let expectedStr = seq ["str3";"str4"]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.skip 0 Seq.empty 
        VerifySeqsEqual resultEpt Seq.empty
        
         
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.skip 1 null |> ignore)
        ()
       
    [<Test>]
    member this.Skip_While() =
    
        // integer Seq
        let funcInt x = (x < 3)
        let resultInt = Seq.skipWhile funcInt (seq [1;2;3;4;5;6])
        let expectedInt = seq [3;4;5;6]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr (x:string) = x.Contains(".")
        let resultStr =Seq.skipWhile funcStr (seq [".";"asdfasdf.asdfasdf";"";"";"";"";"";"";"";"";""])
        let expectedStr = seq ["";"";"";"";"";"";"";"";""]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.skipWhile funcInt Seq.empty 
        VerifySeqsEqual resultEpt Seq.empty
        
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.skipWhile funcInt null |> ignore)
        ()
       
    [<Test>]
    member this.Sort() =

        // integer Seq
        let resultInt = Seq.sort (seq [1;3;2;4;6;5;7])
        let expectedInt = {1..7}
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
       
        let resultStr =Seq.sort (seq ["str1";"str3";"str2";"str4"])
        let expectedStr = seq ["str1";"str2";"str3";"str4"]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.sort Seq.empty 
        VerifySeqsEqual resultEpt Seq.empty
         
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.sort null  |> ignore)
        ()
        
    [<Test>]
    member this.SortBy() =

        // integer Seq
        let funcInt x = Math.Abs(x-5)
        let resultInt = Seq.sortBy funcInt (seq [1;2;4;5;7])
        let expectedInt = seq [5;4;7;2;1]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr (x:string) = x.IndexOf("key")
        let resultStr =Seq.sortBy funcStr (seq ["st(key)r";"str(key)";"s(key)tr";"(key)str"])
        
        let expectedStr = seq ["(key)str";"s(key)tr";"st(key)r";"str(key)"]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.sortBy funcInt Seq.empty 
        VerifySeqsEqual resultEpt Seq.empty
         
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.sortBy funcInt null  |> ignore)
        ()
        
    [<Test>]
    member this.Sum() =
    
        // integer Seq
        let resultInt = Seq.sum (seq [1..10])
        Assert.AreEqual(55,resultInt)
        
        // float32 Seq
        let floatSeq = (seq [ 1.2f;3.5f;6.7f ])
        let resultFloat = Seq.sum floatSeq
        if resultFloat <> 11.4f then Assert.Fail()
        
        // double Seq
        let doubleSeq = (seq [ 1.0;8.0 ])
        let resultDouble = Seq.sum doubleSeq
        if resultDouble <> 9.0 then Assert.Fail()
        
        // decimal Seq
        let decimalSeq = (seq [ 0M;19M;19.03M ])
        let resultDecimal = Seq.sum decimalSeq
        if resultDecimal <> 38.03M then Assert.Fail()      
          
      
        // empty float32 Seq
        let emptyFloatSeq = Seq.empty<System.Single> 
        let resultEptFloat = Seq.sum emptyFloatSeq 
        if resultEptFloat <> 0.0f then Assert.Fail()
        
        // empty double Seq
        let emptyDoubleSeq = Seq.empty<System.Double> 
        let resultDouEmp = Seq.sum emptyDoubleSeq 
        if resultDouEmp <> 0.0 then Assert.Fail()
        
        // empty decimal Seq
        let emptyDecimalSeq = Seq.empty<System.Decimal> 
        let resultDecEmp = Seq.sum emptyDecimalSeq 
        if resultDecEmp <> 0M then Assert.Fail()
       
        ()
        
    [<Test>]
    member this.SumBy() =

        // integer Seq
        let resultInt = Seq.sumBy int (seq [1..10])
        Assert.AreEqual(55,resultInt)
        
        // float32 Seq
        let floatSeq = (seq [ 1.2f;3.5f;6.7f ])
        let resultFloat = Seq.sumBy float32 floatSeq
        if resultFloat <> 11.4f then Assert.Fail()
        
        // double Seq
        let doubleSeq = (seq [ 1.0;8.0 ])
        let resultDouble = Seq.sumBy double doubleSeq
        if resultDouble <> 9.0 then Assert.Fail()
        
        // decimal Seq
        let decimalSeq = (seq [ 0M;19M;19.03M ])
        let resultDecimal = Seq.sumBy decimal decimalSeq
        if resultDecimal <> 38.03M then Assert.Fail()      

        // empty float32 Seq
        let emptyFloatSeq = Seq.empty<System.Single> 
        let resultEptFloat = Seq.sumBy float32 emptyFloatSeq 
        if resultEptFloat <> 0.0f then Assert.Fail()
        
        // empty double Seq
        let emptyDoubleSeq = Seq.empty<System.Double> 
        let resultDouEmp = Seq.sumBy double emptyDoubleSeq 
        if resultDouEmp <> 0.0 then Assert.Fail()
        
        // empty decimal Seq
        let emptyDecimalSeq = Seq.empty<System.Decimal> 
        let resultDecEmp = Seq.sumBy decimal emptyDecimalSeq 
        if resultDecEmp <> 0M then Assert.Fail()
       
        ()
        
    [<Test>]
    member this.Take() =
        // integer Seq
        
        let resultInt = Seq.take 3 (seq [1;2;4;5;7])
       
        let expectedInt = seq [1;2;4]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
       
        let resultStr =Seq.take 2(seq ["str1";"str2";"str3";"str4"])
     
        let expectedStr = seq ["str1";"str2"]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.take 0 Seq.empty 
      
        VerifySeqsEqual resultEpt Seq.empty
        
         
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.take 1 null |> ignore)
        ()
        
    [<Test>]
    member this.takeWhile() =
        // integer Seq
        let funcInt x = (x < 6)
        let resultInt = Seq.takeWhile funcInt (seq [1;2;4;5;6;7])
      
        let expectedInt = seq [1;2;4;5]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr (x:string) = (x.Length < 4)
        let resultStr =Seq.takeWhile funcStr (seq ["a"; "ab"; "abc"; "abcd"; "abcde"])
      
        let expectedStr = seq ["a"; "ab"; "abc"]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.takeWhile funcInt Seq.empty 
        VerifySeqsEqual resultEpt Seq.empty
        
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.takeWhile funcInt null |> ignore)
        ()
        
    [<Test>]
    member this.To_Array() =
        // integer Seq
        let resultInt = Seq.toArray(seq [1;2;4;5;7])
     
        let expectedInt = [|1;2;4;5;7|]
        Assert.AreEqual(expectedInt,resultInt)
        
        // string Seq
        let resultStr =Seq.toArray (seq ["str1";"str2";"str3"])
    
        let expectedStr =  [|"str1";"str2";"str3"|]
        Assert.AreEqual(expectedStr,resultStr)
        
        // empty Seq 
        let resultEpt = Seq.toArray Seq.empty 
        Assert.AreEqual([||],resultEpt)
        
         
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.toArray null |> ignore)
        ()
    
    [<Test>]
    member this.To_List() =
        // integer Seq
        let resultInt = Seq.toList (seq [1;2;4;5;7])
        let expectedInt = [1;2;4;5;7]
        Assert.AreEqual(expectedInt,resultInt)
        
        // string Seq
        let resultStr =Seq.toList (seq ["str1";"str2";"str3"])
        let expectedStr =  ["str1";"str2";"str3"]
        Assert.AreEqual(expectedStr,resultStr)
        
        // empty Seq 
        let resultEpt = Seq.toList Seq.empty 
        Assert.AreEqual([],resultEpt)
         
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.toList null |> ignore)
        ()
        
    [<Test>]
    member this.Truncate() =
        // integer Seq
        let resultInt = Seq.truncate 3 (seq [1;2;4;5;7])
        let expectedInt = [1;2;4]
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.truncate 2 (seq ["str1";"str2";"str3"])
        let expectedStr =  ["str1";"str2"]
        VerifySeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = Seq.truncate 0 Seq.empty
        VerifySeqsEqual Seq.empty resultEpt
        
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.truncate 1 null |> ignore)
        ()
        
    [<Test>]
    member this.tryFind() =
        // integer Seq
        let resultInt = Seq.tryFind (fun x -> (x%2=0)) (seq [1;2;4;5;7])
        Assert.AreEqual(Some(2), resultInt)
        
         // integer Seq - None
        let resultInt = Seq.tryFind (fun x -> (x%2=0)) (seq [1;3;5;7])
        Assert.AreEqual(None, resultInt)
        
        // string Seq
        let resultStr = Seq.tryFind (fun (x:string) -> x.Contains("2")) (seq ["str1";"str2";"str3"])
        Assert.AreEqual(Some("str2"),resultStr)
        
         // string Seq - None
        let resultStr = Seq.tryFind (fun (x:string) -> x.Contains("2")) (seq ["str1";"str4";"str3"])
        Assert.AreEqual(None,resultStr)
       
        
        // empty Seq 
        let resultEpt = Seq.tryFind (fun x -> (x%2=0)) Seq.empty
        Assert.AreEqual(None,resultEpt)

        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.tryFind (fun x -> (x%2=0))  null |> ignore)
        ()
        
    [<Test>]
    member this.TryFindIndex() =

        // integer Seq
        let resultInt = Seq.tryFindIndex (fun x -> (x % 5 = 0)) [8; 9; 10]
        Assert.AreEqual(Some(2), resultInt)
        
         // integer Seq - None
        let resultInt = Seq.tryFindIndex (fun x -> (x % 5 = 0)) [9;3;11]
        Assert.AreEqual(None, resultInt)
        
        // string Seq
        let resultStr = Seq.tryFindIndex (fun (x:string) -> x.Contains("2")) ["str1"; "str2"; "str3"]
        Assert.AreEqual(Some(1),resultStr)
        
         // string Seq - None
        let resultStr = Seq.tryFindIndex (fun (x:string) -> x.Contains("2")) ["str1"; "str4"; "str3"]
        Assert.AreEqual(None,resultStr)
       
        
        // empty Seq 
        let resultEpt = Seq.tryFindIndex (fun x -> (x%2=0)) Seq.empty
        Assert.AreEqual(None, resultEpt)
        
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.tryFindIndex (fun x -> (x % 2 = 0))  null |> ignore)
        ()
        
    [<Test>]
    member this.Unfold() =
        // integer Seq
        
        let resultInt = Seq.unfold (fun x -> if x = 1 then Some(7,2) else  None) 1
        
        VerifySeqsEqual (seq [7]) resultInt
          
        // string Seq
        let resultStr =Seq.unfold (fun (x:string) -> if x.Contains("unfold") then Some("a","b") else None) "unfold"
        VerifySeqsEqual (seq ["a"]) resultStr
        ()
        
        
    [<Test>]
    member this.Windowed() =
        // integer Seq
        let resultInt = Seq.windowed 5 (seq [1..10])
        let expectedInt = 
            seq { for i in 1..6 do
                    yield [| i; i+1; i+2; i+3; i+4 |] }
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.windowed 2 (seq ["str1";"str2";"str3";"str4"])
        let expectedStr = seq [ [|"str1";"str2"|];[|"str2";"str3"|];[|"str3";"str4"|]]
        VerifySeqsEqual expectedStr resultStr
      
        // empty Seq 
        let resultEpt = Seq.windowed 2 Seq.empty
        VerifySeqsEqual Seq.empty resultEpt
          
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.windowed 2 null |> ignore)
        ()
        
    [<Test>]
    member this.Zip() =
    
        // integer Seq
        let resultInt = Seq.zip (seq [1..7]) (seq [11..17])
        let expectedInt = 
            seq { for i in 1..7 do
                    yield i, i+10 }
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.zip (seq ["str3";"str4"]) (seq ["str1";"str2"])
        let expectedStr = seq ["str3","str1";"str4","str2"]
        VerifySeqsEqual expectedStr resultStr
      
        // empty Seq 
        let resultEpt = Seq.zip Seq.empty Seq.empty
        VerifySeqsEqual Seq.empty resultEpt
          
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.zip null null |> ignore)
        CheckThrowsArgumentNullException(fun() -> Seq.zip null (seq [1..7]) |> ignore)
        CheckThrowsArgumentNullException(fun() -> Seq.zip (seq [1..7]) null |> ignore)
        ()
        
    [<Test>]
    member this.Zip3() =
        // integer Seq
        let resultInt = Seq.zip3 (seq [1..7]) (seq [11..17]) (seq [21..27])
        let expectedInt = 
            seq { for i in 1..7 do
                    yield i, (i + 10), (i + 20) }
        VerifySeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =Seq.zip3 (seq ["str1";"str2"]) (seq ["str11";"str12"]) (seq ["str21";"str22"])
        let expectedStr = seq ["str1","str11","str21";"str2","str12","str22" ]
        VerifySeqsEqual expectedStr resultStr
      
        // empty Seq 
        let resultEpt = Seq.zip3 Seq.empty Seq.empty Seq.empty
        VerifySeqsEqual Seq.empty resultEpt
          
        // null Seq
        CheckThrowsArgumentNullException(fun() -> Seq.zip3 null null null |> ignore)
        CheckThrowsArgumentNullException(fun() -> Seq.zip3 null (seq [1..7]) (seq [1..7]) |> ignore)
        CheckThrowsArgumentNullException(fun() -> Seq.zip3 (seq [1..7]) null (seq [1..7]) |> ignore)
        CheckThrowsArgumentNullException(fun() -> Seq.zip3 (seq [1..7]) (seq [1..7]) null |> ignore)
        ()
        
    [<Test>]
    member this.tryPick() =
         // integer Seq
        let resultInt = Seq.tryPick (fun x-> if x = 1 then Some("got") else None) (seq [1..5])
         
        Assert.AreEqual(Some("got"),resultInt)
        
        // string Seq
        let resultStr = Seq.tryPick (fun x-> if x = "Are" then Some("got") else None) (seq ["Lists"; "Are"])
        Assert.AreEqual(Some("got"),resultStr)
        
        // empty Seq   
        let resultEpt = Seq.tryPick (fun x-> if x = 1 then Some("got") else None) Seq.empty
        Assert.IsNull(resultEpt)
       
        // null Seq
        let nullSeq : seq<'a> = null 
        let funcNull x = Some(1)
        
        CheckThrowsArgumentNullException(fun () -> Seq.tryPick funcNull nullSeq |> ignore)
   
        ()