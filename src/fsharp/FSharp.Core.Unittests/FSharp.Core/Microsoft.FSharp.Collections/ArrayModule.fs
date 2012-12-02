
// Various tests for the:
// Microsoft.FSharp.Collections.Array module

namespace FSharp.Core.Unittests.FSharp_Core.Microsoft_FSharp_Collections

open System
open FSharp.Core.Unittests.LibraryTestFx
open NUnit.Framework

(*
[Test Strategy]
Make sure each method works on:
* Integer array (value type)
* String  array (reference type)
* Empty   array (0 elements)
* Null    array (null)
*)

[<TestFixture>]
type ArrayModule() =

    let rec IsNaN (x : obj) =
        match x with
        | :? float   as x -> Double.IsNaN(x)
        | :? float32 as x -> Single.IsNaN(x)
        | :? decimal as x -> Decimal.ToDouble(x) |> box |> IsNaN
        | _ -> failwith "Invalid input. Please provide a numeric type which could possibly be NaN"

    [<Test>]
    member this.Empty() =
        let emptyArray = Array.empty
        if Array.length emptyArray <> 0 then Assert.Fail()    
        
        let c : int[]   = Array.empty<int>
        Assert.IsTrue( (c = [| |]) )
        
        let d : string[] = Array.empty<string>
        Assert.IsTrue( (d = [| |]) )
        ()


    [<Test>]
    member this.Append() =
        // integer array
        let intArray = Array.append [| 1; 2 |] [| 3; 4 |]
        Assert.IsTrue( (intArray = [| 1; 2; 3; 4 |]) )
        
        // string array
        let strArray = Array.append [| "a"; "b" |] [| "C"; "D" |]
        Assert.IsTrue( (strArray = [| "a"; "b"; "C"; "D" |]) )

        // empty array
        let emptyArray : int[]  = [|   |]
        let singleArray : int[] = [| 1 |]
        
        let appEmptySingle = Array.append emptyArray singleArray
        let appSingleEmpty = Array.append singleArray emptyArray
        
        Assert.IsTrue( (appEmptySingle = [| 1 |]) )
        Assert.IsTrue( (appSingleEmpty = [| 1 |]) )
      
        // null array
        let nullArray = null:int[]
        let validArray = [| 1 |]
        CheckThrowsArgumentNullException (fun () -> Array.append validArray nullArray |> ignore)    
        CheckThrowsArgumentNullException (fun () -> Array.append nullArray validArray |> ignore)   

        ()

    [<Test>]
    member this.Average() =   
      
        // empty float32 array
        let emptyFloatArray = Array.empty<float32> 
        CheckThrowsArgumentException(fun () -> Array.average emptyFloatArray |> ignore)
        
        // empty double array
        let emptyDoubleArray = Array.empty<System.Double> 
        CheckThrowsArgumentException(fun () -> Array.average emptyDoubleArray |> ignore)
        
        // empty decimal array
        let emptyDecimalArray = Array.empty<System.Decimal> 
        CheckThrowsArgumentException (fun () -> Array.average emptyDecimalArray |>ignore )

        // float32 array
        let floatArray: float32[] = [| 1.2f; 3.5f; 6.7f |]
        let averageOfFloat = Array.average floatArray
        if averageOfFloat <> 3.8000000000000003f then Assert.Fail()
        
        // double array
        let doubleArray: System.Double[] = [| 1.0;8.0 |]
        let averageOfDouble = Array.average doubleArray
        if averageOfDouble <> 4.5 then Assert.Fail()
        
        // decimal array
        let decimalArray: decimal[] = [| 0M; 19M; 19.03M |]
        let averageOfDecimal = Array.average decimalArray
        if averageOfDecimal <> 12.676666666666666666666666667M then Assert.Fail()      
        
        // null array
        let nullArr = null : double[]    
        CheckThrowsArgumentNullException (fun () -> Array.average nullArr |> ignore) 

        ()
        
    [<Test>]
    member this.AverageBy() =  
    
        // empty double array   
        let emptyDouArray = Array.empty<System.Double>
        let funcd x = x + 6.7
        CheckThrowsArgumentException(fun () -> Array.averageBy funcd emptyDouArray |> ignore)
                
        // empty float32 array
        let emptyFloat32Array: float32[] = [||]
        let funcf x = x + 9.8f 
        CheckThrowsArgumentException(fun () -> Array.averageBy funcf emptyFloat32Array |> ignore)
        
        // empty decimal array
        let emptyDecimalArray = Array.empty<System.Decimal>
        let funcDecimal x = x + 9.8M 
        CheckThrowsArgumentException(fun () -> Array.averageBy funcDecimal emptyDecimalArray |> ignore)
        
        // float32 array
        let floatArray: float32[] = [| 1.2f;3.5f;6.7f |]      
        let averageOfFloat = Array.averageBy funcf floatArray
        if averageOfFloat <> 13.5999994f then Assert.Fail()
        
        // double array
        let doubleArray: System.Double[] = [| 1.0;8.0 |]
        let averageOfDouble = Array.averageBy funcd doubleArray
        if averageOfDouble <> 11.2 then Assert.Fail()
        
        // decimal array
        let decimalArray: decimal[] = [| 0M;19M;19.03M |]
        let averageOfDecimal = Array.averageBy funcDecimal decimalArray
        if averageOfDecimal <> 22.476666666666666666666666667M then Assert.Fail()     
        
        // null array
        let nullArr : double[] = null
        CheckThrowsArgumentNullException (fun () -> Array.averageBy funcd nullArr |> ignore) 
        
        ()
        
    [<Test>]
    member this.Blit() = 
        // int array   
        let intSrc = [| 1..10 |]
        let intDes:int[] = Array.zeroCreate 10 
        Array.blit intSrc 0 intDes 0 5
        if intDes.[4] <> 5 then Assert.Fail()
        if intDes.[5] <> 0 then Assert.Fail()
        
        // string array
        let strSrc = [| "a";"b";"c";"d";"e";"j"|]
        let strDes = Array.create 10 "w"
        Array.blit strSrc 1 strDes 2 3
        if strDes.[3] <> "c" || Array.get strDes 4 = "w" then Assert.Fail()
     
        // null array
        let nullArr = null:string[]
        CheckThrowsArgumentNullException (fun () -> Array.blit nullArr 1 strDes 2 3 |> ignore) 

        // bounds check
        CheckThrowsArgumentException (fun () -> Array.blit intSrc -1 intDes 1 3 |> ignore)
        CheckThrowsArgumentException (fun () -> Array.blit intSrc 1 intDes -1 3 |> ignore)
        CheckThrowsArgumentException (fun () -> Array.blit intSrc 1 intDes 1 -3 |> ignore)
        CheckThrowsArgumentException (fun () -> Array.blit intSrc 1 intDes 1 300 |> ignore)
        CheckThrowsArgumentException (fun () -> Array.blit intSrc 1 intDes 5 8 |> ignore)
        
        ()

      
    member private this.ChooseTester chooseInt chooseString = 
        // int array
        let intSrc:int [] = [| 1..100 |]    
        let funcInt x = if (x%5=0) then Some x else None       
        let intChoosed : int[] = chooseInt funcInt intSrc
        if intChoosed.[1] <> 10 then Assert.Fail()
        
        // string array
        let stringSrc: string [] = "Lists are a commonly used data structure. They are not mutable, i.e., you can't delete an element of a list � instead you create a new list with the element deleted. List values often share storage under the hood, i.e., a list value only allocate more memory when you actually execute construction operations.".Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        let funcString x = match x with
                           | "list"-> Some x
                           | "List" -> Some x
                           | _ -> None
        let strChoosed : string[]  = chooseString funcString stringSrc   
        if strChoosed.[1].ToLower() <> "list" then Assert.Fail()
        
        // empty array
        let emptySrc :int[] = [| |]
        let emptyChoosed = chooseInt funcInt emptySrc
        Assert.IsTrue( (emptyChoosed = [| |]) )

        // null array
        let nullArr = null:int[]    
        CheckThrowsArgumentNullException (fun () -> chooseInt funcInt nullArr |> ignore) 
        
        () 
      
    [<Test>]
    member this.Choose() = 
        this.ChooseTester Array.choose Array.choose

#if FX_NO_TPL_PARALLEL
#else
    [<Test>]
    member this.``Parallel.Choose`` () = 
        this.ChooseTester Array.Parallel.choose Array.Parallel.choose
#endif

    member private this.CollectTester collectInt collectString =
    
        // int array - checking ordering
        let intSrc  = [| 1..3 |]
        let func = fun i -> [| 1..i |]
        let result : int[] = collectInt func intSrc
        Assert.AreEqual ([| 1; 1; 2; 1; 2; 3 |], result)
        
        // string array
        let stringSrc = [| "foo"; "bar" |]
        let func = fun s -> [| s |]
        let result : string[] = collectString func stringSrc
        Assert.AreEqual(stringSrc, result)
        
        // empty array
        let emptyArray : string [] = [| |]
        let result = collectString func emptyArray
        Assert.AreEqual(emptyArray,result)
        
        // null array
        let nullArr = null:int[]
        CheckThrowsArgumentNullException (fun () -> collectInt func nullArr |> ignore)
        
        ()

    [<Test>]
    member this.Collect () =
        this.CollectTester Array.collect Array.collect
        
    [<Test>]
    member this.CollectWithSideEffects () =
        let stamp = ref 0
        let f x = stamp := !stamp + 1; [| x |]
        
        Array.collect f [| |] |> ignore
        Assert.AreEqual(0, !stamp)
        
        stamp := 0
        Array.collect f [|1;2;3|] |> ignore
        Assert.AreEqual(3,!stamp)
        
#if FX_NO_TPL_PARALLEL
#else
    [<Test>]
    member this.``Parallel.Collect`` () =
        this.CollectTester Array.Parallel.collect Array.Parallel.collect
#endif
        
    [<Test>]
    member this.Concat() =
        // integer array
        let seqInt = 
            seq { for i in 1..10 do                
                    yield [|i; i*10|] }
                    
        let conIntArr = Array.concat seqInt
        if Array.length conIntArr <> 20 then Assert.Fail()
        
        // string array
        let strSeq = 
            seq { for a in 'a'..'c' do
                    for b in 'a'..'c' do
                        yield [|a.ToString();b.ToString() |]}
     
        let conStrArr = Array.concat strSeq
        if Array.length conStrArr <> 18 then Assert.Fail()
        
        // Empty array
        let emptyArrays = [| [| |]; [| 0 |]; [| 1 |]; [| |]; [| |] |]
        let result2 = Array.concat emptyArrays
        Assert.IsTrue(result2.[0] = 0 && result2.[1] = 1)
        if result2.[0] <> 0 && result2.[1] <> 1 then Assert.Fail()    

        // null array
        let nullArray = null:int[]
        let nullArrays = Array.create 2 nullArray
        CheckThrowsNullRefException (fun () -> Array.concat nullArrays |> ignore) 
                
        () 
        

    [<Test>]
    member this.Copy() =
        // int array
        let intSrc:int [] = [| 3;5;7 |]    
        let intCopyed = Array.copy  intSrc
        if intCopyed <> [| 3;5;7 |] then Assert.Fail()
        
        // string array
        let stringSrc: string [] = [|"Lists"; "are";  "commonly"  |]
        
        let strCopyed = Array.copy  stringSrc   
        if strCopyed <> [|"Lists"; "are";  "commonly"  |] then Assert.Fail()
        
        // empty array
        let emptySrc :int[] = [| |]
        let emptyCopyed = Array.copy emptySrc
        if emptyCopyed <> [| |] then Assert.Fail()

        // null array
        let nullArr = null:int[]    
        CheckThrowsArgumentNullException (fun () -> Array.copy nullArr |> ignore) 
        
        ()

    [<Test>]
    member this.Create() =
        // int array
        let intArr = Array.create 3 8    
        if intArr <> [| 8;8;8 |] then Assert.Fail()
        
        // string array
        let strArr = Array.create 3 "good"
        Assert.IsTrue( (strArr = [|"good"; "good";  "good"|]) )
        
        // empty array
        let emptyArr = Array.create 0 "empty"    
        if emptyArr <> [| |] then Assert.Fail()

        // array with null elements
        let nullStr = null:string  
        let nullArr = Array.create 3 nullStr
        Assert.IsTrue( (nullArr = [|null; null; null|]) )
        
        ()
        
    [<Test>]
    member this.Exists() =
        // integer array
        let intArr = [| 2;4;6;8 |]
        let funcInt x = if (x%2 = 0) then true else false
        let resultInt = Array.exists funcInt intArr
        if resultInt <> true then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" |]
        let funcStr (x:string) = if (x.Length >15) then true else false
        let resultStr = Array.exists funcStr strArr
        if resultStr <> false then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        let resultEpt = Array.exists funcInt emptyArr
        if resultEpt <> false then Assert.Fail()

        // null array
        let nullArr = null:string[]      
        CheckThrowsArgumentNullException (fun () -> Array.exists funcStr nullArr |> ignore) 
        
        ()
        
    [<Test>]
    member this.Exists2() =
        // integer array
        let intFir = [| 2;4;6;8 |]
        let intSec = [| 1;2;3;4 |]
        let funcInt x y = if (x%y = 0) then true else false
        let resultInt = Array.exists2 funcInt intFir intSec
        if resultInt <> true then Assert.Fail()
        
        // string array
        let strFir = [|"Lists"; "are";  "commonly" |]
        let strSec = [|"good"; "good";  "good"  |]
        let funcStr (x:string) (y:string) = if (x = y) then true else false
        let resultStr = Array.exists2 funcStr strFir strSec
        if resultStr <> false then Assert.Fail()
        
        // empty array
        let eptFir:int[] = [| |]
        let eptSec:int[] = [| |]
        let resultEpt = Array.exists2 funcInt eptFir eptSec
        if resultEpt <> false then Assert.Fail()

        // null array
        let nullFir = null:string[] 
        let validArray = [| "a" |]      
        CheckThrowsArgumentNullException (fun () -> Array.exists2 funcStr nullFir validArray |> ignore)  
        CheckThrowsArgumentNullException (fun () -> Array.exists2 funcStr validArray nullFir |> ignore) 
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Array.exists2 funcInt [|1..10|] [|2..20|] |> ignore)
        
        ()

    [<Test>]
    member this.Fill() =
        // integer array
        let intArr = [|1..5|]
        Array.fill intArr 0 3 21
        if intArr <> [|21;21;21;4;5|] then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
        Array.fill strArr 1 5 "a"
        
        if strArr <> [|"Lists"; "a"; "a"; "a"; "a";"a" |] then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        Array.fill emptyArr 0 0 8
        if emptyArr <> [| |] then Assert.Fail()

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> Array.fill nullArr 0 1 "good" |> ignore)
        
        // start < 0
        CheckThrowsArgumentException(fun () -> Array.fill intArr -1 3 21)
        
        // len < 0        
        CheckThrowsArgumentException(fun () -> Array.fill intArr 1 -2 21)
        
         
        ()

    [<Test>] 
    member this.Filter() =
        // integer array
        let intArr = [| 1..20 |]
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Array.filter funcInt intArr
        if resultInt <> [|5;10;15;20|] then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
        let funcStr (x:string) = if (x.Length > 4) then true else false
        let resultStr = Array.filter funcStr strArr
        if resultStr <> [|"Lists";  "commonly"; "structor" |] then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        let resultEpt = Array.filter funcInt emptyArr
        if resultEpt <> [| |] then Assert.Fail()

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () ->  Array.filter funcStr nullArr |> ignore) 
        
        ()   

    [<Test>]
    member this.Find() =
        // integer array
        let intArr = [| 1..20 |]
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Array.find funcInt intArr
        if resultInt <> 5 then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
        let funcStr (x:string) = if (x.Length >7) then true else false
        let resultStr = Array.find funcStr strArr
        if resultStr <> "commonly" then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |] 
        CheckThrowsKeyNotFoundException (fun () -> Array.find (fun x -> true) emptyArr |> ignore)        

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> Array.find funcStr nullArr |> ignore) 
        
        () 

    [<Test>]
    member this.FindIndex() =
        // integer array
        let intArr = [| 1..20 |]
        let funcInt x = if (x%5 = 0) then true else false
        let resultInt = Array.findIndex funcInt intArr
        if resultInt <> 4 then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
        let funcStr (x:string) = if (x.Length >7) then true else false
        let resultStr = Array.findIndex funcStr strArr
        if resultStr <> 3 then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]  
        CheckThrowsKeyNotFoundException(fun() -> Array.findIndex (fun x -> true) emptyArr |> ignore) 
        

        // null array
        let nullArr = null:string[]  
        CheckThrowsArgumentNullException (fun () -> Array.findIndex funcStr nullArr |> ignore) 
        
        () 
        
    [<Test>]
    member this.Pick() =
        // integers
        let intArr = [| 1..10 |]
        let matchFunc n =
            if n = 3 then Some(n.ToString())
            else None
        let resultInt = Array.pick matchFunc intArr
        Assert.AreEqual("3", resultInt)
        
        // make it not found
        CheckThrowsKeyNotFoundException (fun () -> Array.pick (fun n -> None) intArr |> ignore)
        
    [<Test>]
    member this.ToSeq() =
        let intArr = [| 1..10 |]
        let seq = Array.toSeq intArr
        let sum = Seq.sum seq
        Assert.AreEqual(55, sum)
        
    [<Test>]
    member this.TryPick() =
        // integer array
        let intArr = [| 1..10 |]    
        let funcInt x = 
                match x with
                | _ when x % 3 = 0 -> Some (x.ToString())            
                | _ -> None
        let resultInt = Array.tryPick funcInt intArr
        if resultInt <> Some "3" then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]
        let funcStr x = 
                match x with
                | "good" -> Some (x.ToString())            
                | _ -> None
        let resultStr = Array.tryPick funcStr strArr
        if resultStr <> None then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        let resultEpt = Array.tryPick funcInt emptyArr
        if resultEpt <> None then Assert.Fail()

        // null array
        let nullArr = null:string[]  
        CheckThrowsArgumentNullException (fun () -> Array.tryPick funcStr nullArr |> ignore)  
        
        ()

    [<Test>]
    member this.Fold() =
        // integer array
        let intArr = [| 1..5 |]    
        let funcInt x y = x+"+"+y.ToString()
        let resultInt = Array.fold funcInt "x" intArr
        if resultInt <> "x+1+2+3+4+5" then Assert.Fail()
        
        // string array
        let strArr = [|"A"; "B";  "C" ; "D" |]
        let funcStr x y = x+y
            
        let resultStr = Array.fold funcStr "X" strArr
        if resultStr <> "XABCD" then Assert.Fail()
        
        // empty array
        let emptyArr : int[] = [| |]
        let resultEpt = Array.fold funcInt "x" emptyArr
        if resultEpt <> "x" then Assert.Fail()

        // null array
        let nullArr = null : string[] 
        CheckThrowsArgumentNullException (fun () -> Array.fold funcStr "begin" nullArr |> ignore)  
        
        ()

    [<Test>]
    member this.Fold2() =
        // integer array  
        let funcInt x y z = x + y.ToString() + z.ToString()
        let resultInt = Array.fold2 funcInt "x" [| 1;3;5 |]  [|2;4;6|]
        if resultInt <> "x123456" then Assert.Fail()
        
        // string array
        let funcStr x y z= x + y + z        
        let resultStr = Array.fold2 funcStr "X" [|"A"; "B";  "C" ; "D" |] [|"H"; "I";  "J" ; "K" |]
        if resultStr <> "XAHBICJDK" then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        let resultEpt = Array.fold2 funcInt "x" emptyArr emptyArr
        if resultEpt <> "x" then Assert.Fail()

        // null array
        let nullArr = null:string[]
        let validArray = [| "a" |]
        CheckThrowsArgumentNullException (fun () -> Array.fold2 funcStr "begin" validArray nullArr |> ignore)  
        CheckThrowsArgumentNullException (fun () -> Array.fold2 funcStr "begin" nullArr validArray |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Array.fold2 funcInt "x" [| 1;3;5 |]  [|2;4;6;8|] |> ignore)
                
        ()

    [<Test>]
    member this.FoldBack() =
        // integer array
        let intArr = [| 1..5 |]    
        let funcInt x y = x.ToString()+y
        let resultInt = Array.foldBack funcInt intArr "x"
        if resultInt <> "12345x" then Assert.Fail()
        
        // string array
        let strArr = [|"A"; "B";  "C" ; "D" |]
        let funcStr x y = x+y
            
        let resultStr = Array.foldBack funcStr strArr "X" 
        if resultStr <> "ABCDX" then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        let resultEpt = Array.foldBack funcInt emptyArr "x" 
        if resultEpt <> "x" then Assert.Fail()

        // null array
        let nullArr = null:string[]      
        CheckThrowsArgumentNullException (fun () -> Array.foldBack funcStr nullArr "begin" |> ignore)  
        
        ()

    [<Test>]
    member this.FoldBack2() =
        // integer array  
        let funcInt x y z = x.ToString() + y.ToString() + z
        let resultInt = Array.foldBack2 funcInt  [| 1;3;5 |]  [|2;4;6|] "x"
        if resultInt <> "123456x" then Assert.Fail()
        
        // string array
        let funcStr x y z= x + y + z        
        let resultStr = Array.foldBack2 funcStr [|"A"; "B";  "C" ; "D" |] [|"H"; "I";  "J" ; "K" |] "X"
        if resultStr <> "AHBICJDKX" then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        let resultEpt = Array.foldBack2 funcInt emptyArr emptyArr "x"
        if resultEpt <> "x" then Assert.Fail()

        // null array
        let nullArr = null : string[] 
        let validArray = [| "a" |] 
        CheckThrowsArgumentNullException (fun () -> Array.foldBack2 funcStr nullArr validArray "begin" |> ignore)  
        CheckThrowsArgumentNullException (fun () -> Array.foldBack2 funcStr validArray nullArr "begin" |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Array.foldBack2 funcInt [|1..10|] [|2..20|] "x" |> ignore)
        
        ()

    [<Test>]
    member this.ForAll() =
        // integer array
        let resultInt = Array.forall (fun x -> x > 2) [| 3..2..10 |]
        if resultInt <> true then Assert.Fail()
        
        // string array
        let resultStr = Array.forall (fun (x:string) -> x.Contains("a")) [|"Lists"; "are";  "commonly" ; "list" |]
        if resultStr <> false then Assert.Fail()
        
        // empty array 
        let resultEpt = Array.forall (fun (x:string) -> x.Contains("a")) [||] 
        if resultEpt <> true then Assert.Fail()

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> Array.forall (fun x -> true) nullArr |> ignore)  
        
        ()
        
    [<Test>]
    member this.ForAll2() =
        // integer array
        let resultInt = Array.forall2 (fun x y -> x < y) [| 1..10 |] [|2..2..20|]
        if resultInt <> true then Assert.Fail()
        
        // string array
        let resultStr = Array.forall2 (fun (x:string) (y:string) -> x.Length < y.Length) [|"Lists"; "are";  "commonly" ; "list" |] [|"Listslong"; "arelong";  "commonlylong" ; "listlong" |]
        if resultStr <> true then Assert.Fail()
        
        // empty array 
        let resultEpt = Array.forall2 (fun x y -> x>y) [||] [||]
        if resultEpt <> true then Assert.Fail()

        // null array
        let nullArr = null:string[]
        let validArray = [| "a" |] 
        CheckThrowsArgumentNullException (fun () -> Array.forall2 (fun x y-> true) nullArr validArray |> ignore)  
        CheckThrowsArgumentNullException (fun () -> Array.forall2 (fun x y-> true) validArray nullArr |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Array.forall2 (fun x y -> x < y) [|1..10|] [|2..20|] |> ignore)
        
        ()
        
    [<Test>]
    member this.Get() =
        // integer array
        let intArr = [| 3;4;7;8;10 |]    
        let resultInt = Array.get intArr 3
        if resultInt <> 8 then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]
        
        let resultStr = Array.get strArr 2
        if resultStr <> "commonly" then Assert.Fail()
        
        // empty array
        let emptyArr:int[] = [| |]
        CheckThrowsIndexOutRangException (fun () -> Array.get emptyArr -1 |> ignore)

        // null array
        let nullArr = null:string[] 
        CheckThrowsNullRefException (fun () -> Array.get nullArr 0 |> ignore)  
        
        ()

    member private this.InitTester initInt initString = 
        // integer array
        let resultInt : int[] = initInt 3 (fun x -> x + 3) 
        if resultInt <> [|3;4;5|] then Assert.Fail()
        
        // string array
        let funStr (x:int) = 
            match x with
            | 0 -> "Lists"
            | 1 -> "are"
            | 2 -> "commonly"
            | _ -> "end"    
        let resultStr = initString 3 funStr
        if resultStr <> [|"Lists"; "are";  "commonly"  |] then Assert.Fail()
        
        // empty array  
        let resultEpt = initInt 0 (fun x -> x+1)
        if resultEpt <> [| |] then Assert.Fail()
        
        ()

    [<Test>]
    member this.Init() = 
        this.InitTester Array.init Array.init
        
    [<Test>]
    member this.InitWithSideEffects () =
        let stamp = ref 0
        let f i = 
            stamp := !stamp + 1; 
            i 
        Array.init 0 f |> ignore
        Assert.AreEqual (0, !stamp)
        
        stamp := 0
        Array.init 10 f |> ignore
        Assert.AreEqual (10, !stamp)
        
#if FX_NO_TPL_PARALLEL
#else
    [<Test>]
    member this.``Parallel.Init``() = 
        this.InitTester Array.Parallel.init Array.Parallel.init
#endif

    [<Test>]
    member this.IsEmpty() =
        // integer array
        let intArr = [| 3;4;7;8;10 |]    
        let resultInt = Array.isEmpty intArr 
        if resultInt <> false then Assert.Fail()
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]    
        let resultStr = Array.isEmpty strArr 
        if resultStr <> false then Assert.Fail()
        
        // empty array    
        let emptyArr:int[] = [| |]
        let resultEpt = Array.isEmpty emptyArr 
        if resultEpt <> true then Assert.Fail()

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> Array.isEmpty nullArr |> ignore)  
        
        ()

    [<Test>]
    member this.Iter() =
        // integer array
        let intArr = [| 1..10 |]  
        let resultInt = ref 0    
        let funInt (x:int) =   
            resultInt := !resultInt + x              
            () 
        Array.iter funInt intArr 
        if !resultInt <> 55 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]
        let resultStr = ref ""
        let funStr (x : string) =
            resultStr := (!resultStr) + x   
            ()
        Array.iter funStr strArr  
        if !resultStr <> "Listsarecommonlylist" then Assert.Fail()   
        
        // empty array    
        let emptyArr : int[] = [| |]
        let resultEpt = ref 0
        Array.iter funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        let nullArr = null : string[]  
        CheckThrowsArgumentNullException (fun () -> Array.iter funStr nullArr |> ignore)  
        
        ()
       
    [<Test>]
    member this.Iter2() =
        // integer array
        let resultInt = ref 0    
        let funInt (x:int) (y:int) =   
            resultInt := !resultInt + x + y             
            () 
        Array.iter2 funInt [| 1..10 |] [|2..2..20|] 
        if !resultInt <> 165 then Assert.Fail()    
        
        // string array
        let resultStr = ref ""
        let funStr (x:string) (y:string) =
            resultStr := (!resultStr) + x  + y 
            ()
        Array.iter2 funStr [|"A"; "B";  "C" ; "D" |] [|"a"; "b"; "c"; "d"|]  
        if !resultStr <> "AaBbCcDd" then Assert.Fail()   
        
        // empty array    
        let emptyArr:int[] = [| |]
        let resultEpt = ref 0
        Array.iter2 funInt emptyArr emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        let nullArr = null:string[]  
        let validArray = [| "a" |]     
        CheckThrowsArgumentNullException (fun () -> Array.iter2 funStr nullArr validArray |> ignore)  
        CheckThrowsArgumentNullException (fun () -> Array.iter2 funStr validArray nullArr |> ignore)  
        
        // len1 <> len2        
        CheckThrowsArgumentException(fun () -> Array.iter2 funInt [| 1..10 |] [|2..20|])
  
        ()
        
        
    [<Test>]
    member this.Iteri() =
        // integer array
        let intArr = [| 1..10 |]  
        let resultInt = ref 0    
        let funInt (x:int) y =   
            resultInt := !resultInt + x + y             
            () 
        Array.iteri funInt intArr 
        if !resultInt <> 100 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]
        let resultStr = ref 0
        let funStr (x:int) (y:string) =
            resultStr := (!resultStr) + x + y.Length
            ()
        Array.iteri funStr strArr  
        if !resultStr <> 26 then Assert.Fail()   
        
        // empty array    
        let emptyArr:int[] = [| |]
        let resultEpt = ref 0
        Array.iteri funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> Array.iteri funStr nullArr |> ignore)  
        
        ()
        
    [<Test>]
    member this.Iteri2() =
        // integer array
        let resultInt = ref 0    
        let funInt (x:int) (y:int) (z:int) =   
            resultInt := !resultInt + x + y + z            
            () 
        Array.iteri2 funInt [| 1..10 |] [|2..2..20|] 
        if !resultInt <> 210 then Assert.Fail()    
        
        // string array
        let resultStr = ref ""
        let funStr (x:int) (y:string) (z:string) =
            resultStr := (!resultStr) + x.ToString()  + y + z
            ()
        Array.iteri2 funStr [|"A"; "B";  "C" ; "D" |] [|"a"; "b"; "c"; "d"|]  
        if !resultStr <> "0Aa1Bb2Cc3Dd" then Assert.Fail()   
        
        // empty array    
        let emptyArr:int[] = [| |]
        let resultEpt = ref 0
        Array.iteri2 funInt emptyArr emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        let nullArr = null:string[]
        let validArray = [| "a" |] 
        CheckThrowsArgumentNullException (fun () -> Array.iteri2 funStr nullArr validArray |> ignore)  
        CheckThrowsArgumentNullException (fun () -> Array.iteri2 funStr validArray nullArr |> ignore)  
        
        // len1 <> len2
        CheckThrowsArgumentException(fun () -> Array.iteri2 funInt [| 1..10 |] [|2..20|]  |> ignore)
        
        ()                

    member private this.MapTester mapInt (mapString : (string -> int) -> array<string> -> array<int>) =
        // empty array 
        let f x = x + 1
        let result = mapInt f [| |]
        if result <> [| |] then Assert.Fail ()
        
        // int array
        let result = mapInt f [| 1..100 |]
        if result <> [| 2..101 |] then Assert.Fail ()
        
        // string array
        let result = [| "a"; "aa"; "aaa" |] |> mapString (fun s -> s.Length) 
        if result <> [| 1..3 |] then Assert.Fail ()
        
        // null array
        let nullArg : int [] = null
        CheckThrowsArgumentNullException (fun () -> mapInt f nullArg |> ignore)
        
        ()
        
    [<Test>]  
    member this.Map () =
        this.MapTester Array.map Array.map
        
    [<Test>]
    member this.MapWithSideEffects () =
        let stamp = ref 0
        let f x = stamp := !stamp + 1; x + 1
        
        Array.map f [| |] |> ignore
        Assert.AreEqual(0,!stamp)
        
        stamp := 0
        Array.map f [| 1..100 |] |> ignore
        Assert.AreEqual(100,!stamp)
        
#if FX_NO_TPL_PARALLEL
#else
    [<Test>]
    member this.``Parallel.Map`` () =
        this.MapTester Array.Parallel.map Array.Parallel.map
#endif

    member private this.MapiTester mapiInt mapiString =
        // empty array 
        let f i x = (i, x + 1)
        let result = mapiInt f [| |]
        if result <> [| |] then Assert.Fail ()
        
        // int array
        let result : array<int*int> = mapiInt f [| 1..2 |]
        if result <> [| (0,2); (1,3) |] then Assert.Fail ()
        
        // string array
        let result : array<int*int> = [| "a"; "aa"; "aaa" |] |> mapiString (fun i (s:string) -> i, s.Length) 
        if result <> [| (0,1); (1,2); (2,3) |] then Assert.Fail ()
        
        // null array
        let nullArg : int [] = null
        CheckThrowsArgumentNullException (fun () -> mapiInt f nullArg |> ignore)        
        ()

    [<Test>]
    member this.Mapi () = this.MapiTester Array.mapi Array.mapi
        

    [<Test>]
    member this.MapiWithSideEffects () =
        let stamp = ref 0
        let f i x = stamp := !stamp + 1; (i, x + 1)
       
        Array.mapi f [| |] |> ignore
        Assert.AreEqual(0,!stamp)
       
        stamp := 0
        Array.mapi f [| 1..100 |] |> ignore
        Assert.AreEqual(100,!stamp)
        ()
        
#if FX_NO_TPL_PARALLEL
#else
    [<Test>]
    member this.``Parallel.Mapi`` () =
        this.MapiTester Array.Parallel.mapi Array.Parallel.mapi
        ()
        
    [<Test>]
    member this.``Parallel.Iter``() =
        // integer array
        let intArr = [| 1..10 |]  
        let resultInt = ref 0    
        let funInt (x:int) =   
            lock resultInt (fun () -> resultInt := !resultInt + x)
            () 
        Array.Parallel.iter funInt intArr 
        if !resultInt <> 55 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]
        let resultStr = ref 0
        let funStr (x : string) =
            lock resultStr (fun () -> resultStr := (!resultStr) + x.Length)
            ()
        Array.Parallel.iter funStr strArr  
        if !resultStr <> 20 then Assert.Fail()   
        
        // empty array    
        let emptyArr : int[] = [| |]
        let resultEpt = ref 0
        Array.Parallel.iter funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        let nullArr = null : string[]  
        CheckThrowsArgumentNullException (fun () -> Array.Parallel.iter funStr nullArr |> ignore)  
        
        ()
        
    [<Test>]
    member this.``Parallel.Iteri``() =   
        // integer array
        let intArr = [| 1..10 |] 
                 
        let resultInt = ref 0    
        let funInt (x:int) y =   
            lock resultInt (fun () -> resultInt := !resultInt + x + y)
            () 
        Array.Parallel.iteri funInt intArr 
        if !resultInt <> 100 then Assert.Fail()    
        
        // string array
        let strArr = [|"Lists"; "are";  "commonly" ; "list" |]
        let resultStr = ref 0
        let funStr (x:int) (y:string) =
            lock resultStr (fun () -> resultStr := (!resultStr) + x + y.Length)
            ()
        Array.Parallel.iteri funStr strArr  
        if !resultStr <> 26 then Assert.Fail()   
        
        // empty array    
        let emptyArr:int[] = [| |]
        let resultEpt = ref 0
        Array.Parallel.iteri funInt emptyArr 
        if !resultEpt <> 0 then Assert.Fail()    

        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> Array.Parallel.iteri funStr nullArr |> ignore)  
        
        ()
#endif
    
    member private this.PartitionTester partInt partString =
        // int array
        let intSrc:int [] = [| 1..100 |]    
        let funcInt x = if (x%2=1) then true else false
        let intPartitioned : int[] * int[] = partInt funcInt intSrc
        if ([|1..2..100|],[|2..2..100|]) <> intPartitioned then Assert.Fail ()
        
        let allLeft = partInt (fun _ -> true) intSrc
        if (intSrc, [||]) <> allLeft then Assert.Fail()
        let allRight = partInt (fun _ -> false) intSrc
        if ([||], intSrc) <> allRight then Assert.Fail()

        
        // string array
        let stringSrc: string [] = "List 1 list 2 3 4 5".Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        let funcString x = match x with
                           | "list"-> true
                           | "List" -> true
                           | _ -> false
        let strPartitioned : string[] * string[]  = partString funcString stringSrc   
        if strPartitioned <> ([|"List";"list"|], [| "1";"2"; "3"; "4"; "5"|]) then Assert.Fail ()
        
        // empty array
        let emptySrc :int[] = [| |]
        let emptyPartitioned = partInt funcInt emptySrc
        if emptyPartitioned <> ([| |], [| |]) then Assert.Fail()
        
        // null array
        let nullArr = null:string[] 
        CheckThrowsArgumentNullException (fun () -> partString funcString nullArr |> ignore)
        
        
    [<Test>]
    member this.Partition () =
        this.PartitionTester Array.partition Array.partition    

#if FX_NO_TPL_PARALLEL
#else
    [<Test>]
    member this.``Parallel.Partition`` () =
        this.PartitionTester Array.Parallel.partition Array.Parallel.partition    
#endif    
