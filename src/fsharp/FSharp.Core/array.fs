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

    open System
    open System.Diagnostics
    open System.Collections.Generic
    open System.Diagnostics.CodeAnalysis
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.SR
    open Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings

    /// Basic operations on arrays
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Array = 

        let inline checkNonNull argName arg = 
            match box arg with 
            | null -> nullArg argName 
            | _ -> ()

        [<CompiledName("Length")>]
        let length (array: _[])    = array.Length

        [<CompiledName("Initialize")>]
        let inline init count f      = Microsoft.FSharp.Primitives.Basics.Array.init count f

        [<CompiledName("ZeroCreate")>]
        let zeroCreate count    = 
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count

        [<CompiledName("Create")>]
        let create (count:int) (x:'T) =
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            let array = (Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count : 'T[]) 
            for i = 0 to Operators.Checked.(-) count 1 do // use checked arithmetic here to satisfy FxCop
                array.[i] <- x
            array


        [<CompiledName("IsEmpty")>]
        let isEmpty (array: 'T[]) = 
            checkNonNull "array" array
            (array.Length = 0)

        [<CompiledName("Empty")>]
        let empty<'T> = ([| |] : 'T [])

        [<CodeAnalysis.SuppressMessage("Microsoft.Naming","CA1704:IdentifiersShouldBeSpelledCorrectly")>]
        [<CompiledName("CopyTo")>]
        let blit (source : 'T[]) sourceIndex (target: 'T[]) targetIndex count = 
            checkNonNull "source" source
            checkNonNull "target" target
            if sourceIndex < 0 then invalidArg "sourceIndex" (SR.GetString(SR.inputMustBeNonNegative))
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            if targetIndex < 0 then invalidArg "targetIndex" (SR.GetString(SR.inputMustBeNonNegative))
            if sourceIndex + count > source.Length then invalidArg "count" (SR.GetString(SR.outOfRange))
            if targetIndex + count > target.Length then invalidArg "count" (SR.GetString(SR.outOfRange))
            Array.Copy(source, sourceIndex, target, targetIndex, count)
            // for i = 0 to count - 1 do 
            //    target.[targetIndex+i] <- source.[sourceIndex + i]

        let rec concatAddLengths (arrs:'T[][]) i acc =
            if i >= arrs.Length then acc 
            else concatAddLengths arrs (i+1) (acc + arrs.[i].Length)

        let rec concatBlit (arrs:'T[][]) i j (tgt:'T[]) =
            if i < arrs.Length then 
                let h = arrs.[i]
                let len = h.Length 
                Array.Copy(h, 0, tgt, j, len)
                concatBlit arrs (i+1) (j+len) tgt
                
        let concatArrays (arrs : 'T[][]) =
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (concatAddLengths arrs 0 0) 
            concatBlit arrs 0 0 res ;
            res            

        [<CompiledName("Concat")>]
        let concat (arrays: seq<'T[]>) = 
            checkNonNull "arrays" arrays
            match arrays with 
            | :? ('T[][]) as ts -> ts |> concatArrays // avoid a clone, since we only read the array
            | _ -> arrays |> Seq.toArray |> concatArrays

        [<CompiledName("Collect")>]
        let collect (f : 'T -> 'U[])  (array : 'T[]) : 'U[]=
            checkNonNull "array" array
            let len = array.Length
            let result = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked<'U[]> len
            for i = 0 to len - 1 do
                result.[i] <- f array.[i]
            concatArrays result

        [<CompiledName("Append")>]
        let append (array1:'T[]) (array2:'T[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let n1 = array1.Length 
            let n2 = array2.Length 
            let res : 'T[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (n1 + n2)
            Array.Copy(array1, 0, res, 0, n1)
            Array.Copy(array2, 0, res, n1, n2)
            res            

        [<CompiledName("Copy")>]
        let copy (array: 'T[]) =
            checkNonNull "array" array
            (array.Clone() :?> 'T[]) // this is marginally faster
            //let len = array.Length 
            //let res = zeroCreate len 
            //for i = 0 to len - 1 do 
            //    res.[i] <- array.[i]
            //res

        [<CompiledName("ToList")>]
        let toList array = 
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.List.ofArray array

        [<CompiledName("OfList")>]
        let ofList list  = 
            checkNonNull "list" list
            Microsoft.FSharp.Primitives.Basics.List.toArray list


        [<CompiledName("Iterate")>]
        let inline iter f (array: 'T[]) = 
            checkNonNull "array" array
            let len = array.Length
            for i = 0 to len - 1 do 
                f array.[i]

        [<CompiledName("Map")>]
        let inline map (f: 'T -> 'U) (array:'T[]) =
            checkNonNull "array" array
            let len = array.Length
            let res = (Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len : 'U[]) 
            for i = 0 to len - 1 do 
                res.[i] <- f array.[i]
            res

        [<CompiledName("Iterate2")>]
        let iter2 f (array1: 'T[]) (array2: 'U[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let len1 = array1.Length 
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths));
            for i = 0 to len1 - 1 do 
                f.Invoke(array1.[i], array2.[i])

        [<CompiledName("Map2")>]
        let map2 f (array1: 'T[]) (array2: 'U[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let len1 = array1.Length 
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths));
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len1 
            for i = 0 to len1 - 1 do 
                res.[i] <- f.Invoke(array1.[i], array2.[i])
            res

        [<CompiledName("MapIndexed2")>]
        let mapi2 f (array1: 'T[]) (array2: 'U[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let len1 = array1.Length 
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths));
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len1 
            for i = 0 to len1 - 1 do 
                res.[i] <- f.Invoke(i,array1.[i], array2.[i])
            res

        [<CompiledName("IterateIndexed")>]
        let iteri f (array:'T[]) =
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let len = array.Length
            for i = 0 to len - 1 do 
                f.Invoke(i, array.[i])

        [<CompiledName("IterateIndexed2")>]
        let iteri2 f (array1: 'T[]) (array2: 'U[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let len1 = array1.Length 
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths));
            for i = 0 to len1 - 1 do 
                f.Invoke(i,array1.[i], array2.[i])

        [<CompiledName("MapIndexed")>]
        let mapi (f : int -> 'T -> 'U) (array: 'T[]) =
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let len = array.Length
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            for i = 0 to len - 1 do 
                res.[i] <- f.Invoke(i,array.[i])
            res

        [<CompiledName("Exists")>]
        let exists (f: 'T -> bool) (array:'T[]) =
            checkNonNull "array" array
            let len = array.Length
            let rec loop i = i < len && (f array.[i] || loop (i+1))
            loop 0

        [<CompiledName("Exists2")>]
        let exists2 f (array1: _[]) (array2: _[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let len1 = array1.Length
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths))
            let rec loop i = i < len1 && (f.Invoke(array1.[i], array2.[i]) || loop (i+1))
            loop 0

        [<CompiledName("ForAll")>]
        let forall (f: 'T -> bool) (array:'T[]) =
            checkNonNull "array" array
            let len = array.Length
            let rec loop i = i >= len || (f array.[i] && loop (i+1))
            loop 0

        [<CompiledName("ForAll2")>]
        let forall2 f (array1: _[]) (array2: _[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let len1 = array1.Length
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths))
            let rec loop i = i >= len1 || (f.Invoke(array1.[i], array2.[i]) && loop (i+1))
            loop 0

        [<CompiledName("Pick")>]
        let pick f (array: _[]) = 
            checkNonNull "array" array
            let rec loop i = 
                if i >= array.Length then 
                    raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  
                else 
                    match f array.[i] with 
                    | None -> loop(i+1)
                    | Some res -> res
            loop 0 

        [<CompiledName("TryPick")>]
        let tryPick f (array: _[]) = 
            checkNonNull "array" array
            let rec loop i = 
                if i >= array.Length then None else 
                match f array.[i] with 
                | None -> loop(i+1)
                | res -> res
            loop 0 

        [<CompiledName("Choose")>]
        let choose f (array: _[]) = 
            checkNonNull "array" array
            let res = new System.Collections.Generic.List<_>() // ResizeArray
            for i = 0 to array.Length - 1 do 
                match f array.[i] with 
                | None -> ()
                | Some b -> res.Add(b)
            res.ToArray()

        [<CompiledName("Filter")>]
        let filter f (array: _[]) = 
            checkNonNull "array" array
            let res = new System.Collections.Generic.List<_>() // ResizeArray
            for i = 0 to array.Length - 1 do 
                let x = array.[i] 
                if f x then res.Add(x)
            res.ToArray()

        [<CompiledName("Partition")>]
        let partition f (array: _[]) = 
            checkNonNull "array" array
            let res1 = new System.Collections.Generic.List<_>() // ResizeArray
            let res2 = new System.Collections.Generic.List<_>() // ResizeArray
            for i = 0 to array.Length - 1 do 
                let x = array.[i] 
                if f x then res1.Add(x) else res2.Add(x)
            res1.ToArray(), res2.ToArray()

        [<CompiledName("Find")>]
        let find f (array: _[]) = 
            checkNonNull "array" array
            let rec loop i = 
                if i >= array.Length then raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt))) else 
                if f array.[i] then array.[i]  else loop (i+1)
            loop 0 

        [<CompiledName("TryFind")>]
        let tryFind f (array: _[]) = 
            checkNonNull "array" array
            let rec loop i = 
                if i >= array.Length then None else 
                if f array.[i] then Some array.[i]  else loop (i+1)
            loop 0 

        [<CompiledName("Zip")>]
        let zip (array1: _[]) (array2: _[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let len1 = array1.Length 
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths))
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len1 
            for i = 0 to len1 - 1 do 
                res.[i] <- (array1.[i],array2.[i])
            res

        [<CompiledName("Zip3")>]
        let zip3 (array1: _[]) (array2: _[]) (array3: _[]) = 
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            checkNonNull "array3" array3
            let len1 = array1.Length
            if len1 <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths))
            if len1 <> array3.Length then invalidArg "array3" (SR.GetString(SR.arraysHadDifferentLengths))
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len1 
            for i = 0 to len1 - 1 do 
                res.[i] <- (array1.[i],array2.[i],array3.[i])
            res

        [<CompiledName("Unzip")>]
        let unzip (array: _[]) = 
            checkNonNull "array" array
            let len = array.Length 
            let res1 = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            let res2 = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            for i = 0 to len - 1 do 
                let x,y = array.[i] 
                res1.[i] <- x;
                res2.[i] <- y;
            res1,res2

        [<CompiledName("Unzip3")>]
        let unzip3 (array: _[]) = 
            checkNonNull "array" array
            let len = array.Length 
            let res1 = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            let res2 = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            let res3 = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            for i = 0 to len - 1 do 
                let x,y,z = array.[i] 
                res1.[i] <- x;
                res2.[i] <- y;
                res3.[i] <- z;
            res1,res2,res3

        [<CompiledName("Reverse")>]
        let rev (array: _[]) = 
            checkNonNull "array" array
            let len = array.Length 
            let res = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked len 
            for i = 0 to len - 1 do 
                res.[(len - i) - 1] <- array.[i]
            res

        [<CompiledName("Fold")>]
        let fold<'T,'State> (f : 'State -> 'T -> 'State) (acc: 'State) (array:'T[]) =
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = acc 
            let len = array.Length
            for i = 0 to len - 1 do 
                state <- f.Invoke(state,array.[i])
            state

        [<CompiledName("FoldBack")>]
        let foldBack<'T,'State> (f : 'T -> 'State -> 'State) (array:'T[]) (acc: 'State) =
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable res = acc 
            let len = array.Length
            for i = len - 1 downto 0 do 
                res <- f.Invoke(array.[i],res)
            res


        [<CompiledName("FoldBack2")>]
        let foldBack2<'T1,'T2,'State>  f (array1:'T1[]) (array2:'T2 []) (acc: 'State) =
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let mutable res = acc 
            let len = array1.Length
            if len <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths))
            for i = len - 1 downto 0 do 
                res <- f.Invoke(array1.[i],array2.[i],res)
            res

        [<CompiledName("Fold2")>]
        let fold2<'T1,'T2,'State>  f (acc: 'State) (array1:'T1[]) (array2:'T2 []) =
            checkNonNull "array1" array1
            checkNonNull "array2" array2
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
            let mutable state = acc 
            let len = array1.Length
            if len <> array2.Length then invalidArg "array2" (SR.GetString(SR.arraysHadDifferentLengths))
            for i = 0 to len - 1 do 
                state <- f.Invoke(state,array1.[i],array2.[i])
            state


        let foldSubRight f (array : _[]) start fin acc = 
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable res = acc 
            for i = fin downto start do
                res <- f.Invoke(array.[i],res)
            res

        let scanSubRight f (array : _[]) start fin initState = 
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = initState 
            let res = create (2+fin-start) initState 
            for i = fin downto start do
                state <- f.Invoke(array.[i],state);
                res.[i - start] <- state
            res

        let scanSubLeft f  initState (array : _[]) start fin = 
            checkNonNull "array" array
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = initState 
            let res = create (2+fin-start) initState 
            for i = start to fin do
                state <- f.Invoke(state,array.[i]);
                res.[i - start+1] <- state
            res

        [<CompiledName("Scan")>]
        let scan<'T,'State> f (acc:'State) (array : 'T[]) = 
            checkNonNull "array" array
            let len = array.Length
            scanSubLeft f acc array 0 (len - 1)

        [<CompiledName("ScanBack")>]
        let scanBack<'T,'State> f (array : 'T[]) (acc:'State) = 
            checkNonNull "array" array
            let len = array.Length
            scanSubRight f array 0 (len - 1) acc

        [<CompiledName("Reduce")>]
        let reduce f (array : _[]) = 
            checkNonNull "array" array
            let len = array.Length
            if len = 0 then 
                invalidArg "array" InputArrayEmptyString
            else 
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let mutable res = array.[0]
                for i = 1 to len - 1 do
                    res <- f.Invoke(res,array.[i])
                res

        [<CompiledName("ReduceBack")>]
        let reduceBack f (array : _[]) = 
            checkNonNull "array" array
            let len = array.Length
            if len = 0 then invalidArg "array" InputArrayEmptyString
            else foldSubRight f array 0 (len - 2) array.[len - 1]

        [<CompiledName("SortInPlaceWith")>]
        let sortInPlaceWith f (array : 'T[]) =
            checkNonNull "array" array
            let len = array.Length 
            if len < 2 then () 
            elif len = 2 then 
                let c = f array.[0] array.[1] 
                if c > 0 then
                    let tmp = array.[0] 
                    array.[0] <- array.[1]; 
                    array.[1] <- tmp
            else 
                System.Array.Sort(array, ComparisonIdentity.FromFunction(f))

        [<CompiledName("SortInPlaceBy")>]
        let sortInPlaceBy (f: 'T -> 'U) (array : 'T[]) = 
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.unstableSortInPlaceBy f array

        [<CompiledName("SortInPlace")>]
        let sortInPlace (array : 'T[]) = 
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.unstableSortInPlace array

        [<CompiledName("SortWith")>]
        let sortWith (f: 'T -> 'T -> int) (array : 'T[]) =
            checkNonNull "array" array
            let result = copy array
            sortInPlaceWith f result;
            result

        [<CompiledName("SortBy")>]
        let sortBy f array =
            checkNonNull "array" array
            let result = copy array
            sortInPlaceBy f result;
            result

        [<CompiledName("Sort")>]
        let sort array = 
            checkNonNull "array" array
            let result = copy array
            sortInPlace result;
            result
            
        [<CompiledName("ToSeq")>]
        let toSeq array = 
            checkNonNull "array" array
            Seq.ofArray array

        [<CompiledName("OfSeq")>]
        let ofSeq  source = 
            checkNonNull "source" source
            Seq.toArray source

        [<CompiledName("FindIndex")>]
        let findIndex f (array : _[]) = 
            checkNonNull "array" array
            let len = array.Length 
            let rec go n = 
                if n >= len then 
                    raise (System.Collections.Generic.KeyNotFoundException(SR.GetString(SR.keyNotFoundAlt)))  
                elif f array.[n] then 
                    n 
                else go (n+1)
            go 0

        [<CompiledName("TryFindIndex")>]
        let tryFindIndex f (array : _[]) = 
            checkNonNull "array" array
            let len = array.Length 
            let rec go n = if n >= len then None elif f array.[n] then Some n else go (n+1)
            go 0 

        [<CompiledName("Permute")>]
        let permute p (array : _[]) =  
            checkNonNull "array" array
            Microsoft.FSharp.Primitives.Basics.Array.permute p array

        [<CompiledName("Sum")>]
        let inline sum (array: (^T)[] ) : ^T = 
            checkNonNull "array" array
            let mutable acc = LanguagePrimitives.GenericZero< (^T) >
            for i = 0 to array.Length - 1 do
                acc <- Checked.(+) acc array.[i]
            acc

        [<CompiledName("SumBy")>]
        let inline sumBy (f: 'T -> ^U) (array:'T[]) : ^U = 
            checkNonNull "array" array
            let mutable acc = LanguagePrimitives.GenericZero< (^U) >
            for i = 0 to array.Length - 1 do
                acc <- Checked.(+) acc (f array.[i])
            acc

        [<CompiledName("Min")>]
        let inline min (array:_[]) = 
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" InputArrayEmptyString
            let mutable acc = array.[0]
            for i = 1 to array.Length - 1 do
                let curr = array.[i]
                if curr < acc then 
                    acc <- curr
            acc

        [<CompiledName("MinBy")>]
        let inline minBy f (array:_[]) = 
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" InputArrayEmptyString
            let mutable accv = array.[0]
            let mutable acc = f accv
            for i = 1 to array.Length - 1 do
                let currv = array.[i]
                let curr = f currv
                if curr < acc then
                    acc <- curr
                    accv <- currv
            accv

        [<CompiledName("Max")>]
        let inline max (array:_[]) = 
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" InputArrayEmptyString
            let mutable acc = array.[0]
            for i = 1 to array.Length - 1 do
                let curr = array.[i]
                if curr > acc then 
                      acc <- curr
            acc

        [<CompiledName("MaxBy")>]
        let inline maxBy f (array:_[]) = 
            checkNonNull "array" array
            if array.Length = 0 then invalidArg "array" InputArrayEmptyString
            let mutable accv = array.[0]
            let mutable acc = f accv
            for i = 1 to array.Length - 1 do
                let currv = array.[i]
                let curr = f currv
                if curr > acc then
                    acc <- curr
                    accv <- currv
            accv

        [<CompiledName("Average")>]
        let inline average      (array:_[]) = 
            checkNonNull "array" array
            Seq.average array

        [<CompiledName("AverageBy")>]
        let inline averageBy f (array:_[]) = 
            checkNonNull "array" array
            Seq.averageBy f array

        [<CompiledName("GetSubArray")>]
        let sub (array:'T[]) (startIndex:int) (count:int) =
            checkNonNull "array" array
            if startIndex < 0 then invalidArg "startIndex" (SR.GetString(SR.inputMustBeNonNegative))
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            if startIndex + count > array.Length then invalidArg "count" (SR.GetString(SR.outOfRange))

            let res = (Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count : 'T[])  
            for i = 0 to count - 1 do 
                res.[i] <- array.[startIndex + i]
            res


        [<CompiledName("Get")>]
        let get (array:_[]) n = 
            array.[n]

        [<CompiledName("Set")>]
        let set (array:_[]) n v = 
            array.[n] <- v

        [<CompiledName("Fill")>]
        let fill (target:'T[]) (targetIndex:int) (count:int) (x:'T) =
            checkNonNull "target" target
            if targetIndex < 0 then invalidArg "targetIndex" (SR.GetString(SR.inputMustBeNonNegative))
            if count < 0 then invalidArg "count" (SR.GetString(SR.inputMustBeNonNegative))
            for i = targetIndex to targetIndex + count - 1 do 
                target.[i] <- x
            

#if FX_ATLEAST_40   
        module Parallel =
            open System.Threading.Tasks
            
            [<CompiledName("Choose")>]
            let choose f (array: 'T[]) = 
                checkNonNull "array" array
                let inputLength = array.Length
                let lastInputIndex = inputLength - 1

                let isChosen : bool [] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked inputLength
                let results : 'U [] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked inputLength
                
                Parallel.For(0, inputLength, (fun i -> 
                    match f array.[i] with 
                    | None -> () 
                    | Some v -> 
                        isChosen.[i] <- true; 
                        results.[i] <- v
                )) |> ignore         
                                                                                      
                let mutable outputLength = 0                
                for i = 0 to lastInputIndex do 
                    if isChosen.[i] then 
                        outputLength <- outputLength + 1
                        
                let output = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked outputLength
                let mutable curr = 0
                for i = 0 to lastInputIndex do 
                    if isChosen.[i] then 
                        output.[curr] <- results.[i]
                        curr <- curr + 1
                output
                
            [<CompiledName("Collect")>]
            let collect (f : 'T -> 'U[])  (array : 'T[]) : 'U[]=
                checkNonNull "array" array
                let inputLength = array.Length
                let result = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked inputLength
                Parallel.For(0, inputLength, 
                    (fun i -> result.[i] <- f array.[i])) |> ignore
                concatArrays result
                
            [<CompiledName("Map")>]
            let map (f: 'T -> 'U) (array : 'T[]) : 'U[]=
                checkNonNull "array" array
                let inputLength = array.Length
                let result = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked inputLength
                Parallel.For(0, inputLength, fun i ->
                    result.[i] <- f array.[i]) |> ignore
                result
                
            [<CompiledName("MapIndexed")>]
            let mapi f (array: 'T[]) =
                checkNonNull "array" array
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                let inputLength = array.Length
                let result = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked inputLength 
                Parallel.For(0, inputLength, fun i ->
                    result.[i] <- f.Invoke (i, array.[i])) |> ignore
                result
                
            [<CompiledName("Iterate")>]
            let iter f (array : 'T[]) =
                checkNonNull "array" array
                Parallel.For (0, array.Length, fun i -> f array.[i]) |> ignore  
                
            [<CompiledName("IterateIndexed")>]
            let iteri f (array : 'T[]) =
                checkNonNull "array" array
                let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
                Parallel.For (0, array.Length, fun i -> f.Invoke(i, array.[i])) |> ignore        
                
            [<CompiledName("Initialize")>]
            let init count f =
                let result = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked count
                Parallel.For (0, count, fun i -> result.[i] <- f i) |> ignore
                result
                
            [<CompiledName("Partition")>]
            let partition predicate (array : 'T[]) =
                checkNonNull "array" array
                let inputLength = array.Length
                let lastInputIndex = inputLength - 1

                let isTrue = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked inputLength
                Parallel.For(0, inputLength, 
                    fun i -> isTrue.[i] <- predicate array.[i]
                    ) |> ignore
                
                let mutable trueLength = 0
                for i in 0 .. lastInputIndex do
                    if isTrue.[i] then trueLength <- trueLength + 1
                
                let trueResult = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked trueLength
                let falseResult = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (inputLength - trueLength)
                let mutable iTrue = 0
                let mutable iFalse = 0
                for i = 0 to lastInputIndex do
                    if isTrue.[i] then
                        trueResult.[iTrue] <- array.[i]
                        iTrue <- iTrue + 1
                    else
                        falseResult.[iFalse] <- array.[i]
                        iFalse <- iFalse + 1

                (trueResult, falseResult)
#endif               
