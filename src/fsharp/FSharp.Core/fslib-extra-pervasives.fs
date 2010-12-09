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

namespace Microsoft.FSharp.Core

module ExtraTopLevelOperators =

    open System
    open System.Collections.Generic
    open System.IO
    open System.Diagnostics
    open Microsoft.FSharp
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Text
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Primitives.Basics
    open Microsoft.FSharp.Core.CompilerServices

    let inline checkNonNullNullArg argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()

    let inline checkNonNullInvalidArg argName message arg = 
        match box arg with 
        | null -> invalidArg argName message
        | _ -> ()

    [<CompiledName("CreateSet")>]
    let set l = Collections.Set.ofSeq l

    [<CompiledName("CreateDictionary")>]
    let dict l = 
        // Use a dictionary (this requires hashing and equality on the key type)
        // Wrap keys in a StructBox in case they are null (when System.Collections.Generic.Dictionary fails). 
        let t = new Dictionary<RuntimeHelpers.StructBox<'Key>,_>(RuntimeHelpers.StructBox<'Key>.Comparer)
        for (k,v) in l do 
            t.[RuntimeHelpers.StructBox(k)] <- v
        let d = (t :> IDictionary<_,_>)
        let c = (t :> ICollection<_>)
        // Give a read-only view of the dictionary
        { new IDictionary<'Key, 'T> with 
                member s.Item 
                    with get x = d.[RuntimeHelpers.StructBox(x)]
                    and  set x v = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)))
                member s.Keys = 
                    let keys = d.Keys
                    { new ICollection<'Key> with 
                          member s.Add(x) = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)));
                          member s.Clear() = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)));
                          member s.Remove(x) = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)));
                          member s.Contains(x) = keys.Contains(RuntimeHelpers.StructBox(x))
                          member s.CopyTo(arr,i) = 
                              let mutable n = 0 
                              for k in keys do 
                                  arr.[i+n] <- k.Value
                                  n <- n + 1
                          member s.IsReadOnly = true
                          member s.Count = keys.Count
                      interface IEnumerable<'Key> with
                            member s.GetEnumerator() = (keys |> Seq.map (fun v -> v.Value)).GetEnumerator()
                      interface System.Collections.IEnumerable with
                            member s.GetEnumerator() = ((keys |> Seq.map (fun v -> v.Value)) :> System.Collections.IEnumerable).GetEnumerator() }
                    
                member s.Values = d.Values
                member s.Add(k,v) = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)))
                member s.ContainsKey(k) = d.ContainsKey(RuntimeHelpers.StructBox(k))
                member s.TryGetValue(k,r) = 
                    let key = RuntimeHelpers.StructBox(k)
                    if d.ContainsKey(key) then (r <- d.[key]; true) else false
                member s.Remove(k : 'Key) = (raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated))) : bool) 
          interface ICollection<KeyValuePair<'Key, 'T>> with 
                member s.Add(x) = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)));
                member s.Clear() = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)));
                member s.Remove(x) = raise (NotSupportedException(SR.GetString(SR.thisValueCannotBeMutated)));
                member s.Contains(KeyValue(k,v)) = c.Contains(KeyValuePair<_,_>(RuntimeHelpers.StructBox(k),v))
                member s.CopyTo(arr,i) = 
                    let mutable n = 0 
                    for (KeyValue(k,v)) in c do 
                        arr.[i+n] <- KeyValuePair<_,_>(k.Value,v)
                        n <- n + 1
                member s.IsReadOnly = true
                member s.Count = c.Count
          interface IEnumerable<KeyValuePair<'Key, 'T>> with
                member s.GetEnumerator() = 
                    (c |> Seq.map (fun (KeyValue(k,v)) -> KeyValuePair<_,_>(k.Value,v))).GetEnumerator()
          interface System.Collections.IEnumerable with
                member s.GetEnumerator() = 
                    ((c |> Seq.map (fun (KeyValue(k,v)) -> KeyValuePair<_,_>(k.Value,v))) :> System.Collections.IEnumerable).GetEnumerator() }


    let getArray (vals : seq<'T>) = 
        match vals with
        | :? ('T[]) as arr -> arr
        | _ -> Seq.toArray vals

    [<CompiledName("CreateArray2D")>]
    let array2D (rows : seq<#seq<'T>>) = 
        checkNonNullNullArg "rows" rows
        let rowsArr = getArray rows
        let m = rowsArr.Length
        if m = 0 
        then Array2D.zeroCreate<'T> 0 0 
        else
            checkNonNullInvalidArg "rows" (SR.GetString(SR.nullsNotAllowedInArray)) rowsArr.[0]
            let firstRowArr = getArray rowsArr.[0]
            let n = firstRowArr.Length
            let res = Array2D.zeroCreate<'T> m n
            for j in 0..(n-1) do    
                res.[0,j] <- firstRowArr.[j]
            for i in 1..(m-1) do
              checkNonNullInvalidArg "rows" (SR.GetString(SR.nullsNotAllowedInArray)) rowsArr.[i]
              let rowiArr = getArray rowsArr.[i]
              if rowiArr.Length <> n then invalidArg "vals" (SR.GetString(SR.arraysHadDifferentLengths))
              for j in 0..(n-1) do
                res.[i,j] <- rowiArr.[j]
            res

    // --------------------------------------------------------------------
    // Printf
    // -------------------------------------------------------------------- 

    [<CompiledName("PrintFormatToString")>]
    let sprintf     fp = Printf.sprintf     fp

    [<CompiledName("PrintFormatToStringThenFail")>]
    let failwithf   fp = Printf.failwithf   fp

    [<CompiledName("PrintFormatToTextWriter")>]
    let fprintf (os:TextWriter)  fp = Printf.fprintf os  fp 

    [<CompiledName("PrintFormat")>]
    let printf      fp = Printf.printf      fp 

    [<CompiledName("PrintFormatToError")>]
    let eprintf     fp = Printf.eprintf     fp 

    [<CompiledName("PrintFormatLineToTextWriter")>]
    let fprintfn (os:TextWriter) fp = Printf.fprintfn os fp 

    [<CompiledName("PrintFormatLine")>]
    let printfn     fp = Printf.printfn     fp 

    [<CompiledName("PrintFormatLineToError")>]
    let eprintfn    fp = Printf.eprintfn    fp 

    [<CompiledName("FailWith")>]
    let failwith s = raise (Failure s)

    [<CompiledName("DefaultAsyncBuilder")>]
    let async = new Microsoft.FSharp.Control.AsyncBuilder()

    [<CompiledName("ToSingle")>]
    let inline single x = float32 x

    [<CompiledName("ToDouble")>]
    let inline double x = float x

    [<CompiledName("ToByte")>]
    let inline uint8 x = byte x

    [<CompiledName("ToSByte")>]
    let inline int8 x = sbyte x


    #if FX_MINIMAL_REFLECTION // not on Compact Framework 
    #else
    [<CompiledName("SpliceExpression")>]
    let (~%) (_:Microsoft.FSharp.Quotations.Expr<'a>) : 'a = raise <| InvalidOperationException(SR.GetString(SR.firstClassUsesOfSplice)) 

    [<CompiledName("SpliceUntypedExpression")>]
    let (~%%) (_: Microsoft.FSharp.Quotations.Expr) : 'a = raise <| InvalidOperationException (SR.GetString(SR.firstClassUsesOfSplice)) 
    #endif

    [<assembly: AutoOpen("Microsoft.FSharp")>]
    [<assembly: AutoOpen("Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators")>]
    [<assembly: AutoOpen("Microsoft.FSharp.Core")>]
    [<assembly: AutoOpen("Microsoft.FSharp.Collections")>]
    [<assembly: AutoOpen("Microsoft.FSharp.Control")>]
    do()

    [<CompiledName("LazyPattern")>]
    let (|Lazy|) (x:Lazy<_>) = x.Force()

