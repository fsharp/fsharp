//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
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

    /// Creates a read-only wrapper around a mutable dictionary.
    let private readOnlyDict (t : Dictionary<_,_>) : IDictionary<_,_> =
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
                    match d.TryGetValue (RuntimeHelpers.StructBox(k)) with
                    | true, v ->
                        r <- v
                        true
                    | false, _ ->
                        false
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

    /// An empty, read-only dictionary.
    /// This field is used to avoid (potentially) creating multiple empty dictionary instances;
    /// having only one empty dictionary instance helps make better use of structure-sharing.
    let private emptyDict<'Key, 'Value when 'Key : equality> =
        readOnlyDict <| Dictionary<RuntimeHelpers.StructBox<'Key>, 'Value> (0, RuntimeHelpers.StructBox<'Key>.Comparer)

    [<CompiledName("CreateDictionary")>]
    let dict (keyValuePairs : seq<'Key * 'Value>) =
        checkNonNullNullArg "keyValuePairs" keyValuePairs

        // Use a dictionary (this requires hashing and equality on the key type)
        // Wrap keys in a StructBox in case they are null (when System.Collections.Generic.Dictionary fails).
        // Try to use an optimized implementation based on the input type.
        match keyValuePairs with
        | :? (('Key * 'Value)[]) as kvpArray ->
            match kvpArray.Length with
            | 0 -> emptyDict
            | len ->
                let t = Dictionary<RuntimeHelpers.StructBox<'Key>,_>(kvpArray.Length, RuntimeHelpers.StructBox<'Key>.Comparer)

                for i = 0 to len - 1 do
                    let k, v = kvpArray.[i]
                    t.[RuntimeHelpers.StructBox(k)] <- v
                readOnlyDict t

        | :? (('Key * 'Value) list) as kvpList ->
            if kvpList.IsEmpty then
                emptyDict
            else
                // List.count is O(n)! However, it should still be faster than (potentially) resizing the dictionary.
                let t = Dictionary<RuntimeHelpers.StructBox<'Key>,_>(kvpList.Length, RuntimeHelpers.StructBox<'Key>.Comparer)

                let mutable kvpList = kvpList
                while not kvpList.IsEmpty do
                    let k, v = kvpList.Head
                    t.[RuntimeHelpers.StructBox(k)] <- v
                    kvpList <- kvpList.Tail
                readOnlyDict t

        | :? IList<'Key * 'Value> as kvpList ->
            match kvpList.Count with
            | 0 -> emptyDict
            | len ->
                let t = Dictionary<RuntimeHelpers.StructBox<'Key>,_>(kvpList.Count, RuntimeHelpers.StructBox<'Key>.Comparer)

                for i = 0 to len - 1 do
                    let k, v = kvpList.[i]
                    t.[RuntimeHelpers.StructBox(k)] <- v
                readOnlyDict t

        | :? ICollection<'Key * 'Value> as kvpColl ->
            match kvpColl.Count with
            | 0 -> emptyDict
            | count ->
                let t = Dictionary<RuntimeHelpers.StructBox<'Key>,_>(count, RuntimeHelpers.StructBox<'Key>.Comparer)
                for (k,v) in keyValuePairs do 
                    t.[RuntimeHelpers.StructBox(k)] <- v
                readOnlyDict t

        | _ ->
            let t = Dictionary<RuntimeHelpers.StructBox<'Key>,_>(RuntimeHelpers.StructBox<'Key>.Comparer)
            for (k,v) in keyValuePairs do 
                t.[RuntimeHelpers.StructBox(k)] <- v
            
            // If the created dictionary is empty (because the input sequence was empty), return 'emptyDict';
            // this avoids the need to use Seq.isEmpty (which can cause problems if it has to partially or wholly
            // evaluate the sequence just to determine if it's empty, then evaluate again to create the dictionary).
            if t.Count = 0 then emptyDict
            else readOnlyDict t

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

    [<CompiledName("PrintFormatLineToTextWriter")>]
    let fprintfn (os:TextWriter) fp = Printf.fprintfn os fp 
    
#if FX_NO_SYSTEM_CONSOLE
#else    
    [<CompiledName("PrintFormat")>]
    let printf      fp = Printf.printf      fp 

    [<CompiledName("PrintFormatToError")>]
    let eprintf     fp = Printf.eprintf     fp 

    [<CompiledName("PrintFormatLine")>]
    let printfn     fp = Printf.printfn     fp 

    [<CompiledName("PrintFormatLineToError")>]
    let eprintfn    fp = Printf.eprintfn    fp 
#endif

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
#if QUERIES_IN_FSLIB
    [<assembly: AutoOpen("Microsoft.FSharp.Linq.QueryRunExtensions.LowPriority")>]
    [<assembly: AutoOpen("Microsoft.FSharp.Linq.QueryRunExtensions.HighPriority")>]
#endif
    do()

    [<CompiledName("LazyPattern")>]
    let (|Lazy|) (x:Lazy<_>) = x.Force()


#if QUERIES_IN_FSLIB
    let query = Microsoft.FSharp.Linq.QueryBuilder()
#if EXTRA_DEBUG
    let queryexpr = Microsoft.FSharp.Linq.QueryExprBuilder()
    let queryexprpretrans = Microsoft.FSharp.Linq.QueryExprPreTransBuilder()
    let queryexprpreelim = Microsoft.FSharp.Linq.QueryExprPreEliminateNestedBuilder()
    let queryquote = Microsoft.FSharp.Linq.QueryQuoteBuilder()
    let querylinqexpr = Microsoft.FSharp.Linq.QueryLinqExprBuilder()
#endif


#endif
#if PUT_TYPE_PROVIDERS_IN_FSCORE
namespace Microsoft.FSharp.Core.CompilerServices

    open System
    open System.Reflection
    open System.Linq.Expressions
    open System.Collections.Generic
    open Microsoft.FSharp.Core


    /// <summary>Represents the product of two measure expressions when returned as a generic argument of a provided type.</summary>
    [<Sealed>]
    type MeasureProduct<'Measure1, 'Measure2>() = class end

    /// <summary>Represents the inverse of a measure expressions when returned as a generic argument of a provided type.</summary>
    [<Sealed>]
    type MeasureInverse<'Measure>  = class end

    /// <summary>Represents the '1' measure expression when returned as a generic argument of a provided type.</summary>
    [<Sealed>]
    type MeasureOne  = class end

    [<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
    type TypeProviderAttribute() =
        inherit System.Attribute()

    [<AttributeUsage(AttributeTargets.Assembly, AllowMultiple = false)>]
    type TypeProviderAssemblyAttribute(assemblyName : string) = 
        inherit System.Attribute()
        new () = TypeProviderAssemblyAttribute(null)
        member __.AssemblyName = assemblyName

    [<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
    type TypeProviderXmlDocAttribute(commentText: string) = 
        inherit System.Attribute()
        member __.CommentText = commentText

    [<AttributeUsage(AttributeTargets.All, AllowMultiple = false)>]
    type TypeProviderDefinitionLocationAttribute() = 
        inherit System.Attribute()
        let mutable filePath : string = null
        let mutable line : int = 0
        let mutable column : int = 0
        member this.FilePath with get() = filePath and set v = filePath <- v
        member this.Line with get() = line and set v = line <- v
        member this.Column with get() = column and set v = column <- v

    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Interface ||| AttributeTargets.Struct ||| AttributeTargets.Delegate, AllowMultiple = false)>]
    type TypeProviderEditorHideMethodsAttribute() = 
        inherit System.Attribute()

    /// <summary>Additional type attribute flags related to provided types</summary>
    type TypeProviderTypeAttributes =
        | SuppressRelocate = 0x80000000
        | IsErased = 0x40000000

    type TypeProviderConfig( systemRuntimeContainsType : string -> bool ) =
        let mutable resolutionFolder      : string   = null  
        let mutable runtimeAssembly       : string   = null  
        let mutable referencedAssemblies  : string[] = null  
        let mutable temporaryFolder       : string   = null  
        let mutable isInvalidationSupported : bool   = false 
        let mutable useResolutionFolderAtRuntime : bool = false
        let mutable systemRuntimeAssemblyVersion : System.Version = null
        member this.ResolutionFolder         with get() = resolutionFolder        and set v = resolutionFolder <- v
        member this.RuntimeAssembly          with get() = runtimeAssembly         and set v = runtimeAssembly <- v
        member this.ReferencedAssemblies     with get() = referencedAssemblies    and set v = referencedAssemblies <- v
        member this.TemporaryFolder          with get() = temporaryFolder         and set v = temporaryFolder <- v
        member this.IsInvalidationSupported  with get() = isInvalidationSupported and set v = isInvalidationSupported <- v
        member this.IsHostedExecution with get() = useResolutionFolderAtRuntime and set v = useResolutionFolderAtRuntime <- v
        member this.SystemRuntimeAssemblyVersion  with get() = systemRuntimeAssemblyVersion and set v = systemRuntimeAssemblyVersion <- v
        member this.SystemRuntimeContainsType (typeName : string) = systemRuntimeContainsType typeName

#if FX_NO_CUSTOMATTRIBUTEDATA
    type IProvidedCustomAttributeTypedArgument =
        abstract ArgumentType: System.Type
        abstract Value: System.Object

    type IProvidedCustomAttributeNamedArgument =
        abstract ArgumentType: System.Type
        abstract MemberInfo: System.Reflection.MemberInfo
        abstract TypedValue: IProvidedCustomAttributeTypedArgument

    type IProvidedCustomAttributeData =
        abstract Constructor: System.Reflection.ConstructorInfo
        abstract ConstructorArguments: System.Collections.Generic.IList<IProvidedCustomAttributeTypedArgument>
        abstract NamedArguments: System.Collections.Generic.IList<IProvidedCustomAttributeNamedArgument>
#endif

    type IProvidedNamespace =
        abstract NamespaceName : string
        abstract GetNestedNamespaces : unit -> IProvidedNamespace[] 
        abstract GetTypes : unit -> Type[] 
        abstract ResolveTypeName : typeName: string -> Type

    type ITypeProvider =
        inherit System.IDisposable
        abstract GetNamespaces : unit -> IProvidedNamespace[] 
        abstract GetStaticParameters : typeWithoutArguments:Type -> ParameterInfo[] 
        abstract ApplyStaticArguments : typeWithoutArguments:Type * typePathWithArguments:string[] * staticArguments:obj[] -> Type 
        abstract GetInvokerExpression : syntheticMethodBase:MethodBase * parameters:Microsoft.FSharp.Quotations.Expr[] -> Microsoft.FSharp.Quotations.Expr

        [<CLIEvent>]
        abstract Invalidate : Microsoft.FSharp.Control.IEvent<System.EventHandler, System.EventArgs>
        abstract GetGeneratedAssemblyContents : assembly:System.Reflection.Assembly -> byte[]

#if FX_NO_CUSTOMATTRIBUTEDATA
        abstract GetMemberCustomAttributesData : assembly:System.Reflection.MemberInfo -> System.Collections.Generic.IList<IProvidedCustomAttributeData>
        abstract GetParameterCustomAttributesData : assembly:System.Reflection.ParameterInfo -> System.Collections.Generic.IList<IProvidedCustomAttributeData>
#endif

#endif

#if EXTRAS_FOR_SILVERLIGHT_COMPILER
namespace Microsoft.FSharp

    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.Operators
    open ExtraTopLevelOperators
    open System
    open System.Collections.Generic
    open System.Threading

    [<StructuralEquality; NoComparison>]
    exception UserInterrupt

    [<NoComparison>]
    type Silverlight() =
        static let threadsToKill = HashSet<int>()
        static let mutable isNotEmpty = false
        static let mutable emitChecks = false

        static member EmitInterruptChecks with get() = emitChecks and set b = emitChecks <- b

        static member InterruptThread(id) =
            isNotEmpty <- true
            threadsToKill.Add(id) |> ignore

        static member ResumeThread(id) =
            threadsToKill.Remove(id) |> ignore
            isNotEmpty <- threadsToKill.Count > 0

        static member CheckInterrupt() =
            if isNotEmpty then
                let id = Thread.CurrentThread.ManagedThreadId
                if threadsToKill.Contains(id) then raise UserInterrupt

        static member WriteLine() = printfn ""
        static member WriteLine(value2: string) = printfn "%s" value2
        static member WriteLine(value: obj) = printfn "%O" value
        static member WriteLine(value3: int) = printfn "%d" value3
        static member WriteLine(format: string, arg0: obj) =
            printfn "%s" (String.Format(format, arg0))
        static member WriteLine(format: string, arg0: obj, arg1:obj) =
            printfn "%s" (String.Format(format, arg0, arg1))
        static member WriteLine(format: string, arg0: obj, arg1:obj, arg2: obj) =
            printfn "%s" (String.Format(format, arg0, arg1, arg2))
        static member WriteLine(format: string, arg0: obj, arg1:obj, arg2: obj, arg3: obj) =
            printfn "%s" (String.Format(format, arg0, arg1, arg2, arg3))
        static member WriteLine(format: string, [<ParamArray>] arg: obj[]) =
            printfn "%s" (String.Format(format, arg))

        static member Write(value2: string) = printf "%s" value2
        static member Write(value: obj) = printf "%O" value
        static member Write(value3: int) = printf "%d" value3
        static member Write(format: string, arg0: obj) =
            printf "%s" (String.Format(format, arg0))
        static member Write(format: string, arg0: obj, arg1:obj) =
            printf "%s" (String.Format(format, arg0, arg1))
        static member Write(format: string, arg0: obj, arg1:obj, arg2: obj) =
            printf "%s" (String.Format(format, arg0, arg1, arg2))
        static member Write(format: string, arg0: obj, arg1:obj, arg2: obj, arg3: obj) =
            printf "%s" (String.Format(format, arg0, arg1, arg2, arg3))
        static member Write(format: string, [<ParamArray>] arg: obj[]) =
            printf "%s" (String.Format(format, arg))
#endif
