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

/// <summary>Pervasives: Additional bindings available at the top level</summary>
namespace Microsoft.FSharp.Core

[<AutoOpen>]
module ExtraTopLevelOperators = 

    open System
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Control
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Text
    open Microsoft.FSharp.Math

    /// <summary>Print to <c>stdout</c> using the given format.</summary>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormat")>]
    val printf  :                format:Printf.TextWriterFormat<'T> -> 'T

    /// <summary>Print to <c>stdout</c> using the given format, and add a newline.</summary>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatLine")>]
    val printfn  :                format:Printf.TextWriterFormat<'T> -> 'T

    /// <summary>Print to <c>stderr</c> using the given format.</summary>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatToError")>]
    val eprintf  :               format:Printf.TextWriterFormat<'T> -> 'T

    /// <summary>Print to <c>stderr</c> using the given format, and add a newline.</summary>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatLineToError")>]
    val eprintfn  :               format:Printf.TextWriterFormat<'T> -> 'T

    /// <summary>Print to a string using the given format.</summary>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatToString")>]
    val sprintf :                format:Printf.StringFormat<'T> -> 'T

    /// <summary>Print to a string buffer and raise an exception with the given
    /// result.   Helper printers must return strings.</summary>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatToStringThenFail")>]
    val failwithf: format:Printf.StringFormat<'T,'Result> -> 'T

    /// <summary>Print to a file using the given format.</summary>
    /// <param name="textWriter">The file TextWriter.</param>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatToTextWriter")>]
    val fprintf : textWriter:System.IO.TextWriter -> format:Printf.TextWriterFormat<'T> -> 'T

    /// <summary>Print to a file using the given format, and add a newline.</summary>
    /// <param name="textWriter">The file TextWriter.</param>
    /// <param name="format">The formatter.</param>
    /// <returns>The formatted result.</returns>
    [<CompiledName("PrintFormatLineToTextWriter")>]
    val fprintfn : textWriter:System.IO.TextWriter -> format:Printf.TextWriterFormat<'T> -> 'T

    /// <summary>Builds a set from a sequence of objects. The objects are indexed using generic comparison.</summary>
    /// <param name="elements">The input sequence of elements.</param>
    /// <returns>The created set.</returns>
    [<CompiledName("CreateSet")>]
    val set : elements:seq<'T> -> Set<'T>

    /// <summary>Builds an aysnchronous workflow using computation expression syntax.</summary>
    [<CompiledName("DefaultAsyncBuilder")>]
    val async : Microsoft.FSharp.Control.AsyncBuilder  

    /// <summary>Converts the argument to 32-bit float.</summary>
    /// <remarks>This is a direct conversion for all 
    /// primitive numeric types. For strings, the input is converted using <c>Single.Parse()</c>  with InvariantCulture settings. Otherwise the operation requires and invokes a <c>ToSingle</c> method on the input type.</remarks>
    [<CompiledName("ToSingle")>]
    val inline single     : value:^T -> single     when ^T : (static member op_Explicit : ^T -> single)     and default ^T : int

    /// <summary>Converts the argument to 64-bit float.</summary>
    /// <remarks>This is a direct conversion for all 
    /// primitive numeric types. For strings, the input is converted using <c>Double.Parse()</c>  with InvariantCulture settings. Otherwise the operation requires and invokes a <c>ToDouble</c> method on the input type.</remarks>
    [<CompiledName("ToDouble")>]
    val inline double     : value:^T -> float      when ^T : (static member op_Explicit : ^T -> double)     and default ^T : int

    /// <summary>Converts the argument to byte.</summary>
    /// <remarks>This is a direct conversion for all 
    /// primitive numeric types. For strings, the input is converted using <c>Byte.Parse()</c> on strings and otherwise requires a <c>ToByte</c> method on the input type.</remarks>
    [<CompiledName("ToByte")>]
    val inline uint8       : value:^T -> byte       when ^T : (static member op_Explicit : ^T -> byte)       and default ^T : int        
    
    /// <summary>Converts the argument to signed byte.</summary>
    /// <remarks>This is a direct conversion for all 
    /// primitive numeric types. For strings, the input is converted using <c>SByte.Parse()</c>  with InvariantCulture settings.
    /// Otherwise the operation requires and invokes a <c>ToSByte</c> method on the input type.</remarks>
    [<CompiledName("ToSByte")>]
    val inline int8      : value:^T -> sbyte      when ^T : (static member op_Explicit : ^T -> sbyte)      and default ^T : int
    

    /// <summary>Builds a read-only lookup table from a sequence of key/value pairs. The key objects are indexed using generic hashing and equality.</summary>
    [<CompiledName("CreateDictionary")>]
    val dict : keyValuePairs:seq<'Key * 'Value> -> System.Collections.Generic.IDictionary<'Key,'Value> when 'Key : equality

    /// <summary>Builds a 2D array from a sequence of sequences of elements.</summary>
    [<CompiledName("CreateArray2D")>]
    val array2D : rows:seq<#seq<'T>> -> 'T[,]


    #if FX_MINIMAL_REFLECTION // not on Compact Framework 
    #else
    /// <summary>Special prefix operator for splicing typed expressions into quotation holes.</summary>
    [<CompiledName("SpliceExpression")>]
    val (~%) : expression:Microsoft.FSharp.Quotations.Expr<'T> -> 'T

    /// <summary>Special prefix operator for splicing untyped expressions into quotation holes.</summary>
    [<CompiledName("SpliceUntypedExpression")>]
    val (~%%) : expression:Microsoft.FSharp.Quotations.Expr -> 'T
    #endif

    /// <summary>An active pattern to force the execution of values of type <c>Lazy&lt;_&gt;</c>.</summary>
    [<CompiledName("LazyPattern")>]
    val (|Lazy|) : input:Lazy<'T> -> 'T

#if EXTRAS_FOR_SILVERLIGHT_COMPILER
namespace Microsoft.FSharp

    open Microsoft.FSharp.Core

    [<StructuralEquality; NoComparison>]
    exception UserInterrupt

    [<Class; NoComparison>]
    type Silverlight =
        static member EmitInterruptChecks : bool with get, set
        static member InterruptThread: id: int -> unit
        static member ResumeThread: id: int -> unit
        static member CheckInterrupt: unit -> unit

        static member WriteLine : unit -> unit
        static member WriteLine : value2:string -> unit
        static member WriteLine : value:obj -> unit
        static member WriteLine : value:int -> unit
        static member WriteLine : format:string * arg0:obj -> unit
        static member WriteLine : format:string * arg:obj [] -> unit
        static member WriteLine : format:string * arg0:obj * arg1:obj -> unit
        static member WriteLine : format:string * arg0:obj * arg1:obj * arg2:obj -> unit
        static member WriteLine : format:string * arg0:obj * arg1:obj * arg2:obj * arg3:obj -> unit

        static member Write : value2:string -> unit
        static member Write : value:obj -> unit
        static member Write : value:int -> unit
        static member Write : format:string * arg0:obj -> unit
        static member Write : format:string * arg:obj [] -> unit
        static member Write : format:string * arg0:obj * arg1:obj -> unit
        static member Write : format:string * arg0:obj * arg1:obj * arg2:obj -> unit
        static member Write : format:string * arg0:obj * arg1:obj * arg2:obj * arg3:obj -> unit
#endif
