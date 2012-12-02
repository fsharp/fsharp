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

#if FX_NO_SYSTEM_CONSOLE
#else    
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
#endif

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

        
#if QUERIES_IN_FSLIB
    /// <summary>Builds a query using query syntax and operators.</summary>
    val query : Microsoft.FSharp.Linq.QueryBuilder
#if EXTRA_DEBUG
    val queryexprpretrans : Microsoft.FSharp.Linq.QueryExprPreTransBuilder
    val queryexprpreelim : Microsoft.FSharp.Linq.QueryExprPreEliminateNestedBuilder
    val queryexpr : Microsoft.FSharp.Linq.QueryExprBuilder
    val queryquote : Microsoft.FSharp.Linq.QueryQuoteBuilder
    val querylinqexpr : Microsoft.FSharp.Linq.QueryLinqExprBuilder
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
    type MeasureProduct<'Measure1, 'Measure2> 

    /// <summary>Represents the inverse of a measure expressions when returned as a generic argument of a provided type.</summary>
    type MeasureInverse<'Measure> 

    /// <summary>Represents the '1' measure expression when returned as a generic argument of a provided type.</summary>
    type MeasureOne

    /// <summary>Place on a class that implements ITypeProvider to extend the compiler</summary>
    [<AttributeUsageAttribute(AttributeTargets.Class, AllowMultiple = false)>]
    type TypeProviderAttribute =
        inherit System.Attribute
        /// <summary>Creates an instance of the attribute</summary>
        /// <returns>TypeProviderAttribute</returns>
        new : unit -> TypeProviderAttribute

    /// <summary>Additional type attribute flags related to provided types</summary>
    type TypeProviderTypeAttributes =
        | SuppressRelocate = 0x80000000
        | IsErased = 0x40000000

    /// <summary>Place attribute on runtime assembly to indicate that there is a corresponding design-time 
    /// assembly that contains a type provider. Runtime and designer assembly may be the same. </summary>
    [<AttributeUsageAttribute(AttributeTargets.Assembly, AllowMultiple = false)>]
    type TypeProviderAssemblyAttribute = 
        inherit System.Attribute
        /// <summary>Creates an instance of the attribute</summary>
        /// <returns>TypeProviderAssemblyAttribute</returns>
        new : unit -> TypeProviderAssemblyAttribute
        /// <summary>Creates an instance of the attribute</summary>
        /// <returns>TypeProviderAssemblyAttribute</returns>
        /// <param name="assemblyName">The name of the design-time assembly for this type provider.</param>
        new : assemblyName : string -> TypeProviderAssemblyAttribute
        member AssemblyName : string

    /// The TypeProviderXmlDocAttribute attribute can be added to types and members. 
    /// The language service will display the CommentText property from the attribute 
    /// in the appropriate place when the user hovers over a type or member.
    [<AttributeUsageAttribute(AttributeTargets.All, AllowMultiple = false)>]
    type TypeProviderXmlDocAttribute = 
        inherit System.Attribute
        /// <summary>Creates an instance of the attribute</summary>
        /// <returns>TypeProviderXmlDocAttribute</returns>
        new : commentText : string -> TypeProviderXmlDocAttribute
        member CommentText : string

    [<AttributeUsageAttribute(AttributeTargets.All, AllowMultiple = false)>]
    type TypeProviderDefinitionLocationAttribute = 
        inherit System.Attribute
        new : unit -> TypeProviderDefinitionLocationAttribute
        member FilePath : string with get, set
        member Line : int with get, set
        member Column : int with get, set

    [<AttributeUsageAttribute(AttributeTargets.Class ||| AttributeTargets.Interface ||| AttributeTargets.Struct ||| AttributeTargets.Delegate, AllowMultiple = false)>]
    /// <summary>Indicates that a code editor should hide all System.Object methods from the intellisense menus for instances of a provided type</summary>
    type TypeProviderEditorHideMethodsAttribute = 
        inherit System.Attribute
        /// <summary>Creates an instance of the attribute</summary>
        /// <returns>TypeProviderEditorHideMethodsAttribute</returns>
        new : unit -> TypeProviderEditorHideMethodsAttribute

    /// If the class that implements ITypeProvider has a constructor that accepts TypeProviderConfig
    /// then it will be constructed with an instance of TypeProviderConfig.
    type TypeProviderConfig =
        new :  systemRuntimeContainsType : (string -> bool) -> TypeProviderConfig

        /// Get the full path to use to resolve relative paths in any file name arguments given to the type provider instance.
        member ResolutionFolder : string with get,set

        /// Get the full path to referenced assembly that caused this type provider instance to be created.
        member RuntimeAssembly : string with get,set

        /// Get the referenced assemblies for the type provider instance.
        member ReferencedAssemblies : string[] with get,set

        /// Get the full path to use for temporary files for the type provider instance.
        member TemporaryFolder : string with get,set

        /// Indicates if the type provider host responds to invalidation events for type provider instances. 
        member IsInvalidationSupported : bool with get,set

        /// Indicates if the type provider instance is used in an environment which executes provided code such as F# Interactive.
        member IsHostedExecution : bool with get,set

        /// version of referenced system runtime assembly
        member SystemRuntimeAssemblyVersion : System.Version with get,set

        /// Checks if given type exists in target system runtime library
        member SystemRuntimeContainsType : string -> bool

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
        /// Namespace name the provider injects types into.
        abstract NamespaceName : string

        /// The sub-namespaces in this namespace. An optional member to prevent generation of namespaces until an outer namespace is explored.
        abstract GetNestedNamespaces : unit -> IProvidedNamespace[] 

        /// <summary>
        /// The top-level types
        /// </summary>
        /// <returns></returns>
        abstract GetTypes : unit -> Type[] 

        /// <summary>
        /// Compilers call this method to query a type provider for a type <c>name</c>.
        /// </summary>
        /// <remarks>Resolver should return a type called <c>name</c> in namespace <c>NamespaceName</c> or <c>null</c> if the type is unknown.
        /// </remarks>
        /// <returns></returns>
        abstract ResolveTypeName : typeName: string -> Type

    type ITypeProvider =
        inherit System.IDisposable

        /// <summary>
        /// Namespace name the this TypeProvider injects types into.
        /// </summary>
        abstract GetNamespaces : unit -> IProvidedNamespace[] 

        /// <summary>
        /// Get the static parameters for a provided type. 
        /// </summary>
        /// <param name="typeWithoutArguments">A type returned by GetTypes or ResolveTypeName</param>
        /// <returns></returns>
        abstract GetStaticParameters : typeWithoutArguments:Type -> ParameterInfo[] 

        /// <summary>
        /// Apply static arguments to a provided type that accepts static arguments. 
        /// </summary>
        /// <remarks>The provider must return a type with the given mangled name.</remarks>
        /// <param name="typeWithoutArguments">the provided type definition which has static parameters</param>
        /// <param name="typePathWithArguments">the full path of the type, including encoded representations of static parameters</param>
        /// <param name="staticArguments">the static parameters, indexed by name</param>
        /// <returns></returns>
        abstract ApplyStaticArguments : typeWithoutArguments:Type * typePathWithArguments:string[] * staticArguments:obj[] -> Type 

        /// <summary>
        /// Called by the compiler to ask for an Expression tree to replace the given MethodBase with.
        /// </summary>
        /// <param name="syntheticMethodBase">MethodBase that was given to the compiler by a type returned by a GetType(s) call.</param>
        /// <param name="parameters">Expressions that represent the parameters to this call.</param>
        /// <returns>An expression that the compiler will use in place of the given method base.</returns>
        abstract GetInvokerExpression : syntheticMethodBase:MethodBase * parameters:Microsoft.FSharp.Quotations.Expr[] -> Microsoft.FSharp.Quotations.Expr

        /// <summary>
        /// Triggered when an assumption changes that invalidates the resolutions so far reported by the provider
        /// </summary>
        [<CLIEvent>]
        abstract Invalidate : Microsoft.FSharp.Control.IEvent<System.EventHandler, System.EventArgs>

        /// <summary>
        /// Get the physical contents of the given logical provided assembly.
        /// </summary>
        abstract GetGeneratedAssemblyContents : assembly:System.Reflection.Assembly -> byte[]

#if FX_NO_CUSTOMATTRIBUTEDATA
        /// <summary>
        /// Get the custom attribute data for a provided member or type.
        /// </summary>
        abstract GetMemberCustomAttributesData : assembly:System.Reflection.MemberInfo -> System.Collections.Generic.IList<IProvidedCustomAttributeData>
        /// <summary>
        /// Get the custom attribute data for a provided parameter.
        /// </summary>
        abstract GetParameterCustomAttributesData : assembly:System.Reflection.ParameterInfo -> System.Collections.Generic.IList<IProvidedCustomAttributeData>
#endif

#endif
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
