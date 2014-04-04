// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

// This file contains a set of helper types and methods for providing types in an implementation 
// of ITypeProvider.
//
// This code is a sample for use in conjunction with the F# 3.0 Developer Preview release of September 2011.


namespace Samples.FSharp.ProvidedTypes

open System
open System.Reflection
open System.Linq.Expressions
open Microsoft.FSharp.Core.CompilerServices

/// Represents an erased provided parameter
type internal ProvidedParameter =
    inherit System.Reflection.ParameterInfo
    new : parameterName: string * parameterType: Type * ?isOut:bool * ?optionalValue:obj -> ProvidedParameter
    member IsParamArray : bool with get,set

/// Represents an erased provided constructor.
type internal ProvidedConstructor =    
    inherit System.Reflection.ConstructorInfo

    /// Create a new provided constructor. It is not initially associated with any specific provided type definition.
    new : parameters: ProvidedParameter list -> ProvidedConstructor

    /// Add a 'System.Obsolete' attribute to this provided constructor
    member AddObsoleteAttribute : message: string * ?isError: bool -> unit    
    
    /// Add XML documentation information to this provided constructor
    member AddXmlDoc          : xmlDoc: string -> unit   
    
    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided constructor, where the documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Set the quotation used to compute the implementation of invocations of this constructor.
    member InvokeCode         : (Quotations.Expr list -> Quotations.Expr) with set

    /// Set the target and arguments of the base constructor call. Only used for generated types.
    member BaseConstructorCall : (Quotations.Expr list -> ConstructorInfo * Quotations.Expr list) with set

    /// Set a flag indicating that the constructor acts like an F# implicit constructor, so the
    /// parameters of the constructor become fields and can be accessed using Expr.GlobalVar with the
    /// same name.
    member IsImplicitCtor : bool with set

    /// Add definition location information to the provided constructor.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit
    
    member IsTypeInitializer : bool with get,set

type internal ProvidedMethod = 
    inherit System.Reflection.MethodInfo

    /// Create a new provided method. It is not initially associated with any specific provided type definition.
    new : methodName:string * parameters: ProvidedParameter list * returnType: Type -> ProvidedMethod

    /// Add XML documentation information to this provided method
    member AddObsoleteAttribute : message: string * ?isError: bool -> unit    

    /// Add XML documentation information to this provided constructor
    member AddXmlDoc            : xmlDoc: string -> unit    

    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    /// The documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   
    
    member AddMethodAttrs       : attributes:MethodAttributes -> unit

    /// Set the method attributes of the method. By default these are simple 'MethodAttributes.Public'
    member SetMethodAttrs       : attributes:MethodAttributes -> unit

    /// Get or set a flag indicating if the property is static.
    member IsStaticMethod       : bool with get, set

    /// Set the quotation used to compute the implementation of invocations of this method.
    member InvokeCode         : (Quotations.Expr list -> Quotations.Expr) with set


    /// Add definition location information to the provided type definition.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit



/// Represents an erased provided property.
type internal ProvidedProperty =
    inherit System.Reflection.PropertyInfo

    /// Create a new provided type. It is not initially associated with any specific provided type definition.
    new  : propertyName: string * propertyType: Type * ?parameters:ProvidedParameter list -> ProvidedProperty

    /// Add a 'System.Obsolete' attribute to this provided property
    member AddObsoleteAttribute : message: string * ?isError: bool -> unit    

    /// Add XML documentation information to this provided constructor
    member AddXmlDoc            : xmlDoc: string -> unit    

    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    /// The documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Get or set a flag indicating if the property is static.
    member IsStatic             : bool with set

    /// Set the quotation used to compute the implementation of gets of this property.
    member GetterCode           : (Quotations.Expr list -> Quotations.Expr) with set

    /// Set the function used to compute the implementation of sets of this property.
    member SetterCode           : (Quotations.Expr list -> Quotations.Expr) with set

    /// Add definition location information to the provided type definition.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit

/// Represents an erased provided property.
type internal ProvidedEvent =
    inherit System.Reflection.EventInfo

    /// Create a new provided type. It is not initially associated with any specific provided type definition.
    new  : propertyName: string * eventHandlerType: Type -> ProvidedEvent

    /// Add XML documentation information to this provided constructor
    member AddXmlDoc            : xmlDoc: string -> unit    

    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    /// The documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Get or set a flag indicating if the property is static.
    member IsStatic             : bool with set

    /// Set the quotation used to compute the implementation of gets of this property.
    member AdderCode           : (Quotations.Expr list -> Quotations.Expr) with set

    /// Set the function used to compute the implementation of sets of this property.
    member RemoverCode           : (Quotations.Expr list -> Quotations.Expr) with set

    /// Add definition location information to the provided type definition.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit

/// Represents an erased provided field.
type internal ProvidedLiteralField =
    inherit System.Reflection.FieldInfo

    /// Create a new provided field. It is not initially associated with any specific provided type definition.
    new  : fieldName: string * fieldType: Type * literalValue: obj -> ProvidedLiteralField

    /// Add a 'System.Obsolete' attribute to this provided field
    member AddObsoleteAttribute : message: string * ?isError: bool -> unit    

    /// Add XML documentation information to this provided field
    member AddXmlDoc            : xmlDoc: string -> unit    

    /// Add XML documentation information to this provided field, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided field, where the computation of the documentation is delayed until necessary
    /// The documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   

    /// Add definition location information to the provided field.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit

/// Represents an erased provided field.
type internal ProvidedField =
    inherit System.Reflection.FieldInfo

    /// Create a new provided field. It is not initially associated with any specific provided type definition.
    new  : fieldName: string * fieldType: Type -> ProvidedField

    /// Add a 'System.Obsolete' attribute to this provided field
    member AddObsoleteAttribute : message: string * ?isError: bool -> unit    

    /// Add XML documentation information to this provided field
    member AddXmlDoc            : xmlDoc: string -> unit    

    /// Add XML documentation information to this provided field, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided field, where the computation of the documentation is delayed until necessary
    /// The documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   

    /// Add definition location information to the provided field definition.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit

    member SetFieldAttributes : attributes : FieldAttributes -> unit

/// Provides symbolic provided types
[<Class>]
type internal ProvidedTypeBuilder =
    /// Like typ.MakeGenericType, but will also work with unit-annotated types
    static member MakeGenericType: genericTypeDefinition: System.Type * genericArguments: System.Type list -> System.Type
    /// Like methodInfo.MakeGenericMethod, but will also work with unit-annotated types and provided types
    static member MakeGenericMethod: genericMethodDefinition: System.Reflection.MethodInfo * genericArguments: System.Type list -> MethodInfo

/// Helps create erased provided unit-of-measure annotations.
[<Class>]
type internal ProvidedMeasureBuilder =
    
    /// The ProvidedMeasureBuilder for building measures.
    static member Default : ProvidedMeasureBuilder

    /// e.g. 1
    member One : System.Type
    /// e.g. m * kg
    member Product : measure1: System.Type * measure1: System.Type  -> System.Type
    /// e.g. 1 / kg
    member Inverse : denominator: System.Type -> System.Type

    /// e.g. kg / m
    member Ratio : numerator: System.Type * denominator: System.Type -> System.Type
    
    /// e.g. m * m 
    member Square : ``measure``: System.Type -> System.Type
    
    /// the SI unit from the F# core library, where the string is in capitals and US spelling, e.g. Meter
    member SI : string -> System.Type
    
    /// e.g. float<kg>, Vector<int, kg>
    member AnnotateType : basic: System.Type * argument: System.Type list -> System.Type


/// Represents a provided static parameter.
type internal ProvidedStaticParameter =
    inherit System.Reflection.ParameterInfo
    new : parameterName: string * parameterType:Type * ?parameterDefaultValue:obj -> ProvidedStaticParameter

    /// Add XML documentation information to this provided constructor
    member AddXmlDoc            : xmlDoc: string -> unit    

    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   

/// Represents a provided type definition.
type internal ProvidedTypeDefinition =
    inherit System.Type

    /// Create a new provided type definition in a namespace. 
    new : assembly: Assembly * namespaceName: string * className: string * baseType: Type option -> ProvidedTypeDefinition

    /// Create a new provided type definition, to be located as a nested type in some type definition.
    new : className : string * baseType: Type option -> ProvidedTypeDefinition

    /// Add the given type as an implemented interface.
    member AddInterfaceImplementation : interfaceType: Type -> unit    

    /// Add the given function as a set of on-demand computed interfaces.
    member AddInterfaceImplementationsDelayed : interfacesFunction:(unit -> Type  list)-> unit    

    /// Specifies that the given method body implements the given method declaration.
    member DefineMethodOverride : methodInfoBody: ProvidedMethod * methodInfoDeclaration: MethodInfo -> unit

    /// Add a 'System.Obsolete' attribute to this provided type definition
    member AddObsoleteAttribute : message: string * ?isError: bool -> unit    

    /// Add XML documentation information to this provided constructor
    member AddXmlDoc             : xmlDoc: string -> unit    

    /// Set the base type
    member SetBaseType             : Type -> unit    

    /// Set the base type to a lazily evaluated value
    member SetBaseTypeDelayed      : Lazy<Type option> -> unit    

    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary.
    /// The documentation is only computed once.
    member AddXmlDocDelayed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary
    /// The documentation is re-computed  every time it is required.
    member AddXmlDocComputed   : xmlDocFunction: (unit -> string) -> unit   
    
    /// Set the attributes on the provided type. This fully replaces the default TypeAttributes.
    member SetAttributes        : System.Reflection.TypeAttributes -> unit
    
    /// Reset the enclosing type (for generated nested types)
    member ResetEnclosingType: enclosingType:System.Type -> unit
    
    /// Add a method, property, nested type or other member to a ProvidedTypeDefinition
    member AddMember         : memberInfo:MemberInfo      -> unit  
    /// Add a set of members to a ProvidedTypeDefinition
    member AddMembers        : memberInfos:list<#MemberInfo> -> unit

    /// Add a member to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    member AddMemberDelayed  : memberFunction:(unit -> #MemberInfo)      -> unit

    /// Add a set of members to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    member AddMembersDelayed : (unit -> list<#MemberInfo>) -> unit    
    
    /// Add the types of the generated assembly as generative types, where types in namespaces get hierarchically positioned as nested types.
    member AddAssemblyTypesAsNestedTypesDelayed : assemblyFunction:(unit -> System.Reflection.Assembly) -> unit

    // Parametric types
    member DefineStaticParameters     : parameters: ProvidedStaticParameter list * instantiationFunction: (string -> obj[] -> ProvidedTypeDefinition) -> unit

    /// Add definition location information to the provided type definition.
    member AddDefinitionLocation : line:int * column:int * filePath:string -> unit

    /// Suppress System.Object entries in intellisense menus in instances of this provided type 
    member HideObjectMethods  : bool with set

    /// Get or set a flag indicating if the ProvidedTypeDefinition is erased
    member IsErased : bool  with get,set

    /// Get or set a flag indicating if the ProvidedTypeDefinition has type-relocation suppressed
    [<Experimental("SuppressRelocation is a workaround and likely to be removed")>]
    member SuppressRelocation : bool  with get,set

/// A provided generated assembly
type ProvidedAssembly =
    new : assemblyFileName:string -> ProvidedAssembly
    /// <summary>
    /// Emit the given provided type definitions as part of the assembly 
    /// and adjust the 'Assembly' property of all provided type definitions to return that
    /// assembly.
    ///
    /// The assembly is only emitted when the Assembly property on the root type is accessed for the first time.
    /// The host F# compiler does this when processing a generative type declaration for the type.
    /// </summary>
    /// <param name="enclosingTypeNames">An optional path of type names to wrap the generated types. The generated types are then generated as nested types.</param>
    member AddTypes : types : ProvidedTypeDefinition list -> unit
    member AddNestedTypes : types : ProvidedTypeDefinition list * enclosingGeneratedTypeNames: string list -> unit

#if FX_NO_LOCAL_FILESYSTEM
#else
    /// Register that a given file is a provided generated assembly
    static member RegisterGenerated : fileName:string -> Assembly
#endif


/// A base type providing default implementations of type provider functionality when all provided 
/// types are of type ProvidedTypeDefinition.
type TypeProviderForNamespaces =

    /// Initializes a type provider to provide the types in the given namespace.
    internal new : namespaceName:string * types: ProvidedTypeDefinition list -> TypeProviderForNamespaces

    /// Initializes a type provider 
    internal new : unit -> TypeProviderForNamespaces

    /// Add a namespace of provided types.
    member internal AddNamespace : namespaceName:string * types: ProvidedTypeDefinition list -> unit

    /// Invalidate the information provided by the provider
    member Invalidate : unit -> unit

#if FX_NO_LOCAL_FILESYSTEM
#else
    /// AssemblyResolve handler. Default implementation searches <assemblyname>.dll file in registered folders 
    abstract ResolveAssembly : System.ResolveEventArgs -> Assembly
    default ResolveAssembly : System.ResolveEventArgs -> Assembly

    /// Registers custom probing path that can be used for probing assemblies
    member RegisterProbingFolder : folder : string -> unit
    /// Registers location of RuntimeAssembly (from TypeProviderConfig) as probing folder
    member RegisterRuntimeAssemblyLocationAsProbingFolder : cfg : Core.CompilerServices.TypeProviderConfig -> unit
#endif

    interface ITypeProvider
