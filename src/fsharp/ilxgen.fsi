//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

module internal Microsoft.FSharp.Compiler.Ilxgen

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Tast
open System
open System.IO
open System.Reflection

/// Indicates how the generated IL code is ultimately emitted 
type IlxGenBackend =
    | IlWriteBackend
    | IlReflectBackend

[<NoEquality; NoComparison>]
type internal IlxGenOptions = 
    { fragName                               : string
      generateFilterBlocks                   : bool
      workAroundReflectionEmitBugs           : bool
      emitConstantArraysUsingStaticDataBlobs : bool
      /// If this is set, then the last module becomes the "main" module 
      mainMethodInfo                         : Attribs option
      localOptimizationsAreOn                : bool
      generateDebugSymbols                   : bool
      testFlagEmitFeeFeeAs100001             : bool
      ilxBackend                             : IlxGenBackend
      /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
      /// This includes all interactively compiled code, including #load, definitions, and expressions
      isInteractive                          : bool 
      // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
      // storage, even though 'it' is not logically mutable
      isInteractiveItExpr                    : bool
      // Indicates System.SerializableAttribute is available in the targeting framework
      netFxHasSerializableAttribute          : bool
      /// Indicates that, whenever possible, use callvirt instead of call
      alwaysCallVirt                         : bool}

/// The results of the ILX compilation of one fragment of an assembly
type public IlxGenResults = 
    { /// The generated IL/ILX type definitions
      ilTypeDefs             : ILTypeDef list
      /// The generated IL/ILX assembly attributes
      ilAssemAttrs           : ILAttribute list
      /// The generated IL/ILX .NET module attributes
      ilNetModuleAttrs       : ILAttribute list
      /// The generated IL/ILX resources associated with F# quotations
      quotationResourceBytes : byte[]  list }

  
/// Used to support the compilation-inversion operations "ClearGeneratedValue" and "LookupGeneratedValue"
type ExecutionContext =
    { LookupFieldRef  : (ILFieldRef -> FieldInfo)
      LookupMethodRef : (ILMethodRef -> MethodInfo)
      LookupTypeRef   : (ILTypeRef -> Type)
      LookupType      : (ILType -> Type) } 

/// An incremental ILX code generator for a single assembly
type public IlxAssemblyGenerator =
    /// Create an incremental ILX code generator for a single assembly
    new : Import.ImportMap * Env.TcGlobals * ConstraintSolver.TcValF * CcuThunk -> IlxAssemblyGenerator 
    
    /// Register a set of referenced assemblies with the ILX code generator
    member AddExternalCcus : CcuThunk list -> unit

    /// Register a fragment of the current assembly with the ILX code generator. If 'isIncrementalFragment' is true then the input
    /// is assumed to be a fragment 'typed' into FSI.EXE, otherwise the input is assumed to be the result of a '#load'
    member AddIncrementalLocalAssemblyFragment : isIncrementalFragment: bool * fragName:string * typedAssembly: TypedAssembly -> unit

    /// Generate ILX code for an assembly fragment
    member GenerateCode : IlxGenOptions * TypedAssembly * Attribs * Attribs -> IlxGenResults

    /// Create the CAS permission sets for an assembly fragment
    member CreatePermissionSets : Attrib list ->  ILPermission list

    /// Invert the compilation of the given value and clear the storage of the value
    member ClearGeneratedValue : ExecutionContext * Val -> unit

    /// Invert the compilation of the given value and return its current dynamic value and its compiled System.Type
    member LookupGeneratedValue : ExecutionContext * Val -> (obj * System.Type) option


val ReportStatistics : TextWriter -> unit
