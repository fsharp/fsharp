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

module internal Microsoft.FSharp.Compiler.Ilxgen

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open System
open System.IO  
open System.Reflection
open Microsoft.FSharp.Compiler 

type IlxBackend =
|   IlWriteBackend
|   IlReflectBackend

[<NoEquality; NoComparison>]
type internal cenv = 
 { g: Env.TcGlobals;
   viewCcu: Tast.CcuThunk;
   fragName: string;
   generateFilterBlocks : bool;
   workAroundReflectionEmitBugs: bool;
   emitConstantArraysUsingStaticDataBlobs: bool;
   amap: Import.ImportMap;
   mainMethodInfo: Tast.Attribs option;
   localOptimizationsAreOn: bool;
   generateDebugSymbols: bool;
   testFlagEmitFeeFeeAs100001 : bool;
   ilxBackend : IlxBackend;
   isInteractive: bool; 
   isInteractiveItExpr: bool}

type public  IlxGenEnv 
val internal GetEmptyIlxGenEnv : Tast.CcuThunk -> IlxGenEnv 
val public AddExternalCcusToIlxGenEnv : Import.ImportMap -> Env.TcGlobals -> IlxGenEnv -> Tast.CcuThunk list -> IlxGenEnv 
val public AddIncrementalLocalAssmblyFragmentToIlxGenEnv : Import.ImportMap -> bool -> Env.TcGlobals -> Tast.CcuThunk -> string -> IlxGenEnv -> Tast.TypedAssembly -> IlxGenEnv 

type public CodegenResults = 
    { ilTypeDefs             : ILTypeDef list;
      ilAssemAttrs           : ILAttribute list;
      ilNetModuleAttrs       : ILAttribute list;
      quotationResourceBytes : byte[]  list }

val internal GenerateCode : 
   cenv 
   -> IlxGenEnv 
   -> Tast.TypedAssembly 
   -> Tast.Attribs * Tast.Attribs 
   -> CodegenResults


val ReportStatistics : TextWriter -> unit
  
type ExecutionContext =
    { LookupFieldRef : (ILFieldRef -> FieldInfo);
      LookupMethodRef : (ILMethodRef -> MethodInfo)
      LookupTypeRef : (ILTypeRef -> Type);
      LookupType : (ILType -> Type) } 

val ClearGeneratedValue : ExecutionContext -> Env.TcGlobals -> IlxGenEnv -> Tast.Val -> unit

val LookupGeneratedValue : Import.ImportMap -> ExecutionContext -> Env.TcGlobals -> IlxGenEnv -> Tast.Val -> (obj * System.Type) option

val IsSecurityAttribute : Env.TcGlobals -> Import.ImportMap -> Tast.Attrib -> Range.range -> bool
val CreatePermissionSets : Env.TcGlobals ->   Import.ImportMap -> IlxGenEnv -> Tast.Attrib list ->  ILPermission list

//val LookupGeneratedInfo  : ExecutionContext -> Env.TcGlobals -> IlxGenEnv -> Tast.Val -> System.Reflection.MemberInfo option
