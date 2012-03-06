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


module internal Microsoft.FSharp.Compiler.TypeChecker

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler 

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Infos

open System.Collections.Generic

[<Sealed>]
type TcEnv =
    member DisplayEnv : DisplayEnv
    member NameEnv : Nameres.NameResolutionEnv

(* Incremental construction of environments, e.g. for F# Interactive *)
val internal CreateInitialTcEnv : Env.TcGlobals * Import.ImportMap * range * (CcuThunk * string list * bool) list -> TcEnv 
val internal AddCcuToTcEnv      : Env.TcGlobals * Import.ImportMap * range * TcEnv * CcuThunk * autoOpens: string list * bool -> TcEnv 
val internal AddLocalRootModuleOrNamespace : Env.TcGlobals -> Import.ImportMap -> range -> TcEnv -> ModuleOrNamespaceType -> TcEnv
val internal TcOpenDecl         : Env.TcGlobals -> Import.ImportMap -> range -> range -> TcEnv -> Ast.LongIdent -> TcEnv 

type TopAttribs =
    { mainMethodAttrs : Attribs;
      netModuleAttrs  : Attribs;
      assemblyAttrs   : Attribs  }

type ConditionalDefines = 
    string list

val internal EmptyTopAttrs : TopAttribs
val internal CombineTopAttrs : TopAttribs -> TopAttribs -> TopAttribs

val internal TypecheckOneImplFile : 
      Env.TcGlobals * NiceNameGenerator * Import.ImportMap * CcuThunk * (unit -> bool) * ConditionalDefines * bool
      -> TcEnv 
      -> Tast.ModuleOrNamespaceType option
      -> ImplFile
      -> Eventually<TopAttribs * Tast.TypedImplFile * TcEnv>

val internal TypecheckOneSigFile : 
      Env.TcGlobals * NiceNameGenerator * Import.ImportMap * CcuThunk  * (unit -> bool) * ConditionalDefines * bool
      -> TcEnv                             
      -> SigFile
      -> Eventually<TcEnv * TcEnv * ModuleOrNamespaceType >

//-------------------------------------------------------------------------
// exceptions arising from type checking 
//------------------------------------------------------------------------- 

exception internal BakedInMemberConstraintName of string * range
exception internal FunctionExpected of DisplayEnv * TType * range
exception internal NotAFunction of DisplayEnv * TType * range * range
exception internal Recursion of DisplayEnv * Ast.Ident * TType * TType * range
exception internal RecursiveUseCheckedAtRuntime of DisplayEnv * ValRef * range
exception internal LetRecEvaluatedOutOfOrder of DisplayEnv * ValRef * ValRef * range
exception internal LetRecCheckedAtRuntime of range
exception internal LetRecUnsound of DisplayEnv * ValRef list * range
exception internal TyconBadArgs of DisplayEnv * TyconRef * int * range
exception internal UnionCaseWrongArguments of DisplayEnv * int * int * range
exception internal UnionCaseWrongNumberOfArgs of DisplayEnv * int * int * range
exception internal FieldsFromDifferentTypes of DisplayEnv * RecdFieldRef * RecdFieldRef * range
exception internal FieldGivenTwice of DisplayEnv * RecdFieldRef * range
exception internal MissingFields of string list * range
exception internal UnitTypeExpected of DisplayEnv * TType * bool * range
exception internal FunctionValueUnexpected of DisplayEnv * TType * range
exception internal UnionPatternsBindDifferentNames of range
exception internal VarBoundTwice of Ast.Ident
exception internal ValueRestriction of DisplayEnv * bool * Val * Typar * range
exception internal FieldNotMutable of DisplayEnv * RecdFieldRef * range
exception internal ValNotMutable of DisplayEnv * ValRef * range
exception internal ValNotLocal of DisplayEnv * ValRef * range
exception internal InvalidRuntimeCoercion of DisplayEnv * TType * TType * range
exception internal IndeterminateRuntimeCoercion of DisplayEnv * TType * TType * range
exception internal IndeterminateStaticCoercion of DisplayEnv * TType * TType * range
exception internal StaticCoercionShouldUseBox of DisplayEnv * TType * TType * range
exception internal RuntimeCoercionSourceSealed of DisplayEnv * TType * range
exception internal CoercionTargetSealed of DisplayEnv * TType * range
exception internal UpcastUnnecessary of range
exception internal TypeTestUnnecessary of range
exception internal SelfRefObjCtor of bool * range
exception internal VirtualAugmentationOnNullValuedType of range
exception internal NonVirtualAugmentationOnNullValuedType of range
exception internal UseOfAddressOfOperator of range
exception internal DeprecatedThreadStaticBindingWarning of range
exception internal NotUpperCaseConstructor of range
exception internal IntfImplInIntrinsicAugmentation of range
exception internal IntfImplInExtrinsicAugmentation of range
exception internal OverrideInIntrinsicAugmentation of range
exception internal OverrideInExtrinsicAugmentation of range
exception internal NonUniqueInferredAbstractSlot of Env.TcGlobals * DisplayEnv * string * MethInfo * MethInfo * range
exception internal StandardOperatorRedefinitionWarning of string * range
exception internal ParameterlessStructCtor of range

val IsSecurityAttribute : Env.TcGlobals -> Import.ImportMap -> Dictionary<Stamp,bool> -> Tast.Attrib -> Range.range -> bool
val IsSecurityCriticalAttribute : Env.TcGlobals -> Tast.Attrib -> bool
val internal TcFieldInit : range -> ILFieldInit -> Tast.Const


