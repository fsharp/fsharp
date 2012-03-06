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


module internal Microsoft.FSharp.Compiler.Nameres

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Import
open Microsoft.FSharp.Compiler.Outcome
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.PrettyNaming



/// A NameResolver primarily holds an InfoReader
type NameResolver =
    new : g:TcGlobals * amap:ImportMap * infoReader:InfoReader * instantiationGenerator:(range -> Typars -> TypeInst) -> NameResolver
    member InfoReader : InfoReader
    member amap : ImportMap
    member g : TcGlobals
    member instantiationGenerator : (range -> Typars -> TypeInst)

//---------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

[<NoEquality; NoComparison; RequireQualifiedAccess>]
type Item = 
  // These exist in the "eUnqualifiedItems" List.map in the type environment. 
  | Value of  ValRef
  | UnionCase of UnionCaseInfo
  | ActivePatternResult of ActivePatternInfo * TType * int  * range
  | ActivePatternCase of ActivePatternElemRef 
  | ExnCase of TyconRef 
  | RecdField of RecdFieldInfo

  // The following are never in the items table but are valid results of binding 
  // an identitifer in different circumstances. 
  | NewDef of Ident
  | ILField of ILFieldInfo
  | Event of EventInfo
  | Property of string * PropInfo list
  | MethodGroup of string * MethInfo list
  | CtorGroup of string * MethInfo list
  | FakeInterfaceCtor of TType
  | DelegateCtor of TType
  | Types of string * TType list
  | ModuleOrNamespaces of Tast.ModuleOrNamespaceRef list
  /// Represents the resolution of a source identifier to an implicit use of an infix operator
  | ImplicitOp of Ident
  /// Represents the resolution of a source identifier to a named argument
  | ArgName of Ident 
  | PropName of Ident
  | UnqualifiedType of TyconRef list

type ExtensionMember = 
   | FSExtMem of ValRef * ExtensionMethodPriority
   | ILExtMem of ILTypeRef * ILMethodDef * ExtensionMethodPriority

[<NoEquality; NoComparison>]
type NameResolutionEnv =
    {eDisplayEnv: DisplayEnv;
     eUnqualifiedItems: NameMap<Item>;
     ePatItems: NameMap<Item>;
     eModulesAndNamespaces: NameMultiMap<ModuleOrNamespaceRef>;
     eFullyQualifiedModulesAndNamespaces: NameMultiMap<ModuleOrNamespaceRef>;
     eFieldLabels: NameMultiMap<RecdFieldRef>;
     eTyconsByAccessNames: NameMultiMap<TyconRef>;
     eFullyQualifiedTyconsByAccessNames: NameMultiMap<TyconRef>;
     eTyconsByDemangledNameAndArity: Map<NameArityPair,TyconRef>;
     eFullyQualifiedTyconsByDemangledNameAndArity: Map<NameArityPair,TyconRef>;
     eExtensionMembers: TyconRefMultiMap<ExtensionMember>;
     eTypars: NameMap<Typar>;}
    static member Empty : g:TcGlobals -> NameResolutionEnv
    member DisplayEnv : DisplayEnv
    member UnqualifiedItems : NameMap<Item>

type FullyQualifiedFlag =
  | FullyQualified
  | OpenQualified

val public DisplayNameOfItem : TcGlobals -> Item -> string

val internal AddFakeNamedValRefToNameEnv : string -> NameResolutionEnv -> ValRef -> NameResolutionEnv
val internal AddFakeNameToNameEnv : string -> NameResolutionEnv -> Item -> NameResolutionEnv

val internal AddValRefToNameEnv                    : NameResolutionEnv -> ValRef -> NameResolutionEnv
val internal AddActivePatternResultTagsToNameEnv   : ActivePatternInfo -> NameResolutionEnv -> TType -> range -> NameResolutionEnv
val internal AddTyconRefsToNameEnv                 : bool -> TcGlobals -> ImportMap -> range -> bool -> NameResolutionEnv -> TyconRef list -> NameResolutionEnv
val internal AddExceptionDeclsToNameEnv            : NameResolutionEnv -> TyconRef -> NameResolutionEnv
val internal AddModuleAbbrevToNameEnv              : Ident -> NameResolutionEnv -> ModuleOrNamespaceRef list -> NameResolutionEnv
val internal AddModuleOrNamespaceRefsToNameEnv                   : TcGlobals -> ImportMap -> range -> bool -> AccessorDomain -> NameResolutionEnv -> ModuleOrNamespaceRef list -> NameResolutionEnv
val internal AddModrefToNameEnv                    : TcGlobals -> ImportMap -> range -> bool -> AccessorDomain -> NameResolutionEnv -> ModuleOrNamespaceRef -> NameResolutionEnv
val internal AddModuleOrNamespaceContentsToNameEnv : TcGlobals -> ImportMap -> AccessorDomain -> range -> NameResolutionEnv -> ModuleOrNamespaceRef -> NameResolutionEnv

type CheckForDuplicateTyparFlag =
  | CheckForDuplicateTypars
  | NoCheckForDuplicateTypars

val internal AddDeclaredTyparsToNameEnv : CheckForDuplicateTyparFlag -> NameResolutionEnv -> Typar list -> NameResolutionEnv
val internal LookupTypeNameInEnvNoArity : FullyQualifiedFlag -> string -> NameResolutionEnv -> TyconRef list

type TypeNameInExprOrPatFlag =
  | ResolveTypeNamesToCtors
  | ResolveTypeNamesToTypeRefs

type TypeNameResInfo = TypeNameInExprOrPatFlag * int option

val internal DefaultTypeNameResInfo : TypeNameResInfo

type internal ItemOccurence = 
  | Binding = 0
  | Use = 1
  | Pattern = 2
  
type ITypecheckResultsSink =
    abstract NotifyEnvWithScope   : range * NameResolutionEnv * AccessorDomain -> unit
    abstract NotifyExprHasType    : pos * TType * DisplayEnv * NameResolutionEnv * AccessorDomain * range -> unit
    abstract NotifyNameResolution : pos * Item * ItemOccurence * DisplayEnv * NameResolutionEnv * AccessorDomain * range -> unit

val internal GlobalTypecheckResultsSink : ITypecheckResultsSink option ref 
val internal CallEnvSink                : range * NameResolutionEnv * AccessorDomain -> unit
val internal CallNameResolutionSink     : range * NameResolutionEnv * Item * ItemOccurence * DisplayEnv * AccessorDomain -> unit
val internal CallExprHasTypeSink        : range * NameResolutionEnv * TType * DisplayEnv * AccessorDomain -> unit

val internal AllPropInfosOfTypeInScope : InfoReader -> TyconRefMultiMap<ExtensionMember> -> string option * AccessorDomain -> FindMemberFlag -> range -> TType -> PropInfo list
val internal AllMethInfosOfTypeInScope : InfoReader -> TyconRefMultiMap<ExtensionMember> -> string option * AccessorDomain -> FindMemberFlag -> range -> TType -> MethInfo list

exception internal IndeterminateType of range
exception internal UpperCaseIdentifierInPattern of range

val FreshenRecdFieldRef :NameResolver -> Range.range -> Tast.RecdFieldRef -> Item

type LookupKind =
  | RecdField
  | Pattern
  | Expr
  | Type
  | Ctor


type WarnOnUpperFlag =
  | WarnOnUpperCase
  | AllIdsOK

type GenerateEstTypeFlag = 
    | Yes of Import.AssemblyLoader //Used to find and/or inject assemblies as needed 
    | No

val internal ResolveLongIndentAsModuleOrNamespace   : FullyQualifiedFlag -> NameResolutionEnv -> AccessorDomain -> Ident list -> ResultOrException<(int * ModuleOrNamespaceRef * ModuleOrNamespaceType) list >
val internal ResolveObjectConstructor               : NameResolver -> DisplayEnv -> range -> AccessorDomain -> TType -> ResultOrException<(Item * 'a list)>
val internal ResolveLongIdentInType                 : NameResolver -> NameResolutionEnv -> LookupKind -> range -> AccessorDomain -> Ident list -> FindMemberFlag -> TypeNameInExprOrPatFlag * int option -> TType -> Item * Ident list
val internal ResolvePatternLongIdent                : NameResolver -> WarnOnUpperFlag -> bool -> range -> AccessorDomain -> NameResolutionEnv -> TypeNameInExprOrPatFlag * int option -> Ident list -> Item
val internal ResolveTypeLongIdentInTyconRef         : NameResolver -> NameResolutionEnv -> TypeNameInExprOrPatFlag * int option -> AccessorDomain -> range -> ModuleOrNamespaceRef -> Ident list -> TyconRef 
val internal ResolveTypeLongIdent                   : NameResolver -> ItemOccurence -> FullyQualifiedFlag -> GenerateEstTypeFlag -> NameResolutionEnv -> AccessorDomain -> Ident list -> int -> ResultOrException<TyconRef>
val internal ResolveField                           : NameResolver -> NameResolutionEnv -> AccessorDomain -> TType -> Ident list * Ident -> RecdFieldRef list
val internal ResolveExprLongIdent                   : NameResolver -> range -> AccessorDomain -> NameResolutionEnv -> TypeNameInExprOrPatFlag * int option -> Ident list -> Item * Ident list
val internal ResolveLongIdentAsExprAndComputeRange  : NameResolver -> range -> AccessorDomain -> NameResolutionEnv -> TypeNameInExprOrPatFlag * int option -> Ident list -> Item * range * Ident list
val internal ResolveExprDotLongIdentAndComputeRange : NameResolver -> range -> AccessorDomain -> NameResolutionEnv -> TType -> Ident list -> FindMemberFlag -> Item * range * Ident list

val FakeInstantiationGenerator : range -> Typar list -> TType list
val ResolvePartialLongIdent : NameResolver -> NameResolutionEnv -> range -> AccessorDomain -> string list -> bool -> Item list
val ResolveCompletionsInType       : NameResolver -> NameResolutionEnv -> Range.range -> Infos.AccessorDomain -> bool -> TType -> Item list
