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

module internal Microsoft.FSharp.Compiler.ConstraintSolver

open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Import
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Infos

val NewAnonTypar : TyparKind * range * TyparRigidity * TyparStaticReq * TyparDynamicReq -> Typar
val NewInferenceType : unit -> TType
val NewErrorType : unit -> TType
val NewErrorMeasure : unit -> MeasureExpr
val NewInferenceTypes : 'a list -> TType list

val FreshenAndFixupTypars : range -> TyparRigidity -> Typars -> TType list -> Typars -> Typars * TyparInst * TType list
val FreshenTypeInst : range -> Typars -> Typars * TyparInst * TType list
val FreshenTypars : range -> Typars -> TType list
val FreshenMethInfo : range -> MethInfo -> TType list

exception ConstraintSolverTupleDiffLengths              of DisplayEnv * TType list * TType list * range * range
exception ConstraintSolverInfiniteTypes                 of DisplayEnv * TType * TType * range * range
exception ConstraintSolverTypesNotInEqualityRelation    of DisplayEnv * TType * TType * range * range
exception ConstraintSolverTypesNotInSubsumptionRelation of DisplayEnv * TType * TType * range * range
exception ConstraintSolverMissingConstraint             of DisplayEnv * Typar * TyparConstraint * range * range
exception ConstraintSolverError                         of string * range * range
exception ConstraintSolverRelatedInformation            of string option * range * exn
exception ErrorFromApplyingDefault                      of TcGlobals * DisplayEnv * Typar * TType * exn * range
exception ErrorFromAddingTypeEquation                   of TcGlobals * DisplayEnv * TType * TType * exn * range
exception ErrorsFromAddingSubsumptionConstraint         of TcGlobals * DisplayEnv * TType * TType * exn * range
exception ErrorFromAddingConstraint                     of DisplayEnv * exn * range
exception UnresolvedConversionOperator                  of DisplayEnv * TType * TType * range
exception UnresolvedOverloading                         of DisplayEnv * exn list * exn list * exn list * string * range
exception PossibleOverload                              of DisplayEnv * string * range
//exception PossibleBestOverload                              of DisplayEnv * string * range
exception NonRigidTypar                                 of DisplayEnv * string option * range * TType * TType * range

[<Sealed>]
type ConstraintSolverState =
    static member New: TcGlobals * Import.ImportMap * InfoReader -> ConstraintSolverState
    
type ConstraintSolverEnv 

val BakedInTraitConstraintNames : string list

val MakeConstraintSolverEnv : ConstraintSolverState -> range -> DisplayEnv -> ConstraintSolverEnv

type Trace = Trace of (unit -> unit) list ref

type OptionalTrace =
  | NoTrace
  | WithTrace of Trace

val SimplifyMeasuresInTypeScheme             : TcGlobals -> bool -> Typars -> TType -> TyparConstraint list -> Typars
val SolveTyparEqualsTyp                      : ConstraintSolverEnv -> int -> range -> OptionalTrace -> TType -> TType -> OperationResult<unit>
val SolveTypEqualsTypKeepAbbrevs             : ConstraintSolverEnv -> int -> range -> OptionalTrace -> TType -> TType -> OperationResult<unit>
val CanonicalizeRelevantMemberConstraints    : ConstraintSolverEnv -> int -> OptionalTrace -> Typars -> OperationResult<unit>
val ResolveOverloading                       : ConstraintSolverEnv -> OptionalTrace -> string -> ndeep: int -> bool -> int * int -> AccessorDomain -> Typrelns.CalledMeth<Expr> list ->  bool -> TType option -> Typrelns.CalledMeth<Expr> option * OperationResult<unit>
val UnifyUniqueOverloading                   : ConstraintSolverEnv -> int * int -> string -> AccessorDomain -> Typrelns.CalledMeth<Expr> list -> TType -> OperationResult<bool> 
val EliminateConstraintsForGeneralizedTypars : ConstraintSolverEnv -> OptionalTrace -> Typars -> unit 

val CheckDeclaredTypars                       : DisplayEnv -> ConstraintSolverState -> range -> Typars -> Typars -> unit 

val AddConstraint                             : ConstraintSolverEnv -> int -> Range.range -> OptionalTrace -> Typar -> TyparConstraint -> OperationResult<unit>
val AddCxTypeEqualsType                       : DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> unit
val AddCxTypeEqualsTypeUndoIfFailed           : DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> bool
val AddCxTypeMustSubsumeType                  : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> TType -> unit
val AddCxTypeMustSubsumeTypeUndoIfFailed      : DisplayEnv -> ConstraintSolverState -> range -> TType -> TType -> bool
val AddCxMethodConstraint                     : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TraitConstraintInfo -> unit
val AddCxTypeMustSupportNull                  : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeMustSupportComparison            : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeMustSupportEquality              : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeMustSupportDefaultCtor           : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeIsReferenceType                  : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeIsValueType                      : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeIsUnmanaged                      : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> unit
val AddCxTypeIsEnum                           : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> TType -> unit
val AddCxTypeIsDelegate                       : DisplayEnv -> ConstraintSolverState -> range -> OptionalTrace -> TType -> TType -> TType -> unit

val CodegenWitnessThatTypSupportsTraitConstraint : TcGlobals -> ImportMap -> range -> TraitConstraintInfo -> OperationResult<(MethInfo * TypeInst) option>

val ChooseTyparSolutionAndSolve : ConstraintSolverState -> DisplayEnv -> Typar -> unit
