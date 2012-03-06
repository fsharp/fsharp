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


/// Derived expression manipulation and construction functions.
module internal Microsoft.FSharp.Compiler.Tastops 

open System.Text
open System.Collections.Generic
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Lib

//-------------------------------------------------------------------------
// Type equivalence
//------------------------------------------------------------------------- 

type Erasure = EraseAll | EraseMeasures | EraseNone

val typeEquivAux    : Erasure -> TcGlobals  -> TType          -> TType         -> bool
val typeEquiv       :            TcGlobals  -> TType          -> TType         -> bool
val measureEquiv    :            TcGlobals  -> MeasureExpr  -> MeasureExpr -> bool

//-------------------------------------------------------------------------
// Build common types
//------------------------------------------------------------------------- 

val mkFunTy : TType -> TType -> TType
val ( --> ) : TType -> TType -> TType
val tryMkForallTy : Typars -> TType -> TType
val ( +-> ) : Typars -> TType -> TType
val mkTupleTy : TTypes -> TType
val mkIteratedFunTy : TTypes -> TType -> TType
val typeOfLambdaArg : range -> Val list -> TType
val mkMultiLambdaTy : range -> Val list -> TType -> TType
val mkLambdaTy : Typars -> TTypes -> TType -> TType

//-------------------------------------------------------------------------
// Module publication, used while compiling fslib.
//------------------------------------------------------------------------- 

val ensureCcuHasModuleOrNamespaceAtPath : CcuThunk -> Ident list -> CompilationPath -> XmlDoc -> unit 

//-------------------------------------------------------------------------
// Miscellaneous accessors on terms
//------------------------------------------------------------------------- 

val stripExpr : Expr -> Expr

val valsOfBinds : Bindings -> FlatVals 

//-------------------------------------------------------------------------
// Build decision trees imperatively
//------------------------------------------------------------------------- 

type MatchBuilder =
    new : SequencePointInfoForBinding * range -> MatchBuilder
    member AddTarget : DecisionTreeTarget -> int
    member AddResultTarget : Expr * SequencePointInfoForTarget -> DecisionTree
    member CloseTargets : unit -> DecisionTreeTarget list
    member Close : DecisionTree * range * TType -> Expr

//-------------------------------------------------------------------------
// Make some special decision graphs
//------------------------------------------------------------------------- 

val mkBoolSwitch : range -> Expr -> DecisionTree -> DecisionTree -> DecisionTree
val primMkCond : SequencePointInfoForBinding -> SequencePointInfoForTarget -> SequencePointInfoForTarget -> range -> TType -> Expr -> Expr -> Expr -> Expr
val mkCond : SequencePointInfoForBinding -> SequencePointInfoForTarget -> range -> TType -> Expr -> Expr -> Expr -> Expr
val mkNonNullCond : TcGlobals -> range -> TType -> Expr -> Expr -> Expr -> Expr
val mkIfThen : TcGlobals -> range -> Expr -> Expr -> Expr 

//-------------------------------------------------------------------------
// Generate new locals
//------------------------------------------------------------------------- 

/// Note: try to use exprForValRef or the expression returned from mkLocal instead of this. 
val exprForVal : range -> Val -> Expr
val exprForValRef : range -> ValRef -> Expr

/// Return the local and an expression to reference it
val mkLocal : range -> string -> TType -> Val * Expr
val mkCompGenLocal : range -> string -> TType -> Val * Expr
val mkMutableCompGenLocal : range -> string -> TType -> Val * Expr
val mkCompGenLocalAndInvisbleBind : TcGlobals -> string -> range -> Expr -> Val * Expr * Binding 

//-------------------------------------------------------------------------
// Make lambdas
//------------------------------------------------------------------------- 

val mkMultiLambda : range -> Val list -> Expr * TType -> Expr
val rebuildLambda : range -> Val option -> Val option -> Val list -> Expr * TType -> Expr
val mkLambda : range -> Val -> Expr * TType -> Expr
val mkTypeLambda : range -> Typars -> Expr * TType -> Expr
val mkObjExpr : TType * Val option * Expr * ObjExprMethod list * (TType * ObjExprMethod list) list * Range.range -> Expr
val mkTypeChoose : range -> Typars -> Expr -> Expr
val mkLambdas : range -> Typars -> Val list -> Expr * TType -> Expr
val mkMultiLambdasCore : range -> Val list list -> Expr * TType -> Expr * TType
val mkMultiLambdas : range -> Typars -> Val list list -> Expr * TType -> Expr
val mkMemberLambdas : range -> Typars -> Val option -> Val option -> Val list list -> Expr * TType -> Expr

val mkWhile      : TcGlobals -> SequencePointInfoForWhileLoop * SpecialWhileLoopMarker * Expr * Expr * range                          -> Expr
val mkFor        : TcGlobals -> SequencePointInfoForForLoop * Val * Expr * ForLoopStyle * Expr * Expr * range -> Expr
val mkTryWith  : TcGlobals -> Expr * Val * Expr * Val * Expr * range * TType * SequencePointInfoForTry * SequencePointInfoForWith -> Expr
val mkTryFinally: TcGlobals -> Expr * Expr * range * TType * SequencePointInfoForTry * SequencePointInfoForFinally -> Expr

//-------------------------------------------------------------------------
// Make let/letrec
//------------------------------------------------------------------------- 
 

// Generate a user-level let-bindings
val mkBind : SequencePointInfoForBinding -> Val -> Expr -> Binding
val mkLetBind : range -> Binding -> Expr -> Expr
val mkLetsBind : range -> Binding list -> Expr -> Expr
val mkLetsFromBindings : range -> Bindings -> Expr -> Expr
val mkLet : SequencePointInfoForBinding -> range -> Val -> Expr -> Expr -> Expr
val mkMultiLambdaBind : Val -> SequencePointInfoForBinding -> range -> Typars -> Val list list -> Expr * TType -> Binding

// Compiler generated bindings may involve a user variable.
// Compiler generated bindings may give rise to a sequence point if they are part of
// an SPAlways expression. Compiler generated bindings can arise from for example, inlining.
val mkCompGenBind : Val -> Expr -> Binding
val mkCompGenBinds : Val list -> Exprs -> Bindings
val mkCompGenLet : range -> Val -> Expr -> Expr -> Expr

// Invisible bindings are never given a sequence point and should never have side effects
val mkInvisibleLet : range -> Val -> Expr -> Expr -> Expr
val mkInvisibleBind : Val -> Expr -> Binding
val mkInvisibleFlatBindings : FlatVals -> FlatExprs -> Bindings
val mkLetRecBinds : range -> Bindings -> Expr -> Expr
 
//-------------------------------------------------------------------------
// Generalization/inference helpers
//------------------------------------------------------------------------- 
 
/// TypeSchme (generalizedTypars, tauTy)
///
///    generalizedTypars -- the truly generalized type parameters 
///    tauTy  --  the body of the generalized type. A 'tau' type is one with its type paramaeters stripped off.
type TypeScheme = TypeScheme of Typars  * TType    

val mkGenericBindRhs : TcGlobals -> range -> Typars -> TypeScheme -> Expr -> Expr
val isBeingGeneralized : Typar -> TypeScheme -> bool

//-------------------------------------------------------------------------
// Make lazy and/or
//------------------------------------------------------------------------- 

val mkLazyAnd  : TcGlobals -> range -> Expr -> Expr -> Expr
val mkLazyOr   : TcGlobals -> range -> Expr -> Expr -> Expr
val mkByrefTy  : TcGlobals -> TType -> TType

//-------------------------------------------------------------------------
// Make construction operations
//------------------------------------------------------------------------- 

val mkUnionCaseExpr : UnionCaseRef * TypeInst * Exprs * range -> Expr
val mkExnExpr : TyconRef * Exprs * range -> Expr
val mkAsmExpr : ILInstr list * TypeInst * Exprs * TTypes * range -> Expr
val mkCoerceExpr : Expr * TType * range * TType -> Expr
val mkReraise : range -> TType -> Expr
val mkReraiseLibCall : TcGlobals -> TType -> range -> Expr


//-------------------------------------------------------------------------
// Make projection operations
//------------------------------------------------------------------------- 
 
val mkTupleFieldGet                : Expr                  * TypeInst * int         * range -> Expr
val mkRecdFieldGetViaExprAddr      : Expr * RecdFieldRef   * TypeInst               * range -> Expr
val mkRecdFieldGetAddrViaExprAddr  : Expr * RecdFieldRef   * TypeInst               * range -> Expr
val mkStaticRecdFieldGet           :        RecdFieldRef   * TypeInst               * range -> Expr
val mkStaticRecdFieldSet           :        RecdFieldRef   * TypeInst * Expr        * range -> Expr
val mkStaticRecdFieldGetAddr       :        RecdFieldRef   * TypeInst               * range -> Expr
val mkRecdFieldSetViaExprAddr      : Expr * RecdFieldRef   * TypeInst * Expr        * range -> Expr
val mkUnionCaseTagGet              : Expr * TyconRef       * TypeInst               * range -> Expr
val mkUnionCaseProof               : Expr * UnionCaseRef   * TypeInst               * range -> Expr
val mkUnionCaseFieldGetProven      : Expr * UnionCaseRef   * TypeInst * int         * range -> Expr
val mkUnionCaseFieldGetUnproven    : Expr * UnionCaseRef   * TypeInst * int         * range -> Expr
val mkExnCaseFieldGet              : Expr * TyconRef               * int         * range -> Expr
val mkUnionCaseFieldSet            : Expr * UnionCaseRef   * TypeInst * int  * Expr * range -> Expr
val mkExnCaseFieldSet              : Expr * TyconRef               * int  * Expr * range -> Expr

//-------------------------------------------------------------------------
// Compiled view of tuples
//------------------------------------------------------------------------- 
 
val maxTuple : int
val goodTupleFields : int
val mkCompiledTupleTyconRef : TcGlobals -> 'a list -> TyconRef
val mkCompiledTupleTy : TcGlobals -> TTypes -> TType
val mkCompiledTuple : TcGlobals -> TTypes * Exprs * range -> TyconRef * TTypes * Exprs * range
val mkGetTupleItemN : TcGlobals -> range -> int -> ILType -> Expr -> TType -> Expr

//-------------------------------------------------------------------------
// Take the address of an expression, or force it into a mutable local. Any allocated
// mutable local may need to be kept alive over a larger expression, hence we return
// a wrapping function that wraps "let mutable loc = Expr in ..." around a larger
// expression.
//------------------------------------------------------------------------- 

exception DefensiveCopyWarning of string * range 
type Mutates = DefinitelyMutates | PossiblyMutates | NeverMutates
val mkExprAddrOfExpr : TcGlobals -> bool -> bool -> Mutates -> Expr -> range -> (Expr -> Expr) * Expr

//-------------------------------------------------------------------------
// Tables keyed on values and/or type parameters
//------------------------------------------------------------------------- 

/// Maps Val to T, based on stamps
[<Struct;NoEquality; NoComparison>]
type ValMap<'T> = 
    member Contents : StampMap<'T>
    member Item : Val -> 'T with get
    member TryFind : Val -> 'T option
    member ContainsVal : Val -> bool
    member Add : Val -> 'T -> ValMap<'T>
    member Remove : Val -> ValMap<'T>
    member IsEmpty : bool
    static member Empty : ValMap<'T>
    static member OfList : (Val * 'T) list -> ValMap<'T>

/// Mutable data structure mapping Val's to T based on stamp keys
[<Sealed; NoEquality; NoComparison>]
type ValHash<'T> =
    member Values : seq<'T>
    member TryFind : Val -> 'T option
    member Add : Val * 'T -> unit
    static member Create : unit -> ValHash<'T>


/// Maps Val's to list of T based on stamp keys
[<Struct; NoEquality; NoComparison>]
type ValMultiMap<'T> =
    member Find : Val -> 'T list
    member Add : Val * 'T -> ValMultiMap<'T>
    member Remove : Val -> ValMultiMap<'T>
    member Contents : StampMap<'T list>
    static member Empty : ValMultiMap<'T>

[<Sealed>]
/// Maps Typar to T based on stamp keys
type TyparMap<'T>  =
    member Item : Typar -> 'T with get
    member ContainsKey : Typar -> bool
    member Add : Typar * 'T -> TyparMap<'T> 
    static member Empty : TyparMap<'T> 

[<NoEquality; NoComparison;Sealed>]
/// Maps TyconRef to T based on stamp keys
type TyconRefMap<'T> =
    member Item : TyconRef -> 'T with get
    member TryFind : TyconRef -> 'T option
    member ContainsKey : TyconRef -> bool
    member Add : TyconRef -> 'T -> TyconRefMap<'T>
    member Remove : TyconRef -> TyconRefMap<'T>
    member IsEmpty : bool
    static member Empty : TyconRefMap<'T>
    static member OfList : (TyconRef * 'T) list -> TyconRefMap<'T>

/// Maps TyconRef to list of T based on stamp keys
[<Struct; NoEquality; NoComparison>]
type TyconRefMultiMap<'T> =
    member Find : TyconRef -> 'T list
    member Add : TyconRef * 'T -> TyconRefMultiMap<'T>
    static member Empty : TyconRefMultiMap<'T>


//-------------------------------------------------------------------------
// Orderings on Tycon, Val, RecdFieldRef, Typar
//------------------------------------------------------------------------- 

val valOrder          : IComparer<Val>
val tyconOrder        : IComparer<Tycon>
val recdFieldRefOrder : IComparer<RecdFieldRef>
val typarOrder        : IComparer<Typar>

//-------------------------------------------------------------------------
// Equality on Tycon and Val
//------------------------------------------------------------------------- 

val tyconRefEq : TcGlobals -> TyconRef -> TyconRef -> bool
val valRefEq : TcGlobals -> ValRef -> ValRef -> bool

//-------------------------------------------------------------------------
// Operations on types: substitution
//------------------------------------------------------------------------- 

type TyparInst = (Typar * TType) list

type TyconRefRemap = TyconRefMap<TyconRef>
type ValRemap = ValMap<ValRef>

[<NoEquality; NoComparison>]
type Remap =
    { tpinst : TyparInst;
      valRemap: ValRemap;
      tyconRefRemap : TyconRefRemap }

    static member Empty : Remap

val addTyconRefRemap : TyconRef -> TyconRef -> Remap -> Remap
val addValRemap : Val -> Val -> Remap -> Remap


val mkTyparInst : Typars -> TTypes -> TyparInst
val mkTyconRefInst : TyconRef -> TypeInst -> TyparInst
val emptyTyparInst : TyparInst

val instType               : TyparInst -> TType -> TType
val instTypes              : TyparInst -> TypeInst -> TypeInst
val instTyparConstraints  : TyparInst -> TyparConstraint list -> TyparConstraint list 
val instTrait              : TyparInst -> TraitConstraintInfo -> TraitConstraintInfo 

//-------------------------------------------------------------------------
// From typars to types 
//------------------------------------------------------------------------- 

val generalizeTypars : Typars -> TypeInst
val generalizeTyconRef : TyconRef -> TTypes * TType
val generalizedTyconRef : TyconRef -> TType
val mkTyparToTyparRenaming : Typars -> Typars -> TyparInst * TTypes

//-------------------------------------------------------------------------
// See through typar equations from inference and/or type abbreviation equations.
//------------------------------------------------------------------------- 

val reduceTyconRefAbbrev : TyconRef -> TypeInst -> TType
val reduceTyconRefMeasureable : TyconRef -> TypeInst -> TType
val reduceTyconRefAbbrevMeasureable : TyconRef -> MeasureExpr

/// set bool to 'true' to allow shortcutting of type parameter equation chains during stripping 
val stripTyEqnsA : TcGlobals -> bool -> TType -> TType 
val stripTyEqns : TcGlobals -> TType -> TType
val stripTyEqnsAndMeasureEqns : TcGlobals -> TType -> TType

val tryNormalizeMeasureInType : TcGlobals -> TType -> TType

//-------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

/// See through F# exception abbreviations
val stripExnEqns : TyconRef -> Tycon
val recdFieldsOfExnDefRef : TyconRef -> RecdField list
val recdFieldTysOfExnDefRef : TyconRef -> TType list

//-------------------------------------------------------------------------
// Analyze types.  These all look through type abbreviations and 
// inference equations, i.e. are "stripped"
//------------------------------------------------------------------------- 

val destForallTy     : TcGlobals -> TType -> Typars * TType
val destFunTy        : TcGlobals -> TType -> TType * TType
val destTupleTy      : TcGlobals -> TType -> TTypes
val destTyparTy      : TcGlobals -> TType -> Typar
val destAnyParTy     : TcGlobals -> TType -> Typar
val destMeasureTy    : TcGlobals -> TType -> MeasureExpr
val tryDestForallTy  : TcGlobals -> TType -> Typars * TType

val isFunTy            : TcGlobals -> TType -> bool
val isForallTy         : TcGlobals -> TType -> bool
val isTupleTy          : TcGlobals -> TType -> bool
val isTupleStructTy    : TcGlobals -> TType -> bool
val isUnionTy          : TcGlobals -> TType -> bool
val isReprHiddenTy     : TcGlobals -> TType -> bool
val isFSharpObjModelTy : TcGlobals -> TType -> bool
val isRecdTy           : TcGlobals -> TType -> bool
val isTyparTy          : TcGlobals -> TType -> bool
val isAnyParTy         : TcGlobals -> TType -> bool
val isMeasureTy        : TcGlobals -> TType -> bool

val mkAppTy : TyconRef -> TypeInst -> TType

val mkProvenUnionCaseTy : UnionCaseRef -> TypeInst -> TType
val isProvenUnionCaseTy : TType -> bool

val isAppTy        : TcGlobals -> TType -> bool
val destAppTy      : TcGlobals -> TType -> TyconRef * TypeInst
val tcrefOfAppTy   : TcGlobals -> TType -> TyconRef
val tyconOfAppTy   : TcGlobals -> TType -> Tycon
val tryDestAppTy   : TcGlobals -> TType -> TyconRef option
val argsOfAppTy    : TcGlobals -> TType -> TypeInst
val mkInstForAppTy  : TcGlobals -> TType -> TyparInst

val domainOfFunTy  : TcGlobals -> TType -> TType
val rangeOfFunTy   : TcGlobals -> TType -> TType
val stripFunTy     : TcGlobals -> TType -> TType list * TType
val stripFunTyN    : TcGlobals -> int -> TType -> TType list * TType

val applyForallTy : TcGlobals -> TType -> TypeInst -> TType

val tryDestTupleTy : TcGlobals -> TType -> TType list

//-------------------------------------------------------------------------
// Compute actual types of union cases and fields given an instantiation 
// of the generic type parameters of the enclosing type.
//------------------------------------------------------------------------- 

val actualResultTyOfUnionCase : TypeInst -> UnionCaseRef -> TType

val actualTysOfUnionCaseFields : TyparInst -> UnionCaseRef -> TType list

val actualTysOfInstanceRecdFields    : TyparInst -> TyconRef -> TType list

val actualTyOfRecdField            : TyparInst -> RecdField -> TType
val actualTyOfRecdFieldRef : RecdFieldRef -> TypeInst -> TType
val actualTyOfRecdFieldForTycon    : Tycon -> TypeInst -> RecdField -> TType

//-------------------------------------------------------------------------
// Top types: guaranteed to be compiled to .NET methods, and must be able to 
// have user-specified argument names (for stability w.r.t. reflection)
// and user-specified argument and return attributes.
//------------------------------------------------------------------------- 

type UncurriedArgInfos = (TType * ArgReprInfo) list 
type CurriedArgInfos = UncurriedArgInfos list

val GetTopTauTypeInFSharpForm     : TcGlobals -> ArgReprInfo list list -> TType -> range -> CurriedArgInfos * TType
val GetTopValTypeInFSharpForm     : TcGlobals -> ValReprInfo -> TType -> range -> Typars * CurriedArgInfos * TType * ArgReprInfo
val IsCompiledAsStaticProperty    : TcGlobals -> Val -> bool
val IsCompiledAsStaticPropertyWithField : TcGlobals -> Val -> bool
val GetTopValTypeInCompiledForm   : TcGlobals -> ValReprInfo -> TType -> range -> Typars * CurriedArgInfos * TType option * ArgReprInfo
val GetFSharpViewOfReturnType     : TcGlobals -> TType option -> TType

val NormalizeDeclaredTyparsForEquiRecursiveInference : TcGlobals -> Typars -> Typars

//-------------------------------------------------------------------------
// Compute the return type after an application
//------------------------------------------------------------------------- 
 
val applyTys : TcGlobals -> TType -> TType list * 'T list -> TType

//-------------------------------------------------------------------------
// Compute free variables in types
//------------------------------------------------------------------------- 
 
val emptyFreeTypars : FreeTypars
val unionFreeTypars : FreeTypars -> FreeTypars -> FreeTypars

val emptyFreeTycons : FreeTycons
val unionFreeTycons : FreeTycons -> FreeTycons -> FreeTycons

val emptyFreeTyvars : FreeTyvars
val unionFreeTyvars : FreeTyvars -> FreeTyvars -> FreeTyvars

val emptyFreeLocals : FreeLocals
val unionFreeLocals : FreeLocals -> FreeLocals -> FreeLocals

type FreeVarOptions 

val CollectLocalsNoCaching : FreeVarOptions
val CollectTyparsNoCaching : FreeVarOptions
val CollectTyparsAndLocalsNoCaching : FreeVarOptions
val CollectTyparsAndLocals : FreeVarOptions
val CollectLocals : FreeVarOptions
val CollectTypars : FreeVarOptions
val CollectAllNoCaching : FreeVarOptions
val CollectAll : FreeVarOptions

val accFreeInTypes : FreeVarOptions -> TType list -> FreeTyvars -> FreeTyvars
val accFreeInType : FreeVarOptions -> TType -> FreeTyvars -> FreeTyvars
val accFreeInTypars : FreeVarOptions -> Typars -> FreeTyvars -> FreeTyvars

val freeInType  : FreeVarOptions -> TType      -> FreeTyvars
val freeInTypes : FreeVarOptions -> TType list -> FreeTyvars
val freeInVal   : FreeVarOptions -> Val -> FreeTyvars

// This one puts free variables in canonical left-to-right order. 
val freeInTypeLeftToRight : TcGlobals -> bool -> TType -> Typars
val freeInTypesLeftToRight : TcGlobals -> bool -> TType list -> Typars
val freeInTypesLeftToRightSkippingConstraints : TcGlobals -> TType list -> Typars


//-------------------------------------------------------------------------
// Equivalence of types (up to substitution of type variables in the left-hand type)
//------------------------------------------------------------------------- 

[<NoEquality; NoComparison>]
type TypeEquivEnv = 
    { EquivTypars: TyparMap<TType>;
      EquivTycons: TyconRefRemap }

    static member Empty : TypeEquivEnv
    member BindEquivTypars : Typars -> Typars -> TypeEquivEnv
    static member FromTyparInst : TyparInst -> TypeEquivEnv
    static member FromEquivTypars : Typars -> Typars -> TypeEquivEnv

val traitsAEquivAux           : Erasure -> TcGlobals -> TypeEquivEnv -> TraitConstraintInfo  -> TraitConstraintInfo  -> bool
val traitsAEquiv              :            TcGlobals -> TypeEquivEnv -> TraitConstraintInfo  -> TraitConstraintInfo  -> bool
val typarConstraintsAEquivAux : Erasure -> TcGlobals -> TypeEquivEnv -> TyparConstraint      -> TyparConstraint      -> bool
val typarConstraintsAEquiv    :            TcGlobals -> TypeEquivEnv -> TyparConstraint      -> TyparConstraint      -> bool
val typarsAEquiv              :            TcGlobals -> TypeEquivEnv -> Typars               -> Typars               -> bool
val typeAEquivAux             : Erasure -> TcGlobals -> TypeEquivEnv -> TType                  -> TType                  -> bool
val typeAEquiv                :            TcGlobals -> TypeEquivEnv -> TType                  -> TType                  -> bool
val returnTypesAEquivAux      : Erasure -> TcGlobals -> TypeEquivEnv -> TType option           -> TType option           -> bool
val returnTypesAEquiv         :            TcGlobals -> TypeEquivEnv -> TType option           -> TType option           -> bool
val tcrefAEquiv               :            TcGlobals -> TypeEquivEnv -> TyconRef             -> TyconRef             -> bool
val valLinkageAEquiv          :            TcGlobals -> TypeEquivEnv -> Val   -> Val -> bool


//-------------------------------------------------------------------------
// Unit operations
//------------------------------------------------------------------------- 
val MeasurePower : MeasureExpr -> int -> MeasureExpr
val ListMeasureVarOccsWithNonZeroExponents : MeasureExpr -> (Typar * int) list
val ListMeasureConOccsWithNonZeroExponents : TcGlobals -> bool -> MeasureExpr -> (TyconRef * int) list
val ProdMeasures : MeasureExpr list -> MeasureExpr
val MeasureVarExponent : Typar -> MeasureExpr -> int
val MeasureConExponent : TcGlobals -> bool -> TyconRef -> MeasureExpr -> int

//-------------------------------------------------------------------------
// Members 
//------------------------------------------------------------------------- 

val GetTypeOfMemberInMemberForm : TcGlobals -> ValRef -> Typars * CurriedArgInfos * TType option * ArgReprInfo
val GetTypeOfIntrinsicMemberInCompiledForm : TcGlobals -> ValRef -> Typars * CurriedArgInfos * TType option * ArgReprInfo
val GetMemberTypeInMemberForm : TcGlobals -> MemberFlags -> ValReprInfo -> TType -> range -> Typars * CurriedArgInfos * TType option * ArgReprInfo

val PartitionValTypars : TcGlobals -> Val -> (Typars * Typars * Typars * TyparInst * TType list) option
val PartitionValRefTypars : TcGlobals -> ValRef -> (Typars * Typars * Typars * TyparInst * TType list) option

val ReturnTypeOfPropertyVal : TcGlobals -> Val -> TType
val ArgInfosOfPropertyVal : TcGlobals -> Val -> UncurriedArgInfos 
val ArgInfosOfMember: TcGlobals -> ValRef -> CurriedArgInfos 

val GetMemberCallInfo : TcGlobals -> ValRef * ValUseFlag -> int * bool * bool * bool * bool * bool * bool * bool

//-------------------------------------------------------------------------
// Printing
//------------------------------------------------------------------------- 
 
module PrettyTypes =
    val NeedsPrettyTyparName : Typar -> bool
    val PrettyTyparNames : (Typar -> bool) -> string list -> Typars -> string list
    val PrettifyTypes1 : TcGlobals -> TType -> TyparInst * TType * (Typar * TyparConstraint) list
    val PrettifyTypes2 : TcGlobals -> TType * TType -> TyparInst * (TType * TType) * (Typar * TyparConstraint) list
    val PrettifyTypesN : TcGlobals -> TType list -> TyparInst * TType list * (Typar * TyparConstraint) list

[<NoEquality; NoComparison>]
type DisplayEnv = 
    { openTopPathsSorted: Lazy<string list list>; 
      openTopPathsRaw: string list list; 
      showObsoleteMembers: bool; 
      showTyparBinding: bool;
      showImperativeTyparAnnotations: bool;
      suppressInlineKeyword:bool;
      suppressMutableKeyword:bool;
      showMemberContainers: bool;
      shortConstraints:bool;
      useColonForReturnType:bool;
      showAttributes: bool;
      showOverrides:bool;
      showConstraintTyparAnnotations:bool;
      abbreviateAdditionalConstraints: bool;
      showTyparDefaultConstraints: bool
      g: TcGlobals 
      contextAccessibility: Accessibility
      generatedValueLayout:(Val -> layout option) }
    member SetOpenPaths: string list list -> DisplayEnv
    static member Empty: TcGlobals -> DisplayEnv

    member AddOpenPath : path   -> DisplayEnv
    member AddOpenModuleOrNamespace : ModuleOrNamespaceRef   -> DisplayEnv


/// Return the full text for an item as we want it displayed to the user as a fully qualified entity
val fullDisplayTextOfModRef : ModuleOrNamespaceRef -> string
val fullDisplayTextOfParentOfModRef : ModuleOrNamespaceRef -> string option
val fullDisplayTextOfValRef   : ValRef -> string
val fullDisplayTextOfTyconRef  : TyconRef -> string
val fullDisplayTextOfExnRef  : TyconRef -> string
val fullDisplayTextOfUnionCaseRef  : UnionCaseRef -> string
val fullDisplayTextOfRecdFieldRef  : RecdFieldRef -> string

val ticksAndArgCountTextOfTyconRef : TyconRef -> string

/// Return the full path to the item using mangled names (well, sort of), to act as a unique key into the FSI generation lookaside table.
/// The mangled names have to precisely match the path names implicitly embedded as attributes into layout objects by the NicePrint code below.
/// This is a very fragile technique and the mangled names are not really guaranteed to be unique (e.g. the mangled names
/// don't cope with overloading of types by generic arity), and this whole business of using mangled paths is not a 
/// good technique in general. Hence these functions should not be used outside the FSI generation code.
val approxFullMangledNameOfModuleOrNamespaceRef : ModuleOrNamespaceRef -> string
val approxFullMangledNameOfValRef : ValRef -> string
val approxFullMangledNameOfTyconRef  : TyconRef -> string
val approxFullMangledNameOfExnRef  : TyconRef -> string
val approxFullMangledNameOfUnionCaseRef  : UnionCaseRef -> string
val approxFullMangledNameOfRecdFieldRef  : RecdFieldRef -> string

/// A unique qualified name for each type definition, used to qualify the names of interface implementation methods
val qualifiedMangledNameOfTyconRef : TyconRef -> string -> string

module NicePrint =
    val constL : Const -> layout
    val typeL                       : DisplayEnv -> TType -> layout
    val constraintL                 : DisplayEnv -> (Typar * TyparConstraint) -> layout 
    val topTypAndConstraintsL       : DisplayEnv -> (TType * ArgReprInfo) list -> TType -> layout
    val typesAndConstraintsL        : DisplayEnv -> TType list -> layout list * layout
    val memberSigL                  : DisplayEnv -> TyparInst * string * Typars * CurriedArgInfos * TType -> layout
    val valL                        : DisplayEnv -> Val -> layout
    val dataExprL                   : DisplayEnv -> Expr -> layout

    val inferredSigOfModuleExprL    : bool -> DisplayEnv -> ModuleOrNamespaceExprWithSig -> layout
    val assemblyL                   : DisplayEnv -> ModuleOrNamespace -> layout
    
    val stringOfTy               : DisplayEnv -> TType -> string
    val prettyStringOfTy         : DisplayEnv -> TType -> string
    val stringOfTyparConstraints : DisplayEnv -> (Typar * TyparConstraint) list  -> string
    val stringOfTyparConstraint  : DisplayEnv -> Typar * TyparConstraint -> string

    val outputILTypeRef          : DisplayEnv -> StringBuilder -> ILTypeRef -> unit
    val outputTyconRef           : DisplayEnv -> StringBuilder -> TyconRef -> unit
    val outputTy                 : DisplayEnv -> StringBuilder -> TType -> unit
    val outputTypars             : DisplayEnv -> string -> StringBuilder -> Typars -> unit
    val outputQualifiedValSpec   : DisplayEnv -> StringBuilder -> Val -> unit
    
    val stringOfQualifiedValSpec : DisplayEnv -> Val -> string

    val outputTycon              : DisplayEnv -> StringBuilder -> Tycon -> unit
    val outputExnDef             : DisplayEnv -> StringBuilder -> Tycon -> unit
    
    val stringOfUnionCase        : DisplayEnv -> UnionCase -> string
    val stringOfRecdField        : DisplayEnv -> RecdField -> string
    val stringOfExnDef           : DisplayEnv -> Tycon -> string

//-------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

val superOfTycon : TcGlobals -> Tycon -> TType
val abstractSlotValsOfTycons : Tycon list -> Val list

//-------------------------------------------------------------------------
// Free variables in expressions etc.
//------------------------------------------------------------------------- 

val emptyFreeVars : FreeVars
val unionFreeVars : FreeVars -> FreeVars -> FreeVars

val accFreeInTargets      : FreeVarOptions -> DecisionTreeTarget array -> FreeVars -> FreeVars
val accFreeInExprs        : FreeVarOptions -> Exprs -> FreeVars -> FreeVars
val accFreeInSwitchCases : FreeVarOptions -> DecisionTreeCase list -> DecisionTree option -> FreeVars -> FreeVars
val accFreeInDecisionTree        : FreeVarOptions -> DecisionTree -> FreeVars -> FreeVars

/// Get the free variables in a module definition.
val freeInModuleOrNamespace : FreeVarOptions -> ModuleOrNamespaceExpr -> FreeVars

/// Get the free variables in an expression.
val freeInExpr  : FreeVarOptions -> Expr  -> FreeVars

/// Get the free variables in the right hand side of a binding.
val freeInBindingRhs   : FreeVarOptions -> Binding  -> FreeVars

val freeTyvarsAllPublic  : FreeTyvars -> bool
val freeVarsAllPublic     : FreeVars -> bool

//-------------------------------------------------------------------------
// Mark/range/position information from expressions
//------------------------------------------------------------------------- 

type Expr with 
    member Range : range

//-------------------------------------------------------------------------
// type-of operations on the expression tree
//------------------------------------------------------------------------- 

val tyOfExpr : TcGlobals -> Expr -> TType 

//-------------------------------------------------------------------------
// Top expressions to implement top types
//------------------------------------------------------------------------- 

val stripTopLambda : Expr * TType -> Typars * Val list list * Expr * TType
val InferArityOfExpr : TcGlobals -> TType -> Attribs list list -> Attribs -> Expr -> ValReprInfo
val InferArityOfExprBinding : TcGlobals -> Val -> Expr -> ValReprInfo

//-------------------------------------------------------------------------
//  Copy expressions and types
//------------------------------------------------------------------------- 
                   
// Ideally, this mutation should not be needed 
val setValHasNoArity : Val -> Val

type ValCopyFlag = 
    | CloneAll
    | CloneAllAndMarkExprValsAsCompilerGenerated
    // OnlyCloneExprVals is a nasty setting to reuse the cloning logic in a mode where all 
    // Tycon and "module/member" Val objects keep their identity, but the Val objects for all Expr bindings
    // are cloned. This is used to 'fixup' the TAST created by tlr.fs 
    //
    // This is a fragile mode of use. It's not really clear why TLR needs to create a "bad" expression tree that
    // reuses Val objects as multiple value bindings, and its been the cause of several subtle bugs.
    | OnlyCloneExprVals

val remapTyconRef : TyconRefRemap -> TyconRef -> TyconRef
val remapUnionCaseRef : TyconRefRemap -> UnionCaseRef -> UnionCaseRef
val remapRecdFieldRef : TyconRefRemap -> RecdFieldRef -> RecdFieldRef
val remapValRef : Remap -> ValRef -> ValRef
val remapExpr : TcGlobals -> ValCopyFlag -> Remap -> Expr -> Expr
val remapAttrib : TcGlobals -> Remap -> Attrib -> Attrib
val remapPossibleForallTy : TcGlobals -> Remap -> TType -> TType
val copyModuleOrNamespaceType : TcGlobals -> ValCopyFlag -> ModuleOrNamespaceType -> ModuleOrNamespaceType
val copyExpr : TcGlobals -> ValCopyFlag -> Expr -> Expr
val copyImplFile : TcGlobals -> ValCopyFlag -> TypedImplFile -> TypedImplFile
val copySlotSig : SlotSig -> SlotSig
val instSlotSig : TyparInst -> SlotSig -> SlotSig
val instExpr : TcGlobals -> TyparInst -> Expr -> Expr

//-------------------------------------------------------------------------
// Build the remapping that corresponds to a module meeting its signature
// and also report the set of tycons, tycon representations and values hidden in the process.
//------------------------------------------------------------------------- 

type SignatureRepackageInfo = 
    { mrpiVals: (ValRef * ValRef) list;
      mrpiEntities: (TyconRef * TyconRef) list  }

    static member Empty : SignatureRepackageInfo
      
type SignatureHidingInfo = 
    { mhiTycons  : Zset<Tycon>; 
      mhiTyconReprs : Zset<Tycon>;  
      mhiVals       : Zset<Val>; 
      mhiRecdFields : Zset<RecdFieldRef>;
      mhiUnionCases : Zset<UnionCaseRef> }

val ComputeRemappingFromInferredSignatureToExplicitSignature : TcGlobals -> ModuleOrNamespaceType -> ModuleOrNamespaceType -> SignatureRepackageInfo * SignatureHidingInfo
val ComputeRemappingFromImplementationToSignature : TcGlobals -> ModuleOrNamespaceExpr -> ModuleOrNamespaceType -> SignatureRepackageInfo * SignatureHidingInfo
val ComputeHidingInfoAtAssemblyBoundary : ModuleOrNamespaceType -> SignatureHidingInfo
val mkRepackageRemapping : SignatureRepackageInfo -> Remap 

val wrapModuleOrNamespaceExprInNamespace : Ident -> CompilationPath -> ModuleOrNamespaceExpr -> ModuleOrNamespaceExpr
val wrapModuleOrNamespaceTypeInNamespace : Ident -> CompilationPath -> ModuleOrNamespaceType -> ModuleOrNamespaceType 
val wrapModuleOrNamespaceType : Ident -> CompilationPath -> ModuleOrNamespaceType -> ModuleOrNamespace

val SigTypeOfImplFile : TypedImplFile -> ModuleOrNamespaceType

//-------------------------------------------------------------------------
// Given a list of top-most signatures that together constrain the public compilation units
// of an assembly, compute a remapping that converts local references to non-local references.
// This remapping must be applied to all pickled expressions and types 
// exported from the assembly.
//------------------------------------------------------------------------- 


val tryRescopeEntity : CcuThunk -> Entity -> EntityRef option
val tryRescopeVal    : CcuThunk -> Remap -> Val -> ValRef option

val MakeExportRemapping : CcuThunk -> ModuleOrNamespace -> Remap
val ApplyExportRemappingToEntity :  TcGlobals -> Remap -> ModuleOrNamespace -> ModuleOrNamespace 

/// Query SignatureRepackageInfo
val IsHiddenTycon     : (Remap * SignatureHidingInfo) list -> Tycon -> bool
val IsHiddenTyconRepr : (Remap * SignatureHidingInfo) list -> Tycon -> bool
val IsHiddenVal       : (Remap * SignatureHidingInfo) list -> Val -> bool
val IsHiddenRecdField : (Remap * SignatureHidingInfo) list -> RecdFieldRef -> bool

//-------------------------------------------------------------------------
//  Adjust marks in expressions
//------------------------------------------------------------------------- 

val remarkExpr : range -> Expr -> Expr

//-------------------------------------------------------------------------
// Make applications
//------------------------------------------------------------------------- 
 
val primMkApp : (Expr * TType) -> TypeInst -> Exprs -> range -> Expr
val mkApps : TcGlobals -> (Expr * TType) * TType list list * Exprs * range -> Expr
val mkTyAppExpr : range -> Expr * TType -> TType list -> Expr

///   localv <- e      
val mkValSet   : range -> ValRef -> Expr -> Expr
///  *localv_ptr = e   
val mkAddrSet  : range -> ValRef -> Expr -> Expr
/// *localv_ptr        
val mkAddrGet  : range -> ValRef -> Expr
/// &localv           
val mkValAddr  : range -> ValRef -> Expr

//-------------------------------------------------------------------------
// Note these take the address of the record expression if it is a struct, and
// apply a type instantiation if it is a first-class polymorphic record field.
//------------------------------------------------------------------------- 

val mkRecdFieldGet : TcGlobals -> Expr * RecdFieldRef * TypeInst * TypeInst * range -> Expr
val mkRecdFieldSet : TcGlobals -> Expr * RecdFieldRef * TypeInst * Expr * range -> Expr

//-------------------------------------------------------------------------
//  Get the targets used in a decision graph (for reporting warnings)
//------------------------------------------------------------------------- 

val accTargetsOfDecisionTree : DecisionTree -> int list -> int list

//-------------------------------------------------------------------------
//  Optimizations on decision graphs
//------------------------------------------------------------------------- 

val mkAndSimplifyMatch : SequencePointInfoForBinding  -> range -> range -> TType -> DecisionTree -> DecisionTreeTarget list -> Expr

val primMkMatch : SequencePointInfoForBinding * range * DecisionTree * DecisionTreeTarget array * range * TType -> Expr

//-------------------------------------------------------------------------
//  Work out what things on the r.h.s. of a letrec need to be fixed up
//------------------------------------------------------------------------- 

val IterateRecursiveFixups : 
   TcGlobals -> Val option  -> 
   (Val option -> Expr -> (Expr -> Expr) -> Expr -> unit) -> 
   Expr * (Expr -> Expr) -> Expr -> unit

//-------------------------------------------------------------------------
// From lambdas taking multiple variables to lambdas taking a single variable
// of tuple type. 
//------------------------------------------------------------------------- 

val MultiLambdaToTupledLambda: Val list -> Expr -> Val * Expr
val AdjustArityOfLambdaBody          : TcGlobals -> int -> Val list -> Expr -> Val list * Expr

//-------------------------------------------------------------------------
// Make applications, doing beta reduction by introducing let-bindings
//------------------------------------------------------------------------- 

val MakeApplicationAndBetaReduce : TcGlobals -> Expr * TType * TypeInst list * Exprs * range -> Expr

val JoinTyparStaticReq : TyparStaticReq -> TyparStaticReq -> TyparStaticReq

//-------------------------------------------------------------------------
// More layout - this is for debugging
//------------------------------------------------------------------------- 
module DebugPrint =

    val layoutRanges : bool ref
    val showType : TType -> string
    val showExpr : Expr -> string

    val valRefL : ValRef -> layout
    val unionCaseRefL : UnionCaseRef -> layout
    val vspecAtBindL : Val -> layout
    val intL : int -> layout
    val valL : Val -> layout
    val typarDeclL : Typar -> layout
    val traitL : TraitConstraintInfo -> layout
    val typarL : Typar -> layout
    val typarsL : Typars -> layout
    val typeL : TType -> layout
    val slotSigL : SlotSig -> layout
    val entityTypeL : ModuleOrNamespaceType -> layout
    val entityL : ModuleOrNamespace -> layout
    val typeOfValL : Val -> layout
    val bindingL : Binding -> layout
    val exprL : Expr -> layout
    val tyconL : Tycon -> layout
    val decisionTreeL : DecisionTree -> layout
    val implFileL : TypedImplFile -> layout
    val assemblyL : TypedAssembly -> layout
    val recdFieldRefL : RecdFieldRef -> layout

//-------------------------------------------------------------------------
// Fold on expressions
//------------------------------------------------------------------------- 

type ExprFolder<'State> =
    { exprIntercept            : ('State -> Expr -> 'State) -> 'State -> Expr -> 'State option;
      valBindingSiteIntercept  : 'State -> bool * Val -> 'State;
      nonRecBindingsIntercept  : 'State -> Binding -> 'State;         
      recBindingsIntercept     : 'State -> Bindings -> 'State;         
      dtreeIntercept           : 'State -> DecisionTree -> 'State;
      targetIntercept          : ('State -> Expr -> 'State) -> 'State -> DecisionTreeTarget -> 'State option;
      tmethodIntercept         : ('State -> Expr -> 'State) -> 'State -> ObjExprMethod -> 'State option;}
val ExprFolder0 : ExprFolder<'State>
val FoldImplFile: ExprFolder<'State> -> ('State -> TypedImplFile -> 'State) 
val FoldExpr : ExprFolder<'State> -> ('State -> Expr -> 'State) 

#if DEBUG
val ExprStats : Expr -> string
#endif

//-------------------------------------------------------------------------
// Make some common types
//------------------------------------------------------------------------- 

val mkNativePtrType  : TcGlobals -> TType -> TType
val mkArrayType      : TcGlobals -> TType -> TType
val isOptionTy     : TcGlobals -> TType -> bool
val destOptionTy   : TcGlobals -> TType -> TType
val tryDestOptionTy : TcGlobals -> TType -> TType option

//-------------------------------------------------------------------------
// Primitives associated with compiling the IEvent idiom to .NET events
//------------------------------------------------------------------------- 

val isIDelegateEventType   : TcGlobals -> TType -> bool
val destIDelegateEventType : TcGlobals -> TType -> TType 
val mkIEventType   : TcGlobals -> TType -> TType -> TType
val mkIObservableType   : TcGlobals -> TType -> TType
val mkIObserverType   : TcGlobals -> TType -> TType

//-------------------------------------------------------------------------
// Primitives associated with printf format string parsing
//------------------------------------------------------------------------- 

val mkLazyTy : TcGlobals -> TType -> TType
val mkPrintfFormatTy : TcGlobals -> TType -> TType -> TType -> TType -> TType -> TType

//-------------------------------------------------------------------------
// Classify types
//------------------------------------------------------------------------- 

val isILAppTy      : TcGlobals -> TType -> bool
val isArrayTy        : TcGlobals -> TType -> bool
val isArray1DTy       : TcGlobals -> TType -> bool
val destArrayTy     : TcGlobals -> TType -> TType
val isILReferenceTy        : TcGlobals -> TType -> bool

val mkArrayTy         : TcGlobals -> int -> TType -> TType
val isArrayTyconRef      : TcGlobals -> TyconRef -> bool
val rankOfArrayTyconRef : TcGlobals -> TyconRef -> int

val isUnitTy          : TcGlobals -> TType -> bool
val isObjTy           : TcGlobals -> TType -> bool
val isVoidTy          : TcGlobals -> TType -> bool

/// Get the element type of an array type
val destArrayTy    : TcGlobals -> TType -> TType
/// Get the rank of an array type
val rankOfArrayTy : TcGlobals -> TType -> int

val isInterfaceTyconRef                 : TyconRef -> bool

val isDelegateTy  : TcGlobals -> TType -> bool
val isInterfaceTy : TcGlobals -> TType -> bool
val isRefTy    : TcGlobals -> TType -> bool
val isSealedTy : TcGlobals -> TType -> bool
val isComInteropTy : TcGlobals -> TType -> bool
val underlyingTypeOfEnumTy : TcGlobals -> TType -> TType
val isStructTy : TcGlobals -> TType -> bool
val isUnmanagedTy : TcGlobals -> TType -> bool
val isClassTy  : TcGlobals -> TType -> bool
val isEnumTy   : TcGlobals -> TType -> bool
val isFlagEnumTy   : TcGlobals -> TType -> bool

/// For "type Class as self", 'self' is fixed up after initialization. To support this,
/// it is converted behind the scenes to a ref. This function strips off the ref and
/// returns the underlying type.
val StripSelfRefCell : TcGlobals * ValBaseOrThisInfo * TType -> TType


//-------------------------------------------------------------------------
// Special semantic constraints
//------------------------------------------------------------------------- 

val IsUnionTypeWithNullAsTrueValue: TcGlobals -> Tycon -> bool
val TyconHasUseNullAsTrueValueAttribute : TcGlobals -> Tycon -> bool
val CanHaveUseNullAsTrueValueAttribute : TcGlobals -> Tycon -> bool
val MemberIsCompiledAsInstance : TcGlobals -> TyconRef -> bool -> ValMemberInfo -> Attribs -> bool
val ValSpecIsCompiledAsInstance : TcGlobals -> Val -> bool
val ValRefIsCompiledAsInstanceMember : TcGlobals -> ValRef -> bool
val ModuleNameIsMangled : TcGlobals -> Attribs -> bool

val CompileAsEvent : TcGlobals -> Attribs -> bool

val TypeNullIsExtraValue : TcGlobals -> TType -> bool
val TypeNullIsTrueValue : TcGlobals -> TType -> bool
val TypeNullNotLiked : TcGlobals -> TType -> bool
val TypeNullNever : TcGlobals -> TType -> bool

val TypeSatisfiesNullConstraint : TcGlobals -> TType -> bool
val TypeHasDefaultValue : TcGlobals -> TType -> bool

val isAbstractTycon : Tycon -> bool

val isUnionCaseAllocObservable : UnionCaseRef -> bool
val isTyconRefAllocObservable : TyconRef -> bool
val isExnAllocObservable : TyconRef -> bool 
val isUnionCaseFieldMutable : TcGlobals -> UnionCaseRef -> int -> bool
val isExnFieldMutable : TyconRef -> int -> bool

val useGenuineField : Tycon -> RecdField -> bool 
val ComputeFieldName : Tycon -> RecdField -> string

//-------------------------------------------------------------------------
// Destruct slotsigs etc.
//------------------------------------------------------------------------- 

val slotSigHasVoidReturnTy     : SlotSig -> bool
val actualReturnTyOfSlotSig    : TypeInst -> TypeInst -> SlotSig -> TType option

val returnTyOfMethod : TcGlobals -> ObjExprMethod -> TType option

//-------------------------------------------------------------------------
// Primitives associated with initialization graphs
//------------------------------------------------------------------------- 

val mkRefCell              : TcGlobals -> range -> TType -> Expr -> Expr
val mkRefCellGet          : TcGlobals -> range -> TType -> Expr -> Expr
val mkRefCellSet          : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkLazyDelayed         : TcGlobals -> range -> TType -> Expr -> Expr
val mkLazyForce           : TcGlobals -> range -> TType -> Expr -> Expr

val mkRefCellContentsRef : TcGlobals -> RecdFieldRef
val isRefCellTy   : TcGlobals -> TType -> bool
val destRefCellTy : TcGlobals -> TType -> TType
val mkRefCellTy   : TcGlobals -> TType -> TType

val mkSeqTy          : TcGlobals -> TType -> TType
val mkIEnumeratorTy  : TcGlobals -> TType -> TType
val mkListTy         : TcGlobals -> TType -> TType
val mkOptionTy       : TcGlobals -> TType -> TType
val mkNoneCase  : TcGlobals -> UnionCaseRef
val mkSomeCase  : TcGlobals -> UnionCaseRef

val mkNil  : TcGlobals -> range -> TType -> Expr
val mkCons : TcGlobals -> TType -> Expr -> Expr -> Expr

//-------------------------------------------------------------------------
// Make a few more expressions
//------------------------------------------------------------------------- 

val mkSeq  : SequencePointInfoForSeq -> range -> Expr -> Expr -> Expr
val mkCompGenSequential  : range -> Expr -> Expr -> Expr
val mkSeqList : SequencePointInfoForSeq -> TcGlobals -> range -> Exprs -> Expr   
val mkRecordExpr : TcGlobals -> RecordConstructionInfo * TyconRef * TypeInst * RecdFieldRef list * Exprs * range -> Expr
val mkUnbox : TType -> Expr -> range -> Expr
val mkBox : TType -> Expr -> range -> Expr
val mkIsInst : TType -> Expr -> range -> Expr
val mkNull : range -> TType -> Expr
val mkNullTest : TcGlobals -> range -> Expr -> Expr -> Expr -> Expr
val mkNonNullTest : TcGlobals -> range -> Expr -> Expr
val mkIsInstConditional : TcGlobals -> range -> TType -> Expr -> Val -> Expr -> Expr -> Expr
val mkThrow   : range -> TType -> Expr -> Expr
val mkGetArg0 : range -> TType -> Expr

val mkDefault : range * TType -> Expr

val mkString    : TcGlobals -> range -> string -> Expr
val mkBool      : TcGlobals -> range -> bool -> Expr
val mkByte      : TcGlobals -> range -> byte -> Expr
val mkUInt16    : TcGlobals -> range -> uint16 -> Expr
val mkTrue      : TcGlobals -> range -> Expr
val mkFalse     : TcGlobals -> range -> Expr
val mkUnit      : TcGlobals -> range -> Expr
val mkInt32     : TcGlobals -> range -> int32 -> Expr
val mkInt       : TcGlobals -> range -> int -> Expr
val mkZero      : TcGlobals -> range -> Expr
val mkOne       : TcGlobals -> range -> Expr
val mkTwo       : TcGlobals -> range -> Expr
val mkMinusOne : TcGlobals -> range -> Expr
val destInt32 : Expr -> int32 option

//-------------------------------------------------------------------------
// Primitives associated with quotations
//------------------------------------------------------------------------- 
 
val mkQuotedExprTy : TcGlobals -> TType -> TType
val mkRawQuotedExprTy : TcGlobals -> TType
val mspec_Type_GetTypeFromHandle : ILGlobals ->  ILMethodSpec
val fspec_Missing_Value : ILGlobals ->  ILFieldSpec
val mkByteArrayTy : TcGlobals -> TType

//-------------------------------------------------------------------------
// Construct calls to some intrinsic functions
//------------------------------------------------------------------------- 

val mkCallNewFormat              : TcGlobals -> range -> TType -> TType -> TType -> TType -> TType -> Expr -> Expr

val mkCallUnbox       : TcGlobals -> range -> TType -> Expr -> Expr
val mkCallGetGenericComparer : TcGlobals -> range -> Expr
val mkCallGetGenericEREqualityComparer : TcGlobals -> range -> Expr
val mkCallGetGenericPEREqualityComparer : TcGlobals -> range -> Expr

val mkCallUnboxFast  : TcGlobals -> range -> TType -> Expr -> Expr
val canUseUnboxFast  : TcGlobals -> TType -> bool

val mkCallDispose     : TcGlobals -> range -> TType -> Expr -> Expr
val mkCallSeq         : TcGlobals -> range -> TType -> Expr -> Expr

val mkCallTypeTest      : TcGlobals -> range -> TType -> Expr -> Expr
val canUseTypeTestFast : TcGlobals -> TType -> bool

val mkCallTypeOf      : TcGlobals -> range -> TType -> Expr
val mkCallTypeDefOf   : TcGlobals -> range -> TType -> Expr 

val mkCallCreateInstance     : TcGlobals -> range -> TType -> Expr
val mkCallCreateEvent        : TcGlobals -> range -> TType -> TType -> Expr -> Expr -> Expr -> Expr
val mkCallArrayGet           : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkCallArray2DGet         : TcGlobals -> range -> TType -> Expr -> Expr -> Expr -> Expr 
val mkCallArray3DGet         : TcGlobals -> range -> TType -> Expr -> Expr -> Expr -> Expr -> Expr
val mkCallArray4DGet         : TcGlobals -> range -> TType -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
val mkCallRaise              : TcGlobals -> range -> TType -> Expr -> Expr

val mkCallGenericComparisonWithComparerOuter : TcGlobals -> range -> TType -> Expr -> Expr -> Expr -> Expr
val mkCallGenericEqualityEROuter             : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkCallEqualsOperator                     : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkCallGenericEqualityWithComparerOuter   : TcGlobals -> range -> TType -> Expr -> Expr -> Expr -> Expr
val mkCallGenericHashWithComparerOuter       : TcGlobals -> range -> TType -> Expr -> Expr -> Expr

val mkCallUnpickleQuotation  : TcGlobals -> range -> Expr -> Expr -> Expr -> Expr -> Expr
val mkCallCastQuotation      : TcGlobals -> range -> TType -> Expr -> Expr 
val mkCallLiftValue          : TcGlobals -> range -> TType -> Expr -> Expr
val mkCallSeqCollect         : TcGlobals -> range -> TType  -> TType -> Expr -> Expr -> Expr
val mkCallSeqUsing           : TcGlobals -> range -> TType  -> TType -> Expr -> Expr -> Expr
val mkCallSeqDelay           : TcGlobals -> range -> TType  -> Expr -> Expr
val mkCallSeqAppend          : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkCallSeqFinally         : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkCallSeqGenerated       : TcGlobals -> range -> TType -> Expr -> Expr -> Expr
val mkCallSeqOfFunctions    : TcGlobals -> range -> TType  -> TType -> Expr -> Expr -> Expr -> Expr
val mkCallSeqToArray        : TcGlobals -> range -> TType  -> Expr -> Expr 
val mkCallSeqToList         : TcGlobals -> range -> TType  -> Expr -> Expr 
val mkCallSeqMap             : TcGlobals -> range -> TType  -> TType -> Expr -> Expr -> Expr
val mkCallSeqSingleton       : TcGlobals -> range -> TType  -> Expr -> Expr
val mkCallSeqEmpty           : TcGlobals -> range -> TType  -> Expr
val mkILAsmCeq                   : TcGlobals -> range -> Expr -> Expr -> Expr
val mkILAsmClt                   : TcGlobals -> range -> Expr -> Expr -> Expr

val mkCallFailInit           : TcGlobals -> range -> Expr 
val mkCallFailStaticInit    : TcGlobals -> range -> Expr 
val mkCallCheckThis          : TcGlobals -> range -> TType -> Expr -> Expr 

val mkCase : Test * DecisionTree -> DecisionTreeCase

//-------------------------------------------------------------------------
// operations primarily associated with the optimization to fix
// up loops to generate .NET code that does not include array bound checks
//------------------------------------------------------------------------- 

val mkDecr   : TcGlobals -> range -> Expr -> Expr
val mkIncr   : TcGlobals -> range -> Expr -> Expr
val mkLdlen  : TcGlobals -> range -> Expr -> Expr
val mkLdelem : TcGlobals -> range -> TType -> Expr -> Expr -> Expr

//-------------------------------------------------------------------------
// Analyze attribute sets 
//------------------------------------------------------------------------- 

val ILThingHasILAttrib : ILTypeRef -> ILAttributes -> bool
val ILThingDecodeILAttrib   : TcGlobals -> ILTypeRef -> ILScopeRef option -> ILAttributes -> (ILAttribElem list * ILAttributeNamedArg list) option
val ILThingHasAttrib : Env.BuiltinAttribInfo -> ILAttributes -> bool

val IsMatchingAttrib      : TcGlobals -> Env.BuiltinAttribInfo -> Attrib -> bool
val HasAttrib             : TcGlobals -> Env.BuiltinAttribInfo -> Attribs -> bool
val TryFindAttrib         : TcGlobals -> Env.BuiltinAttribInfo -> Attribs -> Attrib option
val TryFindUnitAttrib     : TcGlobals -> Env.BuiltinAttribInfo -> Attribs -> unit option
val TryFindBoolAttrib     : TcGlobals -> Env.BuiltinAttribInfo -> Attribs -> bool option
val TryFindStringAttrib   : TcGlobals -> Env.BuiltinAttribInfo -> Attribs -> string option
val TryFindInt32Attrib    : TcGlobals -> Env.BuiltinAttribInfo -> Attribs -> int32 option

val TyconRefTryBindAttrib  : TcGlobals -> Env.BuiltinAttribInfo -> TyconRef -> ((ILAttribElem list * ILAttributeNamedArg list) -> 'a option) -> (Attrib -> 'a option) -> 'a option
val TyconRefHasAttrib      : TcGlobals -> Env.BuiltinAttribInfo -> TyconRef -> bool

val IsSignatureDataVersionAttr  : ILAttribute -> bool
val ILThingHasExtensionAttribute : ILAttributes -> bool
val TryFindAutoOpenAttr           : ILAttribute -> string option 
val TryFindInternalsVisibleToAttr : ILAttribute -> string option 
val IsMatchingSignatureDataVersionAttr : ILVersionInfo -> ILAttribute -> bool


val mkCompilationMappingAttr                         : TcGlobals -> int -> ILAttribute
val mkCompilationMappingAttrWithSeqNum               : TcGlobals -> int -> int -> ILAttribute
val mkCompilationMappingAttrWithVariantNumAndSeqNum  : TcGlobals -> int -> int -> int             -> ILAttribute
val mkCompilationArgumentCountsAttr                  : TcGlobals -> int list -> ILAttribute
val mkCompilationSourceNameAttr                      : TcGlobals -> string -> ILAttribute
val mkSignatureDataVersionAttr                       : TcGlobals -> ILVersionInfo -> ILAttribute
val mkCompilerGeneratedAttr                          : TcGlobals -> int -> ILAttribute

val isDefinitelyNotSerializable : TcGlobals -> TType -> bool

//-------------------------------------------------------------------------
// More common type construction
//------------------------------------------------------------------------- 

val isByrefTy : TcGlobals -> TType -> bool
val destByrefTy : TcGlobals -> TType -> TType

val isByrefLikeTyconRef : TcGlobals -> TyconRef -> bool
val isByrefLikeTy : TcGlobals -> TType -> bool

//-------------------------------------------------------------------------
// Tuple constructors/destructors
//------------------------------------------------------------------------- 

val isTupleExpr : Expr -> bool
val tryDestTuple : Expr -> Exprs
val mkTupled : TcGlobals -> range -> Exprs -> TType list -> Expr 
val mkTupledNoTypes : TcGlobals -> range -> Exprs -> Expr 
val mkTupledTy : TcGlobals -> TType list -> TType
val mkTupledVarsTy : TcGlobals -> Val list -> TType
val mkTupledVars : TcGlobals -> range -> Val list -> Expr 
val mkMethodTy : TcGlobals -> TType list list -> TType -> TType

//-------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

val AdjustValForExpectedArity : TcGlobals -> range -> ValRef -> ValUseFlag -> ValReprInfo -> Expr * TType
val AdjustValToTopVal : Val -> ParentRef -> ValReprInfo -> unit
val LinearizeTopMatch : TcGlobals -> ParentRef -> Expr -> Expr
val AdjustPossibleSubsumptionExpr : TcGlobals -> Expr -> Exprs -> (Expr * Exprs) option
val NormalizeAndAdjustPossibleSubsumptionExprs : TcGlobals -> Expr -> Expr

//-------------------------------------------------------------------------
// XmlDoc signatures, used by both VS mode and XML-help emit
//------------------------------------------------------------------------- 

val buildAccessPath : CompilationPath option -> string

val XmlDocArgsEnc : TcGlobals -> Typars * Typars -> TType list -> string
val XmlDocSigOfVal : TcGlobals -> string -> Val -> string
val XmlDocSigOfUnionCase : string -> string -> string -> string
val XmlDocSigOfField : string -> string -> string -> string
val XmlDocSigOfTycon : string -> Tycon -> string
val XmlDocSigOfSubModul : string -> string
val XmlDocSigOfEntity : EntityRef -> string


//---------------------------------------------------------------------------
// Resolve static optimizations
//------------------------------------------------------------------------- 

val DecideStaticOptimizations : Env.TcGlobals -> StaticOptimization list -> int
val mkStaticOptimizationExpr     : Env.TcGlobals -> StaticOptimization list * Expr * Expr * range -> Expr

//---------------------------------------------------------------------------
// Build for loops
//------------------------------------------------------------------------- 

val mkFastForLoop : Env.TcGlobals -> SequencePointInfoForForLoop * range * Val * Expr * bool * Expr * Expr -> Expr

//---------------------------------------------------------------------------
// Active pattern helpers
//------------------------------------------------------------------------- 

type ActivePatternElemRef with 
    member Name : string

val TryGetActivePatternInfo  : ValRef -> PrettyNaming.ActivePatternInfo option
val mkChoiceCaseRef : Env.TcGlobals -> range -> int -> int -> UnionCaseRef

type PrettyNaming.ActivePatternInfo with 
    member Names : string list 
    member IsTotal: bool

    member ResultType : Env.TcGlobals -> range -> TType list -> TType
    member OverallType : Env.TcGlobals -> range -> TType -> TType list -> TType

//---------------------------------------------------------------------------
// Structural rewrites
//------------------------------------------------------------------------- 

[<NoEquality; NoComparison>]
type ExprRewritingEnv = 
    {PreIntercept: ((Expr -> Expr) -> Expr -> Expr option) option;
     PostTransform: Expr -> Expr option;
     IsUnderQuotations: bool }    

val RewriteExpr : ExprRewritingEnv -> Expr -> Expr
val RewriteImplFile : ExprRewritingEnv -> TypedImplFile -> TypedImplFile

val IsGenericValWithGenericContraints: TcGlobals -> Val -> bool

type Entity with 
    member HasInterface : TcGlobals -> TType -> bool
    member HasOverride : TcGlobals -> string -> TType list -> bool

type EntityRef with 
    member HasInterface : TcGlobals -> TType -> bool
    member HasOverride : TcGlobals -> string -> TType list -> bool

val (|AttribBitwiseOrExpr|_|) : TcGlobals -> Expr -> (Expr * Expr) option
val (|EnumExpr|_|) : TcGlobals -> Expr -> Expr option
val (|TypeOfExpr|_|) : TcGlobals -> Expr -> TType option
val (|TypeDefOfExpr|_|) : TcGlobals -> Expr -> TType option

val EvalAttribArg: TcGlobals -> Expr -> Expr
val EvaledAttribExprEquality : TcGlobals -> Expr -> Expr -> bool
val IsSimpleSyntacticConstantExpr: TcGlobals -> Expr -> bool

val (|ExtractAttribNamedArg|_|) : string -> AttribNamedArg list -> AttribExpr option 
val (|AttribInt32Arg|_|) : AttribExpr -> int32 option
val (|AttribInt16Arg|_|) : AttribExpr -> int16 option
val (|AttribBoolArg|_|) : AttribExpr -> bool option
val (|AttribStringArg|_|) : AttribExpr -> string option
val (|Int32Expr|_|) : Expr -> int32 option


/// Determines types that are potentially known to satisfy the 'comparable' constraint and returns
/// a set of residual types that must also satisfy the constraint
val (|SpecialComparableHeadType|_|) : TcGlobals -> TType -> TType list option
val (|SpecialEquatableHeadType|_|) : TcGlobals -> TType -> TType list option
val (|SpecialNotEquatableHeadType|_|) : TcGlobals -> TType -> unit option

val DetectFastIntegerForLoops : TcGlobals -> Expr -> Expr
val TryEliminateDesugaredConstants : TcGlobals -> range -> Const -> Expr option

val ValIsExplicitImpl : TcGlobals -> Val -> bool
val ValRefIsExplicitImpl : TcGlobals -> ValRef -> bool

