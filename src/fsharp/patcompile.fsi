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

module internal Microsoft.FSharp.Compiler.Patcompile

open Internal.Utilities
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Range



/// What should the decision tree contain for any incomplete match? 
type ActionOnFailure = 
    | ThrowIncompleteMatchException 
    | IgnoreWithWarning 
    | Throw 
    | Rethrow 
    | FailFilter

[<NoEquality; NoComparison>]
/// Represents the typechecked, elaborated form of a pattern, prior to pattern-match compilation.
type Pattern =
    | TPat_const of Const * range
    | TPat_wild of range
    | TPat_as of  Pattern * PatternValBinding * range
    | TPat_disjs of  Pattern list * range
    | TPat_conjs of  Pattern list * range
    | TPat_query of (Expr * TType list * (ValRef * TypeInst) option * int * PrettyNaming.ActivePatternInfo) * Pattern * range
    | TPat_unioncase of UnionCaseRef * TypeInst * Pattern list * range
    | TPat_exnconstr of TyconRef * Pattern list * range
    | TPat_tuple of  Pattern list * TType list * range
    | TPat_array of  Pattern list * TType * range
    | TPat_recd of TyconRef * TypeInst * Pattern list * range
    | TPat_range of char * char * range
    | TPat_null of range
    | TPat_isinst of TType * TType * PatternValBinding option * range
    member Range : range

and PatternValBinding = 
    | PBind of Val * TypeScheme

and TypedMatchClause =  
    | TClause of Pattern * Expr option * DecisionTreeTarget * range

/// Compile a pattern into a decision tree and a set of targets.
val internal CompilePattern : 
    Env.TcGlobals ->
    Tastops.DisplayEnv ->
    Import.ImportMap -> 
    // range of the expression we are matching on 
    range ->  
    // range to report "incomplete match" on
    range ->  
    // warn on unused? 
    bool ->   
    ActionOnFailure -> 
    // the value being matched against, perhaps polymorphic 
    Val * Typars -> 
    // input type-checked syntax of pattern matching
    TypedMatchClause list ->
    // input type 
    TType -> 
    // result type
    TType ->
      // produce TAST nodes
      DecisionTree * DecisionTreeTarget list

exception internal MatchIncomplete of bool * (string * bool) option * range
exception internal RuleNeverMatched of range
