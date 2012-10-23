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


/// Generate the hash/compare functions we add to user-defined types by default.
module internal Microsoft.FSharp.Compiler.Augment 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler 

open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Env

val CheckAugmentationAttribs : bool -> TcGlobals -> Import.ImportMap -> Tycon -> unit
val TyconIsCandidateForAugmentationWithCompare  : TcGlobals -> Tycon -> bool
val TyconIsCandidateForAugmentationWithEquals   : TcGlobals -> Tycon -> bool
val TyconIsCandidateForAugmentationWithHash     : TcGlobals -> Tycon -> bool

val MakeValsForCompareAugmentation                : TcGlobals -> TyconRef -> Val * Val
val MakeValsForCompareWithComparerAugmentation    : TcGlobals -> TyconRef -> Val
val MakeValsForEqualsAugmentation                 : TcGlobals -> TyconRef -> Val * Val
val MakeValsForEqualityWithComparerAugmentation   : TcGlobals -> TyconRef -> Val * Val * Val

val MakeBindingsForCompareAugmentation               : TcGlobals -> Tycon -> Binding list
val MakeBindingsForCompareWithComparerAugmentation   : TcGlobals -> Tycon -> Binding list
val MakeBindingsForEqualsAugmentation                : TcGlobals -> Tycon -> Binding list
val MakeBindingsForEqualityWithComparerAugmentation  : TcGlobals -> Tycon -> Binding list

/// This predicate can be used once type inference is complete, before then it is an approximation
/// that doesn't assert any new constraints
val TypeDefinitelyHasEquality : TcGlobals -> TType -> bool

