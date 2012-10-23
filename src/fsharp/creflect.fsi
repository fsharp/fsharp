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


module internal Microsoft.FSharp.Compiler.QuotationTranslator
open Microsoft.FSharp.Compiler 

// Convert quoted TAST data structures to structures ready for pickling 

open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops

[<Sealed>]
type QuotationTranslationEnv =
   static member Empty : QuotationTranslationEnv
   member BindTypars : Typars -> QuotationTranslationEnv

exception InvalidQuotedTerm of exn
exception IgnoringPartOfQuotedTermWarning of string * Range.range

[<RequireQualifiedAccess>]
type IsReflectedDefinition =
|   Yes
|   No
val ConvExprPublic : Env.TcGlobals * Import.ImportMap * CcuThunk * IsReflectedDefinition -> QuotationTranslationEnv -> Expr -> TType list * Expr list * QuotationPickler.ExprData 
val ConvMethodBase  : Env.TcGlobals * Import.ImportMap * CcuThunk -> QuotationTranslationEnv ->  string * Val  -> QuotationPickler.MethodBaseData


