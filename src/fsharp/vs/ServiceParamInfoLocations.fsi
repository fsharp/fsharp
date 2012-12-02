//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// API to the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.FSharp.Compiler 

[<Sealed>]
type internal NoteworthyParamInfoLocations =
    member LongId : string list
    member LongIdStartLocation : int * int
    member LongIdEndLocation : int * int
    member OpenParenLocation : int * int
    member TupleEndLocations : (int * int)[]  // locations of commas and close parenthesis (or, last char of last arg, if no final close parenthesis)
    member IsThereACloseParen : bool   // false if either this is a call without parens "f x" or the parser recovered as in "f(x,y"
    member NamedParamNames : string[]  // null, or a name if an actual named parameter; f(0,a=4,?b=None) would be [|null;"a";"b"|]

// implementation details used by other code in the compiler    
module internal NoteworthyParamInfoLocationsImpl =
    val internal FindNoteworthyParamInfoLocations : int * int * Ast.ParsedInput -> NoteworthyParamInfoLocations option
