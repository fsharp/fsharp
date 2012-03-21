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

/// Internal use only.  Erase closures
module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.EraseIlxFuncs

open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types

val ConvModule: ILGlobals -> ILModuleDef -> ILModuleDef 

type cenv
val mkILFuncTy : cenv -> ILType -> ILType -> ILType
val mkILTyFuncTy : cenv -> ILType
val new_cenv : ILGlobals -> cenv
val mkTyOfLambdas: cenv -> IlxClosureLambdas -> ILType
