(* (c) Microsoft Corporation. Apache 2.0 License  *)

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
