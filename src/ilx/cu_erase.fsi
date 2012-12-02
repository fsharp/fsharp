(* (c) Microsoft Corporation. Apache 2.0 License  *)

// -------------------------------------------------------------------- 
// Internal use only.  Erase discriminated unions.
// -------------------------------------------------------------------- 

module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.EraseIlxUnions

open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types

val ConvModule: ILGlobals -> ILModuleDef -> ILModuleDef 
val GetILTypeForAlternative : IlxUnionSpec -> int -> ILType
