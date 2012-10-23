(* (c) Microsoft Corporation. All rights reserved  *)

// -------------------------------------------------------------------- 
// Internal use only.  Erase discriminated unions.
// -------------------------------------------------------------------- 

module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.EraseIlxUnions

open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types

val ConvModule: ILGlobals -> ILModuleDef -> ILModuleDef 
val GetILTypeForAlternative : IlxUnionSpec -> int -> ILType
