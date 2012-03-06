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

/// Various constants and utilities used when parsing the ILASM format for IL
module internal Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiConstants

open Internal.Utilities

open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types 
open Microsoft.FSharp.Compiler.AbstractIL.IL 

// -------------------------------------------------------------------- 
// IL Parser state - must be initialized before parsing a module
// -------------------------------------------------------------------- 

val parseILGlobals: ILGlobals ref

// -------------------------------------------------------------------- 
// IL Lexer and pretty-printer tables
// -------------------------------------------------------------------- 

type NoArgInstr = unit -> ILInstr
type Int32Instr = int32 -> ILInstr
type Int32Int32Instr = int32 * int32 -> ILInstr
type Int64Instr = int64 -> ILInstr
type DoubleInstr = ILConst -> ILInstr
type MethodSpecInstr = ILMethodSpec * ILVarArgs -> ILInstr
type TypeInstr = ILType -> ILInstr
type IntTypeInstr = int * ILType -> ILInstr
type ValueTypeInstr = ILType -> ILInstr
type StringInstr = string -> ILInstr
type TokenInstr = ILToken -> ILInstr
type SwitchInstr = ILCodeLabel list * ILCodeLabel -> ILInstr

type InstrTable<'T> = (string list * 'T) list
type LazyInstrTable<'T> = Lazy<InstrTable<'T>>

val NoArgInstrs:  LazyInstrTable<NoArgInstr>
val Int64Instrs:  LazyInstrTable<Int64Instr>
val Int32Instrs:  LazyInstrTable<Int32Instr>
val Int32Int32Instrs: LazyInstrTable<Int32Int32Instr>
val DoubleInstrs:  LazyInstrTable<DoubleInstr>
val MethodSpecInstrs:  LazyInstrTable<MethodSpecInstr>
val StringInstrs:  LazyInstrTable<StringInstr>
val TokenInstrs:  LazyInstrTable<TokenInstr>
val TypeInstrs:  LazyInstrTable<TypeInstr>
val IntTypeInstrs:  LazyInstrTable<IntTypeInstr>
val ValueTypeInstrs:  LazyInstrTable<ValueTypeInstr>

#if DEBUG
val wordsOfNoArgInstr : (ILInstr -> string list)
val isNoArgInstr : (ILInstr -> bool)
#endif



