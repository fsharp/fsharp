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

/// A set of "IL rewrites" ("morphs").  These map each sub-construct
/// of particular ILTypeDefs.  The morphing functions are passed
/// some details about the context in which the item being
/// morphed occurs, e.g. the module being morphed itself, the
/// ILTypeDef (possibly nested) where the item occurs, 
/// the ILMethodDef (if any) where the item occurs. etc.
module internal Microsoft.FSharp.Compiler.AbstractIL.Morphs 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types 
open Microsoft.FSharp.Compiler.AbstractIL.IL 

type 'T morph = 'T -> 'T

/// Morph each scope reference inside a type signature 
val morphILScopeRefsInILTypeRef: ILScopeRef morph -> ILTypeRef -> ILTypeRef 

val morphILMethodDefs: ILMethodDef morph -> ILMethodDefs -> ILMethodDefs
/// nb. does not do nested tdefs
val morphILTypeDefs: ILTypeDef morph -> ILTypeDefs -> ILTypeDefs 

val morphExpandILTypeDefs: (ILTypeDef -> ILTypeDef list) -> ILTypeDefs -> ILTypeDefs

/// Morph all tables of ILTypeDefs in "ILModuleDef"
val morphILTypeDefsInILModule: ILTypeDefs morph -> ILModuleDef -> ILModuleDef

/// Morph all type references throughout an entire module.
val morphILTypeRefsInILModuleMemoized:  ILGlobals -> ILTypeRef morph ->  ILModuleDef ->  ILModuleDef

val morphILScopeRefsInILModuleMemoized: ILGlobals -> ILScopeRef morph ->  ILModuleDef ->  ILModuleDef

val morphILMethodBody: ILMethodBody morph -> ILLazyMethodBody -> ILLazyMethodBody
val morphIlxClosureInfo: ILMethodBody morph -> IlxClosureInfo ->  IlxClosureInfo
val morphILInstrsInILCode: (ILInstr -> ILInstr list) -> ILCode -> ILCode

[<Struct; NoComparison; NoEquality>]
type InstrMorph = 
    new : ILInstr list -> InstrMorph
    new : ILCode -> InstrMorph

val morphExpandILInstrsInILCode: (ILCodeLabel -> ILCodeLabel -> ILInstr -> InstrMorph) -> ILCode -> ILCode

// REVIEW: Consider removing Post-Dev11 M3
val enablemorphCustomAttributeData : unit -> unit
val disablemorphCustomAttributeData : unit -> unit
