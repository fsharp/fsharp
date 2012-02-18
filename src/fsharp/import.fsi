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


module internal Microsoft.FSharp.Compiler.Import

open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.AbstractIL.IL



type AssemblyLoader = 
    abstract LoadAssembly : range * ILAssemblyRef -> CcuResolutionResult


[<SealedAttribute ()>]
type ImportMap =
    new : g:Env.TcGlobals * assemMap:AssemblyLoader -> ImportMap
    member assemMap : AssemblyLoader
    member g : Env.TcGlobals

val internal ImportILTypeRef : ImportMap -> range -> ILTypeRef -> TyconRef
val internal ImportILType : ImportMap -> range -> TType list -> ILType -> TType
val internal ImportIlTypars : (unit -> ImportMap) -> range -> ILScopeRef -> TType list -> ILGenericParameterDef list -> Typar list
val internal ImportIlAssembly : (unit -> ImportMap) * range * (ILScopeRef -> ILModuleDef) * ILScopeRef * sourceDir:string * filename: string option * ILModuleDef -> CcuThunk
val internal ImportIlAssemblyTypeForwarders : (unit -> ImportMap) * range * ILExportedTypesAndForwarders -> Lazy<Map<(string array * string), EntityRef>>
