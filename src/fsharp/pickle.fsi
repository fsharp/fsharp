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

module internal Microsoft.FSharp.Compiler.Pickle 

open Internal.Utilities
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.Tast

// Fixup pickled data w.r.t. a set of CCU thunks indexed by name
[<NoEquality; NoComparison>]
type PickledDataWithReferences<'RawData> = 
    { /// The data that uses a collection of CcuThunks internally
      RawData: 'RawData; 
      /// The assumptions that need to be fixed up
      FixupThunks: list<CcuThunk> } 

    member Fixup : (CcuReference -> CcuThunk) -> 'RawData
    /// Like Fixup but loader may return None, in which case there is no fixup.
    member OptionalFixup: (CcuReference -> CcuThunk option) -> 'RawData
    

type WriterState 
type ReaderState 

type pickler<'T> = 'T -> WriterState -> unit
type unpickler<'T> = ReaderState -> 'T

val internal p_byte : int -> WriterState -> unit
val internal u_byte : ReaderState -> int
val internal p_bool : bool -> WriterState -> unit
val internal u_bool : ReaderState -> bool
val internal p_int : int -> WriterState -> unit
val internal u_int : ReaderState -> int
val internal p_string : string -> WriterState -> unit
val internal u_string : ReaderState -> string
val internal p_lazy : 'T pickler -> Lazy<'T> pickler
val internal u_lazy : 'T unpickler -> Lazy<'T> unpickler

val inline  internal p_tup2 : ('T1 pickler) -> ('T2 pickler) -> ('T1 * 'T2) pickler
val inline  internal p_tup3 : ('T1 pickler) -> ('T2 pickler) -> ('T3 pickler) -> ('T1 * 'T2 * 'T3) pickler
val inline  internal p_tup4 : ('T1 pickler) -> ('T2 pickler) -> ('T3 pickler) -> ('T4 pickler) -> ('T1 * 'T2 * 'T3 * 'T4) pickler
val inline  internal u_tup2 : ('T2 unpickler) -> ('T3 unpickler ) -> ('T2 * 'T3) unpickler
val inline  internal u_tup3 : ('T2 unpickler) -> ('T3 unpickler ) -> ('T4 unpickler ) -> ('T2 * 'T3 * 'T4) unpickler
val inline  internal u_tup4 : ('T2 unpickler) -> ('T3 unpickler ) -> ('T4 unpickler ) -> ('T5 unpickler) -> ('T2 * 'T3 * 'T4 * 'T5) unpickler
val internal p_array : 'T pickler -> 'T[] pickler
val internal u_array : 'T unpickler -> 'T[] unpickler
val internal p_namemap : 'T pickler -> Lib.NameMap<'T> pickler
val internal u_namemap : 'T unpickler -> Lib.NameMap<'T> unpickler

val pickleObjWithDanglingCcus : string -> Env.TcGlobals -> scope:CcuThunk -> ('T pickler) -> 'T -> byte[]
val internal unpickleObjWithDanglingCcus : string -> viewedScope:ILScopeRef -> ('T  unpickler) -> byte[] ->  PickledDataWithReferences<'T>

val internal p_const : Const pickler
val internal u_const : Const unpickler
val internal p_vref : string -> ValRef pickler
val internal u_vref : ValRef unpickler
val internal p_tcref : string -> TyconRef pickler
val internal u_tcref : TyconRef unpickler
val internal p_ucref : UnionCaseRef pickler
val internal u_ucref : UnionCaseRef unpickler
val internal p_expr : Expr pickler
val internal u_expr : Expr unpickler
val internal p_typ : TType pickler
val internal u_typ : TType unpickler

val internal pickleModuleOrNamespace : pickler<ModuleOrNamespace>
val internal unpickleModuleOrNamespace : ReaderState -> ModuleOrNamespace
val internal pickleModuleInfo : pickler<PickledModuleInfo>
val internal unpickleModuleInfo : ReaderState -> PickledModuleInfo
