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


module internal Microsoft.FSharp.Compiler.Opt

open Internal.Utilities
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Env 
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 

type OptimizationSettings = 
    { abstractBigTargets : bool
      jitOptUser : bool option
      localOptUser : bool option
      crossModuleOptUser : bool option
      bigTargetSize : int
      veryBigExprSize : int 
      lambdaInlineThreshold : int
      reportingPhase : bool;
      reportNoNeedToTailcall: bool
      reportFunctionSizes : bool
      reportHasEffect : bool
      reportTotalSizes : bool }

    member jitOpt : unit -> bool 
    member localOpt : unit -> bool 
    static member Defaults : OptimizationSettings

/// Optimization information 
type ModuleInfo
type LazyModuleInfo = Lazy<ModuleInfo>

#if NO_COMPILER_BACKEND
#else
[<Sealed>]
type IncrementalOptimizationEnv =
    static member Empty : IncrementalOptimizationEnv

/// For building optimization environments incrementally 
val internal BindCcu : CcuThunk -> LazyModuleInfo -> IncrementalOptimizationEnv -> TcGlobals -> IncrementalOptimizationEnv

/// The entry point. Boolean indicates 'incremental extension' in FSI 
val internal OptimizeImplFile : OptimizationSettings *  CcuThunk (* scope *) * Env.TcGlobals * ConstraintSolver.TcValF * Import.ImportMap * IncrementalOptimizationEnv * isIncrementalFragment: bool * emitTaicalls: bool * TypedImplFile -> IncrementalOptimizationEnv * TypedImplFile * LazyModuleInfo

/// Displaying optimization data
val internal moduleInfoL : TcGlobals -> LazyModuleInfo -> Layout.layout

/// Saving and re-reading optimization information 
val p_LazyModuleInfo : LazyModuleInfo -> Pickle.WriterState -> unit 

/// Rewrite the modul info using the export remapping 
val RemapLazyModulInfo : Env.TcGlobals -> Tastops.Remap -> (LazyModuleInfo -> LazyModuleInfo)
val AbstractLazyModulInfoToEssentials : (LazyModuleInfo -> LazyModuleInfo)
val UnionModuleInfos: seq<LazyModuleInfo> -> LazyModuleInfo
val ExprHasEffect: Env.TcGlobals -> Expr -> bool
#endif

val internal u_LazyModuleInfo : Pickle.ReaderState -> LazyModuleInfo
