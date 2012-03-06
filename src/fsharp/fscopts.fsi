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


module internal Microsoft.FSharp.Compiler.Fscopts

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Ilxgen
open Microsoft.FSharp.Compiler.Import
open Microsoft.FSharp.Compiler.Opt
open Microsoft.FSharp.Compiler.Env

val DisplayBannerText : TcConfigBuilder -> unit

//val GetCompilerOptions : TcConfigBuilder -> CompilerOption list -> CompilerOption list
val GetCoreFscCompilerOptions     : TcConfigBuilder -> CompilerOptionBlock list
val GetCoreFsiCompilerOptions     : TcConfigBuilder -> CompilerOptionBlock list
val GetCoreServiceCompilerOptions : TcConfigBuilder -> CompilerOptionBlock list

// Expose the "setters" for some user switches, to enable setting of defaults
val SetOptimizeSwitch : TcConfigBuilder -> OptionSwitch -> unit
val SetTailcallSwitch : TcConfigBuilder -> OptionSwitch -> unit
val SetDebugSwitch    : TcConfigBuilder -> string option -> OptionSwitch -> unit
val PrintOptionInfo   : TcConfigBuilder -> unit

val fsharpModuleName : CompilerTarget -> string -> string


val InitialOptimizationEnv : TcImports -> IncrementalOptimizationEnv
val AddExternalCcuToOpimizationEnv : IncrementalOptimizationEnv -> ImportedAssembly -> IncrementalOptimizationEnv
val ApplyAllOptimizations : TcConfig * TcGlobals * string * ImportMap * bool * IncrementalOptimizationEnv * CcuThunk * TypedAssembly -> TypedAssembly * Opt.LazyModuleInfo * IncrementalOptimizationEnv

val IlxgenEnvInit : TcConfig * TcImports * TcGlobals * CcuThunk -> IlxGenEnv
val GenerateIlxCode : IlxBackend * bool * bool * bool * TcGlobals * TcConfig * ImportMap * TypeChecker.TopAttribs * TypedAssembly * CcuThunk * string * IlxGenEnv -> CodegenResults

// Used during static linking
val NormalizeAssemblyRefs : TcImports -> (AbstractIL.IL.ILScopeRef -> AbstractIL.IL.ILScopeRef)

// Miscellany
val ignoreFailureOnMono1_1_16 : (unit -> unit) -> unit
val mutable enableConsoleColoring : bool
val DoWithErrorColor : bool -> (unit -> 'a) -> 'a
val ReportTime : TcConfig -> string -> unit
val abbrevFlagSet : TcConfigBuilder -> bool -> Set<string>
val PostProcessCompilerArgs : string Set -> string [] -> string list
