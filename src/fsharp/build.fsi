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

/// Loading initial context, reporting errors etc.
module internal Microsoft.FSharp.Compiler.Build

open System.Text
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.TypeChecker
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.MSBuildResolver
open Microsoft.FSharp.Compiler.Env



#if DEBUG
val showAssertForUnexpectedException : bool ref
#endif

/// Signature file suffixes
val sigSuffixes : string list

/// Implementation file suffixes
val implSuffixes : string list

/// Script file suffixes
val scriptSuffixes : string list

val IsScript : string -> bool

val IsInvalidPath : string -> bool

/// File suffixes where #light is the default
val lightSyntaxDefaultExtensions : string list

//----------------------------------------------------------------------------
// Parsing inputs
//--------------------------------------------------------------------------
  
val QualFileNameOfUniquePath : range * string list -> Ast.QualifiedNameOfFile

val PrependPathToInput : Ast.Ident list -> Ast.Input -> Ast.Input

val ParseInput : (UnicodeLexing.Lexbuf -> Parser.token) * ErrorLogger * UnicodeLexing.Lexbuf * string option * string * isLastCompiland: bool -> Ast.Input



//----------------------------------------------------------------------------
// Errors
//--------------------------------------------------------------------------

type ErrorStyle = 
    | DefaultErrors 
    | EmacsErrors 
    | TestErrors 
    | VSErrors
    

val RangeOfError : PhasedError -> range option
val SplitRelatedErrors : PhasedError -> PhasedError * PhasedError list
val OutputPhasedError : StringBuilder -> PhasedError -> bool -> unit
val SanitizeFileName : filename:string -> implicitIncludeDir:string -> string
val OutputErrorOrWarning : implicitIncludeDir:string * showFullPaths: bool * flattenErrors: bool * errorStyle: ErrorStyle *  warning:bool -> StringBuilder -> PhasedError -> unit
val OutputErrorOrWarningContext : prefix:string -> fileLineFunction:(string -> int -> string) -> StringBuilder -> PhasedError -> unit


//----------------------------------------------------------------------------
// Options and configuration
//--------------------------------------------------------------------------

// For command-line options that can be suffixed with +/-
type OptionSwitch =
    | On
    | Off

/// The spec value describes the action of the argument,
/// and whether it expects a following parameter.
type OptionSpec = 
    | OptionClear of bool ref
    | OptionFloat of (float -> unit)
    | OptionInt of (int -> unit)
    | OptionSwitch of (OptionSwitch -> unit)
    | OptionIntList of (int -> unit)
    | OptionIntListSwitch of (int -> OptionSwitch -> unit)
    | OptionRest of (string -> unit)
    | OptionSet of bool ref
    | OptionString of (string -> unit)
    | OptionStringList of (string -> unit)
    | OptionStringListSwitch of (string -> OptionSwitch -> unit)
    | OptionUnit of (unit -> unit)
    | OptionHelp of (CompilerOptionBlock list -> unit)                      // like OptionUnit, but given the "options"
    | OptionGeneral of (string list -> bool) * (string list -> string list) // Applies? * (ApplyReturningResidualArgs)
and  CompilerOption      = CompilerOption of string * string * OptionSpec * Option<exn> * string option
and  CompilerOptionBlock = PublicOptions  of string * CompilerOption list | PrivateOptions of CompilerOption list

val printCompilerOptionBlocks : CompilerOptionBlock list -> unit  // for printing usage
val dumpCompilerOptionBlocks  : CompilerOptionBlock list -> unit  // for QA
val filterCompilerOptionBlock : (CompilerOption -> bool) -> CompilerOptionBlock -> CompilerOptionBlock

exception AssemblyNotResolved of (*originalName*) string * range
exception FileNameNotResolved of (*filename*) string * (*description of searched locations*) string * range
exception DeprecatedCommandLineOptionFull of string * range
exception DeprecatedCommandLineOptionForHtmlDoc of string * range
exception DeprecatedCommandLineOptionSuggestAlternative of string * string * range
exception DeprecatedCommandLineOptionNoDescription of string * range
exception InternalCommandLineOption of string * range
exception HashLoadedSourceHasIssues of (*warnings*) exn list * (*errors*) exn list * range
exception HashLoadedScriptConsideredSource of range  

type AssemblyReference = 
    | AssemblyReference of range * string 
    member Range : range
    member Text : string

type AssemblyResolution = 
      {/// The original reference to the assembly.
       originalReference : AssemblyReference
       /// Path to the resolvedFile
       resolvedPath : string    
       /// Search path used to find this spot.
       resolvedFrom : ResolvedFrom
       /// The qualified name of the assembly
       fusionName : string
       /// Name of the redist, if any, that the assembly was found in.
       redist : string 
       /// Whether or not this is an installed system assembly (for example, System.dll)
       sysdir : bool
       // Lazily populated ilAssemblyRef for this reference. 
       ilAssemblyRef : ILAssemblyRef option ref  }
    
type CompilerTarget = 
    | WinExe 
    | ConsoleExe 
    | Dll 
    | Module
    member IsExe : bool
    
type ResolveLibFileMode = 
    | Speculative 
    | ReportErrors

type VersionFlag = 
    | VersionString of string
    | VersionFile of string
    | VersionNone
    member GetVersionInfo : (*implicitIncludeDir:*)string -> ILVersionInfo
    member GetVersionString : (*implicitIncludeDir:*)string -> string
         
     
type TcConfigBuilder =
    { mutable mscorlibAssemblyName: string;
      mutable autoResolveOpenDirectivesToDlls: bool;
      mutable noFeedback: bool;
      mutable stackReserveSize: int32 option;
      mutable implicitIncludeDir: string;
      mutable openBinariesInMemory: bool;
      mutable openDebugInformationForLaterStaticLinking: bool;
      defaultFSharpBinariesDir: string;
      mutable compilingFslib: bool;
      mutable compilingFslib20: string option;
      mutable compilingFslib40: bool;
      mutable useIncrementalBuilder: bool;
      mutable includes: string list;
      mutable implicitOpens: string list;
      mutable useFsiAuxLib: bool;
      mutable framework: bool;
      mutable resolutionEnvironment : ResolutionEnvironment
      mutable implicitlyResolveAssemblies : bool
      mutable addVersionSpecificFrameworkReferences : bool
      /// Set if the user has explicitly turned indentation-aware syntax on/off
      mutable light: bool option;
      mutable conditionalCompilationDefines: string list;
      /// Sources added into the build with #load
      mutable loadedSources: (range * string) list;
      
      mutable referencedDLLs: AssemblyReference  list;
      optimizeForMemory: bool;
      mutable inputCodePage: int option;
      mutable embedResources : string list;
      mutable globalWarnAsError: bool;
      mutable globalWarnLevel: int;
      mutable specificWarnOff: int list; 
      mutable specificWarnOn: int list; 
      mutable specificWarnAsError: int list 
      mutable specificWarnAsWarn : int list
      mutable mlCompatibility:bool;
      mutable checkOverflow:bool;
      mutable showReferenceResolutions:bool;
      mutable outputFile : string option;
      mutable resolutionFrameworkRegistryBase : string;
      mutable resolutionAssemblyFoldersSuffix : string; 
      mutable resolutionAssemblyFoldersConditions : string;          
      mutable platform : ILPlatform option
      mutable useMonoResolution : bool
      mutable target : CompilerTarget
      mutable debuginfo : bool
      mutable testFlagEmitFeeFeeAs100001 : bool
      mutable dumpDebugInfo : bool
      mutable debugSymbolFile : string option
      mutable typeCheckOnly : bool
      mutable parseOnly : bool
      mutable simulateException : string option
      mutable printAst : bool
      mutable tokenizeOnly : bool
      mutable testInteractionParser : bool
      mutable reportNumDecls : bool
      mutable printSignature : bool
      mutable printSignatureFile : string
      mutable xmlDocOutputFile : string option
      mutable stats : bool
      mutable generateFilterBlocks : bool 
      mutable signer : string option
      mutable container : string option
      mutable delaysign : bool
      mutable version : VersionFlag 
      mutable metadataVersion : string option
      mutable standalone : bool
      mutable extraStaticLinkRoots : string list 
      mutable noSignatureData : bool
      mutable onlyEssentialOptimizationData : bool
      mutable useOptimizationDataFile : bool
      mutable useSignatureDataFile : bool
      mutable jitTracking : bool
      mutable ignoreSymbolStoreSequencePoints : bool
      mutable internConstantStrings : bool
      mutable extraOptimizationIterations : int
      mutable win32res : string 
      mutable win32manifest : string
      mutable includewin32manifest : bool
      mutable linkResources : string list
      mutable showFullPaths : bool
      mutable errorStyle : ErrorStyle
      mutable utf8output : bool
      mutable flatErrors : bool
      mutable maxErrors : int
      mutable abortOnError : bool
      mutable baseAddress : int32 option
 #if DEBUG
      mutable writeGeneratedILFiles : bool (* write il files? *)  
      mutable showOptimizationData : bool
#endif
      mutable showTerms     : bool 
      mutable writeTermsToFiles : bool 
      mutable doDetuple     : bool 
      mutable doTLR         : bool 
      mutable doFinalSimplify : bool
      mutable optsOn        : bool 
      mutable optSettings   : Opt.OptimizationSettings 
      mutable emitTailcalls : bool
      mutable lcid         : int option
      mutable productNameForBannerText : string
      mutable showBanner  : bool
      mutable showTimes : bool
      mutable pause : bool 
      mutable indirectCallArrayMethods : bool
      mutable noDebugData : bool}
    static member CreateNew : string * bool * string  -> TcConfigBuilder
    member DecideNames : string list -> (*outfile*)string * (*pdbfile*)string option * (*assemblyName*)string 
    member TurnWarningOff : range * string -> unit
    member TurnWarningOn : range * string -> unit
    member AddIncludePath : range * string -> unit
    member AddReferencedAssemblyByPath : range * string -> unit
    member AddLoadedSource : range * string -> unit
    member AddEmbeddedResource : string -> unit
    
    static member SplitCommandLineResourceInfo : string -> string * string * ILResourceAccess


    
[<Sealed>]
// Immutable TcConfig
type TcConfig =
    member mscorlibAssemblyName: string;
    member autoResolveOpenDirectivesToDlls: bool;
    member noFeedback: bool;
    member stackReserveSize: int32 option;
    member implicitIncludeDir: string;
    member openBinariesInMemory: bool;
    member openDebugInformationForLaterStaticLinking: bool;
    member fsharpBinariesDir: string;
    member compilingFslib: bool;
    member compilingFslib20: string option;
    member compilingFslib40: bool;
    member useIncrementalBuilder: bool;
    member includes: string list;
    member implicitOpens: string list;
    member useFsiAuxLib: bool;
    member framework: bool;
    member implicitlyResolveAssemblies : bool
    /// Set if the user has explicitly turned indentation-aware syntax on/off
    member light: bool option;
    member conditionalCompilationDefines: string list;
    /// Sources added into the build with #load
    member loadedSources: (range * string) list;
    
    member referencedDLLs: AssemblyReference list;
    member optimizeForMemory: bool;
    member inputCodePage: int option;
    member embedResources : string list;
    member globalWarnAsError: bool;
    member globalWarnLevel: int;
    member specificWarnOn: int list; 
    member specificWarnOff: int list; 
    member specificWarnAsError: int list 
    member specificWarnAsWarn : int list
    member mlCompatibility:bool;
    member checkOverflow:bool;
    member showReferenceResolutions:bool;
    member outputFile : string option;
    member resolutionFrameworkRegistryBase : string;
    member resolutionAssemblyFoldersSuffix : string; 
    member resolutionAssemblyFoldersConditions : string;          
    member platform : ILPlatform option
    member useMonoResolution : bool
    member target : CompilerTarget
    member debuginfo : bool
    member testFlagEmitFeeFeeAs100001 : bool
    member dumpDebugInfo : bool
    member debugSymbolFile : string option
    member typeCheckOnly : bool
    member parseOnly : bool
    member simulateException : string option
    member printAst : bool
    member tokenizeOnly : bool
    member testInteractionParser : bool
    member reportNumDecls : bool
    member printSignature : bool
    member printSignatureFile : string
    member xmlDocOutputFile : string option
    member stats : bool
    member generateFilterBlocks : bool 
    member signer : string option
    member container : string option
    member delaysign : bool
    member version : VersionFlag 
    member metadataVersion : string option
    member standalone : bool
    member extraStaticLinkRoots : string list 
    member noSignatureData : bool
    member onlyEssentialOptimizationData : bool
    member useOptimizationDataFile : bool
    member useSignatureDataFile : bool
    member jitTracking : bool
    member ignoreSymbolStoreSequencePoints : bool
    member internConstantStrings : bool
    member extraOptimizationIterations : int
    member win32res : string 
    member win32manifest : string
    member includewin32manifest : bool
    member linkResources : string list
    member showFullPaths : bool
    member errorStyle : ErrorStyle
    member utf8output : bool
    member flatErrors : bool

    member maxErrors : int
    member baseAddress : int32 option
#if DEBUG
    member writeGeneratedILFiles : bool (* write il files? *)  
    member showOptimizationData : bool
#endif
    member showTerms     : bool 
    member writeTermsToFiles : bool 
    member doDetuple     : bool 
    member doTLR         : bool 
    member doFinalSimplify : bool
    member optSettings   : Opt.OptimizationSettings 
    member emitTailcalls : bool
    member lcid          : int option
    member optsOn        : bool 
    member productNameForBannerText : string
    member showBanner  : bool
    member showTimes : bool
    member pause : bool 
    member indirectCallArrayMethods : bool
    member noDebugData : bool    


    member ComputeLightSyntaxInitialStatus : string -> bool
    member ClrRoot : string list
    
    /// Get the loaded sources that exist and issue a warning for the ones that don't
    member GetAvailableLoadedSources : unit -> (range*string) list
    
    member ComputeCanContainEntryPoint : sourceFiles:string list -> bool list 

    /// File system query based on TcConfig settings
    member ResolveSourceFile : range * string -> string
    /// File system query based on TcConfig settings
    member MakePathAbsolute : string -> string
    static member Create : TcConfigBuilder * validate: bool -> TcConfig
    


//----------------------------------------------------------------------------
// Tables of referenced DLLs 
//--------------------------------------------------------------------------

type ImportedBinary = 
    { FileName: string;
      IsFSharpBinary: bool;
      RawMetadata: ILModuleDef;
      IsEstGenerated: bool;
      ILAssemblyRefs : ILAssemblyRef list;
      ILScopeRef: ILScopeRef ;}

type ImportedAssembly = 
    { ILScopeRef: ILScopeRef;
      FSharpViewOfMetadata: CcuThunk;
      AssemblyAutoOpenAttributes: string list;
      AssemblyInternalsVisibleToAttributes: string list;
      IsEstGenerated: bool;
      FSharpOptimizationData : Lazy<Option<Opt.LazyModuleInfo>> }

type UnresolvedReference = UnresolvedReference of string * AssemblyReference list

[<Sealed>] 
type TcAssemblyResolutions = 
    member GetAssemblyResolutions : unit -> AssemblyResolution list

    static member SplitNonFoundationalResolutions  : TcConfig -> AssemblyResolution list * AssemblyResolution list * UnresolvedReference list
    static member BuildFromPriorResolutions     : TcConfig * AssemblyResolution list -> TcAssemblyResolutions 
    

[<Sealed>]
type TcConfigProvider = 
    static member Constant : TcConfig -> TcConfigProvider
    static member BasedOnMutableBuilder : TcConfigBuilder -> TcConfigProvider

[<Sealed>] 
type TcImports =
    interface System.IDisposable
    //new : TcImports option -> TcImports
    member SetBase : TcImports -> unit
    member DllTable : NameMap<ImportedBinary> with get
    member GetCcuInfos : unit -> ImportedAssembly list
    member GetCcusInDeclOrder : unit -> CcuThunk list
    /// This excludes any framework imports (which may be shared between multiple builds)
    member GetCcuInfosExcludingBase : unit -> CcuThunk list 
    member FindDllInfo : range * string -> ImportedBinary
    member TryFindDllInfo : range * string * lookupOnly: bool -> option<ImportedBinary>
    member FindCcuFromAssemblyRef : range * ILAssemblyRef -> Tast.CcuResolutionResult
    member AssemblyLoader : Import.AssemblyLoader 
    member GetImportMap : unit -> Import.ImportMap

    /// File system query based on TcConfig settings
    member TryResolveLibFile : AssemblyReference * ResolveLibFileMode -> OperationResult<AssemblyResolution>
    /// File system query based on TcConfig settings
    member ResolveLibFile : AssemblyReference * ResolveLibFileMode -> AssemblyResolution
    /// Try to find the given assembly reference.
    member TryFindExistingFullyQualifiedPathFromAssemblyRef : ILAssemblyRef -> string option

    static member BuildFrameworkTcImports      : TcConfigProvider * AssemblyResolution list * AssemblyResolution list -> TcGlobals * TcImports
    static member BuildNonFrameworkTcImports   : TcConfigProvider * TcGlobals * TcImports * AssemblyResolution list -> TcImports
    static member BuildTcImports               : TcConfigProvider -> TcGlobals * TcImports

//----------------------------------------------------------------------------
// Special resources in DLLs
//--------------------------------------------------------------------------

val IsSignatureDataResource : ILResource -> bool
val IsOptimizationDataResource : ILResource -> bool
val IsReflectedDefinitionsResource : ILResource -> bool

val WriteSignatureData : TcConfig * TcGlobals * Tastops.Remap * CcuThunk * string -> ILResource
val WriteOptimizationData :  TcGlobals -> string -> CcuThunk * Opt.LazyModuleInfo -> ILResource

val GetNameOfILModule : ILModuleDef -> string

val GetFSharpCoreLibraryName : unit -> string

//----------------------------------------------------------------------------
// Finding and requiring DLLs
//--------------------------------------------------------------------------

val RequireDLL : TcImports -> TcEnv -> range -> string -> TcEnv * (ImportedBinary list * ImportedAssembly list)

//----------------------------------------------------------------------------
// Processing # commands
//--------------------------------------------------------------------------

val ProcessMetaCommandsFromInput : 
              ('T -> range * string -> 'T) * 
              ('T -> range * string -> 'T) * 
              ('T -> range * string -> unit) -> TcConfigBuilder -> Ast.Input -> 'T -> 'T


val GetScopedPragmasForInput : Ast.Input -> ScopedPragma list
val GetErrorLoggerFilteringByScopedPragmas : checkFile:bool * ScopedPragma list * ErrorLogger  -> ErrorLogger

val ApplyNoWarnsToTcConfig : TcConfig -> Ast.Input -> TcConfig
val ApplyMetaCommandsFromInputToTcConfig : TcConfig -> Ast.Input -> TcConfig
val GetResolvedAssemblyInformation : TcConfig -> AssemblyResolution list

//----------------------------------------------------------------------------
// Loading the default library sets
//--------------------------------------------------------------------------
                
val DefaultBasicReferencesForOutOfProjectSources : string list

//----------------------------------------------------------------------------
// Parsing inputs
//--------------------------------------------------------------------------
val ParseOneInputFile : TcConfig * Lexhelp.LexResourceManager * string list * string * isLastCompiland: bool * ErrorLogger * (*retryLocked*) bool -> Input option

//----------------------------------------------------------------------------
// Type checking and querying the type checking state
//--------------------------------------------------------------------------

val GetInitialTypecheckerEnv : string option -> range -> TcConfig -> TcImports -> TcGlobals -> TcEnv
                
[<Sealed>]
type TcState =
    member NiceNameGenerator : Ast.NiceNameGenerator
    member Ccu : CcuThunk
    member TcEnvFromSignatures : TcEnv
    member NextStateAfterIncrementalFragment : TcEnv -> TcState
    member TcEnvFromImpls : TcEnv

val TypecheckInitialState : 
    range * string * TcConfig * TcGlobals * Ast.NiceNameGenerator * TcEnv -> TcState

val TypecheckOneInputEventually :
    (unit -> bool) -> TcConfig -> TcImports -> bool -> TcGlobals 
      -> Ast.LongIdent option -> TcState -> Ast.Input  
           -> Eventually<(TcEnv * TopAttribs * Tast.TypedImplFile list) * TcState>

val TypecheckMultipleInputsFinish :
    (TcEnv * TopAttribs * 'T list) list * TcState
        -> (TcEnv * TopAttribs * 'T list) * TcState
    
val TypecheckClosedInputSetFinish :
    TypedImplFile list * TcState 
        -> TcState * TypedAssembly

val TypecheckClosedInputSet :
    (unit -> bool) * TcConfig * TcImports * TcGlobals * Ast.LongIdent option * TcState * Ast.Input  list * bool
        -> TcState * TopAttribs * Tast.TypedAssembly * TcEnv

val TypecheckSingleInputAndFinishEventually :
    (unit -> bool) * TcConfig * TcImports * TcGlobals * Ast.LongIdent option * TcState * Ast.Input 
        -> Eventually<(TcEnv * TopAttribs * Tast.TypedImplFile list) * TcState>

val ParseCompilerOptions : (string -> unit) -> CompilerOptionBlock list -> string list -> unit
val ReportWarning : int -> int list -> int list -> PhasedError -> bool
val ReportWarningAsError : int -> int list -> int list -> int list -> int list -> bool -> PhasedError -> bool

val highestInstalledNetFrameworkVersionMajorMinor : unit -> int * int * int * string

//----------------------------------------------------------------------------
// #load closure
//--------------------------------------------------------------------------
(*
    fsc.exe -- COMPILED\!INTERACTIVE
    fsi.exe -- !COMPILED\INTERACTIVE
    Language service
        .fs -- COMPILED\!INTERACTIVE
        .fsx -- !COMPILED\INTERACTIVE    
*)
type InteractiveOrCompile =
    // Always define INTERACTIVE
    | DefineInteractive
    // Always define COMPILED
    | DefineCompile
    // Choose based on the extension of the file
    | ChooseByFileExtension

type LoadClosure = 
    { /// The source files along with the ranges of the #load positions in each file.
        SourceFiles: (string * range list) list
        /// The resolved references along with the ranges of the #r positions in each file.
        References: (string * AssemblyResolution list) list
        /// The list of all sources in the closure with inputs when available
        Inputs: (string * Input option) list
        /// The #nowarns
        NoWarns: (string * range list) list
        /// *Parse* errors seen while parsing root of closure
        RootErrors : PhasedError list
        /// *Parse* warnings seen while parsing root of closure
        RootWarnings : PhasedError list }
    static member FindFromSource : filename : string * source : string * editing : bool * interactiveOrCompile:InteractiveOrCompile * lexResourceManager : Lexhelp.LexResourceManager -> LoadClosure
    static member FindFromFiles : tcConfig:TcConfig * (string * range) list * editing : bool * interactiveOrCompile:InteractiveOrCompile * useDefaultScriptingReferences : bool * lexResourceManager : Lexhelp.LexResourceManager -> LoadClosure
