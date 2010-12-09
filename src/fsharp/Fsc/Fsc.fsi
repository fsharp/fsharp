namespace FSCstrings
  type internal SR =
    class
      private new : unit -> SR
      static member RunStartupValidation : unit -> unit
      static member docfileNoXmlSuffix : unit -> int * string
      static member fscAssemblyCultureAttributeError : unit -> int * string
      static member
        fscAssemblyNotFoundInDependencySet : a0:System.String -> int * string
      static member fscAssemblyVersionAttributeIgnored : unit -> int * string
      static member
        fscAssumeStaticLinkContainsNoDependencies : a0:System.String ->
                                                      int * string
      static member fscBadAssemblyVersion : a0:System.String -> int * string
      static member fscDelaySignWarning : unit -> int * string
      static member
        fscIgnoringMixedWhenLinking : a0:System.String -> int * string
      static member
        fscKeyFileCouldNotBeOpened : a0:System.String -> int * string
      static member fscKeyFileWarning : unit -> int * string
      static member fscKeyNameWarning : unit -> int * string
      static member fscNoImplementationFiles : unit -> int * string
      static member
        fscProblemWritingBinary : a0:System.String * a1:System.String ->
                                    int * string
      static member
        fscQuotationLiteralsStaticLinking : a0:System.String -> int * string
      static member fscQuotationLiteralsStaticLinking0 : unit -> int * string
      static member fscReferenceOnCommandLine : a0:System.String -> int * string
      static member fscRemotingError : unit -> int * string
      static member fscStaticLinkingNoEXE : unit -> int * string
      static member fscStaticLinkingNoMixedDLL : unit -> int * string
      static member fscTooManyErrors : unit -> string
      static member fscTwoResourceManifests : unit -> int * string
      static member SwallowResourceText : bool
      static member SwallowResourceText : bool with set
    end



namespace Microsoft.FSharp.Compiler
  module internal Driver = begin
    val mutable exiter : ErrorLogger.Exiter
    val ErrorLoggerThatQuitsAfterMaxErrors :
      Build.TcConfigBuilder -> ErrorLogger.ErrorLogger
    val ErrorLoggerInitial : Build.TcConfigBuilder -> ErrorLogger.ErrorLogger
    val BuildInitialDisplayEnvForDocGeneration :
      Env.TcGlobals -> Tastops.DisplayEnv
    module InterfaceFileWriter = begin
      val WriteInterfaceFile :
        Env.TcGlobals * Build.TcConfig * Tast.TypedAssembly -> unit
    end
    module XmlDocWriter = begin
      val getDoc : Ast.XmlDoc -> string
      val hasDoc : Ast.XmlDoc -> bool
      val computeXmlDocSigs : Env.TcGlobals * Tast.CcuThunk -> unit
      val writeXmlDoc : string * Tast.CcuThunk * string -> unit
    end
    val getModuleFileName : unit -> string
    val defaultFSharpBinariesDir : string
    val outpath : string -> string -> string
    val TypeCheck :
      Build.TcConfig * Build.TcImports * Env.TcGlobals * ErrorLogger.ErrorLogger *
      string * Ast.NiceNameGenerator * TypeChecker.TcEnv * Ast.Input list ->
        Build.TcState * TypeChecker.TopAttribs * Tast.TypedAssembly *
        TypeChecker.TcEnv
    val GenerateInterfaceData : Build.TcConfig -> bool
    type private ILResource with
      member Bytes : byte []
    val EncodeInterfaceData :
      Build.TcConfig * Env.TcGlobals * Tastops.Remap * Tast.CcuThunk * string ->
        AbstractIL.IL.ILAttribute list * AbstractIL.IL.ILResource list
    val GenerateOptimizationData : Build.TcConfig -> bool
    val EncodeOptimizationData :
      Env.TcGlobals * Build.TcConfig * string * Tastops.Remap *
      (Tast.CcuThunk * #Opt.LazyModuleInfo) -> AbstractIL.IL.ILResource list
    module BinaryGenerationUtilities = begin
      val b0 : int -> byte
      val b1 : int -> byte
      val b2 : int -> byte
      val b3 : int -> byte
      val i16 : int32 -> byte []
      val i32 : int32 -> byte []
      val Padded : int -> byte [] -> byte []
    end
    module ResFileFormat = begin
      val ResFileNode : int32 * int32 * int32 * int32 * byte [] -> byte []
      val ResFileHeader : unit -> byte []
    end
    module VersionResourceFormat = begin
      val VersionInfoNode : byte [] -> byte []
      val VersionInfoElement :
        int32 * byte [] * byte [] option * byte [] [] * bool -> byte []
      val Version : uint16 * uint16 * uint16 * uint16 -> byte []
      val String : string * string -> byte []
      val StringTable : string * seq<string * string> -> byte []
      val StringFileInfo :
        #seq<string * 'b> -> byte [] when 'b :> seq<string * string>
      val VarFileInfo : #seq<int32 * int32> -> byte []
      val VS_FIXEDFILEINFO :
        AbstractIL.IL.ILVersionInfo * AbstractIL.IL.ILVersionInfo * int32 *
        int32 * int32 * int32 * int32 * int64 -> byte []
      val VS_VERSION_INFO :
        (AbstractIL.IL.ILVersionInfo * AbstractIL.IL.ILVersionInfo * int32 *
         int32 * int32 * int32 * int32 * int64) *
        seq<string * #seq<string * string>> * seq<int32 * int32> -> byte []
      val VS_VERSION_INFO_RESOURCE :
        (AbstractIL.IL.ILVersionInfo * AbstractIL.IL.ILVersionInfo * int32 *
         int32 * int32 * int32 * int32 * int64) *
        seq<string * #seq<string * string>> * seq<int32 * int32> -> byte []
    end
    module ManifestResourceFormat = begin
      val VS_MANIFEST_RESOURCE : byte [] * bool -> byte []
    end
    module AttributeHelpers = begin
      val TryFindStringAttribute :
        Env.TcGlobals -> string -> Tast.Attribs -> string option
      val TryFindIntAttribute :
        Env.TcGlobals -> string -> Tast.Attribs -> int32 option
      val TryFindBoolAttribute :
        Env.TcGlobals -> string -> Tast.Attribs -> bool option
      val TryFindVersionAttribute :
        Env.TcGlobals ->
          string -> Tast.Attribs -> AbstractIL.IL.ILVersionInfo option
    end
    val injectedCompatTypes : Set<string>
    val typesForwardedToMscorlib : Set<string>
    val typesForwardedToSystemNumerics : Set<string>
    val createMscorlibExportList :
      Env.TcGlobals -> AbstractIL.IL.ILExportedTypeOrForwarder list
    val createSystemNumericsExportList :
      Env.TcGlobals -> AbstractIL.IL.ILExportedTypeOrForwarder list
    module MainModuleBuilder = begin
      val CreateMainModule :
        Build.TcConfig * Env.TcGlobals * 'a option * string * string *
        TypeChecker.TopAttribs *
        (AbstractIL.IL.ILAttribute list * #seq<AbstractIL.IL.ILResource>) *
        seq<AbstractIL.IL.ILResource> * Ilxgen.CodegenResults *
        AbstractIL.IL.ILVersionInfo option * string *
        AbstractIL.IL.ILPermissions -> AbstractIL.IL.ILModuleDef
    end
    module StaticLinker = begin
      [<NoEqualityAttribute (); NoComparisonAttribute ()>]
      type Node =
        {name: string;
         data: AbstractIL.IL.ILModuleDef;
         ccu: Tast.CcuThunk option;
         refs: AbstractIL.IL.ILReferences;
         mutable edges: Node list;
         mutable visited: bool;}
      val StaticLinkModules :
        Build.TcConfig ->
          AbstractIL.IL.ILGlobals ->
            AbstractIL.IL.ILModuleDef ->
              (Tast.CcuThunk option * AbstractIL.IL.ILModuleDef) list ->
                AbstractIL.IL.ILModuleDef *
                (AbstractIL.IL.ILScopeRef -> AbstractIL.IL.ILScopeRef)
      val PrintModule : string -> AbstractIL.IL.ILModuleDef -> unit
      val StaticLink :
        Build.TcConfig * Build.TcImports * AbstractIL.IL.ILGlobals ->
          (AbstractIL.IL.ILModuleDef * string -> AbstractIL.IL.ILModuleDef)
    end
    type SigningInfo = | SigningInfo of bool * string option * string option
    module FileWriter = begin
      val EmitIL :
        Build.TcConfig * AbstractIL.IL.ILGlobals * ErrorLogger.ErrorLogger *
        string * string option * AbstractIL.IL.ILModuleDef * SigningInfo -> unit
      val WriteStatsFile : Build.TcConfig * string -> unit
    end
    val abortOnError : ErrorLogger.ErrorLogger -> unit
    val ValidateKeySigningAttributes :
      Build.TcConfig -> Env.TcGlobals -> TypeChecker.TopAttribs -> SigningInfo
    type DelayAndForwardErrorLogger =
      class
        interface ErrorLogger.ErrorLogger
        new : unit -> DelayAndForwardErrorLogger
        member
          ForwardDelayedErrorsAndWarnings : errorLogger:ErrorLogger.ErrorLogger ->
                                              unit
        member
          ForwardDelayedErrorsAndWarnings : tcConfigB:Build.TcConfigBuilder ->
                                              unit
        member ErrorCount : int
      end
    val AdjustForScriptCompile :
      Build.TcConfigBuilder * string list * Lexhelp.LexResourceManager ->
        string list
    [<NoEqualityAttribute (); NoComparisonAttribute ()>]
    type Args<'a> = | Args of 'a
    val main1 :
      string [] * bool ->
        Args<Build.TcConfig * Build.TcImports * Build.TcImports * Env.TcGlobals *
             ErrorLogger.ErrorLogger * Tast.CcuThunk * string *
             Tast.TypedAssembly * TypeChecker.TopAttribs * string option *
             string * AbstractIL.IL.ILVersionInfo option * SigningInfo>
    val main2 :
      Args<Build.TcConfig * Build.TcImports * Build.TcImports * Env.TcGlobals *
           'a * Tast.CcuThunk * string * Tast.TypedAssembly * 'b * 'c * 'd * 'e *
           'f> ->
        Args<Build.TcConfig * Build.TcImports * Env.TcGlobals * 'a *
             Tast.CcuThunk * string * Tast.TypedAssembly * 'b * Import.ImportMap *
             'c * 'd *
             (AbstractIL.IL.ILAttribute list * AbstractIL.IL.ILResource list) *
             AbstractIL.IL.ILResource list * 'e * 'f * string>
        when 'a :> ErrorLogger.ErrorLogger
    val main2b :
      Args<Build.TcConfig * Build.TcImports * Env.TcGlobals * 'a * Tast.CcuThunk *
           string * Tast.TypedAssembly * TypeChecker.TopAttribs *
           Import.ImportMap * 'b option * string *
           (AbstractIL.IL.ILAttribute list * #seq<AbstractIL.IL.ILResource>) *
           #seq<AbstractIL.IL.ILResource> * AbstractIL.IL.ILVersionInfo option *
           'e * string> ->
        Args<Build.TcConfig * 'a *
             (AbstractIL.IL.ILModuleDef * string -> AbstractIL.IL.ILModuleDef) *
             AbstractIL.IL.ILGlobals * string * 'b option *
             AbstractIL.IL.ILModuleDef * 'e> when 'a :> ErrorLogger.ErrorLogger
    val main2c :
      Args<Build.TcConfig * 'a * 'b * AbstractIL.IL.ILGlobals * 'c * 'd *
           AbstractIL.IL.ILModuleDef * 'e> ->
        Args<Build.TcConfig * 'a * 'b * AbstractIL.IL.ILGlobals *
             AbstractIL.IL.ILModuleDef * 'c * 'd * 'e>
        when 'a :> ErrorLogger.ErrorLogger
    val main3 :
      Args<'a * ErrorLogger.ErrorLogger * ('b * 'c -> 'd) * 'e * 'b * 'c * 'f *
           'g> -> Args<'a * ErrorLogger.ErrorLogger * 'e * 'd * 'c * 'f * 'g>
    val main4 :
      Args<Build.TcConfig * #ErrorLogger.ErrorLogger * AbstractIL.IL.ILGlobals *
           AbstractIL.IL.ILModuleDef * string * string option * SigningInfo> ->
        unit
    val mainCompile : string [] * bool -> unit
    module FSharpResidentCompiler = begin
      type private OutputCollector =
        class
          new : unit -> OutputCollector
          member
            GetTextAndClear : unit ->
                                (bool * System.ConsoleColor option * char) []
        end
      type FSharpCompilationServer =
        class
          inherit System.MarshalByRefObject
          new : unit -> FSharpCompilationServer
          abstract member
            Compile : string * string [] ->
                        (bool * System.ConsoleColor option * char) [] * int
          abstract member Ping : unit -> string
          override
            Compile : pwd:string * argv:string [] ->
                        (bool * System.ConsoleColor option * char) [] * int
          override Finalize : unit -> unit
          override InitializeLifetimeService : unit -> obj
          override Ping : unit -> string
          member Run : unit -> unit
          static member
            private ConnectToServer : unit -> FSharpCompilationServer
          static member RunServer : unit -> unit
          static member TryCompileUsingServer : argv:string [] -> int option
        end
    end
    val main : string [] -> int
  end

namespace Microsoft.FSharp.Compiler
  module internal CommandLineMain = begin
    val main : string [] -> int
  end

