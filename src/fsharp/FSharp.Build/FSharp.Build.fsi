namespace FSBuild
  type internal SR =
    class
      private new : unit -> SR
      static member RunStartupValidation : unit -> unit
      static member SwallowResourceText : bool
      static member SwallowResourceText : bool with set
      static member toolpathUnknown : unit -> string
    end



namespace Internal.Utilities
  module internal FSharpEnvironment = begin
    val FSharpCoreLibRunningVersion : string option
    val FSharpTeamVersionNumber : string
    val FSharpBinaryMetadataFormatRevision : string
    val RegOpenKeyExW :
      System.UIntPtr * string * uint32 * int * byref<System.UIntPtr> -> uint32
    val RegQueryValueExW :
      System.UIntPtr * string * uint32 * byref<uint32> * System.IntPtr *
      byref<int> -> uint32
    val RegCloseKey : System.UIntPtr -> uint32
    module Option = begin
      val ofString : string -> string option
    end
    val maxPath : int
    val maxDataLength : int
    val KEY_WOW64_DEFAULT : int
    val KEY_WOW64_32KEY : int
    val HKEY_LOCAL_MACHINE : System.UIntPtr
    val KEY_QUERY_VALUE : int
    val REG_SZ : uint32
    val GetDefaultRegistryStringValueViaDotNet : string -> string option
    val Get32BitRegistryStringValueViaPInvoke : string -> string option
    val is32Bit : bool
    val tryRegKey : string -> string option
    val tryCurrentDomain : unit -> string option
    val tryAppConfig : string -> string option
    val BinFolderOfDefaultFSharpCompiler : string option
  end

namespace Microsoft.FSharp.Build
  type FscCommandLineBuilder =
    class
      new : unit -> FscCommandLineBuilder
      member
        AppendFileNamesIfNotNull : filenames:Build.Framework.ITaskItem array *
                                   sep:string -> unit
      member AppendSwitch : switch:string -> unit
      member AppendSwitchIfNotNull : switch:string * value:string -> unit
      member
        AppendSwitchIfNotNull : switch:string * values:string array * sep:string ->
                                  unit
      member
        AppendSwitchUnquotedIfNotNull : switch:string * value:string -> unit
      member CapturedArguments : unit -> string list
      member CapturedFilenames : unit -> string list
      override ToString : unit -> string
    end
  type FscCustomBuildEventArgs =
    class
      inherit Build.Framework.CustomBuildEventArgs
      new : commandLine:string -> FscCustomBuildEventArgs
      member CommandLine : string
    end
  type Fsc =
    class
      inherit Build.Utilities.ToolTask
      new : unit -> Fsc
      member
        internal BaseExecuteTool : pathToTool:string *
                                   responseFileCommands:string *
                                   commandLineCommands:string -> int
      override
        ExecuteTool : pathToTool:string * responseFileCommands:string *
                      commandLineCommands:string -> int
      override GenerateCommandLineCommands : unit -> string
      override GenerateFullPathToTool : unit -> string
      member internal InternalExecuteTool : string * string * string -> int
      member internal InternalGenerateCommandLineCommands : unit -> string
      member internal InternalGenerateFullPathToTool : unit -> string
      member BaseAddress : string
      member CodePage : string
      member DebugSymbols : bool
      member DebugType : string
      member DefineConstants : Build.Framework.ITaskItem []
      member DisabledWarnings : string
      member DocumentationFile : string
      member GenerateInterfaceFile : string
      member KeyFile : string
      member LCID : string
      member NoFramework : bool
      member Optimize : bool
      member OtherFlags : string
      member OutputAssembly : string
      member PdbFile : string
      member Platform : string
      member ReferencePath : string
      member References : Build.Framework.ITaskItem []
      member Resources : Build.Framework.ITaskItem []
      member Sources : Build.Framework.ITaskItem []
      override StandardErrorEncoding : System.Text.Encoding
      override StandardOutputEncoding : System.Text.Encoding
      member Tailcalls : bool
      member TargetType : string
      member ToolExe : string
      override ToolName : string
      member ToolPath : string
      member TreatWarningsAsErrors : bool
      member Utf8Output : bool
      member VersionFile : string
      member VisualStudioStyleErrors : bool
      member WarningLevel : string
      member WarningsAsErrors : string
      member Win32ManifestFile : string
      member Win32ResourceFile : string
      member BaseAddress : string with set
      member CodePage : string with set
      member DebugSymbols : bool with set
      member DebugType : string with set
      member DefineConstants : Build.Framework.ITaskItem [] with set
      member DisabledWarnings : string with set
      member DocumentationFile : string with set
      member GenerateInterfaceFile : string with set
      member KeyFile : string with set
      member LCID : string with set
      member NoFramework : bool with set
      member Optimize : bool with set
      member OtherFlags : string with set
      member OutputAssembly : string with set
      member PdbFile : string with set
      member Platform : string with set
      member ReferencePath : string with set
      member References : Build.Framework.ITaskItem [] with set
      member Resources : Build.Framework.ITaskItem [] with set
      member Sources : Build.Framework.ITaskItem [] with set
      member Tailcalls : bool with set
      member TargetType : string with set
      member ToolExe : string with set
      member ToolPath : string with set
      member TreatWarningsAsErrors : bool with set
      member Utf8Output : bool with set
      member VersionFile : string with set
      member VisualStudioStyleErrors : bool with set
      member WarningLevel : string with set
      member WarningsAsErrors : string with set
      member Win32ManifestFile : string with set
      member Win32ResourceFile : string with set
    end
  module Attributes = begin
  end

namespace Microsoft.FSharp.Build
  [<ClassAttribute ()>]
  type CreateFSharpManifestResourceName =
    class
      inherit Build.Tasks.CreateCSharpManifestResourceName
      new : unit -> CreateFSharpManifestResourceName
      override
        CreateManifestName : fileName:string * linkFileName:string *
                             rootNamespace:string * dependentUponFileName:string *
                             binaryStream:System.IO.Stream -> string
      override IsSourceFile : filename:string -> bool
    end

