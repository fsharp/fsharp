module internal Microsoft.FSharp.Compiler.Driver 

open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Build

/// the F# project system calls this to pop up type provider security dialog if needed
val internal runFromCommandLineToImportingAssemblies : (string -> unit) * string[] * string * string * Exiter -> unit

#if NO_COMPILER_BACKEND
#else
/// fsc.exe calls this
val mainCompile : argv : string[] * bannerAlreadyPrinted : bool * exiter : Exiter * createErrorLogger:(TcConfigBuilder -> ErrorLogger) -> unit

#endif