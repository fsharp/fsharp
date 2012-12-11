#light
namespace Microsoft.FSharp
open System.Reflection

[<assembly:AssemblyDescription("FSharp.Compiler.Silverlight.dll")>]
[<assembly:AssemblyTitle("FSharp.Compiler.Silverlight.dll")>]
[<assembly:AssemblyCopyright("\169 Microsoft Corporation and other contributors.  Apache 2.0 License.")>]
[<assembly:AssemblyProduct("F# (open source edition)")>]

// Note: internals visible to unit test DLLs in Retail (and all) builds.
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("Salsa")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("Unittests")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("SystematicUnitTests")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("Test")>]

do()
