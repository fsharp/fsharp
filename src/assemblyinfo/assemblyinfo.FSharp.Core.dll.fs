
namespace Microsoft.FSharp
open System.Reflection
[<assembly:AssemblyDescription("FSharp.Core.dll")>]
[<assembly:AssemblyTitle("FSharp.Core.dll")>]
[<assembly:AssemblyCopyright("\169 Microsoft Corporation and other contributors.  Apache 2.0 License.")>]
[<assembly:AssemblyProduct("F# (open source edition)")>]

#if PORTABLE
[<assembly:AssemblyProduct("Microsoft\174 F#")>]
[<assembly:AssemblyFlags(System.Reflection.AssemblyNameFlags.Retargetable)>] // ensure we replace any 4.0.30319.* or 4.0.31105.* versions in the GAC. These are the FileVersions for RTM VS2010 and SP1 VS2010
#endif

do()

