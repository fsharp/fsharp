
namespace Microsoft.FSharp
open System.Reflection
[<assembly:AssemblyDescription("FSharp.Core.dll")>]
[<assembly:AssemblyCompany("Microsoft Corporation")>]
[<assembly:AssemblyTitle("FSharp.Core.dll")>]
[<assembly:AssemblyCopyright("\169 Microsoft Corporation.  All rights reserved.")>]
[<assembly:AssemblyProduct("Microsoft\174 F#")>]

#if PORTABLE
[<assembly:AssemblyProduct("Microsoft\174 F#")>]
[<assembly:AssemblyFlags(System.Reflection.AssemblyNameFlags.Retargetable)>] // ensure we replace any 4.0.30319.* or 4.0.31105.* versions in the GAC. These are the FileVersions for RTM VS2010 and SP1 VS2010
#endif

do()

