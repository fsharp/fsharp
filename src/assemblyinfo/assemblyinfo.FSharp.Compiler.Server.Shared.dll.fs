#light
namespace Microsoft.FSharp
open System.Reflection

#if NO_STRONG_NAMES
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("fsi")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("FSharp.VS.FSI")>]
#else
[<assembly:AssemblyDescription("FSharp.Compiler.Server.Shared.dll")>]
[<assembly:AssemblyCompany("Microsoft Corporation")>]
[<assembly:AssemblyTitle("FSharp.Compiler.Server.Shared.dll")>]
[<assembly:AssemblyCopyright("\169 Microsoft Corporation.  All rights reserved.")>]
[<assembly:AssemblyProduct("Microsoft\174 F#")>]

[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("fsi, PublicKey=002400000480000094000000060200000024000052534131000400000100010079159977d2d03a8e6bea7a2e74e8d1afcc93e8851974952bb480a12c9134474d04062447c37e0e68c080536fcf3c3fbe2ff9c979ce998475e506e8ce82dd5b0f350dc10e93bf2eeecf874b24770c5081dbea7447fddafa277b22de47d6ffea449674a4f9fccf84d15069089380284dbdd35f46cdff12a1bd78e4ef0065d016df")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("FSharp.VS.FSI, PublicKey=002400000480000094000000060200000024000052534131000400000100010079159977d2d03a8e6bea7a2e74e8d1afcc93e8851974952bb480a12c9134474d04062447c37e0e68c080536fcf3c3fbe2ff9c979ce998475e506e8ce82dd5b0f350dc10e93bf2eeecf874b24770c5081dbea7447fddafa277b22de47d6ffea449674a4f9fccf84d15069089380284dbdd35f46cdff12a1bd78e4ef0065d016df")>]
#endif
do()
