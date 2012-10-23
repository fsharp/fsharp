namespace Microsoft.FSharp.Data.TypeProviders.DesignTime

open Microsoft.FSharp.Core.CompilerServices

// This says that this assembly is a runtime DLL for a particular platform, where the design-time
// DLL is found alongside this DLL and has the given name.
[<assembly:TypeProviderAssemblyAttribute("FSharp.Data.TypeProviders.DesignTime.dll") >]
do()

