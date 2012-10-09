namespace EmptyAssembly

open Microsoft.FSharp.Core.CompilerServices

// The point is to test a warning diagnostic about assemblies with the attribute below but with no type providers defined

[<assembly:TypeProviderAssembly>]
do()