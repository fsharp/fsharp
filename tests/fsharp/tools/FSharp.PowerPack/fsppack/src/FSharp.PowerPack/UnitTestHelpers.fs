// (c) Microsoft Corporation 2005-2009.
namespace UnitTestHelpers

open System
open Microsoft.Build.Framework
open Microsoft.Build.Utilities

// For unit testing (attaching a Logger listener)      
type FsCustomBuildEventArgs(s:string) =
    inherit CustomBuildEventArgs()
    
    member this.CommandLine = s