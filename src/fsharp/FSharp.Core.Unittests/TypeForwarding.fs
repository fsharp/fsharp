// Various tests for Microsoft.FSharp.Core type forwarding

namespace FSharp.Core.Unittests.FSharp_Core.Type_Forwarding

open System
open FSharp.Core.Unittests.LibraryTestFx
open NUnit.Framework

#if FX_ATLEAST_PORTABLE
// TODO named #define ?
#else
#if SILVERLIGHT
#else
[<TestFixture>]
type TypeForwardingModule() =
    [<Test>]
    member this.TypeForwarding() =
        let currentRuntimeVersion = System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion()
        let tupleAssemblyName = typeof<System.Tuple<int,int>>.Assembly.FullName
        let mscorlibAssemblyName = "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
        let fsharpCoreAssemblyName = "FSharp.Core, Version=2.3.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
        
        // 2.0 runtime
        if currentRuntimeVersion = "v2.0.50727" then
            Assert.AreEqual(tupleAssemblyName, fsharpCoreAssemblyName)
        else
            Assert.AreEqual(tupleAssemblyName, mscorlibAssemblyName)        
        () 
#endif
#endif