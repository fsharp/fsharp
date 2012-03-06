//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2011 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

namespace Unittests.General

open NUnit.Framework
open System
open System.Reflection
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices

[<TestFixture>]
type GeneralUnitTests() =

    let publicTypesInAsm(asmfile : string) =
        printfn "Validating assembly '%s'" asmfile
        let codeBase = (new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath |> Path.GetDirectoryName
        let asm = Assembly.LoadFrom(Path.Combine(codeBase, asmfile))

        // For public types that have ComVisible, validate that the constructor is internal
        asm.GetTypes()
        |> Seq.fold(fun n t ->
                        if t.IsPublic then
                            if Array.length (t.GetCustomAttributes(typeof<ComVisibleAttribute>, false)) > 0 then
                                t.GetConstructors()
                                |> Seq.fold(fun m c ->
                                                if c.IsPublic then
                                                    printfn "    Public type (ComVisible, public Constructor),%s" t.FullName
                                                    m + 1
                                                else m
                                            ) n
                            else
                                printfn "    Type: %s" t.FullName
                                n + 1
                        else
                            let CVAs = t.GetCustomAttributes(typeof<ComVisibleAttribute>, false)
                            let CVAs = CVAs |> Array.map (fun o -> o :?> ComVisibleAttribute)
                            for cva in CVAs do
                                if cva.Value then
                                    Assert.Fail(sprintf "Type %s is internal, but also ComVisible(true)" t.FullName)
                            let CIAs = t.GetCustomAttributes(typeof<ClassInterfaceAttribute>, false)
                            let CIAs = CIAs |> Array.map (fun o -> o :?> ClassInterfaceAttribute)
                            for cia in CIAs do
                                if cia.Value <> ClassInterfaceType.None then
                                    Assert.Fail(sprintf "Type %s is internal, but also ClassInterface(<something-other-than-none>)" t.FullName)
                            n
                   ) 0

    [<Test>]
    member public this.``PublicSurfaceArea.DotNetReflection``() =
        let comp = publicTypesInAsm @"fsharp.compiler.dll"
        Assert.AreEqual(0, comp)
        let compis = publicTypesInAsm @"FSharp.Compiler.Interactive.Settings.dll"
        Assert.AreEqual(5, compis)
        let compserver = publicTypesInAsm @"FSharp.Compiler.Server.Shared.dll"
        Assert.AreEqual(0, compserver)
        ()
