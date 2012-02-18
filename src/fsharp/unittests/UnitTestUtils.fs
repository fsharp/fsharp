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

namespace Microsoft.FSharp.UnitTest
open System
open System.IO

module FilesystemHelpers =
    /// Create a new temporary directory.
    let rec NewTempDirectory (prefixName : String) = 
        let tick = Environment.TickCount 
        let dir = Path.Combine(Path.GetTempPath(), sprintf "%s-%A" prefixName tick)
        if Directory.Exists dir then NewTempDirectory prefixName
        else 
            let _ = Directory.CreateDirectory(dir)
            dir
       
    /// Create a temporary filename, invoke callback with that filename, then clean up temp file.
    let DoWithTempFile (filename : string) (f : string (*filePath*) -> 'a) = 
        let dir = NewTempDirectory "fsc-tests"
        let filePath = Path.Combine(dir, filename)
        let r = f filePath
        let rec DeleteAll dir =
            for f in Directory.GetFiles(dir) do
                File.Delete(f)
            for d in Directory.GetDirectories(dir) do
                DeleteAll(d)
            try
                Directory.Delete(dir)
            with e ->
                printfn "failed to delete temp directory %s" dir
                printfn "  error was %s" e.Message
                printfn "  ignoring"
        DeleteAll(dir)
        r
