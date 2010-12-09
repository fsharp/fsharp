//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2010 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

// This module related to bugs 4577 and 4651.

// Briefly, there are lots of .Net APIs that take a 'string filename' and if the 
// filename is 'relative', they'll use Directory.GetCurrentDirectory() to 'base' the file.
// Since that involves mutable global state, it's anathema to call any of these methods
// with a non-absolute filename inside the VS process, since you never know what the 
// current working directory may be.

// Thus the idea is to replace all calls to e.g. File.Exists() with calls to File.SafeExists,
// which asserts that we are passing an 'absolute' filename and thus not relying on the 
// (unreliable) current working directory.

// At this point, we have done 'just enough' work to feel confident about the behavior of 
// the product, but in an ideal world (perhaps with 4651) we should ensure that we never
// call unsafe .Net APIs and always call the 'safe' equivalents below, instead.

namespace Internal.Utilities.FileSystem

open System.IO
open System.Diagnostics

type internal File() =
    static member SafeExists filename =
        Debug.Assert(Path.IsPathRooted(filename), sprintf "SafeExists: '%s' is not absolute" filename)
        System.IO.File.Exists(filename)
    static member SafeNewFileStream(filename:string,mode:FileMode,access:FileAccess,share:FileShare) = 
        new FileStream(filename,mode,access,share) 

type internal Path() =
    /// Take in a Windows filename with an absolute path, and return the same filename
    /// but canonicalized with respect to extra path separators (e.g. C:\\\\foo.txt) 
    /// and '..' portions
    static member SafeGetFullPath filename =
        Debug.Assert(Path.IsPathRooted(filename), sprintf "SafeGetFullPath: '%s' is not absolute" filename)
        System.IO.Path.GetFullPath(filename)
