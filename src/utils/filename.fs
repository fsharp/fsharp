//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module internal Internal.Utilities.Filename

open System.IO
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 

exception IllegalFileNameChar of string * char

let illegalPathChars = 
    /// The set of characters which may not be used in a path.
    let illegalPathChars = Path.GetInvalidPathChars ()

    // Sort the array (in-place).
    Array.sortInPlace illegalPathChars

    // Return the sorted array.
    illegalPathChars

let checkPathForIllegalChars (path:string) =
    let len = String.length path
    for i = 0 to len - 1 do
        // Determine if this character is disallowed within a path by
        // attempting to find it in the array of illegal path characters.
        if System.Array.BinarySearch (illegalPathChars, path.[i]) >= 0 then
            // The character is not allowed to be used within a path, raise an exception.
            raise(IllegalFileNameChar(path, path.[i]))

// Case sensitive (original behaviour preserved).
let checkSuffix (x:string) (y:string) = x.EndsWith(y,System.StringComparison.Ordinal) 

let hasExtension (s:string) = 
    checkPathForIllegalChars s
    (s.Length >= 1 && s.[s.Length - 1] = '.' && s <> ".." && s <> ".") 
    || Path.HasExtension(s)

let chopExtension (s:string) =
    if s = "." then "" else // for OCaml compatibility
    checkPathForIllegalChars s
    if not (hasExtension s) then 
        raise (System.ArgumentException("chopExtension")) // message has to be precisely this, for OCaml compatibility, and no argument name can be set
    Path.Combine (Path.GetDirectoryName s,Path.GetFileNameWithoutExtension(s))

let directoryName (s:string) =
    if s = "" then "."
    else
      checkPathForIllegalChars s
      match Path.GetDirectoryName(s) with 
      | null -> if FileSystem.IsPathRootedShim(s) then s else "."
      | res -> if res = "" then "." else res

let fileNameOfPath s = 
    checkPathForIllegalChars s
    Path.GetFileName(s)        

let fileNameWithoutExtension s = 
    checkPathForIllegalChars s
    Path.GetFileNameWithoutExtension(s)        
