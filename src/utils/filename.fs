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

module internal Internal.Utilities.Filename

open System.IO

// Case sensitive (original behaviour preserved).
let checkSuffix (x:string) (y:string) = x.EndsWith(y,System.StringComparison.Ordinal) 

let hasExtension (s:string) = 
    (s.Length >= 1 && s.[s.Length - 1] = '.' && s <> ".." && s <> ".") 
    || Path.HasExtension(s)

let chopExtension (s:string) =
    if s = "." then "" else 
    if not (hasExtension s) then 
        raise (System.ArgumentException("chopExtension")) 
    Path.Combine (Path.GetDirectoryName s,Path.GetFileNameWithoutExtension(s))


let directoryName (s:string) = 
    if s = "" then "."
    else 
      match Path.GetDirectoryName(s) with 
      | null -> if Path.IsPathRooted(s) then s else "."
      | res -> if res = "" then "." else res

