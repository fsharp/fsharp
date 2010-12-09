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

namespace Internal.Utilities.FileSystem

  [<Class>]
  type internal File =
      static member SafeExists : filename:string -> bool
      static member SafeNewFileStream : filename:string * mode:System.IO.FileMode * access:System.IO.FileAccess * share:System.IO.FileShare -> System.IO.FileStream

  [<Class>]
  type internal Path =
      static member SafeGetFullPath : filename:string -> string
