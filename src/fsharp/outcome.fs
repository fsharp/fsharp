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

// --------------------------------------------------------------------	
// Outcomes.  These are used to describe steps of a machine that
// may raise errors.  The errors can be trapped.
// --------------------------------------------------------------------	

module internal Microsoft.FSharp.Compiler.Outcome

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

let success a = Result a
let raze (b:exn) = Exception b

// bind
let (||?>) res f = 
  match res with 
  | Result x -> f x 
  | Exception err -> Exception err

// map
let (|?>) res f = 
  match res with 
  | Result x -> Result(f x )
  | Exception err -> Exception err
  
let ForceRaise = function
  | Result x -> x
  | Exception err -> raise err

let otherwise f x =
  match x with 
  | Result x -> success x
  | Exception _err -> f()

    
