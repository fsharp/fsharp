//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module internal Microsoft.FSharp.Compiler.Detuple 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Tast



(* detuple pass: *)  
val DetupleImplFile : CcuThunk -> Env.TcGlobals -> TypedImplFile -> TypedImplFile

module GlobalUsageAnalysis = 
    val GetValsBoundInExpr : Expr -> Zset<Val>

    type accessor 

    /// Results is "expr information".
    /// This could extend to be a full graph view of the expr.
    /// Later could support "safe" change operations, and optimisations could be in terms of those.
    type Results =
       { /// v -> context / APP inst args 
         Uses   : Zmap<Val,(accessor list * TType list * Expr list) list>; 
         /// v -> binding repr 
         Defns   : Zmap<Val,Expr>;                                    
         /// bound in a decision tree? 
         DecisionTreeBindings    : Zset<Val>;                                              
         /// v -> recursive? * v list   -- the others in the mutual binding 
         RecursiveBindings  : Zmap<Val,(bool * FlatVals)>;                        
         /// val not defined under lambdas 
         TopLevelBindings : Zset<Val>;                                            
         /// top of expr toplevel? (true) 
         IterationIsAtTopLevel      : bool;                                                         
       }
    val GetUsageInfoOfImplFile :  Env.TcGlobals -> TypedImplFile -> Results
