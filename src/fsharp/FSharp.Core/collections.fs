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

namespace Microsoft.FSharp.Collections

    #nowarn "51"

    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Primitives.Basics
    open System
    open System.Diagnostics
    open System.Collections
    open System.Collections.Generic

    module HashIdentity = 
                
        
        let inline Structural<'T when 'T : equality> : IEqualityComparer<'T> = 
            LanguagePrimitives.FastGenericEqualityComparer<'T>
              
        let LimitedStructural<'T when 'T : equality>(limit) : IEqualityComparer<'T> = 
            LanguagePrimitives.FastLimitedGenericEqualityComparer<'T>(limit)
              
        let Reference<'T when 'T : not struct > : IEqualityComparer<'T> = 
            { new IEqualityComparer<'T> with
                  member self.GetHashCode(x) = LanguagePrimitives.PhysicalHash(x) 
                  member self.Equals(x,y) = LanguagePrimitives.PhysicalEquality x y }

        let inline FromFunctions hash eq : IEqualityComparer<'T> = 
            let eq = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(eq)
            { new IEqualityComparer<'T> with 
                member self.GetHashCode(x) = hash x
                member self.Equals(x,y) = eq.Invoke(x,y)  }


    module ComparisonIdentity = 


        let Structural<'T when 'T : comparison > : IComparer<'T> = 
            LanguagePrimitives.FastGenericComparer<'T>
            
        let FromFunction comparer = 
            let comparer = OptimizedClosures.FSharpFunc<'T,'T,int>.Adapt(comparer)
            { new IComparer<'T> with
                  member self.Compare(x,y) = comparer.Invoke(x,y) } 

