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

#nowarn "44" // This construct is deprecated. This function is for use by compiled F# code and should not be used directly
namespace System.Numerics

#if FX_ATLEAST_40
#else
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Primitives.Basics
    open Microsoft.FSharp.Math
    open System
    open System.Globalization


    // INVARIANT: signInt = 1 or -1
    //            value(z) = signInt * v
    // NOTE: 0 has two repns (+1,0) or (-1,0).
    [<Struct>]
    [<CustomEquality; CustomComparison>]
    [<StructuredFormatDisplay("{StructuredDisplayString}I")>]
    type BigInteger(signInt:int, v : BigNat) =

        static let smallLim =  4096
        static let smallPosTab = Array.init smallLim BigNatModule.ofInt32
        static let one = BigInteger(1)
        static let zero = BigInteger(0)

        static member internal nat n = 
            if BigNatModule.isSmall n && BigNatModule.getSmall n < smallLim 
            then smallPosTab.[BigNatModule.getSmall n] 
            else n
        static member internal create (s,n) = BigInteger(s,BigInteger.nat n)
        static member internal posn n = BigInteger(1,BigInteger.nat n)
        static member internal negn n = BigInteger(-1,BigInteger.nat n)


        member x.Sign = if x.IsZero then 0 else signInt
        member x.SignInt = signInt
        member internal x.V = v

        static member op_Equality (x:BigInteger, y:BigInteger) =
            //System.Console.WriteLine("x = {0}",box x)
            //System.Console.WriteLine("y = {0}",box y)
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.equal x.V y.V                    // +1.xv = +1.yv iff xv = yv 
            | -1, -1 -> BigNatModule.equal x.V y.V                     // -1.xv = -1.yv iff xv = yv 
            |  1,-1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V       //  1.xv = -1.yv iff xv=0 and yv=0 
            | -1, 1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V       // -1.xv =  1.yv iff xv=0 and yv=0 
            | _ -> invalidArg "x" "signs should be +/- 1"

        static member op_Inequality (x:BigInteger, y:BigInteger) = not (BigInteger.op_Equality(x,y)) // CA2226: OperatorsShouldHaveSymmetricalOverloads
                
        static member op_LessThan (x:BigInteger, y:BigInteger) =
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.lt x.V y.V                       //  1.xv <  1.yv iff xv < yv 
            | -1,-1 -> BigNatModule.lt y.V x.V                       // -1.xv < -1.yv iff yv < xv 
            |  1,-1 -> false                              //  1.xv < -1.yv iff 0 <= 1.xv < -1.yv <= 0 iff false 
            | -1, 1 -> not (BigNatModule.isZero x.V) || not (BigNatModule.isZero y.V)
                                                          // -1.xv <  1.yv
                                   // (a) xv=0 and yv=0,  then false
                                   // (b) xv<>0,          -1.xv <  0 <= 1.yv, so true
                                   // (c) yv<>0,          -1.xv <= 0 <  1.yv, so true
            | _ -> invalidArg "x" "signs should be +/- 1"
                
        static member op_GreaterThan (x:BigInteger, y:BigInteger) = // Follow lt by +/- symmetry 
            match x.SignInt,y.SignInt with
            | 1, 1 -> BigNatModule.gt x.V y.V 
            | -1,-1 -> BigNatModule.gt y.V x.V
            |  1,-1 -> not (BigNatModule.isZero x.V) || not (BigNatModule.isZero y.V)
            | -1, 1 -> false
            | _ -> invalidArg "x" "signs should be +/- 1"

        static member internal compare(n,nn) = if BigInteger.op_LessThan(n,nn) then -1 elif BigInteger.op_Equality(n,nn) then 0 else 1
        static member internal hash (z:BigInteger) = z.SignInt + BigNatModule.hash(z.V)

        override x.ToString() =
            match x.SignInt with
            |  1 -> BigNatModule.toString x.V                       // positive 
            | -1 -> 
                if BigNatModule.isZero x.V             
                then "0"                    // not negative infact, but zero. 
                else "-" + BigNatModule.toString x.V  // negative 
            | _ -> invalidOp "signs should be +/- 1"
               
        member x.StructuredDisplayString = x.ToString()

        interface System.IComparable with 
            member this.CompareTo(obj:obj) = 
                match obj with 
                | :? BigInteger as that -> BigInteger.compare(this,that)
                | _ -> invalidArg "obj" "the objects are not comparable"

        override this.Equals(obj) = 
            match obj with 
            | :? BigInteger as that -> BigInteger.op_Equality(this, that)
            | _ -> false
  
        override x.GetHashCode() = BigInteger.hash(x)


        new (n:int) = 
            if n>=0 
            then BigInteger (1,BigInteger.nat(BigNatModule.ofInt32   n))
            elif (n = System.Int32.MinValue) 
            then BigInteger(-1,BigInteger.nat(BigNatModule.ofInt64 (-(int64 n))))
            else BigInteger(-1,BigInteger.nat(BigNatModule.ofInt32 (-n)))


        new (n:int64) = 
            if n>=0L 
            then BigInteger(1,BigInteger.nat (BigNatModule.ofInt64   n))
            elif (n = System.Int64.MinValue) 
            then BigInteger(-1,BigInteger.nat (BigNatModule.add (BigNatModule.ofInt64 System.Int64.MaxValue) BigNatModule.one) )
            else BigInteger(-1,BigInteger.nat (BigNatModule.ofInt64 (-n)))

        static member One = one
        static member Zero = zero
        static member (~-) (z:BigInteger)  = BigInteger.create(-1 * z.SignInt,z.V)
        static member Scale(k,z:BigInteger) =
            if k<0
            then BigInteger.create(-z.SignInt, (BigNatModule.scale (-k) z.V))  // k.zsign.zv = -zsign.(-k.zv) 
            else BigInteger.create(z.SignInt, (BigNatModule.scale k z.V))     // k.zsign.zv =  zsign.k.zv 

        // Result: 1.nx - 1.ny  (integer subtraction) 
        static member internal subnn (nx,ny) =                         
            if BigNatModule.gte nx ny 
            then BigInteger.posn (BigNatModule.sub nx ny)          // nx >= ny, result +ve,  +1.(nx - ny) 
            else BigInteger.negn (BigNatModule.sub ny nx)          // nx < ny,  result -ve,  -1.(ny - nx) 

        static member internal addnn (nx,ny) = 
            BigInteger.posn (BigNatModule.add nx ny)              // Compute "nx + ny" to be integer 
            
        member x.IsZero = BigNatModule.isZero x.V                   // signx.xv = 0 iff xv=0, since signx is +1,-1 
        member x.IsOne = (x.SignInt = 1) && BigNatModule.isOne x.V       // signx.xv = 1 iff signx = +1 and xv = 1 
        static member (+) (x:BigInteger,y:BigInteger) =
            if y.IsZero then x else
            if x.IsZero then y else
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigInteger.addnn(x.V,y.V)                //  1.xv +  1.yv =  (xv + yv) 
            | -1,-1 -> -(BigInteger.addnn(x.V,y.V))          // -1.xv + -1.yv = -(xv + yv) 
            |  1,-1 -> BigInteger.subnn (x.V,y.V)                //  1.xv + -1.yv =  (xv - yv) 
            | -1, 1 -> BigInteger.subnn(y.V,x.V)                // -1.xv +  1.yv =  (yv - xv) 
            | _ -> invalidArg "x" "signs should be +/- 1"
                
        static member (-) (x:BigInteger,y:BigInteger) =
            if y.IsZero then x else
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigInteger.subnn(x.V,y.V)                //  1.xv -  1.yv =  (xv - yv) 
            | -1,-1 -> BigInteger.subnn(y.V,x.V)                // -1.xv - -1.yv =  (yv - xv) 
            |  1,-1 -> BigInteger.addnn(x.V,y.V)                //  1.xv - -1.yv =  (xv + yv) 
            | -1, 1 -> -(BigInteger.addnn(x.V,y.V))          // -1.xv -  1.yv = -(xv + yv) 
            | _ -> invalidArg "x" "signs should be +/- 1"
                
        static member ( * ) (x:BigInteger,y:BigInteger) =
            if x.IsZero then x
            elif y.IsZero then y
            elif x.IsOne then y
            elif y.IsOne then x
            else 
                let m = (BigNatModule.mul x.V y.V)
                BigInteger.create (x.SignInt * y.SignInt,m)  // xsign.xv * ysign.yv = (xsign.ysign).(xv.yv) 
                
        static member DivRem (x:BigInteger,y:BigInteger,rem:BigInteger byref) =
            let d,r = BigNatModule.divmod x.V y.V
            // HAVE: |x| = d.|y| + r and 0 <= r < |y| 
            // HAVE: xv  = d.yv  + r and 0 <= r < yv  
            match x.SignInt,y.SignInt with
            |  1, 1 -> rem <- BigInteger.posn r ; BigInteger.posn d     //  1.xv =  1.d.( 1.yv) + ( 1.r) 
            | -1,-1 -> rem <- BigInteger.negn r ; BigInteger.posn d     // -1.xv =  1.d.(-1.yv) + (-1.r) 
            |  1,-1 -> rem <- BigInteger.posn r ; BigInteger.negn d     //  1.xv = -1.d.(-1.yv) + ( 1.r) 
            | -1, 1 -> rem <- BigInteger.negn r ; BigInteger.negn d     // -1.xv = -1.d.( 1.yv) + (-1.r) 
            | _ -> invalidArg "x" "signs should be +/- 1"
                
        static member (/) (x:BigInteger,y:BigInteger) = 
            let mutable rem = new BigInteger(0) 
            BigInteger.DivRem(x,y,&rem)
        static member (%) (x:BigInteger,y:BigInteger) = 
            let mutable rem = new BigInteger(0) 
            BigInteger.DivRem(x,y,&rem) |> ignore ; rem
        static member GreatestCommonDivisor (x:BigInteger,y:BigInteger) = BigInteger.posn (BigNatModule.hcf x.V y.V) // hcf (xsign.xv,ysign.yv) = hcf (xv,yv) 
            
        member x.IsNegative = x.SignInt = -1 && not (x.IsZero)  // signx.xv < 0 iff signx = -1 and xv<>0 
        member x.IsPositive = x.SignInt =  1 && not (x.IsZero)  // signx.xv > 0 iff signx = +1 and xv<>0 
        static member Abs (x:BigInteger)  = if x.SignInt = -1 then -x else x

        static member op_LessThanOrEqual (x:BigInteger,y:BigInteger) =
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.lte x.V y.V                      //  1.xv <=  1.yv iff xv <= yv 
            | -1,-1 -> BigNatModule.lte y.V x.V                      // -1.xv <= -1.yv iff yv <= xv 
            |  1,-1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V       //  1.xv <= -1.yv,
                                                          // (a) if xv=0 and yv=0 then true
                                                          // (b) otherwise false, only meet at zero.
                                                           
            | -1, 1 -> true                               // -1.xv <= 1.yv, true 
            | _ -> invalidArg "x" "signs should be +/- 1"
                
        static member op_GreaterThanOrEqual (x:BigInteger,y:BigInteger) = // Follow lte by +/- symmetry 
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.gte x.V y.V
            | -1,-1 -> BigNatModule.gte y.V x.V
            |  1,-1 -> true
            | -1, 1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V
            | _ -> invalidArg "x" "signs should be +/- 1"
            
                
        static member Pow (x:BigInteger,y:int32) =
            if y < 0 then invalidArg "y" (SR.GetString(SR.inputMustBeNonNegative))
            let yval = BigInteger(y)
            BigInteger.create ((if BigNatModule.isZero (BigNatModule.rem yval.V BigNatModule.two) then 1 else x.SignInt), BigNatModule.pow x.V yval.V)
              
        static member op_Explicit (x:BigInteger) = 
            let u = BigNatModule.toUInt32 x.V
            if u <= uint32 System.Int32.MaxValue then
                // Handle range [-MaxValue,MaxValue] 
                x.SignInt * int32 u     
            elif x.SignInt = -1 &&       u = uint32 (System.Int32.MaxValue + 1) then
                //assert(System.Int32.MinValue = 0 - System.Int32.MaxValue - 1)       
                // Handle MinValue = -(MaxValue+1) special case not covered by the above 
                System.Int32.MinValue
            else
                raise (System.OverflowException())

        static member op_Explicit (x:BigInteger) = 
            let u = BigNatModule.toUInt64 x.V
            if u <= uint64 System.Int64.MaxValue then
                (* Handle range [-MaxValue,MaxValue] *)
                int64 x.SignInt * int64 u
            elif x.SignInt = -1 &&       u = uint64 (System.Int64.MaxValue + 1L) then    
                //assert(System.Int64.MinValue = 0 - System.Int64.MaxValue - 1L)      
                (* Handle MinValue = -(MaxValue+1) special case not covered by the above *)
                System.Int64.MinValue
            else
                raise (System.OverflowException())    

        static member op_Explicit (x:BigInteger) = 
            match x.SignInt with
            |  1 ->    BigNatModule.toFloat x.V                     // float (1.xv)  =   float (xv) 
            | -1 -> - (BigNatModule.toFloat x.V)                    // float (-1.xv) = - float (xv) 
            | _ -> invalidArg "x" "signs should be +/- 1"
             
        static member Parse(text:string) =
            let len = text.Length 
            if len = 0 then raise (new System.FormatException("The value could not be parsed"))
            if text.[0..0] = "-" then
                BigInteger.negn (BigNatModule.ofString text.[1..len-1])
            else
                BigInteger.posn (BigNatModule.ofString text)
              
        member internal x.IsSmall = BigNatModule.isSmall (x.V)
        static member Factorial (x:BigInteger) =
            if x.IsNegative then invalidArg "x" (SR.GetString(SR.inputMustBeNonNegative))
            if x.IsPositive then BigInteger.posn (BigNatModule.factorial x.V)
            else BigInteger.One 

        static member ( ~+ )(n1:BigInteger) = n1
  
        static member FromInt64(x:int64) = new BigInteger(x)
        static member FromInt32(x:int32) = new BigInteger(x)
#endif

namespace Microsoft.FSharp.Core


    type bigint = System.Numerics.BigInteger

    open System
    open System.Diagnostics.CodeAnalysis
    open System.Globalization
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open System.Numerics

#if FX_ATLEAST_40
    // No need for FxCop suppressions on Dev10
#else
    // FxCop suppressions
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Addition(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Division(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_GreaterThan(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_GreaterThanOrEqual(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_LessThan(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_LessThanOrEqual(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Modulus(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Multiply(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Subtraction(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_UnaryNegation(System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_UnaryPlus(System.Numerics.BigInteger)")>]
    do()
#endif

    [<AutoOpen>]
    module NumericLiterals =

        module NumericLiteralI = 

#if FX_ATLEAST_40            
            let numTy = System.Reflection.Assembly.Load("System.Numerics, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089").GetType("System.Numerics.BigInteger")
#else
            let numTy = typeof<BigInteger>
#endif      
            let meth64 = numTy.GetConstructor([|typeof<int64>|])
#if FX_ATLEAST_40            
            let methString = numTy.GetMethod("Parse",[|typeof<string>; typeof<System.Globalization.NumberStyles>; typeof<System.IFormatProvider> |])
#else
            let methString = numTy.GetMethod("Parse",[|typeof<string>|])
#endif            

            let tab64 = new System.Collections.Generic.Dictionary<int64,obj>()
            let tabParse = new System.Collections.Generic.Dictionary<string,obj>()
            
            
            let FromInt64Dynamic (x64:int64) : obj = 
                lock tab64 (fun () -> 
                    let mutable res = Unchecked.defaultof<_> 
                    let ok = tab64.TryGetValue(x64,&res)
                    if ok then res else 
                    res <- meth64.Invoke [| box x64 |] 
                    tab64.[x64] <- res
                    res)                 

            let inline get32 (x32:int32) =  FromInt64Dynamic (int64 x32)

            let inline isOX s = not (System.String.IsNullOrEmpty(s)) && s.Length > 2 && s.[0] = '0' && s.[1] = 'x'
            
            let FromStringDynamic (s:string) : obj = 
                lock tabParse (fun () -> 
                    let mutable res = Unchecked.defaultof<_> 
                    let ok = tabParse.TryGetValue(s,&res)
                    if ok then res else 
                    res <-  
#if FX_ATLEAST_40                    
// Note - no longer used, since we use .NET 4.0 BigInteger instead
                       if isOX s then 
                           methString.Invoke(null,[| box s; box NumberStyles.AllowHexSpecifier; box CultureInfo.InvariantCulture |] )
                       else
                           methString.Invoke(null,[| box s; box NumberStyles.AllowLeadingSign; box CultureInfo.InvariantCulture |] )
#else
                        methString.Invoke(null, [| box s |])
#endif                           
                           
                    tabParse.[s] <- res
                    res)
                    
            let FromZero () : 'T = 
                (get32 0 :?> 'T)
                when 'T : BigInteger = BigInteger.Zero 

            let FromOne () : 'T = 
                (get32 1 :?> 'T)
                when 'T : BigInteger = BigInteger.One

            let FromInt32 (i:int32): 'T = 
                (get32 i :?> 'T)
                when 'T : BigInteger = new BigInteger(i)
            
            let FromInt64 (i:int64): 'T = 
                (FromInt64Dynamic i :?> 'T)
                when 'T : BigInteger = new BigInteger(i)
                
            let getParse s = 
                lock tabParse (fun () -> 
                let mutable res = Unchecked.defaultof<_> 
                let ok = tabParse.TryGetValue(s,&res)
                if ok then 
                    res 
                else 
                    let v = 
#if FX_ATLEAST_40
                       if  isOX s then 
                          BigInteger.Parse (s.[2..],NumberStyles.AllowHexSpecifier,CultureInfo.InvariantCulture)
                       else
                          BigInteger.Parse (s,NumberStyles.AllowLeadingSign,CultureInfo.InvariantCulture)
#else
                       BigInteger.Parse s
#endif
                    res <-  v
                    tabParse.[s] <- res
                    res)
                
            let FromString (s:string) : 'T = 
                (FromStringDynamic s :?> 'T)
                when 'T : BigInteger = getParse s

  

