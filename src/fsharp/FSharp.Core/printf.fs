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

namespace Microsoft.FSharp.Core

open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.StructuredPrintfImpl
open System.Globalization
open System.IO
open System.Text

type PrintfFormat<'printer,'state,'residue,'result>(value:string) =
        member x.Value = value
    
type PrintfFormat<'printer,'state,'residue,'result,'tuple>(value:string) = 
    inherit PrintfFormat<'printer,'state,'residue,'result>(value)

type Format<'printer,'state,'residue,'result> = PrintfFormat<'printer,'state,'residue,'result>
type Format<'printer,'state,'residue,'result,'tuple> = PrintfFormat<'printer,'state,'residue,'result,'tuple>

module PrintfImpl = 

    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Reflection
    open System.Reflection

    type buf = System.Text.StringBuilder

    let stringOfChar (c:char) = System.Char.ToString(c)
    let stringOfInt (i:int) = i.ToString()

    [<NoEquality; NoComparison>]
    type PrintfInfo = 
       { mutable leftJustify    : bool; 
         mutable numPrefixIfPos : char option;
         mutable addZeros       : bool; }

    let outputSignAndLeftSpace(outputChar,info,pos,width,numDigits) = 
        let used = 
            if pos then 
                match info.numPrefixIfPos with 
                | None -> 0
                | Some _ -> 1 
            else 1 
        let len = numDigits + used 
        if not info.leftJustify && not info.addZeros then 
            match width with 
            | None -> () 
            | Some w -> 
                for i = 1 to (w - len) do 
                   outputChar ' '; 
        begin 
          if pos then 
              match info.numPrefixIfPos with 
              | None -> ()
              | Some c -> outputChar c
          else outputChar '-';
        end;
        if not info.leftJustify && info.addZeros then 
            match width with 
            | None -> () 
            | Some w -> 
                for i = 1 to (w - len) do 
                  outputChar (if info.addZeros then '0' else ' '); 
        used

    let decode (c:char) = System.Convert.ToInt32(c)
    let encode (x:int) = System.Convert.ToChar(x)

    let outputDigit(outputChar,intFormatChar,digit) = 
        let digitc = 
            if digit < 10 
            then  decode '0' + digit 
            else decode (if intFormatChar = 'x' then 'a' else 'A') + (digit - 10) 
        outputChar (encode digitc)

    let outputSpace(outputChar,width,len) = 
        match width with 
        | None -> () 
        | Some width -> 
             for i = 1 to (width - len) do 
                 outputChar ' '; 

    let outputZeros(outputChar,width,len) = 
        match width with 
        | None -> () 
        | Some width -> 
             for i = 1 to (width - len) do 
                 outputChar '0'

    let outputRightSpace(outputChar,leftJustify,width,len) =
        if leftJustify then outputSpace(outputChar,width,len)

    let outputUInt64(outputChar,intFormatChar,width,info,(n:uint64)) = 
        let nbase = match intFormatChar with 'o' -> 8uL | 'x' | 'X' -> 16uL | _ -> 10uL 
        let numDigits = 
            let mutable numDigits = 1 
            let mutable nval = n / nbase 
            while nval > 0UL do 
                numDigits <- numDigits + 1; 
                nval <- nval / nbase; 
            numDigits 
        let topdiv = 
            let mutable topdiv = 1UL 
            for i = 1 to numDigits - 1 do 
                topdiv <- topdiv * nbase; 
            topdiv 
      
        let len = numDigits + (outputSignAndLeftSpace(outputChar,info,true,width,numDigits))
        
        let mutable residue = n 
        let mutable divisor = topdiv 
        while divisor > 0UL do 
            let digit = residue / divisor 
            outputDigit(outputChar,intFormatChar, int32(int64 digit));
            residue <- residue % divisor;
            divisor <- divisor / nbase;
        outputRightSpace(outputChar,info.leftJustify,width,len)

    let outputInt64(outputChar,intFormatChar,width,info,(n:int64)) =     
        let nbase = match intFormatChar with 'o' -> 8L | 'x' | 'X' -> 16L | _ -> 10L 
        let numDigits = 
            let mutable numDigits = 1 
            let mutable nval = if n >= 0L then n / nbase else - (n / nbase) 
            while nval > 0L do 
                numDigits <- numDigits + 1; 
                nval <- nval / nbase; 
            numDigits 
        let topdiv = 
            let mutable topdiv = 1L 
            for i = 1 to numDigits - 1 do 
                topdiv <- topdiv * nbase; 
            topdiv 
        
        let len = numDigits + (outputSignAndLeftSpace(outputChar,info,(n >= 0L),width,numDigits) )
        
        let mutable residue = 
            if   n = System.Int64.MinValue then System.Int64.MaxValue
            elif n < 0L            then - n 
            else n 
        let mutable divisor = topdiv 
        while divisor > 0L do 
            let digit = 
                if n = System.Int64.MinValue && divisor = 1L 
                then (match intFormatChar with 'd' | 'i' -> 8L | _ -> 100L) // nb. special case for min_int 
                else residue / divisor 
            outputDigit(outputChar,intFormatChar,int32 digit);
            residue <- residue % divisor;
            divisor <- divisor / nbase;
        outputRightSpace(outputChar,info.leftJustify,width,len)

    // The general technique used this file is to interpret
    // a format string and use reflection to construct a function value that matches
    // the specification of the format string.  
    //
    // Generics add some serious complications here - we have to generate
    // a function value of exactly the right runtime type, though the most
    // natural scheme is to produce one of type 'obj -> obj'.  We get around
    // this by using a semi-reflective approach to creating and invoking
    // function values of the right type.  This comes with some
    // overheads (though they are not too bad) and thus could and should be 
    // optimized in some special cases, e.g. where a format string
    // just contains a single simple format specifier such as '%x'
#if FX_ATLEAST_PORTABLE
    let staticInvokeFlags = BindingFlags.Public ||| BindingFlags.Static
#else
    let staticInvokeFlags = BindingFlags.Public ||| BindingFlags.InvokeMethod ||| BindingFlags.Static
#endif    
    let mkFunctionValue (tys: System.Type[]) (impl:obj->obj) = 
        FSharpValue.MakeFunction(FSharpType.MakeFunctionType(tys.[0],tys.[1]), impl)

    let funTyC = typeof<(obj -> obj)>.GetGenericTypeDefinition()  
    let mkFunTy a b = funTyC.MakeGenericType([| a;b |])

    let isNamedType(ty:System.Type) = not (ty.IsArray ||  ty.IsByRef ||  ty.IsPointer)
    let isFunctionType (ty1:System.Type)  = 
        isNamedType(ty1) && ty1.IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(funTyC)

    let rec destFunTy (ty:System.Type) =
        if isFunctionType ty then 
            ty, ty.GetGenericArguments() 
        else
            match ty.BaseType with 
            | null -> raise <| System.InvalidOperationException(SR.GetString(SR.printfNotAFunType))
            | b -> destFunTy b 
#if FX_ATLEAST_PORTABLE
    let instanceInvokeFlags = BindingFlags.Public ||| BindingFlags.Instance
#else     
    let instanceInvokeFlags = BindingFlags.Public ||| BindingFlags.InvokeMethod ||| BindingFlags.Instance
#endif    
    let invokeFunctionValue (f:obj) (x:obj) = 
        let fTy,_ = destFunTy (f.GetType())
#if FX_ATLEAST_PORTABLE
        let meth = fTy.GetMethod("Invoke",instanceInvokeFlags)
        meth.Invoke(f,[| x |])
#else        
#if FX_NO_CULTURE_INFO_ARGS
        fTy.InvokeMember("Invoke",instanceInvokeFlags,(null:Binder),f,[| x |]) 
#else
        fTy.InvokeMember("Invoke",instanceInvokeFlags,(null:Binder),f,[| x |],CultureInfo.InvariantCulture(*FxCop:1304*)) 
#endif
#endif

    let buildFunctionForOneArgPat (ty: System.Type) impl = 
        let _,tys = destFunTy ty 
        let rty = tys.[1]
        // PERF: this technique is a bit slow (e.g. in simple cases, like 'sprintf "%x"') 
        mkFunctionValue tys (fun inp -> impl rty inp)

    let buildFunctionForTwoArgPat args ty i go = 
        let _,tys1 = destFunTy ty 
        let rty1 = tys1.[1] 
        let _,tys2 =  destFunTy rty1 
        let rty2 = tys2.[1] 
        mkFunctionValue tys1 (fun inpf -> 
          mkFunctionValue tys2 (fun inpx -> 
            go (inpx::inpf::args) rty2 (i+1)))

    let buildFunctionForOneFunArgPat args ty i go = 
        let _,tys1 = destFunTy ty 
        let rty1 = tys1.[1] 
        mkFunctionValue tys1 (fun inpf -> go (inpf::args) rty1 (i+1))

    let isDigit c = ('0' <= c && c <= '9')
    let rec parseFlags info (fmt:string) i = 
        if i >= fmt.Length then raise <| System.ArgumentException (SR.GetString(SR.printfMissingFormatSpecifier));
        match fmt.[i] with
        | '-' -> info.leftJustify <- true; parseFlags info fmt (i+1)
        | '+' -> info.numPrefixIfPos <- Some '+'; parseFlags info fmt (i+1)
        | '0' -> info.addZeros <- true; parseFlags info fmt (i+1)
        | ' ' -> info.numPrefixIfPos <- Some ' '; parseFlags info fmt (i+1)
        | '#' -> raise <| System.ArgumentException (SR.GetString(SR.printfHashFormatSpecifierIllegal)); 
        | _ -> i 

    let rec parseDigitsPrecision (fmt:string) len i = 
        if i >= len then raise <| System.ArgumentException (SR.GetString(SR.printfPrecisonSpecifierIllegal));
        match fmt.[i] with
        | c when isDigit c -> parseDigitsPrecision fmt len (i+1)
        | _ -> i 

    let parsePrecision (fmt:string) len i = 
        if i >= len then raise <| System.ArgumentException (SR.GetString(SR.printfPrecisonSpecifierIllegal));
        match fmt.[i] with
        | c when isDigit c -> false,parseDigitsPrecision fmt len (i+1)
        | '*' -> true,(i+1)
        | _ -> false,i 

    let rec parseSliceDotAndPrecision (fmt:string) len i = 
        match fmt.[i] with
        | '.' -> 
            let w1 = i 
            let precisionArg,i = parsePrecision fmt len (i+1) 
            w1,Some (precisionArg,i),i
        | _ -> i,None,i 

    let rec parseSliceWidthAndPrecision (fmt:string) len i = 
        if i >= len then raise <| System.ArgumentException (SR.GetString(SR.printfWidthSpecifierIllegal));
        match fmt.[i] with
        | c when isDigit c -> parseSliceWidthAndPrecision fmt len (i+1)
        | '*' -> true,parseSliceDotAndPrecision fmt len (i+1)
        | _ -> false,parseSliceDotAndPrecision fmt len i

    let invariantCulture = System.Globalization.CultureInfo.InvariantCulture 
    let parseWidthAndPrecision fmt len i = 
        let w0 = i 
        let widthArg,(w1,w2,i) = parseSliceWidthAndPrecision fmt len i 
        let width = 
            if (w0 = w1) then None 
            elif widthArg then Some(None)
            else Some (Some(System.Int32.Parse (fmt.[w0..w1-1],invariantCulture)) )
        let precision = 
            match w2 with 
            | None -> None 
            | Some (precisionArg,w2') -> 
                if precisionArg then Some(None)
                else Some (Some(System.Int32.Parse (fmt.[w1+1..w2'-1],invariantCulture)) )
        width,precision,i 
      
    let newInfo ()= 
        { leftJustify      = false;
          numPrefixIfPos = None;
          addZeros         = false; }

    let defaultInfo = newInfo() 

    let formatString outputChar info width (s:string) isNum = 
        match s with 
        | null -> outputSpace(outputChar,width,0)
        | _ -> 
            if not info.leftJustify then
                if isNum && info.addZeros then outputZeros(outputChar,width,s.Length)
                else outputSpace(outputChar,width,s.Length);
            s |> String.iter outputChar; 
            if info.leftJustify then
                if isNum && info.addZeros then outputZeros(outputChar,width,s.Length)
                else outputSpace(outputChar,width,s.Length)
                
    let capture1 (fmt:string) i args ty (go : obj list -> System.Type -> int -> obj) : obj = 
        let info = newInfo() 
        let len = fmt.Length 
        let i = parseFlags info fmt i 
        let width,precision,i = parseWidthAndPrecision fmt len i 
        let intFormatChar = fmt.[i] 

        let captureCoreArgs args ty =
            match intFormatChar with
            | '%' -> go args ty (i+1) 
            | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
            | 'l' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+2))
            | 'n' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+2))
            | 'L' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+2))
                  
            | 'U' -> 
                let i = i+1 
                let intFormatChar = fmt.[i]
                match intFormatChar with
                 | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
                 | 'l' ->  buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+2))
                 | 'n' ->  buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+2))
                 | 'L' ->  buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+2))
                 | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "U"))
                  
            | 'f' | 'F' | 'e' | 'E' | 'g' | 'G' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
            | 'M' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
            | 's' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
            | 'c' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
            | 'b' -> buildFunctionForOneArgPat ty (fun rty n -> go (n::args) rty (i+1))
            | 'O' -> buildFunctionForOneArgPat ty (fun rty xobj -> go (xobj::args) rty (i+1))
            | 'A' -> buildFunctionForOneArgPat ty (fun rty xobj -> go (xobj::args) rty (i+1))
            | 'a' -> buildFunctionForTwoArgPat args ty i go
            | 't' -> buildFunctionForOneFunArgPat args ty i go
            | _ -> raise <| System.ArgumentException(SR.GetString1(SR.printfBadFormatSpecifier,intFormatChar.ToString()))
        let capturePrecisionArg args ty = 
            match precision with 
            | None | Some(Some _) -> captureCoreArgs args ty
            | Some(None) -> buildFunctionForOneArgPat ty (fun rty n -> captureCoreArgs (n :: args) rty)
        let captureWidthArg args ty = 
            match width with 
            | None | Some(Some _) -> capturePrecisionArg args ty
            | Some(None) -> buildFunctionForOneArgPat ty (fun rty n -> capturePrecisionArg (n :: args) rty)
        captureWidthArg args ty
        

    let unboxAsInt64 (n:obj) = 
        match n with 
        | :? sbyte      as x -> x |> int64
        | :? int16      as x -> x |> int64
        | :? int32      as x -> x |> int64
        | :? nativeint  as x -> x |> int64
        | :? int64      as x -> x
        | :? byte       as x -> x |> uint64 |> int64
        | :? uint16     as x -> x |> uint64 |> int64
        | :? uint32     as x -> x |> uint64 |> int64
        | :? uint64     as x -> x |> int64 
        | :? unativeint as x -> x |> uint64 |> int64
        | _ -> raise <| System.ArgumentException (SR.GetString(SR.printfBadIntegerForDynamicFomatter))
        
    let unboxAsUInt64 (n:obj) = 
        let unsigned = 
            match n with 
            | :? sbyte     as x -> x |> byte   |> box
            | :? int16     as x -> x |> uint16 |> box
            | :? int32     as x -> x |> uint32 |> box
            | :? int64     as x -> x |> uint64 |> box
            | :? nativeint as x -> x |> unativeint |> box
            | _ -> n
        unboxAsInt64 unsigned |> uint64
        
    let formatOne (outa: 'c -> unit) (outputChar: char -> unit) (os : 'b) (fmt:string) i args : (int * obj list) = 
        let info = newInfo() 
        let len = fmt.Length 
        let i = parseFlags info fmt i 
        let width,precision,i = parseWidthAndPrecision fmt len i 
        let intFormatChar = fmt.[i]

        let width,args = 
            match width,args with
            | None,args -> None,args
            | Some(Some w),args -> Some w,args
            | Some(None),n::args -> Some (unbox n), args
            | _ -> raise <| System.ArgumentException (SR.GetString(SR.printfExpectedWidth)) 

        let precision,args = 
            match precision,args with
            | None,args -> None,args
            | Some(Some w),args -> Some w,args
            | Some(None),n::args -> Some (unbox n), args
            | _ -> raise <| System.ArgumentException (SR.GetString(SR.printfExpectedPrecision)) 

        match intFormatChar,args with
        | '%',args -> 
            outputChar intFormatChar; i+1, args
        | ('d' | 'i'),n::args -> 
            match n with 
            | (:? byte | :? uint16 | :? uint32 | :? uint64 | :? unativeint)  -> 
                outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
            | _ -> 
                outputInt64(outputChar,intFormatChar,width,info,(unboxAsInt64 n)); 
            i+1,args
        | ('o' | 'u' | 'x' | 'X'),n::args -> 
            outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
            i+1,args
        | ('l' | 'L'),n::args ->
            let i = i+1 
            let intFormatChar = fmt.[i]
            match intFormatChar with
            | 'd' | 'i' ->
                outputInt64(outputChar,intFormatChar,width,info,(unboxAsInt64 n)); 
                i+1,args
            | 'o' | 'u' | 'x' | 'X' -> 
                outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
                i+1,args
            | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "n"))
        | 'n',n::args ->
            let i = i+1 
            let intFormatChar = fmt.[i]
            match intFormatChar with
            | 'd' | 'i' ->
                outputInt64(outputChar,intFormatChar,width,info,(unboxAsInt64 n)); 
                i+1,args
            | 'o' | 'u' | 'x' | 'X' -> 
                outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
                i+1,args
            | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "l"))
        | 'U',n::args -> 
            let i = i+1 
            let intFormatChar = fmt.[i]
            match intFormatChar with
            | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' -> 
                outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
                i+1,args
            | 'l' ->
                let i = i+1 
                let intFormatChar = fmt.[i]
                match intFormatChar with
                | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' -> 
                    outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
                    i+1,args
                | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "Ul"))
            | 'n' ->
                let i = i+1 
                let intFormatChar = fmt.[i]
                match intFormatChar with
                | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' -> 
                    outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
                    i+1,args
                | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "Un"))
            | 'L' ->
                let i = i+1 
                let intFormatChar = fmt.[i]
                match intFormatChar with
                | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' -> 
                    outputUInt64(outputChar,intFormatChar,width,info,(unboxAsUInt64 n)); 
                    i+1,args
                | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "UL"))
            | _ -> raise <| System.ArgumentException (SR.GetString1(SR.printfSpecifierAfterIllegal, "U"))
              
        | ('f' | 'F' | 'e' | 'E' | 'g' | 'G'),n::args -> 
            let s, number, positive = 
                match n with 
                | :? float as f -> f.ToString(stringOfChar intFormatChar + (match precision with None -> "6" | Some n -> stringOfInt (max (min n 99) 0)),invariantCulture), not (f = infinity || f = -infinity || System.Double.IsNaN f), f >= 0.
                | :? float32 as f -> f.ToString(stringOfChar intFormatChar + (match precision with None -> "6" | Some n -> stringOfInt (max (min n 99) 0)),invariantCulture), not (f = infinityf || f = -infinityf || System.Single.IsNaN f), f >= 0.f
                | :? decimal as f -> f.ToString(stringOfChar intFormatChar + (match precision with None -> "6" | Some n -> stringOfInt (max (min n 99) 0)),invariantCulture), true, f >= 0M
                | _ -> raise <| System.ArgumentException (SR.GetString(SR.printfBadFloatValue))
                  
            let s = match info.numPrefixIfPos with Some c when positive -> stringOfChar c + s  | _ -> s
            formatString outputChar info width s number;
            i+1,args
        | 'M',n::args -> 
            let d = (unbox n : System.Decimal) 
            let s = d.ToString("G",invariantCulture) 
            let s = match info.numPrefixIfPos with Some c when d >= 0M -> stringOfChar c + s | _ -> s
            formatString outputChar info width s true;
            i+1,args
        | 's',nobj::args -> formatString outputChar info width (unbox nobj) false; i+1,args
        | 'c',nobj::args -> formatString outputChar info width (stringOfChar (unbox nobj)) false; i+1,args
        | 'b',nobj::args -> formatString outputChar info width (if (unbox nobj) then "true" else "false") false; i+1,args
        | 'O',xobj::args -> formatString outputChar info width (match xobj with null -> "<null>" | _ -> xobj.ToString()) false; i+1,args
        | 'A',xobj::args -> 
            let bindingFlags = 
                match info.numPrefixIfPos with 
                | None     -> BindingFlags.Public                            // Default see Public only
                | Some '+' -> BindingFlags.Public ||| BindingFlags.NonPublic // %+A, sees anything possible
                | Some c   -> failwith ("internal: %A has an unexpected numeric prefix '" + string c + "'") 
            let opts = FormatOptions.Default  
            let opts = match width with None -> opts | Some w -> { opts with PrintWidth = w }
            // printfn %0A is considered to mean 'print width zero'
            let opts = if info.addZeros then { opts with PrintWidth = 0 } else opts
            
            let opts = match precision with None -> opts | Some w -> { opts with PrintSize = w }
            let txt = 
                match xobj with 
                | null -> "<null>" 
                | _ -> 
                    Display.anyToStringForPrintf opts bindingFlags xobj
                    
            txt |> String.iter outputChar; 
            i+1,args
        | 'a',fobj::xobj::args -> 
            outa (unbox (invokeFunctionValue (invokeFunctionValue fobj (box os)) xobj)); 
            i+1,args
        | 't',f::args -> outa ((unbox f) os); i+1,args
        | _ -> raise <| System.ArgumentException (SR.GetString(SR.printfBadFormatSpecifier))


    let gprintf (initialize : unit -> 'b * ('c -> unit) * (char -> unit) * (unit -> 'd)) (fmt : PrintfFormat<'a,'b,'c,'d>) : 'a = 
        let fmt = fmt.Value
        match fmt with 
        // optimize some common cases 
        | "%s" -> unbox (box (fun (s:string) -> let _,_,outputChar,finalize = initialize() in formatString outputChar defaultInfo None s false; finalize()))
        // | "%x" -> unbox (box (fun (n:int)    -> let os,outa,outputChar,finalize = initialize() in outputUInt64 outputChar 'x' None defaultInfo (int32_to_uint64 n); finalize()))
        // | "%d" -> unbox (box (fun (n:int)    -> let os,outa,outputChar,finalize = initialize() in outputInt64  outputChar 'd' None defaultInfo (int32_to_int64 n); finalize()))
        | _ -> 
            let len = fmt.Length 

            /// After all arguments are captures we reinterpret and execute the actions
            let run args = 
                let os,outa,outputChar,finalize = initialize()
                let rec go args i = 
                    if i >= len ||  (fmt.[i] = '%' && i+1 >= len) then (box (finalize())) 
                    elif System.Char.IsSurrogatePair(fmt,i) then 
                       outputChar fmt.[i];
                       outputChar fmt.[i+1];
                       go args (i+2)
                    else
                    
                        match fmt.[i] with
                        | '%' ->
                            let i,args = formatOne outa outputChar os fmt (i+1) args 
                            go args i 
                        | c ->
                            outputChar c; go args (i+1) 
                go args 0

            /// Function to capture the arguments and then run.
            let rec capture args ty i = 
                if i >= len ||  (fmt.[i] = '%' && i+1 >= len) then 
                    run (List.rev args) 
                elif System.Char.IsSurrogatePair(fmt,i) then 
                   capture args ty (i+2)
                else
                    match fmt.[i] with
                    | '%' ->
                        let i = i+1 
                        capture1 fmt i args ty capture
                    | _ ->
                        capture args ty (i+1) 

            (unbox (capture [] (typeof<'a>) 0) : 'a)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printf = 

    open System.Text
    open System.Diagnostics
    open PrintfImpl

    type BuilderFormat<'T,'Result>    = Format<'T, StringBuilder, unit, 'Result>
    type StringFormat<'T,'Result>     = Format<'T, unit, string, 'Result>
    type TextWriterFormat<'T,'Result> = Format<'T, TextWriter, unit, 'Result>
    type BuilderFormat<'T>     = BuilderFormat<'T,unit>
    type StringFormat<'T>      = StringFormat<'T,string>
    type TextWriterFormat<'T>  = TextWriterFormat<'T,unit>

#if EXTRAS_FOR_SILVERLIGHT_COMPILER
    let outWriter = ref System.Console.Out
    let errorWriter = ref System.Console.Error

    let setWriter (out : System.IO.TextWriter) = outWriter := out
    let setError  (error : System.IO.TextWriter) = errorWriter := error
#endif
    
    [<CompiledName("PrintFormatToStringThen")>]
    let ksprintf (f : string -> 'd) (fp : StringFormat<'a,'d>)  = 
        let init () = 
            let buf = new StringBuilder() 
            let outputChar (c:char) = ignore (buf.Append(c)) 
            let outa (s:string) = ignore (buf.Append(s)) 
            let finish () = f (buf.ToString())
            (),outa,outputChar,finish 
        PrintfImpl.gprintf init fp

    [<CompiledName("PrintFormatThen")>]
    let kprintf f fmt = ksprintf f fmt

    let kprintf_imperative f handle outputChar fmt =
        let init () = 
            let outa () = () 
            handle,outa,outputChar,f
        PrintfImpl.gprintf init fmt

    [<CompiledName("PrintFormatToStringBuilderThen")>]
    let kbprintf f (buf: StringBuilder) fmt = kprintf_imperative f buf (fun c -> ignore (buf.Append(c))) fmt

    [<CompiledName("PrintFormatToTextWriterThen")>]
    let kfprintf f os fmt = kprintf_imperative f  (os :> TextWriter) (fun c -> ignore (os.Write(c))) fmt

    [<CompiledName("PrintFormatToStringThen")>]
    let sprintf fmt  = ksprintf (fun x -> x) fmt

    [<CompiledName("PrintFormatToStringThenFail")>]
    let failwithf fmt  = ksprintf failwith fmt

    [<CompiledName("PrintFormatToStringBuilder")>]
    let bprintf buf fmt  = kbprintf (fun _ -> ()) buf fmt 

    [<CompiledName("PrintFormatToTextWriter")>]
    let fprintf (os: TextWriter) fmt  = kfprintf (fun _ -> ()) os fmt 

    [<CompiledName("PrintFormatLineToTextWriter")>]
    let fprintfn (os: TextWriter) fmt  = kfprintf (fun _ -> os.WriteLine()) os fmt 

#if FX_NO_SYSTEM_CONSOLE
#else    
#if EXTRAS_FOR_SILVERLIGHT_COMPILER
    [<CompiledName("PrintFormat")>]
    let printf fmt = fprintf (!outWriter) fmt

    [<CompiledName("PrintFormatToError")>]
    let eprintf fmt = fprintf (!errorWriter) fmt

    [<CompiledName("PrintFormatLine")>]
    let printfn fmt = fprintfn (!outWriter) fmt

    [<CompiledName("PrintFormatLineToError")>]
    let eprintfn fmt = fprintfn (!errorWriter) fmt
#else
    [<CompiledName("PrintFormat")>]
    let printf fmt = fprintf System.Console.Out fmt

    [<CompiledName("PrintFormatToError")>]
    let eprintf fmt = fprintf System.Console.Error fmt

    [<CompiledName("PrintFormatLine")>]
    let printfn fmt = fprintfn System.Console.Out fmt

    [<CompiledName("PrintFormatLineToError")>]
    let eprintfn fmt = fprintfn System.Console.Error fmt
#endif
#endif    


