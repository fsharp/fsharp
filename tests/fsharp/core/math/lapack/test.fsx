// #Regression #Conformance #Interop 
//----------------------------------------------------------------------------
// (c) Microsoft Corporation 2005-2006.  
//
// Tutorial Part 1. Understanding interfacing between F# and C/Fortran code
// Tutorial Part 2. Binding to LAPACK.
// Tutorial Part 3. Managing Fortran v. C Matrix Layout
// Tutorial Part 4. Pinning F# Matrix and Vector objects
// Tutorial Part 5. Building some high-level mutating routines 
// Tutorial Part 6. Building some easy-to-use high-level applicative routines 
// Tutorial Part 7. Reusing the primitive bindings on other similarly shaped data structures

#light

#nowarn "0044"  // suppress warnings about the use of native pointer features to interoperate with native code
#nowarn "0051"  // suppress warnings about the use of byref features to interoperate with native code
#nowarn "0049"  // turn off warnings about using upper case identifiers for variables (e.g. matrices)

let failures = ref []

let report_failure (s : string) = 
    stderr.Write" NO: "
    stderr.WriteLine s
    failures := !failures @ [s]

let test (s : string) b = 
    stderr.Write(s)
    if b then stderr.WriteLine " OK"
    else report_failure (s)

let check s b1 b2 = test s (b1 = b2)

//namespace Microsoft.FSharp.Math.Bindings.LAPACK


module AdhocNativeTests = begin
    let f (x : nativeptr<int>) = x

    let mutable x = 1

    do f &&x // sanity check to make sure method can be called

    [<AbstractClass>]
    type C() = 
      class
        abstract AM : nativeptr<int> -> nativeptr<int>
        static member M(x : nativeptr<int>) = x
        member a.IM(x : nativeptr<int>) = x
      end

    type D() = 
      class
        inherit C()
        override a.AM(x : nativeptr<int>) = x
        static member M(x : nativeptr<int>) = x
      end

    type DD() = 
      class
        inherit System.Text.Decoder()
        override x.GetChars(a:nativeptr<byte>,b:int,c:nativeptr<char>,d:int,e:bool) = 0
        override x.GetChars(a:byte[],b:int,c:int,d:char[],e:int) = 0
        override x.GetCharCount(a:byte[],b:int,d:int) = 0
      end

    do C.M &&x // sanity check to make sure method can be called
    let d = D() 
    let p2 = d.AM &&x // sanity check to make sure method can be called
    let p3 = d.IM &&x // sanity check to make sure method can be called

    let dd = DD() 

    let mutable b = 0uy
    let mutable c = '0'
    do dd.GetChars(&&b, 0, &&c, 0, true) // sanity check to make sure method can be called
end


#if ALL_IN_ONE
let RUN() = !failures
#else
let aa =
  match !failures with 
  | [] -> 
      stdout.WriteLine "Test Passed"
      System.IO.File.WriteAllText("test.ok","ok")
      exit 0
  | _ -> 
      stdout.WriteLine "Test Failed"
      exit 1
#endif


