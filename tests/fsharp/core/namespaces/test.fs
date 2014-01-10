// #Conformance #Namespaces #SignatureFiles 


namespace Hello.Goodbye

type A = A | B | C

module Utils = begin
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
end

module X  = begin
  let x = 1 

end



namespace Hello.Beatles

type Song = HeyJude | Yesterday

module X  = begin
  let x = 2 
end


namespace UseMe

open Hello.Goodbye

module Tests  = begin
  do Hello.Goodbye.Utils.test "test292jwe" (Hello.Goodbye.X.x + 1 = Hello.Beatles.X.x)
  do Hello.Goodbye.Utils.test "test292jwe" (Hello.Beatles.HeyJude <> Hello.Beatles.Yesterday)

end

module MoreTests = begin
    open global.Microsoft.FSharp.Core

    let arr1 = global.Microsoft.FSharp.Collections.Array.map (global.Microsoft.FSharp.Core.Operators.(+) 1) [| 1;2;3;4 |]

    let ``global`` = 1

    // THis should still resolve
    let arr2 = global.Microsoft.FSharp.Collections.Array.map (global.Microsoft.FSharp.Core.Operators.(+) 1) [| 1;2;3;4 |]

    let test3 : global.Microsoft.FSharp.Core.int  = 3

    let test4 : global.Microsoft.FSharp.Collections.list<int>  = [3]

    let test5 x = 
        match x with 
        | global.Microsoft.FSharp.Core.None -> 1
        | global.Microsoft.FSharp.Core.Some _ -> 1
end


namespace global

type A = A | B | C

module X  = begin
  let x = 1 

end


module Utils  = begin

#if ALL_IN_ONE
    let RUN() = !failures
#else
    let aa =
      if not (!Hello.Goodbye.Utils.failures).IsEmpty then 
          stdout.WriteLine "Test Failed"
          exit 1
      else   
          stdout.WriteLine "Test Passed"
          System.IO.File.WriteAllText("test.ok","ok")
          exit 0
#endif

end
