#indent "off"




let failures = ref false
let test s b = 
  if not b then (stderr.WriteLine ("test "+s+" failed"); failures := true)

let l = new System.Collections.ArrayList()
let _ = l.Add("abc")
let _ = System.Console.WriteLine("{0}",l.Item(0))
let _ = l.Add(box 1)
let _ = System.Console.WriteLine("{0}",l.Item(0))
let _ = System.Console.WriteLine("{0}",l.Item(1))
let _ = test "fewoij21" ((l.Item(0) :?> string) = "abc")
let _ = test "fewoij332" (unbox(l.Item(1)) = 1)

let genericList = new System.Collections.Generic.List<_>()  (* The compiler works out that it is List<int> from the uses below *)

let _ = genericList.Add(12)
let _ = System.Console.WriteLine("{0}",box(genericList.Item(0)))
let _ = test "fewoij21" (genericList.Item(0) = 12)
let _ = genericList.Add(1)
let _ = System.Console.WriteLine("{0}",box(genericList.Item(0)))
let _ = System.Console.WriteLine("{0}",box(genericList.Item(1)))
let _ = test "fewoij21" (genericList.Item(0) = 12)
let _ = test "fewoij332" (genericList.Item(1) = 1)



let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok");
        exit 0)


