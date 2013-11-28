#indent "off"



let failures = ref false
let test s b = 
  if not b 
  then (stderr.WriteLine ("FAILED: "+s); failures.Value <- true) 
  else (stderr.WriteLine ("PASSED: "+s))


let myComparer = {new System.Collections.IComparer with member x.Compare(a,b) = Unchecked.compare a b}

let myGComparer () = {new System.Collections.Generic.IComparer<'a>
                      with member x.Compare(a,b) = compare a b}

let myFormattable = {new System.IFormattable with member x.ToString(fmt,fmtProvider) = "<to-string>"}
let myEnum = 
  let start = 10 in 
  let x = ref start in  
  {new System.Collections.IEnumerator 
              with member __.Current = box("x = " + string !x) 
                   member __.MoveNext() = (x := !x - 1; (!x > 0) )
                   member __.Reset() = x := start}

let iterate (e: System.Collections.IEnumerator) f = 
  while (e.MoveNext()) do
    f e.Current;
  done

let _ = iterate myEnum (fun obj -> Printf.printf "item = %s\n" (unbox obj))


let iterateG (e: 'a System.Collections.Generic.IEnumerator) f = 
  while (e.MoveNext()) do
    f e.Current;
  done


let myEnumG = 
  let start = 10 in 
  let x = ref start in  
  {new System.Collections.Generic.IEnumerator<string>
              with member __.Current = "x = "+string !x
    interface System.Collections.IEnumerator
      with member __.Current = box ("x = "+string !x)
           member __.MoveNext() = (x := !x - 1; (!x > 0) )
           member __.Reset() = x := 0
    interface System.IDisposable
      with member __.Dispose() = () }

let _ = iterateG myEnumG (fun s -> Printf.printf "generic item = %s\n" s)


let myEnumG2 = 
  let start = 10 in 
  let x = ref start in  
  {new System.Collections.Generic.IEnumerator<string>
      with member __.Current = "x = "+string !x
           member __.Dispose() = ()
    interface System.Collections.IEnumerator
      with member __.Current = box ("x = "+string !x)
           member __.MoveNext() = (x := !x - 1; (!x > 0) )
           member __.Reset() = x := 0 }
let _ = iterateG myEnumG2 (fun s -> Printf.printf "generic item = %s\n" s)


let myForm title n =
  let res = 
    {new System.Windows.Forms.Form() 
      with member __.OnPaint(args) = base.OnPaint(args);  Printf.printf "OnPaint: n = %d\n" n
           member __.OnResize(args) = base.OnResize(args); Printf.printf "OnResize: n = %d\n" n } in
   res.Text <- title;
   res

let [<System.Obsolete("This is obsolete")>] 
    myComparerFactory1() = 
    {new System.Collections.IComparer with member __.Compare(a,b) = Unchecked.compare a b}


let form1 = myForm "Here be the main form" 3
let form2 = myForm "Here be a dialog" 4

let a = new MyStruct(4) 
let b = new MyStruct() 

let a1 = new global.MyStruct(4) 
let b2 = new global.MyStruct() 

let g164 = {new TypesWithProperties.Interface 
         with member __.objectProperty = box(1) 
              member __.stringProperty = "abc" }
let _ = test "g164 vriow" (unbox g164.objectProperty = 1)
let _ = test "g164 vr2ow" (g164.stringProperty = "abc")
         
let _ = {new TypesWithProperties.AbstractBase() 
         with member __.splitProperty with get() = 1  and set x = () }

open System
(* This one once created unverifiable code *)
let concatstring = 
  (new Decimal(0)).ToString() ^ 
  (new Decimal(0)).ToString() ^ 
  (new Decimal(0)).ToString() ^ 
  (new Decimal(0)).ToString() ^ 
  (new Decimal(0)).ToString() ^ 
  (new Decimal(0)).ToString() ^ 
  "test"

do Printf.printf "concatstring = %s\n" concatstring

// Nb. implicit conversion from lambda expressions to delegates for member applications only

let x = new System.Windows.Forms.Form()
do x.add_Paint(fun _ _ -> ())

let x2 = new global.System.Windows.Forms.Form()
do x2.add_Paint(fun _ _ -> ())

module MutableStructTests = begin
    let test2() =
         let o = new Container() in
         test "mutable struct test 1" (o.nested.v = 0);
         test "mutable struct test 2" (o.nested2.v = 0);
         o.nested.setV(1);
         o.nested2.setV(1);
         test "mutable struct test 3" (o.nested.v = 1);
         test "mutable struct test 4" (o.nested2.v = 0);
         
         let o = new Container() in
         test "mutable struct test 1b" (o.nestedNested.v.v = 0);
         test "mutable struct test 2b" (o.nestedNested2.v.v = 0);
         o.nestedNested.setV(1);
         o.nestedNested2.setV(1);
         test "mutable struct test 3b" (o.nestedNested.v.v = 1);
         test "mutable struct test 4b" (o.nestedNested2.v.v = 0);

         // This uses a named-argument-constructor-property-setter for the struct
         let o = new Container(nested=Nested(v=1)) in
         test "mutable struct test 5" (o.nested.v = 1)
         

         
(* THis hits an NYI:         
         
         let o = new Container() in
         test "mutable struct test 1b" (o.nestedNested.v.v = 0);
         test "mutable struct test 2b" (o.nestedNested2.v.v = 0);
         o.nestedNested.v.v <- 1;
         o.nestedNested2.v.v <- 1;
         test "mutable struct test 3b" (o.nestedNested.v.v = 1);
         test "mutable struct test 4b" (o.nestedNested2.v.v = 0)
*)

    do test2()


    type Test = struct
       val mutable v: int
       member t.setV v =
           t.v <- v
    end

    let test2131() =
         let mutable t = new Test() in
         test "mutable struct test 5a" (t.v  = 0);
         t.setV(1);
         test "mutable struct test 5b" (t.v  = 1);

         let (* immutable *) t = new Test() in
         test "mutable struct test 5c" (t.v  = 0);
         t.setV(1);
         test "mutable struct test 5d" (t.v  = 0) // immutable!

    do test2131()

    type TestTest = struct
       val mutable v: Test
       member t.setV v =
           t.v <- v
    end

    let test31r13() =
         let mutable t = new Test() in
         test "mutable struct test 6" (t.v  = 0);
         t.setV(1);
         test "mutable struct test 7" (t.v  = 1);
         let mutable t = new Test() in
         test "mutable struct test 8" (t.v  = 0);
         t.v <- 1;
         test "mutable struct test 9" (t.v  = 1);

(*
         let mutable t = new TestTest() in
         test "mutable struct test 10" (t.v.v  = 0);
         t.v.v <- 1;
         test "mutable struct test 11" (t.v.v  = 1);

         let mutable t = new TestTest() in
         test "mutable struct test 12" (t.v.v  = 0);
         t.v.setV(1);
         test "mutable struct test 13" (t.v.v  = 1);

         let t = new TestTest() in
         test "mutable struct test 14" (t.v.v  = 0);
         t.v.setV(1);
         test "mutable struct test 15" (t.v.v  = 0)
*)

    do test31r13()
end


module MutableStructTestsGeneric = begin
    let test2() =
         let o = new ContainerGeneric<int>() in
         test "mutable struct test 1" (o.nested.v = 0);
         test "mutable struct test 2" (o.nested2.v = 0);
         o.nested.setV(1);
         o.nested2.setV(1);
         test "mutable struct test 3" (o.nested.v = 1);
         test "mutable struct test 4" (o.nested2.v = 0);
         
         let o = new ContainerGeneric<int>() in
         test "mutable struct test 1b" (o.nestedNested.v.v = 0);
         test "mutable struct test 2b" (o.nestedNested2.v.v = 0);
         o.nestedNested.setV(1);
         o.nestedNested2.setV(1);
         test "mutable struct test 3b" (o.nestedNested.v.v = 1);
         test "mutable struct test 4b" (o.nestedNested2.v.v = 0)
         
(* THis hits an NYI:         
         
         let o = new Container() in
         test "mutable struct test 1b" (o.nestedNested.v.v = 0);
         test "mutable struct test 2b" (o.nestedNested2.v.v = 0);
         o.nestedNested.v.v <- 1;
         o.nestedNested2.v.v <- 1;
         test "mutable struct test 3b" (o.nestedNested.v.v = 1);
         test "mutable struct test 4b" (o.nestedNested2.v.v = 0)
*)

    do test2()


    type TestGeneric<'a> = struct
       val mutable v: 'a
       member t.setV v =
           t.v <- v
    end

    let test2131() =
         let mutable t = new TestGeneric<int>() in
         test "mutable struct test 5a" (t.v  = 0);
         t.setV(1);
         test "mutable struct test 5b" (t.v  = 1);

         let (* immutable *) t = new TestGeneric<int>() in
         test "mutable struct test 5c" (t.v  = 0);
         t.setV(1);
         test "mutable struct test 5d" (t.v  = 0) // immutable!

    do test2131()

    type TestTestGeneric<'a> = struct
       val mutable v: TestGeneric<'a>
       member t.setV v =
           t.v <- v
    end

    let test31r13() =
         let mutable t = new TestGeneric<int>() in
         test "mutable struct test 6" (t.v  = 0);
         t.setV(1);
         test "mutable struct test 7" (t.v  = 1);
         let mutable t = new TestGeneric<int>() in
         test "mutable struct test 8" (t.v  = 0);
         t.v <- 1;
         test "mutable struct test 9" (t.v  = 1);
(*
         let mutable t = new TestTestGeneric<int>() in
         test "mutable struct test 10" (t.v.v  = 0);
         t.v.v <- 1;
         test "mutable struct test 11" (t.v.v  = 1);

         let mutable t = new TestTestGeneric<int>() in
         test "mutable struct test 12" (t.v.v  = 0);
         t.v.setV(1);
         test "mutable struct test 13" (t.v.v  = 1);

         let t = new TestTestGeneric<int>() in
         test "mutable struct test 14" (t.v.v  = 0);
         t.v.setV(1);
         test "mutable struct test 15" (t.v.v  = 0)
*)
    do test31r13()
end


module InheritAbstractBase = begin

	type Inherit1() = class
	    inherit TypesWithProperties.AbstractBase()
	    override x.splitProperty with get() = 3 and set v = ()

  end
  let _ = Inherit1()

	type Inherit2() = class
	    inherit TypesWithProperties.DerivedAbstractClass()
	    override x.splitProperty with get() = 3 

  end
  let _ = Inherit2()

	type Inherit3() = class
	    inherit TypesWithProperties.DerivedClass()
	    override x.splitProperty with set v = ()

  end
  let _ = Inherit3()

	type Inherit4() = class
	    inherit TypesWithProperties.ExplicitInterfaceClass()

  end
  let _ = Inherit4()
  
	type Inherit5() = class
	    inherit TypesWithProperties.ExplicitInterfaceClass()

  end
  let _ = Inherit5()

	type Inherit6() = class
	    inherit TypesWithProperties.PartialExplicitInterfaceClass1()
	    override x.objectProperty with get() = new obj()

  end
  let _ = Inherit6()
end

                       
let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)





