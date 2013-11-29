#indent "off"


open Methods

let failures = ref false
let test s b = 
  if not b then (stderr.WriteLine ("FAILED: "+s); failures := true)




do test "fws321" (let res = ref 0 in Methods.StaticMethodsWithRefParams.IntegerOutParam(res); !res = 3)

do test "fws322" (let x = {new Methods.VirtualMethodsWithRefParams() with member __.IntegerOutParam(res) = res <- 4} in
                  let res = ref 0 in 
                  (x.IntegerOutParam(res); !res = 4))
         
do test "fws3223" (let x = {new Methods.VirtualMethodsWithRefParams() with member __.IntegerOutParam(res : int byref) = res <- 4} in
                  let res = ref 0 in 
                  (x.IntegerOutParam(res); !res = 4))
         

let tester(x1,x2,x3,x4) = 
  let o = new TestMethods<_,_> (* <T,U> *) () in
  ignore (o.m0()) ;
  ignore (o.m1(x1)) ;
  ignore (o.inline_m1(x1) );
  ignore (o.withlocal_m1(x1));
  ignore (o.m2(x1,x2) );
  ignore (o.inline_m2(x1,x2));
  ignore (o.withlocal_m2(x1,x2));
  ignore (o.m3 (* (<V>) *)(x1,x2,x3));
  ignore (o.inline_m3 (* (<V>) *)(x1,x2,x3));
  ignore (o.withlocal_m3 (* (<V>) *)(x1,x2,x3));
  ignore (o.m4 (* (<V,W>) *)(x1,x2,x3,x4));
  ignore (o.inline_m4 (* (<V,W>) *) (x1,x2,x3,x4));
  ignore (o.withlocal_m4 (* (<V,W>) *) (x1,x2,x3,x4));
  ignore (o.r1(ref x1));
  ignore (o.inline_r1(ref x1));
  ignore (o.withlocal_r1(ref x1));
  ignore (o.r2(ref x1,ref x2));
  ignore (o.inline_r2(ref x1,ref x2));
  ignore (o.withlocal_r2(ref x1,ref x2));
  ignore (o.r3 (* (<V>) *) (ref x1,ref x2,ref x3));
  ignore (o.inline_r3 (* (<V>) *)(ref x1,ref x2,ref x3));
  ignore (o.withlocal_r3 (* (<V>) *) (ref x1,ref x2,ref x3));
  ignore (o.r4(* (<V,W>) *) (ref x1,ref x2,ref x3,ref x4));
  ignore (o.inline_r4 (* (<V,W>) *) (ref x1,ref x2,ref x3,ref x4));
  ignore (o.withlocal_r4 (* (<V,W>) *) (ref x1,ref x2,ref x3,ref x4))
    
do
  tester (* (<int,int,int,int>) *) (1,2,3,4);
  tester (* (<E,E,E,E>) *) (E.Red,E.Green,E.Blue,E.Black);
  tester (* (<int,string,int,string>) *) (1,"2",3,"string4");
  tester (* (<int,OneString,int,OneString>) *) (1,new OneString("2"),3,new OneString("string4"));
  tester (* (<E,string,E,string>) *) (E.Red,"2",E.Blue,"string4");
  tester (* (<int,object,int,object>) *) (1,"2",3,"string4");
  let dt = System.DateTime.Parse("Jan 1 2001") in
  tester (* (<DateTime,DateTime,DateTime,DateTime>) *) (dt,dt,dt,dt);
  tester (* (<DateTime,string,DateTime,string>) *) (dt,"string2",dt,"string4");
  tester (* (<DateTime,object,DateTime,object>) *) (dt,"string2",dt,"string4");
  tester (* (<DateTime,int,DateTime,int>) *) (dt,2,dt,4);
  tester (* (<double,int,double,int>) *) (1.0,2,3.0,4);
  tester (* (<E,string,E,string>) *) (E.Red,"string2",E.Blue,"string4");
  tester (* (<E,OneString,E,OneString>) *) (E.Red,new OneString("string2"),E.Blue,new OneString("string4"));
  tester (* (<double,string,double,string>) *) (1.0,"string2",3.0,"string4");
  tester (* (<OneDouble,string,double,string>) *) (new OneDouble(1.0),"string2",3.0,"string4");
  tester (* (<double,object,double,object>) *) (1.0,"string2",3.0,"string4")

let sort (f: 'a -> 'a -> int) (arr : 'a array) =
  let len = Array.length arr in 
  if len < 2 then () 
  else 
    if len = 2 then begin
      let c = f arr.[0] arr.[1] in 
      if c > 0 then  begin
        let tmp = arr.[0] in 
        arr.[0] <- arr.[1]; 
        arr.[1] <- tmp
      end
    end             
    else begin
        System.Array.Sort( arr, 
                          { new System.Collections.Generic.IComparer<_> 
                              with member __.Compare(a,b) = f a b
                       } )
    end

// Overloads

open System

do Console.WriteLine("res = {0}\n",Decimal.op_Addition(new Decimal(10), new Decimal(10)))
do Console.WriteLine("res = {0}\n",(new Decimal(10)) + (new Decimal(10)))
do Console.WriteLine("res = {0}\n",(new DateTime(1970,10,1)) + (new TimeSpan(1000000000L)))
//do Console.WriteLine("res = {0}\n",(new DateTime(1970,10,1)) - (new TimeSpan(1000000000L)))
//do Console.WriteLine("res = {0}\n",(new DateTime(1970,10,1)) - (new DateTime(1970,10,1)))
do Console.WriteLine("res = {0}\n",(new Decimal(20)) / (new Decimal(10)))
do Console.WriteLine("res = {0}\n",(new Decimal(20)) - (new Decimal(10)))
do Console.WriteLine("res = {0}\n",(new Decimal(20)) * (new Decimal(10)))

do Console.WriteLine("res = {0}\n",(new Decimal(20)) % (new Decimal(7)))


module CheckOverloadedMethods_FSharp_1_0_4546 = begin


    open Methods.TestLibrary

    module ErrorCase1 =  begin
        //        public static TestStruct operator *(float scalar, TestStruct testStruct)
        //        public static TestStruct Add(TestStruct a, TestStruct b)
        //        public static void Add(ref TestStruct a, ref TestStruct b, out TestStruct result)
        let a = TestStruct 5.0f
        let b = TestStruct 5.0f
        let r = 5.0f * (TestStruct.Add(a, b))  // ERROR
    end

    module OkCase2 =  begin
        //        public static JustByVal operator *(float scalar, JustByVal testStruct)
        //        public static JustByVal Add(JustByVal a, JustByVal b)
        let a = JustByVal 5.0f
        let b = JustByVal 5.0f
        let r = 5.0f * (JustByVal.Add (a, b))   // OK
    end

    module OkCase3 =  begin
    //        public static JustByRef operator *(float scalar, JustByRef testStruct)
    //        public static void Add(ref JustByRef a, ref JustByRef b, out JustByRef result)
        let mutable a = JustByRef 5.0f
        let mutable b = JustByRef 5.0f
        let r =  5.0f * (JustByRef.Add (&a, &b))  // OK
    end


end


let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)






