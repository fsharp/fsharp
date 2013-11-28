#indent "off"

let failures = ref false
let report_failure s  = 
  stderr.WriteLine ("NO: test "+s+" failed"); failures.Value <- true

let test s b = 
  if not b then (stderr.WriteLine ("FAILED: "+s); failures.Value <- true)


open Byrefs

let testByrefs = 
  let dt1 = System.DateTime.Now in 
  let dt2 = System.DateTime.op_Subtraction(System.DateTime.Now, new System.TimeSpan(1,0,0)) in 
  if (Simple.DateTimeChoice(dt1,dt2,true) <> dt1) then report_failure "NO! (cwejikwe)";
  if (Simple.DateTimeChoice(dt1,dt2,false) <> dt2) then report_failure "NO! (cfew20jikwe)";

  if (Simple.DateTimeChoiceRef(ref dt1,ref dt2,true) <> dt1) then report_failure "NO! (cwej25ikwe)";
  if (Simple.DateTimeChoiceRef(ref dt1,ref dt2,false) <> dt2) then report_failure "NO! (cfew253220jikwe)";

  let mutable mdt1 = System.DateTime.Now in 
  let mutable mdt2 = System.DateTime.op_Subtraction(System.DateTime.Now, new System.TimeSpan(1,0,0)) in 
  if (Simple.DateTimeChoice(mdt1,mdt2,true) <> mdt1) then report_failure "NO! (cwejikwe)";
  if (Simple.DateTimeChoice(mdt1,mdt2,false) <> mdt2) then report_failure "NO! (cfew20jikwe)";

  if (Simple.DateTimeChoiceRef(&mdt1,&mdt2,true) <> mdt1) then report_failure "NO! (cwej25ikwe)";
  if (Simple.DateTimeChoiceRef(&mdt1,&mdt2,false) <> mdt2) then report_failure "NO! (cfew253220jikwe)";

  let out = ref dt1 in 
  Simple.DateTimeChoiceOut(ref dt2,out);
  if !out <> dt2 then report_failure "NO! (cwejikwe)";

  let mutable mout = mdt1 in 
  Simple.DateTimeChoiceOut(&mdt2,&mout);
  if mout <> mdt2 then report_failure "NO! (cwejikwe)";

  let x = MakeGenerics.MakeGenericInt() in
  if (x.Choice(1,2,true) <> 1) then report_failure "NO! (cwejikw544se)";
  if (x.Choice(2,3,false) <> 3) then report_failure "NO! (cfew20jikw65h6ye)";

  if (x.ChoiceRef(ref 1,ref 2,true) <> 1) then report_failure "NO! (cwejwecikwe)";
  if (x.ChoiceRef(ref 2,ref 3,false) <> 3) then report_failure "NO! (cewcfew20jikwe)";

  ()


do test "fws321" (let res = ref 0 in ClassWithStaticMethods.OneIntegerOutParam(res); !res = 3)

do test "fws322" (let x = {new ClassWithVirtualMethods() 
                           with member __.OneIntegerOutParam(res) = res <- 4; 
                                member __.TwoIntegerOutParams(res1, res2) = res1 <- res2; res2 <- 5} in
                  let res0 = ref 0 in 
                  let res1 = ref 0 in 
                  let res2 = ref 2 in 
                  x.OneIntegerOutParam(res0); 
                  x.TwoIntegerOutParams(res1,res2); 
                  !res0 = 4 &&
                  !res1 = 2 &&
                  !res2 = 5)
         
do test "fws322b" (let x = {new ClassWithVirtualMethods() 
                            with member __.OneIntegerOutParam(res) = res <- 4; 
                                 member __.TwoIntegerOutParams(res1, res2) = res1 <- res2; res2 <- 5} in
                  let mutable res0 = 0 in 
                  let mutable res1 = 0 in 
                  let mutable res2 = 2 in 
                  x.OneIntegerOutParam(&res0); 
                  x.TwoIntegerOutParams(&res1,&res2); 
                  res0 = 4 &
                  res1 = 2 &
                  res2 = 5)
         

(*
do test "fws323" (let f = new DelegateWithOneOutParam(fun x y -> () ) in 
                  let res = ref 0 in 
                  f.Invoke(3,res); 
                  !res = 0)
*)
         
(*
do test "fws323" (let f = new DelegateWithTwoOutParams(fun x y -> y <- x) in 
                  let res = ref 0 in 
                  f.Invoke(3,res); 
                  !res = 3)
         
*)



open System
open System.Windows.Forms

let form =  
  { new Form() 
    with member __.WndProc(m : Message byref) = base.WndProc(&m) } ;;
form.Visible <- true;;

let timer = new Timer()
do timer.Interval <- 200 (* ms *)
do timer.add_Tick(new EventHandler(fun _ _ -> form.Close()));;
do timer.Start();;

Application.Run(form);;


let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)

