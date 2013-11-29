#indent "off"


module Fields

let failures = ref false
let report_failure s = 
  stderr.WriteLine ("NO: test "+s+" failed"); failures := true


let testIndexers = 
  begin 
    if CSharpFields.S.intFieldStatic <> 0  then report_failure "test38432"; 
    if CSharpFields.S.objectFieldStatic <> null  then report_failure "test3823c2"; 
    if not (CSharpFields.S.structFieldStatic.Equals(box (new CSharpFields.EmptyStruct())))  then report_failure "test382d2c2"; 
    if CSharpFields.S.arrayFieldStatic <> null  then report_failure "tes983kjd2c2"; 
    let s = new CSharpFields.S() in 
    if s.intField <> 0  then report_failure "testiun2"; 
    if s.objectField <> null  then report_failure "test9ij23c2"; 
    if not (s.structField.Equals(box (new CSharpFields.EmptyStruct())))  then report_failure "test63h2c2"; 
    if s.arrayField <> null  then report_failure "tes90c3ejkd2c2"; 

    (* Can set fields on mutable values. *)
    let mutable s2 = new CSharpFields.S() in 
    if s2.intField <> 0  then report_failure "testiun2mmb";
    if s2.intProperty <> 0  then report_failure "testi8hh6un2mmb";

    s2.intField <- 1;
    if s2.intField <> 1  then report_failure "testiun2mmm"; 

    (* Can call property setters on mutable values. *)
    s2.intProperty <- 1;
    if s2.intProperty <> 1  then report_failure "testiuyf67m"; 

    (* Can set fields in byref params. *)
    let cb = { new  CSharpFields.ByrefCallback() with member __.Callback(arg) = arg.intField <- 4; } in

    let s3 = ref (new CSharpFields.S()) in 
    if (!s3).intField  <> 0 then report_failure "tesdce34tiun2mmb";
    Printf.printf "(!s3).intField = %d\n" (!s3).intField;


    CSharpFields.Helpers.CallByrefCallback(s3, cb);
    if (!s3).intField <> 4  then report_failure "tesdetiun2mmm"; 

    (* Can call property setters on  byref params. *)

    let cb2 = { new  CSharpFields.ByrefCallback() with member __.Callback(arg) = arg.intProperty <- 4; } in
    Printf.printf "(!s3).intProperty = %d\n" (!s3).intProperty;
    if (!s3).intProperty <> 0  then report_failure "ewecew34tiun2mmb";

    CSharpFields.Helpers.CallByrefCallback(s3, cb2);
    if (!s3).intProperty <> 4  then report_failure "tesdetiun2mmm"; 


    (* Literals - pattern matching *)
    if not (match 3 with CSharpFields.S.intFieldConst -> true | _ -> false)  then report_failure "tes998u78"; 
    if not (match byte 4 with CSharpFields.S.byteFieldConst -> true | _ -> false)  then report_failure "tes998u78"; 
    if not (match 'a' with CSharpFields.S.charFieldConst -> true | _ -> false)  then report_failure "tes998u78"; 
    if not (match "help" with CSharpFields.S.stringFieldConst -> true | _ -> false)  then report_failure "tes998u78"; 
    if not (match 3.14f with CSharpFields.S.singleFieldConst -> true | _ -> false)  then report_failure "tes998u78"; 
    if not (match 3.14 with CSharpFields.S.doubleFieldConst -> true | _ -> false)  then report_failure "tes998u78"; 

    (* Literals - expressions *)
    if CSharpFields.S.intFieldConst <> 3  then report_failure "tes998e7322"; 
    if CSharpFields.S.byteFieldConst <> byte 4  then report_failure "te9390e7322"; 
    if CSharpFields.S.charFieldConst <> 'a'  then report_failure "te99dw0e7322"; 
    if CSharpFields.S.stringFieldConst <> "help"  then report_failure "cew09dw22"; 
    if CSharpFields.S.singleFieldConst <> 3.14f  then report_failure "dw0dw22"; 
    if CSharpFields.S.doubleFieldConst <> 3.14  then report_failure "dw0dwdw-22"; 

  end

module TestGenericFields = begin
  open System.Collections.Generic

  let check s e r = if r = e then  stdout.WriteLine (s^": YES") else (stdout.WriteLine (s^": FAIL"); report_failure s)
    
  let testGettingGenericField1(x: KeyValuePair<_,_>) = x.Value
(*
  let testSettingGenericField1(x: KeyValuePair<_,_> ref) y = 
    let mutable v = !x in 
    v.Value <- y;
    x := v
*)
  let testGettingGenericField2(x: KeyValuePair<_,_>) = x.Key
(*
  let testSettingGenericField2(x: KeyValuePair<_,_> ref) y = 
    let mutable v = !x in 
    v.Key <- y;
    x := v
*)
       
  do check "ewoi93c3" (testGettingGenericField1 (new KeyValuePair<_,_>(3,"abc"))) "abc"
  let v1 = ref (new KeyValuePair<_,_>(3,"abc"))
(*
  do testSettingGenericField1 v1 "def"
  do check "ewoi93c3" (testGettingGenericField1 !v1) "def"
*)

  do check "ewoi93c3" (testGettingGenericField2 (new KeyValuePair<_,_>(3,"abc"))) 3
  let v2 = ref (new KeyValuePair<_,_>(3,"abc"))
(*
  do testSettingGenericField2 v2 4
  do check "ewoi93c3" (testGettingGenericField2 !v2) 4
*)

end

let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)
