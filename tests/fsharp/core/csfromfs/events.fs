#indent "off"

let failures = ref false
let report_failure () = 
  stderr.WriteLine "NO"; failures := true

open Events

let testEvents = 
  let handler_state = ref 10 in 
  let handler = new Handler(fun i -> handler_state := !handler_state + i; ()) in 
  handler.Invoke(5);
  if !handler_state <> 15 then report_failure();
  begin 
    let x = new C() in 
    let handler_state = ref 10 in 
    x.add_HandlerEvent(new Handler(fun i -> handler_state := !handler_state + i; ()));
    x.FireHandlerEvent(3);
    if !handler_state <> 13 then report_failure();
    let h2 = new Handler(fun i -> handler_state := !handler_state + i; ()) in
    x.add_HandlerEvent(h2);
    x.FireHandlerEvent(4);
    if !handler_state <> 21 then report_failure();
    x.remove_HandlerEvent(h2);
    x.FireHandlerEvent(1);
    if !handler_state <> 22 then report_failure();
  end;
  begin 
    let x = new C() in 
    let handler_state = ref 10 in 
    x.add_HandlerPropEvent(new Handler(fun i -> handler_state := !handler_state + i; ()));
    x.FireHandlerPropEvent(3);
    if !handler_state <> 13 then report_failure();
    let h2 = new Handler(fun i -> handler_state := !handler_state + i; ()) in
    x.add_HandlerPropEvent(h2);
    x.FireHandlerPropEvent(4);
    if !handler_state <> 21 then report_failure();
    x.remove_HandlerPropEvent(h2);
    x.FireHandlerPropEvent(1);
    if !handler_state <> 22 then report_failure();
  end;
  begin 
    let handler_state = ref 10 in 
    C.add_HandlerEventStatic(new Handler(fun i -> handler_state := !handler_state + i; ()));
    C.FireHandlerEventStatic(3);
    if !handler_state <> 13 then report_failure();
    let h2 = new Handler(fun i -> handler_state := !handler_state + i; ()) in
    C.add_HandlerEventStatic(h2);
    C.FireHandlerEventStatic(4);
    if !handler_state <> 21 then report_failure();
    C.remove_HandlerEventStatic(h2);
    C.FireHandlerEventStatic(1);
    if !handler_state <> 22 then report_failure();
  end;
  begin 
    let handler_state = ref 10 in 
    C.add_HandlerPropEventStatic(new Handler(fun i -> handler_state := !handler_state + i; ()));
    C.FireHandlerPropEventStatic(3);
    if !handler_state <> 13 then report_failure();
    let h2 = new Handler(fun i -> handler_state := !handler_state + i; ()) in
    C.add_HandlerPropEventStatic(h2);
    C.FireHandlerPropEventStatic(4);
    if !handler_state <> 21 then report_failure();
    C.remove_HandlerPropEventStatic(h2);
    C.FireHandlerPropEventStatic(1);
    if !handler_state <> 22 then report_failure();
  end

       
let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)
