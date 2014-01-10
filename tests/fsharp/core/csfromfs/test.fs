// #Conformance #Interop #Fields #MemberDefinitions #MethodsAndProperties #Events #Classes #OptionalArguments 
#indent "off"


let failures = ref false
let report_failure () = 
  stderr.WriteLine "NO"; failures := true



(*
let myForm n = {new Form("abc",n,4,6) with 
                  with OnPaint ev = this.OnPaint()... this.base.OnPaint() .... Printf.printf "%d" n; 
                  and  OnButtonClick(this,ev) = ... }
let myForm n = {new Form() with OnPaint(ev) = Printf.printf "%d" n; }

*)


let myComparer = {new System.Collections.IComparer with Compare(a,b) = compare a b}
let myGComparer () = {new System.Collections.Generic.IComparer<'a>
                      with Equals(x,y) = (x = y) 
                      and  GetHashCode(x) = Hashtbl.hash x
                      and  Compare(a,b) = compare a b}
let myFormattable = {new System.IFormattable with ToString(fmt,fmtProvider) = "<to-string>"}
let myEnum = 
  let start = 10 in 
  let x = ref start in  
  {new System.Collections.IEnumerator 
              with get_Current() = box("x = "^string_of_int !x) 
              and MoveNext() = (x := !x - 1; (!x > 0) )
              and Reset() = x := start}

let iterate (e: System.Collections.IEnumerator) f = 
  while (e.MoveNext()) do
    f e.Current;
  done

let _ = iterate myEnum (fun obj -> Printf.printf "item = %s\n" (unbox obj))

let myEnumG = 
  let start = 10 in 
  let x = ref start in  
  {new System.Collections.Generic.IEnumerator`1<string>
              with get_Current() = "x = "^string_of_int !x
              and MoveNext() = (x := !x - 1; (!x > 0) ) 
              and Dispose() = () }

let iterateG (e: 'a System.Collections.Generic.IEnumerator`1) f = 
  while (e.MoveNext()) do
    f e.Current;
  done

let _ = iterateG myEnumG (fun s -> Printf.printf "generic item = %s\n" s)



let myForm title n =
  let res = 
    {new System.Windows.Forms.Form() 
      with OnPaint(paintEventArgs) = Printf.printf "OnPaint: n = %d\n" n
      and OnResize(eventArgs) = Printf.printf "OnResize: n = %d\n" n } in
   res.Text <- title;
   res

let form1 = myForm "Here be the main form" 3
let form2 = myForm "Here be a dialog" 4
let _ = form2.ShowDialog()

let _ = System.Windows.Forms.Application.Run(form1)
                       
                       
let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
    else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)




