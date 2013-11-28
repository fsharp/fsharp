
module Properties

let failures = ref false
let report_failure s  = 
  stderr.WriteLine ("NO: test "+s+" failed"); failures := true

open CSharpProperties

let testProperties = 
  let c = new Class() in 
  if c.prop <> 12 then report_failure "fweoijwe";
  c.prop <- 13;
  if c.prop <> 13 then report_failure "oijeoijwe";
  let s = new Struct() in 
  if s.prop <> 0 then report_failure "fwe26e";
(* BUG: mutation of structs has incorrect semantics 
  s.prop <- 17;
  if s.prop <> 17 then report_failure "oiuowjeoijwe"; *)
  ()

// Check that func -->delegate conversions apply for implicit property setters, for ad C#-defined type
module FSharp_1_0_Bug_6389_Example1 = 
    open Ninject.Planning.Bindings
     
    let h1 = new Binding( Condition = new System.Func<IRequest, bool>((fun x -> true)) );
     
    let h2 = new Binding( Condition = fun x -> true );
     
    // Note, this does typecheck, because uses of property setters are method calls, and hence member conversions are allowed
    let k = new Binding();
    k.Condition <- fun x -> false

    let t1 = k.Condition.Invoke(new Request())
    let t2 = h1.Condition.Invoke(new Request())
    let t3 = h2.Condition.Invoke(new Request())

     
// Check same, for an F#-defined type
module FSharp_1_0_Bug_6389_Example2 = 
    type IRequest = interface end
    type IBinding =
      abstract Service : System.Type
      abstract Condition :System.Func<IRequest, bool> with get,set
 
    type Binding() = 
      let mutable service = null
      let mutable cond = null
      interface IBinding with 
 
        member x.Service with get() = service 
        member x.Condition with get() = cond and set v = (cond <- v)
      member x.Service with get() = service 
      member x.Condition with get() = cond and set v = (cond <- v)
 
    type Request()  =
      interface IRequest
 

    // Explicit coercion fun -> Func: no runtime exception
    let h1 = new Binding( Condition = new System.Func<IRequest, bool>((fun x -> true)) );
     
    // No explicit coercion fun -> Func: runtime exception "Invalid cast..."
    let h2 = new Binding( Condition = fun x -> true );
     
    // Note, this does typecheck, because uses of property setters are method calls, and hence member conversions are allowed
    let k = new Binding();
    k.Condition <- fun x -> false
     
    // Dummy code to invoke the Conditions... and see the runtime behavior
    let t1 = k.Condition.Invoke(new Request())
    let t2 = h1.Condition.Invoke(new Request())
    let t3 = h2.Condition.Invoke(new Request())

// Example of post-hoc setting of record fields
module FSharp_1_0_Bug_6389_Example3 = 
    type IRequest = interface end
    type Binding = 
      { Service : System.Type
        mutable Condition : System.Func<IRequest, bool> }
      static member Create() = { Service=null; Condition=null }
 
    type Request()  =
      interface IRequest

    // Explicit coercion fun -> Func: no runtime exception
    let h1 = Binding.Create( Condition = new System.Func<IRequest, bool>((fun x -> true)) );
     
    // No explicit coercion fun -> Func: runtime exception "Invalid cast..."
    let h2 = Binding.Create( Condition = fun x -> true );
     
    // This does NOT type check, by design for F# v2.0, suggestion FSharp 1.0, 6495 for F# vNext. 
    // Assignments to record fields are not method calls, hence no conversions.
    //
    //let k = Binding.Create();
    //k.Condition <- fun x -> false
     
    // Dummy code to invoke the Conditions... and see the runtime behavior
    let t2 = h1.Condition.Invoke(new Request())
    let t3 = h2.Condition.Invoke(new Request())

// Example of post-hoc setting of .NET fields
module FSharp_1_0_Bug_6389_Example4 = 
    open Ninject.Planning.Bindings2

    let h1 = new Binding( Condition = new System.Func<IRequest, bool>((fun x -> true)) );
     
    let h2 = new Binding( Condition = fun x -> true );
     
    // This does NOT type check, by design for F# v2.0, suggestion FSharp 1.0, 6495 for F# vNext. 
    // Assignments to fields are not method calls, hence no conversions.
    //
    //let k = Binding.Create();
    //k.Condition <- fun x -> false
     
    // Dummy code to invoke the Conditions... and see the runtime behavior
    let t2 = h1.Condition.Invoke(new Request())
    let t3 = h2.Condition.Invoke(new Request())

let _ = 
  if !failures then (stdout.WriteLine "Test Failed"; exit 1) 
  else (stdout.WriteLine "Test Passed"; 
        System.IO.File.WriteAllText("test.ok","ok"); 
        exit 0)
