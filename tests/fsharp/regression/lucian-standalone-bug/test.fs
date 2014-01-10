// #Regression 
#indent "off"

module Test

open System
open Utils
open Ast
open States

let check ((e,s):(bool*string)) : unit =  if not e then raise(new System.Exception("FAILURE: "^s))

let test_compare_lists () =
( let c = list_compare compare [1;2;3] [1;2;3] in check (c=0,"compare_lists 1");
  let c = list_compare compare [] [1;2;3] in check (c<0,"compare_lists 2");
  let c = list_compare compare [2;3;1] [1;2;3] in check (c=0, "compare lists 3");
)

let tmain () =
( log ("Tests.\r\n");
  test_compare_lists();
)

// tests: (1) is_progstate_sorted that I write is like checking that fingerprint before sorting and after is identical
// (2) if progstate_compare says 0, then the key-comparison also says 0.
