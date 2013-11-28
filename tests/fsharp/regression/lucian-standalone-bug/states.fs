#indent "off"

module States

open System
open Ast
open Utils
open Microsoft.FSharp.Compatibility.OCaml
open Microsoft.FSharp.Text.Lexing

// SIMULATOR.
// The model is that there are messages floating in queues.
// Each message value has a single well-defined type, but it's value
// may either be a relevant value like VInt(3) or just VAny to stand
// for some arbitrary value of that type. We use VAny because listing
// every possible VInt(i) is impossible, and even listing every possible
// VBool(true)/VBool(false) might get a bit tedious. We perhaps could
// have extended this generalization to types, such as T1+T2, but didn't...
// multiple types just have to be handled with a fork in the trace.
// As for records, we say that they must be fully fleshed out VRecord(...)
// rather than just VAny, for convenience in the "drill_record" function.
// Note: actually, if a state value is a "readwrite" then we instead
// store the two values as VInts. That's because if we abstracted to VAny
// then we'd lose too much precision for the kinds of analysis we want to do.
//
//
// We maintain these queues:
//
// 1. For every notify which is referenced through an "on Notify(T arg)" handler,
// we keep a queue. Stored like this: ((DstActivity,Handler,HandlerType),(qsize,q))
// NB. that SrcActivity = HandlerName, and HandlerType is a unique type out of the
// notify-types of SrcActivity.
// We also keep a list of (SrcActivity,TypeList) for all those names mentioned in
// the program but where SrcActivity was declared extern; these will
// be used by the model-checker for synthesising new messages.
//
// 2. For every wire in a running instance, we keep a queue. Indexed
// by a lot of things: ( (activity,handler,src,dest,instance), (qsize,q))
// This and the previous data structures are defined with an eye to the
// algorithm for exploring statespaces, i.e. exploring which moves are possible.
//
// Possible moves:
//
// 1. synthesise one of those (SrcActivity,Typelist) messages and place it
// in all the queues waiting for it.
//
// 2. consume a message from a main queue (notify/request). This can happen for any
// of the queues which have at least one message in them. The procedure is to take
// the message, spawn off a new instance of the handler, and post the initial
// message on all wires within that instance that need it. (note: an activity
// cannot have more than one handler for a notify/type pair.)
//
// 3. fire a node. This can happen for any instance/node for which enough of its
// trigger-wires have messages on them. The procedure is to iterate through all
// nodes in the program. For each one, look up the queues on its wires and make
// a decision whether or not it can fire. If it can fire, well, we have to execute
// the respective node. The case of call-nodes is special. This node looks to see
// if its data is ready. If so, it spawns an instance of the handler. The instance
// keeps a record of where to return to.
//
// 4. Also, for an external request/response handler instance that has been invoked,
// we can return any acceptable value from it. (this is implemented within the
// routine that generates node-moves (3)).
//
// Each move is marked as to whether it "isInternal" or not. The definition of an
// internal move is that doing it now or delaying it till later will have no effect
// on the overall behavior of the program. Algorithmically, once we've generated
// a list of possible moves, we check: if there are any "isInternal" moves then
// we deterministically say that the next move WILL be the first isInternal move
// possible. This reduces the statespace.
//
// Note: the analysis goal will constrain which moves we seriously consider.
// For coverage analysis (to detect whether any states fail to be reached),
// we assume that coverage is not dependant upon subtle interleaving of handlers;
// therefore we run instances sequentially. Ultimately we want to reach coverage
// of every possible message being delivered to every reachable state.
// For race-condition analysis (to detect whether there are problems arising from
// interleaving), we of course need to have interleaves. But we'll only look at
// pairwise interleavings, i.e. every possible pair of handlers running together.
// 

type VPrim = VAny | VVoid | VInt of int | VString of string | VBool of bool | VEnum of string | VRecord of (VPrim*TName) list // each field records its type
type StateVersion = {a:string; s:string; seq:int; n:Node option}  // activity.state : sequence-number. The node says at which node the state was set
and  Value = {t:TName; i:Importance; v:VPrim; dep:Dependencies} // r is the return address. "dep" records what this value has depended on so far
and  StateValue = {s:StateVersion; v:VPrim; i:Importance; t:TName}
and  Dependencies = StateVersion list
// nb. VAny cannot be used for a VRecord. It can be used for everything else, though.
//
// in the following queues, "head" of the list is where we remove elements; "tail" is where we add them.
type MainPort = {a:string; h:string; ht:Type; nq:NQueue} // unique identifies a mainport. The nq(queuesize) is for when we use this structure to add to a queue, so we know how big that queue is
type WirePort = {i:int; src:string; dst:string; nq:NQueue} // uniquely identifies a instance+wire
let  NQNotForAdding = NBounded(-1) // what we put in nq field of MainPort/WirePort if we're using them to identify messages to remove, not to add
let  NoReturn = {new WirePort with i=(-1) and src="" and dst="" and nq=NQNotForAdding } // means that there must be no return statement to this
let  IgnoreReturn = {new WirePort with i=(-2) and src="" and dst="" and nq=NQNotForAdding } // means that even if there is a return, we'll just ignore it
type MainQueue = {a:string; h:string; ht:Type; q:(Value*WirePort)list} // a queue leading into a notify-port or request-port. The WirePort is a return-address.
type WireQueue = {src:string; dst:string; q:Value list} // a queue leading into a node in the diagram
type Instance = {a:string; h:string; ht:Type; q:WireQueue list; r:WirePort; count:int} // q is sorted. r is the return-address (if we're spun-up to handle a reqest/response call). If any wirequeue is empty then it should not be listed here. Count says how many "calls" are outstanding.
type ProgState = {mq:MainQueue list; s:StateValue list; i:(int*Instance)list} // a program state. mq, s and i are sorted. If a queue is empty then it should not be listed here.
//
type MSynthesizeDat = {a:string; t:TName; v:Value; isInternal:bool} // isInternal says that the move is purely internal, and doing it now or later is completely immaterial
type MConsumeDat = {mp:MainPort; r:WirePort; isInternal:bool} // r is the return-address
type MFireNodeDat = {instance:int; node:string; src:string list; index:int; isInternal:bool} // src says which wires to take messages out of. index is for "if/iftype" branches to say which one is taken, and for "call" nodes to say (-1) to act on it or "i" for the ith possible output type of the extern req/resp handler
type Move = MSynthesize of MSynthesizeDat | MConsume of MConsumeDat | MFireNode of MFireNodeDat
//
type Environment = (string*Value) list 
//
type ProfileEvent = PStateSet of StateValue*StateValue*Dependencies | PStateGet of StateValue | PExploreEnd of ProgState
       | PCall of ProgState*string*string*Type*Node | PAtomic of string*string*AtomicMode*int*int*Node
       | PExplore of ProgState

let rec vprim_to_string (v:VPrim) = match v with
  | VAny -> "?"
  | VVoid -> "void"
  | VInt(i) -> string(i)
  | VString(s) -> "\""^s^"\""
  | VBool(b) -> if b then "true" else "false"
  | VEnum(s) -> s
  | VRecord(vs) -> "("^(list_to_string "," (List.map (fun (v,t)->(vprim_to_string v)) vs))^")" //^":"^t) vs)) ^ ")"
and vprimt_to_string ((v,t):(VPrim*TName)) = (vprim_to_string v)^":"^t
and stateversion_to_string (sv:StateVersion) = sv.a^"."^sv.s^"#"^string(sv.seq)^" "^(somenode_to_string sv.n)
and somenode_to_string (n:Node option) = match n with None->"start" | Some n -> "line"^string(n.src)
and value_to_string (v:Value) = (if v.i=Important then "important " else "")^(vprim_to_string v.v)^":"^v.t^(dep_to_string v.dep)
and values_to_string (vs:Value list) = list_to_string "," (List.map value_to_string vs)
and dep_to_string (dep:Dependencies) = let dep = list_to_string "," (List.map stateversion_to_string dep) in
  if dep="" then "" else "dep.("^dep^")"
and statevalue_to_string (sv:StateValue) = (stateversion_to_string sv.s)^" = "^(if sv.i=Important then "important " else "")^(vprim_to_string sv.v) //^":"^sv.t
and profileevent_to_string p = match p with
  | PStateGet(sv) -> "GET "^sv.s.a^"."^sv.s.s^": "^(vprim_to_string sv.v)^"#"^string(sv.s.seq)
  | PStateSet(svold,svnew,dep) -> "SET "^svold.s.a^"."^svold.s.s^": "^(vprim_to_string svold.v)^"#"^string(svold.s.seq)^" -> "^(vprim_to_string svnew.v)^"#"^string(svnew.s.seq)^" "^(dep_to_string dep)
  | PExploreEnd(ps) -> "EXPLORE-END"
  | PCall(ps,peer,verb,t,n) -> "CALL "^peer^"."^verb^":"^(type_to_string t)
  | PAtomic(a,s,mode,rcount,wcount,n) -> "ATOMIC "^(atomic_to_string mode)^" "^a^"."^s^"(rcount="^string(rcount)^",wcount="^string(wcount)^")"
  | PExplore(ps) -> "EXPLORE"
and coverage_to_string (cover:(string*VPrim)list) = list_to_string ", " (List.map cover_to_string cover)
and cover_to_string ((s,v):(string*VPrim)) = s^"="^(vprim_to_string v)
//
let rec mainport_to_string (mp:MainPort) = mp.a^"."^mp.h^":"^(type_to_string mp.ht)
and wireport_to_string (wp:WirePort) = if wp=NoReturn then "noret" else if wp=IgnoreReturn then "ignoreret" else "i"^string(wp.i)^"."^wp.src^"->"^wp.dst
and wireports_to_string (wps:WirePort list) = list_to_string ", " (List.map wireport_to_string wps)
and mainqueue_to_string (mq:MainQueue) = mq.a^"."^mq.h^":"^(type_to_string mq.ht)^".{"^(mainqueue_contents_to_string mq.q)^"}"
and mainqueue_contents_to_string (q:(Value*WirePort)list) =
  let s = List.map (fun (v,w)->(value_to_string v)^"; r."^(wireport_to_string w)) q in
  list_to_string "," s
and wirequeue_to_string (wq:WireQueue) = "   "^wq.src^"->"^wq.dst^".{"^(values_to_string wq.q)^"}"
and instance_to_string (i:Instance) =
  let wires = (list_to_string "\n" (List.map wirequeue_to_string i.q)) in
  let wires = (if wires="" then "" else wires^"\n") in
  let count = (if i.count=0 then "" else "count:"^string(i.count)^" ") in
  let ret = (if i.r=NoReturn || i.r=IgnoreReturn then "" else "return-to:"^(wireport_to_string i.r)^" ") in
  i.a^"."^i.h^":"^(type_to_string i.ht)^"{"^count^ret^"\n"^wires^"}\n"
and progstate_to_string (ps:ProgState) =
  let ss = list_to_string "\n" (List.map statevalue_to_string ps.s) in let ss = (if ss="" then "" else ss^"\n\n") in
  let mqs = list_to_string "\n" (List.map mainqueue_to_string ps.mq) in let mqs = (if mqs="" then "" else mqs^"\n\n") in
  let is = list_to_string "" (List.map (fun (i,inst)->"i"^string(i)^" = "^(instance_to_string inst)) ps.i) in
  "--PROGSTATE--\n"^mqs^ss^is^"\n"
//
let rec move_to_string (m:Move) = match m with
  | MSynthesize(md) -> "msynth("^md.a^":"^md.t^" <- "^(value_to_string md.v)^")"
  | MConsume(md) -> "mconsume("^(mainport_to_string md.mp)^" r."^(wireport_to_string md.r)^")"
  | MFireNode(md) -> "mnode(i"^string(md.instance)^" node "^md.node^" #"^string(md.index)^")"
and moves_to_string (ms:Move list) = list_to_string ", " (List.map move_to_string ms)
and environment_to_string (env:Environment) =
  let es = List.map (fun (s,v)->s^"="^(value_to_string v)) env in
  "[" ^ (list_to_string "," es) ^ "]"



// Some ordering functions. These *_compare_keys functions are used for
// ordering the various lists into a canonical order. They do it by
// comparing just the keys. So, once a state has been sorted into canonical
// order, then any two states which are structurally congruent will
// also be syntactically identical. NOTE: That's not quite true.
// To make it so would require "moveable pointers", e.g. for when one progstate
// has an instance called "i2" and the other has an instance called "i3"
// though they're both fundamentally the same. I don't want to do this.
// In any case, in the context in which the canonical is being used (to
// generate fingerprints for states) it doesn't matter, because the
// explorations we use that do this are guaranteed to only ever have
// the same two instances throughout.
let wirequeue_compare_keys (wq1:WireQueue) (wq2:WireQueue) : int = compare_compose [Operators.compare wq1.src wq2.src; Operators.compare wq1.dst wq2.dst]
let mainqueue_compare_keys (mq1:MainQueue) (mq2:MainQueue) : int =  compare_compose [Operators.compare mq1.a mq2.a; Operators.compare mq1.h mq2.h; type_compare_unimp mq1.ht mq2.ht]
let statevalue_compare_keys (sv1:StateValue) (sv2:StateValue) : int = compare_compose [Operators.compare sv1.s.a sv2.s.a; Operators.compare sv1.s.s sv2.s.s]
let instance_compare_keys ((i1,inst1):(int*Instance)) ((i2,inst2):(int*Instance)) : int = compare i1 i2
// The above functions were all for sorting the various lists into canonical order.
// Here's one slightly-different function which checks that the entire progstate
// has been sorted into canonical order throughout (including the wirequeues inside
// each instance).
let is_progstate_sorted (ps:ProgState) : bool =
  (is_list_sorted_uniq mainqueue_compare_keys ps.mq) 
  && (is_list_sorted_uniq statevalue_compare_keys ps.s)
  && (is_list_sorted_uniq instance_compare_keys ps.i)
  && not(List.exists (fun (i,inst)->not (is_list_sorted_uniq wirequeue_compare_keys inst.q)) ps.i)
//
let assert_sorted_progstate (ps:ProgState) : unit =  if not (is_progstate_sorted ps) then
( System.Console.Out.WriteLine("\n\n"^(progstate_to_string ps)^"\n");
  raise(new System.Exception("INTERROR: progstate is not sorted"));
)


// Here are functions which test for actual full equality, leading up to an equality test on states
// Before submitting things to these tests, you should sort themself, so the syntactic structural
// comparison we do here ends up amounting to one quotiented by structural congruence.
let rec vprim_compare (v1:VPrim) (v2:VPrim) : int = match (v1,v2) with
  | (VAny,VAny) -> 0 | (VAny,_) -> -1 | (_,VAny) -> 1
  | (VVoid,VVoid) -> 0 | (VVoid,_) -> -1 | (_,VVoid) -> 1
  | (VInt(i1),VInt(i2)) -> compare i1 i2 | (VInt(_),_) -> -1 | (_,VInt(_)) -> 1
  | (VString(s1),VString(s2)) -> Operators.compare s1 s2 | (VString _,_) -> -1 | (_,VString _) -> 1
  | (VBool(b1),VBool(b2)) -> if b1=b2 then 0 else if b2 then 1 else -1 | (VBool _,_) -> -1 | (_,VBool _) -> 1
  | (VEnum(e1),VEnum(e2)) -> Operators.compare e1 e2 | (VEnum _,_) -> -1 | (_,VEnum _) -> 1
  | (VRecord(fs1),VRecord(fs2)) -> list_compare vprim_record_compare fs1 fs2
and vprim_record_compare (v1,t1) (v2,t2) = compare_compose [vprim_compare v1 v2; Operators.compare t1 t2]
and stateversion_compare (sv1:StateVersion) (sv2:StateVersion) : int = compare_compose [Operators.compare sv1.a sv2.a; Operators.compare sv1.s sv2.s; compare sv1.seq sv2.seq; nodeoption_compare sv1.n sv2.n]
and nodeoption_compare (n1:Node option) (n2:Node option) : int = match (n1,n2) with
  | (None,None) -> 0 | (None,_) -> -1 | (_,None) -> 1
  | (Some n1,Some n2) -> compare_compose [Operators.compare n1.s n2.s; compare n1.src n2.src]
and value_compare (v1:Value) (v2:Value) : int = compare_compose [Operators.compare v1.t v2.t; importance_compare v1.i v2.i; vprim_compare v1.v v2.v; dependencies_compare v1.dep v2.dep]
and importance_compare (i1:Importance) (i2:Importance) = match (i1,i2) with
  | (Unimportant,Unimportant)->0 | (Unimportant,_) -> -1 | (_,Unimportant) -> 1 | (Important,Important) -> 0
and dependencies_compare (dep1:Dependencies) (dep2:Dependencies) = list_compare stateversion_compare dep1 dep2
and statevalue_compare (sv1:StateValue) (sv2:StateValue) : int = compare_compose [stateversion_compare sv1.s sv2.s; vprim_compare sv1.v sv2.v; importance_compare sv1.i sv2.i; Operators.compare sv1.t sv2.t]
and mainport_compare (mp1:MainPort) (mp2:MainPort) : int = compare_compose [Operators.compare mp1.a mp2.a; Operators.compare mp1.h mp2.a; type_compare_imp mp1.ht mp2.ht; queue_compare mp1.nq mp2.nq]
and wireport_compare (wp1:WirePort) (wp2:WirePort) : int = compare_compose [compare wp1.i wp2.i; Operators.compare wp1.src wp2.src; Operators.compare wp1.dst wp2.dst; queue_compare wp1.nq wp2.nq]
and mainqueue_compare (mq1:MainQueue) (mq2:MainQueue) : int = compare_compose [Operators.compare mq1.a mq2.a; Operators.compare mq1.h mq2.h; type_compare_imp mq1.ht mq2.ht; list_compare mainqueue_compare_content mq1.q mq2.q]
and mainqueue_compare_content ((v1,wp1):(Value*WirePort)) ((v2,wp2):(Value*WirePort)) : int = compare_compose [value_compare v1 v2; wireport_compare wp1 wp2]
and wirequeue_compare (wq1:WireQueue) (wq2:WireQueue) : int = compare_compose [Operators.compare wq1.src wq2.src; Operators.compare wq1.dst wq2.dst; list_compare value_compare wq1.q wq2.q]
and instance_compare (i1:Instance) (i2:Instance) : int = compare_compose [Operators.compare i1.a i2.a; Operators.compare i1.h i2.h; type_compare_imp i1.ht i2.ht; list_compare wirequeue_compare i1.q i2.q; wireport_compare i1.r i2.r; compare i1.count i2.count]
and iinstance_compare ((i1,inst1):(int*Instance)) ((i2,inst2):(int*Instance)) : int = compare_compose [compare i1 i2; instance_compare inst1 inst2]
and progstate_compare (ps1:ProgState) (ps2:ProgState) : int = compare_compose [list_compare mainqueue_compare ps1.mq ps2.mq; list_compare statevalue_compare ps1.s ps2.s; list_compare iinstance_compare ps1.i ps2.i]










// genericvalue_of_type: constructs "Any". Or, if we're asked for a record,
// constructs record(Any,Any,Any...).
let rec genericvalue_of_type (p:Prog) ((t,i):Type) : Value =
  // first a subfunction which constructs a VPrim
  let rec vprim_of_type (t:TName) : VPrim =
  ( let prim = List.assoc t p.tp in
    match prim with
    | TRecord(fs) -> let fs = List.map (fun ((ff,ft):TField)->(vprim_of_type ft, ft)) fs in VRecord(fs)
    | _ -> VAny
  )
  in
  {new Value with t=t and i=i and v=vprim_of_type t and dep=[]}


// allvprims_of_type: given a bool named n, constructs [n=true,n=false]. Given an enum,
// constructs [n=enum1,n=enum2,...]. Given a string/int, constructs [n=any]. Given
// a record, constructs [n.0=A, n.0=B, n.1=X, n.1=Y, n.1=Z].
let rec allvprims_of_type (p:Prog) (t:TName) (s:string) : ((string*VPrim)list) =
( let tprim = List.assoc t p.tp in
  match tprim with
  | TVoid -> [(s,VVoid)]
  | TBool -> [(s,VBool(true)); (s,VBool(false))]
  | TInt -> [(s,VAny)]
  | TString -> [(s,VAny)]
  | TEnum(es) -> List.map (fun e -> (s,VEnum(e))) es
  | TRecord(fs) -> List.concat (List.mapi (fun i (f,ft) -> allvprims_of_type p ft (s^"."^string(i))) fs)
)

// remove_assignment_from_allvprims: given a list of things that we have to hit
// [s.track=true, s.track=false, s.scan=true, s.scan=false], and given a
// value id=v:t which has just been assigned, we remove from the "to-hit" list
// anything that it did indeed hit.
let rec remove_assignment_from_allvprims (p:Prog) ((id,v):(string*VPrim)) (sv:(string*VPrim)list) : ((string*VPrim)list) =
( System.Console.Out.WriteLine ("cover: "^id^" = "^(vprim_to_string v)^"\n");
  match v with
  | VAny -> List.choose (fun (s,v)->if s=id then None else Some(s,v)) sv
  | VVoid -> List.choose (fun (s,v)->if s=id && (v=VVoid || v=VAny) then None else Some(s,v)) sv
  | VInt(i) -> List.choose (fun (s,v)->if s=id && (v=VInt(i) || v=VAny) then None else Some(s,v)) sv
  | VString(s) -> List.choose (fun (s,v)->if s=id && (v=VString(s) || v=VAny) then None else Some(s,v)) sv
  | VBool(b) -> List.choose (fun (s,v)->if s=id && (v=VBool(b) || v=VAny) then None else Some(s,v)) sv
  | VEnum(e) -> List.choose (fun (s,v)->if s=id && (v=VEnum(e) || v=VAny) then None else Some(s,v)) sv
  | VRecord(fs) ->
      ( let sv = ref sv in
        List.iteri (fun i (v,t) -> sv := remove_assignment_from_allvprims p (id^"."^string(i),v) (!sv)) fs;
        !sv
      )
)



let rec vop_not (v:VPrim) = match v with VAny -> VAny | VBool(b) -> VBool(not b) | _ -> raise(new System.Exception("vop_not("^(vprim_to_string v)^")"))
and vop_plus v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VInt(i1+i2) | _ -> raise(new System.Exception("vop_plus("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_minus v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VInt(i1-i2) | _ -> raise(new System.Exception("vop_minus("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_times v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VInt(i1*i2) | _ -> raise(new System.Exception("vop_times("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_divide v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VInt(i1/i2) | _ -> raise(new System.Exception("vop_divide("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_below v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VBool(i1<i2) | (VAny,VString s) -> VAny | (VString s,VAny) -> VAny | (VString s1,VString s2) -> VBool(String.Compare(s1,s2)<0) | _ -> raise(new System.Exception("vop_below("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_above v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VBool(i1>i2) | (VAny,VString s) -> VAny | (VString s,VAny) -> VAny | (VString s1,VString s2) -> VBool(String.Compare(s1,s2)>0) | _ -> raise(new System.Exception("vop_above("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_leq v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VBool(i1<=i2) | (VAny,VString s) -> VAny | (VString s,VAny) -> VAny | (VString s1,VString s2) -> VBool(String.Compare(s1,s2)<=0) | _ -> raise(new System.Exception("vop_leq("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_geq v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VInt i) -> VAny | (VInt i,VAny) -> VAny | (VInt i1,VInt i2) -> VBool(i1>=i2) | (VAny,VString s) -> VAny | (VString s,VAny) -> VAny | (VString s1,VString s2) -> VBool(String.Compare(s1,s2)>=0) | _ -> raise(new System.Exception("vop_geq("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_and v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VBool b)->VAny | (VBool b,VAny) -> VAny | (VBool b1,VBool b2)->VBool(b1&&b2) | _ -> raise(new System.Exception("vop_and("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_or v1 v2 = match (v1,v2) with (VAny,VAny)->VAny | (VAny,VBool b)->VAny | (VBool b,VAny) -> VAny | (VBool b1,VBool b2)->VBool(b1||b2) | _ -> raise(new System.Exception("vop_or("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
and vop_neq v1 v2 = vop_not(vop_eq v1 v2)
and vop_eq v1 v2 = match (v1,v2) with | (VAny,_)->VAny | (_,VAny)->VAny
  | (VVoid,VVoid)->VBool(true) | (VInt i1,VInt i2)->VBool(i1=i2) | (VString s1,VString s2)->VBool(s1=s2)
  | (VBool b1,VBool b2)->VBool(b1=b2) | (VEnum e1,VEnum e2)->VBool(e1=e2)
  | (VRecord vs1,VRecord vs2) -> let eqs = List.map2 (fun (v1,t1) (v2,t2) -> [vop_eq v1 v2;VBool(t1=t2)]) vs1 vs2 in 
         let eqs = List.concat eqs in List.foldBack vop_eq eqs (VBool(true))
  | _ -> raise(new System.Exception("vop_eq("^(vprim_to_string v1)^","^(vprim_to_string v2)^")"))
        

let seq_oldest (seq1:int) (seq2:int) = if seq1<seq2 then seq1 else seq2
let stateversion_compare_noseq (sv1:StateVersion) (sv2:StateVersion) = compare_compose [Operators.compare sv1.a sv2.a; Operators.compare sv1.s sv2.s]

let dep_union (dep:Dependencies) : Dependencies = 
  let rec depmerge_seq (dep:Dependencies) = match dep with [] -> []
      | sv1::[] -> [sv1]
      | sv1::sv2::dep -> if sv1.a=sv2.a && sv1.s=sv2.s then depmerge_seq ({sv2 with seq=seq_oldest sv1.seq sv2.seq} :: dep)
                         else sv1 :: (depmerge_seq (sv2::dep)) in
  depmerge_seq (List.sortWith stateversion_compare_noseq dep)


let rec drill_record_getvprim (vt:(VPrim*TName)) (idp:IdPath) : (VPrim*TName) = match (vt,idp) with
  | (_,[]) -> vt
  | ((VRecord(vs),t),i::idp) -> let i=int32(i) in drill_record_getvprim (List.nth vs i) idp
  | (vt,idp) -> raise(new System.Exception("RUNERROR: val "^(vprimt_to_string vt)^" not a record"))
//
and drill_record_setvprim (vt:(VPrim*TName)) (idp:IdPath) (setvt:(VPrim*TName)) : (VPrim*TName) = match (vt,idp) with
  | (_,[]) -> setvt
  | ((VRecord(vs),t),i::idp) -> let i=int32(i) in 
      let vs = Array.ofList(vs) in
      vs.[i] <- drill_record_setvprim vs.[i] idp setvt;
      (VRecord(List.ofArray(vs)),t)
  | (vt,idp) -> raise(new System.Exception("RUNERROR: val "^(vprimt_to_string vt)^" not a record"))



// queuelist_contains_msg: says whether there exists a message on this port.
// Note that the "WireQueue list" data structure within Instance is defined
// so that each WireQueue in the list has a non-empty queue. That makes
// our test easy...
let queuelist_contains_msg (wqs:WireQueue list) (wp:WirePort) =
  List.exists (fun (wq:WireQueue) -> wq.src=wp.src && wq.dst=wp.dst) wqs

// splitoff: if there is an element in the list which matches a,h,ht, then split it off and
// return it separately
let rec splitoff_mainports_element ((a,h,ht):(string*string*Type)) (mps:MainPort list) : (MainPort list * (MainPort option)) =
  match mps with [] -> ([],None)
  | (mp::mps) -> if (mp.a=a && mp.h=h && mp.ht=ht) then (mps,Some mp)
                 else let (a,r)=splitoff_mainports_element (a,h,ht) mps in (mp::a, r)

let rec splitoff_wireports_element ((i,src,dst):(int*string*string)) (wps:WirePort list) : (WirePort list * (WirePort option)) =
  match wps with [] -> ([],None)
  | (wp::wps) -> if (wp.i=i && wp.src=src && wp.dst=dst) then (wps,Some wp)
                 else let (a,r)=splitoff_wireports_element (i,src,dst) wps in (wp::a, r)

let append_queue (q:'a list) (nq:NQueue) (v:'a) = match nq with
  | NUnbounded -> q@[v]
  | NBounded(n) -> if n=0 then [] else if List.length(q)>=n then (List.tail q)@[v] else q@[v]



// queue_remove_main: removes the head of the queue identified by "mp"
// and returns this value/returnport, plus the new queue with that bit removed.
let queue_remove_main (mp:MainPort) (mqs:MainQueue list) : (MainQueue list * (Value*WirePort)) = 
  let (vr:(Value*WirePort) option ref) = ref None in // here we'll keep the answer that we've found
  //
  let inner_loop (mq:MainQueue) : (MainQueue option) = if (!vr)<>None || mq.a<>mp.a || mq.h<>mp.h || mq.ht<>mp.ht then Some mq else
  ( vr := Some (List.head mq.q); let q = List.tail mq.q in
    if List.length(q)=0 then None
    else Some {new MainQueue with a=mq.a and h=mq.h and ht=mq.ht and q=q}
  ) in
  let mqs = List.choose inner_loop mqs in // runs the inner-loop on each of them
  match (!vr) with Some vr -> (mqs,vr) | None -> raise(new System.Exception("No queue found"))


// queue_add_main: adds the element onto the tail of the queues identified in "mps",
// creating the queue if it's not there already. Returns the new queueset.
// For the implementation, we keep a mutable copy of "mps" and remove elements from it
// as we find them in existing queues. Anything left in mps means there was no
// preexisting queue for those mainports so we have to create them.
let queue_add_main (mps:MainPort list) (vr:(Value*WirePort)) (mqs:MainQueue list) : (MainQueue list) =
  let mps = ref mps in
  let inner_loop (mq:MainQueue) : MainQueue =
  ( let (mps2,thismp) = splitoff_mainports_element (mq.a,mq.h,mq.ht) (!mps) in 
    match thismp with None -> mq | Some thismp ->
    ( mps := mps2;
      {new MainQueue with a=mq.a and h=mq.h and ht=mq.ht and q=(append_queue mq.q thismp.nq vr)}
    )
  ) in
  let mqs = List.map inner_loop mqs in // will add to various of the queues; "mps" will have some elements removed from it.
  let nqs = List.map (fun (mp:MainPort)->{new MainQueue with a=mp.a and h=mp.h and ht=mp.ht and q=[vr]})(!mps) in // build fresh queues for the ones where we couldn't just add to a pre-existing one
  List.sortWith mainqueue_compare_keys (mqs @ nqs)
  
// queue_broadcast_main: adds the element onto the tail of all queues which handle
// messages of this type from this activity
let queue_broadcast_main (p:Prog) (srcactivity:string) (t:Type) (mqs:MainQueue list) (v:Value) : (MainQueue list) =
  let rec inner_loop_a (a:Activity) : (MainPort list) = List.choose (inner_loop_h a) a.h
  and inner_loop_h (a:Activity) ((sh,h):(string*Handler)) : (MainPort option) =
    if sh=srcactivity && t=h.at then Some{new MainPort with a=a.s and h=sh and ht=h.at and nq=h.aq} else None
  in
  let mps = List.concat (List.map inner_loop_a p.a) in
  queue_add_main mps (v,NoReturn) mqs



// queue_remove: removes the head of the queue identified by "wp" and returns
// this value, plus the new queue with that bit removed.
// Note: must call purge-instances afterwards to clean up
let queue_remove (wp:WirePort) (instances:(int*Instance)list) : ((int*Instance)list * Value) =
  let (vr:Value option ref) = ref None in // here we'll keep the answer that we've found
  let rec instance_loop ((i,instance):(int*Instance)) : (int*Instance) =
    if (!vr)<>None || wp.i<>i then (i,instance)
    else (i,{instance with q=List.choose (queue_loop i) instance.q}) 
  and queue_loop (i:int) (wq:WireQueue) : (WireQueue option) =
    if wp.src<>wq.src || wp.dst<>wq.dst || (!vr)<>None then Some(wq)
    else
    ( vr := Some(List.head wq.q);
      if List.length(wq.q)=1 then None
      else Some {new WireQueue with src=wq.src and dst=wq.dst and q=List.tail wq.q}
    )
  in
  let instances = List.map instance_loop instances in
  match (!vr) with Some vr -> (instances,vr) | None -> raise(new System.Exception("No queue found to remove from "^(wireport_to_string wp)^"."))

// queue_add: adds the element onto the tail of all queues in the "wps" list.
// Similar to queue_add_main, above.
let queue_add (wps:WirePort list) (v:Value) (instances:(int*Instance)list) : ((int*Instance)list) =
  // in the following inner loop, "wps" is a ref list, and the function removes entries from it as they're added
  let rec instance_loop (wps:WirePort list ref) ((i,instance):(int*Instance)) : (int*Instance) =
    let wpis = ref (List.filter (fun (wp:WirePort)->wp.i=i) (!wps)) in // get the ones just for this instance
    wps := List.filter (fun (wp:WirePort)-> wp.i<>i) (!wps); // and update the main list to remove the rest
    let q = List.map (queue_loop wpis i) instance.q in // this adds things from wpi into queues that the instance already had
    let newq = List.map (fun (wp:WirePort) -> {new WireQueue with src=wp.src and dst=wp.dst and q=[v]}) (!wpis) in // make new queues for stuff that couldn't be added
    (i,{instance with q=List.sortWith wirequeue_compare_keys (q@newq)})
  // once again, "wpis" is a ref list, and we remove from it each time we can add to a queue
  and queue_loop (wpis:WirePort list ref) (i:int) (wq:WireQueue) : (WireQueue) =
  ( let (wpis2,thiswp) = splitoff_wireports_element (i,wq.src,wq.dst) (!wpis) in
    match thiswp with None -> wq | Some thiswp ->
    ( wpis := wpis2;
      {new WireQueue with src=wq.src and dst=wq.dst and q=(append_queue wq.q thiswp.nq v)}
    )
  ) in
  //
  let wps = ref wps in 
  let instances = List.map (instance_loop wps) instances in
  if List.length(!wps)>0 then raise(new System.Exception("RUNERROR: trying to add a message in a non-existant instance"));
  instances  


// modify_instance_callcount: alters the "count" field of the specified instance.
// Note: must call purge-instances afterwards, to clean up any empty instances which have no calls outstanding
// perhaps removing the instance if there are no messages it it and no calls outstanding
let modify_instance_callcount (changei:int) (delta:int) (instances:(int*Instance)list) : ((int*Instance)list) =
  let instance_loop ((i,instance):(int*Instance)) : (int*Instance) =
    if i=changei then (i,{instance with count=instance.count+delta}) else (i,instance)
  in
  List.map instance_loop instances



let rec Evaluate (ef:ExternFun list) (env:Environment) (basedep:Dependencies) (basei:Importance) (e:Expr) : Value = match e.e with
  | EVoid -> {new Value with v=VVoid and t="void" and i=basei and dep=basedep}
  | EString(s) -> {new Value with v=VString(s) and t="string" and i=basei and dep=basedep}
  | EInt(i) -> {new Value with v=VInt(i) and t="int" and i=basei and dep=basedep}
  | EBool(b) -> {new Value with v=VBool(b) and t="bool" and i=basei and dep=basedep}
  | EEnum(t,s) -> {new Value with v=VEnum(s) and t=t and i=basei and dep=basedep}
  | EImportance(i,e) -> let v = Evaluate ef env basedep basei e in
        {new Value with t=v.t and i=i and v=v.v and dep=v.dep}
  | EIdPath(idp) ->
        let envvar = List.assoc (List.head idp) env in
        let (v,t) = drill_record_getvprim (envvar.v,envvar.t) (List.tail idp) in
        {new Value with v=v and t=t and i=envvar.i and dep=envvar.dep}
  | EConstructor(tn,fes) ->
        let vs = List.map (fun (f,e)->Evaluate ef env basedep basei e) fes in // get all the values
        let i = imp_max (List.map (fun (v:Value) -> v.i) vs) in // maximum importance of all of them
        let dep = dep_union (List.concat (List.map (fun v -> v.dep) vs)) in // all the dependencies
        let vs = List.map (fun (v:Value) -> (v.v,v.t)) vs in // get just VPrim*TName fields
        {new Value with v=VRecord(vs) and t=tn and i=i and dep=dep}
  | EExternal(f,es) ->
        let vs = List.map (Evaluate ef env basedep basei) es in // evaluate all the arguments to values
        let i = imp_max (List.map (fun (v:Value) -> v.i) vs) in // maximum importance of them all
        let dep = dep_union (List.concat (List.map (fun v -> v.dep) vs)) in // all the dependencies
        let ef = List.assoc f ef in
        {new Value with v=VAny and t=ef.rt and i=i and dep=dep}
  | EFun(f,es) ->
        let vs = List.map (Evaluate ef env basedep basei) es in // evaluate all arguments to values
        let i = imp_max (List.map (fun (v:Value) -> v.i) vs) in // maximum importance of them all
        let dep = dep_union (List.concat (List.map (fun v -> v.dep) vs)) in // all the dependencies
        if f="!" then
        ( if List.length(vs)<>1 then raise(new System.Exception("RUNERROR: function "^f^" given wrong number of arguments"));
          let v1 = List.head vs in
          {new Value with v=vop_not(v1.v) and t="bool" and i=i and dep=dep}
        )
        else // the rest of the builtins are binary
        ( if List.length(vs)<>2 then raise(new System.Exception("RUNERROR: function "^f^" given wrong number of arguments"));
          let (v1,v2) = (List.head vs, List.head (List.tail vs)) in
          if (f="+") then {new Value with v=vop_plus v1.v v2.v and t="int" and i=i and dep=dep}
          else if (f="-") then {new Value with v=vop_minus v1.v v2.v and t="int" and i=i and dep=dep}
          else if (f="*") then {new Value with v=vop_times v1.v v2.v and t="int" and i=i and dep=dep}
          else if (f="/") then {new Value with v=vop_divide v1.v v2.v and t="int" and i=i and dep=dep}
          else if (f="<") then {new Value with v=vop_below v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f=">") then {new Value with v=vop_above v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f="<=") then {new Value with v=vop_leq v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f=">=") then {new Value with v=vop_geq v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f="==") then {new Value with v=vop_eq v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f="!=") then {new Value with v=vop_neq v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f="&&") then {new Value with v=vop_and v1.v v2.v and t="bool" and i=i and dep=dep}
          else if (f="||") then {new Value with v=vop_or v1.v v2.v and t="bool" and i=i and dep=dep}
          else raise(new System.Exception("RUNERROR: function "^f^" not recognized"));
        )



// InitialState: all it does is set up the state variables for all services
// nb. the flag "useany" means to use VAny rather than the concrete initializer.
// But if the state variable is a "readwrite" then we will indeed use the concrete
// initializer because VAny would lead to a too-imprecise analysis
let MakeInitialState (p:Prog) (useany:bool) : ProgState =
  let (state:StateValue list ref) = ref [] in // we'll accumulate the results here
  //
  let rec mis_activity (a:Activity) = List.iter (mis_state a) a.state
  and mis_state (a:Activity) ((ss,se):State) =
  ( let v = if (not useany || tl_contains_unimp se.t ("readwrite",Unimportant)) then Evaluate p.ef [] [] Unimportant se
    else genericvalue_of_type p (List.head se.t) in // guaranteed from inferTypes that se.t is a unique type
    let sv = {new StateValue with v=v.v and t=v.t and i=v.i and s={new StateVersion with a=a.s and s=ss and seq=0 and n=None} } in
    state := sv :: (!state)
  ) in
  //
  List.iter mis_activity p.a;
  {new ProgState with mq=[] and s=List.sortWith statevalue_compare_keys (!state) and i=[]}


// GenerateMoves:
type GenerateMovesMode = GMFull | GMRemoveInessentialNondeterminism | GMRemoveOrderingNondeterminism | GMRemoveAllNondeterminism | GMToTestForPurging
// GMFull generates every possible interleaved mode
// GMRemoveInessentialNondetermism does "transacted-style" things, i.e. it does the first available purely-internal move. Computed in "GenerateMoves" function.
// GMRemoveOrderingNondeterminsm uses a determinstic order for interleaving, but leaves open all possibilities e.g. for an If
// GMRemoveAllNondeterminism picks one deterministic move out of those available
// GMToTestForPurging is only used to indicate the presence or absence of any moves (including any possible future moves at a later time, e.g. for blocking operations begin_read/begin_write)
// When we rewrite, these things should be systematized. Maybe made into flags/bitmask for whether to generate each kind of move.

// GenerateConsumeMoves: these are easy. A move is merely to consume
// a message from one of the queues. Note: every MainQueue and WireQueue
// data structure that's listed is guaranteed to have a non-empty
// queue. That's our invariant.
let GenerateConsumeMoves (ps:ProgState) (gmode:GenerateMovesMode) : (Move list) =
( // gcm: turns a MainQueue structure into a move which offers up the first element of that queue
  let gcm (mq:MainQueue) : Move =
    let (v,r) = List.head mq.q in
    let mp = {new MainPort with a=mq.a and h=mq.h and ht=mq.ht and nq=NQNotForAdding} in
    MConsume {new MConsumeDat with mp=mp and r=r and isInternal=false} in
  //
  let moves = List.map gcm ps.mq in
  if List.length(moves)=0 then [] else
  if gmode=GMRemoveAllNondeterminism then [List.head moves] else
  if gmode=GMRemoveOrderingNondeterminism then [List.head moves] else
  if gmode=GMRemoveInessentialNondeterminism then moves else
  moves
)


let get_state_value (states:StateValue list) ((a,s):(string*string)) : (VPrim*TName*Importance*int*(Node option)) =
  let r = List.tryPick (fun (sv:StateValue)-> if sv.s.a=a && sv.s.s=s then Some(sv.v,sv.t,sv.i,sv.s.seq,sv.s.n) else None) states
  in match r with Some(r)->r | None -> raise(new System.Exception("RUNERROR: "^a^"."^s^" is not a state"))

let set_state_value (states:StateValue list) ((a,s):(string*string)) ((v,t,i,seq,n):(VPrim*TName*Importance*int*(Node option))) : (StateValue list) =
  let inner_loop (sv:StateValue) =
  ( if sv.s.a=a && sv.s.s=s then {new StateValue with s={new StateVersion with a=sv.s.a and s=sv.s.s and seq=seq and n=n} and v=v and t=t and i=i}
    else sv
  )
  in List.map inner_loop states



// GenerateNodeMoves: a move is when a node consumes the data that's present
// on its wires and does its computation. In this function we have to scan
// through the program; for each node, scan through the instances and see which
// of those instances can trigger the node. We represent a node's "triggers"
// with a Choices structure:
type Choices = Join list // (a&b) + (a&c)
and  Join = string list  // (a&c)
//
let GenerateNodeMoves (p:Prog) (ps:ProgState) (gmode:GenerateMovesMode) : (Move list) =
  let (moves : Move list ref) = ref [] in // accumulate the result here
  //
  // gnm_activity, gnm_handler and gnm_node just drill down through the program
  let rec gnm_activity (a:Activity) = List.iter (gnm_handler a) a.h
  and gnm_handler (a:Activity) ((sh,h):(string*Handler)) =
  ( if not a.ext then List.iter (gnm_node a (sh,h)) h.n;
  )
  and gnm_node (a:Activity) ((sh,h):(string*Handler)) (n:Node) =
  ( // figure out the possible triggers for this node
    let triggers:Choices = (match n.d with
       | NFirst(wts) -> List.map (fun (w,t)->[w]) wts    // from each (wirename,type) pair, we generate [ [wire1];[wire2];... ]
       | _ -> [ List.map (fun (w,q)->w) n.inWires ] ) in // from each (wirename,qsize),  we generate [ [wire1;wire2;...] ]
    // then look through instances to see if they're there
    List.iter (gnm_instance_choice a.s (sh,h.at) n triggers) ps.i
  )
  // gnm_instance takes a proposed activity/handler/node, with the triggers that
  // can fire it, and examines whether the given instance can supply those triggers.
  // all it does is an iteration over the trigger choices, to call gnm_instance_join.
  // That's where the work is.
  and gnm_instance_choice (a:string) ((h,ht):(string*Type)) (n:Node) (triggers:Choices) ((i,inst):(int*Instance)) =
  ( if inst.a=a && inst.h=h && inst.ht=ht then List.iter (gnm_instance_join a (h,ht) n (i,inst)) triggers
  )
  and gnm_instance_join a (h,ht) n (i,inst) (triggers:Join) =
  ( let (wps:WirePort list) = List.map (fun (src:string) -> {new WirePort with i=i and src=src and dst=n.s and nq=NQNotForAdding}) triggers in
    let (contains:bool list) = List.map (queuelist_contains_msg inst.q) wps in
    let any_missing = List.exists (fun b -> b=false) contains in
    let abort = (gmode=GMRemoveAllNondeterminism || gmode=GMRemoveOrderingNondeterminism) && List.length(!moves)>0 in
    if any_missing || abort then () else match n.d with
    | NIf(ifs) ->
         // consider the if-expression if (x>5). If we give it x=VAny, then the first branch
         // might accept it and might also let it fall through. But if we give it x=VInt(7) then
         // the first branch will accept it and deny a fallthrough. Of course, expressions just
         // evaluate to booleans VAny/VBool(true)/VBool(false) which makes this process easier
         // first, we need to grab the environment (made up of the input wires)
         let wps = List.map (fun src->{new WirePort with i=i and src=src and dst=n.s and nq=NQNotForAdding}) triggers in
         let (env:Environment) = List.map (fun (wp:WirePort) -> let (ni,v) = queue_remove wp [(i,inst)] in (wp.src,v)) wps in
         // although the above said "queue_remove", it didn't actually alter the real version of "instances": it discarded the version "ni" where the removals had taken place
         let rec do_ifs index ifs = match ifs with [] -> () |  (id::ifs) ->
           ( let v = Evaluate p.ef env [] Unimportant id.ife in match v.v with // "Unimportant" because here, for purposes of move-generation, we don't care about importance. That comes later in PerformMove.
             | VBool(true) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=index and isInternal=true}) :: (!moves);
             | VBool(false) -> do_ifs (index+1) ifs
             | VAny -> System.Console.Out.WriteLine("IFMOVE: index="^string(index)^"\n"); moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=index and isInternal=true}) :: (!moves); do_ifs (index+1) ifs;
             | _ -> raise(new System.Exception("RUNERROR: not-a-bool handed to if statement"));
           )
         in
         do_ifs 0 ifs
    | NIfType(ifs) ->
         // unlike the case for NIfType, the way Values are is that each one has a single
         // type. Therefore at most one of the if branches here can be taken
         // NB. guarantee from PostParse that there's at least one "if" and that all have the same ifv
         // first, we need to grab the environment (made up of the input wires)
         let wps = List.map (fun src->{new WirePort with i=i and src=src and dst=n.s and nq=NQNotForAdding}) triggers in
         let (env:Environment) = List.map (fun (wp:WirePort) -> let (ni,v) = queue_remove wp [(i,inst)] in (wp.src,v)) wps in
         //
         let ifvar = (List.head ifs).ifv in
         let v = Evaluate p.ef env [] Unimportant {new Expr with e=EIdPath[ifvar] and t=TUnk} in  // again, still Unimportant because we don't use the type here
         let ifs = List.mapi (fun i id -> (i,id)) ifs in // put index numbers on them for convenience
         let index = List.tryPick (fun (i,id) -> if tl_contains_unimp id.ift (v.t,Unimportant) then Some(i) else None) ifs in
         ( match index with None -> () | Some(index) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=index and isInternal=true}) :: (!moves); )
    | NCall(peer,verb,nq,e) ->
         // There are two cases. If we're calling an extern, then we're going to
         // synthesize its return-value right here and now in a kind of short-circuit.
         // (This saves on book-keeping, because we don't have to worry about extra instances
         // in degenerate cases). Note that the shortcircuit does not reduce nondeterminism
         // in any way. First, all call-handlers have infinite input queue (guaranteed by
         // PostParse) so we'd never need to simulate the loss of one of the messages
         // queued up at them. Second, any non-determinism due to a delay before the handler
         // gets activated can just as well be simulated by a delay before calling the "call" node.
         // We use index=(-1) for non-extern handlers which will be instantiated,
         // and index="i" means to call an extern handler and take back the ith element of its type
         // nb. the call argument has only one type, as guaranteed by InferTypes.
         if (List.length(e.t)<>1) then raise(new System.Exception("RUNERROR: call argument has non-unique type"));
         let a = activity_in_prog peer p in
         let h = handler_in_activity (verb,List.head e.t) a in
         // nb. the call handler is infinite as guaranteed by PostParse
         if (h.aq<>NUnbounded) then raise(new System.Exception("RUNERROR: call queue is bounded"));
         // In both the following we say "isInternal=true" as per the discussion above.
         let newmoves = if a.ext then List.mapi (fun index _->MFireNode{new MFireNodeDat with instance=i and node=n.s and src=triggers and index=index and isInternal=true}) h.rt
         else [MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=true}] in
         moves := newmoves @ (!moves)
    | NAtomic(ad) -> 
         // for an atomic primitive (beginread/beginwrite/endread/endwrite) we need to look up the state to see if it can proceed
         // The possibilities under "Any" are a bit weird. See also the discussion
         // in PerformMove. GenerateNodeMoves merely generates "possible" moves from the
         // current abstract state. If the current abstract state had "Any" number of
         // writers current, then an additional Begin.Write is possible in some of the
         // real-states corresponding to that abstract-state. And so we have to return it.
         let (statev,statet,statei,stateseq,staten) = get_state_value ps.s (a,ad.rw) in
         let (rcount,rcountt) = drill_record_getvprim (statev,statet) ["0"] in // field-names have been compiled down to indexes
         let (wcount,wcountt) = drill_record_getvprim (statev,statet) ["1"] in
         let rcount = match rcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: rcount not an int")) in
         let wcount = match wcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: wcount not an int")) in
         let canmove = (match ad.m with BeginRead -> wcount<=0 | BeginWrite -> rcount<=0 && wcount<=0 | EndRead -> true | EndWrite -> true) in
         let canmove = (if gmode=GMToTestForPurging then true else canmove) in
         if canmove then moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=false}) :: (!moves);
    // these two are purely internal
    | NExpr(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=true}) :: (!moves);
    | NCallreturn(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=true}) :: (!moves);
    // these are external: i.e. delaying them can change the behavior of the program
    | NSet(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=false}) :: (!moves);
    | NGet(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=false}) :: (!moves);
    | NFirst(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=false}) :: (!moves);
    | NSignal(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=false}) :: (!moves);
    | NReturn(_) -> moves := (MFireNode {new MFireNodeDat with instance=i and node=n.s and src=triggers and index=(-1) and isInternal=false}) :: (!moves);
  )
  in
  //
  List.iter gnm_activity p.a;
  !moves



// GenerateMoves: figures out all possible moves; but then,
// if there were any purely internal ones, it returns only the first one.
//
let GenerateMoves (p:Prog) (ps:ProgState) (gmode:GenerateMovesMode) =
( // a function to pick out the purely internal moves
  let picki (m:Move) = ( match m with
    | MSynthesize(md) -> if md.isInternal then Some(m) else None
    | MConsume(md) -> if md.isInternal then Some(m) else None
    | MFireNode(md) -> if md.isInternal then Some(m) else None
  ) in
  //
  let cmoves = GenerateConsumeMoves ps gmode in
  let nmoves = GenerateNodeMoves p ps gmode in
  if List.length(cmoves)=0 && List.length(nmoves)=0 then [] else
  //
  // If no nondetermisn is allowed at all, then we pick a single one
  if gmode=GMRemoveAllNondeterminism then [List.head (nmoves@cmoves)] else
  // For the "remove-ordering-nondeterminism" case, we trust that nmoves
  // already has none of that: it only examined the first node that could
  // trigger, and gave all the outcomes from that. If there were no node-moves
  // at all then we'll have to consume something
  if gmode=GMRemoveOrderingNondeterminism then (if List.length(nmoves)>0 then nmoves else [List.head cmoves]) else
  // For the "full" case, give them all!
  if gmode=GMFull then (nmoves@cmoves) else
  // Finally, for the "remove-inessential-nondeterminism" case, we check
  // if there are any available internal-only moves, and if there are then we do them.
  let moves = (GenerateConsumeMoves ps gmode) @ (GenerateNodeMoves p ps gmode) in
  let imoves = List.choose picki moves in
  let moves = if List.length(imoves)>0 then [List.head imoves] else moves in 
  moves
)



// purge_instances: removes any empty queues within an instance;
// and if any instance has no moves available and no outstanding calls then the instance is removed;
let purge_instances (p:Prog) (ps:ProgState) : ProgState =
  // we'll pick out the instances that can fire nodes:
  let moves = GenerateNodeMoves p ps GMToTestForPurging in 
  let movei = List.choose (fun (m:Move)->match m with MFireNode(fd)->Some(fd.instance) | _ -> None) moves in
  let msg = "  movei = "^(list_to_string "," (List.map string movei))^"\n" in
  // and the ones that are waiting for someone to return to them:
  let waiti = List.choose (fun ((i,inst):(int*Instance))->if inst.count>0 then Some(i) else None) ps.i in
  let msg = msg^"  waiti = "^(list_to_string "," (List.map string waiti))^"\n" in
  // and the ones that are external hence 
  // and merge them together:
  let keepi = List.sortWith compare (movei @ waiti) in
  let keepi = list_makeunique keepi in
  let msg = msg^"  keepi = "^(list_to_string "," (List.map string keepi))^"\n" in
  // Now we keep every instance that's on that list
  let (instances, discardi) = List.partition (fun ((i,inst):(int*Instance)) -> List.mem i keepi) ps.i in
  //if List.length(discardi)>0 then log ("Purge instances:\n"^(progstate_to_string ps)^msg);
  {ps with i=instances}





// PerformMove: given a current state and a choice of move, this
// computes the successor state. We keep a global variable "instance-counter"
// so as to generate new names
let instance_counter = ref 0
let PerformMove (p:Prog) (ps:ProgState) (move:Move) (movedesc:string) (profiler:(ProfileEvent->unit)option) : ProgState =
  //log ("\n---\nPerformMove "^movedesc^" "^(move_to_string move)^"...\n");
  match move with
  | MConsume(cd) -> 
    ( // take the message
      let (mq,(v,r)) = queue_remove_main cd.mp ps.mq in
      // spawn off a new instance of the handler
      let inst = (!instance_counter)+1 in
      instance_counter := inst;
      // post the init message on all wires that need it
      let a = activity_in_prog cd.mp.a p in 
      let h = handler_in_activity (cd.mp.h,cd.mp.ht) a in
      let outWires = h.outWires in // all wire-names and wire-sizes where the init message has to be posted
      let (wq:WireQueue list) = List.map (fun ((w,nq):Wire) -> {new WireQueue with src=h.a and dst=w and q=[v]})  outWires in
      let newinst = {new Instance with a=cd.mp.a and h=cd.mp.h and ht=cd.mp.ht and q=List.sortWith wirequeue_compare_keys wq and r=r and count=0} in
      let ps = {ps with mq=mq; i=List.sortWith instance_compare_keys ((inst,newinst)::ps.i)} in
      let ps = purge_instances p ps in // remove all empty queues and instances
      assert_sorted_progstate ps;
      ps
    )
  | MFireNode(fd) ->
    ( let states = ref ps.s in // we may have to update the program-state, so take a copy now
      let mqs = ref ps.mq in // likewise for the mainqueues
      let instances = ref ps.i in // likewise for the instances
      //
      // first, let's store each wire/trigger we need as WirePort datastructures, which are easier:
      let wps = List.map (fun src->{new WirePort with i=fd.instance and src=src and dst=fd.node and nq=NQNotForAdding}) fd.src in
      // next, take the trigger messages out of the queues. Store them in a (wire,value) list, the "environment"
      let (env:Environment) = List.map (fun (wp:WirePort) -> let (ni,v) = queue_remove wp (!instances) in instances := ni; (wp.src,v)) wps in
      let basei:Importance = imp_max (List.map (fun ((s,v):(String*Value))->v.i) env) in
      let basedep:Dependencies = dep_union (List.concat (List.map (fun ((s,v):(String*Value))->v.dep) env)) in
      // the next step depends on what the node is.
      let instance = List.assoc fd.instance (!instances) in
      let a = activity_in_prog instance.a p in
      let h = handler_in_activity (instance.h,instance.ht) a in
      let n = node_in_handler fd.node h in
      log ("  move: "^(node_to_string n)^"\n");
      // we'll just assemble the outWire list, for convenience, because regardless of node type we'll have to send on it
      // use i=(-1) 
      let (nwps:WirePort list) = List.map (fun (dst,nq) -> {new WirePort with i=fd.instance and src=n.s and dst=dst and nq=nq}) n.outWires in
      // NB. Here we have mutable variable "q" for the wirequeues of this instance,
      // and mutable variable "states" for the states of the program,
      // and the following code can read+update them both.
      ( match n.d with 
        | NExpr(e) ->
          ( let v = Evaluate p.ef env basedep basei e in
            instances := queue_add nwps v (!instances)
          )
        | NCall(peer,verb,nq,e) ->
          ( (match profiler with None -> () | Some profiler -> profiler (PCall(ps,peer,verb,List.head e.t,n)) );
            // The index tells us (-1) to instantiate the call-handler,
            // or "i" to immediately synthesis the ith return type of an extern call handler.
            let a:Activity = activity_in_prog peer p in
            let h:Handler = handler_in_activity (verb,List.head e.t) a in
            if a.ext then
            ( // in case of a call to an external node, we synthesis the return value immediately
              let t = List.nth h.rt fd.index in
              let v = genericvalue_of_type p t in // "any"
              if List.length(nwps)>1 then raise(new System.Exception("RUNERROR: a call statement with more than one returnpoints"));
              instances := queue_add nwps v (!instances);
              instances := modify_instance_callcount fd.instance (List.length(nwps)) (!instances);
              // modify-callcount because the very next node will be a CallReturn.
            )
            else
            ( // otherwise, in case of a call to a non-external node, we spin off a new instance
              // This code is similar to MConsume above.
              let v = Evaluate p.ef env basedep basei e in // that's what we're going to send
              // spawn off a new instance of the handler
              let inst = (!instance_counter)+1 in
              instance_counter := inst;
              // post the init message on all wires that need it
              let outWires = h.outWires in // all wire-names and wire-sizes where the init message has to be posted
              let (wq:WireQueue list) = List.map (fun ((w,nq):Wire) -> {new WireQueue with src=h.a and dst=w and q=[v]})  outWires in
              // the return port:
              if List.length(nwps)>1 then raise(new System.Exception("RUNERROR: call has more than one return port")); // PostParse synthesises just one
              let r = (if List.length(nwps)=0 then IgnoreReturn else List.head nwps) in
              let newinst = {new Instance with a=peer and h=verb and ht=h.at and q=List.sortWith wirequeue_compare_keys wq and r=r and count=0} in
              instances := List.sortWith instance_compare_keys ((inst,newinst) :: (!instances));
              instances := modify_instance_callcount fd.instance (List.length(nwps)) (!instances);
            )
          )
        | NCallreturn(e) ->
          ( let v = Evaluate p.ef env basedep basei e in
            instances := queue_add nwps v (!instances);
            instances := modify_instance_callcount fd.instance (-1) (!instances);
          ) 
        | NSet(idp,t,e) -> 
          ( let v = Evaluate p.ef env basedep basei e in
            let varname = List.head idp in
            let (statev,statet,statei,stateseq,staten) = get_state_value (!states) (a.s,varname) in
            let svold = {new StateValue with s={new StateVersion with a=a.s and s=varname and seq=stateseq and n=staten} and v=statev and t=statet and i=statei} in
            let (statev,statet) = drill_record_setvprim (statev,statet) (List.tail idp) (v.v,v.t) in
            let statei = v.i in
            let stateseq = stateseq+1 in
            states := set_state_value (!states) (a.s,varname) (statev,statet,statei,stateseq,Some(n));
            instances := queue_add nwps v (!instances);
            let svnew = {new StateValue with s={new StateVersion with a=a.s and s=varname and seq=stateseq and n=Some(n)} and v=statev and t=statet and i=statei} in
            match profiler with None -> () | Some(profiler) -> profiler(PStateSet(svold,svnew,v.dep));
          )
        | NGet(s,t) -> 
          ( let (statev,statet,statei,stateseq,staten) = get_state_value (!states) (a.s,s) in
            let v = {new Value with t=statet and i=basei and v=statev and dep={new StateVersion with a=a.s and s=s and seq=stateseq and n=Some(n)}::basedep } in
            // NB. When we get the statevalue, we take our "importance" from the inwires to our node;
            // not from whether or not the state was important
            instances := queue_add nwps v (!instances);
            let sv = {new StateValue with s={new StateVersion with a=a.s and s=s and seq=stateseq and n=staten} and v=statev and t=statet and i=v.i} in
            match profiler with None -> () | Some(profiler) -> profiler(PStateGet(sv));
          )
        | NAtomic(ad) ->
          ( // NB. for importance and dependencies, the outwire of an atomic action just
            // reflect that of the inwire. They don't take importance or dependencies from
            // the readwrite state.
            let (statev,statet,statei,stateseq,staten) = get_state_value (!states) (a.s,ad.rw) in
            let svold = {new StateValue with s={new StateVersion with a=a.s and s=ad.rw and seq=stateseq and n=staten} and v=statev and t=statet and i=statei} in
            let (rcount,rcountt) = drill_record_getvprim (statev,statet) ["0"] in // "rcount"
            let (wcount,wcountt) = drill_record_getvprim (statev,statet) ["1"] in // "wcount"
            let rcounti = match rcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: rcount not an int")) in
            let wcounti = match wcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: wcount not an int")) in
            if (ad.m=BeginRead && wcounti>0) then raise(new System.Exception("RUNERROR: BeginRead, but there are writers"));
            if (ad.m=BeginWrite && (rcounti>0 || wcounti>0)) then raise(new System.Exception("RUNERROR: BeginWrite, but there are others"));
            if (ad.m=EndRead && rcounti=0) then raise(new System.Exception("RUNERROR: EndRead, but there are no readers"));
            if (ad.m=EndWrite && wcounti=0) then raise(new System.Exception("RUNERROR: EndWrite, but there are not writers"));
            // The way we deal with VAny might seem a bit weird. But here it is...
            // Suppose that rcount=VAny and wcount=VAny. Then the Generate-Node-Moves function
            // will say that a BeginWrite move may be possible. If the state-space exploration
            // decides to explore this route, well, the route must necessarily have
            // corresponded to a reality with rcount=0 and wcount=0, and so we assign
            // rcount:=0,wcount:=1 in our continuation.
            let (rcounti,wcounti) = (match ad.m with
            | BeginRead -> if rcounti=(-1) then (-1,0) else (rcounti+1, 0)
            | EndRead -> if rcounti=(-1) then (-1,0) else (rcounti-1, 0)
            | BeginWrite -> (0, 1)
            | EndWrite -> (0, 0) ) in
            let rcount = if rcounti=(-1) then VAny else VInt(rcounti) in
            let wcount = if wcounti=(-1) then VAny else VInt(wcounti) in
            let (statev,statet) = drill_record_setvprim (statev,statet) ["0"] (rcount, rcountt) in
            let (statev,statet) = drill_record_setvprim (statev,statet) ["1"] (wcount, wcountt) in            
            let v = {new Value with t="void" and i=basei and v=VVoid and dep=basedep } in
            // for staten, we only record lock-acquisitions, not lock-releases
            let staten = (match ad.m with | BeginRead -> Some(n) | BeginWrite -> Some(n) | EndRead -> staten | EndWrite -> staten) in
            states := set_state_value (!states) (a.s,ad.rw) (statev,statet,statei,stateseq+1,staten);
            instances := queue_add nwps v (!instances);
            let svnew = {new StateValue with s={new StateVersion with a=a.s and s=ad.rw and seq=stateseq+1 and n=Some(n)} and v=statev and t=statet and i=statei} in
            ( match profiler with None -> () | Some(profiler) -> profiler(PStateSet(svold,svnew,basedep)) );
            ( match profiler with None -> () | Some(profiler) -> profiler(PAtomic(a.s,ad.rw,ad.m,rcounti,wcounti,n)) );
          )
        | NFirst(ids) -> 
          ( // it came from fd.src which is guaranteed to have size 1, and was put in the environment
            // therefore the environment has only this value
            if (List.length(env)<>1) then raise(new System.Exception("RUNERROR: NFirst got more than one value"));
            let (n,v) = List.head env in
            instances := queue_add nwps v (!instances);
          )
        | NSignal(e) -> 
          ( let v = Evaluate p.ef env basedep basei e in
            mqs := queue_broadcast_main p a.s (v.t,v.i) (!mqs) v;            
          )
        | NReturn(e) -> 
          ( let instance = List.assoc fd.instance (!instances) in // that's the instance where this node is firing
            let v = Evaluate p.ef env basedep basei e in
            if instance.r=IgnoreReturn then ()
            else if instance.r=NoReturn then raise(new System.Exception("RUNERROR: return on a NoReturn"))
            else instances := queue_add [instance.r] v (!instances);
          )
        | NIf(ifs) -> 
          ( let id = List.nth ifs fd.index in // pick out the winning branch
            // TODO: narrow the local environment and hence "thene"
            let v = Evaluate p.ef env basedep basei id.thene in
            let (nwps:WirePort list) = List.map (fun (dst,nq) -> {new WirePort with i=fd.instance and src=id.thens and dst=dst and nq=nq}) id.thenOutWires in
            instances := queue_add nwps v (!instances)
          )
        | NIfType(ifs) ->
          ( let id = List.nth ifs fd.index in // pick out the winning branch
            let v = Evaluate p.ef env basedep basei id.tthene in
            let (nwps:WirePort list) = List.map (fun (dst,nq) -> {new WirePort with i=fd.instance and src=id.tthens and dst=dst and nq=nq}) id.tthenOutWires in
            instances := queue_add nwps v (!instances)
          )
      );
      // finally, package up the answers
      let ps = {new ProgState with mq=(!mqs) and s=(!states) and i=(!instances)} in
      let ps = purge_instances p ps in // remove all empty queues and instances
      assert_sorted_progstate ps;
      ps
    )
  | _ -> raise(new System.Exception("Move "^(move_to_string move)^" unimplemented"))

  

// step: performs some moves, updating the program state and printing stuff as it goes
let step (p:Prog) (ps:ProgState) (gprint:ProgState->bool->unit) (keepgoing:bool) (profiler:(ProfileEvent->unit)option) : ProgState =
  let ps = ref ps in
  let moves = ref (GenerateMoves p (!ps) GMFull) in
  let nmade = ref 0 in 
  while List.length(!moves)>0 do
  ( let nmoves = List.length(!moves) in
    let i = rgen.Next(0,nmoves) in
    let movedesc="#"^string(i+1)^"/"^string(nmoves) in
    let move = List.nth (!moves) i in
    gprint (!ps) false;
    ps := PerformMove p (!ps) move movedesc profiler;
    gprint (!ps) true;
    System.Threading.Thread.Sleep(10);
    nmade := (!nmade)+1;
    if (keepgoing) then moves := GenerateMoves p (!ps) GMFull else moves := [];
  ) done;
  if (!nmade)>0 then log ((progstate_to_string (!ps))^"\n")
  else log ("stopped.\n");
  !ps


let string_to_expr (s:string) : Expr =
  let lexbuf = Lexing.from_string s in
  let expr =
    try Pars.Expr Lex.token lexbuf
    with e -> let pos = Lexing.lexbuf_curr_p lexbuf in
      raise (new System.Exception("Parsing error near char "^string(pos.pos_cnum+1)^"\n"^e.ToString()))
    in
  expr

// insert: given Notify(v) that the user provides, we stick it in all the
// appropriate notify-queues. (i.e. stick it in every notify-queue in the
// program which handles that notify+type).
let insert (p:Prog) (ps:ProgState) (gprint:ProgState->bool->unit) (notify:string) (v:Value) : ProgState =
  log ("INSERT "^notify^"("^(value_to_string v)^")\n");
  let mq = queue_broadcast_main p notify (v.t,v.i) ps.mq v in
  gprint ps false;
  let ps = {ps with mq=mq} in
  gprint ps true;
  log ((progstate_to_string ps)^"\n");
  ps


let explore (prefix:string) (p:Prog) (ps:ProgState) (gmode:GenerateMovesMode) (gprint:ProgState->bool->unit) (profiler:(ProfileEvent->unit)option) : unit =
( let explored = Hashtbl.create 100 in // contains ps->true for every ps that we've visited
  let workqueue : (ProgState*string) list ref = ref [(ps,prefix)] in // ps*movedesc
  // invariant: the workqueue contains only things which are not already explored
  while List.length(!workqueue)>0 do
    let (ps,prefix) = List.head (!workqueue) in
    workqueue := List.tail (!workqueue);
    //gprint ps true;
    //log ("EXPLORE "^prefix^":\n"^(progstate_to_string ps)^"\n\n");
    log (prefix^"\n");
    (match profiler with None -> () | Some profiler -> profiler (PExplore(ps)) );
    let moves = GenerateMoves p ps gmode in
    let nmoves = List.length moves in
    let per_move (i:int) (m:Move) : unit = 
    ( let movedesc = if nmoves=1 then prefix^"." else prefix^string(i+1)^"/"^string(nmoves)^"." in
      let ps = PerformMove p ps m movedesc profiler in
      if Hashtbl.mem explored ps then ()
      else (workqueue := (ps,movedesc)::(!workqueue); Hashtbl.add explored ps true)
    )
    in
    List.iteri per_move moves
  done;
)



