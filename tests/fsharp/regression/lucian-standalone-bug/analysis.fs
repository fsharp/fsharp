#indent "off"

module Analysis

open System
open System.Windows.Forms
open Utils
open Ast
open States


// TODO:
// check for infinite loops through notifies.
// Each entity in the source code has an optional [id], which is taken to be line-number if absent, and used for debugging messages
// Clean up compiler error messages: have each one print, and allow continued compilation to find further error messages
// Enforce non-multiwire inputs to most nodes, as per email to Georgio
// Make the type-inferer handle circular loops
// If you BeginRead/BeginWrite with no outwires, it's probably a bug!
// Change where the {inwires} go in the syntax
// Georgio wanted to check that locks are released
// Check that there will be only one return.
// Should have much more determinism for instantiating queues and doing CallReturn nodes and synthesising an external thing
// Check all the dependencies. They should flow with wires, not with evaluating. Needs some refactoring.
// When we make a call/return, hopefully the dependencies will propogate
// The types for StateValue/StateVersion/getstate/setstate should be tidied up
// Circular loops (in wires and notify-ports) will require (1) proper hash-table for state-space exploration, and
// (2) a search strategy with good coverage (doesn't get bogged down in places), and
// (3) counting, so we know when to stop prematurely.
// For circular locks between modules, the best solution is type-based. Make the type-inference-thing
// figure out for each handler which calls it makes while possibly under the influence of locks.
// Also admit this information in the handler declaration for externs.
// Then, look for cycles in the graph.
// NB. Won't bother analysing how many "return" statements get called. Because they won't be used in the graphs.
// NB. For purpose of marshalling state to fingerprint to compare if two states are equal, we
// won't bother re-ordering the instance-indexes (because exploration is only used for race-detection,
// where the two instances are fixed and determinate... or at least they will be once I short-circuit
// calling to an extern). We will have to re-order the message queues though. This will be important,
// because reordering and fingerprinting and sharing states will cut things down from 2 + n to n + 2.
// Must come up with an error model that deals with problems of over-approximation - e.g. thinking
// that a lock is released twice when in reality it's only ever released once. At the moment
// I e.g. throw an exception in the twice-release case.
// Must add the cycle-stuff to external handlers.


// VERIFICATION:
// We have a runtime execution model which tracks "causality" and type and importance. That is,
// for each value, (1) if any GETs led to this value then it records the sequence-tag
// on the GET; (2) if any important notifications led to it then it records that fact.
// Runtime errors:
// (1) accessing a field which doesn't exist (done by datatype checking)
// (2) failing a type assertion (activity handlers/notifies have explicit types)
// (3) if we SET#n due to an important message, then any subsequent SET which depends on a GET#<n is erroneous
// Model-checking errors:
// (4) no possible execution trace can reach a particular state
// Bugs so far observed:
// In both my MyState and Bumper, I forgot to SetState. This would have been picked up by reachability analysis
// In the original diagram there was a miswire on b. This would have been picked up by type analysis
// In the translation of Explorer I bet there were race conditions. This will hopefully be found by importance analysis
// When writing the conditions in Wander.OnBumper, I mistyped the third condition so it was the same as the first
// When testing Wander.OnBumper, I accidentally gave it (false,false) so it got nowhere.



// flatten_typelist: given a typelist, if anything inside it is the name of a choice-typedef
// then it will be expanded to the contents of that choice-typedef. Oh, and it's an error to
// call it "important" if it is a choice-typedef, since its expansion will say that.
// Oh, and in what we produce, everything must be a valid type
// tlclean: cleans up a typelist by merging entries; choosing max importance in case of duplicates
let tl_union tail =
  let rec tmerge_imp tail = match tail with
      | [] -> []
      | (t1,imp1)::[] -> [(t1,imp1)]
      | (t1,imp1)::(t2,imp2)::tail -> if t1=t2 then tmerge_imp ((t1,imp_max [imp1;imp2])::tail)
                                    else (t1,imp1) :: (tmerge_imp ((t2,imp2)::tail)) in
  tmerge_imp (List.sortWith type_compare_unimp tail)
//
let rec flatten_typelist tc tp tail =
  if (tail=TUnk) then tail else
  let deref (t,imp) = match List.try_assoc t tc with None -> [(t,imp)]
      | Some tail -> if imp=Unimportant then flatten_typelist tc tp tail else raise (new System.Exception("can't call " + t + " important")) in
  let tail = tl_union (List.concat (List.map deref tail)) in
  let ensuretypedefined (t,i) = (match List.try_assoc t tp with Some _ -> () | None -> raise (new System.Exception("Type " + t + " undefined")) ) in
  List.iter ensuretypedefined tail;
  tail

// PostParse: Normalizes all the type definitions - so that all TDefChoice are lists
// of TPrims, and they're all dereferenced in the code.
// Also ensures that no typename is used as an activity, state, handler or wire name.
// Also ensures that no statename is used as a wirename.
// Also ensures that no two activities have the same name
// Also ensures that in NIfType all the conditionals are on the same name
// (and fills in that for the "else" branch if present)
// and makes sure that their if-types are unique
// Also ensures that NIf and NIfType have at least one ifdat.
// Also converts NExpr(EIdPath[type;field]) to NExpr(EEnum(type,field))
// Also completes inWires, by adding to inWires any wires mentioned in the expression
// that weren't already listed in inWires.
// Also ensures that all request/response handlers have an unbounded input queue
// Also simplifies the code:
// NExpr contains no "state-get" expressions (IdPath on state bceomes NGet), also for if-then nodes.
// (nb. that the grammar of the language already forces if-then nodes to be expressions anyway).
// NCall/NSet/NSignal/NReturn/NFirst arguments are pure wires (EIdPath(IdPath))
// NCall has exactly one outwire which leads to a NCallreturn
// NIf expressions have no gets.
// Note that when we evaluate an NIf consequence, it's done in the same context as the If part of it.
// Also fixes up the outWires.
// Note: outWires should be done after the simplification (so as to fix up outWires of
// the simplification as well). And simplification must be done after inWires. Simplification
// will generate correct inWires itself, but not correct outWires. Simplification needs to
// know the inWires before it can generate extra nodes.
// Note: the simplifications can introduce extra wires. Each wire has a buffer. We generally
// use "maximum buffer size of what it's replacing" as the size of these introduced wires.
// This can result in the system having more buffering than the user originally programmed.
// In effect, we should think of the user program as specifying minimum buffer sizes, not exact.
let PostParse ((tp,tc,ef,a):(TDefPrim list * TDefChoice list * ExternFun list * Activity list)) :Prog =
  let tc = List.map (fun (tname,tail) -> (tname, flatten_typelist tc tp tail)) tc in
  //
  let rec postparse_activity (a:Activity) = 
    System.Console.Out.WriteLine ("postparse: activity " + a.s + "\n");
    if (List.mem_assoc a.s tp) || (List.mem_assoc a.s tc) then raise(new System.Exception("ERROR: activity " + a.s + " is named after a type"));
    let lvars = List.map (fun (s,sf)->s) a.state in
    {new Activity with s=a.s and state=List.map (postparse_state a) a.state and signal=flatten_typelist tc tp a.signal and h=List.map (postparse_h a lvars) a.h and ext=a.ext}
  // postparse_state: flattens the typelist
  and postparse_state (a:Activity) (s,e) =
    if (List.mem_assoc s tp) || (List.mem_assoc s tc) then raise(new System.Exception("ERROR: state " + a.s + "." + s + " is named after a type"));
    (s, {new Expr with e=e.e and t=flatten_typelist tc tp e.t} )
  // postparse_h: given a handler (s,h), it returns a corrected handler - first it
  // makes all expressions simple in the handler (see below), then it computs the outWires list
  // (similar to explanation of postparse_outWires), then it flattens h.at (handler argument type).
  and postparse_h (a:Activity) (lvars:string list) ((s,h):(string*Handler)) =
    System.Console.Out.WriteLine ("postparse: handler " + s + "(" + (type_to_string h.at) + ")\n");
    if (List.mem_assoc s tp) || (List.mem_assoc s tc) then raise(new System.Exception("ERROR: " + a.s + ".on " + s + " is named after a type"));
    if (h.rt<>[] && h.aq<>NUnbounded) then raise(new System.Exception("ERROR: " + a.s + "." + s + ":" + (type_to_string h.at) + " has a bounded queue; being a request/response handler, it must be unbounded"));
    let (hat,hai)=h.at in if (hai=Important && h.aq<>NUnbounded) then raise(new System.Exception("ERROR: " + a.s + "." + s + ":" + (type_to_string h.at) + " has finite queue; being a handler for important notifies, it must be unbounded"));
    let n = List.concat (List.map (postparse_node a h h.a lvars) h.n) in // simplify &c.
    let n = List.concat (List.map (postparse_callOutWires a h) n) in // make so Call nodes have only a single outwire
    //
    let outWires = List.choose (has_inWire h.a) n in
    let at = flatten_typelist tc tp [h.at] in
    let at = if List.length(at)=1 then List.head(at) else raise(new System.Exception("Handler " + s + " must have exactly one input type")) in
    (s, {new Handler with a=h.a and aq=h.aq and at=at and outWires=outWires and rt=flatten_typelist tc tp h.rt and n=n} )
  //
  // postparse2: this simply fixes the output wires. It has to be called as a separate
  // phase after postparse, since postparse might turn a "call" node into a "call''/callreturn" pair
  // and people who before would have had outwires to "call" now have them to "call''".
  // postparse_outWires: given a node n, returns an updated version of it in which
  // the "outWires" list has been computed. (how? by looking for all other nodes in the
  // handler which have n as one of their inWires). Also, if the node is an If node,
  // does the same for all the If branches.
  and postparse2_activity (a:Activity) = {new Activity with h=List.map (postparse2_handler a) a.h and s=a.s and state=a.state and signal=a.signal and ext=a.ext}
  and postparse2_handler (a:Activity) ((s,h):(string*Handler)) = (s, {new Handler with n=List.map (postparse2_outWires a h h.n) h.n and a=h.a and at=h.at and aq=h.aq and outWires=h.outWires and rt=h.rt} )
  and postparse2_outWires (a:Activity) (h:Handler) (ns:Node list) (n:Node) =
    let outWires = List.choose (has_inWire n.s) ns in
    let nd = match n.d with
     | NIf(ifs) -> let ifs = List.map (postparse_outWires_ifdat a h ns) ifs in NIf(ifs)
     | NIfType(ifs) -> let ifs = List.map (postparse_outWires_iftypedat a h ns) ifs in NIfType(ifs)
     | _ -> n.d in
    {new Node with d=nd and outWires=outWires and s=n.s and t=n.t and inWires=n.inWires and src=n.src}
  and postparse_outWires_ifdat (a:Activity) (h:Handler) (ns:Node list) (id:IfDat) : IfDat =
    let outWires = List.choose (has_inWire id.thens) ns in
    {id with thenOutWires = outWires}
  and postparse_outWires_iftypedat (a:Activity) (h:Handler) (ns:Node list) (id:IfTypeDat) : IfTypeDat =
    let outWires = List.choose (has_inWire id.tthens) ns in
    {id with tthenOutWires = outWires}
  //
  // postparse_callOutWires: given a node n, returns an updated version of it in which
  // the "outWires" list has 1 element pointing to a NCallreturn node. This means might
  // have to return a list of nodes. What should be the queue-length of this intermediate
  // thing? -- the maximum of what fallows.
  and postparse_callOutWires (a:Activity) (h:Handler) (n:Node) =  match n.d with
    | NCall(_) -> 
      ( let qmax = queue_max (List.map (fun ((w,q):Wire)->q) (("",NBounded(1))::n.outWires) ) in
        let callnode = {new Node with s=n.s + "''" and outWires=[(n.s,qmax)] and t=n.t and d=n.d and inWires=n.inWires and src=n.src} in
        let returnnode = {new Node with s=n.s and t=n.t and d=NCallreturn({new Expr with e=EIdPath([n.s + "''"]) and t=n.t}) and inWires=[(n.s + "''",qmax)] and outWires=n.outWires and src=n.src} in
        [callnode;returnnode] )
    | _ -> [n]
  // has_inWire: returns Some (n.s,q) if this node has (q,dst) as an inWire
  and has_inWire (dst:string) (n:Node) : (Wire option) =
    let q = List.try_assoc dst n.inWires in
    match q with Some q -> Some (n.s,q) | None ->  None
  // postparse_node: first completes the "inWires" list, by adding to it any
  // wire that was mentioned in the expr but was not yet in the list. (or,
  // makes it triggered by the handler itself if there are no other triggers).
  // Then it simplifies in the expression inside the node, as described above.
  // This might result in some additional prior nodes being generated (all of which
  // will have their inWires correct). It returns a list of nodes: this corrected
  // one, plus any prior ones that arose from expr-simplification.
  and postparse_node (a:Activity) (h:Handler) (activityarg:string) (lvars:string list) (n:Node) = try (
    System.Console.Out.WriteLine ("postparse: line " + string(n.src) + ": " + (node_to_string n) + "\n");
    if (List.mem_assoc n.s tp) || (List.mem_assoc n.s tc) then raise(new System.Exception("ERROR: Node at line " + string(n.src) + " is named " + n.s + ", after a type"));
    if (List.mem n.s lvars) then raise(new System.Exception("ERROR: Node at line " + string(n.src) + " is named " + n.s + ", after a state variable"));
    let inWires = wire_union_qmerge [n.inWires;(get_ndInWires lvars n.d)] in
    let inWires = (if List.length(inWires)=0 then [(activityarg,NBounded(1))] else inWires) in
    // that's so it depends on the handler itself if there were no other inWires
    let n = {new Node with s=n.s and t=flatten_typelist tc tp n.t and d=n.d and inWires=inWires and outWires=n.outWires and src=n.src} in
    match n.d with
    | NExpr(e) -> let (inWires,e,ne) = make_expression_simple false activityarg lvars n e in
          ne @ [{n with d=NExpr(e); inWires=inWires}]
    | NCall(peer,verb,nq,e) -> let (inWires,e,ne) = make_expression_simple true activityarg lvars n e in
          ne @ [{n with d=NCall(peer,verb,nq,e); inWires=inWires}]
    | NCallreturn(s) -> [n]
    | NSet(idp,tail,e) -> let (inWires,e,ne) = make_expression_simple true activityarg lvars n e in
          ne @ [{n with d=NSet(idp,tail,e); inWires=inWires}]
    | NAtomic(ad) -> [n]
    | NGet(idp,tail) -> [n]
    | NFirst(_) -> [n]
    | NSignal(e) -> let (inWires,e,ne) = make_expression_simple true activityarg lvars n e in
          ne @ [{n with d=NSignal(e); inWires=inWires}]
    | NReturn(e) -> let (inWires,e,ne) = make_expression_simple true activityarg lvars n e in
          ne @ [{n with d=NReturn(e); inWires=inWires}]
    | NIf(ifs) -> 
          // to do this easily, we make a dummy expression that includes all the "if" expressions
          // and the "then" expressions.
          // then pass this dummy expression to the simplify_expression routine, then unpack it.
          // That's because the simplify_expression routine only takes in a single expression
          // rather than a list. (we can't call simplify_expression multiple times, once for
          // each expression, because the If node really needs inWires as the union of all expressions,
          // and needs all get-state things pulled out ahead of it.)
          // NOTE: We are requiring all "THEN" variables to be present up-front at the start of the IF.
          // This might be unexpected semantics. But it simplifies implementation and is in any case
          // still a generalisation of what the diagrams can express.
          let es = List.map (fun (id:IfDat) -> {new Expr with t=TUnk and e=EFun("dummyarg",[id.ife;id.thene])}) ifs in
          let e:Expr = {new Expr with t=TUnk and e=EFun("dummy",es)} in 
          let (inWires,e,ne) = make_expression_simple false activityarg lvars n e in
          let es:Expr list = match e.e with EFun(_,es)->es | _ -> raise(new System.Exception("not a dummy fun")) in
          let es:(Expr*Expr)list = List.map (fun e -> match e.e with EFun(_,[e1;e2])->(e1,e2) | _ -> raise(new System.Exception("not a dummy arg"))) es in
          let ifs = List.map2 (fun oldid (e1,e2) -> {oldid with ife=e1; thene=e2}) ifs es in
          if List.length(ifs)=0 then raise(new System.Exception("ERROR: no branches on if"));
          ne @ [{n with d=NIf(ifs); inWires=inWires}]
     | NIfType(ifs) ->
          // like "if" with the dummy expression with all the "then" expressions
          // but we first have to do work on the types -- flatten them, make sure they're unique
          // and we also have to ensure that the variable name that we're testing is unique
          let names = List.choose (fun (id:IfTypeDat) -> if (id.ifv="") then None else Some id.ifv) ifs in
          let names = list_makeunique (List.sortWith Operators.compare names) in
          if (List.length(names)=0) then raise(new System.Exception("ERROR: iftype without names"));
          if (List.length(names)>1) then raise(new System.Exception("ERROR: if (v:type) must use same variable for all branches"));
          let ifv = List.head names in
          if List.mem ifv lvars then raise(new System.Exception("ERROR: if (" + ifv + ":type) cannot test type of a state variable"));
          let ifs = List.map (fun (id:IfTypeDat) -> {id with ifv=ifv; ift=flatten_typelist tc tp id.ift}) ifs in
          // now we can do the dummy expression
          let es = List.map (fun (id:IfTypeDat) -> id.tthene) ifs in
          let e:Expr = {new Expr with t=TUnk and e=EFun("dummy",es)} in
          let (inWires,e,ne) = make_expression_simple false activityarg lvars n e in
          let es:Expr list = match e.e with EFun(_,es)->es | _ -> raise(new System.Exception("not a dummy fun")) in
          let ifs = List.map2 (fun oldid e -> {oldid with tthene=e}) ifs es in
          if List.length(ifs)=0 then raise(new System.Exception("ERROR: no branches on iftype"));
          ne @ [{n with d=NIfType(ifs); inWires=inWires}]
     ) with e -> raise (new System.Exception("line " + string(n.src) + ": " + e.ToString()))
  // recognize_enums: every EIdPath([type;...]) is turned into an EEnum
  and recognize_enums (e:Expr) : Expr = match e.e with
    | EVoid -> e | EInt(_) -> e | EBool(_) -> e | EString(_) -> e | EEnum(_) -> e
    | EImportance(i,e) -> {e with e=EImportance(i,recognize_enums e)}
    | EFun(f,es) -> {e with e=EFun(f,List.map recognize_enums es)}
    | EExternal(f,es) -> {e with e=EExternal(f,List.map recognize_enums es)}
    | EConstructor(t,fs) -> {e with e=EConstructor(t,List.map (fun (f,e)->(f,recognize_enums e)) fs)}
    | EIdPath(idp) -> let head = List.head idp in match (List.try_assoc head tp) with None -> e
        | Some(TEnum(ss)) -> if List.length(idp)<>2 then raise(new System.Exception("ERROR: " + (idpath_to_string idp) + " not a valid enum expression"));
          let field = List.nth idp 1 in if List.mem field ss then {e with e=EEnum(head,field)}
          else raise(new System.Exception("ERROR: " + (idpath_to_string idp) + " not a valid enum field"));
        | _ -> raise(new System.Exception("ERROR: " + (idpath_to_string idp) + " not an enum type"));
  // make_expression_simple: it's job is to factor out an expression so that some operations
  // (e.g. getting state) are done by a prior node. For some purposes we have to factor
  // out all interesting expressions and just return an Expr which is a pure wire.
  // Well, the function returns (inWires, e', nodes) where "nodes" are the additional prior nodes
  // that are needed (their own inWires are all correct), and e' is the updated expression, and
  // inWires is the updated list of inWires needed for this expression.
  // As for the additional nodes that we synthesize, suppose we're handling
  // node "n" which has expression "e"=state.field+2, and suppose that we're asked to
  // return just a pure-wire expression. So we synthesize
  //   n'state = NGet(state)  -- because we have to factor out NGet nodes
  //   n' = n'state.field+2   -- because we were asked to return just a pure wire expression
  //   n  = n'                -- and this, "n'", is the expression we'll return.                
  and make_expression_simple (mustbewire:bool) activityarg (lvars:string list) (n:Node) (e:Expr) : (Wire list * Expr * Node list) =
    let e = recognize_enums e in
    // "states" is a list of all state fields which are needed for this expression, and which we'll synthesize NGet nodes for:
    let states = list_makeunique (List.sortWith Operators.compare (get_expression_states lvars e)) in
    // make_getnode: a small function which produces a "NGet" node which gets the specified state
    let make_getnode s = {new Node with s=n.s + "'" + s and t=TUnk and d=NGet(s,TUnk) and inWires=n.inWires and outWires=[] and src=n.src} in
    let ne = List.map make_getnode states in
    // question: if in simplifying NExpr(e) we need to synthesise a NGet node,
    // then what should be the queue-size of the link from NGet to NExpr? Answer:
    // just the smallest of the queue-sizes that made up the original inWires.
    let minqueue = queue_min (List.map (fun (s,q)->q) n.inWires) in
    let newInWires = List.map (fun s -> (n.s + "'" + s,minqueue)) states in
    let inWires = wire_union_qmerge [newInWires;n.inWires] in
    // "renames" is because an expression which used to be, say, state+5, must now be "state'+5" where state' is the name of the wire we synthesized.
    let renames = List.map (fun s -> (s,n.s + "'" + s)) states in
    let e = rename_expr_id renames e in // expr with all the renamings done on it
    // now for the answer. If our expression is just selecting a field of an inwire, or whoever
    // called us didn't require a pure wire, then we can return what we've made:
    if (is_eidp e || not mustbewire) then
    ( (inWires,e,ne)
    ) else
    ( // otherwise, our caller required an answer that was a pure wire. So we have to synthesize it
      // by generating an extra NExpr node, and then returning an Expr which is a pure wire.
      // See discussion at the start of this function for an example of how nodes are named.
      let ne = ne @ [{new Node with s=n.s + "'" and t=TUnk and d=NExpr(e) and inWires=inWires and outWires=[] and src=n.src}] in
      let inWires = [(n.s + "'", minqueue)] in // the new inWires that our pure wire will depend upon
      ( inWires, {new Expr with t=TUnk and e=EIdPath([n.s + "'"])}, ne)
    )
  // get_expression_states: returns a list of every state-variable that the expression refers to
  and get_expression_states (lvars:string list) (e:Expr) : string list = match e.e with
    | EVoid -> []
    | EImportance(i,e) -> get_expression_states lvars e
    | EIdPath(idp) -> if List.mem (List.head idp) lvars then [List.head idp] else []
    | EFun(f,es) -> List.concat (List.map (get_expression_states lvars) es)
    | EExternal(f,es) -> List.concat (List.map (get_expression_states lvars) es)
    | EConstructor(t,fs) -> List.concat (List.map (fun (s,e)->get_expression_states lvars e) fs)
    | EInt(i) -> [] | EBool(b) -> [] | EString(s) -> [] | EEnum(t,f)->[]
  // rename_expr_id: for every EIdPath[a;b;c] turns it into EIdPath[a';b;c] if "renames" had the pair (a,a')
  and rename_expr_id (renames:(string*string)list) (e:Expr) : Expr = match e.e with
    | EVoid ->  e
    | EIdPath(idp) -> (let nn = List.try_assoc (List.head idp) renames in match nn with None -> e
        | Some nn -> {e with e=EIdPath(nn :: (List.tail idp))} )
    | EFun(f,es) -> {e with e=EFun(f,List.map (rename_expr_id renames) es)}
    | EExternal(f,es) -> {e with e=EExternal(f,List.map (rename_expr_id renames) es)}
    | EConstructor(t,fs) -> {e with e=EConstructor(t,List.map (fun (s,e)->(s,rename_expr_id renames e)) fs)}
    | EImportance(i,e) -> {e with e=EImportance(i,rename_expr_id renames e)}
    | EInt(i) -> e | EBool(b) -> e | EString(s) -> e | EEnum(t,f)->e
  and is_eidp (e:Expr) = match e.e with EIdPath(wire) -> true | _ -> false
  and are_eidp (es:Expr list) = let nwes = List.filter (fun e -> not(is_eidp e)) es in List.length(nwes)=0
  and wire_union_qmerge (t:Wire list list) : (Wire list) = let t = List.sortWith wire_compare_q (List.concat t) in wire_unique_qmerge t
  // get_ndInWires: given a NData, returns a list of all wires that are mentioned in the NData expression.
  and get_ndInWires (lvars:string list) (nd:NData) : (Wire list) = match nd with
    | NExpr(e) -> get_etriggers lvars e
    | NCall(peer,verb,nq,e) -> get_etriggers lvars e
    | NCallreturn(e) -> get_etriggers lvars e
    | NSet(idp,tail,e) -> get_etriggers lvars e
    | NGet(idp,tail) -> []
    | NAtomic(ad) -> []
    | NFirst(fs) -> List.map (fun (s,tail)->(s,NBounded(1))) fs
    | NSignal(e) -> get_etriggers lvars e
    | NReturn(e) -> get_etriggers lvars e
    | NIf(ifs) -> List.concat (List.map (fun (id:IfDat) -> List.concat [get_etriggers lvars id.ife; get_etriggers lvars id.thene]) ifs)
    | NIfType(ifs) -> ((List.head ifs).ifv,NBounded(1)) :: (List.concat (List.map (fun (id:IfTypeDat) -> get_etriggers lvars id.tthene) ifs))
  // get_etriggers: given an expression, the expression will include several EIdPath[a;b;c...],
  // and this function returns the list of all such "a"s that aren't in the argument "lvars"
  and get_etriggers (lvars:string list) (e:Expr) = match e.e with
    | EBool(_) -> [] | EInt(_) -> [] | EString(_) -> [] | EVoid -> [] | EEnum(t,f)->[]
    | EImportance(i,e) -> get_etriggers lvars e
    | EFun(f,es) -> List.concat (List.map (get_etriggers lvars) es)
    | EExternal(f,es) -> List.concat (List.map (get_etriggers lvars) es)
    | EConstructor(cn,args) -> List.concat (List.map (fun (s,e) -> get_etriggers lvars e) args)
    | EIdPath(idp) -> if List.mem (List.head idp) lvars then [] else [(List.head idp,NBounded(1))]
  in
  //
  let anames = List.map (fun (a:Activity)->a.s) a in
  let anames = List.sortWith Operators.compare anames in
  let e = list_firstdup Operators.compare anames in
  (match e with None -> () | Some(e) -> raise(new System.Exception("ERROR: two activities both named " + e + ".")) );
  let a = List.map postparse_activity a in
  let a = List.map postparse2_activity a in
  {new Prog with tp=tp and ef=ef and a=a}



  
// CheckServicePorts: each activity declares what TypeList it can notify
// (or has this empty if it has no notifications), and it declares a list of
// Request/Response handlers with their input Type and output TypeList.
// For "extern" activities this is all. But for the activities implemented
// in the program there is more: there are handlers that receive notifications,
// and there are "call" nodes.
// For declarations, we merely check that each request/response handler within
// an activity has a unique name/type combination (and types must be unique, not 0 or multi)
// For extern activities, we check that there are no handlers and no state and no bodies of request/responses.
// For handlers, we check that the peer they're listening to has been declared,
// and that the type they're listening on is exactly one of the declared notify types
// (not zero or many), and that no activity has two handlers for the same peer+type.
// For call-nodes, we check that the peer+verb has been defined in the program.
// (type-checking of input and output is left for later, as is fixing NCall(..nq).)
let CheckServicePorts (p:Prog) =
  let rec check_activity (a:Activity) =
  ( System.Console.Out.WriteLine ("checkports: activity " + a.s + "\n");
    // first, check the declarations: each request/response handler has unique name and unique input type
    let hts = List.choose (fun ((hs,h):(string*Handler))->if h.rt=[] then None else Some(hs,h.at)) a.h in // all the req/resp handlers
    let hts = List.sortWith (fun (s1,t1) (s2,t2) -> Operators.compare s1 s2) hts in
    let e = list_firstdup (fun (hs1,ht1) (hs2,ht2) -> compare_compose [(Operators.compare hs1 hs2);(type_compare_unimp ht1 ht2)]) hts in
    (match e with None->() | Some(hs,ht) -> raise (new System.Exception("ERROR: " + a.s + ".on " + hs + "(" + (type_to_string ht) + ") is defined twice")) );
    System.Console.Out.WriteLine ("checkports: declarations okay\n");
    // second, for extern activities, check that we have no handlers and no state
    if (a.ext) then
    ( let e = List.tryPick (fun ((hs,h):(string*Handler)) -> if h.rt=[] then Some(hs) else None) a.h in
      (match e with None->() | Some(e)-> raise (new System.Exception("ERROR: extern handler " + a.s + ".on " + e + " defined; should be left abstract")) );
      if (List.length(a.state)>0) then raise (new System.Exception("ERROR: extern activity " + a.s + " has state")) else ();
      let e = List.tryPick (fun ((hs,h):(string*Handler)) -> if List.length(h.n)>0 then Some(hs) else None) a.h in
      (match e with None->() | Some(e)-> raise (new System.Exception("ERROR: handler " + a.s + ".on " + e + " implemented; should be left abstract")) );
      System.Console.Out.WriteLine ("checkports: extern okay\n");
    );
    // third, for implemented activities, check the handlers. (thanks to the above, the hts we construct will be empty for all extern activities)
    let hts = List.choose (fun ((hs,h):(string*Handler))->if h.rt=[] then Some(hs,h.at) else None) a.h in // all the notify-handlers
    let hts = List.sortWith (fun ((hs1,ht1):(string*Type)) ((hs2,ht2):(string*Type)) -> Operators.compare hs1 hs2) hts in
    let e = list_firstdup (fun (hs1,ht1) (hs2,ht2) -> compare_compose [(Operators.compare hs1 hs2);(type_compare_unimp ht1 ht2)]) hts in
    (match e with None->() | Some(hs,ht) -> raise (new System.Exception("ERROR: " + a.s + ".on " + hs + "(" + (type_to_string ht) + ") is defined twice.")) );
    let is_notify_generated (aname:string) (t:Type) = (let a = try_activity_in_prog aname p in match a with None -> false | Some a -> tl_contains_imp a.signal t) in
    let e = List.tryPick (fun (hs,ht) -> if (is_notify_generated hs ht) then None else Some(hs,ht)) hts in
    (match e with None->() | Some(hs,ht)-> raise (new System.Exception("ERROR: " + a.s + ".on " + hs + "(" + (type_to_string ht) + ") will never be triggered.")) );
    System.Console.Out.WriteLine ("checkports: handlers okay\n");
    // fourth, check the call-nodes.
    let n = List.concat (List.map (fun ((hs,h):(string*Handler))->h.n) a.h) in // gathers together every node in this activity
    let n = List.choose (fun n -> match n.d with NCall(peer,verb,nq,e) -> Some(peer,verb,n) | _ -> None) n in // pick out the call nodes
    let is_verb_declared peer verb = (let a = try_activity_in_prog peer p in match a with None->false | Some a -> List.exists (fun ((hs,h):(string*Handler))->hs=verb && List.length(h.rt)>0) a.h) in
    let e = List.tryPick (fun (peer,verb,n) -> if (is_verb_declared peer verb) then None else Some(peer,verb,n)) n in
    (match e with None->() | Some(peer,verb,n)-> raise (new System.Exception("ERROR: " + peer + "." + verb + " is invoked at line " + string(n.src) + " but not declared")) );
    System.Console.Out.WriteLine ("checkports: call-nodes okay\n");
    ()
  )
  //
  in
  List.iter check_activity p.a;
  p
    
  

// type_expr: given an expression and a type-environment, it returns
// a modified version of that expression in which all typefields have been
// filled in. (If there were any type-errors then it raises an exception).
// Note: this function also populates the "important" field of the types
// it fills in. For all constant expressions (void, int, bool, string, enum) it
// says their importance equals the importance-parameter passed to the function.
// For EIdPath, the importance is what EIdPath looks up in the environment.
// For functions on arguments, the importance is the maximal importance of all their arguments.
// Note: this function ignores+overwrites the existing type fields in the expr.
let rec type_expr (tp:TDefPrim list) (ef:ExternFun list) (env:TypeEnv) (basei:Importance) (e:Expr) : Expr =
  match e.e with
  | EVoid -> {new Expr with e=EVoid and t=[("void",basei)]}
  | EInt(i) -> {new Expr with e=EInt(i) and t=[("int",basei)]}
  | EBool(b) -> {new Expr with e=EBool(b) and t=[("bool",basei)]}
  | EString(s) -> {new Expr with e=EString(s) and t=[("string",basei)]}
  | EEnum(t,f) -> {new Expr with e=EEnum(t,f) and t=[(t,basei)]}
  | EImportance(i,sube) ->
       let sube:Expr = type_expr tp ef env basei sube in
       let subt:TypeList = List.map (fun (t,oldi)->(t,i)) sube.t in
       {new Expr with e=EImportance(i,sube) and t=subt}
  | EIdPath(idp) ->
    ( // varname.field.subfield...: we look up varname in the environment, get its type
      // and importance, then we drill down to find the types of its subfields
      let v = (List.head idp) in
      let tail = List.assoc v env in
      if List.length(tail)=0 then raise(new System.Exception("ERROR: field access of nullary type"));
      if List.length(idp)=1 then {new Expr with e=EIdPath(idp) and t=tail} else
      ( if List.length(tail)>1 then raise(new System.Exception("ERROR: field access of non-unique type"));
        let (t,i) = List.head tail in
        let tsub = type_drill_record tp t (List.tail idp) in
        match tsub with None -> raise(new System.Exception("ERROR: field access invalid in " + (idpath_to_string idp) + ".")) | Some tsub ->
        let idp = (List.head idp)::(idp_drill_record tp t (List.tail idp)) in // convert subfields to integers
        {new Expr with e=EIdPath(idp) and t=[(tsub,i)]}
      )
    )
  | EFun (f,es) ->
      // for functions, we check that the argument types are all unique and correct, and
      // figure out from the function name what its return-type is. The importance of the
      // result is the maximal importance of all args.
      if (f="==" || f="!=") then
      ( // equality/inequality is a special case: all types can be compared
        let es = List.map (type_expr tp ef env basei) es in
        match es with [e1;e2] ->
        ( let isokay = (tl_is_unique e1.t) && (tl_is_unique e2.t) && ((type_compare_unimp (List.head e1.t) (List.head e2.t))=0) in
          if (not isokay) then raise(new System.Exception("ERROR: (" + (expr_to_string e1) + ":" + (typelist_to_string e1.t) + ")" + f + "(" + (expr_to_string e2) + ":" + (typelist_to_string e2.t) + ") requires identical unique types"));
          let i = imp_max ((imps_of_tl e1.t)@(imps_of_tl e2.t)) in
          {new Expr with e=EFun(f,es) and t=[("bool",i)]}
        ) | _ -> raise(new System.Exception("ERROR: " + f + " requires two arguments; not (" + (exprs_to_string es) + ")"));
      )
      else let (es,rt) = (type_fun tp ef env f es false basei) in {new Expr with e=EFun(f,es) and t=rt}
    // for other functions (above) and external functions (below) we look up a table of
    // possible functions.
  | EExternal(f,es) -> let (es,rt) = (type_fun tp ef env f es true basei) in {new Expr with e=EExternal(f,es) and t=rt}
  | EConstructor(tname,afes) ->
      // for constructing a record, we make sure that all arguments are filled and all arguments
      // are of the correct type; we return the record type tname. Importance is the maximal
      // importance of all arguments.
      // Implementation: make a mutable list for the remaining arguments, and for each constructor
      // field look for an argument and remove it if it's there; if it's not there or is of the
      // wrong type then raise an exception; if any arguments are left then raise an exception
      let afes = List.map (fun ((f,e):(string*Expr))->(f,type_expr tp ef env basei e)) afes in // typed version of all field assignments
      if List.exists (fun ((f,e):(string*Expr)) -> not (tl_is_unique e.t)) afes then raise(new System.Exception("ERROR: an argument is not unique"));
      let afts = List.map (fun ((f,e):(string*Expr))->(f,List.head e.t)) afes in // pick out just the type of each argument
      let is = List.map (fun (f,t) -> let (t,i)=t in i) afts in // the importances of all arguments
      let afts = ref afts in // mutable list, as discussed above
      let prim = List.try_assoc tname tp in (match prim with
      | None -> raise(new System.Exception("ERROR: constructor " + tname + " not defined"))
      | Some (TRecord(cfs)) ->
        ( let check_field ((cf,ct):TField) = (
            let at = List.try_assoc cf (!afts) in match at with None -> raise(new System.Exception("ERROR: field " + cf + " undefined")) | Some (at,ai) ->
            if at<>ct then raise(new System.Exception("ERROR: field " + cf + " has type " + at + " but requires " + ct + "."));
            afts := List.remove_assoc cf (!afts); () ) in
          List.iter check_field cfs;
          if List.length(!afts)>0 then raise(new System.Exception("ERROR: constructor " + tname + " has excess arguments"));
          {new Expr with e=EConstructor(tname,afes) and t=[(tname,imp_max is)]}
        )
      | _ -> raise(new System.Exception("ERROR: " + tname + " is not a record-constructor")) )

// type_fun: this routine, given set of argument expressions and a function-name, first
// generates correct type-information in the arguments. Then finds a match based on
// both function-name and argument-types (for operator-overloading) and, if it finds one,
// returns the return-type. The return-importance is the maximum of all argument importances.
// If there were any type-errors (e.g. functional match not found) then it raises an exception.
// Note: each argument must have a unique type; not a "+" type.
and type_fun tp ef env f (es:Expr list) (ext:bool) (basei:Importance) : (Expr list * TypeList) =
   let builtins = [("+",["int";"int"],"int"); ("-",["int";"int"],"int"); ("*",["int";"int"],"int"); ("/",["int";"int"],"int");
                   ("<",["int";"int"],"bool"); (">",["int";"int"],"bool"); ("<=",["int";"int"],"bool"); (">=",["int";"int"],"bool");
                   ("&&",["bool";"bool"],"bool"); ("||",["bool";"bool"],"bool"); ("!",["bool"],"bool") ] in
   let externals = List.map (fun ((f,ef):ExternFun)->(f,List.map (fun (s,t)->t) ef.ats,ef.rt)) ef in
   let candidates = (if ext then externals else builtins) in
   let es = List.map (type_expr tp ef env basei) es in
   let i = imp_max (imps_of_es es) in
   if List.exists (fun (e:Expr) -> not (tl_is_unique e.t)) es then raise(new System.Exception("ERROR: function " + f + "(" + (exprs_to_string es) + ") has an ambiguous argument"));
   let ts = tnames_of_tl (tls_of_es es) in
   let candidates = List.choose (fun (cf,cargs,cr) -> if cf=f && (list_eq cargs ts) then Some cr else None) candidates in
   if List.length(candidates)=0 then raise(new System.Exception("ERROR: no match for function " + f + "(" + (exprs_to_string es) + ")"))
   else if List.length(candidates)>1 then raise(new System.Exception("ERROR: ambiguous match for function " + f + "(" + (exprs_to_string es) + ")"))
   else let cr=List.head candidates in
   (es, [(cr,i)])



// InferTypes: given a program that has been PostParsed and which has passed
// CheckServicePorts (so all call nodes point to declared handlers), 
// infers the rest of the types. Also checks for state-fields that initial-exprs match declared types
// Also, compiles "idpath" expression to now refer to subfields by index, so they're
// now of the form "wire.1.3.5". (This compilation requires us to know which type
// the thing has.)
// Also, fixes the "nq" field of NCall nodes
// Also calculates importance of IfType branches
// NB. This version of InferTypes goes through the nodes in sequence, and expects
// all incoming wires to have been already defined. This prevents wire loops. However,
// handler loops "activity test:T { on test(m:T) {signal m} }" are still allowed.
let InferTypes (p:Prog) : Prog =
   // infer_activity: just iterates through all handlers within the activity
   let rec infer_activity (a:Activity) =
      System.Console.Out.WriteLine ("infer: activity " + a.s + "\n");
      {new Activity with state=List.map (infer_state a) a.state and h=List.map (infer_handler a) a.h and s=a.s and signal=a.signal and ext=a.ext}
   // infer_state: calculates types of all initial-value expressions, and ensures that they match the declared types
   and infer_state (a:Activity) ((s,e):(string*Expr)) : (string*Expr) =
      System.Console.Out.WriteLine ("infer: state " + a.s + "." + s + "\n");
      let t = e.t in
      if not (tl_is_unique t) then raise(new System.Exception("ERROR: state variable " + a.s + "." + s + " declared with a non-unique type"));
      let e = type_expr p.tp p.ef [] Unimportant e in
      if not (tl_is_unique e.t) then raise(new System.Exception("ERROR: initial state " + a.s + "." + s + " evaluates to a non-unique type"));
      if (type_compare_imp (List.head t) (List.head e.t))<>0 then raise(new System.Exception("ERROR: state variable " + a.s + "." + s + " declared with type " + (typelist_to_string t) + " but expr " + (expr_to_string e) + " has type " + (typelist_to_string e.t) + "."));
      (s,e)
   // infer_handler: sets up an environment with the initial message then iterates through all nodes within the handler
   and infer_handler (a:Activity) ((hs,h):(string*Handler)) : (string*Handler) =
     System.Console.Out.WriteLine ("infer: handler " + a.s + ".on " + hs + "(" + (type_to_string h.at) + ")\n");
     let env:TypeEnv ref = ref [(h.a,[h.at])] in
     (hs, {new Handler with n=List.map (infer_node env a (hs,h) h.n) h.n and a=h.a and at=h.at and aq=h.aq and outWires=h.outWires and rt=h.rt} )
   // infer_node: does inference within the node data, then checks that the answer is
   // what was declared, and updates the environment.
   and infer_node (env:TypeEnv ref) (a:Activity) ((hs,h):(string*Handler)) (ns:Node list) (n:Node) : Node = 
     // first figure out the base importance arising from the inwires
     let intypes:TypeList = List.concat (List.choose (fun (w,wq)-> type_of_wire_in_handler w h) n.inWires) in
     let inimps:Importance list = List.map (fun (t,i)->i) intypes in
     let basei = imp_max inimps in 
     // now infer the type of the node
     let (t,d)=infer_ndata env a (hs,h) n basei n.d in
     if n.t<>TUnk && n.t<>t then raise (new System.Exception("ERROR: Node " + a.s + "." + hs + "." + n.s + " is declared " + (typelist_to_string n.t) + " but inferred " + (typelist_to_string t)));
     env := (n.s,t)::(!env);
     let d = (match d with NIf(ifs) -> NIf(List.map (infer_ifdat env) ifs) | _ -> d) in
     let n = {new Node with s=n.s and t=t and d=d and inWires=n.inWires and outWires=n.outWires and src=n.src} in
     System.Console.Out.WriteLine ("infer: line " + string(n.src) + ": " + (node_to_string n) + "\n");
     n
   //
   // type_of_wire: looks for a wire of this name in this handler; if it finds it, returns Some(typelist), otherwise None.
   and type_of_wire_in_handler (s:string) (h:Handler) : TypeList option =
      if h.a=s then Some [h.at] else List.tryPick (type_of_wire_in_node s) h.n
   and type_of_wire_in_node (s:string) (n:Node) : TypeList option =
      if n.s=s then Some n.t else (match n.d with
      | NIf(ifs) -> List.tryPick (fun id -> if id.thens=s then Some(id.thent) else None) ifs
      | NIfType(ifs) -> List.tryPick (fun id -> if id.tthens=s then Some(id.tthent) else None) ifs
      | _ -> None )
   //
   and infer_ifdat (env:TypeEnv ref) (id:IfDat) : IfDat = 
     if id.thent<>TUnk && id.thent<>id.thene.t then raise(new System.Exception("ERROR: conditional " + (ifdat_to_string id) + " is declared " + (typelist_to_string id.thent) + " but inferred " + (typelist_to_string id.thene.t) + "."));
     if (id.thens<>"") then env := (id.thens,id.thene.t)::(!env);
     {id with thent=id.thene.t}
   and infer_ndata env a (s,h) n basei nd : (TypeList*NData) = match nd with
     | NExpr(e) -> let e = type_expr p.tp p.ef (!env) basei e in (e.t,NExpr(e))
     | NCall(peer,verb,nq,e) -> // thanks to CheckServicePorts, we know the peer+verb exist
          let e = type_expr p.tp p.ef (!env) basei e in
          if not (tl_is_unique e.t) then raise(new System.Exception("ERROR: argument " + (expr_to_string e) + " to " + peer + "." + verb + " is not uniquely typed"));
          let t = List.head e.t in
          let (a:Activity) = activity_in_prog peer p in
          let (h:Handler) = handler_in_activity (verb,t) a in
          if (type_compare_imp t h.at)<>0 then raise(new System.Exception("ERROR: argument " + (expr_to_string e) + " to " + peer + "." + verb + " has type " + (type_to_string t) + " but should be " + (type_to_string h.at) + "."));
          (h.rt, NCall(peer,verb,h.aq,e)) // with h.aq we have fixed the NCall(nq) field.
     | NCallreturn(e) ->
          let e = type_expr p.tp p.ef (!env) basei e in (e.t,NCallreturn(e))
     | NSet(idp,tail,e) -> 
       (  let e = type_expr p.tp p.ef (!env) basei e in
          if not (tl_is_unique e.t) then raise(new System.Exception("ERROR: attempt to SET " + a.s + "." + (idpath_to_string idp) + " := " + (expr_to_string e) + " but the expr has non-unique type"));
          let state_name = List.head idp in
          let state_t = (List.assoc state_name a.state).t in 
          let (state_t,i) = List.head state_t in // by infer_state above, state_t is guaranteed to be unique
          let state_t = type_drill_record p.tp state_t (List.tail idp) in // drill down to the type of the field we're setting
          match state_t with None -> raise(new System.Exception("ERROR: subfields " + (idpath_to_string idp) + " not valid")) | Some state_t ->
          if (type_compare_unimp (state_t,i) (List.head e.t))<>0 then raise(new System.Exception("ERROR: attempt to SET " + a.s + "." + (idpath_to_string idp) + ":" + (type_to_string (state_t,i)) + " with expr of type " + (typelist_to_string e.t) + "."));
          (e.t, NSet(idp,e.t,e)) ) // we return the importance field of the input expr, not of the state.
     | NAtomic(ad) -> // we ensure that ad.rw is a state-variable with type "readwrite"
          let tail = List.try_assoc ad.rw a.state in
          ( match tail with None -> raise(new System.Exception("ERROR: " + (atomic_to_string ad.m) + "(" + ad.rw + ") but " + ad.rw + " isn't a state-variable of type 'readwrite'"));
          | Some tail -> let tail=tail.t in
          if (not (tl_is_unique tail)) || (not (tl_contains_unimp tail ("readwrite",Unimportant))) then raise(new System.Exception("ERROR: " + (atomic_to_string ad.m) + "(" + ad.rw + ") must be done on a 'readwrite', not a " + (typelist_to_string tail) + "."));
          ([("void",basei)], NAtomic(ad)) )
     | NGet(s,tail) ->
          let tail = (List.assoc s a.state).t in
          (tail, NGet(s,tail))
     | NFirst(ids) -> // we update the declared types of all ids, and our output is the union of them all
          let get_wire_tl s = let e = {new Expr with e=EIdPath([s]) and t=TUnk} in let e = type_expr p.tp p.ef (!env) basei e in e.t in
          let ids = List.map (fun (s,tail)->(s,get_wire_tl s)) ids in
          let tls = List.map (fun (s,tail)->tail) ids in
          let tail = tl_union (List.concat tls) in
          (tail, NFirst(ids))
     | NSignal(e) -> // we update the type, and check that type matches the declared output type of the activity
          let e = type_expr p.tp p.ef (!env) basei e in 
          if not (tl_is_unique e.t) then raise(new System.Exception("ERROR: attempt to signal a non-unique expr " + (expr_to_string e) + "."));
          let t = List.head e.t in
          if not (tl_contains_unimp a.signal t) then raise(new System.Exception("ERROR: signal " + (expr_to_string e) + " not an acceptable type for " + a.s + "." + (typelist_to_string a.signal) + "."));
          ([], NSignal(e))  // empty return type; no one can follow
     | NReturn(e) -> // we update the type, and check that the type matches declared output of handler
          let e = type_expr p.tp p.ef (!env) basei e in
          if not (tl_is_unique e.t) then raise(new System.Exception("ERROR: attempt to signal a non-unique expr " + (expr_to_string e) + "."));
          let t = List.head e.t in
          if not (tl_contains_unimp h.rt t) then raise(new System.Exception("ERROR: signal " + (expr_to_string e) + " not an acceptable type for " + a.s + "." + (typelist_to_string a.signal) + "."));
          ([], NReturn(e)) // empty return type; no one can follow
     | NIf(ifs) -> // we make sure that each "if" has boolean type. Then we figure the output types of the "thene" expressions.
          let do_ifcond (id:IfDat) : IfDat =
          ( let ife = type_expr p.tp p.ef (!env) basei id.ife in
            if not (tl_is "bool" ife.t) then raise(new System.Exception("ERROR: if branch " + (expr_to_string ife) + " isn't boolean"));
            let thene = type_expr p.tp p.ef (!env) basei id.thene in
            {id with ife=ife; thene=thene}
          ) in
          let ifs = List.map do_ifcond ifs in
          ([], NIf(ifs))
     | NIfType(ifs) -> // we fix up the types. This involves importance-escallation if necessary
          // for the ift-branches that have types, and "else" filling-in for the empty ift-branch,
          // and checking that each one can be fulfilled. We keep a reference "tall" which initially
          // lists all types that the input value can have, and which is whittled down. NB. by PostParse,
          // there's guaranteed to be at least one "if" in the list, and all branches are guaranteed to
          // have the same "ifv".
          let ifv = (List.head ifs).ifv in 
          let tall = List.try_assoc ifv (!env) in let tall = (match tall with Some(t)->t | None -> raise(new System.Exception("ERROR: iftype variable " + ifv + " not found")) )  in
          let tall = ref tall in // reference to it, for whittling down
          // we use this subfunction:
          let do_iftype (id:IfTypeDat) : IfTypeDat = 
           ( let ift = (if id.ift=[] then !tall else id.ift) in // the else-branch case
             let notfounds = List.choose (fun t -> if tl_contains_unimp (!tall) t then None else Some(t)) ift in
             if List.length(notfounds)>0 then raise(new System.Exception("ERROR: if (" + id.ifv + ":" + (typelist_to_string id.ift) + ") can never match " + (typelist_to_string notfounds) + "."));
             // now escalate the typenames that the user wrote
             let ift = List.map (fun (t,_) -> if tl_contains_imp (!tall) (t,Important) then (t,Important) else (t,Unimportant)) ift in
             // and remove them
             tall := List.choose (fun (t:Type) -> if tl_contains_unimp ift t then None else Some(t)) (!tall);
             // now type the "thene" branch given this information
             let localenv = (ifv,ift) :: (List.remove_assoc ifv (!env)) in
             let tthene = type_expr p.tp p.ef localenv basei id.tthene in
             {id with ift=ift; tthene=tthene}
           )
          in
          let ifs = List.map do_iftype ifs in
          ([], NIfType(ifs))
   in
   {new Prog with a=List.map infer_activity p.a and tp=p.tp and ef=p.ef}





let reachability (form:Form) (p:Prog) (gprint:ProgState->bool->unit) =
( // First, for convenience, make a flat list of all the signals we're going to exercise
  let signals = List.concat (List.map (fun (a:Activity)->if a.ext then List.map (fun t->(a.s,t)) a.signal else []) p.a) in
  // as we run we're going to log events in the profiler. The profiler will
  // keep track of which states have been reached. So first we initiate it with
  // the full set of state-assignments that we expect to reach.
  // Here for convenience is the flat list of all states
  let states:(string*TypeList)list = List.concat (List.map (fun (a:Activity)->List.map (fun ((s,e):State) -> a.s + "." + s,e.t) a.state) p.a) in
  printf "%A" states;
  let states:(string*TName)list = List.map (fun ((id,tail):(string*TypeList)) -> let (t,i)=List.head tail in (id,t)) states in
  printf "%A" states;
  let cover:(string*VPrim)list = List.concat (List.map (fun (id,t) -> allvprims_of_type p t id) states) in
  System.Console.Out.WriteLine ("FULL COVERAGE REMAINING: " + (coverage_to_string cover) + "\n");
  let cover = ref cover in // and that's the list of things that we have to cover
  let profiler (prof:ProfileEvent) =
  ( match prof with
    | PStateSet(svold,svnew,dep) -> cover := remove_assignment_from_allvprims p (svnew.s.a + "." + svnew.s.s, svnew.v) (!cover);
    | _ -> ()
  ) in
  // First we have to set as "covered" all the initial state assignments:
  let rec sub_state (a:string) ((s,e):State) =
  ( let v = Evaluate p.ef [] [] Unimportant e in
    cover := remove_assignment_from_allvprims p (a + "." + s,v.v) (!cover);
  )
  in List.iter (fun (a:Activity) -> List.iter (sub_state a.s) a.state) p.a;
  System.Console.Out.WriteLine ("COVERAGE REMAINING AFTER INIT: " + (coverage_to_string (!cover)) + "\n");
  //
  // Now we're going to go through that list. For each one, we start with
  // an empty initial-state in which all state variables have been set to "Any".
  // Then we insert an "Any" message and run through to completion. Once it's
  // all done we look at the coverage.
  let rec sub_signal ((a,t):(string*Type)) =
  ( let ps = MakeInitialState p true in
    let v = genericvalue_of_type p t in
    let ps = insert p ps gprint a v in
    explore "" p ps GMRemoveOrderingNondeterminism gprint (Some profiler); // runs through all possible execution traces
  ) in
  //
  List.iter sub_signal signals;
  let cover = !cover in
  // Now we're going to print all the states of the system, some in red if they
  // can't be reached.
  let rtb = new RichTextBox() in
  let rec msg_activity (a:Activity) =
  ( if (not a.ext) then
    ( rtb.SelectionColor <- Drawing.Color.Black;
      rtb.AppendText("Activity " + a.s + "\r\n");
      List.iter (msg_state a) a.state;
      rtb.AppendText("\r\n");
    )
  )
  and msg_state (a:Activity) ((s,e):State) =
  ( let prefix = a.s + "." + s in let plen=String.length(prefix) in
    let uncover = List.choose (fun (s,v) -> if String.length(s)>=plen && (String.sub s 0 plen)=prefix then Some(String.sub s plen (String.length(s)-plen),v) else None) cover in
    if List.length(uncover)=0 then
    ( rtb.SelectionColor <- Drawing.Color.Black;
      rtb.AppendText("  " + s + " = " + (expr_to_string e) + "\r\n");
    )
    else
    ( let uncover = List.map (fun (s,v)->if String.length(s)=0 then vprim_to_string v else "field" + s + "=" + (vprim_to_string v)) uncover in
      let uncover = list_to_string ", " uncover in
      rtb.SelectionColor <- Drawing.Color.Red;
      rtb.AppendText("  " + s + " = " + (expr_to_string e));
      rtb.AppendText("     // NEVER REACHES " + uncover + "\r\n");
    )
  ) in
  List.iter msg_activity p.a;
  // Now to display them.
  let f = new Form() in
  f.Owner <- form;
  f.Size <- new Drawing.Size(800,500);
  f.Text <- "States";
  rtb.Dock <- DockStyle.Fill;
  rtb.ReadOnly <- true;
  f.Controls.Add(rtb);
  let _ = f.ShowDialog() in
  ()
)

// make_pairwise_combinations: given a list, returns a
// new list with every pairwise combination. e.g. given
// [1;2;3] it returns [(1,1);(1,2);(1,3);(2,2);(2,3);(3,3)]
let rec make_pairwise_combinations (xs:'a list) : ('a*'a)list = match xs with [] -> []
  | (x::xs) -> (List.map (fun y -> (x,y)) (x::xs) ) @ (make_pairwise_combinations xs)


// Race detection.  
let race (form:Form) (p:Prog) (gprint:ProgState->bool->unit) =
( let rtb = new RichTextBox() in
  //
  let profiler (a:Activity) (sh1:string) (sh2:string) (scount:int ref) (prof:ProfileEvent) =
  ( match prof with
    | PStateSet(svold,svnew,dep) ->
      ( let msg = sh1 + "--" + sh2 + ". set " + (if svold.i=Important then "important " else "") + svold.s.a + "." + svold.s.s + "#" + string(svold.s.seq) + " = " + (vprim_to_string svnew.v) + "#" + string(svnew.s.seq) + " " + (dep_to_string dep) in
        let older_deps = List.choose (fun (dep:StateVersion)->if dep.a=svold.s.a && dep.s=svold.s.s && dep.seq<svold.s.seq then Some(dep) else None) dep in
        if svold.i=Unimportant || List.length(older_deps)=0 then rtb.AppendText(msg + "\r\n") else
        ( rtb.SelectionColor <- Drawing.Color.Red;
          let important_set_n = somenode_to_string svold.s.n in
          let overwrite_read_n = somenode_to_string (List.head older_deps).n in
          let overwrite_write_n = somenode_to_string svnew.s.n in
          rtb.AppendText(msg + "    // (1)read@" + overwrite_read_n + ", (2) imp.write@" + important_set_n + ", (3) overwrite@" + overwrite_write_n + "\r\n");
          rtb.SelectionColor <- Drawing.Color.Black;
        )
      )
    | PExplore(_) -> scount := (!scount)+1;
    | _ -> ()
  ) in
  //
  let rec race_activity (a:Activity) : unit = if not a.ext then
  ( // construct a new program in which all but this activity are external
    let activities = List.map (fun (a1:Activity)->if a1.s=a.s then a1 else {a1 with ext=true}) p.a in
    let p1 = {new Prog with a=activities and ef=p.ef and tp=p.tp} in
    // our task will be to try every pairwise combination of handlers.
    // here, let's make a list of them
    let hpairs = make_pairwise_combinations a.h in
    List.iter (race_handler_pair p1 a) hpairs;
  )
  and race_handler_pair (p1:Prog) (a:Activity) (((sh1,h1),(sh2,h2)):((string*Handler)*(string*Handler))) : unit =
  ( // We start the program with all variables set to "any" and all
    // readwriters set to (0,0). This will give a maximal set of possible traces.
    // And they really all are sensible traces (up to the limits of abstraction).
    // To justify the "readwriters set to (0,0)", suppose they weren't. Then suppose
    // some other activity finishes so that they reach (0,0). Then continue.
    let (ps:ProgState) = MakeInitialState p1 true in // make it using "Anys" for the state-variables
    let v1 = genericvalue_of_type p1 h1.at in 
    let v2 = genericvalue_of_type p1 h2.at in
    let mq = ps.mq in
    let mq = queue_add_main [{new MainPort with a=a.s and h=sh1 and ht=h1.at and nq=NUnbounded}] (v1,IgnoreReturn) mq in
    let mq = queue_add_main [{new MainPort with a=a.s and h=sh2 and ht=h2.at and nq=NUnbounded}] (v2,IgnoreReturn) mq in
    let ps = {ps with mq=mq} in
    // we're going to instantiate the two instances here. (we can't leave this
    // until later, because that may generate different instance numbers according
    // to when the ConsumeMove is done).
    let scount = ref 0 in // count the states we explore
    let moves = GenerateConsumeMoves ps GMRemoveAllNondeterminism in
    let ps = PerformMove p ps (List.head moves) ("i" + sh1 + ".") (Some(profiler a sh1 sh2 scount)) in
    let moves = GenerateConsumeMoves ps GMRemoveAllNondeterminism in
    let ps = PerformMove p ps (List.head moves) ("i" + sh2 + ".") (Some(profiler a sh1 sh2 scount)) in
    log ("** RACE " + sh1 + " vs " + sh2 + " **\n");
    explore "" p1 ps GMRemoveInessentialNondeterminism gprint (Some (profiler a sh1 sh2 scount));
    rtb.AppendText("RACE " + sh1 + " vs " + sh2 + ": explored " + string(!scount) + " states\n");
  )
  in
  //
  List.iter race_activity p.a;
  // Now to display them.
  let f = new Form() in
  f.Owner <- form;
  f.Size <- new Drawing.Size(800,500);
  f.Text <- "States";
  rtb.Dock <- DockStyle.Fill;
  rtb.ReadOnly <- true;
  f.Controls.Add(rtb);
  let _ = f.ShowDialog() in
  ()
)



// release detection -- is every handler guaranteed to release the locks it acquired?
// The strategy here is to exercise each handler in complete isolation.
// Start it off with (0,0) and check that it finishes with (0,0) for each of
// its readwrites. Actually, it won't be in true isolation. That's because one
// handler in the activity might call another handler in the same activity. 
// And imagine what if one handler acquired the lock but then called another handler
// which releases the lock. So, what we do is check only at each leaf of an explore tree.
// The datastructure of reports is like "activity.handler:type -> [(rw1@line16,(rcount,wcount));(rw2@line12,(rcount,wcount));...]"
let release (form:Form) (p:Prog) (gprint:ProgState->bool->unit) =
( let reports:(string*((string*(int*int))list))list ref = ref [] in 
  //
  let profiler (label:string) (prof:ProfileEvent) =
  ( match prof with
    | PExploreEnd(ps) -> 
      ( // let's pick out the readwrite states that we've ended with
        let pick_rw (sv:StateValue) : (string*(int*int)) option = if sv.t<>"readwrite" then None else
        ( let (rcount,rcountt) = drill_record_getvprim (sv.v,sv.t) ["0"] in // field-names have been compiled down to indexes
          let (wcount,wcountt) = drill_record_getvprim (sv.v,sv.t) ["1"] in
          let rcount = match rcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: rcount not an int")) in
          let wcount = match wcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: wcount not an int")) in
          let rwlabel = sv.s.a + "." + sv.s.s in
          let rwlabel = (match sv.s.n with None->rwlabel | Some n -> rwlabel + "@line" + string(n.src) ) in
          if rcount=0 && wcount=0 then None else Some (rwlabel, (rcount, wcount))
        )
        in
        let readwrites = List.choose pick_rw ps.s in
        // pick out the list of current maximums for each one
        let labmaxes = List.try_assoc label (!reports) in 
        let labmaxes = (match labmaxes with None -> [] | Some labmaxes -> labmaxes) in
        let labmaxes = labmaxes @ readwrites in 
        let labmaxes = List.sortWith (fun (rw1,_) (rw2,_) -> Operators.compare rw1 rw2) labmaxes in
        let labmaxes = list_merge (fun (rw1,(cr1,cw1)) (rw2,(cr2,cw2)) -> if rw1<>rw2 then None else Some(rw1,(max cr1 cr2,max cw1 cw2))) labmaxes in
        reports := (label,labmaxes) :: (List.remove_assoc label (!reports));         
      )
    | _ -> ()
  ) in
  //
  let rec release_activity (a:Activity) : unit = if not a.ext then
  ( // construct a new program in which all but this activity are external
    let activities = List.map (fun (a1:Activity)->if a1.s=a.s then a1 else {a1 with ext=true}) p.a in
    let p1 = {new Prog with a=activities and ef=p.ef and tp=p.tp} in
    List.iter (release_handler p1 a) a.h;
  )
  and release_handler (p1:Prog) (a:Activity) ((sh,h):(string*Handler)) : unit =
  ( let (ps:ProgState) = MakeInitialState p1 true in // make it using "Anys" for the state-variables
    let v = genericvalue_of_type p1 h.at in
    let mq = queue_add_main [{new MainPort with a=a.s and h=sh and ht=h.at and nq=h.aq}] (v,IgnoreReturn) ps.mq in
    let ps = {ps with mq=mq} in
    let label = a.s + "." + sh + ":" + (type_to_string h.at) in
    explore "" p1 ps GMRemoveOrderingNondeterminism gprint (Some (profiler label));
  )
  in
  //
  List.iter release_activity p.a;
  // Now to display them.
  let rtb = new RichTextBox() in
  let print_report (label, labmaxes) =
  ( if List.length(labmaxes)=0 then rtb.AppendText(label + " -- all readwrites released.\r\n")
    else
    ( rtb.SelectionColor <- Drawing.Color.Red;
      let labmaxes = List.map (fun (s,(rc,wc)) -> s + "(rcount=" + string(rc) + ",wcount=" + string(wc) + ")") labmaxes in
      let labmaxes = list_to_string ", " labmaxes in
      rtb.AppendText(label + " -- leaves unreleased " + labmaxes + "\r\n");
      rtb.SelectionColor <- Drawing.Color.Black;
    )
  ) in
  List.iter print_report (!reports);
  let f = new Form() in
  f.Owner <- form;
  f.Size <- new Drawing.Size(800,500);
  f.Text <- "Release";
  rtb.Dock <- DockStyle.Fill;
  rtb.ReadOnly <- true;
  f.Controls.Add(rtb);
  let _ = f.ShowDialog() in
  ()
)




// Cycle detection -- this finds potentially deadlocking cycles.
// It grossly overapproximates. That's because even the hint of inter-activity
// cycles is bad design. More precisely, imagine a graph where each node N
// corresponds to an (activity*handler*htype) triple. Draw a labelled edge
// N -{rw1,rw2,...}-> N' if N does a CALL(N') while potentially holding lock rw1.
// Also, write f(N)={rw1,rw2,...} for the set of all locks that N might acquire.
// If ever we find a path such that N -rw1-> ... -> N' and rw1 is in f(N')
// then this gives a warning. It means that while we hold a lock rw1, we go through
// some set of calls which might attempt to lock rw1 again.
// Implementation:
// (1) Form the graph through analysis of the program. We don't do any simulation.
// For calculating the -rw1-> edges, it's much like type-inference: the wires out
// of a "begin rw1" node carry the "type" rw1=1, and the "type" is decremented upon
// reaching "end rw1". That's how we tag edges. And the function f(N) is just by
// examining the nodes flatly.
// (2) Then we propogate the tags. Given N -rw1-> N' we add the mark (rw1,(0,N)) to N'.
// That means that rw1 has travelled a total distance of 0 to reach us here, and the most
// recent stop on the way was node N. If there are multiple routes into N' that would
// propogate rw1 then we pick one of the ones with the shortest distance.
// (3) If ever we get a node N where it has the thing marked through propogation, and
// also present in f(N), then this is an error.
// (4) For presenting the error message, we must list the starting N0 -rw1-> N1, and
// the subsequent nodes N2, N3, N4, such that f(N4) includes rw1. That's easy. We've
// found rw1 in f(N4) and also (rw1,(5,N3)) in marks(N4). So the trace we print will
// finish in N3,N4. Keep following the trace back in this way until it reaches the
// original edge. Actually, for better printouts, we should record each N with a
// line number of where it's declared, and each -rw1-> edge with a line number where
// the call is made, and each f(N)={rw1,rw2,...} with the line numbers where each one
// comes from.
// Optimization: 
// Work with integer indexes for nodes, and for "rw"s.
// CNode.f is sorted. So is CNode.marks.
type CNodeIndex = int
and  CRwIndex = int
and  CLineNum = int
and  CNode = {f:(CRwIndex*CLineNum) list; e:CEdge list; marks:CMark list}
and  CEdge = {f:(CRwIndex*CLineNum) list; dst:CNodeIndex; n:Node}
and  CMark = CRwIndex * (int*CNodeIndex*CLineNum)
//
let cmark_compare (crw1,_) (crw2,_) = compare crw1 crw2
let cmark_merge_fun ((crw1,(d1,src1,line1)):CMark) ((crw2,(d2,src2,line2)):CMark) : CMark option =
  if crw1<>crw2 then None else
  if d1<d2 then Some(crw1,(d1,src1,line1)) else Some(crw1,(d2,src2,line2))
let marks_merge (marks1:CMark list) (marks2:CMark list) : (CMark list * bool) =
  let marks = List.sortWith cmark_compare (marks1@marks2) in
  let marks = list_merge cmark_merge_fun marks in
  (marks, List.length(marks1)<>List.length(marks))
//
let cycle (form:Form) (p:Prog) (gprint:ProgState->bool->unit) =
( //
  // (some variables to accumulate the names of the nodes we'll build)
  let node_names:string list ref = ref [] in
  let rw_names:string list ref = ref [] in
  // First we construct the graph nodes
  let rec construct_activity (a:Activity) = (List.iter (construct_handler a) a.h; List.iter (construct_state a) a.state; )
  and construct_state (a:Activity) ((s,e):State) = ( let (t,i)=List.head e.t in
    if t="readwrite" then rw_names := (a.s + "." + s) :: (!rw_names); )
  and construct_handler (a:Activity) ((hs,h):(string*Handler)) = ( node_names := (a.s + "." + hs + ":" + (type_to_string h.at)) :: (!node_names); ) in
  // ... and tidy up the arrays we've just constructed.
  List.iter construct_activity p.a;
  let rw_names = List.mapi (fun i s -> (s,i)) (!rw_names) in
  let node_names = List.mapi (fun i s -> (s,i)) (!node_names) in
  let nnodes = List.length(node_names) in
  let nodes:CNode[] = Array.create nnodes {new CNode with f=[] and e=[] and marks=[]} in
  //
  // Now trace through each handler to find which locks it acquires, and where,
  // and which calls it makes while under which locks.
  let profiler (src:CNodeIndex) (prof:ProfileEvent) =
  ( match prof with
    | PCall(ps,peer,verb,t,n) -> 
      ( // let's pick out the readwrite states that we've ended with
        let pick_rw (sv:StateValue) : (CRwIndex*CLineNum) option = if sv.t<>"readwrite" then None else
        ( let (rcount,rcountt) = drill_record_getvprim (sv.v,sv.t) ["0"] in // field-names have been compiled down to indexes
          let (wcount,wcountt) = drill_record_getvprim (sv.v,sv.t) ["1"] in
          let rcount = match rcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: rcount not an int")) in
          let wcount = match wcount with VInt(i)->i | VAny->(-1) | _ -> raise(new System.Exception("RUNERROR: wcount not an int")) in
          let rwlabel = List.assoc (sv.s.a + "." + sv.s.s) rw_names in
          let rwlabel = (match sv.s.n with None->(rwlabel,-1) | Some n -> (rwlabel,n.src) ) in
          if rcount>0 || wcount>0 then Some(rwlabel) else None
        )
        in
        let readwrites = List.choose pick_rw ps.s in // has (CRwIndex*CLineNum) for every lock we held while making the call
        let dst = List.assoc (peer + "." + verb + ":" + (type_to_string t)) node_names in
        let cnode = nodes.[src] in
        let cedges = {new CEdge with f=readwrites and dst=dst and n=n} :: cnode.e in
        nodes.[src] <- {cnode with e=cedges};
      )
    | PAtomic(a,s,mode,rcount,wcount,n) -> if mode=BeginRead || mode=BeginWrite then
      ( let rwlabel = (List.assoc (a + "." + s) rw_names, n.src) in
        let cnode = nodes.[src] in
        let cf = rwlabel :: cnode.f in
        nodes.[src] <- {new CNode with f=cf and e=cnode.e and marks=cnode.marks};
      )
    | _ -> ()
  ) in
  //
  let rec findlock_activity (a:Activity) : unit = if not a.ext then
  ( // construct a new program in which all but this activity are external
    let activities = List.map (fun (a1:Activity)->if a1.s=a.s then a1 else {a1 with ext=true}) p.a in
    let p1 = {new Prog with a=activities and ef=p.ef and tp=p.tp} in
    List.iter (findlock_handler p1 a) a.h;
  )
  and findlock_handler (p1:Prog) (a:Activity) ((hs,h):(string*Handler)) : unit =
  ( let (ps:ProgState) = MakeInitialState p1 true in // make it using "Anys" for the state-variables
    let v = genericvalue_of_type p1 h.at in
    let mq = queue_add_main [{new MainPort with a=a.s and h=hs and ht=h.at and nq=h.aq}] (v,IgnoreReturn) ps.mq in
    let ps = {ps with mq=mq} in
    let label = List.assoc (a.s + "." + hs + ":" + (type_to_string h.at)) node_names in
    explore "" p1 ps GMRemoveOrderingNondeterminism gprint (Some (profiler label));
    // That has built up the labelled-edges out of each node, and the f() on each node
  )
  in
  List.iter findlock_activity p.a;
  //
  // Now propogate the marks
  let workqueue:CNodeIndex list ref = ref [] in
  // first stick in the edges
  let rec mark_initial ((s,src):(string*CNodeIndex)) = List.iter (mark_initial_edge src) nodes.[src].e
  and mark_initial_edge (src:CNodeIndex) (e:CEdge) =
  ( let newmarks = List.map (fun (frw,line) -> (frw,(0,src,line))) e.f in
    let cnode = nodes.[e.dst] in
    let (marks,changed) = marks_merge cnode.marks newmarks in
    if changed then ( nodes.[e.dst] <- {cnode with marks=marks}; workqueue := e.dst :: (!workqueue); )
  ) in
  List.iter mark_initial node_names;
  //
  while List.length(!workqueue)>0 do
  ( let src:CNodeIndex = List.head (!workqueue) in
    workqueue := List.tail (!workqueue);
    let cnode_src = nodes.[src] in
    let newmarks = cnode_src.marks in
    let mark_subsequent_edge (newmarks:CMark list) (src:CNodeIndex) (e:CEdge) =
    ( let cnode_dst = nodes.[e.dst] in
      let (marks,changed) = marks_merge cnode_dst.marks newmarks in
      if changed then ( nodes.[e.dst] <- {cnode_dst with marks=marks}; workqueue := e.dst :: (!workqueue); )
    ) in
    List.iter (mark_subsequent_edge newmarks src) cnode_src.e;
    workqueue := list_makeunique (List.sortWith compare (!workqueue)); // in case we'd put in duplicates
  )
  done;
  //
  // Now look for any nodes which have an rw listed in both marks and f
  let badmarks : (CNodeIndex*CRwIndex*(int*CNodeIndex*CLineNum)*CLineNum) list ref = ref [] in
  let bad_node ((s,n):(string*CNodeIndex)) =
  ( let cnode = nodes.[n] in
    let f = List.sortWith (fun (rw1,_) (rw2,_) -> compare rw1 rw2) cnode.f in
    let marks = List.sortWith (fun (rw1,_) (rw2,_) -> compare rw1 rw2) cnode.marks in
    let mergefun (rw1,line) (rw2,mark) = let c = compare rw1 rw2 in
      if c<>0 then (c,None) else (c,Some(n,rw1,mark,line)) in
    let bads = list_intersect mergefun f marks in
    List.iter (fun b -> badmarks := b :: (!badmarks) ) bads;
  ) in
  List.iter bad_node node_names;
  let badmarks = !badmarks in
  // 
  // Now trace back all those paths
  let rec trace (rw:CRwIndex) ((i,n,line):(int*CNodeIndex*CLineNum)) : (CNodeIndex list) = if i=0 then [n] else
  ( let cnode = nodes.[n] in
    let mark = List.assoc rw cnode.marks in
    n :: (trace rw mark)
  ) in
  let badmarks = List.map (fun (n,rw,(marki,markn,markline),line) -> (n,rw,markline,trace rw (marki,markn,markline),line)) badmarks in
  //
  // Now print an error message
  let rtb = new RichTextBox() in
  let print_report ((n,rw,linecall,mark,linelock):(CNodeIndex*CRwIndex*CLineNum*(CNodeIndex list)*CLineNum)) =
  ( // handler n makes call-chain "mark" from linecall while holding lock rw, but comes back to linelock/rw
    let (n,_) = List.nth node_names n in
    let (rw,_) = List.nth rw_names rw in
    let mark = List.map (fun n -> let (n,_) = List.nth node_names n in n) mark in
    let mark = list_to_string ", " mark in
    let linelock = string(linelock) in
    let linecall = string(linecall) in
    let msg = "@line" + linecall + " we acquire a lock on " + rw + ", and then do the callstack " + mark + ". But this winds up at " + n + "@line" + linelock + " which tries to lock " + rw + " again\r\n" in
    rtb.SelectionColor <- Drawing.Color.Red;
    rtb.AppendText(msg);
    rtb.SelectionColor <- Drawing.Color.Black;
  ) in
  List.iter print_report badmarks;
  if List.length(badmarks)=0 then rtb.AppendText("No circle problems detected.");
  let f = new Form() in
  f.Owner <- form;
  f.Size <- new Drawing.Size(800,500);
  f.Text <- "Release";
  rtb.Dock <- DockStyle.Fill;
  rtb.ReadOnly <- true;
  f.Controls.Add(rtb);
  let _ = f.ShowDialog() in
  ()
)
