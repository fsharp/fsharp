#indent "off"

module Ast

open Utils

// Types are references through typename.
// Primitive types - ints, enums, classes whose fields are other primitive types.
// The references "bool"->TBool, "int"->TInt, "string"->TString are built in, and
// all other primitive type definitions are done by the user with class/enum declarations
// Choice types - a union of (distinct) primitive types, referred to by name, eg. int+string
// Each one has an "important" flag to indicate whether it's bad for messages of that type
// to be lost. When evaluating an expression, the importance of the expression comes from
// the importance of the things that make it up. As for constants (bools, ints, ...),
// their importance equals the maximal importance of all the wires going into the node
type TName = string
and  TPrim = TVoid | TBool | TInt | TString | TRecord of TField list | TEnum of string list
and  TField = string*TName
and  TDefPrim  = TName * TPrim // e.g. "int"=TInt, "TFred"=TRecord(field1:int, field2:string)
and  TDefChoice = TName * TypeList // e.g. "TJones = T1 + (T2 important)
and  Importance = Important | Unimportant
and  Type = TName * Importance // where TName is the name of some primitive
and  TypeList = Type list
// In the code for activities, once types have been resolved, every "Type list" lists only primitive typenames;
// the choice-typenames have been resolved away.
let  TUnk:TypeList = [("?",Unimportant)]
type ExternFunDat = {ats:(string*TName) list; rt:TName} // argument types and return type
type ExternFun = string*ExternFunDat // string is the function-name
type TypeEnv = (string*TypeList)list


type Activity = {s:string; state:State list; signal:TypeList; h:(string*Handler) list; ext:bool}
and  State = string*Expr // string is the varname; expr is the initial value; expr.t is its type
and  Handler = {a:string; at:Type; aq:NQueue; outWires:Wire list; rt:TypeList; n:Node list} // if t is empty, then it has no return value; otherwise the return value is the choice of all these t's
and  NData = NExpr of Expr | NCall of string*string*NQueue*Expr | NCallreturn of Expr | NSet of IdPath*TypeList * Expr | NGet of string*TypeList
           | NFirst of (string*TypeList) list | NSignal of Expr | NReturn of Expr
           | NIf of IfDat list // the "t/outWires" fields of an NIf node are to be empty; that stuff is contained in the IfDat expressions
           | NIfType of IfTypeDat list // likewise
           | NAtomic of AtomicDat // for begin/end of read/write.
and  NQueue = NUnbounded | NBounded of int
and  Node = {s:string; t:TypeList; d:NData; inWires:Wire list; outWires:Wire list; src:int}
and  Wire = string*NQueue
and  IdPath = string list // e.g. "value.field.subfield"
and  Expr = {e:ExprDat; t:TypeList}
and  ExprDat = EVoid | EIdPath of IdPath | EFun of string * Expr list | EExternal of string * Expr list
          | EConstructor of TName * (string*Expr) list
          | EInt of int | EBool of bool | EString of string | EEnum of TName*string
          | EImportance of Importance*Expr
and  IfDat = {ife:Expr; thens:string; thent:TypeList; thene:Expr; thenOutWires:Wire list}
and  IfTypeDat = {ifv:string; ift:TypeList; tthens:string; tthent:TypeList; tthene:Expr; tthenOutWires:Wire list}
and  AtomicMode = BeginRead | EndRead | BeginWrite | EndWrite
and  AtomicDat = {m:AtomicMode; rw:string}
type Prog = {tp:TDefPrim list; ef:ExternFun list; a:Activity list}



let rec type_compare_unimp (n1,imp1) (n2,imp2) = Operators.compare n1 n2
and type_compare_imp (n1,imp1) (n2,imp2) = let c = Operators.compare n1 n2 in if c<>0 then c else imp_compare imp1 imp2
and tl_is n1 tail = match tail with [(n2,i2)]->(n1=n2) | _ -> false
and tl_isnt n1 tail = not (tl_is n1 tail)
and tl_contains_imp (tail:TypeList) (t2:Type) = List.exists (fun t1 -> (type_compare_imp t1 t2)=0) tail
and tl_contains_unimp (tail:TypeList) (t2:Type) = List.exists (fun t1 -> (type_compare_unimp t1 t2)=0) tail
and imp_compare imp1 imp2 = if imp1=imp2 then 0 else if imp1=Unimportant then -1 else 1
and imp_max imps = match imps with [] -> Unimportant | (i::imps) -> if i=Important then Important else imp_max imps
//
let tls_of_es (es:Expr list) = List.concat (List.map (fun (e:Expr)->e.t) es)
let imps_of_tl (tail:TypeList) = List.map (fun (t,i)->i) tail
let imps_of_es (es:Expr list) = imps_of_tl (tls_of_es es)
let rec list_eq (xs) (ys) = match (xs,ys) with ([],[]) -> true | (x::xs,y::ys) -> (x=y && (list_eq xs ys)) | _ -> false
let tnames_of_tl (tail:TypeList) = List.map (fun (t,i)->t) tail
let tl_is_unique (tail:TypeList) = List.length(tail)=1
let activity_in_prog (aname:string) (p:Prog) = List.find (fun (a:Activity) -> a.s=aname) p.a
let try_activity_in_prog (aname:string) (p:Prog) = List.tryFind (fun (a:Activity) -> a.s=aname) p.a
let handler_in_activity ((verb,t):(string*Type)) (a:Activity) : Handler = let (hs,h) = List.find (fun (hs,h) -> hs=verb && (type_compare_imp h.at t)=0) a.h in h
let node_in_handler (ns:string) (h:Handler) = List.find (fun (n:Node) -> n.s=ns) h.n
//
let rgen = new System.Random()
let randb() = if (rgen.Next(0,2) = 0) then false else true
let randi() = rgen.Next(0,20)
let rands() = let ss = ["fred";"bert";"arnie";"wooster";"wilbur";"navel";"grapefruit";"giraffe";"melon";"chair";"happy";"microsoft";"bender";"fry";"bjork";"carcinogen"] in List.nth ss (rgen.Next(0,List.length(ss)))



// type_drill_record: given a starting typename, and an idlist from it, drills down
let rec type_drill_record (tp:TDefPrim list) (t:TName) (idp:IdPath) : TName option =
  match idp with [] -> Some(t) | id::idp -> 
  let prim = List.try_assoc t tp in match prim with None -> None | Some prim ->
  match prim with
  | TRecord(fs) ->
    (  let tf = List.try_assoc id fs in match tf with None -> None | Some tf ->
       type_drill_record tp tf idp )
  | _ -> None
// idp_drill_record: given an field-referencing idpath, compiles it into an integer/index-referencing idpath
let rec idp_drill_record tp t idp : IdPath = match idp with [] -> [] | id::idp ->
  match (List.assoc t tp) with
  | TRecord(fs) ->
    (  let i = list_assoc_index_of fs id in
       let tf = List.assoc id fs in
       (string(i)) :: (idp_drill_record tp tf idp) )
  | _ -> raise (new System.Exception("ERROR: accessing field "^id^" of type "^t^" which isn't a record"))

   

 

let rec type_exemplar (tp:TDefPrim list) ((tname,i):Type) =
  let i = (if i=Important then "important " else "") in
  let t = List.try_assoc tname tp in
  match t with
  | None -> "(type "^tname^" not found)"
  | Some(TVoid) -> i^"void"
  | Some(TBool) -> if randb() then i^"true" else i^"false"
  | Some(TInt) -> i^string(randi())
  | Some(TString) -> i^rands()
  | Some(TEnum(ss)) -> i^tname^"."^(List.nth ss (rgen.Next(0,List.length(ss))))
  | Some(TRecord(fs)) ->
      let fs = List.map (fun (s,tname)->s^"="^(type_exemplar tp (tname,Unimportant))) fs in
      let fs = list_to_string ", " fs in
      i^"new "^tname^"("^fs^")"


let rec wire_compare_q ((s1,q1):Wire) ((s2,q2):Wire) = let c=Operators.compare s1 s2 in if c<>0 then c else queue_compare q1 q2
and queue_compare q1 q2 = match (q1,q2) with
  | (NUnbounded,NUnbounded) -> 0
  | (NUnbounded,_) -> 1
  | (_,NUnbounded) -> -1
  | (NBounded(i1),NBounded(i2)) -> compare i1 i2
and queue_max q = match q with [] -> raise (new System.Exception("max of empty queue list"))
  | [q1] -> q1
  | (NBounded(i1))::(NBounded(i2))::q -> if i1>i2 then queue_max ((NBounded(i1))::q) else queue_max ((NBounded(i2))::q)
  | q1::q2::q -> NUnbounded
and queue_min q = match q with [] -> raise (new System.Exception("min of empty queue list"))
  | [q1] -> q1
  | (NBounded(i1))::(NBounded(i2))::q -> if i1<i2 then queue_min ((NBounded(i1))::q) else queue_max ((NBounded(i2))::q)
  | b::NUnbounded::q -> queue_min (b::q)
  | NUnbounded::b::q -> queue_min (b::q)
and wire_unique_qmerge (t:Wire list) : (Wire list) = match t with [] -> [] | [t]->[t]
  | ((s1,q1)::(s2,q2)::t) -> let r = wire_unique_qmerge ((s2,queue_max [q1;q2])::t) in
    if s1=s2 then r else (s1,q1)::r


let rec tdefprim_to_string (n,t) = match t with
  | TRecord(fs) -> let fs = List.map (fun (s,t) -> t^" "^s) fs in
                "class "^n^" {"^(list_to_string ";" fs)^"}"
  | TEnum(e) -> "enum "^n^" {"^(list_to_string "," e)^"}"
  | TBool -> "type "^n^" = System.Bool"
  | TInt -> "type "^n^" = System.Int"
  | TString -> "type "^n^" = System.String"
  | TVoid ->  "type "^n^" = void"
and tdefchoice_to_string (n,ts) = "type "^n^" = "^(typelist_to_string ts)
and type_to_string (t,i) = match i with Important -> "important "^t | Unimportant -> t
and typelist_to_string ts = if ts=TUnk then "" else list_to_string " + " (List.map type_to_string ts)
and tdefprims_to_string tp = let s = list_to_string "\n" (List.map tdefprim_to_string tp) in if s="" then "" else s^"\n\n"
and tdefchoices_to_string tc = let s = list_to_string "\n" (List.map tdefchoice_to_string tc) in if s="" then "" else s^"\n"
and externfuns_to_string ef = let s = list_to_string "\n" (List.map externfun_to_string ef) in if s="" then "" else s^"\n\n"
and externfun_to_string (f,ef) = "extern "^f^"("^(list_to_string "," (List.map externfunarg_to_string ef.ats))^"):"^ef.rt
and externfunarg_to_string (s,t) = if s="_" then t else t^" "^s
and importance_to_string i = match i with Important -> "important" | Unimportant -> "unimportant"

let rec expr_to_string e = match e.e with
  | EVoid -> ""
  | EIdPath(idp) -> idpath_to_string idp
  | EFun(f,es) -> f^"("^(exprs_to_string es)^")"
  | EConstructor(t,fs) -> "new "^t^"("^(fieldexprs_to_string fs)^")"
  | EInt(i) -> string(i)
  | EBool(b) -> if b then "true" else "false"
  | EString(s) -> "\""^s^"\""
  | EExternal(f,es) -> "external "^f^"("^(exprs_to_string es)^")"
  | EEnum(t,f) -> t^"."^f
  | EImportance(i,e) -> (importance_to_string i)^" "^(expr_to_string e)
and fieldexpr_to_string (s,e) = s^"="^(expr_to_string e)
and fieldexprs_to_string fs = list_to_string ", " (List.map fieldexpr_to_string fs)
and exprs_to_string es = list_to_string "," (List.map expr_to_string es)
and idpath_to_string idp = list_to_string "." idp

let rec node_to_string n = match n.d with
  | NExpr(e) -> (nassign_to_string n)^(expr_to_string e)^(nwires_to_string n.outWires)
  | NCall(peer,verb,nq,e) -> (nassign_to_string n)^"call "^peer^"."^verb^"("^(expr_to_string e)^")"^(nwires_to_string n.outWires)
  | NCallreturn(e) -> (nassign_to_string n)^"callret("^(expr_to_string e)^")"^(nwires_to_string n.outWires)
  | NSet(idp,t,e) -> (nassign_to_string n)^"SET "^(idpath_to_string idp)^":="^(expr_to_string e)^(nwires_to_string n.outWires)
  | NGet(s,t) -> (nassign_to_string n)^"GET s"^(nwires_to_string n.outWires)
  | NFirst(ids) -> (nassign_to_string n)^"first("^(list_to_string "," (List.map fst ids))^")"^(nwires_to_string n.outWires)
  | NSignal(e) -> (nassign_to_string n)^"signal "^(expr_to_string e)^(nwires_to_string n.outWires)
  | NReturn(e) -> (nassign_to_string n)^"return "^(expr_to_string e)^(nwires_to_string n.outWires)
  | NIf(cs) -> (nassign_to_string n)^"if "^(ifdats_to_string cs)^(nwires_to_string n.outWires)
  | NIfType(ifs) -> (nassign_to_string n)^"if "^(iftypedats_to_string ifs)^(nwires_to_string n.outWires)
  | NAtomic(ad) -> (nassign_to_string n)^(atomic_to_string ad.m)^"("^ad.rw^")"^(nwires_to_string n.outWires)
and nassign_to_string n =
  let t = typelist_to_string n.t in let t = (if t="" then "" else t^" ") in
  let s = if n.s="" then "" else t^n.s^" = " in
  s^(wires_to_string n.inWires)
and nwires_to_string (ws:Wire list) = let s = wires_to_string ws in
  if s="" then "" else " -> "^s
and atomic_to_string (m:AtomicMode) = match m with BeginRead -> "begin_read" | EndRead -> "end_read" | BeginWrite -> "begin_write" | EndWrite -> "end_write"
and ifdat_to_string (id:IfDat) = 
  let t = typelist_to_string id.thent in let t = (if t="" then "" else t^" ") in
  let s = if id.thens="" then "" else t^id.thens^" = " in
  "("^(expr_to_string id.ife)^") then "^s^(expr_to_string id.thene)^(nwires_to_string id.thenOutWires)
and ifdats_to_string cs = list_to_string " else if " (List.map ifdat_to_string cs)
and iftypedat_to_string (id:IfTypeDat) =
  let t = typelist_to_string id.tthent in let t = (if t="" then "" else t^" ") in
  let s = if id.tthens="" then "" else t^id.tthens^" = " in
  "("^id.ifv^":"^(typelist_to_string id.ift)^") then "^s^(expr_to_string id.tthene)^(nwires_to_string id.tthenOutWires)
and iftypedats_to_string ifs = list_to_string " else if " (List.map iftypedat_to_string ifs)   
and wires_to_string ws = if List.length(ws)=0 then "" else "{"^(list_to_string "," (List.map wire_to_string ws))^"} "
and wire_to_string (s,q) = match q with NUnbounded -> "*"^s | NBounded(i) -> if i=1 then s else "["^string(i)^"]"^s
and nodes_to_string ns = let s = list_to_string "\n    " (List.map node_to_string ns) in if s="" then "" else s^"\n"

let rec activity_to_string (a:Activity) = 
  let states = (astates_to_string a.state) in 
  let states = (if states="" then "" else states^"\n") in
  let signal = (typelist_to_string a.signal) in 
  let signal = (if signal="" then "" else " : "^signal) in
  "activity "^a.s^signal^" {\n" ^ states ^(ahandlers_to_string a.h)^"}\n"
and astate_to_string (s,e) = "  "^(typelist_to_string e.t)^" "^s^" = "^(expr_to_string e)^"\n"
and astates_to_string states = list_to_string "" (List.map astate_to_string states)
and ahandler_to_string ((s,h):(string*Handler)) = 
  let rtype = if List.length(h.rt)=0 then "" else ":"^(typelist_to_string h.rt) in
  let outWires = wires_to_string h.outWires in
  let outWires = (if outWires="" then "" else " -> "^outWires) in
  "  on "^s^"("^(type_to_string h.at)^" "^h.a^outWires^")"^rtype^" {\n    "^(nodes_to_string h.n)^"  }\n" 
and ahandlers_to_string h = list_to_string "\n" (List.map ahandler_to_string h)
and activities_to_string a = list_to_string "\n" (List.map activity_to_string a)

let rec typeenv_to_string (env:TypeEnv) = "[" ^ (list_to_string "," (List.map typeenvv_to_string env)) ^ "]"
and typeenvv_to_string (s,tail) = s^":"^(typelist_to_string tail)


let rec prog_to_string (p:Prog) = (tdefprims_to_string p.tp) ^ (externfuns_to_string p.ef) ^ (activities_to_string p.a)





