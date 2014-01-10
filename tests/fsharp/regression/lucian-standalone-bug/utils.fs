#indent "off"

module Utils

open System

module String = begin

    let sub (s:string) (start:int) (len:int) =
        try s.Substring(start,len)
        with :? System.ArgumentException -> failwith "String.sub" 

end
// some utility functions. The "unique" ones assume that the list has already been sorted
module List = begin
    let indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException("An index satisfying the predicate was not found in the collection"))
    let rec assoc x l = 
        match l with 
        | [] -> indexNotFound()
        | ((h,r)::t) -> if x = h then r else assoc x t

    let rec try_assoc x l = 
        match l with 
        | [] -> None
        | ((h,r)::t) -> if x = h then Some(r) else try_assoc x t
    let rec mem_assoc x l = 
        match l with 
        | [] -> false
        | ((h,_)::t) -> x = h || mem_assoc x t

    let rec remove_assoc x l = 
        match l with 
        | [] -> []
        | (((h,_) as p) ::t) -> if x = h then t else p:: remove_assoc x t

    let rec contains x l = match l with [] -> false | h::t -> x = h || contains x t
    let mem x l = contains x l

    let combine x1 x2 =  List.zip x1 x2

end
exception Not_found = System.Collections.Generic.KeyNotFoundException

let rec list_assoc_index_of (xs:('a*'b)list) (y:'a) : int = let xs = List.mapi (fun i (xkey,xval) -> (xkey,i)) xs in List.assoc y xs
let rec list_to_string sep l = match l with [] -> "" | [a]->a | (a::b) -> a^sep^(list_to_string sep b)
let rec list_makeunique l = match l with []->[] | [a]->[a] | (a::b::l) -> if a=b then list_makeunique (b::l) else a::(list_makeunique (b::l))
let rec list_intersect f xs ys = match (xs,ys) with ([],_)->[] | (_,[]) -> [] | (x::sxs,y::sys) ->
  let (c,r)=f x y in if c<0 then list_intersect f sxs ys else if c>0 then list_intersect f xs sys else
  ( match r with None->list_intersect f sxs sys | Some r -> r :: (list_intersect f sxs sys) )
let rec list_merge mfn l = match l with []->[] | [a]->[a] | (a::b::l) -> let m=mfn a b in (match m with None -> a::(list_merge mfn (b::l)) | Some ab -> list_merge mfn (ab::l) )
let rec list_firstdup (fcomp:'a->'a->int) (l:'a list) = match l with []->None | [a]->None | (a::b::l) -> if (fcomp a b)=0 then Some(a) else list_firstdup fcomp (b::l)
let rec list_remove (x:'a) (ys:'a list) = match ys with [] -> raise Not_found | (y::ys) -> if x=y then ys else y::(list_remove x ys)
let rec compare_compose (cs:int list) = match cs with [] -> 0 | (c::cs) -> if c<>0 then c else compare_compose cs
let rec list_compare (f:'a->'a->int) (xs:'a list) (ys:'a list) : int = match (xs,ys) with ([],[])->0 | ([],_)-> -1 | (_,[])->1
  | (x::xs,y::ys) -> let c = f x y in if c<>0 then c else list_compare f xs ys
let rec is_list_sorted_uniq (f:'a->'a->int) (xs:'a list) : bool = match xs with []->true | [x]->true
  | (x::y::zs) -> let c = f x y in if c>=0 then false else is_list_sorted_uniq f (y::zs)
let fst (a,b)=a
let snd (a,b)=b


let appendlog = ref false
let log (s:string) =
  System.Console.Out.WriteLine s;
  let f = new IO.StreamWriter("log.txt",!appendlog) in
  let String s = box s in
  let s = s.Replace("\n","\r\n") in
  f.Write(s);
  f.Close();
  appendlog := true
