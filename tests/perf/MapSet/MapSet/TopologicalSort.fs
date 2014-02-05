/// Compute the topological sort of a directed acyclic graph.
///
/// Using F# Core Map: 43% in Map.add, 16% in Seq.fold, 8% in
/// Map.remove, 7% in Map.tryFind and 6% in Set.ofSeq.
///
/// With a gen1 collection every 6 gen0 collections, this
/// benchmark is violating the generational hypothesis and generating
/// many objects that survive the nursery generation.
module TopologicalSort

// Takes ~10s.
let problemSize = 18

type TopologicalSort<'a> = Cyclic | Order of 'a seq

/// Find the destinations from the given source vertex, if any.
let find k map =
  match Map.tryFind k map with
  | Some v -> v
  | None -> Set.empty

let update x f (m: Map<_, Set<_>>) =
  let s = find x m |> f
  if Set.isEmpty s then Map.remove x m else Map.add x s m

let topologicalSort (V, E) =
  let rec loop xs (ins: Map<_, _>, outs: Map<_, _>, s) =
    if Set.isEmpty s then
      if Map.isEmpty outs then Order(List.rev xs) else Cyclic
    else
      let n = Set.minElement s
      ((ins, outs, Set.remove n s), find n outs)
      ||> Set.fold (fun (ins, outs, s) m ->
        let ins = update m (Set.remove n) ins
        ins, update n (Set.remove m) outs, if Map.containsKey m ins then s else Set.add m s)
      |> loop (n::xs)
  let ins, outs =
    ((Map.empty, Map.empty), V)
    ||> Seq.fold (fun (ins, outs) u ->
      Seq.fold (fun (ins, outs) v ->
        update v (Set.add u) ins, update u (Set.add v) outs)
        (ins, outs) (E u))
  let noParents = Map.foldBack (fun m _ -> Set.remove m) ins (set V)
  loop [] (ins, outs, noParents)

let add src dst map =
  Map.add src (Set.add dst (find src map)) map
let rec gen n p d m =
  if d=0 then Map.add p Set.empty m else
    let i, j = !n+1, !n+2
    n := !n + 2
    m
    |> add p i
    |> add p j
    |> gen n i (d-1)
    |> gen n j (d-1)

let generateGraph depth =
  let n = ref 1
  let E = gen n !n depth Map.empty
  let V = Set.unionMany[for KeyValue(i, js) in E -> Set.add i js]
  V, fun i -> E.[i]

let graphs = Array.init problemSize generateGraph

let run() =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let _ = Array.map topologicalSort graphs
  timer.Elapsed.TotalSeconds
// Real: 00:00:08.155, CPU: 00:00:08.268, GC gen0: 363, gen1: 58, gen2: 1
