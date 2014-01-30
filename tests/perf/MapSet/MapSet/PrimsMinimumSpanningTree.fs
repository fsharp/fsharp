/// Compute the minimum spanning tree of an undirected weighted graph.
///
/// Using F# Core Set and Map: 29% in Map.add, 20% in Map.tryFind,
/// 6% in Set.ofSeq, 6% in Map.remove, 6% in Set.remove and 4% in
/// Set.contains = 71% in Set and Map modules.
///
/// With a gen1 collection for every 1 to 6 gen0 collections, this
/// benchmark is generating lots of objects that don't die quickly
/// and survive generations, in violation of the generational
/// hypothesis.
module PrimsMinimumSpanningTree

// Takes ~10s.
let problemSize = 450

/// A priority queue implemented as a Map from priority to Sets of elements
/// with that priority.
module PriorityQueue =
  let contains d u q =
    match Map.tryFind d q with
    | None -> false
    | Some us -> Set.contains u us
    
  let find d q =
    defaultArg (Map.tryFind d q) Set.empty
    
  let addSet d us q =
    if Set.isEmpty us then Map.remove d q else Map.add d us q
    
  let tryExtractMin q =
    if Map.isEmpty q then None else
      let (KeyValue(d, us)) = Seq.head q
      let u = Set.minElement us
      Some(u, addSet d (Set.remove u us) q)
    
  let changeKey d d' u q =
    let q = addSet d (Set.remove u (Map.find d q)) q
    Map.add d' (Set.add u (find d' q)) q

/// Purely functional implementation of Prim's algorithm for computing
/// the minimum spanning tree of a weighted undirected graph.
let minimumSpanningTree (V: seq<'a>, E: 'a -> seq<'a * float>) =
  let find u key = defaultArg (Map.tryFind u key) infinity
  let rec loop (q, key, π) =
    match PriorityQueue.tryExtractMin q with
    | None -> Map.toList π
    | Some(u, q) ->
        ((q, key, π), E u)
        ||> Seq.fold (fun (q, key, π) (v, w) ->
          let vd = find v key
          if PriorityQueue.contains vd v q && w < find v key then
            PriorityQueue.changeKey vd w v q,
            Map.add v w key,
            Map.add v u π
          else
            q, key, π)
        |> loop
  let u = Seq.head V
  let q = Map[infinity, set V]
  let q = PriorityQueue.changeKey infinity 0.0 u q
  loop (q, Map[u, 0.0], Map.empty)

// Precompute an example graph that is square Manhattan grid
// with random edge weights.

open System.Collections.Generic

/// A 2D integer coordinate as a value type.
[<Struct>]
type Posn =
  val x : int
  val y : int
  new(x, y) = {x=x; y=y}

let V =
  [|for ux in 0..problemSize-1 do
      for uy in 0..problemSize-1 do
        yield Posn(ux, uy)|]

let E =
  let rand = System.Random 3
  let g = Dictionary(HashIdentity.Structural)
  for u in V do
    g.[u] <- ResizeArray()
  let add u v w =
    g.[u].Add(v, w)
    g.[v].Add(u, w)
  for u in V do
    for dx, dy in [-1,0; 0,1; 1,0; 0,-1] do
      let v = Posn(u.x+dx, u.y+dy)
      if 0<=v.x && v.x<problemSize && 0<=v.y && v.y<problemSize then
        if u.x<v.x || (u.x=v.x && u.y<v.y) then
          add u v (rand.NextDouble())
  fun u -> seq g.[u]

// Compute the minimum spanning tree of the example graph which
// is a random maze visiting every vertex.
let run() =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let _ = minimumSpanningTree (V, E)
  timer.Elapsed.TotalSeconds
// Real: 00:00:15.177, CPU: 00:00:15.272, GC gen0: 779, gen1: 133, gen2: 1
