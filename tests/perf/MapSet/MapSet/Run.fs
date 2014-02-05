module Run

open System
open Types

// Example Set and Map optimizations:
// Interestingly, adding extra check slows down the int * int sets and maps
// because comparison is so slow.
(*
module Map =
  let remove key map =
    if Map.containsKey key map then Map.remove key map else
      map

module Set =
  let add key set =
    if Set.contains key set then set else
      Set.add key set

  let remove key set =
    if Set.contains key set then Set.remove key set else
      set

  let difference s1 s2 =
    if Set.maxElement s1 < Set.minElement s2 then s1 else
      Set.difference s1 s2

  let intersect s1 s2 =
    if Set.maxElement s1 < Set.minElement s2 then Set.empty else
      Set.intersect s1 s2
*)

/// Samples are taken for this many seconds in order to obtain statistically-meaningful
/// results.
let timeLimit = 0.5

let sizes =
  set
    [ yield! {1..10}
      yield! [100; 1000; 10000; 100000; 1000000] ]

// We must be careful to pre-allocate reference types used in the
// benchmarks to avoid systematic error from GC stutter caused
// by allocations in the inner loops not related to Set or Map.
let strings = Array.init (2*sizes.MaximumElement + 1) string
let pairs = Array.init (2*sizes.MaximumElement + 1) (fun n -> n, n)

/// Keep applying f for "timeLimit" seconds and return the number
/// of calls per second.
let perSec f =
  //if true then 1.0 else
  let mutable n, i = 1, 0
  let timer = System.Diagnostics.Stopwatch.StartNew()
  while timer.Elapsed.TotalSeconds < timeLimit do
    for iter=1 to n do
      ignore(f())
      i <- i + 1
    n <- 2*n
  float i / timer.Elapsed.TotalSeconds

(*
/// Temper ints to simulate randomness.
let temper y =
  let y = y ^^^ (y >>> 11)
  let y = y ^^^ ((y <<< 7) &&& 0x9d2c5680)
  let y = y ^^^ ((y <<< 15) &&& 0xefc60000)
  y ^^^ (y >>> 18)
*)

/// Generate Maps of different sizes that the synthetic
/// benchmarks can use. Note that pre-allocating long-lived
/// collections in this way introduces systematic errors into the
/// synthetic benchmarks that may make them unprepresentative of
/// real programs.
let generateMaps() =
  let intMaps = ResizeArray()
  let stringMaps = ResizeArray()
  let pairMaps = ResizeArray()
  let mutable intMap = Map.empty
  let mutable stringMap = Map.empty
  let mutable pairMap = Map.empty
  for n in 0..sizes.MaximumElement do
    if sizes.Contains n then
      intMaps.Add(n, intMap)
      stringMaps.Add(n, stringMap)
      pairMaps.Add(n, pairMap)
    intMap <- Map.add n n intMap
    stringMap <- Map.add strings.[n] n stringMap
    pairMap <- Map.add pairs.[n] n pairMap
  intMaps, stringMaps, pairMaps

/// Generate Sets of different sizes that the synthetic
/// benchmarks can use.
let generateSets() =
  let intSets = ResizeArray()
  let stringSets = ResizeArray()
  let pairSets = ResizeArray()
  let mutable intSet = Set.empty
  let mutable stringSet = Set.empty
  let mutable pairSet = Set.empty
  for n in 0..sizes.MaximumElement do
    if sizes.Contains n then
      intSets.Add(n, intSet)
      stringSets.Add(n, stringSet)
      pairSets.Add(n, pairSet)
    intSet <- Set.add n intSet
    stringSet <- Set.add strings.[n] stringSet
    pairSet <- Set.add pairs.[n] pairSet
  intSets, stringSets, pairSets

let allBenchmarks() =
  let inline benchmarkMap name (maps: seq<int * Map<'a, 'b>>) (proj: int -> 'a) =
    /// Preallocate all of the key-value pairs.
    let arrayOfKeyValuePairs = Array.init (Seq.max sizes + 1) (fun i -> i, proj i)

    /// Preallocate lists so that we can benchmark Map.ofList without
    /// introducing systematic errors by allocating long lists on-the-fly.
    /// Reuses the key-value pairs from "arrayOfKeyValuePairs".
    let listsOfKeyValuePairs =
      (0, [])
      |> Seq.unfold (fun (n, xs) -> Some((n, xs), (n+1, arrayOfKeyValuePairs.[n]::xs)))
      |> Seq.filter (fun (n, xs) -> sizes.Contains n)
      |> Seq.truncate sizes.Count
      |> dict

    /// Apply "f" once to an entire "map". Used for O(n) functions.
    let inline once f =
      [ for size, map in maps ->
          size, float size * perSec (fun () -> f size map) ]

    /// Apply "f" to "size" keys that are not in "map". Used for
    /// O(1) or O(log n) functions.
    let inline forEachAfter f =
      [ for size, map in maps ->
          let test () =
            for i=0 to size-1 do
              ignore(f size (proj(i+size)) map)
          size, float size * perSec test ]

    /// Apply "f" to "size" keys that are in "map". Used for
    /// O(1) or O(log n) functions.
    let inline forEachIn f =
      [ for size, map in maps ->
          let test () =
            for i=0 to size-1 do
              ignore(f size (proj i) map)
          size, float size * perSec test ]

    [ "Map.add new", forEachAfter (fun _ key map -> Map.add key Unchecked.defaultof<_> map) // O(log n)
      "Map.add replacing", forEachIn (fun _ key map -> Map.add key Unchecked.defaultof<_> map) // O(log n)
      "Map.remove present", forEachIn (fun _ key map -> Map.remove key map) // O(log n)
      "Map.remove absent", forEachAfter (fun _ key map -> Map.remove key map) // O(log n)
      "Map.tryFind", forEachIn (fun _ key map -> Map.tryFind key map) // O(log n)
      "Map.containsKey", forEachIn (fun _ key map -> Map.containsKey key map) // O(log n)
      "Map.find", forEachIn (fun _ key map -> Map.find key map) // O(log n)
      "Map.isEmpty", forEachIn (fun _ key map -> Map.isEmpty map) // O(1)
      "Map.toSeq", forEachIn (fun _ key map -> Map.toSeq map) // O(1)
      "Map.exists", forEachIn (fun _ _ map -> Map.exists (fun _ _ -> true) map) // O(1) due to short-circuit evaluation
      "Map.filter", once (fun size map -> Map.filter (fun _ _ -> true) map) // O(n)
      "Map.findKey", forEachIn (fun _ _ map -> Map.findKey (fun _ _ -> true) map) // O(1) in this case
      "Map.fold", once (fun _ map -> Map.fold (fun count _ _ -> count + 1) 0 map) // O(n)
      "Map.foldBack", once (fun _ map -> Map.foldBack (fun _ _ count -> count + 1) map 0) // O(n)
      "Map.forAll", forEachIn (fun _ _ map -> Map.forall (fun _ _ -> false) map) // O(1) due to short-circuit evaluation
      "Map.iter", once (fun _ map -> Map.iter (fun _ _ -> ()) map) // O(n)
      "Map.map", once (fun _ map -> Map.map (fun _ value -> value) map) // O(n)
      // FIXME: Cannot preallocate 1,000,000 arrays with up to 1,000,000 elements in them because this
      // would require 4TB of storage but we don't want to allocate on-the-fly either...
      "Map.ofArray", once (fun size _ -> Map.ofArray(Array.sub arrayOfKeyValuePairs 0 size)) // O(n)
      "Map.ofList", once (fun size _ -> Map.ofList listsOfKeyValuePairs.[size]) // O(n)
      "Map.ofSeq", once (fun size map -> seq {for i in 1..size -> i, i} |> Map.ofSeq) // O(n)
      "Map.partition", once (fun size map -> Map.partition (fun key _ -> key < proj(size/2)) map) // O(n)
      "Map.pick", forEachIn (fun size _ map -> Map.pick (fun _ value -> Some value) map) // O(1) in this case
      "Map.toArray", once (fun _ map -> Map.toArray map) // O(n)
      "Map.toList", once (fun _ map -> Map.toList map) // O(n)
      "Map.tryFindKey", forEachIn (fun _ _ map -> Map.tryFindKey (fun _ _ -> true) map) // O(1) in this case
      "Map.tryPick", once (fun _ map -> Map.tryPick (fun key value -> None) map) ] // O(1) in this case

  printfn "Generating Maps..."
  let intMaps, stringMaps, pairMaps = generateMaps()
  printfn "Running Map<int, int> benchmarks..."
  let intMapResults = benchmarkMap "Int Map performance" intMaps (fun n -> n)
  printfn "Running Map<string, int> benchmarks..."
  let stringMapResults = benchmarkMap "String Map performance" stringMaps (fun n -> strings.[n])
  printfn "Running Map<int * int, int> benchmarks..."
  let pairMapResults = benchmarkMap "Pair Map performance" pairMaps (fun n -> pairs.[n])

  let inline benchmarkSet sets (proj: int -> 'a) =
    /// Preallocate lists so that we can benchmark Set.ofList without
    /// introducing systematic errors by allocating long lists on-the-fly.
    let listsOfElements =
      (0, [])
      |> Seq.unfold (fun (n, xs) -> Some((n, xs), (n+1, proj n::xs)))
      |> Seq.filter (fun (n, xs) -> sizes.Contains n)
      |> Seq.truncate sizes.Count
      |> dict

    /// Apply "f" once to an entire "set". Used for O(n) functions.
    let inline once f =
      [ for size, set in sets ->
          size, float size * perSec (fun () -> f size set) ]

    /// Apply "f" to "size" keys that are not in "map". Used for
    /// O(1) or O(log n) functions.
    let inline forEachAfter f =
      [ for size, set in sets ->
          let test () =
            for i=0 to size-1 do
              ignore(f size (proj(i+size)) set)
          size, float size * perSec test ]

    /// Apply "f" to "size" keys that are in "map". Used for
    /// O(1) or O(log n) functions.
    let inline forEachIn f =
      [ for size, set in sets ->
          let test () =
            for i=0 to size-1 do
              ignore(f size (proj i) set)
          size, float size * perSec test ]

    [ "Set.add new", forEachAfter (fun _ elt set -> Set.add elt set) // O(log n)
      "Set.add existing", forEachIn (fun _ elt set -> Set.add elt set) // O(log n)
      "Set.remove present", forEachIn (fun _ elt set -> Set.remove elt set) // O(log n)
      "Set.remove absent", forEachAfter (fun _ elt set -> Set.remove elt set) // O(log n)
      "Set.contains", forEachIn (fun _ elt set -> Set.contains elt set) // O(log n)
      "Set.count", once (fun _ set -> Set.count set) // O(n)
      "Set.isEmpty", forEachIn (fun _ elt set -> Set.isEmpty set) // O(1)
      //"Set.singleton", forEachIn (fun _ elt _ -> Set.singleton elt) // Doesn't accept a set
      "Set.exists", forEachIn (fun _ _ set -> Set.exists (fun _ -> true) set) // O(1) due to short-circuit evaluation
      "Set.filter", once (fun size set -> Set.filter (fun _ -> true) set) // O(n)
      "Set.fold", once (fun _ set -> Set.fold (fun count _ -> count + 1) 0 set) // O(n)
      "Set.foldBack", once (fun _ set -> Set.foldBack (fun _ count -> count + 1) set 0) // O(n)
      "Set.forAll", forEachIn (fun _ _ set -> Set.forall (fun _ -> false) set) // O(1) due to short-circuit evaluation
      "Set.iter", once (fun _ set -> Set.iter (fun _ -> ()) set) // O(n)
      "Set.map", once (fun _ set -> Set.map (fun value -> value) set) // O(n)
      "Set.maxElement", forEachIn (fun _ _ set -> Set.maxElement set) // O(log n)
      "Set.minElement", forEachIn (fun _ _ set -> Set.minElement set) // O(log n)
      // FIXME: Cannot preallocate 1,000,000 arrays with up to 1,000,000 elements in them because this
      // would require 4TB of storage but we don't want to allocate on-the-fly either...
      "Set.ofArray", once (fun size _ -> Set.ofArray(Array.init size (fun n -> proj n))) // O(n)
      "Set.ofList", once (fun size _ -> Set.ofList listsOfElements.[size]) // O(n)
      "Set.ofSeq", once (fun size set -> seq {for i in 1..size -> i} |> Set.ofSeq)
      "Set.partition", once (fun size set -> Set.partition (fun elt -> elt < proj(size/2)) set)
      "Set.toArray", once (fun _ set -> Set.toArray set)
      "Set.toList", once (fun _ set -> Set.toList set) ]

  printfn "Generating Sets..."
  let intSets, stringSets, pairSets = generateSets()
  printfn "Running Set<int> benchmarks..."
  let intSetResults = benchmarkSet intSets (fun n -> n)
  printfn "Running Set<string> benchmarks..."
  let stringSetResults = benchmarkSet stringSets (fun n -> strings.[n])
  printfn "Running Set<int * int> benchmarks..."
  let pairSetResults = benchmarkSet pairSets (fun n -> pairs.[n])

  printfn "Generating more Sets..."
  let moreIntSets =
    [|for size, set1 in intSets ->
        let set2 = Set.map (fun n -> n+size) set1
        let set3 = Set.map (fun n -> n+2*size) set1
        size, set1, set2, set3, Set.union set1 set2, Set.union set2 set3|]

  let inline benchSetTheoreticOp f =
    [ "Non-overlapping",
      [ for size, set1, set2, _, _, _ in moreIntSets ->
          size, perSec (fun () -> f set1 set2) ]
      "Related",
      [ for size, _, _, _, set1, set2 in moreIntSets ->
          size, perSec (fun () -> f set1 set2) ]
      "Unrelated",
      [ for size, _, _, _, set1, set2 in moreIntSets ->
          let set2 = Set.map id set2
          size, perSec (fun () -> f set1 set2) ] ]

  printfn "Running Set.union..."
  let setUnionResults = benchSetTheoreticOp (fun s1 s2 -> Set.union s1 s2)
  printfn "Running Set.intersection..."
  let setIntersectionResults = benchSetTheoreticOp (fun s1 s2 -> Set.intersect s1 s2)
  printfn "Running Set.difference..."
  let setDifferenceResults = benchSetTheoreticOp (fun s1 s2 -> Set.difference s1 s2)

  printfn "Running tasks..."
  let tasks =
    [ "Neighbors", NeighborShells.run
      "PrimMST", PrimsMinimumSpanningTree.run
      "SlidingMedian", SlidingMedian.run
      "KnightsTour", KnightsTour.run
      "LZW", LZW.run
      "TopoSort", TopologicalSort.run ]
    |> List.map (fun (name, run) ->
        // I have observed 50% variation in time for one run to the next so force a
        // GC to help prevent contamination from one benchmark to the next.
        System.GC.Collect()
        name, run())

  { IntMap = intMapResults
    StringMap = stringMapResults
    PairMap = pairMapResults
    IntSet = intSetResults
    StringSet = stringSetResults
    PairSet = pairSetResults
    SetUnion = setUnionResults
    SetIntersection = setIntersectionResults
    SetDifference = setDifferenceResults
    Tasks = tasks }
