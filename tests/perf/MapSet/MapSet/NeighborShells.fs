/// Compute nearest neighbor shells.
///
/// Using F# Core Set: 69% in Set.fold, 21% in Set.difference and
/// 5% in Set.add.
///
/// The vast majority of garbage is very short lived in this program.
module NeighborShells

// Takes ~10s.
let problemSize = 560

/// A 2D integer coordinate as a value type.
// Sets and Maps keyed on structs are much faster than keying on tuples.
[<Struct>]
type Posn =
  val x : int
  val y : int
  new(x, y) = {x=x; y=y}

/// Manhattan distance between two points on a grid.
let manhattan (p: Posn) =
  Set.singleton (Posn(p.x-1, p.y))
  |> Set.add (Posn(p.x+1, p.y))
  |> Set.add (Posn(p.x, p.y-1))
  |> Set.add (Posn(p.x, p.y+1))

/// Recursively search for the "n"th nearest neighbors of "i".
let nth i n =
  let rec loop s0 s1 = function
    | 0 -> s0
    | 1 -> s1
    | n ->
        // We really want to use Set.mapReduce here.
        let s2 = Set.unionMany (Seq.map manhattan s1) - s1 - s0
        loop s1 s2 (n-1)
  loop (Set.singleton i) (manhattan i) n

let run() =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let _ = nth (Posn(0, 0)) problemSize
  timer.Elapsed.TotalSeconds
// Real: 00:00:16.736, CPU: 00:00:16.738, GC gen0: 880, gen1: 1, gen2: 0
