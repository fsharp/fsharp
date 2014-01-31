/// Find the first knight's tour (if any) on a board of a given
/// size.
///
/// 74% in Set.contains, 5% in Set.remove.
///
/// Heavy use of backtracking.
///
/// Most garbage is short lived.
module KnightsTour

// Takes ~10s.
let problemSize = 292

/// A 2D integer coordinate as a value type.
[<Struct>]
type Posn =
  val x : byte
  val y : byte
  new(x, y) = {x=x; y=y}
  new(x: int, y: int) = {x=byte x; y=byte y}
  static member (+) (a: Posn, b: Posn) = Posn(a.x+b.x, a.y+b.y)

/// The eight moves of a knight.
let moves f a =
  a
  |> f (Posn(-1, 2))
  |> f (Posn(1, 2))
  |> f (Posn(2, 1))
  |> f (Posn(2, -1))
  |> f (Posn(1, -2))
  |> f (Posn(-1, -2))
  |> f (Posn(-2, -1))
  |> f (Posn(-2, 1))

/// Recursively search for a valid knight's tour, yielding
/// solutions as they are found.
let rec search free ps xy =
  seq { if Set.isEmpty free then
          yield ps
        else
          let freedom xy dxy a =
            if Set.contains (xy + dxy) free then a+1 else a
          let aux dxy t =
            let p = xy + dxy
            if Set.contains p free then (moves (freedom p) 0, p)::t else t
          for _, p in List.sort (moves aux []) do
            yield! search (Set.remove p free) (p::ps) p }

let isSolvable n =
  let p = Posn(1, 1)
  let free =
    set
      [ for x in 1..n do
          for y in 1..n do
            if Posn(x, y) <> p then yield Posn(x, y) ]
  search free [p] p
  |> Seq.isEmpty
  |> not

let run() =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let _ =
    seq { for n in 1..10 do
            let p = Posn(1, 1)

            let free =
              set
                [ for x in 1..n do
                    for y in 1..n do
                      if Posn(x, y) <> p then yield Posn(x, y) ]

            yield! search free [p] p }
    |> Seq.truncate problemSize
    |> Seq.length
  timer.Elapsed.TotalSeconds
// Real: 00:00:09.714, CPU: 00:00:09.687, GC gen0: 375, gen1: 14, gen2: 0
