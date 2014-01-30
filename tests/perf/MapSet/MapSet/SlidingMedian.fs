/// Sliding median of a sequence of floats using a Set of pairs as a
/// searchable priority queue.
/// 
/// Using F# Core Set: 52% in Set.remove, 36% in Set.add and
/// 8.5% in Set.count.
/// 
/// Requires: minElement, maxElement, count, add, remove and isEmpty.
///
/// The vast majority of garbage generated during this benchmark is
/// short lived.
module SlidingMedian

// Takes ~10s.
let problemSize = 225000

/// Pair of float value and int index.
// We really want to represent an ordered multiset as a Map from element
// to count but Map does not provide any way to obtain the largest key-value
// pair efficiently.
[<Struct>]
type Pair =
  val fst : float
  val snd : int
  new(fst, snd) = {fst=fst; snd=snd}

/// Balance a pair of sets.
let balance (lt, gt) =
  let dn = Set.count gt - Set.count lt
  if dn > 0 then
    let y = Set.minElement gt
    Set.add y lt, Set.remove y gt
  elif dn < -1 then
    let x = Set.maxElement lt
    Set.remove x lt, Set.add x gt
  else
    lt, gt

/// Calculate the median from a pair of sets.
let median (lt: Set<Pair>, gt: Set<Pair>) =
  let x = Set.maxElement lt
  if Set.count lt > Set.count gt then x.fst else
    (x.fst + (Set.minElement gt).fst) / 2.0

/// Add a new element into a pair of sets.
let add x (lt, gt) =
  if Set.isEmpty lt then lt, Set.add x gt
  elif x <= Set.maxElement lt then Set.add x lt, gt
  else lt, Set.add x gt
  |> balance

/// Remove an existing element from a pair of sets.
let remove x (lt, gt) =
  balance(Set.remove x lt, Set.remove x gt)

/// Calculate the sliding median of a sequence.
let slidingMedian n xs =
  let ys = ResizeArray()
  let window = System.Collections.Generic.Queue()
  let s = ref(Set.empty, Set.empty)
  for x in xs |> Seq.mapi (fun i x -> Pair(x, i)) do
    if window.Count < n then
      s := add x !s
      window.Enqueue x
      if window.Count = n then
        median !s |> ys.Add
    else
      let y = window.Dequeue()
      s := add x (remove y !s)
      median !s |> ys.Add
      window.Enqueue x
  ys.ToArray()

let rand = System.Random(3)
let xs = Array.init problemSize (fun _ -> rand.NextDouble())

let run() =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let ys = slidingMedian 100 xs
  timer.Elapsed.TotalSeconds
// Real: 00:00:10.520, CPU: 00:00:10.514, GC gen0: 419, gen1: 1, gen2: 0
