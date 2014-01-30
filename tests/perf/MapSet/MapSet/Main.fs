// Takes ~40mins to run with timeLimit=0.5

// Bugs in FSharp.Charting
// 3..300 gives crazy tick values like 299999.99998
// Null reference exception when chart is closed and reopened
// Crash when a log scale contains a zero
// Cannot close charts

// Find present/absent
// Sliding median could use set of elements and map from element to count == multiset
// Map<'a, Set<'a>> is a graph
// Subdivide floats? Array of integers?
// Index tree elements by in and out indices from DFS
// DIETs with stride

module Main

// Prim's MST and topological sort generate a significant amount (~17%) of
// long-lived garbage.

// Run all benchmarks and save the results.
do
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let results = Run.allBenchmarks()
  IO.save (IO.pathFromSourceDirectory["results"]) results
  printfn "Benchmarking took %fs" timer.Elapsed.TotalSeconds
