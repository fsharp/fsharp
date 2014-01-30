module Main

// Run all benchmarks and save the results.
do
  let timer = System.Diagnostics.Stopwatch.StartNew()
  let results = Run.allBenchmarks()
  IO.save (IO.pathFromSourceDirectory["results"]) results
  printfn "Benchmarking took %fs" timer.Elapsed.TotalSeconds
