// FIXME: This file format sucks. Let's use grids in CSV instead.

/// Load and save benchmark results.
module IO

open Types

let saveResults path series =
  let lines =
    [ for name, seriesData in series do
        yield name
        for size, time in seriesData do
          yield sprintf "%d %f" size time ]
  System.IO.File.WriteAllLines(path, lines)

let saveTaskResults path tasks =
  let lines =
    [ for name, time in tasks do
        yield sprintf "%s %f" name time ]
  System.IO.File.WriteAllLines(path, lines)

let save dir (allResults: AllResults) =
  let path filename = System.IO.Path.Combine[|__SOURCE_DIRECTORY__; dir; filename+".csv"|]
  for name, results in allResults.Trends do
    saveResults (path(string name)) results
  saveTaskResults (path "Tasks") allResults.Tasks

let isAlpha c =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

let loadTrend path filename =
  let path = System.IO.Path.Combine[|path; filename+".csv"|]
  let lines = System.IO.File.ReadAllLines path
  let length =
    Seq.skip 1 lines
    |> Seq.findIndex (fun line -> line.Length > 0 && isAlpha line.[0])
    |> (+) 1
  [ for i in 0..lines.Length/length-1 ->
      lines.[length*i],
      [ for line in lines.[length*i+1..length*(i+1)-1] ->
          let numbers = line.Split ' '
          int numbers.[0], float numbers.[1] ] ]

let loadTasks path filename =
  let path = System.IO.Path.Combine[|path; filename+".csv"|]
  [ for line in System.IO.File.ReadLines path ->
      match line.Split ' ' with
      | [|name; time|] -> name, float time
      | _ -> failwith "Invalid tasks benchmark format" ]

let load dir : AllResults =
  let trends =
    [ for name in [ "IntMap"; "StringMap"; "PairMap";
                    "IntSet"; "StringSet"; "PairSet";
                    "SetUnion"; "SetIntersection"; "SetDifference" ] ->
        name, loadTrend dir name ]
  let tasks = loadTasks dir "Tasks"
  allBenchmarksOf trends tasks
(*
  { IntMap = loadTrend dir "IntMap"
    StringMap = loadTrend dir "StringMap"
    PairMap = loadTrend dir "PairMap"
    IntSet = loadTrend dir "IntSet"
    StringSet = loadTrend dir "StringSet"
    PairSet = loadTrend dir "PairSet"
    SetUnion = loadTrend dir "SetUnion"
    SetIntersection = loadTrend dir "SetIntersection"
    SetDifference = loadTrend dir "SetDifference"
    Tasks = loadTasks dir "Tasks" }
*)

let pathFromSourceDirectory path =
  [|yield __SOURCE_DIRECTORY__
    yield! path|]
  |> System.IO.Path.Combine
