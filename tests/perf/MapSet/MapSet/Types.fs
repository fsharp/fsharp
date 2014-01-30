/// General types pertaining to our benchmark suite.
module Types

/// Time taken as a function of size (number of elements).
type SeriesData = (int * float) list

/// Data for each benchmark.
type AllBenchmarks<'a, 'b> =
  { IntMap: 'a
    StringMap: 'a
    PairMap: 'a
    IntSet: 'a
    StringSet: 'a
    PairSet: 'a
    SetUnion: 'a
    SetIntersection: 'a
    SetDifference: 'a
    Tasks: 'b }

  member xs.Trends =
    [ "IntMap", xs.IntMap
      "StringMap", xs.StringMap
      "PairMap", xs.PairMap
      "IntSet", xs.IntSet
      "StringSet", xs.StringSet
      "PairSet", xs.PairSet
      "SetUnion", xs.SetUnion
      "SetIntersection", xs.SetIntersection
      "SetDifference", xs.SetDifference ]

let allBenchmarksOf trends tasks =
  let trends = Map trends
  { IntMap = trends.["IntMap"]
    StringMap = trends.["StringMap"]
    PairMap = trends.["PairMap"]
    IntSet = trends.["IntSet"]
    StringSet = trends.["StringSet"]
    PairSet = trends.["PairSet"]
    SetUnion = trends.["SetUnion"]
    SetIntersection = trends.["SetIntersection"]
    SetDifference = trends.["SetDifference"]
    Tasks = tasks }

/// Convert all benchmark data into a single sequence.
let seqOf (xs: AllBenchmarks<_, _>) =
  Seq.append xs.Trends ["Tasks", xs.Tasks]

/// Performance measurements for each benchmark.
type AllResults = AllBenchmarks<(string * SeriesData) list, (string * float) list>
