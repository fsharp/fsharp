// TODO: Filter out zeroes because they cause FSharp.Charting to crash
// when using a log scale.

#r "System.Windows.Forms.DataVisualization"
#I "../packages/FSharp.Charting.0.90.5/lib/net40/"
#r "FSharp.Charting"

#load "NeighborShells.fs"
#load "PrimsMinimumSpanningTree.fs"
#load "SlidingMedian.fs"
#load "KnightsTour.fs"
#load "LZW.fs"
#load "TopologicalSort.fs"
#load "Types.fs"
#load "Run.fs"
#load "IO.fs"

open FSharp.Charting
open Types

open System.Windows.Forms
open FSharp.Charting.ChartTypes

let Show (c:GenericChart) =
  use cc = new ChartControl(c)
  cc.Dock <- DockStyle.Fill
  use f = new Form()
  f.Size <- System.Drawing.Size(1280, 720) // 720p
  f.Controls.Add cc
  f.ShowDialog() |> ignore

let Save filename (c:GenericChart) =
  use cc = new ChartControl(c)
  cc.Dock <- DockStyle.Fill
  use f = new Form()
  f.Size <- System.Drawing.Size(800, 600)
  f.Controls.Add cc
  f.Load |> Event.add (fun _ -> c.SaveChartAs(filename, ChartImageFormat.Png); f.Close()) // yay
  f.Show()
  //Application.Run f

/// Charts for each benchmark.
type AllCharts = AllBenchmarks<ChartTypes.GenericChart, ChartTypes.GenericChart>

let chartMapResults name (results: (string * SeriesData) seq) =
  (Chart.Combine
    [ for name, series in results ->
        Chart.Line(series, Name=name) ])
    .WithTitle(Text=name)
    .WithLegend(Enabled=true)
    .WithXAxis(Log=true, Title="Number of key-value pairs in Map", Max=1e6)
    .WithYAxis(Log=true, Title="Key-value pairs per second", Min=1e4, Max=1e9)

let chartSetResults name (results: (string * SeriesData) seq) =
  (Chart.Combine
    [ for name, series in results ->
        Chart.Line(series, Name=name) ])
    .WithTitle(Text=name)
    .WithLegend(Enabled=true)
    .WithXAxis(Log=true, Title="Cardinality", Max=1e6)
    .WithYAxis(Log=true, Title="Elements per second", Min=1e4, Max=1e9)

let chartSetTheoreticOp name (results: (string * SeriesData) seq) =
  (Chart.Combine
    [ for name, series in results ->
        Chart.Line(series, Name=name) ])
    .WithTitle(Text="Set "+name+" performance")
    .WithLegend(Enabled=true)
    .WithXAxis(Log=true, Title="Elements", Max=1e6)
    .WithYAxis(Log=true, Title="Operations per second")

let chartMapSpeedups name (results: (string * SeriesData) seq) =
  (Chart.Combine
    [ for name, series in results ->
        Chart.Line(series, Name=name) ])
    .WithTitle(Text=name)
    .WithLegend(Enabled=true)
    .WithXAxis(Log=true, Title="Number of key-value pairs in Map", Max=1e6)
    .WithYAxis(Title="Speedup")

let chartSetSpeedups name (results: (string * SeriesData) seq) =
  (Chart.Combine
    [ for name, series in results ->
        Chart.Line(series, Name=name) ])
    .WithTitle(Text=name)
    .WithLegend(Enabled=true)
    .WithXAxis(Log=true, Title="Cardinality", Max=1e6)
    .WithYAxis(Title="Speedup")

let chartSetTheoreticSpeedup name (results: (string * SeriesData) seq) =
  (Chart.Combine
    [ for name, series in results ->
        Chart.Line(series, Name=name) ])
    .WithTitle(Text="Set "+name+" speedup")
    .WithLegend(Enabled=true)
    .WithXAxis(Log=true, Title="Elements", Max=1e6)
    .WithYAxis(Title="Speedup")

let createCharts (allResults: AllResults) =
  { IntMap = chartMapResults "Map<int> performance" allResults.IntMap
    StringMap = chartMapResults "Map<string> performance" allResults.StringMap
    PairMap = chartMapResults "Map<int * int> map performance" allResults.PairMap

    IntSet = chartSetResults "Set<int> performance" allResults.IntSet
    StringSet = chartSetResults "Set<string> performance" allResults.StringSet
    PairSet = chartSetResults "Set<int * int> performance" allResults.PairSet

    SetUnion = chartSetTheoreticOp "union" allResults.SetUnion
    SetIntersection = chartSetTheoreticOp "intersection" allResults.SetIntersection
    SetDifference = chartSetTheoreticOp "difference" allResults.SetDifference

    Tasks = Chart.Column(allResults.Tasks, Title="Set and Map task performance").WithYAxis(Title="Time (s)") }

let createSpeedupCharts (allResults: AllResults) =
  { IntMap = chartMapSpeedups "Map<int> speedup" allResults.IntMap
    StringMap = chartMapSpeedups "Map<string> speedup" allResults.StringMap
    PairMap = chartMapSpeedups "Map<int * int> map speedup" allResults.PairMap

    IntSet = chartSetSpeedups "Set<int> speedup" allResults.IntSet
    StringSet = chartSetSpeedups "Set<string> speedup" allResults.StringSet
    PairSet = chartSetSpeedups "Set<int * int> speedup" allResults.PairSet

    SetUnion = chartSetTheoreticSpeedup "union" allResults.SetUnion
    SetIntersection = chartSetTheoreticSpeedup "intersection" allResults.SetIntersection
    SetDifference = chartSetTheoreticSpeedup "difference" allResults.SetDifference

    Tasks = Chart.Column(allResults.Tasks, Title="Set and Map task performance").WithYAxis(Title="Speedup") }

let checkNames = false

let compareResults (originalResults: AllResults) (newResults: AllResults) : AllResults =
  let seriesSpeedup (name1, series1) (name2, series2) =
    if checkNames && name1 <> name2 then
      failwithf "Series name mismatch: '%s' <> '%s'" name1 name2
    name1,
    [ for (size1, time1), (size2, time2) in Seq.zip series1 series2 ->
        if checkNames && size1 <> size2 then
          failwithf "Size mismatch: %d <> %d" size1 size2
        size1, time2 / time1 ]
  let taskSpeedup (name1, time1) (name2, time2) =
    if checkNames && name1 <> name2 then
      failwithf "Task name mismatch: '%s' <> '%s'" name1 name2
    name1, time2 / time1
  let trends =
    List.zip originalResults.Trends newResults.Trends
    |> List.map (fun ((name1, trend1), (name2, trend2)) ->
      if checkNames && name1 <> name2 then
        failwithf "Trend name mismatch: '%s' <> '%s'" name1 name2
      name1, List.map2 seriesSpeedup trend1 trend2)
  let tasks = List.map2 taskSpeedup originalResults.Tasks newResults.Tasks
  allBenchmarksOf trends tasks

let showAndSaveSpeedups path originalResults newResults =
  let speedups = compareResults originalResults newResults
  for filename, chart in seqOf(createSpeedupCharts speedups) do
    Save (System.IO.Path.Combine[|path; filename+".png"|]) chart
    //chart.ShowChart()
    //let format = ChartTypes.ChartImageFormat.Png
    //chart.SaveChartAs(System.IO.Path.Combine[|path; filename+".png"|], format)

let show results =
  for _, chart in seqOf(createCharts results) do
    Show chart
    //chart.ShowChart()

let showAndSave path results =
  for filename, chart in seqOf(createCharts results) do
    Save (System.IO.Path.Combine[|path; filename+".png"|]) chart
(*
    chart.ShowChart()
    let format = ChartTypes.ChartImageFormat.Png
    chart.SaveChartAs(System.IO.Path.Combine[|path; filename+".png"|], format)
*)
