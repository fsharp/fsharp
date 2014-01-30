// DONE:
//
// Remove all fsis
// Move all charting into script
// Trim samples: 1..10, 100, 1000, 10000, 100000, 1000000
// Add all operations from Set and Map

// TODO:
//
// Add index.html to output directory
// Create an overview chart with min and max speedups
//
// Performance of set-theoretic operations as proportion of overlap
// changes.

// SlidingMedian (2.7s), LZW (4.8s) and TopologicalSort (8.1s) seem to be
// significantly faster in FSI than when compiled.

// Performance varies considerably, perhaps as the GC optimizes its
// parameters.

#load "Charts.fsx"

// Load the baseline results.
let originalResults = IO.load (IO.pathFromSourceDirectory ["resultsBaseline"])

// Load the new results.
let newResults = IO.load (IO.pathFromSourceDirectory ["results"])

// Chart the new results.
Charts.showAndSave (IO.pathFromSourceDirectory["charts"]) newResults

// Chart the speedups relative to the baseline results.
Charts.showAndSaveSpeedups (IO.pathFromSourceDirectory["speedups"]) originalResults newResults
