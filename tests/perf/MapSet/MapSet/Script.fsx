#load "Charts.fsx"

// Load the new results.
let newResults = IO.load (IO.pathFromSourceDirectory ["results"])

// Chart the new results.
Charts.showAndSave (IO.pathFromSourceDirectory["charts"]) newResults

// Once a set of baseline results have been established, copy the "results"
// directory to a "resultsBaseline" directory, rerun the above and then execute
// the following.

// Load the baseline results.
let originalResults = IO.load (IO.pathFromSourceDirectory ["resultsBaseline"])

// Chart the speedups relative to the baseline results.
Charts.showAndSaveSpeedups (IO.pathFromSourceDirectory["speedups"]) originalResults newResults
