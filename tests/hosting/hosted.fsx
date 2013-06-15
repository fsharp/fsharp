

#r @"../../lib/release/4.0/FSharp.Compiler.dll"
#r @"../../lib/release/4.0/fsiAnyCpu.exe"

open Microsoft.FSharp.Compiler.Interactive.Shell

open System
open System.IO
open System.Collections.Generic

let stdinStream = new CompilerInputStream()
let stdin = new System.IO.StreamReader(stdinStream)


let stdoutStream = new CompilerOutputStream()
let stdout = StreamWriter.Synchronized(new System.IO.StreamWriter(stdoutStream, AutoFlush=true))

let stderrStream = new CompilerOutputStream()
let stderr = StreamWriter.Synchronized(new System.IO.StreamWriter(stderrStream, AutoFlush=true))

System.Console.SetOut stdout
System.Console.SetError stderr

stdinStream.Add("eprintfn \"writing to error\";;\n")
stdinStream.Add("printfn \"hello world to out\";;\n")
stdinStream.Add("let x = 1;;\n")
stdinStream.Add("eprintfn \"this is me on error, x = %d\" x;;\n\n")
stdinStream.Add("printfn \"this is me on out, x = %d\" x;;\n\n")
stdinStream.Add("let x = 1;;\n")

let session = FsiEvaluationSession([| "fsiAnyCpu.exe" |], stdin, stdout, stderr)

// Start the session in the background
async { do session.Run() } |> Async.Start

session.Interrupt()

// Wait a bit before executing these lines
let text1 = stdoutStream.Read()
let text2 = stderrStream.Read()



