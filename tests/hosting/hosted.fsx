

#r @"../../lib/release/4.0/FSharp.Compiler.dll"
#r @"../../lib/release/4.0/fsiAnyCpu.exe"

open Microsoft.FSharp.Compiler.Interactive.Shell

let stdinStream = new CompilerInputStream()
let stdin = new System.IO.StreamReader(stdinStream)

let stdoutStream = new CompilerOutputStream()
let stdout = new System.IO.StreamWriter(stdoutStream)

let stderrStream = new CompilerOutputStream()
let stderr = new System.IO.StreamWriter(stderrStream)

stdinStream.Add("printfn \"hello world\";;\n")
stdinStream.Add("let x = 1;;\n")
stdinStream.Add("printfn \"this is me, x = %d\" x;;\n\n")
stdinStream.Add("let x = 1;;\n")

let session = FsiEvaluationSession([| "fsiAnyCpu.exe" |], stdin, stdout, stderr)

// Start the session in the background
async { do session.Run() } |> Async.Start

session.Interrupt()

