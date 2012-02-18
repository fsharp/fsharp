open Microsoft.FSharp.Compiler.Interactive
open System.IO

let fileName = sprintf @"%s\script.fsx" __SOURCE_DIRECTORY__
// Note: printfn gets tied to the FSI writer so using sprintf to format and write to console
let writef format   = Printf.ksprintf System.Console.Write format
let writefn format  = Printf.ksprintf System.Console.WriteLine format

//let writef format     = Printf.ksprintf Debug.Write format
// Compiling to a DLL
(*let dll = "MyTest.dll"
let standardOpts =  [| "--noframework"; "-r:mscorlib.dll"; "-r:FSharp.Core.dll"; "-r:System.dll"; "-r:System.Core.dll"; |]
let srcCodeServices = new Runner.SimpleSourceCodeServices()
let argv = [| yield "fsc.exe"; yield "-o"; yield dll; yield "-a"; yield! standardOpts; yield fileName; |]
let errors, result = srcCodeServices.Compile argv
writefn "Errors - %A; Result - %A" errors result
writefn "Generated dll"*)

/// Compiling to output stream
let exe = "MyTest.exe"
let standardOpts =  [| "--noframework"; "-r:mscorlib.dll"; "-r:FSharp.Core.dll"; "-r:System.dll"; "-r:System.Core.dll"; |]
let srcCodeServices = new Runner.SimpleSourceCodeServices()
let argv = [| yield "fsc.exe"; yield "-o"; yield exe; yield! standardOpts; yield fileName; |]
let exec = true
let stdin, stdout = new Samples.ConsoleApp.CompilerInputStream(), new Samples.ConsoleApp.CompilerOutputStream()
let stdins, stdouts = (new StreamReader(stdin)), (new StreamWriter(stdout))
let streams =
    if exec then
        Some ((stdins :> TextReader), (stdouts :> TextWriter), (stdouts :> TextWriter))
    else None
let errors, result, assemblyOpt = srcCodeServices.CompileToDynamicAssembly (argv, streams)
stdouts.Flush()
writefn "Errors - %A" errors
let outs = stdout.Read()
writefn "Generated output..."
writefn "%A" outs

/// Running a FSI session
(*let standardOpts =  [| "--noframework"; "-r:mscorlib.dll"; "-r:FSharp.Core.dll"; "-r:System.dll"; "-r:System.Core.dll"; |]
let srcCodeServices = new Runner.SimpleSourceCodeServices()
let argv = [| yield "fsc.exe"; yield "-o"; yield! standardOpts; yield fileName; |]
let stdin, stdout = new Samples.ConsoleApp.CompilerInputStream(), new Samples.ConsoleApp.CompilerOutputStream()
let stdins, stdouts = (new StreamReader(stdin)), (new StreamWriter(stdout))
//let streams = Some ((stdins :> TextReader), (stdouts :> TextWriter), (stdouts :> TextWriter))
let fsi = new Runner.InteractiveConsole(argv,(stdins :> TextReader),(stdouts :> TextWriter), (stdouts :> TextWriter))
fsi.Run()
stdouts.Flush()
//printfn "Errors %A; Result %A; Generated Assembly %A" errors result assemblyOpt
let outs = stdout.Read()
writefn "%A" outs*)


System.Console.ReadKey(true) |> ignore