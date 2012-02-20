open Microsoft.FSharp.Compiler.Interactive
open Microsoft.FSharp.Compiler.SourceCodeServices
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
(*let exe = "MyTest.exe"
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
writefn "%A" outs*)

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

/// Intellisense
let standardOpts =  [| "--noframework"; "-r:FSharp.Core.dll"; "-r:System.dll"; "-r:System.Core.dll"; |]
let srcCodeServices = new Runner.SimpleSourceCodeServices()
let info = srcCodeServices.TypeCheckSource(File.ReadAllText(fileName),standardOpts)
let errorTxt =
    [ for x in info.Errors do
        yield sprintf "(%d.%d-%d.%d): %s" x.StartLine x.StartColumn x.EndLine x.EndColumn x.Message ]
writefn "%A" errorTxt

let code = File.ReadAllLines(fileName)

let tokenizeAndPrint lineNo md token =
    let col = match token with | Some(token) -> token.LeftColumn + 1 | None -> 0
    let dataTip = info.GetDataTipText(lineNo,col,[md])
    let decls   = info.GetDeclarations(lineNo,col,[md],"")
    let f1str   = info.GetF1Keyword(lineNo,col,[md])

    [   yield sprintf "-------------QUICK INFO--------------" 
        yield dataTip
        yield ""
        yield sprintf "-------------DOT-COMPLETION--------------"
        yield sprintf "Decls if you pressed '.' after this token: " 
        yield String.concat "," 
            [ for decl in decls -> decl.Name ]
        yield ""
        match f1str with
        | Some(f1str) ->
            yield sprintf "-------------F1 HELP--------------"
            yield sprintf "The F1 Help Moniker for MSDN is '%s'" f1str
            yield "" 
        | None -> yield ""
    ]
    |> List.iter (writefn "%s")

let token,_ = srcCodeServices.TokenizeLine(code.[0],0L)
let sqrToken = token.[2]
tokenizeAndPrint 0 "sqr" (Some(sqrToken))
tokenizeAndPrint 6 "Math" None

System.Console.ReadKey(true) |> ignore