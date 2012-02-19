module Microsoft.FSharp.Compiler.Interactive.Runner

open System.Text
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Driver
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.ErrorLogger

let private buildFormatComment cmt (sb: StringBuilder) =
    match cmt with
    | XmlCommentText(s) -> sb.AppendLine(s) |> ignore
    | XmlCommentSignature _
    | XmlCommentNone -> ()

let private buildFormatElement isSingle el (sb: StringBuilder) =
    match el with
    | DataTipElementNone -> ()
    | DataTipElement(it, comment) ->
        sb.AppendLine(it) |> buildFormatComment comment
    | DataTipElementGroup(items) ->
        let items, msg =
          if items.Length > 10 then
            (items |> Seq.take 10 |> List.ofSeq),
              sprintf "   (+%d other overloads)" (items.Length - 10)
          else items, null
        if isSingle && items.Length > 1 then
          sb.AppendLine("Multiple overloads") |> ignore
        for (it, comment) in items do
          sb.AppendLine(it) |> buildFormatComment comment
        if msg <> null then sb.AppendFormat(msg) |> ignore
    | DataTipElementCompositionError(err) ->
        sb.Append("Composition error: " + err) |> ignore

// Convert DataTipText to string
let private formatTip tip =
    let sb = new StringBuilder()
    match tip with
    | DataTipText([single]) -> buildFormatElement true single sb
    | DataTipText(its) -> for item in its do buildFormatElement false item sb
    sb.ToString().Trim('\n', '\r')

type public InteractiveConsole(argv:string[],reader:System.IO.TextReader, writer:System.IO.TextWriter, error:System.IO.TextWriter) =
    do
        Microsoft.FSharp.Compiler.Interactive.Shell.inReader := reader
        Microsoft.FSharp.Compiler.Interactive.Shell.outWriter := writer
        Microsoft.FSharp.Compiler.Interactive.Shell.errorWriter := error
        Microsoft.FSharp.Core.Printf.setWriter writer
        Microsoft.FSharp.Core.Printf.setError error
    let session = Microsoft.FSharp.Compiler.Interactive.Shell.FsiEvaluationSession(argv)
    member x.Run() = session.Run()
    member x.Interrupt() = session.Interrupt()

type Declaration = {Name: string; Description: string}
type Position = {Line: int; Column: int}

// Export methods of TypeCheckInfo using an interface suited for C#
type TypeCheckResults internal (results:Microsoft.FSharp.Compiler.SourceCodeServices.TypeCheckResults,
                                source: string[]) = 

    let identToken = Microsoft.FSharp.Compiler.Parser.tagOfToken (Microsoft.FSharp.Compiler.Parser.IDENT "")
    member x.Errors = results.Errors

    member x.GetDeclarations(line, col, names, residue) =
        match results.TypeCheckInfo with
        | None -> [||]
        | Some (info: Microsoft.FSharp.Compiler.SourceCodeServices.TypeCheckInfo) ->
            let tag = identToken
            let items = info.GetDeclarations((line, col), source.[line], (names, residue), tag).Items
            [| for i in items -> {Name = i.Name; Description = formatTip i.DescriptionText} |]

    member x.GetF1Keyword(line, col, names) =
        match results.TypeCheckInfo with
        | None -> None
        | Some (info: Microsoft.FSharp.Compiler.SourceCodeServices.TypeCheckInfo) ->
            info.GetF1Keyword((line, col), source.[line], names)

(*
    member x.GetDeclarationLocation(line, col, names, tokenTag, isDecl) =
        match results.TypeCheckInfo with
        | None -> FindDeclResult.NoDeclInfo
        | Some (info: Microsoft.FSharp.Compiler.SourceCodeServices.TypeCheckInfo) ->
            info.GetDeclarationLocation ((line, col), source.[line], names, tokenTag, isDecl)
*)

    member x.GetDataTipText(line, col, names) =
        match results.TypeCheckInfo with
        | None -> ""
        | Some (info: TypeCheckInfo) ->
            info.GetDataTipText((line, col), source.[line], names, identToken) |> formatTip

    member x.GetDeclarationLocation(line: int, col: int, names, isDecl) =
        match results.TypeCheckInfo with
        | None -> FindDeclResult.NoDeclInfo
        | Some (info: TypeCheckInfo) ->
            info.GetDeclarationLocation((line, col), source.[line], names, identToken, isDecl)

    member x.FullResults = results

type public SimpleSourceCodeServices() =

    let filename = "example.fsx"
    let tokenizer = SourceTokenizer([], filename)
    let checker = InteractiveChecker.Create(fun _ -> ())
    let fileversion = 0
 
    // For colorization
    member x.TokenizeLine (line: string, state: int64) : TokenInformation[] * int64 = 
        let lineTokenizer = tokenizer.CreateLineTokenizer line
        let state = ref (None, state)
        let tokens = 
            [| while (state := lineTokenizer.ScanToken (snd !state); (fst !state).IsSome) do
                   yield (fst !state).Value |]
        tokens, snd !state 

    member x.TokenizeFile (source: string) : TokenInformation[][] = 
        let lines = source.Split('\n')
        let tokens = 
            [| let state = ref 0L
               for line in lines do 
                     let tokens, n = x.TokenizeLine(line, !state) 
                     state := n; 
                     yield tokens |]
        tokens

    /// For brace matching
    member x.MatchBraces (source: string) : (Range * Range) [] = 
        let options = { ProjectFileName="console.fsproj"; ProjectFileNames=[| filename |]; ProjectOptions=[| |]; IsIncompleteTypeCheckEnvironment=false; UseScriptResolutionRules=true }
        checker.MatchBraces(filename, source,  options)

    /// For errors, quick info, goto-definition, declaration list intellisense, method overload intellisense
    member x.TypeCheckSource (source:string, otherFlags: string[]) = 
#if FX_ATLEAST_40    
        begin 
            use file =  new System.IO.StreamWriter(System.IO.IsolatedStorage.IsolatedStorageFile.GetUserStoreForApplication().CreateFile(filename))
            file.Write source
        end;
#endif        
        //let options = checker.GetCheckOptionsFromScriptRoot(filename, source, otherFlags)
        let options = { ProjectFileName="console.fsproj"; ProjectFileNames=[| filename |]; ProjectOptions=otherFlags; IsIncompleteTypeCheckEnvironment=false; UseScriptResolutionRules=true }
        checker.StartBackgroundCompile options;
        // wait for the antecedent to appear
        checker.WaitForBackgroundCompile();
        // do an untyped parse
        let info = checker.UntypedParse(filename, source, options)
        // do an typecheck
        let typedInfo = checker.TypeCheckSource(info, filename, fileversion, source, options, IsResultObsolete (fun _ -> false))
        // return the info
        match typedInfo with 
        | NoAntecedant -> invalidOp "no antecedant"
        | Aborted -> invalidOp "aborted"
        | TypeCheckSucceeded res -> TypeCheckResults(res, source.Split('\n'))

    /// Compiles the given source under the given flags, to a Silverlight binary.  Any source files names 
    /// and DLL names are resolved from IsolatedStorage unless they already exist as resources in the
    /// Silverlight application. The output file name must be given by a -o flag, and will be written to
    /// isolated storage. 
    member x.Compile (argv: string[])  = 
        let errors = ResizeArray<_>()
        let errorSink warn exn = 
            let mainError,relatedErrors = Build.SplitRelatedErrors exn 
            let oneError trim e = errors.Add(ErrorInfo.CreateFromException (e, warn, trim, Range.range0))
            oneError false mainError
            List.iter (oneError true) relatedErrors
        let errorLogger = 
            { new ErrorLogger with 
                member x.WarnSink(exn) = errorSink true exn
                member x.ErrorSink(exn) = errorSink false exn
                member x.ErrorCount = errors |> Seq.filter (fun e -> e.Severity = Severity.Error) |> Seq.length }
      
        let result = 
            use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parse)            
            use unwindEL_2 = PushErrorLoggerPhaseUntilUnwind (fun _ -> errorLogger)
            use unwind = 
               let oldExiter = exiter 
               exiter <- { new Exiter with member x.Exit n = raise StopProcessing }
               { new System.IDisposable with member x.Dispose() = exiter <- oldExiter }

            try 
                mainCompile (argv, true, Some errorLogger); 
                0
            with e -> 
                stopProcessingRecovery e Range.range0
                1
        
        errors.ToArray(), result

    /// Compiles the given source under the given flags, to a Silverlight binary.  Any source files names 
    /// and DLL names are resolved from IsolatedStorage unless they already exist as resources in the
    /// Silverlight application. The output file name must be given by a -o flag, but this will not
    /// be written to isolated storage - instead a dynamic assembly will be created and loaded.
    member x.CompileToDynamicAssembly (otherFlags: string[], execute)  = 
        match execute with
        | Some (reader,writer,error) -> 
            Microsoft.FSharp.Compiler.Interactive.Shell.inReader := reader
            Microsoft.FSharp.Compiler.Interactive.Shell.outWriter := writer
            Microsoft.FSharp.Compiler.Interactive.Shell.errorWriter := error
            Microsoft.FSharp.Core.Printf.setWriter writer
            Microsoft.FSharp.Core.Printf.setError error
        | None -> ()
        let tcImportsRef = ref None
        let res = ref None
        tcImportsCapture <- Some (fun tcImports -> tcImportsRef := Some tcImports)
        dynamicAssemblyCreator <- 
            Some (fun (_tcConfig,ilGlobals,_errorLogger,outfile,_pdbfile,ilxMainModule,_signingInfo) ->
                let assemblyBuilder = System.AppDomain.CurrentDomain.DefineDynamicAssembly(System.Reflection.AssemblyName(System.IO.Path.GetFileNameWithoutExtension outfile),System.Reflection.Emit.AssemblyBuilderAccess.Run)
                let debugInfo = false
                let moduleBuilder = assemblyBuilder.DefineDynamicModule(filename,debugInfo)     
                let _emEnv,execs = 
                    Microsoft.FSharp.Compiler.AbstractIL.ILRuntimeWriter.emitModuleFragment 
                        ilGlobals 
                        Microsoft.FSharp.Compiler.AbstractIL.ILRuntimeWriter.emEnv0 
                        assemblyBuilder moduleBuilder 
                        ilxMainModule
                        debugInfo 
                        tcImportsRef.Value.Value.TryFindExistingFullyQualifiedPathFromAssemblyRef
                if execute.IsSome then 
                    for exec in execs do 
                        match exec() with 
                        | None -> ()
                        | Some exn -> raise exn
                for resource in ilxMainModule.Resources.AsList do 
                    if Build.IsReflectedDefinitionsResource resource then 
                        Quotations.Expr.RegisterReflectedDefinitions(assemblyBuilder, moduleBuilder.Name, resource.Bytes);
                res := Some assemblyBuilder)
            

        try 
            let errorsAndWarnings, result = x.Compile otherFlags
            let assemblyOpt = 
                match res.Value with 
                | None -> None
                | Some a ->  Some (a :> System.Reflection.Assembly)
            errorsAndWarnings, result, assemblyOpt
        finally
            tcImportsCapture <- None
            dynamicAssemblyCreator <- None

