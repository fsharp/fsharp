//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2011 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module internal Microsoft.FSharp.Compiler.Interactive.Shell

#nowarn "55"

[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: System.CLSCompliant(true)>]  
do()

open Internal.Utilities

module Tc = Microsoft.FSharp.Compiler.TypeChecker

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.IO
open System.Text
open System.Threading
open System.Reflection

#if SILVERLIGHT
#else
open System.Windows.Forms
#endif

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler.AbstractIL.ILRuntimeWriter 
open Microsoft.FSharp.Compiler.Interactive.Settings
#if SILVERLIGHT
#else
open Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers
#endif
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Fscopts
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Ilxgen
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.TypeChecker
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Opt
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.PostTypecheckSemanticChecks

open Internal.Utilities.Collections
open Internal.Utilities.StructuredFormat

#if SILVERLIGHT
let inReader = ref Unchecked.defaultof<System.IO.TextReader>
let outWriter = ref Unchecked.defaultof<System.IO.TextWriter>
let errorWriter = ref Unchecked.defaultof<System.IO.TextWriter>
let exit (_ : int) = ()
#endif


//----------------------------------------------------------------------------
// Hardbinding dependencies should we NGEN fsi.exe
//----------------------------------------------------------------------------

#if SILVERLIGHT
#else
open System.Runtime.CompilerServices
[<Dependency("FSharp.Compiler",LoadHint.Always)>] do ()
[<Dependency("FSharp.Core",LoadHint.Always)>] do ()

#endif // SILVERLIGHT

module Utilities = 
    let callStaticMethod (ty:Type) name args =
#if SILVERLIGHT
        ty.InvokeMember(name, (BindingFlags.InvokeMethod ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic), null, null, Array.ofList args)
#else       
        ty.InvokeMember(name, (BindingFlags.InvokeMethod ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic), null, null, Array.ofList args,Globalization.CultureInfo.InvariantCulture)
#endif        

    let callGenericStaticMethod (ty:Type) name tyargs args =
        let m = ty.GetMethod(name,(BindingFlags.InvokeMethod ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)) 
        let m = m.MakeGenericMethod(Array.ofList tyargs) 
        m.Invoke(null,Array.ofList args)

    let ignoreAllErrors f = try f() with _ -> ()


//----------------------------------------------------------------------------
// Timing support
//----------------------------------------------------------------------------
#if SILVERLIGHT
type FsiTimeReporter() =
    member tr.TimeOp(f) =
        let start = System.DateTime.Now
        let res = f()
        let stop = System.DateTime.Now
        printfn "Real: %s" ((stop - start).ToString())
        res

    member tr.TimeOpIf flag f = if flag then tr.TimeOp f else f ()
#else
[<AutoSerializable(false)>]
type FsiTimeReporter() =
    let stopwatch = new System.Diagnostics.Stopwatch()
    let ptime = System.Diagnostics.Process.GetCurrentProcess()
    let numGC = System.GC.MaxGeneration
    member tr.TimeOp(f) =
        let startTotal = ptime.TotalProcessorTime
        let startGC = [| for i in 0 .. numGC -> System.GC.CollectionCount(i) |]
        stopwatch.Reset()
        stopwatch.Start()
        let res = f ()
        stopwatch.Stop()
        let total = ptime.TotalProcessorTime - startTotal
        let spanGC = [ for i in 0 .. numGC-> System.GC.CollectionCount(i) - startGC.[i] ]
        let elapsed = stopwatch.Elapsed 
        printfn "%s" (FSIstrings.SR.fsiTimeInfoMainString((sprintf "%02d:%02d:%02d.%03d" elapsed.Hours elapsed.Minutes elapsed.Seconds elapsed.Milliseconds),(sprintf "%02d:%02d:%02d.%03d" total.Hours total.Minutes total.Seconds total.Milliseconds),(String.concat ", " (List.mapi (sprintf "%s%d: %d" (FSIstrings.SR.fsiTimeInfoGCGenerationLabelSomeShorthandForTheWordGeneration())) spanGC))))
        res

    member tr.TimeOpIf flag f = if flag then tr.TimeOp f else f ()

#endif

//----------------------------------------------------------------------------
// value printing
//----------------------------------------------------------------------------

type FsiValuePrinterMode = 
    | PrintExpr 
    | PrintDecl

type FsiValuePrinter(ilGlobals,generateDebugInfo,resolvePath) = 


    let DefaultPrintingIntercept (ienv: Internal.Utilities.StructuredFormat.IEnvironment) (obj:obj) = 
       match obj with 
       | null -> None 
       | :? System.Collections.IDictionary as ie ->
          let it = ie.GetEnumerator() 
          try 
              let itemLs = 
                  Internal.Utilities.StructuredFormat.LayoutOps.unfoldL // the function to layout each object in the unfold
                          (fun obj -> ienv.GetLayout obj) 
                          // the function to call at each step of the unfold
                          (fun () -> 
                              if it.MoveNext() then 
                                 Some((it.Key, it.Value),()) 
                              else None) () 
                          // the maximum length
                          (1+fsi.PrintLength/3) 
              let makeListL itemLs =
                (leftL "[") ^^
                sepListL (rightL ";") itemLs ^^
                (rightL "]")
              Some(wordL "dict" --- makeListL itemLs)
          finally
             match it with 
             | :? System.IDisposable as d -> d.Dispose()
             | _ -> ()
             
       | _ -> None 


    member __.GetFsiPrintOptions() = 
        { Internal.Utilities.StructuredFormat.FormatOptions.Default with 
              FormatProvider = fsi.FormatProvider;
              PrintIntercepts = 
                  // The fsi object supports the addition of two kinds of printers, one which converts to a string
                  // and one which converts to another object that is recursively formatted.
                  // The internal AddedPrinters reports these to FSI.EXE and we pick them up here to produce a layout
                  [ for x in fsi.AddedPrinters do 
                         match x with 
                         | Choice1Of2 (aty: System.Type, printer) -> 
                                yield (fun _ienv (obj:obj) ->
                                   match obj with 
                                   | null -> None 
                                   | _ when aty.IsAssignableFrom(obj.GetType())  ->  
                                       match printer obj with 
                                       | null -> None
                                       | s -> Some (wordL s) 
                                   | _ -> None)
                                   
                         | Choice2Of2 (aty: System.Type, converter) -> 
                                yield (fun ienv (obj:obj) ->
                                   match obj with 
                                   | null -> None 
                                   | _ when aty.IsAssignableFrom(obj.GetType())  -> 
                                       match converter obj with 
                                       | null -> None
                                       | res -> Some (ienv.GetLayout res)
                                   | _ -> None)
                    yield DefaultPrintingIntercept];
              FloatingPointFormat = fsi.FloatingPointFormat;
              PrintWidth = fsi.PrintWidth; 
              PrintDepth = fsi.PrintDepth; 
              PrintLength = fsi.PrintLength;
              PrintSize = fsi.PrintSize;
              ShowProperties = fsi.ShowProperties;
              ShowIEnumerable = fsi.ShowIEnumerable; }

    member __.GetEvaluationContext emEnv = 
        { LookupFieldRef = ILRuntimeWriter.LookupFieldRef emEnv >> Option.get
          LookupMethodRef = ILRuntimeWriter.LookupMethodRef emEnv >> Option.get
          LookupTypeRef = ILRuntimeWriter.LookupTypeRef emEnv >> Option.get
          LookupType = ILRuntimeWriter.LookupType { ilg = ilGlobals ; generatePdb = generateDebugInfo; resolvePath=resolvePath } emEnv }

    member __.PrintValue (printMode, opts:FormatOptions, x:obj, ty:System.Type) = 
        // We do a dynamic invoke of any_to_layout with the right System.Type parameter for the static type of the saved value.
        // In principle this helps any_to_layout do the right thing as it descends through terms. In practice it means
        // it at least does the right thing for top level 'null' list and option values (but not for nested ones).
        //
        // The static type was saved into the location used by RuntimeHelpers.GetSavedItType when RuntimeHelpers.SaveIt was called.
        // RuntimeHelpers.SaveIt has type ('a -> unit), and fetches the System.Type for 'a by using a typeof<'a> call.
        // The funny thing here is that you might think that the driver (this file) knows more about the static types
        // than the compiled code does. But it doesn't! In particular, it's not that easy to get a System.Type value based on the
        // static type information we do have: we have no direct way to bind a F# TAST type or even an AbstractIL type to 
        // a System.Type value (I guess that functionality should be in ilreflect.fs).
        //
        // This will be more significant when we print values other then 'it'
        //
        try 
            let ass = typeof<Internal.Utilities.StructuredFormat.Layout>.Assembly
            let displayModule = ass.GetType("Internal.Utilities.StructuredFormat.Display")
            match printMode with
              | PrintDecl ->
                  // When printing rhs of fsi declarations, use "fsi_any_to_layout".
                  // This will suppress some less informative values, by returning an empty layout. [fix 4343].
                  Internal.Utilities.StructuredFormat.Display.fsi_any_to_layout |> ignore; // if you adjust this then adjust the dynamic reference too            
                  Utilities.callGenericStaticMethod displayModule "fsi_any_to_layout" [ty] [box opts; box x] |> unbox<Internal.Utilities.StructuredFormat.Layout>
              | PrintExpr -> 
                  Internal.Utilities.StructuredFormat.Display.any_to_layout |> ignore; // if you adjust this then adjust the dynamic reference too            
                  Utilities.callGenericStaticMethod displayModule "any_to_layout" [ty] [box opts; box x] |> unbox<Internal.Utilities.StructuredFormat.Layout>             
        with 
        | :? ThreadAbortException -> Layout.wordL ""
        | e ->
#if DEBUG
          printf "\n\nPrintValue: x = %+A and ty=%s\n" x (ty.FullName)
#endif
          printf "%s" (FSIstrings.SR.fsiExceptionDuringPrettyPrinting(e.ToString())); 
          Layout.wordL ""
            
    member valuePrinter.InvokeDeclLayout (amap:Import.ImportMap) tcGlobals emEnv ilxGenEnv (v:Val) =
        // Bug 2581 requests to print declared values (rather than just expressions).
        // This code supports it by providing a lookup from v to a concrete (System.Object,System.Type).
        // This (obj,objTy) pair can then be fed to the fsi value printer.
        // Note: The value may be (null:Object).
        // Note: A System.Type allows the value printer guide printing of nulls, e.g. as None or [].
        //-------
        // Ilxgen knows what the v:Val was converted to w.r.t. AbsIL datastructures.
        // Ilreflect knows what the AbsIL was generated to.
        // Combining these allows for obtaining the (obj,objTy) by reflection where possible.
        // This assumes the v:Val was given appropriate storage, e.g. StaticField.
        if fsi.ShowDeclarationValues then 
            // Adjust "opts" for printing for "declared-values":
            // - No sequences, because they may have effects or time cost.
            // - No properties, since they may have unexpected effects.
            // - Limit strings to roughly one line, since huge strings (e.g. 1 million chars without \n are slow in vfsi).
            // - Limit PrintSize which is a count on nodes.
            let declaredValueReductionFactor = 10 (* reduce PrintSize for declared values, e.g. see less of large terms *)
            let opts   = valuePrinter.GetFsiPrintOptions()
            let opts   = {opts with ShowProperties  = false // properties off, motivated by Form props 
                                    ShowIEnumerable = false // seq off, motivated by db query concerns 
                                    StringLimit = max 0 (opts.PrintWidth-4) // 4 allows for an indent of 2 and 2 quotes (rough) 
                                    PrintSize = opts.PrintSize / declaredValueReductionFactor } // print less 
            let res    = 
                try  Ilxgen.LookupGeneratedValue amap (valuePrinter.GetEvaluationContext emEnv)  tcGlobals ilxGenEnv v
                with e -> assert(false);
#if DEBUG
                          dprintf "\nlookGenerateVal: failed on v=%+A v.Name=%s\n" v v.LogicalName
#endif
                          None (* lookup may fail *)
            match res with
              | None             -> None
              | Some (obj,objTy) -> let lay : layout = valuePrinter.PrintValue (FsiValuePrinterMode.PrintDecl, opts, obj, objTy)
                                    if isEmptyL lay then None else Some lay (* suppress empty layout *)
                                    
        else
            None
    
    member valuePrinter.InvokeExprPrinter denv vref = 
        let opts        = valuePrinter.GetFsiPrintOptions()
        let savedIt     = Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSavedIt()
        let savedItType = Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSavedItType()
        let rhsL  = valuePrinter.PrintValue (FsiValuePrinterMode.PrintExpr, opts, savedIt, savedItType)
        let denv = { denv with suppressMutableKeyword = true } // suppress 'mutable' in 'val mutable it = ...'
#if SILVERLIGHT
        let stdout = !outWriter
#else
#endif        
        let fullL = if isEmptyL rhsL then
                      Tastops.NicePrint.valL denv vref (* the rhs was suppressed by the printer, so no value to print *)
                    else
                      (Tastops.NicePrint.valL denv vref ++ wordL "=") --- rhsL
        Internal.Utilities.StructuredFormat.Display.output_layout opts stdout fullL;  
        stdout.WriteLine()
    


//----------------------------------------------------------------------------
// Reporting - syphon input text
//----------------------------------------------------------------------------


type FsiStdinSyphon() = 
    let syphonText = new StringBuilder()
#if DEBUG
//    let syphonDump() =
//        let text = syphonText.ToString()
//        let lines = text.Split(Array.ofList [ '\n' ])  
//        Array.iteri (fun i (s:string) -> dprintf "history %2d : %s\n" i s) lines
#endif

    member x.Reset () = 
        syphonText.Remove(0,syphonText.Length) |> ignore

    member x.Add (str:string) = // syphonDump();
        syphonText.Append(str) |> ignore  (* ; printf "syphon: %s\n" str *)

    member x.GetLine filename i =
        if filename<> Lexhelp.stdinMockFilename then 
            "" 
        else
            let text = syphonText.ToString()
            // In Visual Studio, when sending a block of text, it  prefixes  with '# <line> "filename"\n'
            // and postfixes with '# 1 "stdin"\n'. To first, get errors filename context,
            // and second to get them back into stdin context (no position stack...).
            // To find an error line, trim upto the last stdinReset string the syphoned text.
            //printf "PrePrune:-->%s<--\n\n" text;
            let rec prune (text:string) =
              let stdinReset = "# 1 \"stdin\"\n"
              let idx = text.IndexOf(stdinReset,StringComparison.Ordinal)
              if idx <> -1 then
                prune (text.Substring(idx + stdinReset.Length))
              else
                text
           
            let text = prune text
            //printf "PostPrune:-->%s<--\n\n" text;
            let lines = text.Split(Array.ofList [ '\n' ])
            if 0 < i && i <= lines.Length then lines.[i-1] else ""

//----------------------------------------------------------------------------
// Error reporting
//----------------------------------------------------------------------------

    member syphon.PrintError (tcConfig:TcConfigBuilder, isWarn, err) = 
        Utilities.ignoreAllErrors (fun () -> 
            DoWithErrorColor isWarn  (fun () ->
#if SILVERLIGHT
                let stderr = !errorWriter
#else
#endif                
                stderr.WriteLine();
                writeViaBufferWithEnvironmentNewLines stderr (OutputErrorOrWarningContext "  " syphon.GetLine) err; 
                writeViaBufferWithEnvironmentNewLines stderr (OutputErrorOrWarning (tcConfig.implicitIncludeDir,tcConfig.showFullPaths,tcConfig.flatErrors,tcConfig.errorStyle,false))  err;
                stderr.WriteLine()))


/// This ErrorLogger reports all warnings, but raises StopProcessing on first error or early exit
type ErrorLoggerThatStopsOnFirstError(tcConfigB:TcConfigBuilder,fsiStdinSyphon:FsiStdinSyphon) = 
    let mutable errors = 0 
    member x.SetError() = 
        errors <- 1
    member x.ErrorSinkImpl(err) = 
        fsiStdinSyphon.PrintError(tcConfigB,false,err)
        errors <- errors + 1;
        if tcConfigB.abortOnError then exit 1 (* non-zero exit code *)
        // STOP ON FIRST ERROR (AVOIDS PARSER ERROR RECOVERY)
        raise StopProcessing 
    
    member x.CheckForNoErrors() = (errors = 0)
    member x.ResetErrorCount() = (errors <- 0)
    member x.ErrorCount = errors 
    
    
    interface ErrorLogger with
        member x.WarnSink(err) = 
            DoWithErrorColor true (fun () -> 
                if ReportWarningAsError tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn tcConfigB.specificWarnAsError tcConfigB.specificWarnAsWarn tcConfigB.globalWarnAsError err then 
                    x.ErrorSinkImpl err 
                elif ReportWarning tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn err then 
#if SILVERLIGHT
                    let stderr = !errorWriter
#else
#endif                  
                    stderr.WriteLine();
                    writeViaBufferWithEnvironmentNewLines stderr (OutputErrorOrWarningContext "  " fsiStdinSyphon.GetLine) err; 
                    writeViaBufferWithEnvironmentNewLines stderr (OutputErrorOrWarning (tcConfigB.implicitIncludeDir,tcConfigB.showFullPaths,tcConfigB.flatErrors,tcConfigB.errorStyle,true)) err;
                    stderr.WriteLine())
        member x.ErrorSink err = x.ErrorSinkImpl err
        member x.ErrorCount =  errors

    /// A helper function to check if its time to abort
    member x.AbortOnError() = 
        if errors > 0 then 
#if SILVERLIGHT
            let stderr = !errorWriter
#else
#endif          
            eprintf "%s" (FSIstrings.SR.stoppedDueToError()); 
            stderr.Flush(); 
            raise StopProcessing 


//----------------------------------------------------------------------------
// cmd line - option state
//----------------------------------------------------------------------------

let directoryName (s:string) = 
    if s = "" then "."
    else 
        match Path.GetDirectoryName(s) with 
        | null -> if Path.IsPathRootedShim(s) then s else "."
        | res -> if res = "" then "." else res


   
//----------------------------------------------------------------------------
// printfs - user, error
//----------------------------------------------------------------------------

type FsiConsoleOutput(tcConfigB) = 

    let nullOut = new StreamWriter(Stream.Null) :> TextWriter
    let fprintfnn (os: TextWriter) fmt  = Printf.kfprintf (fun _ -> os.WriteLine(); os.WriteLine()) os fmt   
    /// uprintf to write usual responses to stdout (suppressed by --quiet), with various pre/post newlines
#if SILVERLIGHT
    let textWriter = !outWriter
#else
    let textWriter = System.Console.Out
#endif
    member out.uprintf    fmt = fprintf   (if tcConfigB.noFeedback then nullOut else textWriter) fmt 
    member out.uprintfn   fmt = fprintfn  (if tcConfigB.noFeedback then nullOut else textWriter) fmt
    member out.uprintfnn  fmt = fprintfnn (if tcConfigB.noFeedback then nullOut else textWriter) fmt
    member out.uprintnf   fmt = out.uprintfn ""; out.uprintf   fmt
    member out.uprintnfn  fmt = out.uprintfn ""; out.uprintfn  fmt
    member out.uprintnfnn fmt = out.uprintfn ""; out.uprintfnn fmt
      
    /// eprintf to write errors to stderr (not suppressable (yet))
    member out.eprintf fmt = out.eprintf fmt


//----------------------------------------------------------------------------
// cmd line - state for options
//----------------------------------------------------------------------------

type FsiCommandLineOptions(argv: string[], tcConfigB, fsiConsoleOutput: FsiConsoleOutput) = 
    let mutable enableConsoleKeyProcessing = 
       // Mono on Win32 doesn't implement correct console processing
       not (runningOnMono && System.Environment.OSVersion.Platform = System.PlatformID.Win32NT) 
    let mutable gui        = true // override via "--gui", on by default
#if DEBUG
    let mutable showILCode = false // show modul il code 
#endif
    let mutable showTypes  = true  // show types after each interaction?
    let mutable fsiServerName = ""
    let mutable interact = true
    let mutable explicitArgs = []

    let mutable inputFilesAcc   = []  

    let mutable fsiServerInputCodePage = None
    let mutable fsiServerOutputCodePage = None
    let mutable fsiLCID = None

    // internal options  
    let mutable probeToSeeIfConsoleWorks         = true (* Retail always on *)
    let mutable peekAheadOnConsoleToPermitTyping = true (* Retail always on. *)

    let isInteractiveServer() = fsiServerName <> ""  
    let recordExplicitArg arg = explicitArgs <- explicitArgs @ [arg]

    // Additional fsi options are list below.
    // In the "--help", these options can be printed either before (fsiUsagePrefix) or after (fsiUsageSuffix) the core options.

    let displayHelpFsi tcConfigB (blocks:CompilerOptionBlock list) =
        DisplayBannerText tcConfigB;
        printfn ""
        printfn "%s" (FSIstrings.SR.fsiUsage())
        printCompilerOptionBlocks blocks
        exit 0

    // option tags
//    let tagString      = "<string>"
    let tagFile        = "<file>"
    let tagNone        = ""
  
    /// These options preceed the FsiCoreCompilerOptions in the help blocks
    let fsiUsagePrefix tcConfigB =
      [PublicOptions(FSIstrings.SR.fsiInputFiles(),
        [CompilerOption("use",tagFile, OptionString (fun s -> inputFilesAcc <- inputFilesAcc @ [(s,true)]), None,
                                 Some (FSIstrings.SR.fsiUse()));
         CompilerOption("load",tagFile, OptionString (fun s -> inputFilesAcc <- inputFilesAcc @ [(s,false)]), None,
                                 Some (FSIstrings.SR.fsiLoad()));
        ]);
       PublicOptions(FSIstrings.SR.fsiCodeGeneration(),[]);
       PublicOptions(FSIstrings.SR.fsiErrorsAndWarnings(),[]);
       PublicOptions(FSIstrings.SR.fsiLanguage(),[]);
       PublicOptions(FSIstrings.SR.fsiMiscellaneous(),[]);
       PublicOptions(FSIstrings.SR.fsiAdvanced(),[]);
       PrivateOptions(
        [(* Make internal fsi-server* options. Do not print in the help. They are used by VFSI. *)
         CompilerOption("fsi-server","", OptionString (fun s -> fsiServerName <- s), None, None); // "FSI server mode on given named channel");
         CompilerOption("fsi-server-input-codepage","",OptionInt (fun n -> fsiServerInputCodePage <- Some(n)), None, None); // " Set the input codepage for the console"); 
         CompilerOption("fsi-server-output-codepage","",OptionInt (fun n -> fsiServerOutputCodePage <- Some(n)), None, None); // " Set the output codepage for the console"); 
         CompilerOption("fsi-server-no-unicode","", OptionUnit (fun () -> fsiServerOutputCodePage <- None;  fsiServerInputCodePage <- None), None, None); // "Do not set the codepages for the console");
         CompilerOption("fsi-server-lcid","", OptionInt (fun n -> fsiLCID <- Some(n)), None, None); // "LCID from Visual Studio"
         (* We do not want to print the "script.fsx arg2..." as part of the options *)     
         CompilerOption("script.fsx arg1 arg2 ...","",
                                 OptionGeneral((fun args -> args.Length > 0 && IsScript args.[0]),
                                               (fun args -> let scriptFile = args.[0]
                                                            let scriptArgs = List.tail args
                                                            inputFilesAcc <- inputFilesAcc @ [(scriptFile,true)]   (* record script.fsx for evaluation *)
                                                            List.iter recordExplicitArg scriptArgs            (* record rest of line as explicit arguments *)
                                                            tcConfigB.noFeedback <- true                      (* "quiet", no banners responses etc *)
                                                            interact <- false                                 (* --exec, exit after eval *)
                                                            [] (* no arguments passed on, all consumed here *)

                                               )),None,None); // "Run script.fsx with the follow command line arguments: arg1 arg2 ...");
        ]);
       PrivateOptions(
        [
         (* Private options, related to diagnostics around console probing *)
         CompilerOption("probeconsole","", OptionSwitch (fun flag -> probeToSeeIfConsoleWorks <- flag=On), None, None); // "Probe to see if System.Console looks functional");
         CompilerOption("peekahead","", OptionSwitch (fun flag -> peekAheadOnConsoleToPermitTyping <- flag=On), None, None); // "Probe to see if System.Console looks functional");
        ])
      ]

    /// These options follow the FsiCoreCompilerOptions in the help blocks
    let fsiUsageSuffix tcConfigB =
      [PublicOptions(FSComp.SR.optsHelpBannerInputFiles(),
        [CompilerOption("--","", OptionRest recordExplicitArg, None,
                                 Some (FSIstrings.SR.fsiRemaining()));
        ]);
       PublicOptions(FSComp.SR.optsHelpBannerMisc(),    
        [   CompilerOption("help", tagNone,                      
                                 OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks),None,
                                 Some (FSIstrings.SR.fsiHelp()))
        ]);
       PrivateOptions(
        [   CompilerOption("?"        , tagNone, OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks), None, None); // "Short form of --help");
            CompilerOption("help"     , tagNone, OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks), None, None); // "Short form of --help");
            CompilerOption("full-help", tagNone, OptionHelp (fun blocks -> displayHelpFsi tcConfigB blocks), None, None); // "Short form of --help");
        ]);
       PublicOptions(FSComp.SR.optsHelpBannerAdvanced(),
        [CompilerOption("exec","", OptionUnit (fun () -> interact <- false), None,
                                 Some (FSIstrings.SR.fsiExec()));
         CompilerOption("gui", tagNone, OptionSwitch (fun flag -> gui <- (flag = On)),None,
                                 Some (FSIstrings.SR.fsiGui()));
         CompilerOption("quiet","", OptionUnit (fun () -> tcConfigB.noFeedback <- true), None,
                                 Some (FSIstrings.SR.fsiQuiet()));     
         (* Renamed --readline and --no-readline to --tabcompletion:+|- *)
         CompilerOption("readline",tagNone, OptionSwitch (function flag -> enableConsoleKeyProcessing <- (flag = On)),None,
                                 Some (FSIstrings.SR.fsiReadline()));
        ]);
      ]


    //----------------------------------------------------------------------------
    // cmd line - parse options and process inputs
    //----------------------------------------------------------------------------


    /// Process command line, flags and collect filenames 
    /// The ParseCompilerOptions function calls imperative function to process "real" args 
    /// Rather than start processing, just collect names, then process them. 
    let sourceFiles = 
        let collect name = 
            let fsx = Build.IsScript name
            inputFilesAcc <- inputFilesAcc @ [(name,fsx)] (* O(n^2), but n small... *)
        try 
           let fsiCompilerOptions = fsiUsagePrefix tcConfigB @ GetCoreFsiCompilerOptions tcConfigB @ fsiUsageSuffix tcConfigB
           let abbrevArgs = abbrevFlagSet tcConfigB false
           ParseCompilerOptions collect fsiCompilerOptions (List.tail (PostProcessCompilerArgs abbrevArgs argv))
        with e ->
            stopProcessingRecovery e range0;
            stopProcessingRecovery e range0; 
            exit 1;
        inputFilesAcc

#if SILVERLIGHT
#else
    do 
        if tcConfigB.utf8output then
            let prev = System.Console.OutputEncoding
            System.Console.OutputEncoding <- System.Text.Encoding.UTF8
            System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> System.Console.OutputEncoding <- prev)

#endif
    do 
        let firstArg = 
            match sourceFiles with 
            | [] -> argv.[0] 
            | _  -> fst (List.head (List.rev sourceFiles) )
        let args = Array.ofList (firstArg :: explicitArgs) 
        fsi.CommandLineArgs <- args


    //----------------------------------------------------------------------------
    // Banner
    //----------------------------------------------------------------------------

    member __.ShowBanner() =
        fsiConsoleOutput.uprintnfn "%s" (tcConfigB.productNameForBannerText)
        fsiConsoleOutput.uprintfnn "%s" (FSComp.SR.optsCopyright())
        fsiConsoleOutput.uprintfn  "%s" (FSIstrings.SR.fsiBanner3())
     
    member __.ShowHelp() =
#if SILVERLIGHT
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader1directives())
        fsiConsoleOutput.uprintfn  "    #r \"file.dll\"          %s" (FSIstrings.SR.fsiIntroTextHashrInfo())
        fsiConsoleOutput.uprintfn  "    #time [\"on\"|\"off\"]     %s" (FSIstrings.SR.fsiIntroTextHashtimeInfo())
        fsiConsoleOutput.uprintfn  "    #help                  %s" (FSIstrings.SR.fsiIntroTextHashhelpInfo())
#else
        fsiConsoleOutput.uprintfn  ""
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader1directives());
        fsiConsoleOutput.uprintfn  "    #r \"file.dll\";;        %s" (FSIstrings.SR.fsiIntroTextHashrInfo());
        fsiConsoleOutput.uprintfn  "    #I \"path\";;            %s" (FSIstrings.SR.fsiIntroTextHashIInfo());
    #if SUPPORT_USE
        fsiConsoleOutput.uprintfn  "    #use \"file.fs\";;       use the given file, as if typed in.";
    #endif
        fsiConsoleOutput.uprintfn  "    #load \"file.fs\" ...;;  %s" (FSIstrings.SR.fsiIntroTextHashloadInfo());
        fsiConsoleOutput.uprintfn  "    #time [\"on\"|\"off\"];;   %s" (FSIstrings.SR.fsiIntroTextHashtimeInfo());
        fsiConsoleOutput.uprintfn  "    #help;;                %s" (FSIstrings.SR.fsiIntroTextHashhelpInfo());
        fsiConsoleOutput.uprintfn  "    #quit;;                %s" (FSIstrings.SR.fsiIntroTextHashquitInfo()); (* last thing you want to do, last thing in the list - stands out more *)
        fsiConsoleOutput.uprintfn  "";
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader2commandLine());
        fsiConsoleOutput.uprintfn  "%s" (FSIstrings.SR.fsiIntroTextHeader3("fsi --help"));
        fsiConsoleOutput.uprintfn  "";
    #if FX_ATLEAST_40
        // VS2010 (finally!) now shows the key-bindings in the right-click context menus, they are re-mappable so no easy way to print text names here, but now key-bindings are very discoverable
    #else    
        // For VS 2008 plugin, keep showing (non-localized) text
        if isInteractiveServer() then 
            fsiConsoleOutput.uprintfnn "  Visual Studio key bindings:";
            fsiConsoleOutput.uprintfn  "      Up/Down   = cycle history"; 
            fsiConsoleOutput.uprintfn  "      CTRL-DOT  = interrupt session";  (* CTRL-DOT in Visual Studio *)
            fsiConsoleOutput.uprintfn  "      ALT-ENTER = send selected source text to FSI session (adds ;;)";
            fsiConsoleOutput.uprintfn  ""
    #endif
        fsiConsoleOutput.uprintfn "";
#endif

#if DEBUG
    member __.ShowILCode with get() = showILCode and set v = showILCode <- v
#endif
    member __.ShowTypes with get() = showTypes and set v = showTypes <- v
    member __.FsiServerName = fsiServerName
    member __.FsiServerInputCodePage = fsiServerInputCodePage
    member __.FsiServerOutputCodePage = fsiServerOutputCodePage
    member __.FsiLCID with get() = fsiLCID and set v = fsiLCID <- v
    member __.IsInteractiveServer = isInteractiveServer()
    member __.ProbeToSeeIfConsoleWorks = probeToSeeIfConsoleWorks
    member __.EnableConsoleKeyProcessing = enableConsoleKeyProcessing

    member __.Interact = interact
    member __.PeekAheadOnConsoleToPermitTyping = peekAheadOnConsoleToPermitTyping
    member __.SourceFiles = sourceFiles
    member __.Gui = gui

//----------------------------------------------------------------------------
// Set the current ui culture for the current thread.
//----------------------------------------------------------------------------

let SetCurrentUICultureForThread (lcid : int option) =
    match lcid with
    | Some(n) -> System.Threading.Thread.CurrentThread.CurrentUICulture <- 
#if SILVERLIGHT    
                                                new System.Globalization.CultureInfo(n.ToString())
#else
                                                new System.Globalization.CultureInfo(n)
#endif                                                                                                
    | None -> ()


//----------------------------------------------------------------------------
// Reporting - warnings, errors
//----------------------------------------------------------------------------

let InstallErrorLoggingOnThisThread errorLogger =
    if !progress then dprintfn "Installing logger on id=%d name=%s" Thread.CurrentThread.ManagedThreadId Thread.CurrentThread.Name
    SetThreadErrorLoggerNoUnwind(errorLogger)
    SetThreadBuildPhaseNoUnwind(BuildPhase.Interactive)


/// Set the input/output encoding. The use of a thread is due to a known bug on 
/// on Vista where calls to System.Console.InputEncoding can block the process.
let SetServerCodePages(fsiOptions: FsiCommandLineOptions) =     
#if SILVERLIGHT
    match fsiOptions.FsiServerInputCodePage with
    | _ -> ()
#else     
    match fsiOptions.FsiServerInputCodePage, fsiOptions.FsiServerOutputCodePage with 
    | None,None -> ()
    | inputCodePageOpt,outputCodePageOpt -> 
        let successful = ref false 
        Async.Start (async { do match inputCodePageOpt with 
                                | None -> () 
                                | Some(n:int) ->
                                      let encoding = System.Text.Encoding.GetEncoding(n) in 
                                      // Note this modifies the real honest-to-goodness settings for the current shell.
                                      // and the modifiations hang around even after the process has exited.
                                      System.Console.InputEncoding <- encoding
                             do match outputCodePageOpt with 
                                | None -> () 
                                | Some(n:int) -> 
                                      let encoding = System.Text.Encoding.GetEncoding(n) in 
                                      // Note this modifies the real honest-to-goodness settings for the current shell.
                                      // and the modifiations hang around even after the process has exited.
                                      System.Console.OutputEncoding <- encoding
                             do successful := true  });
        for pause in [10;50;100;1000;2000;10000] do 
            if not !successful then 
                System.Threading.Thread.Sleep(pause);
        if not !successful then 
            System.Windows.Forms.MessageBox.Show(FSIstrings.SR.fsiConsoleProblem()) |> ignore

#endif


//----------------------------------------------------------------------------
// Prompt printing
//----------------------------------------------------------------------------

type FsiConsolePrompt(fsiOptions: FsiCommandLineOptions, fsiConsoleOutput: FsiConsoleOutput) =

    // A prompt gets "printed ahead" at start up. Tells users to start type while initialisation completes.
    // A prompt can be skipped by "silent directives", e.g. ones sent to FSI by VS.
    let mutable dropPrompt = 0
    let prompt = if fsiOptions.IsInteractiveServer then "SERVER-PROMPT>\n" else "> "  // NOTE: SERVER-PROMPT is not user displayed, rather it's a prefix that code elsewhere uses to identify the prompt, see vs\FsPkgs\FSharp.VS.FSI\fsiSessionToolWindow.fs

    member __.Print()      = if dropPrompt = 0 then fsiConsoleOutput.uprintf "%s" prompt else dropPrompt <- dropPrompt - 1
    member __.PrintAhead() = dropPrompt <- dropPrompt + 1; fsiConsoleOutput.uprintf "%s" prompt
    member __.SkipNext()   = dropPrompt <- dropPrompt + 1    
    member __.FsiOptions = fsiOptions



//----------------------------------------------------------------------------
// Startup processing
//----------------------------------------------------------------------------
type FsiConsoleInput(fsiOptions: FsiCommandLineOptions) =
#if SILVERLIGHT
#else

    let consoleLooksFunctional() =
        if fsiOptions.ProbeToSeeIfConsoleWorks then 
            try
                // Probe to see if the console looks functional on this version of .NET
                let _ = System.Console.KeyAvailable 
                let _ = System.Console.ForegroundColor
                let _ = System.Console.CursorLeft <- System.Console.CursorLeft
                true
            with _ -> 
                (* warning(Failure("Note: there was a problem setting up custom readline console support. Consider starting fsi.exe with the --no-readline option")); *)
                false
        else
            true 

#endif
    let consoleOpt =
#if SILVERLIGHT
        None
#else                
        // The "console.fs" code does a limitted form of "TAB-completion".
        // Currently, it turns on if it looks like we have a console.
        if fsiOptions.EnableConsoleKeyProcessing && consoleLooksFunctional() then
            Some(new Microsoft.FSharp.Compiler.Interactive.ReadLineConsole(fun (_s1,_s2) -> Seq.empty))
        else
            None

#endif
    // When VFSI is running, there should be no "console", and in particular the console.fs readline code should not to run.
    do  if fsiOptions.IsInteractiveServer then assert(consoleOpt = None)

    /// This threading event gets set after the first-line-reader has finished its work
    let consoleReaderStartupDone = new ManualResetEvent(false)

    /// When using a key-reading console this holds the first line after it is read
    let mutable firstLine = None

    /// Peek on the standard input so that the user can type into it from a console window.
    do if fsiOptions.Interact then
#if SILVERLIGHT
#else       
         if fsiOptions.PeekAheadOnConsoleToPermitTyping then 
          (new Thread(fun () -> 
              match consoleOpt with 
              | Some console when fsiOptions.EnableConsoleKeyProcessing && not fsiOptions.IsInteractiveServer ->
                  if isNil fsiOptions.SourceFiles then 
                      if !progress then dprintf "first-line-reader-thread reading first line...\n";
                      firstLine <- Some(console.ReadLine()); 
                      if !progress then dprintf "first-line-reader-thread got first line = %A...\n" firstLine;
                  consoleReaderStartupDone.Set() |> ignore 
                  if !progress then dprintf "first-line-reader-thread has set signal and exited.\n" ;
              | _ -> 
                  ignore(Console.In.Peek());
                  consoleReaderStartupDone.Set() |> ignore 
            )).Start()
         else
#endif                  
           consoleReaderStartupDone.Set() |> ignore

    member __.FirstLine with get() = firstLine and set v = firstLine <- v
    member __.ConsoleOpt = consoleOpt

    member __.WaitForInitialConsoleInput() = WaitHandle.WaitAll([| (consoleReaderStartupDone :> WaitHandle) |]) |> ignore;
    
/// FSI does a "startup" interaction to automatically page all the libary information.
/// This is mainly information for the typechecker environment.
/// Printing a prompt first means it should happen while the first line is being entered,
/// so effectively the background.


//----------------------------------------------------------------------------
// FsiDynamicCompilerState
//----------------------------------------------------------------------------

[<AutoSerializable(false)>]
[<NoEquality; NoComparison>]
type FsiDynamicCompilerState =
    { optEnv    : Opt.IncrementalOptimizationEnv;
      emEnv     : ILRuntimeWriter.emEnv;
      tcGlobals : Env.TcGlobals;
      tcState   : Build.TcState; 
      ilxGenEnv : Ilxgen.IlxGenEnv;
      // Why is this not in FsiOptions?
      timing    : bool;
    }

let WithImplicitHome (tcConfigB, dir) f = 
    let old = tcConfigB.implicitIncludeDir 
    tcConfigB.implicitIncludeDir <- dir;
    try f() 
    finally tcConfigB.implicitIncludeDir <- old


//----------------------------------------------------------------------------
// ProcessInputs
//----------------------------------------------------------------------------

type FsiDynamicCompiler(timeReporter : FsiTimeReporter, 
                        tcConfigB, tcLockObject, 
                        errorLogger: ErrorLoggerThatStopsOnFirstError, 
                        tcImports, tcGlobals, ilGlobals, 
                        fsiOptions : FsiCommandLineOptions,
                        fsiConsoleOutput : FsiConsoleOutput,
                        niceNameGen,
                        resolvePath) = 

    let outfile = "TMPFSCI.exe"
    let assemblyName = "FSI-ASSEMBLY"

    let mutable fragmentId = 0
    let mutable prevIt : ValRef option = None

    let generateDebugInfo = tcConfigB.debuginfo

    let valuePrinter = FsiValuePrinter(ilGlobals,generateDebugInfo,resolvePath)

    let assemblyBuilder,moduleBuilder = ILRuntimeWriter.mkDynamicAssemblyAndModule assemblyName (tcConfigB.optSettings.localOpt()) generateDebugInfo

    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

#if SILVERLIGHT
#else
    let _writer = moduleBuilder.GetSymWriter()
#endif
    
    /// Add attributes 
    let CreateModuleFragment (tcConfigB, assemblyName, codegenResults) =
        if !progress then dprintf "Creating main module...\n";
        let mainModule = mkILSimpleModule assemblyName (fsharpModuleName tcConfigB.target assemblyName) (tcConfigB.target = Dll) (mkILTypeDefs codegenResults.ilTypeDefs) None None 0x0 (mkILExportedTypes []) ""
        { mainModule 
          with Manifest = 
                (let man = mainModule.ManifestOfAssembly
                 Some { man with  CustomAttrs = mkILCustomAttrs codegenResults.ilAssemAttrs }); }

    let ProcessInputs(i, istate, inputs, showTypes, isIncrementalFragment, isInteractiveItExpr, prefixPath) =
        let optEnv    = istate.optEnv
        let emEnv     = istate.emEnv
        let tcState   = istate.tcState
        let ilxGenEnv = istate.ilxGenEnv
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)

        // typecheck 
        let (tcState:TcState),topCustomAttrs,declaredImpls,tcEnvAtEndOfLastInput =

            lock tcLockObject (fun _ -> TypecheckClosedInputSet(errorLogger.CheckForNoErrors,tcConfig,tcImports,tcGlobals, Some prefixPath,tcState,inputs, true))

        // Logging/debugging
        if tcConfig.printAst then
            let (TAssembly(declaredImpls)) = declaredImpls
            for input in declaredImpls do 
                dprintf "AST:\n%+A\n" input

        errorLogger.AbortOnError();
         
        let importMap = tcImports.GetImportMap()

        (*moved printing of response to after the effects are executed because values may now be shown *)

        // optimize: note we collect the incremental optimization environment 
        let optimizedImpls, _optData, optEnv = 
            ApplyAllOptimizations (tcConfig, tcGlobals, outfile, importMap, isIncrementalFragment, optEnv, tcState.Ccu, declaredImpls)
        errorLogger.AbortOnError();
            
        // codegen: note we collect the incremental optimization environment 
        let fragName = textOfLid prefixPath 
        let codegenResults = GenerateIlxCode (IlReflectBackend,true, isInteractiveItExpr, runningOnMono, tcGlobals, tcConfig, importMap, topCustomAttrs, optimizedImpls, tcState.Ccu, fragName, ilxGenEnv)
        errorLogger.AbortOnError();
        //if assemAttrs <> [] or modulAttrs <> [] then warning(Failure("Assembly attributes are ignored by by F# Interactive"));

        // Each fragment is like a small separately compiled extension to a single source file. 
        // The incremental extension to the environment is dictated by the "signature" of the values as they come out 
        // of the type checker. Hence we add the declaredImpls (unoptimized) to the environment, rather than the 
        // optimizedImpls. 
        let ilxGenEnv = Ilxgen.AddIncrementalLocalAssmblyFragmentToIlxGenEnv (tcImports.GetImportMap()) isIncrementalFragment tcGlobals tcState.Ccu fragName ilxGenEnv declaredImpls 

        ReportTime tcConfig "TAST -> ILX";
        errorLogger.AbortOnError();
            
        (* step *)    
        ReportTime tcConfig "Linking";
        let ilxMainModule = CreateModuleFragment (tcConfigB, assemblyName, codegenResults)

        errorLogger.AbortOnError();
            
        (* step *)
        ReportTime tcConfig "ILX -> IL (Unions)"; 
        let ilxMainModule = EraseIlxUnions.ConvModule ilGlobals ilxMainModule
        ReportTime tcConfig "ILX -> IL (Funcs)"; 
        let ilxMainModule = EraseIlxFuncs.ConvModule ilGlobals ilxMainModule 

        errorLogger.AbortOnError();   
              
        ReportTime tcConfig "Assembly refs Normalised"; 
        let mainmod3 = Morphs.morphILScopeRefsInILModuleMemoized (NormalizeAssemblyRefs tcImports) ilxMainModule
        errorLogger.AbortOnError();

        
        ReportTime tcConfig "Reflection.Emit";
        let emEnv,execs = ILRuntimeWriter.emitModuleFragment ilGlobals emEnv assemblyBuilder moduleBuilder mainmod3 generateDebugInfo resolvePath

        errorLogger.AbortOnError();

        // Explicitly register the resources with the QuotationPickler module 
        // We would save them as resources into the dynamic assembly but there is missing 
        // functionality System.Reflection for dynamic modules that means they can't be read back out 
        //printf "#resources = %d\n" (length resources);
        for bytes in codegenResults.quotationResourceBytes do 
            Microsoft.FSharp.Quotations.Expr.RegisterReflectedDefinitions (assemblyBuilder, fragName, bytes);
            

        ReportTime tcConfig "Run Bindings";
        timeReporter.TimeOpIf istate.timing (fun () -> 
          execs |> List.iter (fun exec -> 
            match exec() with 
            | Some(e) ->         
                eprintfn "%s" (e.ToString())
                errorLogger.SetError()
                errorLogger.AbortOnError()

            | None -> ())) ;

        errorLogger.AbortOnError();

        // Echo the decls (reach inside wrapping)
        // This code occurs AFTER the execution of the declarations.
        // So stored values will have been initialised, modified etc.
        if showTypes && not tcConfig.noFeedback then  
            let denv = tcState.TcEnvFromImpls.DisplayEnv
            let denv = if isIncrementalFragment then
                         // Extend denv with a (Val -> layout option) function for printing of val bindings.
                         {denv with generatedValueLayout = valuePrinter.InvokeDeclLayout (tcImports.GetImportMap()) istate.tcGlobals emEnv ilxGenEnv}
                       else
                         denv (* with #load items, the vals in the inferred signature do not tied up with those generated. Disabling printing. *)
            // open the path for the fragment we just compiled 
            let denv = denv.AddOpenPath (pathOfLid prefixPath) 

            let (TAssembly(declaredImpls)) = declaredImpls
            for (TImplFile(_qname,_,mexpr,_,_)) in declaredImpls do
                let responseL = NicePrint.inferredSigOfModuleExprL false denv mexpr 
                if not (Layout.isEmptyL responseL) then      
                    fsiConsoleOutput.uprintfn "";
                    // There are two copies of the layout sqashTo code (one in compiler, one in library).
                    // The library one converts Leaf objects to strings on the fly.
                    // The compiler one expects Lead objects to be strings already.
                    let opts = valuePrinter.GetFsiPrintOptions()
                    let responseL = Internal.Utilities.StructuredFormat.Display.squash_layout opts responseL
#if SILVERLIGHT
                    let stdout = !outWriter
#else
#endif                    
                    Layout.renderL (Layout.channelR stdout) responseL |> ignore
                    fsiConsoleOutput.uprintfnn ""

        let istate = {istate with  optEnv    = optEnv;
                                   emEnv     = emEnv;
                                   ilxGenEnv = ilxGenEnv;
                                   tcState   = tcState  }
        istate,tcEnvAtEndOfLastInput


    //----------------------------------------------------------------------------
    // EvalDefns, EvalExpr
    //----------------------------------------------------------------------------

    let nextFragmentId() = fragmentId <- fragmentId + 1; fragmentId
    let mkFragmentPath  i = 
        // NOTE: this text shows in exn traces and type names. Make it clear and fixed width 
        [mkSynId rangeStdin (FsiDynamicModulePrefix + sprintf "%04d" i)]


    member __.AssemblyName = assemblyName
    member __.AssemblyBuilder = assemblyBuilder

    member __.EvalInputsFromLoadedFiles (istate, inputs) =
        let i = nextFragmentId()
        let prefix = mkFragmentPath i 
        // Ensure the path includes the qualifying name 
        let inputs = inputs |> List.map (PrependPathToInput prefix) 
#if SILVERLIGHT // don't display types when loading startup file in Silverlight
        let istate,_ = ProcessInputs (i, istate, inputs, false, false, false, prefix)
#else
        let istate,_ = ProcessInputs (i, istate, inputs, true, false, false, prefix)
#endif
        istate

    member __.EvalDefns (istate, showTypes, isInteractiveItExpr, defs) =
        let filename = Lexhelp.stdinMockFilename
        let i = nextFragmentId()
        let prefix = mkFragmentPath i
        let prefixPath = pathOfLid prefix
        let impl = ModuleOrNamespace(prefix,(* isModule: *) true,defs,PreXmlDoc.Empty,[],None,rangeStdin)
        let input = ImplFileInput(ImplFile(filename,true, QualFileNameOfUniquePath (rangeStdin,prefixPath),[],[],[impl],true (* isLastCompiland *) ))
        let istate,tcEnvAtEndOfLastInput = ProcessInputs (i, istate, [input], showTypes, true, isInteractiveItExpr, prefix)
        let tcState = istate.tcState 
        { istate with tcState = tcState.NextStateAfterIncrementalFragment(tcEnvAtEndOfLastInput) }
      
     
    member fsiDynamicCompiler.EvalExpr istate (expr: SynExpr) =
        let m = expr.Range
        let tcConfig = TcConfig.Create (tcConfigB, validate=false)
        let itName = "it" 
        let itID  = mkSynId m itName
        let itExp = SynExpr.Ident itID
        let mkBind pat expr = Binding (None, DoBinding, false, (*mutable*)false, [], PreXmlDoc.Empty, SynInfo.emptyValSynData, pat, None, expr, m, NoSequencePointAtInvisibleBinding)
        let bindingA = mkBind (mkSynPatVar None itID) expr (* let it = <expr> *)  // NOTE: the generalizability of 'expr' must not be damaged, e.g. this can't be an application 
        let saverPath  = ["Microsoft";"FSharp";"Compiler";"Interactive";"RuntimeHelpers";"SaveIt"]
        let bindingB = mkBind (SynPat.Wild m) (SynExpr.App(ExprAtomicFlag.NonAtomic, SynExpr.LongIdent(false, List.map (mkSynId rangeStdin) saverPath,m), itExp,m)) (* let _  = saverPath it *)
        let defA = SynModuleDecl.Let (false, [bindingA], m)
        let defB = SynModuleDecl.Let (false, [bindingB], m)
        let istate = fsiDynamicCompiler.EvalDefns (istate, false, true, [defA;defB])
        // Snarf the type for 'it' via the binding
        match istate.tcState.TcEnvFromImpls.NameEnv.UnqualifiedItems |> NameMap.find itName with 
        | Nameres.Item.Value vref -> 
             if not tcConfig.noFeedback then 
                 valuePrinter.InvokeExprPrinter istate.tcState.TcEnvFromImpls.DisplayEnv vref.Deref
             match prevIt with
             | Some prevVal when not prevVal.Deref.HasBeenReferenced -> 
                 Ilxgen.ClearGeneratedValue (valuePrinter.GetEvaluationContext istate.emEnv) istate.tcGlobals istate.ilxGenEnv prevVal.Deref
             | _ -> ()
             prevIt <- Some vref
        | _ -> ()
        istate

    member __.EvalRequireDll istate m path = 
        if IsInvalidPath(path) then
            error(Error(FSIstrings.SR.fsiInvalidAssembly(path),m))
        // Check the file can be resolved before calling requireDLLReference 
        let _ = tcImports.ResolveLibFile(AssemblyReference(m,path),ResolveLibFileMode.ReportErrors)
        tcConfigB.AddReferencedAssemblyByPath(m,path);
        let tcState = istate.tcState 
        let tcEnv,(dllinfos,ccuinfos) = RequireDLL tcImports tcState.TcEnvFromImpls m path 
        let optEnv = List.fold AddExternalCcuToOpimizationEnv istate.optEnv ccuinfos
        let ilxGenEnv = AddExternalCcusToIlxGenEnv (tcImports.GetImportMap()) tcGlobals istate.ilxGenEnv (ccuinfos |> List.map (fun ccuinfo -> ccuinfo.FSharpViewOfMetadata)) 
        dllinfos,
        { istate with tcState = tcState.NextStateAfterIncrementalFragment(tcEnv);
                      optEnv = optEnv;
                      ilxGenEnv = ilxGenEnv }

    member fsiDynamicCompiler.ProcessMetaCommandsFromInputAsInteractiveCommands m istate sourceFile inp =
        WithImplicitHome
           (tcConfigB, directoryName sourceFile) 
           (fun () ->
               ProcessMetaCommandsFromInput 
                   ((fun st (m,nm) -> tcConfigB.TurnWarningOff(m,nm); st),
                    (fun st (m,nm) -> snd (fsiDynamicCompiler.EvalRequireDll st m nm)),
                    (fun _ _ -> ()))  
                   tcConfigB 
                   inp 
                   istate)
      
    member fsiDynamicCompiler.EvalLoadFiles(istate, m, sourceFiles, lexResourceManager) =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        match sourceFiles with 
        | [] -> istate
        | _ -> 
          // use source file as if it were a module
          let sourceFiles = sourceFiles |> List.map (fun nm -> tcConfig.ResolveSourceFile(m,nm),m) 
         
          // Close the #load graph on each file and gather the inputs from the scripts.
          let closure = LoadClosure.FindFromFiles(TcConfig.Create(tcConfigB,validate=false),sourceFiles,(*editting*)false,DefineInteractive,(*useDefaultScriptingReferences*)true,lexResourceManager)
          
          // Intent "[Loading %s]\n" (String.concat "\n     and " sourceFiles)
          fsiConsoleOutput.uprintf "[%s " (FSIstrings.SR.fsiLoadingFilesPrefixText())
          closure.Inputs  |> List.iteri (fun i (sourceFile,_) -> 
              if i=0 then fsiConsoleOutput.uprintf  "%s" sourceFile
              else fsiConsoleOutput.uprintnf " %s %s" (FSIstrings.SR.fsiLoadingFilesPrefixText()) sourceFile)
          fsiConsoleOutput.uprintfn "]"

          // Play errors and warnings from closures of the surface (root) script files.
          closure.RootErrors|>List.iter errorSink
          closure.RootWarnings|>List.iter warnSink
                
          // Non-scripts will not have been parsed during #load closure so parse them now
          let sourceFiles,inputs = 
                        closure.Inputs  |> List.map(fun (filename,input)->filename,
                                                                            match input with 
                                                                            | None -> ParseOneInputFile(tcConfig,lexResourceManager,["INTERACTIVE"],filename,true,errorLogger,(*retryLocked*)false)
                                                                            | _->input)
                                          |> List.unzip
          
          errorLogger.AbortOnError();
          if List.exists (function None -> true | _ -> false) inputs then failwith "parse error";
          let inputs = List.map Option.get inputs 
          let istate = List.fold2 (fsiDynamicCompiler.ProcessMetaCommandsFromInputAsInteractiveCommands m) istate sourceFiles inputs
          fsiDynamicCompiler.EvalInputsFromLoadedFiles (istate, inputs)

    
    member __.GetInitialInteractiveState () =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        let optEnv0 = InitialOptimizationEnv tcImports
        let emEnv = ILRuntimeWriter.emEnv0
        let tcEnv = GetInitialTypecheckerEnv None rangeStdin tcConfig tcImports tcGlobals
        let ccuName = assemblyName 

        let tcState = TypecheckInitialState (rangeStdin,ccuName,tcConfig,tcGlobals,niceNameGen,tcEnv)

        let ilxgenEnv0 = IlxgenEnvInit(tcConfig,tcImports,tcGlobals,tcState.Ccu )
        {optEnv    = optEnv0;
         emEnv     = emEnv;
         tcGlobals = tcGlobals;
         tcState   = tcState;
         ilxGenEnv = ilxgenEnv0;
         timing    = false;
        } 


type FsiIntellisenseProvider(tcConfigB, tcGlobals, tcImports: TcImports) = 

    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

    //----------------------------------------------------------------------------
    // FsiIntellisense - v1 - identifier completion - namedItemInEnvL
    //----------------------------------------------------------------------------

    member __.CompletionsForPartialLID istate (prefix:string) =
        let lid,stem =
            if prefix.IndexOf(".",StringComparison.Ordinal) >= 0 then
                let parts = prefix.Split(Array.ofList ['.'])
                let n = parts.Length
                Array.sub parts 0 (n-1) |> Array.toList,parts.[n-1]
            else
                [],prefix   
        let tcState = istate.tcState (* folded through now? *)

        let amap = tcImports.GetImportMap()
        let infoReader = new Infos.InfoReader(tcGlobals,amap)
        let ncenv = new Nameres.NameResolver(tcGlobals,amap,infoReader,Nameres.FakeInstantiationGenerator)
        // Note: for the accessor domain we should use (AccessRightsOfEnv tcState.TcEnvFromImpls)
        let ad = Infos.AccessibleFromSomeFSharpCode
        let nItems = Nameres.ResolvePartialLongIdent ncenv tcState.TcEnvFromImpls.NameEnv rangeStdin ad lid false
        let names  = nItems |> List.map (Nameres.DisplayNameOfItem tcGlobals) 
        let names  = names |> List.filter (fun (name:string) -> name.StartsWith(stem,StringComparison.Ordinal)) 
        names

    //----------------------------------------------------------------------------
    // FsiIntellisense (posible feature for v2) - GetDeclarations
    //----------------------------------------------------------------------------

#if SILVERLIGHT
#else    
    member __.FsiGetDeclarations istate (text:string) (names:string[]) =
        try
          let tcConfig = TcConfig.Create(tcConfigB,validate=false)
          Microsoft.FSharp.Compiler.SourceCodeServices.FsiIntelisense.getDeclarations
            (tcConfig,
             tcGlobals,
             tcImports,
             istate.tcState) 
            text 
            names
        with
          e ->
            System.Windows.Forms.MessageBox.Show("FsiGetDeclarations: throws:\n" ^ e.ToString()) |> ignore;
            [| |]

#endif

//----------------------------------------------------------------------------
// ctrl-c handling
//----------------------------------------------------------------------------

module internal NativeMethods = 

    type ControlEventHandler = delegate of int -> bool

#if SILVERLIGHT
#else
    [<DllImport("kernel32.dll")>]
    extern bool SetConsoleCtrlHandler(ControlEventHandler _callback,bool _add)
#endif

// One strange case: when a TAE happens a strange thing 
// occurs the next read from stdin always returns
// 0 bytes, i.e. the channel will look as if it has been closed.  So we check
// for this condition explicitly.  We also recreate the lexbuf whenever CtrlC kicks.
type FsiInterruptStdinState = 
    | StdinEOFPermittedBecauseCtrlCRecentlyPressed 
    | StdinNormal

type FsiInterruptControllerState =  
    | InterruptCanRaiseException 
    | InterruptIgnored 

type FsiInterruptControllerKillerThreadRequest =  
    | ThreadAbortRequest 
    | NoRequest 
    | ExitRequest 
    | PrintInterruptRequest

type FsiInterruptController(fsiOptions : FsiCommandLineOptions, fsiConsoleOutput: FsiConsoleOutput) = 

    let mutable stdinInterruptState = StdinNormal
    let CTRL_C = 0 
    let mutable interruptAllowed = InterruptIgnored
    let mutable killThreadRequest = NoRequest
    let mutable ctrlEventHandlers = [] : NativeMethods.ControlEventHandler list 
    let mutable ctrlEventActions  = [] : (unit -> unit) list 

    let mutable posixReinstate = (fun () -> ())

    member __.FsiInterruptStdinState with get () = stdinInterruptState and set v = stdinInterruptState <- v

    member __.InterruptRequest with set req = killThreadRequest <- req
    
    member __.InterruptAllowed with get () = interruptAllowed and set v = interruptAllowed <- v
    
    member __.Interrupt() = ctrlEventActions |> List.iter (fun act -> act())
    
    member __.EventHandlers = ctrlEventHandlers

    member controller.InstallKillThread(threadToKill:Thread, pauseMilliseconds:int) = 
#if SILVERLIGHT
        let action() =
            Microsoft.FSharp.Silverlight.InterruptThread(threadToKill.ManagedThreadId)

        ctrlEventActions  <- action           :: ctrlEventActions;
#else
        if !progress then dprintf "installing CtrlC handler\n";
        // WINDOWS TECHNIQUE: .NET has more safe points, and you can do more when a safe point. 
        // Hence we actually start up the killer thread within the handler. 
        try 
            let raiseCtrlC() = 
                Printf.eprintf "%s" (FSIstrings.SR.fsiInterrupt());  
                stdinInterruptState <- StdinEOFPermittedBecauseCtrlCRecentlyPressed;
                if (interruptAllowed = InterruptCanRaiseException) then 
                    killThreadRequest <- ThreadAbortRequest;
                    let killerThread = 
                        new Thread(new ThreadStart(fun () ->
                            SetCurrentUICultureForThread fsiOptions.FsiLCID
                            // sleep long enough to allow ControlEventHandler handler on main thread to return 
                            // Also sleep to give computations a bit of time to terminate 
                            Thread.Sleep(pauseMilliseconds);
                            if (killThreadRequest = ThreadAbortRequest) then 
                                if !progress then fsiConsoleOutput.uprintnfn "%s" (FSIstrings.SR.fsiAbortingMainThread());  
                                killThreadRequest <- NoRequest;
                                threadToKill.Abort();
                            ()),Name="ControlCAbortThread") 
                    killerThread.IsBackground <- true;
                    killerThread.Start() 
        
            let ctrlEventHandler = new NativeMethods.ControlEventHandler(fun i ->  if i = CTRL_C then (raiseCtrlC(); true) else false ) 
            ctrlEventHandlers <- ctrlEventHandler :: ctrlEventHandlers;
            ctrlEventActions  <- raiseCtrlC       :: ctrlEventActions;
            let _resultOK = NativeMethods.SetConsoleCtrlHandler(ctrlEventHandler,true)
            false // don't exit via kill thread
        with e -> 
            if !progress then eprintf "Failed to install ctrl-c handler using Windows technique - trying to install one using Unix signal handling...\n";
            // UNIX TECHNIQUE: We start up a killer thread, and it watches the mutable reference location.    
            // We can't have a dependency on Mono DLLs (indeed we don't even have them!)
            // So SOFT BIND the following code:
            // Mono.Unix.Native.Stdlib.signal(Mono.Unix.Native.Signum.SIGINT,new Mono.Unix.Native.SignalHandler(fun n -> PosixSignalProcessor.PosixInvoke(n))) |> ignore;
            match (try Choice1Of2(Assembly.Load("Mono.Posix, Version=2.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756")) with e -> Choice2Of2 e) with 
            | Choice1Of2(monoPosix) -> 
              try
                if !progress then eprintf "loading type Mono.Unix.Native.Stdlib...\n";
                let monoUnixStdlib = monoPosix.GetType("Mono.Unix.Native.Stdlib") 
                if !progress then eprintf "loading type Mono.Unix.Native.SignalHandler...\n";
                let monoUnixSignalHandler = monoPosix.GetType("Mono.Unix.Native.SignalHandler") 
                if !progress then eprintf "creating delegate...\n";
                controller.PosixInvoke(-1);
                let monoHandler = System.Delegate.CreateDelegate(monoUnixSignalHandler,controller,"PosixInvoke") 
                if !progress then eprintf "registering signal handler...\n";
                let monoSignalNumber = System.Enum.Parse(monoPosix.GetType("Mono.Unix.Native.Signum"),"SIGINT")
                let register () = Utilities.callStaticMethod monoUnixStdlib "signal" [ monoSignalNumber; box monoHandler ]  |> ignore 
                posixReinstate <- register;
                register();
                let killerThread = 
                    new Thread(new ThreadStart(fun () ->
                        SetCurrentUICultureForThread fsiOptions.FsiLCID
                        while true do 
                            //Printf.eprintf "\n- kill thread loop...\n"; stderr.Flush();  
                            Thread.Sleep(pauseMilliseconds*2);
                            match killThreadRequest with 
                            | PrintInterruptRequest -> 
                                Printf.eprintf "%s" (FSIstrings.SR.fsiInterrupt()); stderr.Flush();  
                                killThreadRequest <- NoRequest;
                            | ThreadAbortRequest -> 
                                Printf.eprintf "%s" (FSIstrings.SR.fsiInterrupt()); stderr.Flush();  
                                if !progress then fsiConsoleOutput.uprintnfn "%s" (FSIstrings.SR.fsiAbortingMainThread());
                                killThreadRequest <- NoRequest;
                                threadToKill.Abort()
                            | ExitRequest -> 
                                // Mono has some wierd behaviour where it blocks on exit
                                // once CtrlC has ever been pressed.  Who knows why?  Perhaps something
                                // to do with having a signal handler installed, but it only happens _after_
                                // at least one CtrLC has been pressed.  Maybe raising a ThreadAbort causes
                                // exiting to have problems.
                                //
                                // Anyway, we make "#q" work this case by setting ExitRequest and brutally calling
                                // the process-wide 'exit'
                                Printf.eprintf "%s" (FSIstrings.SR.fsiExit()); stderr.Flush();  
                                Utilities.callStaticMethod monoUnixStdlib "exit" [ box 0 ] |> ignore
                            | _ ->  ()
                        done),Name="ControlCAbortAlternativeThread") 
                killerThread.IsBackground <- true;
                killerThread.Start();
                true // exit via kill thread to workaround block-on-exit bugs with Mono once a CtrlC has been pressed
              with e -> 
                eprintf "%s" (FSIstrings.SR.fsiCouldNotInstallCtrlCHandler(e.Message))
                false
            | Choice2Of2 e ->
              eprintf "%s" (FSIstrings.SR.fsiCouldNotInstallCtrlCHandler(e.Message))
              false  


    member x.PosixInvoke(n:int) = 
         // we run this code once with n = -1 to make sure it is JITted before execution begins
         // since we are not allowed to JIT a signal handler.  THis also ensures the "PosixInvoke"
         // method is not eliminated by dead-code elimination
         if n >= 0 then 
             posixReinstate();
             stdinInterruptState <- StdinEOFPermittedBecauseCtrlCRecentlyPressed;
             killThreadRequest <- if (interruptAllowed = InterruptCanRaiseException) then ThreadAbortRequest else PrintInterruptRequest

#endif

//----------------------------------------------------------------------------
// assembly finder
//----------------------------------------------------------------------------

#nowarn "40"

module MagicAssemblyResolution =
    // FxCop identifies Assembly.LoadFrom.
    [<CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods", MessageId="System.Reflection.Assembly.UnsafeLoadFrom")>]
    let private assemblyLoadFrom (path:string) = 
    // See bug 5501 for details on decision to use UnsafeLoadFrom here.
    // Summary:
    //  It is an explicit user trust decision to load an assembly with #r. Scripts are not run automatically (for example, by double-clicking in explorer).
    //  We considered setting loadFromRemoteSources in fsi.exe.config but this would transitively confer unsafe loading to the code in the referenced 
    //  assemblies. Better to let those assemblies decide for themselves which is safer.
#if SILVERLIGHT
        Assembly.Load(path)
#else        
#if FX_ATLEAST_40
        Assembly.UnsafeLoadFrom(path)
#else
        Assembly.LoadFrom(path)
#endif
#endif // SILVERLIGHT
    let resolvePathFromAssemblyRef (tcImports:TcImports) assref = 
        match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef(assref) with
        | Some(assemblyReference) -> Some(assemblyReference)
        | None -> None

// TODO-SILVERLIGHT: ResolveEventHandler is security critical - we should get this to work        
#if SILVERLIGHT
    let Install(_tcConfigB, _tcImports: TcImports, _fsiDynamicCompiler: FsiDynamicCompiler, _fsiConsoleOutput: FsiConsoleOutput) = ()
#else
    let Install(tcConfigB, tcImports: TcImports, fsiDynamicCompiler: FsiDynamicCompiler, fsiConsoleOutput: FsiConsoleOutput) = 

        let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

        AppDomain.CurrentDomain.add_AssemblyResolve(new ResolveEventHandler(fun _ args -> 
           try 
               // Grab the name of the assembly
               let tcConfig = TcConfig.Create(tcConfigB,validate=false)
               let assemName = args.Name.Split([| ',' |]).[0]          
               if !progress then fsiConsoleOutput.uprintfn "ATTEMPT MAGIC LOAD ON ASSEMBLY, assemName = %s" assemName; // "Attempting to load a dynamically required assembly in response to an AssemblyResolve event by using known static assembly references..." 
               
               // Special case: Mono Windows Forms attempts to load an assembly called something like "Windows.Forms.resources"
               // We can't resolve this, so don't try.
               if assemName.EndsWith(".resources",StringComparison.OrdinalIgnoreCase) || (runningOnMono && assemName = "UIAutomationWinforms") then null else

               // Special case: Is this the global unique dynamic assembly for FSI code? In this case just
               // return the dynamic assembly itself.       
               if fsiDynamicCompiler.AssemblyName = assemName then (fsiDynamicCompiler.AssemblyBuilder :> Reflection.Assembly) else

               // Otherwise continue
               let fileName1 = (assemName + ".dll") 
               let fileName2 = (assemName + ".exe") 
               let overallSearchResult =           
                   // OK, try to resolve as a .dll
                   let searchResult = tcImports.TryResolveLibFile (AssemblyReference(rangeStdin,fileName1),ResolveLibFileMode.Speculative)

                   match searchResult with
                   | OkResult (warns,r) -> OkResult (warns,r.resolvedPath)
                   | _ -> 

                   // OK, try to resolve as a .exe
                   let searchResult = tcImports.TryResolveLibFile (AssemblyReference(rangeStdin,fileName2),ResolveLibFileMode.Speculative)

                   match searchResult with
                   | OkResult (warns, r) -> OkResult (warns, r.resolvedPath)
                   | _ -> 

                   if !progress then fsiConsoleOutput.uprintfn "ATTEMPT LOAD, fileName1 = %s" fileName1;
                   /// Take a look through the files quoted, perhaps with explicit paths
                   let searchResult = 
                       (tcConfig.referencedDLLs 
                            |> List.tryPick (fun assemblyReference -> 
                             if !progress then fsiConsoleOutput.uprintfn "ATTEMPT MAGIC LOAD ON FILE, referencedDLL = %s" assemblyReference.Text;
                             if System.String.Compare(System.IO.Path.GetFileName assemblyReference.Text, fileName1,StringComparison.OrdinalIgnoreCase) = 0 then
                                 Some(tcImports.TryResolveLibFile(assemblyReference,ResolveLibFileMode.Speculative))
                             else None ))

                   match searchResult with
                   | Some (OkResult (warns,r)) -> OkResult (warns, r.resolvedPath)
                   | _ -> 

                   match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef(ILAssemblyRef.Create(assemName,None,None,false,None,None)) with
                   | Some(resolvedPath) -> OkResult([],resolvedPath)
                   | None -> 
                   // OK, assembly resolution has failed, at least according to the F# rules. We now give
                   // other AssemblyResolve handlers a chance to run. 
                   // This is a specific request from customers who customize the 
                   // AssemblyResolve mechanism to do magic things like going to a distributed company file store
                   // to pick up DLLs. This is also one of the reasons why the TryResolveLibFile paths can't 
                   // report errors or warnings: we don't want spurious errors and warnings coming out before everyon
                   // has had a chance to resolve an assembly.
                   //
                   // If all other AssemblyResolve also fail then we want to report a "nice" exception. But how do we know if
                   // they failed? We just add a handler to the end of the AssemblyResolve chain, and if it
                   // ever gets executed we know they failed.
                   //
                   // This is also a fix for bug 1171.
                   let rec failingResolveHandler = 
                        new ResolveEventHandler(fun _ _ -> 

                            // OK, the failingResolveHandler is now running now so remove it from the list to prevent it
                            // ever running again
                            (try AppDomain.CurrentDomain.remove_AssemblyResolve(failingResolveHandler) with _ -> ());

                            // See bug 1171
                            if assemName.EndsWith ".XmlSerializers" then null else

                            // Now commit the warnings and errors by re-resolving. If the file suddenly exists in the milliseconds
                            // in between well, then we succeed
                            tcImports.ResolveLibFile(AssemblyReference(rangeStdin,fileName1),ResolveLibFileMode.ReportErrors).resolvedPath |> assemblyLoadFrom)

                   AppDomain.CurrentDomain.add_AssemblyResolve(failingResolveHandler);
                   ErrorResult([],Failure (FSIstrings.SR.fsiFailedToResolveAssembly(assemName)))
                           
               match overallSearchResult with 
               | ErrorResult _ -> null
               | OkResult _ -> 
                   let res = CommitOperationResult overallSearchResult
                   if assemName <> "Mono.Posix" then fsiConsoleOutput.uprintfn "%s" (FSIstrings.SR.fsiBindingSessionTo(res));
                   assemblyLoadFrom(res)
                   
           with e -> 
               stopProcessingRecovery e range0; 
               null));

#endif // SILVERLIGHT
//----------------------------------------------------------------------------
// Reading stdin 
//----------------------------------------------------------------------------

type FsiStdinLexerProvider(tcConfigB, fsiStdinSyphon, 
                           fsiConsoleInput : FsiConsoleInput, 
                           fsiOptions : FsiCommandLineOptions,
                           lexResourceManager : LexResourceManager,
                           errorLogger) = 

    // #light is the default for FSI
    let interactiveInputLightSyntaxStatus = 
        LightSyntaxStatus (tcConfigB.light <> Some(false), false (* no warnings *))

    let LexbufFromLineReader (fsiStdinSyphon: FsiStdinSyphon) readf : UnicodeLexing.Lexbuf = 
        UnicodeLexing.FunctionAsLexbuf 
          (fun (buf: char[], start, len) -> 
            //dprintf "Calling ReadLine\n";
            let inputOption = try Some(readf()) with :? EndOfStreamException -> None
            inputOption |> Option.iter (fun t -> fsiStdinSyphon.Add (t + "\n"));
            match inputOption with 
            |  Some(null) | None -> 
                 if !progress then dprintf "End of file from TextReader.ReadLine\n";
                 0
            | Some (input:string) ->
                let input  = input + "\n" 
                let ninput = input.Length 
                if ninput > len then eprintf "%s" (FSIstrings.SR.fsiLineTooLong());
                let ntrimmed = min len ninput 
                for i = 0 to ntrimmed-1 do
                    buf.[i+start] <- input.[i]
                ntrimmed
        )

    //----------------------------------------------------------------------------
    // Reading stdin as a lex stream
    //----------------------------------------------------------------------------

    let removeZeroCharsFromString (str:string) = (* bug://4466 *)
        if str<>null && str.Contains("\000") then
          System.String(str |> Seq.filter (fun c -> c<>'\000') |> Seq.toArray)
        else
          str

    member __.MkStdinLexer () =
        let lexbuf = 
#if SILVERLIGHT        
            LexbufFromLineReader fsiStdinSyphon (fun () -> (!inReader).ReadLine() |> removeZeroCharsFromString)
#else        
            match fsiConsoleInput.ConsoleOpt with 
            | Some console when fsiOptions.EnableConsoleKeyProcessing && not fsiOptions.IsInteractiveServer -> 
                LexbufFromLineReader fsiStdinSyphon (fun () -> 
                    match fsiConsoleInput.FirstLine with 
                    | Some l -> 
                        fsiConsoleInput.FirstLine <- None; 
                        l
                    | None -> 
                        console.ReadLine())
            | _ -> 
                LexbufFromLineReader fsiStdinSyphon (fun () -> System.Console.In.ReadLine() |> removeZeroCharsFromString)
#endif                
                //lexbufFromTextReader Encoding.UTF8 System.Console.In
        Lexhelp.resetLexbufPos Lexhelp.stdinMockFilename lexbuf;
        fsiStdinSyphon.Reset();
        let lexargs = mkLexargs ((fun () -> tcConfigB.implicitIncludeDir),Lexhelp.stdinMockFilename,"INTERACTIVE"::tcConfigB.conditionalCompilationDefines,interactiveInputLightSyntaxStatus,lexResourceManager, ref[], errorLogger) 
        let skip = true  (* don't report whitespace from lexer *)
        // A single tokenizer must be shared for the entire use of this lexbuf. 
        let tokenizer = Lexfilter.LexFilter(interactiveInputLightSyntaxStatus, Lexer.token lexargs skip, lexbuf)
        tokenizer

    member __.MkIncludeFileLexer sourceFile =
        let lexbuf = UnicodeLexing.UnicodeFileAsLexbuf(sourceFile,tcConfigB.inputCodePage,(*retryLocked*)false)  
        Lexhelp.resetLexbufPos sourceFile lexbuf;
        let skip = true 
        let defines = "INTERACTIVE"::tcConfigB.conditionalCompilationDefines
        let lexargs = mkLexargs ((fun () -> tcConfigB.implicitIncludeDir),sourceFile,defines, interactiveInputLightSyntaxStatus, lexResourceManager, ref [], errorLogger) in 
        let tokenizer = Lexfilter.LexFilter(interactiveInputLightSyntaxStatus, Lexer.token lexargs skip, lexbuf)
        tokenizer

//----------------------------------------------------------------------------
// Process one parsed interaction.  This runs on the GUI thread.
// It might be simpler if it ran on the parser thread.
//----------------------------------------------------------------------------

type FsiInteractionStepStatus = 
    | CtrlC 
    | EndOfFile 
    | Completed 
    | CompletedWithReportedError


type FsiInteractionProcessor(tcConfigB, 
                             errorLogger : ErrorLoggerThatStopsOnFirstError, 
                             fsiOptions: FsiCommandLineOptions,
                             fsiDynamicCompiler: FsiDynamicCompiler,
                             fsiConsolePrompt : FsiConsolePrompt,
                             fsiConsoleOutput : FsiConsoleOutput,
                             fsiInterruptController : FsiInterruptController,
                             fsiStdinLexerProvider : FsiStdinLexerProvider,
                             lexResourceManager : LexResourceManager) = 

    let InteractiveCatch f istate = 
        try
            (* reset error count *)
            errorLogger.ResetErrorCount();  
            f istate
        with  e ->
            stopProcessingRecovery e range0;
            istate,CompletedWithReportedError


    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

    let ChangeDirectory (path:string) m =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        let path = tcConfig.MakePathAbsolute path 
        if Directory.Exists(path) then 
            tcConfigB.implicitIncludeDir <- path
        else
            error(Error(FSIstrings.SR.fsiDirectoryDoesNotExist(path),m))

    //----------------------------------------------------------------------------
    // parsing - ParseInteraction
    //----------------------------------------------------------------------------

    let ParseInteraction (tokenizer:Lexfilter.LexFilter) =   
        let lastToken = ref Parser.ELSE (* Bug 1935: any token <> SEMICOLON_SEMICOLON will do for initial value *)
        try 
            if !progress then dprintf "In ParseInteraction...\n";

            let input = 
                Lexhelp.reusingLexbufForParsing (tokenizer.LexBuffer) (fun () -> 
                    let lexer = tokenizer.Lexer
                    let lexbuf = tokenizer.LexBuffer
                    let lexer = fun lexbuf -> (let tok = lexer lexbuf 
                                               lastToken := tok;
                                               tok)                        
                    Parser.interaction lexer lexbuf)
            Some input
        with e ->
            // Bug 1935. On error, consume tokens until to ;; or EOF.
            // Caveat: Unless the error parse ended on ;; - so check the lastToken returned by the lexer function.
            // Caveat: What if this was a look-ahead? That's fine! Since we need to skip to the ;; anyway.     
            if (match !lastToken with Parser.SEMICOLON_SEMICOLON -> false | _ -> true) then
                let lexer  = tokenizer.Lexer  
                let lexbuf = tokenizer.LexBuffer 
                let mutable tok = Parser.ELSE (* <-- any token <> SEMICOLON_SEMICOLON will do *)
                while (match tok with  Parser.SEMICOLON_SEMICOLON -> false | _ -> true) && not lexbuf.IsPastEndOfStream do
                    tok <- lexer lexbuf            

            stopProcessingRecovery e range0;    
            None

    let rec ExecInteraction exitViaKillThread (tcConfig:TcConfig) istate (action:ParsedFsiInteraction) =
        istate |> InteractiveCatch (fun istate -> 
            match action with 
            | IDefns ([  ],_) ->
                istate,Completed
            | IDefns ([  SynModuleDecl.DoExpr(_,expr,_)],_) ->
                fsiDynamicCompiler.EvalExpr  istate expr, Completed           
            | IDefns (defs,_) -> 
                fsiDynamicCompiler.EvalDefns (istate, true, false, defs), Completed
    #if SUPPORT_USE
            | IHash (ParsedHashDirective("use",[sourceFile],m),_) ->
                MainThreadProcessInteractiveFile exitViaKillThread istate (sourceFile,m)
    #endif

            | IHash (ParsedHashDirective("load",sourceFiles,m),_) -> 
                fsiDynamicCompiler.EvalLoadFiles (istate, m, sourceFiles, lexResourceManager),Completed

            | IHash (ParsedHashDirective(("reference" | "r"),[path],m),_) -> 
                let dllinfos,istate = fsiDynamicCompiler.EvalRequireDll istate m path 
                dllinfos |> List.iter (fun dllinfo -> fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiDidAHashr(dllinfo.FileName)));
                istate,Completed

            | IHash (ParsedHashDirective("I",[path],m),_) -> 
                tcConfigB.AddIncludePath (m,path); 
                fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiDidAHashI(tcConfig.MakePathAbsolute path));
                istate,Completed

            | IHash (ParsedHashDirective("cd",[path],m),_) ->
                ChangeDirectory path m;
                istate,Completed

            | IHash (ParsedHashDirective("silentCd",[path],m),_) ->
                ChangeDirectory path m;
                fsiConsolePrompt.SkipNext(); (* "silent" directive *)
                istate,Completed                  

            | IHash (ParsedHashDirective("time",[],_),_) -> 
                if istate.timing then
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOff())
                else
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOn())
                {istate with timing = not istate.timing},Completed

            | IHash (ParsedHashDirective("time",[("on" | "off") as v],_),_) -> 
                if v <> "on" then
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOff())
                else
                    fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiTurnedTimingOn())
                {istate with timing = (v = "on")},Completed

            | IHash (ParsedHashDirective("nowarn",numbers,m),_) -> 
                List.iter (fun (d:string) -> tcConfigB.TurnWarningOff(m,d)) numbers;
                istate,Completed

            | IHash (ParsedHashDirective("terms",[],_),_) -> 
                tcConfigB.showTerms <- not tcConfig.showTerms; 
                istate,Completed

            | IHash (ParsedHashDirective("types",[],_),_) -> 
                fsiOptions.ShowTypes <- not fsiOptions.ShowTypes; 
                istate,Completed

    #if DEBUG
            | IHash (ParsedHashDirective("ilcode",[],_m),_) -> 
                fsiOptions.ShowILCode <- not fsiOptions.ShowILCode; 
                istate,Completed

            | IHash (ParsedHashDirective("info",[],_m),_) -> 
                PrintOptionInfo tcConfigB
                istate,Completed         
    #endif

#if SILVERLIGHT
#else         
            | IHash (ParsedHashDirective(("q" | "quit"),[],_),_) -> 
                if exitViaKillThread then 
                    fsiInterruptController.InterruptRequest <- ExitRequest;
                    Thread.Sleep(1000)
                exit 0;                

#endif
            | IHash (ParsedHashDirective("help",[],_),_) ->
                fsiOptions.ShowHelp();
                istate,Completed

            | IHash (ParsedHashDirective(c,arg,_),_) -> 
                fsiConsoleOutput.uprintfn "%s" (FSIstrings.SR.fsiInvalidDirective(c, String.concat " " arg));  
                istate,Completed  
        )

    and ExecInteractions exitViaKillThread tcConfig istate (action:ParsedFsiInteraction option) =
        // #directive comes through with other definitions as a SynModuleDecl.HashDirective.
        // Split these out for individual processing.
        let action,nextAction = 
            match action with
            | None                                      -> None  ,None
            | Some (IHash _)                            -> action,None
            | Some (IDefns ([],_))                      -> None  ,None
            | Some (IDefns (SynModuleDecl.HashDirective(hash,mh)::defs,m)) -> Some (IHash(hash,mh)),Some (IDefns(defs,m))
            | Some (IDefns (defs,m))                    -> let isDefHash = function SynModuleDecl.HashDirective(_,_) -> true | _ -> false
                                                           let defsA = Seq.takeWhile (isDefHash >> not) defs |> Seq.toList
                                                           let defsB = Seq.skipWhile (isDefHash >> not) defs |> Seq.toList
                                                           Some (IDefns(defsA,m)),Some (IDefns(defsB,m))
        match action with
          | None -> assert(nextAction.IsNone); istate,Completed
          | Some action ->
              let istate,cont = ExecInteraction exitViaKillThread tcConfig istate action
              match cont with
                | Completed                  -> ExecInteractions exitViaKillThread tcConfig istate nextAction
                | CompletedWithReportedError -> istate,CompletedWithReportedError  (* drop nextAction on error *)
                | EndOfFile                  -> istate,Completed                   (* drop nextAction on EOF *)
                | CtrlC                      -> istate,CtrlC                       (* drop nextAction on CtrlC *)

    and MainThreadProcessParsedInteraction exitViaKillThread action istate = 
        try 
            let tcConfig = TcConfig.Create(tcConfigB,validate=false)
#if SILVERLIGHT
            Microsoft.FSharp.Silverlight.ResumeThread(Threading.Thread.CurrentThread.ManagedThreadId)
            ExecInteractions exitViaKillThread tcConfig istate action
        with
        | :? ThreadAbortException ->
           (istate,CtrlC)
        |  e ->
           stopProcessingRecovery e range0;
           istate,CompletedWithReportedError
#else                                   
            if !progress then dprintf "In MainThreadProcessParsedInteraction...\n";    
            fsiInterruptController.InterruptAllowed <- InterruptCanRaiseException;
            let res = ExecInteractions exitViaKillThread tcConfig istate action
            fsiInterruptController.InterruptRequest <- NoRequest;
            fsiInterruptController.InterruptAllowed <- InterruptIgnored;
            res
        with
        | :? ThreadAbortException ->
           fsiInterruptController.InterruptRequest <- NoRequest;
           fsiInterruptController.InterruptAllowed <- InterruptIgnored;
           (try Thread.ResetAbort() with _ -> ());
           (istate,CtrlC)
        |  e ->
           fsiInterruptController.InterruptRequest <- NoRequest;
           fsiInterruptController.InterruptAllowed <- InterruptIgnored;
           stopProcessingRecovery e range0;
           istate,CompletedWithReportedError
#endif

    /// Parse then process one parsed interaction.  This initially runs on the parser
    /// thread, then calls runCodeOnMainThread to run on the GUI thread. 
    /// 'ProcessAndRunOneInteractionFromLexbuf' calls the runCodeOnMainThread when it has completed 
    /// parsing and needs to typecheck and execute a definition.  Type-checking and execution 
    /// happens on the GUI thread.

    member __.ProcessAndRunOneInteractionFromLexbuf exitViaKillThread runCodeOnMainThread istate (tokenizer:Lexfilter.LexFilter) =
        let lexbuf = tokenizer.LexBuffer

        if lexbuf.IsPastEndOfStream then 
            let stepStatus = 
#if SILVERLIGHT
#else                        
                if fsiInterruptController.FsiInterruptStdinState = StdinEOFPermittedBecauseCtrlCRecentlyPressed then 
                    fsiInterruptController.FsiInterruptStdinState <- StdinNormal; 
                    CtrlC
                else 
#endif                
                    EndOfFile
            istate,stepStatus

        else 

            fsiConsolePrompt.Print();
            istate |> InteractiveCatch (fun istate -> 
                // BLOCKING POINT
                // When FSI.EXE is waiting for input from the console the 
                // parser thread is blocked somewhere deep this call. *)
                if !progress then dprintf "entering ParseInteraction...\n";
                let action  = ParseInteraction tokenizer
                if !progress then dprintf "returned from ParseInteraction...\n";
                // After we've unblocked and got something to run we switch 
                // over to the run-thread (e.g. the GUI thread) 
                if !progress then dprintf "calling runCodeOnMainThread...\n";
                let res = runCodeOnMainThread (MainThreadProcessParsedInteraction exitViaKillThread action) istate 
                if !progress then dprintf "Just called runCodeOnMainThread, res = %O...\n" res;
                res)
        
    member processor.MainThreadProcessInteractiveFile exitViaKillThread istate (sourceFile,m) =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        // Resolve the filename to an absolute filename
        let sourceFile = tcConfig.ResolveSourceFile(m,sourceFile) 
        // During the processing of the file, further filenames are 
        // resolved relative to the home directory of the loaded file.
        WithImplicitHome (tcConfigB, directoryName sourceFile)  (fun () ->
            // use source file containing maybe several ;-interaction blocks 
                let tokenizer = fsiStdinLexerProvider.MkIncludeFileLexer sourceFile
                let rec run istate =
                    let istate,cont = processor.ProcessAndRunOneInteractionFromLexbuf exitViaKillThread (fun f istate -> f istate) istate tokenizer
                    if cont = Completed then run istate else istate,cont 
                let istate,cont = run istate 
                match cont with
                | Completed -> failwith "MainThreadProcessInteractiveFile: Completed expected to have relooped"
                | CompletedWithReportedError -> istate,CompletedWithReportedError
                | EndOfFile -> istate,Completed (* here file-EOF is normal, continue required *)
                | CtrlC     -> istate,CtrlC
          )


    member processor.EvalInteractiveFiles istate exitViaKillThread sourceFiles =
      match sourceFiles with
        | [] -> istate
        | sourceFile :: sourceFiles ->
            // Catch errors on a per-file basis, so results/bindings from pre-error files can be kept.
            let istate,cont = InteractiveCatch (fun istate -> processor.MainThreadProcessInteractiveFile exitViaKillThread istate (sourceFile,rangeStdin)) istate
            match cont with
              | Completed                  -> processor.EvalInteractiveFiles istate exitViaKillThread sourceFiles  
              | CompletedWithReportedError -> istate (* do not process any more files *)             
              | CtrlC                      -> istate (* do not process any more files *)
              | EndOfFile                  -> assert(false); istate (* This is unexpected. EndOfFile is replaced by Completed in the called function *)


    member processor.LoadInitialFiles exitViaKillThread istate =
        let istate = 
            let rec consume istate sourceFiles =
                match sourceFiles with
                | [] -> istate
                | (_,fsx1) :: _ -> 
                    let sourceFiles,rest = List.takeUntil (fun (_,fsx2) -> fsx1 <> fsx2) sourceFiles 
                    let sourceFiles = List.map fst sourceFiles 
                    let istate = 
                        if fsx1 
                        then processor.EvalInteractiveFiles istate exitViaKillThread sourceFiles
                        else istate |> InteractiveCatch (fun istate -> fsiDynamicCompiler.EvalLoadFiles(istate, rangeStdin, sourceFiles, lexResourceManager), Completed) |> fst 
                    consume istate rest 
            consume istate fsiOptions.SourceFiles

        if nonNil fsiOptions.SourceFiles then 
            fsiConsolePrompt.PrintAhead(); (* Seems required. I expected this could be deleted. Why not? *)
        istate 

    member processor.LoadDummyInteraction istate =
        istate |> InteractiveCatch (fun istate ->  fsiDynamicCompiler.EvalDefns (istate, true, false, []), Completed) |> fst

    member processor.FsiOptions = fsiOptions

//----------------------------------------------------------------------------
// GUI runCodeOnMainThread
//----------------------------------------------------------------------------

#if SILVERLIGHT
#else            
//type InteractionStateConverter = delegate of FsiDynamicCompilerState -> FsiDynamicCompilerState * stepStatus

///Use a dummy to access protected member
type DummyForm() = 
    inherit Form() 
    member x.DoCreateHandle() = x.CreateHandle() 

/// This is the event loop implementation for winforms
type WinFormsEventLoop(tcConfigB, fsiStdinSyphon : FsiStdinSyphon, lcid : int option) = 
    let mainForm = new DummyForm() 
    do mainForm.DoCreateHandle();
    // Set the default thread exception handler
    let restart = ref false
    interface Microsoft.FSharp.Compiler.Interactive.IEventLoop with
         member x.Run() =  
             restart := false;
             if !progress then dprintf "MAIN: Calling Application.Run...\n";
             Application.Run()
             if !progress then dprintf "MAIN: Returned from Application.Run...\n";
             !restart
         member x.Invoke (f: unit -> 'T) : 'T =   
            if !progress then dprintf "RunCodeOnWinFormsMainThread: entry...\n";                  
            if not mainForm.InvokeRequired then 
                f() 
            else

                // Workaround: Mono's Control.Invoke returns a null result.  Hence avoid the problem by 
                // transferring the resulting state using a mutable location.
                let mainFormInvokeResultHolder = ref None

                // Actually, Mono's Control.Invoke isn't even blocking (or wasn't on 1.1.15)!  So use a signal to indicate completion.
                // Indeed, we should probably do this anyway with a timeout so we can report progress from 
                // the GUI thread.
                use doneSignal = new AutoResetEvent(false)

                if !progress then dprintf "RunCodeOnWinFormsMainThread: invoking...\n";                  

                // BLOCKING: This blocks the stdin-reader thread until the
                // form invocation has completed.  NOTE: does not block on Mono, or did not on 1.1.15
                mainForm.Invoke(new MethodInvoker(fun () -> 
                                           try 
                                              // When we get called back, someone may jack our culture
                                              // So we must reset our UI culture every time
                                              SetCurrentUICultureForThread lcid;
                                              mainFormInvokeResultHolder := Some(f ());
                                           finally 
                                              doneSignal.Set() |> ignore)) |> ignore;

                if !progress then dprintf "RunCodeOnWinFormsMainThread: Waiting for completion signal....\n";
                while not (doneSignal.WaitOne(new TimeSpan(0,0,1),true)) do 
                    if !progress then dprintf "."; stdout.Flush()

                if !progress then dprintf "RunCodeOnWinFormsMainThread: Got completion signal, res = %b\n" (Option.isSome !mainFormInvokeResultHolder);
                !mainFormInvokeResultHolder |> Option.get

         member x.ScheduleRestart()  =   restart := true; Application.Exit() 
     
let TrySetUnhandledExceptionMode() =  
    let i = ref 0 // stop inlining 
    try 
      Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException) 
      incr i;incr i;incr i;incr i;incr i;incr i;
    with _ -> 
      decr i;decr i;decr i;decr i;()

#endif // SILVERLIGHT


//----------------------------------------------------------------------------
// Server mode:
//----------------------------------------------------------------------------
#if SILVERLIGHT
#else
let SpawnThread name f =
    let th = new Thread(new ThreadStart(f),Name=name)
    th.IsBackground <- true;
    th.Start()

let SpawnInteractiveServer (fsiOptions : FsiCommandLineOptions, fsiIntellisenseProvider : FsiIntellisenseProvider, istateRef, fsiInterruptController : FsiInterruptController, tcLockObject) =   
    //printf "Spawning fsi server on channel '%s'" !fsiServerName;
    SpawnThread "ServerThread" (fun () ->
         SetCurrentUICultureForThread fsiOptions.FsiLCID
         try
             let server =
                 {new Server.Shared.FSharpInteractiveServer() with
                    member this.Interrupt() = //printf "FSI-SERVER: received CTRL-C request...\n";
                        try fsiInterruptController.Interrupt()
                        with e -> assert(false); ()    (* final sanity check! - catch all exns - but not expected *)
                    member this.Completions(prefix) = 
                        try fsiIntellisenseProvider.CompletionsForPartialLID !istateRef prefix  |> List.toArray
                        with e -> assert(false); [| |] (* final sanity check! - catch all exns - but not expected*)
                    member this.GetDeclarations(text,names) = 
                        try lock tcLockObject (fun () -> fsiIntellisenseProvider.FsiGetDeclarations !istateRef text names)
                        with e -> assert(false); [| |] (* final sanity check! - catch all exns - but not expected *)
                 }
             Server.Shared.FSharpInteractiveServer.StartServer(fsiOptions.FsiServerName,server)
         with e ->
             Printf.eprintfn "%s" (FSIstrings.SR.fsiExceptionRaisedStartingServer(e.ToString())))
  
#endif // SILVERLIGHT

let StartStdinReadAndProcessThread(lcid, istateRef, istate, errorLogger, 
                                   fsiConsoleInput: FsiConsoleInput, 
                                   fsiStdinLexerProvider: FsiStdinLexerProvider, 
                                   fsiInteractionProcessor : FsiInteractionProcessor,
                                   exitViaKillThread) = 
    if !progress then dprintf "creating stdinReaderThread\n";
    istateRef := istate;
    let cont = ref Completed 
    let tokenizerRef = ref (fsiStdinLexerProvider.MkStdinLexer())
    let culture = System.Threading.Thread.CurrentThread.CurrentUICulture
    
    let stdinReaderThread = 
        new Thread(new ThreadStart(fun () ->
            InstallErrorLoggingOnThisThread errorLogger // FSI error logging on stdinReaderThread, e.g. parse errors.
            SetCurrentUICultureForThread lcid
            try
               try 
                  if !progress then dprintf "READER: stdin thread started...\n";

                  // Delay until we've peeked the input or read the entire first line
                  fsiConsoleInput.WaitForInitialConsoleInput()
                  
                  if !progress then dprintf "READER: stdin thread got first line...\n";

                  // The main stdin loop, running on the stdinReaderThread.
                  // 
                  // The function 'ProcessAndRunOneInteractionFromLexbuf' is blocking: it reads stdin 
                  // until one or more real chunks of input have been received. 
                  //
                  // We run the actual computations for each action on the main GUI thread by using
                  // mainForm.Invoke to pipe a message back through the form's main event loop. (The message 
                  // is a delegate to execute on the main Thread)
                  //
                  while (!cont = CompletedWithReportedError || !cont = Completed || !cont = CtrlC) do
                      if (!cont = CtrlC) then 
                          tokenizerRef := fsiStdinLexerProvider.MkStdinLexer();

                      let istate',cont' = 
                          let runCodeOnMainThread f istate = 
                              try fsi.EventLoop.Invoke (fun () -> 
                                      InstallErrorLoggingOnThisThread errorLogger; 
                                      SetCurrentUICultureForThread lcid;
                                      f istate) // FSI error logging on switched to thread
                              with _ -> (istate,Completed)
                              
                          fsiInteractionProcessor.ProcessAndRunOneInteractionFromLexbuf exitViaKillThread runCodeOnMainThread !istateRef !tokenizerRef   

                      istateRef := istate'; 
                      cont := cont';
                      if !progress then dprintf "READER: cont = %O\n" !cont;
                  done ;
                  if !progress then dprintf "\n- READER: Exiting stdinReaderThread\n";  
                with e -> stopProcessingRecovery e range0;

            finally 
                // Reset the Culture code
                System.Threading.Thread.CurrentThread.CurrentCulture <- culture
                if !progress then dprintf "\n- READER: Exiting process because of failure/exit on  stdinReaderThread\n";  
                // On some flavors of Mono, calling exit may freeze the process if we're using the WinForms event handler
                // Basically, on Mono 2.6.3, the GUI thread may be left dangling on exit.  At that point:
                //   -- System.Environment.Exit will cause the process to hang
                //   -- Calling Application.Exit() will leave the GUI thread up and running, creating a Zombie process
                //   -- Calling Abort() on the Main thread or the GUI thread will have no effect, and the process will remain hung
                // Also, even the the GUI thread is up and running, the WinForms event loop will be listed as closed
                // In this case, killing the process is harmless, since we've already cleaned up after ourselves and FSI is responding
                // to an error.  (CTRL-C is handled elsewhere.) 
                // We'll only do this if we're running on Mono, "--gui" is specified and our input is piped in from stdin, so it's still
                // fairly constrained.
#if SILVERLIGHT
#else                                
                if runningOnMono && fsiInteractionProcessor.FsiOptions.Gui then
                  System.Environment.ExitCode <- 1
                  Process.GetCurrentProcess().Kill()
                else
                  exit 1
#endif                  

        ),Name="StdinReaderThread")
    // stdinReaderThread.IsBackground <- true; 
    if !progress then dprintf "MAIN: starting stdin thread...\n";
    stdinReaderThread.Start();


let DriveFsiEventLoop (fsiConsoleOutput: FsiConsoleOutput) = 
    let rec runLoop() = 
        if !progress then dprintf "GUI thread runLoop\n";
        let restart = 
            try 
              // BLOCKING POINT: The GUI Thread spends most (all) of its time this event loop
              if !progress then dprintf "MAIN:  entering event loop...\n";
              fsi.EventLoop.Run()
            with
            |  :? ThreadAbortException ->
              // If this TAE handler kicks it's almost certainly too late to save the
              // state of the process - the state of the message loop may have been corrupted 
              fsiConsoleOutput.uprintnfn "%s" (FSIstrings.SR.fsiUnexpectedThreadAbortException());  
#if SILVERLIGHT              
#else              
              (try Thread.ResetAbort() with _ -> ());
#endif              
              true
              // Try again, just case we can restart
            | e -> 
              stopProcessingRecovery e range0;
              true
              // Try again, just case we can restart
        if !progress then dprintf "MAIN:  exited event loop...\n";
        if restart then runLoop() 

    runLoop();

//----------------------------------------------------------------------------
// Main
// Mark the main thread as STAThread since it is a GUI thread
//----------------------------------------------------------------------------

type internal FsiEvaluationSession (argv) = 
    do if not runningOnMono then Lib.UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption() (* SDL recommendation *)
    // See Bug 735819 
    let lcidFromCodePage = 
#if SILVERLIGHT
#else         
        if (System.Console.OutputEncoding.CodePage <> 65001) &&
           (System.Console.OutputEncoding.CodePage <> System.Threading.Thread.CurrentThread.CurrentUICulture.TextInfo.OEMCodePage) &&
           (System.Console.OutputEncoding.CodePage <> System.Threading.Thread.CurrentThread.CurrentUICulture.TextInfo.ANSICodePage) then
                System.Threading.Thread.CurrentThread.CurrentUICulture <- new System.Globalization.CultureInfo("en-US")
                Some(1033)
        else
#endif        
            None

    let timeReporter = FsiTimeReporter()

    //----------------------------------------------------------------------------
    // Console coloring
    //----------------------------------------------------------------------------

    // Testing shows "console coloring" is broken on some Mono configurations (e.g. Mono 2.4 Suse LiveCD).
    // To support fsi usage, the console coloring is switched off by default on Mono.
    do if runningOnMono then enableConsoleColoring <- false 


    do SetUninitializedErrorLoggerFallback AssertFalseErrorLogger
  
    //----------------------------------------------------------------------------
    // tcConfig - build the initial config
    //----------------------------------------------------------------------------

    let defaultFSharpBinariesDir =
#if SILVERLIGHT
        "."
#else    
        System.AppDomain.CurrentDomain.BaseDirectory
#endif
    let currentDirectory =
#if SILVERLIGHT
        "."
#else
        Directory.GetCurrentDirectory()
#endif                

    let tcConfigB = Build.TcConfigBuilder.CreateNew(defaultFSharpBinariesDir, 
                                                    true, // long running: optimizeForMemory 
                                                    currentDirectory)
    let tcConfigP = TcConfigProvider.BasedOnMutableBuilder(tcConfigB)
    do tcConfigB.resolutionEnvironment <- MSBuildResolver.RuntimeLike // See Bug 3608
    do tcConfigB.useFsiAuxLib <- true

    // Preset: --optimize+ -g --tailcalls+ (see 4505)
    do SetOptimizeSwitch tcConfigB On
    
    // Mono 2.8 fails with "can't find a default symbol writer" when running on Windows, so turn off
    // debug symbol generation by default.
    let isMonoRunningOnWindows = 
        runningOnMono && 
        (match System.Environment.OSVersion.Platform with 
         | System.PlatformID.Win32NT 
         | System.PlatformID.Win32S 
         | System.PlatformID.Win32Windows 
         | System.PlatformID.WinCE -> true 
         | _ -> false)

    do SetDebugSwitch    tcConfigB (Some "pdbonly") (if isMonoRunningOnWindows then Off else On)
    do SetTailcallSwitch tcConfigB On    

    let fsiStdinSyphon = new FsiStdinSyphon()
    let errorLogger = ErrorLoggerThatStopsOnFirstError(tcConfigB,fsiStdinSyphon)

    do InstallErrorLoggingOnThisThread errorLogger // FSI error logging on main thread.

    let updateBannerText() =
#if FX_ATLEAST_40
      // See bug 6071 for product banner spec
      tcConfigB.productNameForBannerText <- FSIstrings.SR.fsiProductName(FSharpEnvironment.DotNetBuildString)
#else
      tcConfigB.productNameForBannerText <- FSIstrings.SR.fsiProductName(FSharpEnvironment.FSharpTeamVersionNumber)
#endif
  
    do updateBannerText() // setting the correct banner so that 'fsi -?' display the right thing

    let fsiConsoleOutput = FsiConsoleOutput(tcConfigB)
    let fsiOptions       = FsiCommandLineOptions(argv,tcConfigB, fsiConsoleOutput)
    let fsiConsolePrompt = FsiConsolePrompt(fsiOptions, fsiConsoleOutput)

    // Check if we have a codepage from the console
    do
      match fsiOptions.FsiLCID with
      | Some _ -> ()
      | None -> tcConfigB.lcid <- lcidFromCodePage

    // Set the ui culture
    do 
      match fsiOptions.FsiLCID with
      | Some(n) -> 
#if SILVERLIGHT
        System.Threading.Thread.CurrentThread.CurrentUICulture <- new System.Globalization.CultureInfo(n.ToString())
#else    
        System.Threading.Thread.CurrentThread.CurrentUICulture <- new System.Globalization.CultureInfo(n)
#endif        

      | None -> ()

    do 
      try 
          SetServerCodePages fsiOptions 
      with e -> 
          warning(e)

    do 
      updateBannerText() // resetting banner text after parsing options

      if tcConfigB.showBanner then 
          fsiOptions.ShowBanner()


    do fsiConsoleOutput.uprintfn ""

    // When no source files to load, print ahead prompt here 
    do if isNil  fsiOptions.SourceFiles then 
        fsiConsolePrompt.PrintAhead()       


    let fsiConsoleInput = FsiConsoleInput(fsiOptions)

    let tcGlobals,tcImports =  
#if SILVERLIGHT
      TcImports.BuildTcImports(tcConfigP) 
#else
      try 
          TcImports.BuildTcImports(tcConfigP) 
      with e -> 
          stopProcessingRecovery e range0; exit 1
#endif

    let ilGlobals  = tcGlobals.ilg

    let niceNameGen = NiceNameGenerator() 
    // Share intern'd strings across all lexing/parsing
    let lexResourceManager = new Lexhelp.LexResourceManager() 

    let tcLockObject = box 7 // any new object will do
          
    let fsiDynamicCompiler = FsiDynamicCompiler(timeReporter, tcConfigB, tcLockObject, errorLogger, tcImports, tcGlobals, ilGlobals, fsiOptions, fsiConsoleOutput, niceNameGen, MagicAssemblyResolution.resolvePathFromAssemblyRef tcImports) 

    let fsiInterruptController = FsiInterruptController(fsiOptions, fsiConsoleOutput) 

    do MagicAssemblyResolution.Install(tcConfigB, tcImports, fsiDynamicCompiler, fsiConsoleOutput)
    
    
    let initialInteractiveState = fsiDynamicCompiler.GetInitialInteractiveState ()

    let fsiStdinLexerProvider = FsiStdinLexerProvider(tcConfigB, fsiStdinSyphon, fsiConsoleInput, fsiOptions, lexResourceManager, errorLogger)
    //----------------------------------------------------------------------------
    // interactive state ref - most recent istate 
    //----------------------------------------------------------------------------

    let istateRef = ref initialInteractiveState
      
#if SILVERLIGHT
#else          
    let fsiIntellisenseProvider = FsiIntellisenseProvider(tcConfigB, tcGlobals, tcImports)

#endif        
    let fsiInteractionProcessor = FsiInteractionProcessor(tcConfigB, errorLogger, fsiOptions, fsiDynamicCompiler, fsiConsolePrompt, fsiConsoleOutput, fsiInterruptController, fsiStdinLexerProvider, lexResourceManager) 

    // Update the console completion function now we've got an initial type checking state.
    // This means completion doesn't work until the initial type checking state has finished loading - fair enough!
    member x.Interrupt() = fsiInterruptController.Interrupt() 

    // Update the console completion function now we've got an initial type checking state.
    // This means completion doesn't work until the initial type checking state has finished loading - fair enough!
    member x.Run() = 
#if SILVERLIGHT 
      let _ = fsiInterruptController.InstallKillThread(Thread.CurrentThread, 100)
      let istate = initialInteractiveState
      fsi.EventLoop <- Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSimpleEventLoop()
      let istate = fsiInteractionProcessor.LoadInitialFiles false istate
      StartStdinReadAndProcessThread(fsiOptions.FsiLCID, istateRef, istate, errorLogger, fsiConsoleInput, fsiStdinLexerProvider, fsiInteractionProcessor, true)

      DriveFsiEventLoop fsiConsoleOutput 
#else        
      match fsiConsoleInput.ConsoleOpt with 
      | Some console when fsiOptions.EnableConsoleKeyProcessing -> 
          console.SetCompletion(fun (s1,s2) -> fsiIntellisenseProvider.CompletionsForPartialLID !istateRef (match s1 with | Some s -> s + "." + s2 | None -> s2)  |> Seq.ofList)
      | _ -> ()


      if not runningOnMono && fsiOptions.IsInteractiveServer then 
          SpawnInteractiveServer (fsiOptions, fsiIntellisenseProvider, istateRef, fsiInterruptController, tcLockObject)

      use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Interactive)

      let istate = initialInteractiveState
        
      let threadException exn = 
           fsi.EventLoop.Invoke (
              fun () ->          
                  eprintfn "%s" (exn.ToString())
                  errorLogger.SetError()
                  errorLogger.AbortOnError()
              )

      if fsiOptions.Interact then 
          // page in the type check env 
          let istate = fsiInteractionProcessor.LoadDummyInteraction istate
          if !progress then dprintf "MAIN: InstallKillThread!\n";
          let exitViaKillThread = fsiInterruptController.InstallKillThread(Thread.CurrentThread ,(if fsiOptions.Gui then 400 else 100)) 
          if !progress then dprintf "MAIN: got initial state, creating form\n";

          // Route background exceptions to the exception handlers
          AppDomain.CurrentDomain.UnhandledException.Add (fun args -> match args.ExceptionObject with :? System.Exception as err -> threadException err | _ -> ());

          if fsiOptions.Gui then 
              try 
                  Application.EnableVisualStyles() 
              with _ -> 
                  ()

              // Route GUI application exceptions to the exception handlers
              Application.add_ThreadException(new ThreadExceptionEventHandler(fun _ args -> threadException args.Exception));

              if not runningOnMono then 
                  try 
                      TrySetUnhandledExceptionMode() 
                  with _ -> 
                      ();

              // This is the event loop for winforms
              fsi.EventLoop <- WinFormsEventLoop(tcConfigB, fsiStdinSyphon, fsiOptions.FsiLCID)
                                        
          let istate = fsiInteractionProcessor.LoadInitialFiles exitViaKillThread istate

          StartStdinReadAndProcessThread(fsiOptions.FsiLCID, istateRef, istate, errorLogger, fsiConsoleInput, fsiStdinLexerProvider, fsiInteractionProcessor, exitViaKillThread)            

          DriveFsiEventLoop fsiConsoleOutput 

      else // not interact
        let _istate = fsiInteractionProcessor.LoadInitialFiles false istate
        exit (min errorLogger.ErrorCount 1)

      // The Ctrl-C exception handler that we've passed to native code has
      // to be explicitly kept alive.
      GC.KeepAlive fsiInterruptController.EventHandlers

[<EntryPoint>]
[<STAThread()>]    
[<CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2004:RemoveCallsToGCKeepAlive")>]
let MainMain _ = 
    let argv = System.Environment.GetCommandLineArgs()
    let fsi = FsiEvaluationSession argv
    fsi.Run() 

    0
#endif // SILVERLIGHT





