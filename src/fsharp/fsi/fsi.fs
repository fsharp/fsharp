//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


module Microsoft.FSharp.Compiler.Interactive.Shell

#nowarn "55"

[<assembly: System.Runtime.InteropServices.ComVisible(false)>]
[<assembly: System.CLSCompliant(true)>]  
do()

open Internal.Utilities

module Tc = Microsoft.FSharp.Compiler.TypeChecker

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
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
open Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers
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
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Opt
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.PostTypecheckSemanticChecks

open Internal.Utilities.Collections
open Internal.Utilities.StructuredFormat
open Internal.Utilities.FileSystem

#if SILVERLIGHT
let internal exit (_ : int) = ()
#endif

//----------------------------------------------------------------------------
// Hardbinding dependencies should we NGEN fsi.exe
//----------------------------------------------------------------------------

#if SILVERLIGHT
#else
open System.Runtime.CompilerServices
[<Dependency("FSharp.Compiler",LoadHint.Always)>] do ()
[<Dependency("FSharp.Core",LoadHint.Always)>] do ()
#endif

module internal Utilities = 
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
type internal FsiTimeReporter(outWriter: TextWriter) =
    member tr.TimeOp(f) =
        let start = System.DateTime.Now
        let res = f()
        let stop = System.DateTime.Now
        fprintfn outWriter "Real: %s" ((stop - start).ToString())
        res

    member tr.TimeOpIf flag f = if flag then tr.TimeOp f else f ()
#else
[<AutoSerializable(false)>]
type internal FsiTimeReporter(outWriter: TextWriter) =
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
        fprintfn outWriter "%s" (FSIstrings.SR.fsiTimeInfoMainString((sprintf "%02d:%02d:%02d.%03d" elapsed.Hours elapsed.Minutes elapsed.Seconds elapsed.Milliseconds),(sprintf "%02d:%02d:%02d.%03d" total.Hours total.Minutes total.Seconds total.Milliseconds),(String.concat ", " (List.mapi (sprintf "%s%d: %d" (FSIstrings.SR.fsiTimeInfoGCGenerationLabelSomeShorthandForTheWordGeneration())) spanGC))))
        res

    member tr.TimeOpIf flag f = if flag then tr.TimeOp f else f ()
#endif

//----------------------------------------------------------------------------
// value printing
//----------------------------------------------------------------------------

type internal FsiValuePrinterMode = 
    | PrintExpr 
    | PrintDecl

/// Used to print value signatures along with their values, according to the current
/// set of pretty printers installed in the system, and default printing rules.
type internal FsiValuePrinter(ilGlobals, generateDebugInfo, resolvePath, outWriter) = 

    /// This printer is used by F# Interactive if no other printers apply.
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


    /// Get the print options used when formatting output using the structured printer.
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

    /// Get the evaluation context used when inverting the storage mapping of the ILRuntimeWriter.
    member __.GetEvaluationContext emEnv = 
        { LookupFieldRef = ILRuntimeWriter.LookupFieldRef emEnv >> Option.get
          LookupMethodRef = ILRuntimeWriter.LookupMethodRef emEnv >> Option.get
          LookupTypeRef = ILRuntimeWriter.LookupTypeRef emEnv >> Option.get
          LookupType = ILRuntimeWriter.LookupType { ilg = ilGlobals ; generatePdb = generateDebugInfo; resolvePath=resolvePath } emEnv }

    /// Generate a layout for an actual F# value, where we know the value has the given static type.
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
            
    /// Display the signature of an F# value declaration, along with its actual value.
    member valuePrinter.InvokeDeclLayout (emEnv, ilxGenerator: Ilxgen.IlxAssemblyGenerator, v:Val) =
        // Implemented via a lookup from v to a concrete (System.Object,System.Type).
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
                try  ilxGenerator.LookupGeneratedValue (valuePrinter.GetEvaluationContext emEnv, v)
                with e -> 
                    assert false
#if DEBUG
                    //fprintfn fsiConsoleOutput.Out "lookGenerateVal: failed on v=%+A v.Name=%s" v v.LogicalName
#endif
                    None // lookup may fail 
            match res with
              | None             -> None
              | Some (obj,objTy) -> 
                  let lay = valuePrinter.PrintValue (FsiValuePrinterMode.PrintDecl, opts, obj, objTy)
                  if isEmptyL lay then None else Some lay // suppress empty layout 
                                    
        else
            None
    
    /// Fetch the saved value of an expression out of the 'it' register and show it.
    member valuePrinter.InvokeExprPrinter (denv, vref) = 
        let opts        = valuePrinter.GetFsiPrintOptions()
        let savedIt     = Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSavedIt()
        let savedItType = Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSavedItType()
        let rhsL  = valuePrinter.PrintValue (FsiValuePrinterMode.PrintExpr, opts, savedIt, savedItType)
        let denv = { denv with suppressMutableKeyword = true } // suppress 'mutable' in 'val mutable it = ...'
        let fullL = if isEmptyL rhsL then
                      NicePrint.layoutValOrMember denv vref (* the rhs was suppressed by the printer, so no value to print *)
                    else
                      (NicePrint.layoutValOrMember denv vref ++ wordL "=") --- rhsL
        Internal.Utilities.StructuredFormat.Display.output_layout opts outWriter fullL;  
        outWriter.WriteLine()
    


/// Used to make a copy of input in order to include the input when displaying the error text.
type internal FsiStdinSyphon(errorWriter: TextWriter) = 
    let syphonText = new StringBuilder()

    /// Clears the syphon text
    member x.Reset () = 
#if FX_ATLEAST_40
        syphonText.Clear() |> ignore
#else
        syphonText.Remove(0,syphonText.Length) |> ignore
#endif

    /// Adds a new line to the syphon text
    member x.Add (str:string) = 
        syphonText.Append str |> ignore  

    /// Gets the indicated line in the syphon text
    member x.GetLine filename i =
        if filename <> Lexhelp.stdinMockFilename then 
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
            let lines = text.Split '\n'
            if 0 < i && i <= lines.Length then lines.[i-1] else ""

    /// Display the given error.
    member syphon.PrintError (tcConfig:TcConfigBuilder, isWarn, err) = 
        Utilities.ignoreAllErrors (fun () -> 
            DoWithErrorColor isWarn  (fun () ->
                errorWriter.WriteLine();
                writeViaBufferWithEnvironmentNewLines errorWriter (OutputErrorOrWarningContext "  " syphon.GetLine) err; 
                writeViaBufferWithEnvironmentNewLines errorWriter (OutputErrorOrWarning (tcConfig.implicitIncludeDir,tcConfig.showFullPaths,tcConfig.flatErrors,tcConfig.errorStyle,false))  err;
                errorWriter.WriteLine()))


   
/// Encapsulates functions used to write to outWriter and errorWriter
type internal FsiConsoleOutput(tcConfigB, outWriter:TextWriter, errorWriter:TextWriter) = 

    let nullOut = new StreamWriter(Stream.Null) :> TextWriter
    let fprintfnn (os: TextWriter) fmt  = Printf.kfprintf (fun _ -> os.WriteLine(); os.WriteLine()) os fmt   
    /// uprintf to write usual responses to stdout (suppressed by --quiet), with various pre/post newlines
    member out.uprintf    fmt = fprintf   (if tcConfigB.noFeedback then nullOut else outWriter) fmt 
    member out.uprintfn   fmt = fprintfn  (if tcConfigB.noFeedback then nullOut else outWriter) fmt
    member out.uprintfnn  fmt = fprintfnn (if tcConfigB.noFeedback then nullOut else outWriter) fmt
    member out.uprintnf   fmt = out.uprintfn ""; out.uprintf   fmt
    member out.uprintnfn  fmt = out.uprintfn ""; out.uprintfn  fmt
    member out.uprintnfnn fmt = out.uprintfn ""; out.uprintfnn fmt
      
    member out.Out = outWriter
    member out.Error = errorWriter


/// This ErrorLogger reports all warnings, but raises StopProcessing on first error or early exit
type internal ErrorLoggerThatStopsOnFirstError(tcConfigB:TcConfigBuilder, fsiStdinSyphon:FsiStdinSyphon, fsiConsoleOutput: FsiConsoleOutput) = 
    inherit ErrorLogger("ErrorLoggerThatStopsOnFirstError")
    let mutable errors = 0 
    member x.SetError() = 
        errors <- 1
    member x.ErrorSinkHelper(err) = 
        fsiStdinSyphon.PrintError(tcConfigB,false,err)
        errors <- errors + 1
        if tcConfigB.abortOnError then exit 1 (* non-zero exit code *)
        // STOP ON FIRST ERROR (AVOIDS PARSER ERROR RECOVERY)
        raise StopProcessing 
    
    member x.CheckForErrors() = (errors > 0)
    member x.ResetErrorCount() = (errors <- 0)
    
    override x.WarnSinkImpl(err) = 
        DoWithErrorColor true (fun () -> 
            if ReportWarningAsError tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn tcConfigB.specificWarnAsError tcConfigB.specificWarnAsWarn tcConfigB.globalWarnAsError err then 
                x.ErrorSinkHelper err 
            elif ReportWarning tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn err then 
                fsiConsoleOutput.Error.WriteLine()
                writeViaBufferWithEnvironmentNewLines fsiConsoleOutput.Error (OutputErrorOrWarningContext "  " fsiStdinSyphon.GetLine) err
                writeViaBufferWithEnvironmentNewLines fsiConsoleOutput.Error (OutputErrorOrWarning (tcConfigB.implicitIncludeDir,tcConfigB.showFullPaths,tcConfigB.flatErrors,tcConfigB.errorStyle,true)) err
                fsiConsoleOutput.Error.WriteLine())

    override x.ErrorSinkImpl err = x.ErrorSinkHelper err
    override x.ErrorCount = errors

    /// A helper function to check if its time to abort
    member x.AbortOnError() = 
        if errors > 0 then 
            fprintf fsiConsoleOutput.Error  "%s" (FSIstrings.SR.stoppedDueToError())
            fsiConsoleOutput.Error.Flush()
            raise StopProcessing 

/// Get the directory name from a string, with some defaults if it doesn't have one
let internal directoryName (s:string) = 
    if s = "" then "."
    else 
        match Path.GetDirectoryName s with 
        | null -> if FileSystem.IsPathRootedShim s then s else "."
        | res -> if res = "" then "." else res




//----------------------------------------------------------------------------
// cmd line - state for options
//----------------------------------------------------------------------------

/// Process the command line options 
type internal FsiCommandLineOptions(argv: string[], tcConfigB, fsiConsoleOutput: FsiConsoleOutput) = 
    let mutable enableConsoleKeyProcessing = 
       // Mono on Win32 doesn't implement correct console processing
       not (runningOnMono && System.Environment.OSVersion.Platform = System.PlatformID.Win32NT) 
#if MONO
    let mutable gui        = false // override via "--gui", off by default
#else
    let mutable gui        = true // override via "--gui", on by default
#endif
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
    let mutable probeToSeeIfConsoleWorks         = true 
    let mutable peekAheadOnConsoleToPermitTyping = true   


    let isInteractiveServer() = fsiServerName <> ""  
    let recordExplicitArg arg = explicitArgs <- explicitArgs @ [arg]

    let executableFileName = 
        lazy 
#if SILVERLIGHT
            "fsi.exe"
#else
            let currentProcess = System.Diagnostics.Process.GetCurrentProcess()
            Path.GetFileName(currentProcess.MainModule.FileName)
#endif


    // Additional fsi options are list below.
    // In the "--help", these options can be printed either before (fsiUsagePrefix) or after (fsiUsageSuffix) the core options.

    let displayHelpFsi tcConfigB (blocks:CompilerOptionBlock list) =
        DisplayBannerText tcConfigB;
        fprintfn fsiConsoleOutput.Out ""
        fprintfn fsiConsoleOutput.Out "%s" (FSIstrings.SR.fsiUsage(executableFileName.Value))
        printCompilerOptionBlocks blocks
        exit 0

    // option tags
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
        [// Make internal fsi-server* options. Do not print in the help. They are used by VFSI. 
         CompilerOption("fsi-server","", OptionString (fun s -> fsiServerName <- s), None, None); // "FSI server mode on given named channel");
         CompilerOption("fsi-server-input-codepage","",OptionInt (fun n -> fsiServerInputCodePage <- Some(n)), None, None); // " Set the input codepage for the console"); 
         CompilerOption("fsi-server-output-codepage","",OptionInt (fun n -> fsiServerOutputCodePage <- Some(n)), None, None); // " Set the output codepage for the console"); 
         CompilerOption("fsi-server-no-unicode","", OptionUnit (fun () -> fsiServerOutputCodePage <- None;  fsiServerInputCodePage <- None), None, None); // "Do not set the codepages for the console");
         CompilerOption("fsi-server-lcid","", OptionInt (fun n -> fsiLCID <- Some(n)), None, None); // "LCID from Visual Studio"

         // We do not want to print the "script.fsx arg2..." as part of the options 
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
         // Private options, related to diagnostics around console probing 
         CompilerOption("probeconsole","", OptionSwitch (fun flag -> probeToSeeIfConsoleWorks <- flag=On), None, None); // "Probe to see if Console looks functional");
         CompilerOption("peekahead","", OptionSwitch (fun flag -> peekAheadOnConsoleToPermitTyping <- flag=On), None, None); // "Probe to see if Console looks functional");
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


    /// Process command line, flags and collect filenames.
    /// The ParseCompilerOptions function calls imperative function to process "real" args 
    /// Rather than start processing, just collect names, then process them. 
    let sourceFiles = 
        let collect name = 
            let fsx = Build.IsScript name
            inputFilesAcc <- inputFilesAcc @ [(name,fsx)] // O(n^2), but n small...
        try 
           let fsiCompilerOptions = fsiUsagePrefix tcConfigB @ GetCoreFsiCompilerOptions tcConfigB @ fsiUsageSuffix tcConfigB
           let abbrevArgs = abbrevFlagSet tcConfigB false
           ParseCompilerOptions collect fsiCompilerOptions (List.tail (PostProcessCompilerArgs abbrevArgs argv))
        with e ->
            stopProcessingRecovery e range0; exit 1;
        inputFilesAcc

#if SILVERLIGHT
#else
    do 
        if tcConfigB.utf8output then
            let prev = Console.OutputEncoding
            Console.OutputEncoding <- System.Text.Encoding.UTF8
            System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> Console.OutputEncoding <- prev)
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
        let helpLine = sprintf "%s --help" (Path.GetFileNameWithoutExtension executableFileName.Value)

        fsiConsoleOutput.uprintfn  ""
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader1directives());
        fsiConsoleOutput.uprintfn  "    #r \"file.dll\";;        %s" (FSIstrings.SR.fsiIntroTextHashrInfo());
        fsiConsoleOutput.uprintfn  "    #I \"path\";;            %s" (FSIstrings.SR.fsiIntroTextHashIInfo());
        fsiConsoleOutput.uprintfn  "    #load \"file.fs\" ...;;  %s" (FSIstrings.SR.fsiIntroTextHashloadInfo());
        fsiConsoleOutput.uprintfn  "    #time [\"on\"|\"off\"];;   %s" (FSIstrings.SR.fsiIntroTextHashtimeInfo());
        fsiConsoleOutput.uprintfn  "    #help;;                %s" (FSIstrings.SR.fsiIntroTextHashhelpInfo());
        fsiConsoleOutput.uprintfn  "    #quit;;                %s" (FSIstrings.SR.fsiIntroTextHashquitInfo()); (* last thing you want to do, last thing in the list - stands out more *)
        fsiConsoleOutput.uprintfn  "";
        fsiConsoleOutput.uprintfnn "%s" (FSIstrings.SR.fsiIntroTextHeader2commandLine());
        fsiConsoleOutput.uprintfn  "%s" (FSIstrings.SR.fsiIntroTextHeader3(helpLine));
        fsiConsoleOutput.uprintfn  "";
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

/// Set the current ui culture for the current thread.
let internal SetCurrentUICultureForThread (lcid : int option) =
    match lcid with
    | Some n -> Thread.CurrentThread.CurrentUICulture <- 
#if SILVERLIGHT    
                                                new CultureInfo(n.ToString())
#else
                                                new CultureInfo(n)
#endif                                                                                                
    | None -> ()


//----------------------------------------------------------------------------
// Reporting - warnings, errors
//----------------------------------------------------------------------------

let internal InstallErrorLoggingOnThisThread errorLogger =
    if !progress then dprintfn "Installing logger on id=%d name=%s" Thread.CurrentThread.ManagedThreadId Thread.CurrentThread.Name
    SetThreadErrorLoggerNoUnwind(errorLogger)
    SetThreadBuildPhaseNoUnwind(BuildPhase.Interactive)

/// Set the input/output encoding. The use of a thread is due to a known bug on 
/// on Vista where calls to Console.InputEncoding can block the process.
let internal SetServerCodePages(fsiOptions: FsiCommandLineOptions) =     
#if SILVERLIGHT
    ignore fsiOptions
#else     
    match fsiOptions.FsiServerInputCodePage, fsiOptions.FsiServerOutputCodePage with 
    | None,None -> ()
    | inputCodePageOpt,outputCodePageOpt -> 
        let successful = ref false 
        Async.Start (async { do match inputCodePageOpt with 
                                | None -> () 
                                | Some(n:int) ->
                                      let encoding = System.Text.Encoding.GetEncodingShim n
                                      // Note this modifies the real honest-to-goodness settings for the current shell.
                                      // and the modifiations hang around even after the process has exited.
                                      Console.InputEncoding <- encoding
                             do match outputCodePageOpt with 
                                | None -> () 
                                | Some(n:int) -> 
                                      let encoding = System.Text.Encoding.GetEncodingShim n
                                      // Note this modifies the real honest-to-goodness settings for the current shell.
                                      // and the modifiations hang around even after the process has exited.
                                      Console.OutputEncoding <- encoding
                             do successful := true  });
        for pause in [10;50;100;1000;2000;10000] do 
            if not !successful then 
                Thread.Sleep(pause);
        if not !successful then 
            System.Windows.Forms.MessageBox.Show(FSIstrings.SR.fsiConsoleProblem()) |> ignore
#endif



//----------------------------------------------------------------------------
// Prompt printing
//----------------------------------------------------------------------------

type internal FsiConsolePrompt(fsiOptions: FsiCommandLineOptions, fsiConsoleOutput: FsiConsoleOutput) =

    // A prompt gets "printed ahead" at start up. Tells users to start type while initialisation completes.
    // A prompt can be skipped by "silent directives", e.g. ones sent to FSI by VS.
    let mutable dropPrompt = 0
    // NOTE: SERVER-PROMPT is not user displayed, rather it's a prefix that code elsewhere 
    // uses to identify the prompt, see vs\FsPkgs\FSharp.VS.FSI\fsiSessionToolWindow.fs
    let prompt = if fsiOptions.IsInteractiveServer then "SERVER-PROMPT>\n" else "> "  

    member __.Print()      = if dropPrompt = 0 then fsiConsoleOutput.uprintf "%s" prompt else dropPrompt <- dropPrompt - 1
    member __.PrintAhead() = dropPrompt <- dropPrompt + 1; fsiConsoleOutput.uprintf "%s" prompt
    member __.SkipNext()   = dropPrompt <- dropPrompt + 1    
    member __.FsiOptions = fsiOptions



//----------------------------------------------------------------------------
// Startup processing
//----------------------------------------------------------------------------
type internal FsiConsoleInput(fsiOptions: FsiCommandLineOptions, inReader: TextReader, outWriter: TextWriter) =

#if SILVERLIGHT
#else
    let consoleLooksOperational() =
        if fsiOptions.ProbeToSeeIfConsoleWorks then 
            if !progress then fprintfn outWriter "probing to see if console works..."
            try
                // Probe to see if the console looks functional on this version of .NET
                let _ = Console.KeyAvailable 
                let c1 = Console.ForegroundColor
                let c2 = Console.BackgroundColor
                let _ = Console.CursorLeft <- Console.CursorLeft
                if !progress then fprintfn outWriter "probe succeeded, we might have a console, comparing foreground (%A) and background (%A) colors, if they are the same then we're running in emacs or VS on unix and we turn off readline by default..." c1 c2
                c1 <> c2
            with _ -> 
                if !progress then fprintfn outWriter "probe failed, we have no console..."
                (* warning(Failure("Note: there was a problem setting up custom readline console support. Consider starting fsi.exe with the --no-readline option")); *)
                false
        else
            true 
#endif

    let consoleOpt =
#if SILVERLIGHT
        None
#else                
        // The "console.fs" code does a limited form of "TAB-completion".
        // Currently, it turns on if it looks like we have a console.
        if fsiOptions.EnableConsoleKeyProcessing && consoleLooksOperational() then
            Some(new Microsoft.FSharp.Compiler.Interactive.ReadLineConsole())
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
                      if !progress then fprintfn outWriter "first-line-reader-thread reading first line...";
                      firstLine <- Some(console.ReadLine()); 
                      if !progress then fprintfn outWriter "first-line-reader-thread got first line = %A..." firstLine;
                  consoleReaderStartupDone.Set() |> ignore 
                  if !progress then fprintfn outWriter "first-line-reader-thread has set signal and exited." ;
              | _ -> 
                  ignore(inReader.Peek());
                  consoleReaderStartupDone.Set() |> ignore 
            )).Start()
         else
#endif                  
           if !progress then fprintfn outWriter "first-line-reader-thread not in use."
           consoleReaderStartupDone.Set() |> ignore

    /// Try to get the first line, if we snarfed it while probing.
    member __.TryGetFirstLine() = let r = firstLine in firstLine <- None; r

    /// Try to get the console, if it appears operational.
    member __.TryGetConsole() = consoleOpt

    member __.In = inReader

    member __.WaitForInitialConsoleInput() = WaitHandle.WaitAll [| consoleReaderStartupDone  |] |> ignore;
    

//----------------------------------------------------------------------------
// FsiDynamicCompilerState
//----------------------------------------------------------------------------

[<AutoSerializable(false)>]
[<NoEquality; NoComparison>]
type internal FsiDynamicCompilerState =
    { optEnv    : Opt.IncrementalOptimizationEnv
      emEnv     : ILRuntimeWriter.emEnv
      tcGlobals : Env.TcGlobals
      tcState   : Build.TcState 
      ilxGenerator : Ilxgen.IlxAssemblyGenerator
      // Why is this not in FsiOptions?
      timing    : bool }

let internal WithImplicitHome (tcConfigB, dir) f = 
    let old = tcConfigB.implicitIncludeDir 
    tcConfigB.implicitIncludeDir <- dir;
    try f() 
    finally tcConfigB.implicitIncludeDir <- old



/// Encapsulates the coordination of the typechecking, optimization and code generation
/// components of the F# compiler for interactively executed fragments of code.
///
/// A single instance of this object is created per interactive session.
type internal FsiDynamicCompiler
                       (timeReporter : FsiTimeReporter, 
                        tcConfigB, 
                        tcLockObject : obj, 
                        errorLogger: ErrorLoggerThatStopsOnFirstError, 
                        outWriter: TextWriter,
                        tcImports: TcImports, 
                        tcGlobals: TcGlobals, 
                        ilGlobals: ILGlobals, 
                        fsiOptions : FsiCommandLineOptions,
                        fsiConsoleOutput : FsiConsoleOutput,
                        niceNameGen,
                        resolvePath) = 

    let outfile = "TMPFSCI.exe"
    let assemblyName = "FSI-ASSEMBLY"

    let mutable fragmentId = 0
    let mutable prevIt : ValRef option = None

    let generateDebugInfo = tcConfigB.debuginfo

    let valuePrinter = FsiValuePrinter(ilGlobals, generateDebugInfo, resolvePath, outWriter)

    let assemblyBuilder,moduleBuilder = ILRuntimeWriter.mkDynamicAssemblyAndModule (assemblyName, tcConfigB.optSettings.localOpt(), generateDebugInfo)

    let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

#if SILVERLIGHT
#else
    let _writer = moduleBuilder.GetSymWriter()
#endif

    let infoReader = InfoReader(tcGlobals,tcImports.GetImportMap())    

    /// Add attributes 
    let CreateModuleFragment (tcConfigB, assemblyName, codegenResults) =
        if !progress then fprintfn fsiConsoleOutput.Out "Creating main module...";
        let mainModule = mkILSimpleModule assemblyName (fsharpModuleName tcConfigB.target assemblyName) (tcConfigB.target = Dll) tcConfigB.subsystemVersion tcConfigB.useHighEntropyVA (mkILTypeDefs codegenResults.ilTypeDefs) None None 0x0 (mkILExportedTypes []) ""
        { mainModule 
          with Manifest = 
                (let man = mainModule.ManifestOfAssembly
                 Some { man with  CustomAttrs = mkILCustomAttrs codegenResults.ilAssemAttrs }); }

    let ProcessInputs(istate: FsiDynamicCompilerState, inputs: ParsedInput list, showTypes: bool, isIncrementalFragment: bool, isInteractiveItExpr: bool, prefixPath: LongIdent) =
        let optEnv    = istate.optEnv
        let emEnv     = istate.emEnv
        let tcState   = istate.tcState
        let ilxGenerator = istate.ilxGenerator
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)

        // Typecheck. The lock stops the type checker running at the same time as the 
        // server intellisense implementation (which is currently incomplete and #if disabled)
        let (tcState:TcState),topCustomAttrs,declaredImpls,tcEnvAtEndOfLastInput =
            lock tcLockObject (fun _ -> TypecheckClosedInputSet(errorLogger.CheckForErrors,tcConfig,tcImports,tcGlobals, Some prefixPath,tcState,inputs))

#if DEBUG
        // Logging/debugging
        if tcConfig.printAst then
            let (TAssembly(declaredImpls)) = declaredImpls
            for input in declaredImpls do 
                fprintfn fsiConsoleOutput.Out "AST:" 
                fprintfn fsiConsoleOutput.Out "%+A" input
#endif

        errorLogger.AbortOnError();
         
        let importMap = tcImports.GetImportMap()

        // optimize: note we collect the incremental optimization environment 
        let optimizedImpls, _optData, optEnv = ApplyAllOptimizations (tcConfig, tcGlobals, (LightweightTcValForUsingInBuildMethodCall tcGlobals), outfile, importMap, isIncrementalFragment, optEnv, tcState.Ccu, declaredImpls)
        errorLogger.AbortOnError();
            
        let fragName = textOfLid prefixPath 
        let codegenResults = GenerateIlxCode (IlReflectBackend, isInteractiveItExpr, runningOnMono, tcConfig, topCustomAttrs, optimizedImpls, fragName, true, ilxGenerator)
        errorLogger.AbortOnError();

        // Each input is like a small separately compiled extension to a single source file. 
        // The incremental extension to the environment is dictated by the "signature" of the values as they come out 
        // of the type checker. Hence we add the declaredImpls (unoptimized) to the environment, rather than the 
        // optimizedImpls. 
        ilxGenerator.AddIncrementalLocalAssemblyFragment (isIncrementalFragment, fragName, declaredImpls)

        ReportTime tcConfig "TAST -> ILX";
        errorLogger.AbortOnError();
            
        ReportTime tcConfig "Linking";
        let ilxMainModule = CreateModuleFragment (tcConfigB, assemblyName, codegenResults)

        errorLogger.AbortOnError();
            
        ReportTime tcConfig "ILX -> IL (Unions)"; 
        let ilxMainModule = EraseIlxUnions.ConvModule ilGlobals ilxMainModule
        ReportTime tcConfig "ILX -> IL (Funcs)"; 
        let ilxMainModule = EraseIlxFuncs.ConvModule ilGlobals ilxMainModule 

        errorLogger.AbortOnError();   
              
        ReportTime tcConfig "Assembly refs Normalised"; 
        let mainmod3 = Morphs.morphILScopeRefsInILModuleMemoized ilGlobals (NormalizeAssemblyRefs tcImports) ilxMainModule
        errorLogger.AbortOnError();

#if SILVERLIGHT
#else
#if DEBUG
        if fsiOptions.ShowILCode then 
            fsiConsoleOutput.uprintnfn "--------------------";
            ILAsciiWriter.output_module outWriter mainmod3;
            fsiConsoleOutput.uprintnfn "--------------------"
#endif
#endif
        ignore fsiOptions

        ReportTime tcConfig "Reflection.Emit";
        let emEnv,execs = ILRuntimeWriter.emitModuleFragment(ilGlobals, emEnv, assemblyBuilder, moduleBuilder, mainmod3, generateDebugInfo, resolvePath)

        errorLogger.AbortOnError();

        // Explicitly register the resources with the QuotationPickler module 
        // We would save them as resources into the dynamic assembly but there is missing 
        // functionality System.Reflection for dynamic modules that means they can't be read back out 
        for bytes in codegenResults.quotationResourceBytes do 
            Microsoft.FSharp.Quotations.Expr.RegisterReflectedDefinitions (assemblyBuilder, fragName, bytes);
            

        ReportTime tcConfig "Run Bindings";
        timeReporter.TimeOpIf istate.timing (fun () -> 
          execs |> List.iter (fun exec -> 
            match exec() with 
            | Some err ->         
                fprintfn fsiConsoleOutput.Error "%s" (err.ToString())
                errorLogger.SetError()
                errorLogger.AbortOnError()

            | None -> ())) ;

        errorLogger.AbortOnError();

        // Echo the decls (reach inside wrapping)
        // This code occurs AFTER the execution of the declarations.
        // So stored values will have been initialised, modified etc.
        if showTypes && not tcConfig.noFeedback then  
            let denv = tcState.TcEnvFromImpls.DisplayEnv
            let denv = 
                if isIncrementalFragment then
                  // Extend denv with a (Val -> layout option) function for printing of val bindings.
                  {denv with generatedValueLayout = (fun v -> valuePrinter.InvokeDeclLayout (emEnv, ilxGenerator, v)) }
                else
                  // With #load items, the vals in the inferred signature do not tie up with those generated. Disable printing.
                  denv 

            // 'Open' the path for the fragment we just compiled for any future printing.
            let denv = denv.AddOpenPath (pathOfLid prefixPath) 

            let (TAssembly(declaredImpls)) = declaredImpls
            for (TImplFile(_qname,_,mexpr,_,_)) in declaredImpls do
                let responseL = NicePrint.layoutInferredSigOfModuleExpr false denv infoReader AccessibleFromSomewhere rangeStdin mexpr 
                if not (Layout.isEmptyL responseL) then      
                    fsiConsoleOutput.uprintfn "";
                    let opts = valuePrinter.GetFsiPrintOptions()
                    let responseL = Internal.Utilities.StructuredFormat.Display.squash_layout opts responseL
                    Layout.renderL (Layout.channelR outWriter) responseL |> ignore
                    fsiConsoleOutput.uprintfnn ""

        // Build the new incremental state.
        let istate = {istate with  optEnv    = optEnv;
                                   emEnv     = emEnv;
                                   ilxGenerator = ilxGenerator;
                                   tcState   = tcState  }
        
        // Return the new state and the environment at the end of the last input, ready for further inputs.
        (istate,tcEnvAtEndOfLastInput)

    let nextFragmentId() = fragmentId <- fragmentId + 1; fragmentId

    let mkFragmentPath  i = 
        // NOTE: this text shows in exn traces and type names. Make it clear and fixed width 
        [mkSynId rangeStdin (FsiDynamicModulePrefix + sprintf "%04d" i)]

    member __.DynamicAssemblyName = assemblyName
    member __.DynamicAssembly = (assemblyBuilder :> Assembly)

    member __.EvalParsedSourceFiles (istate, inputs) =
        let i = nextFragmentId()
        let prefix = mkFragmentPath i 
        // Ensure the path includes the qualifying name 
        let inputs = inputs |> List.map (PrependPathToInput prefix) 
        let istate,_ = ProcessInputs (istate, inputs, true, false, false, prefix)
        istate

    /// Evaluate the given definitions and produce a new interactive state.
    member __.EvalParsedDefinitions (istate, showTypes, isInteractiveItExpr, defs: SynModuleDecls) =
        let filename = Lexhelp.stdinMockFilename
        let i = nextFragmentId()
        let prefix = mkFragmentPath i
        let prefixPath = pathOfLid prefix
        let impl = SynModuleOrNamespace(prefix,(* isModule: *) true,defs,PreXmlDoc.Empty,[],None,rangeStdin)
        let input = ParsedInput.ImplFile(ParsedImplFileInput(filename,true, QualFileNameOfUniquePath (rangeStdin,prefixPath),[],[],[impl],true (* isLastCompiland *) ))
        let istate,tcEnvAtEndOfLastInput = ProcessInputs (istate, [input], showTypes, true, isInteractiveItExpr, prefix)
        let tcState = istate.tcState 
        { istate with tcState = tcState.NextStateAfterIncrementalFragment(tcEnvAtEndOfLastInput) }
      
     
    /// Evaluate the given expression and produce a new interactive state.
    member fsiDynamicCompiler.EvalParsedExpression (istate, expr: SynExpr) =
        let tcConfig = TcConfig.Create (tcConfigB, validate=false)
        let itName = "it" 

        // Construct the code that saves the 'it' value into the 'SaveIt' register.
        let defs = fsiDynamicCompiler.BuildItBinding expr

        // Evaluate the overall definitions.
        let istate = fsiDynamicCompiler.EvalParsedDefinitions (istate, false, true, defs)
        // Snarf the type for 'it' via the binding
        match istate.tcState.TcEnvFromImpls.NameEnv.FindUnqualifiedItem itName with 
        | Nameres.Item.Value vref -> 
             if not tcConfig.noFeedback then 
                 valuePrinter.InvokeExprPrinter (istate.tcState.TcEnvFromImpls.DisplayEnv, vref.Deref)
             
             /// Clear the value held in the previous "it" binding, if any, as long as it has never been referenced.
             match prevIt with
             | Some prevVal when not prevVal.Deref.HasBeenReferenced -> 
                 istate.ilxGenerator.ClearGeneratedValue (valuePrinter.GetEvaluationContext istate.emEnv, prevVal.Deref)
             | _ -> ()
             prevIt <- Some vref
        | _ -> ()

        // Return the interactive state.
        istate

    // Construct the code that saves the 'it' value into the 'SaveIt' register.
    member __.BuildItBinding (expr: SynExpr) =
        let m = expr.Range
        let itName = "it" 

        let itID  = mkSynId m itName
        let itExp = SynExpr.Ident itID
        let mkBind pat expr = Binding (None, DoBinding, false, (*mutable*)false, [], PreXmlDoc.Empty, SynInfo.emptySynValData, pat, None, expr, m, NoSequencePointAtInvisibleBinding)
        let bindingA = mkBind (mkSynPatVar None itID) expr (* let it = <expr> *)  // NOTE: the generalizability of 'expr' must not be damaged, e.g. this can't be an application 
        let saverPath  = ["Microsoft";"FSharp";"Compiler";"Interactive";"RuntimeHelpers";"SaveIt"]
        let dots = List.replicate (saverPath.Length - 1) rangeStdin
        let bindingB = mkBind (SynPat.Wild m) (SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.LongIdent(false, LongIdentWithDots(List.map (mkSynId rangeStdin) saverPath,dots),None,m), itExp,m)) (* let _  = saverPath it *)
        let defA = SynModuleDecl.Let (false, [bindingA], m)
        let defB = SynModuleDecl.Let (false, [bindingB], m)
        
        [defA; defB]

    member __.EvalRequireReference istate m path = 
        if Path.IsInvalidPath(path) then
            error(Error(FSIstrings.SR.fsiInvalidAssembly(path),m))
        // Check the file can be resolved before calling requireDLLReference 
        let resolutions = tcImports.ResolveAssemblyReference(AssemblyReference(m,path),ResolveAssemblyReferenceMode.ReportErrors)
        tcConfigB.AddReferencedAssemblyByPath(m,path)
        let tcState = istate.tcState 
        let tcEnv,(_dllinfos,ccuinfos) = 
            try
                RequireDLL tcImports tcState.TcEnvFromImpls m path 
            with e ->
                tcConfigB.RemoveReferencedAssemblyByPath(m,path)
                reraise()
        let optEnv = List.fold (AddExternalCcuToOpimizationEnv tcGlobals) istate.optEnv ccuinfos
        istate.ilxGenerator.AddExternalCcus (ccuinfos |> List.map (fun ccuinfo -> ccuinfo.FSharpViewOfMetadata)) 
        resolutions,
        { istate with tcState = tcState.NextStateAfterIncrementalFragment(tcEnv); optEnv = optEnv }

    member fsiDynamicCompiler.ProcessMetaCommandsFromInputAsInteractiveCommands istate sourceFile inp =
        WithImplicitHome
           (tcConfigB, directoryName sourceFile) 
           (fun () ->
               ProcessMetaCommandsFromInput 
                   ((fun st (m,nm) -> tcConfigB.TurnWarningOff(m,nm); st),
                    (fun st (m,nm) -> snd (fsiDynamicCompiler.EvalRequireReference st m nm)),
                    (fun _ _ -> ()))  
                   tcConfigB 
                   inp 
                   (Path.GetDirectoryName sourceFile)
                   istate)
      
    member fsiDynamicCompiler.EvalSourceFiles(istate, m, sourceFiles, lexResourceManager) =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        match sourceFiles with 
        | [] -> istate
        | _ -> 
          // use a set of source files as though they were command line inputs
          let sourceFiles = sourceFiles |> List.map (fun nm -> tcConfig.ResolveSourceFile(m,nm,tcConfig.implicitIncludeDir),m) 
         
          // Close the #load graph on each file and gather the inputs from the scripts.
          let closure = LoadClosure.ComputeClosureOfSourceFiles(TcConfig.Create(tcConfigB,validate=false),sourceFiles,CodeContext.Evaluation,lexResourceManager=lexResourceManager,useDefaultScriptingReferences=true)
          
          // Intent "[Loading %s]\n" (String.concat "\n     and " sourceFiles)
          fsiConsoleOutput.uprintf "[%s " (FSIstrings.SR.fsiLoadingFilesPrefixText())
          closure.Inputs  |> List.iteri (fun i (sourceFile,_) -> 
              if i=0 then fsiConsoleOutput.uprintf  "%s" sourceFile
              else fsiConsoleOutput.uprintnf " %s %s" (FSIstrings.SR.fsiLoadingFilesPrefixText()) sourceFile)
          fsiConsoleOutput.uprintfn "]"

          // Play errors and warnings from closures of the surface (root) script files.
          closure.RootErrors |> List.iter errorSink
          closure.RootWarnings |> List.iter warnSink
                
          // Non-scripts will not have been parsed during #load closure so parse them now
          let sourceFiles,inputs = 
              closure.Inputs  
              |> List.map (fun (filename, input)-> 
                    let parsedInput = 
                        match input with 
                        | None -> ParseOneInputFile(tcConfig,lexResourceManager,["INTERACTIVE"],filename,true,errorLogger,(*retryLocked*)false)
                        | _-> input
                    filename, parsedInput)
              |> List.unzip
          
          errorLogger.AbortOnError();
          if inputs |> List.exists isNone then failwith "parse error";
          let inputs = List.map Option.get inputs 
          let istate = List.fold2 fsiDynamicCompiler.ProcessMetaCommandsFromInputAsInteractiveCommands istate sourceFiles inputs
          fsiDynamicCompiler.EvalParsedSourceFiles (istate, inputs)

    
    member __.GetInitialInteractiveState () =
        let tcConfig = TcConfig.Create(tcConfigB,validate=false)
        let optEnv0 = InitialOptimizationEnv tcImports tcGlobals
        let emEnv = ILRuntimeWriter.emEnv0
        let tcEnv = GetInitialTypecheckerEnv None rangeStdin tcConfig tcImports tcGlobals
        let ccuName = assemblyName 

        let tcState = TypecheckInitialState (rangeStdin,ccuName,tcConfig,tcGlobals,tcImports,niceNameGen,tcEnv)

        let ilxGenerator = CreateIlxAssemblyGenerator(tcConfig,tcImports,tcGlobals, (LightweightTcValForUsingInBuildMethodCall tcGlobals), tcState.Ccu )
        {optEnv    = optEnv0;
         emEnv     = emEnv;
         tcGlobals = tcGlobals;
         tcState   = tcState;
         ilxGenerator = ilxGenerator;
         timing    = false;
        } 


type internal FsiIntellisenseProvider(tcGlobals, tcImports: TcImports) = 

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
        let nItems = Nameres.ResolvePartialLongIdent ncenv tcState.TcEnvFromImpls.NameEnv (ConstraintSolver.IsApplicableMethApprox tcGlobals amap rangeStdin) rangeStdin ad lid false
        let names  = nItems |> List.map (Nameres.DisplayNameOfItem tcGlobals) 
        let names  = names |> List.filter (fun (name:string) -> name.StartsWith(stem,StringComparison.Ordinal)) 
        names

#if FSI_SERVER_INTELLISENSE
    //----------------------------------------------------------------------------
    // FsiIntellisense (posible feature for v2) - GetDeclarations
    //----------------------------------------------------------------------------

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

    [<DllImport("kernel32.dll")>]
    extern bool SetConsoleCtrlHandler(ControlEventHandler _callback,bool _add)

// One strange case: when a TAE happens a strange thing 
// occurs the next read from stdin always returns
// 0 bytes, i.e. the channel will look as if it has been closed.  So we check
// for this condition explicitly.  We also recreate the lexbuf whenever CtrlC kicks.
type internal FsiInterruptStdinState = 
    | StdinEOFPermittedBecauseCtrlCRecentlyPressed 
    | StdinNormal

type internal FsiInterruptControllerState =  
    | InterruptCanRaiseException 
    | InterruptIgnored 

type internal FsiInterruptControllerKillerThreadRequest =  
    | ThreadAbortRequest 
    | NoRequest 
    | ExitRequest 
    | PrintInterruptRequest

type internal FsiInterruptController(fsiOptions : FsiCommandLineOptions, 
                                     fsiConsoleOutput: FsiConsoleOutput) = 

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

    // REVIEW: streamline all this code to use the same code on Windows and Posix.   
    member controller.InstallKillThread(threadToKill:Thread, pauseMilliseconds:int) = 
#if SILVERLIGHT
        let action() =
            Microsoft.FSharp.Silverlight.InterruptThread(threadToKill.ManagedThreadId)

        ctrlEventActions  <- action           :: ctrlEventActions;
#else
        if !progress then fprintfn fsiConsoleOutput.Out "installing CtrlC handler";
        // WINDOWS TECHNIQUE: .NET has more safe points, and you can do more when a safe point. 
        // Hence we actually start up the killer thread within the handler. 
        try 
            let raiseCtrlC() = 
                SetCurrentUICultureForThread fsiOptions.FsiLCID
                fprintf fsiConsoleOutput.Error "%s" (FSIstrings.SR.fsiInterrupt());
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
            if !progress then fprintfn fsiConsoleOutput.Error "Failed to install ctrl-c handler using Windows technique - trying to install one using Unix signal handling...";
            // UNIX TECHNIQUE: We start up a killer thread, and it watches the mutable reference location.    
            // We can't have a dependency on Mono DLLs (indeed we don't even have them!)
            // So SOFT BIND the following code:
            // Mono.Unix.Native.Stdlib.signal(Mono.Unix.Native.Signum.SIGINT,new Mono.Unix.Native.SignalHandler(fun n -> PosixSignalProcessor.PosixInvoke(n))) |> ignore;
            match (try Choice1Of2(Assembly.Load("Mono.Posix, Version=2.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756")) with e -> Choice2Of2 e) with 
            | Choice1Of2(monoPosix) -> 
              try
                if !progress then fprintfn fsiConsoleOutput.Error "loading type Mono.Unix.Native.Stdlib...";
                let monoUnixStdlib = monoPosix.GetType("Mono.Unix.Native.Stdlib") 
                if !progress then fprintfn fsiConsoleOutput.Error "loading type Mono.Unix.Native.SignalHandler...";
                let monoUnixSignalHandler = monoPosix.GetType("Mono.Unix.Native.SignalHandler") 
                if !progress then fprintfn fsiConsoleOutput.Error "creating delegate...";
                controller.PosixInvoke(-1);
                let monoHandler = System.Delegate.CreateDelegate(monoUnixSignalHandler,controller,"PosixInvoke") 
                if !progress then fprintfn fsiConsoleOutput.Error "registering signal handler...";
                let monoSignalNumber = System.Enum.Parse(monoPosix.GetType("Mono.Unix.Native.Signum"),"SIGINT")
                let register () = Utilities.callStaticMethod monoUnixStdlib "signal" [ monoSignalNumber; box monoHandler ]  |> ignore 
                posixReinstate <- register;
                register();
                let killerThread = 
                    new Thread(new ThreadStart(fun () ->
                        SetCurrentUICultureForThread fsiOptions.FsiLCID
                        while true do 
                            //fprintf fsiConsoleOutput.Error "\n- kill thread loop...\n"; errorWriter.Flush();  
                            Thread.Sleep(pauseMilliseconds*2);
                            match killThreadRequest with 
                            | PrintInterruptRequest -> 
                                fprintf fsiConsoleOutput.Error "%s" (FSIstrings.SR.fsiInterrupt()); fsiConsoleOutput.Error.Flush();  
                                killThreadRequest <- NoRequest;
                            | ThreadAbortRequest -> 
                                fprintf fsiConsoleOutput.Error  "%s" (FSIstrings.SR.fsiInterrupt()); fsiConsoleOutput.Error.Flush();  
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
                                fprintf fsiConsoleOutput.Error  "%s" (FSIstrings.SR.fsiExit()); fsiConsoleOutput.Error.Flush();  
                                Utilities.callStaticMethod monoUnixStdlib "exit" [ box 0 ] |> ignore
                            | _ ->  ()
                        done),Name="ControlCAbortAlternativeThread") 
                killerThread.IsBackground <- true;
                killerThread.Start();
                true // exit via kill thread to workaround block-on-exit bugs with Mono once a CtrlC has been pressed
              with e -> 
                fprintf fsiConsoleOutput.Error  "%s" (FSIstrings.SR.fsiCouldNotInstallCtrlCHandler(e.Message))
                false
            | Choice2Of2 e ->
              fprintf fsiConsoleOutput.Error  "%s" (FSIstrings.SR.fsiCouldNotInstallCtrlCHandler(e.Message))
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

// From http://msdn.microsoft.com/en-us/library/ff527268.aspx
// What the Event Handler Does
//
// The handler for the AssemblyResolve event receives the display name of the assembly to 
// be loaded, in the ResolveEventArgs.Name property. If the handler does not recognize the 
// assembly name, it returns null (Nothing in Visual Basic, nullptr in Visual C++). 
//
// - If the handler recognizes the assembly name, it can load and return an assembly that 
//   satisfies the request. The following list describes some sample scenarios. 
//
// - If the handler knows the location of a version of the assembly, it can load the assembly by 
//   using the Assembly.LoadFrom or Assembly.LoadFile method, and can return the loaded assembly if successful. 
//
// - If the handler has access to a database of assemblies stored as byte arrays, it can load a byte array by 
//   using one of the Assembly.Load method overloads that take a byte array. 
//
// - The handler can generate a dynamic assembly and return it.
// 
// It is the responsibility of the event handler to return a suitable assembly. The handler can parse the display 
// name of the requested assembly by passing the ResolveEventArgs.Name property value to the AssemblyName(String) 
// constructor. Beginning with the .NET Framework version 4, the handler can use the ResolveEventArgs.RequestingAssembly 
// property to determine whether the current request is a dependency of another assembly. This information can help 
// identify an assembly that will satisfy the dependency.
// 
// The event handler can return a different version of the assembly than the version that was requested. 
// 
// In most cases, the assembly that is returned by the handler appears in the load context, regardless of the context 
// the handler loads it into. For example, if the handler uses the Assembly.LoadFrom method to load an assembly into 
// the load-from context, the assembly appears in the load context when the handler returns it. However, in the following 
// case the assembly appears without context when the handler returns it:
// 
// - The handler loads an assembly without context.
// - The ResolveEventArgs.RequestingAssembly property is not null.
// - The requesting assembly (that is, the assembly that is returned by the ResolveEventArgs.RequestingAssembly property) 
//   was loaded without context. 
// 
// For information about contexts, see the Assembly.LoadFrom(String) method overload.

module internal MagicAssemblyResolution =
    // FxCop identifies Assembly.LoadFrom.
    [<CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2001:AvoidCallingProblematicMethods", MessageId="System.Reflection.Assembly.UnsafeLoadFrom")>]
    let private assemblyLoadFrom (path:string) = 

#if SILVERLIGHT
        FileSystem.AssemblyLoadFrom(path)
#else        
    // See bug 5501 for details on decision to use UnsafeLoadFrom here.
    // Summary:
    //  It is an explicit user trust decision to load an assembly with #r. Scripts are not run automatically (for example, by double-clicking in explorer).
    //  We considered setting loadFromRemoteSources in fsi.exe.config but this would transitively confer unsafe loading to the code in the referenced 
    //  assemblies. Better to let those assemblies decide for themselves which is safer.
#if FX_ATLEAST_40
        Assembly.UnsafeLoadFrom(path)
#else
        Assembly.LoadFrom(path)
#endif
#endif // SILVERLIGHT
    let ResolveAssembly(m,tcConfigB, tcImports: TcImports, fsiDynamicCompiler: FsiDynamicCompiler, fsiConsoleOutput: FsiConsoleOutput, fullAssemName:string) = 
           try 
               // Grab the name of the assembly
               let tcConfig = TcConfig.Create(tcConfigB,validate=false)
               let simpleAssemName = fullAssemName.Split([| ',' |]).[0]          
               if !progress then fsiConsoleOutput.uprintfn "ATTEMPT MAGIC LOAD ON ASSEMBLY, simpleAssemName = %s" simpleAssemName; // "Attempting to load a dynamically required assembly in response to an AssemblyResolve event by using known static assembly references..." 
               
               // Special case: Mono Windows Forms attempts to load an assembly called something like "Windows.Forms.resources"
               // We can't resolve this, so don't try.
               // REVIEW: Suggest 4481, delete this special case.
               if simpleAssemName.EndsWith(".resources",StringComparison.OrdinalIgnoreCase) || 
                    // See F# 1.0 Product Studio bug 1171
                    simpleAssemName.EndsWith(".XmlSerializers",StringComparison.OrdinalIgnoreCase) || 
                    (runningOnMono && simpleAssemName = "UIAutomationWinforms") then null else

               // Special case: Is this the global unique dynamic assembly for FSI code? In this case just
               // return the dynamic assembly itself.       
               if fsiDynamicCompiler.DynamicAssemblyName = simpleAssemName then fsiDynamicCompiler.DynamicAssembly else

               // Otherwise continue
               let assemblyReferenceTextDll = (simpleAssemName + ".dll") 
               let assemblyReferenceTextExe = (simpleAssemName + ".exe") 
               let overallSearchResult =           
                   // OK, try to resolve as a .dll
                   let searchResult = tcImports.TryResolveAssemblyReference (AssemblyReference(m,assemblyReferenceTextDll),ResolveAssemblyReferenceMode.Speculative)

                   match searchResult with
                   | OkResult (warns,[r]) -> OkResult (warns, Choice1Of2 r.resolvedPath)
                   | _ -> 

                   // OK, try to resolve as a .exe
                   let searchResult = tcImports.TryResolveAssemblyReference (AssemblyReference(m,assemblyReferenceTextExe),ResolveAssemblyReferenceMode.Speculative)

                   match searchResult with
                   | OkResult (warns, [r]) -> OkResult (warns, Choice1Of2 r.resolvedPath)
                   | _ -> 

                   if !progress then fsiConsoleOutput.uprintfn "ATTEMPT LOAD, assemblyReferenceTextDll = %s" assemblyReferenceTextDll;
                   /// Take a look through the files quoted, perhaps with explicit paths
                   let searchResult = 
                       (tcConfig.referencedDLLs 
                            |> List.tryPick (fun assemblyReference -> 
                             if !progress then fsiConsoleOutput.uprintfn "ATTEMPT MAGIC LOAD ON FILE, referencedDLL = %s" assemblyReference.Text;
                             if System.String.Compare(Filename.fileNameOfPath assemblyReference.Text, assemblyReferenceTextDll,StringComparison.OrdinalIgnoreCase) = 0 ||
                                System.String.Compare(Filename.fileNameOfPath assemblyReference.Text, assemblyReferenceTextExe,StringComparison.OrdinalIgnoreCase) = 0 then
                                 Some(tcImports.TryResolveAssemblyReference(assemblyReference,ResolveAssemblyReferenceMode.Speculative))
                             else None ))

                   match searchResult with
                   | Some (OkResult (warns,[r])) -> OkResult (warns, Choice1Of2 r.resolvedPath)
                   | _ -> 

#if EXTENSIONTYPING
                   match tcImports.TryFindProviderGeneratedAssemblyByName(simpleAssemName) with
                   | Some(assembly) -> OkResult([],Choice2Of2 assembly)
                   | None -> 
#endif
                   
                   // Try to find the reference without an extension
                   match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef(ILAssemblyRef.Create(simpleAssemName,None,None,false,None,None)) with
                   | Some(resolvedPath) -> 
                       OkResult([],Choice1Of2 resolvedPath)
                   | None -> 
                   
                   ErrorResult([],Failure (FSIstrings.SR.fsiFailedToResolveAssembly(simpleAssemName)))
                           
               match overallSearchResult with 
               | ErrorResult _ -> null
               | OkResult _ -> 
                   let res = CommitOperationResult overallSearchResult
                   match res with 
                   | Choice1Of2 assemblyName -> 
                       if simpleAssemName <> "Mono.Posix" then fsiConsoleOutput.uprintfn "%s" (FSIstrings.SR.fsiBindingSessionTo(assemblyName));
                       assemblyLoadFrom assemblyName
                   | Choice2Of2 assembly -> 
                       assembly
                   
           with e -> 
               stopProcessingRecovery e range0; 
               null

#if SILVERLIGHT
    let Install(_tcConfigB, _tcImports: TcImports, _fsiDynamicCompiler: FsiDynamicCompiler, _fsiConsoleOutput: FsiConsoleOutput) = 
        ()
        // // Look through the already loaded assemblies by hand. For some reason Assembly.Load
        // // doesn't find dynamically generated assemblies in Silverlight
        // System.AppDomain.CurrentDomain.add_AssemblyResolve(new System.ResolveEventHandler(fun _ args -> 
        //     System.AppDomain.CurrentDomain.GetAssemblies() 
        //           |> Array.tryFind (fun x -> printfn "args.Name = '%s', x.FullName = '%d'"; args.Name = x.FullName)
        //           |> function None -> null | Some a -> a))
#else
    let Install(tcConfigB, tcImports: TcImports, fsiDynamicCompiler: FsiDynamicCompiler, fsiConsoleOutput: FsiConsoleOutput) = 

        let rangeStdin = rangeN Lexhelp.stdinMockFilename 0

        AppDomain.CurrentDomain.add_AssemblyResolve(new ResolveEventHandler(fun _ args -> 
            ResolveAssembly (rangeStdin, tcConfigB, tcImports, fsiDynamicCompiler, fsiConsoleOutput, args.Name)))


#endif // SILVERLIGHT

//----------------------------------------------------------------------------
// Reading stdin 
//----------------------------------------------------------------------------

type internal FsiStdinLexerProvider(tcConfigB, fsiStdinSyphon, 
                                    fsiConsoleInput : FsiConsoleInput, 
                                    fsiConsoleOutput : FsiConsoleOutput, 
                                    fsiOptions : FsiCommandLineOptions,
                                    lexResourceManager : LexResourceManager,
                                    errorLogger) = 

    // #light is the default for FSI
    let interactiveInputLightSyntaxStatus = 
        let initialLightSyntaxStatus = tcConfigB.light <> Some false
        LightSyntaxStatus (initialLightSyntaxStatus, false (* no warnings *))

    let LexbufFromLineReader (fsiStdinSyphon: FsiStdinSyphon) readf = 
        UnicodeLexing.FunctionAsLexbuf 
          (fun (buf: char[], start, len) -> 
            if !progress then printfn "calling readf..."
            //fprintf fsiConsoleOutput.Out "Calling ReadLine\n";
            let inputOption = try Some(readf()) with :? EndOfStreamException -> None
            inputOption |> Option.iter (fun t -> fsiStdinSyphon.Add (t + "\n"));
            match inputOption with 
            |  Some(null) | None -> 
                 if !progress then fprintfn fsiConsoleOutput.Out "End of file from TextReader.ReadLine";
                 0
            | Some (input:string) ->
                let input  = input + "\n" 
                let ninput = input.Length 
                if ninput > len then fprintf fsiConsoleOutput.Error  "%s" (FSIstrings.SR.fsiLineTooLong());
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

    let CreateLexerForLexBuffer (sourceFileName, lexbuf) =

        Lexhelp.resetLexbufPos sourceFileName lexbuf;
        let skip = true  // don't report whitespace from lexer 
        let defines = "INTERACTIVE"::tcConfigB.conditionalCompilationDefines
        let lexargs = mkLexargs (sourceFileName,defines, interactiveInputLightSyntaxStatus, lexResourceManager, ref [], errorLogger) 
        let tokenizer = Lexfilter.LexFilter(interactiveInputLightSyntaxStatus, tcConfigB.compilingFslib, Lexer.token lexargs skip, lexbuf)
        tokenizer


    // Create a new lexer to read stdin 
    member __.CreateStdinLexer () =
        let lexbuf = 
#if SILVERLIGHT        
            LexbufFromLineReader fsiStdinSyphon (fun () -> fsiConsoleInput.In.ReadLine() |> removeZeroCharsFromString)
#else        
            match fsiConsoleInput.TryGetConsole() with 
            | Some console when fsiOptions.EnableConsoleKeyProcessing && not fsiOptions.IsInteractiveServer -> 
                LexbufFromLineReader fsiStdinSyphon (fun () -> 
                    match fsiConsoleInput.TryGetFirstLine() with 
                    | Some firstLine -> firstLine
                    | None -> 
                          if !progress then printfn "have console... calling ReadLine..."
                          console.ReadLine())
            | _ -> 
              
               LexbufFromLineReader fsiStdinSyphon (fun () -> 
                   if !progress then printfn "no console... calling ReadLine..."
                   fsiConsoleInput.In.ReadLine() |> removeZeroCharsFromString)
#endif                

        fsiStdinSyphon.Reset();
        CreateLexerForLexBuffer (Lexhelp.stdinMockFilename, lexbuf)

    // Create a new lexer to read an "included" script file
    member __.CreateIncludedScriptLexer sourceFileName =
        let lexbuf = UnicodeLexing.UnicodeFileAsLexbuf(sourceFileName,tcConfigB.inputCodePage,(*retryLocked*)false)  
        CreateLexerForLexBuffer (sourceFileName, lexbuf)

//----------------------------------------------------------------------------
// Process one parsed interaction.  This runs on the GUI thread.
// It might be simpler if it ran on the parser thread.
//----------------------------------------------------------------------------

type internal FsiInteractionStepStatus = 
    | CtrlC 
    | EndOfFile 
    | Completed 
    | CompletedWithReportedError


type internal FsiInteractionProcessor
                            (tcConfigB, 
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
            // reset error count 
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


    /// Parse one interaction. Called on the parser thread.
    let ParseInteraction (tokenizer:Lexfilter.LexFilter) =   
        let lastToken = ref Parser.ELSE // Any token besides SEMICOLON_SEMICOLON will do for initial value 
        try 
            if !progress then fprintfn fsiConsoleOutput.Out "In ParseInteraction...";

            let input = 
                Lexhelp.reusingLexbufForParsing tokenizer.LexBuffer (fun () -> 
                    let lexerWhichSavesLastToken lexbuf = 
                        let tok = tokenizer.Lexer lexbuf
                        lastToken := tok
                        tok                        
                    Parser.interaction lexerWhichSavesLastToken tokenizer.LexBuffer)
            Some input
        with e ->
            if !progress then fprintfn fsiConsoleOutput.Out "Error in ParseInteraction: %s" (e.ToString())
            // On error, consume tokens until to ;; or EOF.
            // Caveat: Unless the error parse ended on ;; - so check the lastToken returned by the lexer function.
            // Caveat: What if this was a look-ahead? That's fine! Since we need to skip to the ;; anyway.     
            if (match !lastToken with Parser.SEMICOLON_SEMICOLON -> false | _ -> true) then
                let mutable tok = Parser.ELSE (* <-- any token <> SEMICOLON_SEMICOLON will do *)
                while (match tok with  Parser.SEMICOLON_SEMICOLON -> false | _ -> true) 
                      && not tokenizer.LexBuffer.IsPastEndOfStream do
                    tok <- tokenizer.Lexer tokenizer.LexBuffer

            stopProcessingRecovery e range0;    
            None

    /// Execute a single parsed interaction. Called on the GUI/execute/main thread.
    let ExecInteraction (exitViaKillThread:bool, tcConfig:TcConfig, istate, action:ParsedFsiInteraction) =
        istate |> InteractiveCatch (fun istate -> 
            match action with 
            | IDefns ([  ],_) ->
                istate,Completed
            | IDefns ([  SynModuleDecl.DoExpr(_,expr,_)],_) ->
                fsiDynamicCompiler.EvalParsedExpression  (istate, expr), Completed           
            | IDefns (defs,_) -> 
                fsiDynamicCompiler.EvalParsedDefinitions (istate, true, false, defs), Completed

            | IHash (ParsedHashDirective("load",sourceFiles,m),_) -> 
                fsiDynamicCompiler.EvalSourceFiles (istate, m, sourceFiles, lexResourceManager),Completed

            | IHash (ParsedHashDirective(("reference" | "r"),[path],m),_) -> 
                let resolutions,istate = fsiDynamicCompiler.EvalRequireReference istate m path 
                resolutions |> List.iter (fun ar -> fsiConsoleOutput.uprintnfnn "%s" (FSIstrings.SR.fsiDidAHashr(ar.resolvedPath)))
                istate,Completed

            | IHash (ParsedHashDirective("I",[path],m),_) -> 
                tcConfigB.AddIncludePath (m,path, tcConfig.implicitIncludeDir)
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
                fsiConsoleOutput.uprintfn "%s" (FSIstrings.SR.fsiInvalidDirective(c, String.concat " " arg));  // REVIEW: uprintnfnn - like other directives above
                istate,Completed  (* REVIEW: cont = CompletedWithReportedError *)
        )

    /// Execute a single parsed interaction which may contain multiple items to be executed
    /// independently, because some are #directives. Called on the GUI/execute/main thread.
    /// 
    /// #directive comes through with other definitions as a SynModuleDecl.HashDirective.
    /// We split these out for individual processing.
    let rec ExecInteractions (exitViaKillThread, tcConfig, istate, action:ParsedFsiInteraction option) =
        let action,nextAction = 
            match action with
            | None                                      -> None  ,None
            | Some (IHash _)                            -> action,None
            | Some (IDefns ([],_))                      -> None  ,None
            | Some (IDefns (SynModuleDecl.HashDirective(hash,mh)::defs,m)) -> 
                Some (IHash(hash,mh)),Some (IDefns(defs,m))

            | Some (IDefns (defs,m))                    -> 
                let isDefHash = function SynModuleDecl.HashDirective(_,_) -> true | _ -> false
                let defsA = Seq.takeWhile (isDefHash >> not) defs |> Seq.toList
                let defsB = Seq.skipWhile (isDefHash >> not) defs |> Seq.toList

                // When the last declaration has a shape of DoExp (i.e., non-binding), 
                // transform it to a shape of "let it = <exp>", so we can refer it.
                let defsA = if defsA.Length <= 1 || defsB.Length > 0 then  defsA else
                            match List.headAndTail (List.rev defsA) with
                            | SynModuleDecl.DoExpr(_,exp,_), rest -> (rest |> List.rev) @ (fsiDynamicCompiler.BuildItBinding exp)
                            | _ -> defsA

                Some (IDefns(defsA,m)),Some (IDefns(defsB,m))

        match action with
          | None -> assert(nextAction.IsNone); istate,Completed
          | Some action ->
              let istate,cont = ExecInteraction (exitViaKillThread, tcConfig, istate, action)
              match cont with
                | Completed                  -> ExecInteractions (exitViaKillThread, tcConfig, istate, nextAction)
                | CompletedWithReportedError -> istate,CompletedWithReportedError  (* drop nextAction on error *)
                | EndOfFile                  -> istate,Completed                   (* drop nextAction on EOF *)
                | CtrlC                      -> istate,CtrlC                       (* drop nextAction on CtrlC *)

    /// Execute a single parsed interaction on the parser/execute thread.
    let MainThreadProcessParsedInteraction (exitViaKillThread, action, istate) =         
        try 
            let tcConfig = TcConfig.Create(tcConfigB,validate=false)
#if SILVERLIGHT
            Microsoft.FSharp.Silverlight.ResumeThread(Threading.Thread.CurrentThread.ManagedThreadId)
            ExecInteractions (exitViaKillThread, tcConfig, istate, action)
        with
        | :? ThreadAbortException ->
           (istate,CtrlC)
        |  e ->
           stopProcessingRecovery e range0;
           istate,CompletedWithReportedError
#else                                   
            if !progress then fprintfn fsiConsoleOutput.Out "In MainThreadProcessParsedInteraction...";                  
            fsiInterruptController.InterruptAllowed <- InterruptCanRaiseException;
            let res = ExecInteractions (exitViaKillThread, tcConfig, istate, action)
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


    /// Parse then process one parsed interaction.  
    ///
    /// During normal execution, this initially runs on the parser
    /// thread, then calls runCodeOnMainThread when it has completed 
    /// parsing and needs to typecheck and execute a definition. This blocks the parser thread
    /// until execution has competed on the GUI thread.
    ///
    /// During processing of startup scripts, this runs on the main thread.

    member __.ParseAndProcessAndEvalOneInteractionFromLexbuf (exitViaKillThread, runCodeOnMainThread, istate:FsiDynamicCompilerState, tokenizer:Lexfilter.LexFilter) =

        if tokenizer.LexBuffer.IsPastEndOfStream then 
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
                if !progress then fprintfn fsiConsoleOutput.Out "entering ParseInteraction...";

                // Parse the interaction. When FSI.EXE is waiting for input from the console the 
                // parser thread is blocked somewhere deep this call. 
                let action  = ParseInteraction tokenizer

                if !progress then fprintfn fsiConsoleOutput.Out "returned from ParseInteraction...calling runCodeOnMainThread...";

                // After we've unblocked and got something to run we switch 
                // over to the run-thread (e.g. the GUI thread) 
                let res = istate  |> runCodeOnMainThread (fun istate -> MainThreadProcessParsedInteraction (exitViaKillThread, action, istate)) 

                if !progress then fprintfn fsiConsoleOutput.Out "Just called runCodeOnMainThread, res = %O..." res;
                res)
        
    /// Perform an "include" on a script file (i.e. a script file specified on the command line)
    member processor.EvalIncludedScript (exitViaKillThread, istate, sourceFile, m) =
        let tcConfig = TcConfig.Create(tcConfigB, validate=false)
        // Resolve the filename to an absolute filename
        let sourceFile = tcConfig.ResolveSourceFile(m,sourceFile,tcConfig.implicitIncludeDir) 
        // During the processing of the file, further filenames are 
        // resolved relative to the home directory of the loaded file.
        WithImplicitHome (tcConfigB, directoryName sourceFile)  (fun () ->
              // An included script file may contain maybe several interaction blocks.
              // We repeatedly parse and process these, until an error occurs.
                let tokenizer = fsiStdinLexerProvider.CreateIncludedScriptLexer sourceFile
                let rec run istate =
                    let istate,cont = processor.ParseAndProcessAndEvalOneInteractionFromLexbuf (exitViaKillThread, (fun f istate -> f istate), istate, tokenizer)
                    if cont = Completed then run istate else istate,cont 

                let istate,cont = run istate 

                match cont with
                | Completed -> failwith "EvalIncludedScript: Completed expected to have relooped"
                | CompletedWithReportedError -> istate,CompletedWithReportedError
                | EndOfFile -> istate,Completed // here file-EOF is normal, continue required 
                | CtrlC     -> istate,CtrlC
          )


    /// Load the source files, one by one. Called on the main thread.
    member processor.EvalIncludedScripts (istate, exitViaKillThread, sourceFiles) =
      match sourceFiles with
        | [] -> istate
        | sourceFile :: moreSourceFiles ->
            // Catch errors on a per-file basis, so results/bindings from pre-error files can be kept.
            let istate,cont = InteractiveCatch (fun istate -> processor.EvalIncludedScript (exitViaKillThread, istate, sourceFile, rangeStdin)) istate
            match cont with
              | Completed                  -> processor.EvalIncludedScripts (istate, exitViaKillThread, moreSourceFiles)
              | CompletedWithReportedError -> istate // do not process any more files              
              | CtrlC                      -> istate // do not process any more files 
              | EndOfFile                  -> assert false; istate // This is unexpected. EndOfFile is replaced by Completed in the called function 


    member processor.LoadInitialFiles (exitViaKillThread, istate) =
        /// Consume initial source files in chunks of scripts or non-scripts
        let rec consume istate sourceFiles =
            match sourceFiles with
            | [] -> istate
            | (_,isScript1) :: _ -> 
                let sourceFiles,rest = List.takeUntil (fun (_,isScript2) -> isScript1 <> isScript2) sourceFiles 
                let sourceFiles = List.map fst sourceFiles 
                let istate = 
                    if isScript1 then 
                        processor.EvalIncludedScripts (istate, exitViaKillThread, sourceFiles)
                    else 
                        istate |> InteractiveCatch (fun istate -> fsiDynamicCompiler.EvalSourceFiles(istate, rangeStdin, sourceFiles, lexResourceManager), Completed) |> fst 
                consume istate rest 

        let istate = consume istate fsiOptions.SourceFiles

        if nonNil fsiOptions.SourceFiles then 
            fsiConsolePrompt.PrintAhead(); // Seems required. I expected this could be deleted. Why not?
        istate 

    /// Send a dummy interaction through F# Interactive, to ensure all the most common code generation paths are 
    /// JIT'ed and ready for use.
    member processor.LoadDummyInteraction istate =
        istate |> InteractiveCatch (fun istate ->  fsiDynamicCompiler.EvalParsedDefinitions (istate, true, false, []), Completed) |> fst
        
    member processor.FsiOptions = fsiOptions

//----------------------------------------------------------------------------
// GUI runCodeOnMainThread
//----------------------------------------------------------------------------

//type InteractionStateConverter = delegate of FsiDynamicCompilerState -> FsiDynamicCompilerState * stepStatus

#if SILVERLIGHT
#else            
///Use a dummy to access protected member
type internal DummyForm() = 
    inherit Form() 
    member x.DoCreateHandle() = x.CreateHandle() 
    /// Creating the dummy form object can crash on Mono Mac, and then prints a nasty background
    /// error during finalization of the half-initialized object...
    override x.Finalize() = ()
    

/// This is the event loop implementation for winforms
type internal WinFormsEventLoop(fsiConsoleOutput: FsiConsoleOutput, lcid : int option) = 
    let mainForm = new DummyForm() 
    do mainForm.DoCreateHandle();
    // Set the default thread exception handler
    let restart = ref false
    interface Microsoft.FSharp.Compiler.Interactive.IEventLoop with
         member x.Run() =  
             restart := false;
             if !progress then fprintfn fsiConsoleOutput.Out "MAIN: Calling Application.Run...";
             Application.Run()
             if !progress then fprintfn fsiConsoleOutput.Out "MAIN: Returned from Application.Run...";
             !restart
         member x.Invoke (f: unit -> 'T) : 'T =   
            if !progress then fprintfn fsiConsoleOutput.Out "RunCodeOnWinFormsMainThread: entry...";                  
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

                if !progress then fprintfn fsiConsoleOutput.Out "RunCodeOnWinFormsMainThread: invoking...";                  

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

                if !progress then fprintfn fsiConsoleOutput.Out "RunCodeOnWinFormsMainThread: Waiting for completion signal....";
                while not (doneSignal.WaitOne(new TimeSpan(0,0,1),true)) do 
                    if !progress then fprintf fsiConsoleOutput.Out "."; fsiConsoleOutput.Out.Flush()

                if !progress then fprintfn fsiConsoleOutput.Out "RunCodeOnWinFormsMainThread: Got completion signal, res = %b" (Option.isSome !mainFormInvokeResultHolder);
                !mainFormInvokeResultHolder |> Option.get

         member x.ScheduleRestart()  =   restart := true; Application.Exit() 
     
let internal TrySetUnhandledExceptionMode() =  
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
let internal SpawnThread name f =
    let th = new Thread(new ThreadStart(f),Name=name)
    th.IsBackground <- true;
    th.Start()

let internal SpawnInteractiveServer 
                           (fsiOptions : FsiCommandLineOptions, 
                            fsiConsoleOutput:  FsiConsoleOutput,
                            fsiInterruptController : FsiInterruptController) =   
    //printf "Spawning fsi server on channel '%s'" !fsiServerName;
    SpawnThread "ServerThread" (fun () ->
         SetCurrentUICultureForThread fsiOptions.FsiLCID
         try
             let server =
                 {new Server.Shared.FSharpInteractiveServer() with
                    member this.Interrupt() = 
                        //printf "FSI-SERVER: received CTRL-C request...\n";
                        try 
                            fsiInterruptController.Interrupt()
                        with e -> 
                            // Final sanity check! - catch all exns - but not expected 
                            assert false
                            ()    

#if FSI_SERVER_INTELLISENSE
                    member this.Completions(prefix) = 
                        try 
                            fsiIntellisenseProvider.CompletionsForPartialLID !istateRef prefix  |> List.toArray
                        with e -> 
                            // Final sanity check! - catch all exns - but not expected
                            assert false
                            [| |] 

                    member this.GetDeclarations(text,names) = 
                        try 
                            // Stop the type checker running at the same time as the intellisense provider.
                            lock tcLockObject (fun () -> fsiIntellisenseProvider.FsiGetDeclarations !istateRef text names)
                        with e -> 
                            // Final sanity check! - catch all exns - but not expected 
                            assert false
                            [| |] 
#endif
                 }

             Server.Shared.FSharpInteractiveServer.StartServer(fsiOptions.FsiServerName,server)
         with e ->
             fprintfn fsiConsoleOutput.Error "%s" (FSIstrings.SR.fsiExceptionRaisedStartingServer(e.ToString())))
  
#endif // SILVERLIGHT

let internal StartStdinReadAndProcessThread
                                  (lcid, istateRef, errorLogger, 
                                   fsiConsoleInput: FsiConsoleInput, 
                                   fsiConsoleOutput: FsiConsoleOutput,
                                   fsiStdinLexerProvider: FsiStdinLexerProvider, 
                                   fsiInteractionProcessor : FsiInteractionProcessor,
                                   exitViaKillThread) = 
    if !progress then fprintfn fsiConsoleOutput.Out "creating stdinReaderThread";
    let cont = ref Completed 
    let tokenizerRef = ref (fsiStdinLexerProvider.CreateStdinLexer())
    let culture = Thread.CurrentThread.CurrentUICulture

    let stdinReaderThread = 
        new Thread(new ThreadStart(fun () ->
            InstallErrorLoggingOnThisThread errorLogger // FSI error logging on stdinReaderThread, e.g. parse errors.
            SetCurrentUICultureForThread lcid
            try
               try 
                  if !progress then fprintfn fsiConsoleOutput.Out "READER: stdin thread started...";

                  // Delay until we've peeked the input or read the entire first line
                  fsiConsoleInput.WaitForInitialConsoleInput()
                  
                  if !progress then fprintfn fsiConsoleOutput.Out "READER: stdin thread got first line...";

                  // The main stdin loop, running on the stdinReaderThread.
                  // 
                  // The function 'ParseAndProcessAndEvalOneInteractionFromLexbuf' is blocking: it reads stdin 
                  // until one or more real chunks of input have been received. 
                  //
                  // We run the actual computations for each action on the main GUI thread by using
                  // mainForm.Invoke to pipe a message back through the form's main event loop. (The message 
                  // is a delegate to execute on the main Thread)
                  //
                  while (!cont = CompletedWithReportedError || !cont = Completed || !cont = CtrlC) do
                      if (!cont = CtrlC) then 
                          tokenizerRef := fsiStdinLexerProvider.CreateStdinLexer();

                      let runCodeOnMainThread f istate = 
                          try 
                              fsi.EventLoop.Invoke (fun () -> 
                                  InstallErrorLoggingOnThisThread errorLogger; 
                                  SetCurrentUICultureForThread lcid;
                                  f istate) // FSI error logging on switched to thread
                          with _ -> 
                              (istate,Completed)
                              
                      let istateNew,contNew = 
                          fsiInteractionProcessor.ParseAndProcessAndEvalOneInteractionFromLexbuf (exitViaKillThread, runCodeOnMainThread, !istateRef, !tokenizerRef)   

                      istateRef := istateNew; 
                      cont := contNew;
                      if !progress then fprintfn fsiConsoleOutput.Out "READER: cont = %O" !cont;

                  if !progress then fprintfn fsiConsoleOutput.Out "- READER: Exiting stdinReaderThread";  

                with e -> stopProcessingRecovery e range0;

            finally 
                // Reset the Culture code
                Thread.CurrentThread.CurrentCulture <- culture
                if !progress then fprintfn fsiConsoleOutput.Out "- READER: Exiting process because of failure/exit on  stdinReaderThread";  
                // REVIEW: On some flavors of Mono, calling exit may freeze the process if we're using the WinForms event handler
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
    if !progress then fprintfn fsiConsoleOutput.Out "MAIN: starting stdin thread...";
    stdinReaderThread.Start();


let internal DriveFsiEventLoop (fsiConsoleOutput: FsiConsoleOutput) = 
    let rec runLoop() = 
        if !progress then fprintfn fsiConsoleOutput.Out "GUI thread runLoop";
        let restart = 
            try 
              // BLOCKING POINT: The GUI Thread spends most (all) of its time this event loop
              if !progress then fprintfn fsiConsoleOutput.Out "MAIN:  entering event loop...";
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
        if !progress then fprintfn fsiConsoleOutput.Out "MAIN:  exited event loop...";
        if restart then runLoop() 

    runLoop();


/// The primary type, representing a full F# Interactive session, reading from the given
/// text input, writing to the given text output and error writers.
type FsiEvaluationSession (argv:string[], inReader:TextReader, outWriter:TextWriter, errorWriter: TextWriter) = 
#if SILVERLIGHT
    do
        Microsoft.FSharp.Core.Printf.setWriter outWriter
        Microsoft.FSharp.Core.Printf.setError errorWriter
#endif
    do if not runningOnMono then Lib.UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption() (* SDL recommendation *)
    // See Bug 735819 
    let lcidFromCodePage = 
#if SILVERLIGHT
#else         
        if (Console.OutputEncoding.CodePage <> 65001) &&
           (Console.OutputEncoding.CodePage <> Thread.CurrentThread.CurrentUICulture.TextInfo.OEMCodePage) &&
           (Console.OutputEncoding.CodePage <> Thread.CurrentThread.CurrentUICulture.TextInfo.ANSICodePage) then
                Thread.CurrentThread.CurrentUICulture <- new CultureInfo("en-US")
                Some 1033
        else
#endif        
            None

    let timeReporter = FsiTimeReporter(outWriter)

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

#if SILVERLIGHT
    let defaultFSharpBinariesDir = "."
    let currentDirectory = "."
#else    
    let defaultFSharpBinariesDir = System.AppDomain.CurrentDomain.BaseDirectory
    let currentDirectory = Directory.GetCurrentDirectory()
#endif

    let tcConfigB = Build.TcConfigBuilder.CreateNew(defaultFSharpBinariesDir, 
                                                    true, // long running: optimizeForMemory 
                                                    currentDirectory,isInteractive=true, 
                                                    isInvalidationSupported=false)
    let tcConfigP = TcConfigProvider.BasedOnMutableBuilder(tcConfigB)
    do tcConfigB.resolutionEnvironment <- MSBuildResolver.RuntimeLike // See Bug 3608
    do tcConfigB.useFsiAuxLib <- true

    // Preset: --optimize+ -g --tailcalls+ (see 4505)
    do SetOptimizeSwitch tcConfigB On
    do SetDebugSwitch    tcConfigB (Some "pdbonly") On
    do SetTailcallSwitch tcConfigB On    

#if SILVERLIGHT
#else
#if FX_ATLEAST_40
    // set platform depending on whether the current process is a 64-bit process.
    // BUG 429882 : FsiAnyCPU.exe issues warnings (x64 v MSIL) when referencing 64-bit assemblies
    do tcConfigB.platform <- if System.Environment.Is64BitProcess then Some AMD64 else Some X86
#endif
#endif

    let fsiStdinSyphon = new FsiStdinSyphon(errorWriter)
    let fsiConsoleOutput = FsiConsoleOutput(tcConfigB, outWriter, errorWriter)
    
    let errorLogger = ErrorLoggerThatStopsOnFirstError(tcConfigB, fsiStdinSyphon, fsiConsoleOutput)

    do InstallErrorLoggingOnThisThread errorLogger // FSI error logging on main thread.

    let updateBannerText() =
      tcConfigB.productNameForBannerText <- FSIstrings.SR.fsiProductName(FSharpEnvironment.DotNetBuildString)
  
    do updateBannerText() // setting the correct banner so that 'fsi -?' display the right thing

    let fsiOptions       = FsiCommandLineOptions(argv, tcConfigB, fsiConsoleOutput)
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
        System.Threading.Thread.CurrentThread.CurrentUICulture <- new CultureInfo(n.ToString())
#else    
        System.Threading.Thread.CurrentThread.CurrentUICulture <- new CultureInfo(n)
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


    let fsiConsoleInput = FsiConsoleInput(fsiOptions, inReader, outWriter)

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

    /// The lock stops the type checker running at the same time as the server intellisense implementation.
    let tcLockObject = box 7 // any new object will do
    // NOTE: this should probably be memoized in the .NET version

#if SILVERLIGHT
    // Silverlight has no magic assembly resolution. So we load assemblies here if needed, and memoize the results.
    let resolveType fsiDynamicCompiler = 
        Tables.memoize (fun (aref: ILAssemblyRef) -> 
            match tcImports.TryFindProviderGeneratedAssemblyByName aref.Name with
            | Some assembly -> Some (Choice2Of2 assembly)
            | None -> 
            //match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef aref with
            //| Some resolvedPath -> Some (Choice1Of2 resolvedPath)
            //| None -> 
            let loadAttempt = 
                try 
                    match Assembly.Load(aref.QualifiedName) with 
                    | null -> None
                    | res -> Some (Choice2Of2 res)
                with _ -> None
            match loadAttempt with 
            | Some res -> Some res
            | None -> 
            let rangeStdin = rangeN Lexhelp.stdinMockFilename 0
            let finalAttempt = MagicAssemblyResolution.ResolveAssembly (rangeStdin, tcConfigB, tcImports, fsiDynamicCompiler(), fsiConsoleOutput, aref.QualifiedName)
            match finalAttempt with 
            | null -> None
            | res -> Some (Choice2Of2 res))

#else
    let resolveType _fsiDynamicCompiler (aref: ILAssemblyRef) = 
#if EXTENSIONTYPING
        match tcImports.TryFindProviderGeneratedAssemblyByName aref.Name with
        | Some assembly -> Some (Choice2Of2 assembly)
        | None -> 
#endif
        match tcImports.TryFindExistingFullyQualifiedPathFromAssemblyRef aref with
        | Some resolvedPath -> Some (Choice1Of2 resolvedPath)
        | None -> None
          
#endif
       
    let rec fsiDynamicCompiler = FsiDynamicCompiler(timeReporter, tcConfigB, tcLockObject, errorLogger, outWriter, tcImports, tcGlobals, ilGlobals, fsiOptions, fsiConsoleOutput, niceNameGen, resolveType (fun () -> fsiDynamicCompiler) ) 
    
    let fsiInterruptController = FsiInterruptController(fsiOptions, fsiConsoleOutput) 
    
    do MagicAssemblyResolution.Install(tcConfigB, tcImports, fsiDynamicCompiler, fsiConsoleOutput)
    
    /// This reference cell holds the most recent interactive state 
    let initialInteractiveState = fsiDynamicCompiler.GetInitialInteractiveState ()
    let istateRef = ref initialInteractiveState
      
    let fsiStdinLexerProvider = FsiStdinLexerProvider(tcConfigB, fsiStdinSyphon, fsiConsoleInput, fsiConsoleOutput, fsiOptions, lexResourceManager, errorLogger)

    let fsiIntellisenseProvider = FsiIntellisenseProvider(tcGlobals, tcImports)

    let fsiInteractionProcessor = FsiInteractionProcessor(tcConfigB, errorLogger, fsiOptions, fsiDynamicCompiler, fsiConsolePrompt, fsiConsoleOutput, fsiInterruptController, fsiStdinLexerProvider, lexResourceManager) 

    /// Load the dummy interaction, load the initial files, and,
    /// if interacting, start the background thread to read the standard input.
    member x.Interrupt() = fsiInterruptController.Interrupt()

    /// Performs these steps:
    ///    - Load the dummy interaction, if any
    ///    - Set up exception handling, if any
    ///    - Load the initial files, if any
    ///    - Start the background thread to read the standard input, if any
    ///    - Sit in the GUI event loop indefinitely, if needed

    [<CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2004:RemoveCallsToGCKeepAlive")>]
    member x.Run() = 
#if SILVERLIGHT 
      let _ = fsiInterruptController.InstallKillThread(Thread.CurrentThread, 100)
      let istate = initialInteractiveState
      fsi.EventLoop <- Microsoft.FSharp.Compiler.Interactive.RuntimeHelpers.GetSimpleEventLoop()
      let istate = fsiInteractionProcessor.LoadInitialFiles(false, istate)
      istateRef := istate
      StartStdinReadAndProcessThread(fsiOptions.FsiLCID, istateRef, errorLogger, fsiConsoleInput, fsiConsoleOutput, fsiStdinLexerProvider, fsiInteractionProcessor, true)

      DriveFsiEventLoop fsiConsoleOutput 
#else        
        progress := condition "FSHARP_INTERACTIVE_PROGRESS"
        // Update the console completion function now we've got an initial type checking state.
        // This means completion doesn't work until the initial type checking state has finished loading - fair enough!
        if !progress then fprintfn fsiConsoleOutput.Out "Run: Calling TryGetConsole"
        match fsiConsoleInput.TryGetConsole() with 
        | Some console when fsiOptions.EnableConsoleKeyProcessing -> 
            console.SetCompletionFunction(fun (s1,s2) -> fsiIntellisenseProvider.CompletionsForPartialLID !istateRef (match s1 with | Some s -> s + "." + s2 | None -> s2)  |> Seq.ofList)
        | _ -> ()

    
        if not runningOnMono && fsiOptions.IsInteractiveServer then 
            SpawnInteractiveServer (fsiOptions, fsiConsoleOutput, fsiInterruptController)

        use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Interactive)

        let threadException exn = 
             fsi.EventLoop.Invoke (
                fun () ->          
                    fprintfn fsiConsoleOutput.Error "%s" (exn.ToString())
                    errorLogger.SetError()
                    errorLogger.AbortOnError()
                )


        if fsiOptions.Interact then 

            if !progress then fprintfn fsiConsoleOutput.Out "Run: Interact..."
            // page in the type check env 
            istateRef := fsiInteractionProcessor.LoadDummyInteraction !istateRef
            if !progress then fprintfn fsiConsoleOutput.Out "MAIN: InstallKillThread!";
            
            // Compute how long to pause before a ThreadAbort is actually executed.
            // A somewhat arbitrary choice.
            let pauseMilliseconds = (if fsiOptions.Gui then 400 else 100)

            let exitViaKillThread = fsiInterruptController.InstallKillThread(Thread.CurrentThread, pauseMilliseconds) 
            if !progress then fprintfn fsiConsoleOutput.Out "MAIN: got initial state, creating form";

            // Route background exceptions to the exception handlers
            AppDomain.CurrentDomain.UnhandledException.Add (fun args -> 
                match args.ExceptionObject with 
                | :? System.Exception as err -> threadException err 
                | _ -> ());

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
                try fsi.EventLoop <- WinFormsEventLoop(fsiConsoleOutput, fsiOptions.FsiLCID)
                with e ->
                    printfn "Your system doesn't seem to support WinForms correctly. You will"
                    printfn "need to set fsi.EventLoop use GUI windows from F# Interactive."
                    printfn "You can set different event loops for MonoMac, Gtk#, WinForms and other"
                    printfn "UI toolkits. Drop the --gui argument if no event loop is required."
                    
                                       
            istateRef := fsiInteractionProcessor.LoadInitialFiles (exitViaKillThread, !istateRef)

            StartStdinReadAndProcessThread(fsiOptions.FsiLCID, istateRef, errorLogger, fsiConsoleInput, fsiConsoleOutput, fsiStdinLexerProvider, fsiInteractionProcessor, exitViaKillThread)            

            DriveFsiEventLoop fsiConsoleOutput 

        else // not interact
            if !progress then fprintfn fsiConsoleOutput.Out "Run: not interact, loading intitial files..."
            istateRef := fsiInteractionProcessor.LoadInitialFiles (false, !istateRef)
            if !progress then fprintfn fsiConsoleOutput.Out "Run: done..."
            exit (min errorLogger.ErrorCount 1)

        // The Ctrl-C exception handler that we've passed to native code has
        // to be explicitly kept alive.
        GC.KeepAlive fsiInterruptController.EventHandlers

#endif // SILVERLIGHT



/// Defines a read-only input stream used to feed content to the hosted F# Interactive dynamic compiler.
[<AllowNullLiteral>]
type CompilerInputStream() = 
    inherit Stream()
    // Duration (in milliseconds) of the pause in the loop of waitForAtLeastOneByte. 
    let pauseDuration = 100

    // Queue of characters waiting to be read.
    let readQueue = new Queue<byte>()

    let  waitForAtLeastOneByte(count : int) =
        let rec loop() = 
            let attempt = 
                lock readQueue (fun () ->
                    let n = readQueue.Count
                    if (n >= 1) then 
                        let lengthToRead = if (n < count) then n else count
                        let ret = Array.zeroCreate lengthToRead
                        for i in 0 .. lengthToRead - 1 do
                            ret.[i] <- readQueue.Dequeue()

                        Some ret
                    else 
                        None)
            match attempt with 
            | None -> System.Threading.Thread.Sleep(pauseDuration); loop()
            | Some res -> res
        loop() 

    override x.CanRead = true 
    override x.CanWrite = false
    override x.CanSeek = false
    override x.Position with get() = raise (NotSupportedException()) and set _v = raise (NotSupportedException())
    override x.Length = raise (NotSupportedException()) 
    override x.Flush() = ()
    override x.Seek(_offset, _origin) = raise (NotSupportedException()) 
    override x.SetLength(_value) = raise (NotSupportedException()) 
    override x.Write(_buffer, _offset, _count) = raise (NotSupportedException("Cannot write to input stream")) 
    override x.Read(buffer, offset, count) = 
        let bytes = waitForAtLeastOneByte count
        Array.Copy(bytes, 0, buffer, offset, bytes.Length)
        bytes.Length

    /// Feeds content into the stream.
    member x.Add(str:string) =
        if (System.String.IsNullOrEmpty(str)) then () else

        lock readQueue (fun () -> 
            let bytes = System.Text.Encoding.UTF8.GetBytes(str)
            for i in 0 .. bytes.Length - 1 do
                readQueue.Enqueue(bytes.[i]))



/// Defines a write-only stream used to capture output of the hosted F# Interactive dynamic compiler.
[<AllowNullLiteral>]
type CompilerOutputStream()  =
    inherit Stream()
    // Queue of characters waiting to be read.
    let contentQueue = new Queue<byte>()
    let nyi() = raise (NotSupportedException())

    override x.CanRead = false
    override x.CanWrite = true
    override x.CanSeek = false
    override x.Position with get() = nyi() and set _v = nyi()
    override x.Length = nyi() 
    override x.Flush() = ()
    override x.Seek(_offset, _origin) = nyi() 
    override x.SetLength(_value) = nyi() 
    override x.Read(_buffer, _offset, _count) = raise (NotSupportedException("Cannot write to input stream")) 
    override x.Write(buffer, offset, count) = 
        let stop = offset + count
        if (stop > buffer.Length) then raise (ArgumentException("offset,count"))

        lock contentQueue (fun () -> 
            for i in offset .. stop - 1 do
                contentQueue.Enqueue(buffer.[i]))

    member x.Read() = 
        lock contentQueue (fun () -> 
            let n = contentQueue.Count
            if (n > 0) then 
                let bytes = Array.zeroCreate n
                for i in 0 .. n-1 do 
                    bytes.[i] <- contentQueue.Dequeue()   

                System.Text.Encoding.UTF8.GetString(bytes, 0, n)
            else
                "")
