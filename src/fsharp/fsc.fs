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

// Driver for F# compiler. 
// 
// Roughly divides into:
//    - Parsing
//    - Flags 
//    - Importing IL assemblies
//    - Compiling (including optimizing)
//    - Linking (including ILX-IL transformation)


module internal Microsoft.FSharp.Compiler.Driver 

open System.IO
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text
open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.Ilxgen
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.TypeChecker
open Microsoft.FSharp.Compiler.Infos.AccessibilityLogic
open Microsoft.FSharp.Compiler.Infos.AttributeChecking
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Opt
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Fscopts
open Microsoft.FSharp.Compiler.DiagnosticMessage

#nowarn "45" // This method will be made public in the underlying IL because it may implement an interface or override a method


//----------------------------------------------------------------------------
// Reporting - warnings, errors
//----------------------------------------------------------------------------

let mutable exiter = QuitProcessExiter

/// Create an error logger that counts and prints errors 
let ErrorLoggerThatQuitsAfterMaxErrors (tcConfigB:TcConfigBuilder) = 

    let errors = ref 0

    { new ErrorLogger with 
           member x.ErrorSink(err) = 
                if !errors >= tcConfigB.maxErrors then 
                    DoWithErrorColor true (fun () -> Printf.eprintfn "%s" (FSComp.SR.fscTooManyErrors())) ; 
                    exiter.Exit 1

                DoWithErrorColor false (fun () -> 
                  (writeViaBufferWithEnvironmentNewLines stderr (OutputErrorOrWarning (tcConfigB.implicitIncludeDir,tcConfigB.showFullPaths,tcConfigB.flatErrors,tcConfigB.errorStyle,false)) err;  stderr.WriteLine()));

                incr errors

                match err.Exception with 
                | InternalError _ 
                | Failure _ 
                | :? KeyNotFoundException -> 
                    match tcConfigB.simulateException with
                    | Some _ -> () // Don't show an assert for simulateException case so that unittests can run without an assert dialog.                     
                    | None -> System.Diagnostics.Debug.Assert(false,sprintf "Bug seen in compiler: %s" (err.ToString()))
                | _ -> 
                    ()
           member x.WarnSink(err) =  
                DoWithErrorColor true (fun () -> 
                    if (ReportWarningAsError tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn tcConfigB.specificWarnAsError tcConfigB.specificWarnAsWarn tcConfigB.globalWarnAsError err) then 
                      x.ErrorSink(err)
                    elif ReportWarning tcConfigB.globalWarnLevel tcConfigB.specificWarnOff tcConfigB.specificWarnOn err then 
                      writeViaBufferWithEnvironmentNewLines stderr (OutputErrorOrWarning (tcConfigB.implicitIncludeDir,tcConfigB.showFullPaths,tcConfigB.flatErrors,tcConfigB.errorStyle,true)) err;  
                      stderr.WriteLine())
           member x.ErrorCount = !errors  }

let ErrorLoggerInitial (tcConfigB:TcConfigBuilder) = ErrorLoggerThatQuitsAfterMaxErrors(tcConfigB)

//let ignoreAllFailures f = try f() with _ -> ()

    


let BuildInitialDisplayEnvForDocGeneration tcGlobals = 
    let denv = DisplayEnv.Empty tcGlobals
    let denv = 
        { denv with 
           showImperativeTyparAnnotations=true;
           showAttributes=true; }
    denv.SetOpenPaths 
        [ FSharpLib.RootPath 
          FSharpLib.CorePath 
          FSharpLib.CollectionsPath 
          FSharpLib.ControlPath 
          (IL.splitNamespace FSharpLib.ExtraTopLevelOperatorsName); ] 


module InterfaceFileWriter =

    let WriteInterfaceFile (tcGlobals, tcConfig:TcConfig, TAssembly declaredImpls) =
        /// Use a UTF-8 Encoding with no Byte Order Mark
        let os = 
            if tcConfig.printSignatureFile="" then System.Console.Out
            else (File.CreateText tcConfig.printSignatureFile :> TextWriter)

        if tcConfig.printSignatureFile <> "" && not (List.exists (Filename.checkSuffix tcConfig.printSignatureFile) lightSyntaxDefaultExtensions) then
            fprintfn os "#light" 
            fprintfn os "" 

        for (TImplFile(_,_,mexpr,_,_)) in declaredImpls do
            let denv = BuildInitialDisplayEnvForDocGeneration tcGlobals
            writeViaBufferWithEnvironmentNewLines os (fun os s -> Printf.bprintf os "%s\n\n" s)
              (NicePrint.inferredSigOfModuleExprL true denv mexpr |> Layout.squashTo 80 |> Layout.showL)
       
        if tcConfig.printSignatureFile <> "" then os.Close()


module XmlDocWriter =

    let getDoc xmlDoc = 
        match XmlDoc.Process xmlDoc with
        | XmlDoc [| |] -> ""
        | XmlDoc strs  -> strs |> Array.toList |> String.concat System.Environment.NewLine

    let hasDoc xmlDoc =
        // No need to process the xml doc - just need to know if there's anything there
        match xmlDoc with
        | XmlDoc [| |] -> false
        | _ -> true
        
    let computeXmlDocSigs (tcGlobals,generatedCcu:CcuThunk) =
        (* the xmlDocSigOf* functions encode type into string to be used in "id" *)
        let g = tcGlobals
        let doValSig ptext (v:Val)  = if (hasDoc v.XmlDoc) then v.XmlDocSig <- XmlDocSigOfVal g ptext v
        let doTyconSig ptext (tc:Tycon) = 
            if (hasDoc tc.XmlDoc) then tc.XmlDocSig <- XmlDocSigOfTycon ptext tc
            for vref in tc.MembersOfFSharpTyconSorted do 
                doValSig ptext vref.Deref
            for uc in tc.UnionCasesAsList do
                if (hasDoc uc.XmlDoc) then uc.XmlDocSig <- XmlDocSigOfUnionCase ptext uc.Id.idText tc.CompiledName
            for rf in tc.AllFieldsAsList do
                if (hasDoc rf.XmlDoc) then rf.XmlDocSig <- XmlDocSigOfField ptext rf.Id.idText tc.CompiledRepresentationForNamedType.Name

        let doModuleMemberSig path (m:ModuleOrNamespace) = m.XmlDocSig <- XmlDocSigOfSubModul path
        (* moduleSpec - recurses *)
        let rec doModuleSig path (mspec:ModuleOrNamespace) = 
            let mtype = mspec.ModuleOrNamespaceType
            let path = 
                (* skip the first item in the path which is the assembly name *)
                match path with 
                | None -> Some ""
                | Some "" -> Some mspec.LogicalName
                | Some p -> Some (p+"."+mspec.LogicalName)
            let ptext = match path with None -> "" | Some t -> t
            if mspec.IsModule then doModuleMemberSig ptext mspec;
            let vals = 
                mtype.AllValsAndMembers
                |> Seq.toList
                |> List.filter (fun x  -> not x.IsCompilerGenerated) 
                |> List.filter (fun x -> x.MemberInfo.IsNone || x.IsExtensionMember)
            List.iter (doModuleSig  path)  mtype.ModuleAndNamespaceDefinitions;
            List.iter (doTyconSig  ptext) mtype.ExceptionDefinitions;
            List.iter (doValSig    ptext) vals;
            List.iter (doTyconSig  ptext) mtype.TypeDefinitions
       
        doModuleSig None generatedCcu.Contents;          

    let writeXmlDoc (assemblyName,generatedCcu:CcuThunk,xmlfile) =
        if not (Filename.hasSuffixCaseInsensitive "xml" xmlfile ) then 
            error(Error(FSComp.SR.docfileNoXmlSuffix(), Range.rangeStartup));
        (* the xmlDocSigOf* functions encode type into string to be used in "id" *)
        let members = ref []
        let addMember id xmlDoc = 
            let doc = getDoc xmlDoc
            members := (id,doc) :: !members
        let doVal (v:Val) = addMember v.XmlDocSig v.XmlDoc
        let doUnionCase (uc:UnionCase) = addMember uc.XmlDocSig uc.XmlDoc
        let doField (rf:RecdField) = addMember rf.XmlDocSig rf.XmlDoc
        let doTycon (tc:Tycon) = 
            addMember tc.XmlDocSig tc.XmlDoc;
            for vref in tc.MembersOfFSharpTyconSorted do 
                doVal vref.Deref 
            for uc in tc.UnionCasesAsList do
                doUnionCase uc
            for rf in tc.AllFieldsAsList do
                doField rf

        let modulMember (m:ModuleOrNamespace) = addMember m.XmlDocSig m.XmlDoc
        
        (* moduleSpec - recurses *)
        let rec doModule (mspec:ModuleOrNamespace) = 
            let mtype = mspec.ModuleOrNamespaceType
            if mspec.IsModule then modulMember mspec;
            let vals = 
                mtype.AllValsAndMembers
                |> Seq.toList
                |> List.filter (fun x  -> not x.IsCompilerGenerated) 
                |> List.filter (fun x -> x.MemberInfo.IsNone || x.IsExtensionMember)
            List.iter doModule mtype.ModuleAndNamespaceDefinitions;
            List.iter doTycon mtype.ExceptionDefinitions;
            List.iter doVal vals;
            List.iter doTycon mtype.TypeDefinitions
       
        doModule generatedCcu.Contents;

        use os = File.CreateText(xmlfile)

        fprintfn os ("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
        fprintfn os ("<doc>");
        fprintfn os ("<assembly><name>%s</name></assembly>") assemblyName;
        fprintfn os ("<members>");
        !members |> List.iter (fun (id,doc) -> 
            fprintfn os  "<member name=\"%s\">" id
            fprintfn os  "%s" doc
            fprintfn os  "</member>");
        fprintfn os "</members>"; 
        fprintfn os "</doc>";   


//----------------------------------------------------------------------------
// cmd line - option state
//----------------------------------------------------------------------------

#if SILVERLIGHT
let defaultFSharpBinariesDir = "."
#else
let getModuleFileName() = 
    Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory,
                           System.AppDomain.CurrentDomain.FriendlyName)  

let defaultFSharpBinariesDir = Filename.directoryName (getModuleFileName())
#endif


let outpath outfile extn =
  String.concat "." (["out"; Filename.chopExtension (Path.GetFileName outfile); extn])
  


let TypeCheck (tcConfig,tcImports,tcGlobals,errorLogger:ErrorLogger,assemblyName,niceNameGen,tcEnv0,inputs) =
    try 
        if isNil inputs then error(Error(FSComp.SR.fscNoImplementationFiles(),Range.rangeStartup));
        let ccuName = assemblyName
        let tcInitialState = TypecheckInitialState (rangeStartup,ccuName,tcConfig,tcGlobals,niceNameGen,tcEnv0)
        TypecheckClosedInputSet ((fun () -> errorLogger.ErrorCount = 0),tcConfig,tcImports,tcGlobals,None,tcInitialState,inputs,false)
    with e -> 
        errorRecovery e rangeStartup; 
        exiter.Exit 1



let GenerateInterfaceData(tcConfig:TcConfig) = 
    (* (tcConfig.target = Dll || tcConfig.target = Module) && *)
    not tcConfig.standalone && not tcConfig.noSignatureData 

type ILResource with 
    /// Read the bytes from a resource local to an assembly
    member r.Bytes = 
        match r.Location with 
        | ILResourceLocation.Local b -> b()
        | _-> error(InternalError("Bytes",rangeStartup))

let EncodeInterfaceData(tcConfig:TcConfig,tcGlobals,exportRemapping,generatedCcu,outfile) = 
    try 
      if GenerateInterfaceData(tcConfig) then 
        if verbose then dprintfn "Generating interface data attribute...";
        let resource = WriteSignatureData (tcConfig,tcGlobals,exportRemapping,generatedCcu,outfile)
        if verbose then dprintf "Generated interface data attribute!\n";
        if tcConfig.useOptimizationDataFile || tcGlobals.compilingFslib then 
            let sigDataFileName = (Filename.chopExtension outfile)+".sigdata"
            File.WriteAllBytes(sigDataFileName,resource.Bytes);
        let sigAttr = mkSignatureDataVersionAttr tcGlobals (IL.parseILVersion Internal.Utilities.FSharpEnvironment.FSharpBinaryMetadataFormatRevision) 
        // The resource gets written to a file for FSharp.Core
        let resources = 
            [ if not tcGlobals.compilingFslib then 
                 yield  resource ]
        [sigAttr], resources
      else 
        [],[]
    with e -> 
        errorRecoveryNoRange e; 
        exiter.Exit 1


//----------------------------------------------------------------------------
// EncodeOptimizationData
//----------------------------------------------------------------------------

let GenerateOptimizationData(tcConfig) = 
    (* (tcConfig.target =Dll || tcConfig.target = Module) && *)
    GenerateInterfaceData(tcConfig) 

let EncodeOptimizationData(tcGlobals,tcConfig,outfile,exportRemapping,data) = 
    if GenerateOptimizationData tcConfig then 
        let data = map2Of2 (Opt.RemapLazyModulInfo tcGlobals exportRemapping) data
        if verbose then dprintn "Generating optimization data attribute...";
        // REVIEW: Need a better test
        if tcConfig.useOptimizationDataFile || tcGlobals.compilingFslib then 
            let ccu,modulInfo = data
            let bytes = Pickle.pickleObjWithDanglingCcus outfile tcGlobals ccu Opt.p_LazyModuleInfo modulInfo
            let optDataFileName = (Filename.chopExtension outfile)+".optdata"
            File.WriteAllBytes(optDataFileName,bytes);
        // As with the sigdata file, the optdata gets written to a file for FSharp.Core and FSharp.Compiler.Silverlight
        if tcGlobals.compilingFslib then 
            []
        else
            let data = 
                if tcConfig.onlyEssentialOptimizationData || tcConfig.useOptimizationDataFile 
                then map2Of2 Opt.AbstractLazyModulInfoToEssentials data 
                else data
            [ WriteOptimizationData tcGlobals outfile data ]
    else
        [ ]

//----------------------------------------------------------------------------
// .res file format, for encoding the assembly version attribute. 
//--------------------------------------------------------------------------

// Helpers for generating binary blobs
module BinaryGenerationUtilities = 
    // Little-endian encoding of int32 
    let b0 n =  byte (n &&& 0xFF)
    let b1 n =  byte ((n >>> 8) &&& 0xFF)
    let b2 n =  byte ((n >>> 16) &&& 0xFF)
    let b3 n =  byte ((n >>> 24) &&& 0xFF)

    let i16 (i:int32) = [| b0 i; b1 i |]
    let i32 (i:int32) = [| b0 i; b1 i; b2 i; b3 i |]

    // Emit the bytes and pad to a 32-bit alignment
    let Padded initialAlignment (v:byte[]) = 
        [| yield! v
           for _ in 1..(4 - (initialAlignment + v.Length) % 4) % 4 do
               yield 0x0uy |]

// Generate nodes in a .res file format. These are then linked by Abstract IL using the 
// linkNativeResources function, which invokes the cvtres.exe utility
module ResFileFormat = 
    open BinaryGenerationUtilities
    
    let ResFileNode(dwTypeID,dwNameID,wMemFlags,wLangID,data:byte[]) =
        [| yield! i32 data.Length  // DWORD ResHdr.dwDataSize
           yield! i32 0x00000020  // dwHeaderSize
           yield! i32 ((dwTypeID <<< 16) ||| 0x0000FFFF)  // dwTypeID,sizeof(DWORD)
           yield! i32 ((dwNameID <<< 16) ||| 0x0000FFFF)   // dwNameID,sizeof(DWORD)
           yield! i32 0x00000000 // DWORD       dwDataVersion
           yield! i16 wMemFlags // WORD        wMemFlags
           yield! i16 wLangID   // WORD        wLangID
           yield! i32 0x00000000 // DWORD       dwVersion
           yield! i32 0x00000000 // DWORD       dwCharacteristics
           yield! Padded 0 data |]

    let ResFileHeader() = ResFileNode(0x0,0x0,0x0,0x0,[| |]) 

// Generate the VS_VERSION_INFO structure held in a Win32 Version Resource in a PE file
//
// Web reference: http://www.piclist.com/tecHREF/os/win/api/win32/struc/src/str24_5.htm
module VersionResourceFormat = 
    open BinaryGenerationUtilities

    let VersionInfoNode(data:byte[]) =
        [| yield! i16 (data.Length + 2) // wLength : int16; // Specifies the length, in bytes, of the VS_VERSION_INFO structure. This length does not include any padding that aligns any subsequent version resource data on a 32-bit boundary. 
           yield! data |]

    let VersionInfoElement(wType, szKey, valueOpt: byte[] option, children:byte[][], isString) =
        // for String structs, wValueLength represents the word count, not the byte count
        let wValueLength = (match valueOpt with None -> 0 | Some value -> (if isString then value.Length / 2 else value.Length))
        VersionInfoNode
            [| yield! i16 wValueLength // wValueLength: int16. Specifies the length, in words, of the Value member. This value is zero if there is no Value member associated with the current version structure. 
               yield! i16 wType        // wType : int16; Specifies the type of data in the version resource. This member is 1 if the version resource contains text data and 0 if the version resource contains binary data. 
               yield! Padded 2 szKey 
               match valueOpt with 
               | None -> yield! []
               | Some value -> yield! Padded 0 value 
               for child in children do 
                   yield! child  |]

    let Version((v1,v2,v3,v4):ILVersionInfo) = 
        [| yield! i32 (int32 v1 <<< 16 ||| int32 v2) // DWORD dwFileVersionMS; // Specifies the most significant 32 bits of the file's binary version number. This member is used with dwFileVersionLS to form a 64-bit value used for numeric comparisons. 
           yield! i32 (int32 v3 <<< 16 ||| int32 v4) // DWORD dwFileVersionLS; // Specifies the least significant 32 bits of the file's binary version number. This member is used with dwFileVersionMS to form a 64-bit value used for numeric comparisons. 
        |]

    let String(string,value) = 
        let wType = 0x1 // Specifies the type of data in the version resource. This member is 1 if the version resource contains text data and 0 if the version resource contains binary data. 
        let szKey = Bytes.stringAsUnicodeNullTerminated string
        VersionInfoElement(wType, szKey, Some(Bytes.stringAsUnicodeNullTerminated value),[| |],true)

    let StringTable(language,strings) = 
        let wType = 0x1 // Specifies the type of data in the version resource. This member is 1 if the version resource contains text data and 0 if the version resource contains binary data. 
        let szKey = Bytes.stringAsUnicodeNullTerminated language
             // Specifies an 8-digit hexadecimal number stored as a Unicode string. The four most significant digits represent the language identifier. The four least significant digits represent the code page for which the data is formatted. 
             // Each Microsoft Standard Language identifier contains two parts: the low-order 10 bits specify the major language, and the high-order 6 bits specify the sublanguage. For a table of valid identifiers see Language Identifiers. 
                       
        let children =  
            [| for string in strings do
                   yield String(string) |] 
        VersionInfoElement(wType, szKey, None,children,false)

    let StringFileInfo(stringTables: #seq<string * #seq<string * string> >) = 
        let wType = 0x1 // Specifies the type of data in the version resource. This member is 1 if the version resource contains text data and 0 if the version resource contains binary data. 
        let szKey = Bytes.stringAsUnicodeNullTerminated "StringFileInfo" // Contains the Unicode string StringFileInfo
        // Contains an array of one or more StringTable structures. Each StringTable structures szKey member indicates the appropriate language and code page for displaying the text in that StringTable structure. 
        let children =  
            [| for stringTable in stringTables do
                   yield StringTable(stringTable) |] 
        VersionInfoElement(wType, szKey, None,children,false)
        
    let VarFileInfo(vars: #seq<int32 * int32>) = 
        let wType = 0x1 // Specifies the type of data in the version resource. This member is 1 if the version resource contains text data and 0 if the version resource contains binary data. 
        let szKey = Bytes.stringAsUnicodeNullTerminated "VarFileInfo" // Contains the Unicode string StringFileInfo
        // Contains an array of one or more StringTable structures. Each StringTable structures szKey member indicates the appropriate language and code page for displaying the text in that StringTable structure. 
        let children =  
            [| for (lang,codePage) in vars do
                   let szKey = Bytes.stringAsUnicodeNullTerminated "Translation"
                   yield VersionInfoElement(0x0,szKey, Some([| yield! i16 lang
                                                               yield! i16 codePage |]), [| |],false) |] 
        VersionInfoElement(wType, szKey, None,children,false)
        
    let VS_FIXEDFILEINFO(fileVersion:ILVersionInfo,
                         productVersion:ILVersionInfo,
                         dwFileFlagsMask,
                         dwFileFlags,dwFileOS,
                         dwFileType,dwFileSubtype,
                         lwFileDate:int64) = 
        let dwStrucVersion = 0x00010000
        [| yield! i32  0xFEEF04BD // DWORD dwSignature; // Contains the value 0xFEEFO4BD. This is used with the szKey member of the VS_VERSION_INFO structure when searching a file for the VS_FIXEDFILEINFO structure. 
           yield! i32 dwStrucVersion // DWORD dwStrucVersion; // Specifies the binary version number of this structure. The high-order word of this member contains the major version number, and the low-order word contains the minor version number. 
           yield! Version fileVersion // DWORD dwFileVersionMS,dwFileVersionLS; // Specifies the most/least significant 32 bits of the file's binary version number. This member is used with dwFileVersionLS to form a 64-bit value used for numeric comparisons. 
           yield! Version productVersion // DWORD dwProductVersionMS,dwProductVersionLS; // Specifies the most/least significant 32 bits of the file's binary version number. This member is used with dwFileVersionLS to form a 64-bit value used for numeric comparisons. 
           yield! i32 dwFileFlagsMask // DWORD dwFileFlagsMask; // Contains a bitmask that specifies the valid bits in dwFileFlags. A bit is valid only if it was defined when the file was created. 
           yield! i32 dwFileFlags // DWORD dwFileFlags; // Contains a bitmask that specifies the Boolean attributes of the file. This member can include one or more of the following values: 
                  //          VS_FF_DEBUG 0x1L             The file contains debugging information or is compiled with debugging features enabled. 
                  //          VS_FF_INFOINFERRED            The file's version structure was created dynamically; therefore, some of the members in this structure may be empty or incorrect. This flag should never be set in a file's VS_VERSION_INFO data. 
                  //          VS_FF_PATCHED            The file has been modified and is not identical to the original shipping file of the same version number. 
                  //          VS_FF_PRERELEASE            The file is a development version, not a commercially released product. 
                  //          VS_FF_PRIVATEBUILD            The file was not built using standard release procedures. If this flag is set, the StringFileInfo structure should contain a PrivateBuild entry. 
                  //          VS_FF_SPECIALBUILD            The file was built by the original company using standard release procedures but is a variation of the normal file of the same version number. If this flag is set, the StringFileInfo structure should contain a SpecialBuild entry. 
           yield! i32 dwFileOS //Specifies the operating system for which this file was designed. This member can be one of the following values: Flag 
                  //VOS_DOS 0x0001L  The file was designed for MS-DOS. 
                  //VOS_NT  0x0004L  The file was designed for Windows NT. 
                  //VOS__WINDOWS16  The file was designed for 16-bit Windows. 
                  //VOS__WINDOWS32  The file was designed for the Win32 API. 
                  //VOS_OS216 0x00020000L  The file was designed for 16-bit OS/2. 
                  //VOS_OS232  0x00030000L  The file was designed for 32-bit OS/2. 
                  //VOS__PM16  The file was designed for 16-bit Presentation Manager. 
                  //VOS__PM32  The file was designed for 32-bit Presentation Manager. 
                  //VOS_UNKNOWN  The operating system for which the file was designed is unknown to Windows. 
           yield! i32 dwFileType // Specifies the general type of file. This member can be one of the following values: 
     
                //VFT_UNKNOWN The file type is unknown to Windows. 
                //VFT_APP  The file contains an application. 
                //VFT_DLL  The file contains a dynamic-link library (DLL). 
                //VFT_DRV  The file contains a device driver. If dwFileType is VFT_DRV, dwFileSubtype contains a more specific description of the driver. 
                //VFT_FONT  The file contains a font. If dwFileType is VFT_FONT, dwFileSubtype contains a more specific description of the font file. 
                //VFT_VXD  The file contains a virtual device. 
                //VFT_STATIC_LIB  The file contains a static-link library. 

           yield! i32 dwFileSubtype //     Specifies the function of the file. The possible values depend on the value of dwFileType. For all values of dwFileType not described in the following list, dwFileSubtype is zero. If dwFileType is VFT_DRV, dwFileSubtype can be one of the following values: 
                      //VFT2_UNKNOWN  The driver type is unknown by Windows. 
                      //VFT2_DRV_COMM  The file contains a communications driver. 
                      //VFT2_DRV_PRINTER  The file contains a printer driver. 
                      //VFT2_DRV_KEYBOARD  The file contains a keyboard driver. 
                      //VFT2_DRV_LANGUAGE  The file contains a language driver. 
                      //VFT2_DRV_DISPLAY  The file contains a display driver. 
                      //VFT2_DRV_MOUSE  The file contains a mouse driver. 
                      //VFT2_DRV_NETWORK  The file contains a network driver. 
                      //VFT2_DRV_SYSTEM  The file contains a system driver. 
                      //VFT2_DRV_INSTALLABLE  The file contains an installable driver. 
                      //VFT2_DRV_SOUND  The file contains a sound driver. 
                      //
                      //If dwFileType is VFT_FONT, dwFileSubtype can be one of the following values: 
                      // 
                      //VFT2_UNKNOWN  The font type is unknown by Windows. 
                      //VFT2_FONT_RASTER  The file contains a raster font. 
                      //VFT2_FONT_VECTOR  The file contains a vector font. 
                      //VFT2_FONT_TRUETYPE  The file contains a TrueType font. 
                      //
                      //If dwFileType is VFT_VXD, dwFileSubtype contains the virtual device identifier included in the virtual device control block. 
           yield! i32 (int32 (lwFileDate >>> 32)) // Specifies the most significant 32 bits of the file's 64-bit binary creation date and time stamp. 
           yield! i32 (int32 lwFileDate) //Specifies the least significant 32 bits of the file's 64-bit binary creation date and time stamp. 
         |] 


    let VS_VERSION_INFO(fixedFileInfo,stringFileInfo,varFileInfo)  =
        let wType = 0x0 
        let szKey = Bytes.stringAsUnicodeNullTerminated "VS_VERSION_INFO" // Contains the Unicode string VS_VERSION_INFO
        let value = VS_FIXEDFILEINFO (fixedFileInfo)
        let children =  
            [| yield StringFileInfo(stringFileInfo) 
               yield VarFileInfo(varFileInfo) 
            |] 
        VersionInfoElement(wType, szKey, Some(value),children,false)
       
    let VS_VERSION_INFO_RESOURCE(data) = 
        let dwTypeID = 0x0010
        let dwNameID = 0x0001
        let wMemFlags = 0x0030 
        let wLangID = 0x0
        ResFileFormat.ResFileNode(dwTypeID, dwNameID,wMemFlags,wLangID,VS_VERSION_INFO(data))
        
module ManifestResourceFormat =
    
    let VS_MANIFEST_RESOURCE(data, isLibrary) =
        let dwTypeID = 0x0018
        let dwNameID = if isLibrary then 0x2 else 0x1
        let wMemFlags = 0x0
        let wLangID = 0x0
        ResFileFormat.ResFileNode(dwTypeID, dwNameID, wMemFlags, wLangID, data)

/// Helpers for finding attributes
module AttributeHelpers = 

    /// Try to find an attribute that takes a string argument
    let TryFindStringAttribute tcGlobals attrib attribs =
        match TryFindAttrib tcGlobals (mkMscorlibAttrib tcGlobals attrib) attribs with
        | Some (Attrib(_,_,[ AttribStringArg(s) ],_,_,_))  -> Some (s)
        | _ -> None
        
    let TryFindIntAttribute tcGlobals attrib attribs =
        match TryFindAttrib tcGlobals (mkMscorlibAttrib tcGlobals attrib) attribs with
        | Some (Attrib(_,_,[ AttribInt32Arg(i) ],_,_,_)) -> Some (i)
        | _ -> None
        
    let TryFindBoolAttribute tcGlobals attrib attribs =
        match TryFindAttrib tcGlobals (mkMscorlibAttrib tcGlobals attrib) attribs with
        | Some (Attrib(_,_,[ AttribBoolArg(p) ],_,_,_)) -> Some (p)
        | _ -> None

    // Try to find an AssemblyVersion attribute 
    let TryFindVersionAttribute tcGlobals attrib attribs =
        match TryFindStringAttribute tcGlobals attrib attribs with
        | Some versionString ->
             try Some(IL.parseILVersion versionString)
             with e -> 
                 warning(Error(FSComp.SR.fscBadAssemblyVersion(versionString),Range.rangeStartup));
                 None
        | _ -> None


let injectedCompatTypes = set [ "System.Tuple`1"; 
        "System.Tuple`2" ; 
        "System.Tuple`3" ; 
        "System.Tuple`4"; 
        "System.Tuple`5"; 
        "System.Tuple`6"; 
        "System.Tuple`7"; 
        "System.Tuple`8"; 
        "System.ITuple"; 
        "System.Tuple"; 
        //"System.System_LazyDebugView`1"; 
        //"System.Threading.LazyExecutionMode"; 
        //"System.Threading.LazyInternalExceptionHolder"; 
        //"System.Threading.LazyBlock`1"; 
        "System.Collections.IStructuralComparable"; 
        "System.Collections.IStructuralEquatable"; 
      ]

let typesForwardedToMscorlib = 
    set [  "System.AggregateException";
            "System.Threading.CancellationTokenRegistration";
            "System.Threading.CancellationToken";
            "System.Threading.CancellationTokenSource";
            "System.Lazy`1";
            "System.IObservable`1";
            "System.IObserver`1";
        ]
let typesForwardedToSystemNumerics =
    set [ "System.Numerics.BigInteger"; ]
      
let createMscorlibExportList tcGlobals =
    // We want to write forwarders out for all injected types except for System.ITuple, which is internal
    // Forwarding System.ITuple will cause FxCop failures on 4.0
    Set.union (Set.filter (fun t -> t <> "System.ITuple") injectedCompatTypes) typesForwardedToMscorlib |>
        Seq.map (fun t -> 
                    {   ScopeRef = tcGlobals.sysCcu.ILScopeRef ; 
                        Name = t ; 
                        IsForwarder = true ; 
                        Access = ILTypeDefAccess.Public ; 
                        Nested = mkILNestedExportedTypes List.empty<ILNestedExportedType> ; 
                        CustomAttrs = mkILCustomAttrs List.empty<ILAttribute>  }) |> 
        Seq.toList

let createSystemNumericsExportList tcGlobals =
    let sysAssemblyRef = tcGlobals.sysCcu.ILScopeRef.AssemblyRef
    let systemNumericsAssemblyRef = ILAssemblyRef.Create("System.Numerics", sysAssemblyRef.Hash, sysAssemblyRef.PublicKey, sysAssemblyRef.Retargetable, sysAssemblyRef.Version, sysAssemblyRef.Locale)
    typesForwardedToSystemNumerics |>
        Seq.map (fun t ->
                    {   ScopeRef = ILScopeRef.Assembly(systemNumericsAssemblyRef)
                        Name = t;
                        IsForwarder = true ;
                        Access = ILTypeDefAccess.Public ;
                        Nested = mkILNestedExportedTypes List.empty<ILNestedExportedType> ;
                        CustomAttrs = mkILCustomAttrs List.empty<ILAttribute> }) |>
        Seq.toList
            
module MainModuleBuilder = 
    let CreateMainModule  
            (tcConfig:TcConfig,tcGlobals,
             pdbfile,assemblyName,outfile,topAttrs,
             (iattrs,intfDataResources),optDataResources,
             codegenResults,assemVerFromAttrib,metadataVersion,secDecls) =


        if !progress then dprintf "Creating main module...\n";
        let ilTypeDefs = 
            //let topTypeDef = mkILTypeDefForGlobalFunctions tcGlobals.ilg (mkILMethods [], emptyILFields)
            mkILTypeDefs codegenResults.ilTypeDefs
            


        let mainModule = 
            let hashAlg = AttributeHelpers.TryFindIntAttribute tcGlobals "System.Reflection.AssemblyAlgorithmIdAttribute" topAttrs.assemblyAttrs
            let locale = AttributeHelpers.TryFindStringAttribute tcGlobals "System.Reflection.AssemblyCultureAttribute" topAttrs.assemblyAttrs
            let flags =  match AttributeHelpers.TryFindIntAttribute tcGlobals "System.Reflection.AssemblyFlagsAttribute" topAttrs.assemblyAttrs with | Some(f) -> f | _ -> 0x0
            
            // You're only allowed to set a locale if the assembly is a library
            if (locale <> None && locale.Value <> "") && tcConfig.target <> Dll then
              error(Error(FSComp.SR.fscAssemblyCultureAttributeError(),rangeCmdArgs))

            // Add the type forwarders for the .NET 4.0 FSharp.Core.dll
            let exportedTypesList = if (tcConfig.compilingFslib && tcConfig.compilingFslib40) then (List.append (createMscorlibExportList tcGlobals) (createSystemNumericsExportList tcGlobals)) else []
            
            mkILSimpleModule assemblyName (fsharpModuleName tcConfig.target assemblyName) (tcConfig.target = Dll || tcConfig.target = Module) ilTypeDefs hashAlg locale flags (mkILExportedTypes exportedTypesList) metadataVersion

        let disableJitOptimizations = not (tcConfig.optSettings.jitOpt())
        let manifestAttrs = 
             mkILCustomAttrs 
                 [ if not tcConfig.internConstantStrings then 
                       yield mkILCustomAttribute tcGlobals.ilg
                                 (mkILTyRef (tcGlobals.ilg.mscorlibScopeRef, "System.Runtime.CompilerServices.CompilationRelaxationsAttribute"),
                                  [tcGlobals.ilg.typ_Int32],[ILAttribElem.Int32( 8)], []) 
                   yield! iattrs
                   yield! codegenResults.ilAssemAttrs
                   if Option.isSome pdbfile then 
                       yield (mkDebuggableAttributeV2 tcGlobals.ilg (tcConfig.jitTracking, tcConfig.ignoreSymbolStoreSequencePoints, disableJitOptimizations, false (* enableEnC *) )) ]
                       
        let tcVersion = tcConfig.version.GetVersionInfo(tcConfig.implicitIncludeDir)
        let manifest = 
             if tcConfig.target = Module then None else
             let man = mainModule.ManifestOfAssembly
             let ver = 
                 match assemVerFromAttrib with 
                 | None -> tcVersion
                 | Some v -> v
             Some { man with Version= Some(ver);
                             CustomAttrs = manifestAttrs;
                             DisableJitOptimizations=disableJitOptimizations;
                             JitTracking= tcConfig.jitTracking;
                             SecurityDecls=secDecls } 
                  
        let quotDataResources = 
                codegenResults.quotationResourceBytes |> List.map (fun bytes -> 
                    { Name=QuotationPickler.pickledDefinitionsResourceNameBase+string(newUnique());
                      Location = ILResourceLocation.Local (fun () -> bytes);
                      Access= ILResourceAccess.Public;
                      CustomAttrs = emptyILCustomAttrs }) 

        let resources = 
          mkILResources 
            [ for file in tcConfig.embedResources do
                 let name,bytes,pub = 
                     let lower = String.lowercase file
                     if List.exists (Filename.checkSuffix lower) [".resx"]  then
#if SILVERLIGHT
                         failwith "resx files not supported as legacy compiler inputs"
#else
                         let file = tcConfig.ResolveSourceFile(rangeStartup,file)
                         let outfile = (file |> Filename.chopExtension) + ".resources"
                         
                         let readResX(f:string) = 
                             use rsxr = new System.Resources.ResXResourceReader(f)
                             rsxr 
                             |> Seq.cast 
                             |> Seq.toList
                             |> List.map (fun (d:System.Collections.DictionaryEntry) -> (d.Key :?> string), d.Value)
                         let writeResources((r:(string * obj) list),(f:string)) = 
                             use writer = new System.Resources.ResourceWriter(f)
                             r |> List.iter (fun (k,v) -> writer.AddResource(k,v))
                         writeResources(readResX(file),outfile);
                         let file,name,pub = TcConfigBuilder.SplitCommandLineResourceInfo outfile
                         let file = tcConfig.ResolveSourceFile(rangeStartup,file)
                         let bytes = File.ReadAllBytesShim file
                         File.Delete outfile;
                         name,bytes,pub
#endif
                     else

                         let file,name,pub = TcConfigBuilder.SplitCommandLineResourceInfo file
                         let file = tcConfig.ResolveSourceFile(rangeStartup,file)
                         let bytes = File.ReadAllBytesShim file
                         name,bytes,pub
                 yield { Name=name; 
                         Location=ILResourceLocation.Local (fun () -> bytes); 
                         Access=pub; 
                         CustomAttrs=emptyILCustomAttrs }
               
              yield! quotDataResources
              yield! intfDataResources
              yield! optDataResources
              for ri in tcConfig.linkResources do 
                 let file,name,pub = TcConfigBuilder.SplitCommandLineResourceInfo ri
                 yield { Name=name; 
                         Location=ILResourceLocation.File(ILModuleRef.Create(name=file, hasMetadata=false, hash=Some (sha1HashBytes (File.ReadAllBytesShim file))), 0);
                         Access=pub; 
                         CustomAttrs=emptyILCustomAttrs } ]

        //NOTE: the culture string can be turned into a number using this:
        //    sprintf "%04x" (System.Globalization.CultureInfo.GetCultureInfo("en").KeyboardLayoutId )
        let assemblyVersionResources =
            let assemblyVersion = 
                match tcConfig.version with
                | VersionNone ->assemVerFromAttrib
                | _ -> Some(tcVersion)
            match assemblyVersion with 
            | None -> []
            | Some(assemblyVersion) ->
                let FindAttribute key attrib = 
                    match AttributeHelpers.TryFindStringAttribute tcGlobals attrib topAttrs.assemblyAttrs with
                    | Some text  -> [(key,text)]
                    | _ -> []

                let fileVersion = 
                    match AttributeHelpers.TryFindVersionAttribute tcGlobals "System.Reflection.AssemblyFileVersionAttribute" topAttrs.assemblyAttrs with
                    | Some v -> v
                    | None -> assemblyVersion

                let productVersion = 
                    match AttributeHelpers.TryFindVersionAttribute tcGlobals "System.Reflection.AssemblyInformationalVersionAttribute" topAttrs.assemblyAttrs with
                    | Some v -> v
                    | None -> assemblyVersion

                let stringFileInfo = 
                     // 000004b0:
                     // Specifies an 8-digit hexadecimal number stored as a Unicode string. The four most significant digits represent the language identifier. The four least significant digits represent the code page for which the data is formatted. 
                     // Each Microsoft Standard Language identifier contains two parts: the low-order 10 bits specify the major language, and the high-order 6 bits specify the sublanguage. For a table of valid identifiers see Language Identifiers.                                           //
                     // see e.g. http://msdn.microsoft.com/en-us/library/aa912040.aspx 0000 is neutral and 04b0(hex)=1252(dec) is the code page.
                      [ ("000004b0", [ yield ("Assembly Version", (let v1,v2,v3,v4 = assemblyVersion in sprintf "%d.%d.%d.%d" v1 v2 v3 v4))
                                       yield ("FileVersion", (let v1,v2,v3,v4 = fileVersion in sprintf "%d.%d.%d.%d" v1 v2 v3 v4))
                                       yield ("ProductVersion", (let v1,v2,v3,v4 = productVersion in sprintf "%d.%d.%d.%d" v1 v2 v3 v4))
                                       yield! FindAttribute "Comments" "System.Reflection.AssemblyDescriptionAttribute" 
                                       yield! FindAttribute "FileDescription" "System.Reflection.AssemblyTitleAttribute" 
                                       yield! FindAttribute "ProductName" "System.Reflection.AssemblyProductAttribute" 
                                       yield! FindAttribute "CompanyName" "System.Reflection.AssemblyCompanyAttribute" 
                                       yield! FindAttribute "LegalCopyright" "System.Reflection.AssemblyCopyrightAttribute" 
                                       yield! FindAttribute "LegalTrademarks" "System.Reflection.AssemblyTrademarkAttribute" ]) ]

            
            // These entries listed in the MSDN documentation as "standard" string entries are not yet settable
            
            // InternalName: The Value member identifies the file's internal name, if one exists. For example, this string could contain the module name for Windows dynamic-link libraries (DLLs), a virtual device name for Windows virtual devices, or a device name for MS-DOS device drivers. 
            // OriginalFilename: The Value member identifies the original name of the file, not including a path. This enables an application to determine whether a file has been renamed by a user. This name may not be MS-DOS 8.3-format if the file is specific to a non-FAT file system. 
            // PrivateBuild: The Value member describes by whom, where, and why this private version of the file was built. This string should only be present if the VS_FF_PRIVATEBUILD flag is set in the dwFileFlags member of the VS_FIXEDFILEINFO structure. For example, Value could be 'Built by OSCAR on \OSCAR2'. 
            // SpecialBuild: The Value member describes how this version of the file differs from the normal version. This entry should only be present if the VS_FF_SPECIALBUILD flag is set in the dwFileFlags member of the VS_FIXEDFILEINFO structure. For example, Value could be 'Private build for Olivetti solving mouse problems on M250 and M250E computers'. 



                // "If you use the Var structure to list the languages your application 
                // or DLL supports instead of using multiple version resources, 
                // use the Value member to contain an array of DWORD values indicating the 
                // language and code page combinations supported by this file. The 
                // low-order word of each DWORD must contain a Microsoft language identifier, 
                // and the high-order word must contain the IBM code page number. 
                // Either high-order or low-order word can be zero, indicating that 
                // the file is language or code page independent. If the Var structure is 
                // omitted, the file will be interpreted as both language and code page independent. "
                let varFileInfo = [ (0x0, 0x04b0)  ]

                let fixedFileInfo = 
                    let dwFileFlagsMask = 0x3f 
                    let dwFileFlags = 0x00 
                    let dwFileOS = 0x04 
                    let dwFileType = 0x01 
                    let dwFileSubtype = 0x00 
                    let lwFileDate = 0x00L 
                    (fileVersion,productVersion,dwFileFlagsMask,dwFileFlags,dwFileOS,dwFileType,dwFileSubtype,lwFileDate)

                let vsVersionInfoResource = 
                    VersionResourceFormat.VS_VERSION_INFO_RESOURCE(fixedFileInfo,stringFileInfo,varFileInfo)
                
                
                let resource = 
                    [| yield! ResFileFormat.ResFileHeader()
                       yield! vsVersionInfoResource |]
#if DUMP_ASSEMBLY_RESOURCE
                for i in 0..(resource.Length+15)/16 - 1 do
                    for j in 0..15 do
                        if j % 2 = 0 then printf " " 
                        printf "%02x" resource.[min (i*16+j) (resource.Length - 1)]
                    printf " " 
                    for j in 0..15 do
                        printf "%c" (let c = char resource.[min (i*16+j) (resource.Length - 1)] in if c > ' ' && c < '~' then c else '.')
                    printfn "" 
#endif
                [ resource ]
          
        // a user cannot specify both win32res and win32manifest        
        if not(tcConfig.win32manifest = "") && not(tcConfig.win32res = "") then
            error(Error(FSComp.SR.fscTwoResourceManifests(),rangeCmdArgs));
                      
        let win32Manifest =
#if SILVERLIGHT
           ""
#else


           if not(tcConfig.win32manifest = "") then
               tcConfig.win32manifest
           elif not(tcConfig.includewin32manifest) || not(tcConfig.win32res = "") || runningOnMono then // don't embed a manifest if a native resource is being included
               ""
           else
               match Build.highestInstalledNetFrameworkVersionMajorMinor() with
               | _,_,_,"v3.5" -> System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory() + @"..\v3.5\default.win32manifest"
               | _,_,_,"v4.0" -> System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory() + @"default.win32manifest"
               | _,_,_,_ -> "" // only have default manifests for 3.5 and 4.0               
#endif
        
        let nativeResources = 
#if SILVERLIGHT
            []
#else
            [ for av in assemblyVersionResources do
                  yield Lazy.CreateFromValue av
              if not(tcConfig.win32res = "") then
                  yield Lazy.CreateFromValue (File.ReadAllBytesShim tcConfig.win32res) 
              if tcConfig.includewin32manifest && not(win32Manifest = "") && not(runningOnMono) then
                  yield  Lazy.CreateFromValue [|   yield! ResFileFormat.ResFileHeader() 
                                                   yield! (ManifestResourceFormat.VS_MANIFEST_RESOURCE((File.ReadAllBytesShim win32Manifest), tcConfig.target = Dll))|]]

#endif

        // Add attributes, version number, resources etc. 
        {mainModule with 
              StackReserveSize = tcConfig.stackReserveSize
              Name = (if tcConfig.target = Module then Path.GetFileName outfile else mainModule.Name);
              SubSystemFlags = (if tcConfig.target = WinExe then 2 else 3) ;
              Resources= resources;
              ImageBase = (match tcConfig.baseAddress with None -> 0x00400000l | Some b -> b);
              IsDLL=(tcConfig.target = Dll || tcConfig.target=Module);
              Platform = tcConfig.platform ;
              Is32Bit=(match tcConfig.platform with Some X86 -> true | _ -> false);
              Is64Bit=(match tcConfig.platform with Some AMD64 | Some IA64 -> true | _ -> false);          
              CustomAttrs= mkILCustomAttrs ((if tcConfig.target = Module then iattrs else []) @ codegenResults.ilNetModuleAttrs);
              NativeResources=nativeResources;
              Manifest = manifest }



/// OPTIONAL STATIC LINKING OF ALL DLLs THAT DEPEND ON THE F# LIBRARY
module StaticLinker = 
    [<NoEquality; NoComparison>]
    type Node = 
        { name: string;
          data: ILModuleDef; 
          ccu: option<CcuThunk>;
          refs: ILReferences;
          mutable edges: list<Node>; 
          mutable visited: bool }


    let StaticLinkModules tcConfig ilGlobals ilxMainModule (dependentModules: (CcuThunk option * ILModuleDef) list) = 
        if isNil dependentModules then 
            ilxMainModule,(fun x -> x) 
        else

            match dependentModules |> List.tryPick (function (Some ccu,_) when ccu.UsesQuotations -> Some ccu | _ -> None)  with
            | Some ccu -> error(Error(FSComp.SR.fscQuotationLiteralsStaticLinking(ccu.AssemblyName),rangeStartup));
            | None -> ()
                
            if dependentModules |> List.exists (fun (_,x) -> not x.IsDLL)  then 
                error(Error(FSComp.SR.fscStaticLinkingNoEXE(),rangeStartup))
            if dependentModules |> List.exists (fun (_,x) -> not x.IsILOnly)  then 
                error(Error(FSComp.SR.fscStaticLinkingNoMixedDLL(),rangeStartup))
            let dependentModules  = dependentModules |> List.map snd
            let assems = 
                dependentModules 
                |> List.choose (fun m -> match m.Manifest with Some m -> Some m.Name | _ -> None)
                |> Set.ofList

            (* Rewrite scope references to be local references *)
            let rewriteExternalRefsToLocalRefs x = 
                if assems.Contains(getNameOfScopeRef x) then ILScopeRef.Local else x
            let savedResources = 
                let allResources = 
                    dependentModules 
                    |> List.map (fun m -> m.Resources.AsList)
                    |> List.concat
                // Save only the interface/optimization attributes of generated data 
                let intfDataResources,others = 
                    let intfDataResources,others = List.partition IsSignatureDataResource allResources
                    let intfDataResources = if GenerateInterfaceData(tcConfig)  then intfDataResources else []
                    intfDataResources,others
                let optDataResources,others = 
                    let optDataResources,others = List.partition IsOptimizationDataResource others
                    let optDataResources = if GenerateOptimizationData(tcConfig)  then optDataResources else []
                    optDataResources,others
                let rresources,others = 
                    let rresources,others = List.partition IsReflectedDefinitionsResource others
                    let rresources = rresources |> List.mapi (fun i r -> {r with Name = QuotationPickler.pickledDefinitionsResourceNameBase+string (i+1)})
                    rresources,others
                if verbose then dprintf "#intfDataResources = %d, #optDataResources = %d, #rresources = %d\n" intfDataResources.Length optDataResources.Length rresources.Length;
                intfDataResources@optDataResources@rresources@others
            let moduls = ilxMainModule :: dependentModules
            let topTypeDefs,normalTypeDefs = 
                moduls 
                |> List.map (fun m -> m.TypeDefs.AsList |> List.partition (fun td -> isTypeNameForGlobalFunctions td.Name)) 
                |> List.unzip
            let topTypeDef = 
                let topTypeDefs = List.concat topTypeDefs
                mkILTypeDefForGlobalFunctions ilGlobals
                   (mkILMethods ((topTypeDefs |> List.collect (fun td -> td.Methods.AsList))),
                    mkILFields ((topTypeDefs |> List.collect (fun td -> td.Fields.AsList))))
            let ilxMainModule = 
                { ilxMainModule with 
                    Manifest = (let m = ilxMainModule.ManifestOfAssembly in Some {m with CustomAttrs = mkILCustomAttrs m.CustomAttrs.AsList });
                    CustomAttrs = mkILCustomAttrs [ for m in moduls do yield! m.CustomAttrs.AsList ];
                    TypeDefs = mkILTypeDefs (topTypeDef :: List.concat normalTypeDefs);
                    Resources = mkILResources (savedResources @ ilxMainModule.Resources.AsList);
                    NativeResources = 
                       // NOTE: version resources from statically linked DLLs are dropped in the binary reader/writer
                       [ //yield! ilxMainModule.NativeResources 
                         for m in moduls do 
                             yield! m.NativeResources ] }
            ilxMainModule, rewriteExternalRefsToLocalRefs


    #if DEBUG
    let PrintModule outfile x = 
#if SILVERLIGHT
        ()
#else
        use os = File.CreateText(outfile) :> TextWriter
        ILAsciiWriter.output_module os x  
#endif
    #endif

    // Compute a static linker. This only captures tcImports (a large data structure) if
    // static linking is enabled. Normally this is not the case, which lets us collect tcImports
    // prior to this point.
    let StaticLink (tcConfig:TcConfig, tcImports:TcImports,ilGlobals:ILGlobals) = 

      let estAssemblies = []
      if tcConfig.compilingFslib && tcConfig.compilingFslib20.IsSome then 
          (fun (ilxMainModule,_) -> 
              let mscorlib40 = tcConfig.compilingFslib20.Value // + @"\..\.NET Framework 4.0 Pre Beta\mscorlib.dll"
              
              let ilBinaryReader = 
                  let opts = { ILBinaryReader.defaults with 
                                  ilGlobals=IL.mkILGlobals ILScopeRef.Local (Some tcConfig.mscorlibAssemblyName) tcConfig.noDebugData;
                                  optimizeForMemory=tcConfig.optimizeForMemory;
                                  pdbPath = None; } 
                  ILBinaryReader.OpenILModuleReader mscorlib40 opts
              
              let tdefs1 = ilxMainModule.TypeDefs.AsList  |> List.filter (fun td -> not (injectedCompatTypes.Contains(td.Name)))
              let tdefs2 = ilBinaryReader.ILModuleDef.TypeDefs.AsList |> List.filter (fun td -> injectedCompatTypes.Contains(td.Name))
              //printfn "tdefs2 = %A" (tdefs2 |> List.map (fun tdef -> tdef.Name))

              // rewrite the mscorlib references 
              let tdefs2 = 
                  let fakeModule = mkILSimpleModule "" "" true (mkILTypeDefs tdefs2) None None 0 (mkILExportedTypes []) ""
                  let fakeModule = 
                       fakeModule |> Morphs.morphILTypeRefsInILModuleMemoized (fun tref -> 
                           if injectedCompatTypes.Contains(tref.Name)  || (tref.Enclosing  |> List.exists (fun x -> injectedCompatTypes.Contains(x))) then 
                               tref
                               //|> Morphs.morphILScopeRefsInILTypeRef (function ILScopeRef.Local -> ilGlobals.mscorlibScopeRef | x -> x) 
                           // The implementations of Tuple use two private methods from System.Environment to get a resource string. Remap it
                           elif tref.Name = "System.Environment" then 
                               ILTypeRef.Create(ILScopeRef.Local, [], "Microsoft.FSharp.Core.PrivateEnvironment")  //|> Morphs.morphILScopeRefsInILTypeRef (function ILScopeRef.Local -> ilGlobals.mscorlibScopeRef | x -> x) 
                           else 
                               tref |> Morphs.morphILScopeRefsInILTypeRef (fun _ -> ilGlobals.mscorlibScopeRef) )
                  
                  // strip out System.Runtime.TargetedPatchingOptOutAttribute, which doesn't exist for 2.0
                  let fakeModule = 
                    {fakeModule with 
                      TypeDefs = 
                       mkILTypeDefs 
                           ([ for td in fakeModule.TypeDefs do 
                                    yield {td with 
                                            Methods =
                                             mkILMethods (List.map (fun (md:ILMethodDef) ->
                                                                  {md with                                                            
                                                                    CustomAttrs = 
                                                                     mkILCustomAttrs 
                                                                      (List.filter (fun (ilattr:ILAttribute) -> 
                                                                                     ilattr.Method.EnclosingType.TypeRef.FullName <> "System.Runtime.TargetedPatchingOptOutAttribute") 
                                                                                   td.CustomAttrs.AsList)}) 
                                                                (td.Methods.AsList))}])}
                  //ILAsciiWriter.output_module stdout fakeModule
                  fakeModule.TypeDefs.AsList
                  
              let ilxMainModule = 
                  { ilxMainModule with 
                      TypeDefs = mkILTypeDefs (tdefs1 @ tdefs2); }

              ilxMainModule)
          

      elif not tcConfig.standalone && tcConfig.extraStaticLinkRoots.IsEmpty && estAssemblies.IsEmpty then 
          (fun (ilxMainModule,_outfile) -> ilxMainModule)
      else 
          (fun (ilxMainModule,outfile)  ->
            ReportTime tcConfig "Find assembly references";

            let depModules = 
                if not tcConfig.standalone && tcConfig.extraStaticLinkRoots.IsEmpty then 
                    []
                else
                    // Recursively find all referenced modules and add them to a module graph 
                    let depModuleTable = HashMultiMap(0, HashIdentity.Structural)
                    let dummyEntry nm =
                        { refs = IL.emptyILRefs ;
                          name=nm;
                          ccu=None;
                          data=ilxMainModule; // any old module
                          edges = []; 
                          visited = true }
                    let assumedIndependentSet = set [ "mscorlib";  "System"; "System.Core"; "System.Xml"; "Microsoft.Build.Framework"; "Microsoft.Build.Utilities" ]      

                    begin 
                        let remaining = ref (computeILRefs ilxMainModule).AssemblyReferences
                        while nonNil !remaining do
                            let ilAssemRef = List.head !remaining
                            remaining := List.tail !remaining;
                            if assumedIndependentSet.Contains ilAssemRef.Name || (ilAssemRef.PublicKey = Some ecmaPublicKey) then 
                                depModuleTable.[ilAssemRef.Name] <- dummyEntry ilAssemRef.Name
                            else
                                if not (depModuleTable.ContainsKey ilAssemRef.Name) then 
                                    match tcImports.TryFindDllInfo(Range.rangeStartup,ilAssemRef.Name,lookupOnly=false) with 
                                    | Some dllInfo ->
                                        let ccu = 
                                            match tcImports.FindCcuFromAssemblyRef (Range.rangeStartup, ilAssemRef) with 
                                            | ResolvedCcu ccu -> Some ccu
                                            | UnresolvedCcu(_ccuName) -> None

                                        let modul = dllInfo.RawMetadata

                                        let refs = 
                                            if ilAssemRef.Name = GetFSharpCoreLibraryName() then 
                                                IL.emptyILRefs 
                                            elif not modul.IsILOnly then 
                                                warning(Error(FSComp.SR.fscIgnoringMixedWhenLinking ilAssemRef.Name,rangeStartup))
                                                IL.emptyILRefs 
                                            else
                                                { AssemblyReferences = dllInfo.ILAssemblyRefs; 
                                                  ModuleReferences = [] }

                                        depModuleTable.[ilAssemRef.Name] <- 
                                            { refs=refs;
                                              name=ilAssemRef.Name;
                                              ccu=ccu;
                                              data=modul; 
                                              edges = []; 
                                              visited = false };

                                        // Push the new work items
                                        remaining := refs.AssemblyReferences @ !remaining;

                                    | None -> 
                                        warning(Error(FSComp.SR.fscAssumeStaticLinkContainsNoDependencies(ilAssemRef.Name),rangeStartup)); 
                                        depModuleTable.[ilAssemRef.Name] <- dummyEntry ilAssemRef.Name
                        done;
                    end;

                    ReportTime tcConfig "Find dependencies";

                    // Add edges from modules to the modules that depend on them 
                    for (KeyValue(_,n)) in depModuleTable do 
                        for aref in n.refs.AssemblyReferences do
                            let n2 = depModuleTable.[aref.Name] 
                            n2.edges <- n :: n2.edges
                    
                    // Find everything that depends on FSharp.Core
                    let roots = 
                        [ if tcConfig.standalone && depModuleTable.ContainsKey (GetFSharpCoreLibraryName()) then 
                             yield depModuleTable.[GetFSharpCoreLibraryName()]
                          for n in tcConfig.extraStaticLinkRoots  do
                              match depModuleTable.TryFind n with 
                              | Some x -> yield x
                              | None -> error(Error(FSComp.SR.fscAssemblyNotFoundInDependencySet(n),rangeStartup)); 
                        ]
                              
                    let remaining = ref roots
                    [ while nonNil !remaining do
                        let n = List.head !remaining
                        remaining := List.tail !remaining;
                        if not (n.visited) then 
                            if verbose then dprintn ("Module "+n.name+" depends on "+GetFSharpCoreLibraryName());
                            n.visited <- true;
                            remaining := n.edges @ !remaining
                            yield (n.ccu, n.data);  ]

            let estModules = 
                [ // Add all EST-generated assemblies into the static linking set
                  for importedBinary in estAssemblies do 
                      let ilAssemRef  = importedBinary.ILScopeRef.AssemblyRef
                      printfn "adding EST-injected assembly '%s' into static linking set" ilAssemRef.Name
                      match tcImports.TryFindDllInfo(Range.rangeStartup,ilAssemRef.Name,lookupOnly=false) with 
                      | Some dllInfo ->
                          let ccu = 
                              match tcImports.FindCcuFromAssemblyRef (Range.rangeStartup, ilAssemRef) with 
                              | ResolvedCcu ccu -> Some ccu
                              | UnresolvedCcu(_ccuName) -> None

                          let modul = dllInfo.RawMetadata
                          yield ccu, modul  
                      | None -> () ]

            ReportTime tcConfig "Static link";
            (* Glue all this stuff into ilxMainModule *)
            let ilxMainModule,rewriteExternalRefsToLocalRefs = StaticLinkModules tcConfig ilGlobals ilxMainModule (depModules @ estModules)
               
            let ilxMainModule =
                let rewriteAssemblyRefsToMatchLibraries = NormalizeAssemblyRefs tcImports
                Morphs.morphILTypeRefsInILModuleMemoized (Morphs.morphILScopeRefsInILTypeRef (rewriteExternalRefsToLocalRefs >> rewriteAssemblyRefsToMatchLibraries)) ilxMainModule

             (* Print it out if requested *)
        #if DEBUG
            if tcConfig.writeGeneratedILFiles then (let _ = PrintModule (outpath outfile "ilx.main") ilxMainModule in ());
        #else
            ignore outfile
        #endif
            ilxMainModule)
  
//----------------------------------------------------------------------------
// EMIT IL
//----------------------------------------------------------------------------

type SigningInfo = SigningInfo of (* delaysign:*) bool * (*signer:*)  string option * (*container:*) string option

module FileWriter = 
    let EmitIL (tcConfig:TcConfig,ilGlobals,_errorLogger:ErrorLogger,outfile,pdbfile,ilxMainModule,signingInfo:SigningInfo) =
        let (SigningInfo(delaysign,signer,container)) = signingInfo
        try
    #if DEBUG
            if tcConfig.writeGeneratedILFiles then dprintn "Printing module...";
            if tcConfig.writeGeneratedILFiles  then StaticLinker.PrintModule (outpath outfile "il.txt") ilxMainModule; 
    #endif
            if !progress then dprintn "Writing assembly...";
            try 
                ILBinaryWriter.WriteILBinary 
                  outfile
                  {    mscorlib=ilGlobals.mscorlibScopeRef;
                       pdbfile=pdbfile;
                       emitTailcalls= tcConfig.emitTailcalls;
                       showTimes=tcConfig.showTimes;

                       signer = 
                         begin
                          if isSome container then
                            Some(ILBinaryWriter.ILStrongNameSigner.OpenKeyContainer container.Value)
                          else
                            match signer with 
                            | None -> None
                            | Some(s) ->
                               try 
                                if delaysign then
                                  Some (ILBinaryWriter.ILStrongNameSigner.OpenPublicKeyFile s) 
                                else
                                  Some (ILBinaryWriter.ILStrongNameSigner.OpenKeyPairFile s) 
                               with e -> 
                                   // Note:: don't use errorR here since we really want to fail and not produce a binary
                                   error(Error(FSComp.SR.fscKeyFileCouldNotBeOpened(s),rangeCmdArgs))
                         end;
                       fixupOverlappingSequencePoints = false; 
                       dumpDebugInfo =tcConfig.dumpDebugInfo } 
                  ilxMainModule
                  tcConfig.noDebugData
            with Failure msg -> 
                error(Error(FSComp.SR.fscProblemWritingBinary(outfile,msg), rangeCmdArgs))
        with e -> 
            errorRecoveryNoRange e; 
            exiter.Exit 1 

    let WriteStatsFile (tcConfig:TcConfig,outfile) = 
      if tcConfig.stats then 
          try 
              use oc = new  StreamWriter((outpath outfile "stats.txt"),append=false,encoding=Encoding.UTF8) :> TextWriter
#if STATISTICS
              Ilread.report oc;
#endif
              Ilxgen.ReportStatistics oc;
          with _ -> ()


let abortOnError (errorLogger:ErrorLogger) = 
    if errorLogger.ErrorCount > 0 then exiter.Exit 1 
    
let ValidateKeySigningAttributes (tcConfig : TcConfig) tcGlobals topAttrs =
    let delaySignAttrib = AttributeHelpers.TryFindBoolAttribute tcGlobals "System.Reflection.AssemblyDelaySignAttribute" topAttrs.assemblyAttrs
    let signerAttrib = AttributeHelpers.TryFindStringAttribute tcGlobals "System.Reflection.AssemblyKeyFileAttribute" topAttrs.assemblyAttrs
    let containerAttrib = AttributeHelpers.TryFindStringAttribute tcGlobals "System.Reflection.AssemblyKeyNameAttribute" topAttrs.assemblyAttrs
    
    // if delaySign is set via an attribute, validate that it wasn't set via an option
    let delaysign = 
        match delaySignAttrib with 
        | Some delaysign -> 
          if tcConfig.delaysign then
            warning(Error(FSComp.SR.fscDelaySignWarning(),rangeCmdArgs)) ;
            tcConfig.delaysign
          else
            delaysign
        | _ -> tcConfig.delaysign
        
         
    // if signer is set via an attribute, validate that it wasn't set via an option
    let signer = 
        match signerAttrib with
        | Some signer -> 
            if tcConfig.signer.IsSome && tcConfig.signer <> Some signer then
                warning(Error(FSComp.SR.fscKeyFileWarning(),rangeCmdArgs)) ;
                tcConfig.signer
            else
                Some signer
        | None -> tcConfig.signer
    
    // if container is set via an attribute, validate that it wasn't set via an option, and that they keyfile wasn't set
    // if keyfile was set, use that instead (silently)
    let container = 
        match containerAttrib with 
        | Some container -> 
            if tcConfig.container.IsSome && tcConfig.container <> Some container then
              warning(Error(FSComp.SR.fscKeyNameWarning(),rangeCmdArgs)) ;
              tcConfig.container
            else
              Some container
        | None -> tcConfig.container
    
    SigningInfo (delaysign,signer,container)
    

/// This error logger delays the messages it recieves. At the end, call ForwardDelayedErrorsAndWarnings
/// to send the held messages.     
type DelayAndForwardErrorLogger() =
   let delayed = new ResizeArray<_>()
   let errors = ref 0
   interface ErrorLogger with 
       member x.ErrorSink(e) = 
            errors := !errors + 1
            delayed.Add (e,true)
       member x.ErrorCount = delayed |> Seq.filter snd |> Seq.length
       member x.WarnSink(e) = delayed.Add(e,false)
   member x.ForwardDelayedErrorsAndWarnings(errorLogger:ErrorLogger) = 
       // Eagerly grab all the errors and warnings from the mutable collection
       let errors = delayed |> Seq.toList
       // Now report them
       for (e,isError) in errors do
           if isError then errorLogger.ErrorSink(e) else errorLogger.WarnSink(e)
       // Clear errors just reported. Keep errors count.
       delayed.Clear()
   member x.ForwardDelayedErrorsAndWarnings(tcConfigB:TcConfigBuilder,errorLoggerOpt) = 
       let errorLogger = match errorLoggerOpt with None -> ErrorLoggerInitial(tcConfigB) | Some e -> e
       x.ForwardDelayedErrorsAndWarnings(errorLogger)
   member x.ErrorCount = !errors           

/// Check for .fsx and, if present, compute the load closure for of #loaded files.
let AdjustForScriptCompile(tcConfigB:TcConfigBuilder,commandLineSourceFiles,lexResourceManager) =

    let commandLineSourceFiles = 
        commandLineSourceFiles 
        |> List.map (fun file -> if Path.IsPathRooted(file) then file else Path.Combine(tcConfigB.implicitIncludeDir, file))
        
    let allSources = ref []       
    
    let tcConfig = TcConfig.Create(tcConfigB,validate=false) 
    
    let AddIfNotPresent(filename:string) =
        if not(!allSources |> List.mem filename) then
            allSources := filename::!allSources
    
    let AppendClosureInformation(filename) =
        if IsScript filename then 
            let closure = LoadClosure.FindFromFiles(tcConfig,[filename,rangeStartup],(*editting*)false,DefineCompile,(*useDefaultScriptingReferences*)false,lexResourceManager)
            let references = closure.References |> List.map snd |> List.concat |> List.map (fun r->r.originalReference) |> List.filter (fun r->r.Range<>range0)
            references |> List.iter (fun r-> tcConfigB.AddReferencedAssemblyByPath(r.Range,r.Text))
            closure.NoWarns |> List.map(fun (n,ms)->ms|>List.map(fun m->m,n)) |> List.concat |> List.iter tcConfigB.TurnWarningOff
            closure.SourceFiles |> List.map fst |> List.iter AddIfNotPresent
            closure.RootWarnings |> List.iter warnSink
            closure.RootErrors |> List.iter errorSink
            
         else AddIfNotPresent(filename)
         
    // Find closure of .fsx files.
    commandLineSourceFiles |> List.iter AppendClosureInformation

    List.rev !allSources

//----------------------------------------------------------------------------
// main - split up to make sure that we can GC the
// dead data at the end of each phase.  We explicitly communicate arguments
// from one phase to the next.
//-----------------------------------------------------------------------------
  
[<NoEquality; NoComparison>]
type Args<'a> = Args  of 'a

let main1(argv,bannerAlreadyPrinted,errorLoggerOpt) =

    let lcidFromCodePage = 
#if SILVERLIGHT
        None
#else
    // See Bug 735819 
        if (System.Console.OutputEncoding.CodePage <> 65001) &&
           (System.Console.OutputEncoding.CodePage <> System.Threading.Thread.CurrentThread.CurrentUICulture.TextInfo.OEMCodePage) &&
           (System.Console.OutputEncoding.CodePage <> System.Threading.Thread.CurrentThread.CurrentUICulture.TextInfo.ANSICodePage) then
                System.Threading.Thread.CurrentThread.CurrentUICulture <- new System.Globalization.CultureInfo("en-US")
                Some(1033)
        else
            None
#endif

#if SILVERLIGHT
    let tcConfigB = Build.TcConfigBuilder.CreateNew(defaultFSharpBinariesDir, false, ".")
#else
    let tcConfigB = Build.TcConfigBuilder.CreateNew(defaultFSharpBinariesDir, false, Directory.GetCurrentDirectory())
#endif
    // Preset: --optimize+ -g --tailcalls+ (see 4505)
    SetOptimizeSwitch tcConfigB On
    SetDebugSwitch    tcConfigB None Off
    SetTailcallSwitch tcConfigB On    

    // Now install a delayed logger to hold all errors from flags until after all flags have been parsed (for example, --vserrors)
    let delayForFlagsLogger = DelayAndForwardErrorLogger()
    let _unwindEL_1 = PushErrorLoggerPhaseUntilUnwind (fun _ -> delayForFlagsLogger)          
    
    // Share intern'd strings across all lexing/parsing
    let lexResourceManager = new Lexhelp.LexResourceManager()

    // process command line, flags and collect filenames 
    let sourceFiles = 

      // The ParseCompilerOptions function calls imperative function to process "real" args
      // Rather than start processing, just collect names, then process them. 
      try 
          let inputFilesRef   = ref ([] : string list)
          let collect name = 
              let lower = String.lowercase name
              if List.exists (Filename.checkSuffix lower) [".resx"]  then
                  tcConfigB.AddEmbeddedResource name
              else
                  inputFilesRef := name :: !inputFilesRef
          let abbrevArgs = abbrevFlagSet tcConfigB true
    
          // This is where flags are interpretted by the command line fsc.exee.
          ParseCompilerOptions collect (GetCoreFscCompilerOptions tcConfigB) (List.tail (PostProcessCompilerArgs abbrevArgs argv));
          let inputFiles = List.rev !inputFilesRef

#if SILVERLIGHT
#else
          // Check if we have a codepage from the console
          match tcConfigB.lcid with
          | Some _ -> ()
          | None -> tcConfigB.lcid <- lcidFromCodePage

          match tcConfigB.lcid with
          | Some(n) -> System.Threading.Thread.CurrentThread.CurrentUICulture <- new System.Globalization.CultureInfo(n)
          | None -> ()
          
          if tcConfigB.utf8output then 
              let prev = System.Console.OutputEncoding
              System.Console.OutputEncoding <- Encoding.UTF8
              System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> System.Console.OutputEncoding <- prev)
#endif

          (* step - get dll references *)
          let dllFiles,sourceFiles = List.partition Filename.isDll inputFiles
          match dllFiles with
          | [] -> ()
          | h::_ -> errorR (Error(FSComp.SR.fscReferenceOnCommandLine(h),rangeStartup)) 

          dllFiles |> List.iter (fun f->tcConfigB.AddReferencedAssemblyByPath(rangeStartup,f))
          
          let sourceFiles = AdjustForScriptCompile(tcConfigB,sourceFiles,lexResourceManager)                     
          sourceFiles

      with 
          e -> 
            errorRecovery e rangeStartup
            delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB, errorLoggerOpt)
            exiter.Exit 1 
      
    tcConfigB.conditionalCompilationDefines <- "COMPILED" :: tcConfigB.conditionalCompilationDefines 
    // display the banner text, if necessary
    if not bannerAlreadyPrinted then 
        Microsoft.FSharp.Compiler.Fscopts.DisplayBannerText tcConfigB

    // Create tcGlobals and frameworkTcImports
    let outfile,pdbfile,assemblyName = 
        try 
            tcConfigB.DecideNames sourceFiles 
        with e ->
            errorRecovery e rangeStartup
            delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB, errorLoggerOpt)
            exiter.Exit 1 
                    
    // DecideNames may give "no inputs" error. Abort on error at this point. bug://3911
    if delayForFlagsLogger.ErrorCount > 0 then 
        delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB, errorLoggerOpt)
        exiter.Exit 1
    
    // If there's a problem building TcConfig, abort    
    let tcConfig = 
        try
            TcConfig.Create(tcConfigB,validate=false)
        with e ->
            delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(tcConfigB, errorLoggerOpt)
            exiter.Exit 1
    
    let errorLogger = match errorLoggerOpt with None -> ErrorLoggerThatQuitsAfterMaxErrors tcConfigB | Some e -> e

    // Install the global error logger and never remove it. This logger does have all command-line flags considered.
    let _unwindEL_2 = PushErrorLoggerPhaseUntilUnwind (fun _ -> errorLogger)
    
    // Forward all errors from flags
    delayForFlagsLogger.ForwardDelayedErrorsAndWarnings(errorLogger)

    (* step - decideNames *)  
    abortOnError errorLogger;

    // Nice name generator    
    let niceNameGen = NiceNameGenerator()
    
    let tcGlobals,tcImports,frameworkTcImports,generatedCcu,typedAssembly,topAttrs,tcConfig = 
    
        ReportTime tcConfig "Import mscorlib";
//        let tcConfigP = TcConfigProvider.Constant(tcConfig)

        if tcConfig.useIncrementalBuilder then 
            ReportTime tcConfig "Incremental Parse and Typecheck";
            let projectDirectory = Directory.GetCurrentDirectory() 
            let build,_ = 
                IncrementalFSharpBuild.Create (tcConfig, projectDirectory, assemblyName, niceNameGen, lexResourceManager, sourceFiles, 
                                               false, // no need to stay reactive
                                               IncrementalFSharpBuild.BuildEvents.Default,
                                               errorLogger,
                                               errorRecovery)
            let _build,tcState,topAttribs,typedAssembly,_tcEnv,tcImports,tcGlobals,tcConfig = IncrementalFSharpBuild.TypeCheck(build) 
            tcGlobals,tcImports,tcImports,tcState.Ccu,typedAssembly,topAttribs,tcConfig
        else
        
            ReportTime tcConfig "Import mscorlib and FSharp.Core.dll";
            ReportTime tcConfig "Import system references";
            let foundationalTcConfigP = TcConfigProvider.Constant(tcConfig)
            let sysRes,otherRes,_ = TcAssemblyResolutions.SplitNonFoundationalResolutions tcConfig
            let tcGlobals,frameworkTcImports = TcImports.BuildFrameworkTcImports (foundationalTcConfigP,sysRes,otherRes)

            (* step - parse sourceFiles *)
            ReportTime tcConfig "Parse inputs";
            use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parse)            
            let inputs =
                try  
                   sourceFiles 
                   |> tcConfig.ComputeCanContainEntryPoint 
                   |> List.zip sourceFiles
                   |> List.choose (fun (input,isLastCompiland) -> 
//                         let lower = String.lowercase input
                         ParseOneInputFile(tcConfig,lexResourceManager,["COMPILED"],input,isLastCompiland,errorLogger,(*retryLocked*)false)) 
                with e -> 
                    errorRecoveryNoRange e; exiter.Exit 1
            if tcConfig.parseOnly then exiter.Exit 0 else ();
            abortOnError errorLogger;

            if tcConfig.printAst then                
                inputs |> List.iter (fun input -> printf "AST:\n"; printfn "%+A" input; printf "\n") 

            let tcConfig = (tcConfig,inputs) ||> List.fold ApplyMetaCommandsFromInputToTcConfig 
            let tcConfigP = TcConfigProvider.Constant(tcConfig)

            ReportTime tcConfig "Import non-system references";
            let tcGlobals,tcImports =  
                let tcImports = TcImports.BuildNonFrameworkTcImports(tcConfigP,tcGlobals,frameworkTcImports,otherRes)
                tcGlobals,tcImports
            abortOnError errorLogger;

            ReportTime tcConfig "Typecheck";
            use unwindParsePhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.TypeCheck)            
            let tcEnv0 = GetInitialTypecheckerEnv (Some assemblyName) rangeStartup tcConfig tcImports tcGlobals

            // typecheck 
            let tcState,topAttrs,typedAssembly,_tcEnvAtEnd = 
                TypeCheck(tcConfig,tcImports,tcGlobals,errorLogger,assemblyName,niceNameGen,tcEnv0,inputs)

            let generatedCcu = tcState.Ccu
            abortOnError errorLogger;
            ReportTime tcConfig "Typechecked";

            (tcGlobals,tcImports,frameworkTcImports,generatedCcu,typedAssembly,topAttrs,tcConfig)
                    
    if tcConfig.typeCheckOnly then exiter.Exit 0;
    
    
    use unwindPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.CodeGen)
    let signingInfo = ValidateKeySigningAttributes tcConfig tcGlobals topAttrs
    
    abortOnError errorLogger;

    // Build an updated errorLogger that filters according to the scopedPragmas. Then install
    // it as the updated global error logger and never remove it
    let oldLogger = errorLogger
    let errorLogger = 
        let scopedPragmas = 
            let (TAssembly(impls)) = typedAssembly 
            [ for (TImplFile(_,pragmas,_,_,_)) in impls do yield! pragmas ]
        GetErrorLoggerFilteringByScopedPragmas(true,scopedPragmas,oldLogger)

    let _unwindEL_3 = PushErrorLoggerPhaseUntilUnwind(fun _ -> errorLogger)

    // Try to find an AssemblyVersion attribute 
    let assemVerFromAttrib = 
        match AttributeHelpers.TryFindVersionAttribute tcGlobals "System.Reflection.AssemblyVersionAttribute" topAttrs.assemblyAttrs with
        | Some v -> 
           match tcConfig.version with 
           | VersionNone -> Some v
           | _ -> warning(Error(FSComp.SR.fscAssemblyVersionAttributeIgnored(),Range.rangeStartup)); None
        | _ -> None

    // write interface, xmldoc
    begin
      ReportTime tcConfig ("Write Interface File");
      use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Output)    
      if tcConfig.printSignature   then InterfaceFileWriter.WriteInterfaceFile (tcGlobals,tcConfig,typedAssembly);
      ReportTime tcConfig ("Write XML document signatures")
      if tcConfig.xmlDocOutputFile.IsSome then 
          XmlDocWriter.computeXmlDocSigs (tcGlobals,generatedCcu) 
      ReportTime tcConfig ("Write XML docs");
      tcConfig.xmlDocOutputFile |> Option.iter (fun xmlFile -> XmlDocWriter.writeXmlDoc (assemblyName,generatedCcu,xmlFile))
      ReportTime tcConfig ("Write HTML docs");
    end;


    // Pass on only the minimimum information required for the next phase to ensure GC kicks in.
    // In principle the JIT should be able to do good liveness analysis to clean things up, but the
    // data structures involved here are so large we can't take the risk.
    Args(tcConfig,tcImports,frameworkTcImports,tcGlobals,errorLogger,generatedCcu,outfile,typedAssembly,topAttrs,pdbfile,assemblyName,assemVerFromAttrib,signingInfo)

  
let main2(Args(tcConfig,tcImports,frameworkTcImports : TcImports,tcGlobals,errorLogger,generatedCcu:CcuThunk,outfile,typedAssembly,topAttrs,pdbfile,assemblyName,assemVerFromAttrib,signingInfo)) = 
      
    ReportTime tcConfig ("Encode Interface Data");
#if DEBUG
    if !verboseStamps then 
        dprintf "---------------------- START MAKE EXPORT REMAPPING ------------\n";
#endif
    let exportRemapping = MakeExportRemapping generatedCcu generatedCcu.Contents
#if DEBUG
    if !verboseStamps then 
        dprintf "---------------------- END MAKE EXPORT REMAPPING ------------\n";
#endif
    
    let sigDataAttributes,sigDataResources = 
        EncodeInterfaceData(tcConfig,tcGlobals,exportRemapping,generatedCcu,outfile)
        
    if !progress && tcConfig.optSettings.jitOptUser = Some false then 
        dprintf "Note, optimizations are off.\n";
    (* optimize *)
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Optimize)
    
    let optEnv0 = InitialOptimizationEnv tcImports
   
    let importMap = tcImports.GetImportMap()
    let metadataVersion = 
        match tcConfig.metadataVersion with
        | Some(v) -> v
        | _ -> match (frameworkTcImports.DllTable.TryFind tcConfig.mscorlibAssemblyName) with | Some(ib) -> ib.RawMetadata.MetadataVersion | _ -> ""
    let optimizedImpls,optimizationData,_ = ApplyAllOptimizations (tcConfig, tcGlobals, outfile, importMap, false, optEnv0, generatedCcu, typedAssembly)

    abortOnError errorLogger;
        
    ReportTime tcConfig ("Encoding OptData");
    let generatedOptData = EncodeOptimizationData(tcGlobals,tcConfig,outfile,exportRemapping,(generatedCcu,optimizationData))

    let sigDataResources, _optimizationData = 
        if tcConfig.useSignatureDataFile then 
            let bytes = [| yield! BinaryGenerationUtilities.i32 0x7846ce27
                           yield! BinaryGenerationUtilities.i32 (sigDataResources.Length + generatedOptData.Length)
                           for r in (sigDataResources @ generatedOptData) do 
                               match r.Location with 
                               |  ILResourceLocation.Local f -> 
                                   let bytes = f() 
                                   yield! BinaryGenerationUtilities.i32 bytes.Length
                                   yield! bytes
                               | _ -> 
                                   failwith "unreachable: expected a local resource" |]
            let sigDataFileName = (Filename.chopExtension outfile)+".fsdata"
            File.WriteAllBytes(sigDataFileName,bytes)
            [], []
        else
            sigDataResources, generatedOptData
    
    // Pass on only the minimimum information required for the next phase to ensure GC kicks in.
    // In principle the JIT should be able to do good liveness analysis to clean things up, but the
    // data structures involved here are so large we can't take the risk.
    Args(tcConfig,tcImports,tcGlobals,errorLogger,generatedCcu,outfile,optimizedImpls,topAttrs,importMap,pdbfile,assemblyName, (sigDataAttributes, sigDataResources), generatedOptData,assemVerFromAttrib,signingInfo,metadataVersion)

let mutable tcImportsCapture = None
let mutable dynamicAssemblyCreator = None
let main2b(Args(tcConfig:TcConfig,tcImports,tcGlobals,errorLogger,generatedCcu:CcuThunk,outfile,optimizedImpls,topAttrs,importMap,pdbfile,assemblyName,idata,generatedOptData,assemVerFromAttrib,signingInfo,metadataVersion)) = 
  
    match tcImportsCapture with 
    | None -> ()
    | Some f -> f tcImports
    // Compute a static linker. 
    let ilGlobals = tcGlobals.ilg
    if tcConfig.standalone && generatedCcu.UsesQuotations then 
        error(Error(FSComp.SR.fscQuotationLiteralsStaticLinking0(),rangeStartup));
    let staticLinker = StaticLinker.StaticLink (tcConfig,tcImports,ilGlobals)

    ReportTime tcConfig "TAST -> ILX";
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind  (BuildPhase.IlxGen)
    let ilxGenEnv = IlxgenEnvInit (tcConfig,tcImports,tcGlobals,generatedCcu)

    let codegenResults = 
        match dynamicAssemblyCreator with
        | None -> GenerateIlxCode (IlWriteBackend, false, false, false, tcGlobals, tcConfig,importMap, topAttrs, optimizedImpls, generatedCcu, generatedCcu.AssemblyName, ilxGenEnv)
        | Some _ -> GenerateIlxCode (IlReflectBackend, true, false, runningOnMono, tcGlobals, tcConfig,importMap, topAttrs, optimizedImpls, generatedCcu, generatedCcu.AssemblyName, ilxGenEnv)
    let securityAttrs,topAssemblyAttrs = topAttrs.assemblyAttrs |> List.partition (fun a -> Ilxgen.IsSecurityAttribute tcGlobals (tcImports.GetImportMap()) a rangeStartup)
    // remove any security attributes from the top-level assembly attribute list
    let topAttrs = {topAttrs with assemblyAttrs=topAssemblyAttrs}
    let permissionSets = Ilxgen.CreatePermissionSets tcGlobals (tcImports.GetImportMap()) ilxGenEnv securityAttrs
    let secDecls = if securityAttrs.Length > 0 then (mkILSecurityDecls permissionSets) else (emptyILSecurityDecls)


    let ilxMainModule = MainModuleBuilder.CreateMainModule (tcConfig,tcGlobals,pdbfile,assemblyName,outfile,topAttrs,idata,generatedOptData,codegenResults,assemVerFromAttrib,metadataVersion,secDecls)
#if DEBUG
    // Print code before bailing out from the compiler due to errors 
    // in the backend of the compiler.  The partially-generated 
    // ILX code often contains useful information. 
    if tcConfig.writeGeneratedILFiles then StaticLinker.PrintModule (outpath outfile "ilx.txt") ilxMainModule;
#endif

    abortOnError errorLogger;
    
    Args (tcConfig,errorLogger,staticLinker,ilGlobals,outfile,pdbfile,ilxMainModule,signingInfo)

let main2c(Args(tcConfig,errorLogger,staticLinker,ilGlobals,outfile,pdbfile,ilxMainModule,signingInfo)) = 
      
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.IlGen)
    
    ReportTime tcConfig "ILX -> IL (Unions)"; 
    let ilxMainModule = EraseIlxUnions.ConvModule ilGlobals ilxMainModule
    ReportTime tcConfig "ILX -> IL (Funcs)"; 
    let ilxMainModule = EraseIlxFuncs.ConvModule ilGlobals ilxMainModule 

    abortOnError errorLogger;
    Args(tcConfig,errorLogger,staticLinker,ilGlobals,ilxMainModule,outfile,pdbfile,signingInfo)
  

let main3(Args(tcConfig,errorLogger:ErrorLogger,staticLinker,ilGlobals,ilxMainModule,outfile,pdbfile,signingInfo)) = 
        
    let ilxMainModule =  
        try  staticLinker (ilxMainModule,outfile)
        with e -> errorRecoveryNoRange e; exiter.Exit 1
    abortOnError errorLogger;
        
    Args (tcConfig,errorLogger,ilGlobals,ilxMainModule,outfile,pdbfile,signingInfo)

let main4(Args(tcConfig,errorLogger,ilGlobals,ilxMainModule,outfile,pdbfile,signingInfo)) = 
    ReportTime tcConfig "Write .NET Binary";
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Output)    
    let pdbfile = pdbfile |> Option.map Path.GetFullPathShim

    match dynamicAssemblyCreator with 
    | None -> FileWriter.EmitIL (tcConfig,ilGlobals,errorLogger,outfile,pdbfile,ilxMainModule,signingInfo); 
    | Some da -> da (tcConfig,ilGlobals,errorLogger,outfile,pdbfile,ilxMainModule,signingInfo); 

    ReportTime tcConfig "Write Stats File";
    FileWriter.WriteStatsFile (tcConfig,outfile);

    abortOnError errorLogger;
    ReportTime tcConfig "Exiting"


let mainCompile (argv,bannerAlreadyPrinted,errorLoggerOpt) = main1 (argv,bannerAlreadyPrinted,errorLoggerOpt) |> main2 |> main2b |> main2c |> main3 |> main4

/// Collect the output from the stdout and stderr streams, character by character,
/// recording the console color used along the way.
type OutputCollector() = 
    let output = ResizeArray()
    let outWriter isOut = 
        { new TextWriter() with 
              member x.Write(c:char) = 
                  lock output (fun () -> 
#if SILVERLIGHT
                      output.Add (isOut, None ,c)) 
#else
                      output.Add (isOut, (try Some System.Console.ForegroundColor with _ -> None) ,c)) 
#endif
              member x.Encoding = Encoding.UTF8 }
#if FX_ATLEAST_SILVERLIGHT_50
#else
    do System.Console.SetOut (outWriter true)
    do System.Console.SetError (outWriter false)
#endif
    member x.GetTextAndClear() = lock output (fun () -> let res = output.ToArray() in output.Clear(); res)

#if SILVERLIGHT
#else
/// Implement the optional resident compilation service
module FSharpResidentCompiler = 

    open System
    open System.Diagnostics
    open System.Runtime.Remoting.Channels
    open System.Runtime.Remoting
    open System.Runtime.Remoting.Lifetime

    /// The compilation server, which runs in the server process. Accessed by clients using .NET remoting.
    type FSharpCompilationServer()  =
        inherit MarshalByRefObject()  

        static let onWindows = 
            match System.Environment.OSVersion.Platform with 
            | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE -> true
            | _  -> false
        // The channel/socket name is qualified by the user name (and domain on windows) plus a hash of information
        // about the CLR, F# and F# compiler versions. We use different base channel names on mono and CLR as a
        // CLR remoting process can't talk to a mono server
        static let runtimeHash = hash (runningOnMono, 
                                       typedefof<obj>.Assembly.Location, 
                                       typedefof<list<_>>.Assembly.Location,
                                       typedefof<FSharpCompilationServer>.Assembly.Location)
        // The channel/socket name is qualified by the user name (and domain on windows)
        static let domainName = if onWindows then Environment.GetEnvironmentVariable "USERDOMAIN" else ""
        static let userName = Environment.GetEnvironmentVariable (if onWindows then "USERNAME" else "USER") 
        static let baseChannelName = "FSCChannel" + string runtimeHash
        static let channelName = baseChannelName + "_" +  domainName + "_" + userName
        static let serverName = if runningOnMono then "FSCServerMono" else "FSCSever"
        static let mutable serverExists = true
        
        let outputCollector = new OutputCollector()
        // This lock ensures all compilation requests sent to the server are serialized.
        // Due to a Mono 2.8 bug, we can't use an agent to serialize requests at this 
        // point. We must run the compilation on the request thread. 
        let syncRoot = obj()
        let outstandingRequests = ref 0
        let runRequest (pwd,argv) = 
            lock syncRoot (fun () -> 
                if !progress then printfn "server agent: got compilation request, argv = %A" argv
                let exitCode = 
                    try 
                        Environment.CurrentDirectory <- pwd
                        mainCompile (argv, true, None); 
                        if !progress then printfn "server: finished compilation request, argv = %A" argv
                        0
                    with e -> 
                        if !progress then printfn "server: finished compilation request with errors, argv = %A" argv
                        stopProcessingRecovery e range0
                        1
                let output = outputCollector.GetTextAndClear()
                GC.Collect(3)
                // Exit the server if there are no known outstanding requests and the 
                // current memory usage after collection is over 200MB
                if lock outstandingRequests (fun () -> !outstandingRequests) = 0 && GC.GetTotalMemory(true) > 200L * 1024L * 1024L then
                    Environment.Exit 0
                (output, exitCode))

        member x.Run() = 
            while serverExists do 
               if !progress then printfn "server: startup thread sleeping..." 
               System.Threading.Thread.Sleep 1000

        abstract Ping : unit -> string
        abstract Compile : string * string[] -> (bool * System.ConsoleColor option * char) [] * int
        default x.Ping() = "ping"
        default x.Compile (pwd,argv) = 
            if !progress then printfn "server: got compilation request, (pwd, argv) = %A" (pwd, argv)
            lock outstandingRequests (fun () -> outstandingRequests := !outstandingRequests + 1)
            let res = runRequest(pwd,argv)
            lock outstandingRequests (fun () -> outstandingRequests := !outstandingRequests - 1)
            res
            

        override x.Finalize() =
            serverExists <- false

        // This is called on the server object by .NET remoting to initialize the lifetime characteristics
        // of the server object.
        override x.InitializeLifetimeService() =
            let lease = (base.InitializeLifetimeService() :?> ILease)
            if (lease.CurrentState = LeaseState.Initial)  then
                lease.InitialLeaseTime <- TimeSpan.FromDays(1.0);
                lease.SponsorshipTimeout <- TimeSpan.FromMinutes(2.0);
                lease.RenewOnCallTime <- TimeSpan.FromDays(1.0);
            box lease
            
        static member RunServer() =
            progress := !progress || (System.Environment.GetEnvironmentVariable "FSHARP_SERVER_PROGRESS" <> null)
            if !progress then printfn "server: initializing server object" 
            let server = new FSharpCompilationServer()
            let chan = new Ipc.IpcChannel(channelName) 
            ChannelServices.RegisterChannel(chan,false);
            RemotingServices.Marshal(server,serverName)  |> ignore

            // On Unix, the file permissions of the implicit socket need to be set correctly to make this
            // private to the user.
            if runningOnMono then 
              try 
                  let monoPosix = System.Reflection.Assembly.Load("Mono.Posix, Version=2.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756")
                  let monoUnixFileInfo = monoPosix.GetType("Mono.Unix.UnixFileSystemInfo") 
                  let socketName = Path.Combine(Path.GetTempPath(), channelName)
                  let fileEntry = monoUnixFileInfo.InvokeMember("GetFileSystemEntry", (BindingFlags.InvokeMethod ||| BindingFlags.Static ||| BindingFlags.Public), null, null, [| box socketName |],System.Globalization.CultureInfo.InvariantCulture)
                  // Add 0x00000180 (UserReadWriteExecute) to the access permissions on Unix
                  monoUnixFileInfo.InvokeMember("set_FileAccessPermissions", (BindingFlags.InvokeMethod ||| BindingFlags.Instance ||| BindingFlags.Public), null, fileEntry, [| box 0x00000180 |],System.Globalization.CultureInfo.InvariantCulture) |> ignore
#if DEBUG
                  if !progress then printfn "server: good, set permissions on socket name '%s'"  socketName
                  let fileEntry = monoUnixFileInfo.InvokeMember("GetFileSystemEntry", (BindingFlags.InvokeMethod ||| BindingFlags.Static ||| BindingFlags.Public), null, null, [| box socketName |],System.Globalization.CultureInfo.InvariantCulture)
                  let currPermissions = monoUnixFileInfo.InvokeMember("get_FileAccessPermissions", (BindingFlags.InvokeMethod ||| BindingFlags.Instance ||| BindingFlags.Public), null, fileEntry, [| |],System.Globalization.CultureInfo.InvariantCulture) |> unbox<int>
                  if !progress then printfn "server: currPermissions = '%o' (octal)"  currPermissions
#endif
              with e -> 
#if DEBUG
                  if !progress then printfn "server: failed to set permissions on socket, perhaps on windows? Is is not needed there."  
#endif
                  ()
                  // Fail silently
            // chmod
            server.Run()
            
        static member private ConnectToServer() =
            Activator.GetObject(typeof<FSharpCompilationServer>,"ipc://" + channelName + "/" + serverName) 
            :?> FSharpCompilationServer 

        static member TryCompileUsingServer(argv) =
            let pwd = System.Environment.CurrentDirectory
            let clientOpt = 
                // Detect the absence of the channel via the exception. Probably not the best way.
                // Different exceptions get thrown here on Mono and Windows.
                let client = FSharpCompilationServer.ConnectToServer()
                try 
                    progress := !progress || (System.Environment.GetEnvironmentVariable "FSHARP_CLIENT_PROGRESS" <> null)
                    if !progress then printfn "client: attempting to connect to existing service (1)"
                    client.Ping() |> ignore
                    if !progress then printfn "client: connected to existing service"
                    Some client
                with _ ->
                    let shellName = 
                        if runningOnMono then
                        // e.g. "C:\Program Files\Mono-2.6.1\lib\mono\2.0\mscorlib.dll" --> "C:\Program Files\Mono-2.6.1\bin\mono.exe"
                        // e.g. /opt/mono-2.10/lib/mono/2.0/mscorlib.dll --> /opt/mono-2.10/bin/mono
                            Path.Combine(Path.Combine(Path.Combine(Path.Combine(Path.Combine(Path.GetDirectoryName (typeof<Object>.Assembly.Location), ".."), ".."), ".."), "bin"), (if onWindows then "mono.exe" else "mono"))
                        else
                            typeof<FSharpCompilationServer>.Assembly.Location
                    let args = 
                        if runningOnMono then 
                            typeof<FSharpCompilationServer>.Assembly.Location + " /server"
                                     
                        else 
                            "/server"

                    let procInfo = 
                        ProcessStartInfo(FileName = shellName,
                                         Arguments = args,
                                         CreateNoWindow = true,
                                         UseShellExecute = not onWindows)

                    let cmdProcess = new Process(StartInfo=procInfo)

                    //let exitE = cmdProcess.Exited |> Observable.map (fun x -> x)

                    cmdProcess.Start() |> ignore
                    //exitE.Add(fun _ -> if !progress then eprintfn "client: the server has exited")
                    cmdProcess.EnableRaisingEvents <- true;
                     
                    // Create the client proxy and attempt to connect to the server
                    let rec tryAcccesServer nRemaining =
                        if nRemaining = 0 then 
                            // Failed to connect to server, give up 
                            None
                        else
                            try 
                                if !progress then printfn "client: attempting to connect to existing service (2)"
                                client.Ping() |> ignore
                                if !progress then printfn "client: connected to existing service"
                                Some client
                            // Detect the absence of the channel via the exception. Probably not the best way.
                            // Different exceptions get thrown here on Mono and Windows.
                            with _ (* System.Runtime.Remoting.RemotingException *) ->
                                // Sleep a bit
                                System.Threading.Thread.Sleep 50
                                tryAcccesServer (nRemaining - 1)

                    tryAcccesServer 20

            match clientOpt with
            | Some client -> 
                if !progress then printfn "client: calling client.Compile(%A)" argv
                // Install the global error logger and never remove it. This logger does have all command-line flags considered.
                try 
                    let (output, exitCode) = 
                        try client.Compile (pwd, argv) 
                        with e -> 
                           printfn "server error: %s" (e.ToString())
                           raise (Error (FSComp.SR.fscRemotingError(), rangeStartup))
                        
                    if !progress then printfn "client: returned from client.Compile(%A), res = %d" argv exitCode
                    use holder = 
                        try let originalConsoleColor = Console.ForegroundColor 
                            { new System.IDisposable with member x.Dispose() = Console.ForegroundColor <- originalConsoleColor }
                        with _ -> null
                    let mutable prevConsoleColor = try Console.ForegroundColor with _ -> ConsoleColor.Black
                    for (isOut, consoleColorOpt, c:char) in output do 
                        try match consoleColorOpt with 
                             | Some consoleColor -> 
                                 if prevConsoleColor <> consoleColor then 
                                     Console.ForegroundColor <- consoleColor; 
                             | None -> ()
                        with _ -> ()
                        c |> (if isOut then System.Console.Out.Write else System.Console.Error.Write)
                    Some exitCode
                with err -> 
                   let sb = System.Text.StringBuilder()
                   OutputErrorOrWarning (pwd,true,false,ErrorStyle.DefaultErrors,true) sb (PhasedError.Create(err,BuildPhase.Compile))
                   eprintfn "%s" (sb.ToString())
                   // We continue on and compile in-process - the server appears to have died half way through.
                   None
            | None -> 
                None

let main argv = 
    // Check for --pause as the very first step so that a compiler can be attached here.
    if argv |> Array.exists  (fun x -> x = "/pause" || x = "--pause") then 
        System.Console.WriteLine("Press any key to continue...")
        System.Console.ReadKey() |> ignore
    if runningOnMono && argv |> Array.exists  (fun x -> x = "/resident" || x = "--resident") then 
        let argv = argv |> Array.filter (fun x -> x <> "/resident" && x <> "--resident")

        if not (argv |> Array.exists (fun x -> x = "/nologo" || x = "--nologo")) then 
            printfn "%s" (FSComp.SR.buildProductName(FSharpEnvironment.FSharpTeamVersionNumber))
            printfn "%s" (FSComp.SR.optsCopyright())

        let exitCodeOpt = FSharpResidentCompiler.FSharpCompilationServer.TryCompileUsingServer argv
        match exitCodeOpt with 
        | Some exitCode -> exitCode
        | None -> 
            mainCompile (argv, true, None)
            0

    elif runningOnMono && argv |> Array.exists  (fun x -> x = "/server" || x = "--server") then 
        // Install the right exiter so we can catch "StopProcessing" without exiting the server
        exiter <- { new Exiter with member x.Exit n = raise StopProcessing }
        FSharpResidentCompiler.FSharpCompilationServer.RunServer()        
        0
        
    else
        mainCompile (argv, false, None);
        0 
#endif
