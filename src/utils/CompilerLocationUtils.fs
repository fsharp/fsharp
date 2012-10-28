namespace Internal.Utilities
open System
open System.IO
open System.Configuration
open System.Reflection
open Microsoft.Win32
open System.Runtime.InteropServices

#nowarn "44" // ConfigurationSettings is obsolete but the new stuff is horribly complicated. 

module internal FSharpEnvironment =

    /// The F# version reported in the banner
#if NO_STRONG_NAMES
    let DotNetBuildString = "(private)"
#endif
#if STRONG_NAME_AND_DELAY_SIGN_FSHARP_COMPILER_WITH_MSFT_KEY
    let DotNetBuildString = "(Open Source Edition)"
#endif
#if STRONG_NAME_FSHARP_COMPILER_WITH_TEST_KEY
    let DotNetBuildString = "(private, test-signed)"
#endif

    // The F# binary format revision number. The first three digits of this form the significant part of the 
    // format revision number for F# binary signature and optimization metadata. The last digit is not significant.
    //
    // WARNING: Do not change this revision number unless you absolutely know what you're doing.
    let FSharpBinaryMetadataFormatRevision = "2.0.0.0"

#if SILVERLIGHT
    let Get32BitRegistryStringValueViaPInvoke(_subKey:string) = 
        None
    let BinFolderOfDefaultFSharpCompiler = 
        Some ""
#else

    let FSharpCoreLibRunningVersion = 
        try match (typeof<Microsoft.FSharp.Collections.List<int>>).Assembly.GetName().Version.ToString() with
            | null -> None
            | "" -> None
            | s  -> Some(s)
        with _ -> None

    [<DllImport("Advapi32.dll", CharSet = CharSet.Unicode, BestFitMapping = false)>]
    extern uint32 RegOpenKeyExW(UIntPtr _hKey, string _lpSubKey, uint32 _ulOptions, int _samDesired, UIntPtr & _phkResult);

    [<DllImport("Advapi32.dll", CharSet = CharSet.Unicode, BestFitMapping = false)>]
    extern uint32 RegQueryValueExW(UIntPtr _hKey, string _lpValueName, uint32 _lpReserved, uint32 & _lpType, IntPtr _lpData, int & _lpchData);

    [<DllImport("Advapi32.dll")>]
    extern uint32 RegCloseKey(UIntPtr _hKey)

    module Option = 
        /// Convert string into Option string where null and String.Empty result in None
        let ofString s = 
            if String.IsNullOrEmpty(s) then None
            else Some(s)

            
        

    // MaxPath accounts for the null-terminating character, for example, the maximum path on the D drive is "D:\<256 chars>\0". 
    // See: ndp\clr\src\BCL\System\IO\Path.cs
    let maxPath = 260;
    let maxDataLength = (new System.Text.UTF32Encoding()).GetMaxByteCount(maxPath)
    let KEY_WOW64_DEFAULT = 0x0000
    let KEY_WOW64_32KEY = 0x0200
    let HKEY_LOCAL_MACHINE = UIntPtr(0x80000002u)
    let KEY_QUERY_VALUE = 0x1
    let REG_SZ = 1u

    let GetDefaultRegistryStringValueViaDotNet(subKey: string)  =
        Option.ofString
            (try
                downcast Microsoft.Win32.Registry.GetValue("HKEY_LOCAL_MACHINE\\"+subKey,null,null)
             with e->
                System.Diagnostics.Debug.Assert(false, sprintf "Failed in GetDefaultRegistryStringValueViaDotNet: %s" (e.ToString()))
                null)

// RegistryView.Registry API is not available before .NET 4.0
#if FX_ATLEAST_40_COMPILER_LOCATION
    let Get32BitRegistryStringValueViaDotNet(subKey: string) =
        Option.ofString
            (try
                let key = RegistryKey.OpenBaseKey(RegistryHive.LocalMachine, RegistryView.Registry32)
                if key = null then null
                else
                    let sub = key.OpenSubKey(subKey)
                    if sub = null then null
                    else 
                        downcast (sub.GetValue(null, null))
             with e->
                System.Diagnostics.Debug.Assert(false, sprintf "Failed in Get32BitRegistryStringValueViaDotNet: %s" (e.ToString()))
                null)
#endif



    let Get32BitRegistryStringValueViaPInvoke(subKey:string) = 
        Option.ofString
            (try 
                // 64 bit flag is not available <= Win2k
                let options = 
                    match Environment.OSVersion.Version.Major with
                    | major when major >= 5 -> KEY_WOW64_32KEY
                    | _ -> KEY_WOW64_DEFAULT


                let mutable hkey = UIntPtr.Zero;
                let pathResult = Marshal.AllocCoTaskMem(maxDataLength);

                try
                    let res = RegOpenKeyExW(HKEY_LOCAL_MACHINE,subKey, 0u, KEY_QUERY_VALUE ||| options, & hkey)
                    if res = 0u then
                        let mutable uType = REG_SZ;
                        let mutable cbData = maxDataLength;

                        let res = RegQueryValueExW(hkey, null, 0u, &uType, pathResult, &cbData);

                        if (res = 0u && cbData > 0 && cbData <= maxDataLength) then
                            Marshal.PtrToStringUni(pathResult, (cbData - 2)/2);
                        else 
                            null
                    else
                        null
                finally
                    if hkey <> UIntPtr.Zero then
                        RegCloseKey(hkey) |> ignore
                
                    if pathResult <> IntPtr.Zero then
                        Marshal.FreeCoTaskMem(pathResult)
             with e->
                System.Diagnostics.Debug.Assert(false, sprintf "Failed in Get32BitRegistryStringValueViaPInvoke: %s" (e.ToString()))
                null)

    let is32Bit = IntPtr.Size = 4
    
    let tryRegKey(subKey:string) = 

        if is32Bit then
            let s = GetDefaultRegistryStringValueViaDotNet(subKey)
            // If we got here AND we're on a 32-bit OS then we can validate that Get32BitRegistryStringValueViaPInvoke(...) works
            // by comparing against the result from GetDefaultRegistryStringValueViaDotNet(...)
#if DEBUG
            let viaPinvoke = Get32BitRegistryStringValueViaPInvoke(subKey)
            System.Diagnostics.Debug.Assert((s = viaPinvoke), sprintf "32bit path: pi=%A def=%A" viaPinvoke s)
#endif
            s
        else
#if FX_ATLEAST_40_COMPILER_LOCATION
            match Get32BitRegistryStringValueViaDotNet(subKey) with
            | None -> Get32BitRegistryStringValueViaPInvoke(subKey) 
            | s ->
#if DEBUG
                // If we got here AND we're on .NET 4.0 then we can validate that Get32BitRegistryStringValueViaPInvoke(...) works
                // by comparing against the result from Get32BitRegistryStringValueViaDotNet(...)
                let viaPinvoke = Get32BitRegistryStringValueViaPInvoke(subKey)
                System.Diagnostics.Debug.Assert((s = viaPinvoke), sprintf "Non-32bit path: pi=%A def=%A" viaPinvoke s)
#endif
                s
#else
            Get32BitRegistryStringValueViaPInvoke(subKey) 
#endif
               

    let internal tryCurrentDomain() = 
        let pathFromCurrentDomain = System.AppDomain.CurrentDomain.BaseDirectory
        if not(String.IsNullOrEmpty(pathFromCurrentDomain)) then 
            Some pathFromCurrentDomain
        else
            None
    
    let internal tryAppConfig (appConfigKey:string) = 

        let locationFromAppConfig = ConfigurationSettings.AppSettings.[appConfigKey]
        System.Diagnostics.Debug.Print(sprintf "Considering appConfigKey %s which has value '%s'" appConfigKey locationFromAppConfig) 

        if String.IsNullOrEmpty(locationFromAppConfig) then 
            None
        else
            let exeAssemblyFolder = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
            let locationFromAppConfig = locationFromAppConfig.Replace("{exepath}", exeAssemblyFolder)
            System.Diagnostics.Debug.Print(sprintf "Using path %s" locationFromAppConfig) 
            Some locationFromAppConfig

    // The default location of FSharp.Core.dll and fsc.exe based on the version of fsc.exe that is running
    // Used for
    //     - location of design-time copies of FSharp.Core.dll and FSharp.Compiler.Interactive.Settings.dll for the default assumed environment for scripts
    //     - default ToolPath in tasks in FSharp.Build.dll (for Fsc tasks)
    //     - default F# binaries directory in service.fs (REVIEW: check this)
    //     - default location of fsi.exe in FSharp.VS.FSI.dll
    //     - default location of fsc.exe in FSharp.Compiler.CodeDom.dll
    //     - default F# binaries directory in (project system) Project.fs
    
      /// Try to find the F# compiler location by looking at the "fsharpi" script installed by F# packages
    let internal tryFsharpiScript(url:string) =
      try
        let str = File.ReadAllText(url)
        let reg = new System.Text.RegularExpressions.Regex("mono.* (\/.*)\/fsi\.exe")
        let res = reg.Match(str)
        if res.Success then Some(res.Groups.[1].Value) else None
      with e -> 
        None


    let BackupInstallationProbePoints = 
      [ // prefer the latest installation of Mono on Mac
        "/Library/Frameworks/Mono.framework/Versions/Current"
        // prefer freshly built F# compilers on Linux
        "/usr/local"
        // otherwise look in the standard place
        "/usr" ]

    // The default location of FSharp.Core.dll and fsc.exe based on the version of fsc.exe that is running
  // Used for
  //   - location of design-time copies of FSharp.Core.dll and FSharp.Compiler.Interactive.Settings.dll for the default assumed environment for scripts
  //   - default ToolPath in tasks in FSharp.Build.dll (for Fsc tasks)
  //   - default F# binaries directory in service.fs (REVIEW: check this)
  //   - default location of fsi.exe in FSharp.VS.FSI.dll
  //   - default location of fsc.exe in FSharp.Compiler.CodeDom.dll
    let BinFolderOfDefaultFSharpCompiler() = 
    // Check for an app.config setting to redirect the default compiler location
    // Like fsharp-compiler-location
     try 
      // FSharp.Compiler support setting an appkey for compiler location. I've never seen this used.
      //printfn "Resolution" "BinFolderOfDefaultFSharpCore: Probing app.config"
      let result = tryAppConfig "fsharp-compiler-location"
      match result with 
      | Some _ ->  result 
      | None -> 

        // On windows the location of the compiler is via a registry key
        let key20 = @"Software\Microsoft\FSharp\2.0\Runtime\v4.0"
        let key40 = @"Software\Microsoft\FSharp\3.0\Runtime\v4.0"
        let key1,key2 = 
          match FSharpCoreLibRunningVersion with 
          | None -> key40,key20 
          | Some v -> if v.Length > 1 && v.[0] <= '3' then key20,key40 else key40,key20
        
        //printfn "Resolution" "BinFolderOfDefaultFSharpCore: Probing registry key %s" key1
        let result = tryRegKey key1
        match result with 
        | Some _ ->  result 
        | None -> 
        //printfn "Resolution" "BinFolderOfDefaultFSharpCore: Probing registry key %s" key2
        let result =  tryRegKey key2
        match result with 
        | Some _ ->  result 
        | None ->

        // On Unix we let you set FSHARP_COMILER_BIN. I've rarely seen this used and its not documented in the install isntructions.
        //printfn "Resolution" "BinFolderOfDefaultFSharpCore: Probing environment variable FSHARP_COMPILER_BIN"
        let result = 
            let var = System.Environment.GetEnvironmentVariable("FSHARP_COMPILER_BIN")
            if String.IsNullOrEmpty(var) then None
            else Some(var)
        match result with 
        | Some _ -> result
        | None -> 

        // On Unix we probe 'bin' under various hardwired paths for the scripts 'fsharpc' and 'fsharpi'. 
        // We then loko in the script to see the Mono location it is pointing to. 
        // This is pretty fragile, e.g. the script lookup is done via a regular expression.
        // Really we should just search the path or otherwise resolve the 'mono' command?
        let result = 
            BackupInstallationProbePoints |> List.tryPick (fun x -> 
               //printfn "Resolution" "BinFolderOfDefaultFSharpCore: Probing  %s" x
               let safeExists f = (try File.Exists(f) with _ -> false)
               let file f = Path.Combine(Path.Combine(x,"bin"),f)
               let exists f = safeExists(file f)
               match (if exists "fsc" && exists "fsi" then tryFsharpiScript (file "fsi") else None) with
               | Some res -> Some res
               | None ->
               match (if exists "fsharpc" && exists "fsharpi" then tryFsharpiScript (file "fsharpi") else None) with
               | Some res -> Some res
               | None -> None)
                
        match result with 
        | Some _ -> result
        | None -> 
               // For the prototype compiler, we can just use the current domain
               tryCurrentDomain()
     with e -> 
      System.Diagnostics.Debug.Assert(false, "Error while determining default location of F# compiler")
      //printfn "Resolution" "BinFolderOfDefaultFSharpCore: error %s" (e.ToString())
      None


#endif // SILVERLIGHT
#if FX_ATLEAST_45
    // Apply the given function to the registry entry corresponding to the subkey.
    // The reg key is dispoed at the end of the scope.
    let useKey subkey f =
        let key = Registry.LocalMachine.OpenSubKey subkey
        try f key 
        finally 
            match key with 
            | null -> () 
            | _ -> key.Dispose()

    // Check if the framework version 4.5 or above is installed at the given key entry 
    let IsNetFx45OrAboveInstalledAt subkey =
        useKey subkey (fun regkey ->
            match regkey with
            | null -> false
            | _ -> regkey.GetValue("Release", 0) :?> int |> (fun s -> s >= 0x50000)) // 0x50000 implies 4.5.0

    // Check if the framework version 4.5 or above is installed
    let IsNetFx45OrAboveInstalled =
        IsNetFx45OrAboveInstalledAt @"SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Client" ||
        IsNetFx45OrAboveInstalledAt @"SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full" 

    // Check if the running framework version is 4.5 or above.
    // Use the presence of v4.5.x in the registry to distinguish between 4.0 and 4.5
    let IsRunningOnNetFx45OrAbove =
            let major = typeof<System.Int32>.Assembly.GetName().Version.Major
            major > 4 || (major = 4 && IsNetFx45OrAboveInstalled)
#else
    let IsRunningOnNetFx45OrAbove = false
#endif