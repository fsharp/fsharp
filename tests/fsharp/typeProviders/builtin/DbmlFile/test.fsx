// #Conformance #TypeProviders #DbmlFile
#r "FSharp.Data.TypeProviders.dll"

open Microsoft.FSharp.Core.CompilerServices
open System.IO

[<AutoOpen>]
module Infrastructure = 
    let failures = ref false
    let reportFailure () = stderr.WriteLine " NO"; failures := true
    let test s b = stderr.Write(s:string);  if b then stderr.WriteLine " OK" else reportFailure() 
    let check s v1 v2 = stderr.Write(s:string);  if v1 = v2 then stderr.WriteLine " OK" else eprintf "... FAILURE: expected %A, got %A  " v2 v1;  reportFailure() 

    let argv = System.Environment.GetCommandLineArgs() 
    let SetCulture() = 
      if argv.Length > 2 && argv.[1] = "--culture" then  begin
        let cultureString = argv.[2] in 
        let culture = new System.Globalization.CultureInfo(cultureString) in 
        stdout.WriteLine ("Running under culture "+culture.ToString()+"...");
        System.Threading.Thread.CurrentThread.CurrentCulture <-  culture
      end 
  
    do SetCulture()    

module CheckDbmlFileTypeProvider = 

    let checkHostedType (hostedType: System.Type) = 
        test "ceklc09wlkm1a" (hostedType.Assembly <> typeof<Microsoft.FSharp.Data.TypeProviders.DesignTime.DataProviders>.Assembly)
        test "ceklc09wlkm1b" (hostedType.Assembly.FullName.StartsWith "tmp")

        check "ceklc09wlkm2" hostedType.DeclaringType null
        check "ceklc09wlkm3" hostedType.DeclaringMethod null
        check "ceklc09wlkm4" hostedType.FullName "Microsoft.FSharp.Data.TypeProviders.DbmlFileApplied"
        check "ceklc09wlkm5" (hostedType.GetConstructors()) [| |]
        check "ceklc09wlkm6" (hostedType.GetCustomAttributesData().Count) 1
        check "ceklc09wlkm6" (hostedType.GetCustomAttributesData().[0].Constructor.DeclaringType.FullName) typeof<TypeProviderXmlDocAttribute>.FullName
        check "ceklc09wlkm7" (hostedType.GetEvents()) [| |]
        check "ceklc09wlkm8" (hostedType.GetFields()) [| |]
        check "ceklc09wlkm9" (hostedType.GetMethods()) [| |]
        check "ceklc09wlkm10" (hostedType.GetProperties()) [| |]
        check "ceklc09wlkm11" (hostedType.GetNestedTypes().Length) 14
        check "ceklc09wlkm12" 
            (set [ for x in hostedType.GetNestedTypes() -> x.Name ]) 
            (set ["DataClasses1DataContext"; "Department"; "Employee"; "EmployeeAddress"; "EmployeeDepartmentHistory"; "EmployeePayHistory"; "JobCandidate"; "Shift"; "vEmployee"; "uspGetBillOfMaterialsResult"; "uspGetEmployeeManagersResult"; "uspGetManagerEmployeesResult"; "uspGetWhereUsedProductIDResult"; "ufnGetContactInformationResult"])

        // Note: DbmlFile TP does not organize (yet?)
        //       so I do not need to verify the nested types under "ServiceTypes"
        //       See other TP tests for an example

        // Deep check on one type: Department [Table]
        let departementType = (hostedType.GetNestedTypes() |> Seq.find (fun t -> t.Name = "Department"))
        check "ceklc09wlkm131"  (set [ for x in departementType.GetProperties() -> x.Name ]) (set [|"DepartmentID"; "EmployeeDepartmentHistories"; "GroupName"; "ModifiedDate"; "Name"|])
        check "ceklc09wlkm133a"  (set [ for x in departementType.GetFields() -> x.Name ]) (set [| |])
        check "ceklc09wlkm133b"  (set [ for x in departementType.GetFields(System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Public  ||| System.Reflection.BindingFlags.FlattenHierarchy) -> x.Name ]) 
                                 (set [ ] )
        check "ceklc09wlkm134"  (set [ for x in departementType.GetMethods() -> x.Name ]) 
            (set [
                "get_DepartmentID";
                "set_DepartmentID";
                "set_Name";
                "get_Name";
                "set_GroupName";
                "get_GroupName";
                "set_ModifiedDate";
                "get_ModifiedDate";
                "get_EmployeeDepartmentHistories";
                "set_EmployeeDepartmentHistories";
                "add_PropertyChanging";
                "remove_PropertyChanging";
                "add_PropertyChanged";
                "remove_PropertyChanged";
                "ToString";
                "Equals";
                "GetHashCode";
                "GetType"
             ] )

        check "ceklc09wlkm135"  (set [ for x in departementType.GetMethods(System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Public  ||| System.Reflection.BindingFlags.FlattenHierarchy) -> x.Name ]) 
                                (set [ "Equals"; "ReferenceEquals" ] )
        check "ceklc09wlkm136"  (departementType.GetNestedTypes()) [||]

        // Check on another type: uspGetBillOfMaterialsResult [Return value of a SP]
        let uspGetBillOfMaterialsResultType = (hostedType.GetNestedTypes() |> Seq.find (fun t -> t.Name = "uspGetBillOfMaterialsResult"))
        check "ceklc09wlkm147"  (set [ for x in uspGetBillOfMaterialsResultType.GetProperties() -> x.Name ]) (set [|"BOMLevel"; "ComponentDesc"; "ComponentID"; "ListPrice"; "ProductAssemblyID"; "RecursionLevel"; "StandardCost"; "TotalQuantity" |] )
        check "ceklc09wlkm148"  (uspGetBillOfMaterialsResultType.GetFields()) [||]
        check "ceklc09wlkm149"  (set [ for x in uspGetBillOfMaterialsResultType.GetMethods() -> x.Name ])
                                (set [| 
                                    "get_ProductAssemblyID"; "set_ProductAssemblyID";
                                    "get_ComponentID"; "set_ComponentID";
                                    "get_ComponentDesc"; "set_ComponentDesc";
                                    "get_TotalQuantity"; "set_TotalQuantity";
                                    "set_StandardCost"; "get_StandardCost";
                                    "set_ListPrice"; "get_ListPrice";
                                    "get_BOMLevel"; "set_BOMLevel";
                                    "set_RecursionLevel"; "get_RecursionLevel";
                                    "ToString"; "Equals"; "GetHashCode"; "GetType"
                                    |])
        check "ceklc09wlkm150"  (uspGetBillOfMaterialsResultType.GetNestedTypes()) [||]

        // Check on another type: ufnGetContactInformationResult [Return value of a Function]
        let ufnGetContactInformationResultType = (hostedType.GetNestedTypes() |> Seq.find (fun t -> t.Name = "ufnGetContactInformationResult"))
        check "ceklc09wlkm151"  (set [ for x in ufnGetContactInformationResultType.GetProperties() -> x.Name ]) (set [| "ContactID"; "ContactType"; "FirstName"; "JobTitle"; "LastName" |] )
        check "ceklc09wlkm152"  (ufnGetContactInformationResultType.GetFields()) [||]
        check "ceklc09wlkm153"  (set [ for x in ufnGetContactInformationResultType.GetMethods() -> x.Name ])
                                (set [| 
                                    "get_ContactID"; "set_ContactID";
                                    "get_FirstName"; "set_FirstName";
                                    "get_LastName"; "set_LastName"; 
                                    "get_JobTitle"; "set_JobTitle"; 
                                    "get_ContactType"; "set_ContactType";
                                    "ToString"; "Equals"; "GetHashCode"; "GetType"
                                    |])
        check "ceklc09wlkm154"  (ufnGetContactInformationResultType.GetNestedTypes()) [||]

        // Check on another type: vEmployee [View]
        let vEmployeeType = (hostedType.GetNestedTypes() |> Seq.find (fun t -> t.Name = "vEmployee"))
        check "ceklc09wlkm141"  (set [ for x in vEmployeeType.GetProperties() -> x.Name ]) (set [| "EmployeeID"; "Title"; "FirstName"; "MiddleName"; "LastName"; "Suffix"; "JobTitle"; "Phone"; "EmailAddress"; "EmailPromotion"; "AddressLine1"; "AddressLine2"; "City"; "StateProvinceName"; "PostalCode"; "CountryRegionName"; "AdditionalContactInfo"|])
        check "ceklc09wlkm142"  (vEmployeeType.GetFields()) [||]
        check "ceklc09wlkm144"  (set [ for x in vEmployeeType.GetMethods() -> x.Name ])
                                (set [|
                                    "set_EmployeeID"; "get_EmployeeID";
                                    "set_Title"; "get_Title";
                                    "get_FirstName"; "set_FirstName";
                                    "get_MiddleName"; "set_MiddleName";
                                    "set_LastName"; "get_LastName";
                                    "get_Suffix"; "set_Suffix";
                                    "get_JobTitle"; "set_JobTitle";
                                    "set_Phone"; "get_Phone";
                                    "get_EmailAddress"; "set_EmailAddress";
                                    "get_EmailPromotion"; "set_EmailPromotion";
                                    "get_AddressLine1"; "set_AddressLine1";
                                    "get_AddressLine2"; "set_AddressLine2";
                                    "get_City"; "set_City";
                                    "get_StateProvinceName"; "set_StateProvinceName";
                                    "get_PostalCode"; "set_PostalCode";
                                    "get_CountryRegionName"; "set_CountryRegionName";
                                    "get_AdditionalContactInfo"; "set_AdditionalContactInfo";
                                    "ToString";
                                    "Equals";
                                    "GetHashCode";
                                    "GetType" |])
        check "ceklc09wlkm146"  (vEmployeeType.GetNestedTypes()) [||]

    let instantiateTypeProviderAndCheckOneHostedType( dbmlfile : string, fullPathName:string[] ) = 

        let assemblyFile = typeof<Microsoft.FSharp.Data.TypeProviders.DesignTime.DataProviders>.Assembly.CodeBase.Replace("file:///","").Replace("/","\\")
        test "CheckFSharpDataTypeProvidersDLLExist" (File.Exists assemblyFile) 

        // If/when we care about the "target framework", this mock function will have to be fully implemented
        let systemRuntimeContainsType s = 
            printfn "Call systemRuntimeContainsType(%s) returning dummy value 'true'" s
            true

        let tpConfig = new TypeProviderConfig(systemRuntimeContainsType, ResolutionFolder=__SOURCE_DIRECTORY__, RuntimeAssembly=assemblyFile, ReferencedAssemblies=[| |], TemporaryFolder=Path.GetTempPath(), IsInvalidationSupported=true, IsHostedExecution=true)
        use typeProvider1 = (new Microsoft.FSharp.Data.TypeProviders.DesignTime.DataProviders( tpConfig ) :> ITypeProvider)

        // Setup machinery to keep track of the "invalidate event" (see below)
        let invalidateEventCount = ref 0
        typeProvider1.Invalidate.Add(fun _ -> incr invalidateEventCount)

        // Load a type provider instance for the type and restart
        let hostedNamespace1 = typeProvider1.GetNamespaces() |> Seq.find (fun t -> t.NamespaceName = "Microsoft.FSharp.Data.TypeProviders")

        check "CheckAllTPsAreThere" (set [ for i in hostedNamespace1.GetTypes() -> i.Name ]) (set ["DbmlFile"; "EdmxFile"; "ODataService"; "SqlDataConnection";"SqlEntityConnection";"WsdlService"])

        let hostedType1 = hostedNamespace1.ResolveTypeName("DbmlFile")
        let hostedType1StaticParameters = typeProvider1.GetStaticParameters(hostedType1)
        check "VerifyStaticParam" 
            (set [ for i in hostedType1StaticParameters -> i.Name ]) 
            (set [ "File"; "ResolutionFolder"; "ContextTypeName"; "Serializable" ])

        let staticParameterValues = 
            [| for x in hostedType1StaticParameters -> 
                (match x.Name with 
                 | "File" -> box dbmlfile  
                 | _ -> box x.RawDefaultValue) |]
        printfn "instantiating type... may take a while for code generation tool to run and csc.exe to run..."
        let hostedAppliedType1 = typeProvider1.ApplyStaticArguments(hostedType1, fullPathName, staticParameterValues)

        checkHostedType hostedAppliedType1 

        // Write replacement text into the file and check that the invalidation event is triggered....
        let file1NewContents = System.IO.File.ReadAllText(dbmlfile).Replace("Employee", "Worker")       // Rename 'Employee' to 'Worker'
        do File.WriteAllText(dbmlfile, file1NewContents)

        // Wait for invalidate event to fire....
        for i in 0 .. 30 do
            if !invalidateEventCount = 0 then 
                System.Threading.Thread.Sleep 100

        check "VerifyInvalidateEventFired" !invalidateEventCount 1

    // Test with absolute path
    // Copy the .dbml used for tests to avoid trashing our original (we may overwrite it when testing the event)
    let dbmlfile = Path.Combine(__SOURCE_DIRECTORY__, "AdventureWorksDev10.dbml")
    System.IO.File.Copy(Path.Combine(__SOURCE_DIRECTORY__, @"DbmlFiles\AdventureWorksDev10.dbml"), dbmlfile, true)
    System.IO.File.SetAttributes(dbmlfile, System.IO.FileAttributes.Normal)
    instantiateTypeProviderAndCheckOneHostedType(dbmlfile, [| "DbmlFileApplied" |])

    // Test with relative path
    // Copy the .dbml used for tests to avoid trashing our original (we may overwrite it when testing the event)
    System.IO.File.Copy(Path.Combine(__SOURCE_DIRECTORY__, @"DbmlFiles\AdventureWorksDev10.dbml"), dbmlfile, true)
    System.IO.File.SetAttributes(dbmlfile, System.IO.FileAttributes.Normal)
    instantiateTypeProviderAndCheckOneHostedType( System.IO.Path.GetFileName(dbmlfile), [| "DbmlFileApplied" |])

let _ = 
    if !failures then (stdout.WriteLine "Test Failed"; exit 1) 

do (stdout.WriteLine "Test Passed"; 
    File.WriteAllText("test.ok","ok"); 
    exit 0)

