(cd Sample_MonoDevelop_3_2_8_Console && msbuild) &&
#(cd Sample_VS2010_FSharp_ConsoleApp_net35 && msbuild) &&
(cd Sample_VS2010_FSharp_ConsoleApp_net40 && msbuild) &&
#(cd Sample_VS2012_FSharp_ConsoleApp_net35 && msbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net40 && msbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net45 && msbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net45_with_resource && msbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net45_with_resource_using_bootstrap && msbuild) &&
(cd Sample_VS2012_FSharp_Portable_Library && msbuild) &&
(cd Sample_VS2013_FSharp_ConsoleApp_net40 && msbuild) &&
(cd Sample_VS2013_FSharp_ConsoleApp_net45 && msbuild) &&
(cd Sample_VS2013_FSharp_ConsoleApp_net451 && msbuild) &&
(cd Sample_VS2013_FSharp_Library_net40 && msbuild) &&
(cd Sample_VS2013_FSharp_Library_net45 && msbuild) &&
(cd Sample_VS2013_FSharp_Library_net451 && msbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_Legacy_net40 && msbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_Legacy_net45 && msbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_Legacy_net451 && msbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_net45 && msbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_net451 && msbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net40_upgraded_VS2013 && msbuild) &&
(cd Sample_VS2012_FSharp_Portable_Library_upgraded_2013 && msbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_net451_adjusted_to_profile78 && msbuild) &&
(cd Sample_VS2015_FSharp_Console_App_net40 && msbuild) &&
(cd Sample_VS2015_FSharp_Console_App_net45 && msbuild) &&
(cd Sample_VS2015_FSharp_Console_App_net451 && msbuild) &&
(cd Sample_VS2015_FSharp_Console_App_net452 && msbuild) &&
(cd Sample_VS2015_FSharp_Library_net40 && msbuild) &&
(cd Sample_VS2015_FSharp_Library_net45 && msbuild) &&
(cd Sample_VS2015_FSharp_Library_net45_fsharp_30 && msbuild) &&
(cd Sample_VS2015_FSharp_Library_net45_fsharp_31 && msbuild) &&
(cd Sample_VS2015_FSharp_Portable7_Library && msbuild) &&
(cd Sample_VS2015_FSharp_Portable47_Library && msbuild) &&
(cd Sample_VS2015_FSharp_Portable78_Library && msbuild) &&
(cd Sample_VS2015_FSharp_Portable259_Library && msbuild) &&
echo "all projects built successfully"


# Profile 259 not yet available on CI server Mono installation:
# (cd Sample_VS2013_FSharp_Portable_Library_net451_adjusted_to_profile259 && msbuild)


