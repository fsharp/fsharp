(cd Sample_MonoDevelop_3_2_8_Console && xbuild) &&
(cd Sample_VS2010_FSharp_ConsoleApp_net35 && xbuild) &&
(cd Sample_VS2010_FSharp_ConsoleApp_net40 && xbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net35 && xbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net40 && xbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net45 && xbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net45_with_resource && xbuild) &&
(cd Sample_VS2012_FSharp_Portable_Library && xbuild) &&
(cd Sample_VS2013_FSharp_ConsoleApp_net40 && xbuild) &&
(cd Sample_VS2013_FSharp_ConsoleApp_net45 && xbuild) &&
(cd Sample_VS2013_FSharp_ConsoleApp_net451 && xbuild) &&
(cd Sample_VS2013_FSharp_Library_net40 && xbuild) &&
(cd Sample_VS2013_FSharp_Library_net45 && xbuild) &&
(cd Sample_VS2013_FSharp_Library_net451 && xbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_Legacy_net40 && xbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_Legacy_net45 && xbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_Legacy_net451 && xbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_net45 && xbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_net451 && xbuild) &&
(cd Sample_VS2012_FSharp_ConsoleApp_net40_upgraded_VS2013 && xbuild) &&
(cd Sample_VS2012_FSharp_Portable_Library_upgraded_2013 && xbuild) &&
(cd Sample_VS2013_FSharp_Portable_Library_net451_adjusted_to_profile78 && xbuild) &&
echo "all projects built successfully"


# Profile 259 not yet available on CI server Mono installation:
# (cd Sample_VS2013_FSharp_Portable_Library_net451_adjusted_to_profile259 && xbuild)


