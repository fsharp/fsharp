10.2.1
 * Bump versions to match [versioning spec](https://github.com/fsharp/fslang-design/blob/master/tooling/FST-1004-versioning-plan.md)

10.1.2
 * Bump versions and integrate visualfsharp master

10.1.1
 * Bump versions

10.1.0
 * Integrte visualfsharp master

10.0.2
 * Cherry-pick https://github.com/Microsoft/visualfsharp/pull/4804

10.0.1
 * Switch to new versioning for F# compiler tools
 * add FSharp.Compiler.Tools.props back into nuget package

4.1.34
 * Use strong-name signed FSharp.Compiler.Interactive.Settings.dll in FSharp.Compiler.Tools package

4.1.33
 * Cherry pick https://github.com/Microsoft/visualfsharp/pull/4348/

4.1.32
 * Integrate visualfsharp master to bc7ce194a09474b0bfea185ec15300b54e7aaf9e

4.1.31
  * Integrate visualfsharp master to 31ec0e0fb
  * Remove .NET Core version of compiler in FSharp.Compiler.Tools (now only for .NET Framework and Mono)

4.1.30
  * Integrate visualfsharp master to 94c7fe1f15811d9c57c26d49cc2309883b83d338

4.1.29
  * Integrate visualfsharp master to d903cf6277e40646563f24bbe2790e8a292d5f9b

4.1.28
  * Remove FSharp.Core from GAC

4.1.27
  * Integrate visualfsharp master

4.1.26
  * Tag latest

4.1.25
  * [Make F# resources work more like C#](https://github.com/fsharp/fsharp/pull/761)
  * Fix [Warning regarding WinForms on launch of `fsharpi` on mono](https://github.com/fsharp/fsharp/pull/767)

4.1.24
  * Build using Mono 5.0 and msbuild

4.1.23
  * Fix nuget package for FSharp.Compiler.Tools System.ValueTuple.dll

4.1.21
  * Fix nuget package for FSharp.Compiler.Tools

4.1.19
  * Integrate changes from visualfsharp

4.1.15
  * fix regression on binding redirects for System.Collections.Immutable

4.1.10
  * Integrate visualfsharp to 55ffe91

4.1.9
  * Fix duplicate resources on OSX

4.1.8
  * [Fix regression in Microsoft.Build.FSharp.targets](https://github.com/fsharp/fsharp/pull/707)

4.1.7
  * [fix binding redirects for System.Collections.Immutable](https://github.com/fsharp/fsharp/issues/699)

4.1.6
  * fix version of library going in /usr/lib/mono/fsharp

4.1.4
  * [align fsc task and target file](https://github.com/fsharp/fsharp/pull/690)
  * [use install layout that includes mono/fsharp](https://github.com/fsharp/fsharp/pull/689)
  * [fix F# Intereactive on Mono 4.9+](https://github.com/fsharp/fsharp/pull/687)

4.1.1
  * Update compiler tools

4.1.0.2
  * Include DiaSymReader DLLs in nuget package

4.1.0.0
  * Updates to FSharp.Core nuget package for F# 4.1

4.0.1.21
  * Fix [#656](https://github.com/fsharp/fsharp/issues/656)  - error FS0193: internal error: No access to the given key

4.0.1.20
  * Fix #639 - Problems with F# scripts on Mono 

4.0.1.19
  * Fix FSharp.Compiler.Tools targets for .NET Core usage

4.0.1.16
  * Fix FSharp.Compiler.Tools package dependencies

4.0.1.15
  * Fix null condition in Fsc task

4.0.1.13
  * Integrate visualfsharp master to 2002675/216a38b

4.0.1.3
  * Integrate Microsoft\visualfsharp to 688c26bdbbfc766326fc45e4d918f87fcba1e7ba. F# 4.1 work
  * [Inlined function causes "FS0078: Unable to find the file"](https://github.com/fsharp/fsharp/issues/584)

4.0.1.2
  * Integrate Microsoft\visualfsharp to 5d8126a. F# 4.1 work
  *    FCS API integration and alignment
  *    [#528 Compiler does not catch typing error in pattern matching when using literals](https://github.com/Microsoft/visualfsharp/issues/528)
  *    [#659 Fsi fails to augment a type constructor](https://github.com/Microsoft/visualfsharp/issues/659)
  *    [#807 Async.Choice](https://github.com/Microsoft/visualfsharp/pull/807)
  *    [Response files for fsc/fsi](https://github.com/Microsoft/visualfsharp/pull/831)
  *    [#919 Fix handling of optional IDispatch, IUnknown method args](https://github.com/Microsoft/visualfsharp/pull/919)

4.0.1.1
  * Integrate Microsoft\visualfsharp to 8111c63. See CHANGELOG-visualfsharp.md for changes from Microsoft\visualfsharp
  * 04cd959 - fix problem with loop optimization
  * d995c59 - Assume we are inside XBuild when 4.5/Mono.Posix.dll exists
  * Fix [#483](https://github.com/fsharp/fsharp/issues/483) - Call to Type.GetType for a missing type causes FSI on Mono to produce an erroneous error

4.0.0.4
  * Integrate Microsoft\visualfsharp to 2d413fb94. See CHANGELOG.md for changes from Microsoft\visualfsharp

4.0.0.3
  * Fix build problems

4.0.0.1
  * Integrate F# 4.0

3.1.1.25
  * Upstream fix: implement correct version number scheme for new PCL profiles
  
3.1.1.20
  * Upstream fix:: #! treated as a comment when it is at the start of an fsharp file
  * Upstream fix visualfsharp.codeplex.com issue #78 - allow space characters in active pattern case identifiers
  * Upstream fix visualfsharp.codeplex.com issue #78 - Adjust parser to disallow vertical pipes in active pattern case identifiers. 
  * Upstream fix for visualfsharp.codeplex.com issue #69 - Invalid code generated when calling C# method with optional nullable args
  * Upstream fix for visualfsharp.codeplex.com issue #9 - XML doc comments on F# record type fields do not appear when accessing in C#

3.1.1.19
  * Added files for the nuget packages
  
3.1.1.18
  * Do not install FSharp.Build.dll in the GAC. It is not in the GAC on Windows.

3.1.1.17
  * Remove service components from FSharp.Compiler.dll since. All clients should now use FSharp.Compiler.Service.dll

3.1.1.15
  * Upstream 2d8ebcd43: XmlDoc generation bugfix: remove duplicate namespaces
  * Upstream 3bd52625b: Address-of optimizations for better performance on structs
  * Upstream c46aa237a: Perf improvement for Seq.windowed  
  * Upstream 301a6f33: make it possible to set breakpoint inside quotation
  * FSharp.Core for portable profiles 7, 78, 259 if reference assemblies are present (delay-signed)
  
3.1.1.14
  * Change monotouch and monodroid FSharp.Core.dll to both use 2.3.98.1
  
3.1.1.12
  * Add build command "all-monotouch-monodroid"

3.1.1.11

  * F# Interactive now uses .NET 4.5 profile
  * Fix for performance regression in 3.1 with extension members
  * Add query support to FSharp.Core portable profiles
  * Fix problem with building portable libraries (https://github.com/fsharp/fsharp/issues/299)

3.1.1.10
  * Include fixes from Microsoft included with Visual F# Tools 3.1.1

3.1.1.7
  * Build back versions of FSharp.Core 2.3.0.0, 2.3.5.0, 4.3.0.0 to include code generation fix.
  * Build Profile7 (portable47) FSharp.Core

3.0.30
  * Partial fix for allowing F# 3.1 projects to compile using xbuild (also 
    requires xbuild fix)

  * Remove a multitude of links in xbuild directories in favour of targets 
    files which include the canonical targets

3.0.29

  * Proper DESTDIR support (very useful for making custom deb/rpm packages)


