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


