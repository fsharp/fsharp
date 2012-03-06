namespace TestLibrary
  module LambdaCalculus = begin
    exception LambdaFailure of string
    module Syntax = begin
      type Binder = string
      type Expression =
        | Variable of Binder
        | Lambda of (Binder * Expression)
        | Apply of (Expression * Expression)
      val stringOfExpression : Expression -> string
    end
    module Evaluation = begin
      module Environment = begin
        type Env = Map<Syntax.Binder,Syntax.Expression>
        exception EnvironmentFailure of string
        val add :
          Env ->
            Syntax.Binder ->
              Syntax.Expression -> Map<Syntax.Binder,Syntax.Expression>
        val lookup : Env -> Syntax.Binder -> Syntax.Expression
      end
      exception EvalFailure = LambdaFailure
      val eval : Environment.Env -> Syntax.Expression -> Syntax.Expression
    end
  end
  module OtherTests = begin
    type Point =
      {x: int;
       y: int;}
    val showPoint : Point -> string
    type Shape =
      class
        new : initVertices:Point list -> Shape
        member
          addFilterMap : pr:(Point -> bool) ->
                           f:(Point -> Point) -> ps:Point list -> unit
        member addVertex : p:Point -> unit
        member clearVertices : unit -> unit
        member fold : f:('a -> Point -> 'a) -> acc:'a -> 'a
        member getVertices : unit -> Point list
        member map : f:(Point -> Point) -> unit
        member refold : f:('a -> Point -> 'a) -> acc:'a -> 'a
        member subsume : s:Shape -> unit
        member transpose : unit -> unit
        static member combine : s1:Shape -> s2:Shape -> Shape
      end
  end

namespace Internal.Utilities
  module internal FSharpEnvironment = begin
    val DotNetBuildString : string
    val FSharpCoreLibRunningVersion : string option
    val FSharpTeamVersionNumber : string
    val FSharpBinaryMetadataFormatRevision : string
    val RegOpenKeyExW :
      System.UIntPtr * string * uint32 * int * byref<System.UIntPtr> -> uint32
    val RegQueryValueExW :
      System.UIntPtr * string * uint32 * byref<uint32> * System.IntPtr *
      byref<int> -> uint32
    val RegCloseKey : System.UIntPtr -> uint32
    module Option = begin
      val ofString : string -> string option
    end
    val maxPath : int
    val maxDataLength : int
    val KEY_WOW64_DEFAULT : int
    val KEY_WOW64_32KEY : int
    val HKEY_LOCAL_MACHINE : System.UIntPtr
    val KEY_QUERY_VALUE : int
    val REG_SZ : uint32
    val GetDefaultRegistryStringValueViaDotNet : string -> string option
    val Get32BitRegistryStringValueViaPInvoke : string -> string option
    val is32Bit : bool
    val tryRegKey : string -> string option
    val tryCurrentDomain : unit -> string option
    val tryAppConfig : string -> string option
    val BinFolderOfDefaultFSharpCompiler : string option
  end

namespace Internal.Utilities.Collections
  [<StructuralEqualityAttribute (); NoComparisonAttribute ()>]
  type internal ValueStrength<'T> =
    | Strong of 'T
    | Weak of System.WeakReference
  type internal AgedLookup<'TKey,'TValue> =
    class
      new : keepStrongly:int * areSame:('TKey * 'TKey -> bool) ->
              AgedLookup<'TKey,'TValue>
      member Clear : unit -> unit
      member Put : 'TKey * 'TValue -> unit
      member Remove : key:'TKey -> unit
      member ToSeq : unit -> seq<'TKey * 'TValue>
      member TryGet : key:'TKey -> 'TValue option
      member TryGetKeyValue : key:'TKey -> ('TKey * 'TValue) option
      member TryPeekKeyValue : key:'TKey -> ('TKey * 'TValue) option
      member MostRecent : ('TKey * 'TValue) option
    end
  type internal MruCache<'TKey,'TValue> =
    class
      new : n:int * compute:('TKey -> 'TValue) * areSame:('TKey * 'TKey -> bool) *
            ?isStillValid:('TKey * 'TValue -> bool) *
            ?areSameForSubsumption:('TKey * 'TKey -> bool) *
            ?logComputedNewValue:('TKey -> unit) *
            ?logUsedCachedValue:('TKey -> unit) -> MruCache<'TKey,'TValue>
      member Clear : unit -> unit
      member Get : key:'TKey -> 'TValue
      member GetAvailable : key:'TKey -> 'TValue option
      member Remove : key:'TKey -> unit
      member SetAlternate : key:'TKey * value:'TValue -> unit
      member MostRecent : ('TKey * 'TValue) option
    end
  [<SealedAttribute ()>]
  type internal List =
    class
      static member
        GroupByFirst : l:('TKey * 'TValue) list -> ('TKey * 'TValue list) list
                         when 'TKey : equality
    end

namespace Microsoft.FSharp.UnitTest
  module FilesystemHelpers = begin
    val NewTempDirectory : System.String -> string
    val DoWithTempFile : string -> (string -> 'a) -> 'a
  end

namespace Unittests.General
  [<NUnit.Framework.TestFixture ()>]
  type GeneralUnitTests =
    class
      new : unit -> GeneralUnitTests
      [<NUnit.Framework.Test ()>]
      member ( PublicSurfaceArea.DotNetReflection ) : unit -> unit
    end

namespace Unittests.Build
  module HandyExtensions = begin
    type String with
      member MatchesPattern : p:string -> bool
    type String with
      member AssertMatchesPattern : p:string -> unit
  end
  type MyLogger =
    class
      interface Microsoft.Build.Framework.ILogger
      new : f:(string -> unit) -> MyLogger
    end
  type FauxHostObject =
    class
      interface Microsoft.Build.Framework.ITaskHost
      new : unit -> FauxHostObject
      member
        Compile : compile:System.Converter<int,int> * flags:string [] *
                  sources:string [] -> int
      member Flags : string []
      member Sources : string []
    end
  [<NUnit.Framework.TestFixture ()>]
  type TheTests =
    class
      new : unit -> TheTests
      [<NUnit.Framework.Test ()>]
      member MissingToolPathError : unit -> unit
      [<NUnit.Framework.SetUp ()>]
      member Setup : unit -> unit
      [<NUnit.Framework.TearDown ()>]
      member TearDown : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestAllCombo : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestCodePage : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestDebugSymbols : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestDebugType : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestDefineConstants : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestDisabledWarnings1 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestDisabledWarnings2 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestDocumentationFile : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestGenerateInterfaceFile : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestKeyFile : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestNoFramework : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestOptimize : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestOtherFlags : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestOutputAssembly : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestPdbFile : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestPlatform1 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestPlatform2 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestPlatform3 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestReferencePath : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestReferencePathWithSpaces : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestReferences : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestResources : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestSources : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestTailcalls : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestTargetType1 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestTargetType2 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestTargetType3 : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestUtf8Output : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestVersionFile : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestWin32Manifest : unit -> unit
      [<NUnit.Framework.Test ()>]
      member TestWin32Res : unit -> unit
    end

