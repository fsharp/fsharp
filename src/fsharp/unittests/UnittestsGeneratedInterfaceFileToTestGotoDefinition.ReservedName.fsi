namespace Internal.Utilities.Collections
  [<StructuralEqualityAttribute (); NoComparisonAttribute ()>]
  type internal ValueStrength<'T> =
    | Strong of 'T
    | Weak of System.WeakReference
  type internal AgedLookup<'TKey,'TValue> =
    class
      new : keepStrongly:int * areSame:('TKey * 'TKey -> bool) *
            ?onStrongDiscard:('TValue -> unit) * ?keepMax:int ->
              AgedLookup<'TKey,'TValue>
      member Clear : unit -> unit
      member Put : 'TKey * 'TValue -> unit
      member Remove : key:'TKey -> unit
      member TryGet : key:'TKey -> 'TValue option
      member TryGetKeyValue : key:'TKey -> ('TKey * 'TValue) option
      member TryPeekKeyValue : key:'TKey -> ('TKey * 'TValue) option
      member MostRecent : ('TKey * 'TValue) option
    end
  type internal MruCache<'TKey,'TValue> =
    class
      new : keepStrongly:int * compute:('TKey -> 'TValue) *
            areSame:('TKey * 'TKey -> bool) *
            ?isStillValid:('TKey * 'TValue -> bool) *
            ?areSameForSubsumption:('TKey * 'TKey -> bool) *
            ?logComputedNewValue:('TKey -> unit) *
            ?logUsedCachedValue:('TKey -> unit) * ?onDiscard:('TValue -> unit) *
            ?keepMax:int -> MruCache<'TKey,'TValue>
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
        groupByFirst : l:('TKey * 'TValue) list -> ('TKey * 'TValue list) list
                         when 'TKey : equality
      static member referenceDistinct : 'T list -> 'T list when 'T : not struct
    end

namespace Internal.Utilities
  module internal FSharpEnvironment = begin
    val DotNetBuildString : string
    val FSharpBinaryMetadataFormatRevision : string
    val FSharpCoreLibRunningVersion : string option
    val RegOpenKeyExW :
      _hKey:System.UIntPtr * _lpSubKey:string * _ulOptions:uint32 *
      _samDesired:int * _phkResult:byref<System.UIntPtr> -> uint32
    val RegQueryValueExW :
      _hKey:System.UIntPtr * _lpValueName:string * _lpReserved:uint32 *
      _lpType:byref<uint32> * _lpData:System.IntPtr * _lpchData:byref<int> ->
        uint32
    val RegCloseKey : _hKey:System.UIntPtr -> uint32
    module Option = begin
      val ofString : s:string -> string option
    end
    val maxPath : int
    val maxDataLength : int
    val KEY_WOW64_DEFAULT : int
    val KEY_WOW64_32KEY : int
    val HKEY_LOCAL_MACHINE : System.UIntPtr
    val KEY_QUERY_VALUE : int
    val REG_SZ : uint32
    val GetDefaultRegistryStringValueViaDotNet : subKey:string -> string option
    val Get32BitRegistryStringValueViaPInvoke : subKey:string -> string option
    val is32Bit : bool
    val tryRegKey : subKey:string -> string option
    val tryCurrentDomain : unit -> string option
    val tryAppConfig : appConfigKey:string -> string option
    val BinFolderOfDefaultFSharpCompiler : string option
    val IsRunningOnNetFx45OrAbove : bool
  end

