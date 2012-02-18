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


//-------------------------------------------------------------------------
// Define Initial Environment.  A bunch of types and values are hard-wired 
// into the compiler.  This lets the compiler perform particular optimizations
// for these types and values, for example emitting optimized calls for
// comparison and hashing functions.  The compiler generates the compiled code 
// for these types and values when the the --compiling-fslib switch is 
// provided when linking the FSharp.Core.dll assembly.
//------------------------------------------------------------------------- 

module internal Microsoft.FSharp.Compiler.Env 

open Internal.Utilities
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.PrettyNaming

let private envRange = rangeN "startup" 0

let vara = NewRigidTypar "a" envRange
let varb = NewRigidTypar "b" envRange
let private varc = NewRigidTypar "c" envRange
let private vard = NewRigidTypar "d" envRange
let private vare = NewRigidTypar "e" envRange
let private varf = NewRigidTypar "f" envRange
let private varg = NewRigidTypar "g" envRange

let varaTy = mkTyparTy vara 
let varbTy = mkTyparTy varb 
let private varcTy = mkTyparTy varc
let private vardTy = mkTyparTy vard
let private vareTy = mkTyparTy vare

type public IntrinsicValRef = IntrinsicValRef of NonLocalEntityRef * string * bool * TType * ValLinkageFullKey

let ValRefForIntrinsic (IntrinsicValRef(mvr,_,_,_,key))  = mkNonLocalValRef mvr key

//-------------------------------------------------------------------------
// Access the initial environment: names
//------------------------------------------------------------------------- 

[<AutoOpen>]
module FSharpLib = 

    let CoreOperatorsName        = FSharpLib.Root ^ ".Core.Operators"
    let CoreOperatorsCheckedName = FSharpLib.Root ^ ".Core.Operators.Checked"
    let ControlName              = FSharpLib.Root ^ ".Control"
    let CollectionsName          = FSharpLib.Root ^ ".Collections"
    let LanguagePrimitivesName   = FSharpLib.Root ^ ".Core.LanguagePrimitives"
    let CompilerServicesName     = FSharpLib.Root ^ ".Core.CompilerServices"
    let RuntimeHelpersName       = FSharpLib.Root ^ ".Core.CompilerServices.RuntimeHelpers"
    let ExtraTopLevelOperatorsName = FSharpLib.Root ^ ".Core.ExtraTopLevelOperators" 
    let HashCompareName            = FSharpLib.Root ^ ".Core.LanguagePrimitives.HashCompare"

    let QuotationsName             = FSharpLib.Root ^ ".Quotations"

    let OperatorsPath               = IL.splitNamespace CoreOperatorsName |> Array.ofList
    let OperatorsCheckedPath        = IL.splitNamespace CoreOperatorsCheckedName |> Array.ofList
    let ControlPath                 = IL.splitNamespace ControlName 
    let CollectionsPath             = IL.splitNamespace CollectionsName 
    let LanguagePrimitivesPath      = IL.splitNamespace LanguagePrimitivesName |> Array.ofList
    let HashComparePath             = IL.splitNamespace HashCompareName |> Array.ofList
    let CompilerServicesPath        = IL.splitNamespace CompilerServicesName |> Array.ofList
    let RuntimeHelpersPath          = IL.splitNamespace RuntimeHelpersName |> Array.ofList
    let QuotationsPath              = IL.splitNamespace QuotationsName |> Array.ofList
    let ExtraTopLevelOperatorsPath  = IL.splitNamespace ExtraTopLevelOperatorsName |> Array.ofList

    let RootPathArray                    = FSharpLib.RootPath |> Array.ofList
    let CorePathArray                    = FSharpLib.CorePath |> Array.ofList
    let ControlPathArray                 = ControlPath |> Array.ofList
    let CollectionsPathArray             = CollectionsPath |> Array.ofList

//-------------------------------------------------------------------------
// Access the initial environment: helpers to build references
//-------------------------------------------------------------------------

let private mkNonGenericTy tcref = TType_app(tcref,[])

let mk_MF_nleref                    ccu = mkNonLocalEntityRef ccu FSharpLib.RootPathArray
let mk_MFCore_nleref                ccu = mkNonLocalEntityRef ccu FSharpLib.CorePathArray
let mk_MFControl_nleref             ccu = mkNonLocalEntityRef ccu FSharpLib.ControlPathArray
let mk_MFCollections_nleref         ccu = mkNonLocalEntityRef ccu FSharpLib.CollectionsPathArray
let mk_MFLanguagePrimitives_nleref  ccu = mkNonLocalEntityRef ccu FSharpLib.LanguagePrimitivesPath
let mk_MFCompilerServices_nleref  ccu = mkNonLocalEntityRef ccu FSharpLib.CompilerServicesPath

let mk_nonlocal_top_nleref_tcref ccu path n = mkNonLocalTyconRef (mkNonLocalEntityRef ccu path) n 

let mk_MFCore_tcref                      ccu n = mk_nonlocal_top_nleref_tcref ccu FSharpLib.CorePathArray n 
let mk_MFQuotations_tcref                ccu n = mk_nonlocal_top_nleref_tcref ccu FSharpLib.QuotationsPath n 
let mk_MFCollections_tcref               ccu n = mk_nonlocal_top_nleref_tcref ccu FSharpLib.CollectionsPathArray n 
let mk_MFCompilerServices_tcref ccu n = mk_nonlocal_top_nleref_tcref ccu FSharpLib.CompilerServicesPath n 
let mk_MFRuntimeHelpers_tcref ccu n = mk_nonlocal_top_nleref_tcref ccu FSharpLib.RuntimeHelpersPath n 
let mk_MFControl_tcref                   ccu n = mkNonLocalTyconRef (mk_MFControl_nleref ccu) n 

let mkKnownTyconRef ccu p = 
    let a,b = splitTypeName p 
    mk_nonlocal_top_nleref_tcref ccu (Array.ofList a) b

let mkKnownNonGenericTy ccu n = mkNonGenericTy(mkKnownTyconRef ccu n)

type public BuiltinAttribInfo =
    | AttribInfo of ILTypeRef * TyconRef 
    member this.TyconRef = let (AttribInfo(_,tcref)) = this in tcref
    member this.TypeRef  = let (AttribInfo(tref,_)) = this in tref

//-------------------------------------------------------------------------
// Table of all these "globals"
//------------------------------------------------------------------------- 

[<NoEquality; NoComparison>]
type public TcGlobals = 
    { ilg : ILGlobals;
      ilxPubCloEnv : EraseIlxFuncs.cenv;
      compilingFslib: bool;
      mlCompatibility : bool;
      directoryToResolveRelativePaths : string;
      fslibCcu: CcuThunk; 
      sysCcu: CcuThunk; 
      using40environment: bool;
      indirectCallArrayMethods: bool;
      better_tcref_map: TyconRef -> TypeInst -> TType option;
      refcell_tcr_canon: TyconRef;
      option_tcr_canon : TyconRef;
      choice2_tcr : TyconRef;
      choice3_tcr : TyconRef;
      choice4_tcr : TyconRef;
      choice5_tcr : TyconRef;
      choice6_tcr : TyconRef;
      choice7_tcr : TyconRef;
      list_tcr_canon   : TyconRef;
      set_tcr_canon   : TyconRef;
      map_tcr_canon   : TyconRef;
      lazy_tcr_canon   : TyconRef; 
      
      // These have a slightly different behaviour when compiling GetFSharpCoreLibraryName 
      // hence they are 'methods' on the TcGlobals structure. 

      ucref_eq : UnionCaseRef -> UnionCaseRef -> bool;
      valRefEq  : ValRef         -> ValRef         -> bool;

      refcell_tcr_nice: TyconRef;
      option_tcr_nice : TyconRef;
      list_tcr_nice   : TyconRef; 
      lazy_tcr_nice   : TyconRef; 

      format_tcr      : TyconRef;
      expr_tcr        : TyconRef;
      raw_expr_tcr        : TyconRef;
      int_tcr         : TyconRef; 
      nativeint_tcr   : TyconRef; 
      unativeint_tcr  : TyconRef;
      int32_tcr       : TyconRef;
      int16_tcr       : TyconRef;
      int64_tcr       : TyconRef;
      uint16_tcr      : TyconRef;
      uint32_tcr      : TyconRef;
      uint64_tcr      : TyconRef;
      sbyte_tcr       : TyconRef;
      decimal_tcr     : TyconRef;
      date_tcr        : TyconRef;
      pdecimal_tcr    : TyconRef;
      byte_tcr        : TyconRef;
      bool_tcr        : TyconRef;
      string_tcr      : TyconRef;
      obj_tcr         : TyconRef;
      unit_tcr_canon  : TyconRef;
      unit_tcr_nice   : TyconRef;
      exn_tcr         : TyconRef;
      char_tcr        : TyconRef;
      float_tcr       : TyconRef;
      float32_tcr     : TyconRef;
      pfloat_tcr      : TyconRef;
      pfloat32_tcr    : TyconRef;
      pint_tcr        : TyconRef;
      pint8_tcr       : TyconRef;
      pint16_tcr      : TyconRef;
      pint64_tcr      : TyconRef;
      byref_tcr       : TyconRef;
      nativeptr_tcr   : TyconRef;
      ilsigptr_tcr    : TyconRef;
      fastFunc_tcr    : TyconRef;
      array_tcr_nice  : TyconRef;
      seq_tcr         : TyconRef;
      seq_base_tcr    : TyconRef;
      il_arr1_tcr     : TyconRef;
      il_arr2_tcr     : TyconRef;
      il_arr3_tcr     : TyconRef;
      il_arr4_tcr     : TyconRef;
      tuple1_tcr      : TyconRef;
      tuple2_tcr      : TyconRef;
      tuple3_tcr      : TyconRef;
      tuple4_tcr      : TyconRef;
      tuple5_tcr      : TyconRef;
      tuple6_tcr      : TyconRef;
      tuple7_tcr      : TyconRef;
      tuple8_tcr      : TyconRef;

      tcref_IObservable         : TyconRef;
      tcref_IObserver         : TyconRef;
      fslib_IEvent2_tcr         : TyconRef;
      fslib_IDelegateEvent_tcr: TyconRef;
      system_Nullable_tcref            : TyconRef; 
      system_GenericIComparable_tcref            : TyconRef; 
      system_GenericIEquatable_tcref            : TyconRef; 
      system_IndexOutOfRangeException_tcref : TyconRef;
      int_ty         : TType;
      nativeint_ty   : TType; 
      unativeint_ty  : TType; 
      int32_ty       : TType; 
      int16_ty       : TType; 
      int64_ty       : TType; 
      uint16_ty      : TType; 
      uint32_ty      : TType; 
      uint64_ty      : TType; 
      sbyte_ty       : TType; 
      byte_ty        : TType; 
      bool_ty        : TType; 
      string_ty      : TType; 
      obj_ty         : TType; 
      unit_ty        : TType; 
      exn_ty         : TType; 
      char_ty        : TType; 
      decimal_ty                   : TType; 
      float_ty                     : TType; 
      float32_ty                   : TType; 
      system_Array_typ             : TType; 
      system_Object_typ            : TType; 
      system_IDisposable_typ       : TType; 
      system_Value_typ             : TType; 
      system_Delegate_typ : TType;
      system_MulticastDelegate_typ : TType;
      system_Enum_typ              : TType; 
      system_Exception_typ         : TType; 
      system_Int32_typ             : TType; 
      system_String_typ            : TType; 
      system_Type_typ              : TType; 
      system_TypedReference_tcref    : TyconRef; 
      system_ArgIterator_tcref       : TyconRef; 
      system_SByte_tcref : TyconRef; 
      system_Int16_tcref : TyconRef; 
      system_Int32_tcref : TyconRef; 
      system_Int64_tcref : TyconRef; 
      system_IntPtr_tcref : TyconRef; 
      system_Bool_tcref : TyconRef; 
      system_Char_tcref : TyconRef; 
      system_Byte_tcref : TyconRef; 
      system_UInt16_tcref : TyconRef; 
      system_UInt32_tcref : TyconRef; 
      system_UInt64_tcref : TyconRef; 
      system_UIntPtr_tcref : TyconRef; 
      system_Single_tcref : TyconRef; 
      system_Double_tcref : TyconRef; 
      system_RuntimeArgumentHandle_tcref : TyconRef; 
      system_RuntimeTypeHandle_typ : TType;
      system_RuntimeMethodHandle_typ : TType;
      system_MarshalByRefObject_tcref : TyconRef;
      system_MarshalByRefObject_typ : TType;
      system_Reflection_MethodInfo_typ : TType;
      system_Array_tcref          : TyconRef;
      system_Object_tcref          : TyconRef;
      system_Void_tcref            : TyconRef;
      mk_IComparable_ty            : TType;
      mk_IConvertible_ty            : TType;
      mk_IFormattable_ty            : TType;
      mk_IStructuralComparable_ty : TType;
      mk_IStructuralEquatable_ty : TType;
      mk_IComparer_ty : TType;
      mk_IEqualityComparer_ty : TType;
      tcref_System_Collections_IComparer : TyconRef;
      tcref_System_Collections_IEqualityComparer : TyconRef;
      tcref_System_Collections_Generic_IEqualityComparer : TyconRef;
      tcref_System_Collections_Generic_Dictionary : TyconRef;
      tcref_System_IComparable : TyconRef;
      tcref_System_IStructuralComparable : TyconRef;
      tcref_System_IStructuralEquatable : TyconRef;
      tcref_LanguagePrimitives  : TyconRef;
      attrib_AttributeUsageAttribute   : BuiltinAttribInfo;
      attrib_ParamArrayAttribute       : BuiltinAttribInfo;
      attrib_IDispatchConstantAttribute : BuiltinAttribInfo;
      attrib_IUnknownConstantAttribute  : BuiltinAttribInfo;
      attrib_SystemObsolete            : BuiltinAttribInfo;
      attrib_DllImportAttribute        : BuiltinAttribInfo;
      attrib_CompiledNameAttribute     : BuiltinAttribInfo;
      attrib_NonSerializedAttribute    : BuiltinAttribInfo;
      attrib_AutoSerializableAttribute : BuiltinAttribInfo;
      attrib_StructLayoutAttribute     : BuiltinAttribInfo;
      attrib_TypeForwardedToAttribute     : BuiltinAttribInfo;
      attrib_ComVisibleAttribute       : BuiltinAttribInfo;
      attrib_ComImportAttribute        : BuiltinAttribInfo;
      attrib_FieldOffsetAttribute      : BuiltinAttribInfo;
      attrib_MarshalAsAttribute        : BuiltinAttribInfo;
      attrib_InAttribute               : BuiltinAttribInfo;
      attrib_OutAttribute              : BuiltinAttribInfo;
      attrib_OptionalAttribute         : BuiltinAttribInfo;
      attrib_ThreadStaticAttribute     : BuiltinAttribInfo;
      attrib_SpecialNameAttribute      : BuiltinAttribInfo;
      attrib_VolatileFieldAttribute       : BuiltinAttribInfo;
      attrib_ContextStaticAttribute    : BuiltinAttribInfo;
      attrib_FlagsAttribute            : BuiltinAttribInfo;
      attrib_DefaultMemberAttribute    : BuiltinAttribInfo;
      attrib_DebuggerDisplayAttribute  : BuiltinAttribInfo;
      attrib_DebuggerTypeProxyAttribute  : BuiltinAttribInfo;
      attrib_PreserveSigAttribute      : BuiltinAttribInfo;
      attrib_MethodImplAttribute       : BuiltinAttribInfo;
      tcref_System_Collections_Generic_IList       : TyconRef;
      tcref_System_Collections_Generic_ICollection : TyconRef;
      tcref_System_Collections_Generic_IEnumerable : TyconRef;
      tcref_System_Collections_Generic_IEnumerator : TyconRef;
      tcref_System_Attribute : TyconRef;

      attrib_RequireQualifiedAccessAttribute        : BuiltinAttribInfo; 
      attrib_EntryPointAttribute                    : BuiltinAttribInfo; 
      attrib_DefaultAugmentationAttribute           : BuiltinAttribInfo; 
      attrib_CompilerMessageAttribute            : BuiltinAttribInfo; 
      attrib_ExperimentalAttribute                  : BuiltinAttribInfo; 
      attrib_UnverifiableAttribute                  : BuiltinAttribInfo; 
      attrib_LiteralAttribute                       : BuiltinAttribInfo; 
      attrib_ConditionalAttribute                   : BuiltinAttribInfo; 
      attrib_OptionalArgumentAttribute              : BuiltinAttribInfo; 
      attrib_RequiresExplicitTypeArgumentsAttribute : BuiltinAttribInfo; 
      attrib_DefaultValueAttribute                  : BuiltinAttribInfo; 
      attrib_ClassAttribute                         : BuiltinAttribInfo; 
      attrib_InterfaceAttribute                     : BuiltinAttribInfo; 
      attrib_StructAttribute                        : BuiltinAttribInfo; 
      attrib_ReflectedDefinitionAttribute           : BuiltinAttribInfo; 
      attrib_AutoOpenAttribute                      : BuiltinAttribInfo; 
      attrib_CompilationRepresentationAttribute     : BuiltinAttribInfo; 
      attrib_CompilationArgumentCountsAttribute     : BuiltinAttribInfo; 
      attrib_CompilationMappingAttribute            : BuiltinAttribInfo; 

      attrib_CLIEventAttribute                      : BuiltinAttribInfo; 
      attrib_AllowNullLiteralAttribute              : BuiltinAttribInfo; 
      attrib_NoComparisonAttribute             : BuiltinAttribInfo; 
      attrib_NoEqualityAttribute             : BuiltinAttribInfo; 
      attrib_CustomComparisonAttribute             : BuiltinAttribInfo; 
      attrib_CustomEqualityAttribute             : BuiltinAttribInfo; 
      attrib_EqualityConditionalOnAttribute             : BuiltinAttribInfo; 
      attrib_ComparisonConditionalOnAttribute             : BuiltinAttribInfo; 
      attrib_ReferenceEqualityAttribute             : BuiltinAttribInfo; 
      attrib_StructuralEqualityAttribute            : BuiltinAttribInfo; 
      attrib_StructuralComparisonAttribute          : BuiltinAttribInfo; 
      attrib_SealedAttribute                        : BuiltinAttribInfo; 
      attrib_AbstractClassAttribute                 : BuiltinAttribInfo; 
      attrib_GeneralizableValueAttribute            : BuiltinAttribInfo;
      attrib_MeasureAttribute                       : BuiltinAttribInfo;
      attrib_MeasureableAttribute                   : BuiltinAttribInfo;
      attrib_GeneratedEstTypeAttribute              : BuiltinAttribInfo;
      attrib_NoDynamicInvocationAttribute           : BuiltinAttribInfo;
      
      attrib_SecurityAttribute                      : BuiltinAttribInfo;
      attrib_SecurityCriticalAttribute              : BuiltinAttribInfo;
      attrib_SecuritySafeCriticalAttribute          : BuiltinAttribInfo;
      
      cons_ucref : UnionCaseRef;
      nil_ucref : UnionCaseRef;
      (* These are the library values the compiler needs to know about *)
      seq_vref                  : ValRef;
      and_vref                  : ValRef;
      and2_vref                 : ValRef;
      addrof_vref               : ValRef;
      addrof2_vref              : ValRef;
      or_vref                   : ValRef;
      or2_vref                  : ValRef;
      
      // 'inner' refers to "after optimization boils away inlined functions"
      generic_equality_er_inner_vref    : ValRef;
      generic_equality_per_inner_vref : ValRef;
      generic_equality_withc_inner_vref : ValRef;
      generic_comparison_inner_vref   : ValRef;
      generic_comparison_withc_inner_vref   : ValRef;
      generic_hash_inner_vref: ValRef;
      generic_hash_withc_inner_vref : ValRef;
      reference_equality_inner_vref        : ValRef;

      compare_operator_vref   : ValRef;
      equals_operator_vref   : ValRef;
      not_equals_operator_vref   : ValRef;
      less_than_operator_vref   : ValRef;
      less_than_or_equals_operator_vref   : ValRef;
      greater_than_operator_vref   : ValRef;
      greater_than_or_equals_operator_vref   : ValRef;

      bitwise_or_vref           : ValRef;
      bitwise_and_vref          : ValRef;
      bitwise_xor_vref          : ValRef;
      bitwise_unary_not_vref    : ValRef;
      bitwise_shift_left_vref    : ValRef;
      bitwise_shift_right_vref    : ValRef;
      unchecked_addition_vref   : ValRef;
      unchecked_unary_plus_vref   : ValRef;
      unchecked_unary_minus_vref   : ValRef;
      unchecked_unary_not_vref   : ValRef;
      unchecked_subtraction_vref : ValRef;
      unchecked_multiply_vref    : ValRef;
      unchecked_defaultof_vref    : ValRef;

      seq_info                  : IntrinsicValRef;
      reraise_info              : IntrinsicValRef;
      reraise_vref              : ValRef;      
      typeof_info               : IntrinsicValRef;
      typeof_vref               : ValRef;
      sizeof_vref               : ValRef;
      typedefof_info            : IntrinsicValRef;
      typedefof_vref            : ValRef;
      enum_vref                 : ValRef;
      new_decimal_info          : IntrinsicValRef;
      
      // 'outer' refers to 'before optimization has boiled away inlined functions'
      // Augmentation generation generates calls to these functions
      // Optimization generates calls to these functions
      generic_comparison_withc_outer_info : IntrinsicValRef;
      generic_equality_er_outer_info    : IntrinsicValRef;
      generic_equality_withc_outer_info : IntrinsicValRef;
      generic_hash_withc_outer_info : IntrinsicValRef;

      // Augmentation generation and pattern match compilation generates calls to this function
      equals_operator_info    : IntrinsicValRef;
      
      generic_hash_withc_tuple2_vref : ValRef;
      generic_hash_withc_tuple3_vref : ValRef;
      generic_hash_withc_tuple4_vref : ValRef;
      generic_hash_withc_tuple5_vref : ValRef;
      generic_equals_withc_tuple2_vref : ValRef;
      generic_equals_withc_tuple3_vref : ValRef;
      generic_equals_withc_tuple4_vref : ValRef;
      generic_equals_withc_tuple5_vref : ValRef;
      generic_compare_withc_tuple2_vref : ValRef;
      generic_compare_withc_tuple3_vref : ValRef;
      generic_compare_withc_tuple4_vref : ValRef;
      generic_compare_withc_tuple5_vref : ValRef;
      generic_equality_withc_outer_vref : ValRef;

      create_instance_info      : IntrinsicValRef;
      create_event_info      : IntrinsicValRef;
      unbox_vref                : ValRef;
      unbox_fast_vref           : ValRef;
      istype_vref               : ValRef;
      istype_fast_vref          : ValRef;
      get_generic_comparer_info                : IntrinsicValRef;
      get_generic_er_equality_comparer_info                : IntrinsicValRef;
      get_generic_per_equality_comparer_info            : IntrinsicValRef;
      unbox_info                : IntrinsicValRef;
      unbox_fast_info           : IntrinsicValRef;
      istype_info               : IntrinsicValRef;
      istype_fast_info          : IntrinsicValRef;

      dispose_info              : IntrinsicValRef;

      range_op_vref             : ValRef;
      range_int32_op_vref       : ValRef;
      //range_step_op_vref        : ValRef;
      array_get_vref            : ValRef;
      array2D_get_vref          : ValRef;
      array3D_get_vref          : ValRef;
      array4D_get_vref          : ValRef;
      seq_collect_vref       : ValRef;
      seq_collect_info       : IntrinsicValRef;
      seq_using_info            : IntrinsicValRef;
      seq_using_vref            : ValRef;
      seq_delay_info            : IntrinsicValRef;
      seq_delay_vref            : ValRef;
      seq_append_info           : IntrinsicValRef;
      seq_append_vref           : ValRef;
      seq_generated_info        : IntrinsicValRef;
      seq_generated_vref        : ValRef;
      seq_finally_info          : IntrinsicValRef;
      seq_finally_vref          : ValRef;
      seq_of_functions_info     : IntrinsicValRef;
      seq_of_functions_vref     : ValRef;
      seq_to_array_info         : IntrinsicValRef;
      seq_to_list_info          : IntrinsicValRef;
      seq_map_info              : IntrinsicValRef;
      seq_map_vref              : ValRef;
      seq_singleton_info        : IntrinsicValRef;
      seq_singleton_vref        : ValRef;
      seq_empty_info            : IntrinsicValRef;
      seq_empty_vref            : ValRef;
      new_format_info           : IntrinsicValRef;
      raise_info                : IntrinsicValRef;
      lazy_force_info           : IntrinsicValRef;
      lazy_create_info          : IntrinsicValRef;


      array_get_info             : IntrinsicValRef;
      array2D_get_info             : IntrinsicValRef;
      array3D_get_info             : IntrinsicValRef;
      array4D_get_info             : IntrinsicValRef;
      generic_hash_info             : IntrinsicValRef;
      unpickle_quoted_info       : IntrinsicValRef;
      cast_quotation_info        : IntrinsicValRef;
      lift_value_info            : IntrinsicValRef;
      fail_init_info             : IntrinsicValRef;
      fail_static_init_info       : IntrinsicValRef;
      check_this_info            : IntrinsicValRef;
      sprintf_vref               : ValRef;
      splice_expr_vref           : ValRef;
      splice_raw_expr_vref       : ValRef;
      new_format_vref            : ValRef;

      // A list of types that are explicitly suppressed from the F# intellisense 
      // Note that the suppression checks for the precise name of the type
      // so the lowercase versions are visible
      suppressed_types           : TyconRef list;
      
      /// Memoization table to help minimize the number of ILSourceDocument objects we create
      memoize_file : int -> IL.ILSourceDocument;      
      
    } 
    override x.ToString() = "<TcGlobals>"

#if DEBUG
// This global is only used during debug output 
let global_g = ref (None : TcGlobals option)
#endif

let mkTcGlobals (compilingFslib,sysCcu,ilg,fslibCcu,directoryToResolveRelativePaths,mlCompatibility,using40environment,maybeSysCcu,lazyCcu,observerCcu,indirectCallArrayMethods) = 
  let int_tcr        = mk_MFCore_tcref fslibCcu "int"
  let nativeint_tcr  = mk_MFCore_tcref fslibCcu "nativeint"
  let unativeint_tcr = mk_MFCore_tcref fslibCcu "unativeint"
  let int32_tcr      = mk_MFCore_tcref fslibCcu "int32"
  let int16_tcr      = mk_MFCore_tcref fslibCcu "int16"
  let int64_tcr      = mk_MFCore_tcref fslibCcu "int64"
  let uint16_tcr     = mk_MFCore_tcref fslibCcu "uint16"
  let uint32_tcr     = mk_MFCore_tcref fslibCcu "uint32"
  let uint64_tcr     = mk_MFCore_tcref fslibCcu "uint64"
  let sbyte_tcr      = mk_MFCore_tcref fslibCcu "sbyte"
  let decimal_tcr    = mk_MFCore_tcref fslibCcu "decimal"
  let pdecimal_tcr   = mk_MFCore_tcref fslibCcu "decimal`1"
  let byte_tcr       = mk_MFCore_tcref fslibCcu "byte"
  let bool_tcr       = mk_MFCore_tcref fslibCcu "bool"
  let string_tcr     = mk_MFCore_tcref fslibCcu "string"
  let obj_tcr        = mk_MFCore_tcref fslibCcu "obj"
  let unit_tcr_canon = mk_MFCore_tcref fslibCcu "Unit"
  let unit_tcr_nice  = mk_MFCore_tcref fslibCcu "unit"
  let exn_tcr        = mk_MFCore_tcref fslibCcu "exn"
  let char_tcr       = mk_MFCore_tcref fslibCcu "char"
  let float_tcr      = mk_MFCore_tcref fslibCcu "float"  
  let float32_tcr    = mk_MFCore_tcref fslibCcu "float32"
  let pfloat_tcr     = mk_MFCore_tcref fslibCcu "float`1"  
  let pfloat32_tcr   = mk_MFCore_tcref fslibCcu "float32`1"  
  let pint_tcr       = mk_MFCore_tcref fslibCcu "int`1"  
  let pint8_tcr      = mk_MFCore_tcref fslibCcu "sbyte`1"  
  let pint16_tcr     = mk_MFCore_tcref fslibCcu "int16`1"  
  let pint64_tcr     = mk_MFCore_tcref fslibCcu "int64`1"  
  let byref_tcr      = mk_MFCore_tcref fslibCcu "byref`1"
  let nativeptr_tcr  = mk_MFCore_tcref fslibCcu "nativeptr`1"
  let ilsigptr_tcr   = mk_MFCore_tcref fslibCcu "ilsigptr`1"
  let fastFunc_tcr   = mk_MFCore_tcref fslibCcu "FSharpFunc`2"
  let lazy_tcr = mkKnownTyconRef lazyCcu "System.Lazy`1" 
  let fslib_IEvent2_tcr        = mk_MFControl_tcref fslibCcu "IEvent`2"
  let tcref_IObservable      =  mkKnownTyconRef observerCcu "System.IObservable`1" 
  let tcref_IObserver        =  mkKnownTyconRef observerCcu "System.IObserver`1" 
  let fslib_IDelegateEvent_tcr = mk_MFControl_tcref fslibCcu "IDelegateEvent`1"

  let option_tcr_nice     = mk_MFCore_tcref fslibCcu "option`1"
  let list_tcr_canon        = mk_MFCollections_tcref fslibCcu "List`1"
  let list_tcr_nice            = mk_MFCollections_tcref fslibCcu "list`1"
  let lazy_tcr_nice            = mk_MFControl_tcref fslibCcu "Lazy`1"
  let seq_tcr                  = mk_MFCollections_tcref fslibCcu "seq`1"
  let format_tcr               = mk_MFCore_tcref     fslibCcu "PrintfFormat`5" 
  let format4_tcr              = mk_MFCore_tcref     fslibCcu "PrintfFormat`4" 
  let date_tcr                 = mkKnownTyconRef sysCcu"System.DateTime"
  let IEnumerable_tcr          = mkKnownTyconRef sysCcu "System.Collections.Generic.IEnumerable`1"
  let IEnumerator_tcr          = mkKnownTyconRef sysCcu "System.Collections.Generic.IEnumerator`1"
  let System_Attribute_tcr     = mkKnownTyconRef sysCcu "System.Attribute"
  let expr_tcr                 = mk_MFQuotations_tcref fslibCcu "Expr`1" 
  let raw_expr_tcr             = mk_MFQuotations_tcref fslibCcu "Expr" 
  let il_arr1_tcr              = mk_MFCore_tcref fslibCcu "[]`1"
  let il_arr2_tcr              = mk_MFCore_tcref fslibCcu "[,]`1";
  let il_arr3_tcr              = mk_MFCore_tcref fslibCcu "[,,]`1";
  let il_arr4_tcr              = mk_MFCore_tcref fslibCcu "[,,,]`1";
  
  let bool_ty         = mkNonGenericTy bool_tcr   
  let int_ty          = mkNonGenericTy int_tcr    
  let obj_ty          = mkNonGenericTy obj_tcr    
  let string_ty       = mkNonGenericTy string_tcr
  let byte_ty         = mkNonGenericTy byte_tcr
  let decimal_ty      = mkKnownNonGenericTy sysCcu "System.Decimal"
  let unit_ty         = mkNonGenericTy unit_tcr_nice 
  let system_Type_typ = mkKnownNonGenericTy sysCcu "System.Type" 
  
  let system_Reflection_MethodInfo_typ = mkKnownNonGenericTy sysCcu "System.Reflection.MethodInfo"
  
  (* local helpers to build value infos *)
  let mkByrefTy ty = TType_app(byref_tcr, [ty]) 
  let mkNativePtrType ty = TType_app(nativeptr_tcr, [ty]) 
  let mkFunTy d r = TType_fun (d,r) 
  let (-->) d r = mkFunTy d r
  let mkIteratedFunTy dl r = List.foldBack (-->) dl r
  let mk_small_tupled_ty l = match l with [] -> unit_ty | [h] -> h | tys -> TType_tuple tys
  let tryMkForallTy d r = match d with [] -> r | tps -> TType_forall(tps,r)

  let MakeIntrinsicValRef (mvr,nm,memberParentName,typars,(argtys,rty))  =
      let ty = tryMkForallTy typars (mkIteratedFunTy (List.map mk_small_tupled_ty argtys) rty)
      let isMember = isSome memberParentName
      let argCount = if isMember then List.sum (List.map List.length argtys) else 0
      let linkageType = if isMember then Some ty else None
      let key = ValLinkageFullKey({ MemberParentMangledName=memberParentName; MemberIsOverride=false; LogicalName=nm; TotalArgCount= argCount },linkageType)
      IntrinsicValRef(mvr,nm,isMember,ty,key)

  let mk_IComparer_ty = mkKnownNonGenericTy sysCcu "System.Collections.IComparer";
  let mk_IEqualityComparer_ty = mkKnownNonGenericTy sysCcu "System.Collections.IEqualityComparer";

  let mk_unop_ty ty                   = [[ty]], ty
  let mk_binop_ty ty                   = [[ty]; [ty]], ty
  let mk_shiftop_ty ty                   = [[ty]; [int_ty]], ty
  let mk_binop_ty3 ty1 ty2 ty3                   = [[ty1]; [ty2]], ty3
  let mk_rel_sig ty                     = [[ty];[ty]],bool_ty
  let mk_compare_sig ty                = [[ty];[ty]],int_ty
  let mk_hash_sig ty                   = [[ty]], int_ty
  let mk_compare_withc_sig  ty = [[mk_IComparer_ty];[ty]; [ty]], int_ty
  let mk_equality_withc_sig ty = [[mk_IEqualityComparer_ty];[ty];[ty]], bool_ty
  let mk_hash_withc_sig     ty = [[mk_IEqualityComparer_ty]; [ty]], int_ty
  let mkListTy ty         = TType_app(list_tcr_nice,[ty])
  let mkSeqTy ty1         = TType_app(seq_tcr,[ty1])
  let mkArrayType ty       = TType_app(il_arr1_tcr, [ty])
  let mk_array2_typ ty      = TType_app(il_arr2_tcr, [ty])
  let mk_array3_typ ty      = TType_app(il_arr3_tcr, [ty])
  let mk_array4_typ ty      = TType_app(il_arr4_tcr, [ty])
  let mkLazyTy ty         = TType_app(lazy_tcr, [ty])
  
  let mkPrintfFormatTy aty bty cty dty ety = TType_app(format_tcr, [aty;bty;cty;dty; ety]) 
  let mk_format4_ty aty bty cty dty = TType_app(format4_tcr, [aty;bty;cty;dty]) 
  let mkQuotedExprTy aty = TType_app(expr_tcr, [aty]) 
  let mkRawQuotedExprTy = TType_app(raw_expr_tcr, []) 
  let cons_ucref = mkUnionCaseRef list_tcr_canon "op_ColonColon" 
  let nil_ucref  = mkUnionCaseRef list_tcr_canon "op_Nil" 

  
  (* value infos *)
  let fslib_MFCore_nleref        = mk_MFCore_nleref fslibCcu
  let fslib_MFLanguagePrimitives_nleref        = mkNestedNonLocalEntityRef fslib_MFCore_nleref "LanguagePrimitives"
  let fslib_MFIntrinsicOperators_nleref        = mkNestedNonLocalEntityRef (mk_MFLanguagePrimitives_nleref fslibCcu) "IntrinsicOperators" 
  let fslib_MFIntrinsicFunctions_nleref        = mkNestedNonLocalEntityRef (mk_MFLanguagePrimitives_nleref fslibCcu) "IntrinsicFunctions" 
  let fslib_MFHashCompare_nleref               = mkNestedNonLocalEntityRef (mk_MFLanguagePrimitives_nleref fslibCcu) "HashCompare"
  let fslib_MFOperators_nleref                 = mkNestedNonLocalEntityRef fslib_MFCore_nleref "Operators"
  let fslib_MFOperatorIntrinsics_nleref        = mkNestedNonLocalEntityRef fslib_MFOperators_nleref "OperatorIntrinsics"
  let fslib_MFOperatorsUnchecked_nleref        = mkNestedNonLocalEntityRef fslib_MFOperators_nleref "Unchecked"
  let fslib_MFExtraTopLevelOperators_nleref    = mkNestedNonLocalEntityRef fslib_MFCore_nleref "ExtraTopLevelOperators"

  let fslib_MFSeqModule_nleref                 = mkNestedNonLocalEntityRef (mk_MFCollections_nleref fslibCcu) "SeqModule"
  let fslib_MFRuntimeHelpers_nleref            = mkNestedNonLocalEntityRef (mk_MFCompilerServices_nleref fslibCcu) "RuntimeHelpers"
  let fslib_MFQuotations_nleref                = mkNestedNonLocalEntityRef (mk_MF_nleref fslibCcu) "Quotations"
  
  let fslib_MFLazyExtensions_nleref            = mkNestedNonLocalEntityRef (mk_MFControl_nleref fslibCcu) "LazyExtensions" 

  let tuple1_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`1" 
  let tuple2_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`2" 
  let tuple3_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`3" 
  let tuple4_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`4" 
  let tuple5_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`5" 
  let tuple6_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`6" 
  let tuple7_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`7" 
  let tuple8_tcr      = mkKnownTyconRef maybeSysCcu "System.Tuple`8" 
  
  let choice2_tcr     = mk_MFCore_tcref fslibCcu "Choice`2" 
  let choice3_tcr     = mk_MFCore_tcref fslibCcu "Choice`3" 
  let choice4_tcr     = mk_MFCore_tcref fslibCcu "Choice`4" 
  let choice5_tcr     = mk_MFCore_tcref fslibCcu "Choice`5" 
  let choice6_tcr     = mk_MFCore_tcref fslibCcu "Choice`6" 
  let choice7_tcr     = mk_MFCore_tcref fslibCcu "Choice`7" 
  let tyconRefEq x y = primEntityRefEq compilingFslib fslibCcu  x y
  let valRefEq  x y = primValRefEq compilingFslib fslibCcu x y
  let ucref_eq x y = primUnionCaseRefEq compilingFslib fslibCcu x y
  (* let modref_eq = prim_modref_eq compilingFslib fslibCcu in  *)

  let suppressed_types = 
    [ mk_MFCore_tcref fslibCcu "Option`1";
      mk_MFCore_tcref fslibCcu "Ref`1"; 
      mk_MFCore_tcref fslibCcu "FSharpTypeFunc";
      mk_MFCore_tcref fslibCcu "FSharpFunc`2"; 
      mk_MFCore_tcref fslibCcu "Unit" ] 

  let decode_tuple_ty l = 
      match l with 
      | [t1;t2;t3;t4;t5;t6;t7;marker] -> 
          match marker with 
          | TType_app(tcref,[t8]) when tyconRefEq tcref tuple1_tcr -> TType_tuple [t1;t2;t3;t4;t5;t6;t7;t8]
          | TType_app(_,[TType_tuple t8plus]) -> 
              TType_tuple ([t1;t2;t3;t4;t5;t6;t7] @ t8plus)
          | _ -> TType_tuple l 
      | _ -> TType_tuple l 
      

  let mk_MFCore_attrib nm : BuiltinAttribInfo = 
      AttribInfo(mkILTyRef(IlxSettings.ilxFsharpCoreLibScopeRef (), nm),mk_MFCore_tcref fslibCcu nm) 

  let mkMscorlibAttrib nm : BuiltinAttribInfo = 
      AttribInfo(mkILTyRef (ilg.mscorlibScopeRef,nm), mkKnownTyconRef sysCcu nm)

  let mk_doc filename = ILSourceDocument.Create(language=None, vendor=None, documentType=None, file=filename)
  // Build the memoization table for files
  let memoize_file = new MemoizationTable<int,ILSourceDocument> ((fileOfFileIndex >> Filename.fullpath directoryToResolveRelativePaths >> mk_doc), keyComparer=HashIdentity.Structural)

  //let unary_neg_sinfo =            fslib_MFIntrinsicOperators_nleref, (CompileOpName "~-")        
  let and_info =                   MakeIntrinsicValRef(fslib_MFIntrinsicOperators_nleref,                    CompileOpName "&"                      ,None,[],         mk_rel_sig bool_ty) 
  let addrof_info =                MakeIntrinsicValRef(fslib_MFIntrinsicOperators_nleref,                    CompileOpName "~&"                     ,None,[vara],     ([[varaTy]], mkByrefTy varaTy))   
  let addrof2_info =               MakeIntrinsicValRef(fslib_MFIntrinsicOperators_nleref,                    CompileOpName "~&&"                    ,None,[vara],     ([[varaTy]], mkNativePtrType varaTy))
  let and2_info =                  MakeIntrinsicValRef(fslib_MFIntrinsicOperators_nleref,                    CompileOpName "&&"                     ,None,[],         mk_rel_sig bool_ty) 
  let or_info =                    MakeIntrinsicValRef(fslib_MFIntrinsicOperators_nleref,                    "or"                                   ,None,[],         mk_rel_sig bool_ty) 
  let or2_info =                   MakeIntrinsicValRef(fslib_MFIntrinsicOperators_nleref,                    CompileOpName "||"                     ,None,[],         mk_rel_sig bool_ty) 

  let compare_operator_info                = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "compare"                              ,None,[vara],     mk_compare_sig varaTy) 
  let equals_operator_info                 = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "op_Equality"                          ,None,[vara],     mk_rel_sig varaTy) 
  let not_equals_operator_info             = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "op_Inequality"                        ,None,[vara],     mk_rel_sig varaTy) 
  let less_than_operator_info              = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "op_LessThan"                          ,None,[vara],     mk_rel_sig varaTy) 
  let less_than_or_equals_operator_info    = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "op_LessThanOrEqual"                   ,None,[vara],     mk_rel_sig varaTy) 
  let greater_than_operator_info           = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "op_GreaterThan"                       ,None,[vara],     mk_rel_sig varaTy) 
  let greater_than_or_equals_operator_info = MakeIntrinsicValRef(fslib_MFOperators_nleref,                   "op_GreaterThanOrEqual"                ,None,[vara],     mk_rel_sig varaTy) 

  let generic_comparison_withc_outer_info = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,           "GenericComparisonWithComparer"        ,None,[vara],     mk_compare_withc_sig  varaTy) 


  let generic_hash_withc_tuple2_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastHashTuple2"                                   ,None,[vara;varb],               mk_hash_withc_sig (decode_tuple_ty [varaTy; varbTy]))   
  let generic_hash_withc_tuple3_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastHashTuple3"                                   ,None,[vara;varb;varc],          mk_hash_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy]))   
  let generic_hash_withc_tuple4_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastHashTuple4"                                   ,None,[vara;varb;varc;vard],     mk_hash_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy; vardTy]))   
  let generic_hash_withc_tuple5_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastHashTuple5"                                   ,None,[vara;varb;varc;vard;vare],mk_hash_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy; vardTy; vareTy]))   

  let generic_equals_withc_tuple2_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastEqualsTuple2"                               ,None,[vara;varb],               mk_equality_withc_sig (decode_tuple_ty [varaTy; varbTy]))   
  let generic_equals_withc_tuple3_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastEqualsTuple3"                               ,None,[vara;varb;varc],          mk_equality_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy]))   
  let generic_equals_withc_tuple4_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastEqualsTuple4"                               ,None,[vara;varb;varc;vard],     mk_equality_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy; vardTy]))   
  let generic_equals_withc_tuple5_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastEqualsTuple5"                               ,None,[vara;varb;varc;vard;vare],mk_equality_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy; vardTy; vareTy]))   

  let generic_compare_withc_tuple2_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastCompareTuple2"                             ,None,[vara;varb],               mk_compare_withc_sig (decode_tuple_ty [varaTy; varbTy]))   
  let generic_compare_withc_tuple3_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastCompareTuple3"                             ,None,[vara;varb;varc],          mk_compare_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy]))   
  let generic_compare_withc_tuple4_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastCompareTuple4"                             ,None,[vara;varb;varc;vard],     mk_compare_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy; vardTy]))   
  let generic_compare_withc_tuple5_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,           "FastCompareTuple5"                             ,None,[vara;varb;varc;vard;vare],mk_compare_withc_sig (decode_tuple_ty [varaTy; varbTy; varcTy; vardTy; vareTy]))   


  let generic_equality_er_outer_info             = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,    "GenericEqualityER"                      ,None,[vara],     mk_rel_sig varaTy) 
  let get_generic_comparer_info               = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,       "GenericComparer"                      ,None,[],         ([], mk_IComparer_ty)) 
  let get_generic_er_equality_comparer_info      = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,    "GenericEqualityERComparer"              ,None,[],         ([], mk_IEqualityComparer_ty)) 
  let get_generic_per_equality_comparer_info  = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,       "GenericEqualityComparer"              ,None,[],         ([], mk_IEqualityComparer_ty)) 
  let generic_equality_withc_outer_info       = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,       "GenericEqualityWithComparer"          ,None,[vara],     mk_equality_withc_sig varaTy)
  let generic_hash_withc_outer_info           = MakeIntrinsicValRef(fslib_MFLanguagePrimitives_nleref,       "GenericHashWithComparer"              ,None,[vara],     mk_hash_withc_sig varaTy)

  let generic_equality_er_inner_info         = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,               "GenericEqualityERIntrinsic"             ,None,[vara],     mk_rel_sig varaTy)
  let generic_equality_per_inner_info     = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                  "GenericEqualityIntrinsic"          ,None,[vara],     mk_rel_sig varaTy)
  let generic_equality_withc_inner_info   = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                  "GenericEqualityWithComparerIntrinsic" ,None,[vara],     mk_equality_withc_sig varaTy)
  let generic_comparison_inner_info       = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                  "GenericComparisonIntrinsic"           ,None,[vara],     mk_compare_sig varaTy)
  let generic_comparison_withc_inner_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                  "GenericComparisonWithComparerIntrinsic",None,[vara],    mk_compare_withc_sig varaTy)

  let generic_hash_inner_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                              "GenericHashIntrinsic"                 ,None,[vara],     mk_hash_sig varaTy)
  let generic_hash_withc_inner_info = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                        "GenericHashWithComparerIntrinsic"     ,None,[vara],     mk_hash_withc_sig  varaTy)
  
  let create_instance_info       = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "CreateInstance"                       ,None,[vara],     ([[unit_ty]], varaTy))
  let unbox_info                 = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "UnboxGeneric"                         ,None,[vara],     ([[obj_ty]], varaTy))

  let unbox_fast_info            = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "UnboxFast"                            ,None,[vara],     ([[obj_ty]], varaTy))
  let istype_info                = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "TypeTestGeneric"                      ,None,[vara],     ([[obj_ty]], bool_ty)) 
  let istype_fast_info           = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "TypeTestFast"                         ,None,[vara],     ([[obj_ty]], bool_ty)) 

  let dispose_info               = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "Dispose"                              ,None,[vara],     ([[varaTy]],unit_ty))

  let reference_equality_inner_info         = MakeIntrinsicValRef(fslib_MFHashCompare_nleref,                           "PhysicalEqualityIntrinsic"            ,None,[vara],     mk_rel_sig varaTy)  

  let bitwise_or_info            = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_BitwiseOr"                         ,None,[vara],     mk_binop_ty varaTy)  
  let bitwise_and_info           = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_BitwiseAnd"                        ,None,[vara],     mk_binop_ty varaTy)  
  let bitwise_xor_info           = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_ExclusiveOr"                       ,None,[vara],     mk_binop_ty varaTy)  
  let bitwise_unary_not_info     = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_LogicalNot"                        ,None,[vara],     mk_unop_ty varaTy)  
  let bitwise_shift_left_info    = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_LeftShift"                         ,None,[vara],     mk_shiftop_ty varaTy)  
  let bitwise_shift_right_info   = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_RightShift"                        ,None,[vara],     mk_shiftop_ty varaTy)  
  let unchecked_addition_info    = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_Addition"                          ,None,[vara;varb;varc],     mk_binop_ty3 varaTy varbTy  varcTy)  
  let unchecked_subtraction_info = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_Subtraction"                       ,None,[vara;varb;varc],     mk_binop_ty3 varaTy varbTy  varcTy)  
  let unchecked_multiply_info    = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_Multiply"                          ,None,[vara;varb;varc],     mk_binop_ty3 varaTy varbTy  varcTy)  
  let unchecked_unary_plus_info  = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_UnaryPlus"                         ,None,[vara],     mk_unop_ty varaTy)  
  let unchecked_unary_minus_info = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_UnaryNegation"                     ,None,[vara],     mk_unop_ty varaTy)  
  let unchecked_unary_not_info   = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "not"                                  ,None,[],     mk_unop_ty bool_ty)  


  let raise_info                 = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "raise"                                ,None,[vara],([[mkKnownNonGenericTy sysCcu "System.Exception"]],varaTy))  
  let reraise_info               = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "reraise"                              ,None,[vara],     ([[unit_ty]],varaTy))
  let typeof_info                = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "typeof"                               ,None,[vara],     ([],system_Type_typ))  
  let sizeof_info                = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "sizeof"                               ,None,[vara],     ([],int_ty))  
  let unchecked_defaultof_info   = MakeIntrinsicValRef(fslib_MFOperatorsUnchecked_nleref,                    "defaultof"                            ,None,[vara],     ([],varaTy))  
  let typedefof_info             = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "typedefof"                            ,None,[vara],     ([],system_Type_typ))  
  let enum_info                  = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "enum"                                 ,None,[vara],     ([[int_ty]],varaTy))  
  let range_op_info              = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_Range"                             ,None,[vara],     ([[varaTy];[varaTy]],mkSeqTy varaTy))
  let range_int32_op_info        = MakeIntrinsicValRef(fslib_MFOperatorIntrinsics_nleref,                    "RangeInt32"                           ,None,[],     ([[int_ty];[int_ty];[int_ty]],mkSeqTy int_ty))
  //let range_step_op_info         = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "op_RangeStep"                             ,None,[vara;varb],     ([[varaTy];[varbTy];[varaTy]],mkSeqTy varaTy))
  let array2D_get_info           = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "GetArray2D"                           ,None,[vara],     ([[mk_array2_typ varaTy];[int_ty]; [int_ty]],varaTy))  
  let array3D_get_info           = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "GetArray3D"                           ,None,[vara],     ([[mk_array3_typ varaTy];[int_ty]; [int_ty]; [int_ty]],varaTy))
  let array4D_get_info           = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "GetArray4D"                           ,None,[vara],     ([[mk_array4_typ varaTy];[int_ty]; [int_ty]; [int_ty]; [int_ty]],varaTy))
  let seq_collect_info        = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "collect"                              ,None,[vara;varb;varc],([[varaTy --> varbTy]; [mkSeqTy varaTy]], mkSeqTy varcTy))  
  let seq_delay_info             = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "delay"                                ,None,[varb],     ([[unit_ty --> mkSeqTy varbTy]], mkSeqTy varbTy)) 
  let seq_append_info            = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "append"                               ,None,[varb],     ([[mkSeqTy varbTy]; [mkSeqTy varbTy]], mkSeqTy varbTy))  
  let seq_using_info             = MakeIntrinsicValRef(fslib_MFRuntimeHelpers_nleref,                        "EnumerateUsing"                       ,None,[vara;varb;varc], ([[varaTy];[(varaTy --> varbTy)]],mkSeqTy varcTy))
  let seq_generated_info         = MakeIntrinsicValRef(fslib_MFRuntimeHelpers_nleref,                        "EnumerateWhile"                       ,None,[varb],     ([[unit_ty --> bool_ty]; [mkSeqTy varbTy]], mkSeqTy varbTy))
  let seq_finally_info           = MakeIntrinsicValRef(fslib_MFRuntimeHelpers_nleref,                        "EnumerateThenFinally"                 ,None,[varb],     ([[mkSeqTy varbTy]; [unit_ty --> unit_ty]], mkSeqTy varbTy))
  let seq_of_functions_info      = MakeIntrinsicValRef(fslib_MFRuntimeHelpers_nleref,                        "EnumerateFromFunctions"               ,None,[vara;varb],([[unit_ty --> varaTy]; [varaTy --> bool_ty]; [varaTy --> varbTy]], mkSeqTy varbTy))  
  let create_event_info          = MakeIntrinsicValRef(fslib_MFRuntimeHelpers_nleref,                        "CreateEvent"                          ,None,[vara;varb],     ([[varaTy --> unit_ty]; [varaTy --> unit_ty]; [(obj_ty --> (varbTy --> unit_ty)) --> varaTy]], TType_app (fslib_IEvent2_tcr, [varaTy;varbTy])))
  let seq_to_array_info          = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "toArray"                              ,None,[varb],     ([[mkSeqTy varbTy]], mkArrayType varbTy))  
  let seq_to_list_info           = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "toList"                               ,None,[varb],     ([[mkSeqTy varbTy]], mkListTy varbTy))
  let seq_map_info               = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "map"                                  ,None,[vara;varb],([[varaTy --> varbTy]; [mkSeqTy varaTy]], mkSeqTy varbTy))
  let seq_singleton_info         = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "singleton"                            ,None,[vara],     ([[varaTy]], mkSeqTy varaTy))
  let seq_empty_info             = MakeIntrinsicValRef(fslib_MFSeqModule_nleref,                             "empty"                                ,None,[vara],     ([], mkSeqTy varaTy))
  let new_format_info            = MakeIntrinsicValRef(fslib_MFCore_nleref,                                  ".ctor"                                ,Some "PrintfFormat`5" ,[vara;varb;varc;vard;vare], ([[string_ty]], mkPrintfFormatTy varaTy varbTy varcTy vardTy vareTy))  
  let sprintf_info               = MakeIntrinsicValRef(fslib_MFExtraTopLevelOperators_nleref,                "sprintf"                              ,None,[vara],     ([[mk_format4_ty varaTy unit_ty string_ty string_ty]], varaTy))  
  let lazy_force_info            = 
    // Lazy\Value for > 4.0
                                   MakeIntrinsicValRef(fslib_MFLazyExtensions_nleref,                        "Force"                         ,Some "Lazy`1" ,  [vara],      ([[mkLazyTy varaTy]; []], varaTy))
  let lazy_create_info           = 
                                   MakeIntrinsicValRef(fslib_MFLazyExtensions_nleref,                        "Create"                 ,Some "Lazy`1" ,  [vara],      ([[unit_ty --> varaTy]], mkLazyTy varaTy))
  let seq_info                   = MakeIntrinsicValRef(fslib_MFOperators_nleref,                             "seq"                                  ,None,[vara],     ([[mkSeqTy varaTy]], mkSeqTy varaTy))
  let splice_expr_info           = MakeIntrinsicValRef(fslib_MFExtraTopLevelOperators_nleref,                "op_Splice"                            ,None,[vara],     ([[mkQuotedExprTy varaTy]], varaTy))
  let splice_raw_expr_info       = MakeIntrinsicValRef(fslib_MFExtraTopLevelOperators_nleref,                "op_SpliceUntyped"                     ,None,[vara],     ([[mkRawQuotedExprTy]], varaTy))
  let new_decimal_info           = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "MakeDecimal"                          ,None,[],         ([[int_ty]; [int_ty]; [int_ty]; [bool_ty]; [byte_ty]], decimal_ty))
  let array_get_info             = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "GetArray"                             ,None,[vara],     ([[mkArrayType varaTy]; [int_ty]], varaTy))
  let generic_hash_info          = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "StructuralHash"                       ,None,[vara],     ([[varaTy]], int_ty)) 
  let unpickle_quoted_info       = MakeIntrinsicValRef(fslib_MFQuotations_nleref,                            "Deserialize"                          ,Some "Expr" ,[],          ([[system_Type_typ ;mkListTy system_Type_typ ;mkListTy mkRawQuotedExprTy ; mkArrayType byte_ty]], mkRawQuotedExprTy ))
  let cast_quotation_info        = MakeIntrinsicValRef(fslib_MFQuotations_nleref,                            "Cast"                                 ,Some "Expr",[vara],      ([[mkRawQuotedExprTy]], mkQuotedExprTy varaTy))
  let lift_value_info            = MakeIntrinsicValRef(fslib_MFQuotations_nleref,                            "Value"                                ,Some "Expr",[vara],      ([[varaTy]], mkRawQuotedExprTy))
  let fail_init_info             = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "FailInit"                             ,None,[],      ([[unit_ty]], unit_ty))
  let fail_static_init_info      = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "FailStaticInit"                       ,None,[],      ([[unit_ty]], unit_ty))
  let check_this_info            = MakeIntrinsicValRef(fslib_MFIntrinsicFunctions_nleref,                    "CheckThis"                            ,None,[vara],      ([[varaTy]], varaTy))
    
  { ilg=ilg;
    ilxPubCloEnv=EraseIlxFuncs.new_cenv(ilg);
    compilingFslib=compilingFslib;
    mlCompatibility=mlCompatibility;
    directoryToResolveRelativePaths=directoryToResolveRelativePaths;
    (* modref_eq = modref_eq; *)
    ucref_eq = ucref_eq;
    valRefEq = valRefEq;
    fslibCcu       = fslibCcu;
    using40environment = using40environment;
    indirectCallArrayMethods = indirectCallArrayMethods;
    sysCcu         = sysCcu;
    refcell_tcr_canon       = mk_MFCore_tcref     fslibCcu "Ref`1";
    option_tcr_canon        = mk_MFCore_tcref     fslibCcu "Option`1";
    list_tcr_canon    = mk_MFCollections_tcref   fslibCcu "List`1";
    set_tcr_canon    = mk_MFCollections_tcref   fslibCcu "Set`1";
    map_tcr_canon    = mk_MFCollections_tcref   fslibCcu "Map`2";
    lazy_tcr_canon    = lazy_tcr;
    refcell_tcr_nice  = mk_MFCore_tcref     fslibCcu "ref`1";
    array_tcr_nice  = il_arr1_tcr;
    option_tcr_nice   = option_tcr_nice;
    list_tcr_nice     = list_tcr_nice;
    lazy_tcr_nice     = lazy_tcr_nice;
    format_tcr       = format_tcr;
    expr_tcr       = expr_tcr;
    raw_expr_tcr       = raw_expr_tcr;
    int_tcr        = int_tcr;
    nativeint_tcr  = nativeint_tcr;
    unativeint_tcr = unativeint_tcr;
    int32_tcr      = int32_tcr;
    int16_tcr      = int16_tcr;
    int64_tcr      = int64_tcr;
    uint16_tcr     = uint16_tcr;
    uint32_tcr     = uint32_tcr;
    uint64_tcr     = uint64_tcr;
    sbyte_tcr      = sbyte_tcr;
    decimal_tcr    = decimal_tcr;
    date_tcr    = date_tcr;
    pdecimal_tcr   = pdecimal_tcr;
    byte_tcr       = byte_tcr;
    bool_tcr       = bool_tcr;
    string_tcr     = string_tcr;
    obj_tcr        = obj_tcr;
    unit_tcr_canon = unit_tcr_canon;
    unit_tcr_nice  = unit_tcr_nice;
    exn_tcr        = exn_tcr;
    char_tcr       = char_tcr;
    float_tcr      = float_tcr;
    float32_tcr    = float32_tcr;
    pfloat_tcr     = pfloat_tcr;
    pfloat32_tcr   = pfloat32_tcr;
    pint_tcr       = pint_tcr;
    pint8_tcr      = pint8_tcr;
    pint16_tcr     = pint16_tcr;
    pint64_tcr     = pint64_tcr;
    byref_tcr      = byref_tcr;
    nativeptr_tcr  = nativeptr_tcr;
    ilsigptr_tcr   = ilsigptr_tcr;
    fastFunc_tcr = fastFunc_tcr;
    tcref_IObservable      = tcref_IObservable;
    tcref_IObserver      = tcref_IObserver;
    fslib_IEvent2_tcr      = fslib_IEvent2_tcr;
    fslib_IDelegateEvent_tcr      = fslib_IDelegateEvent_tcr;
    seq_tcr        = seq_tcr;
    seq_base_tcr = mk_MFCompilerServices_tcref fslibCcu "GeneratedSequenceBase`1";
    il_arr1_tcr    = il_arr1_tcr;
    il_arr2_tcr    = il_arr2_tcr;
    il_arr3_tcr    = il_arr3_tcr;
    il_arr4_tcr    = il_arr4_tcr;
    tuple1_tcr     = tuple1_tcr;
    tuple2_tcr     = tuple2_tcr;
    tuple3_tcr     = tuple3_tcr;
    tuple4_tcr     = tuple4_tcr;
    tuple5_tcr     = tuple5_tcr;
    tuple6_tcr     = tuple6_tcr;
    tuple7_tcr     = tuple7_tcr;
    tuple8_tcr     = tuple8_tcr;
    choice2_tcr    = choice2_tcr;
    choice3_tcr    = choice3_tcr;
    choice4_tcr    = choice4_tcr;
    choice5_tcr    = choice5_tcr;
    choice6_tcr    = choice6_tcr;
    choice7_tcr    = choice7_tcr;
    int_ty        = int_ty;
    nativeint_ty  = mkNonGenericTy nativeint_tcr;
    unativeint_ty = mkNonGenericTy unativeint_tcr;
    int32_ty      = mkNonGenericTy int32_tcr;
    int16_ty      = mkNonGenericTy int16_tcr;
    int64_ty      = mkNonGenericTy int64_tcr;
    uint16_ty     = mkNonGenericTy uint16_tcr;
    uint32_ty     = mkNonGenericTy uint32_tcr;
    uint64_ty     = mkNonGenericTy uint64_tcr;
    sbyte_ty      = mkNonGenericTy sbyte_tcr;
    byte_ty       = byte_ty;
    bool_ty       = bool_ty;
    string_ty     = string_ty;
    obj_ty        = mkNonGenericTy obj_tcr;
    unit_ty       = unit_ty;
    exn_ty        = mkNonGenericTy exn_tcr;
    char_ty       = mkNonGenericTy char_tcr;
    decimal_ty    = mkNonGenericTy decimal_tcr;
    float_ty      = mkNonGenericTy float_tcr; 
    float32_ty    = mkNonGenericTy float32_tcr;
    memoize_file  = memoize_file.Apply;

    system_Array_typ     = mkKnownNonGenericTy sysCcu "System.Array";
    system_Object_typ    = mkKnownNonGenericTy sysCcu "System.Object";
    system_IDisposable_typ    = mkKnownNonGenericTy sysCcu "System.IDisposable";
    system_Value_typ     = mkKnownNonGenericTy sysCcu "System.ValueType";
    system_Delegate_typ     = mkKnownNonGenericTy sysCcu "System.Delegate";
    system_MulticastDelegate_typ     = mkKnownNonGenericTy sysCcu "System.MulticastDelegate";
    system_Enum_typ      = mkKnownNonGenericTy sysCcu "System.Enum";
    system_Exception_typ = mkKnownNonGenericTy sysCcu "System.Exception";
    system_String_typ    = mkKnownNonGenericTy sysCcu "System.String";
    system_Int32_typ     = mkKnownNonGenericTy sysCcu "System.Int32";
    system_Type_typ                  = system_Type_typ;
    system_TypedReference_tcref        = mkKnownTyconRef sysCcu "System.TypedReference" ;
    system_ArgIterator_tcref           = mkKnownTyconRef sysCcu "System.ArgIterator" ;
    system_RuntimeArgumentHandle_tcref =  mkKnownTyconRef sysCcu "System.RuntimeArgumentHandle";
    system_SByte_tcref =  mkKnownTyconRef sysCcu "System.SByte";
    system_Int16_tcref =  mkKnownTyconRef sysCcu "System.Int16";
    system_Int32_tcref =  mkKnownTyconRef sysCcu "System.Int32";
    system_Int64_tcref =  mkKnownTyconRef sysCcu "System.Int64";
    system_IntPtr_tcref =  mkKnownTyconRef sysCcu "System.IntPtr";
    system_Bool_tcref =  mkKnownTyconRef sysCcu "System.Boolean"; 
    system_Byte_tcref =  mkKnownTyconRef sysCcu "System.Byte";
    system_UInt16_tcref =  mkKnownTyconRef sysCcu "System.UInt16";
    system_Char_tcref =  mkKnownTyconRef sysCcu "System.Char";
    system_UInt32_tcref =  mkKnownTyconRef sysCcu "System.UInt32";
    system_UInt64_tcref =  mkKnownTyconRef sysCcu "System.UInt64";
    system_UIntPtr_tcref =  mkKnownTyconRef sysCcu "System.UIntPtr";
    system_Single_tcref =  mkKnownTyconRef sysCcu "System.Single";
    system_Double_tcref =  mkKnownTyconRef sysCcu "System.Double";
    system_RuntimeTypeHandle_typ = mkKnownNonGenericTy sysCcu "System.RuntimeTypeHandle";
    system_RuntimeMethodHandle_typ = mkKnownNonGenericTy sysCcu "System.RuntimeMethodHandle";
    system_MarshalByRefObject_tcref = mkKnownTyconRef sysCcu "System.MarshalByRefObject";
    system_MarshalByRefObject_typ = mkKnownNonGenericTy sysCcu "System.MarshalByRefObject";
    system_Reflection_MethodInfo_typ = system_Reflection_MethodInfo_typ;
    
    system_Array_tcref  = mkKnownTyconRef sysCcu "System.Array";
    system_Object_tcref  = mkKnownTyconRef sysCcu "System.Object";
    system_Void_tcref    = mkKnownTyconRef sysCcu "System.Void";
    system_IndexOutOfRangeException_tcref    = mkKnownTyconRef sysCcu "System.IndexOutOfRangeException";
    system_Nullable_tcref = mkKnownTyconRef sysCcu "System.Nullable`1";
    system_GenericIComparable_tcref = mkKnownTyconRef sysCcu "System.IComparable`1";
    system_GenericIEquatable_tcref = mkKnownTyconRef sysCcu "System.IEquatable`1";
    mk_IComparable_ty    = mkKnownNonGenericTy sysCcu "System.IComparable";

    mk_IStructuralComparable_ty = mkKnownNonGenericTy maybeSysCcu "System.Collections.IStructuralComparable";
        
    mk_IStructuralEquatable_ty = mkKnownNonGenericTy maybeSysCcu "System.Collections.IStructuralEquatable";

    mk_IComparer_ty = mk_IComparer_ty;
    mk_IEqualityComparer_ty = mk_IEqualityComparer_ty;
    tcref_System_Collections_IComparer = mkKnownTyconRef sysCcu "System.Collections.IComparer";
    tcref_System_Collections_IEqualityComparer = mkKnownTyconRef sysCcu "System.Collections.IEqualityComparer";
    tcref_System_Collections_Generic_IEqualityComparer = mkKnownTyconRef sysCcu "System.Collections.Generic.IEqualityComparer`1";
    tcref_System_Collections_Generic_Dictionary = mkKnownTyconRef sysCcu "System.Collections.Generic.Dictionary`2";
    
    tcref_System_IComparable = mkKnownTyconRef sysCcu "System.IComparable"
    tcref_System_IStructuralComparable = mkKnownTyconRef maybeSysCcu "System.Collections.IStructuralComparable"
    tcref_System_IStructuralEquatable  = mkKnownTyconRef maybeSysCcu "System.Collections.IStructuralEquatable";
            
    tcref_LanguagePrimitives = mk_MFCore_tcref fslibCcu "LanguagePrimitives";

    mk_IConvertible_ty    = mkKnownNonGenericTy sysCcu "System.IConvertible";
    mk_IFormattable_ty    = mkKnownNonGenericTy sysCcu "System.IFormattable";

    tcref_System_Collections_Generic_IList       = mkKnownTyconRef sysCcu "System.Collections.Generic.IList`1";
    tcref_System_Collections_Generic_ICollection = mkKnownTyconRef sysCcu "System.Collections.Generic.ICollection`1";
    tcref_System_Collections_Generic_IEnumerable = IEnumerable_tcr;
    tcref_System_Collections_Generic_IEnumerator = IEnumerator_tcr;
    
    tcref_System_Attribute = System_Attribute_tcr;

    attrib_AttributeUsageAttribute = mkMscorlibAttrib "System.AttributeUsageAttribute";
    attrib_ParamArrayAttribute     = mkMscorlibAttrib "System.ParamArrayAttribute";
    attrib_IDispatchConstantAttribute  = mkMscorlibAttrib "System.Runtime.CompilerServices.IDispatchConstantAttribute";
    attrib_IUnknownConstantAttribute  = mkMscorlibAttrib "System.Runtime.CompilerServices.IUnknownConstantAttribute";
    
    attrib_SystemObsolete          = mkMscorlibAttrib "System.ObsoleteAttribute";
    attrib_DllImportAttribute      = mkMscorlibAttrib "System.Runtime.InteropServices.DllImportAttribute";
    attrib_StructLayoutAttribute   = mkMscorlibAttrib "System.Runtime.InteropServices.StructLayoutAttribute";
    attrib_TypeForwardedToAttribute   = mkMscorlibAttrib "System.Runtime.CompilerServices.TypeForwardedToAttribute";
    attrib_ComVisibleAttribute     = mkMscorlibAttrib "System.Runtime.InteropServices.ComVisibleAttribute";
    attrib_ComImportAttribute      = mkMscorlibAttrib "System.Runtime.InteropServices.ComImportAttribute";
    attrib_FieldOffsetAttribute    = mkMscorlibAttrib "System.Runtime.InteropServices.FieldOffsetAttribute" ;
    attrib_MarshalAsAttribute      = mkMscorlibAttrib "System.Runtime.InteropServices.MarshalAsAttribute";
    attrib_InAttribute             = mkMscorlibAttrib "System.Runtime.InteropServices.InAttribute" ;
    attrib_OutAttribute            = mkMscorlibAttrib "System.Runtime.InteropServices.OutAttribute" ;
    attrib_OptionalAttribute       = mkMscorlibAttrib "System.Runtime.InteropServices.OptionalAttribute" ;
    attrib_ThreadStaticAttribute   = mkMscorlibAttrib "System.ThreadStaticAttribute";
    attrib_SpecialNameAttribute   = mkMscorlibAttrib "System.Runtime.CompilerServices.SpecialNameAttribute";
    attrib_VolatileFieldAttribute   = mk_MFCore_attrib "VolatileFieldAttribute";
    attrib_ContextStaticAttribute  = mkMscorlibAttrib "System.ContextStaticAttribute";
    attrib_FlagsAttribute          = mkMscorlibAttrib "System.FlagsAttribute";
    attrib_DefaultMemberAttribute  = mkMscorlibAttrib "System.Reflection.DefaultMemberAttribute";
    attrib_DebuggerDisplayAttribute  = mkMscorlibAttrib "System.Diagnostics.DebuggerDisplayAttribute";
    attrib_DebuggerTypeProxyAttribute  = mkMscorlibAttrib "System.Diagnostics.DebuggerTypeProxyAttribute";
    attrib_PreserveSigAttribute    = mkMscorlibAttrib "System.Runtime.InteropServices.PreserveSigAttribute";
    attrib_MethodImplAttribute     = mkMscorlibAttrib "System.Runtime.CompilerServices.MethodImplAttribute";
    
    attrib_NonSerializedAttribute                 = mkMscorlibAttrib "System.NonSerializedAttribute";
    attrib_AutoSerializableAttribute              = mk_MFCore_attrib "AutoSerializableAttribute";
    attrib_RequireQualifiedAccessAttribute        = mk_MFCore_attrib "RequireQualifiedAccessAttribute";
    attrib_EntryPointAttribute                    = mk_MFCore_attrib "EntryPointAttribute";
    attrib_DefaultAugmentationAttribute           = mk_MFCore_attrib "DefaultAugmentationAttribute";
    attrib_CompilerMessageAttribute               = mk_MFCore_attrib "CompilerMessageAttribute";
    attrib_ExperimentalAttribute                  = mk_MFCore_attrib "ExperimentalAttribute";
    attrib_UnverifiableAttribute                  = mk_MFCore_attrib "UnverifiableAttribute";
    attrib_LiteralAttribute                       = mk_MFCore_attrib "LiteralAttribute";
    attrib_ConditionalAttribute                   = mkMscorlibAttrib "System.Diagnostics.ConditionalAttribute";
    attrib_OptionalArgumentAttribute              = mk_MFCore_attrib "OptionalArgumentAttribute";
    attrib_RequiresExplicitTypeArgumentsAttribute = mk_MFCore_attrib "RequiresExplicitTypeArgumentsAttribute";
    attrib_DefaultValueAttribute                  = mk_MFCore_attrib "DefaultValueAttribute";
    attrib_ClassAttribute                         = mk_MFCore_attrib "ClassAttribute";
    attrib_InterfaceAttribute                     = mk_MFCore_attrib "InterfaceAttribute";
    attrib_StructAttribute                        = mk_MFCore_attrib "StructAttribute";
    attrib_ReflectedDefinitionAttribute           = mk_MFCore_attrib "ReflectedDefinitionAttribute";
    attrib_CompiledNameAttribute                  = mk_MFCore_attrib "CompiledNameAttribute";
    attrib_AutoOpenAttribute                      = mk_MFCore_attrib "AutoOpenAttribute";
    attrib_CompilationRepresentationAttribute     = mk_MFCore_attrib "CompilationRepresentationAttribute";
    attrib_CompilationArgumentCountsAttribute     = mk_MFCore_attrib "CompilationArgumentCountsAttribute";
    attrib_CompilationMappingAttribute            = mk_MFCore_attrib "CompilationMappingAttribute";
    attrib_CLIEventAttribute                      = mk_MFCore_attrib "CLIEventAttribute";
    attrib_AllowNullLiteralAttribute                      = mk_MFCore_attrib "AllowNullLiteralAttribute";
    attrib_NoEqualityAttribute             = mk_MFCore_attrib "NoEqualityAttribute";
    attrib_NoComparisonAttribute             = mk_MFCore_attrib "NoComparisonAttribute";
    attrib_CustomEqualityAttribute             = mk_MFCore_attrib "CustomEqualityAttribute";
    attrib_CustomComparisonAttribute             = mk_MFCore_attrib "CustomComparisonAttribute";
    attrib_EqualityConditionalOnAttribute             = mk_MFCore_attrib "EqualityConditionalOnAttribute";
    attrib_ComparisonConditionalOnAttribute             = mk_MFCore_attrib "ComparisonConditionalOnAttribute";
    attrib_ReferenceEqualityAttribute             = mk_MFCore_attrib "ReferenceEqualityAttribute";
    attrib_StructuralEqualityAttribute            = mk_MFCore_attrib "StructuralEqualityAttribute";
    attrib_StructuralComparisonAttribute          = mk_MFCore_attrib "StructuralComparisonAttribute";
    attrib_SealedAttribute                        = mk_MFCore_attrib "SealedAttribute";
    attrib_AbstractClassAttribute                 = mk_MFCore_attrib "AbstractClassAttribute";
    attrib_GeneralizableValueAttribute            = mk_MFCore_attrib "GeneralizableValueAttribute";
    attrib_MeasureAttribute                       = mk_MFCore_attrib "MeasureAttribute";
    attrib_MeasureableAttribute                   = mk_MFCore_attrib "MeasureAnnotatedAbbreviationAttribute";
    attrib_GeneratedEstTypeAttribute              = mk_MFCore_attrib "GenerateAttribute";
    attrib_NoDynamicInvocationAttribute           = mk_MFCore_attrib "NoDynamicInvocationAttribute";
    attrib_SecurityAttribute                      = mkMscorlibAttrib "System.Security.Permissions.SecurityAttribute"
    attrib_SecurityCriticalAttribute              = mkMscorlibAttrib "System.Security.SecurityCriticalAttribute"
    attrib_SecuritySafeCriticalAttribute          = mkMscorlibAttrib "System.Security.SecuritySafeCriticalAttribute"
    

    // Build a map that uses the "canonical" F# type names and TyconRef's for these
    // in preference to the .NET type names. Doing this normalization is a fairly performance critical
    // piece of code as it is frequently invoked in the process of converting .NET metadata to F# internal
    // compiler data structures (see import.fs).
    better_tcref_map = 
       begin 
        let entries1 = 
         [ "Int32", int_tcr; 
           "IntPtr", nativeint_tcr; 
           "UIntPtr", unativeint_tcr;
           "Int16",int16_tcr; 
           "Int64",int64_tcr; 
           "UInt16",uint16_tcr;
           "UInt32",uint32_tcr;
           "UInt64",uint64_tcr;
           "SByte",sbyte_tcr;
           "Decimal",decimal_tcr;
           "Byte",byte_tcr;
           "Boolean",bool_tcr;
           "String",string_tcr;
           "Object",obj_tcr;
           "Exception",exn_tcr;
           "Char",char_tcr;
           "Double",float_tcr;
           "Single",float32_tcr;] 
             |> List.map (fun (nm,tcr) -> 
                   let ty = mkNonGenericTy tcr 
                   nm, mkKnownTyconRef sysCcu ("System."^nm), (fun _ -> ty)) 
        let entries2 =
            [ "IEnumerable`2", IEnumerable_tcr, (fun tinst -> mkSeqTy (List.nth tinst 0));
              "FSharpFunc`2",    fastFunc_tcr, (fun tinst -> mkFunTy (List.nth tinst 0) (List.nth tinst 1));
              "Tuple`2",       tuple2_tcr, decode_tuple_ty;
              "Tuple`3",       tuple3_tcr, decode_tuple_ty;
              "Tuple`4",       tuple4_tcr, decode_tuple_ty;
              "Tuple`5",       tuple5_tcr, decode_tuple_ty;
              "Tuple`6",       tuple6_tcr, decode_tuple_ty;
              "Tuple`7",       tuple7_tcr, decode_tuple_ty;
              "Tuple`8",       tuple8_tcr, decode_tuple_ty;] 
        let entries = (entries1 @ entries2)
        
        if compilingFslib then 
            // This map is for use when building FSharp.Core.dll. The backing Tycon's may not yet exist for
            // the TyconRef's we have inour hands, hence we can't dereference them to find their stamps.

            // So this dictionary is indexed by names.
            let dict = 
                entries 
                |> List.map (fun (nm,tcref,builder) -> nm, (fun tcref2 tinst -> if tyconRefEq tcref tcref2 then Some(builder tinst) else None)) 
                |> Dictionary.ofList  
            (fun tcref tinst -> 
                 if dict.ContainsKey tcref.LogicalName then dict.[tcref.LogicalName] tcref tinst
                 else None )  
        else
            // This map is for use in normal times (not building FSharp.Core.dll). It is indexed by tcref stamp which is 
            // faster than the indexing technique used in the case above.
            //
            // So this dictionary is indexed by integers.
            let dict = 
                entries  
                |> List.map (fun (_,tcref,builder) -> tcref.Stamp, builder) 
                |> Dictionary.ofList 
            (fun tcref2 tinst -> 
                 if dict.ContainsKey tcref2.Stamp then Some(dict.[tcref2.Stamp] tinst)
                 else None)  
       end;
           
    new_decimal_info = new_decimal_info;
    seq_info    = seq_info;
    seq_vref    = (ValRefForIntrinsic seq_info) ;
    and_vref    = (ValRefForIntrinsic and_info) ;
    and2_vref   = (ValRefForIntrinsic and2_info);
    addrof_vref = (ValRefForIntrinsic addrof_info);
    addrof2_vref = (ValRefForIntrinsic addrof2_info);
    or_vref     = (ValRefForIntrinsic or_info);
    //splice_vref     = (ValRefForIntrinsic splice_info);
    splice_expr_vref     = (ValRefForIntrinsic splice_expr_info);
    splice_raw_expr_vref     = (ValRefForIntrinsic splice_raw_expr_info);
    or2_vref    = (ValRefForIntrinsic or2_info); 
    generic_equality_er_inner_vref     = ValRefForIntrinsic generic_equality_er_inner_info;
    generic_equality_per_inner_vref = ValRefForIntrinsic generic_equality_per_inner_info;
    generic_equality_withc_inner_vref  = ValRefForIntrinsic generic_equality_withc_inner_info;
    generic_comparison_inner_vref    = ValRefForIntrinsic generic_comparison_inner_info;
    generic_comparison_withc_inner_vref    = ValRefForIntrinsic generic_comparison_withc_inner_info;
    generic_comparison_withc_outer_info    = generic_comparison_withc_outer_info;
    generic_equality_er_outer_info     = generic_equality_er_outer_info;
    generic_equality_withc_outer_info  = generic_equality_withc_outer_info;
    generic_hash_withc_outer_info = generic_hash_withc_outer_info;
    generic_hash_inner_vref = ValRefForIntrinsic generic_hash_inner_info;
    generic_hash_withc_inner_vref = ValRefForIntrinsic generic_hash_withc_inner_info;

    reference_equality_inner_vref         = ValRefForIntrinsic reference_equality_inner_info;

    bitwise_or_vref            = ValRefForIntrinsic bitwise_or_info;
    bitwise_and_vref           = ValRefForIntrinsic bitwise_and_info;
    bitwise_xor_vref           = ValRefForIntrinsic bitwise_xor_info;
    bitwise_unary_not_vref     = ValRefForIntrinsic bitwise_unary_not_info;
    bitwise_shift_left_vref    = ValRefForIntrinsic bitwise_shift_left_info;
    bitwise_shift_right_vref   = ValRefForIntrinsic bitwise_shift_right_info;
    unchecked_addition_vref    = ValRefForIntrinsic unchecked_addition_info;
    unchecked_unary_plus_vref  = ValRefForIntrinsic unchecked_unary_plus_info;
    unchecked_unary_minus_vref = ValRefForIntrinsic unchecked_unary_minus_info;
    unchecked_unary_not_vref = ValRefForIntrinsic unchecked_unary_not_info;
    unchecked_subtraction_vref = ValRefForIntrinsic unchecked_subtraction_info;
    unchecked_multiply_vref    = ValRefForIntrinsic unchecked_multiply_info;
    unchecked_defaultof_vref    = ValRefForIntrinsic unchecked_defaultof_info;

    compare_operator_vref    = ValRefForIntrinsic compare_operator_info;
    equals_operator_vref    = ValRefForIntrinsic equals_operator_info;
    not_equals_operator_vref    = ValRefForIntrinsic not_equals_operator_info;
    less_than_operator_vref    = ValRefForIntrinsic less_than_operator_info;
    less_than_or_equals_operator_vref    = ValRefForIntrinsic less_than_or_equals_operator_info;
    greater_than_operator_vref    = ValRefForIntrinsic greater_than_operator_info;
    greater_than_or_equals_operator_vref    = ValRefForIntrinsic greater_than_or_equals_operator_info;

    equals_operator_info     = equals_operator_info;

    raise_info                 = raise_info;
    reraise_info               = reraise_info;
    reraise_vref               = ValRefForIntrinsic reraise_info;
    typeof_info                = typeof_info;
    typeof_vref                = ValRefForIntrinsic typeof_info;
    sizeof_vref                = ValRefForIntrinsic sizeof_info;
    typedefof_info             = typedefof_info;
    typedefof_vref             = ValRefForIntrinsic typedefof_info;
    enum_vref                  = ValRefForIntrinsic enum_info;
    range_op_vref              = ValRefForIntrinsic range_op_info;
    range_int32_op_vref        = ValRefForIntrinsic range_int32_op_info;
    //range_step_op_vref         = ValRefForIntrinsic range_step_op_info;
    array_get_vref             = ValRefForIntrinsic array_get_info;
    array2D_get_vref           = ValRefForIntrinsic array2D_get_info;
    array3D_get_vref           = ValRefForIntrinsic array3D_get_info;
    array4D_get_vref           = ValRefForIntrinsic array4D_get_info;
    seq_singleton_vref         = ValRefForIntrinsic seq_singleton_info;
    seq_collect_vref           = ValRefForIntrinsic seq_collect_info;
    seq_collect_info           = seq_collect_info;
    seq_using_info             = seq_using_info;
    seq_using_vref             = ValRefForIntrinsic seq_using_info;
    seq_delay_info             = seq_delay_info;
    seq_delay_vref             = ValRefForIntrinsic  seq_delay_info;
    seq_append_info            = seq_append_info;
    seq_append_vref            = ValRefForIntrinsic  seq_append_info;
    seq_generated_info         = seq_generated_info;
    seq_generated_vref         = ValRefForIntrinsic  seq_generated_info;
    seq_finally_info           = seq_finally_info;
    seq_finally_vref           = ValRefForIntrinsic  seq_finally_info;
    seq_of_functions_info      = seq_of_functions_info;
    seq_of_functions_vref      = ValRefForIntrinsic  seq_of_functions_info;
    seq_map_info               = seq_map_info;
    seq_map_vref               = ValRefForIntrinsic  seq_map_info;
    seq_singleton_info         = seq_singleton_info;
    seq_empty_info             = seq_empty_info;
    seq_empty_vref             = ValRefForIntrinsic  seq_empty_info;
    new_format_info            = new_format_info;
    new_format_vref            = ValRefForIntrinsic new_format_info;
    sprintf_vref               = ValRefForIntrinsic sprintf_info;
    unbox_vref                 = ValRefForIntrinsic unbox_info;
    unbox_fast_vref            = ValRefForIntrinsic unbox_fast_info;
    istype_vref                = ValRefForIntrinsic istype_info;
    istype_fast_vref           = ValRefForIntrinsic istype_fast_info;
    unbox_info                 = unbox_info;
    get_generic_comparer_info                 = get_generic_comparer_info;
    get_generic_er_equality_comparer_info        = get_generic_er_equality_comparer_info;
    get_generic_per_equality_comparer_info    = get_generic_per_equality_comparer_info;
    dispose_info               = dispose_info;
    unbox_fast_info            = unbox_fast_info;
    istype_info                = istype_info;
    istype_fast_info           = istype_fast_info;
    lazy_force_info            = lazy_force_info;
    lazy_create_info           = lazy_create_info;
    create_instance_info       = create_instance_info;
    create_event_info          = create_event_info;
    seq_to_list_info           = seq_to_list_info;
    seq_to_array_info          = seq_to_array_info;
    array_get_info             = array_get_info;
    array2D_get_info             = array2D_get_info;
    array3D_get_info             = array3D_get_info;
    array4D_get_info             = array4D_get_info;
    generic_hash_info          = generic_hash_info;
    unpickle_quoted_info       = unpickle_quoted_info;
    cast_quotation_info        = cast_quotation_info;
    lift_value_info            = lift_value_info;
    fail_init_info             = fail_init_info;
    fail_static_init_info           = fail_static_init_info;
    check_this_info            = check_this_info;


    generic_hash_withc_tuple2_vref = ValRefForIntrinsic generic_hash_withc_tuple2_info;
    generic_hash_withc_tuple3_vref = ValRefForIntrinsic generic_hash_withc_tuple3_info;
    generic_hash_withc_tuple4_vref = ValRefForIntrinsic generic_hash_withc_tuple4_info;
    generic_hash_withc_tuple5_vref = ValRefForIntrinsic generic_hash_withc_tuple5_info;
    generic_equals_withc_tuple2_vref = ValRefForIntrinsic generic_equals_withc_tuple2_info;
    generic_equals_withc_tuple3_vref = ValRefForIntrinsic generic_equals_withc_tuple3_info;
    generic_equals_withc_tuple4_vref = ValRefForIntrinsic generic_equals_withc_tuple4_info;
    generic_equals_withc_tuple5_vref = ValRefForIntrinsic generic_equals_withc_tuple5_info;
    generic_compare_withc_tuple2_vref = ValRefForIntrinsic generic_compare_withc_tuple2_info;
    generic_compare_withc_tuple3_vref = ValRefForIntrinsic generic_compare_withc_tuple3_info;
    generic_compare_withc_tuple4_vref = ValRefForIntrinsic generic_compare_withc_tuple4_info;
    generic_compare_withc_tuple5_vref = ValRefForIntrinsic generic_compare_withc_tuple5_info;
    generic_equality_withc_outer_vref = ValRefForIntrinsic generic_equality_withc_outer_info;


    cons_ucref = cons_ucref;
    nil_ucref = nil_ucref;
    
    suppressed_types = suppressed_types;
   }
     
let public mkMscorlibAttrib g nm : BuiltinAttribInfo = 
      AttribInfo(mkILTyRef (g.ilg.mscorlibScopeRef,nm), mkKnownTyconRef g.sysCcu nm)


