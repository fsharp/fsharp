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


module internal Microsoft.FSharp.Compiler.AbstractIL.ILAsciiWriter 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiConstants 
open Microsoft.FSharp.Compiler.AbstractIL.IL 

open System.Text
open System.IO

#if DEBUG
let tailcall_via_ldftn = ref false
let call_via_ldftn = ref false
let pretty () = true

// -------------------------------------------------------------------- 
// Pretty printing
// --------------------------------------------------------------------


let tyvar_generator = 
  let i = ref 0 
  fun n -> 
    incr i; n^string !i

(* Carry an environment because the way we print method variables *)
(* depends on the gparams of the current scope. *)
type ppenv = 
    { ppenvClassFormals: int;
      ppenvMethodFormals: int }
let ppenv_enter_method  mgparams env = 
    {env with ppenvMethodFormals=mgparams}
let ppenv_enter_tdef gparams env =
    {env with ppenvClassFormals=List.length gparams; ppenvMethodFormals=0}
let mk_ppenv = { ppenvClassFormals=0; ppenvMethodFormals=0 }
let debug_ppenv = mk_ppenv 
let ppenv_enter_modul env = { env with  ppenvClassFormals=0; ppenvMethodFormals=0 }

// -------------------------------------------------------------------- 
// Pretty printing - output streams
// -------------------------------------------------------------------- 

let output_string (os: TextWriter) (s:string) = os.Write s 
let output_char (os: TextWriter) (c:char) = os.Write c
let output_int os (i:int) = output_string os (string i)
let output_hex_digit os i = 
  assert (i >= 0 && i < 16);
  if i > 9 then output_char os (char (int32 'A' + (i-10))) 
  else output_char os (char (int32 '0' + i))

let output_qstring os s =
  output_char os '"';
  for i = 0 to String.length s - 1 do
    let c = String.get s i
    if (c >= '\000' && c <= '\031') || (c >= '\127' && c <= '\255') then 
      let c' = int32 c
      output_char os '\\';
      output_int os (c'/64);
      output_int os ((c' % 64) / 8);
      output_int os (c' % 8) 
    else if (c = '"')  then 
      (output_char os '\\'; output_char os '"')
    else if (c = '\\')  then 
      (output_char os '\\'; output_char os '\\')
    else 
      output_char os c
  done;
  output_char os '"'
let output_sqstring os s =
  output_char os '\'';
  for i = 0 to String.length s - 1 do
    let c = s.[i]
    if (c >= '\000' && c <= '\031') || (c >= '\127' && c <= '\255') then 
      let c' = int32 c
      output_char os '\\';
      output_int os (c'/64);
      output_int os ((c' % 64) / 8);
      output_int os (c' % 8) 
    else if (c = '\\')  then 
      (output_char os '\\'; output_char os '\\')
    else if (c = '\'')  then 
      (output_char os '\\'; output_char os '\'')
    else 
      output_char os c
  done;
  output_char os '\''

let output_list sep f os a =
  if List.length a > 0 then 
      f os (List.head a);
      List.iter (fun x -> output_string os sep; f os x) (List.tail a)
let output_parens f os a = output_string os "("; f os a; output_string os ")"
let output_angled f os a = output_string os "<"; f os a; output_string os ">"
let output_bracks f os a = output_string os "["; f os a; output_string os "]"

let output_id os n = output_sqstring os n

let output_label os n = output_string os n

(* let output_data_label os ((l,_): DataLabel) = output_string os l *)

let output_lid os lid = output_list "." output_string os lid
let string_of_type_name (_,n) = n

let output_byte os i = 
  output_hex_digit os (i / 16);
  output_hex_digit os (i % 16)

let output_bytes os (bytes:byte[]) = 
  for i = 0 to bytes.Length - 1 do
    output_byte os (Bytes.get bytes i);
    output_string os " "
  done


let bits_of_float32 (x:float32) = System.BitConverter.ToInt32(System.BitConverter.GetBytes(x),0)
let bits_of_float (x:float) = System.BitConverter.DoubleToInt64Bits(x)

let output_u8 os (x:byte) = output_string os (string (int x))
let output_i8 os (x:sbyte) = output_string os (string (int x))
let output_u16 os (x:uint16) = output_string os (string (int x))
let output_i16 os (x:int16) = output_string os (string (int x))
let output_u32 os (x:uint32) = output_string os (string (int64 x))
let output_i32 os (x:int32) = output_string os (string x) 
let output_u64 os (x:uint64) = output_string os (string (int64 x))
let output_i64 os (x:int64) = output_string os (string x) 
let output_ieee32 os (x:float32) = (output_string os "float32 ("; output_string os (string (bits_of_float32 x)); output_string os ")")
let output_ieee64 os (x:float) = (output_string os "float64 ("; output_string os (string (bits_of_float x)); output_string os ")")

let rec goutput_scoref _env os = function 
  | ILScopeRef.Local -> ()
  | ILScopeRef.Assembly aref ->
      output_string os "["; output_sqstring os aref.Name; output_string os "]"
  | ILScopeRef.Module mref ->
      output_string os "[.module "; output_sqstring os mref.Name; output_string os "]" 

and goutput_type_name_ref env os (scoref,enc,n) = 
  goutput_scoref env os scoref;
  output_list "/" output_sqstring os (enc@[n])
and goutput_tref env os (x:ILTypeRef) = 
  goutput_type_name_ref env os (x.Scope,x.Enclosing,x.Name)

and goutput_typ env os ty =
  match ty with 
  | ILType.Boxed tr ->  goutput_tspec env os tr
  | ILType.TypeVar tv ->  
      (* Special rule to print method type variables in Generic EE preferred form *)
      (* when an environment is available to help us do this. *)
      let cgparams = env.ppenvClassFormals 
      let mgparams = env.ppenvMethodFormals 
      if int tv < cgparams then 
        output_string os "!";
        output_tyvar os tv
      elif int tv -  cgparams <  mgparams then 
        output_string os "!!";
        output_int os (int tv -  cgparams);
      else 
        output_string os "!";
        output_tyvar os tv;
        output_int os (int tv)
      
  | ILType.Byref typ -> goutput_typ env os typ; output_string os "&"
  | ILType.Ptr typ ->  goutput_typ env os typ; output_string   os "*"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_SByte.Name ->  output_string os "int8" 
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Int16.Name ->  output_string os "int16"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Int32.Name ->  output_string os "int32"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Int64.Name ->  output_string os "int64"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_IntPtr.Name ->  output_string os "native int"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Byte.Name ->  output_string os "unsigned int8" 
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_UInt16.Name ->  output_string os "unsigned int16"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_UInt32.Name ->  output_string os "unsigned int32"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_UInt64.Name ->  output_string os "unsigned int64"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_UIntPtr.Name ->  output_string os "native unsigned int"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Double.Name ->  output_string os "float64"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Single.Name ->  output_string os "float32"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Bool.Name ->  output_string os "bool"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_Char.Name ->  output_string os "char"
  | ILType.Value tspec when tspec.Name = ecmaILGlobals.tspec_TypedReference.Name ->  output_string os "refany"
  | ILType.Value tspec ->
      output_string os "value class ";
      goutput_tref env os tspec.TypeRef;
      output_string os " ";
      goutput_gactuals env os tspec.GenericArgs
  | ILType.Void ->  output_string os "void"
  | ILType.Array (bounds,ty) -> 
      goutput_typ env os ty;
      output_string os "[";
      output_arr_bounds os bounds;
      output_string os "]";
  | ILType.FunctionPointer csig ->
      output_string os "method ";
      goutput_typ env os csig.ReturnType;
      output_string os " *(";
      output_list "," (goutput_typ env) os csig.ArgTypes;
      output_string os ")"
  | _ -> output_string os "NaT"
  
and output_tyvar os d =  
  output_u16 os d; ()

and goutput_ldtoken_info env os = function
  | ILToken.ILType x -> goutput_typ env os x
  | ILToken.ILMethod x -> output_string os "method "; goutput_mspec env os x
  | ILToken.ILField x -> output_string os "field "; goutput_fspec env os x

and goutput_typ_with_shortened_class_syntax env os = function
    ILType.Boxed tspec when tspec.GenericArgs = emptyILGenericArgs -> 
      goutput_tref env os tspec.TypeRef
  | typ2 -> goutput_typ env os typ2

and goutput_gactuals env os inst = 
  if inst = [] then () 
  else  
    output_string os "<";
    goutput_gactual env os (List.head inst);
    List.iter (fun x -> output_string os ", "; goutput_gactual env os x) (List.tail inst);
    output_string os ">";

and goutput_gactual env os ty = goutput_typ env os ty

and goutput_tspec env os tspec = 
      output_string os "class ";
      goutput_tref env os tspec.TypeRef;
      output_string os " ";
      goutput_gactuals env os tspec.GenericArgs;

and output_arr_bounds os = function 
  | bounds when bounds = ILArrayShape.SingleDimensional -> ()
  | ILArrayShape l ->
      output_list "," 
          (fun os -> function
            | (None,None)  -> output_string os ""
            | (None,Some sz) -> 
                output_int os sz
            | (Some lower,None) -> 
                output_int os lower; 
                output_string os " ... "
            | (Some lower,Some d) -> 
                output_int os lower;
                output_string os " ... ";
                output_int os d)
          os 
          l
  
and goutput_permission _env os p = 
  let output_security_action  os x = 
    output_string os 
      (match x with 
        | ILSecurityAction.Request ->  "request"
        | ILSecurityAction.Demand ->  "demand"
        | ILSecurityAction.Assert->  "assert"
        | ILSecurityAction.Deny->  "deny"
        | ILSecurityAction.PermitOnly->  "permitonly"
        | ILSecurityAction.LinkCheck->  "linkcheck"
        | ILSecurityAction.InheritCheck->  "inheritcheck"
        | ILSecurityAction.ReqMin->  "reqmin"
        | ILSecurityAction.ReqOpt->  "reqopt"
        | ILSecurityAction.ReqRefuse->  "reqrefuse"
        | ILSecurityAction.PreJitGrant->  "prejitgrant"
        | ILSecurityAction.PreJitDeny->  "prejitdeny"
        | ILSecurityAction.NonCasDemand->  "noncasdemand"
        | ILSecurityAction.NonCasLinkDemand->  "noncaslinkdemand"
        | ILSecurityAction.NonCasInheritance->  "noncasinheritance" 
        | ILSecurityAction.LinkDemandChoice -> "linkdemandchoice"
        | ILSecurityAction.InheritanceDemandChoice -> "inheritancedemandchoice"
        | ILSecurityAction.DemandChoice -> "demandchoice") 



  match p with 
  | PermissionSet (sa,b) -> 
      output_string os " .permissionset ";
      output_security_action os sa ;
      output_string os " = (" ;
      output_bytes os b ;
      output_string os ")" ;
  
and goutput_security_decls env os (ps: ILPermissions) =  output_list " " (goutput_permission env)  os ps.AsList

and goutput_gparam env os (gf: ILGenericParameterDef) =  
  output_string os (tyvar_generator gf.Name);
  output_parens (output_list "," (goutput_typ env)) os gf.Constraints

and goutput_gparams env os b = 
  if nonNil b then 
     output_string os "<"; output_list "," (goutput_gparam env) os b;  output_string os ">"; () 

and output_bcc os bcc =
  output_string os  
    (match bcc with 
    | ILArgConvention.FastCall -> "fastcall "
    | ILArgConvention.StdCall -> "stdcall "
    | ILArgConvention.ThisCall -> "thiscall "
    | ILArgConvention.CDecl -> "cdecl "
    | ILArgConvention.Default -> " "
    | ILArgConvention.VarArg -> "vararg ")

and output_callconv os (Callconv (hasthis,cc)) = 
  output_string os  
    (match hasthis with 
      ILThisConvention.Instance -> "instance " 
    | ILThisConvention.InstanceExplicit -> "explicit "
    | ILThisConvention.Static -> "") ;
  output_bcc os cc

and goutput_dlocref env os (dref:ILType) = 
  match dref with 
  | dref when 
       dref.IsNominal && 
       isTypeNameForGlobalFunctions dref.TypeRef.Name &&
       dref.TypeRef.Scope = ILScopeRef.Local -> 
   ()
  | dref when 
       dref.IsNominal && 
       isTypeNameForGlobalFunctions dref.TypeRef.Name ->
   goutput_scoref env os dref.TypeRef.Scope;
   output_string os "::"
  | ty ->goutput_typ_with_shortened_class_syntax env os ty;  output_string os "::" 

and goutput_callsig env os (csig:ILCallingSignature) =
  output_callconv os csig.CallingConv;
  output_string os " ";
  goutput_typ env os csig.ReturnType;
  output_parens (output_list "," (goutput_typ env)) os csig.ArgTypes

and goutput_mref env os (mref:ILMethodRef) =
  output_callconv os mref.CallingConv;
  output_string os " ";
  goutput_typ_with_shortened_class_syntax env os mref.ReturnType;
  output_string os " ";
  (* no quotes for ".ctor" *)
  let name = mref.Name 
  if name = ".ctor" || name = ".cctor" then output_string os name else output_id os name; 
  output_parens (output_list "," (goutput_typ env)) os mref.ArgTypes

and goutput_mspec env os (mspec:ILMethodSpec) = 
  let fenv = 
    ppenv_enter_method mspec.GenericArity
      (ppenv_enter_tdef (mkILFormalTypars mspec.EnclosingType.GenericArgs) env) 
  output_callconv os mspec.CallingConv;
  output_string os " ";
  goutput_typ fenv os mspec.FormalReturnType;
  output_string os " ";
  goutput_dlocref env os mspec.EnclosingType;
  output_string os " ";
  let name = mspec.Name 
  if name = ".ctor" || name = ".cctor" then output_string os name else output_id os name; 
  goutput_gactuals env os mspec.GenericArgs;
  output_parens (output_list "," (goutput_typ fenv)) os mspec.FormalArgTypes;

and goutput_vararg_mspec env os (mspec, varargs) =
   match varargs with 
   | None -> goutput_mspec env os mspec
   | Some varargs' -> 
       let fenv = 
         ppenv_enter_method mspec.GenericArity
           (ppenv_enter_tdef (mkILFormalTypars mspec.EnclosingType.GenericArgs) env) 
       output_callconv os mspec.CallingConv;
       output_string os " ";
       goutput_typ fenv os mspec.FormalReturnType;
       output_string os " ";
       goutput_dlocref env os mspec.EnclosingType;
       let name = mspec.Name 
       if name = ".ctor" || name = ".cctor" then output_string os name else output_id os name
       goutput_gactuals env os mspec.GenericArgs;
       output_string os "(";
       output_list "," (goutput_typ fenv) os mspec.FormalArgTypes;
       output_string os ",...,";
       output_list "," (goutput_typ fenv) os varargs';
       output_string os ")";

and goutput_vararg_sig env os (csig:ILCallingSignature,varargs) =
   match varargs with 
   | None -> goutput_callsig env os csig; ()
   | Some varargs' -> 
       goutput_typ env os csig.ReturnType; 
       output_string os " (";
       let argtys = csig.ArgTypes 
       if argtys <> [] then
           goutput_typ env os (List.head argtys);
           List.iter (fun ty -> output_string os ","; goutput_typ env os ty) (List.tail argtys);
       output_string os ",...,"; 
       output_list "," (goutput_typ env) os varargs';
       output_string os ")"; 

and goutput_fspec env os (x:ILFieldSpec) =
  let fenv = ppenv_enter_tdef (mkILFormalTypars x.EnclosingType.GenericArgs) env 
  goutput_typ fenv os x.FormalType;
  output_string os " ";
  goutput_dlocref env os x.EnclosingType;
  output_id os x.Name
    
let output_member_access os access = 
  output_string os 
    (match access with 
    | ILMemberAccess.Public -> "public"
    | ILMemberAccess.Private  -> "private"
    | ILMemberAccess.CompilerControlled  -> "privatescope"
    | ILMemberAccess.Family  -> "family"
    | ILMemberAccess.FamilyAndAssembly -> "famandassem"
    | ILMemberAccess.FamilyOrAssembly -> "famorassem"
    | ILMemberAccess.Assembly -> "assembly")

let output_type_access os access = 
  match access with 
  | ILTypeDefAccess.Public -> output_string os "public"
  | ILTypeDefAccess.Private  -> output_string os "private"
  | ILTypeDefAccess.Nested  ilMemberAccess -> output_string os "nested "; output_member_access os ilMemberAccess

let output_encoding os e = 
  match e with 
  | ILDefaultPInvokeEncoding.Ansi -> output_string os " ansi "
  | ILDefaultPInvokeEncoding.Auto  -> output_string os " autochar "
  | ILDefaultPInvokeEncoding.Unicode -> output_string os " unicode "
let output_field_init os = function
  | ILFieldInit.String s -> output_string os "= "; output_string os s
  | ILFieldInit.Bool x-> output_string os "= bool"; output_parens output_string os (if x then "true" else "false")
  | ILFieldInit.Char x-> output_string os "= char"; output_parens output_u16 os x
  | ILFieldInit.Int8 x-> output_string os "= int8"; output_parens output_i8 os x
  | ILFieldInit.Int16 x-> output_string os "= int16"; output_parens output_i16 os x
  | ILFieldInit.Int32 x-> output_string os "= int32"; output_parens output_i32 os x
  | ILFieldInit.Int64 x-> output_string os "= int64"; output_parens output_i64 os x
  | ILFieldInit.UInt8 x-> output_string os "= uint8"; output_parens output_u8 os x
  | ILFieldInit.UInt16 x-> output_string os "= uint16"; output_parens output_u16 os x
  | ILFieldInit.UInt32 x-> output_string os "= uint32"; output_parens output_u32 os x
  | ILFieldInit.UInt64 x-> output_string os "= uint64"; output_parens output_u64 os x
  | ILFieldInit.Single x-> output_string os "= float32"; output_parens output_ieee32 os x
  | ILFieldInit.Double x-> output_string os "= float64"; output_parens output_ieee64 os x
  | ILFieldInit.Null-> output_string os "= nullref" 

let output_at os b =
   Printf.fprintf os " at (* no labels for data available, data = %a *)" (output_parens output_bytes) b

let output_option f os = function None -> () | Some x -> f os x
    
let goutput_alternative_ref env os (alt: IlxUnionAlternative) = 
  output_id os alt.Name; 
  alt.FieldDefs |> Array.toList |> output_parens (output_list "," (fun os fdef -> goutput_typ env os fdef.Type)) os 

let goutput_curef env os (IlxUnionRef(tref,alts,_,_)) =
  output_string os " .classunion import ";
  goutput_tref env os tref;
  output_parens (output_list "," (goutput_alternative_ref env)) os (Array.toList alts)
    
let goutput_cuspec env os (IlxUnionSpec(IlxUnionRef(tref,_,_,_),i)) =
  output_string os "class /* classunion */ ";
  goutput_tref env os  tref;
  goutput_gactuals env os i

let goutput_cloref env os (IlxClosureRef(tref,_,fvs)) =
  output_string os " .closure import ";
  goutput_tref env os tref;
  output_parens (output_list "," (fun os fv -> goutput_typ env os fv.fvType)) os fvs;
  output_string os "{ /* closure-ref */ }"
    
let goutput_clospec env os (IlxClosureSpec(IlxClosureRef(tref,_,_) as cloref,i) as _clospec) =
  output_string os "class /* closure */ ";
  goutput_cloref env os cloref;

  goutput_tref env os tref;
  goutput_gactuals env os i

let output_basic_type os x = 
  output_string os 
    (match x with
    | DT_I1 ->  "i1"
    | DT_U1 ->  "u1"
    | DT_I2 ->  "i2"
    | DT_U2 ->  "u2"
    | DT_I4 -> "i4"
    | DT_U4 -> "u4"
    | DT_I8 -> "i8"
    | DT_U8 -> "u8"
    | DT_R4 -> "r4"
    | DT_R8 -> "r8"
    | DT_R  -> "r"
    | DT_I  -> "i"
    | DT_U  -> "u"
    | DT_REF  -> "ref")

let output_custom_attr_data os data = 
  output_string os " = "; output_parens output_bytes os data
      
let goutput_custom_attr env os attr =
  output_string os " .custom ";
  goutput_mspec env os attr.Method;
  output_custom_attr_data os attr.Data

let goutput_custom_attrs env os (attrs : ILAttributes) =
  List.iter (fun attr -> goutput_custom_attr env os attr;  output_string os "\n" ) attrs.AsList

let goutput_fdef _tref env os fd =
  output_string os " .field ";
  match fd.Offset with Some i -> output_string os "["; output_i32 os i; output_string os "] " | None -> () 
  match fd.Marshal with Some _i -> output_string os "// marshal attribute not printed\n"; | None -> () 
  output_member_access os fd.Access;
  output_string os " ";
  if fd.IsStatic then output_string os " static ";
  if fd.IsLiteral then output_string os " literal ";
  if fd.IsSpecialName then output_string os " specialname rtspecialname ";
  if fd.IsInitOnly then output_string os " initonly ";
  if fd.NotSerialized then output_string os " notserialized ";
  goutput_typ env os fd.Type;
  output_string os " ";
  output_id os fd.Name;
  output_option output_at os  fd.Data; 
  output_option output_field_init os fd.LiteralValue;
  output_string os "\n";
  goutput_custom_attrs env os fd.CustomAttrs


let output_alignment os =  function
    Aligned -> ()
  | Unaligned1 -> output_string os "unaligned. 1 "
  | Unaligned2 -> output_string os "unaligned. 2 "
  | Unaligned4 -> output_string os "unaligned. 4 "

let output_volatility os =  function
    Nonvolatile -> ()
  | Volatile -> output_string os "volatile. "
let output_tailness os =  function
  | Tailcall -> output_string os "tail. "
  | _ -> ()
let output_after_tailcall os =  function
  | Tailcall  -> output_string os " ret "
  | _ -> ()
let rec goutput_apps env os =  function
  | Apps_tyapp (actual,cs) -> 
      output_angled (goutput_gactual env) os actual;
      output_string os " ";
      output_angled (goutput_gparam env) os (mkILSimpleTypar "T") ;
      output_string os " ";
      goutput_apps env os cs
  | Apps_app(ty,cs) ->  
      output_parens (goutput_typ env) os ty;
      output_string os " ";
      goutput_apps env os cs
  | Apps_done ty ->  
      output_string os "--> "; 
      goutput_typ env os ty

/// utilities to help print out short forms of instructions
let output_short_u16 os (x:uint16) =
     if int x < 256 then (output_string os ".s "; output_u16 os x)
     else (output_string os " "; output_u16 os x)
let output_short_i32 os i32 =
     if  i32 < 256 && 0 >= i32 then (output_string os ".s "; output_i32 os i32)
     else (output_string os " "; output_i32 os i32 )

let output_code_label os lab = 
  output_string os (formatCodeLabel lab)

let goutput_local env os (l: ILLocal) = 
  goutput_typ env os l.Type;
  if l.IsPinned then output_string os " pinned"

let goutput_param env os (l: ILParameter) = 
  match l.Name with 
      None ->  goutput_typ env os l.Type;
    | Some n -> goutput_typ env os l.Type; output_string os " "; output_sqstring os n

let goutput_params env os ps = 
  output_parens (output_list "," (goutput_param env)) os ps

let goutput_freevar env os l = 
  goutput_typ env os l.fvType; output_string os " "; output_sqstring os l.fvName 

let goutput_freevars env os ps = 
  output_parens (output_list "," (goutput_freevar env)) os ps

let output_source os (s:ILSourceMarker) = 
  if s.Document.File <> "" then 
    output_string os " .line ";
    output_int os s.Line;
    if s.Column <> -1 then 
      output_string os " : ";
      output_int os s.Column;
    output_string os " /* - ";
    output_int os s.EndLine;
    if s.Column <> -1 then 
      output_string os " : ";
      output_int os s.EndColumn;
    output_string os "*/ ";
    output_sqstring os s.Document.File


let rec goutput_instr env os inst =
  match inst with
  | si when isNoArgInstr si ->
       output_lid os (wordsOfNoArgInstr si)
  | I_brcmp (cmp,tg1,_tg2)  -> 
      output_string os 
          (match cmp with 
          | BI_beq -> "beq"
          | BI_bgt -> "bgt"
          | BI_bgt_un -> "bgt.un"
          | BI_bge -> "bge"
          | BI_bge_un -> "bge.un"
          | BI_ble -> "ble"
          | BI_ble_un -> "ble.un"
          | BI_blt -> "blt"
          | BI_blt_un -> "blt.un"
          | BI_bne_un -> "bne.un"
          | BI_brfalse -> "brfalse"
          | BI_brtrue -> "brtrue");
      output_string os " "; 
      output_code_label os tg1
  | I_br  tg -> output_string os "/* br "; output_code_label os tg;  output_string os "*/"; 
  | I_leave tg  -> output_string os "leave "; output_code_label os tg
  | I_call  (tl,mspec,varargs)  -> 
      output_tailness os tl;
      output_string os "call ";
      goutput_vararg_mspec env os (mspec,varargs);
      output_after_tailcall os tl;
  | I_calli (tl,mref,varargs) -> 
      output_tailness os tl;
      output_string os "calli ";
      goutput_vararg_sig env os (mref,varargs);
      output_after_tailcall os tl;
  | I_ldarg u16 -> output_string os "ldarg"; output_short_u16 os u16
  | I_ldarga  u16 -> output_string os "ldarga "; output_u16 os u16
  | (AI_ldc (dt, ILConst.I4 x)) -> 
      output_string os "ldc."; output_basic_type os dt; output_short_i32 os x
  | (AI_ldc (dt, ILConst.I8 x)) -> 
      output_string os "ldc."; output_basic_type os dt; output_string os " "; output_i64 os x
  | (AI_ldc (dt, ILConst.R4 x)) -> 
      output_string os "ldc."; output_basic_type os dt; output_string os " "; output_ieee32 os x
  | (AI_ldc (dt, ILConst.R8 x)) -> 
      output_string os "ldc."; output_basic_type os dt; output_string os " "; output_ieee64 os x
  | I_ldftn mspec ->  output_string os "ldftn "; goutput_mspec env os mspec
  | I_ldvirtftn mspec -> output_string os "ldvirtftn "; goutput_mspec env os mspec
  | I_ldind (al,vol,dt) -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "ldind.";
      output_basic_type os dt 
  | I_cpblk (al,vol)  -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "cpblk"
  | I_initblk (al,vol)  -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "initblk"
  | I_ldloc u16 -> output_string os "ldloc"; output_short_u16 os u16
  | I_ldloca  u16 -> output_string os "ldloca "; output_u16 os u16
  | I_starg u16 -> output_string os "starg "; output_u16 os u16
  | I_stind (al,vol,dt) -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "stind.";
      output_basic_type os dt 
  | I_stloc u16 -> output_string os "stloc"; output_short_u16 os u16
  | I_switch (l,_dflt) -> output_string os "switch "; output_parens (output_list "," output_code_label) os l
  | I_callvirt  (tl,mspec,varargs) -> 
      output_tailness os tl;
      output_string os "callvirt ";
      goutput_vararg_mspec env os (mspec,varargs);
      output_after_tailcall os tl;
  | I_callconstraint  (tl,ty,mspec,varargs) -> 
      output_tailness os tl;
      output_string os "constraint. ";
      goutput_typ env os ty;
      output_string os " callvirt ";
      goutput_vararg_mspec env os (mspec,varargs);
      output_after_tailcall os tl;
  | I_castclass ty  -> output_string os "castclass "; goutput_typ env os ty
  | I_isinst  ty  -> output_string os "isinst "; goutput_typ env os ty
  | I_ldfld (al,vol,fspec)  -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "ldfld ";
      goutput_fspec env os fspec
  | I_ldflda  fspec -> 
      output_string os "ldflda " ;
      goutput_fspec env os fspec
  | I_ldsfld  (vol,fspec) -> 
      output_volatility os vol;
      output_string os "ldsfld ";
      goutput_fspec env os fspec
  | I_ldsflda fspec -> 
      output_string os "ldsflda ";
      goutput_fspec env os fspec
  | I_stfld (al,vol,fspec)  -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "stfld ";
      goutput_fspec env os fspec
  | I_stsfld  (vol,fspec) -> 
      output_volatility os vol;
      output_string os "stsfld ";
      goutput_fspec env os fspec
  | I_ldtoken  tok  -> output_string os "ldtoken ";  goutput_ldtoken_info env os tok 
  | I_refanyval ty  -> output_string os "refanyval "; goutput_typ env os ty
  | I_refanytype  -> output_string os "refanytype"
  | I_mkrefany  typ -> output_string os "mkrefany "; goutput_typ env os typ
  | I_ldstr s -> 
      output_string os "ldstr "; 
      output_string os s
  | I_newobj  (mspec,varargs) -> 
      (* newobj: IL has a special rule that the CC is always implicitly "instance" and need *)
      (* not be mentioned explicitly *)
      output_string os "newobj "; 
      goutput_vararg_mspec env os (mspec,varargs)
  | I_stelem    dt      -> output_string os "stelem."; output_basic_type os dt 
  | I_ldelem    dt      -> output_string os "ldelem."; output_basic_type os dt 

  | I_newarr    (shape,typ) -> 
      if shape = ILArrayShape.SingleDimensional then 
        output_string os "newarr "; 
        goutput_typ_with_shortened_class_syntax env os typ
      else 
        output_string os "newobj void ";
        goutput_dlocref env os (mkILArrTy(typ,shape));
        output_string os ".ctor";
        let rank = shape.Rank 
        output_parens (output_list "," (goutput_typ env)) os (Array.toList (Array.create ( rank) ecmaILGlobals.typ_int32))
  | I_stelem_any (shape,dt)     -> 
      if shape = ILArrayShape.SingleDimensional then 
        output_string os "stelem.any "; goutput_typ env os dt 
      else 
        output_string os "call instance void ";
        goutput_dlocref env os (mkILArrTy(dt,shape));
        output_string os "Set";
        let rank = shape.Rank 
        output_parens (output_list "," (goutput_typ env)) os (Array.toList (Array.create ( rank) ecmaILGlobals.typ_int32) @ [dt])
  | I_ldelem_any (shape,tok) -> 
      if shape = ILArrayShape.SingleDimensional then 
        output_string os "ldelem.any "; goutput_typ env os tok 
      else 
        output_string os "call instance ";
        goutput_typ env os tok;
        output_string os " ";
        goutput_dlocref env os (mkILArrTy(tok,shape));
        output_string os "Get";
        let rank = shape.Rank 
        output_parens (output_list "," (goutput_typ env)) os (Array.toList (Array.create ( rank) ecmaILGlobals.typ_int32))
  | I_ldelema   (ro,shape,tok)  -> 
      if ro = ReadonlyAddress then output_string os "readonly. ";
      if shape = ILArrayShape.SingleDimensional then 
        output_string os "ldelema "; goutput_typ env os tok 
      else 
        output_string os "call instance ";
        goutput_typ env os (ILType.Byref tok);
        output_string os " ";
        goutput_dlocref env os (mkILArrTy(tok,shape));
        output_string os "Address";
        let rank = shape.Rank 
        output_parens (output_list "," (goutput_typ env)) os (Array.toList (Array.create ( rank) ecmaILGlobals.typ_int32))
      
  | I_box       tok     -> output_string os "box "; goutput_typ env os tok
  | I_unbox     tok     -> output_string os "unbox "; goutput_typ env os tok
  | I_unbox_any tok     -> output_string os "unbox.any "; goutput_typ env os tok
  | I_initobj   tok     -> output_string os "initobj "; goutput_typ env os tok
  | I_ldobj (al,vol,tok)        -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "ldobj "; 
      goutput_typ env os tok
  | I_stobj  (al,vol,tok) -> 
      output_alignment os al; 
      output_volatility os vol;
      output_string os "stobj "; 
      goutput_typ env os tok
  | I_cpobj tok -> output_string os "cpobj "; goutput_typ env os tok
  | I_sizeof  tok -> output_string os "sizeof "; goutput_typ env os tok
  | I_seqpoint  s -> output_source os s
  |  (EI_ilzero ty) -> output_string os "ilzero "; goutput_typ env os ty
  | I_other e when isIlxExtInstr e -> 
      match (destIlxExtInstr e) with 
      | EI_castdata (check,ty,n)     ->
          if not check then output_string os "/* unchecked. */ ";
          output_string os "castdata ";
          goutput_cuspec env os ty;
          output_string os ",";
          output_int os n
      | (EI_isdata (_,ty,n))  -> 
          output_string os "isdata "; 
          goutput_cuspec env os ty; 
          output_string os ",";  
          output_int os n
      |  (EI_brisdata (_,ty,n,tg1,_)) -> 
          output_string os "brisdata "; 
          goutput_cuspec env os ty; 
          output_string os ",";  
          output_string os "(";  
          output_int os n;
          output_string os ",";  
          output_code_label os tg1;
          output_string os ")"
      | (EI_lddata (_,ty,n,m))  -> 
          output_string os "lddata "; 
          goutput_cuspec env os ty; 
          output_string os ",";  
          output_int os n; 
          output_string os ","; 
          output_int os m
      | (EI_lddatatag (_,ty)) -> 
          output_string os "lddatatag "; 
          goutput_cuspec env os ty
      |  (EI_stdata (ty,n,m)) -> 
          output_string os "stdata "; 
          goutput_cuspec env os ty; 
          output_string os ",";  
          output_int os n; 
          output_string os ","; 
          output_int os m
      |  (EI_newdata (ty,n))  -> 
          output_string os "newdata "; 
          goutput_cuspec env os ty; 
          output_string os ",";  
          output_int os n
      |  (EI_datacase (_,ty,l,_))  -> 
          output_string os "datacase";
          output_string os " ";  
          goutput_cuspec env os ty;
          output_string os ",";  
          output_parens (output_list "," (fun os (x,y) -> output_int os x;  output_string os ",";  output_code_label os y)) os l
      |  (EI_ldenv n) -> output_string os "ldenv ";  output_int os n
      |  (EI_stenv n) -> output_string os "stenv ";  output_int os n
      |  (EI_ldenva n) -> output_string os "ldenva ";  output_int os n
      |  (EI_newclo clospec) -> output_string os "newclo "; goutput_clospec env os clospec
      |  (EI_isclo clospec) -> output_string os "isclo "; goutput_clospec env os clospec
      |  (EI_stclofld (clospec,n)) -> output_string os "stclofld "; goutput_clospec env os clospec;  output_string os " "; output_int os n
      |  (EI_castclo clospec) -> output_string os "castclo "; goutput_clospec env os clospec
      |  (EI_callclo (tl,clospec,apps)) ->  
          output_tailness os tl; 
          output_string os "callclo "; 
          goutput_clospec env os clospec;
          output_string os ", "; 
          goutput_apps env os apps;
          output_after_tailcall os tl;
      |  (EI_callfunc (tl,cs)) -> 
          output_tailness os tl; 
          output_string os "callfunc "; 
          goutput_apps env os cs;
          output_after_tailcall os tl;
  | _ -> 
      output_string os "<printing for this instruction is not implemented>"


let goutput_ilmbody env os il =
  if il.IsZeroInit then output_string os " .zeroinit\n";
  (* Add one to .maxstack if doing "calli" testing . *)
  output_string os " .maxstack ";
  output_i32 os (if !tailcall_via_ldftn || !call_via_ldftn then il.MaxStack+1 else il.MaxStack);
  output_string os "\n";
  let output_susp os susp = 
    match susp with
    | Some s -> 
        output_string os "\nbr "; output_code_label os s; output_string os "\n" 
    | _ -> () 
  let commit_susp os susp lab = 
    match susp with
    | Some s when s <> lab -> output_susp os susp
    | _ -> () 
  if il.Locals <> [] then 
    output_string os " .locals(";
    goutput_local env os (List.head il.Locals); 
    List.iter (fun l -> output_string os ",\n"; goutput_local env os l) (List.tail il.Locals); 
    output_string os ")\n"
  
  (* Print the code by left-to-right traversal *)
  let rec goutput_block env os (susp,block) = 
    match block with 
    | ILBasicBlock bb ->  
        commit_susp os susp bb.Label;
        output_code_label os bb.Label; output_string os ": \n"  ;
        Array.iter (fun i -> goutput_instr env os i; output_string os "\n") bb.Instructions;
        bb.Fallthrough
    | GroupBlock (_,l) -> 
        let new_susp = ref susp 
        List.iter (fun c -> new_susp := goutput_code env os (!new_susp,c)) l;
        !new_susp
    | RestrictBlock (_,c) -> goutput_code env os (susp,c)
    | TryBlock (c,seh) -> 

      commit_susp os susp (uniqueEntryOfCode c);
      output_string os " .try {\n";
      let susp = goutput_code env os (None,c) 
      if (susp <> None) then output_string os "// warning: fallthrough at end of try\n";
      output_string os "\n}";
      match seh with 
      |  FaultBlock flt -> 
          output_string os "fault {\n";
          output_susp os (goutput_code env os (None,flt));
          output_string os "\n}"
      | FinallyBlock flt -> 
          output_string os "finally {\n";
          output_susp os (goutput_code env os (None,flt));
          output_string os "\n}";
      | FilterCatchBlock clauses -> 
          List.iter 
             (fun (flt,ctch) -> 
                match flt with 
                    | TypeFilter typ ->
                        output_string os " catch ";
                        goutput_typ_with_shortened_class_syntax env os typ;
                        output_string os "{\n";
                        output_susp os (goutput_code env os (None,ctch));
                        output_string os "\n}"
                    | CodeFilter fltcode -> 
                        output_string os "filter {\n";
                        output_susp os (goutput_code env os (None,fltcode));
                        output_string os "\n} catch {\n";
                        output_susp os (goutput_code env os (None,ctch));
                        output_string os "\n}";)
             clauses
      None

  and goutput_code env os (susp,code) =
    goutput_block env os (susp,code)

  let goutput_topcode env os code = 
    let final_susp = goutput_code env os (Some (uniqueEntryOfCode code),code) 
    (match final_susp with Some s  -> output_string os "\nbr "; output_code_label os s; output_string os "\n" | _ -> ())

  goutput_topcode env os il.Code;

let goutput_mbody is_entrypoint env os md =
  match md.mdCodeKind with 
  | MethodCodeKind.Native -> output_string os "native "
  | MethodCodeKind.IL -> output_string os "cil "
  | MethodCodeKind.Runtime -> output_string os "runtime "
  
  output_string os (if md.IsInternalCall then "internalcall " else " ");
  output_string os (if md.IsManaged then "managed " else " ");
  output_string os (if md.IsForwardRef then "forwardref " else " ");
  output_string os " \n{ \n"  ;
  goutput_security_decls env os md.SecurityDecls;
  goutput_custom_attrs env os md.CustomAttrs;
  match md.mdBody.Contents with 
    | MethodBody.IL il -> goutput_ilmbody env os il
    | _ -> ()
  if is_entrypoint then output_string os " .entrypoint";
  output_string os "\n";
  output_string os "}\n"
  
let goutput_mdef env os md =
  let attrs = 
      match md.mdKind with
        | MethodKind.Virtual vinfo -> 
            "virtual "^
            (if vinfo.IsFinal then "final " else "")^
            (if vinfo.IsNewSlot then "newslot " else "")^
            (if vinfo.IsCheckAccessOnOverride then " strict " else "")^
            (if vinfo.IsAbstract then " abstract " else "")^
              "  "
        | MethodKind.NonVirtual ->     ""
        | MethodKind.Ctor -> "rtspecialname"
        | MethodKind.Static -> 
            "static "^
            (match md.mdBody.Contents with 
              MethodBody.PInvoke (attr) -> 
                "pinvokeimpl(\""^ attr.Where.Name^"\" as \""^ attr.Name ^"\""^
                (match attr.CallingConv with 
                | PInvokeCallingConvention.None -> ""
                | PInvokeCallingConvention.Cdecl -> " cdecl"
                | PInvokeCallingConvention.Stdcall -> " stdcall"
                | PInvokeCallingConvention.Thiscall -> " thiscall" 
                | PInvokeCallingConvention.Fastcall -> " fastcall"
                | PInvokeCallingConvention.WinApi -> " winapi" ) +

                (match attr.CharEncoding with 
                | PInvokeCharEncoding.None -> ""
                | PInvokeCharEncoding.Ansi -> " ansi"
                | PInvokeCharEncoding.Unicode -> " unicode"
                | PInvokeCharEncoding.Auto -> " autochar") +

                (if attr.NoMangle then " nomangle" else "") +
                (if attr.LastError then " lasterr" else "") +
                ")"
              | _ -> 
                  "")
        | MethodKind.Cctor -> "specialname rtspecialname static" 
  let is_entrypoint = md.IsEntryPoint 
  let menv = ppenv_enter_method (List.length md.GenericParams) env 
  output_string os " .method ";
  if md.IsHideBySig then output_string os "hidebysig ";
  if md.IsReqSecObj then output_string os "reqsecobj ";
  if md.IsSpecialName then output_string os "specialname ";
  if md.IsUnmanagedExport then output_string os "unmanagedexp ";
  output_member_access os md.Access;
  output_string os " ";
  output_string os attrs;
  output_string os " ";
  output_callconv os md.CallingConv;
  output_string os " ";
  (goutput_typ menv) os md.Return.Type;
  output_string os " ";
  output_id os md.Name ;
  output_string os " ";
  (goutput_gparams env) os md.GenericParams;
  output_string os " ";
  (goutput_params menv) os md.Parameters;
  output_string os " ";
  if md.IsSynchronized then output_string os "synchronized ";
  if md.IsMustRun then output_string os "/* mustrun */ ";
  if md.IsPreserveSig then output_string os "preservesig ";
  (goutput_mbody is_entrypoint menv) os md;
  output_string os "\n"

let goutput_pdef env os pd =
    output_string os  "property\n\tgetter: ";
    (match pd.GetMethod with None -> () | Some mref -> goutput_mref env os mref);
    output_string os  "\n\tsetter: ";
    (match pd.SetMethod with None -> () | Some mref -> goutput_mref env os mref)

let goutput_superclass env os = function 
    None -> ()
  | Some typ -> output_string os "extends "; (goutput_typ_with_shortened_class_syntax env) os typ

let goutput_superinterfaces env os imp =
  if imp = [] then () else
  output_string os "implements ";
  output_list "," (goutput_typ_with_shortened_class_syntax env) os imp

let goutput_implements env os imp =
  if imp = [] then () else
  output_string os "implements ";
  output_list "," (goutput_typ_with_shortened_class_syntax env) os imp

let the = function Some x -> x  | None -> failwith "the"

let output_type_layout_info os info =
  if info.Size <> None then (output_string os " .size "; output_i32 os (the info.Size));
  if info.Pack <> None then (output_string os " .pack "; output_u16 os (the info.Pack))

let splitTypeLayout = function
  | ILTypeDefLayout.Auto -> "auto",(fun _os () -> ())
  | ILTypeDefLayout.Sequential info ->  "sequential", (fun os () -> output_type_layout_info os info)
  | ILTypeDefLayout.Explicit info ->  "explicit", (fun os () -> output_type_layout_info os info)

      
let goutput_fdefs tref env os (fdefs: ILFieldDefs) = 
  List.iter (fun f -> (goutput_fdef tref env) os f; output_string os "\n" ) fdefs.AsList
let goutput_mdefs env os (mdefs: ILMethodDefs) = 
  List.iter (fun f -> (goutput_mdef env) os f; output_string os "\n" ) mdefs.AsList
let goutput_pdefs env os (pdefs: ILPropertyDefs) = 
  List.iter (fun f -> (goutput_pdef env) os f; output_string os "\n" ) pdefs.AsList

let rec goutput_tdef (enc) env contents os cd =
  let env = ppenv_enter_tdef cd.GenericParams env 
  let layout_attr,pp_layout_decls = splitTypeLayout cd.Layout 
  if isTypeNameForGlobalFunctions cd.Name then 
      if contents then 
          let tref = (mkILNestedTyRef (ILScopeRef.Local,enc,cd.Name)) 
          goutput_mdefs env os cd.Methods;
          goutput_fdefs tref env os cd.Fields;
          goutput_pdefs env os cd.Properties;
  else 
    let isclo = 
      match cd.tdKind with 
      | ILTypeDefKind.Other e when isIlxExtTypeDefKind e ->
          match destIlxExtTypeDefKind e with 
          | IlxTypeDefKind.Closure _ ->  true
          | _ -> false
      | _ -> false 
    let isclassunion = 
      match cd.tdKind with 
      | ILTypeDefKind.Other e when isIlxExtTypeDefKind e ->
          match destIlxExtTypeDefKind e with 
          | IlxTypeDefKind.Union _ ->  true
          | _ -> false
      | _ -> false 
    if not (isclo || isclassunion) || contents then 
      output_string os "\n";
      match cd.tdKind with 
      | ILTypeDefKind.Class | ILTypeDefKind.Enum | ILTypeDefKind.Delegate | ILTypeDefKind.ValueType -> output_string os ".class "
      | ILTypeDefKind.Interface ->  output_string os ".class  interface "
      | ILTypeDefKind.Other e when isIlxExtTypeDefKind e -> 
          match destIlxExtTypeDefKind e with 
          | IlxTypeDefKind.Closure _ ->  output_string os ".closure "
          | IlxTypeDefKind.Union  _ ->  output_string os ".classunion "
      | ILTypeDefKind.Other _ -> failwith "unknown extension" 
      output_init_semantics os cd.InitSemantics;
      output_string os " ";
      output_type_access os cd.Access;
      output_string os " ";
      output_encoding os cd.Encoding;
      output_string os " ";
      output_string os layout_attr;
      output_string os " ";
      if cd.IsSealed then  output_string os "sealed ";
      if cd.IsAbstract then  output_string os "abstract ";
      if cd.IsSerializable then  output_string os "serializable ";
      if cd.IsComInterop then  output_string os "import ";
      output_sqstring os cd.Name ;
      goutput_gparams env os cd.GenericParams;
      output_string os "\n\t";
      if isclo then 
        match cd.tdKind with 
        | ILTypeDefKind.Other e when isIlxExtTypeDefKind e ->
            match destIlxExtTypeDefKind e with 
            | IlxTypeDefKind.Closure cloinfo ->  goutput_freevars env os cloinfo.cloFreeVars
            | _ -> ()
        | _ -> ()
      else 
          goutput_superclass env os cd.Extends;
          output_string os "\n\t";
      goutput_implements env os cd.Implements;
      output_string os "\n{\n ";
      if contents then 
        let tref = (mkILNestedTyRef (ILScopeRef.Local,enc,cd.Name)) 
        goutput_custom_attrs env os cd.CustomAttrs;
        goutput_security_decls env os cd.SecurityDecls;
        pp_layout_decls os ();
        goutput_fdefs tref env os cd.Fields;
        goutput_mdefs env os cd.Methods;
        match cd.tdKind with 
        | ILTypeDefKind.Other e when isIlxExtTypeDefKind e -> 
            match destIlxExtTypeDefKind e with 
            | IlxTypeDefKind.Closure x ->  
                output_string os "\n.apply ";
                (goutput_lambdas env) os x.cloStructure;
                output_string os "\n { ";
                (goutput_ilmbody env) os (Lazy.force x.cloCode);
                output_string os "}\n";
            | IlxTypeDefKind.Union x ->  
                Array.iter (fun x -> output_string os " .alternative "; 
                                     goutput_alternative_ref env os x) x.cudAlternatives;
        | _ -> ()
      goutput_tdefs contents  (enc@[cd.Name]) env os cd.NestedTypes;
      output_string os "\n}";

and output_init_semantics os f =
  match f with 
    ILTypeInit.BeforeField -> output_string os "beforefieldinit";
  | ILTypeInit.OnAny -> ()

and goutput_lambdas env os lambdas = 
  match lambdas with
   | Lambdas_forall (gf,l) -> 
       output_angled (goutput_gparam env) os gf; 
       output_string os " "; 
       (goutput_lambdas env) os l
   | Lambdas_lambda (ps,l) ->  
       output_parens (goutput_param env) os ps; 
       output_string os " ";
       (goutput_lambdas env) os l
   | Lambdas_return typ -> output_string os "--> "; (goutput_typ env) os typ
  
and goutput_tdefs contents (enc) env os (td: ILTypeDefs) =
  List.iter (goutput_tdef enc env contents os) td.AsList

let output_ver os (a,b,c,d) =
    output_string os " .ver ";
    output_u16 os a;
    output_string os " : ";
    output_u16 os b;
    output_string os " : ";
    output_u16 os c;
    output_string os " : ";
    output_u16 os d

let output_locale os s = output_string os " .Locale "; output_qstring os s

let output_hash os x = 
    output_string os " .hash = "; output_parens output_bytes os x 
let output_publickeytoken os x = 
  output_string os " .publickeytoken = "; output_parens output_bytes os x 
let output_publickey os x = 
  output_string os " .publickey = "; output_parens output_bytes os x 

let output_publickeyinfo os = function
  | PublicKey k -> output_publickey os k
  | PublicKeyToken k -> output_publickeytoken os k

let output_assref os (aref:ILAssemblyRef) =
  output_string os " .assembly extern ";
  output_sqstring os aref.Name;
  if aref.Retargetable then output_string os " retargetable "; 
  output_string os " { ";
  (output_option output_hash) os aref.Hash;
  (output_option output_publickeyinfo) os aref.PublicKey;
  (output_option output_ver) os aref.Version;
  (output_option output_locale) os aref.Locale;
  output_string os " } "

let output_modref os (modref:ILModuleRef) =
  output_string os (if modref.HasMetadata then " .module extern " else " .file nometadata " );
  output_sqstring os modref.Name;
  (output_option output_hash) os modref.Hash

let goutput_resource env os r = 
  output_string os " .mresource ";
  output_string os (match r.Access with ILResourceAccess.Public -> " public " | ILResourceAccess.Private -> " private ");
  output_sqstring os r.Name;
  output_string os " { ";
  goutput_custom_attrs env os r.CustomAttrs;
  match r.Location with 
  | ILResourceLocation.Local _ -> 
      output_string os " /* loc nyi */ "; 
  | ILResourceLocation.File (mref,off) ->
      output_string os " .file "; 
      output_sqstring os mref.Name;
      output_string os "  at "; 
      output_i32 os off 
  | ILResourceLocation.Assembly aref -> 
      output_string os " .assembly extern "; 
      output_sqstring os aref.Name
  output_string os " }\n "

let goutput_manifest env os m = 
  output_string os " .assembly "; 
  match m.AssemblyLongevity with 
            | ILAssemblyLongevity.Unspecified -> ()
            | ILAssemblyLongevity.Library -> output_string os "library "; 
            | ILAssemblyLongevity.PlatformAppDomain -> output_string os "platformappdomain "; 
            | ILAssemblyLongevity.PlatformProcess -> output_string os "platformprocess "; 
            | ILAssemblyLongevity.PlatformSystem  -> output_string os "platformmachine "; 
  output_sqstring os m.Name;
  output_string os " { \n";
  output_string os ".hash algorithm "; output_i32 os m.AuxModuleHashAlgorithm; output_string os "\n";
  goutput_custom_attrs env os m.CustomAttrs;
  goutput_security_decls env os m.SecurityDecls;
  (output_option output_publickey) os m.PublicKey;
  (output_option output_ver) os m.Version;
  (output_option output_locale) os m.Locale;
  output_string os " } \n"


let output_module_fragment_aux _refs os  modul = 
  try 
    let env = mk_ppenv 
    let env = ppenv_enter_modul env 
    goutput_tdefs false ([]) env os modul.TypeDefs;
    goutput_tdefs true ([]) env os modul.TypeDefs;
  with e ->  
    output_string os "*** Error during printing : "; output_string os (e.ToString()); os.Flush();
    reraise()
    raise e

let output_module_fragment os  modul = 
  let refs = computeILRefs modul 
  output_module_fragment_aux refs os  modul;
  refs

let output_module_refs os refs = 
  List.iter (fun  x -> output_assref os x; output_string os "\n") refs.AssemblyReferences;
  List.iter (fun x -> output_modref os x; output_string os "\n") refs.ModuleReferences
  
let goutput_module_manifest env os modul = 
  output_string os " .module "; output_sqstring os modul.Name;
  goutput_custom_attrs env os modul.CustomAttrs;
  output_string os " .imagebase "; output_i32 os modul.ImageBase;
  output_string os " .file alignment "; output_i32 os modul.PhysicalAlignment;
  output_string os " .subsystem "; output_i32 os modul.SubSystemFlags;
  output_string os " .corflags "; output_i32 os ((if modul.IsILOnly then 0x0001 else 0) ||| (if modul.Is32Bit then 0x0002 else 0));
  List.iter (fun r -> goutput_resource env os r) modul.Resources.AsList;
  output_string os "\n";
  (output_option (goutput_manifest env)) os modul.Manifest

let output_module os  modul = 
  try 
    let refs = computeILRefs modul 
    let env = mk_ppenv 
    let env = ppenv_enter_modul env 
    output_module_refs  os refs;
    goutput_module_manifest env os modul;
    output_module_fragment_aux refs os  modul;
  with e ->  
    output_string os "*** Error during printing : "; output_string os (e.ToString()); os.Flush();
    raise e


#endif
    

    
  
