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

/// ILX extensions to Abstract IL types and instructions F# 
module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.IL 

// -------------------------------------------------------------------- 
// Union references 
// -------------------------------------------------------------------- 

type IlxUnionAlternative = 
    { altName: string;
      altFields: ILFieldDef array;
      altCustomAttrs: ILAttributes }

    member FieldDefs : ILFieldDef array
    member FieldDef : int -> ILFieldDef 
    member Name : string
    member IsNullary  : bool
    member FieldTypes : ILType list


type IlxUnionHasHelpers = 
   | NoHelpers
   | AllHelpers
   | SpecialFSharpListHelpers 
   | SpecialFSharpOptionHelpers 
   
type IlxUnionRef = 
    | IlxUnionRef of ILTypeRef * IlxUnionAlternative array * bool (* cudNullPermitted *)  * IlxUnionHasHelpers (* cudHasHelpers *)

type IlxUnionSpec = 
    | IlxUnionSpec of IlxUnionRef * ILGenericArgs
    member EnclosingType : ILType
    member GenericArgs : ILGenericArgs
    member Alternatives : IlxUnionAlternative list
    member AlternativesArray : IlxUnionAlternative array
    member TypeRef : ILTypeRef 
    member IsNullPermitted : bool
    member HasHelpers : IlxUnionHasHelpers
    member Alternative : int -> IlxUnionAlternative
    member FieldDef : int -> int -> ILFieldDef

// -------------------------------------------------------------------- 
// Closure references 
// -------------------------------------------------------------------- 

type IlxClosureLambdas = 
    | Lambdas_forall of ILGenericParameterDef * IlxClosureLambdas
    | Lambdas_lambda of ILParameter * IlxClosureLambdas
    | Lambdas_return of ILType

type IlxClosureFreeVar = 
    { fvName: string ; 
      fvCompilerGenerated:bool; 
      fvType: ILType }

type IlxClosureRef = 
    | IlxClosureRef of ILTypeRef * IlxClosureLambdas * IlxClosureFreeVar list 

type IlxClosureSpec = 
    | IlxClosureSpec of IlxClosureRef * ILGenericArgs

    member TypeRef : ILTypeRef
    member ClosureRef : IlxClosureRef
    member FormalFreeVarType : int -> ILType
    member FormalFreeVars : IlxClosureFreeVar list
    member ActualFreeVars : IlxClosureFreeVar list
    member ActualLambdas : IlxClosureLambdas
    member FormalLambdas : IlxClosureLambdas
    member GenericArgs : ILGenericArgs

/// IlxClosureApps - i.e. types being applied at a callsite
type IlxClosureApps = 
    | Apps_tyapp of ILType * IlxClosureApps 
    | Apps_app of ILType * IlxClosureApps 
    | Apps_done of ILType

/// ILX extensions to the intruction set
///

type IlxInstr = 
    | EI_lddata of (* avoidHelpers: *) bool * IlxUnionSpec * int * int
    | EI_isdata of (* avoidHelpers: *) bool * IlxUnionSpec * int
    | EI_brisdata of (* avoidHelpers: *) bool * IlxUnionSpec * int * ILCodeLabel * ILCodeLabel
    | EI_castdata of bool * IlxUnionSpec * int
    | EI_stdata of IlxUnionSpec * int * int
    | EI_datacase of (* avoidHelpers: *) bool * IlxUnionSpec * (int * ILCodeLabel) list * ILCodeLabel (* last label is fallthrough *)
    | EI_lddatatag of (* avoidHelpers: *) bool * IlxUnionSpec
    | EI_newdata of IlxUnionSpec * int
    | EI_newclo of IlxClosureSpec
    | EI_castclo of IlxClosureSpec
    | EI_isclo of IlxClosureSpec
    | EI_callclo of ILTailcall * IlxClosureSpec * IlxClosureApps
    | EI_stclofld  of (IlxClosureSpec * int)  
    | EI_stenv  of int
    | EI_ldenv  of int
    | EI_ldenva  of int
    | EI_callfunc of ILTailcall * IlxClosureApps

val mkIlxExtInstr: (IlxInstr -> IlxExtensionInstr)
val isIlxExtInstr: (IlxExtensionInstr -> bool)
val destIlxExtInstr: (IlxExtensionInstr -> IlxInstr)

val mkIlxInstr: IlxInstr -> ILInstr

// -------------------------------------------------------------------- 
// ILX extensions to the kinds of type definitions available
// -------------------------------------------------------------------- 

type IlxClosureInfo = 
    { cloStructure: IlxClosureLambdas;
      cloFreeVars: IlxClosureFreeVar list;  
      cloCode: Lazy<ILMethodBody>;
      cloSource: ILSourceMarker option}

and IlxUnionInfo = 
    { /// Is the representation public? 
      cudReprAccess: ILMemberAccess; 
      /// Are the representation helpers public? 
      cudHelpersAccess: ILMemberAccess; 
      /// Generate the helpers? 
      cudHasHelpers: IlxUnionHasHelpers; 
      cudDebugProxies: bool; 
      cudDebugDisplayAttributes: ILAttribute list;
      cudAlternatives: IlxUnionAlternative array;
      cudNullPermitted: bool;
      /// Debug info for generated code for classunions 
      cudWhere: ILSourceMarker option;  
    }

type IlxTypeDefKind = 
   | Closure of IlxClosureInfo
   | Union of IlxUnionInfo

val mkIlxExtTypeDefKind: (IlxTypeDefKind -> IlxExtensionTypeKind)
val isIlxExtTypeDefKind: (IlxExtensionTypeKind -> bool)
val destIlxExtTypeDefKind: (IlxExtensionTypeKind -> IlxTypeDefKind)

val mkIlxTypeDefKind: IlxTypeDefKind -> ILTypeDefKind

// -------------------------------------------------------------------- 
// MS-ILX constructs: Closures, thunks, classunions
// -------------------------------------------------------------------- 

val instAppsAux: int -> ILGenericArgs -> IlxClosureApps -> IlxClosureApps
val destTyFuncApp: IlxClosureApps -> ILType * IlxClosureApps

val mkILFormalCloRef: ILGenericParameterDefs -> IlxClosureRef -> IlxClosureSpec


// -------------------------------------------------------------------- 
// MS-ILX: Unions
// -------------------------------------------------------------------- 


val actualTypOfIlxUnionField: IlxUnionSpec -> int -> int -> ILType

val mkILFreeVar: string * bool * ILType -> IlxClosureFreeVar
