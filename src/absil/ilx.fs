//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------


/// Defines an extension of the IL algebra
module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 

// --------------------------------------------------------------------
// Define an extension of the IL instruction algebra
// -------------------------------------------------------------------- 

let mkLowerName (nm: string) =
    // Use the lower case name of a field or constructor as the field/parameter name if it differs from the uppercase name
    let lowerName = String.uncapitalize nm
    if lowerName = nm then "_" + nm else lowerName

[<Sealed>]
type IlxUnionField(fd: ILFieldDef) =
    let lowerName = mkLowerName fd.Name
    member x.ILField = fd
    member x.Type = x.ILField.Type
    member x.Name = x.ILField.Name
    member x.LowerName = lowerName
    

type IlxUnionAlternative = 
    { altName: string;
      altFields: IlxUnionField[];
      altCustomAttrs: ILAttributes }

    member x.FieldDefs = x.altFields
    member x.FieldDef n = x.altFields.[n]
    member x.Name = x.altName
    member x.IsNullary  = (x.FieldDefs.Length = 0)
    member x.FieldTypes = x.FieldDefs |> Array.map (fun fd -> fd.Type) 

type IlxUnionHasHelpers = 
   | NoHelpers
   | AllHelpers
   | SpecialFSharpListHelpers 
   | SpecialFSharpOptionHelpers 
   
type IlxUnionRef = 
    | IlxUnionRef of ILTypeRef * IlxUnionAlternative[] * bool * (* hasHelpers: *) IlxUnionHasHelpers 

type IlxUnionSpec = 
    | IlxUnionSpec of IlxUnionRef * ILGenericArgs
    member x.EnclosingType = let (IlxUnionSpec(IlxUnionRef(tref,_,_,_),inst)) = x in mkILBoxedTyRaw tref inst
    member x.TypeRef = let (IlxUnionSpec(IlxUnionRef(tref,_,_,_),_)) = x in tref
    member x.GenericArgs = let (IlxUnionSpec(_,inst)) = x in inst
    member x.AlternativesArray = let (IlxUnionSpec(IlxUnionRef(_,alts,_,_),_)) = x in alts
    member x.IsNullPermitted = let (IlxUnionSpec(IlxUnionRef(_,_,np,_),_)) = x in np
    member x.HasHelpers = let (IlxUnionSpec(IlxUnionRef(_,_,_,b),_)) = x in b
    member x.Alternatives = Array.toList x.AlternativesArray
    member x.Alternative idx = x.AlternativesArray.[idx]
    member x.FieldDef idx fidx = x.Alternative(idx).FieldDef(fidx)


type IlxClosureLambdas = 
    | Lambdas_forall of ILGenericParameterDef * IlxClosureLambdas
    | Lambdas_lambda of ILParameter * IlxClosureLambdas
    | Lambdas_return of ILType

type IlxClosureApps = 
  | Apps_tyapp of ILType * IlxClosureApps 
  | Apps_app of ILType * IlxClosureApps 
  | Apps_done of ILType

let rec instAppsAux n inst = function
    Apps_tyapp (ty,rty) -> Apps_tyapp(instILTypeAux n inst ty, instAppsAux n inst rty)
  | Apps_app (dty,rty) ->  Apps_app(instILTypeAux n inst dty, instAppsAux n inst rty)
  | Apps_done rty ->  Apps_done(instILTypeAux n inst rty)

let rec instLambdasAux n inst = function
  | Lambdas_forall (b,rty) -> 
      Lambdas_forall(b, instLambdasAux n inst rty)
  | Lambdas_lambda (p,rty) ->  
      Lambdas_lambda({ p with Type=instILTypeAux n inst p.Type},instLambdasAux n inst rty)
  | Lambdas_return rty ->  Lambdas_return(instILTypeAux n inst rty)

let instLambdas i t = instLambdasAux 0 i t

type IlxClosureFreeVar = 
    { fvName: string ; 
      fvCompilerGenerated:bool; 
      fvType: ILType }

let mkILFreeVar (name,compgen,ty) = 
    { fvName=name;
      fvCompilerGenerated=compgen;
      fvType=ty; }


type IlxClosureRef = 
    | IlxClosureRef of ILTypeRef * IlxClosureLambdas * IlxClosureFreeVar[]
    
type IlxClosureSpec = 
    | IlxClosureSpec of IlxClosureRef * ILGenericArgs * ILType
    member x.TypeRef = let (IlxClosureRef(tref,_,_)) = x.ClosureRef in tref
    member x.ILType = let (IlxClosureSpec(_,_,ty)) = x in ty
    member x.ClosureRef = let (IlxClosureSpec(cloref,_,_)) = x in cloref 
    member x.FormalFreeVars = let (IlxClosureRef(_,_,fvs)) = x.ClosureRef in fvs
    member x.FormalLambdas = let (IlxClosureRef(_,lambdas,_)) = x.ClosureRef in lambdas
    member x.GenericArgs = let (IlxClosureSpec(_,inst,_)) = x in inst
    static member Create (cloref, inst) = 
        let (IlxClosureRef(tref,_,_)) = cloref
        IlxClosureSpec(cloref, inst, mkILBoxedType (mkILTySpecRaw(tref, inst)))
    member clospec.Constructor = 
        let cloTy = clospec.ILType
        let fields = clospec.FormalFreeVars
        mkILCtorMethSpecForTy (cloTy,fields |> Array.map (fun fv -> fv.fvType) |> Array.toList)


type IlxInstr = 
  // Discriminated unions
  | EI_lddata of (* avoidHelpers: *) bool * IlxUnionSpec * int * int
  | EI_isdata of (* avoidHelpers: *) bool * IlxUnionSpec * int
  | EI_brisdata of (* avoidHelpers: *) bool * IlxUnionSpec * int * ILCodeLabel * ILCodeLabel
  | EI_castdata of bool * IlxUnionSpec * int
  | EI_stdata of IlxUnionSpec * int * int
  | EI_datacase of (* avoidHelpers: *) bool * IlxUnionSpec * (int * ILCodeLabel) list * ILCodeLabel (* last label is fallthrough *)
  | EI_lddatatag of (* avoidHelpers: *) bool * IlxUnionSpec
  | EI_newdata of IlxUnionSpec * int
  
  // Closures
  | EI_callfunc of ILTailcall * IlxClosureApps

let destinations i =
  match i with 
  |  (EI_brisdata (_,_,_,l1,l2)) ->  [l1; l2]
  |  (EI_callfunc (Tailcall,_)) ->   []
  |  (EI_datacase (_,_,ls,l)) -> l:: (List.foldBack (fun (_,l) acc -> ListSet.insert l acc) ls [])
  | _ -> []

let fallthrough i = 
  match i with 
  |  (EI_brisdata (_,_,_,_,l)) 
  |  (EI_datacase (_,_,_,l)) -> Some l
  | _ -> None

let isTailcall i = 
  match i with 
  |  (EI_callfunc (Tailcall,_)) -> true
  | _ -> false

let remapIlxLabels lab2cl i = 
  match i with 
    | EI_brisdata (z,a,b,l1,l2) -> EI_brisdata (z,a,b,lab2cl l1,lab2cl l2)
    | EI_datacase (z,x,ls,l) -> EI_datacase (z,x,List.map (fun (y,l) -> (y,lab2cl l)) ls, lab2cl l)
    | _ -> i

let (mkIlxExtInstr,isIlxExtInstr,destIlxExtInstr) = 
  RegisterInstructionSetExtension  
    { instrExtDests=destinations;
      instrExtFallthrough=fallthrough;
      instrExtIsTailcall=isTailcall;
      instrExtRelabel=remapIlxLabels; }

let mkIlxInstr i = I_other (mkIlxExtInstr i)

// Define an extension of the IL algebra of type definitions
type IlxClosureInfo = 
    { cloStructure: IlxClosureLambdas;
      cloFreeVars: IlxClosureFreeVar[];  
      cloCode: Lazy<ILMethodBody>;
      cloSource: ILSourceMarker option}

and IlxUnionInfo = 
    { cudReprAccess: ILMemberAccess; (* is the representation public? *)
      cudHelpersAccess: ILMemberAccess; (* are the representation public? *)
      cudHasHelpers: IlxUnionHasHelpers; (* generate the helpers? *)
      cudDebugProxies: bool; (* generate the helpers? *)
      cudDebugDisplayAttributes: ILAttribute list;
      cudAlternatives: IlxUnionAlternative array;
      cudNullPermitted: bool;
      (* debug info for generated code for classunions *) 
      cudWhere: ILSourceMarker option; }

type IlxTypeDefKind = 
 | Closure of IlxClosureInfo
 | Union of IlxUnionInfo

let (mkIlxExtTypeDefKind,isIlxExtTypeDefKind,destIlxExtTypeDefKind) = 
  (RegisterTypeDefKindExtension TypeDefKindExtension : (IlxTypeDefKind -> IlxExtensionTypeKind) * (IlxExtensionTypeKind -> bool) * (IlxExtensionTypeKind -> IlxTypeDefKind) )

let mkIlxTypeDefKind i = ILTypeDefKind.Other (mkIlxExtTypeDefKind i)

// --------------------------------------------------------------------
// Define these as extensions of the IL types
// -------------------------------------------------------------------- 

let destTyFuncApp = function Apps_tyapp (b,c) -> b,c | _ -> failwith "destTyFuncApp"

let mkILFormalCloRef gparams csig = IlxClosureSpec.Create(csig, mkILFormalGenericArgsRaw gparams)

let actualTypOfIlxUnionField (cuspec : IlxUnionSpec) idx fidx =
  instILType cuspec.GenericArgs (cuspec.FieldDef idx fidx).Type

