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


/// Derived expression manipulation and construction functions.
module internal Microsoft.FSharp.Compiler.Tastops 

open System.Collections.Generic 
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.PrettyNaming


//---------------------------------------------------------------------------
// Basic data structures
//---------------------------------------------------------------------------

[<NoEquality; NoComparison>]
type TyparMap<'T> = 
    | TPMap of StampMap<'T>
    member tm.Item with get (v: Typar) = let (TPMap m) = tm in m.[v.Stamp]
    member tm.ContainsKey (v: Typar) = let (TPMap m) = tm in m.ContainsKey(v.Stamp)
    member tm.Add (v: Typar, x) = let (TPMap m) = tm in TPMap (m.Add(v.Stamp,x))
    static member Empty : TyparMap<'T> = TPMap Map.empty

[<NoEquality; NoComparison; Sealed>]
type TyconRefMap<'T>(imap: StampMap<'T>) =
    member m.Item with get (v: TyconRef) = imap.[v.Stamp]
    member m.TryFind (v: TyconRef) = imap.TryFind v.Stamp 
    member m.ContainsKey (v: TyconRef) =  imap.ContainsKey v.Stamp 
    member m.Add (v: TyconRef) x = TyconRefMap (imap.Add (v.Stamp,x))
    member m.Remove (v: TyconRef) = TyconRefMap (imap.Remove v.Stamp)
    member m.IsEmpty = imap.IsEmpty

    static member Empty : TyconRefMap<'T> = TyconRefMap Map.empty
    static member OfList vs = (vs, TyconRefMap<'T>.Empty) ||> List.foldBack (fun (x,y) acc -> acc.Add x y) 

[<Struct>]
[<NoEquality; NoComparison>]
type ValMap<'T>(imap: StampMap<'T>) = 
     
    member m.Contents = imap
    member m.Item with get (v:Val) = imap.[v.Stamp]
    member m.TryFind (v: Val) = imap.TryFind v.Stamp 
    member m.ContainsVal (v: Val) = imap.ContainsKey v.Stamp 
    member m.Add (v: Val) x = ValMap (imap.Add(v.Stamp,x))
    member m.Remove (v: Val) = ValMap (imap.Remove(v.Stamp))
    static member Empty = ValMap<'T> Map.empty
    member m.IsEmpty = imap.IsEmpty
    static member OfList vs = (vs, ValMap<'T>.Empty) ||> List.foldBack (fun (x,y) acc -> acc.Add x y) 


//--------------------------------------------------------------------------
// renamings
//--------------------------------------------------------------------------

type TyparInst = (Typar * TType) list

type TyconRefRemap = TyconRefMap<TyconRef>
type ValRemap = ValMap<ValRef>

let emptyTyconRefRemap : TyconRefRemap = TyconRefMap<_>.Empty
let emptyTyparInst = ([] : TyparInst)

[<NoEquality; NoComparison>]
type Remap =
    { tpinst : TyparInst;
      valRemap: ValRemap;
      tyconRefRemap : TyconRefRemap }

let emptyRemap = 
    { tpinst        = emptyTyparInst; 
      tyconRefRemap = emptyTyconRefRemap;
      valRemap      = ValMap.Empty }

type Remap with 
    static member Empty = emptyRemap

//--------------------------------------------------------------------------
// Substitute for type variables and remap type constructors 
//--------------------------------------------------------------------------

let addTyconRefRemap tcref1 tcref2 tmenv = 
    {tmenv with tyconRefRemap=tmenv.tyconRefRemap.Add tcref1 tcref2 }

let isRemapEmpty remap = 
    List.isEmpty remap.tpinst && 
    remap.tyconRefRemap.IsEmpty && 
    remap.valRemap.IsEmpty 

let rec instTyparRef tpinst ty tp  =
    match tpinst with 
    | [] -> ty
    | (tp',ty')::t -> 
        if typarEq tp tp' then ty' 
        else instTyparRef t ty tp

let instUnitTyparRef tpinst unt (tp:Typar)  =
   match tp.Kind with 
   | KindType -> failwith "instUnitTyparRef: kind=Type"
   | KindMeasure ->
        let rec loop tpinst = 
            match tpinst with 
            | [] -> unt
            | (tp',ty')::t -> 
                if typarEq tp tp' then 
                    match ty' with 
                    | TType_measure unt -> unt
                    | _ -> failwith "instUnitTyparRef incorrect kind";
                else
                    loop t
        loop tpinst

let remapTyconRef (tcmap: TyconRefMap<_>) tcr  =
    match tcmap.TryFind tcr with 
    | Some tcr ->  tcr
    | None -> tcr

let remapUnionCaseRef tcmap (UCRef(tcref,nm)) = UCRef(remapTyconRef tcmap tcref,nm)
let remapRecdFieldRef tcmap (RFRef(tcref,nm)) = RFRef(remapTyconRef tcmap tcref,nm)

let mkTyparInst (typars: Typars) tyargs =  
#if CHECKED
    if List.length typars <> List.length tyargs then
      failwith ("mkTyparInst: invalid type" + (sprintf " %d <> %d" (List.length typars) (List.length tyargs)));
#endif
    (List.zip typars tyargs : TyparInst)

let generalizeTypar tp = mkTyparTy tp
let generalizeTypars tps = List.map generalizeTypar tps

let rec remapTypeAux (tyenv : Remap) (ty:TType) =
  let ty = stripTyparEqns ty
  match ty with
  | TType_var tp as ty       -> instTyparRef tyenv.tpinst ty tp
  | TType_app (tcr,tinst) as ty -> 
      match tyenv.tyconRefRemap.TryFind tcr with 
      | Some tcr' ->  TType_app (tcr',remapTypesAux tyenv tinst)
      | None -> 
          match tinst with 
          | [] -> ty  // optimization to avoid re-allocation of TType_app node in the common case 
          | _ -> 
              // avoid reallocation on idempotent 
              let tinst' = remapTypesAux tyenv tinst
              if tinst === tinst' then ty else 
              TType_app (tcr,tinst')

  | TType_ucase (UCRef(tcr,n),tinst) -> 
      match tyenv.tyconRefRemap.TryFind tcr with 
      | Some tcr' ->  TType_ucase (UCRef(tcr',n),remapTypesAux tyenv tinst)
      | None -> TType_ucase (UCRef(tcr,n),remapTypesAux tyenv tinst)

  | TType_tuple l  as ty -> 
      let l' = remapTypesAux tyenv l
      if l === l' then ty else  
      TType_tuple (l')
  | TType_fun (d,r) as ty      -> 
      let d' = remapTypeAux tyenv d
      let r' = remapTypeAux tyenv r
      if d === d' && r === r' then ty else
      TType_fun (d', r')
  | TType_forall (tps,ty) -> 
      let tps',tyenv = copyAndRemapAndBindTypars tyenv tps
      TType_forall (tps', remapTypeAux tyenv ty)
  | TType_measure unt -> 
      TType_measure (remapMeasureAux tyenv unt)


and remapMeasureAux tyenv unt =
    match unt with
    | MeasureOne -> unt
    | MeasureCon tcr ->
        match tyenv.tyconRefRemap.TryFind tcr with 
        | Some tcr ->  MeasureCon tcr
        | None -> unt
    | MeasureProd(u1,u2) -> MeasureProd(remapMeasureAux tyenv u1, remapMeasureAux tyenv u2)
    | MeasureInv u -> MeasureInv(remapMeasureAux tyenv u)
    | MeasureVar tp as unt -> 
      match tp.Solution with
       | None -> 
          if ListAssoc.containsKey typarEq tp tyenv.tpinst then 
              match ListAssoc.find typarEq tp tyenv.tpinst with 
              | TType_measure unt -> unt
              | _ -> failwith "remapMeasureAux: incorrect kinds"
          else unt
       | Some (TType_measure unt) -> remapMeasureAux tyenv unt
       | Some ty -> failwithf "incorrect kinds: %A" ty
and remapTypesAux tyenv types = List.mapq (remapTypeAux tyenv) types
and remapTyparConstraintsAux tyenv cs =
   cs |>  List.choose (fun x -> 
         match x with 
         | TTyparCoercesToType(ty,m) -> 
             Some(TTyparCoercesToType (remapTypeAux tyenv ty,m))
         | TTyparMayResolveMemberConstraint(traitInfo,m) -> 
             Some(TTyparMayResolveMemberConstraint (remapTraitAux tyenv traitInfo,m))
         | TTyparDefaultsToType(priority,ty,m) -> Some(TTyparDefaultsToType(priority,remapTypeAux tyenv ty,m))
         | TTyparIsEnum(uty,m) -> 
             Some(TTyparIsEnum(remapTypeAux tyenv uty,m))
         | TTyparIsDelegate(uty1,uty2,m) -> 
             Some(TTyparIsDelegate(remapTypeAux tyenv uty1,remapTypeAux tyenv uty2,m))
         | TTyparSimpleChoice(tys,m) -> Some(TTyparSimpleChoice(remapTypesAux tyenv tys,m))
         | TTyparSupportsComparison  _ 
         | TTyparSupportsEquality  _ 
         | TTyparSupportsNull _ 
         | TTyparIsUnmanaged _ 
         | TTyparIsNotNullableValueType _ 
         | TTyparIsReferenceType _ 
         | TTyparRequiresDefaultConstructor _ -> Some(x))

and remapTraitAux tyenv (TTrait(typs,nm,mf,argtys,rty,slnCell)) =
    let slnCell = 
        match !slnCell with 
        | None -> None
        | Some sln -> 
            let sln = 
                match sln with 
                | ILMethSln(typ,extOpt,ilMethRef,minst) ->
                     ILMethSln(remapTypeAux tyenv typ,extOpt,ilMethRef,remapTypesAux tyenv minst)  
                | FSMethSln(typ, vref,minst) ->
                     FSMethSln(remapTypeAux tyenv typ, remapValRef tyenv vref,remapTypesAux tyenv minst)  
                | BuiltInSln -> 
                     BuiltInSln
            Some sln
    // Note: we reallocate a new solution cell on every traversal of a trait constraint
    // This feels incorrect for trait constraints that are quantified: it seems we should have 
    // formal binders for trait constraints when they are quantified, just as
    // we have formal binders for type variables.
    //
    // The danger here is that a solution for one syntactic occurrence of a trait constraint won't
    // be propagated to other, "linked" solutions. However trait constraints don't appear in any algebrra
    // in the same way as types
    TTrait(remapTypesAux tyenv typs,nm,mf,remapTypesAux tyenv argtys, Option.map (remapTypeAux tyenv) rty,ref slnCell)


and bindTypars tps tyargs tpinst =   
    match tps with 
    | [] -> tpinst 
    | _ -> List.map2 (fun tp tyarg -> (tp,tyarg)) tps tyargs @ tpinst 

// This version is used to remap most type parameters, e.g. ones bound at tycons, vals, records 
// See notes below on remapTypeFull for why we have a function that accepts remapAttribs as an argument 
and copyAndRemapAndBindTyparsFull remapAttrib tyenv tps =
    match tps with 
    | [] -> tps,tyenv 
    | _ -> 
      let tps' = copyTypars tps
      let tyenv = { tyenv with tpinst = bindTypars tps (generalizeTypars tps') tyenv.tpinst } 
      (tps,tps') ||> List.iter2 (fun tporig tp -> 
         tp.FixupConstraints (remapTyparConstraintsAux tyenv  tporig.Constraints);
         tp.Data.typar_attribs  <- tporig.Data.typar_attribs |> remapAttrib) ;
      tps',tyenv

// copies bound typars, extends tpinst 
and copyAndRemapAndBindTypars tyenv tps =
    copyAndRemapAndBindTyparsFull (fun _ -> []) tyenv tps

and remapValLinkage tyenv (vlink: ValLinkageFullKey) = 
    let tyOpt = vlink.TypeForLinkage
    let tyOpt' = 
        match tyOpt with 
        | None -> tyOpt 
        | Some ty -> 
            let ty' = remapTypeAux tyenv ty
            if ty === ty' then tyOpt else
            Some ty'
    if tyOpt === tyOpt' then vlink else
    ValLinkageFullKey(vlink.PartialKey, tyOpt')

and remapNonLocalValRef tyenv (nlvref:NonLocalValOrMemberRef) = 
    let eref = nlvref.EnclosingEntity
    let eref' = remapTyconRef tyenv.tyconRefRemap eref
    let vlink = nlvref.ItemKey
    let vlink' = remapValLinkage tyenv vlink
    if eref === eref' && vlink === vlink' then nlvref else
    { EnclosingEntity = eref'
      ItemKey = vlink'  }

and remapValRef tmenv (vref: ValRef) = 
    match tmenv.valRemap.TryFind vref.Deref  with 
    | None -> 
        if vref.IsLocalRef then vref else 
        let nlvref = vref.nlr
        let nlvref' = remapNonLocalValRef tmenv nlvref
        if nlvref === nlvref' then vref else
        VRefNonLocal nlvref'
    | Some res -> 
        res

let remapType  tyenv x =
    if isRemapEmpty tyenv then x else
    remapTypeAux tyenv x

let remapTypes tyenv x = 
    if isRemapEmpty tyenv then x else 
    remapTypesAux tyenv x

/// Use this one for any type that may be a forall type where the type variables may contain attributes 
/// Logically speaking this is mtuually recursive with remapAttrib defined much later in this file, 
/// because types may contain forall types that contain attributes, which need to be remapped. 
/// We currently break the recursion by passing in remapAttrib as a function parameter. 
/// Use this one for any type that may be a forall type where the type variables may contain attributes 
let remapTypeFull remapAttrib tyenv ty =
    if isRemapEmpty tyenv then ty else 
    match stripTyparEqns ty with
    | TType_forall(tps,tau) -> 
        let tps',tyenvinner = copyAndRemapAndBindTyparsFull remapAttrib tyenv tps
        TType_forall(tps',remapType tyenvinner tau)
    | _ -> 
        remapType tyenv ty

let remapParam tyenv (TSlotParam(nm,typ,fl1,fl2,fl3,attribs) as x) = 
    if isRemapEmpty tyenv then x else 
    TSlotParam(nm,remapTypeAux tyenv typ,fl1,fl2,fl3,attribs) 

let remapSlotSig remapAttrib tyenv (TSlotSig(nm,typ, ctps,methTypars,paraml, rty) as x) =
    if isRemapEmpty tyenv then x else 
    let typ' = remapTypeAux tyenv typ
    let ctps',tyenvinner = copyAndRemapAndBindTyparsFull remapAttrib tyenv ctps
    let methTypars',tyenvinner = copyAndRemapAndBindTyparsFull remapAttrib tyenvinner methTypars
    TSlotSig(nm,typ', ctps',methTypars',List.mapSquared (remapParam tyenvinner) paraml,Option.map (remapTypeAux tyenvinner) rty) 

let mkInstRemap tpinst = 
    { tyconRefRemap = emptyTyconRefRemap; 
      tpinst        = tpinst; 
      valRemap      = ValMap.Empty }

// entry points for "typar -> TType" instantiation 
let instType              tpinst x = if List.isEmpty tpinst then x else remapTypeAux  (mkInstRemap tpinst) x
let instTypes             tpinst x = if List.isEmpty tpinst then x else remapTypesAux (mkInstRemap tpinst) x
let instTrait             tpinst x = if List.isEmpty tpinst then x else remapTraitAux (mkInstRemap tpinst) x
let instTyparConstraints tpinst x = if List.isEmpty tpinst then x else remapTyparConstraintsAux (mkInstRemap tpinst) x
let instSlotSig tpinst ss = remapSlotSig (fun _ -> []) (mkInstRemap tpinst) ss
let copySlotSig ss = remapSlotSig (fun _ -> []) Remap.Empty ss

let mkTyparToTyparRenaming tpsOrig tps = 
    let tinst = generalizeTypars tps
    mkTyparInst tpsOrig tinst,tinst

let mkTyconInst (tycon:Tycon) tinst = mkTyparInst tycon.TyparsNoRange tinst
let mkTyconRefInst (tcref:TyconRef) tinst = mkTyconInst tcref.Deref tinst

//---------------------------------------------------------------------------
// Basic equalites
//---------------------------------------------------------------------------

let tyconRefEq g tcref1 tcref2 = primEntityRefEq g.compilingFslib g.fslibCcu tcref1 tcref2
let valRefEq g vref1 vref2 = primValRefEq g.compilingFslib g.fslibCcu vref1 vref2

//---------------------------------------------------------------------------
// Remove inference equations and abbreviations from units
//---------------------------------------------------------------------------

let reduceTyconRefAbbrevMeasureable (tcref:TyconRef) = 
    let abbrev = tcref.TypeAbbrev
    match abbrev with 
    | Some (TType_measure ms) -> ms
    | _ -> invalidArg "tcref" "not a measure abbreviation, or incorrect kind"

let rec stripUnitEqnsFromMeasureAux canShortcut unt = 
    match stripUnitEqnsAux canShortcut unt with 
    | MeasureCon tcref when tcref.IsTypeAbbrev  ->  
        stripUnitEqnsFromMeasureAux canShortcut (reduceTyconRefAbbrevMeasureable tcref) 
    | m -> m

let stripUnitEqnsFromMeasure m = stripUnitEqnsFromMeasureAux false m

//---------------------------------------------------------------------------
// Basic unit stuff
//---------------------------------------------------------------------------


/// What is the contribution of unit-of-measure constant ucref to unit-of-measure expression measure? 
let rec MeasureConExponent g abbrev ucref unt =
    match (if abbrev then stripUnitEqnsFromMeasure unt else stripUnitEqns unt) with
    | MeasureCon ucref' -> if tyconRefEq g ucref' ucref then 1 else 0
    | MeasureInv unt' -> -(MeasureConExponent g abbrev ucref unt')
    | MeasureProd(unt1,unt2) -> MeasureConExponent g abbrev ucref unt1 + MeasureConExponent g abbrev ucref unt2
    | _ -> 0

/// What is the contribution of unit-of-measure constant ucref to unit-of-measure expression measure
/// after remapping tycons? 
let rec MeasureConExponentAfterRemapping g r ucref unt =
    match stripUnitEqnsFromMeasure unt with
    | MeasureCon ucref' -> if tyconRefEq g (r ucref') ucref then 1 else 0
    | MeasureInv unt' -> -(MeasureConExponentAfterRemapping g r ucref unt')
    | MeasureProd(unt1,unt2) -> MeasureConExponentAfterRemapping g r ucref unt1 + MeasureConExponentAfterRemapping g r ucref unt2
    | _ -> 0

/// What is the contribution of unit-of-measure variable tp to unit-of-measure expression unt? 
let rec MeasureVarExponent tp unt =
    match stripUnitEqnsFromMeasure unt with
    | MeasureVar tp' -> if typarEq tp tp' then 1 else 0
    | MeasureInv unt' -> -(MeasureVarExponent tp unt')
    | MeasureProd(unt1,unt2) -> MeasureVarExponent tp unt1 + MeasureVarExponent tp unt2
    | _ -> 0

/// List the *literal* occurrences of unit variables in a unit expression, without repeats  
let ListMeasureVarOccs unt =
    let rec gather acc unt =  
        match stripUnitEqnsFromMeasure unt with
          MeasureVar tp -> if List.exists (typarEq tp) acc then acc else tp::acc
        | MeasureProd(unt1,unt2) -> gather (gather acc unt1) unt2
        | MeasureInv unt' -> gather acc unt'
        | _ -> acc   
    gather [] unt

/// List the *observable* occurrences of unit variables in a unit expression, without repeats, paired with their non-zero exponents
let ListMeasureVarOccsWithNonZeroExponents untexpr =
    let rec gather acc unt =  
        match stripUnitEqnsFromMeasure unt with
          MeasureVar tp -> if List.exists (fun (tp', _) -> typarEq tp tp') acc then acc 
                           else let e = MeasureVarExponent tp untexpr in if e=0 then acc else (tp,e)::acc
        | MeasureProd(unt1,unt2) -> gather (gather acc unt1) unt2
        | MeasureInv unt' -> gather acc unt'
        | _ -> acc   
    gather [] untexpr

/// List the *observable* occurrences of unit constants in a unit expression, without repeats, paired with their non-zero exponents
let ListMeasureConOccsWithNonZeroExponents g eraseAbbrevs untexpr =
    let rec gather acc unt =  
        match (if eraseAbbrevs then stripUnitEqnsFromMeasure unt else stripUnitEqns unt) with
        | MeasureCon c -> if List.exists (fun (c', _) -> tyconRefEq g c c') acc then acc 
                          else let e = MeasureConExponent g eraseAbbrevs c untexpr in if e=0 then acc else (c,e)::acc
        | MeasureProd(unt1,unt2) -> gather (gather acc unt1) unt2
        | MeasureInv unt' -> gather acc unt'
        | _ -> acc  
    gather [] untexpr

/// List the *literal* occurrences of unit constants in a unit expression, without repeats,
/// and after applying a remapping function r to tycons
let ListMeasureConOccsAfterRemapping g r unt =
    let rec gather acc unt =  
        match (stripUnitEqnsFromMeasure unt) with
        | MeasureCon c -> if List.exists (tyconRefEq g (r c)) acc then acc else r c::acc
        | MeasureProd(unt1,unt2) -> gather (gather acc unt1) unt2
        | MeasureInv unt' -> gather acc unt'
        | _ -> acc
   
    gather [] unt

/// Construct a measure expression representing the n'th power of a measure
let rec MeasurePower u n = 
    if n=0 then MeasureOne
    elif n=1 then u
    elif n<0 then MeasureInv (MeasurePower u (-n))
    else MeasureProd (u, MeasurePower u (n-1))

let MeasureProdOpt m1 m2 =
  match m1, m2 with
  | MeasureOne, _ -> m2
  | _, MeasureOne -> m1
  | _, _ -> MeasureProd (m1,m2)

/// Construct a measure expression representing the product of a list of measures
let ProdMeasures ms = match ms with [] -> MeasureOne | m::ms -> List.foldBack MeasureProdOpt ms m

let isDimensionless g tyarg =
    match stripTyparEqns tyarg with
    | TType_measure unt ->
      List.isEmpty (ListMeasureVarOccsWithNonZeroExponents unt) && 
      List.isEmpty (ListMeasureConOccsWithNonZeroExponents g true unt)
    | _ -> false


let destUnitParMeasure g unt =
    let vs = ListMeasureVarOccsWithNonZeroExponents unt
    let cs = ListMeasureConOccsWithNonZeroExponents g true unt
    match vs, cs with
    | [(v,1)], [] -> v
    | _, _ -> failwith "destUnitParMeasure: not a unit-of-measure parameter"

let isUnitParMeasure g unt =
    let vs = ListMeasureVarOccsWithNonZeroExponents unt
    let cs = ListMeasureConOccsWithNonZeroExponents g true unt
 
    match vs, cs with
    | [(_,1)], [] -> true
    | _,   _ -> false

let normalizeMeasure g ms =
    let vs = ListMeasureVarOccsWithNonZeroExponents ms
    let cs = ListMeasureConOccsWithNonZeroExponents g false ms
    match vs, cs with
    | [],[] -> MeasureOne
    | [(v,1)], [] -> MeasureVar v
    | vs, cs -> List.foldBack (fun (v,e) -> fun m -> MeasureProd (MeasurePower (MeasureVar v) e, m)) vs (List.foldBack (fun (c,e) -> fun m -> MeasureProd (MeasurePower (MeasureCon c) e, m)) cs MeasureOne)
 
let tryNormalizeMeasureInType g ty =
    match ty with
    | TType_measure (MeasureVar v) ->
      match v.Solution with
      | Some (TType_measure ms) ->
        (v.Data.typar_solution <- Some (TType_measure (normalizeMeasure g ms)); ty)
      | _ -> ty
      
    | _ -> ty

let rec sizeMeasure g ms =
  match stripUnitEqns ms with
  | MeasureVar _ -> 1
  | MeasureCon _ -> 1
  | MeasureProd (ms1,ms2) -> sizeMeasure g ms1 + sizeMeasure g ms2
  | MeasureInv ms -> sizeMeasure g ms
  | MeasureOne -> 1

//---------------------------------------------------------------------------
// SOme basic type builders
//---------------------------------------------------------------------------

let mkNativePtrType g ty = TType_app (g.nativeptr_tcr, [ty])
let mkByrefTy g ty = TType_app (g.byref_tcr, [ty])

let mkArrayTy g n ty = 
    if n = 1 then TType_app (g.il_arr1_tcr, [ty]) 
    elif n = 2 then TType_app (g.il_arr2_tcr, [ty]) 
    elif n = 3 then TType_app (g.il_arr3_tcr, [ty]) 
    elif n = 4 then TType_app (g.il_arr4_tcr, [ty]) 
    else failwith "F# supports a maxiumum .NET array dimension of 4"

//--------------------------------------------------------------------------
// Tuple compilation (types)
//------------------------------------------------------------------------ 

let maxTuple = 8
let goodTupleFields = maxTuple-1

let is_tuple_tcref g tcref =
    match tcref with
    | x when 
        (tyconRefEq g g.tuple1_tcr x || 
         tyconRefEq g g.tuple2_tcr x || 
         tyconRefEq g g.tuple3_tcr x || 
         tyconRefEq g g.tuple4_tcr x || 
         tyconRefEq g g.tuple5_tcr x || 
         tyconRefEq g g.tuple6_tcr x || 
         tyconRefEq g g.tuple7_tcr x || 
         tyconRefEq g g.tuple8_tcr x) -> true
    | _ -> false

let mkCompiledTupleTyconRef g tys = 
    let n = List.length tys 
    if   n = 1 then g.tuple1_tcr
    elif n = 2 then g.tuple2_tcr
    elif n = 3 then g.tuple3_tcr
    elif n = 4 then g.tuple4_tcr
    elif n = 5 then g.tuple5_tcr
    elif n = 6 then g.tuple6_tcr
    elif n = 7 then g.tuple7_tcr
    elif n = 8 then g.tuple8_tcr
    else failwithf "mkCompiledTupleTyconRef, n = %d" n

let rec mkCompiledTupleTy g tys = 
    let n = List.length tys 
    if n < maxTuple then TType_app (mkCompiledTupleTyconRef g tys, tys)
    else 
        let tysA,tysB = List.splitAfter goodTupleFields tys
        TType_app (g.tuple8_tcr, tysA@[mkCompiledTupleTy g tysB])

//---------------------------------------------------------------------------
// Remove inference equations and abbreviations from types 
//---------------------------------------------------------------------------

let applyTyconAbbrev abbrevTy tycon tyargs = 
    if List.isEmpty tyargs then abbrevTy 
    else instType (mkTyconInst tycon tyargs) abbrevTy

let reduceTyconAbbrev (tycon:Tycon) tyargs = 
    let abbrev = tycon.TypeAbbrev
    match abbrev with 
    | None -> invalidArg "tycon" "this type definition is not an abbreviation";
    | Some abbrevTy -> 
        applyTyconAbbrev abbrevTy tycon tyargs

let reduceTyconRefAbbrev (tcref:TyconRef) tyargs = 
    reduceTyconAbbrev tcref.Deref tyargs

let reduceTyconMeasureable (tycon:Tycon) tyargs = 
    let repr = tycon.TypeReprInfo
    match repr with 
    | Some (TMeasureableRepr ty) -> 
        if List.isEmpty tyargs then ty else instType (mkTyconInst tycon tyargs) ty
    | _ -> invalidArg "tc" "this type definition is not a refinement"

let reduceTyconRefMeasureable (tcref:TyconRef) tyargs = 
    reduceTyconMeasureable tcref.Deref tyargs

let rec stripTyEqnsA g canShortcut ty = 
    let ty = stripTyparEqnsAux canShortcut ty 
    match ty with 
    | TType_app (tcref,tinst) -> 
        let tycon = tcref.Deref
        match tycon.TypeAbbrev with 
        | Some abbrevTy -> 
            stripTyEqnsA g canShortcut (applyTyconAbbrev abbrevTy tycon tinst)
        | None -> 
            if tycon.IsMeasureableReprTycon && List.forall (isDimensionless g) tinst then
                stripTyEqnsA g canShortcut (reduceTyconMeasureable tycon tinst)
            else 
                ty
    | ty -> ty

let stripTyEqns g ty = stripTyEqnsA g false ty

/// This erases outermost occurences of inference equations, type abbreviations and measureable types (float<_>).
/// It also optionally erases all "compilation representations", i.e. function and
/// tuple types, and also "nativeptr<'T> --> System.IntPtr"
let rec stripTyEqnsAndErase eraseFuncAndTuple g ty =
    let ty = stripTyEqns g ty
    match ty with
    | TType_app (tcref,args) -> 
        let tycon = tcref.Deref
        if tycon.IsMeasureableReprTycon  then
            stripTyEqnsAndErase eraseFuncAndTuple g (reduceTyconMeasureable tycon args)
        elif tyconRefEq g tcref g.nativeptr_tcr && eraseFuncAndTuple then 
            stripTyEqnsAndErase eraseFuncAndTuple g g.nativeint_ty
        else
            ty
    | TType_fun(a,b) when eraseFuncAndTuple -> TType_app(g.fastFunc_tcr,[ a; b]) 
    | TType_tuple(l) when eraseFuncAndTuple -> mkCompiledTupleTy g l
    | ty -> ty

let stripTyEqnsAndMeasureEqns g ty =
   stripTyEqnsAndErase false g ty
       
type Erasure = EraseAll | EraseMeasures | EraseNone

let stripTyEqnsWrtErasure erasureFlag g ty = 
    match erasureFlag with 
    | EraseAll -> stripTyEqnsAndErase true g ty
    | EraseMeasures -> stripTyEqnsAndErase false g ty
    | _ -> stripTyEqns g ty
    
let rec stripExnEqns (eref:TyconRef) = 
    let exnc = eref.Deref
    match exnc.ExceptionInfo with
    | TExnAbbrevRepr eref -> stripExnEqns eref
    | _ -> exnc


let primDestForallTy g ty = ty |> stripTyEqns g |> (function TType_forall (tyvs,tau) -> (tyvs,tau) | _ -> failwith "primDestForallTy: not a forall type")
let destFunTy      g ty = ty |> stripTyEqns g |> (function TType_fun (tyv,tau) -> (tyv,tau) | _ -> failwith "destFunTy: not a function type")
let destTupleTy    g ty = ty |> stripTyEqns g |> (function TType_tuple l -> l | _ -> failwith "destTupleTy: not a tuple type")
let destTyparTy    g ty = ty |> stripTyEqns g |> (function TType_var v -> v | _ -> failwith "destTyparTy: not a typar type")
let destAnyParTy   g ty = ty |> stripTyEqns g |> (function TType_var v -> v | TType_measure unt -> destUnitParMeasure g unt | _ -> failwith "destAnyParTy: not a typar or unpar type")
let destMeasureTy  g ty = ty |> stripTyEqns g |> (function TType_measure m -> m | _ -> failwith "destMeasureTy: not a unit-of-measure type")
let isFunTy        g ty = ty |> stripTyEqns g |> (function TType_fun _ -> true | _ -> false)
let isForallTy     g ty = ty |> stripTyEqns g |> (function TType_forall _ -> true | _ -> false)
let isTupleTy      g ty = ty |> stripTyEqns g |> (function TType_tuple _ -> true | _ -> false)
let isUnionTy      g ty = ty |> stripTyEqns g |> (function TType_app(tcr,_) -> tcr.IsUnionTycon | _ -> false)
let isReprHiddenTy   g ty = ty |> stripTyEqns g |> (function TType_app(tcr,_) -> tcr.IsHiddenReprTycon | _ -> false)
let isFSharpObjModelTy g ty = ty |> stripTyEqns g |> (function TType_app(tcr,_) -> tcr.IsFSharpObjectModelTycon | _ -> false)
let isRecdTy       g ty = ty |> stripTyEqns g |> (function TType_app(tcr,_) -> tcr.IsRecordTycon | _ -> false)
let isTyparTy      g ty = ty |> stripTyEqns g |> (function TType_var _ -> true | _ -> false)
let isAnyParTy     g ty = ty |> stripTyEqns g |> (function TType_var _ -> true | TType_measure unt -> isUnitParMeasure g unt | _ -> false)
let isMeasureTy    g ty = ty |> stripTyEqns g |> (function TType_measure _ -> true | _ -> false)

// WARNING: If you increase this you must make the corresponding types in FSharp.Core.dll structs
#if TUPLE_STRUXT
let highestTupleStructType = 2
let isTupleStructTy g ty = ty |> stripTyEqns g |> (function TType_tuple l -> l.Length <= highestTupleStructType | _ -> false)
#else
let isTupleStructTy (_g:TcGlobals) (_ty:TType) = false
#endif


let isProvenUnionCaseTy ty = match ty with TType_ucase _ -> true | _ -> false

let mkAppTy tcref tyargs = TType_app(tcref,tyargs)
let mkProvenUnionCaseTy ucref tyargs = TType_ucase(ucref,tyargs)
let isAppTy   g ty = ty |> stripTyEqns g |> (function TType_app _ -> true | _ -> false) 
let destAppTy g ty = ty |> stripTyEqns g |> (function TType_app(tcref,tinst) -> tcref,tinst | _ -> failwith "destAppTy") 
let tcrefOfAppTy   g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tcref | _ -> failwith "tcrefOfAppTy") 
let tryDestAppTy   g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> Some tcref | _ -> None) 
let argsOfAppTy   g ty = ty |> stripTyEqns g |> (function TType_app(_,tinst) -> tinst | _ -> []) 
let tyconOfAppTy   g ty = (tcrefOfAppTy g ty).Deref

let mkInstForAppTy g typ = 
    if isAppTy g typ then 
      let tcref,tinst = destAppTy g typ
      mkTyconRefInst tcref tinst
    else []

let domainOfFunTy g ty = fst(destFunTy g ty)
let rangeOfFunTy  g ty = snd(destFunTy g ty)

//---------------------------------------------------------------------------
// Equivalence of types up to alpha-equivalence 
//---------------------------------------------------------------------------


[<NoEquality; NoComparison>]
type TypeEquivEnv = 
    { EquivTypars: TyparMap<TType>;
      EquivTycons: TyconRefRemap}

// allocate a singleton
let typeEquivEnvEmpty = 
    { EquivTypars = TyparMap.Empty; 
      EquivTycons = emptyTyconRefRemap }

type TypeEquivEnv with 
    static member Empty = typeEquivEnvEmpty

    member aenv.BindTyparsToTypes tps1 tys2 =
        {aenv with EquivTypars= (tps1,tys2,aenv.EquivTypars) |||> List.foldBack2 (fun tp ty tpmap -> tpmap.Add(tp,ty)) }

    member aenv.BindEquivTypars tps1 tps2 =
        aenv.BindTyparsToTypes tps1 (List.map mkTyparTy tps2) 

    static member FromTyparInst tpinst =
        let tps,tys = List.unzip tpinst
        TypeEquivEnv.Empty.BindTyparsToTypes tps tys 

    static member FromEquivTypars tps1 tps2 = 
        TypeEquivEnv.Empty.BindEquivTypars tps1 tps2 

let rec traitsAEquivAux erasureFlag g aenv (TTrait(typs1,nm,mf1,argtys,rty,_)) (TTrait(typs2,nm2,mf2,argtys2,rty2,_)) =
   ListSet.equals (typeAEquivAux erasureFlag g aenv) typs1 typs2 &&
   mf1 = mf2 && 
   returnTypesAEquivAux erasureFlag g aenv rty rty2 && 
   List.lengthsEqAndForall2 (typeAEquivAux erasureFlag g aenv) argtys argtys2 &&
   nm = nm2

and returnTypesAEquivAux erasureFlag g aenv rty rty2 =
    match rty,rty2 with  
    | None,None -> true
    | Some t1,Some t2 -> typeAEquivAux erasureFlag g aenv t1 t2
    | _ -> false

    
and typarConstraintsAEquivAux erasureFlag g aenv tpc1 tpc2 =
    match tpc1,tpc2 with
    | TTyparCoercesToType(acty,_),
      TTyparCoercesToType(fcty,_) -> 
        typeAEquivAux erasureFlag g aenv acty fcty

    | TTyparMayResolveMemberConstraint(trait1,_),
      TTyparMayResolveMemberConstraint(trait2,_) -> 
        traitsAEquivAux erasureFlag g aenv trait1 trait2 

    | TTyparDefaultsToType(_,acty,_),
      TTyparDefaultsToType(_,fcty,_) -> 
        typeAEquivAux erasureFlag g aenv acty fcty

    | TTyparIsEnum(uty1,_),TTyparIsEnum(uty2,_) -> 
        typeAEquivAux erasureFlag g aenv uty1 uty2

    | TTyparIsDelegate(aty1,bty1,_),TTyparIsDelegate(aty2,bty2,_) -> 
        typeAEquivAux erasureFlag g aenv aty1 aty2 && 
        typeAEquivAux erasureFlag g aenv bty1 bty2 

    | TTyparSimpleChoice (tys1,_),TTyparSimpleChoice(tys2,_) -> 
        ListSet.equals (typeAEquivAux erasureFlag g aenv) tys1 tys2

    | TTyparSupportsComparison _        ,TTyparSupportsComparison _ 
    | TTyparSupportsEquality _          ,TTyparSupportsEquality _ 
    | TTyparSupportsNull _              ,TTyparSupportsNull _ 
    | TTyparIsNotNullableValueType _    ,TTyparIsNotNullableValueType _
    | TTyparIsReferenceType _           ,TTyparIsReferenceType _ 
    | TTyparIsUnmanaged _               ,TTyparIsUnmanaged _
    | TTyparRequiresDefaultConstructor _, TTyparRequiresDefaultConstructor _ -> true
    | _ -> false

and typarConstraintSetsAEquivAux erasureFlag g aenv (tp1:Typar) (tp2:Typar) = 
    tp1.StaticReq = tp2.StaticReq &&
    ListSet.equals (typarConstraintsAEquivAux erasureFlag g aenv) tp1.Constraints tp2.Constraints

and typarsAEquivAux erasureFlag g (aenv: TypeEquivEnv) tps1 tps2 = 
    List.length tps1 = List.length tps2 &&
    let aenv = aenv.BindEquivTypars tps1 tps2 
    List.forall2 (typarConstraintSetsAEquivAux erasureFlag g aenv) tps1 tps2

and tcrefAEquiv g aenv tc1 tc2 = 
    tyconRefEq g tc1 tc2 || 
    (aenv.EquivTycons.ContainsKey tc1  && tyconRefEq g aenv.EquivTycons.[tc1] tc2)

and typeAEquivAux erasureFlag g aenv ty1 ty2 = 
    let ty1 = stripTyEqnsWrtErasure erasureFlag g ty1 
    let ty2 = stripTyEqnsWrtErasure erasureFlag g ty2
    match ty1, ty2 with
    | TType_forall(tps1,rty1), TType_forall(tps2,rty2) -> 
        typarsAEquivAux erasureFlag g aenv tps1 tps2 && typeAEquivAux erasureFlag g (aenv.BindEquivTypars tps1 tps2) rty1 rty2
    | TType_var tp1, TType_var tp2 when typarEq tp1 tp2 -> 
        true
    | TType_var tp1, _ when aenv.EquivTypars.ContainsKey tp1 -> 
        typeEquivAux erasureFlag g aenv.EquivTypars.[tp1] ty2
    | TType_app (tc1,b1)  ,TType_app (tc2,b2) -> 
        tcrefAEquiv g aenv tc1 tc2 &&
        typesAEquivAux erasureFlag g aenv b1 b2
    | TType_ucase (UCRef(tc1,n1),b1)  ,TType_ucase (UCRef(tc2,n2),b2) -> 
        n1=n2 &&
        tcrefAEquiv g aenv tc1 tc2 &&
        typesAEquivAux erasureFlag g aenv b1 b2
    | TType_tuple l1,TType_tuple l2 -> 
        typesAEquivAux erasureFlag g aenv l1 l2
    | TType_fun (dtys1,rty1),TType_fun (dtys2,rty2) -> 
        typeAEquivAux erasureFlag g aenv dtys1 dtys2 && typeAEquivAux erasureFlag g aenv rty1 rty2
    | TType_measure m1, TType_measure m2 -> 
        match erasureFlag with 
        | EraseNone -> measureAEquiv g aenv m1 m2 
        | _ -> true 
    | _ -> false

and measureAEquiv g aenv un1 un2 =
    let vars1 = ListMeasureVarOccs un1
    let trans tp1 = if aenv.EquivTypars.ContainsKey tp1 then destAnyParTy g aenv.EquivTypars.[tp1] else tp1
    let remapTyconRef tc = if aenv.EquivTycons.ContainsKey tc then aenv.EquivTycons.[tc] else tc
    let vars1' = List.map trans vars1
    let vars2 = ListSet.subtract typarEq (ListMeasureVarOccs un2) vars1'
    let cons1 = ListMeasureConOccsAfterRemapping g remapTyconRef un1
    let cons2 = ListMeasureConOccsAfterRemapping g remapTyconRef un2 
 
    List.forall (fun v -> MeasureVarExponent v un1 = MeasureVarExponent (trans v) un2) vars1 &&
    List.forall (fun v -> MeasureVarExponent v un1 = MeasureVarExponent v un2) vars2 &&
    List.forall (fun c -> MeasureConExponentAfterRemapping g remapTyconRef c un1 = MeasureConExponentAfterRemapping g remapTyconRef c un2) (cons1@cons2)  


and typesAEquivAux erasureFlag g aenv l1 l2 = List.lengthsEqAndForall2 (typeAEquivAux erasureFlag g aenv) l1 l2
and typeEquivAux erasureFlag g ty1 ty2 =  typeAEquivAux erasureFlag g TypeEquivEnv.Empty ty1 ty2

let typeAEquiv g aenv ty1 ty2 = typeAEquivAux EraseNone g aenv ty1 ty2
let typeEquiv g ty1 ty2 = typeEquivAux EraseNone g ty1 ty2
let traitsAEquiv g aenv t1 t2 = traitsAEquivAux EraseNone g aenv t1 t2
let typarConstraintsAEquiv g aenv c1 c2 = typarConstraintsAEquivAux EraseNone g aenv c1 c2
let typarsAEquiv g aenv d1 d2 = typarsAEquivAux EraseNone g aenv d1 d2
let returnTypesAEquiv g aenv t1 t2 = returnTypesAEquivAux EraseNone g aenv t1 t2

let measureEquiv g m1 m2 = measureAEquiv g TypeEquivEnv.Empty m1 m2

//---------------------------------------------------------------------------
// Standard orderings, e.g. for order set/map keys
//---------------------------------------------------------------------------

let valOrder = { new IComparer<Val> with member __.Compare(v1,v2) = compare v1.Stamp v2.Stamp }
let tyconOrder = { new IComparer<Tycon> with member __.Compare(tc1,tc2) = compare tc1.Stamp tc2.Stamp }
let recdFieldRefOrder  = 
    { new IComparer<RecdFieldRef> with 
         member __.Compare(RFRef(tcref1,nm1), RFRef(tcref2,nm2)) = 
            let c = tyconOrder.Compare (tcref1.Deref, tcref2.Deref) 
            if c <> 0 then c else 
            compare nm1 nm2 }

let unionCaseRefOrder = 
    { new IComparer<UnionCaseRef> with 
         member __.Compare(UCRef(tcref1,nm1), UCRef(tcref2,nm2)) = 
            let c = tyconOrder.Compare (tcref1.Deref, tcref2.Deref) 
            if c <> 0 then c else 
            compare nm1 nm2 }

//---------------------------------------------------------------------------
// Make some common types
//---------------------------------------------------------------------------

let mkFunTy d r = TType_fun (d,r)
let (-->) d r = mkFunTy d r
let mkForallTy d r = TType_forall (d,r)
let tryMkForallTy d r = if isNil d then r else mkForallTy d r
let (+->) d r = tryMkForallTy d r
let mkTupleTy l = TType_tuple l
let mkIteratedFunTy dl r = List.foldBack (-->) dl r

let mkLambdaArgTy m tys = 
    match tys with 
    | [] -> error(InternalError("mkLambdaArgTy",m))
    | [h] -> h 
    | _ -> mkTupleTy tys

let typeOfLambdaArg m vs = mkLambdaArgTy m (typesOfVals vs)
let mkMultiLambdaTy m vs rty = mkFunTy (typeOfLambdaArg m vs) rty 
let mkLambdaTy tps tys rty = tryMkForallTy tps (mkIteratedFunTy tys rty)

/// When compiling FSharp.Core.dll we have to deal with the non-local references into
/// the library arising from env.fs. Part of this means that we have to be able to resolve these
/// references. This function artificially forces the existence of a module or namespace at a 
/// particular point in order to do this.
let ensureCcuHasModuleOrNamespaceAtPath (ccu:CcuThunk) path (CompPath(_,cpath)) xml =
    let scoref = ccu.ILScopeRef 
    let rec loop prior_cpath (path:Ident list) cpath (modul:ModuleOrNamespace) =
        let mtype = modul.ModuleOrNamespaceType 
        match path,cpath with 
        | (hpath::tpath),((_,mkind)::tcpath)  -> 
            let modName = hpath.idText 
            if not (Map.containsKey modName mtype.AllEntitiesByCompiledAndLogicalMangledNames) then 
                let smodul = NewModuleOrNamespace (Some(CompPath(scoref,prior_cpath))) taccessPublic hpath xml [] (notlazy (NewEmptyModuleOrNamespaceType mkind))
                mtype.AddModuleOrNamespaceByMutation(smodul);
            let modul = Map.find modName mtype.AllEntitiesByCompiledAndLogicalMangledNames 
            loop (prior_cpath@[(modName,Namespace)]) tpath tcpath modul 

        | _ -> () 

    loop [] path cpath ccu.Contents


//---------------------------------------------------------------------------
// Primitive destructors
//---------------------------------------------------------------------------

/// Look through the Expr.Link nodes arising from type inference
let rec stripExpr e = 
    match e with 
    | Expr.Link eref -> stripExpr !eref
    | _ -> e    

let mkCase (a,b) = TCase(a,b)

let isTupleExpr e = match e with Expr.Op (TOp.Tuple,_,_,_) -> true | _ -> false
let tryDestTuple e = match e with Expr.Op (TOp.Tuple,_,es,_) -> es | _ -> [e]

//---------------------------------------------------------------------------
// Range info for expressions
//---------------------------------------------------------------------------

let rec rangeOfExpr x = 
    match x with
    | Expr.Val (_,_,m) | Expr.Op (_,_,_,m)   | Expr.Const (_,m,_) | Expr.Quote (_,_,m,_)
    | Expr.Obj (_,_,_,_,_,_,m) | Expr.App(_,_,_,_,m) | Expr.Seq (_,_,_,_,m) 
    | Expr.StaticOptimization (_,_,_,m) | Expr.Lambda (_,_,_,_,_,m,_) 
    | Expr.TyLambda (_,_,_,m,_)| Expr.TyChoose (_,_,m) | Expr.LetRec (_,_,m,_) | Expr.Let (_,_,m,_) | Expr.Match (_,_,_,_,m,_) -> m
    | Expr.Link(eref) -> rangeOfExpr (!eref)

type Expr with 
    member x.Range = rangeOfExpr x

//---------------------------------------------------------------------------
// Build nodes in decision graphs
//---------------------------------------------------------------------------


let primMkMatch(spBind,exprm,tree,targets,matchm,ty) = Expr.Match (spBind,exprm,tree,targets,matchm,ty)

type MatchBuilder(spBind,inpRange: Range.range) = 

    let targets = new ResizeArray<_>(10) 
    member x.AddTarget(tg) = 
        let n = targets.Count 
        targets.Add(tg);
        n

    member x.AddResultTarget(e,spTarget) = TDSuccess(FlatList.empty, x.AddTarget(TTarget(FlatList.empty,e,spTarget)))

    member x.CloseTargets() = targets |> ResizeArray.toList

    member x.Close(dtree,m,ty) = primMkMatch  (spBind,inpRange,dtree,targets.ToArray(),m,ty)

let mkBoolSwitch m g t e = TDSwitch(g,[TCase(Test.Const(Const.Bool(true)),t)],Some e,m)

let primMkCond spBind spTarget1 spTarget2 m ty e1 e2 e3 = 
    let mbuilder = new MatchBuilder(spBind,m)
    let dtree = mkBoolSwitch m e1 (mbuilder.AddResultTarget(e2,spTarget1)) (mbuilder.AddResultTarget(e3,spTarget2)) 
    mbuilder.Close(dtree,m,ty)

let mkCond spBind spTarget m ty e1 e2 e3 =  primMkCond spBind spTarget spTarget m ty e1 e2 e3


//---------------------------------------------------------------------------
// Primitive constructors
//---------------------------------------------------------------------------

let exprForValRef m vref =  Expr.Val(vref,NormalValUse,m)
let exprForVal m v =  exprForValRef m (mkLocalValRef v)
let gen_mk_local m s ty mut compgen =
    let thisv = NewVal(s,m,None,ty,mut,compgen,None,taccessPublic,ValNotInRecScope,None,NormalVal,[],OptionalInline,XmlDoc.Empty,false,false,false,false,false,None,ParentNone) 
    thisv,exprForVal m thisv

let mkLocal         m s ty = gen_mk_local m s ty Immutable false
let mkCompGenLocal m s ty = gen_mk_local m s ty Immutable true
let mkMutableCompGenLocal m s ty = gen_mk_local m s ty Mutable true


// Type gives return type.  For type-lambdas this is the formal return type. 
let mkMultiLambda m vs (b,rty) = Expr.Lambda (newUnique(), None,None,vs,b,m, rty)
let rebuildLambda m ctorThisValOpt baseValOpt vs (b,rty) = Expr.Lambda (newUnique(), ctorThisValOpt, baseValOpt,vs,b,m, rty)
let mkLambda m v (b,rty) = mkMultiLambda m [v] (b,rty)
let mkTypeLambda m vs (b,tau_ty) = match vs with [] -> b | _ -> Expr.TyLambda (newUnique(), vs,b,m,tau_ty)
let mkTypeChoose m vs b = match vs with [] -> b | _ -> Expr.TyChoose (vs,b,m)

let mkObjExpr (ty,basev,basecall,overrides,iimpls,m) = 
    Expr.Obj (newUnique(),ty,basev,basecall,overrides,iimpls,m) 

let mkLambdas m tps (vs:Val list) (b,rty) = 
    mkTypeLambda m tps (List.foldBack (fun v (e,ty) -> mkLambda m v (e,ty), v.Type --> ty) vs (b,rty))

let mkMultiLambdasCore m vsl (b,rty) = 
    List.foldBack (fun v (e,ty) -> mkMultiLambda m v (e,ty), typeOfLambdaArg m v --> ty) vsl (b,rty)

let mkMultiLambdas m tps vsl (b,rty) = 
    mkTypeLambda m tps (mkMultiLambdasCore m vsl (b,rty) )

let mkMemberLambdas m tps ctorThisValOpt baseValOpt vsl (b,rty) = 
    let expr = 
        match ctorThisValOpt,baseValOpt with
        | None,None -> mkMultiLambdasCore m vsl (b,rty)
        | _ -> 
            match vsl with 
            | [] -> error(InternalError("mk_basev_multi_lambdas_core: can't attach a basev to a non-lambda expression",m))
            | h::t -> 
                let b,rty = mkMultiLambdasCore m t (b,rty)
                (rebuildLambda m ctorThisValOpt baseValOpt h (b,rty), (typeOfLambdaArg m h --> rty))
    mkTypeLambda m tps expr

let mkMultiLambdaBind v letSeqPtOpt m  tps vsl (b,rty) = 
    TBind(v,mkMultiLambdas m tps vsl (b,rty),letSeqPtOpt)

let mkBind seqPtOpt v e = TBind(v,e,seqPtOpt)

let mkCompGenBind v e = TBind(v,e,NoSequencePointAtStickyBinding)

/// Make bindings that are compiler generated (though the variables may not be - e.g. they may be lambda arguments in a beta reduction)
let mkCompGenBinds vs es = 
    if List.length vs <> List.length es then failwith "mkCompGenBinds: invalid argument";
    List.map2 mkCompGenBind vs es |> FlatList.ofList

// n.b. type gives type of body 
let mkLetBind m bind body = Expr.Let(bind,body, m, NewFreeVarsCache())
let mkLetsBind m binds body = List.foldBack (mkLetBind m) binds body 
let mkLetsFromBindings m binds body = FlatList.foldBack (mkLetBind m) binds body 
let mkLet seqPtOpt m v x body = mkLetBind m (mkBind seqPtOpt v x) body
let mkCompGenLet m v x body = mkLetBind m (mkCompGenBind v x) body

let mkInvisibleBind v e = TBind(v,e,NoSequencePointAtInvisibleBinding)
let mkInvisibleLet m v x body = mkLetBind m (mkInvisibleBind v x) body
let mkInvisibleBinds (vs: Val list) (es: Expr list) = 
    if vs.Length <> es.Length then failwith "mkInvisibleBinds: invalid argument";
    List.map2 mkInvisibleBind vs es

let mkInvisibleFlatBindings vs es = 
    if FlatList.length vs <> FlatList.length es then failwith "mkInvisibleFlatBindings: invalid argument";
    FlatList.map2 mkInvisibleBind vs es

let mkInvisibleLets m vs xs body = mkLetsBind m (mkInvisibleBinds vs xs) body
let mkInvisibleLetsFromBindings m vs xs body = mkLetsFromBindings m (mkInvisibleFlatBindings vs xs) body

let mkLetRecBinds m binds body = if FlatList.isEmpty binds then body else Expr.LetRec(binds,body, m, NewFreeVarsCache())

//-------------------------------------------------------------------------
// Type schemes...
//-------------------------------------------------------------------------

// Type parameters may be have been equated to other tps in equi-recursive type inference 
// and unit type inference. Normalize them here 
let NormalizeDeclaredTyparsForEquiRecursiveInference g tps = 
    match tps with 
    | [] -> []
    | tps -> 
        tps |> List.map (fun tp -> 
          let ty =  mkTyparTy tp
          if isAnyParTy g ty then destAnyParTy g ty else tp)
 
type TypeScheme = TypeScheme of Typars  * TType    
  
let mkGenericBindRhs g m generalizedTyparsForRecursiveBlock typeScheme bodyExpr = 
    let (TypeScheme(generalizedTypars,tauType)) = typeScheme

    // Normalize the generalized typars
    let generalizedTypars = NormalizeDeclaredTyparsForEquiRecursiveInference g generalizedTypars

    // Some recursive bindings result in free type variables, e.g. 
    //    let rec f (x:'a) = ()  
    //    and g() = f y |> ignore 
    // What is the type of y? Type inference equates it to 'a. 
    // But "g" is not polymorphic in 'a. Hence we get a free choice of "'a" 
    // in the scope of "g". Thus at each individual recursive binding we record all 
    // type variables for which we have a free choice, which is precisely the difference 
    // between the union of all sets of generalized type variables and the set generalized 
    // at each particular binding. 
    //
    // We record an expression node that indicates that a free choice can be made 
    // for these. This expression node effectively binds the type variables. 
    let freeChoiceTypars = ListSet.subtract typarEq generalizedTyparsForRecursiveBlock generalizedTypars
    mkTypeLambda m generalizedTypars (mkTypeChoose m freeChoiceTypars bodyExpr, tauType)

let isBeingGeneralized tp typeScheme = 
    let (TypeScheme(generalizedTypars,_)) = typeScheme
    ListSet.contains typarRefEq tp generalizedTypars

//-------------------------------------------------------------------------
// Build conditional expressions...
//------------------------------------------------------------------------- 

let mkLazyAnd g m e1 e2 = mkCond NoSequencePointAtStickyBinding SuppressSequencePointAtTarget m g.bool_ty e1 e2 (Expr.Const(Const.Bool false,m,g.bool_ty))
let mkLazyOr g m e1 e2 = mkCond NoSequencePointAtStickyBinding SuppressSequencePointAtTarget m g.bool_ty e1 (Expr.Const(Const.Bool true,m,g.bool_ty)) e2

let mkCoerceExpr(e,to_ty,m,from_ty)                     = Expr.Op (TOp.Coerce,[to_ty;from_ty],[e],m)

let mkAsmExpr(code,tinst,args,rettys,m)                 = Expr.Op (TOp.ILAsm(code,rettys),tinst,args,m)
let mkUnionCaseExpr(uc,tinst,args,m)                        = Expr.Op (TOp.UnionCase uc,tinst,args,m)
let mkExnExpr(uc,args,m)                          = Expr.Op (TOp.ExnConstr uc,[],args,m)
let mkTupleFieldGet(e,tinst,i,m)                  = Expr.Op (TOp.TupleFieldGet(i), tinst, [e],m)

let mkRecdFieldGetViaExprAddr(e,fref,tinst,m)      = Expr.Op (TOp.ValFieldGet(fref), tinst, [e],m)
let mkRecdFieldGetAddrViaExprAddr(e,fref,tinst,m) = Expr.Op (TOp.ValFieldGetAddr(fref), tinst, [e],m)

let mkStaticRecdFieldGetAddr(fref,tinst,m)          = Expr.Op (TOp.ValFieldGetAddr(fref), tinst, [],m)
let mkStaticRecdFieldGet(fref,tinst,m)               = Expr.Op (TOp.ValFieldGet(fref), tinst, [],m)
let mkStaticRecdFieldSet(fref,tinst,e,m)             = Expr.Op (TOp.ValFieldSet(fref), tinst, [e],m)

let mkRecdFieldSetViaExprAddr(e1,fref,tinst,e2,m)  = Expr.Op (TOp.ValFieldSet(fref), tinst, [e1;e2],m)

let mkUnionCaseTagGet(e1,cref,tinst,m)                = Expr.Op (TOp.UnionCaseTagGet(cref), tinst, [e1],m)
let mkUnionCaseProof(e1,cref,tinst,m)                  = Expr.Op (TOp.UnionCaseProof(cref), tinst, [e1],m)

/// Build a 'get' expression for something we've already determined to be a particular union case, and where the
/// input expression has 'TType_ucase', which is an F# compiler internal "type"
let mkUnionCaseFieldGetProven(e1,cref,tinst,j,m)   = Expr.Op (TOp.UnionCaseFieldGet(cref,j), tinst, [e1],m)

/// Build a 'get' expression for something we've already determined to be a particular union case, but where 
/// the static type of the input is not yet proven to be that particular union case. This requires a type
/// cast to 'prove' the condition.
let mkUnionCaseFieldGetUnproven(e1,cref,tinst,j,m)  = mkUnionCaseFieldGetProven(mkUnionCaseProof(e1,cref,tinst,m),cref,tinst,j,m)

let mkUnionCaseFieldSet(e1,cref,tinst,j,e2,m)         = Expr.Op (TOp.UnionCaseFieldSet(cref,j), tinst, [e1;e2],m)

let mkExnCaseFieldGet(e1,ecref,j,m)             = Expr.Op (TOp.ExnFieldGet(ecref,j), [],[e1],m)
let mkExnCaseFieldSet(e1,ecref,j,e2,m)          = Expr.Op (TOp.ExnFieldSet(ecref,j), [],[e1;e2],m)

let mkDummyLambda g (e:Expr,ety) = 
    let m = e.Range
    mkLambda m (fst (mkCompGenLocal m "unitVar" g.unit_ty)) (e,ety)
                           
let mkWhile       g (spWhile,marker,e1,e2,m)             = 
    Expr.Op (TOp.While (spWhile,marker),[]  ,[mkDummyLambda g (e1,g.bool_ty);mkDummyLambda g (e2,g.unit_ty)],m)

let mkFor         g (spFor,v,e1,dir,e2,e3:Expr,m)    = 
    Expr.Op (TOp.For (spFor,dir)    ,[]  ,[mkDummyLambda g (e1,g.int_ty) ;mkDummyLambda g (e2,g.int_ty);mkLambda e3.Range v (e3,g.unit_ty)],m)

let mkTryWith   g (e1,vf,ef:Expr,vh,eh:Expr,m,ty,spTry,spWith) = 
    Expr.Op (TOp.TryCatch(spTry,spWith),[ty],[mkDummyLambda g (e1,ty);mkLambda ef.Range vf (ef,ty);mkLambda eh.Range vh (eh,ty)],m)

let mkTryFinally g (e1,e2,m,ty,spTry,spFinally)          = 
    Expr.Op (TOp.TryFinally(spTry,spFinally),[ty],[mkDummyLambda g (e1,ty);mkDummyLambda g (e2,g.unit_ty)],m)

let mkDefault (m,ty) = Expr.Const(Const.Zero,m,ty) 

let mkValSet m v e = Expr.Op (TOp.LValueOp (LSet, v), [], [e], m)             
let mkAddrSet m v e = Expr.Op (TOp.LValueOp (LByrefSet, v), [], [e], m)       
let mkAddrGet m v = Expr.Op (TOp.LValueOp (LByrefGet, v), [], [], m)          
let mkValAddr m v = Expr.Op (TOp.LValueOp (LGetAddr, v), [], [], m)           

//--------------------------------------------------------------------------
// Maps tracking extra information for values
//--------------------------------------------------------------------------

[<NoEquality; NoComparison>]
type ValHash<'T> = 
    | ValHash of Dictionary<Stamp,'T>
    member ht.Values = let (ValHash t) = ht in seq { for KeyValue(_,v) in t do yield v }
    member ht.TryFind (v:Val) = let (ValHash t) = ht in let i = v.Stamp in if t.ContainsKey(i) then Some(t.[i]) else None
    member ht.Add (v:Val, x) = let (ValHash t) = ht in t.[v.Stamp] <- x
    static member Create() =  ValHash (new Dictionary<_,'T>(11))

[<Struct; NoEquality; NoComparison>]
type ValMultiMap<'T>(contents: StampMap<'T list>) =
    member m.Find (v: Val) = let stamp = v.Stamp in if contents.ContainsKey stamp then contents.[stamp] else []
    member m.Add (v:Val, x) = ValMultiMap<'T>(contents.Add (v.Stamp, x :: m.Find v))
    member m.Remove (v: Val) = ValMultiMap<'T>(contents.Remove v.Stamp)
    member m.Contents  = contents
    static member Empty = ValMultiMap<'T>(Map.empty)

[<Struct; NoEquality; NoComparison>]
type TyconRefMultiMap<'T>(contents: TyconRefMap<'T list>) =
    member m.Find v = if contents.ContainsKey v then contents.[v] else []
    member m.Add (v, x) = TyconRefMultiMap<'T>(contents.Add v (x :: m.Find v))
    static member Empty = TyconRefMultiMap<'T>(TyconRefMap<_>.Empty)


//--------------------------------------------------------------------------
// From Ref_private to Ref_nonlocal when exporting data.
//--------------------------------------------------------------------------

let tryRescopeEntity viewedCcu (entity:Entity) : EntityRef option = 
    match entity.PublicPath with 
    | Some pubpath -> Some (ERefNonLocal (rescopePubPath viewedCcu pubpath))
    | None -> None


// Try to create a ValRef suitable for accessing the given Val from another assembly 
let tryRescopeVal viewedCcu (entityRemap:Remap) (vspec:Val) : ValRef option = 
    match vspec.PublicPath with 
    | Some (ValPubPath(p,fullLinkageKey)) -> 
        let fullLinkageKey = remapValLinkage entityRemap fullLinkageKey
        let vref = 
            // This compensates for the somewhat poor design decision in the F# compiler and metadata where
            // members are stored as values under the enclosing namespace/module rather than under the type.
            // This stems from the days when types and namespace/modules were separated constructs in the 
            // compiler implementation.
            if vspec.IsIntrinsicMember then  
                mkNonLocalValRef (rescopePubPathToParent viewedCcu p) fullLinkageKey
            else 
                mkNonLocalValRef (rescopePubPath viewedCcu p) fullLinkageKey
        Some vref
    | None -> None

    
//---------------------------------------------------------------------------
// Type information about records, constructors etc.
//---------------------------------------------------------------------------
 
let actualTyOfRecdField inst (fspec:RecdField)  = instType inst fspec.FormalType

let actualTysOfRecdFields inst rfields = List.map (actualTyOfRecdField inst) rfields

let actualTysOfInstanceRecdFields inst (tcref:TyconRef) = tcref.AllInstanceFieldsAsList |>  actualTysOfRecdFields inst 

let actualTysOfUnionCaseFields inst (x:UnionCaseRef) = actualTysOfRecdFields inst x.AllFieldsAsList

let actualResultTyOfUnionCase tinst (x:UnionCaseRef) = 
    instType (mkTyconRefInst x.TyconRef tinst) x.ReturnType

let recdFieldsOfExnDefRef x = (stripExnEqns x).TrueInstanceFieldsAsList
let recdFieldOfExnDefRefByIdx x n = (stripExnEqns x).GetFieldByIndex n

let recdFieldTysOfExnDefRef x = actualTysOfRecdFields [] (recdFieldsOfExnDefRef x)
let recdFieldTyOfExnDefRefByIdx x j = actualTyOfRecdField [] (recdFieldOfExnDefRefByIdx x j)


let actualTyOfRecdFieldForTycon tycon tinst (fspec:RecdField) = 
    instType (mkTyconInst tycon tinst) fspec.FormalType

let actualTyOfRecdFieldRef (fref:RecdFieldRef) tinst = 
    actualTyOfRecdFieldForTycon fref.Tycon tinst fref.RecdField

    
//---------------------------------------------------------------------------
// Apply type functions to types
//---------------------------------------------------------------------------

let destForallTy g ty = 
    let tps,tau = primDestForallTy g ty 
    // tps may be have been equated to other tps in equi-recursive type inference 
    // and unit type inference. Normalize them here 
    let tps = NormalizeDeclaredTyparsForEquiRecursiveInference g tps
    tps,tau

let tryDestForallTy g ty = 
    if isForallTy g ty then destForallTy g ty else ([],ty) 


let rec stripFunTy g ty = 
    if isFunTy g ty then 
        let (d,r) = destFunTy g ty 
        let more,rty = stripFunTy g r 
        d::more, rty
    else [],ty

let applyForallTy g ty tyargs = 
    let tps,tau = destForallTy g ty
    instType (mkTyparInst tps tyargs) tau

let reduceIteratedFunTy g ty args = 
    List.fold (fun ty _ -> 
        if not (isFunTy g ty) then failwith "reduceIteratedFunTy";
        snd (destFunTy g ty)) ty args

let applyTyArgs g functy tyargs = 
    if isForallTy g functy then applyForallTy g functy tyargs else functy

let applyTys g functy (tyargs,argtys) = 
    let afterTyappTy = applyTyArgs g functy tyargs
    reduceIteratedFunTy g afterTyappTy argtys

let formalApplyTys g functy (tyargs,args) = 
    reduceIteratedFunTy g
      (if isNil tyargs then functy else snd (destForallTy g functy))
      args

let rec stripFunTyN g n ty = 
    assert (n >= 0);
    if n > 0 && isFunTy g ty then 
        let (d,r) = destFunTy g ty
        let more,rty = stripFunTyN g (n-1) r in d::more, rty
    else [],ty

        
let tryDestTupleTy g ty = 
    if isTupleTy g ty then destTupleTy g ty else [ty]

type UncurriedArgInfos = (TType * ArgReprInfo) list 
type CurriedArgInfos = (TType * ArgReprInfo) list list

// A 'tau' type is one with its type paramaeters stripped off 
let GetTopTauTypeInFSharpForm g (curriedArgInfos: ArgReprInfo list list) tau m =
    let nArgInfos = curriedArgInfos.Length
    let argtys,rty = stripFunTyN g nArgInfos tau
    if nArgInfos <> argtys.Length then 
        error(Error(FSComp.SR.tastInvalidMemberSignature(),m))
    let argtysl = 
        (curriedArgInfos,argtys) ||> List.map2 (fun argInfos argty -> 
            match argInfos with 
            | [] -> [ (g.unit_ty, ValReprInfo.unnamedTopArg1) ]
            | [argInfo] -> [ (argty, argInfo) ]
            | _ -> List.zip (destTupleTy g argty) argInfos) 
    argtysl,rty

let destTopForallTy g (ValReprInfo (ntps,_,_)) ty =
    let tps,tau = (if isNil ntps then [],ty else tryDestForallTy g ty)
#if CHECKED
    if tps.Length <> kinds.Length then failwith (sprintf "destTopForallTy: internal error, #tps = %d, #ntps = %d" (List.length tps) ntps);
#endif
    // tps may be have been equated to other tps in equi-recursive type inference. Normalize them here 
    let tps = NormalizeDeclaredTyparsForEquiRecursiveInference g tps
    tps,tau

let GetTopValTypeInFSharpForm g (ValReprInfo(_,argInfos,retInfo) as topValInfo) ty m =
    let tps,tau = destTopForallTy g topValInfo ty
    let argtysl,rty = GetTopTauTypeInFSharpForm g argInfos tau m
    tps,argtysl,rty,retInfo


let IsCompiledAsStaticProperty g (v:Val) = 
    (isSome v.ValReprInfo &&
     match GetTopValTypeInFSharpForm g v.ValReprInfo.Value v.Type v.Range with 
     | [],[], _,_ when not v.IsMember -> true
     | _ -> false) 

let IsCompiledAsStaticPropertyWithField g (v:Val) = 
    (not v.IsCompiledAsStaticPropertyWithoutField && IsCompiledAsStaticProperty g v) 

//-------------------------------------------------------------------------
// Multi-dimensional array types...
//-------------------------------------------------------------------------

let isArrayTyconRef g tcr = 
    tyconRefEq g tcr g.il_arr1_tcr || 
    tyconRefEq g tcr g.il_arr2_tcr || 
    tyconRefEq g tcr g.il_arr3_tcr || 
    tyconRefEq g tcr g.il_arr4_tcr 

let rankOfArrayTyconRef g tcr = 
    if tyconRefEq g tcr g.il_arr1_tcr then 1
    elif tyconRefEq g tcr g.il_arr2_tcr then 2
    elif tyconRefEq g tcr g.il_arr3_tcr then 3
    elif tyconRefEq g tcr g.il_arr4_tcr then 4
    else failwith "rankOfArrayTyconRef: unsupported array rank"

//-------------------------------------------------------------------------
// Misc functions on F# types
//------------------------------------------------------------------------- 

let destArrayTy (g:TcGlobals) ty =
    let _,tinst = destAppTy g ty
    match tinst with 
    | [ty] -> ty
    | _ -> failwith "destArrayTy";

let isByrefLikeTyconRef g tcref = 
    tyconRefEq g g.byref_tcr tcref ||
    tyconRefEq g g.system_TypedReference_tcref tcref ||
    tyconRefEq g g.system_ArgIterator_tcref tcref ||
    tyconRefEq g g.system_RuntimeArgumentHandle_tcref tcref

let isArrayTy   g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> isArrayTyconRef g tcref                | _ -> false) 
let isArray1DTy  g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tyconRefEq g tcref g.il_arr1_tcr         | _ -> false) 
let isUnitTy     g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tyconRefEq g g.unit_tcr_canon tcref      | _ -> false) 
let isObjTy      g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tyconRefEq g g.system_Object_tcref tcref | _ -> false) 
let isVoidTy     g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tyconRefEq g g.system_Void_tcref tcref   | _ -> false) 
let isILAppTy g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tcref.IsILTycon                        | _ -> false) 
let isByrefTy    g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> tyconRefEq g g.byref_tcr tcref           | _ -> false) 
let isByrefLikeTy g ty = ty |> stripTyEqns g |> (function TType_app(tcref,_) -> isByrefLikeTyconRef g tcref          | _ -> false) 


let isILClassTy g ty = 
    isILAppTy g ty &&
    (let tcref,_ = destAppTy g ty
     (tcref.ILTyconRawMetadata.tdKind = ILTypeDefKind.Class))

let isILInterfaceTy g ty = 
    (isILAppTy g ty && 
     let tcr,_ = destAppTy g ty
     (tcr.ILTyconRawMetadata.tdKind = ILTypeDefKind.Interface))

let isILReferenceTy g ty = 
    (isILAppTy g ty && 
     let tcref,_ = destAppTy g ty
     not tcref.ILTyconRawMetadata.IsStructOrEnum) ||
    isArrayTy g ty

let isILInterfaceTycon (tycon:Tycon) = 
    (tycon.IsILTycon && (tycon.ILTyconRawMetadata.tdKind = ILTypeDefKind.Interface))


let rankOfArrayTy g ty = rankOfArrayTyconRef g (tcrefOfAppTy g ty)

let isFSharpObjModelRefTy g ty = 
    isFSharpObjModelTy g ty && 
    let tcr,_ = destAppTy g ty
    match tcr.FSharpObjectModelTypeInfo.fsobjmodel_kind with 
    | TTyconClass | TTyconInterface   | TTyconDelegate _ -> true
    | TTyconStruct | TTyconEnum -> false

let isTyconKindStruct k = 
    match k with 
    | TTyconClass | TTyconInterface   | TTyconDelegate _ -> false
    | TTyconStruct | TTyconEnum -> true

let isTyconKindEnum k = 
    match k with 
    | TTyconStruct | TTyconClass | TTyconInterface   | TTyconDelegate _  -> false
    | TTyconEnum -> true


let isFSharpClassTy     g ty = isAppTy g ty && (tyconOfAppTy g ty).IsFSharpClassTycon
let isFSharpStructTy    g ty = isAppTy g ty && (tyconOfAppTy g ty).IsFSharpStructOrEnumTycon
let isFSharpInterfaceTy g ty = isAppTy g ty && (tyconOfAppTy g ty).IsFSharpInterfaceTycon
let isFSharpDelegateTy  g ty = isAppTy g ty && (tyconOfAppTy g ty).IsFSharpDelegateTycon

let isDelegateTy g ty = 
    isFSharpDelegateTy g ty ||
     match tryDestAppTy g ty with 
     | Some tcref -> 
        if tcref.IsILTycon then 
            match tcref.ILTyconRawMetadata.tdKind with
            | ILTypeDefKind.Delegate -> true
            | _ -> false
        else false
     | _ -> false

let isInterfaceTy g ty = 
    isILInterfaceTy g ty || 
    isFSharpInterfaceTy g ty

let isClassTy g ty = 
    isILClassTy g ty || 
    isFSharpClassTy g ty

let isRefTy g ty = 
    isUnionTy g ty || 
    (isTupleTy g ty && not (isTupleStructTy g ty)) || 
    isRecdTy g ty || 
    isILReferenceTy g ty ||
    isFunTy g ty || 
    isReprHiddenTy g ty || 
    isFSharpObjModelRefTy g ty || 
    isUnitTy g ty

let isStructTy g ty = 
    (isAppTy g ty && (tyconOfAppTy g ty).IsStructOrEnumTycon) || isTupleStructTy g ty

// ECMA C# LANGUAGE SPECIFICATION, 27.2
// An unmanaged-type is any type that isn�t a reference-type, a type-parameter, or a generic struct-type and
// contains no fields whose type is not an unmanaged-type. In other words, an unmanaged-type is one of the
// following:
// - sbyte, byte, short, ushort, int, uint, long, ulong, char, float, double, decimal, or bool.
// - Any enum-type.
// - Any pointer-type.
// - Any non-generic user-defined struct-type that contains fields of unmanaged-types only.
// [Note: Constructed types and type-parameters are never unmanaged-types. end note]
let rec isUnmanagedTy g ty =
    if isAppTy g ty then
        let tcref = tcrefOfAppTy g ty
        let isEq tcref2  = tyconRefEq g tcref tcref2 
        if          isEq g.nativeptr_tcr || isEq g.nativeint_tcr ||
                    isEq g.sbyte_tcr || isEq g.byte_tcr || 
                    isEq g.int16_tcr || isEq g.uint16_tcr ||
                    isEq g.int32_tcr || isEq g.uint32_tcr ||
                    isEq g.int64_tcr || isEq g.uint64_tcr ||
                    isEq g.char_tcr ||
                    isEq g.float32_tcr ||
                    isEq g.float_tcr ||
                    isEq g.decimal_tcr ||
                    isEq g.bool_tcr then
            true
        else
            let tycon = tcref.Deref
            if tycon.IsEnumTycon then 
                true
            elif tycon.IsStructOrEnumTycon then
                match tycon.TyparsNoRange with
                | [] -> tycon.AllInstanceFieldsAsList |> List.forall (fun r -> isUnmanagedTy g r.rfield_type) 
                | _ -> false // generic structs are never 
            else false
    else
        false

let isInterfaceTycon x = 
    isILInterfaceTycon x || x.IsFSharpInterfaceTycon

let isInterfaceTyconRef (tcref: TyconRef) = isInterfaceTycon tcref.Deref

let isEnumTy g ty = 
    match tryDestAppTy g ty with 
    | None -> false
    | Some tcref -> tcref.IsEnumTycon

let actualReturnTyOfSlotSig parentTyInst methTyInst (TSlotSig(_,_,parentFormalTypars,methFormalTypars,_,formalRetTy)) = 
    let methTyInst = mkTyparInst methFormalTypars methTyInst
    let parentTyInst = mkTyparInst parentFormalTypars parentTyInst
    Option.map (instType (parentTyInst @ methTyInst)) formalRetTy

let slotSigHasVoidReturnTy (TSlotSig(_,_,_,_,_,formalRetTy)) = 
    isNone formalRetTy 

let returnTyOfMethod g (TObjExprMethod((TSlotSig(_,parentTy,_,_,_,_) as ss),_,methFormalTypars,_,_,_)) =
    let tinst = argsOfAppTy g parentTy
    let methTyInst = generalizeTypars methFormalTypars
    actualReturnTyOfSlotSig tinst methTyInst ss

/// Is the type 'abstract' in C#-speak
let isAbstractTycon (tycon:Tycon) = 
    if tycon.IsFSharpObjectModelTycon then 
      not tycon.IsFSharpDelegateTycon && 
      tycon.TypeContents.tcaug_abstract 
    else 
      tycon.IsILTycon && tycon.ILTyconRawMetadata.IsAbstract

//---------------------------------------------------------------------------
// Determine if a member/Val/ValRef is an explicit impl
//---------------------------------------------------------------------------

let MemberIsExplicitImpl g (membInfo:ValMemberInfo) = 
   membInfo.MemberFlags.IsOverrideOrExplicitImpl &&
   match membInfo.ImplementedSlotSigs with 
   | [] -> false
   | slotsigs -> slotsigs |> List.forall (fun slotsig -> isInterfaceTy g slotsig.ImplementedType )

let ValIsExplicitImpl g (v:Val) = 
    match v.MemberInfo with 
    | Some membInfo -> MemberIsExplicitImpl g membInfo
    | _ -> false

let ValRefIsExplicitImpl g (vref:ValRef) = ValIsExplicitImpl g vref.Deref

//---------------------------------------------------------------------------
// Find all type variables in a type, apart from those that have had 
// an equation assigned by type inference.
//---------------------------------------------------------------------------

let emptyFreeLocals = Zset.empty valOrder
let unionFreeLocals s1 s2 = 
    if s1 === emptyFreeLocals then s2
    elif s2 === emptyFreeLocals then s1
    else Zset.union s1 s2

let emptyFreeRecdFields = Zset.empty recdFieldRefOrder
let unionFreeRecdFields s1 s2 = 
    if s1 === emptyFreeRecdFields then s2
    elif s2 === emptyFreeRecdFields then s1
    else Zset.union s1 s2

let emptyFreeUnionCases = Zset.empty unionCaseRefOrder
let unionFreeUnionCases s1 s2 = 
    if s1 === emptyFreeUnionCases then s2
    elif s2 === emptyFreeUnionCases then s1
    else Zset.union s1 s2

let emptyFreeTycons = Zset.empty tyconOrder
let unionFreeTycons s1 s2 = 
    if s1 === emptyFreeTycons then s2
    elif s2 === emptyFreeTycons then s1
    else Zset.union s1 s2

let typarOrder = 
    { new System.Collections.Generic.IComparer<Typar> with 
        member x.Compare (v1:Typar, v2:Typar) = compare v1.Stamp v2.Stamp } 

let emptyFreeTypars = Zset.empty typarOrder
let unionFreeTypars s1 s2 = 
    if s1 === emptyFreeTypars then s2
    elif s2 === emptyFreeTypars then s1
    else Zset.union s1 s2

let emptyFreeTyvars =  
    { FreeTycons=emptyFreeTycons; 
      /// The summary of values used as trait solutions
      FreeTraitSolutions=emptyFreeLocals;
      FreeTypars=emptyFreeTypars}

let unionFreeTyvars fvs1 fvs2 = 
    if fvs1 === emptyFreeTyvars then fvs2 else 
    if fvs2 === emptyFreeTyvars then fvs1 else
    { FreeTycons           = unionFreeTycons fvs1.FreeTycons fvs2.FreeTycons;
      FreeTraitSolutions   = unionFreeLocals fvs1.FreeTraitSolutions fvs2.FreeTraitSolutions;
      FreeTypars           = unionFreeTypars fvs1.FreeTypars fvs2.FreeTypars }

type FreeVarOptions = 
    { canCache: bool;
      collectInTypes: bool
      includeLocalTycons: bool;
      includeTypars: bool; 
      includeLocalTyconReprs: bool;
      includeRecdFields : bool; 
      includeUnionCases : bool;
      includeLocals : bool }
      
let CollectAllNoCaching = 
        { canCache=false;
          collectInTypes=true;
          includeLocalTycons=true;
          includeLocalTyconReprs=true;
          includeRecdFields =true; 
          includeUnionCases=true;
          includeTypars=true; 
          includeLocals=true }

let CollectTyparsNoCaching = 
        { canCache=false;
          collectInTypes=true;
          includeLocalTycons=false;
          includeTypars=true; 
          includeLocalTyconReprs=false;
          includeRecdFields =false; 
          includeUnionCases=false;
          includeLocals=false }

let CollectLocalsNoCaching = 
        { canCache=false;
          collectInTypes=false;
          includeLocalTycons=false;
          includeTypars=false; 
          includeLocalTyconReprs=false;
          includeRecdFields =false; 
          includeUnionCases=false;
          includeLocals=true }

let CollectTyparsAndLocalsNoCaching = 
        { canCache=false;
          collectInTypes=true;
          includeLocalTycons=false;
          includeLocalTyconReprs=false;
          includeRecdFields =false; 
          includeUnionCases=false;
          includeTypars=true; 
          includeLocals=true }

let CollectAll =
        { canCache=false; 
          collectInTypes=true;
          includeLocalTycons=true;
          includeLocalTyconReprs=true;
          includeRecdFields =true; 
          includeUnionCases=true;
          includeTypars=true; 
          includeLocals=true }
    
let CollectTyparsAndLocals = // CollectAll
        { canCache=true; // only cache for this one
          collectInTypes=true;
          includeTypars=true; 
          includeLocals=true;
          includeLocalTycons=false;
          includeLocalTyconReprs=false;
          includeRecdFields =false; 
          includeUnionCases=false; }


let CollectTypars = CollectTyparsAndLocals
(*
        { canCache=false; 
          collectInTypes=true;
          includeTypars=true; 
          includeLocals=false; 
          includeLocalTycons=false;
          includeLocalTyconReprs=false;
          includeRecdFields =false; 
          includeUnionCases=false;}
*)

let CollectLocals = CollectTyparsAndLocals
(*
        { canCache=false; 
          collectInTypes=false;
          includeLocalTycons=false;
          includeLocalTyconReprs=false;
          includeRecdFields =false; 
          includeUnionCases=false;
          includeTypars=false; 
          includeLocals=true }
*)


let accFreeLocalTycon opts x acc = 
    if not opts.includeLocalTycons then acc else
    if Zset.contains x acc.FreeTycons then acc else 
    {acc with FreeTycons = Zset.add x acc.FreeTycons } 

let accFreeTycon opts (tcr:TyconRef) acc = 
    if not opts.includeLocalTycons then acc else
    match tcr.IsLocalRef with 
    | true -> accFreeLocalTycon opts tcr.PrivateTarget acc
    | _ -> acc

let rec boundTypars opts tps acc = 
    // Bound type vars form a recursively-referential set due to constraints, e.g.  A : I<B>, B : I<A> 
    // So collect up free vars in all constraints first, then bind all variables 
    let acc = List.foldBack (fun (tp:Typar) acc -> accFreeInTyparConstraints opts tp.Constraints acc) tps acc
    List.foldBack (fun tp acc -> {acc with FreeTypars = Zset.remove tp acc.FreeTypars}) tps acc

and accFreeInTyparConstraints opts cxs acc =
    List.foldBack (accFreeInTyparConstraint opts) cxs acc

and accFreeInTyparConstraint opts tpc acc =
    match tpc with 
    | TTyparCoercesToType(typ,_) -> accFreeInType opts typ acc
    | TTyparMayResolveMemberConstraint (traitInfo,_) -> accFreeInTrait opts traitInfo acc
    | TTyparDefaultsToType(_,rty,_) -> accFreeInType opts rty acc
    | TTyparSimpleChoice(tys,_) -> accFreeInTypes opts tys acc
    | TTyparIsEnum(uty,_) -> accFreeInType opts uty acc
    | TTyparIsDelegate(aty,bty,_) -> accFreeInType opts aty (accFreeInType opts bty acc)
    | TTyparSupportsComparison _
    | TTyparSupportsEquality _
    | TTyparSupportsNull _ 
    | TTyparIsNotNullableValueType _ 
    | TTyparIsReferenceType _ 
    | TTyparIsUnmanaged _
    | TTyparRequiresDefaultConstructor _ -> acc

and accFreeInTrait opts (TTrait(typs,_,_,argtys,rty,sln)) acc = 
    Option.foldBack (accFreeInTraitSln opts) sln.Value
       (accFreeInTypes opts typs 
         (accFreeInTypes opts argtys 
           (Option.foldBack (accFreeInType opts) rty acc)))

and accFreeInTraitSln opts sln acc = 
    match sln with 
    | ILMethSln(typ,_,_,minst) ->
         accFreeInType opts typ 
            (accFreeInTypes opts minst acc)
    | FSMethSln(typ, vref,minst) ->
         accFreeInType opts typ 
            (accFreeValRefInTraitSln opts vref  
               (accFreeInTypes opts minst acc))
    | BuiltInSln -> acc

and accFreeLocalValInTraitSln _opts v fvs =
    if Zset.contains v fvs.FreeTraitSolutions then fvs 
    else 
        let fvs = {fvs with FreeTraitSolutions=Zset.add v fvs.FreeTraitSolutions}
        //let fvs =  accFreeInVal opts v fvs
        fvs 
and accFreeValRefInTraitSln opts (vref:ValRef) fvs = 
    match vref.IsLocalRef with 
    | true -> accFreeLocalValInTraitSln opts vref.PrivateTarget fvs
    // non-local values do not contain free variables 
    | _ -> fvs

and accFreeTyparRef opts (tp:Typar) acc = 
    if not opts.includeTypars then acc else
    if Zset.contains tp acc.FreeTypars then acc
    else 
      accFreeInTyparConstraints opts tp.Constraints
        {acc with FreeTypars=Zset.add tp acc.FreeTypars}

and accFreeInType opts ty acc  = 
    match stripTyparEqns ty with 
    | TType_tuple l -> accFreeInTypes opts l acc
    | TType_app (tc,tinst) -> 
        let acc = accFreeTycon opts tc  acc
        match tinst with 
        | [] -> acc  // optimization to avoid unneeded call
        | [h] -> accFreeInType opts h acc // optimization to avoid unneeded call
        | _ -> accFreeInTypes opts tinst acc
    | TType_ucase (UCRef(tc,_),tinst) -> accFreeInTypes opts tinst (accFreeTycon opts tc  acc)
    | TType_fun (d,r) -> accFreeInType opts d (accFreeInType opts r acc)
    | TType_var r -> accFreeTyparRef opts r acc
    | TType_forall (tps,r) -> unionFreeTyvars (boundTypars opts tps (freeInType opts r)) acc
    | TType_measure unt -> accFreeInMeasure opts unt acc

and accFreeInMeasure opts unt acc = List.foldBack (fun (tp,_) acc -> accFreeTyparRef opts tp acc) (ListMeasureVarOccsWithNonZeroExponents unt) acc
and accFreeInTypes opts tys acc = 
    match tys with 
    | [] -> acc
    | h :: t -> accFreeInTypes opts t (accFreeInType opts h acc)
and freeInType opts ty = accFreeInType opts ty emptyFreeTyvars

and accFreeInVal opts (v:Val) acc = accFreeInType opts v.Data.val_type acc

let freeInTypes opts tys = accFreeInTypes opts tys emptyFreeTyvars
let freeInVal opts v = accFreeInVal opts v emptyFreeTyvars
let freeInTyparConstraints opts v = accFreeInTyparConstraints opts v emptyFreeTyvars
let accFreeInTypars opts tps acc = List.foldBack (accFreeTyparRef opts) tps acc
        

//--------------------------------------------------------------------------
// Free in type, left-to-right order preserved. This is used to determine the
// order of type variables for top-level definitions based on their signature,
// so be careful not to change the order.  We accumulate in reverse
// order.
//--------------------------------------------------------------------------

let emptyFreeTyparsLeftToRight = []
let unionFreeTyparsLeftToRight fvs1 fvs2 = ListSet.unionFavourRight typarEq fvs1 fvs2

let rec boundTyparsLeftToRight g cxFlag thruFlag acc tps = 
    // Bound type vars form a recursively-referential set due to constraints, e.g.  A : I<B>, B : I<A> 
    // So collect up free vars in all constraints first, then bind all variables 
    let acc = List.fold (fun acc (tp:Typar) -> accFreeInTyparConstraintsLeftToRight g cxFlag thruFlag acc tp.Constraints) tps acc
    List.foldBack (ListSet.remove typarEq) tps acc

and accFreeInTyparConstraintsLeftToRight g cxFlag thruFlag acc cxs =
    List.fold (accFreeInTyparConstraintLeftToRight g cxFlag thruFlag) acc cxs 

and accFreeInTyparConstraintLeftToRight g cxFlag thruFlag acc tpc =
    match tpc with 
    | TTyparCoercesToType(typ,_) -> accFreeInTypeLeftToRight g cxFlag thruFlag acc typ 
    | TTyparMayResolveMemberConstraint (traitInfo,_) -> accFreeInTraitLeftToRight g cxFlag thruFlag acc traitInfo 
    | TTyparDefaultsToType(_,rty,_) -> accFreeInTypeLeftToRight g cxFlag thruFlag acc rty 
    | TTyparSimpleChoice(tys,_) -> accFreeInTypesLeftToRight g cxFlag thruFlag acc tys 
    | TTyparIsEnum(uty,_) -> accFreeInTypeLeftToRight g cxFlag thruFlag acc uty
    | TTyparIsDelegate(aty,bty,_) -> accFreeInTypeLeftToRight g cxFlag thruFlag (accFreeInTypeLeftToRight g cxFlag thruFlag acc aty) bty  
    | TTyparSupportsComparison _ 
    | TTyparSupportsEquality _ 
    | TTyparSupportsNull _ 
    | TTyparIsNotNullableValueType _ 
    | TTyparIsUnmanaged _
    | TTyparIsReferenceType _ 
    | TTyparRequiresDefaultConstructor _ -> acc

and accFreeInTraitLeftToRight g cxFlag thruFlag acc (TTrait(typs,_,_,argtys,rty,_))  = 
    let acc = accFreeInTypesLeftToRight g cxFlag thruFlag acc typs
    let acc = accFreeInTypesLeftToRight g cxFlag thruFlag acc argtys
    let acc = Option.fold (accFreeInTypeLeftToRight g cxFlag thruFlag) acc rty
    acc

and accFreeTyparRefLeftToRight g cxFlag thruFlag acc (tp:Typar) = 
    if ListSet.contains typarEq tp acc 
    then acc
    else 
        let acc = (ListSet.insert typarEq tp acc)
        if cxFlag then 
            accFreeInTyparConstraintsLeftToRight g cxFlag thruFlag acc tp.Constraints
        else 
            acc

and accFreeInTypeLeftToRight g cxFlag thruFlag acc ty  = 
    if verbose then  dprintf "--> accFreeInTypeLeftToRight \n";
    match (if thruFlag then stripTyEqns g ty else stripTyparEqns ty) with 
    | TType_tuple l -> accFreeInTypesLeftToRight g cxFlag thruFlag acc l 
    | TType_app (_,tinst) -> accFreeInTypesLeftToRight g cxFlag thruFlag acc tinst 
    | TType_ucase (_,tinst) -> accFreeInTypesLeftToRight g cxFlag thruFlag acc tinst 
    | TType_fun (d,r) -> accFreeInTypeLeftToRight g cxFlag thruFlag (accFreeInTypeLeftToRight g cxFlag thruFlag acc d ) r
    | TType_var r -> accFreeTyparRefLeftToRight g cxFlag thruFlag acc r 
    | TType_forall (tps,r) -> unionFreeTyparsLeftToRight (boundTyparsLeftToRight g cxFlag thruFlag tps (accFreeInTypeLeftToRight g cxFlag thruFlag emptyFreeTyparsLeftToRight r)) acc
    | TType_measure unt -> List.foldBack (fun (tp,_) acc -> accFreeTyparRefLeftToRight g cxFlag thruFlag acc tp) (ListMeasureVarOccsWithNonZeroExponents unt) acc

and accFreeInTypesLeftToRight g cxFlag thruFlag acc tys = 
    match tys with 
    | [] -> acc
    | h :: t -> accFreeInTypesLeftToRight g cxFlag thruFlag (accFreeInTypeLeftToRight g cxFlag thruFlag acc h) t
    
let freeInTypeLeftToRight g thruFlag ty = accFreeInTypeLeftToRight g true thruFlag emptyFreeTyparsLeftToRight ty |> List.rev
let freeInTypesLeftToRight g thruFlag ty = accFreeInTypesLeftToRight g true thruFlag emptyFreeTyparsLeftToRight ty |> List.rev
let freeInTypesLeftToRightSkippingConstraints g ty = accFreeInTypesLeftToRight g false true emptyFreeTyparsLeftToRight ty |> List.rev

let valOfBind (b:Binding) = b.Var
let valsOfBinds (binds:Bindings) = binds |> FlatList.map (fun b -> b.Var)

//--------------------------------------------------------------------------
// Values representing member functions on F# types
//--------------------------------------------------------------------------

// Pull apart the type for an F# value that represents an object model method. Do not strip off a 'unit' argument.
// Review: Should GetMemberTypeInFSharpForm have any other direct callers? 
let GetMemberTypeInFSharpForm g memberFlags arities ty m = 
    let tps,argInfos,rty,retInfo = GetTopValTypeInFSharpForm g arities ty m
    let numObjArgs = if memberFlags.IsInstance then 1 else 0    

    let argInfos = 
        if numObjArgs = 1 then 
            match argInfos with
            | [] -> 
                errorR(InternalError("value does not have a valid member type",m)); 
                argInfos
            | _::t -> t
        else argInfos
    tps,argInfos,rty,retInfo

// Check that an F# value represents an object model method. 
// It will also always have an arity (inferred from syntax). 
let checkMemberVal membInfo arity m =
    match membInfo, arity with 
    | None,_ -> error(InternalError("checkMemberVal - no membInfo" , m))
    | _,None -> error(InternalError("checkMemberVal - no arity", m))
    | Some membInfo,Some arity ->  (membInfo,arity)

let checkMemberValRef (vref:ValRef) =
    checkMemberVal vref.MemberInfo vref.ValReprInfo vref.Range
     
let GetTopValTypeInCompiledForm g topValInfo typ m =
    let tps,paramArgInfos,rty,retInfo = GetTopValTypeInFSharpForm g topValInfo typ m
    // Eliminate lone single unit arguments
    let paramArgInfos = 
        match paramArgInfos, topValInfo.ArgInfos with 
        // static member and module value unit argument elimination
        | [[(_argType,_)]] ,[[]] -> 
            //assert isUnitTy g argType 
            [[]]
        // instance member unit argument elimination
        | [objInfo;[(_argType,_)]] ,[[_objArg];[]] -> 
            //assert isUnitTy g argType 
            [objInfo; []]
        | _ -> 
            paramArgInfos
    let rty = (if isUnitTy g rty then None else Some rty)
    (tps,paramArgInfos,rty,retInfo)
     
// Pull apart the type for an F# value that represents an object model method
// and see the "member" form for the type, i.e. 
// detect methods with no arguments by (effectively) looking for single argument type of 'unit'. 
// The analysis is driven of the inferred arity information for the value.
//
// This is used not only for the compiled form - it's also used for all type checking and object model
// logic such as determining if abstract methods have been implemented or not, and how
// many arguments the method takes etc.
let GetMemberTypeInMemberForm g memberFlags topValInfo typ m =
    let tps,paramArgInfos,rty,retInfo = GetMemberTypeInFSharpForm g memberFlags topValInfo typ m
    // Eliminate lone single unit arguments
    let paramArgInfos = 
        match paramArgInfos, topValInfo.ArgInfos with 
        // static member and module value unit argument elimination
        | [[(argType,_)]] ,[[]] -> 
            assert isUnitTy g argType 
            [[]]
        // instance member unit argument elimination
        | [[(argType,_)]] ,[[_objArg];[]] -> 
            assert isUnitTy g argType 
            [[]]
        | _ -> 
            paramArgInfos
    let rty = (if isUnitTy g rty then None else Some rty)
    (tps,paramArgInfos,rty,retInfo)

let GetTypeOfMemberInMemberForm g (vref:ValRef) =
    //assert (not vref.IsExtensionMember)
    let membInfo,topValInfo = checkMemberValRef vref
    GetMemberTypeInMemberForm g membInfo.MemberFlags topValInfo vref.Type vref.Range

let GetTypeOfMemberInFSharpForm g (vref:ValRef) =
    let membInfo,topValInfo = checkMemberValRef vref
    GetMemberTypeInFSharpForm g membInfo.MemberFlags topValInfo vref.Type vref.Range

let GetReturnTypeOMemberInMemberForm g (vref:ValRef) =
    let membInfo,topValInfo = checkMemberValRef vref
    let _,_,rty,_ = GetMemberTypeInMemberForm g membInfo.MemberFlags topValInfo vref.Type vref.Range
    rty
  
/// Match up the type variables on an member value with the type 
/// variables on the apparent enclosing type
let PartitionValTypars g (v:Val)  = 
     match v.ValReprInfo with 
     | None -> error(InternalError("PartitionValTypars: not a top value", v.Range))
     | Some arities -> 
         let fullTypars,_ = destTopForallTy g arities v.Type 
         let parent = v.MemberApparentParent
         let parentTypars = parent.TyparsNoRange
         let nparentTypars = parentTypars.Length
         if nparentTypars <= fullTypars.Length then 
             let memberParentTypars,memberMethodTypars = List.chop nparentTypars fullTypars
             let memberToParentInst,tinst = mkTyparToTyparRenaming memberParentTypars parentTypars
             Some(parentTypars,memberParentTypars,memberMethodTypars,memberToParentInst,tinst)
         else None

let PartitionValRefTypars g (vref: ValRef) = PartitionValTypars g vref.Deref 

/// Get the arguments for an F# value that represents an object model method 
let ArgInfosOfMemberVal g (v:Val) = 
    let membInfo,topValInfo = checkMemberVal v.MemberInfo v.ValReprInfo v.Range
    let _,arginfos,_,_ = GetMemberTypeInMemberForm g membInfo.MemberFlags topValInfo v.Type v.Range
    arginfos

let ArgInfosOfMember g (vref: ValRef) = 
    ArgInfosOfMemberVal g vref.Deref

let GetFSharpViewOfReturnType g retTy =
    match retTy with 
    | None -> g.unit_ty
    | Some retTy ->  retTy


/// Get the property "type" (getter return type) for an F# value that represents a getter or setter
/// of an object model property.
let ReturnTypeOfPropertyVal g (v:Val) = 
    let membInfo,topValInfo = checkMemberVal v.MemberInfo v.ValReprInfo v.Range
    match membInfo.MemberFlags.MemberKind with 
    | MemberKind.PropertySet ->
        let _,arginfos,_,_ = GetMemberTypeInMemberForm g membInfo.MemberFlags topValInfo v.Type v.Range
        if not arginfos.IsEmpty && not arginfos.Head.IsEmpty then
            arginfos.Head |> List.last |> fst 
        else
            error(Error(FSComp.SR.tastValueDoesNotHaveSetterType(), v.Range));
    | MemberKind.PropertyGet ->
        let _,_,rty,_ = GetMemberTypeInMemberForm g membInfo.MemberFlags topValInfo v.Type v.Range
        GetFSharpViewOfReturnType g rty
    | _ -> error(InternalError("ReturnTypeOfPropertyVal",v.Range))


/// Get the property arguments for an F# value that represents a getter or setter
/// of an object model property.
let ArgInfosOfPropertyVal g (v:Val) = 
    let membInfo,topValInfo = checkMemberVal v.MemberInfo v.ValReprInfo v.Range
    match membInfo.MemberFlags.MemberKind with 
    | MemberKind.PropertyGet ->
        ArgInfosOfMemberVal g v |> List.concat
    | MemberKind.PropertySet ->
        let _,arginfos,_,_ = GetMemberTypeInMemberForm g membInfo.MemberFlags topValInfo v.Type v.Range
        if not arginfos.IsEmpty && not arginfos.Head.IsEmpty then
            arginfos.Head |> List.frontAndBack |> fst 
        else
            error(Error(FSComp.SR.tastValueDoesNotHaveSetterType(), v.Range));
    | _ -> 
        error(InternalError("ArgInfosOfPropertyVal",v.Range))

//---------------------------------------------------------------------------
// Generalize type constructors to types
//---------------------------------------------------------------------------

let generalTyconRefInst (tc:TyconRef) =  generalizeTypars tc.TyparsNoRange

let generalizeTyconRef tc = 
    let tinst = generalTyconRefInst tc
    tinst,TType_app(tc, tinst)

let generalizedTyconRef tc = TType_app(tc, generalTyconRefInst tc)

let isTTyparSupportsStaticMethod = function TTyparMayResolveMemberConstraint _ -> true | _ -> false
let isTTyparCoercesToType = function TTyparCoercesToType _ -> true | _ -> false

//--------------------------------------------------------------------------
// Print Signatures/Types - prelude
//-------------------------------------------------------------------------- 

let prefixOfStaticReq s =
    match s with 
    | NoStaticReq -> "'"
    | HeadTypeStaticReq -> " +"

let prefixOfRigidTypar (typar:Typar) =  
  if (typar.Rigidity <> TyparRigid) then "_" else ""

//---------------------------------------------------------------------------
// Prettify: PrettyTyparNames/PrettifyTypes - make typar names human friendly
//---------------------------------------------------------------------------

module PrettyTypes = begin

    let newPrettyTypar (tp:Typar) nm = 
        NewTypar (tp.Kind, tp.Rigidity,Typar(ident(nm, tp.Range),tp.StaticReq,false),false,DynamicReq,[],false,false)

    let newPrettyTypars renaming tps names = 
        let niceTypars = List.map2 newPrettyTypar tps names
        let renaming = renaming @ mkTyparInst tps (generalizeTypars niceTypars)
        (tps,niceTypars) ||> List.iter2 (fun tp tpnice -> tpnice.FixupConstraints (instTyparConstraints renaming tp.Constraints)) ;
        niceTypars, renaming

    // We choose names for type parameters from 'a'..'t'
    // We choose names for unit-of-measure from 'u'..'z'
    // If we run off the end of these ranges, we use 'aX' for positive integer X or 'uX' for positive integer X
    // Finally, we skip any names already in use
    let NeedsPrettyTyparName (tp:Typar) = 
        tp.IsCompilerGenerated && 
        tp.Data.typar_il_name.IsNone && 
        (tp.Data.typar_id.idText = unassignedTyparName) 

    let PrettyTyparNames pred alreadyInUse tps = 
        let rec choose (tps:Typar list) (typeIndex, measureIndex) acc = 
            match tps with
            | [] -> List.rev acc
            | tp::tps ->
            

                // Use a particular name, possibly after incrementing indexes
                let useThisName (nm, typeIndex, measureIndex) = 
                    choose tps (typeIndex, measureIndex) (nm::acc)

                // Give up, try again with incremented indexes
                let tryAgain (typeIndex, measureIndex) = 
                    choose (tp::tps) (typeIndex, measureIndex) acc

                let tryName (nm, typeIndex, measureIndex) f = 
                    if List.mem nm alreadyInUse then 
                        f()
                    else
                        useThisName (nm, typeIndex, measureIndex)

                if pred tp then 
                    if NeedsPrettyTyparName tp then 
                        let (typeIndex, measureIndex, baseName, letters, i) = 
                          match tp.Kind with 
                          | KindType -> (typeIndex+1,measureIndex,'a',20,typeIndex) 
                          | KindMeasure -> (typeIndex,measureIndex+1,'u',6,measureIndex)
                        let nm = 
                           if i < letters then String.make 1 (char(int baseName + i)) 
                           else String.make 1 baseName + string (i-letters+1)
                        tryName (nm, typeIndex, measureIndex)  (fun () -> 
                            tryAgain (typeIndex, measureIndex))

                    else
                        tryName (tp.Name, typeIndex, measureIndex) (fun () -> 
                            // Use the next index and append it to the natural name
                            let (typeIndex, measureIndex, nm) = 
                              match tp.Kind with 
                              | KindType -> (typeIndex+1,measureIndex,tp.Name+ string typeIndex) 
                              | KindMeasure -> (typeIndex,measureIndex+1,tp.Name+ string measureIndex)
                            tryName (nm,typeIndex, measureIndex) (fun () -> 
                                tryAgain (typeIndex, measureIndex)))
                else
                    useThisName (tp.Name,typeIndex, measureIndex)

                          
        choose tps (0,0) []

    let PrettifyTypes g foldTys mapTys tys = 
        let ftps = foldTys (accFreeInTypeLeftToRight g true false) emptyFreeTyparsLeftToRight tys
        let ftps = List.rev ftps
        let rec computeKeep (keep: Typars) change (tps: Typars) = 
            match tps with 
            | [] -> List.rev keep, List.rev change 
            | tp :: rest -> 
                if not (NeedsPrettyTyparName tp) && (not (keep |> List.exists (fun tp2 -> tp.Name = tp2.Name)))  then
                    computeKeep (tp :: keep) change rest
                else 
                    computeKeep keep (tp :: change) rest
        let keep,change = computeKeep [] [] ftps
        
        // change |> List.iter (fun tp -> dprintf "change typar: %s %s %d\n" tp.Name (tp.DisplayName) (stamp_of_typar tp));  
        // keep |> List.iter (fun tp -> dprintf "keep typar: %s %s %d\n" tp.Name (tp.DisplayName) (stamp_of_typar tp));  
        let alreadyInUse = keep |> List.map (fun x -> x.Name)
        let names = PrettyTyparNames (fun x -> List.memq x change) alreadyInUse ftps

        let niceTypars, renaming = newPrettyTypars [] ftps names 
        let prettyTypars = mapTys (instType renaming) tys
        // niceTypars |> List.iter (fun tp -> dprintf "nice typar: %d\n" (stamp_of_typar tp)); *
        let tpconstraints  = niceTypars |> List.collect (fun tpnice -> List.map (fun tpc -> tpnice,tpc) tpnice.Constraints)

        renaming,
        prettyTypars,
        tpconstraints

    let PrettifyTypes1   g x = PrettifyTypes g (fun f -> f) (fun f -> f) x
    let PrettifyTypes2   g x = PrettifyTypes g (fun f -> foldPair (f,f)) (fun f -> mapPair (f,f)) x
    let PrettifyTypesN   g x = PrettifyTypes g List.fold List.map   x
    let PrettifyTypesN1  g x = PrettifyTypes g (fun f -> foldPair (List.fold (fold1Of2  f), f)) (fun f -> mapPair (List.map (map1Of2  f),f)) x
    let PrettifyTypesNN1 g x = PrettifyTypes g (fun f -> foldTriple (List.fold f, List.fold (fold1Of2 f),f)) (fun f -> mapTriple (List.map f, List.map (map1Of2  f), f)) x
    let PrettifyTypesNM1 g x = PrettifyTypes g (fun f -> foldTriple (List.fold f, List.fold (List.fold (fold1Of2 f)),f)) (fun f -> mapTriple (List.map f, List.mapSquared (map1Of2  f), f)) x

end


 
module SimplifyTypes = begin

    // CAREFUL! This function does NOT walk constraints 
    let rec FoldType f z typ =
        let typ = stripTyparEqns typ 
        let z = f z typ
        match typ with
        | TType_forall (_,body) -> FoldType f z body
        | TType_app (_,tinst) -> List.fold (FoldType f) z tinst
        | TType_ucase (_,tinst) -> List.fold (FoldType f) z tinst
        | TType_tuple typs        -> List.fold (FoldType f) z typs
        | TType_fun (s,t)         -> FoldType f (FoldType f z s) t
        | TType_var _            -> z
        | TType_measure _          -> z

    let incM x m =
        if Zmap.mem x m then Zmap.add x (1 + Zmap.find x m) m
        else Zmap.add x 1 m

    let AccTyparCounts z typ =
        // Walk type to determine typars and their counts (for pprinting decisions) 
        FoldType (fun z typ -> match typ with | TType_var tp when tp.Rigidity = TyparRigid  -> incM tp z | _ -> z) z typ

    let emptyTyparCounts = Zmap.empty typarOrder

    // print multiple fragments of the same type using consistent naming and formatting 
    let AccTyparCountsMulti acc l = List.fold AccTyparCounts acc l

    type TypeSimplificationInfo =
        { singletons         : Typar Zset;
          inplaceConstraints :  Zmap<Typar,TType>;
          postfixConstraints : (Typar * TyparConstraint) list; }
          
    let typeSimplificationInfo0 = 
        { singletons         = Zset.empty typarOrder;
          inplaceConstraints = Zmap.empty typarOrder;
          postfixConstraints = [] }

    let CategorizeConstraints simplify m cxs =
        let singletons = if simplify then Zmap.chooseL (fun tp n -> if n=1 then Some tp else None) m else []
        let singletons = Zset.addList singletons (Zset.empty typarOrder)
        // Here, singletons are typars that occur once in the type.
        // However, they may also occur in a type constraint.
        // If they do, they are really multiple occurance - so we should remove them.
        let constraintTypars = (freeInTyparConstraints CollectTyparsNoCaching (List.map snd cxs)).FreeTypars
        let usedInTypeConstraint typar = Zset.contains typar constraintTypars
        let singletons = singletons |> Zset.filter (usedInTypeConstraint >> not) 
        // Here, singletons should really be used once 
        let inplace,postfix =
          cxs |> List.partition (fun (tp,tpc) -> 
            simplify &&
            isTTyparCoercesToType tpc && 
            Zset.contains tp singletons && 
            tp.Constraints.Length = 1)
        let inplace = inplace |> List.map (function (tp,TTyparCoercesToType(ty,_)) -> tp,ty | _ -> failwith "not isTTyparCoercesToType")
        
        { singletons         = singletons;
          inplaceConstraints = Zmap.ofList typarOrder inplace;
          postfixConstraints = postfix;
        }
    let CollectInfo simplify tys cxs = 
        CategorizeConstraints simplify (AccTyparCountsMulti emptyTyparCounts tys) cxs 
        
end

//--------------------------------------------------------------------------
// Print Signatures/Types
//-------------------------------------------------------------------------- 

[<NoEquality; NoComparison>]
type DisplayEnv = 
    { openTopPathsSorted: Lazy<string list list>; 
      openTopPathsRaw: string list list; 
      showObsoleteMembers: bool; 
      showTyparBinding: bool; 
      showImperativeTyparAnnotations: bool;
      suppressInlineKeyword: bool;
      suppressMutableKeyword: bool;
      showMemberContainers:bool;
      shortConstraints:bool;
      useColonForReturnType:bool;
      showAttributes:bool;
      showOverrides:bool;
      showConstraintTyparAnnotations: bool;
      abbreviateAdditionalConstraints: bool;
      showTyparDefaultConstraints : bool;
      g: TcGlobals;
      contextAccessibility: Accessibility;
      generatedValueLayout:(Val -> layout option);    
      }

    member x.SetOpenPaths(paths) = 
        { x with 
             openTopPathsSorted = (lazy (paths |> List.sortWith (fun p1 p2 -> -(compare p1 p2))));
             openTopPathsRaw = paths 
        }

    static member Empty tcGlobals = 
      { openTopPathsRaw = []; 
        openTopPathsSorted = notlazy []; 
        showObsoleteMembers=true;
        showTyparBinding = false;
        showImperativeTyparAnnotations=false;
        suppressInlineKeyword=false;
        suppressMutableKeyword=false;
        showMemberContainers=false;
        showAttributes=false;
        showOverrides=true;
        showConstraintTyparAnnotations=true;
        abbreviateAdditionalConstraints=false;
        showTyparDefaultConstraints=false;
        shortConstraints=false;
        useColonForReturnType=false;
        g=tcGlobals;
        contextAccessibility = taccessPublic;
        generatedValueLayout = (fun _ -> None) }


    member denv.AddOpenPath path = 
        denv.SetOpenPaths (path :: denv.openTopPathsRaw)

    member denv.AddOpenModuleOrNamespace (modref: ModuleOrNamespaceRef) = 
        denv.AddOpenPath (demangledPathOfCompPath (fullCompPathOfModuleOrNamespace modref.Deref))

    member denv.AddAccessibility access =     
        { denv with contextAccessibility = combineAccess denv.contextAccessibility access }

let (+.+) s1 s2 = (if s1 = "" then s2 else s1+"."+s2)

let fullNameOfParentOfPubPath pp = 
    match pp with 
    | PubPath([| _ |]) -> None 
    | pp -> Some(textOfPath (Array.toList pp.EnclosingPath))

let fullNameOfPubPath (PubPath(p)) = textOfPath (Array.toList p) 

let fullNameOfParentOfNonLocalEntityRef (nlr: NonLocalEntityRef) = 
    if nlr.Path.Length = 0 then None
    else Some (textOfArrPath nlr.EnclosingMangledPath)  // <--- BAD BAD BAD: this is a mangled path. This is wrong for nested modules

let fullNameOfParentOfEntityRef eref = 
    match eref with 
    | ERefLocal x ->
         match x.PublicPath with 
         | None -> None
         | Some ppath -> fullNameOfParentOfPubPath ppath
    | ERefNonLocal nlr -> fullNameOfParentOfNonLocalEntityRef nlr

let fullNameOfEntityRef nmF xref = 
    match fullNameOfParentOfEntityRef xref  with 
    | None -> nmF xref 
    | Some pathText -> pathText +.+ nmF xref
  
let fullNameOfParentOfValRef vref = 
    match vref with 
    | VRefLocal x -> 
         match x.PublicPath with 
         | None -> None
         | Some (ValPubPath(pp,_)) -> Some(fullNameOfPubPath pp)
    | VRefNonLocal nlr -> 
        Some (fullNameOfEntityRef (fun (x:EntityRef) -> x.DemangledModuleOrNamespaceName) nlr.EnclosingEntity)

let fullDisplayTextOfParentOfModRef r = fullNameOfParentOfEntityRef r 

let fullDisplayTextOfModRef r = fullNameOfEntityRef (fun (x:EntityRef) -> x.DemangledModuleOrNamespaceName)  r
let fullDisplayTextOfTyconRef  r = fullNameOfEntityRef (fun (tc:TyconRef) -> tc.DisplayNameWithUnderscoreTypars) r
let fullDisplayTextOfExnRef  r = fullNameOfEntityRef (fun (tc:TyconRef) -> tc.DisplayNameWithUnderscoreTypars) r

let fullDisplayTextOfUnionCaseRef (ucref:UnionCaseRef) = fullDisplayTextOfTyconRef ucref.TyconRef +.+ ucref.CaseName
let fullDisplayTextOfRecdFieldRef (rfref:RecdFieldRef) = fullDisplayTextOfTyconRef rfref.TyconRef +.+ rfref.FieldName

let fullDisplayTextOfValRef   (vref:ValRef) = 
    match fullNameOfParentOfValRef   vref  with 
    | None -> vref.DisplayName 
    | Some pathText -> pathText +.+ vref.DisplayName


// This is a broken version of 'fullNameOfEntityRef' that uses a mangled name in the Ref_nonlocal case.
// It is only used by the FSI generation code, which uses a really unstable and subtle technique to propagate information from
// the pretty printing code that lays out signautre files to the consuming code in the Language Service.
let fullMangledNameOfEntityRef_DO_NOT_USE nmF xref = 
    match xref with 
    | ERefLocal _ -> 
        match fullNameOfParentOfEntityRef xref  with 
        | None -> nmF xref 
        | Some pathText -> pathText +.+ nmF xref
    | ERefNonLocal nlr -> 
        let nm = nlr.LastItemMangledName //   <<----- BAD BAD BAD - this is a mangled name. The FSI generation code expects exactly this name to be used for non-local references
        match fullNameOfParentOfEntityRef xref  with 
        | None -> nm 
        | Some pathText -> pathText +.+ nm

let fullMangledNameOfValRef_DO_NOT_USE  (xref:ValRef) = 
    match xref with 
    | VRefLocal x -> 
        match fullNameOfParentOfValRef xref  with 
        | None -> x.LogicalName
        | Some pathText -> pathText +.+ x.LogicalName
    | VRefNonLocal nlr -> 
        let nm = nlr.ItemKey.PartialKey.LogicalName //   <<----- this is a compiled name. The FSI generation code expects exactly this name to be used for non-local references
        match fullNameOfParentOfNonLocalEntityRef nlr.EnclosingEntity.nlr with 
        | None -> nm 
        | Some pathText -> pathText +.+ nm

let approxFullMangledNameOfModuleOrNamespaceRef r = fullMangledNameOfEntityRef_DO_NOT_USE (fun (x:EntityRef) -> x.DemangledModuleOrNamespaceName) r
let approxFullMangledNameOfTyconRef  r = fullMangledNameOfEntityRef_DO_NOT_USE (fun (tc:TyconRef) -> tc.DisplayName) r
let approxFullMangledNameOfExnRef  r = fullMangledNameOfEntityRef_DO_NOT_USE (fun (tc:TyconRef) -> tc.DisplayName) r
let approxFullMangledNameOfUnionCaseRef (ucref:UnionCaseRef) = approxFullMangledNameOfTyconRef ucref.TyconRef +.+ ucref.CaseName
let approxFullMangledNameOfRecdFieldRef (rfref:RecdFieldRef) = approxFullMangledNameOfTyconRef rfref.TyconRef +.+ rfref.FieldName
let approxFullMangledNameOfValRef   r = fullMangledNameOfValRef_DO_NOT_USE r

let fullMangledPathToTyconRef (tcref:TyconRef) = 
    match tcref with 
    | ERefLocal _ -> (match tcref.PublicPath with None -> [| |] | Some pp -> pp.EnclosingPath)
    | ERefNonLocal nlr -> nlr.EnclosingMangledPath
  
let qualifiedMangledNameOfTyconRef tcref nm = 
    String.concat "-" (Array.toList (fullMangledPathToTyconRef tcref) @ [ tcref.LogicalName + "-" + nm ])

let rec firstEq p1 p2 = 
    match p1 with
    | [] -> true 
    | h1::t1 -> 
        match p2 with 
        | h2::t2 -> h1 = h2 && firstEq t1 t2
        | _ -> false 

let rec firstRem p1 p2 = 
   match p1 with [] -> p2 | _::t1 -> firstRem t1 (List.tail p2)

let trimPathByDisplayEnv denv path =
    let findOpenedNamespace opened_path = 
        if  firstEq opened_path path then 
          let t2 = firstRem opened_path path
          if t2 <> [] then Some(textOfPath t2+".")
          else Some("")
        else None
    match List.tryPick findOpenedNamespace (denv.openTopPathsSorted.Force()) with
    | Some s -> s
    | None ->  if isNil path then "" else textOfPath path + "."


let superOfTycon g (tycon:Tycon) = 
    match tycon.TypeContents.tcaug_super with 
    | None -> g.obj_ty 
    | Some ty -> ty 

//----------------------------------------------------------------------------
// Detect attributes
//----------------------------------------------------------------------------

// AbsIL view of attributes (we read these from .NET binaries) 
let isILAttrib (tref:ILTypeRef) attr = 
    (attr.Method.EnclosingType.TypeSpec.Name = tref.Name) &&
    (attr.Method.EnclosingType.TypeSpec.Enclosing = tref.Enclosing)

// These linear iterations cost us a fair bit when there are lots of attributes
// on imported types. However this is fairly rare and can also be solved by caching the
// results of attribute lookups in the TAST
let ILThingHasILAttrib tref (attrs: ILAttributes) = List.exists (isILAttrib tref) attrs.AsList

let ILThingDecodeILAttrib g tref scope (attrs: ILAttributes) = 
    attrs.AsList |> List.tryPick(fun x -> if isILAttrib tref x then Some(decodeILAttribData g.ilg x scope)  else None)

// This one is done by name to ensure the compiler doesn't take a dependency on dereferencing a type that only exists in .NET 3.5
let ILThingHasExtensionAttribute (attrs : ILAttributes) = 
    attrs.AsList |> List.exists (fun attr -> 
        attr.Method.EnclosingType.TypeSpec.Name = "System.Runtime.CompilerServices.ExtensionAttribute")
    
// F# view of attributes (these get converted to AbsIL attributes in ilxgen) 
let IsMatchingAttrib g (AttribInfo(_,tcref)) (Attrib(tcref2,_,_,_,_,_)) = tyconRefEq g tcref  tcref2
let HasAttrib g tref attrs = List.exists (IsMatchingAttrib g tref) attrs
let findAttrib g tref attrs = List.find (IsMatchingAttrib g tref) attrs
let TryFindAttrib g tref attrs = List.tryFind (IsMatchingAttrib g tref) attrs

let (|ExtractAttribNamedArg|_|) nm args = 
    args |> List.tryPick (function (AttribNamedArg(nm2,_,_,v)) when nm = nm2 -> Some v | _ -> None) 

let (|AttribInt32Arg|_|) = function AttribExpr(_,Expr.Const (Const.Int32(n),_,_)) -> Some(n) | _ -> None
let (|AttribInt16Arg|_|) = function AttribExpr(_,Expr.Const (Const.Int16(n),_,_)) -> Some(n) | _ -> None
let (|AttribBoolArg|_|) = function AttribExpr(_,Expr.Const (Const.Bool(n),_,_)) -> Some(n) | _ -> None
let (|AttribStringArg|_|) = function AttribExpr(_,Expr.Const (Const.String(n),_,_)) -> Some(n) | _ -> None

let TryFindBoolAttrib g nm attrs = 
    match TryFindAttrib g nm attrs with
    | Some(Attrib(_,_,[ ],_,_,_)) -> Some(true)
    | Some(Attrib(_,_,[ AttribBoolArg(b) ],_,_,_)) -> Some(b)
    | _ -> None

let TryFindUnitAttrib g nm attrs = 
    match TryFindAttrib g nm attrs with
    | Some(Attrib(_,_,[ ],_,_,_)) -> Some()
    | _ -> None

let TryFindInt32Attrib g nm attrs = 
    match TryFindAttrib g nm attrs with
    | Some(Attrib(_,_,[ AttribInt32Arg(b) ],_,_,_)) -> Some b
    | _ -> None
    
let TryFindStringAttrib g nm attrs = 
    match TryFindAttrib g nm attrs with
    | Some(Attrib(_,_,[ AttribStringArg(b) ],_,_,_)) -> Some b
    | _ -> None
    
let ILThingHasAttrib (AttribInfo (atref,_)) attrs = 
    ILThingHasILAttrib atref attrs

let TyconRefTryBindAttrib g (AttribInfo (atref,_) as args) (tcref:TyconRef) f1 f2 = 
    if tcref.IsILTycon then 
        match ILThingDecodeILAttrib g atref (Some(atref.Scope)) tcref.ILTyconRawMetadata.CustomAttrs with 
        | Some attr -> f1 attr
        | _ -> None
    else 
        match TryFindAttrib g args tcref.Attribs with 
        | Some attr -> f2 attr
        | _ -> None
      
let TyconRefHasAttrib g args tcref = 
    TyconRefTryBindAttrib g args tcref (fun _ -> Some()) (fun _ -> Some()) |> isSome 

/// Detect if a type is definitely known to be non-serializable
let isDefinitelyNotSerializable g typ =
    if isArrayTy g typ || isArrayTy g typ then
      true
    else
      match tryDestAppTy g typ with 
      | None -> false
      | Some tcref -> 
         if tcref.IsILTycon then 
             not tcref.ILTyconRawMetadata.IsSerializable
         else 
             (TryFindBoolAttrib g g.attrib_AutoSerializableAttribute tcref.Attribs = Some(false))

//-------------------------------------------------------------------------
// List and reference types...
//------------------------------------------------------------------------- 

let destByrefTy g ty   = if isByrefTy g ty then List.head (argsOfAppTy g ty) else failwith "destByrefTy: not a byref type"

let isRefCellTy g ty   = 
    match tryDestAppTy g ty with 
    | None -> false
    | Some tcref -> tyconRefEq g g.refcell_tcr_canon tcref

let destRefCellTy g ty = if isRefCellTy g ty then List.head (argsOfAppTy g ty) else failwith "destRefCellTy: not a ref type"

let StripSelfRefCell(g:TcGlobals,baseOrThisInfo:ValBaseOrThisInfo,tau: TType) : TType =
    if baseOrThisInfo = CtorThisVal && isRefCellTy g tau 
        then destRefCellTy g tau 
        else tau

let mkRefCellTy  g ty = TType_app(g.refcell_tcr_nice,[ty])

let mkLazyTy g ty = TType_app(g.lazy_tcr_nice,[ty])

let mkPrintfFormatTy g aty bty cty dty ety = TType_app(g.format_tcr, [aty;bty;cty;dty; ety])

let mkOptionTy g ty = TType_app (g.option_tcr_nice, [ty])

let mkListTy g ty = TType_app (g.list_tcr_nice, [ty])

let isOptionTy g ty = 
    match tryDestAppTy g ty with 
    | None -> false
    | Some tcref -> tyconRefEq g g.option_tcr_canon tcref

let tryDestOptionTy g ty = 
    match argsOfAppTy g ty with 
    | [ty1]  when isOptionTy g ty  -> Some ty1
    | _ -> None

let destOptionTy g ty = 
    match tryDestOptionTy g ty with 
    | Some ty -> ty
    | None -> failwith "destOptionTy: not an option type"

let mkNoneCase g = mkUnionCaseRef g.option_tcr_canon "None"
let mkSomeCase g = mkUnionCaseRef g.option_tcr_canon "Some"

type ValRef with 
    member vref.IsDispatchSlot = 
        match vref.MemberInfo with 
        | Some membInfo -> membInfo.MemberFlags.IsDispatchSlot 
        | None -> false

let (|UnopExpr|_|) _g expr = 
    match expr with 
    | Expr.App(Expr.Val(vref,_,_),_,_,[arg1],_) -> Some (vref, arg1)
    | _ -> None

let (|BinopExpr|_|) _g expr = 
    match expr with 
    | Expr.App(Expr.Val(vref,_,_),_,_,[arg1;arg2],_) -> Some (vref, arg1, arg2)
    | _ -> None

let SpecificUnopExpr  g vrefReqd expr = 
    match expr with 
    | UnopExpr g (vref, arg1) when valRefEq g vref vrefReqd  -> Some arg1
    | _ -> None

let SpecificBinopExpr g vrefReqd expr = 
    match expr with 
    | BinopExpr g (vref, arg1, arg2) when valRefEq g vref vrefReqd -> Some (arg1, arg2)
    | _ -> None

let (|EnumExpr|_|) g expr = SpecificUnopExpr g g.enum_vref  expr
let (|BitwiseOrExpr|_|) g expr = SpecificBinopExpr g g.bitwise_or_vref expr

let (|AttribBitwiseOrExpr|_|) g expr = 
    match expr with 
    | BitwiseOrExpr g (arg1, arg2) -> Some(arg1, arg2)
    // Special workaround, only used when compiling FSharp.Core.dll. Uses of 'a ||| b' occur before the '|||' bitwise or operator
    // is defined. These get through type checking because enums implicitly support the '|||' operator through
    // the automatic resolution of undefined operators (see tc.fs, Item.ImplicitOp). This then compiles as an 
    // application of a lambda to two arguments. We recognize this pattern here
    | Expr.App(Expr.Lambda _,_,_,[arg1;arg2],_) when g.compilingFslib  -> 
        Some(arg1, arg2)
    | _ -> None

let isUncheckedDefaultOfValRef g vref = 
    valRefEq g vref g.unchecked_defaultof_vref 
    // There is an internal version of typeof defined in prim-types.fs that needs to be detected
    || (g.compilingFslib && vref.LogicalName = "defaultof") 

let isTypeOfValRef g vref = 
    valRefEq g vref g.typeof_vref 
    // There is an internal version of typeof defined in prim-types.fs that needs to be detected
    || (g.compilingFslib && vref.LogicalName = "typeof") 

let isSizeOfValRef g vref = 
    valRefEq g vref g.sizeof_vref 
    // There is an internal version of typeof defined in prim-types.fs that needs to be detected
    || (g.compilingFslib && vref.LogicalName = "sizeof") 

let isTypeDefOfValRef g vref = 
    valRefEq g vref g.typedefof_vref 
    // There is an internal version of typedefof defined in prim-types.fs that needs to be detected
    || (g.compilingFslib && vref.LogicalName = "typedefof") 

let (|UncheckedDefaultOfExpr|_|) g expr = 
    match expr with 
    | Expr.App(Expr.Val(vref,_,_),_,[ty],[],_) when isUncheckedDefaultOfValRef g vref ->  Some ty
    | _ -> None

let (|TypeOfExpr|_|) g expr = 
    match expr with 
    | Expr.App(Expr.Val(vref,_,_),_,[ty],[],_) when isTypeOfValRef g vref ->  Some ty
    | _ -> None

let (|SizeOfExpr|_|) g expr = 
    match expr with 
    | Expr.App(Expr.Val(vref,_,_),_,[ty],[],_) when isSizeOfValRef g vref ->  Some ty
    | _ -> None

let (|TypeDefOfExpr|_|) g expr = 
    match expr with 
    | Expr.App(Expr.Val(vref,_,_),_,[ty],[],_) when isTypeDefOfValRef g vref ->  Some ty
    | _ -> None



//--------------------------------------------------------------------------
// Print Signatures/Types 
//-------------------------------------------------------------------------- 

module NicePrint = begin
    open Microsoft.FSharp.Compiler.Layout
    open Microsoft.FSharp.Compiler.PrettyNaming
    open PrettyTypes

    /// Paths are used in FSI generation and Abstract IL metadata printing
    ///
    /// paths in innermost-to-outermost order (e.g., `Foo.Bar.Baz` = ["Baz" ; "Bar" ; "Foo"])
    type Path = 
        | Path of string list 
        | NoPath 

        static member Empty = Path []

        member path.Add ident = 
            match path with
            | NoPath    -> NoPath
            | Path xs -> Path (ident :: xs)

    let fullySplitTypeRef (tref:ILTypeRef) = 
        (List.collect IL.splitNamespace (tref.Enclosing @ [IL.ungenericizeTypeName tref.Name])) 

    let commentL l = wordL "(*" ++ l ++ wordL "*)"
    let comment str = str |> wordL |> commentL

    let layoutsL (ls : layout list) : layout =
        match ls with
        | []      -> emptyL
        | [x]     -> x
        | x :: xs -> List.fold (^^) x xs 

    /// Place the mangled identifier's name at the 'end' of the path; this will make the fully-qualified name later
    let annotateWithPath ident path mangledIdent = 
        let mkPair s = (s, "")
        let node = wordL ident
        match path with
        | NoPath      -> node
        | Path path -> Internal.Utilities.StructuredFormat.Layout.Attr ("goto:path", mangledIdent :: path |> List.rev |> List.map mkPair , node)

    // shortcut to annote elements with a path extended by the identifier.
    let (&==) ident path = annotateWithPath ident path ident 


    module IlPrint = begin

        open Microsoft.FSharp.Compiler.AbstractIL.IL
        
        let private adjustIlNameInternal (path : Path)(n : string) : string list =

            let demangleFSharpBaseTypes n =
                match n with
                | "System.Void"    -> "unit"
                | "System.Object"  -> "obj"
                | "System.String"  -> "string"
                | "System.Single"  -> "float32"
                | "System.Double"  -> "float"
                | "System.Decimal"  -> "decimal"
                | "System.Char"    -> "char"
                | "System.Int16"   -> "int16"
                | "System.Int32"   -> "int" 
                | "System.Int64"   -> "int64" 
                | "System.UInt16"  -> "uint16" 
                | "System.UInt32"  -> "uint32" 
                | "System.UInt64"  -> "uint64" 
                | "System.Boolean" -> "bool"
                | _                -> n

            let dropCurrentNamespace n    = 
                match path with
                | NoPath    -> n
                | Path xs -> 
                    match n |> List.rev with
                    | (y :: _) as ys when ys = xs -> [ y ] // special case for (recursive) references to the type we're currently defining
                    | y :: ys when ys = xs        -> [ y ]
                    | _                           -> n
            n |> demangleFSharpBaseTypes |> SplitNamesForFsiGenerationPath |> ChopUnshowableInFsiGenerationPath |> dropCurrentNamespace |> List.map Lexhelp.Keywords.QuoteIdentifierIfNeeded

        /// fix up a name coming from IL metadata by:
        /// - translating well-known base types (e.g., System.Int32 -> int)
        /// - chopping off numeric (num-of-params) suffixes
        /// - hiding the current namespace (e.g., if we're in A.B.C.D and we're
        ///   showing A.B.C.D.E, we can abbreviate this to E)
        /// - quote "funny" names (keywords, otherwise invalid identifiers)
        let private adjustIlName (path : Path)(n : string) : string =
            n |> adjustIlNameInternal path |> JoinNamesForFsiGenerationPath

        /// this fixes up a name just like adjustIlName but also handles F#
        /// operators
        let private adjustIlMethodName (path : Path)(n : string) : string =
          let specialdemangleOperatorName s =
              if IsMangledOpName s
              then DemangleOperatorName s
              else s
          n |> adjustIlNameInternal path |> List.map specialdemangleOperatorName |> JoinNamesForFsiGenerationPath



        // our good Haskell friend
        let rec intersperse x lst =
            match lst with 
            | []      -> []
            | [y]     -> [y]
            | y :: ys -> y :: x :: intersperse x ys

        let private arrayShapeL (ILArrayShape sh : ILArrayShape) : layout = 
            leftL "[" ^^ wordL (sh |> List.tail |> List.map (fun _ -> ",") |> String.concat "") ^^ rightL "]" // drop off one "," so that a n-dimensional array has n - 1 ","'s

        let private  paramsList (ps : ILGenericParameterDefs) = 
            ps |> List.map (fun x -> "'" + x.Name |> wordL) 

        let private paramsL (ps : layout list) : layout = 
            match ps with
            | [] -> emptyL
            | _  -> 
                let body = ps |> intersperse (sepL ",") |> layoutsL
                sepL "<" ^^ body ^^ rightL ">"

        let private pruneParms (name : string)(parms : layout list) =
            let numParms = let rightMost = name |> SplitNamesForFsiGenerationPath |> List.rev |> List.head
                           try rightMost |> int // can't find a way to see the number of generic parameters for *this* class (the GenericParams also include type variables for enclosing classes); this will have to do
                               with _ -> 0 // looks like it's non-generic
            parms |> List.rev |> List.take numParms |> List.rev
                             
        let rec ilILTypeL (denv : DisplayEnv)(path : Path)(parms : layout list)(typ : ILType) : layout =
            match typ with
            | ILType.Void               -> wordL "unit" // These are type-theoretically totally different type-theoretically `void` is Fin 0 and `unit` is Fin (S 0) ... but, this looks like as close as we can get.
            | ILType.Array (sh, t)      -> ilILTypeL denv path parms t ^^ arrayShapeL sh
            | ILType.Value t
            | ILType.Boxed t            -> (adjustIlName path t.Name |> wordL) ^^ (t.GenericArgs |> List.map (ilILTypeL denv path parms) |> paramsL)
            | ILType.Ptr t
            | ILType.Byref t            -> ilILTypeL denv path parms t
            | ILType.FunctionPointer t             -> ilILCallingSignatureL denv path parms None t
            | ILType.TypeVar n            -> List.nth parms <| int n
            | ILType.Modified (_, _, t) -> ilILTypeL denv path parms t // "Just recurse through them to the contained ILType"--Don

        /// Layout a function's type signature. We need a special case for
        /// constructors (Their return types are reported as `void`, but this is
        /// incorrect; so if we're dealing with a constructor we require that the
        /// return type be passed along as the `cons` parameter.)
        and ilILCallingSignatureL (denv : DisplayEnv)(path : Path)(parms : layout list)(cons : string option)(signatur : ILCallingSignature) : layout =
            let args = signatur.ArgTypes |> List.map (ilILTypeL denv path parms) |> intersperse (wordL "*")
            let res  = 
                match cons with
                | Some className -> (adjustIlName path className |> wordL) ^^ (pruneParms className parms |> paramsL) // special case for constructor return-type (viz., the class itself)
                | None           -> signatur.ReturnType |> ilILTypeL denv path parms
            match args with
            | []      -> wordL "unit" ^^ wordL "->" ^^ res
            | [x]     -> x ^^ wordL "->" ^^ res
            | x :: xs -> (List.fold (^^) x xs) ^^ wordL "->" ^^ res

        /// Layout a method's signature. In the case that we've a constructor, we
        /// pull off the class name from the `path`; naturally, it's the
        /// most-deeply-nested element.

        let private ilMethodDefL (denv : DisplayEnv)(path : Path)(parms : layout list)(className : string)(m : ILMethodDef) : layout =
            let myParms         = m.GenericParams |> paramsList
            let parms           = parms @ myParms
            let name            = adjustIlMethodName path m.Name
            let (nameL, isCons) = 
                match () with
                | _ when m.IsConstructor -> ("new" &== path,                                                              Some className) // we need the unadjusted name here to be able to grab the number of generic parameters
                | _ when m.IsStatic      -> (wordL "static" ^^ wordL "member" ^^ (name &== path) ^^ (myParms |> paramsL), None)
                | _                      -> (wordL "member" ^^ (name &== path) ^^ (myParms |> paramsL),                   None)
            let signaturL       = m.CallingSignature |> ilILCallingSignatureL denv path parms isCons
            nameL ^^ wordL ":" ^^ signaturL

        let private ilFieldDefL (denv : DisplayEnv)(path : Path)(parms : layout list)(f : ILFieldDef) : layout =
            let staticL =  if f.IsStatic then wordL "static" else emptyL
            let name    = adjustIlName path f.Name
            let nameL   = name &== path
            let typL    = ilILTypeL denv path parms f.Type
            staticL ^^ wordL "val" ^^ nameL ^^ wordL ":" ^^ typL            
       
        let private ilPropertyDefL (denv : DisplayEnv)(path : Path)(parms : layout list)(p : ILPropertyDef) : layout =
            let staticL =  if p.CallingConv =  ILThisConvention.Static then wordL "static" else emptyL
            let name    = adjustIlName path p.Name
            let nameL   = name &== path
            
            let getterTypeL (getterRef:ILMethodRef) =
                match getterRef.ArgTypes with
                |   [] -> ilILTypeL denv path parms getterRef.ReturnType
                |   _ -> ilILCallingSignatureL denv path parms None getterRef.CallingSignature
                
            let setterTypeL (setterRef:ILMethodRef) =
                let argTypes = setterRef.ArgTypes
                if isNil argTypes then
                    emptyL // shouldn't happen
                else
                    let frontArgs, lastArg = List.frontAndBack argTypes
                    let argsL = frontArgs |> List.map (ilILTypeL denv path parms) |> intersperse (wordL "*") |> List.fold (^^) emptyL
                    argsL ^^ wordL "->" ^^ (ilILTypeL denv path parms lastArg)
            
            let typL = 
                match p.GetMethod, p.SetMethod with
                |   None, None -> ilILTypeL denv path parms p.Type // shouldn't happen
                |   Some getterRef, _ -> getterTypeL getterRef
                |   None, Some setterRef -> setterTypeL setterRef
                
            let specGetSetL =
                match p.GetMethod, p.SetMethod with
                |   None,None 
                |   Some _, None -> emptyL
                |   None, Some _ -> wordL "with" ^^ wordL "deb  set"
                |   Some _, Some _ -> wordL "with" ^^ wordL "get," ^^ wordL "set"
            staticL ^^ wordL "member" ^^ nameL ^^ wordL ":" ^^ typL ^^ specGetSetL

        let ilEnumFieldDefL (_denv : DisplayEnv) (path : Path) (f : ILFieldDef) : layout =
            let res   = 
                match f.LiteralValue with
                | Some init -> 
                    match init with
                    | ILFieldInit.Bool x   -> 
                        if x
                        then Some "true"
                        else Some "false"
                    | ILFieldInit.Char c   -> Some ("'" + (char c).ToString () + "'")
                    | ILFieldInit.Int16 x  -> Some ((x |> int32 |> string) + "s")
                    | ILFieldInit.Int32 x  -> Some (x |> string)
                    | ILFieldInit.Int64 x  -> Some ((x |> string) + "L")
                    | ILFieldInit.UInt16 x -> Some ((x |> int32 |> string) + "us")
                    | ILFieldInit.UInt32 x -> Some ((x |> int64 |> string) + "u")
                    | ILFieldInit.UInt64 x -> Some ((x |> int64 |> string) + "UL")
                    | ILFieldInit.Single d -> 
                        let s = d.ToString ("g12", System.Globalization.CultureInfo.InvariantCulture)
                        let s = 
                            if String.forall (fun c -> System.Char.IsDigit c || c = '-')  s 
                            then s + ".0" 
                            else s
                        Some (s + "f")
                    | ILFieldInit.Double d -> 
                         let s = d.ToString ("g12", System.Globalization.CultureInfo.InvariantCulture)
                         if String.forall (fun c -> System.Char.IsDigit c || c = '-')  s 
                         then Some (s + ".0")
                         else Some s
                    | _   -> None
                | None      -> None
            let initL = match res with
                        | None   -> wordL "=" ^^ (comment "value unavailable")
                        | Some s -> wordL "=" ^^ wordL s

            let name  = adjustIlName path f.Name
            let nameL = name &== path
            wordL "|" ^^ nameL ^^ initL

        // filtering methods for hiding things we oughtn't show
        let private isStaticProperty    (p : ILPropertyDef)      = match p.GetMethod,p.SetMethod with
                                                                   | Some getter,_    -> getter.CallingSignature.CallingConv.IsStatic
                                                                   | None,Some setter -> setter.CallingSignature.CallingConv.IsStatic
                                                                   | None,None        -> true

        let private isPublicMethod      (m : ILMethodDef) = 
            (m.Access = ILMemberAccess.Public)

        let private isPublicProperty typeDef (m : ILPropertyDef) = 
            try
                match m.GetMethod with 
                | Some ilMethRef -> isPublicMethod (resolveILMethodRef typeDef ilMethRef)
                | None -> 
                    match m.SetMethod with 
                    | None -> false
                    | Some ilMethRef -> isPublicMethod (resolveILMethodRef typeDef ilMethRef)
            // resolveILMethodRef is a possible point of failure if Abstract IL type equality checking fails 
            // to link the method ref to a method def for some reason, e.g. some feature of IL type
            // equality checking has not been implemented. Since this is just intellisense pretty printing code
            // it is better to swallow the exception here, though we don't know of any
            // specific cases where this happens
            with _ -> 
                false

        let private isPublicCtor        (m : ILMethodDef) = 
            (m.Access = ILMemberAccess.Public && m.IsConstructor)

        let private isNotSpecialName    (m : ILMethodDef) = 
            not m.IsSpecialName

        let private isPublicField       (f : ILFieldDef)  = 
            (f.Access = ILMemberAccess.Public)

        let private isPublicClass       (c : ILTypeDef)   : bool =
            match c.Access with
            | ILTypeDefAccess.Public
            | ILTypeDefAccess.Nested ILMemberAccess.Public -> true
            | _                                  -> false

        let private isShowEnumField (f : ILFieldDef) : bool = f.Name <> "value__" // this appears to be the hard-coded underlying storage field
        let private noShow = set [ "System.Object" ; "System.ValueType" ; "obj" ] // hide certain 'obvious' base classes
        let private isShowBase      (n : layout)   : bool = 
            not (noShow.Contains(showL n))

        let rec ilTypeDefL (denv : DisplayEnv)(path : Path)(typeDef : ILTypeDef) : layout =
            let path'       = path // assumption: path's already been augmented when handling the LHS
            let parms       = typeDef.GenericParams |> paramsList

            let baseNameL b = 
                match b with
                | Some b -> 
                    let baseName = ilILTypeL denv path parms b
                    if isShowBase baseName
                       then [ wordL "inherit" ^^ baseName ]
                       else []
                | None   -> []

            let enumsL (flds: ILFieldDefs) = 
                flds.AsList 
                |> List.filter isShowEnumField 
                |> List.map (ilEnumFieldDefL denv path')

            let nestedTypesL  (tdefs: ILTypeDefs)      = 
                tdefs.AsList 
                |> List.filter isPublicClass   
                |> List.map (ilNestedClassDefL denv path')

            let renderL pre body post = 
                match pre with
                | Some pre -> 
                    match body with
                    | [] -> emptyL // empty type
                    | _  -> (pre @@-- aboveListL body) @@ post
                | None     -> 
                    aboveListL body

            let classL (typeDef : ILTypeDef) typeWord = 
                let pre    = Some (wordL typeWord)
                let baseT  = baseNameL typeDef.Extends

                let memberBlockLs (fieldDefs:ILFieldDefs, methodDefs:ILMethodDefs, propertyDefs:ILPropertyDefs) =
                    let ctors  =
                        methodDefs.AsList 
                        |> List.filter isPublicCtor 
                        |> List.map (ilMethodDefL denv path' parms typeDef.Name)

                    let fields = 
                        fieldDefs.AsList
                        |> List.filter isPublicField   
                        |> List.map (ilFieldDefL denv path' parms)

                    let props = 
                        propertyDefs.AsList 
                        |> List.filter (isPublicProperty typeDef)
                        |> List.map (fun pd -> (pd.Name, pd.Args.Length), ilPropertyDefL denv path' parms pd)

                    let meths = 
                        methodDefs.AsList
                        |> List.filter isPublicMethod 
                        |> List.filter isNotSpecialName 
                        |> List.map (fun md -> (md.Name, md.Parameters.Length) ,ilMethodDefL denv path' parms typeDef.Name md)

                    let members = 
                        (props @ meths) 
                        |> List.sortBy fst 
                        |> List.map snd // (properties and members) are sorted by name/arity 

                    ctors @ fields @ members

                let bodyStatic   = 
                    memberBlockLs (typeDef.Fields.AsList |> List.filter (fun fd -> fd.IsStatic)                |> mkILFields,
                                   typeDef.Methods.AsList |> List.filter (fun md -> md.IsStatic)                |> mkILMethods,
                                   typeDef.Properties.AsList |> List.filter (fun pd -> isStaticProperty pd)        |> mkILProperties)

                let bodyInstance = 
                    memberBlockLs (typeDef.Fields.AsList |> List.filter (fun fd -> fd.IsStatic         |> not) |> mkILFields,
                                   typeDef.Methods.AsList |> List.filter (fun md -> md.IsStatic         |> not) |> mkILMethods,
                                   typeDef.Properties.AsList |> List.filter (fun pd -> isStaticProperty pd |> not) |> mkILProperties)
  
                let body = bodyInstance @ bodyStatic // instance "member" before "static member" 
  
                let types  = nestedTypesL typeDef.NestedTypes
  
                let post   = wordL "end"
                renderL pre (baseT @ body @ types) post

            let delegateL (typeDef : ILTypeDef) = 
                let rhs = 
                    match typeDef.Methods.AsList |> List.filter (fun m -> m.Name = "Invoke") with // the delegate delegates to the type of `Invoke`
                    | m :: _ -> ilILCallingSignatureL denv path parms None m.CallingSignature
                    | _      -> comment "`Invoke` method could not be found"
                wordL "delegate" ^^ wordL "of" ^^ rhs

            match typeDef.tdKind with
            | ILTypeDefKind.Class     -> classL typeDef "class"
            | ILTypeDefKind.ValueType -> classL typeDef "struct"
            | ILTypeDefKind.Interface -> classL typeDef "interface"
            | ILTypeDefKind.Enum      -> renderL None (enumsL typeDef.Fields) emptyL
            | ILTypeDefKind.Delegate  -> delegateL typeDef
            | ILTypeDefKind.Other _   -> comment "cannot show type"
          
        and ilNestedClassDefL (denv : DisplayEnv)(path : Path)(typeDef : ILTypeDef) : layout =
            let name     = adjustIlName path typeDef.Name
            let nameL    = name &== path
            let parms    = typeDef.GenericParams |> paramsList
            let paramsL  = pruneParms typeDef.Name parms |> paramsL
            let path'    = path.Add name
            let pre      = wordL "type" ^^ nameL ^^ paramsL
            let body     = ilTypeDefL denv path' typeDef
            (pre ^^ wordL "=") @@-- body
            //if body = emptyL
            //   then pre
            //   else (pre ^^ wordL "=") @@-- body

    end

    // Note: We need nice printing of constants in order to print literals and attributes 
    let constL c =
        let str = 
            match c with
            | Const.Bool x        -> if x then "true" else "false"
            | Const.SByte x       -> (x |> string)+"y"
            | Const.Byte x        -> (x |> string)+"uy"
            | Const.Int16 x       -> (x |> string)+"s"
            | Const.UInt16 x      -> (x |> string)+"us"
            | Const.Int32 x       -> (x |> string)
            | Const.UInt32 x      -> (x |> string)+"u"
            | Const.Int64 x       -> (x |> string)+"L"
            | Const.UInt64 x      -> (x |> string)+"UL"
            | Const.IntPtr x      -> (x |> string)+"n"
            | Const.UIntPtr x     -> (x |> string)+"un"
            | Const.Single d      -> 
                (let s = d.ToString("g12",System.Globalization.CultureInfo.InvariantCulture)
                 if String.forall (fun c -> System.Char.IsDigit(c) || c = '-')  s 
                 then s + ".0" 
                 else s) + "f"
            | Const.Double d      -> 
                let s = d.ToString("g12",System.Globalization.CultureInfo.InvariantCulture)
                if String.forall (fun c -> System.Char.IsDigit(c) || c = '-')  s 
                then s + ".0" 
                else s
            | Const.Char c        -> "'" + c.ToString() + "'" 
            | Const.String bs     -> "\"" + bs + "\"" 
            | Const.Unit          -> "()" 
            | Const.Decimal bs    -> string bs + "M" 
            | Const.Zero       -> "default"
        wordL str

    let bracketIfL x lyt = if x then bracketL lyt else lyt
    let hlinkL (url:string) l = linkL url l
    let squareAngleL x = leftL "[<" ^^ x ^^ rightL ">]"
    let angleL x = sepL "<" ^^ x ^^ rightL ">"  
    let braceL x = leftL "{" ^^ x ^^ rightL "}"  
    let boolL = function true -> wordL "true" | false -> wordL "false"

    let accessibilityL (denv:DisplayEnv) accessibility itemL =   
        let isInternalCompPath x = 
            match x with 
            | CompPath(ILScopeRef.Local,[]) -> true 
            | _ -> false
        let (|Public|Internal|Private|) (TAccess p) = 
            match p with 
            | [] -> Public 
            | _ when List.forall isInternalCompPath p  -> Internal 
            | _ -> Private
        match denv.contextAccessibility,accessibility with
        | Public,Internal  -> wordL "internal" ++ itemL    // print modifier, since more specific than context
        | Public,Private   -> wordL "private" ++ itemL     // print modifier, since more specific than context
        | Internal,Private -> wordL "private" ++ itemL     // print modifier, since more specific than context
        | _ -> itemL

    let trefL denv tref =
        let path = fullySplitTypeRef tref
        let p2,n = List.frontAndBack path
        leftL (trimPathByDisplayEnv denv p2) ^^ wordL n
        
    /// Layout a reference to a type or value, perhaps emitting a HTML hyperlink *)
    let tcrefL denv (tcref:TyconRef) = 
        let nm = tcref.LogicalName
        let demangled = DemangleGenericTypeName nm
        let tyconTextL = wordL demangled
        let path = demangledPathOfCompPath tcref.CompilationPath
        match tcref with 
        | ERefLocal _ -> 
                let pathText = trimPathByDisplayEnv denv path
                (if pathText = "" then tyconTextL else leftL pathText ^^ tyconTextL)
        | ERefNonLocal _ -> 
              let pathText = trimPathByDisplayEnv denv path
              (if pathText = "" then tyconTextL else leftL pathText ^^ tyconTextL)

    /// Layout the flags of a member *)
    let memFlagsL memFlags = 
        let stat = if memFlags.IsInstance || (memFlags.MemberKind = MemberKind.Constructor) then emptyL else wordL "static"
        let stat = if memFlags.IsDispatchSlot then stat ++ wordL "abstract" 
                   elif memFlags.IsOverrideOrExplicitImpl then stat ++ wordL "override" 
                   else stat
        let stat = 
        
            if memFlags.IsOverrideOrExplicitImpl then stat 
            else  
              match memFlags.MemberKind with 
              | MemberKind.ClassConstructor  
              | MemberKind.Constructor 
              | MemberKind.PropertyGetSet -> stat
              | MemberKind.Member 
              | MemberKind.PropertyGet 
              | MemberKind.PropertySet -> stat ++ wordL "member"

        // let stat = if memFlags.IsFinal then stat ++ wordL "final" else stat in  
        stat

    /// Layout a single attibute arg, following the cases of 'gen_attr_arg' in ilxgen.fs
    /// This is the subset of expressions we display in the NicePrint pretty printer 
    /// See also dataExprL - there is overlap between these that should be removed 
    let rec attribArgL denv arg = 
        match arg with 
        | Expr.Const(c,_,ty) -> 
            if isEnumTy denv.g ty then 
                wordL "enum" ^^ angleL (typeL denv ty) ^^ bracketL (constL c)
            else
                constL c

        | Expr.Op (TOp.Array,[_elemTy],args,_) ->
            leftL "[|" ^^ semiListL (List.map (attribArgL denv) args) ^^ rightL "|]"

        // Detect 'typeof<ty>' calls 
        | TypeOfExpr denv.g ty ->
            leftL "typeof<" ^^ typeL denv ty ^^ rightL ">"

        // Detect 'typedefof<ty>' calls 
        | TypeDefOfExpr denv.g ty ->
            leftL "typedefof<" ^^ typeL denv ty ^^ rightL ">"

        | Expr.Op (TOp.Coerce,[tgTy;_],[arg2],_) ->
            leftL "(" ^^ attribArgL denv arg2 ^^ wordL ":>" ^^ typeL denv tgTy ^^ rightL ")"

        | AttribBitwiseOrExpr denv.g (arg1, arg2) ->
            attribArgL denv arg1 ^^ wordL "|||" ^^ attribArgL denv arg2

        // Detect explicit enum values 
        | EnumExpr denv.g arg1 ->
            wordL "enum" ++ bracketL (attribArgL denv arg1)


        | _ -> wordL "(* unsupported attribute argument *)"

    /// Layout arguments of an attribute 'arg1, ..., argN' 
    and attribArgsL denv args = 
        sepListL (rightL ",") (List.map (fun (AttribExpr(e1,_)) -> attribArgL denv e1) args)

    /// Layout an attribute 'Type(arg1, ..., argN)' 
    and attribL denv (Attrib(_,k,args,_props,_,_)) = 
        let argsL = bracketL (attribArgsL denv args)
        match k with 
        | (ILAttrib(ilMethRef)) -> 
            let trimmedName = 
                let name =  ilMethRef.EnclosingTypeRef.Name
                match String.tryDropSuffix name "Attribute" with 
                | Some shortName -> shortName
                | None -> name
            let tref = ilMethRef.EnclosingTypeRef
            let tref = ILTypeRef.Create(scope= tref.Scope, enclosing=tref.Enclosing, name=trimmedName)
            trefL denv tref ++ argsL

        | (FSAttrib(vref)) -> 
            let rty = GetReturnTypeOMemberInMemberForm denv.g vref
            let rty = GetFSharpViewOfReturnType denv.g rty
            let tcref = tcrefOfAppTy denv.g rty
            tcrefL denv tcref ++ argsL


    /// Layout '[<attribs>]' above another block 
    and attribsL denv kind attrs restL = 
        
        if denv.showAttributes then
            // Don't display DllImport attributes in generated signatures  
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_DllImportAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_ContextStaticAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_ThreadStaticAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_EntryPointAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_MarshalAsAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_ReflectedDefinitionAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_StructLayoutAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingAttrib denv.g denv.g.attrib_AutoSerializableAttribute >> not)
            match attrs with
            | [] -> restL 
            | _  -> squareAngleL (sepListL (rightL ";") (List.map (attribL denv) attrs)) @@ 
                    restL
        else 
        match kind with 
        | KindType -> restL
        | KindMeasure -> squareAngleL (wordL "Measure") @@ restL

    and typarAttribsL denv kind attrs restL =         
            match attrs, kind with
            | [], KindType -> restL 
            | _, _  -> squareAngleL (sepListL (rightL ";") ((match kind with KindType -> [] | KindMeasure -> [wordL "Measure"]) @ List.map (attribL denv) attrs)) ^^ restL

    // NOTE: The primed' functions take an "env" - determines inplace printing of typar and constraints
    // NOTE: "denv" is the DisplayEnv - "env" is the TypeSimplificationInfo 

    /// Layout type parameters, taking TypeSimplificationInfo into account  *
    and typarDeclsL' denv env nmL prefix (typars: Typars) =
        let tpcs = typars |> List.collect (fun tp -> List.map (fun tpc -> tp,tpc) tp.Constraints) 
        match typars,tpcs with 
        | [],[]  -> 
            nmL

        | [h],[] when not prefix -> 
            typarL' denv env h --- nmL

        | _ -> 
            let tpcsL = constraintsL denv env tpcs
            let coreL = sepListL (sepL ",") (List.map (typarL' denv env) typars)
            (if prefix || nonNil(tpcs) then nmL ^^ angleL (coreL --- tpcsL) else bracketL coreL --- nmL)

    and typarOccL' denv _env (typar:Typar) =
          wordL (sprintf "%s%s%s"
                   (if denv.showConstraintTyparAnnotations then prefixOfStaticReq typar.StaticReq else "'")
                   (if denv.showImperativeTyparAnnotations then prefixOfRigidTypar typar else "")
                   typar.DisplayName)

    /// Layout a single type parameter declaration, taking TypeSimplificationInfo into account  *)
    /// There are several printing-cases for a typar:
    ///
    ///  'a              - is multiple  occurance.
    ///  _               - singleton occurrence, an underscore prefered over 'b. (OCAML accepts but does not print)
    ///  #Type           - inplace coercion constraint and singleton.
    ///  ('a :> Type)    - inplace coercion constraint not singleton.
    ///  ('a.opM : S->T) - inplace operator constraint.
    ///
    and typarL' denv (env:SimplifyTypes.TypeSimplificationInfo) (typar:Typar) =
        let varL = typarOccL' denv env typar
        let varL = if denv.showAttributes then typarAttribsL denv typar.Kind typar.Attribs varL else varL

        match Zmap.tryFind typar env.inplaceConstraints with
        | Some (typarConstrTyp) ->
            if Zset.contains typar env.singletons then
                leftL "#" ^^ typarSubtypeConstraintL denv env typarConstrTyp
            else
                (varL ^^ sepL ":>" ^^ typarSubtypeConstraintL denv env typarConstrTyp) |> bracketL

        | _ -> varL

      
    /// Layout type parameter constraints, taking TypeSimplificationInfo into account  *)
    and constraintsL denv env cxs = 
        
        
        // Internally member constraints get attached to each type variable in their support. 
        // This means we get too many constraints being printed. 
        // So we normalize the constraints to eliminate duplicate member constraints 
        let cxs = 
            cxs  
            |> ListSet.setify (fun (_,cx1) (_,cx2) ->
                     match cx1,cx2 with 
                     | TTyparMayResolveMemberConstraint(traitInfo1,_),
                       TTyparMayResolveMemberConstraint(traitInfo2,_) -> traitsAEquiv denv.g TypeEquivEnv.Empty traitInfo1 traitInfo2
                     | _ -> false)
                     
        let cxsL = List.collect (constraintL' denv env) cxs
        match cxsL with 
        | [] -> emptyL 
        | _ -> 
            if denv.abbreviateAdditionalConstraints then 
                wordL "when <constraints>"
            elif denv.shortConstraints then 
                leftL "(" ^^ wordL "requires" ^^ sepListL (wordL "and") cxsL ^^ rightL ")"
            else
                wordL "when" ^^ sepListL (wordL "and") cxsL

    /// Layout constraints, taking TypeSimplificationInfo into account  *)
    and constraintL' denv env (tp,tpc) =
        let longConstraintPrefix l = typarL' denv env tp ^^ wordL ":" ^^ l
        match tpc with 
        | TTyparCoercesToType(tpct,_) -> 
            [typarL' denv env tp ^^ wordL ":>" --- typarSubtypeConstraintL denv env tpct]
        | TTyparMayResolveMemberConstraint(traitInfo,_) ->
            [traitL denv env traitInfo]
        | TTyparDefaultsToType(_,ty,_) ->
             if denv.showTyparDefaultConstraints then [wordL "default" ^^ typarL' denv env tp ^^ wordL " :" ^^ typeL' denv env ty]
             else []
        | TTyparIsEnum(ty,_) ->
            if denv.shortConstraints then 
               [wordL "enum"]
            else
               [longConstraintPrefix (tyappL denv env (wordL "enum") 2 true [ty])]
        | TTyparSupportsComparison _ ->
            if denv.shortConstraints then 
               [wordL "comparison"]
            else
               [wordL "comparison" |> longConstraintPrefix]
        | TTyparSupportsEquality _ ->
            if denv.shortConstraints then 
               [wordL "equality"]
            else
               [wordL "equality"  |> longConstraintPrefix]
        | TTyparIsDelegate(aty,bty,_) ->
            if denv.shortConstraints then 
               [wordL "delegate"]
            else
               [tyappL denv env (wordL "delegate") 2 true [aty;bty] |> longConstraintPrefix]
        | TTyparSupportsNull _ ->
           [wordL "null" |> longConstraintPrefix]
        | TTyparIsNotNullableValueType _ ->
            if denv.shortConstraints then 
               [wordL "value type"]
            else
               [wordL "struct" |> longConstraintPrefix]
        | TTyparIsUnmanaged _ ->
            if denv.shortConstraints then
                [wordL "unmanaged"]
            else
                [wordL "unmanaged" |> longConstraintPrefix]
        | TTyparIsReferenceType _ ->
            if denv.shortConstraints then 
               [wordL "reference type"]
            else
               [wordL "not struct" |> longConstraintPrefix]
        | TTyparSimpleChoice(tys,_) ->
           [bracketL (sepListL (sepL "|") (List.map (typeL' denv env) tys)) |> longConstraintPrefix]
        | TTyparRequiresDefaultConstructor _ -> 
            if denv.shortConstraints then 
               [wordL "default constructor"]
            else
               [bracketL (wordL "new : unit -> " ^^ (typarL' denv env tp)) |> longConstraintPrefix]

    /// Layout a subtype constraint *)
    and typarSubtypeConstraintL denv env ty = typeL' denv env ty

    and traitL denv env (TTrait(tys,nm,memFlags,argtys,rty,_)) =
        let nm = DemangleOperatorName nm
        if denv.shortConstraints then 
            wordL ("member "^nm)
        else
            let rty = GetFSharpViewOfReturnType denv.g rty
            let stat = memFlagsL memFlags
            let tys = ListSet.setify (typeEquiv denv.g) tys
            let tysL = 
                match tys with 
                | [ty] -> typeL' denv env ty 
                | tys -> bracketL (typesWithPrecL denv env 2 (wordL "or") tys)
            tysL ^^ wordL ":"  ---  
                bracketL (stat ++ wordL nm ^^ wordL ":" ---
                        ((typesWithPrecL denv env 2 (wordL "*") argtys --- wordL "->") --- (typeL' denv env rty)))


    /// Layout a unit expression *)
    and auxMeasureL' denv env _prec unt =
        let sortVars (vs:(Typar * int) list) = vs |> List.sortBy (fun (v,_) -> v.DisplayName) 
        let sortCons (cs:(TyconRef * int) list) = cs |> List.sortBy (fun (c,_) -> c.DisplayName) 
        let negvs,posvs = ListMeasureVarOccsWithNonZeroExponents              unt |> sortVars |> List.partition (fun (_,e) -> e<0)
        let negcs,poscs = ListMeasureConOccsWithNonZeroExponents denv.g false unt |> sortCons |> List.partition (fun (_,e) -> e<0)
        let unparL uv = typarOccL' denv env uv
        let unconL tc = tcrefL denv tc
        let prefix = spaceListL  (List.map (fun (v,e) -> if e=1  then unparL v else unparL v -- wordL (sprintf "^ %d" e)) posvs @
                                  List.map (fun (c,e) -> if e=1  then unconL c else unconL c -- wordL (sprintf "^ %d" e)) poscs)
        let postfix = spaceListL (List.map (fun (v,e) -> if e= -1 then unparL v else unparL v -- wordL (sprintf "^ %d" (-e))) negvs @
                                  List.map (fun (c,e) -> if e= -1 then unconL c else unconL c -- wordL (sprintf "^ %d" (-e))) negcs)
        match (negvs,negcs) with 
        | [],[] -> (match posvs,poscs with [],[] -> wordL "1" | _ -> prefix)
        | _ -> prefix ^^ sepL "/" ^^ (if List.length negvs + List.length negcs > 1 then sepL "(" ^^ postfix ^^ sepL ")" else postfix)

    /// Layout type arguments, either NAME<ty,...,ty> or (ty,...,ty) NAME *)
    and tyappL denv env tcL prec prefix args =
        if prefix  then 
            match args with
            | [] -> tcL
            | [arg] -> tcL ^^ sepL "<" ^^ (typeWithPrecL denv env 4 arg) ^^ rightL ">"
            | args -> bracketIfL (prec <= 1) (tcL ^^ angleL (typesWithPrecL denv env 2 (sepL ",") args))
        else
            match args with
            | []    -> tcL
            | [arg] ->  typeWithPrecL denv env 2 arg ^^ tcL
            | args  -> bracketIfL (prec <= 1) (bracketL (typesWithPrecL denv env 2 (sepL ",") args) --- tcL)

    /// Layout a type, taking precedence into account to insert brackets where needed *)
    and typeWithPrecL denv env prec typ =

        match stripTyparEqns typ with 

        // Layout a type application 
        | TType_app (tc,args) when tc.IsMeasureableReprTycon && List.forall (isDimensionless denv.g) args ->
          typeWithPrecL denv env prec (reduceTyconRefMeasureable tc args)

        | TType_app (tc,args) -> 
          tyappL denv env (tcrefL denv tc) prec tc.IsPrefixDisplay args 

        | TType_ucase (UCRef(tc,_),args) -> 
          tyappL denv env (tcrefL denv tc) prec tc.IsPrefixDisplay args 

        // Layout a tuple type 
        | TType_tuple t ->
            bracketIfL (prec <= 2) (typesWithPrecL denv env 2 (wordL "*") t)

        // Layout a first-class generic type. 
        | TType_forall (tps,tau) ->
            let tauL = typeWithPrecL denv env prec tau
            match tps with 
            | []  -> tauL
            | [h] -> typarL' denv env h ^^ rightL "." --- tauL
            | (h::t) -> spaceListL (List.map (typarL' denv env) (h::t)) ^^ rightL "." --- tauL

        // Layout a function type. 
        | TType_fun _ ->
            let rec loop soFarL ty = 
              match stripTyparEqns ty with 
              | TType_fun (dty,rty) -> loop (soFarL --- (typeWithPrecL denv env 4 dty ^^ wordL "->")) rty
              | rty -> soFarL --- typeWithPrecL denv env 5 rty
            bracketIfL (prec <= 4) (loop emptyL typ)

        // Layout a type variable . 
        | TType_var r ->
            typarL' denv env r

        | TType_measure unt -> auxMeasureL' denv env 4 unt

    /// Layout a list of types, separated with the given separator, either '*' or ',' *)
    and typesWithPrecL denv env prec sep typl = 
        sepListL sep (List.map (typeWithPrecL denv env prec) typl)

    and measureL' denv env unt = auxMeasureL' denv env 5 unt

    /// Layout a single type, taking TypeSimplificationInfo into account *)
    and typeL' denv env typ = 
        typeWithPrecL denv env 5 typ

    and typeL denv typ  = 
        typeL' denv SimplifyTypes.typeSimplificationInfo0 typ

    /// Layout a single type used as the type of a member or value 
    let topTypeL denv env argInfos rty cxs =
            // Parenthesize the return type to match the topValInfo 
            let rtyL  = typeWithPrecL denv env 4 rty
            let cxsL = constraintsL denv env cxs
            match argInfos with
            | [] -> rtyL --- cxsL
            | _  -> 

               // Format each argument, including its name and type 
               let argL (ty,argInfo: ArgReprInfo) = 
                   
                   // Detect an optional argument 
                   let isOptionalArg = HasAttrib denv.g denv.g.attrib_OptionalArgumentAttribute argInfo.Attribs
                   match argInfo.Name, isOptionalArg, tryDestOptionTy denv.g ty with 
                   // Layout an optional argument 
                   | Some(id), true, Some ty -> 
                       leftL ("?"^id.idText) ^^ sepL ":" ^^ typeWithPrecL denv env 2 ty 
                   // Layout an unnamed argument 
                   | None, _,_ -> 
                       typeWithPrecL denv env 2 ty
                   // Layout a named argument 
                   | Some id,_,_ -> 
                        leftL id.idText ^^ sepL ":" ^^ typeWithPrecL denv env 2 ty
                        
               let delimitReturnValue = if denv.useColonForReturnType then ":" else "->"

               let allArgsL = 
                   argInfos 
                   |> List.mapSquared argL 
                   |> List.map (sepListL (wordL "*"))
                   |> List.map (fun x -> (x ^^ wordL delimitReturnValue)) 
               (List.foldBack (---) allArgsL rtyL) --- cxsL

    let typarDeclsL denv nmL prefix typars = 
        typarDeclsL' denv SimplifyTypes.typeSimplificationInfo0 nmL prefix typars 

    let measureL denv unt  = 
        measureL' denv SimplifyTypes.typeSimplificationInfo0 unt

    let constraintL denv typars = 
        match constraintL' denv SimplifyTypes.typeSimplificationInfo0 typars  with 
        | h::_ -> h 
        | [] -> emptyL

    let typesAndConstraintsL denv taus =
        let _,ptaus,cxs = PrettifyTypesN denv.g taus
        let env = SimplifyTypes.CollectInfo true ptaus cxs
        List.map (typeL' denv env) ptaus,constraintsL denv env env.postfixConstraints

    let topPrettifiedTypesAndConstraintsL denv argInfos tau cxs = 
        let env = SimplifyTypes.CollectInfo true (tau:: List.collect (List.map fst) argInfos) cxs
        topTypeL denv env argInfos tau env.postfixConstraints

    let topTypAndConstraintsL denv argInfos tau = 
        let _,(argInfos,tau),cxs = PrettifyTypesN1 denv.g (argInfos,tau)
        topPrettifiedTypesAndConstraintsL denv [argInfos] tau cxs

    let memberTypeAndConstraintsL denv argInfos retTy parentTyparTys = 
        let _,(parentTyparTys,argInfos,retTy),cxs = PrettifyTypesNM1 denv.g (parentTyparTys,argInfos,retTy)
        // Filter out the parent typars, which don't get shown in the member signature 
        let cxs = cxs |> List.filter (fun (tp,_) -> not (parentTyparTys |> List.exists (fun ty -> isTyparTy denv.g ty && typarEq tp (destTyparTy denv.g ty)))) 
        topPrettifiedTypesAndConstraintsL denv argInfos retTy cxs

    // Layout: type spec - class, datatype, record, abbrev 

    let memberTypeCoreL denv memberToParentInst (methTypars: Typars,argInfos,retTy) = 
       let niceMethodTypars, allTyparInst = 
           let methTyparNames = methTypars |> List.mapi (fun i tp -> if (NeedsPrettyTyparName tp) then sprintf "a%d" (List.length memberToParentInst + i) else tp.Name)
           newPrettyTypars memberToParentInst methTypars methTyparNames

       let retTy = instType allTyparInst retTy
       let argInfos = argInfos |> List.map (fun infos -> if isNil infos then [(denv.g.unit_ty,ValReprInfo.unnamedTopArg1)] else infos |> List.map (map1Of2 (instType allTyparInst))) 

       // Also format dummy types corresponding to any type variables on the container to make sure they 
       // aren't chosen as names for displayed variables. 
       let memberParentTypars = List.map fst memberToParentInst
       let parentTyparTys = List.map (mkTyparTy >> instType allTyparInst) memberParentTypars

       niceMethodTypars,memberTypeAndConstraintsL denv argInfos retTy parentTyparTys

    let memberTypeL denv v _tps argInfos retTy = 
        match PartitionValTypars denv.g v with
        | Some(_,_,memberMethodTypars,memberToParentInst,_) ->  
           memberTypeCoreL denv memberToParentInst (memberMethodTypars, argInfos, retTy)
        | None -> 
           [],topTypAndConstraintsL denv (List.concat argInfos) retTy 

    let memberSigL denv  (memberToParentInst,nm,methTypars,argInfos,retTy) = 
        let niceMethodTypars,tauL = memberTypeCoreL denv memberToParentInst (methTypars, argInfos, retTy)
        let nameL = 
            let nameL = wordL (DemangleOperatorName nm)
            let nameL = if denv.showTyparBinding then typarDeclsL denv nameL true niceMethodTypars else nameL
            nameL
        nameL ^^ wordL ":" ^^ tauL


    let memberLP denv (path : Path) (v:Val) = 
        let membInfo = the v.MemberInfo
        let topValInfo = the v.ValReprInfo
        let id = v.Id
        let ty = v.Type
        let stat = memFlagsL membInfo.MemberFlags
        let tps,argInfos,rty,_ = GetMemberTypeInFSharpForm denv.g membInfo.MemberFlags topValInfo ty v.Range
        let mkNameL niceMethodTypars name =       
            let name  = DemangleOperatorName name
            let nameL = name &== path
            let nameL = 
                if denv.showMemberContainers then 
                    tcrefL denv v.MemberApparentParent ^^ sepL "." ^^ nameL
                else 
                    nameL
            let nameL = if denv.showTyparBinding then typarDeclsL denv nameL true niceMethodTypars else nameL
            let nameL = accessibilityL denv v.Accessibility nameL
            nameL

        match membInfo.MemberFlags.MemberKind with 
        | MemberKind.Member -> 
            let niceMethodTypars,tauL = memberTypeL denv v tps argInfos rty
            let nameL = mkNameL niceMethodTypars v.LogicalName
            stat --- (nameL ^^ wordL ":" ^^ tauL)
        | MemberKind.ClassConstructor  
        | MemberKind.Constructor -> 
            let _,tauL = memberTypeL denv v tps argInfos rty
            let newL = accessibilityL denv v.Accessibility (wordL "new")
            stat ++ newL ^^ wordL ":" ^^ tauL
        | MemberKind.PropertyGetSet -> stat
        | MemberKind.PropertyGet -> 
            if isNil argInfos then 
                // use error recovery because intellisense on an incomplete file will show this
                errorR(Error(FSComp.SR.tastInvalidFormForPropertyGetter(),id.idRange));
                stat --- wordL v.PropertyName --- wordL "with get"
            else
                let argInfos = 
                    match argInfos with 
                    | [[(ty,_)]] when isUnitTy denv.g ty -> []
                    | _ -> argInfos

                let niceMethodTypars,tauL = memberTypeL denv v tps argInfos rty
                let nameL = mkNameL niceMethodTypars v.PropertyName
                stat --- (nameL ^^ wordL ":" ^^ (if isNil argInfos then tauL else tauL --- wordL "with get"))
        | MemberKind.PropertySet -> 
            if argInfos.Length <> 1 || isNil argInfos.Head then 
                // use error recovery because intellisense on an incomplete file will show this
                errorR(Error(FSComp.SR.tastInvalidFormForPropertySetter(),id.idRange));
                stat --- wordL v.PropertyName --- wordL "with set"
            else 
                let argInfos,valueInfo = List.frontAndBack argInfos.Head
                let niceMethodTypars,tauL = memberTypeL denv v tps (if isNil argInfos then [] else [argInfos]) (fst valueInfo)
                let nameL = mkNameL niceMethodTypars v.PropertyName
                stat --- (nameL ^^ wordL ":" ^^ (tauL --- wordL "with set"))

    let nonMemberValSpecLP denv (path : Path) (tps,v:Val,tau,cxs) =
        let env = SimplifyTypes.CollectInfo true [tau] cxs
        let cxs = env.postfixConstraints
        let argInfos,rty = GetTopTauTypeInFSharpForm denv.g (arityOfVal v).ArgInfos tau v.Range
        // Drop the names from value arguments when printing them 
        let argInfos = argInfos |> List.mapSquared (fun (ty,_) -> ty,ValReprInfo.unnamedTopArg1)
        let nameL = v.DisplayName &== path
        let nameL = accessibilityL denv v.Accessibility nameL
        let nameL = 
            if v.IsMutable && not denv.suppressMutableKeyword then 
                wordL "mutable" ++ nameL 
             else 
                 nameL
        let nameL = 
            if v.MustInline && not denv.suppressInlineKeyword then 
                wordL "inline" ++ nameL 
            else 
                nameL

        let isOverGeneric = List.length (Zset.elements (freeInType CollectTyparsNoCaching tau).FreeTypars) < List.length tps // Bug: 1143 
        let isTyFunction  = v.IsTypeFunction     // Bug: 1143, and innerpoly tests 
        let typarBindingsL = 
            if isTyFunction || isOverGeneric || denv.showTyparBinding then 
                typarDeclsL denv nameL true tps 
            else nameL
        let valAndTypeL = (wordL "val"  ^^ typarBindingsL --- wordL ":") --- topTypeL denv env argInfos rty cxs
        match denv.generatedValueLayout v with
          | None      -> valAndTypeL
          | Some rhsL -> (valAndTypeL ++ wordL "=") --- rhsL

    let valLP denv (path : Path) (v:Val) =
        let vL = 
            match v.MemberInfo with 
            | None -> 
                let tps,tau = v.TypeScheme

                // adjust the type in case this is the 'this' pointer stored in a reference cell
                let tau = StripSelfRefCell(denv.g, v.BaseOrThisInfo, tau)

                let tprenaming,ptau,cxs = PrettyTypes.PrettifyTypes1 denv.g tau
                let ptps = 
                    tps  
                       |> generalizeTypars 
                       // Badly formed code may instantiate rigid declared typars to types, e.g. see bug
                       // Hence we double check here that the thing is really a type variable
                       |> List.map (instType tprenaming)
                       |> List.filter (isAnyParTy denv.g) 
                       |> List.map (destAnyParTy denv.g)
                nonMemberValSpecLP denv path (ptps,v,ptau,cxs)
            | Some _ -> 
                memberLP denv path v
        attribsL denv KindType v.Attribs vL

    let valL denv (v:Val) = valLP denv Path.NoPath v

    let extensionMembersLP denv (path:Path) (vs:Val list) =
      let extensionMemberLP (v:Val) =
        let tycon = v.MemberApparentParent.Deref
        let nameL = wordL tycon.DisplayName
        let nameL = accessibilityL denv tycon.Accessibility nameL // "type-accessibility"
        let tps =
          match PartitionValTypars denv.g v with
            | Some(_,memberParentTypars,_,_,_) -> memberParentTypars
            | None -> []          
        let lhsL = wordL "type" ^^ typarDeclsL denv nameL tycon.IsPrefixDisplay tps
        (lhsL ^^ wordL "with") @@-- (valLP denv path v)
      aboveListL (List.map extensionMemberLP vs)

    let ucaseArgTypesL denv argtys = 
        sepListL (wordL "*") (List.map (typeWithPrecL denv SimplifyTypes.typeSimplificationInfo0 2) argtys)

    let ucaseLP denv (path : Path) prefixL ucase =
        let nmL = (DemangleOperatorName ucase.Id.idText) &== path
        //let nmL = accessibilityL denv ucase.Accessibility nmL
        match ucase.RecdFields |> List.map (fun rfld -> rfld.FormalType) with
        | []     -> (prefixL ^^ nmL)
        | argtys -> (prefixL ^^ nmL ^^ wordL "of") --- ucaseArgTypesL denv argtys

    let ucaseL denv prefixL ucase = ucaseLP denv Path.Empty prefixL ucase

    let ucasesLP denv (path : Path) ucases =
        let prefixL = wordL "|" // See bug://2964 - always prefix in case preceeded by accessibility modifier
        List.map (ucaseLP denv path prefixL) ucases
        
    let rfspecLP addAccess denv (path : Path) (fld:RecdField) =
        let lhs = fld.Name &== path
        let lhs = (if addAccess then accessibilityL denv fld.Accessibility lhs else lhs)
        let lhs = if fld.IsMutable then wordL "mutable" --- lhs else lhs
        (lhs ^^ rightL ":") --- typeL denv fld.FormalType

    let rfspecL addAccess denv fld = rfspecLP addAccess denv Path.Empty fld

    /// When to force a break? "type tyname = <HERE> repn"
    /// When repn is class or datatype constructors (not single one).
    let breakTypeDefnEqn repr =
        match repr with 
        | TFsObjModelRepr _ -> true
        | TFiniteUnionRepr r    -> r.CasesTable.UnionCasesAsList.Length > 1
        | TRecdRepr _ -> true
        | TAsmRepr _ 
        | TILObjModelRepr _  -> false
        | TMeasureableRepr _ -> false

    let tyconLP denv (path : Path) typewordL (tycon:Tycon) =
        let prefix = tycon.IsPrefixDisplay
        let name   = tycon.DisplayName
        let nameL  = name &== path
        let nameL = accessibilityL denv tycon.Accessibility nameL 
        let denv = denv.AddAccessibility tycon.Accessibility 
        let path  = path.Add name // the full names of constructors and , e.g., have their type name as part of the path
        let lhsL =
            let tps = tycon.TyparsNoRange
            let tpsL = typarDeclsL denv nameL prefix tps
            typewordL ^^ tpsL
        let memberImplementLs,memberCtorLs,memberInstanceLs,memberStaticLs = 
            let adhoc = 
                tycon.MembersOfFSharpTyconSorted
                |> List.filter (fun v -> not v.IsDispatchSlot) 
                |> List.filter (fun v -> not v.Deref.IsClassConstructor) 
                |> List.filter (fun v -> 
                                    match v.MemberInfo.Value.ImplementedSlotSigs with 
                                    | TSlotSig(_,oty,_,_,_,_) :: _ -> 
                                        // Don't print overrides in HTML docs
                                        denv.showOverrides && 
                                        // Don't print individual methods forming interface implementations - these are currently never exported 
                                        not (isInterfaceTy denv.g oty)
                                    | [] -> true)
                |> List.filter (fun v -> denv.showObsoleteMembers || not (HasAttrib denv.g denv.g.attrib_SystemObsolete v.Attribs))
            // sort 
            let sortKey (v:ValRef) = (not v.IsConstructor,    // constructors before others 
                                      v.Id.idText,            // sort by name 
                                      (if v.IsCompiledAsTopLevel then v.ValReprInfo.Value.NumCurriedArgs else 0),  // sort by #curried
                                      (if v.IsCompiledAsTopLevel then v.ValReprInfo.Value.AritiesOfArgs  else [])  // sort by arity 
                                     )            
            let adhoc = adhoc |> List.sortBy sortKey
            let iimpls = 
                match tycon.TypeReprInfo with 
                | Some (TFsObjModelRepr r) when (match r.fsobjmodel_kind with TTyconInterface -> true | _ -> false) -> []
                | _ -> tycon.InterfacesOfFSharpTycon
            let iimpls = iimpls |> List.filter (fun (_,compgen,_) -> not compgen)
            // if TTyconInterface, the iimpls should be printed as inheritted interfaces 
            let iimplsLs = iimpls |> List.map (fun (ty,_,_) -> wordL "interface" --- typeL denv ty)
            let adhocCtorsLs    = adhoc |> List.filter (fun v -> v.IsConstructor)                               |> List.map (fun vref -> valLP denv path vref.Deref)
            let adhocInstanceLs = adhoc |> List.filter (fun v -> not v.IsConstructor && v.IsInstanceMember)     |> List.map (fun vref -> valLP denv path vref.Deref)
            let adhocStaticLs   = adhoc |> List.filter (fun v -> not v.IsConstructor && not v.IsInstanceMember) |> List.map (fun vref -> valLP denv path vref.Deref)
            iimplsLs,adhocCtorsLs,adhocInstanceLs,adhocStaticLs
        let memberLs = memberImplementLs @ memberCtorLs @ memberInstanceLs @ memberStaticLs
        let addMembersAsWithEnd reprL = 
            if isNil memberLs then reprL
            else reprL @@ (wordL "with" @@-- aboveListL memberLs) @@ wordL "end"

        let reprL = 
            match tycon.TypeReprInfo with 
            | Some repr -> 
                let brk  = nonNil memberLs || breakTypeDefnEqn repr
                let rhsL =                     
                    let addReprAccessL l = accessibilityL denv tycon.TypeReprAccessibility l 
                    let denv = denv.AddAccessibility tycon.TypeReprAccessibility 
                    match repr with 
                    | TRecdRepr _ ->
                        let recdFieldRefL fld = rfspecLP false denv path fld ^^ rightL ";"
                        let recdL = tycon.TrueFieldsAsList |> List.map recdFieldRefL |> aboveListL |> braceL
                        Some (addMembersAsWithEnd (addReprAccessL recdL))
                        
                    | TFsObjModelRepr r -> 
                        match r.fsobjmodel_kind with 
                        | TTyconDelegate (TSlotSig(_,_, _,_,paraml, rty)) ->
                            let rty = GetFSharpViewOfReturnType denv.g rty
                            Some (wordL "delegate of" --- topTypeL denv SimplifyTypes.typeSimplificationInfo0 (paraml |> List.mapSquared (fun sp -> (sp.Type, ValReprInfo.unnamedTopArg1))) rty [])
                        | _ ->
                            match r.fsobjmodel_kind with
                            | TTyconEnum -> 
                                tycon.TrueFieldsAsList
                                |> List.map (fun f -> 
                                                  match f.LiteralValue with 
                                                  | None -> emptyL
                                                  | Some c -> wordL "| " ^^ (f.Name &== path) ^^ wordL " = " ^^ constL c)
                                |> aboveListL
                                |> Some
                            | _ -> 
                                let start = 
                                    match r.fsobjmodel_kind with
                                    | TTyconClass -> "class" 
                                    | TTyconInterface -> "interface" 
                                    | TTyconStruct -> "struct" 
                                    | TTyconEnum -> "enum" 
                                    | _ -> failwith "???"
                                let inherits = 
                                   match r.fsobjmodel_kind, tycon.TypeContents.tcaug_super with
                                   | TTyconClass,Some super -> [wordL  "inherit" ^^ (typeL denv super)] 
                                   | TTyconInterface,_ -> 
                                     tycon.InterfacesOfFSharpTycon
                                       |> List.filter (fun (_,compgen,_) -> not compgen)
                                       |> List.map (fun (ity,_,_) -> wordL  "inherit" ^^ (typeL denv ity))
                                   | _ -> []
                                let vsprs = 
                                    tycon.MembersOfFSharpTyconSorted
                                    |> List.filter (fun v -> isNil (the(v.MemberInfo)).ImplementedSlotSigs) 
                                    |> List.filter (fun v -> v.IsDispatchSlot)
                                    |> List.map (fun vref -> valLP denv path vref.Deref)
                                let staticValsLs  = 
                                    tycon.TrueFieldsAsList
                                    |> List.filter (fun f -> f.IsStatic)
                                    |> List.map (fun f -> wordL "static" ^^ wordL "val" ^^ rfspecLP true denv path f)
                                let instanceValsLs  = 
                                    tycon.TrueFieldsAsList
                                    |> List.filter (fun f -> not f.IsStatic)
                                    |> List.map (fun f -> wordL "val" ^^ rfspecLP true denv path f)
                                let alldecls = inherits @ memberImplementLs @ memberCtorLs @ instanceValsLs @ vsprs @ memberInstanceLs @ staticValsLs @ memberStaticLs
                                let emptyMeasure = match tycon.TypeOrMeasureKind with KindMeasure -> isNil alldecls | _ -> false
                                if emptyMeasure then None else Some ((wordL start @@-- aboveListL alldecls) @@ wordL "end")
                    | TFiniteUnionRepr _        -> 
                        let ucasesL = tycon.UnionCasesAsList |> ucasesLP denv path |> aboveListL
                        Some (addMembersAsWithEnd (addReprAccessL ucasesL))
                    | TAsmRepr _                      -> 
                        Some (wordL "(# \"<Common IL Type Omitted>\" #)")
                    | TMeasureableRepr ty                 ->
                        Some (typeL denv ty)
                 
                    | TILObjModelRepr (_,_,td) -> 
                        Some (IlPrint.ilTypeDefL denv path td)

                let brk  = match tycon.TypeReprInfo with
                           | Some (TILObjModelRepr _) -> true
                           | _                        -> brk
                match rhsL with 
                | None  -> lhsL
                | Some rhsL -> 
                    if brk then 
                        (lhsL ^^ wordL "=") @@-- rhsL 
                    else 
                        (lhsL ^^ wordL "=") ---  rhsL

            | None   -> 
                match tycon.TypeAbbrev with
                | None   -> 
                    addMembersAsWithEnd lhsL 
                | Some a -> 
                    (lhsL ^^ wordL "=") --- (typeL denv a)
        attribsL denv tycon.TypeOrMeasureKind tycon.Attribs reprL

    let tyconL denv typewordL tycon = tyconLP denv Path.NoPath typewordL tycon

    let prettyTypeL denv typ = 
        let _,typ,cxs = PrettyTypes.PrettifyTypes1 denv.g typ
        let env = SimplifyTypes.CollectInfo true [typ] cxs
        let cxsL = constraintsL denv env env.postfixConstraints
        typeWithPrecL denv env 2 typ  --- cxsL

    // Layout: exception spec 
      
    let exnDefnReprL denv repr =
        match repr with 
        | TExnAbbrevRepr ecref -> wordL "=" --- tcrefL denv ecref
        | TExnAsmRepr _     -> wordL "=" --- wordL "(# ... #)"
        | TExnNone             -> emptyL
        | TExnFresh r          -> 
            match r.TrueFieldsAsList with
            | []  -> emptyL
            | r -> wordL "of" --- ucaseArgTypesL denv (r |> List.map (fun rfld -> rfld.FormalType))

    let exnDefnLP denv (path : Path) (exnc:Entity) =
        let nm = exnc.LogicalName
        let nmL = annotateWithPath nm path nm
        let nmL = accessibilityL denv exnc.TypeReprAccessibility nmL
        let exnL = wordL "exception" ^^ nmL // need to tack on the Exception at the right of the name for goto definition
        exnL ^^ exnDefnReprL denv exnc.ExceptionInfo

    let exnDefnL denv exnc = exnDefnLP denv Path.NoPath exnc

    // Layout: module spec 

    let tyconSpecsLP denv (path : Path) (tycons:Tycon list) =
        match tycons with 
        | [] -> emptyL
        | [h] when h.IsExceptionDecl -> exnDefnLP denv path h
        | h :: t -> 
            let x  = tyconLP denv path (wordL "type") h
            let xs = List.map (tyconLP denv path (wordL "and")) t
            aboveListL (x::xs)

    /// Layout the inferred signature of a compilation unit
    let inferredSigOfModuleExprL showHeader denv expr =

        let rec isConcreteNamespace x = 
            match x with 
            | TMDefRec(tycons,binds,mbinds,_) -> 
                nonNil tycons || not (FlatList.isEmpty binds) || (mbinds |> List.exists (fun (ModuleOrNamespaceBinding(x,_)) -> not x.IsNamespace))
            | TMDefLet _  -> true
            | TMDefDo _  -> true
            | TMDefs defs -> defs |> List.exists isConcreteNamespace 
            | TMAbstract(ModuleOrNamespaceExprWithSig(_,def,_)) -> isConcreteNamespace def

        let rec imexprLP denv (path : Path) (ModuleOrNamespaceExprWithSig(_,def,_)) = imdefLP denv path def

        and imexprL denv (ModuleOrNamespaceExprWithSig(mty,def,m)) = imexprLP denv Path.Empty (ModuleOrNamespaceExprWithSig(mty,def,m))

        and imdefsLP denv (path : Path) x = aboveListL (x |> List.map (imdefLP denv path))

        and imdefLP denv (path : Path) x = 
            let filterVal    (v:Val) = not v.IsCompilerGenerated && isNone v.MemberInfo
            let filterExtMem (v:Val) = v.IsExtensionMember
            match x with 
            | TMDefRec(tycons,binds,mbinds,_) -> 
                 tyconSpecsLP denv path tycons @@ 
                 (binds |> valsOfBinds |> List.filter filterExtMem |> extensionMembersLP denv path) @@
                 (binds |> valsOfBinds |> List.filter filterVal    |> List.map (valLP denv path)   |> aboveListL) @@
                 (mbinds |> List.map (imbindLP denv path) |> aboveListL)
            | TMDefLet(bind,_) -> ([bind.Var] |> List.filter filterVal    |> List.map (valLP denv path) |> aboveListL)
            | TMDefs defs -> imdefsLP denv path defs
            | TMDefDo _  -> emptyL
            | TMAbstract mexpr -> imexprLP denv path mexpr
        and imbindLP denv (path : Path) (ModuleOrNamespaceBinding(mspec, def)) = 
            let nm =  mspec.DemangledModuleOrNamespaceName
            let innerPath = (fullCompPathOfModuleOrNamespace mspec).AccessPath
            let outerPath = mspec.CompilationPath.AccessPath

            let denv = denv.AddOpenPath (List.map fst innerPath) 
            if mspec.IsNamespace then  
                let basic = imdefLP denv path def
                // Check if this namespace contains anything interesting
                if isConcreteNamespace def then 
                    // This is a container namespace. We print the header when we get to the first concrete module.
                    let headerL = wordL ("namespace " ^ (String.concat "." (innerPath |> List.map fst)))
                    headerL @@-- basic
                else
                    // This is a namespace that only contains namespaces. Skipt the header
                    basic
            else
                // This is a module 
                let nmL   = accessibilityL denv mspec.Accessibility (nm &== path)
                let denv  = denv.AddAccessibility mspec.Accessibility 
                let basic = imdefLP denv path def
                // Check if its an outer module or a nested module
                if (outerPath |> List.forall (fun (_,istype) -> istype = Namespace) ) then 
                    // OK, this is an outer module
                    if showHeader then 
                        // OK, we're not in F# Interactive
                        // Check if this is an outer module with no namespace
                        if isNil outerPath then 
                            // If so print a "module" declaration
                            (wordL "module" ^^ nmL) @@ basic
                        else 
                            // Otherwise this is an outer module contained immediately in a namespace
                            // We already printed the namespace declaration earlier.  So just print the 
                            // module now.
                            ((wordL "module" ^^ nmL ^^ wordL "=" ^^ wordL "begin") @@-- basic) @@ wordL "end"
                    else
                        // OK, wer'e in F# Interactive, presumably the implicit module for each interaction.
                        basic
                else
                    // OK, this is a nested module
                    ((wordL "module" ^^ nmL ^^ wordL "=" ^^ wordL "begin") @@-- basic) @@ wordL "end"
        imexprL denv expr

    type DeclSpec = 
        | DVal of Val 
        | DTycon of Tycon 
        | DException of Tycon 
        | DModul of ModuleOrNamespace

    let rangeOfDeclSpec = function
        | DVal   v -> v.Range
        | DTycon t -> t.Range
        | DException t -> t.Range
        | DModul m -> m.Range

    /// modul - provides (valspec)* - and also types, exns and submodules.
    /// Each defines a decl block on a given range.
    /// Can sort on the ranges to recover the original declaration order.
    let rec moduleOrNamespaceTypeLP (topLevel : bool)(denv : DisplayEnv)(path : Path)(mtype : ModuleOrNamespaceType) =
        let declSpecs : DeclSpec list =
            List.concat
              [mtype.AllValsAndMembers |> Seq.toList |> List.filter (fun v -> not v.IsCompilerGenerated && v.MemberInfo.IsNone) |> List.map DVal;
               mtype.TypeDefinitions |> List.map DTycon;
               mtype.ExceptionDefinitions |> List.map DException;
               mtype.ModuleAndNamespaceDefinitions |> List.map DModul;
              ]
       
        let declSpecs = List.sortWithOrder (Order.orderOn rangeOfDeclSpec rangeOrder) declSpecs
        let declSpecL =
          function // only show namespaces / modules at the top level; this is because we've no global namespace
          | DVal  vspec      when not topLevel -> valLP                     denv path vspec
          | DTycon tycon     when not topLevel -> tyconLP                   denv path (wordL "type") tycon
          | DException tycon when not topLevel -> exnDefnLP                 denv path tycon 
          | DModul mspec                       -> moduleOrNamespaceLP false denv path mspec
          | _                                  -> emptyL // this catches non-namespace / modules at the top-level

        aboveListL (List.map declSpecL declSpecs)

    and moduleOrNamespaceLP (topLevel: bool) (denv: DisplayEnv) (path: Path) (mspec: ModuleOrNamespace) = 
        let istype = mspec.ModuleOrNamespaceType.ModuleOrNamespaceKind
        let nm     = mspec.DemangledModuleOrNamespaceName
        let denv   = denv.AddOpenModuleOrNamespace (mkLocalModRef mspec) 
        let nmL    = accessibilityL denv mspec.Accessibility (nm &== path)
        let denv   = denv.AddAccessibility mspec.Accessibility 
        let path   = path.Add nm // tack on the current module to be used in calls to linearise all subterms
        let body   = moduleOrNamespaceTypeLP topLevel denv path  mspec.ModuleOrNamespaceType
        if istype = Namespace
           then (wordL "namespace" ^^ nmL) @@-- body
           else (wordL "module" ^^ nmL ^^ wordL "= begin") @@-- body @@ wordL "end"

    let moduleOrNamespaceTypeL (denv : DisplayEnv)(mtype : ModuleOrNamespaceType) = moduleOrNamespaceTypeLP false denv Path.Empty mtype
    let moduleOrNamespaceL (denv : DisplayEnv)(mspec : ModuleOrNamespace) = moduleOrNamespaceLP false denv Path.Empty mspec
    let assemblyL denv (mspec : ModuleOrNamespace) = moduleOrNamespaceTypeLP true denv Path.Empty mspec.ModuleOrNamespaceType // we seem to get the *assembly* name as an outer module, this strips this off

    //--------------------------------------------------------------------------
    // Nice printing of a subset of expressions, e.g. for refutations in pattern matching
    //--------------------------------------------------------------------------

    let rec dataExprL denv expr = dataExprWrapL denv false expr
    and atomL denv expr = dataExprWrapL denv true  expr // true means bracket if needed to be atomic expr 

    and dataExprWrapL denv isAtomic expr =
        match expr with
        | Expr.Const (c,_,ty)                          -> 
            if isEnumTy denv.g ty then 
                wordL "enum" ^^ angleL (typeL denv ty) ^^ bracketL (constL c)
            else
                constL c

        | Expr.Val (v,_,_)                         -> wordL (v.DisplayName)
        | Expr.Link rX                                 -> dataExprWrapL denv isAtomic (!rX)
        | Expr.Op (TOp.UnionCase(c),_,args,_)        -> 
            if denv.g.ucref_eq c denv.g.nil_ucref then wordL "[]"
            elif denv.g.ucref_eq c denv.g.cons_ucref then 
                let rec strip = function (Expr.Op (TOp.UnionCase _,_,[h;t],_)) -> h::strip t | _ -> []
                listL (dataExprL denv) (strip expr)
            elif isNil(args) then 
                wordL c.CaseName 
            else 
                (wordL c.CaseName ++ bracketL (commaListL (dataExprsL denv args)))
            
        | Expr.Op (TOp.ExnConstr(c),_,args,_)           ->  (wordL c.LogicalName ++ bracketL (commaListL (dataExprsL denv args)))
        | Expr.Op (TOp.Tuple,_,xs,_)                  -> tupleL (dataExprsL denv xs)
        | Expr.Op (TOp.Recd (_,tc),_,xs,_) -> 
            let fields = tc.TrueInstanceFieldsAsList
            let lay fs x = (wordL fs.rfield_id.idText ^^ sepL "=") --- (dataExprL denv x)
            leftL "{" ^^ semiListL (List.map2 lay fields xs) ^^ rightL "}" 
        | Expr.Op (TOp.Array,[_],xs,_)                 -> leftL "[|" ^^ semiListL (dataExprsL denv xs) ^^ rightL "|]"
        | _ -> wordL "?"
    and dataExprsL denv xs = List.map (dataExprL denv) xs

    //--------------------------------------------------------------------------
    // Print Signatures/Types - ouput functions - old style
    //-------------------------------------------------------------------------- 
        
    // A few old-style o/p functions are used, e.g. in tc.fs *)  
        
    let outputILTypeRef                 denv os x    = trefL denv x                |> bufferL os
    let outputTyconRef                denv os x    = x |> tcrefL denv      |> bufferL os    
    let outputValSpec             denv os x    = x |> valL denv              |> bufferL os
    let outputTy                  denv os x    = x |> typeL denv             |> bufferL os  
    let outputExnDef                 denv os x    = x |> exnDefnL denv          |> bufferL os
    let stringOfTyparConstraints denv x       = x |> constraintsL denv SimplifyTypes.typeSimplificationInfo0  |> showL
    let outputTycon                denv os x    = tyconL denv (wordL "type") x      |> bufferL os
    let outputTypars               denv nm os tps  = (typarDeclsL denv  (wordL nm) true tps)  |> bufferL os
    let stringOfTyparConstraint  denv tpc     = stringOfTyparConstraints denv [tpc]
    let stringOfTy               denv    typ  = typeL denv typ                                 |> showL
    let prettyStringOfTy        denv    typ  = prettyTypeL denv typ                          |> showL
    let stringOfRecdField            denv x    = x |> rfspecL false denv |> showL
    let stringOfUnionCase             denv x    = ucaseL denv (wordL "|") x |> showL
    let stringOfExnDef              denv x    = x |> exnDefnL denv          |> showL
    let string_of_val_spec          denv x    = x |> valL denv              |> showL
    // Print members with a qualification showing the type they are contained in 
    let outputQualifiedValSpec denv os v = outputValSpec { denv with showMemberContainers=true; } os v
    let stringOfQualifiedValSpec denv v = string_of_val_spec { denv with showMemberContainers=true; } v

end

//--------------------------------------------------------------------------
// DEBUG layout
//---------------------------------------------------------------------------

#if SILVERLIGHT

module DebugPrint = 
    let valRefL (x : ValRef) = wordL (string x)
    let unionCaseRefL (x : UnionCaseRef) = wordL (string x)
    let intL (x : int) = wordL (string x)
    let valL (x : Val) = wordL (string x)
    let typarL (x : Typar) = wordL (string x)
    let typarDeclL (x : Typar) = wordL (string x)
    let typarsL (x : Typars) = wordL (string x)
    let typeL (x : TType) = wordL (string x)
    let slotSigL (x : SlotSig) = wordL (string x)
    let entityTypeL (x : ModuleOrNamespaceType) = wordL (string x)
    let entityL (x : ModuleOrNamespace) = wordL (string x)
    let typeOfValL (x : Val) = wordL (string x)
    let bindingL (x : Binding) = wordL (string x)
    let exprL (x : Expr) = wordL (string x)
    let decisionTreeL (x : DecisionTree) = wordL (string x)    
    let tyconL (x : Tycon) = wordL (string x)
    let implFileL  (x : TypedImplFile) = wordL (string x)
    let assemblyL  (x : TypedAssembly) = wordL (string x)
    let vspecAtBindL (x : Val) = wordL (string x)
    let recdFieldRefL (x : RecdFieldRef) = wordL (string x)
    let traitL (x : TraitConstraintInfo) = wordL (string x)
    let layout_ranges = ref false
    let showType (t : TType) = string t
    let showExpr (e : Expr) = string e
    let mdefL (mdef : ModuleOrNamespaceExpr) = wordL (string mdef)
    let layoutRanges = ref false

#else

module DebugPrint = begin
    open Microsoft.FSharp.Compiler.Layout
    open PrettyTypes
    let layoutRanges = ref false  

    let intL (n:int)          = wordL (string n )
    let int64L (n:int64)          = wordL (string n )

    let jlistL xL xmap = QueueList.foldBack (fun x z -> z @@ xL x) xmap emptyL

    let bracketIfL x lyt = if x then bracketL lyt else lyt

    let lvalopL x = 
        match x with 
        | LGetAddr  -> wordL "LGetAddr"
        | LByrefGet -> wordL "LByrefGet"
        | LSet      -> wordL "LSet"
        | LByrefSet -> wordL "LByrefSet"

    let angleBracketL l = leftL "<" ^^ l ^^ rightL ">"
    let angleBracketListL l = angleBracketL (sepListL (sepL ",") l)


    let memFlagsL memFlags = 
        let stat = if memFlags.IsInstance || (memFlags.MemberKind = MemberKind.Constructor) then emptyL else wordL "static"
        let stat = if memFlags.IsDispatchSlot then stat ++ wordL "abstract" 
                   elif memFlags.IsOverrideOrExplicitImpl then stat ++ wordL "override" 
                   else stat
        stat

    let stampL _n w = 
#if DEBUG
        if !verboseStamps then w ^^ sepL "#" ^^ int64L _n else 
#endif
        w

    let tcrefL (tc:TyconRef) = wordL tc.DisplayName |> stampL tc.Stamp


    let rec auxTypeL env typ = auxTypeWrapL env false typ

    and auxTypeAtomL env typ = auxTypeWrapL env true  typ

    and auxTyparsL env tcL prefix tinst = 
       match tinst with 
       | [] -> tcL
       | [t] -> 
         let tL = auxTypeAtomL env t
         if prefix then        tcL ^^ angleBracketL tL 
         else            tL ^^ tcL 
       | _ -> 
         let tinstL = List.map (auxTypeL env) tinst
         if prefix then                   
             tcL ^^ angleBracketListL tinstL
         else  
             tupleL tinstL ^^ tcL
            
    and auxTypeWrapL env isAtomic typ = 
        let wrap x = NicePrint.bracketIfL isAtomic x in // wrap iff require atomic expr 
        match stripTyparEqns typ with
        | TType_forall (typars,rty) -> 
           (leftL "!" ^^ typarDeclsL typars --- auxTypeL env rty) |> wrap
        | TType_ucase (UCRef(tcref,_),tinst)  
        | TType_app (tcref,tinst)   -> 
           let prefix = tcref.IsPrefixDisplay
           let tcL = tcrefL tcref
           auxTyparsL env tcL prefix tinst
        | TType_tuple typs          -> sepListL (wordL "*") (List.map (auxTypeAtomL env) typs) |> wrap
        | TType_fun (f,x)           -> ((auxTypeAtomL env f ^^ wordL "->") --- auxTypeL env x) |> wrap
        | TType_var typar           -> auxTyparWrapL env isAtomic typar 
        | TType_measure unt -> 
#if DEBUG
          leftL "{" ^^
          (match !global_g with
           | None -> wordL "<no global g>"
           | Some g -> 
             let sortVars (vs:(Typar * int) list) = vs |> List.sortBy (fun (v,_) -> v.DisplayName) 
             let sortCons (cs:(TyconRef * int) list) = cs |> List.sortBy (fun (c,_) -> c.DisplayName) 
             let negvs,posvs = ListMeasureVarOccsWithNonZeroExponents         unt |> sortVars |> List.partition (fun (_,e) -> e<0)
             let negcs,poscs = ListMeasureConOccsWithNonZeroExponents g false unt |> sortCons |> List.partition (fun (_,e) -> e<0)
             let unparL (uv:Typar) = wordL ("'" ^  uv.DisplayName)
             let unconL tc = tcrefL tc
             let prefix = spaceListL  (List.map (fun (v,e) -> if e=1  then unparL v else unparL v -- wordL (sprintf "^ %d" e)) posvs @
                                       List.map (fun (c,e) -> if e=1  then unconL c else unconL c -- wordL (sprintf "^ %d" e)) poscs)
             let postfix = spaceListL (List.map (fun (v,e) -> if e= -1 then unparL v else unparL v -- wordL (sprintf "^ %d" (-e))) negvs @
                                       List.map (fun (c,e) -> if e= -1 then unconL c else unconL c -- wordL (sprintf "^ %d" (-e))) negcs)
             match (negvs,negcs) with 
             | [],[] -> prefix 
             | _ -> prefix ^^ sepL "/" ^^ postfix) ^^
          rightL "}"
#else
          unt |> ignore
          wordL "<measure>"
#endif

    and auxTyparWrapL (env:SimplifyTypes.TypeSimplificationInfo) isAtomic (typar:Typar) =
          let wrap x = NicePrint.bracketIfL isAtomic x in // wrap iff require atomic expr 
          // There are several cases for pprinting of typar.
          // 
          //   'a              - is multiple  occurance.
          //   #Type           - inplace coercion constraint and singleton
          //   ('a :> Type)    - inplace coercion constraint not singleton
          //   ('a.opM : S->T) - inplace operator constraint
          let tpL =
            wordL (prefixOfStaticReq typar.StaticReq
                   ^ prefixOfRigidTypar typar
                   ^ typar.DisplayName)
          let varL = tpL |> stampL typar.Stamp 

          match Zmap.tryFind typar env.inplaceConstraints with
          | Some (typarConstrTyp) ->
              if Zset.contains typar env.singletons then
                leftL "#" ^^ auxTyparConstraintTypL env typarConstrTyp
              else
                (varL ^^ sepL ":>" ^^ auxTyparConstraintTypL env typarConstrTyp) |> wrap
          | _ -> varL

    and auxTypar2L     env typar = auxTyparWrapL env false typar

    and auxTyparAtomL env typar = auxTyparWrapL env true  typar

    and auxTyparConstraintTypL env ty = auxTypeL env ty

    and auxTraitL env (ttrait: TraitConstraintInfo) =
#if DEBUG
        let (TTrait(tys,nm,memFlags,argtys,rty,_)) = ttrait 
        match !global_g with
        | None -> wordL "<no global g>"
        | Some g -> 
            let rty = GetFSharpViewOfReturnType g rty
            let stat = memFlagsL memFlags
            let argsL = sepListL (wordL "*") (List.map (auxTypeAtomL env) argtys)
            let resL  = auxTypeL env rty
            let methodTypeL = (argsL ^^ wordL "->") ++ resL
            bracketL (stat ++ bracketL (sepListL (wordL "or") (List.map (auxTypeAtomL env) tys)) ++ wordL "member" --- (wordL nm ^^ wordL ":" -- methodTypeL))
#else
        ignore (env,ttrait)
        wordL "trait"
#endif

    and auxTyparConstraintL env (tp,tpc) = 
        let constraintPrefix l = auxTypar2L env tp ^^ wordL ":" ^^ l
        match tpc with
        | TTyparCoercesToType(typarConstrTyp,_) ->
            auxTypar2L env tp ^^ wordL ":>" --- auxTyparConstraintTypL env typarConstrTyp
        | TTyparMayResolveMemberConstraint(traitInfo,_) ->
            auxTypar2L env tp ^^ wordL ":"  --- auxTraitL env traitInfo
        | TTyparDefaultsToType(_,ty,_) ->
            wordL "default" ^^ auxTypar2L env tp ^^ wordL ":" ^^ auxTypeL env ty
        | TTyparIsEnum(ty,_) ->
            auxTyparsL env (wordL "enum") true [ty] |> constraintPrefix
        | TTyparIsDelegate(aty,bty,_) ->
            auxTyparsL env (wordL "delegate") true [aty; bty] |> constraintPrefix
        | TTyparSupportsNull _ ->
            wordL "null" |> constraintPrefix
        | TTyparSupportsComparison _ ->
            wordL "comparison" |> constraintPrefix
        | TTyparSupportsEquality _ ->
            wordL "equality" |> constraintPrefix
        | TTyparIsNotNullableValueType _ ->
            wordL "struct" |> constraintPrefix
        | TTyparIsReferenceType _ ->
            wordL "not struct" |> constraintPrefix
        | TTyparIsUnmanaged _ ->
            wordL "unmanaged" |> constraintPrefix
        | TTyparSimpleChoice(tys,_) ->
            bracketL (sepListL (sepL "|") (List.map (auxTypeL env) tys)) |> constraintPrefix
        | TTyparRequiresDefaultConstructor _ ->
            bracketL (wordL "new : unit -> " ^^ (auxTypar2L env tp)) |> constraintPrefix

    and auxTyparConstraintsL env x = 
        match x with 
        | []   -> emptyL
        | cxs -> wordL "when" --- aboveListL (List.map (auxTyparConstraintL env) cxs)    

    and typarL     tp = auxTypar2L     SimplifyTypes.typeSimplificationInfo0 tp 
    and typarAtomL tp = auxTyparAtomL SimplifyTypes.typeSimplificationInfo0 tp

    and typeAtomL tau =
        let tau,cxs = tau,[]
        let env = SimplifyTypes.CollectInfo false [tau] cxs
        match env.postfixConstraints with
        | [] -> auxTypeAtomL env tau
        | _ -> bracketL (auxTypeL env tau --- auxTyparConstraintsL env env.postfixConstraints)
          
    and typeL tau =
        let tau,cxs = tau,[]
        let env = SimplifyTypes.CollectInfo false [tau] cxs
        match env.postfixConstraints with
        | [] -> auxTypeL env tau 
        | _ -> (auxTypeL env tau --- auxTyparConstraintsL env env.postfixConstraints) 

    and typarDeclL tp =
        let tau,cxs = mkTyparTy tp,(List.map (fun x -> (tp,x)) tp.Constraints)
        let env = SimplifyTypes.CollectInfo false [tau] cxs
        match env.postfixConstraints with
        | [] -> auxTypeL env tau 
        | _ -> (auxTypeL env tau --- auxTyparConstraintsL env env.postfixConstraints) 
    and typarDeclsL tps = angleBracketListL (List.map typarDeclL       tps) 

    //--------------------------------------------------------------------------
    // DEBUG layout - types
    //--------------------------------------------------------------------------
      
    let rangeL m = wordL (stringOfRange m)

    let instL tyL tys =
        match tys with
        | []  -> emptyL
        | tys -> sepL "@[" ^^ commaListL (List.map tyL tys) ^^ rightL "]"

    let valRefL  (vr:ValRef)  = 
        wordL vr.LogicalName |> stampL vr.Stamp 

    let attribL (Attrib(_,k,_,_,_,_)) = 
        leftL "[<" ^^ 
        (match k with 
         | ILAttrib (ilmeth) -> wordL ilmeth.Name
         | FSAttrib (vref)   -> valRefL vref) ^^
        rightL ">]"
    
    let attribsL attribs = aboveListL (List.map attribL attribs)

    let arityInfoL (ValReprInfo (tpNames,_,_) as tvd) = 
        let ns = tvd.AritiesOfArgs in 
        leftL "arity<" ^^ intL tpNames.Length ^^ sepL ">[" ^^ commaListL (List.map intL ns) ^^ rightL "]"


    let valL (vspec:Val) =
        let vsL = wordL (DecompileOpName vspec.LogicalName) |> stampL vspec.Stamp
        let vsL = 
#if DEBUG
            if !verboseStamps then vsL ^^ rightL (if isSome(vspec.PublicPath) then "+" else "-") else 
#endif
            vsL
        let vsL = vsL -- attribsL (vspec.Attribs)
        vsL

    let typeOfValL      (v:Val) =
        (valL v
          ^^ (if  v.MustInline then wordL "inline " else emptyL) 
          ^^ (if v.IsMutable then wordL "mutable " else emptyL)
          ^^ wordL ":") -- typeL v.Type


    let tslotparamL(TSlotParam(nmOpt, typ, inFlag, outFlag, _,_)) =
        (optionL wordL nmOpt) ^^ wordL ":" ^^ typeL typ ^^ (if inFlag then wordL "[in]" else emptyL)  ^^ (if outFlag then wordL "[out]" else emptyL)  ^^ (if inFlag then wordL "[opt]" else emptyL)
    

    let slotSigL (slotsig:SlotSig) =
#if DEBUG
        let (TSlotSig(nm,typ,tps1,tps2,pms,rty)) = slotsig 
        match !global_g with
        | None -> wordL "<no global g>"
        | Some g -> 
            let rty = GetFSharpViewOfReturnType g rty
            (wordL "slot" --- (wordL nm) ^^ wordL "@" ^^ typeL typ) --
              (wordL "LAM" --- spaceListL (List.map typarL       tps1) ^^ rightL ".") ---
              (wordL "LAM" --- spaceListL (List.map typarL       tps2) ^^ rightL ".") ---
              (commaListL (List.map (List.map tslotparamL >> tupleL) pms)) ^^ (wordL "-> ") --- (typeL rty) 
#else
        ignore slotsig
        wordL "slotsig"
#endif

    let rec MemberL (v:Val) (membInfo:ValMemberInfo) = 
        (aboveListL [ wordL "compiled_name! = " ^^ wordL v.CompiledName ;
                      wordL "membInfo-slotsig! = " ^^ listL slotSigL membInfo.ImplementedSlotSigs ]) 
    and vspecAtBindL  v = 
        let vL = valL v  in
        let mutL = (if v.IsMutable then wordL "mutable" ++ vL else vL)
        mutL  --- (aboveListL (List.concat [[wordL ":" ^^ typeL v.Type];
                                            (match v.MemberInfo with None -> [] | Some mem_info   -> [wordL "!" ^^ MemberL v mem_info]);
                                            (match v.ValReprInfo with None -> [] | Some arity_info -> [wordL "#" ^^ arityInfoL arity_info])]))

    let unionCaseRefL (ucr:UnionCaseRef) = wordL ucr.CaseName
    let recdFieldRefL (rfref:RecdFieldRef) = wordL rfref.FieldName

    //--------------------------------------------------------------------------
    // DEBUG layout - bind, expr, dtree etc.
    //--------------------------------------------------------------------------

    let identL (id:Ident) = wordL id.idText  

    let rec tyconL (tycon:Tycon) =
        if tycon.IsModuleOrNamespace then entityL tycon else 
        
        let lhsL = wordL (match tycon.TypeOrMeasureKind with KindMeasure -> "[<Measure>] type" | KindType -> "type") ^^ wordL tycon.DisplayName ^^ typarDeclsL tycon.TyparsNoRange
        let lhsL = lhsL --- attribsL tycon.Attribs
        let memberLs = 
            let adhoc = 
                tycon.MembersOfFSharpTyconSorted 
                    |> List.filter (fun v -> not v.IsDispatchSlot)
                    |> List.filter (fun v -> not v.Deref.IsClassConstructor) 
                    // Don't print individual methods forming interface implementations - these are currently never exported 
                    |> List.filter (fun v -> isNil (the(v.MemberInfo)).ImplementedSlotSigs)
            let iimpls = 
                match tycon.TypeReprInfo with 
                | Some (TFsObjModelRepr r) when (match r.fsobjmodel_kind with TTyconInterface -> true | _ -> false) -> []
                | _ -> tycon.InterfacesOfFSharpTycon
            let iimpls = iimpls |> List.filter (fun (_,compgen,_) -> not compgen)
            // if TTyconInterface, the iimpls should be printed as inheritted interfaces 
            if (isNil adhoc && isNil iimpls) 
            then emptyL 
            else 
                let iimplsLs = iimpls |> List.map (fun (ty,_,_) -> wordL "interface" --- typeL ty)
                let adhocLs  = adhoc  |> List.map (fun vref -> vspecAtBindL  vref.Deref)
                (wordL "with" @@-- aboveListL (iimplsLs @ adhocLs)) @@ wordL "end"

        let ucaseArgTypesL argtys = sepListL (wordL "*") (List.map typeL argtys)

        let ucaseL prefixL ucase =
            let nmL = wordL (DemangleOperatorName ucase.Id.idText)
            match ucase.RecdFields |> List.map (fun rfld -> rfld.FormalType) with
            | []     -> (prefixL ^^ nmL)
            | argtys -> (prefixL ^^ nmL ^^ wordL "of") --- ucaseArgTypesL argtys

        let ucasesL ucases =
            let prefixL = if List.length ucases > 1 then wordL "|" else emptyL
            List.map (ucaseL prefixL) ucases
            
        let rfspecL (fld:RecdField) =
            let lhs = wordL fld.Name
            let lhs = if fld.IsMutable then wordL "mutable" --- lhs else lhs
            (lhs ^^ rightL ":") --- typeL fld.FormalType

        let tyconReprL (repr,tycon:Tycon) = 
            match repr with 
            | TRecdRepr _ ->
                tycon.TrueFieldsAsList |> List.map (fun fld -> rfspecL fld ^^ rightL ";") |> aboveListL  
            | TFsObjModelRepr r -> 
                match r.fsobjmodel_kind with 
                | TTyconDelegate _ ->
                    wordL "delegate ..."
                | _ ->
                    let start = 
                        match r.fsobjmodel_kind with
                        | TTyconClass -> "class" 
                        | TTyconInterface -> "interface" 
                        | TTyconStruct -> "struct" 
                        | TTyconEnum -> "enum" 
                        | _ -> failwith "???"
                    let inherits = 
                       match r.fsobjmodel_kind, tycon.TypeContents.tcaug_super with
                       | TTyconClass,Some super -> [wordL  "inherit" ^^ (typeL super)] 
                       | TTyconInterface,_ -> 
                         tycon.InterfacesOfFSharpTycon
                           |> List.filter (fun (_,compgen,_) -> not compgen)
                           |> List.map (fun (ity,_,_) -> wordL  "inherit" ^^ (typeL ity))
                       | _ -> []
                    let vsprs = 
                        tycon.MembersOfFSharpTyconSorted 
                            |> List.filter (fun v -> v.IsDispatchSlot) 
                            |> List.map (fun vref -> vspecAtBindL vref.Deref)
                    let vals  = tycon.TrueFieldsAsList |> List.map (fun f -> (if f.IsStatic then wordL "static" else emptyL) ^^ wordL "val" ^^ rfspecL f)
                    let alldecls = inherits @ vsprs @ vals
                    let emptyMeasure = match tycon.TypeOrMeasureKind with KindMeasure -> isNil alldecls | _ -> false
                    if emptyMeasure then emptyL else (wordL start @@-- aboveListL alldecls) @@ wordL "end"
            | TFiniteUnionRepr _        -> tycon.UnionCasesAsList |> ucasesL |> aboveListL 
            | TAsmRepr _                      -> wordL "(# ... #)"
            | TMeasureableRepr ty             -> typeL ty
            | TILObjModelRepr (_,_,td) -> wordL td.Name
        let reprL = 
            match tycon.TypeReprInfo with 
            | Some a -> let rhsL = tyconReprL (a,tycon) @@ memberLs
                        (lhsL ^^ wordL "=") @@-- rhsL
            | None   -> match tycon.TypeAbbrev with
                              | None   -> lhsL @@-- memberLs
                              | Some a -> (lhsL ^^ wordL "=") --- (typeL a @@ memberLs)
        reprL

        
    //--------------------------------------------------------------------------
    // layout - bind, expr, dtree etc.
    //--------------------------------------------------------------------------

    and bindingL (TBind(v,repr,_)) =
        vspecAtBindL v --- (wordL "=" ^^ exprL repr)

    and exprL expr = exprWrapL false expr
    and atomL expr = exprWrapL true  expr // true means bracket if needed to be atomic expr 

    and letRecL binds bodyL = 
        let eqnsL = 
            binds 
               |>  FlatList.toList 
               |> List.mapHeadTail (fun bind -> wordL "rec" ^^ bindingL bind ^^ wordL "in")
                              (fun bind -> wordL "and" ^^ bindingL bind ^^ wordL "in") 
        (aboveListL eqnsL @@ bodyL) 

    and letL bind bodyL = 
        let eqnL = wordL "let" ^^ bindingL bind ^^ wordL "in"
        (eqnL @@ bodyL) 
                                                               
    and exprWrapL isAtomic expr =
        let wrap = bracketIfL isAtomic // wrap iff require atomic expr 
        let lay =
            match expr with
            | Expr.Const (c,_,_)  -> NicePrint.constL c
            | Expr.Val (v,flags,_) -> 
                 let xL = valL v.Deref 
                 let xL =
#if DEBUG
                     if !verboseStamps then 
                         let tag = 
                           match v with
                           | VRefLocal _    -> ""
                           | VRefNonLocal _ -> "!!" 
                         xL ^^ rightL tag 
                     else
#endif
                         xL
                 let xL =
                     match flags with
                       | PossibleConstrainedCall _    -> xL ^^ rightL "<constrained>"
                       | CtorValUsedAsSelfInit    -> xL ^^ rightL "<selfinit>"
                       | CtorValUsedAsSuperInit -> xL ^^ rightL "<superinit>"
                       | VSlotDirectCall -> xL ^^ rightL "<vdirect>"
                       | NormalValUse -> xL 
                 xL
            | Expr.Seq (x0,x1,flag,_,_)                    -> 
                let flag = 
                    match flag with
                    | NormalSeq   -> "; (*Seq*)"
                    | ThenDoSeq   -> "; (*ThenDo*)" 
                ((exprL x0 ^^ rightL flag) @@ exprL x1) |> wrap
            | Expr.Lambda(_, _, baseValOpt,argvs,body,_,_)  -> 
                let formalsL = spaceListL (List.map vspecAtBindL argvs) in
                let bindingL = 
                    match baseValOpt with
                    | None       -> wordL "lam" ^^ formalsL ^^ rightL "."
                    | Some basev -> wordL "lam" ^^ (leftL "base=" ^^ vspecAtBindL basev) --- formalsL ^^ rightL "." in
                (bindingL ++ exprL body) |> wrap
            | Expr.TyLambda(_,argtyvs,body,_,_) -> 
                ((wordL "LAM"    ^^ spaceListL (List.map typarL       argtyvs) ^^ rightL ".") ++ exprL body) |> wrap
            | Expr.TyChoose(argtyvs,body,_) -> 
                ((wordL "CHOOSE" ^^ spaceListL (List.map typarL       argtyvs) ^^ rightL ".") ++ exprL body) |> wrap
            | Expr.App (f,_,tys,argtys,_) -> 
                let flayout = atomL f
                appL flayout tys argtys |> wrap
            | Expr.LetRec (binds,body,_,_) -> 
                letRecL binds (exprL body) |> wrap
            | Expr.Let    (bind,body,_,_) -> 
                letL bind (exprL body) |> wrap
            | Expr.Link rX -> 
                (wordL "RecLink" --- atomL (!rX)) |> wrap
            | Expr.Match (_,_,dtree,targets,_,_) -> 
                leftL "[" ^^ (decisionTreeL dtree @@ aboveListL (List.mapi targetL (targets |> Array.toList)) ^^ rightL "]")
            | Expr.Op (TOp.UnionCase (c),_,args,_)  -> 
                (unionCaseRefL c ++ spaceListL (List.map atomL args)) |> wrap
            | Expr.Op (TOp.ExnConstr (ecref),_,args,_) -> 
                wordL ecref.LogicalName ^^ bracketL (commaListL (List.map atomL args))
            | Expr.Op (TOp.Tuple,_,xs,_) -> 
                tupleL (List.map exprL xs)
            | Expr.Op (TOp.Recd (ctor,tc),_,xs,_)               -> 
                let fields = tc.TrueInstanceFieldsAsList
                let lay fs x = (wordL fs.rfield_id.idText ^^ sepL "=") --- (exprL x)
                let ctorL = 
                    match ctor with
                    | RecdExpr             -> emptyL
                    | RecdExprIsObjInit-> wordL "(new)"
                leftL "{" ^^ semiListL (List.map2 lay fields xs) ^^ rightL "}" ^^ ctorL
            | Expr.Op (TOp.ValFieldSet rf,_,[rx;x],_) -> 
                (atomL rx --- wordL ".") ^^ (recdFieldRefL rf ^^ wordL "<-" --- exprL x)
            | Expr.Op (TOp.ValFieldSet rf,_,[x],_) -> 
                (recdFieldRefL rf ^^ wordL "<-" --- exprL x)
            | Expr.Op (TOp.ValFieldGet rf,_,[rx],_) -> 
                (atomL rx ^^ rightL ".#" ^^ recdFieldRefL rf)
            | Expr.Op (TOp.ValFieldGet rf,_,[],_) -> 
                recdFieldRefL rf
            | Expr.Op (TOp.ValFieldGetAddr rf,_,[rx],_) -> 
                leftL "&" ^^ bracketL (atomL rx ^^ rightL ".!" ^^ recdFieldRefL rf)
            | Expr.Op (TOp.ValFieldGetAddr rf,_,[],_) -> 
                leftL "&" ^^ (recdFieldRefL rf)
            | Expr.Op (TOp.UnionCaseTagGet tycr,_,[x],_) -> 
                wordL ("#" ^ tycr.LogicalName ^ ".tag") ^^ atomL x
            | Expr.Op (TOp.UnionCaseProof c,_,[x],_) -> 
                wordL ("#" ^ c.CaseName^ ".cast") ^^ atomL x
            | Expr.Op (TOp.UnionCaseFieldGet (c,i),_,[x],_) -> 
                wordL ("#" ^ c.CaseName ^ "." ^ string i) --- atomL x
            | Expr.Op (TOp.UnionCaseFieldSet (c,i),_,[x;y],_) -> 
                ((atomL x --- (rightL ("#" ^ c.CaseName ^ "." ^ string i))) ^^ wordL ":=") --- exprL y
            | Expr.Op (TOp.TupleFieldGet i,_,[x],_) -> 
                wordL ("#" ^ string i) --- atomL x
            | Expr.Op (TOp.Coerce,[typ;_],[x],_) -> 
                atomL x --- (wordL ":>" ^^ typeL typ) 
            | Expr.Op (TOp.Reraise,[_],[],_) -> 
                wordL "Rethrow!"
            | Expr.Op (TOp.ILAsm (a,tys),tyargs,args,_)      -> 
                let instrs = a |> List.map (sprintf "%+A" >> wordL) |> spaceListL // %+A has + since instrs are from an "internal" type  
                let instrs = leftL "(#" ^^ instrs ^^ rightL "#)"
                (appL instrs tyargs args ---
                    wordL ":" ^^ spaceListL (List.map typeAtomL tys)) |> wrap
            | Expr.Op (TOp.LValueOp (lvop,vr),_,args,_) -> 
                (lvalopL lvop ^^ valRefL vr --- bracketL (commaListL (List.map atomL args))) |> wrap
            | Expr.Op (TOp.ILCall (_isVirtCall,_isProtectedCall,_valu,_isNewObjCall,_valUseFlags,_isProperty,_isDllImport,ilMethRef,tinst,minst,_tys),tyargs,args,_) ->
                let meth = ilMethRef.Name
                wordL "ILCall" ^^ aboveListL [wordL "meth  " --- wordL ilMethRef.EnclosingTypeRef.FullName ^^ sepL "." ^^ wordL meth;
                                              wordL "tinst " --- listL typeL tinst;
                                              wordL "minst " --- listL typeL minst;
                                              wordL "tyargs" --- listL typeL tyargs;
                                              wordL "args  " --- listL exprL args] |> wrap
            | Expr.Op (TOp.Array,[_],xs,_) -> 
                leftL "[|" ^^ commaListL (List.map exprL xs) ^^ rightL "|]"
            | Expr.Op (TOp.While _,[],[x1;x2],_) -> 
                wordL "while" ^^ exprL x1 ^^ wordL "do" ^^ exprL x2 ^^ rightL "}"
            | Expr.Op (TOp.For _,[],[x1;x2;x3],_) -> 
                wordL "for" ^^ aboveListL [(exprL x1 ^^ wordL "to" ^^ exprL x2 ^^ wordL "do"); exprL x3 ] ^^ rightL "done"
            | Expr.Op (TOp.TryCatch _,[_],[x1;x2],_) -> 
                wordL "try" ^^ exprL x1 ^^ wordL "with" ^^ exprL x2 ^^ rightL "}"
            | Expr.Op (TOp.TryFinally _,[_],[x1;x2],_) -> 
                wordL "try" ^^ exprL x1 ^^ wordL "finally" ^^ exprL x2 ^^ rightL "}"
            | Expr.Op (TOp.Bytes _,_ ,_ ,_) -> 
                wordL "bytes++"       
            | Expr.Op (TOp.UInt16s _,_ ,_ ,_)                 -> wordL "uint16++"       
            | Expr.Op (TOp.RefAddrGet,_tyargs,_args,_)      -> wordL "GetRefLVal..."
            | Expr.Op (TOp.TraitCall _,_tyargs,_args,_)      -> wordL "traitcall..."
            | Expr.Op (TOp.ExnFieldGet _,_tyargs,_args,_) -> wordL "TOp.ExnFieldGet..."
            | Expr.Op (TOp.ExnFieldSet _,_tyargs,_args,_) -> wordL "TOp.ExnFieldSet..."
            | Expr.Op (TOp.TryFinally _,_tyargs,_args,_) -> wordL "TOp.TryFinally..."
            | Expr.Op (TOp.TryCatch  _,_tyargs,_args,_) -> wordL "TOp.TryCatch..."
            | Expr.Op (_,_tys,args,_)                        -> wordL "Expr.Op ..." ^^ bracketL (commaListL (List.map atomL args)) 
            | Expr.Quote (a,_,_,_)                       -> leftL "<@" ^^ atomL a ^^ rightL "@>"
            | Expr.Obj (_lambdaId,typ,basev,ccall,overrides,iimpls,_)              -> 
                wordL "OBJ:" ^^ aboveListL [typeL typ;
                                            exprL ccall;
                                            optionL vspecAtBindL basev;
                                            aboveListL (List.map overrideL overrides);
                                            aboveListL (List.map iimplL iimpls)]

            | Expr.StaticOptimization (_tcs,csx,x,_)       -> 
                (wordL "opt" @@- (exprL x)) @@--
                   (wordL "|" ^^ exprL csx --- (wordL "when..." ))
           
        // For tracking ranges through expr rewrites 
        if !layoutRanges 
        then leftL "{" ^^ (rangeL expr.Range ^^ rightL ":") ++ lay ^^ rightL "}"
        else lay

    and assemblyL (TAssembly(implFiles)) = 
        aboveListL (List.map implFileL implFiles)
    
    and appL flayout tys args =
        let z = flayout
        let z = z ^^ instL typeL tys
        let z = z --- sepL "`" --- (spaceListL (List.map atomL args))
        z
       
    and implFileL (TImplFile(_,_,e,_,_)) =
        aboveListL [(wordL "top implementation ") @@-- mexprL e]

    and mexprL x =
        match x with 
        | ModuleOrNamespaceExprWithSig(mtyp,defs,_) -> mdefL defs  @@- (wordL ":"  @@-  entityTypeL mtyp)
    and mdefsL defs = wordL "Module Defs" @@-- aboveListL(List.map mdefL defs)
    and mdefL x = 
        match x with 
        | TMDefRec(tycons ,binds,mbinds,_) ->  aboveListL ((tycons |> List.map tyconL) @ [letRecL binds emptyL] @ List.map mbindL mbinds)
        | TMDefLet(bind,_) -> letL bind emptyL
        | TMDefDo(e,_) -> exprL e
        | TMDefs defs -> mdefsL defs; 
        | TMAbstract mexpr -> mexprL mexpr
    and mbindL (ModuleOrNamespaceBinding(mspec, rhs)) =
        (wordL (if mspec.IsNamespace then "namespace" else "module") ^^ (wordL mspec.DemangledModuleOrNamespaceName |> stampL mspec.Stamp)) @@-- mdefL rhs 

    and entityTypeL (mtyp:ModuleOrNamespaceType) =
        aboveListL [jlistL typeOfValL mtyp.AllValsAndMembers;
                    jlistL tyconL  mtyp.AllEntities;]    

    and entityL (ms:ModuleOrNamespace) =
        let header = wordL "module" ^^ (wordL  ms.DemangledModuleOrNamespaceName |> stampL ms.Stamp) ^^ wordL ":"
        let footer = wordL "end"
        let body = entityTypeL ms.ModuleOrNamespaceType
        (header @@-- body) @@ footer

    and ccuL     (ccu:CcuThunk) = entityL ccu.Contents

    and decisionTreeL x = 
        match x with 
        | TDBind (bind,body)            -> let bind = wordL "let" ^^ bindingL bind ^^ wordL "in" in (bind @@ decisionTreeL body) 
        | TDSuccess (args,n)            -> wordL "Success" ^^ leftL "T" ^^ intL n ^^ tupleL (args |> FlatList.toList |> List.map exprL)
        | TDSwitch (test,dcases,dflt,_) -> (wordL "Switch" --- exprL test) @@--
                                            (aboveListL (List.map dcaseL dcases) @@
                                             match dflt with
                                               None       -> emptyL
                                             | Some dtree -> wordL "dflt:" --- decisionTreeL dtree)

    and dcaseL (TCase (test,dtree)) = (dtestL test ^^ wordL "//") --- decisionTreeL dtree

    and dtestL x = 
        match x with 
        |  (Test.UnionCase (c,tinst)) -> wordL "is" ^^ unionCaseRefL c ^^ instL typeL tinst
        |  (Test.ArrayLength (n,ty)) -> wordL "length" ^^ intL n ^^ typeL ty
        |  (Test.Const       c        ) -> wordL "is" ^^ NicePrint.constL c
        |  (Test.IsNull               ) -> wordL "isnull"
        |  (Test.IsInst (_,typ)           ) -> wordL "isinst" ^^ typeL typ
        |  (Test.ActivePatternCase (exp,_,_,_,_)) -> wordL "query" ^^ exprL exp
            
    and targetL i (TTarget (argvs,body,_)) = leftL "T" ^^ intL i ^^ tupleL (flatValsL argvs) ^^ rightL ":" --- exprL body
    and flatValsL vs = vs |> FlatList.toList |> List.map valL

    and tmethodL (TObjExprMethod(TSlotSig(nm,_,_,_,_,_), _, tps, vs, e, _)) =
        (wordL "TObjExprMethod" --- (wordL nm) ^^ wordL "=") --
          (wordL "METH-LAM" --- angleBracketListL (List.map typarL       tps) ^^ rightL ".") ---
          (wordL "meth-lam" --- tupleL (List.map (List.map vspecAtBindL >> tupleL) vs)  ^^ rightL ".") ---
          (atomL e) 
    and overrideL tmeth     = wordL "with" ^^ tmethodL tmeth 
    and iimplL (typ,tmeths) = wordL "impl" ^^ aboveListL (typeL typ :: List.map tmethodL tmeths) 

    let showType x = Layout.showL (typeL x)
    let showExpr x = Layout.showL (exprL x)
    let traitL x = auxTraitL SimplifyTypes.typeSimplificationInfo0 x
    let typarsL   x = typarDeclsL x

end

let valRefL    x = DebugPrint.valRefL x
let unionCaseRefL   x = DebugPrint.unionCaseRefL x
let intL     x = DebugPrint.intL x
let valL   x = DebugPrint.valL x
let typarL   x = DebugPrint.typarL x
let typarDeclL   x = DebugPrint.typarDeclL x
let typarsL   x = DebugPrint.typarDeclsL x
let typeL    x = DebugPrint.typeL x
let slotSigL x = DebugPrint.slotSigL x
let entityTypeL   x = DebugPrint.entityTypeL x
let entityL   x = DebugPrint.entityL x
let typeOfValL x = DebugPrint.typeOfValL x
let bindingL    x = DebugPrint. bindingL x
let exprL    x = DebugPrint.exprL x
let decisionTreeL    x = DebugPrint.decisionTreeL x
let tyconL    x = DebugPrint.tyconL x
let implFileL  x = DebugPrint.implFileL x
let assemblyL  x = DebugPrint.assemblyL x
let vspecAtBindL x = DebugPrint.vspecAtBindL x
let recdFieldRefL x = DebugPrint.recdFieldRefL x
let traitL x = DebugPrint.auxTraitL SimplifyTypes.typeSimplificationInfo0 x
#endif // SILVERLIGHT


//--------------------------------------------------------------------------
// Helpers related to type checking modules & namespaces
//--------------------------------------------------------------------------

let wrapModuleOrNamespaceType id cpath mtyp = 
    NewModuleOrNamespace (Some cpath)  taccessPublic  id  XmlDoc.Empty  [] (notlazy mtyp)

let wrapModuleOrNamespaceTypeInNamespace id cpath (mtyp:ModuleOrNamespaceType) = 
    let mspec = NewModuleOrNamespace (Some cpath) taccessPublic id  XmlDoc.Empty [] (notlazy mtyp)
    NewModuleOrNamespaceType Namespace [ mspec ] []

let wrapModuleOrNamespaceExprInNamespace (id :Ident) cpath mexpr = 
    let mspec = NewModuleOrNamespace (Some cpath) taccessPublic id XmlDoc.Empty [] (notlazy (NewEmptyModuleOrNamespaceType Namespace))
    TMDefRec ([],FlatList.empty,[ModuleOrNamespaceBinding(mspec, mexpr)],id.idRange)

// cleanup: make this a property
let SigTypeOfImplFile (TImplFile(_,_,mexpr,_,_)) = mexpr.Type 

//--------------------------------------------------------------------------
// Data structures representing what gets hidden and what gets remapped (i.e. renamed or alpha-converted)
// when a module signature is applied to a module.
//--------------------------------------------------------------------------

type SignatureRepackageInfo = 
    { mrpiVals  : (ValRef * ValRef) list;
      mrpiEntities: (TyconRef * TyconRef) list  }
    
    member remapInfo.ImplToSigMapping = { TypeEquivEnv.Empty with EquivTycons = TyconRefMap.OfList remapInfo.mrpiEntities }
    static member Empty = { mrpiVals = []; mrpiEntities= [] } 

type SignatureHidingInfo = 
    { mhiTycons     : Zset<Tycon>; 
      mhiTyconReprs : Zset<Tycon>;  
      mhiVals       : Zset<Val>; 
      mhiRecdFields : Zset<RecdFieldRef>; 
      mhiUnionCases : Zset<UnionCaseRef> }

    static member Empty = 
        { mhiTycons      = Zset.empty tyconOrder; 
          mhiTyconReprs  = Zset.empty tyconOrder;  
          mhiVals        = Zset.empty valOrder; 
          mhiRecdFields  = Zset.empty recdFieldRefOrder; 
          mhiUnionCases  = Zset.empty unionCaseRefOrder }

let addValRemap v v' tmenv = 
    { tmenv with valRemap= tmenv.valRemap.Add v (mkLocalValRef v')  }

let mkRepackageRemapping mrpi = 
    { valRemap = ValMap.OfList (mrpi.mrpiVals |> List.map (fun (vref,x) -> vref.Deref, x));
      tpinst = emptyTyparInst; 
      tyconRefRemap = TyconRefMap.OfList mrpi.mrpiEntities }

//--------------------------------------------------------------------------
// Compute instances of the above for mty -> mty
//--------------------------------------------------------------------------

let accEntityRemap (msigty:ModuleOrNamespaceType) (tycon:Tycon) (mrpi,mhi) =
    let sigtyconOpt = (NameMap.tryFind tycon.LogicalName msigty.AllEntitiesByCompiledAndLogicalMangledNames)
    match sigtyconOpt with 
    | None -> 
        // The type constructor is not present in the signature. Hence it is hidden. 
        let mhi = { mhi with mhiTycons = Zset.add tycon mhi.mhiTycons }
        (mrpi,mhi) 
    | Some sigtycon  -> 
        // The type constructor is in the signature. Hence record the repackage entry 
        let sigtcref = mkLocalTyconRef sigtycon
        let tcref = mkLocalTyconRef tycon
        let mrpi = { mrpi with mrpiEntities = ((tcref, sigtcref) :: mrpi.mrpiEntities) }
        // OK, now look for hidden things 
        let mhi = 
            if isSome tycon.TypeReprInfo && isNone sigtycon.TypeReprInfo then 
                // The type representation is absent in the signature, hence it is hidden 
                { mhi with mhiTyconReprs = Zset.add tycon mhi.mhiTyconReprs } 
            else 
                // The type representation is present in the signature. 
                // Find the fields that have been hidden or which were non-public anyway. 
                mhi 
                |> Array.foldBack  (fun (rfield:RecdField) mhi ->
                            match sigtycon.GetFieldByName(rfield.Name) with 
                            | Some _  -> 
                                // The field is in the signature. Hence it is not hidden. 
                                mhi
                            | _ -> 
                                // The field is not in the signature. Hence it is regarded as hidden. 
                                let rfref = mkNestedRecdFieldRef tcref rfield
                                { mhi with mhiRecdFields =  Zset.add rfref mhi.mhiRecdFields })
                        tycon.AllFieldsArray
                |> List.foldBack  (fun (ucase:UnionCase) mhi ->
                            match sigtycon.GetUnionCaseByName ucase.DisplayName with 
                            | Some _  -> 
                                // The constructor is in the signature. Hence it is not hidden. 
                                mhi
                            | _ -> 
                                // The constructor is not in the signature. Hence it is regarded as hidden. 
                                let ucref = mkNestedUnionCaseRef tcref ucase
                                { mhi with mhiUnionCases =  Zset.add ucref mhi.mhiUnionCases })
                        (tycon.UnionCasesAsList)  
        (mrpi,mhi) 

let accSubEntityRemap (msigty:ModuleOrNamespaceType) (tycon:Tycon) (mrpi,mhi) =
    let sigtyconOpt = (NameMap.tryFind tycon.LogicalName msigty.AllEntitiesByCompiledAndLogicalMangledNames)
    match sigtyconOpt with 
    | None -> 
        // The type constructor is not present in the signature. Hence it is hidden. 
        let mhi = { mhi with mhiTycons = Zset.add tycon mhi.mhiTycons }
        (mrpi,mhi) 
    | Some sigtycon  -> 
        // The type constructor is in the signature. Hence record the repackage entry 
        let sigtcref = mkLocalTyconRef sigtycon
        let tcref = mkLocalTyconRef tycon
        let mrpi = { mrpi with mrpiEntities = ((tcref, sigtcref) :: mrpi.mrpiEntities) }
        (mrpi,mhi) 

let valLinkageAEquiv g aenv (v1:Val) (v2:Val) = 
    (v1.LinkagePartialKey = v2.LinkagePartialKey) &&
    (if v1.IsMember && v2.IsMember then typeAEquivAux EraseAll g aenv v1.Type v2.Type else true)
    
let accValRemap g aenv (msigty:ModuleOrNamespaceType) (implVal:Val) (mrpi,mhi) =
    let sigValOpt = 
        msigty.AllValsAndMembersByPartialLinkageKey 
          |> MultiMap.find implVal.LinkagePartialKey 
          |> List.tryFind (fun sigVal -> valLinkageAEquiv g aenv implVal sigVal)
          
    let vref = mkLocalValRef implVal
    match sigValOpt with 
    | None -> 
        if verbose then dprintf "accValRemap, hide = %s#%d\n" implVal.LogicalName implVal.Stamp
        let mhi = { mhi with mhiVals = Zset.add implVal mhi.mhiVals }
        (mrpi,mhi) 
    | Some (sigVal:Val)  -> 
        // The value is in the signature. Add the repackage entry. 
#if DEBUG
        if !verboseStamps then dprintf "accValRemap, remap value %s#%d --> %s#%d\n" implVal.LogicalName implVal.Stamp sigVal.LogicalName sigVal.Stamp; 
#endif
      
        let mrpi = { mrpi with mrpiVals = (vref,mkLocalValRef sigVal) :: mrpi.mrpiVals }
        (mrpi,mhi) 

let getCorrespondingSigTy nm (msigty:ModuleOrNamespaceType) = 
    match NameMap.tryFind nm msigty.AllEntitiesByCompiledAndLogicalMangledNames with 
    | None -> NewEmptyModuleOrNamespaceType FSharpModule 
    | Some sigsubmodul -> sigsubmodul.ModuleOrNamespaceType

let rec accEntityRemapFromModuleOrNamespaceType (mty:ModuleOrNamespaceType) (msigty:ModuleOrNamespaceType) acc = 
    let acc = (mty.ModuleAndNamespaceDefinitions, acc) ||> List.foldBack (fun submodul acc -> accEntityRemapFromModuleOrNamespaceType submodul.ModuleOrNamespaceType (getCorrespondingSigTy submodul.LogicalName msigty) acc) 
    let acc = QueueList.foldBack (accEntityRemap msigty) mty.AllEntities acc
    acc 

let rec accValRemapFromModuleOrNamespaceType g aenv (mty:ModuleOrNamespaceType) msigty acc = 
    let acc = (mty.ModuleAndNamespaceDefinitions, acc) ||> List.foldBack (fun submodul acc -> accValRemapFromModuleOrNamespaceType g aenv submodul.ModuleOrNamespaceType (getCorrespondingSigTy submodul.LogicalName msigty) acc) 
    let acc = QueueList.foldBack (accValRemap g aenv msigty) mty.AllValsAndMembers acc
    acc 

let ComputeRemappingFromInferredSignatureToExplicitSignature g mty msigty = 
    // dprintf "ComputeRemappingFromInferredSignatureToExplicitSignature,\nmty = %s\nmmsigty=%s\n" (showL(entityTypeL mty)) (showL(entityTypeL msigty)); 
    let ((mrpi,_) as entityRemap) = accEntityRemapFromModuleOrNamespaceType mty msigty (SignatureRepackageInfo.Empty, SignatureHidingInfo.Empty)  
    let aenv = mrpi.ImplToSigMapping
    let valAndEntityRemap = accValRemapFromModuleOrNamespaceType g aenv mty msigty entityRemap
    valAndEntityRemap 

//--------------------------------------------------------------------------
// Compute instances of the above for mexpr -> mty
//--------------------------------------------------------------------------

/// At TMDefRec nodes abstract (virtual) vslots are effectively binders, even 
/// though they are tucked away inside the tycon. This helper function extracts the
/// virtual slots to aid with finding this babies.
let abstractSlotValsOfTycons (tycons:Tycon list) =  
    tycons 
    |> List.collect (fun tycon -> if tycon.IsFSharpObjectModelTycon then tycon.FSharpObjectModelTypeInfo.fsobjmodel_vslots else []) 
    |> List.map (fun v -> v.Deref)

let rec accEntityRemapFromModuleOrNamespace msigty x acc = 
    match x with 
    | TMDefRec(tycons,_,mbinds,_) -> 
         List.foldBack (accEntityRemap msigty) tycons 
            (List.foldBack (accEntityRemapFromModuleOrNamespaceBind msigty) mbinds acc)
    | TMDefLet _  -> acc
    | TMDefDo _  -> acc
    | TMDefs defs -> accEntityRemapFromModuleOrNamespaceDefs msigty defs acc
    | TMAbstract mexpr -> accEntityRemapFromModuleOrNamespaceType mexpr.Type msigty acc
and accEntityRemapFromModuleOrNamespaceDefs msigty mdefs acc = List.foldBack (accEntityRemapFromModuleOrNamespace msigty) mdefs acc
and accEntityRemapFromModuleOrNamespaceBind msigty (ModuleOrNamespaceBinding(mspec, def)) acc =
    accSubEntityRemap msigty mspec (accEntityRemapFromModuleOrNamespace (getCorrespondingSigTy mspec.LogicalName msigty) def acc)


let rec accValRemapFromModuleOrNamespace g aenv msigty x acc = 
    match x with 
    | TMDefRec(tycons,binds,mbinds,_) -> 
         //  Abstract (virtual) vslots in the tycons at TMDefRec nodes are binders. They also need to be added to the remapping. 
         let vslotvs = abstractSlotValsOfTycons tycons
         List.foldBack (accValRemap g aenv msigty) vslotvs 
            (FlatList.foldBack (valOfBind >> accValRemap g aenv msigty) binds 
                (List.foldBack (accValRemapFromModuleOrNamespaceBind g aenv msigty) mbinds acc))
    | TMDefLet(bind,_)  -> accValRemap g aenv msigty bind.Var acc
    | TMDefDo _  -> acc
    | TMDefs defs -> accValRemapFromModuleOrNamespaceDefs g aenv msigty defs acc
    | TMAbstract mexpr -> accValRemapFromModuleOrNamespaceType g aenv mexpr.Type msigty acc
and accValRemapFromModuleOrNamespaceBind g aenv msigty (ModuleOrNamespaceBinding(mspec, def)) acc =
    accSubEntityRemap msigty mspec (accValRemapFromModuleOrNamespace g aenv (getCorrespondingSigTy mspec.LogicalName msigty) def acc)

and accValRemapFromModuleOrNamespaceDefs g aenv msigty mdefs acc = List.foldBack (accValRemapFromModuleOrNamespace g aenv msigty) mdefs acc

let ComputeRemappingFromImplementationToSignature g mdef msigty =  
    //if verbose then dprintf "ComputeRemappingFromImplementationToSignature,\nmdefs = %s\nmsigty=%s\n" (showL(DebugPrint.mdefL mdef)) (showL(DebugPrint.entityTypeL msigty));
    let ((mrpi,_) as entityRemap) = accEntityRemapFromModuleOrNamespace msigty mdef (SignatureRepackageInfo.Empty, SignatureHidingInfo.Empty) 
    let aenv = mrpi.ImplToSigMapping
    
    let valAndEntityRemap = accValRemapFromModuleOrNamespace g aenv msigty mdef entityRemap
    valAndEntityRemap

//--------------------------------------------------------------------------
// Compute instances of the above for the assembly boundary
//--------------------------------------------------------------------------

let accTyconHidingInfoAtAssemblyBoundary (tycon:Tycon) mhi =
    if not (canAccessFromEverywhere tycon.Accessibility) then 
        // The type constructor is not public, hence hidden at the assembly boundary. 
        { mhi with mhiTycons = Zset.add tycon mhi.mhiTycons } 
    elif not (canAccessFromEverywhere tycon.TypeReprAccessibility) then 
        { mhi with mhiTyconReprs = Zset.add tycon mhi.mhiTyconReprs } 
    else 
        mhi 
        |> Array.foldBack  
               (fun (rfield:RecdField) mhi ->
                   if not (canAccessFromEverywhere rfield.Accessibility) then 
                       let tcref = mkLocalTyconRef tycon
                       let rfref = mkNestedRecdFieldRef tcref rfield
                       { mhi with mhiRecdFields = Zset.add rfref mhi.mhiRecdFields } 
                   else mhi)
               tycon.AllFieldsArray  
        |> List.foldBack  
               (fun (ucase:UnionCase) mhi ->
                   if not (canAccessFromEverywhere ucase.Accessibility) then 
                       let tcref = mkLocalTyconRef tycon
                       let ucref = mkNestedUnionCaseRef tcref ucase
                       { mhi with mhiUnionCases = Zset.add ucref mhi.mhiUnionCases } 
                   else mhi)
               (tycon.UnionCasesAsList)   

// Collect up the values hidden at the assembly boundary. This is used by IsHiddenVal to 
// determine if something is considered hidden. This is used in turn to eliminate optimization
// information at the assembly boundary and to decide to label things as "internal".
let accValHidingInfoAtAssemblyBoundary (vspec:Val) mhi =
    if // anything labelled "internal" or more restrictive is considered to be hidden at the assembly boundary
       not (canAccessFromEverywhere vspec.Accessibility) || 
       // compiler generated members for class function 'let' bindings are considered to be hidden at the assembly boundary
       vspec.IsIncrClassGeneratedMember ||                     
       // anything that's not a module or member binding gets assembly visibility
       not vspec.IsMemberOrModuleBinding then 
        // The value is not public, hence hidden at the assembly boundary. 
        { mhi with mhiVals = Zset.add vspec mhi.mhiVals } 
    else 
        mhi

let rec accModuleOrNamespaceHidingInfoAtAssemblyBoundary mty acc = 
    let acc = List.foldBack (fun (submodul:ModuleOrNamespace) acc -> accModuleOrNamespaceHidingInfoAtAssemblyBoundary submodul.ModuleOrNamespaceType acc) mty.ModuleAndNamespaceDefinitions acc
    let acc = QueueList.foldBack accTyconHidingInfoAtAssemblyBoundary mty.AllEntities acc
    let acc = QueueList.foldBack accValHidingInfoAtAssemblyBoundary mty.AllValsAndMembers acc
    acc 

let ComputeHidingInfoAtAssemblyBoundary mty = 
//     dprintf "ComputeRemappingFromInferredSignatureToExplicitSignature,\nmty = %s\nmmsigty=%s\n" (showL(entityTypeL mty)) (showL(entityTypeL msigty)); 
    accModuleOrNamespaceHidingInfoAtAssemblyBoundary mty SignatureHidingInfo.Empty

//--------------------------------------------------------------------------
// Compute instances of the above for mexpr -> mty
//--------------------------------------------------------------------------

let IsHidden setF accessF remapF debugF = 
    let rec check mrmi x = 
        if verbose then dprintf "IsHidden %s ??\n" (showL (debugF x));
            // Internal/private? 
        not (canAccessFromEverywhere (accessF x)) || 
        (match mrmi with 
         | [] -> false // Ah! we escaped to freedom! 
         | (rpi,mhi) :: rest -> 
            // Explicitly hidden? 
            Zset.contains x (setF mhi) || 
            // Recurse... 
            check rest (remapF rpi x))
    fun mrmi x -> 
        let res = check mrmi x
        if verbose then dprintf "IsHidden, #mrmi = %d, %s = %b\n" mrmi.Length (showL (debugF x)) res;
        res
        
let IsHiddenTycon     mrmi x = IsHidden (fun mhi -> mhi.mhiTycons)     (fun tc -> tc.Accessibility)        (fun rpi x ->  (remapTyconRef rpi.tyconRefRemap (mkLocalTyconRef x)).Deref) DebugPrint.tyconL mrmi x 
let IsHiddenTyconRepr mrmi x = IsHidden (fun mhi -> mhi.mhiTyconReprs) (fun v -> v.TypeReprAccessibility)  (fun rpi x ->  (remapTyconRef rpi.tyconRefRemap (mkLocalTyconRef x)).Deref) DebugPrint.tyconL mrmi x 
let IsHiddenVal       mrmi x = IsHidden (fun mhi -> mhi.mhiVals)       (fun v -> v.Accessibility)          (fun rpi x ->  (remapValRef rpi (mkLocalValRef x)).Deref) DebugPrint.valL mrmi x 
let IsHiddenRecdField mrmi x = IsHidden (fun mhi -> mhi.mhiRecdFields) (fun rfref -> rfref.RecdField.Accessibility) (fun rpi x ->  remapRecdFieldRef rpi.tyconRefRemap x) DebugPrint.recdFieldRefL mrmi x 


//--------------------------------------------------------------------------
// Generic operations on module types
//--------------------------------------------------------------------------

let foldModuleOrNamespaceTy ft fv mty acc = 
    let rec go mty acc = 
        let acc = NameMap.foldBackRange (fun (mspec:ModuleOrNamespace) acc -> go mspec.ModuleOrNamespaceType acc) mty.ModulesAndNamespacesByDemangledName acc
        let acc = QueueList.foldBack ft mty.AllEntities acc
        let acc = QueueList.foldBack fv mty.AllValsAndMembers acc
        acc
    go mty acc

let allValsOfModuleOrNamespaceTy m = foldModuleOrNamespaceTy (fun _ acc -> acc) (fun v acc -> v :: acc) m []
let allEntitiesOfModuleOrNamespaceTy m = foldModuleOrNamespaceTy (fun ft acc -> ft :: acc) (fun _ acc -> acc) m []

//---------------------------------------------------------------------------
// Free variables in terms.  Are all constructs public accessible?
//---------------------------------------------------------------------------
 
let isPublicVal (lv:Val)                 = (lv.Accessibility = taccessPublic)
let isPublicUnionCase (ucr:UnionCaseRef) = (ucr.UnionCase.Accessibility = taccessPublic)
let isPublicRecdField (rfr:RecdFieldRef) = (rfr.RecdField.Accessibility = taccessPublic)
let isPublicTycon (tcr:Tycon)            = (tcr.Accessibility = taccessPublic)

let freeVarsAllPublic fvs = 
    // Are any non-public items used in the expr (which corresponded to the fvs)?
    // Recall, taccess occurs in:
    //      EntityData     has     ReprAccessibility and Accessiblity
    //      UnionCase    has     Accessibility
    //      RecdField      has     Accessibility
    //      ValData       has     Accessibility
    // The freevars and FreeTyvars collect local constructs.
    // Here, we test that all those constructs are public.
    //
    Zset.forall isPublicVal fvs.FreeLocals  &&
    Zset.forall isPublicUnionCase fvs.FreeUnionCases &&
    Zset.forall isPublicRecdField fvs.FreeRecdFields  &&
    Zset.forall isPublicTycon fvs.FreeTyvars.FreeTycons

let freeTyvarsAllPublic tyvars = 
    Zset.forall isPublicTycon tyvars.FreeTycons

//---------------------------------------------------------------------------
// Free variables in terms.  All binders are distinct.
//---------------------------------------------------------------------------

let emptyFreeVars =  
  { UsesMethodLocalConstructs=false;
    UsesUnboundRethrow=false;
    FreeLocalTyconReprs=emptyFreeTycons; 
    FreeLocals=emptyFreeLocals; 
    FreeTyvars=emptyFreeTyvars;
    FreeRecdFields = emptyFreeRecdFields;
    FreeUnionCases = emptyFreeUnionCases}

let unionFreeVars fvs1 fvs2 = 
  if fvs1 === emptyFreeVars then fvs2 else 
  if fvs2 === emptyFreeVars then fvs1 else
  { FreeLocals                    = unionFreeLocals fvs1.FreeLocals fvs2.FreeLocals;
    FreeTyvars                    = unionFreeTyvars fvs1.FreeTyvars fvs2.FreeTyvars;    
    UsesMethodLocalConstructs     = fvs1.UsesMethodLocalConstructs || fvs2.UsesMethodLocalConstructs;
    UsesUnboundRethrow            = fvs1.UsesUnboundRethrow || fvs2.UsesUnboundRethrow;
    FreeLocalTyconReprs           = unionFreeTycons fvs1.FreeLocalTyconReprs fvs2.FreeLocalTyconReprs; 
    FreeRecdFields                = unionFreeRecdFields fvs1.FreeRecdFields fvs2.FreeRecdFields; 
    FreeUnionCases                = unionFreeUnionCases fvs1.FreeUnionCases fvs2.FreeUnionCases; }

let inline accFreeTyvars (opts:FreeVarOptions) f v acc =
    if not opts.collectInTypes then acc else
    let ftyvs = acc.FreeTyvars
    let ftyvs' = f opts v ftyvs
    if ftyvs === ftyvs' then acc else 
    { acc with FreeTyvars = ftyvs' }

#if FREEVARS_IN_TYPES_ANALYSIS
type CheckCachability<'key,'acc>(name,f: FreeVarOptions -> 'key -> 'acc -> bool * 'acc) =
    let dict = System.Collections.Generic.Dictionary<'key,int>(HashIdentity.Reference)
    let idem = System.Collections.Generic.Dictionary<'key,int>(HashIdentity.Reference)
    let closed = System.Collections.Generic.Dictionary<'key,int>(HashIdentity.Reference)
    let mutable saved = 0
    do System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
        let hist = dict |> Seq.groupBy (fun (KeyValue(k,v)) -> v) |> Seq.map (fun (n,els) -> (n,Seq.length els)) |> Seq.sortBy (fun (n,_) -> n)
        let total = hist |> Seq.sumBy (fun (nhits,nels) -> nels)
        let totalHits = hist |> Seq.sumBy (fun (nhits,nels) -> nhits * nels)
        printfn "*** %s saved %d hits (%g%%) ***" name saved (float  saved / float (saved + totalHits) * 100.0)
        printfn "*** %s had %d hits total, possible saving %d ***" name totalHits (totalHits - total)
        //for (nhits,nels) in hist do 
        //    printfn "%s, %g%% els for %g%% hits had %d hits" name (float nels / float total * 100.0) (float (nels * nhits) / float totalHits * 100.0) nhits

        let hist = idem |> Seq.groupBy (fun (KeyValue(k,v)) -> v) |> Seq.map (fun (n,els) -> (n,Seq.length els)) |> Seq.sortBy (fun (n,_) -> n)
        let total = hist |> Seq.sumBy (fun (nhits,nels) -> nels)
        let totalHits = hist |> Seq.sumBy (fun (nhits,nels) -> nhits * nels)
        printfn "*** %s had %d idempotent hits total, possible saving %d ***" name totalHits (totalHits - total)
        //for (nhits,nels) in hist do 
        //    printfn "%s, %g%% els for %g%% hits had %d idempotent hits" name (float nels / float total * 100.0) (float (nels * nhits) / float totalHits * 100.0) nhits

        let hist = closed |> Seq.groupBy (fun (KeyValue(k,v)) -> v) |> Seq.map (fun (n,els) -> (n,Seq.length els)) |> Seq.sortBy (fun (n,_) -> n)
        let total = hist |> Seq.sumBy (fun (nhits,nels) -> nels)
        let totalHits = hist |> Seq.sumBy (fun (nhits,nels) -> nhits * nels)
        printfn "*** %s had %d closed hits total, possible saving %d ***" name totalHits (totalHits - total)
       )
        
    member cache.Apply(opts,key,acc) = 
        if not opts.collectInTypes then 
            saved <- saved + 1
            acc 
        else
            let cls,res = f opts  key acc
            if opts.canCache then 
                if dict.ContainsKey key then 
                    dict.[key] <- dict.[key] + 1
                else
                    dict.[key] <- 1
                if res === acc then
                    if idem.ContainsKey key then 
                        idem.[key] <- idem.[key] + 1
                    else
                        idem.[key] <- 1
                if cls then
                    if closed.ContainsKey key then 
                        closed.[key] <- closed.[key] + 1
                    else
                        closed.[key] <- 1
            res
            

    //member cache.OnExit() = 

let accFreeVarsInTy_cache =  CheckCachability("accFreeVarsInTy", (fun opts ty fvs -> (freeInType opts ty === emptyFreeTyvars), accFreeTyvars opts (accFreeInType opts) ty fvs))
let accFreevarsInValCache =  CheckCachability("accFreevarsInVal", (fun opts  v fvs ->  (freeInVal opts v === emptyFreeTyvars), accFreeTyvars opts (accFreeInVal opts) v fvs))
let accFreeVarsInTys_cache =  CheckCachability("accFreeVarsInTys", (fun opts  tys fvs -> (freeInTypes opts tys === emptyFreeTyvars), accFreeTyvars opts (accFreeInTypes opts) tys fvs))
let accFreevarsInTyconCache =  CheckCachability("accFreevarsInTycon", (fun opts  tys fvs -> false,accFreeTyvars opts (accFreeTycon opts) tys fvs))

let accFreeVarsInTy opts ty fvs = accFreeVarsInTy_cache.Apply(opts,ty,fvs)
let accFreeVarsInTys opts tys fvs = 
    if isNil tys then fvs else accFreeVarsInTys_cache.Apply(opts,tys,fvs)
let accFreevarsInTycon opts (tcr:TyconRef) acc = 
    match tcr.IsLocalRef with 
    | true -> accFreevarsInTyconCache.Apply(opts,tcr,acc)
    | _ -> acc
let accFreevarsInVal opts v fvs = accFreevarsInValCache.Apply(opts,v,fvs)
#else

let accFreeVarsInTy  opts ty    acc = accFreeTyvars opts accFreeInType ty acc
let accFreeVarsInTys opts tys   acc = if isNil tys then acc else accFreeTyvars opts accFreeInTypes tys acc
let accFreevarsInTycon opts tcref acc = accFreeTyvars opts accFreeTycon tcref acc
let accFreevarsInVal   opts v     acc = accFreeTyvars opts accFreeInVal v acc
#endif
    
let accFreeVarsInTraitSln opts tys acc = accFreeTyvars opts accFreeInTraitSln tys acc 

let boundLocalVal opts v fvs =
    if not opts.includeLocals then fvs else
    let fvs = accFreevarsInVal opts v fvs
    if not (Zset.contains v fvs.FreeLocals) then fvs
    else {fvs with FreeLocals= Zset.remove v fvs.FreeLocals} 

let boundProtect fvs =
    if fvs.UsesMethodLocalConstructs then {fvs with UsesMethodLocalConstructs = false} else fvs

let accUsesFunctionLocalConstructs flg fvs = 
    if flg && not fvs.UsesMethodLocalConstructs then {fvs with UsesMethodLocalConstructs = true} 
    else fvs 

let bound_rethrow fvs =
    if fvs.UsesUnboundRethrow then {fvs with UsesUnboundRethrow = false} else fvs  

let accUsesRethrow flg fvs = 
    if flg && not fvs.UsesUnboundRethrow then {fvs with UsesUnboundRethrow = true} 
    else fvs 

let boundLocalVals opts vs fvs = List.foldBack (boundLocalVal opts) vs fvs

let bindLhs opts (bind:Binding) fvs = boundLocalVal opts bind.Var fvs

let freeVarsCacheCompute opts cache f = if opts.canCache then cached cache f else f()

let rec accBindRhs opts (TBind(_,repr,_)) acc = accFreeInExpr opts repr acc
          
and accFreeInSwitchCases opts csl dflt (acc:FreeVars) =
    Option.foldBack (accFreeInDecisionTree opts) dflt (List.foldBack (accFreeInSwitchCase opts) csl acc)
 
and accFreeInSwitchCase opts (TCase(discrim,dtree)) acc = 
    accFreeInDecisionTree opts dtree (accFreeInTest opts discrim acc)

and accFreeInTest (opts:FreeVarOptions) discrim acc = 
    match discrim with 
    | Test.UnionCase(ucref,tinst) -> accFreeUnionCaseRef opts ucref (accFreeVarsInTys opts tinst acc)
    | Test.ArrayLength(_,ty) -> accFreeVarsInTy opts ty acc
    | Test.Const _
    | Test.IsNull -> acc
    | Test.IsInst (srcty,tgty) -> accFreeVarsInTy opts srcty (accFreeVarsInTy opts tgty acc)
    | Test.ActivePatternCase (exp, tys, activePatIdentity, _, _) -> 
        accFreeInExpr opts exp 
            (accFreeVarsInTys opts tys 
                (Option.foldBack (fun (vref,tinst) acc -> accFreeValRef opts vref (accFreeVarsInTys opts tinst acc)) activePatIdentity acc))

and accFreeInDecisionTree opts x (acc : FreeVars) =
    match x with 
    | TDSwitch(e1,csl,dflt,_) -> accFreeInExpr opts e1 (accFreeInSwitchCases opts csl dflt acc)
    | TDSuccess (es,_) -> accFreeInFlatExprs opts es acc
    | TDBind (bind,body) -> unionFreeVars (bindLhs opts bind (accBindRhs opts bind (freeInDecisionTree opts body))) acc
  
and accFreeInValFlags opts flag acc =
    let isMethLocal = 
        match flag with 
        | VSlotDirectCall 
        | CtorValUsedAsSelfInit 
        | CtorValUsedAsSuperInit -> true 
        | PossibleConstrainedCall  _
        | NormalValUse -> false
    let acc = accUsesFunctionLocalConstructs isMethLocal acc
    match flag with 
    | PossibleConstrainedCall ty -> accFreeTyvars opts accFreeInType ty acc
    | _ -> acc

and accFreeLocalVal opts v fvs =
    if not opts.includeLocals then fvs else
    if Zset.contains v fvs.FreeLocals then fvs 
    else 
        let fvs = accFreevarsInVal opts v fvs
        {fvs with FreeLocals=Zset.add v fvs.FreeLocals}
  
and accLocalTyconRepr opts b fvs = 
    if not opts.includeLocalTyconReprs then fvs else
    if Zset.contains b fvs.FreeLocalTyconReprs  then fvs
    else { fvs with FreeLocalTyconReprs = Zset.add b fvs.FreeLocalTyconReprs } 

and accUsedTyconRepr opts (tc:Tycon) fvs = 
    if isSome tc.TypeReprInfo
    then accLocalTyconRepr opts tc fvs
    else fvs

and accFreeUnionCaseRef opts cr fvs =   
    if not opts.includeUnionCases then fvs else
    if Zset.contains cr fvs.FreeUnionCases then fvs 
    else
        let fvs = fvs |> accUsedTyconRepr opts cr.Tycon
        let fvs = fvs |> accFreevarsInTycon opts cr.TyconRef
        { fvs with FreeUnionCases = Zset.add cr fvs.FreeUnionCases } 

and accFreeRecdFieldRef opts fr fvs = 
    if not opts.includeRecdFields then fvs else
    if Zset.contains fr fvs.FreeRecdFields then fvs 
    else 
        let fvs = fvs |> accUsedTyconRepr opts fr.Tycon
        let fvs = fvs |> accFreevarsInTycon opts fr.TyconRef 
        { fvs with FreeRecdFields = Zset.add fr fvs.FreeRecdFields } 
  
and accFreeExnRef _exnc fvs = fvs // Note: this exnc (TyconRef) should be collected the surround types, e.g. tinst of Expr.Op 
and accFreeValRef opts (vref:ValRef) fvs = 
    match vref.IsLocalRef with 
    | true -> accFreeLocalVal opts vref.PrivateTarget fvs
    // non-local values do not contain free variables 
    | _ -> fvs

and accFreeInMethod opts (TObjExprMethod(slotsig,_attribs,tps,tmvs,e,_)) acc =
    accFreeInSlotSig opts slotsig
     (unionFreeVars (accFreeTyvars opts boundTypars tps (List.foldBack (boundLocalVals opts) tmvs (freeInExpr opts  e))) acc)

and accFreeInMethods opts methods acc = 
    List.foldBack (accFreeInMethod opts) methods acc

and accFreeInInterfaceImpl opts (ty,overrides) acc = 
    accFreeVarsInTy opts ty (accFreeInMethods opts overrides acc)

and accFreeInExpr (opts:FreeVarOptions) x acc = 
    match x with
    | Expr.Let _ -> accFreeInExprLinear opts x acc (fun e -> e)
    | _ -> accFreeInExprNonLinear opts x acc
      
and accFreeInExprLinear (opts:FreeVarOptions) x acc contf =   
    // for nested let-bindings, we need to continue after the whole let-binding is processed 
    match x with
    | Expr.Let (bind,e,_,cache) -> 
        let contf = contf << (fun free ->
          unionFreeVars (freeVarsCacheCompute opts cache (fun () -> bindLhs opts bind (accBindRhs opts bind free))) acc )
        accFreeInExprLinear opts e emptyFreeVars contf
    | _ -> 
      // No longer linear expr
      accFreeInExpr opts x acc |> contf
    
and accFreeInExprNonLinear opts x acc =
    match x with
    // BINDING CONSTRUCTS
    | Expr.Lambda (_,ctorThisValOpt,baseValOpt,vs,b,_,rty)  -> 
        unionFreeVars 
                (Option.foldBack (boundLocalVal opts) ctorThisValOpt 
                   (Option.foldBack (boundLocalVal opts) baseValOpt 
                     (boundLocalVals opts vs 
                         (accFreeVarsInTy opts rty 
                             (freeInExpr opts b)))))
            acc
    | Expr.TyLambda (_,vs,b,_,rty) ->
        unionFreeVars (accFreeTyvars opts boundTypars vs (accFreeVarsInTy opts rty (freeInExpr opts b))) acc
    | Expr.TyChoose (vs,b,_) ->
        unionFreeVars (accFreeTyvars opts boundTypars vs (freeInExpr opts b)) acc
    | Expr.LetRec (binds,e,_,cache) ->
        unionFreeVars (freeVarsCacheCompute opts cache (fun () -> FlatList.foldBack (bindLhs opts) binds (FlatList.foldBack (accBindRhs opts) binds (freeInExpr opts e)))) acc
    | Expr.Let _ -> 
        failwith "unreachable - linear expr"
    | Expr.Obj (_,typ,basev,basecall,overrides,iimpls,_)   ->  
        unionFreeVars 
           (boundProtect
              (Option.foldBack (boundLocalVal opts) basev
                (accFreeVarsInTy opts typ
                   (accFreeInExpr opts basecall
                      (accFreeInMethods opts overrides 
                         (List.foldBack (accFreeInInterfaceImpl opts) iimpls emptyFreeVars))))))
           acc  
    // NON-BINDING CONSTRUCTS 
    | Expr.Const _ -> acc
    | Expr.Val (lvr,flags,_) ->  
        accFreeInValFlags opts flags (accFreeValRef opts lvr acc)
    | Expr.Quote (ast,{contents=Some(argTypes,argExprs,_data)},_,ty) ->  
        accFreeInExpr opts ast 
            (accFreeInExprs opts argExprs
               (accFreeVarsInTys opts argTypes
                  (accFreeVarsInTy opts ty acc))) 
    | Expr.Quote (ast,{contents=None},_,ty) ->  
        accFreeInExpr opts ast (accFreeVarsInTy opts ty acc)
    | Expr.App(f0,f0ty,tyargs,args,_) -> 
        accFreeVarsInTy opts f0ty
          (accFreeInExpr opts f0
             (accFreeVarsInTys opts tyargs
                (accFreeInExprs opts args acc)))
    | Expr.Link(eref) -> accFreeInExpr opts !eref acc
    | Expr.Seq (e1,e2,_,_,_) -> 
        let acc = accFreeInExpr opts e1 acc
        // tail-call - this is required because we should be able to handle (((e1; e2); e3); e4; .... ))
        accFreeInExpr opts e2 acc 

    | Expr.StaticOptimization (_,e2,e3,_) -> accFreeInExpr opts e2 (accFreeInExpr opts e3 acc)
    | Expr.Match (_,_,dtree,targets,_,_) -> 
        accFreeInTargets opts targets (accFreeInDecisionTree opts dtree acc)
            
    //| Expr.Op (TOp.TryCatch,tinst,[Expr.Lambda(_,_,[_],e1,_,_,_); Expr.Lambda(_,_,[_],e2,_,_,_); Expr.Lambda(_,_,[_],e3,_,_,_)],_) ->
    | Expr.Op (TOp.TryCatch _,tinst,[e1;e2;e3],_) ->
        unionFreeVars 
          (accFreeVarsInTys opts tinst
            (accFreeInExprs opts [e1;e2] acc))
          (bound_rethrow (accFreeInExpr opts e3 emptyFreeVars))

    | Expr.Op (op,tinst,args,_) -> 
         let acc = accFreeInOp opts op acc
         let acc = accFreeVarsInTys opts tinst acc
         accFreeInExprs opts args acc

and accFreeInOp opts op acc =
    match op with

    // Things containing no references
    | TOp.Bytes _ 
    | TOp.UInt16s _ 
    | TOp.TryCatch _ 
    | TOp.TryFinally _ 
    | TOp.For _ 
    | TOp.Coerce 
    | TOp.RefAddrGet 
    | TOp.Tuple 
    | TOp.Array 
    | TOp.While _
    | TOp.Goto _ | TOp.Label _ | TOp.Return 
    | TOp.TupleFieldGet _ -> acc

    | TOp.UnionCaseTagGet tr -> accUsedTyconRepr opts tr.Deref acc
    
    // Things containing just a union case reference
    | TOp.UnionCaseProof cr 
    | TOp.UnionCase cr 
    | TOp.UnionCaseFieldGet (cr,_) 
    | TOp.UnionCaseFieldSet (cr,_) -> accFreeUnionCaseRef opts cr acc

    // Things containing just an exception reference
    | TOp.ExnConstr ecr 
    | TOp.ExnFieldGet (ecr,_) 
    | TOp.ExnFieldSet (ecr,_)  -> accFreeExnRef ecr acc

    | TOp.ValFieldGet fr 
    | TOp.ValFieldGetAddr fr 
    | TOp.ValFieldSet fr -> accFreeRecdFieldRef opts fr acc

    | TOp.Recd (kind,tcr) -> 
        let acc = accUsesFunctionLocalConstructs (kind = RecdExprIsObjInit) acc
        (accUsedTyconRepr opts tcr.Deref (accFreeTyvars opts accFreeTycon tcr acc)) 

    | TOp.ILAsm (_,tys) ->  accFreeVarsInTys opts tys acc
    | TOp.Reraise -> accUsesRethrow true acc

    | TOp.TraitCall(TTrait(tys,_,_,argtys,rty,sln)) -> 
        Option.foldBack (accFreeVarsInTraitSln opts) sln.Value
           (accFreeVarsInTys opts tys 
             (accFreeVarsInTys opts argtys 
               (Option.foldBack (accFreeVarsInTy opts) rty acc)))

    | TOp.LValueOp (_,lvr) -> 
        accFreeValRef opts lvr acc

    | TOp.ILCall (_,isProtectedCall,_,_,valUseFlags,_,_,_,enclTypeArgs,methTypeArgs,tys) ->
       accFreeVarsInTys opts enclTypeArgs 
         (accFreeVarsInTys opts methTypeArgs  
           (accFreeInValFlags opts valUseFlags
             (accFreeVarsInTys opts tys 
               (accUsesFunctionLocalConstructs isProtectedCall acc))))

and accFreeInTargets opts targets acc = 
    Array.foldBack (fun (TTarget(vs,e,_)) acc -> FlatList.foldBack (boundLocalVal opts) vs (accFreeInExpr opts e acc)) targets acc

and accFreeInFlatExprs opts (es:FlatExprs) acc = FlatList.foldBack (accFreeInExpr opts) es acc

and accFreeInExprs opts (es: Exprs) acc = 
    match es with 
    | [] -> acc 
    | h::t -> 
        let acc = accFreeInExpr opts h acc
        // tailcall - e.g. Cons(x,Cons(x2,.......Cons(x1000000,Nil))) and [| x1; .... ; x1000000 |]
        accFreeInExprs opts t acc

and accFreeInSlotSig opts (TSlotSig(_,typ,_,_,_,_)) acc = accFreeVarsInTy opts typ acc
 
and freeInDecisionTree opts e = accFreeInDecisionTree opts e emptyFreeVars
and freeInExpr opts e = accFreeInExpr opts e emptyFreeVars

// Note: these are only an approximation - they are currently used only by the optimizer  
let rec accFreeInModuleOrNamespace opts x acc = 
    match x with 
    | TMDefRec(_,binds,mbinds,_) -> FlatList.foldBack (accBindRhs opts) binds  (List.foldBack (accFreeInModuleOrNamespaceBind opts) mbinds acc)
    | TMDefLet(bind,_)  -> accBindRhs opts bind  acc
    | TMDefDo(e,_)  -> accFreeInExpr opts e acc
    | TMDefs defs -> accFreeInModuleOrNamespaces opts defs acc
    | TMAbstract(ModuleOrNamespaceExprWithSig(_,mdef,_)) -> accFreeInModuleOrNamespace opts mdef acc // not really right, but sufficient for how this is used in optimization 
and accFreeInModuleOrNamespaceBind opts (ModuleOrNamespaceBinding(_, def)) acc = accFreeInModuleOrNamespace opts def acc
and accFreeInModuleOrNamespaces opts x acc = 
    List.foldBack (accFreeInModuleOrNamespace opts) x acc

// NOTE: we don't yet need to ask for free variables in module expressions 

let freeInBindingRhs opts bind = accBindRhs opts bind emptyFreeVars
let freeInModuleOrNamespace opts mdef = accFreeInModuleOrNamespace opts mdef emptyFreeVars

//---------------------------------------------------------------------------
// Destruct - rarely needed
//---------------------------------------------------------------------------

let rec stripLambda (e,ty) = 
    match e with 
    | Expr.Lambda (_,ctorThisValOpt,baseValOpt,v,b,_,rty) -> 
        if isSome ctorThisValOpt then errorR(InternalError("skipping ctorThisValOpt", e.Range));
        if isSome baseValOpt then errorR(InternalError("skipping baseValOpt", e.Range));
        let (vs',b',rty') = stripLambda (b,rty)
        (v :: vs', b', rty') 
    | _ -> ([],e,ty)

let rec stripLambdaN n e = 
    assert (n >= 0)
    match e with 
    | Expr.Lambda (_,ctorThisValOpt,baseValOpt,v,body,_,_) when n > 0 -> 
        if isSome ctorThisValOpt then errorR(InternalError("skipping ctorThisValOpt", e.Range));
        if isSome baseValOpt then errorR(InternalError("skipping baseValOpt", e.Range));
        let (vs,body',remaining) = stripLambdaN (n-1) body
        (v :: vs, body', remaining) 
    | _ -> ([],e,n)

let tryStripLambdaN n e = 
    match e with
    | Expr.Lambda(_,None,None,_,_,_,_) -> 
        let argvsl, body, remaining = stripLambdaN n e
        if remaining = 0 then Some (argvsl, body)
        else None
    | _ -> None

let stripTopLambda (e,ty) =
    let tps,taue,tauty = match e with Expr.TyLambda (_,tps,b,_,rty) -> tps,b,rty | _ -> [],e,ty
    let vs,body,rty = stripLambda (taue,tauty)
    tps,vs,body,rty

// This is used to infer arities of expressions 
// i.e. base the chosen arity on the syntactic expression shape and type of arguments 
let InferArityOfExpr g ty partialArgAttribsL retAttribs e = 
    let rec stripLambda_notypes e = 
        match e with 
        | Expr.Lambda (_,_,_,vs,b,_,_) -> 
            let (vs',b') = stripLambda_notypes b
            (vs :: vs', b') 
        | Expr.TyChoose (_,b,_) -> stripLambda_notypes b 
        | _ -> ([],e)

    let stripTopLambdaNoTypes e =
        let tps,taue = match e with Expr.TyLambda (_,tps,b,_,_) -> tps,b | _ -> [],e
        let vs,body = stripLambda_notypes taue
        tps,vs,body

    let tps,vsl,_ = stripTopLambdaNoTypes e
    let fun_arity = vsl.Length
    let dtys,_ =  stripFunTyN g fun_arity (snd (tryDestForallTy g ty))
    let partialArgAttribsL = Array.ofList partialArgAttribsL
    assert (List.length vsl = List.length dtys)
        
    let curriedArgInfos =
        (List.zip vsl dtys) |> List.mapi (fun i (vs,ty) -> 
            let partialAttribs = if i < partialArgAttribsL.Length then partialArgAttribsL.[i] else []
            let tys = if (i = 0 && isUnitTy g ty) then [] else tryDestTupleTy g ty
            let ids = 
                if vs.Length = tys.Length then  vs |> List.map (fun v -> Some v.Id)
                else tys |> List.map (fun _ -> None)
            let attribs = 
                if partialAttribs.Length = tys.Length then  partialAttribs 
                else tys |> List.map (fun _ -> [])
            (ids,attribs) ||> List.map2 (fun id attribs -> { Name = id; Attribs = attribs } : ArgReprInfo ))
    let retInfo : ArgReprInfo = { Attribs = retAttribs; Name = None }
    ValReprInfo (ValReprInfo.InferTyparInfo tps, curriedArgInfos, retInfo)

let InferArityOfExprBinding g (v:Val) e = 
    match v.ValReprInfo with
    | Some info -> info
    | None -> InferArityOfExpr g v.Type [] [] e

//-------------------------------------------------------------------------
// Check if constraints are satisfied that allow us to use more optimized
// implementations
//------------------------------------------------------------------------- 

let underlyingTypeOfEnumTy g typ = 
   assert(isEnumTy g typ);
   let tycon = tyconOfAppTy g typ
   if tycon.IsILEnumTycon then 
       let tdef = tycon.ILTyconRawMetadata
       let info = computeILEnumInfo (tdef.Name,tdef.Fields)
       let il_ty = getTyOfILEnumInfo info
       match il_ty.TypeSpec.Name with 
       | "System.Byte" -> g.byte_ty
       | "System.SByte" -> g.sbyte_ty
       | "System.Int16" -> g.int16_ty
       | "System.Int32" -> g.int32_ty
       | "System.Int64" -> g.int64_ty
       | "System.UInt16" -> g.uint16_ty
       | "System.UInt32" -> g.uint32_ty
       | "System.UInt64" -> g.uint64_ty
       | "System.Single" -> g.float32_ty
       | "System.Double" -> g.float_ty
       | "System.Char" -> g.char_ty
       | "System.Boolean" -> g.bool_ty
       | _ -> g.int32_ty
   else 
       match tycon.GetFieldByName "value__" with 
       | Some rf -> rf.FormalType
       | None ->  error(InternalError("no 'value__' field found for enumeration type "^tycon.LogicalName,tycon.Range))


// CLEANUP NOTE: Get rid of this mutation. 
let setValHasNoArity (f:Val) = 
    f.SetValReprInfo None; f


//--------------------------------------------------------------------------
// Resolve static optimization constraints
//--------------------------------------------------------------------------

let normalizeEnumTy g ty = (if isEnumTy g ty then underlyingTypeOfEnumTy g ty else ty) 

// -1 equals "no", 0 is "unknown", 1 is "yes"
let decideStaticOptimizationConstraint g c = 
    match c with 
    | TTyconEqualsTycon (a,b) ->
       let a = normalizeEnumTy g (stripTyEqnsAndMeasureEqns g a)
       let b = normalizeEnumTy g (stripTyEqnsAndMeasureEqns g b)
       // Both types must be nominal for a definite result
       match tryDestAppTy g a with 
       | Some tcref1 -> 
           match tryDestAppTy g b with 
           | Some tcref2 -> if tyconRefEq g tcref1 tcref2 then 1 else -1
           | None -> 0
       | None -> 0
    | TTyconIsStruct a -> 
       let a = normalizeEnumTy g (stripTyEqnsAndMeasureEqns g a)
       match tryDestAppTy g a with 
       | Some tcref1 -> if tcref1.IsStructOrEnumTycon then 1 else -1
       | None -> 0
            
let rec DecideStaticOptimizations g cs = 
    match cs with 
    | [] -> 1
    | h::t -> 
        let d = decideStaticOptimizationConstraint g h 
        if d = -1 then -1 elif d = 1 then DecideStaticOptimizations g t else 0

let mkStaticOptimizationExpr g (cs,e1,e2,m) = 
    let d = DecideStaticOptimizations g cs in 
    if d = -1 then e2
    elif d = 1 then e1
    else Expr.StaticOptimization(cs,e1,e2,m)

//--------------------------------------------------------------------------
// Copy expressions, including new names for locally bound values.
// Used to inline expressions.
//--------------------------------------------------------------------------


type ValCopyFlag = 
    | CloneAll
    | CloneAllAndMarkExprValsAsCompilerGenerated
    | OnlyCloneExprVals
    
let markAsCompGen compgen d = 
    let compgen = 
        match compgen with 
        | CloneAllAndMarkExprValsAsCompilerGenerated -> true
        | _ -> false
    { d with val_flags= d.val_flags.SetIsCompilerGenerated(d.val_flags.IsCompilerGenerated || compgen) }

let bindLocalVal (v:Val) (v':Val) tmenv = 
    { tmenv with valRemap=tmenv.valRemap.Add v (mkLocalValRef v') }

let bindLocalVals vs vs' tmenv = 
    { tmenv with valRemap= (vs, vs', tmenv.valRemap) |||> List.foldBack2 (fun v v' acc -> acc.Add v (mkLocalValRef v') ) }

let bindLocalFlatVals vs vs' tmenv = 
    { tmenv with valRemap= (vs, vs', tmenv.valRemap) |||> FlatList.foldBack2 (fun v v' acc -> acc.Add v (mkLocalValRef v') ) }

let bindTycon (tc:Tycon) (tc':Tycon) tyenv = 
    { tyenv with tyconRefRemap=tyenv.tyconRefRemap.Add (mkLocalTyconRef tc) (mkLocalTyconRef tc')  }

let bindTycons tcs tcs' tyenv =  
    { tyenv with tyconRefRemap= (tcs,tcs',tyenv.tyconRefRemap) |||> List.foldBack2 (fun tc tc' acc -> acc.Add (mkLocalTyconRef tc) (mkLocalTyconRef tc')) }

let remapAttribKind  tmenv k =  
    match k with 
    | ILAttrib _ as x -> x
    | FSAttrib vref -> FSAttrib(remapValRef tmenv vref)

let tmenvCopyRemapAndBindTypars remapAttrib tmenv tps = 
    let tps',tyenvinner = copyAndRemapAndBindTyparsFull remapAttrib tmenv tps
    let tmenvinner = tyenvinner 
    tps',tmenvinner

let rec remapAttrib g tmenv (Attrib (tcref,kind, args, props,isGetOrSetAttr,m)) = 
    Attrib(remapTyconRef tmenv.tyconRefRemap tcref,
           remapAttribKind tmenv kind, 
           args |> List.map (remapAttribExpr g tmenv), 
           props |> List.map (fun (AttribNamedArg(nm,ty,flg,expr)) -> AttribNamedArg(nm,remapType tmenv ty, flg, remapAttribExpr g tmenv expr)),
           isGetOrSetAttr,
           m)

and remapAttribExpr g tmenv (AttribExpr(e1,e2)) = 
    AttribExpr(remapExpr g CloneAll tmenv e1, remapExpr g CloneAll tmenv e2)
    
and remapAttribs g tmenv xs =  List.map (remapAttrib g tmenv) xs

and remapPossibleForallTy g tmenv ty = remapTypeFull (remapAttribs g tmenv) tmenv ty

and remapArgData g tmenv (argInfo : ArgReprInfo) : ArgReprInfo =
    { Attribs = remapAttribs g tmenv argInfo.Attribs; Name = argInfo.Name }

and remapValReprInfo g tmenv (ValReprInfo(tpNames,arginfosl,retInfo)) =
    ValReprInfo(tpNames,List.mapSquared (remapArgData g tmenv) arginfosl, remapArgData g tmenv retInfo)

and remapValData g tmenv d =
#if DEBUG
    if !verboseStamps then dprintf "remap val data #%d\n" d.val_stamp;
#endif
    let ty = d.val_type
    let topValInfo = d.val_repr_info
    let ty' = ty |> remapPossibleForallTy g tmenv
    { d with 
        val_type    = ty';
        val_actual_parent = d.val_actual_parent |> remapParentRef tmenv;
        val_repr_info = d.val_repr_info |> Option.map (remapValReprInfo g tmenv);
        val_member_info   = d.val_member_info |> Option.map (remapMemberInfo g d.val_defn_range topValInfo ty ty' tmenv);
        val_attribs       = d.val_attribs       |> remapAttribs g tmenv }

and remapParentRef tyenv p =
    match p with 
    | ParentNone -> ParentNone
    | Parent x -> Parent (x |> remapTyconRef tyenv.tyconRefRemap)

and mapImmediateValsAndTycons ft fv (x:ModuleOrNamespaceType) = 
    let vals = x.AllValsAndMembers      |> QueueList.map fv
    let tycons = x.AllEntities |> QueueList.map ft
    new ModuleOrNamespaceType(x.ModuleOrNamespaceKind, vals, tycons)
    
and copyAndRemapVal g compgen tmenv (v:Val) = 
    match compgen with 
    | OnlyCloneExprVals when v.IsMemberOrModuleBinding -> v
    | _ ->  v |> NewModifiedVal (remapValData g tmenv >> markAsCompGen compgen)

and fixupValAttribs g tmenv (v1:Val) (v2:Val) =
    let attrs = v1.Attribs
    if isNil attrs then () else
    v2.Data.val_attribs <- attrs |> remapAttribs g tmenv
    
and copyAndRemapAndBindVals g compgen tmenv vs = 
    let vs' = vs |> List.map (copyAndRemapVal g compgen tmenv)
    let tmenvinner = bindLocalVals vs vs' tmenv
    // Fixup attributes now we've built the full map of value renamings (attributes contain value references) 
    List.iter2 (fixupValAttribs g tmenvinner) vs vs';    
    vs', tmenvinner

and copyAndRemapAndBindFlatVals g compgen tmenv vs = 
    let vs' = vs |> FlatList.map (copyAndRemapVal g compgen tmenv)
    let tmenvinner = bindLocalFlatVals vs vs' tmenv
    // Fixup attributes now we've built the full map of value renamings (attributes contain value references) 
    FlatList.iter2 (fixupValAttribs g tmenvinner) vs vs';    
    vs', tmenvinner

and copyAndRemapAndBindVal g compgen tmenv v = 
    let v' = v |> copyAndRemapVal g compgen tmenv
    let tmenvinner = bindLocalVal v v' tmenv
    // Fixup attributes now we've built the full map of value renamings (attributes contain value references) 
    fixupValAttribs g tmenvinner v v';    
    v', tmenvinner
    
and remapExpr g (compgen:ValCopyFlag) (tmenv:Remap) x =
    match x with
    // Binding constructs - see also dtrees below 
    | Expr.Lambda (_,ctorThisValOpt, baseValOpt,vs,b,m,rty)  -> 
        let ctorThisValOpt, tmenv =  Option.mapFold (copyAndRemapAndBindVal g compgen) tmenv ctorThisValOpt
        let baseValOpt, tmenv =  Option.mapFold (copyAndRemapAndBindVal g compgen) tmenv baseValOpt
        let vs,tmenv = copyAndRemapAndBindVals g compgen tmenv vs
        let b = remapExpr g compgen tmenv b
        let rty = remapType tmenv rty
        Expr.Lambda (newUnique(), ctorThisValOpt, baseValOpt,vs,b,m, rty)
    | Expr.TyLambda (_,tps,b,m,rty) ->
        let tps',tmenvinner = tmenvCopyRemapAndBindTypars (remapAttribs g tmenv) tmenv tps
        mkTypeLambda m tps' (remapExpr g compgen tmenvinner b,remapType tmenvinner rty)
    | Expr.TyChoose (tps,b,m) ->
        let tps',tmenvinner = tmenvCopyRemapAndBindTypars (remapAttribs g tmenv) tmenv tps
        Expr.TyChoose(tps',remapExpr g compgen tmenvinner b,m)
    | Expr.LetRec (binds,e,m,_) ->  
        let binds',tmenvinner = copyAndRemapAndBindBindings g compgen tmenv binds 
        Expr.LetRec (binds',remapExpr g compgen tmenvinner e,m,NewFreeVarsCache())
    | Expr.Seq _  
    | Expr.Let _ -> remapLinearExpr g compgen tmenv x (fun x -> x)
    | Expr.Match (spBind,exprm,pt,targets,m,ty) ->
        primMkMatch (spBind,exprm,remapDecisionTree g compgen tmenv pt,
                     targets |> Array.map (fun (TTarget(vs,e,spTarget)) ->
                       let vs',tmenvinner = copyAndRemapAndBindFlatVals g compgen tmenv vs 
                       TTarget(vs', remapExpr g compgen tmenvinner e,spTarget)),
                     m,remapType tmenv ty)
    // Other constructs 
    | Expr.Val (vr,vf,m) -> 
        let vr' = remapValRef tmenv vr 
        let vf' = remapValFlags tmenv vf
        if vr === vr' && vf === vf' then x 
        else Expr.Val (vr',vf',m)
    | Expr.Quote (a,{contents=Some(argTypes,argExprs,data)},m,ty) ->  
        Expr.Quote (remapExpr g compgen tmenv a,{contents=Some(remapTypesAux tmenv argTypes,remapExprs g compgen tmenv  argExprs,data)},m,remapType tmenv ty)
    | Expr.Quote (a,{contents=None},m,ty) ->  
        Expr.Quote (remapExpr g compgen tmenv a,{contents=None},m,remapType tmenv ty)
    | Expr.Obj (_,typ,basev,basecall,overrides,iimpls,m) -> 
        let basev',tmenvinner = Option.mapFold (copyAndRemapAndBindVal g compgen) tmenv basev 
        mkObjExpr(remapType tmenv typ,basev',
                    remapExpr g compgen tmenv basecall,
                    List.map (remapMethod g compgen tmenvinner) overrides,
                    List.map (remapInterfaceImpl g compgen tmenvinner) iimpls,m) 

    // Addresses of immutable field may "leak" across assembly boundaries - see CanTakeAddressOfRecdField below.
    // This is "ok", in the sense that it is always valid to fix these up to be uses
    // of a temporary local, e.g.
    //       &(E.RF) --> let mutable v = E.RF in &v
    
    | Expr.Op (TOp.ValFieldGetAddr rfref,tinst,[arg],m) when 
          not rfref.RecdField.IsMutable && 
          not (entityRefInThisAssembly g.compilingFslib rfref.TyconRef) -> 

        let tinst = remapTypes tmenv tinst 
        let arg = remapExpr g compgen tmenv arg 
        let tmp,_ = mkMutableCompGenLocal m "copyOfStruct" (actualTyOfRecdFieldRef rfref tinst)
        mkCompGenLet m tmp (mkRecdFieldGetViaExprAddr(arg,rfref,tinst,m)) (mkValAddr m (mkLocalValRef tmp))

    | Expr.Op (op,tinst,args,m) -> 
        let op' = remapOp tmenv op 
        let tinst' = remapTypes tmenv tinst 
        let args' = remapExprs g compgen tmenv args 
        if op === op' && tinst === tinst' && args === args' then x 
        else Expr.Op (op',tinst',args',m)

    | Expr.App(e1,e1ty,tyargs,args,m) -> 
        let e1' = remapExpr g compgen tmenv e1 
        let e1ty' = remapPossibleForallTy g tmenv e1ty 
        let tyargs' = remapTypes tmenv tyargs 
        let args' = remapExprs g compgen tmenv args 
        if e1 === e1' && e1ty === e1ty' && tyargs === tyargs' && args === args' then x 
        else Expr.App(e1',e1ty',tyargs',args',m)
    | Expr.Link(eref) -> 
        remapExpr g compgen tmenv !eref
    | Expr.StaticOptimization (cs,e2,e3,m) -> 
       // note that type instantiation typically resolve the static constraints here 
       mkStaticOptimizationExpr g (List.map (remapConstraint tmenv) cs,remapExpr g compgen tmenv e2,remapExpr g compgen tmenv e3,m)

    | Expr.Const (c,m,ty) -> 
        let ty' = remapType tmenv ty 
        if ty === ty' then x else Expr.Const (c,m,ty')

and remapLinearExpr g compgen tmenv e contf =
    match e with 
    | Expr.Let (bind,e,m,_) ->  
      let bind',tmenvinner = copyAndRemapAndBindBinding g compgen tmenv bind
      remapLinearExpr g compgen tmenvinner e (contf << mkLetBind m bind')

    | Expr.Seq (e1,e2,dir,spSeq,m)  -> 
        let e1' = remapExpr g compgen tmenv e1 
        remapLinearExpr g compgen tmenv e2 (contf << (fun e2' -> 
        if e1 === e1' && e2 === e2' then e 
        else Expr.Seq (e1',e2',dir,spSeq,m)))

    | _ -> contf (remapExpr g compgen tmenv e) 

and remapConstraint tyenv c = 
    match c with 
    | TTyconEqualsTycon(ty1,ty2) -> TTyconEqualsTycon(remapType tyenv ty1, remapType tyenv ty2)
    | TTyconIsStruct(ty1) -> TTyconIsStruct(remapType tyenv ty1)

and remapOp tmenv op = 
    match op with 
    | TOp.Recd (ctor,tcr)             -> TOp.Recd(ctor,remapTyconRef tmenv.tyconRefRemap tcr)
    | TOp.UnionCaseTagGet tcr         -> TOp.UnionCaseTagGet(remapTyconRef tmenv.tyconRefRemap tcr)
    | TOp.UnionCase(ucref)            -> TOp.UnionCase(remapUnionCaseRef tmenv.tyconRefRemap ucref)
    | TOp.UnionCaseProof(ucref)       -> TOp.UnionCaseProof(remapUnionCaseRef tmenv.tyconRefRemap ucref)
    | TOp.ExnConstr ec                -> TOp.ExnConstr(remapTyconRef tmenv.tyconRefRemap ec)
    | TOp.ExnFieldGet(ec,n)           -> TOp.ExnFieldGet(remapTyconRef tmenv.tyconRefRemap ec,n)
    | TOp.ExnFieldSet(ec,n)           -> TOp.ExnFieldSet(remapTyconRef tmenv.tyconRefRemap ec,n)
    | TOp.ValFieldSet rfref           -> TOp.ValFieldSet(remapRecdFieldRef tmenv.tyconRefRemap rfref)
    | TOp.ValFieldGet rfref           -> TOp.ValFieldGet(remapRecdFieldRef tmenv.tyconRefRemap rfref)
    | TOp.ValFieldGetAddr rfref       -> TOp.ValFieldGetAddr(remapRecdFieldRef tmenv.tyconRefRemap rfref)
    | TOp.UnionCaseFieldGet(ucref,n)  -> TOp.UnionCaseFieldGet(remapUnionCaseRef tmenv.tyconRefRemap ucref,n)
    | TOp.UnionCaseFieldSet(ucref,n)  -> TOp.UnionCaseFieldSet(remapUnionCaseRef tmenv.tyconRefRemap ucref,n)
    | TOp.ILAsm (instrs,tys)          -> TOp.ILAsm (instrs,remapTypes tmenv tys)
    | TOp.TraitCall(traitInfo)        -> TOp.TraitCall(remapTraitAux tmenv traitInfo)
    | TOp.LValueOp (kind,lvr)         -> TOp.LValueOp (kind,remapValRef tmenv lvr)
    | TOp.ILCall (isVirtCall,isProtectedCall,valu,isNewObjCall,valUseFlags,isProperty,isDllImport,ilMethRef,enclTypeArgs,methTypeArgs,tys) -> 
       TOp.ILCall (isVirtCall,isProtectedCall,valu,isNewObjCall,remapValFlags tmenv valUseFlags,
                   isProperty,isDllImport,ilMethRef,remapTypes tmenv enclTypeArgs,
                   remapTypes tmenv methTypeArgs,remapTypes tmenv tys)
    | _ ->  op
    

and remapValFlags tmenv x =
    match x with 
    | PossibleConstrainedCall ty -> PossibleConstrainedCall (remapType tmenv ty)
    | _ -> x

and remapExprs g compgen tmenv es = List.mapq (remapExpr g compgen tmenv) es
and remapFlatExprs g compgen tmenv es = FlatList.mapq (remapExpr g compgen tmenv) es

and remapDecisionTree g compgen tmenv x =
    match x with 
    | TDSwitch(e1,csl,dflt,m) -> 
        TDSwitch(remapExpr g compgen tmenv e1,
                List.map (fun (TCase(test,y)) -> 
                  let test' = 
                    match test with 
                    | Test.UnionCase (uc,tinst)   -> Test.UnionCase(remapUnionCaseRef tmenv.tyconRefRemap uc,remapTypes tmenv tinst)
                    | Test.ArrayLength (n,ty)      -> Test.ArrayLength(n,remapType tmenv ty)
                    | Test.Const _                  -> test
                    | Test.IsInst (srcty,tgty)      -> Test.IsInst (remapType tmenv srcty,remapType tmenv tgty) 
                    | Test.IsNull                   -> Test.IsNull 
                    | Test.ActivePatternCase _ -> failwith "Test.ActivePatternCase should only be used during pattern match compilation"
                  TCase(test',remapDecisionTree g compgen tmenv y)) csl, 
                Option.map (remapDecisionTree g compgen tmenv) dflt,
                m)
    | TDSuccess (es,n) -> 
        TDSuccess (remapFlatExprs g compgen tmenv es,n)
    | TDBind (bind,rest) -> 
        let bind',tmenvinner = copyAndRemapAndBindBinding g compgen tmenv bind
        TDBind (bind',remapDecisionTree g compgen tmenvinner rest)
        
and copyAndRemapAndBindBinding g compgen tmenv (bind:Binding) =
    let v = bind.Var
    let v', tmenv = copyAndRemapAndBindVal g compgen tmenv v
    remapAndRenameBind g compgen tmenv bind v' , tmenv

and copyAndRemapAndBindBindings g compgen tmenv binds = 
    let vs', tmenvinner = copyAndRemapAndBindFlatVals g compgen tmenv (valsOfBinds binds)
    remapAndRenameBinds g compgen tmenvinner binds vs',tmenvinner

and remapAndRenameBinds g compgen tmenvinner binds vs' = FlatList.map2 (remapAndRenameBind g compgen tmenvinner) binds vs'
and remapAndRenameBind g compgen tmenvinner (TBind(_,repr,letSeqPtOpt)) v' = TBind(v', remapExpr g compgen tmenvinner repr,letSeqPtOpt)

and remapMethod g compgen tmenv (TObjExprMethod(slotsig,attribs,tps,vs,e,m))  =
    let attribs2 = attribs |> remapAttribs g tmenv
    let slotsig2 = remapSlotSig (remapAttribs g tmenv) tmenv slotsig
    let tps2,tmenvinner = tmenvCopyRemapAndBindTypars (remapAttribs g tmenv) tmenv tps
    let vs2, tmenvinner2 = List.mapFold (copyAndRemapAndBindVals g compgen) tmenvinner vs
    let e2 = remapExpr g compgen tmenvinner2 e
    TObjExprMethod(slotsig2,attribs2,tps2,vs2,e2,m)

and remapInterfaceImpl g compgen tmenv (ty,overrides)  =
    (remapType tmenv ty, List.map (remapMethod g compgen tmenv) overrides)

and remapRecdField g tmenv x = 
    { x with 
          rfield_type     = x.rfield_type     |> remapPossibleForallTy g tmenv;
          rfield_pattribs = x.rfield_pattribs |> remapAttribs g tmenv;
          rfield_fattribs = x.rfield_fattribs |> remapAttribs g tmenv; } 
and remapRecdFields g tmenv (x:TyconRecdFields) = x.AllFieldsAsList |> List.map (remapRecdField g tmenv) |> MakeRecdFieldsTable 

and remapUnionCase g tmenv x = 
    { x with 
          FieldTable = x.FieldTable |> remapRecdFields g tmenv;
          ReturnType     = x.ReturnType     |> remapType tmenv;
          Attribs = x.Attribs |> remapAttribs g tmenv; } 
and remapUnionCases g tmenv (x:TyconUnionData) = x.UnionCasesAsList |> List.map (remapUnionCase g tmenv)|> MakeUnionCases 

and remapFsObjData g tmenv x = 
    { x with 
          fsobjmodel_kind = 
             (match x.fsobjmodel_kind with 
              | TTyconDelegate slotsig -> TTyconDelegate (remapSlotSig (remapAttribs g tmenv) tmenv slotsig)
              | TTyconClass | TTyconInterface | TTyconStruct | TTyconEnum -> x.fsobjmodel_kind);
          fsobjmodel_vslots  = x.fsobjmodel_vslots  |> List.map (remapValRef tmenv);
          fsobjmodel_rfields = x.fsobjmodel_rfields |> remapRecdFields g tmenv } 


and remapTyconRepr g tmenv repr = 
    match repr with 
    | TFsObjModelRepr    x -> TFsObjModelRepr (remapFsObjData g tmenv x)
    | TRecdRepr          x -> TRecdRepr (remapRecdFields g tmenv x)
    | TFiniteUnionRepr   x -> TFiniteUnionRepr (remapUnionCases g tmenv x)
    | TILObjModelRepr    _ -> failwith "cannot remap IL type definitions"
    | TAsmRepr           _ -> repr
    | TMeasureableRepr   x -> TMeasureableRepr (remapType tmenv x)

and remapTyconAug tmenv x = 
    { x with 
          tcaug_equals                 = x.tcaug_equals                  |> Option.map (mapPair (remapValRef tmenv, remapValRef tmenv));
          tcaug_compare                = x.tcaug_compare                 |> Option.map (mapPair (remapValRef tmenv, remapValRef tmenv));
          tcaug_compare_withc          = x.tcaug_compare_withc           |> Option.map(remapValRef tmenv);
          tcaug_hash_and_equals_withc  = x.tcaug_hash_and_equals_withc   |> Option.map (mapTriple (remapValRef tmenv, remapValRef tmenv, remapValRef tmenv));
          tcaug_adhoc                  = x.tcaug_adhoc                   |> NameMap.map (List.map (remapValRef tmenv));
          tcaug_adhoc_list             = x.tcaug_adhoc_list              |> ResizeArray.map (fun (flag, vref) -> (flag, remapValRef tmenv vref));
          tcaug_super                  = x.tcaug_super                   |> Option.map (remapType tmenv);
          tcaug_interfaces             = x.tcaug_interfaces              |> List.map (map1Of3 (remapType tmenv)) } 

and remapTyconExnInfo g tmenv inp =
    match inp with 
    | TExnAbbrevRepr x -> TExnAbbrevRepr (remapTyconRef tmenv.tyconRefRemap x)
    | TExnFresh      x -> TExnFresh (remapRecdFields g tmenv x)
    | TExnAsmRepr  _ | TExnNone -> inp 

and remapMemberInfo g m topValInfo ty ty' tmenv x = 
    // The slotsig in the ImplementedSlotSigs is w.r.t. the type variables in the value's type. 
    assert (isSome topValInfo);
    let tpsOrig,_,_,_ = GetMemberTypeInFSharpForm g x.MemberFlags (the(topValInfo)) ty m
    let tps,_,_,_ = GetMemberTypeInFSharpForm g x.MemberFlags (the(topValInfo)) ty' m
    let renaming,_ = mkTyparToTyparRenaming tpsOrig tps 
    let tmenv = { tmenv with tpinst = tmenv.tpinst @ renaming } 
    { x with 
        ApparentParent    = x.ApparentParent    |>  remapTyconRef tmenv.tyconRefRemap ;
        ImplementedSlotSigs = x.ImplementedSlotSigs |> List.map (remapSlotSig (remapAttribs g tmenv) tmenv); 
    } 

and copyAndRemapAndBindModTy g compgen tmenv mty = 
    let tycons = allEntitiesOfModuleOrNamespaceTy mty
    let vs = allValsOfModuleOrNamespaceTy mty
    let _,_,tmenvinner = copyAndRemapAndBindTyconsAndVals g compgen tmenv tycons vs
    remapModTy g compgen tmenvinner mty, tmenvinner

and remapModTy _g _compgen tmenv mty = 
    mapImmediateValsAndTycons (renameTycon tmenv) (renameVal tmenv) mty 

and renameTycon tyenv x = 
    let tcref = 
        try 
            let res = tyenv.tyconRefRemap.[mkLocalTyconRef x]
            res
        with :? KeyNotFoundException -> 
            errorR(InternalError("couldn't remap internal tycon "^showL(DebugPrint.tyconL x),x.Range)); 
            mkLocalTyconRef x 
    tcref.Deref

and renameVal tmenv x = 
    match tmenv.valRemap.TryFind x with 
    | Some v -> v.Deref
    | None -> x

and copyTycon compgen (tycon:Tycon) = 
    match compgen with 
    | OnlyCloneExprVals -> tycon
    | _ ->  NewClonedTycon tycon

/// This operates over a whole nested collection of tycons and vals simultaneously *)
and copyAndRemapAndBindTyconsAndVals g compgen tmenv tycons vs = 
    let tycons' = tycons |> List.map (copyTycon compgen)

    let tmenvinner = bindTycons tycons tycons' tmenv
    
    // Values need to be copied and renamed. 
    let vs',tmenvinner = copyAndRemapAndBindVals g compgen tmenvinner vs
#if DEBUG
    if !verboseStamps then 
        for tycon in tycons do 
            dprintf "copyAndRemapAndBindTyconsAndVals: tycon %s#%d\n" tycon.LogicalName tycon.Stamp;
        for v in vs do 
            dprintf "copyAndRemapAndBindTyconsAndVals: val %s#%d\n" v.LogicalName v.Stamp;
#endif

    // "if a type constructor is hidden then all its inner values and inner type constructors must also be hidden" 
    // Hence we can just lookup the inner tycon/value mappings in the tables. 

    let lookupVal (v:Val) = 
        let vref = 
            try  
               let res = tmenvinner.valRemap.[v]
#if DEBUG
               if !verboseStamps then 
                   dprintf "remaped internal value %s#%d --> %s#%d\n" v.LogicalName v.Stamp res.LogicalName res.Stamp;
#endif
               res 
            with :? KeyNotFoundException -> 
                errorR(InternalError(sprintf "couldn't remap internal value '%s'" v.LogicalName,v.Range));
                mkLocalValRef v
        vref.Deref
        
    let lookupTycon tycon = 
        let tcref = 
            try 
                let res = tmenvinner.tyconRefRemap.[mkLocalTyconRef tycon]
#if DEBUG
                if !verboseStamps then 
                    dprintf "remaped internal tycon %s#%d --> %s#%d\n" tycon.LogicalName tycon.Stamp res.LogicalName res.Stamp;
#endif
                res
            with :? KeyNotFoundException -> 
                errorR(InternalError("couldn't remap internal tycon "^showL(DebugPrint.tyconL tycon),tycon.Range));
                mkLocalTyconRef tycon
        tcref.Deref
             
    (tycons,tycons') ||> List.iter2 (fun tc tc' -> 
        let tcd = tc.Data
        let tcd' = tc'.Data
        let tps',tmenvinner2 = tmenvCopyRemapAndBindTypars (remapAttribs g tmenvinner) tmenvinner (tcd.entity_typars.Force(tcd.entity_range))
        tcd'.entity_typars         <- LazyWithContext.NotLazy tps';
        tcd'.entity_attribs        <- tcd.entity_attribs |> remapAttribs g tmenvinner2;
        tcd'.entity_tycon_repr           <- tcd.entity_tycon_repr    |> Option.map (remapTyconRepr g tmenvinner2);
        tcd'.entity_tycon_abbrev         <- tcd.entity_tycon_abbrev  |> Option.map (remapType tmenvinner2) ;
        tcd'.entity_tycon_tcaug          <- tcd.entity_tycon_tcaug   |> remapTyconAug tmenvinner2 ;
        tcd'.entity_modul_contents <- notlazy (tcd.entity_modul_contents 
                                              |> Lazy.force 
                                              |> mapImmediateValsAndTycons lookupTycon lookupVal);
        tcd'.entity_exn_info      <- tcd.entity_exn_info      |> remapTyconExnInfo g tmenvinner2) ;
    tycons',vs', tmenvinner


and allTyconsOfModDef mdef =
    match mdef with 
    | TMDefRec(tycons,_,mbinds,_) -> 
        tycons @ List.collect (fun (ModuleOrNamespaceBinding(mspec, def)) -> mspec :: allTyconsOfModDef def) mbinds
    | TMDefLet _           -> []
    | TMDefDo _            -> []
    | TMDefs defs      -> List.collect allTyconsOfModDef defs
    | TMAbstract(ModuleOrNamespaceExprWithSig(mty,_,_)) -> allEntitiesOfModuleOrNamespaceTy mty
and allValsOfModDef mdef = 
    match mdef with 
    | TMDefRec(tycons,binds,mbinds,_) -> 
       abstractSlotValsOfTycons tycons @
       (binds |> valsOfBinds |> FlatList.toList) @ 
       List.collect (fun (ModuleOrNamespaceBinding(_, def)) -> allValsOfModDef def) mbinds
    | TMDefLet(bind,_)            -> [bind.Var]
    | TMDefDo _            -> []
    | TMDefs defs      -> List.collect allValsOfModDef defs
    | TMAbstract(ModuleOrNamespaceExprWithSig(mty,_,_)) -> allValsOfModuleOrNamespaceTy mty

and remapAndBindModExpr g compgen tmenv (ModuleOrNamespaceExprWithSig(mty,mdef,m)) =
    let mdef = copyAndRemapModDef g compgen tmenv mdef
    let mty,tmenv = copyAndRemapAndBindModTy g compgen tmenv mty
    ModuleOrNamespaceExprWithSig(mty,mdef,m), tmenv

and remapModExpr g compgen tmenv (ModuleOrNamespaceExprWithSig(mty,mdef,m)) =
    let mdef = copyAndRemapModDef g compgen tmenv mdef 
    let mty = remapModTy g compgen tmenv mty 
    ModuleOrNamespaceExprWithSig(mty,mdef,m)

and copyAndRemapModDef g compgen tmenv mdef =
    let tycons = allTyconsOfModDef mdef 
    let vs = allValsOfModDef mdef 
    let _,_,tmenvinner = copyAndRemapAndBindTyconsAndVals g compgen tmenv tycons vs
    remapAndRenameModDef g compgen tmenvinner mdef

and remapAndRenameModDefs g compgen tmenv x = 
    List.map (remapAndRenameModDef g compgen tmenv) x 

and remapAndRenameModDef g compgen tmenv mdef =
    match mdef with 
    | TMDefRec(tycons,binds,mbinds,m) -> 
        // Abstract (virtual) vslots in the tycons at TMDefRec nodes are binders. They also need to be copied and renamed. 
        let tycons = tycons |> List.map (renameTycon tmenv)
        let binds = remapAndRenameBinds g compgen tmenv binds (binds |> FlatList.map (valOfBind >> renameVal tmenv))
        let mbinds = mbinds |> List.map (remapAndRenameModBind g compgen tmenv)
        TMDefRec(tycons,binds,mbinds,m)
    | TMDefLet(bind,m)            ->
        let v = bind.Var
        let bind = remapAndRenameBind g compgen tmenv bind (renameVal tmenv v)
        TMDefLet(bind, m)
    | TMDefDo(e,m)            ->
        let e = remapExpr g compgen tmenv e
        TMDefDo(e, m)
    | TMDefs defs      -> 
        let defs = remapAndRenameModDefs g compgen tmenv defs
        TMDefs defs
    | TMAbstract mexpr -> 
        let mexpr = remapModExpr g compgen tmenv mexpr
        TMAbstract mexpr

and remapAndRenameModBind g compgen tmenv (ModuleOrNamespaceBinding(mspec, def)) =
    let mspec = renameTycon tmenv mspec
    let def = remapAndRenameModDef g compgen tmenv def
    ModuleOrNamespaceBinding(mspec, def)

and remapImplFile g compgen tmenv mv = 
    mapAccImplFile (remapAndBindModExpr g compgen) tmenv mv

and remapAssembly g compgen tmenv (TAssembly(mvs)) = 
    let mvs,z = List.mapFold (remapImplFile g compgen) tmenv mvs
    TAssembly(mvs),z

let copyModuleOrNamespaceType     g compgen mtyp = copyAndRemapAndBindModTy g compgen Remap.Empty mtyp |> fst
let copyExpr     g compgen e    = remapExpr g compgen Remap.Empty e    
let copyImplFile g compgen e    = remapImplFile g compgen Remap.Empty e |> fst

let instExpr g tpinst e = remapExpr g CloneAll (mkInstRemap tpinst) e

//--------------------------------------------------------------------------
// Replace Marks - adjust debugging marks when a lambda gets
// eliminated (i.e. an expression gets inlined)
//--------------------------------------------------------------------------

let rec remarkExpr m x =
    match x with
    | Expr.Lambda (uniq,ctorThisValOpt,baseValOpt,vs,b,_,rty)  -> Expr.Lambda (uniq,ctorThisValOpt,baseValOpt,vs,remarkExpr m b,m,rty)  
    | Expr.TyLambda (uniq,tps,b,_,rty) -> Expr.TyLambda (uniq,tps,remarkExpr m b,m,rty)
    | Expr.TyChoose (tps,b,_) -> Expr.TyChoose (tps,remarkExpr m b,m)
    | Expr.LetRec (binds,e,_,fvs) ->  Expr.LetRec (remarkBinds m binds,remarkExpr m e,m,fvs)
    | Expr.Let (bind,e,_,fvs) -> Expr.Let (remarkBind m bind,remarkExpr m e,m,fvs)
    | Expr.Match (_,_,pt,targets,_,ty) -> primMkMatch (NoSequencePointAtInvisibleBinding,m,remarkDecisionTree m pt, Array.map (fun (TTarget(vs,e,_)) ->TTarget(vs, remarkExpr m e,SuppressSequencePointAtTarget)) targets,m,ty)
    | Expr.Val (x,valUseFlags,_) -> Expr.Val (x,valUseFlags,m)
    | Expr.Quote (a,conv,_,ty) ->  Expr.Quote (remarkExpr m a,conv,m,ty)
    | Expr.Obj (n,typ,basev,basecall,overrides,iimpls,_) -> 
        Expr.Obj (n,typ,basev,remarkExpr m basecall,
                     List.map (remarkObjExprMethod m) overrides,
                     List.map (remarkInterfaceImpl m) iimpls,m)
    | Expr.Op (op,tinst,args,_) -> 
        let op = 
            match op with 
            | TOp.TryFinally(_,_) -> TOp.TryFinally(NoSequencePointAtTry,NoSequencePointAtFinally)
            | TOp.TryCatch(_,_) -> TOp.TryCatch(NoSequencePointAtTry,NoSequencePointAtWith)
            | _ -> op
            
        Expr.Op (op,tinst,remarkExprs m args,m)
    | Expr.Link (eref) -> 
        // Preserve identity of fixup nodes during remarkExpr
        eref := remarkExpr m !eref;
        x
    | Expr.App(e1,e1ty,tyargs,args,_) -> Expr.App(remarkExpr m e1,e1ty,tyargs,remarkExprs m args,m)
    | Expr.Seq (e1,e2,dir,_,_)  -> Expr.Seq (remarkExpr m e1,remarkExpr m e2,dir,SuppressSequencePointOnExprOfSequential,m)
    | Expr.StaticOptimization (eqns,e2,e3,_) -> Expr.StaticOptimization (eqns,remarkExpr m e2,remarkExpr m e3,m)
    | Expr.Const (c,_,ty) -> Expr.Const (c,m,ty)
  
and remarkObjExprMethod m (TObjExprMethod(slotsig, attribs, tps, vs, e, _)) = 
    TObjExprMethod(slotsig, attribs, tps, vs, remarkExpr m e, m)

and remarkInterfaceImpl m (ty,overrides) = 
    (ty, List.map (remarkObjExprMethod m) overrides)

and remarkExprs m es = es |> List.map (remarkExpr m) 

and remarkFlatExprs m es = es |> FlatList.map (remarkExpr m) 

and remarkDecisionTree m x =
    match x with 
    | TDSwitch(e1,csl,dflt,_) -> TDSwitch(remarkExpr m e1, List.map (fun (TCase(test,y)) -> TCase(test,remarkDecisionTree m y)) csl, Option.map (remarkDecisionTree m) dflt,m)
    | TDSuccess (es,n) -> TDSuccess (remarkFlatExprs m es,n)
    | TDBind (bind,rest) -> TDBind(remarkBind m bind,remarkDecisionTree m rest)

and remarkBinds m binds = FlatList.map (remarkBind m) binds

// This very deliberately drops the sequence points since this is used when adjusting the marks for inlined expressions 
and remarkBind m (TBind(v,repr,_)) = 
    TBind(v, remarkExpr m repr,NoSequencePointAtStickyBinding)


//--------------------------------------------------------------------------
// Reference semantics?
//--------------------------------------------------------------------------

let isRecdFieldAllocObservable (f:RecdField) = not f.IsStatic && f.IsMutable
let ucaseAllocObservable uc = uc.FieldTable.FieldsByIndex |> Array.exists isRecdFieldAllocObservable
let isUnionCaseAllocObservable (uc:UnionCaseRef) = uc.UnionCase |> ucaseAllocObservable
  
let isTyconAllocObservable (tycon:Tycon) =
    if tycon.IsRecordTycon || tycon.IsStructOrEnumTycon then 
        tycon.AllFieldsArray |> Array.exists isRecdFieldAllocObservable
    elif tycon.IsUnionTycon then 
        tycon.UnionCasesArray |> Array.exists ucaseAllocObservable
    else
        false

let isTyconRefAllocObservable (tcr : TyconRef) = isTyconAllocObservable tcr.Deref
  
// Although from the pure F# perspective exception values cannot be changed, the .NET 
// implementation of exception objects attaches a whole bunch of stack information to 
// each raised object.  Hence we treat exception objects as if they have identity 
let isExnAllocObservable (_ecref:TyconRef) = true 

// Some of the implementations of library functions on lists use mutation on the tail 
// of the cons cell. These cells are always private, i.e. not accessible by any other 
// code until the construction of the entire return list has been completed. 
// However, within the implementation code reads of the tail cell must in theory be treated 
// with caution.  Hence we are conservative and within fslib we don't treat list 
// reads as if they were pure. 
let isUnionCaseFieldMutable g (ucref:UnionCaseRef) n = 
    (g.compilingFslib && tyconRefEq g ucref.TyconRef g.list_tcr_canon && n = 1) ||
    (ucref.FieldByIndex n).IsMutable
  
let isExnFieldMutable ecref n = 
    if n < 0 || n >= List.length (recdFieldsOfExnDefRef ecref) then errorR(InternalError(sprintf "isExnFieldMutable, exnc = %s, n = %d" ecref.LogicalName n,ecref.Range));
    (recdFieldOfExnDefRefByIdx ecref n).IsMutable

let useGenuineField (tycon:Tycon) (f:RecdField) = 
    isSome f.LiteralValue || tycon.IsEnumTycon || f.rfield_secret || (not f.IsStatic && f.rfield_mutable && not tycon.IsRecordTycon)

let ComputeFieldName tycon f = 
    if useGenuineField tycon f then f.rfield_id.idText
    else CompilerGeneratedName f.rfield_id.idText 

//-------------------------------------------------------------------------
// Helpers for building code contained in the initial environment
//------------------------------------------------------------------------- 

let mkQuotedExprTy g ty =  TType_app(g.expr_tcr,[ty])
let mkRawQuotedExprTy g =  TType_app(g.raw_expr_tcr,[])

let mkTupledTy g tys = 
    match tys with 
    | [] -> g.unit_ty 
    | [h] -> h
    | _ -> mkTupleTy tys

let mkTupledVarsTy g vs = 
    mkTupledTy g (typesOfVals vs)

let mkMethodTy g argtys rty = mkIteratedFunTy (List.map (mkTupledTy g) argtys) rty 
let mkArrayType g ty = TType_app (g.array_tcr_nice, [ty])
let mkByteArrayTy g = mkArrayType g g.byte_ty


//--------------------------------------------------------------------------
// tyOfExpr
//--------------------------------------------------------------------------
 
let rec tyOfExpr g e = 
    match e with 
    | Expr.App(_,fty,tyargs,args,_) -> applyTys g fty (tyargs,args)
    | Expr.Obj (_,ty,_,_,_,_,_)  
    | Expr.Match (_,_,_,_,_,ty) 
    | Expr.Quote(_,_,_,ty) 
    | Expr.Const(_,_,ty)              -> (ty)
    | Expr.Val(vref,_,_)  -> vref.Type
    | Expr.Seq(a,b,k,_,_) -> tyOfExpr g (match k with NormalSeq  -> b | ThenDoSeq -> a)
    | Expr.Lambda(_,_,_,vs,_,_,rty) -> (mkTupledVarsTy g vs --> rty)
    | Expr.TyLambda(_,tyvs,_,_,rty) -> (tyvs +-> rty)
    | Expr.Let(_,e,_,_) 
    | Expr.TyChoose(_,e,_)
    | Expr.Link { contents=e}
    | Expr.StaticOptimization (_,_,e,_) 
    | Expr.LetRec(_,e,_,_) -> tyOfExpr g e
    | Expr.Op (op,tinst,_,_) -> 
        match op with 
        | TOp.Coerce -> (match tinst with [to_ty;_fromTy] -> to_ty | _ -> failwith "bad TOp.Coerce node")
        | (TOp.ILCall (_,_,_,_,_,_,_,_,_,_,rtys) | TOp.ILAsm(_,rtys)) -> (match rtys with [h] -> h | _ -> g.unit_ty)
        | TOp.UnionCase uc -> actualResultTyOfUnionCase tinst uc 
        | TOp.UnionCaseProof uc -> mkProvenUnionCaseTy uc tinst  
        | TOp.Recd (_,tcref) -> mkAppTy tcref tinst
        | TOp.ExnConstr _ -> g.exn_ty
        | TOp.Bytes _ -> mkByteArrayTy g
        | TOp.UInt16s _ -> mkArrayType g g.uint16_ty
        | TOp.TupleFieldGet(i) -> List.nth tinst i
        | TOp.Tuple -> mkTupleTy tinst
        | (TOp.For _ | TOp.While _) -> g.unit_ty
        | TOp.Array -> (match tinst with [ty] -> mkArrayType g ty | _ -> failwith "bad TOp.Array node")
        | (TOp.TryCatch _ | TOp.TryFinally _) -> (match tinst with [ty] ->  ty | _ -> failwith "bad TOp_try node")
        | TOp.ValFieldGetAddr(fref) -> mkByrefTy g (actualTyOfRecdFieldRef fref tinst)
        | TOp.ValFieldGet(fref) -> actualTyOfRecdFieldRef fref tinst
        | (TOp.ValFieldSet _ | TOp.UnionCaseFieldSet _ | TOp.ExnFieldSet _ | TOp.LValueOp ((LSet | LByrefSet),_)) ->g.unit_ty
        | TOp.UnionCaseTagGet _ -> g.int_ty
        | TOp.UnionCaseFieldGet(cref,j) -> actualTyOfRecdField (mkTyconRefInst cref.TyconRef tinst) (cref.FieldByIndex j)
        | TOp.ExnFieldGet(ecref,j) -> recdFieldTyOfExnDefRefByIdx ecref j
        | TOp.LValueOp (LByrefGet, v) -> destByrefTy g v.Type
        | TOp.LValueOp (LGetAddr, v) -> mkByrefTy g v.Type
        | TOp.RefAddrGet -> (match tinst with [ty] -> mkByrefTy g ty | _ -> failwith "bad TOp.RefAddrGet node")      
        | TOp.TraitCall (TTrait(_,_,_,_,ty,_)) -> GetFSharpViewOfReturnType g ty
        | TOp.Reraise -> (match tinst with [rtn_ty] -> rtn_ty | _ -> failwith "bad TOp.Reraise node")
        | TOp.Goto _ | TOp.Label _ | TOp.Return -> 
            //assert false; 
            //errorR(InternalError("unexpected goto/label/return in tyOfExpr",m)); 
            // It doesn't matter what type we return here. THis is only used in free variable analysis in the code generator
            g.unit_ty

//--------------------------------------------------------------------------
// Make applications
//---------------------------------------------------------------------------

let primMkApp (f,fty) tyargs argsl m = 
  Expr.App(f,fty,tyargs,argsl,m)

let isExpansiveUnderInstantiation g fty' tyargs pargs =
    isForallTy g fty' && not (isFunTy g (formalApplyTys g fty' (tyargs,pargs)))
    
let rec mkExprApplAux g f fty argsl m =
  match argsl with 
  | [] -> f
  | _ -> 
      // Always combine the term application with a type application
      //
      // Combine the term application with a term application, but only when f' is an under-applied value of known arity
      //
      // In both cases check for the funky case where a generic type instantiation at function type causes a generic function
      // to appear to accept more arguments than it really does, e.g. "id id 1", where the first "id" is 
      // instantiated with "int -> int"
      match f with 
      | Expr.App(f',fty',tyargs,pargs,m2) 
             when
                 (isNil pargs ||
                  (match stripExpr f' with 
                   | Expr.Val(v,_,_) -> 
                       match v.ValReprInfo with 
                       | Some info -> info.NumCurriedArgs > pargs.Length
                       | None -> false
                   | _ -> false)) &&
                 not (isExpansiveUnderInstantiation g fty' tyargs pargs) ->
            primMkApp (f',fty') tyargs (pargs@argsl) (unionRanges m2 m)

      | _ -> 
          // Don't combine. 'f' is not an application
          if not (isFunTy g fty) then error(InternalError("expected a function type",m));
          primMkApp (f,fty) [] argsl m


let rec mkAppsAux g f fty tyargsl argsl m =
  match tyargsl with 
  | tyargs :: rest -> 
      match tyargs with 
      | [] -> mkAppsAux g f fty rest argsl m
      | _ -> 
        let arfty = applyForallTy g fty tyargs
        mkAppsAux g (primMkApp (f,fty) tyargs [] m) arfty rest argsl m
  | [] -> 
      mkExprApplAux g f fty argsl m
      
let mkApps g ((f,fty),tyargsl,argl,m) = mkAppsAux g f fty tyargsl argl m
let mkTyAppExpr m (f,fty) tyargs = match tyargs with [] -> f | _ -> primMkApp (f,fty) tyargs [] m 


//--------------------------------------------------------------------------
// Decision tree reduction
//--------------------------------------------------------------------------

let rec accTargetsOfDecisionTree tree acc =
  match tree with 
  | TDSwitch (_,edges,dflt,_) -> List.foldBack (fun (c:DecisionTreeCase) -> accTargetsOfDecisionTree c.CaseTree) edges (Option.foldBack accTargetsOfDecisionTree dflt acc)
  | TDSuccess (_,i) -> i::acc
  | TDBind (_,rest) -> accTargetsOfDecisionTree rest acc

let rec mapAccTipsOfDecisionTree f tree =
    match tree with 
    | TDSwitch (e,edges,dflt,m) -> TDSwitch (e,List.map (mapAccTipsOfEdge f) edges,Option.map (mapAccTipsOfDecisionTree f) dflt,m)
    | TDSuccess (es,i) -> f es i  
    | TDBind (bind,rest) -> TDBind(bind,mapAccTipsOfDecisionTree f rest)
and mapAccTipsOfEdge f (TCase(x,t)) = 
    TCase(x,mapAccTipsOfDecisionTree f t)

let mapTargetsOfDecisionTree f tree = mapAccTipsOfDecisionTree (fun es i -> TDSuccess(es, f i)) tree

// Dead target elimination 
let eliminateDeadTargetsFromMatch tree (targets:_[]) =
    let used = accTargetsOfDecisionTree tree [] |> ListSet.setify (=) |> Array.ofList
    if used.Length < targets.Length then
        Array.sortInPlace used;
        let ntargets = targets.Length
        let tree' = 
            let remap = Array.create ntargets (-1)
            Array.iteri (fun i tgn -> remap.[tgn] <- i) used;
            tree |> mapTargetsOfDecisionTree (fun tgn -> 
                 if remap.[tgn] = -1 then failwith "eliminateDeadTargetsFromMatch: failure while eliminating unused targets"; 
                 remap.[tgn]) 
        let targets' = Array.map (Array.get targets) used
        tree',targets'
    else 
        tree,targets
    


let rec targetOfSuccessDecisionTree tree =
    match tree with 
    | TDSwitch _ -> None
    | TDSuccess (_,i) -> Some i
    | TDBind(_,t) -> targetOfSuccessDecisionTree t

/// Check a decision tree only has bindings that immediately cover a 'Success'
let rec decisionTreeHasNonTrivialBindings tree =
    match tree with 
    | TDSwitch (_,edges,dflt,_) -> 
        edges |> List.exists (fun c -> decisionTreeHasNonTrivialBindings c.CaseTree) || 
        dflt |> Option.exists decisionTreeHasNonTrivialBindings 
    | TDSuccess _ -> false
    | TDBind (_,t) -> isNone (targetOfSuccessDecisionTree t)

// If a target has assignments and can only be reached through one 
// branch (i.e. is "linear"), then transfer the assignments to the r.h.s. to be a "let". 
let foldLinearBindingTargetsOfMatch tree targets =

    // Don't do this when there are any bindings in the tree except where those bindings immediately cover a success node
    // since the variables would be extruded from their scope. 
    if decisionTreeHasNonTrivialBindings tree then 
        tree,targets 

    else

        // Build a map showing how each target might be reached
        let rec accumulateTipsOfDecisionTree accBinds tree  acc =
            match tree with 
            | TDSwitch (_,edges,dflt,_) -> 
                assert (isNil accBinds)  // No switches under bindings
                List.foldBack (accumulateTipsOfCase accBinds) edges 
                    (Option.foldBack (accumulateTipsOfDecisionTree accBinds) dflt acc)
            | TDSuccess (es,i) -> 
                Map.add i ((List.rev accBinds,es) :: Map.tryFindMulti i acc) acc
            | TDBind (bind,rest) -> 
                accumulateTipsOfDecisionTree (bind::accBinds) rest acc

        and accumulateTipsOfCase accBinds (TCase(_,x)) acc = accumulateTipsOfDecisionTree accBinds x acc

        // Compute the targets that can only be reached one way
        let linearTips = 
            accumulateTipsOfDecisionTree [] tree Map.empty 
            |> Map.filter (fun _ v -> match v with [_] -> true | _ -> false)

        if linearTips.IsEmpty then 

            tree,targets

        else
            
            /// rebuild the decision tree, replacing 'bind-then-success' decision trees by TDSuccess nodes that just go to the target
            let rec rebuildDecisionTree tree =
                
                // Check if this is a bind-then-success tree
                match targetOfSuccessDecisionTree tree with
                | Some i when linearTips.ContainsKey i -> TDSuccess(FlatList.empty,i)
                | _ -> 
                    match tree with 
                    | TDSwitch (e,edges,dflt,m) -> TDSwitch (e,List.map rebuildDecisionTreeEdge edges,Option.map rebuildDecisionTree dflt,m)
                    | TDSuccess _ -> tree
                    | TDBind _ -> tree

            and rebuildDecisionTreeEdge (TCase(x,t)) =  
                TCase(x,rebuildDecisionTree t)

            let tree' =  rebuildDecisionTree tree

            /// rebuild the targets , replacing linear targets by ones that include all the 'let' bindings from the source
            let targets' = 
                targets |> Array.mapi (fun i (TTarget(vs,exprTarget,spTarget) as tg) -> 
                    match Map.tryFind i linearTips with 
                    | Some ((binds,es) :: _) -> 
                        // The value bindings are moved to become part of the target.
                        // Hence the expressions in the value bindings can be remarked with the range of the target.
                        let mTarget = exprTarget.Range
                        let es = es |> FlatList.map (remarkExpr mTarget)
                        TTarget(FlatList.empty,mkLetsBind mTarget binds (mkInvisibleLetsFromBindings mTarget vs es exprTarget),spTarget)
                    | _ -> tg )
     
            tree',targets'

// Simplify a little as we go, including dead target elimination 
let rec simplifyTrivialMatch spBind exprm matchm ty tree (targets : _[]) = 
    match tree with 
    | TDSuccess(es,n) -> 
        if n >= targets.Length then failwith "simplifyTrivialMatch: target out of range";
        let (TTarget(vs,rhs,_spTarget)) = targets.[n]
        if vs.Length <> es.Length then failwith ("simplifyTrivialMatch: invalid argument, n = "^string n^", List.length targets = "^string targets.Length);
        mkInvisibleLetsFromBindings rhs.Range vs es rhs
    | _ -> 
        primMkMatch (spBind,exprm,tree,targets,matchm,ty)
 
// Simplify a little as we go, including dead target elimination 
let mkAndSimplifyMatch spBind exprm matchm ty tree targets  = 
    let targets = Array.ofList targets
    match tree with 
    | TDSuccess _ -> 
        simplifyTrivialMatch spBind exprm matchm ty tree targets
    | _ -> 
        let tree,targets = eliminateDeadTargetsFromMatch tree targets
        let tree,targets = foldLinearBindingTargetsOfMatch tree targets
        simplifyTrivialMatch spBind exprm matchm ty tree targets


//-------------------------------------------------------------------------
// mkExprAddrOfExpr
//------------------------------------------------------------------------- 

type Mutates = DefinitelyMutates | PossiblyMutates | NeverMutates
exception DefensiveCopyWarning of string * range 

let isTyImmutable g ty =
    match tryDestAppTy g ty with 
    | None -> false
    | Some tcref -> 
      not (isTyconRefAllocObservable tcref) ||
      tyconRefEq g tcref g.decimal_tcr ||
      tyconRefEq g tcref g.date_tcr

// We can take the address of values of struct type even if the value is immutable
// under certain conditions
//   - all instances of the type are  known to be immutable; OR
//   - the operation is known not to mutate
//
// Note this may be taking the address of a closure field, i.e. a copy
// of the original struct, e.g. for
//    let f () = 
//        let g1 = A.G(1)
//        (fun () -> g1.x1)
//
// Note: isTyImmutable implies PossiblyMutates or NeverMutates
//
// We only do this for true local or closure fields because we can't take adddresses of immutable static 
// fields across assemblies.
let CanTakeAddressOfImmutableVal g (v:ValRef) mut =
    // We can take the address of values of struct type if the operation doesn't mutate 
    // and the value is a true local or closure field. 
    //
    assert (not v.IsMutable)
    not v.IsMutable &&
    not v.IsMemberOrModuleBinding &&
    (match mut with 
     | NeverMutates -> true 
     | PossiblyMutates -> isTyImmutable g v.Type 
     | DefinitelyMutates -> false)

let MustTakeAddressOfVal g (v:ValRef) = 
    v.IsMutable &&
    // We can only take the address of mutable values in the same assembly
    valRefInThisAssembly g.compilingFslib v

let MustTakeAddressOfRecdField (rfref: RecdFieldRef) = 
    // Static mutable fields must be private, hence we don't have to take their address
    not rfref.RecdField.IsStatic && 
    rfref.RecdField.IsMutable

let CanTakeAddressOfRecdField g (rfref: RecdFieldRef) mut tinst =
    mut <> DefinitelyMutates && 
    // We only do this if the field is defined in this assembly because we can't take adddresses across assemblies for immutable fields
    entityRefInThisAssembly g.compilingFslib rfref.TyconRef &&
    isTyImmutable g (actualTyOfRecdFieldRef rfref tinst)


let rec mkExprAddrOfExpr g mustTakeAddress useReadonlyForGenericArrayAddress mut e m =
    if not mustTakeAddress then (fun x -> x),e else
    match e with 
    // LVALUE: "x" where "x" is byref 
    | Expr.Op (TOp.LValueOp (LByrefGet, v), _,[], m) -> 
        (fun x -> x), exprForValRef m v
    // LVALUE: "x" where "x" is mutable local or operation doesn't mutate 
    // Note: we can always take the address of mutable values
    | Expr.Val(v, _,m) when MustTakeAddressOfVal g v || CanTakeAddressOfImmutableVal g v mut ->
        (fun x -> x), mkValAddr m v
    // LVALUE: "x" where "e.x" is mutable record field. "e" may be an lvalue 
    | Expr.Op (TOp.ValFieldGet rfref, tinst,[e],m) when MustTakeAddressOfRecdField rfref || CanTakeAddressOfRecdField g rfref mut tinst ->
        let exprty = tyOfExpr g e
        let wrap,expra = mkExprAddrOfExpr g (isStructTy g exprty) false mut e m
        wrap, mkRecdFieldGetAddrViaExprAddr(expra,rfref,tinst,m)

    // LVALUE: "x" where "e.x" is a .NET static field. 
    | Expr.Op (TOp.ILAsm ([IL.I_ldsfld(_vol,fspec)],[ty2]), tinst,[],m) -> 
        (fun x -> x),Expr.Op (TOp.ILAsm ([IL.I_ldsflda(fspec)],[mkByrefTy g ty2]), tinst,[],m)

    // LVALUE: "x" where "e.x" is a .NET instance field. "e" may be an lvalue 
    | Expr.Op (TOp.ILAsm ([IL.I_ldfld(_align,_vol,fspec)],[ty2]), tinst,[e],m) 
       -> 
        let exprty = tyOfExpr g e
        let wrap,expra = mkExprAddrOfExpr g (isStructTy g exprty) false mut e m
        wrap,Expr.Op (TOp.ILAsm ([IL.I_ldflda(fspec)],[mkByrefTy g ty2]), tinst,[expra],m)

    // LVALUE: "x" where "x" is mutable static field. 
    | Expr.Op (TOp.ValFieldGet rfref, tinst,[],m) when MustTakeAddressOfRecdField rfref || CanTakeAddressOfRecdField g rfref mut tinst ->
        (fun x -> x), mkStaticRecdFieldGetAddr(rfref,tinst,m)

    // LVALUE:  "e.[n]" where e is an array of structs 
    | Expr.App(Expr.Val(vf,_,_),_,[elemTy],[aexpr;nexpr],_) 
         when (valRefEq g vf g.array_get_vref) -> 
        
        let shape = ILArrayShape.SingleDimensional
        let readonly = if isTyparTy g elemTy &&  useReadonlyForGenericArrayAddress then ReadonlyAddress else NormalAddress
        (fun x -> x), Expr.Op (TOp.ILAsm ([IL.I_ldelema(readonly,shape,mkILTyvarTy 0us)],[mkByrefTy g elemTy]), [elemTy],[aexpr;nexpr],m)

    // LVALUE:  "e.[n1,n2]", "e.[n1,n2,n3]", "e.[n1,n2,n3,n4]" where e is an array of structs 
    | Expr.App(Expr.Val(vf,_,_),_,[elemTy],(aexpr::args),_) 
         when (valRefEq g vf g.array2D_get_vref || valRefEq g vf g.array3D_get_vref || valRefEq g vf g.array4D_get_vref) -> 
        
        let shape = ILArrayShape.FromRank args.Length
        let readonly = if isTyparTy g elemTy &&  useReadonlyForGenericArrayAddress then ReadonlyAddress else NormalAddress

        (fun x -> x), Expr.Op (TOp.ILAsm ([IL.I_ldelema(readonly,shape,mkILTyvarTy 0us)],[mkByrefTy g elemTy]), [elemTy],(aexpr::args),m)

    // Give a nice error message for DefinitelyMutates on immutable values, or mutable values in other assemblies
    | Expr.Val(v, _,m) when mut = DefinitelyMutates
       -> 
        if isByrefTy g v.Type then error(Error(FSComp.SR.tastUnexpectedByRef(),m));
        if v.IsMutable then 
            error(Error(FSComp.SR.tastInvalidAddressOfMutableAcrossAssemblyBoundary(),m));
        else 
            error(Error(FSComp.SR.tastValueMustBeLocalAndMutable(),m));
         
    | _ -> 
        let ty = tyOfExpr g e
        if isStructTy g ty then 
            match mut with 
            | NeverMutates -> ()
            | DefinitelyMutates -> 
                errorR(Error(FSComp.SR.tastInvalidMutationOfConstant(),m));
            | PossiblyMutates -> 
                warning(DefensiveCopyWarning(FSComp.SR.tastValueHasBeenCopied(),m));
        let tmp,_ = mkMutableCompGenLocal m "copyOfStruct" ty
        (fun rest -> mkCompGenLet m tmp e rest), (mkValAddr m (mkLocalValRef tmp))        

let mkRecdFieldGet g (e,fref:RecdFieldRef,tinst,finst,m) = 
    let ftyp = actualTyOfRecdFieldRef fref tinst
    let wrap,e' = mkExprAddrOfExpr g fref.Tycon.IsStructOrEnumTycon false NeverMutates e m 
    wrap (mkTyAppExpr m (mkRecdFieldGetViaExprAddr(e',fref,tinst,m), ftyp) finst)

let mkRecdFieldSet g (e,fref:RecdFieldRef,tinst,e2,m) = 
    let wrap,e' = mkExprAddrOfExpr g fref.Tycon.IsStructOrEnumTycon false DefinitelyMutates e m 
    wrap (mkRecdFieldSetViaExprAddr(e',fref,tinst,e2,m))

//---------------------------------------------------------------------------
// Compute fixups for letrec's.
//
// Generate an assignment expression that will fixup the recursion 
// amongst the vals on the r.h.s. of a letrec.  The returned expressions 
// include disorderly constructs such as expressions/statements 
// to set closure environments and non-mutable fields. These are only ever 
// generated by the backend code-generator when processing a "letrec"
// construct.
//
// [self] is the top level value that is being fixed
// [exprToFix] is the r.h.s. expression
// [rvs] is the set of recursive vals being bound. 
// [acc] accumulates the expression right-to-left. 
//
// Traversal of the r.h.s. term must happen back-to-front to get the
// uniq's for the lambdas correct in the very rare case where the same lambda
// somehow appears twice on the right.
//---------------------------------------------------------------------------

let rec IterateRecursiveFixups g (selfv : Val option) rvs ((access : Expr),set) exprToFix  = 
  let exprToFix =  stripExpr exprToFix
  match exprToFix with 
  | Expr.Const _ -> ()
  | Expr.Op (TOp.Tuple,argtys,args,m) ->
      args |> List.iteri (fun n -> 
          IterateRecursiveFixups g None rvs 
            (mkTupleFieldGet(access,argtys,n,m), 
            (fun e -> 
              // NICE: it would be better to do this check in the type checker 
              errorR(Error(FSComp.SR.tastRecursiveValuesMayNotBeInConstructionOfTuple(),m));
              e)))

  | Expr.Op (TOp.UnionCase (c),tinst,args,m) ->
      args |> List.iteri (fun n -> 
          IterateRecursiveFixups g None rvs 
            (mkUnionCaseFieldGetUnproven(access,c,tinst,n,m), 
             (fun e -> 
               // NICE: it would be better to do this check in the type checker 
               let tcref = c.TyconRef
               errorR(Error(FSComp.SR.tastRecursiveValuesMayNotAppearInConstructionOfType(tcref.LogicalName),m));
               mkUnionCaseFieldSet(access,c,tinst,n,e,m))))

  | Expr.Op (TOp.Recd (_,tcref),tinst,args,m) -> 
      (tcref.TrueInstanceFieldsAsRefList, args) ||> List.iter2 (fun fref arg -> 
          let fspec = fref.RecdField
          IterateRecursiveFixups g None rvs 
            (mkRecdFieldGetViaExprAddr(access,fref,tinst,m), 
             (fun e -> 
               // NICE: it would be better to do this check in the type checker 
               if not fspec.IsMutable && not (entityRefInThisAssembly g.compilingFslib tcref) then
                 errorR(Error(FSComp.SR.tastRecursiveValuesMayNotBeAssignedToNonMutableField(fspec.rfield_id.idText, tcref.LogicalName),m));
               mkRecdFieldSet g (access,fref,tinst,e,m))) arg )
  | Expr.Val _
  | Expr.Lambda _
  | Expr.Obj _
  | Expr.TyChoose _
  | Expr.TyLambda _ -> 
      rvs selfv access set exprToFix
  | _ -> ()




//--------------------------------------------------------------------------
// computations on constraints
//-------------------------------------------------------------------------- 

let JoinTyparStaticReq r1 r2 = 
  match r1,r2 with
  | NoStaticReq,r | r,NoStaticReq -> r 
  | HeadTypeStaticReq,r | r,HeadTypeStaticReq -> r
  


//-------------------------------------------------------------------------
// ExprFolder - fold steps
//-------------------------------------------------------------------------

type ExprFolder<'T> = 
    { exprIntercept    : ('T -> Expr -> 'T) -> 'T -> Expr  -> 'T option;   
      // the bool is 'bound in dtree' 
      valBindingSiteIntercept          : 'T -> bool * Val  -> 'T;                     
      // these values are always bound to these expressions. bool indicates 'recursively' 
      nonRecBindingsIntercept         : 'T -> Binding -> 'T;         
      recBindingsIntercept         : 'T -> Bindings -> 'T;         
      dtreeIntercept         : 'T -> DecisionTree -> 'T;                     
      targetIntercept  : ('T -> Expr -> 'T) -> 'T -> DecisionTreeTarget  -> 'T option; 
      tmethodIntercept : ('T -> Expr -> 'T) -> 'T -> ObjExprMethod -> 'T option; 
    }

let ExprFolder0 =
    { exprIntercept    = (fun _exprF _z _x -> None);
      valBindingSiteIntercept          = (fun z _b  -> z);
      nonRecBindingsIntercept         = (fun z _bs -> z);
      recBindingsIntercept         = (fun z _bs -> z);
      dtreeIntercept         = (fun z _dt -> z);
      targetIntercept  = (fun _exprF _z _x -> None);
      tmethodIntercept = (fun _exprF _z _x -> None); }


//-------------------------------------------------------------------------
// FoldExpr
//-------------------------------------------------------------------------

/// Adapted from usage info folding.
/// Collecting from exprs at moment.
/// To collect ids etc some additional folding needed, over formals etc.
let mkFolders (folders : _ ExprFolder) =
    let {exprIntercept             = exprIntercept; 
         valBindingSiteIntercept   = valBindingSiteIntercept;
         nonRecBindingsIntercept   = nonRecBindingsIntercept;
         recBindingsIntercept      = recBindingsIntercept;
         dtreeIntercept            = dtreeIntercept;
         targetIntercept           = targetIntercept;
         tmethodIntercept          = tmethodIntercept} = folders
    let rec exprsF z xs = List.fold exprF z xs
    and flatExprsF z xs = FlatList.fold exprF z xs
    and exprF z x =
        match exprIntercept exprF z x with // fold this node, then recurse 
        | Some z -> z // intercepted 
        | None ->     // structurally recurse 
            match x with
            | Expr.Const _  -> z
            | Expr.Val _ -> z
            | Expr.Op (_c,_tyargs,args,_) -> exprsF z args
            | Expr.Seq (x0,x1,_dir,_,_)  -> exprsF z [x0;x1]
            | Expr.Lambda(_lambdaId ,_ctorThisValOpt,_baseValOpt,_argvs,body,_m,_rty) -> exprF  z body
            | Expr.TyLambda(_lambdaId,_argtyvs,body,_m,_rty) -> exprF  z body
            | Expr.TyChoose(_,body,_) -> exprF  z body
            | Expr.App (f,_fty,_tys,argtys,_) -> 
                let z = exprF z f
                let z = exprsF z argtys
                z
            | Expr.LetRec (binds,body,_,_) -> 
                let z = valBindsF false z binds
                let z = exprF z body
                z
            | Expr.Let    (bind,body,_,_)  -> 
                let z = valBindF false z bind
                let z = exprF z body
                z
            | Expr.Link rX -> exprF z (!rX)
            | Expr.Match (_spBind,_exprm,dtree,targets,_m,_ty)                 -> 
                let z = dtreeF z dtree
                let z = Array.fold targetF z targets
                z
            | Expr.Quote(_e,{contents=Some(_argTypes,argExprs,_)},_,_)  -> exprsF z argExprs
            | Expr.Quote(_e,{contents=None},_m,_) -> z
            | Expr.Obj (_n,_typ,_basev,basecall,overrides,iimpls,_m)    -> 
                let z = exprF z basecall
                let z = List.fold tmethodF z overrides
                let z = List.fold (foldOn snd (List.fold tmethodF)) z iimpls
                z
            | Expr.StaticOptimization (_tcs,csx,x,_) -> exprsF z [csx;x]

    and valBindF dtree z bind =
        let z = nonRecBindingsIntercept z bind
        bindF dtree z bind 

    and valBindsF dtree z binds =
        let z = recBindingsIntercept z binds
        FlatList.fold (bindF dtree) z binds 

    and bindF dtree z (bind:Binding) =
        let z = valBindingSiteIntercept z (dtree,bind.Var)
        exprF z bind.Expr

    and dtreeF z dtree =
        let z = dtreeIntercept z dtree
        match dtree with
        | TDBind (bind,rest)            -> 
            let z = valBindF true z bind
            dtreeF z rest
        | TDSuccess (args,_)            -> flatExprsF z args
        | TDSwitch (test,dcases,dflt,_) -> 
            let z = exprF z test
            let z = List.fold dcaseF z dcases
            let z = Option.fold dtreeF z dflt
            z

    and dcaseF z = function
        TCase (_,dtree)   -> dtreeF z dtree (* not collecting from test *)

    and targetF z x =
        match targetIntercept exprF z x with 
        | Some z -> z // intercepted 
        | None ->     // structurally recurse 
            let (TTarget (_,body,_)) = x
            exprF z body
              
    and tmethodF z x =
        match tmethodIntercept exprF z x with 
        | Some z -> z // intercepted 
        | None ->     // structurally recurse 
            let (TObjExprMethod(_,_,_,_,e,_)) = x
            exprF z e

    and mexprF z x =
        match x with 
        | ModuleOrNamespaceExprWithSig(_,def,_) -> mdefF z def

    and mdefF z x = 
        match x with
        | TMDefRec(_,binds,mbinds,_) -> 
            let z = valBindsF false z binds
            let z = List.fold mbindF z mbinds
            z
        | TMDefLet(bind,_) -> valBindF false z bind
        | TMDefDo(e,_) -> exprF z e
        | TMDefs defs -> List.fold mdefF z defs 
        | TMAbstract x -> mexprF z x

    and mbindF z (ModuleOrNamespaceBinding(_, def)) = mdefF z def

    and implF z x = foldTImplFile mexprF z x

    and implsF z (TAssembly(x)) = List.fold implF z x
   
    exprF, implF,implsF

let FoldExpr     folders = let exprF,_,_ = mkFolders folders in exprF
let FoldImplFile folders = let _,implF,_ = mkFolders folders in implF

#if DEBUG
//-------------------------------------------------------------------------
// ExprStats
//-------------------------------------------------------------------------

let ExprStats x =
  let count = ref 0
  let folders = {ExprFolder0 with exprIntercept = (fun _ _ _ -> (count := !count + 1; None))}
  let () = FoldExpr folders () x
  string !count ^ " TExpr nodes"
#endif
    
//-------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

let mkString g m n = Expr.Const(Const.String n,m,g.string_ty)
let mkBool g m b = Expr.Const(Const.Bool b,m,g.bool_ty)
let mkByte g m b = Expr.Const(Const.Byte b,m,g.byte_ty)
let mkUInt16 g m b = Expr.Const(Const.UInt16 b,m,g.uint16_ty)
let mkTrue g m = mkBool g m true
let mkFalse g m = mkBool g m false
let mkUnit g m = Expr.Const(Const.Unit,m,g.unit_ty)
let mkInt32 g m n =  Expr.Const(Const.Int32 n,m,g.int32_ty)
let mkInt g m n =  mkInt32 g m (n)
let mkZero g m =  mkInt g m 0
let mkOne g m =  mkInt g m 1
let mkTwo g m =  mkInt g m 2
let mkMinusOne g  m =  mkInt g m (-1)

let destInt32 = function Expr.Const(Const.Int32 n,_,_) -> Some n | _ -> None

let isIDelegateEventType g ty     = isAppTy g ty && tyconRefEq g g.fslib_IDelegateEvent_tcr (tcrefOfAppTy g ty)
let destIDelegateEventType g ty   = 
    if isIDelegateEventType g ty then 
        match argsOfAppTy g ty with 
        | [ty1] -> ty1
        | _ -> failwith "destIDelegateEventType: internal error"
    else failwith "destIDelegateEventType: not an IDelegateEvent type"
let mkIEventType g ty1 ty2 = TType_app (g.fslib_IEvent2_tcr, [ty1;ty2])
let mkIObservableType g ty1 = TType_app (g.tcref_IObservable, [ty1])
let mkIObserverType g ty1 = TType_app (g.tcref_IObserver, [ty1])

let mkRefCellContentsRef g  = mkRecdFieldRef g.refcell_tcr_canon "contents"

let mkSeq spSeq m e1 e2 = Expr.Seq(e1,e2,NormalSeq,spSeq,m)
let mkCompGenSequential m e1 e2 = mkSeq SuppressSequencePointOnExprOfSequential m e1 e2
let rec mkSeqList spSeq g m es = 
    match es with 
    | [e] -> e 
    | e::es -> mkSeq spSeq m e (mkSeqList spSeq g m es) 
    | [] -> mkUnit g m

let mkGetArg0 m ty = mkAsmExpr( [ mkLdarg0 ],[],[],[ty],m) 

//-------------------------------------------------------------------------
// Tuples...
//------------------------------------------------------------------------- 
 
let mkTupled g m es tys = 
    match es with 
    | [] -> mkUnit g m 
    | [e] -> e
    | _ -> Expr.Op (TOp.Tuple,tys,es,m)

let mkTupledNoTypes g m args = mkTupled g m args (List.map (tyOfExpr g) args)

let mkTupledVars g m vs = mkTupled g m (List.map (exprForVal m) vs) (typesOfVals vs)

//--------------------------------------------------------------------------
// Permute expressions
//--------------------------------------------------------------------------
    
let inversePerm (sigma:int array) =
    let n = sigma.Length
    let invSigma = Array.create n -1
    for i = 0 to n-1 do
        let sigma_i = sigma.[i]
        // assert( invSigma.[sigma_i] = -1 ); 
        invSigma.[sigma_i] <- i
    invSigma
  
let permute (sigma:int[]) (data:'T[]) = 
    let n = sigma.Length
    let invSigma = inversePerm sigma
    Array.init n (fun i -> data.[invSigma.[i]])
  
let rec existsR a b pred = if a<=b then pred a || existsR (a+1) b pred else false

let mapFoldListi f z xs =
    let rec fmapi f i z l = 
        match l with 
        | []    -> z,[]
        | x::xs -> let z,x  = f i z x
                   let z,xs = fmapi f (i+1) z xs
                   z,x::xs   
    fmapi f 0 z xs

/// Given expr = xi = [| x0; ... xN |]
/// Given sigma a permutation to apply to the xi.
/// Return (bindings',expr') such that:
///   (a) xi are permutated under sigma, xi -> position sigma(i).
///------
/// Motivation:
///   opt.fs    - put record field assignments in order under known effect information
///   ilxgen.fs - put record field assignments in order if necessary (no optimisations)
///               under unknown-effect information.
let permuteExpr (sigma:int[]) (expr: Expr[]) (typ: TType[]) (names:string[]) =
    let invSigma = inversePerm sigma
    let liftPosition i =
        // Lift out xi if      
        //    LC2: xi goes to position that will be preceded by
        //         an expr with an effect that originally followed xi
        let i' = sigma.[i]
        existsR 0 (i' - 1) (fun j' -> invSigma.[j'] > i)
   
    let rewrite i rbinds (xi:Expr) =
        if liftPosition i then
            let tmpv,tmpe = mkCompGenLocal xi.Range names.[i] typ.[i]
            let bind = mkCompGenBind tmpv xi
            bind :: rbinds,tmpe
        else
            rbinds,xi
 
    let xis = Array.toList expr
    let rbinds,xis = mapFoldListi rewrite [] xis
    let binds = List.rev rbinds
    let expr  = permute sigma (Array.ofList xis)
    binds,expr
    
let permuteExprList (sigma:int array) (expr: Expr list) (typ: TType list)  (names:string list) =
    let binds,expr = permuteExpr sigma (Array.ofList expr) (Array.ofList typ)  (Array.ofList names)
    binds,Array.toList expr
  
//-------------------------------------------------------------------------
// Build record expressions...
//------------------------------------------------------------------------- 


/// Evaluate the expressions in the original order, but build a record with the results in field order 
/// Note some fields may be static. If this were not the case we could just use 
///     let sigma       = Array.map #Index  ()  
/// However the presence of static fields means .Index may index into a non-compact set of instance field indexes. 
/// We still need to sort by index. 
let mkRecordExpr g (lnk,tcref,tinst,rfrefs:RecdFieldRef list,args,m) =  
    // Remove any abbreviations 
    let tcref,tinst = destAppTy g (mkAppTy tcref tinst)
    
    let rfrefsArray = rfrefs |> List.mapi (fun i x -> (i,x)) |> Array.ofList
    rfrefsArray |> Array.sortInPlaceBy (fun (_,r) -> r.Index) ;
    let sigma = Array.create rfrefsArray.Length -1
    Array.iteri (fun j (i,_) -> 
        if sigma.[i] <> -1 then error(InternalError("bad permutation",m));
        sigma.[i] <- j)  rfrefsArray;
    
    let argTyps     = List.map (fun rfref  -> actualTyOfRecdFieldRef rfref tinst) rfrefs
    let names       = rfrefs |> List.map (fun rfref -> rfref.FieldName)
    let binds,args  = permuteExprList sigma args argTyps names
    mkLetsBind m binds (Expr.Op (TOp.Recd(lnk,tcref),tinst,args,m))
  

//-------------------------------------------------------------------------
// List builders
//------------------------------------------------------------------------- 
 
let mkRefCell     g m ty e = mkRecordExpr g (RecdExpr,g.refcell_tcr_canon,[ty],[mkRefCellContentsRef g],[e],m)
let mkRefCellGet g m ty e = mkRecdFieldGet g (e,mkRefCellContentsRef g,[ty],[],m)
let mkRefCellSet g m ty e1 e2 = mkRecdFieldSet g (e1,mkRefCellContentsRef g,[ty],e2,m)

let mkNil g m ty = mkUnionCaseExpr (g.nil_ucref,[ty],[],m)
let mkCons g ty h t = mkUnionCaseExpr (g.cons_ucref,[ty],[h;t],unionRanges h.Range t.Range)

let mkCompGenLocalAndInvisbleBind g nm m e = 
    let locv,loce = mkCompGenLocal m nm (tyOfExpr g e)
    locv,loce,mkInvisibleBind locv e 

//----------------------------------------------------------------------------
// Make some fragments of code
//----------------------------------------------------------------------------

let box = IL.I_box (mkILTyvarTy 0us)
let isinst = IL.I_isinst (mkILTyvarTy 0us)
let unbox = IL.I_unbox_any (mkILTyvarTy 0us)
let mkUnbox ty e m = mkAsmExpr ([ unbox ], [ty],[e], [ ty ], m)
let mkBox ty e m = mkAsmExpr ([box],[],[e],[ty],m)
let mkIsInst ty e m = mkAsmExpr ([ isinst ], [ty],[e], [ ty ], m)

let mspec_Object_GetHashCode     ilg = IL.mkILNonGenericInstanceMethSpecInTy(ilg.typ_Object,"GetHashCode",[],ilg.typ_int32)
let mspec_Type_GetTypeFromHandle ilg = IL.mkILNonGenericStaticMethSpecInTy(ilg.typ_Type,"GetTypeFromHandle",[ilg.typ_RuntimeTypeHandle],ilg.typ_Type)
let fspec_Missing_Value  ilg = IL.mkILFieldSpecInTy(ilg.typ_Missing,"Value",ilg.typ_Missing)


let typedExprForIntrinsic _g m (IntrinsicValRef(_,_,_,ty,_) as i) =
    let vref = ValRefForIntrinsic i
    exprForValRef m vref,ty

let mkCallGetGenericComparer g m = typedExprForIntrinsic g m g.get_generic_comparer_info |> fst
let mkCallGetGenericEREqualityComparer g m = typedExprForIntrinsic g m g.get_generic_er_equality_comparer_info |> fst
let mkCallGetGenericPEREqualityComparer g m = typedExprForIntrinsic g m g.get_generic_per_equality_comparer_info |> fst
let mkCallUnbox                g m ty e1    = mkApps g (typedExprForIntrinsic g m g.unbox_info,       [[ty]], [ e1 ],  m)
let mkCallUnboxFast            g m ty e1    = mkApps g (typedExprForIntrinsic g m g.unbox_fast_info,  [[ty]], [ e1 ],  m)
let mkCallTypeTest             g m ty e1    = mkApps g (typedExprForIntrinsic g m g.istype_info,      [[ty]], [ e1 ],  m)
let mkCallTypeOf               g m ty       = mkApps g (typedExprForIntrinsic g m g.typeof_info,      [[ty]], [ ],  m)
let mkCallTypeDefOf            g m ty       = mkApps g (typedExprForIntrinsic g m g.typedefof_info,   [[ty]], [ ],  m)

     
let mkCallDispose              g m ty e1         = mkApps g (typedExprForIntrinsic g m g.dispose_info,                  [[ty]], [ e1 ],  m)
let mkCallSeq                  g m ty e1         = mkApps g (typedExprForIntrinsic g m g.seq_info,                      [[ty]], [ e1 ],  m)
let mkCallCreateInstance       g m ty            = mkApps g (typedExprForIntrinsic g m g.create_instance_info,          [[ty]], [ mkUnit g m ],  m)

let mkCallCreateEvent                        g m ty1 ty2 e1 e2 e3 = mkApps g (typedExprForIntrinsic g m g.create_event_info,          [[ty1;ty2]], [ e1;e2;e3 ],  m)
let mkCallGenericComparisonWithComparerOuter g m ty comp e1 e2    = mkApps g (typedExprForIntrinsic g m g.generic_comparison_withc_outer_info, [[ty]], [  comp;e1;e2 ],  m)
let mkCallEqualsOperator                     g m ty e1 e2         = mkApps g (typedExprForIntrinsic g m g.equals_operator_info,        [[ty]], [  e1;e2 ],  m)
let mkCallGenericEqualityEROuter             g m ty e1 e2         = mkApps g (typedExprForIntrinsic g m g.generic_equality_er_outer_info,        [[ty]], [  e1;e2 ],  m)
let mkCallGenericEqualityWithComparerOuter   g m ty comp e1 e2    = mkApps g (typedExprForIntrinsic g m g.generic_equality_withc_outer_info,  [[ty]], [comp;e1;e2], m)
let mkCallGenericHashWithComparerOuter       g m ty comp e1       = mkApps g (typedExprForIntrinsic g m g.generic_hash_withc_outer_info,    [[ty]], [comp;e1], m)

let mkCallArrayGet   g m ty e1 e2                  = mkApps g (typedExprForIntrinsic g m g.array_get_info, [[ty]], [ e1 ; e2 ],  m)
let mkCallArray2DGet g m ty e1 idx1 idx2           = mkApps g (typedExprForIntrinsic g m g.array2D_get_info, [[ty]], [ e1 ; idx1; idx2 ],  m)
let mkCallArray3DGet g m ty e1 idx1 idx2 idx3      = mkApps g (typedExprForIntrinsic g m g.array3D_get_info, [[ty]], [ e1 ; idx1; idx2; idx3 ],  m)
let mkCallArray4DGet g m ty e1 idx1 idx2 idx3 idx4 = mkApps g (typedExprForIntrinsic g m g.array4D_get_info, [[ty]], [ e1 ; idx1; idx2; idx3; idx4 ],  m)
let mkCallNewDecimal g m (e1,e2,e3,e4,e5)          = mkApps g (typedExprForIntrinsic g m g.new_decimal_info, [], [ e1;e2;e3;e4;e5 ],  m)

let mkCallNewFormat g m aty bty cty dty ety e1    = mkApps g (typedExprForIntrinsic g m g.new_format_info, [[aty;bty;cty;dty;ety]], [ e1 ],  m)
let mkCallRaise     g m aty e1    = mkApps g (typedExprForIntrinsic g m g.raise_info, [[aty]], [ e1 ],  m)

let TryEliminateDesugaredConstants g m c = 
    match c with 
    | Const.Decimal d -> 
        match System.Decimal.GetBits(d) with 
        | [| lo;med;hi; signExp |] -> 
            let scale = (min (((signExp &&& 0xFF0000) >>> 16) &&& 0xFF) 28) |> byte
            let isNegative = (signExp &&& 0x80000000) <> 0
            Some(mkCallNewDecimal g m (mkInt g m lo,mkInt g m med,mkInt g m hi,mkBool g m isNegative,mkByte g m scale) )
        | _ -> failwith "unreachable"
    | _ -> 
        None

let mkSeqTy g ty = mkAppTy g.seq_tcr [ty] 
let mkIEnumeratorTy g ty = mkAppTy g.tcref_System_Collections_Generic_IEnumerator [ty] 

let mkCallSeqCollect g m alphaTy betaTy arg1 arg2 = 
    let enumty2 = try rangeOfFunTy g (tyOfExpr g arg1) with _ -> (* defensive programming *) (mkSeqTy g betaTy)
    mkApps g (typedExprForIntrinsic g m g.seq_collect_info, [[alphaTy;enumty2;betaTy]], [ arg1; arg2 ],  m) 
                  
let mkCallSeqUsing g m resourceTy elemTy arg1 arg2 = 
    // We're intantiating val using : 'a -> ('a -> 'sb) -> seq<'b> when 'sb :> seq<'b> and 'a :> IDisposable 
    // We set 'sb -> range(typeof(arg2)) 
    let enumty = try rangeOfFunTy g (tyOfExpr g arg2) with _ -> (* defensive programming *) (mkSeqTy g elemTy)
    mkApps g (typedExprForIntrinsic g m g.seq_using_info, [[resourceTy;enumty;elemTy]], [ arg1; arg2 ],  m) 
                  
let mkCallSeqDelay g m elemTy arg1 = 
    mkApps g (typedExprForIntrinsic g m g.seq_delay_info, [[elemTy]], [ arg1 ],  m) 
                  
let mkCallSeqAppend g m elemTy arg1 arg2 = 
    mkApps g (typedExprForIntrinsic g m g.seq_append_info, [[elemTy]], [ arg1; arg2 ],  m) 

let mkCallSeqGenerated g m elemTy arg1 arg2 = 
    mkApps g (typedExprForIntrinsic g m g.seq_generated_info, [[elemTy]], [ arg1; arg2 ],  m) 
                       
let mkCallSeqFinally g m elemTy arg1 arg2 = 
    mkApps g (typedExprForIntrinsic g m g.seq_finally_info, [[elemTy]], [ arg1; arg2 ],  m) 
                       
let mkCallSeqOfFunctions g m ty1 ty2 arg1 arg2 arg3 = 
    mkApps g (typedExprForIntrinsic g m g.seq_of_functions_info, [[ty1;ty2]], [ arg1; arg2; arg3  ],  m) 
                  
let mkCallSeqToArray g m elemTy arg1 =  
    mkApps g (typedExprForIntrinsic g m g.seq_to_array_info, [[elemTy]], [ arg1 ],  m) 
                  
let mkCallSeqToList g m elemTy arg1 = 
    mkApps g (typedExprForIntrinsic g m g.seq_to_list_info, [[elemTy]], [ arg1 ],  m) 
                  
let mkCallSeqMap g m inpElemTy genElemTy arg1 arg2 = 
    mkApps g (typedExprForIntrinsic g m g.seq_map_info, [[inpElemTy;genElemTy]], [ arg1; arg2 ],  m) 
                  
let mkCallSeqSingleton g m ty1 arg1 = 
    mkApps g (typedExprForIntrinsic g m g.seq_singleton_info, [[ty1]], [ arg1 ],  m) 
                  
let mkCallSeqEmpty g m ty1 = 
    mkApps g (typedExprForIntrinsic g m g.seq_empty_info, [[ty1]], [ ],  m) 
                 
let mkCallUnpickleQuotation g m e1 e2 e3 e4 = 
    let args = [ e1; e2; e3; e4 ]
    mkApps g (typedExprForIntrinsic g m g.unpickle_quoted_info, [], [ mkTupledNoTypes g m args ],  m)

let mkCallCastQuotation g m ty e1 = 
    mkApps g (typedExprForIntrinsic g m g.cast_quotation_info, [[ty]], [ e1 ],  m)

let mkCallLiftValue g m ty e1 = 
    mkApps g (typedExprForIntrinsic g m g.lift_value_info , [[ty]], [ e1],  m)

let mkCallCheckThis g m ty e1 = 
    mkApps g (typedExprForIntrinsic g m g.check_this_info, [[ty]], [e1],  m)

let mkCallFailInit g m = 
    mkApps g (typedExprForIntrinsic g m g.fail_init_info , [], [mkUnit g m],  m)

let mkCallFailStaticInit g m = 
    mkApps g (typedExprForIntrinsic g m g.fail_static_init_info , [], [mkUnit g m],  m)

let mkLazyDelayed g m ty f = mkApps g (typedExprForIntrinsic g m g.lazy_create_info, [[ty]], [ f ],  m) 
let mkLazyForce g m ty e = mkApps g (typedExprForIntrinsic g m g.lazy_force_info, [[ty]], [ e; mkUnit g m ],  m) 

// Quotations can't contain any IL.
// As a result, we aim to get rid of all IL generation in the typechecker and pattern match
// compiler, or else train the quotation generator to understand the generated IL. 
// Hence each of the following are marked with places where they are generated.

// Generated by the optimizer and the encoding of 'for' loops     
let mkDecr g m e = mkAsmExpr([ IL.AI_sub  ],[],[e; mkOne g m],[g.int_ty],m)
let mkIncr g m e = mkAsmExpr([ IL.AI_add  ],[],[mkOne g m; e],[g.int_ty],m)

// Generated by the pattern match compiler and the optimizer for
//    1. array patterns
//    2. optimizations associated with getting 'for' loops into the shape expected by the JIT.
// 
// NOTE: The conv.i4 assumes that int_ty is int32. Note: ldlen returns native UNSIGNED int 
let mkLdlen g m arre = mkAsmExpr ([ IL.I_ldlen; (IL.AI_conv IL.DT_I4) ],[],[ arre ], [ g.int_ty ], m)
let mkLdelem (_g:TcGlobals) m ty arre idxe = mkAsmExpr ([ IL.I_ldelem_any (ILArrayShape.SingleDimensional, mkILTyvarTy 0us) ],[ty],[ arre;idxe ], [ ty ], m)

// This is generated in equality/compare/hash augmentations and in the pattern match compiler.
// It is understood by the quotation processor and turned into "Equality" nodes.
//
// Note: this is IL assembly code, don't go inserting this in expressions which will be exposed via quotations
let mkILAsmCeq g m e1 e2 = mkAsmExpr ([ IL.AI_ceq  ],[],  [e1; e2],[g.bool_ty],m)
let mkILAsmClt g m e1 e2 = mkAsmExpr ([ IL.AI_clt  ],[],  [e1; e2],[g.bool_ty],m)

// This is generated in the initialization of the "ctorv" field in the typechecker's compilation of
// an implicit class construction.
let mkNull m ty = Expr.Const(Const.Zero, m,ty)

//----------------------------------------------------------------------------
// rethrow
//----------------------------------------------------------------------------

(* throw, rethrow *)
let mkThrow m ty e = mkAsmExpr ([ IL.I_throw ],[], [e],[ty],m)
let destThrow = function
    | Expr.Op (TOp.ILAsm([IL.I_throw],[ty2]),[],[e],m) -> Some (m,ty2,e)
    | _ -> None
let isThrow x = isSome (destThrow x)

// rethrow - parsed as library call - internally represented as op form.
let mkReraiseLibCall g ty m = let ve,vt = typedExprForIntrinsic g m g.reraise_info in Expr.App(ve,vt,[ty],[mkUnit g m],m)
let mkReraise m returnTy = Expr.Op (TOp.Reraise,[returnTy],[],m) (* could suppress unitArg *)

//----------------------------------------------------------------------------
// CompilationMappingAttribute, SourceConstructFlags
//----------------------------------------------------------------------------

let tnameCompilationSourceNameAttr     = FSharpLib.Core + ".CompilationSourceNameAttribute"
let tnameCompilationArgumentCountsAttr = FSharpLib.Core + ".CompilationArgumentCountsAttribute"
let tnameCompilationMappingAttr        = FSharpLib.Core + ".CompilationMappingAttribute"
let tnameSourceConstructFlags          = FSharpLib.Core + ".SourceConstructFlags"

let tref_CompilationArgumentCountsAttr g = mkILTyRef (g.fslibCcu.ILScopeRef, tnameCompilationArgumentCountsAttr)
let tref_CompilationMappingAttr g        = mkILTyRef (g.fslibCcu.ILScopeRef, tnameCompilationMappingAttr)
let tref_CompilationSourceNameAttr g     = mkILTyRef (g.fslibCcu.ILScopeRef, tnameCompilationSourceNameAttr)
let tref_SourceConstructFlags g          = mkILTyRef (g.fslibCcu.ILScopeRef, tnameSourceConstructFlags)

let mkCompilationMappingAttrPrim g k nums = 
    mkILCustomAttribute g.ilg (tref_CompilationMappingAttr g, 
                               ((mkILNonGenericValueTy (tref_SourceConstructFlags g)) :: (nums |> List.map (fun _ -> g.ilg.typ_Int32))),
                               ((k :: nums) |> List.map (fun n -> ILAttribElem.Int32(n))),
                               [])
let mkCompilationMappingAttr g kind = mkCompilationMappingAttrPrim g kind []
let mkCompilationMappingAttrWithSeqNum g kind seqNum = mkCompilationMappingAttrPrim g kind [seqNum]
let mkCompilationMappingAttrWithVariantNumAndSeqNum g kind varNum seqNum = mkCompilationMappingAttrPrim g kind [varNum;seqNum]

let mkCompilationArgumentCountsAttr g nums = 
    mkILCustomAttribute g.ilg (tref_CompilationArgumentCountsAttr g, [ mkILArr1DTy g.ilg.typ_Int32 ],
                               [ILAttribElem.Array (g.ilg.typ_Int32, List.map (fun n -> ILAttribElem.Int32(n)) nums)],
                               [])

let mkCompilationSourceNameAttr g n = 
    mkILCustomAttribute g.ilg (tref_CompilationSourceNameAttr g, [  g.ilg.typ_String ],
                               [ILAttribElem.String(Some n)],
                               [])

//----------------------------------------------------------------------------
// FSharpInterfaceDataVersionAttribute
//----------------------------------------------------------------------------

let tname_SignatureDataVersionAttr = FSharpLib.Core + ".FSharpInterfaceDataVersionAttribute"
let tref_SignatureDataVersionAttr () = mkILTyRef(IlxSettings.ilxFsharpCoreLibScopeRef (), tname_SignatureDataVersionAttr)

let mkSignatureDataVersionAttr g ((v1,v2,v3,_) : ILVersionInfo)  = 
    mkILCustomAttribute g.ilg
        (tref_SignatureDataVersionAttr(), 
         [g.ilg.typ_Int32;g.ilg.typ_Int32;g.ilg.typ_Int32],
         [ILAttribElem.Int32 (int32 v1);
          ILAttribElem.Int32 (int32 v2) ; 
          ILAttribElem.Int32 (int32 v3)],[])

let tname_AutoOpenAttr = FSharpLib.Core + ".AutoOpenAttribute"
let tref_AutoOpenAttr () = mkILTyRef(IlxSettings.ilxFsharpCoreLibScopeRef (), tname_AutoOpenAttr)

let IsSignatureDataVersionAttr cattr = isILAttrib (tref_SignatureDataVersionAttr ()) cattr
let TryFindAutoOpenAttr cattr = 
    if isILAttrib (tref_AutoOpenAttr ()) cattr then 
        (* ok to use ecmaILGlobals here since we're querying metadata, not making it *)
        match decodeILAttribData IL.ecmaILGlobals cattr None with 
        |  [ILAttribElem.String s],_ -> s
        |  [],_ -> None
        | _ -> 
            warning(Failure(FSComp.SR.tastUnexpectedDecodeOfAutoOpenAttribute())); 
            None
    else
        None
        
let tref_InternalsVisibleToAttr () = 
    mkILTyRef (ecmaMscorlibScopeRef,"System.Runtime.CompilerServices.InternalsVisibleToAttribute")    

let TryFindInternalsVisibleToAttr cattr = 
    if isILAttrib (tref_InternalsVisibleToAttr ()) cattr then 
        (* ok to use ecmaILGlobals here since we're querying metadata, not making it *)
        match decodeILAttribData IL.ecmaILGlobals cattr None with 
        |  [ILAttribElem.String s],_ -> s
        |  [],_ -> None
        | _ -> 
            warning(Failure(FSComp.SR.tastUnexpectedDecodeOfInternalsVisibleToAttribute())); 
            None
    else
        None

let IsMatchingSignatureDataVersionAttr  ((v1,v2,v3,_) : ILVersionInfo)  cattr = 
    IsSignatureDataVersionAttr cattr &&
    // ok to use ecmaILGlobals here since we're querying metadata, not making it 
    match decodeILAttribData IL.ecmaILGlobals cattr None with 
    |  [ILAttribElem.Int32 u1; ILAttribElem.Int32 u2;ILAttribElem.Int32 u3 ],_ -> 
        (v1 = uint16 u1) && (v2 = uint16 u2) && (v3 = uint16 u3)
    | _ -> 
        warning(Failure(FSComp.SR.tastUnexpectedDecodeOfInterfaceDataVersionAttribute())); 
        false

let mkCompilerGeneratedAttr g n = 
    mkILCustomAttribute g.ilg (tref_CompilationMappingAttr g, [mkILNonGenericValueTy (tref_SourceConstructFlags g)],[ILAttribElem.Int32(n)],[])

//--------------------------------------------------------------------------
// tupled lambda --> method/function with a given topValInfo specification.
//
// AdjustArityOfLambdaBody: "(vs,body)" represents a lambda "fun (vs) ->  body".  The
// aim is to produce a "static method" represented by a pair
// "(mvs, body)" where mvs has the List.length "arity".
//--------------------------------------------------------------------------


let untupledToTupled vs =
    let untupledTys = typesOfVals vs
    let m = (List.head vs).Range
    let tupledv,tuplede = mkCompGenLocal m "tupledArg" (mkTupleTy untupledTys)
    let untupling_es =  List.mapi (fun i _ ->  mkTupleFieldGet(tuplede,untupledTys,i,m)) untupledTys
    tupledv, mkInvisibleLets m vs untupling_es 
    
// The required tupled-arity (arity) can either be 1 
// or N, and likewise for the tuple-arity of the input lambda, i.e. either 1 or N 
// where the N's will be identical. 
let AdjustArityOfLambdaBody g arity (vs:Val list) body = 
    let nvs = vs.Length
    if not (nvs = arity || nvs = 1 || arity = 1) then failwith ("lengths don't add up");
    if arity = 0 then 
        vs,body
    elif nvs = arity then 
        vs,body
    elif nvs = 1 then
        let v = vs.Head
        let untupledTys = destTupleTy g v.Type
        if  (untupledTys.Length <> arity) then failwith "length untupledTys <> arity";
        let dummyvs,dummyes = 
            untupledTys 
            |> List.mapi (fun i ty -> mkCompGenLocal v.Range (v.LogicalName ^"_"^string i) ty) 
            |> List.unzip 
        let body = mkInvisibleLet v.Range v (mkTupled g v.Range dummyes untupledTys) body
        dummyvs,body
    else 
        let tupledv, untupler =  untupledToTupled vs
        [tupledv],untupler body

let MultiLambdaToTupledLambda vs body = 
    match vs with 
    | [] -> failwith "MultiLambdaToTupledLambda: expected some argments"
    | [v] -> v,body 
    | vs -> 
        let tupledv, untupler =  untupledToTupled vs
        tupledv, untupler body 
      

//--------------------------------------------------------------------------
// Beta reduction via let-bindings. Reduce immediate apps. of lambdas to let bindings. 
// Includes binding the immediate application of generic
// functions. Input type is the type of the function.  Makes use of the invariant
// that any two expressions have distinct local variables (because we explicitly copy
// expressions).
//------------------------------------------------------------------------ 

let rec MakeApplicationAndBetaReduceAux g (f, fty, tyargsl : TType list list, argsl: Expr list, m) =
  (* let verbose = true in *)
  match f with 
  | Expr.Let(bind,body,mlet,_) ->
      // Lift bindings out, i.e. (let x = e in f) y --> let x = e in f y 
      // This increases the scope of 'x', which I don't like as it mucks with debugging 
      // scopes of variables, but this is an important optimization, especially when the '|>' 
      // notation is used a lot. 
      mkLetBind mlet bind (MakeApplicationAndBetaReduceAux g (body,fty,tyargsl,argsl,m))
  | _ -> 
  match tyargsl with 
  | [] :: rest -> 
     MakeApplicationAndBetaReduceAux g (f,fty,rest,argsl,m)

  | tyargs :: rest -> 
      // Bind type parameters by immediate substitution 
      match f with 
      | Expr.TyLambda(_, tyvs,body,_,bodyty) when tyvs.Length = List.length tyargs -> 
          let tpenv = bindTypars tyvs tyargs emptyTyparInst
          let body = remarkExpr m (instExpr g tpenv body)
          let bodyty' = instType tpenv bodyty
          MakeApplicationAndBetaReduceAux g (body,bodyty', rest,argsl,m) 

      | _ -> 
          let f = mkAppsAux g f fty [tyargs] [] m
          let fty = applyTyArgs g fty tyargs 
          MakeApplicationAndBetaReduceAux g (f,fty, rest,argsl,m)
  | [] -> 
      match argsl with
      | _ :: _ ->
          // Bind term parameters by "let" explicit substitutions 
          // 
          // Only do this if there are enough lambdas for the number of arguments supplied. This is because
          // all arguments get evaluated before application.
          //
          // VALID:
          //      (fun a b -> E[a,b]) t1 t2 ---> let a = t1 in let b = t2 in E[t1,t2]
          // INVALID:
          //      (fun a -> E[a]) t1 t2     ---> let a = t1 in E[a] t2       UNLESS: E[a] has no effects OR t2 has no effects
          
          match tryStripLambdaN argsl.Length f with 
          | Some (argvsl, body) -> 
               assert (argvsl.Length = argsl.Length)
               let argvs,body = List.mapfoldBack MultiLambdaToTupledLambda  argvsl body
               mkLetsBind m (mkCompGenBinds argvs argsl) body
          | _ -> 
              mkExprApplAux g f fty argsl m 

      | [] -> 
          f
      
let MakeApplicationAndBetaReduce g (f,fty,tyargsl,argl,m) = 
  MakeApplicationAndBetaReduceAux g (f,fty,tyargsl,argl,m)

//---------------------------------------------------------------------------
// Adjust for expected usage
// Convert a use of a value to saturate to the given arity.
//--------------------------------------------------------------------------- 

let MakeArgsForTopArgs _g m argtysl tpenv =
    argtysl |> List.mapi (fun i argtys -> 
        argtys |> List.mapi (fun j (argty,argInfo : ArgReprInfo) -> 
            let ty = instType tpenv argty
            let nm = 
               match argInfo.Name with 
               | None -> CompilerGeneratedName ("arg"^ string i^ string j)
               | Some id -> id.idText
            fst (mkCompGenLocal m nm ty)))

let AdjustValForExpectedArity g m (vref:ValRef) flags topValInfo =

    let tps,argtysl,rty,_ = GetTopValTypeInFSharpForm g topValInfo vref.Type m
    let tps' = copyTypars tps
    let tyargs' = List.map mkTyparTy tps'
    let tpenv = bindTypars tps tyargs' emptyTyparInst
    let rty' = instType tpenv rty
    let vsl = MakeArgsForTopArgs g m argtysl tpenv
    let call = MakeApplicationAndBetaReduce g (Expr.Val(vref,flags,m),vref.Type,[tyargs'],(List.map (mkTupledVars g m) vsl),m)
    let tauexpr,tauty = 
        List.foldBack 
            (fun vs (e,ty) -> mkMultiLambda m vs (e, ty), (mkTupledVarsTy g vs --> ty))
            vsl
            (call, rty')
    // Build a type-lambda expression for the toplevel value if needed... 
    mkTypeLambda m tps' (tauexpr,tauty),tps' +-> tauty


//---------------------------------------------------------------------------
// 


let IsSubsumptionExpr g expr =
    match expr with 
    | Expr.Op (TOp.Coerce,[inputTy;actualTy],[_],_) ->
        isFunTy g actualTy && isFunTy g inputTy   
    | _ -> 
        false

let stripTupledFunTy g ty = 
    let argTys,retTy = stripFunTy g ty
    let curriedArgTys = argTys |> List.map (tryDestTupleTy g)
    curriedArgTys, retTy

let (|ExprValWithPossibleTypeInst|_|) expr =
    match expr with 
    | Expr.App(Expr.Val(vref,flags,m),_fty,tyargs,[],_)  ->
        Some(vref,flags,tyargs,m)
    | Expr.Val(vref,flags,m) ->
        Some(vref,flags,[],m)
    | _ -> 
        None

let mkCoerceIfNeeded g tgtTy srcTy expr =
    if typeEquiv g tgtTy srcTy then 
        expr
    else 
        mkCoerceExpr(expr,tgtTy,expr.Range,srcTy)

let mkCompGenLetIn m nm ty e f = 
    let v,ve = mkCompGenLocal m nm ty
    mkCompGenLet m v e (f (v,ve))

/// Take a node representing a coercion from one function type to another, e.g.
///    A -> A * A -> int 
/// to 
///    B -> B * A -> int 
/// and return an expression of the correct type that doesn't use a coercion type. For example
/// return   
///    (fun b1 b2 -> E (b1 :> A) (b2 :> A))
///
///    - Use good names for the closure arguments if available
///    - Create lambda variables if needed, or use the supplied arguments if available.
///
/// Return the new expression and any unused suffix of supplied arguments
///
/// If E is a value with TopInfo then use the arity to help create a better closure.
/// In particular we can create a closure like this:
///    (fun b1 b2 -> E (b1 :> A) (b2 :> A))
/// rather than 
///    (fun b1 -> let clo = E (b1 :> A) in (fun b2 -> clo (b2 :> A)))
/// The latter closures are needed to carefully preserve side effect order
///
/// Note that the results of this translation are visible to quotations

let AdjustPossibleSubsumptionExpr g (expr: Expr) (suppliedArgs: Expr list) : (Expr* Expr list) option =

    match expr with 
    | Expr.Op (TOp.Coerce,[inputTy;actualTy],[exprWithActualTy],m) when 
        isFunTy g actualTy && isFunTy g inputTy  ->
        
        if typeEquiv g actualTy inputTy then 
            Some(exprWithActualTy, suppliedArgs)
        else
            
            let curriedActualArgTys,retTy = stripTupledFunTy g actualTy

            let curriedInputTys,_ = stripFunTy g inputTy

            assert (curriedActualArgTys.Length = curriedInputTys.Length)

            let argTys = (curriedInputTys,curriedActualArgTys) ||> List.mapi2 (fun i x y -> (i,x,y))


            // Use the nice names for a function of known arity and name. Note that 'nice' here also 
            // carries a semantic meaning. For a function with top-info,
            //   let f (x:A) (y:A) (z:A) = ...
            // we know there are no side effects on the application of 'f' to 1,2 args. This greatly simplifies
            // the closure built for 
            //   f b1 b2 
            // and indeed for 
            //   f b1 b2 b3
            // we don't build any closure at all, and just return
            //   f (b1 :> A) (b2 :> A) (b3 :> A)
            
            let curriedNiceNames = 
                match stripExpr exprWithActualTy with 
                | ExprValWithPossibleTypeInst(vref,_,_,_) when vref.ValReprInfo.IsSome -> 

                    let _,argtysl,_,_ = GetTopValTypeInFSharpForm g vref.ValReprInfo.Value vref.Type expr.Range
                    argtysl |> List.mapi (fun i argtys -> 
                        argtys |> List.mapi (fun j (_,argInfo) -> 
                             match argInfo.Name with 
                             | None -> CompilerGeneratedName ("arg" ^ string i ^string j)
                             | Some id -> id.idText))
                | _ -> 
                    []

            assert (curriedActualArgTys.Length >= curriedNiceNames.Length)

            let argTysWithNiceNames,argTysWithoutNiceNames =
                List.chop curriedNiceNames.Length argTys

            /// Only consume 'suppliedArgs' up to at most the number of nice arguments
            let suppliedArgs, droppedSuppliedArgs = 
                List.chop (min suppliedArgs.Length curriedNiceNames.Length) suppliedArgs

            /// THe relevant range for any expressions and applications includes the arguments 
            let appm = (m,suppliedArgs) ||> List.fold (fun m e -> unionRanges m (e.Range)) 

            // See if we have 'enough' suppliedArgs. If not, we have to build some lambdas, and,
            // we have to 'let' bind all arguments that we consume, e.g.
            //   Seq.take (effect;4) : int list -> int list
            // is a classic case. Here we generate
            //   let tmp = (effect;4) in 
            //   (fun v -> Seq.take tmp (v :> seq<_>))
            let buildingLambdas = (suppliedArgs.Length <> curriedNiceNames.Length)
            //printfn "buildingLambdas = %A" buildingLambdas
            //printfn "suppliedArgs.Length = %d" suppliedArgs.Length 

            /// Given a tuple of argument variables that has a tuple type that satisfies the input argument types,
            /// coerce it to a tuple that satisfies the matching coerced argument type(s).
            let CoerceDetupled (argTys: TType list) (detupledArgs: Expr list) (actualTys: TType list) =
                assert (actualTys.Length = argTys.Length)
                assert (actualTys.Length = detupledArgs.Length)
                // Inject the coercions into the user-supplied explicit tuple
                let argm = List.reduce unionRanges (detupledArgs |> List.map (fun e -> e.Range))
                mkTupled g argm (List.map3 (mkCoerceIfNeeded g) actualTys argTys detupledArgs) actualTys

            /// Given an argument variable of tuple type that has been evaluated and stored in the 
            /// given variable, where the tuple type that satisfies the input argument types,
            /// coerce it to a tuple that satisfies the matching coerced argument type(s).
            let CoerceBoundTuple tupleVar argTys (actualTys : TType list) =
                assert (actualTys.Length > 1)
            
                mkTupled g appm 
                   ((actualTys,argTys) ||> List.mapi2 (fun i actualTy dummyTy ->  
                       let argExprElement = mkTupleFieldGet(tupleVar,argTys,i,appm)
                       mkCoerceIfNeeded  g actualTy dummyTy argExprElement))
                   actualTys

            /// Given an argument that has a tuple type that satisfies the input argument types,
            /// coerce it to a tuple that satisfies the matching coerced argument type. Try to detuple the argument if possible.
            let CoerceTupled niceNames (argExpr: Expr) (actualTys: TType list) =
                let argExprTy = (tyOfExpr g argExpr)

                let argTys  = 
                    match actualTys with 
                    | [_] -> 
                        [tyOfExpr g argExpr]
                    | _ -> 
                        tryDestTupleTy g argExprTy 
                
                assert (actualTys.Length = argTys.Length)
                let nm = match niceNames with [nm] -> nm | _ -> "arg"
                if buildingLambdas then 
                    // Evaluate the user-supplied tuple-valued argument expression, inject the coercions and build an explicit tuple
                    // Assign the argument to make sure it is only run once
                    //     f ~~> : B -> int
                    //     f ~~> : (B * B) -> int
                    //
                    //  for 
                    //     let f a = 1
                    //     let f (a,a) = 1
                    let v,ve = mkCompGenLocal appm nm argExprTy
                    let binderBuilder = (fun tm -> mkCompGenLet appm v argExpr tm)
                    let expr = 
                        match actualTys,argTys with
                        | [actualTy],[argTy] -> mkCoerceIfNeeded  g actualTy argTy ve 
                        | _ -> CoerceBoundTuple ve argTys actualTys

                    binderBuilder,expr
                else                
                    if typeEquiv g (mkTupledTy g actualTys) argExprTy then 
                        (fun tm -> tm), argExpr
                    else
                    
                        let detupledArgs,argTys  = 
                            match actualTys with 
                            | [_actualType] -> 
                                [argExpr],[tyOfExpr g argExpr]
                            | _ -> 
                                tryDestTuple argExpr,tryDestTupleTy g argExprTy 

                        // OK, the tuples match, or there is no de-tupling,
                        //     f x
                        //     f (x,y)
                        //
                        //  for 
                        //     let f (x,y) = 1
                        // and we're not building lambdas, just coerce the arguments in place
                        if detupledArgs.Length =  actualTys.Length then 
                            (fun tm -> tm), CoerceDetupled argTys detupledArgs actualTys
                        else 
                            // In this case there is a tuple mismatch.
                            //     f p
                            //
                            //
                            //  for 
                            //     let f (x,y) = 1
                            // Assign the argument to make sure it is only run once
                            let v,ve = mkCompGenLocal appm nm argExprTy
                            let binderBuilder = (fun tm -> mkCompGenLet appm v argExpr tm)
                            let expr = CoerceBoundTuple ve argTys actualTys
                            binderBuilder,expr
                        

            // This variable is really a dummy to make the code below more regular. 
            // In the i = N - 1 cases we skip the introduction of the 'let' for
            // this variable.
            let resVar,resVarAsExpr = mkCompGenLocal appm "result" retTy
            let N = argTys.Length
            let (cloVar,exprForOtherArgs,_) = 
                List.foldBack 
                    (fun (i,inpArgTy,actualArgTys) (cloVar:Val,res,resTy) -> 

                        let inpArgTys = 
                            match actualArgTys with 
                            | [_] -> [inpArgTy]
                            | _ -> destTupleTy g inpArgTy

                        assert (inpArgTys.Length = actualArgTys.Length)
                        
                        let inpsAsVars,inpsAsExprs = inpArgTys |> List.mapi (fun j ty -> mkCompGenLocal appm ("arg"^string i^string j) ty)  |> List.unzip
                        let inpsAsActualArg = CoerceDetupled inpArgTys inpsAsExprs actualArgTys
                        let inpCloVarType = (mkFunTy (mkTupledTy g actualArgTys) cloVar.Type)
                        let newResTy = mkFunTy inpArgTy resTy
                        let inpCloVar,inpCloVarAsExpr = mkCompGenLocal appm ("clo"^string i) inpCloVarType
                        let newRes = 
                            // For the final arg we can skip introducing the dummy variable
                            if i = N - 1 then 
                                mkMultiLambda appm inpsAsVars 
                                    (mkApps g ((inpCloVarAsExpr,inpCloVarType),[],[inpsAsActualArg],appm),resTy)
                            else
                                mkMultiLambda appm inpsAsVars 
                                    (mkInvisibleLet appm cloVar 
                                       (mkApps g ((inpCloVarAsExpr,inpCloVarType),[],[inpsAsActualArg],appm)) 
                                       res, 
                                     resTy)
                            
                        inpCloVar,newRes,newResTy)
                    argTysWithoutNiceNames
                    (resVar,resVarAsExpr,retTy)

            
            // Mark the up as Some/None
            let suppliedArgs = List.map Some suppliedArgs @ List.ofArray (Array.create (curriedNiceNames.Length - suppliedArgs.Length) None)

            assert (suppliedArgs.Length = curriedNiceNames.Length)

            let exprForAllArgs = 

                if isNil argTysWithNiceNames then 
                    mkInvisibleLet appm cloVar exprWithActualTy exprForOtherArgs
                else
                    let lambdaBuilders,binderBuilders,inpsAsArgs = 
                    
                        (argTysWithNiceNames,curriedNiceNames,suppliedArgs) |||> List.map3 (fun (_,inpArgTy,actualArgTys) niceNames suppliedArg -> 

                                let inpArgTys = 
                                    match actualArgTys with 
                                    | [_] -> [inpArgTy]
                                    | _ -> destTupleTy g inpArgTy


                                /// Note: there might not be enough nice names, and they might not match in arity
                                let niceNames = 
                                    match niceNames with 
                                    | nms when nms.Length = inpArgTys.Length -> nms
                                    | [nm] -> inpArgTys |> List.mapi (fun i _ -> (nm^string i))
                                    | nms -> nms
                                match suppliedArg with 
                                | Some arg -> 
                                    let binderBuilder,inpsAsActualArg = CoerceTupled niceNames arg actualArgTys
                                    let lambdaBuilder = (fun tm -> tm)
                                    lambdaBuilder, binderBuilder,inpsAsActualArg
                                | None -> 
                                    let inpsAsVars,inpsAsExprs = (niceNames,inpArgTys)  ||> List.map2 (fun nm ty -> mkCompGenLocal appm nm ty)  |> List.unzip
                                    let inpsAsActualArg = CoerceDetupled inpArgTys inpsAsExprs actualArgTys
                                    let lambdaBuilder = (fun tm -> mkMultiLambda appm inpsAsVars (tm, tyOfExpr g tm))
                                    let binderBuilder = (fun tm -> tm)
                                    lambdaBuilder,binderBuilder,inpsAsActualArg)
                        |> List.unzip3
                    
                    // If no trailing args then we can skip introducing the dummy variable
                    // This corresponds to 
                    //    let f (x:A) = 1      
                    //
                    //   f ~~> type B -> int
                    //
                    // giving
                    //   (fun b -> f (b :> A))
                    // rather than 
                    //   (fun b -> let clo = f (b :> A) in clo)   
                    let exprApp = 
                        if argTysWithoutNiceNames.Length = 0 then 
                            mkApps g ((exprWithActualTy,actualTy),[],inpsAsArgs,appm)
                        else
                            mkInvisibleLet appm 
                                    cloVar (mkApps g ((exprWithActualTy,actualTy),[],inpsAsArgs,appm)) 
                                    exprForOtherArgs

                    List.foldBack (fun f acc -> f acc) binderBuilders 
                        (List.foldBack (fun f acc -> f acc) lambdaBuilders exprApp)

            Some(exprForAllArgs,droppedSuppliedArgs)
    | _ -> 
        None
  
/// Find and make all subsumption eliminations 
let NormalizeAndAdjustPossibleSubsumptionExprs g inputExpr = 
    let expr,args = 
        // AdjustPossibleSubsumptionExpr can take into account an application
        match stripExpr inputExpr with 
        | Expr.App(f,_fty,[],args,_)  ->
             f,args

        | _ -> 
            inputExpr,[]
    
    match AdjustPossibleSubsumptionExpr g expr args with 
    | None -> 
        inputExpr
    | Some (expr',[]) -> 
        expr'
    | Some (expr',args') -> 
        //printfn "adjusted...." 
        Expr.App(expr',tyOfExpr g expr',[],args',inputExpr.Range)  
             
  
//---------------------------------------------------------------------------
// LinearizeTopMatch - when only one non-failing target, make linear.  The full
// complexity of this is only used for spectacularly rare bindings such as 
//    type ('a,'b) either = This of 'a | That of 'b
//    let this_f1 = This (fun x -> x)
//    let This fA | That fA = this_f1
// 
// Here a polymorphic top level binding "fA" is _computed_ by a pattern match!!!
// The TAST coming out of type checking must, however, define fA as a type function,
// since it is marked with an arity that indicates it's r.h.s. is a type function]
// without side effects and so can be compiled as a generic method (for example).

// polymorphic things bound in complex matches at top level require eta expansion of the 
// type function to ensure the r.h.s. of the binding is indeed a type function 
let etaExpandTypeLambda g m tps (tm,ty) = 
  if isNil tps then tm else mkTypeLambda m tps (mkApps g ((tm,ty),[(List.map mkTyparTy tps)],[],m),ty)

let AdjustValToTopVal (tmp:Val) parent valData =
        tmp.SetValReprInfo (Some valData);  
        tmp.Data.val_actual_parent <- parent;  
        tmp.SetIsMemberOrModuleBinding()

/// For match with only one non-failing target T0, the other targets, T1... failing (say, raise exception).
///   tree, T0(v0,..,vN) => rhs ; T1() => fail ; ...
/// Convert it to bind T0's variables, then continue with T0's rhs:
///   let tmp = switch tree, TO(fv0,...,fvN) => Tup (fv0,...,fvN) ; T1() => fail; ...
///   let v1  = #1 tmp in ...
///   and vN  = #N tmp
///   rhs
/// Motivation:
/// - For top-level let bindings with possibly failing matches,
///   this makes clear that subsequent bindings (if reached) are top-level ones.
let LinearizeTopMatchAux g parent  (spBind,m,tree,targets,m2,ty) =
    let targetsL = Array.toList targets
    (* items* package up 0,1,more items *)
    let itemsProj tys i x = 
        match tys with 
        | []  -> failwith "itemsProj: no items?"
        | [_] -> x (* no projection needed *)
        | tys -> Expr.Op (TOp.TupleFieldGet(i),tys,[x],m)
    let isThrowingTarget = function TTarget(_,x,_) -> isThrow x
    if 1 + List.count isThrowingTarget targetsL = targetsL.Length then
        (* Have failing targets and ONE successful one, so linearize *)
        let (TTarget (vs,rhs,spTarget)) = the (List.tryFind (isThrowingTarget >> not) targetsL)
        (* note - old code here used copy value to generate locals - this was not right *)
        let fvs      = vs |> FlatList.map (fun v -> fst(mkLocal v.Range v.LogicalName v.Type)) (* fresh *)
        let vtys     = vs |> FlatList.map (fun v -> v.Type) 
        let tmpTy    = mkTupledVarsTy g (FlatList.toList vs)
        let tmp,tmpe = mkCompGenLocal m "matchResultHolder" tmpTy

        AdjustValToTopVal tmp parent ValReprInfo.emptyValData;  

        let newTg    = TTarget (fvs,mkTupledVars g m (FlatList.toList fvs),spTarget)
        let fixup (TTarget (tvs,tx,spTarget)) = 
           match destThrow tx with
           | Some (m,_,e) -> let tx = mkThrow m tmpTy e
                             TTarget(tvs,tx,spTarget) (* Throwing targets, recast it's "return type" *)
           | None          -> newTg       (* Non-throwing target,  replaced [new/old] *)
       
        let targets  = Array.map fixup targets
        let binds    = 
            vs |> FlatList.mapi (fun i v -> 
                let ty = v.Type
                let rhs =  etaExpandTypeLambda g m  v.Typars (itemsProj (FlatList.toList vtys) i tmpe, ty)
                (* update the arity of the value *)
                v.SetValReprInfo (Some (InferArityOfExpr g ty [] [] rhs))
                mkInvisibleBind v rhs)  in (* vi = proj tmp *)
        mkCompGenLet m
          tmp (primMkMatch (spBind,m,tree,targets,m2,tmpTy)) (* note, probably retyped match, but note, result still has same type *)
          (mkLetsFromBindings m binds rhs)                             
    else
        (* no change *)
        primMkMatch (spBind,m,tree,targets,m2,ty)

let LinearizeTopMatch g parent = function
  | Expr.Match (spBind,m,tree,targets,m2,ty) -> LinearizeTopMatchAux g parent (spBind,m,tree,targets,m2,ty)
  | x -> x


//---------------------------------------------------------------------------
// XmlDoc signatures
//---------------------------------------------------------------------------


let commaEncs strs  = String.concat "," strs
let angleEnc  str   = "{" ^ str ^ "}" 
let ticksAndArgCountTextOfTyconRef (tcref:TyconRef) =
     // Generic type names are (name ^ "`" ^ digits) where name does not contain "`".
     let path = Array.toList (fullMangledPathToTyconRef tcref) @ [tcref.CompiledName]
     textOfPath path
     
let xmlDocTextOfTyconRef (tcref:TyconRef) =
     // Generic type names are (name ^ "`" ^ digits) where name does not contain "`".
     // In XML doc, when used in type instances, these do not use the ticks.
     let path = Array.toList (fullMangledPathToTyconRef tcref) @ [tcref.CompiledName]
     textOfPath (List.map DemangleGenericTypeName path)


let typarEnc _g (gtpsType,gtpsMethod) typar =
    match List.tryFindIndex (typarEq typar) gtpsType with
    | Some idx -> "`"  ^ string idx // single-tick-index for typar from type
    | None     ->
        match List.tryFindIndex (typarEq typar) gtpsMethod with
        | Some idx -> "``" ^ string idx // double-tick-index for typar from method
        | None     -> warning(InternalError("Typar not found during XmlDoc generation",typar.Range))
                      "``0" 

let rec typeEnc g (gtpsType,gtpsMethod) ty = 
    if verbose then  dprintf "--> typeEnc";
    match (stripTyEqns g ty) with 
    | TType_forall _ -> 
        "Microsoft.FSharp.Core.FSharpTypeFunc"
    | _ when isArrayTy g ty   -> 
        let tcref,tinst = destAppTy g ty
        let arraySuffix = 
            match rankOfArrayTyconRef g tcref with
            // The easy case
            | 1 -> "[]"
            // In fact IL supports 3 kinds of multidimensional arrays, and each kind of array has its own xmldoc spec.
            // We don't support all these, and instead always pull xmldocs for 0-based-arbitrary-length ("0:") multidimensional arrays.
            // This is probably the 99% case anyway.
            | 2 -> "[0:,0:]"
            | 3 -> "[0:,0:,0:]"
            | 4 -> "[0:,0:,0:,0:]"
            | _ -> failwith "impossible: rankOfArrayTyconRef: unsupported array rank"
        typeEnc g (gtpsType,gtpsMethod) (List.head tinst) ^ arraySuffix
    | TType_ucase (UCRef(tcref,_),tinst)   
    | TType_app (tcref,tinst)   -> 
        if tyconRefEq g g.byref_tcr tcref then
           typeEnc g (gtpsType,gtpsMethod) (List.head tinst) ^ "@"
        elif tyconRefEq g tcref g.nativeptr_tcr then
           typeEnc g (gtpsType,gtpsMethod) (List.head tinst) ^ "*"
        else
           xmlDocTextOfTyconRef tcref + tyargsEnc g (gtpsType,gtpsMethod) tinst
    | TType_tuple typs          -> 
        sprintf "System.Tuple`%d%s" typs.Length (tyargsEnc g (gtpsType,gtpsMethod) typs)
    | TType_fun (f,x)           -> 
        "Microsoft.FSharp.Core.FSharpFunc`2" + tyargsEnc g (gtpsType,gtpsMethod) [f;x]
    | TType_var typar           -> 
        typarEnc g (gtpsType,gtpsMethod) typar
    | TType_measure _ -> "?"

and tyargsEnc g (gtpsType,gtpsMethod) args = 
     if isNil args then ""
     else angleEnc (commaEncs (List.map (typeEnc g (gtpsType,gtpsMethod)) args)) 

let XmlDocArgsEnc g (gtpsType,gtpsMethod) argTs =
  if isNil argTs then "" 
  else "(" + String.concat "," (List.map (typeEnc g (gtpsType,gtpsMethod)) argTs) + ")"

let buildAccessPath (cp : CompilationPath option) =
    match cp with
    | Some(cp) ->
        let ap = cp.AccessPath |> List.map fst |> List.toArray
        System.String.Join(".",ap)      
    | None -> "Extension Type"
let prependPath path name = if path = "" then name else path + "." + name

let XmlDocSigOfVal g path (v:Val) =
  let parentTypars,methTypars,argInfos,prefix,path,name = 

    // CLEANUP: this is one of several code paths that treat module values and members 
    // seperately when really it would be cleaner to make sure GetTopValTypeInFSharpForm, GetMemberTypeInFSharpForm etc.
    // were lined up so code paths like this could be uniform
    
    match v.MemberInfo with 
    | Some membInfo when not v.IsExtensionMember -> 
        (* Methods, Properties etc. *)
        let tps,argInfos,_,_ = GetMemberTypeInMemberForm g membInfo.MemberFlags (the v.ValReprInfo) v.Type v.Range
        let prefix,name = 
          match membInfo.MemberFlags.MemberKind with 
          | MemberKind.ClassConstructor 
          | MemberKind.Constructor -> "M:", "#ctor"
          | MemberKind.Member -> "M:", v.CompiledName
          | MemberKind.PropertyGetSet 
          | MemberKind.PropertySet
          | MemberKind.PropertyGet -> "P:",v.PropertyName
        let path = prependPath path v.TopValActualParent.CompiledName
        let parentTypars,methTypars = 
          match PartitionValTypars g v with
          | Some(_,memberParentTypars,memberMethodTypars,_,_) -> memberParentTypars,memberMethodTypars
          | None -> [],tps
        parentTypars,methTypars,argInfos,prefix,path,name
    | _ ->
        // Regular F# values and extension members 
        let w = arityOfVal v
        let tps,argInfos,_,_ = GetTopValTypeInCompiledForm g w v.Type v.Range
        let name = v.CompiledName
        let prefix =
          if  w.NumCurriedArgs = 0 && isNil tps then "P:"
          else "M:"
        [],tps,argInfos,prefix,path,name
  let argTs = argInfos |> List.concat |> List.map fst
  let args = XmlDocArgsEnc g (parentTypars,methTypars) argTs
  let arity = List.length methTypars in (* C# XML doc adds ``<arity> to *generic* member names *)
  let genArity = if arity=0 then "" else sprintf "``%d" arity
  prefix + prependPath path name + genArity + args
  
let XmlDocSigOfUnionCase path case typeName =
    // Would like to use "U:", but ParseMemberSignature only accepts C# signatures
    let prefix = "T:"
    let path = prependPath path typeName
    prefix + prependPath path case
    
let XmlDocSigOfField path name compiledName =
    let prefix = "F:"
    let path = prependPath path compiledName
    prefix + prependPath path name

let XmlDocSigOfTycon path (tc:Tycon) =  "T:" + prependPath path tc.CompiledName
let XmlDocSigOfSubModul path = "T:" + path 

let XmlDocSigOfEntity (eref:EntityRef) =
    XmlDocSigOfTycon (buildAccessPath eref.CompilationPathOpt) eref.Deref


//--------------------------------------------------------------------------
// Some unions have null as representations 
//--------------------------------------------------------------------------


let enum_CompilationRepresentationAttribute_Static             = 0b0000000000000001
let enum_CompilationRepresentationAttribute_Instance           = 0b0000000000000010
let enum_CompilationRepresentationAttribute_StaticInstanceMask = 0b0000000000000011
let enum_CompilationRepresentationAttribute_ModuleSuffix       = 0b0000000000000100
let enum_CompilationRepresentationAttribute_PermitNull         = 0b0000000000001000

let HasUseNullAsTrueValueAttribute g attribs =
     match TryFindInt32Attrib  g g.attrib_CompilationRepresentationAttribute attribs with
     | Some(flags) -> ((flags &&& enum_CompilationRepresentationAttribute_PermitNull) <> 0)
     | _ -> false 

let TyconHasUseNullAsTrueValueAttribute g (tycon:Tycon) = HasUseNullAsTrueValueAttribute g tycon.Attribs 

// WARNING: this must match optimizeAlternativeToNull in ilx/cu_erase.fs
let CanHaveUseNullAsTrueValueAttribute (_g:TcGlobals) (tycon:Tycon) =
  (tycon.IsUnionTycon && 
   let ucs = tycon.UnionCasesArray
   (ucs.Length = 0 ||
     (ucs |> Array.existsOne (fun uc -> uc.IsNullary) &&
      ucs |> Array.exists (fun uc -> not uc.IsNullary))))

// WARNING: this must match optimizeAlternativeToNull in ilx/cu_erase.fs
let IsUnionTypeWithNullAsTrueValue (g:TcGlobals) (tycon:Tycon) =
  (tycon.IsUnionTycon && 
   let ucs = tycon.UnionCasesArray
   (ucs.Length = 0 ||
     (TyconHasUseNullAsTrueValueAttribute g tycon &&
      ucs |> Array.existsOne (fun uc -> uc.IsNullary) &&
      ucs |> Array.exists (fun uc -> not uc.IsNullary))))

let TyconCompilesInstanceMembersAsStatic g tycon = IsUnionTypeWithNullAsTrueValue g tycon
let TcrefCompilesInstanceMembersAsStatic g (tcref: TyconRef) = TyconCompilesInstanceMembersAsStatic g tcref.Deref

let TypeNullNever g ty = 
    let underlyingTy = stripTyEqnsAndMeasureEqns g ty
    (isStructTy g underlyingTy) ||
    (isByrefTy g underlyingTy)

let TypeNullIsExtraValue g ty = 
    isILReferenceTy g ty ||
    isDelegateTy g ty ||
    (not (TypeNullNever g ty) && 
     isAppTy g ty && 
     TryFindBoolAttrib  g g.attrib_AllowNullLiteralAttribute (tyconOfAppTy g ty).Attribs = Some(true))

let TypeNullIsTrueValue g ty = 
    (isAppTy g ty && IsUnionTypeWithNullAsTrueValue g (tyconOfAppTy g ty))  ||
    (isUnitTy g ty)

let TypeNullNotLiked g ty = 
       not (TypeNullIsExtraValue g ty) 
    && not (TypeNullIsTrueValue g ty) 
    && not (TypeNullNever g ty) 

let TypeSatisfiesNullConstraint g ty = 
    TypeNullIsExtraValue g ty  

let rec TypeHasDefaultValue g ty = 
    TypeSatisfiesNullConstraint g ty  
    || (isStructTy g ty &&
        // Is it an F# struct type?
        (if isFSharpStructTy g ty then 
            let tcref,tinst = destAppTy g ty 
            let flds = 
                // Note this includes fields implied by the use of the implicit class construction syntax
                tcref.AllInstanceFieldsAsList
                  // We can ignore fields with the DefaultValue(false) attribute 
                  |> List.filter (fun fld -> not (TryFindBoolAttrib g g.attrib_DefaultValueAttribute fld.FieldAttribs = Some(false)))

            flds |> List.forall (actualTyOfRecdField (mkTyconRefInst tcref tinst) >> TypeHasDefaultValue g)
         elif isTupleStructTy g ty then 
            destTupleTy g ty |> List.forall (TypeHasDefaultValue g)
         else
            // All struct types defined in other .NET languages have a DefaultValue regardless of their
            // instantiation
            true))


let (|SpecialComparableHeadType|_|) g ty =           
    if isTupleTy g ty then 
        Some (destTupleTy g ty) 
    elif isAppTy g ty then 
        let tcref,tinst = destAppTy g ty 
        if isArrayTyconRef g tcref ||
           tyconRefEq g tcref g.system_UIntPtr_tcref ||
           tyconRefEq g tcref g.system_IntPtr_tcref then
           //tyconRefEq g tcref g.list_tcr_canon ||
           //tyconRefEq g tcref g.map_tcr_canon ||
           //tyconRefEq g tcref g.set_tcr_canon ||
           //tyconRefEq g tcref g.choice2_tcr ||
           //tyconRefEq g tcref g.choice3_tcr ||
           //tyconRefEq g tcref g.choice4_tcr ||
           //tyconRefEq g tcref g.choice5_tcr ||
           //tyconRefEq g tcref g.choice6_tcr ||
           //tyconRefEq g tcref g.choice7_tcr ||
           //tyconRefEq g tcref g.option_tcr_canon then
             Some tinst 
        else 
            None
    else
        None

let (|SpecialEquatableHeadType|_|) g ty = (|SpecialComparableHeadType|_|) g ty
let (|SpecialNotEquatableHeadType|_|) g ty = 
    if isFunTy g ty then Some() else None



// Can we use the fast helper for the 'LanguagePrimitives.IntrinsicFunctions.TypeTestGeneric'? 
let canUseTypeTestFast g ty = 
     not (isTyparTy g ty) && 
     not (TypeNullIsTrueValue g ty) && 
     not (TypeNullNever g ty)

// Can we use the fast helper for the 'LanguagePrimitives.IntrinsicFunctions.UnboxGeneric'? 
let canUseUnboxFast g ty = 
     not (isTyparTy g ty) && 
     not (TypeNullNotLiked g ty)
     
     
//--------------------------------------------------------------------------
// Nullness tests and pokes 
//--------------------------------------------------------------------------

(* match inp with :? ty as v -> e2[v] | _ -> e3 *)
let mkIsInstConditional g m tgty vinpe v e2 e3 = 
    // No sequence point for this compiler generated expression form
    
    if canUseTypeTestFast g tgty then 

        let mbuilder = new MatchBuilder(NoSequencePointAtInvisibleBinding,m)
        let tg2 = mbuilder.AddResultTarget(e2,SuppressSequencePointAtTarget)
        let tg3 = mbuilder.AddResultTarget(e3,SuppressSequencePointAtTarget)
        let dtree = TDSwitch(exprForVal m v,[TCase(Test.IsNull,tg3)],Some tg2,m)
        let expr = mbuilder.Close(dtree,m,tyOfExpr g e2)
        mkInvisibleLet m v (mkIsInst tgty vinpe m)  expr

    else
        let mbuilder = new MatchBuilder(NoSequencePointAtInvisibleBinding,m)
        let tg2 = TDSuccess(FlatList.one (mkCallUnbox g m tgty vinpe), mbuilder.AddTarget(TTarget(FlatList.one v,e2,SuppressSequencePointAtTarget)))
        let tg3 = mbuilder.AddResultTarget(e3,SuppressSequencePointAtTarget)
        let dtree = TDSwitch(vinpe,[TCase(Test.IsInst(tyOfExpr g vinpe,tgty),tg2)],Some tg3,m)
        let expr = mbuilder.Close(dtree,m,tyOfExpr g e2)
        expr



// Null tests are generated by
//    1. The compilation of array patterns in the pattern match compiler
//    2. The compilation of string patterns in the pattern match compiler

let mkNullTest g m e1 e2 e3 =
        let mbuilder = new MatchBuilder(NoSequencePointAtInvisibleBinding,m)
        let tg2 = mbuilder.AddResultTarget(e2,SuppressSequencePointAtTarget)
        let tg3 = mbuilder.AddResultTarget(e3,SuppressSequencePointAtTarget)            
        let dtree = TDSwitch(e1, [TCase(Test.IsNull,tg3)],Some tg2,m)
        let expr = mbuilder.Close(dtree,m,tyOfExpr g e2)
        expr         
let mkNonNullTest g m e = mkAsmExpr ([ IL.AI_ldnull ; IL.AI_cgt_un  ],[],  [e],[g.bool_ty],m)
let mkNonNullCond g m ty e1 e2 e3 = mkCond NoSequencePointAtStickyBinding SuppressSequencePointAtTarget m ty (mkNonNullTest g m e1) e2 e3
let mkIfThen g m e1 e2 = mkCond NoSequencePointAtStickyBinding SuppressSequencePointAtTarget m g.unit_ty e1 e2 (mkUnit g m)


let ModuleNameIsMangled g attrs =
    match TryFindInt32Attrib g g.attrib_CompilationRepresentationAttribute attrs with
    | Some(flags) -> ((flags &&& enum_CompilationRepresentationAttribute_ModuleSuffix) <> 0)
    | _ -> false 

let CompileAsEvent g attrs = HasAttrib g g.attrib_CLIEventAttribute attrs 


let MemberIsCompiledAsInstance g parent isExtensionMember membInfo attrs =
    // All extension members are compiled as static members
    if isExtensionMember then false
    // Anything implementing a dispatch slot is compiled as an instance member
    elif membInfo.MemberFlags.IsOverrideOrExplicitImpl then true
    elif nonNil membInfo.ImplementedSlotSigs then true
    else 
        // Otherwise check attributes to see if there is an explicit instance or explicit static flag
        let explicitInstance,explicitStatic = 
            match TryFindInt32Attrib g g.attrib_CompilationRepresentationAttribute attrs with
            | Some(flags) -> 
              ((flags &&& enum_CompilationRepresentationAttribute_Instance) <> 0),
              ((flags &&& enum_CompilationRepresentationAttribute_Static) <> 0)
            | _ -> false,false
        explicitInstance ||
        (membInfo.MemberFlags.IsInstance &&
         not explicitStatic &&
         not (TcrefCompilesInstanceMembersAsStatic g parent))


let isSealedTy g ty =
  let ty = stripTyEqnsAndMeasureEqns g ty
  not (isRefTy g ty) ||
  isUnitTy g ty || 
  isArrayTy g ty || 

  (if isILAppTy g ty then 
      let tcref,_ = destAppTy g ty
      let tdef = tcref.ILTyconRawMetadata
      tdef.IsSealed
   elif (isFSharpInterfaceTy g ty || isFSharpClassTy g ty) then 
      let tcref,_ = destAppTy g ty
      (TryFindBoolAttrib g g.attrib_SealedAttribute tcref.Attribs = Some(true))
   else 
      // All other F# types are sealed
      true)
   
let isComInteropTy g ty =
    let tcr,_ = destAppTy g ty
    TryFindBoolAttrib g g.attrib_ComImportAttribute tcr.Attribs = Some(true)
  
let ValSpecIsCompiledAsInstance g (v:Val) =
    match v.MemberInfo with 
    | Some(membInfo) -> 
        // Note it doesn't matter if we pass 'v.TopValActualParent' or 'v.MemberApparentParent' here. 
        // These only differ if the value is an extension member, and in that case MemberIsCompiledAsInstance always returns 
        // false anyway 
        MemberIsCompiledAsInstance g v.MemberApparentParent v.IsExtensionMember membInfo v.Attribs  
    |  _ -> false

let ValRefIsCompiledAsInstanceMember g (vref: ValRef) = ValSpecIsCompiledAsInstance g vref.Deref


//---------------------------------------------------------------------------
// Crack information about an F# object model call
//---------------------------------------------------------------------------

let GetMemberCallInfo g (vref:ValRef,vFlags) = 
    match vref.MemberInfo with 
    | Some(membInfo) when not vref.IsExtensionMember -> 
      let numEnclTypeArgs = vref.MemberApparentParent.TyparsNoRange.Length
      let virtualCall = 
          (membInfo.MemberFlags.IsOverrideOrExplicitImpl || 
           membInfo.MemberFlags.IsDispatchSlot) && 
          not membInfo.MemberFlags.IsFinal && 
          (match vFlags with VSlotDirectCall -> false | _ -> true)
      let isNewObj    = (membInfo.MemberFlags.MemberKind = MemberKind.Constructor) && (match vFlags with NormalValUse -> true | _ -> false)
      let isSuperInit = (membInfo.MemberFlags.MemberKind = MemberKind.Constructor) && (match vFlags with CtorValUsedAsSuperInit -> true | _ -> false) 
      let isSelfInit  = (membInfo.MemberFlags.MemberKind = MemberKind.Constructor) && (match vFlags with CtorValUsedAsSelfInit -> true | _ -> false) 
      let isCompiledAsInstance = ValRefIsCompiledAsInstanceMember g vref
      let takesInstanceArg = isCompiledAsInstance && not isNewObj
      let isPropGet = (membInfo.MemberFlags.MemberKind = MemberKind.PropertyGet) && (membInfo.MemberFlags.IsInstance = isCompiledAsInstance)
      let isPropSet = (membInfo.MemberFlags.MemberKind = MemberKind.PropertySet) && (membInfo.MemberFlags.IsInstance = isCompiledAsInstance)
      numEnclTypeArgs, virtualCall,isNewObj,isSuperInit,isSelfInit ,takesInstanceArg,isPropGet,isPropSet
    | _ -> 
      0,false,false,false,false,false,false,false

//---------------------------------------------------------------------------
// Active pattern name helpers
//---------------------------------------------------------------------------


let TryGetActivePatternInfo (vref:ValRef) =  
    // First is an optimization to prevent calls to CoreDisplayName, which calls DemangleOperatorName
    let logicalName = vref.LogicalName
    if logicalName.Length = 0 || logicalName.[0] <> '|' then 
       None 
    else 
       ActivePatternInfoOfValName vref.CoreDisplayName

type ActivePatternElemRef with 
    member x.Name = 
        let (APElemRef(_,vref,n)) = x
        match TryGetActivePatternInfo vref with
        | None -> error(InternalError("not an active pattern name", vref.Range))
        | Some (APInfo(_,nms)) -> 
            if n < 0 || n >= List.length nms  then error(InternalError("name_of_apref: index out of range for active pattern refernce", vref.Range));
            List.nth nms n

let mkChoiceTyconRef g m n = 
     match n with 
     | 0 | 1 -> error(InternalError("mkChoiceTyconRef",m))
     | 2 -> g.choice2_tcr
     | 3 -> g.choice3_tcr
     | 4 -> g.choice4_tcr
     | 5 -> g.choice5_tcr
     | 6 -> g.choice6_tcr
     | 7 -> g.choice7_tcr
     | _ -> error(Error(FSComp.SR.tastActivePatternsLimitedToSeven(),m))

let mkChoiceTy g m tinst = 
     match List.length tinst with 
     | 0 -> g.unit_ty
     | 1 -> List.head tinst
     | _ -> mkAppTy (mkChoiceTyconRef g m (List.length tinst)) tinst

let mkChoiceCaseRef g m n i = 
     mkUnionCaseRef (mkChoiceTyconRef g m n) ("Choice"+string (i+1)+"Of"+string n)

type PrettyNaming.ActivePatternInfo with 
    member x.Names = let (APInfo(_,nms)) = x in nms
    member x.IsTotal = let (APInfo(total,_)) = x in total

    member apinfo.ResultType g m rtys = 
        let choicety = mkChoiceTy g m rtys
        if apinfo.IsTotal then choicety else mkOptionTy g choicety
    
    member apinfo.OverallType g m dty rtys = 
        mkFunTy dty (apinfo.ResultType g m rtys)
    

//---------------------------------------------------------------------------
// RewriteExpr: rewrite bottom up with interceptors 
//---------------------------------------------------------------------------

[<NoEquality; NoComparison>]
type ExprRewritingEnv = 
    { PreIntercept: ((Expr -> Expr) -> Expr -> Expr option) option;
      PostTransform: Expr -> Expr option;
      IsUnderQuotations: bool }    

let rec rewrite_bind env (TBind(v,e,letSeqPtOpt)) = TBind(v,RewriteExpr env e,letSeqPtOpt) 

and rewrite_binds env binds = FlatList.map (rewrite_bind env) binds

and RewriteExpr env expr =
  match expr with 
  | Expr.Let _ 
  | Expr.Seq _ ->
      rewriteLinearExpr env expr (fun e -> e)
  | _ -> 
      let expr = 
         match preRewriteExpr env expr with 
         | Some expr -> expr
         | None -> rewriteExprStructure env expr
      postRewriteExpr env expr 

and preRewriteExpr env expr = 
     match env.PreIntercept  with 
     | Some f -> f (RewriteExpr env) expr
     | None -> None 

and postRewriteExpr env expr = 
     match env.PostTransform expr with 
     | None -> expr 
     | Some expr -> expr 

and rewriteExprStructure env expr =  
  match expr with
  | Expr.Const _ 
  | Expr.Val _ -> expr
  | Expr.App(f0,f0ty,tyargs,args,m) -> 
      let f0'   = RewriteExpr env f0
      let args' = rewriteExprs env args
      if f0 === f0' && args === args' then expr
      else Expr.App(f0',f0ty,tyargs,args',m)

  | Expr.Quote(ast,{contents=Some(argTypes,argExprs,data)},m,ty) -> 
      Expr.Quote((if env.IsUnderQuotations then RewriteExpr env ast else ast),{contents=Some(argTypes,rewriteExprs env argExprs,data)},m,ty)
  | Expr.Quote(ast,{contents=None},m,ty) -> 
      Expr.Quote((if env.IsUnderQuotations then RewriteExpr env ast else ast),{contents=None},m,ty)

  | Expr.Obj (_,ty,basev,basecall,overrides,iimpls,m) -> 
      mkObjExpr(ty,basev,RewriteExpr env basecall,List.map (rewriteObjExprOverride env) overrides,
                  List.map (rewriteObjExprInterfaceImpl env) iimpls,m)
  | Expr.Link eref -> 
      RewriteExpr env !eref

  | Expr.Op (c,tyargs,args,m) -> 
      let args' = rewriteExprs env args
      if args === args' then expr 
      else Expr.Op (c,tyargs,args',m)

  | Expr.Lambda(_lambdaId,ctorThisValOpt,baseValOpt,argvs,body,m,rty) -> 
      let body = RewriteExpr env body
      rebuildLambda m ctorThisValOpt baseValOpt argvs (body,rty)

  | Expr.TyLambda(_lambdaId,argtyvs,body,m,rty) -> 
      let body = RewriteExpr env body
      mkTypeLambda m argtyvs (body,rty)

  | Expr.Match(spBind,exprm,dtree,targets,m,ty) -> 
      let dtree' = rewriteDecisionTree env dtree
      let targets' = rewriteTargets env targets
      mkAndSimplifyMatch spBind exprm m ty dtree' targets'

  | Expr.LetRec (binds,e,m,_) ->
      let binds = rewrite_binds env binds
      let e' = RewriteExpr env e
      Expr.LetRec(binds,e',m,NewFreeVarsCache())

  | Expr.Let _ -> failwith "unreachable - linear let"

  | Expr.Seq _ -> failwith "unreachable - linear seq"

  | Expr.StaticOptimization (constraints,e2,e3,m) ->
      let e2' = RewriteExpr env e2
      let e3' = RewriteExpr env e3
      Expr.StaticOptimization(constraints,e2',e3',m)

  | Expr.TyChoose (a,b,m) -> 
      Expr.TyChoose(a,RewriteExpr env b,m)

and rewriteLinearExpr env expr contf =
    // schedule a rewrite on the way back up by adding to the continuation 
    let contf = contf << postRewriteExpr env
    match preRewriteExpr env expr with 
    | Some expr -> contf expr  (* done - intercepted! *)
    | None -> 
        match expr with 
        | Expr.Let (bind,body,m,_) ->  
            let bind = rewrite_bind env bind
            rewriteLinearExpr env body (contf << (fun body' ->
                mkLetBind m bind body'))
        | Expr.Seq  (e1,e2,dir,spSeq,m) ->
            let e1' = RewriteExpr env e1
            rewriteLinearExpr env e2 (contf << (fun e2' ->
                if e1 === e1' && e2 === e2' then expr 
                else Expr.Seq(e1',e2',dir,spSeq,m)))
        | _ -> 
            (* no longer linear *)
            contf (RewriteExpr env expr) 

and rewriteExprs env exprs = List.mapq (RewriteExpr env) exprs
and rewriteFlatExprs env exprs = FlatList.mapq (RewriteExpr env) exprs

and rewriteDecisionTree env x =
  match x with 
  | TDSuccess (es,n) -> 
      let es' = rewriteFlatExprs env es
      if FlatList.physicalEquality es es' then x 
      else TDSuccess(es',n)

  | TDSwitch (e,cases,dflt,m) ->
      let e' = RewriteExpr env e
      let cases' = List.map (fun (TCase(discrim,e)) -> TCase(discrim,rewriteDecisionTree env e)) cases
      let dflt' = Option.map (rewriteDecisionTree env) dflt
      TDSwitch (e',cases',dflt',m)

  | TDBind (bind,body) ->
      let bind' = rewrite_bind env bind
      let body = rewriteDecisionTree env body
      TDBind (bind',body)

and rewriteTarget env (TTarget(vs,e,spTarget)) = TTarget(vs,RewriteExpr env e,spTarget)

and rewriteTargets env targets = List.map (rewriteTarget env) (Array.toList targets)

and rewriteObjExprOverride env (TObjExprMethod(slotsig,attribs,tps,vs,e,m)) =
  TObjExprMethod(slotsig,attribs,tps,vs,RewriteExpr env e,m)

and rewriteObjExprInterfaceImpl env (ty,overrides) = 
  (ty, List.map (rewriteObjExprOverride env) overrides)
    
and rewriteModuleOrNamespaceExpr env x = 
    match x with  
    (* | ModuleOrNamespaceExprWithSig(mty,e,m) -> ModuleOrNamespaceExprWithSig(mty,rewriteModuleOrNamespaceExpr env e,m) *)
    | ModuleOrNamespaceExprWithSig(mty,def,m) ->  ModuleOrNamespaceExprWithSig(mty,rewriteModuleOrNamespaceDef env def,m)

and rewriteModuleOrNamespaceDefs env x = List.map (rewriteModuleOrNamespaceDef env) x
    
and rewriteModuleOrNamespaceDef env x = 
    match x with 
    | TMDefRec(tycons,binds,mbinds,m) -> TMDefRec(tycons,rewrite_binds env binds,rewriteModuleOrNamespaceBindings env mbinds,m)
    | TMDefLet(bind,m)         -> TMDefLet(rewrite_bind env bind,m)
    | TMDefDo(e,m)             -> TMDefDo(RewriteExpr env e,m)
    | TMDefs defs             -> TMDefs(rewriteModuleOrNamespaceDefs env defs)
    | TMAbstract mexpr        -> TMAbstract(rewriteModuleOrNamespaceExpr env mexpr)

and rewriteModuleOrNamespaceBinding env (ModuleOrNamespaceBinding(nm, rhs)) = ModuleOrNamespaceBinding(nm,rewriteModuleOrNamespaceDef env rhs)

and rewriteModuleOrNamespaceBindings env mbinds = List.map (rewriteModuleOrNamespaceBinding env) mbinds

and RewriteImplFile env mv = mapTImplFile (rewriteModuleOrNamespaceExpr env) mv


let isFlagEnumTy (g:TcGlobals) typ = 
   (isEnumTy g typ (* && TyconRefHasAttrib g g.attrib_FlagsAttribute (tcrefOfAppTy typ) *) ) 



//--------------------------------------------------------------------------
// Build a Remap that converts all "local" references to "public" things 
// to be non local references.
//--------------------------------------------------------------------------

let MakeExportRemapping viewedCcu (mspec:ModuleOrNamespace) = 

    let accEntityRemap (entity:Entity) acc = 
        match tryRescopeEntity viewedCcu entity with 
        | Some eref -> 
#if DEBUG
            if !verboseStamps then dprintf "adding export remapping for entity %s#%d\n" entity.LogicalName entity.Stamp;
#endif
            addTyconRefRemap (mkLocalTyconRef entity) eref acc
        | None -> 
            if entity.IsNamespace then 
                acc
            else
                error(InternalError("Unexpected entity without a pubpath when remapping assembly data",entity.Range))

    let accValRemap (vspec:Val) acc = 
        // The acc contains the entity remappings
        match tryRescopeVal viewedCcu acc vspec with 
        | Some vref -> 
#if DEBUG
            if !verboseStamps then dprintf "adding export remapping for value %s#%d\n" vspec.LogicalName vspec.Stamp;
#endif
            {acc with valRemap=acc.valRemap.Add vspec vref }
        | None -> 
            error(InternalError("Unexpected value without a pubpath when remapping assembly data",vspec.Range))

    let mty = mspec.ModuleOrNamespaceType
    let entities = allEntitiesOfModuleOrNamespaceTy mty
    let vs = allValsOfModuleOrNamespaceTy mty
    // Remap the entities first so we can correctly remap the types in the signatures of the ValLinkageFullKey's in the value references
    let acc = List.foldBack accEntityRemap entities Remap.Empty
    let allRemap = List.foldBack accValRemap vs acc
    allRemap

//--------------------------------------------------------------------------
// Apply a "local to nonlocal" renaming to a module type.  This can't use
// remap_mspec since the remapping we want isn't to newly created nodes
// but rather to remap to the nonlocal references. This is deliberately 
// "breaking" the binding structure implicit in the module type, which is
// the whole point - one things are rewritten to use non local references then
// the elements can be copied at will, e.g. when inlining during optimization.
//------------------------------------------------------------------------ 


let rec remapTyconDataToNonLocal g tmenv d = 
    let tps',tmenvinner = tmenvCopyRemapAndBindTypars (remapAttribs g tmenv) tmenv (d.entity_typars.Force(d.entity_range))

    { d with 
          entity_typars         = LazyWithContext.NotLazy tps';
          entity_attribs        = d.entity_attribs        |> remapAttribs g tmenvinner;
          entity_tycon_repr           = d.entity_tycon_repr           |> Option.map (remapTyconRepr g tmenvinner);
          entity_tycon_abbrev         = d.entity_tycon_abbrev         |> Option.map (remapType tmenvinner) ;
          entity_tycon_tcaug          = d.entity_tycon_tcaug          |> remapTyconAug tmenvinner ;
          entity_modul_contents = 
              notlazy (d.entity_modul_contents 
                       |> Lazy.force 
                       |> mapImmediateValsAndTycons (remapTyconToNonLocal g tmenv) 
                                                                  (remapValToNonLocal g tmenv));
          entity_exn_info      = d.entity_exn_info      |> remapTyconExnInfo g tmenvinner}

and remapTyconToNonLocal g tmenv x = 
    x |> NewModifiedTycon (remapTyconDataToNonLocal g tmenv)  

and remapValToNonLocal g  tmenv inp = 
    inp |> NewModifiedVal (remapValData g tmenv)

let ApplyExportRemappingToEntity g tmenv x = remapTyconToNonLocal g tmenv x

(* Which constraints actually get compiled to .NET constraints? *)
let isCompiledConstraint cx = 
    match cx with 
      | TTyparIsNotNullableValueType _ 
      | TTyparIsReferenceType _
      | TTyparRequiresDefaultConstructor _
      | TTyparCoercesToType _ -> true
      | _ -> false
    
// Is a value a first-class polymorphic value with .NET constraints? 
// Used to turn off TLR and method splitting
let IsGenericValWithGenericContraints g (v:Val) = 
    isForallTy g v.Type && 
    v.Type |> destForallTy g |> fst |> List.exists (fun tp -> List.exists isCompiledConstraint tp.Constraints)

// Does a type support a given interface? 
type Entity with 
    member tycon.HasInterface g ty = 
        tycon.TypeContents.tcaug_interfaces |> List.exists (fun (x,_,_) -> typeEquiv g ty x)  

    // Does a type have an override matching the given name and argument types? 
    // Used to detet the presence of 'Equals' and 'GetHashCode' in type checking 
    member tycon.HasOverride g nm argtys = 
        tycon.TypeContents.tcaug_adhoc 
        |> NameMultiMap.find nm
        |> List.exists (fun vref -> 
                          match vref.MemberInfo with 
                          | None -> false 
                          | Some membInfo -> 
                                         let argInfos = ArgInfosOfMember g vref 
                                         argInfos.Length = 1 && 
                                         List.lengthsEqAndForall2 (typeEquiv g) (List.map fst (List.head argInfos)) argtys  &&  
                                         membInfo.MemberFlags.IsOverrideOrExplicitImpl) 

type EntityRef with 
    member tcref.HasInterface g ty = tcref.Deref.HasInterface g ty
    member tcref.HasOverride g nm argtys = tcref.Deref.HasOverride g nm argtys

let mkFastForLoop g (spLet,m,idv:Val,start,dir,finish,body) =
    let dir = if dir then FSharpForLoopUp else FSharpForLoopDown 
    mkFor g (spLet,idv,start,dir,finish,body,m)


/// Accessing a binding of the form "let x = 1" or "let x = e" for any "e" satisfying the predicate
/// below does not cause an initialization trigger, i.e. does not get compiled as a static field.
let IsSimpleSyntacticConstantExpr g inputExpr = 
    let rec checkExpr (vrefs: Set<Stamp>) x = 
        match stripExpr x with 
        | Expr.Op (TOp.Coerce,_,[arg],_) 
             -> checkExpr vrefs arg
        | UnopExpr g (vref,arg) 
             when (valRefEq g vref g.unchecked_unary_minus_vref ||
                   valRefEq g vref g.unchecked_unary_plus_vref ||
                   valRefEq g vref g.unchecked_unary_not_vref ||
                   valRefEq g vref g.bitwise_unary_not_vref ||
                   valRefEq g vref g.enum_vref)
             -> checkExpr vrefs arg
        // compare, =, <>, +, -, <, >, <=, >=, <<<, >>>, &&&
        | BinopExpr g (vref, arg1, arg2) 
             when (valRefEq g vref g.equals_operator_vref  ||
                   valRefEq g vref g.compare_operator_vref  ||
                   valRefEq g vref g.unchecked_addition_vref  ||
                   valRefEq g vref g.less_than_operator_vref  ||
                   valRefEq g vref g.less_than_or_equals_operator_vref  ||
                   valRefEq g vref g.greater_than_operator_vref  ||
                   valRefEq g vref g.greater_than_or_equals_operator_vref  ||
                   valRefEq g vref g.not_equals_operator_vref  ||
                   valRefEq g vref g.unchecked_addition_vref  ||
                   valRefEq g vref g.unchecked_multiply_vref  ||
                   valRefEq g vref g.unchecked_subtraction_vref  ||
        // Note: division and modulus can raise exceptions, so are not included
                   valRefEq g vref g.bitwise_shift_left_vref  ||
                   valRefEq g vref g.bitwise_shift_right_vref  ||
                   valRefEq g vref g.bitwise_xor_vref  ||
                   valRefEq g vref g.bitwise_and_vref  ||
                   valRefEq g vref g.bitwise_or_vref) &&
                   (not (typeEquiv g (tyOfExpr g arg1) g.string_ty)  && not (typeEquiv g (tyOfExpr g arg1) g.decimal_ty) )
                -> checkExpr vrefs arg1 && checkExpr vrefs arg2 
        | Expr.Val(vref,_,_) -> vref.Deref.IsCompiledAsStaticPropertyWithoutField || vrefs.Contains vref.Stamp
        | Expr.Match(_,_,dtree,targets,_,_) -> checkDecisionTree vrefs dtree && targets |> Array.forall (checkDecisionTreeTarget vrefs)
        | Expr.Let(b,e,_,_) -> checkExpr vrefs b.Expr && checkExpr (vrefs.Add b.Var.Stamp) e
        // Detect standard constants 
        | Expr.TyChoose (_,b,_) -> checkExpr vrefs b
        | Expr.Const _ 
        | Expr.Op (TOp.UnionCase _,_,[],_)         // Nullary union cases
        | UncheckedDefaultOfExpr g _ 
        | SizeOfExpr g _ 
        | TypeOfExpr g _ -> true
        // All others are not simple constant expressions
        | _ -> false

    and checkDecisionTree vrefs x = 
        match x with 
        | TDSuccess (es,_n) -> es |> FlatList.forall (checkExpr vrefs)
        | TDSwitch (e,cases,dflt,_m) -> checkExpr vrefs e && cases |> List.forall (checkDecisionTreeCase vrefs) && dflt |> Option.forall (checkDecisionTree vrefs)
        | TDBind (bind,body) -> checkExpr vrefs bind.Expr && checkDecisionTree (vrefs.Add bind.Var.Stamp) body
    and checkDecisionTreeCase vrefs (TCase(discrim,dtree)) = 
       (match discrim with Test.Const _c -> true | _ -> false) && checkDecisionTree vrefs dtree
    and checkDecisionTreeTarget vrefs (TTarget(vs,e,_)) = 
       let vrefs = ((vrefs, vs) ||> FlatList.fold (fun s v -> s.Add v.Stamp)) 
       checkExpr vrefs e

    checkExpr Set.empty inputExpr    
    

// See also PostTypecheckSemanticChecks.CheckAttribArgExpr, which must match this precisely
let rec EvalAttribArgExpr g x = 
    match x with 

    // Detect standard constants 
    | Expr.Const(c,m,_) -> 
        match c with 
        | Const.Bool _ 
        | Const.Int32 _ 
        | Const.SByte  _
        | Const.Int16  _
        | Const.Int32 _
        | Const.Int64 _  
        | Const.Byte  _
        | Const.UInt16  _
        | Const.UInt32  _
        | Const.UInt64  _
        | Const.Double _
        | Const.Single _
        | Const.Char _
        | Const.Zero _
        | Const.String _  -> x
        | _ -> 
            errorR (Error ( FSComp.SR.tastConstantCannotBeCustomAttribute(),m)); 
            x

    | TypeOfExpr g _ -> x
    | TypeDefOfExpr g _ -> x
    | Expr.Op (TOp.Coerce,_,[arg],_) -> 
        EvalAttribArgExpr g arg
    | EnumExpr g arg1 -> 
        EvalAttribArgExpr g arg1
    // Detect bitwise or of attribute flags
    | AttribBitwiseOrExpr g (arg1, arg2) ->
        match EvalAttribArgExpr g arg1, EvalAttribArgExpr g arg2 with 
        | Expr.Const(Const.Int32  x1,m,ty), Expr.Const(Const.Int32  x2,_,_) -> Expr.Const(Const.Int32  (x1 ||| x2),m,ty)
        | Expr.Const(Const.SByte  x1,m,ty), Expr.Const(Const.SByte  x2,_,_) -> Expr.Const(Const.SByte  (x1 ||| x2),m,ty)
        | Expr.Const(Const.Int16  x1,m,ty), Expr.Const(Const.Int16  x2,_,_) -> Expr.Const(Const.Int16  (x1 ||| x2),m,ty)
        | Expr.Const(Const.Int64  x1,m,ty), Expr.Const(Const.Int64  x2,_,_) -> Expr.Const(Const.Int64  (x1 ||| x2),m,ty)
        | Expr.Const(Const.Byte   x1,m,ty), Expr.Const(Const.Byte   x2,_,_) -> Expr.Const(Const.Byte   (x1 ||| x2),m,ty)
        | Expr.Const(Const.UInt16 x1,m,ty), Expr.Const(Const.UInt16 x2,_,_) -> Expr.Const(Const.UInt16 (x1 ||| x2),m,ty)
        | Expr.Const(Const.UInt32 x1,m,ty), Expr.Const(Const.UInt32 x2,_,_) -> Expr.Const(Const.UInt32 (x1 ||| x2),m,ty)
        | Expr.Const(Const.UInt64 x1,m,ty), Expr.Const(Const.UInt64 x2,_,_) -> Expr.Const(Const.UInt64 (x1 ||| x2),m,ty)
        | _ -> x
    | _ -> 
        errorR (Error ( FSComp.SR.tastNotAConstantExpression(),x.Range)); 
        x


and EvaledAttribExprEquality g e1 e2 = 
    match e1,e2 with 
    | Expr.Const(c1,_,_),Expr.Const(c2,_,_) -> c1 = c2
    | TypeOfExpr g ty1, TypeOfExpr g ty2  -> typeEquiv g ty1 ty2
    | TypeDefOfExpr g ty1, TypeDefOfExpr g ty2 -> typeEquiv g ty1 ty2
    | _ -> false


let EvalAttribArg g x = 
    match x with 
    | Expr.Op (TOp.Coerce,_,[Expr.Op (TOp.Array,[elemTy],args,m)],_)
    | Expr.Op (TOp.Array,[elemTy],args,m) ->
        let args = args |> List.map (EvalAttribArgExpr g) 
        Expr.Op (TOp.Array,[elemTy],args,m) 
    | _ -> 
        EvalAttribArgExpr g x

// Take into account the fact that some "instance" members are compiled as static
// members when usinging CompilationRepresentation.Static, or any non-virtual instance members
// in a type that supports "null" as a true value. This is all members
// where ValRefIsCompiledAsInstanceMember is false but membInfo.MemberFlags.IsInstance 
// is true.
//
// This is the right abstraction for viewing member types, but the implementation
// below is a little ugly.
let GetTypeOfIntrinsicMemberInCompiledForm g (vref:ValRef) =
    assert (not vref.IsExtensionMember)
    let membInfo,topValInfo = checkMemberValRef vref
    let tps,argInfos,rty,retInfo = GetTypeOfMemberInMemberForm g vref
    let argInfos = 
        // Check if the thing is really an instance member compiled as a static member
        // If so, the object argument counts as a normal argument in the compiled form
        if membInfo.MemberFlags.IsInstance && not (ValRefIsCompiledAsInstanceMember g vref) then 
            let _,origArgInfos,_,_ = GetTopValTypeInFSharpForm g topValInfo vref.Type vref.Range
            match origArgInfos with
            | [] -> 
                errorR(InternalError("value does not have a valid member type",vref.Range)); 
                argInfos
            | h::_ -> h ::argInfos
        else argInfos
    tps,argInfos,rty,retInfo


//--------------------------------------------------------------------------
// Tuple compilation (expressions)
//------------------------------------------------------------------------ 


let rec mkCompiledTuple g (argtys,args,m) = 
    let n = List.length argtys 
    if n <= 0 then failwith "mkCompiledTuple"
    elif n < maxTuple then  (mkCompiledTupleTyconRef g argtys, argtys, args, m)
    else
        let argtysA,argtysB = List.splitAfter goodTupleFields argtys
        let argsA,argsB = List.splitAfter (goodTupleFields) args
        let ty8, v8 = 
            match argtysB,argsB with 
            | [ty8],[arg8] -> 
                match ty8 with
                // if it's already been nested or ended, pass it through
                |  TType_app(tn, _)  when (is_tuple_tcref g tn) ->
                    ty8,arg8
                | _ ->
                    let ty8enc = TType_app(g.tuple1_tcr,[ty8])
                    let v8enc = Expr.Op (TOp.Tuple,[ty8],[arg8],m) 
                    ty8enc,v8enc
            | _ -> 
                let a,b,c,d = mkCompiledTuple g (argtysB, argsB, m)
                let ty8plus = TType_app(a,b)
                let v8plus = Expr.Op (TOp.Tuple,b,c,d)
                ty8plus,v8plus
        let argtysAB = argtysA @ [ty8] 
        (mkCompiledTupleTyconRef g argtysAB, argtysAB,argsA @ [v8],m)

let mkILMethodSpecForTupleItem (_g : TcGlobals) (typ:ILType) n = 
    mkILNonGenericInstanceMethSpecInTy(typ, (if n < goodTupleFields then "get_Item"+(n+1).ToString() else "get_Rest"), [], mkILTyvarTy (uint16 n))

let mkGetTupleItemN g m n typ te retty =
    mkAsmExpr([IL.mkNormalCall(mkILMethodSpecForTupleItem g typ n)],[],[te],[retty],m)

/// Match an Int32 constant expression
let (|Int32Expr|_|) expr = 
    match expr with 
    | Expr.Const(Const.Int32 n,_,_) -> Some n
    | _ -> None 

/// Match a try-finally expression
let (|TryFinally|_|) expr = 
    match expr with 
    | Expr.Op (TOp.TryFinally _,[_resty],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],e2,_,_)],_) -> Some(e1,e2)
    | _ -> None
    
// detect ONLY the while loops that result from compiling 'for ... in ... do ...'
let (|WhileLoopForCompiledForEachExpr|_|) expr = 
    match expr with 
    | Expr.Op (TOp.While (_, WhileLoopForCompiledForEachExprMarker),_,[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],e2,_,_)],m) -> Some(e1,e2,m)
    | _ -> None
    
let (|Let|_|) expr = 
    match expr with 
    | Expr.Let(TBind(v,e1,sp),e2,_,_) -> Some(v,e1,sp,e2)
    | _ -> None

let (|RangeInt32Step|_|) g expr = 
    match expr with 
    // detect 'n .. m' 
    | Expr.App(Expr.Val(vf,_,_),_,[tyarg],[startExpr;finishExpr],_)
         when valRefEq g vf g.range_op_vref && typeEquiv g tyarg g.int_ty -> Some(startExpr, 1, finishExpr)
    
    // detect (RangeInt32 startExpr N finishExpr), the inlined/compiled form of 'n .. m' and 'n .. N .. m'
    | Expr.App(Expr.Val(vf,_,_),_,[],[startExpr; Int32Expr n; finishExpr],_) 
         when valRefEq g vf g.range_int32_op_vref -> Some(startExpr, n, finishExpr)

    | _ -> None

         
// Detect the compiled or optimized form of a 'for <elemVar> in <startExpr> .. <finishExpr>  do <bodyExpr>' expression over integers
// Detect the compiled or optimized form of a 'for <elemVar> in <startExpr> .. <step> .. <finishExpr>  do <bodyExpr>' expression over integers when step is positive
let (|CompiledInt32ForEachExprWithKnownStep|_|) g expr = 
    match expr with 
    | Let (_enumerableVar, RangeInt32Step g (startExpr, step, finishExpr), _, 
           Let (_enumeratorVar, _getEnumExpr, spBind,
              TryFinally (WhileLoopForCompiledForEachExpr (_guardExpr, Let (elemVar,_currentExpr,_,bodyExpr), m), _cleanupExpr))) -> 

        let spForLoop = match spBind with SequencePointAtBinding(spStart) -> SequencePointAtForLoop(spStart) |  _ -> NoSequencePointAtForLoop 

        Some(spForLoop,elemVar,startExpr,step,finishExpr,bodyExpr,m)
    | _ -> 
        None

let DetectFastIntegerForLoops g expr = 
    match expr with 
    | CompiledInt32ForEachExprWithKnownStep g (spForLoop,elemVar,startExpr,step,finishExpr,bodyExpr,m) 
         // fast for loops only allow steps 1 and -1 steps at the moment
         when step = 1 || step = -1 -> 
            mkFastForLoop  g (spForLoop,m,elemVar,startExpr,(step = 1),finishExpr,bodyExpr)
    | _ -> expr


