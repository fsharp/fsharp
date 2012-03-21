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
  
module internal Microsoft.FSharp.Compiler.Tast 

open System
open System.Collections.Generic 
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.QuotationPickler
open Microsoft.FSharp.Core.Printf
open System.Runtime.CompilerServices

#if DEBUG
///verboseStamps: print #stamp on each id -- very verbose - but sometimes useful. Turn on using '--stamps'
let verboseStamps = ref false
#endif

/// Unique name generator for stamps attached to lambdas and object expressions
type Unique = int64
//++GLOBAL MUTABLE STATE
let newUnique = let i = ref 0L in fun () -> i := !i + 1L; !i
type Stamp = int64

/// Unique name generator for stamps attached to to val_specs, tycon_specs etc.
//++GLOBAL MUTABLE STATE
let newStamp = let i = ref 0L in fun () -> i := !i + 1L; !i

type StampMap<'T> = Map<Stamp,'T>

//-------------------------------------------------------------------------
// Flags

type ValInlineInfo =
    /// Indicates the value must always be inlined 
    | PseudoValue 
    /// Indictes the value is inlined but the code for the function still exists, e.g. to satisfy interfaces on objects, but that it is also always inlined 
    | AlwaysInline 
    | OptionalInline
    | NeverInline

let mustinline = function PseudoValue | AlwaysInline -> true | OptionalInline | NeverInline -> false

type ValRecursiveScopeInfo =
    /// Set while the value is within its recursive scope. The flag indicates if the value has been eagerly generalized and accepts generic-recursive calls 
    | ValInRecScope of bool
    /// The normal value for this flag when the value is not within its recursive scope 
    | ValNotInRecScope

type ValMutability   = 
    | Immutable 
    | Mutable

type TyparDynamicReq = 
    /// Indicates the type parameter is not needed at runtime and may be eliminated
    | NoDynamicReq 
    /// Indicates the type parameter is needed at runtime and may not be eliminated
    | DynamicReq

type ValBaseOrThisInfo = 
    /// Indicates a ref-cell holding 'this' or the implicit 'this' used throughout an 
    /// implicit constructor to access and set values
    | CtorThisVal 
    /// Indicates the value called 'base' available for calling base class members
    | BaseVal 
    /// Indicates a normal value
    | NormalVal 
    /// Indicates the 'this' value specified in a memberm e.g. 'x' in 'member x.M() = 1'
    | MemberThisVal

//---------------------------------------------------------------------------
// Flags on values
//---------------------------------------------------------------------------

[<Struct>]
type ValFlags(flags:int64) = 

    new (recValInfo, baseOrThis, isCompGen, inlineInfo, isMutable, isModuleOrMemberBinding, isExtensionMember, isIncrClassSpecialMember, isTyFunc, allowTypeInst) =
        let flags = 
                     (match baseOrThis with
                                        | BaseVal ->           0b000000000000000000L
                                        | CtorThisVal ->       0b000000000000000010L
                                        | NormalVal ->         0b000000000000000100L
                                        | MemberThisVal ->     0b000000000000000110L) |||
                     (if isCompGen then                        0b000000000000001000L 
                      else                                     0b000000000000000000L) |||
                     (match inlineInfo with
                                        | PseudoValue ->       0b000000000000000000L
                                        | AlwaysInline ->      0b000000000000010000L
                                        | OptionalInline ->    0b000000000000100000L
                                        | NeverInline ->       0b000000000000110000L) |||
                     (match isMutable with
                                        | Immutable ->         0b000000000000000000L
                                        | Mutable   ->         0b000000000001000000L) |||

                     (match isModuleOrMemberBinding with
                                        | false     ->         0b000000000000000000L
                                        | true      ->         0b000000000010000000L) |||
                     (match isExtensionMember with
                                        | false     ->         0b000000000000000000L
                                        | true      ->         0b000000000100000000L) |||
                     (match isIncrClassSpecialMember with
                                        | false     ->         0b000000000000000000L
                                        | true      ->         0b000000001000000000L) |||
                     (match isTyFunc with
                                        | false     ->         0b000000000000000000L
                                        | true      ->         0b000000010000000000L) |||

                     (match recValInfo with
                                     | ValNotInRecScope     -> 0b000000000000000000L
                                     | ValInRecScope(true)  -> 0b000000100000000000L
                                     | ValInRecScope(false) -> 0b000001000000000000L) |||

                     (match allowTypeInst with
                                        | false     ->         0b000000000000000000L
                                        | true      ->         0b000100000000000000L)

        ValFlags(flags)

    member x.BaseOrThisInfo = 
                                  match (flags       &&&       0b000000000000000110L) with 
                                                             | 0b000000000000000000L -> BaseVal
                                                             | 0b000000000000000010L -> CtorThisVal
                                                             | 0b000000000000000100L -> NormalVal
                                                             | 0b000000000000000110L -> MemberThisVal
                                                             | _          -> failwith "unreachable"



    member x.IsCompilerGenerated =      (flags       &&&       0b000000000000001000L) <> 0x0L

    member x.SetIsCompilerGenerated(isCompGen) = 
            let flags =                 (flags       &&&    ~~~0b000000000000001000L) |||
                                        (match isCompGen with
                                          | false           -> 0b000000000000000000L
                                          | true            -> 0b000000000000001000L)
            ValFlags(flags)

    member x.InlineInfo = 
                                  match (flags       &&&       0b000000000000110000L) with 
                                                             | 0b000000000000000000L -> PseudoValue
                                                             | 0b000000000000010000L -> AlwaysInline
                                                             | 0b000000000000100000L -> OptionalInline
                                                             | 0b000000000000110000L -> NeverInline
                                                             | _          -> failwith "unreachable"

    member x.MutabilityInfo = 
                                  match (flags       &&&       0b000000000001000000L) with 
                                                             | 0b000000000000000000L -> Immutable
                                                             | 0b000000000001000000L -> Mutable
                                                             | _          -> failwith "unreachable"


    member x.IsMemberOrModuleBinding = 
                                  match (flags       &&&       0b000000000010000000L) with 
                                                             | 0b000000000000000000L -> false
                                                             | 0b000000000010000000L -> true
                                                             | _          -> failwith "unreachable"


    member x.SetIsMemberOrModuleBinding = ValFlags(flags |||   0b000000000010000000L)


    member x.IsExtensionMember        = (flags       &&&       0b000000000100000000L) <> 0L
    member x.IsIncrClassSpecialMember = (flags       &&&       0b000000001000000000L) <> 0L
    member x.IsTypeFunction           = (flags       &&&       0b000000010000000000L) <> 0L

    member x.RecursiveValInfo =   match (flags       &&&       0b000001100000000000L) with 
                                                             | 0b000000000000000000L -> ValNotInRecScope
                                                             | 0b000000100000000000L -> ValInRecScope(true)
                                                             | 0b000001000000000000L -> ValInRecScope(false)
                                                             | _                   -> failwith "unreachable"

    member x.SetRecursiveValInfo(recValInfo) = 
            let flags = 
                     (flags       &&&                       ~~~0b000001100000000000L) |||
                     (match recValInfo with
                                     | ValNotInRecScope     -> 0b000000000000000000L
                                     | ValInRecScope(true)  -> 0b000000100000000000L
                                     | ValInRecScope(false) -> 0b000001000000000000L) 
            ValFlags(flags)

    member x.MakesNoCriticalTailcalls         = (flags &&&     0b000010000000000000L) <> 0L

    member x.SetMakesNoCriticalTailcalls = ValFlags(flags |||  0b000010000000000000L)

    member x.PermitsExplicitTypeInstantiation = (flags &&&     0b000100000000000000L) <> 0L
    member x.HasBeenReferenced                = (flags &&&     0b001000000000000000L) <> 0L

    member x.SetHasBeenReferenced        = ValFlags(flags |||  0b001000000000000000L)

    member x.IsCompiledAsStaticPropertyWithoutField       = (flags &&&     0b010000000000000000L) <> 0L

    member x.SetIsCompiledAsStaticPropertyWithoutField   = ValFlags(flags |||  0b010000000000000000L)
    /// Get the flags as included in the F# binary metadata
    member x.PickledBits = 
        // Clear the RecursiveValInfo, only used during inference and irrelevant across assembly boundaries
        // Clear the IsCompiledAsStaticPropertyWithoutField, only used to determine whether to use a true field for a value, and to eliminate the optimization info for observable bindings
        // Clear the HasBeenReferenced, only used to report "unreferenced variable" warnings and to help collect 'it' values in FSI.EXE
                                        (flags       &&&    ~~~0b011001100000000000L) 

type TyparKind = 
    | KindType 
    | KindMeasure
    member x.AttrName =
      match x with
      | KindType -> None
      | KindMeasure -> Some "Measure"
    override x.ToString() = 
      match x with
      | KindType -> "type"
      | KindMeasure -> "measure"

type TyparRigidity = 
    /// Indicates the type parameter can't be solved
    | TyparRigid 
    /// Indicates the type parameter can't be solved, but the variable is not set to "rigid" until after inference is complete
    | TyparWillBeRigid 
    /// Indicates we give a warning if the type parameter is ever solved
    | TyparWarnIfNotRigid 
    /// Indicates the type parameter is an inference variable may be solved
    | TyparFlexible
    /// Indicates the type parameter derives from an '_' anonymous type
    /// For units-of-measure, we give a warning if this gets solved to '1'
    | TyparAnon
    member x.ErrorIfUnified = match x with TyparRigid -> true | _ -> false
    member x.WarnIfUnified = match x with TyparWillBeRigid | TyparWarnIfNotRigid -> true | _ -> false
    member x.WarnIfMissingConstraint = match x with TyparWillBeRigid -> true | _ -> false


/// Encode typar flags into a bit field  
[<Struct>]
type TyparFlags(flags:int32) =

    new (kind:TyparKind, rigidity:TyparRigidity, isFromError:bool, isCompGen:bool, staticReq:TyparStaticReq, dynamicReq:TyparDynamicReq, equalityDependsOn: bool, comparisonDependsOn: bool) = 
        TyparFlags((if isFromError then       0b000000000010 else 0) |||
                   (if isCompGen   then       0b000000000100 else 0) |||
                   (match staticReq with
                     | NoStaticReq         -> 0b000000000000
                     | HeadTypeStaticReq   -> 0b000000001000) |||
                   (match rigidity with
                     | TyparRigid          -> 0b000000000000
                     | TyparWillBeRigid    -> 0b000000100000
                     | TyparWarnIfNotRigid -> 0b000001000000
                     | TyparFlexible       -> 0b000001100000
                     | TyparAnon           -> 0b000010000000) |||
                   (match kind with
                     | KindType            -> 0b000000000000
                     | KindMeasure         -> 0b000100000000) |||
                   (if comparisonDependsOn then 
                                              0b001000000000 else 0) |||
                   (match dynamicReq with
                     | NoDynamicReq        -> 0b000000000000
                     | DynamicReq          -> 0b010000000000) |||
                   (if equalityDependsOn then 
                                              0b100000000000 else 0))

    member x.IsFromError         = (flags &&& 0b000000000010) <> 0x0
    member x.IsCompilerGenerated = (flags &&& 0b000000000100) <> 0x0
    member x.StaticReq           = 
                             match (flags &&& 0b000000001000) with 
                                            | 0b000000000000 -> NoStaticReq
                                            | 0b000000001000 -> HeadTypeStaticReq
                                            | _             -> failwith "unreachable"

    member x.Rigidity = 
                             match (flags &&& 0b000011100000) with 
                                            | 0b000000000000 -> TyparRigid
                                            | 0b000000100000 -> TyparWillBeRigid
                                            | 0b000001000000 -> TyparWarnIfNotRigid
                                            | 0b000001100000 -> TyparFlexible
                                            | 0b000010000000 -> TyparAnon
                                            | _          -> failwith "unreachable"

    member x.Kind           = 
                             match (flags &&& 0b000100000000) with 
                                            | 0b000000000000 -> KindType
                                            | 0b000100000000 -> KindMeasure
                                            | _             -> failwith "unreachable"


    member x.ComparisonConditionalOn =
                                   (flags &&& 0b001000000000) <> 0x0
    member x.DynamicReq     = 
                             match (flags &&& 0b010000000000) with 
                                            | 0b000000000000 -> NoDynamicReq
                                            | 0b010000000000 -> DynamicReq
                                            | _             -> failwith "unreachable"
    member x.EqualityConditionalOn = 
                                   (flags &&& 0b100000000000) <> 0x0


    /// Get the flags as included in the F# binary metadata. We pickle this as int64 to allow for future expansion
    member x.PickledBits =         flags       

/// Encode entity flags into a bit field. We leave lots of space to allow for future expansion.
[<Struct>]
type EntityFlags(flags:int64) =

    new (usesPrefixDisplay, isModuleOrNamespace, preEstablishedHasDefaultCtor, hasSelfReferentialCtor) = 
        EntityFlags((if isModuleOrNamespace then                        0b00000000001L else 0L) |||
                    (if usesPrefixDisplay   then                        0b00000000010L else 0L) |||
                    (if preEstablishedHasDefaultCtor then               0b00000000100L else 0L) |||
                    (if hasSelfReferentialCtor then                     0b00000001000L else 0L)) 

    member x.IsModuleOrNamespace                 = (flags       &&&     0b00000000001L) <> 0x0L
    member x.IsPrefixDisplay                     = (flags       &&&     0b00000000010L) <> 0x0L
    
    // This bit is not pickled, only used while establishing a type constructor. It is needed because the type constructor
    // is known to satisfy the default constructor constraint even before any of its members have been established.
    member x.PreEstablishedHasDefaultConstructor = (flags       &&&     0b00000000100L) <> 0x0L

    // This bit represents an F# specific condition where a type has at least one constructor that may access
    // the 'this' pointer prior to successful initialization of the partial contents of the object. In this
    // case sub-classes must protect themselves against early access to their contents.
    member x.HasSelfReferentialConstructor       = (flags       &&&     0b00000001000L) <> 0x0L

    /// Get the flags as included in the F# binary metadata
    member x.PickledBits =                         (flags       &&&  ~~~0b00000000100L)


#if DEBUG
assert (sizeof<ValFlags> = 8)
assert (sizeof<EntityFlags> = 8)
assert (sizeof<TyparFlags> = 4)
#endif


let unassignedTyparName = "?"

exception UndefinedName of int * (* error func that expects identifier name *)(string -> string) * Ident * string list
exception InternalUndefinedItemRef of (string * string * string -> int * string) * string * string * string

let AddTyconsByDemangledNameAndArity nm (typars:'a list) x tab = 
    let nm = DemangleGenericTypeName nm 
    Map.add (NameArityPair(nm, typars.Length)) x tab

let AddTyconsByAccessNames nm x tab = 
    if IsMangledGenericName nm then 
        let dnm = DemangleGenericTypeName nm 
        let res = NameMultiMap.add nm x tab 
        NameMultiMap.add dnm x res
    else
        NameMultiMap.add nm x tab 
       
type ModuleOrNamespaceKind = 
    /// Indicates that a module is compiled to a class with the "Module" suffix added. 
    | FSharpModuleWithSuffix 
    /// Indicates that a module is compiled to a class with the same name as the original module 
    | FSharpModule 
    /// Indicates that a 'module' is really a namespace 
    | Namespace


/// The information ILXGEN needs about the location of an item
type CompilationPath = 
    | CompPath of ILScopeRef * (string * ModuleOrNamespaceKind) list
    member x.ILScopeRef = (let (CompPath(scoref,_)) = x in scoref)
    member x.AccessPath = (let (CompPath(_,p)) = x in p)


/// A public path records where a construct lives within the global namespace
/// of a CCU.
type PublicPath      = 
    | PubPath of string[] 
    member x.EnclosingPath = 
        let (PubPath(pp)) = x 
        assert (pp.Length >= 1)
        pp.[0..pp.Length-2]


let getNameOfScopeRef sref = 
    match sref with 
    | ILScopeRef.Local -> "<local>"
    | ILScopeRef.Module mref -> mref.Name
    | ILScopeRef.Assembly aref -> aref.Name
let mangledTextOfCompPath (CompPath(scoref,path)) = getNameOfScopeRef scoref + "/" + textOfPath (List.map fst path)
  
let mangledPathOfCompPath (CompPath(_,path))  = List.map fst path
let publicPathOfCompPath (id:Ident) cpath = PubPath(Array.append (Array.ofList (mangledPathOfCompPath cpath)) [| id.idText |])
let parentCompPath (CompPath(scoref,cpath)) = 
    let a,_ = List.frontAndBack cpath 
    CompPath(scoref,a)
let mkNestedCPath (CompPath(scoref,p)) n modKind = CompPath(scoref,p@[(n,modKind)])


// Type definitions, exception definitions, module definitions and
// namespace definitions are all 'entities'. These have too much in common to make it 
// worth factoring them out as separate types.
//
// Tycons, exncs and moduls are all modelled via tycon_specs, 
// they have different name-resolution logic. 
// For example, an excon ABC really correspond to a type called 
// ABCException with a union case ABC. At the moment they are 
// simply indexed in the excon table as the discriminator constructor ABC. 
type Entity = 
    { mutable Data: EntityData }
    member x.LogicalName = x.Data.entity_logical_name
    member x.CompiledName = match x.Data.entity_compiled_name with None -> x.LogicalName | Some s -> s
    member x.DisplayName = DemangleGenericTypeName x.Data.entity_logical_name
    member x.DisplayNameWithUnderscoreTypars = 
        match x.Typars(x.Range) with 
        | [] -> x.DisplayName
        | tps -> x.DisplayName + "<" + String.concat "," (Array.create tps.Length "_") + ">"
    
    member x.Range = x.Data.entity_range
    member x.Stamp = x.Data.entity_stamp
    member x.Attribs = x.Data.entity_attribs
    member x.XmlDoc = x.Data.entity_xmldoc
    member x.XmlDocSig 
        with get() = x.Data.entity_xmldocsig
        and set(v) = x.Data.entity_xmldocsig <- v
    member x.ModuleOrNamespaceType = x.Data.entity_modul_contents.Force()
    member x.TypeContents = x.Data.entity_tycon_tcaug
    member x.TypeOrMeasureKind = x.Data.entity_kind
    member x.Id = ident(x.LogicalName, x.Range)
    member x.TypeReprInfo = x.Data.entity_tycon_repr
    member x.ExceptionInfo = x.Data.entity_exn_info
    member x.IsExceptionDecl = match x.ExceptionInfo with TExnNone -> false | _ -> true

    static member DemangleEntityName nm k =  
        match k with 
        | FSharpModuleWithSuffix -> String.dropSuffix nm FSharpModuleSuffix
        | _ -> nm

    member x.DemangledModuleOrNamespaceName =  
          Entity.DemangleEntityName x.LogicalName x.ModuleOrNamespaceType.ModuleOrNamespaceKind
    
    member x.Typars(m) = x.Data.entity_typars.Force(m) // lazy because it may read metadata, must provide a context "range" in case error occurs reading metadata
    member x.TyparsNoRange = x.Typars(x.Range)
    member x.TypeAbbrev = x.Data.entity_tycon_abbrev
    member x.IsTypeAbbrev = x.TypeAbbrev.IsSome
    member x.TypeReprAccessibility = x.Data.entity_tycon_repr_accessibility
    member x.CompiledReprCache = x.Data.entity_il_repr_cache
    member x.PublicPath = x.Data.entity_pubpath
    member x.Accessibility = x.Data.entity_accessiblity
    /// Indicates the type prefers the "tycon<a,b>" syntax for display etc. 
    member x.IsPrefixDisplay = x.Data.entity_flags.IsPrefixDisplay
    /// Indicates the "tycon blob" is actually a module 
    member x.IsModuleOrNamespace = x.Data.entity_flags.IsModuleOrNamespace
    member x.IsNamespace = x.IsModuleOrNamespace && (match x.ModuleOrNamespaceType.ModuleOrNamespaceKind with Namespace -> true | _ -> false)
    member x.IsModule = x.IsModuleOrNamespace && (match x.ModuleOrNamespaceType.ModuleOrNamespaceKind with Namespace -> false | _ -> true)
    member x.CompilationPathOpt = x.Data.entity_cpath 
    member x.CompilationPath = 
        match x.CompilationPathOpt with 
        | Some cpath -> cpath 
        | None -> error(Error(FSComp.SR.tastTypeOrModuleNotConcrete(x.LogicalName),x.Range))
    
    member x.AllFieldTable = 
        match x.TypeReprInfo with 
        | Some (TRecdRepr x | TFsObjModelRepr {fsobjmodel_rfields=x}) -> x
        |  _ -> 
        match x.ExceptionInfo with 
        | TExnFresh x -> x
        | _ -> 
        { FieldsByIndex = [| |] 
          FieldsByName = NameMap.empty }

    member x.AllFieldsArray = x.AllFieldTable.FieldsByIndex
    member x.AllFieldsAsList = x.AllFieldsArray |> Array.toList

    // NOTE: This method is over-used...
    member x.AllInstanceFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsStatic)
    member x.TrueFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsCompilerGenerated)
    member x.TrueInstanceFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsStatic && not f.IsCompilerGenerated)

    member x.GetFieldByIndex(n) = x.AllFieldTable.FieldByIndex(n)
    member x.GetFieldByName(n) = x.AllFieldTable.FieldByName(n)

    member x.UnionTypeInfo = 
        match x.Data.entity_tycon_repr with 
        | Some (TFiniteUnionRepr x) -> Some x 
        |  _ -> None

    member x.UnionCasesArray = 
        match x.UnionTypeInfo with 
        | Some x -> x.CasesTable.CasesByIndex 
        | None -> [| |] 

    member x.UnionCasesAsList = x.UnionCasesArray |> Array.toList

    member x.GetUnionCaseByName n =
        match x.UnionTypeInfo with 
        | Some x  -> NameMap.tryFind n x.CasesTable.CasesByName
        | None -> None

    
    // OSGN support
    static member NewUnlinked() : Entity = { Data = nullableSlotEmpty() }
    static member New reason data : Entity  = 
#if DEBUG
        if !verboseStamps then 
            dprintf "entity %s#%d (%s)\n" data.entity_logical_name data.entity_stamp reason
#endif
        { Data = data }
    member x.Link(tg) = x.Data <- nullableSlotFull(tg)
    member x.IsLinked = match box x.Data with null -> false | _ -> true 

    override x.ToString() = x.LogicalName

    member x.FSharpObjectModelTypeInfo = 
         match x.Data.entity_tycon_repr with 
         | Some (TFsObjModelRepr x) -> x 
         |  _ -> failwith "not an F# object model type definition"

    member x.IsILTycon = match x.TypeReprInfo with | Some (TILObjModelRepr _) -> true |  _ -> false
    member x.ILTyconInfo = match x.TypeReprInfo with | Some (TILObjModelRepr (a,b,c)) -> (a,b,c) |  _ -> failwith "not a .NET type definition"
    member x.ILTyconRawMetadata = let _,_,td = x.ILTyconInfo in td

    member x.IsUnionTycon = match x.TypeReprInfo with | Some (TFiniteUnionRepr _) -> true |  _ -> false
    member x.UnionInfo = match x.TypeReprInfo with | Some (TFiniteUnionRepr x) -> Some x |  _ -> None

    member x.IsRecordTycon = match x.TypeReprInfo with | Some (TRecdRepr _) -> true |  _ -> false
    member x.IsFSharpObjectModelTycon = match x.TypeReprInfo with | Some (TFsObjModelRepr _) -> true |  _ -> false
    member x.IsAsmReprTycon = match x.TypeReprInfo with | Some (TAsmRepr _) -> true |  _ -> false
    member x.IsMeasureableReprTycon = match x.TypeReprInfo with | Some (TMeasureableRepr _) -> true |  _ -> false
    member x.IsHiddenReprTycon = match x.TypeAbbrev,x.TypeReprInfo with | None,None -> true |  _ -> false

    member x.IsFSharpInterfaceTycon = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconInterface -> true | _ -> false
    member x.IsFSharpDelegateTycon  = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconDelegate _ -> true | _ -> false
    member x.IsFSharpEnumTycon      = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconEnum -> true | _ -> false
    member x.IsFSharpClassTycon     = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconClass -> true | _ -> false
    member x.IsILEnumTycon          = x.IsILTycon && x.ILTyconRawMetadata.IsEnum
    member x.IsEnumTycon            = x.IsILEnumTycon || x.IsFSharpEnumTycon


    member x.IsFSharpStructOrEnumTycon =
        x.IsFSharpObjectModelTycon &&
        match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with 
        | TTyconClass | TTyconInterface   | TTyconDelegate _ -> false
        | TTyconStruct | TTyconEnum -> true

    member x.IsILStructTycon =
        x.IsILTycon && 
        match x.ILTyconRawMetadata.tdKind with
        | ILTypeDefKind.ValueType | ILTypeDefKind.Enum -> true
        | _ -> false

    member x.IsStructOrEnumTycon = 
        x.IsILStructTycon || x.IsFSharpStructOrEnumTycon

    member x.InterfacesOfFSharpTycon =
        x.TypeContents.tcaug_interfaces

    member x.InterfaceTypesOfFSharpTycon =
        x.InterfacesOfFSharpTycon |> List.map (fun (x,_,_) -> x)

    /// Note: result is alphabetically sorted, then for each name the results are in declaration order
    member x.MembersOfFSharpTyconSorted =
        x.TypeContents.tcaug_adhoc 
        |> NameMultiMap.rangeReversingEachBucket 
        |> List.filter (fun v -> not v.IsCompilerGenerated)

    /// Note: result is a indexed table, and for each name the results are in reverse declaration order
    member x.MembersOfFSharpTyconByName =
        x.TypeContents.tcaug_adhoc 

    member x.GeneratedHashAndEqualsWithComparerValues = x.TypeContents.tcaug_hash_and_equals_withc 
    member x.GeneratedCompareToWithComparerValues = x.TypeContents.tcaug_compare_withc
    member x.GeneratedCompareToValues = x.TypeContents.tcaug_compare
    member x.GeneratedHashAndEqualsValues = x.TypeContents.tcaug_equals
    member x.AllGeneratedValues = 
        [ match x.GeneratedCompareToValues with 
          | None -> ()
          | Some (v1,v2) -> yield v1; yield v2
          match x.GeneratedCompareToWithComparerValues with
          | None -> ()
          | Some v -> yield v
          match x.GeneratedHashAndEqualsValues with
          | None -> ()
          | Some (v1,v2) -> yield v1; yield v2
          match x.GeneratedHashAndEqualsWithComparerValues with
          | None -> ()
          | Some (v1,v2,v3) -> yield v1; yield v2; yield v3 ]
    

    /// From TAST TyconRef to IL ILTypeRef
    member x.CompiledRepresentation =
            let ilTypeRefForCompilationPath (CompPath(sref,p)) item = 
                let rec top racc  p = 
                    match p with 
                    | [] -> ILTypeRef.Create(sref,[],textOfPath  (List.rev (item::racc)))
                    | (h,istype)::t -> 
                        match istype with 
                        | FSharpModuleWithSuffix | FSharpModule -> 
                            let outerTypeName = (textOfPath (List.rev (h::racc)))
                            ILTypeRef.Create(sref, (outerTypeName :: List.map (fun (nm,_) -> nm) t),item)
                        | _ -> 
                          top (h::racc) t
                top [] p 


            cached x.CompiledReprCache (fun () -> 
                match x.ExceptionInfo with 
                | TExnAbbrevRepr ecref2 -> ecref2.CompiledRepresentation
                | TExnAsmRepr tref -> TyrepNamed(tref, AsObject, Some (mkILTy AsObject (mkILTySpec (tref,[]))))
                | _ -> 
                match x.TypeReprInfo with 
                | Some (TAsmRepr typ) -> TyrepOpen typ
                | _ -> 
                    let boxity = if x.IsStructOrEnumTycon then AsValue else AsObject
                    let ilTypeRef = ilTypeRefForCompilationPath x.CompilationPath x.CompiledName
                    let ilTypeOpt = 
                        match x.TyparsNoRange with 
                        | [] -> Some (mkILTy boxity (mkILTySpec (ilTypeRef,[]))) 
                        | _ -> None 
                    TyrepNamed (ilTypeRef, boxity, ilTypeOpt))

    member x.CompiledRepresentationForNamedType =
        match x.CompiledRepresentation with 
        | TyrepNamed(tref, _, _) -> tref
        | TyrepOpen _ -> invalidOp (FSComp.SR.tastTypeHasAssemblyCodeRepresentation(x.DisplayNameWithUnderscoreTypars))


    member x.PreEstablishedHasDefaultConstructor = x.Data.entity_flags.PreEstablishedHasDefaultConstructor
    member x.HasSelfReferentialConstructor = x.Data.entity_flags.HasSelfReferentialConstructor
    member x.SetAttribs attribs = x.Data.entity_attribs <- attribs


and 
    [<NoEquality; NoComparison>]
    EntityData =
    { /// The declared type parameters of the type  
      // MUTABILITY; used only during creation and remapping  of tycons 
      mutable entity_typars: LazyWithContext<Typars, range>        

      // MUTABILITY; used only when establishing tycons. 
      mutable entity_kind : TyparKind
      
      mutable entity_flags : EntityFlags
      
      /// The unique stamp of the "tycon blob". Note the same tycon in signature and implementation get different stamps 
      entity_stamp: Stamp

      /// The name of the type, possibly with `n mangling 
      entity_logical_name: string

      /// The name of the type, possibly with `n mangling 
      // MUTABILITY; used only when establishing tycons. 
      mutable entity_compiled_name: string option

      /// The declaration location for the type constructor 
      entity_range: range
      
      /// The declared accessibility of the representation, not taking signatures into account 
      entity_tycon_repr_accessibility: Accessibility
      
      /// The declared attributes for the type 
      // MUTABILITY; used during creation and remapping of tycons 
      // MUTABILITY; used when propagating signature attributes into the implementation.
      mutable entity_attribs: Attribs     
                
      /// The declared representation of the type, i.e. record, union, class etc. 
      //
      // The 'None' value here has two meanings
      //     - it indicates 'not yet known' during the first 2 phases of establishing type definitions
      //     - it indicated 'no representation' at all other times, i.e. 
      //           type X
      //       in signatures 
      //   It would be better to separate these two cases out, by just adding two cases
      //   to TyconRepresentation and removing the use of 'option'
      //
      // MUTABILITY; used only during creation and remapping of tycons 
      mutable entity_tycon_repr: TyconRepresentation option   

      /// If non-None, indicates the type is an abbreviation for another type. 
      //
      // MUTABILITY; used only during creation and remapping of tycons 
      mutable entity_tycon_abbrev: TType option             
      
      /// The methods and properties of the type 
      //
      // MUTABILITY; used only during creation and remapping of tycons 
      mutable entity_tycon_tcaug: TyconAugmentation      
      
      /// Field used when the 'tycon' is really an exception definition
      // 
      // MUTABILITY; used only during creation and remapping of tycons 
      mutable entity_exn_info: ExceptionInfo     
      
      /// This field is used when the 'tycon' is really a module definition. It holds statically nested type definitions and nested modules 
      //
      // MUTABILITY: only used during creation and remapping  of tycons and 
      // when compiling fslib to fixup compiler forward references to internal items 
      mutable entity_modul_contents: Lazy<ModuleOrNamespaceType>     

      /// The declared documentation for the type or module 
      entity_xmldoc : XmlDoc
      
      /// The XML document signature for this entity
      mutable entity_xmldocsig : string

      /// The stable path to the type, e.g. Microsoft.FSharp.Core.FSharpFunc`2 
      entity_pubpath : PublicPath option 

      /// Indicates how visible is the entitiy is.
      entity_accessiblity: Accessibility   
 
      /// The stable path to the type, e.g. Microsoft.FSharp.Core.FSharpFunc`2 
      entity_cpath : CompilationPath option 

      /// Used during codegen to hold the ILX representation indicating how to access the type 
      entity_il_repr_cache : CompiledTypeRepr cache  
    }

and ParentRef = 
    | Parent of TyconRef
    | ParentNone
    
and 
    [<NoEquality; NoComparison>]
    TyconAugmentation = 
    { /// This is the value implementing the auto-generated comparison 
      /// semantics if any. It is not present if the type defines its own implementation 
      /// of IComparable or if the type doesn't implement IComparable implicitly. 
      mutable tcaug_compare        : (ValRef * ValRef) option
      
      /// This is the value implementing the auto-generated comparison
      /// semantics if any. It is not present if the type defines its own implementation
      /// of IStructuralComparable or if the type doesn't implement IComparable implicitly.
      mutable tcaug_compare_withc : ValRef option                      

      /// This is the value implementing the auto-generated equality 
      /// semantics if any. It is not present if the type defines its own implementation 
      /// of Object.Equals or if the type doesn't override Object.Equals implicitly. 
      mutable tcaug_equals        : (ValRef * ValRef) option

      /// This is the value implementing the auto-generated comparison
      /// semantics if any. It is not present if the type defines its own implementation
      /// of IStructuralEquatable or if the type doesn't implement IComparable implicitly.
      mutable tcaug_hash_and_equals_withc : (ValRef * ValRef * ValRef) option                                    

      /// True if the type defined an Object.GetHashCode method. In this 
      /// case we give a warning if we auto-generate a hash method since the semantics may not match up
      mutable tcaug_hasObjectGetHashCode : bool             
      
      /// Properties, methods etc. in declaration order. The boolean flag for each indicates if the
      /// member is known to be an explicit interface implementation. This must be computed and
      /// saved prior to remapping assembly information.
      tcaug_adhoc_list     : ResizeArray<bool * ValRef> 
      
      /// Properties, methods etc. as lookup table
      mutable tcaug_adhoc          : NameMultiMap<ValRef>
      
      /// Interface implementations - boolean indicates compiler-generated 
      mutable tcaug_interfaces     : (TType * bool * range) list  
      
      /// Super type, if any 
      mutable tcaug_super          : TType option                 
      
      /// Set to true at the end of the scope where proper augmentations are allowed 
      mutable tcaug_closed         : bool                       

      /// Set to true if the type is determined to be abstract 
      mutable tcaug_abstract : bool                       
    }

    member tcaug.SetCompare              x = tcaug.tcaug_compare         <- Some x
    member tcaug.SetCompareWith          x = tcaug.tcaug_compare_withc   <- Some x
    member tcaug.SetEquals               x = tcaug.tcaug_equals <- Some x
    member tcaug.SetHashAndEqualsWith    x = tcaug.tcaug_hash_and_equals_withc <- Some x
    member tcaug.SetHasObjectGetHashCode b = tcaug.tcaug_hasObjectGetHashCode <- b

    static member Create() =
        { tcaug_compare=None 
          tcaug_compare_withc=None 
          tcaug_equals=None 
          tcaug_hash_and_equals_withc=None 
          tcaug_hasObjectGetHashCode=false 
          tcaug_adhoc=NameMultiMap.empty 
          tcaug_adhoc_list=new ResizeArray<_>() 
          tcaug_super=None
          tcaug_interfaces=[] 
          tcaug_closed=false 
          tcaug_abstract=false }
and 
    [<NoEquality; NoComparison>]
    TyconRepresentation = 

    /// Indicates the type is a class, struct, enum, delegate or interface 
    | TFsObjModelRepr    of TyconObjModelData

    /// Indicates the type is a record 
    | TRecdRepr          of TyconRecdFields

    /// Indicates the type is a discriminated union 
    | TFiniteUnionRepr   of TyconUnionData 

    /// TILObjModelRepr(scope, nesting, definition)
    ///
    /// Indicates the type is a type from a .NET assembly without F# metadata.
    | TILObjModelRepr    of ILScopeRef * ILTypeDef list * ILTypeDef 

    /// Indicates the type is implemented as IL assembly code using the given closed Abstract IL type 
    | TAsmRepr           of ILType

    /// Indicates the type is parameterized on a measure (e.g. float<_>) but erases to some other type (e.g. float)
    | TMeasureableRepr   of TType


and 
  TyconObjModelKind = 
    /// Indicates the type is a class (also used for units-of-measure)
    | TTyconClass 
    /// Indicates the type is an interface 
    | TTyconInterface 
    /// Indicates the type is a struct 
    | TTyconStruct 
    /// Indicates the type is a delegate with the given Invoke signature 
    | TTyconDelegate of SlotSig 
    /// Indicates the type is an enumeration 
    | TTyconEnum
    
and 
    [<NoEquality; NoComparison>]
    TyconObjModelData = 
    { /// Indicates whether the type declaration is a class, interface, enum, delegate or struct 
      fsobjmodel_kind: TyconObjModelKind;
      /// The declared abstract slots of the class, interface or struct 
      fsobjmodel_vslots: ValRef list; 
      /// The fields of the class, struct or enum 
      fsobjmodel_rfields: TyconRecdFields }

and 
    [<NoEquality; NoComparison>]
    TyconRecdFields = 
    { /// The fields of the record, in declaration order. 
      FieldsByIndex: RecdField[];
      
      /// The fields of the record, indexed by name. 
      FieldsByName : NameMap<RecdField> }

    member x.FieldByIndex n = 
        if n >= 0 && n < x.FieldsByIndex.Length then x.FieldsByIndex.[n] 
        else failwith "FieldByIndex"

    member x.FieldByName n = x.FieldsByName.TryFind(n)
    member x.AllFieldsAsList = x.FieldsByIndex |> Array.toList
    member x.TrueFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsCompilerGenerated)   
    member x.TrueInstanceFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsStatic && not f.IsCompilerGenerated)   

and 
    [<NoEquality; NoComparison>]
    TyconUnionCases = 
    { /// The cases of the discriminated union, in declaration order. 
      CasesByIndex: UnionCase[];
      /// The cases of the discriminated union, indexed by name. 
      CasesByName : NameMap<UnionCase>
    }
    member x.GetUnionCaseByIndex n = 
        if n >= 0 && n < x.CasesByIndex.Length then x.CasesByIndex.[n] 
        else invalidArg "n" "GetUnionCaseByIndex"

    member x.UnionCasesAsList = x.CasesByIndex |> Array.toList

and 
    [<NoEquality; NoComparison>]
    TyconUnionData =
    { /// The cases contained in the discriminated union. 
      CasesTable: TyconUnionCases;
      /// The ILX data structure representing the discriminated union. 
      CompiledRepresentation: IlxUnionRef cache; 
    }
    member x.UnionCasesAsList = x.CasesTable.CasesByIndex |> Array.toList

and 
    [<NoEquality; NoComparison>]
    [<StructuredFormatDisplay("{DisplayName}")>]
    UnionCase =
    { /// Data carried by the case. 
      FieldTable: TyconRecdFields;
      /// Return type constructed by the case. Normally exactly the type of the enclosing type, sometimes an abbreviation of it 
      ReturnType: TType;
      /// Name of the case in generated IL code 
      CompiledName: string;
      /// Documentation for the case 
      XmlDoc : XmlDoc;
      /// XML documentation signature for the case
      mutable XmlDocSig : string;
      /// Name/range of the case 
      Id: Ident; 
      ///  Indicates the declared visibility of the union constructor, not taking signatures into account 
      Accessibility: Accessibility; 
      /// Attributes, attached to the generated static method to make instances of the case 
      // MUTABILITY: used when propagating signature attributes into the implementation.
      mutable Attribs: Attribs; }

    member uc.Range = uc.Id.idRange
    member uc.DisplayName = uc.Id.idText
    member uc.RecdFieldsArray = uc.FieldTable.FieldsByIndex 
    member uc.RecdFields = uc.FieldTable.FieldsByIndex |> Array.toList
    member uc.GetFieldByName nm = uc.FieldTable.FieldByName nm
    member uc.IsNullary = (uc.FieldTable.FieldsByIndex.Length = 0)

and 
    /// This may represent a "field" in either a struct, class, record or union
    /// It is normally compiled to a property.
    [<NoEquality; NoComparison>]
    RecdField =
    { /// Is the field declared mutable in F#? 
      rfield_mutable: bool;
      /// Documentation for the field 
      rfield_xmldoc : XmlDoc;
      /// XML Documentation signature for the field
      mutable rfield_xmldocsig : string;
      /// The type of the field, w.r.t. the generic parameters of the enclosing type constructor 
      rfield_type: TType;
      /// Indicates a static field 
      rfield_static: bool;
      /// Indicates a volatile field 
      rfield_volatile: bool;
      /// Indicates a compiler generated field, not visible to Intellisense or name resolution 
      rfield_secret: bool;
      /// The default initialization info, for static literals 
      rfield_const: Const option; 
      ///  Indicates the declared visibility of the field, not taking signatures into account 
      rfield_access: Accessibility; 
      /// Attributes attached to generated property 
      // MUTABILITY: used when propagating signature attributes into the implementation.
      mutable rfield_pattribs: Attribs; 
      /// Attributes attached to generated field 
      // MUTABILITY: used when propagating signature attributes into the implementation.
      mutable rfield_fattribs: Attribs; 
      /// Name/declaration-location of the field 
      rfield_id: Ident }
    member v.Accessibility = v.rfield_access
    member v.PropertyAttribs = v.rfield_pattribs
    member v.FieldAttribs = v.rfield_fattribs
    member v.Range = v.rfield_id.idRange
    member v.Id = v.rfield_id
    member v.Name = v.rfield_id.idText
    member v.IsCompilerGenerated = v.rfield_secret
    member v.IsMutable = v.rfield_mutable
    member v.IsStatic = v.rfield_static
    member v.IsVolatile = v.rfield_volatile
    member v.FormalType = v.rfield_type
    member v.XmlDoc = v.rfield_xmldoc
    member v.XmlDocSig
        with get() = v.rfield_xmldocsig
        and set(x) = v.rfield_xmldocsig <- x

    member v.LiteralValue = 
        match v.rfield_const  with 
        | None -> None
        | Some Const.Zero -> None
        | Some k -> Some k

    member v.IsZeroInit = 
        match v.rfield_const  with 
        | None -> false 
        | Some Const.Zero -> true 
        | _ -> false

and ExceptionInfo =
    /// Indicates that an exception is an abbreviation for the given exception 
    | TExnAbbrevRepr of TyconRef 
    /// Indicates that an exception is shorthand for the given .NET exception type 
    | TExnAsmRepr of ILTypeRef
    /// Indicates that an exception carries the given record of values 
    | TExnFresh of TyconRecdFields
    /// Indicates that an exception is abstract, i.e. is in a signature file, and we do not know the representation 
    | TExnNone

and 
    [<Sealed>]
    ModuleOrNamespaceType(kind: ModuleOrNamespaceKind, vals: QueueList<Val>, entities: QueueList<Entity>) = 

      /// Mutation used during compilation of FSharp.Core.dll
      let mutable entities = entities 
      
      // Lookup tables keyed the way various clients expect them to be keyed.
      // We attach them here so we don't need to store lookup tables via any other technique.
      //
      // The type option ref is used because there are a few functions that treat these as first class values.
      // We should probably change to 'mutable'.
      //
      // We do not need to lock this mutable state this it is only ever accessed from the compiler thread.
      let activePatternElemRefCache                        : NameMap<ActivePatternElemRef> option ref = ref None
      let modulesByDemangledNameCache       : NameMap<ModuleOrNamespace>    option ref = ref None
      let exconsByDemangledNameCache        : NameMap<Tycon>                option ref = ref None
      let tyconsByDemangledNameAndArityCache: Map<NameArityPair, Tycon>     option ref = ref None
      let tyconsByAccessNamesCache          : NameMultiMap<Tycon>           option ref = ref None
      let tyconsByMangledNameCache          : NameMap<Tycon>                option ref = ref None
      let allEntitiesByMangledNameCache     : NameMap<Entity>               option ref = ref None
      let allValsAndMembersByPartialLinkageKeyCache : MultiMap<ValLinkagePartialKey, Val>    option ref = ref None
      let allValsByLogicalNameCache         : NameMap<Val>               option ref = ref None
  
      /// Namespace or module-compiled-as-type? 
      member mtyp.ModuleOrNamespaceKind = kind 
              
      /// Values, including members in F# types in this module-or-namespace-fragment. 
      member mtyp.AllValsAndMembers = vals

      /// Type, mapping mangled name to Tycon, e.g. 
      ////     "Dictionary`2" --> Tycon 
      ////     "ListModule" --> Tycon with module info
      ////     "FooException" --> Tycon with exception info
      member mtyp.AllEntities = entities

      /// Mutation used during compilation of FSharp.Core.dll
      member mtyp.AddModuleOrNamespaceByMutation(modul:ModuleOrNamespace) =
          entities <- QueueList.appendOne entities modul
          modulesByDemangledNameCache := None          
          allEntitiesByMangledNameCache := None       

      member mtyp.AddEntity(tycon:Tycon) = 
          ModuleOrNamespaceType(kind, vals, entities.AppendOne tycon)
          
      member mtyp.AddVal(vspec:Val) = 
          ModuleOrNamespaceType(kind, vals.AppendOne vspec, entities)
          
      member mtyp.ActivePatternElemRefLookupTable = activePatternElemRefCache
  
      member mtyp.TypeDefinitions               = entities |> Seq.filter (fun x -> not x.IsExceptionDecl && not x.IsModuleOrNamespace) |> Seq.toList
      member mtyp.ExceptionDefinitions          = entities |> Seq.filter (fun x -> x.IsExceptionDecl) |> Seq.toList
      member mtyp.ModuleAndNamespaceDefinitions = entities |> Seq.filter (fun x -> x.IsModuleOrNamespace) |> Seq.toList
      member mtyp.TypeAndExceptionDefinitions   = entities |> Seq.filter (fun x -> not x.IsModuleOrNamespace) |> Seq.toList

      member mtyp.TypesByDemangledNameAndArity(m) = 
        cacheOptRef tyconsByDemangledNameAndArityCache (fun () -> 
           List.foldBack (fun (tc:Tycon) acc -> AddTyconsByDemangledNameAndArity tc.LogicalName (tc.Typars(m)) tc acc) mtyp.TypeAndExceptionDefinitions  Map.empty)

      member mtyp.TypesByAccessNames = 
          cacheOptRef tyconsByAccessNamesCache (fun () -> 
             List.foldBack (fun (tc:Tycon) acc -> AddTyconsByAccessNames tc.LogicalName tc acc) mtyp.TypeAndExceptionDefinitions  Map.empty)

      member mtyp.TypesByMangledName = 
          let addTyconByMangledName (x:Tycon) tab = NameMap.add x.LogicalName x tab 
          cacheOptRef tyconsByMangledNameCache (fun () -> 
             List.foldBack addTyconByMangledName mtyp.TypeAndExceptionDefinitions  Map.empty)

      member mtyp.AllEntitiesByCompiledAndLogicalMangledNames : NameMap<Entity> = 
          let addEntityByMangledName (x:Entity) tab = 
              let name1 = x.LogicalName
              let name2 = x.CompiledName
              let tab = NameMap.add name1 x tab 
              if name1 = name2 then tab
              else NameMap.add name2 x tab 
          
          cacheOptRef allEntitiesByMangledNameCache (fun () -> 
             QueueList.foldBack addEntityByMangledName entities  Map.empty)

      member mtyp.AllEntitiesByLogicalMangledName : NameMap<Entity> = 
          let addEntityByMangledName (x:Entity) tab = NameMap.add x.LogicalName x tab 
          QueueList.foldBack addEntityByMangledName entities  Map.empty

      member mtyp.AllValsAndMembersByPartialLinkageKey = 
          let addValByMangledName (x:Val) tab = 
             if x.IsCompiledAsTopLevel then
                 MultiMap.add x.LinkagePartialKey x tab 
             else
                 tab
          cacheOptRef allValsAndMembersByPartialLinkageKeyCache (fun () -> 
             QueueList.foldBack addValByMangledName vals MultiMap.empty)

      member mtyp.TryLinkVal(ccu:CcuThunk,key:ValLinkageFullKey) = 
          mtyp.AllValsAndMembersByPartialLinkageKey
            |> MultiMap.find key.PartialKey
            |> List.tryFind (fun v -> match key.TypeForLinkage with 
                                      | None -> true
                                      | Some keyTy -> ccu.MemberSignatureEquality(keyTy,v.Type))

      member mtyp.AllValsByLogicalName = 
          let addValByName (x:Val) tab = 
             // Note: names may occur twice prior to raising errors about this in PostTypecheckSemanticChecks
             // Earlier ones take precedence sice we report errors about the later ones
             if not x.IsMember && not x.IsCompilerGenerated then 
                 NameMap.add x.LogicalName x tab 
             else
                 tab
          cacheOptRef allValsByLogicalNameCache (fun () -> 
             QueueList.foldBack addValByName vals Map.empty)

      member mtyp.AllValsAndMembersByLogicalNameUncached = 
          let addValByName (x:Val) tab = 
             if not x.IsCompilerGenerated then 
                 MultiMap.add x.LogicalName x tab 
             else
                 tab
          QueueList.foldBack addValByName vals MultiMap.empty

      member mtyp.ExceptionDefinitionsByDemangledName = 
          let add (tycon:Tycon) acc = NameMap.add tycon.LogicalName tycon acc
          cacheOptRef exconsByDemangledNameCache (fun () -> 
             List.foldBack add mtyp.ExceptionDefinitions  Map.empty)

      member mtyp.ModulesAndNamespacesByDemangledName = 
          let add (entity:Entity) acc = 
              if entity.IsModuleOrNamespace then 
                  NameMap.add entity.DemangledModuleOrNamespaceName entity acc
              else acc
          cacheOptRef modulesByDemangledNameCache (fun () -> 
             QueueList.foldBack add entities  Map.empty)

and ModuleOrNamespace = Entity 
and Tycon = Entity 
and Construct = 
      
    static member NewModuleOrNamespaceType mkind tycons vals = ModuleOrNamespaceType(mkind, QueueList.ofList vals, QueueList.ofList tycons)
    static member NewEmptyModuleOrNamespaceType mkind = Construct.NewModuleOrNamespaceType mkind [] []

    static member NewModuleOrNamespace cpath access (id:Ident) xml attribs mtype = 
        let stamp = newStamp() 
        // Put the module suffix on if needed 
        Tycon.New "mspec"
          { entity_logical_name=id.idText
            entity_compiled_name=None
            entity_range = id.idRange
            entity_stamp=stamp
            entity_kind=KindType
            entity_modul_contents = mtype
            entity_flags=EntityFlags(usesPrefixDisplay=false, isModuleOrNamespace=true, preEstablishedHasDefaultCtor=false, hasSelfReferentialCtor=false)
            entity_typars=LazyWithContext<_,_>.NotLazy []
            entity_tycon_abbrev = None
            entity_tycon_repr = None
            entity_tycon_repr_accessibility = access
            entity_exn_info=TExnNone
            entity_tycon_tcaug=TyconAugmentation.Create()
            entity_pubpath=cpath |> Option.map (publicPathOfCompPath id)
            entity_cpath=cpath
            entity_accessiblity=access
            entity_attribs=attribs
            entity_xmldoc=xml
            entity_xmldocsig=""        
            entity_il_repr_cache = newCache() 
            } 
and Accessibility = 
    /// Indicates the construct can only be accessed from any code in the given type constructor, module or assembly. [] indicates global scope. 
    | TAccess of CompilationPath list
    
and 
    [<NoEquality; NoComparison>]
    TyparData = 
    { /// MUTABILITY: we set the names of generalized inference type parameters to make the look nice for IL code generation 
      mutable typar_id: Ident 
       
      /// MUTABILITY: we set the names of generalized inference type parameters to make the look nice for IL code generation 
      mutable typar_il_name: string option 
       
      mutable typar_flags: TyparFlags
       
       /// The unique stamp of the typar blob. 
      typar_stamp: Stamp 
       
       /// The documentation for the type parameter. Empty for type inference variables.
      typar_xmldoc : XmlDoc
       
       /// The declared attributes of the type parameter. Empty for type inference variables. 
      mutable typar_attribs: Attribs                      
       
       /// An inferred equivalence for a type inference variable. 
      mutable typar_solution: TType option
       
       /// The inferred constraints for the type inference variable 
      mutable typar_constraints: TyparConstraint list 
    } 

and 
    [<NoEquality; NoComparison>]
    [<StructuredFormatDisplay("{Name}")>]
    Typar = 
    { mutable Data: TyparData
      mutable AsType: TType }
    member x.Name                = x.Data.typar_id.idText
    member x.Range               = x.Data.typar_id.idRange
    member x.Id                  = x.Data.typar_id
    member x.Stamp               = x.Data.typar_stamp
    member x.Solution            = x.Data.typar_solution
    member x.Constraints         = x.Data.typar_constraints
    member x.IsCompilerGenerated = x.Data.typar_flags.IsCompilerGenerated
    member x.Rigidity            = x.Data.typar_flags.Rigidity
    member x.DynamicReq          = x.Data.typar_flags.DynamicReq
    member x.EqualityConditionalOn = x.Data.typar_flags.EqualityConditionalOn
    member x.ComparisonConditionalOn = x.Data.typar_flags.ComparisonConditionalOn
    member x.StaticReq           = x.Data.typar_flags.StaticReq
    member x.IsFromError         = x.Data.typar_flags.IsFromError
    member x.Kind                = x.Data.typar_flags.Kind
    member x.IsErased            = match x.Kind with KindType -> false | _ -> true
    member x.Attribs             = x.Data.typar_attribs
    member x.DisplayName = let nm = x.Name in if nm = "?" then "?"+string x.Stamp else nm

    member tp.FixupConstraints cs =
        tp.Data.typar_constraints <-  cs


    // OSGN support
    static member NewUnlinked() : Typar  = 
        let res = { Data = nullableSlotEmpty(); AsType=Unchecked.defaultof<_> }
        res.AsType <- TType_var res
        res
    static member New data : Typar = 
        let res = { Data = data; AsType=Unchecked.defaultof<_> }
        res.AsType <- TType_var res
        res
    member x.Link tg = x.Data <- nullableSlotFull(tg)
    member x.IsLinked = match box x.Data with null -> false | _ -> true 

    member x.IsSolved = 
        match x.Solution with 
        | None -> false
        | _ -> true

    member x.SetIdent id = x.Data.typar_id <- id

    member x.SetRigidity b            = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, b,              flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, flags.DynamicReq, flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    member x.SetCompilerGenerated b   = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, b,                         flags.StaticReq, flags.DynamicReq, flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    member x.SetStaticReq b           = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, b,               flags.DynamicReq, flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    member x.SetDynamicReq b          = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, b               , flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    member x.SetEqualityDependsOn b   = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, flags.DynamicReq, b                          , flags.ComparisonConditionalOn) 
    member x.SetComparisonDependsOn b = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, flags.DynamicReq, flags.EqualityConditionalOn, b) 

    override x.ToString() = x.Name

and
    [<NoEquality; NoComparison>]
    TyparConstraint = 
    /// Indicates a constraint that a type is a subtype of the given type 
    | TTyparCoercesToType              of TType * range

    /// Indicates a default value for an inference type variable should it be netiher generalized nor solved 
    | TTyparDefaultsToType             of int * TType * range 
    
    /// Indicates a constraint that a type has a 'null' value 
    | TTyparSupportsNull               of range 
    
    /// Indicates a constraint that a type has a member with the given signature 
    | TTyparMayResolveMemberConstraint of TraitConstraintInfo * range 
    
    /// Indicates a constraint that a type is a non-Nullable value type 
    /// These are part of .NET's model of generic constraints, and in order to 
    /// generate verifiable code we must attach them to F# generalzied type variables as well. 
    | TTyparIsNotNullableValueType     of range 
    
    /// Indicates a constraint that a type is a reference type 
    | TTyparIsReferenceType            of range 

    /// Indicates a constraint that a type is a simple choice between one of the given ground types. See format.fs 
    | TTyparSimpleChoice               of TTypes * range 

    /// Indicates a constraint that a type has a parameterless constructor 
    | TTyparRequiresDefaultConstructor of range 

    /// Indicates a constraint that a type is an enum with the given underlying 
    | TTyparIsEnum                     of TType * range 
    
    /// Indicates a constraint that a type implements IComparable, with special rules for some known structural container types
    | TTyparSupportsComparison               of range 
    
    /// Indicates a constraint that a type does not have the Equality(false) attribute, or is not a structural type with this attribute, with special rules for some known structural container types
    | TTyparSupportsEquality                of range 
    
    /// Indicates a constraint that a type is a delegate from the given tuple of args to the given return type
    | TTyparIsDelegate                 of TType * TType * range 
    
    /// Indicates a constraint that a type is .NET unmanaged type
    | TTyparIsUnmanaged                 of range
    
/// The specification of a member constraint that must be solved 
and 
    [<NoEquality; NoComparison>]
    TraitConstraintInfo = 

    /// TTrait(tys,nm,memFlags,argtys,rty,colution)
    ///
    /// Indicates the signature of a member constraint. Contains a mutable solution cell
    /// to store the inferred solution of the constraint.
    | TTrait of TTypes * string * MemberFlags * TTypes * TType option * TraitConstraintSln option ref 

    member x.MemberName = (let (TTrait(_,nm,_,_,_,_)) = x in nm)
    member x.ReturnType = (let (TTrait(_,_,_,_,ty,_)) = x in ty)
    member x.Solution 
        with get() = (let (TTrait(_,_,_,_,_,sln)) = x in sln.Value)
        and set(v) = (let (TTrait(_,_,_,_,_,sln)) = x in sln.Value <- v)
    
and 
    [<NoEquality; NoComparison>]
    TraitConstraintSln = 
    /// FSMethSln(typ, vref, minst)
    ///
    /// Indicates a trait is solved by an F# method.
    ///    typ   -- the type and its instantiation
    ///    vref  -- the method that solves the trait constraint
    ///    minst -- the generic method instantiation 
    | FSMethSln of TType * ValRef * TypeInst 

    /// ILMethSln(typ, extOpt, ilMethodRef, minst)
    ///
    /// Indicates a trait is solved by a .NET method.
    ///    typ         -- the type and its instantiation
    ///    extOpt      -- information about an extension member, if any
    ///    ilMethodRef -- the method that solves the trait constraint
    ///    minst       -- the generic method instantiation 
    | ILMethSln of TType * ILTypeRef option * ILMethodRef * TypeInst    

    | BuiltInSln

/// The partial information used to index the methods of all the types
/// in a ModuleOrNamespace.
and ValLinkagePartialKey = 
   { MemberParentMangledName : string option 
     MemberIsOverride: bool 
     LogicalName: string 
     TotalArgCount: int } 

/// The full information used to identify a specific overloaded method
/// amongst all the types in a ModuleOrNamespace.
and ValLinkageFullKey(partialKey: ValLinkagePartialKey,  typeForLinkage:TType option) =
   member x.PartialKey = partialKey
   member x.TypeForLinkage = typeForLinkage


and 
    [<StructuredFormatDisplay("{LogicalName}")>]
    Val = 
    { mutable Data: ValData }
    /// The place where the value was defined. 
    member x.Range = x.Data.val_range
    /// A unique stamp within the context of this invocation of the compiler process 
    member x.Stamp = x.Data.val_stamp
    /// The type of the value. 
    /// May be a TType_forall for a generic value. 
    /// May be a type variable or type containing type variables during type inference. 

    // Mutability used in inference by adjustAllUsesOfRecValue.  
    // This replaces the recursively inferred type with a schema. 
    // MUTABILITY CLEANUP: find a way to do this using type unification alone. 
    member x.Type                       = x.Data.val_type
    member x.Accessibility              = x.Data.val_access
    /// Range of the definition (implementation) of the value, used by Visual Studio 
    /// Updated by mutation when the implementation is matched against the signature. 
    member x.DefinitionRange            = x.Data.val_defn_range
    /// The value of a value or member marked with [<LiteralAttribute>] 
    member x.LiteralValue               = x.Data.val_const
    member x.ValReprInfo : ValReprInfo option = x.Data.val_repr_info
    member x.Id                         = ident(x.LogicalName,x.Range)
    /// Is this represented as a "top level" static binding (i.e. a static field, static member,
    /// instance member), rather than an "inner" binding that may result in a closure.
    ///
    /// This is implied by IsMemberOrModuleBinding, however not vice versa, for two reasons.
    /// Some optimizations mutate this value when they decide to change the representation of a 
    /// binding to be IsCompiledAsTopLevel. Second, even immediately after type checking we expect
    /// some non-module, non-member bindings to be marked IsCompiledAsTopLevel, e.g. 'y' in 
    /// 'let x = let y = 1 in y + y' (NOTE: check this, don't take it as gospel)
    member x.IsCompiledAsTopLevel       = x.ValReprInfo.IsSome 


    member x.LinkagePartialKey : ValLinkagePartialKey = 
        assert x.IsCompiledAsTopLevel
        { LogicalName = x.LogicalName 
          MemberParentMangledName = (if x.IsMember then Some x.MemberApparentParent.LogicalName else None)
          MemberIsOverride = x.IsOverrideOrExplicitImpl
          TotalArgCount = if x.IsMember then List.sum x.ValReprInfo.Value.AritiesOfArgs else 0 }

    member x.LinkageFullKey : ValLinkageFullKey = 
        assert x.IsCompiledAsTopLevel
        ValLinkageFullKey(x.LinkagePartialKey, (if x.IsMember then Some x.Type else None))


    /// Is this a member definition or module definition?
    member x.IsMemberOrModuleBinding    = x.Data.val_flags.IsMemberOrModuleBinding
    member x.IsExtensionMember          = x.Data.val_flags.IsExtensionMember

    member x.ReflectedDefinition        = x.Data.val_defn

    /// Is this a member, if so some more data about the member.
    ///
    /// Note, the value may still be (a) an extension member or (b) and abtract slot without
    /// a true body. These cases are often causes of bugs in the compiler.
    member x.MemberInfo                 = x.Data.val_member_info

    /// Is this a member, if so some more data about the member.
    ///
    /// Note, the value may still be (a) an extension member or (b) and abtract slot without
    /// a true body. These cases are often causes of bugs in the compiler.
    member x.IsMember                   = x.MemberInfo.IsSome

    /// Is this a member, excluding extension members
    member x.IsIntrinsicMember          = x.IsMember && not x.IsExtensionMember

    /// Is this a value in a module, or an extension member, but excluding compiler generated bindings from optimizations
    member x.IsModuleBinding            = x.IsMemberOrModuleBinding && not x.IsMember 

    /// Is this something compiled into a module, i.e. a user-defined value, an extension member or a compiler-generated value
    member x.IsCompiledIntoModule       = x.IsExtensionMember || x.IsModuleBinding

    /// Is this an instance member. 
    ///
    /// Note, the value may still be (a) an extension member or (b) and abtract slot without
    /// a true body. These cases are often causes of bugs in the compiler.
    member x.IsInstanceMember = x.IsMember && x.MemberInfo.Value.MemberFlags.IsInstance

    /// Is this a 'new' constructor member
    member x.IsConstructor              =
        match x.MemberInfo with 
        | Some(memberInfo) when not x.IsExtensionMember && (memberInfo.MemberFlags.MemberKind = MemberKind.Constructor) -> true
        | _ -> false

    /// Is this a compiler-generated class constructor member
    member x.IsClassConstructor              =
        match x.MemberInfo with 
        | Some(memberInfo) when not x.IsExtensionMember && (memberInfo.MemberFlags.MemberKind = MemberKind.ClassConstructor) -> true
        | _ -> false

    /// Was this member declared 'override' or is it an implementation of an interface slot
    member x.IsOverrideOrExplicitImpl                 =
        match x.MemberInfo with 
        | Some(memberInfo) when memberInfo.MemberFlags.IsOverrideOrExplicitImpl -> true
        | _ -> false
            
    /// Was the value declared 'mutable'
    member x.IsMutable                  = (match x.Data.val_flags.MutabilityInfo with Immutable -> false | Mutable -> true)

    /// Was the value inferred to be a method or function that definitely makes no critical tailcalls?
    member x.MakesNoCriticalTailcalls = x.Data.val_flags.MakesNoCriticalTailcalls
    
    /// Was the value ever referenced?
    member x.HasBeenReferenced = x.Data.val_flags.HasBeenReferenced

    /// Was the value ever referenced?
    member x.IsCompiledAsStaticPropertyWithoutField = x.Data.val_flags.IsCompiledAsStaticPropertyWithoutField

    /// Does the value allow the use of an explicit type instantiation (i.e. does it itself have explciti type arguments,
    /// or does it have a signature?)
    member x.PermitsExplicitTypeInstantiation = x.Data.val_flags.PermitsExplicitTypeInstantiation

    /// Is this a member generated from the de-sugaring of 'let' function bindings in the implicit class syntax?
    member x.IsIncrClassGeneratedMember     = x.IsCompilerGenerated && x.Data.val_flags.IsIncrClassSpecialMember

    /// Is this a constructor member generated from the de-sugaring of implicit constructor for a class type?
    member x.IsIncrClassConstructor = x.IsConstructor && x.Data.val_flags.IsIncrClassSpecialMember

    /// Get the information about the value used during type inference
    member x.RecursiveValInfo           = x.Data.val_flags.RecursiveValInfo

    /// Is this a 'base' or 'this' value?
    member x.BaseOrThisInfo             = x.Data.val_flags.BaseOrThisInfo

    //  Was this value declared to be a type function, e.g. "let f<'a> = typeof<'a>"
    member x.IsTypeFunction             = x.Data.val_flags.IsTypeFunction

    /// Get the inline declaration on the value
    member x.InlineInfo                 = x.Data.val_flags.InlineInfo

    /// Does the inline declaration for the value indicate that the value must be inlined?
    member x.MustInline                 = mustinline(x.InlineInfo)

    /// Was the value generated by the compiler?
    ///
    /// Note: this is true for the overrides generated by hash/compare augmentations
    member x.IsCompilerGenerated        = x.Data.val_flags.IsCompilerGenerated
    
    /// Get the declared attributes for the value
    member x.Attribs                    = x.Data.val_attribs

    /// Get the declared documentation for the value
    member x.XmlDoc                     = x.Data.val_xmldoc
    
    ///Get the signature for the value's XML documentation
    member x.XmlDocSig 
        with get() = x.Data.val_xmldocsig
        and set(v) = x.Data.val_xmldocsig <- v

    /// The parent type or module, if any (None for expression bindings and parameters)
    member x.ActualParent               = x.Data.val_actual_parent

    /// Get the actual parent entity for the value (a module or a type), i.e. the entity under which the
    /// value will appear in compiled code. For extension members this is the module where the extension member
    /// is declared.
    member x.TopValActualParent = 
        match x.ActualParent  with 
        | Parent tcref -> tcref
        | ParentNone -> error(InternalError("TopValActualParent: does not have a parent",x.Range))
            
    /// Get the apparent parent entity for a member
    member x.MemberApparentParent : TyconRef = 
        match x.MemberInfo with 
        | Some membInfo -> membInfo.ApparentParent
        | None -> error(InternalError("MemberApparentParent",x.Range))

    member v.NumObjArgs =
        match v.MemberInfo with 
        | Some membInfo -> if membInfo.MemberFlags.IsInstance then 1 else 0
        | None -> 0

    /// Get the apparent parent entity for the value, i.e. the entity under with which the
    /// value is associated. For extension members this is the nominal type the member extends.
    /// For other values it is just the actual parent.
    member x.ApparentParent = 
        match x.MemberInfo with 
        | Some membInfo -> Parent(membInfo.ApparentParent)
        | None -> x.ActualParent

    /// Get the public path to the value, if any? Should be set if and only if
    /// IsMemberOrModuleBinding is set.
    //
    // We use it here:
    //   - in opt.fs   : when compiling fslib, we bind an entry for the value in a global table (see bind_escaping_local_vspec)
    //   - in ilxgen.fs: when compiling fslib, we bind an entry for the value in a global table (see bind_escaping_local_vspec)
    //   - in opt.fs   : (fullDisplayTextOfValRef) for error reporting of non-inlinable values
    //   - in service.fs (boutput_item_description): to display the full text of a value's binding location
    //   - in check.fs: as a boolean to detect public values for saving quotations 
    //   - in ilxgen.fs: as a boolean to detect public values for saving quotations 
    //   - in MakeExportRemapping, to build non-local references for values
    member x.PublicPath                 = 
        match x.ActualParent  with 
        | Parent eref -> 
            match eref.PublicPath with 
            | None -> None
            | Some p -> Some(ValPubPath(p,x.LinkageFullKey))
        | ParentNone -> 
            None


    member x.IsDispatchSlot = 
        match x.MemberInfo with 
        | Some(membInfo) -> membInfo.MemberFlags.IsDispatchSlot 
        | _ -> false

    /// Get the type of the value including any generic type parameters
    member x.TypeScheme = 
        match x.Type with 
        | TType_forall(tps,tau) -> tps,tau
        | ty -> [],ty

    /// Get the type of the value after removing any generic type parameters
    member x.TauType = 
        match x.Type with 
        | TType_forall(_,tau) -> tau
        | ty -> ty

    /// Get the generic type parameters for the value
    member x.Typars = 
        match x.Type with 
        | TType_forall(tps,_) -> tps
        | _ -> []

    /// The name of the method. 
    ///   - If this is a property then this is 'get_Foo' or 'set_Foo'
    ///   - If this is an implementation of an abstract slot then this is the name of the method implemented by the abstract slot
    ///   - If this is an extension member then this will be the simple name
    member x.LogicalName = 
        match x.MemberInfo with 
        | None -> x.Data.val_logical_name
        | Some membInfo -> 
            match membInfo.ImplementedSlotSigs with 
            | slotsig :: _ -> slotsig.Name
            | _ -> x.Data.val_logical_name

    /// The name of the method in compiled code (with some exceptions where ilxgen.fs decides not to use a method impl)
    ///   - If this is a property then this is 'get_Foo' or 'set_Foo'
    ///   - If this is an implementation of an abstract slot then this may be a mangled name
    ///   - If this is an extension member then this will be a mangled name
    ///   - If this is an operator then this is 'op_Addition'
    member x.CompiledName =
        let givenName = 
            match x.Data.val_compiled_name with 
            | Some n -> n
            | None -> x.LogicalName 
        // These cases must get stable unique names for their static field & static property. This name
        // must be stable across quotation generation and IL code generation (quotations can refer to the 
        // properties implicit in these)
        //
        //    Variable 'x' here, which is compiled as a top level static:
        //         do let x = expr in ...    // IsMemberOrModuleBinding = false, IsCompiledAsTopLevel = true, IsMember = false, CompilerGenerated=false
        //
        //    The implicit 'patternInput' variable here:
        //         let [x] = expr in ...    // IsMemberOrModuleBinding = true, IsCompiledAsTopLevel = true, IsMember = false, CompilerGenerated=true
        //    
        //    The implicit 'copyOfStruct' variables here:
        //         let dt = System.DateTime.Now - System.DateTime.Now // IsMemberOrModuleBinding = false, IsCompiledAsTopLevel = true, IsMember = false, CompilerGenerated=true
        //    
        // However we don't need this for CompilerGenerated members such as the imlpementations of IComparable
        if x.IsCompiledAsTopLevel  && not x.IsMember  && (x.IsCompilerGenerated || not x.IsMemberOrModuleBinding) then 
            globalStableNameGenerator.GetUniqueCompilerGeneratedName(givenName,x.Range,x.Stamp) 
        else 
            givenName


    ///   - If this is a property then this is 'Foo' 
    ///   - If this is an implementation of an abstract slot then this is the name of the property implemented by the abstract slot
    member x.PropertyName = 
        let logicalName =  x.LogicalName
        ChopPropertyName logicalName


    /// The name of the method. 
    ///   - If this is a property then this is 'Foo' 
    ///   - If this is an implementation of an abstract slot then this is the name of the method implemented by the abstract slot
    ///   - If this is an operator then this is 'op_Addition'
    member x.CoreDisplayName = 
        match x.MemberInfo with 
        | Some membInfo -> 
            match membInfo.MemberFlags.MemberKind with 
            | MemberKind.ClassConstructor 
            | MemberKind.Constructor 
            | MemberKind.Member -> x.LogicalName
            | MemberKind.PropertyGetSet 
            | MemberKind.PropertySet
            | MemberKind.PropertyGet -> x.PropertyName
        | None -> x.LogicalName

    ///   - If this is a property then this is 'Foo' 
    ///   - If this is an implementation of an abstract slot then this is the name of the method implemented by the abstract slot
    ///   - If this is an operator then this is '(+)'
    member x.DisplayName = 
        DemangleOperatorName x.CoreDisplayName

    member x.SetValRec b                                 = x.Data.val_flags <- x.Data.val_flags.SetRecursiveValInfo b 
    member x.SetIsMemberOrModuleBinding()                = x.Data.val_flags <- x.Data.val_flags.SetIsMemberOrModuleBinding 
    member x.SetMakesNoCriticalTailcalls()               = x.Data.val_flags <- x.Data.val_flags.SetMakesNoCriticalTailcalls
    member x.SetHasBeenReferenced()                      = x.Data.val_flags <- x.Data.val_flags.SetHasBeenReferenced
    member x.SetIsCompiledAsStaticPropertyWithoutField() = x.Data.val_flags <- x.Data.val_flags.SetIsCompiledAsStaticPropertyWithoutField
    member x.SetValReprInfo info                          = x.Data.val_repr_info <- info
    member x.SetType ty                                  = x.Data.val_type <- ty
    member x.SetDefnRange m                              = x.Data.val_defn_range <- m

    // OSGN support
    static member NewUnlinked() : Val  = { Data = nullableSlotEmpty() }
    static member New(data) : Val = { Data = data }
    member x.Link(tg) = x.Data <- nullableSlotFull(tg)
    member x.IsLinked = match box x.Data with null -> false | _ -> true 

    override x.ToString() = x.LogicalName
    
    
and 
    [<NoEquality; NoComparison>]
    [<StructuredFormatDisplay("{val_logical_name}")>]
    ValData =
    { val_logical_name: string
      val_compiled_name: string option
      val_range: range
      mutable val_defn_range: range 
      mutable val_type: TType
      val_stamp: Stamp 
      /// See vflags section further below for encoding/decodings here 
      mutable val_flags: ValFlags
      mutable val_const: Const option
      
      /// What is the original, unoptimized, closed-term definition, if any? 
      /// Used to implement [<ReflectedDefinition>]
      mutable val_defn: Expr option 

      /// How visible is this? 
      val_access: Accessibility 

      /// Is the value actually an instance method/property/event that augments 
      /// a type, and if so what name does it take in the IL?
      val_member_info: ValMemberInfo option

      /// Custom attributes attached to the value. These contain references to other values (i.e. constructors in types). Mutable to fixup  
      /// these value references after copying a colelction of values. 
      mutable val_attribs: Attribs

      /// Records the "extra information" for a value compiled as a method (rather
      /// than a closure or a local).
      ///
      /// This indicates the number of arguments in each position for a curried 
      /// functions, and relates to the F# spec for arity analysis.
      /// For module-defined values, the currying is based 
      /// on the number of lambdas, and in each position the elements are 
      /// based on attempting to deconstruct the type of the argument as a 
      /// tuple-type.  
      ///
      /// The field is mutable because arities for recursive 
      /// values are only inferred after the r.h.s. is analyzed, but the 
      /// value itself is created before the r.h.s. is analyzed. 
      ///
      /// TLR also sets this for inner bindings that it wants to 
      /// represent as "top level" bindings.
     
      // MUTABILITY CLEANUP: mutability of this field is used by 
      //     -- adjustAllUsesOfRecValue 
      //     -- TLR optimizations
      //     -- LinearizeTopMatch
      //
      // For example, we use mutability to replace the empty arity initially assumed with an arity garnered from the 
      // type-checked expression.  
      mutable val_repr_info: ValReprInfo option


      // MUTABILITY CLEANUP: mutability of this field is used by 
      //     -- LinearizeTopMatch
      //
      // The fresh temporary should just be created with the right parent
      mutable val_actual_parent: ParentRef

      /// XML documentation attached to a value.
      val_xmldoc : XmlDoc 
      
      /// XML documentation signature for the value
      mutable val_xmldocsig : string } 

and 
    [<NoEquality; NoComparison>]
    ValMemberInfo = 
    { /// The parent type. For an extension member this is the type being extended 
      ApparentParent: TyconRef  

      /// Gets updated with full slotsig after interface implementation relation is checked 
      mutable ImplementedSlotSigs: SlotSig list 

      /// Gets updated with 'true' if an abstract slot is implemented in the file being typechecked.  Internal only. 
      mutable IsImplemented: bool                      

      MemberFlags: MemberFlags }


and 
    [<StructuredFormatDisplay("{Display}")>]
    NonLocalValOrMemberRef = 
    { /// A reference to the entity containing the value or member. THis will always be a non-local reference
      EnclosingEntity : EntityRef 
      /// The name of the value, or the full signature of the member
      ItemKey: ValLinkageFullKey }

    member x.Ccu = x.EnclosingEntity.nlr.Ccu
    member x.AssemblyName = x.EnclosingEntity.nlr.AssemblyName
    member x.Display = x.ToString()
    override x.ToString() = x.EnclosingEntity.nlr.ToString() + "::" + x.ItemKey.PartialKey.LogicalName
      
and ValPublicPath      = 
    | ValPubPath of PublicPath * ValLinkageFullKey

/// Index into the namespace/module structure of a particular CCU 
and NonLocalEntityRef    = 
    | NonLocalEntityRef of CcuThunk * string[]
        
    member nleref.TryDeref = 
        let (NonLocalEntityRef(ccu,p)) = nleref 
        ccu.EnsureDerefable(p)
        let rec loop (entity:Entity)  i = 
            if i >= p.Length then Some entity
            else  
                let next = entity.ModuleOrNamespaceType.AllEntitiesByCompiledAndLogicalMangledNames.TryFind(p.[i])
                match next with 
                | Some res -> loop res (i+1)
                | None -> None

        match loop ccu.Contents 0 with
        | Some _ as r -> r
        | None ->
            // OK, the lookup failed. Check if we can redirect through a type forwarder on this assembly.
            // Look for a forwarder for each prefix-path
            let rec tryForwardPrefixPath i = 
                if i < p.Length then 
                    match ccu.TryForward(p.[0..i-1],p.[i]) with
                    | Some tcref -> 
                       // OK, found a forwarder, now continue with the lookup to find the nested type
                       loop tcref.Deref (i+1)
                    | None -> tryForwardPrefixPath (i+1)
                else
                    None
            tryForwardPrefixPath 0
        
    member nleref.Ccu =
        let (NonLocalEntityRef(ccu,_)) = nleref 
        ccu
    member nleref.Path =
        let (NonLocalEntityRef(_,p)) = nleref 
        p

    member nleref.DisplayName =
        String.concat "." nleref.Path

    member nleref.LastItemMangledName = 
        let p = nleref.Path
        p.[p.Length-1]
    member nleref.EnclosingMangledPath =  
        let p = nleref.Path
        p.[0..p.Length-2]
        
    member nleref.AssemblyName = nleref.Ccu.AssemblyName

    member nleref.Deref = 
        match nleref.TryDeref with 
        | Some res -> res
        | None -> 
              errorR (InternalUndefinedItemRef (FSComp.SR.tastUndefinedItemRefModuleNamespace, nleref.DisplayName, nleref.AssemblyName, "<some module on this path>")); 
              raise (KeyNotFoundException())
        
    member nleref.TryModuleOrNamespaceType = 
        nleref.TryDeref |> Option.map (fun v -> v.ModuleOrNamespaceType) 

    member nleref.ModuleOrNamespaceType = 
        nleref.Deref.ModuleOrNamespaceType

    override x.ToString() = x.DisplayName
        
and 
    [<StructuredFormatDisplay("{LogicalName}")>]
    [<NoEquality; NoComparison>]
    EntityRef = 
    { /// Indicates a reference to something bound in this CCU 
      mutable binding: Entity NonNullSlot
      /// Indicates a reference to something bound in another CCU 
      nlr: NonLocalEntityRef }
    member x.IsLocalRef = match box x.nlr with null -> true | _ -> false
    member x.IsResolved = match box x.binding with null -> false | _ -> true
    member x.PrivateTarget = x.binding
    member x.ResolvedTarget = x.binding

    member private tcr.Resolve() = 
        let res = tcr.nlr.TryDeref
        match res with 
        | Some r -> 
             tcr.binding <- nullableSlotFull r 
        | None -> 
             ()

    // Dereference the TyconRef to a Tycon. Amortize the cost of doing this.
    // This path should not allocate in the amortized case
    member tcr.Deref = 
        match box tcr.binding with 
        | null ->
            tcr.Resolve()
            match box tcr.binding with 
            | null -> error (InternalUndefinedItemRef (FSComp.SR.tastUndefinedItemRefModuleNamespaceType, tcr.nlr.DisplayName, tcr.nlr.AssemblyName, tcr.nlr.LastItemMangledName))
            | _ -> tcr.binding
        | _ -> 
            tcr.binding

    // Dereference the TyconRef to a Tycon option.
    member tcr.TryDeref = 
        match box tcr.binding with 
        | null -> 
            tcr.Resolve()
            match box tcr.binding with 
            | null -> None
            | _ -> Some tcr.binding

        | _ -> 
            Some tcr.binding

    /// Is the destination assembly available?
    member tcr.CanDeref = tcr.TryDeref.IsSome

    override x.ToString() = 
       if x.IsLocalRef then 
           x.ResolvedTarget.DisplayName 
       else 
           x.nlr.DisplayName 


    member x.CompiledRepresentation = x.Deref.CompiledRepresentation
    member x.CompiledRepresentationForNamedType = x.Deref.CompiledRepresentationForNamedType
    member x.LogicalName = x.Deref.LogicalName
    member x.CompiledName = x.Deref.CompiledName
    member x.DisplayName = x.Deref.DisplayName
    member x.DisplayNameWithUnderscoreTypars = x.Deref.DisplayNameWithUnderscoreTypars
    member x.Range = x.Deref.Range
    member x.Stamp = x.Deref.Stamp
    member x.Attribs = x.Deref.Attribs
    member x.XmlDoc = x.Deref.XmlDoc
    member x.XmlDocSig = x.Deref.XmlDocSig
    member x.ModuleOrNamespaceType = x.Deref.ModuleOrNamespaceType
    
    member x.DemangledModuleOrNamespaceName = x.Deref.DemangledModuleOrNamespaceName

    member x.TypeContents = x.Deref.TypeContents
    member x.TypeOrMeasureKind = x.Deref.TypeOrMeasureKind
    member x.Id = x.Deref.Id
    member x.TypeReprInfo = x.Deref.TypeReprInfo
    member x.ExceptionInfo = x.Deref.ExceptionInfo
    member x.IsExceptionDecl = x.Deref.IsExceptionDecl
    
    member x.Typars(m) = x.Deref.Typars(m)
    member x.TyparsNoRange = x.Deref.TyparsNoRange
    member x.TypeAbbrev = x.Deref.TypeAbbrev
    member x.IsTypeAbbrev = x.Deref.IsTypeAbbrev
    member x.TypeReprAccessibility = x.Deref.TypeReprAccessibility
    member x.CompiledReprCache = x.Deref.CompiledReprCache
    member x.PublicPath : PublicPath option = x.Deref.PublicPath
    member x.Accessibility = x.Deref.Accessibility
    member x.IsPrefixDisplay = x.Deref.IsPrefixDisplay
    member x.IsModuleOrNamespace  = x.Deref.IsModuleOrNamespace
    member x.IsNamespace          = x.Deref.IsNamespace
    member x.IsModule             = x.Deref.IsModule
    member x.CompilationPathOpt   = x.Deref.CompilationPathOpt
    member x.CompilationPath      = x.Deref.CompilationPath
    member x.AllFieldTable        = x.Deref.AllFieldTable
    member x.AllFieldsArray       = x.Deref.AllFieldsArray
    member x.AllFieldsAsList = x.Deref.AllFieldsAsList
    member x.TrueFieldsAsList = x.Deref.TrueFieldsAsList
    member x.TrueInstanceFieldsAsList = x.Deref.TrueInstanceFieldsAsList
    member x.AllInstanceFieldsAsList = x.Deref.AllInstanceFieldsAsList
    member x.GetFieldByIndex(n)        = x.Deref.GetFieldByIndex(n)
    member x.GetFieldByName(n)         = x.Deref.GetFieldByName(n)
    member x.UnionTypeInfo             = x.Deref.UnionTypeInfo
    member x.UnionCasesArray           = x.Deref.UnionCasesArray
    member x.UnionCasesAsList     = x.Deref.UnionCasesAsList
    member x.GetUnionCaseByName(n)     = x.Deref.GetUnionCaseByName(n)
    member x.FSharpObjectModelTypeInfo = x.Deref.FSharpObjectModelTypeInfo
    member x.InterfacesOfFSharpTycon = x.Deref.InterfacesOfFSharpTycon
    member x.InterfaceTypesOfFSharpTycon = x.Deref.InterfaceTypesOfFSharpTycon
    member x.MembersOfFSharpTyconSorted = x.Deref.MembersOfFSharpTyconSorted
    member x.MembersOfFSharpTyconByName = x.Deref.MembersOfFSharpTyconByName
    member x.IsStructOrEnumTycon             = x.Deref.IsStructOrEnumTycon
    member x.IsAsmReprTycon            = x.Deref.IsAsmReprTycon
    member x.IsMeasureableReprTycon    = x.Deref.IsMeasureableReprTycon
    

    member x.GeneratedHashAndEqualsWithComparerValues = x.Deref.GeneratedHashAndEqualsWithComparerValues
    member x.GeneratedCompareToWithComparerValues = x.Deref.GeneratedCompareToWithComparerValues
    member x.GeneratedCompareToValues = x.Deref.GeneratedCompareToValues
    member x.GeneratedHashAndEqualsValues = x.Deref.GeneratedHashAndEqualsValues
    
    member x.IsILTycon                = x.Deref.IsILTycon
    member x.ILTyconInfo              = x.Deref.ILTyconInfo
    member x.ILTyconRawMetadata       = x.Deref.ILTyconRawMetadata
    member x.IsUnionTycon             = x.Deref.IsUnionTycon
    member x.UnionInfo                = x.Deref.UnionInfo
    member x.IsRecordTycon            = x.Deref.IsRecordTycon
    member x.IsFSharpObjectModelTycon = x.Deref.IsFSharpObjectModelTycon
    member x.IsHiddenReprTycon        = x.Deref.IsHiddenReprTycon

    member x.IsFSharpInterfaceTycon   = x.Deref.IsFSharpInterfaceTycon
    member x.IsFSharpDelegateTycon    = x.Deref.IsFSharpDelegateTycon
    member x.IsFSharpEnumTycon        = x.Deref.IsFSharpEnumTycon
    member x.IsILEnumTycon            = x.Deref.IsILEnumTycon
    member x.IsEnumTycon              = x.Deref.IsEnumTycon

    member x.IsFSharpStructOrEnumTycon      = x.Deref.IsFSharpStructOrEnumTycon

    member x.IsILStructTycon          = x.Deref.IsILStructTycon
    member x.PreEstablishedHasDefaultConstructor = x.Deref.PreEstablishedHasDefaultConstructor
    member x.HasSelfReferentialConstructor = x.Deref.HasSelfReferentialConstructor


/// note: ModuleOrNamespaceRef and TyconRef are type equivalent 
and ModuleOrNamespaceRef       = EntityRef
and TyconRef       = EntityRef

/// References are either local or nonlocal
and 
    [<NoEquality; NoComparison>]
    [<StructuredFormatDisplay("{LogicalName}")>]
    ValRef = 
    { /// Indicates a reference to something bound in this CCU 
      mutable binding: Val NonNullSlot
      /// Indicates a reference to something bound in another CCU 
      nlr: NonLocalValOrMemberRef }
    member x.IsLocalRef = match box x.nlr with null -> true | _ -> false
    member x.IsResolved = match box x.binding with null -> false | _ -> true
    member x.PrivateTarget = x.binding
    member x.ResolvedTarget = x.binding

    member vr.Deref = 
        match box vr.binding with 
        | null ->
            let res = 
                let nlr = vr.nlr 
                let e =  nlr.EnclosingEntity.Deref 
                let possible = e.ModuleOrNamespaceType.TryLinkVal(nlr.EnclosingEntity.nlr.Ccu, nlr.ItemKey)
                match possible with 
                | None -> error (InternalUndefinedItemRef (FSComp.SR.tastUndefinedItemRefVal, e.DisplayName, nlr.AssemblyName, sprintf "%+A" nlr.ItemKey.PartialKey))
                | Some h -> h
            vr.binding <- nullableSlotFull res 
            res 
        | _ -> vr.binding

    member vr.TryDeref = 
        match box vr.binding with 
        | null -> 
            let resOpt = 
                vr.nlr.EnclosingEntity.TryDeref |> Option.bind (fun e -> 
                    e.ModuleOrNamespaceType.TryLinkVal(vr.nlr.EnclosingEntity.nlr.Ccu, vr.nlr.ItemKey))
            match resOpt with 
            | None -> ()
            | Some res -> 
                vr.binding <- nullableSlotFull res 
            resOpt
        | _ -> 
            Some vr.binding

    member x.Type                       = x.Deref.Type
    member x.TypeScheme                 = x.Deref.TypeScheme
    member x.TauType                    = x.Deref.TauType
    member x.Typars                     = x.Deref.Typars
    member x.LogicalName                = x.Deref.LogicalName
    member x.DisplayName                = x.Deref.DisplayName
    member x.CoreDisplayName            = x.Deref.CoreDisplayName
    member x.Range                      = x.Deref.Range

    member x.Accessibility              = x.Deref.Accessibility
    member x.ActualParent               = x.Deref.ActualParent
    member x.ApparentParent             = x.Deref.ApparentParent
    member x.DefinitionRange            = x.Deref.DefinitionRange
    member x.LiteralValue               = x.Deref.LiteralValue
    member x.Id                         = x.Deref.Id
    member x.PropertyName               = x.Deref.PropertyName
    member x.Stamp                      = x.Deref.Stamp
    member x.IsCompiledAsTopLevel       = x.Deref.IsCompiledAsTopLevel
    member x.IsDispatchSlot             = x.Deref.IsDispatchSlot
    member x.CompiledName         = x.Deref.CompiledName

    member x.PublicPath                 = x.Deref.PublicPath
    member x.ReflectedDefinition        = x.Deref.ReflectedDefinition
    member x.IsConstructor              = x.Deref.IsConstructor
    member x.IsOverrideOrExplicitImpl   = x.Deref.IsOverrideOrExplicitImpl
    member x.MemberInfo                 = x.Deref.MemberInfo
    member x.IsMember                   = x.Deref.IsMember
    member x.IsModuleBinding            = x.Deref.IsModuleBinding
    member x.IsInstanceMember           = x.Deref.IsInstanceMember

    member x.IsMutable                  = x.Deref.IsMutable
    member x.PermitsExplicitTypeInstantiation  = x.Deref.PermitsExplicitTypeInstantiation
    member x.MakesNoCriticalTailcalls  = x.Deref.MakesNoCriticalTailcalls
    member x.IsMemberOrModuleBinding    = x.Deref.IsMemberOrModuleBinding
    member x.IsExtensionMember          = x.Deref.IsExtensionMember
    member x.IsIncrClassConstructor = x.Deref.IsIncrClassConstructor
    member x.IsIncrClassGeneratedMember = x.Deref.IsIncrClassGeneratedMember
    member x.RecursiveValInfo           = x.Deref.RecursiveValInfo
    member x.BaseOrThisInfo             = x.Deref.BaseOrThisInfo
    member x.IsTypeFunction             = x.Deref.IsTypeFunction
    member x.ValReprInfo                 = x.Deref.ValReprInfo
    member x.InlineInfo                 = x.Deref.InlineInfo
    member x.MustInline                 = x.Deref.MustInline
    member x.IsCompilerGenerated        = x.Deref.IsCompilerGenerated
    member x.Attribs                    = x.Deref.Attribs
    member x.XmlDoc                     = x.Deref.XmlDoc
    member x.XmlDocSig                  = x.Deref.XmlDocSig
    member x.TopValActualParent         = x.Deref.TopValActualParent
    member x.MemberApparentParent       = x.Deref.MemberApparentParent
    member x.NumObjArgs                 = x.Deref.NumObjArgs

    override x.ToString() = 
       if x.IsLocalRef then x.ResolvedTarget.DisplayName 
       else x.nlr.ToString()

and UnionCaseRef = 
    | UCRef of TyconRef * string
    member x.TyconRef = let (UCRef(tcref,_)) = x in tcref
    member x.CaseName = let (UCRef(_,nm)) = x in nm
    member x.Tycon = x.TyconRef.Deref

and RecdFieldRef = 
    | RFRef of TyconRef * string
    member x.TyconRef = let (RFRef(tcref,_)) = x in tcref
    member x.FieldName = let (RFRef(_,id)) = x in id
    member x.Tycon = x.TyconRef.Deref

and 
  /// The algebra of types
    [<NoEquality; NoComparison>]
// REMOVING because of possible stack overflow 

#if EXTENSIBLE_DUMPER
#if DEBUG
    [<System.Diagnostics.DebuggerTypeProxy(typedefof<Internal.Utilities.Diagnostic.ExtensibleDumper>)>]
#endif  
#endif  
    TType =
    /// Indicates the type is a universal type, only used for types of values, members and record fields 
    | TType_forall of Typars * TType
    /// Indicates the type is a type application 
    | TType_app of TyconRef * TypeInst
    /// Indicates the type is a tuple type 
    | TType_tuple of TTypes
    /// Indicates the type is a function type 
    | TType_fun of  TType * TType
    /// Indicates the type is a non-F#-visible type representing a "proof" that a union value belongs to a particular union case
    /// These types are not user-visible and will never appear as an inferred type. They are the types given to
    /// the temporaries arising out of pattern matching on union values.
    | TType_ucase of  UnionCaseRef * TypeInst
    /// Indicates the type is a variable type, whether declared, generalized or an inference type parameter  
    | TType_var of Typar 
    | TType_measure of MeasureExpr
    override x.ToString() =  
        match x with 
        | TType_forall (_tps,ty) -> "forall _. " + ty.ToString()
        | TType_app (tcref, tinst) -> tcref.DisplayName + (match tinst with [] -> "" | tys -> "<" + String.concat "," (List.map string tys) + ">")
        | TType_tuple tinst -> "(" + String.concat "," (List.map string tinst) + ")"
        | TType_fun (d,r) -> "(" + string d + " -> " + string r + ")"
        | TType_ucase (uc,tinst) -> "union case type " + uc.CaseName + (match tinst with [] -> "" | tys -> "<" + String.concat "," (List.map string tys) + ">")
        | TType_var tp -> tp.DisplayName
        | TType_measure ms -> sprintf "%A" ms

and TypeInst = TType list 
and TTypes = TType list 

and MeasureExpr = 
    | MeasureVar of Typar
    | MeasureCon of TyconRef
    | MeasureProd of MeasureExpr*MeasureExpr
    | MeasureInv of MeasureExpr
    | MeasureOne

and 
    [<NoEquality; NoComparison>]
    CcuData = 
    { /// Holds the filename for the DLL, if any 
      FileName: string option 
      /// Is the CCu an EST injected assembly
      IsEstGenerated: bool 
      
      /// Holds the data indicating how this assembly/module is referenced from the code being compiled. 
      ILScopeRef: ILScopeRef
      
      /// A unique stamp for this DLL 
      Stamp: Stamp
      
      /// The fully qualified assembly reference string to refer to this assembly. This is persisted in quotations 
      QualifiedName: string option 
      
      /// A hint as to where does the code for the CCU live (e.g what was the tcConfig.implicitIncludeDir at compilation time for this DLL?) 
      SourceCodeDirectory: string 
      
      /// Indicates that this DLL was compiled using the F# compiler and has F# metadata
      IsFSharp: bool 
      
      /// Indicates that this DLL uses quotation literals somewhere. This is used to implement a restriction on static linking
      mutable UsesQuotations : bool
      
      /// A handle to the full specification of the contents of the module contained in this ccu
      // NOTE: may contain transient state during typechecking 
      mutable Contents: ModuleOrNamespace
      
      /// A helper function used to link method signatures using type equality. This is effectively a forward call to the type equality 
      /// logic in tastops.fs
      mutable MemberSignatureEquality : (TType -> TType -> bool) 
      
      /// The table of type forwarders for this assembly
      TypeForwarders : CcuTypeForwarderTable }

and CcuTypeForwarderTable = Lazy<Map<string[] * string, EntityRef>>

and CcuReference =  string // ILAssemblyRef

// Compilation units and Cross-compilation-unit thunks.
//
// A compilation unit is, more or less, the new material created in one
// invocation of the compiler.  Due to static linking assemblies may hold more 
// than one compilation unit (i.e. when two assemblies are merged into a compilation
// the resulting assembly will contain 3 CUs).  Compilation units are also created for referenced
// .NET assemblies. 
// 
// References to items such as type constructors are via 
// cross-compilation-unit thunks, which directly reference the data structures that define
// these modules.  Thus, when saving out values to disk we only wish 
// to save out the "current" part of the term graph.  When reading values
// back in we "fixup" the links to previously referenced modules.
//
// All non-local accesses to the data structures are mediated
// by ccu-thunks.  Ultimately, a ccu-thunk is either a (named) element of
// the data structure, or it is a delayed fixup, i.e. an invalid dangling
// reference that has not had an appropriate fixup applied.  

/// A relinkable handle to the contents of a compilation unit. Relinking is performed by mutation.
and CcuThunk = 
    { mutable target: CcuData
      mutable orphanfixup : bool
      name: CcuReference  }
      

    member ccu.Deref = 
        if isNull ccu.target || ccu.orphanfixup then 
            raise(UnresolvedReferenceNoRange ccu.name)
        ccu.target
   
    member ccu.IsUnresolvedReference = (isNull ccu.target || ccu.orphanfixup)

    /// Ensure the ccu is derefable in advance. Supply a path to attach to any resulting error message.
    member ccu.EnsureDerefable(requiringPath:string[]) = 
        // ccu.orphanfixup is true when a reference is missing in the transitive closure of static references that
        // may potentially be required for the metadata of referenced DLLs. It is set to true if the "loader"
        // used in the F# metadata-deserializer or the .NET metadata reader returns a failing value (e.g. None).
        // Note: When used from Visual Studio, the loader will not automatically chase down transitively referenced DLLs - they
        // must be in the explicit references in the project.
        if ccu.IsUnresolvedReference then 
            let path = System.String.Join(".", requiringPath)
            raise(UnresolvedPathReferenceNoRange(ccu.name,path))
            
    /// Indicates that this DLL uses quotation literals somewhere. This is used to implement a restriction on static linking
    member ccu.UsesQuotations 
        with get() = ccu.Deref.UsesQuotations 
        and set v = ccu.Deref.UsesQuotations <- v
    member ccu.AssemblyName        = ccu.name
    /// Holds the data indicating how this assembly/module is referenced from the code being compiled. 
    member ccu.ILScopeRef          = ccu.Deref.ILScopeRef
    /// A unique stamp for this DLL 
    member ccu.Stamp               = ccu.Deref.Stamp
    /// Holds the filename for the DLL, if any 
    member ccu.FileName            = ccu.Deref.FileName
    /// Is the CCu an EST injected assembly
    member ccu.IsEstGenerated      = ccu.Deref.IsEstGenerated
    /// The fully qualified assembly reference string to refer to this assembly. This is persisted in quotations 
    member ccu.QualifiedName       = ccu.Deref.QualifiedName
    /// A hint as to where does the code for the CCU live (e.g what was the tcConfig.implicitIncludeDir at compilation time for this DLL?) 
    member ccu.SourceCodeDirectory = ccu.Deref.SourceCodeDirectory
    /// Indicates that this DLL was compiled using the F# compiler and has F# metadata
    member ccu.IsFSharp            = ccu.Deref.IsFSharp
    /// A handle to the full specification of the contents of the module contained in this ccu
    // NOTE: may contain transient state during typechecking 
    member ccu.Contents            = ccu.Deref.Contents
    /// The table of type forwarders for this assembly
    member ccu.TypeForwarders : Map<string[] * string, EntityRef>  = ccu.Deref.TypeForwarders.Force()

    /// The table of modules and namespaces at the "root" of the assembly
    member ccu.RootModulesAndNamespaces = ccu.Contents.ModuleOrNamespaceType.ModuleAndNamespaceDefinitions
    /// The table of type definitions at the "root" of the assembly
    member ccu.RootTypeAndExceptionDefinitions = ccu.Contents.ModuleOrNamespaceType.TypeAndExceptionDefinitions

    static member Create(nm,x) = 
        { target = x 
          orphanfixup = false
          name = nm  }

    static member CreateDelayed(nm) = 
        { target = Unchecked.defaultof<_> 
          orphanfixup = false
          name = nm  }

    member x.Fixup(avail:CcuThunk) = 
        match box x.target with
        | null -> 
            assert (avail.AssemblyName = x.AssemblyName)
            x.target <- 
               (match box avail.target with
                | null -> error(Failure("internal error: ccu thunk '"+avail.name+"' not fixed up!"))
                | _ -> avail.target)
        | _ -> errorR(Failure("internal error: the ccu thunk for assembly "+x.AssemblyName+" not delayed!"))
        
    member x.FixupOrphaned() = 
        match box x.target with
        | null -> x.orphanfixup<-true
        | _ -> errorR(Failure("internal error: the ccu thunk for assembly "+x.AssemblyName+" not delayed!"))
            
    member ccu.TryForward(nlpath:string[],item:string) : EntityRef option  = 
        ccu.EnsureDerefable(nlpath)
        ccu.TypeForwarders.TryFind(nlpath,item) 
        //printfn "trying to forward %A::%s from ccu '%s', res = '%A'" p n ccu.AssemblyName res.IsSome

    member ccu.MemberSignatureEquality(ty1:TType, ty2:TType) = 
        ccu.Deref.MemberSignatureEquality ty1 ty2
    

    override ccu.ToString() = ccu.AssemblyName

/// The result of attempting to resolve an assembly name to a full ccu.
/// UnresolvedCcu will contain the name of the assembly that could not be resolved.
and CcuResolutionResult =
    | ResolvedCcu of CcuThunk
    | UnresolvedCcu of string

and PickledModuleInfo =
  { mspec: ModuleOrNamespace
    compileTimeWorkingDir: string
    usesQuotations : bool }

//---------------------------------------------------------------------------
// Attributes
//---------------------------------------------------------------------------

and Attribs = Attrib list 

and AttribKind = 
  /// Indicates an attribute refers to a type defined in an imported .NET assembly 
  | ILAttrib of ILMethodRef 
  /// Indicates an attribute refers to a type defined in an imported F# assembly 
  | FSAttrib of ValRef

/// Attrib(kind,unnamedArgs,propVal,appliedToAGetterOrSetter,range)
and Attrib = 
  | Attrib of TyconRef * AttribKind * AttribExpr list * AttribNamedArg list * bool * range

/// We keep both source expression and evaluated expression around to help intellisense and signature printing
and AttribExpr = 
    /// AttribExpr(source, evaluated)
    AttribExpr of Expr * Expr 

/// AttribNamedArg(name,type,isField,value)
and AttribNamedArg = AttribNamedArg of (string*TType*bool*AttribExpr)

/// Constants in expressions
and [<RequireQualifiedAccess>]
    Const = 
    | Bool     of bool
    | SByte    of sbyte
    | Byte     of byte
    | Int16    of int16
    | UInt16   of uint16
    | Int32    of int32
    | UInt32   of uint32
    | Int64    of int64
    | UInt64   of uint64
    | IntPtr   of int64
    | UIntPtr  of uint64
    | Single   of single
    | Double   of double
    | Char     of char
    | String   of string // in unicode 
    | Decimal  of Decimal 
    | Unit
    | Zero // null/zero-bit-pattern 
  

/// Decision trees. Pattern matching has been compiled down to
/// a decision tree by this point.  The right-hand-sides (actions) of
/// the decision tree are labelled by integers that are unique for that
/// particular tree.
and 
    [<NoEquality; NoComparison>]
    DecisionTree = 

    /// TDSwitch(input, cases, default, range)
    ///
    /// Indicates a decision point in a decision tree. 
    ///    input -- the expression being tested
    ///    cases -- the list of tests and their subsequent decision trees
    ///    default -- the default decision tree, if any
    ///    range -- (precise documentation  needed)
    | TDSwitch  of Expr * DecisionTreeCase list * DecisionTree option * range

    /// TDSuccess(results, targets)
    ///
    /// Indicates the decision tree has terminated with success, calling the given target with the given parameters.
    ///    results -- the expressions to be bound to the variables at the target
    ///    target -- the target number for the continuation
    | TDSuccess of FlatExprs * int  

    /// TDBind(binding, body)
    ///
    /// Bind the given value throught the remaining cases of the dtree. 
    /// These arise from active patterns and some optimizations to prevent
    /// repeated computations in decision trees.
    ///    binding -- the value and the expression it is bound to
    ///    body -- the rest of the decision tree
    | TDBind of Binding * DecisionTree

and DecisionTreeCase = 
    | TCase of Test * DecisionTree
    member x.Discriminator = let (TCase(d,_)) = x in d
    member x.CaseTree = let (TCase(_,d)) = x in d

and 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    Test = 
    /// Test if the input to a decision tree matches the given union case
    | UnionCase of UnionCaseRef * TypeInst

    /// Test if the input to a decision tree is an array of the given length 
    | ArrayLength of int * TType  

    /// Test if the input to a decision tree is the given constant value 
    | Const of Const

    /// Test if the input to a decision tree is null 
    | IsNull 

    /// IsInst(source, target)
    ///
    /// Test if the input to a decision tree is an instance of the given type 
    | IsInst of TType * TType

    /// Test.ActivePatternCase(activePatExpr, activePatResTys, activePatIdentity, idx, activePatInfo)
    ///
    /// Run the active pattern and bind a successful result to a 
    /// variable in the remaining tree. 
    ///     activePatExpr     -- The active pattern function being called, perhaps applied to some active pattern parameters.
    ///     activePatResTys   -- The result types (case types) of the active pattern.
    ///     activePatIdentity -- The value and the types it is applied to. If there are any active pattern parameters then this is empty. 
    ///     idx               -- The case number of of the active pattern which the test relates to.
    ///     activePatternInfo -- The extracted info for the active pattern.
    | ActivePatternCase of Expr * TTypes * (ValRef * TypeInst) option * int * ActivePatternInfo


/// A target of a decision tree. Can be thought of as a little function, though is compiled as a local block. 
and DecisionTreeTarget = 
    | TTarget of FlatVals * Expr * SequencePointInfoForTarget

and Bindings = FlatList<Binding>

and Binding = 
    | TBind of Val * Expr * SequencePointInfoForBinding
    member x.Var               = (let (TBind(v,_,_)) = x in v)
    member x.Expr              = (let (TBind(_,e,_)) = x in e)
    member x.SequencePointInfo = (let (TBind(_,_,sp)) = x in sp)
    
// ActivePatternElemRef: active pattern element (deconstruction case), e.g. 'JNil' or 'JCons'. 
// Integer indicates which choice in the target set is being selected by this item. 
and ActivePatternElemRef = 
    | APElemRef of ActivePatternInfo * ValRef * int 

    member x.IsTotalActivePattern = (let (APElemRef(total,_,_)) = x in total)
    member x.ActivePatternVal = (let (APElemRef(_,vref,_)) = x in vref)
    member x.CaseIndex = (let (APElemRef(_,_,n)) = x in n)

/// Records the "extra information" for a value compiled as a method (rather
/// than a closure or a local), including argument names, attributes etc.
and ValReprInfo  = 
    /// ValReprInfo (numTypars, args, result)
    | ValReprInfo  of TyparReprInfo list * ArgReprInfo list list * ArgReprInfo 
    member x.ArgInfos       = (let (ValReprInfo(_,args,_)) = x in args)
    member x.NumCurriedArgs = (let (ValReprInfo(_,args,_)) = x in args.Length)
    member x.NumTypars      = (let (ValReprInfo(n,_,_)) = x in n.Length)
    member x.HasNoArgs      = (let (ValReprInfo(n,args,_)) = x in n.IsEmpty && args.IsEmpty)
    member x.AritiesOfArgs  = (let (ValReprInfo(_,args,_)) = x in List.map List.length args)
    member x.KindsOfTypars  = (let (ValReprInfo(n,_,_)) = x in n |> List.map (fun (TyparReprInfo(_,k)) -> k))

/// Records the "extra information" for an argument compiled as a real
/// method argument, specificially the argument name and attributes.
and 
    [<RequireQualifiedAccess>]
    ArgReprInfo = 
    { 
      // MUTABILITY; used when propagating signature attributes into the implementation.
      mutable Attribs : Attribs 
      // MUTABILITY; used when propagating names of parameters from signature into the implementation.
      mutable Name : Ident option  }

/// Records the extra metadata stored about typars for type parameters
/// compiled as "real" IL type parameters, specifically for values with 
/// ValReprInfo. Any information here is propagated from signature through
/// to the compiled code.
and TyparReprInfo = TyparReprInfo of Ident * TyparKind

and Typars = Typar list
 
and Exprs = Expr list
and FlatExprs = FlatList<Expr>
and Vals = Val list
and FlatVals = FlatList<Val>

/// The big type of expressions.  
and 
    [<RequireQualifiedAccess>]
    Expr =
    /// A constant expression. 
    | Const of Const * range * TType

    /// Reference a value. The flag is only relevant if the value is an object model member 
    /// and indicates base calls and special uses of object constructors. 
    | Val of ValRef * ValUseFlag * range

    /// Sequence expressions, used for "a;b", "let a = e in b;a" and "a then b" (the last an OO constructor). 
    | Seq of Expr * Expr * SequentialOpKind * SequencePointInfoForSeq * range

    /// Lambda expressions. 
    
    // Why multiple vspecs? A Expr.Lambda taking multiple arguments really accepts a tuple. 
    // But it is in a convenient form to be compile accepting multiple 
    // arguments, e.g. if compiled as a toplevel static method. 
    | Lambda of Unique * Val option * Val option * Val list * Expr * range * TType

    // Type lambdas.  These are used for the r.h.s. of polymorphic 'let' bindings and 
    // for expressions that implement first-class polymorphic values. 
    | TyLambda of Unique * Typars * Expr * range * TType

    /// Applications.
    /// Applications combine type and term applications, and are normalized so 
    /// that sequential applications are combined, so "(f x y)" becomes "f [[x];[y]]". 
    /// The type attached to the function is the formal function type, used to ensure we don't build application 
    /// nodes that over-apply when instantiating at function types. 
    | App of Expr * TType * TypeInst * Exprs * range 

    /// Bind a recursive set of values. 
    | LetRec of Bindings * Expr * range * FreeVarsCache

    /// Bind a value. 
    | Let of Binding * Expr * range * FreeVarsCache

    // Object expressions: A closure that implements an interface or a base type. 
    // The base object type might be a delegate type. 
    | Obj of 
         (* unique *)           Unique * 
         (* object typ *)       TType *                                         (* <-- NOTE: specifies type parameters for base type *)
         (* base val *)         Val option * 
         (* ctor call *)        Expr * 
         (* overrides *)        ObjExprMethod list * 
         (* extra interfaces *) (TType * ObjExprMethod list) list *                   
                                range

    // Pattern matching. 

    /// Matches are a more complicated form of "let" with multiple possible destinations 
    /// and possibly multiple ways to get to each destination.  
    /// The first mark is that of the expression being matched, which is used 
    /// as the mark for all the decision making and binding that happens during the match. 
    | Match of SequencePointInfoForBinding * range * DecisionTree * DecisionTreeTarget array * range * TType

    /// If we statically know some infomation then in many cases we can use a more optimized expression 
    /// This is primarily used by terms in the standard library, particularly those implementing overloaded 
    /// operators. 
    | StaticOptimization of StaticOptimization list * Expr * Expr * range

    /// An intrinsic applied to some (strictly evaluated) arguments 
    /// A few of intrinsics (TOp_try, TOp.While, TOp.For) expect arguments kept in a normal form involving lambdas 
    | Op of TOp * TypeInst * Exprs * range

    // Indicates the expression is a quoted expression tree. 
    | Quote of Expr * (TTypes * Exprs * ExprData) option ref * range * TType  
    
    /// Typechecking residue: Indicates a free choice of typars that arises due to 
    /// minimization of polymorphism at let-rec bindings.  These are 
    /// resolved to a concrete instantiation on subsequent rewrites. 
    | TyChoose of Typars * Expr * range

    /// Typechecking residue: A Expr.Link occurs for every use of a recursively bound variable. While type-checking 
    /// the recursive bindings a dummy expression is stored in the mutable reference cell. 
    /// After type checking the bindings this is replaced by a use of the variable, perhaps at an 
    /// appropriate type instantiation. These are immediately eliminated on subsequent rewrites. 
    | Link of Expr ref

and 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    TOp =
    /// An operation representing the creation of a union value of the particular union case
    | UnionCase of UnionCaseRef 
    /// An operation representing the creation of an exception value using an F# exception declaration
    | ExnConstr of TyconRef
    /// An operation representing the creation of a tuple value
    | Tuple 
    /// An operation representing the creation of an array value
    | Array
    /// Constant byte arrays (used for parser tables and other embedded data)
    | Bytes of byte[] 
    /// Constant uint16 arrays (used for parser tables)
    | UInt16s of uint16[] 
    /// An operation representing a lambda-encoded while loop. The special while loop marker is used to mark compilations of 'foreach' expressions
    | While of SequencePointInfoForWhileLoop * SpecialWhileLoopMarker
    /// An operation representing a lambda-encoded for loop
    | For of SequencePointInfoForForLoop * ForLoopStyle (* count up or down? *)
    /// An operation representing a lambda-encoded try/catch
    | TryCatch of SequencePointInfoForTry * SequencePointInfoForWith
    /// An operation representing a lambda-encoded try/finally
    | TryFinally of SequencePointInfoForTry * SequencePointInfoForFinally

    /// Construct a record or object-model value. The ValRef is for self-referential class constructors, otherwise 
    /// it indicates that we're in a constructor and the purpose of the expression is to 
    /// fill in the fields of a pre-created but uninitialized object, and to assign the initialized 
    /// version of the object into the optional mutable cell pointed to be the given value. 
    | Recd of RecordConstructionInfo * TyconRef
    
    /// An operation representing setting a record or class field
    | ValFieldSet of RecdFieldRef 
    /// An operation representing getting a record or class field
    | ValFieldGet of RecdFieldRef 
    /// An operation representing getting the address of a record field
    | ValFieldGetAddr of RecdFieldRef       
    /// An operation representing getting an integer tag for a union value representing the union case number
    | UnionCaseTagGet of TyconRef 
    /// An operation representing a coercion that proves a union value is of a particular union case. THis is not a test, its
    /// simply added proof to enable us to generate verifiable code for field access on union types
    | UnionCaseProof of UnionCaseRef
    /// An operation representing a field-get from a union value, where that value has been proven to be of the corresponding union case.
    | UnionCaseFieldGet of UnionCaseRef * int 
    /// An operation representing a field-get from a union value. THe value is not assumed to have been proven to be of the corresponding union case.
    | UnionCaseFieldSet of  UnionCaseRef * int
    /// An operation representing a field-get from an F# exception value.
    | ExnFieldGet of TyconRef * int 
    /// An operation representing a field-set on an F# exception value.
    | ExnFieldSet of TyconRef * int 
    /// An operation representing a field-get from an F# tuple value.
    | TupleFieldGet of int 
    /// IL assembly code - type list are the types pushed on the stack 
    | ILAsm of ILInstr list * TTypes 
    /// Generate a ldflda on an 'a ref. 
    | RefAddrGet 
    /// Conversion node, compiled via type-directed translation or to box/unbox 
    | Coerce 
    /// Represents a "rethrow" operation. May not be rebound, or used outside of try-finally, expecting a unit argument 
    | Reraise 
    /// Used for state machine compilation
    | Return
    /// Used for state machine compilation
    | Goto of ILCodeLabel
    /// Used for state machine compilation
    | Label of ILCodeLabel

    /// Pseudo method calls. This is used for overloaded operations like op_Addition. 
    | TraitCall of TraitConstraintInfo  

    /// Operation nodes represnting C-style operations on byrefs and mutable vals (l-values) 
    | LValueOp of LValueOperation * ValRef 

    /// ILCall(useCallvirt,isProtected,valu,newobj,valUseFlags,isProp,isDllImport,mref,actualTypeInst,actualMethInst, retTy)
    ///  
    /// IL method calls.
    ///     value -- is the object a value type? 
    ///     isProp -- used for quotation reflection.
    ///     isDllImport - DllImport? if so don't tailcall 
    ///     actualTypeInst -- instantiation of the enclosing type
    ///     actualMethInst -- instantiation of the method
    ///     retTy -- the types of pushed values, if any 
    | ILCall of bool * bool * bool * bool * ValUseFlag * bool * bool * ILMethodRef * TypeInst * TypeInst * TTypes   


and RecordConstructionInfo = 
   /// We're in a constructor. The purpose of the record expression is to 
   /// fill in the fields of a pre-created but uninitialized object 
   | RecdExprIsObjInit
   /// Normal record construction 
   | RecdExpr
   

// If this is Some(ty) then it indicates that a .NET 2.0 constrained call is required, witht he given type as the
// static type of the object argument.
and ConstrainedCallInfo = TType option

and SpecialWhileLoopMarker = 
    | NoSpecialWhileLoopMarker
    | WhileLoopForCompiledForEachExprMarker  // marks the compiled form of a 'for ... in ... do ' expression
    
and ForLoopStyle = 
    /// Evaluate start and end once, loop up
    | FSharpForLoopUp 
    /// Evaluate start and end once, loop down
    | FSharpForLoopDown 
    /// Evaluate start once and end multiple times, loop up
    | CSharpForLoopUp

and LValueOperation = 
    /// In C syntax this is: &localv            
    | LGetAddr      
    /// In C syntax this is: *localv_ptr        
    | LByrefGet     
    /// In C syntax this is:  localv = e     , note == *(&localv) = e == LGetAddr; LByrefSet
    | LSet          
    /// In C syntax this is: *localv_ptr = e   
    | LByrefSet     

and SequentialOpKind = 
    /// a ; b 
    | NormalSeq 
    /// let res = a in b;res 
    | ThenDoSeq     

and ValUseFlag =
    /// Indicates a use of a value represents a call to a method that may require
    /// a .NET 2.0 constrained call. A constrained call is only used for calls where 
    // the object argument is a value type or generic type, and the call is to a method
    //  on System.Object, System.ValueType, System.Enum or an interface methods.
    | PossibleConstrainedCall of TType
    /// A normal use of a value
    | NormalValUse
    /// A call to a constructor, e.g. 'inherit C()'
    | CtorValUsedAsSuperInit
    /// A call to a constructor, e.g. 'new C() = new C(3)'
    | CtorValUsedAsSelfInit
    /// A call to a base method, e.g. 'base.OnPaint(args)'
    | VSlotDirectCall
  
and StaticOptimization = 
    | TTyconEqualsTycon of TType * TType
    | TTyconIsStruct of TType 
  
/// A representation of a method in an object expression. 
/// Note: Methods associated with types are represented as val declarations
/// Note: We should probably use Vals for object expressions, as then the treatment of members 
/// in object expressions could be more unified with the treatment of members in types 
and ObjExprMethod = 
    | TObjExprMethod of SlotSig * Attribs * Typars * Val list list * Expr * range
    member x.Id = let (TObjExprMethod(slotsig,_,_,_,_,m)) = x in mkSynId m slotsig.Name

and SlotSig = 
    | TSlotSig of string * TType * Typars * Typars * SlotParam list list * TType option
    member ss.Name             = let (TSlotSig(nm,_,_,_,_,_)) = ss in nm
    member ss.ImplementedType  = let (TSlotSig(_,ty,_,_,_,_)) = ss in ty
    member ss.ClassTypars      = let (TSlotSig(_,_,ctps,_,_,_)) = ss in ctps
    member ss.MethodTypars     = let (TSlotSig(_,_,_,mtps,_,_)) = ss in mtps
    member ss.FormalParams     = let (TSlotSig(_,_,_,_,ps,_)) = ss in ps
    member ss.FormalReturnType = let (TSlotSig(_,_,_,_,_,rt)) = ss in rt

and SlotParam = 
    | TSlotParam of  string option * TType * bool (* in *) * bool (* out *) * bool (* optional *) * Attribs
    member x.Type = let (TSlotParam(_,ty,_,_,_,_)) = x in ty

/// A type for a module-or-namespace-fragment and the actual definition of the module-or-namespace-fragment
and ModuleOrNamespaceExprWithSig = 
    | ModuleOrNamespaceExprWithSig of 
         /// The ModuleOrNamespaceType is a binder. However it is not used in the ModuleOrNamespaceExpr: it is only referenced from the 'outside' 
         ModuleOrNamespaceType 
         * ModuleOrNamespaceExpr
         * range
    member x.Type = let (ModuleOrNamespaceExprWithSig(mtyp,_,_)) = x in mtyp

/// The contents of a module-or-namespace-fragment definition 
and ModuleOrNamespaceExpr = 
    /// Indicates the module is a module with a signature 
    | TMAbstract of ModuleOrNamespaceExprWithSig
    /// Indicates the module fragment is made of several module fragments in succession 
    | TMDefs     of ModuleOrNamespaceExpr list  
    /// Indicates the module fragment is a 'let' definition 
    | TMDefLet   of Binding * range
    /// Indicates the module fragment is an evaluation of expression for side-effects
    | TMDefDo   of Expr * range
    /// Indicates the module fragment is a 'rec' definition of types, values and modules
    | TMDefRec   of Tycon list * Bindings * ModuleOrNamespaceBinding list * range

/// A named module-or-namespace-fragment definition 
and ModuleOrNamespaceBinding = 
    | ModuleOrNamespaceBinding of 
         /// This ModuleOrNamespace that represents the compilation of a module as a class. 
         /// The same set of tycons etc. are bound in the ModuleOrNamespace as in the ModuleOrNamespaceExpr
         ModuleOrNamespace * 
         /// This is the body of the module/namespace 
         ModuleOrNamespaceExpr


and TypedImplFile = TImplFile of QualifiedNameOfFile * ScopedPragma list * ModuleOrNamespaceExprWithSig * (* hasExplicitEntryPoint: *) bool * (* isScript *) bool

and TypedAssembly = TAssembly of TypedImplFile list

//---------------------------------------------------------------------------
// Freevars.  Computed and cached by later phases (never computed type checking).  Cached in terms. Not pickled.
//---------------------------------------------------------------------------

and FreeLocals = Zset<Val>
and FreeTypars = Zset<Typar>
and FreeTycons = Zset<Tycon>
and FreeRecdFields = Zset<RecdFieldRef>
and FreeUnionCases = Zset<UnionCaseRef>
and FreeTyvars = 
    { /// The summary of locally defined type definitions used in the expression. These may be made private by a signature 
      /// and we have to check various conditions associated with that. 
      FreeTycons: FreeTycons

      /// The summary of values used as trait solutions
      FreeTraitSolutions: FreeLocals
      
      /// The summary of type parameters used in the expression. These may not escape the enclosing generic construct 
      /// and we have to check various conditions associated with that. 
      FreeTypars: FreeTypars }


and FreeVarsCache = FreeVars cache

and FreeVars = 
    { /// The summary of locally defined variables used in the expression. These may be hidden at let bindings etc. 
      /// or made private by a signature or marked 'internal' or 'private', and we have to check various conditions associated with that. 
      FreeLocals: FreeLocals
      
      /// Indicates if the expression contains a call to a protected member or a base call. 
      /// Calls to protected members and direct calls to super classes can't escape, also code can't be inlined 
      UsesMethodLocalConstructs: bool 

      /// Indicates if the expression contains a call to rethrow that is not bound under a (try-)with branch. 
      /// Rethrow may only occur in such locations. 
      UsesUnboundRethrow: bool 

      /// The summary of locally defined tycon representations used in the expression. These may be made private by a signature 
      /// or marked 'internal' or 'private' and we have to check various conditions associated with that. 
      FreeLocalTyconReprs: FreeTycons 

      /// The summary of fields used in the expression. These may be made private by a signature 
      /// or marked 'internal' or 'private' and we have to check various conditions associated with that. 
      FreeRecdFields: FreeRecdFields
      
      /// The summary of union constructors used in the expression. These may be
      /// marked 'internal' or 'private' and we have to check various conditions associated with that.
      FreeUnionCases: FreeUnionCases
      
      /// See FreeTyvars above.
      FreeTyvars: FreeTyvars }

/// Specifies the compiled representations of type and exception definitions.  Basically
/// just an ILTypeRef. Computed and cached by later phases.  Stored in 
/// type and exception definitions. Not pickled. Store an optional ILType object for 
/// non-generic types.
and CompiledTypeRepr = 
    // An AbstractIL type representation that is just the name of a type 
    | TyrepNamed of 
         ILTypeRef * 
         ILBoxity * 
         /// For non-generic types, this next field stores the ILType corresponding to the first two elements of the case. This
         /// is simply to prevent reallocation of the ILType each time we need to generate it. For generic types, it is None.
         ILType option
         
    // An AbstractIL type representation that may include type variables
    // This case is only used for types defined in the F# library by their translation to ILASM types, e.g.
    //   type ``[]``<'T> = (# "!0[]" #)
    //   type ``[,]``<'T> = (# "!0[0 ...,0 ...]" #)
    //   type ``[,,]``<'T> = (# "!0[0 ...,0 ...,0 ...]" #)
    //   type byref<'T> = (# "!0&" #)
    //   type nativeptr<'T when 'T : unmanaged> = (# "native int" #)
    //   type ilsigptr<'T> = (# "!0*" #)
    | TyrepOpen of ILType  

//---------------------------------------------------------------------------
// Basic properties on type definitions
//---------------------------------------------------------------------------


/// Metadata on values (names of arguments etc. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ValReprInfo = 
    let unnamedTopArg1 : ArgReprInfo = { Attribs=[]; Name=None }
    let unnamedTopArg = [unnamedTopArg1]
    let unitArgData = [[]]
    let unnamedRetVal : ArgReprInfo = { Attribs = []; Name=None }
    let selfMetadata = unnamedTopArg
    let emptyValData = ValReprInfo([],[],unnamedRetVal)

    let InferTyparInfo (tps:Typar list) = tps |> List.map (fun tp -> TyparReprInfo(tp.Id, tp.Kind))
    let InferArgReprInfo (v:Val) : ArgReprInfo = { Attribs = []; Name= Some v.Id }
    let InferArgReprInfos (vs:Val list list) = ValReprInfo([],List.mapSquared InferArgReprInfo vs,unnamedRetVal)
    let HasNoArgs (ValReprInfo(n,args,_)) = n.IsEmpty && args.IsEmpty

//---------------------------------------------------------------------------
// Basic properties via functions (old style)
//---------------------------------------------------------------------------

let typeOfVal   (v:Val) = v.Type
let typesOfVals (v:Val list) = v |> List.map (fun v -> v.Type)
let nameOfVal   (v:Val) = v.LogicalName
let arityOfVal (v:Val) = (match v.ValReprInfo with None -> ValReprInfo.emptyValData | Some arities -> arities)

//---------------------------------------------------------------------------
// Aggregate operations to help transform the components that 
// make up the entire compilation unit
//---------------------------------------------------------------------------

let mapTImplFile   f   (TImplFile(fragName,pragmas,moduleExpr,hasExplicitEntryPoint,isScript)) = TImplFile(fragName, pragmas,f moduleExpr,hasExplicitEntryPoint,isScript)
let fmapTImplFile  f z (TImplFile(fragName,pragmas,moduleExpr,hasExplicitEntryPoint,isScript)) = let z,moduleExpr = f z moduleExpr in z,TImplFile(fragName,pragmas,moduleExpr,hasExplicitEntryPoint,isScript)
let mapAccImplFile f z (TImplFile(fragName,pragmas,moduleExpr,hasExplicitEntryPoint,isScript)) = let moduleExpr,z = f z moduleExpr in TImplFile(fragName,pragmas,moduleExpr,hasExplicitEntryPoint,isScript), z
let foldTImplFile  f z (TImplFile(_,_,moduleExpr,_,_)) = f z moduleExpr

//---------------------------------------------------------------------------
// Equality relations on locally defined things 
//---------------------------------------------------------------------------

let typarEq    (lv1:Typar) (lv2:Typar) = (lv1.Stamp = lv2.Stamp)

/// Equality on type varialbes, implemented as reference equality. This should be equivalent to using typarEq.
let typarRefEq (tp1: Typar) (tp2: Typar) = (tp1 === tp2)


/// Equality on value specs, implemented as reference equality
let valEq (lv1: Val) (lv2: Val) = (lv1 === lv2)

/// Equality on CCU references, implemented as reference equality except when unresolved
let ccuEq (mv1: CcuThunk) (mv2: CcuThunk) = 
    (mv1 === mv2) || 
    (if mv1.IsUnresolvedReference || mv2.IsUnresolvedReference then 
        mv1.AssemblyName = mv2.AssemblyName
     else 
        mv1.Contents === mv2.Contents)

/// For derefencing in the middle of a pattern
let (|ValDeref|) (vr :ValRef) = vr.Deref

//---------------------------------------------------------------------------
// Get information from refs
//---------------------------------------------------------------------------

exception InternalUndefinedTyconItem of (string * string -> int * string) * TyconRef * string

type UnionCaseRef with 
    member x.UnionCase = 
        let (UCRef(tcref,nm)) = x
        match tcref.GetUnionCaseByName nm with 
        | Some res -> res
        | None -> error (InternalUndefinedTyconItem (FSComp.SR.tastUndefinedTyconItemUnionCase, tcref, nm))
    member x.Attribs = x.UnionCase.Attribs
    member x.Range = x.UnionCase.Range
    member x.Index = 
        let (UCRef(tcref,id)) = x
        try 
            tcref.UnionCasesArray |> Array.findIndex (fun ucspec -> ucspec.DisplayName = id) 
        with :? KeyNotFoundException -> 
            error(InternalError(sprintf "union case %s not found in type %s" id tcref.LogicalName, tcref.Range))
    member x.AllFieldsAsList = x.UnionCase.FieldTable.AllFieldsAsList
    member x.ReturnType = x.UnionCase.ReturnType
    member x.FieldByIndex n = x.UnionCase.FieldTable.FieldByIndex n

type RecdFieldRef with 
    member x.RecdField = 
        let (RFRef(tcref,id)) = x
        match tcref.GetFieldByName id with 
        | Some res -> res
        | None -> error (InternalUndefinedTyconItem (FSComp.SR.tastUndefinedTyconItemField, tcref, id))
    member x.PropertyAttribs = x.RecdField.PropertyAttribs
    member x.Range = x.RecdField.Range

    member x.Index =
        let (RFRef(tcref,id)) = x
        try 
            tcref.AllFieldsArray |> Array.findIndex (fun rfspec -> rfspec.Name = id)  
        with :? KeyNotFoundException -> 
            error(InternalError(sprintf "field %s not found in type %s" id tcref.LogicalName, tcref.Range))

//--------------------------------------------------------------------------
// Make references to TAST items
//--------------------------------------------------------------------------

let mkRecdFieldRef tcref f = RFRef(tcref, f)
let mkUnionCaseRef tcref c = UCRef(tcref, c)


let ERefLocal x : EntityRef = { binding=x; nlr=Unchecked.defaultof<_> }      
let ERefNonLocal x : EntityRef = { binding=Unchecked.defaultof<_>; nlr=x }      
let ERefNonLocalPreResolved x xref : EntityRef = { binding=x; nlr=xref }      
let (|ERefLocal|ERefNonLocal|) (x: EntityRef) = 
    match box x.nlr with 
    | null -> ERefLocal x.binding
    | _ -> ERefNonLocal x.nlr

//--------------------------------------------------------------------------
// Construct local references
//-------------------------------------------------------------------------- 


let mkLocalTyconRef x = ERefLocal x
let mkNonLocalEntityRef ccu mp = NonLocalEntityRef(ccu,mp)
let mkNestedNonLocalEntityRef (nleref:NonLocalEntityRef) id = mkNonLocalEntityRef nleref.Ccu (Array.append nleref.Path [| id |])
let mkNonLocalTyconRef nleref id = ERefNonLocal (mkNestedNonLocalEntityRef nleref id)
let mkNonLocalTyconRefPreResolved x nleref id = ERefNonLocalPreResolved x (mkNestedNonLocalEntityRef nleref id)

let mkNestedUnionCaseRef tcref (uc: UnionCase) = mkUnionCaseRef tcref uc.Id.idText
let mkNestedRecdFieldRef tcref (rf: RecdField) = mkRecdFieldRef tcref rf.Name

type EntityRef with 
    
    member tcref.UnionCasesAsRefList         = tcref.UnionCasesAsList         |> List.map (mkNestedUnionCaseRef tcref)
    member tcref.TrueInstanceFieldsAsRefList = tcref.TrueInstanceFieldsAsList |> List.map (mkNestedRecdFieldRef tcref)
    member tcref.AllFieldAsRefList           = tcref.AllFieldsAsList          |> List.map (mkNestedRecdFieldRef tcref)

    member tcref.MkNestedTyconRef (x:Entity) : TyconRef  = 
        match tcref with 
        | ERefLocal _ -> mkLocalTyconRef x
        | ERefNonLocal nlr -> mkNonLocalTyconRefPreResolved x nlr x.LogicalName

    member tcref.MkNestedRecdFieldRef tycon (rf:Ident) = mkRecdFieldRef (tcref.MkNestedTyconRef tycon) rf.idText 

/// Make a reference to a union case for type in a module or namespace
let mkModuleUnionCaseRef (modref:ModuleOrNamespaceRef) tycon uc = 
    mkNestedUnionCaseRef (modref.MkNestedTyconRef tycon) uc

let VRefLocal    x : ValRef = { binding=x; nlr=Unchecked.defaultof<_> }      
let VRefNonLocal x : ValRef = { binding=Unchecked.defaultof<_>; nlr=x }      
let VRefNonLocalPreResolved x xref : ValRef = { binding=x; nlr=xref }      

let (|VRefLocal|VRefNonLocal|) (x: ValRef) = 
    match box x.nlr with 
    | null -> VRefLocal x.binding
    | _ -> VRefNonLocal x.nlr

let mkNonLocalValRef mp id = VRefNonLocal {EnclosingEntity = ERefNonLocal mp; ItemKey=id }
let mkNonLocalValRefPreResolved x mp id = VRefNonLocalPreResolved x {EnclosingEntity = ERefNonLocal mp; ItemKey=id }

let ccuOfValRef vref =  
    match vref with 
    | VRefLocal _ -> None
    | VRefNonLocal nlr -> Some nlr.Ccu

let ccuOfTyconRef eref =  
    match eref with 
    | ERefLocal _ -> None
    | ERefNonLocal nlr -> Some nlr.Ccu

//--------------------------------------------------------------------------
// Type parameters and inference unknowns
//-------------------------------------------------------------------------

let mkTyparTy (tp:Typar) = 
    match tp.Kind with 
    | KindType -> tp.AsType 
    | KindMeasure -> TType_measure (MeasureVar tp)

let copyTypar (tp: Typar) = let x = tp.Data in Typar.New { x with typar_stamp=newStamp() }
let copyTypars tps = List.map copyTypar tps

//--------------------------------------------------------------------------
// Inference variables
//-------------------------------------------------------------------------- 
    
let tryShortcutSolvedUnitPar canShortcut (r:Typar) = 
    if r.Kind = KindType then failwith "tryShortcutSolvedUnitPar: kind=type";
    match r.Solution with
    | Some (TType_measure unt) -> 
        if canShortcut then 
            match unt with 
            | MeasureVar r2 -> 
               match r2.Solution with
               | None -> ()
               | Some _ as soln -> 
                  r.Data.typar_solution <- soln
            | _ -> () 
        unt
    | _ -> 
        failwith "tryShortcutSolvedUnitPar: unsolved"
      
let rec stripUnitEqnsAux canShortcut unt = 
    match unt with 
    | MeasureVar r when r.IsSolved -> stripUnitEqnsAux canShortcut (tryShortcutSolvedUnitPar canShortcut r)
    | _ -> unt

let rec stripTyparEqnsAux canShortcut ty = 
    match ty with 
    | TType_var r -> 
        match r.Solution with
        | Some soln -> 
            if canShortcut then 
                match soln with 
                // We avoid shortcutting when there are additional constraints on the type variable we're trying to cut out
                // This is only because IterType likes to walk _all_ the constraints _everywhere_ in a type, including
                // those attached to _solved_ type variables. In an ideal world this would never be needed - see the notes
                // on IterType.
                | TType_var r2 when r2.Constraints.IsEmpty -> 
                   match r2.Solution with
                   | None -> ()
                   | Some _ as soln2 -> 
                      r.Data.typar_solution <- soln2
                | _ -> () 
            stripTyparEqnsAux canShortcut soln
        | None -> 
            ty
    | TType_measure unt -> 
        TType_measure (stripUnitEqnsAux canShortcut unt)
    | _ -> ty

let stripTyparEqns ty = stripTyparEqnsAux false ty
let stripUnitEqns unt = stripUnitEqnsAux false unt

//---------------------------------------------------------------------------
// These make local/non-local references to values according to whether
// the item is globally stable ("published") or not.
//---------------------------------------------------------------------------

let mkLocalValRef   (v:Val) = VRefLocal v
let mkLocalModRef (v:ModuleOrNamespace) = ERefLocal v
let mkLocalEntityRef  (v:Tycon) = ERefLocal v

let mkNonLocalCcuRootEntityRef ccu (x:Tycon) = mkNonLocalTyconRefPreResolved x (mkNonLocalEntityRef ccu [| |]) x.LogicalName

let mkNestedValRef  (cref:TyconRef) (v:Val) : ValRef = 
    match cref with 
    | ERefLocal _ -> mkLocalValRef v
    | ERefNonLocal nlr -> mkNonLocalValRefPreResolved v nlr v.LinkageFullKey



/// From Ref_private to Ref_nonlocal when exporting data.
let rescopePubPathToParent viewedCcu (PubPath(p)) = NonLocalEntityRef(viewedCcu, p.[0..p.Length-2])

/// From Ref_private to Ref_nonlocal when exporting data.
let rescopePubPath viewedCcu (PubPath(p)) = NonLocalEntityRef(viewedCcu,p)

//---------------------------------------------------------------------------
// Equality between TAST items.
//---------------------------------------------------------------------------

let valRefInThisAssembly compilingFslib (x: ValRef) = 
    match x with 
    | VRefLocal _ -> true
    | VRefNonLocal _ -> compilingFslib

let tyconRefUsesLocalXmlDoc compilingFslib (x: TyconRef) = 
    match x with 
    | ERefLocal _ -> true
    | ERefNonLocal _ -> compilingFslib
    
let entityRefInThisAssembly compilingFslib (x: EntityRef) = 
    match x with 
    | ERefLocal _ -> true
    | ERefNonLocal _ -> compilingFslib

let arrayPathEq (y1:string[]) (y2:string[]) =
    let len1 = y1.Length 
    let len2 = y2.Length 
    (len1 = len2) && 
    (let rec loop i = (i >= len1) || (y1.[i] = y2.[i] && loop (i+1)) 
     loop 0)

let nonLocalRefEq (NonLocalEntityRef(x1,y1) as smr1) (NonLocalEntityRef(x2,y2) as smr2) = 
    smr1 === smr2 || (ccuEq x1 x2 && arrayPathEq y1 y2)

/// This predicate tests if non-local resolution paths are definitely known to resolve
/// to different entities. All references with different named paths always resolve to 
/// different entities. Two references with the same named paths may resolve to the same 
/// entities even if they reference through different CCUs, because one reference
/// may be forwarded to another via a .NET TypeForwarder.
let nonLocalRefDefinitelyNotEq (NonLocalEntityRef(_,y1)) (NonLocalEntityRef(_,y2)) = 
    not (arrayPathEq y1 y2)

let pubPathEq (PubPath path1) (PubPath path2) = arrayPathEq path1 path2

let fslibRefEq (nlr1:NonLocalEntityRef) (PubPath(path2)) =
    arrayPathEq nlr1.Path path2

// Compare two EntityRef's for equality when compiling fslib (FSharp.Core.dll)
//
// Compiler-internal references to items in fslib are Ref_nonlocals even when compiling fslib.
// This breaks certain invariants that hold elsewhere, because they dereference to point to 
// Entity's from signatures rather than Entity's from implementations. This means backup, alternative 
// equality comparison techniques are needed when compiling fslib itself.
let fslibEntityRefEq fslibCcu (eref1:EntityRef) (eref2:EntityRef)   =
    match eref1,eref2 with 
    | (ERefNonLocal nlr1, ERefLocal x2)
    | (ERefLocal x2, ERefNonLocal nlr1) ->
        ccuEq nlr1.Ccu fslibCcu &&
        match x2.PublicPath with 
        | Some pp2 -> fslibRefEq nlr1 pp2
        | None -> false
    | (ERefLocal e1, ERefLocal e2) ->
        match e1.PublicPath , e2.PublicPath with 
        | Some pp1, Some pp2 -> pubPathEq pp1 pp2
        | _ -> false
    | _ -> false
  

// Compare two ValRef's for equality when compiling fslib (FSharp.Core.dll)
//
// Compiler-internal references to items in fslib are Ref_nonlocals even when compiling fslib.
// This breaks certain invariants that hold elsewhere, because they dereference to point to 
// Val's from signatures rather than Val's from implementations. This means backup, alternative 
// equality comparison techniques are needed when compiling fslib itself.
let fslibValRefEq fslibCcu vref1 vref2 =
    match vref1, vref2 with 
    | (VRefNonLocal nlr1, VRefLocal x2)
    | (VRefLocal x2, VRefNonLocal nlr1) ->
        ccuEq nlr1.Ccu fslibCcu &&
        match x2.PublicPath with 
        | Some (ValPubPath(pp2,nm2)) -> 
            // Note: this next line is just comparing the values by name, and not even the partial linkage data
            // This relies on the fact that the compiler doesn't use any references to
            // entities in fslib that are overloaded, or, if they are overloaded, then value identity
            // is not significant
            nlr1.ItemKey.PartialKey = nm2.PartialKey  &&
            fslibRefEq nlr1.EnclosingEntity.nlr pp2
        | None -> 
            false
    // Note: I suspect this private-to-private reference comparison is not needed
    | (VRefLocal e1, VRefLocal e2) ->
        match e1.PublicPath, e2.PublicPath with 
        | Some (ValPubPath(pp1,nm1)), Some (ValPubPath(pp2,nm2)) -> 
            pubPathEq pp1 pp2 && 
            (nm1 = nm2)
        | _ -> false
    | _ -> false
  
/// Primitive routine to compare two EntityRef's for equality
/// This takes into account the possibility that they may have type forwarders
let primEntityRefEq compilingFslib fslibCcu (x : EntityRef) (y : EntityRef) = 
    x === y ||
    match x.IsResolved,y.IsResolved with 
    | true, true when not compilingFslib -> x.ResolvedTarget === y.ResolvedTarget 
    | _ -> 
    match x.IsLocalRef,y.IsLocalRef with 
    | false, false when 
        (// Two tcrefs with identical paths are always equal
         nonLocalRefEq x.nlr y.nlr || 
         // The tcrefs may have forwarders. If they may possibly be equal then resolve them to get their canonical references
         // and compare those using pointer equality.
         (not (nonLocalRefDefinitelyNotEq x.nlr y.nlr) && x.Deref === y.Deref)) -> 
        true
    | _ -> 
        compilingFslib && fslibEntityRefEq  fslibCcu x y  

/// Primitive routine to compare two UnionCaseRef's for equality
let primUnionCaseRefEq compilingFslib fslibCcu (UCRef(tcr1,c1) as uc1) (UCRef(tcr2,c2) as uc2) = 
    uc1 === uc2 || (primEntityRefEq compilingFslib fslibCcu tcr1 tcr2 && c1 = c2)

/// Primitive routine to compare two ValRef's for equality.  On the whol value identity is not particularly
/// significant in F#. However it is significant for
///    (a) Active Patterns 
///    (b) detecting uses of "special known values" from FSharp.Core.dll, such as 'seq' 
///        and quotation splicing 
///
/// Note this routine doesn't take type forwarding into account
let primValRefEq compilingFslib fslibCcu (x : ValRef) (y : ValRef) =
    x === y ||
    match x.IsResolved,y.IsResolved with 
    | true, true when x.ResolvedTarget === y.ResolvedTarget -> true
    | _ -> 
    match x.IsLocalRef,y.IsLocalRef with 
    | true,true when valEq x.PrivateTarget y.PrivateTarget -> true
    | _ -> 
           (// Use TryDeref to guard against the platforms/times when certain F# language features aren't available,
            // e.g. CompactFramework doesn't have support for quotations.
            let v1 = x.TryDeref 
            let v2 = y.TryDeref
            v1.IsSome && v2.IsSome && v1.Value === v2.Value)
        || (if compilingFslib then fslibValRefEq fslibCcu x y else false)

//---------------------------------------------------------------------------
// pubpath/cpath 
//---------------------------------------------------------------------------

let stringOfAccess (TAccess paths) = String.concat ";" (List.map mangledTextOfCompPath paths)

let demangledPathOfCompPath (CompPath(_,path)) = 
    path |> List.map (fun (nm,k) -> Entity.DemangleEntityName nm k)

let demangledPathOfTyconRef (tcref:EntityRef) =
    demangledPathOfCompPath tcref.CompilationPath

let fullCompPathOfModuleOrNamespace (m:ModuleOrNamespace) = 
    let (CompPath(scoref,cpath))  = m.CompilationPath
    CompPath(scoref,cpath@[(m.LogicalName, m.ModuleOrNamespaceType.ModuleOrNamespaceKind)])

// Can cpath2 be accessed given a right to access cpath1. That is, is cpath2 a nested type or namespace of cpath1. Note order of arguments.
let canAccessCompPathFrom (CompPath(scoref1,cpath1)) (CompPath(scoref2,cpath2)) =
    let rec loop p1 p2  = 
        match p1,p2 with 
        | (a1,k1)::rest1, (a2,k2)::rest2 -> (a1=a2) && (k1=k2) && loop rest1 rest2
        | [],_ -> true 
        | _ -> false // cpath1 is longer
    loop cpath1 cpath2 &&
    (scoref1 = scoref2)

let canAccessFromOneOf cpaths cpathTest =
    cpaths |> List.exists (fun cpath -> canAccessCompPathFrom cpath cpathTest) 

let canAccessFrom (TAccess x) cpath = 
    x |> List.forall (fun cpath1 -> canAccessCompPathFrom cpath1 cpath)

let canAccessFromEverywhere (TAccess x) = x.IsEmpty
let canAccessFromSomewhere (TAccess _) = true
let isLessAccessible (TAccess aa) (TAccess bb)  = 
    not (aa |> List.forall(fun a -> bb |> List.exists (fun b -> canAccessCompPathFrom a b)))

/// Given (newPath,oldPath) replace oldPath by newPath in the TAccess.
let accessSubstPaths (newPath,oldPath) (TAccess paths) =
    let subst cpath = if cpath=oldPath then newPath else cpath
    TAccess (List.map subst paths)

let compPathOfCcu (ccu:CcuThunk) = CompPath(ccu.ILScopeRef,[]) 
let taccessPublic = TAccess []
let combineAccess (TAccess a1) (TAccess a2) = TAccess(a1@a2)

//---------------------------------------------------------------------------
// Construct TAST nodes
//---------------------------------------------------------------------------

let NewFreeVarsCache() = newCache ()

let MakeUnionCasesTable ucs = 
    { CasesByIndex = Array.ofList ucs 
      CasesByName = NameMap.ofKeyedList (fun uc -> uc.DisplayName) ucs }
                                                                  
let MakeRecdFieldsTable ucs = 
    { FieldsByIndex = Array.ofList ucs 
      FieldsByName = ucs  |> NameMap.ofKeyedList (fun rfld -> rfld.Name) }
                                                                  

let MakeUnionCases ucs = 
    { CasesTable=MakeUnionCasesTable ucs 
      CompiledRepresentation=newCache() }

let MakeUnionRepr ucs = TFiniteUnionRepr (MakeUnionCases ucs)

let NewTypar (kind,rigid,Typar(id,staticReq,isCompGen),isFromError,dynamicReq,attribs,eqDep,compDep) = 
    Typar.New
      { typar_id = id 
        typar_il_name = None
        typar_stamp = newStamp() 
        typar_flags= TyparFlags(kind,rigid,isFromError,isCompGen,staticReq,dynamicReq,eqDep,compDep) 
        typar_attribs= attribs 
        typar_solution = None
        typar_constraints=[]
        typar_xmldoc = XmlDoc.Empty } 

let NewRigidTypar nm m = NewTypar (KindType,TyparRigid,Typar(mkSynId m nm,NoStaticReq,true),false,DynamicReq,[],false,false)

let NewUnionCase id nm tys rty attribs docOption access = 
    { Id=id
      CompiledName=nm
      XmlDoc=docOption
      XmlDocSig=""
      Accessibility=access
      FieldTable = MakeRecdFieldsTable tys
      ReturnType = rty
      Attribs=attribs } 

let NewModuleOrNamespaceType mkind tycons vals = 
    ModuleOrNamespaceType(mkind, QueueList.ofList vals, QueueList.ofList tycons)

let NewEmptyModuleOrNamespaceType mkind = NewModuleOrNamespaceType mkind [] []

let NewExn cpath (id:Ident) access repr attribs doc = 
    Tycon.New "exnc"
      { entity_stamp=newStamp()
        entity_attribs=attribs
        entity_kind=KindType
        entity_logical_name=id.idText
        entity_compiled_name=None
        entity_range=id.idRange
        entity_exn_info= repr
        entity_tycon_tcaug=TyconAugmentation.Create()
        entity_xmldoc=doc
        entity_xmldocsig=""
        entity_pubpath=cpath |> Option.map (publicPathOfCompPath id)
        entity_accessiblity=access
        entity_tycon_repr_accessibility=access
        entity_modul_contents = notlazy (NewEmptyModuleOrNamespaceType FSharpModule)
        entity_cpath= cpath
        entity_typars=LazyWithContext<_,_>.NotLazy []
        entity_tycon_abbrev = None
        entity_tycon_repr = None
        entity_flags=EntityFlags(usesPrefixDisplay=false, isModuleOrNamespace=false, preEstablishedHasDefaultCtor=false, hasSelfReferentialCtor=false)
        entity_il_repr_cache= newCache()   
        } 

let NewRecdField  stat konst id ty isMutable isVolatile pattribs fattribs docOption access secret =
    { rfield_mutable=isMutable
      rfield_pattribs=pattribs
      rfield_fattribs=fattribs
      rfield_type=ty
      rfield_static=stat
      rfield_volatile=isVolatile
      rfield_const=konst
      rfield_access = access
      rfield_secret = secret
      rfield_xmldoc = docOption 
      rfield_xmldocsig = ""
      rfield_id=id }

    
let NewTyconImpl (cpath, nm, m, access, reprAccess, kind, typars, docOption, usesPrefixDisplay, preEstablishedHasDefaultCtor, hasSelfReferentialCtor, mtyp) =
    let stamp = newStamp() 
    Tycon.New "tycon"
      { entity_stamp=stamp
        entity_logical_name=nm
        entity_compiled_name=None
        entity_kind=kind
        entity_range=m
        entity_flags=EntityFlags(usesPrefixDisplay=usesPrefixDisplay, isModuleOrNamespace=false,preEstablishedHasDefaultCtor=preEstablishedHasDefaultCtor, hasSelfReferentialCtor=hasSelfReferentialCtor)
        entity_attribs=[] // fixed up after
        entity_typars=typars
        entity_tycon_abbrev = None
        entity_tycon_repr = None
        entity_tycon_repr_accessibility = reprAccess
        entity_exn_info=TExnNone
        entity_tycon_tcaug=TyconAugmentation.Create()
        entity_modul_contents = mtyp
        entity_accessiblity=access
        entity_xmldoc = docOption
        entity_xmldocsig=""        
        entity_pubpath=cpath |> Option.map (publicPathOfCompPath (mkSynId m nm))
        entity_cpath = cpath
        entity_il_repr_cache = newCache() 
        } 
    
let NewTycon (cpath, nm, m, access, reprAccess, kind, typars, docOption, usesPrefixDisplay, preEstablishedHasDefaultCtor, hasSelfReferentialCtor, mtyp) =
    let stamp = newStamp() 
    Tycon.New "tycon"
      { entity_stamp=stamp
        entity_logical_name=nm
        entity_compiled_name=None
        entity_kind=kind
        entity_range=m
        entity_flags=EntityFlags(usesPrefixDisplay=usesPrefixDisplay, isModuleOrNamespace=false,preEstablishedHasDefaultCtor=preEstablishedHasDefaultCtor, hasSelfReferentialCtor=hasSelfReferentialCtor)
        entity_attribs=[] // fixed up after
        entity_typars=typars
        entity_tycon_abbrev = None
        entity_tycon_repr = None
        entity_tycon_repr_accessibility = reprAccess
        entity_exn_info=TExnNone
        entity_tycon_tcaug=TyconAugmentation.Create()
        entity_modul_contents = mtyp
        entity_accessiblity=access
        entity_xmldoc = docOption
        entity_xmldocsig=""        
        entity_pubpath=cpath |> Option.map (publicPathOfCompPath (mkSynId m nm))
        entity_cpath = cpath
        entity_il_repr_cache = newCache() 
        } 


let NewILTycon nlpath (nm,m) tps (scoref:ILScopeRef, enc, tdef:ILTypeDef) mtyp =

    // NOTE: hasSelfReferentialCtor=false is an assumption about mscorlib
    let hasSelfReferentialCtor = tdef.IsClass && (not scoref.IsAssemblyRef && scoref.AssemblyRef.Name = "mscorlib")
    let tycon = NewTycon(nlpath, nm, m, taccessPublic, taccessPublic, KindType, tps, XmlDoc.Empty, true, false, hasSelfReferentialCtor, mtyp)

    tycon.Data.entity_tycon_repr <- Some (TILObjModelRepr (scoref,enc,tdef))
    tycon.TypeContents.tcaug_closed <- true
    tycon

exception Duplicate of string * string * range
exception NameClash of string * string * string * range * string * string * range
exception FullAbstraction of string * range

let NewModuleOrNamespace cpath access (id:Ident) xml attribs mtype = Construct.NewModuleOrNamespace cpath access id xml attribs mtype

let NewVal (logicalName:string,m:range,compiledName,ty,isMutable,isCompGen,arity,access,recValInfo,specialRepr,baseOrThis,attribs,inlineInfo,doc,isModuleOrMemberBinding,isExtensionMember,isIncrClassSpecialMember,isTyFunc,allowTypeInst,konst,actualParent) : Val = 
    let stamp = newStamp() 
#if DEBUG
    if !verboseStamps then dprintf "NewVal, %s#%d\n" logicalName stamp
#endif
    Val.New
        { val_stamp = stamp
          val_logical_name=logicalName
          val_compiled_name= (match compiledName with Some v when v <> logicalName -> compiledName | _ -> None)
          val_range=m
          val_defn_range=m
          val_defn=None
          val_repr_info= arity
          val_actual_parent= actualParent
          val_flags = ValFlags(recValInfo,baseOrThis,isCompGen,inlineInfo,isMutable,isModuleOrMemberBinding,isExtensionMember,isIncrClassSpecialMember,isTyFunc,allowTypeInst)
          val_const= konst
          val_access=access
          val_member_info=specialRepr
          val_attribs=attribs
          val_type = ty
          val_xmldoc = doc
          val_xmldocsig = ""} 


let NewCcuContents sref m nm mty =
    NewModuleOrNamespace (Some(CompPath(sref,[]))) taccessPublic (ident(nm,m)) XmlDoc.Empty [] (notlazy mty)
      

//--------------------------------------------------------------------------
// Cloning and adjusting
//--------------------------------------------------------------------------
 
/// Create a tycon based on an existing one using the function 'f'. 
/// We require that we be given the new parent for the new tycon. 
/// We pass the new tycon to 'f' in case it needs to reparent the 
/// contents of the tycon. 
let NewModifiedTycon f (orig:Tycon) = 
    let stamp = newStamp() 
    let data = orig.Data 
#if DEBUG
    if !verboseStamps then dprintf "NewModifiedTycon, %s#%d, based on %s#%d\n" orig.LogicalName stamp orig.LogicalName data.entity_stamp
#endif
    Tycon.New "NewModifiedTycon" (f { data with entity_stamp=stamp; }) 
    
/// Create a module Tycon based on an existing one using the function 'f'. 
/// We require that we be given the parent for the new module. 
/// We pass the new module to 'f' in case it needs to reparent the 
/// contents of the module. 
let NewModifiedModuleOrNamespace f orig = 
    orig |> NewModifiedTycon (fun d -> 
        { d with entity_modul_contents = notlazy (f (d.entity_modul_contents.Force())) }) 

/// Create a Val based on an existing one using the function 'f'. 
/// We require that we be given the parent for the new Val. 
let NewModifiedVal f (orig:Val) = 
    let data = orig.Data
    let stamp = newStamp() 
#if DEBUG
    if !verboseStamps then dprintf "NewModifiedVal, stamp #%d, based on stamp #%d\n" stamp data.val_stamp
#endif
    let data' = f { data with val_stamp=stamp }
    Val.New data'

let NewClonedModuleOrNamespace orig =  NewModifiedModuleOrNamespace (fun mty -> mty) orig
let NewClonedTycon orig =  NewModifiedTycon (fun d -> d) orig

//------------------------------------------------------------------------------

/// Combine two maps where the given function reconciles entries that have the same key
let private combineMaps f m1 m2 = 
    Map.foldBack (fun k v acc -> Map.add k (if Map.containsKey k m2 then f [v;Map.find k m2] else f [v]) acc) m1 
      (Map.foldBack (fun k v acc -> if Map.containsKey k m1 then acc else Map.add k (f [v]) acc) m2 Map.empty)

let private combineMultiMaps f (m1: MultiMap<_,_>) (m2: MultiMap<_,_>) = 
    Map.foldBack (fun k v acc -> List.foldBack (MultiMap.add k) (if Map.containsKey k m2 then f [v;Map.find k m2] else f [v]) acc) m1 
      (Map.foldBack (fun k v acc -> if Map.containsKey k m1 then acc else List.foldBack (MultiMap.add k) (f [v]) acc) m2 MultiMap.empty)


/// Combine module types when multiple namespace fragments contribute to the
/// same namespace, making new module specs as we go.
let rec private combineModuleOrNamespaceTypes path m (mty1:ModuleOrNamespaceType)  (mty2:ModuleOrNamespaceType)  = 
    match mty1.ModuleOrNamespaceKind,mty2.ModuleOrNamespaceKind  with 
    | Namespace,Namespace -> 
        let kind = mty1.ModuleOrNamespaceKind
        let entities = 
            (mty1.AllEntitiesByLogicalMangledName,mty2.AllEntitiesByLogicalMangledName) 
            ||>  combineMaps (combineEntityList path) 

        let vals = QueueList.append mty1.AllValsAndMembers mty2.AllValsAndMembers

        new ModuleOrNamespaceType(kind, vals, QueueList.ofList (NameMap.range entities))

    | Namespace, _ | _,Namespace -> 
        error(Error(FSComp.SR.tastNamespaceAndModuleWithSameNameInAssembly(textOfPath path),m))

    | _-> 
        error(Error(FSComp.SR.tastTwoModulesWithSameNameInAssembly(textOfPath path),m))

and private combineEntityList path l = 
    match l with
    | h :: t -> List.fold (combineEntites path) h t
    | _ -> failwith "combineEntityList"

and private combineEntites path (entity1:Entity) (entity2:Entity) = 

    match entity1.IsModuleOrNamespace, entity2.IsModuleOrNamespace with
    | true,true -> 
        entity1 |> NewModifiedTycon (fun data1 -> 
                    { data1 with 
                         entity_xmldoc = XmlDoc.Merge entity1.XmlDoc entity2.XmlDoc
                         entity_attribs = entity1.Attribs @ entity2.Attribs
                         entity_modul_contents=lazy (combineModuleOrNamespaceTypes (path@[entity2.DemangledModuleOrNamespaceName]) entity2.Range entity1.ModuleOrNamespaceType entity2.ModuleOrNamespaceType); }) 
    | false,false -> 
        error(Error(FSComp.SR.tastDuplicateTypeDefinitionInAssembly(entity2.LogicalName, textOfPath path),entity2.Range))
    | _,_ -> 
        error(Error(FSComp.SR.tastConflictingModuleAndTypeDefinitionInAssembly(entity2.LogicalName, textOfPath path),entity2.Range))
    
and combineModuleOrNamespaceTypeList path m l = 
    match l with
    | h :: t -> List.fold (combineModuleOrNamespaceTypes path m) h t
    | _ -> failwith "combineModuleOrNamespaceTypeList"

//--------------------------------------------------------------------------
// Resource format for pickled data
//--------------------------------------------------------------------------

let FSharpOptimizationDataResourceName = "FSharpOptimizationData"
let FSharpSignatureDataResourceName = "FSharpSignatureData"


