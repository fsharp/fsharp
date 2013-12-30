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
  
module internal Microsoft.FSharp.Compiler.Tast 

open System
open System.Collections.Generic 
open System.Reflection
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

#if EXTENSIONTYPING
open Microsoft.FSharp.Compiler.ExtensionTyping
open Microsoft.FSharp.Core.CompilerServices
#endif

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

/// A global generator of compiler generated names
// ++GLOBAL MUTABLE STATE
let globalNng = NiceNameGenerator()

/// A global generator of stable compiler generated names
// ++GLOBAL MUTABLE STATE
let globalStableNameGenerator = StableNiceNameGenerator ()

type StampMap<'T> = Map<Stamp,'T>

//-------------------------------------------------------------------------
// Flags

[<RequireQualifiedAccess>]
type ValInline =
    /// Indicates the value must always be inlined and no .NET IL code is generated for the value/function
    | PseudoVal
    /// Indicates the value is inlined but the .NET IL code for the function still exists, e.g. to satisfy interfaces on objects, but that it is also always inlined 
    | Always
    /// Indicates the value may optionally be inlined by the optimizer
    | Optional
    /// Indicates the value must never be inlined by the optimizer
    | Never

/// Returns true if the implementation of a value must always be inlined
let mustinline = function ValInline.PseudoVal | ValInline.Always -> true | ValInline.Optional | ValInline.Never -> false

/// A flag associated with values that indicates whether the recursive scope of the value is currently being processed, and 
/// if the value has been generalized or not as yet.
type ValRecursiveScopeInfo =
    /// Set while the value is within its recursive scope. The flag indicates if the value has been eagerly generalized and accepts generic-recursive calls 
    | ValInRecScope of bool
    /// The normal value for this flag when the value is not within its recursive scope 
    | ValNotInRecScope

type ValMutability   = 
    | Immutable 
    | Mutable 

[<RequireQualifiedAccess>]
/// Indicates if a type parameter is needed at runtime and may not be eliminated
type TyparDynamicReq = 
    /// Indicates the type parameter is not needed at runtime and may be eliminated
    | No 
    /// Indicates the type parameter is needed at runtime and may not be eliminated
    | Yes

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

    new (recValInfo, baseOrThis, isCompGen, inlineInfo, isMutable, isModuleOrMemberBinding, isExtensionMember, isIncrClassSpecialMember, isTyFunc, allowTypeInst, isGeneratedEventVal) =
        let flags = 
                     (match baseOrThis with
                                        | BaseVal ->                         0b000000000000000000L
                                        | CtorThisVal ->                     0b000000000000000010L
                                        | NormalVal ->                       0b000000000000000100L
                                        | MemberThisVal ->                   0b000000000000000110L) |||
                     (if isCompGen then                                      0b000000000000001000L 
                      else                                                   0b000000000000000000L) |||
                     (match inlineInfo with
                                        | ValInline.PseudoVal ->             0b000000000000000000L
                                        | ValInline.Always ->                0b000000000000010000L
                                        | ValInline.Optional ->              0b000000000000100000L
                                        | ValInline.Never ->                 0b000000000000110000L) |||
                     (match isMutable with
                                        | Immutable ->                       0b000000000000000000L
                                        | Mutable   ->                       0b000000000001000000L) |||

                     (match isModuleOrMemberBinding with
                                        | false     ->                       0b000000000000000000L
                                        | true      ->                       0b000000000010000000L) |||
                     (match isExtensionMember with
                                        | false     ->                       0b000000000000000000L
                                        | true      ->                       0b000000000100000000L) |||
                     (match isIncrClassSpecialMember with
                                        | false     ->                       0b000000000000000000L
                                        | true      ->                       0b000000001000000000L) |||
                     (match isTyFunc with
                                        | false     ->                       0b000000000000000000L
                                        | true      ->                       0b000000010000000000L) |||

                     (match recValInfo with
                                     | ValNotInRecScope     ->               0b000000000000000000L
                                     | ValInRecScope true   ->               0b000000100000000000L
                                     | ValInRecScope false  ->               0b000001000000000000L) |||

                     (match allowTypeInst with
                                        | false     ->                       0b000000000000000000L
                                        | true      ->                       0b000100000000000000L) |||

                     (match isGeneratedEventVal with
                                        | false     ->                       0b000000000000000000L
                                        | true      ->                       0b100000000000000000L)                                        

        ValFlags(flags)

    member x.BaseOrThisInfo = 
                                  match (flags       &&&                     0b000000000000000110L) with 
                                                             |               0b000000000000000000L -> BaseVal
                                                             |               0b000000000000000010L -> CtorThisVal
                                                             |               0b000000000000000100L -> NormalVal
                                                             |               0b000000000000000110L -> MemberThisVal
                                                             | _          -> failwith "unreachable"



    member x.IsCompilerGenerated =      (flags       &&&                     0b000000000000001000L) <> 0x0L

    member x.SetIsCompilerGenerated(isCompGen) = 
            let flags =                 (flags       &&&                  ~~~0b000000000000001000L) |||
                                        (match isCompGen with
                                          | false           ->               0b000000000000000000L
                                          | true            ->               0b000000000000001000L)
            ValFlags(flags)

    member x.InlineInfo = 
                                  match (flags       &&&                     0b000000000000110000L) with 
                                                             |               0b000000000000000000L -> ValInline.PseudoVal
                                                             |               0b000000000000010000L -> ValInline.Always
                                                             |               0b000000000000100000L -> ValInline.Optional
                                                             |               0b000000000000110000L -> ValInline.Never
                                                             | _          -> failwith "unreachable"

    member x.MutabilityInfo = 
                                  match (flags       &&&                     0b000000000001000000L) with 
                                                             |               0b000000000000000000L -> Immutable
                                                             |               0b000000000001000000L -> Mutable
                                                             | _          -> failwith "unreachable"


    member x.IsMemberOrModuleBinding = 
                                  match (flags       &&&                     0b000000000010000000L) with 
                                                             |               0b000000000000000000L -> false
                                                             |               0b000000000010000000L -> true
                                                             | _          -> failwith "unreachable"


    member x.SetIsMemberOrModuleBinding = ValFlags(flags |||                 0b000000000010000000L)


    member x.IsExtensionMember        = (flags       &&&                     0b000000000100000000L) <> 0L
    member x.IsIncrClassSpecialMember = (flags       &&&                     0b000000001000000000L) <> 0L
    member x.IsTypeFunction           = (flags       &&&                     0b000000010000000000L) <> 0L

    member x.RecursiveValInfo =   match (flags       &&&                     0b000001100000000000L) with 
                                                             |               0b000000000000000000L -> ValNotInRecScope
                                                             |               0b000000100000000000L -> ValInRecScope(true)
                                                             |               0b000001000000000000L -> ValInRecScope(false)
                                                             | _                   -> failwith "unreachable"

    member x.SetRecursiveValInfo(recValInfo) = 
            let flags = 
                     (flags       &&&                                     ~~~0b000001100000000000L) |||
                     (match recValInfo with
                                     | ValNotInRecScope     ->               0b000000000000000000L
                                     | ValInRecScope(true)  ->               0b000000100000000000L
                                     | ValInRecScope(false) ->               0b000001000000000000L) 
            ValFlags(flags)

    member x.MakesNoCriticalTailcalls         =                   (flags &&& 0b000010000000000000L) <> 0L

    member x.SetMakesNoCriticalTailcalls =                ValFlags(flags ||| 0b000010000000000000L)

    member x.PermitsExplicitTypeInstantiation =                   (flags &&& 0b000100000000000000L) <> 0L
    member x.HasBeenReferenced                =                   (flags &&& 0b001000000000000000L) <> 0L

    member x.SetHasBeenReferenced                     =   ValFlags(flags ||| 0b001000000000000000L)

    member x.IsCompiledAsStaticPropertyWithoutField =             (flags &&& 0b010000000000000000L) <> 0L

    member x.SetIsCompiledAsStaticPropertyWithoutField =  ValFlags(flags ||| 0b010000000000000000L)
    
    member x.IsGeneratedEventVal =                                (flags &&& 0b100000000000000000L) <> 0L
    /// Get the flags as included in the F# binary metadata
    member x.PickledBits = 
        // Clear the RecursiveValInfo, only used during inference and irrelevant across assembly boundaries
        // Clear the IsCompiledAsStaticPropertyWithoutField, only used to determine whether to use a true field for a value, and to eliminate the optimization info for observable bindings
        // Clear the HasBeenReferenced, only used to report "unreferenced variable" warnings and to help collect 'it' values in FSI.EXE
        // Clear the IsGeneratedEventVal, since there's no use in propagating specialname information for generated add/remove event vals
                                                      (flags       &&&    ~~~0b011001100000000000L) 

[<RequireQualifiedAccess>]
type TyparKind = 
    | Type 
    | Measure
    member x.AttrName =
      match x with
      | TyparKind.Type -> None
      | TyparKind.Measure -> Some "Measure"
    override x.ToString() = 
      match x with
      | TyparKind.Type -> "type"
      | TyparKind.Measure -> "measure"

[<RequireQualifiedAccess>]
/// Indicates if the type variable can be sovled or given new constraints. The status of a type variable
/// evolves towards being either rigid or solved. 
type TyparRigidity = 
    /// Indicates the type parameter can't be solved
    | Rigid 
    /// Indicates the type parameter can't be solved, but the variable is not set to "rigid" until after inference is complete
    | WillBeRigid 
    /// Indicates we give a warning if the type parameter is ever solved
    | WarnIfNotRigid 
    /// Indicates the type parameter is an inference variable may be solved
    | Flexible
    /// Indicates the type parameter derives from an '_' anonymous type
    /// For units-of-measure, we give a warning if this gets solved to '1'
    | Anon
    member x.ErrorIfUnified = match x with TyparRigidity.Rigid -> true | _ -> false
    member x.WarnIfUnified = match x with TyparRigidity.WillBeRigid | TyparRigidity.WarnIfNotRigid -> true | _ -> false
    member x.WarnIfMissingConstraint = match x with TyparRigidity.WillBeRigid -> true | _ -> false


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
                     | TyparRigidity.Rigid          -> 0b000000000000
                     | TyparRigidity.WillBeRigid    -> 0b000000100000
                     | TyparRigidity.WarnIfNotRigid -> 0b000001000000
                     | TyparRigidity.Flexible       -> 0b000001100000
                     | TyparRigidity.Anon           -> 0b000010000000) |||
                   (match kind with
                     | TyparKind.Type            -> 0b000000000000
                     | TyparKind.Measure         -> 0b000100000000) |||
                   (if comparisonDependsOn then 
                                              0b001000000000 else 0) |||
                   (match dynamicReq with
                     | TyparDynamicReq.No        -> 0b000000000000
                     | TyparDynamicReq.Yes          -> 0b010000000000) |||
                   (if equalityDependsOn then 
                                              0b100000000000 else 0))

    /// Indicates if the type inference variable was generated after an error when type checking expressions or patterns
    member x.IsFromError         = (flags &&& 0b000000000010) <> 0x0
    /// Indicates if the type variable is compiler generated, i.e. is an implicit type inference variable 
    member x.IsCompilerGenerated = (flags &&& 0b000000000100) <> 0x0
    /// Indicates if the type variable has a static "head type" requirement, i.e. ^a variables used in FSharp.Core and member constraints.
    member x.StaticReq           = 
                             match (flags &&& 0b000000001000) with 
                                            | 0b000000000000 -> NoStaticReq
                                            | 0b000000001000 -> HeadTypeStaticReq
                                            | _             -> failwith "unreachable"

    /// Indicates if the type variable can be sovled or given new constraints. The status of a type variable
    /// generally always evolves towards being either rigid or solved. 
    member x.Rigidity = 
                             match (flags &&& 0b000011100000) with 
                                            | 0b000000000000 -> TyparRigidity.Rigid
                                            | 0b000000100000 -> TyparRigidity.WillBeRigid
                                            | 0b000001000000 -> TyparRigidity.WarnIfNotRigid
                                            | 0b000001100000 -> TyparRigidity.Flexible
                                            | 0b000010000000 -> TyparRigidity.Anon
                                            | _          -> failwith "unreachable"

    /// Indicates whether a type variable can be instantiated by types or units-of-measure.
    member x.Kind           = 
                             match (flags &&& 0b000100000000) with 
                                            | 0b000000000000 -> TyparKind.Type
                                            | 0b000100000000 -> TyparKind.Measure
                                            | _             -> failwith "unreachable"


    /// Indicates that whether or not a generic type definition satisfies the comparison constraint is dependent on whether this type variable satisfies the comparison constraint.
    member x.ComparisonConditionalOn =
                                   (flags &&& 0b001000000000) <> 0x0
    /// Indicates if a type parameter is needed at runtime and may not be eliminated
    member x.DynamicReq     = 
                             match (flags &&& 0b010000000000) with 
                                            | 0b000000000000 -> TyparDynamicReq.No
                                            | 0b010000000000 -> TyparDynamicReq.Yes
                                            | _             -> failwith "unreachable"
    /// Indicates that whether or not a generic type definition satisfies the equality constraint is dependent on whether this type variable satisfies the equality constraint.
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

    /// This bit is reserved for us in the pickle format, see pickle.fs, it's bing listed here to stop it ever being used for anything else
    static member ReservedBitForPickleFormatTyconReprFlag   =           0b00000010000L

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

let KeyTyconByDemangledNameAndArity nm (typars: _ list) x = 
    KeyValuePair(NameArityPair(DemangleGenericTypeName nm, typars.Length), x)

/// Generic types can be accessed either by 'List' or 'List`1'. This lists both keys. The second form should really be deprecated.
let KeyTyconByAccessNames nm x = 
    if IsMangledGenericName nm then 
        let dnm = DemangleGenericTypeName nm 
        [| KeyValuePair(nm,x); KeyValuePair(dnm,x) |]
    else
        [| KeyValuePair(nm,x) |]

type ModuleOrNamespaceKind = 
    /// Indicates that a module is compiled to a class with the "Module" suffix added. 
    | FSharpModuleWithSuffix 
    /// Indicates that a module is compiled to a class with the same name as the original module 
    | ModuleOrType 
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

#if EXTENSIONTYPING
let definitionLocationOfProvidedItem (p : Tainted<#IProvidedCustomAttributeProvider>) =
    let attrs = p.PUntaintNoFailure(fun x -> x.GetDefinitionLocationAttribute(p.TypeProvider.PUntaintNoFailure(id)))
    match attrs with
    | None | Some (null, _, _) -> None
    | Some (filePath, line, column) -> 
        // Coordinates from type provider are 1-based for lines and columns
        // Coordinates internally in the F# compiler are 1-based for lines and 0-based for columns
        let pos = Range.mkPos line (max 0 (column - 1)) 
        Range.mkRange  filePath pos pos |> Some
    
#endif

// Type definitions, exception definitions, module definitions and
// namespace definitions are all 'entities'. These have too much in common to make it 
// worth factoring them out as separate types.
[<RequireQualifiedAccess>] 
type Entity = 
    { mutable Data: EntityData }
    /// The name of the namespace, module or type, possibly with mangling, e.g. List`1, List or FailureException 
    member x.LogicalName = x.Data.entity_logical_name
    /// The compiled name of the namespace, module or type, e.g. FSharpList`1, ListModule or FailureException 
    member x.CompiledName = match x.Data.entity_compiled_name with None -> x.LogicalName | Some s -> s
    /// The display name of the namespace, module or type, e.g. List instead of List`1, and no static parameters
    member x.DisplayName = x.GetDisplayName(false, false)
    /// The display name of the namespace, module or type with <_,_,_> added for generic types, plus static parameters if any
    member x.DisplayNameWithStaticParametersAndUnderscoreTypars = x.GetDisplayName(true, true)

    /// The display name of the namespace, module or type, e.g. List instead of List`1, including static parameters if any
    member x.DisplayNameWithStaticParameters = x.GetDisplayName(true, false)

    member x.GetDisplayName(withStaticParameters, withUnderscoreTypars) = 
        let nm = x.LogicalName
#if EXTENSIONTYPING
        if x.IsProvidedErasedTycon then 
            let nm,args = PrettyNaming.demangleProvidedTypeName nm
            if withStaticParameters && args.Length > 0 then 
                nm + "<" + String.concat "," (Array.map snd args) + ">"
            else
                nm
        else
#else
        if false then nm 
        else
#endif
            match x.TyparsNoRange with 
            | [] -> nm
            | tps -> 
                let nm = DemangleGenericTypeName nm
                if withUnderscoreTypars && tps.Length > 0 then 
                    nm + "<" + String.concat "," (Array.create tps.Length "_") + ">"
                else
                    nm


    /// The code location where the module, namespace or type is defined.
    member x.Range = 
#if EXTENSIONTYPING    
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info ->
            match definitionLocationOfProvidedItem info.ProvidedType with
            |   Some range -> range
            |   None -> x.Data.entity_range
        | _ -> 
#endif
        x.Data.entity_range

    /// A unique stamp for this module, namespace or type definition within the context of this compilation. 
    /// Note that because of signatures, there are situations where in a single compilation the "same" 
    /// module, namespace or type may have two distinct Entity objects that have distinct stamps.
    member x.Stamp = x.Data.entity_stamp

    /// The F#-defined custom attributes of the entity, if any. If the entity is backed by Abstract IL or provided metadata
    /// then this does not include any attributes from those sources.
    member x.Attribs = x.Data.entity_attribs

    /// The XML documentation of the entity, if any. If the entity is backed by provided metadata
    /// then this _does_ include this documentation. If the entity is backed by Abstract IL metadata
    /// or comes from another F# assembly then it does not (because the documentation will get read from 
    /// an XML file).
    member x.XmlDoc = 
#if EXTENSIONTYPING
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> XmlDoc (info.ProvidedType.PUntaintNoFailure(fun st -> (st :> IProvidedCustomAttributeProvider).GetXmlDocAttributes(info.ProvidedType.TypeProvider.PUntaintNoFailure(id))))
        | _ -> 
#endif
        x.Data.entity_xmldoc

    /// The XML documentation sig-string of the entity, if any, to use to lookup an .xml doc file. This also acts
    /// as a cache for this sig-string computation.
    member x.XmlDocSig 
        with get() = x.Data.entity_xmldocsig
        and set v = x.Data.entity_xmldocsig <- v

    /// The logical contents of the entity when it is a module or namespace fragment.
    member x.ModuleOrNamespaceType = x.Data.entity_modul_contents.Force()

    /// The logical contents of the entity when it is a type definition.
    member x.TypeContents = x.Data.entity_tycon_tcaug

    /// The kind of the type definition - is it a measure definition or a type definition?
    member x.TypeOrMeasureKind = x.Data.entity_kind

    /// The identifier at the point of declaration of the type definition.
    member x.Id = ident(x.LogicalName, x.Range)

    /// The information about the r.h.s. of a type definition, if any. For example, the r.h.s. of a union or record type.
    member x.TypeReprInfo = x.Data.entity_tycon_repr

    /// The information about the r.h.s. of an F# exception definition, if any. 
    member x.ExceptionInfo = x.Data.entity_exn_info

    /// Indicates if the entity represents an F# exception declaration.
    member x.IsExceptionDecl = match x.ExceptionInfo with TExnNone -> false | _ -> true

    /// String 'Module' off an F# module name, if FSharpModuleWithSuffix is used
    static member DemangleEntityName nm k =  
        match k with 
        | FSharpModuleWithSuffix -> String.dropSuffix nm FSharpModuleSuffix
        | _ -> nm

    /// Demangle the module name, if FSharpModuleWithSuffix is used
    member x.DemangledModuleOrNamespaceName =  
          Entity.DemangleEntityName x.LogicalName x.ModuleOrNamespaceType.ModuleOrNamespaceKind
    
    /// Get the type parameters for an entity that is a type declaration, otherwise return the empty list.
    /// 
    /// Lazy because it may read metadata, must provide a context "range" in case error occurs reading metadata.
    member x.Typars m = x.Data.entity_typars.Force m

    /// Get the type parameters for an entity that is a type declaration, otherwise return the empty list.
    member x.TyparsNoRange = x.Typars x.Range

    /// Get the type abbreviated by this type definition, if it is an F# type abbreviation definition
    member x.TypeAbbrev = x.Data.entity_tycon_abbrev

    /// Indicates if this entity is an F# type abbreviation definition
    member x.IsTypeAbbrev = x.TypeAbbrev.IsSome

    /// Get the value representing the accessiblity of the r.h.s. of an F# type definition.
    member x.TypeReprAccessibility = x.Data.entity_tycon_repr_accessibility

    /// Get the cache of the compiled ILTypeRef representation of this module or type.
    member x.CompiledReprCache = x.Data.entity_il_repr_cache

    /// Get a blob of data indicating how this type is nested in other namespaces, modules or types.
    member x.PublicPath = x.Data.entity_pubpath

    /// Get the value representing the accessiblity of an F# type definition or module.
    member x.Accessibility = x.Data.entity_accessiblity

    /// Indicates the type prefers the "tycon<a,b>" syntax for display etc. 
    member x.IsPrefixDisplay = x.Data.entity_flags.IsPrefixDisplay

    /// Indicates the "tycon blob" is actually a module 
    member x.IsModuleOrNamespace = x.Data.entity_flags.IsModuleOrNamespace

    /// Indicates if the entity is a namespace
    member x.IsNamespace = x.IsModuleOrNamespace && (match x.ModuleOrNamespaceType.ModuleOrNamespaceKind with Namespace -> true | _ -> false)

    /// Indicates if the entity is an F# module definition
    member x.IsModule = x.IsModuleOrNamespace && (match x.ModuleOrNamespaceType.ModuleOrNamespaceKind with Namespace -> false | _ -> true)
#if EXTENSIONTYPING

    /// Indicates if the entity is a provided type or namespace definition
    member x.IsProvided = 
        match x.TypeReprInfo with 
        | TProvidedTypeExtensionPoint _ -> true
        | TProvidedNamespaceExtensionPoint _ -> true
        | _ -> false

    /// Indicates if the entity is a provided namespace fragment
    member x.IsProvidedNamespace = 
        match x.TypeReprInfo with 
        | TProvidedNamespaceExtensionPoint _ -> true
        | _ -> false

    /// Indicates if the entity is an erased provided type definition
    member x.IsProvidedErasedTycon = 
        match x.TypeReprInfo with 
        | TProvidedTypeExtensionPoint info -> info.IsErased
        | _ -> false

    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon = 
        match x.TypeReprInfo with 
        | TProvidedTypeExtensionPoint info -> info.IsGenerated
        | _ -> false
#endif

    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased = 
        x.IsMeasureableReprTycon 
#if EXTENSIONTYPING
        || x.IsProvidedErasedTycon
#endif

    /// Get a blob of data indicating how this type is nested inside other namespaces, modules and types.
    member x.CompilationPathOpt = x.Data.entity_cpath 

    /// Get a blob of data indicating how this type is nested inside other namespaces, modules and types.
    member x.CompilationPath = 
        match x.CompilationPathOpt with 
        | Some cpath -> cpath 
        | None -> error(Error(FSComp.SR.tastTypeOrModuleNotConcrete(x.LogicalName),x.Range))
    
    /// Get a table of fields for all the F#-defined record, struct and class fields in this type definition, including
    /// static fields, 'val' declarations and hidden fields from the compilation of implicit class constructions.
    member x.AllFieldTable = 
        match x.TypeReprInfo with 
        | TRecdRepr x | TFsObjModelRepr {fsobjmodel_rfields=x} -> x
        |  _ -> 
        match x.ExceptionInfo with 
        | TExnFresh x -> x
        | _ -> 
        { FieldsByIndex = [| |] 
          FieldsByName = NameMap.empty }

    /// Get an array of fields for all the F#-defined record, struct and class fields in this type definition, including
    /// static fields, 'val' declarations and hidden fields from the compilation of implicit class constructions.
    member x.AllFieldsArray = x.AllFieldTable.FieldsByIndex

    /// Get a list of fields for all the F#-defined record, struct and class fields in this type definition, including
    /// static fields, 'val' declarations and hidden fields from the compilation of implicit class constructions.
    member x.AllFieldsAsList = x.AllFieldsArray |> Array.toList

    /// Get a list of all instance fields for F#-defined record, struct and class fields in this type definition.
    /// including hidden fields from the compilation of implicit class constructions.

    // NOTE: This method doesn't perform particularly well, and is over-used, but doesn't seem to appear on performance traces
    member x.AllInstanceFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsStatic)

    /// Get a list of all fields for F#-defined record, struct and class fields in this type definition,
    /// including static fields, but excluding compiler-generate fields.
    member x.TrueFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsCompilerGenerated)

    /// Get a list of all instance fields for F#-defined record, struct and class fields in this type definition,
    /// excluding compiler-generate fields.
    member x.TrueInstanceFieldsAsList = x.AllFieldsAsList |> List.filter (fun f -> not f.IsStatic && not f.IsCompilerGenerated)

    /// Get a field by index in definition order
    member x.GetFieldByIndex n = x.AllFieldTable.FieldByIndex n

    /// Get a field by name.
    member x.GetFieldByName n = x.AllFieldTable.FieldByName n

    /// Indicate if this is a type whose r.h.s. is known to be a union type definition.
    member x.IsUnionTycon = match x.TypeReprInfo with | TFiniteUnionRepr _ -> true |  _ -> false

    /// Get the union cases and other union-type information for a type, if any
    member x.UnionTypeInfo = 
        match x.TypeReprInfo with 
        | TFiniteUnionRepr x -> Some x 
        |  _ -> None

    /// Get the union cases for a type, if any
    member x.UnionCasesArray = 
        match x.UnionTypeInfo with 
        | Some x -> x.CasesTable.CasesByIndex 
        | None -> [| |] 

    /// Get the union cases for a type, if any, as a list
    member x.UnionCasesAsList = x.UnionCasesArray |> Array.toList

    /// Get a union case of a type by name
    member x.GetUnionCaseByName n =
        match x.UnionTypeInfo with 
        | Some x  -> NameMap.tryFind n x.CasesTable.CasesByName
        | None -> None

    
    // OSGN support
    /// Create a new entity with empty, unlinked data. Only used during unpickling of F# metadata.
    static member NewUnlinked() : Entity = { Data = nullableSlotEmpty() }
    /// Create a new entity with the given backing data. Only used during unpickling of F# metadata.
    static member New reason (data: EntityData) : Entity  = 
#if DEBUG
        if !verboseStamps then 
            dprintf "entity %s#%d (%s)\n" data.entity_logical_name data.entity_stamp reason
#else
        ignore(reason)
#endif
        { Data = data }
    /// Link an entity based on empty, unlinked data to the given data. Only used during unpickling of F# metadata.
    member x.Link tg = x.Data <- nullableSlotFull(tg)
    /// Indicates if the entity is linked to backing data. Only used during unpickling of F# metadata.
    member x.IsLinked = match box x.Data with null -> false | _ -> true 

    override x.ToString() = x.LogicalName

    /// Get the blob of information associated with an F# object-model type definition, i.e. class, interface, struct etc.
    member x.FSharpObjectModelTypeInfo = 
         match x.TypeReprInfo with 
         | TFsObjModelRepr x -> x 
         |  _ -> assert false; failwith "not an F# object model type definition"

    /// Indicate if this is a type definition backed by Abstract IL metadata.
    member x.IsILTycon = match x.TypeReprInfo with | TILObjModelRepr _ -> true |  _ -> false

    /// Get the Abstract IL scope, nesting and metadata for this 
    /// type definition, assuming it is backed by Abstract IL metadata.
    member x.ILTyconInfo = match x.TypeReprInfo with | TILObjModelRepr (a,b,c) -> (a,b,c) |  _ -> assert false; failwith "not a .NET type definition"

    /// Get the Abstract IL metadata for this type definition, assuming it is backed by Abstract IL metadata.
    member x.ILTyconRawMetadata = let _,_,td = x.ILTyconInfo in td

    /// Indicates if this is an F# type definition whose r.h.s. is known to be a record type definition.
    member x.IsRecordTycon = match x.TypeReprInfo with | TRecdRepr _ -> true |  _ -> false

    /// Indicates if this is an F# type definition whose r.h.s. is known to be some kind of F# object model definition
    member x.IsFSharpObjectModelTycon = match x.TypeReprInfo with | TFsObjModelRepr _ -> true |  _ -> false

    /// Indicates if this is an F# type definition which is one of the special types in FSharp.Core.dll which uses 
    /// an assembly-code representation for the type, e.g. the primitive array type constructor.
    member x.IsAsmReprTycon = match x.TypeReprInfo with | TAsmRepr _ -> true |  _ -> false

    /// Indicates if this is an F# type definition which is one of the special types in FSharp.Core.dll like 'float<_>' which
    /// defines a measure type with a relation to an existing non-measure type as a representation.
    member x.IsMeasureableReprTycon = match x.TypeReprInfo with | TMeasureableRepr _ -> true |  _ -> false

    /// Indicates if this is an F# type definition whose r.h.s. definition is unknown (i.e. a traditional ML 'abstract' type in a signature,
    /// which in F# is called a 'unknown representation' type).
    member x.IsHiddenReprTycon = match x.TypeAbbrev,x.TypeReprInfo with | None,TNoRepr -> true |  _ -> false

    /// Indicates if this is an F#-defined interface type definition 
    member x.IsFSharpInterfaceTycon = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconInterface -> true | _ -> false

    /// Indicates if this is an F#-defined delegate type definition 
    member x.IsFSharpDelegateTycon  = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconDelegate _ -> true | _ -> false

    /// Indicates if this is an F#-defined enum type definition 
    member x.IsFSharpEnumTycon      = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconEnum -> true | _ -> false

    /// Indicates if this is an F#-defined class type definition 
    member x.IsFSharpClassTycon     = x.IsFSharpObjectModelTycon && match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with TTyconClass -> true | _ -> false

    /// Indicates if this is a .NET-defined enum type definition 
    member x.IsILEnumTycon          = x.IsILTycon && x.ILTyconRawMetadata.IsEnum

    /// Indicates if this is an enum type definition 
    member x.IsEnumTycon            = 
#if EXTENSIONTYPING
        match x.TypeReprInfo with 
        | TProvidedTypeExtensionPoint info -> info.IsEnum 
        | TProvidedNamespaceExtensionPoint _ -> false
        | _ ->
#endif
        x.IsILEnumTycon || x.IsFSharpEnumTycon


    /// Indicates if this is an F#-defined struct or enum type definition , i.e. a value type definition
    member x.IsFSharpStructOrEnumTycon =
        x.IsFSharpObjectModelTycon &&
        match x.FSharpObjectModelTypeInfo.fsobjmodel_kind with 
        | TTyconClass | TTyconInterface   | TTyconDelegate _ -> false
        | TTyconStruct | TTyconEnum -> true

    /// Indicates if this is a .NET-defined struct or enum type definition , i.e. a value type definition
    member x.IsILStructOrEnumTycon =
        x.IsILTycon && 
        match x.ILTyconRawMetadata.tdKind with
        | ILTypeDefKind.ValueType | ILTypeDefKind.Enum -> true
        | _ -> false

    /// Indicates if this is a struct or enum type definition , i.e. a value type definition
    member x.IsStructOrEnumTycon = 
#if EXTENSIONTYPING
        match x.TypeReprInfo with 
        | TProvidedTypeExtensionPoint info -> info.IsStructOrEnum 
        | TProvidedNamespaceExtensionPoint _ -> false
        | _ ->
#endif
        x.IsILStructOrEnumTycon || x.IsFSharpStructOrEnumTycon

    /// Gets the immediate interface definitions of an F# type definition. Further interfaces may be supported through class and interface inheritance.
    member x.ImmediateInterfacesOfFSharpTycon =
        x.TypeContents.tcaug_interfaces

    /// Gets the immediate interface types of an F# type definition. Further interfaces may be supported through class and interface inheritance.
    member x.ImmediateInterfaceTypesOfFSharpTycon =
        x.ImmediateInterfacesOfFSharpTycon |> List.map (fun (x,_,_) -> x)

    /// Gets the immediate members of an F# type definition, excluding compiler-generated ones.
    /// Note: result is alphabetically sorted, then for each name the results are in declaration order
    member x.MembersOfFSharpTyconSorted =
        x.TypeContents.tcaug_adhoc 
        |> NameMultiMap.rangeReversingEachBucket 
        |> List.filter (fun v -> not v.IsCompilerGenerated)

    /// Gets all immediate members of an F# type definition keyed by name, including compiler-generated ones.
    /// Note: result is a indexed table, and for each name the results are in reverse declaration order
    member x.MembersOfFSharpTyconByName =
        x.TypeContents.tcaug_adhoc 

    /// Gets any implicit hash/equals (with comparer argument) methods added to an F# record, union or struct type definition.
    member x.GeneratedHashAndEqualsWithComparerValues = x.TypeContents.tcaug_hash_and_equals_withc 
    /// Gets any implicit CompareTo (with comparer argument) methods added to an F# record, union or struct type definition.
    member x.GeneratedCompareToWithComparerValues = x.TypeContents.tcaug_compare_withc
    /// Gets any implicit CompareTo methods added to an F# record, union or struct type definition.
    member x.GeneratedCompareToValues = x.TypeContents.tcaug_compare
    /// Gets any implicit hash/equals methods added to an F# record, union or struct type definition.
    member x.GeneratedHashAndEqualsValues = x.TypeContents.tcaug_equals
    /// Gets all implicit hash/equals/compare methods added to an F# record, union or struct type definition.
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
    

    /// Gets the data indicating the compiled representation of a type or module in terms of Abstract IL data structures.
    member x.CompiledRepresentation =
#if EXTENSIONTYPING
        match x.TypeReprInfo with 
        // We should never be computing this property for erased types
        | TProvidedTypeExtensionPoint info when info.IsErased -> 
            failwith "No compiled representation for provided erased type"
        
        // Generated types that are not relocated just point straight to the generated backing assembly, computed from "st".
        // These are used when running F# Interactive, which does not use static linking of provider-generated assemblies,
        // and also for types with relocation suppressed.
        | TProvidedTypeExtensionPoint info when info.IsGenerated && info.IsSuppressRelocate -> 
            let st = info.ProvidedType
            let tref = ExtensionTyping.GetILTypeRefOfProvidedType (st, x.Range)
            let boxity = if x.IsStructOrEnumTycon then AsValue else AsObject
            CompiledTypeRepr.ILAsmNamed(tref, boxity, None)
        | TProvidedNamespaceExtensionPoint _ -> failwith "No compiled representation for provided namespace"
        | _ ->
#endif
            let ilTypeRefForCompilationPath (CompPath(sref,p)) item = 
                let rec top racc  p = 
                    match p with 
                    | [] -> ILTypeRef.Create(sref,[],textOfPath  (List.rev (item::racc)))
                    | (h,istype)::t -> 
                        match istype with 
                        | FSharpModuleWithSuffix | ModuleOrType -> 
                            let outerTypeName = (textOfPath (List.rev (h::racc)))
                            ILTypeRef.Create(sref, (outerTypeName :: List.map (fun (nm,_) -> nm) t),item)
                        | _ -> 
                          top (h::racc) t
                top [] p 


            cached x.CompiledReprCache (fun () -> 
                match x.ExceptionInfo with 
                | TExnAbbrevRepr ecref2 -> ecref2.CompiledRepresentation
                | TExnAsmRepr tref -> CompiledTypeRepr.ILAsmNamed(tref, AsObject, Some (mkILTy AsObject (mkILTySpec (tref,[]))))
                | _ -> 
                match x.TypeReprInfo with 
                | TAsmRepr typ -> CompiledTypeRepr.ILAsmOpen typ
                | _ -> 
                    let boxity = if x.IsStructOrEnumTycon then AsValue else AsObject
                    let ilTypeRef = 
                        match x.TypeReprInfo with 
                        | TILObjModelRepr (ilScopeRef,ilEnclosingTypeDefs,ilTypeDef) -> IL.mkRefForNestedILTypeDef ilScopeRef (ilEnclosingTypeDefs, ilTypeDef)
                        | _ -> ilTypeRefForCompilationPath x.CompilationPath x.CompiledName
                    // Pre-allocate a ILType for monomorphic types, to reduce memory usage from Abstract IL nodes
                    let ilTypeOpt = 
                        match x.TyparsNoRange with 
                        | [] -> Some (mkILTy boxity (mkILTySpec (ilTypeRef,[]))) 
                        | _ -> None 
                    CompiledTypeRepr.ILAsmNamed (ilTypeRef, boxity, ilTypeOpt))

    /// Gets the data indicating the compiled representation of a named type or module in terms of Abstract IL data structures.
    member x.CompiledRepresentationForNamedType =
        match x.CompiledRepresentation with 
        | CompiledTypeRepr.ILAsmNamed(tref, _, _) -> tref
        | CompiledTypeRepr.ILAsmOpen _ -> invalidOp (FSComp.SR.tastTypeHasAssemblyCodeRepresentation(x.DisplayNameWithStaticParametersAndUnderscoreTypars))


    /// Indicates if we have pre-determined that a type definition has a default constructor.
    member x.PreEstablishedHasDefaultConstructor = x.Data.entity_flags.PreEstablishedHasDefaultConstructor

    /// Indicates if we have pre-determined that a type definition has a self-referential constructor using 'as x'
    member x.HasSelfReferentialConstructor = x.Data.entity_flags.HasSelfReferentialConstructor

    /// Set the custom attributes on an F# type definition.
    member x.SetAttribs attribs = x.Data.entity_attribs <- attribs



and 
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
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
      // MUTABILITY; used only during creation and remapping of tycons 
      mutable entity_tycon_repr: TyconRepresentation 

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
      // REVIEW: it looks like entity_cpath subsumes this 
      entity_pubpath : PublicPath option 

      /// Indicates how visible is the entitiy is.
      entity_accessiblity: Accessibility   
 
      /// The stable path to the type, e.g. Microsoft.FSharp.Core.FSharpFunc`2 
      entity_cpath : CompilationPath option 

      /// Used during codegen to hold the ILX representation indicating how to access the type 
      entity_il_repr_cache : CompiledTypeRepr cache
    }

and ParentRef = 
    | Parent of EntityRef
    | ParentNone
    
and 
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
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
    /// The information for the contents of a type. Also used for a provided namespace.
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

#if EXTENSIONTYPING
    /// TProvidedTypeExtensionPoint
    ///
    /// Indicates the representation information for a provided type. 
    | TProvidedTypeExtensionPoint of TProvidedTypeInfo

    /// Indicates the representation information for a provided namespace.  
    //
    // Note, the list could probably be a list of IProvidedNamespace rather than ITypeProvider
    | TProvidedNamespaceExtensionPoint of ExtensionTyping.ResolutionEnvironment * Tainted<ITypeProvider> list
#endif

    /// The 'NoRepr' value here has four meanings: 
    ///     (1) it indicates 'not yet known' during the first 2 phases of establishing type definitions
    ///     (2) it indicates 'no representation', i.e. 'type X' in signatures
    ///     (3) it is the setting used for exception definitions (!)
    ///     (4) it is the setting used for modules and namespaces.
    /// 
    /// It would be better to separate the "not yet known" and other cases out.
    /// The information for exception definitions should be folded into here.
    | TNoRepr

#if EXTENSIONTYPING
and 
   [< RequireQualifiedAccess; NoComparison; NoEquality>]
   
   /// The information kept about a provided type
   TProvidedTypeInfo = 
   { /// The parameters given to the provider that provided to this type.
     ResolutionEnvironment : ExtensionTyping.ResolutionEnvironment
     /// The underlying System.Type (wrapped as a ProvidedType to make sure we don't call random things on
     /// System.Type, and wrapped as Tainted to make sure we track which provider this came from, for reporting
     /// error messages)
     ProvidedType:  Tainted<ProvidedType>
     /// The base type of the type. We use it to compute the compiled representation of the type for erased types.
     /// Reading is delayed, since it does an import on the underlying type
     LazyBaseType: LazyWithContext<TType, range * TType> 
     /// A flag read eagerly from the provided type and used to compute basic properties of the type definition.
     IsClass:  bool 
     /// A flag read eagerly from the provided type and used to compute basic properties of the type definition.
     IsSealed:  bool 
     /// A flag read eagerly from the provided type and used to compute basic properties of the type definition.
     IsInterface:  bool 
     /// A flag read eagerly from the provided type and used to compute basic properties of the type definition.
     IsStructOrEnum: bool 
     /// A flag read eagerly from the provided type and used to compute basic properties of the type definition.
     IsEnum: bool 
     /// A type read from the provided type and used to compute basic properties of the type definition.
     /// Reading is delayed, since it does an import on the underlying type
     UnderlyingTypeOfEnum: (unit -> TType) 
     /// A flag read from the provided type and used to compute basic properties of the type definition.
     /// Reading is delayed, since it looks at the .BaseType
     IsDelegate: (unit -> bool) 
     IsErased: bool 
     /// Indicates the type is generated, but type-relocation is suppressed
     IsSuppressRelocate : bool }

   member info.IsGenerated = not info.IsErased
   member info.BaseTypeForErased (m,objTy) = 
       if info.IsErased then info.LazyBaseType.Force (m,objTy) 
       else assert false; failwith "expect erased type"

#endif

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
    
    member x.IsValueType =
        match x with 
        | TTyconClass | TTyconInterface   | TTyconDelegate _ -> false
        | TTyconStruct | TTyconEnum -> true

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
      let activePatternElemRefCache         : NameMap<ActivePatternElemRef> option ref = ref None
      let modulesByDemangledNameCache       : NameMap<ModuleOrNamespace>    option ref = ref None
      let exconsByDemangledNameCache        : NameMap<Tycon>                option ref = ref None
      let tyconsByDemangledNameAndArityCache: LayeredMap<NameArityPair, Tycon>     option ref = ref None
      let tyconsByAccessNamesCache          : LayeredMultiMap<string,Tycon> option ref = ref None
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

#if EXTENSIONTYPING
      /// Mutation used in hosting scenarios to hold the hosted types in this module or namespace
      member mtyp.AddProvidedTypeEntity(entity:Entity) = 
          entities <- QueueList.appendOne entities entity
          tyconsByMangledNameCache := None          
          tyconsByDemangledNameAndArityCache := None
          tyconsByAccessNamesCache := None
          allEntitiesByMangledNameCache := None             
#endif 
          
      /// Return a new module or namespace type with an entity added.
      member mtyp.AddEntity(tycon:Tycon) = 
          ModuleOrNamespaceType(kind, vals, entities.AppendOne tycon)
          
      /// Return a new module or namespace type with a value added.
      member mtyp.AddVal(vspec:Val) = 
          ModuleOrNamespaceType(kind, vals.AppendOne vspec, entities)
          
      /// Get a table of the active patterns defined in this module.
      member mtyp.ActivePatternElemRefLookupTable = activePatternElemRefCache
  
      /// Get a list of types defined within this module, namespace or type. 
      member mtyp.TypeDefinitions               = entities |> Seq.filter (fun x -> not x.IsExceptionDecl && not x.IsModuleOrNamespace) |> Seq.toList

      /// Get a list of F# exception definitions defined within this module, namespace or type. 
      member mtyp.ExceptionDefinitions          = entities |> Seq.filter (fun x -> x.IsExceptionDecl) |> Seq.toList

      /// Get a list of module and namespace definitions defined within this module, namespace or type. 
      member mtyp.ModuleAndNamespaceDefinitions = entities |> Seq.filter (fun x -> x.IsModuleOrNamespace) |> Seq.toList

      /// Get a list of type and exception definitions defined within this module, namespace or type. 
      member mtyp.TypeAndExceptionDefinitions   = entities |> Seq.filter (fun x -> not x.IsModuleOrNamespace) |> Seq.toList

      /// Get a table of types defined within this module, namespace or type. The 
      /// table is indexed by both name and generic arity. This means that for generic 
      /// types "List`1", the entry (List,1) will be present.
      member mtyp.TypesByDemangledNameAndArity m = 
        cacheOptRef tyconsByDemangledNameAndArityCache (fun () -> 
           LayeredMap.Empty.AddAndMarkAsCollapsible( mtyp.TypeAndExceptionDefinitions |> List.map (fun (tc:Tycon) -> KeyTyconByDemangledNameAndArity tc.LogicalName (tc.Typars m) tc)  |> List.toArray))

      /// Get a table of types defined within this module, namespace or type. The 
      /// table is indexed by both name and, for generic types, also by mangled name.
      member mtyp.TypesByAccessNames = 
          cacheOptRef tyconsByAccessNamesCache (fun () -> 
             LayeredMultiMap.Empty.AddAndMarkAsCollapsible  (mtyp.TypeAndExceptionDefinitions |> List.toArray |> Array.collect (fun (tc:Tycon) -> KeyTyconByAccessNames tc.LogicalName tc)))

      // REVIEW: we can remove this lookup and use AllEntitiedByMangledName instead?
      member mtyp.TypesByMangledName = 
          let addTyconByMangledName (x:Tycon) tab = NameMap.add x.LogicalName x tab 
          cacheOptRef tyconsByMangledNameCache (fun () -> 
             List.foldBack addTyconByMangledName mtyp.TypeAndExceptionDefinitions  Map.empty)

      /// Get a table of entities indexed by both logical and compiled names
      member mtyp.AllEntitiesByCompiledAndLogicalMangledNames : NameMap<Entity> = 
          let addEntityByMangledName (x:Entity) tab = 
              let name1 = x.LogicalName
              let name2 = x.CompiledName
              let tab = NameMap.add name1 x tab 
              if name1 = name2 then tab
              else NameMap.add name2 x tab 
          
          cacheOptRef allEntitiesByMangledNameCache (fun () -> 
             QueueList.foldBack addEntityByMangledName entities  Map.empty)

      /// Get a table of entities indexed by both logical name
      member mtyp.AllEntitiesByLogicalMangledName : NameMap<Entity> = 
          let addEntityByMangledName (x:Entity) tab = NameMap.add x.LogicalName x tab 
          QueueList.foldBack addEntityByMangledName entities  Map.empty

      /// Get a table of values and members indexed by partial linkage key, which includes name, the mangled name of the parent type (if any), 
      /// and the method argument count (if any).
      member mtyp.AllValsAndMembersByPartialLinkageKey = 
          let addValByMangledName (x:Val) tab = 
             if x.IsCompiledAsTopLevel then
                 MultiMap.add x.LinkagePartialKey x tab 
             else
                 tab
          cacheOptRef allValsAndMembersByPartialLinkageKeyCache (fun () -> 
             QueueList.foldBack addValByMangledName vals MultiMap.empty)

      /// Try to find the member with the given linkage key in the given module.
      member mtyp.TryLinkVal(ccu:CcuThunk,key:ValLinkageFullKey) = 
          mtyp.AllValsAndMembersByPartialLinkageKey
            |> MultiMap.find key.PartialKey
            |> List.tryFind (fun v -> match key.TypeForLinkage with 
                                      | None -> true
                                      | Some keyTy -> ccu.MemberSignatureEquality(keyTy,v.Type))

      /// Get a table of values indexed by logical name
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

      /// Compute a table of values and members indexed by logical name.
      member mtyp.AllValsAndMembersByLogicalNameUncached = 
          let addValByName (x:Val) tab = 
             if not x.IsCompilerGenerated then 
                 MultiMap.add x.LogicalName x tab 
             else
                 tab
          QueueList.foldBack addValByName vals MultiMap.empty

      /// Get a table of F# exception definitions indexed by demangled name, so 'FailureException' is indexed by 'Failure'
      member mtyp.ExceptionDefinitionsByDemangledName = 
          let add (tycon:Tycon) acc = NameMap.add tycon.LogicalName tycon acc
          cacheOptRef exconsByDemangledNameCache (fun () -> 
             List.foldBack add mtyp.ExceptionDefinitions  Map.empty)

      /// Get a table of nested module and namespace fragments indexed by demangled name (so 'ListModule' becomes 'List')
      member mtyp.ModulesAndNamespacesByDemangledName = 
          let add (entity:Entity) acc = 
              if entity.IsModuleOrNamespace then 
                  NameMap.add entity.DemangledModuleOrNamespaceName entity acc
              else acc
          cacheOptRef modulesByDemangledNameCache (fun () -> 
             QueueList.foldBack add entities  Map.empty)

and ModuleOrNamespace = Entity 
and Tycon = Entity 


/// A set of static methods for constructing types.
and Construct = 
      
    static member NewModuleOrNamespaceType mkind tycons vals = ModuleOrNamespaceType(mkind, QueueList.ofList vals, QueueList.ofList tycons)
    static member NewEmptyModuleOrNamespaceType mkind = Construct.NewModuleOrNamespaceType mkind [] []

#if EXTENSIONTYPING

    static member NewProvidedTyconRepr(resolutionEnvironment,st:Tainted<ProvidedType>,importProvidedType,isSuppressRelocate,m) = 

        let isErased = st.PUntaint((fun st -> st.IsErased),m)

        let lazyBaseTy = 
            LazyWithContext.Create 
                ((fun (m,objTy) -> 
                      let baseSystemTy = st.PApplyOption((fun st -> match st.BaseType with null -> None | ty -> Some ty), m)
                      match baseSystemTy with 
                      | None -> objTy 
                      | Some t -> importProvidedType t), 
                  ErrorLogger.findOriginalException)

        TProvidedTypeExtensionPoint 
            { ResolutionEnvironment=resolutionEnvironment
              ProvidedType=st
              LazyBaseType=lazyBaseTy
              UnderlyingTypeOfEnum = (fun () -> importProvidedType (st.PApply((fun st -> st.GetEnumUnderlyingType()),m)))
              IsDelegate = (fun () -> st.PUntaint((fun st -> 
                                   let baseType = st.BaseType 
                                   match baseType with 
                                   | null -> false
                                   | x when x.IsGenericType -> false
                                   | x when x.DeclaringType <> null -> false
                                   | x -> x.FullName = "System.Delegate" || x.FullName = "System.MulticastDelegate"), m))
              IsEnum = st.PUntaint((fun st -> st.IsEnum), m)
              IsStructOrEnum = st.PUntaint((fun st -> st.IsValueType || st.IsEnum), m)
              IsInterface = st.PUntaint((fun st -> st.IsInterface), m)
              IsSealed = st.PUntaint((fun st -> st.IsSealed), m)
              IsClass = st.PUntaint((fun st -> st.IsClass), m)
              IsErased = isErased
              IsSuppressRelocate = isSuppressRelocate }

    static member NewProvidedTycon(resolutionEnvironment, st:Tainted<ProvidedType>, importProvidedType, isSuppressRelocate, m, ?access, ?cpath) = 
        let stamp = newStamp() 
        let name = st.PUntaint((fun st -> st.Name), m)
        let id = ident (name,m)
        let kind = 
            let isMeasure = 
                st.PApplyWithProvider((fun (st,provider) -> 
                    let findAttrib (ty:System.Type) (a:CustomAttributeData) = (a.Constructor.DeclaringType.FullName = ty.FullName)  
#if FX_NO_CUSTOMATTRIBUTEDATA
                    provider.GetMemberCustomAttributesData(st.RawSystemType) 
#else
                    ignore provider
                    st.RawSystemType.GetCustomAttributesData()
#endif
                        |> Seq.exists (findAttrib typeof<Microsoft.FSharp.Core.MeasureAttribute>)), m)
                  .PUntaintNoFailure(fun x -> x)
            if isMeasure then TyparKind.Measure else TyparKind.Type

        let access = 
            match access with 
            | Some a -> a 
            | None -> TAccess []
        let cpath =  
            match cpath with 
            | None -> 
                let ilScopeRef = st.TypeProviderAssemblyRef
                let enclosingName = ExtensionTyping.GetFSharpPathToProvidedType(st,m)
                CompPath(ilScopeRef,enclosingName |> List.map(fun id->id,ModuleOrNamespaceKind.Namespace))
            | Some p -> p
        let pubpath = publicPathOfCompPath id cpath

        let repr = Construct.NewProvidedTyconRepr(resolutionEnvironment, st, importProvidedType, isSuppressRelocate, m)

        Tycon.New "tycon"
          { entity_stamp=stamp;
            entity_logical_name=name
            entity_compiled_name=None;
            entity_kind=kind;
            entity_range=m;
            entity_flags=EntityFlags(usesPrefixDisplay=false, isModuleOrNamespace=false,preEstablishedHasDefaultCtor=false, hasSelfReferentialCtor=false);
            entity_attribs=[]; // fetched on demand via est.fs API
            entity_typars= LazyWithContext.NotLazy [];
            entity_tycon_abbrev = None;
            entity_tycon_repr = repr;
            entity_tycon_repr_accessibility = TAccess([]);
            entity_exn_info=TExnNone;
            entity_tycon_tcaug=TyconAugmentation.Create();
            entity_modul_contents = lazy new ModuleOrNamespaceType(Namespace, QueueList.ofList [], QueueList.ofList []);
            // Generated types get internal accessibility
            entity_accessiblity= access;
            entity_xmldoc =  XmlDoc [||]; // fetched on demand via est.fs API
            entity_xmldocsig="";        
            entity_pubpath = Some pubpath;
            entity_cpath = Some cpath;
            entity_il_repr_cache = newCache(); } 
#endif

    static member NewModuleOrNamespace cpath access (id:Ident) xml attribs mtype = 
        let stamp = newStamp() 
        // Put the module suffix on if needed 
        Tycon.New "mspec"
          { entity_logical_name=id.idText
            entity_compiled_name=None
            entity_range = id.idRange
            entity_stamp=stamp
            entity_kind=TyparKind.Type
            entity_modul_contents = mtype
            entity_flags=EntityFlags(usesPrefixDisplay=false, isModuleOrNamespace=true, preEstablishedHasDefaultCtor=false, hasSelfReferentialCtor=false)
            entity_typars=LazyWithContext.NotLazy []
            entity_tycon_abbrev = None
            entity_tycon_repr = TNoRepr
            entity_tycon_repr_accessibility = access
            entity_exn_info=TExnNone
            entity_tycon_tcaug=TyconAugmentation.Create()
            entity_pubpath=cpath |> Option.map (publicPathOfCompPath id)
            entity_cpath=cpath
            entity_accessiblity=access
            entity_attribs=attribs
            entity_xmldoc=xml
            entity_xmldocsig=""        
            entity_il_repr_cache = newCache() } 
and Accessibility = 
    /// Indicates the construct can only be accessed from any code in the given type constructor, module or assembly. [] indicates global scope. 
    | TAccess of CompilationPath list
    
and 
    [<NoEquality; NoComparison>]
    /// Backing data for type parameters and type inference variables
    // 
    // MEMORY PERF: TyparData objects are common. They could be reduced to a record of 4-5 words in 
    // the common case of inference type variables, e.g.
    //
    // TyparDataCommon = 
    //       typar_details: TyparDataUncommon // null indicates standard values for uncommon data
    //       typar_stamp: Stamp 
    //       typar_solution: TType option
    //       typar_constraints: TyparConstraint list 
    // where the "common" settings are 
    //     kind=TyparKind.Type, rigid=TyparRigidity.Flexible, id=compgen_id, staticReq=NoStaticReq, isCompGen=true, isFromError=false,
    //     dynamicReq=TyparDynamicReq.No, attribs=[], eqDep=false, compDep=false
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
    /// A declared generic type/measure parameter, or a type/measure inference variable.
    Typar = 
    { mutable Data: TyparData
      /// A cached TAST type used when this type variable is used as type.
      mutable AsType: TType }
    member x.Name                = x.Data.typar_id.idText
    member x.Range               = x.Data.typar_id.idRange
    member x.Id                  = x.Data.typar_id
    /// The unique stamp of the typar definition
    member x.Stamp               = x.Data.typar_stamp
    /// The inferred equivalence for the type inference variable, if any.
    member x.Solution            = x.Data.typar_solution
    /// The inferred constraints for the type inference variable, if any
    member x.Constraints         = x.Data.typar_constraints
    /// Indicates if the type variable is compiler generated, i.e. is an implicit type inference variable 
    member x.IsCompilerGenerated = x.Data.typar_flags.IsCompilerGenerated
    /// Indicates if the type variable can be sovled or given new constraints. The status of a type variable
    /// generally always evolves towards being either rigid or solved. 
    member x.Rigidity            = x.Data.typar_flags.Rigidity
    /// Indicates if a type parameter is needed at runtime and may not be eliminated
    member x.DynamicReq          = x.Data.typar_flags.DynamicReq
    /// Indicates that whether or not a generic type definition satisfies the equality constraint is dependent on whether this type variable satisfies the equality constraint.
    member x.EqualityConditionalOn = x.Data.typar_flags.EqualityConditionalOn
    /// Indicates that whether or not a generic type definition satisfies the comparison constraint is dependent on whether this type variable satisfies the comparison constraint.
    member x.ComparisonConditionalOn = x.Data.typar_flags.ComparisonConditionalOn
    /// Indicates if the type variable has a static "head type" requirement, i.e. ^a variables used in FSharp.Core and member constraints.
    member x.StaticReq           = x.Data.typar_flags.StaticReq
    /// Indicates if the type inference variable was generated after an error when type checking expressions or patterns
    member x.IsFromError         = x.Data.typar_flags.IsFromError
    /// Indicates whether a type variable can be instantiated by types or units-of-measure.
    member x.Kind                = x.Data.typar_flags.Kind
    /// Indicates whether a type variable is erased in compiled .NET IL code, i.e. whether it is a unit-of-measure variable
    member x.IsErased            = match x.Kind with TyparKind.Type -> false | _ -> true
    /// The declared attributes of the type parameter. Empty for type inference variables and parameters from .NET 
    member x.Attribs             = x.Data.typar_attribs
    /// Indicates the display name of a type variable
    member x.DisplayName = if x.Name = "?" then "?"+string x.Stamp else x.Name

    /// Adjusts the constraints associated with a type variable
    member x.FixupConstraints cs =
        x.Data.typar_constraints <-  cs


    /// Creates a type variable that contains empty data, and is not yet linked. Only used during unpickling of F# metadata.
    static member NewUnlinked() : Typar  = 
        let res = { Data = nullableSlotEmpty(); AsType=Unchecked.defaultof<_> }
        res.AsType <- TType_var res
        res

    /// Creates a type variable based on the given data. Only used during unpickling of F# metadata.
    static member New data : Typar = 
        let res = { Data = data; AsType=Unchecked.defaultof<_> }
        res.AsType <- TType_var res
        res

    /// Links a previously unlinked type variable to the given data. Only used during unpickling of F# metadata.
    member x.Link tg = x.Data <- nullableSlotFull(tg)

    /// Indicates if a type variable has been linked. Only used during unpickling of F# metadata.
    member x.IsLinked = match box x.Data with null -> false | _ -> true 

    /// Indicates if a type variable has been solved.
    member x.IsSolved = 
        match x.Solution with 
        | None -> false
        | _ -> true

    /// Sets the identifier assocaited with a type variable
    member x.SetIdent id = x.Data.typar_id <- id

    /// Sets the rigidity of a type variable
    member x.SetRigidity b            = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, b,              flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, flags.DynamicReq, flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    /// Sets whether a type variable is compiler generated
    member x.SetCompilerGenerated b   = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, b,                         flags.StaticReq, flags.DynamicReq, flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    /// Sets whether a type variable has a static requirement
    member x.SetStaticReq b           = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, b,               flags.DynamicReq, flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    /// Sets whether a type variable is required at runtime
    member x.SetDynamicReq b          = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, b               , flags.EqualityConditionalOn, flags.ComparisonConditionalOn) 
    /// Sets whether the equality constraint of a type definition depends on this type variable 
    member x.SetEqualityDependsOn b   = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, flags.DynamicReq, b                          , flags.ComparisonConditionalOn) 
    /// Sets whether the comparison constraint of a type definition depends on this type variable 
    member x.SetComparisonDependsOn b = let x = x.Data in let flags = x.typar_flags in x.typar_flags <- TyparFlags(flags.Kind, flags.Rigidity, flags.IsFromError, flags.IsCompilerGenerated, flags.StaticReq, flags.DynamicReq, flags.EqualityConditionalOn, b) 

    override x.ToString() = x.Name

and
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    TyparConstraint = 
    /// Indicates a constraint that a type is a subtype of the given type 
    | CoercesTo              of TType * range

    /// Indicates a default value for an inference type variable should it be netiher generalized nor solved 
    | DefaultsTo             of int * TType * range 
    
    /// Indicates a constraint that a type has a 'null' value 
    | SupportsNull               of range 
    
    /// Indicates a constraint that a type has a member with the given signature 
    | MayResolveMember of TraitConstraintInfo * range 
    
    /// Indicates a constraint that a type is a non-Nullable value type 
    /// These are part of .NET's model of generic constraints, and in order to 
    /// generate verifiable code we must attach them to F# generalzied type variables as well. 
    | IsNonNullableStruct     of range 
    
    /// Indicates a constraint that a type is a reference type 
    | IsReferenceType            of range 

    /// Indicates a constraint that a type is a simple choice between one of the given ground types. Only arises from 'printf' format strings. See format.fs 
    | SimpleChoice               of TTypes * range 

    /// Indicates a constraint that a type has a parameterless constructor 
    | RequiresDefaultConstructor of range 

    /// Indicates a constraint that a type is an enum with the given underlying 
    | IsEnum                     of TType * range 
    
    /// Indicates a constraint that a type implements IComparable, with special rules for some known structural container types
    | SupportsComparison               of range 
    
    /// Indicates a constraint that a type does not have the Equality(false) attribute, or is not a structural type with this attribute, with special rules for some known structural container types
    | SupportsEquality                of range 
    
    /// Indicates a constraint that a type is a delegate from the given tuple of args to the given return type
    | IsDelegate                 of TType * TType * range 
    
    /// Indicates a constraint that a type is .NET unmanaged type
    | IsUnmanaged                 of range
    
/// The specification of a member constraint that must be solved 
and 
    [<NoEquality; NoComparison>]
    TraitConstraintInfo = 

    /// TTrait(tys,nm,memFlags,argtys,rty,colution)
    ///
    /// Indicates the signature of a member constraint. Contains a mutable solution cell
    /// to store the inferred solution of the constraint.
    | TTrait of TTypes * string * MemberFlags * TTypes * TType option * TraitConstraintSln option ref 

    /// Get the member name associated with the member constraint.
    member x.MemberName = (let (TTrait(_,nm,_,_,_,_)) = x in nm)
    /// Get the return type recorded in the member constraint.
    member x.ReturnType = (let (TTrait(_,_,_,_,ty,_)) = x in ty)
    /// Get or set the solution of the member constraint during inference
    member x.Solution 
        with get() = (let (TTrait(_,_,_,_,_,sln)) = x in sln.Value)
        and set v = (let (TTrait(_,_,_,_,_,sln)) = x in sln.Value <- v)
    
and 
    [<NoEquality; NoComparison>]
    /// Indicates the solution of a member constraint during inference.
    TraitConstraintSln = 
    /// FSMethSln(typ, vref, minst)
    ///
    /// Indicates a trait is solved by an F# method.
    ///    typ   -- the type and its instantiation
    ///    vref  -- the method that solves the trait constraint
    ///    minst -- the generic method instantiation 
    | FSMethSln of TType * ValRef * TypeInst 

    /// FSRecdFieldSln(tinst, rfref, isSetProp)
    ///
    /// Indicates a trait is solved by an F# record field.
    ///    tinst   -- the instantiation of the declaring type
    ///    rfref   -- the reference to the record field
    ///    isSetProp -- indicates if this is a set of a record field
    | FSRecdFieldSln of TypeInst * RecdFieldRef * bool

    /// ILMethSln(typ, extOpt, ilMethodRef, minst)
    ///
    /// Indicates a trait is solved by a .NET method.
    ///    typ         -- the type and its instantiation
    ///    extOpt      -- information about an extension member, if any
    ///    ilMethodRef -- the method that solves the trait constraint
    ///    minst       -- the generic method instantiation 
    | ILMethSln of TType * ILTypeRef option * ILMethodRef * TypeInst    

    /// ClosedExprSln(expr)
    ///
    /// Indicates a trait is solved by an erased provided expression
    | ClosedExprSln of Expr 

    /// Indicates a trait is solved by a 'fake' instance of an operator, like '+' on integers
    | BuiltInSln

/// The partial information used to index the methods of all those in a ModuleOrNamespace.
and [<RequireQualifiedAccess>]
   ValLinkagePartialKey = 
   { /// The name of the type with which the member is associated. None for non-member values.
     MemberParentMangledName : string option 
     /// Indicates if the member is an override. 
     MemberIsOverride: bool 
     /// Indicates the logical name of the member. 
     LogicalName: string 
     /// Indicates the total argument count of the member.
     TotalArgCount: int } 

/// The full information used to identify a specific overloaded method
/// amongst all those in a ModuleOrNamespace.
and ValLinkageFullKey(partialKey: ValLinkagePartialKey,  typeForLinkage:TType option) =
    /// The partial information used to index the value in a ModuleOrNamespace.
    member x.PartialKey = partialKey
    /// The full type of the value for the purposes of linking. May be None for non-members, since they can't be overloaded.
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
    //
    // Note: this data is mutated during inference by adjustAllUsesOfRecValue when we replace the inferred type with a schema. 
    member x.Type                       = x.Data.val_type

    /// How visible is this value, function or member?
    member x.Accessibility              = x.Data.val_access

    /// Range of the definition (implementation) of the value, used by Visual Studio 
    /// Updated by mutation when the implementation is matched against the signature. 
    member x.DefinitionRange            = x.Data.val_defn_range

    /// The value of a value or member marked with [<LiteralAttribute>] 
    member x.LiteralValue               = x.Data.val_const

    /// Records the "extra information" for a value compiled as a method.
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


    /// The partial information used to index the methods of all those in a ModuleOrNamespace.
    member x.LinkagePartialKey : ValLinkagePartialKey = 
        assert x.IsCompiledAsTopLevel
        { LogicalName = x.LogicalName 
          MemberParentMangledName = (if x.IsMember then Some x.MemberApparentParent.LogicalName else None)
          MemberIsOverride = x.IsOverrideOrExplicitImpl
          TotalArgCount = if x.IsMember then x.ValReprInfo.Value.TotalArgCount else 0 }

    /// The full information used to identify a specific overloaded method amongst all those in a ModuleOrNamespace.
    member x.LinkageFullKey : ValLinkageFullKey = 
        assert x.IsCompiledAsTopLevel
        ValLinkageFullKey(x.LinkagePartialKey, (if x.IsMember then Some x.Type else None))


    /// Is this a member definition or module definition?
    member x.IsMemberOrModuleBinding    = x.Data.val_flags.IsMemberOrModuleBinding

    /// Indicates if this is an F#-defined extension member
    member x.IsExtensionMember          = x.Data.val_flags.IsExtensionMember

    /// The quotation expression associated with a value given the [<ReflectedDefinition>] tag
    member x.ReflectedDefinition        = x.Data.val_defn

    /// Is this a member, if so some more data about the member.
    ///
    /// Note, the value may still be (a) an extension member or (b) and abtract slot without
    /// a true body. These cases are often causes of bugs in the compiler.
    member x.MemberInfo                 = x.Data.val_member_info

    /// Indicates if this is a member, and if so, gives some more data about the member.
    ///
    /// Note, the value may still be (a) an extension member or (b) and abtract slot without
    /// a true body. These cases are often causes of bugs in the compiler.
    member x.IsMember                   = x.MemberInfo.IsSome

    /// Indicates if this is a member, excluding extension members
    member x.IsIntrinsicMember          = x.IsMember && not x.IsExtensionMember

    /// Indicates if this is an F#-defined value in a module, or an extension member, but excluding compiler generated bindings from optimizations
    member x.IsModuleBinding            = x.IsMemberOrModuleBinding && not x.IsMember 

    /// Indicates if this is something compiled into a module, i.e. a user-defined value, an extension member or a compiler-generated value
    member x.IsCompiledIntoModule       = x.IsExtensionMember || x.IsModuleBinding

    /// Indicates if this is an F#-defined instance member. 
    ///
    /// Note, the value may still be (a) an extension member or (b) and abtract slot without
    /// a true body. These cases are often causes of bugs in the compiler.
    member x.IsInstanceMember = x.IsMember && x.MemberInfo.Value.MemberFlags.IsInstance

    /// Indicates if this is an F#-defined 'new' constructor member
    member x.IsConstructor              =
        match x.MemberInfo with 
        | Some(memberInfo) when not x.IsExtensionMember && (memberInfo.MemberFlags.MemberKind = MemberKind.Constructor) -> true
        | _ -> false

    /// Indicates if this is a compiler-generated class constructor member
    member x.IsClassConstructor              =
        match x.MemberInfo with 
        | Some(memberInfo) when not x.IsExtensionMember && (memberInfo.MemberFlags.MemberKind = MemberKind.ClassConstructor) -> true
        | _ -> false

    /// Indicates if this value was a member declared 'override' or an implementation of an interface slot
    member x.IsOverrideOrExplicitImpl                 =
        match x.MemberInfo with 
        | Some(memberInfo) when memberInfo.MemberFlags.IsOverrideOrExplicitImpl -> true
        | _ -> false
            
    /// Indicates if this is declared 'mutable'
    member x.IsMutable                  = (match x.Data.val_flags.MutabilityInfo with Immutable -> false | Mutable -> true)

    /// Indicates if this is inferred to be a method or function that definitely makes no critical tailcalls?
    member x.MakesNoCriticalTailcalls = x.Data.val_flags.MakesNoCriticalTailcalls
    
    /// Indicates if this is ever referenced?
    member x.HasBeenReferenced = x.Data.val_flags.HasBeenReferenced

    /// Indicates if the backing field for a static value is suppressed.
    member x.IsCompiledAsStaticPropertyWithoutField = x.Data.val_flags.IsCompiledAsStaticPropertyWithoutField

    /// Indicates if this is allows the use of an explicit type instantiation (i.e. does it itself have explciti type arguments,
    /// or does it have a signature?)
    member x.PermitsExplicitTypeInstantiation = x.Data.val_flags.PermitsExplicitTypeInstantiation

    /// Indicates if this is a member generated from the de-sugaring of 'let' function bindings in the implicit class syntax?
    member x.IsIncrClassGeneratedMember     = x.IsCompilerGenerated && x.Data.val_flags.IsIncrClassSpecialMember

    /// Indicates if this is a constructor member generated from the de-sugaring of implicit constructor for a class type?
    member x.IsIncrClassConstructor = x.IsConstructor && x.Data.val_flags.IsIncrClassSpecialMember

    /// Get the information about the value used during type inference
    member x.RecursiveValInfo           = x.Data.val_flags.RecursiveValInfo

    /// Indicates if this is a 'base' or 'this' value?
    member x.BaseOrThisInfo             = x.Data.val_flags.BaseOrThisInfo

    //  Indicates if this value was declared to be a type function, e.g. "let f<'a> = typeof<'a>"
    member x.IsTypeFunction             = x.Data.val_flags.IsTypeFunction

    /// Get the inline declaration on the value
    member x.InlineInfo                 = x.Data.val_flags.InlineInfo

    /// Indicates whether the inline declaration for the value indicate that the value must be inlined?
    member x.MustInline                 = mustinline(x.InlineInfo)

    /// Indicates whether this value was generated by the compiler.
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

    /// Get the number of 'this'/'self' object arguments for the member. Instance extension members return '1'.
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

    /// Indicates if this member is an F#-defined dispatch slot.
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

    /// Create a new value with empty, unlinked data. Only used during unpickling of F# metadata.
    static member NewUnlinked() : Val  = { Data = nullableSlotEmpty() }

    /// Create a new value with the given backing data. Only used during unpickling of F# metadata.
    static member New data : Val = { Data = data }

    /// Link a value based on empty, unlinked data to the given data. Only used during unpickling of F# metadata.
    member x.Link tg = x.Data <- nullableSlotFull(tg)

    /// Indicates if a value is linked to backing data yet. Only used during unpickling of F# metadata.
    member x.IsLinked = match box x.Data with null -> false | _ -> true 

    override x.ToString() = x.LogicalName
    
    
and 
    [<NoEquality; NoComparison>]
    [<StructuredFormatDisplay("{val_logical_name}")>]
    ValData =

    // ValData is 19 words!! CONSIDER THIS TINY FORMAT, for all local, immutable, attribute-free values
    // val_logical_name: string
    // val_range: range
    // mutable val_type: TType
    // val_stamp: Stamp 

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

      /// Updated with the full implemented slotsig after interface implementation relation is checked 
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

    /// Try to find the entity correspondng to the given path in the given CCU
    static member TryDerefEntityPath(ccu: CcuThunk, path:string[], i:int, entity:Entity) = 
        if i >= path.Length then Some entity
        else  
            let next = entity.ModuleOrNamespaceType.AllEntitiesByCompiledAndLogicalMangledNames.TryFind(path.[i])
            match next with 
            | Some res -> NonLocalEntityRef.TryDerefEntityPath(ccu, path, (i+1), res)
#if EXTENSIONTYPING
            | None -> NonLocalEntityRef.TryDerefEntityPathViaProvidedType(ccu, path, i, entity)
#else
            | None -> None
#endif

#if EXTENSIONTYPING
    /// Try to find the entity correspondng to the given path, using type-providers to link the data
    static member TryDerefEntityPathViaProvidedType(ccu: CcuThunk, path:string[], i:int, entity:Entity) = 
        // Errors during linking are not necessarily given good ranges. This has always been the case in F# 2.0, but also applies to
        // type provider type linking errors in F# 3.0.
        let m = range0
        match entity.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> 
            let resolutionEnvironment = info.ResolutionEnvironment
            let st = info.ProvidedType
                        
            // In this case, we're safely in the realm of types. Just iterate through the nested
            // types until i = path.Length-1. Create the Tycon's as needed
            let rec tryResolveNestedTypeOf(parentEntity:Entity,resolutionEnvironment,st:Tainted<ProvidedType>,i) = 
                match st.PApply((fun st -> st.GetNestedType path.[i]),m) with
                | Tainted.Null -> None
                | st -> 
                    let newEntity = Construct.NewProvidedTycon(resolutionEnvironment, st, ccu.ImportProvidedType, false, m)
                    parentEntity.ModuleOrNamespaceType.AddProvidedTypeEntity(newEntity)
                    if i = path.Length-1 then Some(newEntity)
                    else tryResolveNestedTypeOf(newEntity,resolutionEnvironment,st,i+1)

            tryResolveNestedTypeOf(entity,resolutionEnvironment,st,i)

        | TProvidedNamespaceExtensionPoint(resolutionEnvironment,resolvers) -> 

            // In this case, we're still in the realm of extensible namespaces. 
            //     <----entity-->
            //     0 .........i-1..i .......... j ..... path.Length-1
            //
            //     <----entity-->  <---resolver---->
            //     0 .........i-1..i ............. j ..... path.Length-1
            //
            //     <----entity-->  <---resolver----> <--loop--->
            //     0 .........i-1..i ............. j ..... path.Length-1
            //
            // We now query the resolvers with 
            //      moduleOrNamespace = path.[0..j-1] 
            //      typeName = path.[j] 
            // starting with j = i and then progressively increasing j
                        
            // This function queries at 'j'
            let tryResolvePrefix j = 
                assert (j >= 0)
                assert (j <= path.Length - 1)
                let matched = 
                    [ for resolver in resolvers  do
                        let moduleOrNamespace = if j = 0 then null else path.[0..j-1]
                        let typename = path.[j]
                        let resolution = ExtensionTyping.TryLinkProvidedType(resolutionEnvironment,resolver,moduleOrNamespace,typename,m)
                        match resolution with
                        | None | Some (Tainted.Null) -> ()
                        | Some st -> yield (resolver,st) ]
                match matched with
                | [(_,st)] ->
                    // 'entity' is at position i in the dereference chain. We resolved to position 'j'.
                    // Inject namespaces until we're an position j, and then inject the type.
                    // Note: this is similar to code in build.fs
                    let rec injectNamespacesFromIToJ (entity: Entity) k = 
                        if k = j  then 
                            let newEntity = Construct.NewProvidedTycon(resolutionEnvironment, st, ccu.ImportProvidedType, false, m)
                            entity.ModuleOrNamespaceType.AddProvidedTypeEntity(newEntity)
                            newEntity
                        else
                            let cpath = mkNestedCPath entity.CompilationPath entity.LogicalName ModuleOrNamespaceKind.Namespace
                            let newEntity = 
                                Construct.NewModuleOrNamespace 
                                    (Some cpath) 
                                    (TAccess []) (ident(path.[k],m)) XmlDoc.Empty [] 
                                    (notlazy (Construct.NewEmptyModuleOrNamespaceType Namespace)) 
                            entity.ModuleOrNamespaceType.AddModuleOrNamespaceByMutation(newEntity)
                            injectNamespacesFromIToJ newEntity (k+1)
                    let newEntity = injectNamespacesFromIToJ entity i
                                
                    // newEntity is at 'j'
                    NonLocalEntityRef.TryDerefEntityPath(ccu, path, (j+1), newEntity) 

                | [] -> None 
                | _ -> failwith "Unexpected"

            let rec tryResolvePrefixes j = 
                if j >= path.Length then None
                else match tryResolvePrefix j with 
                      | None -> tryResolvePrefixes (j+1)
                      | Some res -> Some res

            tryResolvePrefixes i

        | _ -> None
#endif
            
    /// Try to link a non-local entity reference to an actual entity
    member nleref.TryDeref = 
        let (NonLocalEntityRef(ccu,path)) = nleref 
        ccu.EnsureDerefable(path)

        match NonLocalEntityRef.TryDerefEntityPath(ccu, path, 0, ccu.Contents)  with
        | Some _ as r -> r
        | None ->
            // OK, the lookup failed. Check if we can redirect through a type forwarder on this assembly.
            // Look for a forwarder for each prefix-path
            let rec tryForwardPrefixPath i = 
                if i < path.Length then 
                    match ccu.TryForward(path.[0..i-1],path.[i]) with
                       // OK, found a forwarder, now continue with the lookup to find the nested type
                    | Some tcref -> NonLocalEntityRef.TryDerefEntityPath(ccu, path, (i+1), tcref.Deref)  
                    | None -> tryForwardPrefixPath (i+1)
                else
                    None
            tryForwardPrefixPath 0
        
    /// Get the CCU referenced by the nonlocal reference.
    member nleref.Ccu =
        let (NonLocalEntityRef(ccu,_)) = nleref 
        ccu

    /// Get the path into the CCU referenced by the nonlocal reference.
    member nleref.Path =
        let (NonLocalEntityRef(_,p)) = nleref 
        p

    member nleref.DisplayName =
        String.concat "." nleref.Path

    /// Get the mangled name of the last item in the path of the nonlocal reference.
    member nleref.LastItemMangledName = 
        let p = nleref.Path
        p.[p.Length-1]

    /// Get the all-but-last names of the path of the nonlocal reference.
    member nleref.EnclosingMangledPath =  
        let p = nleref.Path
        p.[0..p.Length-2]
        
    /// Get the name of the assembly referenced by the nonlocal reference.
    member nleref.AssemblyName = nleref.Ccu.AssemblyName

    /// Dereference the nonlocal reference, and raise an error if this fails.
    member nleref.Deref = 
        match nleref.TryDeref with 
        | Some res -> res
        | None -> 
              errorR (InternalUndefinedItemRef (FSComp.SR.tastUndefinedItemRefModuleNamespace, nleref.DisplayName, nleref.AssemblyName, "<some module on this path>")); 
              raise (KeyNotFoundException())
        
    /// Try to get the details of the module or namespace fragment referred to by this non-local reference.
    member nleref.TryModuleOrNamespaceType = 
        nleref.TryDeref |> Option.map (fun v -> v.ModuleOrNamespaceType) 

    /// Get the details of the module or namespace fragment for the entity referred to by this non-local reference.
    member nleref.ModuleOrNamespaceType = 
        nleref.Deref.ModuleOrNamespaceType

    override x.ToString() = x.DisplayName
        
and 
    [<StructuredFormatDisplay("{LogicalName}")>]
    [<NoEquality; NoComparison>]
    EntityRef = 
    { /// Indicates a reference to something bound in this CCU 
      mutable binding: NonNullSlot<Entity>
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
            | null -> error (InternalUndefinedItemRef (FSComp.SR.tastUndefinedItemRefModuleNamespaceType, String.concat "." tcr.nlr.EnclosingMangledPath, tcr.nlr.AssemblyName, tcr.nlr.LastItemMangledName))
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

    /// Gets the data indicating the compiled representation of a type or module in terms of Abstract IL data structures.
    member x.CompiledRepresentation = x.Deref.CompiledRepresentation
    /// Gets the data indicating the compiled representation of a named type or module in terms of Abstract IL data structures.
    member x.CompiledRepresentationForNamedType = x.Deref.CompiledRepresentationForNamedType
    /// The name of the namespace, module or type, possibly with mangling, e.g. List`1, List or FailureException 
    member x.LogicalName = x.Deref.LogicalName
    /// The compiled name of the namespace, module or type, e.g. FSharpList`1, ListModule or FailureException 
    member x.CompiledName = x.Deref.CompiledName
    /// The display name of the namespace, module or type, e.g. List instead of List`1, not including static parameters
    member x.DisplayName = x.Deref.DisplayName
    /// The display name of the namespace, module or type with <_,_,_> added for generic types,  including static parameters
    member x.DisplayNameWithStaticParametersAndUnderscoreTypars = x.Deref.DisplayNameWithStaticParametersAndUnderscoreTypars
    /// The display name of the namespace, module or type, e.g. List instead of List`1, including static parameters
    member x.DisplayNameWithStaticParameters = x.Deref.DisplayNameWithStaticParameters
    /// The code location where the module, namespace or type is defined.
    member x.Range = x.Deref.Range
    /// A unique stamp for this module, namespace or type definition within the context of this compilation. 
    /// Note that because of signatures, there are situations where in a single compilation the "same" 
    /// module, namespace or type may have two distinct Entity objects that have distinct stamps.
    member x.Stamp = x.Deref.Stamp
    /// The F#-defined custom attributes of the entity, if any. If the entity is backed by Abstract IL or provided metadata
    /// then this does not include any attributes from those sources.
    member x.Attribs = x.Deref.Attribs
    /// The XML documentation of the entity, if any. If the entity is backed by provided metadata
    /// then this _does_ include this documentation. If the entity is backed by Abstract IL metadata
    /// or comes from another F# assembly then it does not (because the documentation will get read from 
    /// an XML file).
    member x.XmlDoc = x.Deref.XmlDoc
    /// The XML documentation sig-string of the entity, if any, to use to lookup an .xml doc file. This also acts
    /// as a cache for this sig-string computation.
    member x.XmlDocSig = x.Deref.XmlDocSig
    /// The logical contents of the entity when it is a module or namespace fragment.
    member x.ModuleOrNamespaceType = x.Deref.ModuleOrNamespaceType
    
    /// Demangle the module name, if FSharpModuleWithSuffix is used
    member x.DemangledModuleOrNamespaceName = x.Deref.DemangledModuleOrNamespaceName

    /// The logical contents of the entity when it is a type definition.
    member x.TypeContents = x.Deref.TypeContents
    /// The kind of the type definition - is it a measure definition or a type definition?
    member x.TypeOrMeasureKind = x.Deref.TypeOrMeasureKind
    /// The identifier at the point of declaration of the type definition.
    member x.Id = x.Deref.Id
    /// The information about the r.h.s. of a type definition, if any. For example, the r.h.s. of a union or record type.
    member x.TypeReprInfo = x.Deref.TypeReprInfo
    /// The information about the r.h.s. of an F# exception definition, if any. 
    member x.ExceptionInfo        = x.Deref.ExceptionInfo
    /// Indicates if the entity represents an F# exception declaration.
    member x.IsExceptionDecl      = x.Deref.IsExceptionDecl
    
    /// Get the type parameters for an entity that is a type declaration, otherwise return the empty list.
    /// 
    /// Lazy because it may read metadata, must provide a context "range" in case error occurs reading metadata.
    member x.Typars m             = x.Deref.Typars m
    /// Get the type parameters for an entity that is a type declaration, otherwise return the empty list.
    member x.TyparsNoRange        = x.Deref.TyparsNoRange
    /// Indicates if this entity is an F# type abbreviation definition
    member x.TypeAbbrev           = x.Deref.TypeAbbrev
    member x.IsTypeAbbrev         = x.Deref.IsTypeAbbrev
    /// Get the value representing the accessiblity of the r.h.s. of an F# type definition.
    member x.TypeReprAccessibility = x.Deref.TypeReprAccessibility
    /// Get the cache of the compiled ILTypeRef representation of this module or type.
    member x.CompiledReprCache    = x.Deref.CompiledReprCache
    /// Get a blob of data indicating how this type is nested in other namespaces, modules or types.
    member x.PublicPath : PublicPath option = x.Deref.PublicPath
    /// Get the value representing the accessiblity of an F# type definition or module.
    member x.Accessibility        = x.Deref.Accessibility
    /// Indicates the type prefers the "tycon<a,b>" syntax for display etc. 
    member x.IsPrefixDisplay      = x.Deref.IsPrefixDisplay
    /// Indicates the "tycon blob" is actually a module 
    member x.IsModuleOrNamespace  = x.Deref.IsModuleOrNamespace
    /// Indicates if the entity is a namespace
    member x.IsNamespace          = x.Deref.IsNamespace
    /// Indicates if the entity is an F# module definition
    member x.IsModule             = x.Deref.IsModule
    member x.CompilationPathOpt   = x.Deref.CompilationPathOpt
#if EXTENSIONTYPING
    /// Indicates if the entity is a provided namespace fragment
    member x.IsProvided               = x.Deref.IsProvided
    /// Indicates if the entity is a provided namespace fragment
    member x.IsProvidedNamespace      = x.Deref.IsProvidedNamespace
    /// Indicates if the entity is an erased provided type definition
    member x.IsProvidedErasedTycon    = x.Deref.IsProvidedErasedTycon
    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon = x.Deref.IsProvidedGeneratedTycon
#endif
    /// Get a blob of data indicating how this type is nested inside other namespaces, modules and types.
    member x.CompilationPath      = x.Deref.CompilationPath
    /// Get a table of fields for all the F#-defined record, struct and class fields in this type definition, including
    /// static fields, 'val' declarations and hidden fields from the compilation of implicit class constructions.
    member x.AllFieldTable        = x.Deref.AllFieldTable
    /// Get an array of fields for all the F#-defined record, struct and class fields in this type definition, including
    /// static fields, 'val' declarations and hidden fields from the compilation of implicit class constructions.
    member x.AllFieldsArray       = x.Deref.AllFieldsArray
    /// Get a list of fields for all the F#-defined record, struct and class fields in this type definition, including
    /// static fields, 'val' declarations and hidden fields from the compilation of implicit class constructions.
    member x.AllFieldsAsList = x.Deref.AllFieldsAsList
    /// Get a list of all fields for F#-defined record, struct and class fields in this type definition,
    /// including static fields, but excluding compiler-generate fields.
    member x.TrueFieldsAsList = x.Deref.TrueFieldsAsList
    /// Get a list of all instance fields for F#-defined record, struct and class fields in this type definition,
    /// excluding compiler-generate fields.
    member x.TrueInstanceFieldsAsList = x.Deref.TrueInstanceFieldsAsList
    /// Get a list of all instance fields for F#-defined record, struct and class fields in this type definition.
    /// including hidden fields from the compilation of implicit class constructions.
    // NOTE: This method doesn't perform particularly well, and is over-used, but doesn't seem to appear on performance traces
    member x.AllInstanceFieldsAsList = x.Deref.AllInstanceFieldsAsList
    /// Get a field by index in definition order
    member x.GetFieldByIndex  n        = x.Deref.GetFieldByIndex n
    /// Get a field by name.
    member x.GetFieldByName n          = x.Deref.GetFieldByName n
    /// Get the union cases and other union-type information for a type, if any
    member x.UnionTypeInfo             = x.Deref.UnionTypeInfo
    /// Get the union cases for a type, if any
    member x.UnionCasesArray           = x.Deref.UnionCasesArray
    /// Get the union cases for a type, if any, as a list
    member x.UnionCasesAsList          = x.Deref.UnionCasesAsList
    /// Get a union case of a type by name
    member x.GetUnionCaseByName n      = x.Deref.GetUnionCaseByName n
    /// Get the blob of information associated with an F# object-model type definition, i.e. class, interface, struct etc.
    member x.FSharpObjectModelTypeInfo = x.Deref.FSharpObjectModelTypeInfo
    /// Gets the immediate interface definitions of an F# type definition. Further interfaces may be supported through class and interface inheritance.
    member x.ImmediateInterfacesOfFSharpTycon   = x.Deref.ImmediateInterfacesOfFSharpTycon
    /// Gets the immediate interface types of an F# type definition. Further interfaces may be supported through class and interface inheritance.
    member x.ImmediateInterfaceTypesOfFSharpTycon = x.Deref.ImmediateInterfaceTypesOfFSharpTycon
    /// Gets the immediate members of an F# type definition, excluding compiler-generated ones.
    /// Note: result is alphabetically sorted, then for each name the results are in declaration order
    member x.MembersOfFSharpTyconSorted = x.Deref.MembersOfFSharpTyconSorted
    /// Gets all immediate members of an F# type definition keyed by name, including compiler-generated ones.
    /// Note: result is a indexed table, and for each name the results are in reverse declaration order
    member x.MembersOfFSharpTyconByName = x.Deref.MembersOfFSharpTyconByName
    /// Indicates if this is a struct or enum type definition , i.e. a value type definition
    member x.IsStructOrEnumTycon       = x.Deref.IsStructOrEnumTycon
    /// Indicates if this is an F# type definition which is one of the special types in FSharp.Core.dll which uses 
    /// an assembly-code representation for the type, e.g. the primitive array type constructor.
    member x.IsAsmReprTycon            = x.Deref.IsAsmReprTycon
    /// Indicates if this is an F# type definition which is one of the special types in FSharp.Core.dll like 'float<_>' which
    /// defines a measure type with a relation to an existing non-measure type as a representation.
    member x.IsMeasureableReprTycon    = x.Deref.IsMeasureableReprTycon
    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased                  = x.Deref.IsErased
    
    /// Gets any implicit hash/equals (with comparer argument) methods added to an F# record, union or struct type definition.
    member x.GeneratedHashAndEqualsWithComparerValues = x.Deref.GeneratedHashAndEqualsWithComparerValues
    /// Gets any implicit CompareTo (with comparer argument) methods added to an F# record, union or struct type definition.
    member x.GeneratedCompareToWithComparerValues = x.Deref.GeneratedCompareToWithComparerValues
    /// Gets any implicit CompareTo methods added to an F# record, union or struct type definition.
    member x.GeneratedCompareToValues = x.Deref.GeneratedCompareToValues
    /// Gets any implicit hash/equals methods added to an F# record, union or struct type definition.
    member x.GeneratedHashAndEqualsValues = x.Deref.GeneratedHashAndEqualsValues
    
    /// Indicate if this is a type definition backed by Abstract IL metadata.
    member x.IsILTycon                = x.Deref.IsILTycon
    /// Get the Abstract IL scope, nesting and metadata for this 
    /// type definition, assuming it is backed by Abstract IL metadata.
    member x.ILTyconInfo              = x.Deref.ILTyconInfo
    /// Get the Abstract IL metadata for this type definition, assuming it is backed by Abstract IL metadata.
    member x.ILTyconRawMetadata       = x.Deref.ILTyconRawMetadata
    /// Indicate if this is a type whose r.h.s. is known to be a union type definition.
    member x.IsUnionTycon             = x.Deref.IsUnionTycon
    /// Indicates if this is an F# type definition whose r.h.s. is known to be a record type definition.
    member x.IsRecordTycon            = x.Deref.IsRecordTycon
    /// Indicates if this is an F# type definition whose r.h.s. is known to be some kind of F# object model definition
    member x.IsFSharpObjectModelTycon = x.Deref.IsFSharpObjectModelTycon
    /// Indicates if this is an F# type definition whose r.h.s. definition is unknown (i.e. a traditional ML 'abstract' type in a signature,
    /// which in F# is called a 'unknown representation' type).
    member x.IsHiddenReprTycon        = x.Deref.IsHiddenReprTycon

    /// Indicates if this is an F#-defined interface type definition 
    member x.IsFSharpInterfaceTycon   = x.Deref.IsFSharpInterfaceTycon
    /// Indicates if this is an F#-defined delegate type definition 
    member x.IsFSharpDelegateTycon    = x.Deref.IsFSharpDelegateTycon
    /// Indicates if this is an F#-defined enum type definition 
    member x.IsFSharpEnumTycon        = x.Deref.IsFSharpEnumTycon
    /// Indicates if this is a .NET-defined enum type definition 
    member x.IsILEnumTycon            = x.Deref.IsILEnumTycon
    /// Indicates if this is an enum type definition 
    member x.IsEnumTycon              = x.Deref.IsEnumTycon

    /// Indicates if this is an F#-defined struct or enum type definition , i.e. a value type definition
    member x.IsFSharpStructOrEnumTycon      = x.Deref.IsFSharpStructOrEnumTycon

    /// Indicates if this is a .NET-defined struct or enum type definition , i.e. a value type definition
    member x.IsILStructOrEnumTycon          = x.Deref.IsILStructOrEnumTycon
    /// Indicates if we have pre-determined that a type definition has a default constructor.
    member x.PreEstablishedHasDefaultConstructor = x.Deref.PreEstablishedHasDefaultConstructor
    /// Indicates if we have pre-determined that a type definition has a self-referential constructor using 'as x'
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
      mutable binding: NonNullSlot<Val>
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
                | None -> error (InternalUndefinedItemRef (FSComp.SR.tastUndefinedItemRefVal, e.DisplayNameWithStaticParameters, nlr.AssemblyName, sprintf "%+A" nlr.ItemKey.PartialKey))
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

    /// TType_forall(typars, bodyTy).
    ///
    /// Indicates the type is a universal type, only used for types of values and members 
    | TType_forall of Typars * TType

    /// TType_app(tyconRef, typeInstantiation).
    ///
    /// Indicates the type is build from a named type and a number of type arguments
    | TType_app of TyconRef * TypeInst

    /// TType_tuple(elementTypes).
    ///
    /// Indicates the type is a tuple type. elementTypes must be of length 2 or greater.
    | TType_tuple of TTypes

    /// TType_fun(domainType,rangeType).
    ///
    /// Indicates the type is a function type 
    | TType_fun of  TType * TType

    /// TType_ucase(unionCaseRef, typeInstantiation)
    ///
    /// Indicates the type is a non-F#-visible type representing a "proof" that a union value belongs to a particular union case
    /// These types are not user-visible and will never appear as an inferred type. They are the types given to
    /// the temporaries arising out of pattern matching on union values.
    | TType_ucase of  UnionCaseRef * TypeInst

    /// Indicates the type is a variable type, whether declared, generalized or an inference type parameter  
    | TType_var of Typar 

    /// Indicates the type is a unit-of-measure expression being used as an argument to a type or member
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
    /// A variable unit-of-measure
    | MeasureVar of Typar

    /// A constant, leaf unit-of-measure such as 'kg' or 'm'
    | MeasureCon of TyconRef

    /// A product of two units of measure
    | MeasureProd of MeasureExpr*MeasureExpr

    /// An inverse of a units of measure expression
    | MeasureInv of MeasureExpr

    /// The unit of measure '1', e.g. float = float<1>
    | MeasureOne

and 
    [<NoEquality; NoComparison>]
    CcuData = 
    { /// Holds the filename for the DLL, if any 
      FileName: string option 
      
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
      
#if EXTENSIONTYPING
      /// Is the CCu an assembly inected by a type provider
      IsProviderGenerated: bool 

      /// Triggered when the contents of the CCU are invalidated
      InvalidateEvent : IEvent<string> 

      /// A helper function used to link method signatures using type equality. This is effectively a forward call to the type equality 
      /// logic in tastops.fs
      ImportProvidedType : Tainted<ProvidedType> -> TType 
      
#endif
      /// Indicates that this DLL uses quotation literals somewhere. This is used to implement a restriction on static linking
      mutable UsesQuotations : bool
      
      /// A handle to the full specification of the contents of the module contained in this ccu
      // NOTE: may contain transient state during typechecking 
      mutable Contents: ModuleOrNamespace
      
      /// A helper function used to link method signatures using type equality. This is effectively a forward call to the type equality 
      /// logic in tastops.fs
      MemberSignatureEquality : (TType -> TType -> bool) 
      
      /// The table of .NET CLI type forwarders for this assembly
      TypeForwarders : CcuTypeForwarderTable }

/// Represents a table of .NET CLI type forwarders for an assembly
and CcuTypeForwarderTable = Map<string[] * string, Lazy<EntityRef>>

and CcuReference =  string // ILAssemblyRef


/// A relinkable handle to the contents of a compilation unit. Relinking is performed by mutation.
//
/// A compilation unit is, more or less, the new material created in one
/// invocation of the compiler.  Due to static linking assemblies may hold more 
/// than one compilation unit (i.e. when two assemblies are merged into a compilation
/// the resulting assembly will contain 3 CUs).  Compilation units are also created for referenced
/// .NET assemblies. 
/// 
/// References to items such as type constructors are via 
/// cross-compilation-unit thunks, which directly reference the data structures that define
/// these modules.  Thus, when saving out values to disk we only wish 
/// to save out the "current" part of the term graph.  When reading values
/// back in we "fixup" the links to previously referenced modules.
///
/// All non-local accesses to the data structures are mediated
/// by ccu-thunks.  Ultimately, a ccu-thunk is either a (named) element of
/// the data structure, or it is a delayed fixup, i.e. an invalid dangling
/// reference that has not had an appropriate fixup applied.  
and CcuThunk = 
    { mutable target: CcuData
      /// ccu.orphanfixup is true when a reference is missing in the transitive closure of static references that
      /// may potentially be required for the metadata of referenced DLLs. It is set to true if the "loader"
      /// used in the F# metadata-deserializer or the .NET metadata reader returns a failing value (e.g. None).
      /// Note: When used from Visual Studio, the loader will not automatically chase down transitively referenced DLLs - they
      /// must be in the explicit references in the project.
      mutable orphanfixup : bool
      name: CcuReference  }

    member ccu.Deref = 
        if isNull ccu.target || ccu.orphanfixup then 
            raise(UnresolvedReferenceNoRange ccu.name)
        ccu.target
   
    member ccu.IsUnresolvedReference = (isNull ccu.target || ccu.orphanfixup)

    /// Ensure the ccu is derefable in advance. Supply a path to attach to any resulting error message.
    member ccu.EnsureDerefable(requiringPath:string[]) = 
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
#if EXTENSIONTYPING
    /// Is the CCu an EST injected assembly
    member ccu.IsProviderGenerated      = ccu.Deref.IsProviderGenerated

    /// Used to make 'forward' calls into the loader during linking
    member ccu.ImportProvidedType ty : TType = ccu.Deref.ImportProvidedType ty

#endif
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
    member ccu.TypeForwarders : Map<string[] * string, Lazy<EntityRef>>  = ccu.Deref.TypeForwarders

    /// The table of modules and namespaces at the "root" of the assembly
    member ccu.RootModulesAndNamespaces = ccu.Contents.ModuleOrNamespaceType.ModuleAndNamespaceDefinitions

    /// The table of type definitions at the "root" of the assembly
    member ccu.RootTypeAndExceptionDefinitions = ccu.Contents.ModuleOrNamespaceType.TypeAndExceptionDefinitions

    /// Create a CCU with the given name and contents
    static member Create(nm,x) = 
        { target = x 
          orphanfixup = false
          name = nm  }

    /// Create a CCU with the given name but where the contents have not yet been specified
    static member CreateDelayed(nm) = 
        { target = Unchecked.defaultof<_> 
          orphanfixup = false
          name = nm  }

    /// Fixup a CCU to have the given contents
    member x.Fixup(avail:CcuThunk) = 
        match box x.target with
        | null -> 
            assert (avail.AssemblyName = x.AssemblyName)
            x.target <- 
               (match box avail.target with
                | null -> error(Failure("internal error: ccu thunk '"+avail.name+"' not fixed up!"))
                | _ -> avail.target)
        | _ -> errorR(Failure("internal error: the ccu thunk for assembly "+x.AssemblyName+" not delayed!"))
        
    /// Fixup a CCU to record it as "orphaned", i.e. not available
    member x.FixupOrphaned() = 
        match box x.target with
        | null -> x.orphanfixup<-true
        | _ -> errorR(Failure("internal error: the ccu thunk for assembly "+x.AssemblyName+" not delayed!"))
            
    /// Try to resolve a path into the CCU by referencing the .NET/CLI type forwarder table of the CCU
    member ccu.TryForward(nlpath:string[],item:string) : EntityRef option  = 
        ccu.EnsureDerefable(nlpath)
        match ccu.TypeForwarders.TryFind(nlpath,item) with
        | Some entity -> Some(entity.Force())
        | None -> None
        //printfn "trying to forward %A::%s from ccu '%s', res = '%A'" p n ccu.AssemblyName res.IsSome

    /// Used to make forward calls into the type/assembly loader when comparing member signatures during linking
    member ccu.MemberSignatureEquality(ty1:TType, ty2:TType) = 
        ccu.Deref.MemberSignatureEquality ty1 ty2
    
    override ccu.ToString() = ccu.AssemblyName

/// The result of attempting to resolve an assembly name to a full ccu.
/// UnresolvedCcu will contain the name of the assembly that could not be resolved.
and CcuResolutionResult =
    | ResolvedCcu of CcuThunk
    | UnresolvedCcu of string

/// Represents the information saved in the assembly signature data resource for an F# assembly
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

/// Attrib(kind,unnamedArgs,propVal,appliedToAGetterOrSetter,targetsOpt,range)
and Attrib = 
  | Attrib of TyconRef * AttribKind * AttribExpr list * AttribNamedArg list * bool * AttributeTargets option * range

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
    member x.TotalArgCount = 
        let (ValReprInfo(_,args,_)) = x in 
        // This is List.sumBy List.length args
        // We write this by hand as it can be a performance bottleneck in LinkagePartialKey
        let rec loop (args:ArgReprInfo list list) acc = 
            match args with 
            | [] -> acc 
            | []::t -> loop t acc 
            | [_]::t -> loop t (acc+1) 
            | (_::_::h)::t -> loop t (acc + h.Length + 2) 
        loop args 0

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
    | Sequential of Expr * Expr * SequentialOpKind * SequencePointInfoForSeq * range

    /// Lambda expressions. 
    
    /// Why multiple vspecs? A Expr.Lambda taking multiple arguments really accepts a tuple. 
    /// But it is in a convenient form to be compile accepting multiple 
    /// arguments, e.g. if compiled as a toplevel static method. 
    | Lambda of Unique * Val option * Val option * Val list * Expr * range * TType

    /// Type lambdas.  These are used for the r.h.s. of polymorphic 'let' bindings and 
    /// for expressions that implement first-class polymorphic values. 
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

    // Expr.Quote(quotedExpr, savedPickledAstInfoOption, isFromQueryExpression, fullRange, quotedType)
    //
    // Indicates the expression is a quoted expression tree. 
    | Quote of Expr * (TTypes * Exprs * ExprData) option ref * bool * range * TType  
    
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
    /// An operation representing a coercion that proves a union value is of a particular union case. This is not a test, its
    /// simply added proof to enable us to generate verifiable code for field access on union types
    | UnionCaseProof of UnionCaseRef
    /// An operation representing a field-get from a union value, where that value has been proven to be of the corresponding union case.
    | UnionCaseFieldGet of UnionCaseRef * int 
    /// An operation representing a field-get from a union value. The value is not assumed to have been proven to be of the corresponding union case.
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

    /// ILCall(useCallvirt,isProtected,valu,newobj,valUseFlags,isProp,noTailCall,mref,actualTypeInst,actualMethInst, retTy)
    ///  
    /// IL method calls.
    ///     value -- is the object a value type? 
    ///     isProp -- used for quotation reflection.
    ///     noTailCall - DllImport? if so don't tailcall 
    ///     actualTypeInst -- instantiation of the enclosing type
    ///     actualMethInst -- instantiation of the method
    ///     retTy -- the types of pushed values, if any 
    | ILCall of bool * bool * bool * bool * ValUseFlag * bool * bool * ILMethodRef * TypeInst * TypeInst * TTypes   


/// Indicates the kind of record construction operation.
and RecordConstructionInfo = 
   /// We're in an explicit constructor. The purpose of the record expression is to 
   /// fill in the fields of a pre-created but uninitialized object 
   | RecdExprIsObjInit
   /// Normal record construction 
   | RecdExpr
   

/// If this is Some(ty) then it indicates that a .NET 2.0 constrained call is required, witht he given type as the
/// static type of the object argument.
and ConstrainedCallInfo = TType option

/// Indicates the kind of looping operation.
and SpecialWhileLoopMarker = 
    | NoSpecialWhileLoopMarker
    | WhileLoopForCompiledForEachExprMarker  // marks the compiled form of a 'for ... in ... do ' expression
    
/// Indicates the kind of looping operation.
and ForLoopStyle = 
    /// Evaluate start and end once, loop up
    | FSharpForLoopUp 
    /// Evaluate start and end once, loop down
    | FSharpForLoopDown 
    /// Evaluate start once and end multiple times, loop up
    | CSharpForLoopUp

/// Indicates what kind of pointer operation this is.
and LValueOperation = 
    /// In C syntax this is: &localv            
    | LGetAddr      
    /// In C syntax this is: *localv_ptr        
    | LByrefGet     
    /// In C syntax this is:  localv = e     , note == *(&localv) = e == LGetAddr; LByrefSet
    | LSet          
    /// In C syntax this is: *localv_ptr = e   
    | LByrefSet     

/// Indicates the kind of sequential operation, i.e. "normal" or "to a before returning b"
and SequentialOpKind = 
    /// a ; b 
    | NormalSeq 
    /// let res = a in b;res 
    | ThenDoSeq     

/// Indicates how a value, function or member is being used at a particular usage point.
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
  
/// Indicates the kind of an F# core library static optimization construct
and StaticOptimization = 
    | TTyconEqualsTycon of TType * TType
    | TTyconIsStruct of TType 
  
/// A representation of a method in an object expression. 
///
/// TObjExprMethod(slotsig,attribs,methTyparsOfOverridingMethod,methodParams,methodBodyExpr,m)
and ObjExprMethod = 
    | TObjExprMethod of SlotSig * Attribs * Typars * Val list list * Expr * range
    member x.Id = let (TObjExprMethod(slotsig,_,_,_,_,m)) = x in mkSynId m slotsig.Name

/// Represents an abstract method slot, or delegate signature.
///
/// TSlotSig(methodName,declaringType,declaringTypeParameters,methodTypeParameters,slotParameters,returnTy)
and SlotSig = 
    | TSlotSig of string * TType * Typars * Typars * SlotParam list list * TType option
    member ss.Name             = let (TSlotSig(nm,_,_,_,_,_)) = ss in nm
    member ss.ImplementedType  = let (TSlotSig(_,ty,_,_,_,_)) = ss in ty
    member ss.ClassTypars      = let (TSlotSig(_,_,ctps,_,_,_)) = ss in ctps
    member ss.MethodTypars     = let (TSlotSig(_,_,_,mtps,_,_)) = ss in mtps
    member ss.FormalParams     = let (TSlotSig(_,_,_,_,ps,_)) = ss in ps
    member ss.FormalReturnType = let (TSlotSig(_,_,_,_,_,rt)) = ss in rt

/// Represents a parameter to an abstract method slot. 
///
/// TSlotParam(nm,ty,inFlag,outFlag,optionalFlag,attribs)
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


/// Represents a complete typechecked implementation file, including its typechecked signature if any.
///
/// TImplFile(qualifiedNameOfFile,pragmas,implementationExpressionWithSignature,hasExplicitEntryPoint,isScript)
and TypedImplFile = TImplFile of QualifiedNameOfFile * ScopedPragma list * ModuleOrNamespaceExprWithSig * bool * bool

/// Represents a complete typechecked assembly, made up of multiple implementation files.
///
and TypedAssembly = TAssembly of TypedImplFile list

//---------------------------------------------------------------------------
// Freevars.  Computed and cached by later phases (never computed type checking).  Cached in terms. Not pickled.
//---------------------------------------------------------------------------

/// Represents a set of free local values.
and FreeLocals = Zset<Val>
/// Represents a set of free type parameters
and FreeTypars = Zset<Typar>
/// Represents a set of 'free' named type definitions. Used to collect the named type definitions referred to 
/// from atype or expression.
and FreeTycons = Zset<Tycon>
/// Represents a set of 'free' record field definitions. Used to collect the record field definitions referred to 
/// from an expression.
and FreeRecdFields = Zset<RecdFieldRef>
/// Represents a set of 'free' union cases. Used to collect the union cases referred to from an expression.
and FreeUnionCases = Zset<UnionCaseRef>
/// Represents a set of 'free' type-related elements, including named types, trait solutions, union cases and
/// record fields.
and FreeTyvars = 
    { /// The summary of locally defined type definitions used in the expression. These may be made private by a signature 
      /// and we have to check various conditions associated with that. 
      FreeTycons: FreeTycons

      /// The summary of values used as trait solutions
      FreeTraitSolutions: FreeLocals
      
      /// The summary of type parameters used in the expression. These may not escape the enclosing generic construct 
      /// and we have to check various conditions associated with that. 
      FreeTypars: FreeTypars }


/// Represents an amortized computation of the free variables in an expression
and FreeVarsCache = FreeVars cache

/// Represents the set of free variables in an an expression
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
and [<RequireQualifiedAccess>]
    CompiledTypeRepr = 
    /// An AbstractIL type representation that is just the name of a type.
    ///
    /// CompiledTypeRepr.ILAsmNamed (ilTypeRef, ilBoxity, ilTypeOpt)
    /// 
    /// The ilTypeOpt is present for non-generic types. It is an ILType corresponding to the first two elements of the case. This
    /// prevents reallocation of the ILType each time we need to generate it. For generic types, it is None.
    | ILAsmNamed of 
         ILTypeRef * 
         ILBoxity * 
         ILType option
         
    /// An AbstractIL type representation that may include type variables
    // This case is only used for types defined in the F# library by their translation to ILASM types, e.g.
    //   type ``[]``<'T> = (# "!0[]" #)
    //   type ``[,]``<'T> = (# "!0[0 ...,0 ...]" #)
    //   type ``[,,]``<'T> = (# "!0[0 ...,0 ...,0 ...]" #)
    //   type byref<'T> = (# "!0&" #)
    //   type nativeptr<'T when 'T : unmanaged> = (# "native int" #)
    //   type ilsigptr<'T> = (# "!0*" #)
    | ILAsmOpen of ILType  

//---------------------------------------------------------------------------
// Basic properties on type definitions
//---------------------------------------------------------------------------


/// Metadata on values (names of arguments etc. 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ValReprInfo = 
    let unnamedTopArg1 : ArgReprInfo = { Attribs=[]; Name=None }
    let unnamedTopArg = [unnamedTopArg1]
    let unitArgData : ArgReprInfo list list = [[]]
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
           // REVIEW: this could be faster, e.g. by storing the index in the NameMap 
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
            // REVIEW: this could be faster, e.g. by storing the index in the NameMap 
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
    | TyparKind.Type -> tp.AsType 
    | TyparKind.Measure -> TType_measure (MeasureVar tp)

let copyTypar (tp: Typar) = let x = tp.Data in Typar.New { x with typar_stamp=newStamp() }
let copyTypars tps = List.map copyTypar tps

//--------------------------------------------------------------------------
// Inference variables
//-------------------------------------------------------------------------- 
    
let tryShortcutSolvedUnitPar canShortcut (r:Typar) = 
    if r.Kind = TyparKind.Type then failwith "tryShortcutSolvedUnitPar: kind=type";
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
let mkLocalEntityRef  (v:Entity) = ERefLocal v

let mkNonLocalCcuRootEntityRef ccu (x:Entity) = mkNonLocalTyconRefPreResolved x (mkNonLocalEntityRef ccu [| |]) x.LogicalName

let mkNestedValRef  (cref:EntityRef) (v:Val) : ValRef = 
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
    | ERefNonLocal _ ->
#if EXTENSIONTYPING
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint _ -> true
        | _ -> 
#endif
        compilingFslib
    
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
// pubpath/cpath mess
//---------------------------------------------------------------------------

let stringOfAccess (TAccess paths) = String.concat ";" (List.map mangledTextOfCompPath paths)

let demangledPathOfCompPath (CompPath(_,path)) = 
    path |> List.map (fun (nm,k) -> Entity.DemangleEntityName nm k)

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
let taccessPrivate accessPath = TAccess [accessPath]
let taccessInternal = TAccess [CompPath(ILScopeRef.Local,[])]
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

let NewRigidTypar nm m = NewTypar (TyparKind.Type,TyparRigidity.Rigid,Typar(mkSynId m nm,NoStaticReq,true),false,TyparDynamicReq.Yes,[],false,false)

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
        entity_kind=TyparKind.Type
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
        entity_modul_contents = notlazy (NewEmptyModuleOrNamespaceType ModuleOrType)
        entity_cpath= cpath
        entity_typars=LazyWithContext.NotLazy []
        entity_tycon_abbrev = None
        entity_tycon_repr = TNoRepr
        entity_flags=EntityFlags(usesPrefixDisplay=false, isModuleOrNamespace=false, preEstablishedHasDefaultCtor=false, hasSelfReferentialCtor=false)
        entity_il_repr_cache= newCache()   } 

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
        entity_tycon_repr = TNoRepr
        entity_tycon_repr_accessibility = reprAccess
        entity_exn_info=TExnNone
        entity_tycon_tcaug=TyconAugmentation.Create()
        entity_modul_contents = mtyp
        entity_accessiblity=access
        entity_xmldoc = docOption
        entity_xmldocsig=""        
        entity_pubpath=cpath |> Option.map (publicPathOfCompPath (mkSynId m nm))
        entity_cpath = cpath
        entity_il_repr_cache = newCache() } 


let NewILTycon nlpath (nm,m) tps (scoref:ILScopeRef, enc, tdef:ILTypeDef) mtyp =

    // NOTE: hasSelfReferentialCtor=false is an assumption about mscorlib
    let hasSelfReferentialCtor = tdef.IsClass && (not scoref.IsAssemblyRef && scoref.AssemblyRef.Name = "mscorlib")
    let tycon = NewTycon(nlpath, nm, m, taccessPublic, taccessPublic, TyparKind.Type, tps, XmlDoc.Empty, true, false, hasSelfReferentialCtor, mtyp)

    tycon.Data.entity_tycon_repr <- TILObjModelRepr (scoref,enc,tdef)
    tycon.TypeContents.tcaug_closed <- true
    tycon

exception Duplicate of string * string * range
exception NameClash of string * string * string * range * string * string * range
exception FullAbstraction of string * range

let NewModuleOrNamespace cpath access (id:Ident) xml attribs mtype = Construct.NewModuleOrNamespace cpath access id xml attribs mtype

let NewVal (logicalName:string,m:range,compiledName,ty,isMutable,isCompGen,arity,access,recValInfo,specialRepr,baseOrThis,attribs,inlineInfo,doc,isModuleOrMemberBinding,isExtensionMember,isIncrClassSpecialMember,isTyFunc,allowTypeInst,isGeneratedEventVal,konst,actualParent) : Val = 
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
          val_flags = ValFlags(recValInfo,baseOrThis,isCompGen,inlineInfo,isMutable,isModuleOrMemberBinding,isExtensionMember,isIncrClassSpecialMember,isTyFunc,allowTypeInst,isGeneratedEventVal)
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
        // REVIEW: this is not preserving order as we merge namespace declaration groups
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

//--------------------------------------------------------------------------
// One and zero equality
//--------------------------------------------------------------------------

let equalsValueOne = function
    | Const.Byte    1uy
    | Const.UInt16  1us
    | Const.UInt32  1u
    | Const.UInt64  1UL
    | Const.SByte   1y
    | Const.Int16   1s
    | Const.Int32   1
    | Const.Int64   1L
    | Const.Single  1.f
    | Const.Double  1.0 -> true
    | _ -> false

let equalsValueZero = function
    | Const.Byte    0uy
    | Const.UInt16  0us
    | Const.UInt32  0u
    | Const.UInt64  0UL
    | Const.SByte   0y
    | Const.Int16   0s
    | Const.Int32   0
    | Const.Int64   0L
    | Const.Single  0.f
    | Const.Double  0.0 -> true
    | _ -> false

