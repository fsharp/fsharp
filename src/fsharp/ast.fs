//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2010 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

module internal Microsoft.FSharp.Compiler.Ast

open Internal.Utilities
open Internal.Utilities.Text.Lexing
open Internal.Utilities.Text.Parsing
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.UnicodeLexing 
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Range
/// The prefix of the names used for the fake namespace path added to all dynamic code entries in FSI.EXE
let FsiDynamicModulePrefix = "FSI_"

[<RequireQualifiedAccess>]
module FSharpLib = 
    let Root      = "Microsoft.FSharp"
    let RootPath  = IL.splitNamespace Root 
    let Core      = Root ^ ".Core"
    let CorePath  = IL.splitNamespace Core


//------------------------------------------------------------------------
// XML doc pre-processing
//-----------------------------------------------------------------------


type XmlDocCollector() =
    let mutable savedLines = new ResizeArray<(string * pos)>()
    let mutable savedGrabPoints = new ResizeArray<pos>()
    let posCompare p1 p2 = if posGeq p1 p2 then 1 else if posEq p1 p2 then 0 else -1
    let savedGrabPointsAsArray = 
        lazy (savedGrabPoints.ToArray() |> Array.sortWith posCompare)

    let savedLinesAsArray = 
        lazy (savedLines.ToArray() |> Array.sortWith (fun (_,p1) (_,p2) -> posCompare p1 p2))

    let check() = 
        assert (not savedLinesAsArray.IsValueCreated && "can't add more XmlDoc elements to XmlDocCOllector after extracting first XmlDoc from the overall results" <> "")

    member x.AddGrabPoint(pos) = 
        check()
        savedGrabPoints.Add pos 

    member x.AddXmlDocLine(line,pos) = 
        check()
        savedLines.Add(line,pos)

    member x.LinesBefore(grabPointPos) = 
                        
        let lines = savedLinesAsArray.Force()
        let grabPoints = savedGrabPointsAsArray.Force()
        let firstLineIndexAfterGrabPoint = Array.findFirstIndexWhereTrue lines (fun (_,pos) -> posGeq pos grabPointPos) 
        let grabPointIndex = Array.findFirstIndexWhereTrue grabPoints (fun pos -> posGeq pos grabPointPos) 
        assert (posEq grabPoints.[grabPointIndex] grabPointPos)
        let firstLineIndexAfterPrevGrabPoint = 
            if grabPointIndex = 0 then 
                0 
            else
                let prevGrabPointPos = grabPoints.[grabPointIndex-1]
                Array.findFirstIndexWhereTrue lines (fun (_,pos) -> posGeq pos prevGrabPointPos) 
        //printfn "#lines = %d, firstLineIndexAfterPrevGrabPoint = %d, firstLineIndexAfterGrabPoint = %d" lines.Length firstLineIndexAfterPrevGrabPoint  firstLineIndexAfterGrabPoint
        lines.[firstLineIndexAfterPrevGrabPoint..firstLineIndexAfterGrabPoint-1] |> Array.map fst

    
type XmlDoc = 
    | XmlDoc of string[]
    static member Empty = XmlDocStatics.Empty
    static member Merge (XmlDoc lines) (XmlDoc lines') = XmlDoc (Array.append lines lines')
    static member Process (XmlDoc lines) = 
        // This code runs for .XML generation and thus influences cross-project xmldoc tooltips; for within-project tooltips, see XmlDocumentation.fs in the language service
        let rec processLines (lines:string list) =
            match lines with 
            | [] -> []
            | (lineA::rest) as lines ->
                let lineAT = lineA.TrimStart([|' '|])
                if lineAT = "" then processLines rest
                else if String.hasPrefix lineAT "<" then lines
                else ["<summary>"] @
                     (lines |> List.map (fun line -> System.Security.SecurityElement.Escape(line))) @
                     ["</summary>"]               

        let lines = processLines (Array.toList lines)
        if lines.Length = 0 then XmlDoc.Empty 
        else XmlDoc (Array.ofList lines)

and XmlDocStatics() = 
    static let empty = XmlDoc[| |]
    static member Empty = empty

type PreXmlDoc = 
    | PreXmlMerge of PreXmlDoc * PreXmlDoc
    | PreXmlDoc of pos * XmlDocCollector
    | PreXmlDocEmpty 

    member x.ToXmlDoc() = 
        match x with 
        | PreXmlMerge(a,b) -> XmlDoc.Merge (a.ToXmlDoc()) (b.ToXmlDoc())
        | PreXmlDocEmpty -> XmlDoc.Empty
        | PreXmlDoc (pos,collector) -> 
            let lines = collector.LinesBefore pos
            if lines.Length = 0 then XmlDoc.Empty
            else XmlDoc lines

    static member CreateFromGrabPoint(collector:XmlDocCollector,grabPointPos) = 
        collector.AddGrabPoint grabPointPos
        PreXmlDoc(grabPointPos,collector)

    static member Empty = PreXmlDocEmpty 
    static member Merge a b = PreXmlMerge (a,b)



//------------------------------------------------------------------------
//  AST: identifiers and long identifiers
//-----------------------------------------------------------------------


// PERFORMANCE: consider making this a struct.
[<System.Diagnostics.DebuggerDisplay("{idText}")>]
[<Sealed>]
[<NoEquality; NoComparison>]
type Ident (text,range) = 
     member x.idText = text
     member x.idRange = range
     override x.ToString() = text

type LongIdent = Ident list

//------------------------------------------------------------------------
//  AST: the grammar of implicitly scoped type parameters 
//-----------------------------------------------------------------------

type TyparStaticReq = 
    | NoStaticReq 
    | HeadTypeStaticReq 

[<NoEquality; NoComparison>]
type SynTypar = 
    | Typar of Ident * TyparStaticReq * (* compgen: *) bool 

//------------------------------------------------------------------------
//  AST: the grammar of constants and measures
//-----------------------------------------------------------------------

type 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynConst = 
    | Unit
    | Bool of bool
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | IntPtr of int64
    | UIntPtr of uint64
    | Single of single
    | Double of double
    | Char of char
    | Decimal of System.Decimal
    | UserNum of ( (* value: *) string * (* suffix: *) string)
    | String of string * range 
    | Bytes of byte[] * range 
    | UInt16s of uint16[] 
    | Measure of SynConst * SynMeasure (* we never iterate, so the const here is not another SynConst.Measure *)
    member c.Range dflt = 
        match c with 
        | SynConst.String (_,m0) | SynConst.Bytes (_,m0) -> m0 
        | _ -> dflt
      
and  
    [<NoEquality; NoComparison>]
    SynMeasure = 
    | Named of LongIdent * range
    | Product of SynMeasure * SynMeasure * range
    | Seq of SynMeasure list * range
    | Divide of SynMeasure * SynMeasure * range
    | Power of SynMeasure * int * range
    | One 
    | Anon of range
    | Var of SynTypar * range

//------------------------------------------------------------------------
//  AST: the grammar of types, expressions, declarations etc.
//-----------------------------------------------------------------------

[<RequireQualifiedAccess>]
type SynAccess = 
    | Public
    | Internal
    | Private
    //Access of int  (* 0 = public, 1 = assembly, 2 = outer module etc. *)


type SequencePointInfoForTarget = 
    | SequencePointAtTarget
    | SuppressSequencePointAtTarget

type SequencePointInfoForSeq = 
    | SequencePointsAtSeq
    // This means "suppress a in 'a;b'" and "suppress b in 'a before b'"
    | SuppressSequencePointOnExprOfSequential
    // This means "suppress b in 'a;b'" and "suppress a in 'a before b'"
    | SuppressSequencePointOnStmtOfSequential

type SequencePointInfoForTry = 
    | SequencePointAtTry of range
    // Used for "use" and "for"
    | SequencePointInBodyOfTry 
    | NoSequencePointAtTry

type SequencePointInfoForWith = 
    | SequencePointAtWith of range
    | NoSequencePointAtWith

type SequencePointInfoForFinally = 
    | SequencePointAtFinally of range
    | NoSequencePointAtFinally

type SequencePointInfoForForLoop = 
    | SequencePointAtForLoop of range
    | NoSequencePointAtForLoop
    
type SequencePointInfoForWhileLoop = 
    | SequencePointAtWhileLoop of range
    | NoSequencePointAtWhileLoop
    
type SequencePointInfoForBinding = 
    | SequencePointAtBinding of range
    // Indicates the ommission of a sequence point for a binding for a 'do expr' 
    | NoSequencePointAtDoBinding
    // Indicates the ommission of a sequence point for a binding for a 'let e = expr' where 'expr' has immediate control flow
    | NoSequencePointAtLetBinding
    // Indicates the ommission of a sequence point for a compiler generated binding
    // where we've done a local expansion of some construct into something that involves
    // a 'let'. e.g. we've inlined a function and bound its arguments using 'let'
    // The let bindings are 'sticky' in that the inversion of the inlining would involve
    // replacing the entire expression with the original and not just the let bindings alone.
    | NoSequencePointAtStickyBinding
    // Given 'let v = e1 in e2', where this is a compiler generated binding, 
    // we are sometimes forced to generate a sequence point for the expression anyway based on its
    // overall range. If the let binding is given the flag below then it is asserting that
    // the binding has no interesting side effects and can be totally ignored and the range
    // of the inner expression is used instead
    | NoSequencePointAtInvisibleBinding
    
    // Don't drop sequence points when combining sequence points
    member x.Combine(y:SequencePointInfoForBinding) = 
        match x,y with 
        | SequencePointAtBinding _ as g, _  -> g
        | _, (SequencePointAtBinding _ as g)  -> g
        | _ -> x

type SeqExprOnly = 
    | SeqExprOnly of bool

type ExprAtomicFlag =
    | Atomic = 0
    | NonAtomic = 1

type SynBindingKind = 
    | StandaloneExpression
    | NormalBinding
    | DoBinding
  
type
    [<NoEquality; NoComparison>]
    SynTyparDecl = 
    | TyparDecl of SynAttributes * SynTypar


and 
    [<NoEquality; NoComparison>]
    SynTypeConstraint =
    | WhereTyparIsValueType of SynTypar * range
    | WhereTyparIsReferenceType of SynTypar * range
    | WhereTyparIsUnmanaged of SynTypar * range
    | WhereTyparSupportsNull of SynTypar * range
    | WhereTyparIsComparable of SynTypar * range
    | WhereTyparIsEquatable of SynTypar * range
    | WhereTyparDefaultsToType of SynTypar * SynType * range
    | WhereTyparEqualsType of SynTypar *  SynType * range
    | WhereTyparSubtypeOfType of SynTypar *  SynType * range
    | WhereTyparSupportsMember of SynTypar list * SynMemberSig * range
    | WhereTyparIsEnum of SynTypar * SynType list * range
    | WhereTyparIsDelegate of SynTypar * SynType list * range

and 
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
    SynType =
    | LongIdent of LongIdent * range
    | App of SynType * SynType list * bool * range // the bool is true if this is a postfix type application e.g. "int list" or "(int,string) dict"
    | LongIdentApp of SynType * LongIdent * SynType list * range
    | Tuple of (bool*SynType) list * range    // the bool is true if / rather than * follows the type
    | Array of  int * SynType * range
    | Lazy of  SynType * range
    | Fun of  SynType * SynType * range
    | Var of SynTypar * range
    | Anon of range
    | WithGlobalConstraints of SynType * SynTypeConstraint list * range
    | HashConstraint of SynType * range
    /// For units of measure e.g. m / s 
    | MeasureDivide of SynType * SynType * range       
    /// For units of measure e.g. m^3 
    | MeasurePower of SynType * int * range      
    /// For the dimensionless units i.e. 1 
    | MeasureOne of range          
    member x.Range = 
        match x with 
        | SynType.LongIdent(_,m) | SynType.App(_,_,_,m) | SynType.LongIdentApp(_,_,_,m) | SynType.Tuple(_,m) | SynType.Lazy(_,m) | SynType.Array(_,_,m) | SynType.Fun(_,_,m)
        | SynType.Var(_,m) | SynType.Anon m | SynType.WithGlobalConstraints(_,_,m)
        | SynType.HashConstraint(_,m) | SynType.MeasureDivide(_,_,m) | SynType.MeasurePower(_,_,m) | SynType.MeasureOne m -> m

    
and  
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
    SynExpr =
    /// Parenthesized expressions. Kept in AST to distinguish A.M((x,y)) 
    /// from A.M(x,y), among other things.
    | Paren of SynExpr * range  
    /// <@ expr @>, <@@ expr @@>
    | Quote of SynExpr * bool * SynExpr * range 
    /// 1, 1.3, () etc.
    | Const of SynConst * range
    /// expr : type
    | Typed of  SynExpr * SynType * range
    /// e1, ..., eN
    | Tuple of  SynExpr list * range
    /// [ e1; ...; en ], [| e1; ...; en |]
    | ArrayOrList of  bool * SynExpr list * range 
    /// { f1=e1; ...; fn=en }
    | Record of (SynType * SynExpr * range) option * SynExpr option * ((LongIdent * Ident) * SynExpr) list * range
    /// new C(...)
    /// The flag is true if known to be 'family' ('protected') scope 
    | New of bool * SynType * SynExpr * range 
    /// { new ... with ... }
    | ObjExpr of SynType * (SynExpr * Ident option) option * SynBinding list * SynInterfaceImpl list * range
    /// 'while ... do ...'
    | While of SequencePointInfoForWhileLoop * SynExpr * SynExpr * range
    /// 'for i = ... to ... do ...'
    | For of SequencePointInfoForForLoop * Ident * SynExpr * bool * SynExpr * SynExpr * range
    /// 'for ... in ... do ...'
    | ForEach of SequencePointInfoForForLoop * SeqExprOnly * SynPat * SynExpr * SynExpr * range
    /// [ expr ], [| expr |]
    | ArrayOrListOfSeqExpr of bool * SynExpr * range
    /// { expr }
    | CompExpr of bool * bool ref * SynExpr * range
    /// First bool indicates if lambda originates from a method. Patterns here are always "simple" 
    /// Second bool indicates if this is a "later" part of an iterated sequence of lambdas
    | Lambda of  bool * bool * SynSimplePats * SynExpr * range 
    | Match of  SequencePointInfoForBinding * SynExpr * SynMatchClause list * bool * range (* bool indicates if this is an exception match in a computation expression which throws unmatched exceptions *)
    | Do of  SynExpr * range
    | Assert of SynExpr * range
    | App of ExprAtomicFlag * SynExpr * SynExpr * range
    | TypeApp of SynExpr * SynType list * range
    /// let pat = expr in expr 
    /// let f pat1 .. patN = expr in expr 
    /// let rec f pat1 .. patN = expr in expr 
    /// use pat = expr in expr 
    | LetOrUse of (* isRecursive: *) bool * (* isUse: *) bool * SynBinding list * SynExpr * range
    /// try expr with pat -> expr
    | TryWith of SynExpr * range * SynMatchClause list * range * range * SequencePointInfoForTry * SequencePointInfoForWith
    /// try expr finally expr
    | TryFinally of SynExpr * SynExpr * range * SequencePointInfoForTry * SequencePointInfoForFinally
    | Lazy of SynExpr * range
    | Seq of SequencePointInfoForSeq * bool * SynExpr * SynExpr * range (* false for first flag indicates "do a then b then return a" *)
    | IfThenElse of SynExpr * SynExpr * SynExpr option * SequencePointInfoForBinding * range * range

    /// Optimized representation, = SynExpr.LongIdent(false,[id],id.idRange) 
    | Ident of Ident 
    // bool true if preceded by a '?' for an optional named parameter 
    | LongIdent of bool * LongIdent * range  
    | LongIdentSet of LongIdent * SynExpr * range
    | DotGet of SynExpr * LongIdent * range
    | DotSet of SynExpr * LongIdent * SynExpr * range
    | DotIndexedGet of SynExpr * SynExpr list * range * range
    | DotIndexedSet of SynExpr * SynExpr list * SynExpr * range * range
    /// Type.Items(e1) <- e2 , rarely used named-property-setter notation, e.g. str.Chars(3) <- 'a'
    | NamedIndexedPropertySet of LongIdent * SynExpr * SynExpr * range
    /// expr.Items(e1) <- e2 , rarely used named-property-setter notation, e.g. str.Chars(3) <- 'a'
    | DotNamedIndexedPropertySet of SynExpr * LongIdent * SynExpr * SynExpr * range

    | TypeTest of  SynExpr * SynType * range
    | Upcast of  SynExpr * SynType * range
    | Downcast of  SynExpr * SynType * range
    | InferredUpcast of  SynExpr * range
    | InferredDowncast of  SynExpr * range
    | Null of range

    | AddressOf of  bool * SynExpr * range * range
    | TraitCall of SynTypar list * SynMemberSig * SynExpr * range

    /// Computation expressions only, implied by final "do" or "do!"
    | ImplicitZero of range 
    /// Computation expressions only
    | YieldOrReturn   of (bool * bool) * SynExpr * range
    /// Computation expressions only
    | YieldOrReturnFrom  of (bool * bool) * SynExpr * range
    /// Computation expressions only
    | LetOrUseBang    of SequencePointInfoForBinding * bool * SynPat * SynExpr * SynExpr * range
    /// Computation expressions only
    | DoBang      of SynExpr * range

    /// (type int) -- deprecated
    | DeprecatedTypeOf of SynType * range
    /// Only used in FSharp.Core
    | LibraryOnlyILAssembly of ILInstr array *  SynType list * SynExpr list * SynType list * range (* Embedded IL assembly code *)
    /// Only used in FSharp.Core
    | LibraryOnlyStaticOptimization of SynStaticOptimizationConstraint list * SynExpr * SynExpr * range
    /// Only used in FSharp.Core
    | LibraryOnlyUnionCaseFieldGet of SynExpr * LongIdent * int * range
    /// Only used in FSharp.Core
    | LibraryOnlyUnionCaseFieldSet of SynExpr * LongIdent * int * SynExpr * range
  
    /// Inserted for error recovery
    | ArbitraryAfterError  of range  
    /// Inserted for error recovery
    | DiscardAfterError  of SynExpr * range  

and  
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynSimplePat =
    /// Id (ident, isCompilerGenerated, isThisVar, isOptArg, range)
    ///
    /// Indicates a simple pattern variable.
    ///
    ///   isCompilerGenerated : true if a compiler generated name 
    ///   isThisVar: true if 'this' variable in member  
    ///   isOptArg: true if a '?' is in front of the name
    | Id of  Ident * bool * bool *  bool * range
    | Typed of  SynSimplePat * SynType * range
    | Attrib of  SynSimplePat * SynAttributes * range

and 
    [<NoEquality; NoComparison>]
    SynStaticOptimizationConstraint =
    | WhenTyparTyconEqualsTycon of SynTypar *  SynType * range
    | WhenTyparIsStruct of SynTypar * range
    | WhenInlined of range

and  
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
    SynSimplePats =
    | SimplePats of SynSimplePat list * range
    | Typed of  SynSimplePats * SynType * range

and  
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
    SynPat =
    | Const of SynConst * range
    | Wild of range
    | Named of  SynPat * Ident *  bool (* true if 'this' variable *)  * SynAccess option * range
    | Typed of  SynPat * SynType * range
    | Attrib of  SynPat * SynAttributes * range
    | Or of  SynPat * SynPat * range
    | Ands of  SynPat list * range
    | LongIdent of LongIdent * (* holds additional ident for tooling *) Ident option * SynValTyparDecls option (* usually None: temporary used to parse "f<'a> x = x"*) * SynPat list  * SynAccess option * range
    | Tuple of  SynPat list * range
    | Paren of  SynPat * range
    | ArrayOrList of  bool * SynPat list * range
    | Record of ((LongIdent * Ident) * SynPat) list * range
    /// 'null'
    | Null of range
    /// '?id' -- for optional argument names
    | OptionalVal of Ident * range
    /// ':? type '
    | IsInst of SynType * range
    /// <@ expr @>
    | QuoteExpr of SynExpr * range

    /// Deprecated character ranges
    | DeprecatedCharRange of char * char * range
    /// Used internally in the type checker
    | InstanceMember of  Ident * Ident * (* holds additional ident for tooling *) Ident option * SynAccess option * range (* adhoc overloaded method/property *)

    member p.Range = 
      match p with 
      | SynPat.Const(_,m) | SynPat.Wild m | SynPat.Named (_,_,_,_,m) | SynPat.Or (_,_,m) | SynPat.Ands (_,m) 
      | SynPat.LongIdent (_,_,_,_,_,m) | SynPat.ArrayOrList(_,_,m) | SynPat.Tuple (_,m) |SynPat.Typed(_,_,m) |SynPat.Attrib(_,_,m) 
      | SynPat.Record (_,m) | SynPat.DeprecatedCharRange (_,_,m) | SynPat.Null m | SynPat.IsInst (_,m) | SynPat.QuoteExpr (_,m)
      | SynPat.InstanceMember(_,_,_,_,m) | SynPat.OptionalVal(_,m) | SynPat.Paren(_,m) -> m 

and  
    [<NoEquality; NoComparison>]
    SynInterfaceImpl = 
    | InterfaceImpl of SynType * SynBinding list * range

and  
    [<NoEquality; NoComparison>]
    SynMatchClause = 
    | Clause of SynPat * SynExpr option *  SynExpr * range * SequencePointInfoForTarget

and SynAttributes = SynAttribute list

and  
    [<NoEquality; NoComparison>]
    SynAttribute = 
    { TypeName: LongIdent;
      ArgExpr: SynExpr 
      /// Target specifier, e.g. "assembly","module",etc.
      Target: Ident option 
      /// Is this attribute being applied to a property getter or setter?
      AppliesToGetterAndSetter: bool
      Range: range } 

and  
    [<NoEquality; NoComparison>]
    SynValData = 
    | SynValData of MemberFlags option * SynValInfo * Ident option

and  
    [<NoEquality; NoComparison>]
    SynBinding = 
    | Binding of 
        SynAccess option *
        SynBindingKind *  
        bool (* mustinline: *) *  
        bool (* mutable: *) *  
        SynAttributes * 
        PreXmlDoc *
        SynValData * 
        SynPat * 
        SynBindingReturnInfo option * 
        SynExpr  *
        range *
        SequencePointInfoForBinding

    member x.RangeOfHeadPat = let (Binding(_,_,_,_,_,_,_,headPat,_,_,_,_)) = x in headPat.Range

and 
    [<NoEquality; NoComparison>]
    SynBindingReturnInfo = 
    | SynBindingReturnInfo of SynType * range * SynAttributes


and 
    [<NoComparison>]
    MemberFlags =
    { IsInstance: bool;
      IsDispatchSlot: bool;
      IsOverrideOrExplicitImpl: bool;
      IsFinal: bool;
      MemberKind: MemberKind }

/// Note the member kind is actually computed partially by a syntax tree transformation in tc.fs
and 
    [<StructuralEquality; NoComparison; RequireQualifiedAccess>]
    MemberKind = 
    | ClassConstructor
    | Constructor
    | Member 
    | PropertyGet 
    | PropertySet    
    | PropertyGetSet    

and 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynMemberSig = 
    | Member of SynValSig  * MemberFlags * range 
    | Interface of SynType  * range
    | Inherit of SynType * range
    | ValField of SynField  * range
    | NestedType  of SynTypeDefnSig * range

and SynMemberSigs = SynMemberSig list

and 
    [<NoEquality; NoComparison>]
    SynTypeDefnKind = 
    | TyconUnspecified 
    | TyconClass 
    | TyconInterface 
    | TyconStruct 
    | TyconRecord
    | TyconUnion
    | TyconAbbrev
    | TyconHiddenRepr
    | TyconAugmentation
    | TyconILAssemblyCode
    | TyconDelegate of SynType * SynValInfo


and 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynTypeDefnSimpleRepr =
    | Union      of SynAccess option * SynUnionCases * range
    | Enum       of SynEnumCases * range
    | Record     of SynAccess option * SynFields * range
    | General    of SynTypeDefnKind * (SynType * range * Ident option) list * (SynValSig * MemberFlags) list * SynFields * bool * bool * SynSimplePat list option * range 
    | ILAssembly of ILType * range
    | TypeAbbrev of SynType * range
    | None       of range

and SynEnumCases = SynEnumCase list

and 
    [<NoEquality; NoComparison>]
    SynEnumCase =
    | EnumCase of SynAttributes * Ident * SynConst * PreXmlDoc * range

and SynUnionCases = SynUnionCase list

and 
    [<NoEquality; NoComparison>]
    SynUnionCase = 
    | UnionCase of SynAttributes * Ident * SynUnionCaseType * PreXmlDoc * SynAccess option * range

and 
    [<NoEquality; NoComparison>]
    SynUnionCaseType = 
    /// Normal ML-style declaration 
    | UnionCaseFields of SynField list      
    /// Full type spec given by 'UnionCase : ty1 * tyN -> rty' 
    | UnionCaseFullType of (SynType * SynValInfo) 

and 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynTypeDefnSigRepr =
    | ObjectModel of SynTypeDefnKind * SynMemberSigs * range
    | Simple of SynTypeDefnSimpleRepr * range 

and 
    [<NoEquality; NoComparison>]
    SynTypeDefnSig =
    /// The information for a type definition in a signature
    | TypeDefnSig of SynComponentInfo * SynTypeDefnSigRepr * SynMemberSigs * range

and SynFields = SynField list

and 
    [<NoEquality; NoComparison>]
    /// The information for a field declaration in a record or class
    SynField = 
    | Field of SynAttributes * (* static: *) bool * Ident option * SynType * bool * PreXmlDoc * SynAccess option * range


and 
    [<NoEquality; NoComparison>]
    /// The names, attributes, type parameters, constraints, documentation and accessibility 
    /// for a type definition or module. For modules, entries such as the type parameters are
    /// always empty.
    SynComponentInfo = 
    | ComponentInfo of SynAttributes * SynTyparDecl list * SynTypeConstraint list * LongIdent * PreXmlDoc * (* preferPostfix: *) bool * SynAccess option * range

and 
    [<NoEquality; NoComparison>]
    SynValSig = 
    | ValSpfn of 
        SynAttributes * 
        Ident * 
        SynValTyparDecls * 
        SynType * 
        SynValInfo * 
        bool * 
        bool *  (* mutable? *) 
        PreXmlDoc * 
        SynAccess option *
        SynExpr option *
        range 

    member x.RangeOfId  = let (ValSpfn(_,id,_,_,_,_,_,_,_,_,_)) = x in id.idRange
    member x.SynInfo = let (ValSpfn(_,_,_,_,v,_,_,_,_,_,_)) = x in v
    member x.SynType = let (ValSpfn(_,_,_,ty,_,_,_,_,_,_,_)) = x in ty

/// The argument names and other metadata for a member or function
and 
    [<NoEquality; NoComparison>]
    SynValInfo = 
    | SynValInfo of (*args:*) SynArgInfo list list * (*return:*) SynArgInfo 
    member x.ArgInfos = (let (SynValInfo(args,_)) = x in args)

/// The argument names and other metadata for a parameter for a member or function
and 
    [<NoEquality; NoComparison>]
    SynArgInfo = 
    | SynArgInfo of SynAttributes * (*optional:*) bool *  Ident option

/// The names and other metadata for the type parameters for a member or function
and 
    [<NoEquality; NoComparison>]
    SynValTyparDecls = 
    | SynValTyparDecls of SynTyparDecl list * bool * SynTypeConstraint list

/// 'exception E = ... '
and [<NoEquality; NoComparison>]
    SynExceptionRepr = 
    | ExceptionDefnRepr of SynAttributes * SynUnionCase * LongIdent option * PreXmlDoc * SynAccess option * range

/// 'exception E = ... with ...'
and 
    [<NoEquality; NoComparison>]
    SynExceptionDefn = 
    | ExceptionDefn of SynExceptionRepr * SynMemberDefns * range

and 
    [<NoEquality; NoComparison>]
    SynTypeDefnRepr =
    | ObjectModel  of SynTypeDefnKind * SynMemberDefns * range
    | Simple of SynTypeDefnSimpleRepr * range

and 
    [<NoEquality; NoComparison>]
    SynTypeDefn =
    | TypeDefn of SynComponentInfo * SynTypeDefnRepr * SynMemberDefns * range

and 
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynMemberDefn = 
    | Open of LongIdent * range
    | Member of SynBinding * range                          
    /// implicit ctor args as a defn line, 'as' specification 
    | ImplicitCtor of SynAccess option * SynAttributes * SynSimplePat list * Ident option * range    
    /// inherit <typ>(args...) as base 
    | ImplicitInherit of SynType * SynExpr * Ident option * range   
    /// localDefns 
    | LetBindings of SynBinding list * (* static: *) bool * (* recursive: *) bool * range                    
    | AbstractSlot of SynValSig * MemberFlags * range 
    | Interface of SynType * SynMemberDefns option  * range
    | Inherit of SynType  * Ident option * range
    | ValField of SynField  * range
    | NestedType of SynTypeDefn * SynAccess option * range
      
and SynMemberDefns = SynMemberDefn list

and 
    [<NoEquality; NoComparison;RequireQualifiedAccess>]
    SynModuleDecl =
    | ModuleAbbrev of Ident * LongIdent * range
    | NestedModule of SynComponentInfo * SynModuleDecls * bool * range
    | Let of bool * SynBinding list * range (* first flag recursion, second flag must-inline *)
    | DoExpr of SequencePointInfoForBinding * SynExpr * range 
    | Types of SynTypeDefn list * range
    | Exception of SynExceptionDefn * range
    | Open of LongIdent * range
    | Attributes of SynAttributes * range
    | HashDirective of ParsedHashDirective * range
    | NamespaceFragment of SynModuleOrNamespace 

and SynModuleDecls = SynModuleDecl list

and 
    [<NoEquality; NoComparison>]
    SynExceptionSig = 
    | ExceptionSig of SynExceptionRepr * SynMemberSigs * range

and
    [<NoEquality; NoComparison; RequireQualifiedAccess>]
    SynModuleSigDecl =
    | ModuleAbbrev      of Ident * LongIdent * range
    | NestedModule      of SynComponentInfo * SynModuleSigDecls * range
    | Val               of SynValSig * range
    | Types             of SynTypeDefnSig list * range
    | Exception         of SynExceptionSig * range
    | Open              of LongIdent * range
    | HashDirective     of ParsedHashDirective * range
    | NamespaceFragment of SynModuleOrNamespaceSig 

and SynModuleSigDecls = SynModuleSigDecl list

/// ModuleOrNamespace(lid,isModule,decls,xmlDoc,attribs,SynAccess,m)
and 
    [<NoEquality; NoComparison>]
    SynModuleOrNamespace = 
    | ModuleOrNamespace of LongIdent * (*isModule:*) bool * SynModuleDecls * PreXmlDoc * SynAttributes * SynAccess option * range 

and 
    [<NoEquality; NoComparison>]
    SynModuleOrNamespaceSig = 
    | ModuleOrNamespaceSig of LongIdent * (*isModule:*) bool * SynModuleSigDecls * PreXmlDoc * SynAttributes * SynAccess option * range 

and [<NoEquality; NoComparison>]
    ParsedHashDirective = 
    | ParsedHashDirective of string * string list * range

[<NoEquality; NoComparison>]
type ParsedImplFileFragment = 
    | AnonTopModule of SynModuleDecls * range
    | NamedTopModule of SynModuleOrNamespace
    | NamespaceFragment of LongIdent * bool * SynModuleDecls * PreXmlDoc * SynAttributes * range

[<NoEquality; NoComparison>]
type ParsedSigFileFragment = 
    | AnonTopModuleSig of SynModuleSigDecls * range
    | NamedTopModuleSig of SynModuleOrNamespaceSig
    | NamespaceFragmentSig of LongIdent * bool * SynModuleSigDecls * PreXmlDoc * SynAttributes * range

[<NoEquality; NoComparison>]
type ParsedFsiInteraction =
    | IDefns of SynModuleDecl list * range
    | IHash  of ParsedHashDirective * range

[<NoEquality; NoComparison>]
type ParsedImplFile = 
    | ParsedImplFile of ParsedHashDirective list * ParsedImplFileFragment list

[<NoEquality; NoComparison>]
type ParsedSigFile = 
    | ParsedSigFile of ParsedHashDirective list * ParsedSigFileFragment list

//----------------------------------------------------------------------
// AST and parsing utilities.
//----------------------------------------------------------------------

type path = string list 
let ident (s,r) = new Ident(s,r)
let textOfId (id:Ident) = id.idText
let pathOfLid lid = List.map textOfId lid
let arrPathOfLid lid = Array.ofList (List.map textOfId lid)
let textOfPath path = String.concat "." path
let textOfArrPath path = String.concat "." (List.ofArray path)
let textOfLid lid = textOfPath (pathOfLid lid)

let rangeOfLid (lid: Ident list) = 
    match lid with 
    | [] -> failwith "rangeOfLid"
    | [id] -> id.idRange
    | h::t -> unionRanges h.idRange (List.last t).idRange 


type ScopedPragma = 
   | WarningOff of range * int
   // Note: this type may be extended in the future with optimization on/off switches etc.

// These are the results of parsing + folding in the implicit file name
/// ImplFile(modname,isScript,qualName,hashDirectives,modules,isLastCompiland)

/// QualifiedNameOfFile acts to fully-qualify module specifications and implementations, 
/// most importantly the ones that simply contribute fragments to a namespace (i.e. the NamespaceFragmentSig case) 
/// There may be multiple such fragments in a single assembly.  There may thus also 
/// be multiple matching pairs of these in an assembly, all contributing types to the same 
/// namespace. 
[<NoEquality; NoComparison>]
type QualifiedNameOfFile = 
    | QualifiedNameOfFile of Ident 
    member x.Text = (let (QualifiedNameOfFile(t)) = x in t.idText)
    member x.Id = (let (QualifiedNameOfFile(t)) = x in t)
    member x.Range = (let (QualifiedNameOfFile(t)) = x in t.idRange)

[<NoEquality; NoComparison>]
type ImplFile = ImplFile of string * (*isScript: *) bool * QualifiedNameOfFile * ScopedPragma list * ParsedHashDirective list * SynModuleOrNamespace list * bool

[<NoEquality; NoComparison>]
type SigFile = SigFile of string * QualifiedNameOfFile * ScopedPragma list * ParsedHashDirective list * SynModuleOrNamespaceSig list

[<NoEquality; NoComparison>]
type Input = 
    | ImplFileInput of ImplFile
    | SigFileInput of SigFile

    member inp.Range = 
        match inp with
        | ImplFileInput (ImplFile(_,_,_,_,_,(ModuleOrNamespace(_,_,_,_,_,_,m) :: _),_))
        | SigFileInput (SigFile(_,_,_,_,(ModuleOrNamespaceSig(_,_,_,_,_,_,m) :: _))) -> m
        | ImplFileInput (ImplFile(filename,_,_,_,_,[],_))
        | SigFileInput (SigFile(filename,_,_,_,[])) ->
#if DEBUG      
            assert("" = "compiler expects ImplFileInput and SigFileInput to have at least one fragment, 4488")
#endif    
            rangeN filename 0 (* There are no implementations, e.g. due to errors, so return a default range for the file *)


//----------------------------------------------------------------------
// Construct syntactic AST nodes
//-----------------------------------------------------------------------

type GlobalSynArgNameGenerator() = 
    // ++GLOBAL MUTABLE STATE
    static let count = ref 0 
    static let generatedArgNamePrefix = "_arg"

    static member New() : string = incr count; generatedArgNamePrefix + string !count
    static member Reset() = count := 0

//----------------------------------------------------------------------
// Construct syntactic AST nodes
//-----------------------------------------------------------------------


let mkSynId m s = Ident(s,m)
let pathToSynLid m p = List.map (mkSynId m) p
let mkSynIdGet m n = SynExpr.Ident(mkSynId m n)

let mkSynSimplePatVar isOpt id = SynSimplePat.Id (id,false,false,isOpt,id.idRange)
let mkSynCompGenSimplePatVar id = SynSimplePat.Id (id,true,false,false,id.idRange)

type SynExpr with 
    member e.Range = 
        match e with 
        | SynExpr.Paren(_,m) 
        | SynExpr.Quote(_,_,_,m) 
        | SynExpr.Const(_,m) 
        | SynExpr.Typed (_,_,m)
        | SynExpr.Tuple (_,m)
        | SynExpr.ArrayOrList (_,_,m)
        | SynExpr.Record (_,_,_,m)
        | SynExpr.New (_,_,_,m)
        | SynExpr.ObjExpr (_,_,_,_,m)
        | SynExpr.While (_,_,_,m)
        | SynExpr.For (_,_,_,_,_,_,m)
        | SynExpr.ForEach (_,_,_,_,_,m)
        | SynExpr.CompExpr (_,_,_,m)
        | SynExpr.ArrayOrListOfSeqExpr (_,_,m)
        | SynExpr.Lambda (_,_,_,_,m)
        | SynExpr.Match (_,_,_,_,m)
        | SynExpr.Do (_,m)
        | SynExpr.Assert (_,m)
        | SynExpr.App (_,_,_,m)
        | SynExpr.TypeApp (_,_,m)
        | SynExpr.LetOrUse (_,_,_,_,m)
        | SynExpr.TryWith (_,_,_,_,m,_,_)
        | SynExpr.TryFinally (_,_,m,_,_)
        | SynExpr.Seq (_,_,_,_,m)
        | SynExpr.ArbitraryAfterError m
        | SynExpr.DiscardAfterError (_,m) 
        | SynExpr.IfThenElse (_,_,_,_,_,m)
        | SynExpr.LongIdent (_,_,m)
        | SynExpr.LongIdentSet (_,_,m)
        | SynExpr.NamedIndexedPropertySet (_,_,_,m)
        | SynExpr.DotIndexedGet (_,_,_,m)
        | SynExpr.DotIndexedSet (_,_,_,_,m)
        | SynExpr.DotGet (_,_,m)
        | SynExpr.DotSet (_,_,_,m)
        | SynExpr.DotNamedIndexedPropertySet (_,_,_,_,m)
        | SynExpr.LibraryOnlyUnionCaseFieldGet (_,_,_,m)
        | SynExpr.LibraryOnlyUnionCaseFieldSet (_,_,_,_,m)
        | SynExpr.LibraryOnlyILAssembly (_,_,_,_,m)
        | SynExpr.LibraryOnlyStaticOptimization (_,_,_,m)
        | SynExpr.TypeTest (_,_,m)
        | SynExpr.Upcast (_,_,m)
        | SynExpr.AddressOf (_,_,_,m)
        | SynExpr.Downcast (_,_,m)
        | SynExpr.InferredUpcast (_,m)
        | SynExpr.InferredDowncast (_,m)
        | SynExpr.Null m
        | SynExpr.Lazy (_, m)
        | SynExpr.TraitCall(_,_,_,m)
        | SynExpr.DeprecatedTypeOf(_,m)
        | SynExpr.ImplicitZero (m)
        | SynExpr.YieldOrReturn (_,_,m)
        | SynExpr.YieldOrReturnFrom (_,_,m)
        | SynExpr.LetOrUseBang  (_,_,_,_,_,m)
        | SynExpr.DoBang  (_,m) -> m
        | SynExpr.Ident id -> id.idRange

type SynModuleDecl with 
    member d.Range = 
        match d with 
        | SynModuleDecl.ModuleAbbrev(_,_,m) 
        | SynModuleDecl.NestedModule(_,_,_,m)
        | SynModuleDecl.Let(_,_,m) 
        | SynModuleDecl.DoExpr(_,_,m) 
        | SynModuleDecl.Types(_,m)
        | SynModuleDecl.Exception(_,m)
        | SynModuleDecl.Open (_,m)
        | SynModuleDecl.HashDirective (_,m)
        | SynModuleDecl.NamespaceFragment(ModuleOrNamespace(_,_,_,_,_,_,m)) 
        | SynModuleDecl.Attributes(_,m) -> m

type SynModuleSigDecl with 
    member d.Range = 
        match d with 
        | SynModuleSigDecl.ModuleAbbrev (_,_,m)
        | SynModuleSigDecl.NestedModule   (_,_,m)
        | SynModuleSigDecl.Val      (_,m)
        | SynModuleSigDecl.Types    (_,m)
        | SynModuleSigDecl.Exception      (_,m)
        | SynModuleSigDecl.Open     (_,m)
        | SynModuleSigDecl.NamespaceFragment (ModuleOrNamespaceSig(_,_,_,_,_,_,m)) 
        | SynModuleSigDecl.HashDirective     (_,m) -> m

type SynMemberDefn with 
    member d.Range = 
        match d with 
        | SynMemberDefn.Member(_, m)
        | SynMemberDefn.Interface(_, _, m)
        | SynMemberDefn.Open(_, m)
        | SynMemberDefn.LetBindings(_,_,_,m) 
        | SynMemberDefn.ImplicitCtor(_,_,_,_,m)
        | SynMemberDefn.ImplicitInherit(_,_,_,m) 
        | SynMemberDefn.AbstractSlot(_,_,m)
        | SynMemberDefn.Inherit(_,_,m)
        | SynMemberDefn.ValField(_,m)
        | SynMemberDefn.NestedType(_,_,m) -> m


let (|LongOrSingleIdent|_|) inp = 
    match inp with
    | SynExpr.LongIdent(isOpt,lid, m) -> Some (isOpt,lid,m)
    | SynExpr.Ident(id) -> Some (false,[id], id.idRange)
    | _ -> None

let (|SingleIdent|_|) inp = 
    match inp with
    | SynExpr.LongIdent(false,[id], _) -> Some id.idText
    | SynExpr.Ident(id) -> Some id.idText
    | _ -> None
    
/// This affects placement of sequence points
let rec IsControlFlowExpression e = 
    match e with 
    | SynExpr.ObjExpr _ 
    | SynExpr.Lambda _ 
    | SynExpr.LetOrUse _ 
    | SynExpr.Seq _ 
    // Treat "ident { ... }" as a control flow expression
    | SynExpr.App (_, SynExpr.Ident _, SynExpr.CompExpr _,_) 
    | SynExpr.IfThenElse _ 
    | SynExpr.LetOrUseBang _
    | SynExpr.Match _  
    | SynExpr.TryWith _ 
    | SynExpr.TryFinally _ 
    | SynExpr.For _ 
    | SynExpr.ForEach _ 
    | SynExpr.While _ -> true
    | SynExpr.Typed(e,_,_) -> IsControlFlowExpression e
    | _ -> false

let mkAnonField (ty: SynType) = Field([],false,None,ty,false,PreXmlDoc.Empty,None,ty.Range)

let mkSynPatVar vis (id:Ident) = SynPat.Named (SynPat.Wild id.idRange,id,false,vis,id.idRange)
let mkSynThisPatVar (id:Ident) = SynPat.Named (SynPat.Wild id.idRange,id,true,None,id.idRange)
let mkSynPatMaybeVar lid vis m =  SynPat.LongIdent (lid,None,None,[],vis,m) 

/// Extract the argument for patterns corresponding to the declaration of 'new ... = ...'
let (|SynPatForConstructorDecl|_|) x = 
    match x with 
    | SynPat.LongIdent ([_],_,_,[arg],_,_) -> Some arg
    | _ -> None

/// Recognize the '()' in 'new()'
let (|SynPatForNullaryArgs|_|) x = 
    match x with 
    | SynPat.Paren(SynPat.Const(SynConst.Unit,_),_) -> Some()
    | _ -> None
    
let mkSynNewArgVar m  =
    let nm = GlobalSynArgNameGenerator.New()
    let id = mkSynId m nm
    mkSynPatVar None id, mkSynIdGet m nm

/// Push non-simple parts of a patten match over onto the r.h.s. of a lambda.
/// Return a simple pattern and a function to build a match on the r.h.s. if the pattern is complex
let rec SimplePatOfPat p =
    match p with 
    | SynPat.Typed(p',ty,m) -> 
        let p2,laterf = SimplePatOfPat p'
        SynSimplePat.Typed(p2,ty,m), 
        laterf
    | SynPat.Attrib(p',attribs,m) -> 
        let p2,laterf = SimplePatOfPat p'
        SynSimplePat.Attrib(p2,attribs,m), 
        laterf
    | SynPat.Named (SynPat.Wild _, v,thisv,_,m) -> 
        SynSimplePat.Id (v,false,thisv,false,m), 
        None
    | SynPat.OptionalVal (v,m) -> 
        SynSimplePat.Id (v,false,false,true,m), 
        None
    | SynPat.Paren (p,_) -> SimplePatOfPat p 
    | _ -> 
        let m = p.Range
        // 'nm' may be a real variable. Maintain its name. 
        let compgen,nm = (match p with SynPat.LongIdent([id],_,None,[],None,_) -> false,id.idText | _ -> true, GlobalSynArgNameGenerator.New())
        let id = mkSynId m nm
        let item = mkSynIdGet m nm
        SynSimplePat.Id (id,compgen,false,false,id.idRange),
        Some (fun e -> SynExpr.Match(NoSequencePointAtInvisibleBinding, item,[Clause(p,None,e,m,SuppressSequencePointAtTarget)],false,m)) 

let appFunOpt funOpt x = match funOpt with None -> x | Some f -> f x
let composeFunOpt funOpt1 funOpt2 = match funOpt2 with None -> funOpt1 | Some f -> Some (fun x -> appFunOpt funOpt1 (f x))
let rec SimplePatsOfPat p =
    match p with 
    | SynPat.Typed(p',ty,m) -> 
        let p2,laterf = SimplePatsOfPat p'
        SynSimplePats.Typed(p2,ty,m), 
        laterf
//    | SynPat.Paren (p,m) -> SimplePatsOfPat p 
    | SynPat.Tuple (ps,m) 
    | SynPat.Paren(SynPat.Tuple (ps,m),_) -> 
        let ps2,laterf = 
          List.foldBack 
            (fun (p',rhsf) (ps',rhsf') -> 
              p'::ps', 
              (composeFunOpt rhsf rhsf'))
            (List.map SimplePatOfPat ps) 
            ([], None)
        SynSimplePats.SimplePats (ps2,m),
        laterf
    | SynPat.Paren(SynPat.Const (SynConst.Unit,m),_) 
    | SynPat.Const (SynConst.Unit,m) -> 
        SynSimplePats.SimplePats ([],m),
        None
    | _ -> 
        let m = p.Range
        let sp,laterf = SimplePatOfPat p
        SynSimplePats.SimplePats ([sp],m),laterf

let PushPatternToExpr isMember pat (rhs: SynExpr) =
    let nowpats,laterf = SimplePatsOfPat pat
    nowpats, SynExpr.Lambda (isMember,false,nowpats, appFunOpt laterf rhs,rhs.Range)

let isSimplePattern pat =
    let _nowpats,laterf = SimplePatsOfPat pat
    isNone laterf
  
/// "fun (UnionCase x) (UnionCase y) -> body" 
///       ==> 
///   "fun tmp1 tmp2 -> 
///        let (UnionCase x) = tmp1 in 
///        let (UnionCase y) = tmp2 in 
///        body" 
let PushCurriedPatternsToExpr wholem isMember pats rhs =
    // Two phases
    // First phase: Fold back, from right to left, pushing patterns into r.h.s. expr
    let spatsl,rhs = 
        (pats, ([],rhs)) 
           ||> List.foldBack (fun arg (spatsl,body) -> 
              let spats,bodyf = SimplePatsOfPat arg
              // accumulate the body. This builds "let (UnionCase y) = tmp2 in body"
              let body = appFunOpt bodyf body
              // accumulate the patterns
              let spatsl = spats::spatsl
              (spatsl,body))
    // Second phase: build lambdas. Mark subsequent ones with "true" indicating they are part of an iterated sequence of lambdas
    let expr = 
        match spatsl with
        | [] -> rhs
        | h::t -> 
            let expr = List.foldBack (fun spats e -> SynExpr.Lambda (isMember,true,spats, e,wholem)) t rhs
            let expr = SynExpr.Lambda (isMember,false,h, expr,wholem)
            expr
    spatsl,expr

/// Helper for parsing the inline IL fragments. 
let ParseAssemblyCodeInstructions s m = 
    try Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiParser.ilInstrs 
           Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiLexer.token 
           (UnicodeLexing.StringAsLexbuf s)
    with RecoverableParseError -> 
      errorR(Error(FSComp.SR.astParseEmbeddedILError(), m)); [| |]

/// Helper for parsing the inline IL fragments. 
let ParseAssemblyCodeType s m = 
    try Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiParser.ilType 
           Microsoft.FSharp.Compiler.AbstractIL.Internal.AsciiLexer.token 
           (UnicodeLexing.StringAsLexbuf s)
    with RecoverableParseError -> 
      errorR(Error(FSComp.SR.astParseEmbeddedILTypeError(),m)); 
      IL.ecmaILGlobals.typ_Object

//------------------------------------------------------------------------
// AST constructors
//------------------------------------------------------------------------

let opNameParenGet  = CompileOpName parenGet 
let opNameQMark = CompileOpName qmark
let mkSynLidGet m path n = SynExpr.LongIdent(false,pathToSynLid m path @ [mkSynId m n],m)
let mkSynOperator opm oper = mkSynIdGet opm (CompileOpName oper)

// 'false' in SynExpr.App means that operators are never high-precedence applications
let mkSynInfix opm m l oper r = SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, mkSynOperator opm oper,l,m), r,m)
let mkSynBifix m oper l r = SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, mkSynOperator m oper,l,m), r,m)
let mkSynTrifix m oper  x1 x2 x3 = SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, mkSynOperator m oper,x1,m), x2,m), x3,m)
let mkSynQuadfix m oper  x1 x2 x3 x4 = SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, mkSynOperator m oper,x1,m), x2,m), x3,m),x4,m)
let mkSynQuinfix m oper  x1 x2 x3 x4 x5 = SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, SynExpr.App (ExprAtomicFlag.NonAtomic, mkSynOperator m oper,x1,m), x2,m), x3,m),x4,m),x5,m)
let mkSynPrefix opm m oper x = SynExpr.App (ExprAtomicFlag.NonAtomic, mkSynOperator opm oper, x,m)
let mkSynCaseName m n = [mkSynId m (CompileOpName n)]

let mkSynDotParenSet  m a b c = mkSynTrifix m parenSet a b c
let mkSynDotBrackGet  m mDot a b   = SynExpr.DotIndexedGet(a,[b],mDot,m)
let mkSynQMarkSet m a b c = mkSynTrifix m qmarkSet a b c

let mkSynDotBrackSliceGet  m mDot arr (x,y) = 
    SynExpr.DotIndexedGet(arr,[x;y],mDot,m)

let mkSynDotBrackSlice2Get  m mDot arr (x1,y1) (x2,y2) = 
    SynExpr.DotIndexedGet(arr,[x1;y1;x2;y2],mDot,m)

let mkSynDotBrackSlice3Get  m mDot arr (x1,y1) (x2,y2) (x3,y3) = 
    SynExpr.DotIndexedGet(arr,[x1;y1;x2;y2;x3;y3],mDot,m)

let mkSynDotBrackSlice4Get  m mDot arr (x1,y1) (x2,y2) (x3,y3) (x4,y4) = 
    SynExpr.DotIndexedGet(arr,[x1;y1;x2;y2;x3;y3;x4;y4],mDot,m)

let mkSynDotParenGet  m a b   = 
  match b with
  | SynExpr.Tuple ([_;_],_)   -> errorR(Deprecated(FSComp.SR.astDeprecatedIndexerNotation(),m)) ; SynExpr.Const(SynConst.Unit,m)
  | SynExpr.Tuple ([_;_;_],_) -> errorR(Deprecated(FSComp.SR.astDeprecatedIndexerNotation(),m)) ; SynExpr.Const(SynConst.Unit,m)
  | _ -> mkSynInfix m m a parenGet b
let mkSynUnit m = SynExpr.Const(SynConst.Unit,m)
let mkSynUnitPat m = SynPat.Const(SynConst.Unit,m)
let mkSynDelay m e = SynExpr.Lambda (false,false,SynSimplePats.SimplePats ([mkSynCompGenSimplePatVar (mkSynId m "unitVar")],m), e, m)

let mkSynAssign _m (l: SynExpr) (r: SynExpr) = 
    let m = unionRanges l.Range r.Range
    match l with 
    //| SynExpr.Paren(l2,m2)  -> mkSynAssign m l2 r
    | LongOrSingleIdent(false,v,_)  -> SynExpr.LongIdentSet (v,r,m)
    | SynExpr.DotGet(e,v,_)  -> SynExpr.DotSet (e,v,r,m)
    | SynExpr.DotIndexedGet(e1,e2,mDot,_)  -> SynExpr.DotIndexedSet (e1,e2,r,mDot,m)
    | SynExpr.LibraryOnlyUnionCaseFieldGet (x,y,z,_) -> SynExpr.LibraryOnlyUnionCaseFieldSet (x,y,z,r,m) 
    | SynExpr.App (_, SynExpr.App(_, SingleIdent(nm), a, _),b,_) when nm = opNameQMark -> 
        mkSynQMarkSet m a b r
    | SynExpr.App (_, SynExpr.App(_, SingleIdent(nm), a, _),b,_) when nm = opNameParenGet -> 
        mkSynDotParenSet m a b r
    | SynExpr.App (_, SynExpr.LongIdent(false,v,_),x,_)  -> SynExpr.NamedIndexedPropertySet (v,x,r,m)
    | SynExpr.App (_, SynExpr.DotGet(e,v,_),x,_)  -> SynExpr.DotNamedIndexedPropertySet (e,v,x,r,m)
    |   _ -> errorR(Error(FSComp.SR.astInvalidExprLeftHandOfAssignment(), m));  SynExpr.Const(SynConst.Unit,m)

let rec mkSynDot m l r = 
    match l with 
    //| SynExpr.Paren(l2,m2)  -> mkSynDot m l2 r
    | SynExpr.LongIdent(isOpt,lid,_) -> SynExpr.LongIdent(isOpt,lid@[r],m) // MEMORY PERFORMANCE: This is memory intensive (we create a lot of these list nodes) - an ImmutableArray would be better here
    | SynExpr.Ident(id) -> SynExpr.LongIdent(false,[id;r],m)
    | SynExpr.DotGet(e,lid,_) -> SynExpr.DotGet(e,lid@[r],m)// MEMORY PERFORMANCE: This is memory intensive (we create a lot of these list nodes) - an ImmutableArray would be better here
    | expr -> SynExpr.DotGet(expr,[r],m)

let mkSynMatchLambda (isMember,isExnMatch,wholem,mtch,spBind) =
    let p,pe = mkSynNewArgVar wholem
    let _,e = PushCurriedPatternsToExpr wholem isMember [p] (SynExpr.Match(spBind,pe,mtch,isExnMatch,wholem))
    e

let mkSynFunMatchLambdas isMember wholem ps e = 
    let _,e =  PushCurriedPatternsToExpr wholem isMember ps e 
    e

let mkSynCons (x: SynExpr) y =
    let xm = x.Range
    SynExpr.App(ExprAtomicFlag.NonAtomic, SynExpr.Ident(mkSynId xm opNameCons),SynExpr.Tuple([x;y],xm),xm) 

let mkSynList m l = 
    List.foldBack mkSynCons l (SynExpr.Ident(mkSynId m opNameNil))

//------------------------------------------------------------------------
// Arities of members
// Members have strongly syntactically constrained arities.  We must infer
// the arity from the syntax in order to have any chance of handling recursive 
// cross references during type inference.
//
// So we record the arity for: 
// StaticProperty --> [1]               -- for unit arg
// this.StaticProperty --> [1;1]        -- for unit arg
// StaticMethod(args) --> map InferArgSynInfoFromSimplePat args
// this.InstanceMethod() --> 1 :: map InferArgSynInfoFromSimplePat args
// this.InstanceProperty with get(argpat) --> 1 :: [InferArgSynInfoFromSimplePat argpat]
// StaticProperty with get(argpat) --> [InferArgSynInfoFromSimplePat argpat]
// this.InstanceProperty with get() --> 1 :: [InferArgSynInfoFromSimplePat argpat]
// StaticProperty with get() --> [InferArgSynInfoFromSimplePat argpat]
// 
// this.InstanceProperty with set(argpat)(v) --> 1 :: [InferArgSynInfoFromSimplePat argpat; 1]
// StaticProperty with set(argpat)(v) --> [InferArgSynInfoFromSimplePat argpat; 1]
// this.InstanceProperty with set(v) --> 1 :: [1]
// StaticProperty with set(v) --> [1] 
//-----------------------------------------------------------------------

module SynInfo = begin
    let unnamedTopArg1 = SynArgInfo([],false,None)
    let unnamedTopArg = [unnamedTopArg1]
    let unitArgData = unnamedTopArg
    let unnamedRetVal = SynArgInfo([],false,None)
    let selfMetadata = unnamedTopArg

    let HasNoArgs (SynValInfo(args,_)) = isNil args
    let HasOptionalArgs (SynValInfo(args,_)) = List.exists (List.exists (fun (SynArgInfo(_,isOptArg,_)) -> isOptArg)) args
    let IncorporateEmptyTupledArg (SynValInfo(args,retInfo)) = SynValInfo([]::args,retInfo)
    let IncorporateSelfArg (SynValInfo(args,retInfo)) = SynValInfo(selfMetadata::args,retInfo)

    let IncorporateSetterArg (SynValInfo(args,retInfo)) = 
         let args = 
             match args with 
             | [] -> [unnamedTopArg] 
             | [arg] -> [arg@[unnamedTopArg1]] 
             | _ -> failwith "invalid setter type" 
         SynValInfo(args,retInfo)

    let NumCurriedArgs(SynValInfo(args,_)) = args.Length
    let AritiesOfArgs (SynValInfo(args,_)) = List.map List.length args
    let AttribsOfArgData (SynArgInfo(attribs,_,_)) = attribs
    let IsOptionalArg (SynArgInfo(_,isOpt,_)) = isOpt

    let rec InferArgSynInfoFromSimplePat attribs p = 
        match p with 
        | SynSimplePat.Id(nm,compgen,_,isOpt,_) -> 
           SynArgInfo(attribs, isOpt, (if compgen then None else Some nm))
        | SynSimplePat.Typed(a,_,_) -> InferArgSynInfoFromSimplePat attribs a
        | SynSimplePat.Attrib(a,attribs2,_) -> InferArgSynInfoFromSimplePat (attribs @ attribs2) a
      
    let rec InferArgSynInfoFromSimplePats x = 
        match x with 
        | SynSimplePats.SimplePats(ps,_) -> List.map (InferArgSynInfoFromSimplePat []) ps
        | SynSimplePats.Typed(ps,_,_) -> InferArgSynInfoFromSimplePats ps

    let InferArgSynInfoFromPat p = 
        let sp,_ = SimplePatsOfPat p
        InferArgSynInfoFromSimplePats sp

    /// Make sure only a solitary unit argument has unit elimination
    let AdjustArgsForUnitElimination infosForArgs = 
        match infosForArgs with 
        | [[]] -> infosForArgs 
        | _ -> infosForArgs |> List.map (function [] -> unitArgData | x -> x)

    let AdjustMemberArgs memFlags infosForArgs = 
        match infosForArgs with 
        // Transform a property declared using '[static] member P = expr' to a method taking a "unit" argument 
        | [] when memFlags=MemberKind.Member -> [] :: infosForArgs
        | _ -> infosForArgs

    let InferLambdaArgs origRhsExpr = 
        let rec loop e = 
            match e with 
            | SynExpr.Lambda(false,_,spats,rest,_) -> 
                InferArgSynInfoFromSimplePats spats :: loop rest
            | _ -> []
        loop origRhsExpr

    let InferSynReturnData retInfo = 
        match retInfo with 
        | None -> unnamedRetVal 
        | Some((_,retInfo),_) -> retInfo

    let emptyValSynInfo = SynValInfo([],unnamedRetVal)
    let emptyValSynData = SynValData(None,emptyValSynInfo,None)

    let InferValSynData memberFlagsOpt pat retInfo origRhsExpr = 

        let infosForExplicitArgs = 
            match pat with 
            | Some(SynPat.LongIdent(_,_,_,curriedArgs,_,_)) -> List.map InferArgSynInfoFromPat curriedArgs
            | _ -> []

        let explicitArgsAreSimple = 
            match pat with 
            | Some(SynPat.LongIdent(_,_,_,curriedArgs,_,_)) -> List.forall isSimplePattern curriedArgs
            | _ -> true

        let retInfo = InferSynReturnData retInfo

        match memberFlagsOpt with
        | None -> 
            let infosForLambdaArgs = InferLambdaArgs origRhsExpr
            let infosForArgs = infosForExplicitArgs @ (if explicitArgsAreSimple then infosForLambdaArgs else [])
            let infosForArgs = AdjustArgsForUnitElimination infosForArgs 
            SynValData(None,SynValInfo(infosForArgs,retInfo),None)

        | Some memFlags  -> 
            let infosForObjArgs = 
                if memFlags.IsInstance then [ selfMetadata ] else []

            let infosForArgs = AdjustMemberArgs memFlags.MemberKind infosForExplicitArgs
            let infosForArgs = AdjustArgsForUnitElimination infosForArgs 
            
            let argInfos = infosForObjArgs @ infosForArgs
            SynValData(Some(memFlags),SynValInfo(argInfos,retInfo),None)
end


let mkSynBindingRhs staticOptimizations rhsExpr rhsRange retInfo =
    let rhsExpr = List.foldBack (fun (c,e1) e2 -> SynExpr.LibraryOnlyStaticOptimization (c,e1,e2,rhsRange)) staticOptimizations rhsExpr
    let rhsExpr,retTyOpt = 
        match retInfo with 
        | Some ((ty,SynArgInfo(rattribs,_,_)),tym) -> SynExpr.Typed(rhsExpr,ty,rhsExpr.Range), Some(SynBindingReturnInfo(ty,tym,rattribs) )
        | None -> rhsExpr,None 
    rhsExpr,retTyOpt

let mkSynBinding (xmlDoc,headPat) (vis,pseudo,mut,bindm,spBind,_wholem,retInfo,origRhsExpr,rhsRange,staticOptimizations,attrs,memberFlagsOpt) =
    let info = SynInfo.InferValSynData memberFlagsOpt (Some headPat) retInfo origRhsExpr
    let rhsExpr,retTyOpt = mkSynBindingRhs staticOptimizations origRhsExpr rhsRange retInfo
    // dprintfn "headPat = %A, info = %A" headPat info
    // PERFORMANCE: There are quite a lot of these nodes allocated. Perhaps not much we can do about that.
    Binding (vis,NormalBinding,pseudo,mut,attrs,xmlDoc,info,headPat,retTyOpt,rhsExpr,bindm,spBind) 

let NonVirtualMemberFlags k = { MemberKind=k;                           IsInstance=true;  IsDispatchSlot=false; IsOverrideOrExplicitImpl=false; IsFinal=false }
let CtorMemberFlags =         { MemberKind=MemberKind.Constructor;       IsInstance=false; IsDispatchSlot=false; IsOverrideOrExplicitImpl=false; IsFinal=false }
let ClassCtorMemberFlags =    { MemberKind=MemberKind.ClassConstructor;  IsInstance=false; IsDispatchSlot=false; IsOverrideOrExplicitImpl=false; IsFinal=false }
let OverrideMemberFlags k =   { MemberKind=k;                           IsInstance=true;  IsDispatchSlot=false; IsOverrideOrExplicitImpl=true;  IsFinal=false }
let AbstractMemberFlags k =   { MemberKind=k;                           IsInstance=true;  IsDispatchSlot=true;  IsOverrideOrExplicitImpl=false; IsFinal=false }
let StaticMemberFlags k =     { MemberKind=k;                           IsInstance=false; IsDispatchSlot=false; IsOverrideOrExplicitImpl=false; IsFinal=false }

let inferredTyparDecls = SynValTyparDecls([],true,[])
let noInferredTypars = SynValTyparDecls([],false,[])

//------------------------------------------------------------------------
// Lexer args: status of #if/#endif processing.  
//------------------------------------------------------------------------

type LexerIfdefStackEntry = IfDefIf | IfDefElse 
type LexerIfdefStackEntries = (LexerIfdefStackEntry * range) list
type LexerIfdefStack = LexerIfdefStackEntries ref

/// Specifies how the 'endline' function in the lexer should continue after
/// it reaches end of line or eof. The options are to continue with 'token' function
/// or to continue with 'skip' function.
type LexerEndlineContinuation = 
    | Token of LexerIfdefStackEntries
    | Skip of LexerIfdefStackEntries * int * range
    member x.LexerIfdefStack = 
      match x with 
      | LexerEndlineContinuation.Token(ifd) 
      | LexerEndlineContinuation.Skip(ifd, _, _) -> ifd
          
/// The parser defines a number of tokens for whitespace and
/// comments eliminated by the lexer.  These carry a specification of
/// a continuation for the lexer for continued processing after we've dealt with
/// the whitespace.
[<RequireQualifiedAccess>]
[<NoComparison; NoEquality>]
type LexerWhitespaceContinuation = 
    | Token            of LexerIfdefStackEntries
    | IfDefSkip        of LexerIfdefStackEntries * int * range
    | String           of LexerIfdefStackEntries * range
    | VerbatimString   of LexerIfdefStackEntries * range
    | Comment          of LexerIfdefStackEntries * int * range
    | TokenizedComment of LexerIfdefStackEntries * int * range
    | CommentString    of LexerIfdefStackEntries * int * range
    | CommentVerbatimString   of LexerIfdefStackEntries * int * range
    | MLOnly            of LexerIfdefStackEntries * range
    | EndLine           of LexerEndlineContinuation
    
    member x.LexerIfdefStack =
        match x with 
        | LexCont.Token ifd
        | LexCont.IfDefSkip (ifd,_,_)
        | LexCont.String (ifd,_)
        | LexCont.VerbatimString (ifd,_)
        | LexCont.Comment (ifd,_,_)
        | LexCont.TokenizedComment (ifd,_,_)
        | LexCont.CommentString (ifd,_,_)
        | LexCont.CommentVerbatimString (ifd,_,_)
        | LexCont.MLOnly (ifd,_) -> ifd
        | LexCont.EndLine endl -> endl.LexerIfdefStack

and LexCont = LexerWhitespaceContinuation

//------------------------------------------------------------------------
// Parser/Lexer state
//------------------------------------------------------------------------

[<NoEquality; NoComparison>]
exception SyntaxError of obj (* ParseErrorContext<_> *) * range

let posOfLexPosition (p:Position) = 
    mkPos p.Line p.Column

let mkSynRange (p1:Position) p2 = 
    mkFileIndexRange p1.posFileIndex (posOfLexPosition p1) (posOfLexPosition p2)

let getLexerRange (lexbuf:UnicodeLexing.Lexbuf) = 
    mkSynRange lexbuf.StartPos lexbuf.EndPos

/// Get the range corresponding to the result of a grammar rule while it is being reduced
let lhs (parseState: IParseState) = 
    let p1,p2 = parseState.ResultRange
    mkSynRange p1 p2

/// Get the range covering two of the r.h.s. symbols of a grammar rule while it is being reduced
let rhs2 (parseState: IParseState) i j = 
    let p1 = parseState.InputStartPosition i
    let p2 = parseState.InputEndPosition j
    mkSynRange p1 p2

/// Get the range corresponding to one of the r.h.s. symbols of a grammar rule while it is being reduced
let rhs parseState i = rhs2 parseState i i 

//------------------------------------------------------------------------
// XmlDoc F# lexer/parser state (thread local)
//------------------------------------------------------------------------

// The key into the BufferLocalStore used to hold the current accumulated XmlDoc lines 
module LexbufLocalXmlDocStore = 
    let private xmlDocKey = "XmlDoc"

    let ClearXmlDoc (lexbuf:Lexbuf) = 
        lexbuf.BufferLocalStore.[xmlDocKey] <- box (XmlDocCollector())

    let SaveXmlDoc (lexbuf:Lexbuf) (line,pos) = 
        if not (lexbuf.BufferLocalStore.ContainsKey(xmlDocKey)) then 
            lexbuf.BufferLocalStore.[xmlDocKey] <- box (XmlDocCollector())
        let collector = unbox<XmlDocCollector>(lexbuf.BufferLocalStore.[xmlDocKey])
        collector.AddXmlDocLine(line,pos)

    let GrabXML (lexbuf:Lexbuf, markerRange:range)  = 
        if lexbuf.BufferLocalStore.ContainsKey(xmlDocKey) then 
            PreXmlDoc.CreateFromGrabPoint(unbox<XmlDocCollector>(lexbuf.BufferLocalStore.[xmlDocKey]),markerRange.End)
        else
            PreXmlDoc.Empty
#if DEBUG
    let DumpXmlDoc note (XmlDoc lines) = 
        printf "\nXmlDoc: %s\n" note; 
        Array.iter (printf "  %s\n") lines; 
        XmlDoc lines
#endif


   
/// Generates compiler-generated names marked up with a source code location
type NiceNameGenerator() = 

    let basicNameCounts = new System.Collections.Generic.Dictionary<string,_>(100)

    member x.FreshCompilerGeneratedName (name,m:range) =
        let basicName = GetBasicNameOfPossibleCompilerGeneratedName name
        let n = (if basicNameCounts.ContainsKey basicName then basicNameCounts.[basicName] else 0) 
        let nm = CompilerGeneratedNameSuffix basicName (string m.StartLine ^ (match n with 0 -> "" | n -> "-" ^ string n))
        basicNameCounts.[basicName] <- n+1
        nm

    member x.Reset () = basicNameCounts.Clear()

   

/// Generates compiler-generated names marked up with a source code location, but if given the same unique value then
/// return precisely the same name
type StableNiceNameGenerator() = 

    let names = new System.Collections.Generic.Dictionary<(string * int64),_>(100)
    let basicNameCounts = new System.Collections.Generic.Dictionary<string,_>(100)

    member x.GetUniqueCompilerGeneratedName (name,m:range,uniq) =
        let basicName = GetBasicNameOfPossibleCompilerGeneratedName name
        if names.ContainsKey (basicName,uniq) then
            names.[(basicName,uniq)]
        else 
            let n = (if basicNameCounts.ContainsKey basicName then basicNameCounts.[basicName] else 0) 
            let nm = CompilerGeneratedNameSuffix basicName (string m.StartLine ^ (match n with 0 -> "" | n -> "-" ^ string n))
            names.[(basicName,uniq)] <- nm
            basicNameCounts.[basicName] <- n+1
            nm

    member x.Reset () = 
        basicNameCounts.Clear()
        names.Clear()

/// A global generator of compiler generated names
// ++GLOBAL MUTABLE STATE
let globalNng = NiceNameGenerator()

/// A global generator of stable compiler generated names
// ++GLOBAL MUTABLE STATE
let globalStableNameGenerator = StableNiceNameGenerator ()
