//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Open up the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.
//--------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open System
open System.IO
open System.Collections.Generic
 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library  
open Microsoft.FSharp.Compiler 
open Internal.Utilities.Debug
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Build
open Microsoft.FSharp.Compiler.Lib

/// Methods for dealing with F# sources files.
module internal SourceFile =
    /// Source file extensions
    let private compilableExtensions = Build.sigSuffixes @ Build.implSuffixes @ Build.scriptSuffixes
    /// Single file projects extensions
    let private singleFileProjectExtensions = Build.scriptSuffixes
    /// Whether or not this file is compilable
    let IsCompilable file =
        let ext = Path.GetExtension(file)
        compilableExtensions |> List.exists(fun e->0 = String.Compare(e,ext,StringComparison.OrdinalIgnoreCase))
    /// Whether or not this file should be a single-file project
    let MustBeSingleFileProject file =
        let ext = Path.GetExtension(file)
        singleFileProjectExtensions |> List.exists(fun e-> 0 = String.Compare(e,ext,StringComparison.OrdinalIgnoreCase))

module internal SourceFileImpl =
    let IsInterfaceFile file =
        let ext = Path.GetExtension(file)
        0 = String.Compare(".fsi",ext,StringComparison.OrdinalIgnoreCase)

    /// Additonal #defines that should be in place when editing a file in a file editor such as VS.
    let AdditionalDefinesForUseInEditor(filename) =
        if Build.IsScript(filename) then ["INTERACTIVE";"EDITING"] // This is still used by the foreground parse
        else ["COMPILED";"EDITING"]
           
type CompletionPath = string list * string option // plid * residue

type InheritanceOrigin = 
    | Class
    | Interface
    | Unknown

type internal InheritanceContext = 
    | Class
    | Interface
    | Unknown

type internal RecordContext =
    | CopyOnUpdate of range * CompletionPath // range of copy-expr + current field
    | Constructor of string // typename
    | New of CompletionPath

type internal CompletionContext = 
    // completion context cannot be determined due to errors
    | Invalid
    // completing something after the inherit keyword
    | Inherit of InheritanceContext * CompletionPath
    // completing records field
    | RecordField of RecordContext
    | RangeOperator

//----------------------------------------------------------------------------
// Untyped scope
//----------------------------------------------------------------------------

[<NoEquality; NoComparison>]
type internal UntypedParseResults = 
  { // Error infos
    Errors : ErrorInfo[]
    // Untyped AST
    Input : ParsedInput option
    // Do not report errors from the type checker
    ParseHadErrors : bool
    /// When these files change then the build is invalid
    DependencyFiles : string list
    }

[<Sealed>]
type (* internal *) UntypedParseInfo internal (parsed:UntypedParseResults) = 

    member internal scope.ParseTree =
        match parsed with
        | { Input=x } -> x

    member internal scope.Results = parsed

    member internal scope.FindNoteworthyParamInfoLocations(line,col) = 
        match parsed with
        | { Input=Some(input) } -> 
            // Why don't we traverse the AST under a syncop?  We don't need to, because the AST is an _immutable_ DU of DUs of ints and strings and whatnot.  And a SyncOp really does slow it down in practice.
            //let result = ref None
            //syncop (fun () -> result := Some(AstHelpers.FindNoteworthyParamInfoLocations(line,col,input)))
            //Option.get !result
            NoteworthyParamInfoLocationsImpl.FindNoteworthyParamInfoLocations(line,col,input)
        | _ -> None
    
    /// Get declared items and the selected item at the specified location
    member private scope.GetNavigationItemsImpl() =
       ErrorScope.Protect 
            Range.range0 
            (fun () -> 
                use t = Trace.Call("CompilerServices", "GetNavigationItems", fun _ -> "")
                match parsed.Input with
                | Some(ParsedInput.ImplFile(ParsedImplFileInput(_modname,_isScript,_qualName,_pragmas,_hashDirectives,modules,_isLastCompiland))) ->
                    NavigationImpl.getNavigationFromImplFile modules 
                | Some(ParsedInput.SigFile(ParsedSigFileInput(_modname,_qualName,_pragmas,_hashDirectives,_modules))) ->
                    NavigationImpl.empty
                | _ -> 
                    NavigationImpl.empty )
            (fun _ -> NavigationImpl.empty)   
            
    member private scope.ValidateBreakpointLocationImpl((line,col)) =

        
        let pos = Pos.fromVS line col
        
        // Process let-binding
        let findBreakPoints allowSameLine = 
            let checkRange m = [ if rangeContainsPos m pos || (allowSameLine && m.StartLine = pos.Line) then 
                                     yield Range.toVS m ]
            let walkBindSeqPt sp = [ match sp with SequencePointAtBinding m -> yield! checkRange m | _ -> () ]
            let walkForSeqPt sp = [ match sp with SequencePointAtForLoop m -> yield! checkRange m | _ -> () ]
            let walkWhileSeqPt sp = [ match sp with SequencePointAtWhileLoop m -> yield! checkRange m | _ -> () ]
            let walkTrySeqPt sp = [ match sp with SequencePointAtTry m -> yield! checkRange m | _ -> () ]
            let walkWithSeqPt sp = [ match sp with SequencePointAtWith m -> yield! checkRange m | _ -> () ]
            let walkFinallySeqPt sp = [ match sp with SequencePointAtFinally m -> yield! checkRange m | _ -> () ]

            let rec walkBind (Binding(_, _, _, _, _, _, SynValData(memFlagsOpt,_,_), synPat, _, synExpr, _, spInfo)) =
                [ // Don't yield the binding sequence point if there are any arguments, i.e. we're defining a function or a method
                  let isFunction = 
                      isSome memFlagsOpt ||
                      match synPat with 
                      | SynPat.LongIdent (_,_,_,args,_,_) when nonNil args -> true
                      | _ -> false
                  if not isFunction then 
                      yield! walkBindSeqPt spInfo

                  yield! walkExpr (isFunction || (match spInfo with SequencePointAtBinding _ -> false | _-> true)) synExpr ]

            and walkExprs es = [ for e in es do yield! walkExpr false e ]
            and walkBinds es = [ for e in es do yield! walkBind e ]
            and walkMatchClauses cl = 
                [ for (Clause(_,whenExpr,e,_,_)) in cl do 
                    match whenExpr with Some e -> yield! walkExpr false e | _ -> ()
                    yield! walkExpr true e; ]

            and walkExprOpt (spAlways:bool) eOpt = [ match eOpt with Some e -> yield! walkExpr spAlways e | _ -> () ]
            
            // Determine the breakpoint locations for an expression. spAlways indicates we always
            // emit a breakpoint location for the expression unless it is a syntactic control flow construct
            and walkExpr (spAlways:bool)  e = 
                [ if spAlways && not (IsControlFlowExpression e) then 
                      yield! checkRange e.Range
                  match e with 

                  | SynExpr.ArbitraryAfterError _ 
                  | SynExpr.LongIdent _
                  | SynExpr.Quote _
                  | SynExpr.LibraryOnlyILAssembly _
                  | SynExpr.LibraryOnlyStaticOptimization _
                  | SynExpr.Null _
                  | SynExpr.Ident _
                  | SynExpr.ImplicitZero _
                  | SynExpr.Const _ -> 
                     ()

                  | SynExpr.TypeTest (e,_,_)
                  | SynExpr.Upcast (e,_,_)
                  | SynExpr.AddressOf (_,e,_,_)
                  | SynExpr.CompExpr (_,_,e,_) 
                  | SynExpr.ArrayOrListOfSeqExpr (_,e,_)
                  | SynExpr.Typed (e,_,_)
                  | SynExpr.FromParseError (e,_) 
                  | SynExpr.DiscardAfterMissingQualificationAfterDot (e,_) 
                  | SynExpr.Do (e,_)
                  | SynExpr.Assert (e,_)
                  | SynExpr.DotGet (e,_,_,_) 
                  | SynExpr.LongIdentSet (_,e,_)
                  | SynExpr.New (_,_,e,_) 
                  | SynExpr.TypeApp (e,_,_,_,_,_,_) 
                  | SynExpr.LibraryOnlyUnionCaseFieldGet (e,_,_,_) 
                  | SynExpr.Downcast (e,_,_)
                  | SynExpr.InferredUpcast (e,_)
                  | SynExpr.InferredDowncast (e,_)
                  | SynExpr.Lazy (e, _)
                  | SynExpr.TraitCall(_,_,e,_)
                  | SynExpr.Paren(e,_,_,_) -> 
                      yield! walkExpr false e

                  | SynExpr.YieldOrReturn (_,e,_)
                  | SynExpr.YieldOrReturnFrom (_,e,_)
                  | SynExpr.DoBang  (e,_) ->
                      yield! checkRange e.Range
                      yield! walkExpr false e

                  | SynExpr.NamedIndexedPropertySet (_,e1,e2,_)
                  | SynExpr.DotSet (e1,_,e2,_)
                  | SynExpr.LibraryOnlyUnionCaseFieldSet (e1,_,_,e2,_)
                  | SynExpr.App (_,_,e1,e2,_) -> 
                      yield! walkExpr false e1; 
                      yield! walkExpr false e2;

                  | SynExpr.ArrayOrList (_,es,_)
                  | SynExpr.Tuple (es,_,_) -> 
                      yield! walkExprs es

                  | SynExpr.Record (_,_,fs,_) -> 
                    
                      yield! walkExprs (List.map (fun (_, v, _) -> v) fs |> List.choose id)

                  | SynExpr.ObjExpr (_,_,bs,is,_,_) -> 
                      yield! walkBinds bs ; 
                      for (InterfaceImpl(_,bs,_)) in is do yield! walkBinds bs
                  | SynExpr.While (spWhile,e1,e2,_) -> 
                      yield! walkWhileSeqPt spWhile
                      yield! walkExpr false e1; 
                      yield! walkExpr true e2;
                  | SynExpr.JoinIn(e1, _range, e2, _range2) -> 
                      yield! walkExpr false e1; 
                      yield! walkExpr false e2;
                  | SynExpr.For (spFor,_,e1,_,e2,e3,_) -> 
                      yield! walkForSeqPt spFor
                      yield! walkExpr false e1; 
                      yield! walkExpr true e2; 
                      yield! walkExpr true e3;
                  | SynExpr.ForEach (spFor,_,_,_,e1,e2,_) ->
                      yield! walkForSeqPt spFor
                      yield! walkExpr false e1; 
                      yield! walkExpr true e2; 
                  | SynExpr.MatchLambda(_isExnMatch,_argm,cl,spBind,_wholem) -> 
                      yield! walkBindSeqPt spBind
                      for (Clause(_,whenExpr,e,_,_)) in cl do 
                          yield! walkExprOpt false whenExpr
                          yield! walkExpr true e; 
                  | SynExpr.Lambda (_,_,_,e,_) -> 
                      yield! walkExpr true e; 
                  | SynExpr.Match (spBind,e,cl,_,_) ->
                      yield! walkBindSeqPt spBind
                      yield! walkExpr false e; 
                      for (Clause(_,whenExpr,e,_,_)) in cl do 
                          yield! walkExprOpt false whenExpr
                          yield! walkExpr true e; 
                  | SynExpr.LetOrUse (_,_,bs,e,_) -> 
                      yield! walkBinds bs ; 
                      yield! walkExpr true e;

                  | SynExpr.TryWith (e,_,cl,_,_,spTry,spWith) -> 
                      yield! walkTrySeqPt spTry
                      yield! walkWithSeqPt spWith
                      yield! walkExpr true e 
                      yield! walkMatchClauses cl
                  
                  | SynExpr.TryFinally (e1,e2,_,spTry,spFinally) ->
                      yield! walkExpr true e1
                      yield! walkExpr true e2
                      yield! walkTrySeqPt spTry
                      yield! walkFinallySeqPt spFinally
                  | SynExpr.Sequential (spSeq,_,e1,e2,_) -> 
                      yield! walkExpr (match spSeq with SuppressSequencePointOnStmtOfSequential -> false | _ -> true) e1
                      yield! walkExpr (match spSeq with SuppressSequencePointOnExprOfSequential -> false | _ -> true) e2
                  | SynExpr.IfThenElse (e1,e2,e3opt,spBind,_,_,_) ->
                      yield! walkBindSeqPt spBind
                      yield! walkExpr false e1
                      yield! walkExpr true e2
                      yield! walkExprOpt true e3opt
                  | SynExpr.DotIndexedGet (e1,es,_,_) -> 
                      yield! walkExpr false e1; 
                      yield! walkExprs es;

                  | SynExpr.DotIndexedSet (e1,es,e2,_,_,_) ->
                      yield! walkExpr false e1; 
                      yield! walkExprs es;
                      yield! walkExpr false e2; 
                  | SynExpr.DotNamedIndexedPropertySet (e1,_,e2,e3,_) ->
                      yield! walkExpr false e1; 
                      yield! walkExpr false e2; 
                      yield! walkExpr false e3; 

                  | SynExpr.LetOrUseBang  (spBind,_,_,_,e1,e2,_) -> 
                      yield! walkBindSeqPt spBind
                      yield! walkExpr true e1
                      yield! walkExpr true e2 ]
            
            // Process a class declaration or F# type declaration
            let rec walkTycon (TypeDefn(ComponentInfo(_, _, _, _, _, _, _, _), repr, membDefns, _)) =
                [ for m in membDefns do yield! walkMember m 
                  match repr with
                  | SynTypeDefnRepr.ObjectModel(_, membDefns, _) -> 
                      for m in membDefns do yield! walkMember m 
                  | _ -> () ]
                      
            // Returns class-members for the right dropdown                  
            and walkMember memb  = 
                [ match memb with
                  | SynMemberDefn.LetBindings(binds, _, _, _) -> yield! walkBinds binds
                  | SynMemberDefn.AutoProperty(_attribs, _isStatic, _id, _tyOpt, _propKind, _, _xmlDoc, _access, synExpr, _, _) -> yield! walkExpr true synExpr
                  | SynMemberDefn.ImplicitCtor(_,_,_,_,m) -> yield! checkRange m
                  | SynMemberDefn.Member(bind, _) -> yield! walkBind bind
                  | SynMemberDefn.Interface(_synty, Some(membs), _) -> for m in membs do yield! walkMember m
                  | SynMemberDefn.Inherit(_, _, m) -> 
                      // can break on the "inherit" clause
                      yield! checkRange m
                  | _ -> ()  ]

            // Process declarations nested in a module that should be displayed in the left dropdown
            // (such as type declarations, nested modules etc.)                            
            let rec walkDecl decl = 
                [ match decl with 
                  | SynModuleDecl.Let(_, binds, m) -> 
                      if rangeContainsPos m pos then 
                          yield! walkBinds binds
                  | SynModuleDecl.DoExpr(spExpr,expr, _) ->  
                      yield! walkBindSeqPt spExpr
                      yield! walkExpr false expr
                  | SynModuleDecl.ModuleAbbrev _ -> 
                      ()
                  | SynModuleDecl.NestedModule(_, decls, _, m) ->                
                      if rangeContainsPos m pos then 
                          for d in decls do yield! walkDecl d
                  | SynModuleDecl.Types(tydefs, m) -> 
                      if rangeContainsPos m pos then 
                          for d in tydefs do yield! walkTycon d
                  | SynModuleDecl.Exception(ExceptionDefn(ExceptionDefnRepr(_, _, _, _, _, _), membDefns, _), m) ->
                      if rangeContainsPos m pos then 
                          for m in membDefns do yield! walkMember m
                  | _ ->
                      () ] 
                      
            // Collect all the items  
            let walkModule (SynModuleOrNamespace(_,_,decls,_,_,_,m)) =
                if rangeContainsPos m pos then 
                    [ for d in decls do yield! walkDecl d ]
                else
                    []
                      
           /// Get information for implementation file        
            let walkImplFile (modules:SynModuleOrNamespace list) =
                [ for x in modules do yield! walkModule x ]
                     
            match parsed.Input with
            | Some(ParsedInput.ImplFile(ParsedImplFileInput(_,_,_,_,_,modules,_))) -> walkImplFile modules 
            | _ -> []
 
        ErrorScope.Protect 
            Range.range0 
            (fun () -> 
                // Find the last breakpoint reported where the position is inside the region
                match findBreakPoints false |> List.rev with
                | h::_ -> Some(h)
                | _ -> 
                    // If there is no such breakpoint, look for any breakpoint beginning on this line
                    match findBreakPoints true with
                    | h::_ -> Some(h)
                    | _ -> None)
            (fun _msg -> None)   
            
    /// When these files appear or disappear the configuration for the current project is invalidated.
    member scope.DependencyFiles() : string list =
        parsed.DependencyFiles
                    
    member scope.FileName =
      match parsed.Input with
      | Some(ParsedInput.ImplFile(ParsedImplFileInput(modname, _, _, _, _, _, _))) 
      | Some(ParsedInput.SigFile(ParsedSigFileInput(modname, _, _, _, _))) -> modname
      | _ -> ""
    
    // Get items for the navigation drop down bar       
    member scope.GetNavigationItems() =
        use t = Trace.Call("SyncOp","GetNavigationItems", fun _->"")
        // This does not need to be run on the background thread
        scope.GetNavigationItemsImpl()

    member scope.ValidateBreakpointLocation(pos:Position) =
        use t = Trace.Call("SyncOp","ValidateBreakpointLocation", fun _->"")
        // This does not need to be run on the background thread
        scope.ValidateBreakpointLocationImpl(pos)

module internal UntypedParseInfoImpl =
    let GetUntypedParseResults (upi : UntypedParseInfo) = upi.Results

    let GetRangeOfExprLeftOfDot(line,col,parseTreeOpt) =
        match parseTreeOpt with 
        | None -> None 
        | Some(parseTree) ->
        let pos = Pos.fromVS line col  // line was 0-based, need 1-based
        let ResultOfRange range = Range.toVS range
        let CheckLongIdent(longIdent:LongIdent) =
            // find the longest prefix before the "pos" dot
            let mutable r = (List.head longIdent).idRange 
            let mutable couldBeBeforeFront = true
            for i in longIdent do
                if posGeq pos i.idRange.End then
                    r <- unionRanges r i.idRange
                    couldBeBeforeFront <- false
            couldBeBeforeFront, r

        AstTraversal.Traverse(line,col,parseTree, { new AstTraversal.AstVisitorBase<_>() with
        member this.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
            let expr = expr // fix debugger locals
            match expr with
            | SynExpr.LongIdent(_, LongIdentWithDots(longIdent,_), _altNameRefCell, _range) -> 
                let _,r = CheckLongIdent(longIdent)
                Some(ResultOfRange r)
            | SynExpr.LongIdentSet(LongIdentWithDots(longIdent,_), synExpr, _range) -> 
                if AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr.Range pos then
                    traverseSynExpr synExpr
                else
                    let _,r = CheckLongIdent(longIdent)
                    Some(ResultOfRange r)
            | SynExpr.DotGet(synExpr, _dotm, LongIdentWithDots(longIdent,_), _range) -> 
                if AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr.Range pos then
                    traverseSynExpr synExpr
                else
                    let inFront,r = CheckLongIdent(longIdent)
                    if inFront then
                        Some(ResultOfRange synExpr.Range)
                    else
                        // see comment below for SynExpr.DotSet
                        Some(ResultOfRange (unionRanges synExpr.Range r))
            | SynExpr.DotSet(synExpr, LongIdentWithDots(longIdent,_), synExpr2, _range) ->
                if AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr.Range pos then
                    traverseSynExpr synExpr
                elif AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr2.Range pos then
                    traverseSynExpr synExpr2
                else
                    let inFront,r = CheckLongIdent(longIdent)
                    if inFront then
                        Some(ResultOfRange synExpr.Range)
                    else
                        // f(0).X.Y.Z
                        //       ^
                        //      -   r has this value
                        // ----     synExpr.Range has this value
                        // ------   we want this value
                        Some(ResultOfRange (unionRanges synExpr.Range r))
            | SynExpr.DotNamedIndexedPropertySet(synExpr, LongIdentWithDots(longIdent,_), synExpr2, synExpr3, _range) ->  
                if AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr.Range pos then
                    traverseSynExpr synExpr
                elif AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr2.Range pos then
                    traverseSynExpr synExpr2
                elif AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr3.Range pos then
                    traverseSynExpr synExpr3
                else
                    let inFront,r = CheckLongIdent(longIdent)
                    if inFront then
                        Some(ResultOfRange synExpr.Range)
                    else
                        Some(ResultOfRange (unionRanges synExpr.Range r))
            | SynExpr.DiscardAfterMissingQualificationAfterDot(synExpr, _range) ->  // get this for e.g. "bar()."
                if AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr.Range pos then
                    traverseSynExpr synExpr
                else
                    Some(ResultOfRange synExpr.Range) 
            | SynExpr.FromParseError(synExpr, range) -> 
                if AstTraversal.rangeContainsPosLeftEdgeInclusive synExpr.Range pos then
                    traverseSynExpr synExpr
                else
                    Some(ResultOfRange range) 
            | SynExpr.App(ExprAtomicFlag.NonAtomic, true, (SynExpr.Ident(ident)), rhs, _) 
                when ident.idText = "op_ArrayLookup" 
                     && not(AstTraversal.rangeContainsPosLeftEdgeInclusive rhs.Range pos) ->
                match defaultTraverse expr with
                | None ->
                    // (expr).(expr) is an ML-deprecated array lookup, but we want intellisense on the dot
                    // also want it for e.g. [|arr|].(0)
                    Some(ResultOfRange expr.Range) 
                | x -> x  // we found the answer deeper somewhere in the lhs
            | _ -> defaultTraverse expr
        })
    
    /// searches for the expression island suitable for the evaluation by the debugger
    let TryFindExpressionIslandInPosition(line,col,parseTreeOpt) = 
        match parseTreeOpt with 
        | None -> None 
        | Some(parseTree) ->
            let pos = Pos.fromVS line col  // line was 0-based, need 1-based
            let getLidParts (lid : LongIdent) = 
                lid 
                |> Seq.takeWhile (fun i -> posGeq pos i.idRange.Start)
                |> Seq.map (fun i -> i.idText)
                |> Seq.toList

            // tries to locate simple expression island
            // foundCandidate = false  means that we are looking for the candidate expression
            // foundCandidate = true - we found candidate (DotGet) and now drill down to the left part
            let rec TryGetExpression foundCandidate expr = 
                match expr with
                | SynExpr.Paren(e, _, _, _) when foundCandidate -> 
                    TryGetExpression foundCandidate e
                | SynExpr.LongIdent(_isOptional, LongIdentWithDots(lid,_), _altNameRefCell, _m) -> 
                    getLidParts lid |> Some
                | SynExpr.DotGet(leftPart, _, LongIdentWithDots(lid,_), _) when (rangeContainsPos (rangeOfLid lid) pos) || foundCandidate -> 
                    // requested position is at the lid part of the DotGet
                    // process left part and append result to the result of processing lid
                    let leftPartResult = TryGetExpression true leftPart
                    match leftPartResult with 
                    | Some leftPartResult ->
                        [
                            yield! leftPartResult
                            yield! getLidParts lid 
                        ] |> Some
                    | None -> None
                | SynExpr.FromParseError(synExpr, _range) -> TryGetExpression foundCandidate synExpr
                | _ -> None

            let rec walker = 
                { new AstTraversal.AstVisitorBase<_>() with
                    member this.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
                        if rangeContainsPos expr.Range pos then
                            match TryGetExpression false expr with
                            | (Some parts) -> parts |> String.concat "." |> Some
                            | _ -> defaultTraverse(expr)
                        else
                            None }
            AstTraversal.Traverse(line, col, parseTree, walker)

    // Given a cursor position here:
    //    f(x)   .   iden
    //                   ^
    // walk the AST to find the position here:
    //    f(x)   .   iden
    //       ^
    // On success, return Some(thatPos, boolTrueIfCursorIsAfterTheDotButBeforeTheIdentifier)
    // If there's no dot, return None, so for example
    //    foo
    //      ^
    // would return None
    // TODO would be great to unify this with GetRangeOfExprLeftOfDot above, if possible, as they are similar
    let TryFindExpressionASTLeftOfDotLeftOfCursor(line,col,parseTreeOpt) =
        match parseTreeOpt with 
        | None -> None 
        | Some(parseTree) ->
        let dive x = AstTraversal.dive x
        let pos = Pos.fromVS line col  // line was 0-based, need 1-based
        let pick x = AstTraversal.pick pos line col x
        let walker = 
            { new AstTraversal.AstVisitorBase<_>() with
                member this.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
                    let pick = pick expr.Range
                    let traverseSynExpr, defaultTraverse, expr = traverseSynExpr, defaultTraverse, expr  // for debugging: debugger does not get object expression params as local vars
                    if not(rangeContainsPos expr.Range pos) then 
                        match expr with
                        | SynExpr.DiscardAfterMissingQualificationAfterDot(e,_m) ->
                            // This happens with e.g. "f(x)  .   $" when you bring up a completion list a few spaces after a dot.  The cursor is not 'in the parse tree',
                            // but the dive algorithm will dive down into this node, and this is the one case where we do want to give a result despite the cursor
                            // not properly being in a node.
                            match traverseSynExpr(e) with
                            | None -> Some(e.Range.End, false)
                            | r -> r
                        | _ -> 
                            // This happens for e.g. "System.Console.[]$", where the ".[]" token is thrown away by the parser and we dive into the System.Console longId 
                            // even though the cursor/dot is not in there.  In those cases we want to return None, because there is not really a dot completion before
                            // the cursor location.
                            None
                    else
                        let rec traverseLidOrElse (optExprIfLeftOfLongId : SynExpr option) (LongIdentWithDots(lid,dots) as lidwd) =
                            let resultIfLeftOfLongId =
                                match optExprIfLeftOfLongId with
                                | None -> None
                                | Some e -> Some(e.Range.End, posGeq lidwd.Range.Start pos)
                            match dots |> List.mapi (fun i x -> i,x) |> List.rev |> List.tryFind (fun (_,m) -> posGt pos m.Start) with
                            | None -> resultIfLeftOfLongId
                            | Some(n,_) -> Some((List.nth lid n).idRange.End, (List.length lid = n+1)    // foo.$
                                                                              || (posGeq (List.nth lid (n+1)).idRange.Start pos))  // foo.$bar
                        match expr with
                        | SynExpr.LongIdent(_isOptional, lidwd, _altNameRefCell, _m) ->
                            traverseLidOrElse None lidwd
                        | SynExpr.LongIdentSet(lidwd, exprRhs, _m) ->
                            [ dive lidwd lidwd.Range (traverseLidOrElse None)
                              dive exprRhs exprRhs.Range traverseSynExpr
                            ] |> pick expr
                        | SynExpr.DotGet(exprLeft, dotm, lidwd, _m) ->
                            let afterDotBeforeLid = mkRange dotm.FileName dotm.End lidwd.Range.Start 
                            [ dive exprLeft exprLeft.Range traverseSynExpr
                              dive exprLeft afterDotBeforeLid (fun e -> Some(e.Range.End, true))
                              dive lidwd lidwd.Range (traverseLidOrElse (Some exprLeft))
                            ] |> pick expr
                        | SynExpr.DotSet(exprLeft, lidwd, exprRhs, _m) ->
                            [ dive exprLeft exprLeft.Range traverseSynExpr
                              dive lidwd lidwd.Range (traverseLidOrElse(Some exprLeft))
                              dive exprRhs exprRhs.Range traverseSynExpr
                            ] |> pick expr
                        | SynExpr.NamedIndexedPropertySet(lidwd, exprIndexer, exprRhs, _m) ->
                            [ dive lidwd lidwd.Range (traverseLidOrElse None)
                              dive exprIndexer exprIndexer.Range traverseSynExpr
                              dive exprRhs exprRhs.Range traverseSynExpr
                            ] |> pick expr
                        | SynExpr.DotNamedIndexedPropertySet(exprLeft, lidwd, exprIndexer, exprRhs, _m) ->
                            [ dive exprLeft exprLeft.Range traverseSynExpr
                              dive lidwd lidwd.Range (traverseLidOrElse(Some exprLeft))
                              dive exprIndexer exprIndexer.Range traverseSynExpr
                              dive exprRhs exprRhs.Range traverseSynExpr
                            ] |> pick expr
                        | SynExpr.DiscardAfterMissingQualificationAfterDot(e,m) ->
                            match traverseSynExpr(e) with
                            | None -> 
                                if posEq m.End pos then
                                    // the cursor is at the dot
                                    Some(e.Range.End, false)
                                else
                                    // the cursor is left of the dot
                                    None
                            | r -> r
                        | SynExpr.App(ExprAtomicFlag.NonAtomic, true, (SynExpr.Ident(ident)), lhs, _m) 
                            when ident.idText = "op_ArrayLookup" 
                                 && not(AstTraversal.rangeContainsPosLeftEdgeInclusive lhs.Range pos) ->
                            match defaultTraverse expr with
                            | None ->
                                // (expr).(expr) is an ML-deprecated array lookup, but we want intellisense on the dot
                                // also want it for e.g. [|arr|].(0)
                                Some(lhs.Range.End, false)
                            | x -> x  // we found the answer deeper somewhere in the lhs
                        | _ -> defaultTraverse(expr) }
        AstTraversal.Traverse(line, col, parseTree, walker)
    
    type TS = AstTraversal.TraverseStep

    /// try to determine completion context for the given pair (row, columns)
    let TryGetCompletionContext (line : int, col : int, untypedParseInfoOpt : UntypedParseInfo option) : CompletionContext option = 
        let parsedInputOpt =
            match untypedParseInfoOpt with
            | Some upi -> upi.ParseTree
            | None -> None

        match parsedInputOpt with
        | None -> None
        | Some pt -> 
        
        let pos = Pos.fromVS line col  // line was 0-based, need 1-based
        
        let parseLid (LongIdentWithDots(lid, dots)) =            
            let rec collect plid (parts : Ident list) (dots : range list) = 
                match parts, dots with
                | [],_ -> Some (plid, None)
                | x::xs, ds ->
                    if rangeContainsPos x.idRange pos then
                        // pos lies with the range of current identifier
                        let s = x.idText.Substring(0, pos.Column - x.idRange.Start.Column)
                        let residue = if s.Length <> 0 then Some s else None
                        Some(plid, residue)
                    elif posGt x.idRange.Start pos then
                        // can happen if caret is placed after dot but before the exising identifier A. $ B
                        // return accumulated plid with no residue
                        Some (plid, None)
                    else
                        match ds with
                        | [] -> 
                            // pos lies after the id and no dots found - return accumulated plid and current id as residue 
                            Some(plid, Some(x.idText))
                        | d::ds ->
                            if posGeq pos d.End  then 
                                // pos lies after the dot - proceed to the next identifier
                                collect ((x.idText)::plid) xs ds
                            else
                                // pos after the id but before the dot
                                // A $.B - return nothing
                                None

            match collect [] lid dots with
            | Some (parts, residue) ->
                Some((List.rev parts), residue)
            | None -> None
        
        let (|Class|Interface|Struct|Unknown|Invalid|) synAttributes = 
            let (|SynAttr|_|) name (attr : SynAttribute) = 
                match attr with
                | {TypeName = LongIdentWithDots([x], _)} when x.idText = name -> Some ()
                | _ -> None
            
            let rec getKind isClass isInterface isStruct = 
                function
                | [] -> isClass, isInterface, isStruct
                | (SynAttr "Class")::xs -> getKind true isInterface isStruct xs
                | (SynAttr "AbstractClass")::xs -> getKind true isInterface isStruct xs
                | (SynAttr "Interface")::xs -> getKind isClass true isStruct xs
                | (SynAttr "Struct")::xs -> getKind isClass isInterface true xs
                | _::xs -> getKind isClass isInterface isInterface xs

            match getKind false false false synAttributes with
            | false, false, false -> Unknown
            | true, false, false -> Class
            | false, true, false -> Interface
            | false, false, true -> Struct
            | _ -> Invalid

        let getCompletionContextForInheritSynMember ((ComponentInfo(synAttributes, _, _, _,_, _, _, _)), typeDefnKind : SynTypeDefnKind, completionPath) = 
            
            let success k = Some (Inherit (k, completionPath))

            // if kind is specified - take it
            // if kind is non-specified 
            //  - try to obtain it from attribute
            //      - if no attributes present - infer kind from members
            match typeDefnKind with
            | TyconClass -> 
                match synAttributes with
                | Class | Unknown -> success Class
                | _ -> Some CompletionContext.Invalid // non-matching attributes
            | TyconInterface -> 
                match synAttributes with
                | Interface | Unknown -> success Interface
                | _ -> Some CompletionContext.Invalid // non-matching attributes
            | TyconStruct -> 
                // display nothing for structs
                Some CompletionContext.Invalid
            | TyconUnspecified ->
                match synAttributes with
                | Class -> success Class
                | Interface -> success Interface
                | Unknown -> 
                    // user do not specify kind explicitly or via attributes
                    success Unknown
                | _ -> 
                    // unable to uniquely detect kind from the attributes - return invalid context
                    Some CompletionContext.Invalid
            | _ -> None

        // checks if we are in rhs of the range operator
        let isInRhsOfRangeOp (p : AstTraversal.TraversePath) = 
            match p with
            | TS.Expr(SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.App(ExprAtomicFlag.NonAtomic, true, SynExpr.Ident(ident), _, _), _, _))::_  
                when ident.idText = "op_Range"-> true
            | _ -> false

        let walker = 
            { 
                new AstTraversal.AstVisitorBase<_>() with
                    member this.VisitExpr(path, traverseSynExpr, defaultTraverse, expr) = 
                        if isInRhsOfRangeOp path then
                            match defaultTraverse expr with
                            | None -> Some (CompletionContext.RangeOperator) // nothing was found - report that we were in the context of range operator
                            | x -> x // ok, we found something - return it
                        else defaultTraverse expr

                    member this.VisitRecordField(path, copyOpt, field) = 
                        let contextFromTreePath completionPath = 
                            // detect records usage in constructor
                            match path with
                            | TS.Expr(_)::TS.Binding(_):: TS.MemberDefn(_)::TS.TypeDefn(SynTypeDefn.TypeDefn(ComponentInfo(_, _, _, [id], _, _, _, _), _, _, _))::_ ->  
                                RecordContext.Constructor(id.idText)
                            | _ -> RecordContext.New (completionPath)
                        match field with
                        | Some field -> 
                            match parseLid field with
                            | Some (completionPath) ->
                                let recordContext = 
                                    match copyOpt with
                                    | Some (s : SynExpr) -> RecordContext.CopyOnUpdate(s.Range, completionPath)
                                    | None -> contextFromTreePath completionPath
                                Some (CompletionContext.RecordField recordContext)
                            | None -> None
                        | None ->
                            let recordContext = 
                                match copyOpt with
                                | Some s -> RecordContext.CopyOnUpdate(s.Range, ([], None))
                                | None -> contextFromTreePath ([], None)
                            Some (CompletionContext.RecordField recordContext)
                                
                    member this.VisitInheritSynMemberDefn(componentInfo, typeDefnKind, synType, _members, _range) = 
                        match synType with
                        | SynType.LongIdent lidwd ->                                 
                            match parseLid lidwd with
                            | Some (completionPath) -> getCompletionContextForInheritSynMember (componentInfo, typeDefnKind, completionPath)
                            | None -> Some (CompletionContext.Invalid) // A $ .B -> no completion list
                        | _ -> None }
        AstTraversal.Traverse(line, col, pt, walker)