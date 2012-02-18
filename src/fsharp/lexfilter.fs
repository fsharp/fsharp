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

/// LexFilter - process the token stream prior to parsing.
/// Implements the offside rule and a copule of other lexical transformations.
module internal Microsoft.FSharp.Compiler.Lexfilter

open Internal.Utilities
open Internal.Utilities.Text.Lexing
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Parser
open Microsoft.FSharp.Compiler.Lexhelp


#if CHECKED
let debug = (System.Environment.GetEnvironmentVariable("DEBUG_LEXFILTER") <> null)
#else
let debug = false
#endif

let stringOfPos (p:Position) = sprintf "(%d:%d)" p.OriginalLine p.Column
let outputPos os (p:Position) = Printf.fprintf os "(%d:%d)" p.OriginalLine p.Column

/// Used for warning strings, which should display columns as 1-based and display 
/// the lines after taking '# line' directives into account (i.e. do not use
/// p.OriginalLine)
let warningStringOfPos (p:Position) = sprintf "(%d:%d)" p.Line (p.Column + 1)

type Context = 
    // Position is position of keyword.
    // bool indicates 'LET' is an offside let that's part of a CtxtSeqBlock where the 'in' is optional 
    | CtxtLetDecl of bool * Position  
    | CtxtIf of Position  
    | CtxtTry of Position  
    | CtxtFun of Position  
    | CtxtFunction of Position  
    | CtxtWithAsLet of Position  // 'with' when used in an object expression 
    | CtxtWithAsAugment of Position   // 'with' as used in a type augmentation 
    | CtxtMatch of Position  
    | CtxtFor of Position  
    | CtxtWhile of Position  
    | CtxtWhen of Position   
    | CtxtVanilla of Position * bool // boolean indicates if vanilla started with 'x = ...' or 'x.y = ...'
    | CtxtThen of Position  
    | CtxtElse of Position 
    | CtxtDo of Position 
    | CtxtInterfaceHead of Position 
    | CtxtTypeDefns of Position    // 'type <here> =', not removed when we find the "="
    
    | CtxtNamespaceHead of Position  * token
    | CtxtModuleHead of Position  * token
    | CtxtMemberHead of Position 
    | CtxtMemberBody of Position 
    // If bool is true then this is "whole file" 
    //     module A.B
    // If bool is false, this is a "module declaration" 
    //     module A = ...
    | CtxtModuleBody of Position  * bool 
    | CtxtNamespaceBody of Position 
    | CtxtException of Position 
    | CtxtParen of Parser.token * Position 
    // Position is position of following token 
    | CtxtSeqBlock of FirstInSequence * Position * AddBlockEnd   
    // first bool indicates "was this 'with' followed immediately by a '|'"? 
    | CtxtMatchClauses of bool * Position   

    member c.StartPos = 
        match c with 
        | CtxtNamespaceHead (p,_) | CtxtModuleHead (p,_) | CtxtException p | CtxtModuleBody (p,_) | CtxtNamespaceBody p
        | CtxtLetDecl (_,p) | CtxtDo p | CtxtInterfaceHead p | CtxtTypeDefns p | CtxtParen (_,p) | CtxtMemberHead p | CtxtMemberBody p
        | CtxtWithAsLet p
        | CtxtWithAsAugment p
        | CtxtMatchClauses (_,p) | CtxtIf p | CtxtMatch p | CtxtFor p | CtxtWhile p | CtxtWhen p | CtxtFunction p | CtxtFun p | CtxtTry p | CtxtThen p | CtxtElse p | CtxtVanilla (p,_)
        | CtxtSeqBlock (_,p,_) -> p

    member c.StartCol = c.StartPos.Column

    override c.ToString() = 
        match c with 
        | CtxtNamespaceHead _ -> "nshead"
        | CtxtModuleHead _ -> "modhead"
        | CtxtException _ -> "exception"
        | CtxtModuleBody _ -> "modbody"
        | CtxtNamespaceBody _ -> "nsbody"
        | CtxtLetDecl(b,p) -> sprintf "let(%b,%s)" b (stringOfPos p)
        | CtxtWithAsLet p -> sprintf "withlet(%s)" (stringOfPos p)
        | CtxtWithAsAugment _ -> "withaug"
        | CtxtDo _ -> "do"
        | CtxtInterfaceHead _ -> "interface-decl"
        | CtxtTypeDefns _ -> "type"
        | CtxtParen _ -> "paren"
        | CtxtMemberHead _ -> "member-head"
        | CtxtMemberBody _ -> "body"
        | CtxtSeqBlock (b,p,_addBlockEnd) -> sprintf "seqblock(%s,%s)" (match b with FirstInSeqBlock -> "first" | NotFirstInSeqBlock -> "subsequent") (stringOfPos p)
        | CtxtMatchClauses _ -> "matching"

        | CtxtIf _ -> "if"
        | CtxtMatch _ -> "match"
        | CtxtFor _ -> "for"
        | CtxtWhile p -> sprintf "while(%s)" (stringOfPos p)
        | CtxtWhen _ -> "when" 
        | CtxtTry _ -> "try"
        | CtxtFun _ -> "fun"
        | CtxtFunction _ -> "function"

        | CtxtThen _ -> "then"
        | CtxtElse p -> sprintf "else(%s)" (stringOfPos p)
        | CtxtVanilla (p,_) -> sprintf "vanilla(%s)" (stringOfPos p)
  
and AddBlockEnd = AddBlockEnd | NoAddBlockEnd | AddOneSidedBlockEnd
and FirstInSequence = FirstInSeqBlock | NotFirstInSeqBlock


let isInfix token = 
    match token with 
    | COMMA 
    | BAR_BAR 
    | AMP_AMP 
    | AMP 
    | OR
    | INFIX_BAR_OP _ 
    | INFIX_AMP_OP _  
    | INFIX_COMPARE_OP _ 
    | DOLLAR 
    // For the purposes of #light processing, <, > and = are not considered to be infix operators.
    // This is because treating them as infix conflicts with their role in other parts of the grammar,
    // e.g. to delimit "f<int>", or for "let f x = ...." 
    //
    // This has the impact that a SeqBlock does not automatically start on the right of a "<", ">" or "=",
    // e.g.
    //     let f x = (x = 
    //                   let a = 1 // no #light block started here, parentheses or 'in' needed
    //                   a + x)
    // LESS | GREATER | EQUALS
    
    | INFIX_AT_HAT_OP _
    | PLUS_MINUS_OP _ 
    | COLON_COLON
    | COLON_GREATER
    | COLON_QMARK_GREATER
    | COLON_EQUALS 
    | MINUS  
    | STAR 
    | INFIX_STAR_DIV_MOD_OP _
    | INFIX_STAR_STAR_OP _ 
    | QMARK_QMARK -> true
    | _ -> false

let isNonAssocInfixToken token = 
    match token with 
    | EQUALS -> true
    | _ -> false

let infixTokenLength token = 
    match token with 
    | COMMA  -> 1
    | AMP -> 1
    | OR -> 1
    | DOLLAR -> 1
    | MINUS -> 1  
    | STAR  -> 1
    | BAR -> 1
    | LESS false -> 1
    | GREATER false -> 1
    | EQUALS -> 1
    | QMARK_QMARK -> 2
    | COLON_GREATER -> 2
    | COLON_COLON -> 2
    | COLON_EQUALS -> 2
    | BAR_BAR -> 2
    | AMP_AMP -> 2
    | INFIX_BAR_OP d 
    | INFIX_AMP_OP d  
    | INFIX_COMPARE_OP d 
    | INFIX_AT_HAT_OP d
    | PLUS_MINUS_OP d 
    | INFIX_STAR_DIV_MOD_OP d
    | INFIX_STAR_STAR_OP d -> d.Length
    | COLON_QMARK_GREATER -> 3
    | _ -> assert false; 1


/// Determine the tokens that may align with the 'if' of an 'if/then/elif/else' without closing
/// the construct
let rec isIfBlockContinuator token =
    match token with 
    // The following tokens may align with the "if" without closing the "if", e.g.
    //    if  ...
    //    then  ...
    //    elif ...
    //    else ... 
    | THEN | ELSE | ELIF -> true  
    // Likewise 
    //    if  ... then  (
    //    ) elif begin 
    //    end else ... 
    | END | RPAREN -> true  
    // The following arise during reprocessing of the inserted tokens, e.g. when we hit a DONE 
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true 
    | ODUMMY(token) -> isIfBlockContinuator(token)
    | _ -> false

/// Determine the token that may align with the 'try' of a 'try/catch' or 'try/finally' without closing
/// the construct
let rec isTryBlockContinuator token =
    match token with 
    // These tokens may align with the "try" without closing the construct, e.g.
    //         try ...
    //         with ... 
    | FINALLY | WITH -> true  
    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true 
    | ODUMMY(token) -> isTryBlockContinuator(token)
    | _ -> false

let rec isThenBlockContinuator token =
    match token with 
    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true 
    | ODUMMY(token) -> isThenBlockContinuator(token)
    | _ -> false

let rec isDoContinuator token =
    match token with 
    // These tokens may align with the "for" without closing the construct, e.g.
    //                       for ... 
    //                          do 
    //                             ... 
    //                          done *)  
    | DONE -> true 
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY(token) -> isDoContinuator(token)
    | _ -> false

let rec isInterfaceContinuator token =
    match token with 
    // These tokens may align with the token "interface" without closing the construct, e.g.
    //                       interface ... with 
    //                         ...
    //                       end   
    | END -> true 
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY(token) -> isInterfaceContinuator(token)
    | _ -> false

let rec isNamespaceContinuator token =
    match token with 
    // These tokens end the construct, e.g. 
    //     namespace A.B.C 
    //     ...
    //     namespace <-- here
    //     .... 
    | Parser.EOF _ | NAMESPACE -> false 
    | ODUMMY(token) -> isNamespaceContinuator token
    | _ -> true // anything else is a namespace continuator 

let rec isTypeContinuator token =
    match token with 
    // The following tokens may align with the token "type" without closing the construct, e.g.
    //     type X = 
    //     | A
    //     | B
    //     and Y = c            <---          'and' HERE
    //     
    //     type X = {
    //        x: int;
    //        y: int
    //     }                     <---          '}' HERE
    //     and Y = c 
    //
    //     type Complex = struct
    //       val im : float
    //     end with                  <---          'end' HERE
    //       static member M() = 1
    //     end 
    | RBRACE | WITH | BAR | AND | END -> true 
                             
    // The following arise during reprocessing of the inserted tokens when we hit a DONE 
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true 
    | ODUMMY(token) -> isTypeContinuator(token)
    | _ -> false

let rec isForLoopContinuator token =
    match token with 
    // This token may align with the "for" without closing the construct, e.g.
    //                      for ... do 
    //                          ... 
    //                       done 
    | DONE -> true 
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true// The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY(token) -> isForLoopContinuator(token)
    | _ -> false

let rec isWhileBlockContinuator token =
    match token with 
    // This token may align with the "while" without closing the construct, e.g.
    //                       while ... do 
    //                          ... 
    //                       done 
    | DONE -> true 
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY(token) -> isWhileBlockContinuator(token)
    | _ -> false

let rec isLetContinuator token =
    match token with 
    // This token may align with the "let" without closing the construct, e.g.
    //                       let ...
    //                       and ... 
    | AND -> true  
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY(token) -> isLetContinuator(token)
    | _ -> false

let rec isTypeSeqBlockElementContinuator token = 
    match token with 
    // A sequence of items separated by '|' counts as one sequence block element, e.g.
    // type x = 
    //   | A                 <-- These together count as one element
    //   | B                 <-- These together count as one element
    //   member x.M1
    //   member x.M2 
    | BAR -> true
    | OBLOCKBEGIN | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ODUMMY(token) -> isTypeSeqBlockElementContinuator token 
    | _ -> false

// Work out when a token doesn't terminate a single item in a sequence definition 
let rec isSeqBlockElementContinuator token =
    isInfix token || 
          // Infix tokens may align with the first column of a sequence block without closing a sequence element and starting a new one 
          // e.g. 
          //  let f x
          //      h x 
          //      |> y                              <------- NOTE: Not a new element in the sequence

    match token with 
    // The following tokens may align with the first column of a sequence block without closing a sequence element and starting a new one *)
    // e.g. 
    // new MenuItem("&Open...", 
    //              new EventHandler(fun _ _ -> 
    //                  ...
    //              ),                              <------- NOTE RPAREN HERE
    //              Shortcut.CtrlO)
    | END | AND | WITH | THEN | RPAREN | RBRACE | RBRACK | BAR_RBRACK | RQUOTE _ -> true 

    // The following arise during reprocessing of the inserted tokens when we hit a DONE
    | ORIGHT_BLOCK_END | OBLOCKEND | ODECLEND -> true 
    | ODUMMY(token) -> isSeqBlockElementContinuator token 
    | _ -> false

let rec isWithAugmentBlockContinuator token = 
    match token with 
    // This token may align with "with" of an augmentation block without closing the construct, e.g.
    //                       interface Foo
    //                          with 
    //                             member ...
    //                          end 
    | END -> true    
    | ODUMMY(token) -> isWithAugmentBlockContinuator(token)
    | _ -> false

let isLongIdentifier token = (match token with IDENT _ | DOT -> true | _ -> false)
let isLongIdentifierOrGlobal token = (match token with GLOBAL | IDENT _ | DOT -> true | _ -> false)

let isAtomicExprEndToken token = 
    match token with
    | IDENT _ 
    | INT8 _ | INT16 _ | INT32 _ | INT64 _ | NATIVEINT _ 
    | UINT8 _ | UINT16 _ | UINT32 _ | UINT64 _ | UNATIVEINT _
    | DECIMAL _ | BIGNUM _  | STRING _ | BYTEARRAY _  | CHAR _ 
    | IEEE32 _ | IEEE64 _ 
    | RPAREN | RBRACK | RBRACE | BAR_RBRACK | END 
    | NULL | FALSE | TRUE | UNDERSCORE -> true
    | _ -> false
    
//----------------------------------------------------------------------------
// give a 'begin' token, does an 'end' token match?
//--------------------------------------------------------------------------
let parenTokensBalance t1 t2 = 
    match t1,t2 with 
    | (LPAREN,RPAREN) 
    | (LBRACE,RBRACE) 
    | (LBRACK,RBRACK) 
    | (INTERFACE,END) 
    | (CLASS,END) 
    | (SIG,END) 
    | (STRUCT,END) 
    | (LBRACK_BAR,BAR_RBRACK)
    | (LESS true,GREATER true) 
    | (BEGIN,END) -> true 
    | (LQUOTE q1,RQUOTE q2) when q1 = q2 -> true 
    | _ -> false
    
/// Used to save some aspects of the lexbuffer state
[<Struct>]
type LexbufState(startPos: Position, 
                 endPos  : Position, 
                 pastEOF : bool) = 
    member x.StartPos = startPos
    member x.EndPos = endPos
    member x.PastEOF = pastEOF

/// Used to save the state related to a token
[<Class>]
type TokenTup = 
    val Token : token
    val LexbufState : LexbufState
    val LastTokenPos: Position * Position
    new (token,state,lastTokenPos) = { Token=token; LexbufState=state;LastTokenPos=lastTokenPos }
    
    /// Returns starting position of the token
    member x.StartPos = x.LexbufState.StartPos
    /// Returns end position of the token
    member x.EndPos = x.LexbufState.EndPos
    
    /// Returns a token 'tok' with the same position as this token
    member x.UseLocation(tok) = 
        let tokState = x.LexbufState 
        TokenTup(tok,LexbufState(tokState.StartPos, tokState.EndPos,false),x.LastTokenPos)
        
    /// Returns a token 'tok' with the same position as this token, except that 
    /// it is shifted by specified number of characters from the left and from the right
    /// Note: positive value means shift to the right in both cases
    member x.UseShiftedLocation(tok, shiftLeft, shiftRight) = 
        let tokState = x.LexbufState 
        TokenTup(tok,LexbufState(tokState.StartPos.ShiftColumnBy(shiftLeft),
                                 tokState.EndPos.ShiftColumnBy(shiftRight),false),x.LastTokenPos)
        


//----------------------------------------------------------------------------
// Utilities for the tokenizer that are needed in other opalces
//--------------------------------------------------------------------------*)

// Strip a bunch of leading '>' of a token, at the end of a typar application
// Note: this is used in the 'service.fs' to do limited postprocessing
let (|TyparsCloseOp|_|) (txt:string) = 
    let angles = txt |> Seq.takeWhile (fun c -> c = '>') |> Seq.toList
    let afterAngles = txt |> Seq.skipWhile (fun c -> c = '>') |> Seq.toList
    if angles.Length = 0 then None else

    let afterOp = 
        match (new System.String(Array.ofSeq afterAngles)) with 
         | "." -> Some DOT
         | "]" -> Some RBRACK
         | "-" -> Some MINUS
         | ".." -> Some DOT_DOT 
         | "?" ->  Some QMARK 
         | "??" -> Some QMARK_QMARK 
         | ":=" -> Some COLON_EQUALS 
         | "::" -> Some COLON_COLON
         | "*" -> Some STAR 
         | "&" -> Some AMP
         | "->" -> Some RARROW 
         | "<-"  -> Some LARROW 
         | "=" -> Some EQUALS 
         | "<" -> Some (LESS false)
         | "$" -> Some DOLLAR
         | "%" -> Some (PERCENT_OP("%") )
         | "%%" -> Some (PERCENT_OP("%%"))
         | "" -> None
         | s -> 
             match  List.ofSeq afterAngles with 
              | ('=' :: _)
              | ('!' :: '=' :: _)
              | ('<' :: _)
              | ('>' :: _)
              | ('$' :: _) -> Some (INFIX_COMPARE_OP(s))
              | ('&' :: _) -> Some (INFIX_AMP_OP(s))
              | ('|' :: _) -> Some (INFIX_BAR_OP(s))
              | ('!' :: _)
              | ('?' :: _)
              | ('~' :: _) -> Some (PREFIX_OP(s))
              | ('@' :: _)
              | ('^' :: _) -> Some (INFIX_AT_HAT_OP(s))
              | ('+' :: _)
              | ('-' :: _) -> Some (PLUS_MINUS_OP(s))
              | ('*' :: '*' :: _) -> Some (INFIX_STAR_STAR_OP(s))
              | ('*' :: _)
              | ('/' :: _)
              | ('%' :: _) -> Some (INFIX_STAR_DIV_MOD_OP(s))
              | _ -> None
    Some([| for _c in angles do yield GREATER |],afterOp)

//----------------------------------------------------------------------------
// build a LexFilter
//--------------------------------------------------------------------------*)
type LexFilter (lightSyntaxStatus:LightSyntaxStatus, lexer, lexbuf: UnicodeLexing.Lexbuf)  = 

    //----------------------------------------------------------------------------
    // Part I. Building a new lex stream from an old
    //
    // A lexbuf is a stateful object that can be enticed to emit tokens by calling
    // 'lexer' functions designed to work with the lexbuf.  Here we fake a new stream
    // coming out of an existing lexbuf.  Ideally lexbufs would be abstract interfaces
    // and we could just build a new abstract interface that wraps an existing one.
    // However that is not how F# lexbufs currently work.
    // 
    // Part of the fakery we perform involves buffering a lookahead token which 
    // we eventually pass on to the client.  However, this client also looks at
    // other aspects of the 'state' of lexbuf directly, e.g. F# lexbufs have a triple
    //    (start-pos, end-pos, eof-reached)
    //
    // You may ask why the F# parser reads this lexbuf state directly.  Well, the
    // pars.fsy code itself it doesn't, but the parser engines (prim-parsing.fs) 
    // certainly do for F#. e.g. when these parsers read a token 
    // from the lexstream they also read the position information and keep this
    // a related stack. 
    //
    // Anyway, this explains the functions getLexbufState(), setLexbufState() etc.
    //--------------------------------------------------------------------------

    // Make sure we don't report 'eof' when inserting a token, and set the positions to the 
    // last reported token position 
    let lexbufStateForInsertedDummyTokens (lastTokenStartPos,lastTokenEndPos) =
        new LexbufState(lastTokenStartPos,lastTokenEndPos,false) 

    let getLexbufState() = 
        new LexbufState(lexbuf.StartPos, lexbuf.EndPos, lexbuf.IsPastEndOfStream)  

    let setLexbufState (p:LexbufState) =
        // if debug then dprintf "SET lex state to; %a\n" output_any p;  
        lexbuf.StartPos <- p.StartPos;  
        lexbuf.EndPos <- p.EndPos; 
        lexbuf.IsPastEndOfStream <- p.PastEOF

    let startPosOfTokenTup (tokenTup:TokenTup) = 
          match tokenTup.Token with
          // EOF token is processed as if on column -1 
          // This forces the closure of all contexts. 
          | Parser.EOF _ -> 
              let p = tokenTup.LexbufState.StartPos
              { p with posColumnOffset = p.posStartOfLineOffset-1 }
          | _ ->  tokenTup.LexbufState.StartPos 

    //----------------------------------------------------------------------------
    // Part II. The state of the new lex stream object.
    //--------------------------------------------------------------------------

    // Ok, we're going to the wrapped lexbuf.  Set the lexstate back so that the lexbuf 
    // appears consistent and correct for the wrapped lexer function. 
    let runWrappedLexerInConsistentLexbufState =
        let savedLexbufState = ref Unchecked.defaultof<LexbufState>
        let haveLexbufState = ref false
        fun () -> 
            let state = if !haveLexbufState then !savedLexbufState else getLexbufState()
            setLexbufState state;
            let lastTokenStart = state.StartPos
            let lastTokenEnd = state.EndPos
            let token = lexer lexbuf
            // Now we've got the token, remember the lexbuf state, associating it with the token 
            // and remembering it as the last observed lexbuf state for the wrapped lexer function. 
            let tokenLexbufState = getLexbufState()
            savedLexbufState := tokenLexbufState;
            haveLexbufState := true;
            TokenTup(token,tokenLexbufState,(lastTokenStart,lastTokenEnd))

    //----------------------------------------------------------------------------
    // Fetch a raw token, either from the old lexer or from our delayedStack
    //--------------------------------------------------------------------------

    let mutable delayedStack = []

    let delayToken tokenTup = 
      delayedStack <- tokenTup :: delayedStack

    let popNextTokenTup() = 
      match delayedStack with 
      | tokenTup :: rest -> 
          delayedStack <- rest; 
          if debug then dprintf "popNextTokenTup: delayed token, tokenStartPos = %a\n" outputPos (startPosOfTokenTup tokenTup); 
          tokenTup
      | [] -> 
          if debug then dprintf "popNextTokenTup: no delayed tokens, running lexer...\n";
          runWrappedLexerInConsistentLexbufState() 
    

    //----------------------------------------------------------------------------
    // Part III. Initial configuration of state.
    //
    // We read a token.  In F# Interactive the parser thread will be correctly blocking
    // here.
    //--------------------------------------------------------------------------

    let mutable initialized = false
    let mutable offsideStack = []
    let mutable prevWasAtomicEnd = false
    
    let peekInitial() =
        let initialLookaheadTokenTup  = popNextTokenTup()
        if debug then dprintf "first token: initialLookaheadTokenLexbufState = %a\n" outputPos (startPosOfTokenTup initialLookaheadTokenTup); 
        
        delayToken initialLookaheadTokenTup; 
        initialized <- true;
        offsideStack <- (CtxtSeqBlock(FirstInSeqBlock,startPosOfTokenTup initialLookaheadTokenTup,NoAddBlockEnd)) :: offsideStack;
        initialLookaheadTokenTup 

    let warn (s:TokenTup) msg = 
        warning(Lexhelp.IndentationProblem(msg,mkSynRange (startPosOfTokenTup s) s.LexbufState.EndPos))

    //----------------------------------------------------------------------------
    // Part IV. Helper functions for pushing contexts and giving good warnings
    // if a context is undented.  
    //
    // Undentation rules
    //--------------------------------------------------------------------------

    let pushCtxt tokenTup (newCtxt:Context) =
        let rec unindentationLimit strict stack = 
            match newCtxt,stack with 
            | _, [] -> (newCtxt.StartPos, -1) 

            // ignore Vanilla because a SeqBlock is always coming 
            | _, (CtxtVanilla _ :: rest) -> unindentationLimit strict rest

            | _, (CtxtSeqBlock _ :: rest) when not strict -> unindentationLimit strict rest
            | _, (CtxtParen _ :: rest) when not strict -> unindentationLimit strict rest

            // 'begin match' limited by minimum of two  
            // '(match' limited by minimum of two  
            | _,(((CtxtMatch _) as ctxt1) :: CtxtSeqBlock _ :: (CtxtParen ((BEGIN | LPAREN),_) as ctxt2) :: _rest)
                      -> if ctxt1.StartCol <= ctxt2.StartCol 
                         then (ctxt1.StartPos,ctxt1.StartCol) 
                         else (ctxt2.StartPos,ctxt2.StartCol) 

             // 'let ... = function' limited by 'let', precisely  
             // This covers the common form 
             //                          
             //     let f x = function   
             //     | Case1 -> ...       
             //     | Case2 -> ...       
            | (CtxtMatchClauses _), (CtxtFunction _ :: CtxtSeqBlock _ :: (CtxtLetDecl  _ as limitCtxt) :: _rest)
                      -> (limitCtxt.StartPos,limitCtxt.StartCol)

            // Otherwise 'function ...' places no limit until we hit a CtxtLetDecl etc...  (Recursive) 
            | (CtxtMatchClauses _), (CtxtFunction _ :: rest)
                      -> unindentationLimit false rest

            // 'try ... with'  limited by 'try'  
            | _,(CtxtMatchClauses _ :: (CtxtTry _ as limitCtxt) :: _rest)
                      -> (limitCtxt.StartPos,limitCtxt.StartCol)

            // 'fun ->' places no limit until we hit a CtxtLetDecl etc...  (Recursive) 
            | _,(CtxtFun _ :: rest)
                      -> unindentationLimit false rest

            // 'f ...{' places no limit until we hit a CtxtLetDecl etc... 
            | _,(CtxtParen (LBRACE,_) :: CtxtVanilla _ :: CtxtSeqBlock _ :: rest)
            | _,(CtxtSeqBlock _ :: CtxtParen(LBRACE,_) :: CtxtVanilla _ :: CtxtSeqBlock _ :: rest)
                      -> unindentationLimit false rest


            // MAJOR PERMITTED UNDENTATION This is allowing:
            //   if x then y else
            //   let x = 3 + 4
            //   x + x  
            // This is a serious thing to allow, but is required since there is no "return" in this language.
            // Without it there is no way of escaping special cases in large bits of code without indenting the main case.
            | CtxtSeqBlock _, (CtxtElse _  :: (CtxtIf _ as limitCtxt) :: _rest) 
                      -> (limitCtxt.StartPos,limitCtxt.StartCol)

            // Permitted inner-construct precise block alighnment: 
            //           interface ...
            //           with ... 
            //           end 
            //           
            //           type ...
            //           with ... 
            //           end 
            | CtxtWithAsAugment _,((CtxtInterfaceHead _ | CtxtMemberHead _ | CtxtException _ | CtxtTypeDefns _) as limitCtxt  :: _rest)
                      -> (limitCtxt.StartPos,limitCtxt.StartCol) 

            // Permit unindentation via parentheses (or begin/end) following a 'then', 'else' or 'do':
            //        if nr > 0 then (  
            //              nr <- nr - 1;
            //              acc <- d;
            //              i <- i - 1
            //        ) else (
            //              i <- -1
            //        );

            // PERMITTED UNDENTATION: Inner construct (then,with,else,do) that dangle, places no limit until we hit the corresponding leading construct CtxtIf, CtxtFor, CtxtWhile, CtxtVanilla etc... *)
            //    e.g.   if ... then ...
            //              expr
            //           else
            //              expr
            //    rather than forcing 
            //           if ... 
            //           then expr
            //           else expr
            //   Also  ...... with
            //           ...           <-- this is before the "with"
            //         end

            | _,((CtxtWithAsAugment _ | CtxtThen _ | CtxtElse _ | CtxtDo _ )  :: rest)
                      -> unindentationLimit false rest


            // '... (function ->' places no limit until we hit a CtxtLetDecl etc....  (Recursive)
            //
            //   e.g.
            //        let fffffff() = function
            //          | [] -> 0
            //          | _ -> 1 
            //
            //   Note this does not allow
            //        let fffffff() = function _ ->
            //           0
            //   which is not a permitted undentation. This undentation would make no sense if there are multiple clauses in the 'function', which is, after all, what 'function' is really for
            //        let fffffff() = function 1 ->
            //           0
            //          | 2 -> ...       <---- not allowed
            | _,(CtxtFunction _ :: rest)
                      -> unindentationLimit false rest

            // 'module ... : sig'    limited by 'module' 
            // 'module ... : struct' limited by 'module' 
            // 'module ... : begin'  limited by 'module' 
            // 'if ... then ('       limited by 'if' 
            // 'if ... then {'       limited by 'if' 
            // 'if ... then ['       limited by 'if' 
            // 'if ... then [|'       limited by 'if' 
            // 'if ... else ('       limited by 'if' 
            // 'if ... else {'       limited by 'if' 
            // 'if ... else ['       limited by 'if' 
            // 'if ... else [|'       limited by 'if' 
            | _,(CtxtParen ((SIG | STRUCT | BEGIN),_) :: CtxtSeqBlock _  :: (CtxtModuleBody (_,false) as limitCtxt) ::  _)
            | _,(CtxtParen ((BEGIN | LPAREN | LBRACK | LBRACE | LBRACK_BAR)      ,_) :: CtxtSeqBlock _ :: CtxtThen _ :: (CtxtIf _         as limitCtxt) ::  _)
            | _,(CtxtParen ((BEGIN | LPAREN | LBRACK | LBRACE | LBRACK_BAR)      ,_) :: CtxtSeqBlock _ :: CtxtElse _ :: (CtxtIf _         as limitCtxt) ::  _)

            // 'f ... ('  in seqblock     limited by 'f' 
            // 'f ... {'  in seqblock     limited by 'f'  NOTE: this is covered by the more generous case above 
            // 'f ... ['  in seqblock     limited by 'f' 
            // 'f ... [|' in seqblock      limited by 'f' 
            // 'f ... Foo<' in seqblock      limited by 'f' 
            | _,(CtxtParen ((BEGIN | LPAREN | LESS true | LBRACK | LBRACK_BAR)      ,_) :: CtxtVanilla _ :: (CtxtSeqBlock _         as limitCtxt) ::  _)

            // 'type C = class ... '       limited by 'type' 
            // 'type C = interface ... '       limited by 'type' 
            // 'type C = struct ... '       limited by 'type' 
            | _,(CtxtParen ((CLASS | STRUCT | INTERFACE),_) :: CtxtSeqBlock _ :: (CtxtTypeDefns _ as limitCtxt) ::  _)
                      -> (limitCtxt.StartPos,limitCtxt.StartCol + 1) 

            | _,(CtxtSeqBlock _ :: CtxtParen((BEGIN | LPAREN | LBRACK | LBRACK_BAR),_) :: CtxtVanilla _ :: (CtxtSeqBlock _ as limitCtxt) :: _)
            | (CtxtSeqBlock _),(CtxtParen ((BEGIN | LPAREN | LBRACE | LBRACK | LBRACK_BAR)      ,_) :: CtxtSeqBlock _ :: ((CtxtTypeDefns _ | CtxtLetDecl _ | CtxtMemberBody _ | CtxtWithAsLet _) as limitCtxt) ::  _)
                      -> (limitCtxt.StartPos,limitCtxt.StartCol + 1) 

            // Permitted inner-construct (e.g. "then" block and "else" block in overall 
            // "if-then-else" block ) block alighnment: 
            //           if ... 
            //           then expr
            //           elif expr  
            //           else expr  
            | (CtxtIf   _ | CtxtElse _ | CtxtThen _), (CtxtIf _ as limitCtxt) :: _rest  
                      -> (limitCtxt.StartPos,limitCtxt.StartCol)
            // Permitted inner-construct precise block alighnment: 
            //           while  ... 
            //           do expr
            //           done   
            | (CtxtDo _), ((CtxtFor  _ | CtxtWhile _) as limitCtxt) :: _rest  
                      -> (limitCtxt.StartPos,limitCtxt.StartCol)


            // These contexts all require indentation by at least one space 
            | _,((CtxtInterfaceHead _ | CtxtNamespaceHead _ | CtxtModuleHead _ | CtxtException _ | CtxtModuleBody (_,false) | CtxtIf _ | CtxtWithAsLet _ | CtxtLetDecl _ | CtxtMemberHead _ | CtxtMemberBody _) as limitCtxt :: _) 
                      -> (limitCtxt.StartPos,limitCtxt.StartCol + 1) 

            // These contexts can have their contents exactly aligning 
            | _,((CtxtParen _ | CtxtFor _ | CtxtWhen _ | CtxtWhile _ | CtxtTypeDefns _ | CtxtMatch _  | CtxtModuleBody (_,true) | CtxtNamespaceBody _ | CtxtTry _ | CtxtMatchClauses _ | CtxtSeqBlock _) as limitCtxt :: _)
                      -> (limitCtxt.StartPos,limitCtxt.StartCol) 
       
        begin match newCtxt with 
        // Don't bother to check pushes of Vanilla blocks since we've 
        // always already pushed a SeqBlock at this position.
        | CtxtVanilla _ -> ()
        | _ -> 
            let p1,c1 = unindentationLimit true offsideStack
            let c2 = newCtxt.StartCol
            if c2 < c1 then 
                warn tokenTup 
                       (if debug then (sprintf "possible incorrect indentation: this token is offside of context at position %s, newCtxt = %A, stack = %A, newCtxtPos = %s, c1 = %d, c2 = %d" (warningStringOfPos p1) newCtxt offsideStack (stringOfPos (newCtxt.StartPos)) c1 c2)  
                        else          (FSComp.SR.lexfltTokenIsOffsideOfContextStartedEarlier(warningStringOfPos p1))    )
        end;
        let newOffsideStack = newCtxt :: offsideStack
        if debug then dprintf "--> pushing, stack = %A\n" newOffsideStack;
        offsideStack <- newOffsideStack

    let popCtxt() = 
        match offsideStack with 
        |  [] -> ()
        | h :: rest -> 
             if debug then dprintf "<-- popping Context(%A), stack = %A\n" h rest;
             offsideStack <- rest

    let replaceCtxt p ctxt = popCtxt(); pushCtxt p ctxt

    //----------------------------------------------------------------------------
    // Peek ahead at a token, either from the old lexer or from our delayedStack
    //--------------------------------------------------------------------------

    let peekNextTokenTup() = 
        let tokenTup = popNextTokenTup()
        delayToken tokenTup; 
        tokenTup
    
    let peekNextToken() = 
        peekNextTokenTup().Token
    
     //----------------------------------------------------------------------------
     // Adjacency precedence rule
     //--------------------------------------------------------------------------

    let isAdjacent (leftTokenTup:TokenTup) rightTokenTup =
        let lparenStartPos = startPosOfTokenTup rightTokenTup
        let tokenEndPos = leftTokenTup.LexbufState.EndPos
        (tokenEndPos = lparenStartPos)
    
    let nextTokenIsAdjacentLParenOrLBrack (tokenTup:TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup()
        match lookaheadTokenTup.Token with 
        | (LPAREN | LBRACK) -> 
            if isAdjacent tokenTup lookaheadTokenTup then Some(lookaheadTokenTup.Token) else None
        | _ -> 
            None

    let nextTokenIsAdjacent firstTokenTup =
        let lookaheadTokenTup = peekNextTokenTup()
        isAdjacent firstTokenTup lookaheadTokenTup

    let peekAdjacentTypars indentation (tokenTup:TokenTup) =
        let lookaheadTokenTup = peekNextTokenTup()
        match lookaheadTokenTup.Token with 
        //
        //IMPLEMENTATION FOR DESIGN CHANGE 1600 IF REQUIRED
        //      | (INFIX_COMPARE_OP "<>",_,_) -> true

        | INFIX_COMPARE_OP "</" | LESS _ -> 
            let tokenEndPos = tokenTup.LexbufState.EndPos 
            if isAdjacent tokenTup lookaheadTokenTup then 
                let stack = ref []
                let rec scanAhead nParen = 
                   let lookaheadTokenTup = popNextTokenTup()
                   let lookaheadToken = lookaheadTokenTup.Token
                   stack := (lookaheadTokenTup,true) :: !stack;
                   let lookaheadTokenStartPos = startPosOfTokenTup lookaheadTokenTup
                   match lookaheadToken with 
                   | Parser.EOF _ | SEMICOLON_SEMICOLON -> false 
                   | _ when indentation && lookaheadTokenStartPos < tokenEndPos -> false
                   | (RPAREN | RBRACK) ->
                       let nParen = nParen - 1
                       if nParen > 0 then 
                           scanAhead nParen 
                       else 
                           false
                   | GREATER _ | GREATER_RBRACK | GREATER_BAR_RBRACK -> 
                       let nParen = nParen - 1
                       let hasAfterOp = (match lookaheadToken  with  GREATER _ -> false | _ -> true)
                       if nParen > 0 then 
                           // Don't smash the token if there is an after op and we're in a nested paren
                           stack := (lookaheadTokenTup,not hasAfterOp) :: (!stack).Tail;
                           scanAhead nParen 
                       else 
                           // On successful parse of a set of type parameters, look for an adjacent (, e.g. 
                           //    M<int>(args)
                           // and insert a HIGH_PRECEDENCE_PAREN_APP
                           if not hasAfterOp && (match nextTokenIsAdjacentLParenOrLBrack lookaheadTokenTup with Some(LPAREN) -> true | _ -> false) then
                               let dotTokenTup = peekNextTokenTup()
                               stack := (dotTokenTup.UseLocation(HIGH_PRECEDENCE_PAREN_APP),false) :: !stack
                           true
                   | INFIX_COMPARE_OP (TyparsCloseOp(greaters,afterOp)) -> 
                       let nParen = nParen - greaters.Length
                       if nParen > 0 then 
                           // Don't smash the token if there is an after op and we're in a nested paren
                           stack := (lookaheadTokenTup,not afterOp.IsSome) :: (!stack).Tail;
                           scanAhead nParen 
                       else 
                           // On successful parse of a set of type parameters, look for an adjacent (, e.g. 
                           //    M<C<int>>(args)
                           // and insert a HIGH_PRECEDENCE_PAREN_APP
                           if afterOp.IsNone && (match nextTokenIsAdjacentLParenOrLBrack lookaheadTokenTup with Some LPAREN -> true | _ -> false)  then
                               let dotTokenTup = peekNextTokenTup()
                               stack := (dotTokenTup.UseLocation(HIGH_PRECEDENCE_PAREN_APP),false) :: !stack
                           true
                   | (LPAREN | LESS _ | LBRACK | LBRACK_LESS | INFIX_COMPARE_OP "</") -> 
                       scanAhead (nParen+1)
                       
                   // These tokens CAN occur in non-parenthesized positions in the grammar of types or type parameter definitions 
                   // Thus we explicitly DO consider these to be type applications:
                   //      f<int>x
                   //      f<global.int>x
                   //      f<x * x>x
                   //      f<x , x>x
                   //      f<x ^ x>x
                   //      f<x ^- x>x
                   //      f<x / x>x
                   //      f<x -> x>x
                   //      f<x # x>x
                   //      f<x ' x>x
                   | DEFAULT | COLON | COLON_GREATER | STRUCT | NULL | DELEGATE | AND | WHEN 
                   | DOT_DOT
                   | INFIX_AT_HAT_OP "^" 
                   | INFIX_AT_HAT_OP "^-" 
                   | INFIX_STAR_DIV_MOD_OP "/" 
                   | MINUS 
                   | INT32 _ 
                   | DOT | UNDERSCORE 
                   | IDENT _ | COMMA | RARROW | HASH 
                   | STAR | QUOTE -> 
                       scanAhead nParen


                   // All other tokens ARE assumed to be part of the grammar of types or type parameter definitions 
                   | _ -> 
                       if nParen > 1 then 
                           scanAhead nParen 
                       else 
                           false

                let res = scanAhead 0
                // Put the tokens back on and smash them up if needed
                !stack |> List.iter (fun (tokenTup,smash) ->
                    if smash then 
                        match tokenTup.Token with 
                        | INFIX_COMPARE_OP "</" ->
                            delayToken (tokenTup.UseShiftedLocation(INFIX_STAR_DIV_MOD_OP "/", 1, 0));
                            delayToken (tokenTup.UseShiftedLocation(LESS res, 0, -1));
                        | GREATER_BAR_RBRACK -> 
                            delayToken (tokenTup.UseShiftedLocation(BAR_RBRACK, 1, 0));
                            delayToken (tokenTup.UseShiftedLocation(GREATER res, 0, -2));
                        | GREATER_RBRACK ->
                            delayToken (tokenTup.UseShiftedLocation(RBRACK, 1, 0));
                            delayToken (tokenTup.UseShiftedLocation(GREATER res, 0, -1));
                        | GREATER _ ->
                            delayToken (tokenTup.UseLocation(GREATER res));
                        | (INFIX_COMPARE_OP (TyparsCloseOp(greaters,afterOp) as opstr)) -> 
                            match afterOp with
                            | None -> ()
                            | Some tok -> delayToken (tokenTup.UseShiftedLocation(tok, greaters.Length, 0))
                            for i = greaters.Length - 1 downto 0 do
                                delayToken (tokenTup.UseShiftedLocation(greaters.[i] res, i, -opstr.Length + i + 1));
                        | _ -> delayToken tokenTup
                    else
                        delayToken tokenTup);
                res
            else 
                false
        | _ -> false

     //----------------------------------------------------------------------------
     // End actions
     //--------------------------------------------------------------------------

    let returnToken (tokenLexbufState:LexbufState) tok = 

        setLexbufState(tokenLexbufState);
        prevWasAtomicEnd <- isAtomicExprEndToken(tok);
        tok
              
     //----------------------------------------------------------------------------
     // Parse and transform the stream of tokens coming from popNextTokenTup, pushing
     // contexts where needed, popping them where things are offside, balancing
     // parentheses and other constructs.
     //--------------------------------------------------------------------------

              
    let rec hwTokenFetch (useBlockRule) =
            let tokenTup = popNextTokenTup()
            let tokenReplaced = rulesForBothSoftWhiteAndHardWhite(tokenTup)
            if tokenReplaced then hwTokenFetch(useBlockRule) else 

            let tokenStartPos = (startPosOfTokenTup tokenTup)
            let token = tokenTup.Token
            let tokenLexbufState = tokenTup.LexbufState
            let tokenStartCol = tokenStartPos.Column

            let isSameLine() = 
                match token with 
                | Parser.EOF _ -> false
                | _ -> (startPosOfTokenTup (peekNextTokenTup())).OriginalLine = tokenStartPos.OriginalLine

            let isControlFlowOrNotSameLine() = 
                match token with 
                | Parser.EOF _ -> false
                | _ -> 
                    not (isSameLine()) ||  
                    (match peekNextToken() with TRY | MATCH | IF | LET _ | FOR | WHILE -> true | _ -> false) 

            // Look for '=' or '.Id.id.id = ' after an identifier
            let rec isLongIdentEquals token = 
                match token with 
                | Parser.GLOBAL
                | Parser.IDENT _ -> 
                    let rec loop() = 
                        let tokenTup = popNextTokenTup()
                        let res = 
                            match tokenTup.Token with 
                            | Parser.EOF _ -> false
                            | DOT -> 
                                let tokenTup = popNextTokenTup()
                                let res = 
                                    match tokenTup.Token with 
                                    | Parser.EOF _ -> false
                                    | IDENT _ -> loop()
                                    | _ -> false
                                delayToken tokenTup
                                res
                            | EQUALS -> 
                               true 
                            | _ -> false
                        delayToken tokenTup
                        res
                    loop()
                | _ -> false

            let reprocess() = 
              delayToken tokenTup; 
              hwTokenFetch(useBlockRule)

            let reprocessWithoutBlockRule() = 
              delayToken tokenTup; 
              hwTokenFetch(false)
            
            let insertTokenFromPrevPosToCurrentPos tok = 
              delayToken tokenTup; 
              if debug then dprintf "inserting %A\n" tok;
              returnToken (lexbufStateForInsertedDummyTokens (startPosOfTokenTup tokenTup, tokenTup.LexbufState.EndPos)) tok

            let insertToken tok = 
              delayToken tokenTup; 
              if debug then dprintf "inserting %A\n" tok; 
              returnToken (lexbufStateForInsertedDummyTokens (startPosOfTokenTup tokenTup, tokenTup.LexbufState.EndPos)) tok

            let isSemiSemi = match token with SEMICOLON_SEMICOLON -> true | _ -> false

            match token,offsideStack with 

            // Balancing rule. Every 'in' terminates all surrounding blocks up to a CtxtLetDecl, and will be swallowed by 
            // terminating the corresponding CtxtLetDecl in the rule below. 
            // Balancing rule. Every 'done' terminates all surrounding blocks up to a CtxtDo, and will be swallowed by 
            // terminating the corresponding CtxtDo in the rule below. 
            |  (END | SEMICOLON_SEMICOLON | ELSE | ELIF |  DONE |  IN | RPAREN | GREATER true | RBRACE | RBRACK | BAR_RBRACK | WITH | FINALLY | RQUOTE _),  stack
                
                when 
                  (nonNil stack &&
                   match token,stack with 
                   | END, (CtxtWithAsAugment(_)  :: _)
                   | (ELSE | ELIF), (CtxtIf _ :: _)
                   | DONE         , (CtxtDo _ :: _)
                   | IN           , ((CtxtFor _ (* for x in ienum ... *) | CtxtLetDecl _) :: _)
                   // WITH balances except in the following contexts.... Phew - an overused keyword! 
                   | WITH         , (  ((CtxtMatch _ | CtxtException _ | CtxtMemberHead _ | CtxtInterfaceHead _ | CtxtTry _ | CtxtTypeDefns _)  :: _)
                                         // This is the nasty record/object-expression case 
                                         | (CtxtSeqBlock _ :: CtxtParen(LBRACE,_)  :: _) )
                   | FINALLY      , (CtxtTry _  :: _) -> false
                   // NOTE: ;; does not terminate a 'namespace' body. 
                   | SEMICOLON_SEMICOLON, (CtxtSeqBlock _ :: CtxtNamespaceBody _ :: _) -> false
                   | SEMICOLON_SEMICOLON, (CtxtSeqBlock _ :: CtxtModuleBody (_,true) :: _) -> false
                   | t2           , (CtxtParen(t1,_) :: _) -> not (parenTokensBalance t1  t2)
                   | _ -> true)
                
                -> 
                let ctxt = offsideStack.Head
                if debug then dprintf "IN/ELSE/ELIF/DONE/RPAREN/RBRACE/END at %a terminates context at position %a\n" outputPos tokenStartPos outputPos ctxt.StartPos;
                popCtxt();
                (match ctxt with 
                 | CtxtFun _
                 | CtxtMatchClauses _ 
                 | CtxtWithAsLet _       
                         -> (if debug then dprintf "--> inserting OEND\n");     
                            insertToken OEND

                 | CtxtWithAsAugment _       
                         -> (if debug then dprintf "--> closing WithAsAugment that didn't have an END, inserting ODECLEND\n");     
                            insertToken ODECLEND
                 
                 | CtxtDo _        
                 | CtxtLetDecl (true,_) -> 
                             (if debug then dprintf "--> inserting ODECLEND\n"); 
                             insertToken ODECLEND 
                             
                 | CtxtSeqBlock(_,_,AddBlockEnd) ->  
                             (if debug then dprintf "--> inserting OBLOCKEND\n"); 
                             insertToken OBLOCKEND 

                 | CtxtSeqBlock(_,_,AddOneSidedBlockEnd) ->  
                             (if debug then dprintf "--> inserting ORIGHT_BLOCK_END\n"); 
                             insertToken ORIGHT_BLOCK_END 

                 
                 | _                -> reprocess())

            // reset on ';;' rule. A ';;' terminates ALL entries 
            |  SEMICOLON_SEMICOLON, []  -> 
                if debug then dprintf ";; scheduling a reset\n";
                delayToken(tokenTup.UseLocation(ORESET));
                returnToken tokenLexbufState SEMICOLON_SEMICOLON

            |  ORESET, []  -> 
                if debug then dprintf "performing a reset after a ;; has been swallowed\n";
                // NOTE: The parser thread of F# Interactive will often be blocked on this call, e.g. after an entry has been 
                // processed and we're waiting for the first token of the next entry. 
                peekInitial() |> ignore
                hwTokenFetch(true) 


            // Balancing rule. Encountering an 'in' balances with a 'let'. i.e. even a non-offside 'in' closes a 'let' 
            // The 'IN' token is thrown away and becomes an ODECLEND 
            |  IN, (CtxtLetDecl (blockLet,offsidePos) :: _) -> 
                if debug then dprintf "IN at %a (becomes %s)\n" outputPos tokenStartPos (if blockLet then "ODECLEND" else "IN");
                if tokenStartCol < offsidePos.Column then warn tokenTup (FSComp.SR.lexfltIncorrentIndentationOfIn());
                popCtxt();
                // Make sure we queue a dummy token at this position to check if any other pop rules apply
                delayToken(tokenTup.UseLocation(ODUMMY(token))); 
                returnToken tokenLexbufState (if blockLet then ODECLEND else token)

            // Balancing rule. Encountering a 'done' balances with a 'do'. i.e. even a non-offside 'done' closes a 'do' 
            // The 'DONE' token is thrown away and becomes an ODECLEND 
            |  DONE, (CtxtDo offsidePos :: _) -> 
                if debug then dprintf "DONE at %a terminates CtxtDo(offsidePos=%a)\n" outputPos tokenStartPos outputPos offsidePos;
                popCtxt();
                // reprocess as the DONE may close a DO context 
                delayToken(tokenTup.UseLocation(ODECLEND)); 
                hwTokenFetch(useBlockRule)

            // Balancing rule. Encountering a ')' or '}' balances with a '(' or '{', even if not offside 
            |  ((END | RPAREN | RBRACE | RBRACK | BAR_RBRACK | RQUOTE _ | GREATER true) as t2), (CtxtParen (t1,_) :: _) 
                    when parenTokensBalance t1 t2  ->
                if debug then dprintf "RPAREN/RBRACE/RBRACK/BAR_RBRACK/RQUOTE/END at %a terminates CtxtParen()\n" outputPos tokenStartPos;
                popCtxt();
                // Queue a dummy token at this position to check if any closing rules apply
                delayToken(tokenTup.UseLocation(ODUMMY(token))); 
                returnToken tokenLexbufState token

            // Balancing rule. Encountering a 'end' can balance with a 'with' but only when not offside 
            |  END, (CtxtWithAsAugment(offsidePos) :: _) 
                       when not (tokenStartCol + 1 <= offsidePos.Column) -> 
                if debug then dprintf "END at %a terminates CtxtWithAsAugment()\n" outputPos tokenStartPos;
                popCtxt();                
                delayToken(tokenTup.UseLocation(ODUMMY(token))); // make sure we queue a dummy token at this position to check if any closing rules apply
                returnToken tokenLexbufState OEND

            //  Transition rule. CtxtNamespaceHead ~~~> CtxtSeqBlock 
            //  Applied when a token other then a long identifier is seen 
            | _, (CtxtNamespaceHead (namespaceTokenPos, prevToken) :: _) -> 
                match prevToken, token with 
                | NAMESPACE, GLOBAL when namespaceTokenPos.Column < tokenStartPos.Column -> 
                   replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token));
                   returnToken tokenLexbufState token
                | (NAMESPACE | DOT), IDENT _ when namespaceTokenPos.Column < tokenStartPos.Column -> 
                   replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token));
                   returnToken tokenLexbufState token
                | IDENT _, DOT when namespaceTokenPos.Column < tokenStartPos.Column -> 
                   replaceCtxt tokenTup (CtxtNamespaceHead (namespaceTokenPos, token));
                   returnToken tokenLexbufState token
                | _ -> 
                   if debug then dprintf "CtxtNamespaceHead: pushing CtxtSeqBlock\n";
                   popCtxt();
                   // Don't push a new context if next token is EOF, since that raises an offside warning
                   match tokenTup.Token with 
                   | Parser.EOF _ -> 
                       returnToken tokenLexbufState token
                   | _ -> 
                       delayToken tokenTup; 
                       pushCtxt tokenTup (CtxtNamespaceBody namespaceTokenPos);
                       pushCtxtSeqBlockAt (tokenTup, true, AddBlockEnd) 
                       hwTokenFetch(false)
                   


            //  Transition rule. CtxtModuleHead ~~~> push CtxtModuleBody; push CtxtSeqBlock 
            //  Applied when a ':' or '=' token is seen 
            //  Otherwise it's a 'head' module declaration, so ignore it 
            | _, (CtxtModuleHead (moduleTokenPos,prevToken) :: _)  ->
                match prevToken, token with 
                | MODULE, GLOBAL when moduleTokenPos.Column < tokenStartPos.Column -> 
                   replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token));
                   returnToken tokenLexbufState token
                | MODULE, (PUBLIC | PRIVATE | INTERNAL) when moduleTokenPos.Column < tokenStartPos.Column -> 
                   returnToken tokenLexbufState token
                | (MODULE | DOT), IDENT _ when moduleTokenPos.Column < tokenStartPos.Column -> 
                   replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token));
                   returnToken tokenLexbufState token
                | IDENT _, DOT when moduleTokenPos.Column < tokenStartPos.Column -> 
                   replaceCtxt tokenTup (CtxtModuleHead (moduleTokenPos, token));
                   returnToken tokenLexbufState token
                | _, (EQUALS | COLON) -> 
                   if debug then dprintf "CtxtModuleHead: COLON/EQUALS, pushing CtxtModuleBody and CtxtSeqBlock\n";
                   popCtxt();
                   pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos,false));
                   pushCtxtSeqBlock(true,AddBlockEnd);
                   returnToken tokenLexbufState token
                | _ -> 
                   if debug then dprintf "CtxtModuleHead: start of file, CtxtSeqBlock\n";
                   popCtxt();
                   // Don't push a new context if next token is EOF, since that raises an offside warning
                   match tokenTup.Token with 
                   | Parser.EOF _ -> 
                       returnToken tokenLexbufState token
                   | _ -> 
                       delayToken tokenTup; 
                       pushCtxt tokenTup (CtxtModuleBody (moduleTokenPos,true));
                       pushCtxtSeqBlockAt (tokenTup, true, AddBlockEnd) 
                       hwTokenFetch(false)


            //  Offside rule for SeqBlock.  
            //      f x
            //      g x
            //    ...
            | _, (CtxtSeqBlock(_,offsidePos,addBlockEnd) :: rest) when 
                            
                   // NOTE: ;; does not terminate a 'namespace' body. 
                   ((isSemiSemi && not (match rest with (CtxtNamespaceBody _ | CtxtModuleBody (_,true)) :: _ -> true | _ -> false)) || 
                        let grace = 
                            match token, rest with 
                             // When in a type context allow a grace of 2 column positions for '|' tokens, permits 
                             //  type x = 
                             //      A of string    <-- note missing '|' here - bad style, and perhaps should be disallowed
                             //    | B of int 
                                      
                            | BAR, (CtxtTypeDefns _ :: _) -> 2

                             // This ensures we close a type context seq block when the '|' marks
                             // of a type definition are aligned with the 'type' token. 
                             // 
                             //  type x = 
                             //  | A 
                             //  | B 
                             //  
                             //  <TOKEN>    <-- close the type context sequence block here *)

                            | _, (CtxtTypeDefns posType :: _) when offsidePos.Column = posType.Column && not (isTypeSeqBlockElementContinuator token) -> -1

                             // This ensures we close a namespace body when we see the next namespace definition 
                             // 
                             //  namespace A.B.C 
                             //  ...
                             //  
                             //  namespace <-- close the namespace body context here 

                            | _, (CtxtNamespaceBody posNamespace :: _) when offsidePos.Column = posNamespace.Column && (match token with NAMESPACE -> true | _ -> false) -> -1


                            | _ -> 
                               // Allow a grace of >2 column positions for infix tokens, permits 
                               //  let x =           
                               //        expr + expr 
                               //      + expr + expr 
                               // And   
                               //    let x =           
                               //          expr  
                               //       |> f expr 
                               //       |> f expr  
                               // Note you need a semicolon in the following situation:
                               //
                               //  let x =           
                               //        stmt
                               //       -expr     <-- not allowed, as prefix token is here considered infix
                               //
                               // i.e.
                               //
                               //  let x =           
                               //        stmt;
                               //        -expr     
                                (if isInfix token then infixTokenLength token + 1 else 0)
                        (tokenStartCol + grace < offsidePos.Column)) -> 
               if debug then dprintf "offside token at column %d indicates end of CtxtSeqBlock started at %a!\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               if debug then (match addBlockEnd with AddBlockEnd -> dprintf "end of CtxtSeqBlock, insert OBLOCKEND \n" | _ -> ()) ;
               match addBlockEnd with 
               | AddBlockEnd -> insertToken OBLOCKEND
               | AddOneSidedBlockEnd -> insertToken ORIGHT_BLOCK_END
               | NoAddBlockEnd -> reprocess()

            //  Offside rule for SeqBlock.
            //    fff
            //       eeeee
            //  <tok>
            | _, (CtxtVanilla(offsidePos,_) :: _) when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "offside token at column %d indicates end of CtxtVanilla started at %a!\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            //  Offside rule for SeqBlock - special case
            //  [< ... >]
            //  decl

            | _, (CtxtSeqBlock(NotFirstInSeqBlock,offsidePos,addBlockEnd) :: _) 
                     when (match token with GREATER_RBRACK -> true | _ -> false) -> 
               // Attribute-end tokens mean CtxtSeqBlock rule is NOT applied to the next token
               replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock,offsidePos,addBlockEnd));
               reprocessWithoutBlockRule()

            //  Offside rule for SeqBlock - avoiding inserting OBLOCKSEP on first item in block
            | _, (CtxtSeqBlock (FirstInSeqBlock,offsidePos,addBlockEnd) :: _) when useBlockRule -> 
               // This is the first token in a block, or a token immediately 
               // following an infix operator (see above). 
               // Return the token, but only after processing any additional rules 
               // applicable for this token.  Don't apply the CtxtSeqBlock rule for 
               // this token, but do apply it on subsequent tokens. 
               if debug then dprintf "repull for CtxtSeqBlockStart\n" ;
               replaceCtxt tokenTup (CtxtSeqBlock (NotFirstInSeqBlock,offsidePos,addBlockEnd));
               reprocessWithoutBlockRule()

            //  Offside rule for SeqBlock - inserting OBLOCKSEP on subsequent items in a block when they are precisely aligned
            //
            // let f1 () = 
            //    expr
            //    ...
            // ~~> insert OBLOCKSEP
            //
            // let f1 () = 
            //    let x = expr
            //    ...
            // ~~> insert OBLOCKSEP
            //
            // let f1 () = 
            //    let x1 = expr
            //    let x2 = expr
            //    let x3 = expr
            //    ...
            // ~~> insert OBLOCKSEP
            | _, (CtxtSeqBlock (NotFirstInSeqBlock,offsidePos,addBlockEnd) :: rest) 
                   when  useBlockRule 
                      && not (let isTypeCtxt = (match rest with | (CtxtTypeDefns _ :: _) -> true | _ -> false)
                              // Don't insert 'OBLOCKSEP' between namespace declarations
                              let isNamespaceCtxt = (match rest with | (CtxtNamespaceBody _ :: _) -> true | _ -> false)
                              if isNamespaceCtxt then (match token with NAMESPACE -> true | _ -> false)
                              elif isTypeCtxt then isTypeSeqBlockElementContinuator token
                              else isSeqBlockElementContinuator  token)
                      && (tokenStartCol = offsidePos.Column) 
                      && (tokenStartPos.OriginalLine <> offsidePos.OriginalLine) -> 
                 if debug then dprintf "offside at column %d matches start of block(%a)! delaying token, returning OBLOCKSEP\n" tokenStartCol outputPos offsidePos;
                 replaceCtxt tokenTup (CtxtSeqBlock (FirstInSeqBlock,offsidePos,addBlockEnd));
                 // No change to offside stack: another statement block starts...
                 insertTokenFromPrevPosToCurrentPos OBLOCKSEP

            //  Offside rule for CtxtLetDecl 
            // let .... = 
            //    ...
            // <and>
            //
            // let .... = 
            //    ...
            // <in>
            //
            //   let .... =
            //       ...
            //  <*>
            | _, (CtxtLetDecl (true,offsidePos) :: _) when 
                          isSemiSemi || (if isLetContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from LET(offsidePos=%a)! delaying token, returning ODECLEND\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               insertToken ODECLEND

            | _, (CtxtDo offsidePos :: _) 
                   when isSemiSemi || (if isDoContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from DO(offsidePos=%a)! delaying token, returning ODECLEND\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               insertToken ODECLEND

            // class
            //    interface AAA
            //  ...
            // ...

            | _, (CtxtInterfaceHead offsidePos :: _) 
                   when isSemiSemi || (if isInterfaceContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from INTERFACE(offsidePos=%a)! pop and reprocess\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            | _, (CtxtTypeDefns offsidePos :: _) 
                   when (isSemiSemi || 
                         ((if isTypeContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column)) -> 
               if debug then dprintf "token at column %d is offside from TYPE(offsidePos=%a)! pop and reprocess\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            // module A.B.M  
            //    ...
            // module M = ...
            // end
            //  module M = ...
            // ...
            // NOTE: ;; does not terminate a whole file module body. 
            | _, ((CtxtModuleBody (offsidePos,wholeFile)) :: _) when (isSemiSemi && not wholeFile) ||  tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from MODULE with offsidePos %a! delaying token\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            // NOTE: ;; does not terminate a 'namespace' body. 
            | _, ((CtxtNamespaceBody offsidePos) :: _) when (* isSemiSemi || *) (if isNamespaceContinuator token then tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from NAMESPACE with offsidePos %a! delaying token\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            | _, ((CtxtException offsidePos) :: _) when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from EXCEPTION with offsidePos %a! delaying token\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            // Pop CtxtMemberBody when offside.  Insert an ODECLEND to indicate the end of the member 
            | _, ((CtxtMemberBody(offsidePos)) :: _) when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from MEMBER/OVERRIDE head with offsidePos %a!\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               insertToken ODECLEND

            // Pop CtxtMemberHead when offside 
            | _, ((CtxtMemberHead(offsidePos)) :: _) when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "token at column %d is offside from MEMBER/OVERRIDE head with offsidePos %a!\n" tokenStartCol outputPos offsidePos;
               popCtxt();
               reprocess()

            | _, (CtxtIf offsidePos :: _) 
                       when isSemiSemi || (if isIfBlockContinuator token then  tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtIf\n";
               popCtxt();
               reprocess()
                
            | _, (CtxtWithAsLet offsidePos :: _) 
                       when isSemiSemi || (if isLetContinuator token then  tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtWithAsLet\n";
               popCtxt();
               insertToken OEND
                
            | _, (CtxtWithAsAugment(offsidePos) :: _) 
                       when isSemiSemi || (if isWithAugmentBlockContinuator token then tokenStartCol + 1  else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtWithAsAugment, isWithAugmentBlockContinuator = %b\n" (isWithAugmentBlockContinuator token);
               popCtxt();
               insertToken ODECLEND 
                
            | _, (CtxtMatch offsidePos :: _) 
                       when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtMatch\n";
               popCtxt();
               reprocess()
                
            | _, (CtxtFor offsidePos :: _) 
                       when isSemiSemi || (if isForLoopContinuator token then  tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtFor\n";
               popCtxt();
               reprocess()
                
            | _, (CtxtWhile offsidePos :: _) 
                       when isSemiSemi || (if isWhileBlockContinuator token then  tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtWhile\n";
               popCtxt();
               reprocess()
                
            | _, (CtxtWhen offsidePos :: _) 
                       when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtWhen\n";
               popCtxt();
               reprocess()
                
            | _, (CtxtFun offsidePos :: _) 
                       when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtFun\n";
               popCtxt();
               insertToken OEND
                
            | _, (CtxtFunction offsidePos :: _) 
                       when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               popCtxt();
               reprocess()
                
            | _, (CtxtTry offsidePos :: _) 
                       when isSemiSemi || (if isTryBlockContinuator token then  tokenStartCol + 1 else tokenStartCol) <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtTry\n";
               popCtxt();
               reprocess()
                
            //  then 
            //     ...
            //  else  
            //
            //  then 
            //     ...
            
            | _, (CtxtThen offsidePos :: _) when isSemiSemi ||  (if isThenBlockContinuator token then  tokenStartCol + 1 else tokenStartCol)<= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtThen, popping\n";
               popCtxt();
               reprocess()
                
            //  else ...
            // ....
            //
            | _, (CtxtElse (offsidePos) :: _) when isSemiSemi || tokenStartCol <= offsidePos.Column -> 
               if debug then dprintf "offside from CtxtElse, popping\n";
               popCtxt();
               reprocess()

            // leadingBar=false permits match patterns without an initial '|' 
            | _, (CtxtMatchClauses (leadingBar,offsidePos) :: _) 
                      when (isSemiSemi || 
                            (match token with 
                             // BAR occurs in pattern matching 'with' blocks 
                             | BAR -> 
                                 let cond1 = tokenStartCol + (if leadingBar then 0 else 2)  < offsidePos.Column
                                 let cond2 = tokenStartCol + (if leadingBar then 1 else 2)  < offsidePos.Column
                                 if (cond1 <> cond2) then 
                                     errorR(Lexhelp.IndentationProblem(FSComp.SR.lexfltSeparatorTokensOfPatternMatchMisaligned(),mkSynRange (startPosOfTokenTup tokenTup) tokenTup.LexbufState.EndPos))
                                 cond1
                             | END -> tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column
                             | _   -> tokenStartCol + (if leadingBar then -1 else 1) < offsidePos.Column)) -> 
                if debug then dprintf "offside from WITH, tokenStartCol = %d, offsidePos = %a, delaying token, returning OEND\n" tokenStartCol outputPos offsidePos;
                popCtxt();
                insertToken OEND
                

            //  namespace ... ~~~> CtxtNamespaceHead 
            |  NAMESPACE,(_ :: _) -> 
                if debug then dprintf "NAMESPACE: entering CtxtNamespaceHead, awaiting end of long identifier to push CtxtSeqBlock\n" ;
                pushCtxt tokenTup (CtxtNamespaceHead (tokenStartPos, token));
                returnToken tokenLexbufState token
                
            //  module ... ~~~> CtxtModuleHead 
            |  MODULE,(_ :: _) -> 
                if debug then dprintf "MODULE: entering CtxtModuleHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtModuleHead (tokenStartPos, token));
                returnToken tokenLexbufState token
                
            // exception ... ~~~> CtxtException 
            |  EXCEPTION,(_ :: _) -> 
                if debug then dprintf "EXCEPTION: entering CtxtException(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtException tokenStartPos);
                returnToken tokenLexbufState token
                
            //  let ... ~~~> CtxtLetDecl 
            //     -- this rule only applies to 
            //              - 'static let' 
            | LET(isUse), (ctxt :: _) when (match ctxt with CtxtMemberHead _ -> true | _ -> false) -> 
                if debug then dprintf "LET: entering CtxtLetDecl(), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" outputPos tokenStartPos;
                let startPos = match ctxt with CtxtMemberHead startPos -> startPos | _ -> tokenStartPos
                popCtxt(); // get rid of the CtxtMemberHead
                pushCtxt tokenTup (CtxtLetDecl(true,startPos));
                returnToken tokenLexbufState (OLET(isUse))

            // let ... ~~~> CtxtLetDecl 
            //     -- this rule only applies to 
            //              - 'let' 'right-on' a SeqBlock line 
            | LET(isUse), (ctxt :: _) -> 
                let blockLet = match ctxt with CtxtSeqBlock _ -> true | _ -> false
                if debug then dprintf "LET: entering CtxtLetDecl(blockLet=%b), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" blockLet outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtLetDecl(blockLet,tokenStartPos));
                returnToken tokenLexbufState (if blockLet then OLET(isUse) else token)
                
            //  let!  ... ~~~> CtxtLetDecl 
            | BINDER b, (ctxt :: _) -> 
                let blockLet = match ctxt with CtxtSeqBlock _ -> true | _ -> false
                if debug then dprintf "LET: entering CtxtLetDecl(blockLet=%b), awaiting EQUALS to go to CtxtSeqBlock (%a)\n" blockLet outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtLetDecl(blockLet,tokenStartPos));
                returnToken tokenLexbufState (if blockLet then OBINDER b else token)
                
            //  static member ... ~~~> CtxtMemberHead 
            //  static ... ~~~> CtxtMemberHead 
            //  member ... ~~~> CtxtMemberHead 
            //  override ... ~~~> CtxtMemberHead 
            //  default ... ~~~> CtxtMemberHead 
            |  (STATIC | ABSTRACT | MEMBER | OVERRIDE | DEFAULT),(ctxt :: _) when (match ctxt with CtxtMemberHead _ -> false | _ -> true) -> 
                if debug then dprintf "STATIC/MEMBER/OVERRIDE/DEFAULT: entering CtxtMemberHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtMemberHead(tokenStartPos));
                returnToken tokenLexbufState token

            //  public new... ~~~> CtxtMemberHead 
            |  (PUBLIC | PRIVATE | INTERNAL),(_ctxt :: _) when (match peekNextToken() with NEW -> true | _ -> false) -> 
                if debug then dprintf "PUBLIC/PRIVATE/INTERNAL NEW: entering CtxtMemberHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtMemberHead(tokenStartPos));
                returnToken tokenLexbufState token

            //  new( ~~~> CtxtMemberHead, if not already there because of 'public' 
            | NEW, ctxt :: _  when (match peekNextToken() with LPAREN -> true | _ -> false) &&  (match ctxt with CtxtMemberHead _ -> false | _ -> true)   -> 
                if debug then dprintf "NEW: entering CtxtMemberHead, awaiting EQUALS to go to CtxtSeqBlock (%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtMemberHead(tokenStartPos));
                returnToken tokenLexbufState token
                                     
            //  'let ... = ' ~~~> CtxtSeqBlock 
            | EQUALS, (CtxtLetDecl _ :: _) ->  
                if debug then dprintf "CtxtLetDecl: EQUALS, pushing CtxtSeqBlock\n";
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token

            | EQUALS, (CtxtTypeDefns _ :: _) ->  
                if debug then dprintf "CtxType: EQUALS, pushing CtxtSeqBlock\n";
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token

            | (LAZY | ASSERT), _ ->  
                if isControlFlowOrNotSameLine() then 
                    if debug then dprintf "LAZY/ASSERT, pushing CtxtSeqBlock\n";
                    pushCtxtSeqBlock(true,AddBlockEnd);
                    returnToken tokenLexbufState (match token with LAZY -> OLAZY | _  -> OASSERT)
                else
                    returnToken tokenLexbufState token

            //  'with id = ' ~~~> CtxtSeqBlock 
            //  'with M.id = ' ~~~> CtxtSeqBlock 
            //  'with id1 = 1
            //        id2 = ...  ~~~> CtxtSeqBlock 
            //  'with id1 = 1
            //        M.id2 = ...  ~~~> CtxtSeqBlock 
            //  '{ id = ... ' ~~~> CtxtSeqBlock 
            //  '{ M.id = ... ' ~~~> CtxtSeqBlock 
            //  '{ id1 = 1
            //     id2 = ... ' ~~~> CtxtSeqBlock 
            //  '{ id1 = 1
            //     M.id2 = ... ' ~~~> CtxtSeqBlock 
            | EQUALS, ((CtxtWithAsLet _) :: _)  // This detects 'with = '. 
            | EQUALS, ((CtxtVanilla (_,true)) :: (CtxtSeqBlock _) :: (CtxtWithAsLet _ | CtxtParen(LBRACE,_)) :: _) ->  
                if debug then dprintf "CtxtLetDecl/CtxtWithAsLet: EQUALS, pushing CtxtSeqBlock\n";
                // We don't insert begin/end block tokens for single-line bindings since we can't properly distinguish single-line *)
                // record update expressions such as "{ t with gbuckets=Array.copy t.gbuckets; gcount=t.gcount }" *)
                // These have a syntactically odd status because of the use of ";" to terminate expressions, so each *)
                // "=" binding is not properly balanced by "in" or "and" tokens in the single line syntax (unlike other bindings) *)
                if isControlFlowOrNotSameLine() then 
                    pushCtxtSeqBlock(true,AddBlockEnd);
                else
                    pushCtxtSeqBlock(false,NoAddBlockEnd);
                returnToken tokenLexbufState token


            //  'new(... =' ~~~> CtxtMemberBody, CtxtSeqBlock 
            //  'member ... =' ~~~> CtxtMemberBody, CtxtSeqBlock 
            //  'static member ... =' ~~~> CtxtMemberBody, CtxtSeqBlock 
            //  'default ... =' ~~~> CtxtMemberBody, CtxtSeqBlock 
            //  'override ... =' ~~~> CtxtMemberBody, CtxtSeqBlock 
            | EQUALS, ((CtxtMemberHead(offsidePos)) :: _) ->  
                if debug then dprintf "CtxtMemberHead: EQUALS, pushing CtxtSeqBlock\n";
                replaceCtxt tokenTup (CtxtMemberBody (offsidePos));
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token

            // '(' tokens are balanced with ')' tokens and also introduce a CtxtSeqBlock 
            | (BEGIN | LPAREN | SIG | LBRACE | LBRACK | LBRACK_BAR | LQUOTE _ | LESS true), _ ->                      
                if debug then dprintf "LPAREN etc., pushes CtxtParen, pushing CtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtParen (token,tokenStartPos));
                pushCtxtSeqBlock(false,NoAddBlockEnd);
                returnToken tokenLexbufState token

            // '(' tokens are balanced with ')' tokens and also introduce a CtxtSeqBlock 
            | STRUCT, ctxts                       
                   when (match ctxts with 
                         | CtxtSeqBlock _ :: (CtxtModuleBody _ | CtxtTypeDefns _) :: _ -> 
                                // type ... = struct ... end 
                                // module ... = struct ... end 
                            true 
                             
                         | _ -> false) (* type X<'a when 'a : struct> *) ->
                if debug then dprintf "LPAREN etc., pushes CtxtParen, pushing CtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtParen (token,tokenStartPos));
                pushCtxtSeqBlock(false,NoAddBlockEnd);
                returnToken tokenLexbufState token

            | RARROW, ctxts 
                   // Only treat '->' as a starting a sequence block in certain circumstances 
                   when (match ctxts with 
                            // comprehension/match 
                            | (CtxtWhile _ | CtxtFor _ | CtxtWhen _ | CtxtMatchClauses _ | CtxtFun _) :: _ -> true 
                            // comprehension 
                            | (CtxtSeqBlock _ :: CtxtParen ((LBRACK | LBRACE | LBRACK_BAR), _) :: _) -> true  
                            // comprehension 
                            | (CtxtSeqBlock _ :: (CtxtDo _ | CtxtWhile _ | CtxtFor _ | CtxtWhen _ | CtxtMatchClauses _  | CtxtTry _ | CtxtThen _ | CtxtElse _) :: _) -> true 
                            | _ -> false) ->
                if debug then dprintf "RARROW, pushing CtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxtSeqBlock(false,AddOneSidedBlockEnd);
                returnToken tokenLexbufState token

            | LARROW, _  when isControlFlowOrNotSameLine() ->
                if debug then dprintf "LARROW, pushing CtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token

            //  do  ~~> CtxtDo;CtxtSeqBlock  (unconditionally) 
            | (DO | DO_BANG), _ -> 
                if debug then dprintf "DO: pushing CtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtDo (tokenStartPos));
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState (match token with DO -> ODO | DO_BANG -> ODO_BANG | _ -> failwith "unreachable")

            // The r.h.s. of an infix token begins a new block. 
            | _, ctxts when (isInfix token && 
                             not (isSameLine()) && 
                             // This doesn't apply to the use of any infix tokens in a pattern match or 'when' block
                             // For example
                             //
                             //       match x with
                             //       | _ when true &&
                             //                false ->   // the 'false' token shouldn't start a new block
                             //                let x = 1 
                             //                x
                             (match ctxts with CtxtMatchClauses _ :: _ -> false | _ -> true)) -> 

                if debug then dprintf "(Infix etc.), pushing CtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxtSeqBlock(false,NoAddBlockEnd);
                returnToken tokenLexbufState token


            | WITH, ((CtxtTry _ | CtxtMatch _) :: _)  -> 
                let lookaheadTokenTup = peekNextTokenTup()
                let lookaheadTokenStartPos = startPosOfTokenTup lookaheadTokenTup
                let leadingBar = match (peekNextToken()) with BAR -> true | _ -> false
                if debug then dprintf "WITH, pushing CtxtMatchClauses, lookaheadTokenStartPos = %a, tokenStartPos = %a\n" outputPos lookaheadTokenStartPos outputPos tokenStartPos;
                pushCtxt lookaheadTokenTup (CtxtMatchClauses(leadingBar,lookaheadTokenStartPos));
                returnToken tokenLexbufState OWITH 

            | FINALLY, (CtxtTry _ :: _)  -> 
                if debug then dprintf "FINALLY, pushing pushCtxtSeqBlock, tokenStartPos = %a\n" outputPos tokenStartPos;
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token

            | WITH, (((CtxtException _ | CtxtTypeDefns _ | CtxtMemberHead _ | CtxtInterfaceHead _) as limCtxt) :: _) 
            | WITH, ((CtxtSeqBlock _) as limCtxt :: CtxtParen(LBRACE,_) :: _)  -> 
                let lookaheadTokenTup = peekNextTokenTup()
                let lookaheadTokenStartPos = startPosOfTokenTup lookaheadTokenTup
                begin match lookaheadTokenTup.Token with 
                | IDENT _ 
                // The next clause detects the access annotations after the 'with' in:
                //    member  x.PublicGetSetProperty 
                //                 with public get i = "Ralf"
                //                 and  private set i v = ()  
                //    
                //    as well as:                      
                //    member  x.PublicGetSetProperty 
                //                 with inline get() = "Ralf"
                //                 and  [<Foo>] set(v) = ()  
                //    
                | PUBLIC | PRIVATE | INTERNAL | INLINE -> 

                    let offsidePos = 
                       if lookaheadTokenStartPos.Column > tokenTup.LexbufState.EndPos.Column  then
                            // This detects:
                            //    { new Foo 
                            //      with M() = 1
                            //      and  N() = 2 } 
                            // and treats the inner bindings as if they were member bindings. 
                            // (HOWEVER: note the above language construct is now deprecated/removed)
                            // 
                            // It also happens to detect
                            //    { foo with m = 1;
                            //               n = 2 }
                            // So we're careful to set the offside column to be the minimum required *)
                          tokenStartPos
                        else
                            // This detects:
                            //    { foo with 
                            //        m = 1;
                            //        n = 2 }
                            // So we're careful to set the offside column to be the minimum required *)
                          limCtxt.StartPos
                    if debug then dprintf "WITH, pushing CtxtWithAsLet, tokenStartPos = %a, lookaheadTokenStartPos = %a\n" outputPos tokenStartPos outputPos lookaheadTokenStartPos;
                    pushCtxt tokenTup (CtxtWithAsLet(offsidePos));
                    
                    
                    // Detect 'with' bindings of the form 
                    //
                    //    with x = ...
                    //
                    // Which can only be part of 
                    //
                    //   { r with x = ... }
                    //
                    // and in this case push a CtxtSeqBlock to cover the sequence
                    let isFollowedByLongIdentEquals = 
                        let tokenTup = popNextTokenTup()
                        let res = isLongIdentEquals tokenTup.Token
                        delayToken tokenTup
                        res

                    if isFollowedByLongIdentEquals then
                        pushCtxtSeqBlock(false,NoAddBlockEnd);
                      
                    returnToken tokenLexbufState OWITH 
                | _ -> 
                    if debug then dprintf "WITH, pushing CtxtWithAsAugment and CtxtSeqBlock, tokenStartPos = %a, limCtxt = %A\n" outputPos tokenStartPos limCtxt;
                    //
                    //  For attributes on properties:
                    //      member  x.PublicGetSetProperty 
                    //         with [<Foo>]  get() = "Ralf"
                    if (match lookaheadTokenTup.Token with LBRACK_LESS -> true | _ -> false)  && (lookaheadTokenStartPos.OriginalLine = tokenTup.StartPos.OriginalLine) then
                        let offsidePos = tokenStartPos
                        pushCtxt tokenTup (CtxtWithAsLet(offsidePos));
                        returnToken tokenLexbufState OWITH 
                    else
                        // In these situations
                        //    interface I with 
                        //        ...
                        //    end
                        //    exception ... with 
                        //        ...
                        //    end
                        //    type ... with 
                        //        ...
                        //    end
                        //    member x.P 
                        //       with get() = ...
                        //       and  set() = ...
                        //    member x.P with 
                        //        get() = ...
                        // The limit is "interface"/"exception"/"type" 
                        let offsidePos = limCtxt.StartPos
                           
                        pushCtxt tokenTup (CtxtWithAsAugment(offsidePos));
                        pushCtxtSeqBlock(true,AddBlockEnd);
                        returnToken tokenLexbufState token 
                end;

            | WITH, stack  -> 
                if debug then dprintf "WITH\n";
                if debug then dprintf "WITH --> NO MATCH, pushing CtxtWithAsAugment (type augmentation), stack = %A" stack;
                pushCtxt tokenTup (CtxtWithAsAugment(tokenStartPos));
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token 

            | FUNCTION, _  -> 
                let lookaheadTokenTup = peekNextTokenTup()
                let lookaheadTokenStartPos = startPosOfTokenTup lookaheadTokenTup
                let leadingBar = match (peekNextToken()) with BAR -> true | _ -> false
                pushCtxt tokenTup (CtxtFunction(tokenStartPos));
                pushCtxt lookaheadTokenTup (CtxtMatchClauses(leadingBar,lookaheadTokenStartPos));
                returnToken tokenLexbufState OFUNCTION

            | THEN,_  -> 
                if debug then dprintf "THEN, replacing THEN with OTHEN, pushing CtxtSeqBlock;CtxtThen(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtThen(tokenStartPos));
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState OTHEN 

            | ELSE, _   -> 
                let lookaheadTokenTup = peekNextTokenTup()
                let lookaheadTokenStartPos = startPosOfTokenTup lookaheadTokenTup
                begin match peekNextToken() with 
                | IF when isSameLine() ->
                  // We convert ELSE IF to ELIF since it then opens the block at the right point, 
                  // In particular the case
                  //    if e1 then e2
                  //    else if e3 then e4
                  //    else if e5 then e6 
                  let _ = popNextTokenTup()
                  if debug then dprintf "ELSE IF: replacing ELSE IF with ELIF, pushing CtxtIf, CtxtVanilla(%a)\n" outputPos tokenStartPos;
                  pushCtxt tokenTup (CtxtIf(tokenStartPos));
                  returnToken tokenLexbufState ELIF
                  
                | _ -> 
                  if debug then dprintf "ELSE: replacing ELSE with OELSE, pushing CtxtSeqBlock, CtxtElse(%a)\n" outputPos lookaheadTokenStartPos;
                  pushCtxt tokenTup (CtxtElse(tokenStartPos));
                  pushCtxtSeqBlock(true,AddBlockEnd);
                  returnToken tokenLexbufState OELSE
                end

            | (ELIF | IF), _   -> 
                if debug then dprintf "IF, pushing CtxtIf(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtIf (tokenStartPos));
                returnToken tokenLexbufState token

            | MATCH, _   -> 
                if debug then dprintf "MATCH, pushing CtxtMatch(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtMatch (tokenStartPos));
                returnToken tokenLexbufState token

            | FOR, _   -> 
                if debug then dprintf "FOR, pushing CtxtFor(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtFor (tokenStartPos));
                returnToken tokenLexbufState token

            | WHILE, _   -> 
                if debug then dprintf "WHILE, pushing CtxtWhile(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtWhile (tokenStartPos));
                returnToken tokenLexbufState token

            | WHEN, ((CtxtSeqBlock _) :: _)  -> 
                if debug then dprintf "WHEN, pushing CtxtWhen(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtWhen (tokenStartPos));
                returnToken tokenLexbufState token

            | FUN, _   -> 
                if debug then dprintf "FUN, pushing CtxtFun(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtFun (tokenStartPos));
                returnToken tokenLexbufState OFUN

            | INTERFACE, _  -> 
                let lookaheadTokenTup = peekNextTokenTup()
                let lookaheadTokenStartPos = startPosOfTokenTup lookaheadTokenTup
                begin match lookaheadTokenTup.Token with 
                // type I = interface .... end 
                | DEFAULT | OVERRIDE | INTERFACE | NEW | TYPE | STATIC | END | MEMBER | ABSTRACT  | INHERIT | LBRACK_LESS -> 
                    if debug then dprintf "INTERFACE, pushing CtxtParen, tokenStartPos = %a, lookaheadTokenStartPos = %a\n" outputPos tokenStartPos outputPos lookaheadTokenStartPos;
                    pushCtxt tokenTup (CtxtParen (token,tokenStartPos));
                    pushCtxtSeqBlock(true,AddBlockEnd);
                    returnToken tokenLexbufState token
                // type C with interface .... with 
                // type C = interface .... with 
                | _ -> 
                    if debug then dprintf "INTERFACE, pushing CtxtInterfaceHead, tokenStartPos = %a, lookaheadTokenStartPos = %a\n" outputPos tokenStartPos outputPos lookaheadTokenStartPos;
                    pushCtxt tokenTup (CtxtInterfaceHead(tokenStartPos));
                    returnToken tokenLexbufState OINTERFACE_MEMBER
                end;

            | CLASS, _   -> 
                if debug then dprintf "CLASS, pushing CtxtParen(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtParen (token,tokenStartPos));
                pushCtxtSeqBlock(true,AddBlockEnd);
                returnToken tokenLexbufState token

            | TYPE, _   -> 
                if debug then dprintf "TYPE, pushing CtxtTypeDefns(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtTypeDefns(tokenStartPos));
                returnToken tokenLexbufState token

            | TRY, _   -> 
                if debug then dprintf "Try, pushing CtxtTry(%a)\n" outputPos tokenStartPos;
                pushCtxt tokenTup (CtxtTry (tokenStartPos));
                // The ideal spec would be to push a begin/end block pair here, but we can only do that 
                // if we are able to balance the WITH with the TRY.  We can't do that because of the numerous ways 
                // WITH is used in the grammar (see what happens when we hit a WITH below. 
                // This hits in the single line case: "try make ef1 t with _ -> make ef2 t". 
                
                pushCtxtSeqBlock(false,AddOneSidedBlockEnd);
                returnToken tokenLexbufState token

            |  OBLOCKBEGIN,_ -> 
              returnToken tokenLexbufState token  
                
            |  ODUMMY(_),_ -> 
              if debug then dprintf "skipping dummy token as no offside rules apply\n";
              hwTokenFetch (useBlockRule) 
                
            // Ordinary tokens start a vanilla block 
            |  _,CtxtSeqBlock _ :: _ -> 
                pushCtxt tokenTup (CtxtVanilla(tokenStartPos, isLongIdentEquals token));
                if debug then dprintf "pushing CtxtVanilla at tokenStartPos = %a\n" outputPos tokenStartPos;
                returnToken tokenLexbufState token  
                
            |  _ -> 
              returnToken tokenLexbufState token  

     and rulesForBothSoftWhiteAndHardWhite(tokenTup:TokenTup) = 
          match tokenTup.Token with 
          // Insert HIGH_PRECEDENCE_PAREN_APP if needed 
          |  IDENT _ when (nextTokenIsAdjacentLParenOrLBrack tokenTup).IsSome ->
              let dotTokenTup = peekNextTokenTup()
              if debug then dprintf "inserting HIGH_PRECEDENCE_PAREN_APP at dotTokenPos = %a\n" outputPos (startPosOfTokenTup dotTokenTup);
              let hpa = 
                  match nextTokenIsAdjacentLParenOrLBrack tokenTup with 
                  | Some(LPAREN) -> HIGH_PRECEDENCE_PAREN_APP
                  | Some(LBRACK) -> HIGH_PRECEDENCE_BRACK_APP
                  | _ -> failwith "unreachable"
              delayToken(dotTokenTup.UseLocation(hpa));
              delayToken(tokenTup);
              true

          // Insert HIGH_PRECEDENCE_TYAPP if needed 
          |  (DELEGATE | IDENT _ | IEEE64 _ | IEEE32 _ | DECIMAL _ | INT8 _ | INT16 _ | INT32 _ | INT64 _ | NATIVEINT _ | UINT8 _ | UINT16 _ | UINT32 _ | UINT64 _ | BIGNUM _) when peekAdjacentTypars false tokenTup ->
              let lessTokenTup = popNextTokenTup()
              delayToken (lessTokenTup.UseLocation(match lessTokenTup.Token with LESS _ -> LESS true | _ -> failwith "unreachable")); 

              if debug then dprintf "softwhite inserting HIGH_PRECEDENCE_TYAPP at dotTokenPos = %a\n" outputPos (startPosOfTokenTup lessTokenTup);

              //  IMPLEMENTATION FOR DESIGN CHANGE 1600 IF REQUIRED
              //            | INFIX_COMPARE_OP "<>" ->
              //                delayToken(LESS,tokenLexbufState,tokenPrevEndPos);
              //                delayToken(GREATER,tokenLexbufState,tokenPrevEndPos);
              //                delayToken(tokenTup);

              delayToken (lessTokenTup.UseLocation(HIGH_PRECEDENCE_TYAPP));
              delayToken (tokenTup);
              true

          // Split this token to allow "1..2" for range specification 
          |  INT32_DOT_DOT (i,v) ->
              let dotdotPos = new LexbufState(tokenTup.EndPos.ShiftColumnBy(-2), tokenTup.EndPos, false);
              delayToken(new TokenTup(DOT_DOT, dotdotPos, tokenTup.LastTokenPos));
              delayToken(tokenTup.UseShiftedLocation(INT32(i,v), 0, -2));
              true
          // Split @>. and @@>. into two 
          |  RQUOTE_DOT (s,raw) ->
              let dotPos = new LexbufState(tokenTup.EndPos.ShiftColumnBy(-1), tokenTup.EndPos, false);
              delayToken(new TokenTup(DOT, dotPos, tokenTup.LastTokenPos));
              delayToken(tokenTup.UseShiftedLocation(RQUOTE(s,raw), 0, -1));
              true

          |  MINUS | PLUS_MINUS_OP _ | PERCENT_OP _  | AMP | AMP_AMP
                when ((match tokenTup.Token with 
                       | PLUS_MINUS_OP s -> (s = "+") || (s = "+.")  || (s = "-.") 
                       | PERCENT_OP s -> (s = "%") || (s = "%%") 
                       | _ -> true) &&
                      nextTokenIsAdjacent tokenTup && 
                      not (prevWasAtomicEnd && (snd(tokenTup.LastTokenPos) = startPosOfTokenTup tokenTup))) ->

              let plus = 
                  match tokenTup.Token with 
                  | PLUS_MINUS_OP s -> (s = "+") 
                  | _ -> false
              let plusOrMinus = 
                  match tokenTup.Token with 
                  | PLUS_MINUS_OP s -> (s = "+") 
                  | MINUS -> true 
                  | _ -> false
              let nextTokenTup = popNextTokenTup()
              begin 
                /// Merge the location of the prefix token and the literal
                let delayMergedToken tok = delayToken(new TokenTup(tok,new LexbufState(tokenTup.LexbufState.StartPos,nextTokenTup.LexbufState.EndPos,nextTokenTup.LexbufState.PastEOF),tokenTup.LastTokenPos))
                let noMerge() = 
                    let tokenName = 
                        match tokenTup.Token with 
                        | PLUS_MINUS_OP s
                        | PERCENT_OP s -> s
                        | AMP -> "&"
                        | AMP_AMP -> "&&"
                        | MINUS -> "-" 
                        | _ -> failwith "unreachable" 
                    let token = ADJACENT_PREFIX_OP tokenName
                    delayToken nextTokenTup; 
                    delayToken (tokenTup.UseLocation(token));

                if plusOrMinus then 
                    match nextTokenTup.Token with 
                    | INT8(v,bad)          -> delayMergedToken(INT8((if plus then v else -v),(plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
                    | INT16(v,bad)         -> delayMergedToken(INT16((if plus then v else -v),(plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
                    | INT32(v,bad)         -> delayMergedToken(INT32((if plus then v else -v),(plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
                    | INT32_DOT_DOT(v,bad) -> delayMergedToken(INT32_DOT_DOT((if plus then v else -v),(plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
                    | INT64(v,bad)         -> delayMergedToken(INT64((if plus then v else -v),(plus && bad))) // note: '-' makes a 'bad' max int 'good'. '+' does not
                    | NATIVEINT(v)         -> delayMergedToken(NATIVEINT(if plus then v else -v))
                    | IEEE32(v)            -> delayMergedToken(IEEE32(if plus then v else -v))
                    | IEEE64(v)            -> delayMergedToken(IEEE64(if plus then v else -v))
                    | DECIMAL(v)           -> delayMergedToken(DECIMAL(if plus then v else System.Decimal.op_UnaryNegation v))
                    | BIGNUM(v,s)          -> delayMergedToken(BIGNUM((if plus then v else "-"^v),s))
                    | _ -> noMerge()
                else
                    noMerge()
              end;
              true

          | _ -> 
              false
  
     and pushCtxtSeqBlock(addBlockBegin,addBlockEnd) = pushCtxtSeqBlockAt (peekNextTokenTup(),addBlockBegin,addBlockEnd) 
     and pushCtxtSeqBlockAt(p:TokenTup,addBlockBegin,addBlockEnd) = 
         if addBlockBegin then (
           if debug then dprintf "--> insert OBLOCKBEGIN \n" ;
           delayToken(p.UseLocation(OBLOCKBEGIN))
         );
         pushCtxt p (CtxtSeqBlock(FirstInSeqBlock, startPosOfTokenTup p,addBlockEnd)) 

    let rec swTokenFetch() = 
          let tokenTup = popNextTokenTup()
          let tokenReplaced = rulesForBothSoftWhiteAndHardWhite(tokenTup)
          if tokenReplaced then swTokenFetch() 
          else returnToken tokenTup.LexbufState tokenTup.Token

    //----------------------------------------------------------------------------
    // Part VI. Publish the new lexer function.  
    //--------------------------------------------------------------------------

    member __.LexBuffer = lexbuf
    member __.Lexer _ = 
        if not initialized then 
            let _firstTokenTup = peekInitial();
            ()

        if lightSyntaxStatus.Status
        then hwTokenFetch(true)  
        else swTokenFetch()

  
