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

//------------------------------------------------------------------------
// Helper functions for the F# lexer lex.mll
//-----------------------------------------------------------------------


module internal Microsoft.FSharp.Compiler.Lexhelp

open System.Text
open Internal.Utilities
open Internal.Utilities.Collections
open Internal.Utilities.Text
open Internal.Utilities.Text.Lexing
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Parser



// The "mock" filename used by fsi.exe when reading from stdin.
// Has special treatment by the lexer, i.e. __SOURCE_DIRECTORY__ becomes GetCurrentDirectory()
let stdinMockFilename = "stdin" 

/// Lexer args: status of #light processing.  Mutated when a #light
/// directive is processed. This alters the behaviour of the lexfilter.
[<Sealed>]
type LightSyntaxStatus(initial:bool,warn:bool) = 
    let mutable status = None
    member x.Status 
       with get() = match status with None -> initial | Some v -> v
       and  set v = status <- Some(v)
    member x.ExplicitlySet = status.IsSome
    member x.WarnOnMultipleTokens = warn
    

/// Manage lexer resources (string interning)
[<Sealed>]
type LexResourceManager() =
    let strings = new System.Collections.Generic.Dictionary<string,string>(100)
    member x.InternString(s) = 
        let mutable res = "" in
        let ok = strings.TryGetValue(s,&res)  in
        if ok then res 
        else 
            (strings.[s] <- s; s)
              
/// Lexer parameters 
type lexargs =  
    { defines: string list;
      ifdefStack: LexerIfdefStack;
      resourceManager: LexResourceManager;
      getSourceDirectory: (unit -> string); 
      lightSyntaxStatus : LightSyntaxStatus;
      errorLogger: ErrorLogger }

let mkLexargs (srcdir,_filename,defines,lightSyntaxStatus,resourceManager,ifdefStack,errorLogger) =
    { defines = defines;
      ifdefStack= ifdefStack;
      lightSyntaxStatus=lightSyntaxStatus;
      resourceManager=resourceManager;
      getSourceDirectory=srcdir; 
      errorLogger=errorLogger }

/// Register the lexbuf and call the given function
let reusingLexbufForParsing lexbuf f = 
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind (BuildPhase.Parse)
    LexbufLocalXmlDocStore.ClearXmlDoc lexbuf;
    try
      f () 
    with e ->
      raise (WrappedError(e,(try getLexerRange lexbuf with _ -> range0)))

let resetLexbufPos filename (lexbuf: UnicodeLexing.Lexbuf) = 
    lexbuf.EndPos <- {lexbuf.EndPos with posFileIndex= fileIndexOfFile filename; 
                                         posColumnOffset=0;
                                         posOriginalLineNum = 0;
                                         posLineNum=1 }

/// Reset the lexbuf, configure the initial position with the given filename and call the given function
let usingLexbufForParsing (lexbuf:UnicodeLexing.Lexbuf,filename) f =
    resetLexbufPos filename lexbuf;
    reusingLexbufForParsing lexbuf (fun () -> f lexbuf)

//------------------------------------------------------------------------
// Functions to manipulate lexer transient state
//-----------------------------------------------------------------------

let defaultStringFinisher = (fun _endm _b s -> STRING (Encoding.Unicode.GetString s)) 

let callStringFinisher fin (buf: ByteBuffer) endm b = fin endm b (buf.Close())

let addUnicodeString (buf: ByteBuffer) (x:string) = buf.EmitBytes (Encoding.Unicode.GetBytes x)

let addIntChar (buf: ByteBuffer) c = 
    buf.EmitIntAsByte (c % 256);
    buf.EmitIntAsByte (c / 256)

let addUnicodeChar buf c = addIntChar buf (int c)
let addByteChar buf (c:char) = addIntChar buf (int32 c % 256)

/// When lexing bytearrays we don't expect to see any unicode stuff. 
/// Likewise when lexing string constants we shouldn't see any trigraphs > 127 
/// So to turn the bytes collected in the string buffer back into a bytearray 
/// we just take every second byte we stored.  Note all bytes > 127 should have been 
/// stored using addIntChar 
let stringBufferAsBytes (buf: ByteBuffer) = 
    let bytes = buf.Close()
    Array.init (bytes.Length / 2) (fun i -> bytes.[i*2]) 

/// Sanity check that high bytes are zeros. Further check each low byte <= 127 
let stringBufferIsBytes (buf: ByteBuffer) = 
    let bytes = buf.Close()
    let mutable ok = true 
    for i = 0 to bytes.Length / 2-1 do
        if bytes.[i*2+1] <> 0uy then ok <- false
    ok

let newline (lexbuf:LexBuffer<_>) = 
    lexbuf.EndPos <- lexbuf.EndPos.NextLine

let trigraph c1 c2 c3 =
    let digit (c:char) = int c - int '0' 
    char (digit c1 * 100 + digit c2 * 10 + digit c3)

let digit d = 
    if d >= '0' && d <= '9' then int32 d - int32 '0'   
    else failwith "digit" 

let hexdigit d = 
    if d >= '0' && d <= '9' then digit d 
    elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
    elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
    else failwith "hexdigit" 

let unicodeGraphShort (s:string) =
    if s.Length <> 4 then failwith "unicodegraph";
    uint16 (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])

let hexGraphShort (s:string) =
    if s.Length <> 2 then failwith "hexgraph";
    uint16 (hexdigit s.[0] * 16 + hexdigit s.[1])

let unicodeGraphLong (s:string) =
    if s.Length <> 8 then failwith "unicodeGraphLong";
    let high = hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3] in 
    let low = hexdigit s.[4] * 4096 + hexdigit s.[5] * 256 + hexdigit s.[6] * 16 + hexdigit s.[7] in 
    if high = 0 then None, uint16 low 
    else 
      (* A surrogate pair - see http://www.unicode.org/unicode/uni2book/ch03.pdf, section 3.7 *)
      Some (uint16 (0xD800 + ((high * 0x10000 + low - 0x10000) / 0x400))),
      uint16 (0xDF30 + ((high * 0x10000 + low - 0x10000) % 0x400))

let escape c = 
    match c with
    | '\\' -> '\\'
    | '\'' -> '\''
    | 'a' -> char 7
    | 'f' -> char 12
    | 'v' -> char 11
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    | c -> c

//------------------------------------------------------------------------
// Keyword table
//-----------------------------------------------------------------------   

exception ReservedKeyword of string * range
exception IndentationProblem of string * range

module Keywords = 
    type private compatibilityMode =
        | ALWAYS  (* keyword *)
        | FSHARP  (* keyword, but an identifier under --ml-compatibility mode *)

    let private keywordList = 
     [ FSHARP, "abstract", ABSTRACT;
      ALWAYS, "and"        ,AND;
      ALWAYS, "as"         ,AS;
      ALWAYS, "assert"     ,ASSERT;
      ALWAYS, "asr"        ,INFIX_STAR_STAR_OP "asr";
      ALWAYS, "base"       ,BASE;
      ALWAYS, "begin"      ,BEGIN;
      ALWAYS, "class"      ,CLASS;
      FSHARP, "default"    ,DEFAULT;
      FSHARP, "delegate"   ,DELEGATE;
      ALWAYS, "do"         ,DO;
      ALWAYS, "done"       ,DONE;
      FSHARP, "downcast"   ,DOWNCAST;
      ALWAYS, "downto"     ,DOWNTO;
      FSHARP, "elif"       ,ELIF;
      ALWAYS, "else"       ,ELSE;
      ALWAYS, "end"        ,END;
      ALWAYS, "exception"  ,EXCEPTION;
      FSHARP, "extern"     ,EXTERN;
      ALWAYS, "false"      ,FALSE;
      ALWAYS, "finally"    ,FINALLY;
      ALWAYS, "for"        ,FOR;
      ALWAYS, "fun"        ,FUN;
      ALWAYS, "function"   ,FUNCTION;
      FSHARP, "global"     ,GLOBAL;
      ALWAYS, "if"         ,IF;
      ALWAYS, "in"         ,IN;
      ALWAYS, "inherit"    ,INHERIT;
      FSHARP, "inline"     ,INLINE;
      FSHARP, "interface"  ,INTERFACE;
      FSHARP, "internal"   ,INTERNAL;
      ALWAYS, "land"       ,INFIX_STAR_DIV_MOD_OP "land";
      ALWAYS, "lazy"       ,LAZY;
      ALWAYS, "let"        ,LET(false);
      ALWAYS, "lor"        ,INFIX_STAR_DIV_MOD_OP "lor";
      ALWAYS, "lsl"        ,INFIX_STAR_STAR_OP "lsl";
      ALWAYS, "lsr"        ,INFIX_STAR_STAR_OP "lsr";
      ALWAYS, "lxor"       ,INFIX_STAR_DIV_MOD_OP "lxor";
      ALWAYS, "match"      ,MATCH;
      FSHARP, "member"     ,MEMBER;
      ALWAYS, "mod"        ,INFIX_STAR_DIV_MOD_OP "mod";
      ALWAYS, "module"     ,MODULE;
      ALWAYS, "mutable"    ,MUTABLE;
      FSHARP, "namespace"  ,NAMESPACE;
      ALWAYS, "new"        ,NEW;
      FSHARP, "null"       ,NULL;
      ALWAYS, "of"         ,OF;
      ALWAYS, "open"       ,OPEN;
      ALWAYS, "or"         ,OR;
      FSHARP, "override"   ,OVERRIDE;
      ALWAYS, "private"    ,PRIVATE;  
      FSHARP, "public"     ,PUBLIC;
      ALWAYS, "rec"        ,REC;
      FSHARP, "return"      ,YIELD(false);
      ALWAYS, "sig"        ,SIG;
      FSHARP, "static"     ,STATIC;
      ALWAYS, "struct"     ,STRUCT;
      ALWAYS, "then"       ,THEN;
      ALWAYS, "to"         ,TO;
      ALWAYS, "true"       ,TRUE;
      ALWAYS, "try"        ,TRY;
      ALWAYS, "type"       ,TYPE;
      FSHARP, "upcast"     ,UPCAST;
      FSHARP, "use"        ,LET(true);
      ALWAYS, "val"        ,VAL;
      FSHARP, "void"       ,VOID;
      ALWAYS, "when"       ,WHEN;
      ALWAYS, "while"      ,WHILE;
      ALWAYS, "with"       ,WITH;
      FSHARP, "yield"      ,YIELD(true);
      ALWAYS, "_"          ,UNDERSCORE;
    (*------- for prototyping and explaining offside rule *)
      FSHARP, "__token_OBLOCKSEP" ,OBLOCKSEP;
      FSHARP, "__token_OWITH"     ,OWITH;
      FSHARP, "__token_ODECLEND"  ,ODECLEND;
      FSHARP, "__token_OTHEN"     ,OTHEN;
      FSHARP, "__token_OELSE"     ,OELSE;
      FSHARP, "__token_OEND"      ,OEND;
      FSHARP, "__token_ODO"       ,ODO;
      FSHARP, "__token_OLET"      ,OLET(true);
      FSHARP, "__token_constraint",CONSTRAINT;
      ]
    (*------- reserved keywords which are ml-compatibility ids *) 
    @ List.map (fun s -> (FSHARP,s,RESERVED)) 
        [ "atomic"; "break"; 
          "checked"; "component"; "const"; "constraint"; "constructor"; "continue"; 
          "eager"; 
          "fixed"; "fori"; "functor"; 
          "include";  
          "measure"; "method"; "mixin"; 
          "object"; 
          "parallel"; "params";  "process"; "protected"; "pure"; 
          "recursive"; 
          "sealed"; 
          "trait";  "tailcall";
          "virtual"; "volatile"; ]

    let private unreserveWords = 
        keywordList |> List.choose (function (mode,keyword,_) -> if mode = FSHARP then Some keyword else None) 

    //------------------------------------------------------------------------
    // Keywords
    //-----------------------------------------------------------------------

    let keywordNames = 
        keywordList |> List.map (fun (_, w, _) -> w) 

    let keywordTable = 
        let tab = HashMultiMap(1000, HashIdentity.Structural)
        List.iter (fun (_mode,keyword,token) -> tab.Add(keyword,token)) keywordList;
        tab
        
    let KeywordToken s = keywordTable.[s]

    let permitFsharpKeywords = ref true

    let IdentifierToken args (lexbuf:UnicodeLexing.Lexbuf) (s:string) =
        if IsCompilerGeneratedName s then 
            let m = getLexerRange lexbuf 
            warning(Error(FSComp.SR.lexhlpIdentifiersContainingAtSymbolReserved(),m));
        IDENT (args.resourceManager.InternString(s))

    let KeywordOrIdentifierToken args (lexbuf:UnicodeLexing.Lexbuf) s =
        if not !permitFsharpKeywords && List.mem s unreserveWords then
            IdentifierToken args lexbuf s
        elif keywordTable.ContainsKey s then 
            let v = KeywordToken s 
            if (match v with RESERVED -> true | _ -> false) then
                let m = getLexerRange lexbuf 
                warning(ReservedKeyword(FSComp.SR.lexhlpIdentifierReserved(s),m));
                IdentifierToken args lexbuf s
            else v
        else 
            match s with 
            | "__SOURCE_DIRECTORY__" ->
                let filename = fileOfFileIndex lexbuf.StartPos.posFileIndex
                let dirname  = if filename = stdinMockFilename then
                                   System.IO.Directory.GetCurrentDirectory()
                               else
                                   filename |> Internal.Utilities.FileSystem.Path.SafeGetFullPath (* asserts that path is already absolute *)
                                            |> System.IO.Path.GetDirectoryName
                KEYWORD_STRING dirname
            | "__SOURCE_FILE__" -> 
               KEYWORD_STRING (System.IO.Path.GetFileName((fileOfFileIndex lexbuf.StartPos.posFileIndex)))
            | "__LINE__" -> 
               KEYWORD_STRING (string lexbuf.StartPos.Line)
            | _ -> 
               IdentifierToken args lexbuf s

    /// A utility to help determine if an identifier needs to be quoted 
    let QuoteIdentifierIfNeeded (s : string) : string =
        let isKeyword (n : string) : bool = List.exists ((=) n) keywordNames
        if isKeyword s || not (String.forall IsLongIdentifierPartCharacter s) then 
            "``" + s + "``"
        else 
            s


