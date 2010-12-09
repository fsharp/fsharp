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

//----------------------------------------------------------------------------
// Some general F# utilities for mangling / unmangling / manipulating names.
//--------------------------------------------------------------------------


/// Anything to do with special names of identifiers and other lexical rules 
module internal Microsoft.FSharp.Compiler.PrettyNaming
    open Internal.Utilities
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.AbstractIL.Internal
    open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

/// Anything to do with special names of identifiers and other lexical rules 

    open System.Globalization
    open System.Collections.Generic

    //------------------------------------------------------------------------
    // Operator name compilation
    //-----------------------------------------------------------------------

    let parenGet = ".()"
    let parenSet = ".()<-"
    let qmark = "?"
    let qmarkSet = "?<-"

    let private opNameTable = 
     [ ("[]", "op_Nil");
       ("::", "op_ColonColon");
       ("+", "op_Addition");
       ("~%", "op_Splice");
       ("~%%", "op_SpliceUntyped");
       ("~++", "op_Increment");
       ("~--", "op_Decrement");
       ("-", "op_Subtraction");
       ("*", "op_Multiply");
       ("**", "op_Exponentiation");
       ("/", "op_Division");
       ("@", "op_Append");
       ("^", "op_Concatenate");
       ("%", "op_Modulus");
       ("&&&", "op_BitwiseAnd");
       ("|||", "op_BitwiseOr");
       ("^^^", "op_ExclusiveOr");
       ("<<<", "op_LeftShift");
       ("~~~", "op_LogicalNot");
       (">>>", "op_RightShift");
       ("~+", "op_UnaryPlus");
       ("~-", "op_UnaryNegation");
       ("~&", "op_AddressOf");
       ("~&&", "op_IntegerAddressOf");
       ("&&", "op_BooleanAnd");
       ("||", "op_BooleanOr");
       ("<=", "op_LessThanOrEqual");
       ("=","op_Equality");
       ("<>","op_Inequality");
       (">=", "op_GreaterThanOrEqual");
       ("<", "op_LessThan");
       (">", "op_GreaterThan");
       ("|>", "op_PipeRight");
       ("||>", "op_PipeRight2");
       ("|||>", "op_PipeRight3");
       ("<|", "op_PipeLeft");
       ("<||", "op_PipeLeft2");
       ("<|||", "op_PipeLeft3");
       ("!", "op_Dereference");
       (">>", "op_ComposeRight");
       ("<<", "op_ComposeLeft");
       ("<< >>", "op_TypedQuotationUnicode");
       ("<<| |>>", "op_ChevronsBar");
       ("<@ @>", "op_Quotation");
       ("<@@ @@>", "op_QuotationUntyped");
       ("+=", "op_AdditionAssignment");
       ("-=", "op_SubtractionAssignment");
       ("*=", "op_MultiplyAssignment");
       ("/=", "op_DivisionAssignment");
       ("..", "op_Range");
       (".. ..", "op_RangeStep"); 
       ("?", "op_Dynamic");
       ("?<-", "op_DynamicAssignment");
       (parenGet, "op_ArrayLookup");
       (parenSet, "op_ArrayAssign");
       ]

    let private opCharTranslateTable =
      [ ( '>', "Greater");
        ( '<', "Less"); 
        ( '+', "Plus");
        ( '-', "Minus");
        ( '*', "Multiply");
        ( '=', "Equals");
        ( '~', "Twiddle");
        ( '%', "Percent");
        ( '.', "Dot");
        ( '$', "Dollar");
        ( '&', "Amp");
        ( '|', "Bar");
        ( '@', "At");
        ( '#', "Hash");
        ( '^', "Hat");
        ( '!', "Bang");
        ( '?', "Qmark");
        ( '/', "Divide");
        ( ':', "Colon");
        ( '(', "LParen");
        ( ',', "Comma");
        ( ')', "RParen");
        ( ' ', "Space");
        ( '[', "LBrack");
        ( ']', "RBrack"); ]

    let private opCharDict = 
        let t = new Dictionary<_,_>()
        for (c,_) in opCharTranslateTable do 
            t.Add(c,1)
        t
        
    let IsOpName (n:string) =
        let rec loop i = (i < n.Length && (opCharDict.ContainsKey(n.[i]) || loop (i+1)))
        loop 0

    let CompileOpName =
        let t = Map.ofList opNameTable
        let t2 = Map.ofList opCharTranslateTable
        fun n -> 
            match t.TryFind(n) with 
            | Some(x) -> x 
            | None -> 
                if IsOpName n then 
                  let mutable r = []
                  for i = 0 to String.length n - 1 do
                      let c = n.[i]
                      let c2 = match t2.TryFind(c) with Some(x) -> x | None -> string c
                      r <- c2 :: r 
                  "op_"^(String.concat "" (List.rev r))
                else n

    let IsMangledOpName (n:string) = n.Length >= 3 && n.Substring(0,3) = "op_"     
                             
    let DecompileOpName = 
      let t = new Dictionary<string,string>()
      for (x,y) in opNameTable do
          t.Add(y,x)
      fun n -> 
          let mutable res = Unchecked.defaultof<_>
          if t.TryGetValue(n,&res) then 
              res
          else
              if n.StartsWith("op_",System.StringComparison.Ordinal) then 
                let rec loop (remaining:string) = 
                    let l = remaining.Length
                    if l = 0 then Some(remaining) else
                    let choice = 
                      opCharTranslateTable |> List.tryPick (fun (a,b) -> 
                          let bl = b.Length
                          if bl <= l && remaining.Substring(0,bl) = b then 
                            Some(string a, remaining.Substring(bl,l - bl)) 
                          else None) 
                        
                    match choice with 
                    | Some (a,remaining2) -> 
                        match loop remaining2 with 
                        | None -> None
                        | Some a2 -> Some(a^a2)
                    | None -> None (* giveup *)
                match loop (n.Substring(3,n.Length - 3)) with
                | Some res -> res
                | None -> n
              else n
                  
    let opNameCons = CompileOpName "::"
    let opNameNil = CompileOpName "[]"
    let opNameEquals = CompileOpName "="


    /// The characters that are allowed to be in an identifier.
    let IsIdentifierPartCharacter c =
        let cat = System.Char.GetUnicodeCategory(c)
        (    cat = UnicodeCategory.UppercaseLetter // Letters
          || cat = UnicodeCategory.LowercaseLetter 
          || cat = UnicodeCategory.TitlecaseLetter
          || cat = UnicodeCategory.ModifierLetter
          || cat = UnicodeCategory.OtherLetter
          || cat = UnicodeCategory.LetterNumber 
          || cat = UnicodeCategory.DecimalDigitNumber // Numbers
          || cat = UnicodeCategory.ConnectorPunctuation // Connectors
          || cat = UnicodeCategory.NonSpacingMark // Combiners
          || cat = UnicodeCategory.SpacingCombiningMark
          || c = '\'' // Tick
        )

    /// Is this character a part of a long identifier 
    let IsLongIdentifierPartCharacter c = 
        (IsIdentifierPartCharacter c) || (c = '.')

    let IsValidPrefixOperatorUse s = 
        match s with 
        | "+" | "-" | "+." | "-." | "%" | "%%" | "&" | "&&" -> true
        | _ -> s.[0] = '!' || (s.[0] = '~' && String.forall (fun c -> c = s.[0]) s)
    
    let IsValidPrefixOperatorDefinitionName s = 
        match s with 
        | "~+" | "~-" | "~+." | "~-." | "~%" | "~%%" | "~&" | "~&&" -> true
        | "?<-" -> true // this comes through as a PREFIX_OP at a definition site. Let it through as a definition, not a use
        | _ -> (s.[0] = '!' && s <> "!=") || (s.[0] = '~' && String.forall (fun c -> c = s.[0]) s)
        
    let IsPrefixOperator s = 
        let s = DecompileOpName s
        match s with 
        | "~+" | "~-" | "~+." | "~-." | "~%" | "~%%" | "~&" | "~&&" -> true
        | _ -> (s.[0] = '!' && s <> "!=")  || (s.[0] = '~' && String.forall (fun c -> c = s.[0]) s)

    let IsTernaryOperator s = 
        DecompileOpName s = "?<-"

    let IsInfixOperator s (* where s is assumed to be a compiled name *) =
        // Certain operator idents are parsed as infix expression operators.
        // The parsing as infix operators is hardwired in the grammar [see declExpr productions]
        // where certain operator tokens are accepted in infix forms, i.e. <expr> <op> <expr>.
        // The lexer defines the strings that lead to those tokens.
        //------
        // This function recognises these "infix operator" names.
        let s = DecompileOpName s
        let skipIgnoredChars = s.TrimStart([| '.' |])
        let afterSkipStartsWith prefix   = skipIgnoredChars.StartsWith(prefix,System.StringComparison.Ordinal)
        let afterSkipStarts     prefixes = List.exists afterSkipStartsWith prefixes
        // The following conditions follow the declExpr infix clauses. The test corresponds to the lexer definition for the token.
        s = ":=" ||                                    // COLON_EQUALS
        afterSkipStartsWith "|" ||                     // BAR_BAR, INFIX_BAR_OP
        afterSkipStartsWith "&"  ||                    // AMP, AMP_AMP, INFIX_AMP_OP
        afterSkipStarts ["=";"!=";"<";">";"$"] ||      // EQUALS, INFIX_COMPARE_OP, LESS, GREATER
        s = "$" ||                                     // DOLLAR
        afterSkipStarts ["@";"^"] ||                   // INFIX_AT_HAT_OP
        s = "::" ||                                    // COLON_COLON
        afterSkipStarts ["+";"-"] ||                   // PLUS_MINUS_OP, MINUS
        afterSkipStarts ["*";"/";"%"] ||               // PERCENT_OP, STAR, INFIX_STAR_DIV_MOD_OP
        s = "**"                                       // INFIX_STAR_STAR_OP

    let (|Control|Equality|Relational|Indexer|FixedTypes|Other|) opName = 
        if (opName = "&" || opName = "or" || opName = "&&" || opName = "||") then Control
        elif (opName = "<>" || opName = "=" ) then Equality
        elif (opName = "<" || opName = ">" || opName = "<=" || opName = ">=") then Relational
        elif (opName = "<<" || opName = "<|" || opName = "<||" || opName = "<||" || opName = "|>" || opName = "||>" || opName = "|||>" || opName = ">>" || opName = "^" || opName = ":=" || opName = "@") then FixedTypes
        elif (opName = ".[]" ) then Indexer
        else Other

    let private compilerGeneratedMarker = "@"
    
    let IsCompilerGeneratedName (nm:string) =
        nm.Contains(compilerGeneratedMarker) 
        
    let CompilerGeneratedName nm =
        if IsCompilerGeneratedName nm then nm else nm+compilerGeneratedMarker

    let GetBasicNameOfPossibleCompilerGeneratedName (name:string) =
            match name.IndexOf compilerGeneratedMarker with 
            | -1 | 0 -> name
            | n -> name.[0..n-1]

    let CompilerGeneratedNameSuffix (basicName:string) suffix =
        basicName^compilerGeneratedMarker^suffix


    //-------------------------------------------------------------------------
    // Handle mangled .NET generic type names
    //------------------------------------------------------------------------- 
     
    let private mangledGenericTypeNameSym = '`'
    let IsMangledGenericName (n:string) = 
        n.IndexOf mangledGenericTypeNameSym <> -1 &&
        (* check what comes after the symbol is a number *)
        let m = n.LastIndexOf mangledGenericTypeNameSym
        let mutable res = m < n.Length - 1
        for i = m + 1 to n.Length - 1 do
            res <- res && n.[i] >= '0' && n.[i] <= '9';
        res

    type NameArityPair = NameArityPair of string*int
    let DecodeGenericTypeName n = 
        if IsMangledGenericName n then 
            let pos = n.LastIndexOf mangledGenericTypeNameSym
            let res = n.Substring(0,pos)
            let num = n.Substring(pos+1,n.Length - pos - 1)
            NameArityPair(res, int32 num)
        else NameArityPair(n,0)

    let DemangleGenericTypeName n = 
        if  IsMangledGenericName n then 
            let pos = n.LastIndexOf mangledGenericTypeNameSym
            n.Substring(0,pos)
        else n

    //-------------------------------------------------------------------------
    // Property name mangling.
    // Expecting s to be in the form (as returned by qualifiedMangledNameOfTyconRef) of:
    //    get_P                         or  set_P
    //    Names/Space/Class/NLPath-get_P  or  Names/Space/Class/NLPath.set_P
    // Required to return "P"
    //-------------------------------------------------------------------------

    let private chopStringTo (s:string) (c:char) =
        (* chopStringTo "abcdef" 'c' --> "def" *)
        if s.IndexOf c <> -1 then
            let i =  s.IndexOf c + 1
            s.Substring(i, s.Length - i)
        else
            s

    /// Try to chop "get_" or "set_" from a string
    let TryChopPropertyName (s: string) =
        // extract the logical name from any mangled name produced by MakeMemberDataAndMangledNameForMemberVal 
        let s = 
            if s.StartsWith("get_", System.StringComparison.Ordinal) || 
               s.StartsWith("set_", System.StringComparison.Ordinal) 
            then s 
            else chopStringTo s '.'

        if s.Length <= 4 || (let s = s.Substring(0,4) in s <> "get_" && s <> "set_") then
            None
        else 
            Some(s.Substring(4,s.Length - 4) )


    let ChopPropertyName s =
        match TryChopPropertyName s with 
        | None -> 
            failwith("Invalid internal property name: '"^s^"'"); 
            s
        | Some res -> res
        

    let DemangleOperatorName nm = 
        let nm = DecompileOpName nm
        if IsOpName nm then "( "^nm^" )" else nm 

    /// Used when generating the paths used by FSI file generation
    let SplitNamesForFsiGenerationPath (s : string) : string list = 
        if s.StartsWith("``",System.StringComparison.Ordinal) && s.EndsWith("``",System.StringComparison.Ordinal) && s.Length > 4 then [s.Substring(2, s.Length-4)] // identifier is enclosed in `` .. ``, so it is only a single element (this is very approximate)
        else s.Split [| '.' ; '`' |] |> Array.toList      // '.' chops members / namespaces / modules; '`' chops generic parameters for .NET types
        
    /// Used when generating the paths used by FSI file generation
    let JoinNamesForFsiGenerationPath (ns : string list) : string = String.concat "." ns

    /// Used when generating the paths used by FSI file generation
    let ChopUnshowableInFsiGenerationPath (ns : string list) : string list =
        let showable s =
            match s with
            | "static" -> false
            | _        -> s |> String.forall (fun x -> x >= '0' && x <= '9') |> not // this is safe because a stretch composed solely of digits doesn't constitute an identifier

        let rec loop =
          function []                            -> []
                 | x :: _  as xs when showable x -> xs
                 | _ :: xs                       -> loop xs
        ns |> List.rev |> loop |> List.rev

    
    let FSharpModuleSuffix = "Module"

    let MangledGlobalName = "`global`"
    
    let IllegalCharactersInTypeAndNamespaceNames = [| '.'; '+'; '$'; '&'; '['; ']'; '/'; '\\'; '*'; '\"'; '`'  |]

    let IsActivePatternName (nm:string) =
        (nm.IndexOf '|' = 0) &&
        nm.Length >= 3 &&
        (nm.LastIndexOf '|' = nm.Length - 1) &&
        (let core = nm.Substring(1,nm.Length - 2) 
         // no operator characters except '|'
         core |> String.forall (fun c -> c = '|' || not (opCharDict.ContainsKey c)) &&
         // at least one non-operator character
         core |> String.exists (fun c -> not (opCharDict.ContainsKey c)))

    //IsActivePatternName "|+|" = false
    //IsActivePatternName "|ABC|" = true
    //IsActivePatternName "|ABC|DEF|" = true
    //IsActivePatternName "|||" = false
    //IsActivePatternName "||S|" = true

    type ActivePatternInfo = 
        | APInfo of bool * string list 
        member x.IsTotal = let (APInfo(p,_)) = x in p
        member x.ActiveTags = let (APInfo(_,tags)) = x in tags

    let ActivePatternInfoOfValName nm = 
        let rec loop (nm:string) = 
            let n = nm.IndexOf '|'
            if n > 0 then 
               nm.[0..n-1] :: loop nm.[n+1..]
            else
               [nm]
        let nm = DecompileOpName nm
        if IsActivePatternName nm then 
            let res = loop nm.[1..nm.Length-2]
            let resH,resT = List.frontAndBack res
            Some(if resT = "_" then APInfo(false,resH) else APInfo(true,res))
        else 
            None
    
