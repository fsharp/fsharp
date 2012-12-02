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

//--------------------------------------------------------------------------
// Print Signatures/Types, for signatures, intellisense, quick info, FSI responses
//-------------------------------------------------------------------------- 

module internal Microsoft.FSharp.Compiler.NicePrint

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.IL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.AbstractIL.IL (* Abstract IL  *)
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Core.Printf
#if EXTENSIONTYPING
open Microsoft.FSharp.Compiler.ExtensionTyping
open Microsoft.FSharp.Core.CompilerServices
#endif

open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.PrettyNaming

[<AutoOpen>]
module private PrintUtilities = 
    let bracketIfL x lyt = if x then bracketL lyt else lyt
    let squareAngleL x = leftL "[<" ^^ x ^^ rightL ">]"
    let angleL x = sepL "<" ^^ x ^^ rightL ">"  
    let braceL x = leftL "{" ^^ x ^^ rightL "}"  

    let commentL l = wordL "(*" ++ l ++ wordL "*)"
    let comment str = str |> wordL |> commentL

    let layoutsL (ls : layout list) : layout =
        match ls with
        | []      -> emptyL
        | [x]     -> x
        | x :: xs -> List.fold (^^) x xs 

    let suppressInheritanceAndInterfacesForTyInSimplifiedDisplays g amap m ty = 
        isEnumTy g ty || isDelegateTy g ty || ExistsHeadTypeInEntireHierarchy g amap m ty g.exn_tcr || ExistsHeadTypeInEntireHierarchy g amap m ty g.tcref_System_Attribute 


    let applyMaxMembers maxMembers (alldecls : _ list) = 
        match maxMembers with 
        | Some n when alldecls.Length > n -> (alldecls |> Seq.truncate n |> Seq.toList) @ [wordL "..."] 
        | _ -> alldecls

    /// fix up a name coming from IL metadata by quoting "funny" names (keywords, otherwise invalid identifiers)
    let adjustILName n =
        n |> Lexhelp.Keywords.QuoteIdentifierIfNeeded

    // Put the "+ N overloads" into the layout
    let shrinkOverloads layoutFunction resultFunction group = 
        match group with 
        | [x] -> [resultFunction x (layoutFunction x)] 
        | (x:: rest) -> [ resultFunction x (layoutFunction x -- leftL (match rest.Length with 1 -> FSComp.SR.nicePrintOtherOverloads1() | n -> FSComp.SR.nicePrintOtherOverloadsN(n))) ] 
        | _ -> []

module private PrintIL = 

    open Microsoft.FSharp.Compiler.AbstractIL.IL
        
    let fullySplitILTypeRef (tref:ILTypeRef) = 
        (List.collect IL.splitNamespace (tref.Enclosing @ [IL.ungenericizeTypeName tref.Name])) 

    let layoutILTypeRefName denv path =
        let path = 
            match path with 
            | [ "System"; "Void"   ] -> ["unit"]
            | [ "System"; "Object" ] -> ["obj"]
            | [ "System"; "String" ] -> ["string"]
            | [ "System"; "Single" ] -> ["float32"]
            | [ "System"; "Double" ] -> ["float"]
            | [ "System"; "Decimal"] -> ["decimal"]
            | [ "System"; "Char"   ] -> ["char"]
            | [ "System"; "Byte"   ] -> ["byte"]
            | [ "System"; "SByte"  ] -> ["sbyte"]
            | [ "System"; "Int16"  ] -> ["int16"]
            | [ "System"; "Int32"  ] -> ["int" ]
            | [ "System"; "Int64"  ] -> ["int64" ]
            | [ "System"; "UInt16" ] -> ["uint16" ]
            | [ "System"; "UInt32" ] -> ["uint32" ]
            | [ "System"; "UInt64" ] -> ["uint64" ]
            | [ "System"; "IntPtr" ] -> ["nativeint" ]
            | [ "System"; "UIntPtr" ] -> ["unativeint" ]
            | [ "System"; "Boolean"] -> ["bool"]
            | _                -> path
        let p2,n = List.frontAndBack path
        if denv.shortTypeNames then 
            wordL n
          else
            leftL (trimPathByDisplayEnv denv p2) ^^ wordL n

    let layoutILTypeRef denv tref =
        let path = fullySplitILTypeRef tref
        layoutILTypeRefName denv path

    /// this fixes up a name just like adjustILName but also handles F#
    /// operators
    let private adjustILMethodName n =
        let demangleOperatorNameIfNeeded s =
            if IsMangledOpName s
            then DemangleOperatorName s
            else s
        n |> Lexhelp.Keywords.QuoteIdentifierIfNeeded |> demangleOperatorNameIfNeeded 

    let private isStaticILEvent (e: ILEventDef) = 
        e.AddMethod.CallingSignature.CallingConv.IsStatic || 
        e.RemoveMethod.CallingSignature.CallingConv.IsStatic

    let private layoutILArrayShape (ILArrayShape sh) = 
        sepL "[" ^^ wordL (sh |> List.tail |> List.map (fun _ -> ",") |> String.concat "") ^^ rightL "]" // drop off one "," so that a n-dimensional array has n - 1 ","'s

    let private  layoutILGenericParameterDefs (ps: ILGenericParameterDefs) = 
        ps |> List.map (fun x -> "'" + x.Name |> wordL) 

    let private paramsL (ps: layout list) : layout = 
        match ps with
        | [] -> emptyL
        | _  -> 
            let body = Layout.commaListL ps
            sepL "<" ^^ body ^^ rightL ">"

    let private pruneParms (className: string) (ilTyparSubst: layout list) =
        let numParms = 
            // can't find a way to see the number of generic parameters for *this* class (the GenericParams also include type variables for enclosing classes); this will have to do
            let rightMost = className |> SplitNamesForILPath |> List.last
            match System.Int32.TryParse(rightMost, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) with 
            | true, n -> n
            | false, _ -> 0 // looks like it's non-generic
        ilTyparSubst |> List.rev |> List.take numParms |> List.rev
                             
    let rec layoutILType (denv: DisplayEnv) (ilTyparSubst: layout list) (typ : ILType) : layout =
        match typ with
        | ILType.Void               -> wordL "unit" // These are type-theoretically totally different type-theoretically `void` is Fin 0 and `unit` is Fin (S 0) ... but, this looks like as close as we can get.
        | ILType.Array (sh, t)      -> layoutILType denv ilTyparSubst t ^^ layoutILArrayShape sh
        | ILType.Value t
        | ILType.Boxed t            -> layoutILTypeRef denv t.TypeRef ^^ (t.GenericArgs |> ILList.toList |> List.map (layoutILType denv ilTyparSubst) |> paramsL)
        | ILType.Ptr t
        | ILType.Byref t            -> layoutILType denv ilTyparSubst t
        | ILType.FunctionPointer t  -> layoutILCallingSignature denv ilTyparSubst None t
        | ILType.TypeVar n            -> List.nth ilTyparSubst (int n)
        | ILType.Modified (_, _, t) -> layoutILType denv ilTyparSubst t // "Just recurse through them to the contained ILType"--Don

    /// Layout a function pointer signature using type-only-F#-style. No argument names are printed.
    and private layoutILCallingSignature denv ilTyparSubst cons (signatur : ILCallingSignature) =
        // We need a special case for
        // constructors (Their return types are reported as `void`, but this is
        // incorrect; so if we're dealing with a constructor we require that the
        // return type be passed along as the `cons` parameter.)
        let args = signatur.ArgTypes |> ILList.toList |> List.map (layoutILType denv ilTyparSubst) 
        let res  = 
            match cons with
            | Some className -> layoutILTypeRefName denv (SplitNamesForILPath (ungenericizeTypeName className)) ^^ (pruneParms className ilTyparSubst |> paramsL) // special case for constructor return-type (viz., the class itself)
            | None           -> signatur.ReturnType |> layoutILType denv ilTyparSubst
        match args with
        | []   -> wordL "unit" ^^ wordL "->" ^^ res
        | [x]  -> x ^^ wordL "->" ^^ res
        | _    -> sepListL (wordL "*") args ^^ wordL "->" ^^ res

    /// Layout a function pointer signature using type-only-F#-style. No argument names are printed.
    //
    // Note, this duplicates functionality in formatParamDataToBuffer
    and private layoutILParameter denv ilTyparSubst (p: ILParameter) =
        let preL = 
            let isParamArray = TryFindILAttribute denv.g.attrib_ParamArrayAttribute p.CustomAttrs
            match isParamArray, p.Name, p.IsOptional with 
            // Layout an optional argument 
            | _, Some nm, true -> leftL ("?" + nm + ":")
            // Layout an unnamed argument 
            | _, None, _ -> leftL ":"
            // Layout a named argument 
            | true, Some nm,_ -> wordL "params" ^^ leftL (nm+":")
            | false, Some nm,_ -> leftL (nm+":")
        preL ^^ (layoutILType denv ilTyparSubst p.Type)
       

    /// Layout a function pointer signature using type-only-F#-style. No argument names are printed.
    and private layoutILParameters denv ilTyparSubst cons (parameters: ILParameters, retType: ILType) =
        // We need a special case for
        // constructors (Their return types are reported as `void`, but this is
        // incorrect; so if we're dealing with a constructor we require that the
        // return type be passed along as the `cons` parameter.)
        let res  = 
            match cons with
            | Some className -> layoutILTypeRefName denv (SplitNamesForILPath (ungenericizeTypeName className)) ^^ (pruneParms className ilTyparSubst |> paramsL) // special case for constructor return-type (viz., the class itself)
            | None           -> retType |> layoutILType denv ilTyparSubst
        match parameters |> ILList.toList  with
        | []   -> wordL "unit" ^^ wordL "->" ^^ res
        | [x]  -> layoutILParameter denv ilTyparSubst x ^^ wordL "->" ^^ res
        | args    -> sepListL (wordL "*") (List.map (layoutILParameter denv ilTyparSubst) args) ^^ wordL "->" ^^ res


    /// Layout a method's signature using type-only-F#-style. No argument names are printed.
    /// 
    /// In the case that we've a constructor, we
    /// pull off the class name from the `path`; naturally, it's the
    /// most-deeply-nested element.
    //
    // For C# and provided members:
    //          new : argType1 * ... * argTypeN -> retType
    //          Method : argType1 * ... * argTypeN -> retType
    //
    let private layoutILMethodDef denv ilTyparSubst className (m: ILMethodDef) =
        let myParms         = m.GenericParams |> layoutILGenericParameterDefs
        let ilTyparSubst           = ilTyparSubst @ myParms
        let name            = adjustILMethodName m.Name
        let (nameL, isCons) = 
            match () with
            | _ when m.IsConstructor -> (wordL "new",                                                            Some className) // we need the unadjusted name here to be able to grab the number of generic parameters
            | _ when m.IsStatic      -> (wordL "static" ^^ wordL "member" ^^ wordL name ^^ (myParms |> paramsL), None)
            | _                      -> (wordL "member" ^^ wordL name ^^ (myParms |> paramsL),                   None)
        let signaturL       = (m.Parameters, m.Return.Type) |> layoutILParameters denv ilTyparSubst isCons
        nameL ^^ wordL ":" ^^ signaturL

    let private layoutILFieldDef (denv: DisplayEnv) (ilTyparSubst: layout list) (f: ILFieldDef) =
        let staticL =  if f.IsStatic then wordL "static" else emptyL
        let name    = adjustILName f.Name
        let nameL   = wordL name
        let typL    = layoutILType denv ilTyparSubst f.Type
        staticL ^^ wordL "val" ^^ nameL ^^ wordL ":" ^^ typL  
            
    let private layoutILEventDef denv  ilTyparSubst (e: ILEventDef) =
        let staticL = if isStaticILEvent e then wordL "static" else emptyL
        let name = adjustILName e.Name
        let nameL = wordL name
        let typL = 
            match e.Type with
            | Some t -> layoutILType denv ilTyparSubst t
            | _ -> emptyL
        staticL ^^ wordL "event" ^^ nameL ^^ wordL ":" ^^ typL     
       
    let private layoutILPropertyDef denv ilTyparSubst (p : ILPropertyDef) =
        let staticL =  if p.CallingConv =  ILThisConvention.Static then wordL "static" else emptyL
        let name    = adjustILName p.Name
        let nameL   = wordL name 
            
        let layoutGetterType (getterRef:ILMethodRef) =
            if ILList.isEmpty getterRef.ArgTypes then
                layoutILType denv ilTyparSubst getterRef.ReturnType
            else
                layoutILCallingSignature denv ilTyparSubst None getterRef.CallingSignature
                
        let layoutSetterType (setterRef:ILMethodRef) =
            let argTypes = setterRef.ArgTypes |> ILList.toList 
            if isNil argTypes then
                emptyL // shouldn't happen
            else
                let frontArgs, lastArg = List.frontAndBack argTypes
                let argsL = frontArgs |> List.map (layoutILType denv ilTyparSubst) |> sepListL (wordL "*") 
                argsL ^^ wordL "->" ^^ (layoutILType denv ilTyparSubst lastArg)
            
        let typL = 
            match p.GetMethod, p.SetMethod with
            |   None, None -> layoutILType denv ilTyparSubst p.Type // shouldn't happen
            |   Some getterRef, _ -> layoutGetterType getterRef
            |   None, Some setterRef -> layoutSetterType setterRef
                
        let specGetSetL =
            match p.GetMethod, p.SetMethod with
            |   None,None 
            |   Some _, None -> emptyL
            |   None, Some _ -> wordL "with" ^^ wordL " set"
            |   Some _, Some _ -> wordL "with" ^^ wordL "get," ^^ wordL "set"
        staticL ^^ wordL "member" ^^ nameL ^^ wordL ":" ^^ typL ^^ specGetSetL

    let layoutILFieldInit x =
        let textOpt = 
            match x with
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
        match textOpt with
        | None   -> wordL "=" ^^ (comment "value unavailable")
        | Some s -> wordL "=" ^^ wordL s

    let layoutILEnumDefParts nm litVal =
        wordL "|" ^^ wordL (adjustILName nm) ^^ layoutILFieldInit litVal

    let private layoutILEnumDef (f : ILFieldDef) = layoutILEnumDefParts f.Name f.LiteralValue

    // filtering methods for hiding things we oughtn't show
    let private isStaticILProperty    (p : ILPropertyDef)      = 
        match p.GetMethod,p.SetMethod with
        | Some getter, _    -> getter.CallingSignature.CallingConv.IsStatic
        | None, Some setter -> setter.CallingSignature.CallingConv.IsStatic
        | None, None        -> true


    let private isPublicILMethod      (m : ILMethodDef) = 
        (m.Access = ILMemberAccess.Public)

    let private isPublicILEvent typeDef (e: ILEventDef)         = 
        try
            isPublicILMethod(resolveILMethodRef typeDef e.AddMethod) &&
            isPublicILMethod(resolveILMethodRef typeDef e.RemoveMethod)
        with _ ->
            false

    let private isPublicILProperty typeDef (m : ILPropertyDef) = 
        try
            match m.GetMethod with 
            | Some ilMethRef -> isPublicILMethod (resolveILMethodRef typeDef ilMethRef)
            | None -> 
                match m.SetMethod with 
                | None -> false
                | Some ilMethRef -> isPublicILMethod (resolveILMethodRef typeDef ilMethRef)
        // resolveILMethodRef is a possible point of failure if Abstract IL type equality checking fails 
        // to link the method ref to a method def for some reason, e.g. some feature of IL type
        // equality checking has not been implemented. Since this is just intellisense pretty printing code
        // it is better to swallow the exception here, though we don't know of any
        // specific cases where this happens
        with _ -> 
            false

    let private isPublicILCtor        (m : ILMethodDef) = 
        (m.Access = ILMemberAccess.Public && m.IsConstructor)

    let private isNotSpecialName    (m : ILMethodDef) = 
        not m.IsSpecialName

    let private isPublicILField       (f : ILFieldDef)  = 
        (f.Access = ILMemberAccess.Public)

    let private isPublicILTypeDef       (c : ILTypeDef)   : bool =
        match c.Access with
        | ILTypeDefAccess.Public
        | ILTypeDefAccess.Nested ILMemberAccess.Public -> true
        | _                                  -> false

    let private isShowEnumField (f : ILFieldDef) : bool = f.Name <> "value__" // this appears to be the hard-coded underlying storage field
    let private noShow = set [ "System.Object" ; "Object"; "System.ValueType" ; "ValueType"; "obj" ] // hide certain 'obvious' base classes
    let private isShowBase      (n : layout)   : bool = 
        not (noShow.Contains(showL n))

    let rec layoutILTypeDef (denv: DisplayEnv) (typeDef : ILTypeDef) : layout =
        let ilTyparSubst       = typeDef.GenericParams |> layoutILGenericParameterDefs

        let renderL pre body post = 
            match pre with
            | Some pre -> 
                match body with
                | [] -> emptyL // empty type
                | _  -> (pre @@-- aboveListL body) @@ post
            | None     -> 
                aboveListL body

        match typeDef.tdKind with
        | ILTypeDefKind.Class     
        | ILTypeDefKind.ValueType 
        | ILTypeDefKind.Interface -> 
            let pre = 
                match typeDef.tdKind with
                | ILTypeDefKind.Class     -> None
                | ILTypeDefKind.ValueType -> Some (wordL "struct")
                | ILTypeDefKind.Interface -> None
                | _ -> failwith "unreachable"
            let baseT  = 
                match typeDef.Extends with
                | Some b -> 
                    let baseName = layoutILType denv ilTyparSubst b
                    if isShowBase baseName
                        then [ wordL "inherit" ^^ baseName ]
                        else []
                | None   -> []

            let memberBlockLs (fieldDefs:ILFieldDefs, methodDefs:ILMethodDefs, propertyDefs:ILPropertyDefs, eventDefs:ILEventDefs) =
                let ctors  =
                    methodDefs.AsList 
                    |> List.filter isPublicILCtor 
                    |> List.sortBy (fun md -> md.Parameters.Length)
                    |> shrinkOverloads (layoutILMethodDef denv ilTyparSubst typeDef.Name) (fun _ xL -> xL) 

                let fields = 
                    fieldDefs.AsList
                    |> List.filter isPublicILField   
                    |> List.map (layoutILFieldDef denv ilTyparSubst)

                let props = 
                    propertyDefs.AsList 
                    |> List.filter (isPublicILProperty typeDef)
                    |> List.map (fun pd -> (pd.Name, pd.Args.Length), layoutILPropertyDef denv ilTyparSubst pd)
                    
                let events =
                    eventDefs.AsList
                    |> List.filter (isPublicILEvent typeDef)
                    |> List.map (layoutILEventDef denv ilTyparSubst)

                let meths = 
                    methodDefs.AsList
                    |> List.filter isPublicILMethod 
                    |> List.filter isNotSpecialName 
                    |> List.map (fun md -> (md.Name, md.Parameters.Length), md)
                    // collect into overload groups
                    |> List.groupBy (fst >> fst)
                    |> List.collect (fun (_,group) -> group |> List.sortBy fst |> shrinkOverloads (snd >> layoutILMethodDef denv ilTyparSubst typeDef.Name) (fun x xL -> (fst x,xL)))

                let members = 
                    (props @ meths) 
                    |> List.sortBy fst 
                    |> List.map snd // (properties and members) are sorted by name/arity 


                ctors @ fields @ members @ events

            let bodyStatic   = 
                memberBlockLs (typeDef.Fields.AsList |> List.filter (fun fd -> fd.IsStatic)                 |> mkILFields,
                                typeDef.Methods.AsList |> List.filter (fun md -> md.IsStatic)                |> mkILMethods,
                                typeDef.Properties.AsList |> List.filter (fun pd -> isStaticILProperty pd)     |> mkILProperties,
                                typeDef.Events.AsList |> List.filter (fun ed -> isStaticILEvent ed)            |> mkILEvents)

            let bodyInstance = 
                memberBlockLs (typeDef.Fields.AsList |> List.filter (fun fd -> not(fd.IsStatic))                |> mkILFields,
                                typeDef.Methods.AsList |> List.filter (fun md -> not(md.IsStatic))               |> mkILMethods,
                                typeDef.Properties.AsList |> List.filter (fun pd -> not(isStaticILProperty pd))    |> mkILProperties,
                                typeDef.Events.AsList |> List.filter (fun ed -> not(isStaticILEvent ed))           |> mkILEvents )
  
            let body = bodyInstance @ bodyStatic // instance "member" before "static member" 

            // Only show at most maxMembers members...
            let body = applyMaxMembers denv.maxMembers body
  
            let types  = 
                typeDef.NestedTypes.AsList 
                |> List.filter isPublicILTypeDef
                |> List.sortBy(fun t -> adjustILName t.Name)   
                |> List.map (layoutILNestedClassDef denv)
  
            let post   = wordL "end"
            renderL pre (baseT @ body @ types ) post

        | ILTypeDefKind.Enum      -> 
            let fldsL = 
                typeDef.Fields.AsList 
                |> List.filter isShowEnumField 
                |> List.map layoutILEnumDef
                |> applyMaxMembers denv.maxMembers

            renderL None fldsL emptyL

        | ILTypeDefKind.Delegate  -> 
            let rhs = 
                match typeDef.Methods.AsList |> List.filter (fun m -> m.Name = "Invoke") with // the delegate delegates to the type of `Invoke`
                | m :: _ -> layoutILCallingSignature denv ilTyparSubst None m.CallingSignature
                | _      -> comment "`Invoke` method could not be found"
            wordL "delegate" ^^ wordL "of" ^^ rhs

        | ILTypeDefKind.Other _   -> comment "cannot show type"
          
    and layoutILNestedClassDef (denv: DisplayEnv) (typeDef : ILTypeDef) =
        let name     = adjustILName typeDef.Name
        let nameL    = wordL name 
        let ilTyparSubst    = typeDef.GenericParams |> layoutILGenericParameterDefs
        let paramsL  = pruneParms typeDef.Name ilTyparSubst |> paramsL
        if denv.suppressNestedTypes then 
            wordL "nested" ^^ wordL "type" ^^ nameL ^^ paramsL
        else 
            let pre      = wordL "nested" ^^ wordL "type" ^^ nameL ^^ paramsL
            let body     = layoutILTypeDef denv typeDef
            (pre ^^ wordL "=") @@-- body
                

module private PrintTypes = 
    // Note: We need nice printing of constants in order to print literals and attributes 
    let layoutConst g ty c =
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
            // either "null" or "the defaut value for a struct"
            | Const.Zero       -> if isRefTy g ty then "null" else "default"
        wordL str

    let layoutAccessibility (denv:DisplayEnv) accessibility itemL =   
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

    /// Layout a reference to a type 
    let layoutTyconRef (denv: DisplayEnv) (tcref:TyconRef) = 
        let demangled = if denv.includeStaticParametersInTypeNames then 
                            tcref.DisplayNameWithStaticParameters 
                        elif tcref.DisplayName = tcref.DisplayNameWithStaticParameters then
                            tcref.DisplayName // has no static params
                        else
                            tcref.DisplayName+"<...>" // shorten
        let tyconTextL = wordL demangled
        if denv.shortTypeNames then 
            tyconTextL
        else
            let path = demangledPathOfCompPath tcref.CompilationPath
            let path =
                if denv.includeStaticParametersInTypeNames then
                    path
                else
                    path |> List.map (fun s -> let i = s.IndexOf(',')
                                               if i <> -1 then s.Substring(0,i)+"<...>" // apparently has static params, shorten
                                               else s)
            let pathText = trimPathByDisplayEnv denv path
            if pathText = "" then tyconTextL else leftL pathText ^^ tyconTextL

    /// Layout the flags of a member 
    let layoutMemberFlags memFlags = 
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
    let rec private layoutAttribArg denv arg = 
        match arg with 
        | Expr.Const(c,_,ty) -> 
            if isEnumTy denv.g ty then 
                wordL "enum" ^^ angleL (layoutType denv ty) ^^ bracketL (layoutConst denv.g ty c)
            else
                layoutConst denv.g ty c

        | Expr.Op (TOp.Array,[_elemTy],args,_) ->
            leftL "[|" ^^ semiListL (List.map (layoutAttribArg denv) args) ^^ rightL "|]"

        // Detect 'typeof<ty>' calls 
        | TypeOfExpr denv.g ty ->
            leftL "typeof<" ^^ layoutType denv ty ^^ rightL ">"

        // Detect 'typedefof<ty>' calls 
        | TypeDefOfExpr denv.g ty ->
            leftL "typedefof<" ^^ layoutType denv ty ^^ rightL ">"

        | Expr.Op (TOp.Coerce,[tgTy;_],[arg2],_) ->
            leftL "(" ^^ layoutAttribArg denv arg2 ^^ wordL ":>" ^^ layoutType denv tgTy ^^ rightL ")"

        | AttribBitwiseOrExpr denv.g (arg1, arg2) ->
            layoutAttribArg denv arg1 ^^ wordL "|||" ^^ layoutAttribArg denv arg2

        // Detect explicit enum values 
        | EnumExpr denv.g arg1 ->
            wordL "enum" ++ bracketL (layoutAttribArg denv arg1)


        | _ -> wordL "(* unsupported attribute argument *)"

    /// Layout arguments of an attribute 'arg1, ..., argN' 
    and private layoutAttribArgs denv args = 
        sepListL (rightL ",") (List.map (fun (AttribExpr(e1,_)) -> layoutAttribArg denv e1) args)

    /// Layout an attribute 'Type(arg1, ..., argN)' 
    //
    // REVIEW: we are ignoring "props" here
    and private layoutAttrib denv (Attrib(_,k,args,_props,_,_,_)) = 
        let argsL = bracketL (layoutAttribArgs denv args)
        match k with 
        | (ILAttrib(ilMethRef)) -> 
            let trimmedName = 
                let name =  ilMethRef.EnclosingTypeRef.Name
                match String.tryDropSuffix name "Attribute" with 
                | Some shortName -> shortName
                | None -> name
            let tref = ilMethRef.EnclosingTypeRef
            let tref = ILTypeRef.Create(scope= tref.Scope, enclosing=tref.Enclosing, name=trimmedName)
            PrintIL.layoutILTypeRef denv tref ++ argsL

        | (FSAttrib(vref)) -> 
            // REVIEW: this is not trimming "Attribute" 
            let _,_,rty,_ = GetTypeOfMemberInMemberForm denv.g vref
            let rty = GetFSharpViewOfReturnType denv.g rty
            let tcref = tcrefOfAppTy denv.g rty
            layoutTyconRef denv tcref ++ argsL


    /// Layout '[<attribs>]' above another block 
    and layoutAttribs denv kind attrs restL = 
        
        if denv.showAttributes then
            // Don't display DllImport attributes in generated signatures  
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_DllImportAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_ContextStaticAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_ThreadStaticAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_EntryPointAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_MarshalAsAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_ReflectedDefinitionAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_StructLayoutAttribute >> not)
            let attrs = attrs |> List.filter (IsMatchingFSharpAttribute denv.g denv.g.attrib_AutoSerializableAttribute >> not)
            match attrs with
            | [] -> restL 
            | _  -> squareAngleL (sepListL (rightL ";") (List.map (layoutAttrib denv) attrs)) @@ 
                    restL
        else 
        match kind with 
        | TyparKind.Type -> restL
        | TyparKind.Measure -> squareAngleL (wordL "Measure") @@ restL

    and private layoutTyparAttribs denv kind attrs restL =         
        match attrs, kind with
        | [], TyparKind.Type -> restL 
        | _, _  -> squareAngleL (sepListL (rightL ";") ((match kind with TyparKind.Type -> [] | TyparKind.Measure -> [wordL "Measure"]) @ List.map (layoutAttrib denv) attrs)) ^^ restL

    and private layoutTyparRef denv (typar:Typar) =
        wordL (sprintf "%s%s%s"
                  (if denv.showConstraintTyparAnnotations then prefixOfStaticReq typar.StaticReq else "'")
                  (if denv.showImperativeTyparAnnotations then prefixOfRigidTypar typar else "")
                  typar.DisplayName)

    /// Layout a single type parameter declaration, taking TypeSimplificationInfo into account  
    /// There are several printing-cases for a typar:
    ///
    ///  'a              - is multiple  occurance.
    ///  _               - singleton occurrence, an underscore prefered over 'b. (OCAML accepts but does not print)
    ///  #Type           - inplace coercion constraint and singleton.
    ///  ('a :> Type)    - inplace coercion constraint not singleton.
    ///  ('a.opM : S->T) - inplace operator constraint.
    ///
    and private layoutTyparRefWithInfo denv (env:SimplifyTypes.TypeSimplificationInfo) (typar:Typar) =
        let varL = layoutTyparRef denv typar
        let varL = if denv.showAttributes then layoutTyparAttribs denv typar.Kind typar.Attribs varL else varL

        match Zmap.tryFind typar env.inplaceConstraints with
        | Some (typarConstrTyp) ->
            if Zset.contains typar env.singletons then
                leftL "#" ^^ layoutTypeWithInfo denv env typarConstrTyp
            else
                (varL ^^ sepL ":>" ^^ layoutTypeWithInfo denv env typarConstrTyp) |> bracketL

        | _ -> varL

      
    /// Layout type parameter constraints, taking TypeSimplificationInfo into account 
    and layoutConstraintsWithInfo denv env cxs = 
        
        
        // Internally member constraints get attached to each type variable in their support. 
        // This means we get too many constraints being printed. 
        // So we normalize the constraints to eliminate duplicate member constraints 
        let cxs = 
            cxs  
            |> ListSet.setify (fun (_,cx1) (_,cx2) ->
                      match cx1,cx2 with 
                      | TyparConstraint.MayResolveMember(traitInfo1,_),
                        TyparConstraint.MayResolveMember(traitInfo2,_) -> traitsAEquiv denv.g TypeEquivEnv.Empty traitInfo1 traitInfo2
                      | _ -> false)
                     
        let cxsL = List.collect (layoutConstraintWithInfo denv env) cxs
        match cxsL with 
        | [] -> emptyL 
        | _ -> 
            if denv.abbreviateAdditionalConstraints then 
                wordL "when <constraints>"
            elif denv.shortConstraints then 
                leftL "(" ^^ wordL "requires" ^^ sepListL (wordL "and") cxsL ^^ rightL ")"
            else
                wordL "when" ^^ sepListL (wordL "and") cxsL

    /// Layout constraints, taking TypeSimplificationInfo into account 
    and private layoutConstraintWithInfo denv env (tp,tpc) =
        let longConstraintPrefix l = layoutTyparRefWithInfo denv env tp ^^ wordL ":" ^^ l
        match tpc with 
        | TyparConstraint.CoercesTo(tpct,_) -> 
            [layoutTyparRefWithInfo denv env tp ^^ wordL ":>" --- layoutTypeWithInfo denv env tpct]
        | TyparConstraint.MayResolveMember(traitInfo,_) ->
            [layoutTraitWithInfo denv env traitInfo]
        | TyparConstraint.DefaultsTo(_,ty,_) ->
              if denv.showTyparDefaultConstraints then [wordL "default" ^^ layoutTyparRefWithInfo denv env tp ^^ wordL " :" ^^ layoutTypeWithInfo denv env ty]
              else []
        | TyparConstraint.IsEnum(ty,_) ->
            if denv.shortConstraints then 
                [wordL "enum"]
            else
                [longConstraintPrefix (layoutTypeAppWithInfoAndPrec denv env (wordL "enum") 2 true [ty])]
        | TyparConstraint.SupportsComparison _ ->
            if denv.shortConstraints then 
                [wordL "comparison"]
            else
                [wordL "comparison" |> longConstraintPrefix]
        | TyparConstraint.SupportsEquality _ ->
            if denv.shortConstraints then 
                [wordL "equality"]
            else
                [wordL "equality"  |> longConstraintPrefix]
        | TyparConstraint.IsDelegate(aty,bty,_) ->
            if denv.shortConstraints then 
                [wordL "delegate"]
            else
                [layoutTypeAppWithInfoAndPrec denv env (wordL "delegate") 2 true [aty;bty] |> longConstraintPrefix]
        | TyparConstraint.SupportsNull _ ->
            [wordL "null" |> longConstraintPrefix]
        | TyparConstraint.IsNonNullableStruct _ ->
            if denv.shortConstraints then 
                [wordL "value type"]
            else
                [wordL "struct" |> longConstraintPrefix]
        | TyparConstraint.IsUnmanaged _ ->
            if denv.shortConstraints then
                [wordL "unmanaged"]
            else
                [wordL "unmanaged" |> longConstraintPrefix]
        | TyparConstraint.IsReferenceType _ ->
            if denv.shortConstraints then 
                [wordL "reference type"]
            else
                [wordL "not struct" |> longConstraintPrefix]
        | TyparConstraint.SimpleChoice(tys,_) ->
            [bracketL (sepListL (sepL "|") (List.map (layoutTypeWithInfo denv env) tys)) |> longConstraintPrefix]
        | TyparConstraint.RequiresDefaultConstructor _ -> 
            if denv.shortConstraints then 
                [wordL "default constructor"]
            else
                [bracketL (wordL "new : unit -> " ^^ (layoutTyparRefWithInfo denv env tp)) |> longConstraintPrefix]

    and private layoutTraitWithInfo denv env (TTrait(tys,nm,memFlags,argtys,rty,_)) =
        let nm = DemangleOperatorName nm
        if denv.shortConstraints then 
            wordL ("member "^nm)
        else
            let rty = GetFSharpViewOfReturnType denv.g rty
            let stat = layoutMemberFlags memFlags
            let tys = ListSet.setify (typeEquiv denv.g) tys
            let tysL = 
                match tys with 
                | [ty] -> layoutTypeWithInfo denv env ty 
                | tys -> bracketL (layoutTypesWithInfoAndPrec denv env 2 (wordL "or") tys)
            tysL ^^ wordL ":"  ---  
                bracketL (stat ++ wordL nm ^^ wordL ":" ---
                        ((layoutTypesWithInfoAndPrec denv env 2 (wordL "*") argtys --- wordL "->") --- (layoutTypeWithInfo denv env rty)))


    /// Layout a unit expression 
    and private layoutMeasure denv unt =
        let sortVars vs = vs |> List.sortBy (fun (v:Typar,_) -> v.DisplayName) 
        let sortCons cs = cs |> List.sortBy (fun (c:TyconRef,_) -> c.DisplayName) 
        let negvs,posvs = ListMeasureVarOccsWithNonZeroExponents              unt |> sortVars |> List.partition (fun (_,e) -> e<0)
        let negcs,poscs = ListMeasureConOccsWithNonZeroExponents denv.g false unt |> sortCons |> List.partition (fun (_,e) -> e<0)
        let unparL uv = layoutTyparRef denv uv
        let unconL tc = layoutTyconRef denv tc
        let prefix = spaceListL  (List.map (fun (v,e) -> if e=1  then unparL v else unparL v -- wordL (sprintf "^ %d" e)) posvs @
                                  List.map (fun (c,e) -> if e=1  then unconL c else unconL c -- wordL (sprintf "^ %d" e)) poscs)
        let postfix = spaceListL (List.map (fun (v,e) -> if e= -1 then unparL v else unparL v -- wordL (sprintf "^ %d" (-e))) negvs @
                                  List.map (fun (c,e) -> if e= -1 then unconL c else unconL c -- wordL (sprintf "^ %d" (-e))) negcs)
        match (negvs,negcs) with 
        | [],[] -> (match posvs,poscs with [],[] -> wordL "1" | _ -> prefix)
        | _ -> prefix ^^ sepL "/" ^^ (if List.length negvs + List.length negcs > 1 then sepL "(" ^^ postfix ^^ sepL ")" else postfix)

    /// Layout type arguments, either NAME<ty,...,ty> or (ty,...,ty) NAME *)
    and private layoutTypeAppWithInfoAndPrec denv env tcL prec prefix args =
        if prefix  then 
            match args with
            | [] -> tcL
            | [arg] -> tcL ^^ sepL "<" ^^ (layoutTypeWithInfoAndPrec denv env 4 arg) ^^ rightL ">"
            | args -> bracketIfL (prec <= 1) (tcL ^^ angleL (layoutTypesWithInfoAndPrec denv env 2 (sepL ",") args))
        else
            match args with
            | []    -> tcL
            | [arg] ->  layoutTypeWithInfoAndPrec denv env 2 arg ^^ tcL
            | args  -> bracketIfL (prec <= 1) (bracketL (layoutTypesWithInfoAndPrec denv env 2 (sepL ",") args) --- tcL)

    /// Layout a type, taking precedence into account to insert brackets where needed *)
    and layoutTypeWithInfoAndPrec denv env prec typ =

        match stripTyparEqns typ with 

        // Layout a type application 
        | TType_app (tc,args) when tc.IsMeasureableReprTycon && List.forall (isDimensionless denv.g) args ->
          layoutTypeWithInfoAndPrec denv env prec (reduceTyconRefMeasureableOrProvided denv.g tc args)

        | TType_app (tc,args) -> 
          layoutTypeAppWithInfoAndPrec denv env (layoutTyconRef denv tc) prec tc.IsPrefixDisplay args 

        | TType_ucase (UCRef(tc,_),args) -> 
          layoutTypeAppWithInfoAndPrec denv env (layoutTyconRef denv tc) prec tc.IsPrefixDisplay args 

        // Layout a tuple type 
        | TType_tuple t ->
            bracketIfL (prec <= 2) (layoutTypesWithInfoAndPrec denv env 2 (wordL "*") t)

        // Layout a first-class generic type. 
        | TType_forall (tps,tau) ->
            let tauL = layoutTypeWithInfoAndPrec denv env prec tau
            match tps with 
            | []  -> tauL
            | [h] -> layoutTyparRefWithInfo denv env h ^^ rightL "." --- tauL
            | (h::t) -> spaceListL (List.map (layoutTyparRefWithInfo denv env) (h::t)) ^^ rightL "." --- tauL

        // Layout a function type. 
        | TType_fun _ ->
            let rec loop soFarL ty = 
              match stripTyparEqns ty with 
              | TType_fun (dty,rty) -> loop (soFarL --- (layoutTypeWithInfoAndPrec denv env 4 dty ^^ wordL "->")) rty
              | rty -> soFarL --- layoutTypeWithInfoAndPrec denv env 5 rty
            bracketIfL (prec <= 4) (loop emptyL typ)

        // Layout a type variable . 
        | TType_var r ->
            layoutTyparRefWithInfo denv env r

        | TType_measure unt -> layoutMeasure denv unt

    /// Layout a list of types, separated with the given separator, either '*' or ','
    and private layoutTypesWithInfoAndPrec denv env prec sep typl = 
        sepListL sep (List.map (layoutTypeWithInfoAndPrec denv env prec) typl)

    /// Layout a single type, taking TypeSimplificationInfo into account 
    and private layoutTypeWithInfo denv env typ = 
        layoutTypeWithInfoAndPrec denv env 5 typ

    and layoutType denv typ  = 
        layoutTypeWithInfo denv SimplifyTypes.typeSimplificationInfo0 typ

    /// Layout a single type used as the type of a member or value 
    let layoutTopType denv env argInfos rty cxs =
        // Parenthesize the return type to match the topValInfo 
        let rtyL  = layoutTypeWithInfoAndPrec denv env 4 rty
        let cxsL = layoutConstraintsWithInfo denv env cxs
        match argInfos with
        | [] -> rtyL --- cxsL
        | _  -> 

            // Format each argument, including its name and type 
            let argL (ty,argInfo: ArgReprInfo) = 
                   
                // Detect an optional argument 
                let isOptionalArg = HasFSharpAttribute denv.g denv.g.attrib_OptionalArgumentAttribute argInfo.Attribs
                let isParamArray = HasFSharpAttribute denv.g denv.g.attrib_ParamArrayAttribute argInfo.Attribs
                match argInfo.Name, isOptionalArg, isParamArray, tryDestOptionTy denv.g ty with 
                // Layout an optional argument 
                | Some(id), true, _, Some ty -> 
                    leftL ("?"^id.idText) ^^ sepL ":" ^^ layoutTypeWithInfoAndPrec denv env 2 ty 
                // Layout an unnamed argument 
                | None, _,_, _ -> 
                    layoutTypeWithInfoAndPrec denv env 2 ty
                // Layout a named argument 
                | Some id,_,isParamArray,_ -> 
                    leftL ((if isParamArray then "params " else "") + id.idText) ^^ sepL ":" ^^ layoutTypeWithInfoAndPrec denv env 2 ty
                        
            let delimitReturnValue = if denv.useColonForReturnType then ":" else "->"

            let allArgsL = 
                argInfos 
                |> List.mapSquared argL 
                |> List.map (sepListL (wordL "*"))
                |> List.map (fun x -> (x ^^ wordL delimitReturnValue)) 
            (List.foldBack (---) allArgsL rtyL) --- cxsL

    /// Layout type parameters
    let layoutTyparDecls denv nmL prefix (typars: Typars) =
        let env = SimplifyTypes.typeSimplificationInfo0 
        let tpcs = typars |> List.collect (fun tp -> List.map (fun tpc -> tp,tpc) tp.Constraints) 
        match typars,tpcs with 
        | [],[]  -> 
            nmL

        | [h],[] when not prefix -> 
            layoutTyparRefWithInfo denv env h --- nmL

        | _ -> 
            let tpcsL = layoutConstraintsWithInfo denv env tpcs
            let coreL = sepListL (sepL ",") (List.map (layoutTyparRefWithInfo denv env) typars)
            (if prefix || nonNil(tpcs) then nmL ^^ angleL (coreL --- tpcsL) else bracketL coreL --- nmL)


    let layoutTyparConstraint denv typars = 
        match layoutConstraintWithInfo denv SimplifyTypes.typeSimplificationInfo0 typars  with 
        | h::_ -> h 
        | [] -> emptyL

    let layoutPrettifiedTypes denv taus =
        let _,ptaus,cxs = PrettyTypes.PrettifyTypesN denv.g taus
        let env = SimplifyTypes.CollectInfo true ptaus cxs
        List.map (layoutTypeWithInfo denv env) ptaus,layoutConstraintsWithInfo denv env env.postfixConstraints

    let layoutPrettifiedTypesAndConstraints denv argInfos tau cxs = 
        let env = SimplifyTypes.CollectInfo true (tau:: List.collect (List.map fst) argInfos) cxs
        layoutTopType denv env argInfos tau env.postfixConstraints

    let layoutPrettifiedTypeAndConstraints denv argInfos tau = 
        let _,(argInfos,tau),cxs = PrettyTypes.PrettifyTypesN1 denv.g (argInfos,tau)
        layoutPrettifiedTypesAndConstraints denv [argInfos] tau cxs

    let layoutMemberTypeAndConstraints denv argInfos retTy parentTyparTys = 
        let _,(parentTyparTys,argInfos,retTy),cxs = PrettyTypes.PrettifyTypesNM1 denv.g (parentTyparTys,argInfos,retTy)
        // Filter out the parent typars, which don't get shown in the member signature 
        let cxs = cxs |> List.filter (fun (tp,_) -> not (parentTyparTys |> List.exists (fun ty -> isTyparTy denv.g ty && typarEq tp (destTyparTy denv.g ty)))) 
        layoutPrettifiedTypesAndConstraints denv argInfos retTy cxs

    // Layout: type spec - class, datatype, record, abbrev 

    let private layoutMemberTypeCore denv memberToParentInst (methTypars: Typars,argInfos,retTy) = 
        let niceMethodTypars, allTyparInst = 
            let methTyparNames = methTypars |> List.mapi (fun i tp -> if (PrettyTypes.NeedsPrettyTyparName tp) then sprintf "a%d" (List.length memberToParentInst + i) else tp.Name)
            PrettyTypes.NewPrettyTypars memberToParentInst methTypars methTyparNames

        let retTy = instType allTyparInst retTy
        let argInfos = argInfos |> List.map (fun infos -> if isNil infos then [(denv.g.unit_ty,ValReprInfo.unnamedTopArg1)] else infos |> List.map (map1Of2 (instType allTyparInst))) 

        // Also format dummy types corresponding to any type variables on the container to make sure they 
        // aren't chosen as names for displayed variables. 
        let memberParentTypars = List.map fst memberToParentInst
        let parentTyparTys = List.map (mkTyparTy >> instType allTyparInst) memberParentTypars

        niceMethodTypars,layoutMemberTypeAndConstraints denv argInfos retTy parentTyparTys

    let layoutMemberType denv v argInfos retTy = 
        match PartitionValRefTypars denv.g v with
        | Some(_,_,memberMethodTypars,memberToParentInst,_) ->  
            layoutMemberTypeCore denv memberToParentInst (memberMethodTypars, argInfos, retTy)
        | None -> 
            [],layoutPrettifiedTypeAndConstraints denv (List.concat argInfos) retTy 

    let layoutMemberSig denv  (memberToParentInst,nm,methTypars,argInfos,retTy) = 
        let niceMethodTypars,tauL = layoutMemberTypeCore denv memberToParentInst (methTypars, argInfos, retTy)
        let nameL = 
            let nameL = wordL (DemangleOperatorName nm)
            let nameL = if denv.showTyparBinding then layoutTyparDecls denv nameL true niceMethodTypars else nameL
            nameL
        nameL ^^ wordL ":" ^^ tauL


    let layoutPrettyType denv typ = 
        let _,typ,cxs = PrettyTypes.PrettifyTypes1 denv.g typ
        let env = SimplifyTypes.CollectInfo true [typ] cxs
        let cxsL = layoutConstraintsWithInfo denv env env.postfixConstraints
        layoutTypeWithInfoAndPrec denv env 2 typ  --- cxsL


/// Printing TAST objects
module private PrintTastMemberOrVals = 
    open PrintTypes
    let private layoutMember denv (v:Val) = 
        let v = mkLocalValRef v
        let membInfo = Option.get v.MemberInfo
        let stat = PrintTypes.layoutMemberFlags membInfo.MemberFlags
        let _tps,argInfos,rty,_ = GetTypeOfMemberInFSharpForm denv.g v
        let mkNameL niceMethodTypars name =       
            let name  = DemangleOperatorName name
            let nameL = wordL name 
            let nameL = 
                if denv.showMemberContainers then 
                    layoutTyconRef denv v.MemberApparentParent ^^ sepL "." ^^ nameL
                else 
                    nameL
            let nameL = if denv.showTyparBinding then layoutTyparDecls denv nameL true niceMethodTypars else nameL
            let nameL = layoutAccessibility denv v.Accessibility nameL
            nameL

        match membInfo.MemberFlags.MemberKind with 
        | MemberKind.Member -> 
            let niceMethodTypars,tauL = layoutMemberType denv v argInfos rty
            let nameL = mkNameL niceMethodTypars v.LogicalName
            stat --- (nameL ^^ wordL ":" ^^ tauL)
        | MemberKind.ClassConstructor  
        | MemberKind.Constructor -> 
            let _,tauL = layoutMemberType denv v argInfos rty
            let newL = layoutAccessibility denv v.Accessibility (wordL "new")
            stat ++ newL ^^ wordL ":" ^^ tauL
        | MemberKind.PropertyGetSet -> stat
        | MemberKind.PropertyGet -> 
            if isNil argInfos then 
                // use error recovery because intellisense on an incomplete file will show this
                errorR(Error(FSComp.SR.tastInvalidFormForPropertyGetter(),v.Id.idRange));
                stat --- wordL v.PropertyName --- wordL "with get"
            else
                let argInfos = 
                    match argInfos with 
                    | [[(ty,_)]] when isUnitTy denv.g ty -> []
                    | _ -> argInfos

                let niceMethodTypars,tauL = layoutMemberType denv v argInfos rty
                let nameL = mkNameL niceMethodTypars v.PropertyName
                stat --- (nameL ^^ wordL ":" ^^ (if isNil argInfos then tauL else tauL --- wordL "with get"))
        | MemberKind.PropertySet -> 
            if argInfos.Length <> 1 || isNil argInfos.Head then 
                // use error recovery because intellisense on an incomplete file will show this
                errorR(Error(FSComp.SR.tastInvalidFormForPropertySetter(),v.Id.idRange));
                stat --- wordL v.PropertyName --- wordL "with set"
            else 
                let argInfos,valueInfo = List.frontAndBack argInfos.Head
                let niceMethodTypars,tauL = layoutMemberType denv v (if isNil argInfos then [] else [argInfos]) (fst valueInfo)
                let nameL = mkNameL niceMethodTypars v.PropertyName
                stat --- (nameL ^^ wordL ":" ^^ (tauL --- wordL "with set"))

    let private layoutNonMemberVal denv  (tps,v:Val,tau,cxs) =
        let env = SimplifyTypes.CollectInfo true [tau] cxs
        let cxs = env.postfixConstraints
        let argInfos,rty = GetTopTauTypeInFSharpForm denv.g (arityOfVal v).ArgInfos tau v.Range
        let nameL = wordL v.DisplayName 
        let nameL = layoutAccessibility denv v.Accessibility nameL
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
                layoutTyparDecls denv nameL true tps 
            else nameL
        let valAndTypeL = (wordL "val"  ^^ typarBindingsL --- wordL ":") --- layoutTopType denv env argInfos rty cxs
        match denv.generatedValueLayout v with
          | None      -> valAndTypeL
          | Some rhsL -> (valAndTypeL ++ wordL "=") --- rhsL

    let layoutValOrMember denv  (v:Val) =
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
                layoutNonMemberVal denv (ptps,v,ptau,cxs)
            | Some _ -> 
                layoutMember denv v
        layoutAttribs denv TyparKind.Type v.Attribs vL

let layoutMemberSig denv x       = x |> PrintTypes.layoutMemberSig denv 
let layoutTyparConstraint denv x = x |> PrintTypes.layoutTyparConstraint denv 
let outputTy denv os x           = x |> PrintTypes.layoutType denv |> bufferL os  
let outputTypars denv nm os x    = x |> PrintTypes.layoutTyparDecls denv  (wordL nm) true  |> bufferL os
let outputTyconRef denv os x     = x |> PrintTypes.layoutTyconRef denv |> bufferL os    
let layoutConst g ty c = PrintTypes.layoutConst g ty c
let layoutPrettifiedTypeAndConstraints denv argInfos tau = PrintTypes.layoutPrettifiedTypeAndConstraints denv argInfos tau

//-------------------------------------------------------------------------

/// Printing info objects
module InfoMemberPrinting = 

    /// Format the arguments of a method to a buffer. 
    ///
    /// This uses somewhat "old fashioned" printf-style buffer printing.
    let formatParamDataToBuffer denv os (ParamData(isParamArray,_isOutArg,optArgInfo,nmOpt,pty)) =
        let isOptArg = optArgInfo.IsOptional
        match isParamArray, nmOpt, isOptArg, tryDestOptionTy denv.g pty with 
        // Layout an optional argument 
        | _, Some nm, true, ptyOpt -> 
            // detect parameter type, if ptyOpt is None - this is .NET style optional argument
            let pty = defaultArg ptyOpt pty
            bprintf os "?%s: " nm 
            outputTy denv os pty
        // Layout an unnamed argument 
        | _, None, _,_ -> 
            outputTy denv os pty;
        // Layout a named argument 
        | true, Some nm,_,_ -> 
            bprintf os "params %s: " nm 
            outputTy denv os pty
        | false, Some nm,_,_ -> 
            bprintf os "%s: " nm 
            outputTy denv os pty

    /// Format a method info using "F# style".
    //
    // That is, this style:
    //          new : argName1:argType1 * ... * argNameN:argTypeN -> retType
    //          Method : argName1:argType1 * ... * argNameN:argTypeN -> retType
    let private formatMethInfoToBufferFSharpStyle amap m denv os (minfo:MethInfo) minst =
        if not minfo.IsConstructor && not minfo.IsInstance then  bprintf os "static "
        if minfo.IsConstructor then  
          bprintf os "new : "
        else
          bprintf os "member "
          outputTypars denv minfo.LogicalName os minfo.FormalMethodTypars;
          bprintf os " : " 
        let paramDatas = minfo.GetParamDatas(amap, m, minst)
        if (List.concat paramDatas).Length = 0 then 
           bprintf os "unit"
        paramDatas |> List.iteri (fun i datas -> 
            if i > 0 then bprintf os " -> "; 
            datas |> List.iteri (fun j arg -> 
              if j > 0 then bprintf os " * "; 
              formatParamDataToBuffer denv os arg))
        let retTy = minfo.GetFSharpReturnTy(amap, m, minst)
        bprintf os " -> "  
        outputTy denv os retTy

    /// Format a method info using "half C# style".
    //
    // That is, this style:
    //          Container(argName1:argType1, ..., argNameN:argTypeN) : retType
    //          Container.Method(argName1:argType1, ..., argNameN:argTypeN) : retType
    let private formatMethInfoToBufferCSharpStyle amap m denv os (minfo:MethInfo) minst =
        let retTy = minfo.GetFSharpReturnTy(amap, m, minst)
        outputTyconRef denv os (tcrefOfAppTy amap.g minfo.EnclosingType);
        if minfo.IsConstructor then  
          bprintf os "("
        else
          bprintf os "." 
          outputTypars denv minfo.LogicalName os minfo.FormalMethodTypars;
          bprintf os "(" 
        let paramDatas = minfo.GetParamDatas (amap, m, minst)
        paramDatas |> List.iter (List.iteri (fun i arg -> 
              if i > 0 then bprintf os ", "; 
              formatParamDataToBuffer denv os arg))
        bprintf os ") : "  
        outputTy denv os retTy


           // Prettify this baby
    let prettifyILMethInfo (amap:Import.ImportMap) m (minfo:MethInfo) = function
        | ILMethInfo(ILTypeInfo(tcref,tref,tinst,tdef),extInfo,mdef,_) ->  
            let _,tys,_ = PrettyTypes.PrettifyTypesN amap.g (tinst @ minfo.FormalMethodInst)
            let tinst,minst = List.chop tinst.Length tys
            let minfo = ILMethInfo.Create (amap, m, ILTypeInfo(tcref,tref,tinst,tdef), extInfo, None, mdef)
            minfo,minst
        | ILFSMethInfo _ as ilminfo ->
            ILMeth(amap.g,ilminfo,None),[]


    /// Format a method to a buffer using "standalone" display style. 
    /// For example, these are the formats used when printing signatures of methods that have not been overriden,
    /// and the format used when showing the individual member in QuickInfo and DeclarationInfo.
    /// The formats differ between .NET/provided methods and F# methods. Surprisingly people don't really seem 
    /// to notice this, or they find it helpful. It feels that moving from this position should not be done lightly.
    //
    // For F# members, we use layoutValOrMemberm which gives:
    //          new : unit -> retType
    //          new : argName1:argType1 * ... * argNameN:argTypeN -> retType
    //          Container.Method : unit -> retType
    //          Container.Method : argName1:argType1 * ... * argNameN:argTypeN -> retType
    //
    // For C# and provided members:
    //          Container(argName1:argType1, ..., argNameN:argTypeN) : retType
    //          Container.Method(argName1:argType1, ..., argNameN:argTypeN) : retType
    //
    let formatMethInfoToBufferFreeStyle amap m denv os minfo =
        match minfo with 
        | DefaultStructCtor(g,_typ) -> 
            outputTyconRef denv os (tcrefOfAppTy g minfo.EnclosingType)
            bprintf os "()"
        | FSMeth(_,_,vref,_) -> 
            vref.Deref |> PrintTastMemberOrVals.layoutValOrMember { denv with showMemberContainers=true; } |> bufferL os
        | ILMeth(_,ilminfo,_) -> 
            // Prettify first
            let minfo,minst = prettifyILMethInfo amap m minfo ilminfo
            formatMethInfoToBufferCSharpStyle amap m denv os minfo minst
    #if EXTENSIONTYPING
        | ProvidedMeth _  -> 
            formatMethInfoToBufferCSharpStyle amap m denv os minfo minfo.FormalMethodInst
    #endif

    /// Format a method to a layotu (actually just containing a string) using "free style" (aka "standalone"). 
    let layoutMethInfoFSharpStyle amap m denv (minfo: MethInfo) = 
        wordL (bufs (fun buf -> formatMethInfoToBufferFSharpStyle amap m denv buf minfo minfo.FormalMethodInst))


//-------------------------------------------------------------------------

/// Printing TAST objects
module private TastDefinitionPrinting = 
    open PrintTypes

    let layoutExtensionMember denv (v:Val) =
        let tycon = v.MemberApparentParent.Deref
        let nameL = wordL tycon.DisplayName
        let nameL = layoutAccessibility denv tycon.Accessibility nameL // "type-accessibility"
        let tps =
            match PartitionValTypars denv.g v with
              | Some(_,memberParentTypars,_,_,_) -> memberParentTypars
              | None -> []          
        let lhsL = wordL "type" ^^ layoutTyparDecls denv nameL tycon.IsPrefixDisplay tps
        (lhsL ^^ wordL "with") @@-- (PrintTastMemberOrVals.layoutValOrMember denv v)

    let layoutExtensionMembers denv vs =
        aboveListL (List.map (layoutExtensionMember denv) vs)

    let layoutUnionCaseArgTypes denv argtys = 
        sepListL (wordL "*") (List.map (layoutTypeWithInfoAndPrec denv SimplifyTypes.typeSimplificationInfo0 2) argtys)

    let layoutUnionCase denv  prefixL ucase =
        let nmL = wordL (DemangleOperatorName ucase.Id.idText) 
        //let nmL = layoutAccessibility denv ucase.Accessibility nmL
        match ucase.RecdFields |> List.map (fun rfld -> rfld.FormalType) with
        | []     -> (prefixL ^^ nmL)
        | argtys -> (prefixL ^^ nmL ^^ wordL "of") --- layoutUnionCaseArgTypes denv argtys

    let layoutUnionCases denv  ucases =
        let prefixL = wordL "|" // See bug://2964 - always prefix in case preceeded by accessibility modifier
        List.map (layoutUnionCase denv prefixL) ucases
        
    let layoutRecdField addAccess denv  (fld:RecdField) =
        let lhs = wordL fld.Name 
        let lhs = (if addAccess then layoutAccessibility denv fld.Accessibility lhs else lhs)
        let lhs = if fld.IsMutable then wordL "mutable" --- lhs else lhs
        (lhs ^^ rightL ":") --- layoutType denv fld.FormalType

    /// When to force a break? "type tyname = <HERE> repn"
    /// When repn is class or datatype constructors (not single one).
    let breakTypeDefnEqn repr =
        match repr with 
        | TFsObjModelRepr _ -> true
        | TFiniteUnionRepr r    -> r.CasesTable.UnionCasesAsList.Length > 1
        | TRecdRepr _ -> true
        | TAsmRepr _ 
        | TILObjModelRepr _  
        | TMeasureableRepr _ 
#if EXTENSIONTYPING
        | TProvidedTypeExtensionPoint _
        | TProvidedNamespaceExtensionPoint _
#endif
        | TNoRepr  -> false


              
#if EXTENSIONTYPING
    let private layoutILFieldInfo denv amap m (e: ILFieldInfo) =
        let staticL = if e.IsStatic then wordL "static" else emptyL
        let nameL = wordL (adjustILName e.FieldName)
        let typL = layoutType denv (e.FieldType(amap,m))
        staticL ^^ wordL "val" ^^ nameL ^^ wordL ":" ^^ typL     

    let private layoutEventInfo denv amap m (e: EventInfo) =
        let staticL = if e.IsStatic then wordL "static" else emptyL
        let nameL = wordL (adjustILName e.EventName)
        let typL = layoutType denv (e.GetDelegateType(amap,m))
        staticL ^^ wordL "event" ^^ nameL ^^ wordL ":" ^^ typL     
       
    let private layoutPropInfo denv amap m (p : PropInfo) =
        let staticL =  if p.IsStatic then wordL "static" else emptyL
        let nameL   = wordL (adjustILName p.PropertyName)
            
        let typL = layoutType denv (p.GetPropertyType(amap,m)) // shouldn't happen
                
        let specGetSetL =
            match p.HasGetter, p.HasSetter with
            |   false,false | true,false -> emptyL
            |   false, true -> wordL "with" ^^ wordL " set"
            |   true, true -> wordL "with" ^^ wordL "get," ^^ wordL "set"

        staticL ^^ wordL "member" ^^ nameL ^^ wordL ":" ^^ typL ^^ specGetSetL

    /// Another re-implementation of type printing, this time based off provided info objects.
    let layoutProvidedTycon (denv:DisplayEnv) (infoReader:InfoReader) ad m start lhsL ty =
      let g = denv.g
      let tcref,_ = destAppTy g ty

      if isEnumTy g ty then 
        let fieldLs = 
            infoReader.GetILFieldInfosOfType (None,ad,m,ty) 
            |> List.filter (fun x -> x.FieldName <> "value__")
            |> List.map (fun x -> PrintIL.layoutILEnumDefParts x.FieldName x.LiteralValue)
            |> aboveListL
        (lhsL ^^ wordL "=") @@-- fieldLs
      else
        let amap = infoReader.amap
        let sortKey (v:MethInfo) = 
            (not v.IsConstructor, 
              not v.IsInstance,  // instance first
              v.DisplayName,            // sort by name 
              List.sum v.NumArgs ,  // sort by #curried
              v.NumArgs.Length)  // sort by arity 
        let ctors = GetIntrinsicConstructorInfosOfType infoReader m ty 
        let meths = GetImmediateIntrinsicMethInfosOfType (None,ad) g amap m ty 
        let iimplsLs = 
            if suppressInheritanceAndInterfacesForTyInSimplifiedDisplays g amap m ty then 
                []
            else 
                GetImmediateInterfacesOfType g amap m ty |> List.map (fun ity -> wordL (if isInterfaceTy g ty then "inherit" else "interface") --- layoutType denv ity)

        let props = 
            //GetImmediateIntrinsicPropInfosOfType  (None,ad) g amap m ty 
            GetIntrinsicPropInfosOfType infoReader (None,ad,AllowMultiIntfInstantiations.No)  PreferOverrides m ty 

        let events = 
            infoReader.GetEventInfosOfType(None,ad,m,ty) 

        let impliedNames = 
            try 
                Set.ofList [ for p in props do 
                                if p.HasGetter then yield p.GetterMethod.DisplayName
                                if p.HasSetter then yield p.SetterMethod.DisplayName  
                             for e in events do 
                                yield e.GetAddMethod().DisplayName 
                                yield e.GetRemoveMethod().DisplayName ]
            with _ -> Set.empty

        let ctorLs    = 
            ctors 
            |> shrinkOverloads (InfoMemberPrinting.layoutMethInfoFSharpStyle amap m denv) (fun _ xL -> xL) 

        let methLs    = 
            meths 
            |> List.filter (fun md -> not (impliedNames.Contains md.DisplayName))
            |> List.groupBy (fun md -> md.DisplayName)
            |> List.collect (fun (_,group) -> shrinkOverloads (InfoMemberPrinting.layoutMethInfoFSharpStyle amap m denv) (fun x xL -> (sortKey x, xL)) group)

        let fieldLs = 
            infoReader.GetILFieldInfosOfType (None,ad,m,ty) 
            |> List.map (fun x -> (true,x.IsStatic,x.FieldName,0,0),layoutILFieldInfo denv amap m x)

    
        let propLs = 
            props
            |> List.map (fun x -> (true,x.IsStatic,x.PropertyName,0,0),layoutPropInfo denv amap m x)

        let eventLs = 
            events
            |> List.map (fun x -> (true,x.IsStatic,x.EventName,0,0), layoutEventInfo denv amap m x)

        let membLs = (methLs @ fieldLs @ propLs @ eventLs) |> List.sortBy fst  |> List.map snd

        let nestedTypeLs  = 
          match tcref.TypeReprInfo with 
          | TProvidedTypeExtensionPoint info ->
                [ 
                    for nestedType in info.ProvidedType.PApplyArray((fun sty -> sty.GetNestedTypes()), "GetNestedTypes", m) do 
                        yield nestedType.PUntaint((fun t -> t.Name), m)
                ] 
                |> List.sort
                |> List.map (fun t -> wordL "nested" ^^ wordL "type" ^^ wordL t)
          | _ -> 
              []

        let inherits = 
            if suppressInheritanceAndInterfacesForTyInSimplifiedDisplays g amap m ty then 
                []
            else
                match GetSuperTypeOfType g amap m ty with 
                | Some super when not (isObjTy g super) -> [wordL  "inherit" ^^ (layoutType denv super)] 
                | _ -> []

        let erasedL = 
#if SHOW_ERASURE
            if tcref.IsProvidedErasedTycon then 
                [ wordL ""; wordL (FSComp.SR.erasedTo()) ^^ PrintIL.layoutILTypeRef { denv with shortTypeNames = false } tcref.CompiledRepresentationForNamedType; wordL "" ] 
            else 
#endif
                []
        let decls = inherits @ iimplsLs @ ctorLs @ membLs @ nestedTypeLs @ erasedL
        if List.isEmpty decls then
            lhsL
        else
            let declsL = (inherits @ iimplsLs @ ctorLs @ membLs @ nestedTypeLs @ erasedL) |> applyMaxMembers denv.maxMembers |> aboveListL 
            let rhsL = match start with Some s -> (wordL s @@-- declsL) @@ wordL "end" | None -> declsL
            (lhsL ^^ wordL "=") @@-- rhsL
#endif

    let layoutTycon (denv:DisplayEnv) (infoReader:InfoReader) ad m simplified typewordL (tycon:Tycon) =
      let g = denv.g
      let nameL  = wordL tycon.DisplayName
      let nameL = layoutAccessibility denv tycon.Accessibility nameL 
      let denv = denv.AddAccessibility tycon.Accessibility 
      let lhsL =
          let tps = tycon.TyparsNoRange
          let tpsL = layoutTyparDecls denv nameL tycon.IsPrefixDisplay tps
          typewordL ^^ tpsL
      let _,ty = generalizeTyconRef (mkLocalTyconRef tycon) 
      let start = 
          if isClassTy g ty then (if simplified then None else Some "class" )
          elif isInterfaceTy g ty then Some "interface" 
          elif isStructTy g ty then Some "struct" 
          else None


#if EXTENSIONTYPING
      match tycon.IsProvided with 
      | true -> 
          layoutProvidedTycon denv infoReader ad m start lhsL ty 
      | false -> 
#else
      ignore (infoReader, ad, m)
#endif
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
              |> List.filter (fun v -> denv.showObsoleteMembers || not (HasFSharpAttribute denv.g denv.g.attrib_SystemObsolete v.Attribs))
          // sort 
          let sortKey (v:ValRef) = (not v.IsConstructor,    // constructors before others 
                                    v.Id.idText,            // sort by name 
                                    (if v.IsCompiledAsTopLevel then v.ValReprInfo.Value.NumCurriedArgs else 0),  // sort by #curried
                                    (if v.IsCompiledAsTopLevel then v.ValReprInfo.Value.AritiesOfArgs  else [])  // sort by arity 
                                    )            
          let adhoc = adhoc |> List.sortBy sortKey
          let iimpls = 
              match tycon.TypeReprInfo with 
              | TFsObjModelRepr r when (match r.fsobjmodel_kind with TTyconInterface -> true | _ -> false) -> []
              | _ -> tycon.ImmediateInterfacesOfFSharpTycon
          let iimpls = iimpls |> List.filter (fun (_,compgen,_) -> not compgen)
          // if TTyconInterface, the iimpls should be printed as inheritted interfaces 
          let iimplsLs = iimpls |> List.map (fun (ty,_,_) -> wordL "interface" --- layoutType denv ty)
          let adhocCtorsLs    = adhoc |> List.filter (fun v -> v.IsConstructor)                               |> List.map (fun vref -> PrintTastMemberOrVals.layoutValOrMember denv vref.Deref)
          let adhocInstanceLs = adhoc |> List.filter (fun v -> not v.IsConstructor && v.IsInstanceMember)     |> List.map (fun vref -> PrintTastMemberOrVals.layoutValOrMember denv vref.Deref)
          let adhocStaticLs   = adhoc |> List.filter (fun v -> not v.IsConstructor && not v.IsInstanceMember) |> List.map (fun vref -> PrintTastMemberOrVals.layoutValOrMember denv vref.Deref)
          iimplsLs,adhocCtorsLs,adhocInstanceLs,adhocStaticLs
      let memberLs = memberImplementLs @ memberCtorLs @ memberInstanceLs @ memberStaticLs
      let addMembersAsWithEnd reprL = 
          if isNil memberLs then reprL
          elif simplified then reprL @@ aboveListL memberLs
          else reprL @@ (wordL "with" @@-- aboveListL memberLs) @@ wordL "end"

      let reprL = 
          let repr = tycon.TypeReprInfo
          match repr with 
          | TRecdRepr _ 
          | TFiniteUnionRepr _  
          | TFsObjModelRepr _ 
          | TAsmRepr _         
          | TMeasureableRepr _
          | TILObjModelRepr _ -> 
              let brk  = nonNil memberLs || breakTypeDefnEqn repr
              let rhsL =                     
                  let addReprAccessL l = layoutAccessibility denv tycon.TypeReprAccessibility l 
                  let denv = denv.AddAccessibility tycon.TypeReprAccessibility 
                  match repr with 
                  | TRecdRepr _ ->
                      let recdFieldRefL fld = layoutRecdField false denv fld ^^ rightL ";"
                      let recdL = tycon.TrueFieldsAsList |> List.map recdFieldRefL |> applyMaxMembers denv.maxMembers |> aboveListL |> braceL
                      Some (addMembersAsWithEnd (addReprAccessL recdL))
                        
                  | TFsObjModelRepr r -> 
                      match r.fsobjmodel_kind with 
                      | TTyconDelegate (TSlotSig(_,_, _,_,paraml, rty)) ->
                          let rty = GetFSharpViewOfReturnType denv.g rty
                          Some (wordL "delegate of" --- layoutTopType denv SimplifyTypes.typeSimplificationInfo0 (paraml |> List.mapSquared (fun sp -> (sp.Type, ValReprInfo.unnamedTopArg1))) rty [])
                      | _ ->
                          match r.fsobjmodel_kind with
                          | TTyconEnum -> 
                              tycon.TrueFieldsAsList
                              |> List.map (fun f -> 
                                                match f.LiteralValue with 
                                                | None -> emptyL
                                                | Some c -> wordL "| " ^^ wordL f.Name ^^ wordL " = " ^^ layoutConst denv.g ty c)
                              |> aboveListL
                              |> Some
                          | _ -> 
                              let inherits = 
                                  match r.fsobjmodel_kind, tycon.TypeContents.tcaug_super with
                                  | TTyconClass,Some super -> [wordL  "inherit" ^^ (layoutType denv super)] 
                                  | TTyconInterface,_ -> 
                                    tycon.ImmediateInterfacesOfFSharpTycon
                                      |> List.filter (fun (_,compgen,_) -> not compgen)
                                      |> List.map (fun (ity,_,_) -> wordL  "inherit" ^^ (layoutType denv ity))
                                  | _ -> []
                              let vsprs = 
                                  tycon.MembersOfFSharpTyconSorted
                                  |> List.filter (fun v -> isNil (Option.get v.MemberInfo).ImplementedSlotSigs) 
                                  |> List.filter (fun v -> v.IsDispatchSlot)
                                  |> List.map (fun vref -> PrintTastMemberOrVals.layoutValOrMember denv vref.Deref)
                              let staticValsLs  = 
                                  tycon.TrueFieldsAsList
                                  |> List.filter (fun f -> f.IsStatic)
                                  |> List.map (fun f -> wordL "static" ^^ wordL "val" ^^ layoutRecdField true denv f)
                              let instanceValsLs  = 
                                  tycon.TrueFieldsAsList
                                  |> List.filter (fun f -> not f.IsStatic)
                                  |> List.map (fun f -> wordL "val" ^^ layoutRecdField true denv f)
                              let alldecls = inherits @ memberImplementLs @ memberCtorLs @ instanceValsLs @ vsprs @ memberInstanceLs @ staticValsLs @ memberStaticLs
                              if List.isEmpty alldecls then
                                  None
                              else
                                  let alldecls = applyMaxMembers denv.maxMembers alldecls
                                  let emptyMeasure = match tycon.TypeOrMeasureKind with TyparKind.Measure -> isNil alldecls | _ -> false
                                  if emptyMeasure then None else 
                                  let declsL = aboveListL alldecls
                                  let declsL = match start with Some s -> (wordL s @@-- declsL) @@ wordL "end" | None -> declsL
                                  Some declsL
                  | TFiniteUnionRepr _        -> 
                      let layoutUnionCases = tycon.UnionCasesAsList |> layoutUnionCases denv |> applyMaxMembers denv.maxMembers |> aboveListL
                      Some (addMembersAsWithEnd (addReprAccessL layoutUnionCases))
                  | TAsmRepr _                      -> 
                      Some (wordL "(# \"<Common IL Type Omitted>\" #)")
                  | TMeasureableRepr ty                 ->
                      Some (layoutType denv ty)
                  | TILObjModelRepr (_,_,td) -> 
                      Some (PrintIL.layoutILTypeDef denv td)
                  | _  -> None

              let brk  = match tycon.TypeReprInfo with | TILObjModelRepr _ -> true | _  -> brk
              match rhsL with 
              | None  -> lhsL
              | Some rhsL -> 
                  if brk then 
                      (lhsL ^^ wordL "=") @@-- rhsL 
                  else 
                      (lhsL ^^ wordL "=") ---  rhsL

          | _ -> 
              match tycon.TypeAbbrev with
              | None   -> 
                  addMembersAsWithEnd lhsL 
              | Some a -> 
                  (lhsL ^^ wordL "=") --- (layoutType { denv with shortTypeNames = false } a)
      layoutAttribs denv tycon.TypeOrMeasureKind tycon.Attribs reprL

    // Layout: exception definition
    let layoutExnDefn denv  (exnc:Entity) =
        let nm = exnc.LogicalName
        let nmL = wordL nm 
        let nmL = layoutAccessibility denv exnc.TypeReprAccessibility nmL
        let exnL = wordL "exception" ^^ nmL // need to tack on the Exception at the right of the name for goto definition
        let reprL = 
            match exnc.ExceptionInfo with 
            | TExnAbbrevRepr ecref -> wordL "=" --- layoutTyconRef denv ecref
            | TExnAsmRepr _     -> wordL "=" --- wordL "(# ... #)"
            | TExnNone             -> emptyL
            | TExnFresh r          -> 
                match r.TrueFieldsAsList with
                | []  -> emptyL
                | r -> wordL "of" --- layoutUnionCaseArgTypes denv (r |> List.map (fun rfld -> rfld.FormalType))

        exnL ^^ reprL

    // Layout: module spec 

    let layoutTyconDefns denv infoReader ad m  (tycons:Tycon list) =
        match tycons with 
        | [] -> emptyL
        | [h] when h.IsExceptionDecl -> layoutExnDefn denv h
        | h :: t -> 
            let x  = layoutTycon denv infoReader ad m false (wordL "type") h
            let xs = List.map (layoutTycon denv infoReader ad m false (wordL "and")) t
            aboveListL (x::xs)


//--------------------------------------------------------------------------

module private InferredSigPrinting = 
    open PrintTypes

    /// Layout the inferred signature of a compilation unit
    let layoutInferredSigOfModuleExpr showHeader denv infoReader ad m expr =

        let rec isConcreteNamespace x = 
            match x with 
            | TMDefRec(tycons,binds,mbinds,_) -> 
                nonNil tycons || not (FlatList.isEmpty binds) || (mbinds |> List.exists (fun (ModuleOrNamespaceBinding(x,_)) -> not x.IsNamespace))
            | TMDefLet _  -> true
            | TMDefDo _  -> true
            | TMDefs defs -> defs |> List.exists isConcreteNamespace 
            | TMAbstract(ModuleOrNamespaceExprWithSig(_,def,_)) -> isConcreteNamespace def

        let rec imexprLP denv  (ModuleOrNamespaceExprWithSig(_,def,_)) = imdefL denv def

        and imexprL denv (ModuleOrNamespaceExprWithSig(mty,def,m)) = imexprLP denv (ModuleOrNamespaceExprWithSig(mty,def,m))

        and imdefsL denv  x = aboveListL (x |> List.map (imdefL denv))

        and imdefL denv  x = 
            let filterVal    (v:Val) = not v.IsCompilerGenerated && isNone v.MemberInfo
            let filterExtMem (v:Val) = v.IsExtensionMember
            match x with 
            | TMDefRec(tycons,binds,mbinds,_) -> 
                  TastDefinitionPrinting.layoutTyconDefns denv infoReader ad m tycons @@ 
                  (binds |> valsOfBinds |> List.filter filterExtMem |> TastDefinitionPrinting.layoutExtensionMembers denv) @@
                  (binds |> valsOfBinds |> List.filter filterVal    |> List.map (PrintTastMemberOrVals.layoutValOrMember denv)   |> aboveListL) @@
                  (mbinds |> List.map (imbindL denv) |> aboveListL)
            | TMDefLet(bind,_) -> ([bind.Var] |> List.filter filterVal    |> List.map (PrintTastMemberOrVals.layoutValOrMember denv) |> aboveListL)
            | TMDefs defs -> imdefsL denv defs
            | TMDefDo _  -> emptyL
            | TMAbstract mexpr -> imexprLP denv mexpr
        and imbindL denv  (ModuleOrNamespaceBinding(mspec, def)) = 
            let nm =  mspec.DemangledModuleOrNamespaceName
            let innerPath = (fullCompPathOfModuleOrNamespace mspec).AccessPath
            let outerPath = mspec.CompilationPath.AccessPath

            let denv = denv.AddOpenPath (List.map fst innerPath) 
            if mspec.IsNamespace then  
                let basic = imdefL denv def
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
                let nmL   = layoutAccessibility denv mspec.Accessibility (wordL nm)
                let denv  = denv.AddAccessibility mspec.Accessibility 
                let basic = imdefL denv def
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

//--------------------------------------------------------------------------

module private PrintData = 
    open PrintTypes

    /// Nice printing of a subset of expressions, e.g. for refutations in pattern matching
    let rec dataExprL denv expr = dataExprWrapL denv false expr

    and private dataExprWrapL denv isAtomic expr =
        match expr with
        | Expr.Const (c,_,ty)                          -> 
            if isEnumTy denv.g ty then 
                wordL "enum" ^^ angleL (layoutType denv ty) ^^ bracketL (layoutConst denv.g ty c)
            else
                layoutConst denv.g ty c

        | Expr.Val (v,_,_)                         -> wordL (v.DisplayName)
        | Expr.Link rX                                 -> dataExprWrapL denv isAtomic (!rX)
        | Expr.Op (TOp.UnionCase(c),_,args,_)        -> 
            if denv.g.unionCaseRefEq c denv.g.nil_ucref then wordL "[]"
            elif denv.g.unionCaseRefEq c denv.g.cons_ucref then 
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
    and private dataExprsL denv xs = List.map (dataExprL denv) xs

let dataExprL denv expr = PrintData.dataExprL denv expr

//--------------------------------------------------------------------------
// Print Signatures/Types - output functions 
//-------------------------------------------------------------------------- 


let outputValOrMember denv os x    = x |> PrintTastMemberOrVals.layoutValOrMember denv |> bufferL os
let stringValOrMember denv x    = x |> PrintTastMemberOrVals.layoutValOrMember denv |> showL
/// Print members with a qualification showing the type they are contained in 
let outputQualifiedValOrMember denv os v = outputValOrMember { denv with showMemberContainers=true; } os v
let outputQualifiedValSpec denv os v = outputQualifiedValOrMember denv os v
let stringOfQualifiedValOrMember denv v = PrintTastMemberOrVals.layoutValOrMember { denv with showMemberContainers=true; } v |> showL
        
/// Convert a MethInfo to a string
let formatMethInfoToBufferFreeStyle amap m denv buf d = InfoMemberPrinting.formatMethInfoToBufferFreeStyle amap m denv buf d

/// Convert a MethInfo to a string
let stringOfMethInfo amap m denv d = bufs (fun buf -> InfoMemberPrinting.formatMethInfoToBufferFreeStyle amap m denv buf d)

/// Convert a ParamData to a string
let stringOfParamData denv paramData = bufs (fun buf -> InfoMemberPrinting.formatParamDataToBuffer denv buf paramData)

let outputILTypeRef         denv os x = x |> PrintIL.layoutILTypeRef denv |> bufferL os
let outputExnDef            denv os x = x |> TastDefinitionPrinting.layoutExnDefn denv |> bufferL os
let stringOfTyparConstraints denv x   = x |> PrintTypes.layoutConstraintsWithInfo denv SimplifyTypes.typeSimplificationInfo0  |> showL
let outputTycon             denv infoReader ad m (* width *) os x = TastDefinitionPrinting.layoutTycon denv infoReader ad m true (wordL "type") x (* |> Layout.squashTo width *) |>  bufferL os
let stringOfTyparConstraint denv tpc  = stringOfTyparConstraints denv [tpc]
let stringOfTy              denv x    = x |> PrintTypes.layoutType denv |> showL
let prettyStringOfTy        denv x    = x |> PrintTypes.layoutPrettyType denv |> showL
let stringOfRecdField       denv x    = x |> TastDefinitionPrinting.layoutRecdField false denv |> showL
let stringOfUnionCase       denv x    = x |> TastDefinitionPrinting.layoutUnionCase denv (wordL "|")  |> showL
let stringOfExnDef          denv x    = x |> TastDefinitionPrinting.layoutExnDefn denv |> showL

let layoutInferredSigOfModuleExpr showHeader denv infoReader ad m expr = InferredSigPrinting.layoutInferredSigOfModuleExpr showHeader denv infoReader ad m expr 
let layoutValOrMember denv v = PrintTastMemberOrVals.layoutValOrMember denv v 
let layoutPrettifiedTypes denv taus = PrintTypes.layoutPrettifiedTypes denv taus

/// Generate text for comparing two types.
///
/// If the output text is different without showing constraints and/or imperative type variable 
/// annotations and/or fully qualifying paths then don't show them! 
let minimalStringsOfTwoTypes denv t1 t2= 
    let _renamings,(t1,t2),tpcs = PrettyTypes.PrettifyTypes2 denv.g (t1,t2)
    // try denv + no type annotations 
    let attempt1 = 
        let denv = { denv with showImperativeTyparAnnotations=false; showConstraintTyparAnnotations=false  }
        let min1 = stringOfTy denv t1
        let min2 = stringOfTy denv t2
        if min1 <> min2 then Some (min1,min2,"") else None
    match attempt1 with 
    | Some res -> res
    | None -> 
    // try denv + no type annotations + show full paths
    let attempt2 = 
        let denv = { denv with showImperativeTyparAnnotations=false; showConstraintTyparAnnotations=false  }.SetOpenPaths []
        let min1 = stringOfTy denv t1
        let min2 = stringOfTy denv t2
        if min1 <> min2 then Some (min1,min2,"") else None
        // try denv 
    match attempt2 with 
    | Some res -> res
    | None -> 
    let attempt3 = 
        let min1 = stringOfTy denv t1
        let min2 = stringOfTy denv t2
        if min1 <> min2 then Some (min1,min2,stringOfTyparConstraints denv tpcs) else None
    match attempt3 with 
    | Some res -> res 
    | None -> 
    let lastAttempt = 
        // try denv + show full paths + static parameters
        let denv = denv.SetOpenPaths []
        let denv = { denv with includeStaticParametersInTypeNames=true }
        let min1 = stringOfTy denv t1
        let min2 = stringOfTy denv t2
        (min1,min2,stringOfTyparConstraints denv tpcs)  
    lastAttempt    

// Note: Always show imperative annotations when comparing value signatures 
let minimalStringsOfTwoValues denv v1 v2= 
    let denvMin = { denv with showImperativeTyparAnnotations=true; showConstraintTyparAnnotations=false  }
    let min1 = bufs (fun buf -> outputQualifiedValOrMember denvMin buf v1)
    let min2 = bufs (fun buf -> outputQualifiedValOrMember denvMin buf v2) 
    if min1 <> min2 then 
        (min1,min2) 
    else
        let denvMax = { denv with showImperativeTyparAnnotations=true; showConstraintTyparAnnotations=true  }
        let max1 = bufs (fun buf -> outputQualifiedValOrMember denvMax buf v1)
        let max2 = bufs (fun buf -> outputQualifiedValOrMember denvMax buf v2) 
        max1,max2
    
let minimalStringOfType denv ty = 
    let _, ty, _cxs = PrettyTypes.PrettifyTypes1 denv.g ty
    let denvMin = { denv with showImperativeTyparAnnotations=false; showConstraintTyparAnnotations=false  }
    showL (PrintTypes.layoutTypeWithInfoAndPrec denvMin SimplifyTypes.typeSimplificationInfo0 2 ty)


#if ASSEMBLY_AND_MODULE_SIGNATURE_PRINTING
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
let rec moduleOrNamespaceTypeLP (topLevel : bool) (denv: DisplayEnv) (mtype : ModuleOrNamespaceType) =
    // REVIEW: consider a better way to keep decls in order. 
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
      | DVal  vspec      when not topLevel -> layoutValOrMember                     denv vspec
      | DTycon tycon     when not topLevel -> tyconL                   denv (wordL "type") tycon
      | DException tycon when not topLevel -> layoutExnDefn                 denv tycon 
      | DModul mspec                       -> moduleOrNamespaceLP false denv mspec
      | _                                  -> emptyL // this catches non-namespace / modules at the top-level

    aboveListL (List.map declSpecL declSpecs)

and moduleOrNamespaceLP (topLevel: bool) (denv: DisplayEnv) (mspec: ModuleOrNamespace) = 
    let istype = mspec.ModuleOrNamespaceType.ModuleOrNamespaceKind
    let nm     = mspec.DemangledModuleOrNamespaceName
    let denv   = denv.AddOpenModuleOrNamespace (mkLocalModRef mspec) 
    let nmL    = layoutAccessibility denv mspec.Accessibility (wordL nm)
    let denv   = denv.AddAccessibility mspec.Accessibility 
    let path   = path.Add nm // tack on the current module to be used in calls to linearise all subterms
    let body   = moduleOrNamespaceTypeLP topLevel denv path  mspec.ModuleOrNamespaceType
    if istype = Namespace
        then (wordL "namespace" ^^ nmL) @@-- body
        else (wordL "module" ^^ nmL ^^ wordL "= begin") @@-- body @@ wordL "end"

let moduleOrNamespaceTypeL (denv: DisplayEnv) (mtype : ModuleOrNamespaceType) = moduleOrNamespaceTypeLP false denv Path.Empty mtype
let moduleOrNamespaceL denv mspec = moduleOrNamespaceLP false denv Path.Empty mspec
let assemblyL denv (mspec : ModuleOrNamespace) = moduleOrNamespaceTypeLP true denv Path.Empty mspec.ModuleOrNamespaceType // we seem to get the *assembly* name as an outer module, this strips this off
#endif

