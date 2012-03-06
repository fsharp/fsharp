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

// Reflection on F# values. Analyze an object to see if it the representation
// of an F# value.

namespace Microsoft.FSharp.Reflection 

open System
open System.Globalization
open System.Reflection
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Primitives.Basics

module internal Impl =

    let debug = false

    let inline checkNonNull argName (v: 'T) = 
        match box v with 
        | null -> nullArg argName 
        | _ -> ()
        
    let emptyArray arr = (Array.length arr = 0)
    let nonEmptyArray arr = Array.length arr > 0

    let isNamedType(typ:Type) = not (typ.IsArray || typ.IsByRef || typ.IsPointer)

    let equivHeadTypes (ty1:Type) (ty2:Type) = 
        isNamedType(ty1) &&
        if ty1.IsGenericType then 
          ty2.IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(ty2.GetGenericTypeDefinition())
        else 
          ty1.Equals(ty2)

    let option = typedefof<obj option>
    let func = typedefof<(obj -> obj)>

    let isOptionTy typ = equivHeadTypes typ (typeof<int option>)
    let isFunctionType typ = equivHeadTypes typ (typeof<(int -> int)>)
    let isListType typ = equivHeadTypes typ (typeof<int list>)

    //-----------------------------------------------------------------
    // GENERAL UTILITIES
    
    let instancePropertyFlags = BindingFlags.GetProperty ||| BindingFlags.Instance 
    let staticPropertyFlags = BindingFlags.GetProperty ||| BindingFlags.Static
    let staticFieldFlags = BindingFlags.GetField ||| BindingFlags.Static 
    let staticMethodFlags = BindingFlags.Static 

    let getInstancePropertyInfo (typ: Type,propName,bindingFlags) = typ.GetProperty(propName,instancePropertyFlags ||| bindingFlags) 
    let getInstancePropertyInfos (typ,names,bindingFlags) = names |> Array.map (fun nm -> getInstancePropertyInfo (typ,nm,bindingFlags)) 

    let getInstancePropertyReader (typ: Type,propName,bindingFlags) =
        match getInstancePropertyInfo(typ, propName, bindingFlags) with
        | null -> None
        | prop -> Some(fun (obj:obj) -> prop.GetValue(obj,instancePropertyFlags ||| bindingFlags,null,null,null))

    //-----------------------------------------------------------------
    // ATTRIBUTE DECOMPILATION

    let tryFindCompilationMappingAttribute (attrs:obj[]) =
      match attrs with
      | null | [| |] -> None
      | [| res |] -> Some (res :?> CompilationMappingAttribute)
      | _ -> raise <| System.InvalidOperationException (SR.GetString(SR.multipleCompilationMappings))

    let findCompilationMappingAttribute (attrs:obj[]) =
      match attrs with
      | null | [| |] -> failwith "no compilation mapping attribute"
      | [| res |] -> (res :?> CompilationMappingAttribute)
      | _ -> raise <| System.InvalidOperationException (SR.GetString(SR.multipleCompilationMappings))

    let tryFindCompilationMappingAttributeFromType       (typ:Type)        = tryFindCompilationMappingAttribute ( typ.GetCustomAttributes (typeof<CompilationMappingAttribute>,false))
    let tryFindCompilationMappingAttributeFromMemberInfo (info:MemberInfo) = tryFindCompilationMappingAttribute (info.GetCustomAttributes (typeof<CompilationMappingAttribute>,false))
    let    findCompilationMappingAttributeFromMemberInfo (info:MemberInfo) =    findCompilationMappingAttribute (info.GetCustomAttributes (typeof<CompilationMappingAttribute>,false))

    let sequenceNumberOfMember          (x: MemberInfo) = (findCompilationMappingAttributeFromMemberInfo x).SequenceNumber
    let variantNumberOfMember           (x: MemberInfo) = (findCompilationMappingAttributeFromMemberInfo x).VariantNumber

    let sortFreshArray f arr = Array.sortInPlaceWith f arr; arr

    let isFieldProperty (prop : PropertyInfo) =
        match tryFindCompilationMappingAttributeFromMemberInfo(prop) with
        | None -> false
        | Some attr -> (attr.SourceConstructFlags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.Field

    let allInstance  (ps : PropertyInfo[]) = (ps, false)
    let allStatic  (ps : PropertyInfo[]) = (ps, true)

    let tryFindSourceConstructFlagsOfType (typ:Type) = 
      match tryFindCompilationMappingAttributeFromType typ with 
      | None -> None
      | Some attr -> Some attr.SourceConstructFlags 

    //-----------------------------------------------------------------
    // UNION DECOMPILATION
    

    // Get the type where the type definitions are stored
    let getUnionCasesTyp (typ: Type, _bindingFlags) = 
#if CASES_IN_NESTED_CLASS
       let casesTyp = typ.GetNestedType("Cases", bindingFlags)
       if casesTyp.IsGenericTypeDefinition then casesTyp.MakeGenericType(typ.GetGenericArguments())
       else casesTyp
#else
       typ
#endif
            
    let getUnionTypeTagNameMap (typ:Type,bindingFlags) = 
        let enumTyp = typ.GetNestedType("Tags", bindingFlags)
        // Unions with a singleton case do not get a Tags type (since there is only one tag), hence enumTyp may be null in this case
        match enumTyp with
        | null -> 
            typ.GetMethods(staticMethodFlags ||| bindingFlags) 
            |> Array.choose (fun minfo -> 
                match tryFindCompilationMappingAttributeFromMemberInfo(minfo) with
                | None -> None
                | Some attr -> if (attr.SourceConstructFlags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.UnionCase then 
                                   let nm = minfo.Name 
                                   // chop "get_" or  "New" off the front 
                                   let nm = 
                                       if not (isListType typ) && not (isOptionTy typ) then 
                                           if   nm.Length > 4 && nm.[0..3] = "get_" then nm.[4..] 
                                           elif nm.Length > 3 && nm.[0..2] = "New" then nm.[3..]
                                           else nm
                                       else nm
                                   Some (attr.SequenceNumber, nm)
                               else
                                   None) 
        | _ -> 
            enumTyp.GetFields(staticFieldFlags ||| bindingFlags) 
            |> Array.filter (fun (f:FieldInfo) -> f.IsStatic && f.IsLiteral) 
            |> sortFreshArray (fun f1 f2 -> compare (f1.GetValue(null) :?> int) (f2.GetValue(null) :?> int))
            |> Array.map (fun tagfield -> (tagfield.GetValue(null) :?> int),tagfield.Name)

    let getUnionCaseTyp (typ: Type, tag: int, bindingFlags) = 
        let tagFields = getUnionTypeTagNameMap(typ,bindingFlags)
        let tagField = tagFields |> Array.pick (fun (i,f) -> if i = tag then Some f else None)
        if tagFields.Length = 1 then 
            typ
        else 
            let casesTyp = getUnionCasesTyp (typ, bindingFlags)
            let caseTyp = casesTyp.GetNestedType(tagField, bindingFlags) // if this is null then the union is nullary
            match caseTyp with 
            | null -> null
            | _ when caseTyp.IsGenericTypeDefinition -> caseTyp.MakeGenericType(casesTyp.GetGenericArguments())
            | _ -> caseTyp

    let getUnionTagConverter (typ:Type,bindingFlags) = 
        if isOptionTy typ then (fun tag -> match tag with 0 -> "None" | 1 -> "Some" | _ -> invalidArg "tag" (SR.GetString(SR.outOfRange)))
        elif isListType typ then (fun tag -> match tag with  0 -> "Empty" | 1 -> "Cons" | _ -> invalidArg "tag" (SR.GetString(SR.outOfRange)))
        else 
          let tagfieldmap = getUnionTypeTagNameMap (typ,bindingFlags) |> Map.ofSeq
          (fun tag -> tagfieldmap.[tag])

    let isUnionType (typ:Type,bindingFlags:BindingFlags) = 
        isOptionTy typ || 
        isListType typ || 
        match tryFindSourceConstructFlagsOfType(typ) with 
        | None -> false
        | Some(flags) ->
          (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.SumType &&
          // We see private representations only if BindingFlags.NonPublic is set
          (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
              (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
           else 
              true)

    // Check the base type - if it is also an F# type then
    // for the moment we know it is a Discriminated Union
    let isConstructorRepr (typ:Type,bindingFlags:BindingFlags) = 
        let rec get (typ:Type) = isUnionType (typ,bindingFlags) || match typ.BaseType with null -> false | b -> get b
        get typ 

    let unionTypeOfUnionCaseType (typ:Type,bindingFlags) = 
        let rec get (typ:Type) = if isUnionType (typ,bindingFlags) then typ else match typ.BaseType with null -> typ | b -> get b
        get typ 
                   
    let swap (x,y) = (y,x)

    let fieldsPropsOfUnionCase(typ:Type, tag:int, bindingFlags) =
        if isOptionTy typ then 
            match tag with 
            | 0 (* None *) -> getInstancePropertyInfos (typ,[| |],bindingFlags) 
            | 1 (* Some *) -> getInstancePropertyInfos (typ,[| "Value" |] ,bindingFlags) 
            | _ -> failwith "fieldsPropsOfUnionCase"
        elif isListType typ then 
            match tag with 
            | 0 (* Nil *)  -> getInstancePropertyInfos (typ,[| |],bindingFlags) 
            | 1 (* Cons *) -> getInstancePropertyInfos (typ,[| "Head"; "Tail" |],bindingFlags) 
            | _ -> failwith "fieldsPropsOfUnionCase"
        else
            // Lookup the type holding the fields for the union case
            let caseTyp = getUnionCaseTyp (typ, tag, bindingFlags)
            match caseTyp with 
            | null ->  [| |]
            | _ ->  caseTyp.GetProperties(instancePropertyFlags ||| bindingFlags) 
                    |> Array.filter isFieldProperty
                    |> Array.filter (fun prop -> variantNumberOfMember prop = tag)
                    |> sortFreshArray (fun p1 p2 -> compare (sequenceNumberOfMember p1) (sequenceNumberOfMember p2))
                

    let getUnionCaseRecordReader (typ:Type,tag:int,bindingFlags) = 
        let props = fieldsPropsOfUnionCase(typ,tag,bindingFlags)
        (fun (obj:obj) -> props |> Array.map (fun prop -> prop.GetValue(obj,bindingFlags,null,null,null)))

    let getUnionTagReader (typ:Type,bindingFlags) : (obj -> int) = 
        if isOptionTy typ then 
            (fun (obj:obj) -> match obj with null -> 0 | _ -> 1)
        else
            let tagMap = getUnionTypeTagNameMap (typ, bindingFlags)
            if tagMap.Length <= 1 then 
                (fun (_obj:obj) -> 0)
            else   
                match getInstancePropertyReader (typ,"Tag",bindingFlags) with
                | Some reader -> (fun (obj:obj) -> reader obj :?> int)
                | None -> 
                    (fun (obj:obj) -> 
                        let m2b = typ.GetMethod("GetTag", BindingFlags.Static ||| bindingFlags, null, [| typ |], null)
                        m2b.Invoke(null, [|obj|]) :?> int)
        
    let getUnionTagMemberInfo (typ:Type,bindingFlags) = 
        match getInstancePropertyInfo (typ,"Tag",bindingFlags) with
        | null -> (typ.GetMethod("GetTag",BindingFlags.Static ||| bindingFlags) :> MemberInfo)
        | info -> (info :> MemberInfo)

    let isUnionCaseNullary (typ:Type, tag:int, bindingFlags) = 
        let props = fieldsPropsOfUnionCase(typ, tag, bindingFlags) 
        emptyArray props

    let getUnionCaseConstructorMethod (typ:Type,tag:int,bindingFlags) = 
        let constrname = getUnionTagConverter (typ,bindingFlags) tag 
        let methname = 
            if isUnionCaseNullary (typ, tag, bindingFlags) then "get_"+constrname 
            elif isListType typ || isOptionTy typ then constrname
            else "New"+constrname 
        match typ.GetMethod(methname, BindingFlags.Static  ||| bindingFlags) with
        | null -> raise <| System.InvalidOperationException (SR.GetString1(SR.constructorForUnionCaseNotFound, methname))
        | meth -> meth

    let getUnionCaseConstructor (typ:Type,tag:int,bindingFlags) = 
        let meth = getUnionCaseConstructorMethod (typ,tag,bindingFlags)
        (fun args -> 
            meth.Invoke(null,BindingFlags.Static ||| BindingFlags.InvokeMethod ||| bindingFlags,null,args,null))

    let checkUnionType(unionType,bindingFlags) =
        checkNonNull "unionType" unionType;
        if not (isUnionType (unionType,bindingFlags)) then 
            if isUnionType (unionType,bindingFlags ||| BindingFlags.NonPublic) then 
                invalidArg "unionType" (SR.GetString1(SR.privateUnionType, unionType.FullName))
            else
                invalidArg "unionType" (SR.GetString1(SR.notAUnionType, unionType.FullName))
    let emptyObjArray : obj[] = [| |]

    //-----------------------------------------------------------------
    // TUPLE DECOMPILATION
    
    let tuple1 = typedefof<Tuple<obj>>
    let tuple2 = typedefof<obj * obj>
    let tuple3 = typedefof<obj * obj * obj>
    let tuple4 = typedefof<obj * obj * obj * obj>
    let tuple5 = typedefof<obj * obj * obj * obj * obj>
    let tuple6 = typedefof<obj * obj * obj * obj * obj * obj>
    let tuple7 = typedefof<obj * obj * obj * obj * obj * obj * obj>
    let tuple8 = typedefof<obj * obj * obj * obj * obj * obj * obj * obj>

    let isTuple1Type typ = equivHeadTypes typ tuple1
    let isTuple2Type typ = equivHeadTypes typ tuple2
    let isTuple3Type typ = equivHeadTypes typ tuple3
    let isTuple4Type typ = equivHeadTypes typ tuple4
    let isTuple5Type typ = equivHeadTypes typ tuple5
    let isTuple6Type typ = equivHeadTypes typ tuple6
    let isTuple7Type typ = equivHeadTypes typ tuple7
    let isTuple8Type typ = equivHeadTypes typ tuple8

    let isTupleType typ = 
           isTuple1Type typ
        || isTuple2Type typ
        || isTuple3Type typ 
        || isTuple4Type typ 
        || isTuple5Type typ 
        || isTuple6Type typ 
        || isTuple7Type typ 
        || isTuple8Type typ

    let maxTuple = 8
    // Which field holds the nested tuple?
    let tupleEncField = maxTuple-1
    
    let rec mkTupleType (tys: Type[]) = 
        match tys.Length with 
        | 1 -> tuple1.MakeGenericType(tys)
        | 2 -> tuple2.MakeGenericType(tys)
        | 3 -> tuple3.MakeGenericType(tys)
        | 4 -> tuple4.MakeGenericType(tys)
        | 5 -> tuple5.MakeGenericType(tys)
        | 6 -> tuple6.MakeGenericType(tys)
        | 7 -> tuple7.MakeGenericType(tys)
        | n when n >= maxTuple -> 
            let tysA = tys.[0..tupleEncField-1]
            let tysB = tys.[maxTuple-1..]
            let tyB = mkTupleType tysB
            tuple8.MakeGenericType(Array.append tysA [| tyB |])
        | _ -> invalidArg "tys" (SR.GetString(SR.invalidTupleTypes))


    let rec getTupleTypeInfo    (typ:Type) = 
      if not (isTupleType (typ) ) then invalidArg "typ" (SR.GetString1(SR.notATupleType, typ.FullName));
      let tyargs = typ.GetGenericArguments()
      if tyargs.Length = maxTuple then 
          let tysA = tyargs.[0..tupleEncField-1]
          let tyB = tyargs.[tupleEncField]
          Array.append tysA (getTupleTypeInfo tyB)
      else 
          tyargs

    let orderTupleProperties (props:PropertyInfo[]) =
        // The tuple properties are of the form:
        //   Item1
        //   ..
        //   Item1, Item2, ..., Item<maxTuple-1>
        //   Item1, Item2, ..., Item<maxTuple-1>, Rest
        // The PropertyInfo may not come back in order, so ensure ordering here.
        assert(maxTuple < 10) // Alphasort will only works for upto 9 items: Item1, Item10, Item2, Item3, ..., Item9, Rest
        let props = props |> Array.sortBy (fun p -> p.Name) // they are not always in alphabetic order
        assert(props.Length <= maxTuple)
        assert(let haveNames   = props |> Array.map (fun p -> p.Name)
               let expectNames = Array.init props.Length (fun i -> let j = i+1 // index j = 1,2,..,props.Length <= maxTuple
                                                                   if   j<maxTuple then "Item" + string j
                                                                   elif j=maxTuple then "Rest"
                                                                   else (assert false; "")) // dead code under prior assert, props.Length <= maxTuple
               haveNames = expectNames)
        props
            
    let getTupleConstructorMethod(typ:Type,bindingFlags) =
          let props = typ.GetProperties() |> orderTupleProperties
          let ctor = typ.GetConstructor(BindingFlags.Instance ||| bindingFlags,null,props |> Array.map (fun p -> p.PropertyType),null)
          checkNonNull "typ" ctor;
          ctor
        
    let getTupleCtor(typ:Type,bindingFlags) =
          let ctor = getTupleConstructorMethod(typ,bindingFlags)
          (fun (args:obj[]) ->
              ctor.Invoke(BindingFlags.InvokeMethod ||| BindingFlags.Instance ||| bindingFlags,null,args,null))

    let rec getTupleReader (typ:Type) = 
        let etys = typ.GetGenericArguments() 
        // Get the reader for the outer tuple record
        let props = typ.GetProperties(instancePropertyFlags ||| BindingFlags.Public) |> orderTupleProperties
        let reader = (fun (obj:obj) -> props |> Array.map (fun prop -> prop.GetValue(obj,null)))
        if etys.Length < maxTuple 
        then reader
        else
            let tyBenc = etys.[tupleEncField]
            let reader2 = getTupleReader(tyBenc)
            (fun obj ->
                let directVals = reader obj
                let encVals = reader2 directVals.[tupleEncField]
                Array.append directVals.[0..tupleEncField-1] encVals)
                
    let rec getTupleConstructor (typ:Type) = 
        let etys = typ.GetGenericArguments() 
        let maker1 =  getTupleCtor (typ,BindingFlags.Public)
        if etys.Length < maxTuple 
        then maker1
        else
            let tyBenc = etys.[tupleEncField]
            let maker2 = getTupleConstructor(tyBenc)
            (fun (args:obj[]) ->
                let encVal = maker2 args.[tupleEncField..]
                maker1 (Array.append args.[0..tupleEncField-1] [| encVal |]))
                
    let getTupleConstructorInfo (typ:Type) = 
        let etys = typ.GetGenericArguments() 
        let maker1 =  getTupleConstructorMethod (typ,BindingFlags.Public)
        if etys.Length < maxTuple then
            maker1,None
        else
            maker1,Some(etys.[tupleEncField])

    let getTupleReaderInfo (typ:Type,index:int) =         
        if index < 0 then invalidArg "index" (SR.GetString2(SR.tupleIndexOutOfRange, typ.FullName, index.ToString()))
        let props = typ.GetProperties(instancePropertyFlags ||| BindingFlags.Public) |> orderTupleProperties
        let get index = 
            if index >= props.Length then invalidArg "index" (SR.GetString2(SR.tupleIndexOutOfRange, typ.FullName, index.ToString()))
            props.[index]
        
        if index < tupleEncField then
            get index, None  
        else
            let etys = typ.GetGenericArguments()
            get tupleEncField, Some(etys.[tupleEncField],index-(maxTuple-1))
            
      
    //-----------------------------------------------------------------
    // FUNCTION DECOMPILATION
    
      
    let getFunctionTypeInfo (typ:Type) =
      if not (isFunctionType typ) then invalidArg "typ" (SR.GetString1(SR.notAFunctionType, typ.FullName))
      let tyargs = typ.GetGenericArguments()
      tyargs.[0], tyargs.[1]

    //-----------------------------------------------------------------
    // MODULE DECOMPILATION
    
    let isModuleType (typ:Type) = 
      match tryFindSourceConstructFlagsOfType(typ) with 
      | None -> false 
      | Some(flags) -> 
        (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.Module 

    let rec isClosureRepr typ = 
        isFunctionType typ || 
        (match typ.BaseType with null -> false | bty -> isClosureRepr bty) 

    //-----------------------------------------------------------------
    // RECORD DECOMPILATION
    
    let isRecordType (typ:Type,bindingFlags:BindingFlags) = 
      match tryFindSourceConstructFlagsOfType(typ) with 
      | None -> false 
      | Some(flags) ->
        (flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.RecordType &&
        // We see private representations only if BindingFlags.NonPublic is set
        (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
            (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
         else 
            true) &&
        not (isTupleType typ)

    let fieldPropsOfRecordType(typ:Type,bindingFlags) =
      typ.GetProperties(instancePropertyFlags ||| bindingFlags) 
      |> Array.filter isFieldProperty
      |> sortFreshArray (fun p1 p2 -> compare (sequenceNumberOfMember p1) (sequenceNumberOfMember p2))

    let recdDescOfProps props = 
       props |> Array.toList |> List.map (fun (p:PropertyInfo) -> p.Name, p.PropertyType) 

    let getRecd obj (props:PropertyInfo[]) = 
        props |> Array.map (fun prop -> prop.GetValue(obj,null))

    let getRecordReader(typ:Type,bindingFlags) = 
        let props = fieldPropsOfRecordType(typ,bindingFlags)
        (fun (obj:obj) -> props |> Array.map (fun prop -> prop.GetValue(obj,null)))

    let getRecordConstructorMethod(typ:Type,bindingFlags) = 
        let props = fieldPropsOfRecordType(typ,bindingFlags)
        let ctor = typ.GetConstructor(BindingFlags.Instance ||| bindingFlags,null,props |> Array.map (fun p -> p.PropertyType),null)
        checkNonNull "typ" ctor;
        ctor

    let getRecordConstructor(typ:Type,bindingFlags) = 
        let ctor = getRecordConstructorMethod(typ,bindingFlags)
        (fun (args:obj[]) -> 
            ctor.Invoke(BindingFlags.InvokeMethod  ||| BindingFlags.Instance ||| bindingFlags,null,args,null))

    //-----------------------------------------------------------------
    // EXCEPTION DECOMPILATION
    

    // Check the base type - if it is also an F# type then
    // for the moment we know it is a Discriminated Union
    let isExceptionRepr (typ:Type,bindingFlags) = 
        match tryFindSourceConstructFlagsOfType(typ) with 
        | None -> false 
        | Some(flags) -> 
          ((flags &&& SourceConstructFlags.KindMask) = SourceConstructFlags.Exception) &&
          // We see private representations only if BindingFlags.NonPublic is set
          (if (flags &&& SourceConstructFlags.NonPublicRepresentation) <> enum(0) then 
              (bindingFlags &&& BindingFlags.NonPublic) <> enum(0)
           else 
              true)


    let getTypeOfReprType (typ:Type,bindingFlags) = 
        if isExceptionRepr(typ,bindingFlags) then typ.BaseType
        elif isConstructorRepr(typ,bindingFlags) then unionTypeOfUnionCaseType(typ,bindingFlags)
        elif isClosureRepr(typ) then 
          let rec get (typ:Type) = if isFunctionType typ then typ else match typ.BaseType with null -> typ | b -> get b
          get typ 
        else typ


    //-----------------------------------------------------------------
    // CHECKING ROUTINES

    let checkExnType (exceptionType, bindingFlags) =
        if not (isExceptionRepr (exceptionType,bindingFlags)) then 
            if isExceptionRepr (exceptionType,bindingFlags ||| BindingFlags.NonPublic) then 
                invalidArg "exceptionType" (SR.GetString1(SR.privateExceptionType, exceptionType.FullName))
            else
                invalidArg "exceptionType" (SR.GetString1(SR.notAnExceptionType, exceptionType.FullName))
           
    let checkRecordType(argName,recordType,bindingFlags) =
        checkNonNull argName recordType;
        if not (isRecordType (recordType,bindingFlags) ) then 
            if isRecordType (recordType,bindingFlags ||| BindingFlags.NonPublic) then 
                invalidArg argName (SR.GetString1(SR.privateRecordType, recordType.FullName))
            else
                invalidArg argName (SR.GetString1(SR.notARecordType, recordType.FullName))
        
    let checkTupleType(argName,tupleType) =
        checkNonNull argName tupleType;
        if not (isTupleType tupleType) then invalidArg argName (SR.GetString1(SR.notATupleType, tupleType.FullName))
        
[<Sealed>]
type UnionCaseInfo(typ: System.Type, tag:int) =
    // Cache the tag -> name map
    let mutable names = None
    member x.Name = 
        match names with 
        | None -> (let conv = Impl.getUnionTagConverter (typ,BindingFlags.Public ||| BindingFlags.NonPublic) in names <- Some conv; conv tag)
        | Some conv -> conv tag
        
    member x.DeclaringType = typ
    //member x.CustomAttributes = failwith<obj[]> "nyi"
    member x.GetFields() = 
        let props = Impl.fieldsPropsOfUnionCase(typ,tag,BindingFlags.Public ||| BindingFlags.NonPublic) 
        props

    member x.GetCustomAttributes() = 
        let methInfo = Impl.getUnionCaseConstructorMethod (typ,tag,BindingFlags.Public ||| BindingFlags.NonPublic) 
        methInfo.GetCustomAttributes(false)
    
    member x.GetCustomAttributes(attributeType) = 
        let methInfo = Impl.getUnionCaseConstructorMethod (typ,tag,BindingFlags.Public ||| BindingFlags.NonPublic) 
        methInfo.GetCustomAttributes(attributeType,false)

    member x.Tag = tag
    override x.ToString() = typ.Name + "." + x.Name
    override x.GetHashCode() = typ.GetHashCode() + tag
    override x.Equals(obj:obj) = 
        match obj with 
        | :? UnionCaseInfo as uci -> uci.DeclaringType = typ && uci.Tag = tag
        | _ -> false
    

[<AbstractClass; Sealed>]
type FSharpType = 

    static member IsTuple(typ:Type) =  
        Impl.checkNonNull "typ" typ;
        Impl.isTupleType typ

    static member IsRecord(typ:Type,?bindingFlags) =  
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "typ" typ;
        Impl.isRecordType (typ,bindingFlags)

    static member IsUnion(typ:Type,?bindingFlags) =  
        Impl.checkNonNull "typ" typ;
        let typ = Impl.getTypeOfReprType (typ ,BindingFlags.Public ||| BindingFlags.NonPublic)
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.isUnionType (typ,bindingFlags)

    static member IsFunction(typ:Type) =  
        Impl.checkNonNull "typ" typ;
        let typ = Impl.getTypeOfReprType (typ ,BindingFlags.Public ||| BindingFlags.NonPublic)
        Impl.isFunctionType typ

    static member IsModule(typ:Type) =  
        Impl.checkNonNull "typ" typ;
        Impl.isModuleType typ

    static member MakeFunctionType(domain:Type,range:Type) = 
        Impl.checkNonNull "domain" domain;
        Impl.checkNonNull "range" range;
        Impl.func.MakeGenericType [| domain; range |]

    static member MakeTupleType(types:Type[]) =  
        Impl.checkNonNull "types" types;
        if types |> Array.exists (function null -> true | _ -> false) then 
             invalidArg "types" (SR.GetString(SR.nullsNotAllowedInArray))
        Impl.mkTupleType types

    static member GetTupleElements(tupleType:Type) =
        Impl.checkTupleType("tupleType",tupleType);
        Impl.getTupleTypeInfo tupleType

    static member GetFunctionElements(functionType:Type) =
        Impl.checkNonNull "functionType" functionType;
        let functionType = Impl.getTypeOfReprType (functionType ,BindingFlags.Public ||| BindingFlags.NonPublic)
        Impl.getFunctionTypeInfo functionType

    static member GetRecordFields(recordType:Type,?bindingFlags) =
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkRecordType("recordType",recordType,bindingFlags);
        Impl.fieldPropsOfRecordType(recordType,bindingFlags) 

    static member GetUnionCases (unionType:Type,?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "unionType" unionType;
        let unionType = Impl.getTypeOfReprType (unionType ,bindingFlags)
        Impl.checkUnionType(unionType,bindingFlags);
        Impl.getUnionTypeTagNameMap(unionType,bindingFlags) |> Array.mapi (fun i _ -> UnionCaseInfo(unionType,i))

    static member IsExceptionRepresentation(exceptionType:Type, ?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "exceptionType" exceptionType;
        Impl.isExceptionRepr(exceptionType,bindingFlags)

    static member GetExceptionFields(exceptionType:Type, ?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "exceptionType" exceptionType;
        Impl.checkExnType(exceptionType,bindingFlags);
        Impl.fieldPropsOfRecordType (exceptionType,bindingFlags) 

type public DynamicFunction<'T1,'T2>() =
    inherit FSharpFunc<obj -> obj, obj>()
    override x.Invoke(impl: obj -> obj) : obj = 
        box<('T1 -> 'T2)> (fun inp -> unbox<'T2>(impl (box<'T1>(inp))))

[<AbstractClass; Sealed>]
type FSharpValue = 

    static member MakeRecord(recordType:Type,args,?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkRecordType("recordType",recordType,bindingFlags);
        Impl.getRecordConstructor (recordType,bindingFlags) args

    static member GetRecordField(record:obj,info:PropertyInfo) =
        Impl.checkNonNull "info" info;
        Impl.checkNonNull "record" record;
        let reprty = record.GetType() 
        if not (Impl.isRecordType(reprty,BindingFlags.Public ||| BindingFlags.NonPublic)) then invalidArg "record" (SR.GetString(SR.objIsNotARecord));
        info.GetValue(record,null)

    static member GetRecordFields(record:obj,?bindingFlags) =
        Impl.checkNonNull "record" record;
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        let typ = record.GetType() 
        if not (Impl.isRecordType(typ,bindingFlags)) then invalidArg "record" (SR.GetString(SR.objIsNotARecord));
        Impl.getRecordReader (typ,bindingFlags) record

    static member PreComputeRecordFieldReader(info:PropertyInfo) = 
        Impl.checkNonNull "info" info;
        (fun (obj:obj) -> info.GetValue(obj,null))

    static member PreComputeRecordReader(recordType:Type,?bindingFlags) : (obj -> obj[]) =  
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkRecordType("recordType",recordType,bindingFlags);
        Impl.getRecordReader (recordType,bindingFlags)

    static member PreComputeRecordConstructor(recordType:Type,?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkRecordType("recordType",recordType,bindingFlags);
        Impl.getRecordConstructor (recordType,bindingFlags)

    static member PreComputeRecordConstructorInfo(recordType:Type, ?bindingFlags) =
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkRecordType("recordType",recordType,bindingFlags);
        Impl.getRecordConstructorMethod(recordType,bindingFlags)

    static member MakeFunction(functionType:Type,implementation:(obj->obj)) = 
        Impl.checkNonNull "functionType" functionType;
        if not (Impl.isFunctionType functionType) then invalidArg "functionType" (SR.GetString1(SR.notAFunctionType, functionType.FullName));
        Impl.checkNonNull "implementation" implementation;
        let domain,range = Impl.getFunctionTypeInfo functionType
        let dynCloMakerTy = typedefof<DynamicFunction<obj,obj>>
        let saverTy = dynCloMakerTy.MakeGenericType [| domain; range |]
        let o = Activator.CreateInstance(saverTy)
        let (f : (obj -> obj) -> obj) = downcast o
        f implementation

    static member MakeTuple(tupleElements: obj[],tupleType:Type) =
        Impl.checkNonNull "tupleElements" tupleElements;
        Impl.checkTupleType("tupleType",tupleType) 
        Impl.getTupleConstructor tupleType tupleElements
    
    static member GetTupleFields(tuple:obj) = // argument name(s) used in error message
        Impl.checkNonNull "tuple" tuple;
        let typ = tuple.GetType() 
        if not (Impl.isTupleType typ ) then invalidArg "tuple" (SR.GetString1(SR.notATupleType, tuple.GetType().FullName));
        Impl.getTupleReader typ tuple

    static member GetTupleField(tuple:obj,index:int) = // argument name(s) used in error message
        Impl.checkNonNull "tuple" tuple;
        let typ = tuple.GetType() 
        if not (Impl.isTupleType typ ) then invalidArg "tuple" (SR.GetString1(SR.notATupleType, tuple.GetType().FullName));
        let fields = Impl.getTupleReader typ tuple
        if index < 0 || index >= fields.Length then invalidArg "index" (SR.GetString2(SR.tupleIndexOutOfRange, tuple.GetType().FullName, index.ToString()));
        fields.[index]
    
    static member PreComputeTupleReader(tupleType:Type) : (obj -> obj[])  =
        Impl.checkTupleType("tupleType",tupleType) 
        Impl.getTupleReader tupleType
    
    static member PreComputeTuplePropertyInfo(tupleType:Type,index:int) =
        Impl.checkTupleType("tupleType",tupleType) 
        Impl.getTupleReaderInfo (tupleType,index)
    
    static member PreComputeTupleConstructor(tupleType:Type) = 
        Impl.checkTupleType("tupleType",tupleType) 
        Impl.getTupleConstructor tupleType

    static member PreComputeTupleConstructorInfo(tupleType:Type) =
        Impl.checkTupleType("tupleType",tupleType) 
        Impl.getTupleConstructorInfo (tupleType) 

    static member MakeUnion(unionCase:UnionCaseInfo,args: obj [],?bindingFlags) = 
        Impl.checkNonNull "unionCase" unionCase;
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.getUnionCaseConstructor (unionCase.DeclaringType,unionCase.Tag,bindingFlags) args

    static member PreComputeUnionConstructor (unionCase:UnionCaseInfo,?bindingFlags) = 
        Impl.checkNonNull "unionCase" unionCase;
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.getUnionCaseConstructor (unionCase.DeclaringType,unionCase.Tag,bindingFlags)

    static member PreComputeUnionConstructorInfo(unionCase:UnionCaseInfo, ?bindingFlags) =
        Impl.checkNonNull "unionCase" unionCase;
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.getUnionCaseConstructorMethod (unionCase.DeclaringType,unionCase.Tag,bindingFlags) 

    static member GetUnionFields(obj:obj,unionType:Type,?bindingFlags) = 
        let ensureType (typ:Type,obj:obj) = 
                match typ with 
                | null -> 
                    match obj with 
                    | null -> invalidArg "obj" (SR.GetString(SR.objIsNullAndNoType))
                    | _ -> obj.GetType()
                | _ -> typ 
        //System.Console.WriteLine("typ1 = {0}",box unionType)
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        let unionType = ensureType(unionType,obj) 
        //System.Console.WriteLine("typ2 = {0}",box unionType)
        Impl.checkNonNull "unionType" unionType;
        let unionType = Impl.getTypeOfReprType (unionType ,bindingFlags)
        //System.Console.WriteLine("typ3 = {0}",box unionType)
        Impl.checkUnionType(unionType,bindingFlags);
        let tag = Impl.getUnionTagReader (unionType,bindingFlags) obj
        let flds = Impl.getUnionCaseRecordReader (unionType,tag,bindingFlags) obj 
        UnionCaseInfo(unionType,tag), flds

    static member PreComputeUnionTagReader(unionType: Type,?bindingFlags) : (obj -> int) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "unionType" unionType;
        let unionType = Impl.getTypeOfReprType (unionType ,bindingFlags)
        Impl.checkUnionType(unionType,bindingFlags);
        Impl.getUnionTagReader (unionType ,bindingFlags)

    static member PreComputeUnionTagMemberInfo(unionType: Type,?bindingFlags) = 
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        Impl.checkNonNull "unionType" unionType;
        let unionType = Impl.getTypeOfReprType (unionType ,bindingFlags)
        Impl.checkUnionType(unionType,bindingFlags);
        Impl.getUnionTagMemberInfo(unionType ,bindingFlags)

    static member PreComputeUnionReader(unionCase: UnionCaseInfo,?bindingFlags) : (obj -> obj[])  = 
        Impl.checkNonNull "unionCase" unionCase;
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        let typ = unionCase.DeclaringType 
        Impl.getUnionCaseRecordReader (typ,unionCase.Tag,bindingFlags) 
    

    static member GetExceptionFields(exn:obj, ?bindingFlags) = 
        Impl.checkNonNull "exn" exn;
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public 
        let typ = exn.GetType() 
        Impl.checkExnType(typ,bindingFlags);
        Impl.getRecordReader (typ,bindingFlags) exn


