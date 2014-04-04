// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

// This file contains a set of helper types and methods for providing types in an implementation 
// of ITypeProvider.
//
// This code is a sample for use in conjunction with the F# 3.0 Beta release of March 2012

namespace Samples.FSharp.ProvidedTypes

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices

type internal ExpectedStackState = 
    | Empty = 1
    | Address = 2
    | Value = 3

[<AutoOpen>]
module internal Misc =
    let TypeBuilderInstantiationType = typeof<TypeBuilder>.Assembly.GetType("System.Reflection.Emit.TypeBuilderInstantiation")
    let GetTypeFromHandleMethod = typeof<Type>.GetMethod("GetTypeFromHandle")
    let LanguagePrimitivesType = typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Core.LanguagePrimitives")
    let ParseInt32Method = LanguagePrimitivesType.GetMethod "ParseInt32"
    let isEmpty s = s = ExpectedStackState.Empty
    let isAddress s = s = ExpectedStackState.Address

    let nonNull str x = if x=null then failwith ("Null in " + str) else x
    
    let notRequired opname item = 
        let msg = sprintf "The operation '%s' on item '%s' should not be called on provided type, member or parameter" opname item
        System.Diagnostics.Debug.Assert (false, msg)
        raise (System.NotSupportedException msg)

    let mkParamArrayCustomAttributeData() = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        { new IProvidedCustomAttributeData with 
#else
        { new CustomAttributeData() with 
#endif 
            member __.Constructor =  typeof<ParamArrayAttribute>.GetConstructors().[0]
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |] }

#if FX_NO_CUSTOMATTRIBUTEDATA
    let CustomAttributeTypedArgument(ty,v) = 
        { new IProvidedCustomAttributeTypedArgument with 
              member x.ArgumentType = ty
              member x.Value = v }
    let CustomAttributeNamedArgument(memb,arg:IProvidedCustomAttributeTypedArgument) = 
        { new IProvidedCustomAttributeNamedArgument with 
              member x.MemberInfo = memb
              member x.ArgumentType = arg.ArgumentType
              member x.TypedValue = arg }
    type CustomAttributeData = Microsoft.FSharp.Core.CompilerServices.IProvidedCustomAttributeData
#endif

    let mkEditorHideMethodsCustomAttributeData() = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        { new IProvidedCustomAttributeData with 
#else
        { new CustomAttributeData() with 
#endif 
            member __.Constructor =  typeof<TypeProviderEditorHideMethodsAttribute>.GetConstructors().[0]
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = upcast [| |] }

    /// This makes an xml doc attribute w.r.t. an amortized computation of an xml doc string.
    /// It is important that the text of the xml doc only get forced when poking on the ConstructorArguments
    /// for the CustomAttributeData object.
    let mkXmlDocCustomAttributeDataLazy(lazyText: Lazy<string>) = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        { new IProvidedCustomAttributeData with 
#else
        { new CustomAttributeData() with 
#endif
            member __.Constructor =  typeof<TypeProviderXmlDocAttribute>.GetConstructors().[0]
            member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, lazyText.Force())  |]
            member __.NamedArguments = upcast [| |] }

    let mkXmlDocCustomAttributeData(s:string) =  mkXmlDocCustomAttributeDataLazy (lazy s)

    let mkDefinitionLocationAttributeCustomAttributeData(line:int,column:int,filePath:string) = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        { new IProvidedCustomAttributeData with 
#else
        { new CustomAttributeData() with 
#endif
            member __.Constructor =  typeof<TypeProviderDefinitionLocationAttribute>.GetConstructors().[0]
            member __.ConstructorArguments = upcast [| |]
            member __.NamedArguments = 
                upcast [| CustomAttributeNamedArgument(typeof<TypeProviderDefinitionLocationAttribute>.GetProperty("FilePath"), CustomAttributeTypedArgument(typeof<string>, filePath));
                            CustomAttributeNamedArgument(typeof<TypeProviderDefinitionLocationAttribute>.GetProperty("Line"), CustomAttributeTypedArgument(typeof<int>, line)) ;
                            CustomAttributeNamedArgument(typeof<TypeProviderDefinitionLocationAttribute>.GetProperty("Column"), CustomAttributeTypedArgument(typeof<int>, column)) 
                        |] }
    let mkObsoleteAttributeCustomAttributeData(message:string, isError: bool) = 
#if FX_NO_CUSTOMATTRIBUTEDATA
        { new IProvidedCustomAttributeData with 
#else
        { new CustomAttributeData() with 
#endif
                member __.Constructor =  typeof<System.ObsoleteAttribute>.GetConstructors() |> Array.find (fun x -> x.GetParameters().Length = 1)
                member __.ConstructorArguments = upcast [|CustomAttributeTypedArgument(typeof<string>, message) ; CustomAttributeTypedArgument(typeof<bool>, isError)  |]
                member __.NamedArguments = upcast [| |] }

    type CustomAttributesImpl() =
        let customAttributes = ResizeArray<CustomAttributeData>()
        let mutable hideObjectMethods = false
        let mutable obsoleteMessage = None
        let mutable xmlDocDelayed = None
        let mutable xmlDocAlwaysRecomputed = None
        let mutable hasParamArray = false

        // XML doc text that we only compute once, if any. This must _not_ be forced until the ConstructorArguments
        // property of the custom attribute is foced.
        let xmlDocDelayedText = 
            lazy 
                (match xmlDocDelayed with None -> assert false; "" | Some f -> f())

        // Custom atttributes that we only compute once
        let customAttributesOnce = 
            lazy 
               [| if hideObjectMethods then yield mkEditorHideMethodsCustomAttributeData() 
                  match xmlDocDelayed with None -> () | Some _ -> customAttributes.Add(mkXmlDocCustomAttributeDataLazy xmlDocDelayedText) 
                  match obsoleteMessage with None -> () | Some s -> customAttributes.Add(mkObsoleteAttributeCustomAttributeData s) 
                  if hasParamArray then yield mkParamArrayCustomAttributeData()
                  yield! customAttributes |]

        member __.AddDefinitionLocation(line:int,column:int,filePath:string) = customAttributes.Add(mkDefinitionLocationAttributeCustomAttributeData(line, column, filePath))
        member __.AddObsolete(msg : string, isError) = obsoleteMessage <- Some (msg,isError)
        member __.HasParamArray with get() = hasParamArray and set(v) = hasParamArray <- v
        member __.AddXmlDocComputed(xmlDoc : unit -> string) = xmlDocAlwaysRecomputed <- Some xmlDoc
        member __.AddXmlDocDelayed(xmlDoc : unit -> string) = xmlDocDelayed <- Some xmlDoc
        member this.AddXmlDoc(text:string) =  this.AddXmlDocDelayed (fun () -> text)
        member __.HideObjectMethods with set v = hideObjectMethods <- v
        member __.GetCustomAttributesData() = 
            [| yield! customAttributesOnce.Force()
               match xmlDocAlwaysRecomputed with None -> () | Some f -> customAttributes.Add(mkXmlDocCustomAttributeData (f()))  |]
            :> IList<_>

    let transExpr isGenerated q =     
        let rec trans q = 
            match q with 
            // convert NewTuple to the call to the constructor of the Tuple type (only for generated types)
            | Quotations.Patterns.NewTuple(items) when isGenerated ->
                let rec mkCtor args ty = 
                    let ctor, restTyOpt = Reflection.FSharpValue.PreComputeTupleConstructorInfo ty
                    match restTyOpt with
                    | None -> Quotations.Expr.NewObject(ctor, List.map trans args)
                    | Some restTy ->
                        let curr = [for a in Seq.take 7 args -> trans a]
                        let rest = List.ofSeq (Seq.skip 7 args) 
                        Quotations.Expr.NewObject(ctor, curr @ [mkCtor rest restTy])
                let tys = [| for e in items -> e.Type |]
                let tupleTy = Reflection.FSharpType.MakeTupleType tys
                trans (mkCtor items tupleTy)
            // convert TupleGet to the chain of PropertyGet calls (only for generated types)
            | Quotations.Patterns.TupleGet(e, i) when isGenerated ->
                let rec mkGet ty i (e : Quotations.Expr)  = 
                    let pi, restOpt = Reflection.FSharpValue.PreComputeTuplePropertyInfo(ty, i)
                    let propGet = Quotations.Expr.PropertyGet(e, pi)
                    match restOpt with
                    | None -> propGet
                    | Some (restTy, restI) -> mkGet restTy restI propGet
                trans (mkGet e.Type i (trans e))
            | Quotations.Patterns.Value(value, ty) ->
                if value <> null then
                   let tyOfValue = value.GetType()
                   transValue(value, tyOfValue, ty)
                else q
            // Eliminate F# property gets to method calls
            | Quotations.Patterns.PropertyGet(obj,propInfo,args) -> 
                match obj with 
                | None -> trans (Quotations.Expr.Call(propInfo.GetGetMethod(),args))
                | Some o -> trans (Quotations.Expr.Call(trans o,propInfo.GetGetMethod(),args))
            // Eliminate F# property sets to method calls
            | Quotations.Patterns.PropertySet(obj,propInfo,args,v) -> 
                 match obj with 
                 | None -> trans (Quotations.Expr.Call(propInfo.GetSetMethod(),args@[v]))
                 | Some o -> trans (Quotations.Expr.Call(trans o,propInfo.GetSetMethod(),args@[v]))
            // Eliminate F# function applications to FSharpFunc<_,_>.Invoke calls
            | Quotations.Patterns.Application(f,e) -> 
                trans (Quotations.Expr.Call(trans f, f.Type.GetMethod "Invoke", [ e ]) )
            | Quotations.Patterns.NewUnionCase(ci, es) ->
                trans (Quotations.Expr.Call(Reflection.FSharpValue.PreComputeUnionConstructorInfo ci, es) )
            | Quotations.Patterns.NewRecord(ci, es) ->
                trans (Quotations.Expr.NewObject(Reflection.FSharpValue.PreComputeRecordConstructorInfo ci, es) )
            | Quotations.Patterns.UnionCaseTest(e,uc) ->
                let tagInfo = Reflection.FSharpValue.PreComputeUnionTagMemberInfo uc.DeclaringType
                let tagExpr = 
                    match tagInfo with 
                    | :? PropertyInfo as tagProp ->
                         trans (Quotations.Expr.PropertyGet(e,tagProp) )
                    | :? MethodInfo as tagMeth -> 
                         if tagMeth.IsStatic then trans (Quotations.Expr.Call(tagMeth, [e]))
                         else trans (Quotations.Expr.Call(e,tagMeth,[]))
                    | _ -> failwith "unreachable: unexpected result from PreComputeUnionTagMemberInfo"
                let tagNumber = uc.Tag
                trans <@@ (%%(tagExpr) : int) = tagNumber @@>

            // Explicitly handle weird byref variables in lets (used to populate out parameters), since the generic handlers can't deal with byrefs
            | Quotations.Patterns.Let(v,vexpr,bexpr) when v.Type.IsByRef ->

                // the binding must have leaves that are themselves variables (due to the limited support for byrefs in expressions)
                // therefore, we can perform inlining to translate this to a form that can be compiled
                inlineByref v vexpr bexpr

            // Handle the generic cases
            | Quotations.ExprShape.ShapeLambda(v,body) -> 
                Quotations.Expr.Lambda(v, trans body)
            | Quotations.ExprShape.ShapeCombination(comb,args) -> 
                Quotations.ExprShape.RebuildShapeCombination(comb,List.map trans args)
            | Quotations.ExprShape.ShapeVar _ -> q
        and inlineByref v vexpr bexpr =
            match vexpr with
            | Quotations.Patterns.Sequential(e',vexpr') ->
                (* let v = (e'; vexpr') in bexpr => e'; let v = vexpr' in bexpr *)
                Quotations.Expr.Sequential(e', inlineByref v vexpr' bexpr)
                |> trans
            | Quotations.Patterns.IfThenElse(c,b1,b2) ->
                (* let v = if c then b1 else b2 in bexpr => if c then let v = b1 in bexpr else let v = b2 in bexpr *)
                Quotations.Expr.IfThenElse(c, inlineByref v b1 bexpr, inlineByref v b2 bexpr)
                |> trans
            | Quotations.Patterns.Var _ -> 
                (* let v = v1 in bexpr => bexpr[v/v1] *)
                bexpr.Substitute(fun v' -> if v = v' then Some vexpr else None)
                |> trans
            | _ -> 
                failwith (sprintf "Unexpected byref binding: %A = %A" v vexpr)
        and transValue (v : obj, tyOfValue : Type, expectedTy : Type) = 
            let rec transArray (o : Array, ty : Type) = 
                let elemTy = ty.GetElementType()
                let converter = getConverterForType elemTy
                let elements = 
                    [
                        for el in o do
                            yield converter el
                    ]
                Quotations.Expr.NewArray(elemTy, elements)
            and transList(o, ty : Type, nil, cons) =
                let converter = getConverterForType (ty.GetGenericArguments().[0])
                o
                |> Seq.cast
                |> List.ofSeq
                |> fun l -> List.foldBack(fun o s -> Quotations.Expr.NewUnionCase(cons, [ converter(o); s ])) l (Quotations.Expr.NewUnionCase(nil, []))
                |> trans
            and getConverterForType (ty : Type) = 
                if ty.IsArray then 
                    fun (v : obj) -> transArray(v :?> Array, ty)
                elif ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<_ list> then 
                    let nil, cons =
                        let cases = Reflection.FSharpType.GetUnionCases(ty)
                        let a = cases.[0]
                        let b = cases.[1]
                        if a.Name = "Empty" then a,b
                        else b,a
                     
                    fun v -> transList (v :?> System.Collections.IEnumerable, ty, nil, cons)
                else 
                    fun v -> Quotations.Expr.Value(v, ty)
            let converter = getConverterForType tyOfValue
            let r = converter v
            if tyOfValue <> expectedTy then Quotations.Expr.Coerce(r, expectedTy)
            else r
        trans q

    let transQuotationToCode isGenerated qexprf (argExprs: Quotations.Expr[]) = 
        // add let bindings for arguments to ensure that arguments will be evaluated
        let vars = argExprs |> Array.mapi (fun i e -> Quotations.Var(("var" + string i), e.Type))
        let expr = qexprf ([for v in vars -> Quotations.Expr.Var v])

        let pairs = Array.zip argExprs vars
        let expr = Array.foldBack (fun (arg, var) e -> Quotations.Expr.Let(var, arg, e)) pairs expr

        transExpr isGenerated expr

    let adjustTypeAttributes attributes isNested = 
        let visibilityAttributes = 
            match attributes &&& TypeAttributes.VisibilityMask with 
            | TypeAttributes.Public when isNested -> TypeAttributes.NestedPublic
            | TypeAttributes.NotPublic when isNested -> TypeAttributes.NestedAssembly
            | TypeAttributes.NestedPublic when not isNested -> TypeAttributes.Public
            | TypeAttributes.NestedAssembly 
            | TypeAttributes.NestedPrivate 
            | TypeAttributes.NestedFamORAssem
            | TypeAttributes.NestedFamily
            | TypeAttributes.NestedFamANDAssem when not isNested -> TypeAttributes.NotPublic
            | a -> a
        (attributes &&& ~~~TypeAttributes.VisibilityMask) ||| visibilityAttributes
        
type ProvidedStaticParameter(parameterName:string,parameterType:Type,?parameterDefaultValue:obj) = 
    inherit System.Reflection.ParameterInfo()

    let customAttributesImpl = CustomAttributesImpl()
    member __.AddXmlDocDelayed(xmlDoc : unit -> string) = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member __.AddXmlDocComputed(xmlDoc : unit -> string) = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDoc(text:string) = customAttributesImpl.AddXmlDoc text

    override __.RawDefaultValue = defaultArg parameterDefaultValue null
    override __.Attributes = if parameterDefaultValue.IsNone then enum 0 else ParameterAttributes.Optional
    override __.Position = 0
    override __.ParameterType = parameterType
    override __.Name = parameterName 

    override __.GetCustomAttributes(_inherit) = ignore(_inherit); notRequired "GetCustomAttributes" parameterName
    override __.GetCustomAttributes(_attributeType, _inherit) = notRequired "GetCustomAttributes" parameterName

type ProvidedParameter(name:string,parameterType:Type,?isOut:bool,?optionalValue:obj) = 
    inherit System.Reflection.ParameterInfo()
    let customAttributesImpl = CustomAttributesImpl()
    let isOut = defaultArg isOut false
    member this.IsParamArray with get() = customAttributesImpl.HasParamArray and set(v) = customAttributesImpl.HasParamArray <- v
    override this.Name = name
    override this.ParameterType = parameterType
    override this.Attributes = (base.Attributes ||| (if isOut then ParameterAttributes.Out else enum 0)
                                                ||| (match optionalValue with None -> enum 0 | Some _ -> ParameterAttributes.Optional ||| ParameterAttributes.HasDefault))
    override this.RawDefaultValue = defaultArg optionalValue null
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()
#endif

type ProvidedConstructor(parameters : ProvidedParameter list) = 
    inherit ConstructorInfo()
    let parameters  = parameters |> List.map (fun p -> p :> ParameterInfo) 
    let mutable baseCall  = None

    let mutable declaringType = null : System.Type
    let mutable invokeCode    = None : option<Quotations.Expr list -> Quotations.Expr>
    let mutable isImplicitCtor  = false
    let mutable ctorAttributes = MethodAttributes.Public ||| MethodAttributes.RTSpecialName
    let nameText () = sprintf "constructor for %s" (if declaringType=null then "<not yet known type>" else declaringType.FullName)

    let customAttributesImpl = CustomAttributesImpl()
    member this.IsTypeInitializer 
        with get() = ctorAttributes.HasFlag(MethodAttributes.Static) && ctorAttributes.HasFlag(MethodAttributes.Private)
        and set(v) = 
            let typeInitializerAttributes = MethodAttributes.Static ||| MethodAttributes.Private
            ctorAttributes <- if v then ctorAttributes ||| typeInitializerAttributes else ctorAttributes &&& ~~~typeInitializerAttributes

    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddObsoleteAttribute (msg,?isError)         = customAttributesImpl.AddObsolete (msg,defaultArg isError false)
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member this.HideObjectMethods with set v                = customAttributesImpl.HideObjectMethods <- v
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.DeclaringTypeImpl 
        with set x = 
            if declaringType<>null then failwith (sprintf "ProvidedConstructor: declaringType already set on '%s'" (nameText())); 
            declaringType <- x

    member this.InvokeCode 
        with set (q:Quotations.Expr list -> Quotations.Expr) = 
            match invokeCode with
            | None -> invokeCode <- Some q
            | Some _ -> failwith (sprintf "ProvidedConstructor: code already given for '%s'" (nameText()))        

    member this.BaseConstructorCall
        with set (d:Quotations.Expr list -> (ConstructorInfo * Quotations.Expr list)) = 
            match baseCall with
            | None -> baseCall <- Some d
            | Some _ -> failwith (sprintf "ProvidedConstructor: base call already given for '%s'" (nameText()))        

    member this.GetInvokeCodeInternal isGenerated =
        match invokeCode with
        | Some f -> transQuotationToCode isGenerated f
        | None -> failwith (sprintf "ProvidedConstructor: no invoker for '%s'" (nameText()))

    member this.GetBaseConstructorCallInternal isGenerated =
        match baseCall with
        | Some f -> Some(fun ctorArgs -> let c,baseCtorArgExprs = f ctorArgs in c, List.map (transExpr isGenerated) baseCtorArgExprs)
        | None -> None
    member this.IsImplicitCtor with get() = isImplicitCtor and set v = isImplicitCtor <- v

    // Implement overloads
    override this.GetParameters() = parameters |> List.toArray 
    override this.Attributes = ctorAttributes
    override this.Name = if this.IsStatic then ".cctor" else ".ctor"
    override this.DeclaringType = declaringType |> nonNull "ProvidedConstructor.DeclaringType"                                   
    override this.IsDefined(_attributeType, _inherit) = true 

    override this.Invoke(_invokeAttr, _binder, _parameters, _culture)      = notRequired "Invoke" (nameText())
    override this.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" (nameText())
    override this.ReflectedType                                        = notRequired "ReflectedType" (nameText())
    override this.GetMethodImplementationFlags()                       = notRequired "GetMethodImplementationFlags" (nameText())
    override this.MethodHandle                                         = notRequired "MethodHandle" (nameText())
    override this.GetCustomAttributes(_inherit)                     = notRequired "GetCustomAttributes" (nameText())
    override this.GetCustomAttributes(_attributeType, _inherit)      = notRequired "GetCustomAttributes" (nameText())

type ProvidedMethod(methodName: string, parameters: ProvidedParameter list, returnType: Type) =
    inherit System.Reflection.MethodInfo()
    let argParams = parameters |> List.map (fun p -> p :> ParameterInfo) 

    // State
    let mutable declaringType : Type = null
    let mutable methodAttrs   = MethodAttributes.Public
    let mutable invokeCode    = None : option<Quotations.Expr list -> Quotations.Expr>

    let customAttributesImpl = CustomAttributesImpl()
    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddObsoleteAttribute (msg,?isError)         = customAttributesImpl.AddObsolete (msg,defaultArg isError false)
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.SetMethodAttrs m = methodAttrs <- m 
    member this.AddMethodAttrs m = methodAttrs <- methodAttrs ||| m
    member this.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice
    member this.IsStaticMethod 
        with get()  = methodAttrs.HasFlag(MethodAttributes.Static)
        and set x = if x then methodAttrs <- methodAttrs ||| MethodAttributes.Static
                    else methodAttrs <- methodAttrs &&& (~~~ MethodAttributes.Static)
    member this.InvokeCode 
        with set  (q:Quotations.Expr list -> Quotations.Expr) = 
            match invokeCode with
            | None -> invokeCode <- Some q
            | Some _ -> failwith (sprintf "ProvidedConstructor: code already given for %s on type %s" this.Name (if declaringType=null then "<not yet known type>" else declaringType.FullName))


    member this.GetInvokeCodeInternal isGenerated =
        match invokeCode with
        | Some f ->  transQuotationToCode isGenerated f
        | None -> failwith (sprintf "ProvidedMethod: no invoker for %s on type %s" this.Name (if declaringType=null then "<not yet known type>" else declaringType.FullName))
   // Implement overloads
    override this.GetParameters() = argParams |> Array.ofList
    override this.Attributes = methodAttrs
    override this.Name = methodName
    override this.DeclaringType = declaringType |> nonNull "ProvidedMethod.DeclaringType"                                   
    override this.IsDefined(_attributeType, _inherit) : bool = true
    override this.MemberType = MemberTypes.Method
    override this.CallingConvention = 
        let cc = CallingConventions.Standard
        let cc = if not (this.IsStatic) then cc ||| CallingConventions.HasThis else cc
        cc
    override this.ReturnType = returnType
    override this.ReturnParameter = null // REVIEW: Give it a name and type?
    override this.ToString() = "Method " + this.Name
    
    // These don't have to return fully accurate results - they are used 
    // by the F# Quotations library function SpecificCall as a pre-optimization
    // when comparing methods
    override this.MetadataToken = hash declaringType + hash this.Name
    override this.MethodHandle = RuntimeMethodHandle()

    override this.ReturnTypeCustomAttributes                           = notRequired "ReturnTypeCustomAttributes" this.Name
    override this.GetBaseDefinition()                                  = notRequired "GetBaseDefinition" this.Name
    override this.GetMethodImplementationFlags()                       = notRequired "GetMethodImplementationFlags" this.Name
    override this.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" this.Name
    override this.ReflectedType                                        = notRequired "ReflectedType" this.Name
    override this.GetCustomAttributes(_inherit)                     = notRequired "GetCustomAttributes" this.Name
    override this.GetCustomAttributes(_attributeType, _inherit)      =  notRequired "GetCustomAttributes" this.Name


type ProvidedProperty(propertyName:string,propertyType:Type, ?parameters:ProvidedParameter list) = 
    inherit System.Reflection.PropertyInfo()
    // State

    let parameters = defaultArg parameters []
    let mutable declaringType = null
    let mutable isStatic = false
    let mutable getterCode = None : option<Quotations.Expr list -> Quotations.Expr>
    let mutable setterCode = None : option<Quotations.Expr list -> Quotations.Expr>

    let hasGetter() = getterCode.IsSome
    let hasSetter() = setterCode.IsSome

    // Delay construction - to pick up the latest isStatic
    let markSpecialName (m:ProvidedMethod) = m.AddMethodAttrs(MethodAttributes.SpecialName); m
    let getter = lazy (ProvidedMethod("get_" + propertyName,parameters,propertyType,IsStaticMethod=isStatic,DeclaringTypeImpl=declaringType,InvokeCode=getterCode.Value) |> markSpecialName)  
    let setter = lazy (ProvidedMethod("set_" + propertyName,parameters @ [ProvidedParameter("value",propertyType)],typeof<System.Void>,IsStaticMethod=isStatic,DeclaringTypeImpl=declaringType,InvokeCode=setterCode.Value) |> markSpecialName) 
 
    let customAttributesImpl = CustomAttributesImpl()
    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddObsoleteAttribute (msg,?isError)         = customAttributesImpl.AddObsolete (msg,defaultArg isError false)
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice
    member this.IsStatic 
        with get()  = isStatic
        and set x = isStatic <- x

    member this.GetterCode 
        with set  (q:Quotations.Expr list -> Quotations.Expr) = 
            if not getter.IsValueCreated then getterCode <- Some q else failwith "ProvidedProperty: getter MethodInfo has already been created"

    member this.SetterCode 
        with set (q:Quotations.Expr list -> Quotations.Expr) = 
            if not (setter.IsValueCreated) then setterCode <- Some q else failwith "ProvidedProperty: setter MethodInfo has already been created"

    // Implement overloads
    override this.PropertyType = propertyType
    override this.SetValue(_obj, _value, _invokeAttr, _binder, _index, _culture) = notRequired "SetValue" this.Name
    override this.GetAccessors _nonPublic  = notRequired "nonPublic" this.Name
    override this.GetGetMethod _nonPublic = if hasGetter() then getter.Force() :> MethodInfo else null
    override this.GetSetMethod _nonPublic = if hasSetter() then setter.Force() :> MethodInfo else null
    override this.GetIndexParameters() = [| for p in parameters -> upcast p |]
    override this.Attributes = PropertyAttributes.None
    override this.CanRead = hasGetter()
    override this.CanWrite = hasSetter()
    override this.GetValue(_obj, _invokeAttr, _binder, _index, _culture) : obj = notRequired "GetValue" this.Name
    override this.Name = propertyName
    override this.DeclaringType = declaringType |> nonNull "ProvidedProperty.DeclaringType"
    override this.MemberType : MemberTypes = MemberTypes.Property

    override this.ReflectedType                                     = notRequired "ReflectedType" this.Name
    override this.GetCustomAttributes(_inherit)                  = notRequired "GetCustomAttributes" this.Name
    override this.GetCustomAttributes(_attributeType, _inherit)   = notRequired "GetCustomAttributes" this.Name
    override this.IsDefined(_attributeType, _inherit)             = notRequired "IsDefined" this.Name

type ProvidedEvent(eventName:string,eventHandlerType:Type) = 
    inherit System.Reflection.EventInfo()
    // State

    let mutable declaringType = null
    let mutable isStatic = false
    let mutable adderCode = None : option<Quotations.Expr list -> Quotations.Expr>
    let mutable removerCode = None : option<Quotations.Expr list -> Quotations.Expr>

    // Delay construction - to pick up the latest isStatic
    let markSpecialName (m:ProvidedMethod) = m.AddMethodAttrs(MethodAttributes.SpecialName); m
    let adder = lazy (ProvidedMethod("add_" + eventName, [ProvidedParameter("handler", eventHandlerType)],typeof<System.Void>,IsStaticMethod=isStatic,DeclaringTypeImpl=declaringType,InvokeCode=adderCode.Value) |> markSpecialName)  
    let remover = lazy (ProvidedMethod("remove_" + eventName, [ProvidedParameter("handler", eventHandlerType)],typeof<System.Void>,IsStaticMethod=isStatic,DeclaringTypeImpl=declaringType,InvokeCode=removerCode.Value) |> markSpecialName) 
 
    let customAttributesImpl = CustomAttributesImpl()
    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice
    member this.IsStatic 
        with get()  = isStatic
        and set x = isStatic <- x

    member this.AdderCode 
        with get() = adderCode.Value
        and  set f = 
            if not adder.IsValueCreated then adderCode <- Some f else failwith "ProvidedEvent: Add MethodInfo has already been created"                                         

    member this.RemoverCode
        with get() = removerCode.Value
        and  set f = 
            if not (remover.IsValueCreated) then removerCode <- Some f else failwith "ProvidedEvent: Remove MethodInfo has already been created"

    // Implement overloads
    override this.EventHandlerType = eventHandlerType
    override this.GetAddMethod _nonPublic = adder.Force() :> MethodInfo
    override this.GetRemoveMethod _nonPublic = remover.Force() :> MethodInfo
    override this.Attributes = EventAttributes.None
    override this.Name = eventName
    override this.DeclaringType = declaringType |> nonNull "ProvidedEvent.DeclaringType"
    override this.MemberType : MemberTypes = MemberTypes.Event

    override this.GetRaiseMethod _nonPublic                      = notRequired "GetRaiseMethod" this.Name
    override this.ReflectedType                                     = notRequired "ReflectedType" this.Name
    override this.GetCustomAttributes(_inherit)                  = notRequired "GetCustomAttributes" this.Name
    override this.GetCustomAttributes(_attributeType, _inherit)   = notRequired "GetCustomAttributes" this.Name
    override this.IsDefined(_attributeType, _inherit)             = notRequired "IsDefined" this.Name

type ProvidedLiteralField(fieldName:string,fieldType:Type,literalValue:obj) = 
    inherit System.Reflection.FieldInfo()
    // State

    let mutable declaringType = null

    let customAttributesImpl = CustomAttributesImpl()
    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddObsoleteAttribute (msg,?isError)         = customAttributesImpl.AddObsolete (msg,defaultArg isError false)
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice


    // Implement overloads
    override this.FieldType = fieldType
    override this.GetRawConstantValue()  = literalValue
    override this.Attributes = FieldAttributes.Static ||| FieldAttributes.Literal ||| FieldAttributes.Public
    override this.Name = fieldName
    override this.DeclaringType = declaringType |> nonNull "ProvidedLiteralField.DeclaringType"
    override this.MemberType : MemberTypes = MemberTypes.Field

    override this.ReflectedType                                     = notRequired "ReflectedType" this.Name
    override this.GetCustomAttributes(_inherit)                  = notRequired "GetCustomAttributes" this.Name
    override this.GetCustomAttributes(_attributeType, _inherit)   = notRequired "GetCustomAttributes" this.Name
    override this.IsDefined(_attributeType, _inherit)             = notRequired "IsDefined" this.Name

    override this.SetValue(_obj, _value, _invokeAttr, _binder, _culture) = notRequired "SetValue" this.Name
    override this.GetValue(_obj) : obj = notRequired "GetValue" this.Name
    override this.FieldHandle = notRequired "FieldHandle" this.Name

type ProvidedField(fieldName:string,fieldType:Type) = 
    inherit System.Reflection.FieldInfo()
    // State

    let mutable declaringType = null

    let customAttributesImpl = CustomAttributesImpl()
    let mutable fieldAttrs = FieldAttributes.Private
    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddObsoleteAttribute (msg,?isError)         = customAttributesImpl.AddObsolete (msg,defaultArg isError false)
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice

    member this.SetFieldAttributes attrs = fieldAttrs <- attrs
    // Implement overloads
    override this.FieldType = fieldType
    override this.GetRawConstantValue()  = null
    override this.Attributes = fieldAttrs
    override this.Name = fieldName
    override this.DeclaringType = declaringType |> nonNull "ProvidedField.DeclaringType"
    override this.MemberType : MemberTypes = MemberTypes.Field

    override this.ReflectedType                                     = notRequired "ReflectedType" this.Name
    override this.GetCustomAttributes(_inherit)                  = notRequired "GetCustomAttributes" this.Name
    override this.GetCustomAttributes(_attributeType, _inherit)   = notRequired "GetCustomAttributes" this.Name
    override this.IsDefined(_attributeType, _inherit)             = notRequired "IsDefined" this.Name

    override this.SetValue(_obj, _value, _invokeAttr, _binder, _culture) = notRequired "SetValue" this.Name
    override this.GetValue(_obj) : obj = notRequired "GetValue" this.Name
    override this.FieldHandle = notRequired "FieldHandle" this.Name

/// Represents the type constructor in a provided symbol type.
type SymbolKind = 
    | SDArray 
    | Array of int 
    | Pointer 
    | ByRef 
    | Generic of System.Type 
    | FSharpTypeAbbreviation of (System.Reflection.Assembly * string * string[])


/// Represents an array or other symbolic type involving a provided type as the argument.
/// See the type provider spec for the methods that must be implemented.
/// Note that the type provider specification does not require us to implement pointer-equality for provided types.
type ProvidedSymbolType(kind: SymbolKind, args: Type list) =
    inherit Type()

    static member convType (parameters: Type list) (ty:Type) = 
        if ty.IsGenericType then 
            let args = Array.map (ProvidedSymbolType.convType parameters) (ty.GetGenericArguments())
            ProvidedSymbolType(Generic (ty.GetGenericTypeDefinition()), Array.toList args)  :> Type
        elif ty.HasElementType then 
            let ety = ProvidedSymbolType.convType parameters (ty.GetElementType()) 
            if ty.IsArray then 
                let rank = ty.GetArrayRank()
                if rank = 1 then ProvidedSymbolType(SDArray,[ety]) :> Type
                else ProvidedSymbolType(Array rank,[ety]) :> Type
            elif ty.IsPointer then ProvidedSymbolType(Pointer,[ety]) :> Type
            elif ty.IsByRef then ProvidedSymbolType(ByRef,[ety]) :> Type
            else ty
        elif ty.IsGenericParameter then 
            if ty.GenericParameterPosition <= parameters.Length - 1 then 
                parameters.[ty.GenericParameterPosition]
            else
                ty
        else ty

    override this.FullName =   
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> arg.FullName + "[]" 
        | SymbolKind.Array _,[arg] -> arg.FullName + "[*]" 
        | SymbolKind.Pointer,[arg] -> arg.FullName + "*" 
        | SymbolKind.ByRef,[arg] -> arg.FullName + "&"
        | SymbolKind.Generic gty, args -> gty.FullName + "[" + (args |> List.map (fun arg -> arg.ToString()) |> String.concat ",") + "]"
        | SymbolKind.FSharpTypeAbbreviation (_,nsp,path),args -> String.concat "." (Array.append [| nsp |] path) + args.ToString()
        | _ -> failwith "unreachable"
   
    /// Although not strictly required by the type provider specification, this is required when doing basic operations like FullName on
    /// .NET symbolic types made from this type, e.g. when building Nullable<SomeProvidedType[]>.FullName
    override this.DeclaringType =                                                                 
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> arg
        | SymbolKind.Array _,[arg] -> arg
        | SymbolKind.Pointer,[arg] -> arg
        | SymbolKind.ByRef,[arg] -> arg
        | SymbolKind.Generic gty,_ -> gty
        | SymbolKind.FSharpTypeAbbreviation _,_ -> null
        | _ -> failwith "unreachable"

    override this.IsAssignableFrom(otherTy) = 
        match kind with
        | Generic gtd ->
            if otherTy.IsGenericType then
                let otherGtd = otherTy.GetGenericTypeDefinition()
                let otherArgs = otherTy.GetGenericArguments()
                let yes = gtd.Equals(otherGtd) && Seq.forall2 (=) args otherArgs
                yes
                else
                    base.IsAssignableFrom(otherTy)
        | _ -> base.IsAssignableFrom(otherTy)

    override this.Name =
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> arg.Name + "[]" 
        | SymbolKind.Array _,[arg] -> arg.Name + "[*]" 
        | SymbolKind.Pointer,[arg] -> arg.Name + "*" 
        | SymbolKind.ByRef,[arg] -> arg.Name + "&"
        | SymbolKind.Generic gty, args -> gty.FullName + args.ToString()
        | SymbolKind.FSharpTypeAbbreviation (_,_,path),_ -> path.[path.Length-1]
        | _ -> failwith "unreachable"

    override this.BaseType =
        match kind with 
        | SymbolKind.SDArray -> typeof<System.Array>
        | SymbolKind.Array _ -> typeof<System.Array>
        | SymbolKind.Pointer -> typeof<System.ValueType>
        | SymbolKind.ByRef -> typeof<System.ValueType>
        | SymbolKind.Generic gty  -> ProvidedSymbolType.convType args gty.BaseType
        | SymbolKind.FSharpTypeAbbreviation _ -> typeof<obj>

    override this.GetArrayRank() = (match kind with SymbolKind.Array n -> n | SymbolKind.SDArray -> 1 | _ -> invalidOp "non-array type")
    override this.IsArrayImpl() = (match kind with SymbolKind.Array _ | SymbolKind.SDArray -> true | _ -> false)
    override this.IsByRefImpl() = (match kind with SymbolKind.ByRef _ -> true | _ -> false)
    override this.IsPointerImpl() = (match kind with SymbolKind.Pointer _ -> true | _ -> false)
    override this.IsPrimitiveImpl() = false
    override this.IsGenericType = (match kind with SymbolKind.Generic _ -> true | _ -> false)
    override this.GetGenericArguments() = (match kind with SymbolKind.Generic _ -> args |> List.toArray | _ -> invalidOp "non-generic type")
    override this.GetGenericTypeDefinition() = (match kind with SymbolKind.Generic e -> e | _ -> invalidOp "non-generic type")
    override this.IsCOMObjectImpl() = false
    override this.HasElementTypeImpl() = (match kind with SymbolKind.Generic _ -> false | _ -> true)
    override this.GetElementType() = (match kind,args with (SymbolKind.Array _  | SymbolKind.SDArray | SymbolKind.ByRef | SymbolKind.Pointer),[e] -> e | _ -> invalidOp "not an array, pointer or byref type")
    override this.ToString() = this.FullName

    override this.Module : Module                                                                  = notRequired "Module" this.Name
    override this.Assembly = 
        match kind with 
        | SymbolKind.FSharpTypeAbbreviation (assembly,_nsp,_path) -> assembly
        | SymbolKind.Generic gty -> gty.Assembly
        | _ -> notRequired "Assembly" this.Name
    override this.Namespace = 
        match kind with 
        | SymbolKind.FSharpTypeAbbreviation (_assembly,nsp,_path) -> nsp
        | _ -> notRequired "Namespace" this.Name

    override this.GetHashCode()                                                                    = 
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> 10 + hash arg
        | SymbolKind.Array _,[arg] -> 163 + hash arg
        | SymbolKind.Pointer,[arg] -> 283 + hash arg
        | SymbolKind.ByRef,[arg] -> 43904 + hash arg
        | SymbolKind.Generic gty,_ -> 9797 + hash gty + List.sumBy hash args
        | SymbolKind.FSharpTypeAbbreviation _,_ -> 3092
        | _ -> failwith "unreachable"
    member this.Kind = kind
    member this.Args = args
    
    override this.Equals(that:obj) = 
        match that with 
        | :? ProvidedSymbolType as that -> (kind,args) = (that.Kind, that.Args)
        | _ -> false

    override this.GetConstructors _bindingAttr                                                      = notRequired "GetConstructors" this.Name
    override this.GetMethodImpl(_name, _bindingAttr, _binderBinder, _callConvention, _types, _modifiers) = 
        match kind with
        | Generic gtd -> 
            let ty = gtd.GetGenericTypeDefinition().MakeGenericType(Array.ofList args)
            ty.GetMethod(_name, _bindingAttr)
        | _ -> notRequired "GetMethodImpl" this.Name
    override this.GetMembers _bindingAttr                                                           = notRequired "GetMembers" this.Name
    override this.GetMethods _bindingAttr                                                           = notRequired "GetMethods" this.Name
    override this.GetField(_name, _bindingAttr)                                                      = notRequired "GetField" this.Name
    override this.GetFields _bindingAttr                                                            = notRequired "GetFields" this.Name
    override this.GetInterface(_name, _ignoreCase)                                                   = notRequired "GetInterface" this.Name
    override this.GetInterfaces()                                                                  = notRequired "GetInterfaces" this.Name
    override this.GetEvent(_name, _bindingAttr)                                                      = notRequired "GetEvent" this.Name
    override this.GetEvents _bindingAttr                                                            = notRequired "GetEvents" this.Name
    override this.GetProperties _bindingAttr                                                        = notRequired "GetProperties" this.Name
    override this.GetPropertyImpl(_name, _bindingAttr, _binder, _returnType, _types, _modifiers)         = notRequired "GetPropertyImpl" this.Name
    override this.GetNestedTypes _bindingAttr                                                       = notRequired "GetNestedTypes" this.Name
    override this.GetNestedType(_name, _bindingAttr)                                                 = notRequired "GetNestedType" this.Name
    override this.GetAttributeFlagsImpl()                                                          = notRequired "GetAttributeFlagsImpl" this.Name
    override this.UnderlyingSystemType                                                             = 
        match kind with 
        | SymbolKind.SDArray
        | SymbolKind.Array _
        | SymbolKind.Pointer
        | SymbolKind.FSharpTypeAbbreviation _
        | SymbolKind.ByRef -> notRequired "UnderlyingSystemType" this.Name
        | SymbolKind.Generic gty -> gty.UnderlyingSystemType      
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                                                        =  ([| |] :> IList<_>)
#endif
    override this.MemberType                                                                       = notRequired "MemberType" this.Name
    override this.GetMember(_name,_mt,_bindingAttr)                                                = notRequired "GetMember" this.Name
    override this.GUID                                                                             = notRequired "GUID" this.Name
    override this.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "InvokeMember" this.Name
    override this.AssemblyQualifiedName                                                            = notRequired "AssemblyQualifiedName" this.Name
    override this.GetConstructorImpl(_bindingAttr, _binder, _callConvention, _types, _modifiers)   = notRequired "GetConstructorImpl" this.Name
    override this.GetCustomAttributes(_inherit)                                                    = [| |]
    override this.GetCustomAttributes(_attributeType, _inherit)                                    = [| |]
    override this.IsDefined(_attributeType, _inherit)                                              = false

type ProvidedSymbolMethod(genericMethodDefinition: MethodInfo, parameters: Type list) =
    inherit System.Reflection.MethodInfo()

    let convParam (p:ParameterInfo) = 
        { new System.Reflection.ParameterInfo() with
              override this.Name = p.Name
              override this.ParameterType = ProvidedSymbolType.convType parameters p.ParameterType
              override this.Attributes = p.Attributes
              override this.RawDefaultValue = p.RawDefaultValue
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
              override __.GetCustomAttributesData() = p.GetCustomAttributesData()
#endif
        } 
    override this.IsGenericMethod = 
        (if this.DeclaringType.IsGenericType then this.DeclaringType.GetGenericArguments().Length else 0) < parameters.Length
    override this.GetGenericArguments() = 
        Seq.skip (if this.DeclaringType.IsGenericType then this.DeclaringType.GetGenericArguments().Length else 0) parameters |> Seq.toArray 
    override this.GetGenericMethodDefinition() = genericMethodDefinition
    override this.DeclaringType = ProvidedSymbolType.convType parameters genericMethodDefinition.DeclaringType
    override this.ToString() = "Method " + this.Name
    override this.Name = genericMethodDefinition.Name
    override this.MetadataToken = genericMethodDefinition.MetadataToken
    override this.Attributes = genericMethodDefinition.Attributes
    override this.CallingConvention = genericMethodDefinition.CallingConvention
    override this.MemberType = genericMethodDefinition.MemberType

    override this.IsDefined(_attributeType, _inherit) : bool = notRequired "IsDefined" this.Name
    override this.ReturnType = ProvidedSymbolType.convType parameters genericMethodDefinition.ReturnType
    override this.GetParameters() = genericMethodDefinition.GetParameters() |> Array.map convParam
    override this.ReturnParameter = genericMethodDefinition.ReturnParameter |> convParam
    override this.ReturnTypeCustomAttributes                           = notRequired "ReturnTypeCustomAttributes" this.Name
    override this.GetBaseDefinition()                                  = notRequired "GetBaseDefinition" this.Name
    override this.GetMethodImplementationFlags()                       = notRequired "GetMethodImplementationFlags" this.Name
    override this.MethodHandle                                         = notRequired "MethodHandle" this.Name
    override this.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" this.Name
    override this.ReflectedType                                        = notRequired "ReflectedType" this.Name
    override this.GetCustomAttributes(_inherit)                     = notRequired "GetCustomAttributes" this.Name
    override this.GetCustomAttributes(_attributeType, _inherit)      =  notRequired "GetCustomAttributes" this.Name 



type ProvidedTypeBuilder() =
    static member MakeGenericType(genericTypeDefinition, genericArguments) = ProvidedSymbolType(Generic genericTypeDefinition, genericArguments) :> Type
    static member MakeGenericMethod(genericMethodDefinition, genericArguments) = ProvidedSymbolMethod(genericMethodDefinition, genericArguments) :> MethodInfo

[<Class>]
type ProvidedMeasureBuilder() =

    static let theBuilder = ProvidedMeasureBuilder()
    static member Default = theBuilder
    member b.One = typeof<Core.CompilerServices.MeasureOne> 
    member b.Product (m1,m2) = typedefof<Core.CompilerServices.MeasureProduct<_,_>>.MakeGenericType [| m1;m2 |] 
    member b.Inverse m = typedefof<Core.CompilerServices.MeasureInverse<_>>.MakeGenericType [| m |] 
    member b.Ratio (m1, m2) = b.Product(m1, b.Inverse m2)
    member b.Square m = b.Product(m, m)
    member b.SI m = 
        match typedefof<list<int>>.Assembly.GetType("Microsoft.FSharp.Data.UnitSystems.SI.UnitNames."+m) with 
        | null ->         
            ProvidedSymbolType
               (SymbolKind.FSharpTypeAbbreviation
                   (typeof<Core.CompilerServices.MeasureOne>.Assembly,
                    "Microsoft.FSharp.Data.UnitSystems.SI.UnitNames", 
                    [| m |]), 
                []) :> Type
        | v -> v
    member b.AnnotateType (basicType, annotation) = ProvidedSymbolType(Generic basicType, annotation) :> Type



[<RequireQualifiedAccess>]
type TypeContainer =
  | Namespace of Assembly * string // namespace
  | Type of System.Type
  | TypeToBeDecided

module GlobalProvidedAssemblyElementsTable = 
    let theTable = Dictionary<Assembly, Lazy<byte[]>>()

type ProvidedTypeDefinition(container:TypeContainer,className : string, baseType  : Type option) as this =
    inherit Type()
    // state
    let mutable attributes   = 
        TypeAttributes.Public ||| 
        TypeAttributes.Class ||| 
        TypeAttributes.Sealed |||
        enum (int32 TypeProviderTypeAttributes.IsErased)


    let mutable baseType   =  lazy baseType
    let mutable membersKnown   = ResizeArray<MemberInfo>()
    let mutable membersQueue   = ResizeArray<(unit -> list<MemberInfo>)>()       
    let mutable staticParams = [ ] 
    let mutable staticParamsApply = None
    let mutable container = container
    let mutable interfaceImpls = ResizeArray<Type>()
    let mutable interfaceImplsDelayed = ResizeArray<unit -> list<Type>>()
    let mutable methodOverrides = ResizeArray<ProvidedMethod * MethodInfo>()

    // members API
    let getMembers() = 
        if membersQueue.Count > 0 then 
            let elems = membersQueue |> Seq.toArray // take a copy in case more elements get added
            membersQueue.Clear()
            for  f in elems do
                for i in f() do 
                    membersKnown.Add i       
                    match i with
                    | :? ProvidedProperty    as p -> 
                        if p.CanRead then membersKnown.Add (p.GetGetMethod true)
                        if p.CanWrite then membersKnown.Add (p.GetSetMethod true)
                    | :? ProvidedEvent       as e -> 
                        membersKnown.Add (e.GetAddMethod true)
                        membersKnown.Add (e.GetRemoveMethod true)
                    | _ -> ()
        
        membersKnown.ToArray()

            // members API
    let getInterfaces() = 
        if interfaceImplsDelayed.Count > 0 then 
            let elems = interfaceImplsDelayed |> Seq.toArray // take a copy in case more elements get added
            interfaceImplsDelayed.Clear()
            for  f in elems do
                for i in f() do 
                    interfaceImpls.Add i       
        
        interfaceImpls.ToArray()

    let mutable theAssembly = 
      lazy
        match container with
        | TypeContainer.Namespace (theAssembly, rootNamespace) ->
            if theAssembly = null then failwith "Null assemblies not allowed"
            if rootNamespace<>null && rootNamespace.Length=0 then failwith "Use 'null' for global namespace"
            theAssembly
        | TypeContainer.Type superTy -> superTy.Assembly
        | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" this.Name)
    
    let rootNamespace =
      lazy 
        match container with
        | TypeContainer.Namespace (_,rootNamespace) -> rootNamespace
        | TypeContainer.Type enclosingTyp           -> enclosingTyp.Namespace
        | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" this.Name)

    let declaringType =
      lazy
        match container with
        | TypeContainer.Namespace _ -> null
        | TypeContainer.Type enclosingTyp           -> enclosingTyp
        | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" this.Name)

    let fullName = 
      lazy
        match container with
        | TypeContainer.Type declaringType -> declaringType.FullName + "+" + className
        | TypeContainer.Namespace (_,namespaceName) ->  
            if namespaceName="" then failwith "use null for global namespace"
            match namespaceName with
            | null -> className
            | _    -> namespaceName + "." + className
        | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" this.Name)
                                                            
    let patchUpAddedMemberInfo (this:Type) (m:MemberInfo) = 
        match m with
        | :? ProvidedConstructor as c -> c.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
        | :? ProvidedMethod      as m -> m.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
        | :? ProvidedProperty    as p -> p.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
        | :? ProvidedEvent       as e -> e.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
        | :? ProvidedTypeDefinition  as t -> t.DeclaringTypeImpl <- this 
        | :? ProvidedLiteralField as l -> l.DeclaringTypeImpl <- this
        | :? ProvidedField as l -> l.DeclaringTypeImpl <- this
        | _ -> ()

    let customAttributesImpl = CustomAttributesImpl()
    member this.AddXmlDocComputed xmlDoc                    = customAttributesImpl.AddXmlDocComputed xmlDoc
    member this.AddXmlDocDelayed xmlDoc                     = customAttributesImpl.AddXmlDocDelayed xmlDoc
    member this.AddXmlDoc xmlDoc                            = customAttributesImpl.AddXmlDoc xmlDoc
    member this.AddObsoleteAttribute (msg,?isError)         = customAttributesImpl.AddObsolete (msg,defaultArg isError false)
    member this.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
    member this.HideObjectMethods with set v                = customAttributesImpl.HideObjectMethods <- v
    member __.GetCustomAttributesDataImpl() = customAttributesImpl.GetCustomAttributesData()
#if FX_NO_CUSTOMATTRIBUTEDATA
#else
    override this.GetCustomAttributesData()                 = customAttributesImpl.GetCustomAttributesData()
#endif

    member this.ResetEnclosingType (ty) = 
        container <- TypeContainer.Type ty
    new (assembly:Assembly,namespaceName,className,baseType) = new ProvidedTypeDefinition(TypeContainer.Namespace (assembly,namespaceName), className, baseType)
    new (className,baseType) = new ProvidedTypeDefinition(TypeContainer.TypeToBeDecided, className, baseType)
    // state ops

    member this.SetBaseType t = baseType <- lazy Some t
    member this.SetBaseTypeDelayed t = baseType <- t
    member this.SetAttributes x = attributes <- x
    // Add MemberInfos
    member this.AddMembersDelayed(makeMS : unit -> list<#MemberInfo>) =
        membersQueue.Add (fun () -> makeMS() |> List.map (fun x -> patchUpAddedMemberInfo this x; x :> MemberInfo ))
    member this.AddMembers(ms:list<#MemberInfo>) = (* strict *)
        ms |> List.iter (patchUpAddedMemberInfo this) // strict: patch up now
        membersQueue.Add (fun () -> ms |> List.map (fun x -> x :> MemberInfo))
    member this.AddMember(m:MemberInfo) = this.AddMembers [m]    
    member this.AddMemberDelayed(m : unit -> #MemberInfo) = this.AddMembersDelayed(fun () -> [m()])

    member this.AddAssemblyTypesAsNestedTypesDelayed (assemblyf : unit -> System.Reflection.Assembly)  = 
            let bucketByPath nodef tipf (items: (string list * 'Value) list) = 
                // Find all the items with an empty key list and call 'tipf' 
                let tips = 
                    [ for (keylist,v) in items do 
                         match keylist with 
                         | [] -> yield tipf v
                         | _ -> () ]

                // Find all the items with a non-empty key list. Bucket them together by
                // the first key. For each bucket, call 'nodef' on that head key and the bucket.
                let nodes = 
                    let buckets = new Dictionary<_,_>(10)
                    for (keylist,v) in items do
                        match keylist with 
                        | [] -> ()
                        | key::rest -> 
                            buckets.[key] <- (rest,v) :: (if buckets.ContainsKey key then buckets.[key] else []);

                    [ for (KeyValue(key,items)) in buckets -> nodef key items ]

                tips @ nodes
            this.AddMembersDelayed (fun _ -> 
                let topTypes = [ for ty in assemblyf().GetTypes() do 
                                        if not ty.IsNested then
                                             let namespaceParts = match ty.Namespace with null -> [] | s -> s.Split '.' |> Array.toList
                                             yield namespaceParts,  ty ]
                let rec loop types = 
                    types 
                    |> bucketByPath
                        (fun namespaceComponent typesUnderNamespaceComponent -> 
                            let t = ProvidedTypeDefinition(namespaceComponent, baseType = Some typeof<obj>)
                            t.AddMembers (loop typesUnderNamespaceComponent)
                            (t :> Type))
                        (fun ty -> ty)
                loop topTypes)

    /// Abstract a type to a parametric-type. Requires "formal parameters" and "instantiation function".
    member this.DefineStaticParameters(staticParameters : list<ProvidedStaticParameter>, apply    : (string -> obj[] -> ProvidedTypeDefinition)) =
        staticParams      <- staticParameters 
        staticParamsApply <- Some apply

    /// Get ParameterInfo[] for the parametric type parameters (//s GetGenericParameters)
    member this.GetStaticParameters() = [| for p in staticParams -> p :> ParameterInfo |]

    /// Instantiate parametrics type
    member this.MakeParametricType(name:string,args:obj[]) =
        if staticParams.Length>0 then
            if staticParams.Length <> args.Length then
                failwith (sprintf "ProvidedTypeDefinition: expecting %d static parameters but given %d for type %s" staticParams.Length args.Length (fullName.Force()))
            match staticParamsApply with
            | None -> failwith "ProvidedTypeDefinition: DefineStaticParameters was not called"
            | Some f -> f name args

        else
            failwith (sprintf "ProvidedTypeDefinition: static parameters supplied but not expected for %s" (fullName.Force()))

    member this.DeclaringTypeImpl
        with set x = 
            match container with TypeContainer.TypeToBeDecided -> () | _ -> failwith (sprintf "container type for '%s' was already set to '%s'" this.FullName x.FullName); 
            container <- TypeContainer.Type  x

    // Implement overloads
    override this.Assembly = theAssembly.Force()
    member this.SetAssembly assembly = theAssembly <- lazy assembly
    member this.SetAssemblyLazy assembly = theAssembly <- assembly
    override this.FullName = fullName.Force()
    override this.Namespace = rootNamespace.Force()
    override this.BaseType = match baseType.Value with Some ty -> ty | None -> null
    
    // Constructors
    override this.GetConstructors bindingAttr = 
        [| for m in this.GetMembers bindingAttr do                
                if m.MemberType = MemberTypes.Constructor then
                    yield (m :?> ConstructorInfo) |]
    // Methods
    override this.GetMethodImpl(name, bindingAttr, _binderBinder, _callConvention, _types, _modifiers) : MethodInfo = 
        let membersWithName = 
            [ for m in this.GetMembers(bindingAttr) do                
                if m.MemberType.HasFlag(MemberTypes.Method) && m.Name = name then
                    yield  m ]
        match membersWithName with 
        | []        -> null
        | [meth]    -> meth :?> MethodInfo
        | _several   -> failwith "GetMethodImpl. not support overloads"

    override this.GetMethods bindingAttr = 
        this.GetMembers bindingAttr 
        |> Array.filter (fun m -> m.MemberType.HasFlag(MemberTypes.Method)) 
        |> Array.map (fun m -> m :?> MethodInfo)

    // Fields
    override this.GetField(name, bindingAttr) = 
        let fields = [| for m in this.GetMembers bindingAttr do
                            if m.MemberType.HasFlag(MemberTypes.Field) && (name = null || m.Name = name) then // REVIEW: name = null. Is that a valid query?!
                                yield m |] 
        if fields.Length > 0 then fields.[0] :?> FieldInfo else null

    override this.GetFields bindingAttr = 
        [| for m in this.GetMembers bindingAttr do if m.MemberType.HasFlag(MemberTypes.Field) then yield m :?> FieldInfo |]

    override this.GetInterface(_name, _ignoreCase) = notRequired "GetInterface" this.Name

    override this.GetInterfaces() = 
        [| yield! getInterfaces()  |]

    member this.GetInterfaceImplementations() = 
        [| yield! getInterfaces() |]

    member this.AddInterfaceImplementation ityp = interfaceImpls.Add ityp
    member this.AddInterfaceImplementationsDelayed itypf = interfaceImplsDelayed.Add itypf
    member this.GetMethodOverrides() = 
        [| yield! methodOverrides |]
    member this.DefineMethodOverride (bodyMethInfo,declMethInfo) = methodOverrides.Add (bodyMethInfo, declMethInfo)

    // Events
    override this.GetEvent(name, bindingAttr) = 
        let events = this.GetMembers bindingAttr 
                     |> Array.filter(fun m -> m.MemberType.HasFlag(MemberTypes.Event) && (name = null || m.Name = name)) 
        if events.Length > 0 then events.[0] :?> EventInfo else null

    override this.GetEvents bindingAttr = 
        [| for m in this.GetMembers bindingAttr do if m.MemberType.HasFlag(MemberTypes.Event) then yield downcast m |]    

    // Properties
    override this.GetProperties bindingAttr = 
        [| for m in this.GetMembers bindingAttr do if m.MemberType.HasFlag(MemberTypes.Property) then yield downcast m |]

    override this.GetPropertyImpl(name, bindingAttr, binder, returnType, types, modifiers) = 
        if returnType <> null then failwith "Need to handle specified return type in GetPropertyImpl"
        if types      <> null then failwith "Need to handle specified parameter types in GetPropertyImpl"
        if modifiers  <> null then failwith "Need to handle specified modifiers in GetPropertyImpl"
        if binder  <> null then failwith "Need to handle binder in GetPropertyImpl"
        let props = this.GetMembers bindingAttr |> Array.filter(fun m -> m.MemberType.HasFlag(MemberTypes.Property) && (name = null || m.Name = name))  // Review: nam = null, valid query!?
        if props.Length > 0 then
            props.[0] :?> PropertyInfo
        else
            null
    // Nested Types
    override this.MakeArrayType() = ProvidedSymbolType(SymbolKind.SDArray, [this]) :> Type
    override this.MakeArrayType arg = ProvidedSymbolType(SymbolKind.Array arg, [this]) :> Type
    override this.MakePointerType() = ProvidedSymbolType(SymbolKind.Pointer, [this]) :> Type
    override this.MakeByRefType() = ProvidedSymbolType(SymbolKind.ByRef, [this]) :> Type

    // The binding attributes are always set to DeclaredOnly ||| Static ||| Instance ||| Public when GetMembers is called directly by the F# compiler
    // However, it's possible for the framework to generate other sets of flags in some corner cases (e.g. via use of `enum` with a provided type as the target)
    override this.GetMembers bindingAttr = 
        let mems = 
            getMembers() 
            |> Array.filter (fun mem -> 
                                let isStatic, isPublic = 
                                    match mem with
                                    | :? FieldInfo as f -> f.IsStatic, f.IsPublic
                                    | :? MethodInfo as m -> m.IsStatic, m.IsPublic
                                    | :? ConstructorInfo as c -> c.IsStatic, c.IsPublic
                                    | :? PropertyInfo as p -> 
                                        let m = if p.CanRead then p.GetGetMethod() else p.GetSetMethod()
                                        m.IsStatic, m.IsPublic
                                    | :? EventInfo as e -> 
                                        let m = e.GetAddMethod()
                                        m.IsStatic, m.IsPublic
                                    | :? Type as ty -> 
                                        true, ty.IsNestedPublic
                                    | _ -> failwith (sprintf "Member %O is of unexpected type" mem)
                                bindingAttr.HasFlag(if isStatic then BindingFlags.Static else BindingFlags.Instance) &&
                                (
                                    (bindingAttr.HasFlag(BindingFlags.Public) && isPublic) || (bindingAttr.HasFlag(BindingFlags.NonPublic) && not isPublic)
                                ))

        if bindingAttr.HasFlag(BindingFlags.DeclaredOnly) || this.BaseType = null then mems
        else 
            let baseMems = this.BaseType.GetMembers bindingAttr
            Array.append mems baseMems

    override this.GetNestedTypes bindingAttr = 
        this.GetMembers bindingAttr 
        |> Array.filter(fun m -> 
            m.MemberType.HasFlag(MemberTypes.NestedType) || 
            // Allow 'fake' nested types that are actually real .NET types
            m.MemberType.HasFlag(MemberTypes.TypeInfo)) |> Array.map(fun m -> m :?> Type)

    override this.GetMember(name,mt,_bindingAttr) = 
        let mt = 
            if mt &&& MemberTypes.NestedType = MemberTypes.NestedType then 
                mt ||| MemberTypes.TypeInfo 
            else
                mt
        getMembers() |> Array.filter(fun m->0<>(int(m.MemberType &&& mt)) && m.Name = name)
        
    override this.GetNestedType(name, bindingAttr) = 
        let nt = this.GetMember(name, MemberTypes.NestedType ||| MemberTypes.TypeInfo, bindingAttr)
        match nt.Length with
        | 0 -> null
        | 1 -> downcast nt.[0]
        | _ -> failwith (sprintf "There is more than one nested type called '%s' in type '%s'" name this.FullName)

    // Attributes, etc..
    override this.GetAttributeFlagsImpl() = adjustTypeAttributes attributes this.IsNested 
    override this.IsArrayImpl() = false
    override this.IsByRefImpl() = false
    override this.IsPointerImpl() = false
    override this.IsPrimitiveImpl() = false
    override this.IsCOMObjectImpl() = false
    override this.HasElementTypeImpl() = false
    override this.UnderlyingSystemType = typeof<System.Type>
    override this.Name = className
    override this.DeclaringType = declaringType.Force()
    override this.MemberType = if this.IsNested then MemberTypes.NestedType else MemberTypes.TypeInfo      
    override this.GetHashCode() = rootNamespace.GetHashCode() ^^^ className.GetHashCode()
    override this.Equals(that:obj) = 
        match that with
        | null              -> false
        | :? ProvidedTypeDefinition as ti -> System.Object.ReferenceEquals(this,ti)
        | _                 -> false

    override this.GetGenericArguments() = [||] 
    override this.ToString() = this.Name
    

    override this.Module : Module = notRequired "Module" this.Name
    override this.GUID                                                                                   = Guid.Empty
    override this.GetConstructorImpl(_bindingAttr, _binder, _callConvention, _types, _modifiers)         = null
    override this.GetCustomAttributes(_inherit)                                                          = [| |]
    override this.GetCustomAttributes(_attributeType, _inherit)                                          = [| |]
    override this.IsDefined(_attributeType: Type, _inherit)                                              = false

    override this.GetElementType()                                                                                  = notRequired "Module" this.Name
    override this.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "Module" this.Name
    override this.AssemblyQualifiedName                                                                             = notRequired "Module" this.Name
    member this.IsErased 
        with get() = (attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased)) <> enum 0
        and set v = 
           if v then attributes <- attributes ||| enum (int32 TypeProviderTypeAttributes.IsErased)
           else attributes <- attributes &&& ~~~(enum (int32 TypeProviderTypeAttributes.IsErased))

    member this.SuppressRelocation 
        with get() = (attributes &&& enum (int32 TypeProviderTypeAttributes.SuppressRelocate)) <> enum 0
        and set v = 
           if v then attributes <- attributes ||| enum (int32 TypeProviderTypeAttributes.SuppressRelocate)
           else attributes <- attributes &&& ~~~(enum (int32 TypeProviderTypeAttributes.SuppressRelocate))

type AssemblyGenerator(assemblyFileName) = 
    let assemblyShortName = Path.GetFileNameWithoutExtension assemblyFileName
    let assemblyName = AssemblyName assemblyShortName
#if FX_NO_LOCAL_FILESYSTEM
    let assembly = 
        System.AppDomain.CurrentDomain.DefineDynamicAssembly(name=assemblyName,access=AssemblyBuilderAccess.Run)
    let assemblyMainModule = 
        assembly.DefineDynamicModule("MainModule")
#else
    let assembly = 
        System.AppDomain.CurrentDomain.DefineDynamicAssembly(name=assemblyName,access=(AssemblyBuilderAccess.Save ||| AssemblyBuilderAccess.Run),dir=Path.GetDirectoryName assemblyFileName)
    let assemblyMainModule = 
        assembly.DefineDynamicModule("MainModule", Path.GetFileName assemblyFileName)
#endif
    let typeMap = Dictionary<ProvidedTypeDefinition,TypeBuilder>(HashIdentity.Reference)
    let typeMapExtra = Dictionary<string,TypeBuilder>(HashIdentity.Structural)
    let uniqueLambdaTypeName() = 
        // lambda name should be unique across all types that all type provider might contribute in result assembly
        sprintf "Lambda%O" (Guid.NewGuid()) 
    member __.Assembly = assembly :> Assembly
    /// Emit the given provided type definitions into an assembly and adjust 'Assembly' property of all type definitions to return that
    /// assembly.
    member __.Generate(providedTypeDefinitions:(ProvidedTypeDefinition * string list option) list) = 
        let ALL = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
        // phase 1 - set assembly fields and emit type definitions
        begin 
            let rec typeMembers (tb:TypeBuilder)  (td : ProvidedTypeDefinition) = 
                for ntd in td.GetNestedTypes(ALL) do
                    nestedType tb ntd

            and nestedType (tb:TypeBuilder)  (ntd : Type) = 
                match ntd with 
                | :? ProvidedTypeDefinition as pntd -> 
                    if pntd.IsErased then invalidOp ("The nested provided type "+pntd.Name+"is marked as erased and cannot be converted to a generated type. Set 'IsErased' to false on the ProvidedTypeDefinition")
                    // Adjust the attributes - we're codegen'ing this type as nested
                    let attributes = adjustTypeAttributes ntd.Attributes true
                    let ntb = tb.DefineNestedType(pntd.Name,attr=attributes)
                    pntd.SetAssembly null
                    typeMap.[pntd] <- ntb
                    typeMembers ntb pntd
                | _ -> ()
                     
            for (pt,enclosingGeneratedTypeNames) in providedTypeDefinitions do 
              match enclosingGeneratedTypeNames with 
              | None -> 
                // Filter out the additional TypeProviderTypeAttributes flags
                let attributes = pt.Attributes &&& ~~~(enum (int32 TypeProviderTypeAttributes.SuppressRelocate))
                                                &&& ~~~(enum (int32 TypeProviderTypeAttributes.IsErased))
                // Adjust the attributes - we're codegen'ing as non-nested
                let attributes = adjustTypeAttributes attributes false 
                let tb = assemblyMainModule.DefineType(name=pt.FullName,attr=attributes) 
                pt.SetAssembly null
                typeMap.[pt] <- tb
                typeMembers tb pt 
              | Some ns -> 
                let otb,_ = 
                    ((None,""),ns) ||> List.fold (fun (otb:TypeBuilder option,fullName) n -> 
                        let fullName = if fullName = "" then n else fullName + "." + n
                        let priorType = if typeMapExtra.ContainsKey(fullName) then Some typeMapExtra.[fullName]  else None
                        let tb = 
                            match priorType with 
                            | Some tbb -> tbb 
                            | None ->
                            // OK, the implied nested type is not defined, define it now
                            let attributes = 
                                  TypeAttributes.Public ||| 
                                  TypeAttributes.Class ||| 
                                  TypeAttributes.Sealed 
                            // Filter out the additional TypeProviderTypeAttributes flags
                            let attributes = adjustTypeAttributes attributes otb.IsSome
                            let tb = 
                                match otb with 
                                | None -> assemblyMainModule.DefineType(name=n,attr=attributes) 
                                | Some (otb:TypeBuilder) -> otb.DefineNestedType(name=n,attr=attributes)
                            typeMapExtra.[fullName] <- tb
                            tb
                        (Some tb, fullName))
                nestedType otb.Value pt
        end
        let rec convType (ty:Type) = 
            match ty with 
            | :? ProvidedTypeDefinition as ptd ->   
                if typeMap.ContainsKey ptd then typeMap.[ptd] :> Type else ty
            | _ -> 
                if ty.IsGenericType then ty.GetGenericTypeDefinition().MakeGenericType (Array.map convType (ty.GetGenericArguments()))
                elif ty.HasElementType then 
                    let ety = convType (ty.GetElementType()) 
                    if ty.IsArray then 
                        let rank = ty.GetArrayRank()
                        if rank = 1 then ety.MakeArrayType() 
                        else ety.MakeArrayType rank 
                    elif ty.IsPointer then ety.MakePointerType() 
                    elif ty.IsByRef then ety.MakeByRefType()
                    else ty
                else ty

        let ctorMap = Dictionary<ProvidedConstructor, ConstructorBuilder>(HashIdentity.Reference)
        let methMap = Dictionary<ProvidedMethod, MethodBuilder>(HashIdentity.Reference)
        let fieldMap = Dictionary<ProvidedField, FieldBuilder>(HashIdentity.Reference)

        let iterateTypes f = 
            let rec typeMembers (ptd : ProvidedTypeDefinition) = 
                let tb = typeMap.[ptd] 
                f tb (Some ptd)
                for ntd in ptd.GetNestedTypes(ALL) do
                    nestedType ntd

            and nestedType (ntd : Type) = 
                match ntd with 
                | :? ProvidedTypeDefinition as pntd -> typeMembers pntd
                | _ -> ()
                     
            for (pt,enclosingGeneratedTypeNames) in providedTypeDefinitions do 
              match enclosingGeneratedTypeNames with 
              | None -> 
                typeMembers pt 
              | Some ns -> 
                let _fullName  = 
                    ("",ns) ||> List.fold (fun fullName n -> 
                        let fullName = if fullName = "" then n else fullName + "." + n
                        f typeMapExtra.[fullName] None
                        fullName)
                nestedType pt
        
        
        // phase 1b - emit base types
        iterateTypes (fun tb ptd -> 
            match ptd with 
            | None -> ()
            | Some ptd -> 
            match ptd.BaseType with null -> () | bt -> tb.SetParent(convType bt))

        let defineCustomAttrs f (cattrs: IList<CustomAttributeData>) = 
            for attr in cattrs do
                let constructorArgs = [ for x in attr.ConstructorArguments -> x.Value ]
                let namedProps,namedPropVals = [ for x in attr.NamedArguments do match x.MemberInfo with :? PropertyInfo as pi -> yield (pi, x.TypedValue.Value) | _ -> () ] |> List.unzip
                let namedFields,namedFieldVals = [ for x in attr.NamedArguments do match x.MemberInfo with :? FieldInfo as pi -> yield (pi, x.TypedValue.Value) | _ -> () ] |> List.unzip
                let cab = CustomAttributeBuilder(attr.Constructor, Array.ofList constructorArgs, Array.ofList namedProps, Array.ofList namedPropVals, Array.ofList namedFields, Array.ofList namedFieldVals)
                f cab

        // phase 2 - emit member definitions
        iterateTypes (fun tb ptd -> 
            match ptd with 
            | None -> ()
            | Some ptd -> 
            for cinfo in ptd.GetConstructors(ALL) do
                match cinfo with 
                | :? ProvidedConstructor as pcinfo when not (ctorMap.ContainsKey pcinfo)  ->
                    let cb =
                        if pcinfo.IsTypeInitializer then
                            if (cinfo.GetParameters()).Length <> 0 then failwith "Type initializer should not have parameters"
                            tb.DefineTypeInitializer()
                        else 
                            let cb = tb.DefineConstructor(cinfo.Attributes, CallingConventions.Standard, [| for p in cinfo.GetParameters() -> convType p.ParameterType |])
                            for (i,p) in cinfo.GetParameters() |> Seq.mapi (fun i x -> (i,x)) do
                                cb.DefineParameter(i+1, ParameterAttributes.None, p.Name) |> ignore
                            cb
                    ctorMap.[pcinfo] <- cb
                | _ -> () 
                    
            for finfo in ptd.GetFields(ALL) do
                match finfo with 
                | :? ProvidedField as pfinfo when not (fieldMap.ContainsKey pfinfo)  -> 
                    let fb = tb.DefineField(finfo.Name, convType finfo.FieldType, finfo.Attributes)
                    let cattr = pfinfo.GetCustomAttributesDataImpl() 
                    defineCustomAttrs fb.SetCustomAttribute cattr
                    fieldMap.[pfinfo] <- fb
                | _ -> () 
            for minfo in ptd.GetMethods(ALL) do
                match minfo with 
                | :? ProvidedMethod as pminfo when not (methMap.ContainsKey pminfo)  -> 
                    let mb = tb.DefineMethod(minfo.Name, minfo.Attributes, convType minfo.ReturnType, [| for p in minfo.GetParameters() -> convType p.ParameterType |])
                    for (i,p) in minfo.GetParameters() |> Seq.mapi (fun i x -> (i,x)) do
                        mb.DefineParameter(i+1, ParameterAttributes.None, p.Name) |> ignore
                    methMap.[pminfo] <- mb
                | _ -> () 

            for ityp in ptd.GetInterfaceImplementations() do
                tb.AddInterfaceImplementation ityp)

        // phase 3 - emit member code
        iterateTypes (fun  tb ptd -> 
            match ptd with 
            | None -> ()
            | Some ptd -> 
            let cattr = ptd.GetCustomAttributesDataImpl() 
            defineCustomAttrs tb.SetCustomAttribute cattr
            // Allow at most one constructor, and use its arguments as the fields of the type
            let ctors =
                ptd.GetConstructors(ALL) // exclude type initializer
                |> Seq.choose (function :? ProvidedConstructor as pcinfo when not pcinfo.IsTypeInitializer -> Some pcinfo | _ -> None) 
                |> Seq.toList
            let implictCtorArgs =
                match ctors  |> List.filter (fun x -> x.IsImplicitCtor)  with
                | [] -> []
                | [ pcinfo ] -> [ for p in pcinfo.GetParameters() -> p ]
                | _ -> failwith "at most one implicit constructor allowed"

            let implicitCtorArgsAsFields = 
                [ for ctorArg in implictCtorArgs -> 
                      tb.DefineField(ctorArg.Name, convType ctorArg.ParameterType, FieldAttributes.Private) ]
            
            let rec emitLambda(callSiteIlg : ILGenerator, v : Quotations.Var, body : Quotations.Expr, freeVars : seq<Quotations.Var>, locals : Dictionary<_, LocalBuilder>, parameters) =
                let lambda = assemblyMainModule.DefineType(uniqueLambdaTypeName(), TypeAttributes.Class)
                let baseType = typedefof<FSharpFunc<_, _>>.MakeGenericType(v.Type, body.Type)
                lambda.SetParent(baseType)
                let ctor = lambda.DefineDefaultConstructor(MethodAttributes.Public)
                let decl = baseType.GetMethod "Invoke"
                let paramTypes = [| for p in decl.GetParameters() -> p.ParameterType |]
                let invoke = lambda.DefineMethod("Invoke", MethodAttributes.Virtual ||| MethodAttributes.Final ||| MethodAttributes.Public, decl.ReturnType, paramTypes)
                lambda.DefineMethodOverride(invoke, decl)

                // promote free vars to fields
                let fields = ResizeArray()
                for v in freeVars do
                    let f = lambda.DefineField(v.Name, v.Type, FieldAttributes.Assembly)
                    fields.Add(v, f)

                let copyOfLocals = Dictionary()
                
                let ilg = invoke.GetILGenerator()
                for (v, f) in fields do
                    let l = ilg.DeclareLocal(v.Type)
                    ilg.Emit(OpCodes.Ldarg_0)
                    ilg.Emit(OpCodes.Ldfld, f)
                    ilg.Emit(OpCodes.Stloc, l)
                    copyOfLocals.[v] <- l

                let expectedState = if (invoke.ReturnType = typeof<System.Void>) then ExpectedStackState.Empty else ExpectedStackState.Value
                emitExpr (ilg, copyOfLocals, [| Quotations.Var("this", lambda); v|]) expectedState body
                ilg.Emit(OpCodes.Ret) 

                let ty = lambda.CreateType()

                callSiteIlg.Emit(OpCodes.Newobj, ctor)
                for (v, f) in fields do
                    callSiteIlg.Emit(OpCodes.Dup)
                    match locals.TryGetValue v with
                    | true, loc -> 
                        callSiteIlg.Emit(OpCodes.Ldloc, loc)
                    | false, _ -> 
                        let index = parameters |> Array.findIndex ((=) v)
                        callSiteIlg.Emit(OpCodes.Ldarg, index)
                    callSiteIlg.Emit(OpCodes.Stfld, f)

            and emitExpr (ilg: ILGenerator, locals:Dictionary<Quotations.Var,LocalBuilder>, parameterVars) expectedState expr = 
                let pop () = ilg.Emit(OpCodes.Pop)
                let popIfEmptyExpected s = if isEmpty s then pop()
                let emitConvIfNecessary t1 = 
                    if t1 = typeof<int16> then
                        ilg.Emit(OpCodes.Conv_I2)
                    elif t1 = typeof<uint16> then
                        ilg.Emit(OpCodes.Conv_U2)
                    elif t1 = typeof<sbyte> then
                        ilg.Emit(OpCodes.Conv_I1)
                    elif t1 = typeof<byte> then
                        ilg.Emit(OpCodes.Conv_U1)
                /// emits given expression to corresponding IL
                let rec emit (expectedState : ExpectedStackState) (expr: Quotations.Expr) = 
                    match expr with 
                    | Quotations.Patterns.ForIntegerRangeLoop(loopVar, first, last, body) ->
                      // for(loopVar = first..last) body
                      let lb = 
                          match locals.TryGetValue loopVar with
                          | true, lb -> lb
                          | false, _ ->
                              let lb = ilg.DeclareLocal(convType loopVar.Type)
                              locals.Add(loopVar, lb)
                              lb

                      // loopVar = first
                      emit ExpectedStackState.Value first
                      ilg.Emit(OpCodes.Stloc, lb)

                      let before = ilg.DefineLabel()
                      let after = ilg.DefineLabel()

                      ilg.MarkLabel before
                      ilg.Emit(OpCodes.Ldloc, lb)
                            
                      emit ExpectedStackState.Value last
                      ilg.Emit(OpCodes.Bgt, after)

                      emit ExpectedStackState.Empty body

                      // loopVar++
                      ilg.Emit(OpCodes.Ldloc, lb)
                      ilg.Emit(OpCodes.Ldc_I4_1)
                      ilg.Emit(OpCodes.Add)
                      ilg.Emit(OpCodes.Stloc, lb)

                      ilg.Emit(OpCodes.Br, before)
                      ilg.MarkLabel(after)

                    | Quotations.Patterns.NewArray(elementTy, elements) ->
                      ilg.Emit(OpCodes.Ldc_I4, List.length elements)
                      ilg.Emit(OpCodes.Newarr, convType elementTy)

                      elements 
                      |> List.iteri (fun i el ->
                          ilg.Emit(OpCodes.Dup)
                          ilg.Emit(OpCodes.Ldc_I4, i)
                          emit ExpectedStackState.Value el
                          ilg.Emit(OpCodes.Stelem, convType elementTy)
                          )

                      popIfEmptyExpected expectedState

                    | Quotations.Patterns.WhileLoop(cond, body) ->
                      let before = ilg.DefineLabel()
                      let after = ilg.DefineLabel()

                      ilg.MarkLabel before
                      emit ExpectedStackState.Value cond
                      ilg.Emit(OpCodes.Brfalse, after)
                      emit ExpectedStackState.Empty body
                      ilg.Emit(OpCodes.Br, before)

                      ilg.MarkLabel after

                    | Quotations.Patterns.Var v -> 
                        if isEmpty expectedState then () else
                        let methIdx = parameterVars |> Array.tryFindIndex (fun p -> p = v) 
                        match methIdx with 
                        | Some idx -> 
                            ilg.Emit((if isAddress expectedState then OpCodes.Ldarga else OpCodes.Ldarg), idx)
                        | None -> 
                        let implicitCtorArgFieldOpt = implicitCtorArgsAsFields |> List.tryFind (fun f -> f.Name = v.Name) 
                        match implicitCtorArgFieldOpt with 
                        | Some ctorArgField -> 
                            ilg.Emit(OpCodes.Ldarg_0)
                            ilg.Emit(OpCodes.Ldfld, ctorArgField)
                        | None -> 
                        match locals.TryGetValue v with 
                        | true, localBuilder -> 
                            ilg.Emit((if isAddress expectedState  then OpCodes.Ldloca else OpCodes.Ldloc), localBuilder.LocalIndex)
                        | false, _ -> 
                            failwith "unknown parameter/field"

                    | Quotations.Patterns.Coerce (arg,ty) -> 
                        // castClass may lead to observable side-effects - InvalidCastException
                        emit ExpectedStackState.Value arg
                        let argTy = convType arg.Type
                        let targetTy = convType ty
                        if argTy.IsValueType && not targetTy.IsValueType then
                          ilg.Emit(OpCodes.Box, argTy)
                        elif not argTy.IsValueType && targetTy.IsValueType then
                          ilg.Emit(OpCodes.Unbox_Any, targetTy)
                        // emit castclass if 
                        // - targettype is not obj (assume this is always possible for ref types)
                        // AND 
                        // - HACK: targettype is TypeBuilderInstantiationType 
                        //   (its implementation of IsAssignableFrom raises NotSupportedException so it will be safer to always emit castclass)
                        // OR
                        // - not (argTy :> targetTy)
                        elif targetTy <> typeof<obj> && (Misc.TypeBuilderInstantiationType.Equals(targetTy.GetType()) || not (targetTy.IsAssignableFrom(argTy))) then
                          ilg.Emit(OpCodes.Castclass, targetTy)
                              
                        popIfEmptyExpected expectedState
                    | Quotations.DerivedPatterns.SpecificCall <@ (-) @>(None, [t1; t2; t3], [a1; a2]) ->
                        assert(t1 = t2)
                        emit ExpectedStackState.Value a1
                        emit ExpectedStackState.Value a2
                        if t1 = typeof<decimal> then
                            ilg.Emit(OpCodes.Call, typeof<decimal>.GetMethod "op_Subtraction")
                        else
                            ilg.Emit(OpCodes.Sub)
                            emitConvIfNecessary t1

                        popIfEmptyExpected expectedState

                    | Quotations.DerivedPatterns.SpecificCall <@ (/) @> (None, [t1; t2; t3], [a1; a2]) ->
                        assert (t1 = t2)
                        emit ExpectedStackState.Value a1
                        emit ExpectedStackState.Value a2
                        if t1 = typeof<decimal> then
                            ilg.Emit(OpCodes.Call, typeof<decimal>.GetMethod "op_Division")
                        else
                            match Type.GetTypeCode t1 with
                            | TypeCode.UInt32
                            | TypeCode.UInt64
                            | TypeCode.UInt16
                            | TypeCode.Byte
                            | _ when t1 = typeof<unativeint> -> ilg.Emit (OpCodes.Div_Un)
                            | _ -> ilg.Emit(OpCodes.Div)

                            emitConvIfNecessary t1

                        popIfEmptyExpected expectedState

                    | Quotations.DerivedPatterns.SpecificCall <@ int @>(None, [sourceTy], [v]) ->
                        emit ExpectedStackState.Value v
                        match Type.GetTypeCode(sourceTy) with
                        | TypeCode.String -> 
                            ilg.Emit(OpCodes.Call, Misc.ParseInt32Method)
                        | TypeCode.Single
                        | TypeCode.Double
                        | TypeCode.Int64 
                        | TypeCode.UInt64
                        | TypeCode.UInt16
                        | TypeCode.Char
                        | TypeCode.Byte
                        | _ when sourceTy = typeof<nativeint> || sourceTy = typeof<unativeint> ->
                            ilg.Emit(OpCodes.Conv_I4)
                        | TypeCode.Int32
                        | TypeCode.UInt32
                        | TypeCode.Int16
                        | TypeCode.SByte -> () // no op
                        | _ -> failwith "TODO: search for op_Explicit on sourceTy"

                    | Quotations.DerivedPatterns.SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray @> (None, [ty], [arr; index]) ->
                        // observable side-effect - IndexOutOfRangeException
                        emit ExpectedStackState.Value arr
                        emit ExpectedStackState.Value index
                        if isAddress expectedState then
                            ilg.Emit(OpCodes.Readonly)
                            ilg.Emit(OpCodes.Ldelema, convType ty)
                        else
                            ilg.Emit(OpCodes.Ldelem, convType ty)

                        popIfEmptyExpected expectedState

                    | Quotations.DerivedPatterns.SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray2D @> (None, _ty, arr::indices)
                    | Quotations.DerivedPatterns.SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray3D @> (None, _ty, arr::indices)
                    | Quotations.DerivedPatterns.SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray4D @> (None, _ty, arr::indices) ->
                              
                        let meth = 
                          let name = if isAddress expectedState then "Address" else "Get"
                          arr.Type.GetMethod(name)

                        // observable side-effect - IndexOutOfRangeException
                        emit ExpectedStackState.Value arr
                        for index in indices do
                          emit ExpectedStackState.Value index
                              
                        if isAddress expectedState then
                          ilg.Emit(OpCodes.Readonly)

                        ilg.Emit(OpCodes.Call, meth)

                        popIfEmptyExpected expectedState

                    | Quotations.Patterns.FieldGet (objOpt,field) -> 
                        match objOpt with 
                        | None -> () 
                        | Some e -> 
                          let s = if e.Type.IsValueType then ExpectedStackState.Address else ExpectedStackState.Value
                          emit s e
                        let field = match field with :? ProvidedField as pf when fieldMap.ContainsKey pf -> fieldMap.[pf] :> FieldInfo | m -> m
                        if field.IsStatic then 
                            ilg.Emit(OpCodes.Ldsfld, field)
                        else
                            ilg.Emit(OpCodes.Ldfld, field)

                    | Quotations.Patterns.FieldSet (objOpt,field,v) -> 
                        match objOpt with 
                        | None -> () 
                        | Some e -> 
                          let s = if e.Type.IsValueType then ExpectedStackState.Address else ExpectedStackState.Value
                          emit s e
                        emit ExpectedStackState.Value v
                        let field = match field with :? ProvidedField as pf when fieldMap.ContainsKey pf -> fieldMap.[pf] :> FieldInfo | m -> m
                        if field.IsStatic then 
                            ilg.Emit(OpCodes.Stsfld, field)
                        else
                            ilg.Emit(OpCodes.Stfld, field)
                    | Quotations.Patterns.Call (objOpt,meth,args) -> 
                        match objOpt with 
                        | None -> () 
                        | Some e -> 
                          let s = if e.Type.IsValueType then ExpectedStackState.Address else ExpectedStackState.Value
                          emit s e
                        for pe in args do 
                            emit ExpectedStackState.Value pe
                        let getMeth (m:MethodInfo) = match m with :? ProvidedMethod as pm when methMap.ContainsKey pm -> methMap.[pm] :> MethodInfo | m -> m
                        // Handle the case where this is a generic method instantiated at a type being compiled
                        let mappedMeth = 
                            if meth.IsGenericMethod then 
                                let args = meth.GetGenericArguments() |> Array.map convType
                                let gmd = meth.GetGenericMethodDefinition() |> getMeth
                                gmd.GetGenericMethodDefinition().MakeGenericMethod args
                            elif meth.DeclaringType.IsGenericType then 
                                let gdty = convType (meth.DeclaringType.GetGenericTypeDefinition())
                                let gdtym = gdty.GetMethods() |> Seq.find (fun x -> x.Name = meth.Name)
                                assert (gdtym <> null) // ?? will never happen - if method is not found - KeyNotFoundException will be raised
                                let dtym =
                                    match convType meth.DeclaringType with
                                    | :? TypeBuilder as dty -> TypeBuilder.GetMethod(dty, gdtym)
                                    | dty -> MethodBase.GetMethodFromHandle(meth.MethodHandle, dty.TypeHandle) :?> _
                                
                                assert (dtym <> null)
                                dtym
                            else
                                getMeth meth
                        match objOpt with 
                        | Some obj when mappedMeth.IsAbstract || mappedMeth.IsVirtual  ->
                            if obj.Type.IsValueType then ilg.Emit(OpCodes.Constrained, convType obj.Type)
                            ilg.Emit(OpCodes.Callvirt, mappedMeth)
                        | _ ->
                            ilg.Emit(OpCodes.Call, mappedMeth)

                        let returnTypeIsVoid = mappedMeth.ReturnType = typeof<System.Void>
                        match returnTypeIsVoid, (isEmpty expectedState) with
                        | false, true -> 
                              // method produced something, but we don't need it
                              pop()
                        | true, false when expr.Type = typeof<unit> -> 
                              // if we need result and method produce void and result should be unit - push null as unit value on stack
                              ilg.Emit(OpCodes.Ldnull)
                        | _ -> ()

                    | Quotations.Patterns.NewObject (ctor,args) -> 
                        for pe in args do 
                            emit ExpectedStackState.Value pe
                        let meth = match ctor with :? ProvidedConstructor as pc when ctorMap.ContainsKey pc -> ctorMap.[pc] :> ConstructorInfo | c -> c
                        ilg.Emit(OpCodes.Newobj, meth)
                              
                        popIfEmptyExpected expectedState                              

                    | Quotations.Patterns.Value (obj, _ty) -> 
                        let rec emitC (v:obj) = 
                            match v with 
                            | :? string as x -> ilg.Emit(OpCodes.Ldstr, x)
                            | :? int8 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                            | :? uint8 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 (int8 x))
                            | :? int16 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                            | :? uint16 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 (int16 x))
                            | :? int32 as x -> ilg.Emit(OpCodes.Ldc_I4, x)
                            | :? uint32 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                            | :? int64 as x -> ilg.Emit(OpCodes.Ldc_I8, x)
                            | :? uint64 as x -> ilg.Emit(OpCodes.Ldc_I8, int64 x)
                            | :? char as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                            | :? bool as x -> ilg.Emit(OpCodes.Ldc_I4, if x then 1 else 0)
                            | :? float32 as x -> ilg.Emit(OpCodes.Ldc_R4, x)
                            | :? float as x -> ilg.Emit(OpCodes.Ldc_R8, x)
#if BROWSER
#else
                            | :? System.Enum as x when x.GetType().GetEnumUnderlyingType() = typeof<int32> -> ilg.Emit(OpCodes.Ldc_I4, unbox<int32> v)
#endif
                            | :? Type as ty ->
                                ilg.Emit(OpCodes.Ldtoken, convType ty)
                                ilg.Emit(OpCodes.Call, Misc.GetTypeFromHandleMethod)
                            | null -> ilg.Emit(OpCodes.Ldnull)
                            | _ -> failwithf "unknown constant '%A' in generated method" v
                        if isEmpty expectedState then ()
                        else emitC obj

                    | Quotations.Patterns.Let(v,e,b) -> 
                        let lb = ilg.DeclareLocal (convType v.Type)
                        locals.Add (v, lb) 
                        emit ExpectedStackState.Value e
                        ilg.Emit(OpCodes.Stloc, lb.LocalIndex)
                        emit expectedState b
                              
                    | Quotations.Patterns.Sequential(e1, e2) ->
                        emit ExpectedStackState.Empty e1
                        emit expectedState e2                          

                    | Quotations.Patterns.IfThenElse(cond, ifTrue, ifFalse) ->
                        let ifFalseLabel = ilg.DefineLabel()
                        let endLabel = ilg.DefineLabel()

                        emit ExpectedStackState.Value cond

                        ilg.Emit(OpCodes.Brfalse, ifFalseLabel)

                        emit expectedState ifTrue
                        ilg.Emit(OpCodes.Br, endLabel)

                        ilg.MarkLabel(ifFalseLabel)
                        emit expectedState ifFalse

                        ilg.Emit(OpCodes.Nop)
                        ilg.MarkLabel(endLabel)

                    | Quotations.Patterns.TryWith(body, _filterVar, _filterBody, catchVar, catchBody) ->                                                                                      
                              
                        let stres, ldres = 
                            if isEmpty expectedState then ignore, ignore
                            else
                              let local = ilg.DeclareLocal (convType body.Type)
                              let stres = fun () -> ilg.Emit(OpCodes.Stloc, local)
                              let ldres = fun () -> ilg.Emit(OpCodes.Ldloc, local)
                              stres, ldres

                        let exceptionVar = ilg.DeclareLocal(convType catchVar.Type)
                        locals.Add(catchVar, exceptionVar)

                        let _exnBlock = ilg.BeginExceptionBlock()
                              
                        emit expectedState body
                        stres()

                        ilg.BeginCatchBlock(convType  catchVar.Type)
                        ilg.Emit(OpCodes.Stloc, exceptionVar)
                        emit expectedState catchBody
                        stres()
                        ilg.EndExceptionBlock()

                        ldres()

                    | Quotations.Patterns.VarSet(v,e) -> 
                        emit ExpectedStackState.Value e
                        match locals.TryGetValue v with 
                        | true, localBuilder -> 
                            ilg.Emit(OpCodes.Stloc, localBuilder.LocalIndex)
                        | false, _ -> 
                            failwith "unknown parameter/field in assignment. Only assignments to locals are currently supported by TypeProviderEmit"
                    | Quotations.Patterns.Lambda(v, body) ->
                        emitLambda(ilg, v, body, expr.GetFreeVars(), locals, parameterVars)
                        popIfEmptyExpected expectedState
                    | n -> 
                        failwith (sprintf "unknown expression '%A' in generated method" n)
                emit expectedState expr


            // Emit the constructor (if any)
            for pcinfo in ctors do 
                assert ctorMap.ContainsKey pcinfo
                let cb = ctorMap.[pcinfo]
                let cattr = pcinfo.GetCustomAttributesDataImpl() 
                defineCustomAttrs cb.SetCustomAttribute cattr
                let ilg = cb.GetILGenerator()
                ilg.Emit(OpCodes.Ldarg_0)
                let locals = Dictionary<Quotations.Var,LocalBuilder>()
                let parameterVars = 
                    [| yield Quotations.Var("this", pcinfo.DeclaringType)
                       for p in pcinfo.GetParameters() do 
                            yield Quotations.Var(p.Name, p.ParameterType) |]
                let parameters = 
                    [| for v in parameterVars -> Quotations.Expr.Var v |]
                match pcinfo.GetBaseConstructorCallInternal true with
                | None ->  
                    let cinfo = ptd.BaseType.GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance, null, [| |], null)
                    ilg.Emit(OpCodes.Call,cinfo)
                | Some f -> 
                    let (cinfo,argExprs) = f (Array.toList parameters)
                    for argExpr in argExprs do 
                        emitExpr (ilg, locals, parameterVars) ExpectedStackState.Value argExpr
                    ilg.Emit(OpCodes.Call,cinfo)

                if pcinfo.IsImplicitCtor then
                    for ctorArgsAsFieldIdx,ctorArgsAsField in List.mapi (fun i x -> (i,x)) implicitCtorArgsAsFields do 
                        ilg.Emit(OpCodes.Ldarg_0)
                        ilg.Emit(OpCodes.Ldarg, ctorArgsAsFieldIdx+1)
                        ilg.Emit(OpCodes.Stfld, ctorArgsAsField)
                else
                    let code  = pcinfo.GetInvokeCodeInternal true
                    let code = code parameters
                    emitExpr (ilg, locals, parameterVars) ExpectedStackState.Empty code
                ilg.Emit(OpCodes.Ret)
            
            match ptd.GetConstructors(ALL) |> Seq.tryPick (function :? ProvidedConstructor as pc when pc.IsTypeInitializer -> Some pc | _ -> None) with
            | None -> ()
            | Some pc ->
                let cb = ctorMap.[pc]
                let ilg = cb.GetILGenerator()
                let cattr = pc.GetCustomAttributesDataImpl() 
                defineCustomAttrs cb.SetCustomAttribute cattr
                let expr = pc.GetInvokeCodeInternal true [||]
                emitExpr(ilg, new Dictionary<_, _>(), [||]) ExpectedStackState.Empty expr
                ilg.Emit OpCodes.Ret

            // Emit the methods
            for minfo in ptd.GetMethods(ALL) do
              match minfo with 
              | :? ProvidedMethod as pminfo   -> 
                let mb = methMap.[pminfo]
                let ilg = mb.GetILGenerator()
                let cattr = pminfo.GetCustomAttributesDataImpl() 
                defineCustomAttrs mb.SetCustomAttribute cattr

                let parameterVars = 
                    [| if not pminfo.IsStatic then 
                            yield Quotations.Var("this", pminfo.DeclaringType)
                       for p in pminfo.GetParameters() do 
                            yield Quotations.Var(p.Name, p.ParameterType) |]
                let parameters = 
                    [| for v in parameterVars -> Quotations.Expr.Var v |]

                let expr = pminfo.GetInvokeCodeInternal true parameters 

                let locals = Dictionary<Quotations.Var,LocalBuilder>()
                //printfn "Emitting linqCode for %s::%s, code = %s" pminfo.DeclaringType.FullName pminfo.Name (try linqCode.ToString() with _ -> "<error>")


                let expectedState = if (minfo.ReturnType = typeof<System.Void>) then ExpectedStackState.Empty else ExpectedStackState.Value
                emitExpr (ilg, locals, parameterVars) expectedState expr
                ilg.Emit OpCodes.Ret
              | _ -> ()
  
            for (bodyMethInfo,declMethInfo) in ptd.GetMethodOverrides() do 
                let bodyMethBuilder = methMap.[bodyMethInfo]
                tb.DefineMethodOverride(bodyMethBuilder,declMethInfo)

            for evt in ptd.GetEvents(ALL) |> Seq.choose (function :? ProvidedEvent as pe -> Some pe | _ -> None) do
                let eb = tb.DefineEvent(evt.Name, evt.Attributes, evt.EventHandlerType)
                defineCustomAttrs eb.SetCustomAttribute (evt.GetCustomAttributesDataImpl())
                eb.SetAddOnMethod(methMap.[evt.GetAddMethod(true) :?> _])
                eb.SetRemoveOnMethod(methMap.[evt.GetRemoveMethod(true) :?> _])
                // TODO: add raiser
            
            for pinfo in ptd.GetProperties(ALL) |> Seq.choose (function :? ProvidedProperty as pe -> Some pe | _ -> None) do
                let pb = tb.DefineProperty(pinfo.Name, pinfo.Attributes, convType pinfo.PropertyType, [| for p in pinfo.GetIndexParameters() -> convType p.ParameterType |])
                let cattr = pinfo.GetCustomAttributesDataImpl() 
                defineCustomAttrs pb.SetCustomAttribute cattr
                if  pinfo.CanRead then 
                    let minfo = pinfo.GetGetMethod(true)
                    pb.SetGetMethod (methMap.[minfo :?> ProvidedMethod ])
                if  pinfo.CanWrite then 
                    let minfo = pinfo.GetSetMethod(true)
                    pb.SetSetMethod (methMap.[minfo :?> ProvidedMethod ]))


        // phase 4 - complete types
        iterateTypes (fun tb _ptd -> tb.CreateType() |> ignore)

#if FX_NO_LOCAL_FILESYSTEM
#else
        assembly.Save (Path.GetFileName assemblyFileName)
#endif

        let assemblyLoadedInMemory = assemblyMainModule.Assembly 

        iterateTypes (fun _tb ptd -> 
            match ptd with 
            | None -> ()
            | Some ptd -> ptd.SetAssembly assemblyLoadedInMemory)

#if FX_NO_LOCAL_FILESYSTEM
#else
    member __.GetFinalBytes() = 
        let assemblyBytes = File.ReadAllBytes assemblyFileName
        let _assemblyLoadedInMemory = System.Reflection.Assembly.Load(assemblyBytes,null,System.Security.SecurityContextSource.CurrentAppDomain)
        //printfn "final bytes in '%s'" assemblyFileName
        //File.Delete assemblyFileName
        assemblyBytes
#endif

type ProvidedAssembly(assemblyFileName: string) = 
    let theTypes = ResizeArray<_>()
    let assemblyGenerator = AssemblyGenerator(assemblyFileName)
    let assemblyLazy = 
        lazy 
            assemblyGenerator.Generate(theTypes |> Seq.toList)
            assemblyGenerator.Assembly
#if FX_NO_LOCAL_FILESYSTEM
#else
    let theAssemblyBytesLazy = 
      lazy
        assemblyGenerator.GetFinalBytes()

    do
        GlobalProvidedAssemblyElementsTable.theTable.Add(assemblyGenerator.Assembly, theAssemblyBytesLazy) 

#endif

    let add (providedTypeDefinitions:ProvidedTypeDefinition list, enclosingTypeNames: string list option) = 
        for pt in providedTypeDefinitions do 
            if pt.IsErased then invalidOp ("The provided type "+pt.Name+"is marked as erased and cannot be converted to a generated type. Set 'IsErased' to false on the ProvidedTypeDefinition")
            theTypes.Add(pt,enclosingTypeNames)
            pt.SetAssemblyLazy assemblyLazy

    member x.AddNestedTypes (providedTypeDefinitions, enclosingTypeNames) = add (providedTypeDefinitions, Some enclosingTypeNames)
    member x.AddTypes (providedTypeDefinitions) = add (providedTypeDefinitions, None)
#if FX_NO_LOCAL_FILESYSTEM
#else
    static member RegisterGenerated (fileName:string) = 
        //printfn "registered assembly in '%s'" fileName
        let assemblyBytes = System.IO.File.ReadAllBytes fileName
        let assembly = Assembly.Load(assemblyBytes,null,System.Security.SecurityContextSource.CurrentAppDomain)
        GlobalProvidedAssemblyElementsTable.theTable.Add(assembly, Lazy.CreateFromValue assemblyBytes)
        assembly
#endif


module Local = 

    let makeProvidedNamespace (namespaceName:string) (types:ProvidedTypeDefinition list) =
        let types = [| for ty in types -> ty :> Type |]
        {new IProvidedNamespace with
            member __.GetNestedNamespaces() = [| |]
            member __.NamespaceName = namespaceName
            member __.GetTypes() = types |> Array.copy
            member __.ResolveTypeName typeName : System.Type = 
                match types |> Array.tryFind (fun ty -> ty.Name = typeName) with
                | Some ty -> ty
                | None    -> null
                // let typenames = String.concat "," (types |> Array.map (fun t -> t.Name))
                //             failwith (sprintf "Unknown type '%s' in namespace '%s' (contains %s)" typeName namespaceName typenames)    
        }


type TypeProviderForNamespaces(namespacesAndTypes : list<(string * list<ProvidedTypeDefinition>)>) as this =
    let otherNamespaces = ResizeArray<string * list<ProvidedTypeDefinition>>()

    let providedNamespaces = 
        lazy [| for (namespaceName,types) in namespacesAndTypes do 
                     yield Local.makeProvidedNamespace namespaceName types 
                for (namespaceName,types) in otherNamespaces do 
                     yield Local.makeProvidedNamespace namespaceName types |]

    let invalidateE = new Event<EventHandler,EventArgs>()    

#if FX_NO_LOCAL_FILESYSTEM
#else
    let probingFolders = ResizeArray()
    let handler = ResolveEventHandler(fun _ args -> this.ResolveAssembly(args))
    do AppDomain.CurrentDomain.add_AssemblyResolve handler
#endif

    new (namespaceName:string,types:list<ProvidedTypeDefinition>) = new TypeProviderForNamespaces([(namespaceName,types)])
    new () = new TypeProviderForNamespaces([])

#if FX_NO_LOCAL_FILESYSTEM
    interface System.IDisposable with 
        member x.Dispose() = ()
#else
    abstract member ResolveAssembly : args : System.ResolveEventArgs -> Assembly
    default this.ResolveAssembly(args) = 
        let expectedName = (AssemblyName(args.Name)).Name + ".dll"
        let expectedLocationOpt = 
            probingFolders 
            |> Seq.map (fun f -> IO.Path.Combine(f, expectedName))
            |> Seq.tryFind IO.File.Exists
        match expectedLocationOpt with
        | Some f -> Assembly.LoadFrom f
        | None -> null

    member this.RegisterProbingFolder (folder) = 
        // use GetFullPath to ensure that folder is valid
        ignore(IO.Path.GetFullPath folder)
        probingFolders.Add folder
    member this.RegisterRuntimeAssemblyLocationAsProbingFolder (cfg : Core.CompilerServices.TypeProviderConfig) =  
        cfg.RuntimeAssembly
        |> IO.Path.GetDirectoryName
        |> this.RegisterProbingFolder
    interface System.IDisposable with 
        member x.Dispose() = AppDomain.CurrentDomain.remove_AssemblyResolve handler
#endif

    member __.AddNamespace (namespaceName,types:list<_>) = otherNamespaces.Add (namespaceName,types)
    member self.Invalidate() = invalidateE.Trigger(self,EventArgs())
    interface ITypeProvider with
        [<CLIEvent>]
        override this.Invalidate = invalidateE.Publish
        override this.GetNamespaces() = Array.copy providedNamespaces.Value
        member __.GetInvokerExpression(methodBase, parameters) =
            let rec getInvokerExpression (methodBase : MethodBase) parameters =
                match methodBase with
                | :? ProvidedMethod as m when (match methodBase.DeclaringType with :? ProvidedTypeDefinition as pt -> pt.IsErased | _ -> true) ->
                    m.GetInvokeCodeInternal false parameters
                    |> expand
                | :? ProvidedConstructor as m when (match methodBase.DeclaringType with :? ProvidedTypeDefinition as pt -> pt.IsErased | _ -> true) -> 
                    m.GetInvokeCodeInternal false parameters
                    |> expand
                // Otherwise, assume this is a generative assembly and just emit a call to the constructor or method
                | :?  ConstructorInfo as cinfo ->  
                    Quotations.Expr.NewObject(cinfo, Array.toList parameters) 
                | :? System.Reflection.MethodInfo as minfo ->  
                    if minfo.IsStatic then 
                        Quotations.Expr.Call(minfo, Array.toList parameters) 
                    else
                        Quotations.Expr.Call(parameters.[0], minfo, Array.toList parameters.[1..])
                | _ -> failwith ("TypeProviderForNamespaces.GetInvokerExpression: not a ProvidedMethod/ProvidedConstructor/ConstructorInfo/MethodInfo, name=" + methodBase.Name + " class=" + methodBase.GetType().FullName)
            and expand expr = 
                match expr with
                | Quotations.Patterns.NewObject(ctor, args) -> getInvokerExpression ctor [| for arg in args -> expand arg|]
                | Quotations.Patterns.Call(inst, mi, args) ->
                    let args = 
                        [|
                            match inst with
                            | Some inst -> yield expand inst
                            | _ -> ()
                            yield! List.map expand args
                        |]
                    getInvokerExpression mi args
                | Quotations.ExprShape.ShapeVar v -> Quotations.Expr.Var v
                | Quotations.ExprShape.ShapeLambda(v, body) -> Quotations.Expr.Lambda(v, expand body)
                | Quotations.ExprShape.ShapeCombination(shape, args) -> Quotations.ExprShape.RebuildShapeCombination(shape, List.map expand args)
            getInvokerExpression methodBase parameters
#if FX_NO_CUSTOMATTRIBUTEDATA

        member __.GetMemberCustomAttributesData(methodBase) = 
            match methodBase with
            | :? ProvidedTypeDefinition as m  -> m.GetCustomAttributesDataImpl()
            | :? ProvidedMethod as m  -> m.GetCustomAttributesDataImpl()
            | :? ProvidedProperty as m  -> m.GetCustomAttributesDataImpl()
            | :? ProvidedConstructor as m -> m.GetCustomAttributesDataImpl()
            | :? ProvidedEvent as m -> m.GetCustomAttributesDataImpl()
            | :?  ProvidedLiteralField as m -> m.GetCustomAttributesDataImpl()
            | :?  ProvidedField as m -> m.GetCustomAttributesDataImpl()
            | _ -> [| |] :> IList<_>

        member __.GetParameterCustomAttributesData(methodBase) = 
            match methodBase with
            | :? ProvidedParameter as m  -> m.GetCustomAttributesDataImpl()
            | _ -> [| |] :> IList<_>


#endif
        override this.GetStaticParameters(ty) =
            match ty with
            | :? ProvidedTypeDefinition as t ->
                if ty.Name = t.Name (* REVIEW: use equality? *) then
                    t.GetStaticParameters()
                else
                    [| |]
            | _ -> [| |]

        override this.ApplyStaticArguments(ty,typePathAfterArguments:string[],objs) = 
            let typePathAfterArguments = typePathAfterArguments.[typePathAfterArguments.Length-1]
            match ty with
            | :? ProvidedTypeDefinition as t -> (t.MakeParametricType(typePathAfterArguments,objs) :> Type)
            | _ -> failwith (sprintf "ApplyStaticArguments: static params for type %s are unexpected" ty.FullName)

#if FX_NO_LOCAL_FILESYSTEM
        override x.GetGeneratedAssemblyContents(_assembly) = 
            // TODO: this is very fake, we rely on the fact it is never needed
            match System.Windows.Application.GetResourceStream(System.Uri("FSharp.Core.dll",System.UriKind.Relative)) with 
            | null -> failwith "FSharp.Core.dll not found as Manifest Resource, we're just trying to read some random .NET assembly, ok?"
            | resStream ->  
                use stream = resStream.Stream
                let len = stream.Length
                let buf = Array.zeroCreate<byte> (int len)
                let rec loop where rem = 
                    let n = stream.Read(buf, 0, int rem)
                    if n < rem then loop (where  + n) (rem - n)
                loop 0 (int len) 
                buf

            //failwith "no file system"
#else
        override x.GetGeneratedAssemblyContents(assembly:Assembly) = 
            //printfn "looking up assembly '%s'" assembly.FullName
            match GlobalProvidedAssemblyElementsTable.theTable.TryGetValue assembly with 
            | true,bytes -> bytes.Force()
            | _ -> 
                let bytes = System.IO.File.ReadAllBytes assembly.ManifestModule.FullyQualifiedName
                GlobalProvidedAssemblyElementsTable.theTable.[assembly] <- Lazy.CreateFromValue bytes
                bytes
#endif