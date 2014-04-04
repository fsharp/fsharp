namespace TypeProviders

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes

[<assembly : TypeProviderAssembly>]
do()

module Shared = 
    type private T = interface end
    let ThisAssembly =typeof<T>.Assembly

module ErasedFields =
    let Namespace = "Fields"

    let private defineType (name: string, baseType, ctorCode, staticGetInvokeCode, staticSetInvokeCode, instanceGetInvokeCode, instanceSetInvokeCode) = 
        let ty = ProvidedTypeDefinition(Shared.ThisAssembly, Namespace, name, Some baseType, IsErased = true)
        do
            let ctor = ProvidedConstructor([], InvokeCode = ctorCode)
            ty.AddMember ctor
        do
            let m = ProvidedMethod("Get", [], typeof<int>, IsStaticMethod = true, InvokeCode = staticGetInvokeCode)
            ty.AddMember m
        do
            let m = ProvidedMethod("Set", [ProvidedParameter("v", typeof<int>)], typeof<unit>, IsStaticMethod = true, InvokeCode = staticSetInvokeCode)
            ty.AddMember m
        do
            let m = ProvidedMethod("InstanceGet", [], typeof<string>, IsStaticMethod = false, InvokeCode = instanceGetInvokeCode)
            ty.AddMember m
        do
            let m = ProvidedMethod("InstanceSet", [ProvidedParameter("v", typeof<string>)], typeof<unit>, IsStaticMethod = false, InvokeCode = instanceSetInvokeCode)
            ty.AddMember m
        ty

    [<TypeProvider>]
    type TypeProvider() as this =
        inherit TypeProviderForNamespaces()

        let structBasedTy = 
            defineType
                (
                    "StructBased",
                    typeof<BaseTypes.BaseStruct>,
                    (fun _ -> <@@ BaseTypes.BaseStruct() @@>),
                    (fun _ -> <@@ BaseTypes.BaseStruct.StaticField @@>),
                    (fun [v] -> <@@ BaseTypes.BaseStruct.StaticField <- (%%v : int) @@>),
                    (fun [inst] -> <@@ (%%inst: BaseTypes.BaseStruct).InstanceField @@>),
                    (fun [inst; v] -> Expr.FieldSet(inst, typeof<BaseTypes.BaseStruct>.GetField("InstanceField"), v))
                )
        let classBasedTy = 
            defineType
                (
                    "ClassBased",
                    typeof<BaseTypes.BaseClass>,
                    (fun _ -> <@@ BaseTypes.BaseClass() @@>),
                    (fun _ -> <@@ BaseTypes.BaseClass.StaticField @@>),
                    (fun [v] -> <@@ BaseTypes.BaseClass.StaticField <- (%%v : int) @@>),
                    (fun [inst] -> <@@ (%%inst: BaseTypes.BaseClass).InstanceField @@>),
                    (fun [inst; v] -> Expr.FieldSet(inst, typeof<BaseTypes.BaseClass>.GetField("InstanceField"), v))
                )

        do this.AddNamespace(Namespace, [structBasedTy;classBasedTy])
