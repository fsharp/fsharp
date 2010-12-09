// Various tests for Microsoft.FSharp.Quotations

namespace SystematicUnitTests.FSharp_Core.Microsoft_FSharp_Quotations

open System
open FSharp.Core.Unittests.LibraryTestFx
open NUnit.Framework
open Microsoft.FSharp.Quotations

type E = Microsoft.FSharp.Quotations.Expr;;

[<TestFixture>]
type FSharpQuotationsNRETests() =
    
    [<Test>]
    member x.MethodInfoNRE() =
        let f() = 
            E.Call(null, []) |> ignore
        CheckThrowsArgumentNullException f

    [<Test>]
    member x.FieldInfoNRE() =
        let f() =
            E.FieldGet(null) |> ignore
        CheckThrowsArgumentNullException f
    
    [<Test>]
    member x.ConstructorNRE() =
        let f() =
            E.NewObject(null,[]) |> ignore
        CheckThrowsArgumentNullException f

    [<Test>]
    member x.PropertyInfoNRE() =
        let f() =
            E.PropertyGet(null,[]) |> ignore
        CheckThrowsArgumentNullException f
        
    [<Test>]
    member x.UnionCaseInfoNRE() =
        let f() =
            E.NewUnionCase(Unchecked.defaultof<Microsoft.FSharp.Reflection.UnionCaseInfo>,[]) |> ignore
        CheckThrowsArgumentNullException f
        