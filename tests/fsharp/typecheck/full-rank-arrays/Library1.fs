namespace HighRankArrayTests.FSharp

open HighRankArrayTests


type Class1() = 
    member this.X =
        GenericStaticClassWithMethod<int>.Example [| 2;3;4;5 |]

    member this.Y =
        StaticClassWithGenericMethod.Example [| 1;2;3;4 |]

    member this.Z =
        let foo = ClassWithArrayCtor ([| 1;3;5;7 |])
        ()
