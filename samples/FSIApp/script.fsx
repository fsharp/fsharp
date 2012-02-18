﻿(*namespace CodeDomTest

    module Math =
        let sqr (x:float) = x * x
        let cube (x:float) = x * x * x*)

let sqr x = x * x
printfn "Sqr of 10 - %d" (sqr 10)

let cube (x:float) = x * x * x
printfn "Cube - %A" ([1. .. 10.] |> List.map cube)