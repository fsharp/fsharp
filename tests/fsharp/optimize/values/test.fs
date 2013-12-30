module Test

let testMultiplyOne =
    let x = 500 / 3
    let y = x * 1
    y

let testMultiplyZero =
    let x = 500 / 3
    let y = x * 0
    y

let testDivideOne =
    let x = 500 / 3
    let y = x / 1
    y

let testDivideZero =
    let x = 500 / 3
    let y = 0 / x
    y