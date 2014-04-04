module Test

// Tests for value one optimizations

let testByteOne =
    let x = 75uy / 3uy
    let y = 1uy * (x * 1uy)
    y

let testSByteOne =
    let x = 75y / 3y
    let y = 1y * (x * 1y)
    y

let testUInt16One =
    let x = 75us / 3us
    let y = 1us * (x * 1us)
    y

let testInt16One =
    let x = 75s / 3s
    let y = 1s * (x * 1s)
    y

let testUInt32One =
    let x = 75u / 3u
    let y = 1u * (x * 1u)
    y

let testInt32One =
    let x = 75 / 3
    let y = 1 * (x * 1)
    y

let testUInt64One =
    let x = 75UL / 3UL
    let y = 1UL * (x * 1UL)
    y

let testInt64One =
    let x = 75L / 3L
    let y = 1L * (x * 1L)
    y

let testSingleOne =
    let x = 75.f / 3.f
    let y = 1.f * (x * 1.f)
    y

let testDoubleOne =
    let x = 75. / 3.
    let y = 1. * (x * 1.)
    y