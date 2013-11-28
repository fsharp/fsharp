module Test

type T() = 
    inherit CSLib.Outer()

    let a = CSLib.Outer.ProtectedInner1()

    do a.ProtectedMethod()
    let _ = a.ProtectedProperty

    let b = CSLib.Outer.ProtectedInner1.ProtectedInnerInner1()
    
    let c = CSLib.Outer.ProtectedInner2()
    do c.ProtectedMethod()
    let _ = c.ProtectedProperty

    let d = CSLib.Outer.ProtectedInner3()
    let e = CSLib.Outer.ProtectedInner3.ProtectedInner()
    
    let f = CSLib.Outer.PublicInner1()
    do f.PublicMethod()
    let _ = f.PublicProperty
    
    let g = CSLib.Outer.PublicInner1.ProtectedInnerInner1()

let a = CSLib.Outer.ProtectedInner1()
let b = CSLib.Outer.ProtectedInner1.ProtectedInnerInner1()
let d = CSLib.Outer.ProtectedInner3()
let e = CSLib.Outer.ProtectedInner3.ProtectedInner()
let g = CSLib.Outer.PublicInner1.ProtectedInnerInner1()