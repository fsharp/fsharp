type CB = Fields.ClassBased
type SB = Fields.StructBased
try

    let check id v1 v2 = if v2 <> v2 then failwithf "check at %s failed" id

    check "ClassBased::Static::Initial" (CB.Get()) 555
    CB.Set(100)
    check "ClassBased::Static::Modified" (CB.Get()) 100

    check "StructBased::Static::Initial" (SB.Get()) 888
    SB.Set(500)
    check "StructBased::Static::Modified" (SB.Get()) 500

    let cb = new CB()
    check "ClassBased::Instance::Initial" (cb.InstanceGet()) "initial instance"
    cb.InstanceSet("modified instance")
    check "ClassBased::Instance::modified" (cb.InstanceGet()) "modified instance"
    0
with e -> printfn "%s" e.Message; 1
