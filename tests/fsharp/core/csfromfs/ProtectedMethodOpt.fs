open TestBaseClass
type DerivedClass() = class
    inherit BaseClass()
        
    member x.SomeMethod() = BaseClass.ProtectedStatic()
    static member AnotherMethod() = BaseClass.ProtectedStatic()
end
 
let r1 = DerivedClass().SomeMethod()
let r2 = DerivedClass.AnotherMethod()
 
