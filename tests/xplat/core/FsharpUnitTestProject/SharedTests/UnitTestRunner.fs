namespace LanguageUnitTests
open System
open NUnit.Framework

[<TestFixture; Description("#Conformance #TypeInference #TypeConstraints #UnitsOfMeasure #Regression #Operators #Mutable")>]
type ``Subtype tests``() =
    [<Test>]
    member x.``Subtype Tests``() =
        let results = Core_subtype.RUN()
        Assert.IsTrue(results.IsEmpty)

[<TestFixture; Description("#Regression #Conformance #Operators #SyntacticSugar #Exceptions #ControlFlow #Arrays #Tuples #Lists #Classes #Constants #Records")>]
type ``Syntax tests``() =
    [<Test>]
    member x.``Syntax Tests``() =
        let failures = Core_syntax.RUN()
        Assert.IsFalse(failures)

[<TestFixture; Description("#Conformance #Regression #Recursion #LetBindings")>]
type ``Tlr tests``() =
    [<Test>]
    member x.``Tlr Tests``() =
        let failures = Core_tlr.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #Globalization")>]
type ``Unicode tests``() =
    [<Test>]
    member x.``Unicode Tests``() =
        let failures = Core_unicode.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #Quotations #Interop #Classes #ObjectConstructors #Attributes #Reflection")>]
type ``Quotes tests``() =
    [<Test>]
    member x.``Quotes Tests``() =
        let failures = Core_unicode.RUN()
        Assert.IsTrue(failures.IsEmpty)

#if Portable
#else
[<TestFixture; Description(""); Ignore("Investigating failure")>]
type ``Quotation debug tests``() =
    [<Test>]
    member x.``Quotation Debug Tests``() =
        let failures = QuotesDebugInfo.RUN()
        Assert.AreEqual(failures, 0)
#endif

[<TestFixture; Description("#Regression #Conformance #Sequences")>]
type ``Sequence tests``() =

    [<Test>]
    member x.``Sequence Tests``() =
        let failures = Core_seq.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance")>]
type ``Nested tests``() =
    [<Test>]
    member x.``Nested Tests``() =
        let failures = Core_nested.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #PatternMatching #Regression #Lists #ActivePatterns")>]
type ``Patterns tests``() =
    [<Test>]
    member x.``Pattern Tests``() =
        let failures = Core_patterns.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #Regression")>]
type ``Lift tests``() =
    [<Test>]
    member x.``List Tests``() =
        let failures = Core_lift.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #MemberDefinitions")>]
type ``Members-ctree tests``() =
    [<Test>]
    member x.``Members-ctree Tests``() =
        let failures = Core_members_ctree.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("")>]
type ``Members-factors tests``() =
    [<Test>]
    member x.``Members-factors Tests``() =
        let failures = Core_members_factors.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #MemberDefinitions #Overloading #ComputationExpressions")>]
type ``Members-ops tests``() =
    [<Test>]
    member x.``Members-ops Tests``() =
        let failures = Core_members_ops.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #MemberDefinitions #Mutable #ObjectOrientedTypes #Classes #InterfacesAndImplementations #Recursion")>]
type ``Members-incremental-test-hw tests``() =
    [<Test>]
    member x.``Members-incremental-test-hw Tests``() =
        let failures = Core_members_incremental_testhw.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #MemberDefinitions #Mutable #ObjectOrientedTypes #Classes #InterfacesAndImplementations #Recursion")>]
type ``Members-incremental-test tests``() =
    [<Test>]
    member x.``Members-incremental-test Tests``() =
        let failures = Core_members_incremental.RUN()
        Assert.IsTrue(failures.IsEmpty)
  
[<TestFixture; Description("#Conformance #ObjectConstructors ")>]
type ``Longnames tests``() =
    [<Test>]
    member x.``Longnames Tests``() =
        let failures = Core_longnames.RUN()
        Assert.IsTrue(failures.IsEmpty)
                
[<TestFixture; Description("#Conformance #Regression")>]
type ``lazy tests``() =
    [<Test>]
    member x.``lazy Tests``() =
        let failures = Core_lazy.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #Regression #Collections")>]
type ``map tests``() =
    [<Test>]
    member x.``map Tests``() =
        let failures = Core_map.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #LetBindings #Recursion #TypeInference #ObjectConstructors #Classes #Records")>]
type ``letrec tests``() =
    [<Test>]
    member x.``letrec Tests``() =
        let failures = Core_letrec.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #Regression #LetBindings #TypeInference")>]
type ``innerpoly tests``() =
    [<Test>]
    member x.``innerpoly Tests``() =
        let failures = Core_innerpoly.RUN()
        Assert.IsTrue(failures.IsEmpty)
        
[<TestFixture; Description("#Conformance #Sequences #Regression #ControlFlow #SyntacticSugar #ComputationExpressions")>]
type ``comprehensions tests``() =
    [<Test>]
    member x.``comprehensions Tests``() =
        let failures = Core_comprehensions.RUN()
        Assert.IsTrue(failures.IsEmpty)  

[<TestFixture; Description("#Conformance #Arrays #Stress #Structs #Mutable #ControlFlow #LetBindings")>]
type ``array tests``() =
    [<Test>]
    member x.``array Tests``() =
        let failures = Core_array.RUN()
        Assert.IsTrue(failures.IsEmpty)  

[<TestFixture; Description("#Conformance #Constants #Recursion #LetBindings #MemberDefinitions #Mutable")>]
type ``apporder tests``() =
    [<Test>]
    member x.``apporder Tests``() =
        let failures = Core_apporder.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Regression #Conformance #Accessibility #SignatureFiles #Regression #Records #Unions")>]
type ``access tests``() =
    [<Test>]
    member x.``access Tests``() =
        let failures = Core_access.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Regression #Conformance #Regression")>]
type ``math numbers tests``() =
    [<Test>]
    member x.``math numbers Tests``() =
        let failures = Core_math_numbers.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Regression #Conformance #Regression")>]
type ``math numbers VS2008 tests``() =
    [<Test>]
    member x.``math numbers Tests``() =
        let failures = Core_math_numbersVS2008.RUN()
        Assert.IsTrue(failures.IsEmpty)  

[<TestFixture; Description("#Conformance #UnitsOfMeasure #Constants ")>]
type ``math measures tests``() =
    [<Test>]
    member x.``math measures Tests``() =
        let failures = Core_measures.RUN()
        Assert.IsTrue(failures.IsEmpty)  

[<TestFixture; Description("#Conformance #UnitsOfMeasure #Constants ")>]
type ``math generic measures tests``() =
    [<Test>]
    member x.``math generic measures Tests``() =
        let failures = Core_genericMeasures.RUN()
        Assert.IsTrue(failures.IsEmpty)
#if __ANDROID__
#else
[<TestFixture; Description("#Regression #Conformance #ComputationExpressions #Async #Regression #Events #Stress")>]
type ``control tests``() =
    [<Test;Ignore "Long running">]
    member x.``control Tests``() =
        let failures = Core_control.RUN()
        Assert.IsTrue(failures.IsEmpty)

[<TestFixture; Description("#Conformance #ComputationExpressions #Async")>]
type ``control chamenos tests``() =
    [<Test>]
    member x.``control chamenos Tests``() =
        let failures = Core_controlChamenos.RUN()
        Assert.IsTrue(failures.IsEmpty)  

[<TestFixture(Description = "#Regression #Conformance #ComputationExpressions #Async #Regression #Events #Stress")>]
type ``control mailbox tests``() =
    [<Test>]
    member x.``control mailbox Tests``() =
        let failures = Core_controlMailBox.RUN()
        Assert.IsTrue(failures.IsEmpty)  

[<TestFixture; Description("#Conformance #ComputationExpressions #Async")>]
type ``control stack overflow tests``() =
    [<Test;Ignore "Long running">]
    member x.``control stack overflow Tests``() =
        let failures = Core_controlStackOverflow.RUN()
        Assert.IsTrue(failures.IsEmpty)  
#endif
//[<TestFixture; Description("#Quotations #Query")>]
//type ``queries over ienumerable tests``() =
//    [<Test>]
//    member x.``queries over ienumerable Tests``() =
//        let failures = Core_queriesOverIEnumerable.RUN()
//        Assert.IsTrue(failures.IsEmpty)  
//
//[<TestFixture; Description("#Quotations #Query")>]
//type ``queries over iqueriable tests``() =
//    [<Test>]
//    member x.``notset Tests``() =
//        let failures = Core_queriesOverIEnumerable.RUN()
//        Assert.IsTrue(failures.IsEmpty)  





       
