

let mutable allErrors = []

let check (dir:string) f = 
    System.Console.Write("Running {0}....", dir)
    let errors = try f() with e -> [e.ToString()]
    allErrors <- allErrors @ [ for e in errors -> (dir,e) ]
    match errors with 
    | [] -> System.Console.WriteLine("OK") 
    | _ -> 
        System.Console.WriteLine "" 
        System.Console.WriteLine "" 
        System.Console.WriteLine "-------------------" 
        System.Console.WriteLine("Failures in {0}",dir) 
        for error in errors do 
            System.Console.WriteLine("    FAILED: {0}",error)
        System.Console.WriteLine "-------------------" 
        System.Console.WriteLine "" 



#load "queriesCustomQueryOps/test.fsx"
#load "queriesLeafExpressionConvert/test.fsx"
#load "queriesNullableOperators/test.fsx"

check "queriesCustomQueryOps/test.fsx" Core_queriesCustomQueryOps.RUN
check "queriesLeafExpressionConvert/test.fsx" Core_queriesLeafExpressionConvert.RUN
check "queriesNullableOperators/test.fsx" Core_queriesNullableOperators.RUN

#load "subtype/test.fsx"
#load "syntax/test.fsx"
#load "tlr/test.fsx"
#load "unicode/test.fsx"

check "subtype" Core_subtype.RUN
check "syntax" (fun () -> if Core_syntax.RUN() then ["FAILED"] else [])
check "tlr" Core_tlr.RUN
check "unicode" Core_unicode.RUN

#load "quotes/test.fsx"
#load "quotesDebugInfo/test.fsx"
#load "seq/test.fsx"
#load "nested/test.fsx"
#load "patterns/test.fsx"

check "quotes/test.fsx" Core_quotes.RUN
check "quotesDebugInfo/test.fsx" (fun () -> match Test.RUN() with 0 -> [] | n -> [sprintf "%d failures" n])
check "seq/test.fsx" Core_seq.RUN
check "nested/test.fsx" Core_nested.RUN
check "patterns/test.fsx" Core_patterns.RUN

// PASSED, but slow
// #load "printf/test.fsx"
// check "printf/test.fsx" Core_printf.RUN


#load "lift/test.fsx"
check "lift" Core_lift.RUN

#load "members/ctree/test.fsx"
#load "members/factors/test.fsx"
#load "members/ops/test.fsx"
#load "members/incremental-hw/test.fsx"
#load "members/incremental/test.fsx"

check "members/ctree/test.fsx" Core_members_ctree.RUN
check "members/factors/test.fsx" Core_members_factors.RUN
check "members/ops/test.fsx" Core_members_ops.RUN
check "members/incremental-hw/test-hw.fsx" Core_members_incremental_testhw.RUN
check "members/incremental/test.fsx" Core_members_incremental.RUN


#load "longnames/test.fsx"
#load "map/test.fsx"
#load "lazy/test.fsx"
#load "letrec/test.fsx"


check "longnames" Core_longnames.RUN
check "lazy" Core_lazy.RUN
check "map" Core_map.RUN
check "letrec" Core_letrec.RUN

#load "innerpoly/test.fsx"
check "innerpoly" Core_innerpoly.RUN

#load "comprehensions/test.fsx"
#load "array/test.fsx"
#load "apporder/test.fsx"
#load "access/test.fsx"


check "access" Core_access.RUN
check "apporder" Core_apporder.RUN
check "array" Core_array.RUN
check "comprehensions" Core_comprehensions.RUN


#load "math/numbers/test.fsx"
#load "math/numbersVS2008/test.fsx"
#load "measures/test.fsx"
#load "genericmeasures/test.fsx"

check "math/numbers/test.fsx" Core_math_numbers.RUN
check "math/numbersVS2008/test.fsx" Core_math_numbersVS2008.RUN
check "math/measures/test.fsx" Core_measures.RUN
check "math/measures/test.fsx" Core_genericMeasures.RUN


#load "control/test.fsx"
check "control/test.fsx" Core_control.RUN

#load "controlChamenos/test.fsx"
check "controlChamenos/test.fsx" Core_controlChamenos.RUN

#load "controlMailbox/test.fsx"
check "controlMailbox/test.fsx" Core_controlMailBox.RUN

// PASSED, but slow
// #load "controlStackOverflow/test.fsx"
// check "controlStackOverflow/test.fsx" Core_controlStackOverflow.RUN


#load "queriesOverIEnumerable/test.fsx"
#load "queriesOverIQueryable/test.fsx"
 
check "queriesOverIEnumerable/test.fsx" Core_queriesOverIEnumerable.RUN
check "queriesOverIQueryable/test.fsx" Core_queriesOverIQueryable.RUN

// FAILED (repro1, repro2)
//
// #load "members/basics/test-hw.fsx"
// check "members/basics/test.fsx" Core_members_basics.RUN


#load "libtest/test.fsx"
check "libtest" Core_libtest.RUN

#load "int32/test.fsx" 
check "int32" Core_int32.RUN

#load "attributes/test.fsx"
check "attributes" Core_attributes.RUN

// FAILS - takes too long
//
// #load "controlWebExt/test.fsx"
// check "controlWebExt/test.fsx" Core_controlWebExt.RUN


// FAILS - no DataSvcUtil.exe
//
// #load "queriesOverOData/test.fsx"
// check "queriesOverOData/test.fsx" Core_queriesOverOData.RUN


match allErrors with 
| [] -> 
    System.Console.WriteLine("ALL PASSED!")
    exit 0
| _ -> 
    for (dir,err) in allErrors do
        System.Console.WriteLine("FAILURE: {0} - {1}", dir, err)
    exit 1


// ----------------------------------

(*
#load "csext/test.fsx"
#load "fsfromfsviacs/test.fsx"
#load "fsiAndModifiers/test.fsx"
#load "load-script/test.fsx"
#load "members/console/test.fsx"
#load "math/lapack/test.fsx"
#load "mscorlib/test.fsx"
#load "pinvoke/test.fsx"
#load "portable/portablelibrary/test.fsx"
#load "printing/test.fsx"
#load "queriesOverIQueryableLinqToEntities/test.fsx"
#load "queriesOverIQueryableLinqToSql/test.fsx"
#load "quotesInMultipleModules/test.fsx"
#load "topinit/test.fsx"

#load "controlWpf/test.fsx"
*)
