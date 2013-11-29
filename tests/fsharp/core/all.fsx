

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


(*
#load "queriesCustomQueryOps/test.fsx"
#load "queriesLeafExpressionConvert/test.fsx"
#load "queriesNullableOperators/test.fsx"
#load "queriesOverIEnumerable/test.fsx"
#load "queriesOverIQueryable/test.fsx"

check "queriesCustomQueryOps/test.fsx" Core_queriesCustomQueryOps.RUN
check "queriesLeafExpressionConvert/test.fsx" Core_queriesLeafExpressionConvert.RUN
check "queriesNullableOperators/test.fsx" Core_queriesNullableOperators.RUN
check "queriesOverIEnumerable/test.fsx" Core_queriesOverIEnumerable.RUN
check "queriesOverIQueryable/test.fsx" Core_queriesOverIQueryable.RUN
*)

(*
#load "subtype/test.fsx"
#load "syntax/test.fsx"
#load "tlr/test.fsx"
#load "unicode/test.fsx"

check "subtype" Core_subtype.RUN
check "syntax" (fun () -> if Core_syntax.RUN() then ["FAILED"] else [])
check "tlr" Core_tlr.RUN
check "unicode" Core_unicode.RUN
*)

(*
#load "quotes/test.fsx"
#load "quotesDebugInfo/test.fsx"
#load "seq/test.fsx"
#load "nested/test.fsx"
#load "patterns/test.fsx"
#load "printf/test.fsx"

check "quotes/test.fsx" Core_quotes.RUN
check "quotesDebugInfo/test.fsx" (fun () -> match Test.RUN() with 0 -> [] | n -> [sprintf "%d failures" n])
check "seq/test.fsx" Core_seq.RUN
check "nested/test.fsx" Core_nested.RUN
check "patterns/test.fsx" Core_patterns.RUN
check "printf/test.fsx" Core_printf.RUN
*)

#load "lift/test.fsx"
#load "members/basics/test-hw.fsx"
#load "members/ctree/test.fsx"
#load "members/factors/test.fsx"
#load "members/incremental/test-hw.fsx"
#load "members/incremental/test.fsx"
#load "members/ops/test.fsx"

check "lift" Core_lift.RUN
check "members/basics/test.fsx" Core_members_basics.RUN
check "members/ctree/test.fsx" Core_members_ctree.RUN
check "members/factors/test.fsx" Core_members_factors.RUN
check "members/incremental/test.fsx" Core_members_incremental.RUN
check "members/incremental/test-hw.fsx" Core_members_incremental_testhw.RUN
check "members/ops/test.fsx" Core_members_ops.RUN

(*
#load "longnames/test.fsx"
#load "map/test.fsx"
#load "lazy/test.fsx"
#load "letrec/test.fsx"
#load "libtest/test.fsx"
#load "int32/test.fsx"
#load "innerpoly/test.fsx"

#load "comprehensions/test.fsx"
#load "attributes/test.fsx"
#load "array/test.fsx"
#load "apporder/test.fsx"
#load "access/test.fsx"


check "access" Core_access.RUN
check "apporder" Core_apporder.RUN
check "array" Core_array.RUN
check "attributes" Core_attributes.RUN
check "comprehensions" Core_comprehensions.RUN
check "innerpoly" Core_innerpoly.RUN
check "int32" Core_int32.RUN
check "lazy" Core_lazy.RUN
check "libtest" Core_libtest.RUN
check "letrec" Core_letrec.RUN
check "longnames" Core_longnames.RUN
check "map" Core_map.RUN
*)
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
#load "genericmeasures/test.fsx"
#load "load-script/test.fsx"
#load "members/console/test.fsx"
#load "math/lalgebra/test.fsx"
#load "math/lapack/test.fsx"
#load "math/numbers/test.fsx"
#load "math/numbersVS2008/test.fsx"
#load "measures/test.fsx"
#load "mscorlib/test.fsx"
#load "pinvoke/test.fsx"
#load "portable/portablelibrary/test.fsx"
#load "printing/test.fsx"
#load "queriesOverIQueryableLinqToEntities/test.fsx"
#load "queriesOverIQueryableLinqToSql/test.fsx"
#load "queriesOverOData/test.fsx"
#load "quotesInMultipleModules/test.fsx"
#load "topinit/test.fsx"


#load "control/test.fsx"
#load "controlChamenos/test.fsx"
#load "controlMailbox/test.fsx"
#load "controlStackOverflow/test.fsx"
#load "controlWebExt/test.fsx"
#load "controlWpf/test.fsx"
*)
