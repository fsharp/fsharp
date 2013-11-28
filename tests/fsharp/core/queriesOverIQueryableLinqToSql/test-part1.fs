// #Quotations

module TestPart1

open Microsoft.FSharp.Linq
open Microsoft.FSharp.Data.TypeProviders


open System
open System.Linq
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq.RuntimeHelpers

[<AutoOpen>]
module Infrastructure =
    let mutable failures = []
    let reportFailure s = 
        stderr.WriteLine " NO"; failures <- s :: failures

    let argv = System.Environment.GetCommandLineArgs() 
    let SetCulture() = 
        if argv.Length > 2 && argv.[1] = "--culture" then  
            let cultureString = argv.[2] 
            let culture = new System.Globalization.CultureInfo(cultureString) 
            stdout.WriteLine ("Running under culture "+culture.ToString()+"...");
            System.Threading.Thread.CurrentThread.CurrentCulture <-  culture
  
    do SetCulture()    

    let check  s v1 v2 = 
       if v1 = v2 then 
           printfn "test %s...passed " s 
       else 
           failures <- failures @ [(s, box v1, box v2)]
           printfn "test %s...failed, expected \n\t%A\ngot\n\t%A" s v2 v1

    let test s b = check s b true
    let qmap f (x:System.Linq.IQueryable<_>) = x |> Seq.map f |> System.Linq.Queryable.AsQueryable

    let checkCommuteSeq s (q1: System.Linq.IQueryable<'T>) q2 =
        check s (q1 |> Seq.toList) (q2 |> Seq.toList)

    let checkCommuteVal s q1 q2 =
        check s q1 q2

 
type NorthwndDb = SqlDataConnection<ConnectionString = @"AttachDBFileName  = 'C:\fsharp\vspro\devdiv\extras\extenders\docs\tutorial\northwnd.mdf';Server='.\SQLEXPRESS';User Instance=true;Integrated Security=SSPI",ForceUpdate=false,Pluralize=true,ContextTypeName = "Northwnd",LocalSchemaFile="schema1.dbml">

module Test = begin end

module DuplicateTypes = 
     
    type NorthwndDb = SqlDataConnection<ConnectionString = @"AttachDBFileName  = 'c:\fsharp\vspro\devdiv\extras\extenders\docs\tutorial\northwnd.mdf';Server='.\SQLEXPRESS';User Instance=true;Integrated Security=SSPI",ForceUpdate=false,Pluralize=true,ContextTypeName = "Northwnd",LocalSchemaFile="schema2.dbml">


 
type NorthwndDb2 = SqlDataConnection<ConnectionString = @"AttachDBFileName  = 'c:\fsharp\vspro\devdiv\extras\extenders\docs\tutorial\northwnd.mdf';Server='.\SQLEXPRESS';User Instance=true;Integrated Security=SSPI",ForceUpdate=false,Pluralize=true,ContextTypeName = "Northwnd",LocalSchemaFile="schema3.dbml">
 
