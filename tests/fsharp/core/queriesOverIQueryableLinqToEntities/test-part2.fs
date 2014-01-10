// #Quotations

module internal Foo.TestPart2


open Foo.TestPart1
open System
open System.Linq
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq
open Microsoft.FSharp.Linq.RuntimeHelpers

module Queryable = 

    let db = NorthwndDb.GetDataContext()


    let checkLinqQueryText s (q1: System.Linq.IQueryable<'T>) text =
        //db.DataContext.Log <- System.Console.Out
        check s (try q1.Expression.ToString() with e -> "Unexpected error: " + e.ToString()) text

    let checkLinqSqlText s (q1: System.Linq.IQueryable<'T>) (text:string) =
        //db.DataContext.Log <- new System.IO.StringWriter()
        q1 |> Seq.length |> ignore
       // For now we don't actually test the query text, since I don't know how to log the SQL used by EF
        //check s (db.DataContext.Log.ToString().Split('\r','\n') |> Seq.filter (fun s -> not (s.StartsWith("--")) && not (String.IsNullOrWhiteSpace s)) |> String.concat "\n") (text.Replace("\r",""))

    let checkLinqSqlTextForValue s (q1: unit -> 'T) (text:string) =
        //db.DataContext.Log <- new System.IO.StringWriter()
        q1() |> ignore
       // For now we don't actually test the query text, since I don't know how to log the SQL used by EF
       //check s (db.DataContext.Log.ToString().Split('\r','\n') |> Seq.filter (fun s -> not (s.StartsWith("--")) && not (String.IsNullOrWhiteSpace s)) |> String.concat "\n") (text.Replace("\r",""))


    let customers = 
        checkLinqSqlText "vwe4yuwe091"
            (query { for c in db.Customers do select c })
            """SELECT [t0].[CustomerID], [t0].[CompanyName], [t0].[ContactName], [t0].[ContactTitle], [t0].[Address], [t0].[City], [t0].[Region], [t0].[PostalCode], [t0].[Country], [t0].[Phone], [t0].[Fax]
FROM [dbo].[Customers] AS [t0]"""

    let customerWithNamesStartingWithB = 
        checkLinqSqlText "vwe4yuwe092"
            (query { for c in db.Customers do where (c.CompanyName.StartsWith "A"); select c.ContactName })
            """SELECT [t0].[ContactName]
FROM [dbo].[Customers] AS [t0]
WHERE [t0].[CompanyName] LIKE @p0"""


    let customerNames = 
        checkLinqSqlText "vwe4yuwe093"
            (query { for c in db.Customers do select c.ContactName })
            """SELECT [t0].[ContactName]
FROM [dbo].[Customers] AS [t0]"""


    let customersAverageOrders = 
        checkLinqSqlTextForValue "vwe4yuwe094"
            (fun () -> query { for c in db.Customers do averageBy (float c.Orders.Count) })
            """SELECT AVG([t2].[value]) AS [value]
FROM (
    SELECT CONVERT(Float,(
        SELECT COUNT(*)
        FROM [dbo].[Orders] AS [t1]
        WHERE [t1].[CustomerID] = [t0].[CustomerID]
        )) AS [value]
    FROM [dbo].[Customers] AS [t0]
    ) AS [t2]"""

    let customersSorted = 
        checkLinqSqlText "vwe4yuwe095"
            (query { for c in db.Customers do sortBy c.Country; select (c.Country, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[CompanyName] AS [Item2]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country]"""


    let joinCustomersAndEmployeesByName = 
        checkLinqSqlText "vwe4yuwe096"
            (query { for c in db.Customers do join e in db.Employees on (c.Country = e.Country); select (c.ContactName, e.LastName) })
            """SELECT [t0].[ContactName] AS [Item1], [t1].[LastName] AS [Item2]
FROM [dbo].[Customers] AS [t0]
INNER JOIN [dbo].[Employees] AS [t1] ON [t0].[Country] = [t1].[Country]""" 



    let customersSortedDescending = 
        checkLinqSqlText "vwe4yuwe097"
            (query { for c in db.Customers do sortByDescending c.Country; select (c.Country, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[CompanyName] AS [Item2]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country] DESC""" 

    let customersSortedTwoColumns = 
        checkLinqSqlText "vwe4yuwe098"
            (query { for c in db.Customers do sortBy c.Country; thenBy c.Region; select (c.Country, c.Region, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[Region] AS [Item2], [t0].[CompanyName] AS [Item3]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country], [t0].[Region]""" 

    let customersSortedTwoColumnsAscendingDescending = 
        checkLinqSqlText "vwe4yuwe099"
            (query { for c in db.Customers do sortBy c.Country; thenByDescending c.Region; select (c.Country, c.Region, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[Region] AS [Item2], [t0].[CompanyName] AS [Item3]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country], [t0].[Region] DESC""" 


    let sumOfAllOrdersForCustomers = 
        checkLinqSqlTextForValue "vwe4yuwe09q"
            (fun () -> query { for c in db.Customers do sumBy (float c.Orders.Count) })
            """SELECT SUM([t2].[value]) AS [value]
FROM (
    SELECT CONVERT(Float,(
        SELECT COUNT(*)
        FROM [dbo].[Orders] AS [t1]
        WHERE [t1].[CustomerID] = [t0].[CustomerID]
        )) AS [value]
    FROM [dbo].[Customers] AS [t0]
    ) AS [t2]""" 

    let customersSortedTwoColumnsDescendingAscending = 
        checkLinqSqlText "vwe4yuwe09w"
            (query { for c in db.Customers do sortByDescending c.Country; thenBy c.Region; select (c.Country, c.Region, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[Region] AS [Item2], [t0].[CompanyName] AS [Item3]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country] DESC, [t0].[Region]""" 

    let customerSpecificsSorted = 
        checkLinqSqlText "vwe4yuwe09e"
            (query { for c in db.Customers do sortBy c.Country; select (c.Country, c.Region, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[Region] AS [Item2], [t0].[CompanyName] AS [Item3]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country]""" 

    let customerSpecificsSortedTwoColumns = 
        checkLinqSqlText "vwe4yuwe09r"
            (query { for c in db.Customers do sortBy c.Country; thenBy c.Region; select (c.Country, c.Region, c.CompanyName) })
            """SELECT [t0].[Country] AS [Item1], [t0].[Region] AS [Item2], [t0].[CompanyName] AS [Item3]
FROM [dbo].[Customers] AS [t0]
ORDER BY [t0].[Country], [t0].[Region]""" 

    let customerLongestNameLength = 
        checkLinqSqlTextForValue "vwe4yuwe09t"
            (fun () -> query { for c in db.Customers do maxBy c.ContactName.Length })
            """SELECT MAX(LEN([t0].[ContactName])) AS [value]
FROM [dbo].[Customers] AS [t0]"""  


    let sumOfLengthsOfCustomerNames = 
        checkLinqSqlTextForValue "vwe4yuwe09y7"
            (fun () -> query { for c in db.Customers do sumBy c.ContactName.Length })
            """SELECT SUM(LEN([t0].[ContactName])) AS [value]
FROM [dbo].[Customers] AS [t0]""" 

    let customersAtSpecificAddress = 
        checkLinqSqlText "vwe4yuwe09u"
            (query { for c in db.Customers do where (c.Address.Contains("Jardim das rosas")); select c })
            """SELECT [t0].[CustomerID], [t0].[CompanyName], [t0].[ContactName], [t0].[ContactTitle], [t0].[Address], [t0].[City], [t0].[Region], [t0].[PostalCode], [t0].[Country], [t0].[Phone], [t0].[Fax]
FROM [dbo].[Customers] AS [t0]
WHERE [t0].[Address] LIKE @p0""" 

    let customersAtSpecificAddressUsingIf = 
        checkLinqSqlText "vwe4yuwe09i"
            (query { for c in db.Customers do if (c.Address.Contains("Jardim das rosas")) then select c })
            """SELECT [t0].[CustomerID], [t0].[CompanyName], [t0].[ContactName], [t0].[ContactTitle], [t0].[Address], [t0].[City], [t0].[Region], [t0].[PostalCode], [t0].[Country], [t0].[Phone], [t0].[Fax]
FROM [dbo].[Customers] AS [t0]
WHERE [t0].[Address] LIKE @p0""" 

    checkLinqSqlText "vwe4yuwe09p"
        (query { for p in db.Products do groupBy p.ProductName into g; select g.Key })
        """SELECT [t0].[ProductName]
FROM [dbo].[Products] AS [t0]
GROUP BY [t0].[ProductName]""" 

    checkLinqSqlText "vwe4yuwe09a"
        (query { for p in db.Products do groupBy p.ProductName into g; select (g.Count()) })
        """SELECT COUNT(*) AS [value]
FROM [dbo].[Products] AS [t0]
GROUP BY [t0].[ProductName]""" 

    checkLinqQueryText "vwe4yuwe09s"
        (query { for p in db.Products do groupValBy p.UnitPrice p.ProductName into g; select (query { for x in g do averageByNullable x }) })
        """Convert(value(System.Data.Objects.ObjectSet`1[SqlEntityConnection1.Product])).MergeAs(AppendOnly).GroupBy(p => p.ProductName, p => p.UnitPrice).Select(g => g.Average(x => x))"""

    checkLinqSqlText "vwe4yuwe09d"
        (query { for p in db.Products do groupValBy p.UnitPrice p.ProductName into g; select (query { for x in g do averageByNullable x }) })
        """SELECT [t3].[value]
FROM (
    SELECT [t0].[ProductName]
    FROM [dbo].[Products] AS [t0]
    GROUP BY [t0].[ProductName]
    ) AS [t1]
OUTER APPLY (
    SELECT AVG([t2].[UnitPrice]) AS [value]
    FROM [dbo].[Products] AS [t2]
    WHERE [t1].[ProductName] = [t2].[ProductName]
    ) AS [t3]""" 


    let countOfAllUnitsInStockForAllProducts = 
        checkLinqSqlTextForValue "vwe4yuwe09f"
            (fun () -> query { for p in db.Products do sumBy  (int p.UnitsInStock.Value) })
            """SELECT SUM(CONVERT(Int,[t0].[UnitsInStock])) AS [value]
FROM [dbo].[Products] AS [t0]""" 

    let sumByUsingValue = 
        // .Net SqlClient Data Provider: Warning: Null value is eliminated by an aggregate or other SET operation..
        checkLinqSqlTextForValue "vwe4yuwe09g"
            (fun () -> query { for p in db.Employees do sumBy p.ReportsTo.Value })
            """SELECT SUM([t0].[ReportsTo]) AS [value]
FROM [dbo].[Employees] AS [t0]
.Net SqlClient Data Provider: Warning: Null value is eliminated by an aggregate or other SET operation..""" 

    let sumByNullableExample = 
        checkLinqSqlTextForValue "vwe4yuwe09h"
            (fun () -> query { for p in db.Employees do sumByNullable p.ReportsTo })
            """SELECT SUM([t0].[ReportsTo]) AS [value]
FROM [dbo].[Employees] AS [t0]
.Net SqlClient Data Provider: Warning: Null value is eliminated by an aggregate or other SET operation..""" 



    let namesAndIdsOfProductsGroupedByName = 
        checkLinqSqlText "vwe4yuwe09j"
            (query { for p in db.Products do groupBy p.Category.CategoryName into group; for p in group do select (group.Key, p.ProductName) })
            """SELECT [t2].[CategoryName] AS [Item1], [t3].[ProductName] AS [Item2]
FROM (
    SELECT [t1].[CategoryName]
    FROM [dbo].[Products] AS [t0]
    LEFT OUTER JOIN [dbo].[Categories] AS [t1] ON [t1].[CategoryID] = [t0].[CategoryID]
    GROUP BY [t1].[CategoryName]
    ) AS [t2]
CROSS JOIN ([dbo].[Products] AS [t3]
    LEFT OUTER JOIN [dbo].[Categories] AS [t4] ON [t4].[CategoryID] = [t3].[CategoryID])
WHERE [t2].[CategoryName] = [t4].[CategoryName]""" 

    let productsGroupedByNameAndCountedTest1 =
        checkLinqSqlText "vwe4yuwe09k"
            (query { for p in db.Products do
                     groupBy p.Category.CategoryName into group
                     let sum = 
                       query { for p in group do
                                sumBy (int p.UnitsInStock.Value) }
                     select (group.Key, sum) })
            """SELECT [t2].[CategoryName] AS [Item1], (
    SELECT SUM(CONVERT(Int,[t3].[UnitsInStock]))
    FROM [dbo].[Products] AS [t3]
    LEFT OUTER JOIN [dbo].[Categories] AS [t4] ON [t4].[CategoryID] = [t3].[CategoryID]
    WHERE [t2].[CategoryName] = [t4].[CategoryName]
    ) AS [Item2]
FROM (
    SELECT [t1].[CategoryName]
    FROM [dbo].[Products] AS [t0]
    LEFT OUTER JOIN [dbo].[Categories] AS [t1] ON [t1].[CategoryID] = [t0].[CategoryID]
    GROUP BY [t1].[CategoryName]
    ) AS [t2]""" 

    let sumOfUnitsInStock = 
        checkLinqSqlTextForValue "vwe4yuwe09l"
            (fun () -> query { for p in db.Products do sumBy (int p.UnitsInStock.Value) })
            """SELECT SUM(CONVERT(Int,[t0].[UnitsInStock])) AS [value]
FROM [dbo].[Products] AS [t0]""" 

    let namesAndIdsOfProductsGroupdByID = 
        checkLinqSqlText "vwe4yuwe09z"
            (query { for p in db.Products do
                     groupBy p.CategoryID into group
                     for p in group do
                     select (group.Key, p.ProductName, p.ProductID) })
            """SELECT [t1].[CategoryID] AS [Item1], [t2].[ProductName] AS [Item2], [t2].[ProductID] AS [Item3]
FROM (
    SELECT [t0].[CategoryID]
    FROM [dbo].[Products] AS [t0]
    GROUP BY [t0].[CategoryID]
    ) AS [t1]
CROSS JOIN [dbo].[Products] AS [t2]
WHERE (([t1].[CategoryID] IS NULL) AND ([t2].[CategoryID] IS NULL)) OR (([t1].[CategoryID] IS NOT NULL) AND ([t2].[CategoryID] IS NOT NULL) AND ([t1].[CategoryID] = [t2].[CategoryID]))""" 

(*
    let minUnitPriceOfProductsGroupedByName = 
        checkLinqSqlText "vwe4yuwe09x"
            (query { for p in db.Products do
                     groupBy p.Category into group
                     let minOfGroup = 
                          query { for p in group do 
                                    minByNullable p.UnitPrice }
                     select (group.Key.CategoryName, minOfGroup) })
            """SELECT [t2].[CategoryName] AS [Item1], (
    SELECT MIN([t3].[UnitPrice])
    FROM [dbo].[Products] AS [t3]
    WHERE (([t1].[CategoryID] IS NULL) AND ([t3].[CategoryID] IS NULL)) OR (([t1].[CategoryID] IS NOT NULL) AND ([t3].[CategoryID] IS NOT NULL) AND ([t1].[CategoryID] = [t3].[CategoryID]))
    ) AS [Item2]
FROM (
    SELECT [t0].[CategoryID]
    FROM [dbo].[Products] AS [t0]
    GROUP BY [t0].[CategoryID]
    ) AS [t1]
LEFT OUTER JOIN [dbo].[Categories] AS [t2] ON [t2].[CategoryID] = [t1].[CategoryID]""" 
*)
    let db2 = (db.DataContext :?> NorthwndDb.ServiceTypes.EntityContainer)
    let crossJoinOfCustomersAndEmployeesUsingPropertyGetForDb = 
        checkLinqSqlText "vwe4yuwe09c"
            (query { for c in db2.Customers do 
                     for e in db2.Employees do 
                     select (c.CompanyName, e.LastName) })
            """SELECT [t0].[CompanyName] AS [Item1], [t1].[LastName] AS [Item2]
FROM [dbo].[Customers] AS [t0], [dbo].[Employees] AS [t1]""" 


    let crossJoinOfCustomersAndEmployeesWithParam (db2: NorthwndDb.ServiceTypes.EntityContainer) = 
        checkLinqSqlText "vwe4yuwe09v"
            (query { for c in db2.Customers do 
                     for e in db2.Employees do 
                     select (c.CompanyName, e.LastName) })
            """SELECT [t0].[CompanyName] AS [Item1], [t1].[LastName] AS [Item2]
FROM [dbo].[Customers] AS [t0], [dbo].[Employees] AS [t1]""" 

    crossJoinOfCustomersAndEmployeesWithParam db2

    let crossJoinOfCustomersAndEmployeesWithParamUnerased (db: NorthwndDb.ServiceTypes.SimpleDataContextTypes.EntityContainer) = 
        checkLinqSqlText "vwe4yuwe09b"
            (query { for c in db.Customers do 
                     for e in db.Employees do 
                     select (c.CompanyName, e.LastName) })
            """SELECT [t0].[CompanyName] AS [Item1], [t1].[LastName] AS [Item2]
FROM [dbo].[Customers] AS [t0], [dbo].[Employees] AS [t1]""" 

    crossJoinOfCustomersAndEmployeesWithParamUnerased db

    let crossJoinOfCustomersAndEmployeesUsingPropertyGetOfErasedTypeWithCoercionForDb = 
        checkLinqSqlText "vwe4yuwe09n"
            (query { for c in db.Customers do 
                     for e in db.Employees do 
                     select (c.CompanyName, e.LastName) })
            """SELECT [t0].[CompanyName] AS [Item1], [t1].[LastName] AS [Item2]
FROM [dbo].[Customers] AS [t0], [dbo].[Employees] AS [t1]""" 



    let innerJoinQuery = 
        checkLinqSqlText "vwe4yuwe09m"
            (query { for c in db.Categories do
                     join p in db.Products on (c.CategoryID =? p.CategoryID) 
                     select (p.ProductName, c.CategoryName) })
            """SELECT [t1].[ProductName] AS [Item1], [t0].[CategoryName] AS [Item2]
FROM [dbo].[Categories] AS [t0]
INNER JOIN [dbo].[Products] AS [t1] ON ([t0].[CategoryID]) = [t1].[CategoryID]""" 

    let joinCustomersAndEmployeesByNameUsingLoopAndConstraint = 
        checkLinqSqlText "vwe4yuwe0911"
            (query { for c in db.Customers do 
                     for e in db.Employees do 
                     where (c.Country = e.Country)
                     select (c.ContactName + " " + e.LastName) })
            """SELECT ([t0].[ContactName] + @p0) + [t1].[LastName] AS [value]
FROM [dbo].[Customers] AS [t0], [dbo].[Employees] AS [t1]
WHERE [t0].[Country] = [t1].[Country]"""  

    let innerJoinQueryUsingLoopAndConstraint = 
        checkLinqSqlText "vwe4yuwe0922"
            (query { for c in db.Categories do
                     for p in db.Products do
                     where (c.CategoryID =? p.CategoryID)
                     select (p.ProductName, c.CategoryName) })
            """SELECT [t1].[ProductName] AS [Item1], [t0].[CategoryName] AS [Item2]
FROM [dbo].[Categories] AS [t0], [dbo].[Products] AS [t1]
WHERE ([t0].[CategoryID]) = [t1].[CategoryID]""" 

    let innerGroupJoinQuery =
        checkLinqSqlText "vwe4yuwe0933"
            (query { for c in db.Categories do
                     groupJoin p in db.Products on ( c.CategoryID =? p.CategoryID) into prodGroup
                     select (c.CategoryName, prodGroup) })
            """SELECT [t0].[CategoryName] AS [Item1], [t1].[ProductID], [t1].[ProductName], [t1].[SupplierID], [t1].[CategoryID], [t1].[QuantityPerUnit], [t1].[UnitPrice], [t1].[UnitsInStock], [t1].[UnitsOnOrder], [t1].[ReorderLevel], [t1].[Discontinued], (
    SELECT COUNT(*)
    FROM [dbo].[Products] AS [t2]
    WHERE ([t0].[CategoryID]) = [t2].[CategoryID]
    ) AS [value]
FROM [dbo].[Categories] AS [t0]
LEFT OUTER JOIN [dbo].[Products] AS [t1] ON ([t0].[CategoryID]) = [t1].[CategoryID]
ORDER BY [t0].[CategoryID], [t1].[ProductID]""" 


    let innerGroupJoinQueryWithAggregation =
        checkLinqSqlText "vwe4yuwe0955"
            (query { for c in db.Categories do
                     groupJoin p in db.Products on ( c.CategoryID =? p.CategoryID) into prodGroup
                     let groupMax = query { for p in prodGroup do maxByNullable p.UnitsOnOrder }
                     select (c.CategoryName, groupMax) })
            """SELECT [t0].[CategoryName] AS [Item1], (
    SELECT MAX([t1].[UnitsOnOrder])
    FROM [dbo].[Products] AS [t1]
    WHERE ([t0].[CategoryID]) = [t1].[CategoryID]
    ) AS [Item2]
FROM [dbo].[Categories] AS [t0]""" 

    let innerGroupJoinQueryWithFollowingLoop =
        checkLinqSqlText "vwe4yuwe0966"
            (query { for c in db.Categories do
                     groupJoin p in db.Products on ( c.CategoryID =? p.CategoryID) into prodGroup
                     for prod2 in prodGroup do 
                     where (prod2.UnitPrice ?> 2.50M)
                     select (c.CategoryName, prod2) }    )
            """SELECT [t0].[CategoryName] AS [Item1], [t1].[ProductID], [t1].[ProductName], [t1].[SupplierID], [t1].[CategoryID], [t1].[QuantityPerUnit], [t1].[UnitPrice], [t1].[UnitsInStock], [t1].[UnitsOnOrder], [t1].[ReorderLevel], [t1].[Discontinued]
FROM [dbo].[Categories] AS [t0], [dbo].[Products] AS [t1]
WHERE ([t1].[UnitPrice] > @p0) AND (([t0].[CategoryID]) = [t1].[CategoryID])""" 

    let leftOuterJoinQuery =
        checkLinqSqlText "vwe4yuwe0977"
            (query { for c in db.Categories do
                     groupJoin p in db.Products on ( c.CategoryID =? p.CategoryID) into prodGroup
                     let prodGroup = System.Linq.Enumerable.DefaultIfEmpty prodGroup
                     for item in prodGroup do
                        select (c.CategoryName, (match item with null -> "" | _ -> item.ProductName)) })
            """SELECT [t0].[CategoryName] AS [Item1], 
    (CASE 
        WHEN [t2].[test] IS NULL THEN CONVERT(NVarChar(40),@p0)
        ELSE [t2].[ProductName]
     END) AS [Item2]
FROM [dbo].[Categories] AS [t0]
LEFT OUTER JOIN (
    SELECT 1 AS [test], [t1].[ProductName], [t1].[CategoryID]
    FROM [dbo].[Products] AS [t1]
    ) AS [t2] ON ([t0].[CategoryID]) = [t2].[CategoryID]""" 

    let checkForLongCustomerNameLength = 
        checkLinqSqlTextForValue "vwe4yuwe0988"
            (fun () -> query { for c in db.Customers do 
                               exists (c.Address.Length > 10) })
            """SELECT 
    (CASE 
        WHEN EXISTS(
            SELECT NULL AS [EMPTY]
            FROM [dbo].[Customers] AS [t0]
            WHERE LEN([t0].[Address]) > @p0
            ) THEN 1
        ELSE 0
     END) AS [value]""" 

    let checkCustomerNameLengthsAreNotAllShort = 
        checkLinqSqlTextForValue "vwe4yuwe0999"
            (fun () -> query { for c in db.Customers do all (c.Address.Length < 10) })
            """SELECT 
    (CASE 
        WHEN NOT (EXISTS(
            SELECT NULL AS [EMPTY]
            FROM [dbo].[Customers] AS [t1]
            WHERE (
                (CASE 
                    WHEN LEN([t1].[Address]) < @p0 THEN 1
                    ELSE 0
                 END)) = 0
            )) THEN 1
        WHEN NOT NOT (EXISTS(
            SELECT NULL AS [EMPTY]
            FROM [dbo].[Customers] AS [t1]
            WHERE (
                (CASE 
                    WHEN LEN([t1].[Address]) < @p0 THEN 1
                    ELSE 0
                 END)) = 0
            )) THEN 0
        ELSE NULL
     END) AS [value]""" 



    let queryWithOrderByInStrangePosition = 
        checkLinqSqlText "vwe4yuwe09qq"
            (query { for c in db.Customers do
                     sortBy c.City
                     where (c.Country = "UK")
                     select c.CompanyName })
            """SELECT [t0].[CompanyName]
FROM [dbo].[Customers] AS [t0]
WHERE [t0].[Country] = @p0
ORDER BY [t0].[City]""" 

    let queryWithNestedQueryInLetBeforeFinalSelect = 
        checkLinqSqlText "vwe4yuwe09ww"
            (query { for c in db.Customers do
                     let orders = query { for o in db.Orders do where (o.CustomerID = c.CustomerID); select o }
                     select (c.ContactName,orders) })
            """SELECT [t0].[ContactName] AS [Item1], [t1].[OrderID], [t1].[CustomerID], [t1].[EmployeeID], [t1].[OrderDate], [t1].[RequiredDate], [t1].[ShippedDate], [t1].[ShipVia], [t1].[Freight], [t1].[ShipName], [t1].[ShipAddress], [t1].[ShipCity], [t1].[ShipRegion], [t1].[ShipPostalCode], [t1].[ShipCountry], (
    SELECT COUNT(*)
    FROM [dbo].[Orders] AS [t2]
    WHERE [t2].[CustomerID] = [t0].[CustomerID]
    ) AS [value]
FROM [dbo].[Customers] AS [t0]
LEFT OUTER JOIN [dbo].[Orders] AS [t1] ON [t1].[CustomerID] = [t0].[CustomerID]
ORDER BY [t0].[CustomerID], [t1].[OrderID]""" 

(*
    let queryWithExplicitNestedEnumerableQueryInLetBeforeFinalSelect = 
        checkLinqSqlText "vwe4yuwe09ee"
            (query { for c in db.Customers do
                     let orders = query { for o in 0 .. 100 do select (o+1) }
                     select (c.ContactName,orders) })
            """SELECT [t0].[ContactName] AS [Item1]
FROM [dbo].[Customers] AS [t0]""" 

    let queryWithImplicitNestedEnumerableQueryInLetBeforeFinalSelect = 
        checkLinqSqlText "vwe4yuwe09rr"
            (query { for c in db.Customers do
                     let orders = query { for o in 0 .. 100 do select (o+1) }
                     select (c.ContactName,orders) })
            """SELECT [t0].[ContactName] AS [Item1]
FROM [dbo].[Customers] AS [t0]""" 
*)

    let queryWithNestedQueryInFinalSelect = 
        checkLinqSqlText "vwe4yuwe09tt"
            (query { for c in db.Customers do
                     select (c.ContactName, query { for o in db.Orders do where (o.CustomerID = c.CustomerID); select o }) })
            """SELECT [t0].[ContactName] AS [Item1], [t1].[OrderID], [t1].[CustomerID], [t1].[EmployeeID], [t1].[OrderDate], [t1].[RequiredDate], [t1].[ShippedDate], [t1].[ShipVia], [t1].[Freight], [t1].[ShipName], [t1].[ShipAddress], [t1].[ShipCity], [t1].[ShipRegion], [t1].[ShipPostalCode], [t1].[ShipCountry], (
    SELECT COUNT(*)
    FROM [dbo].[Orders] AS [t2]
    WHERE [t2].[CustomerID] = [t0].[CustomerID]
    ) AS [value]
FROM [dbo].[Customers] AS [t0]
LEFT OUTER JOIN [dbo].[Orders] AS [t1] ON [t1].[CustomerID] = [t0].[CustomerID]
ORDER BY [t0].[CustomerID], [t1].[OrderID]""" 

    // The following example demonstrates how to use a composite key to join data from three tables:
    let compositeKeyQuery = 
        checkLinqSqlText "vwe4yuwe09yy"
            (query { for o in db.Orders do
                     for p in db.Products do
                     groupJoin d in db.Order_Details on ( (o.OrderID, p.ProductID) = (d.OrderID, d.ProductID)) into details
                     for d in details do
                     select (o.OrderID, p.ProductID, d.UnitPrice) })
            """SELECT [t0].[OrderID] AS [Item1], [t1].[ProductID] AS [Item2], [t2].[UnitPrice] AS [Item3]
FROM [dbo].[Orders] AS [t0], [dbo].[Products] AS [t1], [dbo].[Order Details] AS [t2]
WHERE ([t0].[OrderID] = [t2].[OrderID]) AND ([t1].[ProductID] = [t2].[ProductID])""" 

    let firstCustomerWithNamesStartingWithB = 
        checkLinqSqlTextForValue "vwe4yuwe09uu"
            (fun () -> query { for c in db.Customers do where (c.ContactName.StartsWith "B"); headOrDefault })
            """SELECT TOP (1) [t0].[CustomerID], [t0].[CompanyName], [t0].[ContactName], [t0].[ContactTitle], [t0].[Address], [t0].[City], [t0].[Region], [t0].[PostalCode], [t0].[Country], [t0].[Phone], [t0].[Fax]
FROM [dbo].[Customers] AS [t0]
WHERE [t0].[ContactName] LIKE @p0""" 

    let distinctCompanyNames = 
        checkLinqSqlText "vwe4yuwe09"
            (query { for c in db.Customers do 
                     select c.CompanyName
                     distinct } )
            """SELECT DISTINCT [t0].[CompanyName]
FROM [dbo].[Customers] AS [t0]""" 


#if COMPILED
    [<System.STAThread>]
    do()
#endif



#if ALL_IN_ONE
let RUN() = !failures
#else
let aa =
  match !failures with 
  | [] -> 
      stdout.WriteLine "Test Passed"
      System.IO.File.WriteAllText("test.ok","ok")
      exit 0
  | _ -> 
      stdout.WriteLine "Test Failed"
      exit 1
#endif



