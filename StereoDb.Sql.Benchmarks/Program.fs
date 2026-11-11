open System
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open StereoDB
open StereoDB.FSharp
open BenchmarkDotNet.Configs

type Book = {
    Id: int
    Title: string
    Quantity: int
}

type SubBook = {
    Id: int
    Quantity: int
}

type MutableBook = {
    Id: int
    Title: string
    ISBN: string
    mutable Quantity: int
}

type NullableBook = {
    Id: int
    Title: string
    OptionalValue: option<int>
    NullableValueType: Nullable<int>
    NullableRefType: string
}
        
type Order = {
    Id: Guid
    BookId: int
    Quantity: int    
}

type Schema() =
    let _books = {| Table = StereoDb.createTable<int, Book>("books") |}
    let _mutableBooks = {| Table = StereoDb.createTable<int, MutableBook>("mutable_book") |}
    let _nullableBooks = {| Table = StereoDb.createTable<int, NullableBook>("nullable_books") |}
    
    let _ordersTable = StereoDb.createTable<Guid, Order>("orders")
    let _orders = {|
        Table = _ordersTable
        BookIdIndex = _ordersTable.AddValueIndex(fun order -> order.BookId)
        QuantityIndex = _ordersTable.AddRangeScanIndex(fun order -> order.Quantity)
    |}
    
    member this.Books = _books
    member this.MutableBooks = _mutableBooks
    member this.NullableBooks = _nullableBooks
    member this.Orders = _orders
    
    interface IDbSchema with
        member this.AllTables = [_books.Table; _mutableBooks.Table; _nullableBooks.Table; _orders.Table]

type StereoDbBenchmarks() =
    let db = StereoDb.create(Schema(), StereoDbSettings.Default)
    let parameterizedSql = $"SELECT Id, Quantity FROM Books WHERE Id = ?"
    let preparedQuery = db.PrepareExecSql<SubBook> parameterizedSql
    
    let maxIndex = 1_000_000
    let maxIndex = 1_000
    //let index = Random.Shared.Next(1, maxIndex)
    //let index = 500_000
    let index = 100
    do 
        // add books
        db.WriteTransaction(fun ctx ->
            let books = ctx.UseTable(ctx.Schema.Books.Table)
        
            for i in [1..maxIndex] do
                let book = { Id = i; Title = $"book_{i}"; Quantity = 1 }
                books.Set book
        )
    let sql = $"SELECT Id, Quantity FROM Books WHERE Id = {index}"
    
    [<Benchmark(Baseline = true)>]
    member this.PlainSQL() =
        db.ExecSql<SubBook> sql
    
    [<Benchmark>]
    member this.ParameterizedSQL() =
        db.ExecSql<SubBook> (parameterizedSql, index)

    [<Benchmark>]
    member this.PreparedParameterizedSQL() =
        preparedQuery.Invoke([| box index |])

[<EntryPoint>]
let main argv =
    let config = 
        DefaultConfig.Instance
            // Would be not needed if https://github.com/dotnet/BenchmarkDotNet/pull/3318 merged and released.
            .WithOptions(ConfigOptions.DisableOptimizationsValidator)
    BenchmarkRunner.Run<StereoDbBenchmarks>(config) |> ignore
    0
