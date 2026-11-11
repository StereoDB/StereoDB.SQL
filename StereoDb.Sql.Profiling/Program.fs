open System
open StereoDB
open StereoDB.FSharp
open Microsoft.DiagnosticsHub

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



let db = StereoDb.create(Schema(), StereoDbSettings.Default)
let parameterizedSql = $"SELECT Id, Quantity FROM Books WHERE Id = ?"
let preparedQuery = db.PrepareExecSql<SubBook> parameterizedSql
    
let maxIndex = 1_000_000
//let index = Random.Shared.Next(1, maxIndex)
//let index = 500_000
let index = 100
printfn $"Index: {index}"
do 
    // add books
    db.WriteTransaction(fun ctx ->
        let books = ctx.UseTable(ctx.Schema.Books.Table)
        
        for i in [1..maxIndex] do
            let book = { Id = i; Title = $"book_{i}"; Quantity = 1 }
            books.Set book
    )
let sql = $"SELECT Id, Quantity FROM Books WHERE Id = {index}"
UserMarks.EmitMessage("Initialization finished");

[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    for i = 0 to 100 do 
        let sql = $"SELECT Id, Quantity FROM Books WHERE Id = {i}"
        db.ExecSql<SubBook> sql |> ignore
    let elapsed = sw.Elapsed
    printfn $"Elapsed time: {elapsed.TotalMilliseconds} ms"
    let sw = System.Diagnostics.Stopwatch.StartNew()
    for i = 0 to 100 do 
        db.ExecSql<SubBook> (parameterizedSql, i) |> ignore
    let elapsed = sw.Elapsed
    printfn $"Elapsed time: {elapsed.TotalMilliseconds} ms"
    let sw = System.Diagnostics.Stopwatch.StartNew()
    for i = 0 to 100 do 
        preparedQuery.Invoke([| box i |]) |> ignore
    let elapsed = sw.Elapsed
    printfn $"Elapsed time: {elapsed.TotalMilliseconds} ms"
    0
