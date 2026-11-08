namespace StereoDB.FSharp

open System

[<AutoOpen>]
module SqlExtensions =
    open StereoDB
    open StereoDB.Sql
           
    let internal readQueryExecution (context: ReadOnlyTsContext<'TSchema>) (func:QueryBuilder.QueryExecution) (parameters: obj[]) =
        match func with
        | QueryBuilder.Write _ ->
            failwith "Execution of UPDATE and DELETE queries from this method is not supported. Please use ExecuteSql<T>(string)"
        
        | QueryBuilder.Read caller -> 
            let value = caller.DynamicInvoke (Array.append [|context:> obj|] parameters)
            if value = null then ValueNone else ValueSome (value :?> System.Collections.Generic.List<'T>)

    let internal writeQueryExecution (context: ReadWriteTsContext<'TSchema>) (func:QueryBuilder.QueryExecution) (parameters: obj[]) : unit =
        match func with
        | QueryBuilder.Write caller -> caller.DynamicInvoke (Array.append [|context:> obj|] parameters) |> ignore
        | QueryBuilder.Read _       -> failwith "Execution of SELECT query from this method is not supported. Please use ExecuteSql<T>(string)"

    type StereoDB.FSharp.IStereoDb<'TSchema> with
        member this.ExecuteNonQuery(sql: string, [<ParamArray>] parameters: obj[]) =
            let query = SqlParser.parseSql sql
            this.WriteTransaction(fun (_rwCtx) -> 
                let func = QueryBuilder.buildQuery<'TSchema, unit> query _rwCtx _rwCtx.Schema
                writeQueryExecution _rwCtx func parameters)

        member this.ExecSql<'TResult>(sql: string, [<ParamArray>] parameters: obj[]): ResizeArray<'TResult> voption =
            let query = SqlParser.parseSql sql
            this.ReadTransaction(fun (_rCtx) -> 
                let func = QueryBuilder.buildQuery<'TSchema, 'TResult> query _rCtx _rCtx.Schema
                readQueryExecution _rCtx func parameters)

        // Executes a SQL query with the provided context and returns the result as a ResizeArray<'TResult> voption.
        // StereoDB.SQL assumes cooperative transaction management, so the context is passed to the query execution function.
        member this.ExecSqlWithContext<'TResult>(context: ReadOnlyTsContext<'TSchema>, sql: string, [<ParamArray>] parameters: obj[]): ResizeArray<'TResult> voption =
            let query = SqlParser.parseSql sql
            let func = QueryBuilder.buildQuery<'TSchema, 'TResult> query context context.Schema
            readQueryExecution context func parameters