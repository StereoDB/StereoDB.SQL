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
            let value = caller.DynamicInvoke [|context:> obj; parameters:> obj|]
            if value = null then ValueNone else ValueSome (value :?> System.Collections.Generic.List<'T>)

    let internal writeQueryExecution (context: ReadWriteTsContext<'TSchema>) (func:QueryBuilder.QueryExecution) (parameters: obj[]) : unit =
        match func with
        | QueryBuilder.Write caller -> caller.DynamicInvoke (Array.append [|context:> obj|] parameters) |> ignore
        | QueryBuilder.Read _       -> failwith "Execution of SELECT query from this method is not supported. Please use ExecuteSql<T>(string)"

    let internal createExecutionContext (parameters: obj[]) : Expr.ExecutionContext =
        { 
            parameters = parameters
            currentIndex = 0
            parametersExpression = System.Linq.Expressions.Expression.Parameter(typeof<obj[]>, "parameters")
        }

    type StereoDB.FSharp.IStereoDb<'TSchema> with
        member this.ExecuteNonQuery(sql: string, [<ParamArray>] parameters: obj[]) =
            let query = SqlParser.parseSql sql
            this.WriteTransaction(fun (_rwCtx) -> 
                let executionContext = createExecutionContext parameters
                let func = QueryBuilder.buildQuery<'TSchema, unit> query executionContext _rwCtx.Schema
                writeQueryExecution _rwCtx func parameters)

        member this.ExecSql<'TResult>(sql: string, [<ParamArray>] parameters: obj[]): ResizeArray<'TResult> voption =
            let query = SqlParser.parseSql sql
            this.ReadTransaction(fun (_rCtx) -> 
                let executionContext = createExecutionContext parameters
                let func = QueryBuilder.buildQuery<'TSchema, 'TResult> query executionContext _rCtx.Schema
                readQueryExecution _rCtx func parameters)

        // Executes a SQL query with the provided context and returns the result as a ResizeArray<'TResult> voption.
        // StereoDB.SQL assumes cooperative transaction management, so the context is passed to the query execution function.
        member this.ExecSqlWithContext<'TResult>(context: ReadOnlyTsContext<'TSchema>, sql: string, [<ParamArray>] parameters: obj[]): ResizeArray<'TResult> voption =
            let query = SqlParser.parseSql sql
            let executionContext = createExecutionContext parameters
            let func = QueryBuilder.buildQuery<'TSchema, 'TResult> query executionContext context.Schema
            readQueryExecution context func parameters