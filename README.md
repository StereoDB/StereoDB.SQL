# StereoDB.SQL

The SQL interface for StereoDB. It allows you to execute SQL queries against the StereoDB database.

## Benchmarks

```
dotnet run --project StereoDb.Sql.Benchmarks/StereoDb.Sql.Benchmarks.fsproj -с Release
```

Example output:
```
// * Summary *

BenchmarkDotNet v0.15.8, Windows 11 (10.0.26300.9818)
11th Gen Intel Core i5-1145G7 2.60GHz (Max: 2.61GHz), 1 CPU, 8 logical and 4 physical cores
.NET SDK 11.0.100-rc.3.26522.148
  [Host]     : .NET 10.0.14 (10.0.14, 10.0.1426.52326), X64 RyuJIT x86-64-v4 DEBUG
  DefaultJob : .NET 10.0.14 (10.0.14, 10.0.1426.52326), X64 RyuJIT x86-64-v4


| Method                   | Mean        | Error     | StdDev     | Median      | Ratio | RatioSD |
|------------------------- |------------:|----------:|-----------:|------------:|------:|--------:|
| PlainSQL                 | 1,894.32 us | 41.828 us | 122.014 us | 1,887.89 us |  1.00 |    0.09 |
| ParameterizedSQL         | 1,975.54 us | 77.166 us | 220.160 us | 1,904.68 us |  1.05 |    0.13 |
| PreparedParameterizedSQL |    29.63 us |  0.581 us |   0.938 us |    29.55 us |  0.02 |    0.00 |
```