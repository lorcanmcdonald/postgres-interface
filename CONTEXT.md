# postgres-interface

A Haskell library for exposing arbitrary data sources as PostgreSQL-compatible servers, allowing any PostgreSQL client (Grafana, psql, etc.) to query Haskell functions without knowing they are not talking to real PostgreSQL.

## Language

**Queryable**:
A typeclass that a Haskell type implements to declare itself as a table source — exposing its column schema and how to serialise its values to wire-level column values.
_Avoid_: Serialisable, table-able, exportable

**Schema**:
The column names and types that a `Queryable` type exposes, used to describe the table to clients and validate query compatibility.
_Avoid_: Columns, structure, metadata

**ColumnValue**:
A wire-level sum type representing a single cell value in a result row — the boundary between typed Haskell values and the PostgreSQL wire protocol.
_Avoid_: Cell, field value, serialised value

**QueryPlan**:
The parsed SQL predicates and operations passed to a `Queryable` implementation at query time, allowing the implementation to generate only the data the query needs.
_Avoid_: Query, parsed query, filter

**NaiveQueryPlan**:
A mode where the library applies SQL predicates itself against a user-provided stream, at the cost of potential materialisation. An easy-path escape hatch for implementations that do not need pushdown.
_Avoid_: Auto-filter, server-side filter

**AnyTable**:
An existential wrapper that holds a `Queryable` instance, enabling heterogeneous collections of tables with different row types to coexist in a single server.
_Avoid_: Table, heterogeneous table, erased table

**Table**:
A named, registered data source backed by a `Queryable` type, serving rows in response to SQL queries from connected clients.
_Avoid_: Relation, dataset, source

**Stream**:
A conduit `ConduitT` of row values, produced by a `Queryable` implementation at query time and consumed by the protocol layer to write wire messages without full materialisation.
_Avoid_: List, lazy list, result set

**SimpleQuery**:
The PostgreSQL wire protocol flow where a client sends a raw SQL string and the server parses, executes, and returns results in one round-trip. The only query mode supported in v1.
_Avoid_: Query, request

**WireProtocol**:
The PostgreSQL Frontend/Backend Protocol implemented from scratch in Haskell — handling startup handshake, authentication, SimpleQuery flow, and pg_catalog stubs. A self-contained module boundary separate from query execution.
_Avoid_: Protocol, Postgres protocol

**QueryError**:
A structured error returned by `toQuery` when a SQL operation cannot be satisfied — maps to a PostgreSQL `ErrorResponse` on the wire so clients receive a standard Postgres error.
_Avoid_: Error, exception, failure

**Aggregatable**:
A planned opt-in typeclass for efficient server-side aggregation, enabling pushdown of `GROUP BY` operations. Not in v1 — v1 materialises rows and aggregates in the library.
_Avoid_: Groupable, foldable
