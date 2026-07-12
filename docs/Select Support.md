# SELECT Support

Which SQL SELECT constructs are supported by postgres-interface.

## FROM clause

- [x] Table reference — `FROM t`
- [x] Table alias — `FROM t AS foo`
- [x] `JOIN ON` — `FROM t JOIN u ON t.id = u.id`
- [x] Subquery with alias — `FROM (SELECT ...) AS sub`
- [x] Multiple tables (cross join) — `FROM t, u`
- [ ] `JOIN USING (col)`
- [ ] `NATURAL JOIN`
- [ ] `LATERAL`
- [ ] Table functions — `FROM generate_series(...)`
- [ ] `SELECT` without a `FROM` clause

## SELECT list

- [x] `SELECT *`
- [x] Named columns — `SELECT a, b`
- [x] Column alias — `SELECT date AS "time"`
- [x] Expression with alias (silently empty at execution) — `SELECT SUM(x) AS total`
- [ ] `SELECT *, foo` — bare `*` mixed with other columns
- [ ] Expression without alias — `SELECT COUNT(*)`

## WHERE clause

- [x] `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`
- [x] `AND`, `OR`, `NOT`
- [x] `LIKE`, `ILIKE`
- [x] `IS NULL`, `IS NOT NULL`
- [x] `IS TRUE`, `IS FALSE`, `IS NOT TRUE`, `IS NOT FALSE`
- [x] `IN (literal, ...)`, `NOT IN (literal, ...)`
- [x] `BETWEEN` (literal constants only)
- [x] Column typecast — `col::type = lit`
- [x] Column–column comparison — `t.id = u.id`
- [ ] `SIMILAR TO`
- [ ] `IN (SELECT ...)` — subquery form
- [ ] Literal on the left-hand side — `42 = col`
- [ ] `BETWEEN` with column references

## ORDER BY

- [x] `ORDER BY col ASC`
- [x] `ORDER BY col DESC`
- [x] `ORDER BY col` (defaults to ASC)
- [ ] `ORDER BY` with `USING` operator syntax

## LIMIT / OFFSET

- [x] `LIMIT n`
- [ ] `OFFSET n` without `LIMIT`
- [ ] `LIMIT ALL`
- [ ] `FETCH FIRST n ROWS ONLY`
- [ ] Non-integer `LIMIT` expressions

## GROUP BY

- [x] `GROUP BY col`
- [ ] `GROUP BY` expression
- [ ] `ROLLUP`, `CUBE`, `GROUPING SETS`
- [ ] `HAVING`

## Top-level SELECT forms

- [x] Simple `SELECT`
- [x] Parenthesised `SELECT`
- [ ] `WITH` (CTEs)
- [ ] `SELECT INTO`
- [ ] `DISTINCT ON (expr)`
- [ ] `WINDOW`
- [ ] `UNION`, `INTERSECT`, `EXCEPT`
