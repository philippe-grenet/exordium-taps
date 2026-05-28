# BQL Next Mode: implementation plan

## Goal

Implement a new mode for syntax highlighting of BQL.Next queries. This directory already
contains a mode for BQL (bql-mode.el). BQL.Next is a version 2.0 of the language, which
has a different syntax. Note that BQL.Next is still experimental at this stage.

Specifically:
- The code should be put into a new file `bql-next-mode.el` in the same directory.
  No other file should be changed.
- The mode

## BQL.Next Syntax Reference for Emacs Mode

This section outlines the syntax elements, keywords, functions, and operators proposed
for BQL.Next to assist in building an Emacs syntax highlighting mode. BQL.Next includes
multiple syntax variants (e.g., "programmer-ish" pipeline syntax, "non-technical"
Simplified English, and direct dot-notation expressions) , so this list encompasses
elements across these styles.

### 1. Keywords (Clauses and Subclauses)

These are the primary structural keywords used to construct queries, pipelines, and variable assignments.

- Data Loading & Retrieval: `load`, `retrieve`, `get`, `show`, `fetch`
- Filtering & Shaping: `filter`, `top`, `sort`, `skip`, `take`, `order`, `by`
- Variable Assignment & Aliasing: `let`, `set`, `as`
- Grouping & Aggregation: `group`, `aggregate`, `per`
- Set Operations: `union`, `intersect`, `except`
- Graph Navigation Verbs: `has`, `having`, `is`
- Other/Legacy: `for`, `with`, `on`
.
### 2. Built-in Functions & Analytics

BQL.Next supports a wide range of analytical, mathematical, and string manipulation functions.

- Aggregation/Windowing: `avg`, `sum`, `count`, `count_all`, `wavg`, `medianif`,
  `sumif`, `countif`, `groupavg`, `groupsort`, `groupcount`
- Analytics & Tuples: `correlation`, `zscore`, `pct_diff`
- String Manipulation: `concat`, `textjoin`, `left`
- Math & Formatting: `round`, `floor`, `mod`, `abs`, `sqrt`, `format`
- Logical & Control Flow: `if`, `avail`, `any`, `all`, `in`, `dropna`, `matches`, `replaceNA`
- Date/Time: `year`, `month`, `range`, `today`
- Legacy Data Mapping: `value`, `to_ids`

### 3. Operators & Symbols

BQL.Next replaces heavily nested functions with pipeline and traversal operators.

- Pipeline & Evaluation:
    - `|>` (Pipe operator)
    - `.` (Dot notation for field access, method chaining, and expression evaluation)
- Graph Traversal:
    - `->`, `=>` (Forward edge/relationship traversal)
    - `<-`, `<=` (Reverse edge/relationship traversal)
- Logical Operators: `and`, `&&`, `or`, `||`, `not`, `!`
- Comparison Operators: `==`, `=`, `<`, `>`, `<=`, `>=`, `<>`, `!=`
- Arithmetic Operators: `+`, `-`, `*`, `/`
- Assignment & Aliasing: `:` (used for entity aliasing, e.g., `e:Equity`), `=`
- Entity Reference Symbol: `@` (used to prefix specific entity IDs, e.g., `@'GOOG US Equity'`)
- Variable Prefix: `#` (used in hybrid queries to denote variables, e.g., `#cond`)
.
### 4. Constants and Special Tokens

- `true`, `false`
- `null`, `na`, `NA`
- `ALL` (used as a universe parameter, e.g., `trade_partner=ALL`)

### 5. Delimiters & Punctuation

- Strings: Single quotes (`'`) and double quotes (`"`)
- Lists/Collections: Square brackets (`[`, `]`) for lists and arrays
- Tuples/CDTs: Angle brackets (`<`, `>`) or curly braces (`{`, `}`) for constructed data
  types and complex filtering
- Function Arguments: Parentheses (`(`, `)`)

### 6. Comments

Single-line comments: `//` and `##` are both used to denote single-line comments in BQL.Next query snippets

## Example queries for testing

Here are several example queries across the different proposed BQL.Next syntax
variants. These snippets cover a wide range of use cases and can be used to test your
Emacs syntax highlighting mode to ensure keywords, operators, strings, and variables are
parsed correctly.

### Programmer-ish (Pipeline) Syntax

```
// Single entity with multiple attributes
load @'GOOG US Equity'
|> px_last, px_volume

// Seed universe with filtering and explicit aliasing
load e:Equity
|> filter e.active == true && e.primary == true && e.market_cap > 10b
|> e.ID, e.px_last

// Multi-hop graph traversal with complex inline filtering
load @'SPX Index'=>members(weights == 4 && position > p.attr1)=>parent:p=>bond
|> name
```

### Non-Technical (Action-Oriented) Syntax

```
## Simple retrieval and projection
retrieve 'GOOG US Equity'
show px_last, px_volume

## Complex filtering, sorting, and slicing
retrieve 'SPX Index' members
filter cntry_of_domicile == 'US'
top 3 by cur_mkt_cap
show cur_mkt_cap, esg_score

## Tuple algebra and intermediate variables
retrieve 'SPX Index'=>members:e1,e2
let c = correlation(e1.px_last, e2.px_last)
let z = zscore(e1.px_last - e2.px_last)
filter (e1.id <> e2.id) AND (e1.sector == e2.sector) AND c > 0.8
top 3 by z desc
show e1.name, e2.name, c, z
```

### Direct Expressions (Dot Notation)

```
// Atomic entity and field access
'GOOG US Equity'.px_last

// List evaluation
['GOOG US Equity', 'AAPL US Equity'].px_last

// Cross-entity math
'GOOG US Equity'.px_last / 'TSLA US Equity'.px_last

// Inline filtering on a universe
equity(active, primary, market_cap > 10b).px_last

// Multi-hop graph traversal expression
'SPX Index'=>members(weights > 0.1)=>parent=>bonds.name
```

### Set Operations and Advanced Chaining

```
// Union of different entities and filtered universes
retrieve 'INDU Index'=>members
union 'ADSK US Equity'
union equity(active, primary, cntry_of_domicile=='US')
show px_last

// Value function replacement with explicit mapping
retrieve equity:e has index:i
filter active AND primary AND market_cap > 10b
show e.px_last / i.px_last
```

## BQL Advanced Syntax and Analytical Operations

### Analytical Operations & Windowing

These examples test the highlighter's ability to parse mathematical tuples, windowing
keywords (per, group), and chained analytics.

```
// Windowing: filtering against a group aggregate
load "SPX Index"->members
set sector_avg_pe_ratio = avg(pe_ratio) per bics_sector_name
filter pe_ratio > sector_avg_pe_ratio
get name, pe_ratio, sector_avg_pe_ratio

// Tuple algebra with pairwise correlation
retrieve 'SPX Index'=>members:e1,e2
let c = correlation(e1.px_last, e2.px_last)
let z = zscore(e1.px_last - e2.px_last)
filter (e1.id <> e2.id) AND (e1.sector == e2.sector) AND c > 0.8
top 3 by z desc
show e1.name, e2.name, c, z
```

### Edge Cases and Advanced Syntax

These snippets include attribute ambiguity resolution, temporal modifiers, array
constructions, and control flow testing.

```
// Edge Case: Attribute ambiguity resolution using explicit path variables
load @'SPX Index'=>members(weights == 4 && position > p.attr1)=>parent:p=>bond
|> name

// Edge Case: Temporal modifiers and chained methods
'AAPL US Equity'.px_last(dates=range(-1y, 0d)).dropna().pct_chg().std()*sqrt(252)

// Edge Case: Control flow, variable prefixing (#), and type coercion
let(#bool=px_last(dates=range(-3d,0d))<250 ;)
get(
  if(#bool, 1, '20') + 1 as #a,
  if(#bool , '3', 20) + 1 as #b
)
for('IBM US Equity')

// Edge Case: Text joining with inline arrays and string literals
get(
  textjoin(
    [
      'Financial Snapshot: Trading in (',
      crncy,
      '), Reporting in (',
      eqy_fund_crncy,
      ')'
    ],
    ""
  )
)
for(['CSU CN Equity'])
```
