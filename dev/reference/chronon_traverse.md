# Traverse the graph of time unit relationships

Walks out from a chronon through the time units it is related to by
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
methods, either to coarser or finer time units, accumulating a value
along the way. This is a low-level building block for questions about
how time units relate, such as "which coarser time units does a month
nest within a fixed number of times?" or "which finer time units divide
a day between 3 and 15 times?".

## Usage

``` r
chronon_traverse(
  x,
  .f,
  .init,
  .until = function(acc, granule) FALSE,
  direction = c("coarser", "finer"),
  graph = c("fixed", "cardinality")
)
```

## Arguments

- x:

  A time granule to start from (e.g. `cal_gregorian$month(1L)`).

- .f:

  A function `function(acc, from, to)` returning the accumulated value
  at time granule `to`, given the accumulated value `acc` at the
  adjacent time granule `from` it was reached from. `from` is `x` for
  the time units adjacent to `x`.

- .init:

  The accumulated value at `x`.

- .until:

  A function `function(acc, granule)` that returns `TRUE` to stop the
  traversal beyond `granule`. The granule is still included in the
  result, but time units only reachable through it are not visited.

- direction:

  Walk to `"coarser"` time units (e.g. from months to years) or
  `"finer"` time units (e.g. from months to days).

- graph:

  Which relationships to walk along: `"fixed"` uses only the constant
  relationships of
  [`chronon_cardinality_fixed()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality_fixed.md)
  methods (e.g. 12 months in a year), while `"cardinality"` also
  includes context-dependent ones from
  [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
  methods (e.g. days in a month).

## Value

A list with one element for each time unit reached (excluding `x`'s own
time unit), named by its class and in breadth-first order. Each element
is a list of `granule`, a single unit (`n = 1L`) granule of that time
unit which inherits the calendar and properties (such as time zone) of
`x`, and `value`, the accumulated value. A time unit with no registered
relationships gives an empty list.

## Details

The traversal is breadth-first, and when a time unit can be reached
along several paths only the first (shortest) is followed. The
accumulated value is a fold along that path, which suits values that
compose along a path, such as fixed cardinalities, the path itself, or
its length.

Context-dependent cardinalities don't compose along a path: the 29 days
of February 2024 times the 3 months of a quarter is not the 91 days of
the first quarter of 2024. Compute these from `x` itself, for example
with `function(acc, from, to) chronon_cardinality(x, to, at = at)`.

## Examples

``` r
# Cumulative fixed cardinality of each coarser time unit
card_up <- function(acc, from, to) acc * chronon_cardinality_fixed(from, to)

# There are 3 months in a quarter, and 12 in a year
traversal <- chronon_traverse(cal_gregorian$month(1L), card_up, 1)
vapply(traversal, function(e) e$value, numeric(1L))
#>    mixtime::tu_year mixtime::tu_quarter 
#>                  12                   3 

# Coarser time units containing at most 15 hours
traversal <- chronon_traverse(
  cal_gregorian$hour(1L), card_up, 1,
  .until = function(acc, granule) acc >= 15
)
vapply(traversal, function(e) e$value, numeric(1L))
#> mixtime::tu_ampm  mixtime::tu_day 
#>               12               24 

# Finer time units that divide a day up to 15 times
card_down <- function(acc, from, to) acc * chronon_cardinality_fixed(to, from)
traversal <- chronon_traverse(
  cal_gregorian$day(1L), card_down, 1,
  .until = function(acc, granule) acc > 15,
  direction = "finer"
)
vapply(traversal, function(e) e$value, numeric(1L))
#> mixtime::tu_hour mixtime::tu_ampm 
#>               24                2 

# The path from minutes to each coarser time unit
traversal <- chronon_traverse(
  cal_gregorian$minute(1L),
  function(acc, from, to) c(acc, class(to)[[1L]]),
  .init = "mixtime::tu_minute"
)
vapply(traversal, function(e) paste(e$value, collapse = " > "), character(1L))
#>                                                             mixtime::tu_hour 
#>                                      "mixtime::tu_minute > mixtime::tu_hour" 
#>                                                             mixtime::tu_ampm 
#>                   "mixtime::tu_minute > mixtime::tu_hour > mixtime::tu_ampm" 
#>                                                              mixtime::tu_day 
#>                    "mixtime::tu_minute > mixtime::tu_hour > mixtime::tu_day" 
#>                                                             mixtime::tu_week 
#> "mixtime::tu_minute > mixtime::tu_hour > mixtime::tu_day > mixtime::tu_week" 
```
