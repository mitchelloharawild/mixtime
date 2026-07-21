# Fixed cardinality between time granules

A restricted variant of
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
for time granule pairs whose relationship is a constant,
context-independent number (e.g., 60 seconds in a minute, 24 hours in a
day). Unlike
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md),
methods for this generic do not receive (and must not need) an `at` time
point, and should return the number of unit (`n = 1L`) `x` granules that
fit within one unit `y` granule.

## Usage

``` r
chronon_cardinality_fixed(x, y, ...)
```

## Arguments

- x:

  The finer time granule (e.g. `cal_gregorian$month(1L)`)

- y:

  The coarser time granule (e.g. `cal_gregorian$year(1L)`)

- ...:

  Additional arguments for methods.

## Value

A single number describing how many unit `x` time granules fit into a
unit `y` time granule.

## Details

Defining a method for `chronon_cardinality_fixed()` automatically
provides a
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
method for the pair (scaled by the requested granule sizes via the
`list(mt_unit, mt_unit)` fallback method), and marks the relationship as
safe to use for
[`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_divmod.md)'s
graph traversal, where variable (context-dependent) cardinalities cannot
be used since no `at` is available mid-traversal.
