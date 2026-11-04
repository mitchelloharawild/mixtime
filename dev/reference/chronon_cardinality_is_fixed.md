# Test whether the cardinality between time granules is fixed

Tests whether the number of `x` time granules in a `y` time granule is a
constant (e.g. 12 months in a year, 24 hours in a day), or instead
depends on where in time it is measured (e.g. 28 to 31 days in a month,
52 or 53 weeks in a year). A fixed cardinality can be found with
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
alone, while an irregular one also needs a time point `at`.

## Usage

``` r
chronon_cardinality_is_fixed(x, y)
```

## Arguments

- x:

  The finer time granule (e.g. `cal_gregorian$month(1L)`)

- y:

  The coarser time granule (e.g. `cal_gregorian$year(1L)`)

## Value

A single logical value.

## Details

The cardinality is fixed when the granules are of the same time unit, or
are connected only by
[`chronon_cardinality_fixed()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality_fixed.md)
relationships. Granules with no relationship between them don't have a
fixed cardinality.

## Examples

``` r
# There are always 12 months in a year
with(cal_gregorian, chronon_cardinality_is_fixed(month(1L), year(1L)))
#> [1] TRUE

# The number of days in a month varies
with(cal_gregorian, chronon_cardinality_is_fixed(day(1L), month(1L)))
#> [1] FALSE

# The order of the granules doesn't matter
with(cal_gregorian, chronon_cardinality_is_fixed(day(1L), hour(1L)))
#> [1] TRUE
```
