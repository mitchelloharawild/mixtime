# Tolerant comparison of mixtime time values

[vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html)'s
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) method (which
`mixtime` inherits, since a mixtime vector is a `vecvec`) handles
length/`NA` mismatches and groups elements by their underlying storage
slot, then compares each group's *raw* values with
[`all.equal()`](https://rdrr.io/r/base/all.equal.html). That raw
comparison has no notion of chronon: two elements that are `==` (e.g.
`days(1)` and `hours(24)`, or a `yearmonth` and an equivalent-instant
`yearweek`) can end up stored with different chronons or magnitudes, and
so are wrongly reported as unequal.

This method fixes that at the per-slot level: elements already `==`
(which is chronon-aware, see
[mt_linear-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_linear-compare.md)/[mt_duration-compare](https://pkg.mitchelloharawild.com/mixtime/reference/mt_duration-compare.md))
count as equal outright; for the rest, the discrepancy is measured as
the duration between them in their common chronon. A time point has no
"typical magnitude" to scale a *relative* tolerance against the way a
plain number does, so - unlike
[`base::all.equal.numeric()`](https://rdrr.io/r/base/all.equal.html) -
`tolerance` is always absolute, in (possibly fractional) chronon units.

## Usage

``` r
# S3 method for class '`mixtime::mt_time`'
all.equal(target, current, tolerance = sqrt(.Machine$double.eps), ...)
```

## Arguments

- target, current:

  `mt_time` vectors of the same length to compare (as passed down by
  [vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html)'s
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html)).

- tolerance:

  Numeric tolerance, as an absolute number of `target`'s chronon units.
  Defaults to `sqrt(.Machine$double.eps)`, as for
  [`base::all.equal()`](https://rdrr.io/r/base/all.equal.html).

- ...:

  Ignored.

## Value

`TRUE` if `target` and `current` are equal within `tolerance`, otherwise
a string describing the discrepancy.

## Examples

``` r
all.equal(yearmonth(0), yearmonth(0))
#> [1] TRUE
all.equal(yearmonth(0), yearmonth(1))
#> [1] "Mean absolute difference: 1"
all.equal(days(1), hours(24))
#> [1] TRUE
```
