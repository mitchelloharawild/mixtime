# Test whether time is determinate at a granule's precision

`time_is_determinate_at()` tests, for each element of a `mixtime`
vector, whether the time point is well-defined at the precision of
`granule`.

## Usage

``` r
time_is_determinate_at(x, granule, ...)
```

## Arguments

- x:

  A time object (typically a `mixtime` vector).

- granule:

  The time granule whose precision to test, given as a granule generator
  (e.g. `cal_gregorian$month`) or a sized time unit (e.g.
  `cal_gregorian$month(1L)`).

- ...:

  Additional arguments for methods.

## Value

A logical vector the same length as `x`.

## Details

Discrete (integer) time cannot resolve a granule finer than its own
chronon (a
[`year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)
has no determinate month), so those elements are `FALSE`. Continuous
(fractional) time tracks progress within its chronon and so resolves
finer granules exactly (0% through 2020 is 0% through January), giving
`TRUE`. Coarser-or-equal granules are always determinate. Missing (`NA`)
and infinite times give `NA`.

## Examples

``` r
# Discrete: a year has no determinate month
time_is_determinate_at(year(2020L), cal_gregorian$month(1L))
#> [1] FALSE

# Continuous: 0% through 2020 is 0% through January
time_is_determinate_at(year(2020), cal_gregorian$month(1L))
#> [1] TRUE

# A coarser granule is always determinate
time_is_determinate_at(yearmonth(as.Date("2020-02-01")), cal_gregorian$year(1L))
#> [1] TRUE
```
