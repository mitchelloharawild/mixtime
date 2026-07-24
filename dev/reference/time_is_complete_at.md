# Test whether a granule is completed at a time point

`time_is_complete_at()` tests, for each element of a `mixtime` vector,
whether the coarser `granule` that element falls into is fully observed
*by the vector as a whole* – that is, whether every finer chronon making
up that granule is present somewhere in `x`.

## Usage

``` r
time_is_complete_at(x, granule, ...)
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

Unlike
[`time_is_determinate_at()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_is_determinate_at.md),
completeness is a collective property: an element is `TRUE` only when
the other elements needed to fill its granule are also present. For
example, in `year(1L)` the months of `2020 Jan : 2020 Oct` are all
`FALSE` (November and December are missing, so 2020 is incomplete),
whereas in `2020 Jan : 2021 Mar` the twelve months of 2020 are `TRUE`
(they complete 2020) while the three months of 2021 remain `FALSE`.

A granule equal to `x`'s own chronon is completed by each point on its
own (`TRUE`). A granule finer than `x` cannot be completed by coarser
points (`FALSE`). Missing (`NA`) and infinite times give `NA`.

Completeness is only defined within a single time granularity.
Mixed-type `mixtime` vectors (e.g. months alongside days) are not yet
supported and raise an error.

## See also

[`time_is_determinate_at()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_is_determinate_at.md)

## Examples

``` r
# 2020 Jan : 2020 Oct does not complete the year -> all FALSE
time_is_complete_at(yearmonth(as.Date("2020-01-01")) + 0:9, cal_gregorian$year(1L))
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# 2020 Jan : 2021 Mar completes 2020 (TRUE) but not 2021 (FALSE)
time_is_complete_at(yearmonth(as.Date("2020-01-01")) + 0:14, cal_gregorian$year(1L))
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#> [13] FALSE FALSE FALSE
```
