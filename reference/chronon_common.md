# Find the common chronon of a time object

This utility function takes a set of chronons and identifies a common
chronon of the finest granularity that can represent all input chronons
without loss of information. This is useful for operations that require
a shared time granule, such as combining or comparing different time
measured at different precisions.

The result is obtained by finding the greatest lower bound (GLB) of the
input chronons using the ordered relationships defined by
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md)
methods. The GLB represents the finest chronon that can represent all
input chronons without loss of information.

## Usage

``` r
chronon_common(x, ...)

chronon_common.mixtime(x, .ptype = NULL, ...)
```

## Arguments

- x:

  A time object (typically a
  [`mixtime`](https://pkg.mitchelloharawild.com/mixtime/reference/mixtime.md)).

- ...:

  Additional arguments for methods.

- .ptype:

  If NULL, the default, the output returns the common chronon across all
  chronons of `x`. Alternatively, a prototype chronon can be supplied to
  `.ptype` to demand a specific chronon is used. If the supplied
  `.ptype` cannot represent all input chronons without loss of
  information, an error is raised.

## Value

A time granule object representing the common chronon.

## Examples

``` r
# The common chronon between a year-month and a day is a day
chronon_common(c(yearmonth(Sys.Date()), date(Sys.Date())))
#> <mixtime::tu_day>
#>  @ n : int 1
#>  @ tz: 'mt_naive' chr NA

# The common chronon between a Gregorian month and an ISO week is a day
chronon_common(c(yearmonth(Sys.Date()), yearweek(Sys.Date())))
#> <mixtime::tu_day>
#>  @ n : int 1
#>  @ tz: 'mt_naive' chr NA

# The common chronon between a ISO week and an hour is an hour
chronon_common(c(yearweek(Sys.Date()), linear_time(Sys.time(), hour(1L))))
#> <mixtime::tu_hour>
#>  @ n : int 1
#>  @ tz: 'mt_naive' chr NA
```
