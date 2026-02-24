# Find a common chronon from a set of chronons

This utility function takes a set of chronons and identifies a common
chronon of the finest granularity that can represent all input chronons
without loss of information. This is useful for operations that require
a shared time unit, such as combining or comparing different time
measured at different precisions.

## Usage

``` r
chronon_common(..., .ptype = NULL)
```

## Arguments

- ...:

  A set of chronons to find a common chronon for.

- .ptype:

  If NULL, the default, the output returns the common chronon across all
  elements of `...`. Alternatively, a prototype chronon can be supplied
  to `.ptype` to demand a specific chronon is used. If the supplied
  `.ptype` cannot represent all input chronons without loss of
  information, an error is raised.

## Value

A time unit object representing the common chronon.

## Examples

``` r
# The common chronon between days and weeks is a day
with(cal_isoweek, chronon_common(day(1L), week(1L)))
#> <mixtime::tu_day> int 1
#>  @ tz: chr "UTC"

# The common chronon between days and months is a day
chronon_common(cal_isoweek$week(1L), cal_gregorian$month(1L))
#> <mixtime::tu_day> int 1
#>  @ tz: chr "UTC"

# The common chronon between hours, months, and years is an hour
with(cal_gregorian, chronon_common(hour(1L), month(1L), year(1L)))
#> <mixtime::tu_hour> int 1
#>  @ tz: chr "UTC"

# The common chronon between months, quarters, and years is a month
with(cal_gregorian, chronon_common(month(1L), quarter(1L), year(1L)))
#> <mixtime::tu_month> int 1
#>  @ tz: chr "UTC"
```
