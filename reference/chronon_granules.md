# Default linear time granules for chronons

Provides a default set of linear time granules for a given chronon (time
unit). Granules represent the subdivisions or breakpoints used when
representing time values at that chronon level.

## Usage

``` r
chronon_granules(x, ...)
```

## Arguments

- x:

  A chronon (time unit) object.

- ...:

  Additional arguments passed to methods.

## Value

A list of granule values for the chronon. Returns an empty list for the
base `mt_unit` class.

## Examples

``` r
chronon_granules(cal_gregorian$year(1L))
#> list()
chronon_granules(cal_gregorian$month(1L))
#> [[1]]
#> <mixtime::tu_year> int 1
#>  @ tz: chr "UTC"
#> 
```
