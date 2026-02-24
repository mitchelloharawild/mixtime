# Time units as a string

These S7 generic functions provide the full and abbreviated names for
time units. `time_unit_full()` is used in messages and durations (e.g.,
"2 months"). `time_unit_abbr()` is used for tsibble index interval
displays (e.g., "1M").

## Usage

``` r
time_unit_full(x, ...)

time_unit_abbr(x, ...)
```

## Arguments

- x:

  A time unit object (e.g., `cal_gregorian$month(1L)`)

- ...:

  Additional arguments for methods.

## Value

A string representing the time unit

## Examples

``` r
time_unit_full(cal_gregorian$year(1L))
#> [1] "year"
time_unit_abbr(cal_gregorian$year(1L))
#> [1] "Y"
```
