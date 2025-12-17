# Time units as a string

These S7 generic functions provide the full and abbreviated names for
time units. This is used internally for the display of continuous time,
cyclical time, and durations.

## Usage

``` r
time_unit_full(x, ...)

time_unit_abbr(x, ...)
```

## Arguments

- x:

  A time unit object (e.g., `tu_month(1L)`)

- ...:

  Additional arguments for methods.

## Value

A string representing the time unit

## Examples

``` r
time_unit_full(tu_year(1L))
#> [1] "year"
time_unit_abbr(tu_year(1L))
#> [1] "Y"
```
