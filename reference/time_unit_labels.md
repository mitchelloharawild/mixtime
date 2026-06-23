# Time units as a string

These S7 generic functions provide the full and abbreviated names for
time units. `time_unit_full()` is used in messages and durations (e.g.,
"2 months"). `time_unit_abbr()` is used for tsibble index interval
displays (e.g., "1M"). `time_unit_plural()` pluralises the full unit
name for a given quantity using cli-style pluralisation (e.g.,
`"year{?/s}"` becomes `"year"` or `"years"`).

## Usage

``` r
time_unit_full(x, ...)

time_unit_plural(x, n = 2L)

time_unit_abbr(x, ...)
```

## Arguments

- x:

  A time granule object (e.g., `cal_gregorian$month(1L)`)

- ...:

  Additional arguments for methods.

- n:

  Numeric quantity used to select singular or plural form.

## Value

A string representing the time unit

## Examples

``` r
time_unit_full(cal_gregorian$year(1L))
#> [1] "year{?/s}"
time_unit_plural(cal_gregorian$year(1L), 1L)
#> [1] "year"
time_unit_plural(cal_gregorian$year(1L), 2L)
#> [1] "years"
time_unit_abbr(cal_gregorian$year(1L))
#> [1] "Y"
```
