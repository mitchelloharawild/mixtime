# Convert between chronons of different time granules

This function converts between chronons measured in different time
granules. It is used internally for converting between different
continuous time types, and is particularly useful for efficiently
converting between irregular time granules. The default method uses
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
to cast between time granules, which is efficient for regular time
granules.

## Usage

``` r
chronon_divmod(from, to, ...)
```

## Arguments

- from:

  The time granule that `x` is measured in (e.g., `day(1L)`).

- to:

  The time granule to convert `x` into (e.g., `week(1L)`).

- ...:

  Additional arguments for methods.

## Value

An list of two elements:

- `div`: integer vector of chronons measured in the `to` time granule.

- `mod`: integer vector of the remainder (in `from` time granule) after
  converting to the `to` time granule.

## Examples

``` r
# Convert day 16 after epoch (1970-01-01) into weeks since epoch (and remainder days)
with(cal_isoweek, chronon_divmod(day(1L), week(1L), 16L))
#> $div
#> [1] 2
#> 
#> $mod
#> [1] 5
#> 

# Convert week 4 after epoch (1970-W1) into days since epoch
with(cal_isoweek, chronon_divmod(week(1L), day(1L), 4L))
#> $div
#> [1] 25
#> 
#> $mod
#> [1] 0
#> 
```
