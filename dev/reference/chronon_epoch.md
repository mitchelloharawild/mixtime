# Epoch offset for chronons

Returns the epoch offset for a given chronon (time unit). The epoch
defines the starting point of the chronon's linear numbering, used when
converting from internal representations to common displays (e.g.
applying an epoch of 1970-01-01).

## Usage

``` r
chronon_epoch(x, ...)
```

## Arguments

- x:

  A chronon (time unit) object.

- ...:

  Additional arguments for methods.

## Value

A numeric value representing the epoch for the chronon.

## Examples

``` r
# The epoch for year linear time displays is 1970
chronon_epoch(cal_gregorian$year(1L))
#> [1] 1970
```
