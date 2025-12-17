# Convert between chronons of different time units

This function converts between chronons measured in different time
units. It is used internally for converting between different continuous
time types, and is particularly useful for efficiently converting
between irregular time units. The default method uses
[`chronon_cardinality()`](chronon_cardinality.md) to cast between time
units, which is efficient for regular time units.

## Usage

``` r
chronon_divmod(from, to, ...)

chronon_divmod.S7_methods(from, to, x)
```

## Arguments

- from:

  The time unit that `x` is measured in (e.g., `tu_day(1L)`).

- to:

  The time unit to convert `x` into (e.g., `tu_week(1L)`).

- ...:

  Additional arguments for methods.

- x:

  An integer vector of chronons measured in the `from` time unit.

## Value

An list of two elements:

- `chronon`: integer vector of chronons measured in the `to` time unit.

- `remainder`: integer vector of the remainder (in `from` time unit)
  after converting to the `to` time unit.

## Examples

``` r
# Convert day 16 since epoch into weeks since epoch (and remainder days)
chronon_divmod(tu_day(1L), tu_week(1L), 16L)
#> $chronon
#> [1] 2
#> 
#> $remainder
#> [1] 5
#> 

# Convert week 4 since epoch into days since epoch
chronon_divmod(tu_week(1L), tu_day(1L), 4L)
#> $chronon
#> [1] 32
#> 
#> $remainder
#> [1] 0
#> 
```
