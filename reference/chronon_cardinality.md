# Cardinality between time units

This S7 generic function defines the calendrical relationships between
two chronons, and is one of the building block for defining calendars in
mixtime. It calculates how many `x` chronons fit into the `y` chronon.
Some chronon sizes are context-dependent (such as the number of days in
a month), and so an optional time point defined in terms of `y` chronons
can be provided with `at`.

## Usage

``` r
chronon_cardinality(x, y, ...)

chronon_cardinality.S7_methods(x, y, at = NULL)
```

## Arguments

- x:

  The finer time unit (e.g. `cal_gregorian$month(1L)`)

- y:

  The coarser time unit (e.g. `cal_gregorian$year(1L)`)

- ...:

  Additional arguments for methods.

- at:

  Optional time point for context-dependent cardinality, defined in
  terms of `y` (e.g., if `y` is `month()`, then `at` could be a
  [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/reference/linear_time_helpers.md))

## Value

Numeric describing how many `x` time units fit into `y` at time `at`.

## Details

The methods are dispatched based on the shortest path along defined
methods. This allows for defining only the direct relationships between
adjacent time units, and relying on graph traversal to find how to
convert between more distant units. For example the number of seconds in
an hour can be calculated from the number of seconds in a minute and
then number of minutes in an hour.

If a method is defined for converting between time units of different
calendar systems (e.g., Gregorian calendar days to Chinese calendar
days), then that method can be used to convert times at any granularity
between the two systems.

## Examples

``` r
# There are 12 months in a year
with(cal_gregorian, chronon_cardinality(month(1L), year(1L)))
#> [1] 12

# There are 7 days in a week
with(cal_isoweek, chronon_cardinality(day(1L), week(1L)))
#> [1] 7

# There are 3600 seconds in an hour
with(cal_gregorian, chronon_cardinality(second(1L), hour(1L)))
#> [1] 3600

# There are 18 "2 months" in 3 years
with(cal_gregorian, chronon_cardinality(month(2L), year(3L)))
#> [1] 18

# There are 365 days in 2025 (a common year)
with(cal_gregorian, chronon_cardinality(day(1L), year(1L), at = year(as.Date("2025-01-01"))))
#> [1] 365

# There are 366 days in 2024 (a leap year)
with(cal_gregorian, chronon_cardinality(day(1L), year(1L), at = year(as.Date("2024-01-01"))))
#> [1] 365

# There are 29 days in February 2024 (a leap year)
with(cal_gregorian, chronon_cardinality(day(1L), month(1L), at = yearmonth(as.Date("2024-02-01"))))
#> [1] 29
```
