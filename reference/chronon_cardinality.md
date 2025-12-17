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

  The primary time unit

- y:

  The time unit to convert `x` into

- ...:

  Additional arguments for methods.

- at:

  Optional time point for context-dependent cardinality, defined in
  terms of `y` (e.g., if `y` is [`tu_month()`](calendar_gregorian.md),
  then `at` could be a [`yearmonth()`](linear_gregorian.md))

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
chronon_cardinality(tu_year(1L), tu_month(1L))
#> [1] 12

# There are 7 days in a week
chronon_cardinality(tu_week(1L), tu_day(1L))
#> [1] 7

# There are 3600 seconds in an hour
chronon_cardinality(tu_hour(1L), tu_second(1L))
#> [1] 3600

# There are 18 "2 months" in 3 years
chronon_cardinality(tu_year(3L), tu_month(2L))
#> [1] 18

# There are 365 days in 2025 (a common year)
chronon_cardinality(tu_year(1L), tu_day(1L), at = year(as.Date("2025-01-01")))
#> [1] 365

# There are 366 days in 2024 (a leap year)
chronon_cardinality(tu_year(1L), tu_day(1L), at = year(as.Date("2024-01-01")))
#> [1] 366

# There are 29 days in February 2024 (a leap year)
chronon_cardinality(tu_month(1L), tu_day(1L), at = yearmonth(as.Date("2024-02-01")))
#> [1] 29
```
