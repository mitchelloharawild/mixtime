# Arithmetic for underspecified cyclical time (`mt_cyclical_free`)

Underspecified cyclical time (see
[`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md))
is a position within a cycle whose enclosing cycle is unknown, e.g.
`month_of_year(3)` is "March of some year". Only operations that don't
depend on the enclosing cycle are defined:

- **Differences**: `a - b` between values of the same cycle is a
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md)
  in their common chronon, the difference between their positions within
  the cycle (`month_of_year(3) - month_of_year(1)` is 2 months, and
  `month_of_year(1) - month_of_year(3)` is -2 months). This follows the
  linear ordering of positions within one cycle (see
  [mt_cyclical_free-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical_free-compare.md)),
  for cycles of fixed and variable length alike
  (`day_of_month(31) - day_of_month(1)` is 30 days).

- **Shifts**: `x + n` and `x - n` shift by `n` chronons, where `n` is a
  number or a
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md)
  that is a whole number of the value's chronons with a fixed ratio
  (e.g. `month_of_year(1) + months(2)` or
  `time_of_day(3600) + hours(2)`). Shifts wrap around cycles of a fixed
  length (`month_of_year(12) + 1` is January, `day_of_week(1) - 1` is
  Sunday), so `a + (b - a) == b`. Cycles of variable length (e.g.
  [`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`day_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md)
  and
  [`week_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md))
  can't be shifted, whatever the values: `day_of_month(28) + 1` is day
  29, or day 1 in a non-leap February.

- [`seq()`](https://rdrr.io/r/base/seq.html) follows the same rules as
  shifts. The sequence runs linearly within one cycle between `from` and
  `to` (so `seq(month_of_year(11), month_of_year(2))` counts down from
  November to February), while sequences of a given `length.out` wrap
  around the cycle.

Everything else is an error: shifting by a duration that isn't a whole
number of chronons with a fixed ratio (`month_of_year(1) + days(1)`), or
that spans a whole cycle or more (`month_of_year(1) + years(1)` would
move to another, unknown, year); differences between values of differing
cycles, or between an underspecified value and a linear or located time
point; subtracting an underspecified value from a number or duration;
multiplying or dividing; and summaries that create new values, such as
[`mean()`](https://rdrr.io/r/base/mean.html) or
[`cumsum()`](https://rdrr.io/r/base/cumsum.html). Comparisons and
ordering ([`sort()`](https://rdrr.io/r/base/sort.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`range()`](https://rdrr.io/r/base/range.html)) are defined, see
[mt_cyclical_free-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical_free-compare.md).

To do any of these, locate the value in a specific enclosing cycle (e.g.
a particular month for a day of month) with
[`time_compose()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_compose.md),
giving linear time, e.g.
`time_compose(yearmonth("2024-02"), day_of_month(28)) + days(1)`.

Missing values stay missing.

## Usage

``` r
# S4 method for class 'mt_cyclical_free'
e1 + e2
# S4 method for class 'mt_cyclical_free'
e1 - e2
```

## Arguments

- e1, e2:

  Operands, at least one an underspecified (`mt_cyclical_free`) vector.

## Value

For differences, a duration
([mt_duration](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md)).
For shifts, a `mt_cyclical_free` vector of the same type as the
underspecified operand.

## Examples

``` r
month_of_year(12) + 1L
#> <mixtime[1]>
#> [1] Jan
month_of_year(1) - months(1)
#> <mixtime[1]>
#> [1] Dec
day_of_week(6) + 0:3
#> <mixtime[4]>
#> [1] Sat Sun Mon Tue
month_of_year(3) - month_of_year(1)
#> <mixtime[1]>
#> [1] 2 months
day_of_month(31) - day_of_month(1)
#> <mixtime[1]>
#> [1] 30 days
seq(day_of_week(6), by = 1, length.out = 4)
#> <mixtime[4]>
#> [1] Sat Sun Mon Tue

# Cycles of variable length can't be shifted until located
try(day_of_month(28) + 1L)
#> Error : Can't add 1 day to a day of month in an unknown month.
#> ✖ The number of days in a month varies, so where a day of month lands after
#>   adding 1 day depends on which month it is in.
#> ℹ Locate it in a specific month first, e.g. `time_compose(yearmonth("2024-02"),
#>   x) + days(1)`.
time_compose(yearmonth("2024-02"), day_of_month(28)) + days(1)
#> <mixtime[1]>
#> [1] 2024-02-29
```
