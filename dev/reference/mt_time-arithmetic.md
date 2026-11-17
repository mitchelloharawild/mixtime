# Arithmetic for time points (`mt_time`)

Time points, linear (e.g.
[`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md))
or located cyclical (e.g. `month_of_year(Sys.Date())`), support:

- **Shifts**: `x + d`, `x - d` and `d + x` shift by a
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md).
  Time points that don't exist (e.g. 31 January plus one month) are
  resolved with the `mixtime.on_invalid` option; see
  [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_shift.md).

- **Shifts by chronons**: `x + n` and `x - n` shift by `n` of the time
  points' own chronon.

- **Differences**: `a - b` is a
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md)
  in their finest common chronon.

Located cyclical time shifts its underlying time, and its position
within the cycle follows. For underspecified cyclical time, see
[mt_cyclical_free-arithmetic](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical_free-arithmetic.md).

A discrete time point spans its whole chronon, so shifting by a duration
finer than its chronon, or taking differences between discrete time
points of different chronons, is an error. Any other combination, such
as adding two time points or subtracting a time point from a duration,
is also an error.

## Usage

``` r
# S4 method for class 'mt_time'
e1 + e2
# S4 method for class 'mt_time'
e1 - e2
```

## Arguments

- e1, e2:

  Operands, at least one a time point (`mt_time`) vector.

## Value

A time point vector for shifts, a duration
([mt_duration](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md))
for differences.

## Examples

``` r
x <- yearmonth("2024-01")
x + months(2)
#> <mixtime[1]>
#> [1] 2024 Mar
x + 0:3
#> <mixtime[4]>
#> [1] 2024 Jan 2024 Feb 2024 Mar 2024 Apr
yearmonth("2024-05") - x
#> <mixtime[1]>
#> [1] 4.0 months
date("2024-01-31") + months(1)
#> <mixtime[1]>
#> [1] 2024-02-29
month_of_year(yearmonth("2024-11")) + 3L
#> <mixtime[1]>
#> [1] Feb
try(date("2024-03-15") + hours(3))
#> Error : Cannot shift 1 day time points by 3 hours, the result is temporally
#> indeterminate.
#> ✖ A discrete 1 day spans a period of time that doesn't shift as a whole by 3
#>   hours.
#> ℹ Shift by a duration that 1 day nests within or that is a multiple of 1 day,
#>   or use continuous time.
```
