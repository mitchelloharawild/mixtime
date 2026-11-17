# Cyclical time points

`cyclical_time()` creates a vector of cyclical time points representing
positions within repeating cycles. This function is useful for creating
custom cyclical time representations that aren't covered by the
convenience functions like
[`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md)
or
[`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md).

## Usage

``` r
cyclical_time(
  data,
  chronon = time_chronon(data),
  cycle = time_cycle(data),
  discrete = TRUE,
  calendar = time_calendar(data)
)
```

## Arguments

- data:

  Input data to convert to cyclical time. Can be:

  - Numeric values: underspecified cyclical time (positions with no
    enclosing cycle), counted the way the chronon is displayed (e.g. `3`
    is March for months of the year, `0` is midnight for seconds of the
    day). Positions beyond the longest possible cycle (e.g. day 32 of a
    month) are `NA`.

  - Character strings (parsed as dates/times)

  - Date or POSIXct objects

  - Other time objects

- chronon:

  A time granule representing the chronon (finest indivisible time
  granule), evaluated in the context of `calendar`. Use unquoted
  expressions like `day(1L)` or `month(1L)`. Chronons from a specific
  calendar can also be used (e.g. `cal_isoweek$day(1L)`).

- cycle:

  A time granule representing the cycle (coarser time granule that
  defines the period), evaluated in the context of `calendar`. Use
  unquoted expressions like `week(1L)` or `year(1L)`.

- discrete:

  Logical. If `TRUE` (default), returns integer positions within the
  cycle (discrete time model). If `FALSE`, returns fractional positions
  allowing representation of fractional time chronons (continuous time
  model).

- calendar:

  Calendar system used to evaluate `chronon` and `cycle`. Defaults to
  `time_calendar(data)` for existing time objects. Common options
  include
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_gregorian.md)
  and
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_isoweek.md).

## Value

A `mixtime` time vector of located cyclical time (`mt_cyclical`), or
underspecified cyclical time (`mt_cyclical_free`) for numeric `data`.

## Located and underspecified cyclical time

A cyclical time point is a position within a cycle, such as March within
a year. The cycle it falls in (which year) is its *enclosing cycle*, and
whether that is known gives two types of cyclical time:

- **Located** cyclical time knows its enclosing cycle. It is created
  from time points: `month_of_year(as.Date("2024-03-15"))` is March
  2024, displayed as `Mar`.

- **Underspecified** cyclical time doesn't. It is created from
  positions: `month_of_year(3)` is March of any year.

Both compare and sort by their position within the cycle, so
`month_of_year(3) == month_of_year(as.Date("2024-03-15"))` is `TRUE`.

They differ in operations that move a position. Located cyclical time
moves its underlying time point, so the result accounts for the
enclosing cycle: adding a day to
`day_of_month(as.Date(c("2023-02-28", "2024-02-28")))` gives `D01 D29`.

Underspecified cyclical time only supports operations that don't need
its enclosing cycle, and which those are depends on the cycle's length:

- **Fixed-length cycles** (e.g. months of the year, days of the week)
  wrap around: `month_of_year(12) + 1` is `Jan`.

- **Variable-length cycles** (e.g. days of the month or year) can't be
  moved: `day_of_month(28) + 1` is day 29, or day 1 of a non-leap
  February, so it is an error. Positions are only checked against the
  longest cycle, so `day_of_month(31)` is valid but `day_of_month(32)`
  is `NA`.

Locate underspecified cyclical time in a specific enclosing cycle with
[`time_compose()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_compose.md):
adding a day to `time_compose(yearmonth("2024-02"), day_of_month(28))`
gives 2024-02-29. The two types can't be cast into each other. See
[mt_cyclical_free-arithmetic](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical_free-arithmetic.md)
and
[mt_cyclical_free-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_cyclical_free-compare.md)
for details.

## See also

- [`new_cyclical_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_cyclical_time_fn.md)
  for creating reusable cyclical time functions

- [`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`day_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md)
  for common cyclical representations

- [`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`week_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md)
  for other cyclical time helpers

- [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_gregorian.md),
  [cal_isoweek](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_isoweek.md)
  for calendar systems

## Examples

``` r
# Day of week (1-7, Monday = 1)
cyclical_time(
  Sys.Date(),
  chronon = day(1L),
  cycle = week(1L),
  calendar = cal_isoweek
)
#> <mixtime[1]>
#> [1] Tue

# Month of year (1-12)
cyclical_time(
  Sys.Date(),
  chronon = month(1L),
  cycle = year(1L)
)
#> <mixtime[1]>
#> [1] Nov

# Continuous time (discrete = FALSE) for fractional month of year
cyclical_time(
  Sys.Date(),
  chronon = month(1L),
  cycle = year(1L),
  discrete = FALSE
)
#> <mixtime[1]>
#> [1] Nov 53.3%

# Day of month with Gregorian calendar
cyclical_time(
  Sys.Date(),
  chronon = day(1L),
  cycle = month(1L),
  calendar = cal_gregorian
)
#> <mixtime[1]>
#> [1] D17

# Hours, minutes, and seconds
cyclical_time(
  Sys.time(),
  chronon = second(1L),
  cycle = day(1L)
)
#> <mixtime[1]>
#> [1] 08:53:11
#' # Underspecified cyclical time from positions
month_of_year(c(3, 12)) + 1
#> <mixtime[2]>
#> [1] Apr Jan
try(day_of_month(28) + 1)
#> Error : Can't add 1 day to a day of month in an unknown month.
#> ✖ The number of days in a month varies, so where a day of month lands after
#>   adding 1 day depends on which month it is in.
#> ℹ Locate it in a specific month first, e.g. `time_compose(yearmonth("2024-02"),
#>   x) + days(1)`.
time_compose(yearmonth("2024-02"), day_of_month(28)) + days(1)
#> <mixtime[1]>
#> [1] 2024-02-29

# Located cyclical time moves with its enclosing cycle
day_of_month(as.Date(c("2023-02-28", "2024-02-28"))) + days(1)
#> <mixtime[2]>
#> [1] D01 D29
```
