# Shift time points by a duration

Shifts time points by a duration, like `x + by`, with control over how
time points that don't exist in the target granule are resolved.
Shifting by a duration with an irregular relationship to the time
points' chronon (e.g. days within months) can produce time points that
don't exist, such as shifting 31 January by one month. In a time zone,
shifting by days or longer keeps the local clock time, which may have
been skipped when daylight saving starts (e.g. 02:30 on the day clocks
go forward from 02:00 to 03:00). The `+` and `-` operators use the
default strategy, set by the `mixtime.on_invalid` option (`"clamp"`
unless set).

## Usage

``` r
time_shift(x, by, on_invalid = getOption("mixtime.on_invalid", "clamp"))
```

## Arguments

- x:

  A time point vector.

- by:

  A time
  [`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md)
  (e.g., `months(1L)`), or a character string specifying one (e.g.,
  `"1 month"`).

- on_invalid:

  How to resolve time points that don't exist in the target granule.
  Defaults to the `mixtime.on_invalid` option, or `"clamp"` if it isn't
  set:

  - `"clamp"`: Reduce the invalid component to its last valid value,
    preserving finer components (e.g., the time of day). A skipped local
    time moves back by the size of the gap (02:30 becomes 01:30).

  - `"previous"`: Use the last valid time point before the invalid one,
    the end of the target granule, so finer components are not
    preserved. For discrete time this is the last chronon within the
    granule (e.g., 23:59:59 on the last day); for continuous time it is
    the end of that chronon, the same instant as `"next"`. A skipped
    local time becomes the end of the valid time before the gap
    (01:59:59).

  - `"next"`: Use the first valid time point after the invalid one, the
    start of the following granule, so finer components are not
    preserved (e.g., 00:00:00 on the first day of the following month).
    A skipped local time becomes the start of the valid time after the
    gap (03:00).

  - `"overflow"`: Allow the invalid component to overflow into the next
    granule. A skipped local time moves forward by the size of the gap
    (02:30 becomes 03:30).

  - `"reject"`: Return `NA` with a warning.

  - `"error"`: Throw an error.

  `"previous"` and `"next"` preserve the order of the time points being
  shifted, although distinct time points may become equal. `"clamp"` and
  `"overflow"` do not: shifting `1955-01-29 23:00:00` and
  `1955-01-31 08:00:00` by one month clamps them to
  `1955-02-28 23:00:00` and `1955-02-28 08:00:00`, reversing their
  order.

## Value

A time point vector of the same type as `x`.

## Examples

``` r
x <- datetime("2021-01-30 09:30:00")
time_shift(x, months(1L))
#> <mixtime[1]>
#> [1] 2021-02-28 09:30:00
time_shift(x, months(1L), on_invalid = "previous")
#> <mixtime[1]>
#> [1] 2021-02-28 23:59:59
time_shift(x, months(1L), on_invalid = "next")
#> <mixtime[1]>
#> [1] 2021-03-01 00:00:00
time_shift(x, months(1L), on_invalid = "overflow")
#> <mixtime[1]>
#> [1] 2021-03-02 09:30:00
time_shift(x, months(1L), on_invalid = "reject")
#> Warning: 1 time point did not exist in the target granule and was replaced with "NA".
#> ℹ Set `on_invalid` to "clamp", "previous", "next", or "overflow" to resolve
#>   them to valid time points instead.
#> <mixtime[1]>
#> [1] NA
try(time_shift(x, months(1L), on_invalid = "error"))
#> Error : 1 time point did not exist in the target granule, at position 1.
#> ℹ Set `on_invalid` to "clamp", "previous", "next", or "overflow" to resolve
#>   them to valid time points instead.

# Shifting backwards
time_shift(date("2021-03-31"), months(-1L), on_invalid = "overflow")
#> <mixtime[1]>
#> [1] 2021-03-03

# 02:30 doesn't exist on 2025-10-05 in Melbourne (clocks go forward at 02:00)
x <- datetime(as.POSIXct("2025-10-04 02:30", tz = "Australia/Melbourne"))
time_shift(x, days(1L))
#> <mixtime[1]>
#> [1] 2025-10-05 01:30:00 AEST
time_shift(x, days(1L), on_invalid = "previous")
#> <mixtime[1]>
#> [1] 2025-10-05 01:59:59 AEST
time_shift(x, days(1L), on_invalid = "next")
#> <mixtime[1]>
#> [1] 2025-10-05 03:00:00 AEDT
time_shift(x, days(1L), on_invalid = "overflow")
#> <mixtime[1]>
#> [1] 2025-10-05 03:30:00 AEDT
```
