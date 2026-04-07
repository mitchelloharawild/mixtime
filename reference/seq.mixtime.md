# Generate sequences of mixtime values

Create regular sequences of time values. This method handles both linear
time sequences (dates, date-times) and cyclical time sequences (day of
week, month of year).

## Usage

``` r
# S3 method for class 'mixtime'
seq(...)

# S3 method for class 'mt_time'
seq(
  from,
  to,
  by,
  length.out = NULL,
  along.with = NULL,
  on_invalid = c("nearest", "overflow"),
  ...
)
```

## Arguments

- ...:

  Additional arguments passed to the underlying sequence method.

- from:

  Starting value of the sequence.

- to:

  End value of the sequence (if provided).

- by:

  Increment of the sequence. Can be:

  - An integer for the number of time units

  - A character string specifying the interval (e.g., "1 day", "2
    weeks", "1 month", "1 year")

  - A time unit object created with time unit functions (e.g.,
    `cal_gregorian$year(1L)`, `cal_gregorian$month(1L)`,
    `cal_gregorian$day(1L)`)

- length.out:

  Desired length of the sequence (alternative to `to`).

- along.with:

  Take the length from this argument (alternative to `length.out`).

- on_invalid:

  How to handle time points that overflow the cycle when using a `by`
  argument with different time units than the sequence type. Options
  are:

  - `"nearest"` (default): Adjust overflowing time points to the nearest
    valid time point within the cycle

  - `"overflow"`: Allow time points to overflow into the next cycle

  This is relevant when the starting time point has an offset that
  doesn't exist in all cycles. For example, starting on day 31 with
  `by = "1 month"` will overflow in months with fewer than 31 days
  (e.g., February). With `"nearest"`, these will be adjusted to the last
  day of the month (e.g., Feb 28/29). With `"overflow"`, the extra days
  carry into the next month.

  If not explicitly specified and overflow occurs, a warning is issued
  with the default `"nearest"` behavior applied.

## Value

A mixtime vector containing the sequence.

## Details

For linear time types (Date, POSIXct, yearmonth, etc.), sequences
progress forward or backward in time. For cyclical time types
(month_of_year, day_of_week, etc.), sequences wrap around cyclically.

## Examples

``` r
# Linear time sequences with integer by
seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"))
#> <mixtime[12]>
#>  [1] 2020 Jan 2020 Feb 2020 Mar 2020 Apr 2020 May 2020 Jun 2020 Jul 2020 Aug
#>  [9] 2020 Sep 2020 Oct 2020 Nov 2020 Dec
seq(yearquarter("2020-01-01"), length.out = 5, by = 3)
#> <mixtime[5]>
#> [1] 2020 Q1 2020 Q4 2021 Q3 2022 Q2 2023 Q1

# Linear time sequences with string intervals
seq(date("2020-01-01"), date("2020-12-31"), by = "1 month")
#> <mixtime[12]>
#>  [1] 2020-01-01 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
#>  [7] 2020-07-01 2020-08-01 2020-09-01 2020-10-01 2020-11-01 2020-12-01
seq(yearmonth("2020-01-01"), yearmonth("2025-01-01"), by = "1 year")
#> <mixtime[6]>
#> [1] 2020 Jan 2021 Jan 2022 Jan 2023 Jan 2024 Jan 2025 Jan
seq(date("2020-01-01"), length.out = 10, by = "2 weeks")
#> <mixtime[10]>
#>  [1] 2020-01-01 2020-01-15 2020-01-29 2020-02-12 2020-02-26 2020-03-11
#>  [7] 2020-03-25 2020-04-08 2020-04-22 2020-05-06

# Linear time sequences with time units
seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"), by = cal_gregorian$month(2L))
#> <mixtime[6]>
#> [1] 2020 Jan 2020 Mar 2020 May 2020 Jul 2020 Sep 2020 Nov
seq(date("2020-01-01"), length.out = 5, by = cal_gregorian$year(1L))
#> <mixtime[5]>
#> [1] 2020-01-01 2021-01-01 2022-01-01 2023-01-01 2024-01-01
seq(date("2020-01-01"), date("2020-01-31"), by = cal_gregorian$day(7L))
#> <mixtime[5]>
#> [1] 2020-01-01 2020-01-08 2020-01-15 2020-01-22 2020-01-29

# Handling invalid dates with on_invalid
seq(date("2020-01-31"), length.out = 3, by = "1 month")  # warns, uses nearest
#> Warning: The cycle offset (31 days) has produced time points that overflow the month
#> cycle.
#> ! Using the nearest valid time points in the cycle, `on_invalid = "nearest"`
#>   (the default).
#> ℹ Specify `on_invalid` explicitly to suppress this warning.
#> <mixtime[3]>
#> [1] 2020-01-31 2020-02-29 2020-03-31
seq(date("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "nearest")
#> <mixtime[3]>
#> [1] 2020-01-31 2020-02-29 2020-03-31
seq(date("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "overflow")
#> <mixtime[3]>
#> [1] 2020-01-31 2020-03-02 2020-03-31

# Cyclical time sequences
seq(month_of_year(0L), month_of_year(11L))
#> <mixtime[12]>
#>  [1] Dec Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov
# seq(month_of_year(5L), month_of_year(3L), by = cal_gregorian$month(2L))
seq(day_of_week(0L), day_of_week(6L), by = 1)
#> <mixtime[7]>
#> [1] Wed Thu Fri Sat Sun Mon Tue
```
