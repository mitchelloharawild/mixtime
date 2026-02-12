# Generate sequences of mixtime values

Create regular sequences of time values. This method handles both linear
time sequences (dates, date-times) and cyclical time sequences (day of
week, month of year).

## Usage

``` r
# S3 method for class 'mt_cyclical'
seq(from, to, by, length.out = NULL, along.with = NULL, ...)

# S3 method for class 'mt_linear'
seq(from, to, by, length.out = NULL, along.with = NULL, ...)

# S3 method for class 'mixtime'
seq(...)
```

## Arguments

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
    `tu_year(1L)`, `tu_month(1L)`, `tu_day(1L)`)

- length.out:

  Desired length of the sequence (alternative to `to`).

- along.with:

  Take the length from this argument (alternative to `length.out`).

- ...:

  Additional arguments passed to the underlying sequence method.

## Value

A mixtime vector containing the sequence.

## Details

For linear time types (Date, POSIXct, yearmonth, etc.), sequences
progress forward or backward in time. For cyclical time types
(month_of_year, day_of_week, etc.), sequences wrap around cyclically.

## Examples

``` r
# Linear time sequences with integer by
seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"))
#> <mixtime[12]>
#>  [1] 2020-Jan 2020-Feb 2020-Mar 2020-Apr 2020-May 2020-Jun 2020-Jul 2020-Aug
#>  [9] 2020-Sep 2020-Oct 2020-Nov 2020-Dec
seq(yearquarter("2020-01-01"), length.out = 5, by = 3)
#> <mixtime[5]>
#> [1] 2020-Q1 2020-Q4 2021-Q3 2022-Q2 2023-Q1

# Linear time sequences with string intervals
seq(yearmonthday("2020-01-01"), yearmonthday("2020-12-31"), by = "1 month")
#> <mixtime[12]>
#>  [1] 2020-Jan-1 2020-Feb-1 2020-Mar-1 2020-Apr-1 2020-May-1 2020-Jun-1
#>  [7] 2020-Jul-1 2020-Aug-1 2020-Sep-1 2020-Oct-1 2020-Nov-1 2020-Dec-1
seq(yearmonth("2020 Jan"), yearmonth("2025 Jan"), by = "1 year")
#> <mixtime[6]>
#> [1] 2020-Jan 2021-Jan 2022-Jan 2023-Jan 2024-Jan 2025-Jan
seq(yearmonthday("2020-01-01"), length.out = 10, by = "2 weeks")
#> <mixtime[10]>
#>  [1] 2020-Jan-1  2020-Jan-15 2020-Jan-29 2020-Feb-12 2020-Feb-26 2020-Mar-11
#>  [7] 2020-Mar-25 2020-Apr-8  2020-Apr-22 2020-May-6 

# Linear time sequences with time units
seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"), by = tu_month(2L))
#> <mixtime[6]>
#> [1] 2020-Jan 2020-Mar 2020-May 2020-Jul 2020-Sep 2020-Nov
seq(yearmonthday("2020-01-01"), length.out = 5, by = tu_year(1L))
#> <mixtime[5]>
#> [1] 2020-Jan-1 2021-Jan-1 2022-Jan-1 2023-Jan-1 2024-Jan-1
seq(yearmonthday("2020-01-01"), yearmonthday("2020-01-31"), by = tu_day(7L))
#> <mixtime[5]>
#> [1] 2020-Jan-1  2020-Jan-8  2020-Jan-15 2020-Jan-22 2020-Jan-29

# Cyclical time sequences
seq(month_of_year(0L), month_of_year(11L))
#> <mixtime[12]>
#>  [1] Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
seq(month_of_year(5L), month_of_year(3L), by = tu_month(2L))
#> <mixtime[6]>
#> [1] Jun Aug Oct Dec Feb Apr
seq(day_of_week(0L), day_of_week(6L), by = 1)
#> <mixtime[7]>
#> [1] Thu Fri Sat Sun Mon Tue Wed
```
