# Gregorian cyclical time representations

Cyclical time representations for the Gregorian calendar system. These
functions create time objects that repeat within a larger time cycle,
useful for identifying seasonal patterns or positions within a calendar
period.

## Usage

``` r
month_of_year(.data, tz = NULL, discrete = TRUE)

day_of_year(.data, tz = NULL, discrete = TRUE)

day_of_month(.data, tz = NULL, discrete = TRUE)
```

## Arguments

- .data:

  Another object to be coerced into the specified cyclical time.

- tz:

  Timezone, defaults to "UTC".

- discrete:

  If `TRUE`, the position within the cycle that `.data` falls into is
  returned as an integer. If `FALSE`, a fractional position is returned
  (analagous to time using a continuous time model).

## Details

- `month_of_year()`: Represents the month position within a year (1-12).
  The chronon is one month, cycling within a year.

- `day_of_year()`: Represents the day position within a year (1-365 or
  1-366 for leap years). The chronon is one day, cycling within a year.

- `day_of_month()`: Represents the day position within a month (1-28,
  1-29, 1-30, or 1-31 depending on the month). The chronon is one day,
  cycling within a month.

These cyclical representations are useful for analyzing seasonal
patterns or comparing time points at similar positions across different
years.

## Custom Gregorian cyclical time representations

You can create custom cyclical time representations using
[`cyclical_time()`](cyclical_time.md) with any of the supported
Gregorian time units (see [calendar_gregorian](calendar_gregorian.md)).

For example, to create a representation for day of the month:

    day_of_month <- cyclical_time(
      chronon = tu_day(1L),
      cycle = tu_month(1L)
    )

## See also

[linear_gregorian](linear_gregorian.md) for linear Gregorian time
representations, [`cyclical_time()`](cyclical_time.md) for creating
custom cyclical time representations

## Examples

``` r
month_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] Dec


day_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] D353-0.0%
```
