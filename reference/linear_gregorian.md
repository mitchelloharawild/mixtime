# Gregorian continuous time representations

Linear time representations for the Gregorian calendar system. These
functions create time objects measured in years, year-quarters, or
year-months since the Unix epoch (1970-01-01).

## Usage

``` r
year(.data, tz = NULL, discrete = TRUE)

yearquarter(.data, tz = NULL, discrete = TRUE)

yearmonth(.data, tz = NULL, discrete = TRUE)
```

## Arguments

- .data:

  Another object to be coerced into the specified time.

- tz:

  Timezone, defaults to "UTC".

- discrete:

  If `TRUE`, the number of chronons since Unix epoch that `.data` falls
  into is returned as an integer. If `FALSE`, a fractional number of
  chronons is returned (analagous to time using a continuous time
  model).

## Details

- `year()`: Represents time in whole years since 1970. The chronon is
  one year.

- `yearquarter()`: Represents time in quarters, grouped by year. The
  chronon is one quarter, with years as the granule for display and
  grouping.

- `yearmonth()`: Represents time in months, grouped by year. The chronon
  is one month, with years as the granule for display and grouping.

## Custom Gregorian time representations

You can create custom time representations using
[`linear_time()`](linear_time.md) with any of the supported Gregorian
time units (see [calendar_gregorian](calendar_gregorian.md)).

For example, to create a time representation in hours since epoch with
day granules:

    dayhour <- linear_time(
      granules = list(tu_day(1L)),
      chronon = tu_hour(1L)
    )

## Examples

``` r
year(Sys.Date())
#> <mixtime[1]>
#> [1] 2025
year(Sys.Date(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2025-96.4%


yearquarter(Sys.Date())
#> <mixtime[1]>
#> [1] 2025-Q4
yearquarter(Sys.Date(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2025-Q4-85.8%


yearmonth(Sys.Date())
#> <mixtime[1]>
#> [1] 2025-Dec
yearmonth(Sys.Date(), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2025-Dec-58.1%
```
