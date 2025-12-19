# ISO 8601 day of week

A cyclical time representation for days within a week using the ISO 8601
standard where weeks start on Monday.

## Usage

``` r
day_of_week(.data, tz = NULL, discrete = TRUE)

week_of_year(.data, tz = NULL, discrete = TRUE)
```

## Arguments

- .data:

  A vector to be coerced into day of week. This can be a date,
  date-time, or numeric vector.

- tz:

  Timezone, defaults to "UTC".

- discrete:

  If `TRUE`, the number of chronons since Unix epoch that `.data` falls
  into is returned as an integer. If `FALSE`, a fractional number of
  chronons is returned (analagous to time using a continuous time
  model).

## Value

A cyclical time object representing the day of the week.

## See also

[`yearweek()`](linear_iso8601.md) for ISO 8601 year-week representation,
[`cyclical_time()`](cyclical_time.md) for creating custom cyclical time
representations

## Examples

``` r
day_of_week(Sys.Date())
#> <mt_cyclical[1]>
#> [1] Fri-0.0%
day_of_week(as.Date("2025-12-15") + 0:6)
#> <mt_cyclical[7]>
#> [1] Mon-0.0% Tue-0.0% Wed-0.0% Thu-0.0% Fri-0.0% Sat-0.0% Sun-0.0%
```
