# Cyclical time representation

`cyclical_time()` creates a cyclical time representation using specified
cycle and chronon (and optionally display granules). The cycles is the
larger time unit that defines the time period over which the chronon
loops over (e.g. a week, `tu_week(1L)`). The chronon is the smaller time
unit that iterates within each cycle (e.g. a day, `tu_day(1L)`).
Combined, these two granules form a cyclical time point (e.g. day of the
week).

## Usage

``` r
cyclical_time(chronon, cycle)
```

## Arguments

- chronon:

  A time unit object representing the chronon (e.g., `tu_month(1L)`)

- cycle:

  A time unit object representing the cycle (e.g., `tu_year(1L)`)

## Value

An function used to create cyclical time points.

## Examples

``` r
day_of_week <- cyclical_time(tu_day(1L), tu_week(1L))
day_of_week(Sys.Date())
#> <mixtime[1]>
#> [1] Tue-0.0%

month_of_year <- cyclical_time(tu_month(1L), tu_year(1L))
month_of_year(Sys.Date())
#> <mixtime[1]>
#> [1] Feb
```
