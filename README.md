
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mixtime <a href="https://pkg.mitchelloharawild.com/mixtime/"><img src="man/figures/logo.svg" align="right" height="139" alt="mixtime website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/mitchelloharawild/mixtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mitchelloharawild/mixtime/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

mixtime provides flexible time classes for time series analysis and
forecasting with mixed temporal granularities. It is designed for
handling temporal data at different frequencies, making it ideal for:

- Changing observation frequencies (e.g., historical quarterly records
  now reported monthly)
- Analysis involving data at different temporal resolutions (e.g.,
  quarterly tourism and monthly turnover)
- Forecasting with temporal reconciliation across multiple time scales

## Key Features

**📈 Linear Time** - Create linear time vectors with `linear_time()` or
with helpers:

- `yearquarter()`, `yearmonth()`, `yearweek()`, `yearmonthday()`

**🔄 Cyclical Time** - Create cyclical time vectors with
`cyclical_time()` or with helpers:

- `month_of_year()`, `day_of_year()`, `day_of_month()`, `day_of_week()`,
  `week_of_year()`

**🕰️ Time types**

- Discrete and continuous time models
- Timezone support for all chronons
- Custom granule sizes (e.g. `week(2L)` for fortnights)
- Mixed granularity vectors for different temporal resolution over time
  (or series)

**📅 Calendar Systems** - Support for several calendars:

- `cal_gregorian` - Gregorian dates (e.g. 2026-02-17)
- `cal_isoweek` - ISO week dates (e.g. 2026-W8-Tue)
- More calendars coming, including custom censored calendars

**🧮 Temporal Operations**

- Rounding: `floor_time()`, `round_time()`, `ceiling_time()`
- Sequencing: `seq()` for linear and cyclical time points

## Installation

The development version can be installed from
[GitHub](https://github.com/mitchelloharawild/mixtime) with:

``` r
# install.packages("remotes")
remotes::install_github("mitchelloharawild/mixtime")
```

## Usage

``` r
library(mixtime)
demo_time <- as.POSIXct("2026-02-22 18:30:42", tz = "UTC")
demo_date <- as.Date("2026-02-22")
```

The mixtime package is designed around the time units of calendars.
These time units are used to create and manipulate time vectors.

### Calendars and time units

Calendars have a `cal_*` prefix, which contain a set of time units that
can be accessed with `<cal>$<unit>`.

``` r
# The Gregorian calendar
cal_gregorian
#> <cal_gregorian>
#> Time units:
#>   - year
#>   - quarter
#>   - month
#>   - day
#>   - hour
#>   - minute
#>   - second
#>   - millisecond

# A 1-month time unit
cal_gregorian$month(1L) # (1L is integer 1)
#> <mixtime::tu_month> int 1
#>  @ tz: chr "UTC"

# A 2-week time unit (fortnights)
cal_isoweek$week(2L)
#> <mixtime::tu_week> int 2
#>  @ tz: chr "UTC"
```

### Linear Time

A linear time vector uses time units to define the resolution of time
points, known as a `chronon` (the smallest unit of time). When the input
time has a different resolution than the `chronon`, it will be
automatically converted.

``` r
linear_time(demo_date, chronon = cal_gregorian$month(1L))
#> <mixtime[1]>
#> [1] 2026 Feb
```

Discrete time models (integer-based values) are used by default, however
continuous time models (double-based values) can be used with
`discrete = FALSE` to allow fractional chronons.

``` r
# February 22nd is 75% through the month (in non-leap years)
linear_time(demo_date, chronon = cal_gregorian$month(1L), discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026 Feb 75.0%
```

Linear time helper functions are available to quickly create common time
points.

``` r
# Create time vectors at different granularities
yearquarter(demo_date) + 0:7
#> <mixtime[8]>
#> [1] 2026 Q1 2026 Q2 2026 Q3 2026 Q4 2027 Q1 2027 Q2 2027 Q3 2027 Q4
yearmonth(demo_date) + 0:11
#> <mixtime[12]>
#>  [1] 2026 Feb 2026 Mar 2026 Apr 2026 May 2026 Jun 2026 Jul 2026 Aug 2026 Sep
#>  [9] 2026 Oct 2026 Nov 2026 Dec 2027 Jan
yearweek(demo_date) + 0:10
#> <mixtime[11]>
#>  [1] 2026 W08 2026 W09 2026 W10 2026 W11 2026 W12 2026 W13 2026 W14 2026 W15
#>  [9] 2026 W16 2026 W17 2026 W18
yearmonthday(demo_date) + 0:6
#> <mixtime[7]>
#> [1] 2026-02-22 2026-02-23 2026-02-24 2026-02-25 2026-02-26 2026-02-27 2026-02-28
```

The mixtime package allows time of different granulities to be combined
in a single vector.

``` r
c(
  year(demo_date), yearquarter(demo_date), 
  yearmonth(demo_date), yearweek(demo_date)
)
#> <mixtime[4]>
#> [1] 2026     2026 Q1  2026 Feb 2026 W08
```

### Cyclical Time

A cyclical time vector is defined by two calendar time units: a
`chronon` (the smaller unit of time) and a `cycle` (the larger unit that
the `chronon` loops over).

``` r
# The `calendar` argument provides a masking scope to `chronon` and `cycle`
cyclical_time(demo_date, chronon = day(1L), cycle = week(1L), calendar = cal_isoweek)
#> <mixtime[1]>
#> [1] Sun
```

There are several cyclical time helper functions for convenience.

``` r
# Extract cyclical components
month_of_year(demo_date)
#> <mixtime[1]>
#> [1] Feb
week_of_year(demo_date)
#> <mixtime[1]>
#> [1] 08
day_of_week(demo_date)
#> <mixtime[1]>
#> [1] Sun

# Continuous cyclical time shows progress through chronons
day_of_week(demo_time, discrete = FALSE)
#> <mixtime[1]>
#> [1] Sun-77.1%
```

### Timezones

All linear and cyclical time vectors support timezones via the `tz`
argument.

``` r
demo_time
#> [1] "2026-02-22 18:30:42 UTC"
# Same day (Sunday) in LA
yearmonthday(demo_time, tz = "America/Los_Angeles")
#> <mixtime[1]>
#> [1] 2026-02-22 PST
yearmonthday(demo_time, tz = "America/Los_Angeles", discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-02-22 77.1% PST
day_of_week(demo_time, tz = "America/Los_Angeles")
#> <mixtime[1]>
#> [1] Sun

# Next day (Monday) in Melbourne (23% through the 23rd)
yearmonthday(demo_time, tz = "Australia/Melbourne")
#> <mixtime[1]>
#> [1] 2026-02-23 AEDT
yearmonthday(demo_time, tz = "Australia/Melbourne", discrete = FALSE)
#> <mixtime[1]>
#> [1] 2026-02-22 77.1% AEDT
day_of_week(demo_time, tz = "Australia/Melbourne")
#> <mixtime[1]>
#> [1] Mon
```

### Temporal Manipulation

Linear time points can be adjusted to the floor, ceiling, or rounded to
a specified time unit.

``` r
# Round dates to different granularities
floor_time(demo_date, cal_gregorian$month(1L))
#> [1] "2026-02-01"
round_time(demo_date, cal_isoweek$week(1L))
#> [1] "2026-02-23"
ceiling_time(demo_date, cal_gregorian$month(1L))
#> [1] "2026-03-01"
```

### Time Sequences

The `seq()` function creates sequences of time points iterating by a
given time unit.

``` r
# Integer increments (advances by chronon's natural unit)
seq(yearmonth(demo_date), by = 1L, length.out = 10)
#> <mixtime[10]>
#>  [1] 2026 Feb 2026 Mar 2026 Apr 2026 May 2026 Jun 2026 Jul 2026 Aug 2026 Sep
#>  [9] 2026 Oct 2026 Nov

# Calendar time units allow sequencing by other units
seq(yearmonthday(demo_date), by = cal_gregorian$month(1L), length.out = 8)
#> <mixtime[8]>
#> [1] 2026-02-22 2026-03-22 2026-04-22 2026-05-22 2026-06-22 2026-07-22 2026-08-22
#> [8] 2026-09-22
```
