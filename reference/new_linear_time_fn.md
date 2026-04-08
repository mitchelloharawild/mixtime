# Linear time function factory

`new_linear_time_fn()` creates a linear time function for a specified
chronon. A chronon is the smallest indivisible time unit (e.g., days,
hours).

## Usage

``` r
new_linear_time_fn(chronon, fallback_calendar = cal_gregorian)
```

## Arguments

- chronon:

  A bare call for a time unit object representing the chronon (e.g.,
  `day(1)`)

- fallback_calendar:

  A fallback calendar used to find the time units for conversion if they
  don't exist in the calendar of the input data (e.g., `cal_isoweek`)

## Value

A function used to create linear time points.

## Examples

``` r
# NOTE: These examples need updating to define default granules/format strings.

# A year-month time representation with months as the chronon
ym <- new_linear_time_fn(month(1L))
ym(Sys.Date())
#> <mixtime[1]>
#> [1] 2026 Apr

# A year-quarter-month time representation with months as the chronon
yqm <- new_linear_time_fn(month(1L))
yqm(1:100)
#> <mixtime[100]>
#>   [1] 1970 Feb 1970 Mar 1970 Apr 1970 May 1970 Jun 1970 Jul 1970 Aug 1970 Sep
#>   [9] 1970 Oct 1970 Nov 1970 Dec 1971 Jan 1971 Feb 1971 Mar 1971 Apr 1971 May
#>  [17] 1971 Jun 1971 Jul 1971 Aug 1971 Sep 1971 Oct 1971 Nov 1971 Dec 1972 Jan
#>  [25] 1972 Feb 1972 Mar 1972 Apr 1972 May 1972 Jun 1972 Jul 1972 Aug 1972 Sep
#>  [33] 1972 Oct 1972 Nov 1972 Dec 1973 Jan 1973 Feb 1973 Mar 1973 Apr 1973 May
#>  [41] 1973 Jun 1973 Jul 1973 Aug 1973 Sep 1973 Oct 1973 Nov 1973 Dec 1974 Jan
#>  [49] 1974 Feb 1974 Mar 1974 Apr 1974 May 1974 Jun 1974 Jul 1974 Aug 1974 Sep
#>  [57] 1974 Oct 1974 Nov 1974 Dec 1975 Jan 1975 Feb 1975 Mar 1975 Apr 1975 May
#>  [65] 1975 Jun 1975 Jul 1975 Aug 1975 Sep 1975 Oct 1975 Nov 1975 Dec 1976 Jan
#>  [73] 1976 Feb 1976 Mar 1976 Apr 1976 May 1976 Jun 1976 Jul 1976 Aug 1976 Sep
#>  [81] 1976 Oct 1976 Nov 1976 Dec 1977 Jan 1977 Feb 1977 Mar 1977 Apr 1977 May
#>  [89] 1977 Jun 1977 Jul 1977 Aug 1977 Sep 1977 Oct 1977 Nov 1977 Dec 1978 Jan
#>  [97] 1978 Feb 1978 Mar 1978 Apr 1978 May
yqm(Sys.Date())
#> <mixtime[1]>
#> [1] 2026 Apr

# A year-day time representation with days as the chronon
yd <- new_linear_time_fn(day(1L))
yd(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-04-08

# Gregorian date time with hourly precision
ymd_h <- new_linear_time_fn(hour(1L))
ymd_h(Sys.time())
#> <mixtime[1]>
#> [1] 2026-04-08 00h

# ISO-week-date calendar
ywd <- new_linear_time_fn(day(1L), fallback_calendar = cal_isoweek)
ywd(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-04-08
```
