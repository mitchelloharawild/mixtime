# Time format strings

``` r

library(mixtime)
```

mixtime format strings describe how a point in time is formatted as
text. They are analagous to the time format strings used in
[`strptime()`](https://rdrr.io/r/base/strptime.html) and
[`strftime()`](https://rdrr.io/r/base/strptime.html).

## Time component codes

Where [`strftime()`](https://rdrr.io/r/base/strptime.html) uses
`%`-prefixed codes tied to the Gregorian calendar (e.g. `%Y` for year,
`%m` for month), mixtime uses [glue](https://glue.tidyverse.org/)-style
`{...}` placeholders that refer explicitly to time components defined by
linear and cyclical granules.

| Code | Description |
|----|----|
| `lin(granule)` | A linear granularity is typically used for the coarsest granule (e.g. years). |
| `cyc(granule, cycle)` | A cyclical granularity displays finer granules within a coarser cycle (e.g. months within years). |

The `granule` and `cycle` arguments accept time granules from a
calendar, for example the format codes for
[`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)
are `lin(year(1L))` and `cyc(month(1L), year(1L))`. These format codes
are combined in the time format string with glue substitutions:
`"{lin(year(1L))}-{cyc(month(1L), year(1L))}"`.

``` r

format(date(Sys.Date()), format = "{lin(year(1L))}-{cyc(month(1L), year(1L))}")
#> [1] "2026-05"
```

The linear labels are produced by
[`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_labels.md),
and the cyclical labels are produced by
[`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_labels.md).
When formatting mixtime vectors, additional arguments passed to `lin()`
and `cyc()` are forwarded to the label helper functions. Common
arguments include `label = TRUE` to get named labels (e.g. “January”),
and `abbreviate = TRUE` for short names (e.g. “Jan” instead of
“January”).

``` r

format(date(Sys.Date()), format = "{lin(year)} {cyc(month, year, label = TRUE, abbreviate = TRUE)}")
#> [1] "2026 May"
```

Default format strings for each chronon (and cycle) are provided by
methods of the
[`chronon_format_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
and
[`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
generics (see
[`vignette("extending-mixtime")`](https://pkg.mitchelloharawild.com/mixtime/dev/articles/extending-mixtime.md)).
For example, the default format string for a time with a 1-month chronon
is `"{lin(year)}-{cyc(month, year)}"`:

``` r

format(linear_time(Sys.Date(), chronon = cal_gregorian$month(1L)))
#> [1] "2026 May"
```

The granules used in the format string are calendar-aware, so they
inherit attributes from the data being formatted. This includes the
calendar (e.g. `cal_gregorian`) and granule attributes (e.g. time zone
for civil time and location for astronomical time). Explicitly
specifying the calendar or granule attributes in the format string
allows different calendars and granule attributes to be combined in the
same format string.

For example, the lunar phase can be added to a date format string by
including the `phase` granule from the `cal_time_lunar` calendar. We can
also set `emoji = TRUE` to get a lunar phase emoji instead of a numeric
index:

``` r

fmt_date  <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
fmt_lunar <- "{with(cal_time_lunar, cyc(phase, month, emoji = TRUE))}"
format(date(Sys.Date()), format = paste(fmt_date, fmt_lunar))
#> [1] "2026-05-20 🌑"
```

This makes the relationship between the format string and the calendar
structure transparent, and means the same format-string idiom works
unchanged across calendars.

## Advanced time formatting

In addition to the `lin()` and `cyc()` codes, mixtime format strings
support glue-style expressions that can be used to insert arbitrary text
and perform calculations on the time components. The `.time` variable is
available within the format string and refers to the time being
formatted. This variable is commonly used to display time attributes,
including:

| Code          | Description                        |
|---------------|------------------------------------|
| `tz(.time)`   | Time zone (e.g. `AEST`)            |
| `loc(.time)`  | Location (e.g. `37.81S 144.96E`)   |
| `frac(.time)` | Fractional chronons (e.g. `39.2%`) |

The `.time` argument can be used in any calculation within the format
string.

## Comparison with POSIX time format strings

POSIX time format strings are concise but require memorising a large
number of `%`-prefixed codes which are platform-dependent and and mostly
specific to the Gregorian calendar. In contrast, mixtime format strings
are more verbose but more intuitive and generalisable to different
calendars and time formats.

An ISO-8601 date (`YYYY-MM-DD`) is represented with the mixtime format
string `"{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"`. While more
verbose than the equivalent `strftime` format string (`"%Y-%m-%d"`), the
mixtime string is more descriptive and generalisable to other
combinations of granules.

``` r

# YYYY-MM-DD format for a Gregorian date
format(date(Sys.Date()), format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")
#> [1] "2026-05-20"
format(Sys.Date(), format = "%Y-%m-%d")
#> [1] "2026-05-20"
```

This simplifies the construction of less common time formats, for
example the year-day format (`YYYY-DDD`), which is
`"{lin(year)}-D{cyc(day, year)}"` in mixtime but `"%Y-D%j"` in
`strftime` (where `%j` is a non-obvious code for “day of year”).

``` r

# YYY-DDD format for a Gregorian date
format(date(Sys.Date()), format = "{lin(year)}-D{cyc(day, year)}")
#> [1] "2026-D140"
format(Sys.Date(), format = "%Y-D%j")
#> [1] "2026-D140"
```

Format strings for non-Gregorian calendars are also more intuitive,
since the granules are specified in reference to units of calendar
systems. For example, the format string for ISO week dates
(`YYYY-WW-DDD`) is
`"{lin(year)}-W{cyc(week, year)}-{cyc(day, week, label = TRUE)}"` in
mixtime. The equivalent `strftime` format string (`"%G-W%V-%a"`)
requires knowing that `%G`/`%V` are the ISO week variants of `%Y`/`%W` -
a nuanced distinction that is easy to get wrong.

``` r

# YYYY-WW-DD format for a ISO week date
format(
  date(Sys.Date(), calendar = cal_isoweek),
  format = "{lin(year)}-W{cyc(week, year)}-{cyc(day, week, label = TRUE)}"
)
#> [1] "2026-W21-Wed"
format(Sys.Date(), format = "%G-W%V-%a")
#> [1] "2026-W21-Wed"
```

The table below maps every `strftime`/`strptime` conversion
specification to its mixtime equivalent. There are many more possible
mixtime format strings that don’t have a direct `strftime` equivalent.

Show/hide POSIX and mixtime equivalent code table

| POSIX | mixtime | Description |
|----|----|----|
| `%Y` | `{lin(year)}` | Year with century (e.g. 2026) |
| `%G` | `{lin(cal_isoweek$year)}` | ISO 8601 week-based year (1-53) |
| `%m` | `{cyc(month, year)}` | Month as decimal number (01–12) |
| `%B` | `{cyc(month, year, label = TRUE)}` | Full month name (e.g. January) |
| `%b`, `%h` | `{cyc(month, year, label = TRUE, abbreviate = TRUE)}` | Abbreviated month name (e.g. Jan) |
| `%d` | `{cyc(day, month)}` | Day of month (01–31) |
| `%j` | `{cyc(day, year)}` | Day of year (001–366) |
| `%H` | `{cyc(hour, day)}` | Hour, 24-hour clock (00–23) |
| `%I` | `{cyc(hour, ampm)}` | Hour, 12-hour clock (01–12) |
| `%p` | `{cyc(ampm, day)}` | AM/PM indicator |
| `%M` | `{cyc(minute, hour)}` | Minute (00–59) |
| `%S` | `{cyc(second, minute)}` | Second as integer (00–61) |
| `%A` | `{cyc(day, cal_isoweek$week, label = TRUE)}` | Full weekday name (e.g. Monday) |
| `%a` | `{cyc(day, cal_isoweek$week, label = TRUE, abbreviate = TRUE)}` | Abbreviated weekday name (e.g. Mon) |
| `%u` | `{cyc(day, cal_isoweek$week)}` | Weekday as numeric (1 = Monday, 7 = Sunday) |
| `%V` | `{with(cal_isoweek, cyc(week, year))}` | ISO 8601 week number (01–53) |
| `%Z` | `{tz(.time)}` | Time zone abbreviation (e.g. `AEST`) |
| `%z` | `+{tz_offset(.time)}` | UTC offset (e.g. `+1000`) |
| `%s` | `{as.numeric(as.POSIXt(.time))}` | Seconds since the Unix epoch |
