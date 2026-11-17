# Parse text into time points or durations

`time_parse()` is the inverse of
[`format()`](https://rdrr.io/r/base/format.html): given text and the
same `{lin(...)}`/`{cyc(...)}`/`{dur(...)}` template `format` uses, it
reconstructs the time points (via
[`time_compose()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_compose.md))
or durations that would have produced that text.

## Usage

``` r
time_parse(
  x,
  chronon = NULL,
  cycle = NULL,
  format = NULL,
  regex = FALSE,
  na = c("", "NA"),
  calendar = NULL,
  locale = NULL,
  discrete = TRUE
)
```

## Arguments

- x:

  A character vector to parse.

- chronon:

  Target time granule for the result, and (with `cycle`) the source of
  `format` candidates when `format` is `NULL`. Its attributes (e.g.
  `tz`) fill in whatever `format` leaves unset, and the result is
  converted onto it if `format` reaches a different chronon.

- cycle:

  Target cycle granule, pairing with `chronon` for a cyclical result.
  Requires `chronon`.

- format:

  A glue-style format string of
  [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
  tokens (or a
  [`dur()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
  token), e.g. `"{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"` (see
  [`vignette("time-format-strings")`](https://pkg.mitchelloharawild.com/mixtime/dev/articles/time-format-strings.md)),
  or several to try: whichever parses the most values of `x` is used for
  the whole vector (ties keep the earliest-listed format), and its
  unparsed values become `NA` (with a warning). Aborts if no format
  matches the shape of even one value.
  `time_parse(format(x, fmt), format = fmt)` round-trips back to `x`.
  `NULL` (the default) derives candidates from `chronon`/`cycle` via
  [`chronon_parse_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)/[`chronon_parse_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md);
  requires `chronon`.

- regex:

  If `FALSE` (the default), literal text surrounding tokens is matched
  exactly. If `TRUE`, it's instead used verbatim as a regular
  expression, e.g. `"[/-]"` to accept either `/` or `-` as a separator;
  `(...)` groups you write are treated as non-capturing, since capturing
  groups are reserved for the tokens. Ignored when `format` is derived
  from `chronon`, which carries its own regex mode.

- na:

  Strings to treat as missing (`NA`), checked before matching `format`.
  Not counted in the parsing-failure warning, unlike a value that fails
  to match `format`.

- calendar:

  Calendar used to resolve granule names in `format`, and to
  disambiguate `chronon`'s
  [`chronon_parse_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)
  candidates when `format` is `NULL`. `NULL` (the default) uses
  `time_calendar(cycle)` or `time_calendar(chronon)`, whichever is
  supplied, else
  [cal_gregorian](https://pkg.mitchelloharawild.com/mixtime/dev/reference/calendar_gregorian.md).

- locale:

  Default locale for named (`label = TRUE`) tokens that don't specify
  their own. `NULL` defers to each token's own scheme. Duration
  (`{dur(...)}`) unit names are currently English-only, so a non-English
  locale is an error for them.

- discrete:

  Whether the result is discrete (integer chronon counts) or continuous
  (fractional). See
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md).
  A fractional duration magnitude (e.g. `"1.5 days"`) only parses when
  `discrete = FALSE`.

## Value

A `mixtime` time vector, the same length as `x`. A duration if `format`
is a `{dur(...)}` token, linear if it includes a `{lin(...)}` token (or
`cycle` is `NULL`), cyclical otherwise.

## Details

A `format` with a `{lin(...)}` token parses to linear time; one of only
`{cyc(...)}` tokens parses to cyclical time, e.g.
`time_parse("Feb", format = "{cyc(month, year, label = TRUE)}")`
recovers the same kind of value as
[`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md).
A format with a `{dur(...)}` token (which must be its only token) parses
to a
[`duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration.md),
e.g. `time_parse("3 days", format = "{dur(day)}")`.

Granule-specific extraction and decoding labels for each `{lin(...)}`/
`{cyc(...)}` token is done by
[`linear_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md)/
[`cyclical_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md).
A `{dur(...)}` token matches a number, optionally followed by the
granule's unit name (e.g. `"3 days"`, `"1 day"`, `"3D"` or `"3"`).

## See also

[`format()`](https://rdrr.io/r/base/format.html) for the inverse
direction,
[`time_compose()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_compose.md)
for composing a time point from already-decoded components,
[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
for declaring how a granule's labels parse,
[`chronon_parse_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)/[`chronon_parse_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)
for the candidate formats derived from `chronon`/`cycle`, and
[`vignette("time-format-strings")`](https://pkg.mitchelloharawild.com/mixtime/dev/articles/time-format-strings.md)
for the format string syntax.

## Examples

``` r
time_parse("2024-02-15", format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")
#> <mixtime[1]>
#> [1] 2024-02-15
time_parse(
  "15 Feb 2024",
  format = "{cyc(day, month)} {cyc(month, year, label = TRUE)} {lin(year)}"
)
#> <mixtime[1]>
#> [1] 2024-02-15

# One bad value becomes NA (with a warning) instead of aborting the batch
time_parse(
  c("2024-02-15", "not a date"),
  format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
)
#> Warning: 1 value failed to parse and was set to `NA`.
#> ✖ "not a date"
#> <mixtime[2]>
#> [1] 2024-02-15 NA        

# No {lin(...)} token: parses to cyclical time
time_parse("Feb", format = "{cyc(month, year, label = TRUE)}")
#> <mixtime[1]>
#> [1] Feb

# Several formats: whichever parses the most values is used for the whole
# vector; here none of the "Y-M-D" format's values match, so the "D/M/Y"
# format (which matches both) is used instead
time_parse(
  c("15/02/2024", "20/03/2024"),
  format = c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )
)
#> <mixtime[2]>
#> [1] 2024-02-15 2024-03-20

# regex = TRUE: match "/" or "-" as the separator, 
# and tolerate a trailing comment after the date.
time_parse(
  c("2024-02-15", "2024/02/15 (approx)"),
  format = "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}( .*)?",
  regex = TRUE
)
#> <mixtime[2]>
#> [1] 2024-02-15 2024-02-15

# Durations, with an optional unit name after the magnitude
time_parse(c("3 days", "1 day", "2D", "5"), format = "{dur(day)}")
#> <mixtime[4]>
#> [1] 3 days 1 day  2 days 5 days
time_parse("1.5 hours", format = "{dur(hour)}", discrete = FALSE)
#> <mixtime[1]>
#> [1] 1.5 hours

# Default format strings from the target chronon, and results with `tz`.
time_parse("2024-02-15 09:00:00", chronon = cal_gregorian$second(1L, tz = "America/Los_Angeles"))
#> <mixtime[1]>
#> [1] 2024-02-15 09:00:00 PST
```
