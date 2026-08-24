# Candidate format strings for parsing time

Combines one or more format strings into a character vector for use as
the `format` argument of
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md).
When multiple format strings are given,
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)
tries each in turn and keeps whichever parses the most values, so
`parse_format()` is the usual way to build up a set of candidates to try
(it's also how
[`chronon_parse_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)/
[`chronon_parse_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)
methods build theirs). It can optionally mark its candidates as using
regex syntax rather than literal text; see the `regex` argument below.

## Usage

``` r
parse_format(..., regex = FALSE)
```

## Arguments

- ...:

  Format strings, as for the `format` argument of
  [`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md).

- regex:

  Whether the literal (non-token) text in `...` should be matched as
  regular expression syntax rather than escaped literally (e.g. `"[/-]"`
  to accept either `/` or `-` as a separator). This is a niche option
  for irregular text - most formats leave it at the default, `FALSE`.
  See the `regex` argument of
  [`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)
  for details.

## Value

A character vector of format strings suitable for
[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md),
with a `"regex"` attribute for the `regex` argument the parser.

## See also

[`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)
for using the result as `format`,
[`chronon_parse_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)/[`chronon_parse_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)
for calendar-specific candidates built this way.

## Examples

``` r
parse_format("{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")
#> [1] "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
#> attr(,"regex")
#> [1] FALSE

# Multiple candidates are tried in turn, keeping whichever parses most values
parse_format(
  "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
  "{lin(year)}/{cyc(month, year)}/{cyc(day, month)}"
)
#> [1] "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
#> [2] "{lin(year)}/{cyc(month, year)}/{cyc(day, month)}"
#> attr(,"regex")
#> [1] FALSE

# regex = TRUE treats the surrounding text as regular expression syntax
parse_format(
  "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}",
  regex = TRUE
)
#> [1] "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}"
#> attr(,"regex")
#> [1] TRUE
```
