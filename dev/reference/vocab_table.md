# Build a `vocab` function from a plain name table

The common case for a
[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
`vocab` argument: a hand-listed table of names, one entry per locale,
each a named list of renderings. By convention these follow CLDR's
wide/abbreviated/narrow, but `type` can be anything. `vocab_table()`
wraps it in the `function(type = NULL, locale = NULL)` shape `vocab`
requires.

## Usage

``` r
vocab_table(..., default_locale = "en-GB")
```

## Arguments

- ...:

  Named entries, one per locale (e.g.
  ``  `en-GB` = list(wide = month.name, abbreviated = month.abb) ``).
  Locale tags follow BCP 47.

- default_locale:

  The locale returned when `locale` is unsupplied (or explicitly `NULL`,
  meaning "caller didn't ask", since
  [`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)/[`format()`](https://rdrr.io/r/base/format.html)
  always pass some value for `locale`).

## Value

A function `function(type = NULL, locale = NULL)`, suitable as the
`vocab` field of a
[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md).

## Details

For vocab backed by an external i18n source instead of a hand-listed
table, write that `function(type, locale)` directly instead. Same shape,
no list needed.

## See also

[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)

## Examples

``` r
month_vocab <- vocab_table(`en-GB` = list(wide = month.name, abbreviated = month.abb))
month_vocab("abbreviated")
#>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
month_vocab(locale = "en-GB")
#> $wide
#>  [1] "January"   "February"  "March"     "April"     "May"       "June"     
#>  [7] "July"      "August"    "September" "October"   "November"  "December" 
#> 
#> $abbreviated
#>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
#> 
```
