# Describe a time unit's naming scheme

Builds the function a `unit_labels()` method is: assign its result
directly, e.g. `method(unit_labels, granule) <- unit_scheme(...)`. A
unit scheme names a granule itself, e.g. the "month" in "3 months", and
is the single source of truth for every output that renders a count of a
unit: duration formatting and parsing
([`dur()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
tokens), granule labels (`format(month(3L))`), tsibble interval displays
and messages.

## Usage

``` r
unit_scheme(vocab, symbol = "", locale = NULL)

unit_labels(granule, ...)
```

## Arguments

- vocab:

  A function `function(type = NULL, locale = NULL)` returning either the
  full named list of renderings for one locale (`type = NULL`), or one
  rendering (e.g. `vocab("wide")`), as for a
  [`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md).
  Each rendering is a cli-style plural template, e.g. `"day{?/s}"` or
  `"{?zero/one/other}"`. The `"wide"` type gives the unit's full name.
  [`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
  builds this from a plain lookup table.

- symbol:

  A short identifier for the unit, e.g. `"D"`. It is used as the prefix
  of default format strings, in abbreviated label fallbacks (e.g.
  `"5Y"`) and when parsing durations. Symbols are not localised, and
  should be distinct across units (e.g. `"M"` for month and `"m"` for
  minute).

- locale:

  A caller-overridable default locale, selecting which `vocab()` entry
  to use by default.

- granule:

  A time granule instance (e.g. `cal_gregorian$month(1L)`).

- ...:

  Unused; only present for S7 dispatch.

## Value

A function `function(granule) list(vocab =, symbol =, locale =)`,
assignable directly as a `unit_labels()` method. `granule` is only used
for dispatch: a scheme describes a granule class, not the instance a
method happens to be called with.

## Details

    method(unit_labels, cal_gregorian$month) <- unit_scheme(
      vocab = vocab_table(`en-GB` = list(wide = "month{?/s}")),
      symbol = "M"
    )

This differs from a
[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md),
which labels the *values* of a granule (e.g. "March" or "2024") rather
than the granule itself, but the two share the same `vocab` and `locale`
fields.

## See also

`unit_labels()`, the generic a scheme is returned from;
[`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_scheme.md)
for labelling the values of a granule;
[`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
for the common hand-listed name table case.

## Examples

``` r
scheme <- unit_labels(cal_gregorian$year(1L))
scheme$vocab("wide")
#> [1] "year{?/s}"
scheme$symbol
#> [1] "Y"
```
