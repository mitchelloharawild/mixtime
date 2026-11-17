# Format underspecified cyclical time (`mt_cyclical_free`)

Underspecified cyclical time has no `at` (no enclosing cycle) to resolve
a within-cycle position from, so unlike located
[mt_cyclical](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md)
it is rendered directly from its own stored offset via the same
[`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
template and
[`cyclical_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md)
renderer a located value's reduced position would use, called with
`at = NULL`. The offset is split across the template's tokens using
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
only, never
[`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_divmod.md),
which would need a specific enclosing cycle (e.g. which year).

## Usage

``` r
# S4 method for class 'mt_cyclical_free'
format(x, ..., attr = TRUE)
```

## Arguments

- x:

  A mixtime vector.

- format:

  A glue-style format string of `{cyc(...)}` tokens (see
  [format.mt_time](https://pkg.mitchelloharawild.com/mixtime/dev/reference/format.mt_time.md)),
  defaulting to one derived automatically for `x`. A free value has no
  position on the time line, so `{lin(...)}` tokens are an error, and
  attribute tokens such as `{tz(.time)}` render as empty text since a
  free value has no time zone.

- ...:

  Additional arguments for methods, including `format`: a glue-style
  format string, defaulting to one derived automatically for `x`.

- attr:

  If `TRUE` (default), append attribute information (e.g. timezone) to
  the default format.

## Value

A character vector the same length as `x`.
