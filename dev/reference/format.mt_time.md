# Format mixtime vectors

Formats a mixtime vector as a character vector, using a glue-style
format string of `{lin(...)}`/`{cyc(...)}` tokens tailored to the
vector's chronon and cycle. If `format` is omitted, a sensible default
is derived automatically. See
[`vignette("time-format-strings")`](https://pkg.mitchelloharawild.com/mixtime/dev/articles/time-format-strings.md)
for the format string syntax.

## Usage

``` r
# S4 method for class 'mt_time'
format(x, ..., attr = TRUE)
```

## Arguments

- x:

  A mixtime vector.

- ...:

  Additional arguments for methods, including `format`: a glue-style
  format string, defaulting to one derived automatically for `x`.

- attr:

  If `TRUE` (default), append attribute information (e.g. timezone) to
  the default format.

## Value

A character vector the same length as `x`.
