# Check if an object is a mixtime

Tests whether `x` inherits from the `mixtime` class.

## Usage

``` r
is_mixtime(x)
```

## Arguments

- x:

  An object to test.

## Value

A scalar logical: `TRUE` if `x` is a `mixtime` vector, `FALSE`
otherwise.

## See also

[`as_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/as_mixtime.md)
to coerce objects to `mixtime`,
[`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/mixtime.md)
to construct a `mixtime`.

## Examples

``` r
is_mixtime(Sys.Date())
#> [1] FALSE
is_mixtime(mixtime(Sys.Date()))
#> [1] TRUE
```
