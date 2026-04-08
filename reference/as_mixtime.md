# Convert a time class into a mixtime

Coerces a time object (e.g. `Date`, `POSIXct`, `yearmonth`) to a
`mixtime` vector using
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).
The chronon and cycle are inferred from `x` via
[`time_chronon()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_chronon.md)
and
[`time_cycle()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_cycle.md).

## Usage

``` r
as_mixtime(x, ...)
```

## Arguments

- x:

  A time value to convert to a `mixtime`. Any time class with a defined
  [`time_chronon()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_chronon.md)
  method can be converted (e.g. `Date`, `POSIXct`, `yearmonth`, etc.).

- ...:

  Additional arguments passed to the underlying
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html)
  method.

## Value

A `mixtime` object corresponding to `x`.

## See also

[`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/mixtime.md)
for constructing a `mixtime` directly from data,
[`is_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/reference/is_mixtime.md)
for testing if an object is a `mixtime`.

## Examples

``` r
as_mixtime(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-04-08
as_mixtime(Sys.time())
#> <mixtime[1]>
#> [1] 2026-04-08 03:06:05
```
