# Constructor for mixtime vectors

Creates a `mixtime` vector, which can contain time points of different
granularities (e.g. monthly and quarterly) in a single vector via
`vecvec`.

## Usage

``` r
new_mixtime(x = new_time())
```

## Arguments

- x:

  A mixtime time vector (created with
  [`new_time()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_time.md))
  to wrap in a mixtime class.

## Value

A `mixtime` object, which allows mixed-type time vectors to coexist in a
single vector.
