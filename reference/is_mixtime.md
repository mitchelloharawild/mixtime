# Check if the object is a mixtime

Check if the object is a mixtime

## Usage

``` r
is_mixtime(x)
```

## Arguments

- x:

  An object.

## Value

`TRUE` if the object inherits from the `mixtime` class.

## Examples

``` r
is_mixtime(Sys.Date())
#> [1] FALSE
is_mixtime(yearmonth(1))
#> [1] TRUE
```
