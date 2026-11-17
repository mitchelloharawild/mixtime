# Ordering of mixtime vectors

[`sort()`](https://rdrr.io/r/base/sort.html) and
[`xtfrm()`](https://rdrr.io/r/base/xtfrm.html) (which powers
[`order()`](https://rdrr.io/r/base/order.html) and
[`rank()`](https://rdrr.io/r/base/rank.html)) order a mixtime vector
chronologically. A discrete time (e.g. `yearmonth("2020 Jan")`) is a
span rather than an instant, so ordering it against time of another
chronon first picks an instant within the span.

`align_discrete` sets that instant: `0` for the start of the span, `1`
for its end, and `0.5` (the default) for its midpoint. It only changes
the order of time with different chronons; time sharing a chronon keeps
its order.

[`xtfrm()`](https://rdrr.io/r/base/xtfrm.html) takes no other arguments,
so [`order()`](https://rdrr.io/r/base/order.html),
[`rank()`](https://rdrr.io/r/base/rank.html) and vctrs ordering always
use the midpoint. Use [`sort()`](https://rdrr.io/r/base/sort.html) to
choose another instant.

## Usage

``` r
## S7 method for class <mixtime::mixtime>
xtfrm(x)

## S7 method for class <mixtime::mixtime>
sort(x, decreasing = FALSE, na.last = NA, ..., align_discrete = 0.5)
```

## Arguments

- x, decreasing, na.last, ...:

  See [`sort()`](https://rdrr.io/r/base/sort.html).

- align_discrete:

  The position within a discrete time span used to order it against time
  of another chronon, from `0` (start) to `1` (end).

## Value

[`sort()`](https://rdrr.io/r/base/sort.html) returns a mixtime vector;
[`xtfrm()`](https://rdrr.io/r/base/xtfrm.html) returns a numeric vector
suitable for ranking.

## Examples

``` r
x <- c(yearmonth("2020 Jan"), date("2020-01-20"), yearmonth("2020 Feb"))
sort(x)
#> <mixtime[3]>
#> [1] 2020 Jan   2020-01-20 2020 Feb  
# January ends after the 20th
sort(x, align_discrete = 1)
#> <mixtime[3]>
#> [1] 2020-01-20 2020 Jan   2020 Feb  
sort(x, decreasing = TRUE)
#> <mixtime[3]>
#> [1] 2020 Feb   2020-01-20 2020 Jan  
```
