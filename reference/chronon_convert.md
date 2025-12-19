# Convert between chronons

Convert between chronons

## Usage

``` r
chronon_convert(x, ...)

chronon_convert.S7_methods(x, to, discrete = FALSE)
```

## Arguments

- x:

  A linear time object (of class `mt_linear`)

- ...:

  Additional arguments for methods. that `x` falls into is returned as
  an integer. If `FALSE`, a fractional number of target chronons is
  returned (analagous to time using a continuous time model).

- to:

  The target chronon to convert to (a time unit object)

- discrete:

  If `TRUE`, the number of target chronons since Unix epoch

## Value

A numeric or integer representing the time since Unix epoch in terms of
the target chronon's precision.

## Examples

``` r
# Convert from months since epoch to years since epoch
chronon_convert(yearmonth(Sys.Date()), tu_year(1L))
#> [1] 55.91667

# Convert from days since epoch to months since epoch
chronon_convert(Sys.Date(), tu_month(1L))
#> [1] 671.5806
chronon_convert(Sys.Date(), tu_month(1L), discrete = TRUE)
#> [1] 671

# Convert from seconds since epoch to hours since epoch
chronon_convert(Sys.time(), tu_hour(1L))
#> [1] 490592.8
```
