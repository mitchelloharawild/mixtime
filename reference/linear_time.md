# Linear time representation

`linear_time()` creates a linear time representation using specified
granules and a chronon. Granules are larger time units that define the
structure of time (e.g., years, months), while the chronon is the
smallest indivisible time unit (e.g., days, hours).

## Usage

``` r
linear_time(chronon, granules = list())
```

## Arguments

- chronon:

  A time unit object representing the chronon (e.g., `tu_day(1)`)

- granules:

  A list of time unit objects representing the granules (e.g.,
  `list(tu_year(1), tu_month(1))`)

## Value

An function used to create continuous time points.

## Examples

``` r
# A year-month time representation with months as the chronon
ym <- linear_time(tu_month(1L), list(tu_year(1L)))
ym(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-Feb

# A year-quarter-month time representation with months as the chronon
yqm <- linear_time(tu_month(1L), list(tu_year(1L), tu_quarter(1L)))
yqm(1:100)
#> <mixtime[100]>
#>   [1] 1970-Q1-M1 1970-Q1-M2 1970-Q2-M0 1970-Q2-M1 1970-Q2-M2 1970-Q3-M0
#>   [7] 1970-Q3-M1 1970-Q3-M2 1970-Q4-M0 1970-Q4-M1 1970-Q4-M2 1971-Q1-M0
#>  [13] 1971-Q1-M1 1971-Q1-M2 1971-Q2-M0 1971-Q2-M1 1971-Q2-M2 1971-Q3-M0
#>  [19] 1971-Q3-M1 1971-Q3-M2 1971-Q4-M0 1971-Q4-M1 1971-Q4-M2 1972-Q1-M0
#>  [25] 1972-Q1-M1 1972-Q1-M2 1972-Q2-M0 1972-Q2-M1 1972-Q2-M2 1972-Q3-M0
#>  [31] 1972-Q3-M1 1972-Q3-M2 1972-Q4-M0 1972-Q4-M1 1972-Q4-M2 1973-Q1-M0
#>  [37] 1973-Q1-M1 1973-Q1-M2 1973-Q2-M0 1973-Q2-M1 1973-Q2-M2 1973-Q3-M0
#>  [43] 1973-Q3-M1 1973-Q3-M2 1973-Q4-M0 1973-Q4-M1 1973-Q4-M2 1974-Q1-M0
#>  [49] 1974-Q1-M1 1974-Q1-M2 1974-Q2-M0 1974-Q2-M1 1974-Q2-M2 1974-Q3-M0
#>  [55] 1974-Q3-M1 1974-Q3-M2 1974-Q4-M0 1974-Q4-M1 1974-Q4-M2 1975-Q1-M0
#>  [61] 1975-Q1-M1 1975-Q1-M2 1975-Q2-M0 1975-Q2-M1 1975-Q2-M2 1975-Q3-M0
#>  [67] 1975-Q3-M1 1975-Q3-M2 1975-Q4-M0 1975-Q4-M1 1975-Q4-M2 1976-Q1-M0
#>  [73] 1976-Q1-M1 1976-Q1-M2 1976-Q2-M0 1976-Q2-M1 1976-Q2-M2 1976-Q3-M0
#>  [79] 1976-Q3-M1 1976-Q3-M2 1976-Q4-M0 1976-Q4-M1 1976-Q4-M2 1977-Q1-M0
#>  [85] 1977-Q1-M1 1977-Q1-M2 1977-Q2-M0 1977-Q2-M1 1977-Q2-M2 1977-Q3-M0
#>  [91] 1977-Q3-M1 1977-Q3-M2 1977-Q4-M0 1977-Q4-M1 1977-Q4-M2 1978-Q1-M0
#>  [97] 1978-Q1-M1 1978-Q1-M2 1978-Q2-M0 1978-Q2-M1
yqm(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-Q1-M1

# A year-day time representation with days as the chronon
yd <- linear_time(tu_day(1L), list(tu_year(1L)))
yd(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-41

ymd_h <- linear_time(tu_hour(1L), list(tu_year(1L), tu_month(1L), tu_day(1L)))
ymd_h(Sys.time())
#> <mixtime[1]>
#> [1] 2026-Feb-10-h7
```
