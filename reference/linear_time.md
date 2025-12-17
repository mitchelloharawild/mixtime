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
#> [1] 2025-NA

# A year-quarter-month time representation with months as the chronon
yqm <- linear_time(tu_month(1L), list(tu_year(1L), tu_quarter(1L)))
yqm(1:100)
#> <mixtime[100]>
#>   [1] 1970-Q2-M3 1970-Q2-M4 1970-Q3-M2 1970-Q3-M3 1970-Q3-M4 1970-Q4-M2
#>   [7] 1970-Q4-M3 1970-Q4-M4 1970-Q5-M2 1970-Q5-M3 1970-Q5-M4 1971-Q2-M2
#>  [13] 1971-Q2-M3 1971-Q2-M4 1971-Q3-M2 1971-Q3-M3 1971-Q3-M4 1971-Q4-M2
#>  [19] 1971-Q4-M3 1971-Q4-M4 1971-Q5-M2 1971-Q5-M3 1971-Q5-M4 1972-Q2-M2
#>  [25] 1972-Q2-M3 1972-Q2-M4 1972-Q3-M2 1972-Q3-M3 1972-Q3-M4 1972-Q4-M2
#>  [31] 1972-Q4-M3 1972-Q4-M4 1972-Q5-M2 1972-Q5-M3 1972-Q5-M4 1973-Q2-M2
#>  [37] 1973-Q2-M3 1973-Q2-M4 1973-Q3-M2 1973-Q3-M3 1973-Q3-M4 1973-Q4-M2
#>  [43] 1973-Q4-M3 1973-Q4-M4 1973-Q5-M2 1973-Q5-M3 1973-Q5-M4 1974-Q2-M2
#>  [49] 1974-Q2-M3 1974-Q2-M4 1974-Q3-M2 1974-Q3-M3 1974-Q3-M4 1974-Q4-M2
#>  [55] 1974-Q4-M3 1974-Q4-M4 1974-Q5-M2 1974-Q5-M3 1974-Q5-M4 1975-Q2-M2
#>  [61] 1975-Q2-M3 1975-Q2-M4 1975-Q3-M2 1975-Q3-M3 1975-Q3-M4 1975-Q4-M2
#>  [67] 1975-Q4-M3 1975-Q4-M4 1975-Q5-M2 1975-Q5-M3 1975-Q5-M4 1976-Q2-M2
#>  [73] 1976-Q2-M3 1976-Q2-M4 1976-Q3-M2 1976-Q3-M3 1976-Q3-M4 1976-Q4-M2
#>  [79] 1976-Q4-M3 1976-Q4-M4 1976-Q5-M2 1976-Q5-M3 1976-Q5-M4 1977-Q2-M2
#>  [85] 1977-Q2-M3 1977-Q2-M4 1977-Q3-M2 1977-Q3-M3 1977-Q3-M4 1977-Q4-M2
#>  [91] 1977-Q4-M3 1977-Q4-M4 1977-Q5-M2 1977-Q5-M3 1977-Q5-M4 1978-Q2-M2
#>  [97] 1978-Q2-M3 1978-Q2-M4 1978-Q3-M2 1978-Q3-M3
yqm(Sys.Date())
#> <mixtime[1]>
#> [1] 2025-Q5-M4

# A year-day time representation with days as the chronon
yd <- linear_time(tu_day(1L), list(tu_year(1L)))
yd(Sys.Date())
#> <mixtime[1]>
#> [1] 2025-D352

ymd_h <- linear_time(tu_hour(1L), list(tu_year(1L), tu_month(1L), tu_day(1L)))
ymd_h(Sys.time())
#> <mixtime[1]>
#> [1] 2025-NA-D18-h9
```
