# ISO 8601 year-week time representation

Create or coerce using `yearweek()`.

## Usage

``` r
yearweek(.data, tz = NULL, discrete = TRUE)
```

## Arguments

- .data:

  Another object to be coerced into ISO 8601 year-weeks.

- tz:

  Timezone, defaults to "UTC".

- discrete:

  If `TRUE`, the number of chronons since Unix epoch that `.data` falls
  into is returned as an integer. If `FALSE`, a fractional number of
  chronons is returned (analagous to time using a continuous time
  model).

## Examples

``` r
yearweek(Sys.Date())
#> <mixtime[1]>
#> [1] 2026-W7
yearweek(0:52)
#> <mixtime[53]>
#>  [1] 1970-W1  1970-W2  1970-W3  1970-W4  1970-W5  1970-W6  1970-W7  1970-W8 
#>  [9] 1970-W9  1970-W10 1970-W11 1970-W12 1970-W13 1970-W14 1970-W15 1970-W16
#> [17] 1970-W17 1970-W18 1970-W19 1970-W20 1970-W21 1970-W22 1970-W23 1970-W24
#> [25] 1970-W25 1970-W26 1970-W27 1970-W28 1970-W29 1970-W30 1970-W31 1970-W32
#> [33] 1970-W33 1970-W34 1970-W35 1970-W36 1970-W37 1970-W38 1970-W39 1970-W40
#> [41] 1970-W41 1970-W42 1970-W43 1970-W44 1970-W45 1970-W46 1970-W47 1970-W48
#> [49] 1970-W49 1970-W50 1970-W51 1970-W52 1970-W53
```
