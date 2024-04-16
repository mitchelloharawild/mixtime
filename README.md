
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mixtime

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/mitchelloharawild/mixtime/workflows/R-CMD-check/badge.svg)](https://github.com/mitchelloharawild/mixtime/actions)
[![Coverage
status](https://codecov.io/gh/mitchelloharawild/mixtime/branch/master/graph/badge.svg)](https://codecov.io/github/mitchelloharawild/mixtime?branch=master)
<!-- badges: end -->

mixtime provides flexible time classes for representing mixed temporal
granularities and custom calendar structures.

The feature wishlist for this package includes:

- Multiple temporal granularities (sub-daily, daily, weekly, monthly,
  quarterly, etc.)
- Optional and custom temporal origins
- Custom calendar structures (working days/hours, holiday effects,
  trading days)
- Mixed temporal classes (daily and weekly granularities in same vector)

Stretch goals for the package include:

- Representing temporal nesting (2020-01-05 is nested by 2020-01 is
  nested by 2020)
- `time_join()` operation which respects temporal nesting

## Installation

<!-- You can install the released version of mixtime from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("mixtime") -->
<!-- ``` -->

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mitchelloharawild/mixtime")
```

## Example

Creating mixtimes.

``` r
library(mixtime)
yearmonth(0:11) # By default, time classes have an origin
#> <mixtime[12]>
#>  [1] 1970 Jan 1970 Feb 1970 Mar 1970 Apr 1970 May 1970 Jun 1970 Jul 1970 Aug
#>  [9] 1970 Sep 1970 Oct 1970 Nov 1970 Dec
yearmonth(0:11) - yearmonth(0) # However some operations can produce mixtimes without origins
#> <mixtime[12]>
#>  [1] 0 months  1 month   2 months  3 months  4 months  5 months  6 months 
#>  [8] 7 months  8 months  9 months  10 months 11 months

yearquarter(0:3)
#> <mixtime[4]>
#> [1] 1970 Q1 1970 Q2 1970 Q3 1970 Q4
yearquarter(0:3) - yearquarter(0)
#> <mixtime[4]>
#> [1] 0 quarters 1 quarter  2 quarters 3 quarters

# Different temporal granularities can be combined:
c(yearquarter(0:3), yearmonth(0:11))
#> <mixtime[16]>
#>  [1] 1970 Q1  1970 Q2  1970 Q3  1970 Q4  1970 Jan 1970 Feb 1970 Mar 1970 Apr
#>  [9] 1970 May 1970 Jun 1970 Jul 1970 Aug 1970 Sep 1970 Oct 1970 Nov 1970 Dec
```

Sequences of mixtimes.

``` r
seq(yearmonth(0), yearmonth(10), by = 1)
#> <mixtime[11]>
#>  [1] 1970 Jan 1970 Feb 1970 Mar 1970 Apr 1970 May 1970 Jun 1970 Jul 1970 Aug
#>  [9] 1970 Sep 1970 Oct 1970 Nov
```

Integration with tsibble.

``` r
tsibble::tsibble(time = yearmonth(0:5), index = time)
#> # A tsibble: 6 x 1 [1M]
#>        time
#>   <mixtime>
#> 1  1970 Jan
#> 2  1970 Feb
#> 3  1970 Mar
#> 4  1970 Apr
#> 5  1970 May
#> 6  1970 Jun
tsibble::tsibble(time = c(year(0), yearquarter(0:3), yearmonth(0:5), Sys.Date()), index = time)
#> # A tsibble: 12 x 1 [1Y, 1Q, 1M, 1D]
#>          time
#>     <mixtime>
#>  1          0
#>  2    1970 Q1
#>  3   1970 Jan
#>  4    1970 Q2
#>  5   1970 Feb
#>  6    1970 Q3
#>  7   1970 Mar
#>  8    1970 Q4
#>  9   1970 Apr
#> 10   1970 May
#> 11   1970 Jun
#> 12 2024-04-16
```

Change granularities by updating the calendar.

``` r
x <- yearmonth(0:11)
tsibble::tsibble(
  yearmonth = x,
  yearquarter = set_time_units(x, tu_quarter(1)),
  year = set_time_units(x, tu_year(1)),
  index = yearmonth
)
#> # A tsibble: 12 x 3 [1M]
#>    yearmonth yearquarter      year
#>    <mixtime>   <mixtime> <mixtime>
#>  1  1970 Jan     1970 Q1      1970
#>  2  1970 Feb     1970 Q1      1970
#>  3  1970 Mar     1970 Q1      1970
#>  4  1970 Apr     1970 Q2      1970
#>  5  1970 May     1970 Q2      1970
#>  6  1970 Jun     1970 Q2      1970
#>  7  1970 Jul     1970 Q3      1970
#>  8  1970 Aug     1970 Q3      1970
#>  9  1970 Sep     1970 Q3      1970
#> 10  1970 Oct     1970 Q4      1970
#> 11  1970 Nov     1970 Q4      1970
#> 12  1970 Dec     1970 Q4      1970
```
