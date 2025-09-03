

<!-- README.md is generated from README.qmd. Please edit that file -->

# mixtime <a href="https://pkg.mitchelloharawild.com/mixtime/"><img src="man/figures/logo.svg" align="right" height="139" alt="mixtime website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/mitchelloharawild/mixtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mitchelloharawild/mixtime/actions/workflows/R-CMD-check.yaml)
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
# install.packages("remotes")
remotes::install_github("mitchelloharawild/mixtime")
```

## Example

Creating mixtimes.

``` r
library(mixtime)
#> Registered S3 method overwritten by 'tsibble':
#>   method               from 
#>   as_tibble.grouped_df dplyr
yearmonth(0:11) # By default, time classes have an origin
#> <mixtime[12]>
#>  [1] 1970 Jan 1970 Feb 1970 Mar 1970 Apr 1970 May 1970 Jun 1970 Jul 1970 Aug
#>  [9] 1970 Sep 1970 Oct 1970 Nov 1970 Dec
yearmonth(0:11) - yearmonth(0) # However some operations can produce mixtimes without origins
#> <vecvec[12]>
#>  [1]  0  1  2  3  4  5  6  7  8  9 10 11

yearquarter(0:3)
#> <mixtime[4]>
#> [1] 1970 Q1 1970 Q2 1970 Q3 1970 Q4
yearquarter(0:3) - yearquarter(0)
#> <vecvec[4]>
#> [1] 0 1 2 3

# Different temporal granularities can be combined:
c(yearquarter(0:3), yearmonth(0:11))
#> <mixtime[16]>
#>  [1] 1970 Q1  1970 Q2  1970 Q3  1970 Q4  1970 Jan 1970 Feb 1970 Mar 1970 Apr
#>  [9] 1970 May 1970 Jun 1970 Jul 1970 Aug 1970 Sep 1970 Oct 1970 Nov 1970 Dec
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
tsibble::tsibble(time = c(year(2010:2013), yearquarter(0:3), yearmonth(0:5), as.Date("2010-03-05") + 0:4), index = time)
#> # A tsibble: 19 x 1 [1Y, 1Q, 1M, 1D]
#>          time
#>     <mixtime>
#>  1       2010
#>  2    1970 Q1
#>  3   1970 Jan
#>  4 2010-03-05
#>  5       2011
#>  6    1970 Q2
#>  7   1970 Feb
#>  8 2010-03-06
#>  9       2012
#> 10    1970 Q3
#> 11   1970 Mar
#> 12 2010-03-07
#> 13       2013
#> 14    1970 Q4
#> 15   1970 Apr
#> 16 2010-03-08
#> 17   1970 May
#> 18 2010-03-09
#> 19   1970 Jun
```

<!-- Change granularities by updating the calendar. -->

<!-- ```{r granularity} -->

<!-- x <- yearmonth(0:11) -->

<!-- tsibble::tsibble( -->

<!--   yearmonth = x, -->

<!--   yearquarter = set_time_units(x, tu_quarter(1)), -->

<!--   year = set_time_units(x, tu_year(1)), -->

<!--   index = yearmonth -->

<!-- ) -->

<!-- ``` -->
