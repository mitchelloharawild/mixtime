
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moment

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/mitchelloharawild/moment/workflows/R-CMD-check/badge.svg)](https://github.com/mitchelloharawild/moment/actions)
[![Coverage
status](https://codecov.io/gh/mitchelloharawild/moment/branch/master/graph/badge.svg)](https://codecov.io/github/mitchelloharawild/moment?branch=master)
<!-- badges: end -->

moment provides flexible time classes for representing mixed temporal
granularities and custom calendar structures.

The feature wishlist for this package includes:

  - Multiple temporal granularities (sub-daily, daily, weekly, monthly,
    quarterly, etc.)
  - Optional and custom temporal origins
  - Custom calendar structures (working days/hours, holiday effects,
    trading days)
  - Mixed temporal classes (daily and weekly granularities in same
    vector)

Stretch goals for the package include:

  - Representing temporal nesting (2020-01-05 is nested by 2020-01 is
    nested by 2020)
  - `time_join()` operation which respects temporal nesting

## Installation

<!-- You can install the released version of moment from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("moment") -->

<!-- ``` -->

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mitchelloharawild/moment")
```

## Example

``` r
library(moment)
yearmonth(0:11) # By default, time classes have an origin
#> <moment[12]>
#>  [1] 1970 Jan 1970 Feb 1970 Mar 1970 Apr 1970 May 1970 Jun 1970 Jul 1970 Aug
#>  [9] 1970 Sep 1970 Oct 1970 Nov 1970 Dec
yearmonth(0:11) - yearmonth(0) # However some operations can produce moments without origins
#> <moment[12]>
#>  [1] 0 months  1 month   2 months  3 months  4 months  5 months  6 months 
#>  [8] 7 months  8 months  9 months  10 months 11 months

yearquarter(0:5)
#> <moment[6]>
#> [1] 1970 Q1 1970 Q2 1970 Q3 1970 Q4 1971 Q1 1971 Q2
yearquarter(0:5) - yearquarter(0)
#> <moment[6]>
#> [1] 0 quarters 1 quarter  2 quarters 3 quarters 4 quarters 5 quarters
```
