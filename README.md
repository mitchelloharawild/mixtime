
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

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mitchelloharawild/moment")
```

## Example

``` r
library(moment)
```
