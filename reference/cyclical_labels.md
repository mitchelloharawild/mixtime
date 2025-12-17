# Friendly labels for cyclical relationships

This S7 generic function provides the labels for cyclical relationships
between time units. These functions should return locale specific
labels.

## Usage

``` r
cyclical_labels(granule, cycle, ...)

cyclical_labels.S7_methods(granule, cycle, i)
```

## Arguments

- granule:

  A time unit object representing the granule (e.g., `tu_month(1L)`)

- cycle:

  A time unit object representing the cycle (e.g., `tu_year(1L)`)

- ...:

  Additional arguments for methods.

- i:

  Integer vector representing the position within the cycle.

## Value

Character vector of labels for the time point within the cycle.

## Examples

``` r
# Labels for months in a year
cyclical_labels(tu_month(1L), tu_year(1L), 1:12)
#>  [1] "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" NA   

# Labels for days in a week
cyclical_labels(tu_day(1L), tu_week(1L), 1:7)
#> [1] "Tue" "Wed" "Thu" "Fri" "Sat" "Sun" "Mon"

# Labels for weeks in a year, defaulted from time_unit_abbr()
cyclical_labels(tu_week(1L), tu_year(1L), 1:52)
#>  [1] "W2"  "W3"  "W4"  "W5"  "W6"  "W7"  "W8"  "W9"  "W10" "W11" "W12" "W13"
#> [13] "W14" "W15" "W16" "W17" "W18" "W19" "W20" "W21" "W22" "W23" "W24" "W25"
#> [25] "W26" "W27" "W28" "W29" "W30" "W31" "W32" "W33" "W34" "W35" "W36" "W37"
#> [37] "W38" "W39" "W40" "W41" "W42" "W43" "W44" "W45" "W46" "W47" "W48" "W49"
#> [49] "W50" "W51" "W52" "W53"
```
