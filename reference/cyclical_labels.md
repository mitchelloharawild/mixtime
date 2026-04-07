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

  A time unit object representing the granule (e.g., `month(1L)`)

- cycle:

  A time unit object representing the cycle (e.g., `year(1L)`)

- ...:

  Additional arguments for methods.

- i:

  Integer vector representing the position within the cycle.

## Value

Character vector of labels for the time point within the cycle.

## Examples

``` r
# Labels for months in a year
with(cal_gregorian, cyclical_labels(month(1L), year(1L), 1:12))
#>  [1] "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13"

# Labels for days in a week
with(cal_isoweek, cyclical_labels(day(1L), week(1L), 1:7))
#> [1] "2" "3" "4" "5" "6" "7" "8"

# Labels for weeks in a year, defaulted from time_unit_abbr()
with(cal_isoweek, cyclical_labels(week(1L), year(1L), 1:52))
#>  [1] "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16"
#> [16] "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31"
#> [31] "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46"
#> [46] "47" "48" "49" "50" "51" "52" "53"
```
