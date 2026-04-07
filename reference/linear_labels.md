# Friendly labels for linear relationships

This S7 generic function provides the labels for linear (non-repeating)
positions of a time unit. These functions should return locale specific
labels.

## Usage

``` r
linear_labels(granule, ...)

linear_labels.S7_methods(granule, i)
```

## Arguments

- granule:

  A time unit object representing the granule (e.g., `year(1L)`)

- ...:

  Additional arguments for methods.

- i:

  Integer vector representing the position along the linear axis.

## Value

Character vector of labels for the time point.

## Examples

``` r
# Labels for years on a linear axis
with(cal_gregorian, linear_labels(year(1L), 2020:2025))
#> [1] "2020" "2021" "2022" "2023" "2024" "2025"
```
