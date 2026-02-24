# Compute circular rolling sums

Calculates rolling sums of length `k` for all contiguous subsequences
around a circular vector. Returns sums for each valid k-element window
that wraps around the vector as if arranged in a circle.

## Usage

``` r
circsum(x, size, step = size)
```

## Arguments

- x:

  A numeric vector to compute circular sums over.

- size:

  Integer; the window size (number of consecutive elements to sum).

- step:

  Integer; the step size (the increment in starting index for each sum).

## Value

A numeric vector containing the sum of each contiguous subsequence
around the circle. The length of the resulting vector is the number of
combinations until the pattern between `x` and `step` repeats

## Examples

``` r
# Simple circular sum with window of 2
circsum(c(1, 2, 3, 4), 2)
#> [1] 3 7
# Returns: 3 7 (1+2, 3+4)

# Window of 3 elements
circsum(c(1, 2, 3, 4, 5), 3)
#> [1]  6 10  9  8 12
# Returns: 6 10 9 8 12 (1+2+3, 4+5+1, 2+3+4, 5+1+2, 3+4+5)
```
