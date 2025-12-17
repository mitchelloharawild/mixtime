# Create a temporal durations from time units

**\[experimental\]**

Constructs a new duration object (`mt_duration`) from a set of mixtime
time units. This functionality is in active development and is not ready
for use.

## Usage

``` r
new_duration(...)
```

## Arguments

- ...:

  A set of time unit objects (e.g., `tu_year(1)`, `tu_month(2)`, etc.)

## Value

An object of class `mt_duration` representing the specified duration.

## Details

This is a low-level constructor function that creates duration objects.
