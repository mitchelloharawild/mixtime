# Constructor for mixtime time vectors

**\[defunct\]**

`new_time()` was the low-level constructor for `mt_time` vectors. It is
now defunct; call the concrete time class constructors directly instead:
[`mt_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md)
for linear time,
[`mt_duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md)
for durations, and
[`mt_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md)
for cyclical time.

## Usage

``` r
new_time(x = integer(), chronon = mt_unit(1L), cycle = NULL, class = NULL)
```

## Arguments

- x, chronon, cycle, class:

  Ignored.
