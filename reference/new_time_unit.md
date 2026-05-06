# Create a new time unit class

Define a new S7 class representing a time unit for use in a mixtime
calendar. Time units are the building blocks of calendars: each unit
represents a specific temporal component (e.g., day, month, year) and
carries a step size `n`. Units are combined via
[`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md)
to form a complete calendar system.

## Usage

``` r
new_time_unit(
  name,
  parent = mt_unit,
  package = topNamespaceName(parent.frame()),
  properties = list(),
  abstract = FALSE,
  constructor = NULL,
  validator = NULL
)
```

## Arguments

- name:

  A string naming the new S7 class (e.g., `"tu_my_year"`). By
  convention, time unit class names are prefixed with `tu_`.

- parent:

  The parent S7 class. Should be one of
  [`mt_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  [`mt_tz_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  or
  [`mt_loc_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  or a class that itself inherits from one of these. Defaults to
  [`mt_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md).

- package:

  A string giving the package name that owns the class. Defaults to the
  name of the calling namespace, so typically does not need to be set
  explicitly.

- properties:

  A named list of additional
  [`S7::new_property()`](https://rconsortium.github.io/S7/reference/new_property.html)
  definitions beyond those inherited from `parent`. Each entry becomes a
  slot on instances of the new class. Defaults to an empty list.

- abstract:

  Logical. If `TRUE` the class cannot be instantiated directly; it
  serves only as a base for further subclassing.

- constructor:

  A function to use as the class constructor, or `NULL` (default) to
  generate one automatically. The auto-generated constructor accepts
  `...` (forwarded to the parent constructor) plus one argument per
  entry in `properties`, with defaults taken from each property's
  `default` field.

- validator:

  A function of one argument (`self`) that returns `NULL` when the
  object is valid, or a character string describing the problem. See
  [`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html)
  for details.

## Value

An S7 class object, as returned by
[`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html).

## Details

Choose the parent class based on the type of time the unit represents:

- [`mt_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  — abstract or calendar-only time (no timezone or location context;
  e.g., Gregorian year or ISO week).

- [`mt_tz_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  — civil time with a timezone (e.g., a clock hour that needs `tz` to
  resolve wall-clock ambiguity).

- [`mt_loc_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  — astronomical time with a geographic location (e.g., a solar day tied
  to observer longitude/latitude).

## See also

- [`mt_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  [`mt_tz_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md),
  [`mt_loc_unit`](https://pkg.mitchelloharawild.com/mixtime/reference/mt_unit.md)
  for the base classes to inherit from.

- [`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/reference/new_calendar.md)
  for assembling time units into a calendar.

- [`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html)
  for the underlying S7 class constructor.

## Examples

``` r
# An abstract unit with no properties
tu_my_seq <- new_time_unit("tu_my_seq", parent = mt_unit)

# A civil-time unit with an extra property
tu_my_quarter <- new_time_unit(
  "tu_my_quarter",
  parent = mt_tz_unit,
  properties = list(
    fiscal = S7::new_property(S7::class_logical, default = FALSE)
  )
)
```
