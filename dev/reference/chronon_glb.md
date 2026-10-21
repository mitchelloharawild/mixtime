# Find the finest or coarsest common chronon of a time object

These utility functions take a set of chronons and identify a common
chronon that can represent all input chronons without loss of
information, using the ordered relationships defined by
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
methods. This is useful for operations that require a shared time
granule, such as combining or comparing different time measured at
different precisions.

`chronon_glb()` finds the greatest lower bound (GLB): the common chronon
of the *finest* granularity that can represent all input chronons
without loss of information.

`chronon_lub()` finds the least upper bound (LUB): the common chronon of
the *coarsest* granularity that every input chronon evenly aggregates
into - the dual of `chronon_glb()`.

**\[deprecated\]**

`chronon_common()` was renamed to `chronon_glb()` to distinguish it from
its dual, `chronon_lub()`.

## Usage

``` r
chronon_glb(x, ...)

chronon_glb.mixtime(x, .ptype = NULL, ...)

chronon_lub(x, ...)

chronon_lub.mixtime(x, .ptype = NULL, ...)

chronon_common(x, .ptype = NULL, ...)
```

## Arguments

- x:

  A time object (typically a
  [`mixtime`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)).

- ...:

  Additional arguments for methods.

- .ptype:

  If NULL, the default, the output returns the common chronon across all
  chronons of `x`. Alternatively, a prototype chronon can be supplied to
  `.ptype` to demand a specific chronon is used. If the supplied
  `.ptype` cannot represent all input chronons without loss of
  information, an error is raised.

## Value

A time granule object representing the common chronon.

## Examples

``` r
# The finest common chronon between a year-month and a day is a day
chronon_glb(c(yearmonth(Sys.Date()), date(Sys.Date())))
#> <mixtime::tu_day>
#>  @ n       : int 1
#>  @ calendar: <mixtime::cal_gregorian> List of 11
#>  .. $ year       : <mixtime::tu_year/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ quarter    : <mixtime::tu_quarter/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ month      : <mixtime::tu_month/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ day        : <mixtime::tu_day/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ ampm       : <mixtime::tu_ampm/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ hour       : <mixtime::tu_hour/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ minute     : <mixtime::tu_minute/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ second     : <mixtime::tu_second/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ millisecond: <mixtime::tu_millisecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ microsecond: <mixtime::tu_microsecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ nanosecond : <mixtime::tu_nanosecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  @ tz      : chr NA

# The finest common chronon between a Gregorian month and an ISO week is a day
chronon_glb(c(yearweek(Sys.Date()), yearmonth(Sys.Date())))
#> <mixtime::tu_day>
#>  @ n       : int 1
#>  @ calendar: <mixtime::cal_gregorian> List of 11
#>  .. $ year       : <mixtime::tu_year/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ quarter    : <mixtime::tu_quarter/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ month      : <mixtime::tu_month/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ day        : <mixtime::tu_day/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ ampm       : <mixtime::tu_ampm/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ hour       : <mixtime::tu_hour/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ minute     : <mixtime::tu_minute/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ second     : <mixtime::tu_second/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ millisecond: <mixtime::tu_millisecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ microsecond: <mixtime::tu_microsecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ nanosecond : <mixtime::tu_nanosecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  @ tz      : chr NA

# The finest common chronon between a ISO week and an hour is an hour
chronon_glb(c(yearweek(Sys.Date()), linear_time(Sys.time(), hour(1L))))
#> <mixtime::tu_hour>
#>  @ n       : int 1
#>  @ calendar: <mixtime::cal_time_civil> List of 8
#>  .. $ day        : <mixtime::tu_day/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ ampm       : <mixtime::tu_ampm/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ hour       : <mixtime::tu_hour/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ minute     : <mixtime::tu_minute/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ second     : <mixtime::tu_second/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ millisecond: <mixtime::tu_millisecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ microsecond: <mixtime::tu_microsecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ nanosecond : <mixtime::tu_nanosecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  @ tz      : chr NA

# The coarsest common chronon between a second and a minute is a minute
chronon_lub(c(linear_time(Sys.time(), second(1L)), linear_time(Sys.time(), minute(1L))))
#> <mixtime::tu_minute>
#>  @ n       : int 1
#>  @ calendar: <mixtime::cal_gregorian> List of 11
#>  .. $ year       : <mixtime::tu_year/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ quarter    : <mixtime::tu_quarter/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ month      : <mixtime::tu_month/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ day        : <mixtime::tu_day/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ ampm       : <mixtime::tu_ampm/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ hour       : <mixtime::tu_hour/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ minute     : <mixtime::tu_minute/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ second     : <mixtime::tu_second/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ millisecond: <mixtime::tu_millisecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ microsecond: <mixtime::tu_microsecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  .. $ nanosecond : <mixtime::tu_nanosecond/mixtime::mt_tz_unit/mixtime::mt_unit/S7_object> constructor
#>  @ tz      : chr "UTC"
```
