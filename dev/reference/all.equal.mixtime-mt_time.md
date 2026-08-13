# Tolerant comparison of mixtime time values

[vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html)'s
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) method (which
`mixtime` inherits, since a mixtime vector is a `vecvec`) handles
length/`NA` mismatches and groups elements by their underlying storage
slot, then compares each group's *raw* values with
[`all.equal()`](https://rdrr.io/r/base/all.equal.html). That raw
comparison has no notion of chronon: two elements that are `==` (e.g.
`days(1)` and `hours(24)`, or a `yearmonth` and an equivalent-instant
`yearweek`) can end up stored with different chronons or magnitudes, and
so are wrongly reported as unequal.

This method fixes that at the per-slot level: elements already `==`
(which is chronon-aware, see
[mt_linear-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_linear-compare.md)/[mt_duration-compare](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_duration-compare.md))
count as equal outright; for the rest, the discrepancy is measured as
the duration between them in their common chronon. A time point has no
"typical magnitude" to scale a *relative* tolerance against the way a
plain number does, so - unlike
[`base::all.equal.numeric()`](https://rdrr.io/r/base/all.equal.html) -
`tolerance` is always absolute, in (possibly fractional) chronon units.

## Usage

``` r
# S3 method for class '`mixtime::mt_time`'
all.equal(target, current, tolerance = sqrt(.Machine$double.eps), ...)
```

## Arguments

- target, current:

  `mt_time` vectors of the same length to compare (as passed down by
  [vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html)'s
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html)).

- tolerance:

  Numeric tolerance, as an absolute number of `target`'s chronon units.
  Defaults to `sqrt(.Machine$double.eps)`, as for
  [`base::all.equal()`](https://rdrr.io/r/base/all.equal.html).

- ...:

  Ignored.

## Value

`TRUE` if `target` and `current` are equal within `tolerance`, otherwise
a string describing the discrepancy.

## Examples

``` r
all.equal(yearmonth(0), yearmonth(0))
#> [1] TRUE
all.equal(yearmonth(0), yearmonth(1))
#> Error in `method(/, list(mixtime::mt_time, class_any))`(e1 = structure(1, class = c("mixtime::mt_linear", "mixtime::mt_time", "mt_time_data", "S7_object"), S7_class = structure(function (.data = integer(),     chronon = mt_unit(1L)) S7::new_object(mt_time(.data = .data, chronon = chronon)), name = "mt_linear", parent = structure(function (.data = integer(),     chronon = mt_unit(1L)) S7::new_object(new_mt_time_data(.data = .data), chronon = chronon), name = "mt_time", parent = structure(list(    class = "mt_time_data", constructor = function (.data = integer())     {        structure(.data, class = "mt_time_data")    }, validator = function (self)     {        if (!is.numeric(self)) {            cli::cli_abort("{.var self} must be an integer or double vector.",                 call. = FALSE)        }    }), class = "S7_S3_class"), package = "mixtime", properties = list(    chronon = structure(list(name = "chronon", class = structure(function (n = 1L)     {        n        S7::new_object(S7::S7_object(), n = n)    }, name = "mt_unit", parent = structure(function ()     {        .Call(S7_object_)    }, name = "S7_object", properties = list(), abstract = FALSE, constructor = function ()     {        .Call(S7_object_)    }, validator = function (self)     {        if (!is_S7_type(self)) {            "Underlying data is corrupt"        }    }, class = c("S7_class", "S7_object")), package = "mixtime", properties = list(        n = structure(list(name = "n", class = structure(list(            classes = list(structure(list(class = "integer",                 constructor_name = "integer", constructor = function (.data = integer(0))                 .data, validator = function (object)                 {                  if (base_class(object) != name) {                    sprintf("Underlying data must be <%s> not <%s>",                       name, base_class(object))                  }                }), class = "S7_base_class"), structure(list(                class = "double", constructor_name = "double",                 constructor = function (.data = numeric(0))                 .data, validator = function (object)                 {                  if (base_class(object) != name) {                    sprintf("Underlying data must be <%s> not <%s>",                       name, base_class(object))                  }                }), class = "S7_base_class"))), class = "S7_union"),             getter = NULL, setter = NULL, validator = NULL, default = 1L), class = "S7_property")), abstract = FALSE, constructor = function (n = 1L)     {        n        S7::new_object(S7::S7_object(), n = n)    }, class = c("S7_class", "S7_object")), getter = NULL, setter = NULL,         validator = function (value)         {            if (length(value@n) != 1L) {                "must wrap a single time granule (its `@n` must be length 1)"            }        }, default = mt_unit(1L)), class = "S7_property")), abstract = FALSE, constructor = function (.data = integer(),     chronon = mt_unit(1L)) S7::new_object(new_mt_time_data(.data = .data), chronon = chronon), class = c("S7_class", "S7_object")), package = "mixtime", properties = list(chronon = structure(list(    name = "chronon", class = structure(function (n = 1L)     {        n        S7::new_object(S7::S7_object(), n = n)    }, name = "mt_unit", parent = structure(function ()     {        .Call(S7_object_)    }, name = "S7_object", properties = list(), abstract = FALSE, constructor = function ()     {        .Call(S7_object_)    }, validator = function (self)     {        if (!is_S7_type(self)) {            "Underlying data is corrupt"        }    }, class = c("S7_class", "S7_object")), package = "mixtime", properties = list(        n = structure(list(name = "n", class = structure(list(            classes = list(structure(list(class = "integer",                 constructor_name = "integer", constructor = function (.data = integer(0))                 .data, validator = function (object)                 {                  if (base_class(object) != name) {                    sprintf("Underlying data must be <%s> not <%s>",                       name, base_class(object))                  }                }), class = "S7_base_class"), structure(list(                class = "double", constructor_name = "double",                 constructor = function (.data = numeric(0))                 .data, validator = function (object)                 {                  if (base_class(object) != name) {                    sprintf("Underlying data must be <%s> not <%s>",                       name, base_class(object))                  }                }), class = "S7_base_class"))), class = "S7_union"),             getter = NULL, setter = NULL, validator = NULL, default = 1L), class = "S7_property")), abstract = FALSE, constructor = function (n = 1L)     {        n        S7::new_object(S7::S7_object(), n = n)    }, class = c("S7_class", "S7_object")), getter = NULL, setter = NULL,     validator = function (value)     {        if (length(value@n) != 1L) {            "must wrap a single time granule (its `@n` must be length 1)"        }    }, default = mt_unit(1L)), class = "S7_property")), abstract = FALSE, constructor = function (.data = integer(),     chronon = mt_unit(1L)) S7::new_object(mt_time(.data = .data, chronon = chronon)), class = c("S7_class", "S7_object")), chronon = <object>), e2 = 1, ...): Division is only supported between two durations, or a duration divided
#> by a number.
all.equal(days(1), hours(24))
#> [1] TRUE
```
