#' Base S7 class for creating new time units
#' 
#' This class is the primative class for time units, and should 
#' be extended from when creating new time units. A new class
#' is typically created with S7 using: 
#' `S7::new_class("tu_***", parent = mt_tz_unit)`
#' 
#' Time units are the building blocks of calendars in mixtime. Each unit
#' represents a specific temporal component (e.g., day, month, year) and
#' can be combined using [new_calendar()] to create a calendar system.
#' 
#' When creating custom calendars, define time unit classes that inherit
#' from either `mt_unit` (for standard units) or `mt_tz_unit` (for
#' timezone-aware units), then pass them as named arguments to
#' [new_calendar()]. The calendar will use these names to create
#' constructor functions accessible via `$` notation (e.g., `calendar$day(1L)`).
#' 
#' @section Calendar Algebra Methods:
#'
#' Time units enable calendar arithmetic through key generic methods
#' that should be implemented for custom time units:
#'
#' * `chronon_cardinality_fixed(x, y)` - Returns the number of unit `x`
#'   granules that fit within one unit `y` granule, for relationships that are
#'   a constant, context-independent number (e.g., 7 days per week, 24 hours
#'   per day). Prefer this over `chronon_cardinality()` whenever the
#'   relationship does not depend on `at`, since it is also used to determine
#'   which relationships are safe to use for [chronon_divmod()]'s graph
#'   traversal.
#'
#' * `chronon_cardinality(x, y, at)` - Returns the number of `x` granule
#'   that fit within one `y` granule. This is variable based on `at` (e.g.,
#'   28-31 days per month). Only implement this directly (rather than
#'   `chronon_cardinality_fixed()`) when the relationship genuinely depends on
#'   `at`, and pair it with a direct `chronon_divmod()` method so that the
#'   relationship remains reachable during graph traversal.
#'
#' * `chronon_divmod(x, from, to)` - Converts time point `x` from granules of
#'   `from` to granules of `to`, returning a list with `div` (the quotient)
#'   and `mod`. This enables conversions between granules that have
#'   variable cardinality (e.g., the date 2020-03-23 to the month 2020-03).
#'   All conversions should be based on chronons since epoch (1970-01-01),
#'   in the UTC time zone.
#' 
#' These methods work together to enable mixtime to perform calendar-aware
#' arithmetic, understanding that months have variable lengths and handling
#' timezone-aware conversions.
#' 
#' @param n The step size of time granule. For example, `n = 2L` is 2 time 
#'   units, and `cal_isoweek$week(2L)` would represent 2 weeks (a fortnight).
#' 
#' @return A time granule object of class `mt_unit`
#' 
#' @seealso [new_calendar()] for creating calendars from time units
#' 
#' @examples
#' # Create a timezone-aware unit class
#' 
#' # Use these units to create a calendar
#' my_calendar <- new_calendar(
#'   day = S7::new_class("tu_my_day", parent = mt_unit),
#'   month = S7::new_class("tu_my_month", parent = mt_tz_unit),
#'   class = "my_calendar"
#' )
#' 
#' # Access unit constructors from the calendar
#' my_calendar$day(1L)
#' my_calendar$month(3L, tz = "America/New_York")
#' 
#' @export
#' @rdname mt_unit
mt_unit <- S7::new_class(
  "mt_unit", 
  properties = list(
    n = S7::new_property(
      class = S7::class_numeric,
      default = 1L
    )
  ),
  # validator = function(self) {
  #   if (length(self@n) != 1L) {
  #     paste0("@n must be length 1 <numeric> value, not length ", length(self@n), ".")
  #   }
  # }
)

#' Time vector classes
#'
#' @description
#' The `mt_time` family are the S7 vector classes that store time points as a
#' numeric count of chronons. `mt_time` is the (internal) base class carrying 
#' the `chronon` property; the common modes of time are:
#'
#' * `mt_linear` - linear time points, typically produced with [linear_time()].
#' * `mt_cyclical` - cyclical time points with additional `cycle` granule, 
#'   typically produced with [cyclical_time()].
#' * `mt_duration` - time durations, typically produced with [duration()].
#'
#' The underlying data can be either **integer** (discrete time) or **double**
#' (continuous time). The `chronon` (and, for `mt_cyclical`, the `cycle`) are 
#' time granules, the result from a [mt_unit] object.
#'
#' @param x A numeric vector of chronon counts (integer or double).
#'
#' @return An S7 class object (used for method dispatch), or a time vector when
#'   called as a constructor.
#'
#' @seealso [mt_linear()], [mt_duration()], and [mt_cyclical()] to construct
#'   time vectors, and [mt_unit] for the granule type stored in `chronon`/`cycle`.
#'
#' @name mt_time-class
NULL

# Lightweight S3 carrier for the mode of time (integer or double). It exists
# *only* so S7 accepts either an integer or double inputs (an S7 base-type
# class fixes a single base type).
mt_data <- S7::new_S3_class(
  "mt_time_data",
  constructor = function(.data = integer()) {
    structure(.data, class = "mt_time_data")
  },
  validator = function(self) {
    if (!is.numeric(self)) {
      cli::cli_abort("{.var self} must be an integer or double vector.", call. = FALSE)
    }
  }
)

granule_len1 <- function(value) {
  if (length(value@n) != 1L) {
    "must wrap a single time granule (its `@n` must be length 1)"
  }
}

# The vector behaviour of `mt_time` (`[`, `c()`, `rep()`, comparisons, arithmetic) is
# provided by S7 methods registered directly on the S7 classes below - see vctrs.R (the
# base-R `S7_data()`-swap methods and comparisons) and arithmetic.R (native `+`/`-`/`*`/`/`
# operator methods). The double-dispatch vctrs coercion generics (`vec_cast`/`vec_ptype2`)
# ignore inheritance, so they are registered on the package-namespaced concrete class names
# in `register_mt_vctrs()` (vctrs.R); `vec_restore`/`vec_math` are S7 methods on `mt_time`.

#' @rdname mt_time-class
#' @export
mt_time <- S7::new_class(
  "mt_time",
  parent = mt_data,
  properties = list(
    chronon = S7::new_property(class = mt_unit, default = mt_unit(1L), validator = granule_len1)
  )
)

#' @rdname mt_time-class
#' @export
mt_linear <- S7::new_class("mt_linear", parent = mt_time)

#' @rdname mt_time-class
#' @export
mt_duration <- S7::new_class("mt_duration", parent = mt_time)

#' @rdname mt_time-class
#' @export
mt_cyclical <- S7::new_class(
  "mt_cyclical",
  parent = mt_time,
  properties = list(
    cycle = S7::new_property(class = mt_unit, default = mt_unit(1L), validator = granule_len1)
  )
)

#' Base S7 class for mixtime vector objects
#'
#' `class_mixtime` is the base S7 class for all mixtime vector objects,
#' inheriting from [vecvec::class_vecvec]. While not intended to be used
#' directly, this S7 class is suitable to use when defining S7 methods for
#' mixtime vectors. S3 methods can be defined using the `mixtime::mixtime` 
#' class.
#' 
#' @param x A list of `"mt_time"` vectors, see [new_time()] for details.
#' @inheritParams vecvec::class_vecvec
#'
#' @return When used as a class definition (e.g., in `S7::method(generic,
#'   class_mixtime)`), an S7 class object representing the `mixtime` class,
#'   inheriting from [vecvec::class_vecvec]. When called as a constructor
#'   (`class_mixtime(list(...))`), a mixtime vector of S7 class `mixtime`
#'   (also inheriting the S3 class `"mixtime"`), containing the supplied list
#'   of time vectors as a [vecvec::class_vecvec] structure. End users should
#'   prefer [mixtime()] or [new_mixtime()] for construction.
#'
#' @seealso [mixtime()] for creating mixtime vectors, and [new_mixtime()] for
#' the low-level constructor function of this S7 class.
#'
#' @importFrom vecvec class_vecvec
#' @export
class_mixtime <- S7::new_class(
  "mixtime",
  parent = class_vecvec
)