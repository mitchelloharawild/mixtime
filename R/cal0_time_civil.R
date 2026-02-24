#' @rdname mt_unit
#' @export
mt_tz_unit <- new_class(
  "mt_tz_unit", 
  parent = mt_unit,
  properties = list(tz = new_property(S7::class_character, default = "UTC")),
  validator = function(self) {
    check_tz_name(self@tz)
    NULL
  }
)

#' Civil time unit classes
#'
#' Time unit constructors for the civil time system where the boundary of each
#' day is at midnight on the 24 hour clock. This calendar is intended to be
#' built on by other calendars (e.g. `[cal_time_civil_midnight]` and `[cal_isoweek]`) to
#' add common time components.  These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @return A time unit object for the civil time system.
#' 
#' @details
#' The following time units are available (`cal_time_civil_midnight$`).
#' 
#' - `day()`: Day unit
#' - `hour()`: Hour unit
#' - `minute()`: Minute unit
#' - `second()`: Second unit
#' - `millisecond()`: Millisecond unit
#' 
#' @seealso [`cal_time_civil_midnight`], [`cal_isoweek`]
#' 
#' @examples
#' # Create a custom time representation using Gregorian units
#' hms <- new_cyclical_time_fn(
#'   chronon = second(1L),
#'   cycle = hour(1L)
#' )
#' 
#' @name calendar_time_civil
#' @export
cal_time_civil_midnight <- new_calendar(
  day = new_class("tu_day", parent = mt_tz_unit),
  hour = new_class("tu_hour", parent = mt_tz_unit),
  minute = new_class("tu_minute", parent = mt_tz_unit),
  second = new_class("tu_second", parent = mt_tz_unit),
  millisecond = new_class("tu_millisecond", parent = mt_tz_unit),
  class = "cal_time_civil_midnight"
)


# Time unit labels
method(time_unit_full, cal_time_civil_midnight$day) <- function(x) "day"
method(time_unit_abbr, cal_time_civil_midnight$day) <- function(x) "D"
method(time_unit_full, cal_time_civil_midnight$hour) <- function(x) "hour"
method(time_unit_abbr, cal_time_civil_midnight$hour) <- function(x) "h"
method(time_unit_full, cal_time_civil_midnight$minute) <- function(x) "minute"
method(time_unit_abbr, cal_time_civil_midnight$minute) <- function(x) "m"
method(time_unit_full, cal_time_civil_midnight$second) <- function(x) "second"
method(time_unit_abbr, cal_time_civil_midnight$second) <- function(x) "s"
method(time_unit_full, cal_time_civil_midnight$millisecond) <- function(x) "millisecond"
method(time_unit_abbr, cal_time_civil_midnight$millisecond) <- function(x) "ms"

# Default granules
method(chronon_granules, cal_time_civil_midnight$day) <- function(x) list(cal_gregorian$year(1L), cal_gregorian$month(1L))
method(chronon_granules, cal_time_civil_midnight$hour) <- function(x) list(cal_gregorian$year(1L), cal_gregorian$month(1L), cal_gregorian$day(1L))
method(chronon_granules, cal_time_civil_midnight$minute) <- function(x) list(cal_gregorian$year(1L), cal_gregorian$month(1L), cal_gregorian$day(1L), cal_gregorian$hour(1L))
method(chronon_granules, cal_time_civil_midnight$second) <- function(x) list(cal_gregorian$year(1L), cal_gregorian$month(1L), cal_gregorian$day(1L), cal_gregorian$hour(1L), cal_gregorian$minute(1L))
method(chronon_granules, cal_time_civil_midnight$millisecond) <- function(x) list(cal_gregorian$year(1L), cal_gregorian$month(1L), cal_gregorian$day(1L), cal_gregorian$hour(1L), cal_gregorian$minute(1L), cal_gregorian$second(1L))

## DAY <-> HOUR
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$day, cal_time_civil_midnight$hour)
) <- function(x, y, at = NULL) {
  vec_data(x)*24/vec_data(y)
}

## HOUR <-> MINUTE
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$hour, cal_time_civil_midnight$minute)
) <- function(x, y, at = NULL) {
  vec_data(x)*60/vec_data(y)
}

## MINUTE <-> SECOND
method(
  chronon_cardinality,
  list(cal_time_civil_midnight$minute, cal_time_civil_midnight$second)
) <- function(x, y, at = NULL) {
  vec_data(x)*60/vec_data(y)
}

## SECOND <-> MILLISECOND
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$second, cal_time_civil_midnight$millisecond)
) <- function(x, y, at = NULL) {
  vec_data(x)*1000/vec_data(y)
}


# Cyclical labels
method(cyclical_labels, list(cal_time_civil_midnight$day, S7::class_any)) <- function(granule, cycle, i) {
  # Days count with 1-indexing
  i + 1L
}
