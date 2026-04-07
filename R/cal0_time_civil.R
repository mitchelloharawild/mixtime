#' @rdname mt_unit
#' @export
mt_tz_unit <- new_class(
  "mt_tz_unit", 
  parent = mt_unit,
  properties = list(tz = new_property(S7::class_character, default = "")),
  validator = function(self) {
    if (nzchar(self@tz)) check_tz_name(self@tz)
    NULL
  }
)

# Default formats
method(chronon_format_attr, mt_tz_unit) <- function(x) {
  if (x@tz != "UTC") " {tz(.time)}" else ""
}

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
  ampm = new_class("tu_ampm", parent = mt_tz_unit),
  hour = new_class("tu_hour", parent = mt_tz_unit),
  minute = new_class("tu_minute", parent = mt_tz_unit),
  second = new_class("tu_second", parent = mt_tz_unit),
  millisecond = new_class("tu_millisecond", parent = mt_tz_unit),
  class = "cal_time_civil_midnight"
)


# Time unit labels
method(time_unit_full, cal_time_civil_midnight$day) <- function(x) "day"
method(time_unit_abbr, cal_time_civil_midnight$day) <- function(x) "D"
method(time_unit_full, cal_time_civil_midnight$ampm) <- function(x) "halfday"
method(time_unit_abbr, cal_time_civil_midnight$ampm) <- function(x) "hd"
method(time_unit_full, cal_time_civil_midnight$hour) <- function(x) "hour"
method(time_unit_abbr, cal_time_civil_midnight$hour) <- function(x) "h"
method(time_unit_full, cal_time_civil_midnight$minute) <- function(x) "minute"
method(time_unit_abbr, cal_time_civil_midnight$minute) <- function(x) "m"
method(time_unit_full, cal_time_civil_midnight$second) <- function(x) "second"
method(time_unit_abbr, cal_time_civil_midnight$second) <- function(x) "s"
method(time_unit_full, cal_time_civil_midnight$millisecond) <- function(x) "millisecond"
method(time_unit_abbr, cal_time_civil_midnight$millisecond) <- function(x) "ms"

# Default formats
method(chronon_format_linear, list(cal_time_civil_midnight$day, class_any)) <- function(x, cal) "{lin(year)}-{cyc(month,year)}-{cyc(day, month)}"
method(chronon_format_linear, list(cal_time_civil_midnight$hour, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour, day)}h")
method(chronon_format_linear, list(cal_time_civil_midnight$minute, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour, day)}:{cyc(minute, hour)}")
method(chronon_format_linear, list(cal_time_civil_midnight$second, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}")
method(chronon_format_linear, list(cal_time_civil_midnight$millisecond, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}.{cyc(millisecond, second)}")
method(chronon_format_cyclical, list(cal_time_civil_midnight$millisecond, cal_time_civil_midnight$day)) <- function(x, y) "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}.{cyc(millisecond, second)}"
method(chronon_format_cyclical, list(cal_time_civil_midnight$second, cal_time_civil_midnight$day)) <- function(x, y) "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}"
method(chronon_format_cyclical, list(cal_time_civil_midnight$minute, cal_time_civil_midnight$day)) <- function(x, y) "{cyc(hour, day)}:{cyc(minute, hour)}"

## HOURs in DAYs
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$hour, cal_time_civil_midnight$day)
) <- function(x, y, at = NULL) {
  vec_data(y)*24L/vec_data(x)
}

## AMPMs in DAYs
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$ampm, cal_time_civil_midnight$day)
) <- function(x, y, at = NULL) {
  vec_data(y)*2L/vec_data(x)
}

## HOURs in AMPMs
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$hour, cal_time_civil_midnight$ampm)
) <- function(x, y, at = NULL) {
  vec_data(y)*12L/vec_data(x)
}

## MINUTEs in HOURs
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$minute, cal_time_civil_midnight$hour)
) <- function(x, y, at = NULL) {
  vec_data(y)*60L/vec_data(x)
}

## SECONDs in MINUTEs
method(
  chronon_cardinality,
  list(cal_time_civil_midnight$second, cal_time_civil_midnight$minute)
) <- function(x, y, at = NULL) {
  vec_data(y)*60L/vec_data(x)
}

## MILLISECONDs in SECONDs
method(
  chronon_cardinality, 
  list(cal_time_civil_midnight$millisecond, cal_time_civil_midnight$second)
) <- function(x, y, at = NULL) {
  vec_data(y)*1000L/vec_data(x)
}


# Cyclical labels
method(cyclical_labels, list(cal_time_civil_midnight$day, S7::class_any)) <- function(granule, cycle, i) {
  # Days count with 1-indexing
  sprintf("%02d", i + 1L)
}
method(cyclical_labels, list(cal_time_civil_midnight$ampm, S7::class_any)) <- function(granule, cycle, i) {
  # 0 is AM, 1 is PM
  c("AM", "PM")[i%%2 + 1L]
}
method(cyclical_labels, list(cal_time_civil_midnight$hour, S7::class_any)) <- function(granule, cycle, i) {
  if (S7::S7_inherits(cycle, cal_time_civil_midnight$ampm)) {
    # 12 hours count with 12,1,2,...,11
    sprintf("%02d", (i-1L)%%12L + 1L)
  } else {
    # 24 hours count with 0-indexing
    sprintf("%02d", i)
  }
}
method(cyclical_labels, list(cal_time_civil_midnight$minute, S7::class_any)) <- function(granule, cycle, i) {
  # Minutes count with 0-indexing
  sprintf("%02d", i)
}
method(cyclical_labels, list(cal_time_civil_midnight$second, S7::class_any)) <- function(granule, cycle, i) {
  # Seconds count with 0-indexing
  sprintf("%02d", i)
}
method(cyclical_labels, list(cal_time_civil_midnight$millisecond, S7::class_any)) <- function(granule, cycle, i) {
  # Milliseconds are shown without decimals
  sprintf("%03d", i)
}