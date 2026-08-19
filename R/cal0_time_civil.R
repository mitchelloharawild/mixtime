naive_tz <- naive(NA_character_)

#' @param tz The timezone name for the unit (valid units can be found with `[tzdb::tzdb_names()]`)
#'
#' @rdname mt_unit
#' @export
mt_tz_unit <- new_class(
  "mt_tz_unit",
  parent = mt_unit,
  properties = list(tz = new_property(S7::class_character, default = naive_tz)),
  constructor = function(n = 1L, tz = naive_tz) {
    # Allow for logical NA for convenience
    if (is.na(tz) && !identical(tz, naive_tz)) {
      tz <- NA_character_
    }
    S7::new_object(mt_unit(n = n), tz = tz)
  },
  validator = function(self) {
    if (!is.na(self@tz)) {
      check_tz_name(self@tz)
    }
    NULL
  }
)

# Default formats
method(chronon_format_attr, mt_tz_unit) <- function(x) {
  if (!is.na(x@tz) && x@tz != "UTC") " {tz(.time)}" else ""
}

#' Civil time unit classes
#'
#' Time unit constructors for the civil time system where the boundary of each
#' day is at midnight on the 24 hour clock. This calendar is intended to be
#' built on by other calendars (e.g. `[cal_time_civil]` and `[cal_isoweek]`) to
#' add common time components.  These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @return A time granule object for the civil time system.
#'
#' @details
#' The following time units are available (`cal_time_civil$`).
#'
#' - `day()`: Day unit
#' - `hour()`: Hour unit
#' - `minute()`: Minute unit
#' - `second()`: Second unit
#' - `millisecond()`: Millisecond unit
#' - `microsecond()`: Microsecond unit
#' - `nanosecond()`: Nanosecond unit
#'
#' @seealso [`cal_time_civil`], [`cal_isoweek`]
#'
#' @examples
#' # Create a custom time representation using civil time granules
#' hms <- new_cyclical_time_fn(
#'   chronon = second(1L),
#'   cycle = hour(1L)
#' )
#'
#' @name calendar_time_civil
#' @export
cal_time_civil <- new_calendar(
  day = new_class("tu_day", parent = mt_tz_unit),
  ampm = new_class("tu_ampm", parent = mt_tz_unit),
  hour = new_class("tu_hour", parent = mt_tz_unit),
  minute = new_class("tu_minute", parent = mt_tz_unit),
  second = new_class("tu_second", parent = mt_tz_unit),
  millisecond = new_class("tu_millisecond", parent = mt_tz_unit),
  microsecond = new_class("tu_microsecond", parent = mt_tz_unit),
  nanosecond = new_class("tu_nanosecond", parent = mt_tz_unit),
  class = "cal_time_civil"
)

# Time unit labels
method(time_unit_full, cal_time_civil$day) <- function(x) "day{?/s}"
method(time_unit_abbr, cal_time_civil$day) <- function(x) "D"
method(time_unit_full, cal_time_civil$ampm) <- function(x) "halfday{?/s}"
method(time_unit_abbr, cal_time_civil$ampm) <- function(x) "hd"
method(time_unit_full, cal_time_civil$hour) <- function(x) "hour{?/s}"
method(time_unit_abbr, cal_time_civil$hour) <- function(x) "h"
method(time_unit_full, cal_time_civil$minute) <- function(x) "minute{?/s}"
method(time_unit_abbr, cal_time_civil$minute) <- function(x) "m"
method(time_unit_full, cal_time_civil$second) <- function(x) "second{?/s}"
method(time_unit_abbr, cal_time_civil$second) <- function(x) "s"
method(time_unit_full, cal_time_civil$millisecond) <- function(x) {
  "millisecond{?/s}"
}
method(time_unit_abbr, cal_time_civil$millisecond) <- function(x) "ms"
method(time_unit_full, cal_time_civil$microsecond) <- function(x) {
  "microsecond{?/s}"
}
method(time_unit_abbr, cal_time_civil$microsecond) <- function(x) "us"
method(time_unit_full, cal_time_civil$nanosecond) <- function(x) {
  "nanosecond{?/s}"
}
method(time_unit_abbr, cal_time_civil$nanosecond) <- function(x) "ns"

# Default formats
method(chronon_format_linear, list(cal_time_civil$day, class_any)) <- function(
  x,
  cal
) {
  # `year`/`month` are only in `cal` when the chronon's timezone is
  # resolvable (a Gregorian date is only defined relative to a timezone).
  # Without them, defer to the generic mt_unit fallback rather than
  # referencing calendar units that aren't actually in scope.
  if (is.null(cal$year) || is.null(cal$month)) {
    chronon_format_linear(S7::super(x, mt_unit), cal)
  } else {
    "{lin(year)}-{cyc(month,year)}-{cyc(day, month)}"
  }
}
method(chronon_format_linear, list(cal_time_civil$hour, class_any)) <- function(
  x,
  cal
) {
  paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour, day)}h")
}
method(
  chronon_format_linear,
  list(cal_time_civil$minute, class_any)
) <- function(x, cal) {
  paste(
    chronon_format_linear(cal$day(1L), cal),
    "{cyc(hour, day)}:{cyc(minute, hour)}"
  )
}
method(
  chronon_format_linear,
  list(cal_time_civil$second, class_any)
) <- function(x, cal) {
  paste(
    chronon_format_linear(cal$day(1L), cal),
    "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}"
  )
}
method(
  chronon_format_linear,
  list(cal_time_civil$millisecond, class_any)
) <- function(x, cal) {
  paste(
    chronon_format_linear(cal$day(1L), cal),
    "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}.{cyc(millisecond, second)}"
  )
}
method(
  chronon_format_cyclical,
  list(cal_time_civil$millisecond, cal_time_civil$day)
) <- function(x, y) {
  "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}.{cyc(millisecond, second)}"
}
method(
  chronon_format_cyclical,
  list(cal_time_civil$second, cal_time_civil$day)
) <- function(x, y) "{cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}"
method(
  chronon_format_cyclical,
  list(cal_time_civil$minute, cal_time_civil$day)
) <- function(x, y) "{cyc(hour, day)}:{cyc(minute, hour)}"

## HOURs in DAYs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$hour, cal_time_civil$day)
) <- function(x, y) {
  24L
}

## AMPMs in DAYs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$ampm, cal_time_civil$day)
) <- function(x, y) {
  2L
}

## HOURs in AMPMs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$hour, cal_time_civil$ampm)
) <- function(x, y) {
  12L
}

## MINUTEs in HOURs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$minute, cal_time_civil$hour)
) <- function(x, y) {
  60L
}

## SECONDs in MINUTEs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$second, cal_time_civil$minute)
) <- function(x, y) {
  60L
}

## MILLISECONDs in SECONDs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$millisecond, cal_time_civil$second)
) <- function(x, y) {
  1000L
}

## MICROSECONDs in MILLISECONDs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$microsecond, cal_time_civil$millisecond)
) <- function(x, y) {
  1000L
}

## NANOSECONDs in MICROSECONDs
method(
  chronon_cardinality_fixed,
  list(cal_time_civil$nanosecond, cal_time_civil$microsecond)
) <- function(x, y) {
  1000L
}


# Cyclical labels
method(
  cyclical_labels,
  list(cal_time_civil$day, S7::class_any)
) <- label_scheme(start = 1L, width = 2L)

method(
  cyclical_labels,
  list(cal_time_civil$ampm, S7::class_any)
) <- label_scheme(
  start = 0L,
  vocab = vocab_table(
    `en-GB` = list(wide = c("AM", "PM"), abbreviated = c("AM", "PM"))
  ),
  transform = list(
    encode = function(i, at = NULL) i + 1L,
    decode = function(d, at = NULL) d - 1L
  )
)

# Hours need custom methods for the 12-hour clock's "12,1,2,...,11" wraparound
method(
  cyclical_labels_format,
  list(cal_time_civil$hour, S7::class_any)
) <- function(granule, cycle, i, at = NULL, ...) {
  if (S7_inherits(cycle, cal_time_civil$ampm)) {
    # 12 hours count with 12,1,2,...,11
    sprintf("%02d", (i - 1L) %% 12L + 1L)
  } else {
    # 24 hours count with 0-indexing
    sprintf("%02d", i)
  }
}
method(
  cyclical_labels_parse,
  list(cal_time_civil$hour, S7::class_any)
) <- function(granule, cycle, ...) {
  list(
    pattern = "\\d+",
    decode = function(text, at = NULL) {
      d <- as.integer(text)
      if (S7_inherits(cycle, cal_time_civil$ampm)) d %% 12L else d
    }
  )
}

# Minutes/seconds/milliseconds count with 0-indexing -- plain numeric (tier 1).
method(
  cyclical_labels,
  list(cal_time_civil$minute, S7::class_any)
) <- label_scheme(start = 0L, width = 2L)
method(
  cyclical_labels,
  list(cal_time_civil$second, S7::class_any)
) <- label_scheme(start = 0L, width = 2L)
method(
  cyclical_labels,
  list(cal_time_civil$millisecond, S7::class_any)
) <- label_scheme(start = 0L, width = 3L)
