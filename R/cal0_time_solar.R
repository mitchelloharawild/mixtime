#' @rdname mt_unit
#' @export
mt_loc_unit <- S7::new_class(
  "mt_loc_unit", 
  parent = mt_unit,
  properties = list(
    lat = S7::new_property(S7::class_numeric, default = 0),
    lon = S7::new_property(S7::class_numeric, default = 0),
    alt = S7::new_property(S7::class_numeric, default = 0)
  ),
  validator = function(self) {
    NULL
  }
)

#' Solar time unit classes
#'
#' Time unit constructors for the solar time system where the boundary of each
#' day is at sunrise, sunset, or noon. This calendar is intended to be
#' built on by other calendars to add common time components.
#'
#' @return A time unit object for the Gregorian calendar system.
#' 
#' @details
#' The following time units are available in the solar calendar systems.
#' 
#' - `day()`: Day unit
#' 
#' @seealso [`cal_time_civil_midnight`]
#' 
#' @examples
#' # Find the time of sunset in the Gregorian calendar
#' 
#' @name calendar_time_solar
#' @export
cal_time_solar_sunset <- new_calendar(
  day = S7::new_class("tu_day_sunset", parent = mt_loc_unit),
)

#' @rdname calendar_time_solar
#' @export
cal_time_solar_sunrise <- new_calendar(
  day = S7::new_class("tu_day_sunrise", parent = mt_loc_unit),
)

#' @rdname calendar_time_solar
#' @export
cal_time_solar_noon <- new_calendar(
  day = S7::new_class("tu_day_noon", parent = mt_loc_unit),
)