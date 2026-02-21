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

# Time unit labels
S7::method(time_unit_full, cal_time_solar_sunrise$day) <- function(x) "sunrise"
S7::method(time_unit_abbr, cal_time_solar_sunrise$day) <- function(x) "D"
S7::method(time_unit_full, cal_time_solar_noon$day) <- function(x) "noon"
S7::method(time_unit_abbr, cal_time_solar_noon$day) <- function(x) "D"
S7::method(time_unit_full, cal_time_solar_sunset$day) <- function(x) "sunset"
S7::method(time_unit_abbr, cal_time_solar_sunset$day) <- function(x) "D"

# The number of UTC seconds in a sunrise-based day
S7::method(chronon_cardinality, list(cal_time_solar_sunrise$day, cal_time_civil_midnight$second)) <- function(x, y, at = NULL) {
  at <- approx_sunrises_from_utc(at, x@lat, x@lon, -0.833)
  (approx_utc_from_sunrises(at + vec_data(x), x@lat, x@lon, -0.833) - approx_utc_from_sunrises(at, x@lat, x@lon, -0.833)) / vec_data(y)
}

# The number of UTC seconds in a noon-based day
S7::method(chronon_cardinality, list(cal_time_solar_noon$day, cal_time_civil_midnight$second)) <- function(x, y, at = NULL) {
  at <- approx_noons_from_utc(at, x@lat, x@lon)
  (approx_utc_from_noons(at + vec_data(x), x@lat, x@lon) - approx_utc_from_noons(at, x@lat, x@lon)) / vec_data(y)
}

# The number of UTC seconds in a sunset-based day
S7::method(chronon_cardinality, list(cal_time_solar_sunset$day, cal_time_civil_midnight$second)) <- function(x, y, at = NULL) {
  at <- approx_sunsets_from_utc(at, x@lat, x@lon, -0.833)
  (approx_utc_from_sunsets(at + vec_data(x), x@lat, x@lon, -0.833) - approx_utc_from_sunsets(at, x@lat, x@lon, -0.833)) / vec_data(y)
}


S7::method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_sunrise$day)) <- function(from, to, x) {
  list(
    chronon = approx_sunrises_from_utc(as.double(x), to@lat, to@lon, -0.833),
    remainder = 0
  )
}

S7::method(chronon_divmod, list(cal_time_solar_sunrise$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    chronon = approx_utc_from_sunrises(as.double(x), from@lat, from@lon, -0.833),
    remainder = 0
  )
}

S7::method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_noon$day)) <- function(from, to, x) {
  list(
    chronon = approx_noons_from_utc(as.double(x), to@lat, to@lon),
    remainder = 0
  )
}

S7::method(chronon_divmod, list(cal_time_solar_noon$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    chronon = approx_utc_from_noons(as.double(x), from@lat, from@lon),
    remainder = 0
  )
}

S7::method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_sunset$day)) <- function(from, to, x) {
  list(
    chronon = approx_sunsets_from_utc(as.double(x), to@lat, to@lon, -0.833),
    remainder = 0
  )
}

S7::method(chronon_divmod, list(cal_time_solar_sunset$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    chronon = approx_utc_from_sunsets(as.double(x), from@lat, from@lon, -0.833),
    remainder = 0
  )
}