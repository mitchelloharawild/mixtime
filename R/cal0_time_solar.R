#' Solar time unit classes
#'
#' Time unit constructors for the solar time system where the boundary of each
#' day is at sunrise, sunset, or noon. This calendar is intended to be
#' built on by other calendars to add common time components.
#'
#' @format A location-based calendar containing solar time units.
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
#' t <- linear_time(Sys.Date(), cal_time_solar_sunset$day(1L, lat = -37.8136, lon = 144.9631))
#' datetime(t, tz = "Australia/Melbourne")
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

#' @rdname calendar_time_solar
#' @export
cal_time_solar_midnight <- new_calendar(
  day = S7::new_class("tu_day_midnight", parent = mt_loc_unit),
)

#' @rdname calendar_time_solar
#' @export
cal_time_solar_dawn <- new_calendar(
  day = S7::new_class("tu_day_dawn", parent = mt_loc_unit),
)

#' @rdname calendar_time_solar
#' @export
cal_time_solar_dusk <- new_calendar(
  day = S7::new_class("tu_day_dusk", parent = mt_loc_unit),
)

# Time unit labels
method(time_unit_full, cal_time_solar_sunrise$day) <- function(x) "sunrise"
method(time_unit_abbr, cal_time_solar_sunrise$day) <- function(x) "D"
method(time_unit_full, cal_time_solar_noon$day) <- function(x) "noon"
method(time_unit_abbr, cal_time_solar_noon$day) <- function(x) "D"
method(time_unit_full, cal_time_solar_sunset$day) <- function(x) "sunset"
method(time_unit_abbr, cal_time_solar_sunset$day) <- function(x) "D"
method(time_unit_full, cal_time_solar_midnight$day) <- function(x) "midnight"
method(time_unit_abbr, cal_time_solar_midnight$day) <- function(x) "D"
method(time_unit_full, cal_time_solar_dawn$day) <- function(x) "dawn"
method(time_unit_abbr, cal_time_solar_dawn$day) <- function(x) "D"
method(time_unit_full, cal_time_solar_dusk$day) <- function(x) "dusk"
method(time_unit_abbr, cal_time_solar_dusk$day) <- function(x) "D"

# The number of UTC seconds in a sunrise-based day
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_solar_sunrise$day)) <- function(x, y, at = NULL) {
  at <- approx_sunrises_from_utc(at, y@lat, y@lon, -0.833)
  (approx_utc_from_sunrises(at + y@n, y@lat, y@lon, -0.833) - approx_utc_from_sunrises(at, y@lat, y@lon, -0.833)) / x@n
}

# The number of UTC seconds in a noon-based day
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_solar_noon$day)) <- function(x, y, at = NULL) {
  at <- approx_noons_from_utc(at, y@lat, y@lon)
  (approx_utc_from_noons(at + y@n, y@lat, y@lon) - approx_utc_from_noons(at, y@lat, y@lon)) / x@n
}

# The number of UTC seconds in a sunset-based day
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_solar_sunset$day)) <- function(x, y, at = NULL) {
  at <- approx_sunsets_from_utc(at, y@lat, y@lon, -0.833)
  (approx_utc_from_sunsets(at + y@n, y@lat, y@lon, -0.833) - approx_utc_from_sunsets(at, y@lat, y@lon, -0.833)) / x@n
}

# The number of UTC seconds in a midnight-based day
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_solar_midnight$day)) <- function(x, y, at = NULL) {
  at <- approx_midnights_from_utc(at, y@lat, y@lon)
  (approx_utc_from_midnights(at + y@n, y@lat, y@lon) - approx_utc_from_midnights(at, y@lat, y@lon)) / x@n
}

# The number of UTC seconds in a dawn-based day
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_solar_dawn$day)) <- function(x, y, at = NULL) {
  at <- approx_dawns_from_utc(at, y@lat, y@lon, -6.0)
  (approx_utc_from_dawns(at + y@n, y@lat, y@lon, -6.0) - approx_utc_from_dawns(at, y@lat, y@lon, -6.0)) / x@n
}

# The number of UTC seconds in a dusk-based day
method(chronon_cardinality, list(cal_time_civil_midnight$second, cal_time_solar_dusk$day)) <- function(x, y, at = NULL) {
  at <- approx_dusks_from_utc(at, y@lat, y@lon, -6.0)
  (approx_utc_from_dusks(at + y@n, y@lat, y@lon, -6.0) - approx_utc_from_dusks(at, y@lat, y@lon, -6.0)) / x@n
}


method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_sunrise$day)) <- function(from, to, x) {
  list(
    div = approx_sunrises_from_utc(as.double(x), to@lat, to@lon, -0.833),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_solar_sunrise$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_sunrises(as.double(x), from@lat, from@lon, -0.833),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_noon$day)) <- function(from, to, x) {
  list(
    div = approx_noons_from_utc(as.double(x), to@lat, to@lon),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_solar_noon$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_noons(as.double(x), from@lat, from@lon),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_sunset$day)) <- function(from, to, x) {
  list(
    div = approx_sunsets_from_utc(as.double(x), to@lat, to@lon, -0.833),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_solar_sunset$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_sunsets(as.double(x), from@lat, from@lon, -0.833),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_midnight$day)) <- function(from, to, x) {
  list(
    div = approx_midnights_from_utc(as.double(x), to@lat, to@lon),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_solar_midnight$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_midnights(as.double(x), from@lat, from@lon),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_dawn$day)) <- function(from, to, x) {
  list(
    div = approx_dawns_from_utc(as.double(x), to@lat, to@lon, -6.0),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_solar_dawn$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_dawns(as.double(x), from@lat, from@lon, -6.0),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_civil_midnight$second, cal_time_solar_dusk$day)) <- function(from, to, x) {
  list(
    div = approx_dusks_from_utc(as.double(x), to@lat, to@lon, -6.0),
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_solar_dusk$day, cal_time_civil_midnight$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_dusks(as.double(x), from@lat, from@lon, -6.0),
    mod = 0
  )
}