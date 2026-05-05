#' Solar time
#' 
#' This time calendar contains solar time units, where the boundary of each day
#' is at apparent solar midnight. Solar events define the `ampm` (midnight and
#' noon) and `illumination` (dawn, sunrise, sunset, dusk) units.
#'
#' @format A location-based calendar containing solar time units.
#' 
#' @details
#' The following time units are available in the solar calendar systems.
#' 
#' - `day()`: Day unit
#' - `ampm()`: Half-day units (AM = before solar noon, PM = after solar noon)
#' - `hour()`: Hour units within the solar day
#' - `minute()`: Minute units within the solar hour
#' - `second()`: Second units within the solar minute
#' - `degree()`: Solar angle units within the day
#' - `arcminute()`: Arcminute units within the solar degree
#' - `arcsecond()`: Arcsecond units within the solar arcminute
#' - `illumination()`: Illumination phases (night, astronomical/nautical/civil dawn, day, civil/nautical/astronomical dusk)
#' 
#' ## AM/PM half-days
#' 
#' The `ampm` unit divides each solar day into two halves between solar noon and
#' solar midnight:
#' 
#' | Half | Period                  | Description                              |
#' |------|-------------------------|------------------------------------------|
#' | AM   | Solar midnight to noon  | Morning half; before solar transit       |
#' | PM   | Solar noon to midnight  | Afternoon half; after solar transit      |
#' 
#' ## Solar clock units
#' 
#' Phases describe the illumination state of the sky and correspond to standard
#' twilight definitions used in astronomy and navigation. Each phase is bounded
#' by a pair of solar altitude thresholds:
#' 
#' | Phase              | Solar altitude range  | Description                                                 |
#' |--------------------|-----------------------|-------------------------------------------------------------|
#' | Night              | < −18°                | Sky fully dark; from last dusk to first dawn (spans noon)   |
#' | Astronomical dawn  | −18° to −12°          | Astronomical twilight before sunrise; faint objects obscured|
#' | Nautical dawn      | −12° to −6°           | Nautical twilight before sunrise; horizon visible at sea    |
#' | Civil dawn         | −6° to −0.833°        | Civil twilight before sunrise; sky brightening in the east  |
#' | Day                | > −0.833°             | Sun above the horizon; spans solar noon                     |
#' | Civil dusk         | −0.833° to −6°        | Civil twilight after sunset; sky fading in the west         |
#' | Nautical dusk      | −6° to −12°           | Nautical twilight after sunset; horizon visible at sea      |
#' | Astronomical dusk  | −12° to −18°          | Astronomical twilight after sunset; faint objects obscured  |
#' 
#' The −0.833° threshold for sunrise and sunset accounts for the mean angular
#' radius of the solar disc (0.267°) plus the standard atmospheric refraction
#' at the horizon (0.566°). Noon and midnight are derived from the equation of
#' time rather than a fixed altitude. Locations that experience polar day or
#' polar night (civil days where sunrise does not occur) are not currently 
#' supported, it is recommended to use an alternative reference location.
#' 
#' @seealso [`cal_time_civil`]
#' 
#' @examples
#' # Find the time of sunset in the Gregorian calendar
#' t <- linear_time(Sys.Date(), cal_time_solar_sunset$day(1L, lat = -37.8136, lon = 144.9631))
#' datetime(t, tz = "Australia/Melbourne")
#' 
#' @name calendar_time_solar
#' @export
cal_time_solar <- new_calendar(
  day = S7::new_class("tu_solar_day", parent = mt_loc_unit),
  ampm = S7::new_class("tu_solar_ampm", parent = mt_loc_unit),
  hour = S7::new_class("tu_solar_hour", parent = mt_loc_unit),
  minute = S7::new_class("tu_solar_minute", parent = mt_loc_unit),
  second = S7::new_class("tu_solar_second", parent = mt_loc_unit),
  degree = S7::new_class("tu_solar_degree", parent = mt_loc_unit),
  arcminute = S7::new_class("tu_solar_arcminute", parent = mt_loc_unit),
  arcsecond = S7::new_class("tu_solar_arcsecond", parent = mt_loc_unit),
  illumination = S7::new_class("tu_solar_lum", parent = mt_loc_unit)
)

# Time unit labels
method(time_unit_full, cal_time_solar$day) <- function(x) "day"
method(time_unit_abbr, cal_time_solar$day) <- function(x) "D"
method(time_unit_full, cal_time_solar$ampm) <- function(x) "halfday"
method(time_unit_abbr, cal_time_solar$ampm) <- function(x) "HD"
method(time_unit_full, cal_time_solar$hour) <- function(x) "hour"
method(time_unit_abbr, cal_time_solar$hour) <- function(x) "h"
method(time_unit_full, cal_time_solar$minute) <- function(x) "minute"
method(time_unit_abbr, cal_time_solar$minute) <- function(x) "m"
method(time_unit_full, cal_time_solar$second) <- function(x) "second"
method(time_unit_abbr, cal_time_solar$second) <- function(x) "s"
method(time_unit_full, cal_time_solar$degree) <- function(x) "degree"
method(time_unit_abbr, cal_time_solar$degree) <- function(x) "°"
method(time_unit_full, cal_time_solar$arcminute) <- function(x) "arcminute"
method(time_unit_abbr, cal_time_solar$arcminute) <- function(x) "'"
method(time_unit_full, cal_time_solar$arcsecond) <- function(x) "arcsecond"
method(time_unit_abbr, cal_time_solar$arcsecond) <- function(x) "\""
method(time_unit_full, cal_time_solar$illumination) <- function(x) "illumination phase"
method(time_unit_abbr, cal_time_solar$illumination) <- function(x) "I"

# Default formats
method(chronon_format_linear, list(cal_time_solar$day, class_any)) <- function(x, cal) "{lin(day)}"
method(chronon_format_linear, list(cal_time_solar$ampm, class_any)) <- function(x, cal) "{lin(day)} {cyc(ampm,day)}"
method(chronon_format_linear, list(cal_time_solar$hour, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour,day)}h")
method(chronon_format_linear, list(cal_time_solar$minute, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour,day)}:{cyc(minute,hour)}")
method(chronon_format_linear, list(cal_time_solar$second, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour,day)}:{cyc(minute,hour)}:{cyc(second,minute)}")
method(chronon_format_linear, list(cal_time_solar$degree, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(degree,day)}°")
method(chronon_format_linear, list(cal_time_solar$arcminute, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(degree,day)}°{cyc(arcminute,degree)}'")
method(chronon_format_linear, list(cal_time_solar$arcsecond, class_any)) <- function(x, cal) paste(chronon_format_linear(cal$day(1L), cal), "{cyc(degree,day)}°{cyc(arcminute,degree)}'{cyc(arcsecond,arcminute)}\"")
method(chronon_format_linear, list(cal_time_solar$illumination, class_any)) <- function(x, cal) "{lin(day)} {cyc(illumination,day)}"

# 8 illumination phases per solar day (night, 3 x dawn, day, 3 x dusk)
method(chronon_cardinality, list(cal_time_solar$illumination, cal_time_solar$day)) <- function(x, y, at = NULL) {
  y@n * 8L / x@n
}

method(chronon_cardinality, list(cal_time_solar$ampm, cal_time_solar$day)) <- function(x, y, at = NULL) {
  y@n * 2L / x@n
}

method(chronon_cardinality, list(cal_time_solar$hour, cal_time_solar$day)) <- function(x, y, at = NULL) {
  y@n * 24L / x@n
}

method(chronon_cardinality, list(cal_time_solar$minute, cal_time_solar$hour)) <- function(x, y, at = NULL) {
  y@n * 60L / x@n
}

# Events per solar day: 86400 seconds
method(chronon_cardinality, list(cal_time_solar$second, cal_time_solar$minute)) <- function(x, y, at = NULL) {
  y@n * 60L / x@n
}

# Events per solar day: 360 degrees
method(chronon_cardinality, list(cal_time_solar$degree, cal_time_solar$day)) <- function(x, y, at = NULL) {
  y@n * 360L / x@n
}

method(chronon_cardinality, list(cal_time_solar$arcminute, cal_time_solar$degree)) <- function(x, y, at = NULL) {
  y@n * 60L / x@n
}

method(chronon_cardinality, list(cal_time_solar$arcsecond, cal_time_solar$arcminute)) <- function(x, y, at = NULL) {
  y@n * 60L / x@n
}

# The number of UTC seconds in a solar day (midnight-to-midnight)
method(chronon_cardinality, list(cal_time_civil$second, cal_time_solar$day)) <- function(x, y, at = NULL) {
  at_sd <- approx_solar_days_from_utc(at, y@lat, y@lon, y@alt)
  (approx_utc_from_solar_days(at_sd + y@n, y@lat, y@lon, y@alt) -
     approx_utc_from_solar_days(at_sd, y@lat, y@lon, y@alt)) / x@n
}

# Convert UTC seconds → solar day count (day boundary = solar midnight)
method(chronon_divmod, list(cal_time_civil$second, cal_time_solar$day)) <- function(from, to, x) {
  list(
    div = approx_solar_days_from_utc(as.double(x), to@lat, to@lon, to@alt),
    mod = 0
  )
}

# Convert solar day count → UTC seconds
method(chronon_divmod, list(cal_time_solar$day, cal_time_civil$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_solar_days(as.double(x), from@lat, from@lon, from@alt),
    mod = 0
  )
}

# Convert UTC seconds → continuous solar phase count.
# Delegates entirely to C++ which handles midnight boundaries and NaN phases.
method(chronon_divmod, list(cal_time_civil$second, cal_time_solar$illumination)) <- function(from, to, x) {
  list(
    div = approx_solar_phase_from_utc(as.double(x), to@lat, to@lon, to@alt),
    mod = 0
  )
}

# Convert solar phase count → UTC seconds.
# Integer phase counts return the phase start boundary; fractional counts interpolate.
method(chronon_divmod, list(cal_time_solar$illumination, cal_time_civil$second)) <- function(from, to, x) {
  list(
    div = approx_solar_phase_utc(as.double(x), from@lat, from@lon, from@alt),
    mod = 0
  )
}

# Convert UTC seconds → continuous ampm count (d*2 + half + frac).
# Noon is computed directly from the equation of time; midnight from solar anti-transit.
method(chronon_divmod, list(cal_time_civil$second, cal_time_solar$ampm)) <- function(from, to, x) {
  list(
    div = approx_solar_ampm_from_utc(as.double(x), to@lat, to@lon, to@alt) / to@n,
    mod = 0
  )
}

# Convert ampm count → UTC seconds.
# Noon is reconstructed directly from the equation of time via C++.
method(chronon_divmod, list(cal_time_solar$ampm, cal_time_civil$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_solar_ampm(as.double(x) * from@n, from@lat, from@lon, from@alt),
    mod = 0
  )
}

## Time labels
### Linear labels for solar days (re-use Gregorian dates)
S7::method(linear_labels, cal_time_solar$day) <- function(granule, i, ...) {
  format(.Date(i*granule@n))
}

### Cyclical labels for solar time granules
solar_illumination_phases <- c(
  "night", "astronomical dawn", "nautical dawn", "civil dawn",
  "day", "civil dusk", "nautical dusk", "astronomical dusk"
)
method(cyclical_labels, list(cal_time_solar$illumination, cal_time_solar$day)) <- function(granule, cycle, i) {
  solar_illumination_phases[i + 1L]
}

solar_ampm_labels <- c("AM", "PM")
method(cyclical_labels, list(cal_time_solar$ampm, cal_time_solar$day)) <- function(granule, cycle, i) {
  solar_ampm_labels[i + 1L]
}

method(cyclical_labels, list(cal_time_solar$hour, S7::class_any)) <- function(granule, cycle, i) {
  if (S7::S7_inherits(cycle, cal_time_civil$ampm)) {
    # 12 hours count with 12,1,2,...,11
    sprintf("%02d", (i-1L)%%12L + 1L)
  } else {
    # 24 hours count with 0-indexing
    sprintf("%02d", i)
  }
}
method(cyclical_labels, list(cal_time_solar$minute, S7::class_any)) <- function(granule, cycle, i) {
  sprintf("%02d", i)
}
method(cyclical_labels, list(cal_time_solar$second, S7::class_any)) <- function(granule, cycle, i) {
  sprintf("%02d", i)
}
method(cyclical_labels, list(cal_time_solar$degree, S7::class_any)) <- function(granule, cycle, i) {
  sprintf("%03d", i)
}
method(cyclical_labels, list(cal_time_solar$arcminute, S7::class_any)) <- function(granule, cycle, i) {
  sprintf("%02d", i)
}
method(cyclical_labels, list(cal_time_solar$arcsecond, S7::class_any)) <- function(granule, cycle, i) {
  sprintf("%02d", i)
}