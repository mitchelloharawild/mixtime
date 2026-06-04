#' Lunar time unit classes
#'
#' Time unit constructors for the synodic lunar time system, where the boundary
#' of each month is at the new moon (lunar conjunction) and the boundary of
#' each phase is at a lunar octant (each eighth of the synodic cycle). Both
#' boundaries are location-independent, as the new moon is a geocentric
#' astronomical event. `cal_time_lunar` is an alias for
#' `cal_time_lunar_synodic`.
#'
#' @format A calendar containing synodic lunar time units.
#' 
#' @details
#' The following time units are available in the lunar calendar systems.
#' 
#' - `month()`: Synodic month unit
#' - `phase()`: Synodic phase unit
#' 
#' @return An S3 list of class `c("cal_time_lunar", "mt_calendar")` containing
#'   the named time unit classes of the synodic lunar calendar. Each unit is
#'   accessible via `$` notation and calling it with a step size produces a
#'   time granule (e.g., 1 synodic month granule as
#'   `cal_time_lunar$month(1L)`). Lunar month and phase boundaries are
#'   location-independent.
#'
#' @seealso [`cal_time_civil`], [`cal_time_solar`]
#' 
#' @examples
#' # Find the time of a new moon in the Gregorian calendar
#' t <- linear_time(Sys.Date(), cal_time_lunar$month(1L))
#' datetime(t, tz = "Australia/Melbourne")
#' 
#' 
#' @name calendar_time_lunar
#' @export
cal_time_lunar_synodic <- new_calendar(
  month = S7::new_class("tu_lunar_month", parent = mt_loc_unit),
  phase = S7::new_class("tu_lunar_phase", parent = mt_loc_unit)
)

#' @export
#' @rdname calendar_time_lunar
cal_time_lunar <- cal_time_lunar_synodic

# Time granule labels
method(time_unit_full, cal_time_lunar$month) <- function(x) "synodic month"
method(time_unit_abbr, cal_time_lunar$month) <- function(x) "LM"
method(time_unit_full, cal_time_lunar$phase) <- function(x) "synodic phase"
method(time_unit_abbr, cal_time_lunar$phase) <- function(x) "LP"

# Default formats
method(chronon_format_linear, list(cal_time_lunar$month, class_any)) <- function(x, cal) "L{lin(month)}"
method(chronon_format_linear, list(cal_time_lunar$phase, class_any)) <- function(x, cal) "L{lin(month)} {cyc(phase,month)}"
method(chronon_format_cyclical, list(cal_time_lunar$phase, cal_time_lunar$month)) <- function(x, y) "{cyc(phase,month)}"

# Epoch for lunations
method(chronon_epoch, cal_time_lunar$month) <- function(x) 953L

# The number of UTC seconds in a lunar (synodic) phase
method(chronon_cardinality, list(cal_time_civil$second, cal_time_lunar$phase)) <- function(x, y, at = NULL) {
  at <- approx_lunations_from_utc(at / 8)
  (approx_utc_from_lunations(at + y@n) - approx_utc_from_lunations(at)) / x@n
}

# The number of lunar phases in a lunar (synodic) month
method(chronon_cardinality, list(cal_time_lunar$phase, cal_time_lunar$month)) <- function(x, y, at = NULL) {
  y@n*8L/x@n
}

# The number of UTC seconds in a lunar (synodic) phase
method(chronon_divmod, list(cal_time_civil$second, cal_time_lunar$phase)) <- function(from, to, x) {
  list(
    div = approx_lunations_from_utc(as.double(x)) * 8,
    mod = 0
  )
}

method(chronon_divmod, list(cal_time_lunar$phase, cal_time_civil$second)) <- function(from, to, x) {
  list(
    div = approx_utc_from_lunations(as.double(x) / 8),
    mod = 0
  )
}

lunar_phase_abbr <- c("NM", "WxC", "FQ", "WxG", "FM", "WnG", "LQ", "WnC")
lunar_phase_full <- c("new moon", "waxing crescent", "first quarter", "waxing gibbous", "full moon", "waning gibbous", "last quarter", "waning crescent")
lunar_phase_emoji <- c("\U0001F311", "\U0001F312", "\U0001F313", "\U0001F314", "\U0001F315", "\U0001F316", "\U0001F317", "\U0001F318")
method(cyclical_labels, list(cal_time_lunar$phase, cal_time_lunar$month)) <- function(granule, cycle, i, label = TRUE, abbreviate = TRUE, emoji = l10n_info()[["UTF-8"]], intermediate = TRUE) {
  i <- i + 1L
  if (label) {
    if (emoji) {
      lunar_phase_emoji[i]
    } else if (abbreviate) {
      lunar_phase_abbr[i]
    } else {
      lunar_phase_full[i]
    }
  } else {
    as.character(i)
  }
}