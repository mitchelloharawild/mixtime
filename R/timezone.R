#' Extract timezone from an object
#'
#' Generic function to extract the timezone from objects that have timezone information.
#'
#' @param x An object with timezone information.
#'
#' @return A character string representing the timezone (e.g., "America/New_York", "UTC").
#' @export
#'
#' @examples
#' tz_name(Sys.time())
#' tz_name(as.POSIXct("2024-06-15 12:00:00", tz = "America/New_York"))
tz_name <- S7::new_generic("tz_name", "x")
S7::method(tz_name, S7::class_POSIXt) <- function(x) attr(x, "tzone") %||% "UTC"
S7::method(tz_name, mt_tz_unit) <- function(x) x@tz
S7::method(tz_name, S7::new_S3_class("mt_linear")) <- function(x) tz_name(time_chronon(x))
S7::method(tz_name, S7::class_any) <- function(x) "UTC"

#' Get timezone offset
#'
#' Returns the UTC offset for a given datetime in its specified timezone.
#'
#' @param x A time class coercible to POSIXt with an associated time zone.
#'
#' @return A numeric vector of offsets from UTC in the same chronon (e.g. seconds for POSIXt, days for dates, etc.)
#' @export
#'
#' @examples
#' tz_offset(Sys.time())
#' tz_offset(as.POSIXct("2024-06-15 12:00:00", tz = "America/New_York"))
tz_offset <- S7::new_generic("tz_offset", "x")
S7::method(tz_offset, S7::class_POSIXct) <- function(x) get_tz_offset(x, tz_name(x))
method(tz_offset, S7::new_S3_class("mixtime")) <- function(x) {
  vecvec::unvecvec(
    vecvec::vecvec_apply(x, tz_offset)
  )
}
method(tz_offset, S7::new_S3_class("mt_linear")) <- function(x, to, discrete = FALSE, ...) {
  time_s <- as.POSIXct(x)
  offset_s <- get_tz_offset(time_s, tz_name(x))
  offset_s*chronon_cardinality(tu_second(1L), time_chronon(x), time_s)
}

#' Get timezone abbreviation
#'
#' Returns the timezone abbreviation (e.g., "EST", "PDT") for a given datetime
#' in its specified timezone.
#'
#' @param x A POSIXct datetime object or something coercible to POSIXct.
#'   The timezone is extracted from this object.
#'
#' @return A character vector of timezone abbreviations.
#' @export
#'
#' @examples
#' tz_abbreviation(Sys.time())
#' tz_abbreviation(as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York"))
tz_abbreviation <- function(x) {
  x <- as.POSIXct(x)
  tz <- attr(x, "tzone") %||% Sys.timezone()
  get_tz_abbreviation(x, tz)
}

#' Get timezone transitions
#'
#' Returns all timezone transitions (e.g., daylight saving time changes) that
#' occur between two datetimes. The timezone is taken from the start datetime.
#'
#' @param start A POSIXct datetime object or something coercible to POSIXct,
#'   representing the start of the time range. The timezone is extracted from this object.
#' @param end A POSIXct datetime object or something coercible to POSIXct,
#'   representing the end of the time range.
#'
#' @return A data frame containing information about timezone transitions
#'   in the specified range.
#' @export
#'
#' @examples
#' # Get all DST transitions in 2024 for New York
#' tz_transitions(
#'   as.POSIXct("2024-01-01", tz = "America/New_York"),
#'   as.POSIXct("2024-12-31", tz = "America/New_York")
#' )
tz_transitions <- function(start, end) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  tz <- attr(start, "tzone") %||% Sys.timezone()
  get_tz_transitions(start, end, tz)
}
