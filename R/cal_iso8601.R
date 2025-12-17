#' ISO 8601 time unit classes
#'
#' Time unit constructors for the ISO 8601 calendar system. These units can be
#' used with [linear_time()] to create custom time representations.
#'
#' @inheritParams mt_unit
#' 
#' @return A time unit object for the ISO 8601 calendar system.
#' 
#' @details
#' The following ISO 8601 time units are available:
#' 
#' - `tu_isoyear()`: ISO year unit (years start on the week containing the first Thursday)
#' - `tu_week()`: Week unit (7-day periods)
#' 
#' ISO 8601 weeks always start on Monday and the first week of a year is the week
#' containing the first Thursday of that year. This means that some days in early
#' January may belong to the last week of the previous ISO year, and some days in
#' late December may belong to the first week of the next ISO year.
#' 
#' @seealso [linear_time()] for creating custom time representations,
#'   [yearweek()] for a pre-defined ISO 8601 year-week representation
#' 
#' @name calendar_iso8601
#' @export
tu_isoyear <- S7::new_class("tu_isoyear", parent = mt_unit)
S7::method(time_unit_full, tu_isoyear) <- function(x) "isoyear"
S7::method(time_unit_abbr, tu_isoyear) <- function(x) "IY"

#' @rdname calendar_iso8601
#' @export
tu_week <- S7::new_class("tu_week", parent = mt_unit)
S7::method(time_unit_full, tu_week) <- function(x) "week"
S7::method(time_unit_abbr, tu_week) <- function(x) "W"

# 1:1 mapping for isoyears to years 
# TODO - this is not entirely accurate, but is currently necessary
# for converting the 1970 epoch in the print method
S7::method(chronon_cardinality, list(tu_year, tu_isoyear)) <- function(x, y, at = NULL) {
  vec_data(x)*1L/vec_data(y)
}

S7::method(chronon_cardinality, list(tu_week, tu_day)) <- function(x, y, at = NULL) {
  vec_data(x)*7L/vec_data(y)
}

S7::method(chronon_divmod, list(tu_day, tu_week)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  list(
    chronon = (x + 3) %/% 7L,
    remainder = (x + 3) %% 7L
  )
}

S7::method(chronon_divmod, list(tu_week, tu_day)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  list(
    chronon = x*7L + 4L,
    remainder = 0L
  )
}

S7::method(chronon_divmod, list(tu_week, tu_isoyear)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to months
  if (chronon_cardinality(to, tu_isoyear(1L)) != 1L) {
    stop("Converting to multi-year chronons from weeks is not yet supported", call. = FALSE)
  }
  # TODO: should be swapped out to arithmetic on integer days since epoch

  # ISO 8601 years for weekly chronons cycle with the first week with a Thursday
  # Unix epoch 1970-01-01 is a Thursday, a convenient reference point
  
  x <- as.POSIXlt(as.Date(x*7))
  list(
    chronon = x$year-70L,
    remainder = x$yday %/% 7L
  )
}

S7::method(cyclical_labels, list(tu_day, tu_week)) <- function(granule, cycle, i) {
  # TODO: Add offset for different week starting days
  format(as.Date(i+4L), "%a")
}

#' ISO 8601 year-week time representation
#'
#' Create or coerce using `yearweek()`.
#'
#' @inheritParams yearmonth
#' @param .data Another object to be coerced into ISO 8601 year-weeks.
#'
#' @examples
#' 
#' yearweek(Sys.Date())
#' yearweek(0:52)
#' 
#' @export
#' @name linear_iso8601
yearweek <- linear_time(granules = list(tu_isoyear(1L)), chronon = tu_week(1L))
