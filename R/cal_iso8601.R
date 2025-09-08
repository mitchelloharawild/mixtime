#' ISO 8601 time classes
#' 
#' @examples
#' yw <- linear_time(tu_week(1L), list(tu_year(1L)))
#' yw(Sys.Date())
#' 
#' 
#' @rdname calendar_iso8601
#' @export
tu_week <- S7::new_class("tu_week", parent = mt_unit)
S7::method(time_unit_full, tu_week) <- function(x) "week"
S7::method(time_unit_abbr, tu_week) <- function(x) "W"

S7::method(chronon_cast, list(tu_day, tu_week)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  list(
    chronon = (x - 4) %/% 7L,
    remainder = (x - 4) %% 7L
  )
}

S7::method(chronon_cast, list(tu_week, tu_day)) <- function(from, to, x) {
  # TODO: Add week start specification (e.g., week starts on Monday vs Sunday)
  list(
    chronon = x*7L + 4L,
    remainder = 0L
  )
}

S7::method(chronon_cast, list(tu_week, tu_year)) <- function(from, to, x) {
  # Modulo arithmetic to convert from days to months
  if (calendar_algebra(to, tu_year(1L)) != 1L) {
    stop("Converting to multi-year chronons from weeks is not yet supported", call. = FALSE)
  }
  # TODO: should be swapped out to arithmetic on integer days since epoch

  # ISO 8601 years for weekly chronons cycle with the first week with a Thursday
  # Unix epoch 1970-01-01 is a Thursday, a convenient reference point
  x <- as.POSIXlt(as.Date(x*7))
  list(
    chronon = x$year-70L,
    remainder = (x$yday - x$wday + 10) %/% 7
  )
}

S7::method(cyclical_labels, list(tu_day, tu_week)) <- function(granule, cycle, i) {
  # TODO: Add offset for different week starting days
  format(as.Date(i-1L), "%a")
}

#' ISO 8601 year-week time representation
#'
#' Create or coerce using `yearweek()`.
#'
#' \lifecycle{experimental}
#'
#' @param .data Another object to be coerced into ISO 8601 year-weeks.
#' @param ... Arguments for methods.
#'
#' @examples
#' 
#' yearweek(Sys.Date())
#' yearweek(0:52)
#' 
#' @export
yearweek <- linear_time(granules = list(tu_year(1L)), chronon = tu_week(1L))
