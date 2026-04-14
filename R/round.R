#' Round, floor and ceiling transformations for time objects
#'
#' A family of helpers to round date/time objects to a specified time granule
#' such as second, minute, hour, or day. These functions preserve the input 
#' time class, as rounded by the attributes of the `unit`.
#'
#' @name round_time
#' @aliases round_time floor_time ceiling_time trunc_time
#'
#' @param x A date/time object to be rounded. Accepted types include Date,
#'   POSIXct, POSIXlt and other objects that inherit from POSIXt. The returned
#'   object will be of the same class as the input.
#' @param unit A time unit (or object coercible to a time unit, e.g. "day").
#' @param ... Additional arguments passed to specific implementations.
#'
#' @return An object of the same class as x with its time components adjusted
#'   to the requested unit.
#'
#' @examples
#' # Round POSIXct to the nearest minute (preserving tz)
#' t <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
#' round_time(t, unit = cal_gregorian$minute(1L))
#'
#' # Floor to the nearest hour
#' floor_time(t, unit = cal_gregorian$hour(1L))
#'
#' # Ceiling a Date (treated as midnight-of-day rounding)
#' d <- as.Date("2020-01-01")
#' ceiling_time(d, unit = cal_gregorian$month(1L))
#'
#' @seealso [base::round], [lubridate::round_date]
#' @export
round_time <- new_generic("round_time", "x")
method(round_time, class_mixtime) <- function(x, unit, ...) {
  vecvec::vecvec_apply(x, round_time, unit = unit, ...)
}
#' @export
method(round_time, S7::class_any) <- function(x, unit, ...) {
  if (is.character(unit)) unit <- parse_time_unit(unit)
  res <- chronon_divmod(
    from = unit,
    to = time_chronon(x),
    x = round(chronon_convert(x, unit))
  )$div
  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}

#' @rdname round_time
#' @export
ceiling_time <- new_generic("ceiling_time", "x")
method(ceiling_time, class_mixtime) <- function(x, unit, ...) {
  vecvec::vecvec_apply(x, ceiling_time, unit = unit, ...)
}
method(ceiling_time, S7::class_any) <- function(x, unit, ...) {
  if (is.character(unit)) unit <- parse_time_unit(unit)
  res <- chronon_divmod(
    from = unit,
    to = time_chronon(x),
    # Special handling of ceiling to round .0 up
    x = ceiling(floor(chronon_convert(x, unit)) + 0.5)
  )$div
  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}

#' @rdname round_time
#' @export
floor_time <- new_generic("floor_time", "x")
method(floor_time, class_mixtime) <- function(x, unit, ...) {
  vecvec::vecvec_apply(x, floor_time, unit = unit, ...)
}
method(floor_time, S7::class_any) <- function(x, unit, ...) {
  if (is.character(unit)) unit <- parse_time_unit(unit)
  res <- chronon_divmod(
    from = unit,
    to = time_chronon(x),
    x = floor(chronon_convert(x, unit))
  )$div
  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}
