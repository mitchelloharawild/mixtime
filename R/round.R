#' Round date/time objects to specified time units
#'
#' A family of helpers to round date/time objects to a specified time granule
#' such as second, minute, hour, or day. These functions preserve the input 
#' time class, as rounded by the attributes of the `unit`.
#'
#' @name round_time
#' @rdname round_time
#' @aliases round_time floor_time ceiling_time trunc_time
#'
#' @param x A date/time object to be rounded. Accepted types include Date,
#'   POSIXct, POSIXlt and other objects that inherit from POSIXt. The returned
#'   object will be of the same class as the input.
#' @param units A time unit (or object coercible to a time unit, e.g. "day").
#' @param ... Additional arguments passed to specific implementations.
#'
#' @return An object of the same class as x with its time components adjusted
#'   to the requested unit.
#'
#' @examples
#' # Round POSIXct to the nearest minute (preserving tz)
#' t <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
#' round_time(t, units = "minute")
#'
#' # Floor to the nearest hour
#' floor_time(t, units = "hour")
#'
#' # Ceiling a Date (treated as midnight-of-day rounding)
#' d <- as.Date("2020-01-01")
#' ceiling_time(d, units = "day")
#'
#' @seealso [base::round], [lubridate::floor_date]
#' @export
round_time <- function(x, unit, ...) {
  UseMethod("round_time")
}
#' @export
round_time.mixtime <- function(x, unit, ...) {
  vecvec::vecvec_apply(x, round_time, unit = unit, ...)
}
#' @export
round_time.default <- function(x, unit, ...) {
  res <- chronon_divmod(
    from = unit,
    to = time_chronon(x),
    x = round(chronon_convert(x, unit))
  )$chronon
  attributes(res) <- attributes(x)
  res
}

#' @export
ceiling_time <- function(x, unit, ...) {
  UseMethod("ceiling_time")
}
#' @export
ceiling_time.mixtime <- function(x, unit, ...) {
  vecvec::vecvec_apply(x, ceiling_time, unit = unit, ...)
}
#' @export
ceiling_time.default <- function(x, unit, ...) {
  res <- chronon_divmod(
    from = unit,
    to = time_chronon(x),
    x = ceiling(chronon_convert(x, unit))
  )$chronon
  attributes(res) <- attributes(x)
  res
}

#' @export
floor_time <- function(x, unit, ...) {
  UseMethod("floor_time")
}
#' @export
floor_time.mixtime <- function(x, unit, ...) {
  vecvec::vecvec_apply(x, floor_time, unit = unit, ...)
}
#' @export
floor_time.default <- function(x, unit, ...) {
  res <- chronon_divmod(
    from = unit,
    to = time_chronon(x),
    x = floor(chronon_convert(x, unit))
  )$chronon
  attributes(res) <- attributes(x)
  res
}
