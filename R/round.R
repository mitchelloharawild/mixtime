#' Round, floor and ceiling transformations for time objects
#'
#' A family of helpers to round date/time objects to a specified time granule
#' such as second, minute, hour, or day. These functions preserve the input 
#' time class, as rounded by the attributes of the `granule`.
#'
#' @name time_round
#' @aliases time_round time_floor time_ceiling trunc_time
#'
#' @param x A date/time object to be rounded. Accepted types include Date,
#'   POSIXct, POSIXlt and other objects that inherit from POSIXt. The returned
#'   object will be of the same class as the input.
#' @param granule A time granule (or object coercible to a time granule, e.g. "day").
#' @param ... Additional arguments passed to specific implementations.
#'
#' @return An object of the same class as x with its time components adjusted
#'   to the requested granule.
#'
#' @examples
#' # Round POSIXct to the nearest minute (preserving tz)
#' t <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
#' time_round(t, granule = cal_gregorian$minute(1L))
#'
#' # Floor to the nearest hour
#' time_floor(t, granule = cal_gregorian$hour(1L))
#'
#' # Ceiling a Date (treated as midnight-of-day rounding)
#' d <- as.Date("2020-01-01")
#' time_ceiling(d, granule = cal_gregorian$month(1L))
#'
#' @seealso [base::round], [lubridate::round_date]
#' @export
time_round <- new_generic("time_round", "x", function(x, granule, ...) {
  S7::S7_dispatch()
})
method(time_round, class_mixtime) <- vecvec::vecvec_apply_fn(time_round)
method(time_round, S7::class_any) <- function(x, granule, ...) {
  if (is.character(granule)) granule <- parse_time_unit(granule)
  if (S7::S7_inherits(granule, mt_unit)) granule <- duration(1L, granule)
  if (length(granule) != 1L) {
    cli::cli_abort("{.var granule} must be a single time duration", call. = FALSE)
  }
  chronon <- time_chronon(granule)
  chronon@n <- chronon@n * as.numeric(granule)
  res <- chronon_divmod(
    from = chronon,
    to = time_chronon(x),
    x = round(chronon_convert(x, chronon))
  )$div
  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}

#' @rdname time_round
#' @export
time_ceiling <- new_generic("time_ceiling", "x", function(x, granule, ...) {
  S7::S7_dispatch()
})
method(time_ceiling, class_mixtime) <- vecvec::vecvec_apply_fn(time_ceiling)
method(time_ceiling, S7::class_any) <- function(x, granule, ...) {
  if (is.character(granule)) granule <- parse_time_unit(granule)
  if (S7::S7_inherits(granule, mt_unit)) granule <- duration(1L, granule)
  if (length(granule) != 1L) {
    cli::cli_abort("{.var granule} must be a single time duration", call. = FALSE)
  }
  chronon <- time_chronon(granule)
  chronon@n <- chronon@n * as.numeric(granule)
  res <- chronon_divmod(
    from = chronon,
    to = time_chronon(x),
    # Special handling of ceiling to round .0 up
    x = ceiling(floor(chronon_convert(x, chronon)) + 0.5)
  )$div
  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}

#' @rdname time_round
#' @export
time_floor <- new_generic("time_floor", "x", function(x, granule, ...) {
  S7::S7_dispatch()
})
method(time_floor, class_mixtime) <- vecvec::vecvec_apply_fn(time_floor)
method(time_floor, S7::class_any) <- function(x, granule, ...) {
  if (is.character(granule)) granule <- parse_time_unit(granule)
  if (S7::S7_inherits(granule, mt_unit)) granule <- duration(1L, granule)
  if (length(granule) != 1L) {
    cli::cli_abort("{.var granule} must be a single time duration", call. = FALSE)
  }
  chronon <- time_chronon(granule)
  chronon@n <- chronon@n * as.numeric(granule)
  res <- chronon_divmod(
    from = chronon,
    to = time_chronon(x),
    x = floor(chronon_convert(x, chronon))
  )$div
  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}
