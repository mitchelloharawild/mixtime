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
  time_round_impl(x, granule, round)
}

#' @rdname time_round
#' @export
time_ceiling <- new_generic("time_ceiling", "x", function(x, granule, ...) {
  S7::S7_dispatch()
})
method(time_ceiling, class_mixtime) <- vecvec::vecvec_apply_fn(time_ceiling)
method(time_ceiling, S7::class_any) <- function(x, granule, ...) {
  # Ceiling time (floor + 0.5 to round .0 up)
  time_round_impl(x, granule, function(res) ceiling(floor(res) + 0.5))
}

#' @rdname time_round
#' @export
time_floor <- new_generic("time_floor", "x", function(x, granule, ...) {
  S7::S7_dispatch()
})
method(time_floor, class_mixtime) <- vecvec::vecvec_apply_fn(time_floor)
method(time_floor, S7::class_any) <- function(x, granule, ...) {
  time_round_impl(x, granule, floor)
}

# Shared boundary-rounding logic for time_round()/time_ceiling()/time_floor().
#
# The timezone offset is applied while `x` is still expressed in its own
# native chronon (which always converts to/from itself exactly - a 1:1
# ratio), and only then converted into the granule's units for rounding.
# Applying the offset *after* converting into the granule would require
# scaling it by `chronon_cardinality()`, which for granules like `quarter()`
# is a variable, context-dependent conversion (the number of seconds in a
# quarter isn't fixed) - and can only be approximated (e.g. by an average
# quarter length), leaking drift into the rounded boundary.
time_round_impl <- function(x, granule, round_fn) {
  if (is.character(granule)) granule <- parse_time_unit(granule)
  if (S7_inherits(granule, mt_unit)) granule <- duration(1L, granule)
  if (length(granule) != 1L) {
    cli::cli_abort("{.var granule} must be a single time duration", call. = FALSE)
  }
  by <- attr(granule@x[[1L]], "chronon")
  by@n <- by@n * as.numeric(granule)

  # Inherit non-naive attributes from chronon
  by <- granule_inherit_props(by, chronon <- chronon_common(x))

  # Native numeric value of `x`, in its own chronon (an exact conversion)
  res <- chronon_convert(x, chronon)

  # Apply timezone offsets (UTC -> tz) while still in the exact native unit
  res <- res + tz_offset_impl(res, chronon, tz = tz_name(by))

  # Convert the (now local wall-clock) instant into the granule's units and round it
  res <- chronon_convert_impl(res, from = chronon, to = by, discrete = FALSE, tz = "UTC")
  res <- round_fn(res)

  # Convert the rounded boundary back to the native chronon (still local wall-clock)
  res <- chronon_convert_impl(res, from = by, to = chronon, discrete = FALSE, tz = "UTC")

  # Undo timezone offsets (tz -> UTC)
  # Second pass of tz offset for DST changes between `x` and the boundary
  res <- res - tz_offset_impl(
    res - tz_offset_impl(res, chronon, tz = tz_name(by)),
    chronon, tz = tz_name(by)
  )

  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}
