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
  by <- attr(granule@x[[1L]], "chronon")
  by@n <- by@n * as.numeric(granule)
  
  # Inherit non-naive attributes from chronon
  by <- granule_inherit_props(by, chronon <- chronon_common(x))

  # Convert time into the granule chronon
  res <- chronon_convert(x, by)
  # Apply timezone offsets (UTC -> tz)
  res <- res + tz_offset_impl(res, by)
  # Round time
  res <- round(res)
  
  # Undo timezone offsets (tz -> UTC)
  # Second pass of tz offset for DST changes within the day
  # TODO - ideally this is more directly handled in chronon_convert or similar
  res <- res - tz_offset_impl(res - tz_offset_impl(res, by), by)

  # Convert back to original chronon (includes tz -> UTC)
  res <- chronon_convert_impl(
    res,
    from = by, to = chronon,
    discrete = FALSE,
    tz = "UTC"
  )

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
  by <- attr(granule@x[[1L]], "chronon")
  by@n <- by@n * as.numeric(granule)
  
  # Inherit non-naive attributes from chronon
  by <- granule_inherit_props(by, chronon <- chronon_common(x))

  # Convert time into the granule chronon
  res <- chronon_convert(x, by)
  # Apply timezone offsets (UTC -> tz)
  res <- res + tz_offset_impl(res, by)
  # Ceiling time (floor + 0.5 to round .0 up)
  res <- ceiling(floor(res) + 0.5)
  
  # Undo timezone offsets (tz -> UTC)
  # Second pass of tz offset for DST changes within the day
  # TODO - ideally this is more directly handled in chronon_convert or similar
  res <- res - tz_offset_impl(res - tz_offset_impl(res, by), by)

  # Convert back to original chronon (includes tz -> UTC)
  res <- chronon_convert_impl(
    res,
    from = by, to = chronon,
    discrete = FALSE,
    tz = "UTC"
  )

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
  by <- attr(granule@x[[1L]], "chronon")
  by@n <- by@n * as.numeric(granule)
  
  # Inherit non-naive attributes from chronon
  by <- granule_inherit_props(by, chronon <- chronon_common(x))

  # Convert time into the granule chronon
  res <- chronon_convert(x, by)
  # Apply timezone offsets (UTC -> tz)
  res <- res + tz_offset_impl(res, by)
  # Floor time
  res <- floor(res)
  
  # Undo timezone offsets (tz -> UTC)
  # Second pass of tz offset for DST changes within the day
  # TODO - ideally this is more directly handled in chronon_convert or similar
  res <- res - tz_offset_impl(res - tz_offset_impl(res, by), by)

  # Convert back to original chronon (includes tz -> UTC)
  res <- chronon_convert_impl(
    res,
    from = by, to = chronon,
    discrete = FALSE,
    tz = "UTC"
  )

  if (is.integer(x)) res <- as.integer(res)
  attributes(res) <- attributes(x)
  res
}
