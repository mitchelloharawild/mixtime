#' Convert between chronons
#'
#' @param x A linear time object (of class `mt_linear`)
#' @param to The target chronon to convert to (a time granule object)
#' @param discrete If `TRUE`, the number of target chronons since Unix epoch
#' that `x` falls into is returned as an integer. If `FALSE`, a fractional
#' number of target chronons is returned (analagous to time using a continuous
#' time model).
#' @param allow_indeterminate Whether to allow a conversion that is temporally
#' indeterminate, i.e. a discrete `x` whose chronon does not nest within `to`
#' (such as the day of a month). If `TRUE` (the default), each time point is
#' converted from its start. If `FALSE`, the conversion errors.
#' @param ... Additional arguments for methods.
#'
#' @return A numeric or integer representing the time since Unix epoch in terms of the target
#' chronon's precision.
#'
#' @examples
#'
#' # Convert from months since epoch to years since epoch
#' chronon_convert(yearmonth(Sys.Date()), cal_gregorian$year(1L))
#'
#' # Convert from days since epoch to months since epoch
#' chronon_convert(Sys.Date(), cal_gregorian$month(1L))
#' chronon_convert(Sys.Date(), cal_gregorian$month(1L), discrete = TRUE)
#'
#' # Convert from seconds since epoch to hours since epoch
#' chronon_convert(Sys.time(), cal_gregorian$hour(1L))
#'
#' @noRd
#' @keywords internal
chronon_convert <- S7::new_generic("chronon_convert", "x")

chronon_convert_impl <- function(x, from, to, discrete) {
  # Inherit granule properties from `from` if not given in `to` granule
  to <- granule_inherit_props(to, from)
  x <- as.double(vec_data(x))
  chronon_check_context(from, to)

  if (granule_identical(from, to)) {
    x <- if (discrete) floor_ulp(x) else x
  } else if (chronon_shares_frame(from, to)) {
    # Same context, stored as local values: the context-blind graph applies.
    x <- chronon_convert_graph(x, from, to, discrete)
  } else if (chronon_drops_context(from, to)) {
    # Zoned -> naive keeps the local wall-clock reading.
    x <- chronon_convert_graph(chronon_to_local(from, x), from, to, discrete)
  } else if (chronon_gains_context(from, to)) {
    # Naive -> located reads the naive value as the local wall-clock reading.
    x <- chronon_convert_graph(x, from, to)
    x <- chronon_from_local(to, x, discrete)$value
  } else {
    # Otherwise go through absolute time (UTC seconds).
    x <- chronon_from_utc(to, chronon_to_utc(from, x), discrete)
  }

  if (discrete) as.integer(x) else x
}

# Context-blind conversion of `x` between chronons along the chronon_divmod
# graph. Properties such as a time zone are ignored, so `x` must already be in
# the frame the graph expects (UTC, or local wall-clock time).
chronon_convert_graph <- function(x, from, to, discrete = FALSE) {
  path <- S7_graph_dispatch(chronon_divmod_graph(), from, to)

  # Only finite values have a position to convert: NA stays NA and +/-Inf is
  # the same infinite count in any chronon.
  finite <- is.finite(x)
  y <- x[finite]

  if (length(path) == 1L) {
    # Simple direct conversion by cardinality
    y <- y / chronon_cardinality(from, to, at = y)
  } else {
    # Convert along the path of time granules
    path[[1]] <- from
    path[[length(path)]] <- to
    # Initialise intermediate classes with 1L and adjacent properties
    path[c(-1, -length(path))] <- lapply(
      path[c(-1, -length(path))],
      function(tu) {
        # Ideally inherit from `to`, but if incomplete inherit from `from`
        granule_inherit_props(granule_inherit_props(tu(1L), to), from)
      }
    )

    # Convert chronons along the path
    for (i in seq(2, length.out = length(path) - 1)) {
      res <- chronon_divmod_dispatch(path[[i - 1L]], path[[i]], y)
      y <- res$div
      nz_mod <- res$mod != 0
      part <- chronon_cardinality(
        path[[i - 1L]],
        path[[i]],
        floor(res$div[nz_mod])
      )
      y[nz_mod] <- y[nz_mod] + res$mod[nz_mod] / part
    }
  }
  x[finite] <- y

  if (discrete) floor_ulp(x) else x
}

# Floor, after nudging up by a few ULPs so that floating-point error
# accumulated along a conversion can't drop a value that lands just below an
# exact unit boundary into the previous unit.
floor_ulp <- function(x) {
  floor(x + 8 * .Machine$double.eps * pmax(abs(x), 1))
}

# Snap values within a few ULPs of a whole number onto it, removing
# floating-point error (e.g. a continuous time that should be exactly 03:30:00
# but is stored a hair below it).
round_ulp <- function(x) {
  r <- round(x)
  near <- (abs(x - r) <= 8 * .Machine$double.eps * pmax(abs(x), 1)) %in% TRUE
  x[near] <- r[near]
  x
}

method(chronon_convert, mt_linear) <- function(
  x,
  to,
  discrete = FALSE,
  allow_indeterminate = TRUE,
  ...
) {
  from <- attr(x, "chronon")
  if (!allow_indeterminate) check_convert_determinate(x, from, to)
  chronon_convert_impl(vec_data(x), from, to, discrete)
}

method(chronon_convert, class_mixtime) <- vecvec::vecvec_apply_fn(
  chronon_convert,
  SIMPLIFY = TRUE
)

method(chronon_convert, S7::class_any) <- function(
  x,
  to,
  discrete = FALSE,
  allow_indeterminate = TRUE,
  ...
) {
  from <- chronon_glb(x)
  if (!allow_indeterminate) check_convert_determinate(x, from, to)
  chronon_convert_impl(as.numeric(x), from, to, discrete)
}

# A discrete time spans its whole chronon, so converting it to a chronon it
# doesn't nest within is temporally indeterminate (e.g. the day of the week of
# a month). Checked once from the chronons rather than element-wise; times from
# other classes (Date, POSIXct, ...) are discrete.
check_convert_determinate <- function(x, from, to) {
  discrete <- !S7_inherits(x, mt_time) || is.integer(vec_data(x))
  if (chronon_is_determinate_at(from, to, discrete)) {
    return(invisible())
  }
  cli::cli_abort(
    c(
      "Cannot convert {time_granule_label(from)} to {time_granule_label(to)}, the result is temporally indeterminate.",
      "x" = "A discrete {.field {time_granule_label(from)}} spans a period of time that {time_granule_label(to)} cannot identify.",
      "i" = "Convert from a time point at least as precise as {time_granule_label(to)}, or use continuous time."
    ),
    class = "mixtime_error_indeterminate",
    call = NULL
  )
}
