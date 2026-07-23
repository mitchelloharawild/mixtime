# --- shared helpers ---------------------------------------------------------------

# Shift a time point by a duration: returns the raw chronon-count shift (in the time's own
# chronon) that corresponds to `dur`, evaluated at the time point's position (so that
# variable-length granules advance correctly).
duration_shift <- function(time, dur) {
  time_data <- S7_data(time)
  time_chronon <- time@chronon
  dur_chronon <- dur@chronon
  # `at` must be in units of the coarser chronon (the duration's), not the time's chronon
  at <- floor(chronon_convert_impl(time_data, time_chronon, dur_chronon, discrete = FALSE))
  cardinality <- chronon_cardinality(time_chronon, dur_chronon, at = at)
  shift <- S7_data(dur) * cardinality
  # Preserve integer type for discrete time points
  if (is.integer(time_data)) shift <- as.integer(round(shift))
  shift
}

# Combine two durations (or divide them) via their common chronon.
duration_combine <- function(e1, e2, op) {
  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  tu <- chronon_common_impl(list(x_chronon, y_chronon))
  # Scale magnitudes to common chronon units before performing arithmetic
  xd <- S7_data(e1) * chronon_cardinality(tu, x_chronon)
  yd <- S7_data(e2) * chronon_cardinality(tu, y_chronon)
  res <- op(xd, yd)
  # duration / duration = numeric ratio; otherwise a duration with the common chronon
  if (identical(op, `/`)) return(res)
  mt_duration(res, chronon = tu)
}

# --- math ---------------------------------------------------------------------
#' @importFrom vctrs vec_math
#' @export
S7::method(vec_math, mt_time) <- function(.fn, .x, ...) {
  if (.fn == "mean") {
    res <- vctrs::vec_math_base(.fn, .x, ...)
    if (is.integer(.x)) {
      res <- as.integer(res)
    }
    return(vec_restore(res, .x))
  }
  if (.fn %in% c("is.nan", "is.finite", "is.infinite")) {
    return(vctrs::vec_math_base(.fn, .x, ...))
  }
  if (.fn %in% c("round", "floor", "ceiling")) {
    res <- vctrs::vec_math_base(.fn, .x, ...)
    return(vec_restore(res, .x))
  }
  stop(sprintf("Math function '%s' not supported for continuous time", .fn), call. = FALSE)
}

# --- differences in time produce durations in the common chronon --------------

method(`-`, list(mt_time, mt_time)) <- function(e1, e2) {
  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  tu <- chronon_common_impl(list(x_chronon, y_chronon))
  cx <- chronon_convert(e1, tu, discrete = FALSE)
  cy <- chronon_convert(e2, tu, discrete = FALSE)
  # When a TZ-aware operand uses a coarser granule than the common chronon, the conversion
  # produces a wall-clock value rather than a UTC-aligned value (the TZ offset cancels in
  # same-TZ coarser->finer conversions). Subtract the TZ offset at the wall-clock position
  # to realign to UTC.
  cx <- tz_wall_clock_to_utc(cx, x_chronon, tu)
  cy <- tz_wall_clock_to_utc(cy, y_chronon, tu)
  mt_duration(cx - cy, chronon = tu)
}

# --- relative time (linear time and durations) --------------------------------

method(`+`, list(mt_time, mt_duration)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) + duration_shift(e1, e2)
  e1
}

method(`-`, list(mt_time, mt_duration)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) - duration_shift(e1, e2)
  e1
}

method(`+`, list(mt_duration, mt_time)) <- function(e1, e2) {
  S7_data(e2) <- duration_shift(e2, e1) + S7_data(e2)
  e2
}

method(`-`, list(mt_duration, mt_time)) <- function(e1, e2) {
  S7_data(e2) <- duration_shift(e2, e1) - S7_data(e2)
  e2
}

# --- combining durations (resulting in common chronon durations) --------------

method(`+`, list(mt_duration, mt_duration)) <- function(e1, e2) duration_combine(e1, e2, `+`)
method(`-`, list(mt_duration, mt_duration)) <- function(e1, e2) duration_combine(e1, e2, `-`)
method(`/`, list(mt_duration, mt_duration)) <- function(e1, e2) duration_combine(e1, e2, `/`)

# --- implicit chronons (time and numeric) -------------------------------------

method(`+`, list(mt_time, S7::class_numeric)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) + e2
  e1
}
method(`-`, list(mt_time, S7::class_numeric)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) - e2
  e1
}
method(`+`, list(S7::class_numeric, mt_time)) <- function(e1, e2) {
  S7_data(e2) <- e1 + S7_data(e2)
  e2
}
method(`-`, list(S7::class_numeric, mt_time)) <- function(e1, e2) {
  S7_data(e2) <- e1 - S7_data(e2)
  e2
}

# --- numeric scaling of durations (e.g. days(3) * 2) --------------------------

method(`*`, list(mt_duration, S7::class_numeric)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) * e2
  e1
}
method(`/`, list(mt_duration, S7::class_numeric)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) / e2
  e1
}
method(`*`, list(S7::class_numeric, mt_duration)) <- function(e1, e2) {
  S7_data(e2) <- e1 * S7_data(e2)
  e2
}

# `mt_cyclical` has no dedicated numeric method: its stored value is a raw chronon count
# (like `mt_linear`), and the cyclical "position within the cycle" is purely a display-time
# computation from that raw count (see format.R). So `mt_cyclical + <number>` falls through
# to the general `(mt_time, class_numeric)` method above, which just advances the raw
# chronon and lets formatting recompute the position - this also makes variable-length
# cycles (e.g. day_of_month()) advance correctly, since there is no fixed period to wrap
# against.

# --- friendly errors for unsupported operator/operand combinations ----------------
# Registered on `class_any` so any operand pair that isn't handled above lands here. The
# concrete methods above are more specific and always take precedence.

method(`+`, list(mt_time, S7::class_any)) <- function(e1, e2) {
  cli::cli_abort(
    "Addition is only supported between a time point and a duration, or between durations.",
    call. = FALSE
  )
}
method(`+`, list(S7::class_any, mt_time)) <- function(e1, e2) {
  cli::cli_abort(
    "Addition is only supported between a time point and a duration, or between durations.",
    call. = FALSE
  )
}
method(`*`, list(mt_time, S7::class_any)) <- function(e1, e2) {
  cli::cli_abort("Multiplication is only supported between a duration and a number.", call. = FALSE)
}
method(`*`, list(S7::class_any, mt_time)) <- function(e1, e2) {
  cli::cli_abort("Multiplication is only supported between a duration and a number.", call. = FALSE)
}
method(`/`, list(mt_time, S7::class_any)) <- function(e1, e2) {
  cli::cli_abort(
    "Division is only supported between two durations, or a duration divided by a number.",
    call. = FALSE
  )
}
method(`/`, list(S7::class_any, mt_time)) <- function(e1, e2) {
  cli::cli_abort(
    "Division is only supported between two durations, or a duration divided by a number.",
    call. = FALSE
  )
}

# --- helpers ----------------------------------------------------------------------

# Correct wall-clock -> UTC alignment for coarser-granule TZ-aware operands.
# When from_chronon has a non-UTC TZ and is a different (coarser) class than to_chronon,
# the chronon_convert result is a wall-clock value (the offset was added then canceled).
# Subtracting the TZ offset at that position restores the true UTC-equivalent value.
tz_wall_clock_to_utc <- function(value, from_chronon, to_chronon) {
  if (!S7_inherits(from_chronon, mt_tz_unit)) return(value)
  from_tz <- tz_name(from_chronon)
  if (is.na(from_tz) || from_tz == "UTC") return(value)
  # Same granule class: offset already cancelled correctly (no correction needed)
  if (identical(S7::S7_class(from_chronon)@name, S7::S7_class(to_chronon)@name)) return(value)
  # Coarser granule: subtract the TZ offset at the wall-clock position
  tzo <- get_tz_offset(as.double(value), from_tz)
  value - tzo
}
