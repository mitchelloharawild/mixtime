# --- shared helpers ---------------------------------------------------------------

# Shift stored values `x` of `chronon` by `n` units of `unit`, used by both
# time point arithmetic and seq(). `x` and `n` are recycled.
#
# Shifts involving time and duration chronons with irregular cardinalities are
# computed with calendar-field arithmetic, clamping any components that are
# invalid (e.g. 2026-01-31 + months(1) -> 2026-02-28, not 2026-03-03). See
# chronon_recompose() for the `on_invalid` resolutions.
#
# Returns the shifted stored values, and which of them needed resolving.
chronon_shift <- function(
  chronon,
  x,
  unit,
  n,
  on_invalid = "clamp",
  discrete = FALSE
) {
  # Shift in `unit`'s context (e.g. by days of another time zone).
  ctx_chronon <- chronon_in_context(chronon, unit)
  x <- chronon_convert_impl(x, chronon, ctx_chronon, discrete = FALSE)
  invalid <- rep_len(FALSE, max(length(x), length(n)))

  # Calendar arithmetic keeps the local wall-clock reading ("same clock time,
  # N units later"), so it works on local values.
  local <- chronon_to_local(ctx_chronon, x)

  if (chronon_needs_clamping(ctx_chronon, unit)) {
    path <- chronon_longest_path(ctx_chronon, unit)
    decomposed <- chronon_decompose(local, path)
    div <- decomposed$div[[length(path)]] + n
    recomposed <- chronon_recompose(
      div,
      decomposed$mod,
      path,
      on_invalid,
      discrete = discrete
    )
    local <- as.numeric(recomposed$value)
    invalid <- recomposed$invalid
  } else {
    at <- chronon_convert_graph(local, ctx_chronon, unit)
    local <- chronon_convert_graph(at + n, unit, ctx_chronon)
  }
  x <- chronon_from_local(ctx_chronon, local)

  list(
    value = chronon_convert_impl(x, ctx_chronon, chronon, discrete = FALSE),
    invalid = invalid
  )
}

# Shift a time point by a duration
duration_shift <- function(time, dur, on_invalid = "clamp") {
  time_data <- S7_data(time)
  time_chronon <- time@chronon
  # The time point's context applies, rather than one given to the duration.
  dur_chronon <- chronon_in_context(dur@chronon, time_chronon)

  shifted <- chronon_shift(
    time_chronon,
    as.numeric(time_data),
    dur_chronon,
    S7_data(dur),
    on_invalid,
    discrete = is.integer(time_data)
  )
  if (on_invalid == "reject" && any(shifted$invalid)) {
    warn_invalid_reject(shifted$invalid)
  }

  shift <- shifted$value - time_data
  # Preserve integer type for discrete time points
  if (is.integer(time_data)) {
    shift <- as.integer(round(shift))
  }
  shift
}

# Combine two durations (or divide them) via their common chronon.
duration_combine <- function(e1, e2, op) {
  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  tu <- chronon_glb_impl(list(x_chronon, y_chronon))
  # Scale magnitudes to common chronon units before performing arithmetic
  xd <- S7_data(e1) * chronon_cardinality(tu, x_chronon)
  yd <- S7_data(e2) * chronon_cardinality(tu, y_chronon)
  res <- op(xd, yd)
  # duration / duration = numeric ratio; otherwise a duration with the common chronon
  if (identical(op, `/`)) {
    return(res)
  }
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
  stop(
    sprintf("Math function '%s' not supported for continuous time", .fn),
    call. = FALSE
  )
}

# --- differences in time produce durations in the common chronon --------------

method(`-`, list(mt_time, mt_time)) <- function(e1, e2) {
  x_chronon <- e1@chronon
  y_chronon <- e2@chronon
  tu <- chronon_glb_impl(list(x_chronon, y_chronon))
  cx <- chronon_convert(e1, tu, discrete = FALSE)
  cy <- chronon_convert(e2, tu, discrete = FALSE)
  mt_duration(cx - cy, chronon = tu)
}

# --- relative time (linear time and durations) --------------------------------

method(`+`, list(mt_time, mt_duration)) <- function(e1, e2) {
  S7_data(e1) <- S7_data(e1) + duration_shift(e1, e2)
  e1
}

method(`-`, list(mt_time, mt_duration)) <- function(e1, e2) {
  # Negate the duration itself to keep duration_shift() simpler
  S7_data(e2) <- -S7_data(e2)

  S7_data(e1) <- S7_data(e1) + duration_shift(e1, e2)
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

#' Shift time points by a duration
#'
#' Shifts time points by a duration, like `x + by`, with control over how time
#' points that don't exist in the target granule are resolved. Shifting by a
#' duration with an irregular relationship to the time points' chronon (e.g.
#' days within months) can produce time points that don't exist, such as
#' shifting 31 January by one month. The `+` and `-` operators always use
#' `on_invalid = "clamp"`.
#'
#' @param x A time point vector.
#' @param by A time [duration()] (e.g., `months(1L)`), or a character string
#'   specifying one (e.g., `"1 month"`).
#' @param on_invalid How to resolve time points that don't exist in the target
#'   granule:
#'   * `"clamp"` (default): Reduce the invalid component to its last valid
#'     value, preserving finer components (e.g., the time of day).
#'   * `"nearest"`: Use the end of the target granule, so finer components are
#'     not preserved. For discrete time this is the last chronon within the
#'     granule (e.g., 23:59:59 on the last day); for continuous time it is the
#'     end of that chronon (e.g., 00:00:00 on the following day).
#'   * `"overflow"`: Allow the invalid component to overflow into the next
#'     granule.
#'   * `"reject"`: Return `NA` with a warning.
#'
#' @return A time point vector of the same type as `x`.
#'
#' @examples
#' x <- datetime("2021-01-30 09:30:00")
#' time_shift(x, months(1L))
#' time_shift(x, months(1L), on_invalid = "nearest")
#' time_shift(x, months(1L), on_invalid = "overflow")
#' time_shift(x, months(1L), on_invalid = "reject")
#'
#' # Shifting backwards
#' time_shift(date("2021-03-31"), months(-1L), on_invalid = "overflow")
#'
#' @export
time_shift <- new_generic(
  "time_shift",
  "x",
  function(x, by, on_invalid = c("clamp", "nearest", "overflow", "reject")) {
    S7::S7_dispatch()
  }
)
method(time_shift, class_mixtime) <- vecvec::vecvec_apply_fn(time_shift)
method(time_shift, mt_time) <- function(
  x,
  by,
  on_invalid = c("clamp", "nearest", "overflow", "reject")
) {
  on_invalid <- rlang::arg_match(on_invalid)
  if (is.character(by)) {
    by <- parse_time_unit(by)
  }
  if (S7_inherits(by, mt_unit)) {
    by <- duration(1L, by)
  }
  if (is_mixtime(by)) {
    by <- vecvec::unvecvec(by)
  }
  if (!S7_inherits(by, mt_duration)) {
    cli::cli_abort("{.arg by} must be a time duration.")
  }
  S7_data(x) <- S7_data(x) + duration_shift(x, by, on_invalid)
  x
}

# --- combining durations (resulting in common chronon durations) --------------

method(`+`, list(mt_duration, mt_duration)) <- function(e1, e2) {
  duration_combine(e1, e2, `+`)
}
method(`-`, list(mt_duration, mt_duration)) <- function(e1, e2) {
  duration_combine(e1, e2, `-`)
}
method(`/`, list(mt_duration, mt_duration)) <- function(e1, e2) {
  duration_combine(e1, e2, `/`)
}

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
  cli::cli_abort(
    "Multiplication is only supported between a duration and a number.",
    call. = FALSE
  )
}
method(`*`, list(S7::class_any, mt_time)) <- function(e1, e2) {
  cli::cli_abort(
    "Multiplication is only supported between a duration and a number.",
    call. = FALSE
  )
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
