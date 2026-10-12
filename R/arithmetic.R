# --- shared helpers ---------------------------------------------------------------

# Shift stored values `x` of `chronon` by `n` units of `unit`, used by both
# time point arithmetic and seq(). `x` and `n` are recycled.
#
# Shifts involving time and duration chronons with irregular cardinalities are
# computed with calendar-field arithmetic, clamping any components that are
# invalid (e.g. 2026-01-31 + months(1) -> 2026-02-28, not 2026-03-03). A local
# time skipped by daylight saving is invalid in the same way. See
# chronon_recompose() and chronon_from_local() for the `on_invalid`
# resolutions.
#
# Returns the shifted stored values, which of them needed resolving, and which
# of those were local times skipped by a time zone change.
chronon_shift <- function(
  chronon,
  x,
  unit,
  n,
  on_invalid = "clamp",
  discrete = FALSE
) {
  # A duration has no position, so a multi-unit `unit` (e.g. 18 months) is a
  # count of its single unit, not a window counted from the epoch.
  if (unit@n != 1L) {
    n <- n * unit@n
    unit@n <- 1L
  }

  if (discrete) check_shift_determinate(chronon, unit, n)

  # Calendar shifts decompose through the chronon's class (e.g. minute -> hour
  # -> day -> year), which is only defined for single-unit chronons. A
  # multi-unit chronon (e.g. 30 minutes) is shifted as its single unit.
  if (chronon@n != 1L) {
    base <- chronon
    base@n <- 1L
    x <- chronon_convert_impl(x, chronon, base, discrete = FALSE)
    shifted <- chronon_shift(base, x, unit, n, on_invalid, discrete)
    shifted$value <- chronon_convert_impl(
      shifted$value,
      base,
      chronon,
      discrete = FALSE
    )
    return(shifted)
  }

  # Shift in `unit`'s context (e.g. by days of another time zone).
  ctx_chronon <- chronon_in_context(chronon, unit)
  x <- chronon_convert_impl(x, chronon, ctx_chronon, discrete = FALSE)
  invalid <- NULL
  skipped <- NULL

  if (chronon_shift_is_exact(ctx_chronon, unit)) {
    # Exact arithmetic adds real time ("N hours later"), on absolute time.
    s <- chronon_to_utc(ctx_chronon, x) + n * chronon_fixed_seconds(unit)
    x <- chronon_from_utc(ctx_chronon, s)
  } else {
    # Calendar arithmetic keeps the local wall-clock reading ("same clock
    # time, N units later"), so it works on local values.
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
    resolved <- chronon_from_local(
      ctx_chronon,
      local,
      discrete = discrete,
      on_invalid = on_invalid
    )
    x <- resolved$value
    skipped <- resolved$invalid
    invalid <- if (is.null(invalid)) skipped else invalid | skipped
  }

  list(
    value = chronon_convert_impl(x, ctx_chronon, chronon, discrete = FALSE),
    invalid = invalid %||% rep_len(FALSE, length(x)),
    skipped = skipped %||% rep_len(FALSE, length(x))
  )
}

# A discrete time point spans its whole chronon, so shifting it is only
# determinate when every instant within it shifts into the same chronon: when
# the chronon nests within `unit` (calendar arithmetic shifts each instant's
# components alike), or when the shift is a whole number of chronons (a week by
# 14 days, but not 11). Otherwise, as for conversions (see
# check_convert_determinate()), the shift is refused.
check_shift_determinate <- function(chronon, unit, n) {
  if (chronon_nests_in(chronon, unit)) {
    return(invisible())
  }
  ok <- chronon_fixed_multiple(chronon, unit, n)
  if (all(ok)) {
    return(invisible())
  }
  by <- time_granule_label(unit, abs(n[!ok][[1L]]))
  label <- time_granule_label(chronon)
  # A duration from another calendar can share a name with one that would
  # nest (a Gregorian year is not an ISO week-numbering year), so name both.
  cal <- time_calendar(chronon)
  unit_cal <- time_calendar(unit)
  other_cal <- !is.null(cal) &&
    !is.null(unit_cal) &&
    !S7_class_id(unit) %in% vapply(cal, S7_class_id, character(1L))

  cli::cli_abort(
    c(
      "Cannot shift {label} time points by {by}, the result is temporally indeterminate.",
      "x" = "A discrete {.field {label}} spans a period of time that doesn't shift as a whole by {by}.",
      "!" = if (other_cal) "{by} is from {.code {class(unit_cal)[[1L]]}}, while {label} is from {.code {class(cal)[[1L]]}}.",
      "i" = "Shift by a duration that {label} {cli::qty(chronon@n)}nest{?s/} within or that is a multiple of {label}, or use continuous time."
    ),
    class = "mixtime_error_indeterminate",
    call = NULL
  )
}

# Shift a time point by a duration
duration_shift <- function(
  time,
  dur,
  on_invalid = getOption("mixtime.on_invalid", "clamp")
) {
  check_on_invalid(on_invalid, call = NULL)
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
  signal_invalid(shifted$invalid, on_invalid)

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
  # A discrete time point spans a whole chronon (e.g. year(2027L) spans 365
  # days), so its difference from a time point of another chronon depends on
  # which instant within it is measured from. Only continuous time points are
  # instants that can be compared across chronons.
  discrete <- c(is.integer(S7_data(e1)), is.integer(S7_data(e2)))
  if (!identical(x_chronon, y_chronon) && any(discrete)) {
    cli::cli_abort(
      c(
        "The difference between time points of different chronons is indeterminate when either is discrete.",
        "x" = "A discrete {.field {time_granule_label(if (discrete[1]) x_chronon else y_chronon)}} spans a period of time, not a single instant.",
        "i" = "Convert the time points to a common chronon, or use continuous time points."
      ),
      call. = FALSE
    )
  }
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
#' shifting 31 January by one month. In a time zone, shifting by days or
#' longer keeps the local clock time, which may have been skipped when daylight
#' saving starts (e.g. 02:30 on the day clocks go forward from 02:00 to 03:00).
#' The `+` and `-` operators use the default strategy, set by the
#' `mixtime.on_invalid` option (`"clamp"` unless set).
#'
#' @param x A time point vector.
#' @param by A time [duration()] (e.g., `months(1L)`), or a character string
#'   specifying one (e.g., `"1 month"`).
#' @param on_invalid How to resolve time points that don't exist in the target
#'   granule. Defaults to the `mixtime.on_invalid` option, or `"clamp"` if it
#'   isn't set:
#'   * `"clamp"`: Reduce the invalid component to its last valid
#'     value, preserving finer components (e.g., the time of day). A skipped
#'     local time moves back by the size of the gap (02:30 becomes 01:30).
#'   * `"previous"`: Use the last valid time point before the invalid one, the
#'     end of the target granule, so finer components are not preserved. For
#'     discrete time this is the last chronon within the granule (e.g.,
#'     23:59:59 on the last day); for continuous time it is the end of that
#'     chronon, the same instant as `"next"`. A skipped local time becomes the
#'     end of the valid time before the gap (01:59:59).
#'   * `"next"`: Use the first valid time point after the invalid one, the
#'     start of the following granule, so finer components are not preserved
#'     (e.g., 00:00:00 on the first day of the following month). A skipped
#'     local time becomes the start of the valid time after the gap (03:00).
#'   * `"overflow"`: Allow the invalid component to overflow into the next
#'     granule. A skipped local time moves forward by the size of the gap
#'     (02:30 becomes 03:30).
#'   * `"reject"`: Return `NA` with a warning.
#'   * `"error"`: Throw an error.
#'
#'   `"previous"` and `"next"` preserve the order of the time points being
#'   shifted, although distinct time points may become equal. `"clamp"` and
#'   `"overflow"` do not: shifting `1955-01-29 23:00:00` and
#'   `1955-01-31 08:00:00` by one month clamps them to `1955-02-28 23:00:00`
#'   and `1955-02-28 08:00:00`, reversing their order.
#'
#' @return A time point vector of the same type as `x`.
#'
#' @examples
#' x <- datetime("2021-01-30 09:30:00")
#' time_shift(x, months(1L))
#' time_shift(x, months(1L), on_invalid = "previous")
#' time_shift(x, months(1L), on_invalid = "next")
#' time_shift(x, months(1L), on_invalid = "overflow")
#' time_shift(x, months(1L), on_invalid = "reject")
#' try(time_shift(x, months(1L), on_invalid = "error"))
#'
#' # Shifting backwards
#' time_shift(date("2021-03-31"), months(-1L), on_invalid = "overflow")
#'
#' # 02:30 doesn't exist on 2025-10-05 in Melbourne (clocks go forward at 02:00)
#' x <- datetime(as.POSIXct("2025-10-04 02:30", tz = "Australia/Melbourne"))
#' time_shift(x, days(1L))
#' time_shift(x, days(1L), on_invalid = "previous")
#' time_shift(x, days(1L), on_invalid = "next")
#' time_shift(x, days(1L), on_invalid = "overflow")
#'
#' @export
time_shift <- new_generic(
  "time_shift",
  "x",
  function(x, by, on_invalid = getOption("mixtime.on_invalid", "clamp")) {
    S7::S7_dispatch()
  }
)
method(time_shift, class_mixtime) <- vecvec::vecvec_apply_fn(time_shift)
method(time_shift, mt_time) <- function(
  x,
  by,
  on_invalid = getOption("mixtime.on_invalid", "clamp")
) {
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
