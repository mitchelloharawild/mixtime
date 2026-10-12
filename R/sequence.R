#' Generate sequences of mixtime values
#'
#' Create regular sequences of time values. This method handles both linear
#' time sequences (dates, date-times) and cyclical time sequences (day of week,
#' month of year).
#'
#' @param from Starting value of the sequence.
#' @param to End value of the sequence (if provided).
#' @param by Increment of the sequence. Can be:
#'   * A numeric for the number of time chronons
#'   * A character string specifying the interval (e.g., "1 day", "2 weeks",
#'     "1 month", "1 year")
#'   * A time granule object created with time unit functions (e.g., `cal_gregorian$year(1L)`,
#'     `cal_gregorian$month(1L)`, `cal_gregorian$day(1L)`)
#'   * A time [duration()] object (e.g., `years(1L)`, `months(1L)`, `days(1L)`)
#' @param length.out Desired length of the sequence (alternative to `to`).
#' @param along.with Take the length from this argument (alternative to `length.out`).
#' @param on_invalid How to handle time points that don't exist, either
#'   because they overflow the cycle when using a `by` argument with different
#'   time granule than the sequence chronon, or because they fall in the local
#'   time skipped when daylight saving starts. Defaults to the
#'   `mixtime.on_invalid` option, or `"clamp"` if it isn't set. Options are:
#'   * `"clamp"`: Reduce the overflowing component to its last valid
#'     value, preserving finer components (e.g., the time of day). A skipped
#'     local time moves back by the size of the gap (02:30 becomes 01:30)
#'   * `"previous"`: Use the end of the cycle, so finer components are not
#'     preserved. For discrete time this is the last chronon within the cycle;
#'     for continuous time it is the end of that chronon, the same instant as
#'     `"next"`. A skipped local time becomes the end of the valid time before
#'     the gap (01:59:59)
#'   * `"next"`: Use the start of the following cycle, so finer components are
#'     not preserved. A skipped local time becomes the start of the valid time
#'     after the gap (03:00)
#'   * `"overflow"`: Allow time points to overflow into the next cycle. A
#'     skipped local time moves forward by the size of the gap (02:30 becomes
#'     03:30)
#'   * `"reject"`: Return `NA` with a warning
#'   * `"error"`: Throw an error
#'
#'   This is relevant when the starting time point has an offset that doesn't
#'   exist in all cycles. For example, starting on day 31 with `by = "1 month"`
#'   will overflow in months with fewer than 31 days (e.g., February). With
#'   `"clamp"`, these will be adjusted to the last day of the month (e.g.,
#'   Feb 28/29). With `"next"`, they move to the first day of the next month,
#'   and with `"overflow"`, the extra days carry into the next month.
#'
#'   If neither `on_invalid` nor the `mixtime.on_invalid` option is set and
#'   overflow occurs, a warning is issued with the default `"clamp"` behavior
#'   applied. See [time_shift()] for shifting
#'   time points with the same options.
#' @param ... Additional arguments passed to the underlying sequence method.
#'
#' @return A mixtime vector containing the sequence.
#'
#' @details
#' For linear time types (Date, POSIXct, yearmonth, etc.), sequences progress
#' forward or backward in time. For cyclical time types (month_of_year,
#' day_of_week, etc.), sequences wrap around cyclically.
#'
#' Each time point is `from` (or `to`, if `from` is missing) shifted by a whole
#' number of `by` steps, matching `from + duration(k, by)`. For time-zone-aware times, steps of a day or
#' longer keep the local clock time, while shorter steps (e.g. hours) are real
#' time across daylight saving changes.
#'
#' @examples
#' # Linear time sequences with integer by
#' seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"))
#' seq(yearquarter("2020 Q1"), length.out = 5, by = 3)
#'
#' # Linear time sequences with string intervals
#' seq(date("2020-01-01"), date("2020-12-31"), by = "1 month")
#' seq(yearmonth("2020 Jan"), yearmonth("2025 Jan"), by = "1 year")
#' seq(date("2020-01-01"), length.out = 10, by = "2 weeks")
#'
#' # Linear time sequences incrementing by time granules
#' seq(yearmonth("2020 Jan"), yearmonth(("2020 Dec")), by = cal_gregorian$month(2L))
#' seq(date("2020-01-01"), length.out = 5, by = cal_gregorian$year(1L))
#' seq(date("2020-01-01"), date("2020-01-31"), by = cal_gregorian$day(7L))
#'
#' # Handling invalid dates with on_invalid
#' seq(date("2020-01-31"), length.out = 3, by = "1 month")  # warns, uses clamp
#' seq(date("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "clamp")
#' seq(date("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "next")
#' seq(date("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "overflow")
#'
#' # Cyclical time sequences
#' seq(month_of_year(0L), month_of_year(11L))
#' seq(day_of_week(0L), day_of_week(6L), by = 1)
#'
#' @rdname seq.mixtime
#' @aliases seq.mixtime
#' @method seq mixtime::mixtime
#' @export
`seq.mixtime::mixtime` <- function(...) {
  # Strip mixtime vecvec wrapper
  arg <- lapply(
    rlang::list2(...),
    function(x) if (is_mixtime(x)) vecvec::unvecvec(x) else x
  )

  # Call seq method with bare vectors
  new_mixtime(rlang::exec(seq, !!!arg))
}

#' @rdname seq.mixtime
#' @method seq mixtime::mt_time
#' @export
`seq.mixtime::mt_time` <- function(
  from,
  to,
  by,
  length.out = NULL,
  along.with = NULL,
  on_invalid = getOption("mixtime.on_invalid", "clamp"),
  ...
) {
  if (!is.null(along.with)) {
    length.out <- length(along.with)
  } else if (!is.null(length.out)) {
    if (length(length.out) != 1L) {
      cli::cli_abort("{.arg length.out} must be of length 1.")
    }
    length.out <- ceiling(length.out)
  }

  missing_from <- missing(from)
  missing_to <- missing(to)
  missing_by <- missing(by)

  n_provided <- sum(!missing_from, !missing_to, !is.null(length.out))
  if (missing_by && n_provided < 2L) {
    cli::cli_abort(
      "When {.arg by} is missing, at least two of {.arg from}, {.arg to}, or {.arg length.out} must be specified."
    )
  }
  if (!missing_by && n_provided != 2L) {
    cli::cli_abort(
      "When {.arg by} is provided, exactly two of {.arg from}, {.arg to}, or {.arg length.out} must be specified."
    )
  }

  ptype <- if (missing_from) to else from
  # Sequence chronon
  chronon <- chronon_glb(ptype)

  # Check that from and to have the same time classes
  if (!missing_from && !missing_to) {
    to_chronon <- chronon_glb(to)
    if (!identical(chronon, to_chronon)) {
      cli::cli_abort(c(
        "Incompatible time chronons in {.fn seq}.",
        "i" = "{.arg from} has {.cls {time_granule_label(chronon)}} chronons.",
        "i" = "{.arg to} has {.cls {time_granule_label(to_chronon)}} chronons."
      ))
    }
  }

  if (missing_by) {
    arg <- list(length.out = length.out)
    if (!missing_from) {
      arg$from <- as.numeric(from)
    }
    if (!missing_to) {
      arg$to <- as.numeric(to)
    }
    return(vec_restore(rlang::exec(seq.int, !!!arg), ptype))
  }

  # Allow duration input for by, but convert to granule
  if (length(by) != 1L) {
    cli::cli_abort("{.var by} must be a single time duration", call. = FALSE)
  }
  if (time_is_duration(by)) {
    by_size <- as.numeric(by)
    by <- attr(by, "chronon")
    by@n <- by@n * by_size
  }

  # Parse `by` argument
  if (is.character(by)) {
    by <- parse_time_unit(by)
  }
  # Numeric `by` uses chronon
  if (is.numeric(by) && !S7_inherits(by, mt_unit)) {
    by <- S7::S7_class(chronon)(by)
  }

  # Inherit granule properties from `by` if not given in `chronon` granule
  by <- granule_inherit_props(by, chronon)
  # A negative `by` steps backwards by whole units of `by`
  step <- if (by@n < 0) -1L else 1L
  # Shift by multiples of `by`'s single unit, as `+` does (`months(18L)` is
  # 18 months, not one 18-month window). Calendar shifts add to the unit's
  # count after decomposing the anchor once, which only matches repeated
  # shifts of a multi-unit `by` window when its size divides the calendar
  # cycle (e.g. 18 months drift against years).
  by_n <- abs(by@n)
  by@n <- 1L

  # Warn when clamping by default, unless the default was chosen with the
  # `mixtime.on_invalid` option
  missing_on_invalid <- missing(on_invalid) &&
    is.null(getOption("mixtime.on_invalid"))
  check_on_invalid(on_invalid)

  # Each time point is shifted from an anchor by a whole number of `by` units
  # (`anchor + duration(k, by)`), rather than from the previous time point.
  # The anchor is `from`, or `to` (right aligned) if `from` is missing.
  anchor <- as.numeric(ptype)
  if (!missing_to) {
    to <- as.numeric(to)
  }
  local_chronon <- chronon_in_context(chronon, by)
  k <- if (missing_from) {
    step * (seq_len(length.out) - length.out)
  } else if (missing_to) {
    step * (seq_len(length.out) - 1L)
  } else if (chronon_shift_is_exact(local_chronon, by)) {
    # Whole `by` steps of real time from `from` to `to`
    d <- chronon_to_utc(chronon, to) - chronon_to_utc(chronon, anchor)
    d <- d / (chronon_fixed_seconds(by) * by_n)
    seq.int(0L, step * floor_ulp(step * d), by = step)
  } else {
    # Whole units of the local calendar from `from` to `to`, found by
    # shifting `to` left by `from`'s offset into its unit. A `by` boundary
    # (e.g. "start of month") is defined on the local wall clock of `by`'s
    # context, the same convention `chronon_parts()` uses.
    local <- chronon_to_local(
      local_chronon,
      chronon_convert_impl(c(anchor, to), chronon, local_chronon, discrete = FALSE)
    )
    from_divmod <- chronon_divmod(chronon, by, local[1L])
    to_divmod <- chronon_divmod(chronon, by, local[2L] - from_divmod$mod)
    # Stepping backwards, round up to stay at or after `to`
    to_div <- to_divmod$div + (step < 0 & to_divmod$mod != 0)
    # Whole `by` steps within those units
    n_steps <- floor(step * (to_div - from_divmod$div) / by_n)
    seq.int(0L, step * n_steps, by = step)
  }

  shifted <- chronon_shift(
    chronon,
    anchor,
    by,
    k * by_n,
    on_invalid,
    discrete = is.integer(ptype)
  )
  res <- shifted$value

  if (missing_on_invalid && any(shifted$invalid & !shifted$skipped)) {
    cli::cli_warn(c(
      "Some time points in the sequence fall past the end of a shorter {time_unit_plural(by, 1L)}.",
      "!" = "Clamping them to the last valid time point in that {time_unit_plural(by, 1L)}, {.code on_invalid = \"clamp\"} (the default).",
      "i" = "Specify {.arg on_invalid}, or set the {.code mixtime.on_invalid} option, to suppress this warning."
    ))
  }
  if (missing_on_invalid && any(shifted$skipped)) {
    n_skipped <- sum(shifted$skipped)
    cli::cli_warn(c(
      "{n_skipped} time point{?s} in the sequence {?falls/fall} in local time skipped by a time zone change (e.g. when daylight saving starts).",
      "!" = "Moving {cli::qty(n_skipped)}{?it/them} back by the size of the gap, {.code on_invalid = \"clamp\"} (the default).",
      "i" = "Specify {.arg on_invalid}, or set the {.code mixtime.on_invalid} option, to suppress this warning."
    ))
  }
  signal_invalid(shifted$invalid, on_invalid)

  if (!missing_from && !missing_to) {
    # A local time that doesn't exist can move forward, past `to`
    past <- step * (res - to) > 8 * .Machine$double.eps * pmax(abs(to), 1)
    res <- res[!(past %in% TRUE)]
  }

  # Restore integer type for discrete time input, rounding like `+` does
  if (is.integer(ptype)) res <- as.integer(round(res))

  vec_restore(res, ptype)
}

# #' @rdname seq.mixtime
# #' @export
# seq.mt_cyclical <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
#   if (!is.null(along.with)) {
#     length.out <- length(along.with)
#   }
#   else if (!is.null(length.out)) {
#     if (length(length.out) != 1L)
#       stop(sprintf("'%s' must be of length 1", length.out))
#     length.out <- ceiling(length.out)
#   }

#   # Capture extra arguments
#   arg <- rlang::list2(...)
#   if (!missing(by)) arg$by <- by
#   if (!is.null(length.out)) arg$length.out <- length.out
#   # NB: Passing along.with = NULL to seq.int causes an overflow in R
#   if (!is.null(along.with)) arg$along.with <- along.with

#   # Parse `by` argument
#   if (is.character(arg$by)) arg$by <- parse_time_unit(arg$by)

#   # Convert mt_cyclical to numeric for seq() method
#   if (!missing(from) && inherits(from, "mt_cyclical")) {
#     ptype <- from
#     arg$from <- from@n
#   }
#   if (!missing(to) && inherits(to, "mt_cyclical")) {
#     arg$to <- vctrs::vec_data(to)
#     if (!is.null(arg$from)) {
#       # Require compatible cyclical time objects for from:to
#       vec_assert(to, from)
#     } else {
#       ptype <- to
#     }
#   }

#   # Cyclical period
#   period <- chronon_cardinality(attr(ptype, "chronon"), attr(ptype, "cycles")[[1L]])

#   # Adjust from:to for looping around cycles
#   if (!is.null(arg$to) && !is.null(arg$from)) {
#     if (arg$by%||%1 > 0 && to < from) {
#       arg$to <- arg$to + period
#     } else if (to > from && arg$by%||%1 < 0) {
#       arg$to <- arg$to - period
#     }
#   }

#   # Convert `by` to match `ptype` units
#   if (!is.null(arg$by) && S7_inherits(arg$by, mt_unit)) {
#     arg$by <- chronon_cardinality(time_chronon(ptype), arg$by)
#   }

#   # Generate linear sequence
#   res <- rlang::inject(seq.int(!!!arg))

#   # Compute cyclical component
#   res <- res %% period

#   # Restore integer type for discrete time input
#   if (is.integer(ptype)) res <- as.integer(res)

#   # Restore cyclical time attributes
#   vec_restore(res, ptype)
# }
