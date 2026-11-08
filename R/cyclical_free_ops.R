#' Arithmetic for underspecified cyclical time (`mt_cyclical_free`)
#'
#' @description
#' Underspecified cyclical time (see [cyclical_time()]) is a position within a
#' cycle whose enclosing cycle is unknown, e.g. `month_of_year(3)` is
#' "March of some year". Only operations that don't depend on the enclosing
#' cycle are defined:
#'
#' - **Differences**: `a - b` between values of the same cycle is a
#'   [duration()] in their common chronon, the difference between their
#'   positions within the cycle (`month_of_year(3) - month_of_year(1)` is 2
#'   months, and `month_of_year(1) - month_of_year(3)` is -2 months). This
#'   follows the linear ordering of positions within one cycle (see
#'   [mt_cyclical_free-compare]), for cycles of fixed and variable length
#'   alike (`day_of_month(31) - day_of_month(1)` is 30 days).
#' - **Shifts**: `x + n` and `x - n` shift by `n` chronons, where `n` is a
#'   number or a [duration()] that is a whole number of the value's chronons
#'   with a fixed ratio (e.g. `month_of_year(1) + months(2)` or
#'   `time_of_day(3600) + hours(2)`). Shifts wrap around cycles of a fixed
#'   length (`month_of_year(12) + 1` is January, `day_of_week(1) - 1` is
#'   Sunday), so `a + (b - a) == b`. Cycles of variable length (e.g.
#'   [day_of_month()], [day_of_year()] and [week_of_year()]) can't be shifted,
#'   whatever the values: `day_of_month(28) + 1` is day 29, or day 1 in a
#'   non-leap February.
#' - [seq()] follows the same rules as shifts. The sequence runs linearly
#'   within one cycle between `from` and `to` (so `seq(month_of_year(11),
#'   month_of_year(2))` counts down from November to February), while
#'   sequences of a given `length.out` wrap around the cycle.
#'
#' Everything else is an error: shifting by a duration that isn't a whole
#' number of chronons with a fixed ratio (`month_of_year(1) + days(1)`), or
#' that spans a whole cycle or more (`month_of_year(1) + years(1)` would move
#' to another, unknown, year); differences between values of differing
#' cycles, or between an underspecified value and a linear or located time
#' point; subtracting an underspecified value from a number or duration; multiplying or
#' dividing; and summaries that create new values, such as [mean()] or
#' [cumsum()]. Comparisons and ordering ([sort()], [min()], [max()],
#' [range()]) are defined, see [mt_cyclical_free-compare].
#'
#' To do any of these, locate the value in a specific enclosing cycle (e.g. a
#' particular month for a day of month) with [time_compose()], giving linear
#' time, e.g.
#' `time_compose(yearmonth("2024-02"), day_of_month(28)) + days(1)`.
#'
#' Missing values stay missing.
#'
#' @param e1,e2 Operands, at least one an underspecified (`mt_cyclical_free`) vector.
#'
#' @return For differences, a duration ([mt_duration]). For shifts, a
#'   `mt_cyclical_free` vector of the same type as the underspecified operand.
#'
#' @examples
#' month_of_year(12) + 1L
#' month_of_year(1) - months(1)
#' day_of_week(6) + 0:3
#' month_of_year(3) - month_of_year(1)
#' day_of_month(31) - day_of_month(1)
#' seq(day_of_week(6), by = 1, length.out = 4)
#'
#' # Cycles of variable length can't be shifted until located
#' try(day_of_month(28) + 1L)
#' time_compose(yearmonth("2024-02"), day_of_month(28)) + days(1)
#'
#' @usage
#' \S4method{+}{mt_cyclical_free}(e1, e2)
#' \S4method{-}{mt_cyclical_free}(e1, e2)
#'
#' @aliases +,mt_cyclical_free-method -,mt_cyclical_free-method
#' @name mt_cyclical_free-arithmetic
NULL

# --- errors -------------------------------------------------------------------

# The single error for operations a free value doesn't support. `what` and
# `why` are plain strings (already formatted). The hint anchors `x` with
# time_compose(); `example` is a sprintf() template wrapping the anchored
# expression (e.g. "%s + days(1)"), or `hint` replaces the hint entirely.
abort_free_cyclical_op <- function(x, what, why, example = "%s", hint = NULL) {
  if (is.null(hint)) {
    hint <- free_anchor_hint(x, example)
  }
  cli::cli_abort(
    c("{what}", "x" = "{why}", "i" = "{hint}"),
    class = "mixtime_error_free_cyclical",
    call = NULL
  )
}

# Suggest anchoring `x` with time_compose(), using an example anchor whose
# chronon is the free value's cycle.
free_anchor_hint <- function(x, example = "%s") {
  cycle <- free_unit(x@cycle)
  anchor <- free_anchor_example(x@cycle)
  if (is.null(anchor)) {
    return(cli::format_inline(
      "Locate it in a specific {cycle} first with {.fn time_compose}, using a time point measured in {cycle}s."
    ))
  }
  code <- sprintf(example, sprintf("time_compose(%s, x)", anchor))
  cli::format_inline("Locate it in a specific {cycle} first, e.g. {.code {code}}.")
}

# An example time point measured in `cycle`, as code. Each anchor's chronon is
# the cycle, as time_compose() requires.
free_anchor_example <- function(cycle) {
  if (cycle@n != 1L) {
    return(NULL)
  }
  switch(
    S7_class_id(cycle),
    "mixtime::tu_year" = "year(2024)",
    "mixtime::tu_quarter" = 'yearquarter("2024 Q1")',
    "mixtime::tu_month" = 'yearmonth("2024-02")',
    "mixtime::tu_week" = 'yearweek("2024 W03")',
    "mixtime::tu_day" = 'date("2024-02-01")',
    NULL
  )
}

# Singular name of a granule's unit (e.g. "month", or "2 months").
free_unit <- function(granule) {
  if (granule@n == 1L) time_unit_plural(granule, 1L) else time_granule_label(granule)
}

# Plural name of a granule's unit (e.g. "months").
free_units <- function(granule) {
  if (granule@n == 1L) time_unit_plural(granule, 2L) else paste("periods of", time_granule_label(granule))
}

# A free value's kind, e.g. "day of month".
free_kind <- function(x) {
  paste(free_unit(x@chronon), "of", free_unit(x@cycle))
}

# "a day of month with no month"
free_noun <- function(x) {
  kind <- free_kind(x)
  paste(article(kind), kind, "with no", free_unit(x@cycle))
}

article <- function(word) if (grepl("^([aeiou]|hour)", word)) "an" else "a"

# Describe the other operand of a failed operation.
operand_noun <- function(x) {
  if (S7_inherits(x, mt_cyclical_free)) {
    return(free_noun(x))
  }
  if (S7_inherits(x, mt_cyclical)) {
    kind <- paste(free_unit(x@chronon), "of", free_unit(x@cycle))
    return(paste("a located", kind))
  }
  if (S7_inherits(x, mt_duration)) {
    return("a duration")
  }
  if (S7_inherits(x, mt_time)) {
    return(paste("a", free_unit(x@chronon), "time point"))
  }
  if (is.numeric(x)) {
    return("a number")
  }
  paste(article(class(x)[[1L]]), paste0("<", class(x)[[1L]], ">"))
}

# The time between a free value and a time point at a known place in time.
abort_free_anchored_difference <- function(e1, e2) {
  free <- if (S7_inherits(e1, mt_cyclical_free)) e1 else e2
  other <- if (S7_inherits(e1, mt_cyclical_free)) e2 else e1
  abort_free_cyclical_op(
    free,
    what = paste("Can't subtract", operand_noun(e2), "from", operand_noun(e1)),
    why = paste0(
      "Which ", free_unit(free@cycle), " the ", free_kind(free),
      " falls in is unknown, so the time between it and ",
      operand_noun(other), " is unknown."
    ),
    example = "%s - y"
  )
}

# --- shifts -------------------------------------------------------------------

# The plain stored values of a time vector.
free_data <- function(x) {
  as.vector(unclass(S7_data(x)))
}

# Restore shifted values, keeping integer storage for whole numbers.
free_restore <- function(x, value) {
  if (is.integer(free_data(x)) && all(value == round(value), na.rm = TRUE)) {
    value <- as.integer(round(value))
  }
  S7_data(x) <- structure(value, class = "mt_time_data")
  x
}

# The number of chronons in a free value's cycle, or NULL if it varies.
free_cycle_length <- function(x) {
  if (!chronon_cardinality_is_fixed(x@chronon, x@cycle)) {
    return(NULL)
  }
  chronon_cardinality(x@chronon, x@cycle)
}

# Describe a shift of `x` for errors, e.g. "Can't add 1 day to ...".
free_shift_what <- function(x, label, sign) {
  if (sign > 0) {
    paste("Can't add", label, "to", free_noun(x))
  } else {
    paste("Can't subtract", label, "from", free_noun(x))
  }
}

# Example code that shifts anchored `x` as the user tried, e.g. "%s + days(1)".
# Anchored time has the free value's chronon, so `by` defaults to it.
free_shift_example <- function(by, n, sign) {
  op <- if (sign > 0) "+" else "-"
  fn <- time_unit_plural(by, 2L)
  if (by@n == 1L && is.function(get0(fn, envir = asNamespace("mixtime"), inherits = FALSE))) {
    return(sprintf("%%s %s %s(%s)", op, fn, n))
  }
  paste("%s", op, "by")
}

# Convert a shift to a count of `x`'s chronons, checking it is determinate.
# `n` counts `by` granules (or `x`'s chronons if `by` is NULL), `sign` is +1
# for addition and -1 for subtraction. The checks depend only on the units
# (and on whether `n` is whole), never on the positions being shifted.
free_shift_count <- function(x, n, by = NULL, sign = 1L, what = NULL, example = NULL) {
  n <- as.vector(unclass(n))
  shown <- n[!is.na(n)]
  shown <- if (length(shown)) abs(shown[[1L]]) else 1L
  label <- time_granule_label(by %||% x@chronon, shown)
  what <- what %||% free_shift_what(x, label, sign)
  example <- example %||% free_shift_example(by %||% x@chronon, shown, sign)
  chronons <- free_units(x@chronon)
  cycle <- free_unit(x@cycle)
  len <- free_cycle_length(x)

  if (!is.null(by)) {
    if (!chronon_cardinality_is_fixed(x@chronon, by)) {
      # Name the varying relationship when one unit is finer than the other.
      same_unit <- function(a, b) S7_class_id(a) == S7_class_id(b) && a@n == b@n
      fine <- chronon_glb_impl(list(x@chronon, by))
      coarse <- if (same_unit(fine, by)) x@chronon else by
      why <- if (same_unit(fine, x@chronon) || same_unit(fine, by)) {
        paste0(
          "The number of ", free_units(fine), " in ", article(free_unit(coarse)),
          " ", free_unit(coarse), " varies, so ", label,
          " isn't a fixed number of ", chronons, "."
        )
      } else {
        paste0(label, " isn't a fixed number of ", chronons, ".")
      }
      abort_free_cyclical_op(
        x,
        what = paste0(what, ", the result is temporally indeterminate."),
        why = why,
        hint = cli::format_inline(
          "Shift by a whole number of {chronons} instead, or locate it in a specific {cycle} with {.fn time_compose} first."
        )
      )
    }
    per <- chronon_cardinality(x@chronon, by)
    if (!is.null(len) && per >= len) {
      abort_free_cyclical_op(
        x,
        what = paste0(what, ", the result is temporally indeterminate."),
        why = paste0(
          label, " spans a whole ", cycle, " or more, which moves it into ",
          "another ", cycle, ", but which ", cycle, " it is in is unknown."
        ),
        example = example
      )
    }
    n <- n * per
    whole <- abs(n - round(n)) < sqrt(.Machine$double.eps)
    if (!all(whole, na.rm = TRUE)) {
      abort_free_cyclical_op(
        x,
        what = paste0(what, ", the result is temporally indeterminate."),
        why = paste0(label, " isn't a whole number of ", chronons, "."),
        hint = cli::format_inline(
          "Shift by a whole number of {chronons}, or by a duration that is one."
        )
      )
    }
    n <- round(n)
  }

  if (is.null(len)) {
    kind <- free_kind(x)
    abort_free_cyclical_op(
      x,
      what = paste0(what, "."),
      why = paste0(
        "The number of ", chronons, " in ", article(cycle), " ", cycle,
        " varies, so where ", article(kind), " ", kind, " lands after ",
        if (sign > 0) "adding " else "subtracting ", label,
        " depends on which ", cycle, " it is in."
      ),
      example = example
    )
  }
  sign * n
}

# Shift free values by `n` chronons, wrapping around the (fixed length) cycle.
free_shift <- function(x, n) {
  len <- free_cycle_length(x)
  free_restore(x, (free_data(x) + n) %% len)
}

method(`+`, list(mt_cyclical_free, S7::class_numeric)) <- function(e1, e2) {
  free_shift(e1, free_shift_count(e1, e2, sign = 1L))
}
method(`-`, list(mt_cyclical_free, S7::class_numeric)) <- function(e1, e2) {
  free_shift(e1, free_shift_count(e1, e2, sign = -1L))
}
method(`+`, list(S7::class_numeric, mt_cyclical_free)) <- function(e1, e2) {
  free_shift(e2, free_shift_count(e2, e1, sign = 1L))
}
method(`+`, list(mt_cyclical_free, mt_duration)) <- function(e1, e2) {
  free_shift(e1, free_shift_count(e1, S7_data(e2), e2@chronon, sign = 1L))
}
method(`-`, list(mt_cyclical_free, mt_duration)) <- function(e1, e2) {
  free_shift(e1, free_shift_count(e1, S7_data(e2), e2@chronon, sign = -1L))
}
method(`+`, list(mt_duration, mt_cyclical_free)) <- function(e1, e2) {
  free_shift(e2, free_shift_count(e2, S7_data(e1), e1@chronon, sign = 1L))
}
# Unary plus keeps the position; negating a position has no meaning.
method(`+`, list(mt_cyclical_free, S7::class_missing)) <- function(e1, e2) e1
method(`-`, list(mt_cyclical_free, S7::class_missing)) <- function(e1, e2) {
  abort_free_cyclical_op(
    e1,
    what = paste("Can't negate", free_noun(e1)),
    why = paste0(
      "A ", free_kind(e1), " is a position within ", article(free_unit(e1@cycle)),
      " ", free_unit(e1@cycle), ", not an amount of time."
    ),
    hint = cli::format_inline("Subtract it from another position to get a duration, e.g. {.code y - x}.")
  )
}

# --- differences --------------------------------------------------------------

method(`-`, list(mt_cyclical_free, mt_cyclical_free)) <- function(e1, e2) {
  if (is.null(cycle_common(e1@cycle, e2@cycle))) {
    abort_free_cyclical_op(
      e1,
      what = paste("Can't subtract", operand_noun(e2), "from", operand_noun(e1)),
      why = paste0(
        "They are positions in different cycles (",
        free_unit(e1@cycle), " and ", free_unit(e2@cycle),
        "), so the time between them is unknown."
      ),
      hint = cli::format_inline(
        "Subtract values of the same cycle, or locate both with {.fn time_compose} first."
      )
    )
  }
  tu <- chronon_glb_impl(list(e1@chronon, e2@chronon))
  xd <- free_data(e1) * chronon_cardinality(tu, e1@chronon)
  yd <- free_data(e2) * chronon_cardinality(tu, e2@chronon)
  res <- xd - yd
  if (is.integer(free_data(e1)) && is.integer(free_data(e2)) &&
      all(res == round(res), na.rm = TRUE)) {
    res <- as.integer(round(res))
  }
  mt_duration(res, chronon = tu)
}

# A free value and a linear or anchored time point (either order)
method(`-`, list(mt_cyclical_free, mt_time)) <- function(e1, e2) {
  abort_free_anchored_difference(e1, e2)
}
method(`-`, list(mt_time, mt_cyclical_free)) <- function(e1, e2) {
  abort_free_anchored_difference(e1, e2)
}
method(`-`, list(mt_duration, mt_cyclical_free)) <- function(e1, e2) {
  abort_free_cyclical_op(
    e2,
    what = paste("Can't subtract", operand_noun(e2), "from a duration"),
    why = "A duration is an amount of time, which has no position in a cycle.",
    hint = cli::format_inline("Subtract the duration from it instead, e.g. {.code x - by}.")
  )
}
method(`-`, list(S7::class_numeric, mt_cyclical_free)) <- function(e1, e2) {
  abort_free_cyclical_op(
    e2,
    what = paste("Can't subtract", operand_noun(e2), "from a number"),
    why = "A number counts chronons, which have no position in a cycle.",
    hint = cli::format_inline("Subtract the number from it instead, e.g. {.code x - n}.")
  )
}

# --- unsupported operations ---------------------------------------------------

abort_free_operand <- function(op, e1, e2) {
  x <- if (S7_inherits(e1, mt_cyclical_free)) e1 else e2
  verb <- switch(op, "+" = "add", "-" = "subtract", "*" = "multiply", "/" = "divide")
  what <- switch(
    op,
    "+" = paste("Can't add", operand_noun(e2), "to", operand_noun(e1)),
    "-" = paste("Can't subtract", operand_noun(e2), "from", operand_noun(e1)),
    paste("Can't", verb, operand_noun(e1), "by", operand_noun(e2))
  )
  why <- if (op %in% c("*", "/")) {
    paste0(
      toupper(substr(article(free_kind(x)), 1, 1)),
      substring(article(free_kind(x)), 2), " ", free_kind(x),
      " is a position within ", article(free_unit(x@cycle)), " ",
      free_unit(x@cycle), ", not an amount of time."
    )
  } else {
    paste0(
      "Only numbers and durations can be added to or subtracted from ",
      article(free_kind(x)), " ", free_kind(x), ", and which ",
      free_unit(x@cycle), " it falls in is unknown."
    )
  }
  abort_free_cyclical_op(x, what = what, why = why)
}

method(`+`, list(mt_cyclical_free, S7::class_any)) <- function(e1, e2) abort_free_operand("+", e1, e2)
method(`+`, list(S7::class_any, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("+", e1, e2)
method(`+`, list(mt_time, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("+", e1, e2)
method(`-`, list(mt_cyclical_free, S7::class_any)) <- function(e1, e2) abort_free_operand("-", e1, e2)
method(`-`, list(S7::class_any, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("-", e1, e2)
method(`*`, list(mt_cyclical_free, S7::class_any)) <- function(e1, e2) abort_free_operand("*", e1, e2)
method(`*`, list(S7::class_any, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("*", e1, e2)
method(`*`, list(mt_time, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("*", e1, e2)
method(`/`, list(mt_cyclical_free, S7::class_any)) <- function(e1, e2) abort_free_operand("/", e1, e2)
method(`/`, list(S7::class_any, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("/", e1, e2)
method(`/`, list(mt_time, mt_cyclical_free)) <- function(e1, e2) abort_free_operand("/", e1, e2)

# --- summaries and math -------------------------------------------------------

abort_free_summary <- function(x, fn) {
  why <- if (fn == "mean") {
    paste0(
      "Positions in a cycle wrap around, so which ", free_unit(x@cycle),
      " the mean falls in is unknown (the mean of December and January ",
      "could be in the middle of the ", free_unit(x@cycle), " or at its turn)."
    )
  } else {
    paste0(
      "Positions within ", article(free_unit(x@cycle)), " ", free_unit(x@cycle),
      " aren't amounts of time, and which ", free_unit(x@cycle),
      " they fall in is unknown."
    )
  }
  abort_free_cyclical_op(
    x,
    what = paste0("Can't compute `", fn, "()` of ", free_noun(x), "."),
    why = why,
    example = paste0(fn, "(%s)")
  )
}

#' @export
S7::method(mean, mt_cyclical_free) <- function(x, ...) {
  abort_free_summary(x, "mean")
}

#' @method Math mixtime::mt_cyclical_free
#' @export
`Math.mixtime::mt_cyclical_free` <- function(x, ...) {
  # Rounding keeps positions within the cycle; other maths creates new values.
  if (.Generic %in% c("round", "signif", "floor", "ceiling", "trunc")) {
    return(NextMethod())
  }
  abort_free_summary(x, .Generic)
}

# --- sequences ----------------------------------------------------------------

#' @export
S7::method(seq, mt_cyclical_free) <- function(
  from,
  to,
  by,
  length.out = NULL,
  along.with = NULL,
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
  arg <- list(length.out = length.out)

  if (!missing_from && !missing_to) {
    if (!S7_inherits(to, mt_cyclical_free) || !S7_inherits(from, mt_cyclical_free) ||
        is.null(cycle_common(from@cycle, to@cycle)) ||
        !granule_identical(from@chronon, to@chronon)) {
      abort_free_cyclical_op(
        ptype,
        what = paste("Can't create a sequence from", operand_noun(from), "to", operand_noun(to)),
        why = "Sequences need both ends to be positions of the same chronon in the same cycle.",
        hint = cli::format_inline(
          "Convert {.arg from} and {.arg to} to the same kind of cyclical time."
        )
      )
    }
  }
  if (!missing_from) arg$from <- free_data(from)
  if (!missing_to) arg$to <- free_data(to)

  # Steps (in chronons) follow the rules of `x + by`, so a sequence of a
  # cycle with variable length is an error even without `by`.
  what <- paste("Can't create a sequence of", free_noun(ptype))
  example <- "seq(%s, ...)"
  if (missing_by) {
    free_shift_count(ptype, 1L, what = what, example = example)
  } else {
    if (length(by) != 1L) {
      cli::cli_abort("{.var by} must be a single time duration", call. = FALSE)
    }
    if (is_mixtime(by)) by <- vecvec::unvecvec(by)
    if (is.character(by)) by <- parse_time_unit(by)
    if (S7_inherits(by, mt_unit)) by <- vecvec::unvecvec(duration(1L, by))
    arg$by <- if (S7_inherits(by, mt_duration)) {
      free_shift_count(ptype, S7_data(by), by@chronon, what = what, example = example)
    } else {
      free_shift_count(ptype, by, what = what, example = example)
    }
  }

  res <- rlang::exec(seq.int, !!!arg)
  free_restore(ptype, res %% free_cycle_length(ptype))
}
