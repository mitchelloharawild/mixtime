#' Extract timezone from an object
#'
#' Generic function to extract the timezone from objects that have timezone information.
#'
#' @param x An object with timezone information.
#' @param ... Additional arguments passed to methods.
#'
#' @return A character vector representing the timezone of each time point
#'   (e.g., "America/New_York", "UTC").
#'
#' @examples
#' tz_name(Sys.time())
#' tz_name(as.POSIXct("2024-06-15 12:00:00", tz = "America/New_York"))
#'
#' @export
tz_name <- S7::new_generic("tz_name", "x")
S7::method(tz_name, class_mixtime) <- function(x) {
  as.character(vecvec::vecvec_apply(x, tz_name))
}
S7::method(tz_name, mt_time) <- function(x) {
  rep_len(tz_name(attr(x, "chronon")), length(x))
}
S7::method(tz_name, mt_tz_unit) <- function(x) x@tz
S7::method(tz_name, S7::class_POSIXt) <- function(x) {
  rep_len(attr(x, "tzone") %||% NA_character_, length(x))
}
S7::method(tz_name, S7::class_any) <- function(x) {
  rep_len(NA_character_, length(x))
}

#' Get timezone offset
#'
#' Returns the UTC offset for a given datetime in its specified timezone.
#'
#' @param x A time class coercible to POSIXt with an associated time zone.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `mixtime` duration vector of offsets from UTC in the same chronon
#'   (e.g. seconds for POSIXt, days for dates, etc.)
#' @export
#'
#' @examples
#' tz_offset(as.POSIXct(Sys.time(), tz = Sys.timezone()))
#' tz_offset(as.POSIXct("2024-06-15 12:00:00", tz = "America/New_York"))
tz_offset <- S7::new_generic("tz_offset", "x")
S7::method(tz_offset, S7::class_POSIXt) <- function(
  x,
  tz = tz_name(time_chronon(x)),
  ...
) {
  duration(
    get_tz_offset(x, tz),
    chronon = cal_gregorian$second(1L, tz = tz[1L])
  )
}
S7::method(tz_offset, S7::class_Date) <- function(x, ...) {
  duration(rep.int(0, length(x)), chronon = cal_gregorian$day(1L))
}
method(tz_offset, class_mixtime) <- vecvec::vecvec_apply_fn(tz_offset)
method(tz_offset, mt_time) <- function(
  x,
  tz = tz_name(attr(x, "chronon")),
  ...
) {
  chronon <- attr(x, "chronon")
  duration(tz_offset_impl(as.numeric(x), chronon, tz), chronon = chronon)
}

tz_offset_impl <- function(x, chronon, tz = tz_name(chronon)) {
  offset_s <- rep(0L, length(x))

  # Naive and UTC time has no time zone offsets
  if (is.na(tz) || tz == "UTC") {
    return(offset_s)
  }

  tu_s <- cal_time_civil$second(1L, tz = "UTC")
  time_s <- chronon_convert_impl(x, chronon, tu_s, FALSE, "UTC")
  offset_s <- get_tz_offset(as.double(time_s), tz)
  nz_offset <- !is.na(offset_s) & offset_s != 0
  if (!any(nz_offset)) {
    return(rep(0, length(x)))
  }
  offset_s[nz_offset] <- offset_s[nz_offset] *
    chronon_cardinality(
      chronon,
      tu_s,
      time_s[nz_offset]
    )
  offset_s
}

# Shift a `chronon`-native numeric value from UTC into the local wall-clock
# domain of `tz`, expressed as if it were UTC. `chronon_divmod()`,
# `chronon_decompose()`/`chronon_recompose()`, and `chronon_convert_impl()`
# (when called with `tz = "UTC"`) are all tz-blind and work in this shifted
# domain - see `time_round_impl()` and `seq.mixtime::mt_time()`.
tz_to_local <- function(x, chronon, tz) {
  x + tz_offset_impl(x, chronon, tz = tz)
}

# Undo `tz_to_local()`, recovering the true (UTC) `chronon` value. A second
# pass re-evaluates the offset at the candidate raw instant rather than
# reusing the forward offset, since a value that falls in a DST gap/overlap
# can have a different offset there than at its own local-shifted value.
#
# TODO(perf): this approximates gap/overlap resolution with two
# `tz_offset_impl()` round trips (each its own `chronon_convert_impl()` +
# `get_tz_offset()` C call). The vendored `date`/`tzdb` library already has a
# purpose-built, single-lookup primitive for exactly this -
# `date::time_zone::to_sys(local_time, choose::earliest|latest)` (see
# tzdb's `include/date/tz.h`) - which is what `as.POSIXct()` itself relies on.
# Wiring up a new cpp11-registered resolver around that would both halve the
# lookup cost and replace this two-sample approximation with the library's
# real resolution logic, at the cost of touching `src/` (new registered
# function, regenerated `src/cpp11.cpp`/`R/cpp11.R` bindings, recompile).
# Left as pure R for now since the 2x cost is negligible at realistic
# `seq()`/`time_round()` sizes.
tz_to_utc <- function(x, chronon, tz, discrete = FALSE, boundary = TRUE) {
  tzo <- tz_offset_impl(
    x - tz_offset_impl(x, chronon, tz = tz),
    chronon,
    tz = tz
  )

  if (discrete) {
    if (boundary) {
      # A boundary conversion (`chronon_convert_impl()`'s multi-hop path) has
      # already resolved which `chronon` bucket the local-shifted value falls
      # into via a real divmod - the offset is spent, so only a leftover
      # *whole*-unit remainder (never seen with real-world offsets, but kept
      # for safety) still needs correcting; floor(x) - trunc(tzo) keeps that
      # correction separate from the local-bucket alignment so a fractional
      # offset can't borrow across a unit boundary and land a whole unit off.
      nudge <- 8 * .Machine$double.eps * pmax(abs(x), 1)
      as.integer(floor(x + nudge) - trunc(tzo))
    } else {
      # No divmod happened between the matching tz_to_local() shift and this
      # call (e.g. a same-chronon or pure-ratio conversion), so the offset is
      # still fully outstanding in `x` and must be subtracted whole, before
      # flooring - not floored away first and only partly corrected after.
      shifted <- x - tzo
      nudge <- 8 * .Machine$double.eps * pmax(abs(shifted), 1)
      as.integer(floor(shifted + nudge))
    }
  } else {
    x - tzo
  }
}

# Resolve the true (real-elapsed) start of the `unit_size`-second bucket that
# the true instant `sec` (raw UTC seconds) falls into, plus that bucket's
# nominal index. Used by chronon_convert_impl()'s contracting-direction
# tz-aware branch to give two instants that share a local wall-clock reading
# during a DST fall-back overlap (or a transition day's non-nominal span)
# distinct, real-elapsed positions - see
# _dev/tz_transitions_real_elapsed.md.
#
# `sec`'s own offset (`get_tz_offset()` at a true instant is never ambiguous)
# finds the unshifted local reading and, from it, a naive ("as if UTC")
# nominal boundary label. That label itself is then re-resolved to a true
# instant: convergence is checked over two steps (not one) before falling
# back to `offset_here`, since a first pass can land on a real moment far
# away with a very different offset in effect - one non-match doesn't yet
# mean the label is genuinely ambiguous, just that it hasn't converged yet.
tz_resolve_boundary <- function(sec, tz, unit_size) {
  offset_here <- get_tz_offset(sec, tz)
  local_reading <- sec + offset_here
  label_start <- floor(local_reading / unit_size) * unit_size

  off_b <- get_tz_offset(label_start - get_tz_offset(label_start, tz), tz)
  off_c <- get_tz_offset(label_start - off_b, tz)
  offset <- ifelse(off_b == off_c, off_c, offset_here)

  list(
    start = label_start - offset,
    index = label_start / unit_size - trunc(offset / unit_size)
  )
}

# Inverse of the contracting mapping `here$index + (sec - here$start) /
# (after$start - here$start)` (tz_resolve_boundary(), assembled in
# chronon_convert_impl()'s contracting branch): given a real-elapsed
# `bucket_index + fraction` value `v` in that same domain, recover the raw
# UTC seconds `sec` it came from. Used by the expanding-direction branch to
# round-trip values the contracting branch produced.
#
# Not a closed-form inversion: near a DST fall-back, the forward mapping
# anchors an ambiguous local label to one occurrence (see the "here"/"after"
# resolution in tz_resolve_boundary()) and lets *both* real instants sharing
# that label extend from it - positions on either side of the anchor's own
# bucket, not each with an independently resolvable one. So this instead
# treats the forward mapping as the oracle and fixed-point-iterates: guess a
# raw-seconds candidate, see which anchor ("here") the forward mapping would
# resolve it to, invert the (exactly linear, once the anchor is right)
# `index`/`start`/width relationship to get a better candidate, and repeat
# until the anchor stops changing. `v * unit_size` (nominal, ignoring tz)
# seeds the first guess; offsets don't accumulate over time, so this is
# always within one realistic single transition of converging, per
# tz_resolve_boundary()'s own "1.5x nominal size clears one transition"
# assumption - a few iterations comfortably cover it.
tz_resolve_boundary_inverse <- function(v, tz, unit_size, max_iter = 4L) {
  here <- tz_resolve_boundary(v * unit_size, tz, unit_size)
  sec <- NULL
  for (i in seq_len(max_iter)) {
    after <- tz_resolve_boundary(here$start + unit_size * 1.5, tz, unit_size)
    width <- after$start - here$start
    sec <- here$start + (v - here$index) * width

    next_here <- tz_resolve_boundary(sec, tz, unit_size)
    stable <- next_here$index == here$index
    if (all(stable)) {
      break
    }
    here$start[!stable] <- next_here$start[!stable]
    here$index[!stable] <- next_here$index[!stable]
  }
  sec
}

# TRUE if chronon_convert_impl()'s expanding-direction real-elapsed branch
# (`from` coarser than `to`, tz-aware, e.g. day -> second - see
# R/chronon_convert.R) applies to this (from, to) pair. When it does, the
# conversion already returns a fully-resolved absolute instant, so
# tz_wall_clock_to_utc() (R/arithmetic.R) - written for chronon_convert_impl()'s
# older, only-partially-cancelling behaviour on a cross-class tz-aware
# conversion - must not apply its own correction on top of it.
chronon_tz_expanding_resolved <- function(from, to) {
  if (!S7_inherits(from, mt_tz_unit) || !S7_inherits(to, mt_tz_unit)) {
    return(FALSE)
  }
  # `from`'s tz decides whether its values are local labels; a UTC `to` still
  # needs resolving (its values are true instants), a naive `to` does not.
  from_tz <- tz_name(from)
  if (is.na(from_tz) || from_tz == "UTC" || is.na(tz_name(to))) {
    return(FALSE)
  }
  if (!chronon_fixed_reachable(to, from) || chronon_cardinality(to, from) <= 1) {
    return(FALSE)
  }
  tu_s <- cal_time_civil$second(1L, tz = "UTC")
  chronon_cardinality(tu_s, from) <= 86400
}

#' Get timezone abbreviation
#'
#' Returns the timezone abbreviation (e.g., "EST", "PDT") for a given datetime
#' in its specified timezone.
#'
#' @param x A POSIXct datetime object or something coercible to POSIXct.
#'   The timezone is extracted from this object.
#' @param tz A character vector of timezones to abbreviate at time point `x`.
#'
#' @return A character vector of timezone abbreviations.
#'
#' @examples
#' tz_abbreviation(Sys.time())
#' tz_abbreviation(as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York"))
#'
#' @export
tz_abbreviation <- function(x, tz = tz_name(x)) {
  tz_abbr <- character(length(x))

  # If tz is empty, then the object has a naive local timezone
  tz_given <- !is.na(tz) & !is.na(x)

  # TODO: Handle timezone changes within chronon using [before]/[after]
  if (any(tz_given)) {
    # Evaluated at the chronon's naive (tz-blind) start (`tz = "UTC"`): a
    # sub-day chronon's index is UTC-based, so this is its true instant, while
    # a day (or coarser) index is a local label, giving one stable,
    # day-representative abbreviation.
    naive_utc <- as.double(chronon_convert(
      x[tz_given],
      cal_time_civil$second(1L, tz = "UTC"),
      tz = "UTC"
    ))
    tz_abbr[tz_given] <- get_tz_abbreviation(naive_utc, tz[tz_given])
  }

  tz_abbr
}

#' Get timezone transitions
#'
#' Returns all timezone transitions (e.g., daylight saving time changes) that
#' occur between two datetimes. The timezone is taken from the start datetime.
#'
#' @param start A POSIXct datetime object or something coercible to POSIXct,
#'   representing the start of the time range. The timezone is extracted from this object.
#' @param end A POSIXct datetime object or something coercible to POSIXct,
#'   representing the end of the time range.
#'
#' @return A data frame with columns:
#'   * `time`: A `mixtime` linear time point (continuous, UTC seconds) giving
#'     the instant of the transition.
#'   * `offset_before`, `offset_after`: `mixtime` durations (UTC seconds)
#'     giving the UTC offset immediately before and after the transition.
#'
#' @examples
#' # Get all DST transitions in 2024 for New York
#' tz_transitions(
#'   as.POSIXct("2024-01-01", tz = "America/New_York"),
#'   as.POSIXct("2024-12-31", tz = "America/New_York")
#' )
#'
#' @export
tz_transitions <- function(start, end) {
  start <- as.POSIXct(start)
  tz <- attr(start, "tzone") %||% Sys.timezone()
  end <- as.double(as.POSIXct(end))
  start <- as.double(start)

  transitions <- get_tz_transitions(start, end, tz)

  transitions$time <- linear_time(
    transitions$time,
    chronon = cal_gregorian$second(1L, tz = "UTC"),
    discrete = FALSE
  )
  transitions$offset_before <- duration(
    transitions$offset_before,
    chronon = cal_gregorian$second(1L, tz = "UTC")
  )
  transitions$offset_after <- duration(
    transitions$offset_after,
    chronon = cal_gregorian$second(1L, tz = "UTC")
  )

  transitions
}
