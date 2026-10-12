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
  # A bare `mt_duration`, not a `mixtime`: the `mixtime` method combines these
  # granule by granule, and wrapping each one would nest a mixtime inside it.
  chronon <- attr(x, "chronon")
  mt_duration(tz_offset_impl(as.numeric(x), chronon, tz), chronon = chronon)
}

tz_offset_impl <- function(x, chronon, tz = tz_name(chronon)) {
  # Naive and UTC time has no time zone offsets
  if (is.na(tz) || tz == "UTC") {
    return(rep(0, length(x)))
  }

  tu_s <- tu_utc_second()
  time_s <- chronon_to_utc(chronon, x)
  offset_s <- tz_offset_at(time_s, tz)
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

# --- time zone context ----------------------------------------------------------
#
# A zoned chronon divides absolute time into buckets: the spans between
# consecutive local boundaries of the chronon (e.g. local midnights for a day).
# A stored value is the bucket's index plus the elapsed fraction of that bucket,
# so a day that is 23 or 25 hours long still runs from 0 to 1.
#
# The index is the bucket's local label less the whole-unit part of the UTC
# offset. For a day (or longer) the offset is under one unit, so the index is
# the local label. For an hour in a whole-hour zone it is the UTC hour, which
# keeps the two local 02:00 hours at the end of daylight saving distinct.
#
# A naive or UTC zone has no offsets, so its values are local and use the
# defaults (R/chronon_context.R).

tz_has_offset <- function(chronon) {
  tz <- chronon@tz
  !is.na(tz) && tz != "UTC"
}

# UTC offset (seconds) in effect at UTC instants `s`.
tz_offset_at <- function(s, tz) {
  s <- as.double(s)
  s[!is.finite(s)] <- NA_real_
  as.double(get_tz_offset(s, tz))
}

# Offsets either side of any transition near UTC instants `s`.
tz_offsets_around <- function(s, tz) {
  offset <- tz_offset_at(c(s - 172800, s + 172800), tz)
  list(before = offset[seq_along(s)], after = offset[-seq_along(s)])
}

# Apply `f` to the unique values of `x` only. Many values usually share a
# bucket, and calendar conversions (e.g. of months) are comparatively slow.
by_unique <- function(x, f) {
  u <- unique(x)
  f(u)[match(x, u)]
}

# Local start (seconds) of labels of `chronon`, as a lookup function. Each
# distinct label is converted once however often it is looked up, so one
# operation can ask for a label's start, its end (the next label's start) and
# its neighbours' without converting them again.
tz_label_starts <- function(chronon) {
  tu_s <- tu_utc_second()
  label <- double()
  start <- double()
  function(x) {
    pos <- match(x, label)
    missing <- is.na(pos)
    if (any(missing)) {
      new <- unique(x[missing])
      pos[missing] <- length(label) + match(x[missing], new)
      label <<- c(label, new)
      start <<- c(start, chronon_convert_graph(new, chronon, tu_s))
    }
    start[pos]
  }
}

# Nominal length (seconds) of local labels `label`, from their local `starts`.
tz_label_length <- function(starts, label) {
  starts(label + 1) - starts(label)
}

# UTC instant at which bucket `i` of `chronon` starts.
tz_bucket_start <- function(chronon, i, starts = tz_label_starts(chronon)) {
  by_unique(i, function(i) tz_bucket_start_impl(chronon, i, starts))
}

tz_bucket_start_impl <- function(chronon, i, starts) {
  tz <- chronon@tz
  # The local start of label `i`, and the unit's nominal length (seconds)
  local <- starts(i)
  unit <- tz_label_length(starts, i)
  around <- tz_offsets_around(local, tz)

  # UTC start of elements `k`, numbered with UTC offsets `offset`
  start_with <- function(offset, k) {
    shift <- trunc(offset / unit[k])
    s <- local[k]
    shifted <- (shift != 0) %in% TRUE
    s[shifted] <- starts(i[k][shifted] + shift[shifted])
    s - offset
  }

  # Away from a transition the offset is known. A local boundary that falls in
  # a gap also keeps this start (numbered with the offset before the gap),
  # which is the transition itself.
  start <- start_with(around$before, seq_along(i))

  # Near a transition, try the offset either side of it and keep the earliest
  # start that is consistent with its offset.
  near <- which((around$before != around$after) %in% TRUE)
  if (length(near)) {
    # Both candidates in one batch: those with the offset before, then after.
    offset <- c(around$before[near], around$after[near])
    candidate <- start_with(offset, c(near, near))
    candidate[!((tz_offset_at(candidate, tz) == offset) %in% TRUE)] <- NA
    before <- seq_along(near)
    found <- pmin(candidate[before], candidate[-before], na.rm = TRUE)
    start[near] <- ifelse(is.na(found), start[near], found)
  }
  start
}

# Local label of bucket `i` of `chronon`.
tz_bucket_label <- function(chronon, i, starts = tz_label_starts(chronon)) {
  by_unique(i, function(i) {
    local <- starts(i)
    unit <- tz_label_length(starts, i)
    around <- tz_offsets_around(local, chronon@tz)

    # Away from a transition the label follows from the offset
    label <- i + trunc(around$before / unit)

    near <- (around$before != around$after) %in% TRUE
    if (any(near)) {
      s <- tz_bucket_start_impl(chronon, i[near], starts)
      label[near] <- chronon_convert_graph(
        s + tz_offset_at(s, chronon@tz),
        tu_utc_second(),
        chronon,
        discrete = TRUE
      )
    }
    label
  })
}

method(chronon_is_local, mt_tz_unit) <- function(chronon) {
  !tz_has_offset(chronon)
}

# Units shorter than a day (e.g. hours) shift by exact real time, so an hour
# across a daylight saving transition is one real hour. Longer units keep the
# local clock time. Without UTC offsets the two agree.
method(chronon_shift_is_exact, mt_tz_unit) <- function(chronon, unit) {
  tz_has_offset(chronon) &&
    (chronon_fixed_seconds(unit) / abs(unit@n) < 86400) %in% TRUE
}

method(chronon_to_utc, mt_tz_unit) <- function(chronon, x) {
  if (!tz_has_offset(chronon)) {
    return(chronon_convert_graph(x, chronon, tu_utc_second()))
  }
  i <- floor_ulp(x)
  f <- x - i
  starts <- tz_label_starts(chronon)
  s <- tz_bucket_start(chronon, i, starts)
  frac <- (f != 0) %in% TRUE
  if (any(frac)) {
    end <- tz_bucket_start(chronon, i[frac] + 1, starts)
    s[frac] <- s[frac] + f[frac] * (end - s[frac])
  }
  s
}

method(chronon_from_utc, mt_tz_unit) <- function(chronon, x, discrete = FALSE) {
  tu_s <- tu_utc_second()
  if (!tz_has_offset(chronon)) {
    return(chronon_convert_graph(x, tu_s, chronon, discrete))
  }
  offset <- tz_offset_at(x, chronon@tz)
  label <- chronon_convert_graph(x + offset, tu_s, chronon, discrete = TRUE)
  starts <- tz_label_starts(chronon)
  i <- label - trunc(offset / tz_label_length(starts, label))
  if (discrete) {
    return(i)
  }
  bounds <- tz_bucket_start(chronon, c(i, i + 1), starts)
  start <- bounds[seq_along(i)]
  end <- bounds[-seq_along(i)]
  i + (x - start) / (end - start)
}

method(chronon_to_local, mt_tz_unit) <- function(chronon, x) {
  if (!tz_has_offset(chronon)) {
    return(as.double(x))
  }
  i <- floor_ulp(x)
  tz_bucket_label(chronon, i) + (x - i)
}

method(chronon_from_local, mt_tz_unit) <- function(
  chronon,
  x,
  discrete = FALSE,
  on_invalid = "clamp"
) {
  if (!tz_has_offset(chronon)) {
    return(list(
      value = if (discrete) floor_ulp(x) else as.double(x),
      invalid = rep_len(FALSE, length(x))
    ))
  }
  label <- floor_ulp(x)
  starts <- tz_label_starts(chronon)
  local <- starts(label)
  unit <- tz_label_length(starts, label)
  around <- tz_offsets_around(local, tz = chronon@tz)

  # Away from a transition the bucket follows from the label and its offset.
  i <- label - trunc(around$before / unit)

  # Near a transition, try the offset either side of it and keep the earliest
  # bucket whose label matches. A label that matches neither falls in a gap.
  near <- (around$before != around$after) %in% TRUE
  gap <- rep_len(FALSE, length(x))
  if (any(near)) {
    # Both candidates in one batch: those with the offset before, then after.
    offset <- c(around$before[near], around$after[near])
    candidate <- rep(label[near], 2L) - trunc(offset / rep(unit[near], 2L))
    matched <- tz_bucket_label(chronon, candidate, starts) == rep(label[near], 2L)
    candidate[!(matched %in% TRUE)] <- NA
    before <- seq_len(sum(near))
    found <- pmin(candidate[before], candidate[-before], na.rm = TRUE)
    i[near] <- found
    gap[near] <- is.na(found) & !is.na(label[near])
  }

  value <- if (discrete) i else i + (x - label)
  if (any(gap)) {
    value[gap] <- tz_resolve_gap(
      chronon,
      x[gap],
      around$before[gap],
      around$after[gap],
      discrete,
      on_invalid
    )
  }
  list(value = value, invalid = gap)
}

# Stored values for local readings `x` of `chronon` that fall in a gap, where
# the UTC offset changes from `before` to `after` (see chronon_from_local()).
tz_resolve_gap <- function(chronon, x, before, after, discrete, on_invalid) {
  if (on_invalid %in% c("reject", "error")) {
    return(rep(NA_real_, length(x)))
  }
  local <- chronon_convert_graph(x, chronon, tu_utc_second())
  if (!on_invalid %in% c("previous", "next")) {
    # The local reading read with the offset after the gap moves it back by the
    # size of the gap, and with the offset before moves it forward.
    s <- local - if (on_invalid == "clamp") after else before
    return(chronon_from_utc(chronon, s, discrete))
  }
  # The transition lies between those two readings
  s <- mapply(
    function(from, to) {
      tr <- get_tz_transitions(from, to + 1, chronon@tz)
      tr$time[tr$offset_before != tr$offset_after][1L]
    },
    local - after,
    local - before
  )
  if (discrete) {
    chronon_from_utc(chronon, s, discrete = TRUE) - (on_invalid == "previous")
  } else {
    chronon_from_utc(chronon, s)
  }
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
    # Evaluated at the instant each time starts (e.g. local midnight for a
    # day), or at the time itself for a continuous time model.
    start <- as.double(chronon_convert(x[tz_given], tu_utc_second()))
    tz_abbr[tz_given] <- get_tz_abbreviation(start, tz[tz_given])
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
