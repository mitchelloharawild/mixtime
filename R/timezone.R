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
  list(
    before = tz_offset_at(s - 172800, tz),
    after = tz_offset_at(s + 172800, tz)
  )
}

# Apply `f` to the unique values of `x` only. Many values usually share a
# bucket, and calendar conversions (e.g. of months) are comparatively slow.
by_unique <- function(x, f) {
  u <- unique(x)
  f(u)[match(x, u)]
}

# UTC instant at which bucket `i` of `chronon` starts.
tz_bucket_start <- function(chronon, i) {
  by_unique(i, function(i) tz_bucket_start_impl(chronon, i))
}

tz_bucket_start_impl <- function(chronon, i) {
  tz <- chronon@tz
  tu_s <- tu_utc_second()
  # The local start of label `i`, and the unit's nominal length (seconds)
  local <- chronon_convert_graph(i, chronon, tu_s)
  unit <- chronon_convert_graph(i + 1, chronon, tu_s) - local
  around <- tz_offsets_around(local, tz)

  # UTC start of elements `k`, numbered with UTC offsets `offset`
  start_with <- function(offset, k) {
    shift <- trunc(offset / unit[k])
    s <- local[k]
    shifted <- (shift != 0) %in% TRUE
    s[shifted] <- chronon_convert_graph(i[k][shifted] + shift[shifted], chronon, tu_s)
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
    candidate <- function(offset) {
      s <- start_with(offset[near], near)
      s[!((tz_offset_at(s, tz) == offset[near]) %in% TRUE)] <- NA
      s
    }
    found <- pmin(candidate(around$before), candidate(around$after), na.rm = TRUE)
    start[near] <- ifelse(is.na(found), start[near], found)
  }
  start
}

# Local label of bucket `i` of `chronon`.
tz_bucket_label <- function(chronon, i) {
  tu_s <- tu_utc_second()
  by_unique(i, function(i) {
    local <- chronon_convert_graph(i, chronon, tu_s)
    unit <- chronon_convert_graph(i + 1, chronon, tu_s) - local
    around <- tz_offsets_around(local, chronon@tz)

    # Away from a transition the label follows from the offset
    label <- i + trunc(around$before / unit)

    near <- (around$before != around$after) %in% TRUE
    if (any(near)) {
      s <- tz_bucket_start_impl(chronon, i[near])
      label[near] <- chronon_convert_graph(
        s + tz_offset_at(s, chronon@tz),
        tu_s,
        chronon,
        discrete = TRUE
      )
    }
    label
  })
}

# Nominal length (seconds) of local label `label` of `chronon`.
tz_label_length <- function(chronon, label) {
  tu_s <- tu_utc_second()
  by_unique(label, function(label) {
    chronon_convert_graph(label + 1, chronon, tu_s) -
      chronon_convert_graph(label, chronon, tu_s)
  })
}

method(chronon_is_local, mt_tz_unit) <- function(chronon) {
  !tz_has_offset(chronon)
}

method(chronon_to_utc, mt_tz_unit) <- function(chronon, x) {
  if (!tz_has_offset(chronon)) {
    return(chronon_convert_graph(x, chronon, tu_utc_second()))
  }
  i <- floor_ulp(x)
  f <- x - i
  s <- tz_bucket_start(chronon, i)
  frac <- (f != 0) %in% TRUE
  if (any(frac)) {
    end <- tz_bucket_start(chronon, i[frac] + 1)
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
  i <- label - trunc(offset / tz_label_length(chronon, label))
  if (discrete) {
    return(i)
  }
  bounds <- tz_bucket_start(chronon, c(i, i + 1))
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

method(chronon_from_local, mt_tz_unit) <- function(chronon, x, discrete = FALSE) {
  if (!tz_has_offset(chronon)) {
    return(if (discrete) floor_ulp(x) else as.double(x))
  }
  label <- floor_ulp(x)
  unit <- tz_label_length(chronon, label)
  local <- by_unique(label, function(label) {
    chronon_convert_graph(label, chronon, tu_utc_second())
  })
  around <- tz_offsets_around(local, tz = chronon@tz)

  # Away from a transition the bucket follows from the label and its offset.
  # A label that falls in a gap keeps this bucket (numbered with the offset
  # before the gap), which moves it forward by the size of the gap.
  i <- label - trunc(around$before / unit)

  # Near a transition, try the offset either side of it and keep the earliest
  # bucket whose label matches.
  near <- (around$before != around$after) %in% TRUE
  if (any(near)) {
    candidate <- function(offset) {
      ic <- label[near] - trunc(offset[near] / unit[near])
      ic[(tz_bucket_label(chronon, ic) != label[near]) %in% TRUE] <- NA
      ic
    }
    found <- pmin(candidate(around$before), candidate(around$after), na.rm = TRUE)
    i[near] <- ifelse(is.na(found), i[near], found)
  }

  if (discrete) i else i + (x - label)
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
    # Evaluated at the chronon's context-blind start (`graph = TRUE`): a
    # sub-day chronon's index is UTC-based, so this is its true instant, while
    # a day (or coarser) index is a local label, giving one stable,
    # day-representative abbreviation.
    naive_utc <- as.double(chronon_convert(
      x[tz_given],
      tu_utc_second(),
      graph = TRUE
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
