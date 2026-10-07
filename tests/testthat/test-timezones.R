test_that("get_tz_offset returns correct offset values", {
  # Test with a known timezone and time
  winter <- as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York")
  summer <- as.POSIXct("2024-07-15 12:00:00", tz = "America/New_York")

  winter_offset <- get_tz_offset(winter, "America/New_York")
  summer_offset <- get_tz_offset(summer, "America/New_York")

  # EST is UTC-5 (-18000 seconds), EDT is UTC-4 (-14400 seconds)
  expect_equal(winter_offset, -18000)
  expect_equal(summer_offset, -14400)

  # Test with UTC
  utc_time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(get_tz_offset(utc_time, "UTC"), 0)

  # Test with positive offset timezone
  tokyo_time <- as.POSIXct("2024-01-15 12:00:00", tz = "Asia/Tokyo")
  expect_equal(get_tz_offset(tokyo_time, "Asia/Tokyo"), 32400) # UTC+9
})

test_that("get_tz_offset works with vector input", {
  times <- as.POSIXct(
    c("2024-01-15 12:00:00", "2024-07-15 12:00:00"),
    tz = "America/New_York"
  )
  offsets <- get_tz_offset(times, "America/New_York")

  expect_length(offsets, 2)
  expect_equal(offsets[1], -18000) # Winter
  expect_equal(offsets[2], -14400) # Summer
})

test_that("get_tz_offset is exact after the end of the transition table", {
  utc <- function(x) as.numeric(as.POSIXct(x, tz = "UTC"))

  # Daylight saving in New York starts at 07:00 UTC on 12 March 2045
  start <- utc("2045-03-12 07:00:00")
  x <- c(start - 1, start, utc("2100-01-01"), utc("2100-07-01"))
  expected <- c(-18000, -14400, -18000, -14400)
  expect_identical(get_tz_offset(x, "America/New_York"), expected)

  # Lord Howe Island's daylight saving is 30 minutes
  x <- c(utc("2060-01-15"), utc("2060-07-15"))
  expect_identical(get_tz_offset(x, "Australia/Lord_Howe"), c(39600, 37800))

  # The order of the lookups doesn't change their results
  x <- seq(utc("1900-01-01"), utc("2400-01-01"), length.out = 20011)
  one_by_one <- vapply(x, get_tz_offset, double(1), zone = "Europe/Dublin")
  expect_identical(get_tz_offset(rev(x), "Europe/Dublin"), rev(one_by_one))
  shuffle <- sample(length(x))
  expect_identical(get_tz_offset(x[shuffle], "Europe/Dublin"), one_by_one[shuffle])
})

test_that("get_tz_abbreviation returns correct abbreviations", {
  winter <- as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York")
  summer <- as.POSIXct("2024-07-15 12:00:00", tz = "America/New_York")

  expect_equal(get_tz_abbreviation(winter, "America/New_York"), "EST")
  expect_equal(get_tz_abbreviation(summer, "America/New_York"), "EDT")

  # Test UTC
  utc_time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(get_tz_abbreviation(utc_time, "UTC"), "UTC")
})

test_that("get_tz_abbreviation works with vector input", {
  times <- as.POSIXct(
    c("2024-01-15 12:00:00", "2024-07-15 12:00:00"),
    tz = "America/New_York"
  )
  abbrevs <- get_tz_abbreviation(times, "America/New_York")

  expect_length(abbrevs, 2)
  expect_equal(abbrevs[1], "EST")
  expect_equal(abbrevs[2], "EDT")
})

test_that("get_tz_transitions returns DST transitions", {
  start <- as.POSIXct("2024-01-01", tz = "America/New_York")
  end <- as.POSIXct("2024-12-31", tz = "America/New_York")

  transitions <- get_tz_transitions(start, end, "America/New_York")

  # Should have 2 transitions in 2024 (spring forward, fall back)
  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 2)
  expect_named(transitions, c("time", "offset_before", "offset_after"))

  # Check that offsets change
  expect_equal(transitions$offset_before[1], -18000) # EST
  expect_equal(transitions$offset_after[1], -14400) # EDT
  expect_equal(transitions$offset_before[2], -14400) # EDT
  expect_equal(transitions$offset_after[2], -18000) # EST
})

test_that("get_tz_transitions works with Southern Hemisphere DST", {
  start <- as.POSIXct("2024-01-01", tz = "Australia/Melbourne")
  end <- as.POSIXct("2024-12-31", tz = "Australia/Melbourne")

  transitions <- get_tz_transitions(start, end, "Australia/Melbourne")

  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 2)

  # Southern hemisphere: transitions are reversed (fall in April, spring in October)
  expect_equal(transitions$offset_before[1], 39600) # AEDT (UTC+11)
  expect_equal(transitions$offset_after[1], 36000) # AEST (UTC+10)
})

test_that("get_tz_transitions returns empty for no transitions", {
  start <- as.POSIXct("2024-01-01", tz = "UTC")
  end <- as.POSIXct("2024-12-31", tz = "UTC")

  transitions <- get_tz_transitions(start, end, "UTC")

  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 0)
})

test_that("timezone functions handle invalid timezone names gracefully", {
  time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")

  # These should error with invalid timezone names
  expect_error(get_tz_offset(time, "Invalid/Timezone"))
  expect_error(get_tz_abbreviation(time, "Invalid/Timezone"))
  expect_error(get_tz_transitions(time, time, "Invalid/Timezone"))
})

test_that("converting zoned time to naive time strips the timezone", {
  zoned <- datetime("2024-04-07 02:00:00", tz = "Australia/Melbourne")
  naive <- datetime(zoned, tz = NA)

  expect_true(is.na(tz_name(naive)))
  # Wall-clock time is preserved
  expect_equal(format(naive), "2024-04-07 02:00:00")
})

test_that("converting zoned time to naive preserves wall-clock time across DST gap", {
  # Melbourne falls back at 03:00 AEDT → 02:00 AEST on 2024-04-07.
  # The ambiguous 02:xx hour appears twice; both wall-clock values are kept as-is.
  from <- datetime("2024-04-07 00:00:00", tz = "Australia/Melbourne")
  mel_dst <- seq(from, length.out = 10, by = hours(1L))
  naive <- datetime(mel_dst, tz = NA)

  expect_true(all(is.na(tz_name(naive))))
  expect_length(naive, 10)
  # The two 02:00 entries (one AEDT, one AEST) both map to 02:00 wall-clock
  expect_equal(sum(format(naive) == "2024-04-07 02:00:00"), 2L)
})

test_that("converting naive time to zoned time errors", {
  zoned <- datetime("2024-01-15 12:00:00", tz = "America/New_York")
  naive <- datetime(zoned, tz = NA)

  expect_error(
    datetime(naive, tz = "America/New_York"),
    regexp = "timezone-naive"
  )
})

test_that("converting naive time to zoned errors for any target timezone", {
  naive <- datetime(as.POSIXct("2024-07-15 09:00:00", tz = "UTC"), tz = NA)

  expect_error(datetime(naive, tz = "UTC"))
  expect_error(datetime(naive, tz = "Asia/Tokyo"))
  expect_error(datetime(naive, tz = "Europe/Berlin"))
})

test_that("get_tz_transitions handles time ranges with no transitions", {
  # Short time range with no transitions
  start <- as.POSIXct("2024-06-01", tz = "America/New_York")
  end <- as.POSIXct("2024-06-15", tz = "America/New_York")

  transitions <- get_tz_transitions(start, end, "America/New_York")

  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 0)
})

test_that("the common chronon keeps a timezone the inputs agree on", {
  # The common chronon is found as the greatest lower bound of the inputs, which
  # is built from the granule's class alone. Properties describing the time
  # rather than its granularity (such as the timezone) must be carried across,
  # otherwise combining two Melbourne times gives a timezone-naive result which
  # can no longer be converted back to either input.
  tz <- "Australia/Melbourne"
  secs <- datetime(as.POSIXct("2015-01-01 00:00:00", tz = tz))
  hrs <- linear_time(
    as.POSIXct("2015-01-01 00:00:00", tz = tz),
    cal_gregorian$hour(1L)
  )

  expect_equal(tz_name(chronon_glb(c(secs, secs))), tz)
  expect_equal(tz_name(chronon_glb(c(secs, hrs))), tz)

  # Combining no longer drops the timezone, whichever way round it is done.
  expect_equal(unique(tz_name(c(secs, hrs))), tz)
  expect_equal(unique(tz_name(c(hrs, secs))), tz)
})

test_that("the common chronon of disagreeing known timezones is UTC", {
  # A common chronon that claimed one of the input timezones would silently
  # move the other input's time points. But both zones are real, resolvable
  # zones, so the disagreement can be reconciled at UTC instead of discarding
  # the timezone entirely (which left the common chronon unformattable).
  melb <- datetime(as.POSIXct(
    "2015-01-01 00:00:00",
    tz = "Australia/Melbourne"
  ))
  utc <- datetime(as.POSIXct("2015-01-01 00:00:00", tz = "UTC"))
  naive <- datetime(melb, tz = NA)

  expect_equal(tz_name(chronon_glb(c(melb, utc))), "UTC")
  # Naive time cannot be represented in a timezone, so a naive input keeps the
  # common chronon naive rather than being adopted into the other's zone.
  expect_true(is.na(tz_name(chronon_glb(c(melb, naive)))))
})

test_that("combining a zoned time with a naive one is consistently wall-clock", {
  # Regression test: `c()` on a zoned + a naive element with the same
  # wall-clock reading used to silently convert the zoned element to its true
  # (UTC) absolute instant while leaving the naive element untouched, landing
  # both in the same combined chronon (naive, since they disagree) but on two
  # different numeric bases.
  zoned <- linear_time(
    datetime("2024-04-07 02:00:00", tz = "Australia/Melbourne"),
    hour(1L, tz = "Australia/Melbourne")
  )
  naive <- linear_time(datetime("2024-04-07 02:00:00"), hour(1L))

  combined <- c(zoned, naive)

  expect_true(is.na(tz_name(chronon_glb(combined))))
  expect_equal(as.numeric(vecvec::unvecvec(combined)), c(475682, 475682))
})

test_that("chronon_glb() of mixed known timezones formats without error", {
  # Regression test for tz-merge.md: mixing two real (but different) known
  # zones used to collapse the common chronon to naive, and formatting
  # through that naive chronon directly (as ggtime's axis breaks do) crashed.
  london <- datetime(seq(
    as.POSIXct("2026-07-30 06:00:00", tz = "Europe/London"),
    by = "1 hour",
    length.out = 3
  ))
  melbourne <- datetime(seq(
    as.POSIXct("2026-07-30 06:00:00", tz = "Australia/Melbourne"),
    by = "1 hour",
    length.out = 3
  ))
  combined <- c(london, melbourne)

  expect_equal(tz_name(chronon_glb(combined)), "UTC")

  x <- vecvec::unvecvec(mixtime(
    as.numeric(combined),
    chronon = chronon_glb(combined),
    discrete = FALSE
  ))
  expect_no_error(format(x))
})

test_that("tz_offset() of a POSIXct states one offset per element", {
  # A POSIXct carries a single timezone for the whole vector, unlike the
  # per-element timezone that `tz_name(time_chronon(x))` reports.
  x <- as.POSIXct(c("2024-01-01", "2024-06-01"), tz = "Australia/Melbourne")
  offset <- tz_offset(x)

  expect_true(all(time_is_duration(offset)))
  expect_equal(as.numeric(offset), c(39600, 36000))
  expect_equal(tz_name(chronon_glb(offset)), "Australia/Melbourne")
})

test_that("tz_offset() of a mixtime matches each timezone's own offsets", {
  mel <- as.POSIXct("2024-04-06", tz = "Australia/Melbourne") + 0:47 * 3600
  ber <- as.POSIXct("2024-03-30", tz = "Europe/Berlin") + 0:47 * 3600
  x <- c(mixtime(mel), mixtime(ber))[c(rbind(1:48, 49:96))]
  offset <- tz_offset(x)

  expect_true(is_mixtime(offset))
  expect_true(all(time_is_duration(offset)))
  # Each element keeps its own zone's offset, in its own chronon (hours here),
  # including across each zone's daylight saving transition.
  expect_equal(
    as.numeric(offset),
    c(rbind(
      as.numeric(tz_offset(mixtime(mel))),
      as.numeric(tz_offset(mixtime(ber)))
    ))
  )
  expect_setequal(as.numeric(offset), c(1, 2, 10, 11))

  # Long vectors stay fast: offsets are computed per timezone, not per element.
  long <- mixtime(as.POSIXct("2024-01-01", tz = "Australia/Melbourne") + 0:9999 * 3600)
  expect_lt(system.time(tz_offset(long))[["elapsed"]], 5)
})

test_that("tz_offset() of a mixtime keeps one granule per chronon", {
  mel <- "Australia/Melbourne"
  hours <- mixtime(
    as.POSIXct("2023-04-01 20:00", tz = mel) + 0:1 * 3600,
    chronon = cal_gregorian$hour(1L, tz = mel)
  )
  days <- mixtime(
    as.POSIXct("2023-04-03", tz = mel) + 0:1 * 86400,
    chronon = cal_gregorian$day(1L, tz = mel)
  )
  offset <- tz_offset(c(hours, days))

  # Each granule's offsets sit directly in `@x`, not nested in another mixtime.
  expect_length(offset@x, 2L)
  expect_false(any(vapply(offset@x, is_mixtime, logical(1L))))
  expect_equal(as.numeric(offset), c(11, 11, 10 / 24, 10 / 24))
})

test_that("tz_transitions() of a range with no transitions is an empty table", {
  transitions <- tz_transitions(
    as.POSIXct("2024-06-01", tz = "America/New_York"),
    as.POSIXct("2024-06-15", tz = "America/New_York")
  )

  expect_equal(nrow(transitions), 0)
  expect_named(transitions, c("time", "offset_before", "offset_after"))
})

test_that("discrete days keep fractional-offset midnights on the right day", {
  # Zones with a fractional-hour UTC offset, right near local midnight, where
  # the fractional parts of the time and offset could borrow across a day
  # boundary. Whole-hour zones (as used elsewhere in this file) don't exercise
  # it.

  # Asia/Kolkata: UTC+5:30, just after local midnight
  kolkata <- as.POSIXct("2024-06-15 00:00:30", tz = "Asia/Kolkata")
  expect_equal(format(date(kolkata, tz = "Asia/Kolkata")), "2024-06-15 IST")

  # Pacific/Chatham: UTC+12:45, just after local midnight
  chatham <- as.POSIXct("2024-06-15 00:10:00", tz = "Pacific/Chatham")
  expect_equal(format(date(chatham, tz = "Pacific/Chatham")), "2024-06-15 +1245")

  # Pacific/Marquesas: UTC-9:30, just before local midnight
  marquesas <- as.POSIXct("2024-06-15 23:50:00", tz = "Pacific/Marquesas")
  expect_equal(format(date(marquesas, tz = "Pacific/Marquesas")), "2024-06-15 -0930")

  # America/New_York: UTC-4:00 (EDT), just before local midnight
  new_york <- as.POSIXct("2024-06-15 23:59:30", tz = "America/New_York")
  expect_equal(format(date(new_york, tz = "America/New_York")), "2024-06-15 EDT")
})

test_that("discrete days resolve DST gap/overlap to the correct local day", {
  # Times near a DST transition must not pick up the offset from the wrong
  # side of it.

  # America/New_York spring-forward gap, 2024-03-10: 02:00 -> 03:00, and the
  # fall-back overlap, 2024-11-03: 02:00 occurs twice
  new_york <- as.POSIXct(
    c(
      "2024-03-09 23:59:58",
      "2024-03-10 00:00:02",
      "2024-03-10 23:59:58",
      "2024-03-11 00:00:02",
      "2024-11-02 23:59:58",
      "2024-11-03 23:59:58",
      "2024-11-04 00:00:02"
    ),
    tz = "America/New_York"
  )
  expect_equal(
    format(date(new_york, tz = "America/New_York")),
    c(
      "2024-03-09 EST",
      "2024-03-10 EST",
      "2024-03-10 EST",
      "2024-03-11 EDT",
      "2024-11-02 EDT",
      "2024-11-03 EDT",
      "2024-11-04 EST"
    )
  )

  # Australia/Melbourne fall-back day itself (2015-04-05), labelled by the
  # zone in effect when it starts
  melbourne <- as.POSIXct(
    c("2015-04-04 23:59:58", "2015-04-05 23:59:58", "2015-04-06 00:00:02"),
    tz = "Australia/Melbourne"
  )
  expect_equal(
    format(date(melbourne, tz = "Australia/Melbourne")),
    c("2015-04-04 AEDT", "2015-04-05 AEDT", "2015-04-06 AEST")
  )
})

test_that("discrete rounding survives floating-point error at unit boundaries", {
  # A conversion chains enough floating-point operations that a value exactly
  # on a `to`-boundary can land a few ULPs below it. A bare floor() then drops
  # a whole unit - this hit Europe/London specifically (seq() starting one
  # second before `from`), fixed with a small ULP-relative nudge before
  # flooring.
  w <- cal_isoweek$week(1L)
  zones <- c(
    "UTC",
    "Etc/GMT-10",
    "Australia/Melbourne",
    "America/New_York",
    "Asia/Kolkata",
    "Europe/London"
  )

  for (tz in zones) {
    from <- time_floor(
      datetime(as.POSIXct("2015-06-01", tz = tz)),
      cal_isoweek$week(1L, tz = tz)
    )
    to <- time_ceiling(
      datetime(as.POSIXct("2015-06-22", tz = tz)),
      cal_isoweek$week(1L, tz = tz)
    )
    s <- seq(from, to, by = cal_isoweek$week(1L, tz = tz))

    expect_equal(as.numeric(s[1]), as.numeric(from), info = tz)
    expect_length(s, 5)
  }
})

test_that("mixtime()/datetime() preserve both instants across a DST fall-back overlap", {
  # Melbourne falls back 03:00 AEDT -> 02:00 AEST on 2024-04-07, so local
  # wall-clock 02:00-02:45 occurs twice. Building `before`/`after` from
  # unambiguous absolute-time arithmetic (rather than an ambiguous literal
  # string, which has its own separate resolution question) means they are
  # genuinely distinct instants that happen to share a local reading.
  #
  # Regression test: converting through the local wall-clock reading maps
  # both instants to the same ambiguous reading, after which nothing can
  # recover which instant `before` came from.
  before <- as.POSIXct(
    as.POSIXct("2024-04-06 15:30:00", tz = "UTC"),
    tz = "Australia/Melbourne"
  )
  after <- as.POSIXct(
    as.POSIXct("2024-04-06 16:30:00", tz = "UTC"),
    tz = "Australia/Melbourne"
  )
  expect_false(identical(as.numeric(before), as.numeric(after)))

  expect_equal(format(as.POSIXct(mixtime(before)), tz = "UTC"), "2024-04-06 15:30:00")
  expect_equal(format(as.POSIXct(mixtime(after)), tz = "UTC"), "2024-04-06 16:30:00")
  expect_equal(format(as.POSIXct(datetime(before)), tz = "UTC"), "2024-04-06 15:30:00")
  expect_equal(format(as.POSIXct(datetime(after)), tz = "UTC"), "2024-04-06 16:30:00")

  # A 15-minute series spanning the whole transition round-trips exactly,
  # point for point, on both sides of it.
  series <- as.POSIXct("2024-04-06 21:00:00", tz = "Australia/Melbourne") +
    (0:36) * 900
  roundtrip <- format(as.POSIXct(datetime(series)), tz = "UTC")
  expect_equal(roundtrip, format(series, tz = "UTC"))
})

test_that("comparing a zoned time against a naive one uses wall-clock time", {
  # Regression test: comparison used to convert the zoned operand to its true
  # (UTC) absolute instant while leaving the naive operand untouched, mixing
  # two different bases and producing numerically wrong results. Both sides
  # should instead be compared at face value, as `datetime(x, tz = NA)`
  # already does for a single value (wall-clock time is preserved, not the
  # absolute instant).
  zoned <- linear_time(
    datetime("2015-02-01 10:00:00", tz = "Australia/Melbourne"),
    hour(1L, tz = "Australia/Melbourne")
  )
  naive <- datetime("2015-02-01 00:00:00")

  expect_true(zoned > naive)
  expect_false(zoned < naive)
  expect_false(zoned == naive)

  # Same-zone comparisons must keep comparing true absolute instants (not be
  # affected by the wall-clock stripping used for naive/zoned comparisons).
  melb <- datetime(as.POSIXct("2015-01-01 00:00:00", tz = "Australia/Melbourne"))
  utc <- datetime(as.POSIXct("2015-01-01 00:00:00", tz = "UTC"))
  expect_true(melb < utc)
})

# --- Zoned chronons across a DST transition -------------------------------
#
# A zoned chronon's value is the index of its local bucket (e.g. a local day)
# plus the elapsed fraction of that bucket, so conversions between zoned
# chronons keep distinct instants distinct and reflect a 23 or 25 hour day.

test_that("chronon_convert() to a coarser tz-aware chronon preserves both instants across a DST fall-back overlap", {
  # `before`/`after` are exactly 1 real hour apart (see "mixtime()/datetime()
  # preserve both instants..." above for how they're constructed
  # unambiguously), so converting to an hour-granularity chronon in the same
  # zone must keep them distinguishable, and converting back to seconds must
  # recover each exactly.
  before <- mixtime(as.POSIXct(
    as.POSIXct("2024-04-06 15:30:00", tz = "UTC"),
    tz = "Australia/Melbourne"
  ))
  after <- mixtime(as.POSIXct(
    as.POSIXct("2024-04-06 16:30:00", tz = "UTC"),
    tz = "Australia/Melbourne"
  ))

  hr <- cal_gregorian$hour(1L, tz = "Australia/Melbourne")
  sec <- cal_gregorian$second(1L, tz = "Australia/Melbourne")

  before_hr <- chronon_convert(before, hr, discrete = FALSE)
  after_hr <- chronon_convert(after, hr, discrete = FALSE)

  # Exactly 1 hour apart in real time, so they must stay distinguishable as
  # hour-granule values too - not both collapse to the same fractional hour.
  expect_false(isTRUE(all.equal(as.numeric(before_hr), as.numeric(after_hr))))

  # And the conversion must round-trip: going back to seconds recovers the
  # original UTC instants exactly, for both.
  before_rt <- as.POSIXct(
    chronon_convert(
      linear_time(before_hr, chronon = hr, discrete = FALSE),
      sec,
      discrete = TRUE
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )
  after_rt <- as.POSIXct(
    chronon_convert(
      linear_time(after_hr, chronon = hr, discrete = FALSE),
      sec,
      discrete = TRUE
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )
  expect_equal(format(before_rt, tz = "UTC"), "2024-04-06 15:30:00")
  expect_equal(format(after_rt, tz = "UTC"), "2024-04-06 16:30:00")
})

test_that("chronon_convert() to a continuous day position reflects true elapsed hours across a DST transition", {
  # Melbourne's fall-back day 2024-04-07 has 25 real hours (03:00 AEDT ->
  # 02:00 AEST repeats the 02:xx hour), so a continuous "day" position built
  # from hourly ticks should advance by 1/25 per hour on this day, not the
  # nominal 1/24. It must also land on the integer boundary at the following
  # local midnight, agreeing with the discrete day.
  day_start <- as.POSIXct("2024-04-06 13:00:00", tz = "UTC") # 2024-04-07 00:00 AEDT
  hourly <- as.POSIXct(day_start + (0:25) * 3600, tz = "Australia/Melbourne") # 26 ticks over the 25-hour day

  # From both the detected (hourly) chronon and an explicit second chronon
  for (x in list(
    mixtime(hourly),
    mixtime(hourly, cal_gregorian$second(1L, tz = "Australia/Melbourne"))
  )) {
    day_pos <- as.numeric(chronon_convert(
      x,
      cal_gregorian$day(1L, tz = "Australia/Melbourne"),
      discrete = FALSE
    ))

    # Every real hour is a distinct point in continuous day-space, including
    # the two local 02:00 readings either side of the transition.
    expect_length(unique(day_pos), 26)

    # Each step is 1/25 of the (25-real-hour) day, uniformly - not 1/24 with a
    # doubled-up jump where the two local 02:00 readings collapse.
    expect_equal(diff(day_pos), rep(1 / 25, 25), tolerance = 1e-9)

    # A full local day is exactly 1.0 day-units later, landing exactly back on
    # an integer boundary that agrees with the (already-correct) discrete day
    # index for the same instant.
    expect_equal(day_pos[26] - day_pos[1], 1, tolerance = 1e-9)
    expect_equal(day_pos[26] %% 1, 0, tolerance = 1e-9)
    expect_equal(
      floor(day_pos[26]),
      as.numeric(date(hourly[26], tz = "Australia/Melbourne"))
    )
  }
})

test_that("tz-aware day mixtime round trips to local midnight via as.POSIXct()", {
  # Regression: as.POSIXct() converted to a UTC second chronon, which skipped
  # all tz handling and read the local day label as a UTC day
  # (2025-01-01 11:00 AEDT rather than 00:00). Includes both Melbourne DST
  # transition dates (fall-back 2025-04-06, spring-forward 2025-10-05).
  tz <- "Australia/Melbourne"
  x <- as.POSIXct(
    c("2025-01-01", "2025-01-02", "2025-04-06", "2025-10-05"),
    tz = tz
  )
  for (discrete in c(TRUE, FALSE)) {
    m <- mixtime(x, cal_gregorian$day(1L, tz = tz), discrete = discrete)
    expect_equal(as.POSIXct(m), x, ignore_attr = TRUE)
    expect_equal(format(as.POSIXct(m), tz = tz), format(x))
  }

  # Sub-day instants on transition days floor to that day's local midnight
  y <- as.POSIXct(
    c("2025-04-06 12:34:56", "2025-04-06 02:30:00", "2025-10-05 03:30:00"),
    tz = tz
  )
  m <- mixtime(y, cal_gregorian$day(1L, tz = tz))
  expect_equal(
    format(as.POSIXct(m), "%Y-%m-%d %H:%M:%S", tz = tz),
    c("2025-04-06 00:00:00", "2025-04-06 00:00:00", "2025-10-05 00:00:00")
  )
})

test_that("tz-aware days cast to POSIXct at local midnight", {
  tz <- "Australia/Melbourne"
  d <- as.POSIXct(c("2025-01-01", "2025-01-02", "2025-04-06", "2025-10-05"), tz = tz)
  day <- cal_gregorian$day(1L, tz = tz)
  expect_equal(as.POSIXct(mixtime(d, day)), d)
  expect_equal(as.POSIXct(mixtime(d, day, discrete = FALSE)), d)
  # Sub-day times floor to local midnight
  expect_equal(as.POSIXct(mixtime(d + 3600 * c(5, 23.5, 20, 12), day)), d)
  # Days on transition days are labelled by the zone in effect at their start
  expect_equal(
    tz_abbreviation(mixtime(as.POSIXct(c("2024-04-07", "2024-10-06"), tz = tz), day)),
    c("AEDT", "AEST")
  )
  expect_equal(tz_abbreviation(mixtime(as.POSIXct("2024-04-07 00:00:00", tz = tz))), "AEDT")
})

test_that("zoned months, weeks and years start at local midnight", {
  tz <- "Australia/Melbourne"
  for (chronon in list(
    cal_gregorian$month(1L, tz = tz),
    cal_isoweek$week(1L, tz = tz),
    cal_gregorian$year(1L, tz = tz)
  )) {
    # 2025-04-01 is AEDT midnight, which is on the previous day in UTC
    x <- as.POSIXct(c("2025-01-01", "2025-04-01", "2025-03-31"), tz = tz)
    start <- as.POSIXct(time_floor(mixtime(x), chronon), tz = tz)
    for (discrete in c(TRUE, FALSE)) {
      m <- mixtime(x, chronon, discrete = discrete)
      expect_equal(as.POSIXct(mixtime(m, chronon, discrete = TRUE)), start, ignore_attr = TRUE)
      expect_equal(format(start, "%H:%M:%S"), rep("00:00:00", 3))
    }
    # A continuous position floors to the discrete one
    y <- as.POSIXct("2025-01-01", tz = tz) + 86400 * 0:400 + 3600 * 13
    expect_equal(
      floor(as.numeric(mixtime(y, chronon, discrete = FALSE))),
      as.numeric(mixtime(y, chronon))
    )
  }
})

test_that("hourly data keeps both hours at the end of daylight saving", {
  tz <- "Australia/Melbourne"
  h <- as.POSIXct("2025-04-05 14:00:00", tz = "UTC") + 3600 * 0:4
  attr(h, "tzone") <- tz
  m <- mixtime(h)
  expect_length(unique(as.numeric(m)), 5)
  expect_equal(as.numeric(as.POSIXct(m)), as.numeric(h))
  expect_equal(
    format(m),
    paste("2025-04-06", c("01h AEDT", "02h AEDT", "02h AEST", "03h AEST", "04h AEST"))
  )
})

test_that("the fraction through a zoned chronon is in its own time zone", {
  # Brisbane has no daylight saving: 2026-08-19 02:24 AEST is 30% through
  # ISO week 34 (2 days 2.4 hours of 7 days), but only 24% through that week
  # in UTC, which starts 10 hours later.
  x <- as.POSIXct("2026-08-19 02:24", tz = "Australia/Brisbane")
  fmt <- "{lin(year)} W{cyc(week, year)} {frac(.time)}"
  bne <- mixtime(x, cal_isoweek$week(1L, tz = "Australia/Brisbane"), discrete = FALSE)
  utc <- mixtime(x, cal_isoweek$week(1L, tz = "UTC"), discrete = FALSE)
  expect_equal(format(bne, fmt), "2026 W34 30.0%")
  expect_equal(format(utc, fmt), "2026 W34 24.0%")
})

test_that("calendar arithmetic on zoned times keeps the local clock time", {
  tz <- "Australia/Melbourne"
  x <- mixtime(as.POSIXct("2025-01-31 10:00", tz = tz))
  expect_equal(
    as.POSIXct(x + duration(1L, cal_gregorian$month(1L))),
    as.POSIXct("2025-02-28 10:00", tz = tz),
    ignore_attr = TRUE
  )
  # A local time that occurs twice resolves to its first occurrence
  x <- mixtime(as.POSIXct("2025-04-05 02:30", tz = tz))
  expect_equal(
    format(x + duration(1L, cal_gregorian$day(1L))),
    "2025-04-06 02:30:00 AEDT"
  )
})

test_that("solar time converts to zoned civil time through absolute time", {
  tz <- "Australia/Melbourne"
  x <- as.numeric(as.POSIXct("2025-01-01 00:00", tz = tz))
  utc <- cal_time_civil$second(1L, tz = "UTC")
  solar <- cal_time_solar$second(1L, lat = -37.8, lon = 144.96)
  day <- cal_gregorian$day(1L, tz = tz)
  at_solar <- chronon_convert_impl(x, utc, solar, discrete = FALSE)
  expect_equal(
    chronon_convert_impl(at_solar, solar, day, discrete = FALSE),
    chronon_convert_impl(x, utc, day, discrete = FALSE)
  )
})

test_that("as.POSIXct() keeps the time zone of a mixtime for round trips", {
  # Regression: as.POSIXct() returned a POSIXct with an empty time zone, which
  # mixtime() then rejected.
  tz <- "Australia/Brisbane"
  x <- as.POSIXct("2026-08-19 02:24", tz = tz)
  w <- mixtime(x, cal_isoweek$week(1L, tz = tz), discrete = FALSE)
  expect_equal(attr(as.POSIXct(w), "tzone"), tz)
  expect_equal(tz_name(mixtime(as.POSIXct(w))), tz)
  expect_equal(as.POSIXct(mixtime(x)), x)
  # An explicit time zone is still respected
  expect_equal(attr(as.POSIXct(w, tz = "UTC"), "tzone"), "UTC")
  # Several time zones fall back to UTC
  m <- c(mixtime(x), mixtime(as.POSIXct("2026-08-19 02:24", tz = "UTC")))
  expect_equal(attr(as.POSIXct(m), "tzone"), "UTC")
})

test_that("mixtime() reads an empty POSIXct time zone as the local time zone", {
  old <- Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old))
  Sys.setenv(TZ = "America/New_York")

  x <- as.POSIXct("2026-08-19 02:24", tz = "")
  m <- mixtime(x)
  expect_equal(tz_name(m), "America/New_York")
  expect_equal(format(m), "2026-08-19 02:24:00 EDT")
  expect_equal(as.numeric(as.POSIXct(m)), as.numeric(x))
})

test_that("time zone lookups match one-at-a-time lookups in any order", {
  # Offsets and abbreviations reuse the previous lookup while times stay in
  # the same period, so cover shuffled times either side of transitions.
  tz <- "Australia/Melbourne"
  x <- as.numeric(as.POSIXct("2024-03-30", tz = tz)) + 1800 * (0:400)
  x <- c(sample(x), NA, x)
  one_at_a_time <- vapply(x, function(s) get_tz_offset(s, tz), numeric(1L))
  expect_equal(get_tz_offset(x, tz), one_at_a_time)
  expect_equal(
    get_tz_abbreviation(x, tz),
    vapply(x, function(s) get_tz_abbreviation(s, tz), character(1L))
  )

  # Interleaved time zones, one per element
  zones <- rep_len(c(tz, tz, "Europe/Berlin", "UTC"), length(x))
  expect_equal(
    get_tz_offset(x, zones),
    mapply(function(s, z) get_tz_offset(s, z), x, zones, USE.NAMES = FALSE)
  )
  expect_equal(
    get_tz_abbreviation(x, zones),
    mapply(function(s, z) get_tz_abbreviation(s, z), x, zones, USE.NAMES = FALSE)
  )
})

test_that("fractional times before 1970 use the offset of the second they fall in", {
  # New York's 1960 transition: half a second before it is still EST
  tz <- "America/New_York"
  t0 <- as.numeric(as.POSIXct("1960-04-24 03:00:00", tz = tz))
  expect_equal(get_tz_offset(c(t0 - 1, t0 - 0.5, t0), tz), c(-18000, -18000, -14400))
  expect_equal(get_tz_abbreviation(t0 - 0.5, tz), "EST")
})

test_that("parsing a local time skipped by daylight saving gives NA", {
  # Melbourne goes forward from 02:00 AEST to 03:00 AEDT on 2025-10-05, like
  # an invalid date (e.g. "2026-02-30") fails to parse
  expect_warning(
    x <- datetime(
      c("2025-10-05 02:30:00", "2025-10-05 03:30:00"),
      tz = "Australia/Melbourne"
    ),
    "does not exist"
  )
  expect_equal(is.na(x), c(TRUE, FALSE))
  expect_equal(format(x[2]), "2025-10-05 03:30:00 AEDT")
})


test_that("continuous zoned days round trip to the same instant", {
  # A day count near 20000 holds a time to about 3e-7 seconds, so allow 1e-6
  tz <- "Australia/Melbourne"
  times <- c("00:00:00", "01:15:00", "03:30:00", "07:45:12.5", "12:00:00", "23:59:59")
  # Spring-forward (23 hours), fall-back (25 hours) and an ordinary day
  for (day in c("2025-10-05", "2025-04-06", "2025-06-15")) {
    p <- as.POSIXct(paste(day, times), tz = tz)
    d <- mixtime(p, cal_gregorian$day(1L, tz = tz), discrete = FALSE)
    err <- as.numeric(as.POSIXct(d)) - as.numeric(p)
    expect_true(all(abs(err) < 1e-6), info = day)
  }
})
