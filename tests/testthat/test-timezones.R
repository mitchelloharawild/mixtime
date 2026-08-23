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

  expect_equal(tz_name(chronon_common(c(secs, secs))), tz)
  expect_equal(tz_name(chronon_common(c(secs, hrs))), tz)

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

  expect_equal(tz_name(chronon_common(c(melb, utc))), "UTC")
  # Naive time cannot be represented in a timezone, so a naive input keeps the
  # common chronon naive rather than being adopted into the other's zone.
  expect_true(is.na(tz_name(chronon_common(c(melb, naive)))))
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

  expect_true(is.na(tz_name(chronon_common(combined))))
  expect_equal(as.numeric(vecvec::unvecvec(combined)), c(475682, 475682))
})

test_that("chronon_common() of mixed known timezones formats without error", {
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

  expect_equal(tz_name(chronon_common(combined)), "UTC")

  x <- vecvec::unvecvec(mixtime(
    as.numeric(combined),
    chronon = chronon_common(combined),
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
  expect_equal(tz_name(chronon_common(offset)), "Australia/Melbourne")
})

test_that("tz_transitions() of a range with no transitions is an empty table", {
  transitions <- tz_transitions(
    as.POSIXct("2024-06-01", tz = "America/New_York"),
    as.POSIXct("2024-06-15", tz = "America/New_York")
  )

  expect_equal(nrow(transitions), 0)
  expect_named(transitions, c("time", "offset_before", "offset_after"))
})

test_that("tz_to_utc() discrete rounding keeps fractional-offset midnights on the right day", {
  # Regression test for the discrete branch of tz_to_utc(): naively flooring
  # `x - tzo` (rather than `floor(x) - trunc(tzo)`) lets the fractional parts
  # of the local-shifted value and the re-derived offset interact and borrow
  # across a day boundary, silently landing on the wrong calendar day. This
  # only shows up for zones with a fractional-hour UTC offset, right near
  # local midnight, so whole-hour zones (as used elsewhere in this file)
  # don't exercise it.

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

test_that("tz_to_utc() discrete rounding resolves DST gap/overlap to the correct local day", {
  # The offset used by tz_to_utc() is re-derived at the candidate raw instant
  # (a second `tz_offset_impl()` pass), rather than reused from the forward
  # shift, precisely so a value near a DST transition doesn't pick up the
  # wrong side of it.

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

  # Australia/Melbourne fall-back day itself (2015-04-05)
  melbourne <- as.POSIXct(
    c("2015-04-04 23:59:58", "2015-04-05 23:59:58", "2015-04-06 00:00:02"),
    tz = "Australia/Melbourne"
  )
  expect_equal(
    format(date(melbourne, tz = "Australia/Melbourne")),
    c("2015-04-04 AEDT", "2015-04-05 AEST", "2015-04-06 AEST")
  )
})

test_that("tz_to_utc() discrete rounding survives the shift/unshift ULP chain", {
  # The shift/unshift pair in chronon_convert_impl() (tz_to_local() before
  # the path conversion, tz_to_utc() after it) chains enough floating-point
  # operations that a value exactly on a `to`-boundary can land a few ULPs
  # below it. A bare floor() in the discrete branch then drops a whole unit -
  # this hit Europe/London specifically (seq() starting one second before
  # `from`), fixed with a small ULP-relative nudge before flooring.
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
