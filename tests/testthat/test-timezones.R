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
