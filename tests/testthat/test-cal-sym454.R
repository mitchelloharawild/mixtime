test_that("Cardinality of the Symmetric454 calendar: cal_sym454", {
  # 1970 is a leap year, so Jan has 4 weeks, Feb has 5 weeks, Mar has 4 weeks, ..., Dec has 5 weeks
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(1L), at = 0:11L)),
    c(4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 5L)
  )
  # 1971 is not a leap year; its months are at positions 12..23 from the epoch
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(1L), at = 12:23L)),
    c(4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 4L)
  )

  # 1970 is a leap year and should have 53 weeks
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), year(1L), at = 0L)),
    53L
  )
  # 1971 is not a leap year and should have 52 weeks
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), year(1L), at = 12L)),
    52L
  )

  # Each quarter (3 months) should have 13 weeks except the last having 14 weeks in a leap year
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(3L), at = 0:3L)),
    c(13L, 13L, 13L, 14L)
  )

  # Each quarter (3 months) should have 13 weeks in a non-leap year
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(3L), at = 4:7L)),
    c(13L, 13L, 13L, 13L)
  )
})

test_that("Divmod of the Symmetric454 calendar is invertible via Date round-trip", {
  # Converting Sym454 year integers -> Date -> Sym454 year (continuous) should
  # recover the original values exactly, since each integer maps to the first
  # day of that Sym454 year.
  years <- 1970:2000
  sym454_years <- linear_time(years, chronon = cal_sym454$year(1L))
  dates <- as.Date(sym454_years)
  recovered <- linear_time(dates, chronon = cal_sym454$year(1L), discrete = FALSE)
  expect_equal(as.double(recovered), as.double(sym454_years))
})


test_that("Divmod of the Symmetric454 calendar: cal_sym454", {
  # Regular 4-5-4 pattern in 1970 (leap year)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(1L), month(1L), c(0L, 4L, 9L, 13L, 51L, 52L))),
    list(div = c(0L, 1L, 2L, 3L, 11L, 11L), mod = c(0L, 0L, 0L, 0L, 3L, 4L))
  )
  # Regular 4-5-4 pattern in 1971 (non-leap year)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(1L), month(1L), c(53L, 55L, 62L, 66L, 104L))),
    list(div = c(12L, 12L, 14L, 15L, 23L), mod = c(0L, 2L, 0L, 0L, 3L))
  )

  # 2-week units (fortnights)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(2L), month(1L), c(0L, 4L, 5L, 9L, 13L))),
    list(div = c(0L, 1L, 2L, 4L, 6L), mod = c(0L, 2L, 0L, 0L, 0L))
  )

  # Multi-month units (quarters)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(1L), month(3L), c(0L, 13L, 26L, 39L))),
    list(div = c(0L, 1L, 2L, 3L), mod = c(0L, 0L, 0L, 0L))
  )

  # Fortnight units with multi-month units (2-week quarters)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(2L), month(3L), c(0L, 3L, 13L, 26L, 39L))),
    list(div = c(0L, 0L, 2L, 3L, 5L), mod = c(0L, 3L, 0L, 6L, 6L))
  )
})

test_that("Divmod of the Symmetric454 calendar (month -> week) agrees with cardinality, including leap-year December", {
  # month -> week is the forward direction of the tests above. Its result
  # (the week-index a month starts on) must equal the cumulative sum of
  # week -> month cardinalities. This must hold across a leap-year December
  # (months 549:552 = Oct 2015..Jan 2016; 2015 is a Sym454 leap year), where
  # a previous bug double-counted December's own leap week, making its
  # starting week (and everything from Jan 2016 onwards) one week too late.
  months_idx <- 548:554
  fwd <- with(cal_sym454, chronon_divmod(month(1L), week(1L), months_idx))$div
  card <- with(cal_sym454, chronon_cardinality(week(1L), month(1L), at = 0:554))
  expected <- cumsum(c(0L, card))[months_idx + 1L]
  expect_equal(fwd, expected)

  # Same check for multi-month (quarter) units spanning the same leap-year
  # December -- quarter 183 = Oct/Nov/Dec 2015, quarter 184 = Jan/Feb/Mar 2016.
  expect_equal(
    with(cal_sym454, chronon_divmod(month(3L), week(1L), 182:185))$div,
    c(2374L, 2387L, 2401L, 2414L)
  )

  # Systematic check across 1970-2028: every month's forward divmod must
  # agree with the cumulative cardinality, not just leap-year Decembers.
  months_idx <- 0:700
  fwd <- with(cal_sym454, chronon_divmod(month(1L), week(1L), months_idx))$div
  card <- with(cal_sym454, chronon_cardinality(week(1L), month(1L), at = months_idx))
  expected <- cumsum(c(0L, card))[seq_along(months_idx)]
  expect_equal(fwd, expected)
})
