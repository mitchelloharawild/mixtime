# mt_cyclical_free ("cyclical time without knowledge of which cycle it is
# in") - see `_dev/cycle.md` for the design this exercises.

# ---------------------------------------------------------------------------
# Construction
# ---------------------------------------------------------------------------

test_that("bare numeric input to a cyclical helper builds mt_cyclical_free, not mt_cyclical", {
  x <- day_of_year(60)
  expect_true(time_is_cyclical_free(x))
  expect_false(time_is_cyclical(x))
  expect_true(S7::S7_inherits(x@x[[1L]], mt_cyclical_free))
})

test_that("Date/POSIXct input still builds an anchored mt_cyclical", {
  expect_true(time_is_cyclical(day_of_year(as.Date("2020-01-01"))))
})

test_that("an irregular chronon/cycle pair (no fixed cardinality) is never bounded", {
  # day/year's cardinality varies by year (365/366), so there's no fixed bound.
  expect_equal(format(day_of_year(1000)), "D1000")
  expect_equal(format(day_of_year(366)), "D366")
  expect_false(is.na(day_of_year(1000)))
})

test_that("a fixed chronon/cycle pair rejects out-of-range bare input as NA", {
  # month/year is fixed at 12 months.
  expect_true(is.na(month_of_year(0)))
  expect_true(is.na(month_of_year(13)))
  expect_false(is.na(month_of_year(1)))
  expect_false(is.na(month_of_year(12)))

  # day/week (7 days) and second/day (86400 seconds) are likewise fixed.
  expect_true(is.na(day_of_week(0)))
  expect_true(is.na(day_of_week(8)))
  expect_true(is.na(time_of_day(86400)))
  expect_false(is.na(time_of_day(0)))
})

test_that("a free value's indexing follows the chronon's own registered label scheme", {
  # day/month/year register a 1-indexed ("everyday counting") scheme.
  expect_equal(format(day_of_year(60)), "D60")
  expect_equal(format(month_of_year(3)), "Mar")
  expect_equal(
    time_compose(cyc(month, year) ~ 3)@x[[1L]],
    month_of_year(3)@x[[1L]]
  )

  # time-of-day starts at 0: time_of_day(0) is midnight, not "the 1st second".
  expect_equal(format(time_of_day(0)), "00:00:00")
})

test_that("time_compose()/time_parse() with no lin() anchor build mt_cyclical_free", {
  expect_true(time_is_cyclical_free(time_compose(cyc(month, year) ~ 3)))
  expect_true(time_is_cyclical_free(time_parse("Feb", format = "{cyc(month, year, label = TRUE)}")))
})

# ---------------------------------------------------------------------------
# Comparison: magnitude, not interval (mirrors mt_duration-compare)
# ---------------------------------------------------------------------------

test_that("free values compare as plain magnitudes in a shared cycle", {
  expect_true(day_of_year(60) == day_of_year(60))
  expect_true(day_of_year(60) < day_of_year(61))
  expect_false(day_of_year(60) > day_of_year(61))
  expect_true(day_of_year(60) != day_of_year(61))
})

test_that("free values of differing chronons compare in their common chronon", {
  # hour is unshifted (offset 24); day 2 (1-indexed) is offset 1, i.e. 24 hours.
  hoy <- cyclical_time(24, hour(1L), year(1L))
  doy <- day_of_year(2)
  expect_true(hoy == doy)
})

test_that("free values of differing cycles can't be compared", {
  dow <- day_of_week(3)
  doy <- day_of_year(60)
  expect_error(dow == doy, class = "vctrs_error_incompatible_op")
})

test_that("comparing chronons that require `at` errors, since a free value never has one", {
  expect_error(
    day_of_year(60) == month_of_year(3),
    "requires the time context"
  )
})

test_that("Allen's interval algebra is not supported for mt_cyclical_free (matches mt_duration)", {
  expect_error(day_of_year(60) %p% day_of_year(61))
})

# ---------------------------------------------------------------------------
# Comparison against an anchored mt_cyclical
# ---------------------------------------------------------------------------

test_that("a free value compares against an anchored mt_cyclical by magnitude", {
  expect_true(day_of_year(60) == day_of_year(as.Date("2020-02-29")))
  expect_true(month_of_year(3) == month_of_year(as.Date("2020-03-15")))
  expect_false(day_of_year(61) == day_of_year(as.Date("2020-02-29")))
})

# ---------------------------------------------------------------------------
# Compositional disambiguation (no MonthDay-style compound type needed)
# ---------------------------------------------------------------------------

test_that("exact Feb 29 match is recovered by intersecting free predicates", {
  t <- day_of_month(as.Date("2020-02-29"))
  expect_true(t == day_of_month(29))
  expect_true(month_of_year(as.Date("2020-02-29")) == month_of_year(2))
})

# ---------------------------------------------------------------------------
# Format
# ---------------------------------------------------------------------------

test_that("format() falls back to a plain numeric token with no `at`", {
  expect_equal(format(day_of_year(60)), "D60")
  expect_equal(format(day_of_month(29)), "D29")
})

test_that("format() supports named labels for chronons with a label scheme", {
  expect_equal(format(month_of_year(3)), "Mar")
  expect_equal(format(day_of_week(3)), "Wed")
})

test_that("format() decomposes a chained multi-token template without chronon_divmod()", {
  fmt <- chronon_parse_cyclical(cal_gregorian$second(1L), cal_gregorian$day(1L))
  r <- time_parse("09:30:45", format = fmt, calendar = cal_gregorian)
  expect_true(time_is_cyclical_free(r))
  expect_equal(format(r), "09:30:45")
})

test_that("format() accepts a custom format string, like format.mt_time", {
  x <- day_of_year(60)
  expect_equal(format(x, "{cyc(day,year)}"), "60")
})

test_that("NA and infinite values format like other time types", {
  expect_equal(format(day_of_year(NA_integer_)), "NA")
})
