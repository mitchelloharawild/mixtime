test_that("year() converts dates correctly", {
  # Basic conversions
  expect_equal(format(year(as.Date("1970-01-01"))), "1970")
  expect_equal(format(year(as.Date("2025-12-16"))), "2025")
  
  # Edge case: last day of year
  expect_equal(format(year(as.Date("1970-12-31"))), "1970")
  expect_equal(format(year(as.Date("2025-12-31"))), "2025")
  
  # Edge case: first day of year
  expect_equal(format(year(as.Date("1971-01-01"))), "1971")
  expect_equal(format(year(as.Date("2026-01-01"))), "2026")
  
  # Vector consistency check over large range
  dates <- as.Date(0:25000, origin = "1970-01-01")
  diff <- format(year(dates)) == format(dates, "%Y")
  expect_true(all(diff))
  
  # Fractional years from dates
  y_start <- as.numeric(year(as.Date("2025-01-01"), discrete = FALSE))
  y_mid <- as.numeric(year(as.Date("2025-07-01"), discrete = FALSE))
  y_end <- as.numeric(year(as.Date("2025-12-31"), discrete = FALSE))
  
  expect_equal(y_start, 55)
  expect_equal(y_mid, 55.49589, tolerance = 1e-5)
  expect_equal(y_end, 55.99726, tolerance = 1e-5)
})

test_that("year() handles leap years correctly", {
  # Leap year boundaries
  expect_equal(format(year(as.Date("1972-02-29"))), "1972")
  expect_equal(format(year(as.Date("2000-02-29"))), "2000")
  expect_equal(format(year(as.Date("2024-02-29"))), "2024")
  
  # Century years (1900 not leap, 2000 is leap)
  expect_equal(format(year(as.Date("1900-02-28"))), "1900")
  expect_equal(format(year(as.Date("2000-02-29"))), "2000")
  expect_equal(format(year(as.Date("2100-02-28"))), "2100")
  
  # Day after leap day
  expect_equal(format(year(as.Date("1972-03-01"))), "1972")
  expect_equal(format(year(as.Date("2000-03-01"))), "2000")
  
  # Fractional years for leap year dates
  y_leap_start <- as.numeric(year(as.Date("2024-01-01"), discrete = FALSE))
  y_leap_mid <- as.numeric(year(as.Date("2024-07-01"), discrete = FALSE))
  
  # Leap year has 366 days, so midpoint fraction should be different
  expect_equal(y_leap_start, 54)
  expect_equal(y_leap_mid, 54.49727, tolerance = 1e-5)
})

test_that("year() handles discrete vs continuous time", {
  # Discrete should return integer chronons
  y_discrete <- year(Sys.time(), discrete = TRUE)
  expect_true(is.integer(as.integer(y_discrete)))
  
  # Continuous should return fractional chronons
  y_continuous <- year(Sys.time(), discrete = FALSE)
  y_numeric <- as.numeric(y_continuous)
  expect_true(y_numeric - floor(y_numeric) > 0)
  
  # Continuous should progress through the year
  t1 <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2025-07-01 00:00:00", tz = "UTC")
  y1 <- as.numeric(year(t1, discrete = FALSE))
  y2 <- as.numeric(year(t2, discrete = FALSE))
  expect_true(y2 > y1)
})

test_that("yearmonth() converts dates correctly", {
  # Basic conversions
  expect_equal(format(yearmonth(as.Date("1970-01-01"))), "1970 Jan")
  expect_equal(format(yearmonth(as.Date("1970-02-01"))), "1970 Feb")
  
  # Edge case: end of month
  expect_equal(format(yearmonth(as.Date("1970-01-31"))), "1970 Jan")
  expect_equal(format(yearmonth(as.Date("1970-02-28"))), "1970 Feb")
  
  # Edge case: start of next month
  expect_equal(format(yearmonth(as.Date("1970-02-01"))), "1970 Feb")
  
  # Vector consistency check
  dates <- as.Date(0:150000, origin = "1970-01-01")
  diff <- format(yearmonth(dates)) == format(dates, "%Y %b")
  expect_true(all(diff))
})

test_that("yearmonth() handles leap year months correctly", {
  # February in leap years
  expect_equal(format(yearmonth(as.Date("1972-02-29"))), "1972 Feb")
  expect_equal(format(yearmonth(as.Date("2000-02-29"))), "2000 Feb")
  expect_equal(format(yearmonth(as.Date("2024-02-29"))), "2024 Feb")
  
  # March after leap day
  expect_equal(format(yearmonth(as.Date("1972-03-01"))), "1972 Mar")
  expect_equal(format(yearmonth(as.Date("2000-03-01"))), "2000 Mar")
})

test_that("yearquarter() converts correctly", {
  # All quarters
  expect_equal(format(yearquarter(0L)), "1970 Q1")
  expect_equal(format(yearquarter(1L)), "1970 Q2")
  expect_equal(format(yearquarter(2L)), "1970 Q3")
  expect_equal(format(yearquarter(3L)), "1970 Q4")
  
  # Next year
  expect_equal(format(yearquarter(as.Date("1971-02-04"))), "1971 Q1")
  
  # Vector
  expect_length(yearquarter(0:7), 8)
})

test_that("chronon_divmod between days and months handles edge cases", {
  # Algorithm anchor date (0000-03-01 in proleptic Gregorian)
  # Days since epoch for key dates
  
  # Start of months
  expect_equal(format(yearmonth(as.Date("1970-01-01"))), "1970 Jan")
  expect_equal(format(yearmonth(as.Date("1970-12-01"))), "1970 Dec")
  expect_equal(format(yearmonth(as.Date("1971-01-01"))), "1971 Jan")
  
  # End of months (should still be in that month)
  expect_equal(format(yearmonth(as.Date("1970-01-31"))), "1970 Jan")
  expect_equal(format(yearmonth(as.Date("1970-04-30"))), "1970 Apr")
  expect_equal(format(yearmonth(as.Date("1970-12-31"))), "1970 Dec")
  
  # Leap year February
  expect_equal(format(yearmonth(as.Date("1972-02-29"))), "1972 Feb")
  expect_equal(format(yearmonth(as.Date("1972-03-01"))), "1972 Mar")
})

test_that("chronon_divmod between days and years handles edge cases", {
  # Year boundaries
  expect_equal(format(year(as.Date("1970-12-31"))), "1970")
  expect_equal(format(year(as.Date("1971-01-01"))), "1971")
  
  # Leap year transitions
  expect_equal(format(year(as.Date("1972-02-29"))), "1972")
  expect_equal(format(year(as.Date("1972-12-31"))), "1972")
  expect_equal(format(year(as.Date("1973-01-01"))), "1973")
  
  # Century transitions
  expect_equal(format(year(as.Date("1999-12-31"))), "1999")
  expect_equal(format(year(as.Date("2000-01-01"))), "2000")
  expect_equal(format(year(as.Date("2000-12-31"))), "2000")
  expect_equal(format(year(as.Date("2001-01-01"))), "2001")
})

test_that("reverse conversion from year to Date works correctly", {
  # Converting back to Date should give Jan 1
  years_seq <- year(0:500)
  dates_back <- as.Date(years_seq)
  
  # All should be January 1st
  days <- format(dates_back, "%m-%d")
  expect_true(all(days == "01-01"))
  
  # Years should match
  years_check <- as.integer(format(dates_back, "%Y"))
  expect_equal(years_check, 0:500)
})

test_that("chronon_cardinality handles months with variable days", {
  # This requires the `at` context parameter
  # Test that error is thrown when at is NULL
  expect_error(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(1L), at = NULL),
    "time context"
  )
  
  # Test that error is thrown for years without at
  expect_error(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$year(1L), at = NULL),
    "time context"
  )
})

test_that("chronon_cardinality(day, month) handles a negative-magnitude month chronon", {
  # Regression test: `y@n < 0` (e.g. from a descending `by = months(-1L)`
  # sequence step) used to pass a negative window size straight into
  # circsum(), which returns numeric(0) for size <= 0 and cascades to NA.
  # `at` is a block index in `y` units, so `at * y@n` recovers the correct
  # absolute month regardless of `y@n`'s sign.
  march_2020 <- (2020L - 1970L) * 12L + 3L - 1L # absolute month index, 0-based
  expect_equal(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(-1L), at = -march_2020),
    31 # March
  )
  expect_equal(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(-1L), at = -(march_2020 - 1L)),
    29 # February 2020 (leap year)
  )

  # Same absolute month, positive vs negative `y@n`, should agree
  expect_equal(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(1L), at = march_2020),
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(-1L), at = -march_2020)
  )
})

test_that("chronon_divmod(day, month) round-trips through multi-month `to` chronons", {
  # Regression test: a stray `res <- chronon_cardinality(...)` reassignment
  # (an accidental rebase regression, originally a discarded validation-only
  # call) corrupted `div` for any `to@n != 1`, e.g. `by = "2 months"`.
  expect_equal(
    format(seq(date("2020-01-01"), length.out = 6, by = "2 months")),
    c("2020-01-01", "2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01", "2020-11-01")
  )
})

test_that("chronon_divmod(day, month)'s `mod` accounts for whole months already elapsed in a wide window", {
  # Regression test: `mod` used to be `day + x_frac` unconditionally - the
  # offset into the *calendar* month containing `x`, which only coincides
  # with the offset into the `to@n`-month *window* when `to@n == 1`. Starting
  # mid-window (here, the 2nd month of a 2-month window) exposed the gap: the
  # whole first month of the window was silently dropped from `mod`, so every
  # subsequent step landed a full window early.
  expect_equal(
    format(seq(date("2020-02-15"), length.out = 4, by = "2 months")),
    c("2020-02-15", "2020-04-15", "2020-06-15", "2020-08-15")
  )
})

test_that("chronon_divmod(day, month)'s `div` is correct for a negative, multi-month `to`", {
  # Regression test: `div` used to be `fdiv(res, res_scale)` unconditionally,
  # which only agrees with chronon_cardinality()'s `at * n_months` window
  # anchor when `res_scale > 0`. For `res_scale < 0` and `abs(res_scale) > 1`
  # it picked a window running *backwards* from `res` instead of forwards
  # from `div * res_scale`, disagreeing with chronon_cardinality() (and thus
  # with clamping, which calls chronon_cardinality() using `div` as `at`).
  feb_1970 <- as.numeric(date("1970-02-01"))
  expect_equal(
    with(cal_gregorian, chronon_divmod(day(1L), month(-2L), feb_1970)),
    list(div = 0L, mod = 31)
  )
  # div/mod must always reconstruct `x` via chronon_cardinality()'s own
  # window convention: x = month_start_days(div * n) + mod.
  for (n in c(-13L, -7L, -2L, -1L, 1L, 2L, 7L, 13L)) {
    dm <- with(cal_gregorian, chronon_divmod(day(1L), month(n), feb_1970))
    expect_equal(
      with(cal_gregorian, chronon_divmod(month(n), day(1L), dm$div))$div + dm$mod,
      feb_1970,
      info = paste("n =", n)
    )
  }
})

test_that("chronon_cardinality/chronon_divmod(day, month) support month chronons >= 12", {
  # Previously hard-errored ("Month chronons >= 12 are not yet supported"):
  # circsum()'s per-window cost scaled with `abs(n_months)` and its leap-day
  # correction only ever accounted for a single February per window, both of
  # which broke down for a >= 12-month window. Replaced by a difference of
  # two O(1) `month_start_days()` calls, which has no such limit.

  # `at` is a block index in `month(13L)` units, so its window is the 13
  # calendar months [at * 13, at * 13 + 13). `at = 1` lands the window on
  # Feb 1971 - Feb 1972 inclusive, which contains *two* Februaries (one
  # non-leap, one leap - 1972 is a leap year), exercising the case the old
  # single-February assumption couldn't.
  expect_equal(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(13L), at = 1L),
    as.numeric(date("1972-03-01") - date("1971-02-01"))
  )
  # `at = 0`'s window (Jan 1970 - Jan 1971 inclusive) contains only one.
  expect_equal(
    chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(13L), at = 0L),
    as.numeric(date("1971-02-01") - date("1970-01-01"))
  )

  # Round-trips through seq().
  expect_equal(
    format(seq(date("1970-01-01"), length.out = 3, by = months(13L))),
    c("1970-01-01", "1971-02-01", "1972-03-01")
  )
})

test_that("is_leap_year helper function works correctly", {
  # Standard leap years (divisible by 4)
  expect_true(is_leap_year(1972))
  expect_true(is_leap_year(2024))
  
  # Non-leap years
  expect_false(is_leap_year(1970))
  expect_false(is_leap_year(2025))
  
  # Century years (not leap unless divisible by 400)
  expect_false(is_leap_year(1900))
  expect_true(is_leap_year(2000))
  expect_false(is_leap_year(2100))
  
  # Edge case: year 2400
  expect_true(is_leap_year(2400))
})

test_that("conversion handles dates before Unix epoch", {
  # Negative days since epoch
  expect_equal(format(year(as.Date("1969-12-31"))), "1969")
  expect_equal(format(year(as.Date("1969-01-01"))), "1969")
  expect_equal(format(year(as.Date("1900-01-01"))), "1900")
  
  expect_equal(format(yearmonth(as.Date("1969-12-31"))), "1969 Dec")
  expect_equal(format(yearmonth(as.Date("1969-06-15"))), "1969 Jun")
})

test_that("cyclical_labels_format for months work correctly", {
  # The method should return abbreviated month names
  labels <- cyclical_labels_format(cal_gregorian$month(1L), cal_gregorian$year(1L), 0:11, label = TRUE, abbreviate = TRUE)
  expect_equal(labels, month.abb)
})

test_that("time_unit methods return correct strings", {
  expect_equal(time_unit_full(cal_gregorian$year(1L)), "year{?/s}")
  expect_equal(time_unit_abbr(cal_gregorian$year(1L)), "Y")
  
  expect_equal(time_unit_full(cal_gregorian$month(1L)), "month{?/s}")
  expect_equal(time_unit_abbr(cal_gregorian$month(1L)), "M")
  
  expect_equal(time_unit_full(cal_gregorian$day(1L)), "day{?/s}")
  expect_equal(time_unit_abbr(cal_gregorian$day(1L)), "D")
  
  expect_equal(time_unit_full(cal_gregorian$hour(1L)), "hour{?/s}")
  expect_equal(time_unit_abbr(cal_gregorian$hour(1L)), "h")
})

test_that("time_unit_plural produces correct singular and plural forms", {
  expect_equal(time_unit_plural(cal_gregorian$year(1L), 1L), "year")
  expect_equal(time_unit_plural(cal_gregorian$year(1L), 2L), "years")
  expect_equal(time_unit_plural(cal_gregorian$month(1L), 1L), "month")
  expect_equal(time_unit_plural(cal_gregorian$month(1L), 3L), "months")
  expect_equal(time_unit_plural(cal_gregorian$day(1L), 1L), "day")
  expect_equal(time_unit_plural(cal_gregorian$day(1L), 7L), "days")
})

test_that("formatting of fractional/continuous dates (discrete = FALSE) around March 31st", {
  t <- date(.POSIXct(1774949379), discrete = FALSE)
  expect_equal(format(t), "2026-03-31 39.6%")
})

test_that("chronon_divmod day->year is correct for sub-day times on Dec 31", {
  # Regression test: fractional day values on Dec 31 previously corrupted the
  # month-prime calculation (mp), causing year to roll over to the next year at
  # 14:24 instead of midnight.  All times on Dec 31 must return the correct year.

  # The old boundary: 14:24:00 on Dec 31 2007 would (wrongly) return 2008
  expect_equal(format(year("2007-12-31 14:23:59")), "2007")
  expect_equal(format(year("2007-12-31 14:24:00")), "2007")
  expect_equal(format(year("2007-12-31 14:24:01")), "2007")

  # Full day of Dec 31 in a non-leap year
  expect_equal(format(year("2007-12-31 00:00:00")), "2007")
  expect_equal(format(year("2007-12-31 23:59:59")), "2007")

  # Full day of Dec 31 in a leap year
  expect_equal(format(year("2008-12-31 14:23:59")), "2008")
  expect_equal(format(year("2008-12-31 14:24:00")), "2008")
  expect_equal(format(year("2008-12-31 14:24:01")), "2008")
  expect_equal(format(year("2008-12-31 23:59:59")), "2008")

  # Midnight tick into the new year must return the new year
  expect_equal(format(year("2008-01-01 00:00:00")), "2008")
  expect_equal(format(year("2025-01-01 00:00:00")), "2025")

  # Vector: every hour of 2007-12-31 should be 2007
  hours <- seq(
    as.POSIXct("2007-12-31 00:00:00", tz = "UTC"),
    as.POSIXct("2007-12-31 23:00:00", tz = "UTC"),
    by = "hour"
  )
  expect_true(all(format(year(hours)) == "2007"))
})

test_that("chronon_divmod day->month is correct for sub-day times on Dec 31", {
  # Companion regression test for the day->month path, which shares the same
  # fractional-day bug.

  expect_equal(format(yearmonth("2007-12-31 14:23:59")), "2007 Dec")
  expect_equal(format(yearmonth("2007-12-31 14:24:00")), "2007 Dec")
  expect_equal(format(yearmonth("2007-12-31 14:24:01")), "2007 Dec")
  expect_equal(format(yearmonth("2007-12-31 23:59:59")), "2007 Dec")

  # Same checks for a leap-year December
  expect_equal(format(yearmonth("2008-12-31 14:24:00")), "2008 Dec")
  expect_equal(format(yearmonth("2008-12-31 23:59:59")), "2008 Dec")

  # Midnight tick into January
  expect_equal(format(yearmonth("2008-01-01 00:00:00")), "2008 Jan")

  # Vector: every hour of 2007-12-31 should be 2007 Dec
  hours <- seq(
    as.POSIXct("2007-12-31 00:00:00", tz = "UTC"),
    as.POSIXct("2007-12-31 23:00:00", tz = "UTC"),
    by = "hour"
  )
  expect_true(all(format(yearmonth(hours)) == "2007 Dec"))
})
