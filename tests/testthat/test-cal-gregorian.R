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
  dates <- as.Date(0:150000, origin = "1970-01-01")
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
  expect_equal(years_check, 1970L + 0:500)
})

test_that("chronon_cardinality handles months with variable days", {
  # This requires the `at` context parameter
  # Test that error is thrown when at is NULL
  expect_error(
    chronon_cardinality(cal_gregorian$month(1L), cal_gregorian$day(1L), at = NULL),
    "time context"
  )
  
  # Test that error is thrown for years without at
  expect_error(
    chronon_cardinality(cal_gregorian$year(1L), cal_gregorian$day(1L), at = NULL),
    "time context"
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

test_that("cyclical_labels for months work correctly", {
  # The method should return abbreviated month names
  labels <- cyclical_labels(cal_gregorian$month(1L), cal_gregorian$year(1L), 0:11)
  expect_equal(labels, month.abb)
})

test_that("time_unit methods return correct strings", {
  expect_equal(time_unit_full(cal_gregorian$year(1L)), "year")
  expect_equal(time_unit_abbr(cal_gregorian$year(1L)), "Y")
  
  expect_equal(time_unit_full(cal_gregorian$month(1L)), "month")
  expect_equal(time_unit_abbr(cal_gregorian$month(1L)), "M")
  
  expect_equal(time_unit_full(cal_gregorian$day(1L)), "day")
  expect_equal(time_unit_abbr(cal_gregorian$day(1L)), "D")
  
  expect_equal(time_unit_full(cal_gregorian$hour(1L)), "hour")
  expect_equal(time_unit_abbr(cal_gregorian$hour(1L)), "h")
})