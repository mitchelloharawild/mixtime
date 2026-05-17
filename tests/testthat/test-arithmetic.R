# Tests for time arithmetic: addition, subtraction, and duration operations.
#
# Cross-chronon arithmetic (e.g. date + months, date + years) uses the
# cardinality of the duration's chronon *at the current time point*. This means
# the result is exact for same-chronon additions, and uses the starting
# period's length for cross-chronon additions, which may differ from the
# intuitive "same day next month / next year" semantics.

# ---------------------------------------------------------------------------
# date + duration (same chronon: days)
# ---------------------------------------------------------------------------

test_that("date() + days() advances by the correct number of days", {
  d <- date(as.Date("2020-01-02"))

  expect_equal(format(d + days(1L)),  "2020-01-03")
  expect_equal(format(d + days(7L)),  "2020-01-09")
  expect_equal(format(d + days(30L)), "2020-02-01")
  expect_equal(format(d + days(0L)),  "2020-01-02")
})

test_that("date() - days() retreats by the correct number of days", {
  d <- date(as.Date("2020-03-01"))

  expect_equal(format(d - days(1L)),  "2020-02-29")  # 2020 is a leap year
  expect_equal(format(d - days(29L)), "2020-02-01")
})

# ---------------------------------------------------------------------------
# date + duration (cross-chronon: days <-> months)
#
# The shift added is `n * days_in_current_month`. For n == 1 this matches the
# intuitive "same day next month" when the start is in a same-length month. For
# n > 1 the result can differ when intermediate months vary in length.
# Subtraction uses the current month's day count in the same way.
# ---------------------------------------------------------------------------

test_that("date() + months(1L) advances by the days in the current month", {
  # January has 31 days -> 2020-01-02 + 31 = 2020-02-02
  expect_equal(format(date(as.Date("2020-01-02")) + months(1L)), "2020-02-02")
  # December has 31 days -> 2019-12-15 + 31 = 2020-01-15
  expect_equal(format(date(as.Date("2019-12-15")) + months(1L)), "2020-01-15")
})

test_that("date() + months(n) uses the starting month's cardinality for all steps", {
  # January (31 days) * 3 = 93 days -> 2020-01-02 + 93 = 2020-04-04
  # (intuitive "3 months later" would be 2020-04-02, i.e. 31+29+31=91 days)
  expect_equal(format(date(as.Date("2020-01-02")) + months(3L)), "2020-04-04")

  # December (31) * 2 = 62 days -> 2019-12-15 + 62 = 2020-02-15
  # Works here because December and January both have 31 days
  expect_equal(format(date(as.Date("2019-12-15")) + months(2L)), "2020-02-15")
})

test_that("months() + date() is commutative", {
  expect_equal(format(months(1L) + date(as.Date("2020-01-02"))), "2020-02-02")
})

test_that("date() - months() subtracts the days in the current month", {
  d <- date(as.Date("2020-03-15"))

  # March has 31 days -> 2020-03-15 - 31 = 2020-02-13
  # (intuitive "same day last month" would be 2020-02-15, i.e. 29 days back)
  expect_equal(format(d - months(1L)), "2020-02-13")
  # March * 3 = 93 days back -> 2019-12-13
  expect_equal(format(d - months(3L)), "2019-12-13")
})

# ---------------------------------------------------------------------------
# date + duration (cross-chronon: days <-> years)
#
# The shift is `days_in_current_year`: 366 for a leap-year start, 365 for a
# common-year start. Subtraction applies the same rule using the current year.
# ---------------------------------------------------------------------------

test_that("date() + years(1L) advances by the days in the current year", {
  # 2020 is a leap year (366 days) -> 2020-01-02 + 366 = 2021-01-02
  expect_equal(format(date(as.Date("2020-01-02")) + years(1L)), "2021-01-02")
  # 2021 is a common year (365 days) -> 2021-01-02 + 365 = 2022-01-02
  expect_equal(format(date(as.Date("2021-01-02")) + years(1L)), "2022-01-02")
})

test_that("date() - years(1L) subtracts the days in the current year", {
  # 2020 is a leap year (366 days) -> 2020-01-02 - 366 = 2019-01-01
  expect_equal(format(date(as.Date("2020-01-02")) - years(1L)), "2019-01-01")
  # 2021 is a common year (365 days) -> 2021-01-02 - 365 = 2020-01-03
  expect_equal(format(date(as.Date("2021-01-02")) - years(1L)), "2020-01-03")
})

# ---------------------------------------------------------------------------
# yearmonth + duration (same chronon: months) -- exact by definition
# ---------------------------------------------------------------------------

test_that("yearmonth() + months() advances by the correct number of months", {
  ym <- yearmonth(as.Date("2020-01-01"))

  expect_equal(format(ym + months(1L)),  "2020 Feb")
  expect_equal(format(ym + months(12L)), "2021 Jan")
  expect_equal(format(ym + months(0L)),  "2020 Jan")
})

test_that("yearmonth() - months() retreats by the correct number of months", {
  ym <- yearmonth(as.Date("2020-03-01"))

  expect_equal(format(ym - months(1L)), "2020 Feb")
  expect_equal(format(ym - months(3L)), "2019 Dec")
})

# ---------------------------------------------------------------------------
# yearmonth + duration (cross-chronon: months <-> years) -- exact (12 months/yr)
# ---------------------------------------------------------------------------

test_that("yearmonth() + years() advances by 12 months per year", {
  ym <- yearmonth(as.Date("2020-06-01"))

  expect_equal(format(ym + years(1L)), "2021 Jun")
  expect_equal(format(ym + years(2L)), "2022 Jun")
})

test_that("yearmonth() - years() retreats by 12 months per year", {
  ym <- yearmonth(as.Date("2023-03-01"))

  expect_equal(format(ym - years(1L)), "2022 Mar")
})

# ---------------------------------------------------------------------------
# time - time => duration
# ---------------------------------------------------------------------------

test_that("date() - date() returns the difference in days", {
  d1 <- date(as.Date("2020-02-01"))
  d2 <- date(as.Date("2020-01-01"))

  result <- d1 - d2
  expect_s7_class(result, class_mixtime)
  expect_equal(as.integer(result), 31L)  # January has 31 days
})

test_that("date() - date() handles leap-year February correctly", {
  d1 <- date(as.Date("2020-03-01"))
  d2 <- date(as.Date("2020-02-01"))

  expect_equal(as.integer(d1 - d2), 29L)  # 2020 is a leap year
})

test_that("date() - date() is zero for identical times", {
  d <- date(as.Date("2023-07-04"))
  expect_equal(as.integer(d - d), 0L)
})

test_that("yearmonth() - yearmonth() returns the difference in months", {
  ym1 <- yearmonth(as.Date("2021-04-01"))
  ym2 <- yearmonth(as.Date("2020-01-01"))

  expect_equal(as.integer(ym1 - ym2), 15L)  # 12 + 3
})

test_that("yearmonth() - yearmonth() is negative when ym1 < ym2", {
  ym1 <- yearmonth(as.Date("2020-01-01"))
  ym2 <- yearmonth(as.Date("2020-04-01"))

  expect_equal(as.integer(ym1 - ym2), -3L)
})

# ---------------------------------------------------------------------------
# date +/- integer (direct numeric shift along the chronon)
# ---------------------------------------------------------------------------

test_that("date() + integer shifts by raw day units", {
  d <- date(as.Date("2020-01-01"))

  expect_equal(format(d + 1L),  "2020-01-02")
  expect_equal(format(d + 31L), "2020-02-01")
})

test_that("yearmonth() + integer shifts by raw month units", {
  ym <- yearmonth(as.Date("2020-01-01"))

  expect_equal(format(ym + 1L),  "2020 Feb")
  expect_equal(format(ym + 12L), "2021 Jan")
})

# ---------------------------------------------------------------------------
# Duration arithmetic (duration +/- duration, duration +/- integer)
# ---------------------------------------------------------------------------

test_that("duration + duration with the same chronon sums correctly", {
  expect_equal(as.integer(months(3L) + months(2L)), 5L)
  expect_equal(as.integer(days(10L)  + days(5L)),   15L)
  expect_equal(as.integer(years(1L)  + years(2L)),  3L)
})

test_that("duration - duration with the same chronon differences correctly", {
  expect_equal(as.integer(months(5L) - months(2L)), 3L)
  expect_equal(as.integer(days(10L)  - days(3L)),   7L)
})

test_that("duration + integer shifts the duration magnitude", {
  expect_equal(as.integer(months(3L) + 2L), 5L)
  expect_equal(as.integer(days(7L)   - 3L), 4L)
})

test_that("integer + duration shifts the duration magnitude", {
  expect_equal(as.integer(2L + months(3L)), 5L)
})

# ---------------------------------------------------------------------------
# Vectorised operations
# ---------------------------------------------------------------------------

test_that("vectorised date() + days() works element-wise", {
  d <- date(as.Date("2020-01-01") + 0:2)

  result <- d + days(1L)
  expect_equal(format(result), c("2020-01-02", "2020-01-03", "2020-01-04"))
})

test_that("vectorised date() + months() works element-wise", {
  d <- date(as.Date(c("2020-01-15", "2020-02-15", "2020-03-15")))

  # Jan=31, Feb=29 (2020 leap), Mar=31 -> each advances by its own month's length
  result <- d + months(1L)
  expect_equal(format(result), c("2020-02-15", "2020-03-15", "2020-04-15"))
})

test_that("vectorised date() - date() returns element-wise differences in days", {
  d1 <- date(as.Date("2020-12-31") + 0:2)
  d2 <- date(as.Date("2020-12-31"))

  result <- d1 - d2
  expect_equal(as.integer(result), 0:2)
})

# ---------------------------------------------------------------------------
# NA propagation
# ---------------------------------------------------------------------------

test_that("NA propagates through date() + months()", {
  d <- date(as.Date(c("2020-01-01", NA, "2020-03-01")))

  result <- d + months(1L)
  expect_equal(is.na(result), c(FALSE, TRUE, FALSE))
})

test_that("NA propagates through date() - date()", {
  d1 <- date(as.Date(c("2020-02-01", NA)))
  d2 <- date(as.Date("2020-01-01"))

  result <- d1 - d2
  expect_equal(is.na(result), c(FALSE, TRUE))
})

# ---------------------------------------------------------------------------
# Unsupported operations error gracefully
# ---------------------------------------------------------------------------

test_that("multiplying two time points errors", {
  d <- date(as.Date("2020-01-01"))
  expect_error(d * d)
})

test_that("adding two time points errors", {
  d <- date(as.Date("2020-01-01"))
  expect_error(d + d)
})
