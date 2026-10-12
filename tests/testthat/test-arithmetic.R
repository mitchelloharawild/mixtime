# Tests for time arithmetic: addition, subtraction, and duration operations.
#
# Cross-chronon arithmetic (e.g. date + months, date + years) uses calendar-field
# arithmetic: the duration's own chronon (e.g. month) is advanced by its magnitude, and the
# finer remainder (e.g. day-of-month) is preserved exactly, clamped to the shifted period's
# valid range if it would otherwise overflow (e.g. 2020-01-31 + months(1) -> 2020-02-29,
# not 2020-03-02).

# ---------------------------------------------------------------------------
# date + duration (same chronon: days)
# ---------------------------------------------------------------------------

test_that("date() + days() advances by the correct number of days", {
  d <- date(as.Date("2020-01-02"))

  expect_equal(format(d + days(1L)), "2020-01-03")
  expect_equal(format(d + days(7L)), "2020-01-09")
  expect_equal(format(d + days(30L)), "2020-02-01")
  expect_equal(format(d + days(0L)), "2020-01-02")
})

test_that("date() - days() retreats by the correct number of days", {
  d <- date(as.Date("2020-03-01"))

  expect_equal(format(d - days(1L)), "2020-02-29") # 2020 is a leap year
  expect_equal(format(d - days(29L)), "2020-02-01")
})

# ---------------------------------------------------------------------------
# date + duration (cross-chronon: days <-> months)
#
# The day-of-month is preserved exactly, clamped to the target month's length
# if it would otherwise overflow. Subtraction clamps against the earlier
# month's length in the same way.
# ---------------------------------------------------------------------------

test_that("date() + months(1L) advances to the same day next month", {
  expect_equal(format(date(as.Date("2020-01-02")) + months(1L)), "2020-02-02")
  expect_equal(format(date(as.Date("2019-12-15")) + months(1L)), "2020-01-15")
})

test_that("date() + months(n) advances by n calendar months, preserving day-of-month", {
  expect_equal(format(date(as.Date("2020-01-02")) + months(3L)), "2020-04-02")
  expect_equal(format(date(as.Date("2019-12-15")) + months(2L)), "2020-02-15")
})

test_that("date() + months() clamps an overflowing day-of-month to the target month's length", {
  # January 31 has no equivalent in February -> clamps to the last day
  expect_equal(format(date(as.Date("2020-01-31")) + months(1L)), "2020-02-29") # 2020 is a leap year
  expect_equal(format(date(as.Date("2021-01-31")) + months(1L)), "2021-02-28")
  # Clamping applies per intervening month, not just the destination
  expect_equal(format(date(as.Date("2020-01-31")) + months(2L)), "2020-03-31")
})

test_that("months() + date() is commutative", {
  expect_equal(format(months(1L) + date(as.Date("2020-01-02"))), "2020-02-02")
})

test_that("date() - months() retreats to the same day the given number of months earlier", {
  d <- date(as.Date("2020-03-15"))

  expect_equal(format(d - months(1L)), "2020-02-15")
  expect_equal(format(d - months(3L)), "2019-12-15")
})

test_that("date() - months() clamps an overflowing day-of-month to the earlier month's length", {
  # March 31 has no equivalent in February -> clamps to the last day
  expect_equal(format(date(as.Date("2020-03-31")) - months(1L)), "2020-02-29") # 2020 is a leap year
  expect_equal(format(date(as.Date("2021-03-31")) - months(1L)), "2021-02-28")
})

# ---------------------------------------------------------------------------
# date + duration (cross-chronon: days <-> years)
#
# days <-> years also has a direct (context-dependent) chronon_divmod() method - a flat
# day-of-year encoding, with no notion of "month" - but calendar-field arithmetic
# deliberately does *not* use it: it decomposes along the *longest* available path
# (day -> month -> year, not the shortcut edge) so that day-of-month is the component
# that gets clamped, exactly as it is for days <-> months. This is what keeps
# `+ years(1L)` and `+ months(12L)` in agreement.
# ---------------------------------------------------------------------------

test_that("date() + years(1L) advances to the same day next year", {
  expect_equal(format(date(as.Date("2020-01-02")) + years(1L)), "2021-01-02")
  expect_equal(format(date(as.Date("2021-01-02")) + years(1L)), "2022-01-02")
})

test_that("date() + years(1L) clamps a leap day to the last day of February in a common year", {
  expect_equal(format(date(as.Date("2024-02-29")) + years(1L)), "2025-02-28")
  # Agrees with the equivalent months() shift
  expect_equal(
    format(date(as.Date("2024-02-29")) + years(1L)),
    format(date(as.Date("2024-02-29")) + months(12L))
  )
})

test_that("date() - years(1L) retreats to the same day the given number of years earlier", {
  expect_equal(format(date(as.Date("2020-01-02")) - years(1L)), "2019-01-02")
  expect_equal(format(date(as.Date("2021-01-02")) - years(1L)), "2020-01-02")
})

# ---------------------------------------------------------------------------
# yearmonth + duration (same chronon: months) -- exact by definition
# ---------------------------------------------------------------------------

test_that("yearmonth() + months() advances by the correct number of months", {
  ym <- yearmonth(as.Date("2020-01-01"))

  expect_equal(format(ym + months(1L)), "2020 Feb")
  expect_equal(format(ym + months(12L)), "2021 Jan")
  expect_equal(format(ym + months(0L)), "2020 Jan")
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
  expect_equal(as.integer(result), 31L) # January has 31 days
})

test_that("date() - date() handles leap-year February correctly", {
  d1 <- date(as.Date("2020-03-01"))
  d2 <- date(as.Date("2020-02-01"))

  expect_equal(as.integer(d1 - d2), 29L) # 2020 is a leap year
})

test_that("date() - date() is zero for identical times", {
  d <- date(as.Date("2023-07-04"))
  expect_equal(as.integer(d - d), 0L)
})

test_that("yearmonth() - yearmonth() returns the difference in months", {
  ym1 <- yearmonth(as.Date("2021-04-01"))
  ym2 <- yearmonth(as.Date("2020-01-01"))

  expect_equal(as.integer(ym1 - ym2), 15L) # 12 + 3
})

test_that("yearmonth() - yearmonth() is negative when ym1 < ym2", {
  ym1 <- yearmonth(as.Date("2020-01-01"))
  ym2 <- yearmonth(as.Date("2020-04-01"))

  expect_equal(as.integer(ym1 - ym2), -3L)
})

# ---------------------------------------------------------------------------
# diff() on unwrapped time objects (uses the class's own `-` method)
# ---------------------------------------------------------------------------

test_that("diff() on linear times returns durations with the chronon kept", {
  x <- vecvec::unvecvec(
    yearmonth(as.Date(c("2000-01-01", "2001-01-01", "2003-01-01")))
  )
  d <- diff(x)

  expect_true(S7::S7_inherits(d, mt_duration))
  expect_equal(d@chronon, x@chronon)
  expect_equal(d, x[-1] - x[-length(x)])
  expect_equal(as.integer(d), c(12L, 24L))
  expect_equal(as.integer(max(d)), 24L)
})

test_that("diff() respects lag and differences", {
  x <- vecvec::unvecvec(
    yearmonth(as.Date(c("2000-01-01", "2001-01-01", "2003-01-01")))
  )

  expect_equal(as.integer(diff(x, lag = 2)), 36L)
  expect_equal(as.integer(diff(x, differences = 2)), 12L)
})

test_that("diff() on a length-1 time returns a zero-length duration", {
  x <- vecvec::unvecvec(yearmonth(as.Date("2000-01-01")))
  d <- diff(x)

  expect_length(d, 0L)
  expect_true(S7::S7_inherits(d, mt_duration))
  expect_equal(d@chronon, x@chronon)
})

# ---------------------------------------------------------------------------
# date +/- integer (direct numeric shift along the chronon)
# ---------------------------------------------------------------------------

test_that("date() + integer shifts by raw day units", {
  d <- date(as.Date("2020-01-01"))

  expect_equal(format(d + 1L), "2020-01-02")
  expect_equal(format(d + 31L), "2020-02-01")
})

test_that("yearmonth() + integer shifts by raw month units", {
  ym <- yearmonth(as.Date("2020-01-01"))

  expect_equal(format(ym + 1L), "2020 Feb")
  expect_equal(format(ym + 12L), "2021 Jan")
})

# ---------------------------------------------------------------------------
# Duration arithmetic (duration +/- duration, duration +/- integer)
# ---------------------------------------------------------------------------

test_that("duration + duration with the same chronon sums correctly", {
  expect_equal(as.integer(months(3L) + months(2L)), 5L)
  expect_equal(as.integer(days(10L) + days(5L)), 15L)
  expect_equal(as.integer(years(1L) + years(2L)), 3L)
})

test_that("duration - duration with the same chronon differences correctly", {
  expect_equal(as.integer(months(5L) - months(2L)), 3L)
  expect_equal(as.integer(days(10L) - days(3L)), 7L)
})

test_that("duration + integer shifts the duration magnitude", {
  expect_equal(as.integer(months(3L) + 2L), 5L)
  expect_equal(as.integer(days(7L) - 3L), 4L)
})

test_that("integer + duration shifts the duration magnitude", {
  expect_equal(as.integer(2L + months(3L)), 5L)
})

test_that("duration * numeric scales the duration magnitude", {
  expect_equal(as.integer(days(3L) * 3), 9L)
  expect_equal(as.integer(months(2L) * 6), 12L)
  expect_equal(as.integer(days(5L) * 0), 0L)
  expect_equal(as.integer(days(5L) * 1), 5L)
  expect_equal(as.integer(days(5L) * -1), -5L)
})

test_that("numeric * duration is commutative", {
  expect_equal(as.integer(3 * days(3L)), 9L)
  expect_equal(as.integer(0.5 * days(6L)), 3L)
})

test_that("duration / numeric scales the duration magnitude", {
  expect_equal(as.integer(days(6L) / 2), 3L)
  expect_equal(as.integer(months(9L) / 3), 3L)
  expect_equal(as.double(days(5L) / 2), 2.5)
})

test_that("duration / duration returns a dimensionless ratio", {
  expect_equal(as.double(days(6L) / days(2L)), 3)
  expect_equal(as.double(days(6L) / days(6L)), 1)
  expect_equal(as.double(months(4L) / months(2L)), 2)
})

test_that("duration / duration converts to a common chronon before dividing (same calendar)", {
  # months and years share cal_gregorian; years(1L) == 12 months
  expect_equal(as.double(years(2L) / months(1L)), 24)
  expect_equal(as.double(months(6L) / years(1L)), 0.5)
})

test_that("duration / duration converts to a common chronon before dividing (cross-calendar)", {
  # weeks (cal_isoweek) and days (cal_gregorian) share a common day chronon
  expect_equal(as.double(weeks(1L) / days(1L)), 7)
  expect_equal(as.double(days(14L) / weeks(1L)), 2)
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

# ---------------------------------------------------------------------------
# Timezone-aware time - date subtraction (issue #97)
# ---------------------------------------------------------------------------

test_that("datetime() - datetime() of a date's start uses the date's timezone for its day boundary", {
  # 2026-06-16 09:30:00 AEST = 2026-06-15 23:30:00 UTC = 2026-06-16 01:30:00 CEST
  t <- datetime("2026-06-16 09:30:00", tz = "Australia/Melbourne")
  time_of_day <- function(tz) {
    x <- datetime(t, tz = tz)
    x - time_floor(x, cal_gregorian$day(1L))
  }

  # Melbourne date: 2026-06-16 AEST; time-of-day in Melbourne = 9h30m = 34200s
  expect_equal(as.double(time_of_day("Australia/Melbourne")), 34200)

  # Berlin date: 2026-06-16 CEST; time-of-day in Berlin = 1h30m = 5400s
  expect_equal(as.double(time_of_day("Europe/Berlin")), 5400)

  # UTC date: 2026-06-15 UTC; time-of-day in UTC = 23h30m = 84600s
  expect_equal(as.double(time_of_day("UTC")), 84600)
})

test_that("subtracting discrete time points of different chronons errors", {
  # Which day of 2027 would be measured from is indeterminate
  expect_error(year(2027L) - date("2026-08-21"), "indeterminate")
  expect_error(date("2026-08-21") - year(2027L), "indeterminate")
  expect_error(yearmonth("2027-01") - date("2026-08-21"), "indeterminate")
  # Discrete times in different time zones are different chronons
  t <- datetime("2026-06-16 09:30:00", tz = "Australia/Melbourne")
  expect_error(t - date(t, tz = "UTC", discrete = TRUE), "indeterminate")
  expect_error(t - datetime(t, tz = "UTC"), "indeterminate")
})

test_that("continuous time points of different chronons can be subtracted", {
  expect_equal(
    as.double(year(2027.5) - date("2026-08-21", discrete = FALSE)),
    315.5
  )
})

test_that("datetime() with a timezone can be shifted by durations with a regular cardinality", {
  d <- datetime("2026-06-03 02:30:00", tz = "Australia/Melbourne")

  expect_equal(format(d + hours(1L)), "2026-06-03 03:30:00 AEST")
  expect_equal(format(d + days(1L)), "2026-06-04 02:30:00 AEST")
  expect_equal(format(d - hours(1L)), "2026-06-03 01:30:00 AEST")
  expect_equal(format(hours(1L) + d), "2026-06-03 03:30:00 AEST")
})

# Melbourne falls back from 03:00 AEDT to 02:00 AEST on 2025-04-06, and springs
# forward from 02:00 AEST to 03:00 AEDT on 2025-10-05.
mel_utc <- function(x) {
  datetime(as.POSIXct(as.POSIXct(x, tz = "UTC"), tz = "Australia/Melbourne"))
}
real_seconds <- function(x, y) {
  as.numeric(as.POSIXct(y)) - as.numeric(as.POSIXct(x))
}

test_that("units shorter than a day add real time across the end of daylight saving", {
  # 02:30 AEDT, the first of the two 02:30s
  x <- mel_utc("2025-04-05 15:30:00")
  expect_equal(format(x), "2025-04-06 02:30:00 AEDT")

  expect_equal(format(x + hours(1L)), "2025-04-06 02:30:00 AEST")
  expect_equal(real_seconds(x, x + hours(1L)), 3600)
  expect_equal(format(x + hours(1L) - hours(1L)), format(x))

  y <- datetime("2025-04-06 01:30:00", tz = "Australia/Melbourne")
  expect_equal(format(y + hours(2L)), "2025-04-06 02:30:00 AEST")
  expect_equal(real_seconds(y, y + hours(2L)), 7200)
  expect_equal(format(y + minutes(90L)), "2025-04-06 02:00:00 AEST")

  # 24 hours is real time; a day keeps the clock time
  expect_equal(format(x + hours(24L)), "2025-04-07 01:30:00 AEST")
  expect_equal(real_seconds(x, x + hours(24L)), 86400)
  expect_equal(
    format(x + duration(1L, cal_gregorian$hour(24L))),
    "2025-04-07 01:30:00 AEST"
  )
  expect_equal(format(x + days(1L)), "2025-04-07 02:30:00 AEST")
  expect_equal(real_seconds(x, x + days(1L)), 90000)
})

test_that("units shorter than a day add real time across the start of daylight saving", {
  x <- datetime("2025-10-05 01:30:00", tz = "Australia/Melbourne")
  expect_equal(format(x + hours(1L)), "2025-10-05 03:30:00 AEDT")
  expect_equal(real_seconds(x, x + hours(1L)), 3600)
  expect_equal(
    format(datetime("2025-10-05 03:30:00", tz = "Australia/Melbourne") - hours(1L)),
    "2025-10-05 01:30:00 AEST"
  )
  expect_equal(format(x + hours(24L)), "2025-10-06 02:30:00 AEDT")
})

test_that("units of a day or longer keep the clock time across daylight saving", {
  # A local time that doesn't exist is clamped back by the size of the gap,
  # keeping its minutes, like other invalid time points
  x <- datetime("2025-10-04 02:30:00", tz = "Australia/Melbourne")
  expect_equal(format(x + days(1L)), "2025-10-05 01:30:00 AEST")
  expect_equal(format(x + days(2L)), "2025-10-06 02:30:00 AEDT")
  expect_equal(
    format(datetime("2025-10-06 02:30:00", tz = "Australia/Melbourne") - days(1L)),
    "2025-10-05 01:30:00 AEST"
  )

  # A local time that occurs twice resolves to its first occurrence
  x <- datetime("2025-04-05 02:30:00", tz = "Australia/Melbourne")
  expect_equal(format(x + days(1L)), "2025-04-06 02:30:00 AEDT")
  expect_equal(format(x + weeks(1L)), "2025-04-12 02:30:00 AEST")
})

# ---------------------------------------------------------------------------
# time_shift(): resolving invalid time points with `on_invalid`
# ---------------------------------------------------------------------------

test_that("time_shift() defaults to clamping, matching `+`", {
  x <- datetime("2021-01-30 09:30:00")
  expect_equal(format(time_shift(x, months(1L))), "2021-02-28 09:30:00")
  expect_equal(format(time_shift(x, months(1L))), format(x + months(1L)))
})

test_that("time_shift() on_invalid = 'previous' uses the last chronon of the target granule", {
  x <- datetime("2021-01-30 09:30:00")
  expect_equal(
    format(time_shift(x, months(1L), on_invalid = "previous")),
    "2021-02-28 23:59:59"
  )
  # Valid time points are unaffected
  expect_equal(
    format(time_shift(
      datetime(c("2021-01-15 09:30:00", "2021-01-31 12:00:00")),
      months(1L),
      on_invalid = "previous"
    )),
    c("2021-02-15 09:30:00", "2021-02-28 23:59:59")
  )
  # With day chronons, previous and clamp agree
  expect_equal(
    format(time_shift(date("2024-02-29"), years(1L), on_invalid = "previous")),
    "2025-02-28"
  )
})

test_that("time_shift() on_invalid = 'previous' uses the end of the last chronon for continuous time", {
  # The end of the last second (and day) is the start of the next granule
  expect_equal(
    format(time_shift(
      datetime("2021-01-30 09:30:00", discrete = FALSE),
      months(1L),
      on_invalid = "previous"
    )),
    format(datetime("2021-03-01 00:00:00", discrete = FALSE))
  )
  expect_equal(
    format(time_shift(
      date("2021-01-31", discrete = FALSE),
      months(1L),
      on_invalid = "previous"
    )),
    format(date("2021-03-01", discrete = FALSE))
  )
})

test_that("time_shift() on_invalid = 'next' uses the start of the following granule", {
  expect_equal(
    format(time_shift(
      datetime(c("2021-01-15 09:30:00", "2021-01-30 09:30:00")),
      months(1L),
      on_invalid = "next"
    )),
    c("2021-02-15 09:30:00", "2021-03-01 00:00:00")
  )
  # Unlike "clamp" and "previous", day chronons move to the next month
  expect_equal(
    format(time_shift(date("1955-01-31"), months(1L), on_invalid = "next")),
    "1955-03-01"
  )
  expect_equal(
    format(time_shift(date("2024-02-29"), years(1L), on_invalid = "next")),
    "2025-03-01"
  )
  # Continuous time: "next" is the same instant as "previous"
  xc <- datetime("2021-01-30 09:30:00", discrete = FALSE)
  expect_equal(
    format(time_shift(xc, months(1L), on_invalid = "next")),
    format(time_shift(xc, months(1L), on_invalid = "previous"))
  )
})

test_that("time_shift() on_invalid 'previous' and 'next' preserve order, 'clamp' doesn't", {
  x <- datetime(c("1955-01-29 23:00:00", "1955-01-31 08:00:00", "1955-02-01 00:00:00"))
  expect_false(!is.unsorted(as.numeric(time_shift(x, months(1L), on_invalid = "clamp"))))
  expect_false(!is.unsorted(as.numeric(time_shift(x, months(1L), on_invalid = "overflow"))))
  expect_false(is.unsorted(as.numeric(time_shift(x, months(1L), on_invalid = "previous"))))
  expect_false(is.unsorted(as.numeric(time_shift(x, months(1L), on_invalid = "next"))))
})

test_that("time_shift() on_invalid = 'overflow' carries into the next granule", {
  expect_equal(
    format(time_shift(
      datetime("2021-01-30 09:30:00"),
      months(1L),
      on_invalid = "overflow"
    )),
    "2021-03-02 09:30:00"
  )
  expect_equal(
    format(time_shift(date("2021-03-31"), months(-1L), on_invalid = "overflow")),
    "2021-03-03"
  )
})

test_that("time_shift() on_invalid = 'reject' gives NA with a warning", {
  expect_warning(
    res <- time_shift(
      date(c("2020-01-31", "2020-01-15")),
      months(1L),
      on_invalid = "reject"
    ),
    "1 time point did not exist"
  )
  expect_equal(is.na(res), c(TRUE, FALSE))
  expect_equal(format(res[2]), "2020-02-15")
  # No warning when every time point is valid
  expect_no_warning(time_shift(date("2020-01-15"), months(1L), on_invalid = "reject"))
})

test_that("time_shift() on_invalid = 'error' errors on invalid time points", {
  expect_error(
    time_shift(
      date(c("2020-01-15", "2020-01-31")),
      months(1L),
      on_invalid = "error"
    ),
    "1 time point did not exist in the target granule, at position 2"
  )
  expect_equal(
    format(time_shift(date("2020-01-15"), months(1L), on_invalid = "error")),
    "2020-02-15"
  )
  x <- datetime("2025-10-04 02:30:00", tz = "Australia/Melbourne")
  expect_error(time_shift(x, days(1L), on_invalid = "error"), "did not exist")
  # "nearest" was renamed to "previous"
  expect_error(time_shift(x, days(1L), on_invalid = "nearest"))
})

test_that("the mixtime.on_invalid option sets the default strategy", {
  x <- datetime("2021-01-30 09:30:00")
  rlang::local_options(mixtime.on_invalid = "next")
  expect_equal(format(time_shift(x, months(1L))), "2021-03-01 00:00:00")
  expect_equal(format(x + months(1L)), "2021-03-01 00:00:00")
  expect_equal(format(x - months(-1L)), "2021-03-01 00:00:00")
  # An explicit argument takes precedence
  expect_equal(
    format(time_shift(x, months(1L), on_invalid = "clamp")),
    "2021-02-28 09:30:00"
  )

  rlang::local_options(mixtime.on_invalid = "error")
  expect_error(x + months(1L), "did not exist")

  rlang::local_options(mixtime.on_invalid = "bad")
  expect_error(x + months(1L), "mixtime.on_invalid")
  expect_error(time_shift(x, months(1L)), "mixtime.on_invalid")
})

test_that("shifting a discrete time by a duration it doesn't nest within is indeterminate", {
  # A week can straddle two Gregorian years, months, or days
  expect_error(yearweek("2020 W53") + years(1L), class = "mixtime_error_indeterminate")
  expect_error(yearweek("2021 W05") + months(1L), class = "mixtime_error_indeterminate")
  expect_error(yearweek("2021 W05") + days(11L), class = "mixtime_error_indeterminate")
  expect_error(yearmonth("2021 Jan") + weeks(1L), class = "mixtime_error_indeterminate")
  expect_error(date("2021-01-31") + hours(23L), class = "mixtime_error_indeterminate")
  expect_error(
    time_shift(yearweek("2020 W53"), years(1L), on_invalid = "next"),
    class = "mixtime_error_indeterminate"
  )

  # A duration from another calendar says so
  expect_error(
    yearweek("2020 W53") + years(1L),
    "1 year is from `cal_gregorian`, while 1 week is from `cal_isoweek`"
  )
  expect_error(
    yearmonth("2021 Jan") + weeks(1L),
    "1 week is from `cal_isoweek`, while 1 month is from `cal_gregorian`"
  )
  expect_no_match(
    tryCatch(date("2021-01-31") + hours(23L), error = conditionMessage),
    "is from"
  )

  # A whole number of chronons is determinate
  expect_equal(format(yearweek("2021 W05") + days(14L)), "2021 W07")
  expect_equal(format(yearweek("2021 W05") - days(7L)), "2021 W04")
  expect_equal(format(date("2021-01-31") + hours(48L)), "2021-02-02")

  # As is a duration the chronon nests within
  expect_equal(
    format(yearweek("2021 W05") + duration(1L, cal_isoweek$year(1L))),
    "2022 W05"
  )

  # Continuous time is an instant, so shifts as before
  expect_no_error(yearweek("2021 W05", discrete = FALSE) + years(1L))
})

test_that("shifting multi-unit chronons checks the whole chronon", {
  x <- mixtime(as.POSIXct("2021-01-31 10:30", tz = "UTC"), cal_gregorian$minute(30L))
  expect_equal(format(x + months(1L)), "2021-02-28 10:30")
  expect_equal(format(x + hours(1L)), "2021-01-31 11:30")
  expect_error(x + minutes(15L), class = "mixtime_error_indeterminate")
})

test_that("time_shift() accepts character durations and validates `by`", {
  expect_equal(
    format(time_shift(date("2021-01-31"), "1 month", on_invalid = "previous")),
    "2021-02-28"
  )
  expect_error(time_shift(date("2021-01-31"), date("2021-01-31")), "duration")
  expect_error(time_shift(date("2021-01-31"), months(1L), on_invalid = "bad"))
})

test_that("time_shift() resolves local times skipped by daylight saving with on_invalid", {
  # Melbourne goes forward from 02:00 AEST to 03:00 AEDT on 2025-10-05
  tz <- "Australia/Melbourne"
  x <- datetime("2025-10-04 02:30:00", tz = tz)
  expect_equal(format(time_shift(x, days(1L))), "2025-10-05 01:30:00 AEST")
  expect_equal(
    format(time_shift(x, days(1L), on_invalid = "previous")),
    "2025-10-05 01:59:59 AEST"
  )
  expect_equal(
    format(time_shift(x, days(1L), on_invalid = "next")),
    "2025-10-05 03:00:00 AEDT"
  )
  expect_equal(
    format(time_shift(x, days(1L), on_invalid = "overflow")),
    "2025-10-05 03:30:00 AEDT"
  )
  expect_warning(
    res <- time_shift(x, days(1L), on_invalid = "reject"),
    "did not exist"
  )
  expect_true(is.na(res))

  # Continuous time: "previous" and "next" are the transition itself
  xc <- mixtime(as.POSIXct("2025-10-04 02:30", tz = tz), discrete = FALSE)
  expect_equal(
    as.numeric(time_shift(xc, days(1L), on_invalid = "previous")),
    as.numeric(as.POSIXct("2025-10-05 03:00", tz = tz))
  )
  expect_equal(
    as.numeric(time_shift(xc, days(1L), on_invalid = "next")),
    as.numeric(as.POSIXct("2025-10-05 03:00", tz = tz))
  )

  # Discrete hours: the hour skipped clamps to the hour before it
  h <- mixtime(as.POSIXct("2025-10-04 02:00", tz = tz), cal_gregorian$hour(1L, tz = tz))
  expect_equal(format(time_shift(h, days(1L))), "2025-10-05 01h AEST")
  expect_equal(
    format(time_shift(h, days(1L), on_invalid = "overflow")),
    "2025-10-05 03h AEDT"
  )

  # Every minute in the skipped hour gives the same durations
  for (m in c("02:00", "02:10", "02:59")) {
    y <- datetime(paste0("2025-10-04 ", m, ":00"), tz = tz)
    steps <- c(y, y + days(1L), y + days(2L))
    expect_equal(diff(as.numeric(steps)) / 3600, c(23, 24), info = m)
  }

  # Samoa skipped all of 2011-12-30
  apia <- "Pacific/Apia"
  d <- mixtime(as.POSIXct("2011-12-29 10:00", tz = apia))
  expect_equal(format(time_shift(d, days(1L))), "2011-12-29 10:00:00 -10")
  expect_equal(
    format(time_shift(d, days(1L), on_invalid = "overflow")),
    "2011-12-31 10:00:00 +14"
  )
})

test_that("multi-unit durations shift by their single unit, not epoch windows", {
  x <- date("2020-01-31")
  expect_equal(
    format(x + duration(1L, cal_gregorian$month(18L))),
    format(x + months(18L))
  )
  expect_equal(format(x + duration(1L, cal_gregorian$month(5L))), "2020-06-30")
  expect_equal(format(date("2020-07-31") - duration(1L, cal_gregorian$month(5L))), "2020-02-29")
  expect_equal(
    format(time_shift(x, duration(1L, cal_gregorian$month(5L)), on_invalid = "overflow")),
    "2020-07-01"
  )
})
