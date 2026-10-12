test_that("seq.mixtime works with linear time using integer by", {
  # yearmonth sequences
  result <- seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Jan")))
  expect_equal(format(result[[12]]), format(yearmonth("2020 Dec")))

  # yearquarter with length.out and integer by
  result <- seq(yearquarter("2020 Q1"), length.out = 5, by = 1L)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(yearquarter("2020 Q1")))
  expect_equal(format(result[[5]]), format(yearquarter("2021 Q1")))

  # date sequences
  result <- seq(date("2020-01-01"), date("2020-01-10"))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 10)
  expect_equal(format(result[[1]]), format(date("2020-01-01")))
  expect_equal(format(result[[10]]), format(date("2020-01-10")))
})

test_that("seq.mixtime works with linear time using string intervals", {
  # Month intervals
  result <- seq(date("2020-01-01"), date("2020-12-31"), by = "1 month")
  expect_s7_class(result, class_mixtime)
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(date("2020-01-01")))
  expect_equal(format(result[[12]]), format(date("2020-12-01")))

  # Year intervals
  result <- seq(yearmonth("2020 Jan"), yearmonth("2025 Jan"), by = "1 year")
  expect_s7_class(result, class_mixtime)
  expect_length(result, 6)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Jan")))
  expect_equal(format(result[[6]]), format(yearmonth("2025 Jan")))

  # Week intervals with length.out
  result <- seq(date("2020-01-01"), length.out = 10, by = "2 weeks")
  expect_s7_class(result, class_mixtime)
  expect_length(result, 10)
  expect_equal(format(result[[1]]), format(date("2020-01-01")))
  expect_equal(
    format(result[[10]]),
    format(date(as.Date("2020-01-01") + 2 * 7 * 9))
  )
})

test_that("seq.mixtime works with linear time using time unit objects", {
  # Month time units
  result <- seq(
    yearmonth("2020 Jan"),
    yearmonth("2020 Dec"),
    by = cal_gregorian$month(2L)
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 6)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Jan")))
  expect_equal(format(result[[6]]), format(yearmonth("2020 Nov")))

  # Year time units with length.out
  result <- seq(date("2020-01-01"), length.out = 5, by = cal_gregorian$year(1L))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(date("2020-01-01")))
  expect_equal(format(result[[5]]), format(date("2024-01-01")))

  # Day time units
  result <- seq(
    date("2020-01-01"),
    date("2020-01-31"),
    by = cal_gregorian$day(7L)
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(date("2020-01-01")))
  expect_equal(format(result[[5]]), format(date("2020-01-29")))
})

test_that("seq.mixtime works with cyclical time", {
  # month_of_year sequence (full cycle)
  result <- seq(month_of_year(0L), month_of_year(11L))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(month_of_year(0L)))
  expect_equal(format(result[[12]]), format(month_of_year(11L)))

  # month_of_year with wrap-around and time unit by
  # result <- seq(month_of_year(5L), month_of_year(3L), by = cal_gregorian$month(2L))
  # expect_s7_class(result, class_mixtime)
  # Should wrap around: Jun -> Aug -> Oct -> Dec -> Feb -> Apr
  # expect_length(result, 6)
  # expect_equal(format(result[[1]]), format(month_of_year(5L)))
  # expect_equal(format(result[[6]]), format(month_of_year(3L)))

  # day_of_week sequence with integer by
  result <- seq(day_of_week(0L), day_of_week(6L), by = 1L)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 7)
  expect_equal(format(result[[1]]), format(day_of_week(0L)))
  expect_equal(format(result[[7]]), format(day_of_week(6L)))
})

test_that("seq.mixtime handles length.out correctly", {
  # With linear time
  result <- seq(yearmonth("2020 Jan"), length.out = 24)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 24)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Jan")))
  expect_equal(format(result[[24]]), format(yearmonth("2021 Dec")))

  # With cyclical time
  result <- seq(month_of_year(0L), length.out = 15)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 15)
  expect_equal(format(result[[1]]), format(month_of_year(0L)))
})

test_that("seq.mixtime handles along.with correctly", {
  reference_vec <- 1:10

  result <- seq(yearmonth("2020 Jan"), along.with = reference_vec)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 10)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Jan")))
  expect_equal(format(result[[10]]), format(yearmonth("2020 Oct")))
})

test_that("seq.mixtime handles backward sequences", {
  # Linear time going backward
  result <- seq(yearmonth("2020 Dec"), yearmonth("2020 Jan"), by = -1L)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Dec")))
  expect_equal(format(result[[12]]), format(yearmonth("2020 Jan")))

  # With negative time units
  result <- seq(
    date("2020-12-31"),
    date("2020-12-01"),
    by = cal_gregorian$day(-5L)
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 7)
  expect_equal(format(result[[1]]), format(date("2020-12-31")))
  expect_equal(format(result[[7]]), format(date("2020-12-01")))
})

test_that("seq.mixtime handles edge cases", {
  # Single element sequence
  result <- seq(yearmonth("2020 Jan"), yearmonth("2020 Jan"))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 1)
  expect_equal(format(result[[1]]), format(yearmonth("2020 Jan")))

  # Length.out = 1
  result <- seq(yearmonth("2020 Jan"), length.out = 1)
  expect_s7_class(result, class_mixtime)
  expect_length(result, 1)

  # Empty along.with (though unusual)
  result <- seq(yearmonth("2020 Jan"), along.with = integer(0))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 0)
})

test_that("seq.mixtime works with Date objects in mixtime", {
  # Dates should work through mixtime
  result <- seq(mixtime(as.Date("2020-01-01")), mixtime(as.Date("2020-01-10")))
  expect_s7_class(result, class_mixtime)
  expect_length(result, 10)
})

test_that("seq.mixtime works with different by specifications", {
  # Integer by
  result1 <- seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"), by = 2L)
  expect_length(result1, 6)

  # String by
  result2 <- seq(date("2020-01-01"), date("2020-02-01"), by = "1 week")
  expect_s7_class(result2, class_mixtime)

  # Time unit by
  result3 <- seq(
    yearmonth("2020 Jan"),
    yearmonth("2020 Dec"),
    by = cal_gregorian$quarter(1L)
  )
  expect_s7_class(result3, class_mixtime)
  expect_length(result3, 4)
})


test_that("seq.mixtime with fractional linear time", {
  # Fractional yearmonth with day-based by
  result <- seq(
    yearmonth("2020 Jan", discrete = FALSE),
    yearmonth("2025 Jan"),
    by = "30 days"
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 61)
  expect_equal(
    format(result[[1]]),
    format(yearmonth("2020 Jan", discrete = FALSE))
  )
  # Last element should be close to but not exceed the end
  expect_match(format(result[[61]]), "2024 Dec ")

  # Fractional date with week-based by
  result <- seq(
    date("2020-01-01", discrete = FALSE),
    date("2020-02-01"),
    by = "1 week"
  )
  expect_s7_class(result, class_mixtime)
  expect_gt(length(result), 4) # More than 4 weeks
  expect_equal(
    format(result[[1]]),
    format(date("2020-01-01", discrete = FALSE))
  )

  # Fractional yearmonth with month-based by (should work cleanly)
  result <- seq(
    yearmonth("2020 Jan", discrete = FALSE),
    yearmonth("2020 Jun"),
    by = "1 month"
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 6)
  expect_equal(
    format(result[[1]]),
    format(yearmonth("2020 Jan", discrete = FALSE))
  )
  expect_equal(
    format(result[[6]]),
    format(yearmonth("2020 Jun", discrete = FALSE))
  )

  # Fractional yearquarter with day-based by
  result <- seq(
    yearquarter("2020 Q1", discrete = FALSE),
    length.out = 10,
    by = "15 days"
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 10)
  # Should show fractional percentages within quarters
  expect_equal(
    format(result[[1]]),
    format(yearquarter("2020 Q1", discrete = FALSE))
  )

  # Fractional with length.out
  result <- seq(
    yearmonth("2020 Jan", discrete = FALSE),
    length.out = 13,
    by = "25 days"
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 13)
  expect_equal(
    format(result[[13]]),
    # 2020 Oct, 83.9% through the month (day 27 of 31) - no string format
    # captures a mid-month fraction, so this stays numeric
    format(yearmonth(609 + 26 / 31, discrete = FALSE))
  )

  # Backward fractional sequence
  result <- seq(
    yearmonth("2020 Dec", discrete = FALSE),
    yearmonth("2020 Jan"),
    by = "-30 days"
  )
  expect_s7_class(result, class_mixtime)
  expect_gt(length(result), 10)
  expect_equal(
    format(result[[1]]),
    format(yearmonth("2020 Dec", discrete = FALSE))
  )
  expect_equal(
    format(result[[12]]),
    format(yearmonth(600 + 5 / 31, discrete = FALSE))
  )
})

test_that("seq.mixtime with duration by", {
  # Using a duration for by should work
  result <- seq(
    date("2020-01-01"),
    date("2020-01-10"),
    by = duration(2L, day(1L))
  )
  expect_s7_class(result, class_mixtime)
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(date("2020-01-01")))
  expect_equal(format(result[[5]]), format(date("2020-01-09")))
})
test_that("seq.mixtime by a coarser granule of the same unit works (day(7L) etc.)", {
  # chronon and by share the same time unit (day), just different `n` - not a calendar-field
  # shift, so no clamping is involved.
  result <- seq(
    date("2020-01-01"),
    date("2020-01-31"),
    by = cal_gregorian$day(7L)
  )
  expect_equal(
    format(result),
    c("2020-01-01", "2020-01-08", "2020-01-15", "2020-01-22", "2020-01-29")
  )
})

test_that("seq.mixtime by = '1 month' clamps day-of-month but preserves time-of-day", {
  # Regression test: clamping used to flatten day-of-month and time-of-day into one
  # remainder, so overflowing into a shorter month also destroyed the time-of-day.
  result <- suppressWarnings(seq(
    datetime("2026-01-31 14:30:00"),
    length.out = 2,
    by = "1 month"
  ))
  expect_equal(format(result[[2]]), "2026-02-28 14:30:00")
})

test_that("seq.mixtime on_invalid = 'overflow' lets the day-of-month overflow into the next month", {
  result <- seq(
    date("2020-01-31"),
    length.out = 3,
    by = "1 month",
    on_invalid = "overflow"
  )
  expect_equal(format(result), c("2020-01-31", "2020-03-02", "2020-03-31"))
})

test_that("seq.mixtime on_invalid = 'previous' uses the last chronon of the cycle", {
  result <- seq(
    datetime("2021-01-31 09:30:00"),
    length.out = 3,
    by = "1 month",
    on_invalid = "previous"
  )
  expect_equal(
    format(result),
    c("2021-01-31 09:30:00", "2021-02-28 23:59:59", "2021-03-31 09:30:00")
  )
})

test_that("seq.mixtime on_invalid = 'next' uses the start of the following cycle", {
  result <- seq(
    date("2020-01-31"),
    length.out = 3,
    by = "1 month",
    on_invalid = "next"
  )
  expect_equal(format(result), c("2020-01-31", "2020-03-01", "2020-03-31"))
})

test_that("seq.mixtime on_invalid = 'error' errors on invalid time points", {
  expect_error(
    seq(date("2020-01-31"), length.out = 3, by = "1 month", on_invalid = "error"),
    "did not exist"
  )
})

test_that("seq.mixtime uses the mixtime.on_invalid option without warning", {
  rlang::local_options(mixtime.on_invalid = "next")
  expect_no_warning(
    result <- seq(date("2020-01-31"), length.out = 3, by = "1 month")
  )
  expect_equal(format(result), c("2020-01-31", "2020-03-01", "2020-03-31"))

  # Setting the option to "clamp" also silences the warning
  rlang::local_options(mixtime.on_invalid = "clamp")
  expect_no_warning(seq(date("2020-01-31"), length.out = 3, by = "1 month"))

  rlang::local_options(mixtime.on_invalid = "bad")
  expect_error(
    seq(date("2020-01-31"), length.out = 3, by = "1 month"),
    "mixtime.on_invalid"
  )
})

test_that("seq.mixtime on_invalid = 'reject' gives NA with a warning", {
  expect_warning(
    result <- seq(
      date("2020-01-31"),
      length.out = 3,
      by = "1 month",
      on_invalid = "reject"
    ),
    "1 time point did not exist"
  )
  expect_equal(is.na(result), c(FALSE, TRUE, FALSE))
  expect_equal(format(result[c(1, 3)]), c("2020-01-31", "2020-03-31"))
})

test_that("seq.mixtime messages describe the granules involved", {
  # Granules are S7 scalars, so their size comes from @n rather than vec_data()
  expect_error(
    seq(date("2020-01-01"), yearmonth("2020 Jun")),
    "1 day"
  )
  expect_warning(
    seq(date("2020-01-31"), by = months(1L), length.out = 3L),
    "past the end of a shorter month"
  )
})

test_that("seq.mixtime with negative-magnitude `by` clamps and steps backwards", {
  expect_warning(
    result <- seq(date("2020-03-31"), by = "-1 month", length.out = 3),
    "past the end of a shorter month"
  )
  expect_equal(format(result), c("2020-03-31", "2020-02-29", "2020-01-31"))

  result2 <- suppressWarnings(seq(
    date("2020-03-31"),
    by = months(-1L),
    length.out = 3
  ))
  expect_equal(format(result2), format(result))
})

# Melbourne falls back from 03:00 AEDT to 02:00 AEST on 2025-04-06, and springs
# forward from 02:00 AEST to 03:00 AEDT on 2025-10-05.

test_that("seq.mixtime by day keeps the local clock time across daylight saving", {
  tz <- "Australia/Melbourne"
  result <- seq(
    datetime("2025-04-05 10:00:00", tz = tz),
    by = cal_gregorian$day(1L),
    length.out = 3
  )
  expect_equal(
    format(result),
    c(
      "2025-04-05 10:00:00 AEDT",
      "2025-04-06 10:00:00 AEST",
      "2025-04-07 10:00:00 AEST"
    )
  )

  # Each day is counted from the start. A local time skipped when daylight
  # saving starts is clamped back by the size of the gap (with a warning, as
  # for other invalid time points), and returns to its clock time the
  # following day.
  expect_warning(
    result <- seq(
      datetime("2025-10-04 02:30:00", tz = tz),
      by = "1 day",
      length.out = 3
    ),
    "skipped"
  )
  expect_equal(
    format(result),
    c(
      "2025-10-04 02:30:00 AEST",
      "2025-10-05 01:30:00 AEST",
      "2025-10-06 02:30:00 AEDT"
    )
  )
  expect_equal(diff(as.numeric(result)) / 3600, c(23, 24))

  # The other `on_invalid` options, without a warning
  x <- datetime("2025-10-04 02:30:00", tz = tz)
  expect_no_warning(
    result_overflow <- seq(x, by = "1 day", length.out = 3, on_invalid = "overflow")
  )
  expect_equal(format(result_overflow)[2], "2025-10-05 03:30:00 AEDT")
  expect_equal(
    format(seq(x, by = "1 day", length.out = 3, on_invalid = "previous"))[2],
    "2025-10-05 01:59:59 AEST"
  )
  expect_equal(
    format(seq(x, by = "1 day", length.out = 3, on_invalid = "next"))[2],
    "2025-10-05 03:00:00 AEDT"
  )
  expect_warning(
    result_reject <- seq(x, by = "1 day", length.out = 3, on_invalid = "reject"),
    "did not exist"
  )
  expect_equal(is.na(result_reject), c(FALSE, TRUE, FALSE))

  # A time moved forward past `to` is left out
  to <- datetime("2025-10-05 03:00:00", tz = tz)
  expect_equal(
    format(suppressWarnings(seq(x, to, by = "1 day"))),
    c("2025-10-04 02:30:00 AEST", "2025-10-05 01:30:00 AEST")
  )
  expect_equal(
    format(seq(x, to, by = "1 day", on_invalid = "overflow")),
    "2025-10-04 02:30:00 AEST"
  )
  # Right aligned to `to`
  expect_equal(
    format(suppressWarnings(seq(
      to = datetime("2025-10-06 02:30:00", tz = tz),
      by = "1 day",
      length.out = 3
    ))),
    format(result)
  )
})

test_that("seq.mixtime by hour steps real hours across daylight saving", {
  tz <- "Australia/Melbourne"
  from <- datetime("2025-04-06 01:00:00", tz = tz)
  expected <- c(
    "2025-04-06 01:00:00 AEDT",
    "2025-04-06 02:00:00 AEDT",
    "2025-04-06 02:00:00 AEST",
    "2025-04-06 03:00:00 AEST"
  )
  expect_equal(
    format(seq(from, by = cal_gregorian$hour(1L), length.out = 4)),
    expected
  )
  to <- datetime("2025-04-06 03:00:00", tz = tz)
  expect_equal(format(seq(from, to, by = "1 hour")), expected)
  expect_equal(format(seq(to, from, by = "-1 hour")), rev(expected))

  expect_equal(
    format(seq(
      datetime("2025-10-05 01:00:00", tz = tz),
      by = "1 hour",
      length.out = 3
    )),
    c(
      "2025-10-05 01:00:00 AEST",
      "2025-10-05 03:00:00 AEDT",
      "2025-10-05 04:00:00 AEDT"
    )
  )
})

test_that("seq.mixtime matches adding durations to its start", {
  units <- list(
    hour = cal_gregorian$hour(1L),
    day = cal_gregorian$day(1L),
    hour24 = cal_gregorian$hour(24L),
    month = cal_gregorian$month(1L),
    year = cal_gregorian$year(1L)
  )
  starts <- list(
    # Before the end and the start of daylight saving in Melbourne
    datetime("2025-04-05 02:30:00", tz = "Australia/Melbourne"),
    datetime("2025-10-04 02:30:00", tz = "Australia/Melbourne"),
    datetime("2024-01-31 02:30:00", tz = "Australia/Melbourne"),
    datetime("2024-01-31 02:30:00", tz = "UTC"),
    datetime("2024-01-31 02:30:00")
  )
  for (x in starts) {
    for (u in names(units)) {
      by <- units[[u]]
      # Enough hours to cross the transition, without passing 2038 in years
      n <- if (u == "hour") 48L else 10L
      result <- suppressWarnings(seq(x, by = by, length.out = n))
      expected <- x + duration(seq_len(n) - 1L, by)
      info <- paste(format(x), u)
      expect_equal(format(result), format(expected), info = info)
      expect_equal(as.numeric(result), as.numeric(expected), info = info)
    }
  }
})

test_that("seq.mixtime refuses temporally indeterminate steps", {
  expect_error(
    seq(yearweek("2020 W01"), by = "1 year", length.out = 3),
    class = "mixtime_error_indeterminate"
  )
  expect_equal(
    format(seq(yearweek("2020 W01"), by = "14 days", length.out = 3)),
    c("2020 W01", "2020 W03", "2020 W05")
  )
})

test_that("seq.mixtime shifts multi-unit chronons by calendar units", {
  # Half-hourly time zone aware times, like tsibbledata::vic_elec
  x <- as.POSIXct("2024-04-06 02:30:00", tz = "Australia/Melbourne") +
    1800 * (0:3)
  half_hourly <- mixtime(x, chronon = cal_gregorian$minute(30L))
  minutely <- mixtime(x, chronon = cal_gregorian$minute(1L))
  for (by in list(
    cal_gregorian$day(1L),
    cal_gregorian$month(1L),
    cal_gregorian$year(1L)
  )) {
    result <- seq(half_hourly[1L], by = by, length.out = 4L)
    expect_equal(
      format(result),
      format(seq(minutely[1L], by = by, length.out = 4L))
    )
    expect_equal(
      as.numeric(result),
      as.numeric(half_hourly[1L] + duration(0:3, by))
    )
  }

  # Across the daylight saving change the local clock time is kept
  start <- mixtime(
    as.POSIXct("2024-10-05 02:30:00", tz = "Australia/Melbourne"),
    chronon = cal_gregorian$minute(30L)
  )
  expect_equal(
    format(seq(start, by = cal_gregorian$day(1L), length.out = 3L)),
    format(seq(
      mixtime(
        as.POSIXct("2024-10-05 02:30:00", tz = "Australia/Melbourne"),
        chronon = cal_gregorian$minute(1L)
      ),
      by = cal_gregorian$day(1L),
      length.out = 3L
    ))
  )
})

test_that("seq.mixtime steps by months that don't divide a year", {
  from <- date("2020-01-31")

  # Chaining `+` (no step is clamped from a month-end in Jan/Jul)
  chained <- format(from)
  x <- from
  for (i in 1:3) {
    x <- x + months(18L)
    chained <- c(chained, format(x))
  }
  result <- seq(from, by = months(18L), length.out = 4L)
  expect_equal(format(result), chained)
  expect_equal(
    format(result),
    c("2020-01-31", "2021-07-31", "2023-01-31", "2024-07-31")
  )

  # Each time point is a single shift of the anchor, like `+`
  for (by in list(months(5L), months(7L), months(-7L), months(13L))) {
    result <- seq(from, by = by, length.out = 8L, on_invalid = "clamp")
    expect_equal(format(result), format(from + by * (0:7)))
  }

  # Granule `by` and `to`
  expect_equal(
    format(seq(from, date("2024-07-31"), by = cal_gregorian$month(18L))),
    format(result <- from + months(18L) * (0:3))
  )
  expect_equal(
    format(seq(from, date("2024-07-30"), by = months(18L))),
    format(result[1:3])
  )
  expect_equal(
    format(seq(date("2024-07-31"), from, by = months(-18L))),
    format(rev(result))
  )
  expect_equal(
    format(seq(date("2024-07-31"), date("2020-02-01"), by = months(-18L))),
    format(rev(result)[1:3])
  )
  expect_equal(
    format(seq(to = date("2024-07-31"), by = months(18L), length.out = 4L)),
    format(result)
  )

  # Overflowing past month-end
  expect_equal(
    format(seq(from, by = months(5L), length.out = 3L, on_invalid = "overflow")),
    c("2020-01-31", "2020-07-01", "2020-12-01")
  )
})
