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

test_that("seq.mixtime messages describe the granules involved", {
  # Granules are S7 scalars, so their size comes from @n rather than vec_data()
  expect_error(
    seq(date("2020-01-01"), yearmonth("2020 Jun")),
    "1 day"
  )
  expect_warning(
    seq(date("2020-01-31"), by = months(1L), length.out = 3L),
    "31 days"
  )
})

test_that("seq.mixtime with negative-magnitude `by` clamps and steps backwards", {
  expect_warning(
    result <- seq(date("2020-03-31"), by = "-1 month", length.out = 3),
    "31 days"
  )
  expect_equal(format(result), c("2020-03-31", "2020-02-29", "2020-01-31"))

  result2 <- suppressWarnings(seq(
    date("2020-03-31"),
    by = months(-1L),
    length.out = 3
  ))
  expect_equal(format(result2), format(result))
})
