test_that("zoo::yearmon converts to the correct months (tidyverts/tsibble#272)", {
  skip_if_not_installed("zoo")
  x <- zoo::as.yearmon(2020 + c(5, 0:4, NA, 11) / 12)
  expected <- c(
    "2020 Jun", "2020 Jan", "2020 Feb", "2020 Mar", "2020 Apr", "2020 May",
    "NA", "2020 Dec"
  )

  expect_equal(format(yearmonth(x)), expected)
  expect_equal(format(mixtime(x)), expected)
  expect_equal(format(as_mixtime(x)), expected)
  expect_equal(yearmonth(x), yearmonth(zoo::as.Date(x)))

  # Other chronons
  expect_equal(
    format(yearquarter(x)),
    c("2020 Q2", "2020 Q1", "2020 Q1", "2020 Q1", "2020 Q2", "2020 Q2", "NA", "2020 Q4")
  )
  # Finer chronons are temporally indeterminate for a discrete month
  expect_error(date(x[1:2]), class = "mixtime_error_indeterminate")

  # Before the epoch, and zero-length input
  expect_equal(
    format(yearmonth(zoo::as.yearmon(c(1969, 1900 + 11 / 12)))),
    c("1969 Jan", "1900 Dec")
  )
  expect_length(yearmonth(zoo::as.yearmon(numeric())), 0L)
})

test_that("zoo::yearqtr converts to the correct quarters (tidyverts/tsibble#272)", {
  skip_if_not_installed("zoo")
  x <- zoo::as.yearqtr(2020 + c(3, 0, NA, 1) / 4)
  expected <- c("2020 Q4", "2020 Q1", "NA", "2020 Q2")

  expect_equal(format(yearquarter(x)), expected)
  expect_equal(format(mixtime(x)), expected)
  expect_equal(format(as_mixtime(x)), expected)
  expect_error(yearmonth(x), class = "mixtime_error_indeterminate")
  expect_equal(format(yearquarter(zoo::as.yearqtr(1969.75))), "1969 Q4")
})

test_that("zoo time classes combine with mixtime", {
  skip_if_not_installed("zoo")
  expect_equal(
    format(c(yearmonth(as.Date("2019-12-01")), zoo::as.yearmon(2020))),
    c("2019 Dec", "2020 Jan")
  )
})
