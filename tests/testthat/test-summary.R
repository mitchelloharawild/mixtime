test_that("min(), max() and range() return mixtime (tidyverts/tsibble#272)", {
  x <- yearmonth(as.Date("2020-01-01") + c(40, 0, 80))

  expect_s7_class(min(x), class_mixtime)
  expect_equal(min(x), yearmonth(as.Date("2020-01-01")))
  expect_equal(max(x), yearmonth(as.Date("2020-03-01")))
  expect_equal(range(x), yearmonth(as.Date(c("2020-01-01", "2020-03-01"))))

  # Multiple arguments, including non-mixtime time classes
  expect_equal(min(x, yearmonth(as.Date("2019-01-01"))), yearmonth(as.Date("2019-01-01")))
  expect_equal(format(min(x, as.Date("2019-06-15"))), "2019-06-15")

  # Other chronons
  expect_equal(max(date(as.Date("2020-01-01") + 0:3)), date(as.Date("2020-01-04")))
})

test_that("min(), max() and range() handle missing values", {
  x <- c(yearmonth(as.Date("2020-01-01") + c(40, 0, 80)), NA)

  expect_true(is.na(min(x)))
  expect_length(min(x), 1L)
  expect_equal(is.na(range(x)), c(TRUE, TRUE))
  expect_equal(min(x, na.rm = TRUE), yearmonth(as.Date("2020-01-01")))
  expect_equal(range(x, na.rm = TRUE), yearmonth(as.Date(c("2020-01-01", "2020-03-01"))))
})

test_that("min(), max() and range() of empty input warn and return NA", {
  x <- yearmonth(1:3)[0]
  expect_warning(res <- min(x), "No non-missing arguments")
  expect_s7_class(res, class_mixtime)
  expect_true(is.na(res))
  expect_warning(res <- range(x), "No non-missing arguments")
  expect_equal(is.na(res), c(TRUE, TRUE))
  expect_warning(res <- max(yearmonth(NA_integer_), na.rm = TRUE), "No non-missing arguments")
  expect_true(is.na(res))
})

test_that("min(), max() and range() work with mixed chronons", {
  x <- c(yearmonth(as.Date("2020-01-01") + c(0, 40, 80)), year(c(2019L, 2021L)))
  expect_equal(range(x), year(c(2019L, 2021L)))
  expect_equal(min(x[1:3], year(2021L)), yearmonth(as.Date("2020-01-01")))
  expect_equal(max(x[1:3], year(2019L)), yearmonth(as.Date("2020-03-01")))
})

test_that("Other Summary generics error meaningfully", {
  x <- yearmonth(1:3)
  expect_error(sum(x), "`sum\\(\\)` is not supported")
  expect_error(prod(x), "`prod\\(\\)` is not supported")
  expect_error(any(x), "`any\\(\\)` is not supported")
  expect_error(all(x), "`all\\(\\)` is not supported")
  expect_error(sum(x, days(1)), "`sum\\(\\)` is not supported")
})

test_that("Other Summary generics still summarise durations", {
  expect_equal(sum(days(1:3)), 6L)
  expect_equal(sum(days(1:3), days(4L)), 10L)
})
