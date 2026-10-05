test_that("index_valid returns TRUE for mixtime objects", {
  expect_true(tsibble::index_valid(yearmonth(1:3)))
  expect_true(tsibble::index_valid(c(yearmonth(1:3), year(2000:2002))))
})

test_that("interval_pull works for a single-chronon mixtime", {
  # Legacy interval shape, only for tsibble without its own mixtime methods
  skip_if_not(is.null(tsibble_mixtime_method("interval_pull")))
  intvl <- tsibble::interval_pull(yearmonth(360L))
  expect_s3_class(intvl, "interval")
  expect_equal(format(intvl), "1M")
})

test_that("interval_pull works for a mixed-chronon mixtime", {
  # Legacy interval shape, only for tsibble without its own mixtime methods
  skip_if_not(is.null(tsibble_mixtime_method("interval_pull")))
  # This was the original failing case: @n accessed on a duration wrapper
  intvl <- tsibble::interval_pull(c(yearmonth(360L), year(2000L)))
  expect_s3_class(intvl, "interval")
  expect_equal(format(intvl), "1M, 1Y")
})

test_that("tsibble's own mixtime methods take precedence", {
  own_interval <- tsibble_mixtime_method("interval_pull")
  own_valid <- tsibble_mixtime_method("index_valid")
  skip_if(is.null(own_interval) || is.null(own_valid))
  x <- yearmonth(1:3)
  expect_identical(tsibble::interval_pull(x), own_interval(x))
  expect_identical(tsibble::index_valid(x), own_valid(x))
})
