test_that("index_valid returns TRUE for mixtime objects", {
  expect_true(tsibble::index_valid(yearmonth(1:3)))
  expect_true(tsibble::index_valid(c(yearmonth(1:3), year(2000:2002))))
})

test_that("interval_pull works for a single-chronon mixtime", {
  intvl <- tsibble::interval_pull(yearmonth(360L))
  expect_s3_class(intvl, "interval")
  expect_equal(format(intvl), "1M")
})

test_that("interval_pull works for a mixed-chronon mixtime", {
  # This was the original failing case: @n accessed on a duration wrapper
  intvl <- tsibble::interval_pull(c(yearmonth(360L), year(2000L)))
  expect_s3_class(intvl, "interval")
  expect_equal(format(intvl), "1M, 1Y")
})
