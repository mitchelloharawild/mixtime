test_that("yearmonth() format", {
  ym <- yearmonth(0:11)
  expect_equal(format(ym), paste("1970", month.abb))
})
