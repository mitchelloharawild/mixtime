test_that("yearmonth() format", {
  ym <- yearmonth(0:11)
  expect_equal(format(ym), paste("1970", month.abb, sep = "-"))
  # ym <- ym - yearmonth(0)
  # expect_equal(format(ym), paste0(0:11, " month", ifelse(0:11!=1,"s","")))
})