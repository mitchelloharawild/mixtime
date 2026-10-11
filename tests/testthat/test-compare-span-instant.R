test_that("bare numbers compare as mixtime() reads them, from the chronon's epoch", {
  expect_true(year(2026L) == 2026L)
  expect_false(year(2026L) == 56L)
  expect_true(yearmonth(as.Date("1970-02-01")) == 1L)
  # durations have no epoch
  expect_true(days(3L) == 3)
})
