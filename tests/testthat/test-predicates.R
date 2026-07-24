# Tests for time_is_determinate_at(): whether a time point is well-defined at the
# precision of a given granule (see also the indeterminacy handling exercised in
# test-components.R).

test_that("discrete time is indeterminate below its chronon", {
  # A discrete (integer) year has no determinate month.
  expect_false(time_is_determinate_at(year(2020L), cal_gregorian$month))
  # A coarser-or-equal granule is always determinate.
  expect_true(time_is_determinate_at(year(2020L), cal_gregorian$year))
  expect_true(time_is_determinate_at(yearmonth(as.Date("2020-02-01")), cal_gregorian$year))
})

test_that("continuous time resolves finer granules exactly", {
  # 0% through 2020 is 0% through January -> determinate.
  expect_true(time_is_determinate_at(year(2020), cal_gregorian$month))
})

test_that("granule accepts a generator or a sized unit", {
  expect_identical(
    time_is_determinate_at(year(2020L), cal_gregorian$month),
    time_is_determinate_at(year(2020L), cal_gregorian$month(1L))
  )
})

test_that("determinacy is computed per element of a mixtime", {
  mx <- c(year(2020L), yearmonth(as.Date("2020-02-01")), year(2019L))
  expect_identical(
    time_is_determinate_at(mx, cal_gregorian$month),
    c(FALSE, TRUE, FALSE)
  )
})

test_that("missing times give NA", {
  mx <- c(year(2020L), yearmonth(as.Date(NA)))
  expect_identical(time_is_determinate_at(mx, cal_gregorian$month), c(FALSE, NA))
})
