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

test_that("granules of the same time unit are compared by size", {
  # A quarter, expressed as three months, cannot resolve the month within it.
  q <- linear_time(0:2, chronon = cal_gregorian$month(3L))
  expect_false(time_is_determinate_at(q, cal_gregorian$month(1L))[[1L]])
  expect_true(time_is_determinate_at(q, cal_gregorian$month(3L))[[1L]])
  expect_true(time_is_determinate_at(q, cal_gregorian$month(15L))[[1L]])

  # ... and so cannot complete one either.
  expect_identical(
    time_is_complete_at(q, cal_gregorian$month(1L)),
    rep(FALSE, 3L)
  )
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

# Tests for time_is_complete_at(): whether the observed vector fills the coarser
# granule that each element falls into.

test_that("an incomplete granule is FALSE for all its points", {
  # 2020 Jan : 2020 Oct does not complete the year (Nov, Dec missing).
  jan_oct <- yearmonth(as.Date("2020-01-01")) + 0:9
  expect_identical(
    time_is_complete_at(jan_oct, cal_gregorian$year),
    rep(FALSE, 10L)
  )
})

test_that("only completed granules are TRUE", {
  # 2020 is complete (Jan:Dec), 2021 is not (only Jan:Mar).
  jan20_mar21 <- yearmonth(as.Date("2020-01-01")) + 0:14
  expect_identical(
    time_is_complete_at(jan20_mar21, cal_gregorian$year),
    c(rep(TRUE, 12L), rep(FALSE, 3L))
  )
})

test_that("granule accepts a generator or a sized unit", {
  x <- yearmonth(as.Date("2020-01-01")) + 0:11
  expect_identical(
    time_is_complete_at(x, cal_gregorian$year),
    time_is_complete_at(x, cal_gregorian$year(1L))
  )
})

test_that("completeness handles unordered and duplicated points", {
  # Same twelve months, shuffled and with a duplicate, still complete.
  x <- yearmonth(as.Date("2020-01-01")) + c(11:0, 5L)
  expect_true(all(time_is_complete_at(x, cal_gregorian$year)))
  # Dropping one month leaves the year incomplete.
  x <- yearmonth(as.Date("2020-01-01")) + c(0:4, 6:11)
  expect_true(all(!time_is_complete_at(x, cal_gregorian$year)))
})

test_that("duplicated points are not double-counted towards completeness", {
  # Eleven distinct months (December missing) plus a duplicate of January: twelve
  # observations but only eleven distinct months, so the year is not complete.
  x <- yearmonth(as.Date("2020-01-01")) + c(0:10, 0L)
  expect_true(all(!time_is_complete_at(x, cal_gregorian$year)))
})

test_that("a granule equal to the chronon is completed by each point", {
  x <- yearmonth(as.Date("2020-01-01")) + 0:2
  expect_identical(
    time_is_complete_at(x, cal_gregorian$month),
    rep(TRUE, 3L)
  )
})

test_that("a granule finer than the chronon is never complete", {
  # A yearly point cannot complete a month.
  expect_identical(
    time_is_complete_at(year(2020L), cal_gregorian$month),
    FALSE
  )
})

test_that("variable cardinality (days in a month) is respected", {
  # All 29 days of February 2024 (a leap year) complete the month.
  feb <- as.Date("2024-02-01") + 0:28
  expect_true(all(time_is_complete_at(feb, cal_gregorian$month)))
  # 28 days do not complete a 29-day February.
  expect_true(all(!time_is_complete_at(feb[-1L], cal_gregorian$month)))
})

test_that("completeness is computed per granule across a mixtime", {
  mx <- c(
    yearmonth(as.Date("2020-01-01")) + 0:11, # complete 2020
    yearmonth(as.Date("2021-01-01")) + 0:2   # incomplete 2021
  )
  expect_identical(
    time_is_complete_at(mx, cal_gregorian$year),
    c(rep(TRUE, 12L), rep(FALSE, 3L))
  )
})

test_that("mixed-granularity mixtimes are not supported", {
  mx <- c(yearmonth(as.Date("2020-01-01")), as.Date("2020-01-01"))
  expect_error(
    time_is_complete_at(mx, cal_gregorian$year),
    "mixed-granularity"
  )
})

test_that("missing times give NA and do not count towards completeness", {
  x <- c(yearmonth(as.Date("2020-01-01")) + 0:11, yearmonth(as.Date(NA)))
  expect_identical(
    time_is_complete_at(x, cal_gregorian$year),
    c(rep(TRUE, 12L), NA)
  )
})
