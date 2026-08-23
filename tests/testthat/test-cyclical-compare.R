# A cycle is a modulus rather than a unit of measure, so unlike a chronon there
# is no common cycle to reconcile two cyclical vectors to.
test_that("cyclical vectors of differing cycles can't be combined", {
  dow <- day_of_week(date("2020-01-15"))@x[[1L]]
  doy <- day_of_year(date("2020-01-15"))@x[[1L]]

  expect_error(vctrs::vec_ptype2(dow, doy), class = "vctrs_error_incompatible_type")
  expect_error(vctrs::vec_c(dow, doy), class = "vctrs_error_incompatible_type")
  expect_error(vctrs::vec_cast(dow, doy), class = "vctrs_error_cast")
})

test_that("cyclical vectors sharing a cycle combine on their common chronon", {
  doy <- day_of_year(as.Date("2020-01-15"))@x[[1L]]
  moy <- month_of_year(as.Date("2020-01-15"))@x[[1L]]

  ptype <- vctrs::vec_ptype2(doy, moy)
  expect_true(S7::S7_inherits(ptype, mt_cyclical))
  # finest common chronon, cycle carried through unchanged
  expect_equal(ptype@chronon, doy@chronon)
  expect_equal(ptype@cycle, doy@cycle)

  # the month casts to the first day of that month within the same cycle
  expect_equal(format(vctrs::vec_c(doy, moy)), c("D15", "D01"))
})

test_that("cycles differing only in inherited properties are reconciled", {
  naive <- day_of_week(date("2020-01-15"))@x[[1L]]
  tzed <- day_of_week(date("2020-01-22", tz = "Australia/Melbourne"))@x[[1L]]

  expect_equal(cycle_common(naive@cycle, tzed@cycle), tzed@cycle)
  expect_no_error(vctrs::vec_c(naive, tzed))
})

test_that("a mixtime still holds differing cycles side by side", {
  d <- as.Date("2020-01-15")
  x <- c(day_of_week(d), day_of_year(d))

  expect_length(x, 2L)
  expect_length(x@x, 2L)
  expect_equal(format(x), c("Wed", "D15"))
})

# Comparison reduces both operands to their position within the cycle, so a
# cyclical value means a weekday / a day-of-year, not a particular instant.
test_that("cyclical equality compares the position within the cycle", {
  # 2020-01-15 and 2020-01-22 are both a Wednesday
  expect_true(day_of_week(date("2020-01-15")) == day_of_week(date("2020-01-22")))
  expect_false(day_of_week(date("2020-01-15")) == day_of_week(date("2020-01-16")))
  expect_true(day_of_week(date("2020-01-15")) != day_of_week(date("2020-01-16")))

  # the same day of two different years
  expect_true(day_of_year(date("2020-01-15")) == day_of_year(date("2021-01-15")))
  expect_false(day_of_year(date("2020-01-15")) == day_of_year(date("2020-01-16")))
})

test_that("cyclical ordering follows the position within the cycle", {
  wed <- day_of_week(date("2020-01-15"))
  fri <- day_of_week(date("2021-01-15"))

  # Friday is later in its week than Wednesday, despite the earlier absolute date
  expect_true(wed < fri)
  expect_false(fri < wed)
  expect_true(fri > wed)
  expect_true(wed <= fri)
  expect_true(fri >= wed)
})

test_that("cyclical comparison of differing chronons uses interval endpoints", {
  jan <- month_of_year(date("2020-01-15"))
  d15 <- day_of_year(date("2020-01-15"))
  mar <- month_of_year(date("2020-03-01"))

  # D15 falls within January, so neither is wholly before the other
  expect_false(d15 == jan)
  expect_false(d15 < jan)
  expect_false(d15 > jan)

  # ... but March begins after D15 ends
  expect_true(d15 < mar)
  expect_true(mar > d15)
})

test_that("cyclical comparison of differing cycles is an error", {
  expect_error(day_of_week(date("2020-01-15")) == day_of_year(date("2020-01-15")))
  expect_error(day_of_week(date("2020-01-15")) < day_of_month(date("2020-01-15")))
})

test_that("continuous cyclical time compares fractional positions", {
  t <- as.POSIXct(c("2020-01-01 09:30:00", "2020-01-05 09:30:00"), tz = "UTC")
  same <- time_of_day(t, discrete = FALSE)
  expect_true(same[1] == same[2])

  later <- time_of_day(as.POSIXct("2020-01-02 09:45:00", tz = "UTC"), discrete = FALSE)
  expect_true(same[1] < later)
})

# Equality and ordering must agree with `==`/`<`, or `unique()` would keep two
# Wednesdays that `==` reports as equal.
test_that("unique(), duplicated() and sort() use the position within the cycle", {
  d <- as.Date("2020-01-13") + 0:13
  dow <- day_of_week(d)

  expect_length(unique(dow), 7L)
  expect_equal(format(unique(dow)), c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  expect_equal(duplicated(dow), rep(c(FALSE, TRUE), each = 7L))
  expect_equal(anyDuplicated(dow), 8L)
  expect_equal(
    format(sort(rev(dow))),
    rep(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), each = 2L)
  )
  expect_equal(vctrs::vec_match(dow, unique(dow)), rep(1:7, times = 2L))
})

test_that("de-duplication spans the parts of a mixtime sharing a cycle", {
  x <- c(
    day_of_year(as.Date(c("2020-02-01", "2021-02-01"))),
    month_of_year(as.Date("2020-02-10"))
  )

  # the two 1 Februaries share a position; the month is a distinct chronon
  expect_equal(duplicated(x), c(FALSE, TRUE, FALSE))
  expect_equal(format(sort(x)), c("D32", "D32", "Feb"))
})

test_that("empty mixtime vectors and prototypes stay comparable", {
  expect_length(vctrs::vec_proxy_order(day_of_week(integer(0))), 0L)
  expect_length(vctrs::vec_proxy_order(vctrs::vec_ptype(day_of_week(0L))), 0L)
  expect_length(unique(vctrs::vec_ptype(day_of_week(0L))), 0L)
  expect_length(sort(day_of_week(integer(0))), 0L)
})

test_that("comparing a mixtime requires a single mode of time", {
  mixed <- c(yearmonth(as.Date("2020-01-15")), days(1L))
  expect_error(unique(mixed), "share one mode of time")
  expect_error(sort(mixed), "share one mode of time")

  cycles <- c(day_of_week(date("2020-01-15")), day_of_month(date("2020-01-15")))
  expect_error(unique(cycles), "differing cycles")
  expect_error(sort(cycles), "differing cycles")
})
