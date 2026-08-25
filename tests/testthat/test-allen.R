# These tests cover Allen's interval algebra (see R/compare.R, `?allen-algebra`,
# `?mt_linear-compare`). They reuse the same half-open interval endpoints
# already exercised by test-cyclical-compare.R, so focus on the relations
# themselves rather than re-testing chronon reconciliation.

test_that("precedes/preceded-by require a genuine gap, unlike `<`/`>`", {
  jan <- yearmonth(as.Date("2020-01-01"))
  feb <- yearmonth(as.Date("2020-02-01"))
  mar <- yearmonth(as.Date("2020-03-01"))

  # adjacent months meet, but don't (strictly) precede one another
  expect_false(jan %p% feb)
  expect_false(feb %pi% jan)
  # ... though mixtime's own `<`/`>` don't distinguish adjacency from a gap
  expect_true(jan < feb)
  expect_true(feb > jan)

  # an actual gap (February in between) does precede
  expect_true(jan %p% mar)
  expect_true(mar %pi% jan)
  expect_true(jan < mar)
  expect_true(mar > jan)
})

test_that("meets/met-by detect adjacency, not just a gap", {
  jan <- yearmonth(as.Date("2020-01-01"))
  feb <- yearmonth(as.Date("2020-02-01"))
  mar <- yearmonth(as.Date("2020-03-01"))

  expect_true(jan %m% feb)
  expect_true(feb %mi% jan)

  # a gap (February in between) is not "meets"
  expect_false(jan %m% mar)
  expect_false(mar %mi% jan)

  # adjacency doesn't exclude "<"/">" (see the caveat in ?allen-algebra):
  # the existing `<`/`>` don't distinguish a gap from immediate adjacency
  expect_true(jan < feb)
  expect_true(feb > jan)
})

test_that("starts/started-by require a shared start and differing ends", {
  jan <- yearmonth(as.Date("2020-01-01"))
  q1 <- yearquarter(as.Date("2020-01-01"))

  expect_true(jan %s% q1)
  expect_true(q1 %si% jan)
  # not both directions at once
  expect_false(jan %si% q1)
  expect_false(q1 %s% jan)
})

test_that("during/contains require strict containment (no shared boundary)", {
  feb <- yearmonth(as.Date("2020-02-01"))
  jan <- yearmonth(as.Date("2020-01-01"))
  q1 <- yearquarter(as.Date("2020-01-01"))

  expect_true(feb %d% q1)
  expect_true(q1 %di% feb)

  # January touches Q1's own start, so it starts Q1 rather than being during it
  expect_false(jan %d% q1)
  expect_true(jan %s% q1)
})

test_that("finishes/finished-by require a shared end and differing starts", {
  mar <- yearmonth(as.Date("2020-03-01"))
  q1 <- yearquarter(as.Date("2020-01-01"))

  expect_true(mar %f% q1)
  expect_true(q1 %fi% mar)
  expect_false(mar %fi% q1)
  expect_false(q1 %f% mar)
})

test_that("overlaps/overlapped-by hold for a genuine partial intersection", {
  # An ISO week spanning the January/February boundary
  w <- yearweek(as.Date("2020-01-29"))
  feb <- yearmonth(as.Date("2020-02-01"))

  expect_true(w %o% feb)
  expect_true(feb %oi% w)
  expect_false(w %oi% feb)
  expect_false(feb %o% w)
})

test_that("%o% falls back to base's outer product for non-mixtime operands", {
  expect_identical(1:2 %o% 1:3, base::outer(1:2, 1:3))
  expect_identical(c(1, 2.5) %o% c(1, 2, 3), base::outer(c(1, 2.5), c(1, 2, 3)))
})

test_that("exactly one relation holds for well-formed, non-adjacent pairs", {
  jan <- yearmonth(as.Date("2020-01-01"))
  feb <- yearmonth(as.Date("2020-02-01"))
  mar <- yearmonth(as.Date("2020-03-01"))
  q1 <- yearquarter(as.Date("2020-01-01"))

  relations <- c(
    "==", "%p%", "%pi%",
    "%m%", "%mi%", "%o%", "%oi%",
    "%s%", "%si%", "%d%", "%di%", "%f%", "%fi%"
  )
  holds <- function(a, b) {
    vapply(relations, function(r) do.call(r, list(a, b)), logical(1L))
  }

  expect_equal(sum(holds(jan, jan)), 1L) # ==
  expect_equal(sum(holds(jan, mar)), 1L) # %p% (an actual gap, not adjacency)
  expect_equal(sum(holds(jan, q1)), 1L) # %s%
  expect_equal(sum(holds(q1, jan)), 1L) # %si%
  expect_equal(sum(holds(feb, q1)), 1L) # %d%
  expect_equal(sum(holds(q1, feb)), 1L) # %di%
  expect_equal(sum(holds(mar, q1)), 1L) # %f%
  expect_equal(sum(holds(q1, mar)), 1L) # %fi%
})

test_that("Allen relations work between mt_cyclical vectors sharing a cycle", {
  day1 <- day_of_year(date("2020-01-01"))
  jan <- month_of_year(date("2020-01-15"))
  d15 <- day_of_year(date("2020-01-15"))
  feb <- month_of_year(date("2020-02-01"))

  # 1 January starts January (the month)
  expect_true(day1 %s% jan)
  expect_true(jan %si% day1)

  # 15 January is strictly within January
  expect_true(d15 %d% jan)
  expect_true(jan %di% d15)

  # adjacent months meet
  expect_true(jan %m% feb)
  expect_true(feb %mi% jan)
})

test_that("Allen relations error for cyclical vectors with differing cycles", {
  dow <- day_of_week(date("2020-01-15"))
  doy <- day_of_year(date("2020-01-15"))

  expect_error(dow %p% doy, class = "vctrs_error_incompatible_op")
  expect_error(dow %m% doy, class = "vctrs_error_incompatible_op")
  expect_error(dow %o% doy, class = "vctrs_error_incompatible_op")
})

test_that("Allen relations aren't defined for mt_duration (no interval to compare)", {
  expect_error(days(1) %p% days(2))
  expect_error(days(1) %m% days(2))
})

test_that("two equal continuous instants also satisfy %m% (documented caveat)", {
  # A continuous (zero-width) value trivially touches its own boundary, so
  # equal instants satisfy both `==` and `%m%` simultaneously - see the
  # non-degenerate-interval caveat in ?allen-algebra.
  t <- as.POSIXct("2020-01-01 09:00:00", tz = "UTC")
  a <- linear_time(t, chronon = cal_gregorian$second(1L), discrete = FALSE)
  b <- linear_time(t, chronon = cal_gregorian$second(1L), discrete = FALSE)

  expect_true(a == b)
  expect_true(a %m% b)
  # but relations requiring a strict endpoint difference do not also hold
  expect_false(a %p% b)
  expect_false(a %s% b)
  expect_false(a %f% b)
  expect_false(a %o% b)
  expect_false(a %d% b)
})
