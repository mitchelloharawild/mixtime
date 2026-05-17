# Tests for Math group generics (round, floor, ceiling) on time objects.
#
# These operations apply to the inner numeric value of the time object,
# preserving the time class and its attributes.

# ---------------------------------------------------------------------------
# round / floor / ceiling on durations
# ---------------------------------------------------------------------------

test_that("round() on a duration rounds the inner value", {
  expect_equal(as.numeric(round(days(1.3))),  1)
  expect_equal(as.numeric(round(days(1.5))),  2)
  expect_equal(as.numeric(round(days(1.7))),  2)
  expect_equal(as.numeric(round(days(-1.3))), -1)
})

test_that("round() on a duration preserves the time class", {
  result <- round(days(1.3))
  expect_true(is_mixtime(result))
})

test_that("floor() on a duration floors the inner value", {
  expect_equal(as.numeric(floor(days(1.3))),  1)
  expect_equal(as.numeric(floor(days(1.9))),  1)
  expect_equal(as.numeric(floor(days(-1.3))), -2)
})

test_that("floor() on a duration preserves the time class", {
  result <- floor(days(1.3))
  expect_true(is_mixtime(result))
})

test_that("ceiling() on a duration ceils the inner value", {
  expect_equal(as.numeric(ceiling(days(1.1))),  2)
  expect_equal(as.numeric(ceiling(days(1.0))),  1)
  expect_equal(as.numeric(ceiling(days(-1.3))), -1)
})

test_that("ceiling() on a duration preserves the time class", {
  result <- ceiling(days(1.3))
  expect_true(is_mixtime(result))
})

# ---------------------------------------------------------------------------
# round / floor / ceiling on linear time
# ---------------------------------------------------------------------------

test_that("round() on a linear time rounds the inner value", {
  expect_equal(as.numeric(round(yearmonth(1.3))),  1)
  expect_equal(as.numeric(round(yearmonth(1.5))),  2)
  expect_equal(as.numeric(round(yearmonth(1.7))),  2)
})

test_that("round() on a linear time preserves the time class", {
  result <- round(yearmonth(1.3))
  expect_true(is_mixtime(result))
})

test_that("floor() on a linear time floors the inner value", {
  expect_equal(as.numeric(floor(yearmonth(1.3))), 1)
  expect_equal(as.numeric(floor(yearmonth(1.9))), 1)
})

test_that("ceiling() on a linear time ceils the inner value", {
  expect_equal(as.numeric(ceiling(yearmonth(1.1))), 2)
  expect_equal(as.numeric(ceiling(yearmonth(1.0))), 1)
})

# ---------------------------------------------------------------------------
# round() with digits argument
# ---------------------------------------------------------------------------

test_that("round() respects the digits argument for durations", {
  expect_equal(as.numeric(round(days(1.567), digits = 1)), 1.6)
  expect_equal(as.numeric(round(days(1.567), digits = 2)), 1.57)
})
