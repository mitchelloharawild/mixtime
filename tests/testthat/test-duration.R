# ---------------------------------------------------------------------------
# Integer (discrete) durations
# ---------------------------------------------------------------------------

test_that("integer duration of 1 is singular", {
  expect_equal(format(minutes(1L)), "1 minute")
  expect_equal(format(seconds(1L)), "1 second")
  expect_equal(format(hours(1L)),   "1 hour")
  expect_equal(format(days(1L)),    "1 day")
  expect_equal(format(months(1L)),  "1 month")
  expect_equal(format(years(1L)),   "1 year")
})

test_that("integer duration > 1 is plural", {
  expect_equal(format(minutes(2L)),  "2 minutes")
  expect_equal(format(seconds(30L)), "30 seconds")
  expect_equal(format(hours(12L)),   "12 hours")
  expect_equal(format(days(7L)),     "7 days")
  expect_equal(format(months(6L)),   "6 months")
  expect_equal(format(years(3L)),    "3 years")
})

test_that("integer duration of 0 is plural", {
  expect_equal(format(minutes(0L)), "0 minutes")
})

# ---------------------------------------------------------------------------
# Double (fractional) durations
# ---------------------------------------------------------------------------

test_that("fractional duration 1.0 shows one decimal place and is plural", {
  expect_equal(format(minutes(1.0)), "1.0 minutes")
  expect_equal(format(seconds(1.0)), "1.0 seconds")
  expect_equal(format(hours(1.0)),   "1.0 hours")
})

test_that("fractional duration with existing decimal shows at least one decimal place", {
  expect_equal(format(minutes(1.1)),  "1.1 minutes")
  expect_equal(format(minutes(2.5)),  "2.5 minutes")
  expect_equal(format(minutes(0.5)),  "0.5 minutes")
})

test_that("fractional duration with more decimals is shown up to 3 decimal places by default", {
  expect_equal(format(minutes(1.1)),    "1.1 minutes")
  expect_equal(format(minutes(1.16)),   "1.16 minutes")
  expect_equal(format(minutes(1.123)),  "1.123 minutes")
  expect_equal(format(minutes(1.1234)), "1.1234 minutes")
})

test_that("fractional duration respects digits argument (max decimal places)", {
  expect_equal(format(minutes(1.0),    digits = 1L), "1.0 minutes")
  expect_equal(format(minutes(1.16),   digits = 2L), "1.2 minutes")
  expect_equal(format(minutes(1.123),  digits = 3L), "1.12 minutes")
})

test_that("fractional duration 0.0 is plural with one decimal", {
  expect_equal(format(minutes(0.0)), "0.0 minutes")
})

# ---------------------------------------------------------------------------
# NA values
# ---------------------------------------------------------------------------

test_that("NA integer duration formats as NA", {
  expect_equal(format(minutes(NA_integer_)), "NA")
})

test_that("NA double duration formats as NA", {
  expect_equal(format(minutes(NA_real_)), "NA")
})

# ---------------------------------------------------------------------------
# Vectors mixing values and NAs
# ---------------------------------------------------------------------------

test_that("vector of integer durations formats correctly", {
  expect_equal(
    format(minutes(c(1L, 2L, NA_integer_))),
    c("1 minute", "2 minutes", "NA")
  )
})

test_that("vector of fractional durations formats correctly", {
  expect_equal(
    format(minutes(c(1.0, 1.1, NA_real_))),
    c("1.0 minutes", "1.1 minutes", "NA")
  )
})
