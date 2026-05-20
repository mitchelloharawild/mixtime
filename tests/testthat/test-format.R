test_that("format() handles NA in linear time", {
  expect_equal(format(yearmonth(NA_real_)), "NA")
  expect_equal(format(date(NA_character_)), "NA")
})

test_that("format() handles NaN in linear time", {
  expect_equal(format(yearmonth(NaN)), "NaN")
})

test_that("format() handles Inf and -Inf in linear time", {
  expect_equal(format(yearmonth(Inf)),  "Inf")
  expect_equal(format(yearmonth(-Inf)), "-Inf")
})

test_that("format() handles mixed regular, NA, NaN and Inf values", {
  x <- yearmonth(c(0, NA, NaN, Inf, -Inf))
  result <- format(x)
  expect_equal(result[[1]], "1970 Jan 0.0%")
  expect_equal(result[[2]], "  NA")
  expect_equal(result[[3]], " NaN")
  expect_equal(result[[4]], " Inf")
  expect_equal(result[[5]], "-Inf")
})

test_that("format() handles NA in durations", {
  expect_equal(format(months(NA_integer_)), "NA")
  expect_equal(format(days(NA_real_)), "NA")
})

test_that("format() handles NaN in durations", {
  expect_equal(format(days(NaN)), "NaN")
})

test_that("format() handles Inf and -Inf in durations", {
  expect_equal(format(months(Inf)),  "Inf")
  expect_equal(format(months(-Inf)), "-Inf")
})

test_that("format() handles mixed special values in durations", {
  x <- months(c(1L, NA_integer_, 2L))
  result <- format(x)
  expect_equal(result[[1]], "1 month")
  expect_equal(result[[2]], "NA")
  expect_equal(result[[3]], "2 months")

  x_frac <- days(c(1.5, NaN, Inf, -Inf))
  result_frac <- format(x_frac)
  expect_equal(result_frac[[2]], " NaN")
  expect_equal(result_frac[[3]], " Inf")
  expect_equal(result_frac[[4]], "-Inf")
})

test_that("format() handles special values in cyclical time", {
  x <- yearmonth(c(0, NA, Inf))
  result <- format(x)
  expect_length(result, 3L)
  expect_equal(result[[2]], " NA")
  expect_equal(result[[3]], "Inf")
})
