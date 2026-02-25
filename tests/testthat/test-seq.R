test_that("seq.mixtime works with linear time using integer by", {
  # yearmonth sequences
  result <- seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"))
  expect_s3_class(result, "mixtime")
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01")))
  expect_equal(format(result[[12]]), format(yearmonth("2020-12-01")))
  
  # yearquarter with length.out and integer by
  result <- seq(yearquarter("2020-01-01"), length.out = 5, by = 1L)
  expect_s3_class(result, "mixtime")
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(yearquarter("2020-01-01")))
  expect_equal(format(result[[5]]), format(yearquarter("2021-01-01")))
  
  # yearmonthday sequences
  result <- seq(yearmonthday("2020-01-01"), yearmonthday("2020-01-10"))
  expect_s3_class(result, "mixtime")
  expect_length(result, 10)
  expect_equal(format(result[[1]]), format(yearmonthday("2020-01-01")))
  expect_equal(format(result[[10]]), format(yearmonthday("2020-01-10")))
})

test_that("seq.mixtime works with linear time using string intervals", {
  # Month intervals
  result <- seq(yearmonthday("2020-01-01"), yearmonthday("2020-12-31"), by = "1 month")
  expect_s3_class(result, "mixtime")
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(yearmonthday("2020-01-01")))
  expect_equal(format(result[[12]]), format(yearmonthday("2020-12-01")))
  
  # Year intervals
  result <- seq(yearmonth("2020-01-01"), yearmonth("2025-01-01"), by = "1 year")
  expect_s3_class(result, "mixtime")
  expect_length(result, 6)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01")))
  expect_equal(format(result[[6]]), format(yearmonth("2025-01-01")))
  
  # Week intervals with length.out
  result <- seq(yearmonthday("2020-01-01"), length.out = 10, by = "2 weeks")
  expect_s3_class(result, "mixtime")
  expect_length(result, 10)
  expect_equal(format(result[[1]]), format(yearmonthday("2020-01-01")))
  expect_equal(format(result[[10]]), format(yearmonthday(as.Date("2020-01-01") + 2 * 7 * 9)))
})

test_that("seq.mixtime works with linear time using time unit objects", {
  # Month time units
  result <- seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"), by = cal_gregorian$month(2L))
  expect_s3_class(result, "mixtime")
  expect_length(result, 6)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01")))
  expect_equal(format(result[[6]]), format(yearmonth("2020-11-01")))
  
  # Year time units with length.out
  result <- seq(yearmonthday("2020-01-01"), length.out = 5, by = cal_gregorian$year(1L))
  expect_s3_class(result, "mixtime")
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(yearmonthday("2020-01-01")))
  expect_equal(format(result[[5]]), format(yearmonthday("2024-01-01")))
  
  # Day time units
  result <- seq(yearmonthday("2020-01-01"), yearmonthday("2020-01-31"), by = cal_gregorian$day(7L))
  expect_s3_class(result, "mixtime")
  expect_length(result, 5)
  expect_equal(format(result[[1]]), format(yearmonthday("2020-01-01")))
  expect_equal(format(result[[5]]), format(yearmonthday("2020-01-29")))
})

test_that("seq.mixtime works with cyclical time", {
  # month_of_year sequence (full cycle)
  result <- seq(month_of_year(0L), month_of_year(11L))
  expect_s3_class(result, "mixtime")
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(month_of_year(0L)))
  expect_equal(format(result[[12]]), format(month_of_year(11L)))
  
  # month_of_year with wrap-around and time unit by
  result <- seq(month_of_year(5L), month_of_year(3L), by = cal_gregorian$month(2L))
  expect_s3_class(result, "mixtime")
  # Should wrap around: Jun -> Aug -> Oct -> Dec -> Feb -> Apr
  expect_length(result, 6)
  expect_equal(format(result[[1]]), format(month_of_year(5L)))
  expect_equal(format(result[[6]]), format(month_of_year(3L)))
  
  # day_of_week sequence with integer by
  result <- seq(day_of_week(0L), day_of_week(6L), by = 1L)
  expect_s3_class(result, "mixtime")
  expect_length(result, 7)
  expect_equal(format(result[[1]]), format(day_of_week(0L)))
  expect_equal(format(result[[7]]), format(day_of_week(6L)))
})

test_that("seq.mixtime handles length.out correctly", {
  # With linear time
  result <- seq(yearmonth("2020-01-01"), length.out = 24)
  expect_s3_class(result, "mixtime")
  expect_length(result, 24)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01")))
  expect_equal(format(result[[24]]), format(yearmonth("2021-12-01")))
  
  # With cyclical time
  result <- seq(month_of_year(0L), length.out = 15)
  expect_s3_class(result, "mixtime")
  expect_length(result, 15)
  expect_equal(format(result[[1]]), format(month_of_year(0L)))
})

test_that("seq.mixtime handles along.with correctly", {
  reference_vec <- 1:10
  
  result <- seq(yearmonth("2020-01-01"), along.with = reference_vec)
  expect_s3_class(result, "mixtime")
  expect_length(result, 10)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01")))
  expect_equal(format(result[[10]]), format(yearmonth("2020-10-01")))
})

test_that("seq.mixtime handles backward sequences", {
  # Linear time going backward
  result <- seq(yearmonth("2020-12-01"), yearmonth("2020-01-01"), by = -1L)
  expect_s3_class(result, "mixtime")
  expect_length(result, 12)
  expect_equal(format(result[[1]]), format(yearmonth("2020-12-01")))
  expect_equal(format(result[[12]]), format(yearmonth("2020-01-01")))
  
  # With negative time units
  result <- seq(yearmonthday("2020-12-31"), yearmonthday("2020-12-01"), by = cal_gregorian$day(-5L))
  expect_s3_class(result, "mixtime")
  expect_length(result, 7)
  expect_equal(format(result[[1]]), format(yearmonthday("2020-12-31")))
  expect_equal(format(result[[7]]), format(yearmonthday("2020-12-01")))
})

test_that("seq.mixtime handles edge cases", {
  # Single element sequence
  result <- seq(yearmonth("2020-01-01"), yearmonth("2020-01-01"))
  expect_s3_class(result, "mixtime")
  expect_length(result, 1)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01")))
  
  # Length.out = 1
  result <- seq(yearmonth("2020-01-01"), length.out = 1)
  expect_s3_class(result, "mixtime")
  expect_length(result, 1)
  
  # Empty along.with (though unusual)
  result <- seq(yearmonth("2020-01-01"), along.with = integer(0))
  expect_s3_class(result, "mixtime")
  expect_length(result, 0)
})

test_that("seq.mixtime works with Date objects in mixtime", {
  # Dates should work through mixtime
  result <- seq(mixtime(as.Date("2020-01-01")), mixtime(as.Date("2020-01-10")))
  expect_s3_class(result, "mixtime")
  expect_length(result, 10)
})

test_that("seq.mixtime works with different by specifications", {
  # Integer by
  result1 <- seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"), by = 2L)
  expect_length(result1, 6)
  
  # String by
  result2 <- seq(yearmonthday("2020-01-01"), yearmonthday("2020-02-01"), by = "1 week")
  expect_s3_class(result2, "mixtime")
  
  # Time unit by
  result3 <- seq(yearmonth("2020-01-01"), yearmonth("2020-12-01"), by = cal_gregorian$quarter(1L))
  expect_s3_class(result3, "mixtime")
  expect_length(result3, 4)
})


test_that("seq.mixtime with fractional linear time", {
  # Fractional yearmonth with day-based by
  result <- seq(yearmonth("2020-01-01", discrete = FALSE), yearmonth("2025-01-01"), by = "30 days")
  expect_s3_class(result, "mixtime")
  expect_length(result, 61)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01", discrete = FALSE)))
  # Last element should be close to but not exceed the end
  expect_match(format(result[[61]]), "2024 Dec ")
  
  # Fractional yearmonthday with week-based by
  result <- seq(yearmonthday("2020-01-01", discrete = FALSE), yearmonthday("2020-02-01"), by = "1 week")
  expect_s3_class(result, "mixtime")
  expect_gt(length(result), 4)  # More than 4 weeks
  expect_equal(format(result[[1]]), format(yearmonthday("2020-01-01", discrete = FALSE)))
  
  # Fractional yearmonth with month-based by (should work cleanly)
  result <- seq(yearmonth("2020-01-01", discrete = FALSE), yearmonth("2020-06-01"), by = "1 month")
  expect_s3_class(result, "mixtime")
  expect_length(result, 6)
  expect_equal(format(result[[1]]), format(yearmonth("2020-01-01", discrete = FALSE)))
  expect_equal(format(result[[6]]), format(yearmonth("2020-06-01", discrete = FALSE)))
  
  # Fractional yearquarter with day-based by
  result <- seq(yearquarter("2020-01-01", discrete = FALSE), length.out = 10, by = "15 days")
  expect_s3_class(result, "mixtime")
  expect_length(result, 10)
  # Should show fractional percentages within quarters
  expect_equal(format(result[[1]]), format(yearquarter("2020-01-01", discrete = FALSE)))
  
  # Fractional with length.out
  result <- seq(yearmonth("2020-01-01", discrete = FALSE), length.out = 13, by = "25 days")
  expect_s3_class(result, "mixtime")
  expect_length(result, 13)
  expect_equal(format(result[[13]]), format(yearmonth(as.Date("2020-01-01") + 25*12, discrete = FALSE)))
  
  # Backward fractional sequence
  result <- seq(yearmonth("2020-12-01", discrete = FALSE), yearmonth("2020-01-01"), by = "-30 days")
  expect_s3_class(result, "mixtime")
  expect_gt(length(result), 10)
  expect_equal(format(result[[1]]), format(yearmonth("2020-12-01", discrete = FALSE)))
  expect_equal(format(result[[12]]), format(yearmonth("2020-01-06", discrete = FALSE)))
})
