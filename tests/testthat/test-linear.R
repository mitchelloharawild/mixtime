test_that("Linear time handles text parsing with set timezones", {
  # Test that yearmonthday preserves the date in the specified timezone
  result <- yearmonthday("2020-01-01", tz = "America/Los_Angeles")
  
  # Should display as 2020-Jan-01 in PST timezone
  expect_equal(format(result), "2020-Jan-1-PST")
  
  # Test with another timezone
  result_utc <- yearmonthday("2020-01-01", tz = "UTC")
  expect_equal(format(result_utc), "2020-Jan-1")
  
  # Test with Europe/Berlin
  result_berlin <- yearmonthday("2020-01-01", tz = "Europe/Berlin")
  expect_equal(format(result_berlin), "2020-Jan-1-CET")
})

test_that("linear_time() creates hour and minute times with timezone", {
  # Create a POSIXct in America/Los_Angeles
  base_time <- as.POSIXct("2020-01-15 14:30:00", tz = "America/Los_Angeles")
  result <- linear_time(base_time, minute(1L), tz = "America/Los_Angeles")
  
  # Should include date and time in Pacific timezone
  expect_s3_class(result, "mixtime")
  expect_match(format(result), "2020-01-15 14:30 PST")
  
  # Test with UTC
  base_utc <- as.POSIXct("2020-01-15 09:15:00", tz = "UTC")
  result_utc <- linear_time(base_utc, minute(1L), tz = "UTC")
  expect_match(format(result_utc), "2020-01-15 09:15")
  
  # Test with Australia/Melbourne
  base_melb <- as.POSIXct("2020-01-15 23:45:00", tz = "Australia/Melbourne")
  result_melb <- linear_time(base_melb, minute(1L), tz = "Australia/Melbourne")
  expect_match(format(result_melb), "2020-01-15 23:45 AEDT")
  
  # Test with hour precision
  base_berlin <- as.POSIXct("2020-01-15 16:00:00", tz = "Europe/Berlin")
  result_hour <- linear_time(base_berlin, hour(1L), tz = "Europe/Berlin")
  expect_match(format(result_hour), "2020-01-15 16h CET")
})

test_that("linear_time() converts between timezones showing date differences", {
  # Create a POSIXct representing 2020-01-01 09:00 in Australia/Melbourne
  melbourne_time <- as.POSIXct("2020-01-01 09:00:00", tz = "Australia/Melbourne")
  
  # Convert to America/Los_Angeles - should be previous day (2019-12-31)
  pacific_ymd <- yearmonthday(melbourne_time, tz = "America/Los_Angeles")
  expect_equal(format(pacific_ymd), "2019-Dec-31-PST")
  
  # Convert to Europe/Berlin - should also be previous day (2019-12-31)
  berlin_ymd <- yearmonthday(melbourne_time, tz = "Europe/Berlin")
  expect_equal(format(berlin_ymd), "2019-Dec-31-CET")
  
  # The Melbourne date should still be 2020-01-01
  melbourne_ymd <- yearmonthday(melbourne_time)
  expect_equal(format(melbourne_ymd), "2020-Jan-1-AEDT")
})

test_that("linear_time() preserves instant across timezone conversions", {
  # Start with a specific moment in America/Los_Angeles
  pacific_time <- as.POSIXct("2020-06-15 09:00:00", tz = "America/Los_Angeles")
  
  # Convert to Australia/Melbourne
  melbourne_converted <- linear_time(pacific_time, tz = "Australia/Melbourne")
  
  # Convert back to America/Los_Angeles
  pacific_back <- linear_time(melbourne_converted, tz = "America/Los_Angeles")
  
  # Should represent the same instant in time
  expect_equal(as.numeric(pacific_time), as.numeric(pacific_back))
  
  # But the calendar dates differ
  expect_equal(
    format(yearmonthday(melbourne_converted)),
    "2020-Jun-16-AEST"  # Next day in Melbourne
  )
  expect_equal(
    format(yearmonthday(pacific_time)),
    "2020-Jun-15-PDT"  # Original day in Pacific
  )
})