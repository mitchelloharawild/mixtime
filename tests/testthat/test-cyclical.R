test_that("Cyclical time handles text parsing with set timezones", {
  # Test that day_of_week preserves the correct day in the specified timezone
  result <- day_of_week("2020-01-01", tz = "America/Los_Angeles")
  
  # 2020-01-01 is a Wednesday
  expect_equal(format(result), "Wed")
  
  # Test with UTC
  result_utc <- day_of_week("2020-01-01", tz = "UTC")
  expect_equal(format(result_utc), "Wed")
  
  # Test with Europe/Berlin (same date, so same day of week)
  result_berlin <- day_of_week("2020-01-01", tz = "Europe/Berlin")
  expect_equal(format(result_berlin), "Wed")
})

test_that("cyclical_time() handles timezone conversions showing position differences", {
  # Create a POSIXct representing 2020-01-01 09:00 in Australia/Melbourne
  melbourne_time <- as.POSIXct("2020-01-01 09:00:00", tz = "Australia/Melbourne")
  
  # In Melbourne, this is 2020-01-01 (Wednesday)
  melbourne_dow <- day_of_week(melbourne_time, tz = "Australia/Melbourne")
  expect_equal(format(melbourne_dow), "Wed")
  
  # Convert to America/Los_Angeles - should be 2019-12-31 (Tuesday)
  pacific_dow <- day_of_week(melbourne_time, tz = "America/Los_Angeles")
  expect_equal(format(pacific_dow), "Tue")
  
  # Convert to Europe/Berlin - should be 2019-12-31 (Tuesday)
  berlin_dow <- day_of_week(melbourne_time, tz = "Europe/Berlin")
  expect_equal(format(berlin_dow), "Tue")
})

test_that("cyclical_time() month_of_year handles timezone-induced date shifts", {
  # Create a POSIXct representing 2020-12-31 20:00 in America/Los_Angeles
  pacific_time <- as.POSIXct("2020-12-31 20:00:00", tz = "America/Los_Angeles")
  
  # In Pacific time, this is December (month 12)
  pacific_month <- month_of_year(pacific_time, tz = "America/Los_Angeles")
  expect_equal(format(pacific_month), "Dec")
  
  # Convert to Australia/Melbourne - should be next day (2021-01-01), January (month 1)
  melbourne_month <- month_of_year(pacific_time, tz = "Australia/Melbourne")
  expect_equal(format(melbourne_month), "Jan")
  
  # UTC should also be January 1st (month 1)
  utc_month <- month_of_year(pacific_time, tz = "UTC")
  expect_equal(format(utc_month), "Jan")
})

test_that("cyclical_time() day_of_month handles date boundaries across timezones", {
  # Create a POSIXct representing 2020-01-31 23:00 in UTC
  utc_time <- as.POSIXct("2020-01-31 23:00:00", tz = "UTC")
  
  # In UTC, this is the 31st
  utc_dom <- day_of_month(utc_time, tz = "UTC")
  expect_equal(as.character(format(utc_dom)), "31")
  
  # In Australia/Sydney (UTC+11), this is February 1st (day 1)
  sydney_dom <- day_of_month(utc_time, tz = "Australia/Sydney")
  expect_equal(as.character(format(sydney_dom)), "01")
})

test_that("cyclical_time() day_of_year handles year boundary transitions", {
  # Create a POSIXct representing 2020-12-31 23:00 in America/Los_Angeles
  pacific_time <- as.POSIXct("2020-12-31 23:00:00", tz = "America/Los_Angeles")
  
  # In Pacific time, this is day 366 of 2020 (leap year)
  pacific_doy <- day_of_year(pacific_time, tz = "America/Los_Angeles")
  expect_equal(as.character(format(pacific_doy)), "366")
  
  # In UTC, this is 2021-01-01, so day 1 of 2021
  utc_doy <- day_of_year(pacific_time, tz = "UTC")
  expect_equal(as.character(format(utc_doy)), "01")
})

test_that("cyclical_time() week_of_year handles timezone shifts across week boundaries", {
  # Create a POSIXct representing 2020-01-01 09:00 in America/Los_Angeles (Wednesday)
  pacific_time <- as.POSIXct("2020-01-01 09:00:00", tz = "America/Los_Angeles")
  
  # In Pacific time, 2020-01-01 is in week 1
  pacific_week <- week_of_year(pacific_time, tz = "America/Los_Angeles")
  expect_equal(format(pacific_week), "01")
  
  # Convert to Australia/Melbourne - becomes 2020-01-02, still in week 1
  melbourne_week <- week_of_year(pacific_time, tz = "Australia/Melbourne")
  expect_equal(format(melbourne_week), "01")
})

test_that("cyclical_time() preserves cycle position for same calendar day across timezones", {
  # Create a POSIXct representing a time on 2020-06-15 (Monday)
  utc_time <- as.POSIXct("2020-06-15 12:00:00", tz = "UTC")
  
  # Convert to different timezones - all should have same day of week
  # since the date 2020-06-15 is the same across timezones (just different times)
  pacific_dow <- day_of_week(utc_time, tz = "America/Los_Angeles")
  berlin_dow <- day_of_week(utc_time, tz = "Europe/Berlin")
  sydney_dow <- day_of_week(utc_time, tz = "Australia/Sydney")
  
  # All should show Monday
  expect_equal(format(pacific_dow), "Mon")
  expect_equal(format(berlin_dow), "Mon")
  expect_equal(format(sydney_dow), "Mon")
})

test_that("cyclical_time() discrete vs continuous modes work with timezones", {
  # Create a POSIXct
  utc_time <- as.POSIXct("2020-06-15 12:30:45", tz = "UTC")
  
  # Discrete mode should return clean labels without percentage
  discrete_dow <- day_of_week(utc_time, tz = "UTC", discrete = TRUE)
  expect_equal(format(discrete_dow), "Mon")
  
  # Continuous mode should include fractional percentage
  continuous_dow <- day_of_week(utc_time, tz = "UTC", discrete = FALSE)
  expect_equal(format(continuous_dow), "Mon-52.1%")
})
