test_that("yearweek creation from integers works", {
  # Test basic creation
  yw <- yearweek(0:52)
  expect_length(yw, 53)
  
  # Test single value
  yw_single <- yearweek(1)
  expect_length(yw_single, 1)
})

test_that("yearweek creation from dates works", {
  # Test with multiple dates
  dates <- as.Date(c("2023-01-01", "2023-06-15", "2023-12-31"))
  yw <- yearweek(dates)
  expect_length(yw, 3)
})

test_that("yearweek handles edge cases around year boundaries", {
  # December 31, 2024 is a Tuesday, belongs to 2025-W1
  expect_equal(format(yearweek(as.Date("2024-12-31"))), "2025 W01")
  
  # January 1, 2024 is a Monday, starts 2024-W1
  expect_equal(format(yearweek(as.Date("2024-01-01"))), "2024 W01")
  
  # January 1, 2023 is a Sunday, belongs to 2022-W52
  expect_equal(format(yearweek(as.Date("2023-01-01"))), "2022 W52")
  
  # January 2, 2023 is a Monday, starts 2023-W1
  expect_equal(format(yearweek(as.Date("2023-01-02"))), "2023 W01")
})

test_that("yearweek handles years with 53 weeks", {
  # 2020 had 53 weeks (leap year starting on Wednesday)
  expect_equal(format(yearweek(as.Date("2020-12-31"))), "2020 W53")
  
  # 2015 had 53 weeks (started on Thursday)
  expect_equal(format(yearweek(as.Date("2015-12-31"))), "2015 W53")
})

test_that("yearweek Thursday rule is correct", {
  # ISO 8601 rule: Week 1 is the first week with a Thursday
  
  # 2019-01-01 is Tuesday, so it's in 2019-W01 (Thu is Jan 3)
  expect_equal(format(yearweek(as.Date("2019-01-01"))), "2019 W01")
  
  # 2021-01-01 is Friday, so it's in 2020-W53 (no Thu in that week of 2021)
  expect_equal(format(yearweek(as.Date("2021-01-01"))), "2020 W53")
  
  # 2022-01-01 is Saturday, so it's in 2021-W52
  expect_equal(format(yearweek(as.Date("2022-01-01"))), "2021 W52")
})

test_that("yearweek handles various date ranges without errors", {
  # Test that converting dates gives consistent results
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "7 days")
  yw <- yearweek(dates)
  expect_false(any(is.na(yw)))
  expect_length(yw, length(dates))
  
  # Leap year dates
  leap_dates <- as.Date(c("2020-02-28", "2020-02-29", "2020-03-01"))
  yw_leap <- yearweek(leap_dates)
  expect_length(yw_leap, 3)
  expect_false(any(is.na(yw_leap)))
  
  # Non-leap year dates
  non_leap_dates <- as.Date(c("2021-02-28", "2021-03-01"))
  yw_non_leap <- yearweek(non_leap_dates)
  expect_length(yw_non_leap, 2)
  expect_false(any(is.na(yw_non_leap)))
})

test_that("yearweek handles Unix epoch correctly", {
  # Unix epoch: 1970-01-01 is a Thursday, should be 1970-W01
  expect_equal(format(yearweek(as.Date("1970-01-01"))), "1970 W01")
})



test_that("yearweek handles all days of week consistently", {
  # Test a full week to ensure all weekdays are handled
  week_start <- as.Date("2023-06-05") # A Monday
  full_week <- seq(week_start, by = "1 day", length.out = 7)
  yw_week <- yearweek(full_week)
  
  # All days in the same week should have the same yearweek
  expect_equal(length(unique(yw_week)), 1)
})

test_that("yearweek handles year boundary transitions", {
  # Comprehensive test of dates around year boundaries for multiple years
  test_years <- 2020:2025
  
  for (year in test_years) {
    # Test last few days of December
    dec_dates <- as.Date(paste0(year, "-12-", 29:31))
    yw_dec <- yearweek(dec_dates)
    expect_false(any(is.na(yw_dec)))
    
    # Test first few days of January
    jan_dates <- as.Date(paste0(year, "-01-", 1:3))
    yw_jan <- yearweek(jan_dates)
    expect_false(any(is.na(yw_jan)))
  }
})

test_that("cal_isoweek$week chronon_cardinality works correctly", {
  week_unit <- cal_isoweek$week(1L)
  day_unit <- cal_isoweek$day(1L)
  
  # 1 week = 7 days
  expect_equal(chronon_cardinality(week_unit, day_unit), 7)
  
  # 2 weeks = 14 days
  expect_equal(chronon_cardinality(cal_isoweek$week(2L), day_unit), 14)
})

test_that("chronon_divmod from days to weeks works", {
  day_unit <- cal_isoweek$day(1L)
  week_unit <- cal_isoweek$week(1L)
  
  # Test with multiples of 7 (should have remainder 3 for Thursday epoch)
  result <- chronon_divmod(day_unit, week_unit, 7L)
  expect_equal(result$remainder, 3L)
  
  # Test with non-multiples
  result2 <- chronon_divmod(day_unit, week_unit, 10L)
  expect_equal(result2$chronon, 1L)
  expect_equal(result2$remainder, 6L)
})

test_that("chronon_divmod from weeks to days works", {
  week_unit <- cal_isoweek$week(1L)
  day_unit <- cal_isoweek$day(1L)
  
  # 1 week should convert to 7 days (plus epoch offset)
  result <- chronon_divmod(week_unit, day_unit, 1L)
  expect_equal(result$remainder, 0L)
  expect_equal(result$chronon %% 7, 4L)  # Check epoch alignment
})