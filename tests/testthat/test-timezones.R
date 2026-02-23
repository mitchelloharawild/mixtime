test_that("get_tz_offset returns correct offset values", {
  # Test with a known timezone and time
  winter <- as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York")
  summer <- as.POSIXct("2024-07-15 12:00:00", tz = "America/New_York")
  
  winter_offset <- get_tz_offset(winter, "America/New_York")
  summer_offset <- get_tz_offset(summer, "America/New_York")
  
  # EST is UTC-5 (-18000 seconds), EDT is UTC-4 (-14400 seconds)
  expect_equal(winter_offset, -18000)
  expect_equal(summer_offset, -14400)
  
  # Test with UTC
  utc_time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(get_tz_offset(utc_time, "UTC"), 0)
  
  # Test with positive offset timezone
  tokyo_time <- as.POSIXct("2024-01-15 12:00:00", tz = "Asia/Tokyo")
  expect_equal(get_tz_offset(tokyo_time, "Asia/Tokyo"), 32400) # UTC+9
})

test_that("get_tz_offset works with vector input", {
  times <- as.POSIXct(c("2024-01-15 12:00:00", "2024-07-15 12:00:00"), 
                      tz = "America/New_York")
  offsets <- get_tz_offset(times, "America/New_York")
  
  expect_length(offsets, 2)
  expect_equal(offsets[1], -18000) # Winter
  expect_equal(offsets[2], -14400) # Summer
})

test_that("get_tz_abbreviation returns correct abbreviations", {
  winter <- as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York")
  summer <- as.POSIXct("2024-07-15 12:00:00", tz = "America/New_York")
  
  expect_equal(get_tz_abbreviation(winter, "America/New_York"), "EST")
  expect_equal(get_tz_abbreviation(summer, "America/New_York"), "EDT")
  
  # Test UTC
  utc_time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(get_tz_abbreviation(utc_time, "UTC"), "UTC")
})

test_that("get_tz_abbreviation works with vector input", {
  times <- as.POSIXct(c("2024-01-15 12:00:00", "2024-07-15 12:00:00"), 
                      tz = "America/New_York")
  abbrevs <- get_tz_abbreviation(times, "America/New_York")
  
  expect_length(abbrevs, 2)
  expect_equal(abbrevs[1], "EST")
  expect_equal(abbrevs[2], "EDT")
})

test_that("get_tz_transitions returns DST transitions", {
  start <- as.POSIXct("2024-01-01", tz = "America/New_York")
  end <- as.POSIXct("2024-12-31", tz = "America/New_York")
  
  transitions <- get_tz_transitions(start, end, "America/New_York")
  
  # Should have 2 transitions in 2024 (spring forward, fall back)
  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 2)
  expect_named(transitions, c("time", "offset_before", "offset_after"))
  
  # Check that offsets change
  expect_equal(transitions$offset_before[1], -18000) # EST
  expect_equal(transitions$offset_after[1], -14400)  # EDT
  expect_equal(transitions$offset_before[2], -14400) # EDT
  expect_equal(transitions$offset_after[2], -18000)  # EST
})

test_that("get_tz_transitions works with Southern Hemisphere DST", {
  start <- as.POSIXct("2024-01-01", tz = "Australia/Melbourne")
  end <- as.POSIXct("2024-12-31", tz = "Australia/Melbourne")
  
  transitions <- get_tz_transitions(start, end, "Australia/Melbourne")
  
  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 2)
  
  # Southern hemisphere: transitions are reversed (fall in April, spring in October)
  expect_equal(transitions$offset_before[1], 39600)  # AEDT (UTC+11)
  expect_equal(transitions$offset_after[1], 36000)   # AEST (UTC+10)
})

test_that("get_tz_transitions returns empty for no transitions", {
  start <- as.POSIXct("2024-01-01", tz = "UTC")
  end <- as.POSIXct("2024-12-31", tz = "UTC")
  
  transitions <- get_tz_transitions(start, end, "UTC")
  
  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 0)
})

test_that("timezone functions handle invalid timezone names gracefully", {
  time <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  
  # These should error with invalid timezone names
  expect_error(get_tz_offset(time, "Invalid/Timezone"))
  expect_error(get_tz_abbreviation(time, "Invalid/Timezone"))
  expect_error(get_tz_transitions(time, time, "Invalid/Timezone"))
})

test_that("get_tz_transitions handles time ranges with no transitions", {
  # Short time range with no transitions
  start <- as.POSIXct("2024-06-01", tz = "America/New_York")
  end <- as.POSIXct("2024-06-15", tz = "America/New_York")
  
  transitions <- get_tz_transitions(start, end, "America/New_York")
  
  expect_s3_class(transitions, "data.frame")
  expect_equal(nrow(transitions), 0)
})
