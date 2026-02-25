J2000_epoch <- unclass(as.Date("2000-01-01"))
date1 <- as.integer(as.Date("2026-02-26")) - J2000_epoch
date2 <- as.integer(as.Date("2026-08-19")) - J2000_epoch
expect_equal_time <- function(object, expected, tolerance = 120, ...) {
  expect_equal(as.numeric(object), as.numeric(expected), tolerance = tolerance, ...)
}

test_that("Solar sunrise calculations", {
  mel_sunrise <- cal_time_solar_sunrise$day(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1, mel_sunrise, tz = NULL)),
    as.POSIXct("2026-02-26 07:01:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, mel_sunrise, tz = NULL)),
    as.POSIXct("2026-08-19 07:00:00", tz = "Australia/Melbourne")
  )

  muc_sunrise <- cal_time_solar_sunrise$day(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1, muc_sunrise, tz = NULL)),
    as.POSIXct("2026-02-26 07:00:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, muc_sunrise, tz = NULL)),
    as.POSIXct("2026-08-19 06:13:00", tz = "Europe/Berlin")
  )
})

test_that("Solar sunset calculations", {
  mel_sunset <- cal_time_solar_sunset$day(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1, mel_sunset, tz = NULL)),
    as.POSIXct("2026-02-26 20:04:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, mel_sunset, tz = NULL)),
    as.POSIXct("2026-08-19 17:47:00", tz = "Australia/Melbourne")
  )

  muc_sunset <- cal_time_solar_sunset$day(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1, muc_sunset, tz = NULL)),
    as.POSIXct("2026-02-26 17:53:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, muc_sunset, tz = NULL)),
    as.POSIXct("2026-08-19 20:20:00", tz = "Europe/Berlin")
  )
})

test_that("Solar noon calculations", {
  mel_noon <- cal_time_solar_noon$day(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1, mel_noon, tz = NULL)),
    as.POSIXct("2026-02-26 13:33:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, mel_noon, tz = NULL)),
    as.POSIXct("2026-08-19 12:23:00", tz = "Australia/Melbourne")
  )

  muc_noon <- cal_time_solar_noon$day(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1, muc_noon, tz = NULL)),
    as.POSIXct("2026-02-26 12:26:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, muc_noon, tz = NULL)),
    as.POSIXct("2026-08-19 13:17:00", tz = "Europe/Berlin")
  )
})

test_that("Solar dawn calculations (civil twilight)", {
  mel_dawn <- cal_time_solar_dawn$day(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1, mel_dawn, tz = NULL)),
    as.POSIXct("2026-02-26 06:34:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, mel_dawn, tz = NULL)),
    as.POSIXct("2026-08-19 06:33:00", tz = "Australia/Melbourne")
  )

  muc_dawn <- cal_time_solar_dawn$day(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1, muc_dawn, tz = NULL)),
    as.POSIXct("2026-02-26 06:29:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, muc_dawn, tz = NULL)),
    as.POSIXct("2026-08-19 05:39:00", tz = "Europe/Berlin")
  )
})

test_that("Solar dusk calculations (civil twilight)", {
  mel_dusk <- cal_time_solar_dusk$day(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1, mel_dusk, tz = NULL)),
    as.POSIXct("2026-02-26 20:31:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, mel_dusk, tz = NULL)),
    as.POSIXct("2026-08-19 18:14:00", tz = "Australia/Melbourne")
  )

  muc_dusk <- cal_time_solar_dusk$day(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1, muc_dusk, tz = NULL)),
    as.POSIXct("2026-02-26 18:24:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, muc_dusk, tz = NULL)),
    as.POSIXct("2026-08-19 20:53:00", tz = "Europe/Berlin")
  )
})

test_that("Solar midnight calculations", {
  mel_midnight <- cal_time_solar_midnight$day(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1, mel_midnight, tz = NULL)),
    as.POSIXct("2026-02-26 01:33:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, mel_midnight, tz = NULL)),
    as.POSIXct("2026-08-19 00:23:00", tz = "Australia/Melbourne")
  )

  muc_midnight <- cal_time_solar_midnight$day(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1, muc_midnight, tz = NULL)),
    as.POSIXct("2026-02-27 00:26:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2, muc_midnight, tz = NULL)),
    as.POSIXct("2026-08-20 01:17:00", tz = "Europe/Berlin")
  )
})
