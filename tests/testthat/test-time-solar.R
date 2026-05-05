date1 <- as.integer(as.Date("2026-02-26"))
date2 <- as.integer(as.Date("2026-08-19"))
expect_equal_time <- function(object, expected, tolerance = 120, ...) {
  expect_equal(as.numeric(object), as.numeric(expected), tolerance = tolerance, ...)
}

test_that("Solar sunrise calculations", {
  # Sunrise = illumination phase 4 (start of day phase)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, mel_illumination)),
    as.POSIXct("2026-02-26 07:01:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, mel_illumination)),
    as.POSIXct("2026-08-19 07:00:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 4L, muc_illumination)),
    as.POSIXct("2026-02-26 07:00:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 4L, muc_illumination)),
    as.POSIXct("2026-08-19 06:13:00", tz = "Europe/Berlin")
  )
})

test_that("Solar sunset calculations", {
  # Sunset = illumination phase 5 (start of civil dusk phase)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, mel_illumination)),
    as.POSIXct("2026-02-26 20:04:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, mel_illumination)),
    as.POSIXct("2026-08-19 17:47:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 5L, muc_illumination)),
    as.POSIXct("2026-02-26 17:53:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 5L, muc_illumination)),
    as.POSIXct("2026-08-19 20:20:00", tz = "Europe/Berlin")
  )
})

test_that("Solar noon calculations", {
  # Noon = ampm half 1 (start of PM half)
  mel_ampm <- cal_time_solar$ampm(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 2L + 1L, mel_ampm)),
    as.POSIXct("2026-02-26 13:33:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 2L + 1L, mel_ampm)),
    as.POSIXct("2026-08-19 12:23:00", tz = "Australia/Melbourne")
  )

  muc_ampm <- cal_time_solar$ampm(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 2L + 1L, muc_ampm)),
    as.POSIXct("2026-02-26 12:26:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 2L + 1L, muc_ampm)),
    as.POSIXct("2026-08-19 13:17:00", tz = "Europe/Berlin")
  )
})

test_that("Solar dawn calculations (civil twilight)", {
  # Civil dawn = illumination phase 3 (start of civil dawn phase)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, mel_illumination)),
    as.POSIXct("2026-02-26 06:34:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, mel_illumination)),
    as.POSIXct("2026-08-19 06:33:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 3L, muc_illumination)),
    as.POSIXct("2026-02-26 06:29:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 3L, muc_illumination)),
    as.POSIXct("2026-08-19 05:39:00", tz = "Europe/Berlin")
  )
})

test_that("Solar dusk calculations (civil twilight)", {
  # Civil dusk = illumination phase 6 (start of nautical dusk phase; sun crosses -6° descending)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 6L, mel_illumination)),
    as.POSIXct("2026-02-26 20:31:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 6L, mel_illumination)),
    as.POSIXct("2026-08-19 18:14:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 6L, muc_illumination)),
    as.POSIXct("2026-02-26 18:24:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 6L, muc_illumination)),
    as.POSIXct("2026-08-19 20:53:00", tz = "Europe/Berlin")
  )
})

test_that("Solar midnight calculations", {
  # Midnight = illumination phase 0 (start of night phase / solar anti-transit)
  mel_illumination <- cal_time_solar$illumination(1L, lat = -37.8136, lon = 144.9631)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, mel_illumination)),
    as.POSIXct("2026-02-26 01:33:00", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, mel_illumination)),
    as.POSIXct("2026-08-19 00:23:00", tz = "Australia/Melbourne")
  )

  muc_illumination <- cal_time_solar$illumination(1L, lat = 48.133, lon = 11.583)
  expect_equal_time(
    as.POSIXct(linear_time(date1 * 8L + 0L, muc_illumination)),
    as.POSIXct("2026-02-27 00:26:00", tz = "Europe/Berlin")
  )
  expect_equal_time(
    as.POSIXct(linear_time(date2 * 8L + 0L, muc_illumination)),
    as.POSIXct("2026-08-20 01:17:00", tz = "Europe/Berlin")
  )
})
