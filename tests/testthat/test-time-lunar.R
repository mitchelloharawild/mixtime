# Lunation 1276: new moon 2026-02-17 12:03 UTC
# Lunation 1281: new moon 2026-07-14 09:45 UTC
# Tolerance of 5 minutes (~300s) for approximate lunation algorithm

expect_equal_time <- function(object, expected, tolerance = 300, ...) {
  expect_equal(as.numeric(object), as.numeric(expected), tolerance = tolerance, ...)
}

test_that("New moon times (synodic month)", {
  # Lunation 1276: 2026-02-17 ~12:03 UTC (new moon)
  expect_equal_time(
    as.POSIXct(linear_time(1276L, cal_time_lunar$month(1L)), tz = "UTC"),
    as.POSIXct("2026-02-17 12:03:00", tz = "UTC")
  )
  # Local timezone representations of the same instant
  expect_equal_time(
    as.POSIXct(linear_time(1276L, cal_time_lunar$month(1L))),
    as.POSIXct("2026-02-17 23:03:19", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1276L, cal_time_lunar$month(1L))),
    as.POSIXct("2026-02-17 13:03:19", tz = "Europe/Berlin")
  )

  # Lunation 1281: 2026-07-14 ~09:45 UTC (new moon)
  expect_equal_time(
    as.POSIXct(linear_time(1281L, cal_time_lunar$month(1L)), tz = "UTC"),
    as.POSIXct("2026-07-14 09:45:00", tz = "UTC")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1281L, cal_time_lunar$month(1L))),
    as.POSIXct("2026-07-14 19:45:18", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1281L, cal_time_lunar$month(1L))),
    as.POSIXct("2026-07-14 11:45:18", tz = "Europe/Berlin")
  )
})

test_that("Full moon times (synodic phase index 4)", {
  # Lunation 1276: full moon ~2026-03-04 06:44 UTC
  expect_equal_time(
    as.POSIXct(linear_time(1276L * 8 + 4, cal_time_lunar$phase(1L)), tz = "UTC"),
    as.POSIXct("2026-03-04 06:44:00", tz = "UTC")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1276L * 8 + 4, cal_time_lunar$phase(1L))),
    as.POSIXct("2026-03-04 17:44:34", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1276L * 8 + 4, cal_time_lunar$phase(1L))),
    as.POSIXct("2026-03-04 07:44:34", tz = "Europe/Berlin")
  )

  # Lunation 1281: full moon ~2026-07-29 01:41 UTC
  expect_equal_time(
    as.POSIXct(linear_time(1281L * 8 + 4, cal_time_lunar$phase(1L)), tz = "UTC"),
    as.POSIXct("2026-07-29 01:41:00", tz = "UTC")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1281L * 8 + 4, cal_time_lunar$phase(1L))),
    as.POSIXct("2026-07-29 11:41:40", tz = "Australia/Melbourne")
  )
  expect_equal_time(
    as.POSIXct(linear_time(1281L * 8 + 4, cal_time_lunar$phase(1L))),
    as.POSIXct("2026-07-29 03:41:40", tz = "Europe/Berlin")
  )
})

test_that("Consecutive new moons are ~29.5 days apart", {
  posix_times <- as.POSIXct(linear_time(320:330, cal_time_lunar$month(1L)))
  gaps_days <- as.numeric(diff(posix_times), units = "days")
  expect_true(all(gaps_days > 29.2 & gaps_days < 29.9))
})
