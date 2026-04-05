# Fixed reference: 2024-01-15 09:20:30 UTC
t_ref <- datetime(as.POSIXct("2024-01-15 09:20:30", tz = "UTC"))

# ---- Linear parts -----------------------------------------------------------

test_that("chronon_parts() returns correct linear parts", {
  # chronon_parts is internally applied to mt_time vectors, not mixed-type vectors
  x <- vecvec::unvecvec(t_ref)
  
  # Singular linear parts
  result <- chronon_parts(x, linear = list(cal_gregorian$second(1L)))
  expect_equal(result$linear[[1L]], 1705310430L)

  result <- chronon_parts(x, linear = list(cal_gregorian$year(1L)))
  expect_equal(result$linear[[1L]], 2024L)

  result <- chronon_parts(x, linear = list(cal_gregorian$day(1L)))
  expect_equal(result$linear[[1L]], 19737L)

  # Multiple linear parts together
  result <- chronon_parts(
    x,
    linear = list(
      cal_gregorian$second(1L),
      cal_gregorian$minute(1L),
      cal_gregorian$hour(1L),
      cal_gregorian$day(1L),
      cal_gregorian$month(1L),
      cal_gregorian$year(1L)
    )
  )
  expect_equal(result$linear[[1L]], 1705310430L) # total seconds
  expect_equal(result$linear[[2L]], 28421840L)   # total minutes
  expect_equal(result$linear[[3L]], 473697L)     # total hours
  expect_equal(result$linear[[4L]], 19737L)      # total days
  expect_equal(result$linear[[5L]], 648L)        # total months
  expect_equal(result$linear[[6L]], 2024L)       # year

  # No linear parts should return empty list
  result <- chronon_parts(x)
  expect_identical(result$linear, list())
})

# ---- Cyclical parts ---------------------------------------------------------

test_that("chronon_parts() returns correct cyclical parts", {
  # chronon_parts is internally applied to mt_time vectors, not mixed-type vectors
  x <- vecvec::unvecvec(t_ref)
  
  # Singular cyclical parts
  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$second(1L), cal_gregorian$minute(1L)))
  )
  expect_equal(result$cyclical[[1L]], 30L)

  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$minute(1L), cal_gregorian$hour(1L)))
  )
  expect_equal(result$cyclical[[1L]], 20L)

  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$hour(1L), cal_gregorian$day(1L)))
  )
  expect_equal(result$cyclical[[1L]], 9L)

  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$month(1L), cal_gregorian$year(1L)))
  )
  expect_equal(result$cyclical[[1L]], 0L)

  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$day(1L), cal_gregorian$month(1L)))
  )
  expect_equal(result$cyclical[[1L]], 14L)

  # Multiple cyclical parts together
  result <- chronon_parts(
    x,
    cyclical = list(
      list(cal_gregorian$second(1L), cal_gregorian$minute(1L)),
      list(cal_gregorian$minute(1L), cal_gregorian$hour(1L)),
      list(cal_gregorian$hour(1L), cal_gregorian$day(1L))
    )
  )
  expect_equal(result$cyclical[[1L]], 30L) # second-of-minute
  expect_equal(result$cyclical[[2L]], 20L) # minute-of-hour
  expect_equal(result$cyclical[[3L]], 9L)  # hour-of-day
})

# ---- Multi-step cyclical (e.g. second-of-hour, second-of-day) ---------------

test_that("chronon_parts() correctly computes multi-step cyclical parts", {
  # chronon_parts is internally applied to mt_time vectors, not mixed-type vectors
  x <- vecvec::unvecvec(t_ref)
  
  # 09:20:30 => second-of-hour = 20*60 + 30 = 1230
  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$second(1L), cal_gregorian$hour(1L)))
  )
  expect_equal(result$cyclical[[1L]], 1230L)

  # 09:20:30 => 9*3600 + 20*60 + 30 = 33630
  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$second(1L), cal_gregorian$day(1L)))
  )
  expect_equal(result$cyclical[[1L]], 33630L)

  # Both together
  result <- chronon_parts(
    x,
    cyclical = list(
      list(cal_gregorian$second(1L), cal_gregorian$minute(1L)),
      list(cal_gregorian$second(1L), cal_gregorian$hour(1L)),
      list(cal_gregorian$second(1L), cal_gregorian$day(1L))
    )
  )
  expect_equal(result$cyclical[[1L]], 30L)    # second-of-minute
  expect_equal(result$cyclical[[2L]], 1230L)  # second-of-hour
  expect_equal(result$cyclical[[3L]], 33630L) # second-of-day
})

# ---- Mixed linear and cyclical ----------------------------------------------
test_that("chronon_parts() returns correct linear and cyclical parts together", {
  # chronon_parts is internally applied to mt_time vectors, not mixed-type vectors
  x <- vecvec::unvecvec(t_ref)
  
  result <- chronon_parts(
    x,
    linear = list(cal_gregorian$year(1L), cal_gregorian$day(1L)),
    cyclical = list(
      list(cal_gregorian$hour(1L), cal_gregorian$day(1L)),
      list(cal_gregorian$minute(1L), cal_gregorian$hour(1L))
    )
  )
  expect_equal(result$linear[[1L]], 2024L)  # year
  expect_equal(result$linear[[2L]], 19737L) # total days
  expect_equal(result$cyclical[[1L]], 9L)   # hour-of-day
  expect_equal(result$cyclical[[2L]], 20L)  # minute-of-hour
})

# ---- Timezones --------------------------------------------------------------

test_that("chronon_parts() applies timezone offsets to granules", {
  # 2024-01-15 09:20:30 UTC = 2024-01-15 20:20:30 AEDT
  x <- vecvec::unvecvec(datetime(t_ref, tz = "Australia/Melbourne"))
  result <- chronon_parts(
    x,
    cyclical = list(list(cal_gregorian$hour(1L), cal_gregorian$day(1L)))
  )
  expect_equal(result$cyclical[[1L]], 20L)

  # 2024-01-14 20:20:00 UTC = 2024-01-14 15:20:00 EST (UTC-5) = 2024-01-15 07:20:00 AEDT (UTC+11)
  t <- as.POSIXct("2024-01-14 20:20:00", tz = "UTC")
  x_utc <- vecvec::unvecvec(datetime(t, tz = "UTC"))
  x_est <- vecvec::unvecvec(datetime(t, tz = "America/New_York"))
  x_melb <- vecvec::unvecvec(datetime(t, tz = "Australia/Melbourne"))

  r_est <- chronon_parts(x_est, cyclical = list(
    list(cal_gregorian$minute(1L), cal_gregorian$hour(1L)),
    list(cal_gregorian$hour(1L), cal_gregorian$day(1L)),
    list(cal_gregorian$day(1L), cal_gregorian$month(1L))
  ))
  r_utc <- chronon_parts(x_utc, cyclical = list(
    list(cal_gregorian$minute(1L), cal_gregorian$hour(1L)),
    list(cal_gregorian$hour(1L), cal_gregorian$day(1L)),
    list(cal_gregorian$day(1L), cal_gregorian$month(1L))
  ))
  r_melb <- chronon_parts(x_melb, cyclical = list(
    list(cal_gregorian$minute(1L), cal_gregorian$hour(1L)),
    list(cal_gregorian$hour(1L), cal_gregorian$day(1L)),
    list(cal_gregorian$day(1L), cal_gregorian$month(1L))
  ))

  # Minute-of-hour should be the same across timezones since it's a sub-hour unit
  expect_equal(r_utc$cyclical[[1L]], 20L)
  expect_equal(r_est$cyclical[[1L]], 20L)
  expect_equal(r_melb$cyclical[[1L]], 20L)

  expect_equal(r_utc$cyclical[[2L]], 20L) # UTC: 20h on same day
  expect_equal(r_est$cyclical[[2L]], 15L) # EST: 15h on same day
  expect_equal(r_melb$cyclical[[2L]], 7L) # AEDT: 07h next day

  expect_equal(r_utc$cyclical[[3L]], 13L) # UTC: 14th day of month
  expect_equal(r_est$cyclical[[3L]], 13L) # EST: 14th day of month
  expect_equal(r_melb$cyclical[[3L]], 14L) # AEDT: 15th day of month
})

# ---- Vectorised inputs ------------------------------------------------------

test_that("chronon_parts() handles vectorised inputs", {
  t_mult <- as.POSIXct(
    c("2024-01-15 09:20:30", "2024-06-21 15:45:00"),
    tz = "UTC"
  )
  x_mult <- vecvec::unvecvec(datetime(t_mult, tz = "UTC"))

  result <- chronon_parts(
    x_mult,
    cyclical = list(
      list(cal_gregorian$hour(1L), cal_gregorian$day(1L)),
      list(cal_gregorian$second(1L), cal_gregorian$hour(1L))
    )
  )
  
  # Should return both hours: 9 (09:20:30) and 15 (15:45:00)
  expect_equal(result$cyclical[[1L]], c(9L, 15L))

  # 09:20:30 => 20*60+30 = 1230; 15:45:00 => 45*60+0 = 2700
  expect_equal(result$cyclical[[2L]], c(1230L, 2700L))

  t_mult <- as.POSIXct(
    c("2024-01-15 09:20:30", "2024-06-21 15:45:00", "2024-12-31 23:59:59"),
    tz = "UTC"
  )
  x_mult <- vecvec::unvecvec(datetime(t_mult, tz = "UTC"))
  result <- chronon_parts(
    x_mult,
    cyclical = list(
      list(cal_gregorian$hour(1L), cal_gregorian$day(1L)),
      list(cal_gregorian$minute(1L), cal_gregorian$hour(1L)),
      list(cal_gregorian$second(1L), cal_gregorian$minute(1L))
    )
  )

  expect_equal(result$cyclical[[1L]], c(9L, 15L, 23L))    # hour-of-day
  expect_equal(result$cyclical[[2L]], c(20L, 45L, 59L))   # minute-of-hour
  expect_equal(result$cyclical[[3L]], c(30L, 0L, 59L))    # second-of-minute

  t_mult <- as.POSIXct(
    c("2020-03-01 00:00:00", "2023-07-04 12:00:00", "2024-01-15 09:20:30"),
    tz = "UTC"
  )
  x_mult <- vecvec::unvecvec(datetime(t_mult, tz = "UTC"))
  result <- chronon_parts(
    x_mult,
    linear = list(cal_gregorian$year(1L))
  )
  expect_equal(result$linear[[1L]], c(2020L, 2023L, 2024L))
})
