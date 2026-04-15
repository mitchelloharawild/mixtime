test_that("time_floor/time_ceiling/time_round work for hourly linear_time with 6-hour granule", {
  # hour 13 since epoch (1970-01-01 13h) -- 1/3 of the way into a 6-hour block
  h13 <- linear_time(13L, cal_gregorian$hour(1L))
  h12 <- linear_time(12L, cal_gregorian$hour(1L))
  h18 <- linear_time(18L, cal_gregorian$hour(1L))

  expect_equal(time_floor(h13,   granule = cal_gregorian$hour(6L)), h12)
  expect_equal(time_ceiling(h13, granule = cal_gregorian$hour(6L)), h18)
  expect_equal(time_round(h13,   granule = cal_gregorian$hour(6L)), h12) # closer to 12 than 18

  # hour 16 -- past the midpoint, so round goes up
  h16 <- linear_time(16L, cal_gregorian$hour(1L))
  expect_equal(time_floor(h16,   granule = cal_gregorian$hour(6L)), h12)
  expect_equal(time_ceiling(h16, granule = cal_gregorian$hour(6L)), h18)
  expect_equal(time_round(h16,   granule = cal_gregorian$hour(6L)), h18) # closer to 18
})

test_that("time_floor/time_ceiling/time_round on exact 6-hour boundary", {
  # hour 12 is exactly on a 6-hour boundary
  h12 <- linear_time(12L, cal_gregorian$hour(1L))
  h18 <- linear_time(18L, cal_gregorian$hour(1L))

  expect_equal(time_floor(h12,   granule = cal_gregorian$hour(6L)), h12)
  # ceiling always advances an exact boundary (per implementation intent)
  expect_equal(time_ceiling(h12, granule = cal_gregorian$hour(6L)), h18)
  expect_equal(time_round(h12,   granule = cal_gregorian$hour(6L)), h12)
})

test_that("time_floor/time_ceiling/time_round work for hourly linear_time with 1-hour granule", {
  # Rounding to the same chronon granule is an identity for floor/round;
  # ceiling advances exact boundaries.
  h13 <- linear_time(13L, cal_gregorian$hour(1L))
  h14 <- linear_time(14L, cal_gregorian$hour(1L))

  expect_equal(time_floor(h13,   granule = cal_gregorian$hour(1L)), h13)
  expect_equal(time_ceiling(h13, granule = cal_gregorian$hour(1L)), h14) # exact → advances
  expect_equal(time_round(h13,   granule = cal_gregorian$hour(1L)), h13)
})

test_that("time_floor/time_ceiling/time_round work for yearmonth with quarter granule", {
  # Feb 2020 is the 2nd month of Q1 2020
  ym_feb <- yearmonth(as.Date("2020-02-01"))
  q1_2020 <- yearmonth(as.Date("2020-01-01"))  # month 600 since epoch
  q2_2020 <- yearmonth(as.Date("2020-04-01"))  # month 603

  expect_equal(time_floor(ym_feb,   granule = cal_gregorian$quarter(1L)), q1_2020)
  expect_equal(time_ceiling(ym_feb, granule = cal_gregorian$quarter(1L)), q2_2020)
  expect_equal(time_round(ym_feb,   granule = cal_gregorian$quarter(1L)), q1_2020)

  # Aug 2020 (month 607): 607/3 = 202.33 — rounds down to Q3 (Jul)
  ym_aug <- yearmonth(as.Date("2020-08-01"))
  q3_2020 <- yearmonth(as.Date("2020-07-01"))  # month 606
  q4_2020 <- yearmonth(as.Date("2020-10-01"))  # month 609

  expect_equal(time_floor(ym_aug,   granule = cal_gregorian$quarter(1L)), q3_2020)
  expect_equal(time_ceiling(ym_aug, granule = cal_gregorian$quarter(1L)), q4_2020)
  expect_equal(time_round(ym_aug,   granule = cal_gregorian$quarter(1L)), q3_2020)

  # Sep 2020 (month 608): 608/3 = 202.67 — rounds up to Q4 (Oct)
  ym_sep <- yearmonth(as.Date("2020-09-01"))
  expect_equal(time_floor(ym_sep,   granule = cal_gregorian$quarter(1L)), q3_2020)
  expect_equal(time_ceiling(ym_sep, granule = cal_gregorian$quarter(1L)), q4_2020)
  expect_equal(time_round(ym_sep,   granule = cal_gregorian$quarter(1L)), q4_2020)
})

test_that("time_floor/time_ceiling/time_round work for yearmonth with year granule", {
  # Jun 2020 (month 605): 605/12 = 50.42 — floors to year 50 (2020)
  ym_jun <- yearmonth(as.Date("2020-06-01"))
  yr_2020 <- yearmonth(as.Date("2020-01-01"))  # month 600
  yr_2021 <- yearmonth(as.Date("2021-01-01"))  # month 612

  expect_equal(time_floor(ym_jun,   granule = cal_gregorian$year(1L)), yr_2020)
  expect_equal(time_ceiling(ym_jun, granule = cal_gregorian$year(1L)), yr_2021)
  expect_equal(time_round(ym_jun,   granule = cal_gregorian$year(1L)), yr_2020) # < 6 months past Jan

  # Aug 2020 (month 607): 607/12 = 50.58 — rounds up to 2021
  ym_aug <- yearmonth(as.Date("2020-08-01"))
  expect_equal(time_floor(ym_aug,   granule = cal_gregorian$year(1L)), yr_2020)
  expect_equal(time_ceiling(ym_aug, granule = cal_gregorian$year(1L)), yr_2021)
  expect_equal(time_round(ym_aug,   granule = cal_gregorian$year(1L)), yr_2021) # > 6 months past Jan
})

test_that("time_floor/time_ceiling/time_round work for yearquarter with year granule", {
  # Q2 2020 = quarter 201 since epoch; 201/4 = 50.25, floors to year 2020
  yq_q2 <- yearquarter(as.Date("2020-05-01"))  # quarter 201
  yq_q1_2020 <- yearquarter(as.Date("2020-01-01"))  # quarter 200
  yq_q1_2021 <- yearquarter(as.Date("2021-01-01"))  # quarter 204

  expect_equal(time_floor(yq_q2,   granule = cal_gregorian$year(1L)), yq_q1_2020)
  expect_equal(time_ceiling(yq_q2, granule = cal_gregorian$year(1L)), yq_q1_2021)
  expect_equal(time_round(yq_q2,   granule = cal_gregorian$year(1L)), yq_q1_2020) # Q2 is closer to Q1
})

test_that("time_floor/time_ceiling/time_round handle NA in vectorised mt_linear input", {
  ym_vec <- yearmonth(c(as.Date("2020-02-01"), as.Date("2020-08-01"), NA))

  res_floor   <- time_floor(ym_vec,   granule = cal_gregorian$quarter(1L))
  res_ceiling <- time_ceiling(ym_vec, granule = cal_gregorian$quarter(1L))
  res_round   <- time_round(ym_vec,   granule = cal_gregorian$quarter(1L))

  expect_equal(res_floor[1],   yearmonth(as.Date("2020-01-01")))  # Feb → Q1 start
  expect_equal(res_floor[2],   yearmonth(as.Date("2020-07-01")))  # Aug → Q3 start
  expect_true(is.na(res_floor[3]))

  expect_equal(res_ceiling[1], yearmonth(as.Date("2020-04-01")))  # Feb → Q2 start
  expect_equal(res_ceiling[2], yearmonth(as.Date("2020-10-01")))  # Aug → Q4 start
  expect_true(is.na(res_ceiling[3]))

  expect_equal(res_round[1],   yearmonth(as.Date("2020-01-01")))  # Feb rounds to Q1
  expect_equal(res_round[2],   yearmonth(as.Date("2020-07-01")))  # Aug rounds to Q3
  expect_true(is.na(res_round[3]))
})

test_that("time_floor/time_ceiling/time_round handle vectorised hourly linear_time with NA", {
  h_vec <- linear_time(c(13L, 16L, NA), cal_gregorian$hour(1L))

  res_floor   <- time_floor(h_vec,   granule = cal_gregorian$hour(6L))
  res_ceiling <- time_ceiling(h_vec, granule = cal_gregorian$hour(6L))
  res_round   <- time_round(h_vec,   granule = cal_gregorian$hour(6L))

  expect_equal(res_floor[1],   linear_time(12L, cal_gregorian$hour(1L)))
  expect_equal(res_floor[2],   linear_time(12L, cal_gregorian$hour(1L)))
  expect_true(is.na(res_floor[3]))

  expect_equal(res_ceiling[1], linear_time(18L, cal_gregorian$hour(1L)))
  expect_equal(res_ceiling[2], linear_time(18L, cal_gregorian$hour(1L)))
  expect_true(is.na(res_ceiling[3]))

  expect_equal(res_round[1],   linear_time(12L, cal_gregorian$hour(1L)))  # 13 → 12
  expect_equal(res_round[2],   linear_time(18L, cal_gregorian$hour(1L)))  # 16 → 18
  expect_true(is.na(res_round[3]))
})


test_that("time_floor/time_ceiling/time_round handle vectorised hourly linear_time with timezones", {
  h_vec <- linear_time(c(440557:440581), cal_gregorian$hour(1L, tz = "Australia/Melbourne"))
  h_vec
  res_floor   <- time_floor(h_vec,   granule = cal_gregorian$day(1L))
  res_ceiling <- time_ceiling(h_vec, granule = cal_gregorian$day(1L))
  res_round   <- time_round(h_vec,   granule = cal_gregorian$day(1L))

  expect_equal(res_floor[1],   linear_time(12L, cal_gregorian$hour(1L)))
  expect_equal(res_floor[2],   linear_time(12L, cal_gregorian$hour(1L)))
  expect_true(is.na(res_floor[3]))

  expect_equal(res_ceiling[1], linear_time(18L, cal_gregorian$hour(1L)))
  expect_equal(res_ceiling[2], linear_time(18L, cal_gregorian$hour(1L)))
  expect_true(is.na(res_ceiling[3]))

  expect_equal(res_round[1],   linear_time(12L, cal_gregorian$hour(1L)))  # 13 → 12
  expect_equal(res_round[2],   linear_time(18L, cal_gregorian$hour(1L)))  # 16 → 18
  expect_true(is.na(res_round[3]))
})
test_that("time_round/time_floor/time_ceiling work for POSIXct with tz preservation", {
  t <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")

  # expected values computed via epoch arithmetic (seconds)
  expected_round <- as.POSIXct(round(as.numeric(t) / 60) * 60, origin = "1970-01-01", tz = "UTC")
  expected_floor <- as.POSIXct(floor(as.numeric(t) / 60) * 60, origin = "1970-01-01", tz = "UTC")
  expected_ceiling <- as.POSIXct(ceiling(as.numeric(t) / 60) * 60, origin = "1970-01-01", tz = "UTC")

  # using package minute unit object (if available) and character granule
  expect_equal(
    time_round(t, granule = cal_gregorian$minute(1L)), 
    expected_round
  )
  expect_equal(
    time_floor(t, granule = cal_gregorian$minute(1L)), 
    expected_floor
  )
  expect_equal(
    time_ceiling(t, granule = cal_gregorian$minute(1L)), 
    expected_ceiling
  )

  expect_equal(
    time_round(t, granule = "1 minute"),
    expected_round
  )
  expect_equal(
    time_floor(t, granule = "1 minute"),
    expected_floor
  )
  expect_equal(
    time_ceiling(t, granule = "1 minute"),
    expected_ceiling
  )

  # tz preserved
  res <- time_round(t, granule = "1 minute")
  expect_equal(attr(res, "tzone"), attr(t, "tzone"))
  expect_s3_class(res, "POSIXct")

  # vectorized inputs and NA preservation
  t2 <- as.POSIXct("2020-01-01 12:34:20", tz = "UTC")
  vec_in <- c(t, t2, NA)
  attr(vec_in, "tzone") <- attr(t, "tzone")
  out_round <- time_round(vec_in, granule = "1 minute")
  expect_equal(
    out_round[1], 
    expected_round
  )
  expect_equal(
    out_round[2], 
    as.POSIXct(round(as.numeric(t2) / 60) * 60, origin = "1970-01-01", tz = "UTC")
  )
  expect_true(is.na(out_round[3]))
})

test_that("time_floor/time_ceiling work for Date", {
  d <- as.Date("2020-01-15")

  first_of_month <- as.Date(format(d, "%Y-%m-01"))
  first_of_next_month <- seq(first_of_month, by = "1 month", length.out = 2)[2]

  expect_equal(
    time_floor(d, granule = cal_gregorian$month(1L)), 
    first_of_month
  )
  expect_equal(
    time_ceiling(d, granule = cal_gregorian$month(1L)), 
    first_of_next_month
  )

  expect_equal(
    time_floor(d, granule = "month"), 
    first_of_month
  )
  expect_equal(
    time_ceiling(d, granule = "month"), 
    first_of_next_month
  )

  # class preserved
  res_floor <- time_floor(d, granule = "month")
  expect_s3_class(res_floor, "Date")
})

