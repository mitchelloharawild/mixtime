# Tests for time_components(): mutate-like extraction of linear (lin()) and
# cyclical (cyc()) components from time vectors.
#
# A linear and a cyclical component of the same granule store the same value
# (the chronon count since the Unix epoch); the difference is only in how a
# cyclical vector is formatted. So a lin()/cyc() component must be identical to
# the corresponding linear_time()/cyclical_time() helper.

test_that("lin() extracts linear components matching the helper functions", {
  t <- yearmonth(as.Date("2026-02-14") + c(0, 40, 400))

  res <- time_components(t, yr = lin(year), ym = lin(month))

  expect_s3_class(res, "data.frame")
  expect_named(res, c("yr", "ym"))
  expect_true(all(time_is_linear(res$yr)))
  expect_identical(res$yr, year(t))
  expect_identical(res$ym, yearmonth(t))
})

test_that("cyc() extracts cyclical components matching the helper functions", {
  t <- yearmonth(as.Date("2026-02-14") + c(0, 40, 400))

  res <- time_components(t, m = cyc(month, year), q = cyc(quarter, year))

  expect_true(all(time_is_cyclical(res$m)))
  expect_identical(res$m, month_of_year(t))
  expect_identical(res$q, cyclical_time(t, chronon = quarter(1L), cycle = year(1L)))
})

test_that("cyc() with an alternate calendar (ISO week) works", {
  d <- as.Date("2025-12-15") + 0:6  # Mon..Sun

  res <- time_components(d, dow = cyc(day, week), calendar = cal_isoweek)

  expect_identical(res$dow, day_of_week(d))
  expect_equal(format(res$dow), format(day_of_week(d)))
})

test_that("time-of-day components extract from a datetime (second chronon)", {
  dt <- datetime(as.POSIXct(c("2026-02-14 06:30:00", "2026-02-14 18:45:00"), tz = "UTC"))

  res <- time_components(dt, tod = cyc(second, day), h = cyc(hour, day))

  expect_identical(res$tod, time_of_day(dt))
  expect_equal(format(res$h), c("h06", "h18"))
})

test_that("components of a time zone aware time apply its UTC offset once", {
  tz <- "Australia/Melbourne"
  # 20:00 UTC is 07:00 AEDT (+11, January) or 06:00 AEST (+10, July) on the
  # next local day; 06:00 UTC is 17:00 AEDT on the same day.
  utc <- as.POSIXct(
    c("2026-01-15 20:00:00", "2026-07-15 20:00:00", "2026-01-15 06:00:00"),
    tz = "UTC"
  )
  dt <- datetime(datetime(utc), tz = tz)

  res <- time_components(
    dt,
    hr = lin(hour), h = cyc(hour, day), dy = lin(day), d = cyc(day, month),
    wd = cyc(day, week),
    calendar = cal_isoweek
  )

  # Sub-daily cycle
  expect_equal(format(res$h), c("h07 AEDT", "h06 AEST", "h17 AEDT"))
  expect_identical(
    res$h,
    cyclical_time(dt, chronon = hour(1L, tz = tz), cycle = day(1L, tz = tz))
  )
  expect_identical(res$hr, linear_time(dt, chronon = hour(1L, tz = tz)))

  # Day-level cycles across the local/UTC day boundary
  expect_equal(format(res$d), c("D16 AEDT", "D16 AEST", "D15 AEDT"))
  expect_equal(format(res$wd), c("Fri AEDT", "Thu AEST", "Thu AEDT"))
  expect_identical(res$dy, linear_time(dt, chronon = day(1L, tz = tz)))

  # The UTC times when clocks go back (02:30 AEDT, then 02:30 AEST) are
  # distinct hours
  dt <- datetime(
    datetime(as.POSIXct(c("2026-04-04 15:30:00", "2026-04-04 16:30:00"), tz = "UTC")),
    tz = tz
  )
  res <- time_components(dt, hr = lin(hour), h = cyc(hour, day))
  expect_equal(format(res$h), c("h02 AEDT", "h02 AEST"))
  expect_identical(res$hr, linear_time(dt, chronon = hour(1L, tz = tz)))

  # A half-hour offset shifts the hour and minute
  dt <- datetime(
    datetime(as.POSIXct("2026-07-15 20:45:00", tz = "UTC")),
    tz = "Australia/Adelaide"
  )
  res <- time_components(dt, h = cyc(hour, day), m = cyc(minute, hour))
  expect_equal(format(res$h), "h06 ACST")
  expect_equal(format(res$m), "m15 ACST")
})

test_that("columns follow the ... order and support automatic naming", {
  t <- yearmonth(as.Date("2026-02-14"))

  res <- time_components(t, cyc(month, year), yr = lin(year))

  expect_length(res, 2L)
  expect_identical(names(res)[[2L]], "yr")
  # First column auto-named from its expression.
  expect_match(names(res)[[1L]], "cyc\\(month, year\\)")
})

test_that("special values (NA) propagate to all components", {
  t <- yearmonth(date(c("2026-02-14", NA, "2027-03-01")))

  res <- time_components(t, yr = lin(year), m = cyc(month, year))

  expect_true(is.na(res$yr[2L]))
  expect_true(is.na(res$m[2L]))
  expect_equal(format(res$yr), c("2026", "NA", "2027"))
})

test_that("Inf inputs give infinite linear and NA cyclical components", {
  t <- yearmonth(c(0, NA, Inf, -Inf))

  res <- time_components(t, yr = lin(year), m = cyc(month, year))

  # Linear: an infinite time is an infinite count; NA stays NA.
  expect_identical(is.na(res$yr), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(trimws(format(res$yr)), c("1970 0.0%", "NA", "Inf", "-Inf"))

  # Cyclical: no cyclical position for a special value, so NA (incl. Inf).
  expect_true(all(is.na(res$m[2:4])))
})

test_that("mixed-granularity mixtimes decompose each part correctly", {
  mx <- c(yearmonth(date("2026-02-14")), yearquarter(date("2026-08-01")))

  res <- time_components(mx, yr = lin(year), q = cyc(quarter, year))

  expect_equal(format(res$yr), format(year(mx)))
  expect_equal(format(res$q), c("Q1", "Q3"))
})

test_that("discrete components finer than the input chronon are NA", {
  # Discrete (integer) year(2020L) has no determinate month.
  mx <- c(year(2020L), yearmonth(as.Date("2020-02-01")))

  res <- time_components(mx, yr = lin(year), mth = cyc(month, year), m = lin(month))

  # Month components are NA for the year(); the yearmonth() resolves to February.
  expect_identical(is.na(res$mth), c(TRUE, FALSE))
  expect_identical(is.na(res$m), c(TRUE, FALSE))
  expect_equal(format(res$mth), c("NA", "Feb"))

  # The coarser (year) component is determinable for both.
  expect_false(any(is.na(res$yr)))
  expect_equal(format(res$yr), c("2020", "2020"))
})

test_that("continuous time resolves finer components exactly (no NA)", {
  # Continuous (double) year(2020) is 0% of the way through 2020, i.e. the start
  # of January -> the month is determinate, not NA.
  res <- time_components(year(2020), mth = cyc(month, year), m = lin(month))

  expect_false(is.na(res$mth))
  expect_false(is.na(res$m))
  expect_equal(format(res$mth), "Jan 0.0%")
})

test_that("components are extracted at multi-unit granule sizes", {
  d <- as.Date(c("2023-06-21", "2024-06-21", "2025-06-21", "2026-06-21"))

  # Two-year granules: the calendar's own day -> year divmod converts to single
  # years only, so the size is resolved by rescaling from the year granule.
  res <- time_components(d, y = lin(cal_gregorian$year(2L)))
  expect_equal(format(res$y), c("2022", "2024", "2024", "2026"))

  # Quarters expressed as three months are counted in quarters, not months
  res <- time_components(d, q = lin(cal_gregorian$month(3L)))
  expect_equal(
    vctrs::vec_data(vecvec::unvecvec(res$q)),
    ((2023:2026 - 1970) * 12L + 5L) %/% 3L
  )
})

test_that("time_components() errors informatively on misuse", {
  t <- yearmonth(date("2026-02-14"))

  expect_error(time_components(t), "at least one component")
  expect_error(
    time_components(as.Date("2025-12-15"), cyc(day, week)),
    "week"
  )
})
