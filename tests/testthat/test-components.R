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
  expect_true(all(is_time_linear(res$yr)))
  expect_identical(res$yr, year(t))
  expect_identical(res$ym, yearmonth(t))
})

test_that("cyc() extracts cyclical components matching the helper functions", {
  t <- yearmonth(as.Date("2026-02-14") + c(0, 40, 400))

  res <- time_components(t, m = cyc(month, year), q = cyc(quarter, year))

  expect_true(all(is_time_cyclical(res$m)))
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

test_that("columns follow the ... order and support automatic naming", {
  t <- yearmonth(as.Date("2026-02-14"))

  res <- time_components(t, cyc(month, year), yr = lin(year))

  expect_length(res, 2L)
  expect_identical(names(res)[[2L]], "yr")
  # First column auto-named from its expression.
  expect_match(names(res)[[1L]], "cyc\\(month, year\\)")
})

test_that("special values (NA) propagate to all components", {
  t <- yearmonth(c("2026-02-14", NA, "2027-03-01"))

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
  mx <- c(yearmonth("2026-02-14"), yearquarter("2026-08-01"))

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

test_that("time_components() errors informatively on misuse", {
  t <- yearmonth("2026-02-14")

  expect_error(time_components(t), "at least one component")
  expect_error(
    time_components(as.Date("2025-12-15"), cyc(day, week)),
    "week"
  )
})
