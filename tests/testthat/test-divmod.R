test_that("fdiv() and fdivmod() agree with %/% and %%", {
  x <- as.numeric(c(-1000:1000, -2^40, 2^40, -719468L, 719468L))

  for (y in c(1, 2, 5L, 7L, 12L, 60L, 146097L, 0.5, 1.5)) {
    expect_equal(fdiv(x, y), x %/% y, info = paste("divisor", y))

    dm <- fdivmod(x, y)
    expect_equal(dm$div, x %/% y, info = paste("divisor", y))
    expect_equal(dm$mod, x %% y, info = paste("divisor", y))
  }

  # Vectorised divisors (e.g. a context-dependent cardinality per element)
  y <- rep_len(c(28, 29, 30, 31), length(x))
  expect_equal(fdiv(x, y), x %/% y)
  expect_equal(fdivmod(x, y)$mod, x %% y)
})

test_that("coarse-to-fine conversion takes a monotone path", {
  quarter <- cal_gregorian$quarter(1L)
  day <- cal_gregorian$day(1L)

  # Routing quarters to days via years would make a quarter an even 1/4 of the
  # year (91.5 days in 2020) rather than the 91 days of January to March.
  path <- S7_graph_dispatch(chronon_divmod_graph(), quarter, day)
  expect_equal(
    vapply(path, function(unit) class(unit(1L))[[1L]], character(1L)),
    c("mixtime::tu_quarter", "mixtime::tu_month", "mixtime::tu_day")
  )

  # Quarter boundaries land exactly on the day they begin.
  starts <- as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"))
  expect_equal(
    chronon_convert_impl(200:203, quarter, day, discrete = FALSE),
    as.numeric(starts)
  )
  # Interpolating within the granule is not the same as splitting the interval
  # it spans: half a quarter is 1.5 months, which is only the middle of the
  # quarter when its months are of equal length. Q3 2020 spans 92 days, so its
  # midpoint is 46 days in, while mid-August is a day-and-a-half later. This is
  # why the ordering proxy takes the mean of the two bounds instead.
  expect_equal(chronon_convert_impl(202.5, quarter, day, discrete = FALSE), 18490.5)
  expect_equal(mean(chronon_convert_impl(202:203, quarter, day, discrete = FALSE)), 18490)
})

test_that("units with no monotone path still convert", {
  # A week is neither finer nor coarser than a month, so the unrestricted
  # search is still needed to route between them (via days).
  expect_no_error(
    chronon_convert_impl(2611L, cal_isoweek$week(1L), cal_gregorian$month(1L), discrete = TRUE)
  )
})
