test_that("format() handles NA in linear time", {
  expect_equal(format(yearmonth(NA_real_)), "NA")
  expect_equal(format(date(NA_character_)), "NA")
})

test_that("format() handles NaN in linear time", {
  expect_equal(format(yearmonth(NaN)), "NaN")
})

test_that("format() handles Inf and -Inf in linear time", {
  expect_equal(format(yearmonth(Inf)),  "Inf")
  expect_equal(format(yearmonth(-Inf)), "-Inf")
})

test_that("format() handles mixed regular, NA, NaN and Inf values", {
  x <- yearmonth(c(0, NA, NaN, Inf, -Inf))
  result <- format(x)
  expect_equal(result[[1]], "1970 Jan 0.0%")
  expect_equal(result[[2]], "  NA")
  expect_equal(result[[3]], " NaN")
  expect_equal(result[[4]], " Inf")
  expect_equal(result[[5]], "-Inf")
})

test_that("format() handles NA in durations", {
  expect_equal(format(months(NA_integer_)), "NA")
  expect_equal(format(days(NA_real_)), "NA")
})

test_that("format() handles NaN in durations", {
  expect_equal(format(days(NaN)), "NaN")
})

test_that("format() handles Inf and -Inf in durations", {
  expect_equal(format(months(Inf)),  "Inf")
  expect_equal(format(months(-Inf)), "-Inf")
})

test_that("format() handles mixed special values in durations", {
  x <- months(c(1L, NA_integer_, 2L))
  result <- format(x)
  expect_equal(result[[1]], "1 month")
  expect_equal(result[[2]], "NA")
  expect_equal(result[[3]], "2 months")

  x_frac <- days(c(1.5, NaN, Inf, -Inf))
  result_frac <- format(x_frac)
  expect_equal(result_frac[[2]], " NaN")
  expect_equal(result_frac[[3]], " Inf")
  expect_equal(result_frac[[4]], "-Inf")
})

test_that("format() handles special values in cyclical time", {
  x <- yearmonth(c(0, NA, Inf))
  result <- format(x)
  expect_length(result, 3L)
  expect_equal(result[[2]], " NA")
  expect_equal(result[[3]], "Inf")
})

test_that("format() reports an unmatched granule in the format string", {
  # Granules are S7 scalars, so their size comes from @n rather than vec_data()
  expect_error(
    mt_unit_display(
      cal_gregorian$day(2L),
      units = list(cal_gregorian$day(1L)),
      parts = list(1L)
    ),
    "2 days"
  )
})

test_that("format() falls back to the generic mt_unit template for a naive-tz civil chronon", {
  # A common chronon between an untimezoned Date and a zoned datetime is
  # genuinely tz-naive; its calendar has no year/month, so the default
  # templates must not reference them (see tz-merge.md). Instead they defer
  # (via S7::super()) to the same generic mt_unit fallback used for any other
  # chronon without a bespoke format template.
  combined <- c(date(Sys.Date()), datetime(as.POSIXct("2015-01-01", tz = "UTC")))
  ch <- chronon_glb(combined)
  expect_true(is.na(tz_name(ch)))
  expect_null(time_calendar(ch)$year)

  naive_second <- vecvec::unvecvec(mixtime(as.numeric(combined), chronon = ch, discrete = FALSE))
  expect_no_error(out <- format(naive_second))
  expect_match(out, "^D[0-9]")
})

test_that("format() describes a time unit", {
  expect_identical(format(cal_gregorian$month(1L)), "1 month")
  expect_identical(format(cal_gregorian$minute(15L)), "15 minutes")
  expect_identical(format(cal_isoweek$week(1L)), "1 week")
})

test_that("format() of continuous time ignores floating-point error at unit boundaries", {
  # 03:30:00 AEDT, stored one ULP below the boundary (as a conversion from a
  # continuous day on a 23-hour day gives)
  tz <- "Australia/Melbourne"
  sec <- cal_gregorian$second(1L, tz = tz)
  x <- linear_time(1759595400 - 2.4e-7, chronon = sec, discrete = FALSE)
  expect_equal(format(x), "2025-10-05 03:30:00 0.0% AEDT")

  hr <- cal_gregorian$hour(1L, tz = "UTC")
  expect_equal(
    format(linear_time(488776.5 - 6e-11, chronon = hr, discrete = FALSE), "{frac(.time)}"),
    "50.0%"
  )
})

test_that("format() zero-pads microsecond and nanosecond chronons to 6 and 9 digits", {
  expect_equal(
    chronon_format_linear(cal_gregorian$microsecond(1L)),
    "{lin(year)}-{cyc(month,year)}-{cyc(day,month)} {cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}.{cyc(microsecond, second)}"
  )
  expect_equal(
    format(linear_time(1000056L, chronon = cal_gregorian$microsecond(1L))),
    "1970-01-01 00:00:01.000056"
  )
  expect_equal(
    format(linear_time(1000000056L, chronon = cal_gregorian$nanosecond(1L))),
    "1970-01-01 00:00:01.000000056"
  )
})

test_that("chronon_divmod() across several granules works on vectors", {
  res <- chronon_divmod(cal_gregorian$microsecond(1L), cal_gregorian$second(1L), c(1500000, 2, NA))
  expect_equal(res$div, c(1, 0, NA))
  expect_equal(res$mod, c(500000, 2, NA))
})
