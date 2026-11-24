# mt_cyclical_free: cyclical time without knowledge of which cycle it is in.

# ---------------------------------------------------------------------------
# Construction
# ---------------------------------------------------------------------------

test_that("bare numeric input to a cyclical helper builds mt_cyclical_free, not mt_cyclical", {
  x <- day_of_year(60)
  expect_true(time_is_cyclical_free(x))
  expect_false(time_is_cyclical(x))
  expect_true(S7::S7_inherits(x@x[[1L]], mt_cyclical_free))
})

test_that("Date/POSIXct input still builds a located mt_cyclical", {
  expect_true(time_is_cyclical(day_of_year(as.Date("2020-01-01"))))
})

test_that("an irregular chronon/cycle pair is bounded by its largest cycle", {
  # day/year varies (365/366), so the bound is the longest year.
  expect_equal(format(day_of_year(366)), "D366")
  expect_true(is.na(day_of_year(367)))
  expect_true(is.na(day_of_year(1000)))
  expect_true(is.na(day_of_year(0)))

  # 28-31 days in a month, 52-53 ISO weeks in a year.
  expect_equal(format(day_of_month(31)), "D31")
  expect_true(is.na(day_of_month(32)))
  expect_equal(format(week_of_year(53)), "W53")
  expect_true(is.na(week_of_year(54)))
})

test_that("a fixed chronon/cycle pair rejects out-of-range bare input as NA", {
  # month/year is fixed at 12 months.
  expect_true(is.na(month_of_year(0)))
  expect_true(is.na(month_of_year(13)))
  expect_false(is.na(month_of_year(1)))
  expect_false(is.na(month_of_year(12)))

  # day/week (7 days) and second/day (86400 seconds) are likewise fixed.
  expect_true(is.na(day_of_week(0)))
  expect_true(is.na(day_of_week(8)))
  expect_true(is.na(time_of_day(86400)))
  expect_false(is.na(time_of_day(0)))
})

test_that("a free value's indexing follows the chronon's own registered label scheme", {
  # day/month/year register a 1-indexed ("everyday counting") scheme.
  expect_equal(format(day_of_year(60)), "D60")
  expect_equal(format(month_of_year(3)), "Mar")
  expect_equal(
    time_compose(cyc(month, year) ~ 3)@x[[1L]],
    month_of_year(3)@x[[1L]]
  )

  # time-of-day starts at 0: time_of_day(0) is midnight, not "the 1st second".
  expect_equal(format(time_of_day(0)), "00:00:00")
})

test_that("time_compose()/time_parse() with no lin() component build mt_cyclical_free", {
  expect_true(time_is_cyclical_free(time_compose(cyc(month, year) ~ 3)))
  expect_true(time_is_cyclical_free(time_parse("Feb", format = "{cyc(month, year, label = TRUE)}")))
})

# ---------------------------------------------------------------------------
# Comparison: magnitude, not interval (mirrors mt_duration-compare)
# ---------------------------------------------------------------------------

test_that("free values compare as plain magnitudes in a shared cycle", {
  expect_true(day_of_year(60) == day_of_year(60))
  expect_true(day_of_year(60) < day_of_year(61))
  expect_false(day_of_year(60) > day_of_year(61))
  expect_true(day_of_year(60) != day_of_year(61))
})

test_that("free values of differing chronons compare in their common chronon", {
  # hour is unshifted (offset 24); day 2 (1-indexed) is offset 1, i.e. 24 hours.
  hoy <- cyclical_time(24, hour(1L), year(1L))
  doy <- day_of_year(2)
  expect_true(hoy == doy)
})

test_that("free values of differing cycles can't be compared", {
  dow <- day_of_week(3)
  doy <- day_of_year(60)
  expect_error(dow == doy, class = "vctrs_error_incompatible_op")
})

test_that("comparing chronons that require `at` errors, since a free value never has one", {
  expect_error(
    day_of_year(60) == month_of_year(3),
    "requires the time context"
  )
})

test_that("Allen's interval algebra is not supported for mt_cyclical_free (matches mt_duration)", {
  expect_error(day_of_year(60) %p% day_of_year(61))
})

# ---------------------------------------------------------------------------
# Comparison against a located mt_cyclical
# ---------------------------------------------------------------------------

test_that("a free value compares against a located mt_cyclical by magnitude", {
  expect_true(day_of_year(60) == day_of_year(as.Date("2020-02-29")))
  expect_true(month_of_year(3) == month_of_year(as.Date("2020-03-15")))
  expect_false(day_of_year(61) == day_of_year(as.Date("2020-02-29")))
})

# ---------------------------------------------------------------------------
# Compositional disambiguation (no MonthDay-style compound type needed)
# ---------------------------------------------------------------------------

test_that("exact Feb 29 match is recovered by intersecting free predicates", {
  t <- day_of_month(as.Date("2020-02-29"))
  expect_true(t == day_of_month(29))
  expect_true(month_of_year(as.Date("2020-02-29")) == month_of_year(2))
})

# ---------------------------------------------------------------------------
# Format
# ---------------------------------------------------------------------------

test_that("format() falls back to a plain numeric token with no `at`", {
  expect_equal(format(day_of_year(60)), "D60")
  expect_equal(format(day_of_month(29)), "D29")
})

test_that("format() supports named labels for chronons with a label scheme", {
  expect_equal(format(month_of_year(3)), "Mar")
  expect_equal(format(day_of_week(3)), "Wed")
})

test_that("format() decomposes a chained multi-token template without chronon_divmod()", {
  fmt <- chronon_parse_cyclical(cal_gregorian$second(1L), cal_gregorian$day(1L))
  r <- time_parse("09:30:45", format = fmt, calendar = cal_gregorian)
  expect_true(time_is_cyclical_free(r))
  expect_equal(format(r), "09:30:45")
})

test_that("format() accepts a custom format string, like format.mt_time", {
  x <- day_of_year(60)
  expect_equal(format(x, "{cyc(day,year)}"), "60")
})

test_that("NA and infinite values format like other time types", {
  expect_equal(format(day_of_year(NA_integer_)), "NA")
})

test_that("format() works without mixtime attached", {
  # Formatting from a caller that can't see mixtime on the search path.
  env <- list2env(
    list(
      format = base::format,
      doy = day_of_year(60)@x[[1L]],
      dom = day_of_month(28)@x[[1L]],
      qoy = cyclical_time(2, chronon = cal_gregorian$quarter(1L), cycle = cal_gregorian$year(1L))@x[[1L]]
    ),
    parent = emptyenv()
  )
  expect_equal(evalq(format(doy), env), "D60")
  expect_equal(evalq(format(dom), env), "D28")
  expect_equal(
    evalq(format(qoy), env),
    format(cyclical_time(as.Date("2024-05-01"), chronon = cal_gregorian$quarter(1L), cycle = cal_gregorian$year(1L)))
  )
})

test_that("format() scales chronons longer than one unit", {
  x <- mt_cyclical_free(c(0L, 1L, 47L), chronon = cal_gregorian$minute(30L), cycle = cal_gregorian$day(1L))
  expect_equal(format(x), c("00:00", "00:30", "23:30"))

  # Positions count 3-month chronons, formatted like located values.
  q <- cyclical_time(c(1, 2, 4), chronon = cal_gregorian$month(3L), cycle = cal_gregorian$year(1L))
  located <- cyclical_time(
    as.Date(c("2024-01-01", "2024-04-01", "2024-10-01")),
    chronon = cal_gregorian$month(3L), cycle = cal_gregorian$year(1L)
  )
  expect_equal(format(q), format(located))
  expect_true(is.na(cyclical_time(5, chronon = cal_gregorian$month(3L), cycle = cal_gregorian$year(1L))))
  expect_true(is.na(cyclical_time(48, chronon = cal_gregorian$minute(30L), cycle = cal_gregorian$day(1L))))
})

test_that("free values never carry a time zone", {
  x <- mt_cyclical_free(
    c(0L, 3600L),
    chronon = cal_gregorian$second(1L, tz = "Australia/Melbourne"),
    cycle = cal_gregorian$day(1L, tz = "Australia/Melbourne")
  )
  expect_equal(format(x), c("00:00:00", "01:00:00"))
  expect_true(is.na(attr(x, "chronon")@tz))
  expect_true(is.na(attr(x, "cycle")@tz))

  y <- time_of_day(c(0, 3600), tz = "Australia/Melbourne")
  expect_equal(format(y), c("00:00:00", "01:00:00"))
  expect_true(is.na(attr(y@x[[1L]], "chronon")@tz))
})

test_that("format() never leaks unresolved tokens", {
  x <- month_of_year(3)
  expect_false(grepl("{", format(x, "{cyc(month, year)} {tz(.time)}"), fixed = TRUE))
  expect_error(format(x, "{lin(year)}-{cyc(month, year)}"), "lin()", fixed = TRUE)
})

test_that("an all-NA logical builds a free NA", {
  x <- month_of_year(NA)
  expect_true(time_is_cyclical_free(x))
  expect_true(is.na(x))
  expect_equal(length(month_of_year(c(NA, NA))), 2L)
})

test_that("numbers combined with free values are positions counted from 1", {
  expect_equal(month_of_year(c(1, 3)) == 3, c(FALSE, TRUE))

  x <- vctrs::vec_c(month_of_year(1), 3)
  expect_true(all(time_is_cyclical_free(x)))
  expect_equal(format(x), c("Jan", "Mar"))
  expect_equal(format(vctrs::vec_c(3L, month_of_year(1))), c("Mar", "Jan"))

  expect_equal(format(vctrs::vec_cast(3, month_of_year(1))), "Mar")
  expect_equal(vctrs::vec_cast(3, month_of_year(1)@x[[1L]]), month_of_year(3)@x[[1L]])
  # Out of range positions are NA, as in the constructor.
  expect_true(is.na(vctrs::vec_cast(13, month_of_year(1))))
  expect_equal(format(vctrs::vec_cast(0, time_of_day(0))), "00:00:00")
})

test_that("numbers keep combining with located and linear time as before", {
  x <- vctrs::vec_c(month_of_year(yearmonth("2024-03")), 3)
  expect_false(any(time_is_cyclical_free(x)))
})

test_that("underspecified and located cyclical time don't convert to each other", {
  located <- month_of_year(yearmonth("2024-03"))
  free <- month_of_year(1)
  expect_error(vctrs::vec_cast(located, free), class = "vctrs_error_incompatible_type")
  expect_error(vctrs::vec_cast(free, located), class = "vctrs_error_incompatible_type")
  expect_error(vctrs::vec_cast(located@x[[1L]], free@x[[1L]]), "no enclosing cycle")
  expect_error(vctrs::vec_ptype2(located@x[[1L]], free@x[[1L]]), "no enclosing cycle")

  # A mixtime holds them side by side, like linear time and durations.
  both <- vctrs::vec_c(located, free)
  expect_equal(time_is_cyclical_free(both), c(FALSE, TRUE))
})

test_that("c() and assignment read numbers as free cyclical positions", {
  expect_equal(c(month_of_year(1), 3), month_of_year(c(1, 3)))

  x <- month_of_year(1:2)
  x[2] <- 5
  expect_equal(x, month_of_year(c(1, 5)))

  x[[1]] <- 12
  expect_equal(x, month_of_year(c(12, 5)))
})

test_that("as.numeric() of cyclical time is the position by label start", {
  p <- month_of_year(c(8, 3, 12))
  expect_equal(as.numeric(p), c(8, 3, 12))
  expect_equal(as.integer(p), c(8L, 3L, 12L))
  expect_equal(month_of_year(as.numeric(p)), p)
  expect_equal(as.numeric(time_of_day(c(0, 13))), c(0, 13))

  x <- cyclical_time(
    as_mixtime(as.POSIXct("2020-01-01 10:00", tz = "UTC") + 3600 * 0:2),
    chronon = hour(1L), cycle = day(1L)
  )
  expect_equal(as.numeric(x), 10:12)
})

test_that("base numeric consumers read mixtime as they read its time vector", {
  # vecvec passes the data of a single-type vector to base R and C code
  # (lm(), prcomp(), ...), as for the plain time vector: the position in the
  # cycle for free cyclical time, and the linear time for located cyclical
  # time.
  skip_if(packageVersion("vecvec") < "2.0.0")
  set.seed(1)
  n <- 40
  d <- as.Date("2020-01-01") + seq_len(n)
  cols <- list(
    lin = as_mixtime(d), cyc = month_of_year(d), cyc2 = day_of_month(d),
    free = month_of_year(rep(1:12, length.out = n))
  )
  y <- rnorm(n)
  z <- rnorm(n)
  for (k in names(cols)) {
    df <- data.frame(y = y, z = z)
    df[[k]] <- cols[[k]]
    dfp <- data.frame(y = y, z = z)
    dfp[[k]] <- vecvec::unvecvec(cols[[k]])
    expect_true(is.numeric(cols[[k]]))
    expect_equal(
      unname(coef(lm(reformulate(c(k, "z"), "y"), df))),
      unname(coef(lm(reformulate(c(k, "z"), "y"), dfp)))
    )
    expect_equal(
      prcomp(reformulate(c(k, "z")), df, scale. = TRUE)$sdev,
      prcomp(reformulate(c(k, "z")), dfp, scale. = TRUE)$sdev
    )
  }

  # The data of linear time is its value, so models agree with as.numeric()
  df <- data.frame(y = y, z = z, lin = cols$lin)
  dfp <- data.frame(y = y, z = z, lin = as.numeric(cols$lin))
  expect_equal(unname(coef(lm(y ~ lin + z, df))), unname(coef(lm(y ~ lin + z, dfp))))
})
