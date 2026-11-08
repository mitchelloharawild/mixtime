# Underlying 0-indexed positions of free cyclical values
free_pos <- function(x) vctrs::vec_data(vecvec::unvecvec(x))

minute_of_day <- function(x) cyclical_time(x, chronon = minute(1L), cycle = day(1L))
hour_of_day <- function(x) cyclical_time(x, chronon = hour(1L), cycle = day(1L))

test_that("shifting fixed length cycles wraps around the cycle", {
  expect_equal(free_pos(month_of_year(12) + 1L), 0L)
  expect_equal(free_pos(month_of_year(1) - 1L), 11L)
  expect_equal(free_pos(1L + month_of_year(12)), 0L)
  expect_equal(free_pos(day_of_week(6) + 0:3), c(5L, 6L, 0L, 1L))
  expect_equal(free_pos(month_of_year(1) + 25L), 1L)
  expect_equal(free_pos(month_of_year(3) - 27L), 11L)

  # Discrete positions stay integers
  expect_type(free_pos(month_of_year(12) + 1), "integer")

  # Shifting keeps the type
  x <- month_of_year(12) + 1L
  expect_true(S7::S7_inherits(vecvec::unvecvec(x), mt_cyclical_free))
  expect_true(x == month_of_year(1))
})

test_that("shifting by durations of a fixed whole number of chronons wraps", {
  expect_equal(free_pos(month_of_year(1) - months(1)), 11L)
  expect_equal(free_pos(months(2) + month_of_year(11)), 0L)
  expect_equal(free_pos(month_of_year(11) + quarters(1)), 1L)
  expect_equal(free_pos(minute_of_day(3) + hours(2)), 123L)
  expect_equal(free_pos(minute_of_day(3) - hours(1)), 1383L)
  expect_equal(free_pos(hour_of_day(23) + minutes(60)), 0L)
  expect_equal(free_pos(day_of_week(7) + days(1)), 0L)
})

test_that("a + (b - a) == b for fixed length cycles", {
  a <- month_of_year(c(1, 3, 12, 7))
  b <- month_of_year(c(12, 1, 3, 7))
  expect_true(all(a + (b - a) == b))
  expect_true(all(b - (b - a) == a))
})

test_that("missing values stay missing", {
  x <- month_of_year(c(NA, 12)) + 1L
  expect_equal(free_pos(x), c(NA, 0L))
  expect_equal(free_pos(month_of_year(c(NA, 1)) - months(1)), c(NA, 11L))
  d <- month_of_year(c(NA, 3)) - month_of_year(1)
  expect_equal(as.numeric(d), c(NA, 2))
})

test_that("shifting variable length cycles errors whatever the values", {
  expect_error(day_of_month(31) + 1L, class = "mixtime_error_free_cyclical")
  expect_error(day_of_month(15) + 1L, class = "mixtime_error_free_cyclical")
  expect_error(day_of_month(15) - 1L, class = "mixtime_error_free_cyclical")
  expect_error(day_of_month(15) + days(1), class = "mixtime_error_free_cyclical")
  expect_error(1L + day_of_month(15), class = "mixtime_error_free_cyclical")
  expect_error(day_of_year(60) + 1L, class = "mixtime_error_free_cyclical")
  expect_error(week_of_year(3) + 1L, class = "mixtime_error_free_cyclical")
  expect_error(day_of_month(15) + 0L, class = "mixtime_error_free_cyclical")
})

test_that("the variable length error explains and suggests locating", {
  rlang::local_options(cli.width = Inf)
  err <- tryCatch(day_of_month(28) + days(1), error = identity)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "Can't add 1 day to a day of month with no month", fixed = TRUE)
  expect_match(msg, "number of days in a month varies", fixed = TRUE)
  expect_match(msg, 'time_compose(yearmonth("2024-02"), x) + days(1)', fixed = TRUE)
  expect_null(err$call)

  # The suggested anchoring works
  x <- day_of_month(28)
  expect_equal(
    time_compose(yearmonth("2024-02"), x) + days(1),
    time_compose(yearmonth("2024-02"), day_of_month(29))
  )
  expect_equal(
    format(time_compose(yearmonth("2024-02"), day_of_month(29))),
    "2024-02-29"
  )
})

test_that("suggested locations compose with underspecified values of their cycle", {
  expect_no_error(time_compose(year(2024), month_of_year(3)))
  expect_no_error(time_compose(yearweek("2024 W03"), day_of_week(2)))
  expect_no_error(time_compose(date("2024-02-01"), minute_of_day(3)))
  expect_no_error(time_compose(yearmonth("2024-02"), day_of_month(3)))
  expect_no_error(time_compose(year(2024), day_of_year(60)))
})

test_that("shifting by incompatible or coarser durations errors", {
  expect_error(month_of_year(1) + days(1), class = "mixtime_error_free_cyclical")
  expect_error(day_of_week(1) + months(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) + years(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) - years(1), class = "mixtime_error_free_cyclical")
  expect_error(day_of_week(1) + weeks(1), class = "mixtime_error_free_cyclical")
  expect_error(hour_of_day(1) + minutes(90), class = "mixtime_error_free_cyclical")
  expect_error(days(1) + month_of_year(1), class = "mixtime_error_free_cyclical")
})

test_that("free - free gives the duration between positions", {
  expect_equal(month_of_year(3) - month_of_year(1), months(2L))
  expect_equal(month_of_year(1) - month_of_year(3), months(-2L))
  expect_equal(month_of_year(c(1, 12)) - month_of_year(3), months(c(-2L, 9L)))
  # Variable length cycles too
  expect_equal(day_of_month(31) - day_of_month(1), days(30L))
  expect_equal(day_of_year(60) - day_of_year(1), days(59L))
  # Differing chronons of the same cycle use their common chronon
  expect_equal(as.numeric(hour_of_day(3) - minute_of_day(60)), 120)
  # diff() uses `-`
  expect_equal(diff(month_of_year(c(1, 3, 12))), months(c(2L, 9L)))
})

test_that("differences with other cycles or located time error", {
  expect_error(month_of_year(1) - day_of_week(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) - yearmonth("2024-01"), class = "mixtime_error_free_cyclical")
  expect_error(yearmonth("2024-01") - month_of_year(1), class = "mixtime_error_free_cyclical")
  anchored <- month_of_year(yearmonth("2024-03"))
  expect_error(anchored - month_of_year(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) - anchored, class = "mixtime_error_free_cyclical")
})

test_that("unsupported arithmetic errors", {
  expect_error(3 - month_of_year(1), class = "mixtime_error_free_cyclical")
  expect_error(months(3) - month_of_year(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) * 2, class = "mixtime_error_free_cyclical")
  expect_error(2 * month_of_year(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) / 2, class = "mixtime_error_free_cyclical")
  expect_error(months(2) * month_of_year(1), class = "mixtime_error_free_cyclical")
  expect_error(month_of_year(1) + month_of_year(2), class = "mixtime_error_free_cyclical")
  expect_error(yearmonth("2024-01") + month_of_year(2), class = "mixtime_error_free_cyclical")
  expect_error(-month_of_year(1), class = "mixtime_error_free_cyclical")
  expect_equal(free_pos(+month_of_year(3)), 2L)
})

test_that("mean and other new-value maths error, rounding is kept", {
  x <- month_of_year(c(1, 12))
  expect_error(mean(x), class = "mixtime_error_free_cyclical")
  expect_error(abs(x), class = "mixtime_error_free_cyclical")
  expect_error(sum(x))
  expect_equal(free_pos(round(x)), free_pos(x))
  expect_equal(free_pos(floor(x)), free_pos(x))
  expect_equal(is.na(month_of_year(c(NA, 1))), c(TRUE, FALSE))
})

test_that("ordering is linear within the cycle", {
  x <- month_of_year(c(5, 1, 12, 3))
  expect_equal(order(x), c(2L, 4L, 1L, 3L))
  expect_equal(free_pos(sort(x)), c(0L, 2L, 4L, 11L))
  expect_equal(free_pos(sort(x, decreasing = TRUE)), c(11L, 4L, 2L, 0L))
  expect_equal(free_pos(min(x)), 0L)
  expect_equal(free_pos(max(x)), 11L)
  expect_equal(free_pos(range(x)), c(0L, 11L))
  expect_true(month_of_year(1) < month_of_year(12))
  expect_equal(free_pos(range(day_of_month(c(31, 2, 15)))), c(1L, 30L))
})

test_that("seq() wraps fixed length cycles", {
  expect_equal(free_pos(seq(day_of_week(6), by = 1, length.out = 4)), c(5L, 6L, 0L, 1L))
  expect_equal(free_pos(seq(month_of_year(1), length.out = 15)), c(0:11, 0:2))
  expect_equal(
    free_pos(seq(month_of_year(1), by = "2 months", length.out = 8)),
    c(0L, 2L, 4L, 6L, 8L, 10L, 0L, 2L)
  )
  expect_equal(free_pos(seq(month_of_year(1), by = months(3), length.out = 5)), c(0L, 3L, 6L, 9L, 0L))
  expect_equal(free_pos(seq(month_of_year(1), by = cal_gregorian$month(2L), length.out = 3)), c(0L, 2L, 4L))
  expect_equal(free_pos(seq(to = month_of_year(2), by = 1, length.out = 4)), c(10L, 11L, 0L, 1L))
  # Between `from` and `to`, sequences are linear within the cycle
  expect_equal(free_pos(seq(month_of_year(1), month_of_year(5), by = months(2))), c(0L, 2L, 4L))
  expect_equal(free_pos(seq(month_of_year(11), month_of_year(2))), 10:1)
  expect_true(all(seq(month_of_year(1), length.out = 13) == month_of_year(1) + 0:12))
})

test_that("seq() errors for variable length cycles and incompatible steps", {
  expect_error(seq(day_of_month(1), length.out = 3), class = "mixtime_error_free_cyclical")
  expect_error(seq(day_of_month(1), day_of_month(3)), class = "mixtime_error_free_cyclical")
  expect_error(seq(day_of_year(1), by = 1, length.out = 3), class = "mixtime_error_free_cyclical")
  expect_error(seq(month_of_year(1), by = "1 day", length.out = 3), class = "mixtime_error_free_cyclical")
  expect_error(seq(month_of_year(1), by = years(1), length.out = 3), class = "mixtime_error_free_cyclical")
  expect_error(seq(month_of_year(1), day_of_week(3)), class = "mixtime_error_free_cyclical")
})
