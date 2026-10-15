test_that("calendars sharing a unit retain one S7 class and separate calendars", {
  tu_shared_day <- new_time_unit(name = "tu_shared_day", parent = mt_tz_unit)
  first <- new_calendar(day = tu_shared_day, class = "cal_first")
  second <- new_calendar(inherit = first, class = "cal_second")
  method(time_unit_abbr, first$day) <- function(x) "d"

  x <- first$day(n = 2L, tz = "UTC")
  y <- second$day(n = 3L, tz = "UTC")
  expect_identical(S7::S7_class(x), tu_shared_day)
  expect_identical(S7::S7_class(y), tu_shared_day)
  expect_identical(time_unit_abbr(y), "d")
  expect_identical(time_calendar(x), first)
  expect_identical(time_calendar(y), second)
  expect_identical(x@n, 2L)
  expect_identical(y@tz, "UTC")
})

test_that("ordinary calendar constructors retain their calendar", {
  calendars <- list(cal_gregorian, cal_isoweek, cal_time_civil, cal_time_solar)
  for (calendar in calendars) {
    expect_identical(time_calendar(calendar$day(n = 1L)), calendar)
  }
  expect_identical(
    S7::S7_class(cal_gregorian$day(n = 1L)),
    S7::S7_class(cal_isoweek$day(n = 1L))
  )
})

test_that("custom unit constructors forward their calendar", {
  tu_custom_day <- new_time_unit(
    name = "tu_custom_day",
    parent = mt_tz_unit,
    constructor = function(
      n = 1L,
      tz = "UTC",
      calendar = attr(sys.function(), "cal")$calendar
    ) {
      S7::new_object(mt_tz_unit(n = n, tz = tz, calendar = calendar))
    }
  )
  calendar <- new_calendar(day = tu_custom_day)
  x <- calendar$day(n = 2L, tz = "Europe/London")
  expect_identical(time_calendar(x), calendar)
  expect_identical(x@tz, "Europe/London")
})

test_that("calendar metadata survives reconstruction by time operations", {
  x <- c(yearmonth(as.Date("2024-02-01")), date(as.Date("2024-02-29")))
  expect_identical(time_calendar(chronon_glb(x)), cal_gregorian)
  expect_identical(time_calendar(time_chronon(x)), cal_gregorian)

  y <- linear_time(as.Date("2024-02-29"), chronon = cal_isoweek$day(n = 1L))
  expect_identical(time_calendar(seq(y, by = 1, length.out = 2)), cal_isoweek)
  expect_identical(time_calendar(y + days(1L)), cal_isoweek)

  z <- c(minutes(1L), hours(1L))
  expect_identical(time_calendar(chronon_lub(z)), cal_gregorian)
})

test_that("cardinality methods receive the calendar through intermediate units", {
  calendar <- new_calendar(
    fine = new_time_unit(name = "tu_calendar_fine"),
    middle = new_time_unit(name = "tu_calendar_middle"),
    coarse = new_time_unit(name = "tu_calendar_coarse")
  )
  seen <- list()
  cardinality <- function(x, y) {
    seen[[length(seen) + 1L]] <<- list(time_calendar(x), time_calendar(y))
    2L
  }
  method(
    chronon_cardinality_fixed,
    list(calendar$fine, calendar$middle)
  ) <- cardinality
  method(
    chronon_cardinality_fixed,
    list(calendar$middle, calendar$coarse)
  ) <- cardinality
  inherited <- new_calendar(inherit = calendar, class = "cal_inherited")

  expect_equal(
    chronon_cardinality_fixed(inherited$fine(n = 1L), inherited$coarse(n = 1L)),
    4
  )
  expect_identical(seen, rep(list(list(inherited, inherited)), 2L))
})

test_that("shared calendar units keep the same arithmetic and context", {
  x <- linear_time(as.Date("2024-02-29"), chronon = cal_gregorian$day(n = 1L))
  y <- linear_time(as.Date("2024-02-29"), chronon = cal_isoweek$day(n = 1L))
  expect_identical(as.numeric(x - y), 0)
  expect_identical(x == y, TRUE)
  expect_identical(as.numeric(x + days(1L)), as.numeric(y + days(1L)))
  expect_identical(time_calendar(y + days(1L)), cal_isoweek)
  expect_identical(
    as.numeric(linear_time(x, chronon = cal_isoweek$day(n = 1L))),
    as.numeric(y)
  )
  expect_identical(
    as.numeric(seq(x, y + days(1L), by = 1)),
    as.numeric(x) + 0:1
  )
  expect_identical(
    duration(1L, chronon = cal_gregorian$day(n = 1L)) ==
      duration(1L, chronon = cal_isoweek$day(n = 1L)),
    TRUE
  )
})

test_that("shared calendar units retain compatible cycles", {
  x <- cyclical_time(
    1:2,
    chronon = cal_gregorian$hour(n = 1L),
    cycle = cal_gregorian$day(n = 1L)
  )
  y <- cyclical_time(
    1:2,
    chronon = cal_isoweek$hour(n = 1L),
    cycle = cal_isoweek$day(n = 1L)
  )
  expect_identical(x == y, c(TRUE, TRUE))
})

test_that("calendar metadata supports parsing and formatting", {
  x <- date("2024-02-29")
  expect_identical(format(x), "2024-02-29")
  expect_identical(format(yearmonth(x)), "2024 Feb")
  expect_identical(time_calendar(x), cal_gregorian)
})
