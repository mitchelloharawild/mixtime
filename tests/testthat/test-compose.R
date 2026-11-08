# Tests for time_compose(): the inverse of time_components(), building a
# single mixtime time point back up from lin()/cyc() components supplied as
# `spec ~ value` formulas or as already-tagged linear/cyclical time vectors.
# A cyc() formula's value is the 1-indexed position within the cycle (e.g.
# month 3 is March, day 15 is the 15th), matching everyday counting rather
# than the raw 0-indexed position chronon_parts()/time_components() use
# internally.

test_that("composes a date from year/month/day formulas", {
  r <- time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(day, month) ~ 15)

  expect_equal(format(r), "1980-03-15")
  expect_equal(as.numeric(vecvec::unvecvec(r)), as.numeric(as.Date("1980-03-15")))
})

test_that("a lin() component alone is a valid, coarser time point", {
  r <- time_compose(lin(year) ~ 1980)

  expect_equal(format(r), "1980")
  expect_true(time_is_linear(r))
})

test_that("round-trips through time_components() output columns", {
  parts <- time_components(as.Date("2024-03-15"), yr = lin(year), mth = cyc(month, year))

  r <- time_compose(parts$yr, parts$mth)

  expect_equal(format(r), "2024 Mar")
})

test_that("mixes formulas and already-tagged vectors", {
  r <- time_compose(lin(year) ~ 1980, month_of_year(as.Date("1980-03-01")))

  expect_equal(format(r), "1980 Mar")
})

test_that("cyc() formula values are 1-indexed, not the raw internal position", {
  # cyc(month, year) ~ 3 is the 3rd month (March), matching everyday counting
  # -- not chronon_parts()'s raw 0-indexed position (which would be April).
  r <- time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3)

  expect_equal(format(r), "1980 Mar")
})

test_that("multi-unit (self-referencing) cycles compose correctly", {
  # Block 3 (0-indexed) of 3-month blocks is months 9-11 since epoch; the 3rd
  # (1-indexed) month within it is month 11 -> December 1970.
  r <- time_compose(
    lin(cal_gregorian$month(3L)) ~ 3,
    cyc(cal_gregorian$month(1L), cal_gregorian$month(3L)) ~ 3
  )

  expect_equal(as.numeric(vecvec::unvecvec(r)), 11)
  expect_equal(format(r), "1970 Dec")
})

test_that("a multi-unit cyc() only connects to a matching-size frontier", {
  expect_error(
    time_compose(
      lin(year) ~ 1980,
      cyc(month, year) ~ 3,
      cyc(cal_gregorian$month(1L), cal_gregorian$month(3L)) ~ 3
    ),
    "does not connect"
  )
})

test_that("vectorised components recycle to a common length", {
  r <- time_compose(lin(year) ~ 1980, cyc(month, year) ~ 1:3, cyc(day, month) ~ 1)

  expect_equal(format(r), c("1980-01-01", "1980-02-01", "1980-03-01"))
})

test_that("NA in any component propagates to NA in the result", {
  r1 <- time_compose(lin(year) ~ c(1980, NA), cyc(month, year) ~ 3, cyc(day, month) ~ 15)
  expect_identical(is.na(r1), c(FALSE, TRUE))

  r2 <- time_compose(lin(year) ~ 1980, cyc(month, year) ~ c(3, NA), cyc(day, month) ~ 15)
  expect_identical(is.na(r2), c(FALSE, TRUE))
})

test_that("Inf in the lin() component carries through as an infinite count", {
  r <- time_compose(lin(year) ~ c(1980, Inf, -Inf), cyc(month, year) ~ 3, cyc(day, month) ~ 15)

  expect_identical(is.na(r), c(FALSE, FALSE, FALSE))
  expect_equal(as.numeric(vecvec::unvecvec(r)), c(3726, Inf, -Inf))
})

test_that("a single cyc() with no lin() component composes a cyclical time point", {
  r <- time_compose(cyc(month, year) ~ 3)

  expect_true(time_is_cyclical_free(r))
  expect_equal(format(r), "Mar")
  expect_equal(
    as.numeric(vecvec::unvecvec(r)),
    as.numeric(vecvec::unvecvec(month_of_year(as.Date("1970-03-01"))))
  )
})

test_that("a fixed-length cyc() chain with no lin() component collapses to a cyclical time point", {
  # minute 30 of hour 3 -> minute 149 (0-indexed) of the day
  r <- time_compose(cyc(minute, hour) ~ 30, cyc(hour, day) ~ 3)

  expect_true(time_is_cyclical_free(r))
  expect_equal(as.numeric(vecvec::unvecvec(r)), 2 * 60 + 29)
})

test_that("a variable-length cyc() chain with no lin() component errors", {
  expect_error(
    time_compose(cyc(day, month) ~ 29, cyc(month, year) ~ 3),
    "varies"
  )
  expect_error(time_compose(day_of_month(29), month_of_year(2)), "lin")
})

test_that("free components compose against a lin() component in any order", {
  r1 <- time_compose(lin(year) ~ 2024, day_of_month(29), month_of_year(2))
  r2 <- time_compose(day_of_month(29), month_of_year(2), lin(year) ~ 2024)
  r3 <- time_compose(yearmonth("2024-02"), day_of_month(29))
  expect_equal(format(r1), "2024-02-29")
  expect_equal(format(r2), "2024-02-29")
  expect_equal(format(r3), "2024-02-29")
})

test_that("free components are validated against the located time", {
  expect_error(time_compose(lin(year) ~ 2023, day_of_month(29), month_of_year(2)), "28")
  expect_error(time_compose(yearmonth("2023-02"), day_of_month(29)), "28")
  expect_equal(
    format(time_compose(lin(year) ~ 2024, cyc(month, year) ~ 3, cyc(day, month) ~ 29)),
    "2024-03-29"
  )
})

test_that("cyclical time_compose() output round-trips through time_compose() again", {
  anchored <- time_compose(lin(day) ~ 0, cyc(hour, day) ~ 3, cyc(minute, hour) ~ 30)
  cyclical <- time_compose(cyc(minute, hour) ~ 30, cyc(hour, day) ~ 3)

  r <- time_compose(lin(day) ~ 0, cyclical)
  expect_equal(format(r), format(anchored))
})

test_that("errors when every cyc() component's cycle is itself supplied (no coarsest link)", {
  # Neither component is coarser than the other, so neither can root the chain.
  expect_error(
    time_compose(cyc(day, month) ~ 15, cyc(month, day) ~ 1),
    "coarsest link"
  )
})

test_that("errors on disconnected cyc()-only chains with no lin() component", {
  expect_error(
    time_compose(cyc(month, year) ~ 3, cyc(quarter, year) ~ 1),
    "disconnected"
  )
})

test_that("errors when multiple lin() components are supplied", {
  expect_error(time_compose(lin(year) ~ 1980, lin(month) ~ 5), "at most one")
})

test_that("errors when a cyc() component doesn't connect to the chain", {
  expect_error(
    time_compose(lin(year) ~ 1980, cyc(day, month) ~ 15),
    "does not connect"
  )
})

test_that("errors when two cyc() components branch from the same frontier", {
  expect_error(
    time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(quarter, year) ~ 1),
    "Multiple.*cyc"
  )
})

test_that("errors on a duplicate cyc() component", {
  expect_error(
    time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(month, year) ~ 4),
    "Multiple.*cyc"
  )
})

test_that("errors when a cyc() value is out of range", {
  expect_error(
    time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(day, month) ~ 32),
    "not a valid"
  )
})

test_that("errors when a cyc() value is out of range for a boundaried cycle (12-hour AM/PM clock)", {
  # Valid 1-indexed hour-within-ampm positions are 1-12; 13 overflows into
  # the next ampm and is rejected rather than silently wrapping.
  expect_error(
    time_compose(lin(cal_time_civil$ampm) ~ 0, cyc(cal_time_civil$hour, cal_time_civil$ampm) ~ 13),
    "not a valid"
  )
})

test_that("errors for an input that is neither a formula nor a time vector", {
  expect_error(time_compose(42), "formula")
})

test_that("errors on an empty call", {
  expect_error(time_compose(), "at least one component")
})
