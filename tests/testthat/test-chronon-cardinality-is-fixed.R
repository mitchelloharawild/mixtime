test_that("chronon_cardinality_is_fixed() identifies fixed cardinalities", {
  greg <- cal_gregorian
  expect_true(chronon_cardinality_is_fixed(greg$month(1L), greg$year(1L)))
  expect_true(chronon_cardinality_is_fixed(greg$quarter(1L), greg$year(1L)))
  expect_true(chronon_cardinality_is_fixed(greg$hour(1L), greg$day(1L)))
  expect_true(chronon_cardinality_is_fixed(greg$second(1L), greg$day(1L)))
  expect_true(chronon_cardinality_is_fixed(greg$day(1L), cal_isoweek$week(1L)))
  expect_true(chronon_cardinality_is_fixed(greg$month(2L), greg$year(3L)))
  expect_true(chronon_cardinality_is_fixed(greg$day(1L), greg$day(7L)))
})

test_that("chronon_cardinality_is_fixed() identifies irregular cardinalities", {
  greg <- cal_gregorian
  expect_false(chronon_cardinality_is_fixed(greg$day(1L), greg$month(1L)))
  expect_false(chronon_cardinality_is_fixed(greg$day(1L), greg$year(1L)))
  expect_false(chronon_cardinality_is_fixed(greg$hour(1L), greg$month(1L)))
  expect_false(
    chronon_cardinality_is_fixed(cal_isoweek$week(1L), cal_isoweek$year(1L))
  )
})

test_that("chronon_cardinality_is_fixed() is symmetric", {
  greg <- cal_gregorian
  expect_true(chronon_cardinality_is_fixed(greg$year(1L), greg$month(1L)))
  expect_false(chronon_cardinality_is_fixed(greg$month(1L), greg$day(1L)))
})
