test_that("Cardinality of the Symmetric454 calendar: cal_sym454", {
  # 1970 is a leap year, so Jan has 4 weeks, Feb has 5 weeks, Mar has 4 weeks, ..., Dec has 5 weeks
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(1L), at = 0:11L)),
    c(4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 5L)
  )
  # 1971 is not a leap year; its months are at positions 12..23 from the epoch
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(1L), at = 12:23L)),
    c(4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 4L, 4L, 5L, 4L)
  )

  # 1970 is a leap year and should have 53 weeks
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), year(1L), at = 0L)),
    53L
  )
  # 1971 is not a leap year and should have 52 weeks
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), year(1L), at = 12L)),
    52L
  )

  # Each quarter (3 months) should have 13 weeks except the last having 14 weeks in a leap year
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(3L), at = 0:3L)),
    c(13L, 13L, 13L, 14L)
  )

  # Each quarter (3 months) should have 13 weeks in a non-leap year
  expect_equal(
    with(cal_sym454, chronon_cardinality(week(1L), month(3L), at = 4:7L)),
    c(13L, 13L, 13L, 13L)
  )
})


test_that("Divmod of the Symmetric454 calendar: cal_sym454", {
  # Regular 4-5-4 pattern in 1970 (leap year)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(1L), month(1L), c(0L, 4L, 9L, 13L, 51L, 52L))),
    list(div = c(0L, 1L, 2L, 3L, 11L, 11L), mod = c(0L, 0L, 0L, 0L, 3L, 4L))
  )
  # Regular 4-5-4 pattern in 1971 (non-leap year)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(1L), month(1L), c(53L, 55L, 62L, 66L, 104L))),
    list(div = c(12L, 12L, 14L, 15L, 23L), mod = c(0L, 2L, 0L, 0L, 3L))
  )

  # 2-week units (fortnights)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(2L), month(1L), c(0L, 4L, 5L, 9L, 13L))),
    list(div = c(0L, 1L, 2L, 4L, 6L), mod = c(0L, 2L, 0L, 0L, 0L))
  )

  # Multi-month units (quarters)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(1L), month(3L), c(0L, 13L, 26L, 39L))),
    list(div = c(0L, 1L, 2L, 3L), mod = c(0L, 0L, 0L, 0L))
  )

  # Fortnight units with multi-month units (2-week quarters)
  expect_equal(
    with(cal_sym454, chronon_divmod(week(2L), month(3L), c(0L, 3L, 13L, 26L, 39L))),
    list(div = c(0L, 0L, 2L, 3L, 5L), mod = c(0L, 3L, 0L, 6L, 6L))
  )
})
