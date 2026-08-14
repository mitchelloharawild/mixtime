test_that("chronon_nests_in_fixed() checks divisibility, not just reachability", {
  # Regression test: previously ignored `@n` entirely for any cross-class
  # pair, returning TRUE whenever the classes were merely connected in the
  # fixed-cardinality graph (e.g. month -> year), regardless of whether the
  # chronon's own size tiled evenly into the granule. 12 months/year is not a
  # whole multiple of 5, so a 5-month chronon does not nest in a 1-year
  # granule - 2, 3, 4, 6, 12 do (all divisors of 12).
  expect_false(chronon_nests_in_fixed(cal_gregorian$month(5L), cal_gregorian$year(1L)))
  expect_false(chronon_nests_in_fixed(cal_gregorian$month(7L), cal_gregorian$year(1L)))
  expect_false(chronon_nests_in_fixed(cal_gregorian$month(13L), cal_gregorian$year(1L)))
  for (n in c(1L, 2L, 3L, 4L, 6L, 12L)) {
    expect_true(
      chronon_nests_in_fixed(cal_gregorian$month(n), cal_gregorian$year(1L)),
      info = paste("n =", n)
    )
  }

  # Scaling the granule's own `@n` changes what divides evenly too (e.g. 5
  # months nests in 5 years: 60 months / 5 == 12).
  expect_true(chronon_nests_in_fixed(cal_gregorian$month(5L), cal_gregorian$year(5L)))
  expect_true(chronon_nests_in_fixed(cal_gregorian$month(4L), cal_gregorian$year(2L)))

  # Multi-hop fixed chain (month -> quarter -> year), not just a directly
  # registered edge.
  expect_true(chronon_nests_in_fixed(cal_gregorian$month(1L), cal_gregorian$quarter(1L)))
  expect_true(chronon_nests_in_fixed(cal_gregorian$quarter(1L), cal_gregorian$year(1L)))

  # Same-class case is untouched (n divisibility only).
  expect_true(chronon_nests_in_fixed(cal_gregorian$month(2L), cal_gregorian$month(6L)))
  expect_false(chronon_nests_in_fixed(cal_gregorian$month(5L), cal_gregorian$month(6L)))

  # No registered fixed relationship at all (day <-> month is irregular).
  expect_false(chronon_nests_in_fixed(cal_gregorian$day(1L), cal_gregorian$month(1L)))
})

test_that("chronon_nests_in() matches chronon_nests_in_fixed() for any fixed-reachable pair", {
  # A fixed path's ratio is constant regardless of `at`, so if it doesn't
  # divide evenly, the pair never nests - no other (necessarily irregular)
  # path could make it nest either. chronon_nests_in() defers to
  # chronon_nests_in_fixed()'s exact answer for these pairs rather than
  # stopping at plain reachability.
  expect_false(chronon_nests_in(cal_gregorian$month(5L), cal_gregorian$year(1L)))
  expect_true(chronon_nests_in(cal_gregorian$month(4L), cal_gregorian$year(1L)))
})

test_that("chronon_nests_in() checks @n for pairs only reachable via an irregular edge", {
  # day <-> month has no fixed cardinality (see chronon_nests_in_fixed() test
  # above), so every day <-> month/year path crosses an irregular edge. A
  # single-unit chronon always nests (a day is never split across a month
  # boundary), regardless of the granule's own `@n`.
  expect_true(chronon_nests_in(cal_gregorian$day(1L), cal_gregorian$month(5L)))
  expect_true(chronon_nests_in(cal_gregorian$day(1L), cal_gregorian$year(1L)))

  # A multi-unit chronon's blocks can straddle a month boundary depending on
  # where in the calendar they fall (e.g. day(2) blocks land differently
  # depending on the parity of the month's start day-of-epoch) - this can't be
  # resolved without a specific `at`, so it errors rather than guessing.
  expect_error(chronon_nests_in(cal_gregorian$day(2L), cal_gregorian$month(1L)))
  expect_error(chronon_nests_in(cal_gregorian$day(7L), cal_gregorian$year(1L)))

  # No registered relationship at all is still a plain FALSE, not an error.
  expect_false(chronon_nests_in(cal_gregorian$month(1L), cal_time_lunar$phase(1L)))
})
