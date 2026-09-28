test_that("chronon_glb() finds the finest common chronon", {
  # A yearmonth and a date share days as their finest common chronon
  expect_equal(
    chronon_glb(c(yearmonth(Sys.Date()), date(Sys.Date()))),
    cal_gregorian$day(1L)
  )

  # A month and an ISO week don't nest, so their finest common chronon is a day
  expect_equal(
    chronon_glb(c(yearweek(Sys.Date()), yearmonth(Sys.Date()))),
    cal_gregorian$day(1L)
  )

  # A single input chronon is its own GLB
  expect_equal(
    chronon_glb(yearmonth(Sys.Date())),
    cal_gregorian$month(1L)
  )
})

test_that("chronon_lub() finds the coarsest common chronon", {
  # A second and a minute evenly aggregate into a minute
  expect_equal(
    chronon_lub(c(
      linear_time(Sys.time(), second(1L)),
      linear_time(Sys.time(), minute(1L))
    )),
    cal_gregorian$minute(1L)
  )

  # A single input chronon is its own LUB
  expect_equal(
    chronon_lub(yearmonth(Sys.Date())),
    cal_gregorian$month(1L)
  )

  # Chronons with no common coarser chronon (e.g. weeks don't nest in years)
  # error rather than silently picking an unrelated bound.
  expect_error(
    chronon_lub(c(yearweek(Sys.Date()), yearmonth(Sys.Date()))),
    "do not share a common coarser chronon"
  )
})

test_that("chronon_common() is a deprecated alias for chronon_glb()", {
  lifecycle::expect_deprecated(
    result <- chronon_common(c(yearmonth(Sys.Date()), date(Sys.Date())))
  )
  expect_equal(result, chronon_glb(c(yearmonth(Sys.Date()), date(Sys.Date()))))
})
