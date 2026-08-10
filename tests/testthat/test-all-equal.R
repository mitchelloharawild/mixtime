test_that("all.equal() on time values succeeds instead of erroring", {
  expect_true(isTRUE(all.equal(yearmonth(0), yearmonth(0))))
  expect_false(isTRUE(all.equal(yearmonth(0), yearmonth(1))))
  expect_match(all.equal(yearmonth(0), yearmonth(1)), "Mean absolute difference")
})

test_that("all.equal() agrees with == across differing chronons", {
  # These are `==` (24 hours is exactly 1 day), so all.equal() must treat
  # them as equal too, despite differing raw storage/chronon.
  expect_true(days(1) == hours(24))
  expect_true(isTRUE(all.equal(days(1), hours(24))))

  expect_true(isTRUE(all.equal(days(1), days(1))))
  expect_false(isTRUE(all.equal(days(1), days(2))))
})

test_that("all.equal() tolerance is absolute, in chronon units", {
  a <- yearmonth(Sys.Date(), discrete = FALSE)
  b <- a + 1e-10
  expect_true(isTRUE(all.equal(a, b)))
  expect_false(isTRUE(all.equal(a, b, tolerance = 1e-12)))
})

test_that("all.equal() reports length and NA mismatches", {
  expect_match(
    all.equal(yearmonth(0:2), yearmonth(0:1)),
    "Lengths \\(3, 2\\) differ"
  )
  expect_match(
    all.equal(yearmonth(c(0, NA)), yearmonth(c(0, 1))),
    "is.NA"
  )
  expect_true(isTRUE(all.equal(yearmonth(c(0, NA)), yearmonth(c(0, NA)))))
})

test_that("all.equal() works via testthat::expect_equal()", {
  expect_equal(days(1), hours(24))
  expect_failure(expect_equal(yearmonth(0), yearmonth(1)))
})
