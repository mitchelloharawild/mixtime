test_that("sort()/xtfrm() align discrete spans to their midpoint by default (#mixtime-order)", {
  x <- c(yearmonth("2020 Jan"), date("2020-01-15"), yearmonth("2020 Feb"))

  # January's midpoint (~Jan 16) is after the 15th
  expect_equal(as.character(sort(x)), c("2020-01-15", "2020 Jan", "2020 Feb"))

  # aligning to the start of the span puts the whole month before the 15th
  expect_equal(as.character(sort(x, align_discrete = 0)), c("2020 Jan", "2020-01-15", "2020 Feb"))
  # aligning to the end of the span puts the whole month after the 15th
  expect_equal(as.character(sort(x, align_discrete = 1)), c("2020-01-15", "2020 Jan", "2020 Feb"))
})

test_that("sort() on a mixtime vector respects decreasing/na.last", {
  x <- yearmonth(c(0L, NA_integer_, -1L))

  expect_equal(as.character(sort(x)), c("1969 Dec", "1970 Jan"))
  expect_equal(as.character(sort(x, decreasing = TRUE)), c("1970 Jan", "1969 Dec"))
  expect_equal(as.character(sort(x, na.last = TRUE)), c("1969 Dec", "1970 Jan", "NA"))
})

test_that("align_discrete has no effect when every value shares a chronon", {
  x <- yearmonth(c(2L, 0L, 1L))
  expect_equal(sort(x, align_discrete = 0), sort(x, align_discrete = 1))
})
