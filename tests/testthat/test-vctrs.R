# https://github.com/mitchelloharawild/mixtime/issues/81
test_that("vec_detect_complete() recycles hashed chronons correctly (#81)", {
  expect_true(all(vctrs::vec_detect_complete(yearmonth(0:3))))
})

test_that("vec_detect_complete() correctly identifies missing values", {
  expect_equal(vctrs::vec_detect_complete(yearmonth(c(0:3, NA))), c(TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("vec_proxy_order() works for mixed-chronon mixtime vectors", {
  expect_no_error(vctrs::vec_proxy_order(c(yearmonth(360L), year(2000L))))
  expect_length(vctrs::vec_proxy_order(c(yearmonth(360L), year(2000L))), 2L)
})

test_that("vec_cast to character works for mt_duration", {
  expect_equal(vctrs::vec_cast(years(1L), character()), format(years(1L)))
  expect_equal(vctrs::vec_cast(months(3L), character()), format(months(3L)))
  expect_type(vctrs::vec_cast(years(1:3), character()), "character")
  expect_length(vctrs::vec_cast(years(1:3), character()), 3L)
})

test_that("inner time vectors are S7 objects of the right class", {
  lin <- yearmonth(0:3)@x[[1L]]
  cyc <- month_of_year(0:3)@x[[1L]]
  dur <- days(0:3)@x[[1L]]
  expect_true(S7::S7_inherits(lin, mt_linear))
  expect_true(S7::S7_inherits(cyc, mt_cyclical))
  expect_true(S7::S7_inherits(dur, mt_duration))
  # cyclical carries a typed `cycle` property; chronon is a typed `mt_unit`
  expect_true(S7::S7_inherits(cyc@cycle, mt_unit))
  expect_true(S7::S7_inherits(lin@chronon, mt_unit))
})

# vec_cast() from character (mt_cast_from_character() in vctrs.R) parses via
# time_parse_impl(), using the target's chronon/cycle to pick candidate
# formats. This is what lets comparison operators (see mt_linear-compare /
# mt_cyclical-compare in compare.R) accept a bare character operand.
test_that("vec_cast() parses character onto a linear time vector's own chronon", {
  to <- yearmonth(0L)@x[[1L]]
  cast <- vctrs::vec_cast("2020 Jan", to)
  expect_true(S7::S7_inherits(cast, mt_linear))
  expect_equal(cast, yearmonth("2020 Jan")@x[[1L]])
})

test_that("vec_cast() parses character onto a cyclical time vector's own chronon/cycle", {
  to <- month_of_year(0L)@x[[1L]]
  cast <- vctrs::vec_cast("Feb", to)
  expect_true(S7::S7_inherits(cast, mt_cyclical))
  expect_equal(cast, month_of_year(date("2020-02-15"))@x[[1L]])
})

test_that("vec_cast() from character fills in unset chronon properties (e.g. tz) from the target", {
  to <- datetime(0L, tz = "Australia/Melbourne")@x[[1L]]
  cast <- vctrs::vec_cast("2020-06-01 05:00:00", to)
  expect_equal(format(cast), "2020-06-01 05:00:00 AEST")
})

test_that("vec_cast() from character errors on unparseable text", {
  to <- yearmonth(0L)@x[[1L]]
  expect_error(vctrs::vec_cast("not a date", to), class = "mixtime_parse_no_match")
})

test_that("vec_cast() from character is not supported for durations (no meaningful text form)", {
  to <- days(1L)@x[[1L]]
  expect_error(vctrs::vec_cast("2 days", to), class = "vctrs_error_incompatible_type")
})

test_that("comparison operators accept a bare character operand, parsed onto the other side's chronon", {
  ym <- yearmonth("2020 Jan")

  expect_true(ym > "2019 Jan")
  expect_true("2019 Jan" < ym)
  expect_true(ym == "2020 Jan")
  expect_true(ym != "2019 Jan")
  expect_true(ym <= "2020 Jan")
  expect_true(ym >= "2020 Jan")

  expect_true(date("2020-01-15") > "2020-01-01")
  expect_true(month_of_year(date("2020-01-15")) == "Jan")
})

test_that("mode of time (integer/double) survives vec_slice/vec_c/vec_restore", {
  disc <- yearmonth(0:3)@x[[1L]]              # discrete -> integer data
  cont <- yearmonth(as.Date("2024-03-15") + 0:3, discrete = FALSE)@x[[1L]]  # continuous -> double data
  expect_type(vctrs::vec_data(disc), "integer")
  expect_type(vctrs::vec_data(cont), "double")
  expect_type(vctrs::vec_data(vctrs::vec_slice(disc, 2:3)), "integer")
  expect_type(vctrs::vec_data(vctrs::vec_c(disc, disc)), "integer")
  expect_type(vctrs::vec_data(vctrs::vec_c(cont, cont)), "double")
  # round-trip through the mixtime/vecvec container preserves class and type
  combined <- c(yearmonth(0:1), yearmonth(2:3))
  expect_s7_class(combined, class_mixtime)
  expect_type(vctrs::vec_data(combined@x[[1L]]), "integer")
})
