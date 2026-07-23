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
