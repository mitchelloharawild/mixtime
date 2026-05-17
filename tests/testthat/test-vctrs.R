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
