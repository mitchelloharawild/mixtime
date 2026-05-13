# https://github.com/mitchelloharawild/mixtime/issues/81
test_that("vec_detect_complete() recycles hashed chronons correctly (#81)", {
  expect_true(all(vctrs::vec_detect_complete(yearmonth(0:3))))
})

test_that("vec_detect_complete() correctly identifies missing values", {
  expect_equal(vctrs::vec_detect_complete(yearmonth(c(0:3, NA))), c(TRUE, TRUE, TRUE, TRUE, FALSE))
})
