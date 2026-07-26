test_that("fdiv() and fdivmod() agree with %/% and %%", {
  x <- as.numeric(c(-1000:1000, -2^40, 2^40, -719468L, 719468L))

  for (y in c(1, 2, 5L, 7L, 12L, 60L, 146097L, 0.5, 1.5)) {
    expect_equal(fdiv(x, y), x %/% y, info = paste("divisor", y))

    dm <- fdivmod(x, y)
    expect_equal(dm$div, x %/% y, info = paste("divisor", y))
    expect_equal(dm$mod, x %% y, info = paste("divisor", y))
  }

  # Vectorised divisors (e.g. a context-dependent cardinality per element)
  y <- rep_len(c(28, 29, 30, 31), length(x))
  expect_equal(fdiv(x, y), x %/% y)
  expect_equal(fdivmod(x, y)$mod, x %% y)
})
