test_that("circsum works with basic examples", {
  # Example from documentation
  expect_equal(circsum(c(1, 2, 3, 4), 2), c(3, 7))
  expect_equal(circsum(c(1, 2, 3, 4, 5), 3), c(6, 10, 9, 8, 12))
  
  # Size equals step (default behavior)
  expect_equal(circsum(c(1, 2, 3, 4), 2, 2), c(3, 7))
  
  # Different step sizes
  expect_equal(circsum(c(1, 2, 3, 4), 2, 1), c(3, 5, 7, 5))
})

test_that("circsum works with month days", {
  monthdays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  expect_equal(circsum(monthdays, 1L), monthdays)
  expect_equal(circsum(monthdays, 2L), c(59, 61, 61, 62, 61, 61))
  expect_equal(circsum(monthdays, 3L), c(90, 91, 92, 92))
  expect_equal(circsum(monthdays, 4L), c(120, 123, 122))
  expect_equal(circsum(monthdays, 9L), c(273, 273, 274, 275))
  expect_equal(circsum(monthdays, 13L), c(396, 393, 396, 395, 396, 395, 396, 396, 395, 396, 395, 396))
})

test_that("circsum handles edge cases", {
  # Empty vector
  expect_equal(circsum(numeric(0), 2), numeric(0))
  
  # Size of 1
  expect_equal(circsum(c(1, 2, 3), 1), c(1, 2, 3))
  
  # Size equals length
  expect_equal(circsum(c(1, 2, 3, 4), 4), c(10))
  
  # Size greater than length (wraps around)
  expect_equal(circsum(c(1, 2, 3), 5), c(9, 10, 11))
})

test_that("circsum handles invalid inputs", {
  expect_equal(circsum(c(1, 2, 3), 0), numeric(0))
  expect_equal(circsum(c(1, 2, 3), -1), numeric(0))
  expect_equal(circsum(c(1, 2, 3), 2, 0), numeric(0))
  expect_equal(circsum(c(1, 2, 3), 2, -1), numeric(0))
})

test_that("circsum step parameter works correctly", {
  # Step larger than size
  expect_equal(circsum(c(1, 2, 3, 4, 5), 2, 3), c(3, 9, 5, 6, 7))
  
  # Step of 1 with size 2
  expect_equal(circsum(c(1, 2, 3, 4), 2, 1), c(3, 5, 7, 5))
})

test_that("circsum shortcut: size=1, step=1 returns input vector", {
  # This tests the first shortcut path: returns x directly
  x <- c(1, 2, 3, 4, 5)
  expect_identical(circsum(x, 1, 1), x)
  
  # Single element
  expect_identical(circsum(10, 1, 1), 10)
  
  # Larger vector
  x <- 1:100
  expect_identical(circsum(x, 1, 1), x)
  
  # Non-integer values
  x <- c(1.5, 2.7, 3.2)
  expect_identical(circsum(x, 1, 1), x)
})

test_that("circsum shortcut: size=1, step>1 uses GCD-based indexing", {
  # This tests the second shortcut path
  
  # Step of 2 with vector length 4 (GCD=2, num_windows=2)
  expect_equal(circsum(c(1, 2, 3, 4), 1, 2), c(1, 3))
  
  # Step of 2 with vector length 5 (GCD=1, num_windows=5)
  expect_equal(circsum(c(1, 2, 3, 4, 5), 1, 2), c(1, 3, 5, 2, 4))
  
  # Step of 3 with vector length 6 (GCD=3, num_windows=2)
  expect_equal(circsum(c(10, 20, 30, 40, 50, 60), 1, 3), c(10, 40))
  
  # Step of 3 with vector length 7 (GCD=1, num_windows=7)
  expect_equal(circsum(c(1, 2, 3, 4, 5, 6, 7), 1, 3), c(1, 4, 7, 3, 6, 2, 5))
  
  # Step equals length (wraps back to start)
  expect_equal(circsum(c(1, 2, 3, 4), 1, 4), c(1))
  
  # Step of 5 with vector length 10 (GCD=5, num_windows=2)
  expect_equal(circsum(1:10, 1, 5), c(1, 6))
})

test_that("circsum shortcut: size=1 with various step sizes", {
  # Step larger than length
  expect_equal(circsum(c(1, 2, 3), 1, 5), c(1, 3, 2))
  
  # Prime number step with prime length
  expect_equal(circsum(c(10, 20, 30, 40, 50), 1, 3), c(10, 40, 20, 50, 30))
  
  # Step coprime with length
  expect_equal(circsum(c(1, 2, 3, 4, 5, 6), 1, 5), c(1, 6, 5, 4, 3, 2))
})