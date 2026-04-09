# ----------------------------------------------------------------------------
# Vector size (length(), vctrs::vec_size())
# ----------------------------------------------------------------------------

test_that("Size of mixtime vectors", {
  expect_equal(length(yearmonth(1:10L)), 10L)
  expect_equal(length(yearmonth(1L)),   1L)
  expect_equal(length(yearmonth(integer())), 0L)

  expect_equal(length(month_of_year(1:5L)), 5L)
  expect_equal(length(month_of_year(1L)), 1L)
  expect_equal(length(month_of_year(integer())), 0L)

  expect_equal(vctrs::vec_size(yearmonth(1:7L)), 7L)
  expect_equal(vctrs::vec_size(yearweek(1:4L)), 4L)
})

# ----------------------------------------------------------------------------
# Formatting (format())
# ----------------------------------------------------------------------------

test_that("Formatting mixtime vectors", {
  x <- yearmonth(648:650L)
  expect_equal(format(x), c("2024 Jan", "2024 Feb", "2024 Mar"))

  x <- yearweek(2818:2820L)
  expect_equal(format(x), c("2024 W01", "2024 W02", "2024 W03"))
})

# ---------------------------------------------------------------------------
# Missing values (is.na())
# ---------------------------------------------------------------------------

test_that("Missing values in mixtime vectors", {
  x <- yearmonth(1:4L)
  expect_equal(is.na(x), rep(FALSE, 4L))

  x <- yearmonth(c(1L, NA, 3L))
  expect_equal(is.na(x), c(FALSE, TRUE, FALSE))
})

# ---------------------------------------------------------------------------
# Arithmetic (+, -)
# ---------------------------------------------------------------------------

test_that("Arithmetic - addition and subtraction of integers", {
  x <- yearmonth(648:650L)
  result <- x + 2L
  expect_s7_class(result, class_mixtime)
  expect_equal(format(result), c("2024 Mar", "2024 Apr", "2024 May"))

  x <- yearweek(2818:2820L)
  result <- x + 2L
  expect_s7_class(result, class_mixtime)
  expect_equal(format(result), c("2024 W03", "2024 W04", "2024 W05"))
})

test_that("Arithmetic - subtraction of mixtime vectors", {
  x <- yearmonth(648:652L)
  result <- x - x
  expect_equal(as.integer(result), rep(0L, 5L))
})

# ---------------------------------------------------------------------------
#  Comparison (<, <=, ==, >=, >)
# ---------------------------------------------------------------------------

test_that("Comparison - inequalities", {
  x <- yearmonth(648:650L)
  expect_equal(x < yearmonth(648:650L), rep(FALSE, 3L))  # equal, not less

  x <- yearmonth(648:652L)
  y <- x + 1L
  expect_equal(y > x, rep(TRUE, 5L))

  y <- x - 1L
  expect_equal(y < x, rep(TRUE, 5L))
})

test_that("Comparison - equality", {
  x <- yearmonth(648:650L)
  expect_equal(x == x, rep(TRUE, 3L))
})


# ---------------------------------------------------------------------------
#  Indexing and subsetting ([, [[)
# ---------------------------------------------------------------------------


test_that("Subsetting - single-element", {
  x <- yearmonth(648:650L)
  expect_s7_class(x[1L], class_mixtime)
  expect_equal(length(x[1L]), 1L)
  expect_equal(format(x[1L]), "2024 Jan")
})

test_that("Subsetting - multi-element", {
  x <- yearmonth(648:652L)
  sub <- x[c(3L, 1L, 2L)]
  expect_s7_class(sub, class_mixtime)
  expect_equal(length(sub), 3L)
  expect_equal(format(sub), c("2024 Mar", "2024 Jan", "2024 Feb"))
})

test_that("Subsetting - negative indexing", {
  x <- yearmonth(648:650L)
  result <- x[-1L]
  expect_equal(length(result), 2L)
  expect_equal(format(result), c("2024 Feb", "2024 Mar"))
})

test_that("Subsetting - logical indexing", {
  x <- yearmonth(648:651L)
  result <- x[c(TRUE, FALSE, TRUE, FALSE)]
  expect_equal(length(result), 2L)
  expect_equal(format(result), c("2024 Jan", "2024 Mar"))
})

# ---------------------------------------------------------------------------
# Uniqueness (unique())
# ---------------------------------------------------------------------------

test_that("Uniqueness - unique() removes duplicates", {
  x <- yearmonth(648:652L)
  # rep() itself will fail until subsetting is fixed, so construct via + 0L
  duped <- c(x, x)
  result <- unique(duped)
  expect_s7_class(result, class_mixtime)
  expect_equal(length(result), 5L)

  x <- yearmonth(650:652L)
  expect_equal(length(unique(x)), 3L)
})

# ---------------------------------------------------------------------------
# Ordering (rev(), sort())
# ---------------------------------------------------------------------------

test_that("rev() reverses mixtime yearmonth", {
  x <- yearmonth(648:652L)
  result <- rev(x)
  expect_s7_class(result, class_mixtime)
  expect_equal(format(result), rev(format(x)))
})

test_that("sort() orders mixtime vectors", {
  x <- yearmonth(648:652L)
  x_rev <- rev(x)
  # Construct reversed by arithmetic rather than subsetting
  expect_equal(x, sort(x_rev))
  
  expect_equal(x_rev, sort(x, decreasing = TRUE))
})

# ---------------------------------------------------------------------------
# Combinations (c())
# ---------------------------------------------------------------------------

test_that("Combinations - c() combines same type vectors", {
  x <- yearmonth(1:2L)
  y <- yearmonth(3:5L)
  result <- c(x, y)
  expect_s7_class(result, class_mixtime)
  expect_equal(length(result), 5L)
})

test_that("Combinations - c() combines mixed type vectors", {
  x <- yearmonth(1:2L)
  y <- yearweek(3:5L)
  result <- c(x, y)
  expect_s7_class(result, class_mixtime)
  expect_equal(length(result), 5L)
})

test_that("Combinations - c() with NA", {
  x <- yearmonth(1:2L)
  result <- c(x, NA)
  expect_s7_class(result, class_mixtime)
  expect_equal(length(result), 3L)
  expect_equal(is.na(result), c(FALSE, FALSE, TRUE))
})
