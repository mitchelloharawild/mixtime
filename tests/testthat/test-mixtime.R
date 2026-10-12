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

test_that("Comparison - mixed granularity discrete intervals (#95)", {
  # {1980} = [1980-01-01, 1980-12-31], {1980 Dec} = [1980-12-01, 1980-12-31]
  y1980 <- year(1980L)
  dec1980 <- yearmonth(as.integer((1980 - 1970) * 12 + 11))

  expect_false(y1980 == dec1980)
  expect_false(y1980 < dec1980)
  expect_false(y1980 > dec1980)
  expect_true(y1980 <= dec1980)   # same right endpoint (end of Dec 1980)
  expect_false(y1980 >= dec1980)  # year starts earlier than December

  # A month entirely before another year is strictly less than it
  expect_true(yearmonth(as.integer((1979 - 1970) * 12)) < y1980)
  # A month entirely after another year is strictly greater than it
  expect_true(yearmonth(as.integer((1981 - 1970) * 12)) > y1980)
})

test_that("Comparison - identical granules compare directly", {
  x <- yearmonth(648:650L)
  y <- yearmonth(649:651L)
  expect_equal(x < y, rep(TRUE, 3L))
  expect_equal(x == yearmonth(648:650L), rep(TRUE, 3L))
})

test_that("Comparison - continuous linear time compares as points", {
  x <- year(as.Date("2020-06-01"), discrete = FALSE)
  y <- yearmonth(as.Date("2020-01-01"), discrete = FALSE)
  expect_true(x > y)
  expect_true(y < x)
})

test_that("Comparison - continuous point within a discrete span", {
  point <- linear_time(as.Date("1980-06-15"), chronon = month(1L), discrete = FALSE)
  y1980 <- year(1980L)
  expect_true(point <= y1980)
  expect_true(point >= y1980)
  expect_false(point < y1980)
  expect_false(point > y1980)
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

# ----------------------------------------------------------------------------
# Chronon of Date and POSIXct (time_chronon())
# ----------------------------------------------------------------------------

test_that("Date/POSIXct chronons are identified from the observed interval", {
  chronon_of <- function(x) attr(time_chronon(x)@x[[1L]], "chronon")

  # Hourly datetimes have an hourly chronon
  x <- as.POSIXct("2024-01-01", tz = "UTC") + 3600 * (0:5)
  expect_equal(chronon_of(x), cal_gregorian$hour(1L, tz = "UTC"))
  expect_equal(chronon_glb(mixtime(x)), cal_gregorian$hour(1L, tz = "UTC"))
  expect_equal(as.POSIXct(mixtime(x), tz = "UTC"), x)

  # Multiple units are used when observations fall on the chronon boundaries
  x <- as.POSIXct("2024-01-01", tz = "UTC") + 3 * 3600 * (0:3)
  expect_equal(chronon_of(x)@n, 3)
  x <- as.POSIXct("2024-01-01", tz = "UTC") + 90 * 60 * (0:3)
  expect_equal(chronon_of(x), cal_gregorian$minute(90, tz = "UTC"))

  # ... otherwise a single unit is used to avoid losing information
  x <- as.POSIXct("2024-01-01 01:00", tz = "UTC") + 3 * 3600 * (0:3)
  expect_equal(chronon_of(x), cal_gregorian$hour(1L, tz = "UTC"))
  expect_equal(as.POSIXct(mixtime(x), tz = "UTC"), x)

  # Days are measured in local time, including across DST transitions
  x <- as.POSIXct(
    c("2024-03-29", "2024-03-30", "2024-03-31", "2024-04-01"),
    tz = "Europe/London"
  )
  expect_equal(chronon_of(x), cal_gregorian$day(1L, tz = "Europe/London"))

  # Sub-second intervals
  x <- as.POSIXct("2024-01-01", tz = "UTC") + 0.25 * (0:3)
  expect_equal(chronon_of(x), cal_gregorian$millisecond(250, tz = "UTC"))

  # Without an interval, the chronon is a second
  expect_equal(chronon_of(as.POSIXct("2024-01-01 01:00", tz = "UTC")), cal_gregorian$second(1L, tz = "UTC"))
  expect_equal(chronon_of(as.POSIXct(NA, tz = "UTC")), cal_gregorian$second(1L, tz = "UTC"))

  # Weekly dates use a 7 day chronon only when aligned to the epoch (Thursday)
  x <- as.Date("2024-01-04") + 7 * (0:3)
  expect_equal(chronon_of(x), cal_gregorian$day(7))
  expect_equal(as.Date(mixtime(x)), x)
  x <- as.Date("2024-01-01") + 7 * (0:3)
  expect_equal(chronon_of(x), cal_gregorian$day(1L))
  expect_equal(as.Date(mixtime(x)), x)

  # Without an interval, the chronon is a day
  expect_equal(chronon_of(as.Date("2024-01-01")), cal_gregorian$day(1L))
})

# TODO: Restore these to `expect_error(class = "mixtime_error_indeterminate")`
# once the deprecation warning is escalated to an error in the next release.
test_that("converting discrete time to a chronon it doesn't nest within is deprecated", {
  d <- as.Date("2026-10-12")
  # Which day (or week) of a month is indeterminate
  lifecycle::expect_deprecated(day_of_week(yearmonth(d)))
  lifecycle::expect_deprecated(date(yearmonth(d)))
  lifecycle::expect_deprecated(yearweek(yearmonth(d)))
  # A week can straddle two months
  lifecycle::expect_deprecated(yearmonth(yearweek(d)))
  lifecycle::expect_deprecated(month_of_year(yearweek(d)))
  # Base classes are discrete
  lifecycle::expect_deprecated(datetime(d))

  # Until then, each time point is converted from its start
  lifecycle::expect_deprecated(x <- date(yearmonth(d)))
  expect_equal(x, date(as.Date("2026-10-01")))

  # Coarser (nested) targets are determinate
  expect_equal(year(yearmonth(d)), year(d))
  expect_equal(month_of_year(date(d)), month_of_year(d))
  # Continuous time is an instant, so any chronon is determinate
  expect_equal(day_of_week(yearmonth(d, discrete = FALSE)), day_of_week(d))
})

test_that("chronon_convert() allows indeterminate conversion unless asked not to", {
  ym <- yearmonth(as.Date("2026-10-12"))
  day <- cal_gregorian$day(1L)
  expect_equal(chronon_convert(ym, day), as.numeric(as.Date("2026-10-01")))
  expect_error(
    chronon_convert(ym, day, allow_indeterminate = FALSE),
    class = "mixtime_error_indeterminate"
  )
  expect_error(
    chronon_convert(as.Date("2026-10-12"), cal_gregorian$hour(1L), allow_indeterminate = FALSE),
    class = "mixtime_error_indeterminate"
  )
})
