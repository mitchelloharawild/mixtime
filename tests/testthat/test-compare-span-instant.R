span_instant_hint <- function(expr) {
  w <- tryCatch(expr, mixtime_warning_span_instant = function(w) w)
  expect_s3_class(w, "mixtime_warning_span_instant")
  cli::ansi_strip(conditionMessage(w))
}

test_that("comparing a span with an instant at its start warns", {
  span <- year(2015L)
  instant <- year(2015)

  expect_warning(res <- span == instant, class = "mixtime_warning_span_instant")
  expect_false(res)
  expect_warning(res <- instant != span, class = "mixtime_warning_span_instant")
  expect_true(res)
  expect_warning(
    year(datetime("2015-03-01 10:00:00", tz = "Australia/Melbourne")) == instant,
    class = "mixtime_warning_span_instant"
  )
})

test_that("the span/instant hint rewrites the code used for the instant", {
  x <- year(2015L)
  myvar <- 2015
  mv <- year(2015)

  expect_match(span_instant_hint(x == 2026), "use `2026L` instead of `2026`", fixed = TRUE)
  expect_match(span_instant_hint(x == -1), "use `-1L` instead of `-1`", fixed = TRUE)
  expect_match(
    span_instant_hint(year(2015) != x),
    "use `year(2015L)` instead of `year(2015)`", fixed = TRUE
  )
  expect_match(
    span_instant_hint(x == myvar),
    "use `as.integer(myvar)` instead of `myvar`", fixed = TRUE
  )
  expect_match(
    span_instant_hint(x == mv),
    "use `linear_time(mv)` instead of `mv`", fixed = TRUE
  )
  # dplyr re-signals the warning under its own class
  expect_warning(
    dplyr::filter(data.frame(t = x), t == year(2015)),
    "use `year(2015L)` instead of `year(2015)`", fixed = TRUE
  )
})

test_that("no span/instant hint when the comparison is intended", {
  expect_no_warning(year(2015L) == year(2015L))
  expect_no_warning(year(2015) == year(2015))
  # an instant part-way through a chronon is clearly an instant
  expect_no_warning(year(2015L) == year(2015.5))
  # ordering a span and an instant is well defined
  expect_no_warning(year(2015L) <= year(2015))
  expect_no_warning(year(NA_integer_) == year(NA_real_))
})

test_that("bare numbers compare as mixtime() reads them, from the chronon's epoch", {
  expect_true(year(2026L) == 2026L)
  expect_false(year(2026L) == 56L)
  expect_true(yearmonth(as.Date("1970-02-01")) == 1L)
  # durations have no epoch
  expect_true(days(3L) == 3)
})
