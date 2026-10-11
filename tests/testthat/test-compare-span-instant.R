span_instant_hint <- function(expr) {
  w <- tryCatch(expr, mixtime_warning_span_instant = function(w) w)
  expect_s3_class(w, "mixtime_warning_span_instant")
  gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
}

# Evaluate `code` as user code would be, outside of any package namespace
# (`R CMD check` runs tests inside mixtime's namespace).
user_code <- function(code, env = parent.frame()) {
  eval(substitute(code), list2env(as.list(env), parent = globalenv()))
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

  expect_match(span_instant_hint(user_code(x == 2026)), "use `2026L` instead of `2026`", fixed = TRUE)
  expect_match(span_instant_hint(user_code(x == -1)), "use `-1L` instead of `-1`", fixed = TRUE)
  expect_match(
    span_instant_hint(user_code(year(2015) != x)),
    "use `year(2015L)` instead of `year(2015)`", fixed = TRUE
  )
  expect_match(
    span_instant_hint(user_code(x == myvar)),
    "use `as.integer(myvar)` instead of `myvar`", fixed = TRUE
  )
  expect_match(
    span_instant_hint(user_code(x == mv)),
    "use `linear_time(mv)` instead of `mv`", fixed = TRUE
  )
  # evaluated in a data mask, as in dplyr::filter()
  expect_match(
    span_instant_hint(user_code(
      rlang::eval_tidy(quote(t == year(2015)), data = list(t = x))
    )),
    "use `year(2015L)` instead of `year(2015)`", fixed = TRUE
  )
})

test_that("the span/instant hint falls back when the code isn't the user's", {
  x <- year(2015L)
  mv <- year(2015)
  general <- "create both from integers (e.g. `year(2015L)` rather than `year(2015)`)"

  # names local to a package's code
  pkg_compare <- function(a, b) a == b
  environment(pkg_compare) <- asNamespace("stats")
  expect_match(span_instant_hint(pkg_compare(x, mv)), general, fixed = TRUE)
  expect_match(span_instant_hint(Map(`==`, list(x), list(mv))), general, fixed = TRUE)

  # an integer first argument needn't make a call setting `discrete` discrete
  expect_match(
    span_instant_hint(user_code(
      x == linear_time(2015, chronon = cal_gregorian$year(1L), discrete = FALSE)
    )),
    general, fixed = TRUE
  )

  # evaluated objects injected into the call
  expect_match(span_instant_hint(do.call("==", list(x, mv))), general, fixed = TRUE)
  expect_match(span_instant_hint(rlang::inject(x == !!mv)), general, fixed = TRUE)
})

test_that("ordering warns exactly when reading the instant as its span differs", {
  x <- year(2014:2018)
  for (op in c("<", "<=", ">", ">=")) {
    f <- match.fun(op)
    for (instant_left in c(FALSE, TRUE)) {
      compare <- function(y) if (instant_left) f(y, x) else f(x, y)
      differs <- !identical(compare(year(2016L)), suppressWarnings(compare(year(2016))))
      warned <- FALSE
      withCallingHandlers(
        compare(year(2016)),
        mixtime_warning_span_instant = function(w) {
          warned <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      expect(
        warned == differs,
        sprintf("`%s` with the instant on the %s: warned %s, differs %s",
          op, if (instant_left) "left" else "right", warned, differs)
      )
    }
  }
})

test_that("the ordering hint names the operator", {
  x <- year(2016L)
  expect_match(
    span_instant_hint(user_code(x > year(2016))),
    "`>` between time spans and instants compares with the instant, not the span it starts.",
    fixed = TRUE
  )
  expect_match(
    span_instant_hint(user_code(x <= year(2016))),
    "use `year(2016L)` instead of `year(2016)`", fixed = TRUE
  )
})

test_that("a mixed-granularity comparison warns once", {
  jan <- as.Date("2015-01-01")
  spans <- c(year(2015L), yearmonth(jan))
  instants <- c(year(2015), yearmonth(jan, discrete = FALSE))
  n <- 0L
  withCallingHandlers(
    spans == instants,
    mixtime_warning_span_instant = function(w) {
      n <<- n + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(n, 1L)
})

test_that("no span/instant hint when the comparison is intended", {
  expect_no_warning(year(2015L) == year(2015L))
  expect_no_warning(year(2015) == year(2015))
  # an instant part-way through a chronon is clearly an instant
  expect_no_warning(year(2015L) == year(2015.5))
  # ordering only by starts agrees whether the instant is read as a span
  expect_no_warning(year(2015L) >= year(2015))
  expect_no_warning(year(2015L) < year(2015))
  expect_no_warning(year(NA_integer_) == year(NA_real_))
})

test_that("bare numbers compare as mixtime() reads them, from the chronon's epoch", {
  expect_true(year(2026L) == 2026L)
  expect_false(year(2026L) == 56L)
  expect_true(yearmonth(as.Date("1970-02-01")) == 1L)
  # durations have no epoch
  expect_true(days(3L) == 3)
})
