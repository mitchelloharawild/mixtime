# Tests for time_parse(): the inverse of format(), turning text back into a
# mixtime using the same {lin(...)}/{cyc(...)} format string. Invalid results
# behave like readr::parse_datetime() -- NA plus a summary warning -- rather
# than time_compose()'s own always-abort-on-any-bad-row behaviour.

test_that("parses a date from year/month/day tokens", {
  r <- time_parse("1980-03-15", "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")

  expect_equal(format(r), "1980-03-15")
  expect_equal(
    as.numeric(vecvec::unvecvec(r)),
    as.numeric(vecvec::unvecvec(time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3, cyc(day, month) ~ 15)))
  )
})

test_that("round-trips through format()", {
  x <- as_mixtime(as.Date("2024-03-15") + 0:5)
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  txt <- format(x, fmt)
  r <- time_parse(txt, fmt)

  expect_equal(as.numeric(vecvec::unvecvec(r)), as.numeric(vecvec::unvecvec(x)))
})

test_that("decodes in chain order, not text order", {
  # day comes before month/year in the text, but a day's cardinality (is the
  # 29th valid?) depends on the month/year that appear later -- decoding
  # must follow the lin()->cyc() chain, not the token's position in the text.
  fmt <- "{cyc(day, month)} {cyc(month, year, label = TRUE)} {lin(year)}"

  expect_equal(format(time_parse("29 Feb 2024", fmt)), "2024-02-29")
  expect_warning(time_parse("29 Feb 2023", fmt), "failed to parse")
})

test_that("parses abbreviated and wide named labels", {
  fmt_abbr <- "{lin(year)} {cyc(month, year, label = TRUE)} {cyc(day, month)}"
  fmt_wide <- "{lin(year)} {cyc(month, year, label = TRUE, abbreviate = FALSE)} {cyc(day, month)}"

  expect_equal(format(time_parse("2024 Feb 15", fmt_abbr)), "2024-02-15")
  expect_equal(format(time_parse("2024 February 15", fmt_wide)), "2024-02-15")
})

test_that("literal template text tolerates whitespace runs", {
  fmt <- "{lin(year)} {cyc(month, year)} {cyc(day, month)}"

  expect_equal(format(time_parse("2024   02   15", fmt)), "2024-02-15")
})

test_that("BC/AD year labels round-trip (a granule overriding the generics directly)", {
  expect_equal(format(time_parse("44BC", "{lin(year)}")), "44BC")
  expect_equal(format(time_parse("1980", "{lin(year)}")), "1980")
})

test_that("na values become NA without a parse-failure warning", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  r <- time_parse(c("1980-03-15", "", NA, "NA"), fmt)

  expect_equal(is.na(r), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("a custom na vector is respected", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  r <- time_parse(c("1980-03-15", "missing"), fmt, na = "missing")

  expect_equal(is.na(r), c(FALSE, TRUE))
})

test_that("one bad value becomes NA with a warning, the rest still parse", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(
    r <- time_parse(c("1980-03-15", "not a date"), fmt),
    "1 value failed to parse"
  )
  expect_equal(format(r), c("1980-03-15", "NA"))
})

test_that("an invalid cardinality (day 30 of February) becomes NA with a warning", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(r <- time_parse("2024-02-30", fmt), "failed to parse")
  expect_true(is.na(r))
})

test_that("multiple bad values are all reported and set to NA", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(
    r <- time_parse(c("2024-02-30", "1980-03-15", "xx", "2024-13-01"), fmt),
    "3 values failed to parse"
  )
  expect_equal(is.na(r), c(TRUE, FALSE, TRUE, TRUE))
})

test_that("a format with no lin() anchor parses to cyclical time", {
  r <- time_parse("Feb", "{cyc(month, year, label = TRUE)}")

  expect_true(time_is_cyclical(r))
  expect_equal(format(r), "Feb")
  expect_equal(
    as.numeric(vecvec::unvecvec(r)),
    as.numeric(vecvec::unvecvec(month_of_year(as.Date("1970-02-01"))))
  )
})

test_that("a chained cyc()-only format parses to cyclical time", {
  # Collapses to (day, year): day-of-year 74, same as day_of_year() for 15 March.
  r <- time_parse("15 Mar", "{cyc(day, month)} {cyc(month, year, label = TRUE)}")

  expect_true(time_is_cyclical(r))
  expect_equal(format(r), "D74")
  expect_equal(
    as.numeric(vecvec::unvecvec(r)),
    as.numeric(vecvec::unvecvec(day_of_year(as.Date("1970-03-15"))))
  )
})

test_that("errors immediately for a cyc()-only format with no coarsest link", {
  expect_error(
    time_parse("1 15", "{cyc(month, day)} {cyc(day, month)}"),
    "coarsest link"
  )
})

test_that("errors immediately for a disconnected cyc() chain", {
  expect_error(time_parse("15", "{lin(year)} {cyc(day, month)}"), "does not connect")
})

test_that("an empty input returns a length-0 mixtime", {
  r <- time_parse(character(0), "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")

  expect_length(r, 0L)
  expect_true(is_mixtime(r))
})

test_that("multiple formats: the first one under which every value parses is used", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # Both values already fit the first format, so it's used outright.
  r <- time_parse(c("2024-02-15", "2024-03-20"), fmts)
  expect_equal(format(r), c("2024-02-15", "2024-03-20"))
})

test_that("multiple formats: a format that doesn't parse everything is skipped entirely", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # Neither value fits the first ("Y-M-D") format, so the second ("D/M/Y")
  # is used for both instead.
  r <- time_parse(c("15/02/2024", "20/03/2024"), fmts)
  expect_equal(format(r), c("2024-02-15", "2024-03-20"))
})

test_that("multiple formats: formats are never mixed value-by-value", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # "2024-02-15" only fits the first format and "15/02/2024" only fits the
  # second -- neither format parses *both*, so each parses exactly one value
  # and the tie goes to the earliest-listed format, not opportunistically
  # mixing formats value-by-value.
  expect_warning(
    r <- time_parse(c("2024-02-15", "15/02/2024"), fmts),
    "1 value failed to parse"
  )
  expect_equal(format(r), c("2024-02-15", "NA"))
})

test_that("multiple formats: the format that parses the most values wins, not the last one", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # The first ("Y-M-D") format parses 2 of 3 values; the second ("D/M/Y")
  # parses only 1, so the first format wins on coverage.
  expect_warning(
    r <- time_parse(c("2024-02-15", "2024-03-20", "15/02/2024"), fmts),
    "1 value failed to parse"
  )
  expect_equal(format(r), c("2024-02-15", "2024-03-20", "NA"))
})

test_that("multiple formats: a partial win from a non-last format isn't discarded", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # The first format parses 2 of 3 values; the third value's shape doesn't
  # match either format, so the second format matches nothing at all.
  expect_warning(
    r <- time_parse(c("2024-02-15", "2024-03-20", "not-a-date"), fmts),
    "1 value failed to parse"
  )
  expect_equal(format(r), c("2024-02-15", "2024-03-20", "NA"))
})

test_that("a format that matches but is invalid falls through to the next format", {
  # Both formats share the same literal shape ("{lin(year)}-{cyc(X, year)}"),
  # so "2024-60" matches the regex of both -- but 60 is not a valid quarter
  # (max 4), so it should fall through to being read as a day-of-year (max
  # 366) instead of failing outright.
  fmts <- c(
    "{lin(year)}-{cyc(quarter, year)}",
    "{lin(year)}-{cyc(day, year)}"
  )

  r <- time_parse("2024-60", fmts)
  expect_equal(format(r), "2024-02-29")
})

test_that("errors when no candidate format matches the shape of any value", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_error(time_parse("not a date at all", fmt), class = "mixtime_parse_no_match")
})

test_that("multiple formats: still errors when none of them shape-match anything", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  expect_error(time_parse("nonsense", fmts), class = "mixtime_parse_no_match")
})

test_that("a value that shape-matches but fails to parse still becomes NA, not an error", {
  # Shape-matches fine; only the cardinality (day 30 of February) is invalid.
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(r <- time_parse("2024-02-30", fmt), "failed to parse")
  expect_true(is.na(r))
})

test_that("continuous (discrete = FALSE) parsing keeps a fractional chronon", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  r <- time_parse("1980-03-15", fmt, discrete = FALSE)

  expect_true(is.double(vecvec::unvecvec(r)))
  expect_equal(format(r), "1980-03-15 0.0%")
})

test_that("regex = FALSE (default) treats literal metacharacters literally", {
  # "." between tokens is a literal dot, not a "match any char" wildcard.
  fmt <- "{lin(year)}.{cyc(month, year)}.{cyc(day, month)}"

  expect_equal(format(time_parse("2024.02.15", fmt)), "2024-02-15")
  expect_error(time_parse("2024x02x15", fmt), class = "mixtime_parse_no_match")
})

test_that("regex = TRUE uses literal template text as a regular expression", {
  # Same "." separator, now matched as a wildcard rather than escaped.
  fmt <- "{lin(year)}.{cyc(month, year)}.{cyc(day, month)}"

  expect_equal(format(time_parse("2024x02x15", fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE supports character classes for alternative separators", {
  fmt <- "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}"

  expect_equal(format(time_parse("2024-02-15", fmt, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("2024/02/15", fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE supports trailing free text via a wildcard", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}.*"

  expect_equal(
    format(time_parse("2024-02-15 (approx)", fmt, regex = TRUE)),
    "2024-02-15"
  )
})

test_that("regex = TRUE does not collapse whitespace runs (unlike the default escaping)", {
  fmt <- "{lin(year)} {cyc(month, year)} {cyc(day, month)}"

  # Default: any run of spaces is tolerated.
  expect_equal(format(time_parse("2024   02   15", fmt)), "2024-02-15")
  # regex = TRUE: the literal " " is matched exactly, so extra spaces don't fit.
  expect_error(
    time_parse("2024   02   15", fmt, regex = TRUE),
    class = "mixtime_parse_no_match"
  )
  expect_equal(format(time_parse("2024 02 15", fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE treats a user's capturing group as non-capturing", {
  # A plain "(...)" group here would otherwise shift the positional group
  # indices used to extract the {lin(...)}/{cyc(...)} tokens' text out of
  # alignment; it must be silently treated as "(?:...)" instead.
  fmt <- "{lin(year)}(-){cyc(month, year)}-{cyc(day, month)}"

  expect_equal(format(time_parse("2024-02-15", fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE: an auto-converted capturing group composes with alternation and optionality", {
  fmt <- "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}( .*)?"

  expect_equal(format(time_parse("2024-02-15", fmt, regex = TRUE)), "2024-02-15")
  expect_equal(
    format(time_parse("2024/02/15 (approx)", fmt, regex = TRUE)),
    "2024-02-15"
  )
})

test_that("regex = TRUE leaves already-special groups (non-capturing, lookaround) untouched", {
  fmt_noncapture <- "{lin(year)}(?:-){cyc(month, year)}-{cyc(day, month)}"
  fmt_lookahead <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}(?=$)"

  expect_equal(format(time_parse("2024-02-15", fmt_noncapture, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("2024-02-15", fmt_lookahead, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE still needs doubled braces to write a literal '{n}' quantifier", {
  # mt_glue_fmt() treats a single "{...}" as an expression to evaluate, so a
  # literal regex quantifier like "\\d{2}" must be written with doubled
  # braces to survive as text. Trailing "\\d{2}" here isn't captured by a
  # cyc() token, so the parsed chronon stops at month.
  fmt <- "{lin(year)}-{cyc(month, year)}-\\d{{2}}"

  expect_equal(format(time_parse("2024-02-15", fmt, regex = TRUE)), "2024 Feb")
  expect_error(time_parse("2024-02-1", fmt, regex = TRUE), class = "mixtime_parse_no_match")
})

test_that("parse_format() records its regex mode as an attribute", {
  expect_equal(attr(parse_format("{lin(year)}"), "regex"), FALSE)
  expect_equal(attr(parse_format("{lin(year)}", regex = TRUE), "regex"), TRUE)
  expect_equal(
    as.character(parse_format("{lin(year)}", "{cyc(month, year)}")),
    c("{lin(year)}", "{cyc(month, year)}")
  )
})

test_that("a format's regex attribute (from parse_format()) doesn't override time_parse()'s regex argument", {
  fmt <- parse_format("{lin(year)}.{cyc(month, year)}.{cyc(day, month)}", regex = TRUE)

  # regex = TRUE is baked into `fmt` as an attribute, but time_parse() only
  # looks at its own `regex` argument, so "." is still matched literally by
  # default and "2024x02x15" doesn't match.
  expect_error(
    time_parse("2024x02x15", fmt),
    class = "mixtime_parse_no_match"
  )
  # Passing regex = TRUE explicitly is what's needed for "." to match as a
  # wildcard.
  expect_equal(format(time_parse("2024x02x15", fmt, regex = TRUE)), "2024-02-15")
})

test_that("chronon_parse_linear()/chronon_parse_cyclical() candidates are built with parse_format()", {
  # cal_gregorian$day's candidates use regex = TRUE for tolerant separators
  expect_true(attr(chronon_parse_linear(cal_gregorian$day(1L)), "regex"))
  expect_false(attr(chronon_parse_cyclical(cal_gregorian$month(1L), cal_gregorian$year(1L)), "regex"))
})

test_that("chronon_parse_linear()/chronon_parse_cyclical() tolerate irregular separators and casing", {
  # These candidates are all built with parse_format(..., regex = TRUE), so
  # regex = TRUE must be passed to time_parse() explicitly - the attribute
  # on the format vector no longer does this automatically.

  # Gregorian day: mixed separators/whitespace and D/M/Y vs. M/D/Y vs. Y/M/D
  fmt_day <- chronon_parse_linear(cal_gregorian$day(1L))
  expect_equal(format(time_parse("2024-02-15", fmt_day, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("2024/02/15", fmt_day, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("15 Feb 2024", fmt_day, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("Feb 15, 2024", fmt_day, regex = TRUE)), "2024-02-15")

  # Gregorian month: label/numeric and either order, various separators
  fmt_month <- chronon_parse_linear(cal_gregorian$month(1L))
  expect_equal(format(time_parse("2024 Feb", fmt_month, regex = TRUE)), "2024 Feb")
  expect_equal(format(time_parse("Feb/2024", fmt_month, regex = TRUE)), "2024 Feb")

  # ISO week: "W" designator is optional and case-insensitive
  fmt_week <- chronon_parse_cyclical(cal_isoweek$week(1L), cal_isoweek$year(1L))
  expect_equal(
    time_parse("w03", fmt_week, calendar = cal_isoweek, regex = TRUE),
    time_parse("W03", fmt_week, calendar = cal_isoweek, regex = TRUE)
  )
  expect_equal(
    time_parse("3", fmt_week, calendar = cal_isoweek, regex = TRUE),
    time_parse("W03", fmt_week, calendar = cal_isoweek, regex = TRUE)
  )

  # ISO week linear: separator between year/week and week/day is flexible
  fmt_isoday <- chronon_parse_linear(cal_isoweek$day(1L))
  expect_equal(
    time_parse("2024-W07-Mon", fmt_isoday, calendar = cal_isoweek, regex = TRUE),
    time_parse("2024 w07 Monday", fmt_isoday, calendar = cal_isoweek, regex = TRUE)
  )

  # Symmetry454 day: Y-M-W-D and D-W-M-Y agree
  fmt_sym454day <- chronon_parse_linear(cal_sym454$day(1L))
  expect_equal(
    time_parse("2024-Jan-W2-3", fmt_sym454day, calendar = cal_sym454, regex = TRUE),
    time_parse("3rd-W2-Jan-2024", fmt_sym454day, calendar = cal_sym454, regex = TRUE)
  )
})
