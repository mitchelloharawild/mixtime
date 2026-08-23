# Tests for time_parse(): the inverse of format(), turning text back into a
# mixtime using the same {lin(...)}/{cyc(...)} format string. Invalid results
# behave like readr::parse_datetime() -- NA plus a summary warning -- rather
# than time_compose()'s own always-abort-on-any-bad-row behaviour.

test_that("parses a date from year/month/day tokens", {
  r <- time_parse("1980-03-15", format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")

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
  r <- time_parse(txt, format = fmt)

  expect_equal(as.numeric(vecvec::unvecvec(r)), as.numeric(vecvec::unvecvec(x)))
})

test_that("decodes in chain order, not text order", {
  # day comes before month/year in the text, but a day's cardinality (is the
  # 29th valid?) depends on the month/year that appear later -- decoding
  # must follow the lin()->cyc() chain, not the token's position in the text.
  fmt <- "{cyc(day, month)} {cyc(month, year, label = TRUE)} {lin(year)}"

  expect_equal(format(time_parse("29 Feb 2024", format = fmt)), "2024-02-29")
  expect_warning(time_parse("29 Feb 2023", format = fmt), "failed to parse")
})

test_that("parses abbreviated and wide named labels", {
  fmt_abbr <- "{lin(year)} {cyc(month, year, label = TRUE)} {cyc(day, month)}"
  fmt_wide <- "{lin(year)} {cyc(month, year, label = TRUE, abbreviate = FALSE)} {cyc(day, month)}"

  expect_equal(format(time_parse("2024 Feb 15", format = fmt_abbr)), "2024-02-15")
  expect_equal(format(time_parse("2024 February 15", format = fmt_wide)), "2024-02-15")
})

test_that("literal template text tolerates whitespace runs", {
  fmt <- "{lin(year)} {cyc(month, year)} {cyc(day, month)}"

  expect_equal(format(time_parse("2024   02   15", format = fmt)), "2024-02-15")
})

test_that("BC/AD year labels round-trip (a granule overriding the generics directly)", {
  expect_equal(format(time_parse("44BC", format = "{lin(year)}")), "44BC")
  expect_equal(format(time_parse("1980", format = "{lin(year)}")), "1980")
})

test_that("na values become NA without a parse-failure warning", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  r <- time_parse(c("1980-03-15", "", NA, "NA"), format = fmt)

  expect_equal(is.na(r), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("a custom na vector is respected", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  r <- time_parse(c("1980-03-15", "missing"), format = fmt, na = "missing")

  expect_equal(is.na(r), c(FALSE, TRUE))
})

test_that("one bad value becomes NA with a warning, the rest still parse", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(
    r <- time_parse(c("1980-03-15", "not a date"), format = fmt),
    "1 value failed to parse"
  )
  expect_equal(format(r), c("1980-03-15", "NA"))
})

test_that("an invalid cardinality (day 30 of February) becomes NA with a warning", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(r <- time_parse("2024-02-30", format = fmt), "failed to parse")
  expect_true(is.na(r))
})

test_that("multiple bad values are all reported and set to NA", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(
    r <- time_parse(c("2024-02-30", "1980-03-15", "xx", "2024-13-01"), format = fmt),
    "3 values failed to parse"
  )
  expect_equal(is.na(r), c(TRUE, FALSE, TRUE, TRUE))
})

test_that("a single invalid row in a large batch doesn't fall back to retrying every row", {
  # compose_recompose(strict = FALSE) resolves its own cardinality-invalid rows
  # to NA in place (R/compose.R) rather than aborting the whole vectorized
  # compose, so this stays a single vectorized pass -- not O(m) individual
  # mixtime() constructions -- even with one bad row among many.
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  n <- 5000L
  x <- rep("2024-02-15", n)
  x[2500L] <- "2024-02-30"

  expect_warning(r <- time_parse(x, format = fmt), "1 value failed to parse")
  expect_equal(is.na(r), seq_len(n) == 2500L)
  expect_equal(format(r[-2500L]), rep("2024-02-15", n - 1L))
})

test_that("bad rows at different links in the chain each resolve to NA independently", {
  # "2024-13-01" is invalid at the month link; "2024-02-30" is invalid at the
  # day link (which itself depends on the already-decoded month); both must
  # become NA without disturbing the valid rows around them.
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  x <- c("2024-01-15", "2024-13-01", "2024-03-20", "2024-02-30", "2024-12-25")

  expect_warning(r <- time_parse(x, format = fmt), "2 values failed to parse")
  expect_equal(is.na(r), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(format(r[c(1L, 3L, 5L)]), c("2024-01-15", "2024-03-20", "2024-12-25"))
})

test_that("a calendar-author decode() that throws still falls back to per-row retry", {
  # Simulates label_scheme()'s documented "Irregular cycles" case (R/01_labels.R),
  # where decode() aborts for text that matched the token's regex but is
  # semantically invalid given `at` -- a failure compose_recompose()'s own
  # cardinality check can't catch, since it happens inside the decode closure
  # before that check ever runs. This must still isolate to the one bad row
  # via time_parse_attempt()'s retry loop, not abort the whole call.
  old <- S7::method(cyclical_labels_parse, list(cal_gregorian$month, cal_gregorian$year))
  withr_cleanup <- function() {
    S7::method(cyclical_labels_parse, list(cal_gregorian$month, cal_gregorian$year)) <- old
  }
  on.exit(withr_cleanup())

  S7::method(cyclical_labels_parse, list(cal_gregorian$month, cal_gregorian$year)) <- function(granule, cycle, ...) {
    list(
      pattern = "\\d+",
      decode = function(text, at = NULL) {
        n <- as.integer(text)
        if (any(n == 6L, na.rm = TRUE)) {
          cli::cli_abort("Month 6 is reserved in this test scheme.")
        }
        n - 1L
      }
    )
  }

  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  x <- c("2024-01-15", "2024-06-01", "2024-03-20")

  expect_warning(r <- time_parse(x, format = fmt), "1 value failed to parse")
  expect_equal(is.na(r), c(FALSE, TRUE, FALSE))
  expect_equal(format(r[c(1L, 3L)]), c("2024-01-15", "2024-03-20"))
})

test_that("a format with no lin() anchor parses to cyclical time", {
  r <- time_parse("Feb", format = "{cyc(month, year, label = TRUE)}")

  expect_true(time_is_cyclical(r))
  expect_equal(format(r), "Feb")
  expect_equal(
    as.numeric(vecvec::unvecvec(r)),
    as.numeric(vecvec::unvecvec(month_of_year(as.Date("1970-02-01"))))
  )
})

test_that("a chained cyc()-only format parses to cyclical time", {
  # Collapses to (day, year): day-of-year 74, same as day_of_year() for 15 March.
  r <- time_parse("15 Mar", format = "{cyc(day, month)} {cyc(month, year, label = TRUE)}")

  expect_true(time_is_cyclical(r))
  expect_equal(format(r), "D74")
  expect_equal(
    as.numeric(vecvec::unvecvec(r)),
    as.numeric(vecvec::unvecvec(day_of_year(as.Date("1970-03-15"))))
  )
})

test_that("errors immediately for a cyc()-only format with no coarsest link", {
  expect_error(
    time_parse("1 15", format = "{cyc(month, day)} {cyc(day, month)}"),
    "coarsest link"
  )
})

test_that("errors immediately for a disconnected cyc() chain", {
  expect_error(time_parse("15", format = "{lin(year)} {cyc(day, month)}"), "does not connect")
})

test_that("an empty input returns a length-0 mixtime", {
  r <- time_parse(character(0), format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")

  expect_length(r, 0L)
  expect_true(is_mixtime(r))
})

test_that("multiple formats: the first one under which every value parses is used", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # Both values already fit the first format, so it's used outright.
  r <- time_parse(c("2024-02-15", "2024-03-20"), format = fmts)
  expect_equal(format(r), c("2024-02-15", "2024-03-20"))
})

test_that("multiple formats: a format that doesn't parse everything is skipped entirely", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  # Neither value fits the first ("Y-M-D") format, so the second ("D/M/Y")
  # is used for both instead.
  r <- time_parse(c("15/02/2024", "20/03/2024"), format = fmts)
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
    r <- time_parse(c("2024-02-15", "15/02/2024"), format = fmts),
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
    r <- time_parse(c("2024-02-15", "2024-03-20", "15/02/2024"), format = fmts),
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
    r <- time_parse(c("2024-02-15", "2024-03-20", "not-a-date"), format = fmts),
    "1 value failed to parse"
  )
  expect_equal(format(r), c("2024-02-15", "2024-03-20", "NA"))
})

test_that("multiple formats: a tie is won by the earliest-listed format, even when tried later", {
  fmts <- c(
    "{lin(year)}-Q{cyc(quarter, year)}",
    "{lin(year)}-{cyc(day, year)}"
  )

  # Both formats end up parsing 2 of 5 values (a tie), but the second format
  # has a higher *sample* match count (its regex matches 3 of the 5 values,
  # vs. 2 for the first), so it's tried first. The earliest-listed format
  # must still win the tie once both are actually attempted, not whichever
  # was tried first.
  x <- c("2024-Q1", "2024-Q2", "2024-99", "2024-150", "2024-999")
  expect_warning(
    r <- time_parse(x, format = fmts),
    "3 values failed to parse"
  )
  expect_equal(format(r), c("2024 Q1", "2024 Q2", "NA", "NA", "NA"))
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

  r <- time_parse("2024-60", format = fmts)
  expect_equal(format(r), "2024-02-29")
})

test_that("errors when no candidate format matches the shape of any value", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_error(time_parse("not a date at all", format = fmt), class = "mixtime_parse_no_match")
})

test_that("multiple formats: still errors when none of them shape-match anything", {
  fmts <- c(
    "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
    "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
  )

  expect_error(time_parse("nonsense", format = fmts), class = "mixtime_parse_no_match")
})

test_that("a value that shape-matches but fails to parse still becomes NA, not an error", {
  # Shape-matches fine; only the cardinality (day 30 of February) is invalid.
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  expect_warning(r <- time_parse("2024-02-30", format = fmt), "failed to parse")
  expect_true(is.na(r))
})

test_that("continuous (discrete = FALSE) parsing keeps a fractional chronon", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  r <- time_parse("1980-03-15", format = fmt, discrete = FALSE)

  expect_true(is.double(vecvec::unvecvec(r)))
  expect_equal(format(r), "1980-03-15 0.0%")
})

test_that("no chronon override leaves the parsed chronon as the chain reached it", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)} {cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}"
  r <- time_parse("2024-02-15 09:00:00", format = fmt)

  expect_true(is.na(tz_name(time_chronon(r)@x[[1L]])))
  expect_equal(format(r), "2024-02-15 09:00:00")
})

test_that("chronon with a tz treats the decoded text as clock time in that zone", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)} {cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}"
  r <- time_parse("2024-02-15 09:00:00", format = fmt, chronon = cal_gregorian$second(1L, tz = "America/Los_Angeles"))

  expect_equal(format(r), "2024-02-15 09:00:00 PST")
  # Storage is UTC-referenced: the same clock time in UTC is 8 hours earlier.
  r_utc <- time_parse("2024-02-15 09:00:00", format = fmt)
  expect_equal(as.numeric(vecvec::unvecvec(r)) - as.numeric(vecvec::unvecvec(r_utc)), 8 * 3600)
})

test_that("chronon with tz = \"UTC\" is a no-op on the stored value", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)} {cyc(hour, day)}:{cyc(minute, hour)}:{cyc(second, minute)}"
  no_tz <- time_parse("2024-02-15 09:00:00", format = fmt)
  utc <- time_parse("2024-02-15 09:00:00", format = fmt, chronon = cal_gregorian$second(1L, tz = "UTC"))

  expect_equal(vecvec::unvecvec(no_tz), vecvec::unvecvec(utc))
})

test_that("chronon applies to empty and all-missing input without erroring", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
  chronon <- cal_gregorian$day(1L, tz = "Australia/Melbourne")

  r0 <- time_parse(character(0), format = fmt, chronon = chronon)
  expect_equal(length(r0), 0L)

  rna <- time_parse(NA_character_, format = fmt, chronon = chronon)
  expect_equal(length(rna), 1L)
  expect_true(is.na(rna))
})

test_that("mixtime() parses character data straight to a tz-aware chronon via time_parse()", {
  result <- mixtime("2020-06-01 05:00:00", chronon = cal_gregorian$second(1L, tz = "Australia/Melbourne"))
  expect_equal(format(result), "2020-06-01 05:00:00 AEST")
})

test_that("format = NULL derives candidate formats from chronon/cycle", {
  expect_equal(format(time_parse("2024-02-15", chronon = cal_gregorian$day(1L))), "2024-02-15")
  expect_equal(format(time_parse("3", chronon = cal_gregorian$month(1L), cycle = cal_gregorian$year(1L))), "Mar")
})

test_that("chronon converts the result when it reaches a coarser or finer chronon than format", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"

  r <- time_parse("2024-02-15", format = fmt, chronon = cal_gregorian$month(1L))
  expect_equal(format(r), "2024 Feb")
})

test_that("format = NULL without chronon errors", {
  expect_error(time_parse("2024-02-15"), "chronon.*format")
})

test_that("cycle without chronon errors", {
  expect_error(
    time_parse("Feb", cycle = cal_gregorian$year(1L)),
    "cycle.*chronon"
  )
})

test_that("regex = FALSE (default) treats literal metacharacters literally", {
  # "." between tokens is a literal dot, not a "match any char" wildcard.
  fmt <- "{lin(year)}.{cyc(month, year)}.{cyc(day, month)}"

  expect_equal(format(time_parse("2024.02.15", format = fmt)), "2024-02-15")
  expect_error(time_parse("2024x02x15", format = fmt), class = "mixtime_parse_no_match")
})

test_that("regex = TRUE uses literal template text as a regular expression", {
  # Same "." separator, now matched as a wildcard rather than escaped.
  fmt <- "{lin(year)}.{cyc(month, year)}.{cyc(day, month)}"

  expect_equal(format(time_parse("2024x02x15", format = fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE supports character classes for alternative separators", {
  fmt <- "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}"

  expect_equal(format(time_parse("2024-02-15", format = fmt, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("2024/02/15", format = fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE supports trailing free text via a wildcard", {
  fmt <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}.*"

  expect_equal(
    format(time_parse("2024-02-15 (approx)", format = fmt, regex = TRUE)),
    "2024-02-15"
  )
})

test_that("regex = TRUE does not collapse whitespace runs (unlike the default escaping)", {
  fmt <- "{lin(year)} {cyc(month, year)} {cyc(day, month)}"

  # Default: any run of spaces is tolerated.
  expect_equal(format(time_parse("2024   02   15", format = fmt)), "2024-02-15")
  # regex = TRUE: the literal " " is matched exactly, so extra spaces don't fit.
  expect_error(
    time_parse("2024   02   15", format = fmt, regex = TRUE),
    class = "mixtime_parse_no_match"
  )
  expect_equal(format(time_parse("2024 02 15", format = fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE treats a user's capturing group as non-capturing", {
  # A plain "(...)" group here would otherwise shift the positional group
  # indices used to extract the {lin(...)}/{cyc(...)} tokens' text out of
  # alignment; it must be silently treated as "(?:...)" instead.
  fmt <- "{lin(year)}(-){cyc(month, year)}-{cyc(day, month)}"

  expect_equal(format(time_parse("2024-02-15", format = fmt, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE: an auto-converted capturing group composes with alternation and optionality", {
  fmt <- "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}( .*)?"

  expect_equal(format(time_parse("2024-02-15", format = fmt, regex = TRUE)), "2024-02-15")
  expect_equal(
    format(time_parse("2024/02/15 (approx)", format = fmt, regex = TRUE)),
    "2024-02-15"
  )
})

test_that("regex = TRUE leaves already-special groups (non-capturing, lookaround) untouched", {
  fmt_noncapture <- "{lin(year)}(?:-){cyc(month, year)}-{cyc(day, month)}"
  fmt_lookahead <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}(?=$)"

  expect_equal(format(time_parse("2024-02-15", format = fmt_noncapture, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("2024-02-15", format = fmt_lookahead, regex = TRUE)), "2024-02-15")
})

test_that("regex = TRUE still needs doubled braces to write a literal '{n}' quantifier", {
  # mt_glue_fmt() treats a single "{...}" as an expression to evaluate, so a
  # literal regex quantifier like "\\d{2}" must be written with doubled
  # braces to survive as text. Trailing "\\d{2}" here isn't captured by a
  # cyc() token, so the parsed chronon stops at month.
  fmt <- "{lin(year)}-{cyc(month, year)}-\\d{{2}}"

  expect_equal(format(time_parse("2024-02-15", format = fmt, regex = TRUE)), "2024 Feb")
  expect_error(time_parse("2024-02-1", format = fmt, regex = TRUE), class = "mixtime_parse_no_match")
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
    time_parse("2024x02x15", format = fmt),
    class = "mixtime_parse_no_match"
  )
  # Passing regex = TRUE explicitly is what's needed for "." to match as a
  # wildcard.
  expect_equal(format(time_parse("2024x02x15", format = fmt, regex = TRUE)), "2024-02-15")
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
  expect_equal(format(time_parse("2024-02-15", format = fmt_day, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("2024/02/15", format = fmt_day, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("15 Feb 2024", format = fmt_day, regex = TRUE)), "2024-02-15")
  expect_equal(format(time_parse("Feb 15, 2024", format = fmt_day, regex = TRUE)), "2024-02-15")

  # Gregorian month: label/numeric and either order, various separators
  fmt_month <- chronon_parse_linear(cal_gregorian$month(1L))
  expect_equal(format(time_parse("2024 Feb", format = fmt_month, regex = TRUE)), "2024 Feb")
  expect_equal(format(time_parse("Feb/2024", format = fmt_month, regex = TRUE)), "2024 Feb")

  # ISO week: "W" designator is optional and case-insensitive
  fmt_week <- chronon_parse_cyclical(cal_isoweek$week(1L), cal_isoweek$year(1L))
  expect_equal(
    time_parse("w03", format = fmt_week, calendar = cal_isoweek, regex = TRUE),
    time_parse("W03", format = fmt_week, calendar = cal_isoweek, regex = TRUE)
  )
  expect_equal(
    time_parse("3", format = fmt_week, calendar = cal_isoweek, regex = TRUE),
    time_parse("W03", format = fmt_week, calendar = cal_isoweek, regex = TRUE)
  )

  # ISO week linear: separator between year/week and week/day is flexible
  fmt_isoday <- chronon_parse_linear(cal_isoweek$day(1L))
  expect_equal(
    time_parse("2024-W07-Mon", format = fmt_isoday, calendar = cal_isoweek, regex = TRUE),
    time_parse("2024 w07 Monday", format = fmt_isoday, calendar = cal_isoweek, regex = TRUE)
  )

  # Symmetry454 day: Y-M-W-D and D-W-M-Y agree
  fmt_sym454day <- chronon_parse_linear(cal_sym454$day(1L))
  expect_equal(
    time_parse("2024-Jan-W2-3", format = fmt_sym454day, calendar = cal_sym454, regex = TRUE),
    time_parse("3rd-W2-Jan-2024", format = fmt_sym454day, calendar = cal_sym454, regex = TRUE)
  )
})

test_that("chronon_parse_linear() for civil time-of-day chronons builds on the day chronon's own candidates", {
  # cal_gregorian$second's candidates are cal_gregorian$day's date candidates
  # (Y-M-D, D-M-Y, M-D-Y) each with a fixed "H:M:S" suffix pasted on -- the
  # parsing counterpart of chronon_format_linear()'s
  # paste(chronon_format_linear(cal$day(1L), cal), "{cyc(hour, day)}:...").
  fmt <- chronon_parse_linear(cal_gregorian$second(1L))
  expect_true(attr(fmt, "regex"))

  expect_equal(format(time_parse("2024-02-15 09:30:45", format = fmt, regex = TRUE)), "2024-02-15 09:30:45")
  # A date order other than Y-M-D only works because the suffix was combined
  # with every one of the day chronon's own parse candidates, not just its
  # single chronon_format_linear() default (which is always Y-M-D).
  expect_equal(format(time_parse("15 Feb 2024 09:30:45", format = fmt, regex = TRUE)), "2024-02-15 09:30:45")
  expect_equal(format(time_parse("Feb 15, 2024 09:30:45", format = fmt, regex = TRUE)), "2024-02-15 09:30:45")
})

test_that("chronon_parse_linear() for a plain (non-Gregorian) civil calendar falls back to a single date candidate, non-regex", {
  # cal_time_civil has no year/month, so its day chronon defers to the plain
  # mt_unit fallback -- chronon_parse_linear() should still work, combining
  # that single date candidate with each of the 24-hour/12-hour AM-PM
  # time-of-day variants, each joined by a space and by a bare "T" (4
  # candidates total: 1 date candidate x 2 clocks x 2 separators).
  fmt <- chronon_parse_linear(cal_time_civil$minute(1L))
  expect_false(isTRUE(attr(fmt, "regex")))
  expect_length(fmt, 4L)
})

test_that("chronon_parse_cyclical() for civil time-of-day-in-day chronons parses a bare clock time", {
  fmt <- chronon_parse_cyclical(cal_gregorian$second(1L), cal_gregorian$day(1L))
  r <- time_parse("09:30:45", format = fmt, calendar = cal_gregorian)

  expect_true(time_is_cyclical(r))
  expect_equal(format(r), "09:30:45")
})

test_that("mixtime() parses civil clock-time text using the new default chronon_parse_linear() candidates", {
  expect_equal(
    format(mixtime("15 Feb 2024 09:30:45", chronon = cal_gregorian$second(1L))),
    "2024-02-15 09:30:45"
  )
})

test_that("chronon_parse_linear() for civil time-of-day chronons also accepts an ISO 8601 'T' date-time separator", {
  fmt <- chronon_parse_linear(cal_gregorian$second(1L))
  expect_equal(
    format(time_parse("2024-02-15T09:30:45", format = fmt, regex = TRUE)),
    "2024-02-15 09:30:45"
  )
  # The 12-hour AM/PM suffix is also offered with a "T" separator.
  expect_equal(
    format(time_parse("2024-02-15T9:30:45 PM", format = fmt, regex = TRUE)),
    "2024-02-15 21:30:45"
  )
  expect_equal(
    format(mixtime("2024-02-15T09:30:45", chronon = cal_gregorian$second(1L))),
    "2024-02-15 09:30:45"
  )
})

test_that("chronon_parse_cyclical() for civil hour-in-day chronons parses a bare hour", {
  fmt <- chronon_parse_cyclical(cal_gregorian$hour(1L), cal_gregorian$day(1L))
  r24 <- time_parse("14h", format = fmt, calendar = cal_gregorian)
  rpm <- time_parse("2 PM", format = fmt, calendar = cal_gregorian)
  ram <- time_parse("2 AM", format = fmt, calendar = cal_gregorian)

  expect_true(time_is_cyclical(r24))
  expect_equal(format(r24, "{cyc(hour,day)}"), "14")
  expect_equal(format(rpm, "{cyc(hour,day)}"), "14")
  expect_equal(format(ram, "{cyc(hour,day)}"), "02")
})

test_that("chronon_parse_linear() for solar time-of-day chronons builds on the solar day chronon", {
  chronon <- cal_time_solar$second(1L, lat = -37.8136, lon = 144.9631)
  fmt <- chronon_parse_linear(chronon)

  r <- time_parse("5 09:30:45", format = fmt, calendar = cal_time_solar)
  r <- mixtime(r, chronon = chronon)
  expect_equal(format(r), "1970-01-06 09:30:45 [37.81S 144.96E]")
})

test_that("chronon_parse_linear() for solar arc chronons builds on the solar day chronon", {
  chronon <- cal_time_solar$arcsecond(1L, lat = -37.8136, lon = 144.9631)
  fmt <- chronon_parse_linear(chronon)

  r <- time_parse("5 090°15'30\"", format = fmt, calendar = cal_time_solar)
  r <- mixtime(r, chronon = chronon)
  expect_equal(format(r), "1970-01-06 090°15'30\" [37.81S 144.96E]")
})

test_that("chronon_parse_linear()/chronon_parse_cyclical() for civil time-of-day chronons also accept a 12-hour AM/PM clock", {
  fmt <- chronon_parse_linear(cal_gregorian$second(1L))
  expect_equal(format(time_parse("2024-02-15 9:30:45 PM", format = fmt, regex = TRUE)), "2024-02-15 21:30:45")
  expect_equal(format(time_parse("2024-02-15 9:30:45 AM", format = fmt, regex = TRUE)), "2024-02-15 09:30:45")
  # Midnight/noon are the 12-hour clock's edge cases: "12" means 0 (AM) or 12 (PM).
  expect_equal(format(time_parse("2024-02-15 12:00:00 AM", format = fmt, regex = TRUE)), "2024-02-15 00:00:00")
  expect_equal(format(time_parse("2024-02-15 12:00:00 PM", format = fmt, regex = TRUE)), "2024-02-15 12:00:00")

  fmt_cyc <- chronon_parse_cyclical(cal_gregorian$second(1L), cal_gregorian$day(1L))
  expect_equal(format(time_parse("9:30:45 PM", format = fmt_cyc, calendar = cal_gregorian)), "21:30:45")
})

test_that("chronon_parse_linear() for solar time-of-day chronons also accepts a 12-hour AM/PM clock", {
  chronon <- cal_time_solar$second(1L, lat = -37.8136, lon = 144.9631)
  fmt <- chronon_parse_linear(chronon)

  r_pm <- time_parse("5 9:30:45 PM", format = fmt, calendar = cal_time_solar)
  r_am <- time_parse("5 9:30:45 AM", format = fmt, calendar = cal_time_solar)
  expect_equal(
    as.numeric(vecvec::unvecvec(r_pm)) - as.numeric(vecvec::unvecvec(r_am)),
    12 * 3600
  )
})
