#' Default parsing format strings for chronons
#'
#' Provides candidate format strings for parsing text into a given chronon
#' (finest time granule), for use as the `format` argument of
#' [time_parse()]. Dispatches the same way as
#' [chronon_format_linear()]/[chronon_format_cyclical()], but returns every
#' common format instead of a single default, so [time_parse()] can try each
#' in turn and keep whichever parses the most values. Methods should build
#' their return value with [parse_format()].
#'
#' @inheritParams chronon_format_linear
#'
#' @return A character vector of format templates, ordered from most to
#'   least common, built with [parse_format()]. The first element typically
#'   matches [chronon_format_linear()] (for `chronon_parse_linear()`) or
#'   [chronon_format_cyclical()] (for `chronon_parse_cyclical()`) for the
#'   same chronon.
#'
#' @seealso [parse_format()] for building candidate format strings (what
#'   these methods are built from), [time_parse()] for using the result as
#'   candidate formats, [chronon_format_linear()]/[chronon_format_cyclical()]
#'   for the single default format these are based on.
#'
#' @export
#' @examples
#' chronon_parse_linear(cal_gregorian$year(1L))
#' chronon_parse_linear(cal_gregorian$month(1L))
#' chronon_parse_linear(cal_gregorian$day(1L))
#' chronon_parse_linear(cal_isoweek$day(1L))
#'
#' @rdname chronon_parse
chronon_parse_linear <- new_generic("chronon_parse_linear", c("x", "cal"), function(x, cal = time_calendar(x), ...) {
  S7::S7_dispatch()
})
method(chronon_parse_linear, list(mt_unit, class_any)) <- function(x, cal) {
  chronon_format_linear(x, cal)
}

#' @examples
#' chronon_parse_cyclical(cal_gregorian$month(1L), cal_gregorian$year(1L))
#' chronon_parse_cyclical(cal_isoweek$day(1L), cal_isoweek$week(1L))
#'
#' @rdname chronon_parse
#' @export
chronon_parse_cyclical <- new_generic("chronon_parse_cyclical", c("x", "y"))
method(chronon_parse_cyclical, list(mt_unit, mt_unit)) <- function(x, y) {
  chronon_format_cyclical(x, y)
}

#' Candidate format strings for parsing time
#'
#' Combines one or more format strings into a character vector for use as
#' the `format` argument of [time_parse()]. When multiple format strings are
#' given, [time_parse()] tries each in turn and keeps whichever parses the
#' most values, so `parse_format()` is the usual way to build up a set of
#' candidates to try (it's also how [chronon_parse_linear()]/
#' [chronon_parse_cyclical()] methods build theirs). It can optionally mark
#' its candidates as using regex syntax rather than literal text; see the
#' `regex` argument below.
#'
#' @param ... Format strings, as for the `format` argument of [time_parse()].
#' @param regex Whether the literal (non-token) text in `...` should be
#'   matched as regular expression syntax rather than escaped literally
#'   (e.g. `"[/-]"` to accept either `/` or `-` as a separator). This is a
#'   niche option for irregular text - most formats leave it at the
#'   default, `FALSE`. See the `regex` argument of [time_parse()] for
#'   details.
#'
#' @return A character vector of format strings suitable for [time_parse()],
#'   with a `"regex"` attribute for the `regex` argument the parser.
#' 
#' @seealso [time_parse()] for using the result as `format`,
#'   [chronon_parse_linear()]/[chronon_parse_cyclical()] for calendar-specific
#'   candidates built this way.
#' @export
#' @examples
#' parse_format("{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")
#'
#' # Multiple candidates are tried in turn, keeping whichever parses most values
#' parse_format(
#'   "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
#'   "{lin(year)}/{cyc(month, year)}/{cyc(day, month)}"
#' )
#'
#' # regex = TRUE treats the surrounding text as regular expression syntax
#' parse_format(
#'   "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}",
#'   regex = TRUE
#' )
parse_format <- function(..., regex = FALSE) {
  structure(as.character(c(...)), regex = regex)
}

# Helper function for inheriting the 'date' parsing string and extending it
# with time parsing strings for cal_time_* time systems
parse_format_time_of_day <- function(cal, suffix, suffix_regex = suffix) {
  date_fmts <- chronon_parse_linear(cal$day(1L), cal)
  regex <- isTRUE(attr(date_fmts, "regex"))
  suffixes <- if (regex) suffix_regex else suffix
  parse_format(as.vector(outer(date_fmts, suffixes, paste0)), regex = regex)
}
