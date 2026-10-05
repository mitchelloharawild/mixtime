#' Create a new time unit class
#'
#' Define a new S7 class representing a time unit for use in a mixtime calendar.
#' Time units are the building blocks of calendars: each unit represents a
#' specific temporal component (e.g., day, month, year) and carries a step size
#' `n`. Units are combined via [new_calendar()] to form a complete calendar
#' system.
#'
#' Choose the parent class based on the type of time the unit represents:
#'
#' * [`mt_unit`] — abstract or calendar-only time (no timezone or location
#'   context; e.g., Gregorian year or ISO week).
#' * [`mt_tz_unit`] — civil time with a timezone (e.g., a clock hour that
#'   needs `tz` to resolve wall-clock ambiguity).
#' * [`mt_loc_unit`] — astronomical time with a geographic location (e.g., a
#'   solar day tied to observer longitude/latitude).
#'
#' @param name A string naming the new S7 class (e.g., `"tu_my_year"`).
#'   By convention, time unit class names are prefixed with `tu_`.
#' @param parent The parent S7 class. Should be one of [`mt_unit`],
#'   [`mt_tz_unit`], or [`mt_loc_unit`], or a class that itself inherits from
#'   one of these. Defaults to [`mt_unit`].
#' @param package A string giving the package name that owns the class.
#'   Defaults to the name of the calling namespace, so typically does not
#'   need to be set explicitly.
#' @param properties A named list of additional [`S7::new_property()`]
#'   definitions beyond those inherited from `parent`. Each entry becomes
#'   a slot on instances of the new class. Defaults to an empty list.
#' @param abstract Logical. If `TRUE` the class cannot be instantiated
#'   directly; it serves only as a base for further subclassing.
#' @param constructor A function to use as the class constructor, or `NULL`
#'   (default) to let S7 generate one from the parent constructor and properties.
#'   Custom constructors must accept
#'   `calendar = attr(sys.function(), "cal")` and forward it to the
#'   parent constructor as `calendar = calendar` to retain calendar membership.
#' @param validator A function of one argument (`self`) that returns `NULL`
#'   when the object is valid, or a character string describing the problem.
#'   See [S7::new_class()] for details.
#'
#' @return An S7 class object, as returned by [S7::new_class()].
#'
#' @details
#' Use `new_time_unit()` for units included in [new_calendar()]. Its generated
#' constructor records the calendar on each instance, including when the parent
#' has a custom constructor. A bare `S7::new_class()` constructor may only
#' forward `...` to its parent and cannot infer the calling calendar.
#'
#' @seealso
#' * [`mt_unit`], [`mt_tz_unit`], [`mt_loc_unit`] for the base classes to
#'   inherit from.
#' * [new_calendar()] for assembling time units into a calendar.
#' * [unit_scheme()] for naming a time unit (e.g. "day", "days" and "D").
#' * [S7::new_class()] for the underlying S7 class constructor.
#'
#' @examples
#' # An abstract unit with no properties
#' tu_my_seq <- new_time_unit("tu_my_seq", parent = mt_unit)
#'
#' # A civil-time unit with an extra property
#' tu_my_quarter <- new_time_unit(
#'   "tu_my_quarter",
#'   parent = mt_tz_unit,
#'   properties = list(
#'     fiscal = S7::new_property(S7::class_logical, default = FALSE)
#'   )
#' )
#'
#' @export
new_time_unit <- function(
  name,
  parent = mt_unit,
  package = topNamespaceName(parent.frame()),
  properties = list(),
  abstract = FALSE,
  constructor = NULL,
  validator = NULL
) {
  # The calendar is stored by identifier, so only a new default is retained
  # from a supplied `calendar` property.
  calendar <- parent@properties$calendar
  if (!is.null(properties$calendar)) {
    calendar$default <- properties$calendar$default
  }
  properties$calendar <- calendar

  S7::new_class(
    name = name,
    parent = parent,
    package = package,
    properties = properties,
    abstract = abstract,
    constructor = constructor,
    validator = validator
  )
}

#' Describe a time unit's naming scheme
#'
#' Builds the function a [unit_labels()] method is: assign its result
#' directly, e.g. `method(unit_labels, granule) <- unit_scheme(...)`. A unit
#' scheme names a granule itself, e.g. the "month" in "3 months", and is the
#' single source of truth for every output that renders a count of a unit:
#' duration formatting and parsing (`dur()` tokens), granule labels
#' (`format(month(3L))`), tsibble interval displays and messages.
#'
#' ```r
#' method(unit_labels, cal_gregorian$month) <- unit_scheme(
#'   vocab = vocab_table(`en-GB` = list(wide = "month{?/s}")),
#'   symbol = "M"
#' )
#' ```
#'
#' This differs from a [label_scheme()], which labels the *values* of a
#' granule (e.g. "March" or "2024") rather than the granule itself, but the
#' two share the same `vocab` and `locale` fields.
#'
#' @param vocab A function `function(type = NULL, locale = NULL)` returning
#'   either the full named list of renderings for one locale (`type = NULL`),
#'   or one rendering (e.g. `vocab("wide")`), as for a [label_scheme()]. Each
#'   rendering is a cli-style plural template, e.g. `"day{?/s}"` or
#'   `"{?zero/one/other}"`. The `"wide"` type gives the unit's full name.
#'   [vocab_table()] builds this from a plain lookup table.
#' @param symbol A short identifier for the unit, e.g. `"D"`. It is used as
#'   the prefix of default format strings, in abbreviated label fallbacks
#'   (e.g. `"5Y"`) and when parsing durations. Symbols are not localised, and
#'   should be distinct across units (e.g. `"M"` for month and `"m"` for
#'   minute).
#' @param locale A caller-overridable default locale, selecting which
#'   `vocab()` entry to use by default.
#'
#' @return A function `function(granule) list(vocab =, symbol =, locale =)`,
#'   assignable directly as a [unit_labels()] method. `granule` is only used
#'   for dispatch: a scheme describes a granule class, not the instance a
#'   method happens to be called with.
#'
#' @seealso [unit_labels()], the generic a scheme is returned from;
#'   [label_scheme()] for labelling the values of a granule; [vocab_table()]
#'   for the common hand-listed name table case.
#'
#' @examples
#' scheme <- unit_labels(cal_gregorian$year(1L))
#' scheme$vocab("wide")
#' scheme$symbol
#'
#' @export
unit_scheme <- function(vocab, symbol = "", locale = NULL) {
  fields <- list(
    vocab = vocab,
    symbol = symbol,
    locale = locale
  )
  # granule is unused, only here so this can be assigned directly as a
  # unit_labels() method.
  function(granule) fields
}

#' @param granule A time granule instance (e.g. `cal_gregorian$month(1L)`).
#' @param ... Unused; only present for S7 dispatch.
#' @rdname unit_scheme
#' @export
unit_labels <- S7::new_generic("unit_labels", "granule")
method(unit_labels, mt_unit) <- unit_scheme(
  vocab = vocab_table(`en-GB` = list(wide = "unit{?/s}"))
)

# The full name of a unit, pluralised for quantity `n` (e.g. "year", "years").
# Vectorised over `n`.
unit_name <- function(x, n = 2L, type = "wide", locale = NULL) {
  scheme <- unit_labels(x)
  str_plural(scheme$vocab(type, locale %||% scheme$locale), as.numeric(n))
}

# The symbol of a unit (e.g. "Y").
unit_symbol <- function(x) {
  unit_labels(x)$symbol
}

# A quantity of a time unit as text, e.g. "15 months". Defaults to describing
# the granule itself (`month(15L)`), since `mt_unit` objects are S7 scalars
# which neither paste() nor cli can interpolate directly. Vectorised over `n`.
time_granule_label <- function(x, n = x@n) {
  paste(n, unit_name(x, n))
}
