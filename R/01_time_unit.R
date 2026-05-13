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
#'   (default) to generate one automatically. The auto-generated constructor
#'   accepts `...` (forwarded to the parent constructor) plus one argument per
#'   entry in `properties`, with defaults taken from each property's
#'   `default` field.
#' @param validator A function of one argument (`self`) that returns `NULL`
#'   when the object is valid, or a character string describing the problem.
#'   See [S7::new_class()] for details.
#'
#' @return An S7 class object, as returned by [S7::new_class()].
#'
#' @seealso
#' * [`mt_unit`], [`mt_tz_unit`], [`mt_loc_unit`] for the base classes to
#'   inherit from.
#' * [new_calendar()] for assembling time units into a calendar.
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
  if (is.null(constructor)) {
    props <- syms(names(properties))
    names(props) <- names(properties)
    
    constructor <- rlang::new_function(
      args = rlang::pairlist2(... =, !!!lapply(properties, `[[`, "default")),
      body = expr(S7::new_object(parent(!!sym("...")), !!!props))
    )
  }

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

#' Time units as a string
#' 
#' These S7 generic functions provide the full and abbreviated names for time
#' units. `time_unit_full()` is used in messages and durations (e.g., "2 months").
#' `time_unit_abbr()` is used for tsibble index interval displays (e.g., "1M").
#' 
#' @param x A time granule object (e.g., `cal_gregorian$month(1L)`)
#' @param ... Additional arguments for methods.
#' 
#' @return A string representing the time unit
#' 
#' @examples
#' time_unit_full(cal_gregorian$year(1L))
#' time_unit_abbr(cal_gregorian$year(1L))
#' 
#' @rdname time_unit_labels
#' @export
time_unit_full <- S7::new_generic("time_unit_full", "x")
method(time_unit_full, mt_unit) <- function(x) "unit"

#' @rdname time_unit_labels
#' @export
time_unit_abbr <- S7::new_generic("time_unit_abbr", "x")
method(time_unit_abbr, mt_unit) <- function(x) ""