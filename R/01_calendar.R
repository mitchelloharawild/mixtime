#' Create a new calendar
#'
#' Define a new calendar as a collection of time units. Calendars are the
#' foundation for representing dates and times in terms of human-readable
#' components like years, months, days, hours, minutes, and seconds. Each
#' calendar is defined by specifying the time units it contains, which
#' determine how time values can be interpreted and manipulated.
#'
#' Time units are typically S7 class definitions that inherit from `mt_unit`
#' for standard units or `mt_tz_unit` for timezone-aware units. The calendar
#' object provides a namespace for accessing these unit constructors and
#' defines the relationships between them for calendar arithmetic.
#'
#' @param ... Named time unit class definitions. Each argument should be a
#'   time unit class (typically created with `S7::new_class()`) that inherits
#'   from `mt_unit` or `mt_tz_unit`. The names define the calendar's fields
#'   and are used to access unit constructors (e.g., `calendar$year()`).
#' @param class Character vector of additional classes for the calendar object.
#'
#' @return A calendar object of class `c(class, "mt_calendar")`, consisting of a
#'   named list containing the specified time unit classes.
#'
#' @seealso [linear_time()], [cyclical_time()]
#'
#' @examples
#' # Create a simple calendar with year and month units
#' cal_simple <- new_calendar(
#'   year = S7::new_class("tu_year", parent = mt_tz_unit),
#'   month = S7::new_class("tu_month", parent = mt_tz_unit),
#'   class = "cal_simple"
#' )
#'
#' # Access unit constructors from the calendar
#' year_unit <- cal_simple$year(1L)
#' month_unit <- cal_simple$month(1L)
#' 
#' @importFrom rlang list2
#' @export
new_calendar <- function(..., class = character()) {
  vctrs::vec_assert(class, character())
  time_units <- list2(...)
  
  cal <- structure(
    time_units,
    class = c(class, "mt_calendar")
  )
  
  # Create a reference environment that wraps the calendar
  cal_env <- new.env(parent = emptyenv())
  cal_env$calendar <- cal
  
  # Add the environment reference to each time unit
  for (i in seq_along(cal)) {
    attr(cal[[i]], "cal") <- cal_env
  }
  
  cal
}

#' @export
print.mt_calendar <- function(x, ...) {
  # Get calendar class (excluding mt_calendar base class)
  cal_classes <- setdiff(class(x), "mt_calendar")
  
  # Header
  if (length(cal_classes) > 0) {
    cat("<", paste(cal_classes, collapse = ", "), ">\n", sep = "")
  } else {
    cat("<mt_calendar>\n")
  }
  
  # List time units
  if (length(x) > 0) {
    cat("Time units:\n")
    for (name in names(x)) {
      cat("  - ", name, "\n", sep = "")
    }
  } else {
    cat("No time units defined\n")
  }
  
  invisible(x)
}

#' Obtain the calendar of a time object
#' 
#' This S7 generic function extracts the calendar system from a time object.
#' The calendar defines the collection of time units (years, months, days, etc.)
#' used to interpret the time representation.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A calendar object (e.g., `cal_gregorian`, `cal_isoweek`)
#' 
#' @examples
#' 
#' # The calendar of a Date object is the Gregorian calendar
#' time_calendar(Sys.Date())
#' 
#' # The calendar of a POSIXct object is also Gregorian
#' time_calendar(Sys.time())
#' 
#' # The calendar of a yearweek object is the ISO week calendar
#' time_calendar(yearweek(Sys.Date()))
#' 
#' # A mixed time object returns a list of calendars
#' time_calendar(c(yearmonth(Sys.Date()), Sys.Date()))
#' 
#' @export
time_calendar <- S7::new_generic("calendar", c("x"))

method(time_calendar, S7::class_Date) <- function(x) cal_gregorian
method(time_calendar, S7::class_POSIXt) <- function(x) cal_gregorian
method(time_calendar, mt_unit) <- function(x) {
  attr(S7::S7_class(x), "cal")$calendar
}
method(time_calendar, S7::new_S3_class("mixtime")) <- function(x) {
  time_calendar(time_chronon(x))
}
method(time_calendar, S7::new_S3_class("mt_linear")) <- function(x) time_calendar(time_chronon(x))
method(time_calendar, S7::new_S3_class("mt_cyclical")) <- function(x) time_calendar(time_chronon(x))
method(time_calendar, S7::class_any) <- function(x) cal_gregorian


# Fallback calendar for evaluating time units
cal_fallback <- function(calendar, fallback_calendar) {
  attr(calendar, "fallback") <- fallback_calendar
  class(calendar) <- c("mt_calendar_fb", class(calendar))
  calendar
}
