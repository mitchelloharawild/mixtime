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
  structure(
    list2(...),
    class = c(class, "mt_calendar")
  )
}
