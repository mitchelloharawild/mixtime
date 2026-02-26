mt_unit_s3 <- S7::new_S3_class(
  "mixtime::mt_unit", 
  constructor = function(.data = 1L) .data,
  validator = function(self) {
    if (!typeof(self) %in% c("integer", "double")) {
      sprintf("Underlying data must be <integer> or <double>,  not <%s>", typeof(self))
    }
  }
)

#' Base S7 class for creating new time units
#' 
#' This class is the primative class for time units, and should 
#' be extended from when creating new time units. A new class
#' is typically created with S7 using: 
#' `S7::new_class("tu_***", parent = mt_tz_unit)`
#' 
#' Time units are the building blocks of calendars in mixtime. Each unit
#' represents a specific temporal component (e.g., day, month, year) and
#' can be combined using [new_calendar()] to create a calendar system.
#' 
#' When creating custom calendars, define time unit classes that inherit
#' from either `mt_unit` (for standard units) or `mt_tz_unit` (for
#' timezone-aware units), then pass them as named arguments to
#' [new_calendar()]. The calendar will use these names to create
#' constructor functions accessible via `$` notation (e.g., `calendar$day(1L)`).
#' 
#' @section Calendar Algebra Methods:
#' 
#' Time units enable calendar arithmetic through two key generic methods
#' that should be implemented for custom time units:
#' 
#' * `chronon_cardinality(from, to, at)` - Returns the number of `to` units
#'   that fit within one `from` unit. This can be a fixed value (e.g.,
#'   7 days per week) or variable based on `at` (e.g., 28-31 days per month).
#'   
#' * `chronon_divmod(x, from, to)` - Converts time unit `x` from units of
#'   `from` to units of `to`, returning a list with `div` (the quotient)
#'   and `mod`. This enables conversions between units that have
#'   variable cardinality (e.g., the date 2020-03-23 to the month 2020-03).
#'   All conversions should be based on chronons since epoch (1970-01-01),
#'   in the UTC time zone.
#' 
#' These methods work together to enable mixtime to perform calendar-aware
#' arithmetic, understanding that months have variable lengths and handling
#' timezone-aware conversions.
#' 
#' @param .data The number of time units
#' @param tz The timezone name for the unit (valid units can be found with `[tzdb::tzdb_names()]`)
#' 
#' @return A time unit object of class `mt_unit`
#' 
#' @seealso [new_calendar()] for creating calendars from time units
#' 
#' @examples
#' # Create a timezone-aware unit class
#' 
#' # Use these units to create a calendar
#' my_calendar <- new_calendar(
#'   day = S7::new_class("tu_my_day", parent = mt_unit),
#'   month = S7::new_class("tu_my_month", parent = mt_tz_unit),
#'   class = "my_calendar"
#' )
#' 
#' # Access unit constructors from the calendar
#' my_calendar$day(1L)
#' my_calendar$month(3L, tz = "America/New_York")
#' 
#' @export
#' @rdname mt_unit
mt_unit <- S7::new_class("mt_unit", parent = mt_unit_s3)
