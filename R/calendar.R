#' Create a new calendar
#'
#' \lifecycle{experimental}
#'
#' A calendar describes how time changes over some vector. It is responsible
#' for converting a vector of numbers into a point in time.
#'
#' @param granularity An origin-less identifier for the time increment of each value.
#' @param origin Logical identifier for if the origin is meaningful.
#'
#' @return A calendar data frame.
#'
#' @export
new_calendar <- function(granularity = list_of_time_units(), origin = logical()){
  vctrs::new_data_frame(
    list(granularity = granularity, origin = origin),
    class = "calendar"
  )
}

#' Extract the calendar data from an object
#'
#' \lifecycle{experimental}
#'
#' @param x An object containing a calendar
#'
#' @examples
#' calendar_data(yearmonth(0:11))
#'
#' @export
calendar_data <- function(x) {
  UseMethod("calendar_data")
}
