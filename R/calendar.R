#' Create a new calendar
#'
#' \lifecycle{experimental}
#'
#' A calendar describes how time changes over some vector. It is responsible
#' for converting a vector of numbers into a point in time.
#'
#' @param origin The initial time point for the calendar
#' @param granularity An origin-less identifier for the time increment of each value.
#'
#' @return A calendar data frame.
#'
#' @export
new_calendar <- function(origin, granularity){
  vctrs::new_data_frame(
    list(origin = origin, granularity = granularity),
    class = "calendar"
  )
}
