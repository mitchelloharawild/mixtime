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
new_calendar <- function(granularity, origin = TRUE){
  vctrs::new_data_frame(
    list(granularity = granularity, origin = origin),
    class = "calendar"
  )
}
