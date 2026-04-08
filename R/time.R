#' Constructor for mixtime time vectors
#' 
#' Creates a `mixtime` time vector at a specific time point, with a specified 
#' chronon and optional cycle. The `chronon` defines the smallest indivisible
#' time unit for the time vector, while the `cycle` allows for cyclical time 
#' representations (e.g. day-of-week, month-of-year). 
#' 
#' @param x A numeric vector of time points, integers for discrete time or 
#'   doubles for continuous time. 
#' @param chronon A time unit object representing the smallest indivisible time
#'   unit (chronon) for the time vector (e.g. `cal_gregorian$day(1L)`). 
#' @param cycle An optional time unit object representing the cycle for cyclical
#'   time (e.g. `cal_gregorian$week(1L)` for day-of-week). If not provided, the 
#'   time vector will be treated as linear time.
#' 
#' @return A `mt_time` vector representing the time points in `x` according to 
#'   the specified `chronon` and `cycle`.
#' 
#' @examples
#' # Create a continuous mixtime time vector for today
#' new_time(
#'   as.double(Sys.Date()),
#'   chronon = cal_gregorian$day(1L, tz = Sys.timezone())
#' )
#'
#' # Create a discrete mixtime time vector for the current date and time
#' new_time(
#'   as.integer(Sys.time()),
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone())
#' )
#' 
#' # Create a discrete mixtime time vector for the time of day (cyclical time)
#' new_time(
#'   as.integer(Sys.time()), 
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone()), 
#'   cycle = cal_gregorian$day(1L, tz = Sys.timezone())
#' )
#' 
#' @export
new_time <- function(x = integer(), chronon = NULL, cycle = NULL) {
   vctrs::new_vctr(
    x,
    class = c(
      if (!is.null(cycle)) "mt_cyclical" else "mt_linear",
      "mt_time"
    ),
    chronon = chronon,
    cycle = cycle
  )
}

#' @export
format.mt_time <- function(x, format = time_format_default(x), ...) {
  time_format_impl(x, format = format, ...)
}