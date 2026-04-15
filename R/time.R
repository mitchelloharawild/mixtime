#' Constructor for mixtime time vectors
#' 
#' @description
#' `r lifecycle::badge("stable")`
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
#' @param class An optional character vector of additional S3 classes to assign 
#'   to the resulting time vector. This allows for further subclassing of 
#'   `mt_time` for specific time types (e.g. linear, cyclical, durations, etc.).
#' 
#' @return A `mt_time` vector representing the time points in `x` according to 
#'   the specified `chronon` and `cycle`.
#' 
#' @examples
#' # Create a continuous mixtime time vector for today
#' new_time(
#'   as.double(Sys.Date()),
#'   chronon = cal_gregorian$day(1L, tz = Sys.timezone()),
#'   class = "mt_linear"
#' )
#'
#' # Create a discrete mixtime time vector for the current date and time
#' new_time(
#'   as.integer(Sys.time()),
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone()),
#'   class = "mt_linear"
#' )
#' 
#' # Create a discrete mixtime time vector for the time of day (cyclical time)
#' new_time(
#'   as.integer(Sys.time()), 
#'   chronon = cal_gregorian$second(1L, tz = Sys.timezone()), 
#'   cycle = cal_gregorian$day(1L, tz = Sys.timezone()),
#'   class = "mt_cyclical"
#' )
#' 
#' @export
new_time <- function(x = integer(), chronon = mt_unit(1L), cycle = NULL, class = NULL) {
  if (length(chronon@n) != 1L) {
    cli::cli_abort("{.var chronon} must be a single time unit object.", call. = FALSE)
  }
  if (!is.null(cycle) && length(cycle@n) != 1L) {
    cli::cli_abort("{.var cycle} must be a single time unit object.", call. = FALSE)
  }
  vctrs::new_vctr(
    x,
    class = c(class, "mt_time"),
    chronon = chronon,
    cycle = cycle
  )
}

#' @export
format.mt_time <- function(x, format = time_format_default(x), ...) {
  time_format_impl(x, format = format, ...)
}