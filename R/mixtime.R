#' Create a new mixtime
#'
#' A mixtime is a vector which describes a point in time. It uses a calendar
#' definition to translate a vector of numbers into a point in time.
#'
#' @param x The list of time values to become a mixtime.
#'
#' @importFrom rlang is_empty
#' @export
new_mixtime <- function(x = list()) {
  validate_x <- vapply(x, mixtime_valid, logical(1L))

  if (!all(validate_x)) {
    stop("A mixtime must contain only valid time points")
  }
  vecvec::new_vecvec(x, class = "mixtime")
}

#' @export
mixtime <- function(...) {
  new_mixtime(rlang::list2(...))
}

#' Convert time class into a mixtime
#'
#' @param x A time value to convert to a mixtime
#' @param ... Additional arguments for methods
#'
#' @export
as_mixtime <- function(x, ...) {
  vec_cast(x, new_mixtime())
}

#' Check if the object is a mixtime
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `mixtime` class.
#'
#' @examples
#' is_mixtime(Sys.Date())
#' is_mixtime(yearmonth(1))
#'
#' @export
is_mixtime <- function(x) {
  inherits(x, "mixtime")
}

#' Check if times can be used within mixtime
#' 
#' @param x A vector of times.
#' 
#' @return A logical vector.
#' 
#' @export
mixtime_valid <- function(x) {
  UseMethod("mixtime_valid")
}

#' @export
mixtime_valid.default <- function(x) {
  isTRUE(tsibble::index_valid(x))
}

#' @export
mixtime_valid.mixtime <- function(x) TRUE

#' @export
vec_ptype_full.mixtime <- function(x, ...) "mixtime"

#' @export
vec_ptype_abbr.mixtime <- function(x, ...) "mixtime"

vec_cast_to_mixtime <- function(x, to, ...) mixtime(x)

vec_cast_from_mixtime <- function(x, to, ...) {
  class(x) <- setdiff(class(x), "mixtime")
  vec_cast(x, to, ...)
}

## Custom vec cast methods since some time classes don't have cast methods

#' @export
#' @method vec_cast.character mixtime
vec_cast.character.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.character, ...)
  vecvec::unvecvec(x)
}

#' @export
#' @method vec_cast.double mixtime
vec_cast.double.mixtime <- function(x, to, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), as.double, ...)
  vecvec::unvecvec(x)
}

#' @export
vec_ptype2.mixtime <- function(x, y, ...) {
  x_is_time <- isTRUE(index_valid(x))
  y_is_time <- isTRUE(index_valid(y))

  if (!(x_is_time && y_is_time) && !(is.numeric(x) || is.numeric(y))) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  new_mixtime()
}

#' @export
#' @importFrom vctrs vec_proxy_order
vec_proxy_order.mixtime <- function(x, ...) {
  if (length(attr(x, "v")) > 1L) {
    # Convert all time values to a common chronon
    chronons <- lapply(attr(x, "v"), time_chronon)
    chronon_type <- chronon_common(!!!chronons)

    attr(x, "v") <- lapply(attr(x, "v"), function(v) {
      if (is.integer(v)) v <- v + 0.5
      mixtime::chronon_convert(v, chronon_type)
    })
  }
  vec_proxy_order(vctrs::vec_data(vecvec::unvecvec(x)))
}


#' Generate sequences of mixtime values
#' 
#' Create regular sequences of time values. This method handles both linear
#' time sequences (dates, date-times) and cyclical time sequences (day of week,
#' month of year).
#' 
#' @param from Starting value of the sequence.
#' @param to End value of the sequence (if provided).
#' @param by Increment of the sequence. Can be:
#'   * An integer for the number of time units
#'   * A character string specifying the interval (e.g., "1 day", "2 weeks", 
#'     "1 month", "1 year")
#'   * A time unit object created with time unit functions (e.g., `tu_year(1L)`,
#'     `tu_month(1L)`, `tu_day(1L)`)
#' @param length.out Desired length of the sequence (alternative to `to`).
#' @param along.with Take the length from this argument (alternative to `length.out`).
#' @param ... Additional arguments passed to the underlying sequence method.
#' 
#' @return A mixtime vector containing the sequence.
#' 
#' @details
#' For linear time types (Date, POSIXct, yearmonth, etc.), sequences progress
#' forward or backward in time. For cyclical time types (month_of_year, 
#' day_of_week, etc.), sequences wrap around cyclically.
#' 
#' @examples
#' # Linear time sequences with integer by
#' seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"))
#' seq(yearquarter("2020-01-01"), length.out = 5, by = 3)
#' 
#' # Linear time sequences with string intervals
#' seq(yearmonthday("2020-01-01"), yearmonthday("2020-12-31"), by = "1 month")
#' seq(yearmonth("2020 Jan"), yearmonth("2025 Jan"), by = "1 year")
#' seq(yearmonthday("2020-01-01"), length.out = 10, by = "2 weeks")
#' 
#' # Linear time sequences with time units
#' seq(yearmonth("2020 Jan"), yearmonth("2020 Dec"), by = tu_month(2L))
#' seq(yearmonthday("2020-01-01"), length.out = 5, by = tu_year(1L))
#' seq(yearmonthday("2020-01-01"), yearmonthday("2020-01-31"), by = tu_day(7L))
#' 
#' # Cyclical time sequences
#' seq(month_of_year(0L), month_of_year(11L))
#' seq(month_of_year(5L), month_of_year(3L), by = tu_month(2L))
#' seq(day_of_week(0L), day_of_week(6L), by = 1)
#' 
#' @rdname seq.mixtime
#' @export
seq.mixtime <- function(...) {
  # Strip mixtime vecvec wrapper
  arg <- lapply(
    rlang::list2(...), 
    function(x) if(is_mixtime(x)) vecvec::unvecvec(x) else x
  )
  
  # Call seq method with bare vectors
  mixtime(rlang::exec(seq, !!!arg))
}