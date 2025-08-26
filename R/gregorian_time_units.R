#' Base S7 class for time units
#'
#' @export
mt_unit <- S7::new_class("mt_unit", parent = S7::class_integer)
# TODO: the parent class should ideally be double.
# This requires a rework of the methods to not calculate the value as a ratio of non-1L durations...
# But this could cause problems with identifying the appropriate range of time specified.
# QUESTION: Should these methods EVER return a non-integer value?

#' Gregorian time unit classes
#'
#' @rdname gregorian_time_units
#' @export
tu_year <- S7::new_class("tu_year", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_quarter <- S7::new_class("tu_quarter", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_month <- S7::new_class("tu_month", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_week <- S7::new_class("tu_week", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_day <- S7::new_class("tu_day", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_hour <- S7::new_class("tu_hour", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_minute <- S7::new_class("tu_minute", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_second <- S7::new_class("tu_second", parent = mt_unit)

#' @rdname gregorian_time_units
#' @export
tu_millisecond <- S7::new_class("tu_millisecond", parent = mt_unit)

#' Find the ratio between two time units
#'
#' This S7 generic function calculates the ratio between two time units,
#' optionally at a specific time point.
#'
#' @param x First time unit
#' @param y Second time unit  
#' @param at Optional time point for context-dependent ratios
#'
#' @return Numeric ratio of x to y
#' 
#' @examples
#' # Ratio of years to months
#' time_unit_ratio(tu_year(1), tu_month(1))
#' 
#'
#' @export
time_unit_ratio <- S7::new_generic("time_unit_ratio", c("x", "y"))

#' Default method for time unit ratio comparison
#' 
#' When no specific method is defined, attempts to find the inverse ratio
#' by swapping the arguments and taking the reciprocal.
#'
#' @export
S7::method(time_unit_ratio, list(mt_unit, mt_unit)) <- function(x, y, at = NULL) {
  # Try to find a method with arguments swapped
  # (This feels unsafe for finding exact matching of S7 dispatch.)
  
  if (!is.null(y_env <- time_unit_ratio@methods[[S7_class_id(attr(y, "S7_class"))]])) {
    if (S7_class_id(attr(x, "S7_class")) %in% names(y_env)) {
      # Matching inverse method found, use it with inversion.
      return(1/time_unit_ratio(y, x, at = at))
    }
  }
  
  # No specific method defined between these classes
  # Attempt graph traversal to find a sequence of methods
  path <- S7_graph_dispatch(time_unit_ratio, x, y)

  browser()
  path[[1]] <- x
  path[[length(path)]] <- y
  # Initialise intermediate classes with 1L
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(x) x(1L))

  result <- path[[1]]
  for (i in seq(2, length.out = length(path)-1)) {
    ## QUESTION: Why does this not work with `generic` instead of `time_unit_ratio`? S7 bug?

    result <- time_unit_ratio(result, path[[i]])
    # Class the result with the next class in the path
    result <- attr(path[[i]], "S7_class")(as.integer(result))
  }

  browser()
  result
}


#' @export
S7::method(time_unit_ratio, list(tu_quarter, tu_year)) <- function(x, y, at = NULL) {
  as.integer(y)*4*as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_month, tu_year)) <- function(x, y, at = NULL) {
  as.integer(y)*12/as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_day, tu_week)) <- function(x, y, at = NULL) {
  as.integer(y)*7/as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_hour, tu_day)) <- function(x, y, at = NULL) {
  as.integer(y)*24/as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_minute, tu_hour)) <- function(x, y, at = NULL) {
  as.integer(y)*60/as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_second, tu_minute)) <- function(x, y, at = NULL) {
  # if(at %in% .leap.seconds) 61 else 60

  as.integer(y)*60/as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_millisecond, tu_second)) <- function(x, y, at = NULL) {
  as.integer(y)*1000/as.integer(x)
}

#' @export
S7::method(time_unit_ratio, list(tu_day, tu_month)) <- function(x, y, at = NULL) {
  # lubridate::days_in_month(at)
  stop("Not yet supported: Durations between days and months require a specific date context to calculate ratio")
}

### S7 methods graph dispatch
time_unit_ratio_pathway <- function(x, y) {
  time_unit_ratio@dispatch_args

  S7:::methods_rec(time_unit_ratio@methods, character())

  time_unit_ratio@methods$`mixtime::mt_unit`$`mixtime::mt_unit`
  
  names(time_unit_ratio@methods)
}
