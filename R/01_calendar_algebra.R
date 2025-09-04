#' Calendrical algebra for converting between time units
#'
#' This S7 generic function defines the calendrical relationships between two 
#' time units, and is the building block for defining calendars in mixtime. It
#' calculates how many `x` time units fit into one `y` unit. Some cyclical
#' granularities are context-dependent (such as the number of days in a month),
#' and so an optional time point can be provided with `at`.
#'
#' @param x The primary time unit
#' @param y The time unit to convert `x` into
#' @param at Optional time point for context-dependent ratios
#'
#' @return Numeric describing how many `x` time units fit into `y` at time `at`.
#' 
#' @details
#' 
#' The methods are dispatched based on the shortest path along defined methods.
#' This allows for defining only the direct relationships between adjacent
#' time units, and relying on graph traversal to find how to convert between
#' more distant units. For example the number of seconds in an hour can be
#' calculated from the number of seconds in a minute and then number of minutes
#' in an hour.
#' 
#' If a method is defined for converting between time units of different 
#' calendar systems (e.g., Gregorian calendar days to Chinese calendar days),
#' then that method can be used to convert times at any granularity between the
#' two systems.
#' 
#' @examples
#' # There are 12 months in a year
#' calendar_algebra(tu_year(1L), tu_month(1L))
#' 
#' # There are 7 days in a week
#' calendar_algebra(tu_week(1L), tu_day(1L))
#' 
#' # There are 3600 seconds in an hour
#' calendar_algebra(tu_hour(1L), tu_second(1L))
#' 
#' # There are 18 "2 months" in 3 years
#' calendar_algebra(tu_year(3L), tu_month(2L))
#'
#' @export
calendar_algebra <- S7::new_generic("calendar_algebra", c("x", "y"))

#' Default method for time unit ratio comparison
#' 
#' When no specific method is defined, attempts to find the inverse ratio
#' by swapping the arguments and taking the reciprocal.
#' 
#' @noRd
method(calendar_algebra, list(mt_unit, mt_unit)) <- function(x, y, at = NULL) {
  # Check if x and y are the same class
  if (S7_class_id(x) == S7_class_id(y)) {
    return(as.integer(x)/as.integer(y))
  }

  # Try to find a method with arguments swapped
  # (This feels unsafe for finding exact matching of S7 dispatch.)
  
  if (!is.null(y_env <- calendar_algebra@methods[[S7_class_id(y)]])) {
    if (S7_class_id(x) %in% names(y_env)) {
      # Matching inverse method found, use it with inversion.
      return(1/calendar_algebra(y, x, at = at))
    }
  }
  
  # No specific method defined between these classes
  # Attempt graph traversal to find a sequence of methods
  path <- S7_graph_dispatch(calendar_algebra, x, y)

  path[[1]] <- x
  path[[length(path)]] <- y
  # Initialise intermediate classes with 1L
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(x) x(1L))

  result <- path[[1]]
  for (i in seq(2, length.out = length(path)-1)) {
    ## QUESTION: Why does this not work with `generic` instead of `calendar_algebra`? S7 bug?

    result <- calendar_algebra(result, path[[i]])
    # Class the result with the next class in the path
    result <- attr(path[[i]], "S7_class")(as.integer(result))
  }

  attributes(result) <- NULL
  result
}