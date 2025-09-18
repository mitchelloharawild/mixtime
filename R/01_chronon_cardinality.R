# TODO: Reverse `x` and `y` in the generic definition to match chronon_divmod
# and usual reading of how many "days in month".

#' Cardinality between time units
#'
#' This S7 generic function defines the calendrical relationships between two 
#' chronons, and is one of the building block for defining calendars in mixtime.
#' It calculates how many `x` chronons fit into the `y` chronon. Some chronon 
#' sizes are context-dependent (such as the number of days in a month),
#' and so an optional time point defined in terms of `y` chronons can be
#' provided with `at`.
#'
#' @param x The primary time unit
#' @param y The time unit to convert `x` into
#' @param at Optional time point for context-dependent cardinality, defined in
#' terms of `y` (e.g., if `y` is `tu_month()`, then `at` could be a 
#' `yearmonth()`)
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
#' chronon_cardinality(tu_year(1L), tu_month(1L))
#' 
#' # There are 7 days in a week
#' chronon_cardinality(tu_week(1L), tu_day(1L))
#' 
#' # There are 3600 seconds in an hour
#' chronon_cardinality(tu_hour(1L), tu_second(1L))
#' 
#' # There are 18 "2 months" in 3 years
#' chronon_cardinality(tu_year(3L), tu_month(2L))
#' 
#' # There are 365 days in 2025 (a common year)
#' chronon_cardinality(tu_year(1L), tu_day(1L), at = year(as.Date("2025-01-01")))
#' 
#' # There are 366 days in 2024 (a leap year)
#' chronon_cardinality(tu_year(1L), tu_day(1L), at = year(as.Date("2024-01-01")))
#' 
#' # There are 29 days in February 2024 (a leap year)
#' chronon_cardinality(tu_month(1L), tu_day(1L), at = yearmonth(as.Date("2024-02-01")))
#'
#' @export
chronon_cardinality <- S7::new_generic("chronon_cardinality", c("x", "y"))

#' Default method for time unit ratio comparison
#' 
#' When no specific method is defined, attempts to find the inverse ratio
#' by swapping the arguments and taking the reciprocal.
#' 
#' @noRd
method(chronon_cardinality, list(mt_unit, mt_unit)) <- function(x, y, at = NULL) {
  # Check if x and y are the same class
  if (S7_class_id(x) == S7_class_id(y)) {
    return(vec_data(x)/vec_data(y))
  }

  # Try to find a method with arguments swapped
  # (This feels unsafe for finding exact matching of S7 dispatch.)
  
  if (!is.null(y_env <- chronon_cardinality@methods[[S7_class_id(y)]])) {
    if (S7_class_id(x) %in% names(y_env)) {
      # Matching inverse method found, use it with inversion.
      return(1/chronon_cardinality(y, x, at = at))
    }
  }

  # No specific method defined between these classes
  # Attempt graph traversal to find a sequence of methods
  path <- S7_graph_dispatch(method_signatures(chronon_cardinality), x, y)

  path[[1]] <- x
  path[[length(path)]] <- y
  # Initialise intermediate classes with 1L
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(x) x(1L))

  result <- path[[1]]
  for (i in seq(2, length.out = length(path)-1)) {
    ## QUESTION: Why does this not work with `generic` instead of `chronon_cardinality`? S7 bug?

    result <- chronon_cardinality(result, path[[i]])
    # Class the result with the next class in the path
    result <- attr(path[[i]], "S7_class")(result)
  }

  vec_data(result)
}