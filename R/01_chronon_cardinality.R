#' Cardinality between time granules
#'
#' This S7 generic function defines the calendrical relationships between two 
#' chronons, and is one of the building block for defining calendars in mixtime.
#' It calculates how many `x` chronons fit into the `y` chronon. Some chronon 
#' sizes are context-dependent (such as the number of days in a month),
#' and so an optional time point defined in terms of `y` chronons can be
#' provided with `at`.
#' 
#' @param x The finer time granule (e.g. `cal_gregorian$month(1L)`)
#' @param y The coarser time granule (e.g. `cal_gregorian$year(1L)`)
#' @param ... Additional arguments for methods.
# #' @param at Optional time point for context-dependent cardinality, defined in
# #' terms of `y` (e.g., if `y` is `month()`, then `at` could be a 
# #' `yearmonth()`)
#'
#' @return Numeric describing how many `x` time granules fit into `y` at time `at`.
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
#' with(cal_gregorian, chronon_cardinality(month(1L), year(1L)))
#' 
#' # There are 7 days in a week
#' with(cal_isoweek, chronon_cardinality(day(1L), week(1L)))
#' 
#' # There are 3600 seconds in an hour
#' with(cal_gregorian, chronon_cardinality(second(1L), hour(1L)))
#' 
#' # There are 18 "2 months" in 3 years
#' with(cal_gregorian, chronon_cardinality(month(2L), year(3L)))
#' 
#' # There are 365 days in 2025 (a common year)
#' chronon_cardinality(
#'   cal_gregorian$day(1L), cal_gregorian$year(1L),
#'   at = year(as.Date("2025-01-01"))
#' )
#' 
#' # There are 366 days in 2024 (a leap year)
#' chronon_cardinality(
#'   cal_gregorian$day(1L), cal_gregorian$year(1L), 
#'   at = mixtime::year(as.Date("2024-01-01"))
#' )
#' 
#' # There are 29 days in February 2024 (a leap year)
#' chronon_cardinality(
#'   cal_gregorian$day(1L), cal_gregorian$month(1L), 
#'   at = yearmonth(as.Date("2024-02-01"))
#' )
#'
#' @export
chronon_cardinality <- S7::new_generic("chronon_cardinality", c("x", "y"))

#' Fixed cardinality between time granules
#'
#' A restricted variant of [chronon_cardinality()] for time granule pairs
#' whose relationship is a constant, context-independent number (e.g., 60
#' seconds in a minute, 24 hours in a day). Unlike `chronon_cardinality()`,
#' methods for this generic do not receive (and must not need) an `at` time
#' point, and should return the number of unit (`n = 1L`) `x` granules that
#' fit within one unit `y` granule.
#'
#' Defining a method for `chronon_cardinality_fixed()` automatically provides
#' a `chronon_cardinality()` method for the pair (scaled by the requested
#' granule sizes via the `list(mt_unit, mt_unit)` fallback method), and marks
#' the relationship as safe to use for [chronon_divmod()]'s graph traversal,
#' where variable (context-dependent) cardinalities cannot be used since no
#' `at` is available mid-traversal.
#'
#' @inheritParams chronon_cardinality
#'
#' @return A single number describing how many unit `x` time granules fit
#'   into a unit `y` time granule.
#'
#' @keywords internal
#' @export
chronon_cardinality_fixed <- S7::new_generic("chronon_cardinality_fixed", c("x", "y"))

# #' @rdname chronon_cardinality
method(chronon_cardinality, list(mt_unit, mt_unit)) <- function(x, y, at = NULL) {
  # Check if x and y are the same class
  if (S7_class_id(x) == S7_class_id(y)) {
    return(y@n/x@n)
  }

  # Use a registered fixed-cardinality method if available, scaling the
  # constant (unit granule) ratio by the requested granule sizes.
  if (!is.null(chronon_cardinality_fixed@methods[[S7_class_id(x)]][[S7_class_id(y)]])) {
    return(chronon_cardinality_fixed(x, y) * y@n / x@n)
  }
  if (!is.null(chronon_cardinality_fixed@methods[[S7_class_id(y)]][[S7_class_id(x)]])) {
    return(y@n / (chronon_cardinality_fixed(y, x) * x@n))
  }

  # Try to find a method with arguments swapped
  # (This feels unsafe for finding exact matching of S7 dispatch.)

  if (!is.null(y_env <- chronon_cardinality@methods[[S7_class_id(y)]])) {
    if (S7_class_id(x) %in% names(y_env)) {
      # Matching inverse method found, use it with inversion.
      # TODO: Convert at to x units?
      return(1/chronon_cardinality(y, x, at = at))
    }
  }

  # No specific method defined between these classes
  # Attempt graph traversal to find a sequence of methods
  path <- S7_graph_dispatch(chronon_cardinality_graph(), y, x)

  path[[1]] <- y
  path[[length(path)]] <- x
  # Initialise intermediate classes with 1L and adjacent properties
  path[c(-1, -length(path))] <- lapply(path[c(-1, -length(path))], function(tu){
    # Ideally inherit from `x`, but if incomplete inherit from `y`
    granule_inherit_props(granule_inherit_props(tu(1L), x), y)
  })
  
  result <- path[[1]]
  for (i in seq(2, length.out = length(path)-1)) {
    ## QUESTION: Why does this not work with `generic` instead of `chronon_cardinality`? S7 bug?

    result <- chronon_cardinality(path[[i]], result, at)
    if (!is.null(at)) at <- at * result
    # Class the result with the next class in the path
    result <- S7::S7_class(path[[i]])(result)
  }

  result@n
  # vec_data(result)
}

# #' @rdname chronon_cardinality_fixed
method(chronon_cardinality_fixed, list(mt_unit, mt_unit)) <- function(x, y) {
  # Check if x and y are the same class
  if (S7_class_id(x) == S7_class_id(y)) {
    return(1)
  }

  # No specific method defined between these classes.
  # Attempt graph traversal using only registered fixed-cardinality edges,
  # multiplying the unit-granule cardinalities along the path.
  path <- S7_graph_dispatch(chronon_cardinality_fixed_graph(), y, x)

  result <- 1
  for (i in seq(2, length.out = length(path) - 1)) {
    result <- result * chronon_cardinality_fixed(path[[i]](1L), path[[i - 1]](1L))
  }
  result
}