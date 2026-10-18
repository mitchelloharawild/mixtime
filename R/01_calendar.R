#' Create a new calendar
#'
#' Define a new calendar as a collection of time units. Calendars are the
#' foundation for representing dates and times in terms of human-readable
#' components like years, months, days, hours, minutes, and seconds. Each
#' calendar is defined by specifying the time units it contains, which
#' determine how time values can be interpreted and manipulated.
#'
#' Time units are typically S7 class definitions that inherit from [`mt_unit`]
#' for standard units, [`mt_tz_unit`] for timezone-aware units (civil time), or 
#' [`mt_loc_unit`] for location-aware units (astronomical time). The calendar
#' object provides a namespace for accessing these unit constructors and
#' defines the relationships between them for calendar arithmetic.
#'
#' @param ... Named time unit class definitions. Each argument should be a
#'   time unit class (typically created with [new_time_unit()]) that inherits
#'   from `mt_unit` or `mt_tz_unit`. The names define the calendar's fields
#'   and are used to access unit constructors (e.g., `calendar$year()`).
#' @param name A string naming the calendar (e.g. `"cal_gregorian"`). Together
#'   with the package that defines it, this identifies the calendar of time
#'   units. Calendars defined in a package should be bound in that package's
#'   namespace under this name, so that they can be found when time objects
#'   are read back in from a file.
#' @param inherit Optional calendar to inherit time units from. Units
#'   defined in `...` will override inherited units with the same name.
#' @param class `r lifecycle::badge("deprecated")` Use `name` instead.
#'
#' @return A calendar object, the single instance of a new S7 class `name`
#'   which inherits from [`mt_calendar`]. A calendar is a named list containing
#'   the specified time unit classes.
#'
#' @seealso [linear_time()], [cyclical_time()]
#'
#' @examples
#' # Create a simple calendar with year and month units
#' # (inheriting from civil time units for day, hour, minute, second, ...)
#' cal_simple <- new_calendar(
#'   year = new_time_unit("tu_year", parent = mt_tz_unit),
#'   month = new_time_unit("tu_month", parent = mt_tz_unit),
#'   inherit = cal_time_civil,
#'   name = "cal_simple"
#' )
#'
#' # Create time granules from the calendar
#' cal_simple$year(1L)
#' cal_simple$month(1L)
#' 
#' @importFrom rlang list2
#' @export
new_calendar <- function(..., name, inherit = NULL, class = lifecycle::deprecated()) {
  if (lifecycle::is_present(class)) {
    lifecycle::deprecate_soft("0.5.0", "new_calendar(class)", "new_calendar(name)")
    if (missing(name)) {
      name <- class[[1L]]
    }
  }
  if (missing(name)) {
    cli::cli_abort("A calendar requires a {.arg name}.", call = NULL)
  }
  vctrs::vec_assert(name, character(), size = 1L)
  package <- topNamespaceName(parent.frame())

  time_units <- list2(...)
  if (!is.null(inherit)) {
    # Add units from inherit that aren't already in time_units
    new_names <- setdiff(names(inherit), names(time_units))
    time_units <- c(time_units, inherit[new_names])
  }

  # Units created from the calendar's constructors record it by identifier
  id <- paste(c(package, name), collapse = "::")
  for (i in seq_along(time_units)) {
    attr(time_units[[i]], "cal") <- id
  }

  cal <- new_calendar_class(name, package)(time_units)
  calendar_register(id, cal)
  cal
}

# S7 class constructors enclose the frame that created them, so the calendar
# class is created here to avoid retaining the time units of new_calendar().
new_calendar_class <- function(name, package) {
  S7::new_class(name, parent = mt_calendar, package = package)
}

method(print, mt_calendar) <- function(x, ...) {
  cat("<", S7::S7_class(x)@name, ">\n", sep = "")

  # List time units
  if (length(x) > 0) {
    cat("Time units:\n")
    for (name in names(x)) {
      cat("  - ", name, "\n", sep = "")
    }
  } else {
    cat("No time units defined\n")
  }

  invisible(x)
}

# The identifier of a calendar, `"<package>::<name>"` (or `"<name>"` for a
# calendar defined outside of a package). Identifiers and NULL are returned
# as is.
calendar_id <- function(x) {
  if (is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x))) {
    return(x)
  }
  if (!is_calendar(x)) {
    cli::cli_abort(
      "{.field @calendar} must be a {.cls mt_calendar}, a calendar identifier, or {.code NULL}, not {.obj_type_friendly {x}}.",
      call = NULL
    )
  }
  S7_class_id(x)
}

# The time units of a calendar as a plain list (empty without a calendar)
calendar_units <- function(x) {
  if (is.null(x)) list() else S7::S7_data(x)
}

# S7_inherits() is comparatively slow for the frequent checks made here
is_calendar <- function(x) inherits(x, "mixtime::mt_calendar")

# Resolved calendars, keyed by identifier. Each entry holds the `calendar`
# and the `package` namespace `ns` it was found in, which must still be loaded
# for the entry to be used (otherwise the package has since been reloaded).
# Calendars created after mixtime is loaded (e.g. interactively, or when
# sourcing a package's code) are registered without a namespace. The registry
# is created when mixtime is loaded, so calendars created while a package is
# built aren't registered and are found in that package's namespace instead.
calendar_registry <- NULL

calendar_register <- function(id, cal, package = NULL) {
  if (is.null(calendar_registry)) {
    return(invisible())
  }
  ns <- if (!is.null(package)) .getNamespace(package)
  assign(id, list(calendar = cal, package = package, ns = ns), envir = calendar_registry)
}

# Resolve a calendar identifier to its calendar.
calendar_resolve <- function(id) {
  if (is.null(id)) {
    return(NULL)
  }
  entry <- calendar_registry[[id]]
  if (!is.null(entry)) {
    if (is.null(entry$package) || identical(entry$ns, .getNamespace(entry$package))) {
      return(entry$calendar)
    }
  }
  cal <- calendar_find(id)
  if (is.null(cal)) {
    cli::cli_abort(
      c(
        "Can't find the calendar {.val {id}}.",
        "i" = "Calendars defined outside of a package must be re-created with {.fn new_calendar} after restarting R."
      ),
      call = NULL
    )
  }
  cal
}

# Find a calendar in the namespace of the package that defines it.
calendar_find <- function(id) {
  parts <- strsplit(id, "::", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) {
    return(NULL)
  }
  ns <- tryCatch(asNamespace(parts[[1L]]), error = function(e) NULL)
  if (is.null(ns)) {
    return(NULL)
  }

  is_id <- function(x) is_calendar(x) && identical(S7_class_id(x), id)

  # Calendars are usually bound under their own name, otherwise (e.g. an
  # alias) search the namespace.
  cal <- get0(parts[[2L]], envir = ns, inherits = FALSE)
  if (!is_id(cal)) {
    cal <- NULL
    for (nm in names(ns)) {
      obj <- get0(nm, envir = ns, inherits = FALSE)
      if (is_id(obj)) {
        cal <- obj
        break
      }
    }
  }
  if (!is.null(cal)) {
    calendar_register(id, cal, parts[[1L]])
  }
  cal
}

#' Obtain the calendar of a time object
#' 
#' This S7 generic function extracts the calendar system from a time object.
#' The calendar defines the collection of time units (years, months, days, etc.)
#' used to interpret the time representation.
#' 
#' @param x A time object (e.g., [base::Date], [base::POSIXct], [linear_time()], etc.)
#' @param ... Additional arguments for methods.
#' 
#' @return A calendar object (e.g., `cal_gregorian`, `cal_isoweek`)
#' 
#' @examples
#' 
#' # The calendar of a Date object is the Gregorian calendar
#' time_calendar(Sys.Date())
#' 
#' # The calendar of a POSIXct object is also Gregorian
#' time_calendar(Sys.time())
#' 
#' # The calendar of a yearweek object is the ISO week calendar
#' time_calendar(yearweek(Sys.Date()))
#' 
#' # A mixed time object returns a list of calendars
#' time_calendar(c(yearmonth(Sys.Date()), Sys.Date()))
#' 
#' @export
time_calendar <- S7::new_generic("calendar", c("x"))

method(time_calendar, S7::class_Date) <- function(x) cal_gregorian
method(time_calendar, S7::class_POSIXt) <- function(x) cal_gregorian
method(time_calendar, mt_unit) <- function(x) {
  x@calendar
}
method(time_calendar, class_mixtime) <- function(x) {
  time_calendar(chronon_glb(x))
}
method(time_calendar, mt_time) <- function(x) time_calendar(time_chronon(x))
method(time_calendar, S7::class_any) <- function(x) cal_gregorian
