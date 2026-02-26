# called internally by mt_glue_fmt
mt_unit_display <- function(x, units, parts, ...) {
  if (is_mt <- inherits(x, "S7_class")) {
    # Match based on class only, e.g. {year}-{month}-{day}
    xi <- which(vapply(units, S7::S7_inherits, logical(1L), x))
    if (length(xi) == 0L) {
      # This doesn't match one of the expected units, maybe it's not a mixtime S7 class?
      is_mt <- FALSE
    } else if (length(xi) > 1L) {
      # This matches multiple expected units, more precision is needed.
      cal <- time_calendar(units[[length(units)]])
      bad_tu <- names(cal)[match(S7_class_id(x), vapply(cal, S7_class_id, character(1L)))]
      cli::cli_abort(
        c(
          "Multiple units match {.code {paste0('{', bad_tu, '}')}} in the mixtime format string.",
          i =  "Specify chronon size precisely using e.g. {.code {paste0('{', bad_tu, '(1L)}')}}"
        ),
        call = NULL
      )
    }
  } else if (is_mt <- S7::S7_inherits(x, mt_unit)) {
    xi <- vec_match(
      data.frame(x = vec_data(x), tu = S7_class_id(x)),
      data.frame(x = vapply(units, vec_data, numeric(1L)), tu = vapply(units, S7_class_id, character(1L)))
    )
    # No matches found
    if (is.na(xi)) {
      cli::cli_abort(
        c(
          "The calendar time unit of {x} {time_unit_full(x)}{cli::qty(x)}{?s} could not be found.",
          i =  "All format time units need to be included as granules for {.code linear_time()}"
        ),
        call = NULL
      )
    }
  }
  if (is_mt) {
    # Return cyclical labels for cyclical parts
    if (xi > 1) return(cyclical_labels(units[[xi]], units[[xi-1L]], parts[[xi]], ...))
    # Return linear labels for coarsest part
    return(as.character(parts[[xi]]))
  } else {
    as.character(x)
  }
}

time_format_linear_impl <- function(x, format = chronon_format(time_chronon(x)), ...) {
  # Cascading formatting based on granules and chronon
  units <- c(
    attr(x, "granules"),
    list(time_chronon(x))
  )
  force(format)
  
  is_discrete <- is.integer(x)
  if (is_zoned <- tz_name(time_chronon(x)) != "UTC") {
    # Apply timezone offset to produce local time
    tz_ext <- tz_abbreviation(x)
    x <- vec_data(x) + trunc(tz_offset(x))
  } else {
    x <- vec_data(x)
  }
  
  # Initialise display granules
  parts <- rep(list(numeric(length(x))), n_units <- length(units))
  parts[[n_units]] <- floor(x)
  
  # Compute fractional component of chronon
  if(!is_discrete) {
    frac <- x - parts[[n_units]]
  }

  # Compute display granules
  for (i in seq(n_units, by = -1L, length.out = n_units - 1L)) {
    mod <- chronon_divmod(units[[i]], units[[i-1L]], parts[[i]])
    parts[[i - 1L]] <- mod$div
    parts[[i]] <- mod$mod
  }

  # Add epoch offset to the largest granule
  # TODO: Use calendar specific epochs
  parts[[1L]] <- parts[[1L]] - chronon_convert_impl(-1970L, cal_gregorian$year(1L), units[[1L]], discrete = TRUE, tz = "UTC")

  # Create glue evaluation environment
  env <- rlang::new_environment(
    data = c(
      # The calendar units from chronon
      time_calendar(units[[length(units)]]),
      # The lbl() helper function
      lbl = function(x, ...) mt_unit_display(x, units, parts, ...)
    ),
    parent = rlang::caller_env()
  )

  out <- mt_glue_fmt(format, env = env, units, parts)


  if(!is_discrete) {
    out <- paste(out, sprintf("%.1f%%", frac*100))
  }

  if (is_zoned) {
    out <- paste(out, tz_ext)
  }

  out
}