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

time_format_default <- function(x) {
  chronon <- time_chronon(x)
  cycle <- time_cycle(x)

  fmt <- if (is.null(cycle)) {
    chronon_format_linear(chronon)
  } else {
    chronon_format_cyclical(chronon, cycle)
  }

  # Add format attributes (e.g. tz or location)
  fmt <- paste0(fmt, chronon_format_attr(chronon))

  # Add fractional
  if (is.double(x)) fmt <- paste(fmt, "{frac(.time)}")
  fmt
}

time_format_impl <- function(x, format = time_format_default(x), ...) {
  chronon <- time_chronon(x)
  cal <- time_calendar(chronon)
  
  # Create glue evaluation environment
  as_tu <- function(x) {
    if (S7::S7_inherits(x, mt_unit)) x else x(1L)
  }
  env <- rlang::new_environment(
    data = c(
      # The calendar units from chronon
      cal,

      # The label helper functions, returns time units and label options
      lin = function(x, ...) structure(list(as_tu(x)), ...),
      cyc = function(x, y, ...) {
        structure(list(as_tu(x), as_tu(y)), ...)
      },

      # Attribute helper functions
      tz = tz_abbreviation,
      loc = function(x) {
        chronon <- time_chronon(x)
        if (!S7::S7_inherits(mt_loc_unit)) return("")
        lat <- chronon@lat
        lon <- chronon@lon
        alt <- chronon@alt
        lat_str <- sprintf("%.4f%s", abs(lat), if (lat >= 0) "N" else "S")
        lon_str <- sprintf("%.4f%s", abs(lon), if (lon >= 0) "E" else "W")
        if (alt != 0) {
          paste0(lat_str, " ", lon_str, " ", sprintf("%.0fm", alt))
        } else {
          paste0(lat_str, " ", lon_str)
        }
      },
      frac = function(x) {
        x <- as.numeric(x)
        sprintf("%.1f%%", (x - floor(x))*100)
      },
        
      # Attach .time for specialised usage (e.g. tz_abbreviation(.time))
      list(.time = x)
    ),
    parent = rlang::caller_env()
  )

  out <- mt_glue_fmt(format, env = env)
  out_parts <- !vapply(out, is.character, logical(1L))

  # # Early exit if parts are not needed
  # if (!length(out$res)) return(rep_len(out$chr, length(x)))

  # TODO: Resolve automatic formatting usage (e.g. {year})
  # If it is the coarsest time unit, use linear_labels
  # Otherwise, use cyclical_labels with the next finest time unit.

  # Compute the numeric parts for display
  res_split <- split(out[out_parts], lengths(out[out_parts]))
  parts <- chronon_parts(
    x        = x,
    linear   = unlist(res_split[["1"]], recursive = FALSE),
    cyclical = res_split[["2"]]
  )

  # Apply labels
  parts$linear <- .mapply(
    \(tu, x) rlang::exec(linear_labels, tu[[1L]], x, !!!attributes(tu)), 
    dots = list(res_split[["1"]], parts$linear), 
    MoreArgs = NULL
  )
  parts$cyclical <- .mapply(
    # TODO floor(x) shouldn't be necessary, fix chronon_parts()?
    \(tu, x) rlang::exec(cyclical_labels, tu[[1L]], tu[[2L]], floor(x), !!!attributes(tu)), 
    dots = list(res_split[["2"]], parts$cyclical), 
    MoreArgs = NULL
  )

  # Insert time labels into format string
  out[out_parts] <- unsplit(Filter(length, parts), lengths(out[out_parts]))
  rlang::exec(paste0, !!!out)
}