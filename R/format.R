# called internally by mt_glue_fmt
mt_unit_display <- function(x, units, parts, ...) {
  if (is_mt <- inherits(x, "S7_class")) {
    # Match based on class only, e.g. {year}-{month}-{day}
    xi <- which(vapply(units, S7_inherits, logical(1L), x))
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
  } else if (is_mt <- S7_inherits(x, mt_unit)) {
    xi <- vec_match(
      data.frame(x = x@n, tu = S7_class_id(x)),
      data.frame(x = vapply(units, function(u) u@n, numeric(1L)), tu = vapply(units, S7_class_id, character(1L)))
    )
    # No matches found
    if (is.na(xi)) {
      cli::cli_abort(
        c(
          "The calendar time unit of {time_granule_label(x)} could not be found."
        ),
        call = NULL
      )
    }
  }
  if (is_mt) {
    # Return cyclical labels for cyclical parts
    if (xi > 1) return(cyclical_labels_format(units[[xi]], units[[xi-1L]], parts[[xi]], at = NULL, ...))
    # Return linear labels for coarsest part
    return(as.character(parts[[xi]]))
  } else {
    as.character(x)
  }
}

# Decodes one mt_glue_fmt() `{lin(...)}`/`{cyc(...)}` token with attributes
format_token_spec <- function(tok) {
  list(
    chronon = tok[[1L]],
    cycle = if (length(tok) == 2L) tok[[2L]] else NULL,
    attrs = attributes(tok) %||% list()
  )
}

time_format_default <- function(x, attr = TRUE) {
  chronon <- base::attr(x, "chronon")

  if (S7_inherits(x, mt_duration)) {
    return(chronon_format_duration(chronon))
  }

  cycle <- base::attr(x, "cycle")
  fmt <- if (is.null(cycle)) {
    chronon_format_linear(chronon)
  } else {
    chronon_format_cyclical(chronon, cycle)
  }

  # Add fractional
  if (is.double(x)) fmt <- paste(fmt, "{frac(.time)}")

  # Add format attributes (e.g. tz or location)
  if (attr) fmt <- paste0(fmt, chronon_format_attr(chronon))

  fmt
}

time_format_impl <- function(x, format = time_format_default(x, attr = attr), ..., attr = TRUE) {
  # Obtain core time information
  chronon <- attr(x, "chronon")

  # Evaluation calendar: the chronon's own, extended with the cycle's units so
  # cyclical granules (e.g. {cyc(month, year)}) can be named.
  cycle <- attr(x, "cycle")
  calendar <- if (is.null(cycle)) time_calendar(chronon) else time_calendar(cycle)

  x_special <- is.na(x) | is.infinite(vec_data(x))

  # Create the glue evaluation environment by layering format-only helpers on top
  # of the shared lin()/cyc() component mask: the attribute helpers (tz / loc /
  # frac) and .time are added to the same calendar + lin/cyc vocabulary that
  # time_components() evaluates against.
  env <- rlang::new_environment(
    data = c(
      component_mask(calendar, chronon),
      list(
        # Attribute helper functions
        tz = tz_abbreviation,
        loc = function(x) {
          chronon <- attr(x, "chronon")
          if (!S7_inherits(mt_loc_unit)) return("")
          lat <- chronon@lat
          lon <- chronon@lon
          alt <- chronon@alt
          lat_str <- sprintf("%.2f%s", abs(lat), if (lat >= 0) "N" else "S")
          lon_str <- sprintf("%.2f%s", abs(lon), if (lon >= 0) "E" else "W")
          if (alt != 0) {
            paste0(lat_str, " ", lon_str, " ", sprintf("%.0fm", alt))
          } else {
            paste0(lat_str, " ", lon_str)
          }
        },
        frac = function(x) {
          # Apply time zone offset to x, with truncation for discrete time models.
          chronon <- attr(x, "chronon")
          x <- vec_data(x)
          x_tz <- tz_offset_impl(x, chronon)
          if(is.integer(x)) x_tz <- trunc(x_tz)
          x <- x + x_tz
          sprintf("%.1f%%", (x - floor(x))*100)
        },

        # Attach .time for specialised usage (e.g. tz_abbreviation(.time))
        .time = x
      )
    ),
    parent = rlang::caller_env()
  )

  fmt <- mt_glue_fmt(format, env = env)
  fmt_parts <- vapply(fmt, is.list, logical(1L))

  # Resolve bare S7 class tokens (e.g. from {year}, {month}, {day}) into
  # lin/cyc lists using the chronon_cardinality graph to order fine->coarse.
  s7_class_lgl <- vapply(fmt, inherits, logical(1L), "S7_class")
  if (any(s7_class_lgl)) {
    bare_units <- vapply(fmt[s7_class_lgl], S7_class_id, character(1L))
    cal <- time_calendar(x)

    tu_i <- match(
      bare_units,
      vapply(cal, S7_class_id, character(1L))
    )

    cli::cli_abort(
      c(
        "Bare time granule tokens are not currently supported in format strings.",
        i = "Use {.fn lin} for linear (coarsest) units, e.g. {.code {paste0('{lin(', names(cal)[tu_i[1]], ')}')}}}",
        i = "Use {.fn cyc} for cyclical units, e.g. {.code {paste0('{cyc(', names(cal)[tu_i[1]], ', <coarser_unit>)}')}}"
      ),
      call = NULL
    )

    # class_idx <- which(s7_class_lgl)
    # class_num <- length(class_idx)

    # # Initialise as time units of size 1L
    # fmt[class_idx] <- lapply(fmt[class_idx], function(f) f(1L))

    # # Find order of granularity from fine -> coarse
    # ordered_idx <- class_idx[S7_order_granules(fmt[class_idx])]

    # # Replace string parts such that:
    # # * finer time units are cyclical with the next coarser
    # for (i in seq_len(class_num - 1L)) {
    #   fmt[[ordered_idx[i]]] <- list(fmt[[ordered_idx[i]]], fmt[[ordered_idx[i+1L]]])
    # }
    # # * coarsest time unit is linear
    # fmt[[ordered_idx[class_num]]] <- list(fmt[[ordered_idx[class_num]]])
  }

  # Compute the numeric parts for display
  res_split <- split(fmt[fmt_parts], lengths(fmt[fmt_parts]))
  parts <- chronon_parts(
    x        = x[!x_special],
    linear   = unlist(res_split[["1"]], recursive = FALSE),
    cyclical = res_split[["2"]]
  )

  # Apply labels
  parts$linear <- .mapply(
    function(tu, x) {
      spec <- format_token_spec(tu)
      rlang::exec(linear_labels_format, spec$chronon, x, !!!spec$attrs)
    },
    dots = list(res_split[["1"]], parts$linear),
    MoreArgs = NULL
  )
  parts$cyclical <- .mapply(
    # TODO floor(x) shouldn't be necessary, fix chronon_parts()?
    function(tu, x, at) {
      spec <- format_token_spec(tu)
      rlang::exec(cyclical_labels_format, spec$chronon, spec$cycle, floor(x), at = at, !!!spec$attrs)
    },
    dots = list(res_split[["2"]], parts$cyclical, parts$cyclical_at),
    MoreArgs = NULL
  )

  # Insert time labels into format string parts
  if (any(fmt_parts)) {
    fmt[fmt_parts] <- unsplit(Filter(length, parts), lengths(fmt[fmt_parts]))
  }

  # Handle format parts which include special values (e.g. timezones)
  if (any(x_special) && any(fmt_inc_special <- lengths(fmt) == length(x))) {
    fmt[fmt_inc_special] <- lapply(fmt[fmt_inc_special], function(x) x[!x_special])
  }

  out <- character(length(x))
  out[!x_special] <- trimws(rlang::exec(paste0, !!!fmt))
  out[x_special] <- format(vec_data(x)[x_special])
  out
}

#' @export
S7::method(format, mt_time) <- function(x, ..., attr = TRUE) {
  time_format_impl(x, ..., attr = attr)
}

#' @export
S7::method(print, mt_time) <- function(x, ...) {
  cat(paste0("<", class(x)[[1L]], "[", length(x), "]>"), "\n", sep = "")
  print(format(x), quote = FALSE)
  invisible(x)
}

#' @export
S7::method(str, mt_linear) <- function(object, ..., vec.len = getOption("str")$vec.len %||% 4L) {
  cat("linear<", format(time_chronon(object)[1L]), "> [1:", length(object), "] ", .mt_str_values(object, vec.len), "\n", sep = "")
}

#' @export
S7::method(str, mt_cyclical) <- function(object, ..., vec.len = getOption("str")$vec.len %||% 4L) {
  cat("cyclical<", format(time_chronon(object)[1L]), "/", format(time_cycle(object)[1L]), "> [1:", length(object), "] ", .mt_str_values(object, vec.len), "\n", sep = "")
}

#' @export
S7::method(str, mt_duration) <- function(object, ..., vec.len = getOption("str")$vec.len %||% 4L) {
  cat("duration<", format(time_chronon(object)[1L]), "> [1:", length(object), "] ", .mt_str_values(object, vec.len), "\n", sep = "")
}

.mt_str_values <- function(x, vec_len) {
  vals <- format(x)
  if (length(vals) > vec_len) {
    paste0(paste0(vals[seq_len(vec_len)], collapse = " "), " ...")
  } else {
    paste0(vals, collapse = " ")
  }
}