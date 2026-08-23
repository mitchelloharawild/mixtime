#' Parse text into a time point
#'
#' `time_parse()` is the inverse of [format()]: given text and the same
#' `{lin(...)}`/`{cyc(...)}` template `format` uses, it reconstructs the time
#' points that would have produced that text, via [time_compose()].
#'
#' A `format` with a `{lin(...)}` token parses to linear time; one of only
#' `{cyc(...)}` tokens parses to cyclical time, e.g. `time_parse("Feb",
#' format = "{cyc(month, year, label = TRUE)}")` recovers the same kind of
#' value as [month_of_year()].
#'
#' Granule-specific extraction and decoding labels for each token is done by
#' [linear_labels_parse()]/[cyclical_labels_parse()].
#'
#' @param x A character vector to parse.
#' @param chronon Target time granule for the result, and (with `cycle`) the
#'   source of `format` candidates when `format` is `NULL`. Its attributes
#'   (e.g. `tz`) fill in whatever `format` leaves unset, and the result is
#'   converted onto it if `format` reaches a different chronon.
#' @param cycle Target cycle granule, pairing with `chronon` for a cyclical
#'   result. Requires `chronon`.
#' @param format A glue-style format string of [lin()]/[cyc()] tokens, e.g.
#'   `"{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"` (see
#'   `vignette("time-format-strings")`), or several to try: whichever parses
#'   the most values of `x` is used for the whole vector (ties keep the
#'   earliest-listed format), and its unparsed values become `NA` (with a
#'   warning). Aborts if no format matches the shape of even one value.
#'   `time_parse(format(x, fmt), format = fmt)` round-trips back to `x`.
#'   `NULL` (the default) derives candidates from `chronon`/`cycle` via
#'   [chronon_parse_linear()]/[chronon_parse_cyclical()]; requires `chronon`.
#' @param regex If `FALSE` (the default), literal text surrounding tokens is
#'   matched exactly. If `TRUE`, it's instead used verbatim as a regular
#'   expression, e.g. `"[/-]"` to accept either `/` or `-` as a separator;
#'   `(...)` groups you write are treated as non-capturing, since capturing
#'   groups are reserved for the tokens. Ignored when `format` is derived
#'   from `chronon`, which carries its own regex mode.
#' @param na Strings to treat as missing (`NA`), checked before matching
#'   `format`. Not counted in the parsing-failure warning, unlike a value
#'   that fails to match `format`.
#' @param calendar Calendar used to resolve granule names in `format`, and
#'   to disambiguate `chronon`'s [chronon_parse_linear()] candidates when
#'   `format` is `NULL`. `NULL` (the default) uses `time_calendar(cycle)` or
#'   `time_calendar(chronon)`, whichever is supplied, else [cal_gregorian].
#' @param locale Default locale for named (`label = TRUE`) tokens that don't
#'   specify their own. `NULL` defers to each token's own scheme.
#' @param discrete Whether the result is discrete (integer chronon counts)
#'   or continuous (fractional). See [linear_time()].
#'
#' @return A `mixtime` time vector, the same length as `x`. Linear if
#'   `format` includes a `{lin(...)}` token (or `cycle` is `NULL`),
#'   cyclical otherwise.
#'
#' @seealso [format()] for the inverse direction, [time_compose()] for
#'   composing a time point from already-decoded components,
#'   [label_scheme()] for declaring how a granule's labels parse,
#'   [chronon_parse_linear()]/[chronon_parse_cyclical()] for the candidate
#'   formats derived from `chronon`/`cycle`, and `vignette("time-format-strings")`
#'   for the format string syntax.
#'
#' @examples
#' time_parse("2024-02-15", format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")
#' time_parse(
#'   "15 Feb 2024",
#'   format = "{cyc(day, month)} {cyc(month, year, label = TRUE)} {lin(year)}"
#' )
#'
#' # One bad value becomes NA (with a warning) instead of aborting the batch
#' time_parse(
#'   c("2024-02-15", "not a date"),
#'   format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
#' )
#'
#' # No {lin(...)} token: parses to cyclical time
#' time_parse("Feb", format = "{cyc(month, year, label = TRUE)}")
#'
#' # Several formats: whichever parses the most values is used for the whole
#' # vector; here none of the "Y-M-D" format's values match, so the "D/M/Y"
#' # format (which matches both) is used instead
#' time_parse(
#'   c("15/02/2024", "20/03/2024"),
#'   format = c(
#'     "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}",
#'     "{cyc(day, month)}/{cyc(month, year)}/{lin(year)}"
#'   )
#' )
#'
#' # regex = TRUE: match "/" or "-" as the separator, 
#' # and tolerate a trailing comment after the date.
#' time_parse(
#'   c("2024-02-15", "2024/02/15 (approx)"),
#'   format = "{lin(year)}[/-]{cyc(month, year)}[/-]{cyc(day, month)}( .*)?",
#'   regex = TRUE
#' )
#'
#' # Default format strings from the target chronon, and results with `tz`.
#' time_parse("2024-02-15 09:00:00", chronon = cal_gregorian$second(1L, tz = "America/Los_Angeles"))
#'
#' @export
time_parse <- function(
  x,
  chronon = NULL,
  cycle = NULL,
  format = NULL,
  regex = FALSE,
  na = c("", "NA"),
  calendar = NULL,
  locale = NULL,
  discrete = TRUE
) {
  x <- as.character(x)
  n <- length(x)

  if (!is.null(cycle) && is.null(chronon)) {
    cli::cli_abort("{.arg cycle} requires {.arg chronon} to also be supplied.", call = NULL)
  }

  if (is.null(calendar)) {
    calendar <- if (!is.null(cycle)) {
      time_calendar(cycle)
    } else if (!is.null(chronon)) {
      time_calendar(chronon)
    } else {
      cal_gregorian
    }
  }

  if (is.null(format)) {
    if (is.null(chronon)) {
      cli::cli_abort("Either {.arg chronon} or {.arg format} must be supplied.", call = NULL)
    }
    format <- if (is.null(cycle)) {
      chronon_parse_linear(chronon, calendar)
    } else {
      chronon_parse_cyclical(chronon, cycle)
    }
    regex <- attr(format, "regex") %||% FALSE
  }
  format <- as.character(format)
  if (length(format) == 0L) {
    cli::cli_abort("{.arg format} must have length 1 or more.", call = NULL)
  }

  env <- rlang::caller_env()
  # Validate each format's shape and resolve its chain order once, up front.
  compiled_list <- lapply(format, time_parse_compile, calendar = calendar, locale = locale, env = env, regex = regex)

  is_missing <- is.na(x) | x %in% na
  attempt <- !is_missing
  x_try <- x[attempt]
  m <- length(x_try)

  if (n == 0L) {
    result <- time_parse_na(compiled_list[[1L]]$chain, compiled_list[[1L]]$cycle, discrete, 1L)
    if (!is.null(chronon)) result <- time_parse_recast(result, chronon, cycle, discrete)
    return(result[0L])
  }
  if (m == 0L) {
    # Every value is missing: no data to choose a format by.
    result <- time_parse_na(compiled_list[[1L]]$chain, compiled_list[[1L]]$cycle, discrete, 1L)
    if (!is.null(chronon)) result <- time_parse_recast(result, chronon, cycle, discrete)
    return(result[rep(1L, n)])
  }

  # Keep whichever format parses the most values; ties keep the earliest.
  # Try likely winners first, ranked by a cheap match count on a small
  # sample, so a clear winner can end the search early.
  sample <- x_try[seq_len(min(16L, m))]
  sample_counts <- vapply(
    compiled_list,
    function(compiled) sum(time_parse_matches(compiled, sample)$matched),
    integer(1L)
  )
  search_order <- order(-sample_counts)

  best <- NULL
  for (f in search_order) {
    compiled <- compiled_list[[f]]
    matches <- time_parse_matches(compiled, x_try)
    match_count <- sum(matches$matched)
    # Match count upper-bounds parse count, so this can't beat the best yet.
    if (match_count == 0L ||
        (!is.null(best) &&
         (match_count < best$ok_count ||
          (match_count == best$ok_count && f > best$format)))) {
      next
    }

    attempted <- time_parse_attempt(compiled, x_try, matches, discrete)
    ok_count <- sum(attempted$ok)
    if (is.null(best) || ok_count > best$ok_count || (ok_count == best$ok_count && f < best$format)) {
      best <- list(format = f, attempted = attempted, ok_count = ok_count)
    }
    # A near-total match ends the search rather than checking every format.
    if (best$ok_count >= m * 0.975) {
      break
    }
  }

  # No candidate format matched the shape of even a single value.
  if (is.null(best)) {
    cli::cli_abort(
      c(
        "None of the {length(compiled_list)} candidate format{?s} in {.arg format} matched any value in {.arg x}.",
        i = "{.val {utils::head(x_try, 5L)}}{if (m > 5L) paste0(', and ', m - 5L, ' more') else ''}",
        i = "Check the format string against the shape of {.arg x} (separators, token order, numeric vs. labelled tokens)."
      ),
      class = "mixtime_parse_no_match",
      call = NULL
    )
  }

  attempted <- best$attempted
  na_one <- time_parse_na(compiled_list[[best$format]]$chain, compiled_list[[best$format]]$cycle, discrete, 1L)

  failed <- !attempted$ok
  if (any(failed)) {
    time_parse_warn_failures(x_try[failed])
  }

  # Combine parsed values and repeated NA placeholders, then reorder.
  good <- attempted$value %||% na_one[0L]
  n_bad <- sum(is_missing) + sum(failed)
  bad <- na_one[rep(1L, n_bad)]
  combined <- vctrs::vec_c(good, bad)

  perm <- integer(n)
  idx_good <- which(attempt)[attempted$ok]
  idx_bad <- c(which(is_missing), which(attempt)[failed])
  perm[idx_good] <- seq_along(idx_good)
  perm[idx_bad] <- length(idx_good) + seq_along(idx_bad)

  result <- combined[perm]
  if (is.null(chronon)) result else time_parse_recast(result, chronon, cycle, discrete)
}

# Regex-match `x_try` against a compiled format, without decoding/composing.
# A cheap pre-check for whether a format can possibly parse every value.
time_parse_matches <- function(compiled, x_try) {
  groups <- regmatches(x_try, regexec(compiled$pattern, x_try, perl = TRUE))
  list(matched = lengths(groups) > 0L, groups = groups)
}

# Decode/compose `x_try` under one compiled format. Returns list(value, ok).
time_parse_attempt <- function(compiled, x_try, matches, discrete) {
  m <- length(x_try)
  row_matched <- matches$matched
  groups <- matches$groups
  n_specs <- length(compiled$specs)

  groups[!row_matched] <- list(rep(NA_character_, n_specs + 1L))
  mat <- matrix(unlist(groups, use.names = FALSE), nrow = n_specs + 1L)
  text_cols <- lapply(seq_len(n_specs), function(k) mat[k + 1L, ])

  res <- tryCatch(
    list(value = time_parse_compose(compiled$specs, text_cols, compiled$chain_order, compiled$cycle, discrete)),
    error = function(e) list(error = e)
  )

  if (is.null(res$error)) {
    ok <- row_matched & !is.na(res$value)
    value <- res$value[ok]
  } else {
    # One bad row aborts the vectorized attempt; retry row by row to find it.
    ok <- row_matched
    row_values <- vector("list", m)
    n_ok <- 0L
    for (i in which(row_matched)) {
      row_res <- tryCatch(
        list(value = time_parse_compose(compiled$specs, lapply(text_cols, `[`, i), compiled$chain_order, compiled$cycle, discrete)),
        error = function(e) list(error = e)
      )
      if (is.null(row_res$error) && !is.na(row_res$value)) {
        n_ok <- n_ok + 1L
        row_values[[n_ok]] <- row_res$value
      } else {
        ok[[i]] <- FALSE
      }
    }
    value <- if (n_ok > 0L) vctrs::vec_c(!!!row_values[seq_len(n_ok)]) else NULL
  }

  list(value = value, ok = ok)
}

# Tokenize `format` into an anchored extraction regex plus its lin()/cyc() specs.
time_parse_compile <- function(format, calendar, locale, env, regex = FALSE) {
  mask_env <- rlang::new_environment(data = component_mask(calendar), parent = env)
  fmt <- mt_glue_fmt(format, env = mask_env)
  is_spec <- vapply(fmt, is.list, logical(1L))

  specs <- lapply(fmt[is_spec], function(tok) {
    spec <- format_token_spec(tok)
    attrs <- spec$attrs
    if (is.null(attrs$locale)) attrs$locale <- locale

    parse_spec <- if (is.null(spec$cycle)) {
      rlang::exec(linear_labels_parse, spec$chronon, !!!attrs)
    } else {
      rlang::exec(cyclical_labels_parse, spec$chronon, spec$cycle, !!!attrs)
    }
    list(chronon = spec$chronon, cycle = spec$cycle, pattern = parse_spec$pattern, decode = parse_spec$decode)
  })

  fragments <- character(length(fmt))
  spec_i <- 0L
  for (i in seq_along(fmt)) {
    if (is_spec[[i]]) {
      spec_i <- spec_i + 1L
      fragments[[i]] <- paste0("(", specs[[spec_i]]$pattern, ")")
    } else if (regex) {
      # Verbatim regex: no escaping, no whitespace tolerance. Capturing
      # groups are rewritten to non-capturing so they can't shift the
      # positional group indices used below (see time_parse_no_capture()).
      fragments[[i]] <- time_parse_no_capture(fmt[[i]])
    } else {
      fragments[[i]] <- time_parse_escape_literal(fmt[[i]])
    }
  }

  # Validate the format's shape and resolve its chain order once, up front.
  # `chain$cycle` is non-NULL for a cyc()-only (no lin()) format, giving a
  # cyclical result; see compose_chain()/compose_recompose().
  chain <- compose_chain(lapply(specs, function(s) list(chronon = s$chronon, cycle = s$cycle)))
  chain_order <- vapply(
    chain$chain,
    function(link) {
      which(vapply(
        specs,
        function(s) identical(s$chronon, link$chronon) && identical(s$cycle, link$cycle),
        logical(1L)
      ))
    },
    integer(1L)
  )

  list(
    pattern = paste0("^", paste0(fragments, collapse = ""), "$"),
    specs = specs,
    chain = chain$chain,
    cycle = chain$cycle,
    chain_order = chain_order
  )
}

# Regex-escape literal format text, collapsing runs of spaces to `\s+` for
# tolerance to whitespace variation.
time_parse_escape_literal <- function(text) {
  esc <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", text, perl = TRUE)
  gsub(" +", "\\\\s+", esc)
}

# Rewrite a user's verbatim regex (regex = TRUE) so every capturing group it
# opens becomes non-capturing (?:...). The positional groups in the
# compiled pattern are reserved for {lin(...)}/{cyc(...)} tokens
# (time_parse_attempt() picks out each spec's text by group index), so a
# stray user capturing group would silently shift them out of alignment;
# rewriting means there's nothing for the caller to get right.
#
# `(` is left alone (already special/non-capturing) when it's escaped
# (`\(`), inside a `[...]` character class (where it's a literal char, not a
# group), or already followed by `?` (covers `(?:`, `(?=`, `(?!`, `(?<=`,
# `(?<!`, `(?<name>`).
time_parse_no_capture <- function(text) {
  if (!nzchar(text)) return(text)
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  n <- length(chars)
  in_class <- FALSE
  escaped <- FALSE
  for (i in seq_len(n)) {
    ch <- chars[[i]]
    if (escaped) {
      escaped <- FALSE
    } else if (ch == "\\") {
      escaped <- TRUE
    } else if (in_class) {
      if (ch == "]") in_class <- FALSE
    } else if (ch == "[") {
      in_class <- TRUE
    } else if (ch == "(" && !(i < n && chars[[i + 1L]] == "?")) {
      chars[[i]] <- "(?:"
    }
  }
  paste0(chars, collapse = "")
}

# Decode each spec's text column and hand the result to compose_recompose().
# A cyc() link's decode is deferred until its coarser instance is resolved,
# except at the chain's root (position 1), which decodes outright: `at`
# defaults to NULL, so this works whether the root is a lin() or (for
# cyclical time) a cyc() token.
time_parse_compose <- function(specs, text_cols, chain_order, cycle, discrete) {
  comps <- lapply(seq_along(chain_order), function(pos) {
    k <- chain_order[[pos]]
    spec <- specs[[k]]
    text <- text_cols[[k]]
    if (pos == 1L) {
      list(chronon = spec$chronon, cycle = NULL, value = spec$decode(text))
    } else {
      list(chronon = spec$chronon, cycle = spec$cycle, value = function(at) spec$decode(text, at))
    }
  })
  compose_recompose(comps, discrete = discrete, cycle = cycle, strict = FALSE)
}

# An all-`NA` result at the chain's finest chronon, built via compose_recompose().
time_parse_na <- function(shape_chain, cycle, discrete, k) {
  chain <- shape_chain
  chain[[1L]]$value <- rep(NA_real_, k)
  for (j in seq_along(chain)[-1L]) {
    chain[[j]]$value <- function(at) rep(NA_real_, length(at))
  }
  compose_recompose(chain, discrete = discrete, cycle = cycle)
}

# Cast a time_parse() result onto the caller-requested chronon/cycle, filling
# in whichever of its properties (tz, lat/lon, ...) were left unset
time_parse_recast <- function(x, chronon, cycle, discrete) {
  inner <- x@x[[1L]]
  from <- attr(inner, "chronon")
  chronon <- granule_inherit_props(chronon, from)
  if (!is.null(cycle)) cycle <- granule_inherit_props(cycle, chronon)

  data <- vctrs::vec_data(vecvec::unvecvec(x))

  # A tz-naive parse has no absolute instant to convert from - reinterpret it
  # as local wall-clock time in the target tz before any granule conversion.
  if (
    S7::S7_inherits(from, mt_tz_unit) && S7::S7_inherits(chronon, mt_tz_unit) &&
      is.na(tz_name(from)) && !is.na(tz_name(chronon))
  ) {
    from <- granule_inherit_props(from, mt_tz_unit(tz = tz_name(chronon)))
    if (tz_name(from) != "UTC") data <- tz_to_utc(data, from, tz_name(from), discrete = discrete)
  }

  if (!identical(chronon, from)) {
    data <- chronon_convert_impl(data, from, chronon, discrete)
  }

  new_mixtime(
    if (is.null(cycle)) {
      mt_linear(data, chronon = chronon)
    } else {
      mt_cyclical(data, chronon = chronon, cycle = cycle)
    }
  )
}

time_parse_warn_failures <- function(bad, show = 5L) {
  n <- length(bad)
  cli::cli_warn(
    c(
      "{n} value{?s} failed to parse and {cli::qty(n)} {?was/were} set to {.code NA}.",
      "x" = "{.val {utils::head(bad, show)}}{if (n > show) paste0(', and ', n - show, ' more') else ''}"
    ),
    class = "mixtime_parse_failure",
    call = NULL
  )
}

# Parse time sequence strings into time units
# TODO - Generalise to be calendar-aware
#        e.g. "year" being cal_gregorian$year() or cal_gregorian$isoyear()
parse_time_unit <- function(x) {
  by2 <- strsplit(x, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) 
      stop("invalid 'by' string")
  n <- if(length(by2) == 2L) as.integer(by2[[1L]]) else 1L
  x <- by2[[length(by2)]]
  
  switch(sub("s$", "", x),
    "second" = ,
    "sec" = cal_gregorian$second(n),
    "minute" = ,
    "min" = cal_gregorian$minute(n),
    "hour" = cal_gregorian$hour(n),
    "day" = cal_gregorian$day(n),
    "dstday" = cal_gregorian$day(n), # DSTdays map to regular days
    "week" = cal_isoweek$week(n),
    "month" = cal_gregorian$month(n),
    "year" = cal_gregorian$year(n),
    "quarter" = cal_gregorian$quarter(n),
    stop("Unknown time unit: '", x, "'. Valid units are: secs, mins, hours, days, weeks, months, years, DSTdays, quarters")
  )
}
