#' Describe a granule's label scheme
#'
#' Builds the function a [linear_labels()]/[cyclical_labels()] method is:
#' assign its result directly, e.g. `method(cyclical_labels, list(granule,
#' cycle)) <- label_scheme(...)`. Three levels, from plain to most irregular:
#'
#' 1. Plain numeric. No `vocab`: labels are `i + start`, optionally
#'    zero-padded to `width`. The default for most granules (minutes,
#'    seconds).
#' 2. Named, regular. `vocab` supplies the names (see [vocab_table()]);
#'    `start` positions raw index 0 in both the numeric and vocab-indexed
#'    rendering. Covers most named calendar units (months, weekdays).
#' 3. Named, irregular. `transform` overrides the plain `start` shift with
#'    hand-written `encode`/`decode`, for cycles where raw index and name
#'    aren't a constant offset apart (e.g. a leap month splitting one name
#'    into two). See "Irregular cycles" below.
#'
#' A scheme is only consulted for named (`label = TRUE`) rendering and its
#' inverse; numeric rendering is always the plain `i + start` shift (see
#' [linear_labels_format()]). Labels that aren't index-shaped at all (e.g.
#' "1BC") override the generics directly, documented in [linear_labels()]
#' instead.
#'
#' @param start Author-fixed. Raw index 0 displays as `start`, shared by
#'   numeric rendering and vocab indexing (e.g. `start = 1L` makes January,
#'   raw index 0, display as `"1"` and index `vocab()[[1]]`). A caller can't
#'   override this: get it wrong and dates are wrong, not just styled
#'   differently.
#' @param vocab `NULL` (numeric only), or a function `function(type = NULL,
#'   locale = NULL)` returning either the full named list of renderings for
#'   one locale (`type = NULL`), or one rendering (e.g.
#'   `vocab("abbreviated")`). [vocab_table()] builds this from a plain
#'   lookup table; see "Locale specific labels" below for how to specify labels.
#' @param transform `NULL` (plain `start` shift), or `list(encode =
#'   function(i, at = NULL) -> vocab index, decode = function(d, at = NULL)
#'   -> raw i)`, overriding the indexing used for named rendering only. Both
#'   functions are authored scalar (`if`/`return`) and vectorized
#'   internally. See "Irregular cycles" below.
#' @param width,locale Caller-overridable defaults for `cyc(granule, cycle,
#'   width = ...)`/`locale = ...` at format/parse time, not calendar facts.
#'   `width` zero-pads numeric rendering (e.g. `width = 2L` for `"02"`);
#'   `locale` selects which `vocab()` entry to use by default.
#'
#' @return A function `function(granule, cycle) list(start =, vocab =,
#'   transform =, width =, locale =)`, assignable directly as a
#'   [linear_labels()]/[cyclical_labels()] method. `granule`/`cycle` are only
#'   used for dispatch: a scheme describes a granule class, not the instance
#'   a method happens to be called with.
#'
#' @section Locale specific labels:
#' `vocab` is a plain closure, not a shared registry, so two calendar
#' packages can't collide on it. To add locale support to an existing
#' granule, redefine the `linear_labels()`/`cyclical_labels()` method and
#' call the previous one for anything you're not changing: the normal S7
#' rule that the last `method(...) <-` wins. That needs a hand-written
#' wrapper rather than assigning `label_scheme()`'s result directly, since
#' overriding one field means fetching the old scheme and changing it:
#'
#' ```r
#' scheme <- cyclical_labels(cal_gregorian$month(1L), cal_gregorian$year(1L))
#' method(cyclical_labels, list(cal_gregorian$month, cal_gregorian$year)) <-
#'   function(granule, cycle) {
#'     scheme$vocab <- function(type = NULL, locale = NULL) {
#'       if (identical(locale %||% "en-GB", "fr-FR")) {
#'         fr <- list(
#'           wide = c("janvier", "février", "mars", "avril", "mai", "juin",
#'                    "juillet", "août", "septembre", "octobre", "novembre", "décembre"),
#'           abbreviated = c("janv.", "févr.", "mars", "avr.", "mai", "juin",
#'                           "juil.", "août", "sept.", "oct.", "nov.", "déc.")
#'         )
#'         return(if (is.null(type)) fr else fr[[type]])
#'       }
#'       scheme$vocab(type, locale)
#'     }
#'     scheme
#'   }
#' ```
#'
#' Locale tags follow BCP 47 (`"en-GB"`, `"fr-FR"`), like ICU, `stringi` and
#' `clock`. Type names follow CLDR's wide/abbreviated/narrow, so CLDR data
#' (e.g. `stringi::stri_datetime_symbols()`) works with [vocab_table()]
#' unchanged. Other `type` values are allowed too, e.g. `"emoji"` for lunar
#' phases, picked at render time via [cyclical_labels_format()]'s `type`
#' argument.
#'
#' @section Irregular cycles:
#' `at` (in `transform`'s `encode`/`decode`, and in
#' [cyclical_labels_format()]/[cyclical_labels_parse()]) is the coarser
#' granule instance's raw position, not epoch-shifted, with `n = 1`. It
#' follows the same convention as [chronon_cardinality()]. It lets
#' `transform` pick the right name when that name depends on which cycle
#' instance a raw index falls in, e.g. the Hebrew calendar's leap year
#' splitting "Adar" into "Adar I"/"Adar II":
#'
#' ```r
#' month_names <- c("Tishrei", "Cheshvan", "Kislev", "Tevet", "Shevat",
#'                   "Adar", "Adar I", "Adar II",
#'                   "Nisan", "Iyar", "Sivan", "Tammuz", "Av", "Elul")
#'
#' method(cyclical_labels, list(cal_hebrew$month, cal_hebrew$year)) <- label_scheme(
#'   start = 1L,
#'   vocab = vocab_table(`en-GB` = list(wide = month_names, abbreviated = month_names)),
#'   transform = list(
#'     encode = function(i, at) {
#'       leap <- is_hebrew_leap_year(at)
#'       if (i <= 4L) return(i + 1L)
#'       if (!leap) return(if (i == 5L) 6L else i + 3L)
#'       if (i == 5L) return(7L)
#'       if (i == 6L) return(8L)
#'       i + 2L
#'     },
#'     decode = function(d, at) {
#'       leap <- is_hebrew_leap_year(at)
#'       if (d <= 5L) return(d - 1L)
#'       if (!leap) {
#'         if (d == 6L) return(5L)
#'         if (d %in% c(7L, 8L)) cli::cli_abort("'Adar I'/'Adar II' are not valid outside a leap year.")
#'         return(d - 3L)
#'       }
#'       if (d == 6L) cli::cli_abort("'Adar' is ambiguous in a leap year, use 'Adar I' or 'Adar II'.")
#'       if (d == 7L) return(5L)
#'       if (d == 8L) return(6L)
#'       d - 2L
#'     }
#'   )
#' )
#' ```
#'
#' The aborts in `decode()` matter: without them, "Adar II" typed in a
#' common year would fall through to the same offset as plain "Adar" and
#' silently parse to the wrong month. Only the label layer, which knows
#' both the text and `at`, can catch that. `transform`'s leap predicate must
#' also be the same one [chronon_cardinality()] uses for this pair; put it
#' in one shared helper.
#'
#' @seealso [linear_labels()]/[cyclical_labels()], the generics a scheme is
#'   returned from; [vocab_table()] for the common hand-listed name table
#'   case.
#' @export
label_scheme <- function(
  start = 0L,
  vocab = NULL,
  transform = NULL,
  width = NULL,
  locale = NULL
) {
  fields <- list(
    start = start,
    vocab = vocab,
    transform = transform,
    width = width,
    locale = locale
  )
  # granule/cycle are unused, only here so this can be assigned directly as
  # a linear_labels()/cyclical_labels() method. S7 forbids default values on
  # dispatch args, so this can't be called with no arguments either.
  function(granule, cycle) fields
}

#' A granule's label scheme
#'
#' `linear_labels()` and `cyclical_labels()` are S7 generics returning a
#' granule's label scheme: a plain list describing how its internal position
#' renders as text and parses back (`start`, `vocab`, `transform`, `width`,
#' `locale`; see [label_scheme()]).
#'
#' [linear_labels_format()]/[linear_labels_parse()] (and their cyclical
#' counterparts) do the actual rendering/parsing, with the default method
#' looking up the scheme and applying it. A granule with labels that are
#' incompatible with label schemes (e.g. 2BC, 1BC, 1, 2, ...) registers a method
#' directly on those generics instead, skipping the scheme system. See
#' "Overriding the generics directly" below.
#'
#' @param granule A time granule instance (e.g. `month(1L)`).
#' @param cycle For `cyclical_labels()`, the coarser granule instance this
#'   cycle repeats within (e.g. `year(1L)`).
#' @param ... Unused; only present for S7 dispatch.
#'
#' @return A plain list, as constructed by [label_scheme()].
#'
#' @section Overriding the format and parsing methods:
#' `linear_labels_format()`/`linear_labels_parse()` are ordinary S7
#' generics with a default method, not the only place formatting logic can
#' live. So escaping the scheme system for one granule is just registering
#' a method directly:
#'
#' ```r
#' method(linear_labels_format, cal_gregorian$year) <- function(granule, i, ...) {
#'   ifelse(i <= 0L, paste0(-i + 1L, "BC"), i)
#' }
#' method(linear_labels_parse, cal_gregorian$year) <- function(granule, ...) {
#'   list(
#'     pattern = "\\d+(?:BC)?",
#'     decode = function(text, at = NULL) {
#'       bc <- grepl("BC$", text)
#'       n <- as.integer(sub("BC$", "", text))
#'       ifelse(bc, 1L - n, n) - chronon_epoch(granule)
#'     }
#'   )
#' }
#' ```
#'
#' `decode()` needs `- chronon_epoch(granule)` because this method bypasses
#' the default, which normally does that step (see [linear_labels_format()]'s
#' "Epoch shift" section).
#'
#' The `...` on `_format()` is not just style: `format()` forwards whatever
#' [component_helpers]' `lin()`/`cyc()` were called with (`label`,
#' `abbreviate`, `type`, `width`, `locale`, ...) as named arguments. A
#' method without `...` errors with "unused argument" as soon as a caller
#' passes something it doesn't declare.
#'
#' Registering only `linear_labels_format()` is fine: that's the same "no
#' parsing yet" state a fresh granule starts in. But registering it without
#' `linear_labels_parse()` leaves parsing silently wrong instead of just
#' missing: text like `"1BC"` falls through to whatever the granule's
#' scheme says, usually plain numeric decoding, which doesn't understand
#' "BC" at all.
#'
#' @seealso [label_scheme()] for the fields a method returns;
#'   [linear_labels_format()]/[cyclical_labels_format()] for the generics
#'   that interpret it; [vocab_table()] for the common hand-listed name
#'   table case.
#'
#' @examples
#' # A scheme for months of the year, using R's localised month names.
#' S7::method(cyclical_labels, list(cal_gregorian$month, cal_gregorian$year)) <- label_scheme(
#'   start = 1L, width = 2L,
#'   vocab = vocab_table(`en-GB` = list(wide = month.name, abbreviated = month.abb))
#' )
#'
#' @rdname label_scheme
#' @export
linear_labels <- S7::new_generic("linear_labels", "granule")

#' @rdname label_scheme
#' @export
cyclical_labels <- S7::new_generic("cyclical_labels", c("granule", "cycle"))

method(linear_labels, mt_unit) <- label_scheme(start = 0L)

# start = 0L here because this fallback must leave raw i unchanged,
# matching the old behaviour before schemes existed. start = 1L is a good
# default when an author declares a scheme, not when nobody has customised
# anything.
method(cyclical_labels, list(mt_unit, mt_unit)) <- label_scheme(start = 0L)

#' Render and parse a granule's labels
#'
#' The generics turning a granule's internal position into display text
#' (`linear_labels_format()` for a non-repeating position, e.g. the year;
#' `cyclical_labels_format()` for a position within a larger cycle, e.g. the
#' month within the year) and back (`linear_labels_parse()`/
#' `cyclical_labels_parse()`).
#'
#' Each has one default method (on the base `mt_unit` class) that looks up
#' the granule's scheme via [linear_labels()]/[cyclical_labels()] and
#' applies it (see [label_scheme()] for the fields). Calendar authors
#' usually declare a scheme instead of writing a method here directly.
#' Writing one directly overrides the generics for labels that aren't
#' index-shaped at all, see [linear_labels()]'s "Overriding the generics
#' directly" section.
#'
#' @param granule A time granule object representing the granule (e.g.
#'   `month(1L)`).
#' @param cycle A time granule object representing the cycle (e.g.
#'   `year(1L)`).
#' @param ... Passed on to the method. The default method (see below) takes:
#'   - `i`: Integer vector: the position along the linear axis, or within
#'     the cycle for `cyclical_labels_format()`.
#'   - `at`: The linear position of the cycle granule, letting a method
#'     produce labels specific to that cycle instance (mainly for irregular
#'     cycles, e.g. a leap month). Same convention as `at` in
#'     [chronon_cardinality()].
#'   - `label`: If `TRUE`, return named labels (e.g. "February"). If
#'     `FALSE`, return the numeric position as character.
#'   - `abbreviate`: If `TRUE`, return abbreviated labels (e.g. "Feb"). If
#'     `FALSE`, return full labels (e.g. "February").
#'   - `type`: Overrides `abbreviate`'s abbreviated/wide choice of
#'     [vocab_table()] type outright, e.g. `type = "emoji"`. `NULL` (default)
#'     defers to `abbreviate`.
#'   - `width`,`locale`: Call-time overrides of the scheme's `width`/`locale`
#'     defaults (see [label_scheme()]); `NULL` defers to the scheme.
#'
#' @return Character vector of labels for the time point.
#'
#' @section Epoch shift:
#' `linear_labels_format()`'s `i` arrives already epoch-shifted for display
#' (`chronon_parts()` adds [chronon_epoch()] before calling it, e.g. a
#' Gregorian year's `i` is `2024`, not `54`). The default
#' `linear_labels_parse()` method shifts `decode()`'s output back, by
#' subtracting [chronon_epoch()] after the scheme's own decode runs. A
#' hand-written override method skips this and must do the same
#' subtraction itself if the granule has a non-zero epoch; see the example
#' in [linear_labels()].
#'
#' @seealso [linear_labels()]/[cyclical_labels()] for the authoring
#'   interface these generics' default methods interpret.
#'
#' @examples
#' # Labels for years on a linear axis
#' with(cal_gregorian, linear_labels_format(year(1L), 2020:2025))
#'
#' # Labels for months in a year
#' with(cal_gregorian, cyclical_labels_format(month(1L), year(1L), 0:11))
#'
#' @rdname label_format
#' @export
linear_labels_format <- S7::new_generic("linear_labels_format", "granule")

method(linear_labels_format, mt_unit) <- function(
  granule,
  i,
  label = FALSE,
  abbreviate = TRUE,
  type = NULL,
  width = NULL,
  locale = NULL,
  ...
) {
  scheme <- linear_labels(granule)
  label_render(
    i,
    at = NULL,
    label = label,
    abbreviate = abbreviate,
    type = type,
    width = width %||% scheme$width,
    locale = locale %||% scheme$locale,
    start = scheme$start,
    vocab = scheme$vocab,
    transform = scheme$transform,
    # No vocab: fall back to the pre-scheme "<i><abbr>" style (e.g. "5Y").
    fallback_label = function(i) {
      paste0(i, if (abbreviate) time_unit_abbr(granule) else time_unit_plural(granule, i))
    }
  )
}

#' @rdname label_format
#' @export
cyclical_labels_format <- S7::new_generic(
  "cyclical_labels_format",
  c("granule", "cycle")
)

method(cyclical_labels_format, list(mt_unit, mt_unit)) <- function(
  granule,
  cycle,
  i,
  at = NULL,
  label = FALSE,
  abbreviate = TRUE,
  type = NULL,
  width = NULL,
  locale = NULL,
  ...
) {
  scheme <- cyclical_labels(granule, cycle)
  label_render(
    i,
    at = at,
    label = label,
    abbreviate = abbreviate,
    type = type,
    width = width %||% scheme$width,
    locale = locale %||% scheme$locale,
    start = scheme$start,
    vocab = scheme$vocab,
    transform = scheme$transform,
    # Same fallback as linear, "<abbr><i>" order (e.g. "M5" not "5M").
    fallback_label = function(i) {
      paste0(if (abbreviate) time_unit_abbr(granule) else time_unit_plural(granule, i), i)
    }
  )
}

#' @rdname label_format
#' @export
linear_labels_parse <- S7::new_generic("linear_labels_parse", "granule")

method(linear_labels_parse, mt_unit) <- function(granule, ...) {
  scheme <- linear_labels(granule)
  spec <- label_parse_spec(
    start = scheme$start,
    vocab = scheme$vocab,
    transform = scheme$transform
  )
  # Undo chronon_parts()'s epoch shift, to match time_compose()'s raw convention.
  spec$decode <- local({
    inner <- spec$decode
    function(text, at = NULL) inner(text, at) - chronon_epoch(granule)
  })
  spec
}

#' @rdname label_format
#' @export
cyclical_labels_parse <- S7::new_generic(
  "cyclical_labels_parse",
  c("granule", "cycle")
)

method(cyclical_labels_parse, list(mt_unit, mt_unit)) <- function(
  granule,
  cycle,
  ...
) {
  scheme <- cyclical_labels(granule, cycle)
  label_parse_spec(
    start = scheme$start,
    vocab = scheme$vocab,
    transform = scheme$transform
  )
}

# Wraps a scalar transform (written with if/return) so it can be called
# with vectors, one at value per element. force(f) because callers
# reassign the result onto the same name; without forcing, f would stay
# lazy and recurse into itself.
vectorize_over_at <- function(f) {
  force(f)
  function(x, at = NULL) {
    n <- length(x)
    at <- if (is.null(at)) vector("list", n) else as.list(rep_len(at, n))
    unlist(.mapply(f, list(as.list(x), at), NULL))
  }
}

# Shared rendering logic behind the *_format() default methods. Numeric
# rendering never uses transform or locale. Only label = TRUE rendering
# needs vocab (and transform$encode for irregular cycles). With no vocab,
# fallback_label(i) runs instead.
label_render <- function(
  i,
  at,
  label,
  abbreviate,
  type,
  width,
  locale,
  start,
  vocab,
  transform,
  fallback_label
) {
  if (!label) {
    idx <- i + start
    if (!is.null(width)) {
      return(sprintf(paste0("%0", width, "d"), idx))
    }
    return(as.character(idx))
  }
  if (is.null(vocab)) {
    return(fallback_label(i))
  }
  encode <- if (is.null(transform)) {
    function(i, at = NULL) i + start
  } else {
    vectorize_over_at(transform$encode)
  }

  # type lets a caller pick a vocab_table() type other than wide/abbreviated
  # (e.g. "emoji").
  type <- type %||% if (abbreviate) "abbreviated" else "wide"
  names <- vocab(type, locale)
  unname(names[encode(i, at)])
}

# Shared spec-builder behind the *_parse() default methods (see
# linear_labels_format()'s docs for the pattern/decode shape returned).
# Mirrors label_render(): a numeric token always decodes as d - start and
# skips transform; only a vocab-name token goes through transform$decode.
label_parse_spec <- function(start, vocab, transform) {
  if (is.null(vocab)) {
    return(list(
      pattern = "\\d+",
      decode = function(text, at = NULL) as.integer(text) - start
    ))
  }

  # vocab() with no type returns every rendering for the default locale.
  # Types are parallel-indexed, so a match in any one gives the same vocab
  # index.
  vocab_all <- vocab()
  all_names <- unlist(vocab_all, use.names = FALSE)
  uniq_names <- unique(all_names)
  # Escape regex metacharacters (e.g. the "." in French "janv.") via a
  # bracket expression + backreference before matching them literally.
  escaped_names <- gsub(
    "([.|()\\^{}+$*?\\[\\]\\\\])",
    "\\\\\\1",
    uniq_names[order(-nchar(uniq_names))],
    perl = TRUE
  )
  pattern <- paste0(
    "(?i:", paste(c(escaped_names, "\\d+"), collapse = "|"), ")"
  )

  decode <- if (is.null(transform)) {
    function(d, at = NULL) d - start
  } else {
    vectorize_over_at(transform$decode)
  }

  list(
    pattern = pattern,
    decode = function(text, at = NULL) {
      out <- integer(length(text))
      numeric <- grepl("^\\d+$", text)
      if (any(numeric)) {
        out[numeric] <- as.integer(text[numeric]) - start
      }
      if (any(!numeric)) {
        text_lower <- tolower(text[!numeric])
        idx_by_type <- lapply(vocab_all, function(v) match(text_lower, tolower(v)))
        vocab_idx <- Reduce(function(a, b) ifelse(is.na(a), b, a), idx_by_type)
        if (anyNA(vocab_idx)) {
          cli::cli_abort(
            "Unrecognised label {.val {text[!numeric][is.na(vocab_idx)][1L]}}."
          )
        }
        out[!numeric] <- decode(vocab_idx, at[!numeric])
      }
      out
    }
  )
}

#' Build a `vocab` function from a plain name table
#'
#' The common case for a [label_scheme()] `vocab` argument: a hand-listed
#' table of names, one entry per locale, each a named list of renderings. By
#' convention these follow CLDR's wide/abbreviated/narrow, but `type` can be
#' anything. `vocab_table()` wraps it in the `function(type = NULL, locale =
#' NULL)` shape `vocab` requires.
#'
#' For vocab backed by an external i18n source instead of a hand-listed
#' table, write that `function(type, locale)` directly instead. Same shape,
#' no list needed.
#'
#' @param ... Named entries, one per locale (e.g. `` `en-GB` = list(wide =
#'   month.name, abbreviated = month.abb)``). Locale tags follow BCP 47.
#' @param default_locale The locale returned when `locale` is unsupplied (or
#'   explicitly `NULL`, meaning "caller didn't ask", since
#'   `time_parse()`/`format()` always pass some value for `locale`).
#'
#' @return A function `function(type = NULL, locale = NULL)`, suitable as
#'   the `vocab` field of a [label_scheme()].
#'
#' @examples
#' month_vocab <- vocab_table(`en-GB` = list(wide = month.name, abbreviated = month.abb))
#' month_vocab("abbreviated")
#' month_vocab(locale = "en-GB")
#'
#' @seealso [label_scheme()]
#' @export
vocab_table <- function(..., default_locale = "en-GB") {
  table <- list(...)
  function(type = NULL, locale = NULL) {
    locale <- locale %||% default_locale
    entry <- table[[locale]] %||%
      cli::cli_abort("Unsupported locale: {.val {locale}}")
    if (is.null(type)) {
      return(entry)
    }
    entry[[type]] %||%
      cli::cli_abort(
        "Unsupported type {.val {type}} for locale {.val {locale}}"
      )
  }
}
