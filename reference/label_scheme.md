# Describe a granule's label scheme

Builds the function a `linear_labels()`/`cyclical_labels()` method is:
assign its result directly, e.g.
`method(cyclical_labels, list(granule, cycle)) <- label_scheme(...)`.
Three levels, from plain to most irregular:

`linear_labels()` and `cyclical_labels()` are S7 generics returning a
granule's label scheme: a plain list describing how its internal
position renders as text and parses back (`start`, `vocab`, `transform`,
`width`, `locale`; see `label_scheme()`).

## Usage

``` r
label_scheme(
  start = 0L,
  vocab = NULL,
  transform = NULL,
  width = NULL,
  locale = NULL
)

linear_labels(granule, ...)

cyclical_labels(granule, cycle, ...)
```

## Arguments

- start:

  Author-fixed. Raw index 0 displays as `start`, shared by numeric
  rendering and vocab indexing (e.g. `start = 1L` makes January, raw
  index 0, display as `"1"` and index `vocab()[[1]]`). A caller can't
  override this: get it wrong and dates are wrong, not just styled
  differently.

- vocab:

  `NULL` (numeric only), or a function
  `function(type = NULL, locale = NULL)` returning either the full named
  list of renderings for one locale (`type = NULL`), or one rendering
  (e.g. `vocab("abbreviated")`).
  [`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
  builds this from a plain lookup table; see "Locale specific labels"
  below for how to specify labels.

- transform:

  `NULL` (plain `start` shift), or
  `list(encode = function(i, at = NULL) -> vocab index, decode = function(d, at = NULL) -> raw i)`,
  overriding the indexing used for named rendering only. Both functions
  are authored scalar (`if`/`return`) and vectorized internally. See
  "Irregular cycles" below.

- width, locale:

  Caller-overridable defaults for
  `cyc(granule, cycle, width = ...)`/`locale = ...` at format/parse
  time, not calendar facts. `width` zero-pads numeric rendering (e.g.
  `width = 2L` for `"02"`); `locale` selects which `vocab()` entry to
  use by default.

- granule:

  A time granule instance (e.g. `month(1L)`).

- ...:

  Unused; only present for S7 dispatch.

- cycle:

  For `cyclical_labels()`, the coarser granule instance this cycle
  repeats within (e.g. `year(1L)`).

## Value

A function
`function(granule, cycle) list(start =, vocab =, transform =, width =, locale =)`,
assignable directly as a `linear_labels()`/`cyclical_labels()` method.
`granule`/`cycle` are only used for dispatch: a scheme describes a
granule class, not the instance a method happens to be called with.

A plain list, as constructed by `label_scheme()`.

## Details

1.  Plain numeric. No `vocab`: labels are `i + start`, optionally
    zero-padded to `width`. The default for most granules (minutes,
    seconds).

2.  Named, regular. `vocab` supplies the names (see
    [`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md));
    `start` positions raw index 0 in both the numeric and vocab-indexed
    rendering. Covers most named calendar units (months, weekdays).

3.  Named, irregular. `transform` overrides the plain `start` shift with
    hand-written `encode`/`decode`, for cycles where raw index and name
    aren't a constant offset apart (e.g. a leap month splitting one name
    into two). See "Irregular cycles" below.

A scheme is only consulted for named (`label = TRUE`) rendering and its
inverse; numeric rendering is always the plain `i + start` shift (see
[`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)).
Labels that aren't index-shaped at all (e.g. "1BC") override the
generics directly, documented in `linear_labels()` instead.

[`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)/[`linear_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)
(and their cyclical counterparts) do the actual rendering/parsing, with
the default method looking up the scheme and applying it. A granule with
labels that are incompatible with label schemes (e.g. 2BC, 1BC, 1, 2,
...) registers a method directly on those generics instead, skipping the
scheme system. See "Overriding the generics directly" below.

## Locale specific labels

`vocab` is a plain closure, not a shared registry, so two calendar
packages can't collide on it. To add locale support to an existing
granule, redefine the `linear_labels()`/`cyclical_labels()` method and
call the previous one for anything you're not changing: the normal S7
rule that the last `method(...) <-` wins. That needs a hand-written
wrapper rather than assigning `label_scheme()`'s result directly, since
overriding one field means fetching the old scheme and changing it:

    scheme <- cyclical_labels(cal_gregorian$month(1L), cal_gregorian$year(1L))
    method(cyclical_labels, list(cal_gregorian$month, cal_gregorian$year)) <-
      function(granule, cycle) {
        scheme$vocab <- function(type = NULL, locale = NULL) {
          if (identical(locale %||% "en-GB", "fr-FR")) {
            fr <- list(
              wide = c("janvier", "février", "mars", "avril", "mai", "juin",
                       "juillet", "août", "septembre", "octobre", "novembre", "décembre"),
              abbreviated = c("janv.", "févr.", "mars", "avr.", "mai", "juin",
                              "juil.", "août", "sept.", "oct.", "nov.", "déc.")
            )
            return(if (is.null(type)) fr else fr[[type]])
          }
          scheme$vocab(type, locale)
        }
        scheme
      }

Locale tags follow BCP 47 (`"en-GB"`, `"fr-FR"`), like ICU, `stringi`
and `clock`. Type names follow CLDR's wide/abbreviated/narrow, so CLDR
data (e.g.
[`stringi::stri_datetime_symbols()`](https://rdrr.io/pkg/stringi/man/stri_datetime_symbols.html))
works with
[`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
unchanged. Other `type` values are allowed too, e.g. `"emoji"` for lunar
phases, picked at render time via
[`cyclical_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)'s
`type` argument.

## Irregular cycles

`at` (in `transform`'s `encode`/`decode`, and in
[`cyclical_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)/[`cyclical_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md))
is the coarser granule instance's raw position, not epoch-shifted, with
`n = 1`. It follows the same convention as
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md).
It lets `transform` pick the right name when that name depends on which
cycle instance a raw index falls in, e.g. the Hebrew calendar's leap
year splitting "Adar" into "Adar I"/"Adar II":

    month_names <- c("Tishrei", "Cheshvan", "Kislev", "Tevet", "Shevat",
                      "Adar", "Adar I", "Adar II",
                      "Nisan", "Iyar", "Sivan", "Tammuz", "Av", "Elul")

    method(cyclical_labels, list(cal_hebrew$month, cal_hebrew$year)) <- label_scheme(
      start = 1L,
      vocab = vocab_table(`en-GB` = list(wide = month_names, abbreviated = month_names)),
      transform = list(
        encode = function(i, at) {
          leap <- is_hebrew_leap_year(at)
          if (i <= 4L) return(i + 1L)
          if (!leap) return(if (i == 5L) 6L else i + 3L)
          if (i == 5L) return(7L)
          if (i == 6L) return(8L)
          i + 2L
        },
        decode = function(d, at) {
          leap <- is_hebrew_leap_year(at)
          if (d <= 5L) return(d - 1L)
          if (!leap) {
            if (d == 6L) return(5L)
            if (d %in% c(7L, 8L)) cli::cli_abort("'Adar I'/'Adar II' are not valid outside a leap year.")
            return(d - 3L)
          }
          if (d == 6L) cli::cli_abort("'Adar' is ambiguous in a leap year, use 'Adar I' or 'Adar II'.")
          if (d == 7L) return(5L)
          if (d == 8L) return(6L)
          d - 2L
        }
      )
    )

The aborts in `decode()` matter: without them, "Adar II" typed in a
common year would fall through to the same offset as plain "Adar" and
silently parse to the wrong month. Only the label layer, which knows
both the text and `at`, can catch that. `transform`'s leap predicate
must also be the same one
[`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/reference/chronon_cardinality.md)
uses for this pair; put it in one shared helper.

## Overriding the format and parsing methods

[`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)/[`linear_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)
are ordinary S7 generics with a default method, not the only place
formatting logic can live. So escaping the scheme system for one granule
is just registering a method directly:

    method(linear_labels_format, cal_gregorian$year) <- function(granule, i, ...) {
      ifelse(i <= 0L, paste0(-i + 1L, "BC"), i)
    }
    method(linear_labels_parse, cal_gregorian$year) <- function(granule, ...) {
      list(
        pattern = "\\d+(?:BC)?",
        decode = function(text, at = NULL) {
          bc <- grepl("BC$", text)
          n <- as.integer(sub("BC$", "", text))
          ifelse(bc, 1L - n, n) - chronon_epoch(granule)
        }
      )
    }

`decode()` needs `- chronon_epoch(granule)` because this method bypasses
the default, which normally does that step (see
[`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)'s
"Epoch shift" section).

The `...` on `_format()` is not just style:
[`format()`](https://rdrr.io/r/base/format.html) forwards whatever
[component_helpers](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)'
[`lin()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)/[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/reference/component_helpers.md)
were called with (`label`, `abbreviate`, `type`, `width`, `locale`, ...)
as named arguments. A method without `...` errors with "unused argument"
as soon as a caller passes something it doesn't declare.

Registering only
[`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)
is fine: that's the same "no parsing yet" state a fresh granule starts
in. But registering it without
[`linear_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)
leaves parsing silently wrong instead of just missing: text like `"1BC"`
falls through to whatever the granule's scheme says, usually plain
numeric decoding, which doesn't understand "BC" at all.

## See also

`linear_labels()`/`cyclical_labels()`, the generics a scheme is returned
from;
[`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
for the common hand-listed name table case.

`label_scheme()` for the fields a method returns;
[`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)/[`cyclical_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/reference/label_format.md)
for the generics that interpret it;
[`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/reference/vocab_table.md)
for the common hand-listed name table case.

## Examples

``` r
# A scheme for months of the year, using R's localised month names.
S7::method(cyclical_labels, list(cal_gregorian$month, cal_gregorian$year)) <- label_scheme(
  start = 1L, width = 2L,
  vocab = vocab_table(`en-GB` = list(wide = month.name, abbreviated = month.abb))
)
#> Overwriting method cyclical_labels(<mixtime::tu_month>, <mixtime::tu_year>)
```
