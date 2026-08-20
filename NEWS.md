# mixtime 0.3.0

## New features

* Added `time_components()`, which decomposes a time vector into its
  constituent parts using `dplyr::mutate()`-like semantics: named `lin()`/
  `cyc()` expressions (the same vocabulary used in `format()` strings) are
  resolved together in a single decomposition of `x`, returning a data frame
  of linear (`lin()`) and cyclical (`cyc()`) component time vectors (e.g.
  `time_components(yearmonth(x), yr = lin(year), mth = cyc(month, year))`).

* Added `time_compose()`, the inverse of `time_components()`: builds a single
  `mixtime` time point from a set of `lin()`/`cyc()` components, supplied
  either as `spec ~ value` formulas (e.g.
  `time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3` for March 1980) or as
  already-tagged linear/cyclical time vectors.

* Added `time_is_determinate_at()`, which tests whether a time point resolves
  a given (typically finer) granule exactly: a discrete time point never
  determines a finer granule (a year has no determinate month), while a
  continuous time point tracks progress within its chronon and so always does
  (0% through 2020 is 0% through January).

* Added `time_is_complete_at()`, which tests, for each element of a time
  vector, whether the coarser granule it falls into is fully observed *by the
  vector as a whole* - unlike `time_is_determinate_at()`, completeness is a
  collective property: an element is `TRUE` only when the other elements
  needed to fill out its granule are also present elsewhere in the vector
  (e.g. a year is only complete once all twelve of its months are present).

* Cyclical time vectors can now be compared with `==`, `!=`, `<`, `<=`, `>` and
  `>=`, which previously errored. A cyclical value means a position within its
  cycle rather than an instant, so comparison is made on that position: two
  Wednesdays are equal regardless of which week they fall in.

* Added `chronon_cardinality_fixed()`, a variant of `chronon_cardinality()`
  for time granule pairs whose relationship is a constant, context-independent
  number (e.g., 60 seconds in a minute) rather than dependent on an `at` time
  point (e.g., 28-31 days in a month).

* Added civil time microseconds and nanoseconds to `cal_time_civil` (#92).

* Added `linear_labels()` and `cyclical_labels()`, an authoring interface for
  declaring a calendar granule's label scheme once and getting both text
  *formatting* and its `time_parse()`-ready inverse, text *parsing*
  (`linear_labels_parse()`/`cyclical_labels_parse()`), registered together so
  the two directions can't drift apart. Covers plain numeric labels, named
  labels from a lookup table (`vocab_table()`), irregular cycles where the
  raw-index-to-name mapping isn't a constant shift (`transform`, e.g. a leap
  month that splits one name into two), and a full `format`/`parse` escape
  hatch for labels that aren't index-shaped at all (e.g. "2BC"). See
  `?cyclical_labels`. A scheme is built with `label_scheme()` and assigned
  directly as a `linear_labels()`/`cyclical_labels()` method (e.g.
  `method(cyclical_labels, list(granule, cycle)) <- label_scheme(...)`).
  `validate_label_scheme()` checks a scheme's format/parse round-trip, most
  useful for a hand-written `transform`.

## Deprecated

* `new_time()`, the low-level constructor for `mt_time` vectors, is deprecated
  in favour of calling the concrete time class constructors directly:
  `mt_linear()`, `mt_duration()`, and `mt_cyclical()`.

## Breaking changes

* `is_time_linear()`, `is_time_cyclical()`, and `is_time_duration()` have been
  renamed to `time_is_linear()`, `time_is_cyclical()`, and `time_is_duration()`
  for consistency with semantic property functions.

* `tz_offset()` now returns a `mixtime` duration vector instead of a plain
  numeric vector, so offsets carry their chronon (and can be combined with
  other durations) rather than being a bare count of seconds/days/etc.

* `tz_transitions()` now returns `time` as a continuous `mixtime` linear time
  point (UTC seconds) and `offset_before`/`offset_after` as `mixtime`
  durations (UTC seconds), instead of plain numeric columns.

* Added `at` argument to `cyclical_labels()`, which provides the linear position
  of the cycle granule to allow appropriate labelling of irregular cycles, the 
  same convention as `chronon_cardinality()`'s `at` argument (#100).

* `linear_labels()` and `cyclical_labels()`, the S7 generics that turn a
  granule's position into label text, have been renamed to
  `linear_labels_format()` and `cyclical_labels_format()`. The bare names are
  now the authoring interface described above; a hand-written
  `method(linear_labels_format, ...) <-`/`method(cyclical_labels_format, ...) <-`
  is still available for granules that only ever needs formatting (no parsing).

## Bug fixes

* `cal_gregorian`'s day<->month conversion (`chronon_cardinality()` and
  `chronon_divmod()`) now supports a month chronon of *any* non-zero size
  (previously capped at `abs(n) < 12`, e.g. `month(13L)`/`years(1L) +
  months(6L)`-scale steps errored with "Month chronons >= 12 are not yet
  supported"), and fixes two correctness bugs uncovered while generalising
  it, both specific to a multi-month (`abs(n) > 1`) chronon:
  `chronon_divmod(day, month)`'s `mod` used to be the offset into the
  *calendar* month containing the input day, which silently dropped any
  whole months already elapsed earlier in a wider window (e.g. `seq(date(
  "2020-02-15"), by = "2 months")` produced `2020-01-15, 2020-03-15, ...`,
  a full window early, instead of `2020-02-15, 2020-04-15, ...`); and
  `chronon_divmod(day, month)`'s `div` used to be `fdiv(res, res_scale)`
  unconditionally, which only agrees with `chronon_cardinality()`'s window
  anchor (`at * n`, always counting the window forward regardless of sign)
  when `res_scale > 0` - for a negative multi-month `to` it instead picked
  the window running backwards from the input day. Both the `>= 12` cap and
  the old `circsum()`-tabulated approach it came from are gone: day counts
  for a window are now a difference of two calls to a new O(1)
  `month_start_days()` helper (the same closed-form leap-year arithmetic
  `chronon_divmod(year, day)` already used, reused here anchored to a month
  instead of a year), so the cost no longer scales with the chronon's `n`.

* `chronon_nests_in_fixed()` (and so `chronon_needs_clamping()`, which it
  backs) previously ignored `@n` entirely for a cross-class pair, treating
  e.g. `month(5L)` and `month(2L)` identically as long as month and year were
  connected at all - so it reported `month(5L)` as nesting inside `year(1L)`
  even though 12 months/year isn't a whole multiple of 5. Fixed to check the
  same divisibility the identical-class case already did, generalised across
  a fixed-cardinality chain (multiplying `chronon_cardinality_fixed()` along
  it, e.g. 3 months/quarter * 4 quarters/year = 12 months/year).

* `chronon_nests_in()` previously ignored `@n` entirely for any cross-class
  pair, reporting e.g. both `month(5L)` as nesting inside `year(1L)` (wrong -
  12 months/year isn't a whole multiple of 5, so a 5-month block never lands
  on a year boundary) and `day(2L)` as nesting inside `month(1L)` (also wrong
  in general - whether a 2-day block straddles a month boundary depends on
  the parity of that month's start day-of-epoch). Fixed to match
  `chronon_nests_in_fixed()`'s exact answer whenever a fixed (context-
  independent) path connects the two classes (that path's ratio is constant,
  so if it doesn't divide evenly, no other path could make it nest either).
  For a pair connected only by an irregular (context-dependent) path, a
  single-unit (`n = 1`) chronon still always nests (it's never split across a
  boundary), but any other `n` now errors instead of silently assuming it
  nests - checking every `at` in the cycle isn't generally possible, since
  cycle length isn't a declared, inspectable property of
  `chronon_cardinality()` methods. `chronon_needs_clamping()` (which uses
  both functions together) is unaffected in practice - it now no longer
  fires for a fixed-reachable, non-dividing pair like `month(5L)`/`year(1L)`,
  but no built-in calendar arithmetic exercises that combination.

* `seq()` on a `linear_time` with a `by` whose magnitude doesn't divide
  evenly into the coarser calendar unit it's ultimately anchored to (e.g.
  `by = months(5L)`, `by = months(13L)`, `by = "18 months"` - none of 5, 13,
  18 divide 12) now errors instead of silently producing calendar-drifted
  output. `seq()`'s clamped path decomposes `from` once and then advances by
  whole-number offsets in a single pass, which assumes every `by`-sized block
  has the same shape - only true when `by`'s magnitude divides evenly into
  that coarser unit (checked with the fixed `chronon_nests_in_fixed()` fix
  above). When it doesn't, blocks land on a different month-of-year each
  step and the output silently drifts (e.g. `seq(date("2020-01-31"), by =
  months(18L), length.out = 4)` used to give `2020-01-31, 2021-08-03,
  2023-01-31, 2024-08-02` - only every second step agreeing with repeated
  `date + months(18L)`, which is unaffected by this and already correct).
  Supporting this case properly (matching what repeated `+`/`-` already do)
  is left for later; `by = months(1L)/(2L)/(3L)/(4L)/(6L)/(12L)` and `by =
  years(n)` are all unaffected, and `+`/`-` were never affected (each call
  re-decomposes the current instant rather than batching an offset from a
  single decomposition).

* `linear_time + duration` and `linear_time - duration` across chronons of
  different granularity (e.g. `date() + months()`, `date() + years()`), and
  `seq()` on a `linear_time` with a coarser `by` (e.g. `seq(date, by = "1
  month")`), now use calendar-field arithmetic: the duration's/`by`'s own
  chronon is advanced by its magnitude, decomposing along the *longest*
  available chain of calendar boundaries (e.g. day -> month -> year, not a
  shorter direct day -> year edge) so that only the specific invalid
  component (e.g. day-of-month) is clamped to the target period's valid
  range, and every other component - including time-of-day - is preserved
  exactly (e.g. `date("2020-01-31") + months(1)` is now `2020-02-29`, not
  `2020-03-02`; `date("2024-02-29") + years(1)` now agrees with
  `+ months(12)`, both giving `2025-02-28`). Previously the shift was
  approximated as `n * cardinality_at_start`, which only happened to be exact
  for `n == 1` additions within same-length periods, and compounded error for
  larger `n` (`date("2026-03-25") + months(6)` was `2026-09-27`, two days
  late; twelve months never returned to the same day of the same month a year
  later). See `_dev/clamping.md` for the design and remaining open questions.

* Fixed `linear_time + duration`/`- duration` erroring with "Cannot convert
  from timezone-naive chronon to timezone-aware chronon" whenever a
  timezone-aware time point (e.g. `datetime(tz = "Australia/Melbourne")`) was
  shifted by a duration with a regular (fixed-cardinality) relationship to the
  time's chronon, such as `hours()`, `minutes()`, `seconds()`, or `days()`
  (`months()`/`years()` were unaffected, as they use the calendar-clamping
  path above). This was a regression from the change above: the shift's
  non-clamping path round-trips the time through the duration's chronon and
  back, and the return leg needs the duration's chronon to carry the time's
  timezone first, the same way `seq()` already does.

* Fixed `seq()` on a `linear_time` with a negative-magnitude `by` (e.g.
  `seq(date("2020-03-31"), by = "-1 month", length.out = 3)`,
  `by = months(-1L)`) erroring with `'from' must be a finite number`, and,
  once that was fixed, clamping to the wrong end of the target period for
  descending sequences (`2020-03-31, 2020-03-02, 2020-01-31` instead of
  `2020-03-31, 2020-02-29, 2020-01-31`). Also fixes `seq()`/calendar-field
  arithmetic silently landing on the wrong date for any multi-unit calendar
  `by`/duration whose target chronon isn't `n = 1` (e.g. `by = "2 months"`).
  The negative-`by` error was `circsum()` (the internal circular rolling-sum
  helper backing days-in-a-month lookups) rejecting a negative `size`/`step`
  outright; it now gives both a real meaning instead - a negative `step`
  walks the same windows backwards, and a negative `size` anchors a window
  at its end instead of its start (a trailing rather than leading rolling
  sum) - which a negative-`n` month chronon needs directly. The multi-unit
  case was a stray reassignment in `R/cal_gregorian.R`'s day<->month
  conversion left over from an unrelated rebase, corrupting results for any
  target month-chronon whose multiplier wasn't exactly 1.

* Combining two cyclical time vectors with different cycles is now an error
  rather than silently collapsing them. A cycle is a modulus rather than a unit
  of measure, so unlike a chronon it has no meaningful common value.

* Converting to a finer chronon now takes a path that only ever steps finer,
  instead of the shortest path through the registered calendar arithmetic
  methods. A path that steps coarser first has to express the time point as a
  fraction of the coarser unit, and expanding that fraction again assumes the
  unit subdivides evenly: quarters were routed to days via years, making
  2020 Q1 91.5 days long rather than the 91 days of January to March.

* Fixed comparisons (`==`, `<`, etc.) between a timezone-aware `mt_linear`/
  `mt_cyclical` time point and a naive one, and `c()`/`vec_c()` combining two
  such values, giving numerically wrong results, e.g. `linear_time(
  "2015-02-01 10:00:00", hour(1L, tz = "Australia/Melbourne")) < datetime(
  "2015-02-01 00:00:00")` incorrectly returned `TRUE`, and combining a
  timezone-aware "02:00" with a naive "02:00" produced two different instants
  under the hood despite sharing a (naive) common chronon. Both operations
  convert their operands into a shared common chronon that correctly comes
  out naive when the operands disagree on timezone (see `chronon_common()`),
  but converting an operand *into* that common chronon silently re-inherited
  the operand's own timezone back (one conversion at a time), so a
  timezone-aware operand ended up at its true (UTC) absolute instant while a
  naive operand was left as-is - two different bases being compared/combined
  as if they were the same. `chronon_common()`'s naive result is now hardened
  so it can no longer be re-inherited into, and every operand converted into
  it lands on bare wall-clock time instead, the same way `datetime(x, tz =
  NA)` already strips a timezone for a single value.

## Improvements

* Substantially faster performance by caching the granule cardinality graph 
  (used by `chronon_convert()`, `chronon_divmod()`, `chronon_common()` and
  `time_parts()`) and tzdb names (used in operations with timezones).
* Reworked time vectors to be built with S7 rather than vctrs for improved
  double dispatch methods for arithmetic and other operations (#63).

# mixtime 0.2.0

## New features

* Added support for multiplication and division of time durations.
* Added `round()`/`floor()`/`ceiling()` methods for rounding time by chronons.
  (note: use `time_round()`, `time_floor()`, and `time_ceiling()` for rounding
  to a specified time granule).
* Added casting from time durations to character vectors.
* Added explicit conversion to naive time by using `tz = NA`.

## Improvements

* `time_unit_full()` now uses cli-style pluralisation templates (e.g.,
  `"year{?/s}"`, `"centur{?y/ies}"`), enabling accurate plural forms beyond a
  simple `s` suffix. A new `time_unit_plural(x, n)` helper resolves the
  template for a given quantity.
* Continuous time model durations now always show at least one decimal place.
* Added formatting support for `NaN`, `Inf`, and `-Inf` values in time types.
* Added time zone support for arithmetic.
* Time durations now use the same formatting system and format strings as linear
  and cyclical time.
* Improved `str()` output to be more compact for each time type.

## Bug fixes

* Fixed incorrect usage of `time_chronon()` causing arithmetic to fail.
* Fixed incompatibilities with vctrs sorting, ptype2, and casting methods.
* Fixed formatting of cyclical time with mixed-calendar chronon and cycle (#62).
* Fixed divmod of Gregorian day -> month and day -> year producing incorrect
  divisors near the year boundary for continuous time dates.
* Fixed `c()` allowing non-time vectors in mixtime vectors.

# mixtime 0.1.0

This is the initial CRAN release of the package with provides the core data 
types and temporal manipulation for temporal analysis with mixed granularity
data.

## New features

### Time

* The generic `mixtime()` constructor for creating mixed temporal vectors that 
  combine time points of different granularities (e.g. monthly and quarterly)
  in a single vector via `vecvec`.

### Linear time

* `linear_time()` creates linear time vectors with a user-specified chronon
  (smallest indivisible time granule), supporting both discrete (integer) and
  continuous (fractional) time models.
* Convenience functions for common temporal granularities: `year()`,
  `yearquarter()`, `yearmonth()`, `yearweek()`, `date()`, and `datetime()`.

### Cyclical time

* `cyclical_time()` creates cyclical time vectors representing positions
  within a repeating cycle (e.g. day-of-week, month-of-year).
* Convenience functions for common cyclical temporal granularities:
  `month_of_year()`, `day_of_year()`, `day_of_month()`, `day_of_week()`, 
  `week_of_year()`, and `time_of_day()`.

### Calendar systems

* Three initial calendar systems:
  * `cal_gregorian`: standard Gregorian calendar with year, quarter, month,
    day, hour, minute, second, and millisecond units.
  * `cal_isoweek`: ISO 8601 week-date calendar with ISO year, week, and day
    units. Weeks always start on Monday; the first week of the year is the
    week containing the first Thursday.
  * `cal_sym454`: Symmetry454 perennial solar calendar with a symmetrical
    4–5–4 week pattern per quarter and a leap-week rule.

### Sequences

* `seq()` methods for `mixtime` and `mt_time` objects support integer, string
  (e.g. `"1 month"`), and time unit `by` arguments, as well as `length.out`
  and `along.with`. Overflow behaviour when step granules differ from sequence
  granules can be controlled with `on_invalid = "nearest"` (default) or
  `"overflow"`.

### Rounding

* `round_time()`, `floor_time()`, and `ceiling_time()` round time objects to a
  specified time granule, preserving the input class and timezone.

### Formatting

* A mixed-calendar general purpose formatting system is provided for `mixtime`
  objects. The format strings use glue-like `{}` substitutions with the helper
  functions `lin(x)` and `cyc(x, y)` to position linear and cyclical time
  components in a string. `x` and `y` are time granules from a calendar, which 
  can be used to create general purpose mixed-calendar time formats.

### Timezone support

* `tz_name()` extracts the timezone from a time object.
* `tz_offset()` returns the UTC offset for a datetime in its timezone.
* `tz_abbreviation()` returns the timezone abbreviation (e.g. `"EST"`, `"PDT"`).
* `tz_transitions()` returns a data frame of DST and other timezone transitions
  between two time points.

### Accessor functions

* `time_calendar()` extracts the calendar system from a time object.

### tsibble and fable compatibility

* Methods for `tsibble` index validation are provided, enabling `mixtime`
  vectors to be used as time indexes in `tsibble` objects and in forecasting
  with `fable`.

### Extensibility

* `new_calendar()` defines a calendar as a named collection of time unit
  classes, with optional inheritance from another calendar.
* `mt_unit()`, `mt_tz_unit()`, and `mt_loc_unit()` primitives for creating
  new time units.
* `new_linear_time_fn()` and `new_cyclical_time_fn()` are factories for building
  convenient linear and cyclical time functions (e.g. `yearmonth()`).
* Calendar arithmetic with:

  * `chronon_divmod()` performs division-with-remainder arithmetic between time
    granules (e.g. 1 day to 1 month), enabling cross-granule calendrical 
    arithmetic.
  * `chronon_cardinality()` returns the number of smaller granules contained in 
    a coarser granule (e.g. days in 1 month), with context-dependent results for
    variable-length granules.
  * `chronon_epoch()` returns the epoch offset for a given time unit.

* Displaying time with:

  * `time_unit_abbr()` and `time_unit_full()` for time unit text.
  * `linear_labels()` and `cyclical_labels()` for time labels (e.g. Jan, Feb,
     ... for months of year).
  * `chronon_format_linear()` and `chronon_format_cyclical()` for default
    formatting strings.
  * `chronon_format_attr()` for appending attribute information (e.g. timezones)

### Vignettes

* Extending mixtime - a vignette on how to define new calendars and implement
  calendar arithmetic for temporal operations within the calendar's units and
  with other calendars.
