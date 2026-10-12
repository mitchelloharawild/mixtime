# Changelog

## mixtime 0.4.0

### Breaking changes

- Converting a discrete time to a chronon it doesn’t nest within is now
  an error, since the result is temporally indeterminate. For example,
  `day_of_week(yearmonth(x))` (which day of the month?) and
  `yearmonth(yearweek(x))` (a week can straddle two months) previously
  used the start of each time point. Continuous times are instants and
  convert as before.

- Shifting a discrete time point by a duration it doesn’t nest within is
  now an error, since the result is temporally indeterminate, matching
  the conversions refused by time constructors. For example,
  `yearweek(x) + years(1L)` (a week can straddle two years) previously
  shifted the start of the week, giving results such as `2020 W01` plus
  one year being `2020 W53`. Shifting by a whole number of chronons is
  still allowed (a week by 14 days, but not 11), as is shifting
  continuous times. This applies to `+`, `-`,
  [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_shift.md),
  and [`seq()`](https://rdrr.io/r/base/seq.html).

- Bare numbers compared with (or cast to) a linear or cyclical time are
  now read as
  [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
  reads them, counting from the chronon’s epoch, so `year(x) == 2026L`
  compares with the year 2026. Previously the number was a raw count of
  chronons since 1970 (2026 was the year 3996).

- The `on_invalid` strategy `"nearest"` is renamed to `"previous"`, the
  backward counterpart of the new `"next"`. It always resolves to the
  end of the target granule, rather than whichever valid time point is
  closest.

### New features

- Added the twelve remaining base relations of Allen’s interval algebra
  as infix operators - `%p%`/`%pi%` (precedes/preceded by), `%m%`/`%mi%`
  (meets/met by), `%o%`/`%oi%` (overlaps/overlapped by), `%s%`/`%si%`
  (starts/started by), `%d%`/`%di%` (during/contains), and `%f%`/`%fi%`
  (finishes/finished by) - completing the set alongside the existing
  `==`.

- Added
  [`chronon_glb()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md)
  and
  [`chronon_lub()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md),
  which find the *greatest lower bound* (the finest common chronon) and
  the *least upper bound* (the coarsest chronon that every input chronon
  evenly aggregates into) of a set of chronons.

- [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_shift.md)
  and [`seq()`](https://rdrr.io/r/base/seq.html) gain two `on_invalid`
  strategies. `"next"` resolves an invalid time point to the start of
  the following granule, so `date("1955-01-31")` plus one month is
  `1955-03-01`, and a local time skipped by daylight saving moves to the
  end of the gap (02:30 becomes 03:00). `"error"` throws an error
  instead of returning `NA` like `"reject"`.

- The new `mixtime.on_invalid` option sets the default `on_invalid`
  strategy for
  [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_shift.md),
  [`seq()`](https://rdrr.io/r/base/seq.html), and the `+` and `-`
  operators, which otherwise clamp.
  [`seq()`](https://rdrr.io/r/base/seq.html) no longer warns about
  clamping when the option is set.

### Deprecated

- [`chronon_common()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md)
  is deprecated in favour of
  [`chronon_glb()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md),
  which names the *greatest lower bound* it finds explicitly and
  distinguishes it from its dual,
  [`chronon_lub()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md).

- [`new_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_time.md),
  deprecated in 0.3.0, is now defunct. Use
  [`mt_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md),
  [`mt_duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md),
  or
  [`mt_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md)
  instead.

### Improvements

- Comparing a discrete time (a span) with a continuous time at the start
  of its chronon (an instant, such as `year(2015)`) now warns when
  reading the instant as a span would change the result: `==` and `!=`
  (never equal), and the orderings that measure against the instant’s
  end (e.g. `span > instant` and `span <= instant`). The warning
  suggests code for the intended span (e.g. `year(2015L)`, `2026L`, or
  `as.integer(myvar)`).

- Performance improvements for temporal operations with time zones.

- Generalised the default parse strings used by
  [`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  and
  [`yearquarter()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)
  to accept strings that are accepted by tsibble equivalents for ease of
  migration (with the intentional exception of dates and datetimes,
  which need to be parsed first with
  [`date()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)
  or
  [`datetime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)).

- Parsing named labels (e.g. month and weekday names) is now
  case-insensitive.

- The chronon of `Date` and `POSIXct` objects is now identified from the
  interval between observations (like tsibble’s interval), rather than
  always being 1 day or 1 second. For example,
  [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
  on hourly datetimes now produces 1 hour chronons. A multiple of a unit
  is used only when every observation is aligned to it, so conversion
  never loses information.

### Bug fixes

- [`chronon_cardinality_fixed()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality_fixed.md)
  now scales its result by the granule sizes (`n`) of both arguments,
  like
  [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
  does (e.g. there are 4 `minute(15L)` in `hour(1L)`, not 60, and 10
  `day(1L)` in `day(10L)`, not 1). Methods still return the ratio
  between unit granules.
- Durations of multi-unit chronons now print with the chronon’s size, so
  `duration(2L, hour(3L))` shows `"6 hours"` (not `"2 hours"`) and the
  chronon of a 3-hourly time vector reads `"3 hours"` (e.g. in
  [`time_chronon()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_chronon.md)
  and [`str()`](https://rdrr.io/r/utils/str.html)), not `"1 hour"`.
- Time-zone-aware days, weeks, months and years now start at local
  midnight, so
  `as.POSIXct(mixtime(x, month(tz = "Australia/Melbourne")))` returns
  00:00 AEDT instead of 11:00. Previously a conversion to UTC read the
  local label as a UTC one.
- Conversions of time-zone-aware times now account for daylight saving
  changes: a day that is 23 or 25 hours long is converted by its real
  length, and the two instants that share a clock reading when daylight
  saving ends are kept apart, including in hourly data.
- The fraction through a continuous time-zone-aware time (e.g. a week)
  is now the fraction through that unit in its own time zone, so
  [`floor()`](https://rdrr.io/r/base/Round.html) of a continuous time
  always gives the matching discrete time.
- Calendar arithmetic on time-zone-aware times (e.g. adding months) now
  keeps the local clock time. A local time that occurs twice when
  daylight saving ends resolves to its first occurrence. A local time
  skipped when daylight saving starts is invalid, and is resolved like
  other invalid time points (e.g. 31 January plus one month): by default
  it is clamped back by the size of the gap, keeping its minutes (02:30
  becomes 01:30 when clocks go forward from 02:00 to 03:00).
  [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_shift.md)
  and [`seq()`](https://rdrr.io/r/base/seq.html) resolve it with their
  `on_invalid` argument: `"previous"` gives the end of the time before
  the gap, `"next"` the start of the time after it (03:00), `"overflow"`
  moves it forward by the gap (03:30), and `"reject"` gives `NA`.
- Shifting or [`seq()`](https://rdrr.io/r/base/seq.html)-ing times with
  a multi-unit chronon (e.g. half-hourly times) by days, months or years
  no longer errors with “Cannot determine whether 30 minutes nest within
  1 year”.
- Parsing a local time skipped when daylight saving starts (e.g.
  `datetime("2025-10-05 02:30:00", tz = "Australia/Melbourne")`) now
  gives `NA` with a warning, like other invalid times such as
  `"2026-02-30"`.
- Adding or subtracting hours, minutes or seconds on time-zone-aware
  times now moves by real time across daylight saving changes, so 02:30
  AEDT plus one hour is 02:30 AEST when daylight saving ends (previously
  03:30 AEST, two real hours later). Days and longer units keep the
  local clock time, so `hours(24L)` and `days(1L)` differ on a 23 or 25
  hour day.
- [`seq()`](https://rdrr.io/r/base/seq.html) of time-zone-aware times by
  days (or weeks) now keeps the local clock time across daylight saving
  changes, so a daily sequence at 10:00 stays at 10:00 (previously it
  moved to 09:00 when daylight saving ended). Each element of
  [`seq()`](https://rdrr.io/r/base/seq.html) now matches adding a
  duration to the start, so a time adjusted because daylight saving
  skipped it returns to its clock time on the following day.
- Converting solar time to time-zone-aware civil time now accounts for
  the time zone, and converting solar time between two locations now
  accounts for the change of location.
- [`tz_abbreviation()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_abbreviation.md)
  (and so [`format()`](https://rdrr.io/r/base/format.html)) now labels a
  time-zone-aware time by the zone in effect when it starts, so a day on
  which daylight saving ends reads as daylight time
  (e.g. `"2015-04-05 AEDT"` in Melbourne, previously `"AEST"`).
- [`format()`](https://rdrr.io/r/base/format.html) of a continuous time
  no longer shows a time just below its true value when floating-point
  error leaves it a fraction of a second short (e.g. `"03:29:59 100.0%"`
  instead of `"03:30:00 0.0%"`).
- [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) of a mixtime
  now keeps its time zone (or uses UTC when it has several), so
  `mixtime(as.POSIXct(x))` round trips. Previously the result had an
  empty time zone, which
  [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
  rejected.
  [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
  now also reads a `POSIXct` with an empty time zone as the local time
  zone, like base R does.
- A fractional time before 1970 within a second of a time zone
  transition (e.g. half a second before New York’s 1960 change to
  daylight time) used the offset from after the transition.

## mixtime 0.3.0

CRAN release: 2026-08-24

### New features

- Added
  [`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md),
  the inverse of [`format()`](https://rdrr.io/r/base/format.html):
  parses text back into a time point using the same
  `{lin(...)}`/`{cyc(...)}` template
  [`format()`](https://rdrr.io/r/base/format.html) uses. Default parse
  strings are specified with
  [`chronon_parse_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)
  and
  [`chronon_parse_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_parse.md)
  (with the
  [`parse_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/parse_format.md)
  helper), mirroring how `chronon_format_*()` methods specify their
  default format.

- Added
  [`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md),
  which decomposes a time vector into its constituent parts using
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)-like
  semantics: named
  [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/
  [`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
  expressions (the same vocabulary used in
  [`format()`](https://rdrr.io/r/base/format.html) strings) are resolved
  together in a single decomposition of `x`, returning a data frame of
  linear
  ([`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md))
  and cyclical
  ([`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md))
  component time vectors (e.g.
  `time_components(yearmonth(x), yr = lin(year), mth = cyc(month, year))`).

- Added
  [`time_compose()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_compose.md),
  the inverse of
  [`time_components()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_components.md):
  builds a single `mixtime` time point from a set of
  [`lin()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)/[`cyc()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/component_helpers.md)
  components, supplied either as `spec ~ value` formulas (e.g.
  `time_compose(lin(year) ~ 1980, cyc(month, year) ~ 3` for March 1980)
  or as already-tagged linear/cyclical time vectors.

- Added
  [`time_is_determinate_at()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_is_determinate_at.md),
  which tests whether a time point resolves a given (typically finer)
  granule exactly: a discrete time point never determines a finer
  granule (a year has no determinate month), while a continuous time
  point tracks progress within its chronon and so always does (0%
  through 2020 is 0% through January).

- Added
  [`time_is_complete_at()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_is_complete_at.md),
  which tests, for each element of a time vector, whether the coarser
  granule it falls into is fully observed *by the vector as a whole* -
  unlike
  [`time_is_determinate_at()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_is_determinate_at.md),
  completeness is a collective property: an element is `TRUE` only when
  the other elements needed to fill out its granule are also present
  elsewhere in the vector (e.g. a year is only complete once all twelve
  of its months are present).

- Cyclical time vectors can now be compared with `==`, `!=`, `<`, `<=`,
  `>` and `>=`, which previously errored. A cyclical value means a
  position within its cycle rather than an instant, so comparison is
  made on that position: two Wednesdays are equal regardless of which
  week they fall in.

- Added
  [`chronon_cardinality_fixed()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality_fixed.md),
  a variant of
  [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
  for time granule pairs whose relationship is a constant,
  context-independent number (e.g., 60 seconds in a minute) rather than
  dependent on an `at` time point (e.g., 28-31 days in a month).

- Added civil time microseconds and nanoseconds to `cal_time_civil`
  ([\#92](https://github.com/mitchelloharawild/mixtime/issues/92)).

- Added
  [`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
  and
  [`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md),
  an authoring interface for declaring a calendar granule’s label scheme
  once and getting both text *formatting* and its
  [`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md)-ready
  inverse, text *parsing*
  ([`linear_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md)/[`cyclical_labels_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md)),
  registered together so the two directions can’t drift apart. Covers
  plain numeric labels, named labels from a lookup table
  ([`vocab_table()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/vocab_table.md)),
  irregular cycles where the raw-index-to-name mapping isn’t a constant
  shift (`transform`, e.g. a leap month that splits one name into two),
  and a full `format`/`parse` escape hatch for labels that aren’t
  index-shaped at all (e.g. “2BC”). See
  [`?cyclical_labels`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md).
  A scheme is built with
  [`label_scheme()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
  and assigned directly as a
  [`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)/[`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
  method (e.g.
  `method(cyclical_labels, list(granule, cycle)) <- label_scheme(...)`).
  `validate_label_scheme()` checks a scheme’s format/parse round-trip,
  most useful for a hand-written `transform`.

### Deprecated

- [`new_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_time.md),
  the low-level constructor for `mt_time` vectors, is deprecated in
  favour of calling the concrete time class constructors directly:
  [`mt_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md),
  [`mt_duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md),
  and
  [`mt_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_time-class.md).

### Breaking changes

- [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
  (and
  [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md),
  [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md),
  and convenience functions like
  [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)
  and
  [`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md))
  now parse character `data` with
  [`time_parse()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_parse.md).
  Parsing is now tighter and chronon-specific: text must match the
  requested chronon’s own format (e.g. `yearmonth("2026 Feb")` now
  works, but `yearmonth("2026-02-14")`, now errors).

- [`is_time_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/is_time.md),
  [`is_time_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/is_time.md),
  and
  [`is_time_duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/is_time.md)
  have been renamed to
  [`time_is_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/is_time.md),
  [`time_is_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/is_time.md),
  and
  [`time_is_duration()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/is_time.md)
  for consistency with semantic property functions.

- [`tz_offset()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_offset.md)
  now returns a `mixtime` duration vector instead of a plain numeric
  vector, so offsets carry their chronon (and can be combined with other
  durations) rather than being a bare count of seconds/days/etc.

- [`tz_transitions()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_transitions.md)
  now returns `time` as a continuous `mixtime` linear time point (UTC
  seconds) and `offset_before`/`offset_after` as `mixtime` durations
  (UTC seconds), instead of plain numeric columns.

- Added `at` argument to
  [`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md),
  which provides the linear position of the cycle granule to allow
  appropriate labelling of irregular cycles, the same convention as
  [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)’s
  `at` argument
  ([\#100](https://github.com/mitchelloharawild/mixtime/issues/100)).

- [`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
  and
  [`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md),
  the S7 generics that turn a granule’s position into label text, have
  been renamed to
  [`linear_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md)
  and
  [`cyclical_labels_format()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_format.md).
  The bare names are now the authoring interface described above; a
  hand-written
  `method(linear_labels_format, ...) <-`/`method(cyclical_labels_format, ...) <-`
  is still available for granules that only ever needs formatting (no
  parsing).

### Bug fixes

- `cal_gregorian`’s day\<-\>month conversion
  ([`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
  and
  [`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_divmod.md))
  now supports a month chronon of *any* non-zero size (previously capped
  at `abs(n) < 12`, e.g. `month(13L)`/`years(1L) + months(6L)`-scale
  steps errored with “Month chronons \>= 12 are not yet supported”), and
  fixes two correctness bugs uncovered while generalising it, both
  specific to a multi-month (`abs(n) > 1`) chronon:
  `chronon_divmod(day, month)`’s `mod` used to be the offset into the
  *calendar* month containing the input day, which silently dropped any
  whole months already elapsed earlier in a wider window
  (e.g. `seq(date( "2020-02-15"), by = "2 months")` produced
  `2020-01-15, 2020-03-15, ...`, a full window early, instead of
  `2020-02-15, 2020-04-15, ...`); and `chronon_divmod(day, month)`’s
  `div` used to be `fdiv(res, res_scale)` unconditionally, which only
  agrees with
  [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)’s
  window anchor (`at * n`, always counting the window forward regardless
  of sign) when `res_scale > 0` - for a negative multi-month `to` it
  instead picked the window running backwards from the input day. Both
  the `>= 12` cap and the old
  [`circsum()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/circsum.md)-tabulated
  approach it came from are gone: day counts for a window are now a
  difference of two calls to a new O(1) `month_start_days()` helper (the
  same closed-form leap-year arithmetic `chronon_divmod(year, day)`
  already used, reused here anchored to a month instead of a year), so
  the cost no longer scales with the chronon’s `n`.

- `chronon_nests_in_fixed()` (and so `chronon_needs_clamping()`, which
  it backs) previously ignored `@n` entirely for a cross-class pair,
  treating e.g. `month(5L)` and `month(2L)` identically as long as month
  and year were connected at all - so it reported `month(5L)` as nesting
  inside `year(1L)` even though 12 months/year isn’t a whole multiple
  of 5. Fixed to check the same divisibility the identical-class case
  already did, generalised across a fixed-cardinality chain (multiplying
  [`chronon_cardinality_fixed()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality_fixed.md)
  along it, e.g. 3 months/quarter \* 4 quarters/year = 12 months/year).

- `chronon_nests_in()` previously ignored `@n` entirely for any
  cross-class pair, reporting e.g. both `month(5L)` as nesting inside
  `year(1L)` (wrong - 12 months/year isn’t a whole multiple of 5, so a
  5-month block never lands on a year boundary) and `day(2L)` as nesting
  inside `month(1L)` (also wrong in general - whether a 2-day block
  straddles a month boundary depends on the parity of that month’s start
  day-of-epoch). Fixed to match `chronon_nests_in_fixed()`’s exact
  answer whenever a fixed (context- independent) path connects the two
  classes (that path’s ratio is constant, so if it doesn’t divide
  evenly, no other path could make it nest either). For a pair connected
  only by an irregular (context-dependent) path, a single-unit (`n = 1`)
  chronon still always nests (it’s never split across a boundary), but
  any other `n` now errors instead of silently assuming it nests -
  checking every `at` in the cycle isn’t generally possible, since cycle
  length isn’t a declared, inspectable property of
  [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
  methods. `chronon_needs_clamping()` (which uses both functions
  together) is unaffected in practice - it now no longer fires for a
  fixed-reachable, non-dividing pair like `month(5L)`/`year(1L)`, but no
  built-in calendar arithmetic exercises that combination.

- [`seq()`](https://rdrr.io/r/base/seq.html) on a `linear_time` with a
  `by` whose magnitude doesn’t divide evenly into the coarser calendar
  unit it’s ultimately anchored to (e.g. `by = months(5L)`,
  `by = months(13L)`, `by = "18 months"` - none of 5, 13, 18 divide 12)
  now errors instead of silently producing calendar-drifted output.
  [`seq()`](https://rdrr.io/r/base/seq.html)’s clamped path decomposes
  `from` once and then advances by whole-number offsets in a single
  pass, which assumes every `by`-sized block has the same shape - only
  true when `by`’s magnitude divides evenly into that coarser unit
  (checked with the fixed `chronon_nests_in_fixed()` fix above). When it
  doesn’t, blocks land on a different month-of-year each step and the
  output silently drifts
  (e.g. `seq(date("2020-01-31"), by = months(18L), length.out = 4)` used
  to give `2020-01-31, 2021-08-03, 2023-01-31, 2024-08-02` - only every
  second step agreeing with repeated `date + months(18L)`, which is
  unaffected by this and already correct). Supporting this case properly
  (matching what repeated `+`/`-` already do) is left for later;
  `by = months(1L)/(2L)/(3L)/(4L)/(6L)/(12L)` and `by = years(n)` are
  all unaffected, and `+`/`-` were never affected (each call
  re-decomposes the current instant rather than batching an offset from
  a single decomposition).

- `linear_time + duration` and `linear_time - duration` across chronons
  of different granularity (e.g. `date() + months()`,
  `date() + years()`), and [`seq()`](https://rdrr.io/r/base/seq.html) on
  a `linear_time` with a coarser `by`
  (e.g. `seq(date, by = "1 month")`), now use calendar-field arithmetic:
  the duration’s/`by`’s own chronon is advanced by its magnitude,
  decomposing along the *longest* available chain of calendar boundaries
  (e.g. day -\> month -\> year, not a shorter direct day -\> year edge)
  so that only the specific invalid component (e.g. day-of-month) is
  clamped to the target period’s valid range, and every other
  component - including time-of-day - is preserved exactly
  (e.g. `date("2020-01-31") + months(1)` is now `2020-02-29`, not
  `2020-03-02`; `date("2024-02-29") + years(1)` now agrees with
  `+ months(12)`, both giving `2025-02-28`). Previously the shift was
  approximated as `n * cardinality_at_start`, which only happened to be
  exact for `n == 1` additions within same-length periods, and
  compounded error for larger `n` (`date("2026-03-25") + months(6)` was
  `2026-09-27`, two days late; twelve months never returned to the same
  day of the same month a year later). See `_dev/clamping.md` for the
  design and remaining open questions.

- Fixed `linear_time + duration`/`- duration` erroring with “Cannot
  convert from timezone-naive chronon to timezone-aware chronon”
  whenever a timezone-aware time point
  (e.g. `datetime(tz = "Australia/Melbourne")`) was shifted by a
  duration with a regular (fixed-cardinality) relationship to the time’s
  chronon, such as
  [`hours()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration_helpers.md),
  [`minutes()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration_helpers.md),
  [`seconds()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration_helpers.md),
  or
  [`days()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration_helpers.md)
  ([`months()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration_helpers.md)/[`years()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/duration_helpers.md)
  were unaffected, as they use the calendar-clamping path above). This
  was a regression from the change above: the shift’s non-clamping path
  round-trips the time through the duration’s chronon and back, and the
  return leg needs the duration’s chronon to carry the time’s timezone
  first, the same way [`seq()`](https://rdrr.io/r/base/seq.html) already
  does.

- Fixed [`seq()`](https://rdrr.io/r/base/seq.html) on a `linear_time`
  with a negative-magnitude `by` (e.g.
  `seq(date("2020-03-31"), by = "-1 month", length.out = 3)`,
  `by = months(-1L)`) erroring with `'from' must be a finite number`,
  and, once that was fixed, clamping to the wrong end of the target
  period for descending sequences (`2020-03-31, 2020-03-02, 2020-01-31`
  instead of `2020-03-31, 2020-02-29, 2020-01-31`). Also fixes
  [`seq()`](https://rdrr.io/r/base/seq.html)/calendar-field arithmetic
  silently landing on the wrong date for any multi-unit calendar
  `by`/duration whose target chronon isn’t `n = 1`
  (e.g. `by = "2 months"`). The negative-`by` error was
  [`circsum()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/circsum.md)
  (the internal circular rolling-sum helper backing days-in-a-month
  lookups) rejecting a negative `size`/`step` outright; it now gives
  both a real meaning instead - a negative `step` walks the same windows
  backwards, and a negative `size` anchors a window at its end instead
  of its start (a trailing rather than leading rolling sum) - which a
  negative-`n` month chronon needs directly. The multi-unit case was a
  stray reassignment in `R/cal_gregorian.R`’s day\<-\>month conversion
  left over from an unrelated rebase, corrupting results for any target
  month-chronon whose multiplier wasn’t exactly 1.

- Combining two cyclical time vectors with different cycles is now an
  error rather than silently collapsing them. A cycle is a modulus
  rather than a unit of measure, so unlike a chronon it has no
  meaningful common value.

- Converting to a finer chronon now takes a path that only ever steps
  finer, instead of the shortest path through the registered calendar
  arithmetic methods. A path that steps coarser first has to express the
  time point as a fraction of the coarser unit, and expanding that
  fraction again assumes the unit subdivides evenly: quarters were
  routed to days via years, making 2020 Q1 91.5 days long rather than
  the 91 days of January to March.

- Fixed comparisons (`==`, `<`, etc.) between a timezone-aware
  `mt_linear`/ `mt_cyclical` time point and a naive one, and
  [`c()`](https://rdrr.io/r/base/c.html)/`vec_c()` combining two such
  values, giving numerically wrong results,
  e.g. `linear_time( "2015-02-01 10:00:00", hour(1L, tz = "Australia/Melbourne")) < datetime( "2015-02-01 00:00:00")`
  incorrectly returned `TRUE`, and combining a timezone-aware “02:00”
  with a naive “02:00” produced two different instants under the hood
  despite sharing a (naive) common chronon. Both operations convert
  their operands into a shared common chronon that correctly comes out
  naive when the operands disagree on timezone (see
  [`chronon_common()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md)),
  but converting an operand *into* that common chronon silently
  re-inherited the operand’s own timezone back (one conversion at a
  time), so a timezone-aware operand ended up at its true (UTC) absolute
  instant while a naive operand was left as-is - two different bases
  being compared/combined as if they were the same.
  [`chronon_common()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md)’s
  naive result is now hardened so it can no longer be re-inherited into,
  and every operand converted into it lands on bare wall-clock time
  instead, the same way `datetime(x, tz = NA)` already strips a timezone
  for a single value.

### Improvements

- Substantially faster performance by caching the granule cardinality
  graph (used by `chronon_convert()`,
  [`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_divmod.md),
  [`chronon_common()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_glb.md)
  and `time_parts()`) and tzdb names (used in operations with
  timezones).
- Reworked time vectors to be built with S7 rather than vctrs for
  improved double dispatch methods for arithmetic and other operations
  ([\#63](https://github.com/mitchelloharawild/mixtime/issues/63)).

## mixtime 0.2.0

CRAN release: 2026-06-28

### New features

- Added support for multiplication and division of time durations.
- Added
  [`round()`](https://rdrr.io/r/base/Round.html)/[`floor()`](https://rdrr.io/r/base/Round.html)/[`ceiling()`](https://rdrr.io/r/base/Round.html)
  methods for rounding time by chronons. (note: use
  [`time_round()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_round.md),
  [`time_floor()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_round.md),
  and
  [`time_ceiling()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_round.md)
  for rounding to a specified time granule).
- Added casting from time durations to character vectors.
- Added explicit conversion to naive time by using `tz = NA`.

### Improvements

- [`time_unit_full()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_unit_labels.md)
  now uses cli-style pluralisation templates (e.g., `"year{?/s}"`,
  `"centur{?y/ies}"`), enabling accurate plural forms beyond a simple
  `s` suffix. A new `time_unit_plural(x, n)` helper resolves the
  template for a given quantity.
- Continuous time model durations now always show at least one decimal
  place.
- Added formatting support for `NaN`, `Inf`, and `-Inf` values in time
  types.
- Added time zone support for arithmetic.
- Time durations now use the same formatting system and format strings
  as linear and cyclical time.
- Improved [`str()`](https://rdrr.io/r/utils/str.html) output to be more
  compact for each time type.

### Bug fixes

- Fixed incorrect usage of
  [`time_chronon()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_chronon.md)
  causing arithmetic to fail.
- Fixed incompatibilities with vctrs sorting, ptype2, and casting
  methods.
- Fixed formatting of cyclical time with mixed-calendar chronon and
  cycle
  ([\#62](https://github.com/mitchelloharawild/mixtime/issues/62)).
- Fixed divmod of Gregorian day -\> month and day -\> year producing
  incorrect divisors near the year boundary for continuous time dates.
- Fixed [`c()`](https://rdrr.io/r/base/c.html) allowing non-time vectors
  in mixtime vectors.

## mixtime 0.1.0

CRAN release: 2026-05-19

This is the initial CRAN release of the package with provides the core
data types and temporal manipulation for temporal analysis with mixed
granularity data.

### New features

#### Time

- The generic
  [`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
  constructor for creating mixed temporal vectors that combine time
  points of different granularities (e.g. monthly and quarterly) in a
  single vector via `vecvec`.

#### Linear time

- [`linear_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time.md)
  creates linear time vectors with a user-specified chronon (smallest
  indivisible time granule), supporting both discrete (integer) and
  continuous (fractional) time models.
- Convenience functions for common temporal granularities:
  [`year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  [`yearquarter()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  [`yearweek()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  [`date()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md),
  and
  [`datetime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md).

#### Cyclical time

- [`cyclical_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time.md)
  creates cyclical time vectors representing positions within a
  repeating cycle (e.g. day-of-week, month-of-year).
- Convenience functions for common cyclical temporal granularities:
  [`month_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`day_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`day_of_month()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`day_of_week()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  [`week_of_year()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md),
  and
  [`time_of_day()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/cyclical_time_helpers.md).

#### Calendar systems

- Three initial calendar systems:
  - `cal_gregorian`: standard Gregorian calendar with year, quarter,
    month, day, hour, minute, second, and millisecond units.
  - `cal_isoweek`: ISO 8601 week-date calendar with ISO year, week, and
    day units. Weeks always start on Monday; the first week of the year
    is the week containing the first Thursday.
  - `cal_sym454`: Symmetry454 perennial solar calendar with a
    symmetrical 4–5–4 week pattern per quarter and a leap-week rule.

#### Sequences

- [`seq()`](https://rdrr.io/r/base/seq.html) methods for `mixtime` and
  `mt_time` objects support integer, string (e.g. `"1 month"`), and time
  unit `by` arguments, as well as `length.out` and `along.with`.
  Overflow behaviour when step granules differ from sequence granules
  can be controlled with `on_invalid = "nearest"` (default) or
  `"overflow"`.

#### Rounding

- `round_time()`, `floor_time()`, and `ceiling_time()` round time
  objects to a specified time granule, preserving the input class and
  timezone.

#### Formatting

- A mixed-calendar general purpose formatting system is provided for
  `mixtime` objects. The format strings use glue-like
  [`{}`](https://rdrr.io/r/base/Paren.html) substitutions with the
  helper functions `lin(x)` and `cyc(x, y)` to position linear and
  cyclical time components in a string. `x` and `y` are time granules
  from a calendar, which can be used to create general purpose
  mixed-calendar time formats.

#### Timezone support

- [`tz_name()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_name.md)
  extracts the timezone from a time object.
- [`tz_offset()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_offset.md)
  returns the UTC offset for a datetime in its timezone.
- [`tz_abbreviation()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_abbreviation.md)
  returns the timezone abbreviation (e.g. `"EST"`, `"PDT"`).
- [`tz_transitions()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/tz_transitions.md)
  returns a data frame of DST and other timezone transitions between two
  time points.

#### Accessor functions

- [`time_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_calendar.md)
  extracts the calendar system from a time object.

#### tsibble and fable compatibility

- Methods for `tsibble` index validation are provided, enabling
  `mixtime` vectors to be used as time indexes in `tsibble` objects and
  in forecasting with `fable`.

#### Extensibility

- [`new_calendar()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_calendar.md)
  defines a calendar as a named collection of time unit classes, with
  optional inheritance from another calendar.

- [`mt_unit()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md),
  [`mt_tz_unit()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md),
  and
  [`mt_loc_unit()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mt_unit.md)
  primitives for creating new time units.

- [`new_linear_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_linear_time_fn.md)
  and
  [`new_cyclical_time_fn()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_cyclical_time_fn.md)
  are factories for building convenient linear and cyclical time
  functions
  (e.g. [`yearmonth()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/linear_time_helpers.md)).

- Calendar arithmetic with:

  - [`chronon_divmod()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_divmod.md)
    performs division-with-remainder arithmetic between time granules
    (e.g. 1 day to 1 month), enabling cross-granule calendrical
    arithmetic.
  - [`chronon_cardinality()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_cardinality.md)
    returns the number of smaller granules contained in a coarser
    granule (e.g. days in 1 month), with context-dependent results for
    variable-length granules.
  - [`chronon_epoch()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_epoch.md)
    returns the epoch offset for a given time unit.

- Displaying time with:

  - [`time_unit_abbr()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_unit_labels.md)
    and
    [`time_unit_full()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/time_unit_labels.md)
    for time unit text.
  - [`linear_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
    and
    [`cyclical_labels()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/label_scheme.md)
    for time labels (e.g. Jan, Feb, … for months of year).
  - [`chronon_format_linear()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
    and
    [`chronon_format_cyclical()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format.md)
    for default formatting strings.
  - [`chronon_format_attr()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/chronon_format_attr.md)
    for appending attribute information (e.g. timezones)

#### Vignettes

- Extending mixtime - a vignette on how to define new calendars and
  implement calendar arithmetic for temporal operations within the
  calendar’s units and with other calendars.
