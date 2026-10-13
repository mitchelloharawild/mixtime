# mixtime: Mixed Temporal Vectors and Operations

Flexible time classes for time series analysis and forecasting with
mixed temporal granularities. Supports linear and cyclical time
representations in discrete and continuous forms, with timezone support,
across multiple calendar systems including Gregorian and ISO week date
calendars. Time points are stored numerically relative to a chronon; an
atomic time granule defined by time units of a calendar. Calendrical
arithmetic enables conversion between time granules (e.g. days to
months) and calendar systems. Multi-unit arithmetic allows for temporal
analysis with other granules of common calendars (e.g. fortnights are
2-week units). Time vectors of different granularities (e.g. monthly and
quarterly) can be combined in a single vector, making 'mixtime' ideal
for data that changes observation frequency over time or requires
temporal reconciliation across scales. The package is extensible,
allowing users to define custom calendars that build upon civil and
astronomical time systems.

## Package options

- `mixtime.on_invalid`: The default strategy for resolving time points
  that don't exist, such as 31 January plus one month, used by `+`, `-`,
  [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_shift.md)
  and
  [seq()](https://pkg.mitchelloharawild.com/mixtime/reference/seq.mixtime.md).
  One of `"clamp"` (the default), `"previous"`, `"next"`, `"overflow"`,
  `"reject"`, or `"error"`. See
  [`time_shift()`](https://pkg.mitchelloharawild.com/mixtime/reference/time_shift.md)
  for details.

## See also

Useful links:

- <https://pkg.mitchelloharawild.com/mixtime/>

- <https://github.com/mitchelloharawild/mixtime>

- Report bugs at <https://github.com/mitchelloharawild/mixtime/issues>

## Author

**Maintainer**: Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
([ORCID](https://orcid.org/0000-0001-6729-7695))

Authors:

- Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
  ([ORCID](https://orcid.org/0000-0001-6729-7695))

Other contributors:

- Cynthia Huang ([ORCID](https://orcid.org/0000-0002-9218-987X))
  \[contributor\]

- Rob Hyndman ([ORCID](https://orcid.org/0000-0002-2140-5352))
  \[contributor\]
