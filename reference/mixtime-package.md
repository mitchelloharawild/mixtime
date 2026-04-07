# mixtime: Mixed Temporal Vectors and Operations

Flexible time classes for time series analysis and forecasting with
mixed temporal granularities. Supports linear and cyclical time
representations in discrete and continuous forms, with timezone support,
across multiple calendar systems including Gregorian and ISO week date
calendars. Time points are stored numerically relative to a chronon; an
atomic time unit defined by a calendar. Calendrical arithmetic enables
conversion between time units (e.g. days to months) and calendar
systems. Multi-unit arithmetic allows for temporal analysis with other
granules of common calendars (e.g. fortnights are 2-week units). Time
vectors of different granularities (e.g. monthly and quarterly) can be
combined in a single vector, making mixtime ideal for data that changes
observation frequency over time or requires temporal reconciliation
across scales. The package is extensible, allowing users to define
custom calendars that build upon civil and astronomical time systems.

## See also

Useful links:

- <https://pkg.mitchelloharawild.com/mixtime/>

- <https://github.com/mitchelloharawild/mixtime>

- Report bugs at <https://github.com/mitchelloharawild/mixtime/issues>

## Author

**Maintainer**: Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
([ORCID](https://orcid.org/0000-0001-6729-7695))
